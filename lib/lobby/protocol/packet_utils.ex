defmodule Lobby.Protocol.PacketUtils do
  @moduledoc """
  Utilities to define and manipulate packets
  """
  alias Lobby.Protocol.PacketInfo
  alias Lobby.Protocol.Bincode
  alias Lobby.Protocol.Packet
  alias Lobby.Protocol.Message

  @doc """
  Macro used to define the packets. Given a keyword list of [packet_type: {packet_id, [field_name: field_type]}],
  it will populate 3 attributes in the caller modules. @packet_types for the type to id
  mapping, @packet_ids for the id to type mapping, and @packet_infos for the type to
  %PacketInfo{} mapping.
  """
  defmacro declare_packets(packets) when is_list(packets) do
    %Macro.Env{module: caller_module} = __CALLER__

    packets_by_type = Enum.into(packets, %{}, fn {type, {id, _}} -> {type, id} end)
    packets_by_id = Enum.into(packets, %{}, fn {type, {id, _}} -> {id, type} end)

    packet_infos =
      Enum.into(packets, %{}, fn {type, {id, _}} ->
        {type,
         %PacketInfo{
           packet_type: type,
           name: Macro.camelize(to_string(type)),
           fixed_size: nil
         }}
      end)

    Module.put_attribute(caller_module, :packet_types, packets_by_type)
    Module.put_attribute(caller_module, :packet_ids, packets_by_id)
    Module.put_attribute(caller_module, :packet_infos, packet_infos)

    for {type, {id, fields}} <- packets do
      message_module = Module.concat([Lobby.Messages, Macro.camelize(to_string(type))])
      struct_data = for {field_name, _} <- fields, do: {field_name, nil}

      serialize_quoted_fields =
        for {field_name, field_type} <- fields do
          quote do
            <<Bincode.serialize(
                var!(message).unquote(Macro.var(field_name, nil)),
                unquote(field_type)
              )::binary>>
          end
        end

      deserialize_quoted_fields =
        for {field_name, field_type} <- fields do
          quote do
            {
              var!(unquote(Macro.var(field_name, nil))),
              var!(data)
            } = Bincode.deserialize(var!(data), unquote(field_type))
          end
        end

      message_struct_quoted_fields =
        for {field_name, field_type} <- fields do
          quote do: {unquote(field_name), var!(unquote(Macro.var(field_name, nil)))}
        end

      quote do
        defmodule unquote(message_module) do
          defstruct unquote(struct_data)

          def serialize(%unquote(message_module){} = var!(message)) do
            <<unquote_splicing(serialize_quoted_fields)>>
          end

          def deserialize(var!(data)) do
            unquote_splicing(deserialize_quoted_fields)
            struct!(unquote(message_module), unquote(message_struct_quoted_fields))
          end
        end

        defimpl Lobby.Protocol.Message, for: unquote(message_module) do
          def packet_type(_message) do
            unquote(type)
          end

          def serialize(message) do
            unquote(message_module).serialize(message)
          end
        end
      end
    end
  end

  def message_to_packet(message) do
    packet_type = Message.packet_type(message)
    packet_data = Message.serialize(message)
    Packet.new(packet_type, packet_data)
  end
end
