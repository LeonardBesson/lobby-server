defmodule Lobby.Protocol.PacketUtils do
  @moduledoc """
  Utilities to define and manipulate packets
  """
  alias Lobby.Protocol.PacketInfo
  alias Lobby.Protocol.Bincode

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
          Bincode.serialize(field_name, field_type)
        end

      deserialize_quoted_fields =
        for {field_name, field_type} <- fields do
          Bincode.deserialize(field_name, field_type)
        end

      message_struct_quoted_fields =
        for {field_name, field_type} <- fields do
          quote do: {unquote(field_name), var!(unquote(Macro.var(field_name, nil)))}
        end

      quote do
        defmodule unquote(message_module) do
          defstruct unquote(struct_data)

          def serialize(message) do
            var!(message) = message
            <<unquote_splicing(serialize_quoted_fields)>>
          end

          def deserialize(data) do
            <<unquote_splicing(deserialize_quoted_fields)>> = data
            struct!(unquote(message_module), unquote(message_struct_quoted_fields))
          end
        end

        defimpl Lobby.Protocol.MessageSerializer, for: unquote(message_module) do
          def serialize(message) do
            unquote(message_module).serialize(message)
          end
        end
      end
    end
  end
end
