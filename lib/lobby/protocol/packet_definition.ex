defmodule Lobby.Protocol.PacketDefinition do
  @moduledoc """
  Macros to define packets
  """
  alias Lobby.Protocol.PacketInfo

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
      Enum.into(packets, %{}, fn {type, _} ->
        {type,
         %PacketInfo{
           packet_type: type,
           name: Macro.camelize(to_string(type)),
           message_module: get_message_module(type),
           fixed_size: nil
         }}
      end)

    Module.put_attribute(caller_module, :packet_types, packets_by_type)
    Module.put_attribute(caller_module, :packet_ids, packets_by_id)
    Module.put_attribute(caller_module, :packet_infos, packet_infos)

    quote do
      require Bincode.Structs

      unquote do
        for {type, {_, fields}} <- packets do
          message_module = get_message_module(type)

          quote do
            Bincode.Structs.declare_struct(unquote(message_module), unquote(fields), absolute: true)

            defimpl Lobby.Protocol.Message, for: unquote(message_module) do
              def packet_type(_message) do
                unquote(type)
              end

              def serialize(message, opts \\ []) do
                unquote(message_module).serialize(message, opts)
              end
            end
          end
        end
      end
    end
  end

  defp get_message_module(type) when is_atom(type) do
    Module.concat([Lobby.Messages, Macro.camelize(to_string(type))])
  end
end
