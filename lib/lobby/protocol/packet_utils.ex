defmodule Lobby.Protocol.PacketUtils do
  @moduledoc """
  Utilities to define and manipulate packets
  """
  alias Lobby.Protocol.PacketInfo

  @doc """
  Macro used to define the packets. Given a keyword list of [packet_type: packet_id],
  it will populate 3 attributes in the caller modules. @packet_types for the type to id
  mapping, @packet_ids for the id to type mapping, and @packet_infos for the type to
  %PacketInfo{} mapping.
  """
  defmacro declare_packets(packets) when is_list(packets) do
    %Macro.Env{module: caller_module} = __CALLER__

    packets_by_type = Enum.into(packets, %{})
    packets_by_id = Enum.into(packets, %{}, fn {type, id} -> {id, type} end)

    packet_infos =
      Enum.into(packets, %{}, fn {type, id} ->
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
  end
end
