defmodule Lobby.Protocol.Packet do
  @moduledoc """
  A packet sent to or received from the network
  """
  use Bitwise
  alias Lobby.Protocol.PacketInfo
  import Lobby.Protocol.PacketUtils

  defstruct flags: 0, packet_type: 0, data: <<>>

  @type t :: %__MODULE__{
          flags: byte,
          packet_type: non_neg_integer,
          data: binary
        }

  declare_packets(
    packet_init:
      {0,
       [
         protocol_version: :u16,
         app_version: :u16
       ]}
  )

  @flag_fixed_header 1 <<< 7
  @flag_short_type 1 <<< 6
  @flag_short_size 1 <<< 5

  defmacro fixed_header?(flags) do
    quote do: (unquote(flags) &&& unquote(@flag_fixed_header)) != 0
  end

  defmacro short_type?(flags) do
    quote do: (unquote(flags) &&& unquote(@flag_short_type)) != 0
  end

  defmacro short_size?(flags) do
    quote do: (unquote(flags) &&& unquote(@flag_short_size)) != 0
  end

  def new(type, data) when is_atom(type) and is_binary(data) do
    packet_info = get!(type)

    packet_id = get_id!(type)

    size = byte_size(data)

    if packet_info.fixed_size != nil and size != packet_info.fixed_size do
      raise ArgumentError,
        message:
          "Attempt to create fixed size packet #{type} with wrong size. (fixed: #{
            packet_info.fixed_size
          }, actual: #{size})"
    end

    flags = @flag_fixed_header

    flags =
      if packet_id < 256 do
        flags ||| @flag_short_type
      else
        flags
      end

    flags =
      if size < 256 do
        flags ||| @flag_short_size
      else
        flags
      end

    %__MODULE__{flags: flags, packet_type: type, data: data}
  end

  @doc """
  Return true if the packet of the given type is declared
  """
  def has?(type), do: Map.has_key?(@packet_infos, type)

  @doc """
  Return the packet info for the given packet type
  """
  def get!(type) do
    case @packet_infos do
      %{^type => packet_info} -> packet_info
      _ -> raise ArgumentError, message: "Packet #{inspect(type)} not declared"
    end
  end

  @doc """
  Return the packet id for the given packet type
  """
  def get_id!(%__MODULE__{packet_type: type}), do: get_id!(type)

  def get_id!(type) do
    case @packet_types do
      %{^type => int_type} -> int_type
      _ -> raise ArgumentError, message: "Packet #{inspect(type)} not declared"
    end
  end

  @doc """
  Return the packet type for the given packet id
  """
  def get_type!(packet_id) do
    case @packet_ids do
      %{^packet_id => type} -> type
      _ -> raise ArgumentError, message: "Packet ID #{inspect(packet_id)} not declared"
    end
  end

  def data_size(%__MODULE__{data: data}) do
    byte_size(data)
  end
end
