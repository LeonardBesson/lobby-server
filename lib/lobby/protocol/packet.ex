defmodule Lobby.Protocol.Packet do
  @moduledoc """
  A packet sent to or received from the network
  """
  use Bitwise
  alias Lobby.Protocol.PacketInfo

  defstruct flags: 0, packet_type: 0, data: <<>>

  @type t :: %__MODULE__{
          flags: byte,
          packet_type: non_neg_integer,
          data: binary
        }

  @packet_types %{
    packet_init: 0
  }

  # TODO: macro
  @packet_infos %{
    packet_init: %PacketInfo{packet_type: :packet_init, name: "PacketInit", fixed_size: nil}
  }

  # Flags
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

    int_type = @packet_types[type]

    if int_type == nil do
      raise ArgumentError, message: "Attempt to create packet of unknown type #{type}"
    end

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
      if int_type < 256 do
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

  def has?(type), do: Map.has_key?(@packet_infos, type)

  def get!(type) do
    case @packet_infos[type] do
      nil -> raise ArgumentError, message: "Packet #{inspect(type)} doesn't exist"
      packet_type -> packet_type
    end
  end

  def int_type!(%__MODULE__{packet_type: type}), do: int_type!(type)

  def int_type!(type) do
    case @packet_types[type] do
      nil -> raise ArgumentError, message: "Packet #{inspect(type)} doesn't exist"
      int_type -> int_type
    end
  end

  def data_size(%__MODULE__{data: data}) do
    byte_size(data)
  end
end
