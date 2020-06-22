defmodule Lobby.Protocol.PacketDecoder do
  @moduledoc false
  alias Lobby.Utils.ByteStream
  require Lobby.Protocol.Packet
  alias Lobby.Protocol.Packet

  defstruct stream: ByteStream.new()

  @type t :: %__MODULE__{
          stream: ByteStream.t()
        }

  def new do
    %__MODULE__{stream: ByteStream.new()}
  end

  def push_buffer(%__MODULE__{stream: stream} = decoder, buffer) when is_binary(buffer) do
    stream = ByteStream.push_buffer(stream, buffer)
    %{decoder | stream: stream}
  end

  def next_packet(%__MODULE{stream: stream} = decoder) do
    remaining = ByteStream.remaining(stream)

    if remaining < 1 do
      {nil, decoder}
    else
      {<<flags::size(8)>>, _} = ByteStream.peek(stream, 1)

      type_size = if Packet.short_type?(flags), do: 8, else: 16
      size_size = if Packet.short_size?(flags), do: 8, else: 24

      header_size = div(8 + type_size + size_size, 8)

      if remaining < header_size do
        {nil, decoder}
      else
        {header, _} = ByteStream.peek(stream, header_size)

        <<
          _flags::size(8),
          type::big-integer-size(type_size),
          header_rest::binary
        >> = header

        packet_type = Packet.get_type!(type)
        packet_info = Packet.get!(packet_type)

        data_size =
          if packet_info.fixed_size != nil do
            packet_info.fixed_size
          else
            <<size::big-integer-size(size_size)>> = header_rest
            size
          end

        if remaining < header_size + data_size do
          {nil, decoder}
        else
          {_, stream} = ByteStream.read(stream, header_size)
          {data, stream} = ByteStream.read(stream, data_size)

          packet = Packet.new(packet_type, data)
          decoder = %{decoder | stream: stream}
          {packet, decoder}
        end
      end
    end
  end
end
