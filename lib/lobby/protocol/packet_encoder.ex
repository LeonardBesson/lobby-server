defmodule Lobby.Protocol.PacketEncoder do
  @moduledoc false

  alias Lobby.Utils.Queue
  require Lobby.Protocol.Packet
  alias Lobby.Protocol.Packet
  use Bitwise

  defstruct target_size: 8 * 1024, buffers: Queue.new(), packets: Queue.new()

  @type t :: %__MODULE__{
          target_size: pos_integer,
          buffers: Queue.t(iodata),
          packets: Queue.t(Packet.t())
        }

  @max_packet_header_size 6

  def new(target_size) do
    %__MODULE__{target_size: target_size, buffers: Queue.new(), packets: Queue.new()}
  end

  def add_packet(%__MODULE__{packets: packets} = encoder, %Packet{} = packet) do
    packets = packets |> Queue.push_back(packet)
    %{encoder | packets: packets}
  end

  def next_buffer(%__MODULE__{buffers: buffers} = encoder) do
    if Queue.empty?(buffers) do
      encoder = pack(encoder)
      {buffer, buffers} = Queue.pop_front(encoder.buffers)
      {buffer, %{encoder | buffers: buffers}}
    else
      {buffer, buffers} = Queue.pop_front(buffers)
      {buffer, %{encoder | buffers: buffers}}
    end
  end

  defp pack(%__MODULE__{packets: packets} = encoder) do
    if Queue.empty?(packets) do
      encoder
    else
      pack(encoder, <<>>)
    end
  end

  defp pack(
         %__MODULE__{packets: packets, buffers: buffers, target_size: target_size} = encoder,
         current_buffer
       ) do
    if Queue.empty?(packets) do
      buffers = buffers |> Queue.push_back(current_buffer)
      %{encoder | buffers: buffers}
    else
      {packet, packets} = Queue.pop_front(packets)

      current_length = IO.iodata_length(current_buffer)
      required_size = current_length + @max_packet_header_size + Packet.data_size(packet)

      if current_length > 0 and required_size > target_size do
        buffers = buffers |> Queue.push_back(current_buffer)
        %{encoder | buffers: buffers}
      else
        new_buffer = <<
          packet.flags::size(8),
          encode_type(packet)::binary,
          encode_size(packet)::binary,
          packet.data::binary
        >>

        # Discard empty buffers
        current_buffer =
          if current_length == 0 do
            new_buffer
          else
            [current_buffer, new_buffer]
          end

        encoder = %{encoder | packets: packets}
        pack(encoder, current_buffer)
      end
    end
  end

  defp encode_type(%Packet{flags: flags} = packet) when Packet.short_type?(flags),
    do: <<Packet.get_id!(packet)::size(8)>>

  defp encode_type(%Packet{} = packet),
    do: <<Packet.get_id!(packet)::big-integer-size(16)>>

  defp encode_size(%Packet{flags: flags} = packet) when Packet.short_size?(flags),
    do: <<Packet.data_size(packet)::size(8)>>

  defp encode_size(%Packet{} = packet),
    do: <<Packet.data_size(packet)::big-integer-size(24)>>
end
