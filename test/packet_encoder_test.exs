defmodule PacketEncoderTest do
  use ExUnit.Case
  alias Lobby.Protocol.PacketEncoder
  alias Lobby.Protocol.Packet
  alias Lobby.Utils.Queue

  describe "PacketEncoder" do
    test "single packet" do
      encoder = PacketEncoder.new(256)
      encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(1, 25)))
      {buffer, encoder} = PacketEncoder.next_buffer(encoder)
      assert IO.iodata_length(buffer) == 3 + 25
      assert(buffer == <<224, 0, 25, fill_binary(1, 25)::binary>>)
      assert Queue.empty?(encoder.packets)
      assert Queue.empty?(encoder.buffers)
    end

    test "size flag" do
      encoder = PacketEncoder.new(1024)
      encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(1, 255)))
      {buffer, encoder} = PacketEncoder.next_buffer(encoder)
      assert IO.iodata_length(buffer) == 3 + 255
      assert(buffer == <<224, 0, 255, fill_binary(1, 255)::binary>>)
      assert Queue.empty?(encoder.packets)
      assert Queue.empty?(encoder.buffers)

      encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(1, 256)))
      {buffer, encoder} = PacketEncoder.next_buffer(encoder)
      assert IO.iodata_length(buffer) == 5 + 256
      assert(buffer == <<192, 0, 0, 1, 0, fill_binary(1, 256)::binary>>)
      assert Queue.empty?(encoder.packets)
      assert Queue.empty?(encoder.buffers)
    end

    test "multiple packets" do
      encoder = PacketEncoder.new(1024)
      encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(1, 25)))
      encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(2, 30)))
      encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(3, 444)))
      {buffer, encoder} = PacketEncoder.next_buffer(encoder)
      assert IO.iodata_length(buffer) == 3 + 25 + 3 + 30 + 5 + 444
      packet_1 = <<224, 0, 25, fill_binary(1, 25)::binary>>
      packet_2 = <<224, 0, 30, fill_binary(2, 30)::binary>>
      packet_3 = <<192, 0, 0, 1, 188, fill_binary(3, 444)::binary>>
      assert(buffer == [[packet_1, packet_2], packet_3])
      assert Queue.empty?(encoder.packets)
      assert Queue.empty?(encoder.buffers)
    end
  end

  def fill_binary(value, length) do
    for _ <- 1..length, into: <<>>, do: <<value>>
  end
end
