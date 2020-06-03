defmodule PacketDecoderTest do
  use ExUnit.Case
  alias Lobby.Protocol.PacketEncoder
  alias Lobby.Protocol.PacketDecoder
  alias Lobby.Protocol.Packet

  describe "PacketDecoder" do
    test "single packet" do
      encoder = PacketEncoder.new(256)
      encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(1, 25)))
      {buffer, _} = PacketEncoder.next_buffer(encoder)
      assert IO.iodata_length(buffer) == 3 + 25
      assert(buffer == <<224, 0, 25, fill_binary(1, 25)::binary>>)

      decoder = PacketDecoder.new()
      decoder = PacketDecoder.push_buffer(decoder, buffer)
      {packet, decoder} = PacketDecoder.next_packet(decoder)
      expected_data = fill_binary(1, 25)
      assert %Packet{flags: 224, packet_type: :packet_init, data: ^expected_data} = packet

      assert {nil, decoder} = PacketDecoder.next_packet(decoder)
    end

    test "multiple packets" do
        encoder = PacketEncoder.new(256)
        encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(1, 25)))
        encoder = PacketEncoder.add_packet(encoder, Packet.new(:packet_init, fill_binary(1, 75)))
        {buffer, _} = PacketEncoder.next_buffer(encoder)
        assert IO.iodata_length(buffer) == 3 + 25 + 3 + 75

        decoder = PacketDecoder.new()
        decoder = PacketDecoder.push_buffer(decoder, IO.iodata_to_binary(buffer))
        {packet, decoder} = PacketDecoder.next_packet(decoder)
        expected_data = fill_binary(1, 25)
        assert %Packet{flags: 224, packet_type: :packet_init, data: ^expected_data} = packet

        assert {packet, decoder} = PacketDecoder.next_packet(decoder)
        expected_data = fill_binary(1, 75)
        assert %Packet{flags: 224, packet_type: :packet_init, data: ^expected_data} = packet

        assert {nil, decoder} = PacketDecoder.next_packet(decoder)
    end
  end

  def fill_binary(value, length) do
    for _ <- 1..length, into: <<>>, do: <<value>>
  end
end

