defmodule Lobby.Protocol.Utils do
  @moduledoc """
  Utilities to manipulate packet and messages
  """
  alias Lobby.Protocol.Message
  alias Lobby.Protocol.Packet

  @spec message_to_packet(Message.t()) :: {:ok, Packet.t()} | {:error, binary}
  def message_to_packet(message) do
    packet_data = Message.serialize(message, varint: true)

    case packet_data do
      {:ok, data} ->
        packet_type = Message.packet_type(message)
        {:ok, Packet.new(packet_type, data)}

      {:error, msg} ->
        {:error, msg}
    end
  end

  @spec message_to_packet!(Message.t()) :: Packet.t()
  def message_to_packet!(message) do
    case message_to_packet(message) do
      {:ok, packet} -> packet
      {:error, msg} -> raise ArgumentError, message: msg
    end
  end

  @spec packet_to_message(Packet.t(), module) :: {:ok, Message.t()} | {:error, binary}
  def packet_to_message(%Packet{} = packet, message_module) do
    case message_module.deserialize(packet.data, varint: true) do
      {:ok, {msg, ""}} -> {:ok, msg}
      {:ok, {_msg, _rest}} -> {:error, "Packet has too much data"}
      {:error, msg} -> {:error, msg}
    end
  end

  @spec packet_to_message!(Packet.t(), module) :: Message.t()
  def packet_to_message!(%Packet{} = packet, message_module) do
    case packet_to_message(packet, message_module) do
      {:ok, msg} -> msg
      {:error, msg} -> raise ArgumentError, message: msg
    end
  end
end
