defprotocol Lobby.Protocol.Message do
  @moduledoc """
  Protocol responsible for serializing the messages structs
  into packets.
  """

  @doc """
  Returns the packet type associated with the message
  """
  @spec packet_type(t) :: atom
  def packet_type(message)

  @doc """
  Serialize the message into a binary
  """
  @spec serialize(t) :: binary
  def serialize(message)
end
