defprotocol Lobby.Protocol.Message do
  @moduledoc """
  Protocol responsible for serializing the messages structs
  into packets.
  """

  @type message :: any

  @doc """
  Returns the packet type associated with the message
  """
  @spec packet_type(message) :: atom
  def packet_type(message)

  @doc """
  Serialize the message into a binary
  """
  @spec serialize(message) :: binary
  def serialize(message)
end
