defprotocol Lobby.Protocol.MessageSerializer do
  @moduledoc """
  Protocol used to serialize the messages structs into binary
  """

  @type message :: any

  @doc """
  Serialize the given message into a binary
  """
  @spec serialize(message) :: binary
  def serialize(message)
end
