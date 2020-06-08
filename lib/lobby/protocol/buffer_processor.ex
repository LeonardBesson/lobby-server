defprotocol Lobby.Protocol.BufferProcessor do
  @moduledoc """
  Protocol for modifying network buffers
  """

  @type direction :: :in | :out

  @spec process(t, iodata, direction) :: {binary, t}
  def process(processor, buffer, direction)
end
