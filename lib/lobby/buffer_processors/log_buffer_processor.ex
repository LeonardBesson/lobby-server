defmodule Lobby.BufferProcessors.LogBufferProcessor do
  @moduledoc """
  A buffer processor which simply logs the buffer
  """
  defstruct []
end

defimpl Lobby.Protocol.BufferProcessor, for: Lobby.BufferProcessors.LogBufferProcessor do
  require Logger

  def process(%Lobby.BufferProcessors.LogBufferProcessor{} = processor, buffer, direction) do
    case direction do
      :in -> Logger.info("Processing received buffer #{inspect(buffer)}")
      :out -> Logger.info("Processing sent buffer #{inspect(buffer)}")
    end

    {buffer, processor}
  end
end
