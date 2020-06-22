defmodule Lobby.Messages do
  @moduledoc """
  Entry point module for interacting with messages.
  Holds the mapping of message handlers.
  """
  require Logger
  import Lobby.MessageHandlers.Utils
  alias Lobby.BaseClient

  register_message_handlers()

  @doc """
  Submit the given message and connection state through all the handlers
  associated with its type.
  Returns the updated connection state.
  """
  def handle(message_type, message, %BaseClient.State{} = state) when is_atom(message_type) do
    case @message_handlers do
      %{^message_type => handlers} ->
        Enum.reduce(handlers, state, fn handler, state ->
          handler.handle(message_type, message, state)
        end)

      _ ->
        Logger.error("No MessageHandler registered for message type #{inspect(message_type)}")
        state
    end
  end
end
