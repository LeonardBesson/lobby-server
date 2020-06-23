defmodule Lobby.MessageHandler do
  @moduledoc """
  Behaviour representing a module which is interested in various messages
  and can handle them.
  """
  alias Lobby.ClientState

  @doc """
  Returns the list of message types which can be handled.
  """
  @callback message_types :: list(atom)

  @doc """
  Handle the given message along with connection state from which it is received.
  Returns the new connection state.
  """
  @callback handle(message_type :: atom, message :: map, state :: ClientState.t()) ::
              new_state
            when new_state: ClientState.t()

  defmacro __using__(types) do
    quote location: :keep do
      @behaviour Lobby.MessageHandler
      @before_compile Lobby.MessageHandler

      import Lobby.BaseClient
      alias Lobby.ClientState
      require Logger

      def message_types do
        unquote(types)
      end
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def handle(message_type, _message, %ClientState{} = state) do
        Logger.error(
          "Registered type: #{inspect(message_type)} but def handle(#{inspect(message_type)}, message, state) is missing"
        )

        if Lobby.get_env!(:raise_on_missing_handler) do
          raise ArgumentError, message: "Missing message handling function"
        else
          state
        end
      end
    end
  end
end
