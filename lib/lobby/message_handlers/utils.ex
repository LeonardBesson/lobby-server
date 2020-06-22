defmodule Lobby.MessageHandlers.Utils do
  @moduledoc """
  Utilities to define and manipulate message handlers.
  """
  import Lobby.Protocol.Utils
  alias Lobby.Connection
  alias Lobby.ClientConn.State
  alias Lobby.Messages.FatalError
  require Lobby

  @disconnect_delay_millis Lobby.compile_env!(:disconnect_delay_millis)

  defmacro register_message_handlers do
    %Macro.Env{module: caller_module} = __CALLER__

    handlers = Lobby.get_env!(:message_handlers)

    message_handlers =
      Enum.reduce(handlers, %{}, fn handler, handlers ->
        Enum.reduce(handler.message_types(), handlers, fn type, handlers ->
          Map.update(handlers, type, [handler], fn existing_handlers ->
            [handler | existing_handlers]
          end)
        end)
      end)
      |> Enum.map(fn {type, handlers} -> {type, Enum.reverse(handlers)} end)
      |> Enum.into(%{})

    Module.put_attribute(caller_module, :message_handlers, message_handlers)
  end

  def send_message(client, message) do
    GenServer.cast(client, {:send_message, message})
  end

  def receive_message(client, message) do
    GenServer.cast(client, {:receive_message, message})
  end

  def disconnect(%State{conn: conn, user: user} = state, error_message)
      when is_binary(error_message) do
    message = %FatalError{message: error_message}
    conn = Connection.send_packet(conn, message_to_packet!(message))
    {_, conn} = Connection.flush(conn)

    if user != nil do
      ClientRegistry.client_disconnected(user.id)
    end

    Process.send_after(self(), :close, @disconnect_delay_millis)
    %{state | conn: conn}
  end
end
