defmodule Lobby.BaseClient do
  @moduledoc """
  Base functions to interact with a client
  """
  import Lobby.Protocol.Utils
  alias Lobby.Transport.Connection
  alias Lobby.ClientState
  alias Lobby.ClientRegistry
  alias Lobby.LobbyRegistry
  alias Lobby.Lobbies.LobbyServer
  alias Lobby.Messages.FatalError
  require Lobby

  @disconnect_delay_millis Lobby.compile_env!(:disconnect_delay_millis)

  def send_message(client, message) do
    GenServer.cast(client, {:send_message, message})
  end

  def receive_message(client, message) do
    GenServer.cast(client, {:receive_message, message})
  end

  def disconnect(%ClientState{conn: conn, user: user} = state, error_message)
      when is_binary(error_message) do
    message = %FatalError{message: error_message}
    conn = Connection.send_packet(conn, message_to_packet!(message))
    {_, conn} = Connection.flush(conn)

    notify_disconnected(state)

    Process.send_after(self(), :close, @disconnect_delay_millis)
    %{state | conn: conn}
  end

  def notify_disconnected(%ClientState{conn: conn, user: user} = state) do
    if user != nil do
      ClientRegistry.client_disconnected(user.id)
      lobby_id = ClientRegistry.get_lobby_id!(user.id)

      if lobby_id != nil do
        LobbyRegistry.if_online(lobby_id, fn lobby_pid ->
          :ok = LobbyServer.member_presence_update(lobby_pid, user.user_tag, false)
        end)
      end
    end
  end
end
