defmodule Lobby.BaseClient do
  @moduledoc """
  Base functions to interact with a client
  """
  import Lobby.Protocol.Utils
  alias Lobby.Transport.Connection
  alias Lobby.ClientRegistry
  alias Lobby.ClientConnection.State
  alias Lobby.Messages.FatalError
  require Lobby

  defmodule State do
    defstruct conn: nil,
              user: nil,
              flush_timer: nil,
              auth_timeout_timer: nil,
              ping_timer: nil,
              last_ping_id: nil,
              last_ping_time: nil,
              round_trip_ms: nil

    @type t :: %__MODULE__{
            conn: Connection.t(),
            user: map,
            flush_timer: reference,
            auth_timeout_timer: reference,
            ping_timer: reference,
            last_ping_id: String.t(),
            last_ping_time: DateTime.t(),
            round_trip_ms: non_neg_integer
          }
  end

  @disconnect_delay_millis Lobby.compile_env!(:disconnect_delay_millis)

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
