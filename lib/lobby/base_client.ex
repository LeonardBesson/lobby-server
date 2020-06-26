defmodule Lobby.BaseClient do
  @moduledoc """
  Base functions to interact with a client
  """
  import Lobby.Protocol.Utils
  alias Lobby.Protocol.Message
  alias Lobby.Transport.Connection
  alias Lobby.ClientRegistry
  alias Lobby.ClientState
  alias Lobby.Messages
  alias Lobby.Messages.FatalError
  require Lobby
  require Logger

  @protocol_version Lobby.compile_env!(:protocol_version)
  @app_version Lobby.compile_env!(:app_version)

  @round_trip_threshold_warning_millis Lobby.compile_env!(:round_trip_threshold_warning_millis)
  @disconnect_delay_millis Lobby.compile_env!(:disconnect_delay_millis)

  def send_message(client, message) when is_pid(client) do
    GenServer.call(client, {:send_message, message})
  end

  def send_message(%ClientState{conn: conn} = state, message) do
    Logger.debug("Sending message: #{inspect(message)}")

    packet = message_to_packet!(message)
    conn = Connection.send_packet(conn, packet)
    state = %{state | conn: conn}

    schedule_flush(state)
  end

  def receive_message(client, message) when is_pid(client) do
    GenServer.call(client, {:receive_message, message})
  end

  def receive_message(%ClientState{} = state, message) do
    Logger.debug("Received message: #{inspect(message)}")

    type = Message.packet_type(message)
    handle_incoming_message(type, message, state)
  end

  def schedule_flush(%ClientState{flush_timer: flush_timer} = state) do
    flush_timer =
      if flush_timer == nil do
        Logger.debug("Scheduling flush")
        send(self(), :flush)
        make_ref()
      else
        Logger.debug("Flush already scheduled")
        flush_timer
      end

    %{state | flush_timer: flush_timer}
  end

  def handle_incoming_message(type, msg, %ClientState{conn: conn} = state) do
    case type do
      :packet_init ->
        cond do
          msg.protocol_version != @protocol_version ->
            disconnect(state, "Invalid protocol version #{msg.protocol_version}")

          msg.app_version != @app_version ->
            disconnect(state, "Invalid application version #{msg.protocol_version}")

          true ->
            conn = %{conn | state: :authenticating}
            %{state | conn: conn}
        end

      :fatal_error ->
        Logger.error("Fatal error from peer #{conn.peername}: #{msg.message}")
        state

      :packet_pong ->
        if msg.id != state.last_ping_id do
          Logger.error("Ping/Pong ID mismatch")
          state
        else
          now = DateTime.utc_now()
          round_trip = DateTime.diff(now, state.last_ping_time, :millisecond)

          if round_trip > @round_trip_threshold_warning_millis do
            Logger.warn("Round trip threshold exceeded: #{round_trip} ms")
          end

          %{state | round_trip_ms: round_trip, last_ping_id: nil}
        end

      _ ->
        Messages.handle(type, msg, state)
    end
  end

  def disconnect(%ClientState{conn: conn, user: user} = state, error_message)
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
