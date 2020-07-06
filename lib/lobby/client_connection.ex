defmodule Lobby.ClientConnection do
  @moduledoc """
  The process handling the client connection. It is the entry point for which messages
  are sent to and receiving from a client.
  """
  use GenServer
  import Lobby.Protocol.Utils
  import Lobby.BaseClient
  alias Lobby.ClientState
  alias Lobby.Transport.Connection
  alias Lobby.ClientRegistry
  alias Lobby.Protocol.Packet
  alias Lobby.Protocol.Message
  alias Lobby.Messages
  alias Lobby.Messages.PacketInit
  alias Lobby.Messages.PacketPing
  alias Lobby.BufferProcessors.LogBufferProcessor
  require Lobby
  require Logger

  @behaviour :ranch_protocol

  @protocol_version Lobby.compile_env!(:protocol_version)
  @app_version Lobby.compile_env!(:app_version)

  @auth_timeout_millis Lobby.compile_env!(:auth_timeout_millis)
  @ping_interval_millis Lobby.compile_env!(:ping_interval_millis)
  @ping_timeout_millis Lobby.compile_env!(:ping_timeout_millis)
  @round_trip_threshold_warning_millis Lobby.compile_env!(:round_trip_threshold_warning_millis)

  @doc """
  Starts the handler with `:proc_lib.spawn_link/3`.
  """
  @impl :ranch_protocol
  def start_link(ref, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, transport])
    {:ok, pid}
  end

  @impl GenServer
  def init(init_arg) do
    {:ok, init_arg}
  end

  @doc """
  Initiates the handler, acknowledging the connection was accepted.
  Finally it makes the existing process into a `:gen_server` process and
  enters the `:gen_server` receive loop with `:gen_server.enter_loop/3`.
  """
  def init(ref, transport) do
    # Should we handle :continue here?
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, active: :once)
    peername = stringify_peername(socket)

    Logger.info("Peer #{peername} connecting")

    conn =
      Connection.new(socket, transport, peername)
      |> Connection.add_buffer_processor(%LogBufferProcessor{})

    # Init handshake
    packet_init = %PacketInit{
      protocol_version: @protocol_version,
      app_version: @app_version
    }

    send_message(self(), packet_init)

    auth_timeout_timer = Process.send_after(self(), :auth_timeout, @auth_timeout_millis)

    :gen_server.enter_loop(__MODULE__, [], %ClientState{
      conn: conn,
      auth_timeout_timer: auth_timeout_timer
    })
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

  @impl GenServer
  def handle_cast({:send_message, message}, %ClientState{conn: conn} = state) do
    Logger.debug("Sending message: #{inspect(message)}")
    packet = message_to_packet!(message)
    conn = Connection.send_packet(conn, packet)
    state = schedule_flush(state)

    {:noreply, %{state | conn: conn}}
  end

  @impl GenServer
  def handle_cast({:receive_message, message}, %ClientState{} = state) do
    Logger.debug("Received message: #{inspect(message)}")

    type = Message.packet_type(message)
    state = handle_incoming_message(type, message, state)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:tcp, _, message}, %ClientState{conn: conn} = state) do
    Logger.debug("Received #{inspect(message)} from #{conn.peername}")

    conn = Connection.received(conn, message) |> Connection.continue_receiving()

    state = schedule_flush(state)

    {:noreply, %{state | conn: conn}}
  end

  @impl GenServer
  def handle_info({:tcp_closed, _}, %ClientState{conn: conn, user: user} = state) do
    Logger.info("Peer #{conn.peername} disconnected")

    notify_disconnected(state)

    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _, reason}, %ClientState{conn: conn, user: user} = state) do
    Logger.error("Error with peer #{conn.peername}: #{inspect(reason)}")

    notify_disconnected(state)

    {:stop, :normal, state}
  end

  def handle_info(:flush, %ClientState{conn: conn} = state) do
    Logger.debug("Flushing")
    {incoming_packets, conn} = Connection.flush(conn)
    state = %{state | conn: conn}
    state = Enum.reduce(incoming_packets, state, &handle_incoming_packet/2)

    {:noreply, %{state | flush_timer: nil}}
  end

  def handle_info(:ping_client, %ClientState{} = state) do
    Logger.debug("Ping triggered")

    if state.last_ping_id != nil do
      elapsed_millis = DateTime.diff(DateTime.utc_now(), state.last_ping_time, :millisecond)

      if elapsed_millis > @ping_timeout_millis do
        state = disconnect(state, "Timeout")
        {:noreply, state}
      else
        ping_timer = Process.send_after(self(), :ping_client, @ping_interval_millis)
        {:noreply, %{state | ping_timer: ping_timer}}
      end
    else
      now = DateTime.utc_now()
      id = Ecto.UUID.generate()
      send_message(self(), %PacketPing{id: id, peer_time: DateTime.to_unix(now, :millisecond)})
      ping_timer = Process.send_after(self(), :ping_client, @ping_interval_millis)
      {:noreply, %{state | ping_timer: ping_timer, last_ping_id: id, last_ping_time: now}}
    end
  end

  def handle_info(:auth_timeout, %ClientState{conn: conn} = state) do
    Logger.warn("Auth timed out for peer #{conn.peername}")
    state = disconnect(state, "Authentication timed out")

    {:noreply, state}
  end

  def handle_info(:close, %ClientState{conn: conn, user: user} = state) do
    conn = Connection.shutdown(conn)

    notify_disconnected(state)

    {:stop, :normal, %{state | conn: conn}}
  end

  defp handle_incoming_packet(%Packet{} = packet, %ClientState{} = state) do
    Logger.debug("Received incoming packet #{inspect(packet)}")

    packet_info = Packet.get!(packet.packet_type)
    msg = packet_to_message!(packet, packet_info.message_module)
    {handled, state} = handle_incoming_message(packet.packet_type, msg, state)

    if handled do
      state
    else
      Messages.handle(packet.packet_type, msg, state)
    end
  end

  defp handle_incoming_message(type, msg, %ClientState{conn: conn} = state) do
    case type do
      :packet_init ->
        cond do
          msg.protocol_version != @protocol_version ->
            disconnect(state, "Invalid protocol version #{msg.protocol_version}")

          msg.app_version != @app_version ->
            disconnect(state, "Invalid application version #{msg.protocol_version}")

          true ->
            conn = %{conn | state: :authenticating}
            {true, %{state | conn: conn}}
        end

      :fatal_error ->
        Logger.error("Fatal error from peer #{conn.peername}: #{msg.message}")
        {true, state}

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

          {true, %{state | round_trip_ms: round_trip, last_ping_id: nil}}
        end

      _ ->
        {false, state}
    end
  end

  defp stringify_peername(socket) do
    {:ok, {addr, port}} = :inet.peername(socket)

    address =
      addr
      |> :inet_parse.ntoa()
      |> to_string()

    "#{address}:#{port}"
  end
end
