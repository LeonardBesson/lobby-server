defmodule Lobby.ClientConn do
  @moduledoc """
  The process handling the client connection. It is the entry point for which messages
  are sent to and receiving from a client.
  """
  use GenServer
  require Lobby
  alias Lobby.Accounts
  alias Lobby.Accounts.User
  alias Lobby.Bans
  alias Lobby.Connection
  alias Lobby.Protocol.Packet
  import Lobby.Protocol.Structs
  import Lobby.Protocol.Utils
  alias Lobby.Messages.PacketInit
  alias Lobby.Messages.PacketPing
  alias Lobby.Messages.PacketPong
  alias Lobby.Messages.FatalError
  alias Lobby.Messages.AuthenticationRequest
  alias Lobby.Messages.AuthenticationResponse
  alias Lobby.BufferProcessors.LogBufferProcessor
  alias Lobby.Utils.Crypto
  require Logger

  @behaviour :ranch_protocol

  @protocol_version Lobby.compile_env!(:protocol_version)
  @app_version Lobby.compile_env!(:app_version)

  @auth_timeout_millis Lobby.compile_env!(:auth_timeout_millis)
  @disconnect_delay_millis Lobby.compile_env!(:disconnect_delay_millis)
  @ping_interval_millis Lobby.compile_env!(:ping_interval_millis)
  @ping_timeout_millis Lobby.compile_env!(:ping_timeout_millis)
  @round_trip_threshold_warning_millis Lobby.compile_env!(:round_trip_threshold_warning_millis)
  @reveal_ban_reason Lobby.compile_env!(:reveal_ban_reason)

  defmodule State do
    defstruct conn: nil,
              flush_timer: nil,
              auth_timeout_timer: nil,
              ping_timer: nil,
              last_ping_id: nil,
              last_ping_time: nil,
              round_trip_ms: nil

    @type t :: %__MODULE__{
            conn: Connection.t(),
            flush_timer: reference,
            auth_timeout_timer: reference,
            ping_timer: reference,
            last_ping_id: String.t(),
            last_ping_time: DateTime.t(),
            round_trip_ms: non_neg_integer
          }
  end

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

    :gen_server.enter_loop(__MODULE__, [], %State{
      conn: conn,
      auth_timeout_timer: auth_timeout_timer
    })
  end

  def send_message(client, message) do
    GenServer.cast(client, {:send_message, message})
  end

  def schedule_flush(%State{flush_timer: flush_timer} = state) do
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
  def handle_cast({:send_message, message}, %State{conn: conn} = state) do
    Logger.debug("Sending message: #{inspect(message)}")
    packet = message_to_packet!(message)
    conn = Connection.send_packet(conn, packet)
    state = schedule_flush(state)

    {:noreply, %{state | conn: conn}}
  end

  @impl GenServer
  def handle_info({:tcp, _, message}, %State{conn: conn} = state) do
    Logger.debug("Received #{inspect(message)} from #{conn.peername}")

    conn = Connection.received(conn, message) |> Connection.continue_receiving()

    state = schedule_flush(state)

    {:noreply, %{state | conn: conn}}
  end

  @impl GenServer
  def handle_info({:tcp_closed, _}, %State{conn: conn} = state) do
    Logger.info("Peer #{conn.peername} disconnected")

    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _, reason}, %State{conn: conn} = state) do
    Logger.error("Error with peer #{conn.peername}: #{inspect(reason)}")

    {:stop, :normal, state}
  end

  def handle_info(:flush, %State{conn: conn} = state) do
    Logger.debug("Flushing")
    {incoming_packets, conn} = Connection.flush(conn)
    state = %{state | conn: conn}
    state = Enum.reduce(incoming_packets, state, &handle_incoming_packet/2)

    {:noreply, %{state | flush_timer: nil}}
  end

  def handle_info(:ping_client, %State{conn: conn} = state) do
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

  def handle_info(:auth_timeout, %State{conn: conn} = state) do
    Logger.warn("Auth timed out for peer #{conn.peername}")
    state = disconnect(state, "Authentication timed out")

    {:noreply, state}
  end

  def handle_info(:close, %State{conn: conn} = state) do
    conn = Connection.shutdown(conn)

    {:stop, :normal, %{state | conn: conn}}
  end

  defp handle_incoming_packet(%Packet{} = packet, %State{conn: conn} = state) do
    Logger.debug("Received incoming packet #{inspect(packet)}")

    case packet.packet_type do
      :packet_init ->
        msg = packet_to_message!(packet, PacketInit)

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
        msg = packet_to_message!(packet, FatalError)
        Logger.error("Fatal error from peer #{conn.peername}: #{msg.message}")
        state

      :packet_pong ->
        msg = packet_to_message!(packet, PacketPong)

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

      :authentication_request ->
        msg = packet_to_message!(packet, AuthenticationRequest)

        if conn.state != :authenticating do
          disconnect(state, "Packets out of order")
        else
          case Accounts.authenticate(msg.email, msg.password) do
            {:ok, %User{} = user} ->
              case Bans.validate(user) do
                {:banned, reason, expire_at} ->
                  ban_message =
                    if @reveal_ban_reason do
                      "Banned until #{expire_at}.\n#{reason}"
                    else
                      "Banned until #{expire_at}"
                    end

                  disconnect(state, ban_message)

                :valid ->
                  conn = %{conn | state: :running}

                  if state.auth_timeout_timer != nil do
                    Process.cancel_timer(state.auth_timeout_timer)
                  end

                  ping_timer = Process.send_after(self(), :ping_client, @ping_interval_millis)
                  state = %{state | conn: conn, auth_timeout_timer: nil, ping_timer: ping_timer}

                  send_message(self(), %AuthenticationResponse{
                    session_token: Crypto.gen_session_token(),
                    user_profile: get_user_profile(user)
                  })

                  state
              end

            {:error, _} ->
              send_message(self(), %AuthenticationResponse{error_code: "invalid_credentials"})
              state
          end
        end

      type ->
        Logger.error("Unknown packet type #{type}")
        state
    end
  end

  defp disconnect(%State{conn: conn} = state, error_message) when is_binary(error_message) do
    message = %FatalError{message: error_message}
    conn = Connection.send_packet(conn, message_to_packet!(message))
    {_, conn} = Connection.flush(conn)
    Process.send_after(self(), :close, @disconnect_delay_millis)
    %{state | conn: conn}
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
