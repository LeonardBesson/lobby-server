defmodule Lobby.ClientConn do
  @moduledoc """
  The process handling the client connection. It is the entry point for which messages
  are sent to and receiving from a client.
  """
  use GenServer
  require Lobby
  alias Lobby.Accounts
  alias Lobby.Accounts.User
  alias Lobby.Connection
  alias Lobby.Protocol.Packet
  import Lobby.Protocol.Utils
  alias Lobby.Messages.PacketInit
  alias Lobby.Messages.FatalError
  alias Lobby.Messages.AuthenticationRequest
  alias Lobby.Messages.AuthenticationResponse
  alias Lobby.BufferProcessors.LogBufferProcessor
  alias Lobby.Utils.Crypto
  require Logger

  @behaviour :ranch_protocol

  @protocol_version Lobby.compile_env!(:protocol_version)
  @app_version Lobby.compile_env!(:app_version)

  @auth_timeout_millis 2 * 60_000
  @disconnect_delay_millis 100

  defmodule State do
    defstruct conn: nil, flush_timer: nil, auth_timeout_timer: nil

    @type t :: %__MODULE__{
            conn: Connection.t(),
            flush_timer: reference,
            auth_timeout_timer: reference
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

      :authentication_request ->
        msg = packet_to_message!(packet, AuthenticationRequest)

        if conn.state != :authenticating do
          disconnect(state, "Packets out of order")
        else
          {response, state} =
            case Accounts.authenticate(msg.email, msg.password) do
              {:ok, %User{} = user} ->
                conn = %{conn | state: :running}
                Process.cancel_timer(state.auth_timeout_timer)
                state = %{state | auth_timeout_timer: nil}
                {%AuthenticationResponse{session_token: Crypto.gen_session_token()}, state}

              {:error, _} ->
                {%AuthenticationResponse{error_code: "invalid_credentials"}, state}
            end

          send_message(self(), response)
          state
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
