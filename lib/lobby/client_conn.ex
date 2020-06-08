defmodule Lobby.ClientConn do
  @moduledoc """
  The process handling the client connection. It is the entry point for which messages
  are sent to and receiving from a client.
  """
  use GenServer
  require Lobby
  alias Lobby.Connection
  alias Lobby.Protocol.Packet
  import Lobby.Protocol.PacketUtils
  alias Lobby.Messages.PacketInit
  alias Lobby.Messages.FatalError
  alias Lobby.BufferProcessors.LogBufferProcessor
  require Logger

  @behaviour :ranch_protocol

  @protocol_version Lobby.compile_env!(:protocol_version)
  @app_version Lobby.compile_env!(:app_version)

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

    :gen_server.enter_loop(__MODULE__, [], %{conn: conn})
  end

  def send_message(client, message) do
    GenServer.cast(client, {:send_message, message})
  end

  @impl GenServer
  def handle_cast({:send_message, message}, %{conn: conn} = state) do
    Logger.info("Sending message: #{inspect(message)}")
    packet = message_to_packet(message)
    conn = Connection.send_packet(conn, packet)
    send(self(), :flush)

    {:noreply, %{state | conn: conn}}
  end

  @impl GenServer
  def handle_info({:tcp, _, message}, %{conn: conn} = state) do
    Logger.info("Received #{inspect(message)} from #{conn.peername}")

    conn = Connection.received(conn, message) |> Connection.continue_receiving()

    # TODO: Instead of sending this all the time, we should
    # schedule a flush at most every X ms using Process.send_after().
    # That way we don't risk surcharging the mailbox with constant flush
    # messages that have no effect (i.e if we send 2 packets in a row it will
    # flush 2 times but the second one is useless.
    # tickrate of 60 should be enough
    send(self(), :flush)

    {:noreply, %{state | conn: conn}}
  end

  @impl GenServer
  def handle_info({:tcp_closed, _}, %{conn: conn} = state) do
    Logger.info("Peer #{conn.peername} disconnected")

    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _, reason}, %{conn: conn} = state) do
    Logger.info("Error with peer #{conn.peername}: #{inspect(reason)}")

    {:stop, :normal, state}
  end

  def handle_info(:flush, %{conn: conn} = state) do
    {incoming_packets, conn} = Connection.flush(conn)
    conn = Enum.reduce(incoming_packets, conn, &handle_incoming_packet/2)

    {:noreply, %{state | conn: conn}}
  end

  defp handle_incoming_packet(%Packet{} = packet, %Connection{} = conn) do
    Logger.info("Received incoming packet #{inspect(packet)}")

    case packet.packet_type do
      :packet_init ->
        msg = PacketInit.deserialize(packet.data)

        cond do
          msg.protocol_version != @protocol_version ->
            disconnect(conn, "Invalid protocol version #{msg.protocol_version}")

          msg.app_version != @app_version ->
            disconnect(conn, "Invalid application version #{msg.protocol_version}")

          true ->
            conn
        end

      :fatal_error ->
        msg = FatalError.deserialize(packet.data)
        Logger.error("Fatal error from peer #{conn.peername}: #{msg.message}")
        conn

      type ->
        Logger.error("Unknown packet type #{type}")
        conn
    end
  end

  defp disconnect(conn, error_message) when is_binary(error_message) do
    message = %FatalError{message: error_message}
    conn = Connection.send_packet(conn, message_to_packet(message))
    {_, conn} = Connection.flush(conn)
    Connection.shutdown(conn)
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
