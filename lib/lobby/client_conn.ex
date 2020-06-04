defmodule Lobby.ClientConn do
  @moduledoc """
  A simple TCP protocol handler that echoes all messages received.
  """
  use GenServer
  alias Lobby.Connection
  alias Lobby.Protocol.PacketDecoder
  alias Lobby.Protocol.PacketEncoder
  require Logger

  @behaviour :ranch_protocol

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

    state = %{
      conn: Connection.new(socket, transport, peername)
    }

    :gen_server.enter_loop(__MODULE__, [], state)
  end

  @impl GenServer
  def handle_info(
        {:tcp, _, message},
        %{conn: conn} = state
      ) do
    Logger.info("Received #{inspect(message)} from #{conn.peername}")

    conn = Connection.received(conn, message) |> Connection.enable_receive_once()

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
    conn = Connection.flush(conn)
    {:noreply, %{state | conn: conn}}
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
