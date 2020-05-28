defmodule Lobby.ClientConn do
  @moduledoc """
  A simple TCP protocol handler that echoes all messages received.
  """
  use GenServer

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

    :gen_server.enter_loop(__MODULE__, [], %{
      socket: socket,
      transport: transport,
      peername: peername
    })
  end

  @impl GenServer
  def handle_info(
        {:tcp, _, message},
        %{socket: socket, transport: transport, peername: peername} = state
      ) do
    Logger.info("Received #{inspect(message)} from #{peername}")

    case transport.send(socket, message) do
      :ok ->
        :ok

      {:error, reason} ->
        Logger.error("Failed to send message to #{peername}, reason: #{inspect(reason)}")
    end

    transport.setopts(socket, active: :once)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:tcp_closed, _}, %{peername: peername} = state) do
    Logger.info("Peer #{peername} disconnected")

    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _, reason}, %{peername: peername} = state) do
    Logger.info("Error with peer #{peername}: #{inspect(reason)}")

    {:stop, :normal, state}
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
