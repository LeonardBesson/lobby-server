defmodule Lobby.Transport.Connection do
  @moduledoc """
  Represents a physical TCP connection
  """
  alias Lobby.Protocol.Packet
  alias Lobby.Protocol.BufferProcessor
  alias Lobby.Protocol.PacketDecoder
  alias Lobby.Protocol.PacketEncoder
  alias Lobby.Utils.Queue

  require Logger

  defstruct socket: nil,
            transport: nil,
            peername: nil,
            state: :initializing,
            buffer_processors: [],
            tcp_encoder: PacketEncoder.new(8 * 1024),
            tcp_decoder: PacketDecoder.new(),
            unprocessed_out: Queue.new(),
            processed_out: Queue.new(),
            unprocessed_in: Queue.new(),
            processed_in: Queue.new()

  @type state :: :initializing | :authenticating | :running | :closed

  @type t :: %__MODULE__{
          socket: any(),
          transport: module(),
          peername: String.t(),
          state: state(),
          buffer_processors: list(list(BufferProcessor.t())),
          tcp_encoder: PacketEncoder.t(),
          tcp_decoder: PacketDecoder.t(),
          unprocessed_out: Queue.t(binary),
          processed_out: Queue.t(binary),
          unprocessed_in: Queue.t(binary),
          processed_in: Queue.t(binary)
        }

  def new(socket, transport, peername) do
    %__MODULE__{socket: socket, transport: transport, peername: peername}
  end

  def add_buffer_processor(
        %__MODULE__{buffer_processors: buffer_processors} = conn,
        buffer_processor
      ) do
    buffer_processors = [buffer_processor | buffer_processors]
    %{conn | buffer_processors: buffer_processors}
  end

  def continue_receiving(%__MODULE__{socket: socket, transport: transport} = conn) do
    transport.setopts(socket, active: :once)
    conn
  end

  def received(%__MODULE__{unprocessed_in: unprocessed_in} = conn, message) do
    unprocessed_in = Queue.push_back(unprocessed_in, message)
    %{conn | unprocessed_in: unprocessed_in}
  end

  def send_packet(%__MODULE__{tcp_encoder: tcp_encoder} = conn, %Packet{} = packet) do
    Logger.info("Sending packet #{inspect(packet)}")
    tcp_encoder = PacketEncoder.add_packet(tcp_encoder, packet)
    %{conn | tcp_encoder: tcp_encoder}
  end

  def flush(%__MODULE__{} = conn) do
    conn
    # Out
    |> encode()
    |> process_out()
    |> write_outgoing_packets()
    # In
    |> process_in()
    |> decode()
    |> incoming_packets()
  end

  defp encode(%__MODULE__{tcp_encoder: tcp_encoder, unprocessed_out: unprocessed_out} = conn) do
    {buffer, tcp_encoder} = PacketEncoder.next_buffer(tcp_encoder)

    if buffer == nil do
      %{conn | tcp_encoder: tcp_encoder}
    else
      unprocessed_out = Queue.push_back(unprocessed_out, buffer)
      encode(%{conn | tcp_encoder: tcp_encoder, unprocessed_out: unprocessed_out})
    end
  end

  defp decode(%__MODULE__{tcp_decoder: tcp_decoder, processed_in: processed_in} = conn) do
    {buffer, processed_in} = Queue.pop_front(processed_in)

    if buffer == nil do
      %{conn | processed_in: processed_in}
    else
      tcp_decoder = PacketDecoder.push_buffer(tcp_decoder, buffer)
      decode(%{conn | processed_in: processed_in, tcp_decoder: tcp_decoder})
    end
  end

  def process_out(
        %__MODULE__{unprocessed_out: unprocessed_out, processed_out: processed_out} = conn
      ) do
    {buffer, unprocessed_out} = Queue.pop_front(unprocessed_out)

    if buffer == nil do
      %{conn | unprocessed_out: unprocessed_out}
    else
      {buffer, conn} = apply_buffer_processors(conn, buffer, :out)
      processed_out = Queue.push_back(processed_out, buffer)
      process_out(%{conn | unprocessed_out: unprocessed_out, processed_out: processed_out})
    end
  end

  def process_in(%__MODULE__{unprocessed_in: unprocessed_in, processed_in: processed_in} = conn) do
    {buffer, unprocessed_in} = Queue.pop_front(unprocessed_in)

    if buffer == nil do
      %{conn | unprocessed_in: unprocessed_in}
    else
      {buffer, conn} = apply_buffer_processors(conn, buffer, :in)
      processed_in = Queue.push_back(processed_in, buffer)
      process_out(%{conn | unprocessed_in: unprocessed_in, processed_in: processed_in})
    end
  end

  defp apply_buffer_processors(
         %__MODULE__{buffer_processors: buffer_processors} = conn,
         buffer,
         direction
       ) do
    # Processors are applied backwards inbound and forward outbound.
    # They are prepended when inserted that's why we reverse them for out instead of in
    flip_processors = fn processors ->
      case direction do
        :in -> processors
        :out -> Enum.reverse(processors)
      end
    end

    processors = flip_processors.(buffer_processors)

    {processors, buffer} =
      Enum.map_reduce(processors, buffer, fn processor, buffer ->
        {buffer, processor} = BufferProcessor.process(processor, buffer, direction)

        {processor, buffer}
      end)

    # Reverse back to original order
    processors = flip_processors.(processors)
    {buffer, %{conn | buffer_processors: processors}}
  end

  def write_outgoing_packets(
        %__MODULE__{
          socket: socket,
          transport: transport,
          peername: peername,
          processed_out: processed_out
        } = conn
      ) do
    {buffer, processed_out} = Queue.pop_front(processed_out)
    conn = %{conn | processed_out: processed_out}

    if buffer == nil do
      conn
    else
      case transport.send(socket, buffer) do
        :ok ->
          Logger.info("Written #{inspect(buffer)} to #{conn.peername}")

          :ok

        {:error, reason} ->
          # TODO: handle
          Logger.error("Failed to send message to #{peername}, reason: #{inspect(reason)}")
      end

      write_outgoing_packets(conn)
    end
  end

  def incoming_packets(%__MODULE__{} = conn), do: incoming_packets(conn, [])

  def incoming_packets(%__MODULE__{tcp_decoder: tcp_decoder} = conn, packets) do
    {packet, tcp_decoder} = PacketDecoder.next_packet(tcp_decoder)
    conn = %{conn | tcp_decoder: tcp_decoder}

    if packet == nil do
      {Enum.reverse(packets), conn}
    else
      incoming_packets(conn, [packet | packets])
    end
  end

  def shutdown(%__MODULE__{socket: socket, transport: transport} = conn) do
    case transport.shutdown(socket, :read_write) do
      :ok -> Logger.info("Closed connection with peer: #{conn.peername}")
      {:error, reason} -> Logger.error("Error while closing connection: #{inspect(reason)}")
    end

    conn
  end
end
