defmodule Lobby.Utils.ByteStream do
  @moduledoc """
  Utility struct to represent and manipulate a stream of bytes
  """
  alias Lobby.Utils.Queue

  defstruct buffers: Queue.new(), remaining: 0

  @type t :: %__MODULE__{
          buffers: Queue.t(binary),
          remaining: non_neg_integer
        }

  def new do
    %__MODULE__{buffers: Queue.new(), remaining: 0}
  end

  def push_buffer(%__MODULE__{buffers: buffers, remaining: remaining} = stream, buffer)
      when is_binary(buffer) do
    size = byte_size(buffer)

    if size > 0 do
      buffers = buffers |> Queue.push_back(buffer)
      %{stream | buffers: buffers, remaining: remaining + size}
    else
      stream
    end
  end

  def remaining(%__MODULE__{remaining: remaining}), do: remaining

  def peek(%__MODULE__{} = stream, length), do: read(stream, length, true)

  def read(%__MODULE__{} = stream, length), do: read(stream, length, false)

  defp read(%__MODULE__{} = stream, length, keep) when length < 0,
    do: raise(ArgumentError, message: "length must be positive")

  defp read(%__MODULE__{} = stream, 0, keep), do: {<<>>, stream}

  defp read(%__MODULE__{buffers: buffers, remaining: remaining} = stream, length, keep)
       when length > remaining,
       do: {:insufficient, stream}

  defp read(%__MODULE__{buffers: buffers, remaining: remaining} = stream, length, keep) do
    {result, new_buffers} = read_internal(buffers, length, <<>>)

    if keep do
      {result, stream}
    else
      stream = %{stream | buffers: new_buffers, remaining: remaining - length}
      {result, stream}
    end
  end

  defp read_internal(%Queue{} = buffers, remaining, result) do
    {buffer, buffers} = Queue.pop_front(buffers)
    size = byte_size(buffer)

    #    cond do
    #      remaining > size ->
    #        read_internal(buffers, remaining - size, [result, buffer])
    #
    #      remaining == size ->
    #        {IO.iodata_to_binary([result, buffer]), buffers}
    #
    #      true ->
    #        <<truncated::binary-size(remaining), rest::binary>> = buffer
    #
    #        buffers = buffers |> Queue.push_front(rest)
    #        result = IO.iodata_to_binary([result, truncated])
    #        {result, buffers}
    #    end

    if remaining > size do
      read_internal(buffers, remaining - size, [result, buffer])
    else
      <<truncated::binary-size(remaining), rest::binary>> = buffer

      buffers =
        if byte_size(rest) == 0 do
          buffers
        else
          buffers |> Queue.push_front(rest)
        end

      result = IO.iodata_to_binary([result, truncated])
      {result, buffers}
    end
  end
end
