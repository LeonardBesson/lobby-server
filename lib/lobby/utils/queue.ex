defmodule Lobby.Utils.Queue do
  @moduledoc """
  A wrapper for Erlang's queue
  """

  defstruct inner: nil, length: 0

  @type t :: %__MODULE__{
          inner: {list(any), list(any)},
          length: non_neg_integer
        }
  @type t(inner) :: %__MODULE__{
          inner: {list(inner), list(inner)},
          length: non_neg_integer
        }

  @spec new :: t()
  def new do
    inner = :queue.new()
    %__MODULE__{inner: inner, length: 0}
  end

  def push_back(%__MODULE__{inner: inner, length: length} = queue, value) do
    inner = :queue.in(value, inner)
    %{queue | inner: inner, length: length + 1}
  end

  def pop_front(%__MODULE__{inner: inner, length: length} = queue) do
    case :queue.out(inner) do
      {:empty, _} ->
        {nil, queue}

      {{:value, value}, inner} ->
        {value, %{queue | inner: inner, length: length - 1}}
    end
  end

  def length(%__MODULE__{length: length} = queue) do
    length
  end

  def empty?(%__MODULE__{length: length} = queue) do
    length == 0
  end
end
