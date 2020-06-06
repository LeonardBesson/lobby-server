defmodule Lobby.Protocol.Bincode do
  @moduledoc """
  A serialization module compatible with Rust's bincode (https://github.com/servo/bincode).
  """

  def serialize(field, :u16) do
    quote do: var!(message).unquote(Macro.var(field, nil)) :: little - integer - size(16)
  end

  def serialize(_, _, type) do
    raise ArgumentError, message: "Cannot serialize unknown field type #{inspect(type)}"
  end

  def deserialize(field, :u16) do
    quote do: var!(unquote(Macro.var(field, nil))) :: little - integer - size(16)
  end

  def deserialize(_, type) do
    raise ArgumentError, message: "Cannot deserialize unknown field type #{inspect(type)}"
  end
end
