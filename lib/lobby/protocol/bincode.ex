defmodule Lobby.Protocol.Bincode do
  @moduledoc """
  A serialization module compatible with Rust's bincode (https://github.com/servo/bincode).
  """
  for int_type <- [:u8, :u16, :u32, :u64, :u128] do
    {size, ""} = to_string(int_type) |> String.trim_leading("u") |> Integer.parse()

    def serialize(value, unquote(int_type)) do
      <<value::little-integer-size(unquote(size))>>
    end

    def deserialize(
          <<value::little-integer-size(unquote(size)), rest::binary>>,
          unquote(int_type)
        ) do
      {value, rest}
    end
  end

  def serialize(value, :string) do
    <<
      byte_size(value)::little-integer-size(64),
      value::binary
    >>
  end

  def serialize(value, {:list, inner}) when is_list(value) do
    serialize(value, 0, <<>>, {:list, inner})
  end

  def serialize([], length, result, {:list, inner}) do
    <<
      length::little-integer-size(64),
      IO.iodata_to_binary(result)::binary
    >>
  end

  def serialize([head | tail], length, result, {:list, inner}) do
    serialized = serialize(head, inner)
    result = [result, serialized]
    serialize(tail, length + 1, result, {:list, inner})
  end

  def serialize(value, type) do
    raise ArgumentError,
      message: "Cannot serialize value #{inspect(value)} into type #{inspect(type)}"
  end

  def deserialize(
        <<
          string_size::little-integer-size(64),
          content::binary-size(string_size),
          rest::binary
        >>,
        :string
      ) do
    {content, rest}
  end

  def deserialize(
        <<
          size::little-integer-size(64),
          rest::binary
        >>,
        {:list, inner}
      ) do
    deserialize(rest, size, [], {:list, inner})
  end

  def deserialize(rest, 0, result, {:list, inner}) do
    result = Enum.reverse(result)
    {result, rest}
  end

  def deserialize(rest, remaining, result, {:list, inner}) do
    {deserialized, rest} = deserialize(rest, inner)
    result = [deserialized | result]

    deserialize(rest, remaining - 1, result, {:list, inner})
  end

  def deserialize(value, type) do
    raise ArgumentError,
      message: "Cannot deserialize value #{inspect(value)} into type #{inspect(type)}"
  end
end
