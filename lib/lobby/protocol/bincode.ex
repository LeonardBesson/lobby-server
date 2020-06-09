defmodule Lobby.Protocol.Bincode do
  @moduledoc """
  A serialization module compatible with Rust's bincode (https://github.com/servo/bincode).
  """
  # Unsigned
  for int_type <- [:u8, :u16, :u32, :u64, :u128] do
    {size, ""} = to_string(int_type) |> String.trim_leading("u") |> Integer.parse()

    def serialize(value, unquote(int_type)) when value < 0 do
      raise ArgumentError,
        message:
          "Attempt to serialize negative integer #{inspect(value)} into #{unquote(int_type)}"
    end

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

  # Signed
  for int_type <- [:i8, :i16, :i32, :i64, :i128] do
    {size, ""} = to_string(int_type) |> String.trim_leading("i") |> Integer.parse()

    def serialize(value, unquote(int_type)) do
      <<value::little-integer-signed-size(unquote(size))>>
    end

    def deserialize(
          <<value::little-integer-signed-size(unquote(size)), rest::binary>>,
          unquote(int_type)
        ) do
      {value, rest}
    end
  end

  # Float
  for float_type <- [:f32, :f64] do
    {size, ""} = to_string(float_type) |> String.trim_leading("f") |> Integer.parse()

    def serialize(value, unquote(float_type)) do
      <<value::little-float-size(unquote(size))>>
    end

    def deserialize(
          <<value::little-float-size(unquote(size)), rest::binary>>,
          unquote(float_type)
        ) do
      {value, rest}
    end
  end

  # Bool
  for boolean <- [true, false] do
    v = if boolean, do: 1, else: 0

    def serialize(unquote(boolean), :bool) do
      <<unquote(v)::size(8)>>
    end

    def deserialize(<<unquote(v)::size(8), rest::binary>>, :bool) do
      {unquote(boolean), rest}
    end
  end

  # String
  def serialize(value, :string) do
    <<
      byte_size(value)::little-integer-size(64),
      value::binary
    >>
  end

  # List
  def serialize(list, {:list, inner}) when is_list(list) do
    serialize(list, 0, <<>>, {:list, inner})
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

  # Map
  def serialize(map, {:map, {key_type, value_type}}) when is_map(map) do
    serialize(map, Map.keys(map), 0, <<>>, {:map, {key_type, value_type}})
  end

  def serialize(map, [], length, result, {:map, {key_type, value_type}}) do
    <<
      length::little-integer-size(64),
      IO.iodata_to_binary(result)::binary
    >>
  end

  def serialize(map, [key | keys], length, result, {:map, {key_type, value_type}}) do
    serialized_key = serialize(key, key_type)
    serialized_value = serialize(map[key], value_type)
    result = [result, serialized_key, serialized_value]

    serialize(map, keys, length + 1, result, {:map, {key_type, value_type}})
  end

  def serialize(value, type) do
    raise ArgumentError,
      message: "Cannot serialize value #{inspect(value)} into type #{inspect(type)}"
  end

  # String
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

  # List
  def deserialize(<<size::little-integer-size(64), rest::binary>>, {:list, inner}) do
    deserialize(rest, size, [], {:list, inner})
  end

  def deserialize(rest, 0, result, {:list, _}) do
    result = Enum.reverse(result)
    {result, rest}
  end

  def deserialize(rest, remaining, result, {:list, inner}) do
    {deserialized, rest} = deserialize(rest, inner)
    result = [deserialized | result]

    deserialize(rest, remaining - 1, result, {:list, inner})
  end

  # Map
  def deserialize(<<size::little-integer-size(64), rest::binary>>, {:map, {key_type, value_type}}) do
    deserialize(rest, size, %{}, {:map, {key_type, value_type}})
  end

  def deserialize(rest, 0, result, {:map, {_, _}}) do
    {result, rest}
  end

  def deserialize(rest, remaining, result, {:map, {key_type, value_type}}) do
    {deserialized_key, rest} = deserialize(rest, key_type)
    {deserialized_value, rest} = deserialize(rest, value_type)

    result = Map.put(result, deserialized_key, deserialized_value)

    deserialize(rest, remaining - 1, result, {:map, {key_type, value_type}})
  end

  def deserialize(value, type) do
    raise ArgumentError,
      message: "Cannot deserialize value #{inspect(value)} into type #{inspect(type)}"
  end
end
