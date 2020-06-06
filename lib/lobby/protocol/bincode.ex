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

  def deserialize(value, type) do
    raise ArgumentError,
      message: "Cannot deserialize value #{inspect(value)} into type #{inspect(type)}"
  end
end
