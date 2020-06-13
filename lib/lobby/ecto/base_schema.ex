defmodule Lobby.Ecto.BaseSchema do
  @moduledoc """
  The base schema model, it enables:
    - UUID type for our primary and foreign keys
    - Milliseconds precision on our timestamp types
  """
  defmacro __using__(_opts) do
    quote do
      use Ecto.Schema

      @primary_key {:id, :binary_id, autogenerate: true}
      @foreign_key_type :binary_id

      @timestamps_opts [type: :utc_datetime_usec]
    end
  end
end
