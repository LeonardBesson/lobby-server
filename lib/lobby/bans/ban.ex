defmodule Lobby.Bans.Ban do
  @moduledoc false
  use Lobby.Ecto.BaseSchema
  alias Lobby.Accounts.User
  import Ecto.Changeset

  schema "bans" do
    field(:reason, :string)
    field(:expire_at, :utc_datetime_usec)
    belongs_to(:user, User)

    timestamps()
  end

  def changeset(ban, attrs) do
    ban
    |> cast(attrs, [:reason, :expire_at])
    |> validate_required([:reason, :expire_at])
  end
end
