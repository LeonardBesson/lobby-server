defmodule Lobby.Profiles.Profile do
  @moduledoc false
  use Lobby.Ecto.BaseSchema
  alias Lobby.Accounts.User
  import Ecto.Changeset

  schema "profiles" do
    field(:display_name, :string)
    field(:avatar_url, :string, default: nil)
    belongs_to(:user, User)

    timestamps()
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:display_name, :avatar_url])
    |> validate_required([:display_name])
  end
end
