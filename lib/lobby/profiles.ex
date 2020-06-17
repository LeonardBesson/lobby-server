defmodule Lobby.Profiles do
  @moduledoc false
  import Ecto.Query
  alias Lobby.Profiles.Profile
  alias Lobby.Repo

  def get_by_user_id(user_id) do
    Repo.get_by(Profile, user_id: user_id)
  end

  def create_profile(attrs \\ %{}) do
    %Profile{}
    |> Profile.changeset(attrs)
    |> Ecto.Changeset.cast(attrs, [:user_id])
    |> Ecto.Changeset.unique_constraint(:user_id)
    |> Ecto.Changeset.assoc_constraint(:user)
    |> Repo.insert()
  end

  def update_profile(%Profile{} = profile, attrs) do
    profile
    |> Profile.changeset(attrs)
    |> Ecto.Changeset.cast(attrs, [:user_id])
    |> Ecto.Changeset.unique_constraint(:user_id)
    |> Ecto.Changeset.assoc_constraint(:user)
    |> Repo.update()
  end

  def delete_profile(%Profile{} = profile) do
    Repo.delete(profile)
  end
end
