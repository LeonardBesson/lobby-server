defmodule Lobby.Bans do
  @moduledoc false
  import Ecto.Query
  alias Lobby.Accounts.User
  alias Lobby.Bans.Ban
  alias Lobby.Repo

  def validate(%User{id: user_id}) do
    case get_active_by_user_id(user_id) do
      [] -> :valid
      [%Ban{} = ban | _] -> {:banned, ban.reason, ban.expire_at}
    end
  end

  def get_active_by_user_id(user_id) do
    now = DateTime.utc_now()

    Repo.all(
      from(b in Ban,
        where: b.user_id == ^user_id and b.expire_at > ^now,
        order_by: [desc: :inserted_at]
      )
    )
  end

  def get_by_user_id(user_id) do
    Repo.all(from(b in Ban, where: b.user_id == ^user_id, order_by: [desc: :inserted_at]))
  end

  def create_ban(attrs \\ %{}) do
    %Ban{}
    |> Ban.changeset(attrs)
    |> Ecto.Changeset.cast(attrs, [:user_id])
    |> Ecto.Changeset.assoc_constraint(:user)
    |> Repo.insert()
  end

  def update_ban(%Ban{} = ban, attrs) do
    ban
    |> Ban.changeset(attrs)
    |> Ecto.Changeset.cast(attrs, [:user_id])
    |> Ecto.Changeset.assoc_constraint(:user)
    |> Repo.update()
  end

  def delete_ban(%Ban{} = ban) do
    Repo.delete(ban)
  end
end
