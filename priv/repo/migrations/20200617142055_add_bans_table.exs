defmodule Lobby.Repo.Migrations.AddBansTable do
  use Ecto.Migration

  def change do
    create table(:bans, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :reason, :string, null: false
      add :expire_at, :utc_datetime_usec, null: false
      add :user_id, references(:users, on_delete: :delete_all, type: :binary_id), null: false

      timestamps()
    end

    create index(:bans, [:user_id])
  end
end
