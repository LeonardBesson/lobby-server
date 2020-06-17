defmodule Lobby.Repo.Migrations.AddUserTable do
  use Ecto.Migration

  def change do
    create table(:users, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :email, :text, null: false
      add :user_tag, :text, null: false
      add :is_active, :boolean, default: true, null: false
      add :password_hash, :text, null: false

      timestamps()
    end

    create unique_index(:users, [:email])
    create unique_index(:users, [:user_tag])
  end
end
