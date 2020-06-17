defmodule Lobby.Repo.Migrations.AddProfileTable do
  use Ecto.Migration

  def change do
    create table(:profiles, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :display_name, :text, null: false
      add :avatar_url, :text, null: true, default: nil
      add :user_id, references(:users, on_delete: :delete_all, type: :binary_id), null: false

      timestamps()
    end

    create unique_index(:profiles, [:user_id])
  end
end
