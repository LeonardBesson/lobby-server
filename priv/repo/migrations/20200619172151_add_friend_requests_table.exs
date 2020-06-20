defmodule Lobby.Repo.Migrations.AddFriendRequestsTable do
  use Ecto.Migration

  def change do
    create table(:friend_requests, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :inviter_id, references(:users, on_delete: :delete_all, type: :binary_id), null: false
      add :invitee_id, references(:users, on_delete: :delete_all, type: :binary_id), null: false
      add :state, :string, null: false

      timestamps()
    end

    create index(:friend_requests, [:inviter_id])
    create index(:friend_requests, [:invitee_id])
    create unique_index(:friend_requests, [:inviter_id, :invitee_id])
  end
end
