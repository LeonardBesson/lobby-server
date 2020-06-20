defmodule Lobby.Friends.FriendRequest do
  @moduledoc false
  use Lobby.Ecto.BaseSchema
  alias Lobby.Accounts.User
  import Ecto.Changeset

  schema "friend_requests" do
    belongs_to(:inviter, User)
    belongs_to(:invitee, User)
    field(:state, :string)

    timestamps()
  end

  def changeset(friend_request, attrs) do
    friend_request
    |> cast(attrs, [:state])
    |> validate_required([:state])
    |> unique_constraint([:inviter_id, :invitee_id])
  end
end
