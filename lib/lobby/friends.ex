defmodule Lobby.Friends do
  @moduledoc false
  import Ecto.Query
  import Lobby.Protocol.Structs
  alias Lobby.Friends.FriendRequest
  alias Lobby.Repo

  def fetch_pending_requests(user_id) when is_binary(user_id) do
    pending_inviter_requests = get_pending_as_inviter(user_id) |> Enum.map(&get_friend_request/1)
    pending_invitee_requests = get_pending_as_invitee(user_id) |> Enum.map(&get_friend_request/1)

    {pending_inviter_requests, pending_invitee_requests}
  end

  def get_pending_as_inviter(user_id) when is_binary(user_id) do
    Repo.all(
      from(fr in FriendRequest,
        join: u in assoc(fr, :invitee),
        join: p in assoc(u, :profile),
        where: fr.inviter_id == ^user_id and fr.state == "pending",
        preload: [invitee: {u, profile: p}]
      )
    )
  end

  def get_pending_as_invitee(user_id) when is_binary(user_id) do
    Repo.all(
      from(fr in FriendRequest,
        join: u in assoc(fr, :inviter),
        join: p in assoc(u, :profile),
        where: fr.invitee_id == ^user_id and fr.state == "pending",
        preload: [inviter: {u, profile: p}]
      )
    )
  end

  def create_friend_request(attrs \\ %{}) do
    %FriendRequest{}
    |> FriendRequest.changeset(attrs)
    |> Ecto.Changeset.cast(attrs, [:inviter_id, :invitee_id])
    |> Ecto.Changeset.assoc_constraint(:inviter)
    |> Ecto.Changeset.assoc_constraint(:invitee)
    |> Repo.insert()
  end

  def update_friend_request(%FriendRequest{} = friend_request, attrs) do
    friend_request
    |> FriendRequest.changeset(attrs)
    |> Ecto.Changeset.cast(attrs, [:inviter_id, :invitee_id])
    |> Ecto.Changeset.assoc_constraint(:inviter)
    |> Ecto.Changeset.assoc_constraint(:invitee)
    |> Repo.update()
  end

  def delete_friend_request(%FriendRequest{} = friend_request) do
    Repo.delete(friend_request)
  end
end
