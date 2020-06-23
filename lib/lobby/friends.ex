defmodule Lobby.Friends do
  @moduledoc false
  import Ecto.Query
  import Lobby.Protocol.Structs
  alias Lobby.FriendRequestAction
  alias Lobby.Friends.FriendRequest
  alias Lobby.Repo

  def are_friends(user_tag, other_user_tag) do
    Repo.exists?(
      from(fr in FriendRequest,
        join: inviter in assoc(fr, :inviter),
        join: invitee in assoc(fr, :invitee),
        where:
          fr.state == "accepted" and
            ((inviter.user_tag == ^user_tag and invitee.user_tag == ^other_user_tag) or
               (inviter.user_tag == ^other_user_tag and invitee.user_tag == ^user_tag))
      )
    )
  end

  def remove_friend(user_id, other_user_id) do
    {count, _} =
      from(fr in FriendRequest,
        where:
          (fr.inviter_id == ^user_id and fr.invitee_id == ^other_user_id) or
            (fr.inviter_id == ^other_user_id and fr.invitee_id == ^user_id)
      )
      |> Repo.delete_all()

    case count do
      1 -> :ok
      0 -> :error
    end
  end

  def fetch_friend_list(user_id) do
    Repo.all(
      from(fr in FriendRequest,
        join: inviter in assoc(fr, :inviter),
        join: inviter_p in assoc(inviter, :profile),
        join: invitee in assoc(fr, :invitee),
        join: invitee_p in assoc(invitee, :profile),
        where:
          fr.state == "accepted" and (fr.inviter_id == ^user_id or fr.invitee_id == ^user_id),
        preload: [inviter: {inviter, profile: inviter_p}, invitee: {invitee, profile: invitee_p}]
      )
    )
    |> Enum.map(fn fr -> get_friend(user_id, fr) end)
  end

  def friend_request_action(id, invitee_id, action) do
    state =
      case action do
        %FriendRequestAction.Accept{} -> "accepted"
        %FriendRequestAction.Decline{} -> "declined"
      end

    case get_by(id: id, invitee_id: invitee_id) do
      %FriendRequest{} = request ->
        update_friend_request(request, %{state: state})

      nil ->
        {:error, :not_found}
    end
  end

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

  def get_by(fields, opts \\ []) when is_list(fields) and is_list(opts) do
    Repo.get_by(FriendRequest, fields, opts)
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
