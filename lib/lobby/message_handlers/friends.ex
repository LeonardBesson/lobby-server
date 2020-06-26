defmodule Lobby.MessageHandlers.Friends do
  @moduledoc """
  Handles all friends related messages
  """
  use Lobby.MessageHandler, [
    :add_friend_request,
    :fetch_pending_friend_requests,
    :friend_request_action,
    :fetch_friend_list,
    :remove_friend
  ]

  alias Lobby.Repo
  alias Lobby.Accounts
  alias Lobby.Accounts.User
  alias Lobby.Friends
  alias Lobby.FriendsCache
  alias Lobby.ClientRegistry
  alias Lobby.Messages.AddFriendRequestResponse
  alias Lobby.Messages.FetchPendingFriendRequestsResponse
  alias Lobby.Messages.FriendRequestActionResponse
  alias Lobby.Messages.FetchFriendListResponse
  alias Lobby.Messages.RemoveFriendResponse

  def handle(:add_friend_request, msg, %ClientState{user: user} = state) do
    case Accounts.get_by_user_tag(msg.user_tag) do
      %User{} = invitee ->
        friend_request_attrs = %{
          inviter_id: user.id,
          invitee_id: invitee.id,
          state: "pending"
        }

        case Friends.create_friend_request(friend_request_attrs) do
          {:ok, _} ->
            update_friend_requests(invitee.id)
            state = update_friend_requests(state)
            send_message(state, %AddFriendRequestResponse{user_tag: msg.user_tag})

          {:error,
           %Ecto.Changeset{
             errors: [
               inviter_id:
                 {_,
                  [
                    constraint: :unique,
                    constraint_name: "friend_requests_inviter_id_invitee_id_index"
                  ]}
             ]
           }} ->
            # Already exists, done
            send_message(state, %AddFriendRequestResponse{user_tag: msg.user_tag})

          {:error, _} ->
            send_message(state, %AddFriendRequestResponse{
              user_tag: msg.user_tag,
              error_code: "not_found"
            })
        end

      nil ->
        Logger.debug("AddFriendRequest for unknown user tag: #{msg.user_tag}")

        send_message(state, %AddFriendRequestResponse{
          user_tag: msg.user_tag,
          error_code: "not_found"
        })
    end
  end

  def handle(:fetch_pending_friend_requests, _msg, %ClientState{} = state) do
    update_friend_requests(state)
  end

  def handle(:friend_request_action, msg, %ClientState{user: user} = state) do
    case Friends.friend_request_action(msg.request_id, user.id, msg.action) do
      {:ok, request} ->
        state = update_friend_requests(state)
        state = update_friend_list(state)
        update_friend_requests(request.inviter_id)
        update_friend_list(request.inviter_id)
        request = request |> Repo.preload(:inviter)
        FriendsCache.update(user.user_tag, request.inviter.user_tag, true)
        send_message(state, %FriendRequestActionResponse{request_id: msg.request_id})

      {:error, _} ->
        send_message(state, %FriendRequestActionResponse{
          request_id: msg.request_id,
          error_code: "not_found"
        })
    end
  end

  def handle(:fetch_friend_list, _msg, %ClientState{} = state) do
    update_friend_list(state)
  end

  def handle(:remove_friend, msg, %ClientState{user: user} = state) do
    other_user = Accounts.get_by_user_tag(msg.user_tag)

    if other_user == nil do
      %RemoveFriendResponse{error_code: "not_found"}
    else
      case Friends.remove_friend(user.id, other_user.id) do
        :ok ->
          state = update_friend_list(state)
          update_friend_list(other_user.id)
          FriendsCache.update(user.user_tag, other_user.user_tag, false)
          send_message(state, %RemoveFriendResponse{})

        :error ->
          send_message(state, %RemoveFriendResponse{error_code: "not_found"})
      end
    end
  end

  defp update_friend_requests(%ClientState{user: user} = state) do
    {pending_as_inviter, pending_as_invitee} = Friends.fetch_pending_requests(user.id)

    send_message(state, %FetchPendingFriendRequestsResponse{
      pending_as_inviter: pending_as_inviter,
      pending_as_invitee: pending_as_invitee
    })
  end

  defp update_friend_requests(user_id) do
    ClientRegistry.if_online(user_id, fn client_pid ->
      {pending_as_inviter, pending_as_invitee} = Friends.fetch_pending_requests(user_id)

      send_message(client_pid, %FetchPendingFriendRequestsResponse{
        pending_as_inviter: pending_as_inviter,
        pending_as_invitee: pending_as_invitee
      })
    end)
  end

  defp update_friend_list(%ClientState{user: user} = state) do
    friend_list = Friends.fetch_friend_list(user.id)

    send_message(state, %FetchFriendListResponse{friend_list: friend_list})
  end

  defp update_friend_list(user_id) do
    ClientRegistry.if_online(user_id, fn client_pid ->
      friend_list = Friends.fetch_friend_list(user_id)

      send_message(client_pid, %FetchFriendListResponse{friend_list: friend_list})
    end)
  end
end
