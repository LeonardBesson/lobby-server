defmodule Lobby.Protocol.Structs do
  @moduledoc """
  Structs used in messages
  """
  import Bincode.Structs
  alias Lobby.ClientRegistry
  alias Lobby.Accounts.User
  alias Lobby.Friends.FriendRequest
  alias Lobby.Profiles.Profile

  declare_struct(
    Lobby.UserProfile,
    [user_tag: :string, display_name: :string, avatar_url: {:option, :string}],
    absolute: true
  )

  def get_user_profile(nil), do: nil
  def get_user_profile(%User{profile: nil}), do: nil

  def get_user_profile(%User{
        user_tag: user_tag,
        profile: %Profile{display_name: display_name, avatar_url: avatar_url}
      }) do
    %Lobby.UserProfile{user_tag: user_tag, display_name: display_name, avatar_url: avatar_url}
  end

  declare_enum(Lobby.FriendRequestAction, [Accept: [], Decline: []], absolute: true)

  declare_struct(
    Lobby.FriendRequest,
    # user_profile is inviter or invitee depending on context
    [id: :string, state: :string, user_profile: Lobby.UserProfile],
    absolute: true
  )

  def get_friend_request(%FriendRequest{
        id: id,
        state: state,
        inviter: %User{profile: %Profile{}} = user,
        invitee: %Ecto.Association.NotLoaded{}
      }) do
    %Lobby.FriendRequest{id: id, state: state, user_profile: get_user_profile(user)}
  end

  def get_friend_request(%FriendRequest{
        id: id,
        state: state,
        inviter: %Ecto.Association.NotLoaded{},
        invitee: %User{profile: %Profile{}} = user
      }) do
    %Lobby.FriendRequest{id: id, state: state, user_profile: get_user_profile(user)}
  end

  def get_friend_request(_), do: nil

  declare_struct(
    Lobby.Friend,
    [user_profile: Lobby.UserProfile, is_online: :bool],
    absolute: true
  )

  def get_friend(user_id, %FriendRequest{
        inviter: %User{id: inviter_id, profile: %Profile{}} = inviter,
        invitee: %User{id: invitee_id, profile: %Profile{}} = invitee
      }) do
    {user_profile, is_online} =
      case user_id do
        ^inviter_id -> {get_user_profile(invitee), ClientRegistry.is_online(invitee_id)}
        ^invitee_id -> {get_user_profile(inviter), ClientRegistry.is_online(inviter_id)}
      end

    %Lobby.Friend{
      user_profile: user_profile,
      is_online: is_online
    }
  end

  def get_friend(_), do: nil

  declare_enum(Lobby.LobbyInviteAction, [Accept: [], Decline: []], absolute: true)
  declare_enum(Lobby.LobbyRole, [Leader: [], Member: []], absolute: true)

  declare_struct(
    Lobby.LobbyMember,
    [user_profile: Lobby.UserProfile, role: Lobby.LobbyRole, is_online: :bool],
    absolute: true
  )
end
