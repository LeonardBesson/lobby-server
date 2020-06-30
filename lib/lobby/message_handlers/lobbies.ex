defmodule Lobby.MessageHandlers.Lobbies do
  @moduledoc """
  Handles messages related to lobbies
  """
  use Lobby.MessageHandler, [:invite_user]
  alias Lobby.ClientState
  alias Lobby.ClientRegistry
  alias Lobby.ProfileCache
  alias Lobby.LobbyRegistry
  alias Lobby.LobbyInviteCache
  alias Lobby.Lobbies
  alias Lobby.Lobbies.LobbyServer
  alias Lobby.Messages.LobbyInvite
  alias Lobby.Messages.SystemNotification
  require Logger

  def handle(:invite_user, msg, %ClientState{user: user} = state) do
    cond do
      not ClientRegistry.is_online_by_tag(msg.user_tag) ->
        send_message(self(), %SystemNotification{
          content: "User #{msg.user_tag} is already in a lobby"
        })

      ClientRegistry.get_lobby_id_by_tag!(msg.user_tag) != nil ->
        send_message(self(), %SystemNotification{
          content: "User #{msg.user_tag} is already in a lobby"
        })

      true ->
        case ClientRegistry.get_lobby_id_by_tag!(user.user_tag) do
          nil ->
            case Lobbies.Supervisor.start_lobby(user.user_tag) do
              {:ok, {lobby_id, lobby_pid}} ->
                ClientRegistry.if_online_by_tag(
                  msg.user_tag,
                  fn client_pid ->
                    {:ok, invite_id} =
                      LobbyInviteCache.get_or_create_invite(lobby_id, msg.user_tag)

                    inviter_profile = ProfileCache.get_or_create!(user.id)

                    send_message(client_pid, %LobbyInvite{id: invite_id, inviter: inviter_profile})

                    send_message(self(), %SystemNotification{content: "Invite sent"})
                  end,
                  fn ->
                    send_message(self(), %SystemNotification{
                      content: "User #{msg.user_tag} is offline"
                    })
                  end
                )

              {:error, reason} ->
                Logger.error("Could not start new LobbyServer: #{inspect(reason)}")
                send_message(self(), %SystemNotification{content: "Error inviting user to lobby"})
            end

          lobby_id ->
            ClientRegistry.if_online_by_tag(msg.user_tag, fn client_pid ->
              {:ok, invite_id} = LobbyInviteCache.get_or_create_invite(lobby_id, msg.user_tag)
              inviter_profile = ProfileCache.get_or_create!(user.id)
              send_message(client_pid, %LobbyInvite{id: invite_id, inviter: inviter_profile})
            end)
        end
    end

    state
  end
end
