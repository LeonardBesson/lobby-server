defmodule Lobby.MessageHandlers.Lobbies do
  @moduledoc """
  Handles messages related to lobbies
  """
  use Lobby.MessageHandler, [:invite_user, :lobby_invite_action]
  alias Lobby.ClientState
  alias Lobby.ClientRegistry
  alias Lobby.ProfileCache
  alias Lobby.LobbyRegistry
  alias Lobby.LobbyInviteCache
  alias Lobby.Lobbies
  alias Lobby.Lobbies.LobbyServer
  alias Lobby.LobbyInviteAction
  alias Lobby.Messages.LobbyInvite
  alias Lobby.Messages.SystemNotification
  require Logger
  require Lobby

  @lobby_invite_expiry_millis Lobby.compile_env!(:lobby_invite_expiry_millis)
  @lobby_invite_notify_inviter_on_decline Lobby.compile_env!(
                                            :lobby_invite_notify_inviter_on_decline
                                          )

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
              {:ok, {lobby_id, _}} ->
                ClientRegistry.if_online_by_tag(
                  msg.user_tag,
                  fn client_pid ->
                    {:ok, invite_id} =
                      LobbyInviteCache.get_or_create_invite(lobby_id, user.user_tag, msg.user_tag)

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
            LobbyRegistry.if_online(lobby_id, fn lobby_pid ->
              if LobbyServer.can_user_invite?(lobby_pid, user.user_tag) do
                ClientRegistry.if_online_by_tag(msg.user_tag, fn client_pid ->
                  {:ok, invite_id} =
                    LobbyInviteCache.get_or_create_invite(lobby_id, user.user_tag, msg.user_tag)

                  inviter_profile = ProfileCache.get_or_create!(user.id)
                  send_message(client_pid, %LobbyInvite{id: invite_id, inviter: inviter_profile})
                end)
              else
                send_message(self(), %SystemNotification{content: "Only lobby leaders can invite"})
              end
            end)
        end
    end

    state
  end

  def handle(:lobby_invite_action, msg, %ClientState{user: user} = state) do
    case validate_invite(msg.invite_id, user.user_tag) do
      {:ok, {lobby_id, inviter}} ->
        case msg.action do
          %LobbyInviteAction.Accept{} ->
            LobbyRegistry.if_online(
              lobby_id,
              fn lobby_pid ->
                case LobbyServer.add_member(lobby_pid, user.user_tag) do
                  :ok ->
                    LobbyInviteCache.delete_invite(msg.invite_id)

                    Logger.debug(
                      "User #{user.user_tag} successfully accepted invitation to lobby #{lobby_id}"
                    )

                  {:error, reason} ->
                    Logger.error("Error accepted lobby invitation: #{inspect(reason)}")

                    send_message(self(), %SystemNotification{
                      content: "Error accepting lobby invitation: #{inspect(reason)}"
                    })
                end
              end,
              fn ->
                LobbyInviteCache.delete_invite(msg.invite_id)
                send_message(self(), %SystemNotification{content: "Lobby closed"})
              end
            )

          %LobbyInviteAction.Decline{} ->
            LobbyInviteCache.delete_invite(msg.invite_id)

            if @lobby_invite_notify_inviter_on_decline do
              ClientRegistry.if_online_by_tag(inviter, fn client_pid ->
                send_message(client_pid, %SystemNotification{
                  content: "User #{user.user_tag} declined your invitation"
                })
              end)
            end
        end

      {:error, :not_found} ->
        send_message(self(), %SystemNotification{content: "Invitation not found"})

      {:error, :invite_expired} ->
        send_message(self(), %SystemNotification{content: "Invitation expired"})
    end

    state
  end

  defp validate_invite(id, invitee) do
    case LobbyInviteCache.get_invite!(id) do
      {^id, lobby_id, inviter, ^invitee, inserted_at} ->
        expire_at = DateTime.add(inserted_at, @lobby_invite_expiry_millis, :millisecond)

        if DateTime.compare(DateTime.utc_now(), expire_at) == :gt do
          {:error, :invite_expired}
        else
          {:ok, {lobby_id, inviter}}
        end

      _ ->
        {:error, :not_found}
    end
  end
end
