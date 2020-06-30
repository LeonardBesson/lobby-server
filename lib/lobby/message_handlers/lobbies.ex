defmodule Lobby.MessageHandlers.Lobbies do
  @moduledoc """
  Handles messages related to lobbies
  """
  use Lobby.MessageHandler, [:invite_user]
  alias Lobby.ClientState
  alias Lobby.ClientRegistry
  alias Lobby.ProfileCache
  alias Lobby.LobbyRegistry
  alias Lobby.Lobbies
  alias Lobby.Lobbies.LobbyServer
  alias Lobby.Messages.LobbyInvite
  alias Lobby.Messages.SystemNotification
  require Logger

  def handle(:invite_user, msg, %ClientState{user: user} = state) do
    if ClientRegistry.get_lobby_id_by_tag!(msg.user_tag) != nil do
      send_message(self(), %SystemNotification{
        content: "User #{msg.user_tag} is already in a lobby"
      })
    else
      case get_lobby_pid!(user.user_tag) do
        nil ->
          case Lobbies.Supervisor.start_lobby(user.user_tag) do
            {:ok, {_, lobby_pid}} ->
              ClientRegistry.if_online_by_tag(
                msg.user_tag,
                fn client_pid ->
                  inviter_profile = ProfileCache.get_or_create!(user.id)
                  send_message(client_pid, %LobbyInvite{inviter: inviter_profile})
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

        lobby_pid ->
          ClientRegistry.if_online_by_tag(msg.user_tag, fn client_pid ->
            inviter_profile = ProfileCache.get_or_create!(user.id)
            send_message(client_pid, %LobbyInvite{inviter: inviter_profile})
          end)
      end
    end

    state
  end

  defp get_lobby_pid!(user_tag) when is_binary(user_tag) do
    case ClientRegistry.get_lobby_id_by_tag(user_tag) do
      {:ok, nil} ->
        nil

      {:ok, lobby_id} ->
        case LobbyRegistry.whereis(lobby_id) do
          {:ok, lobby_pid} -> lobby_pid
          _ -> nil
        end

      _ ->
        nil
    end
  end
end
