defmodule Lobby.MessageHandlers.Chat do
  @moduledoc """
  Handles messages related to chat
  """
  use Lobby.MessageHandler, [:send_private_message]
  import Lobby.Protocol.Structs
  alias Lobby.ClientRegistry
  alias Lobby.ProfileCache
  alias Lobby.Messages.NewPrivateMessage
  alias Lobby.Messages.SystemNotification

  def handle(:send_private_message, msg, %ClientState{user: user} = state) do
    # TODO: restrict private messages to friends in config
    # TODO: allow defining content filters in config
    ClientRegistry.if_online_by_tag(
      msg.user_tag,
      fn client_pid ->
        send_message(client_pid, %NewPrivateMessage{
          profile: get_user_profile(user),
          content: msg.content,
          is_self: false
        })

        case ProfileCache.get_or_create_by_tag(msg.user_tag) do
          {:ok, other_user_profile} ->
            send_message(self(), %NewPrivateMessage{
              profile: other_user_profile,
              content: msg.content,
              is_self: true
            })

          {:error, reason} ->
            Logger.error("ProfileCache error: #{inspect(reason)}")
            disconnect(state, "Internal error")
        end
      end,
      fn ->
        send_message(self(), %SystemNotification{content: "User #{msg.user_tag} is offline"})
      end
    )

    state
  end
end
