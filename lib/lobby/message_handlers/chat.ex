defmodule Lobby.MessageHandlers.Chat do
  @moduledoc """
  Handles messages related to chat
  """
  use Lobby.MessageHandler, [:send_private_message]
  import Lobby.Protocol.Structs
  alias Lobby.ClientRegistry
  alias Lobby.ProfileCache
  alias Lobby.FriendsCache
  alias Lobby.Messages.NewPrivateMessage
  alias Lobby.Messages.SystemNotification
  require Lobby

  @allow_open_private_messages Lobby.compile_env!(:allow_open_private_messages)

  def handle(:send_private_message, msg, %ClientState{user: user} = state) do
    cond do
      msg.user_tag == user.user_tag ->
        state

      check_private_message_allowed(user.user_tag, msg.user_tag) ->
        # TODO: allow defining content filters in config
        {_, state} =
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
                  send_message(state, %NewPrivateMessage{
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
              send_message(state, %SystemNotification{content: "User #{msg.user_tag} is offline"})
            end
          )

        state

      true ->
        send_message(state, %SystemNotification{
          content: "You are not friends with #{msg.user_tag}"
        })
    end
  end

  defp check_private_message_allowed(user_tag, other_user_tag) do
    if @allow_open_private_messages do
      true
    else
      FriendsCache.are_friends(user_tag, other_user_tag)
    end
  end
end
