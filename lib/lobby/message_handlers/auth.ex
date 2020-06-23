defmodule Lobby.MessageHandlers.Auth do
  @moduledoc """
  Handles authentication messages
  """
  use Lobby.MessageHandler, [:authentication_request]
  import Lobby.Protocol.Structs
  require Lobby
  alias Lobby.Accounts
  alias Lobby.Accounts.User
  alias Lobby.Bans
  alias Lobby.Transport.Connection
  alias Lobby.ClientRegistry
  alias Lobby.ProfileCache
  alias Lobby.Messages.AuthenticationResponse
  alias Lobby.Utils.Crypto

  @reveal_ban_reason Lobby.compile_env!(:reveal_ban_reason)
  @ping_interval_millis Lobby.compile_env!(:ping_interval_millis)

  def handle(
        :authentication_request,
        _msg,
        %ClientState{conn: %Connection{state: conn_state}} = state
      )
      when conn_state != :authenticating do
    disconnect(state, "Packets out of order")
  end

  def handle(:authentication_request, msg, %ClientState{conn: conn} = state) do
    case Accounts.authenticate(msg.email, msg.password) do
      {:ok, %User{} = user} ->
        case Bans.validate(user) do
          {:banned, reason, expire_at} ->
            ban_message =
              if @reveal_ban_reason do
                "Banned until #{expire_at}.\n#{reason}"
              else
                "Banned until #{expire_at}"
              end

            disconnect(state, ban_message)

          :valid ->
            conn = %{conn | state: :running}

            case ClientRegistry.client_authenticated(user.id, user.user_tag, self()) do
              :ok ->
                Logger.info("Client registered: (#{user.id} <-> #{inspect(self())}})")

                case ProfileCache.get_or_create(user.id) do
                  {:ok, _} ->
                    if state.auth_timeout_timer != nil do
                      Process.cancel_timer(state.auth_timeout_timer)
                    end

                    ping_timer = Process.send_after(self(), :ping_client, @ping_interval_millis)

                    state = %{
                      state
                      | conn: conn,
                        user: user,
                        auth_timeout_timer: nil,
                        ping_timer: ping_timer
                    }

                    send_message(self(), %AuthenticationResponse{
                      session_token: Crypto.gen_session_token(),
                      user_profile: get_user_profile(user)
                    })

                    state

                  {:error, reason} ->
                    Logger.error("ProfileCache error: #{inspect(reason)}")
                    disconnect(state, "Internal error")
                end

              {:error, reason} ->
                Logger.error("Could not register client: #{inspect(reason)}")
                disconnect(state, "Internal error")
            end
        end

      {:error, _} ->
        send_message(self(), %AuthenticationResponse{error_code: "invalid_credentials"})
        state
    end
  end
end
