use Mix.Config

config :lobby,
  env: Mix.env()

config :lobby,
  protocol_version: 1,
  app_version: 1

config :lobby,
  ecto_repos: [Lobby.Repo],
  generators: [binary_id: true]

config :lobby,
  message_handlers: [
    Lobby.MessageHandlers.Auth,
    Lobby.MessageHandlers.Friends,
    Lobby.MessageHandlers.Chat,
    Lobby.MessageHandlers.Lobbies
  ],
  raise_on_missing_handler: false

config :lobby,
  display_tag_validation: ~r/^[[:alnum:]]{1,16}#[[:digit:]]{4,6}$/

config :lobby,
  reveal_ban_reason: false

config :lobby,
  allow_open_private_messages: true

config :lobby,
  lobby_default_max_size: 8,
  lobby_invite_expiry_millis: 2 * 60_000,
  lobby_invite_notify_inviter_on_decline: true

config :lobby,
  auth_timeout_millis: 2 * 60_000,
  disconnect_delay_millis: 100,
  ping_interval_millis: 10_000,
  ping_timeout_millis: 5 * 60_000,
  round_trip_threshold_warning_millis: 500

config :lobby,
  session_token_bytes: 16

import_config "#{Mix.env()}.exs"
