use Mix.Config

config :lobby, Lobby.Repo,
  database: "lobby",
  username: "postgres",
  password: "postgres",
  hostname: "postgres",
  port: "5432",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

config :lobby,
  raise_on_missing_handler: true

config :lobby,
  ping_interval_millis: 2 * 60_000

config :lobby,
  reveal_ban_reason: true
