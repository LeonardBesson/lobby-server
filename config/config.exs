use Mix.Config

config :lobby,
  env: Mix.env()

config :lobby,
  protocol_version: 1,
  app_version: 1

config :lobby,
  ecto_repos: [Lobby.Repo],
  generators: [binary_id: true]

config :lobby, Lobby.Repo,
  database: "lobby",
  username: "postgres",
  password: "postgres",
  hostname: "postgres",
  port: "5432"

config :lobby,
  session_token_bytes: 16
