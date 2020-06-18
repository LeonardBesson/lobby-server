use Mix.Config

config :lobby, Lobby.Repo,
  database: "lobby_test",
  username: "postgres",
  password: "postgres",
  hostname: "postgres",
  pool: Ecto.Adapters.SQL.Sandbox
