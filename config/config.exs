use Mix.Config

config :lobby,
  env: Mix.env()

config :lobby,
  protocol_version: 1,
  app_version: 1

config :lobby,
  buffer_processors: []
