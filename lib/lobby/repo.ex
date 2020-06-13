defmodule Lobby.Repo do
  use Ecto.Repo,
    otp_app: :lobby,
    adapter: Ecto.Adapters.Postgres
end
