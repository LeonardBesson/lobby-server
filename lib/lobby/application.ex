defmodule Lobby.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      :ranch.child_spec(
        :network,
        :ranch_tcp,
        [{:port, 9000}],
        Lobby.ClientConn,
        []
      )
    ]

    opts = [strategy: :one_for_one, name: Lobby.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
