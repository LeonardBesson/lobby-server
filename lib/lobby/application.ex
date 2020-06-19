defmodule Lobby.Application do
  @moduledoc false

  use Application
  require Logger
  alias Lobby.ClientRegistry

  def start(_type, _args) do
    children = [
      Lobby.Repo,
      :ranch.child_spec(
        :network,
        :ranch_tcp,
        [{:port, 9000}],
        Lobby.ClientConn,
        []
      )
    ]

    init_mnesia()

    opts = [strategy: :one_for_one, name: Lobby.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def init_mnesia do
    :mnesia.start()
    ClientRegistry.create_table()
    :ok = :mnesia.wait_for_tables([ClientRegistry.table_name()], :timer.seconds(15))
    Logger.debug("Mnesia initialized")
  end
end
