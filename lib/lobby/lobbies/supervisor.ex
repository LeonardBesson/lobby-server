defmodule Lobby.Lobbies.Supervisor do
  @moduledoc """
  Supervisor responsible for creating the lobby processes
  """
  use DynamicSupervisor
  alias Lobby.Lobbies.LobbyServer
  require Logger

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def start_lobby(leader, opts \\ []) when is_binary(leader) do
    id = Ecto.UUID.generate()
    spec = {LobbyServer, [id, leader, opts]}

    case DynamicSupervisor.start_child(__MODULE__, spec) do
      {:ok, pid} -> {:ok, {id, pid}}
      {:error, reason} -> {:error, reason}
      reason -> {:error, reason}
    end
  end

  @impl true
  def init(_init_arg) do
    Logger.debug("Lobby Supervisor started")
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
