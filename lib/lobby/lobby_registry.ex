defmodule Lobby.LobbyRegistry do
  @moduledoc false
  import Lobby.Utils.Mnesia
  require Logger

  @table_name LobbyRegistryEntry

  def create_table do
    {:atomic, :ok} = :mnesia.create_table(@table_name, attributes: [:id, :pid, :saved_state])
    {:atomic, :ok} = :mnesia.add_table_index(@table_name, :pid)

    @table_name
  end

  def register_lobby(id, pid) when is_binary(id) and is_pid(pid) do
    mnesia_transaction(fn ->
      saved_state =
        case :mnesia.read({@table_name, id}) do
          [{@table_name, ^id, pid, saved_state}] ->
            if pid != nil do
              Logger.error(
                "LobbyServer #{id} pid is still set. The GenServer wasn't properly terminated!"
              )
            else
              Logger.debug("LobbyServer #{id} updating its pid and starting back up")
            end

            saved_state

          _ ->
            Logger.debug("Registering new LobbyServer #{id}")
            nil
        end

      case :mnesia.write({@table_name, id, pid, saved_state}) do
        :ok -> saved_state
        error -> error
      end
    end)
  end

  def lobby_terminating(id, state) when is_binary(id) do
    result =
      mnesia_transaction(fn ->
        :mnesia.write({@table_name, id, nil, state})
      end)

    case result do
      {:ok, :ok} -> :ok
      _ -> result
    end
  end

  def whereis(id) when is_binary(id) do
    result = mnesia_transaction(fn -> :mnesia.read({@table_name, id}) end)

    case result do
      {:ok, [{@table_name, ^id, pid, _}]} -> {:ok, pid}
      _ -> result
    end
  end

  def whereis!(id) do
    case whereis(id) do
      {:ok, res} -> res
      {:error, reason} -> throw(reason)
    end
  end

  def if_online(lobby_id, callback, offline_callback \\ nil)
      when is_function(callback, 1) and is_function(offline_callback, 0) do
    case whereis(lobby_id) do
      {:ok, lobby_pid} when is_pid(lobby_pid) ->
        {:ok, callback.(lobby_pid)}

      _ ->
        if offline_callback == nil do
          {:error, :offline}
        else
          {:error, offline_callback.()}
        end
    end
  end
end
