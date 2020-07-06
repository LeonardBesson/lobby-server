defmodule Lobby.ClientRegistry do
  @moduledoc false

  import Lobby.Utils.Mnesia
  require Logger

  @table_name ClientRegistryEntry

  def create_table do
    {:atomic, :ok} =
      :mnesia.create_table(@table_name,
        attributes: [:user_id, :user_tag, :conn_pid, :lobby_id, :online_since]
      )

    {:atomic, :ok} = :mnesia.add_table_index(@table_name, :user_tag)

    @table_name
  end

  def client_authenticated(user_id, user_tag, conn_pid)
      when is_binary(user_id) and is_pid(conn_pid) do
    # TODO: update if exists (reconnect)
    result =
      mnesia_transaction(fn ->
        :mnesia.write({@table_name, user_id, user_tag, conn_pid, nil, DateTime.utc_now()})
      end)

    case result do
      {:ok, :ok} -> :ok
      _ -> result
    end
  end

  def client_disconnected(nil), do: :ok

  def client_disconnected(user_id) when is_binary(user_id) do
    result =
      mnesia_transaction(fn ->
        case :mnesia.read({@table_name, user_id}) do
          [] ->
            :ok

          [{@table_name, ^user_id, user_tag, _, lobby_id, _}] ->
            # TODO: mark for delayed deletion (value in config)
            # so clients can keep their state after deco/reco
            :mnesia.write({@table_name, user_id, user_tag, nil, lobby_id, nil})
        end
      end)

    case result do
      {:ok, :ok} -> :ok
      _ -> result
    end
  end

  def whereis(user_id) when is_binary(user_id) do
    result = mnesia_transaction(fn -> :mnesia.read({@table_name, user_id}) end)

    case result do
      {:ok, []} -> {:ok, nil}
      {:ok, [{@table_name, ^user_id, _, nil, _, _}]} -> {:ok, nil}
      {:ok, [{@table_name, ^user_id, _, conn_pid, _, _}]} -> {:ok, conn_pid}
      _ -> result
    end
  end

  def whereis!(user_id) do
    case whereis(user_id) do
      {:ok, res} -> res
      {:error, reason} -> throw(reason)
    end
  end

  def whereis_by_tag(user_tag) when is_binary(user_tag) do
    result = mnesia_transaction(fn -> :mnesia.index_read(@table_name, user_tag, :user_tag) end)

    case result do
      {:ok, []} -> {:ok, nil}
      {:ok, [{@table_name, _, ^user_tag, nil, _, _}]} -> {:ok, nil}
      {:ok, [{@table_name, _, ^user_tag, conn_pid, _, _}]} -> {:ok, conn_pid}
      _ -> result
    end
  end

  def whereis_by_tag!(user_tag) do
    case whereis_by_tag(user_tag) do
      {:ok, res} -> res
      {:error, reason} -> throw(reason)
    end
  end

  def set_lobby_id(user_tag, lobby_id) do
    result =
      mnesia_transaction(fn ->
        case :mnesia.index_read(@table_name, user_tag, :user_tag) do
          [] ->
            Logger.error("Trying to set lobby id for offline player")
            :user_offline

          [{@table_name, user_id, ^user_tag, conn_pid, _, online_since}] ->
            :mnesia.write({@table_name, user_id, user_tag, conn_pid, lobby_id, online_since})
        end
      end)

    case result do
      {:ok, :ok} -> :ok
      {:error, reason} -> {:error, reason}
      reason -> {:error, reason}
    end
  end

  def get_lobby_id(user_id) when is_binary(user_id) do
    result = mnesia_transaction(fn -> :mnesia.read({@table_name, user_id}) end)

    case result do
      {:ok, []} -> {:ok, nil}
      {:ok, [{@table_name, ^user_id, _, _, lobby_id, _}]} -> {:ok, lobby_id}
      _ -> result
    end
  end

  def get_lobby_id!(user_id) do
    case get_lobby_id(user_id) do
      {:ok, res} -> res
      {:error, reason} -> throw(reason)
    end
  end

  def get_lobby_id_by_tag(user_tag) when is_binary(user_tag) do
    result = mnesia_transaction(fn -> :mnesia.index_read(@table_name, user_tag, :user_tag) end)

    case result do
      {:ok, []} -> {:ok, nil}
      {:ok, [{@table_name, _, ^user_tag, _, lobby_id, _}]} -> {:ok, lobby_id}
      _ -> result
    end
  end

  def get_lobby_id_by_tag!(user_tag) do
    case get_lobby_id_by_tag(user_tag) do
      {:ok, res} -> res
      {:error, reason} -> throw(reason)
    end
  end

  def is_online(user_id) when is_binary(user_id) do
    case whereis(user_id) do
      {:ok, conn_pid} when is_pid(conn_pid) -> true
      _ -> false
    end
  end

  def is_online_by_tag(user_tag) when is_binary(user_tag) do
    case whereis_by_tag(user_tag) do
      {:ok, conn_pid} when is_pid(conn_pid) -> true
      _ -> false
    end
  end

  @doc """
  Execute the given function if the `user_id` is online.
  The PID of the user's client connection is passed.
  """
  def if_online(user_id, callback, offline_callback \\ nil)
      when is_binary(user_id) and is_function(callback, 1) do
    case whereis(user_id) do
      {:ok, conn_pid} when is_pid(conn_pid) ->
        {:ok, callback.(conn_pid)}

      _ ->
        if offline_callback == nil do
          {:error, :offline}
        else
          {:error, offline_callback.()}
        end
    end
  end

  def if_online_by_tag(user_tag, callback, offline_callback \\ nil)
      when is_binary(user_tag) and is_function(callback, 1) do
    case whereis_by_tag(user_tag) do
      {:ok, conn_pid} when is_pid(conn_pid) ->
        {:ok, callback.(conn_pid)}

      _ ->
        if offline_callback == nil do
          {:error, :offline}
        else
          {:error, offline_callback.()}
        end
    end
  end
end
