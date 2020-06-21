defmodule Lobby.ClientRegistry do
  @moduledoc false

  require Logger

  @table_name ClientRegistryEntry

  def table_name, do: @table_name

  def create_table do
    {:atomic, :ok} =
      :mnesia.create_table(@table_name, attributes: [:user_id, :user_tag, :conn_pid])

    {:atomic, :ok} = :mnesia.add_table_index(@table_name, :user_tag)
  end

  def client_authenticated(user_id, user_tag, conn_pid)
      when is_binary(user_id) and is_pid(conn_pid) do
    transaction =
      :mnesia.transaction(fn -> :mnesia.write({@table_name, user_id, user_tag, conn_pid}) end)

    case transaction do
      {:atomic, :ok} -> :ok
      {:aborted, reason} -> {:error, reason}
    end
  end

  def client_disconnected(nil), do: :ok

  def client_disconnected(user_id) when is_binary(user_id) do
    transaction = :mnesia.transaction(fn -> :mnesia.delete({@table_name, user_id}) end)

    case transaction do
      {:atomic, :ok} -> :ok
      {:aborted, reason} -> {:error, reason}
    end
  end

  def whereis(user_id) when is_binary(user_id) do
    transaction = :mnesia.transaction(fn -> :mnesia.read({@table_name, user_id}) end)

    case transaction do
      {:atomic, []} -> {:ok, nil}
      {:atomic, [{@table_name, ^user_id, _, conn_pid}]} -> {:ok, conn_pid}
      {:aborted, reason} -> {:error, reason}
    end
  end

  def whereis!(user_id) do
    case whereis(user_id) do
      {:ok, res} -> res
      {:error, reason} -> throw(reason)
    end
  end

  def whereis_by_tag(user_tag) when is_binary(user_tag) do
    transaction =
      :mnesia.transaction(fn -> :mnesia.index_read(@table_name, user_tag, :user_tag) end)

    case transaction do
      {:atomic, []} -> {:ok, nil}
      {:atomic, [{@table_name, _, ^user_tag, conn_pid}]} -> {:ok, conn_pid}
      {:aborted, reason} -> {:error, reason}
    end
  end

  def whereis_by_tag!(user_tag) do
    case whereis_by_tag(user_tag) do
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
