defmodule Lobby.ClientRegistry do
  @moduledoc false

  require Logger

  @table_name ClientRegistryEntry

  def table_name, do: @table_name

  def create_table do
    {:atomic, :ok} = :mnesia.create_table(@table_name, attributes: [:user_id, :conn_pid])
  end

  def client_authenticated(user_id, conn_pid) when is_binary(user_id) and is_pid(conn_pid) do
    transaction = :mnesia.transaction(fn -> :mnesia.write({@table_name, user_id, conn_pid}) end)

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
      {:atomic, [{@table_name, ^user_id, conn_pid}]} -> {:ok, conn_pid}
      {:aborted, reason} -> {:error, reason}
    end
  end

  def whereis!(user_id) do
    case whereis(user_id) do
      {:ok, res} -> res
      {:error, reason} -> throw(reason)
    end
  end

  @doc """
  Execute the given function if the `user_id` is online.
  The PID of the user's client connection is passed.
  """
  def if_online(user_id, callback) when is_binary(user_id) and is_function(callback, 1) do
    case whereis(user_id) do
      {:ok, conn_pid} when is_pid(conn_pid) -> {:ok, callback.(conn_pid)}
      _ -> {:error, :offline}
    end
  end
end
