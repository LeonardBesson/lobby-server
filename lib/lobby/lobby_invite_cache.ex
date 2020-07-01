defmodule Lobby.LobbyInviteCache do
  @moduledoc false
  import Lobby.Utils.Mnesia
  require Logger

  @table_name LobbyInviteCacheEntry

  def create_table do
    {:atomic, :ok} =
      :mnesia.create_table(@table_name,
        attributes: [:id, :lobby_id, :inviter_user_tag, :invitee_user_tag, :inserted_at]
      )

    {:atomic, :ok} = :mnesia.add_table_index(@table_name, :lobby_id)
    {:atomic, :ok} = :mnesia.add_table_index(@table_name, :invitee_user_tag)

    @table_name
  end

  def get_invite(id) do
    result = mnesia_transaction(fn -> :mnesia.read({@table_name, id}) end)

    case result do
      {:ok, []} ->
        {:ok, nil}

      {:ok, [{@table_name, ^id, lobby_id, inviter_user_tag, invitee_user_tag, inserted_at}]} ->
        {:ok, {id, lobby_id, inviter_user_tag, invitee_user_tag, inserted_at}}

      _ ->
        result
    end
  end

  def get_invite!(id) do
    case get_invite(id) do
      {:ok, nil} -> nil
      {:ok, res} -> res
      {:error, reason} -> throw(reason)
    end
  end

  def get_or_create_invite(lobby_id, inviter_user_tag, invitee_user_tag) do
    mnesia_transaction(fn ->
      case :mnesia.match_object({@table_name, :_, lobby_id, :_, invitee_user_tag, :_}) do
        [] ->
          id = Ecto.UUID.generate()

          with :ok <-
                 :mnesia.write(
                   {@table_name, id, lobby_id, inviter_user_tag, invitee_user_tag,
                    DateTime.utc_now()}
                 ),
               do: id

        [{@table_name, id, ^lobby_id, _, ^invitee_user_tag, _}] ->
          id

        _ ->
          :mnesia.abort("Duplicate lobby invite")
      end
    end)
  end

  def delete_invite(id) do
    result = mnesia_transaction(fn -> :mnesia.delete({@table_name, id}) end)

    case result do
      {:ok, :ok} -> :ok
      _ -> result
    end
  end
end
