defmodule Lobby.FriendsCache do
  @moduledoc false
  import Lobby.Utils.Mnesia
  alias Lobby.Friends
  require Logger

  @table_name FriendsCacheEntry

  def create_table do
    {:atomic, :ok} =
      :mnesia.create_table(@table_name,
        attributes: [:id, :user_tag, :other_user_tag, :are_friends]
      )

    {:atomic, :ok} = :mnesia.add_table_index(@table_name, :user_tag)
    {:atomic, :ok} = :mnesia.add_table_index(@table_name, :other_user_tag)

    @table_name
  end

  def are_friends(user_tag, other_user_tag) do
    result =
      mnesia_transaction(fn ->
        case select(user_tag, other_user_tag) do
          [] ->
            are_friends = Friends.are_friends(user_tag, other_user_tag)

            :mnesia.write(
              {@table_name, Ecto.UUID.generate(), user_tag, other_user_tag, are_friends}
            )

            are_friends

          [[_, _, _, are_friends]] ->
            are_friends
        end
      end)

    case result do
      {:ok, are_friends} ->
        are_friends

      {:error, reason} ->
        Logger.error("FriendsCache add_friends/2: #{inspect(reason)}")
        false
    end
  end

  def update(user_tag, other_user_tag, are_friends) do
    result =
      mnesia_transaction(fn ->
        case select(user_tag, other_user_tag) do
          [] ->
            :ok

          [[id, user_tag, other_user_tag, _]] ->
            :mnesia.write({@table_name, id, user_tag, other_user_tag, are_friends})
        end
      end)

    case result do
      {:ok, _} ->
        :ok

      {:error, reason} ->
        Logger.error("FriendsCache update/3: #{inspect(reason)}")
        :error
    end
  end

  defp select(user_tag, other_user_tag) do
    :mnesia.select(@table_name, [
      {{@table_name, :"$1", :"$2", :"$3", :"$4"},
       [
         {:orelse, {:andalso, {:==, :"$2", user_tag}, {:==, :"$3", other_user_tag}},
          {:andalso, {:==, :"$2", other_user_tag}, {:==, :"$3", user_tag}}}
       ], [:"$$"]}
    ])
  end
end
