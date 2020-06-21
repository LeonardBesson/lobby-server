defmodule Lobby.ProfileCache do
  @moduledoc false
  alias Lobby.UserProfile
  alias Lobby.Accounts
  alias Lobby.Accounts.User
  alias Lobby.Profiles.Profile
  import Lobby.Protocol.Structs

  @table_name ProfileCacheEntry

  def create_table do
    {:atomic, :ok} =
      :mnesia.create_table(@table_name,
        attributes: [:user_id, :user_tag, :display_name, :avatar_url]
      )

    {:atomic, :ok} = :mnesia.add_table_index(@table_name, :user_tag)

    @table_name
  end

  def get_or_create(user_id) do
    transaction =
      :mnesia.transaction(fn ->
        case :mnesia.read({@table_name, user_id}) do
          [{@table_name, ^user_id, user_tag, display_name, avatar_url}] ->
            %UserProfile{user_tag: user_tag, display_name: display_name, avatar_url: avatar_url}

          _ ->
            create_by(id: user_id)
        end
      end)

    case transaction do
      {:atomic, nil} -> {:ok, nil}
      {:atomic, profile} -> {:ok, profile}
      {:aborted, reason} -> {:error, reason}
    end
  end

  def get_or_create_by_tag(user_tag) do
    transaction =
      :mnesia.transaction(fn ->
        case :mnesia.index_read(@table_name, user_tag, :user_tag) do
          [{@table_name, _, ^user_tag, display_name, avatar_url}] ->
            %UserProfile{user_tag: user_tag, display_name: display_name, avatar_url: avatar_url}

          _ ->
            create_by(user_tag: user_tag)
        end
      end)

    case transaction do
      {:atomic, nil} -> {:ok, nil}
      {:atomic, profile} -> {:ok, profile}
      {:aborted, reason} -> {:error, reason}
    end
  end

  defp create_by(clauses) do
    case Accounts.get_by_with_profile(clauses) do
      %User{
        id: user_id,
        user_tag: user_tag,
        profile: %Profile{display_name: display_name, avatar_url: avatar_url}
      } = user ->
        :ok = :mnesia.write({@table_name, user_id, user_tag, display_name, avatar_url})

        %UserProfile{
          user_tag: user_tag,
          display_name: display_name,
          avatar_url: avatar_url
        }

      _ ->
        nil
    end
  end
end
