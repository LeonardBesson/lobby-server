defmodule Lobby.Lobbies.LobbyServer do
  @moduledoc """
  Process representing a "lobby". A group of users queuing for a game.
  """
  use GenServer
  import Lobby.BaseClient
  alias Lobby.LobbyRegistry
  alias Lobby.ProfileCache
  alias Lobby.ClientRegistry
  alias Lobby.LobbyRole
  alias Lobby.LobbyMember
  alias Lobby.Messages.SystemNotification
  alias Lobby.Messages.NewLobbyMessage
  alias Lobby.Messages.LobbyMemberUpdate
  alias Lobby.Messages.LobbyJoined
  alias Lobby.Messages.LobbyLeft
  require Logger
  require Lobby

  @valid_roles [:leader, :member]

  @default_opts [
    open_invites: false,
    max_size: Lobby.compile_env!(:lobby_default_max_size),
    max_message_history: 250,
    health_check_interval_ms: Lobby.compile_env!(:lobby_health_check_interval_ms)
  ]

  def start_link(id, leader, opts) do
    GenServer.start_link(__MODULE__, [id, leader, opts])
  end

  @impl true
  def init([id, leader, opts]) do
    default_state = fn ->
      opts = Keyword.merge(@default_opts, opts)
      %{id: id, members: %{leader => :leader}, messages: [], opts: opts}
    end

    case LobbyRegistry.register_lobby(id, self()) do
      {:ok, saved_state} ->
        Logger.debug("LobbyServer (#{id} <-> #{inspect(self())}) registered")

        case ClientRegistry.set_lobby_id(leader, id) do
          :ok ->
            state = saved_state || default_state.()
            broadcast_message(%LobbyJoined{lobby_id: id}, state)
            broadcast_members_update(state)
            Process.send_after(self(), :health_check, state.opts[:health_check_interval_ms])
            {:ok, state}

          :error ->
            {:stop, "Could not set lobby id for leader #{leader}"}
        end

      {:error, reason} ->
        Logger.debug(
          "LobbyServer (#{id} <-> #{inspect(self())}) could not be registered. Aborting init"
        )

        {:stop, reason}
    end
  end

  # TODO: chat

  def add_member_validated(lobby, inviter, invitee, role \\ :member) when role in @valid_roles do
    GenServer.call(lobby, {:add_member_validated, inviter, invitee, role})
  end

  def add_member_validated(_, _, _, role), do: {:error, :invalid_role}

  def add_member(lobby, invitee, role \\ :member) when role in @valid_roles do
    GenServer.call(lobby, {:add_member, invitee, role})
  end

  def add_member(_, _, _, role), do: {:error, :invalid_role}

  def remove_member(lobby, inviter, invitee) do
    GenServer.call(lobby, {:remove_member, inviter, invitee})
  end

  def get_members(lobby) do
    GenServer.call(lobby, :get_members)
  end

  def open_invites?(lobby) do
    opts = GenServer.call(lobby, :get_opts)
    Keyword.get(opts, :open_invites, false)
  end

  def can_user_invite?(lobby, user_tag) do
    GenServer.call(lobby, {:can_user_invite, user_tag})
  end

  def member_presence_update(lobby, user_tag, is_online) do
    GenServer.call(lobby, {:member_presence_update, user_tag, is_online})
  end

  def new_message(lobby, user_tag, content) do
    GenServer.call(lobby, {:new_message, user_tag, content})
  end

  @impl true
  def handle_call(
        {:add_member_validated, inviter, invitee, role},
        _,
        %{id: id, members: members} = state
      ) do
    if can_invite?(inviter, state) do
      {res, state} = add_member_internal(invitee, role, state)
      {:reply, res, state}
    else
      {:reply, {:error, :not_leader}, state}
    end
  end

  @impl true
  def handle_call({:add_member, invitee, role}, _, state) do
    {res, state} = add_member_internal(invitee, role, state)
    {:reply, res, state}
  end

  @impl true
  def handle_call({:remove_member, inviter, invitee}, _, %{members: members} = state) do
    cond do
      !is_leader?(inviter, state) ->
        {:reply, {:error, :not_leader}, state}

      !is_member?(invitee, state) ->
        {:reply, :ok, state}

      true ->
        case ClientRegistry.set_lobby_id(invitee, nil) do
          :ok ->
            members = Map.delete(members, invitee)
            Logger.debug("Member #{invitee} removed from lobby #{state.id}")
            {:reply, :ok, %{state | members: members}}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end
    end
  end

  @impl true
  def handle_call(:get_members, _, %{members: members} = state) do
    {:reply, members, state}
  end

  @impl true
  def handle_call(:get_opts, _, %{opts: opts} = state) do
    {:reply, opts, state}
  end

  @impl true
  def handle_call({:can_user_invite, user_tag}, _, %{members: members, opts: opts} = state) do
    can_invite?(user_tag, state)
  end

  @impl true
  def handle_call({:member_presence_update, user_tag, is_online}, _, %{members: members} = state) do
    case members do
      %{^user_tag => _} ->
        broadcast_members_update(state)

        Logger.debug(
          "LobbyServer member #{user_tag} is now #{if is_online, do: "online", else: "offline"}"
        )

        {:reply, :ok, state}

      _ ->
        {:reply, :ok, state}
    end
  end

  @impl true
  def handle_call(
        {:new_message, user_tag, content},
        _,
        %{members: members, messages: messages} = state
      ) do
    if is_member?(user_tag, state) do
      case ProfileCache.get_or_create_by_tag(user_tag) do
        {:ok, profile} ->
          messages = [{user_tag, content} | messages]
          broadcast_message(profile, content, state)
          {:reply, :ok, %{state | messages: messages}}

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      {:reply, {:error, :not_member}, state}
    end
  end

  @impl true
  def handle_info(
        :health_check,
        %{id: id, members: members, messages: messages, opts: opts} = state
      ) do
    Logger.debug("LobbyServer #{id} health check")

    not_empty =
      Enum.any?(members, fn {user_tag, _} -> ClientRegistry.is_online_by_tag(user_tag) end)

    if not_empty do
      Process.send_after(self(), :health_check, opts[:health_check_interval_ms])
      messages = Enum.take(messages, opts[:max_message_history])
      {:noreply, %{state | messages: messages}}
    else
      {:stop, {:shutdown, :empty}, state}
    end
  end

  @impl true
  def terminate(reason, %{id: id} = state) do
    Logger.debug("LobbyServer #{id} down: #{inspect(reason)}")
    broadcast_message(%LobbyLeft{lobby_id: id}, state)

    normal_exit = fn ->
      case LobbyRegistry.delete_lobby(id) do
        :ok ->
          :ok

        {:error, reason} ->
          Logger.error("Could not delete LobbyServer #{id} from registry")
          Logger.error(inspect(reason))
      end
    end

    case reason do
      r when r in [:normal, :shutdown] ->
        normal_exit.()

      {:shutdown, reason} ->
        normal_exit.()

      _ ->
        case LobbyRegistry.save_state(id, state) do
          {:error, reason} ->
            Logger.error("Could not save state of terminating LobbyServer #{id}")
            Logger.error(inspect(reason))

          _ ->
            :ok
        end
    end
  end

  defp add_member_internal(user_tag, role, %{id: id, members: members} = state) do
    case ClientRegistry.set_lobby_id(user_tag, id) do
      :ok ->
        members = Map.put(members, user_tag, role)
        Logger.debug("Member #{user_tag} added as #{role} to lobby #{state.id}")
        state = %{state | members: members}
        send_to_member(user_tag, %LobbyJoined{lobby_id: id})
        broadcast_members_update(state)

        broadcast_system_message("User #{user_tag} joined", state)
        {:ok, state}

      {:error, reason} ->
        {{:error, reason}, state}
    end
  end

  defp broadcast_system_notification(content, state) do
    broadcast_message(%SystemNotification{content: content}, state)
  end

  defp broadcast_system_message(content, state) do
    broadcast_message(nil, content, state)
  end

  defp broadcast_message(from_profile, content, %{id: id, members: members} = state) do
    broadcast_message(
      %NewLobbyMessage{lobby_id: id, profile: from_profile, content: content},
      state
    )
  end

  defp broadcast_message(message, %{members: members} = state) do
    Enum.each(members, fn {user_tag, _} ->
      send_to_member(user_tag, message)
    end)
  end

  defp broadcast_members_update(%{id: id, members: members} = state) do
    broadcast_message(
      %LobbyMemberUpdate{lobby_id: id, members: members_with_profile(members)},
      state
    )
  end

  defp send_to_member(user_tag, message) when is_binary(user_tag) do
    ClientRegistry.if_online_by_tag(user_tag, fn client_pid ->
      send_message(client_pid, message)
    end)
  end

  defp is_leader?(user_tag, %{members: members} = state) do
    case members do
      %{^user_tag => :leader} -> true
      _ -> false
    end
  end

  defp is_member?(user_tag, %{members: members} = state) do
    case members do
      %{^user_tag => _} -> true
      _ -> false
    end
  end

  defp can_invite?(user_tag, %{opts: opts} = state) do
    Keyword.get(opts, :open_invites, false) or is_leader?(user_tag, state)
  end

  defp members_with_profile(members) do
    Enum.map(members, fn {user_tag, role} ->
      %LobbyMember{
        user_profile: ProfileCache.get_or_create_by_tag!(user_tag),
        role: get_role(role),
        is_online: ClientRegistry.is_online_by_tag(user_tag)
      }
    end)
  end

  defp get_role(:leader), do: %LobbyRole.Leader{}
  defp get_role(:member), do: %LobbyRole.Member{}

  def child_spec(arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, arg},
      restart: :transient
    }
  end
end
