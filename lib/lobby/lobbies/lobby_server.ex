defmodule Lobby.Lobbies.LobbyServer do
  @moduledoc """
  Process representing a "lobby". A group of users queuing for a game.
  """
  use GenServer
  import Lobby.BaseClient
  alias Lobby.LobbyRegistry
  alias Lobby.ClientRegistry
  alias Lobby.Messages.SystemNotification
  require Logger
  require Lobby

  @valid_roles [:leader, :member]

  @default_opts [
    open_invites: false,
    max_size: Lobby.compile_env!(:lobby_default_max_size),
    max_message_history: 250
  ]

  def start_link(id, leader, opts) do
    GenServer.start_link(__MODULE__, [id, leader, opts])
  end

  @impl true
  def init([id, leader, opts]) do
    default_state = fn ->
      opts = Keyword.merge(@default_opts, opts)
      %{id: id, members: %{leader => :leader}, opts: opts}
    end

    case LobbyRegistry.register_lobby(id, self()) do
      {:ok, saved_state} ->
        Logger.debug("LobbyServer (#{id} <-> #{inspect(self())}) registered")

        case ClientRegistry.set_lobby_id(leader, id) do
          :ok -> {:ok, saved_state || default_state.()}
          :error -> {:stop, "Could not set lobby id for leader #{leader}"}
        end

      {:error, reason} ->
        Logger.debug(
          "LobbyServer (#{id} <-> #{inspect(self())}) could not be registered. Aborting init"
        )

        {:stop, reason}
    end
  end

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

  def open_invites?(lobby) do
    opts = GenServer.call(lobby, :get_opts)
    Keyword.get(opts, :open_invites, false)
  end

  def can_user_invite?(lobby, user_tag) do
    GenServer.call(lobby, {:can_user_invite, user_tag})
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
  def handle_call(:get_opts, _, %{opts: opts} = state) do
    {:reply, opts, state}
  end

  @impl true
  def handle_call({:can_user_invite, user_tag}, _, %{members: members, opts: opts} = state) do
    can_invite?(user_tag, state)
  end

  @impl true
  def terminate(reason, %{id: id} = state) do
    Logger.debug("LobbyServer #{id} down: #{inspect(reason)}")

    case LobbyRegistry.lobby_terminating(id, state) do
      {:error, reason} ->
        Logger.error("Could not save state of terminating LobbyServer #{id}")
        Logger.error(inspect(reason))

      _ ->
        :ok
    end
  end

  defp add_member_internal(user_tag, role, %{id: id, members: members} = state) do
    case ClientRegistry.set_lobby_id(user_tag, id) do
      :ok ->
        members = Map.put(members, user_tag, role)
        Logger.debug("Member #{user_tag} added as #{role} to lobby #{state.id}")
        # TODO: this should be a chat message instead of notification
        state = %{state | members: members}
        broadcast_system_notification("User #{user_tag} joined", state)
        {:ok, state}

      {:error, reason} ->
        {{:error, reason}, state}
    end
  end

  defp broadcast_system_notification(content, state) do
    broadcast_message(%SystemNotification{content: content}, state)
  end

  defp broadcast_message(message, %{members: members} = state) do
    Enum.each(members, fn {user_tag, _} ->
      ClientRegistry.if_online_by_tag(user_tag, fn client_pid ->
        send_message(client_pid, message)
      end)
    end)
  end

  # TODO: chat

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

  def child_spec(arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, arg}
    }
  end
end
