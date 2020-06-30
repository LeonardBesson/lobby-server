defmodule Lobby.Lobbies.LobbyServer do
  @moduledoc """
  Process representing a "lobby". A group of users queuing for a game.
  """
  use GenServer
  import Lobby.Utils.Mnesia
  alias Lobby.LobbyRegistry
  alias Lobby.ClientRegistry
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

  def add_member(lobby, inviter, invitee, role \\ :member) when role in @valid_roles do
    GenServer.call(lobby, {:add_member, inviter, invitee, role})
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
  def handle_call({:add_member, inviter, invitee, role}, _, %{id: id, members: members} = state) do
    if can_invite?(inviter, state) do
      case ClientRegistry.set_lobby_id(invitee, id) do
        :ok ->
          members = Map.put(members, invitee, role)
          Logger.debug("Member #{invitee} added as #{role} to lobby #{state.id}")
          {:reply, :ok, %{state | members: members}}

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      {:reply, {:error, :not_leader}, state}
    end
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
