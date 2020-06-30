defmodule Lobby.Protocol.Packet do
  @moduledoc """
  A packet sent to or received from the network
  """
  use Bitwise
  import Lobby.Protocol.PacketDefinition
  alias Lobby.UserProfile
  alias Lobby.Friend
  alias Lobby.FriendRequestAction
  alias Lobby.FriendRequest
  alias Lobby.LobbyInviteAction

  defstruct flags: 0, packet_type: 0, data: <<>>

  @type t :: %__MODULE__{
          flags: byte,
          packet_type: atom,
          data: binary
        }

  declare_packets(
    fatal_error: {0, [message: :string]},
    packet_init: {1, [protocol_version: :u16, app_version: :u16]},
    authentication_request: {2, [email: :string, password: :string]},
    authentication_response:
      {3,
       [
         error_code: {:option, :string},
         session_token: {:option, :string},
         user_profile: {:option, UserProfile}
       ]},
    packet_ping: {4, [id: :string, peer_time: :u64]},
    packet_pong: {5, [id: :string, peer_time: :u64]},
    add_friend_request: {6, [user_tag: :string]},
    add_friend_request_response: {7, [user_tag: :string, error_code: {:option, :string}]},
    friend_request_action: {8, [request_id: :string, action: FriendRequestAction]},
    friend_request_action_response: {9, [request_id: :string, error_code: {:option, :string}]},
    fetch_pending_friend_requests: {10, []},
    fetch_pending_friend_requests_response:
      {11,
       [pending_as_inviter: {:list, FriendRequest}, pending_as_invitee: {:list, FriendRequest}]},
    fetch_friend_list: {12, []},
    fetch_friend_list_response: {13, [friend_list: {:list, Friend}]},
    remove_friend: {14, [user_tag: :string]},
    remove_friend_response: {15, [error_code: {:option, :string}]},
    send_private_message: {16, [user_tag: :string, content: :string]},
    new_private_message: {17, [profile: UserProfile, content: :string, is_self: :bool]},
    system_notification: {18, [content: :string]},
    invite_user: {19, [user_tag: :string]},
    lobby_invite: {20, [id: :string, inviter: UserProfile]},
    lobby_invite_action: {21, [invite_id: :string, action: LobbyInviteAction]}
  )

  @flag_fixed_header 1 <<< 7
  @flag_short_type 1 <<< 6
  @flag_short_size 1 <<< 5

  defmacro fixed_header?(flags) do
    quote do: (unquote(flags) &&& unquote(@flag_fixed_header)) != 0
  end

  defmacro short_type?(flags) do
    quote do: (unquote(flags) &&& unquote(@flag_short_type)) != 0
  end

  defmacro short_size?(flags) do
    quote do: (unquote(flags) &&& unquote(@flag_short_size)) != 0
  end

  def new(type, data) when is_atom(type) and is_binary(data) do
    packet_info = get!(type)

    packet_id = get_id!(type)

    size = byte_size(data)

    if packet_info.fixed_size != nil and size != packet_info.fixed_size do
      raise ArgumentError,
        message:
          "Attempt to create fixed size packet #{type} with wrong size. (fixed: #{
            packet_info.fixed_size
          }, actual: #{size})"
    end

    flags = @flag_fixed_header

    flags =
      if packet_id < 256 do
        flags ||| @flag_short_type
      else
        flags
      end

    flags =
      if size < 256 do
        flags ||| @flag_short_size
      else
        flags
      end

    %__MODULE__{flags: flags, packet_type: type, data: data}
  end

  @doc """
  Return true if the packet of the given type is declared
  """
  def has?(type), do: Map.has_key?(@packet_infos, type)

  @doc """
  Return the packet info for the given packet type
  """
  def get!(type) do
    case @packet_infos do
      %{^type => packet_info} -> packet_info
      _ -> raise ArgumentError, message: "Packet #{inspect(type)} not declared"
    end
  end

  @doc """
  Return the packet id for the given packet type
  """
  def get_id!(%__MODULE__{packet_type: type}), do: get_id!(type)

  def get_id!(type) do
    case @packet_types do
      %{^type => int_type} -> int_type
      _ -> raise ArgumentError, message: "Packet #{inspect(type)} not declared"
    end
  end

  @doc """
  Return the packet type for the given packet id
  """
  def get_type!(packet_id) do
    case @packet_ids do
      %{^packet_id => type} -> type
      _ -> raise ArgumentError, message: "Packet ID #{inspect(packet_id)} not declared"
    end
  end

  def data_size(%__MODULE__{data: data}) do
    byte_size(data)
  end
end
