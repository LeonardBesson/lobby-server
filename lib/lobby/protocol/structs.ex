defmodule Lobby.Protocol.Structs do
  @moduledoc """
  Structs used in messages
  """
  import Bincode
  alias Lobby.Accounts.User
  alias Lobby.Profiles.Profile

  declare_struct(
    Lobby.UserProfile,
    [user_tag: :string, display_name: :string, avatar_url: {:option, :string}],
    absolute: true
  )

  def get_user_profile(nil), do: nil
  def get_user_profile(%User{profile: nil}), do: nil

  def get_user_profile(%User{
        user_tag: user_tag,
        profile: %Profile{display_name: display_name, avatar_url: avatar_url}
      }) do
    %Lobby.UserProfile{user_tag: user_tag, display_name: display_name, avatar_url: avatar_url}
  end
end
