defmodule Lobby.Accounts.User do
  @moduledoc false
  use Lobby.Ecto.BaseSchema
  alias Lobby.Bans.Ban
  alias Lobby.Profiles.Profile
  alias Lobby.Friends.FriendRequest
  import Ecto.Changeset
  require Lobby

  @display_tag_validation Lobby.compile_env!(:display_tag_validation)

  schema "users" do
    field(:email, :string)
    field(:user_tag, :string)
    field(:is_active, :boolean, default: true)
    field(:password, :string, virtual: true)
    field(:password_hash, :string)
    has_one(:profile, Profile)
    has_many(:bans, Ban)
    has_many(:sent_friend_requests, FriendRequest, foreign_key: :inviter_id)
    has_many(:received_friend_requests, FriendRequest, foreign_key: :invitee_id)

    timestamps()
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :user_tag, :is_active, :password])
    |> validate_required([:email, :user_tag])
    |> validate_format(:user_tag, @display_tag_validation)
    |> unique_constraint(:email)
    |> unique_constraint(:user_tag)
    |> put_password_hash()
  end

  defp put_password_hash(
         %Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset
       ) do
    change(changeset, Bcrypt.add_hash(password))
  end

  defp put_password_hash(changeset), do: changeset
end
