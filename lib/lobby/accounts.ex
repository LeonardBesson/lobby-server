defmodule Lobby.Accounts do
  @moduledoc false
  import Ecto.Query
  alias Lobby.Accounts.User
  alias Lobby.Repo

  def authenticate(email, password) do
    case get_by_email(email) do
      %User{} = user ->
        if Bcrypt.verify_pass(password, user.password_hash) do
          {:ok, user |> Repo.preload(:profile)}
        else
          {:error, :invalid_credentials}
        end

      nil ->
        {:error, :not_found}
    end
  end

  def get_by_email(email) do
    Repo.get_by(User, email: email)
  end

  def get_by_user_tag(user_tag) do
    Repo.get_by(User, user_tag: user_tag)
  end

  def create_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

  def update_user(%User{} = user, attrs) do
    user
    |> User.changeset(attrs)
    |> Repo.update()
  end

  def delete_user(%User{} = user) do
    Repo.delete(user)
  end
end
