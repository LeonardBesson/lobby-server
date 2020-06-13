alias Lobby.Accounts

if Lobby.env() != :prod do
  Accounts.create_user(%{email: "dev@lobby.com", password: "admin"})
end

