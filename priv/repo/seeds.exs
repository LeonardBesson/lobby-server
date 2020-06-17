alias Lobby.Accounts
alias Lobby.Profiles

if Lobby.env() != :prod do
  {:ok, user} = Accounts.create_user(%{email: "dev@lobby.com", password: "admin", user_tag: "dev#111111"})
  {:ok, _} = Profiles.create_profile(%{user_id: user.id, display_name: "Dev"})
end

