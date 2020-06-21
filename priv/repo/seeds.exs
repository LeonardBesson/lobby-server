alias Lobby.Accounts
alias Lobby.Profiles

if Lobby.env() != :prod do
  {:ok, user} = Accounts.create_user(%{email: "dev@lobby.com", password: "admin", user_tag: "dev#1111"})
  {:ok, _} = Profiles.create_profile(%{user_id: user.id, display_name: "Dev"})

  {:ok, user} = Accounts.create_user(%{email: "dev2@lobby.com", password: "admin", user_tag: "dev#2222"})
  {:ok, _} = Profiles.create_profile(%{user_id: user.id, display_name: "Dev2"})
end

