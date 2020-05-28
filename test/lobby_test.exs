defmodule LobbyTest do
  use ExUnit.Case
  doctest Lobby

  test "greets the world" do
    assert Lobby.hello() == :world
  end
end
