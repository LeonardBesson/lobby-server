defmodule Lobby.MixProject do
  use Mix.Project

  def project do
    [
      app: :lobby,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Lobby.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ranch, "~> 2.0.0-rc.2"},
      {:bincode, "~> 0.2"}
    ]
  end
end
