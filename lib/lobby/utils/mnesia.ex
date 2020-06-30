defmodule Lobby.Utils.Mnesia do
  @moduledoc """
  Utils to make the Mnesia API more Elixir friendly.
  """

  def mnesia_transaction(transaction) when is_function(transaction, 0) do
    case :mnesia.transaction(fn -> transaction.() end) do
      {:atomic, nil} -> {:ok, nil}
      {:atomic, result} -> {:ok, result}
      {:aborted, reason} -> {:error, reason}
    end
  end
end
