defmodule Lobby.Utils.Crypto do
  @moduledoc """
  Cryptographic utilities
  """
  require Lobby

  @session_token_bytes Lobby.compile_env!(:session_token_bytes)

  def gen_session_token do
    :crypto.strong_rand_bytes(@session_token_bytes) |> Base.encode16()
  end
end
