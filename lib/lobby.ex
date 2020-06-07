defmodule Lobby do
  @moduledoc """
  Documentation for `Lobby`.
  """

  def env, do: Application.get_env(:lobby, :env)

  @spec get_env!(atom) :: any | no_return
  def get_env!(key) when is_atom(key) do
    case Application.fetch_env(:lobby, key) do
      {:ok, {:system, var}} when is_binary(var) ->
        System.get_env(var)

      {:ok, value} ->
        value

      :error ->
        raise ArgumentError, "Configuration parameter #{inspect(key)} is missing"
    end
  end

  @spec compile_env!(atom) :: any | no_return
  defmacro compile_env!(key) when is_atom(key) do
    quote do
      Application.compile_env!(:lobby, unquote(key))
    end
  end
end
