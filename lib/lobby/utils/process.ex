defmodule Lobby.Utils.Process do
  @moduledoc false

  @doc """
  Returns true if the process associated with the given PID is alive.
  Works on local and remove processes.
  """
  def process_alive?(pid) when is_pid(pid) do
    case :rpc.pinfo(pid, :status) do
      {:status, :running} -> true
      _ -> false
    end
  end
end
