defmodule Lobby.ClientState do
  @moduledoc """
  Struct holding the state of a client connection.
  """
  defstruct conn: nil,
            user: nil,
            flush_timer: nil,
            auth_timeout_timer: nil,
            ping_timer: nil,
            last_ping_id: nil,
            last_ping_time: nil,
            round_trip_ms: nil

  @type t :: %__MODULE__{
          conn: Connection.t(),
          user: map,
          flush_timer: reference,
          auth_timeout_timer: reference,
          ping_timer: reference,
          last_ping_id: String.t(),
          last_ping_time: DateTime.t(),
          round_trip_ms: non_neg_integer
        }
end
