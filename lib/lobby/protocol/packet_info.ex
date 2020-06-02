defmodule Lobby.Protocol.PacketInfo do
  @moduledoc """
  Struct holding metadata info about a packet type
  """

  defstruct packet_type: nil, name: nil, fixed_size: nil

  @type t :: %__MODULE__{
          packet_type: non_neg_integer,
          name: String.t(),
          fixed_size: nil | non_neg_integer
        }
end
