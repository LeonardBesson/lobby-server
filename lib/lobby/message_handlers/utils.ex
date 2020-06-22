defmodule Lobby.MessageHandlers.Utils do
  @moduledoc """
  Utilities to define and manipulate message handlers.
  """

  defmacro register_message_handlers do
    %Macro.Env{module: caller_module} = __CALLER__

    handlers = Lobby.get_env!(:message_handlers)

    message_handlers =
      Enum.reduce(handlers, %{}, fn handler, handlers ->
        Enum.reduce(handler.message_types(), handlers, fn type, handlers ->
          Map.update(handlers, type, [handler], fn existing_handlers ->
            [handler | existing_handlers]
          end)
        end)
      end)
      |> Enum.map(fn {type, handlers} -> {type, Enum.reverse(handlers)} end)
      |> Enum.into(%{})

    Module.put_attribute(caller_module, :message_handlers, message_handlers)
  end
end
