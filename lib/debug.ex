defmodule Debug do
  def pp(x) do
      :io_lib.format("~p", [x])
      |> :lists.flatten
      |> :erlang.list_to_binary
  end
end