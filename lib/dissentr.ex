defmodule Dissentr do
  use Application.Behaviour

  def start(_type, {}) do
    Dissentr.Cascade.start_link(:node1, nil)
    Dissentr.Cascade.start_link(:node2, :node1)
    Dissentr.Cascade.start_link(:node3, :node2)
  end
end
