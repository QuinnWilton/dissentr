defmodule Dissentr do
  use Application.Behaviour

  def start(_type, {}) do
    Dissentr.Cascade.start_link({ :node1, nil, "example_data/pub1.pem",
    										                       "example_data/priv1.pem" })

    Dissentr.Cascade.start_link({ :node2, :node1, "example_data/pub2.pem",
    										                          "example_data/priv2.pem" })

    Dissentr.Cascade.start_link({ :node3, :node2, "example_data/pub3.pem",
    										                          "example_data/priv3.pem" })

    Dissentr.Cascade.start_link({ :node4, :node3, "example_data/pub4.pem",
                                                  "example_data/priv4.pem" })

    Dissentr.Cascade.start_link({ :node5, :node4, "example_data/pub5.pem",
                                                  "example_data/priv5.pem" })
  end
end
