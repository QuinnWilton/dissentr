defmodule Dissentr.Cascade do
  use Supervisor.Behaviour

  def start_link(name, target) do
    :supervisor.start_link( __MODULE__, {name, target})
  end

  def init({name, target}) do
    public_keyfile = "test/fixtures/pub-test.pem"
    private_keyfile = "test/fixtures/priv-test.pem"

    node = worker(Dissentr.Node, [name,
                                  target,
                                  public_keyfile,
                                  private_keyfile])

    supervise( [ node ], strategy: :one_for_one)
  end
end