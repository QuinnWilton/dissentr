defmodule Dissentr.Node do
	use GenServer.Behaviour

  defrecord NodeInfo, next: nil,
                      public_keyfile: nil,
                      private_keyfile: nil

  def start_link(name, next, public_keyfile, private_keyfile) do
    state = { next, public_keyfile, private_keyfile }
    :gen_server.start_link( { :local, name }, __MODULE__, state, [])
  end

	def init({ next, public_keyfile, private_keyfile }) do
    node_info = NodeInfo.new(next: next,
                             public_keyfile: public_keyfile,
                             private_keyfile: private_keyfile)

		{ :ok, node_info }
	end

  def handle_call(:public_key, _from, node_info) do
    public_key = KeyParser.from_file(node_info.public_keyfile)

    { :reply, public_key, node_info }
  end

  def handle_cast({ :handle, message },
                  node_info = NodeInfo[next: nil]) do
      decrypted = KeyParser.decrypt(message, node_info.private_keyfile)

      IO.puts "Final message: #{decrypted}"

      { :noreply, node_info }
  end

  def handle_cast({ :handle, message }, node_info) do
    decrypted = KeyParser.decrypt(message, node_info.private_keyfile)

    IO.puts "Hop message: #{decrypted}"

    :gen_server.cast(node_info.next, { :handle, decrypted })

    { :noreply, node_info }
  end
end