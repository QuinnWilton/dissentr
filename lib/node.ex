defmodule Dissentr.Node do
	use GenServer.Behaviour

  defrecord NodeInfo, next: nil,
                      public_keys: nil,
                      private_keyfile: nil

  def start_link(name, state) do
    :gen_server.start_link( { :local, name }, __MODULE__, state, [])
  end

	def init({ next, public_keyfile, private_keyfile }) do
    public_key = KeyParser.from_file(public_keyfile)
    public_keys = accumulate_public_keys(next) ++ [public_key]

    node_info = NodeInfo.new(next: next,
                             public_keys: public_keys,
                             private_keyfile: private_keyfile)

		{ :ok, node_info }
	end

  def handle_call(:public_keys, _from, node_info) do
    { :reply, node_info.public_keys, node_info }
  end

  def handle_cast({ :handle, message, [encrypted_key] },
                  node_info = NodeInfo[next: nil]) do
    private_rsa_key = KeyParser.from_file(node_info.private_keyfile)
    plaintext       = CryptoHybrid.decrypt_hybrid(message,
                                                  encrypted_key,
                                                  private_rsa_key)

    IO.puts "Result: #{plaintext}"

    { :noreply, node_info }
  end

  def handle_cast({ :handle, message, [encrypted_key|next_keys] }, node_info) do
    private_rsa_key = KeyParser.from_file(node_info.private_keyfile)
    plaintext       = CryptoHybrid.decrypt_hybrid(message,
                                                  encrypted_key,
                                                  private_rsa_key)

    :gen_server.cast(node_info.next, { :handle, plaintext, next_keys })

    { :noreply, node_info }
  end

  def accumulate_public_keys(nil) do
    []
  end

  def accumulate_public_keys(next) do
    :gen_server.call(next, :public_keys)
  end
end