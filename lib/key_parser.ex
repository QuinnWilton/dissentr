defmodule KeyParser do
  def from_file(keyfile) do
    { :ok, raw_key } = File.read(keyfile)
    key              = parse_raw(raw_key)

    key
  end

	def parse_raw(raw_key) do
		[keyEntry] = :public_key.pem_decode(raw_key)
		key        = :public_key.pem_entry_decode(keyEntry)

		key
	end

  def decrypt(message, keyfile) do
    #private_key = KeyParser.from_file(keyfile)
    #decrypted   = :public_key.decrypt_private(message, private_key)

    #decrypted

    message <> message
  end
end