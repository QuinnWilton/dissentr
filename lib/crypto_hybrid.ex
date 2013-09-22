defmodule CryptoHybrid do
  def encrypt_hybrid(message, rsa_key) do
    aes_key           = generate_aes_key(16)
    ciphertext        = encrypt_aes(message, aes_key)
    encrypted_aes_key = encrypt_rsa(aes_key, rsa_key)

    {ciphertext, encrypted_aes_key}
  end

  def decrypt_hybrid(message, encrypted_aes_key, rsa_key) do
    aes_key   = decrypt_rsa(encrypted_aes_key, rsa_key)
    plaintext = decrypt_aes(message, aes_key)

    plaintext
  end

	def generate_aes_key(size_in_bytes) do
		:crypto.strong_rand_bytes(size_in_bytes)
	end

	def generate_aes_iv(size_in_bytes) do
		:binary.copy(<<0>>, size_in_bytes)
	end

	def encrypt_aes(message, key) do
		iv = generate_aes_iv(16)
    #padded_message = pad(message, 16)
		:crypto.block_encrypt(:aes_cfb128, key, iv, message)
	end

	def decrypt_aes(message, key) do
		iv        = generate_aes_iv(16)
		plaintext = :crypto.block_decrypt(:aes_cfb128, key, iv, message)
    #unpadded  = unpad(plaintext)
	end

	def encrypt_rsa(message, key) do
		:public_key.encrypt_public(message, key)
	end

	def decrypt_rsa(message, key) do
		:public_key.decrypt_private(message, key)
	end

  # Using the algorithm described in [PKCS5]
  def pad(binary, padding) do
    missing_bytes = padding - rem(String.length(binary), padding)
    binary <> :binary.copy(<<missing_bytes>>, missing_bytes)
  end

  def unpad(binary) do
    original_length = String.length(binary) - :binary.last(binary)
    :binary.part(binary, 0, original_length)
  end
end