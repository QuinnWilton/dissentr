defmodule KeyParser do
  def from_file(keyfile) do
    { :ok, raw_key } = File.read(keyfile)
    key              = parse_raw(raw_key)
  end

  def parse_raw(raw_key) do
    [keyEntry] = :public_key.pem_decode(raw_key)
    key        = :public_key.pem_entry_decode(keyEntry)
  end
end