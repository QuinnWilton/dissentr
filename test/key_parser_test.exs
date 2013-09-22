Code.require_file "test_helper.exs", __DIR__

defmodule KeyParserTest do
  use ExUnit.Case

  test "parses keys from files" do
    expected_key = {:RSAPublicKey, 232949724401241277535825317330854533509, 65537}
    path    = 'test/fixtures/pub-test.pem'

    assert KeyParser.from_file(path) == expected_key
  end

  test "parses public RSA keys" do
    expected_key = {:RSAPublicKey, 232949724401241277535825317330854533509, 65537}
  	path    = 'test/fixtures/pub-test.pem'
    key_bin = File.read!(path)
    assert KeyParser.parse_raw(key_bin) == expected_key
  end

  test "parses private RSA keys" do
  	expected_key = {:RSAPrivateKey, :"two-prime", 232949724401241277535825317330854533509, 65537, 68391681755409668133710426245930466445, 15902876728072726103, 14648275804718038403, 15027377708665576007, 13183828193864435283, 9577991315827861001, :asn1_NOVALUE}

  	path    = 'test/fixtures/priv-test.pem'
    key_bin = File.read!(path)
    assert KeyParser.parse_raw(key_bin) == expected_key
  end
end