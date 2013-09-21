defmodule Dissentr.Mixfile do
  use Mix.Project

  def project do
    [ app: :dissentr,
      version: "0.0.1",
      elixir: "~> 0.10.0",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [:dissentr],
      mod: { Dissentr, {} } ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    []
  end
end
