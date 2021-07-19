defmodule Giftwrap.MixProject do
  use Mix.Project

  def project do
    [
      app: :giftwrap,
      version: "0.1.0",
      language: :erlang,
      description: "Create standalone binaries from escripts",
      deps: deps(),
      package: package(),
      start_permanent: Mix.env() == :prod,
    ]
  end

  def application do
    []
  end

  defp deps do
    [
      {:jsx, "~> 3.0"}
    ]
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/bbridges/giftwrap"}
    ]
  end
end
