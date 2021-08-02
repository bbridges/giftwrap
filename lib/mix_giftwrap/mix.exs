defmodule MixGiftwrap.MixProject do
  use Mix.Project

  def project do
    [
      app: :mix_giftwrap,
      version: "0.1.0",
      elixir: "~> 1.11",
      description: "Mix tasks for creating giftwrap binaries",
      deps: deps(),
      package: package(),
      start_permanent: Mix.env() == :prod
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:giftwrap, "~> 0.1.0", in_umbrella: Mix.env() != :prod, env: Mix.env()}
    ]
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/bbridges/giftwrap"}
    ]
  end
end
