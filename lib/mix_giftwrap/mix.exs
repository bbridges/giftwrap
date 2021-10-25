defmodule MixGiftwrap.MixProject do
  use Mix.Project

  @dev_launcher Path.join(__ENV__.file, "../../../launcher/crates/giftwrap_launcher")
                |> Path.expand()

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
      extra_applications: [:logger],
      env: [
        launcher: launcher(Mix.env())
      ]
    ]
  end

  defp launcher(:prod), do: {:crates_io, "giftwrap_launcher", "0.1.0"}
  defp launcher(_), do: {:path, @dev_launcher}

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
