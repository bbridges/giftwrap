defmodule Hello.MixProject do
  use Mix.Project

  def project do
    [
      app: :hello,
      version: "0.1.0",
      elixir: "~> 1.11",
      escript: escript(),
      deps: deps(),
      aliases: aliases()
    ]
  end

  defp escript do
    [
      main_module: Hello,
      path: "hello.escript"
    ]
  end

  defp deps do
    [
      {:mix_giftwrap, path: "../../lib/mix_giftwrap", env: :dev, only: :dev},
    ]
  end

  defp aliases do
    [
      "deps.get": ["deps.get", "giftwrap.get"],
      compile: ["compile", "escript.build", "giftwrap.build"]
    ]
  end
end
