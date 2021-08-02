defmodule Mix.Tasks.Giftwrap do
  use Mix.Task

  @shortdoc "Prints Giftwrap help information"

  @switches [
    version: :boolean
  ]

  @aliases [
    v: :version
  ]

  @moduledoc """
  Prints Giftwrap help information.

      mix giftwrap

  ## Options

  - `-v`, `--version` - Prints giftwrap version

  """

  @doc false
  @impl true
  def run(args) do
    {opts, _} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)

    Application.ensure_all_started(:giftwrap)
    Application.ensure_all_started(:mix_giftwrap)

    version = Application.spec(:giftwrap, :vsn)
    task_version = Application.spec(:mix_giftwrap, :vsn)

    Mix.shell().info("giftwrap v#{version} with mix_giftwrap v#{task_version}")

    unless opts[:version] do
      Mix.shell().info("\nCreate standalone binaries from escripts\n")
      Mix.shell().info("## Options\n")
      Mix.shell().info("-v, --version        # Prints Giftwrap version\n")
      Mix.shell().info("## Mix Tasks\n")

      Mix.Tasks.Help.run(["--search", "giftwrap."])
    end
  end
end
