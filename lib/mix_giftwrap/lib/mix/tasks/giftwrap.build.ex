defmodule Mix.Tasks.Giftwrap.Build do
  use Mix.Task

  @shortdoc "Creates a Giftwrap binary"

  @switches [
    quiet: :boolean
  ]

  @aliases [
    q: :quiet
  ]

  @moduledoc """
  Creates a Giftwrap binary from an Escript and launcher.

      mix giftwrap.build

  A default giftwrap launcher is used unless specified.

  ## Options

  - `-q`, `--quiet` - Silences output to stdout

  """

  @doc false
  @impl true
  def run(args) do
    {opts, _} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)

    unless opts[:quiet] do
      Mix.shell().info("Building Giftwrap binary...")
    end

    config = Mix.Project.config()

    escript_path = Mix.Giftwrap.escript_path(config)
    path = Mix.Giftwrap.path(config)

    if Path.expand(escript_path, File.cwd!()) == Path.expand(path, File.cwd!()) do
      Mix.raise("Escript path `#{escript_path}` and output path `#{path}` are the same")
    end

    launcher_path = Application.app_dir(:mix_giftwrap, "priv/launchers/bin/launcher")

    case :giftwrap.wrap_escript(path, escript_path, launcher_path) do
      :ok ->
        unless opts[:quiet] do
          Mix.shell().info("Created binary `#{path}`")
        end

        :ok

      {:error, error} ->
        Mix.raise("Failed to create giftwrap binary: #{:giftwrap.format_error(error)}")
    end
  end
end
