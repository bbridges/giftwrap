defmodule Mix.Tasks.Giftwrap.Get do
  use Mix.Task

  alias Mix.Giftwrap.Util

  @shortdoc "Fetches and builds a Giftwrap launcher"

  @switches [
    quiet: :boolean
  ]

  @aliases [
    q: :quiet
  ]

  @moduledoc """
  Fetches and builds a Giftwrap launcher.

      mix giftwrap.get

  A default giftwrap launcher is used unless specified. Fetching is only
  supported for Rust-based launchers. `cargo` is required to be on the `PATH`
  for this task to run.

  ## Options

  - `-q`, `--quiet` - Silences output to stdout

  """

  @doc false
  @impl true
  def run(args) do
    {opts, _} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)

    config = Mix.Project.config()

    launcher_root = Application.app_dir(:mix_giftwrap, "priv/launchers")

    launcher_version =
      case Mix.Giftwrap.launcher(config) do
        {:crates_io, crate, version} ->
          "\"#{crate}\" --version \"#{version}\""

        {:path, path} ->
          "--path \"#{path}\""
      end

    # Forcing in case we're replacing another launcher.
    install_cmd =
      "cargo install -f #{launcher_version} " <>
        "--bin launcher --root \"#{launcher_root}\""

    # Adding the destination bin folder to the path temporarily so cargo doesn't
    # show a warning saying to put it on the path.
    install_opts = [
      env: [{"PATH", System.get_env("PATH", "") <> path_sep() <> Path.join(launcher_root, "bin")}],
      quiet: opts[:quiet] not in [false, nil]
    ]

    unless opts[:quiet] do
      Mix.shell().info("Getting Giftwrap launcher...")
      Mix.shell().info("#{shell_char()} #{install_cmd}")
    end

    if Mix.shell().cmd(install_cmd, install_opts) != 0 do
      Mix.raise("Failed to run `cargo install`")
    end

    :ok
  end

  defp shell_char do
    if Util.windows?(), do: ">", else: "$"
  end

  defp path_sep do
    if Util.windows?(), do: ";", else: ":"
  end
end
