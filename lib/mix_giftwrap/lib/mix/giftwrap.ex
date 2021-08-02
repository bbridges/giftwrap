defmodule Mix.Giftwrap do
  @moduledoc false

  @compile_env Mix.env()

  alias Mix.Giftwrap.Util

  def escript_path(config) do
    case get_in(config, [:escript, :path]) do
      nil ->
        case get_in(config, [:escript, :name]) do
          nil -> config[:app]
          name -> name
        end

      path ->
        path
    end
    |> to_string()
  end

  def path(config) do
    case get_in(config, [:giftwrap, :path]) do
      nil ->
        rootname = Path.rootname(escript_path(config), ".escript")

        if Util.windows?(), do: rootname ++ ".exe", else: rootname

      path ->
        path
    end
    |> to_string()
  end

  def launcher(config) do
    case get_in(config, [:giftwrap, :launcher]) do
      nil ->
        default_launcher()

      launcher ->
        launcher
    end
  end

  defp default_launcher do
    case @compile_env do
      :prod ->
        Application.ensure_all_started(:giftwrap)
        version = Application.spec(:giftwrap, :vsn)

        {:crates_io, "giftwrap_launcher", version}

      _ ->
        path =
          Path.join(__ENV__.file, "../../../../../launcher/crates/giftwrap_launcher")
          |> Path.expand()

        {:path, path}
    end
  end
end
