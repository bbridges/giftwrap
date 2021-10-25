defmodule Mix.Giftwrap do
  @moduledoc false

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
        Application.fetch_env!(:mix_giftwrap, :launcher)

      launcher ->
        launcher
    end
  end
end
