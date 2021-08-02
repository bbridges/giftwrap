defmodule Mix.Giftwrap.Util do
  @moduledoc false

  def windows?(), do: match?({:win32, _}, :os.type())
end
