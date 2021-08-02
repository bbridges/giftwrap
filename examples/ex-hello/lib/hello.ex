defmodule Hello do
  # The first argument is the path to the giftwrapped binary!
  def main([_]),       do: greet()
  def main([_, name]), do: greet(name)
  def main(_),         do: usage()

  defp greet(name \\ "world"), do: IO.puts("Hello, #{name}!")

  defp usage do
    IO.puts(:stderr, "usage: hello [name]")
    :erlang.halt(1)
  end
end
