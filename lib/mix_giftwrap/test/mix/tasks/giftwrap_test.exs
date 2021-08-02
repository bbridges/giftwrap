defmodule Mix.Tasks.GiftwrapTest do
  use ExUnit.Case

  test "show giftwrap information" do
    Mix.Tasks.Giftwrap.run([])

    assert_receive {:mix_shell, :info, ["giftwrap v" <> _]}
    assert_receive {:mix_shell, :info, ["## Mix Tasks" <> _]}
  end

  test "show version information only" do
    Mix.Tasks.Giftwrap.run(["--version"])

    assert_receive {:mix_shell, :info, ["giftwrap v" <> _]}
  end
end
