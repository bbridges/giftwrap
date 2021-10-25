-module(hello).
-export([main/1]).

%% The first argument is the path to the giftwrapped binary!
main([_])       -> greet("world");
main([_, Name]) -> greet(Name);
main(_)         -> usage().

greet(Name) ->
  io:fwrite("Hello, ~s!~n", [Name]).

usage() ->
  io:fwrite(standard_error, "usage: hello [name]~n", []),
  halt(1).
