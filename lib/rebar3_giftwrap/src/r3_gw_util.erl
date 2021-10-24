-module(r3_gw_util).

-export([is_windows/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec is_windows() -> boolean().
is_windows() ->
  case os:type() of
    {win32, _} -> true;
    _          -> false
  end.
