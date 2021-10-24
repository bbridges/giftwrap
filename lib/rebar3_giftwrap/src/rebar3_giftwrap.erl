-module(rebar3_giftwrap).

-export([init/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, State1} = r3_gw_prv_get:init(State),
  {ok, State2} = r3_gw_prv_build:init(State1),
  {ok, State2}.
