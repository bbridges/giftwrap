-module(r3_gw_prv_build).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, build).
-define(NAMESPACE, giftwrap).
-define(DEPS, []).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
    {name, ?PROVIDER},
    {namespace, ?NAMESPACE},
    {module, ?MODULE},
    {bare, true},
    {deps, ?DEPS},
    {example, "rebar3 giftwrap build"},
    {opts, [
      {quiet, $q, "quiet", undefined, "Silences output to stdout"}
    ]},
    {short_desc, "Create a Giftwarp binary."},
    {desc, "Creates a Giftwrap binary from an Escript and launcher.\n\n"
           "A default giftwrap launcher is used unless specified.\n"}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  Quiet = proplists:get_bool(quiet, Args),

  case Quiet of
    false -> rebar_api:info("Building Giftwrap binary...", []);
    _     -> ok
  end,

  EscriptPath =
    case r3_gw_config:escript_path(State) of
      {ok, E}         -> E;
      {error, Error1} -> rebar_api:abort(Error1, [])
    end,
  Path =
    case r3_gw_config:path(State) of
      {ok, P}         -> P;
      {error, Error2} -> rebar_api:abort(Error2, [])
    end,

  case filename:absname(EscriptPath) == filename:absname(Path) of
    true -> rebar_api:abort("Escript path `~s` and output path `~s` are the same",
                            [EscriptPath, Path]);
    _    -> ok
  end,

  LauncherPath = filename:join(code:priv_dir(rebar3_giftwrap), "launchers/bin/launcher"),

  case giftwrap:wrap_escript(Path, EscriptPath, LauncherPath) of
    ok ->
      case Quiet of
        false -> rebar_api:info("Created binary `~s`", [Path]);
        _     -> ok
      end;
    {error, Error3} ->
      rebar_api:abort("Failed to create giftwrap binary: ~s", [giftwrap:format_error(Error3)])
  end,

  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
