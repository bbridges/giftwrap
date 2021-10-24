-module(r3_gw_prv_get).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, get).
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
    {example, "rebar3 giftwrap get"},
    {opts, [
      {quiet, $q, "quiet", undefined, "Silences output to stdout"}
    ]},
    {short_desc, "Fetches and builds a Giftwrap launcher."},
    {desc, "Fetches and builds a Giftwrap launcher.\n\n"
           "A default giftwrap launcher is used unless specified. Fetching is only supported\n"
           "for Rust-based launchers. `cargo` is required to be on the `PATH` for this task\n"
           "to run.\n"}
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

  LauncherRoot = filename:join(code:priv_dir(rebar3_giftwrap), "launchers"),

  LauncherVersion =
    case r3_gw_config:launcher(State) of
      {ok, {crates_io, Crate, Version}} ->
        io_lib:format("\"~s\" --version \"~s\"", [Crate, Version]);
      {ok, {path, Path}} ->
        io_lib:format("--path \"~s\"", [normalize_path(Path)]);
      {error, Error1} ->
        rebar_api:abort(Error1, [])
    end,

  % Forcing in case we're replacing another launcher.
  InstallCmd = io_lib:format("cargo install -f ~s --bin launcher --root \"~s\"",
                             [LauncherVersion, LauncherRoot]),

  % Adding the destination bin folder to the path temporarily so cargo doesn't
  % show a warning saying to put it on the path.
  InstallOpts = [
    {env, [{"PATH", lists:flatten([os:getenv("PATH", ""),
                                   path_sep(),
                                   filename:join(LauncherRoot, "bin")])}]},
    {use_stdout, not Quiet}
  ],

  case Quiet of
    false -> rebar_api:info("Getting Giftwrap launcher...", []),
             rebar_api:console("~s ~s", [shell_char(), InstallCmd]);
    _     -> ok
  end,

  case rebar_utils:sh(InstallCmd, InstallOpts) of
    {ok, _}    -> ok;
    {error, _} -> rebar_api:abort("Failed to run `cargo install`", [])
  end,

  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Normalize a path to resolve symlinks, ".", and ".." away.
normalize_path(Path) ->
  % HACK: Make the path absolute, use safe_relative_path to do the normalization
  %       with respect to a relative path to the root directory, and then going
  %       back to an absolute path. It works well enough.
  case filename:split(filename:absname(Path)) of
    [Root] ->
      Root;
    [Root | PathSegments] ->
      filename:join(Root, filelib:safe_relative_path(filename:join(PathSegments), Root))
  end.

shell_char() ->
  case r3_gw_util:is_windows() of
    true -> ">";
    _    -> "$"
  end. 

path_sep() ->
  case r3_gw_util:is_windows() of
    true -> ";";
    _    -> ":"
  end.
