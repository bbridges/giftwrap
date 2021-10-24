-module(r3_gw_config).

-export([escript_path/1, path/1, launcher/1]).

-ifndef(DEV_LAUNCHER).
-ifdef(LAUNCHER_VERSION).
-define(DEFAULT_LAUNCHER, {crates_io, "giftwrap_launcher", ?LAUNCHER_VERSION}).
-endif.
-endif.

-ifndef(DEFAULT_LAUNCHER).
-define(DEFAULT_LAUNCHER, {path, filename:absname_join(
                                   ?FILE,
                                   "../../../../launcher/crates/giftwrap_launcher"
                                 )}).
-endif.

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec escript_path(rebar_state:t()) -> {ok, file:filename_all()} | {error, string()}. 
escript_path(State) ->
  EscriptName =
    case rebar_state:get(State, escript_name, undefined) of
      undefined ->
        case rebar_state:get(State, escript_main_app, undefined) of
          undefined ->
            case rebar_state:project_apps(State) of
              [AppInfo] -> rebar_app_info:name(AppInfo);
              _ ->         {error, "No main app found, escript_main_app must be configured"}
            end;
          AppName ->
            atom_to_binary(AppName)
        end;
      Name ->
        Name
    end,
  case EscriptName of
    {error, _} = Error -> Error;
    _ ->                  {ok, filename:join([rebar_dir:base_dir(State), "bin", EscriptName])}
  end.

-spec path(rebar_state:t()) -> {ok, file:filename_all()} | {error, string()}.
path(State) ->
  case giftwrap_opt(path, State, undefined) of
    undefined ->
      case escript_path(State) of
        {ok, EscriptPath} ->
          Rootname = filename:rootname(EscriptPath, ".escript"),

          {ok, case r3_gw_util:is_windows() of
                 true -> Rootname ++ ".exe";
                 _    -> Rootname
               end};
        {error, _} = Error ->
          Error
      end;
    Path ->
      {ok, Path}
  end.

-spec launcher(rebar_state:t()) -> {ok, file:filename_all()} | {error, string()}.
launcher(State) ->
  {ok, giftwrap_opt(launcher, State, ?DEFAULT_LAUNCHER)}.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

giftwrap_opt(Name, State, Default) ->
  case rebar_state:get(State, giftwrap, undefined) of
    undefined -> Default;
    Opts      -> proplists:get_value(Name, Opts, Default)
  end.
