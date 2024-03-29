-module(giftwrap).

-export([wrap_escript/3, wrap_escript/4, format_error/1]).
-export_type([escript_opt/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-type escript_opt() :: {app, atom()}
                     | {version, string()}
                     | {erl_folder, file:filename_all()}.

-spec wrap_escript(file:filename_all(), file:filename_all(), file:filename_all()) ->
        ok | {error, term()}.
wrap_escript(Filename, EscriptFilename, LauncherFilename) ->
  wrap_escript(Filename, EscriptFilename, LauncherFilename, []).

-spec wrap_escript(file:filename_all(), file:filename_all(), file:filename_all(),
                   [escript_opt()]) ->
        ok | {error, term()}.
wrap_escript(Filename, EscriptFilename, LauncherFilename, Options) ->
  case do_setup(Filename, EscriptFilename, LauncherFilename) of
    ok              -> do_create(Filename, EscriptFilename, LauncherFilename, Options);
    {error, Reason} -> {error, Reason}
  end.

-spec format_error(term()) -> string().
format_error(Reason) ->
  gw_error:format(Reason).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

do_setup(Filename, EscriptFilename, LauncherFilename) ->
  case filelib:is_regular(EscriptFilename) of
    true ->
      case filelib:is_regular(LauncherFilename) of
        true ->
          case filelib:ensure_dir(Filename) of
            ok ->
              ok;
            {error, Reason} ->
              {error, {ensure_dir_error, Reason}}
          end;
        false ->
          {error, invalid_launcher}
      end;
    false ->
      {error, invalid_escript}
  end.

do_create(Filename, EscriptFilename, LauncherFilename, Options) ->
  ContentName = binary_to_list(gw_util:bytes_to_hex(crypto:strong_rand_bytes(16))) ++ ".content",
  ContentFilename = filename:join(gw_util:tmpdir(), ContentName),

  try
    case gw_content:create(ContentFilename, EscriptFilename, Options) of
      {ok, Config} ->
        case gw_exe:create(Filename, LauncherFilename, ContentFilename, Config) of
          ok ->
            ok;
          {error, Reason} ->
            {error, Reason}
        end;
      {error, Reason} ->
        {error, Reason}
    end
  after
    file:delete(ContentFilename)
  end.
