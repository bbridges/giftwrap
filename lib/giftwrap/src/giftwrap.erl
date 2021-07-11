-module(giftwrap).

-export([create_exe/3]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec create_exe(file:filename_all(), file:filename_all(), file:filename_all()) ->
        ok | {error, term()}.
create_exe(Filename, EscriptFilename, LauncherFilename) ->
  ContentName = binary_to_list(gw_util:bytes_to_hex(crypto:strong_rand_bytes(16))) ++ ".content",
  ContentFilename = filename:join(gw_util:tmpdir(), ContentName),

  try
    case gw_content:create(ContentFilename, EscriptFilename, []) of
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
