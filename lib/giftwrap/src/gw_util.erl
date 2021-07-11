-module(gw_util).

-export([tmpdir/0, bytes_to_hex/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Get the temporary directory path.
-spec tmpdir() -> file:filename_all().
tmpdir() ->
  case os:type() of
    {unix, _} ->
      case os:getenv("TMPDIR") of
        false  -> "/tmp";
        TmpDir -> TmpDir
      end;
    {win32, _} ->
      case os:getenv("TMP") of
        false ->
          case os:getenv("TEMP") of
            false  -> os:getenv("windir") ++ "\\temp";
            TmpDir -> TmpDir
          end;
        TmpDir ->
          TmpDir
      end
  end.

%% Convert bytes in a binary to a hex-encoded binary.
-spec bytes_to_hex(binary()) -> binary().
bytes_to_hex(Bytes) ->
  <<<<(hex(X))>> || <<X:4>> <= Bytes>>.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

hex(X) when X >= 10 -> X + $a - 10;
hex(X)              -> X + $0.
