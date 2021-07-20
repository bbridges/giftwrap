-module(gw_content).

-export([create/3]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec create(file:filename_all(), file:filename_all(), list()) ->
        {ok, map()} | {error, term()}.
create(ContentFilename, EscriptFilename, Options) ->
  try
    TarDesc = open_tar(ContentFilename, [write]),

    ErlFolder = proplists:get_value(erl_folder, Options, code:root_dir()),

    LibFolder = lib_folder(ErlFolder),
    ErtsFolder = erts_folder(ErlFolder),

    add_tar(TarDesc, {filename:basename(LibFolder), LibFolder}),
    add_tar(TarDesc, {filename:basename(ErtsFolder), ErtsFolder}),

    EntryPoint = filename:join("bin", "entrypoint"),
    WrappedEscript = EntryPoint ++ ".escript",

    add_tar(TarDesc, {EntryPoint, filename:join([ErtsFolder, "bin", "escript"])}),
    add_tar(TarDesc, {WrappedEscript, filename_to_string(EscriptFilename)}),

    close_tar(TarDesc),

    Config = #{
      <<"app">> => maybe_atom_to_binary(proplists:get_value(app, Options, null)),
      <<"version">> => maybe_iolist_to_binary(proplists:get_value(version, Options, null)),
      <<"compression">> => null,
      <<"entry_point">> => iolist_to_binary(EntryPoint)
    },
    {ok, Config}
  catch
    throw:{tar_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

lib_folder(ErlFolder) ->
  filename:join(ErlFolder, "lib").

erts_folder(ErlFolder) ->
  case filelib:wildcard(filename:join(ErlFolder, "erts-*")) of
    []           -> throw({tar_error, no_erts_folder});
    [ErtsFolder] -> ErtsFolder;
    _            -> throw({tar_error, ambig_erts_folder})
  end.

filename_to_string(Filename) when is_list(Filename) ->
  Filename;
filename_to_string(Filename) when is_binary(Filename) ->
  binary_to_list(Filename).

open_tar(Filename, Options) ->
  case erl_tar:open(Filename, Options) of
    {ok, TarDesc}   -> TarDesc;
    {error, Reason} -> throw({tar_error, {open_tar_error, Reason}})
  end.

close_tar(TarDesc) ->
  case erl_tar:close(TarDesc) of
    ok              -> ok;
    {error, Reason} -> throw({tar_error, {close_tar_error, Reason}})
  end.

add_tar(TarDesc, {Name, Path}) ->
  case erl_tar:add(TarDesc, Path, Name, []) of
    ok              -> ok;
    {error, Reason} -> throw({tar_error, {add_tar_error, Reason}})
  end.

maybe_iolist_to_binary(null) ->
  null;
maybe_iolist_to_binary(IOList) ->
  iolist_to_binary(IOList).

maybe_atom_to_binary(null) ->
  null;
maybe_atom_to_binary(Atom) ->
  atom_to_binary(Atom).
