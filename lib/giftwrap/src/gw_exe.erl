-module(gw_exe).

-export([create/4]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Create a giftwrap executable with launcher, content, and config.
-spec create(file:filename_all(), file:filename_all(), file:filename_all(), map()) ->
        ok | {error, term()}.
create(Filename, LauncherFilename, ContentFilename, Config) ->
  try
    File         = open_file(Filename,         [write, raw, binary]),
    LauncherFile = open_file(LauncherFilename, [read,  raw, binary]),
    ContentFile  = open_file(ContentFilename,  [read,  raw, binary]),

    Exe1               = init(File, LauncherFile, ContentFile, Config),
    Exe2               = add_launcher(Exe1),
    {Exe3, ContentLen} = add_content(Exe2),
    {Exe4, ConfigLen}  = add_config(Exe3),
    {Exe5, TagLen}     = add_tag(Exe4),
    _                  = add_trailer(Exe5, ContentLen, ConfigLen, TagLen),

    close_file(File),
    close_file(LauncherFile),
    close_file(ContentFile),

    ok
  catch
    throw:{exe_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

-define(READ_SIZE, 65536).

-record(exe, {file, launcher_file, content_file, config, hash_state = crypto:hash_init(sha)}).

init(File, LauncherFile, ContentFile, Config) ->
  #exe{file = File, launcher_file = LauncherFile, content_file = ContentFile, config = Config}.

add_launcher(Exe) ->
  case read_file(Exe#exe.launcher_file, ?READ_SIZE) of
    eof ->
      Exe;
    Bytes ->
      write_file(Exe#exe.file, Bytes),
      add_launcher(update_hash_state(Exe, Bytes))
  end.

add_content(Exe) ->
  add_content(Exe, 0).

add_content(Exe, Acc) ->
  case read_file(Exe#exe.content_file, ?READ_SIZE) of
    eof ->
      {Exe, Acc};
    Bytes ->
      write_file(Exe#exe.file, Bytes),
      add_content(update_hash_state(Exe, Bytes), Acc + byte_size(Bytes))
  end.

add_config(Exe) ->
  Config = jsx:encode(Exe#exe.config),

  write_file(Exe#exe.file, Config),
  {update_hash_state(Exe, Config), byte_size(Config)}.

add_tag(Exe) ->
  Tag = gw_util:bytes_to_hex(compute_hash(Exe)),

  write_file(Exe#exe.file, Tag),
  {Exe, byte_size(Tag)}.

add_trailer(Exe, ContentLen, ConfigLen, TagLen) ->
  Trailer = <<ContentLen:64, ConfigLen:16, TagLen:8, 1:8, "GIFT">>,

  write_file(Exe#exe.file, Trailer),
  Exe.

update_hash_state(Exe, Bytes) ->
  Exe#exe{hash_state = crypto:hash_update(Exe#exe.hash_state, Bytes)}.

compute_hash(Exe) ->
  crypto:hash_final(Exe#exe.hash_state).

open_file(Filename, Options) ->
  case file:open(Filename, Options) of
    {ok, File}      -> File;
    {error, Reason} -> throw({exe_error, {open_error, Reason}})
  end.

close_file(File) ->
  case file:close(File) of
    ok              -> ok;
    {error, Reason} -> throw({exe_error, {close_error, Reason}})
  end. 

read_file(File, Length) ->
  case file:read(File, Length) of
    eof             -> eof;
    {ok, Bytes}     -> Bytes;
    {error, Reason} -> throw({exe_error, {read_error, Reason}})
  end.

write_file(File, Bytes) ->
  case file:write(File, Bytes) of
    ok              -> ok;
    {error, Reason} -> throw({exe_error, {write_error, Reason}})
  end.
