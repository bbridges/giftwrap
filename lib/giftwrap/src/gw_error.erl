-module(gw_error).

-export([format/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Give a human-readable error message.
-spec format(term()) -> string().

%% input errors.
format(invalid_escript) ->
  "escript path does not exist or is invalid";
format(invalid_launcher) ->
  "launcher path does not exist or is invalid";
format(no_erts_folder) ->
  "could not find ERTS folder in Erlang directory";
format(ambig_erts_folder) ->
  "found multiple ERTS folders in Erlang directory";

%% file/tar errors.
format({ensure_dir_error, Reason}) ->
  flat_format("error creating parent dirs: ~p", [file:format_error(Reason)]);
format({open_error, Reason}) ->
  flat_format("error opening file: ~p", [file:format_error(Reason)]);
format({close_error, Reason}) ->
  flat_format("error closing file: ~p", [file:format_error(Reason)]);
format({read_error, Reason}) ->
  flat_format("error reading file: ~p", [file:format_error(Reason)]);
format({write_error, Reason}) ->
  flat_format("error writing file: ~p", [file:format_error(Reason)]);
format({chmod_x_error, Reason}) ->
  flat_format("error making file executable: ~p", [file:format_error(Reason)]);
format({open_tar_error, Reason}) ->
  flat_format("error opening tar file: ~p", [erl_tar:format_error(Reason)]);
format({close_tar_error, Reason}) ->
  flat_format("error closing tar file: ~p", [erl_tar:format_error(Reason)]);
format({add_tar_error, Reason}) ->
  flat_format("error adding to tar file: ~p", [erl_tar:format_error(Reason)]);

%% anything else...
format(Reason) -> 
  flat_format("unknown error: ~p", [Reason]).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

flat_format(Format, Args) ->
  lists:flatten(io_lib:format(Format, Args)).
