%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Shared low-level port utilities for subprocess gen_servers (BT-1187).
%%%
%%% **DDD Context:** Actor System Context
%%%
%%% Provides line-splitting, binary validation, and exec port wrappers shared
%%% by `beamtalk_subprocess` (pull) and `beamtalk_reactive_subprocess` (push).
%%% Keeping shared logic here avoids coupling the two state machines to each
%%% other while keeping both DRY.
%%%
%%% == Exports ==
%%%
%%% * `split_lines/1` — split binary on newlines, return {Lines, Remainder}
%%% * `flush_and_collect/2` — prepend pending fragment, split on newlines
%%% * `bt_array_to_list/3` — coerce Beamtalk Array to Erlang list
%%% * `ensure_binary_args/3` — validate that all args are binaries
%%% * `ensure_binary_env/3` — validate that all env keys/values are binaries
%%% * `open/0` — open an exec port
%%% * `spawn_child/4,5` — spawn a subprocess through the port
%%% * `kill_child/2` — send kill command to a subprocess
%%% * `write_stdin/3` — write data to subprocess stdin
%%% * `close/1` — close the exec port

-module(beamtalk_subprocess_port).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    split_lines/1,
    flush_and_collect/2,
    bt_array_to_list/3,
    ensure_binary_args/3,
    ensure_binary_env/3,
    open/0,
    spawn_child/4,
    spawn_child/5,
    kill_child/2,
    write_stdin/3,
    close/1
]).

%%% ============================================================================
%%% Line splitting
%%% ============================================================================

%% @doc Split binary data on newlines.
%%
%% Returns {CompleteLines, PartialRemainder} where CompleteLines is the list
%% of lines without trailing newlines, and PartialRemainder is the data after
%% the last newline (may be <<>> if data ends with a newline).
-spec split_lines(binary()) -> {[binary()], binary()}.
split_lines(Data) ->
    case binary:split(Data, <<"\n">>, [global]) of
        [Remainder] ->
            %% No newlines — entire data is a partial line
            {[], Remainder};
        Parts ->
            %% Last element is the remainder after the last newline (may be <<>>)
            {Lines, [Remainder]} = lists:split(length(Parts) - 1, Parts),
            {Lines, Remainder}
    end.

%% @doc Prepend Pending to Data then split on newlines.
%%
%% Combines the buffered partial fragment with newly arrived data before
%% splitting. Returns {CompleteLines, NewPending}.
-spec flush_and_collect(binary(), binary()) -> {[binary()], binary()}.
flush_and_collect(Pending, Data) ->
    Combined = <<Pending/binary, Data/binary>>,
    split_lines(Combined).

%%% ============================================================================
%%% Input validators
%%% ============================================================================

%% @doc Coerce a Beamtalk Array (tagged map or plain list) to an Erlang list.
%%
%% Array literals in Beamtalk method call arguments compile to plain Erlang
%% lists; Array values returned from collection operations are tagged maps.
%% Raises `type_error` via `beamtalk_error` on invalid input.
-spec bt_array_to_list(atom(), term(), atom()) -> list().
bt_array_to_list(Class, #{'$beamtalk_class' := 'Array', 'data' := Arr}, Selector) ->
    case array:is_array(Arr) of
        true ->
            array:to_list(Arr);
        false ->
            Err = beamtalk_error:new(type_error, Class, Selector),
            beamtalk_error:raise(Err)
    end;
bt_array_to_list(_Class, List, _Selector) when is_list(List) ->
    List;
bt_array_to_list(Class, _Other, Selector) ->
    Err = beamtalk_error:new(type_error, Class, Selector),
    beamtalk_error:raise(Err).

%% @doc Raise type_error if any element of Args is not a binary.
-spec ensure_binary_args(atom(), list(), atom()) -> ok.
ensure_binary_args(Class, Args, Selector) when is_list(Args) ->
    case lists:all(fun erlang:is_binary/1, Args) of
        true ->
            ok;
        false ->
            Err = beamtalk_error:new(type_error, Class, Selector),
            beamtalk_error:raise(Err)
    end.

%% @doc Raise type_error if any key or value in Env is not a binary.
-spec ensure_binary_env(atom(), map(), atom()) -> ok.
ensure_binary_env(Class, Env, Selector) when is_map(Env) ->
    case lists:all(fun({K, V}) -> is_binary(K) andalso is_binary(V) end, maps:to_list(Env)) of
        true ->
            ok;
        false ->
            Err = beamtalk_error:new(type_error, Class, Selector),
            beamtalk_error:raise(Err)
    end.

%%% ============================================================================
%%% Exec port wrappers
%%% ============================================================================

%% @doc Open a port to the exec binary, finding it automatically.
-spec open() -> port().
open() ->
    beamtalk_exec_port:open().

%% @doc Spawn a subprocess through the exec port (no custom env or dir).
-spec spawn_child(port(), non_neg_integer(), binary(), [binary()]) -> true.
spawn_child(Port, ChildId, Executable, Args) ->
    beamtalk_exec_port:spawn_child(Port, ChildId, Executable, Args).

%% @doc Spawn a subprocess through the exec port with optional env/dir.
-spec spawn_child(port(), non_neg_integer(), binary(), [binary()], map()) -> true.
spawn_child(Port, ChildId, Executable, Args, Options) ->
    beamtalk_exec_port:spawn_child(Port, ChildId, Executable, Args, Options).

%% @doc Send a graceful kill to a subprocess.
-spec kill_child(port(), non_neg_integer()) -> true.
kill_child(Port, ChildId) ->
    beamtalk_exec_port:kill_child(Port, ChildId).

%% @doc Write data to a subprocess's stdin.
-spec write_stdin(port(), non_neg_integer(), binary()) -> true.
write_stdin(Port, ChildId, Data) ->
    beamtalk_exec_port:write_stdin(Port, ChildId, Data).

%% @doc Close the exec port (terminates all managed subprocesses).
-spec close(port()) -> true.
close(Port) ->
    beamtalk_exec_port:close(Port).
