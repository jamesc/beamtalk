%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_subprocess_port).

%%% **DDD Context:** Actor System Context

-moduledoc """
Shared low-level port utilities for subprocess gen_servers.

Provides line-splitting, binary validation, and exec port wrappers shared
by `beamtalk_subprocess` (pull) and `beamtalk_reactive_subprocess` (push).
Keeping shared logic here avoids coupling the two state machines to each
other while keeping both DRY.

== Exports ==

* `split_lines/1` — split binary on newlines, return {Lines, Remainder}
* `flush_and_collect/2` — prepend pending fragment, split on newlines
* `bt_array_to_list/3` — coerce Beamtalk Array to Erlang list
* `ensure_binary_args/3` — validate that all args are binaries
* `ensure_binary_env/3` — validate that all env keys/values are binaries
* `open/0` — open an exec port
* `spawn_child/4,5` — spawn a subprocess through the port
* `kill_child/2` — send kill command to a subprocess
* `write_stdin/3` — write data to subprocess stdin
* `submit_write/6`, `await_write/2` — park a `writeLine:` caller
* `write_queued/2`, `write_written/1`, `write_failed/3`, `write_dropped/3`,
  `fail_pending_writes/2` — answer parked callers from the port's events
* `close/1` — close the exec port
""".

-include_lib("beamtalk_runtime/include/beamtalk_sentinel_tags.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    flush_and_collect/2,
    bt_array_to_list/3,
    normalize_args/3,
    normalize_env/3,
    open/0,
    spawn_child/4, spawn_child/5,
    kill_child/2,
    write_stdin/3,
    close/1,
    submit_write/6,
    await_write/2,
    write_queued/2,
    write_written/1,
    write_failed/3,
    write_dropped/3,
    stdin_dropped_error/2,
    fail_pending_writes/2
]).

%% Exported for white-box tests only (no production caller).
-ifdef(TEST).
-export([
    ensure_binary_args/3,
    ensure_binary_env/3,
    split_lines/1
]).
-endif.

%%% ============================================================================
%%% Line splitting
%%% ============================================================================

-doc """
Split binary data on newlines.

Returns {CompleteLines, PartialRemainder} where CompleteLines is the list
of lines without trailing newlines, and PartialRemainder is the data after
the last newline (may be <<>> if data ends with a newline).
""".
-spec split_lines(binary()) -> {[binary()], binary()}.
split_lines(Data) ->
    case binary:split(Data, <<"\n">>, [global]) of
        [Remainder] ->
            %% No newlines — entire data is a partial line
            {[], Remainder};
        Parts ->
            %% Last element is the remainder after the last newline (may be <<>>)
            {Lines, [Remainder]} = lists:split(length(Parts) - 1, Parts),
            {[strip_cr(L) || L <- Lines], Remainder}
    end.

-doc "Strip trailing \\r from a line (normalises Windows \\r\\n → \\n).".
-spec strip_cr(binary()) -> binary().
strip_cr(<<>>) ->
    <<>>;
strip_cr(Bin) ->
    case binary:last(Bin) of
        $\r -> binary:part(Bin, 0, byte_size(Bin) - 1);
        _ -> Bin
    end.

-doc """
Prepend Pending to Data then split on newlines.

Combines the buffered partial fragment with newly arrived data before
splitting. Returns {CompleteLines, NewPending}.
""".
-spec flush_and_collect(binary(), binary()) -> {[binary()], binary()}.
flush_and_collect(Pending, Data) ->
    Combined = <<Pending/binary, Data/binary>>,
    split_lines(Combined).

%%% ============================================================================
%%% Input validators
%%% ============================================================================

-doc """
Coerce a Beamtalk Array (tagged map or plain list) to an Erlang list.

Array literals in Beamtalk method call arguments compile to plain Erlang
lists; Array values returned from collection operations are tagged maps.
Raises `type_error` via `beamtalk_error` on invalid input.
""".
-spec bt_array_to_list(atom(), term(), atom()) -> list().
bt_array_to_list(_Class, #{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data} = Arr, _Selector) when
    is_map(Data)
->
    beamtalk_tagged_map:array_to_list(Arr);
bt_array_to_list(Class, #{?BT_CLASS_TAG_KEY := 'Array'}, Selector) ->
    beamtalk_error:raise_type_error(Class, Selector, undefined);
bt_array_to_list(_Class, List, _Selector) when is_list(List) ->
    List;
bt_array_to_list(Class, _Other, Selector) ->
    beamtalk_error:raise_type_error(Class, Selector, undefined).

-doc "Raise type_error if any element of Args is not text (binary or String).".
-spec ensure_binary_args(atom(), list(), atom()) -> ok.
ensure_binary_args(Class, Args, Selector) when is_list(Args) ->
    case lists:all(fun is_text/1, Args) of
        true ->
            ok;
        false ->
            beamtalk_error:raise_type_error(Class, Selector, undefined)
    end.

-doc "Raise type_error if any key or value in Env is not text (binary or String).".
-spec ensure_binary_env(atom(), map(), atom()) -> ok.
ensure_binary_env(Class, Env, Selector) when is_map(Env) ->
    case lists:all(fun({K, V}) -> is_text(K) andalso is_text(V) end, maps:to_list(Env)) of
        true ->
            ok;
        false ->
            beamtalk_error:raise_type_error(Class, Selector, undefined)
    end.

-doc "Validate text args and unwrap each to its bytes for the exec port.".
-spec normalize_args(atom(), list(), atom()) -> [binary()].
normalize_args(Class, Args, Selector) when is_list(Args) ->
    ok = ensure_binary_args(Class, Args, Selector),
    [beamtalk_text:unwrap_or_raise(A, Class, Selector, undefined) || A <- Args].

-doc "Validate a text env map and unwrap keys/values to bytes.".
-spec normalize_env(atom(), map(), atom()) -> #{binary() => binary()}.
normalize_env(Class, Env, Selector) when is_map(Env) ->
    ok = ensure_binary_env(Class, Env, Selector),
    maps:from_list([
        {
            beamtalk_text:unwrap_or_raise(K, Class, Selector, undefined),
            beamtalk_text:unwrap_or_raise(V, Class, Selector, undefined)
        }
     || {K, V} <- maps:to_list(Env)
    ]).

is_text(Value) -> beamtalk_text:unwrap(Value) =/= error.

%%% ============================================================================
%%% Exec port wrappers
%%% ============================================================================

-doc "Open a port to the exec binary, finding it automatically.".
-spec open() -> port().
open() ->
    beamtalk_exec_port:open().

-doc "Spawn a subprocess through the exec port (no custom env or dir).".
-spec spawn_child(port(), non_neg_integer(), binary(), [binary()]) -> true.
spawn_child(Port, ChildId, Executable, Args) ->
    beamtalk_exec_port:spawn_child(Port, ChildId, Executable, Args).

-doc "Spawn a subprocess through the exec port with optional env/dir.".
-spec spawn_child(port(), non_neg_integer(), binary(), [binary()], map()) -> true.
spawn_child(Port, ChildId, Executable, Args, Options) ->
    beamtalk_exec_port:spawn_child(Port, ChildId, Executable, Args, Options).

-doc "Send a graceful kill to a subprocess.".
-spec kill_child(port(), non_neg_integer()) -> true.
kill_child(Port, ChildId) ->
    beamtalk_exec_port:kill_child(Port, ChildId).

-doc "Write data to a subprocess's stdin.".
-spec write_stdin(port(), non_neg_integer(), binary()) -> true.
write_stdin(Port, ChildId, Data) ->
    beamtalk_exec_port:write_stdin(Port, ChildId, Data).

-doc """
Send `Line` to the child's stdin and park `From` until the port answers
(BT-5259).

`port_command` raises `badarg` when the exec port has already died. That must
not crash the owning gen_server: the caller gets a structured `port_closed`
error (with the raise reason in its details) and parked writers are failed.
The result is `{port_dead, Err, State}`: the owner then tears down its readers
and marks the port closed, exactly as for an unexpected `exit_status`.
""".
-spec submit_write(atom(), port(), non_neg_integer(), binary(), term(), map()) ->
    {noreply, map()} | {port_dead, #beamtalk_error{}, map()}.
submit_write(Class, Port, ChildId, Line, From, State) ->
    try write_stdin(Port, ChildId, Line) of
        true -> {noreply, await_write(From, State)}
    catch
        error:Reason ->
            Err = beamtalk_error:with_details(
                beamtalk_error:new(port_closed, Class, 'writeLine:'),
                #{reason => Reason}
            ),
            %% The caller finishes the teardown (readers, EOF, port_closed),
            %% as its exit_status handler does.
            {port_dead, Err, fail_pending_writes(Class, State)}
    end.

-doc """
Park `From`, a `writeLine:` caller, until the exec port says whether its line
reached the child's stdin pipe (BT-5248, BT-5269).

A write is answered in two steps, each in order. The command thread answers
every `write_stdin` once: `stdin_queued` when it was accepted (the caller
moves to `stdin_unwritten`), `error` when it was dropped (the caller raises).
The writer thread then answers each queued write once: `stdin_written` when
the line is in the pipe (the caller gets `nil`), or `stdin_failed` when the
child exited or closed its stdin first (that caller and every one queued
behind it raise). So no caller is told `nil` for a line that was then lost
before reaching the pipe.
""".
-spec await_write(term(), map()) -> map().
await_write(From, State) ->
    State#{stdin_waiters => queue:in(From, stdin_waiters(State))}.

-doc """
The oldest parked write was queued by the exec port: its caller waits on for
the writer thread's report. If the writer thread has already failed, the line
will never be written, so the caller raises now.
""".
-spec write_queued(atom(), map()) -> map().
write_queued(Class, State) ->
    case queue:out(stdin_waiters(State)) of
        {{value, From}, Rest} ->
            S1 = State#{stdin_waiters => Rest},
            case S1 of
                #{stdin_dropped := Reason} ->
                    %% Not sent by beamtalk-exec (no write is queued after a
                    %% refusal or `stdin_failed`); guards a protocol violation.
                    gen_server:reply(From, {error, line_not_written_error(Class, Reason)}),
                    S1;
                _ ->
                    S1#{stdin_unwritten => queue:in(From, stdin_unwritten(S1))}
            end;
        {empty, _} ->
            State
    end.

-doc "The oldest queued write is in the child's stdin pipe: its caller gets `nil`.".
-spec write_written(map()) -> map().
write_written(State) ->
    case queue:out(stdin_unwritten(State)) of
        {{value, From}, Rest} ->
            gen_server:reply(From, nil),
            State#{stdin_unwritten => Rest};
        {empty, _} ->
            State
    end.

-doc """
The writer thread could not write the oldest queued line (the child exited or
closed its stdin, BT-5269): it and every line queued behind it are lost, so
each of their callers gets the `runtime_error` a dropped line gets. Every
later `writeLine:` raises too.
""".
-spec write_failed(atom(), binary(), map()) -> map().
write_failed(Class, Reason, State) ->
    Err = line_not_written_error(Class, Reason),
    lists:foreach(
        fun(From) -> gen_server:reply(From, {error, Err}) end,
        queue:to_list(stdin_unwritten(State))
    ),
    mark_stdin_dropped(Reason, State#{stdin_unwritten => queue:new()}).

-doc """
The `runtime_error` a `writeLine:` raises once the exec port has dropped a
write: kind and hint (`Reason` from the port) shared by the dropped call and
every later one.
""".
-spec stdin_dropped_error(atom(), binary()) -> #beamtalk_error{}.
stdin_dropped_error(Class, Reason) ->
    beamtalk_error:new(
        runtime_error, Class, 'writeLine:', <<"stdin write failed: ", Reason/binary>>
    ).

-doc """
The oldest parked write was dropped: its caller gets a `runtime_error` saying
the line was not written. Every later `writeLine:` raises too.
""".
-spec write_dropped(atom(), binary(), map()) -> map().
write_dropped(Class, Reason, State) ->
    S1 =
        case queue:out(stdin_waiters(State)) of
            {{value, From}, Rest} ->
                gen_server:reply(From, {error, line_not_written_error(Class, Reason)}),
                State#{stdin_waiters => Rest};
            {empty, _} ->
                State
        end,
    mark_stdin_dropped(Reason, S1).

%% The error a writeLine: whose own line was lost raises.
line_not_written_error(Class, Reason) ->
    beamtalk_error:with_message(
        stdin_dropped_error(Class, Reason),
        <<"writeLine: failed: the line was not written">>
    ).

%% Sticky: the first loss's reason is kept for every later writeLine:.
mark_stdin_dropped(Reason, State) ->
    case maps:is_key(stdin_dropped, State) of
        true -> State;
        false -> State#{stdin_dropped => Reason}
    end.

-doc """
The port is gone (child exited, or closed) before it answered the parked
writes: their callers get `port_closed`, since nothing confirms those lines
reached the child.
""".
-spec fail_pending_writes(atom(), map()) -> map().
fail_pending_writes(Class, State) ->
    lists:foreach(
        fun(From) ->
            gen_server:reply(From, {error, beamtalk_error:new(port_closed, Class, 'writeLine:')})
        end,
        queue:to_list(stdin_waiters(State)) ++ queue:to_list(stdin_unwritten(State))
    ),
    State#{stdin_waiters => queue:new(), stdin_unwritten => queue:new()}.

-spec stdin_waiters(map()) -> queue:queue(term()).
stdin_waiters(State) ->
    maps:get(stdin_waiters, State, queue:new()).

-spec stdin_unwritten(map()) -> queue:queue(term()).
stdin_unwritten(State) ->
    maps:get(stdin_unwritten, State, queue:new()).

-doc "Close the exec port (terminates all managed subprocesses).".
-spec close(port()) -> true.
close(Port) ->
    beamtalk_exec_port:close(Port).
