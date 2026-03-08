%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc OS class implementation — shell command execution.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% OS provides class-side methods for executing shell commands
%%% and capturing their output as a String.
%%%
%%% ## Methods
%%%
%%% | Selector          | Description                                      |
%%% |-------------------|--------------------------------------------------|
%%% | `run:`            | Execute shell command, return trimmed stdout     |
%%% | `run:timeout:`    | Same, with an explicit wall-clock timeout (ms)   |

-module(beamtalk_os).

-export(['run:'/1, run/1, 'run:timeout:'/2, run/2]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Default wall-clock timeout for OS run: (30 seconds).
-define(DEFAULT_TIMEOUT_MS, 30000).

%% Maximum output bytes collected from a shell command.
%% Guards against memory exhaustion from commands with unbounded output.
-define(MAX_OUTPUT_BYTES, 10485760).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Execute a shell command and return its trimmed stdout as a binary.
%%
%% Runs the command through `/bin/sh -c` via an Erlang port.
%% Blocks until the command exits or the default 30-second timeout fires.
%% Raises `#beamtalk_error{kind = timeout}` on timeout,
%% `output_too_large` if stdout exceeds 10 MB.
-spec 'run:'(binary()) -> binary().
'run:'(Cmd) when is_binary(Cmd) ->
    run_cmd(Cmd, ?DEFAULT_TIMEOUT_MS);
'run:'(_) ->
    raise_type_error('run:').

%% @doc Execute a shell command with an explicit timeout in milliseconds.
%%
%% Behaves like `run:` but the caller controls the wall-clock deadline.
%% Raises `#beamtalk_error{kind = timeout}` if the command has not exited
%% within `TimeoutMs` milliseconds.
-spec 'run:timeout:'(binary(), non_neg_integer()) -> binary().
'run:timeout:'(Cmd, TimeoutMs) when is_binary(Cmd), is_integer(TimeoutMs), TimeoutMs > 0 ->
    run_cmd(Cmd, TimeoutMs);
'run:timeout:'(Cmd, TimeoutMs) when not is_binary(Cmd) ->
    _ = TimeoutMs,
    raise_type_error('run:timeout:');
'run:timeout:'(_, _) ->
    raise_type_error('run:timeout:').

%% @doc FFI shim for (Erlang beamtalk_os) run: cmd dispatch.
-spec run(binary()) -> binary().
run(Cmd) -> 'run:'(Cmd).

%% @doc FFI shim for (Erlang beamtalk_os) run: cmd timeout: ms dispatch.
-spec run(binary(), non_neg_integer()) -> binary().
run(Cmd, TimeoutMs) -> 'run:timeout:'(Cmd, TimeoutMs).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @private Open a shell port and collect all stdout until exit or timeout.
-spec run_cmd(binary(), non_neg_integer()) -> binary().
run_cmd(Cmd, TimeoutMs) ->
    Port = open_port({spawn, binary_to_list(Cmd)}, [exit_status, binary]),
    Timer = erlang:start_timer(TimeoutMs, self(), os_run_timeout),
    collect(Port, <<>>, Timer).

%% @private Accumulate port output, then trim trailing whitespace on exit.
-spec collect(port(), binary(), reference()) -> binary().
collect(Port, Acc, Timer) ->
    receive
        {Port, {data, Data}} ->
            NewAcc = <<Acc/binary, Data/binary>>,
            case byte_size(NewAcc) > ?MAX_OUTPUT_BYTES of
                true ->
                    erlang:cancel_timer(Timer),
                    port_close(Port),
                    raise_output_too_large();
                false ->
                    collect(Port, NewAcc, Timer)
            end;
        {Port, {exit_status, _}} ->
            erlang:cancel_timer(Timer),
            string:trim(Acc, trailing);
        {timeout, Timer, os_run_timeout} ->
            port_close(Port),
            raise_timeout()
    end.

%% @private
-spec raise_type_error(atom()) -> no_return().
raise_type_error(Selector) ->
    Error0 = beamtalk_error:new(type_error, 'OS'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @private
-spec raise_timeout() -> no_return().
raise_timeout() ->
    Error0 = beamtalk_error:new(timeout, 'OS'),
    Error1 = beamtalk_error:with_selector(Error0, 'run:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Command exceeded time limit">>),
    beamtalk_error:raise(Error2).

%% @private
-spec raise_output_too_large() -> no_return().
raise_output_too_large() ->
    Error0 = beamtalk_error:new(output_too_large, 'OS'),
    Error1 = beamtalk_error:with_selector(Error0, 'run:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Command output exceeded 10 MB limit">>),
    beamtalk_error:raise(Error2).
