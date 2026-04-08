%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_os).

%%% **DDD Context:** Object System Context

-moduledoc """
OS class implementation — shell command execution.

OS provides class-side methods for executing shell commands
and capturing their output as a String.

## Methods

| Selector          | Description                                      |
|-------------------|--------------------------------------------------|
| `run:`            | Execute shell command, return trimmed stdout     |
| `run:timeout:`    | Same, with an explicit wall-clock timeout (ms)   |
""".

-export(['run:'/1, run/1, 'run:timeout:'/2, run/2]).

%% Default wall-clock timeout for OS run: (30 seconds).
-define(DEFAULT_TIMEOUT_MS, 30000).

%% Maximum output bytes collected from a shell command.
%% Guards against memory exhaustion from commands with unbounded output.
-define(MAX_OUTPUT_BYTES, 10485760).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Execute a shell command and return its trimmed stdout as a binary.

Runs the command through the platform shell (`/bin/sh -c` on Unix,
`cmd.exe /c` on Windows) via an Erlang port.
Blocks until the command exits or the default 30-second timeout fires.
Raises `#beamtalk_error{kind = timeout}` on timeout,
`output_too_large` if stdout exceeds 10 MB.
""".
-spec 'run:'(binary()) -> binary().
'run:'(Cmd) when is_binary(Cmd) ->
    run_cmd(Cmd, ?DEFAULT_TIMEOUT_MS, 'run:');
'run:'(_) ->
    raise_type_error('run:', <<"Argument must be a String">>).

-doc """
Execute a shell command with an explicit timeout in milliseconds.

Behaves like `run:` but the caller controls the wall-clock deadline.
Raises `#beamtalk_error{kind = timeout}` if the command has not exited
within `TimeoutMs` milliseconds.
""".
-spec 'run:timeout:'(binary(), Timeout :: pos_integer()) -> binary().
'run:timeout:'(Cmd, TimeoutMs) when is_binary(Cmd), is_integer(TimeoutMs), TimeoutMs > 0 ->
    run_cmd(Cmd, TimeoutMs, 'run:timeout:');
'run:timeout:'(Cmd, _TimeoutMs) when not is_binary(Cmd) ->
    raise_type_error('run:timeout:', <<"Argument must be a String">>);
'run:timeout:'(_, _) ->
    raise_type_error('run:timeout:', <<"Timeout must be a positive Integer">>).

-doc "FFI shim for (Erlang beamtalk_os) run: cmd dispatch.".
-spec run(binary()) -> binary().
run(Cmd) -> 'run:'(Cmd).

-doc "FFI shim for (Erlang beamtalk_os) run: cmd timeout: ms dispatch.".
-spec run(binary(), Timeout :: pos_integer()) -> binary().
run(Cmd, TimeoutMs) -> 'run:timeout:'(Cmd, TimeoutMs).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc """
Open a shell port and collect all stdout until exit or timeout.

On Unix, `{spawn, Cmd}` routes through `/bin/sh -c` automatically.
On Windows we must explicitly route through `cmd.exe /c` so that shell
builtins (e.g. `ver`) and chaining operators (`&`) work correctly.
""".
-spec run_cmd(binary(), pos_integer(), atom()) -> binary().
run_cmd(Cmd, TimeoutMs, Selector) ->
    Port = open_shell_port(Cmd),
    Timer = erlang:start_timer(TimeoutMs, self(), os_run_timeout),
    collect(Port, <<>>, Timer, Selector).

-doc "Open a port through the platform shell.".
-spec open_shell_port(binary()) -> port().
open_shell_port(Cmd) ->
    CmdStr = binary_to_list(Cmd),
    case os:type() of
        {win32, _} ->
            ComSpec = os:getenv("COMSPEC", "cmd.exe"),
            open_port(
                {spawn_executable, ComSpec},
                [{args, ["/c", CmdStr]}, hide, exit_status, binary]
            );
        _ ->
            open_port({spawn, CmdStr}, [exit_status, binary])
    end.

-doc "Accumulate port output, then trim trailing whitespace on exit.".
-spec collect(port(), binary(), reference(), atom()) -> binary().
collect(Port, Acc, Timer, Selector) ->
    receive
        {Port, {data, Data}} ->
            case byte_size(Acc) + byte_size(Data) > ?MAX_OUTPUT_BYTES of
                true ->
                    erlang:cancel_timer(Timer),
                    port_close(Port),
                    flush_port(Port),
                    raise_output_too_large(Selector);
                false ->
                    collect(Port, <<Acc/binary, Data/binary>>, Timer, Selector)
            end;
        {Port, {exit_status, _}} ->
            %% Cancel the timer and flush any timer message that may have
            %% already been enqueued before cancel_timer/1 ran.
            erlang:cancel_timer(Timer),
            flush_timer(Timer),
            string:trim(Acc, trailing);
        {timeout, Timer, os_run_timeout} ->
            port_close(Port),
            flush_port(Port),
            raise_timeout(Selector)
    end.

-doc """
Drain any residual port messages left in the mailbox after
port_close/1 so they do not accumulate in long-lived callers.
""".
-spec flush_port(port()) -> ok.
flush_port(Port) ->
    receive
        {Port, _} -> flush_port(Port)
    after 0 -> ok
    end.

-doc """
Drain a stale timer message that may have been enqueued before
erlang:cancel_timer/1 ran. Safe to call even when cancel_timer succeeded.
""".
-spec flush_timer(reference()) -> ok.
flush_timer(Timer) ->
    receive
        {timeout, Timer, os_run_timeout} -> ok
    after 0 -> ok
    end.

-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'OS'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

-spec raise_timeout(atom()) -> no_return().
raise_timeout(Selector) ->
    Error0 = beamtalk_error:new(timeout, 'OS'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Command exceeded time limit">>),
    beamtalk_error:raise(Error2).

-spec raise_output_too_large(atom()) -> no_return().
raise_output_too_large(Selector) ->
    Error0 = beamtalk_error:new(output_too_large, 'OS'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Command output exceeded 10 MB limit">>),
    beamtalk_error:raise(Error2).
