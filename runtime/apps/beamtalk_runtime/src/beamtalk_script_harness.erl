%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_script_harness).

%%% **DDD Context:** Runtime Context — Program Execution

-moduledoc """
Run-mode entry harness — dispatches a script's entry method and maps the
outcome to a POSIX process exit status (ADR 0099 §3).

`beamtalk run ClassName selector [args]` evaluates a one-shot `-eval` that
bootstraps a run-mode workspace and then calls `dispatch/3` here. This module
owns the **implicit exit code** contract:

* the entry method returns normally → the node exits `0`;
* the entry method raises an uncaught error → a readable message is written to
  `standard_error` and the node exits non-zero (`1`).

`Program exit: N` (ADR 0099 §3, amended by BT-3634) throws
`{beamtalk_script_exit, N}` when it unwinds to a harness. This harness catches
it and stops the node *gracefully* with `stop_node/1` (`init:stop(N)`: the
supervision tree runs down, `terminate/2` runs, Logger handlers flush). From an
actor, `Program exit:` itself calls `stop_node/1`. `System halt: N` is the
immediate `halt_node/1`.
""".

-export([dispatch/3, stop_node/1, halt_node/1, halt_unless_stopping/1, flush_loggers/0]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Dispatch the entry method and halt with the implicit exit status.

Never returns: a normal return halts `0`, an uncaught error prints to stderr
and halts `1`, and `Program exit: N` stops the node gracefully with status `N`.
""".
-spec dispatch(pid(), atom(), list()) -> no_return().
dispatch(ClassPid, Selector, Args) ->
    try beamtalk_class_dispatch:class_send(ClassPid, Selector, Args) of
        _Result -> halt_unless_stopping(0)
    catch
        throw:{beamtalk_script_exit, Code} ->
            stop_node(Code);
        _Class:Reason:Stacktrace ->
            %% `beamtalk_error:format_safe/2` handles every shape this can take —
            %% a bare `#beamtalk_error{}`, the wrapped exception map from
            %% `beamtalk_error:raise/1`, and a raw Erlang reason with the real
            %% `#beamtalk_error{}` buried in the stacktrace — so no special-casing
            %% here.
            %%
            %% The write itself is guarded: if `standard_error`'s io server has
            %% exited (a detached node), `io:put_chars` raises an exit-class
            %% exception; swallow it with `catch` so the contract's `halt(1)`
            %% always runs rather than letting the node crash out of dispatch/3.
            catch io:put_chars(
                standard_error, [beamtalk_error:format_safe(Reason, Stacktrace), $\n]
            ),
            halt_unless_stopping(1)
    end.

-doc """
Stop the node gracefully with exit status `Code` (`Program exit:` where the
program owns the node). `init:stop/1` is asynchronous: it runs the supervision
tree down (`terminate/2`) and flushes Logger handlers, bounded by the
supervisors' shutdown timeouts, so a hanging `terminate/2` delays the exit. The
calling process then blocks until the node dies, so `Program exit:` never
returns.
""".
-spec stop_node(0..255) -> no_return().
stop_node(Code) ->
    init:stop(Code),
    receive
    after infinity -> ok
    end.

-doc """
Halt with the implicit exit status `Code`, unless a graceful stop is already
under way. An actor's `Program exit: N` calls `init:stop(N)` and the entry's
own call then fails as the tree shuts down; that failure must not race the
requested status with an implicit `halt(1)`, so wait for the stop to finish.
""".
-spec halt_unless_stopping(0..255) -> no_return().
halt_unless_stopping(Code) ->
    case init:get_status() of
        {stopping, _} ->
            receive
            after infinity -> ok
            end;
        _ ->
            erlang:halt(Code)
    end.

-doc """
Halt the node immediately with `Code` (`System halt:` where the program owns
the node). No `terminate/2` and no OTP shutdown, but Logger handlers are
flushed first (ADR 0129 §5); `erlang:halt/1` flushes the `io` layer.
""".
-spec halt_node(0..255) -> no_return().
halt_node(Code) ->
    flush_loggers(),
    erlang:halt(Code).

-doc "Best-effort synchronous flush of every Logger handler that can flush.".
-spec flush_loggers() -> ok.
flush_loggers() ->
    lists:foreach(
        fun
            (#{id := Id, module := logger_std_h}) ->
                catch logger_std_h:filesync(Id);
            (_) ->
                ok
        end,
        logger:get_handler_config()
    ).
