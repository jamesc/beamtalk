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

`Program exit: N` and `System halt: N` (ADR 0099 §3) end the node *directly*
via `erlang:halt/1` from the running process — in run-mode the program owns the
node — so they never unwind back here. This harness therefore only sees the two
*implicit* outcomes above.
""".

-export([dispatch/3]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Dispatch the entry method and halt with the implicit exit status.

Never returns: a normal return halts `0`, an uncaught error prints to stderr
and halts `1`. `Program exit:` / `System halt:` halt before reaching the catch.
""".
-spec dispatch(pid(), atom(), list()) -> no_return().
dispatch(ClassPid, Selector, Args) ->
    try beamtalk_class_dispatch:class_send(ClassPid, Selector, Args) of
        _Result -> erlang:halt(0)
    catch
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
            erlang:halt(1)
    end.
