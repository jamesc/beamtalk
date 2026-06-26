%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_program).

%%% **DDD Context:** Object System Context

-moduledoc """
Program class implementation — the running program/invocation (ADR 0099 §2).

`Program` names *this running program* and ends it. It exposes `commandName`
(a convenience for usage/`--help` text) and the job-level half of the two-tier
exit (`exit`/`exit:`; `System halt:` is the node-level half). All methods are
class-side — `Program` has no instances.

The accessor is `commandName`, not `name`: `name` is the sealed reflective
class-identity accessor (`Behaviour>>name`), so a class-side `name` on `Program`
would shadow it and break `SystemNavigation` / reflection over the class universe.

## `commandName`

Returns the invoked program's name as a `String`:

* under a packaged escript it is the invoked filename (`escript:script_name/0`,
  wired by the escript bootstrap in Phase 4 via the `program_name` app env);
* under `beamtalk run` there is no `argv[0]`, so it is the literal `"beamtalk"`.

The value is read from the `beamtalk_runtime` `program_name` application
environment, seeded at boot by each entry harness (run-mode startup sets
`"beamtalk"`; the escript `main/1` sets the script name). This app-env signal is
process-independent and set synchronously before any user code dispatches — the
mechanism settled for the `program_name` Open Question (ADR 0099 §2). Absent
(e.g. the persistent/connected workspace path), it defaults to `"beamtalk"`.
""".

%% `exit/1` shadows the auto-imported `erlang:exit/1` BIF; we never call the BIF
%% unqualified, and only ever call `erlang:halt/1` (also qualified).
-compile({no_auto_import, [exit/1]}).

-export([commandName/0, exit/0, 'exit:'/1, exit/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Return the invoked program's name as a binary String.".
-spec commandName() -> binary().
commandName() ->
    case application:get_env(beamtalk_runtime, program_name) of
        {ok, Name} when is_binary(Name) -> Name;
        {ok, Name} when is_list(Name) -> list_to_binary(Name);
        _ -> <<"beamtalk">>
    end.

-doc "End this program with status 0 (the job-level, safe exit). Does not return.".
-spec exit() -> no_return().
exit() ->
    'exit:'(0).

-doc """
End this program with status `Code` — the job-level, safe exit (ADR 0099 §3).

In a **node-owning** context (run-mode / escript — the program *is* the node)
this exits the node with `Code` via `erlang:halt/1` (which flushes the `io`
layer). In a **shared/connected** context (REPL / MCP / LSP / connected
`beamtalk run`) it must end only *this session's* job and leave the node up; it
raises a tagged `script_exit` signal carrying the status, which the session
evaluator catches to report the status to the connecting client and terminate
the session (BT-2688, ADR 0099 §3 / Phase 5).

**Process boundary.** The run-mode/escript path is an immediate `erlang:halt/1`,
so it is effective from *any* process — including a spawned/supervised Actor —
because the node is dedicated to this one program; there is no harness throw to
unwind, so the ADR 0099 §3 supervised-Actor-restart caveat does not apply here.
That caveat *does* apply to the connected-mode path: the signal is caught by the
session evaluator, so it is effective only from the entry method's own
synchronous call chain; a spawned Actor there must return a status to the entry
method and let *it* call `exit:`.

**`on:do:` caveat (connected mode).** Because the connected exit is a `throw`, a
block that wraps `Program exit:` in an `on:do:` whose handler class matches
(`on: Error do:` / `on: Exception do:` / a catch-all) will intercept it as an
`erlang_throw` rather than letting the session end. `ensure:` is unaffected (it
re-raises after running cleanup, so the exit still propagates). The robust fix is
to add this signal to the non-local-return passthrough the `on:do:` codegen
already emits for `{'$bt_nlr', _}` (see `control_flow/exception_handling.rs`);
that is a follow-up (tracked on BT-2688). In practice `Program exit:` is called at
the top of a `main:` or as a bare REPL expression, neither of which is wrapped in
such a handler.
""".
-spec 'exit:'(integer()) -> no_return().
'exit:'(Code) when is_integer(Code), Code >= 0, Code =< 255 ->
    case node_owning() of
        true ->
            erlang:halt(Code);
        false ->
            %% Connected/shared context: end only THIS session's job, not the
            %% shared node. The tagged `script_exit` signal carries the status; the
            %% session evaluator (`beamtalk_repl_eval`/`beamtalk_repl_shell`) catches
            %% it, replies with the exit status, and stops the session shell
            %% (BT-2688). `throw` (not `error`) keeps it distinct from a user-level
            %% `#beamtalk_error{}`, so an ordinary `on:do:` handler does not swallow
            %% it.
            throw({beamtalk_script_exit, Code})
    end;
'exit:'(Code) when is_integer(Code) ->
    %% Out of the POSIX range — a wrong *value*, not a wrong *type*. Validated
    %% here (not deferred to erlang:halt/1, which would silently truncate to the
    %% low byte) for consistency with System halt:.
    Error0 = beamtalk_error:new(invalid_argument, 'Program'),
    Error1 = beamtalk_error:with_selector(Error0, 'exit:'),
    Error2 = beamtalk_error:with_details(Error1, #{got => Code}),
    Error3 = beamtalk_error:with_hint(
        Error2, <<"Exit status must be in the POSIX range 0..255">>
    ),
    beamtalk_error:raise(Error3);
'exit:'(_Code) ->
    beamtalk_error:raise_type_error('Program', 'exit:', <<"Exit status must be an Integer">>).

-doc "FFI shim for `(Erlang beamtalk_program) exit:` (proxy strips the colon).".
-spec exit(integer()) -> no_return().
exit(Code) ->
    'exit:'(Code).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc "Whether this invocation owns the node (run-mode / escript boot sets the env).".
-spec node_owning() -> boolean().
node_owning() ->
    application:get_env(beamtalk_runtime, node_owning, false) =:= true.
