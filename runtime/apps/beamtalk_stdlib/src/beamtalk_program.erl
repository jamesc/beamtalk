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
%% unqualified.
-compile({no_auto_import, [exit/1]}).

-export([commandName/0, package/0, rootSupervisor/0, exit/0, 'exit:'/1, exit/1]).

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

-doc """
The running application's root supervisor (`Program rootSupervisor`): the
`Supervisor` handle the generated `beamtalk_<pkg>_app` callback registered when
its `[application] supervisor` started, or `nil` when no `[application]` has
started (legitimate for a library). Reads `beamtalk_supervisor:get_root/0`, whose
table is owned by `beamtalk_runtime`, so it needs no workspace.
""".
-spec rootSupervisor() -> tuple() | nil.
rootSupervisor() ->
    beamtalk_supervisor:get_root().

-doc """
The program's root package (BT-3651): `Package named: <root_package>`, where
`root_package` is the `beamtalk_runtime` app-env key every launcher sets from
the Rust-parsed manifest. Never returns `nil`; raises a structured error:

* `no_program_package` - no launcher recorded a root package (bare REPL).
* `ambiguous_program_package` - `beamtalk test` runs several packages.
* `package_not_loaded` - the root package's `.app` is not on the code path
  (the project was never built).
""".
-spec package() -> term().
package() ->
    case beamtalk_package:root_package() of
        {ok, Name} ->
            case beamtalk_package:find_app_for_package(Name) of
                {ok, _App} ->
                    beamtalk_package:named(Name);
                error ->
                    program_package_error(
                        package_not_loaded,
                        <<"The root package ", Name/binary, " is not loaded">>,
                        <<"Run `beamtalk build` so the package's .app is on the code path.">>
                    )
            end;
        ambiguous ->
            program_package_error(
                ambiguous_program_package,
                <<"More than one package is under test, so there is no single program package">>,
                <<"Run beamtalk test on a single package, or use Package named: explicitly.">>
            );
        undefined ->
            program_package_error(
                no_program_package,
                <<"This program has no root package">>,
                <<"Run from a project directory containing beamtalk.toml.">>
            )
    end.

-doc "End this program with status 0 (the job-level, safe exit). Does not return.".
-spec exit() -> no_return().
exit() ->
    'exit:'(0).

-doc """
End this program with status `Code` — the job-level, safe exit (ADR 0099 §3,
amended by BT-3634). What it does depends on who owns the node
(`beamtalk_capability:exit_policy/0`):

* **`node`** (`beamtalk run`, escript, release `eval`/`foreground`): the
  program owns the node, so it stops *gracefully* with `init:stop(Code)` (the
  supervision tree runs down, `terminate/2` runs, Logger handlers flush). The
  calling process blocks until the node dies. From the entry method's own call
  chain it throws `{beamtalk_script_exit, Code}` to the nearest harness
  (`beamtalk_script_harness:dispatch/3`, `beamtalk_repl_eval:dispatch_sync/3`),
  which stops the node (`eval`/`run`) or ends only that session (`rpc`).
  From an **actor** (a service process, off any harness's call chain) it stops
  the node directly. `init:stop/1` is asynchronous and bounded by the
  supervisors' shutdown timeouts: a hanging `terminate/2` delays the exit.
* **`shared`** (REPL / MCP / LSP / connected `run`): ends only *this session*.
  The throw is caught by the session evaluator, which reports the status and
  stops the session. From an **actor** there is no session to end, so it raises
  `#beamtalk_error{kind = program_exit_outside_entry}`: return a status to the
  entry method and call `Program exit:` there.
* **`none`** (`beamtalk test`, a bare runtime): raises
  `#beamtalk_error{kind = program_exit, details = #{status => Code}}` so code
  that calls it is testable with `should: [...] raise: #program_exit`.

No context lets `{beamtalk_script_exit, _}` escape as an uncaught throw from an
actor or a test.

**`on:do:` caveat.** Because the entry-chain exit is a `throw`, a block that
wraps `Program exit:` in an `on:do:` whose handler matches (`on: Error do:`, a
catch-all) intercepts it as an `erlang_throw`. `ensure:` re-raises after its
cleanup, so the exit still propagates.
""".
-spec 'exit:'(integer()) -> no_return().
'exit:'(Code) when is_integer(Code), Code >= 0, Code =< 255 ->
    Policy = beamtalk_capability:exit_policy(),
    case {Policy, in_actor()} of
        {none, _} ->
            Err0 = beamtalk_error:new(program_exit, 'Program', 'exit:'),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary(
                    io_lib:format("Program exit: ~B with no program or workspace to end", [Code])
                )
            ),
            Err2 = beamtalk_error:with_details(Err1, #{status => Code}),
            beamtalk_error:raise(
                beamtalk_error:with_hint(
                    Err2,
                    <<
                        "This node has no workspace (for example under beamtalk test), so "
                        "there is nothing to exit. Assert with should: [...] raise: #program_exit."
                    >>
                )
            );
        {node, true} ->
            beamtalk_script_harness:stop_node(Code);
        {shared, true} ->
            Err0 = beamtalk_error:new(program_exit_outside_entry, 'Program', 'exit:'),
            Err1 = beamtalk_error:with_details(Err0, #{status => Code}),
            beamtalk_error:raise(
                beamtalk_error:with_hint(
                    Err1,
                    <<
                        "Program exit: from an actor has no session to end in a shared "
                        "workspace. Return a status to the entry method and call "
                        "Program exit: there."
                    >>
                )
            );
        {_, false} ->
            %% On an entry call chain: unwind to the nearest harness (a script
            %% harness, the REPL evaluator, or `rpc`). `throw` (not `error`) keeps
            %% it distinct from a user-level `#beamtalk_error{}`, so an ordinary
            %% `on:do:` handler does not swallow it.
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

-spec program_package_error(atom(), binary(), binary()) -> no_return().
program_package_error(Kind, Message, Hint) ->
    Err0 = beamtalk_error:new(Kind, 'Program', package),
    Err1 = beamtalk_error:with_message(Err0, Message),
    beamtalk_error:raise(beamtalk_error:with_hint(Err1, Hint)).

-doc """
Is the caller an actor's `gen_server` process, mid-dispatch (off any harness's
call chain)? Actor dispatch stashes its state under `'$bt_actor_state'`.
""".
-spec in_actor() -> boolean().
in_actor() ->
    get('$bt_actor_state') =/= undefined.
