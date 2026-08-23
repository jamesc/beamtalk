%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_boot).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture: boot the real runtime + stdlib for EUnit suites that
need a genuinely compiled class (not a hand-written `.erl` test double) —
e.g. `beamtalk_workspace`'s `beamtalk_repl_docs_tests` (doc-formatting
integration tests) and `beamtalk_repl_ops_browse_tests` (BT-3242 native-class
delegate-callers regression test), and `beamtalk_stdlib`'s
`beamtalk_test_case_stdlib_tests` (BT-3251 BIF-fallback-path regression
test against a real compiled `TestCase` subclass).

BT-3251: originally `beamtalk_workspace_test_boot`, living under
`beamtalk_workspace/test/` — moved here (mirroring `beamtalk_test_corpus`'s
precedent, see its `moduledoc` for the "why a standalone app" rationale) once
`beamtalk_stdlib`'s EUnit suite needed the same boot sequence too. Reaching
into a peer/dependent app's `test/` tree from another app's suite would
recreate the duplication `beamtalk_test_support` exists to avoid.

Guarded so it is safe to call from more than one test module in the same
EUnit VM: `beamtalk_bootstrap:start_link/0` is only invoked when no bootstrap
process is already registered, and `beamtalk_stdlib:init/0` is itself
idempotent (a normal stdlib load skips a module whose on_load already ran).
""".

-export([boot_real_stdlib/1, wait_for_class/2]).

-doc """
Boot `beamtalk_runtime` + real stdlib, then block until `CanaryClass` is
registered as a live class process — the caller's signal that the compiled
module backing it has finished loading (on_load → register_class/0).

Exported alongside `wait_for_class/2` so a caller can also wait on a second,
later-loaded class within the same test (e.g. one loaded lazily after the
canary), without re-booting.
""".
-spec boot_real_stdlib(atom()) -> ok.
boot_real_stdlib(CanaryClass) ->
    application:ensure_all_started(beamtalk_runtime),
    case whereis(beamtalk_bootstrap) of
        undefined ->
            case beamtalk_bootstrap:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok
            end;
        _ ->
            ok
    end,
    beamtalk_stdlib:init(),
    wait_for_class(CanaryClass, 50),
    ok.

-spec wait_for_class(atom(), non_neg_integer()) -> ok.
wait_for_class(ClassName, 0) ->
    error({class_not_registered, ClassName});
wait_for_class(ClassName, Retries) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            timer:sleep(50),
            wait_for_class(ClassName, Retries - 1);
        _Pid ->
            ok
    end.
