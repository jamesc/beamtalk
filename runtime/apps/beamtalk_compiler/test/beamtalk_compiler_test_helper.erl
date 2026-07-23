%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_test_helper).

%%% **DDD Context:** Compilation Context

-moduledoc """
Shared test helpers for tests that fault-inject the compiler server.

Used by beamtalk_compiler_server_tests.erl and (cross-app, via the umbrella
test code path) beamtalk_recheck_tests.erl.
""".

-export([wait_for_compiler_server_restart/0]).

-doc """
Wait for beamtalk_compiler_sup to restart beamtalk_compiler_server after an
injected exit, erroring out after ~1s of polling.

Note on timing: `whereis/1` returns a Pid as soon as OTP registers the name
during `gen:init_it`, which happens *before* `Module:init/1` completes.
That's fine for callers: a subsequent `gen_server:call` queues safely behind
`init/1` and is served once the server finishes initialising.
""".
-spec wait_for_compiler_server_restart() -> ok.
wait_for_compiler_server_restart() ->
    wait_for_compiler_server_restart(100).

wait_for_compiler_server_restart(0) ->
    error(compiler_server_did_not_restart);
wait_for_compiler_server_restart(Attempts) ->
    case whereis(beamtalk_compiler_server) of
        undefined ->
            timer:sleep(10),
            wait_for_compiler_server_restart(Attempts - 1);
        Pid when is_pid(Pid) ->
            ok
    end.
