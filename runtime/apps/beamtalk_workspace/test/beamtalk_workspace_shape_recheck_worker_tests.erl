%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_shape_recheck_worker_tests).

-moduledoc """
Unit tests for `beamtalk_workspace_shape_recheck_worker`'s gen_server OTP
interface and the enqueue variants whose cast handlers the integration test
suites (`beamtalk_repl_loader_shape_recheck_tests`,
`beamtalk_repl_loader_leaf_change_recheck_tests`,
`beamtalk_repl_alias_change_recheck_tests`) leave uncovered because they go
through the high-level `enqueue/1` → `handle_cast({recheck,...})` path only.

Specifically covers (from the 2026-07-20 coverage artifact, 30.8% baseline):

  - `enqueue_leaf_change/1` (line 115) — the `{leaf_change, ...}` cast send
  - `enqueue_alias_change/1` (line 129) — the `{alias_change, ...}` cast send
  - `handle_cast({leaf_change, ...}, State)` (lines 139-157)
  - `handle_cast({alias_change, ...}, State)` (lines 177-195)
  - `handle_cast(_Msg, State)` catch-all (lines 196-197)
  - `handle_call/3` returning `{error, unknown_request}` (lines 199-200)
  - `handle_info/2` no-op (lines 202-203)
  - `terminate/2` (lines 205-206)
  - `code_change/3` (lines 208-209)

Each test uses a minimal setup — only the worker itself is started. The
`leaf_change` and `alias_change` cast handlers call
`beamtalk_repl_loader:maybe_trigger_*` functions that require the full compiler
stack; without it those functions will fail, but each handler's try/catch (see
the moduledoc's "Crash isolation" section) swallows any exception and returns
`{noreply, State}` — the worker stays alive.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Minimal fixture: just the worker process, no compiler stack
%%====================================================================

worker_setup() ->
    case whereis(beamtalk_workspace_shape_recheck_worker) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    {ok, _} = beamtalk_workspace_shape_recheck_worker:start_link(),
    ok.

worker_teardown(_) ->
    case whereis(beamtalk_workspace_shape_recheck_worker) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

%%====================================================================
%% handle_call/3: unknown requests return {error, unknown_request}
%%====================================================================

handle_call_returns_error_for_unknown_request_test_() ->
    {setup, fun worker_setup/0, fun worker_teardown/1, fun(_) ->
        [
            ?_test(begin
                Result = gen_server:call(
                    beamtalk_workspace_shape_recheck_worker, some_unknown_request
                ),
                ?assertEqual({error, unknown_request}, Result)
            end)
        ]
    end}.

%%====================================================================
%% handle_info/2: unexpected messages are silently ignored
%%====================================================================

handle_info_ignores_unexpected_messages_test_() ->
    {setup, fun worker_setup/0, fun worker_teardown/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_workspace_shape_recheck_worker ! some_unexpected_info,
                %% A synchronous sys:get_state/1 only returns after all
                %% messages enqueued ahead of it (our info message) have been
                %% processed — confirming the worker is still alive and the
                %% message did not crash it.
                ?assertEqual(undefined, sys:get_state(beamtalk_workspace_shape_recheck_worker))
            end)
        ]
    end}.

%%====================================================================
%% terminate/2: returns ok (OTP callback, called by gen_server on stop)
%%====================================================================

terminate_returns_ok_test() ->
    ?assertEqual(ok, beamtalk_workspace_shape_recheck_worker:terminate(normal, undefined)).

%%====================================================================
%% code_change/3: returns {ok, State} (OTP hot-reload callback)
%%====================================================================

code_change_returns_ok_test() ->
    State = undefined,
    ?assertEqual(
        {ok, State},
        beamtalk_workspace_shape_recheck_worker:code_change(<<"1.0">>, State, [])
    ).

%%====================================================================
%% handle_cast: leaf_change — worker survives even without the compiler stack
%%====================================================================

leaf_change_cast_worker_survives_test_() ->
    {setup, fun worker_setup/0, fun worker_teardown/1, fun(_) ->
        [
            ?_test(begin
                %% enqueue_leaf_change/1 sends {leaf_change, NewlyNonLeafSuperclasses}.
                %% The handler calls maybe_trigger_leaf_change_recheck/1 which
                %% requires the full compiler stack (not started here) — the
                %% try/catch in handle_cast({leaf_change,...}) swallows any
                %% exception, so the worker must still be alive afterwards.
                ok = beamtalk_workspace_shape_recheck_worker:enqueue_leaf_change([
                    <<"SomeParentClass">>
                ]),
                %% Synchronous round trip forces the cast to drain before the
                %% assertion below.
                ?assertEqual(undefined, sys:get_state(beamtalk_workspace_shape_recheck_worker))
            end)
        ]
    end}.

%%====================================================================
%% handle_cast: alias_change — worker survives even without the compiler stack
%%====================================================================

alias_change_cast_worker_survives_test_() ->
    {setup, fun worker_setup/0, fun worker_teardown/1, fun(_) ->
        [
            ?_test(begin
                %% enqueue_alias_change/1 sends {alias_change, AliasNameBins}.
                %% Same defensive reasoning as leaf_change above.
                ok = beamtalk_workspace_shape_recheck_worker:enqueue_alias_change([
                    <<"SomeAliasName">>
                ]),
                ?assertEqual(undefined, sys:get_state(beamtalk_workspace_shape_recheck_worker))
            end)
        ]
    end}.

%%====================================================================
%% handle_cast: catch-all — unknown casts are silently dropped
%%====================================================================

unknown_cast_is_silently_dropped_test_() ->
    {setup, fun worker_setup/0, fun worker_teardown/1, fun(_) ->
        [
            ?_test(begin
                gen_server:cast(
                    beamtalk_workspace_shape_recheck_worker, {totally_unknown, payload}
                ),
                ?assertEqual(undefined, sys:get_state(beamtalk_workspace_shape_recheck_worker))
            end)
        ]
    end}.
