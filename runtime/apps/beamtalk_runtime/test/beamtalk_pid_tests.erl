%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_pid_tests).

-moduledoc """
Unit tests for `beamtalk_pid` (ADR 0126 §7.3).

Covers both exported functions directly:

`is_alive/1`:
- A live local pid answers true.
- A dead local pid answers false.
- A remote-node pid answers true optimistically (no network round trip;
  the caller discovers the real answer from the send itself).

`qualify_registered_ref/1`:
- An unqualified `{registered, Name}` ref is stamped with `node()`.
- An already-qualified `{registered, Name, OtherNode}` ref passes through.
- A `{global, Name}` ref passes through (node-independent by construction).
- An ordinary pid passes through (already node-qualified by BEAM).
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% is_alive/1 — local pids
%%% ============================================================================

is_alive_live_local_pid_test() ->
    Pid = spawn(fun() -> timer:sleep(5000) end),
    ?assert(beamtalk_pid:is_alive(Pid)),
    exit(Pid, kill).

is_alive_dead_local_pid_test() ->
    Pid = spawn(fun() -> ok end),
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        error(process_did_not_exit)
    end,
    ?assertNot(beamtalk_pid:is_alive(Pid)).

%%% ============================================================================
%%% is_alive/1 — remote pid (ADR 0126 §7.3: optimistic true)
%%% ============================================================================

is_alive_remote_pid_answers_true_test() ->
    %% A pid whose node() differs from node() must return true without raising
    %% badarg (is_process_alive/1 would crash on a remote pid). The module doc
    %% explains why the optimistic answer is correct: a dead remote actor's cast
    %% lands in the void, and a partitioned node surfaces as noconnection from
    %% the send itself.
    RemotePid = beamtalk_dist_test_helper:unreachable_pid(pid_liveness_remote_test),
    ?assertNotEqual(node(), node(RemotePid)),
    ?assert(beamtalk_pid:is_alive(RemotePid)).

%%% ============================================================================
%%% qualify_registered_ref/1
%%% ============================================================================

qualify_registered_ref_stamps_unqualified_ref_test() ->
    %% {registered, Name} with no node is stamped with this node.
    ?assertEqual(
        {registered, some_service, node()},
        beamtalk_pid:qualify_registered_ref({registered, some_service})
    ).

qualify_registered_ref_passes_through_qualified_ref_test() ->
    %% {registered, Name, OtherNode} already carries a node — pass through.
    OtherNode = 'other@example.com',
    Ref = {registered, some_service, OtherNode},
    ?assertEqual(Ref, beamtalk_pid:qualify_registered_ref(Ref)).

qualify_registered_ref_passes_through_global_ref_test() ->
    %% {global, Name} is node-independent by construction — pass through.
    Ref = {global, global_service},
    ?assertEqual(Ref, beamtalk_pid:qualify_registered_ref(Ref)).

qualify_registered_ref_passes_through_pid_test() ->
    %% An ordinary pid is already node-qualified by BEAM — pass through.
    Pid = self(),
    ?assertEqual(Pid, beamtalk_pid:qualify_registered_ref(Pid)).
