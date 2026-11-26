%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_registered_send_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Two-node EUnit tests for sends to a node-qualified `{registered, Name, Node}`
ref (BT-5271, ADR 0126 §3).

A send to such a ref addresses `{Name, Node}` directly: one network round
trip, with no `erpc` `erlang:whereis/1` on `Node` first. The tests count the
peer-side `erlang:whereis(Name)` calls a send makes with a call trace on the
peer (`whereis_counter/2`), read only after `erlang:trace_delivered/1`, so
the count is exact without any sleep.

1. A plain sync send (`sync_send/3,4`) and a cast make no `whereis` on the
   peer, unless dispatch events are on (then the pid is resolved, so the
   send is observed with it).
2. Errors are unchanged: `no_such_process` for a name not registered on a
   live node, `node_down` across a partition.
3. Lifecycle selectors on the ref keep their answers (they still resolve the
   pid), and a send inside a call chain that holds the target still raises
   `deadlock_detected`.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

-export([whereis_counter/2]).

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt5271_registered"),
    {ok, _Started} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {Peer, PeerNode}.

cleanup({Peer, PeerNode}) ->
    %% A failed test can leave the partition in place; never leak it.
    try
        beamtalk_dist_test_helper:heal(PeerNode)
    catch
        _:_ -> ok
    end,
    beamtalk_dist_test_helper:stop_peer(Peer).

registered_send_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {"a sync send to a remote named ref makes no whereis round trip", fun() ->
                    sync_send_makes_no_whereis(PeerNode)
                end},
                {"a sync send with a timeout to a remote named ref makes no whereis", fun() ->
                    sync_send_timeout_makes_no_whereis(PeerNode)
                end},
                {"a cast to a remote named ref makes no whereis round trip", fun() ->
                    cast_makes_no_whereis(PeerNode)
                end},
                {"with dispatch events on, a remote named send resolves the pid", fun() ->
                    dispatch_events_on_resolves_the_pid(PeerNode)
                end},
                {"a remote name not registered on a live node is no_such_process", fun() ->
                    unregistered_name_is_no_such_process(PeerNode)
                end},
                {"a remote named send across a partition is node_down", fun() ->
                    named_send_across_partition_is_node_down(PeerNode)
                end},
                {"lifecycle selectors on a remote named ref keep their answers", fun() ->
                    lifecycle_selectors_keep_their_answers(PeerNode)
                end},
                {"a remote named send to an actor already in the call chain is a deadlock", fun() ->
                    call_chain_cycle_is_still_detected(PeerNode)
                end}
            ]
        end}}.

%%====================================================================
%% Helpers
%%====================================================================

%% Start a test counter on the peer, registered there as a fresh name.
start_named_counter(PeerNode, Initial) ->
    Name = beamtalk_test_unique:atom("bt5271_counter"),
    {ok, Pid} = rpc:call(PeerNode, test_counter, start, [Initial]),
    true = rpc:call(PeerNode, erlang, register, [Name, Pid]),
    {Name, Pid}.

stop_remote(PeerNode, Pid) ->
    _ = rpc:call(PeerNode, gen_server, stop, [Pid]),
    ok.

%% Run `Fun` with a peer-side counter of `erlang:whereis(Name)` calls, and
%% answer `{FunResult, WhereisCalls}`.
with_whereis_count(PeerNode, Name, Fun) ->
    Counter = rpc:call(PeerNode, erlang, spawn, [?MODULE, whereis_counter, [Name, self()]]),
    ?awaitMessage({whereis_counting, Counter}),
    try
        Result = Fun(),
        Counter ! {count, self()},
        receive
            {whereis_count, Counter, N} -> {Result, N}
        after 10000 ->
            error(whereis_counter_did_not_answer)
        end
    after
        Counter ! stop
    end.

-doc """
Runs on the peer: traces every process created from now on (an `erpc` call
runs in a fresh process there) for calls to `erlang:whereis(Name)`, and
answers `{whereis_count, self(), N}` on `{count, From}`. Before answering it
waits on `erlang:trace_delivered/1`, so every call made before the request
is counted.
""".
-spec whereis_counter(atom(), pid()) -> ok.
whereis_counter(Name, Owner) ->
    _ = erlang:trace_pattern({erlang, whereis, 1}, true, [global]),
    _ = erlang:trace(new_processes, true, [call]),
    Owner ! {whereis_counting, self()},
    whereis_counter_loop(Name, 0).

whereis_counter_loop(Name, N) ->
    receive
        {trace, _Pid, call, {erlang, whereis, [Name]}} ->
            whereis_counter_loop(Name, N + 1);
        {trace, _Pid, call, _Other} ->
            whereis_counter_loop(Name, N);
        {count, From} ->
            Ref = erlang:trace_delivered(all),
            receive
                {trace_delivered, all, Ref} -> ok
            end,
            Final = drain_whereis_calls(Name, N),
            From ! {whereis_count, self(), Final},
            whereis_counter_loop(Name, Final);
        stop ->
            _ = erlang:trace(new_processes, false, [call]),
            _ = erlang:trace_pattern({erlang, whereis, 1}, false, [global]),
            ok
    end.

drain_whereis_calls(Name, N) ->
    receive
        {trace, _Pid, call, {erlang, whereis, [Name]}} -> drain_whereis_calls(Name, N + 1);
        {trace, _Pid, call, _Other} -> drain_whereis_calls(Name, N)
    after 0 -> N
    end.

%%====================================================================
%% 1. One round trip
%%====================================================================

sync_send_makes_no_whereis(PeerNode) ->
    {Name, Pid} = start_named_counter(PeerNode, 42),
    Ref = {registered, Name, PeerNode},
    try
        {Result, Whereis} = with_whereis_count(PeerNode, Name, fun() ->
            beamtalk_actor:sync_send(Ref, getValue, [])
        end),
        ?assertEqual(42, Result),
        ?assertEqual(0, Whereis),
        %% The class-hinted entry point (what generated code calls) too.
        {Known, WhereisKnown} = with_whereis_count(PeerNode, Name, fun() ->
            beamtalk_actor:sync_send_known(Ref, 'Counter', 'setValue:', [5])
        end),
        ?assertEqual(ok, Known),
        ?assertEqual(0, WhereisKnown),
        ?assertEqual(5, beamtalk_actor:sync_send(Pid, getValue, []))
    after
        stop_remote(PeerNode, Pid)
    end.

sync_send_timeout_makes_no_whereis(PeerNode) ->
    {Name, Pid} = start_named_counter(PeerNode, 7),
    Ref = {registered, Name, PeerNode},
    try
        {Result, Whereis} = with_whereis_count(PeerNode, Name, fun() ->
            beamtalk_actor:sync_send(Ref, getValue, [], 5000)
        end),
        ?assertEqual(7, Result),
        ?assertEqual(0, Whereis),
        %% An invalid timeout keeps the pid path's type_error.
        ?assertBtError(type_error, beamtalk_actor:sync_send(Ref, getValue, [], -1))
    after
        stop_remote(PeerNode, Pid)
    end.

cast_makes_no_whereis(PeerNode) ->
    {Name, Pid} = start_named_counter(PeerNode, 0),
    Ref = {registered, Name, PeerNode},
    try
        {ok, Whereis} = with_whereis_count(PeerNode, Name, fun() ->
            beamtalk_actor:cast_send(Ref, 'setValue:', [9])
        end),
        ?assertEqual(0, Whereis),
        %% A sync send to the same actor is ordered after the cast (one
        %% sender, one receiver), so it sees the cast's effect.
        ?assertEqual(9, beamtalk_actor:sync_send(Pid, getValue, []))
    after
        stop_remote(PeerNode, Pid)
    end.

%% While dispatch events are on (as under `Tracing enable`), a send resolves
%% the pid first, so its events and trace capture see the actor's pid.
dispatch_events_on_resolves_the_pid(PeerNode) ->
    {Name, Pid} = start_named_counter(PeerNode, 4),
    Ref = {registered, Name, PeerNode},
    ok = beamtalk_telemetry:enable_dispatch_events(),
    try
        {Result, Whereis} = with_whereis_count(PeerNode, Name, fun() ->
            beamtalk_actor:sync_send(Ref, getValue, [])
        end),
        ?assertEqual(4, Result),
        ?assertEqual(1, Whereis),
        {ok, CastWhereis} = with_whereis_count(PeerNode, Name, fun() ->
            beamtalk_actor:cast_send(Ref, 'setValue:', [4])
        end),
        ?assertEqual(1, CastWhereis)
    after
        ok = beamtalk_telemetry:disable_dispatch_events(),
        stop_remote(PeerNode, Pid)
    end.

%%====================================================================
%% 2. Errors unchanged
%%====================================================================

unregistered_name_is_no_such_process(PeerNode) ->
    Name = beamtalk_test_unique:atom("bt5271_unregistered"),
    Ref = {registered, Name, PeerNode},
    ?assertBtError(no_such_process, beamtalk_actor:sync_send(Ref, getValue, [])),
    ?assertBtError(no_such_process, beamtalk_actor:sync_send(Ref, getValue, [], 1000)),
    %% The error still names the missing name.
    ?assertBtErrorWith(
        no_such_process,
        _,
        getValue,
        fun(#beamtalk_error{details = Details}) -> ?assertEqual(Name, maps:get(name, Details)) end,
        beamtalk_actor:sync_send(Ref, getValue, [])
    ),
    %% A cast to it is a silent no-op.
    ?assertEqual(ok, beamtalk_actor:cast_send(Ref, 'setValue:', [1])).

named_send_across_partition_is_node_down(PeerNode) ->
    {Name, Pid} = start_named_counter(PeerNode, 3),
    Ref = {registered, Name, PeerNode},
    try
        ok = beamtalk_dist_test_helper:partition(PeerNode),
        try
            ?assertBtErrorWith(
                node_down,
                _,
                getValue,
                fun(#beamtalk_error{details = Details}) ->
                    ?assertEqual(PeerNode, maps:get(node, Details))
                end,
                beamtalk_actor:sync_send(Ref, getValue, [])
            ),
            ?assertBtError(node_down, beamtalk_actor:sync_send(Ref, getValue, [], 1000)),
            %% A cast across the partition never raises. Distribution may
            %% buffer it and deliver it once the node is back (as for a cast
            %% to a remote pid), so it sets the value the actor already holds.
            ?assertEqual(ok, beamtalk_actor:cast_send(Ref, 'setValue:', [3]))
        after
            ok = beamtalk_dist_test_helper:heal(PeerNode)
        end,
        %% The actor was never touched: node_down, not dead.
        ?assertEqual(3, beamtalk_actor:sync_send(Ref, getValue, []))
    after
        stop_remote(PeerNode, Pid)
    end.

%%====================================================================
%% 3. Lifecycle selectors and the call-chain check
%%====================================================================

lifecycle_selectors_keep_their_answers(PeerNode) ->
    {Name, Pid} = start_named_counter(PeerNode, 0),
    Ref = {registered, Name, PeerNode},
    try
        ?assertEqual(Pid, beamtalk_actor:sync_send(Ref, pid, [])),
        ?assertEqual(true, beamtalk_actor:sync_send(Ref, isAlive, [])),
        ?assertEqual(true, beamtalk_actor:sync_send(Ref, isRegistered, [])),
        ?assertEqual(Name, beamtalk_actor:sync_send(Ref, registeredName, [])),
        MRef = beamtalk_actor:sync_send(Ref, monitor, []),
        ?assert(is_reference(MRef)),
        ?assertEqual(ok, beamtalk_actor:sync_send(Ref, stop, [])),
        ?assertMatch({'DOWN', MRef, process, Pid, _}, receive_down(MRef)),
        ?assertEqual(false, beamtalk_actor:sync_send(Ref, isAlive, [])),
        ?assertBtError(no_such_process, beamtalk_actor:sync_send(Ref, stop, []))
    after
        stop_remote(PeerNode, Pid)
    end.

receive_down(MRef) ->
    receive
        {'DOWN', MRef, _, _, _} = Down -> Down
    after 10000 -> error(no_down)
    end.

call_chain_cycle_is_still_detected(PeerNode) ->
    {Name, Pid} = start_named_counter(PeerNode, 0),
    Ref = {registered, Name, PeerNode},
    %% As if this process were serving a sync call that `Pid` made: sending
    %% back to `Pid` by name would deadlock, and the Layer 2 call-stack check
    %% must still see it although the send names no pid.
    Old = put(?BT_CALL_STACK_KEY, [Pid]),
    try
        ?assertBtError(deadlock_detected, beamtalk_actor:sync_send(Ref, getValue, []))
    after
        restore_call_stack(Old),
        stop_remote(PeerNode, Pid)
    end.

restore_call_stack(undefined) -> erase(?BT_CALL_STACK_KEY);
restore_call_stack(Old) -> put(?BT_CALL_STACK_KEY, Old).
