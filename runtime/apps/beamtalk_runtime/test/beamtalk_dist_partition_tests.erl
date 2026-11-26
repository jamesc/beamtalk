%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_partition_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Two-node EUnit tests for remote lifecycle selectors across a partition
(BT-5192, ADR 0126 §7.1, §7.3).

`beamtalk_dist_test_helper:partition/1` cuts this node off from a peer that
keeps running, so the remote actor is alive throughout; `heal/1` reconnects.
No sleeps: both helpers return once the partition (or the reconnection) is in
place. Each test checks after healing that the actor really did survive, so
"node unreachable" is never confused with "dead":

1. `kill` raises `node_down` (it answered `ok` before, as if the actor died).
2. `stop`, sync and async, raises/rejects with `node_down` (it was
   `actor_dead` before).
3. `isAlive` on a remote pid asks its node: `true` for a live actor, `false`
   for a dead one (it answered `true` for any remote pid before), and
   `node_down` across a partition. The same holds for a remote supervisor.
4. A remote lookup (`named:on:`, `allRegisteredOn:`), `spawnOn:`, `unregister`, shape
   manifest read or process-navigation snapshot whose target answers outside
   its contract is a structured error (each crashed with `case_clause` before).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

-export([hold_registered_port/2]).

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt5192_partition"),
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

partition_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {"remote kill across a partition raises node_down; the actor survives", fun() ->
                    kill_across_partition(PeerNode)
                end},
                {"remote stop across a partition raises node_down; the actor survives", fun() ->
                    sync_stop_across_partition(PeerNode)
                end},
                {"remote async stop across a partition rejects with node_down", fun() ->
                    async_stop_across_partition(PeerNode)
                end},
                {"remote isAlive asks the node: true for a live actor, false for a dead one",
                    fun() -> is_alive_asks_remote_node(PeerNode) end},
                {"remote isAlive of a pid from an earlier node incarnation is false", fun() ->
                    is_alive_of_earlier_node_incarnation_is_false(PeerNode)
                end},
                {"remote isAlive across a partition raises node_down", fun() ->
                    is_alive_across_partition(PeerNode)
                end},
                {"remote async isAlive across a partition rejects with node_down", fun() ->
                    async_is_alive_across_partition(PeerNode)
                end},
                {"remote supervisor isAlive asks the node", fun() ->
                    supervisor_is_alive_asks_remote_node(PeerNode)
                end},
                {"a send to a remote name registered to a port is no_such_process", fun() ->
                    remote_name_registered_to_port(PeerNode)
                end},
                {"remote lookups whose target answers outside its contract are errors", fun() ->
                    unexpected_remote_results_are_errors(PeerNode)
                end}
            ]
        end}}.

%%====================================================================
%% Helpers
%%====================================================================

start_remote_counter(PeerNode) ->
    {ok, Pid} = rpc:call(PeerNode, test_counter, start, [0]),
    ?assertEqual(PeerNode, node(Pid)),
    Pid.

remote_alive(PeerNode, Pid) ->
    rpc:call(PeerNode, erlang, is_process_alive, [Pid]).

%% Run `Fun` while partitioned from `PeerNode`, then heal, whatever `Fun` does.
across_partition(PeerNode, Fun) ->
    ok = beamtalk_dist_test_helper:partition(PeerNode),
    try
        Fun()
    after
        ok = beamtalk_dist_test_helper:heal(PeerNode)
    end.

%% `{error, Error}` returned (a raising op converts its own error with
%% `?assertBtErrorWith`), or what else happened.
run_op(F) ->
    try F() of
        {error, #beamtalk_error{} = E} -> {error, E};
        Value -> {returned, Value}
    catch
        Class:Reason -> {raised, Class, Reason}
    end.

%% A structured error whose text names the unexpected answer.
is_unexpected_result_error({error, #beamtalk_error{} = E}) ->
    Text = iolist_to_binary(io_lib:format("~tp", [E])),
    binary:match(Text, <<"unexpected_result">>) =/= nomatch;
is_unexpected_result_error(_) ->
    false.

%%====================================================================
%% 1. kill
%%====================================================================

kill_across_partition(PeerNode) ->
    Pid = start_remote_counter(PeerNode),
    try
        across_partition(PeerNode, fun() ->
            ?assertBtErrorDetails(
                node_down, _, kill, #{node := PeerNode}, beamtalk_actor:sync_send(Pid, kill, [])
            )
        end),
        ?assert(remote_alive(PeerNode, Pid))
    after
        rpc:call(PeerNode, erlang, exit, [Pid, kill])
    end.

%%====================================================================
%% 2. stop
%%====================================================================

sync_stop_across_partition(PeerNode) ->
    Pid = start_remote_counter(PeerNode),
    try
        across_partition(PeerNode, fun() ->
            ?assertBtErrorDetails(
                node_down, _, stop, #{node := PeerNode}, beamtalk_actor:sync_send(Pid, stop, [])
            )
        end),
        ?assert(remote_alive(PeerNode, Pid)),
        %% Healed: the same stop now succeeds.
        ?assertEqual(ok, beamtalk_actor:sync_send(Pid, stop, [])),
        ?assertNot(remote_alive(PeerNode, Pid))
    after
        rpc:call(PeerNode, erlang, exit, [Pid, kill])
    end.

async_stop_across_partition(PeerNode) ->
    Pid = start_remote_counter(PeerNode),
    try
        across_partition(PeerNode, fun() ->
            Future = beamtalk_future:new(),
            ok = beamtalk_actor:async_send(Pid, stop, [], Future),
            try beamtalk_future:await(Future, 10000) of
                Result -> erlang:error({expected_rejection, Result})
            catch
                throw:{future_rejected, #beamtalk_error{} = E} ->
                    ?assertEqual({node_down, stop}, {
                        E#beamtalk_error.kind, E#beamtalk_error.selector
                    }),
                    ?assertMatch(#{node := PeerNode}, E#beamtalk_error.details)
            end
        end),
        ?assert(remote_alive(PeerNode, Pid))
    after
        rpc:call(PeerNode, erlang, exit, [Pid, kill])
    end.

%%====================================================================
%% 3. isAlive
%%====================================================================

is_alive_asks_remote_node(PeerNode) ->
    Pid = start_remote_counter(PeerNode),
    ?assertEqual(true, beamtalk_actor:sync_send(Pid, isAlive, [])),
    ok = rpc:call(PeerNode, gen_server, stop, [Pid]),
    ?assertEqual(false, beamtalk_actor:sync_send(Pid, isAlive, [])).

%% A pid from an earlier incarnation of the peer node (the node restarted
%% under the same name) is dead: `isAlive` answers `false`. On the supported
%% OTP releases (28, 29) the far side's `erlang:is_process_alive/1` answers
%% `false` for such a pid rather than raising `badarg`; this pins that.
is_alive_of_earlier_node_incarnation_is_false(PeerNode) ->
    Live = start_remote_counter(PeerNode),
    try
        Stale = with_other_creation(Live),
        ?assertEqual(PeerNode, node(Stale)),
        ?assertNotEqual(Live, Stale),
        ?assertEqual(false, beamtalk_actor:sync_send(Stale, isAlive, []))
    after
        rpc:call(PeerNode, erlang, exit, [Live, kill])
    end.

%% `Pid` with the creation (node incarnation) number changed: the same node
%% name and process id, but from a different life of that node. The creation
%% is the last 4 bytes of a NEW_PID_EXT encoding.
with_other_creation(Pid) ->
    <<131, 88, Rest/binary>> = term_to_binary(Pid, [{minor_version, 2}]),
    Size = byte_size(Rest) - 4,
    <<Head:Size/binary, Creation:32>> = Rest,
    binary_to_term(<<131, 88, Head/binary, (Creation bxor 1):32>>).

is_alive_across_partition(PeerNode) ->
    Pid = start_remote_counter(PeerNode),
    try
        across_partition(PeerNode, fun() ->
            ?assertBtErrorDetails(
                node_down,
                _,
                isAlive,
                #{node := PeerNode},
                beamtalk_actor:sync_send(Pid, isAlive, [])
            )
        end),
        ?assertEqual(true, beamtalk_actor:sync_send(Pid, isAlive, []))
    after
        rpc:call(PeerNode, erlang, exit, [Pid, kill])
    end.

async_is_alive_across_partition(PeerNode) ->
    Pid = start_remote_counter(PeerNode),
    try
        across_partition(PeerNode, fun() ->
            Future = beamtalk_future:new(),
            ok = beamtalk_actor:async_send(Pid, isAlive, [], Future),
            ?assertThrow(
                {future_rejected, #beamtalk_error{
                    kind = node_down, selector = isAlive, details = #{node := PeerNode}
                }},
                beamtalk_future:await(Future, 10000)
            )
        end),
        Healed = beamtalk_future:new(),
        ok = beamtalk_actor:async_send(Pid, isAlive, [], Healed),
        ?assertEqual(true, beamtalk_future:await(Healed, 10000))
    after
        rpc:call(PeerNode, erlang, exit, [Pid, kill])
    end.

supervisor_is_alive_asks_remote_node(PeerNode) ->
    %% Any remote pid behind a supervisor handle: isAlive only reads liveness.
    Pid = start_remote_counter(PeerNode),
    Handle = {beamtalk_supervisor, 'BT5192RemoteSup', bt5192_remote_sup, Pid},
    ?assertEqual(true, beamtalk_supervisor:isAlive(Handle)),
    try
        across_partition(PeerNode, fun() ->
            ?assertBtErrorDetails(
                node_down,
                'BT5192RemoteSup',
                isAlive,
                #{node := PeerNode, reason := noconnection},
                beamtalk_supervisor:isAlive(Handle)
            )
        end)
    after
        ok = rpc:call(PeerNode, gen_server, stop, [Pid])
    end,
    ?assertEqual(false, beamtalk_supervisor:isAlive(Handle)).

%%====================================================================
%% 4. Unexpected erpc results
%%====================================================================

%% `meck` replaces the erpc target on the peer only, so each remote op gets an
%% answer outside its contract without touching this node's code.
unexpected_remote_results_are_errors(PeerNode) ->
    ok = rpc:call(PeerNode, meck, new, [?BT_ACTOR_WIRE_MODULE, [passthrough, no_link]]),
    ok = rpc:call(PeerNode, meck, new, [beamtalk_release, [passthrough, no_link]]),
    ok = rpc:call(PeerNode, meck, new, [beamtalk_process_navigation, [passthrough, no_link]]),
    try
        Weird = fun(_A, _B) -> bt5192_not_a_contract_answer end,
        ok = rpc:call(PeerNode, meck, expect, [?BT_ACTOR_WIRE_MODULE, remote_named_target, Weird]),
        ok = rpc:call(PeerNode, meck, expect, [
            ?BT_ACTOR_WIRE_MODULE, allRegistered, fun(_) -> bt5192_not_a_list end
        ]),
        ok = rpc:call(PeerNode, meck, expect, [?BT_ACTOR_WIRE_MODULE, remote_spawn_target, Weird]),
        ok = rpc:call(PeerNode, meck, expect, [
            ?BT_ACTOR_WIRE_MODULE, unregister, fun(_) -> bt5192_not_ok end
        ]),
        ok = rpc:call(PeerNode, meck, expect, [
            beamtalk_release, shape_manifest, fun() -> bt5192_not_a_map end
        ]),
        ok = rpc:call(PeerNode, meck, expect, [
            beamtalk_process_navigation, remote_snapshot_target, fun(_) -> bt5192_not_a_list end
        ]),
        Proxy = #beamtalk_object{
            class = 'Actor', class_mod = undefined, pid = {registered, bt5192_some_name, PeerNode}
        },
        NodeObj = #{?BT_CLASS_TAG_KEY => 'Node', name => PeerNode},
        %% Each op is run on its own, so one that crashes (`case_clause`, as
        %% they all did before) is reported by name rather than hiding the rest.
        Ops = [
            {named_on, fun() ->
                beamtalk_actor_remote:remote_named(PeerNode, 'Actor', bt5192_some_name)
            end},
            {all_registered_on, fun() -> beamtalk_actor_remote:remote_all_registered(PeerNode) end},
            {spawn_on, fun() ->
                beamtalk_actor_remote:remote_spawn(PeerNode, 'Actor', undefined, 'spawnOn:')
            end},
            {shape_manifest, fun() -> beamtalk_node:remote_shape_manifest(PeerNode) end},
            {process_navigation_on, fun() -> beamtalk_process_navigation:on(NodeObj, 10) end},
            {unregister, fun() ->
                ?assertBtErrorWith(
                    _,
                    _,
                    unregister,
                    fun(E) -> {error, E} end,
                    beamtalk_actor_ffi:unregister(Proxy)
                )
            end}
        ],
        Results = [{Op, run_op(F)} || {Op, F} <- Ops],
        ?assertEqual([], [R || {_Op, Res} = R <- Results, not is_unexpected_result_error(Res)])
    after
        rpc:call(PeerNode, meck, unload, [?BT_ACTOR_WIRE_MODULE]),
        rpc:call(PeerNode, meck, unload, [beamtalk_release]),
        rpc:call(PeerNode, meck, unload, [beamtalk_process_navigation])
    end.

%%====================================================================
%% 5. A remote name that names a port
%%====================================================================

remote_name_registered_to_port(PeerNode) ->
    Name = beamtalk_test_unique:atom("bt5192_port"),
    Holder = rpc:call(PeerNode, erlang, spawn, [?MODULE, hold_registered_port, [Name, self()]]),
    ?awaitMessage({port_registered, Holder}),
    try
        ?assertBtError(
            no_such_process, beamtalk_actor:sync_send({registered, Name, PeerNode}, getValue, [])
        )
    after
        Holder ! stop
    end.

-doc "Runs on the peer: registers a port under `Name` until told to stop.".
-spec hold_registered_port(atom(), pid()) -> ok.
hold_registered_port(Name, Collector) ->
    %% A UDP socket is a port on every platform, with no external program.
    {ok, Port} = gen_udp:open(0, [{inet_backend, inet}]),
    true = is_port(Port),
    true = register(Name, Port),
    Collector ! {port_registered, self()},
    receive
        stop -> gen_udp:close(Port)
    end.
