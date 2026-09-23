%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_remote_spawn_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
Two-node EUnit tests for argument-free remote spawn and lookup (ADR 0126
§3, Phase 2, BT-3599): `beamtalk_actor:remote_spawn/4`, `remote_named/3`,
`remote_all_registered/1`, and the `{registered, Name, Node}` node-qualified
proxy shape those introduce.

Reuses `beamtalk_dist_test_helper` (BT-3578) for the peer node — no second
harness — and the existing `test_counter` actor fixture (already forwarded
to the peer's code path) as the spawn target, registered under class name
`'Counter'` in the peer's own `beamtalk_class_metadata` (a fresh ETS table
per peer boot, so this never collides with any `'Counter'` registration on
the origin test node or another peer).

Exercises `beamtalk_actor`'s public entry points directly (mirroring
`beamtalk_dist_wirecheck_tests`' own level) rather than through the
`actor.bt` FFI shims (`doSpawnOn/2` etc.) — those shims are thin argument
plumbing (`class_self_to_name_and_module/1` + `node_arg_to_atom/1`) around
exactly these functions, and `just build`'s stdlib compile already proves
the `.bt` selectors parse and wire up to the right arity.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Shared fixture
%%====================================================================

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3599_remote_spawn"),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

-doc "A short, run-unique atom, so repeated `just test` runs never collide on a stale name.".
unique_name(Prefix) ->
    list_to_atom(Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))).

-doc """
Register `'Counter'` -> `test_counter` in the target node's class metadata
(ADR 0126 §3: "the class is resolved by name on the target node"), matching
`test_counter:init/1`'s hardcoded `'$beamtalk_class' => 'Counter'` marker —
`pid_class_name/1` (used by `named:on:`'s class check) reads that marker
back, so the registered name and the marker must agree.
""".
register_counter_class(Node) ->
    ok = rpc:call(Node, beamtalk_class_metadata, insert, [
        'Counter', test_counter, undefined, none, false
    ]).

remote_spawn_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {"spawnOn:-shaped anonymous remote spawn lands on the target node", fun() ->
                    anonymous_spawn_lands_on_target(PeerNode)
                end},
                {"spawnAs:on: + named:on: round-trip, survives the erpc worker's exit", fun() ->
                    named_spawn_and_lookup_round_trip(PeerNode)
                end},
                {"node_down on an unreachable node", fun() -> node_down_on_unreachable_node() end},
                {"class_not_found for a class not loaded on the peer", fun() ->
                    class_not_found_for_unregistered_class(PeerNode)
                end},
                {
                    "a node-qualified {registered, Name, Node} ref resolves against its "
                    "origin node, not a same-named local process",
                    fun() -> registered_ref_is_node_qualified(PeerNode) end
                },
                {"allRegisteredOn: lists the target node's registered actors, node-qualified",
                    fun() -> all_registered_on_lists_target_node(PeerNode) end}
            ]
        end}}.

%%====================================================================
%% Anonymous remote spawn (spawnOn:)
%%====================================================================

anonymous_spawn_lands_on_target(PeerNode) ->
    register_counter_class(PeerNode),
    {ok, Pid} = beamtalk_actor:remote_spawn(PeerNode, 'Counter', undefined, 'spawnOn:'),
    ?assert(is_pid(Pid)),
    ?assertEqual(PeerNode, node(Pid)),
    try
        %% Genuinely responsive, not just alive — round-trips through the
        %% actor's own message loop on the target node.
        ?assertEqual(ok, beamtalk_actor:sync_send(Pid, 'setValue:', [7])),
        ?assertEqual(7, beamtalk_actor:sync_send(Pid, getValue, []))
    after
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.

%%====================================================================
%% Named remote spawn (spawnAs:on:) + named:on: round-trip
%%====================================================================

named_spawn_and_lookup_round_trip(PeerNode) ->
    register_counter_class(PeerNode),
    Name = unique_name("bt3599_named"),
    {ok, Pid} = beamtalk_actor:remote_spawn(PeerNode, 'Counter', Name, 'spawnAs:on:'),
    ?assert(is_pid(Pid)),
    ?assertEqual(PeerNode, node(Pid)),
    try
        %% ADR 0126 §3 / BT-3579 (a): the actor must survive the erpc
        %% worker's own (non-normal) exit — this is the unlink-after-start
        %% this ADR requires `remote_spawn_target/2` to perform.
        ?assert(rpc:call(PeerNode, erlang, is_process_alive, [Pid])),
        ?assertEqual(Pid, rpc:call(PeerNode, erlang, whereis, [Name])),

        %% named:on: finds it and returns a node-qualified proxy.
        {ok, Obj} = beamtalk_actor:remote_named(PeerNode, 'Counter', Name),
        ?assertMatch(#beamtalk_object{class = 'Counter', pid = {registered, Name, PeerNode}}, Obj),

        %% The proxy actually addresses the spawned actor.
        Ref = Obj#beamtalk_object.pid,
        ?assertEqual(ok, beamtalk_actor:sync_send(Ref, 'setValue:', [11])),
        ?assertEqual(11, beamtalk_actor:sync_send(Ref, getValue, [])),

        %% A retry after the name is taken fails with name_registered, not
        %% a crash — the documented non-idempotency recovery path.
        ?assertMatch(
            {error, #beamtalk_error{kind = name_registered}},
            beamtalk_actor:remote_spawn(PeerNode, 'Counter', Name, 'spawnAs:on:')
        )
    after
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.

%%====================================================================
%% node_down
%%====================================================================

node_down_on_unreachable_node() ->
    %% Never booted — same host as this node (so the §9 connect policy lets
    %% the attempt through without TLS) but nothing is listening under this
    %% name, so `erpc:call/5` fails with `{erpc, noconnection}`.
    {ok, Host} = inet:gethostname(),
    GhostNode = list_to_atom(
        "bt3599_ghost_" ++ integer_to_list(erlang:unique_integer([positive])) ++ "@" ++ Host
    ),
    ?assertMatch(
        {error, #beamtalk_error{kind = node_down}},
        beamtalk_actor:remote_spawn(GhostNode, 'Counter', undefined, 'spawnOn:')
    ),
    ?assertMatch(
        {error, #beamtalk_error{kind = node_down}},
        beamtalk_actor:remote_named(GhostNode, 'Counter', someName)
    ),
    ?assertMatch(
        {error, #beamtalk_error{kind = node_down}},
        beamtalk_actor:remote_all_registered(GhostNode)
    ).

%%====================================================================
%% class_not_found
%%====================================================================

class_not_found_for_unregistered_class(PeerNode) ->
    %% Deliberately never registered via register_counter_class/1 (or under
    %% any other name) on the peer.
    UnknownClass = unique_name("BT3599NoSuchClass"),
    ?assertMatch(
        {error, #beamtalk_error{kind = class_not_found, class = UnknownClass}},
        beamtalk_actor:remote_spawn(PeerNode, UnknownClass, undefined, 'spawnOn:')
    ).

%%====================================================================
%% Registered-ref node-qualification
%%====================================================================

-doc """
Proves the qualification actually routes by node, not just by name: two
actors registered under the *same* name on two *different* nodes, holding
distinguishable state. A node-qualified ref must reach the peer's actor
even though a same-named process also exists locally.
""".
registered_ref_is_node_qualified(PeerNode) ->
    Name = unique_name("bt3599_qualified"),
    {ok, LocalPid} = test_counter:start(-999),
    {ok, RemotePid} = rpc:call(PeerNode, test_counter, start, [42]),
    true = erlang:register(Name, LocalPid),
    true = rpc:call(PeerNode, erlang, register, [Name, RemotePid]),
    try
        %% A local (unqualified) two-tuple ref resolves against *this*
        %% node's registry.
        ?assertEqual(-999, beamtalk_actor:sync_send({registered, Name}, getValue, [])),
        %% The node-qualified three-tuple ref resolves against the peer's
        %% registry instead, even though the name is also locally bound.
        ?assertEqual(42, beamtalk_actor:sync_send({registered, Name, PeerNode}, getValue, [])),
        %% isRegistered/registeredName answer from the identity slot alone
        %% (no network hop) for both shapes.
        ?assertEqual(
            true, beamtalk_actor:sync_send({registered, Name, PeerNode}, isRegistered, [])
        ),
        ?assertEqual(
            Name, beamtalk_actor:sync_send({registered, Name, PeerNode}, registeredName, [])
        ),
        %% isAlive on the qualified ref genuinely checks the peer.
        ?assertEqual(true, beamtalk_actor:sync_send({registered, Name, PeerNode}, isAlive, []))
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid]),
        gen_server:stop(LocalPid)
    end.

%%====================================================================
%% allRegisteredOn:
%%====================================================================

all_registered_on_lists_target_node(PeerNode) ->
    register_counter_class(PeerNode),
    Name = unique_name("bt3599_allreg"),
    {ok, Pid} = beamtalk_actor:remote_spawn(PeerNode, 'Counter', Name, 'spawnAs:on:'),
    try
        {ok, List} = beamtalk_actor:remote_all_registered(PeerNode),
        ?assert(is_list(List)),
        ?assertMatch(
            [_ | _],
            [
                Obj
             || #beamtalk_object{pid = {registered, N, Node}} = Obj <- List,
                N =:= Name,
                Node =:= PeerNode
            ]
        )
    after
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.
