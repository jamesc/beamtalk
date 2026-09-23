%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_node_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_node` (the `Node` value class backing) and
`beamtalk_node_monitor` (`NodeUp`/`NodeDown` cluster events), ADR 0126
Phase 1 / BT-3598.

Single-node tests pin `Node named:` validation, the §9 item 2
`insecure_distribution` connect policy and `nodedown_reason`
normalisation. The policy is tested through `connect_policy/2` and through
`connect/1`/`ping/1` against a TEST-NET-1 IP-literal host (`192.0.2.1`,
RFC 5737 — resolves without DNS and is never a local address), with TLS
distribution toggled as a parameter: standing up real `inet_tls`
distribution in CI (certificates, `ssl_dist` config) is out of proportion
to what the guard decides, which is purely "is this host local, and is
TLS on".

Two-node tests use the BT-3578 `peer` harness
(`beamtalk_dist_test_helper`): `Node connected` after a peer connects,
`NodeUp`/`NodeDown` firing on connect/disconnect (hidden nodes silent),
and `isRemote`/`node` on an actor spawned on the peer.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% RFC 5737 TEST-NET-1: an IP literal that is never a local address.
-define(OFF_HOST_NODE, 'worker@192.0.2.1').

%%====================================================================
%% Node named:
%%====================================================================

named_valid_test() ->
    Result = beamtalk_node:named('worker@localhost'),
    ?assertMatch(#{'$beamtalk_class' := 'Result', isOk := true}, Result),
    ?assertEqual(
        #{'$beamtalk_class' => 'Node', name => 'worker@localhost'},
        maps:get(okValue, Result)
    ).

named_malformed_test_() ->
    [
        ?_assertEqual(invalid_node_name, named_error_kind(Name))
     || Name <- [nohost, '@host', 'name@', 'a@b@c', 'bad name@host', 'name@ho st', '']
    ].

named_malformed_carries_hint_test() ->
    #{errReason := #{error := Error}} = beamtalk_node:named(nohost),
    ?assertEqual(<<"expected name@host">>, Error#beamtalk_error.hint),
    ?assertEqual(#{name => nohost}, Error#beamtalk_error.details).

named_non_symbol_raises_type_error_test() ->
    ?assertError(#{error := #beamtalk_error{kind = type_error}}, beamtalk_node:named(<<"a@b">>)).

named_error_kind(Name) ->
    #{isOk := false, errReason := #{error := #beamtalk_error{kind = Kind}}} =
        beamtalk_node:named(Name),
    Kind.

%%====================================================================
%% Value semantics
%%====================================================================

value_equality_is_name_equality_test() ->
    ?assertEqual(beamtalk_node:from_atom('a@h'), beamtalk_node:from_atom('a@h')),
    ?assertNotEqual(beamtalk_node:from_atom('a@h'), beamtalk_node:from_atom('b@h')).

current_and_pid_node_test() ->
    Current = beamtalk_node:current(),
    ?assertEqual(node(), beamtalk_node:name(Current)),
    ?assert(beamtalk_node:isCurrent(Current)),
    ?assertEqual(Current, beamtalk_node:ofPid(self())).

current_node_is_trivially_reachable_test() ->
    Current = beamtalk_node:current(),
    ?assert(beamtalk_node:ping(Current)),
    ?assert(beamtalk_node:isConnected(Current)),
    ?assertNot(beamtalk_node:disconnect(Current)),
    ?assertEqual(
        #{'$beamtalk_class' => 'Result', isOk => true, okValue => Current, errReason => nil},
        beamtalk_node:connect(Current)
    ).

print_string_test() ->
    ?assertEqual(
        <<"Node(worker@localhost)">>,
        beamtalk_node:printString(beamtalk_node:from_atom('worker@localhost'))
    ).

%%====================================================================
%% §9 item 2: insecure_distribution connect policy
%%====================================================================

policy_allows_loopback_hosts_without_tls_test_() ->
    [
        ?_assertEqual(ok, beamtalk_node:connect_policy(Node, false))
     || Node <- ['w@localhost', 'w@LOCALHOST', 'w@127.0.0.1', 'w@127.1.2.3', 'w@::1']
    ].

policy_allows_this_hosts_own_name_without_tls_test() ->
    {ok, Host} = inet:gethostname(),
    ?assertEqual(ok, beamtalk_node:connect_policy(list_to_atom("w@" ++ Host), false)).

policy_refuses_off_host_without_tls_test() ->
    {error, Error} = beamtalk_node:connect_policy(?OFF_HOST_NODE, false),
    ?assertEqual(insecure_distribution, Error#beamtalk_error.kind),
    ?assertEqual(
        <<"configure TLS distribution (ADR 0091) or use a tunnel">>,
        Error#beamtalk_error.hint
    ),
    ?assertEqual(?OFF_HOST_NODE, maps:get(node, Error#beamtalk_error.details)).

policy_allows_off_host_under_tls_test() ->
    ?assertEqual(ok, beamtalk_node:connect_policy(?OFF_HOST_NODE, true)).

policy_refuses_unresolvable_host_without_tls_test() ->
    %% `.invalid` is reserved (RFC 2606) and never resolves, so the host
    %% cannot be proven local.
    ?assertMatch(
        {error, #beamtalk_error{kind = insecure_distribution}},
        beamtalk_node:connect_policy('w@no-such-host.invalid', false)
    ).

%% This VM is not started with `-proto_dist inet_tls`, so the public
%% selectors take the non-TLS branch of the policy — and refuse *before*
%% touching the network.
tls_distribution_is_off_in_tests_test() ->
    ?assertNot(beamtalk_node:tls_distribution()).

connect_off_host_without_tls_is_insecure_distribution_test() ->
    Result = beamtalk_node:connect(beamtalk_node:from_atom(?OFF_HOST_NODE)),
    ?assertMatch(
        #{isOk := false, errReason := #{error := #beamtalk_error{kind = insecure_distribution}}},
        Result
    ).

ping_off_host_without_tls_is_false_test() ->
    ?assertNot(beamtalk_node:ping(beamtalk_node:from_atom(?OFF_HOST_NODE))).

%%====================================================================
%% beamtalk_node_monitor: nodedown_reason normalisation
%%====================================================================

normalize_reason_test_() ->
    [
        ?_assertEqual(disconnect, beamtalk_node_monitor:normalize_reason(disconnect)),
        ?_assertEqual(
            net_tick_timeout, beamtalk_node_monitor:normalize_reason(net_tick_timeout)
        ),
        ?_assertEqual(unknown, beamtalk_node_monitor:normalize_reason(undefined)),
        ?_assertEqual(unknown, beamtalk_node_monitor:normalize_reason({weird, term}))
    ].

%%====================================================================
%% Two-node: Node connected, NodeUp / NodeDown, remote actors
%%====================================================================

setup() ->
    beamtalk_dist_test_helper:ensure_distribution(),
    %% Stand up the announcements bus and the node monitor only if the
    %% runtime supervisor isn't already running them, and stop exactly what
    %% this suite started in cleanup — so a later
    %% `beamtalk_runtime_sup_tests:all_children_alive_test/0` never finds a
    %% stray registered name.
    Bus =
        case whereis(beamtalk_announcements) of
            undefined ->
                {ok, BusPid} = beamtalk_announcements:start_link(),
                unlink(BusPid),
                [BusPid];
            _ ->
                []
        end,
    Monitor =
        case whereis(beamtalk_node_monitor) of
            undefined ->
                {ok, MonitorPid} = gen_server:start(
                    {local, beamtalk_node_monitor}, beamtalk_node_monitor, [], []
                ),
                [MonitorPid];
            _ ->
                []
        end,
    Monitor ++ Bus.

cleanup(Started) ->
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, Started).

two_node_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(_) ->
            [
                {"NodeUp fires and Node connected lists the peer once it connects",
                    fun node_up_and_connected/0},
                {"NodeDown fires on disconnect; connect re-establishes (NodeUp again)",
                    fun node_down_then_reconnect/0},
                {"a hidden peer never announces and is not in Node connected",
                    fun hidden_peer_is_silent/0},
                {"isRemote/node on an actor spawned on the peer",
                    fun remote_actor_node_and_is_remote/0}
            ]
        end}}.

node_up_and_connected() ->
    Up = subscribe_self('NodeUp'),
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3598_up"),
    try
        Event = expect_event('NodeUp', PeerNode),
        ?assertEqual(beamtalk_node:from_atom(PeerNode), maps:get(node, Event)),
        PeerValue = beamtalk_node:from_atom(PeerNode),
        ?assert(lists:member(PeerValue, beamtalk_node:connected())),
        ?assertNot(lists:member(beamtalk_node:current(), beamtalk_node:connected())),
        ?assert(beamtalk_node:isConnected(PeerValue)),
        ?assertNot(beamtalk_node:isCurrent(PeerValue)),
        ?assert(beamtalk_node:ping(PeerValue))
    after
        beamtalk_announcements:unsubscribe(Up),
        beamtalk_dist_test_helper:stop_peer(Peer)
    end.

node_down_then_reconnect() ->
    %% standard_io control channel: the default distribution-based one would
    %% halt the peer the moment we disconnect from it.
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer(
        "bt3598_down", #{connection => standard_io}
    ),
    Up = subscribe_self('NodeUp'),
    Down = subscribe_self('NodeDown'),
    try
        PeerValue = beamtalk_node:from_atom(PeerNode),
        ?assert(beamtalk_node:disconnect(PeerValue)),
        DownEvent = expect_event('NodeDown', PeerNode),
        ?assertEqual(PeerValue, maps:get(node, DownEvent)),
        ?assertEqual(disconnect, maps:get(reason, DownEvent)),
        ?assertNot(lists:member(PeerValue, beamtalk_node:connected())),
        %% Same host as this node, so the §9 policy lets it through without TLS.
        ?assertMatch(#{isOk := true, okValue := PeerValue}, beamtalk_node:connect(PeerValue)),
        _ = expect_event('NodeUp', PeerNode),
        ?assert(lists:member(PeerValue, beamtalk_node:connected()))
    after
        beamtalk_announcements:unsubscribe(Up),
        beamtalk_announcements:unsubscribe(Down),
        beamtalk_dist_test_helper:stop_peer(Peer)
    end.

hidden_peer_is_silent() ->
    Up = subscribe_self('NodeUp'),
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer(
        "bt3598_hidden", #{extra_args => ["-hidden"]}
    ),
    try
        ?assert(lists:member(PeerNode, nodes(hidden))),
        ?assertNot(lists:member(beamtalk_node:from_atom(PeerNode), beamtalk_node:connected())),
        receive
            {got, 'NodeUp', #{node := #{name := PeerNode}}} -> ?assert(false)
        after 500 -> ok
        end
    after
        beamtalk_announcements:unsubscribe(Up),
        beamtalk_dist_test_helper:stop_peer(Peer)
    end.

remote_actor_node_and_is_remote() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3598_actor"),
    {ok, LocalPid} = test_counter:start(0),
    try
        {ok, RemotePid} = rpc:call(PeerNode, test_counter, start, [0]),
        ?assertEqual(
            beamtalk_node:from_atom(PeerNode), beamtalk_actor:sync_send(RemotePid, node, [])
        ),
        %% isRemote is answered in the caller's process, not the actor's —
        %% the actor itself would always see its own node.
        ?assert(beamtalk_actor:sync_send(RemotePid, isRemote, [])),
        ?assert(beamtalk_actor:sync_send(RemotePid, isRemote, [], 5000)),
        ?assertNot(beamtalk_actor:sync_send(LocalPid, isRemote, [])),
        %% An actor with no `node` method (DNU) falls back to the pid's node
        %% rather than crashing isRemote.
        {ok, NoNodePid} = rpc:call(PeerNode, test_wirecheck_actor, start, [0]),
        ?assert(beamtalk_actor:sync_send(NoNodePid, isRemote, [])),
        ?assertEqual(
            beamtalk_node:from_atom(PeerNode), beamtalk_node:ofPid(RemotePid)
        )
    after
        gen_server:stop(LocalPid),
        beamtalk_dist_test_helper:stop_peer(Peer)
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% Subscribe `self()` to a system event class with a fun handler that
%% forwards each payload back to this process (mirrors
%% beamtalk_system_announcements_tests:subscribe_self/1).
subscribe_self(EventClass) ->
    Collector = self(),
    Handler = fun(Event) -> Collector ! {got, EventClass, Event} end,
    {ok, SubRef} = beamtalk_announcements:subscribe(EventClass, self(), Handler, false),
    SubRef.

%% Wait for the `EventClass` event about `PeerNode` specifically — other
%% suites' peers may come and go on the shared system bus concurrently.
expect_event(EventClass, PeerNode) ->
    receive
        {got, EventClass, #{node := #{name := PeerNode}} = Event} -> Event
    after 10000 ->
        ?assert(false)
    end.
