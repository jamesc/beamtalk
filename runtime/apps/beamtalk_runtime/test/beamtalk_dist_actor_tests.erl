%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_actor_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Two-node EUnit tests for remote-safe actor sends (ADR 0126 Phase 0 /
BT-3578), using the `beamtalk_dist_test_helper` harness to boot a real
`peer`-booted second node rather than mocking distribution.

Covers the four scenarios BT-3578's acceptance criteria call out:

1. A sync send to an actor spawned on the peer through raw FFI/erpc
   returns the right value (before this change it crashed with `badarg`
   inside `is_process_alive/1`).
2. Killing the peer node mid-call raises `node_down`, not `actor_dead`.
3. A sync send to a dead remote pid on a live node raises `actor_dead`.
4. Announcements still deliver to remote subscribers (regression check
   for BT-2530).

Plus one for the harness itself (BT-3929):

5. A dropped distribution connection leaves the peer alive and the next
   call auto-connects — the `standard_io` control channel
   `beamtalk_dist_test_helper:start_peer/2` now defaults to.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% Exported so it can be spawned remotely via
%% `erlang:spawn(Mod, Fun, Args)` (an MFA spawn, not a fun closure, so it
%% doesn't depend on fun-term serialization across nodes — just on this
%% module being on the peer's code path, which beamtalk_dist_test_helper
%% guarantees by forwarding the full local code path).
-export([remote_subscriber_loop/1]).

%%====================================================================
%% Shared fixture (tests 1, 3, 4 — none of them kill the peer node)
%%====================================================================

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3578_shared"),
    %% Test 4 needs the announcements bus, which comes from the full runtime
    %% application (idempotent, never stopped). Starting it standalone via
    %% `beamtalk_announcements:start_link/0` made a later suite's
    %% `ensure_all_started(beamtalk_runtime)` fail with `already_started`
    %% when the dist suites run in isolation (BT-3938).
    {ok, _Started} = application:ensure_all_started(beamtalk_runtime),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

remote_safe_sends_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {
                    "sync send to a peer-spawned actor returns the right value "
                    "(pre-fix: badarg in is_process_alive/1)",
                    fun() -> sync_send_to_peer_actor_returns_value(PeerNode) end
                },
                {"sync send to a dead remote pid on a live node raises actor_dead", fun() ->
                    sync_send_to_dead_remote_pid_raises_actor_dead(PeerNode)
                end},
                {"announcements still deliver to remote subscribers (BT-2530 regression)", fun() ->
                    announcements_deliver_to_remote_subscriber(PeerNode)
                end},
                {
                    "a send that carries the class (sync_send_known/4) reports it for a "
                    "remote pid, which lookup_class/1 cannot resolve",
                    fun() -> known_class_send_reports_class_for_remote_pid(PeerNode) end
                }
            ]
        end}}.

%%====================================================================
%% 1. Sync send to a peer-spawned actor
%%====================================================================

sync_send_to_peer_actor_returns_value(PeerNode) ->
    %% Raw FFI/erpc spawn: gen_server:start/3 on the peer node, via rpc,
    %% exactly the shape a beamtalk actor's generated spawn takes (no
    %% beamtalk-specific spawn plumbing involved — this pins the send
    %% path, not the spawn path).
    {ok, RemotePid} = rpc:call(PeerNode, test_counter, start, [10]),
    ?assert(is_pid(RemotePid)),
    ?assertEqual(PeerNode, node(RemotePid)),
    try
        %% Before this ADR, is_process_alive(RemotePid) inside sync_send
        %% raised badarg for a remote pid — this would crash, not return 10.
        ?assertEqual(10, beamtalk_actor:sync_send(RemotePid, getValue, [])),
        ok = beamtalk_actor:cast_send(RemotePid, increment, []),
        %% cast is fire-and-forget; poll briefly rather than a fixed sleep.
        ?assertEqual(11, wait_for_value(RemotePid, 11, 20))
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

wait_for_value(RemotePid, Expected, Retries) ->
    case beamtalk_actor:sync_send(RemotePid, getValue, []) of
        Expected ->
            Expected;
        Other when Retries =< 0 ->
            Other;
        _ ->
            timer:sleep(50),
            wait_for_value(RemotePid, Expected, Retries - 1)
    end.

%%====================================================================
%% 3. Sync send to a dead remote pid on a live node
%%====================================================================

sync_send_to_dead_remote_pid_raises_actor_dead(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_counter, start, [0]),
    ?assert(is_pid(RemotePid)),
    ok = rpc:call(PeerNode, gen_server, stop, [RemotePid]),
    %% Give the peer a moment to actually reap the process before we probe
    %% it — gen_server:stop/1 is synchronous on the peer, but the rpc reply
    %% racing the process table update is not something we want to also
    %% be testing here.
    timer:sleep(50),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = actor_dead}},
        beamtalk_actor:sync_send(RemotePid, getValue, [])
    ).

%%====================================================================
%% 4. Announcements deliver to remote subscribers (BT-2530 regression)
%%====================================================================

announcements_deliver_to_remote_subscriber(PeerNode) ->
    RemoteSub = rpc:call(PeerNode, erlang, spawn, [?MODULE, remote_subscriber_loop, [self()]]),
    ?assert(is_pid(RemoteSub)),
    try
        {ok, SubRef} = beamtalk_announcements:subscribe(
            'BT3578RemoteEvent', RemoteSub, remote_h, false
        ),
        ?assert(beamtalk_announcements:is_active(SubRef)),
        ok = beamtalk_announcements:announce('BT3578RemoteEvent', {payload, 1}),
        receive
            {remote_received, RemoteSub, SubRef, 'BT3578RemoteEvent', remote_h, {payload, 1}} ->
                ok
        after 2000 ->
            ?assert(false)
        end
    after
        RemoteSub ! stop
    end.

-doc "Forwards every received announcement back to Collector via a plain message.".
-spec remote_subscriber_loop(pid()) -> ok.
remote_subscriber_loop(Collector) ->
    receive
        {beamtalk_announcement, SubRef, EventClass, Handler, Event} ->
            Collector ! {remote_received, self(), SubRef, EventClass, Handler, Event},
            remote_subscriber_loop(Collector);
        stop ->
            ok
    end.

%%====================================================================
%% beamtalk_actor:sync_send_known/4 for a remote pid (ADR 0126 §7.3, BT-4113)
%%====================================================================

-doc """
A remote pid's class is not in the local-only instance registry, so
`lookup_class/1` answers `unknown`; `beamtalk_message_dispatch` passes the
`#beamtalk_object{}` record's class with the send instead (BT-4113, which
replaced the per-send process-dictionary stash). The dispatch event
metadata shows which class the send reported.
""".
known_class_send_reports_class_for_remote_pid(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_counter, start, [7]),
    Self = self(),
    HandlerId = {?MODULE, known_class, make_ref()},
    ok = telemetry:attach(
        HandlerId,
        [beamtalk, actor, dispatch, stop],
        fun(_E, _M, Meta, #{dest := Dest}) -> Dest ! {dispatch_stop, Meta} end,
        #{dest => Self}
    ),
    ok = beamtalk_telemetry:enable_dispatch_events(),
    try
        ?assertEqual(unknown, beamtalk_actor:lookup_class(RemotePid)),
        ?assertEqual(7, beamtalk_actor:sync_send_known(RemotePid, 'Counter', getValue, [])),
        receive
            {dispatch_stop, #{pid := RemotePid} = Meta} ->
                ?assertEqual('Counter', maps:get(class, Meta))
        after 2000 ->
            ?assert(false)
        end,
        %% Without the class, the send reports `unknown`.
        ?assertEqual(7, beamtalk_actor:sync_send(RemotePid, getValue, [])),
        receive
            {dispatch_stop, #{pid := RemotePid} = Meta2} ->
                ?assertEqual(unknown, maps:get(class, Meta2))
        after 2000 ->
            ?assert(false)
        end
    after
        ok = beamtalk_telemetry:disable_dispatch_events(),
        telemetry:detach(HandlerId),
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% 2. Killing the peer node mid-call raises node_down
%%====================================================================

node_down_on_killed_peer_test_() ->
    {timeout, 30,
        ?_test(begin
            {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3578_nodedown"),
            try
                {ok, RemotePid} = rpc:call(PeerNode, test_counter, start, [0]),
                ?assert(is_pid(RemotePid)),
                %% Kill the peer node partway through a slow call so the
                %% gen_server:call is genuinely in flight when the
                %% partition happens, mirroring a real mid-call node loss
                %% rather than a pre-emptively-dead node.
                spawn(fun() ->
                    timer:sleep(300),
                    beamtalk_dist_test_helper:stop_peer(Peer)
                end),
                try
                    Result = beamtalk_actor:sync_send(RemotePid, 'slowGet:', [5000]),
                    erlang:error({unexpected_success, Result})
                catch
                    error:#{
                        '$beamtalk_class' := _,
                        error := #beamtalk_error{kind = Kind, details = Details}
                    } ->
                        ?assertEqual(node_down, Kind),
                        ?assertMatch(#{node := PeerNode}, Details)
                end
            after
                %% Already down by now in the normal case; stop_peer/1 is
                %% safe to call again (idempotent — see its doc).
                beamtalk_dist_test_helper:stop_peer(Peer)
            end
        end)}.

%%====================================================================
%% 5. A dropped distribution connection does not take the peer with it
%%    (BT-3929 — beamtalk_dist_test_helper:start_peer/2's `standard_io`
%%    default)
%%====================================================================

peer_survives_dropped_connection_test_() ->
    {timeout, 30,
        ?_test(begin
            {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3929_dropped"),
            try
                ok = net_kernel:monitor_nodes(true),
                ?assert(lists:member(PeerNode, nodes())),
                %% Sever ordinary distribution — what a connection the
                %% emulator tears down looks like from here. With `peer`'s
                %% distribution-based control channel this halted the peer,
                %% so every later call in the fixture raised node_down.
                true = erlang:disconnect_node(PeerNode),
                receive
                    {nodedown, PeerNode} -> ok
                after 5000 -> erlang:error({no_nodedown, PeerNode})
                end,
                ?assertNot(lists:member(PeerNode, nodes())),
                %% The next call auto-connects, and the peer is still there
                %% to answer it.
                ?assertEqual(PeerNode, rpc:call(PeerNode, erlang, node, [])),
                ?assert(lists:member(PeerNode, nodes()))
            after
                net_kernel:monitor_nodes(false),
                beamtalk_dist_test_helper:stop_peer(Peer)
            end
        end)}.
