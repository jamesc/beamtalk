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
    %% Test 4 needs the announcements bus; use an already-running supervised
    %% one when there is one (the runtime app starts it under the
    %% supervisor), otherwise stand a fresh one up — mirrors
    %% beamtalk_announcements_tests:setup/0.
    case whereis(beamtalk_announcements) of
        undefined -> {ok, _} = beamtalk_announcements:start_link();
        _ -> ok
    end,
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
                    "stash_known_class/2 is lookup_class/1's fallback for a remote pid, "
                    "scoped to the stashed pid and cleared by clear_known_class/0",
                    fun() -> stash_known_class_is_scoped_lookup_class_fallback(PeerNode) end
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
%% beamtalk_actor:stash_known_class/2 + lookup_class/1 (ADR 0126 §7.3)
%%====================================================================

-doc """
Direct coverage of the class-hint mechanism `beamtalk_message_dispatch`
relies on (ADR 0126 §7.3): a genuinely remote pid is the only way to
observe `lookup_class/1`'s registry-miss fallback (a local pid always
resolves via the instance registry, stashed or not), so this needs the
real peer, not a mock.
""".
stash_known_class_is_scoped_lookup_class_fallback(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_counter, start, [0]),
    try
        %% No hint stashed yet: the local-only instance registry has
        %% nothing for a remote pid, so this is the pre-fix behavior this
        %% ADR fixes for callers that DO have the class.
        ?assertEqual(unknown, beamtalk_actor:lookup_class(RemotePid)),

        ok = beamtalk_actor:stash_known_class(RemotePid, 'Counter'),
        ?assertEqual('Counter', beamtalk_actor:lookup_class(RemotePid)),

        %% Scoped by pid: a hint for a *different* pid is never returned
        %% for this one — guards against a stale hint leaking across sends.
        {ok, OtherRemotePid} = rpc:call(PeerNode, test_counter, start, [0]),
        try
            ?assertEqual(unknown, beamtalk_actor:lookup_class(OtherRemotePid))
        after
            rpc:call(PeerNode, gen_server, stop, [OtherRemotePid])
        end,

        ok = beamtalk_actor:clear_known_class(),
        ?assertEqual(unknown, beamtalk_actor:lookup_class(RemotePid)),

        %% No-op for a local pid: stashing must never override (or risk
        %% masking) the instance registry, which is already authoritative.
        ok = beamtalk_actor:stash_known_class(self(), 'SomeClass'),
        ?assertEqual(unknown, beamtalk_actor:lookup_class(self()))
    after
        beamtalk_actor:clear_known_class(),
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
