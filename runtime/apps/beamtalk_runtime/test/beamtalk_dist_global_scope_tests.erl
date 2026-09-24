%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_global_scope_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Two-node EUnit tests for cluster-unique names, `scope: #global` (ADR 0126
§4, Phase 5, BT-3603): `beamtalk_actor:'spawnAsGlobal'/2,3`, the `{global,
Name}` ref shape, and `resolve_global_conflict/3` (the partition-heal
conflict resolver).

Reuses `beamtalk_dist_test_helper` (BT-3578) for the peer node — no second
harness — and the existing `test_counter` fixture (already forwarded to the
peer's code path), mirroring `beamtalk_dist_remote_spawn_tests`'s own level:
exercises `beamtalk_actor`'s public entry points directly rather than
through the `actor.bt` FFI shims (`doSpawnAsScope/3` etc.) — those shims are
thin argument plumbing (`resolve_scope/2` + `class_self_to_name_and_module/1`)
around exactly these functions, and `just build-stdlib`'s stdlib compile
already proves the `.bt` selectors parse and wire up to the right arity.

`scope: #local` behaving identically to the unscoped selectors and an
invalid scope Symbol raising `type_error` are FFI-shim-level concerns
(`resolve_scope/2`, reached only through `doSpawnAsScope/3` /
`doSpawnWithAsScope/4` / `doNamedScope/3`, which need a real class object to
exercise) — covered by the BUnit suite `ActorGlobalScopeTest`
(`stdlib/test/actor_global_scope_test.bt`) instead of duplicated here.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Shared fixture (round-trip + reserved-name — no netsplit)
%%====================================================================

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3603_global"),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

-doc "A short, run-unique atom, so repeated `just test` runs never collide on a stale name.".
unique_name(Prefix) ->
    list_to_atom(Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))).

global_scope_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {
                    "spawnAs:scope:#global + named:scope:#global-equivalent lookup "
                    "round-trip from either node",
                    fun() -> round_trip_from_either_node(PeerNode) end
                },
                {
                    "scope: #global and scope: #local are separate registries — no "
                    "cross-scope collision or visibility",
                    fun() -> local_and_global_scopes_are_separate(PeerNode) end
                }
            ]
        end}}.

%% Standalone (no peer) — reserved-name rejection under #global scope is a
%% purely local check (spawn_named_scoped/4 rejects before ever touching
%% `global`), but kept in this suite per ADR 0126 §4's acceptance criteria.
reserved_name_rejected_under_global_scope_test() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = reserved_name}},
        beamtalk_actor:'spawnAsGlobal'(logger, test_counter, 0)
    ).

%%====================================================================
%% Round trip from either node
%%====================================================================

round_trip_from_either_node(PeerNode) ->
    Name = unique_name("bt3603_roundtrip"),
    {ok, Pid} = beamtalk_actor:'spawnAsGlobal'(Name, test_counter, 0),
    try
        %% Visible immediately from this (origin) node...
        ?assertEqual(Pid, global:whereis_name(Name)),
        %% ...and from the peer node too — `global` is a single, cluster-wide
        %% registry (unlike `{registered, Name}`, no per-node qualification
        %% is needed or applied).
        ?assertEqual(Pid, rpc:call(PeerNode, global, whereis_name, [Name])),

        %% A `{global, Name}` ref sent from THIS node resolves and dispatches.
        Ref = {global, Name},
        ?assertEqual(ok, beamtalk_actor:sync_send(Ref, 'setValue:', [7])),
        ?assertEqual(7, beamtalk_actor:sync_send(Ref, getValue, [])),

        %% The same `{global, Name}` ref, sent from the PEER node's own
        %% process (erpc — runs ON PeerNode), resolves to the SAME actor —
        %% proves resolution is genuinely cluster-wide, not sender-relative.
        ?assertEqual(
            7, rpc:call(PeerNode, beamtalk_actor, sync_send, [Ref, getValue, []])
        ),
        ?assertEqual(
            ok, rpc:call(PeerNode, beamtalk_actor, sync_send, [Ref, 'setValue:', [9]])
        ),
        ?assertEqual(9, beamtalk_actor:sync_send(Ref, getValue, [])),

        %% isAlive/isRegistered/registeredName answer correctly for the
        %% `{global, Name}` shape too (async_send/4's matching clauses).
        ?assertEqual(true, beamtalk_actor:sync_send(Ref, isAlive, [])),
        ?assertEqual(true, beamtalk_actor:sync_send(Ref, isRegistered, [])),
        ?assertEqual(Name, beamtalk_actor:sync_send(Ref, registeredName, []))
    after
        gen_server:stop(Pid)
    end.

%%====================================================================
%% #local and #global are separate registries
%%====================================================================

local_and_global_scopes_are_separate(PeerNode) ->
    Name = unique_name("bt3603_scopesep"),
    %% Same name, both scopes, no conflict — they are different registries.
    {ok, LocalPid} = beamtalk_actor:'spawnAs'(Name, test_counter, 111),
    {ok, GlobalPid} = beamtalk_actor:'spawnAsGlobal'(Name, test_counter, 222),
    ?assertNotEqual(LocalPid, GlobalPid),
    try
        %% The local registration is invisible to `global` — separate
        %% registries entirely — while the global one resolves fine.
        ?assertEqual(GlobalPid, global:whereis_name(Name)),
        %% ...and, conversely, the peer node's plain `erlang:whereis/1` never
        %% sees this node's local registration (per-node by construction),
        %% while it DOES see the global one via a `global:whereis_name/1`
        %% call routed to the peer.
        ?assertEqual(undefined, rpc:call(PeerNode, erlang, whereis, [Name])),
        ?assertEqual(GlobalPid, rpc:call(PeerNode, global, whereis_name, [Name]))
    after
        gen_server:stop(LocalPid),
        gen_server:stop(GlobalPid)
    end.

%%====================================================================
%% Partition heal / conflict resolver (ADR 0126 §4)
%%====================================================================

setup_conflict() ->
    %% `standard_io` control channel: this test deliberately severs ordinary
    %% distribution between the two nodes (simulating a netsplit) and still
    %% needs to drive the peer afterwards — `peer:call/4` uses this separate
    %% channel, so it keeps working while distribution is down (see
    %% beamtalk_dist_test_helper:start_peer/2's doc).
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer(
        "bt3603_global_conflict", #{connection => standard_io}
    ),
    %% The announcements bus: reuse a supervised instance or stand one up
    %% (mirrors beamtalk_system_announcements_tests:setup/0) — this test
    %% observes the loser's ActorStopped announcement.
    case whereis(beamtalk_announcements) of
        undefined -> {ok, _} = beamtalk_announcements:start_link();
        _ -> ok
    end,
    {Peer, PeerNode}.

cleanup_conflict({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

netsplit_heal_conflict_test_() ->
    {timeout, 60,
        {setup, fun setup_conflict/0, fun cleanup_conflict/1, fun({Peer, PeerNode}) ->
            [
                {
                    "a simulated netsplit heal resolves a global name conflict to "
                    "the older registrant, gracefully STOPS the loser (not "
                    "exit(Pid, kill)), and its ActorStopped announcement carries "
                    "reason: #globalNameConflict",
                    fun() -> netsplit_heal_resolves_conflict(Peer, PeerNode) end
                }
            ]
        end}}.

%% Subscribe self() to ActorStopped, forwarding matching events back as
%% plain messages — mirrors beamtalk_system_announcements_tests.erl's
%% subscribe_self/1.
subscribe_self(EventClass) ->
    Collector = self(),
    Handler = fun(Event) -> Collector ! {got, EventClass, Event} end,
    {ok, SubRef} = beamtalk_announcements:subscribe(EventClass, self(), Handler, false),
    SubRef.

netsplit_heal_resolves_conflict(Peer, PeerNode) ->
    _Sub = subscribe_self('ActorStopped'),
    Name = unique_name("bt3603_conflict"),

    %% Simulate a netsplit: sever ordinary distribution between the two
    %% nodes. Each side can now register the SAME global name independently
    %% (neither can see the other's global_name_server), exactly like two
    %% real partitions that both kept taking traffic.
    true = erlang:disconnect_node(PeerNode),

    %% Register the OLDER registrant on the peer side first — via
    %% `peer:call/4`'s own control channel (ordinary `rpc:call/4` would fail;
    %% distribution to PeerNode is down). `'spawnAsGlobal'/3` stays LINKED
    %% to its caller by design (ADR 0079/§4: it doubles as the
    %% `SupervisionSpec withName:scope:` child start MFA, which needs that
    %% link) — `peer:call/4` runs the MFA in a throwaway worker process on
    %% the peer, so without an explicit unlink here (mirroring
    %% `remote_spawn_target/2`'s own unlink-after-start dance) PeerPid would
    %% die the moment that worker exits.
    {ok, PeerPid} = peer:call(Peer, erlang, apply, [
        fun() ->
            {ok, P} = beamtalk_actor:'spawnAsGlobal'(Name, test_counter, 100),
            unlink(P),
            {ok, P}
        end,
        []
    ]),

    %% Generous headroom (resolve_global_conflict/3 compares microsecond
    %% start-time markers) so the two registrants' ages are unambiguously
    %% ordered — this is not a tight race, just avoiding same-microsecond
    %% ties on a fast host.
    timer:sleep(50),

    %% Register the YOUNGER registrant here, on the origin node. Same
    %% link-then-unlink reasoning as PeerPid above — this test process would
    %% otherwise die when the conflict resolver later stops OriginPid with a
    %% non-`normal` reason (a linked, non-trapping process is killed by
    %% ANY non-`normal` exit signal from its link partner).
    {ok, OriginPid} = beamtalk_actor:'spawnAsGlobal'(Name, test_counter, 200),
    unlink(OriginPid),
    OriginMon = erlang:monitor(process, OriginPid),

    %% Heal: reconnect distribution. `global`'s mesh sync now discovers
    %% `Name` registered on both (previously partitioned) sides and invokes
    %% the resolver both registrations were bound to,
    %% `beamtalk_actor:resolve_global_conflict/3`.
    pong = net_adm:ping(PeerNode),
    %% Blocks until this node's global_name_server has fully synced with
    %% every currently-connected node — the conflict is resolved by the
    %% time this returns.
    ok = global:sync(),

    %% The OLDER registrant (the peer's, registered first) survives.
    ?assertEqual(PeerPid, global:whereis_name(Name)),

    %% The younger (origin's) actor was gracefully STOPPED, not killed: the
    %% DOWN reason is the specific `{shutdown, global_name_conflict}` tuple
    %% `resolve_global_conflict/3` uses — never bare `killed`, which is what
    %% OTP's own default resolver (`random_exit_name/3`) would have produced.
    DownReason =
        receive
            {'DOWN', OriginMon, process, OriginPid, Reason} -> Reason
        after 10000 ->
            erlang:demonitor(OriginMon, [flush]),
            timeout
        end,
    ?assertEqual({shutdown, global_name_conflict}, DownReason),

    %% A graceful `gen_server:stop/3` (unlike a kill) always runs
    %% `terminate/2` — confirmed here via the ActorStopped announcement it
    %% publishes, whose `reason` is the dedicated `#globalNameConflict`
    %% Symbol (`normalize_stop_reason/1`), distinct from a plain `#shutdown`.
    Event =
        receive
            {got, 'ActorStopped', #{pid := OriginPid} = Ev} -> Ev
        after 5000 ->
            ?assert(false)
        end,
    ?assertEqual('globalNameConflict', maps:get(reason, Event)),

    %% Cleanup: stop the survivor via the peer's control channel (this node
    %% is reconnected to it now, so rpc:call/4 would also work, but the
    %% control channel is already at hand).
    ok = peer:call(Peer, gen_server, stop, [PeerPid]).
