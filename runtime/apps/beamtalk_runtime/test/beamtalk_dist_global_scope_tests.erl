%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_global_scope_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Two-node EUnit tests for cluster-unique names, `scope: #global` (ADR 0126
§4, Phase 5, BT-3603): `beamtalk_actor_registry:'spawnAsGlobal'/2,3`, the `{global,
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

%% `supervisor` callback for the supervised heal-conflict tests (BT-5231).
-export([init/1]).
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%%====================================================================
%% Shared fixture (round-trip + reserved-name — no netsplit)
%%====================================================================

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3603_global"),
    %% `global` learns about a freshly connected node asynchronously; names
    %% registered before that exchange completes are not yet visible on the
    %% peer. Settle it before any test registers a name (BT-3938).
    ok = global:sync(),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

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
                },
                {
                    "named:scope:#global resolves a name registered on the PEER "
                    "node without crashing (review round 2: pid_class_name/1's "
                    "process_info read raised badarg for a cross-node pid)",
                    fun() -> named_scope_global_resolves_remote_registrant(PeerNode) end
                }
            ]
        end}}.

%% Standalone (no peer) — reserved-name rejection under #global scope is a
%% purely local check (spawn_named_scoped/4 rejects before ever touching
%% `global`), but kept in this suite per ADR 0126 §4's acceptance criteria.
reserved_name_rejected_under_global_scope_test() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = reserved_name}},
        beamtalk_actor_registry:'spawnAsGlobal'(logger, test_counter, 0)
    ).

%%====================================================================
%% Round trip from either node
%%====================================================================

round_trip_from_either_node(PeerNode) ->
    Name = beamtalk_test_unique:atom("bt3603_roundtrip"),
    {ok, Pid} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 0),
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
    Name = beamtalk_test_unique:atom("bt3603_scopesep"),
    %% Same name, both scopes, no conflict — they are different registries.
    {ok, LocalPid} = beamtalk_actor_registry:'spawnAs'(Name, test_counter, 111),
    {ok, GlobalPid} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 222),
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
%% named:scope:#global against a cross-node registrant (review round 2)
%%====================================================================

-doc """
`global:whereis_name/1` can resolve to a pid on any connected node — the
entire point of a cluster-unique name — but `named_lookup/4`'s `global`
branch originally read the candidate's class via the local-only
`pid_class_name/1` (`erlang:process_info/2`), which raises `badarg` for a
remote pid. Registers the actor ON THE PEER node specifically (rather than
this node, like `round_trip_from_either_node/1` does) so the lookup below
is guaranteed to resolve a genuinely remote pid, then looks it up FROM THIS
node — exactly the "spawn on A, `named:scope:#global` from B" scenario the
review flagged as the documented, headline use case for a cluster-unique
name.

`'Counter'` -> `test_counter` is registered in *this* node's
`beamtalk_class_metadata` (mirroring
`beamtalk_dist_remote_spawn_tests:register_counter_class/1`, which does the
same for the peer) since `named_lookup/4`'s `class_mod_for/1` check runs
locally, on whichever node calls it — here, this node.

`'spawnAsGlobal'/3` stays linked to its caller by design (ADR 0079/§4 — see
`beamtalk_dist_global_scope_tests`'s own netsplit-heal test for the same
note), so spawning it via a bare `rpc:call/4` would kill it the moment
`rpc`'s throwaway evaluation process exits; the explicit `unlink/1` here
mirrors that test's own workaround.
""".
named_scope_global_resolves_remote_registrant(PeerNode) ->
    Name = beamtalk_test_unique:atom("bt3603_remote_named"),
    %% Only claim the shared 'Counter' row when no real Counter is registered, and
    %% delete only a row inserted here: overwriting then deleting a live
    %% Counter's row broke later modules (hot_reload: class_not_found, BT-4962).
    InsertedRow = beamtalk_class_metadata:lookup_module('Counter') =:= not_found,
    case InsertedRow of
        true ->
            ok = beamtalk_class_metadata:insert('Counter', test_counter, undefined, none, false);
        false ->
            ok
    end,
    {ok, PeerPid} = rpc:call(PeerNode, erlang, apply, [
        fun() ->
            {ok, P} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 5),
            unlink(P),
            {ok, P}
        end,
        []
    ]),
    try
        ?assertNotEqual(node(), node(PeerPid)),
        %% Before the fix: badarg crash inside pid_class_name/1. After: a
        %% normal {ok, #beamtalk_object{}} carrying the {global, Name} ref.
        {ok, Obj} = beamtalk_actor_ffi:named_lookup(global, 'Counter', Name, 'named:scope:'),
        ?assertEqual('Counter', Obj#beamtalk_object.class),
        ?assertEqual({global, Name}, Obj#beamtalk_object.pid),
        %% The returned proxy actually resolves and dispatches to the
        %% peer-resident actor.
        ?assertEqual(5, beamtalk_actor:sync_send(Obj#beamtalk_object.pid, getValue, []))
    after
        %% Unlike beamtalk_dist_remote_spawn_tests:register_counter_class/1
        %% (which registers on the PEER's own table), this inserts into
        %% THIS node's shared beamtalk_class_metadata — a singleton ETS
        %% table other EUnit modules in the same VM also read — so it must
        %% be cleaned up here, mirroring beamtalk_class_metadata_tests.erl
        %% and beamtalk_supervisor_tests.erl's own after-block convention.
        InsertedRow andalso beamtalk_class_metadata:delete('Counter'),
        rpc:call(PeerNode, gen_server, stop, [PeerPid])
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
    %% The announcements bus (this test observes the loser's ActorStopped
    %% announcement) comes from the full runtime application (idempotent,
    %% never stopped). Starting it standalone via
    %% `beamtalk_announcements:start_link/0` made a later suite's
    %% `ensure_all_started(beamtalk_runtime)` fail with `already_started`
    %% when the dist suites run in isolation (BT-3938).
    {ok, _Started} = beamtalk_test_boot:start_app(beamtalk_runtime),
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
                    %% BT-5252: EUnit gives each test inside a group its own 5 s
                    %% default; the outer `{timeout, 60, ...}` bounds only the
                    %% group (see beamtalk_dist_test_helper's "EUnit timeouts").
                    %% A netsplit + heal + awaits overruns 5 s on a loaded runner.
                    {timeout, 60, fun() -> netsplit_heal_resolves_conflict(Peer, PeerNode) end}
                }
            ]
        end}}.

%% Simulate a netsplit that stays split until `heal/1` (BT-5252). A bare
%% `disconnect_node/1` is not enough: the peer's `global` (and anything else
%% that messages this node) auto-reconnects within milliseconds on a busy
%% runner, so the "partitioned" registrations can land on a re-formed mesh and
%% no conflict ever arises. Pointing this node's cookie for the peer at a value
%% the peer does not hold makes every reconnect handshake fail until `heal/1`
%% restores it. Then wait until the peer has processed the nodedown: `nodes()`
%% updates when the link drops, `global_name_server` handles its own `nodedown`
%% later from its mailbox, so a synchronous call to it (queued behind that
%% message) confirms it. `peer:call/4` uses the separate `standard_io` channel,
%% so it keeps working while distribution is down.
sever(Peer, PeerNode) ->
    PeerCookie = erlang:get_cookie(PeerNode),
    true = erlang:set_cookie(PeerNode, beamtalk_test_split_cookie),
    true = erlang:disconnect_node(PeerNode),
    ok = beamtalk_test_wait:until(fun() ->
        not lists:member(node(), peer:call(Peer, erlang, nodes, []))
    end),
    _ = peer:call(Peer, sys, get_state, [global_name_server]),
    PeerCookie.

%% Undo `sever/2`: restore the cookie it returned and reconnect.
heal(PeerNode, PeerCookie) ->
    true = erlang:set_cookie(PeerNode, PeerCookie),
    ok = beamtalk_test_wait:until(fun() -> net_adm:ping(PeerNode) =:= pong end).

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
    Name = beamtalk_test_unique:atom("bt3603_conflict"),

    %% Simulate a netsplit: sever ordinary distribution between the two
    %% nodes. Each side can now register the SAME global name independently
    %% (neither can see the other's global_name_server), exactly like two
    %% real partitions that both kept taking traffic.
    PeerCookie = sever(Peer, PeerNode),

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
            {ok, P} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 100),
            unlink(P),
            {ok, P}
        end,
        []
    ]),

    %% resolve_global_conflict/3 compares microsecond start-time markers:
    %% wait until this node's clock is past the peer registrant's marker, so
    %% the two registrants' ages are unambiguously ordered (no
    %% same-microsecond tie on a fast host).
    PeerStartedAt = peer:call(Peer, beamtalk_actor_registry, started_at_from_dictionary, [
        PeerPid
    ]),
    ?assert(is_integer(PeerStartedAt)),
    ok = beamtalk_test_wait:until(fun() ->
        erlang:system_time(microsecond) > PeerStartedAt
    end),

    %% Register the YOUNGER registrant here, on the origin node. Same
    %% link-then-unlink reasoning as PeerPid above — this test process would
    %% otherwise die when the conflict resolver later stops OriginPid with a
    %% non-`normal` reason (a linked, non-trapping process is killed by
    %% ANY non-`normal` exit signal from its link partner).
    {ok, OriginPid} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 200),
    unlink(OriginPid),
    OriginMon = erlang:monitor(process, OriginPid),

    %% Heal: reconnect distribution. `global`'s mesh sync now discovers
    %% `Name` registered on both (previously partitioned) sides and invokes
    %% the resolver both registrations were bound to,
    %% `beamtalk_actor_registry:resolve_global_conflict/3`.
    ok = heal(PeerNode, PeerCookie),
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
        begin
            ?awaitMessage({got, 'ActorStopped', #{pid := OriginPid} = Ev}),
            Ev
        end,
    ?assertEqual('globalNameConflict', maps:get(reason, Event)),

    %% Cleanup: stop the survivor via the peer's control channel (this node
    %% is reconnected to it now, so rpc:call/4 would also work, but the
    %% control channel is already at hand).
    ok = peer:call(Peer, gen_server, stop, [PeerPid]).

%%====================================================================
%% A supervised #global child that loses a heal conflict stays down (BT-5231)
%%====================================================================

-doc """
ADR 0126 BT-5193 amendment D1. A `one_for_one` supervisor (intensity 3 per
5 s) runs a sibling and a `{global, Name}` actor child started through
`beamtalk_supervisor:start_actor_child/3`. The peer registers `Name` first
during a netsplit; the heal stops this node's younger child with
`{shutdown, global_name_conflict}`. Before BT-5231 a `permanent` child's
restart answered `{error, _}` in a loop until the supervisor died with its
sibling; now the restart answers `ignore`, so the supervisor keeps the spec
with no pid. A `transient` child is not restarted; `restartChild` then answers
`ignore` (`Result ok: nil`) and logs the warning. Also run for a `native:`
child (`beamtalk_test_native_counter_facade`).
""".
supervised_global_child_heal_conflict_test_() ->
    {timeout, 120,
        {setup, fun setup_conflict/0, fun cleanup_conflict/1, fun({Peer, PeerNode}) ->
            [
                {
                    lists:flatten(io_lib:format("~p ~p child stays down", [Mod, Restart])),
                    %% Per-test timeout, as above (BT-5252).
                    {timeout, 60, fun() ->
                        supervised_child_loses_heal_conflict(Peer, PeerNode, Mod, Restart)
                    end}
                }
             || {Mod, Restart} <- [
                    {test_counter, permanent},
                    {test_counter, transient},
                    {beamtalk_test_native_counter_facade, permanent}
                ]
            ]
        end}}.

init(ChildSpecs) ->
    {ok, {#{strategy => one_for_one, intensity => 3, period => 5}, ChildSpecs}}.

child_args(test_counter) -> 0;
child_args(_NativeFacade) -> #{}.

supervised_child_loses_heal_conflict(Peer, PeerNode, Mod, Restart) ->
    _Sub = subscribe_self('ActorStopped'),
    Name = beamtalk_test_unique:atom("bt5231_supervised"),
    Args = child_args(Mod),
    PeerCookie = sever(Peer, PeerNode),
    %% The older registrant, on the peer (unlinked: see
    %% netsplit_heal_resolves_conflict/2).
    {ok, PeerPid} = peer:call(Peer, erlang, apply, [
        fun() ->
            {ok, P} = beamtalk_actor_registry:'spawnAsGlobal'(Name, Mod, Args),
            unlink(P),
            {ok, P}
        end,
        []
    ]),
    PeerStartedAt = peer:call(Peer, beamtalk_actor_registry, started_at_from_dictionary, [
        PeerPid
    ]),
    ?assert(is_integer(PeerStartedAt)),
    ok = beamtalk_test_wait:until(fun() -> erlang:system_time(microsecond) > PeerStartedAt end),
    {ok, Sup} = supervisor:start_link(?MODULE, [
        #{id => sibling, start => {beamtalk_supervisor, start_actor_child, [test_counter, 0]}},
        #{
            id => child,
            start => {beamtalk_supervisor, start_actor_child, [Mod, Args, {global, Name}]},
            restart => Restart
        }
    ]),
    %% The supervisor must not take this test process down if it dies; its
    %% death is what the assertions below catch.
    unlink(Sup),
    SupMon = erlang:monitor(process, Sup),
    Self = {beamtalk_supervisor, 'BT5231Sup', ?MODULE, Sup},
    try
        {ok, Sibling} = beamtalk_supervisor:childWithId(Self, sibling),
        {ok, Loser} = beamtalk_supervisor:childWithId(Self, child),
        LoserPid = pid_of(Loser),
        ?assertEqual(node(), node(LoserPid)),
        LoserMon = erlang:monitor(process, LoserPid),
        beamtalk_test_log:with_capture(warning, fun() ->
            %% Heal. The conflict is resolved when global:sync/0 returns.
            ok = heal(PeerNode, PeerCookie),
            ok = global:sync(),
            ?assertEqual(PeerPid, global:whereis_name(Name)),
            ?awaitMessage(
                {'DOWN', LoserMon, process, LoserPid, {shutdown, global_name_conflict}}
            ),
            ?awaitMessage({got, 'ActorStopped', #{pid := LoserPid, reason := globalNameConflict}}),
            case Restart of
                permanent ->
                    %% The supervisor restarts the child at once; the start
                    %% answers `ignore` and logs the warning (before the fix:
                    %% {error, _}, a restart loop and a dead supervisor).
                    ok = await_holder_warning(Name, PeerNode, SupMon);
                transient ->
                    %% `{shutdown, _}` is a normal exit for a transient child:
                    %% no restart, so no start and no warning yet.
                    ok
            end,
            %% The supervisor handles the child's EXIT independently of the
            %% DOWN/announcement above (no cross-process ordering), so wait
            %% for it to drop the pid rather than read it once (BT-5252).
            ok = beamtalk_test_wait:until(fun() -> child_pid(Sup, child) =:= undefined end),
            ?assertEqual(undefined, child_pid(Sup, child)),
            %% A later restartChild: retries; the name is still held, so
            %% the start is ignored again: `Result ok: nil`, not an error.
            ?assertEqual({ok, nil}, beamtalk_supervisor:restartChild(Self, child)),
            ok = await_holder_warning(Name, PeerNode, SupMon)
        end),
        %% No restart loop: the supervisor and the sibling are unaffected.
        ?assert(is_process_alive(Sup)),
        ?assertEqual(pid_of(Sibling), child_pid(Sup, sibling)),
        ?assertEqual(undefined, child_pid(Sup, child)),
        ?assertEqual(PeerPid, global:whereis_name(Name))
    after
        erlang:demonitor(SupMon, [flush]),
        is_process_alive(Sup) andalso beamtalk_test_proc:stop(Sup),
        ok = peer:call(Peer, gen_server, stop, [PeerPid])
    end.

pid_of(#beamtalk_object{pid = Pid}) -> Pid;
pid_of(Pid) when is_pid(Pid) -> Pid.

child_pid(Sup, Id) ->
    {Id, Pid, _Type, _Mods} = lists:keyfind(Id, 1, supervisor:which_children(Sup)),
    Pid.

%% The start's warning, naming the child and the holder's node. Fails at once
%% (not after a timeout) if the supervisor dies instead.
await_holder_warning(Name, PeerNode, SupMon) ->
    receive
        {beamtalk_test_log, #{level := warning, meta := #{child := Name} = Meta}} ->
            ?assertEqual(PeerNode, maps:get(holder_node, Meta)),
            ok;
        {'DOWN', SupMon, process, _, Reason} ->
            ?assertEqual(supervisor_alive, {supervisor_died, Reason})
    after 10000 ->
        ?assertEqual(warning_logged, timeout)
    end.

%%====================================================================
%% Unsupervised spawnAs:scope: #global against a remote holder (BT-5231)
%%====================================================================

unsupervised_global_spawn_keeps_name_registered_test_() ->
    {timeout, 60, fun() ->
        {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt5231_unsup"),
        try
            ok = global:sync(),
            Name = beamtalk_test_unique:atom("bt5231_unsup"),
            {ok, PeerPid} = rpc:call(PeerNode, erlang, apply, [
                fun() ->
                    {ok, P} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 0),
                    unlink(P),
                    {ok, P}
                end,
                []
            ]),
            ?assertEqual(PeerPid, global:whereis_name(Name)),
            ?assertMatch(
                {error, #beamtalk_error{kind = name_registered}},
                beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 0)
            )
        after
            beamtalk_dist_test_helper:stop_peer(Peer)
        end
    end}.

%%====================================================================
%% Register right after connect (BT-5156)
%%====================================================================

register_right_after_connect_test_() ->
    {timeout, 60, fun() ->
        {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt5156_connect"),
        try
            %% Drop the connection, then reconnect through `Node connect`
            %% and register at once, with no explicit `global:sync/0`.
            true = erlang:disconnect_node(PeerNode),
            ?assertMatch(
                {ok, _},
                beamtalk_result:to_tagged_tuple(
                    beamtalk_node:connect(beamtalk_node:from_atom(PeerNode))
                )
            ),
            Name = beamtalk_test_unique:atom("bt5156_afterconnect"),
            {ok, Pid} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 0),
            try
                ?assertEqual(Pid, rpc:call(PeerNode, global, whereis_name, [Name]))
            after
                gen_server:stop(Pid)
            end
        after
            beamtalk_dist_test_helper:stop_peer(Peer)
        end
    end}.

%%====================================================================
%% Connect to an already-connected node still syncs (BT-5179)
%%====================================================================

connect_after_raw_connect_sees_peer_global_name_test_() ->
    {timeout, 60, fun() ->
        {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt5179_connect"),
        try
            %% Drop the link, register a name on the peer alone, then
            %% reconnect through `net_kernel` (no explicit sync) so the
            %% node is already connected when `Node connect` runs.
            true = erlang:disconnect_node(PeerNode),
            Name = beamtalk_test_unique:atom("bt5179_peername"),
            {ok, PeerPid} = peer:call(
                Peer, beamtalk_actor_registry, 'spawnAsGlobal', [Name, test_counter, 0]
            ),
            true = net_kernel:connect_node(PeerNode),
            ?assert(lists:member(PeerNode, nodes(connected))),
            ?assertMatch(
                {ok, _},
                beamtalk_result:to_tagged_tuple(
                    beamtalk_node:connect(beamtalk_node:from_atom(PeerNode))
                )
            ),
            ?assertEqual(PeerPid, global:whereis_name(Name))
        after
            beamtalk_dist_test_helper:stop_peer(Peer)
        end
    end}.
