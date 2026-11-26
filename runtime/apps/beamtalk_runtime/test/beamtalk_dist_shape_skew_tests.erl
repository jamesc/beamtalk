%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_shape_skew_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
Two-node `NodeShapeSkew` suite (ADR 0126 §5.3, Phase 4, BT-3602).

Exercises `beamtalk_node_monitor`'s connect-time and reload-time
shape-manifest comparison over a real `peer` node (BT-3578 harness,
`beamtalk_dist_test_helper`).

A genuine *second compiled version* of a class is not available here: the
compiler ties a `.bt` file's name to its declared class name 1:1 (`shape_skew_cart.bt`
can only ever declare `ShapeSkewCart`), so two files cannot both declare the
same class under different `shapeVersion:`s. Instead, each "version skew"
scenario registers the real, compiled fixture at its implicit `shapeVersion:
1` on both nodes, then repoints the class's registered module — on
whichever side the scenario needs skewed — at a
`beamtalk_shape_migration_tests:build_delegate_proxy/3` stub (the same
technique `beamtalk_dist_wire_tests.erl`'s `install_meta_stub/2` uses for
wire-*decode* testing, reused rather than a second forms-and-compile copy —
AGENTS.md's no-duplicate-implementations rule) whose `'__beamtalk_meta'/0`
reports a different `shape_version`. Unlike that helper's own stub name
(`bt_..._meta_stub`, no `@`), every stub built here keeps the `bt@` prefix
`beamtalk_module_activation:is_user_class_module/1` requires — the stub
name feeds straight into `beamtalk_release:shape_manifest/0`'s
project-class filter via `beamtalk_class_registry:live_class_entries/0`,
and a `bt_...`-style name would silently drop the class from the manifest
instead of skewing its version. The metadata swap
(`beamtalk_class_metadata:merge_identity/5`) is a real change —
`beamtalk_release_shapes:class_shape_entry/1` genuinely reports the new
version afterward, exactly as it would for a real second module — so a
scenario needing a `ClassLoaded` announcement (the reload one) fires it
explicitly after the swap, the same direct `system_announce/2` technique
`beamtalk_system_announcements_tests.erl` already uses.

Three scenarios, each its own explicit disconnect/reconnect cycle
(`standard_io` peer connection, matching `beamtalk_node_tests.erl`'s
`node_down_then_reconnect` precedent) so each gets a fresh, deterministic
`nodeup` to observe — `start_peer/2`'s own implicit connect already fired
and consumed itself before any fixture is registered:

- `skew_announced_once_on_connect/1` — a shared class at different
  versions on each side announces exactly one `NodeShapeSkew` on connect.
- `no_skew_for_matching_or_non_shared/1` — a shared class at the *same*
  version, and a class registered on only one side, announce nothing.
- `reload_reannounces_skew/1` — matching versions on connect (no skew),
  then a local version swap + `ClassLoaded` re-announces.
- `peer_reload_updates_both_tallies/2` (BT-5232) — a reload on the *peer*
  announces `NodeShapeSkew` and moves `skew_count` on both nodes, and a
  reload back to a matching shape brings both counts to 0.

The suite also covers `NodeVersionSkew` (BT-5232, ADR 0126 Amendment D3):
the peer's `beamtalk_version:dist_versions/0` is faked with `meck` on the
peer, so the local connect-time check sees a mismatch or a failed read.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

-export([
    load_fixture/2,
    load_fixture_settled/2,
    install_shape_version_stub/3,
    reload_with_shape_version/3,
    reload_to_real_module/2,
    forward_events/2,
    connect_checks_running/0,
    fake_dist_versions/1,
    unfake_dist_versions/0,
    local_gen/1,
    peer_gen_of/1,
    reload_seq_of/2,
    suspend_monitor/0,
    resume_monitor/0,
    stop_monitor/0,
    start_monitor/0,
    restart_monitor/0,
    ensure_shape_version_stub/4
]).

%%====================================================================
%% Shared fixture
%%====================================================================

-doc """
This suite's fixtures are real `Actor subclass:` classes, so the local side
needs the full `beamtalk_runtime` application up (bootstrap's core stdlib
classes, the announcements bus, the node monitor) — not just the two bare
gen_servers `beamtalk_node_tests.erl`'s own `setup/0` starts standalone,
which is enough for *that* suite's non-Beamtalk-class fixtures but not for
`register_class/0` against an `Actor` superclass that must already be
registered. `application:ensure_all_started/1` is idempotent — a no-op
when an earlier suite in the same `rebar3 eunit` run already started it —
so this passes standalone
(`rebar3 eunit --module=beamtalk_dist_shape_skew_tests`) and as part of the
full app run alike, without a partial-manual-start/full-app-start
conflict. Started applications are never stopped in `cleanup/1`: the app
is meant to keep running for the rest of the `rebar3 eunit` session, the
same as any other suite that is first to bring it up. The peer always gets
its own copy via `start_peer/2`'s identical call.
""".
setup() ->
    {ok, _Started} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer(
        "bt3602_skew", #{connection => standard_io}
    ),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

-doc """
Load `Basename`'s compiled fixture module and register it under
`ClassName` if not already registered — idempotent, the same convention
`beamtalk_dist_wire_tests:ensure_fixture_loaded/2` documents its own copy
of (and calls out as shared across `beamtalk_shape_migration_tests.erl`/
`beamtalk_wire_tests.erl` too). Runs on whichever node calls it — used both
locally and via `erpc:call/5` on the peer.
""".
-spec load_fixture(atom(), atom()) -> ok.
load_fixture(Basename, ClassName) ->
    beamtalk_test_class:ensure_fixture_loaded(
        ClassName, list_to_atom("bt@" ++ atom_to_list(Basename))
    ).

-doc """
Repoint `ClassName`'s registered module (real, at this point — `load_fixture/2`
must already have registered it) at a fresh `build_delegate_proxy/3` stub
whose `'__beamtalk_meta'/0` reports `shape_version => ShapeVersion` instead
of the real module's own — see the moduledoc for why this stands in for a
second compiled `.bt` file. Runs on whichever node calls it (local or, via
`erpc:call/5`, the peer). Does **not** announce `ClassLoaded` — callers that
need the reload-time re-check to fire do that explicitly afterward (the
connect-time scenario doesn't: it re-reads the manifest fresh on `nodeup`
regardless of `ClassLoaded`).
""".
-spec install_shape_version_stub(atom(), atom(), pos_integer()) -> ok.
install_shape_version_stub(ClassName, StubBasename, ShapeVersion) ->
    {ok, RealMod, Selectors} = beamtalk_class_metadata:lookup_methods(ClassName),
    {ok, Superclass} = beamtalk_class_metadata:lookup_superclass(ClassName),
    {ok, IsAbstract} = beamtalk_class_metadata:lookup_is_abstract(ClassName),
    StubMod = list_to_atom("bt@" ++ atom_to_list(StubBasename)),
    OverrideSrc =
        "'__beamtalk_meta'() -> "
        "Real = apply('" ++ atom_to_list(RealMod) ++
            "', '__beamtalk_meta', []), "
            "Real#{shape_version => " ++ integer_to_list(ShapeVersion) ++ "}.",
    ok = beamtalk_shape_migration_tests:build_delegate_proxy(RealMod, StubMod, [
        {{'__beamtalk_meta', 0}, OverrideSrc}
    ]),
    ok = beamtalk_class_metadata:merge_identity(
        ClassName, StubMod, Selectors, Superclass, IsAbstract
    ),
    ok.

shape_skew_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun({Peer, PeerNode}) ->
            [
                %% First: it asserts exact skew_count values, and the later
                %% scenarios leave other fixture classes skewed for good.
                {"a reload on the peer updates skew_count and announces on both nodes",
                    {timeout, 60, fun() -> peer_reload_updates_both_tallies(Peer, PeerNode) end}},
                {
                    "connecting two nodes with a class at different shapeVersions "
                    "announces exactly one NodeShapeSkew for it",
                    {timeout, 60, fun() -> skew_announced_once_on_connect(Peer, PeerNode) end}
                },
                {
                    "a shared class at matching versions, and a class not shared "
                    "with the peer at all, announce no skew",
                    {timeout, 60, fun() -> no_skew_for_matching_or_non_shared(Peer, PeerNode) end}
                },
                {"reloading a class on one side while connected re-announces skew",
                    {timeout, 60, fun() -> reload_reannounces_skew(Peer, PeerNode) end}},
                {"a reload notification for an earlier connection is dropped",
                    {timeout, 60, fun() ->
                        earlier_connection_notification_dropped(Peer, PeerNode)
                    end}},
                {"a peer reload made before the peer handled our nodeup is counted",
                    {timeout, 60, fun() ->
                        peer_reload_before_its_nodeup_counted(Peer, PeerNode)
                    end}},
                {"a restarted monitor still exchanges reload notifications",
                    {timeout, 60, fun() -> restarted_monitor_notified(Peer, PeerNode) end}},
                {"a peer that never says hello gets one connect-time announcement",
                    {timeout, 60, fun() -> peer_without_hello_announced_once(Peer, PeerNode) end}},
                {"matching versions announce no NodeVersionSkew",
                    {timeout, 60, fun() -> versions_match_no_announcement(PeerNode) end}},
                {"a wire-version mismatch announces exactly one NodeVersionSkew",
                    {timeout, 60, fun() ->
                        version_mismatch_announced_once(Peer, PeerNode, #{
                            wireVersion => 999, beamtalkVersion => local_beamtalk_version()
                        })
                    end}},
                {"a Beamtalk-version mismatch announces exactly one NodeVersionSkew",
                    {timeout, 60, fun() ->
                        version_mismatch_announced_once(Peer, PeerNode, #{
                            wireVersion => local_wire_version(), beamtalkVersion => <<"0.0.1-old">>
                        })
                    end}},
                {"a peer whose version read fails is announced with nil remote fields",
                    {timeout, 60, fun() -> version_read_failure_announced_nil(Peer, PeerNode) end}}
            ]
        end}}.

%%====================================================================
%% Connect-time: exactly one NodeShapeSkew per skewed shared class
%%====================================================================

skew_announced_once_on_connect(Peer, PeerNode) ->
    force_disconnect(PeerNode),
    ok = peer_call(Peer, load_fixture_settled, [shape_skew_cart, 'ShapeSkewCart']),
    %% Only the peer's copy is skewed to version 2 — the local side stays
    %% at the real module's implicit version 1.
    ok = peer_call(Peer, install_shape_version_stub, [
        'ShapeSkewCart', shape_skew_cart_v2_stub, 2
    ]),
    ok = load_fixture_settled(shape_skew_cart, 'ShapeSkewCart'),
    Sub = subscribe_self('NodeShapeSkew'),
    try
        reconnect(PeerNode),
        Event = expect_skew_event(PeerNode, 'ShapeSkewCart'),
        ?assertEqual(beamtalk_node:from_atom(PeerNode), maps:get(node, Event)),
        ?assertEqual(1, maps:get(localVersion, Event)),
        ?assertEqual(2, maps:get(remoteVersion, Event)),
        %% Exactly once: no second NodeShapeSkew for the same (peer, class)
        %% follows the first.
        expect_no_skew_event('ShapeSkewCart', 1000)
    after
        beamtalk_announcements:unsubscribe(Sub)
    end.

%%====================================================================
%% No skew for a matching-version shared class or a non-shared class
%%====================================================================

no_skew_for_matching_or_non_shared(Peer, PeerNode) ->
    %% Registered identically (the very same compiled module) on both sides.
    force_disconnect(PeerNode),
    ok = peer_call(Peer, load_fixture_settled, [shape_skew_match_cart, 'ShapeSkewMatchCart']),
    ok = load_fixture_settled(shape_skew_match_cart, 'ShapeSkewMatchCart'),
    %% Registered locally only — deliberately never touches the peer.
    ok = load_fixture_settled(shape_skew_local_only_cart, 'ShapeSkewLocalOnlyCart'),
    Sub = subscribe_self('NodeShapeSkew'),
    try
        reconnect(PeerNode),
        expect_no_skew_event('ShapeSkewMatchCart', 2000),
        expect_no_skew_event('ShapeSkewLocalOnlyCart', 100)
    after
        beamtalk_announcements:unsubscribe(Sub)
    end.

%%====================================================================
%% Reload-time: matching on connect, skewed after a local reload
%%====================================================================

reload_reannounces_skew(Peer, PeerNode) ->
    force_disconnect(PeerNode),
    ok = peer_call(Peer, load_fixture_settled, [shape_skew_reload_cart, 'ShapeSkewReloadCart']),
    ok = load_fixture_settled(shape_skew_reload_cart, 'ShapeSkewReloadCart'),
    Sub = subscribe_self('NodeShapeSkew'),
    try
        reconnect(PeerNode),
        %% Matching version 1 on both sides: no skew on connect.
        expect_no_skew_event('ShapeSkewReloadCart', 2000),
        %% Local-only version swap, still connected, then the real
        %% `ClassLoaded` trigger `beamtalk_node_monitor` subscribes to.
        ok = install_shape_version_stub('ShapeSkewReloadCart', shape_skew_reload_cart_v2_stub, 2),
        ok = beamtalk_announcements:system_announce('ClassLoaded', #{
            className => 'ShapeSkewReloadCart'
        }),
        Event = expect_skew_event(PeerNode, 'ShapeSkewReloadCart'),
        ?assertEqual(2, maps:get(localVersion, Event)),
        ?assertEqual(1, maps:get(remoteVersion, Event))
    after
        beamtalk_announcements:unsubscribe(Sub)
    end.

%%====================================================================
%% BT-5232: a reload on the peer updates both nodes
%%====================================================================

peer_reload_updates_both_tallies(Peer, PeerNode) ->
    Class = 'ShapeSkewPeerReloadCart',
    force_disconnect(PeerNode),
    ok = peer_call(Peer, load_fixture_settled, [shape_skew_peer_reload_cart, Class]),
    ok = load_fixture_settled(shape_skew_peer_reload_cart, Class),
    reconnect(PeerNode),
    %% Let both nodes' connect-time checks finish first: one that read the
    %% class before the reload could otherwise clear the count after it.
    await_connect_checks_done(Peer),
    await_skew_counts(PeerNode, 0),
    Sub = subscribe_self('NodeShapeSkew'),
    Forwarder = peer_call(Peer, forward_events, ['NodeShapeSkew', self()]),
    try
        %% The peer reloads to version 2. Before BT-5232 only the peer
        %% re-checked, so this node never announced and kept a 0 count.
        ok = peer_call(Peer, reload_with_shape_version, [
            Class, shape_skew_peer_reload_cart_v2_stub, 2
        ]),
        Local = expect_skew_event(PeerNode, Class),
        ?assertEqual(1, maps:get(localVersion, Local)),
        ?assertEqual(2, maps:get(remoteVersion, Local)),
        SelfValue = beamtalk_node:from_atom(node()),
        ?awaitMessage(
            {peer_got, 'NodeShapeSkew', #{node := SelfValue, className := Class}}, 10000
        ),
        await_skew_counts(PeerNode, 1),
        %% The peer reloads back to the real module (version 1): both
        %% counts go to 0.
        ok = peer_call(Peer, reload_to_real_module, [Class, shape_skew_peer_reload_cart]),
        await_skew_counts(PeerNode, 0)
    after
        Forwarder ! stop,
        beamtalk_announcements:unsubscribe(Sub)
    end.

-doc """
Wait until no connect-time check process is running on this node or the
peer. The monitor spawns them while handling `nodeup`, and again on the
other side's `hello` (BT-5265), so first wait until each side holds the
other's current generation: both `hello`s have then been handled and their
checks spawned. Afterwards a `sys:get_state/1` on each monitor flushes the
casts the checks sent.
""".
await_connect_checks_done(Peer) ->
    PeerNode = peer:call(Peer, erlang, node, []),
    beamtalk_test_wait:until(
        fun() ->
            peer_gen_of(PeerNode) =:= peer_call(Peer, local_gen, [node()]) andalso
                peer_call(Peer, peer_gen_of, [node()]) =:= local_gen(PeerNode)
        end,
        10000
    ),
    beamtalk_test_wait:until(
        fun() ->
            not connect_checks_running() andalso
                not peer_call(Peer, connect_checks_running, [])
        end,
        10000
    ),
    _ = sys:get_state(beamtalk_node_monitor),
    _ = peer:call(Peer, sys, get_state, [beamtalk_node_monitor]),
    ok.

-doc "Whether a monitor check (connect, reload or peer reload) is running on this node.".
-spec connect_checks_running() -> boolean().
connect_checks_running() ->
    Checks = [
        check_version_skew_on_connect,
        check_shape_skew_on_connect,
        check_shape_skew_on_reload,
        check_peer_reload
    ],
    lists:any(
        fun(Pid) ->
            case erlang:process_info(Pid, current_stacktrace) of
                {current_stacktrace, Frames} ->
                    lists:any(
                        fun({M, F, _, _}) ->
                            M =:= beamtalk_node_monitor andalso lists:member(F, Checks)
                        end,
                        Frames
                    );
                undefined ->
                    false
            end
        end,
        erlang:processes()
    ).

-doc "Wait until both this node's count for `PeerNode` and the peer's count for this node are `N`.".
await_skew_counts(PeerNode, N) ->
    beamtalk_test_wait:until(
        fun() ->
            beamtalk_node_monitor:skew_count(PeerNode) =:= N andalso
                erpc:call(PeerNode, beamtalk_node_monitor, skew_count, [node()]) =:= N
        end,
        10000
    ).

-doc """
Repoint `ClassName` at a stub reporting `ShapeVersion` and announce
`ClassLoaded`, as a real reload does. Runs on the peer via `peer_call/3`.
""".
-spec reload_with_shape_version(atom(), atom(), pos_integer()) -> ok.
reload_with_shape_version(ClassName, StubBasename, ShapeVersion) ->
    ok = install_shape_version_stub(ClassName, StubBasename, ShapeVersion),
    beamtalk_announcements:system_announce('ClassLoaded', #{className => ClassName}).

-doc """
Point `ClassName` back at its real compiled fixture module (`Basename`)
and announce `ClassLoaded`. Runs on the peer via `peer_call/3`.
""".
-spec reload_to_real_module(atom(), atom()) -> ok.
reload_to_real_module(ClassName, Basename) ->
    RealMod = list_to_atom("bt@" ++ atom_to_list(Basename)),
    {ok, _StubMod, Selectors} = beamtalk_class_metadata:lookup_methods(ClassName),
    {ok, Superclass} = beamtalk_class_metadata:lookup_superclass(ClassName),
    {ok, IsAbstract} = beamtalk_class_metadata:lookup_is_abstract(ClassName),
    ok = beamtalk_class_metadata:merge_identity(
        ClassName, RealMod, Selectors, Superclass, IsAbstract
    ),
    beamtalk_announcements:system_announce('ClassLoaded', #{className => ClassName}).

-doc """
Start a process on the calling node that forwards every `EventClass`
announcement to `Collector` as `{peer_got, EventClass, Event}`, and return
its pid; send it `stop` to end it (and its subscription).
""".
-spec forward_events(atom(), pid()) -> pid().
forward_events(EventClass, Collector) ->
    Parent = self(),
    Pid = spawn(fun() ->
        Handler = fun(Event) -> Collector ! {peer_got, EventClass, Event} end,
        {ok, _Ref} = beamtalk_announcements:subscribe(EventClass, self(), Handler, false),
        Parent ! {forwarding, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {forwarding, Pid} -> Pid
    after 10000 -> error(forwarder_not_started)
    end.

%%====================================================================
%% BT-5265: hello, and the sender's generation on reload notifications
%%====================================================================

-doc """
The sender reconnect window. This node's monitor still holds a reload
notification request for the old connection when the link drops and comes
back: it is suspended, so it handles that request before the `nodedown`,
while the old generation still looks current, and sends it on the new
connection. Before BT-5265 the peer accepted it and counted a skew from a
version this node never has (99). Now it carries this node's old
generation, arrives before this node's new `hello`, and is dropped.

A second notification (a class the peer does not have) is sent after it
by the same monitor; the peer accepting that one shows it has handled the
first.
""".
earlier_connection_notification_dropped(Peer, PeerNode) ->
    flush_events(),
    Class = 'ShapeSkewMatchCart',
    force_disconnect(PeerNode),
    ok = peer_call(Peer, load_fixture_settled, [shape_skew_match_cart, Class]),
    ok = load_fixture_settled(shape_skew_match_cart, Class),
    reconnect(PeerNode),
    await_connect_checks_done(Peer),
    Before = peer_count(Peer),
    Real = beamtalk_release:project_class_shape_entry(Class),
    OldGen = local_gen(PeerNode),
    %% Stamps after each generation's `hello`, so the sender lets them out.
    StaleSeq = local_hello_at(PeerNode) + 1,
    Forwarder = peer_call(Peer, forward_events, ['NodeShapeSkew', self()]),
    try
        ok = sys:suspend(beamtalk_node_monitor),
        try
            gen_server:cast(
                beamtalk_node_monitor,
                {send_peer_class_checked, PeerNode, OldGen, Class, Real#{version => 99}, StaleSeq}
            ),
            force_disconnect(PeerNode),
            reconnect(PeerNode)
        after
            sys:resume(beamtalk_node_monitor)
        end,
        _ = sys:get_state(beamtalk_node_monitor),
        NewGen = local_gen(PeerNode),
        ?assertNotEqual(OldGen, NewGen),
        MarkerSeq = local_hello_at(PeerNode) + 1,
        gen_server:cast(
            beamtalk_node_monitor,
            {send_peer_class_checked, PeerNode, NewGen, 'Bt5265Marker', Real, MarkerSeq}
        ),
        beamtalk_test_wait:until(
            fun() -> peer_call(Peer, reload_seq_of, [node(), 'Bt5265Marker']) =:= MarkerSeq end,
            10000
        ),
        await_connect_checks_done(Peer),
        ?assertEqual(Before, peer_count(Peer)),
        ?assertEqual(undefined, peer_call(Peer, reload_seq_of, [node(), Class])),
        receive
            {peer_got, 'NodeShapeSkew', #{className := Class} = Event} ->
                erlang:error({stale_notification_announced, Event})
        after 500 -> ok
        end
    after
        Forwarder ! stop
    end.

-doc """
The peer reloads a class after the connection is up but before its
monitor has handled `nodeup` for this node (its monitor is suspended).
This node's own reload check of the class runs first and reads the peer's
old entry. The reload must still reach this node's tally, announced once.
""".
peer_reload_before_its_nodeup_counted(Peer, PeerNode) ->
    flush_events(),
    Class = 'ShapeSkewPeerReloadCart',
    force_disconnect(PeerNode),
    ok = peer_call(Peer, load_fixture_settled, [shape_skew_peer_reload_cart, Class]),
    ok = load_fixture_settled(shape_skew_peer_reload_cart, Class),
    Sub = subscribe_self('NodeShapeSkew'),
    ok = peer_call(Peer, suspend_monitor, []),
    Base =
        try
            reconnect(PeerNode),
            %% This node's nodeup check, then its own reload check of the
            %% class, both read the peer's version 1.
            _ = sys:get_state(beamtalk_node_monitor),
            await_local_checks_done(),
            beamtalk_node_monitor ! {'$beamtalk_class_loaded', Class},
            _ = sys:get_state(beamtalk_node_monitor),
            await_local_checks_done(),
            ?assertEqual(clear, class_result(PeerNode, Class)),
            ok = peer_call(Peer, reload_with_shape_version, [
                Class, shape_skew_peer_reload_cart_v2_stub, 2
            ]),
            beamtalk_node_monitor:skew_count(PeerNode)
        after
            ok = peer_call(Peer, resume_monitor, [])
        end,
    try
        Event = expect_skew_event(PeerNode, Class),
        ?assertEqual(2, maps:get(remoteVersion, Event)),
        beamtalk_test_wait:until(
            fun() -> beamtalk_node_monitor:skew_count(PeerNode) =:= Base + 1 end, 10000
        ),
        await_connect_checks_done(Peer),
        expect_no_skew_event(Class, 1000),
        ?assertEqual({skewed, 1, 2}, class_result(PeerNode, Class))
    after
        beamtalk_announcements:unsubscribe(Sub),
        ok = peer_call(Peer, reload_to_real_module, [Class, shape_skew_peer_reload_cart])
    end,
    beamtalk_test_wait:until(
        fun() -> beamtalk_node_monitor:skew_count(PeerNode) =:= Base end, 10000
    ).

-doc """
The peer's monitor restarts while connected. Its new generation reaches
this node in its `hello`, and this node's comes back in reply, so reload
notifications are accepted both ways without a reconnect.
""".
restarted_monitor_notified(Peer, PeerNode) ->
    Class = 'ShapeSkewPeerReloadCart',
    ok = peer_call(Peer, load_fixture_settled, [shape_skew_peer_reload_cart, Class]),
    ok = load_fixture_settled(shape_skew_peer_reload_cart, Class),
    await_connect_checks_done(Peer),
    ok = peer_call(Peer, restart_monitor, []),
    %% Each side holds the other's current generation.
    beamtalk_test_wait:until(
        fun() ->
            peer_call(Peer, peer_gen_of, [node()]) =:= local_gen(PeerNode) andalso
                peer_gen_of(PeerNode) =:= peer_call(Peer, local_gen, [node()])
        end,
        10000
    ),
    await_connect_checks_done(Peer),
    LocalBase = beamtalk_node_monitor:skew_count(PeerNode),
    PeerBase = peer_count(Peer),
    try
        %% This node reloads to version 2: the restarted peer counts it.
        ok = reload_with_shape_version(Class, shape_skew_peer_reload_cart_v2_stub, 2),
        await_counts(Peer, PeerNode, LocalBase + 1, PeerBase + 1),
        %% The peer reloads to version 2 too: matching again on both.
        ok = peer_call(Peer, reload_with_shape_version, [
            Class, shape_skew_peer_reload_cart_v2_stub, 2
        ]),
        await_counts(Peer, PeerNode, LocalBase, PeerBase)
    after
        ok = reload_to_real_module(Class, shape_skew_peer_reload_cart),
        ok = peer_call(Peer, reload_to_real_module, [Class, shape_skew_peer_reload_cart])
    end,
    await_counts(Peer, PeerNode, LocalBase, PeerBase).

-doc """
Mixed versions: a peer that never says `hello`, like 0.6.0. The peer's
monitor is stopped, so nothing answers this node's `hello` and none comes
back. The `nodeup` check alone still finds the skewed class, announced
once.
""".
peer_without_hello_announced_once(Peer, PeerNode) ->
    flush_events(),
    %% Skewed: the real module (1) here, a version-2 stub on the peer.
    Class = 'ShapeSkewCart',
    force_disconnect(PeerNode),
    %% Stopped first: a `ClassLoaded` re-check on the peer could otherwise
    %% re-dial this node before the scenario's own reconnect.
    ok = peer_call(Peer, stop_monitor, []),
    ok = peer_call(Peer, ensure_shape_version_stub, [
        Class, shape_skew_cart, shape_skew_cart_v2_stub, 2
    ]),
    ok = load_fixture_settled(shape_skew_cart, Class),
    Sub = subscribe_self('NodeShapeSkew'),
    try
        reconnect(PeerNode),
        Event = expect_skew_event(PeerNode, Class),
        ?assertEqual(2, maps:get(remoteVersion, Event)),
        _ = sys:get_state(beamtalk_node_monitor),
        await_local_checks_done(),
        expect_no_skew_event(Class, 1000),
        ?assertEqual({skewed, 1, 2}, class_result(PeerNode, Class)),
        ?assertEqual(undefined, peer_gen_of(PeerNode))
    after
        beamtalk_announcements:unsubscribe(Sub),
        ok = peer_call(Peer, start_monitor, [])
    end.

%% This node's monitor generation for its connection to `Node`.
local_gen(Node) ->
    #{gen := Gen} = maps:get(Node, element(4, sys:get_state(beamtalk_node_monitor))),
    Gen.

%% The stamp of this node's monitor's last `hello` to `Node`.
local_hello_at(Node) ->
    #{hello_at := HelloAt} = maps:get(Node, element(4, sys:get_state(beamtalk_node_monitor))),
    HelloAt.

%% The generation `Node`'s monitor sent this node's monitor in its `hello`.
peer_gen_of(Node) ->
    maps:get(peer_gen, maps:get(Node, element(4, sys:get_state(beamtalk_node_monitor))), undefined).

%% The newest reload stamp of `ClassName` this node's monitor accepted from
%% `Node`, or `undefined`.
reload_seq_of(Node, ClassName) ->
    #{reload_seq := Seqs} = maps:get(Node, element(4, sys:get_state(beamtalk_node_monitor))),
    maps:get(ClassName, Seqs, undefined).

%% Drop announcement events earlier scenarios' subscriptions delivered to
%% this (shared) test process after they ended.
flush_events() ->
    receive
        {got, _, _} -> flush_events();
        {peer_got, _, _} -> flush_events()
    after 0 -> ok
    end.

%% The result this node's tally holds for `ClassName` against `Node`.
class_result(Node, ClassName) ->
    #{classes := Classes} = maps:get(Node, element(4, sys:get_state(beamtalk_node_monitor))),
    {_Stamp, Result} = maps:get(ClassName, Classes),
    Result.

peer_count(Peer) ->
    peer:call(Peer, beamtalk_node_monitor, skew_count, [node()]).

await_counts(Peer, PeerNode, Local, Remote) ->
    beamtalk_test_wait:until(
        fun() ->
            beamtalk_node_monitor:skew_count(PeerNode) =:= Local andalso
                peer_count(Peer) =:= Remote
        end,
        10000
    ).

await_local_checks_done() ->
    beamtalk_test_wait:until(fun() -> not connect_checks_running() end, 10000),
    _ = sys:get_state(beamtalk_node_monitor),
    ok.

-doc """
Make `ClassName` run on the `StubBasename` stub reporting `ShapeVersion`:
register the `Basename` fixture first if the class is not registered, and
install the stub unless it is already in place. Registering the fixture
over an earlier stub, or a stub over itself, is never done. Runs on the
peer via `peer_call/3`.
""".
-spec ensure_shape_version_stub(atom(), atom(), atom(), pos_integer()) -> ok.
ensure_shape_version_stub(ClassName, Basename, StubBasename, ShapeVersion) ->
    StubMod = list_to_atom("bt@" ++ atom_to_list(StubBasename)),
    case beamtalk_class_metadata:lookup_methods(ClassName) of
        {ok, StubMod, _} ->
            ok;
        {ok, _RealMod, _} ->
            install_shape_version_stub(ClassName, StubBasename, ShapeVersion);
        _NotRegistered ->
            ok = load_fixture(Basename, ClassName),
            install_shape_version_stub(ClassName, StubBasename, ShapeVersion)
    end.

%% Run on the peer via `peer_call/3`.
suspend_monitor() -> sys:suspend(beamtalk_node_monitor).
resume_monitor() -> sys:resume(beamtalk_node_monitor).

stop_monitor() ->
    ok = supervisor:terminate_child(beamtalk_runtime_sup, beamtalk_node_monitor).

start_monitor() ->
    {ok, _} = supervisor:restart_child(beamtalk_runtime_sup, beamtalk_node_monitor),
    ok.

restart_monitor() ->
    ok = stop_monitor(),
    ok = start_monitor(),
    _ = sys:get_state(beamtalk_node_monitor),
    ok.

%%====================================================================
%% BT-5232: NodeVersionSkew at connect (ADR 0126 Amendment D3)
%%====================================================================

versions_match_no_announcement(PeerNode) ->
    force_disconnect(PeerNode),
    Sub = subscribe_self('NodeVersionSkew'),
    try
        reconnect(PeerNode),
        expect_no_version_skew_event(PeerNode, 2000)
    after
        beamtalk_announcements:unsubscribe(Sub)
    end.

version_mismatch_announced_once(Peer, PeerNode, FakeVersions) ->
    force_disconnect(PeerNode),
    ok = peer_call(Peer, fake_dist_versions, [{reply, FakeVersions}]),
    Sub = subscribe_self('NodeVersionSkew'),
    try
        reconnect(PeerNode),
        Event = expect_version_skew_event(PeerNode),
        ?assertEqual(local_wire_version(), maps:get(localWireVersion, Event)),
        ?assertEqual(maps:get(wireVersion, FakeVersions), maps:get(remoteWireVersion, Event)),
        ?assertEqual(
            {beamtalk_string, local_beamtalk_version()}, maps:get(localBeamtalkVersion, Event)
        ),
        ?assertEqual(
            {beamtalk_string, maps:get(beamtalkVersion, FakeVersions)},
            maps:get(remoteBeamtalkVersion, Event)
        ),
        expect_no_version_skew_event(PeerNode, 1000),
        %% Announced, never refused.
        ?assert(lists:member(PeerNode, nodes()))
    after
        beamtalk_announcements:unsubscribe(Sub),
        ok = peer_call(Peer, unfake_dist_versions, [])
    end.

version_read_failure_announced_nil(Peer, PeerNode) ->
    force_disconnect(PeerNode),
    %% `undef` is what a peer too old to export dist_versions/0 raises.
    ok = peer_call(Peer, fake_dist_versions, [{raise, undef}]),
    Sub = subscribe_self('NodeVersionSkew'),
    try
        reconnect(PeerNode),
        Event = expect_version_skew_event(PeerNode),
        ?assertEqual(nil, maps:get(remoteWireVersion, Event)),
        ?assertEqual(nil, maps:get(remoteBeamtalkVersion, Event)),
        ?assertEqual(local_wire_version(), maps:get(localWireVersion, Event)),
        expect_no_version_skew_event(PeerNode, 1000),
        ?assert(lists:member(PeerNode, nodes()))
    after
        beamtalk_announcements:unsubscribe(Sub),
        ok = peer_call(Peer, unfake_dist_versions, [])
    end.

-doc """
Replace `beamtalk_version:dist_versions/0` on the calling node (the peer, via
`peer_call/3`): `{reply, Map}` answers `Map`, `{raise, Reason}` raises
`error:Reason`.
""".
-spec fake_dist_versions({reply, map()} | {raise, term()}) -> ok.
fake_dist_versions(Fake) ->
    ok = meck:new(beamtalk_version, [passthrough, no_link]),
    Fun =
        case Fake of
            {reply, Map} -> fun() -> Map end;
            {raise, Reason} -> fun() -> erlang:error(Reason) end
        end,
    ok = meck:expect(beamtalk_version, dist_versions, Fun).

-spec unfake_dist_versions() -> ok.
unfake_dist_versions() ->
    meck:unload(beamtalk_version).

local_wire_version() ->
    maps:get(wireVersion, beamtalk_version:dist_versions()).

local_beamtalk_version() ->
    maps:get(beamtalkVersion, beamtalk_version:dist_versions()).

expect_version_skew_event(PeerNode) ->
    PeerValue = beamtalk_node:from_atom(PeerNode),
    ?awaitMessage({got, 'NodeVersionSkew', #{node := PeerValue} = Event}, 10000),
    Event.

expect_no_version_skew_event(PeerNode, TimeoutMs) ->
    PeerValue = beamtalk_node:from_atom(PeerNode),
    receive
        {got, 'NodeVersionSkew', #{node := PeerValue} = Event} ->
            erlang:error({unexpected_version_skew_event, Event})
    after TimeoutMs ->
        ok
    end.

%%====================================================================
%% Helpers
%%====================================================================

-doc """
Load a fixture locally while disconnected from the peer and wait until
`beamtalk_node_monitor` has finished reacting to the resulting `ClassLoaded`.

BT-4999: registering a class fires `ClassLoaded`; the monitor reacts by
spawning a reload-time skew re-check against `nodes()` (a *second*, legitimate
trigger besides `nodeup`). That chain (dispatch process -> monitor mailbox ->
spawned check) is asynchronous, so under CPU load it could land *after* the
scenario's reconnect, see the peer, and announce a duplicate `NodeShapeSkew`
inside the "exactly once" window. Production announces once per trigger; the
race was the test's. BT-5232: the same holds on the peer, whose reload check now
also reports to this node, so the peer loads its fixtures this way too
(via `peer_call/3`). Loading while disconnected makes that re-check a no-op
(`nodes()` is empty), and draining the chain before reconnecting keeps it so.
""".
-spec load_fixture_settled(atom(), atom()) -> ok.
load_fixture_settled(Basename, ClassName) ->
    Self = self(),
    Handler = fun(#{className := C}) -> Self ! {class_loaded_seen, C} end,
    {ok, Sub} = beamtalk_announcements:subscribe('ClassLoaded', Self, Handler, false),
    try
        %% `ensure_fixture_loaded` is idempotent: only a first registration
        %% announces `ClassLoaded`, so only then is there an event to wait for.
        Fresh = beamtalk_class_registry:whereis_class(ClassName) =:= undefined,
        ok = load_fixture(Basename, ClassName),
        case Fresh of
            true ->
                receive
                    {class_loaded_seen, ClassName} -> ok
                after 5000 -> ok
                end;
            false ->
                ok
        end
    after
        beamtalk_announcements:unsubscribe(Sub)
    end,
    %% The monitor has now been handed (or is about to drain) its own copy of
    %% the event; a synchronous call flushes it, and the spawned re-check it
    %% starts sees `nodes() =:= []` and exits at once.
    _ = sys:get_state(beamtalk_node_monitor),
    _ = sys:get_state(beamtalk_node_monitor),
    ok.

-doc """
Disconnect from `PeerNode` and wait until `nodes()` no longer lists it, so
the following `reconnect/1` produces a fresh, unambiguous `nodeup` for the
scenario under test rather than racing a disconnect already in flight.
`standard_io` peer connection (see `setup/0`) means disconnecting from our
side never halts the peer.
""".
-spec force_disconnect(node()) -> ok.
force_disconnect(PeerNode) ->
    _ = erlang:disconnect_node(PeerNode),
    beamtalk_test_wait:until(fun() -> not lists:member(PeerNode, nodes()) end, 5000).

-doc """
Call `?MODULE:Fun(Args)` on the peer over `peer`'s own control channel, which
is independent of Erlang distribution. BT-4999: while the nodes are
disconnected, an `erpc` would silently re-dial the peer and produce a
`nodeup` (hence a `NodeShapeSkew`) *before* the scenario subscribes, so the
scenario's own `reconnect/1` would then be a no-op and its event lost.
""".
-spec peer_call(peer:server_ref(), atom(), [term()]) -> term().
peer_call(Peer, Fun, Args) ->
    peer:call(Peer, ?MODULE, Fun, Args, 60000).

-doc "Reconnect to `PeerNode`, asserting the connection actually came up.".
-spec reconnect(node()) -> ok.
reconnect(PeerNode) ->
    ?assertEqual(true, net_kernel:connect_node(PeerNode)).

%% Subscribe `self()` to a system event class with a fun handler that
%% forwards each payload back to this process (mirrors
%% beamtalk_node_tests.erl's subscribe_self/1).
subscribe_self(EventClass) ->
    Collector = self(),
    Handler = fun(Event) -> Collector ! {got, EventClass, Event} end,
    {ok, SubRef} = beamtalk_announcements:subscribe(EventClass, self(), Handler, false),
    SubRef.

%% Wait for the 'NodeShapeSkew' event about `PeerNode`/`ClassName`
%% specifically — other suites' peers/classes may come and go on the shared
%% system bus concurrently.
expect_skew_event(PeerNode, ClassName) ->
    PeerValue = beamtalk_node:from_atom(PeerNode),
    ?awaitMessage(
        {got, 'NodeShapeSkew', #{node := PeerValue, className := ClassName} = Event}, 10000
    ),
    Event.

%% Confirm no 'NodeShapeSkew' for exactly `ClassName` arrives within
%% `TimeoutMs` — used both for "no skew at all" scenarios and, after
%% `expect_skew_event/2` has consumed the one expected event, to prove it
%% was not followed by a duplicate.
expect_no_skew_event(ClassName, TimeoutMs) ->
    receive
        {got, 'NodeShapeSkew', #{className := ClassName} = Event} ->
            erlang:error({unexpected_skew_event, ClassName, Event})
    after TimeoutMs ->
        ok
    end.
