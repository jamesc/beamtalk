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
CLAUDE.md's no-duplicate-implementations rule) whose `'__beamtalk_meta'/0`
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
""".

-include_lib("eunit/include/eunit.hrl").

-export([
    load_fixture/2,
    install_shape_version_stub/3
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
    {ok, _Started} = application:ensure_all_started(beamtalk_runtime),
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
    Module = list_to_atom("bt@" ++ atom_to_list(Basename)),
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    true = erlang:function_exported(Module, register_class, 0),
                    Module:register_class(),
                    ok;
                _Pid ->
                    ok
            end;
        {error, Reason} ->
            error({fixture_module_not_found, Basename, ClassName, Reason})
    end.

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
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {
                    "connecting two nodes with a class at different shapeVersions "
                    "announces exactly one NodeShapeSkew for it",
                    fun() -> skew_announced_once_on_connect(PeerNode) end
                },
                {
                    "a shared class at matching versions, and a class not shared "
                    "with the peer at all, announce no skew",
                    fun() -> no_skew_for_matching_or_non_shared(PeerNode) end
                },
                {"reloading a class on one side while connected re-announces skew", fun() ->
                    reload_reannounces_skew(PeerNode)
                end}
            ]
        end}}.

%%====================================================================
%% Connect-time: exactly one NodeShapeSkew per skewed shared class
%%====================================================================

skew_announced_once_on_connect(PeerNode) ->
    ok = load_fixture(shape_skew_cart, 'ShapeSkewCart'),
    ok = erpc:call(PeerNode, ?MODULE, load_fixture, [shape_skew_cart, 'ShapeSkewCart']),
    %% Only the peer's copy is skewed to version 2 — the local side stays
    %% at the real module's implicit version 1.
    ok = erpc:call(PeerNode, ?MODULE, install_shape_version_stub, [
        'ShapeSkewCart', shape_skew_cart_v2_stub, 2
    ]),
    force_disconnect(PeerNode),
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

no_skew_for_matching_or_non_shared(PeerNode) ->
    %% Registered identically (the very same compiled module) on both sides.
    ok = load_fixture(shape_skew_match_cart, 'ShapeSkewMatchCart'),
    ok = erpc:call(PeerNode, ?MODULE, load_fixture, [shape_skew_match_cart, 'ShapeSkewMatchCart']),
    %% Registered locally only — deliberately never touches the peer.
    ok = load_fixture(shape_skew_local_only_cart, 'ShapeSkewLocalOnlyCart'),
    force_disconnect(PeerNode),
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

reload_reannounces_skew(PeerNode) ->
    ok = load_fixture(shape_skew_reload_cart, 'ShapeSkewReloadCart'),
    ok = erpc:call(PeerNode, ?MODULE, load_fixture, [
        shape_skew_reload_cart, 'ShapeSkewReloadCart'
    ]),
    force_disconnect(PeerNode),
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
%% Helpers
%%====================================================================

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
    wait_until(fun() -> not lists:member(PeerNode, nodes()) end, 5000).

-doc "Reconnect to `PeerNode`, asserting the connection actually came up.".
-spec reconnect(node()) -> ok.
reconnect(PeerNode) ->
    ?assertEqual(true, net_kernel:connect_node(PeerNode)).

-spec wait_until(fun(() -> boolean()), non_neg_integer()) -> ok.
wait_until(_Pred, RemainingMs) when RemainingMs =< 0 ->
    ok;
wait_until(Pred, RemainingMs) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait_until(Pred, RemainingMs - 20)
    end.

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
    receive
        {got, 'NodeShapeSkew', #{node := PeerValue, className := ClassName} = Event} -> Event
    after 10000 ->
        ?assert(false)
    end.

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
