%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_shapes_conformance_tests).

-moduledoc """
Disk-vs-live shape conformance test (ADR 0125 §2.2/§2.3, BT-3574).

`beamtalk_release_shapes:class_field_shape/1` (build-time, disk-derived —
lives in `beamtalk_runtime`) and `beamtalk_workspace_shape_store:
read_shape_from_meta/1` (live, ETS-backed — lives in `beamtalk_workspace`)
both flatten a class's `field_types`/`field_kinds` via the same two
shared-leaf primitives (`beamtalk_class_metadata:flatten_ancestor_map/2` and
`normalize_field_shape/2`). This test pins that they report **structurally
identical** results for the same registered classes — the invariant
CLAUDE.md requires ("never write a keep-in-sync comment without a test that
enforces it") — rather than trusting the two call sites to stay aligned by
convention.

Lives here, in `beamtalk_workspace`'s test suite, rather than
`beamtalk_runtime`'s: `beamtalk_workspace` already depends on
`beamtalk_runtime` (ADR-mandated one-way dependency,
`docs/development/architecture-principles.md` §1), so this is the one test
scope that can call both flatteners without introducing an upward
`beamtalk_runtime -> beamtalk_workspace` edge.

Reuses `beamtalk_workspace_shape_store_tests`'
`beamtalk_shape_store_superclass_fixture`/`beamtalk_shape_store_subclass_fixture`
— exactly the "subclass with an ancestor field, and a `shapeVersion:`-bumped
class" fixture pair ADR 0125's BT-3574 acceptance criteria ask for — rather
than a second, redundant hand-rolled `__beamtalk_meta/0` fixture.
""".

-include_lib("eunit/include/eunit.hrl").

-define(TABLE, beamtalk_class_metadata).

setup() ->
    %% `beamtalk_release_shapes:class_field_shape/1` resolves via
    %% `beamtalk_shape_migration:resolve_migrations/1`, whose `read_meta/1`
    %% gates on `erlang:function_exported/3` — which, unlike a qualified
    %% `Module:'__beamtalk_meta'()` call, does **not** auto-load an as-yet-
    %% unloaded module (see `beamtalk_shape_migration:read_meta/1`'s doc). A
    %% real release build guarantees these fixture modules are already
    %% loaded (`beamtalk_module_activation:activate_modules/2` runs first);
    %% here, running this test module in isolation (alphabetically ahead of
    %% `beamtalk_workspace_shape_store_tests`, whose own tests are what
    %% would otherwise load them first) needs the same guarantee made
    %% explicit, or the conformance check would spuriously report "no
    %% shape" on both sides rather than exercising the real comparison.
    {module, beamtalk_shape_store_superclass_fixture} =
        code:ensure_loaded(beamtalk_shape_store_superclass_fixture),
    {module, beamtalk_shape_store_subclass_fixture} =
        code:ensure_loaded(beamtalk_shape_store_subclass_fixture),
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureSuperclass',
        beamtalk_shape_store_superclass_fixture,
        [count, taxRate],
        none,
        undefined
    ),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureSubclass',
        beamtalk_shape_store_subclass_fixture,
        [name],
        'ShapeFixtureSuperclass',
        undefined
    ),
    ok.

cleanup(_) ->
    ets:delete(?TABLE, 'ShapeFixtureSubclass'),
    ets:delete(?TABLE, 'ShapeFixtureSuperclass'),
    ok.

conformance_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun disk_and_live_agree_on_the_ancestor_only_class/0,
        fun disk_and_live_agree_on_the_flattened_subclass/0,
        fun disk_and_live_agree_that_an_unregistered_class_is_undefined/0
    ]}.

%% The root/ancestor half: no ancestors of its own, so disk and live should
%% both report just its own two fields.
disk_and_live_agree_on_the_ancestor_only_class() ->
    Disk = beamtalk_release_shapes:class_field_shape('ShapeFixtureSuperclass'),
    Live = beamtalk_workspace_shape_store:read_shape_from_meta(<<"ShapeFixtureSuperclass">>),
    ?assertEqual(Live, Disk),
    %% Pinned to a concrete value too, not just "the two agree with each
    %% other" — a bug shared by both sides would otherwise pass silently.
    ?assertEqual(
        #{
            <<"count">> => {<<"Integer">>, <<"eager">>},
            <<"taxRate">> => {<<"Float">>, <<"eager">>}
        },
        Disk
    ).

%% The subclass half: flattened with the ancestor's fields merged in, and
%% carrying its own declared shapeVersion: 2 / migrateFromV1: — the whole
%% point of the conformance test (ADR 0125 §2.2's "shared leaf" claim).
disk_and_live_agree_on_the_flattened_subclass() ->
    Disk = beamtalk_release_shapes:class_field_shape('ShapeFixtureSubclass'),
    Live = beamtalk_workspace_shape_store:read_shape_from_meta(<<"ShapeFixtureSubclass">>),
    ?assertEqual(Live, Disk),
    ?assertEqual(
        #{
            <<"count">> => {<<"Integer">>, <<"eager">>},
            <<"taxRate">> => {<<"Float">>, <<"eager">>},
            <<"name">> => {<<"Dynamic">>, <<"eager">>}
        },
        Disk
    ),
    %% The version/migrations half of the same generation, via
    %% class_shape_entry/1 (the JSON-facing projection) vs.
    %% read_generation_from_meta/1 (the live one) — both must report the
    %% declared shapeVersion: 2 and its migrateFromV1: entry the same way.
    DiskEntry = beamtalk_release_shapes:class_shape_entry('ShapeFixtureSubclass'),
    LiveGen = beamtalk_workspace_shape_store:read_generation_from_meta(<<"ShapeFixtureSubclass">>),
    ?assertEqual(maps:get(version, LiveGen), maps:get(version, DiskEntry)),
    ?assertEqual(2, maps:get(version, DiskEntry)),
    ?assertEqual(#{1 => 'migrateFromV1:'}, maps:get(migrations, LiveGen)).

%% Both sides degrade to `undefined` for a class neither can resolve —
%% never one reporting a stale/empty shape while the other reports
%% undefined.
disk_and_live_agree_that_an_unregistered_class_is_undefined() ->
    ?assertEqual(
        beamtalk_workspace_shape_store:read_shape_from_meta(<<"NeverSeenClassXyz">>),
        beamtalk_release_shapes:class_field_shape('NeverSeenClassXyz')
    ),
    ?assertEqual(undefined, beamtalk_release_shapes:class_field_shape('NeverSeenClassXyz')).
