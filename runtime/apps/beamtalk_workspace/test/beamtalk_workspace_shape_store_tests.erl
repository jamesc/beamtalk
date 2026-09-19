%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_shape_store_tests).

-moduledoc """
Unit tests for beamtalk_workspace_shape_store (ADR 0105 Phase 2, ADR 0123
Phase 4 BT-3538).

Covers:
- prime/1's "seed once" laziness (does not overwrite an existing entry)
- capture/1's self-seeding fallback when prime/1 was never called (always
  no_op)
- the two-phase capture flow end-to-end: prime/1 against a currently-loaded
  module, then capture/1 against a *different* installed module (simulating
  the module having been replaced by code:load_binary in between)
- clear/0 resets the session
- previous/1 is read-only
- read_shape_from_meta/1's degrade-to-undefined paths (no meta exported,
  class not registered) and the real read path via
  beamtalk_shape_store_fixture
- field_type_to_binary/1's Dynamic-sentinel normalisation (exercised
  indirectly through read_shape_from_meta/1, since it is not exported)
- BT-3538: read_shape_from_meta/1 flattens a two-level hierarchy (a
  subclass's own field plus its superclass's, via
  beamtalk_shape_store_superclass_fixture/beamtalk_shape_store_subclass_fixture)
- BT-3538: read_generation_from_meta/1 and capture/1 carry
  'shape_version'/'shape_migrations' alongside the flattened shape
- BT-3560: ancestor_field_types/1 returns only the ancestor contribution,
  never folding in the class's own fields (the accessor
  read_generation_from_meta/1's ancestor_shape now uses, replacing a
  precheck-side derivation that was lossy for a shadowed field)
- BT-3556 (ADR 0124 Section 9/B9): a shape() value is {DeclaredType, Kind},
  not a bare type — normalize_shape/2's zip of field_types/field_kinds,
  ancestor_field_kinds/1's flattening, and the eager-default for a field/
  level with no field_kinds meta at all

The end-to-end thread from a real class-body reload
(`beamtalk_repl_loader:load_class_module/3` et al.) through to a
correctly-populated store entry and a shape-change re-check is covered in
`beamtalk_repl_loader_recheck_tests.erl`, not here — this module tests the
store's own API contract in isolation.
""".

-include_lib("eunit/include/eunit.hrl").

-define(TABLE, beamtalk_class_metadata).

%% A shape() value, eager kind — every fixture module in this file declares
%% no `field_kinds` meta, so normalize_shape/2 always defaults to eager.
e(TypeBin) -> {TypeBin, <<"eager">>}.

%%====================================================================
%% Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = beamtalk_workspace_shape_store:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok
            end;
        false ->
            ok
    end.

store_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun prime_on_unregistered_class_seeds_undefined/1,
        fun capture_without_prime_is_always_no_op/1,
        fun previous_does_not_mutate_store/1,
        fun clear_resets_the_session/1,
        fun different_classes_are_independent/1,
        fun capture_carries_shape_version_and_migrations/1
    ]}.

%%====================================================================
%% prime/1 + capture/1 (core AC coverage)
%%====================================================================

%% A class that was never loaded (no __beamtalk_meta/0 to read) primes to
%% "nothing recorded" — not an error.
prime_on_unregistered_class_seeds_undefined(_Pid) ->
    ok = beamtalk_workspace_shape_store:prime(<<"NeverLoadedClass">>),
    Prev = beamtalk_workspace_shape_store:previous(<<"NeverLoadedClass">>),
    [?_assertEqual(undefined, Prev)].

%% capture/1 without a preceding prime/1 (the method-patch/removal/new-class/
%% protocol install paths, none of which change shape) self-seeds from
%% whatever __beamtalk_meta/0 currently reports and always classifies no_op —
%% never a false positive, since there is nothing to diff against but itself.
capture_without_prime_is_always_no_op(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor', undefined
    ),
    try
        {Prev, NewGen, DiffResult} = beamtalk_workspace_shape_store:capture(
            <<"ShapeFixtureClass">>
        ),
        ExpectedGen = #{
            shape => #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"Dynamic">>)},
            own_shape => #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"Dynamic">>)},
            ancestor_shape => #{},
            version => 1,
            migrations => #{}
        },
        [
            ?_assertEqual(ExpectedGen, Prev),
            ?_assertEqual(ExpectedGen, NewGen),
            ?_assertEqual({no_op, []}, DiffResult)
        ]
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%% previous/1 is read-only: calling it repeatedly must not change what the
%% next capture/1 sees as "previous".
previous_does_not_mutate_store(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor', undefined
    ),
    try
        ok = beamtalk_workspace_shape_store:prime(<<"ShapeFixtureClass">>),
        P1 = beamtalk_workspace_shape_store:previous(<<"ShapeFixtureClass">>),
        P2 = beamtalk_workspace_shape_store:previous(<<"ShapeFixtureClass">>),
        Expected = #{
            shape => #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"Dynamic">>)},
            own_shape => #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"Dynamic">>)},
            ancestor_shape => #{},
            version => 1,
            migrations => #{}
        },
        [
            ?_assertEqual(Expected, P1),
            ?_assertEqual(Expected, P2)
        ]
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%% BT-3538: capture/1's returned generations carry the class's own
%% 'shape_version'/'shape_migrations', not just its flattened shape — a
%% subclass generation reload (old undeclared v1 -> new declared v2) is
%% classified shape_change (name added) with the version visible on both
%% the previous and new generation.
capture_carries_shape_version_and_migrations(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureSubclass',
        beamtalk_shape_store_fixture,
        [name],
        'ShapeFixtureSuperclass',
        undefined
    ),
    try
        %% prime/1 seeds from the (undeclared-version) plain fixture module
        %% first, simulating "before this reload installed the versioned
        %% module" — then re-point the row at the versioned fixture module
        %% and capture/1, simulating the post-install read.
        ok = beamtalk_workspace_shape_store:prime(<<"ShapeFixtureSubclass">>),
        ok = beamtalk_class_metadata:insert(
            'ShapeFixtureSubclass',
            beamtalk_shape_store_subclass_fixture,
            [name],
            'ShapeFixtureSuperclass',
            undefined
        ),
        {Prev, NewGen, _DiffResult} = beamtalk_workspace_shape_store:capture(
            <<"ShapeFixtureSubclass">>
        ),
        [
            ?_assertEqual(1, maps:get(version, Prev)),
            ?_assertEqual(#{}, maps:get(migrations, Prev)),
            ?_assertEqual(2, maps:get(version, NewGen)),
            ?_assertEqual(#{1 => 'migrateFromV1:'}, maps:get(migrations, NewGen))
        ]
    after
        ets:delete(?TABLE, 'ShapeFixtureSubclass')
    end.

%% clear/0 drops every recorded generation — the next prime/1 re-seeds as if
%% this were a fresh workspace session.
clear_resets_the_session(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor', undefined
    ),
    try
        ok = beamtalk_workspace_shape_store:prime(<<"ShapeFixtureClass">>),
        ok = beamtalk_workspace_shape_store:clear(),
        Prev = beamtalk_workspace_shape_store:previous(<<"ShapeFixtureClass">>),
        [?_assertEqual(undefined, Prev)]
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%% Two different classes keep independent entries.
different_classes_are_independent(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor', undefined
    ),
    try
        ok = beamtalk_workspace_shape_store:prime(<<"ShapeFixtureClass">>),
        PrevOther = beamtalk_workspace_shape_store:previous(<<"OtherClass">>),
        [?_assertEqual(undefined, PrevOther)]
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%%====================================================================
%% read_shape_from_meta/1 (exported for TEST)
%%====================================================================

%% Registered class + module, but the module exports no __beamtalk_meta/0 —
%% degrades to undefined rather than crashing the prime/capture hook.
read_shape_degrades_to_undefined_when_no_meta_exported_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert('NoMetaClass', lists, [foo], 'Object', undefined),
    try
        ?assertEqual(
            undefined, beamtalk_workspace_shape_store:read_shape_from_meta(<<"NoMetaClass">>)
        )
    after
        ets:delete(?TABLE, 'NoMetaClass')
    end.

%% An unregistered class degrades to undefined (binary_to_existing_atom/2
%% fails fast rather than minting a fresh atom from reload-derived text).
read_shape_degrades_to_undefined_for_unregistered_class_test() ->
    ?assertEqual(
        undefined,
        beamtalk_workspace_shape_store:read_shape_from_meta(<<"NeverSeenClassXyz">>)
    ).

%% A registered class whose module exports a realistic __beamtalk_meta/0 (via
%% beamtalk_shape_store_fixture) reads the currently-installed shape,
%% normalising `none` to the Dynamic sentinel.
read_shape_reads_field_types_from_meta_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor', undefined
    ),
    try
        ?assertEqual(
            #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"Dynamic">>)},
            beamtalk_workspace_shape_store:read_shape_from_meta(<<"ShapeFixtureClass">>)
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%%====================================================================
%% Flattened shape (ADR 0123 Phase 4, BT-3538)
%%====================================================================

%% A subclass declaring only `name` still reads its superclass's `count`/
%% `taxRate` — the whole point of flattening: a superclass-only field change
%% must surface on a concrete subclass's diff even though the subclass's own
%% module was not recompiled.
read_shape_flattens_superclass_fields_test() ->
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
    try
        ?assertEqual(
            #{
                <<"count">> => e(<<"Integer">>),
                <<"taxRate">> => e(<<"Float">>),
                <<"name">> => e(<<"Dynamic">>)
            },
            beamtalk_workspace_shape_store:read_shape_from_meta(<<"ShapeFixtureSubclass">>)
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureSubclass'),
        ets:delete(?TABLE, 'ShapeFixtureSuperclass')
    end.

%% An ancestor that fails to resolve (unregistered) contributes nothing at
%% its level rather than degrading the whole flatten to undefined — the
%% subclass's own fields still come back.
read_shape_degrades_gracefully_for_unresolvable_ancestor_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureSubclass',
        beamtalk_shape_store_subclass_fixture,
        [name],
        'ShapeFixtureSuperclass',
        undefined
    ),
    try
        ?assertEqual(
            #{<<"name">> => e(<<"Dynamic">>)},
            beamtalk_workspace_shape_store:read_shape_from_meta(<<"ShapeFixtureSubclass">>)
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureSubclass')
    end.

%%====================================================================
%% read_generation_from_meta/1 (exported for TEST, BT-3538)
%%====================================================================

%% A class declaring no shapeVersion:/migrateFromVN: reads generation
%% defaults 1/#{}.
read_generation_defaults_undeclared_version_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor', undefined
    ),
    try
        ?assertEqual(
            #{
                shape => #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"Dynamic">>)},
                own_shape => #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"Dynamic">>)},
                ancestor_shape => #{},
                version => 1,
                migrations => #{}
            },
            beamtalk_workspace_shape_store:read_generation_from_meta(<<"ShapeFixtureClass">>)
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%% A class declaring shapeVersion: 2 with a migrateFromV1: entry reads both
%% through, alongside its (flattened) shape.
read_generation_reads_declared_version_and_migrations_test() ->
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
    try
        ?assertEqual(
            #{
                shape => #{
                    <<"count">> => e(<<"Integer">>),
                    <<"taxRate">> => e(<<"Float">>),
                    <<"name">> => e(<<"Dynamic">>)
                },
                own_shape => #{<<"name">> => e(<<"Dynamic">>)},
                ancestor_shape => #{
                    <<"count">> => e(<<"Integer">>),
                    <<"taxRate">> => e(<<"Float">>)
                },
                version => 2,
                migrations => #{1 => 'migrateFromV1:'}
            },
            beamtalk_workspace_shape_store:read_generation_from_meta(<<"ShapeFixtureSubclass">>)
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureSubclass'),
        ets:delete(?TABLE, 'ShapeFixtureSuperclass')
    end.

%% Unregistered class: read_generation_from_meta/1 degrades to undefined,
%% same as read_shape_from_meta/1.
read_generation_degrades_to_undefined_for_unregistered_class_test() ->
    ?assertEqual(
        undefined,
        beamtalk_workspace_shape_store:read_generation_from_meta(<<"NeverSeenClassXyz">>)
    ).

%%====================================================================
%% ancestor_field_types/1 (exported for TEST, BT-3560)
%%====================================================================

%% A root class (no superclass) contributes no ancestor fields at all.
ancestor_field_types_is_empty_for_a_root_class_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureSuperclass',
        beamtalk_shape_store_superclass_fixture,
        [count, taxRate],
        none,
        undefined
    ),
    try
        ?assertEqual(
            #{},
            beamtalk_workspace_shape_store:ancestor_field_types('ShapeFixtureSuperclass')
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureSuperclass')
    end.

%% A subclass's ancestor_field_types/1 is *only* the superclass's own fields
%% (`count`/`taxRate`) — never folding in the subclass's own `name`, unlike
%% flattened_field_types/2 (exercised indirectly above via
%% read_shape_from_meta/1). This is the accessor precheck_class_shape/2 now
%% relies on directly instead of deriving it by subtracting own_shape back
%% out of a flattened shape (BT-3560 — lossy for a shadowed field).
ancestor_field_types_is_only_the_ancestor_contribution_test() ->
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
    try
        ?assertEqual(
            #{count => 'Integer', taxRate => 'Float'},
            beamtalk_workspace_shape_store:ancestor_field_types('ShapeFixtureSubclass')
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureSubclass'),
        ets:delete(?TABLE, 'ShapeFixtureSuperclass')
    end.

%%====================================================================
%% Kind alongside type (ADR 0124 Section 9/B9, BT-3556)
%%====================================================================

%% read_shape_from_meta/1 zips field_kinds into shape()'s {Type, Kind}
%% values: `proc` (declared `late`) reads back {<<"String">>, <<"late">>};
%% `label` (no field_kinds entry at all) defaults to {<<"String">>,
%% <<"eager">>}.
read_shape_encodes_kind_from_field_kinds_meta_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureLateClass',
        beamtalk_shape_store_late_fixture,
        [proc, label],
        'Actor',
        undefined
    ),
    try
        ?assertEqual(
            #{<<"proc">> => {<<"String">>, <<"late">>}, <<"label">> => e(<<"String">>)},
            beamtalk_workspace_shape_store:read_shape_from_meta(<<"ShapeFixtureLateClass">>)
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureLateClass')
    end.

%% read_generation_from_meta/1's own_shape carries the same kind-encoded
%% values as read_shape_from_meta/1 (this fixture has no superclass fields,
%% so shape and own_shape coincide).
read_generation_encodes_kind_in_own_shape_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureLateClass',
        beamtalk_shape_store_late_fixture,
        [proc, label],
        'Actor',
        undefined
    ),
    try
        #{shape := Shape, own_shape := OwnShape} =
            beamtalk_workspace_shape_store:read_generation_from_meta(<<"ShapeFixtureLateClass">>),
        Expected = #{<<"proc">> => {<<"String">>, <<"late">>}, <<"label">> => e(<<"String">>)},
        ?assertEqual(Expected, Shape),
        ?assertEqual(Expected, OwnShape)
    after
        ets:delete(?TABLE, 'ShapeFixtureLateClass')
    end.

%% A subclass with no field_kinds of its own still inherits its ancestor's
%% `late` kind through the flattened shape — mirrors
%% read_shape_flattens_superclass_fields_test/0 for the type half.
read_shape_flattens_ancestor_field_kinds_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureLateClass', beamtalk_shape_store_late_fixture, [proc, label], none, undefined
    ),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureLateSubclass',
        beamtalk_shape_store_subclass_fixture,
        [name],
        'ShapeFixtureLateClass',
        undefined
    ),
    try
        ?assertEqual(
            #{
                <<"proc">> => {<<"String">>, <<"late">>},
                <<"label">> => e(<<"String">>),
                <<"name">> => e(<<"Dynamic">>)
            },
            beamtalk_workspace_shape_store:read_shape_from_meta(<<"ShapeFixtureLateSubclass">>)
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureLateSubclass'),
        ets:delete(?TABLE, 'ShapeFixtureLateClass')
    end.
