%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Hot Reload Context

-module(beamtalk_shape_migration_tests).

-moduledoc """
EUnit tests for beamtalk_shape_migration (ADR 0123 Phase 2, BT-3536).

`migrate/3` is exercised against real registered classes — `ShapeChainCart`
unmocked (defaults: no `'shape_version'`/`'shape_migrations'` in its meta
yet, Phase 3 has not shipped), `TypedFieldCounter` (typed-no-default
reconcile failure), and `ShapeChainCart` again with its `__beamtalk_meta/0`
meck'd to report those keys for real (a real two-step
`migrateFromV1:`/`migrateFromV2:` chain, invoked via `local_call/3` — meck
stands in only for the meta lookup Phase 3 will eventually populate for
real; the hooks themselves are real compiled methods). `pack/1`/`unpack/1`
are exercised against hand-built tagged instance maps — pack/unpack care
about a class's registered shape, not how an instance came to exist, so
there is no need to actually spawn one via `new`. Every fixture here is
this test file's own (never a class shared with another test file's
setup), so there is no cross-file ordering dependency on when some other
suite happens to register it first.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Fixture
%%====================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_stdlib:init(),
    ok = ensure_fixture_loaded(typed_field_counter, 'TypedFieldCounter'),
    ok = ensure_fixture_loaded(shape_chain_cart, 'ShapeChainCart'),
    ok = ensure_fixture_loaded(shape_point, 'ShapePoint'),
    ok = ensure_fixture_loaded(shape_box, 'ShapeBox'),
    ok = ensure_fixture_loaded(shape_handle_box, 'ShapeHandleBox'),
    ok = ensure_fixture_loaded(shape_hazard_worker, 'ShapeHazardWorker'),
    ok = ensure_fixture_loaded(shape_hazard_cart, 'ShapeHazardCart'),
    ok = ensure_fixture_loaded(shape_handle_cart, 'ShapeHandleCart'),
    ok.

teardown(_) ->
    ok.

shape_migration_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"absent shape_version/shape_migrations meta default to 1/#{}",
                fun test_migrate_defaults_when_meta_absent/0},
            {"unregistered class fails with class_not_found",
                fun test_migrate_unregistered_class_fails/0},
            {"chain order v1 -> v2 -> v3 via real local_call/3 hooks",
                fun test_migrate_chain_order_via_real_hooks/0},
            {"a gap step no-ops; reconcile runs once at the end",
                fun test_migrate_gap_step_then_single_reconcile/0},
            {"downgrade (To < From) runs reconcile only",
                fun test_migrate_downgrade_runs_reconcile_only/0},
            {"idempotent when From =:= To", fun test_migrate_idempotent_when_current/0},
            {"typed field left unset by the chain fails the migration",
                fun test_migrate_typed_field_unset_fails/0},
            {"typed field with a declared default still fails when init/1 is unavailable",
                fun test_migrate_typed_field_with_default_fails_when_init_unavailable/0},
            {"undeclared field is dropped with a warning",
                fun test_migrate_drops_undeclared_field/0},
            {"pack/1 rejects a SendableRef (Actor-typed) field",
                fun test_pack_rejects_sendable_ref_field/0},
            {"pack/1 rejects a HandleScoped field", fun test_pack_rejects_handle_scoped_field/0},
            {"pack/1 packs a nested Value field recursively; unpack/1 reverses it",
                fun test_pack_unpack_nested_value_round_trip/0},
            {"pack/1 passes a builtin Array field through as a raw term",
                fun test_pack_passthrough_builtin_array_field/0},
            {"unpack/1 rejects an envelope nested past the recursion depth limit",
                fun test_unpack_rejects_excessive_nesting/0}
        ]
    end}.

%%====================================================================
%% migrate/3 — meta defaults, class resolution
%%====================================================================

test_migrate_defaults_when_meta_absent() ->
    %% ShapeChainCart has no 'shape_version'/'shape_migrations' key in its
    %% real (unmocked) meta (Phase 3 has not shipped) — migrate/3 must still
    %% work, defaulting to ToVersion 1 and an empty migrations table. Every
    %% declared field is already present, so nothing is added or dropped.
    Fields = #{itemCount => 5, total => 0, tag => <<"none">>},
    {ok, NewFields, ToVersion} = beamtalk_shape_migration:migrate('ShapeChainCart', 1, Fields),
    ?assertEqual(Fields, NewFields),
    ?assertEqual(1, ToVersion).

test_migrate_unregistered_class_fails() ->
    {error, Reason} = beamtalk_shape_migration:migrate('ShapeMigrationNoSuchClass', 1, #{}),
    ?assertMatch(
        #beamtalk_error{kind = class_not_found, class = 'ShapeMigrationNoSuchClass'}, Reason
    ).

%%====================================================================
%% Chain order, gaps, downgrade, idempotency
%%====================================================================

test_migrate_chain_order_via_real_hooks() ->
    with_shape_chain_cart_meta(3, #{1 => 'migrateFromV1:', 2 => 'migrateFromV2:'}, fun() ->
        {ok, NewFields, ToVersion} =
            beamtalk_shape_migration:migrate('ShapeChainCart', 1, #{itemCount => 4}),
        %% migrateFromV1: sets total := itemCount * 10 (= 40); migrateFromV2:
        %% then sets tag := "migrated" — both real class-side methods,
        %% invoked via local_call/3.
        ?assertEqual(3, ToVersion),
        ?assertEqual(4, maps:get(itemCount, NewFields)),
        ?assertEqual(40, maps:get(total, NewFields)),
        ?assertEqual(<<"migrated">>, maps:get(tag, NewFields))
    end).

test_migrate_gap_step_then_single_reconcile() ->
    %% Only v1 has a declared hook; v2 is a gap. The chain's raw result
    %% therefore never sets `tag` — the single final reconcile pass fills
    %% it from the class's own declared default ("none"), not from any
    %% step in between.
    with_shape_chain_cart_meta(3, #{1 => 'migrateFromV1:'}, fun() ->
        {ok, NewFields, ToVersion} =
            beamtalk_shape_migration:migrate('ShapeChainCart', 1, #{itemCount => 2}),
        ?assertEqual(3, ToVersion),
        ?assertEqual(20, maps:get(total, NewFields)),
        ?assertEqual(<<"none">>, maps:get(tag, NewFields))
    end).

test_migrate_downgrade_runs_reconcile_only() ->
    %% From (3) > To (1, meck'd as the class's "current" version) — no
    %% chain steps run (migrateFromV1: would raise via should_not_run/0 if
    %% it did); reconcile still fills declared fields from their defaults.
    with_shape_chain_cart_meta(1, #{1 => 'shouldNotRun:'}, fun() ->
        {ok, NewFields, ToVersion} =
            beamtalk_shape_migration:migrate('ShapeChainCart', 3, #{itemCount => 7}),
        ?assertEqual(1, ToVersion),
        ?assertEqual(7, maps:get(itemCount, NewFields)),
        %% total/tag were never chained — reconcile defaults them.
        ?assertEqual(0, maps:get(total, NewFields)),
        ?assertEqual(<<"none">>, maps:get(tag, NewFields))
    end).

test_migrate_idempotent_when_current() ->
    with_shape_chain_cart_meta(2, #{1 => 'shouldNotRun:'}, fun() ->
        Fields = #{itemCount => 1, total => 10, tag => <<"kept">>},
        {ok, NewFields, ToVersion} = beamtalk_shape_migration:migrate('ShapeChainCart', 2, Fields),
        ?assertEqual(2, ToVersion),
        ?assertEqual(Fields, NewFields)
    end).

%%====================================================================
%% Reconcile failure/warning modes
%%====================================================================

test_migrate_typed_field_unset_fails() ->
    %% TypedFieldCounter's `label :: String` has no declared default and no
    %% migration touches it — a migration may not leave a typed slot unset.
    {error, Reason} = beamtalk_shape_migration:migrate('TypedFieldCounter', 1, #{value => 3}),
    ?assertMatch(
        #beamtalk_error{
            kind = shape_migration_failed, class = 'TypedFieldCounter', selector = label
        },
        Reason
    ).

%% Review finding (BT-3536 PR): a typed field that *declares* a default is
%% not exempt from the "may not leave a typed slot unset" rule when the
%% real default can't actually be computed. TypedFieldCounter's
%% `value :: Integer = 0` has a declared default; `label` is supplied so
%% only `value` is missing, and init/1 is meck'd unavailable so
%% safe_init_defaults/2 degrades to `#{}` — silently defaulting `value` to
%% `nil` here would defeat the typed-slot invariant just as surely as the
%% no-default case above.
test_migrate_typed_field_with_default_fails_when_init_unavailable() ->
    meck:new('bt@typed_field_counter', [passthrough]),
    meck:expect('bt@typed_field_counter', init, fun(_Args) -> {error, boom} end),
    try
        {error, Reason} = beamtalk_shape_migration:migrate(
            'TypedFieldCounter', 1, #{label => <<"x">>}
        ),
        ?assertMatch(
            #beamtalk_error{
                kind = shape_migration_failed, class = 'TypedFieldCounter', selector = value
            },
            Reason
        )
    after
        meck:unload('bt@typed_field_counter')
    end.

test_migrate_drops_undeclared_field() ->
    Fields = #{itemCount => 1, total => 0, tag => <<"none">>, ghost => true},
    {ok, NewFields, _ToVersion} = beamtalk_shape_migration:migrate('ShapeChainCart', 1, Fields),
    ?assertNot(maps:is_key(ghost, NewFields)),
    ?assertEqual(1, maps:get(itemCount, NewFields)).

%%====================================================================
%% pack/1 — Sendable-tier walk
%%====================================================================

test_pack_rejects_sendable_ref_field() ->
    Instance = #{
        '$beamtalk_class' => 'ShapeHazardCart',
        '__shape_version__' => 1,
        worker => nil,
        label => <<"cart">>
    },
    {error, Reason} = beamtalk_shape_migration:pack(Instance),
    ?assertMatch(
        #beamtalk_error{kind = not_serialisable, class = 'ShapeHazardCart', selector = worker},
        Reason
    ).

test_pack_rejects_handle_scoped_field() ->
    Instance = #{
        '$beamtalk_class' => 'ShapeHandleCart',
        '__shape_version__' => 1,
        handle => nil,
        label => <<"cart">>
    },
    {error, Reason} = beamtalk_shape_migration:pack(Instance),
    ?assertMatch(
        #beamtalk_error{kind = not_serialisable, class = 'ShapeHandleCart', selector = handle},
        Reason
    ).

%%====================================================================
%% pack/1 + unpack/1 — nested Value packing, builtin pass-through
%%====================================================================

test_pack_unpack_nested_value_round_trip() ->
    Origin = #{'$beamtalk_class' => 'ShapePoint', x => 3, y => 4},
    History = #{'$beamtalk_class' => 'Array', data => #{0 => <<"first">>}},
    Instance = #{
        '$beamtalk_class' => 'ShapeBox',
        '__shape_version__' => 1,
        origin => Origin,
        history => History,
        label => <<"hi">>
    },
    {ok, Envelope} = beamtalk_shape_migration:pack(Instance),
    {beamtalk_shape, 'ShapeBox', 1, PackedFields} = Envelope,
    %% The nested Value field gained its own versioned envelope.
    ?assertEqual(
        {beamtalk_shape, 'ShapePoint', 1, #{x => 3, y => 4}}, maps:get(origin, PackedFields)
    ),
    %% The builtin Array field passed through as a raw term, unversioned.
    ?assertEqual(History, maps:get(history, PackedFields)),
    ?assertEqual(<<"hi">>, maps:get(label, PackedFields)),

    {ok, Unpacked} = beamtalk_shape_migration:unpack(Envelope),
    ?assertEqual('ShapeBox', maps:get('$beamtalk_class', Unpacked)),
    ?assertEqual(1, maps:get('__shape_version__', Unpacked)),
    ?assertEqual(<<"hi">>, maps:get(label, Unpacked)),
    ?assertEqual(History, maps:get(history, Unpacked)),
    %% The nested envelope was unpacked back into a tagged instance map.
    ?assertEqual(
        #{'$beamtalk_class' => 'ShapePoint', '__shape_version__' => 1, x => 3, y => 4},
        maps:get(origin, Unpacked)
    ).

test_pack_passthrough_builtin_array_field() ->
    History = #{'$beamtalk_class' => 'Array', data => #{0 => 1, 1 => 2}},
    Instance = #{
        '$beamtalk_class' => 'ShapeBox',
        '__shape_version__' => 1,
        origin => #{'$beamtalk_class' => 'ShapePoint', x => 0, y => 0},
        history => History,
        label => <<"box">>
    },
    {ok, {beamtalk_shape, 'ShapeBox', 1, PackedFields}} = beamtalk_shape_migration:pack(Instance),
    %% Bit-for-bit unchanged — not re-tagged, not re-versioned.
    ?assertEqual(History, maps:get(history, PackedFields)).

%% unpack/1 recurses on any {beamtalk_shape, ...} value it finds inside a
%% field dictionary, regardless of the outer class's own declared field
%% types (persistence/distribution data is not obliged to match a live
%% class's current shape) — so a synthetic, artificially deep chain of
%% ShapePoint envelopes exercises the depth guard without needing a
%% self-referential fixture class.
test_unpack_rejects_excessive_nesting() ->
    TooDeep = build_nested_shape_point_envelope(40),
    {error, Reason} = beamtalk_shape_migration:unpack(TooDeep),
    ?assertMatch(#beamtalk_error{kind = not_serialisable}, Reason).

build_nested_shape_point_envelope(0) ->
    {beamtalk_shape, 'ShapePoint', 1, #{x => 0, y => 0}};
build_nested_shape_point_envelope(N) ->
    {beamtalk_shape, 'ShapePoint', 1, #{x => build_nested_shape_point_envelope(N - 1), y => 0}}.

%%====================================================================
%% Helpers
%%====================================================================

%% Meck ShapeChainCart's __beamtalk_meta/0 to report `shape_version` and
%% `shape_migrations` — the Phase 3 language surface (shapeVersion:,
%% migrateFromVN: parsing and meta emission) has not shipped yet, so these
%% keys do not exist on any compiled class today (ADR 0123 § Runtime
%% contract: absent means 1/#{}). The migrateFromV1:/migrateFromV2: class
%% methods themselves are real, compiled, and invoked via local_call/3 —
%% only the meta lookup is stubbed.
with_shape_chain_cart_meta(ShapeVersion, Migrations, Fun) ->
    meck:new('bt@shape_chain_cart', [passthrough]),
    meck:expect('bt@shape_chain_cart', '__beamtalk_meta', fun() ->
        Real = meck:passthrough([]),
        Real#{shape_version => ShapeVersion, shape_migrations => Migrations}
    end),
    try
        Fun()
    after
        meck:unload('bt@shape_chain_cart')
    end.

%% Load a fixture .bt module (bt@<Basename>) and register its class if it
%% is not already registered — idempotent, same convention
%% beamtalk_hot_reload_tests.erl's several ensure_*_loaded/0 helpers use,
%% generalised here since this module has more fixtures than any one of
%% them repeats individually.
ensure_fixture_loaded(Basename, ClassName) ->
    Module = list_to_atom("bt@" ++ atom_to_list(Basename)),
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    case erlang:function_exported(Module, register_class, 0) of
                        true ->
                            Module:register_class(),
                            ok;
                        false ->
                            ok
                    end;
                _Pid ->
                    ok
            end;
        {error, Reason} ->
            error({fixture_module_not_found, Basename, Reason})
    end.
