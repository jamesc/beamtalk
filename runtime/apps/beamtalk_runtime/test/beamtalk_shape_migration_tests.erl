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

%% Logger handler callback for the fun-outside-table warning test below.
-export([log/2]).

%% Called from the dynamically-compiled meta-stub module installed by
%% with_shape_chain_cart_meta/3 below — must be exported since it's invoked
%% as beamtalk_shape_migration_tests:apply_meta_override/1 from that other
%% module.
-export([apply_meta_override/1]).

log(LogEvent, #{config := #{parent := Parent}}) ->
    Parent ! {log_event, LogEvent},
    ok.

apply_meta_override(Real) ->
    {ShapeVersion, Migrations} = get(bt_3536_shape_chain_cart_meta_override),
    Real#{shape_version => ShapeVersion, shape_migrations => Migrations}.

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
    ok = ensure_fixture_loaded(shape_plain_object, 'ShapePlainObject'),
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
            {"a migrateFromV* class method absent from shape_migrations warns (ADR 0123 §2)",
                fun test_migrate_warns_when_class_method_outside_shape_migrations_table/0},
            {"migrate/4 with skip_stray_warning => true suppresses the warning (BT-3543)",
                fun test_migrate_skip_stray_warning_suppresses_warning/0},
            {"check_stray_migrations/1 warns directly, independent of migrate (BT-3543)",
                fun test_check_stray_migrations_warns_directly/0},
            {"field_tier/1 matches the shared compile-time conformance corpus (BT-3542)",
                fun test_sendability_tier_conformance_matches_shared_corpus/0},
            {"field_tier/1 does not compose a generic annotation's type_args (BT-3542)",
                fun test_field_tier_does_not_compose_generic_type_args/0},
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
%% only `value` is missing, and init/1 is stubbed unavailable so
%% safe_init_defaults/2 degrades to `#{}` — silently defaulting `value` to
%% `nil` here would defeat the typed-slot invariant just as surely as the
%% no-default case above.
%%
%% `meck:new('bt@typed_field_counter', [passthrough])` can't be used here
%% (see with_class_module_override/3's moduledoc): `.bt` classes compile
%% straight to Core Erlang, so their `.beam` carries an empty
%% `raw_abstract_v1` chunk rather than real abstract code, and meck's
%% unconditional (any option set) recompile-from-forms step in
%% `backup_original/4` crashes on it. A disposable delegate-proxy stub
%% (build_delegate_proxy/3) with only `init/1` overridden, installed via
%% with_class_module_override/3, gets the same effect without going through
%% meck at all. The stub must still forward `'__beamtalk_meta'/0` to the
%% real module rather than omitting it: `beamtalk_shape_migration:reconcile/5`
%% reads `is_typed` from the stub's meta (falling back to `false` when a
%% module has no `'__beamtalk_meta'/0` at all — see `read_meta/1`), and this
%% test's whole point is exercising the *typed*-slot-unset path.
test_migrate_typed_field_with_default_fails_when_init_unavailable() ->
    RealMod = 'bt@typed_field_counter',
    StubMod = bt_3536_typed_field_counter_bad_init,
    build_delegate_proxy(RealMod, StubMod, [
        {{init, 1}, "init(_Args) -> {error, boom}."}
    ]),
    try
        with_class_module_override('TypedFieldCounter', StubMod, fun() ->
            {error, Reason} = beamtalk_shape_migration:migrate(
                'TypedFieldCounter', 1, #{label => <<"x">>}
            ),
            ?assertMatch(
                #beamtalk_error{
                    kind = shape_migration_failed, class = 'TypedFieldCounter', selector = value
                },
                Reason
            )
        end)
    after
        code:purge(StubMod),
        code:delete(StubMod)
    end.

test_migrate_drops_undeclared_field() ->
    Fields = #{itemCount => 1, total => 0, tag => <<"none">>, ghost => true},
    {ok, NewFields, _ToVersion} = beamtalk_shape_migration:migrate('ShapeChainCart', 1, Fields),
    ?assertNot(maps:is_key(ghost, NewFields)),
    ?assertEqual(1, maps:get(itemCount, NewFields)).

%% ADR 0123 §2: `ShapeChainCart`'s `migrateFromV2:` is a real, installed
%% class-side method (see the fixture), but this scenario's meck'd
%% `shape_migrations` table only lists `migrateFromV1:` — modelling a fun
%% installed outside the compiler-emitted table (e.g. a bare
%% `ClassBuilder addClassMethod:body:` patch with no recompile). `migrate/3`
%% must still succeed (the gap is a no-op step, same as any other gap) while
%% also warning that `migrateFromV2:` exists but will not run.
test_migrate_warns_when_class_method_outside_shape_migrations_table() ->
    logger:set_primary_config(level, all),
    HandlerId = bt_3537_stray_migration_warning_test_handler,
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        config => #{parent => self()},
        level => all
    }),
    try
        with_shape_chain_cart_meta(3, #{1 => 'migrateFromV1:'}, fun() ->
            {ok, NewFields, ToVersion} =
                beamtalk_shape_migration:migrate('ShapeChainCart', 1, #{itemCount => 2}),
            ?assertEqual(3, ToVersion),
            %% migrateFromV1: still ran (it IS in the table); migrateFromV2:
            %% is a gap from the chain's point of view — reconcile defaults
            %% `tag`, it does not run the stray method.
            ?assertEqual(20, maps:get(total, NewFields)),
            ?assertEqual(<<"none">>, maps:get(tag, NewFields)),
            ?assert(receive_stray_migration_warning('ShapeChainCart', 'migrateFromV2:', 5))
        end)
    after
        logger:remove_handler(HandlerId),
        logger:set_primary_config(level, error)
    end.

%% BT-3543: the same scenario as
%% test_migrate_warns_when_class_method_outside_shape_migrations_table/0, but
%% via migrate/4 with skip_stray_warning => true — the per-instance hot-reload
%% call path (beamtalk_hot_reload:migrate_state/3) — which must not warn,
%% since beamtalk_repl_loader:hot_reload_class/2 already ran the check once
%% for the class before fanning out to instances.
test_migrate_skip_stray_warning_suppresses_warning() ->
    logger:set_primary_config(level, all),
    HandlerId = bt_3543_skip_stray_warning_test_handler,
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        config => #{parent => self()},
        level => all
    }),
    try
        with_shape_chain_cart_meta(3, #{1 => 'migrateFromV1:'}, fun() ->
            {ok, _NewFields, ToVersion} =
                beamtalk_shape_migration:migrate('ShapeChainCart', 1, #{itemCount => 2}, #{
                    skip_stray_warning => true
                }),
            ?assertEqual(3, ToVersion),
            ?assertNot(receive_stray_migration_warning('ShapeChainCart', 'migrateFromV2:', 2))
        end)
    after
        logger:remove_handler(HandlerId),
        logger:set_primary_config(level, error)
    end.

%% BT-3543: check_stray_migrations/1 is the once-per-reload call site
%% (beamtalk_repl_loader:hot_reload_class/2) — it must still warn on its own,
%% with no migrate/3-4 call at all.
test_check_stray_migrations_warns_directly() ->
    logger:set_primary_config(level, all),
    HandlerId = bt_3543_check_stray_migrations_test_handler,
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        config => #{parent => self()},
        level => all
    }),
    try
        with_shape_chain_cart_meta(3, #{1 => 'migrateFromV1:'}, fun() ->
            ok = beamtalk_shape_migration:check_stray_migrations('ShapeChainCart'),
            ?assert(receive_stray_migration_warning('ShapeChainCart', 'migrateFromV2:', 5))
        end)
    after
        logger:remove_handler(HandlerId),
        logger:set_primary_config(level, error)
    end.

%% Drains up to N pending {log_event, ...} messages looking for the
%% stray-migration warning naming Class and Selector.
receive_stray_migration_warning(_Class, _Selector, 0) ->
    false;
receive_stray_migration_warning(Class, Selector, N) ->
    receive
        {log_event, #{level := warning, meta := Meta}} ->
            case
                maps:get(class, Meta, undefined) =:= Class andalso
                    lists:member(Selector, maps:get(selectors, Meta, []))
            of
                true -> true;
                false -> receive_stray_migration_warning(Class, Selector, N - 1)
            end
    after 1000 ->
        false
    end.

%%====================================================================
%% field_tier/1 — cross-boundary sendability conformance (BT-3542)
%%====================================================================

%% BT-3542: field_tier/1's kind-based branches (`ClassMeta`'s `kind`/
%% `handle_scope` keys) necessarily re-derive the same core mapping the
%% compile-time checker's kind-based fallback
%% (`sendability.rs`'s `tier_of_known`) already encodes — the two cannot
%% literally share code across the Rust/Erlang boundary (ADR 0123
%% commissions this walk as new work, not a port). This corpus is the
%% single source of truth both are pinned to; the Rust side asserts the
%% identical cases in
%% `sendability::tests::runtime_field_tier_kind_mapping_matches_compile_time_base_tier`.
test_sendability_tier_conformance_matches_shared_corpus() ->
    Cases = beamtalk_test_corpus:load_json_fixture([
        "runtime",
        "apps",
        "beamtalk_runtime",
        "test",
        "fixtures",
        "sendability_tier_conformance.json"
    ]),
    ?assert(length(Cases) > 0),
    lists:foreach(
        fun(Case) ->
            ClassNameBin = maps:get(<<"class_name">>, Case),
            ExpectedBin = maps:get(<<"erlang_field_tier">>, Case),
            Why = maps:get(<<"why">>, Case, <<>>),
            ClassName = binary_to_existing_atom(ClassNameBin, utf8),
            Expected = binary_to_existing_atom(ExpectedBin, utf8),
            ?assertEqual(
                Expected,
                beamtalk_shape_migration:field_tier(ClassName),
                {corpus_mismatch, ClassName, Why}
            )
        end,
        Cases
    ).

%% BT-3542 acceptance criterion 3: pins the documented scope gap down as a
%% regression, rather than only a doc comment — `List(Port)` grades on
%% `List`'s own kind (`value_nested`, a `Collection` → `Value`) here, not
%% `Port`'s `handle_scoped` hazard the compile-time checker's generic
%% `type_args` composition would produce (see
%% `sendability.rs::tests::generic_collection_composes_element_tier`, the
%% compile-time counterpart that DOES compose). If this starts asserting
%% `handle_scoped`, field_tier/1 gained generic composition — update this
%% test (and the field_tier/1 doc) deliberately rather than treating it as
%% a stale assertion to relax.
test_field_tier_does_not_compose_generic_type_args() ->
    ?assertEqual(value_nested, beamtalk_shape_migration:field_tier('List(Port)')).

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

%% Stub ShapeChainCart's __beamtalk_meta/0 to report `shape_version` and
%% `shape_migrations` — the Phase 3 language surface (shapeVersion:,
%% migrateFromVN: parsing and meta emission) has not shipped yet, so these
%% keys do not exist on any compiled class today (ADR 0123 § Runtime
%% contract: absent means 1/#{}). The migrateFromV1:/migrateFromV2: class
%% methods themselves are real, compiled, and invoked via local_call/3 —
%% only the meta lookup is stubbed.
%%
%% `meck:new('bt@shape_chain_cart', [passthrough])` + `meck:passthrough/1`
%% inside the expectation can't be used here: see
%% with_class_module_override/3's moduledoc for why meck can never back up
%% a `.bt` class's original code. The stub is a build_delegate_proxy/3
%% delegate proxy with only `'__beamtalk_meta'/0` overridden — it calls the
%% real one directly (an ordinary remote call, no meck involved) and
%% overlays the requested keys via apply_meta_override/1.
with_shape_chain_cart_meta(ShapeVersion, Migrations, Fun) ->
    RealMod = 'bt@shape_chain_cart',
    StubMod = bt_3536_shape_chain_cart_meta_stub,
    put(bt_3536_shape_chain_cart_meta_override, {ShapeVersion, Migrations}),
    OverrideSrc =
        "'__beamtalk_meta'() -> "
        "Real = apply('" ++ atom_to_list(RealMod) ++
            "', '__beamtalk_meta', []), "
            "beamtalk_shape_migration_tests:apply_meta_override(Real).",
    build_delegate_proxy(RealMod, StubMod, [{{'__beamtalk_meta', 0}, OverrideSrc}]),
    try
        with_class_module_override('ShapeChainCart', StubMod, Fun)
    after
        code:purge(StubMod),
        code:delete(StubMod),
        erase(bt_3536_shape_chain_cart_meta_override)
    end.

%% Build and load a disposable module named StubMod that delegates every
%% export of RealMod to RealMod via apply/3, except the {Name, Arity} pairs
%% named in OverrideSources, whose given source text (a complete
%% "name(Args) -> Body." form) is used instead.
%%
%% Used in place of `meck:new(RealMod, [passthrough])`: `.bt` classes
%% compile straight to Core Erlang via
%% `compile:forms(..., [from_core | Opts])` (CLAUDE.md), which never
%% produces a real `raw_abstract_v1` abstract-code chunk — the `.beam` still
%% carries the chunk (so meck's `abstract_code/1` doesn't throw
%% `no_abstract_code`), but with an empty forms list. meck's
%% `backup_original/4` reads it and recompiles it via `compile:forms/2`
%% *unconditionally*, regardless of the `passthrough` option — an empty
%% forms list degrades that recompile to `{error, ...}` rather than
%% raising, and `meck_code:compile_and_load_forms/2` turns that into
%% `exit({compile_forms, {error, ...}})`, killing the meck_proc gen_server
%% the instant `meck:new/2` is called on any `bt@...` module (the flaky
%% "N cancelled" `test-runtime` failures across the BT-3531..BT-3537
%% overnight runs). The proxy built here needs no backup step at all: it's
%% compiled fresh from a real export list (via beam_lib, so it reflects the
%% actual compiled `.beam` rather than a hand-kept list that could drift)
%% and every non-overridden call forwards straight to RealMod, still fully
%% loaded and untouched throughout.
build_delegate_proxy(RealMod, StubMod, OverrideSources) ->
    {ok, {RealMod, [{exports, Exports}]}} = beam_lib:chunks(code:which(RealMod), [exports]),
    ExportsDecl =
        "-export([" ++
            string:join(
                ["'" ++ atom_to_list(F) ++ "'/" ++ integer_to_list(A) || {F, A} <- Exports],
                ", "
            ) ++
            "]).",
    Bodies = [
        case lists:keyfind({F, A}, 1, OverrideSources) of
            {_, Source} -> Source;
            false -> delegate_source(RealMod, F, A)
        end
     || {F, A} <- Exports
    ],
    Forms = beamtalk_test_erl_forms:parse_forms(
        ["-module(" ++ atom_to_list(StubMod) ++ ")." | [ExportsDecl | Bodies]]
    ),
    {ok, StubMod, Bin} = compile:forms(Forms, [return_errors]),
    {module, StubMod} = code:load_binary(StubMod, atom_to_list(StubMod) ++ ".erl", Bin),
    ok.

%% Source text for a single delegate-proxy function body that forwards its
%% arguments to RealMod via apply/3.
delegate_source(RealMod, F, A) ->
    Args = string:join(["A" ++ integer_to_list(N) || N <- lists:seq(1, A)], ", "),
    QuotedF = "'" ++ atom_to_list(F) ++ "'",
    QuotedF ++
        "(" ++ Args ++ ") -> apply('" ++ atom_to_list(RealMod) ++ "', " ++ QuotedF ++ ", [" ++
        Args ++ "]).".

%% Temporarily repoint Class's registered module (beamtalk_class_metadata's
%% class->module row) to TempModule for the duration of Fun/0, restoring
%% the original row afterward — used instead of `meck:new/2` on a compiled
%% `.bt` module (see build_delegate_proxy/3's moduledoc for why). Swapping
%% the class->module row never touches meck or recompiles anything:
%% `beamtalk_class_registry`'s pid-based method dispatch (used by
%% `local_call/3`, e.g. for `migrateFromVN:` hooks) is a separate table,
%% keyed and populated independently of `beamtalk_class_metadata`, so it is
%% unaffected by this swap.
%%
%% Uses `merge_identity/5`, not `insert/5`: `ClassName` already has a row
%% (this is an update, not row creation), and `insert/5`'s own moduledoc
%% warns that overwriting an existing row resets `has_runtime_class_methods`
%% to `false` on every call — `merge_identity/5` updates the same four
%% fields without touching that gate, so the "restore" leaves the row
%% exactly as it was, not just field-for-field equal.
with_class_module_override(ClassName, TempModule, Fun) ->
    {ok, OrigModule, Selectors} = beamtalk_class_metadata:lookup_methods(ClassName),
    {ok, Superclass} = beamtalk_class_metadata:lookup_superclass(ClassName),
    {ok, IsAbstract} = beamtalk_class_metadata:lookup_is_abstract(ClassName),
    ok = beamtalk_class_metadata:merge_identity(
        ClassName, TempModule, Selectors, Superclass, IsAbstract
    ),
    try
        Fun()
    after
        beamtalk_class_metadata:merge_identity(
            ClassName, OrigModule, Selectors, Superclass, IsAbstract
        )
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
