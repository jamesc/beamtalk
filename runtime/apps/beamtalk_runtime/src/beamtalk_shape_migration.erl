%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_migration).

%%% **DDD Context:** Hot Reload Context

-moduledoc """
Shape migration lookups and effects (ADR 0123 Phase 2, BT-3536).

Sits beside `beamtalk_hot_reload` — it needs the class registry and
`beamtalk_object_class`, so it is not a leaf like `beamtalk_shape_chain`.
`migrate/3` resolves a class's module and migration table from
`__beamtalk_meta`, runs the pure chain (`beamtalk_shape_chain:migrate/4`),
then reconciles the result against the class's **flattened** declared
fields — the same `classAllFieldNames/1` semantics BT-3531 fixed hot reload
to use. `pack/1`/`unpack/1` build the versioned envelope that persistence
and distribution (BT-3527) consume; `beamtalk_hot_reload:code_change/3`
delegates its field migration to `migrate/3` in this same change.

**Reads `__beamtalk_meta`'s `'shape_version'`/`'shape_migrations'` with
defaults `1`/`#{}`** — Phase 3 (`shapeVersion:`, `migrateFromVN:` parsing and
meta emission) has not shipped yet, so every class today resolves to
`ToVersion = 1`, `Migrations = #{}`, and `migrate/3` degrades to its
reconcile step alone. That is intentional (ADR 0123 § Runtime contract) —
this module works standalone, ahead of the language surface that will
populate those keys.

**Migrations run in the calling process**, not the class's gen_server:
`invoke_hook/3` resolves the class's registered class object and calls
`beamtalk_object_class:local_call/3` (`performLocally:withArguments:`'s
Erlang surface) — proven safe mid-reload by BT-3535's napkin spike. A raised
exception, a `does_not_understand` (stale/mismatched migrations table), or a
non-`Dictionary` hook result all become a `{error, {Step, Selector, Reason}}`
from `beamtalk_shape_chain:migrate/4`, wrapped here as
`#beamtalk_error{kind = shape_migration_failed}`.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    migrate/3, migrate/4,
    check_stray_migrations/1,
    pack/1,
    pack_wire/1,
    unpack/1,
    unpack_strict/1,
    resolve_migrations/1,
    %% ADR 0126 §5.1/§5.4: beamtalk_wire's runtime class→tier dispatch routes
    %% through this same table (BT-3542 conformance-pinned against
    %% sendability.rs) — no second tier table. field_tier/1 was previously
    %% exported `-ifdef(TEST)` only (for beamtalk_shape_migration_tests'
    %% conformance test); it is now a genuine production dependency of
    %% beamtalk_wire, so it is a plain export like the rest of this list.
    field_tier/1,
    max_pack_depth/0
]).

-export_type([envelope/0]).

-type envelope() ::
    {beamtalk_shape, Class :: atom(), ShapeVersion :: pos_integer(), Fields :: map()}.

-doc """
Sendability/persistence policy threaded through `pack/3`'s internal walk
(ADR 0126 §5.4): `persist` is `pack/1`'s existing policy (`SendableRef`
fields rejected — a pid has no meaningful on-disk shape); `wire` is
`pack_wire/1`'s policy (`SendableRef` fields pass — pids are node-qualified
natively and fine to ship). `HandleScoped` is rejected under both.
""".
-type pack_policy() :: persist | wire.

-doc """
Strictness threaded through `unpack/3`'s internal walk (ADR 0125 §3.4):
`lenient` is `unpack/1`'s existing behaviour (migrates forward or backward,
silently truncating fields a downgraded receiver does not declare — the
right policy for persistence); `strict` is `unpack_strict/1`'s policy (an
envelope newer than this node's own declared `shapeVersion` for that class
is refused before the migration chain runs, rather than silently guessed
at — the right policy for a wire receiver that must never truncate an
already-upgraded peer's message).
""".
-type unpack_policy() :: lenient | strict.

%%====================================================================
%% migrate/3
%%====================================================================

-doc """
Run the shape migration chain from `FromVersion` to `Class`'s current
`shapeVersion`, then reconcile against its flattened declared fields.

`Fields` is user fields only (no internal keys such as `'$beamtalk_class'` or
`'__shape_version__'`) — `beamtalk_hot_reload:code_change/3` strips those
before calling in and re-attaches them to the result. Runs in the calling
process (see moduledoc).

Chain semantics (ADR 0123 § Runtime contract):
1. Resolve `Module` from `Class` via `beamtalk_class_metadata:lookup_module/1`
   — `{error, class_not_found}` if `Class` is not registered.
2. `ToVersion = __beamtalk_meta's 'shape_version'` (default `1`),
   `Migrations = __beamtalk_meta's 'shape_migrations'` (default `#{}`).
3. Run `beamtalk_shape_chain:migrate/4` from `FromVersion` to `ToVersion`.
   `ToVersion =< FromVersion` (downgrade or already-current) runs no steps.
4. Reconcile the chain's result against the flattened declared field list:
   a declared field present is kept, regardless of kind; absent falls back
   to `Module:init(#{'__skip_initialize__' => true})`'s default when the
   field declares one; absent with no default is `nil` on an untyped class
   and a `shape_migration_failed` error on a `typed` one (a migration may
   not leave a typed slot unset — the same promise ADR 0078's post-
   `initialize` check makes at spawn time) — **except** a `late` field
   (`classAllFieldKindsByName/1`, ADR 0124 §8/B9), which is never defaulted
   or failed when absent: it simply **stays absent** from the reconciled
   map, on both a `typed` and an untyped class (a `late` field can declare
   no default in the first place — `docs/beamtalk-language-features.md`'s
   `late` Slots section — so this check runs before the typed-no-default
   failure above, not as a fallback from it). An undeclared key is dropped
   with a `?LOG_WARNING`.
""".
-spec migrate(Class :: atom(), FromVersion :: pos_integer(), Fields :: map()) ->
    {ok, NewFields :: map(), ToVersion :: pos_integer()} | {error, #beamtalk_error{}}.
migrate(Class, FromVersion, Fields) ->
    migrate(Class, FromVersion, Fields, #{}).

-doc """
Same as `migrate/3`, with `Opts`. `skip_stray_warning => true` (default
`false`) skips this call's own `warn_migrations_outside_table/2` check —
for a caller that already ran it once at a class-wide granularity (BT-3543:
`beamtalk_hot_reload`'s per-instance `code_change/3` path, via
`check_stray_migrations/1` hoisted to `beamtalk_repl_loader:hot_reload_class/2`)
and would otherwise repeat it, and its `?LOG_WARNING`, once per live instance.
""".
-spec migrate(Class :: atom(), FromVersion :: pos_integer(), Fields :: map(), Opts :: map()) ->
    {ok, NewFields :: map(), ToVersion :: pos_integer()} | {error, #beamtalk_error{}}.
migrate(Class, FromVersion, Fields, Opts) when
    is_atom(Class), is_integer(FromVersion), FromVersion > 0, is_map(Fields), is_map(Opts)
->
    case resolve_migrations(Class) of
        {ok, Module, Meta, Migrations} ->
            ToVersion = maps:get(shape_version, Meta, 1),
            maybe_log_downgrade(Class, FromVersion, ToVersion),
            case maps:get(skip_stray_warning, Opts, false) of
                true -> ok;
                false -> warn_migrations_outside_table(Class, Migrations)
            end,
            Invoke = fun(Selector, Dict) -> invoke_hook(Class, Selector, Dict) end,
            case
                beamtalk_shape_chain:migrate(Migrations, {FromVersion, ToVersion}, Fields, Invoke)
            of
                {ok, ChainedFields} ->
                    reconcile(Class, Module, Meta, ChainedFields, ToVersion);
                {error, {Step, Selector, Reason}} ->
                    {error, step_error(Class, Step, Selector, Reason)}
            end;
        not_found ->
            {error, class_not_found_error(Class)}
    end.

-doc """
Resolve `Class`'s compiled `Module`, its full `__beamtalk_meta/0` map, and
its `'shape_migrations'` table (default `#{}`) in one lookup — the
resolution sequence `migrate/4` and `check_stray_migrations/1` both need,
extracted so a future change to it (an inherited-chain lookup, a different
fallback) only needs updating here. Also the resolution `beamtalk_release_shapes`
(ADR 0125 §2.2/§3.4, BT-3571) uses for a build-time-loaded class, and the
per-level reader its ancestor-flatten walk calls — exported (not
`-ifdef(TEST)`-gated) for exactly that cross-module reuse.
""".
-spec resolve_migrations(atom()) -> {ok, module(), map(), map()} | not_found.
resolve_migrations(Class) ->
    case beamtalk_class_metadata:lookup_module(Class) of
        {ok, Module} ->
            Meta = read_meta(Module),
            Migrations = maps:get(shape_migrations, Meta, #{}),
            {ok, Module, Meta, Migrations};
        not_found ->
            not_found
    end.

-doc """
Log a downgrade (`ToVersion < FromVersion` — OTP's `{down, Vsn}` shape,
ADR 0123 § Runtime contract). The chain itself already runs no steps in this
case (`beamtalk_shape_chain:migrate/4`'s `From >= To` guard); this only
makes the otherwise-silent "state is newer than the code" case visible.
""".
-spec maybe_log_downgrade(atom(), integer(), integer()) -> ok.
maybe_log_downgrade(Class, FromVersion, ToVersion) when ToVersion < FromVersion ->
    ?LOG_WARNING(
        "Shape migration downgrade",
        #{
            class => Class,
            from_version => FromVersion,
            to_version => ToVersion,
            domain => [beamtalk, runtime]
        }
    );
maybe_log_downgrade(_Class, _FromVersion, _ToVersion) ->
    ok.

-doc """
Run `warn_migrations_outside_table/2` once for `Class` — the class-level
call site BT-3543 hoists this check to (`beamtalk_repl_loader:hot_reload_class/2`,
once per reload) instead of the `migrate/4` call inside it repeating the same
two class-process round-trips, and any resulting `?LOG_WARNING`, once per live
instance. Degrades to `ok` when `Class` is not a registered/compiled class —
the same tolerant-degrade convention `read_meta/1` uses — since a class this
finds no module for has no `migrateFromV*` methods to warn about either.
""".
-spec check_stray_migrations(atom()) -> ok.
check_stray_migrations(Class) ->
    case resolve_migrations(Class) of
        {ok, _Module, _Meta, Migrations} ->
            warn_migrations_outside_table(Class, Migrations);
        not_found ->
            ok
    end.

-doc """
Warn when a class-method fun whose selector matches `migrateFromV*` is
installed on `Class` but absent from `Migrations` (the compiler-emitted
`'shape_migrations'` table) — the **one place** this module inspects a
selector's *name* (ADR 0123 §2). Every other decision here reads the table,
never a selector spelling: a migration installed by a path that recompiles
the class (`Cart class >> migrateFromV1: …`, `compile:source:`) regenerates
`__beamtalk_meta` and lands in the table; one installed as a bare fun with
no recompile (`ClassBuilder addClassMethod:body:` after `register`, or any
future fun-only patch path) is invisible to the table *and* to
`local_call/3`, and does not run — this only makes that silent gap visible.
Never fails the migration itself; a lookup failure degrades to no warning,
the same tolerant-degrade convention `read_meta/1` uses.
""".
-spec warn_migrations_outside_table(atom(), map()) -> ok.
warn_migrations_outside_table(Class, Migrations) ->
    TableSelectors = sets:from_list(maps:values(Migrations), [{version, 2}]),
    try
        case beamtalk_class_registry:whereis_class(Class) of
            undefined ->
                ok;
            ClassPid ->
                Stray = [
                    S
                 || S <- beamtalk_object_class:local_class_methods(ClassPid),
                    is_migrate_from_v_selector(S),
                    not sets:is_element(S, TableSelectors)
                ],
                log_stray_migrations(Class, Stray)
        end
    catch
        _:_ -> ok
    end.

-spec log_stray_migrations(atom(), [atom()]) -> ok.
log_stray_migrations(_Class, []) ->
    ok;
log_stray_migrations(Class, Stray) ->
    ?LOG_WARNING(
        "migrateFromV* class method installed outside the shape_migrations "
        "table — it will not run in the migration chain until the class is "
        "recompiled",
        #{class => Class, selectors => Stray, domain => [beamtalk, runtime]}
    ).

-doc """
`true` for an atom spelled `migrateFromV<N>:` where `N` is one or more
digits — the same selector shape
`beamtalk_core::ast::migrate_from_v_version` (Rust) recognizes for meta
emission, re-derived here in Erlang rather than shared because this is
advisory only (see moduledoc): a mismatch between the two costs a missed or
spurious warning, never a wrong migration result, so it does not need the
shared-leaf-module machinery a correctness-critical rule would.
""".
-spec is_migrate_from_v_selector(atom()) -> boolean().
is_migrate_from_v_selector(Selector) ->
    Str = atom_to_list(Selector),
    Prefix = "migrateFromV",
    case lists:prefix(Prefix, Str) of
        true ->
            Rest = lists:nthtail(length(Prefix), Str),
            case lists:reverse(Rest) of
                [$: | RevDigits] when RevDigits =/= [] ->
                    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, RevDigits);
                _ ->
                    false
            end;
        false ->
            false
    end.

-doc """
Invoke a chain step's hook (`migrateFromVN:`, per the migrations table) via
`local_call/3`, in the calling process — this is `Invoke` (ADR 0123 §3's
`local_call/3` contract, proven mid-reload by BT-3535).

`beamtalk_shape_chain:migrate/4` catches any exception `local_call/3` raises
(a `does_not_understand` for a stale table entry, or the hook's own raise)
and turns it into a step failure — this function does not catch.
""".
-spec invoke_hook(atom(), atom(), map()) -> map() | {error, term()}.
invoke_hook(Class, Selector, Fields) ->
    case beamtalk_class_registry:whereis_class(Class) of
        undefined ->
            {error, {class_not_registered, Class}};
        ClassPid ->
            ClassObj = beamtalk_class_registry:class_object_from_pid(ClassPid),
            beamtalk_object_class:local_call(ClassObj, Selector, [Fields])
    end.

%%====================================================================
%% Reconcile (chain step 3 — ADR 0123 § Runtime contract)
%%====================================================================

-spec reconcile(atom(), atom(), map(), map(), pos_integer()) ->
    {ok, map(), pos_integer()} | {error, #beamtalk_error{}}.
reconcile(Class, Module, Meta, ChainedFields, ToVersion) ->
    IsTyped = maps:get(is_typed, Meta, false),
    DeclaredFields = beamtalk_behaviour_intrinsics:classAllFieldNamesByName(Class),
    HasDefaultMap = beamtalk_behaviour_intrinsics:classAllFieldHasDefaultByName(Class),
    KindsMap = beamtalk_behaviour_intrinsics:classAllFieldKindsByName(Class),
    Defaults = safe_init_defaults(Class, Module),
    case
        reconcile_declared(
            DeclaredFields, #{}, ChainedFields, Defaults, HasDefaultMap, KindsMap, IsTyped
        )
    of
        {ok, Kept} ->
            log_dropped_fields(Class, DeclaredFields, ChainedFields),
            {ok, Kept, ToVersion};
        {error, Reason} ->
            {error, reconcile_error(Class, Reason)}
    end.

-doc """
Walk `DeclaredFields`, keeping/defaulting/failing each one against
`ChainedFields` (ADR 0123 § Runtime contract, extended by ADR 0124 §8/B9 for
`late` — see `migrate/3`'s moduledoc step 4).

A field present in `ChainedFields` is always kept, regardless of `KindsMap`
— a `late` slot that *was* assigned before this migration ran stays
assigned. A field absent from `ChainedFields` whose `KindsMap` entry is
`late` (default `eager` when absent from the map — a class predating B5a's
`field_kinds` meta, or a field `classAllFieldKindsByName/1`'s dynamic-class
fallback reports) is left absent from `Acc` entirely: no `nil`, no
`typed_field_unset` error, checked **before** the has-default/`IsTyped`
branches below — a `late` field cannot declare a default in the first
place (parse-time rejected, `docs/beamtalk-language-features.md`'s `late`
Slots section), so `HasDefaultMap`'s entry for it is always `false` and
would otherwise fall straight into the `typed_field_unset` branch on a
`typed` class.
""".
-spec reconcile_declared([atom()], map(), map(), map(), map(), map(), boolean()) ->
    {ok, map()} | {error, {typed_field_unset, atom()}}.
reconcile_declared([], Acc, _ChainedFields, _Defaults, _HasDefaultMap, _KindsMap, _IsTyped) ->
    {ok, Acc};
reconcile_declared(
    [Field | Rest], Acc, ChainedFields, Defaults, HasDefaultMap, KindsMap, IsTyped
) ->
    case maps:find(Field, ChainedFields) of
        {ok, Value} ->
            reconcile_declared(
                Rest,
                Acc#{Field => Value},
                ChainedFields,
                Defaults,
                HasDefaultMap,
                KindsMap,
                IsTyped
            );
        error ->
            case maps:get(Field, KindsMap, eager) of
                late ->
                    %% ADR 0124 §8/B9: absent + late stays absent, whether
                    %% or not the class is typed — never nil, never a
                    %% typed_field_unset failure.
                    reconcile_declared(
                        Rest, Acc, ChainedFields, Defaults, HasDefaultMap, KindsMap, IsTyped
                    );
                eager ->
                    reconcile_declared_eager_absent(
                        Field, Rest, Acc, ChainedFields, Defaults, HasDefaultMap, KindsMap, IsTyped
                    )
            end
    end.

-spec reconcile_declared_eager_absent(
    atom(), [atom()], map(), map(), map(), map(), map(), boolean()
) ->
    {ok, map()} | {error, {typed_field_unset, atom()}}.
reconcile_declared_eager_absent(
    Field, Rest, Acc, ChainedFields, Defaults, HasDefaultMap, KindsMap, IsTyped
) ->
    case maps:get(Field, HasDefaultMap, false) of
        true ->
            case maps:find(Field, Defaults) of
                {ok, Default} ->
                    reconcile_declared(
                        Rest,
                        Acc#{Field => Default},
                        ChainedFields,
                        Defaults,
                        HasDefaultMap,
                        KindsMap,
                        IsTyped
                    );
                error when IsTyped ->
                    %% Declares a default, but safe_init_defaults/2
                    %% couldn't compute it (init/1 degraded to #{})
                    %% — on a typed class this may not silently
                    %% fall back to nil, same as the no-default
                    %% case below (ADR 0123 § Runtime contract: a
                    %% migration may not leave a typed slot unset).
                    {error, {typed_field_unset, Field}};
                error ->
                    reconcile_declared(
                        Rest,
                        Acc#{Field => nil},
                        ChainedFields,
                        Defaults,
                        HasDefaultMap,
                        KindsMap,
                        IsTyped
                    )
            end;
        false when IsTyped ->
            {error, {typed_field_unset, Field}};
        false ->
            reconcile_declared(
                Rest, Acc#{Field => nil}, ChainedFields, Defaults, HasDefaultMap, KindsMap, IsTyped
            )
    end.

-doc """
Fetch declared-field defaults for reconcile's "declared field has a default"
case — `Module:init(#{'__skip_initialize__' => true})`'s defaults (the
2-tuple, no-telemetry branch, ADR 0123 Current state ¶5) for an Actor/Object
class, or `Module:new/0`'s fields (stripped of internal keys) for a `Value`
class, which has no `init/1` at all (ADR 0123 §3: Values have no live
process and are never versioned in-memory, but reconcile still needs their
declared defaults — `beamtalk_class_instantiation:ancestor_compiled_defaults/1`
is the same "call the compiled constructor, strip internal fields" derivation
the class-instantiation default-collection walk already uses for a compiled
ancestor).

Degrades to `#{}` (every has-default field then falls back to `nil`, and a
warning is logged) rather than failing the whole migration: a constructor
that cannot run is the same "not usable right now" condition
`beamtalk_hot_reload`'s pre-Phase-2 `migrate_fields/3` already tolerated —
losing the actual default values is a lesser harm than aborting or
suspending a migration whose chain steps already ran successfully.
""".
-spec safe_init_defaults(atom(), atom()) -> map().
safe_init_defaults(Class, Module) ->
    case erlang:function_exported(Module, init, 1) of
        true -> safe_actor_init_defaults(Class, Module);
        false -> safe_value_new_defaults(Class, Module)
    end.

-spec safe_value_new_defaults(atom(), atom()) -> map().
safe_value_new_defaults(_Class, Module) ->
    %% ancestor_compiled_defaults/1 already degrades to #{} internally
    %% (its own try/catch, logged at ?LOG_DEBUG) — no second layer needed.
    beamtalk_class_instantiation:ancestor_compiled_defaults(Module).

-spec safe_actor_init_defaults(atom(), atom()) -> map().
safe_actor_init_defaults(Class, Module) ->
    try Module:init(#{'__skip_initialize__' => true}) of
        {ok, Map} when is_map(Map) ->
            Map;
        Other ->
            ?LOG_WARNING(
                "Shape migration reconcile: unexpected init/1 return",
                #{
                    class => Class,
                    module => Module,
                    returned => Other,
                    domain => [beamtalk, runtime]
                }
            ),
            #{}
    catch
        Kind:Reason ->
            ?LOG_WARNING(
                "Shape migration reconcile: init/1 failed",
                #{
                    class => Class,
                    module => Module,
                    kind => Kind,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }
            ),
            #{}
    end.

-spec log_dropped_fields(atom(), [atom()], map()) -> ok.
log_dropped_fields(Class, DeclaredFields, ChainedFields) ->
    DeclaredSet = sets:from_list(DeclaredFields, [{version, 2}]),
    case [K || K <- maps:keys(ChainedFields), not sets:is_element(K, DeclaredSet)] of
        [] ->
            ok;
        Dropped ->
            ?LOG_WARNING(
                "Shape migration dropped undeclared fields",
                #{class => Class, fields => Dropped, domain => [beamtalk, runtime]}
            )
    end.

%%====================================================================
%% pack/1, unpack/1 — the envelope (ADR 0123 § Runtime contract › Envelope)
%%====================================================================

-doc """
Pack a live instance into a versioned envelope, for persistence
(BT-3527, not yet a real caller) to consume — the `persist` policy (see
`pack_policy()`).

Defined only for `Sendable`-tier classes (ADR 0103): walks the flattened
declared field types (`classAllFieldTypesByName/1`) and the referenced
classes' kinds, and a field whose declared type resolves to an `Actor`
(`SendableRef`) or an `Object` declaring `handleScope:` (`HandleScoped`)
fails with `{error, #beamtalk_error{kind = not_serialisable}}` naming the
field. Nested `Value`-kind field values are packed recursively, so each
carries its own version. `Array`'s `'data'` map, `Dictionary`, and `String`
are builtin tagged maps, not user classes — they pass through as terms,
unversioned (their shape is the runtime's own, covered by BT-3528).
""".
-spec pack(Instance :: map()) -> {ok, envelope()} | {error, #beamtalk_error{}}.
pack(Instance) when is_map(Instance) ->
    pack(Instance, persist, 0).

-doc """
Pack a live instance for the wire (ADR 0126 §5.4) — the `wire` policy (see
`pack_policy()`).

Shares `pack/1`'s internal walk (`pack/3`, `pack_fields/7`,
`pack_field_value/4`), threaded with the `wire` policy: a `SendableRef`
field (an `Actor`-typed field, or — via `beamtalk_wire`'s runtime-class
dispatch, which calls this on a `Value`-kind instance it meets during its
term walk — a `Value` class with an `Actor`/`Pid`-typed field) **passes**
rather than being rejected, since pids are node-qualified natively and fine
to ship (§5.4 item 1). `HandleScoped` still rejects under `wire`, same as
`persist` (§5.4: an `Ets`/`Port` handle is meaningless on another node too).

Called both directly (a `Value` argument to a remote send) and recursively,
by `beamtalk_wire:encode/1`'s term walk whenever it meets a `Value`-kind
instance (ADR 0126 §5.1).
""".
-spec pack_wire(Instance :: map()) -> {ok, envelope()} | {error, #beamtalk_error{}}.
pack_wire(Instance) when is_map(Instance) ->
    pack(Instance, wire, 0).

%% Recursion cap for nested Value packing (mirrors the compile-time
%% checker's MAX_COMPOSE_DEPTH,
%% `beamtalk-core/src/semantic_analysis/type_checker/sendability.rs`) — a
%% guard against a pathologically deep or (were one ever constructible) a
%% cyclic chain of nested Value instances turning a single pack/1 call into
%% unbounded recursion. Values are immutable and built bottom-up, so a
%% genuine reference cycle should not arise through ordinary Beamtalk code,
%% but pack/1 accepts any hand-built tagged map (persistence/distribution
%% callers), so the guard costs little and cannot be skipped by construction.
%%
%% `beamtalk_wire:encode/1`'s own (separate) term walk reuses this exact cap
%% via `max_pack_depth/0` (ADR 0126 §5.1: "bounded by the same depth cap as
%% pack/1") rather than defining its own copy of the number.
-define(MAX_PACK_DEPTH, 32).

-doc "The recursion depth cap shared by `pack/3`'s nested-Value walk and `beamtalk_wire`'s term walk (see the `?MAX_PACK_DEPTH` comment above).".
-spec max_pack_depth() -> pos_integer().
max_pack_depth() -> ?MAX_PACK_DEPTH.

-spec pack(map(), pack_policy(), non_neg_integer()) ->
    {ok, envelope()} | {error, #beamtalk_error{}}.
pack(Instance, _Policy, Depth) when Depth >= ?MAX_PACK_DEPTH ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(not_serialisable, beamtalk_tagged_map:class_of(Instance, 'Object')),
            <<"nested Value packing exceeded the recursion depth limit">>
        )};
pack(Instance, Policy, Depth) ->
    case beamtalk_tagged_map:class_of(Instance) of
        undefined ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(not_serialisable, 'UndefinedObject'),
                    <<"value is not a tagged Beamtalk instance">>
                )};
        Class ->
            pack_instance(Class, Instance, Policy, Depth)
    end.

-spec pack_instance(atom(), map(), pack_policy(), non_neg_integer()) ->
    {ok, envelope()} | {error, #beamtalk_error{}}.
pack_instance(Class, Instance, Policy, Depth) ->
    case beamtalk_class_metadata:lookup_module(Class) of
        {ok, Module} ->
            Meta = read_meta(Module),
            ShapeVersion = maps:get(
                '__shape_version__', Instance, maps:get(shape_version, Meta, 1)
            ),
            FieldTypes = beamtalk_behaviour_intrinsics:classAllFieldTypesByName(Class),
            UserFields = beamtalk_tagged_map:user_field_keys(Instance),
            case pack_fields(Class, UserFields, Instance, FieldTypes, Policy, Depth, #{}) of
                {ok, PackedFields} ->
                    {ok, {beamtalk_shape, Class, ShapeVersion, PackedFields}};
                {error, _} = Err ->
                    Err
            end;
        not_found ->
            {error, class_not_found_error(Class)}
    end.

-spec pack_fields(atom(), [atom()], map(), map(), pack_policy(), non_neg_integer(), map()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
pack_fields(_Class, [], _Instance, _FieldTypes, _Policy, _Depth, Acc) ->
    {ok, Acc};
pack_fields(Class, [Field | Rest], Instance, FieldTypes, Policy, Depth, Acc) ->
    Value = maps:get(Field, Instance),
    Tier = field_tier(maps:get(Field, FieldTypes, none)),
    case reject_tier(Tier, Policy) of
        {true, TierLabel} ->
            {error, not_serialisable_error(Class, Field, TierLabel)};
        false ->
            case pack_field_value(Tier, Value, Policy, Depth) of
                {ok, PackedValue} ->
                    pack_fields(Class, Rest, Instance, FieldTypes, Policy, Depth, Acc#{
                        Field => PackedValue
                    });
                {error, _} = Err ->
                    Err
            end
    end.

%% ADR 0126 §5.4 item 1: `SendableRef` is rejected under `persist` (a pid has
%% no meaningful on-disk shape) but allowed under `wire` (pids are
%% node-qualified natively). `HandleScoped` is rejected under both.
-spec reject_tier(sendable_ref | handle_scoped | value_nested | passthrough, pack_policy()) ->
    {true, binary()} | false.
reject_tier(sendable_ref, persist) -> {true, <<"SendableRef">>};
reject_tier(sendable_ref, wire) -> false;
reject_tier(handle_scoped, _Policy) -> {true, <<"HandleScoped">>};
reject_tier(_Tier, _Policy) -> false.

%% Recurse into a Value-kind field's actual value when it is itself a tagged
%% instance. A `sendable_ref`-tier field (only reachable under `wire` —
%% `reject_tier/2` rejects it under `persist`) holds an actor reference,
%% which needs the same registered-ref node-qualification the generic wire
%% walk applies when it meets an actor ref directly (ADR 0126 §3/§5.1);
%% `beamtalk_pid:qualify_registered_ref/1` is the shared leaf both paths
%% call, so this can't drift from `beamtalk_wire`'s own rewrite. Every other
%% tier (passthrough) carries the raw value through unchanged, including
%% builtin Array/Dictionary/String maps (ADR 0090).
-spec pack_field_value(
    sendable_ref | value_nested | passthrough, term(), pack_policy(), non_neg_integer()
) ->
    {ok, term()} | {error, #beamtalk_error{}}.
pack_field_value(value_nested, Value, Policy, Depth) when is_map(Value) ->
    case beamtalk_tagged_map:is_tagged(Value) of
        true -> pack(Value, Policy, Depth + 1);
        false -> {ok, Value}
    end;
pack_field_value(sendable_ref, #beamtalk_object{pid = Pid} = Value, _Policy, _Depth) ->
    {ok, Value#beamtalk_object{pid = beamtalk_pid:qualify_registered_ref(Pid)}};
pack_field_value(_Tier, Value, _Policy, _Depth) ->
    {ok, Value}.

-doc """
Grade a declared field type's sendability tier — a small runtime
counterpart to the compile-time lattice
(`beamtalk-core/src/semantic_analysis/type_checker/sendability.rs`,
ADR 0103), scoped to what `pack/1` needs: whether the class kind a field's
*declared* type resolves to blocks packing (`Actor` / a `handleScope:`
`Object`) or asks for recursive versioning (`Value`, excluding the builtin
passthrough set).

**BT-3542 (cross-boundary conformance):** the two implementations cannot
literally share code across the Rust/Erlang boundary (ADR 0123 commissions
this walk as new work, not a port of the Rust lattice), so this module's
class-kind-based branches below are pinned against
`runtime/apps/beamtalk_runtime/test/fixtures/sendability_tier_conformance.json`
— a corpus shared with `sendability.rs`'s own
`runtime_field_tier_kind_mapping_matches_compile_time_base_tier` test — by
`beamtalk_shape_migration_tests`'s
`test_sendability_tier_conformance_matches_shared_corpus/0`. A future kind
added to either side's `case` (or a change to the `Object`/`handleScope:`
precedence) that isn't reflected in the corpus fails one or both of those
tests, catching the drift this function alone cannot prevent. Exported
under `-ifdef(TEST)` for that test's direct use (see `beamtalk_stdlib.erl`
for the same test-only export-gating convention).

**Known, deliberate scope gaps versus the compile-time lattice** (both
recorded as corpus entries or their own regression coverage, not just this
comment, per BT-3542):
1. A generic annotation's type arguments (`List(Port)`) are not composed
   here — only the head type name is graded. Deferred alongside this
   module's other Phase 2 scoping choices (ADR 0123 § Consequences); see
   `test_field_tier_does_not_compose_generic_type_args/0`.
2. An `Object` with no `handleScope:` declaration grades `Unknown` at
   compile time (silent, ADR 0100 advisory-only) but `passthrough` here
   (allowed through `pack/1` as an opaque unversioned term) — the
   corpus's `ShapePlainObject` entry.
""".
-spec field_tier(atom()) -> sendable_ref | handle_scoped | value_nested | passthrough.
field_tier(none) ->
    passthrough;
field_tier(TypeAtom) when is_atom(TypeAtom) ->
    BaseName = base_type_name(TypeAtom),
    case is_builtin_passthrough(BaseName) of
        true ->
            passthrough;
        false ->
            case beamtalk_class_metadata:lookup_module(BaseName) of
                {ok, Module} ->
                    Meta = read_meta(Module),
                    case maps:get(kind, Meta, object) of
                        actor ->
                            sendable_ref;
                        value ->
                            value_nested;
                        object ->
                            case maps:is_key(handle_scope, Meta) of
                                true -> handle_scoped;
                                false -> passthrough
                            end;
                        _ ->
                            passthrough
                    end;
                not_found ->
                    passthrough
            end
    end.

%% A generic annotation ("List(Port)") is stored as one atom carrying the
%% whole rendered type string (class_meta.rs's `meta_field_types_map`); the
%% base class name is everything before the first `(`.
-spec base_type_name(atom()) -> atom().
base_type_name(TypeAtom) ->
    case string:split(atom_to_list(TypeAtom), "(") of
        [Base, _Rest] -> list_to_atom(Base);
        [Base] -> list_to_atom(Base)
    end.

%% ADR 0123 § Envelope: builtin tagged maps that are not user classes pass
%% through pack/1 as terms, unversioned — their shape is the runtime's own.
-spec is_builtin_passthrough(atom()) -> boolean().
is_builtin_passthrough('Array') -> true;
is_builtin_passthrough('Dictionary') -> true;
is_builtin_passthrough('String') -> true;
is_builtin_passthrough(_) -> false.

-doc """
Unpack a versioned envelope back into a live instance map, running the
shape migration chain from the envelope's version — so a value packed under
an old version reads back current without the caller knowing. The `lenient`
policy (see `unpack_policy()`): an envelope newer than this node's own
declared `shapeVersion` is migrated anyway, via `migrate/3`'s existing
`ToVersion < FromVersion` downgrade branch (a logged warning, reconciled
against this node's — older — declared field list, silently dropping any
field that list does not know). The right policy for persistence, where an
operator-initiated rollback genuinely cannot use the new fields.

Nested envelopes (from `pack/1`'s recursive `Value` packing) are unpacked
first, depth-first, so migration hooks see ordinary instance maps, never raw
envelope tuples. The result is tagged with `'$beamtalk_class'` and
`'__shape_version__'` — a usable instance map, not a bare field dictionary.
""".
-spec unpack(envelope()) -> {ok, Instance :: map()} | {error, #beamtalk_error{}}.
unpack(Envelope) ->
    unpack(Envelope, lenient, 0).

-doc """
Unpack a versioned envelope for the wire (ADR 0125 §3.4, ADR 0126 §5.2) —
the `strict` policy (see `unpack_policy()`).

Shares `unpack/1`'s internal walk (`unpack/3`, `unpack_nested_fields/3`,
`unpack_nested_value/3`), threaded with the `strict` policy: an envelope
whose `ShapeVersion` **exceeds** this node's own declared `shapeVersion` for
that class is refused with `#beamtalk_error{kind = shape_version_ahead}`
**before** the migration chain runs, rather than silently truncated —
"receivers migrate forward, never backward" for a wire, where a rolling
deploy makes every message from an already-upgraded peer a legitimate
newer-version envelope, not a malformed one. The check runs per envelope
(threaded down the same nested-`Value` recursion `unpack/1` uses), per ADR
0125 §3.4: a `Cart` at the receiver's own version carrying a `Money` field
one version ahead is still skew, caught at the `Money` envelope, not missed
by only checking the top level.

Called by `beamtalk_wire:decode/1` at each envelope its term walk meets.
""".
-spec unpack_strict(envelope()) -> {ok, Instance :: map()} | {error, #beamtalk_error{}}.
unpack_strict(Envelope) ->
    unpack(Envelope, strict, 0).

-spec unpack(envelope(), unpack_policy(), non_neg_integer()) ->
    {ok, Instance :: map()} | {error, #beamtalk_error{}}.
unpack(_Envelope, _Policy, Depth) when Depth >= ?MAX_PACK_DEPTH ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(not_serialisable, 'Object'),
            <<"nested Value unpacking exceeded the recursion depth limit">>
        )};
unpack({beamtalk_shape, Class, ShapeVersion, Fields}, Policy, Depth) when
    is_atom(Class), is_integer(ShapeVersion), ShapeVersion > 0, is_map(Fields)
->
    case check_unpack_policy(Policy, Class, ShapeVersion) of
        ok ->
            case unpack_nested_fields(Fields, Policy, Depth) of
                {ok, UnpackedFields} ->
                    case migrate(Class, ShapeVersion, UnpackedFields) of
                        {ok, NewFields, ToVersion} ->
                            {ok, NewFields#{
                                '$beamtalk_class' => Class, '__shape_version__' => ToVersion
                            }};
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% `lenient` (unpack/1) never refuses on version alone — migrate/3's own
%% downgrade branch already handles a ShapeVersion the receiver is behind on.
%% `strict` (unpack_strict/1) refuses upfront when ShapeVersion is AHEAD of
%% this node's own declared shapeVersion, before the chain runs (ADR 0125
%% §3.4 item 3).
-spec check_unpack_policy(unpack_policy(), atom(), pos_integer()) ->
    ok | {error, #beamtalk_error{}}.
check_unpack_policy(lenient, _Class, _ShapeVersion) ->
    ok;
check_unpack_policy(strict, Class, ShapeVersion) ->
    case resolve_migrations(Class) of
        {ok, _Module, Meta, _Migrations} ->
            KnownVersion = maps:get(shape_version, Meta, 1),
            case ShapeVersion > KnownVersion of
                true -> {error, shape_version_ahead_error(Class, ShapeVersion, KnownVersion)};
                false -> ok
            end;
        not_found ->
            {error, class_not_found_error(Class)}
    end.

-spec unpack_nested_fields(map(), unpack_policy(), non_neg_integer()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
unpack_nested_fields(Fields, Policy, Depth) ->
    maps:fold(
        fun
            (_K, _V, {error, _} = Err) ->
                Err;
            (K, V, {ok, Acc}) ->
                case unpack_nested_value(V, Policy, Depth) of
                    {ok, NV} -> {ok, Acc#{K => NV}};
                    {error, _} = Err -> Err
                end
        end,
        {ok, #{}},
        Fields
    ).

-spec unpack_nested_value(term(), unpack_policy(), non_neg_integer()) ->
    {ok, term()} | {error, #beamtalk_error{}}.
unpack_nested_value({beamtalk_shape, Class, ShapeVersion, NestedFields}, Policy, Depth) when
    is_atom(Class), is_integer(ShapeVersion), is_map(NestedFields)
->
    unpack({beamtalk_shape, Class, ShapeVersion, NestedFields}, Policy, Depth + 1);
unpack_nested_value(V, _Policy, _Depth) ->
    {ok, V}.

%%====================================================================
%% Errors
%%====================================================================

-spec class_not_found_error(atom()) -> #beamtalk_error{}.
class_not_found_error(Class) ->
    beamtalk_error:new(class_not_found, Class, migrate).

%% ADR 0125 §3.4 item 3 / ADR 0126 §5.2: an envelope ahead of this node's own
%% declared shapeVersion for Class, refused by unpack_strict/1 before the
%% migration chain runs.
-spec shape_version_ahead_error(atom(), pos_integer(), pos_integer()) -> #beamtalk_error{}.
shape_version_ahead_error(Class, SentVersion, KnownVersion) ->
    %% unicode:characters_to_binary/1, not iolist_to_binary/1 — see
    %% reconcile_error/2's comment (em dash outside iolist_to_binary's
    %% 0-255 byte range).
    Hint = unicode:characters_to_binary(
        io_lib:format(
            "~s envelope at shape version ~p is ahead of this node's known version ~p "
            "— upgrade this node before the sender",
            [Class, SentVersion, KnownVersion]
        )
    ),
    beamtalk_error:with_details(
        beamtalk_error:with_hint(
            beamtalk_error:new(shape_version_ahead, Class),
            Hint
        ),
        #{sent => SentVersion, known => KnownVersion, node => node()}
    ).

-spec step_error(atom(), pos_integer(), atom(), term()) -> #beamtalk_error{}.
step_error(Class, Step, Selector, Reason) ->
    Hint = iolist_to_binary(
        io_lib:format(
            "~s class >> ~s raised ~s",
            [Class, Selector, format_step_reason(Reason)]
        )
    ),
    beamtalk_error:with_details(
        beamtalk_error:with_hint(
            beamtalk_error:new(shape_migration_failed, Class, Selector),
            Hint
        ),
        #{from => Step, to => Step + 1, selector => Selector, reason => Reason}
    ).

%% beamtalk_shape_chain:apply_step/3 catches a raised exception as
%% `{Class, Reason}` (Erlang's `catch Class:Reason`) — for a Beamtalk-raised
%% exception (`beamtalk_error:raise/1`), `Class` is the atom `error` and
%% `Reason` is the wrapped `#{'$beamtalk_class' := _, error := …}` Exception
%% tagged map, so a hook's own raise (or a stale-table `does_not_understand`)
%% commonly reaches here as `{error, WrappedException}`. Unwrap that shape
%% through `format_safe/2` (whose clause matches it directly, independent of
%% the stacktrace argument) for a readable hint instead of a raw term dump.
-spec format_step_reason(term()) -> binary().
format_step_reason({error, #{'$beamtalk_class' := _, error := #beamtalk_error{}} = Wrapped}) ->
    beamtalk_error:format_safe(Wrapped, []);
format_step_reason(Reason) ->
    beamtalk_error:format_safe(Reason).

-spec reconcile_error(atom(), {typed_field_unset, atom()}) -> #beamtalk_error{}.
reconcile_error(Class, {typed_field_unset, Field}) ->
    %% unicode:characters_to_binary/1, not iolist_to_binary/1: the message
    %% below contains an em dash, a code point outside iolist_to_binary's
    %% 0-255 byte range.
    Hint = unicode:characters_to_binary(
        io_lib:format(
            "migration left typed field '~s' unset — a migration may not leave a typed slot unset",
            [Field]
        )
    ),
    beamtalk_error:with_hint(
        beamtalk_error:new(shape_migration_failed, Class, Field),
        Hint
    ).

-spec not_serialisable_error(atom(), atom(), binary()) -> #beamtalk_error{}.
not_serialisable_error(Class, Field, TierLabel) ->
    %% unicode:characters_to_binary/1 — see reconcile_error/2's comment.
    Hint = unicode:characters_to_binary(
        io_lib:format("field '~s' is ~s — not serialisable", [Field, TierLabel])
    ),
    beamtalk_error:with_hint(
        beamtalk_error:new(not_serialisable, Class, Field),
        Hint
    ).

%%====================================================================
%% Meta reading
%%====================================================================

-doc """
Read `__beamtalk_meta/0` from a compiled module. Returns `#{}` for a
dynamic class (no such export) or a module whose meta call fails — the same
tolerant-degrade convention `beamtalk_object_class:read_meta/1` and
`beamtalk_behaviour_intrinsics:meta_for_module/1` use.
""".
-spec read_meta(atom()) -> map().
read_meta(Module) ->
    case erlang:function_exported(Module, '__beamtalk_meta', 0) of
        true ->
            try Module:'__beamtalk_meta'() of
                M when is_map(M) -> M;
                _ -> #{}
            catch
                _:_ -> #{}
            end;
        false ->
            #{}
    end.
