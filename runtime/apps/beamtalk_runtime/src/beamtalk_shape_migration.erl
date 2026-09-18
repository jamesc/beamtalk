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

-export([migrate/3, pack/1, unpack/1]).

-export_type([envelope/0]).

-type envelope() ::
    {beamtalk_shape, Class :: atom(), ShapeVersion :: pos_integer(), Fields :: map()}.

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
   a declared field present is kept; absent falls back to
   `Module:init(#{'__skip_initialize__' => true})`'s default when the field
   declares one; absent with no default is `nil` on an untyped class and a
   `shape_migration_failed` error on a `typed` one (a migration may not
   leave a typed slot unset — the same promise ADR 0078's post-`initialize`
   check makes at spawn time); an undeclared key is dropped with a
   `?LOG_WARNING`.
""".
-spec migrate(Class :: atom(), FromVersion :: pos_integer(), Fields :: map()) ->
    {ok, NewFields :: map(), ToVersion :: pos_integer()} | {error, #beamtalk_error{}}.
migrate(Class, FromVersion, Fields) when
    is_atom(Class), is_integer(FromVersion), FromVersion > 0, is_map(Fields)
->
    case beamtalk_class_metadata:lookup_module(Class) of
        {ok, Module} ->
            Meta = read_meta(Module),
            ToVersion = maps:get(shape_version, Meta, 1),
            Migrations = maps:get(shape_migrations, Meta, #{}),
            maybe_log_downgrade(Class, FromVersion, ToVersion),
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
    Defaults = safe_init_defaults(Class, Module),
    case reconcile_declared(DeclaredFields, #{}, ChainedFields, Defaults, HasDefaultMap, IsTyped) of
        {ok, Kept} ->
            log_dropped_fields(Class, DeclaredFields, ChainedFields),
            {ok, Kept, ToVersion};
        {error, Reason} ->
            {error, reconcile_error(Class, Reason)}
    end.

-spec reconcile_declared([atom()], map(), map(), map(), map(), boolean()) ->
    {ok, map()} | {error, {typed_field_unset, atom()}}.
reconcile_declared([], Acc, _ChainedFields, _Defaults, _HasDefaultMap, _IsTyped) ->
    {ok, Acc};
reconcile_declared([Field | Rest], Acc, ChainedFields, Defaults, HasDefaultMap, IsTyped) ->
    case maps:find(Field, ChainedFields) of
        {ok, Value} ->
            reconcile_declared(
                Rest, Acc#{Field => Value}, ChainedFields, Defaults, HasDefaultMap, IsTyped
            );
        error ->
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
                                IsTyped
                            )
                    end;
                false when IsTyped ->
                    {error, {typed_field_unset, Field}};
                false ->
                    reconcile_declared(
                        Rest, Acc#{Field => nil}, ChainedFields, Defaults, HasDefaultMap, IsTyped
                    )
            end
    end.

-doc """
Fetch `Module:init(#{'__skip_initialize__' => true})`'s defaults — the
2-tuple, no-telemetry branch (ADR 0123 Current state ¶5) — for reconcile's
"declared field has a default" case.

Degrades to `#{}` (every has-default field then falls back to `nil`, and a
warning is logged) rather than failing the whole migration: an `init/1` that
cannot run is the same "not usable right now" condition
`beamtalk_hot_reload`'s pre-Phase-2 `migrate_fields/3` already tolerated —
losing the actual default values is a lesser harm than aborting or
suspending a migration whose chain steps already ran successfully.
""".
-spec safe_init_defaults(atom(), atom()) -> map().
safe_init_defaults(Class, Module) ->
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
Pack a live instance into a versioned envelope, for persistence and
distribution (BT-3527) to consume.

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
%% Recursion cap for nested Value packing (mirrors the compile-time
%% checker's MAX_COMPOSE_DEPTH,
%% `beamtalk-core/src/semantic_analysis/type_checker/sendability.rs`) — a
%% guard against a pathologically deep or (were one ever constructible) a
%% cyclic chain of nested Value instances turning a single pack/1 call into
%% unbounded recursion. Values are immutable and built bottom-up, so a
%% genuine reference cycle should not arise through ordinary Beamtalk code,
%% but pack/1 accepts any hand-built tagged map (persistence/distribution
%% callers), so the guard costs little and cannot be skipped by construction.
-define(MAX_PACK_DEPTH, 32).

-spec pack(Instance :: map()) -> {ok, envelope()} | {error, #beamtalk_error{}}.
pack(Instance) when is_map(Instance) ->
    pack(Instance, 0).

-spec pack(map(), non_neg_integer()) -> {ok, envelope()} | {error, #beamtalk_error{}}.
pack(Instance, Depth) when Depth >= ?MAX_PACK_DEPTH ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(not_serialisable, beamtalk_tagged_map:class_of(Instance, 'Object')),
            <<"nested Value packing exceeded the recursion depth limit">>
        )};
pack(Instance, Depth) ->
    case beamtalk_tagged_map:class_of(Instance) of
        undefined ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(not_serialisable, 'UndefinedObject'),
                    <<"value is not a tagged Beamtalk instance">>
                )};
        Class ->
            pack_instance(Class, Instance, Depth)
    end.

-spec pack_instance(atom(), map(), non_neg_integer()) ->
    {ok, envelope()} | {error, #beamtalk_error{}}.
pack_instance(Class, Instance, Depth) ->
    case beamtalk_class_metadata:lookup_module(Class) of
        {ok, Module} ->
            Meta = read_meta(Module),
            ShapeVersion = maps:get(
                '__shape_version__', Instance, maps:get(shape_version, Meta, 1)
            ),
            FieldTypes = beamtalk_behaviour_intrinsics:classAllFieldTypesByName(Class),
            UserFields = beamtalk_tagged_map:user_field_keys(Instance),
            case pack_fields(Class, UserFields, Instance, FieldTypes, Depth, #{}) of
                {ok, PackedFields} ->
                    {ok, {beamtalk_shape, Class, ShapeVersion, PackedFields}};
                {error, _} = Err ->
                    Err
            end;
        not_found ->
            {error, class_not_found_error(Class)}
    end.

-spec pack_fields(atom(), [atom()], map(), map(), non_neg_integer(), map()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
pack_fields(_Class, [], _Instance, _FieldTypes, _Depth, Acc) ->
    {ok, Acc};
pack_fields(Class, [Field | Rest], Instance, FieldTypes, Depth, Acc) ->
    Value = maps:get(Field, Instance),
    case field_tier(maps:get(Field, FieldTypes, none)) of
        sendable_ref ->
            {error, not_serialisable_error(Class, Field, <<"SendableRef">>)};
        handle_scoped ->
            {error, not_serialisable_error(Class, Field, <<"HandleScoped">>)};
        Tier ->
            case pack_field_value(Tier, Value, Depth) of
                {ok, PackedValue} ->
                    pack_fields(Class, Rest, Instance, FieldTypes, Depth, Acc#{
                        Field => PackedValue
                    });
                {error, _} = Err ->
                    Err
            end
    end.

%% Recurse into a Value-kind field's actual value when it is itself a tagged
%% instance — every other tier (passthrough) carries the raw value through
%% unchanged, including builtin Array/Dictionary/String maps (ADR 0090).
-spec pack_field_value(value_nested | passthrough, term(), non_neg_integer()) ->
    {ok, term()} | {error, #beamtalk_error{}}.
pack_field_value(value_nested, Value, Depth) when is_map(Value) ->
    case beamtalk_tagged_map:is_tagged(Value) of
        true -> pack(Value, Depth + 1);
        false -> {ok, Value}
    end;
pack_field_value(_Tier, Value, _Depth) ->
    {ok, Value}.

%% Grade a declared field type's sendability tier — a small runtime
%% counterpart to the compile-time lattice
%% (`beamtalk-core/src/semantic_analysis/type_checker/sendability.rs`,
%% ADR 0103), scoped to what pack/1 needs: whether the class kind a field's
%% *declared* type resolves to blocks packing (`Actor` / a `handleScope:`
%% `Object`) or asks for recursive versioning (`Value`, excluding the
%% builtin passthrough set). A generic annotation's type arguments
%% (`List(Port)`) are not composed here — only the head type name is graded,
%% a known simplification versus the compile-time checker's full
%% composition; deferred alongside this module's other Phase 2 scoping
%% choices (ADR 0123 § Consequences).
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
an old version reads back current without the caller knowing.

Nested envelopes (from `pack/1`'s recursive `Value` packing) are unpacked
first, depth-first, so migration hooks see ordinary instance maps, never raw
envelope tuples. The result is tagged with `'$beamtalk_class'` and
`'__shape_version__'` — a usable instance map, not a bare field dictionary.
""".
-spec unpack(envelope()) -> {ok, Instance :: map()} | {error, #beamtalk_error{}}.
unpack(Envelope) ->
    unpack(Envelope, 0).

-spec unpack(envelope(), non_neg_integer()) -> {ok, Instance :: map()} | {error, #beamtalk_error{}}.
unpack(_Envelope, Depth) when Depth >= ?MAX_PACK_DEPTH ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(not_serialisable, 'Object'),
            <<"nested Value unpacking exceeded the recursion depth limit">>
        )};
unpack({beamtalk_shape, Class, ShapeVersion, Fields}, Depth) when
    is_atom(Class), is_integer(ShapeVersion), ShapeVersion > 0, is_map(Fields)
->
    case unpack_nested_fields(Fields, Depth) of
        {ok, UnpackedFields} ->
            case migrate(Class, ShapeVersion, UnpackedFields) of
                {ok, NewFields, ToVersion} ->
                    {ok, NewFields#{'$beamtalk_class' => Class, '__shape_version__' => ToVersion}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec unpack_nested_fields(map(), non_neg_integer()) -> {ok, map()} | {error, #beamtalk_error{}}.
unpack_nested_fields(Fields, Depth) ->
    maps:fold(
        fun
            (_K, _V, {error, _} = Err) ->
                Err;
            (K, V, {ok, Acc}) ->
                case unpack_nested_value(V, Depth) of
                    {ok, NV} -> {ok, Acc#{K => NV}};
                    {error, _} = Err -> Err
                end
        end,
        {ok, #{}},
        Fields
    ).

-spec unpack_nested_value(term(), non_neg_integer()) -> {ok, term()} | {error, #beamtalk_error{}}.
unpack_nested_value({beamtalk_shape, Class, ShapeVersion, NestedFields}, Depth) when
    is_atom(Class), is_integer(ShapeVersion), is_map(NestedFields)
->
    unpack({beamtalk_shape, Class, ShapeVersion, NestedFields}, Depth + 1);
unpack_nested_value(V, _Depth) ->
    {ok, V}.

%%====================================================================
%% Errors
%%====================================================================

-spec class_not_found_error(atom()) -> #beamtalk_error{}.
class_not_found_error(Class) ->
    beamtalk_error:new(class_not_found, Class, migrate).

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
