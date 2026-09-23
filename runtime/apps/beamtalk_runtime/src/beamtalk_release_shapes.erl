%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_shapes).

%%% **DDD Context:** Object System Context

-moduledoc """
Build-time shape extractor (ADR 0125 §2.2/§3.4, BT-3571).

`__beamtalk_meta/0` is a **compiled function**, not a BEAM chunk — its value
exists only when it is *executed* — so `beam_lib` cannot read a release's
class shapes off disk the way it reads `attributes`/`exports`. This module
is the small `erl -noshell` step that can: it loads a release's staged
`bt@*` beams into a scratch node (this process), lets their `-on_load`
hooks register them the normal way, and then evaluates `__beamtalk_meta/0`
on each — the same thing a real release boot does
(`beamtalk_module_activation:activate_modules/2`, which this module calls
directly rather than re-implementing module discovery/loading/registration).

`extract_shapes/2` is invoked by `beamtalk-cli`'s `beamtalk release`
(`crates/beamtalk-cli/src/commands/release/assembly.rs`) via a single
`erl -eval`, mirroring how that module already drives `systools:make_script/2`
— no second ad-hoc spawner (CLAUDE.md). Its two directory-list parameters
are staged `lib/<app>-<vsn>/ebin` paths (`AppClosure`, `closure.rs`):
`RuntimeLibDirs` (the runtime closure the extractor needs *loaded* so
ancestor classes like `Object`/`Actor` resolve, but never emits a shapes.json
entry for) and `EmitLibDirs` (the project's own app, its ADR 0070
dependencies, and any `[release] apps` extras — every class here *does* get
an entry). This mirrors `beamtalk_module_activation:find_bt_modules_in_dir/1`'s
own `bt@stdlib@*`-exclusion convention: stdlib classes are versioned with the
toolchain (ADR 0098), not a project's own `shapeVersion:` declarations, so
they have no place in a project's shapes.json.

## The shared projection (ADR 0125 §2.2)

`class_shape_entry/1` is the projection this module exists to expose
publicly: given a *registered, loaded* class, it returns the shapes.json
entry ADR 0125 §3.4 specifies — `shape_version` (default `1`), the
**flattened** field-type map (ancestors merged in, closer wins), and the
`shape_migrations` table. It is deliberately usable from either side of the
build/live divide:

- Here, against a scratch node that just loaded a release's beams.
- From a *live* running node, once `Beamtalk shapeManifest` (the BT-3575
  console issue) and the `--upgrade-from` preflight (BT-3574) exist — both
  call this exact function, so a build-time `shapes.json` and a live node's
  `shapeManifest` answer can never structurally disagree about what "the
  shape of a class" means.

The ancestor walk-and-merge itself is `beamtalk_class_metadata:
flatten_ancestor_map/2` — the one "merge ancestor field maps, closer wins"
primitive this module shares with `beamtalk_workspace_shape_store`'s live,
ETS-backed flattening (CLAUDE.md's no-duplicate-implementations rule): only
the per-level reader differs (a build-time `__beamtalk_meta/0` read here, a
live one there).

## Why `beamtalk_stdlib`, not just `beamtalk_runtime`

`RuntimeLibDirs`/`EmitLibDirs` are both added to the code path, but only
`beamtalk_stdlib` is *started* (`application:ensure_all_started/1`, which
transitively starts its own dependency `beamtalk_runtime` first). Starting
`beamtalk_stdlib` runs its own internal stdlib activation
(`beamtalk_stdlib:start/2`), registering `Object`, `Actor`, `Value` and
every other stdlib class in `beamtalk_class_metadata` — exactly the
ancestors a project's or dependency's classes chain up to. Starting only
`beamtalk_runtime` would leave those ancestors unregistered and truncate
every flattened field map at the first stdlib superclass.

## JSON encoding

`write_shapes_json/4` uses OTP's own `json` module (`json:encode/1`,
OTP 27+ — the same encoder `beamtalk_json_formatter` already uses for
structured log lines) — no new JSON-encoding dependency.
""".

-include_lib("kernel/include/logger.hrl").

-export([extract_shapes/2, write_shapes_json/4, class_shape_entry/1, class_field_shape/1]).

-type shape_entry() :: #{
    version := pos_integer(),
    fields := #{binary() => binary() | null},
    migrations := #{binary() => binary()}
}.
-export_type([shape_entry/0]).

%%====================================================================
%% Public API
%%====================================================================

-doc """
Load every `bt@*` module found under `RuntimeLibDirs` ++ `EmitLibDirs`
(`RuntimeLibDirs` for ancestor resolution only — see the moduledoc) and
return a shapes.json-ready map: `ClassNameBinary => shape_entry()`, one
entry per class found under `EmitLibDirs` only.

# Errors

Returns `{error, {stdlib_start_failed, Reason}}` if `beamtalk_stdlib` (and
so, transitively, `beamtalk_runtime`) cannot be started — nothing can be
extracted without the class-registration machinery it provides. A module
that fails to *load or register* (a partial/stale build, a genuinely broken
class) is not fatal to the whole extraction: it is logged and skipped,
mirroring `beamtalk_module_activation:activate_modules/2`'s own
never-abort-on-one-module-failure contract — the caller
(`assembly.rs`) has already verified every declared module has a matching
`.beam` file at staging time (`closure.rs`'s `read_staged_app`), so a load
failure here would indicate a different, load-time-only defect worth
surfacing without losing every *other* class's shape.
""".
-spec extract_shapes([file:filename_all()], [file:filename_all()]) ->
    {ok, #{binary() => shape_entry()}} | {error, term()}.
extract_shapes(RuntimeLibDirs, EmitLibDirs) ->
    lists:foreach(fun(Dir) -> code:add_pathz(Dir) end, RuntimeLibDirs ++ EmitLibDirs),
    case application:ensure_all_started(beamtalk_stdlib) of
        {ok, _Started} ->
            EmitModules = lists:usort(
                lists:flatmap(
                    fun beamtalk_module_activation:find_bt_modules_in_dir/1, EmitLibDirs
                )
            ),
            {ok, ActivationErrors} = beamtalk_module_activation:activate_modules(
                EmitModules, #{}
            ),
            lists:foreach(
                fun({Module, Reason}) ->
                    ?LOG_WARNING(
                        "beamtalk_release_shapes: module failed to activate, skipping its shape",
                        #{module => Module, reason => Reason, domain => [beamtalk, runtime]}
                    )
                end,
                ActivationErrors
            ),
            {ok, build_shapes_map(EmitModules)};
        {error, Reason} ->
            {error, {stdlib_start_failed, Reason}}
    end.

-doc """
The build/live-shared projection (ADR 0125 §2.2/§3.4) — see the moduledoc's
"The shared projection" section.

`undefined` when `ClassAtom` is not a registered, loaded class (never
raises) — the same tolerant-degrade convention
`beamtalk_shape_migration:resolve_migrations/1`, which this delegates to for
`Meta`/`Migrations`, already uses.
""".
-spec class_shape_entry(atom()) -> shape_entry() | undefined.
class_shape_entry(ClassAtom) ->
    case beamtalk_shape_migration:resolve_migrations(ClassAtom) of
        {ok, _Module, Meta, Migrations} ->
            OwnFieldTypes = maps:get(field_types, Meta, #{}),
            AncestorFieldTypes = beamtalk_class_metadata:flatten_ancestor_map(
                ClassAtom, fun ancestor_field_types/1
            ),
            FlattenedFieldTypes = maps:merge(AncestorFieldTypes, OwnFieldTypes),
            #{
                version => maps:get(shape_version, Meta, 1),
                fields => normalize_fields(FlattenedFieldTypes),
                migrations => normalize_migrations(Migrations)
            };
        not_found ->
            undefined
    end.

-doc """
Like `class_shape_entry/1`, but returns the flattened field map in
`beamtalk_class_metadata:field_shape()` form — `{DeclaredType, Kind}` per
field, the exact shape `beamtalk_shape_diff:shape()` (ADR 0105 Phase 2)
expects — rather than `shapes.json`'s own `<<"Dynamic">>`/`null`-free JSON
projection (`normalize_fields/1`, which drops `Kind` entirely; ADR 0125 §3.4
has no use for it).

This is the disk-side half of the ADR 0125 §2.3/BT-3574 preflight's
conformance test: for the same class, this function's result must equal
`beamtalk_workspace_shape_store:read_shape_from_meta/1`'s live result — both
flatten via `beamtalk_class_metadata:flatten_ancestor_map/2` and normalise
via `beamtalk_class_metadata:normalize_field_shape/2`, the two shared-leaf
primitives this function and that one both call, so the two flatteners
cannot structurally drift (CLAUDE.md's no-duplicate-implementations rule).

`undefined` under the same conditions `class_shape_entry/1` degrades under.
""".
-spec class_field_shape(atom()) -> beamtalk_class_metadata:field_shape() | undefined.
class_field_shape(ClassAtom) ->
    case beamtalk_shape_migration:resolve_migrations(ClassAtom) of
        {ok, _Module, Meta, _Migrations} ->
            OwnFieldTypes = maps:get(field_types, Meta, #{}),
            OwnFieldKinds = maps:get(field_kinds, Meta, #{}),
            AncestorFieldTypes = beamtalk_class_metadata:flatten_ancestor_map(
                ClassAtom, fun ancestor_field_types/1
            ),
            AncestorFieldKinds = beamtalk_class_metadata:flatten_ancestor_map(
                ClassAtom, fun ancestor_field_kinds/1
            ),
            FlattenedFieldTypes = maps:merge(AncestorFieldTypes, OwnFieldTypes),
            FlattenedFieldKinds = maps:merge(AncestorFieldKinds, OwnFieldKinds),
            beamtalk_class_metadata:normalize_field_shape(FlattenedFieldTypes, FlattenedFieldKinds);
        not_found ->
            undefined
    end.

-doc """
Extract `RuntimeLibDirs`/`EmitLibDirs` (`extract_shapes/2`) and write
`releases/<vsn>/shapes.json` (ADR 0125 §3.4) to `OutPath`.
""".
-spec write_shapes_json(
    [file:filename_all()], [file:filename_all()], file:filename_all(), binary()
) -> ok | {error, term()}.
write_shapes_json(RuntimeLibDirs, EmitLibDirs, OutPath, ReleaseVsnBin) when
    is_binary(ReleaseVsnBin)
->
    case extract_shapes(RuntimeLibDirs, EmitLibDirs) of
        {ok, Shapes} ->
            Doc = #{
                <<"schema">> => 1,
                <<"release_version">> => ReleaseVsnBin,
                <<"shapes">> => Shapes
            },
            file:write_file(OutPath, json:encode(Doc));
        {error, _} = Err ->
            Err
    end.

%%====================================================================
%% Internal
%%====================================================================

-spec build_shapes_map([module()]) -> #{binary() => shape_entry()}.
build_shapes_map(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            case class_name_for_module(Module) of
                {ok, ClassAtom} ->
                    case class_shape_entry(ClassAtom) of
                        undefined -> Acc;
                        Entry -> Acc#{atom_to_binary(ClassAtom, utf8) => Entry}
                    end;
                error ->
                    Acc
            end
        end,
        #{},
        Modules
    ).

-doc """
`Module`'s class name, via `beamtalk_module_activation:extract_class_names/1`
(the `beamtalk_class` module attribute the compiler embeds) — not a second,
independent way to derive a class name from a module. Empty for a
protocol-only or pure-extension module (no class defined); this module's
callers simply contribute no shapes.json entry for it.
""".
-spec class_name_for_module(module()) -> {ok, atom()} | error.
class_name_for_module(Module) ->
    case beamtalk_module_activation:extract_class_names(Module) of
        [ClassAtom | _] -> {ok, ClassAtom};
        [] -> error
    end.

-doc """
An ancestor's own (un-flattened) `field_types` — the per-level reader
`flatten_ancestor_map/2` calls at each step of the ancestor walk. A thin
wrapper over `ancestor_own_field_map/2` (see its doc for the shared
tolerant-degrade behaviour).
""".
-spec ancestor_field_types(atom()) -> #{atom() => atom()}.
ancestor_field_types(ClassAtom) ->
    ancestor_own_field_map(ClassAtom, field_types).

-doc """
Like `ancestor_field_types/1`, for `field_kinds` (ADR 0124 §9/B9) — the
per-level reader `class_field_shape/1`'s `field_kinds` ancestor walk calls.
Only used by `class_field_shape/1`, not `class_shape_entry/1`'s JSON
projection, which has no use for `Kind` (see `class_field_shape/1`'s doc).
""".
-spec ancestor_field_kinds(atom()) -> #{atom() => atom()}.
ancestor_field_kinds(ClassAtom) ->
    ancestor_own_field_map(ClassAtom, field_kinds).

-doc """
Shared per-level reader behind `ancestor_field_types/1`/`ancestor_field_kinds/1`
— one "read `MetaKey` off a resolved ancestor's meta, or contribute nothing"
implementation, parameterised by which `__beamtalk_meta/0` key it reads,
rather than two near-identical copies of the same `resolve_migrations/1`
call and degrade (CLAUDE.md's no-duplicate-implementations rule). Delegates
to `beamtalk_shape_migration:resolve_migrations/1`, the same tolerant lookup
`class_shape_entry/1` itself uses, so an ancestor that cannot be resolved
(unregistered, no meta) simply contributes no fields at its level — mirrors
`beamtalk_workspace_shape_store:ancestor_own_field_map/2`'s identical degrade
on the live side (a different implementation, since the live side reads a
qualified `Module:'__beamtalk_meta'()` call directly rather than going
through this build-time-shared resolver — see that module's own doc for why).
""".
-spec ancestor_own_field_map(atom(), atom()) -> #{atom() => atom()}.
ancestor_own_field_map(ClassAtom, MetaKey) ->
    case beamtalk_shape_migration:resolve_migrations(ClassAtom) of
        {ok, _Module, Meta, _Migrations} -> maps:get(MetaKey, Meta, #{});
        not_found -> #{}
    end.

-doc """
Flattened `field_types` (atom -> atom, `none` for untyped) -> shapes.json
`fields` (binary -> binary | `null`) — `none` becomes JSON `null`
(ADR 0125 §3.4's own example: `"startedAt": null`), not the `<<"Dynamic">>`
sentinel `beamtalk_workspace_shape_store:field_type_to_binary/1` uses for the
same case on the live/REPL side. The two sentinels serve different readers:
a REPL display always wants a printable type name, while a machine-readable
manifest a deploy tool diffs structurally is better served by JSON's own
"no declared type" value than by a string a naive diff would treat as just
another type name.
""".
-spec normalize_fields(#{atom() => atom()}) -> #{binary() => binary() | null}.
normalize_fields(FieldTypes) ->
    maps:fold(
        fun(FieldAtom, TypeAtom, Acc) ->
            Acc#{atom_to_binary(FieldAtom, utf8) => field_type_json(TypeAtom)}
        end,
        #{},
        FieldTypes
    ).

-spec field_type_json(atom()) -> binary() | null.
field_type_json(none) -> null;
field_type_json(TypeAtom) when is_atom(TypeAtom) -> atom_to_binary(TypeAtom, utf8).

-doc """
`shape_migrations` (integer -> atom selector) -> shapes.json `migrations`
(binary version string -> binary selector) — JSON object keys must be
strings, so the integer version number is rendered as its decimal text.
""".
-spec normalize_migrations(#{pos_integer() => atom()}) -> #{binary() => binary()}.
normalize_migrations(Migrations) ->
    maps:fold(
        fun(Version, Selector, Acc) ->
            Acc#{integer_to_binary(Version) => atom_to_binary(Selector, utf8)}
        end,
        #{},
        Migrations
    ).
