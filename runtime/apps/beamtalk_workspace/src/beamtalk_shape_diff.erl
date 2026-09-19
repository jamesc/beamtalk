%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_diff).

%%% **DDD Context:** Workspace Context

-moduledoc """
Pure shape-diff classification (ADR 0105 Phase 2).

The `state:`/`field:` counterpart to `beamtalk_signature_diff`:
given a class's previous-generation `state:`/`field:` slot set (name ->
declared type) and its newly-installed one, classifies the change as
`shape_change` (at least one slot was added, removed, or retyped) or
`no_op` (nothing comparable changed, including "no previous generation to
compare against"). Unlike `beamtalk_signature_diff:diff/2`, which only
needs to *say* what happened, this also reports exactly *which* slots
changed and how: `beamtalk_recheck:trigger_shape/2` needs the
per-field detail to pick the right dependent selectors to re-check
(`spawnWith:` for every change; a removed/retyped slot's own compiler-
generated accessor selectors only for `removed`/`retyped`, never `added` —
nothing referenced an added slot before it existed) and to attribute each
finding to the slot responsible.

Deliberately side-effect-free, mirroring `beamtalk_signature_diff` — no
class registry, no compiler port, no store. Callers pass in already-resolved
shape values; `beamtalk_workspace_shape_store:capture/1` is the usual
caller.
""".

-export([diff/2, field_name/1, reload_findings/4, suspended_finding/3]).

-export_type([
    shape/0,
    maybe_shape/0,
    field_change/0,
    classification/0,
    diff_result/0,
    generation/0,
    maybe_generation/0,
    migration_outcome/0,
    reload_finding_kind/0,
    reload_finding_severity/0,
    reload_finding/0
]).

%% A class's declared shape: `state:`/`field:` slot name -> declared type
%% name, rendered the same way the class hierarchy and `__beamtalk_meta/0`
%% do (`TypeAnnotation:type_name/0` on the compiler side). An untyped slot is
%% the `<<"Dynamic">>` sentinel, never omitted, so two shapes are always
%% directly comparable field-by-field.
-type shape() :: #{binary() => binary()}.

%% `undefined` marks "no generation recorded yet" — the seed state before any
%% class-body reload this session, or an unresolvable original (nothing to
%% compare against, so classification degrades to `no_op` rather than risk a
%% false `shape_change`).
-type maybe_shape() :: shape() | undefined.

-type field_change() ::
    {added, binary()}
    | {removed, binary()}
    | {retyped, binary(), binary(), binary()}.

-type classification() :: shape_change | no_op.

-type diff_result() :: {classification(), [field_change()]}.

%% A captured generation (ADR 0123 Phase 4, BT-3538): the flattened `shape()`
%% plus the two `__beamtalk_meta/0` keys BT-3537's language surface
%% populates — `'shape_version'` (default `1` when a class declares no
%% `shapeVersion:`) and `'shape_migrations'` (the compiler-emitted `N ->
%% migrateFromVN:` table, `#{}` when the class declares none).
%% `beamtalk_workspace_shape_store` captures one of these per class per
%% generation; `reload_findings/4` joins two of them (the previous and the
%% just-installed generation) with `diff/2`'s field-level result to produce
%% the ADR 0123 §4 findings table. `own_shape` is the *un-flattened* subset —
%% just this class's own `field_types`, no ancestors — kept alongside `shape`
%% so a pre-save precheck (`beamtalk_repl_loader:precheck_class_shape/2`,
%% ADR 0105 Phase 3) can recompute a *pending* edit's flattened shape without
%% re-walking the ancestor chain. `ancestor_shape` is the ancestor-only
%% counterpart, captured directly by `beamtalk_workspace_shape_store`'s own
%% ancestor walk (BT-3560) rather than derived as `shape` minus `own_shape`'s
%% keys: that subtraction is lossy whenever a class's own field *shadows* a
%% same-named ancestor field, since `shape` (a flat map) only keeps one value
%% per field name and the shadowed ancestor value is already gone from it by
%% the time `own_shape` is in hand. A precheck that derived it that way would
%% report a false `dropped_without_bump` for a pending edit that merely
%% removes such a shadowing override — the field is still covered by the
%% (now-visible-again) ancestor, but the subtraction can't tell.
-type generation() :: #{
    shape := shape(),
    own_shape := shape(),
    ancestor_shape := shape(),
    version := pos_integer(),
    migrations := #{pos_integer() => atom()}
}.

%% `undefined` marks "no generation recorded yet" — mirrors `maybe_shape()`.
-type maybe_generation() :: generation() | undefined.

%% `trigger_code_change/3`'s outcome for one class's live instances (ADR 0123
%% §3): how many were successfully migrated onto the new code, and which were
%% left suspended (state intact) by a failing migration hook, paired with the
%% failure reason `beamtalk_hot_reload:try_change_code/3` returned for each.
-type migration_outcome() :: #{
    migrated := non_neg_integer(),
    suspended := [{pid(), term()}]
}.

%% The five ADR 0123 §4 "Tooling" table rows, one atom each — never a free-text
%% classification, so a consuming surface can switch on `kind` without string
%% matching `message`.
-type reload_finding_kind() ::
    dropped_without_bump
    | added_only
    | bumped_without_migration
    | version_decreased
    | instances_suspended.

-type reload_finding_severity() :: hint | warning | error.

%% ADR 0123 §4's "structured payload (class, kind, fields, counts, pids)" —
%% distinct from `beamtalk_recheck:finding()` (the ADR 0105 xref-dependent-
%% caller finding, which this is not: every field below is about the
%% *reloaded class itself* and its own live instances, not a caller class
%% broken by the change). `fields`/`from_version`/`to_version`/`migrated`/
%% `suspended`/`pids` are `undefined` (or `[]`) when a given kind has nothing
%% to say for that slot — e.g. `instances_suspended` carries no `fields`, and
%% `version_decreased` carries no `pids`.
-type reload_finding() :: #{
    class := binary(),
    kind := reload_finding_kind(),
    severity := reload_finding_severity(),
    fields := [binary()],
    from_version := pos_integer() | undefined,
    to_version := pos_integer() | undefined,
    migrated := non_neg_integer() | undefined,
    suspended := non_neg_integer() | undefined,
    pids := [pid()],
    message := binary()
}.

-doc """
Classify the change from `Old` (generation N-1) to `New` (generation N).

- `Old =:= undefined` or `New =:= undefined` — nothing to compare against
  (first-ever capture this session, or a resolution failure on either side)
  — `{no_op, []}`. Advisory-never-blocking (ADR 0105) means we do not
  manufacture a finding from nothing.
- `Old =:= New` (structural equality — including both empty maps, a class
  with no state at all) — `{no_op, []}`.
- Anything else — `{shape_change, FieldChanges}`, where `FieldChanges` lists
  every slot present in one shape but not the other (`added`/`removed`) or
  present in both with a different declared type (`retyped`), in no
  particular order.
""".
-spec diff(maybe_shape(), maybe_shape()) -> diff_result().
diff(undefined, _New) ->
    {no_op, []};
diff(_Old, undefined) ->
    {no_op, []};
diff(Old, New) when Old =:= New ->
    {no_op, []};
diff(Old, New) ->
    Added = [{added, K} || K <- maps:keys(New), not maps:is_key(K, Old)],
    Removed = [{removed, K} || K <- maps:keys(Old), not maps:is_key(K, New)],
    Retyped = [
        {retyped, K, maps:get(K, Old), maps:get(K, New)}
     || K <- maps:keys(Old), maps:is_key(K, New), maps:get(K, Old) =/= maps:get(K, New)
    ],
    {shape_change, Added ++ Removed ++ Retyped}.

-doc "The slot name a `field_change()` is about, regardless of its kind.".
-spec field_name(field_change()) -> binary().
field_name({added, Name}) -> Name;
field_name({removed, Name}) -> Name;
field_name({retyped, Name, _OldType, _NewType}) -> Name.

%%====================================================================
%% reload_findings/4, suspended_finding/3 (ADR 0123 §4, BT-3538)
%%====================================================================

-doc """
Classify a class-body reload's field/version-shape drift into the first four
rows of ADR 0123 §4's findings table, joining `diff/2`'s field-level result
with the previous and just-installed generation's `'shape_version'`/
`'shape_migrations'`.

`PrevGen` is `undefined` for a class's first-ever captured generation this
session — nothing to compare a version against, so every row degrades to
`[]` (mirrors `diff/2`'s own "nothing to compare against" `no_op` rule); a
version-independent `instances_suspended` finding can still apply even then,
via `suspended_finding/3`, called separately.

At most one of `dropped_without_bump`/`added_only` ever fires for a given
`DiffResult` — the first needs at least one `removed`/`retyped` entry, the
second needs every entry to be `added`, which are mutually exclusive field
sets. `bumped_without_migration` and `version_decreased` are independent of
`DiffResult` entirely (a version can be bumped or lowered with no field
change at all, e.g. bumped defensively ahead of a later edit), so they are
evaluated unconditionally against the two versions. The returned list is
never longer than 3 findings (dropped-or-added-only, plus bump-or-decrease —
`bumped_without_migration` and `version_decreased` are themselves mutually
exclusive, since one needs `ToVersion > FromVersion` and the other
`ToVersion < FromVersion`).
""".
-spec reload_findings(binary(), maybe_generation(), generation(), diff_result()) ->
    [reload_finding()].
reload_findings(_ClassNameBin, undefined, _NewGen, _DiffResult) ->
    [];
reload_findings(ClassNameBin, #{version := FromVersion}, NewGen, DiffResult) ->
    #{version := ToVersion, migrations := Migrations} = NewGen,
    {Classification, FieldChanges} = DiffResult,
    lists:append([
        dropped_or_retyped_finding(
            ClassNameBin, FromVersion, ToVersion, Classification, FieldChanges
        ),
        added_only_finding(ClassNameBin, FromVersion, ToVersion, Classification, FieldChanges),
        bumped_without_migration_finding(ClassNameBin, FromVersion, ToVersion, Migrations),
        version_decreased_finding(ClassNameBin, FromVersion, ToVersion)
    ]).

-doc """
ADR 0123 §4 row 5 — `instances_suspended`, an **error**: it reports a
migration failure that already happened (`trigger_code_change/3`'s
`Suspended` list, `[{pid(), Reason}]`), not a prediction like the other four
rows. `[]` when nothing was left suspended. Independent of any shape/version
diff — a migration hook can raise even when the reload's own field/version
change looks innocuous (or absent), so this is always evaluated, never gated
on `reload_findings/4`'s `PrevGen =/= undefined` precondition.
""".
-spec suspended_finding(binary(), pos_integer(), migration_outcome()) -> [reload_finding()].
suspended_finding(_ClassNameBin, _ToVersion, #{suspended := []}) ->
    [];
suspended_finding(ClassNameBin, ToVersion, #{migrated := Migrated, suspended := Suspended}) ->
    Pids = [Pid || {Pid, _Reason} <- Suspended],
    Count = length(Suspended),
    Message = unicode:characters_to_binary(
        io_lib:format(
            "~p instance(s) of ~s suspended: migration failed — fix the hook and reload "
            "again, or Workspace actorAt: kill",
            [Count, ClassNameBin]
        )
    ),
    [
        #{
            class => ClassNameBin,
            kind => instances_suspended,
            severity => error,
            fields => [],
            from_version => undefined,
            to_version => ToVersion,
            migrated => Migrated,
            suspended => Count,
            pids => Pids,
            message => Message
        }
    ].

%%====================================================================
%% reload_findings/4 helpers
%%====================================================================

-spec dropped_or_retyped_finding(
    binary(), pos_integer(), pos_integer(), classification(), [field_change()]
) -> [reload_finding()].
dropped_or_retyped_finding(ClassNameBin, Version, Version, shape_change, FieldChanges) ->
    case [FC || FC <- FieldChanges, is_removed_or_retyped(FC)] of
        [] ->
            [];
        Dropped ->
            Fields = lists:usort([field_name(FC) || FC <- Dropped]),
            Message = unicode:characters_to_binary(
                io_lib:format(
                    "shape of ~s changed (~s) but shapeVersion is still ~p; bump "
                    "shapeVersion: and add class migrateFromV~p:",
                    [ClassNameBin, join_fields(Fields), Version, Version]
                )
            ),
            [
                #{
                    class => ClassNameBin,
                    kind => dropped_without_bump,
                    severity => warning,
                    fields => Fields,
                    from_version => Version,
                    to_version => Version,
                    migrated => undefined,
                    suspended => undefined,
                    pids => [],
                    message => Message
                }
            ]
    end;
dropped_or_retyped_finding(_ClassNameBin, _FromVersion, _ToVersion, _Classification, _FieldChanges) ->
    [].

-spec added_only_finding(
    binary(), pos_integer(), pos_integer(), classification(), [field_change()]
) -> [reload_finding()].
added_only_finding(ClassNameBin, Version, Version, shape_change, FieldChanges) ->
    case FieldChanges =/= [] andalso lists:all(fun is_added/1, FieldChanges) of
        true ->
            Fields = lists:usort([field_name(FC) || FC <- FieldChanges]),
            Message = unicode:characters_to_binary(
                io_lib:format(
                    "~s shape gained ~s; shapeVersion unchanged — structural fallback "
                    "applies, nothing to do",
                    [ClassNameBin, join_fields(Fields)]
                )
            ),
            [
                #{
                    class => ClassNameBin,
                    kind => added_only,
                    severity => hint,
                    fields => Fields,
                    from_version => Version,
                    to_version => Version,
                    migrated => undefined,
                    suspended => undefined,
                    pids => [],
                    message => Message
                }
            ];
        false ->
            []
    end;
added_only_finding(_ClassNameBin, _FromVersion, _ToVersion, _Classification, _FieldChanges) ->
    [].

-spec bumped_without_migration_finding(binary(), pos_integer(), pos_integer(), #{
    pos_integer() => atom()
}) -> [reload_finding()].
bumped_without_migration_finding(ClassNameBin, FromVersion, ToVersion, Migrations) when
    ToVersion > FromVersion
->
    case maps:is_key(FromVersion, Migrations) of
        true ->
            [];
        false ->
            Message = unicode:characters_to_binary(
                io_lib:format(
                    "~s shapeVersion ~p -> ~p has no migrateFromV~p:; structural fallback applies",
                    [ClassNameBin, FromVersion, ToVersion, FromVersion]
                )
            ),
            [
                #{
                    class => ClassNameBin,
                    kind => bumped_without_migration,
                    severity => hint,
                    fields => [],
                    from_version => FromVersion,
                    to_version => ToVersion,
                    migrated => undefined,
                    suspended => undefined,
                    pids => [],
                    message => Message
                }
            ]
    end;
bumped_without_migration_finding(_ClassNameBin, _FromVersion, _ToVersion, _Migrations) ->
    [].

-spec version_decreased_finding(binary(), pos_integer(), pos_integer()) -> [reload_finding()].
version_decreased_finding(ClassNameBin, FromVersion, ToVersion) when ToVersion < FromVersion ->
    Message = unicode:characters_to_binary(
        io_lib:format(
            "~s shapeVersion decreased from ~p to ~p",
            [ClassNameBin, FromVersion, ToVersion]
        )
    ),
    [
        #{
            class => ClassNameBin,
            kind => version_decreased,
            severity => warning,
            fields => [],
            from_version => FromVersion,
            to_version => ToVersion,
            migrated => undefined,
            suspended => undefined,
            pids => [],
            message => Message
        }
    ];
version_decreased_finding(_ClassNameBin, _FromVersion, _ToVersion) ->
    [].

-spec is_removed_or_retyped(field_change()) -> boolean().
is_removed_or_retyped({removed, _}) -> true;
is_removed_or_retyped({retyped, _, _, _}) -> true;
is_removed_or_retyped({added, _}) -> false.

-spec is_added(field_change()) -> boolean().
is_added({added, _}) -> true;
is_added(_) -> false.

-spec join_fields([binary()]) -> binary().
join_fields(Fields) ->
    iolist_to_binary(lists:join(<<", ">>, [<<"#", F/binary>> || F <- Fields])).
