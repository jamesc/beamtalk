%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_diff).

%%% **DDD Context:** Workspace Context

-moduledoc """
Pure shape-diff classification (ADR 0105 Phase 2, BT-2780).

The `state:`/`field:` counterpart to `beamtalk_signature_diff` (BT-2777):
given a class's previous-generation `state:`/`field:` slot set (name ->
declared type) and its newly-installed one, classifies the change as
`shape_change` (at least one slot was added, removed, or retyped) or
`no_op` (nothing comparable changed, including "no previous generation to
compare against"). Unlike `beamtalk_signature_diff:diff/2`, which only
needs to *say* what happened, this also reports exactly *which* slots
changed and how: `beamtalk_recheck:trigger_shape/2` (BT-2780) needs the
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

-export([diff/2, field_name/1]).

-export_type([shape/0, maybe_shape/0, field_change/0, classification/0, diff_result/0]).

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
