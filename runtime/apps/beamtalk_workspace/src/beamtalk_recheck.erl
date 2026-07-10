%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_recheck).

%%% **DDD Context:** Workspace Context

-moduledoc """
Reload-triggered re-check orchestration (ADR 0105 Phase 1, BT-2778).

On every live definition change whose signature-generation diff
(`beamtalk_workspace_signature_store:capture/4`, BT-2777) is *not* `no_op`,
finds the change's known dependents and re-checks them, producing findings at
ADR 0100 severities. Generalises the Phase 0 spike
(`docs/internal/adr-0105-phase0-spike-findings.md`) to production.

## Mechanism (ADR 0105 §Mechanism steps 2-3)

1. **Dependent lookup.** `beamtalk_xref:senders_of/1` is selector-keyed —
   sites carry a caller (`owner`/`method`/`line`) and a receiver *kind*
   (self/super/ffi/other), but never a receiver *class* (ADR 0087). So a
   `size` sender on an unrelated `List` comes back alongside the real
   `Counter` dependent; xref cannot separate them.
2. **The receiver-type filter is not a pre-filter — it is what re-checking
   itself does.** Each candidate caller's *whole class* is re-checked through
   the compiler with the changed class's *current* (already-live,
   post-install) signature. A site whose receiver resolves to a different
   type simply re-checks clean against that signature, because the checker
   only applies `Counter>>getCount`'s new interface to sends whose receiver
   it infers as `Counter`. This is why the **caller cap is load-bearing**:
   every candidate is paid for with a compile before its relevance is known
   (ADR 0105 Alternatives — the xref receiver-type-key extension is the
   follow-up if this proves hot).
3. **Re-check unit is the caller's whole class**, read from the *live image*
   (`beamtalk_workspace_meta:get_class_source/1`), never disk — a flagged
   caller may itself carry unflushed edits (ADR 0082). One
   `beamtalk_compiler:diagnostics/3` port round-trip per distinct caller
   class (multiple sites in the same class collapse into one request — the
   compiler port rejects multi-class sources, so "batched" means "not one
   round-trip per call site", per the Phase 0 spike finding).

## Finding attribution (no severity escalation, ADR 0100)

The ambient class hierarchy the compiler port already accumulates
(`beamtalk_compiler_server`'s `register_class` cache, ADR 0050 Phase 4) is
current by the time this module runs — the reload's `register_class/0` /
`code:load_binary` already installed the new generation. So a
`diagnostics/3` call (with `class_hierarchy => true`) against a candidate's
live source sees the *new* signature for free; no explicit per-class
override is constructed here. `class_hierarchy` is opt-in on `diagnostics/3`
rather than the `diagnostics/2` default specifically so this module's new
behaviour does not also change the keystroke-driven cockpit editor's
diagnostics (BT-2556, `beamtalk_repl_ops_dev:diagnostics_for/2`), which
stays on the unchanged class-context-free default.

Returned diagnostics are filtered to the ones attributable to *this* reload,
not just any pre-existing issue the candidate happens to have:

- **`removal`** — a `Dnu`-category diagnostic naming the removed selector
  (`'Selector'`, matching the exact quoting `type_checker/validation.rs`
  emits) is kept. This is always `Hint` under ADR 0100 Rule 1 for a
  closed-complete receiver; `Open`/`Dynamic` receivers never produce the
  diagnostic in the first place — the checker is already silent for
  `perform:`/DNU-overriding cases, so no extra suppression is needed here.
- **`signature_change`** — any `Dnu`- or `Type`-category diagnostic is kept,
  whatever severity the ordinary type checker gave it (no escalation, per
  ADR 0105 §Severity). Unlike `removal`, this does **not** also require the
  message to name the changed selector: a signature change is discovered
  through whatever selector the caller applies to the now-different return
  value (`getCount -> String` breaks the caller's `+`, not `getCount`
  itself — confirmed empirically: the checker classifies "`String` does not
  understand `'+'`" as `Dnu`, not `Type`, since it is itself an unresolved-
  selector diagnostic on the *new* return type).
- Anything else (unrelated pre-existing diagnostics in the same class, and
  any `error`-severity diagnostic — a reload finding is never build-failing)
  is dropped.

**Documented precision limit:** this filters by *category* (+ selector name
for removals only), not by matching the diagnostic's span against the exact
xref call site. A caller class with an unrelated *pre-existing* `Dnu`/`Type`
diagnostic could be misattributed to a `signature_change` reload. The
`removal` path's selector-name match narrows this — a caller with an
unrelated pre-existing `Dnu` on the *same selector name but a different
receiver* (`someString reset` when `Counter>>reset` is what was removed) is
still misattributed, since the match is on the message substring, not the
receiver's identity — it is narrower than `signature_change`'s filter, not
immune to the same class of false positive. Precise span-to-site (and
receiver-identity) matching is a documented refinement, not built here —
consistent with the ADR's other accepted-gap tradeoffs (one level of
fan-out only, the proxy-routed-call miss).

## Scope

Produces and returns findings; publishing them to a surface (LSP / REPL
notification / workspace UI) and persisting/clearing them across reloads is
`beamtalk_workspace_findings_store` + `beamtalk_repl_loader`'s
`maybe_trigger_recheck/4` (ADR 0105 Phase 1, BT-2779) — this module stays a
pure "what changed" computation. `result()`'s `checked_owners` field exists
specifically for that consumer: it is the exact set of caller classes a
diagnostics round-trip *completed* for this trigger (`ok` status in
`recheck_owner/5`, regardless of whether that class turned out clean or
stale), which is what BT-2779 needs to know which owners' stored findings to
replace — a clean re-check still has to `put_owner(Owner, [])` its way past
a stale generation-A finding (`checked` alone can't answer "which classes",
and `findings` alone omits clean classes entirely). `trigger/4` is
synchronous and best-effort: any internal failure degrades to an empty
result rather than raising, since a re-check failure must never affect the
reload that triggered it (ADR 0105: "advisory, never blocking").
""".

-include_lib("kernel/include/logger.hrl").

-export([trigger/4, trigger_shape/2]).

-ifdef(TEST).
-export([
    group_by_owner/1,
    apply_cap/2,
    relevant_diagnostic/4,
    cap_note/1,
    shape_dependent_selectors/1,
    with_star_selector/1,
    relevant_diagnostic_shape/3
]).
-endif.

-export_type([finding/0, result/0, classification/0]).

-type classification() :: signature_change | removal | shape_change.

-type shape_field_change() :: beamtalk_shape_diff:field_change().

-type site_ref() :: #{method := binary(), line := pos_integer()}.

%% The finding shape before `to_finding/6` attaches its classification-specific
%% `note` — kept as its own type so `base_finding/6`'s spec is exact (it never
%% sets `note`; only its two callers in `to_finding/6` do).
-type finding_without_note() :: #{
    owner := binary(),
    changed_class := binary(),
    selector := binary(),
    classification := classification(),
    severity := binary(),
    category := binary() | undefined,
    message := binary(),
    sites := [site_ref()],
    start := non_neg_integer(),
    'end' := non_neg_integer()
}.

%% For `signature_change`/`removal`, `selector` is the changed method's own
%% selector. For `shape_change` (BT-2780, `to_finding_shape/5`) there is no
%% single changed selector — `selector` instead names the specific `state:`/
%% `field:` slot the finding is attributed to (`beamtalk_shape_diff:field_name/1`
%% of the matched `field_change()`), which is what a caller broken by e.g. a
%% removed field's accessor is actually about.
-type finding() :: #{
    owner := binary(),
    changed_class := binary(),
    selector := binary(),
    classification := classification(),
    severity := binary(),
    category := binary() | undefined,
    message := binary(),
    note := binary() | undefined,
    sites := [site_ref()],
    start := non_neg_integer(),
    'end' := non_neg_integer()
}.

-type result() :: #{
    findings := [finding()],
    checked := non_neg_integer(),
    total_candidates := non_neg_integer(),
    not_checked := non_neg_integer(),
    cap_note := binary() | undefined,
    checked_owners := [binary()]
}.

%%====================================================================
%% API
%%====================================================================

-doc """
Run the re-check orchestration for a reload of `{ClassNameBin, SelectorBin,
Side}` classified as `Classification` (the non-`no_op` outcome of
`beamtalk_workspace_signature_store:capture/4`).

Never raises: any internal failure is logged and degrades to an empty
`result()` (zero findings, zero candidates) — the caller should not gate the
reload's own success/reply on this.
""".
-spec trigger(binary(), binary(), instance | class, classification()) -> result().
trigger(ClassNameBin, SelectorBin, Side, Classification) ->
    try
        %% `Side` is accepted (matching the signature store's capture/4 key)
        %% but not threaded into do_trigger/3: beamtalk_xref:senders_of/1 is
        %% selector-keyed only, with no instance/class-side component, so it
        %% cannot narrow the dependent lookup. Kept in scope here purely for
        %% the ?LOG_WARNING context below on a caught failure.
        do_trigger(ClassNameBin, SelectorBin, Classification)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Re-check orchestration failed (reload unaffected)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    selector => SelectorBin,
                    side => Side,
                    classification => Classification,
                    domain => [beamtalk, runtime]
                }
            ),
            empty_result()
    end.

-doc """
Run the shape-triggered re-check orchestration (ADR 0105 Phase 2, BT-2780)
for a class-body reload of `ClassNameBin` classified as `shape_change` by
`beamtalk_workspace_shape_store:capture/1`, with `FieldChanges` the per-field
detail (`beamtalk_shape_diff:field_change()` list) that produced that
classification.

Generalises `trigger/4`'s mechanism (ADR 0105 §Mechanism steps 2-3) to a
class-shape change rather than a single selector: the dependent-lookup
selector set is not one selector but `spawnWith:` (every field change,
`added` included — a `spawnWith:` call site's key/value validity depends on
the class's *whole* current slot set) unioned with the removed/retyped
slots' own compiler-generated accessor selectors
(`shape_dependent_selectors/1` — `removed`/`retyped` only; nothing
references an `added` slot's accessor before the slot exists). Every other
step — receiver-type filtering by re-checking the whole candidate class
(reusing the same `class_hierarchy => true` ambient-cache trick, since the
reload that triggered this already installed the new shape before this runs
— see `beamtalk_repl_loader:activate_module/3`), the per-reload caller cap,
`checked_owners` for BT-2779's findings-store consumer — is identical to
`trigger/4`; see this module's moduledoc.

Never raises: any internal failure is logged and degrades to an empty
`result()`, exactly like `trigger/4`.
""".
-spec trigger_shape(binary(), [shape_field_change()]) -> result().
trigger_shape(ClassNameBin, FieldChanges) ->
    try
        do_trigger_shape(ClassNameBin, FieldChanges)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Shape re-check orchestration failed (reload unaffected)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    field_changes => FieldChanges,
                    domain => [beamtalk, runtime]
                }
            ),
            empty_result()
    end.

%%====================================================================
%% Internal
%%====================================================================

-spec empty_result() -> result().
empty_result() ->
    #{
        findings => [],
        checked => 0,
        total_candidates => 0,
        not_checked => 0,
        cap_note => undefined,
        checked_owners => []
    }.

-spec do_trigger(binary(), binary(), classification()) -> result().
do_trigger(ClassNameBin, SelectorBin, Classification) ->
    %% `binary_to_existing_atom/2`, not `binary_to_atom/2`: by the time a
    %% reload reaches this trigger the selector was just compiled into a live
    %% class, so its atom already exists — using the existing-only conversion
    %% fails fast (caught by trigger/4) instead of ever minting a fresh atom
    %% from what is, transitively, patch-source-derived text (atom-table
    %% exhaustion is a standing concern for `binary_to_atom` on
    %% externally-influenced input).
    Selector = binary_to_existing_atom(SelectorBin, utf8),
    Sites = beamtalk_xref:senders_of(Selector),
    OwnerGroups = group_by_owner(Sites),
    Cap = recheck_caller_cap(),
    {Kept, NotChecked} = apply_cap(OwnerGroups, Cap),
    Outcomes = [
        {Owner, recheck_owner(Owner, OwnerSites, ClassNameBin, SelectorBin, Classification)}
     || {Owner, OwnerSites} <- Kept
    ],
    Findings = lists:flatmap(
        fun({_Owner, {_Status, OwnerFindings}}) -> OwnerFindings end, Outcomes
    ),
    %% `checked` counts candidates a diagnostics round-trip actually completed
    %% for — not every *kept* candidate: one with no recorded live source (a
    %% stdlib/dependency class) or a compiler-port failure never ran a check,
    %% so it must not inflate the "N callers checked" figure the ADR 0105 demo
    %% reports ("2 callers re-checked, 1 stale"). `checked_owners` is the same
    %% set, named rather than counted — BT-2779's findings-store consumer
    %% needs to know exactly *which* owners a check completed for (including
    %% ones that came back clean), not just how many.
    CheckedOwners = [
        atom_to_binary(Owner, utf8)
     || {Owner, {ok, _}} <- Outcomes
    ],
    #{
        findings => Findings,
        checked => length(CheckedOwners),
        total_candidates => length(OwnerGroups),
        not_checked => NotChecked,
        cap_note => cap_note(NotChecked),
        checked_owners => CheckedOwners
    }.

-spec do_trigger_shape(binary(), [shape_field_change()]) -> result().
do_trigger_shape(ClassNameBin, FieldChanges) ->
    Selectors = shape_dependent_selectors(FieldChanges),
    Sites = lists:flatmap(fun beamtalk_xref:senders_of/1, Selectors),
    OwnerGroups = group_by_owner(Sites),
    Cap = recheck_caller_cap(),
    {Kept, NotChecked} = apply_cap(OwnerGroups, Cap),
    Outcomes = [
        {Owner, recheck_owner_for_shape(Owner, OwnerSites, ClassNameBin, FieldChanges)}
     || {Owner, OwnerSites} <- Kept
    ],
    Findings = lists:flatmap(
        fun({_Owner, {_Status, OwnerFindings}}) -> OwnerFindings end, Outcomes
    ),
    CheckedOwners = [
        atom_to_binary(Owner, utf8)
     || {Owner, {ok, _}} <- Outcomes
    ],
    #{
        findings => Findings,
        checked => length(CheckedOwners),
        total_candidates => length(OwnerGroups),
        not_checked => NotChecked,
        cap_note => cap_note(NotChecked),
        checked_owners => CheckedOwners
    }.

-doc """
The selector set whose senders are candidate dependents of a shape change:
`spawnWith:` (always — every field change, `added` included, can flip a call
site's key/value validity), plus the getter and `with*:` setter selectors of
every `removed`/`retyped` slot (never `added` — nothing could have
referenced a slot that did not exist in the previous generation).
Deduplicated (`lists:usort/1`) since `senders_of/1` is otherwise cheap to
run twice but there is no reason to.
""".
-spec shape_dependent_selectors([shape_field_change()]) -> [atom()].
shape_dependent_selectors(FieldChanges) ->
    AccessorSelectors = lists:flatmap(fun accessor_selectors/1, FieldChanges),
    lists:usort(['spawnWith:' | AccessorSelectors]).

-spec accessor_selectors(shape_field_change()) -> [atom()].
accessor_selectors({added, _Name}) ->
    [];
accessor_selectors({removed, Name}) ->
    field_accessor_atoms(Name);
accessor_selectors({retyped, Name, _OldType, _NewType}) ->
    field_accessor_atoms(Name).

-doc """
The getter (unary, the field name itself) and `with*:` setter selector atoms
for `NameBin`, as already-interned atoms only. Only `ClassKind::Value`
classes get these auto-generated (`value_type_codegen.rs`'s
`compute_auto_slot_methods` returns `None` for `Object`/`Actor` — see
`synthetic_selectors.rs`'s moduledoc), so for a `removed`/`retyped` slot on
an `Actor`/`Object` class neither selector atom was ever interned and both
lookups fall through the `badarg` clause below, contributing nothing to
`shape_dependent_selectors/1` beyond the unconditional `spawnWith:` — correct,
not a gap: those classes have no synthesized accessor for anything external
to call in the first place, so there is no sender to find. For a `Value`
class slot, `code:load_binary` interned both accessor selectors the first
time any generation of the class compiled with that slot present, so the
lookup here succeeds; a selector that was somehow never interned (e.g. a
slot that existed only fleetingly, never in an installed generation — not
reachable via `beamtalk_shape_diff:diff/2`'s contract) is silently skipped
rather than minting a fresh atom from reload-derived text, mirroring
`do_trigger/3`'s `binary_to_existing_atom/2` rationale.
""".
-spec field_accessor_atoms(binary()) -> [atom()].
field_accessor_atoms(NameBin) ->
    lists:filtermap(
        fun(SelBin) ->
            try
                {true, binary_to_existing_atom(SelBin, utf8)}
            catch
                error:badarg -> false
            end
        end,
        [NameBin, with_star_selector(NameBin)]
    ).

-doc """
The `with*:` copy-setter selector name for a slot field (mirrors
`crate::synthetic_selectors::with_star_selector` on the Rust side — see
`crates/beamtalk-core/src/synthetic_selectors.rs` — capitalising the first
byte and wrapping in `with`/`:`). Beamtalk identifiers are ASCII, so a
byte-wise uppercase of the first character is exact, not an approximation.
""".
-spec with_star_selector(binary()) -> binary().
with_star_selector(<<>>) ->
    <<"with:">>;
with_star_selector(<<First, Rest/binary>>) when First >= $a, First =< $z ->
    <<"with", (First - 32), Rest/binary, ":">>;
with_star_selector(<<First, Rest/binary>>) ->
    <<"with", First, Rest/binary, ":">>.

-doc "Read the per-reload caller cap (ADR 0105 §Mechanism step 2).".
-spec recheck_caller_cap() -> pos_integer().
recheck_caller_cap() ->
    application:get_env(beamtalk_workspace, recheck_caller_cap, 20).

-doc """
Group xref sites by caller class, deduping so multiple sends of the changed
selector within the same class collapse into one re-check candidate
(`{Owner, [Site, ...]}`, sorted by `Owner` for deterministic ordering — see
`apply_cap/2`'s doc for what that ordering means for which candidates get
dropped when the cap bites).
""".
-spec group_by_owner([map()]) -> [{atom(), [map()]}].
group_by_owner(Sites) ->
    %% `maps:groups_from_list/2` is a single O(n) pass (vs. an O(owners *
    %% sites) re-scan of `Sites` per owner) — matters once a common selector
    %% (`size`, `at:`) has thousands of sites. `lists:keysort/2` on the atom
    %% keys restores the deterministic order the rest of this module (and its
    %% tests) depend on; `maps:groups_from_list/2` does not guarantee one.
    Grouped = maps:groups_from_list(fun(S) -> maps:get(owner, S) end, Sites),
    lists:keysort(1, maps:to_list(Grouped)).

-doc """
Keep at most `Cap` candidates; return `{Kept, NotCheckedCount}` — the
"N more not checked" note is `NotCheckedCount` rendered by `cap_note/1`.

`Candidates` arrives sorted by owner class name (`group_by_owner/1`), so the
kept set is always the alphabetically-first `Cap` owners — deterministic
across runs (not random, not order-of-discovery-dependent), but also not
relevance-ranked: an over-cap reload always drops the same
alphabetically-last owners, every time, regardless of which caller is
actually most likely to matter. Load-bearing only as an interim guard (ADR
0105 Alternatives) — the real fix for a hot common selector is the xref
receiver-type-key extension.
""".
-spec apply_cap([T], pos_integer()) -> {[T], non_neg_integer()}.
apply_cap(Candidates, Cap) ->
    Total = length(Candidates),
    if
        Total =< Cap -> {Candidates, 0};
        true -> {lists:sublist(Candidates, Cap), Total - Cap}
    end.

-spec cap_note(non_neg_integer()) -> binary() | undefined.
cap_note(0) ->
    undefined;
cap_note(N) ->
    iolist_to_binary(io_lib:format("~p more not checked", [N])).

-doc """
Re-check one candidate caller class: read its live source (never disk — a
flagged caller may itself carry unflushed edits, ADR 0082), run diagnostics
through the compiler port, and keep only the diagnostics attributable to
this reload (`relevant_diagnostic/4`).

Returns `{Status, Findings}` — `Status` is `ok` only when a diagnostics
round-trip actually completed for this candidate, so `do_trigger/3` can count
`checked` accurately: a candidate with no live source recorded (never
installed through `beamtalk_repl_loader`, e.g. a stdlib/dependency class) is
`skipped` with a logged warning rather than falling back to disk, and a
compiler-port failure for this one candidate is `failed` — both degrade to
"no findings for this caller" rather than failing the whole reload's
orchestration, but neither counts as a completed check.
""".
-spec recheck_owner(atom(), [map()], binary(), binary(), classification()) ->
    {ok | skipped | failed, [finding()]}.
recheck_owner(Owner, OwnerSites, ClassNameBin, SelectorBin, Classification) ->
    OwnerBin = atom_to_binary(Owner, utf8),
    case beamtalk_workspace_meta:get_class_source(OwnerBin) of
        undefined ->
            ?LOG_WARNING(
                "Re-check skipped: no live source recorded for candidate caller",
                #{owner => OwnerBin, domain => [beamtalk, runtime]}
            ),
            {skipped, []};
        Source ->
            SourceBin = unicode:characters_to_binary(Source),
            %% `class_hierarchy => true` opts into the ambient class cache
            %% (beamtalk_compiler_server:diagnostics/3) — this re-check is
            %% the one caller that needs it; the keystroke-driven cockpit
            %% editor path (BT-2556) stays on the class-context-free default.
            case
                beamtalk_compiler:diagnostics(SourceBin, <<"expression">>, #{
                    class_hierarchy => true
                })
            of
                {ok, Diagnostics} ->
                    SiteRefs = [site_ref(S) || S <- OwnerSites],
                    Findings = [
                        to_finding(OwnerBin, ClassNameBin, SelectorBin, Classification, SiteRefs, D)
                     || D <- Diagnostics,
                        relevant_diagnostic(D, ClassNameBin, SelectorBin, Classification)
                    ],
                    {ok, Findings};
                {error, Reason} ->
                    ?LOG_WARNING(
                        "Re-check compile failed for candidate caller (no findings reported)",
                        #{owner => OwnerBin, reason => Reason, domain => [beamtalk, runtime]}
                    ),
                    {failed, []}
            end
    end.

-spec site_ref(map()) -> site_ref().
site_ref(Site) ->
    #{
        method => atom_to_binary(maps:get(method, Site), utf8),
        line => maps:get(line, Site)
    }.

-doc """
Is `Diagnostic` attributable to this reload? See the moduledoc's "Finding
attribution" section for the exact rule and its documented precision limit.
`error`-severity is always dropped — a reload finding is advisory, never
build-failing (ADR 0105 §Severity).
""".
-spec relevant_diagnostic(map(), binary(), binary(), classification()) -> boolean().
relevant_diagnostic(#{severity := <<"error">>}, _ClassNameBin, _SelectorBin, _Classification) ->
    false;
relevant_diagnostic(
    #{category := <<"Dnu">>, message := Message}, _ClassNameBin, SelectorBin, removal
) ->
    binary:match(Message, quoted_selector(SelectorBin)) =/= nomatch;
relevant_diagnostic(#{category := Category}, _ClassNameBin, _SelectorBin, signature_change) when
    Category =:= <<"Dnu">>; Category =:= <<"Type">>
->
    true;
relevant_diagnostic(_Diagnostic, _ClassNameBin, _SelectorBin, _Classification) ->
    false.

-spec quoted_selector(binary()) -> binary().
quoted_selector(SelectorBin) ->
    <<"'", SelectorBin/binary, "'">>.

-spec to_finding(binary(), binary(), binary(), classification(), [site_ref()], map()) -> finding().
to_finding(OwnerBin, ClassNameBin, SelectorBin, removal, SiteRefs, Diagnostic) ->
    (base_finding(OwnerBin, ClassNameBin, SelectorBin, removal, SiteRefs, Diagnostic))#{
        note => <<"removed by the reload of ", ClassNameBin/binary>>
    };
to_finding(OwnerBin, ClassNameBin, SelectorBin, signature_change, SiteRefs, Diagnostic) ->
    (base_finding(OwnerBin, ClassNameBin, SelectorBin, signature_change, SiteRefs, Diagnostic))#{
        note => undefined
    }.

-spec base_finding(binary(), binary(), binary(), classification(), [site_ref()], map()) ->
    finding_without_note().
base_finding(
    OwnerBin,
    ClassNameBin,
    SelectorBin,
    Classification,
    SiteRefs,
    #{
        message := Message, severity := Severity, start := Start, 'end' := End
    } = Diagnostic
) ->
    #{
        owner => OwnerBin,
        changed_class => ClassNameBin,
        selector => SelectorBin,
        classification => Classification,
        severity => Severity,
        category => maps:get(category, Diagnostic, undefined),
        message => Message,
        sites => SiteRefs,
        start => Start,
        'end' => End
    }.

%%====================================================================
%% Shape-change re-check (ADR 0105 Phase 2, BT-2780)
%%====================================================================

-doc """
Re-check one candidate caller class against a shape change — structurally
identical to `recheck_owner/5` (same live-source read, same
`class_hierarchy => true` diagnostics round-trip, same `{Status, Findings}`
contract) but filters/attributes via `relevant_diagnostic_shape/3` instead
of `relevant_diagnostic/4`.
""".
-spec recheck_owner_for_shape(atom(), [map()], binary(), [shape_field_change()]) ->
    {ok | skipped | failed, [finding()]}.
recheck_owner_for_shape(Owner, OwnerSites, ClassNameBin, FieldChanges) ->
    OwnerBin = atom_to_binary(Owner, utf8),
    case beamtalk_workspace_meta:get_class_source(OwnerBin) of
        undefined ->
            ?LOG_WARNING(
                "Shape re-check skipped: no live source recorded for candidate caller",
                #{owner => OwnerBin, domain => [beamtalk, runtime]}
            ),
            {skipped, []};
        Source ->
            SourceBin = unicode:characters_to_binary(Source),
            case
                beamtalk_compiler:diagnostics(SourceBin, <<"expression">>, #{
                    class_hierarchy => true
                })
            of
                {ok, Diagnostics} ->
                    SiteRefs = [site_ref(S) || S <- OwnerSites],
                    Findings = [
                        to_finding_shape(OwnerBin, ClassNameBin, SiteRefs, D, Matched)
                     || D <- Diagnostics,
                        {true, Matched} <- [
                            relevant_diagnostic_shape(D, ClassNameBin, FieldChanges)
                        ]
                    ],
                    {ok, Findings};
                {error, Reason} ->
                    ?LOG_WARNING(
                        "Shape re-check compile failed for candidate caller (no findings reported)",
                        #{owner => OwnerBin, reason => Reason, domain => [beamtalk, runtime]}
                    ),
                    {failed, []}
            end
    end.

-doc """
Is `Diagnostic` attributable to this shape change, and if so, which
`field_change()` is it most specifically about?

- **`spawnWith:` key/value diagnostics** (`check_spawn_with_map_keys`/
  `check_spawn_with_value` in `crates/beamtalk-core`'s type checker) are
  always `Type`-category and always name both the class and the literal
  text `"state key"`, quoting the slot name in backticks — matched by
  `match_field_change_by_name/2` against every changed slot's name, added
  slots included (an `added` slot only ever shows up *here*, never in the
  accessor-removal branch below, since nothing referenced it before).
- **A removed slot's accessor** (`Dnu` — the getter/setter method just
  stopped existing, an ordinary unresolved-selector diagnostic like any
  other) is matched by `match_removed_accessor/2` against the `removed`
  slots' quoted getter/setter selector names.
- **A retyped slot's accessor** propagates its new return type into
  whatever the caller does with it (`self thing count + 1` when `count`'s
  declared type changed) — confirmed by `relevant_diagnostic/4`'s own
  moduledoc note to always surface as `Dnu` (an unresolved-selector
  diagnostic on the new return type, e.g. `'+'` not understood), never
  `Type`, so it cannot be pinned to a quoted selector the way a removal can.
  `retyped_fallback/1` — the same "not by exact site" precision limit
  `relevant_diagnostic/4`'s moduledoc documents and accepts for
  `signature_change` — attributes any otherwise-unmatched `Dnu` to the
  *first* `retyped` change in `FieldChanges` when one is present. **Known,
  accepted limitation (adversarial review, BT-2780):** when a single reload
  retypes *two or more* slots, this picks list order over the true culprit —
  there is no field-name signal in the `Dnu` message to disambiguate which
  retyped slot actually caused it (unlike the `Type`/removed-accessor
  branches above, which match on quoted text). The finding's `note` can
  therefore name the wrong field when multiple retypes land in one reload.
  Not fixed here: closing it needs either per-selector call-site tracking
  (well past what a `class_hierarchy => true` diagnostics round-trip
  gives) or accepting that ambiguity in the finding itself (a `finding()`
  shape change) — both bigger than this pass. See
  `relevant_diagnostic_shape_retyped_fallback_picks_first_when_multiple_test/0`
  for the pinned behaviour.
- `error`-severity is always dropped, matching `relevant_diagnostic/4`.
""".
-spec relevant_diagnostic_shape(map(), binary(), [shape_field_change()]) ->
    {true, shape_field_change()} | false.
relevant_diagnostic_shape(#{severity := <<"error">>}, _ClassNameBin, _FieldChanges) ->
    false;
relevant_diagnostic_shape(
    #{category := <<"Type">>, message := Message}, ClassNameBin, FieldChanges
) ->
    case
        binary:match(Message, <<"state key">>) =/= nomatch andalso
            binary:match(Message, ClassNameBin) =/= nomatch
    of
        true -> match_field_change_by_name(Message, FieldChanges);
        false -> false
    end;
relevant_diagnostic_shape(
    #{category := <<"Dnu">>, message := Message}, _ClassNameBin, FieldChanges
) ->
    case match_removed_accessor(Message, FieldChanges) of
        {true, _} = Match -> Match;
        false -> retyped_fallback(FieldChanges)
    end;
relevant_diagnostic_shape(_Diagnostic, _ClassNameBin, _FieldChanges) ->
    false.

-spec match_field_change_by_name(binary(), [shape_field_change()]) ->
    {true, shape_field_change()} | false.
match_field_change_by_name(Message, FieldChanges) ->
    Matches = [
        FC
     || FC <- FieldChanges,
        binary:match(Message, backtick_quoted(beamtalk_shape_diff:field_name(FC))) =/= nomatch
    ],
    first_match(Matches).

-spec match_removed_accessor(binary(), [shape_field_change()]) ->
    {true, shape_field_change()} | false.
match_removed_accessor(Message, FieldChanges) ->
    Matches = [
        FC
     || {removed, Name} = FC <- FieldChanges,
        (binary:match(Message, quoted_selector(Name)) =/= nomatch orelse
            binary:match(Message, quoted_selector(with_star_selector(Name))) =/= nomatch)
    ],
    first_match(Matches).

-spec retyped_fallback([shape_field_change()]) -> {true, shape_field_change()} | false.
retyped_fallback(FieldChanges) ->
    first_match([FC || {retyped, _, _, _} = FC <- FieldChanges]).

-spec first_match([shape_field_change()]) -> {true, shape_field_change()} | false.
first_match([First | _]) -> {true, First};
first_match([]) -> false.

-spec backtick_quoted(binary()) -> binary().
backtick_quoted(NameBin) ->
    <<"`", NameBin/binary, "`">>.

-doc """
Build a shape-change finding, attributing it to `FieldChange` — `selector`
is `FieldChange`'s slot name (see `finding()`'s doc for why), and `note`
describes what happened to that slot in the reload.
""".
-spec to_finding_shape(binary(), binary(), [site_ref()], map(), shape_field_change()) -> finding().
to_finding_shape(OwnerBin, ClassNameBin, SiteRefs, Diagnostic, FieldChange) ->
    SelectorBin = beamtalk_shape_diff:field_name(FieldChange),
    (base_finding(OwnerBin, ClassNameBin, SelectorBin, shape_change, SiteRefs, Diagnostic))#{
        note => field_change_note(ClassNameBin, FieldChange)
    }.

-spec field_change_note(binary(), shape_field_change()) -> binary().
field_change_note(ClassNameBin, {added, Name}) ->
    <<"state field `", Name/binary, "` added by the reload of ", ClassNameBin/binary>>;
field_change_note(ClassNameBin, {removed, Name}) ->
    <<"state field `", Name/binary, "` removed by the reload of ", ClassNameBin/binary>>;
field_change_note(ClassNameBin, {retyped, Name, OldType, NewType}) ->
    <<"state field `", Name/binary, "` retyped by the reload of ", ClassNameBin/binary, " (",
        OldType/binary, " -> ", NewType/binary, ")">>.
