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

## Pre-save advisory (`trigger_pending/5`, ADR 0105 Phase 3, BT-2782)

`trigger/4` re-checks dependents against the changed class's *installed*
signature, read from the compiler port's ambient class-hierarchy cache
(`beamtalk_compiler_server`'s `classes` map). A pre-save advisory needs the
same dependent re-check run against a *pending* edit that has not installed
— so the cache still holds the old generation. `trigger_pending/5` bridges
this by temporarily overwriting the changed class's ambient entry with the
pending signature spliced in, running the unmodified `do_trigger/3` used by
`trigger/4`, then restoring the original entry — see `trigger_pending/5`'s
own doc for the exact mechanism and its accepted concurrency tradeoff. Never
installs anything and never touches `beamtalk_workspace_findings_store`
(BT-2779) — there is nothing to publish or clear until an actual install
happens; the caller (`beamtalk_repl_eval:precheck_method/5`) returns the
`result()` directly to whichever surface asked.

## Whole-image re-check (`trigger_image/0`, ADR 0105 Phase 3, BT-2782)

`Workspace recheckImage` / `:recheck image` is the "complete but unbounded"
path ADR 0105's Alternatives section keeps out of the default per-reload
trigger: re-check every live class the workspace has a source for, not just
the xref-filtered dependents of one changed selector. `trigger_image/0` is a
separate, simpler entry point — there is no single changed selector to
attribute findings to or cap candidates against (the whole point is
"complete"), so it returns `image_result()` (`checked`/`stale`/`findings`),
not `result()`.

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

`result()`'s `not_checked_owners` field exists for the same consumer, as
`checked_owners`'s complement: it is the caller-cap-dropped candidates (the
alphabetically-last owners `apply_cap/2` excluded from `Kept`), named rather
than only counted (`not_checked`). BT-2802: a candidate dropped by the cap
this reload may already have a stored finding from an earlier reload where
it *was* checked — that finding was never re-verified against the current
generation and must not keep asserting itself as current forever.
`beamtalk_repl_loader:maybe_run_recheck/4` uses this field to mark any such
pre-existing finding's `note` as possibly-stale in place, rather than either
silently dropping it (could hide a real, still-live problem) or leaving it
looking freshly-verified (the BT-2802 bug).

**`not_verified_owners` (BT-2828)** widens that same treatment to two more
ways a `Kept` candidate can end up with nothing to show for it:
`recheck_owner/5` (and `recheck_owner_for_shape/4`) returns `{skipped, []}`
when `beamtalk_workspace_meta:get_class_source/1` has no live source for the
owner, and `{failed, []}` when the `diagnostics/3` round-trip itself errors
out or the compiler port call fails/times out. Both outcomes fall through
`checked_owners`'s `{Owner, {ok, _}}` filter exactly like a cap-dropped
candidate does, but — before this field existed — were *also* absent from
`not_checked_owners` (strictly `Candidates -- Kept`, computed before any
individual re-check runs), so an owner in this state was in neither list:
never replaced (not checked) and never marked stale (not cap-dropped) — the
identical stranded-finding symptom BT-2802 fixed, reached via source/
diagnostics unavailability instead of the N-candidate cap. `not_verified_owners`
is `not_checked_owners` unioned with every `Kept` candidate whose outcome
status was not `ok`, so `beamtalk_repl_loader` has one single set to feed
into its cap-dropped staleness-marking path (`mark_unverified_findings_stale/2`)
covering all three "never actually re-verified" cases.
""".

-include_lib("kernel/include/logger.hrl").

-export([
    trigger/4,
    trigger_shape/2,
    trigger_pending/5,
    trigger_image/0,
    trigger_leaf_change/1,
    trigger_alias_change/1
]).

-ifdef(TEST).
-export([
    group_by_owner/1,
    apply_cap/2,
    relevant_diagnostic/4,
    cap_note/1,
    shape_dependent_selectors/1,
    with_star_selector/1,
    relevant_diagnostic_shape/3,
    override_method_signature/4,
    relevant_image_diagnostic/1,
    field_change_note/3,
    not_verified_owners/2,
    relevant_diagnostic_leaf_change/2,
    class_name_mentioned/2,
    relevant_diagnostic_alias_change/1,
    alias_change_candidates/1
]).
-endif.

-export_type([finding/0, result/0, classification/0, image_finding/0, image_result/0]).

-type classification() :: signature_change | removal | shape_change | leaf_change | alias_change.

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
    checked_owners := [binary()],
    not_checked_owners := [binary()],
    not_verified_owners := [binary()]
}.

%% A whole-image finding (`trigger_image/0`) — lighter than `finding()`: no
%% single changed selector to attribute it to, so there is no
%% `changed_class`/`selector`/`classification`/`note`/`sites` (a class's own
%% diagnostic already carries its own `start`/`end` span; there is no xref
%% call-site to translate against, unlike `finding()`'s `sites`).
-type image_finding() :: #{
    owner := binary(),
    severity := binary(),
    category := binary() | undefined,
    message := binary(),
    start := non_neg_integer(),
    'end' := non_neg_integer()
}.

-type image_result() :: #{
    findings := [image_finding()],
    checked := non_neg_integer(),
    stale := non_neg_integer()
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

-doc """
Pre-save advisory (ADR 0105 Phase 3, BT-2782): re-check `{ClassNameBin,
SelectorBin, Side}`'s known dependents against `PendingSignature` — a
freshly-compiled but **not-yet-installed** signature — instead of the
already-live one `trigger/4` relies on.

`PendingSignature` is the same `#{return_type := binary(), param_types :=
[binary()]}` shape `beamtalk_workspace_signature_store:capture/4` records,
computed by compiling the pending edit (e.g.
`beamtalk_repl_compiler:compile_method_reload/2`) without calling
`code:load_binary/3`.

## Mechanism: a scoped, temporary ambient-cache swap

`recheck_owner/5` (reused unchanged via `do_trigger/3`) sees the changed
class's signature through the compiler port's ambient class-hierarchy cache
(`beamtalk_compiler_server`'s `classes` map, injected by `diagnostics/3`'s
`class_hierarchy => true`). That cache reflects whatever last *installed* —
for a pending edit, nothing has, so it still holds the old generation. This
function:

1. Reads the changed class's current ambient meta
   (`beamtalk_compiler_server:get_classes/0`).
2. Splices `PendingSignature` into that meta's `method_info` (or
   `class_method_info` for `Side =:= class`) entry for `SelectorBin`,
   preserving every other field (arity, other methods, fields, superclass).
3. Casts the spliced meta into the ambient cache
   (`beamtalk_compiler_server:register_class/2`), runs the *exact* same
   `do_trigger/3` `trigger/4` uses, then restores the original meta —
   `after` guarantees the restore runs even if `do_trigger/3` raises (caught
   one level up by `trigger_pending/5`'s own try/catch, which still needs the
   real generation back in place for the next request).

**Ordering invariant this relies on** (same shape as
`beamtalk_repl_loader:load_recompiled_method/8`'s documented invariant): the
cast in step 3 and every `diagnostics/3` call `do_trigger/3` subsequently
makes go to the *same* `beamtalk_compiler_server` process from this *same*
calling process — Erlang's per-sender mailbox ordering guarantees the cast
is processed before any of those calls, so every candidate's diagnostics
round-trip sees the spliced (pending) meta, never a race against the cast
itself. This would break only if `register_class/2` or `diagnostics/3` ever
routed through a different process/mailbox than a direct `gen_server`
send to `beamtalk_compiler_server`.

No ambient meta at all for the changed class (never registered this
session — brand new class, or the workspace just restarted) means there is
no baseline to splice into and no `class_hierarchy` context for the
checker to resolve receivers against either; this degrades to
`empty_result()` rather than guess.

**Accepted race (advisory-only, same risk tolerance as the rest of ADR
0105):** the swap is not synchronised against `beamtalk_compiler_server`'s
other callers. A concurrent compile/diagnostics request that lands inside
the swap window sees the hypothetical pending signature instead of the real
live one — a wrong-but-momentary result on an advisory-only surface, not a
build/runtime failure. Not fixed here; mirrors
`beamtalk_workspace_findings_store`'s and
`beamtalk_workspace_signature_store`'s own documented concurrency gaps.

A sharper variant of the same gap: the `after`-block restore always writes
back the *snapshot* taken in step 1, not "whatever is ambient now". If a
real `compile:source:`/`reload` for this same class commits a genuine
`register_class/2` from a different process while this swap window is open,
the restore overwrites that real commit with the stale pre-swap snapshot —
unlike the read-only race above, this one is not self-healing (the next
real compile/diagnostics call sees the reverted, stale generation until
something re-registers it). Accepted for the same reason: both classes of
race require a concurrent write to this *specific* class landing in a
narrow, single-request window, on an advisory surface where the ADR's own
risk tolerance is "momentary noise, not corruption" — not fixed here.

Never installs anything and never touches `beamtalk_workspace_findings_store`
— nothing has changed on the live image yet, so there is nothing to publish
or clear. Never raises: degrades to `empty_result()` on any internal failure,
mirroring `trigger/4`.
""".
-spec trigger_pending(
    binary(),
    binary(),
    instance | class,
    classification(),
    beamtalk_workspace_signature_store:signature()
) -> result().
trigger_pending(ClassNameBin, SelectorBin, Side, Classification, PendingSignature) ->
    try
        do_trigger_pending(ClassNameBin, SelectorBin, Side, Classification, PendingSignature)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Pre-save re-check orchestration failed (advisory only, save unaffected)",
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
Whole-image re-check (ADR 0105 Phase 3, BT-2782) backing `Workspace
recheckImage` / `:recheck image` — the "complete but unbounded" path ADR
0105's Alternatives section keeps out of the default per-reload trigger.
Re-checks every live class the workspace has a recorded source for
(`beamtalk_workspace_meta:all_class_sources/0`) against the **current**
ambient class hierarchy — the same `diagnostics/3` call `recheck_owner/5`
makes for one candidate, just applied to every live class rather than an
xref-filtered subset, and against the class's own already-installed
signature (unlike `trigger_pending/5`, there is nothing pending here).

There is no single changed selector to attribute findings to or cap
candidates against (completeness is the point), so relevance is just "any
non-`error` `Dnu`/`Type` diagnostic" (`relevant_image_diagnostic/1`) — no
selector-name matching, no classification, no per-site translation.

`checked` counts classes whose diagnostics round-trip completed (mirrors
`trigger/4`'s `checked` semantics: a compiler-port failure for one class
degrades that class to "not checked" rather than aborting the whole scan —
`recheck_image_class/2` catches both an `{error, Reason}` compile result
and the compiler port's `gen_server:call/3` itself exiting, e.g. `timeout`).
`stale` counts distinct classes with at least one finding. Never raises:
degrades to `#{findings => [], checked => 0, stale => 0}` on any internal
failure.

**Cost, beyond "unbounded latency" for the caller:** each class's
diagnostics round-trip is one more synchronous `gen_server:call` against
the single, shared, workspace-wide `beamtalk_compiler_server` — the same
process every other surface's compiles/diagnostics/completions go through.
`trigger_image/0` makes these calls serially, one live class at a time, for
the duration of the whole sweep. On an image with many live classes this is
not just "the `:recheck image` caller waits a while" — every other
connected client's hover/completion/save requests queue up behind it on
the same server for that whole window, since `beamtalk_compiler_server`
processes one request at a time. Not fixed here (ADR 0105 accepts
`trigger_image/0`'s latency as a deliberate unbounded-but-explicit,
on-demand tradeoff); a future revision batching/parallelising the sweep
would need to weigh that against `beamtalk_compiler_server`'s own
single-process-serialises-everything design.
""".
-spec trigger_image() -> image_result().
trigger_image() ->
    try
        do_trigger_image()
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Whole-image re-check failed",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            ),
            #{findings => [], checked => 0, stale => 0}
    end.

-doc """
BT-2856 / ADR 0107 Phase A: re-check for every superclass in
`SuperclassBins` that a live class-body reload just made non-leaf (gained
its first subclass — see
`beamtalk_repl_loader:superclasses_losing_leaf_status/1`, the detection
half this composes with).

**BT-2873:** `SuperclassBins` is the *whole* list a single reload event
produced (`superclasses_losing_leaf_status/1` can return more than one name
when one reload installs several classes at once, each subclassing a
different previously-leaf superclass) — this runs **one** whole-image sweep
covering all of them, not one independent sweep per superclass. Each live
class's source is compiled exactly once; a diagnostic naming any superclass
in the batch is attributed to that specific superclass
(`recheck_owner_for_leaf_change/3`), so the `result()`'s `findings` list can
mix `changed_class` values when the batch has more than one entry. This
matters because `trigger_leaf_change/1` is already "at least as expensive as
`trigger_image/0`" per this module's own doc — paying for N independent
whole-image sweeps when N superclasses lose leaf status in the same reload
would multiply that cost for no benefit, since every sweep would recompile
the exact same candidate set against the exact same (already fully updated)
hierarchy.

## Why this exists

ADR 0107 Phase A's `Type` pattern (`binding :: ClassName`) and
`matchExhaustive:`'s residual computation over a closed `Known | Nil` union
are both restricted to **leaf** classes at compile time
(`match_validators:validate_type_pattern_class`'s "has subclasses" error,
BT-2854; `is_concrete_leaf_class`, BT-2856). A `matchExhaustive:` site
proved exhaustive while `SuperclassBin` was leaf has that proof silently
invalidated the moment a live reload gives it a first subclass — with
nothing to catch it, the first instance of the new subclass to reach that
`case` crashes at runtime with an opaque Erlang `case_clause` error (ADR
0107 Constraints §Hot reload). This closes that gap.

## Why this cannot reuse `trigger/4`/`trigger_shape/2`'s xref-based lookup

Both existing triggers query `beamtalk_xref:senders_of/1` — a **selector**-
keyed index of message-send call sites. There is no selector a
`matchExhaustive:`/`Type`-pattern site "sends" that names the class it
tests, so xref has nothing to look up here (confirmed: no such index exists
anywhere in this codebase as of BT-2856). Absent a purpose-built index
(a real follow-up — see the moduledoc note below), the only currently-shippable
way to find every affected site is `trigger_image/0`'s own strategy: recompile
every live class's own recorded source against the current (now-updated)
hierarchy and see what falls out — same candidate universe, same
`beamtalk_workspace_meta:all_class_sources/0` source, same per-class
`beamtalk_compiler:diagnostics/3` round trip. Unlike `trigger_image/0`, this
runs automatically (see the repl_loader hook), not just on the explicit
`:recheck image` command — but the trigger event itself (a class gaining
its *first* subclass) is a genuinely rare, structural one, not "every
reload," so the unbounded-recompile-every-class cost `trigger_image/0`
already accepts as an explicit trade-off is reused here at a proportionally
rare cadence, not a routine one.

## Why this deliberately does NOT drop `error`-severity diagnostics

Every other relevance filter in this module
(`relevant_diagnostic/4`, `relevant_diagnostic_shape/3`,
`relevant_image_diagnostic/1`) drops `severity := <<"error">>` outright —
ADR 0105's own text: "a reload finding is advisory, never build-failing."
That rule was written before either `matchExhaustive:` (ADR 0106) or the
"has subclasses" restriction (BT-2854) existed, when no type-checker
diagnostic was ever `Error` severity, so it was a no-op in practice; it is
not a no-op here; a genuinely non-exhaustive `matchExhaustive:` or a
now-non-leaf `Type` pattern arm both compile to `Error`, and dropping them
would silently defeat the entire point of this trigger (surfacing exactly
the newly-broken proof ADR 0107 describes). This is the one deliberate
exception to that pattern in this module: **the *finding* stays advisory**
(it never blocks or re-fails the reload that already happened, matching
ADR 0105 §Severity's actual guarantee), even though the diagnostic it wraps
would be build-failing on a fresh compile.

## Recorded limitation

Scoped to whichever live class sources are recorded in
`beamtalk_workspace_meta` (session-only, matching every other ADR 0105
mechanism) — a `matchExhaustive:` site in a file nobody has loaded/edited
this session is invisible here, same limitation `trigger_image/0` already
has. A dedicated xref index keyed by "classes referenced in a `Type`
pattern/`matchExhaustive:` site" would make this precise and un-coupled
from `all_class_sources/0`'s scope; not built here (BT-2856 keeps to the
smallest change that closes the crash-with-no-warning gap) — a candidate
follow-up alongside BT-2798's own recorded xref extension.

Never raises: any internal failure degrades to an empty `result()`, exactly
like every other trigger in this module.
""".
-spec trigger_leaf_change([binary()]) -> result().
trigger_leaf_change(SuperclassBins) ->
    try
        do_trigger_leaf_change(SuperclassBins)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Leaf-change re-check orchestration failed (reload unaffected)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    superclasses => SuperclassBins,
                    domain => [beamtalk, runtime]
                }
            ),
            empty_result()
    end.

-doc """
ADR 0108 hot-reload re-check trigger (BT-2899): re-check every live class
recorded as a dependent of any alias name in `AliasNameBins` — a live
redefinition of `type Foo = ...` (or of any alias transitively reachable
through it, since `AliasNameBins` is expected to already be the redefined
alias plus every alias that references it — see
`beamtalk_repl_eval:handle_type_alias_definition/3`, this trigger's one
production caller) invalidates any annotation-resolution or exhaustiveness
proof computed against the old expansion.

## Why this is NOT `trigger_leaf_change/1`'s whole-image sweep

ADR 0107's `trigger_leaf_change/1` has no lookup key for its dependency kind
(no selector a `Type`-pattern/`matchExhaustive:` site "sends" names the
class it tests) and falls back to recompiling every live class's source.
Aliases are different: `beamtalk_alias_xref` is a real alias-name -> class
index, populated at every class-defining compile from the compiler port's
`referenced_aliases` response field (ADR 0108 Implementation: "the alias
name is a natural key"). `alias_change_candidates/1` queries it directly —
no whole-image sweep. The ordinary `apply_cap/2`/`recheck_caller_cap/0`
machinery `trigger/4`/`trigger_shape/2` already use applies here too, since
a hot stdlib-style alias (`RestartStrategy`, `JsonValue`) could plausibly
exceed the default cap (20) of dependents in a large project.

**Caveat on ADR 0108's "impossible after this lands" guarantee (AC5):** that
guarantee holds for every *checked* dependent, but a dependent dropped by
the cap (named in the result's `not_checked_owners`/`cap_note`, published on
the `'ReloadCheckCompleted'` event exactly like every other trigger's
cap-dropped candidates) is not re-verified this redefinition — it keeps
whatever finding state (stale or clean) it already had. This mirrors the
identical, already-accepted caveat on `trigger/4`/`trigger_shape/2`'s own
guarantees; it is not a gap specific to this trigger, and a workspace with
more than `recheck_caller_cap` live dependents of one alias is the same
"interim guard, not the final word" tradeoff ADR 0105's Alternatives section
already accepts for every other selector-fan-out trigger (see
`apply_cap/2`'s own doc).

## Re-check re-runs `resolve_type_annotation` from scratch (AC3)

Each candidate's re-check is the ordinary `beamtalk_compiler:diagnostics/3`
round trip (`recheck_owner/5`-shaped: read the live source, compile it
fresh). Because `class_hierarchy => true` now also threads the *ambient
session alias cache* (`beamtalk_compiler_server:get_aliases/0` — see
`do_diagnostics/5`'s doc), and that cache reflects the alias's *just-
committed* redefinition by the time this trigger runs
(`beamtalk_repl_eval:handle_type_alias_definition/3` calls
`beamtalk_compiler_server:register_aliases/1` before enqueuing this
trigger), the candidate's `::` annotations resolve through
`resolve_type_annotation` against the redefined alias table from a clean
slate — never a patched `InferredType`, exactly as ADR 0108's Implementation
section requires.

## Relevance filter is coarse, like `trigger_leaf_change/1`'s

`relevant_diagnostic_alias_change/1` is "any non-`error` `Dnu`/`Type`
diagnostic" — the same `trigger_image/0`/`trigger_leaf_change/1` shape,
for the same reason: an alias redefinition's fallout (a `matchExhaustive:`
newly non-exhaustive, an annotation that now names a different structural
type) has no reliable "mentions this alias name" signal in the diagnostic
message text the way a removed-selector `Dnu` does for `trigger/4`'s
`removal` classification — the message describes the *residual* structural
type, not the alias name that produced it.

Never raises: any internal failure degrades to an empty `result()`, exactly
like every other trigger in this module.
""".
-spec trigger_alias_change([binary()]) -> result().
trigger_alias_change(AliasNameBins) ->
    try
        do_trigger_alias_change(AliasNameBins)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Alias-change re-check orchestration failed (redefinition unaffected)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    aliases => AliasNameBins,
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
        checked_owners => [],
        not_checked_owners => [],
        not_verified_owners => []
    }.

-spec do_trigger_pending(
    binary(),
    binary(),
    instance | class,
    classification(),
    beamtalk_workspace_signature_store:signature()
) -> result().
do_trigger_pending(ClassNameBin, SelectorBin, Side, Classification, PendingSignature) ->
    %% Same existing-atom-only conversion `do_trigger/3` uses — by the time a
    %% pending edit reaches here it has already compiled successfully, so its
    %% class/selector atoms already exist.
    ClassAtom = binary_to_existing_atom(ClassNameBin, utf8),
    SelectorAtom = binary_to_existing_atom(SelectorBin, utf8),
    AmbientClasses = beamtalk_compiler_server:get_classes(),
    case maps:find(ClassAtom, AmbientClasses) of
        error ->
            %% No ambient meta at all for the changed class this session — no
            %% baseline to splice the pending signature into, and no
            %% `class_hierarchy` context for the checker to resolve candidate
            %% receivers against either. Degrade rather than guess.
            empty_result();
        {ok, AmbientMeta} ->
            PendingMeta = override_method_signature(
                AmbientMeta, Side, SelectorAtom, PendingSignature
            ),
            ok = beamtalk_compiler_server:register_class(ClassAtom, PendingMeta),
            try
                do_trigger(ClassNameBin, SelectorBin, Classification)
            after
                %% Restore the real (installed) generation regardless of
                %% whether do_trigger/3 succeeded or raised — the ambient
                %% cache is shared workspace-wide state and must never be
                %% left holding a hypothetical, never-installed signature.
                ok = beamtalk_compiler_server:register_class(ClassAtom, AmbientMeta)
            end
    end.

-doc """
Splice `PendingSignature` into `AmbientMeta`'s `method_info` (or
`class_method_info`) entry for `SelectorAtom`, preserving every other field
of both the class meta and (if `SelectorAtom` already has an entry) the
method entry itself — only `return_type`/`param_types` change. A selector
with no existing entry (a brand-new, never-installed method) gets a bare
entry with just the overridden type fields; it has no arity, so the
compiler-port's ETF parser (`parse_method_infos_from_map`,
`beamtalk-compiler-port/src/main.rs`) silently drops it rather than fail —
harmless here, since a brand-new method has no existing dependents for xref
to find anyway.
""".
-spec override_method_signature(
    map(), instance | class, atom(), beamtalk_workspace_signature_store:signature()
) -> map().
override_method_signature(
    AmbientMeta, Side, SelectorAtom, #{return_type := ReturnTypeBin, param_types := ParamTypeBins}
) ->
    InfoKey = method_info_key(Side),
    MethodInfoMap = maps:get(InfoKey, AmbientMeta, #{}),
    ExistingEntry = maps:get(SelectorAtom, MethodInfoMap, #{}),
    NewEntry = ExistingEntry#{
        return_type => meta_type_from_binary(ReturnTypeBin),
        param_types => [meta_type_from_binary(P) || P <- ParamTypeBins]
    },
    AmbientMeta#{InfoKey => MethodInfoMap#{SelectorAtom => NewEntry}}.

-spec method_info_key(instance | class) -> method_info | class_method_info.
method_info_key(instance) -> method_info;
method_info_key(class) -> class_method_info.

-doc """
Inverse of `beamtalk_workspace_signature_store`'s `meta_type_to_binary/1`:
`<<"Dynamic">>` is the un-annotated sentinel (`none` in `__beamtalk_meta/0`
terms); anything else round-trips through `binary_to_existing_atom/2` — the
type name is always either a builtin type atom or an already-loaded class
name, both already atoms by the time a method compiles clean against them.
Falls back to `none` on a name with no existing atom rather than mint one
from patch-source-derived text (same atom-table-safety reasoning as
`do_trigger/3`'s selector conversion).
""".
-spec meta_type_from_binary(binary()) -> atom().
meta_type_from_binary(<<"Dynamic">>) ->
    none;
meta_type_from_binary(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> none
    end.

-spec do_trigger_image() -> image_result().
do_trigger_image() ->
    Sources = beamtalk_workspace_meta:all_class_sources(),
    Outcomes = [
        {OwnerBin, recheck_image_class(OwnerBin, Source)}
     || {OwnerBin, Source} <- lists:keysort(1, maps:to_list(Sources))
    ],
    Findings = lists:flatmap(
        fun({_Owner, {_Status, OwnerFindings}}) -> OwnerFindings end, Outcomes
    ),
    Checked = length([ok || {_Owner, {ok, _}} <- Outcomes]),
    Stale = length(lists:usort([maps:get(owner, F) || F <- Findings])),
    #{findings => Findings, checked => Checked, stale => Stale}.

-doc """
Re-check one live class's own current source against the ambient class
hierarchy (mirrors `recheck_owner/5`'s compiler call, minus the xref
site-cap/attribution machinery a single changed selector needs). Returns
`{Status, Findings}` — `Status` is `ok` only when the diagnostics round-trip
completed, so `do_trigger_image/0` can count `checked` accurately; a
compiler-port failure (a `{error, Reason}' tuple, or the compiler port's
`gen_server:call/3' exiting on `noproc`/`timeout` — unlike the `{error,
Reason}' case, `beamtalk_compiler_server:diagnostics/3' does not itself
catch these) degrades this one class to `failed` (no findings for it)
rather than aborting the whole scan — a thousand-class image should not
lose its entire sweep because one class's diagnostics call outlives the
compiler port's 30s timeout.
""".
-spec recheck_image_class(binary(), string()) -> {ok | failed, [image_finding()]}.
recheck_image_class(OwnerBin, Source) ->
    SourceBin = unicode:characters_to_binary(Source),
    try beamtalk_compiler:diagnostics(SourceBin, <<"expression">>, #{class_hierarchy => true}) of
        {ok, Diagnostics} ->
            Findings = [
                image_finding(OwnerBin, D)
             || D <- Diagnostics, relevant_image_diagnostic(D)
            ],
            {ok, Findings};
        {error, Reason} ->
            ?LOG_WARNING(
                "Whole-image re-check compile failed for class (no findings reported)",
                #{owner => OwnerBin, reason => Reason, domain => [beamtalk, runtime]}
            ),
            {failed, []}
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Whole-image re-check compiler-port call failed for class (no findings reported)",
                #{
                    owner => OwnerBin,
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            ),
            {failed, []}
    end.

-doc """
Is `Diagnostic` relevant to a whole-image scan? Unlike `relevant_diagnostic/4`
there is no changed selector to match against, so this is simply "a
non-`error` `Dnu` or `Type` diagnostic" — the same two categories `trigger/4`
treats as reload-relevant, minus the selector-name/classification narrowing
that only makes sense relative to one specific change.
""".
-spec relevant_image_diagnostic(map()) -> boolean().
relevant_image_diagnostic(#{severity := <<"error">>}) ->
    false;
relevant_image_diagnostic(#{category := Category}) when
    Category =:= <<"Dnu">>; Category =:= <<"Type">>
->
    true;
relevant_image_diagnostic(_Diagnostic) ->
    false.

-spec image_finding(binary(), map()) -> image_finding().
image_finding(
    OwnerBin, #{message := Message, severity := Severity, start := Start, 'end' := End} = Diagnostic
) ->
    #{
        owner => OwnerBin,
        severity => Severity,
        category => maps:get(category, Diagnostic, undefined),
        message => Message,
        start => Start,
        'end' => End
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
    NotCheckedOwners = not_checked_owners(OwnerGroups, Kept),
    #{
        findings => Findings,
        checked => length(CheckedOwners),
        total_candidates => length(OwnerGroups),
        not_checked => NotChecked,
        cap_note => cap_note(NotChecked),
        checked_owners => CheckedOwners,
        not_checked_owners => NotCheckedOwners,
        not_verified_owners => not_verified_owners(NotCheckedOwners, Outcomes)
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
    NotCheckedOwners = not_checked_owners(OwnerGroups, Kept),
    #{
        findings => Findings,
        checked => length(CheckedOwners),
        total_candidates => length(OwnerGroups),
        not_checked => NotChecked,
        cap_note => cap_note(NotChecked),
        checked_owners => CheckedOwners,
        not_checked_owners => NotCheckedOwners,
        not_verified_owners => not_verified_owners(NotCheckedOwners, Outcomes)
    }.

-doc """
The candidates the caller cap dropped this trigger — `Candidates` (every
group `group_by_owner/1` found) minus `Kept` (`apply_cap/2`'s survivors),
named as owner binaries. `Candidates` arrives as `Kept`'s superset with
`Kept` as its prefix (`apply_cap/2`'s contract), so list subtraction is exact
and does not need a set datatype. See `result()`'s `not_checked_owners` doc
(BT-2802) for why this is exposed rather than just counted.
""".
-spec not_checked_owners([{atom(), [map()]}], [{atom(), [map()]}]) -> [binary()].
not_checked_owners(Candidates, Kept) ->
    [atom_to_binary(Owner, utf8) || {Owner, _} <- Candidates -- Kept].

-doc """
`NotCheckedOwners` (the cap-dropped candidates) unioned with every `Kept`
candidate whose `recheck_owner/5`/`recheck_owner_for_shape/4` outcome status
was not `ok` — i.e. `skipped` (no live source recorded) or `failed` (the
diagnostics round-trip errored or the compiler port call itself failed).
Both groups share the same defining property `result()`'s moduledoc
documents for `not_verified_owners` (BT-2828): a diagnostics round-trip
never completed for this owner this trigger, so any pre-existing finding for
it was neither replaced nor known-fixed — `beamtalk_repl_loader` must not
treat it as freshly verified. `NotCheckedOwners` and `Outcomes`' owners are
always disjoint (`Outcomes` only ever covers `Kept`, and `NotCheckedOwners`
is exactly `Candidates -- Kept`), so `lists:usort/1` here is for
deterministic ordering, not for deduplication across the two sources.
""".
-spec not_verified_owners([binary()], [{atom(), {ok | skipped | failed, [finding()]}}]) ->
    [binary()].
not_verified_owners(NotCheckedOwners, Outcomes) ->
    UnverifiedFromOutcomes = [
        atom_to_binary(Owner, utf8)
     || {Owner, {Status, _}} <- Outcomes, Status =/= ok
    ],
    lists:usort(NotCheckedOwners ++ UnverifiedFromOutcomes).

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
                        to_finding_shape(
                            OwnerBin, ClassNameBin, SiteRefs, D, Matched, AmbiguousWith
                        )
                     || D <- Diagnostics,
                        {true, Matched, AmbiguousWith} <- [
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
`field_change()` is it most specifically about — and, when the attribution
itself is ambiguous, which *other* changes could equally be the true cause?

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
  *first* `retyped` change in `FieldChanges` when one is present. **When a
  single reload retypes two or more slots** (BT-2780 adversarial review,
  BT-2805), there is no field-name signal in the `Dnu` message to
  disambiguate which retyped slot actually caused it (unlike the
  `Type`/removed-accessor branches above, which match on quoted text), so
  `retyped_fallback/1` cannot pin the finding to a single slot with
  confidence. Rather than silently naming the wrong field, the third
  element of the returned triple carries the *other* equally-plausible
  `retyped` changes, and `field_change_note/3` renders the ambiguity
  explicitly in the finding's `note` instead of asserting one slot as the
  cause. See
  `relevant_diagnostic_shape_retyped_fallback_notes_ambiguity_when_multiple_test/0`
  for the pinned behaviour.
- `error`-severity is always dropped, matching `relevant_diagnostic/4`.
""".
-spec relevant_diagnostic_shape(map(), binary(), [shape_field_change()]) ->
    {true, shape_field_change(), [shape_field_change()]} | false.
relevant_diagnostic_shape(#{severity := <<"error">>}, _ClassNameBin, _FieldChanges) ->
    false;
relevant_diagnostic_shape(
    #{category := <<"Type">>, message := Message}, ClassNameBin, FieldChanges
) ->
    case
        binary:match(Message, <<"state key">>) =/= nomatch andalso
            binary:match(Message, ClassNameBin) =/= nomatch
    of
        true -> unambiguous(match_field_change_by_name(Message, FieldChanges));
        false -> false
    end;
relevant_diagnostic_shape(
    #{category := <<"Dnu">>, message := Message}, _ClassNameBin, FieldChanges
) ->
    case match_removed_accessor(Message, FieldChanges) of
        {true, _} = Match -> unambiguous(Match);
        false -> retyped_fallback(FieldChanges)
    end;
relevant_diagnostic_shape(_Diagnostic, _ClassNameBin, _FieldChanges) ->
    false.

-spec unambiguous({true, shape_field_change()} | false) ->
    {true, shape_field_change(), [shape_field_change()]} | false.
unambiguous(false) -> false;
unambiguous({true, FC}) -> {true, FC, []}.

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

-doc """
Falls back to the *first* `retyped` change in `FieldChanges` when no
removed-accessor selector matched (see `relevant_diagnostic_shape/3`'s
moduledoc). When more than one `retyped` change is in flight, the other
candidates are returned alongside it so the caller can render the
attribution as ambiguous instead of asserting a single, possibly-wrong,
slot.
""".
-spec retyped_fallback([shape_field_change()]) ->
    {true, shape_field_change(), [shape_field_change()]} | false.
retyped_fallback(FieldChanges) ->
    case [FC || {retyped, _, _, _} = FC <- FieldChanges] of
        [] -> false;
        [First | Rest] -> {true, First, Rest}
    end.

-spec first_match([shape_field_change()]) -> {true, shape_field_change()} | false.
first_match([First | _]) -> {true, First};
first_match([]) -> false.

-spec backtick_quoted(binary()) -> binary().
backtick_quoted(NameBin) ->
    <<"`", NameBin/binary, "`">>.

-doc """
Build a shape-change finding, attributing it to `FieldChange` — `selector`
is `FieldChange`'s slot name (see `finding()`'s doc for why), and `note`
describes what happened to that slot in the reload. `AmbiguousWith` is the
non-empty list of other `retyped` changes that could equally have caused
this diagnostic (see `relevant_diagnostic_shape/3`'s moduledoc), or `[]`
when the attribution is exact.
""".
-spec to_finding_shape(
    binary(), binary(), [site_ref()], map(), shape_field_change(), [shape_field_change()]
) -> finding().
to_finding_shape(OwnerBin, ClassNameBin, SiteRefs, Diagnostic, FieldChange, AmbiguousWith) ->
    SelectorBin = beamtalk_shape_diff:field_name(FieldChange),
    (base_finding(OwnerBin, ClassNameBin, SelectorBin, shape_change, SiteRefs, Diagnostic))#{
        note => field_change_note(ClassNameBin, FieldChange, AmbiguousWith)
    }.

%% `AmbiguousWith` is only ever non-empty for `retyped` (see
%% `retyped_fallback/1` — the only producer of ambiguity); `added`/`removed`
%% always come from `unambiguous/1`, which pins it to `[]`. The `added`/
%% `removed` clauses match any `AmbiguousWith` value (not just `[]`) so an
%% unexpected non-empty list is silently ignored rather than crashing with
%% `function_clause` — those change kinds have no ambiguity to render either
%% way.
-spec field_change_note(binary(), shape_field_change(), [shape_field_change()]) -> binary().
field_change_note(ClassNameBin, {added, Name}, _AmbiguousWith) ->
    <<"state field `", Name/binary, "` added by the reload of ", ClassNameBin/binary>>;
field_change_note(ClassNameBin, {removed, Name}, _AmbiguousWith) ->
    <<"state field `", Name/binary, "` removed by the reload of ", ClassNameBin/binary>>;
field_change_note(ClassNameBin, {retyped, Name, OldType, NewType}, []) ->
    <<"state field `", Name/binary, "` retyped by the reload of ", ClassNameBin/binary, " (",
        OldType/binary, " -> ", NewType/binary, ")">>;
field_change_note(ClassNameBin, {retyped, Name, OldType, NewType}, AmbiguousWith) ->
    OtherNamesBin = retyped_names_bin(AmbiguousWith),
    <<"state field `", Name/binary, "` retyped by the reload of ", ClassNameBin/binary, " (",
        OldType/binary, " -> ", NewType/binary,
        ") — ambiguous: could also be caused by the retyping of ", OtherNamesBin/binary>>.

-spec retyped_names_bin([shape_field_change()]) -> binary().
retyped_names_bin(FieldChanges) ->
    Names = [backtick_quoted(beamtalk_shape_diff:field_name(FC)) || FC <- FieldChanges],
    iolist_to_binary(lists:join(<<", ">>, Names)).

%%====================================================================
%% Leaf-change re-check (ADR 0107 Phase A, BT-2856)
%%====================================================================

-doc """
Re-check every live class's own recorded source against the current
(already-updated, per `trigger_leaf_change/1`'s doc) hierarchy, keeping only
diagnostics attributable to one of `SuperclassBins` losing its leaf status
(`relevant_diagnostic_leaf_change/2`, applied per superclass in the batch —
see `recheck_owner_for_leaf_change/3`). Structurally mirrors
`do_trigger_image/0` (same source, same per-class round trip, run **once**
regardless of how many superclasses are in `SuperclassBins` — BT-2873) but
returns a `result()` — not an `image_result()` — since `checked_owners`/
`not_verified_owners` are what the publish path
(`beamtalk_repl_loader:publish_leaf_change_recheck_outcome/2`, called once
per superclass in the batch against this one shared `result()`) needs to
reuse the exact same findings-store/announcement plumbing
`publish_shape_recheck_outcome/3` already established. There is no
xref-derived candidate set to cap here (see `trigger_leaf_change/1`'s doc),
so `not_checked`/`not_checked_owners` are always empty/`[]` — every live
class with a recorded source is a candidate, unconditionally.
""".
-spec do_trigger_leaf_change([binary()]) -> result().
do_trigger_leaf_change(SuperclassBins) ->
    Sources = beamtalk_workspace_meta:all_class_sources(),
    Owners = lists:keysort(1, maps:to_list(Sources)),
    Outcomes = [
        {OwnerBin, recheck_owner_for_leaf_change(OwnerBin, Source, SuperclassBins)}
     || {OwnerBin, Source} <- Owners
    ],
    Findings = lists:flatmap(
        fun({_Owner, {_Status, OwnerFindings}}) -> OwnerFindings end, Outcomes
    ),
    CheckedOwners = [Owner || {Owner, {ok, _}} <- Outcomes],
    NotVerifiedOwners = lists:usort([
        Owner
     || {Owner, {Status, _}} <- Outcomes, Status =/= ok
    ]),
    #{
        findings => Findings,
        checked => length(CheckedOwners),
        total_candidates => length(Owners),
        not_checked => 0,
        cap_note => undefined,
        checked_owners => CheckedOwners,
        not_checked_owners => [],
        not_verified_owners => NotVerifiedOwners
    }.

-doc """
Re-check one live class's own current source for diagnostics attributable
to any superclass in `SuperclassBins` — mirrors `recheck_image_class/2`'s
compiler-port call (same `class_hierarchy => true` diagnostics round trip,
same `{ok | failed, ...}` degrade-on-failure contract) but filters/builds
findings via `relevant_diagnostic_leaf_change/2` / `to_finding_leaf_change/3`
instead of `image_finding/2`.

**BT-2873:** one `diagnostics/3` round trip covers every superclass in the
batch — a diagnostic that happens to name more than one batch member (not
observed in practice; every shipped diagnostic message names exactly one
class) contributes one finding per matching superclass, rather than picking
just the first match, since each is an independently real, independently
attributable finding.
""".
-spec recheck_owner_for_leaf_change(binary(), string(), [binary()]) -> {ok | failed, [finding()]}.
recheck_owner_for_leaf_change(OwnerBin, Source, SuperclassBins) ->
    SourceBin = unicode:characters_to_binary(Source),
    try beamtalk_compiler:diagnostics(SourceBin, <<"expression">>, #{class_hierarchy => true}) of
        {ok, Diagnostics} ->
            Findings = lists:flatmap(
                fun(D) -> findings_for_leaf_change_diagnostic(OwnerBin, SuperclassBins, D) end,
                Diagnostics
            ),
            {ok, Findings};
        {error, Reason} ->
            ?LOG_WARNING(
                "Leaf-change re-check compile failed for class (no findings reported)",
                #{owner => OwnerBin, reason => Reason, domain => [beamtalk, runtime]}
            ),
            {failed, []}
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Leaf-change re-check compiler-port call failed for class (no findings reported)",
                #{
                    owner => OwnerBin,
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            ),
            {failed, []}
    end.

-spec findings_for_leaf_change_diagnostic(binary(), [binary()], map()) -> [finding()].
findings_for_leaf_change_diagnostic(OwnerBin, SuperclassBins, Diagnostic) ->
    [
        to_finding_leaf_change(OwnerBin, SuperclassBin, Diagnostic)
     || SuperclassBin <- SuperclassBins,
        relevant_diagnostic_leaf_change(Diagnostic, SuperclassBin)
    ].

-doc """
Is `Diagnostic` attributable to `SuperclassBin` losing its leaf status? A
`Type`-category diagnostic naming `SuperclassBin` — the "has subclasses"
compile error (BT-2854) and a non-exhaustive/"cannot verify"
`matchExhaustive:` (BT-2856, both share `DiagnosticCategory::Type`) are the
only two diagnostic shapes this hierarchy change can newly introduce, and
both always name the class in their message text. Deliberately does **not**
drop `error`-severity (see `trigger_leaf_change/1`'s doc for why this is the
one deliberate exception to this module's usual rule).

Whole-identifier match, not bare substring (`class_name_mentioned/2`) — a
naive `binary:match/2` would wrongly attribute a diagnostic about an
unrelated `ShapeGroup`/`MyShape` class to a `Shape` leaf change. No
before/after diff is taken against the *same* class's diagnostics from the
prior generation the way `relevant_diagnostic_shape/3` conceptually could —
this trigger has no previous-generation baseline to diff against (unlike
`beamtalk_shape_diff`), so a *pre-existing*, unrelated `Type` diagnostic
that happens to name `SuperclassBin` (e.g. an already-broken `x ::
SuperclassBin` site with some other error) can still be misattributed as
newly caused by this reload — an accepted precision limit, not a
correctness bug: the finding still names a real, currently-true diagnostic
on that class, just not necessarily a *new* one.
""".
-spec relevant_diagnostic_leaf_change(map(), binary()) -> boolean().
relevant_diagnostic_leaf_change(#{category := <<"Type">>, message := Message}, SuperclassBin) ->
    class_name_mentioned(Message, SuperclassBin);
relevant_diagnostic_leaf_change(_Diagnostic, _SuperclassBin) ->
    false.

-doc """
Whether `Message` mentions `ClassNameBin` as a whole identifier — not merely
as a substring of a longer name — at any occurrence. Mirrors the boundary
check `InferredType::class_name_for_diagnostic` already uses on the Rust
side (`crates/beamtalk-core/.../types.rs`) for its `UndefinedObject` ->
`Nil` diagnostic rewrite: a byte immediately before/after the match is only
a boundary when it is not itself an identifier character (ASCII
letter/digit/underscore) — ASCII-only is sufficient here since Beamtalk
class names are ASCII identifiers (matching `beamtalk_shape_diff`'s own
ASCII-byte-wise assumption for slot names).
""".
-spec class_name_mentioned(binary(), binary()) -> boolean().
class_name_mentioned(Message, ClassNameBin) ->
    Len = byte_size(ClassNameBin),
    lists:any(
        fun({Start, _MatchLen}) ->
            not is_ident_byte_at(Message, Start - 1) andalso
                not is_ident_byte_at(Message, Start + Len)
        end,
        binary:matches(Message, ClassNameBin)
    ).

-spec is_ident_byte_at(binary(), integer()) -> boolean().
is_ident_byte_at(_Message, Pos) when Pos < 0 ->
    false;
is_ident_byte_at(Message, Pos) when Pos >= byte_size(Message) ->
    false;
is_ident_byte_at(Message, Pos) ->
    <<_:Pos/binary, Byte, _/binary>> = Message,
    is_ident_byte(Byte).

-spec is_ident_byte(byte()) -> boolean().
is_ident_byte(B) when B >= $a, B =< $z -> true;
is_ident_byte(B) when B >= $A, B =< $Z -> true;
is_ident_byte(B) when B >= $0, B =< $9 -> true;
is_ident_byte($_) -> true;
is_ident_byte(_) -> false.

-doc """
Build a leaf-change finding. `changed_class`/`selector` both name
`SuperclassBin` — there is no single call-site selector this is "about"
(the same shape-change precedent `finding()`'s own doc already
establishes for `shape_change`'s `selector` field), and `sites` is always
`[]` — there is no xref call-site to translate against (see
`trigger_leaf_change/1`'s doc for why no xref index exists for this
classification).
""".
-spec to_finding_leaf_change(binary(), binary(), map()) -> finding().
to_finding_leaf_change(
    OwnerBin,
    SuperclassBin,
    #{message := Message, severity := Severity, start := Start, 'end' := End} =
        Diagnostic
) ->
    #{
        owner => OwnerBin,
        changed_class => SuperclassBin,
        selector => SuperclassBin,
        classification => leaf_change,
        severity => Severity,
        category => maps:get(category, Diagnostic, undefined),
        message => Message,
        note =>
            <<SuperclassBin/binary,
                " gained a new subclass live, invalidating a leaf-only Type-pattern/matchExhaustive: proof (ADR 0107)">>,
        sites => [],
        start => Start,
        'end' => End
    }.

%%====================================================================
%% Alias-change re-check (ADR 0108 hot-reload re-check trigger, BT-2899)
%%====================================================================

-doc """
The candidate owner classes for a live redefinition of any alias in
`AliasNameBins` — the union of `beamtalk_alias_xref:dependents_of/1` over
every name, deduplicated and sorted for deterministic ordering (mirrors
`group_by_owner/1`'s ordering guarantee for `apply_cap/2`'s benefit — see
that function's doc for what the ordering means for which candidates get
dropped when the cap bites).
""".
-spec alias_change_candidates([binary()]) -> [binary()].
alias_change_candidates(AliasNameBins) ->
    lists:usort(lists:flatmap(fun beamtalk_alias_xref:dependents_of/1, AliasNameBins)).

-spec do_trigger_alias_change([binary()]) -> result().
do_trigger_alias_change(AliasNameBins) ->
    Candidates = alias_change_candidates(AliasNameBins),
    Cap = recheck_caller_cap(),
    {Kept, NotChecked} = apply_cap(Candidates, Cap),
    Outcomes = [
        {OwnerBin, recheck_owner_for_alias_change(OwnerBin, AliasNameBins)}
     || OwnerBin <- Kept
    ],
    Findings = lists:flatmap(
        fun({_Owner, {_Status, OwnerFindings}}) -> OwnerFindings end, Outcomes
    ),
    CheckedOwners = [OwnerBin || {OwnerBin, {ok, _}} <- Outcomes],
    NotCheckedOwners = Candidates -- Kept,
    #{
        findings => Findings,
        checked => length(CheckedOwners),
        total_candidates => length(Candidates),
        not_checked => NotChecked,
        cap_note => cap_note(NotChecked),
        checked_owners => CheckedOwners,
        not_checked_owners => NotCheckedOwners,
        not_verified_owners => not_verified_owners_alias_change(NotCheckedOwners, Outcomes)
    }.

-doc """
Same "not verified" widening `not_verified_owners/2` does for `trigger/4`/
`trigger_shape/2` — `NotCheckedOwners` (cap-dropped) unioned with every
`Kept` candidate whose `recheck_owner_for_alias_change/2` outcome status
wasn't `ok`. Kept as its own small function (rather than reusing
`not_verified_owners/2` directly) because this trigger's `Outcomes`/
`NotCheckedOwners` are already plain binaries, not the `atom()`-keyed shape
`not_verified_owners/2`'s spec expects (this trigger has no selector to
convert candidate owners to atoms for — `beamtalk_alias_xref` stores class
names as binaries throughout).
""".
-spec not_verified_owners_alias_change([binary()], [
    {binary(), {ok | skipped | failed, [finding()]}}
]) ->
    [binary()].
not_verified_owners_alias_change(NotCheckedOwners, Outcomes) ->
    UnverifiedFromOutcomes = [OwnerBin || {OwnerBin, {Status, _}} <- Outcomes, Status =/= ok],
    lists:usort(NotCheckedOwners ++ UnverifiedFromOutcomes).

-doc """
Re-check one candidate caller class for diagnostics attributable to a
redefinition of any alias in `AliasNameBins` — mirrors `recheck_owner/5`'s
live-source read and `class_hierarchy => true` diagnostics round trip (see
`trigger_alias_change/1`'s doc for why that round trip now also resolves
against the *current* ambient alias cache), but filters via
`relevant_diagnostic_alias_change/1` instead of `relevant_diagnostic/4`.
""".
-spec recheck_owner_for_alias_change(binary(), [binary()]) -> {ok | skipped | failed, [finding()]}.
recheck_owner_for_alias_change(OwnerBin, AliasNameBins) ->
    case beamtalk_workspace_meta:get_class_source(OwnerBin) of
        undefined ->
            ?LOG_WARNING(
                "Alias-change re-check skipped: no live source recorded for candidate caller",
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
                    %% AliasNameBins is 1 name for the overwhelmingly common
                    %% single-alias redefinition; a rename-affecting-a-chain
                    %% caller can pass more, but the *finding* is always
                    %% attributed to the first (primary) name — the exact
                    %% redefined alias, not a transitively-affected one it
                    %% happens to also reference — mirroring how a
                    %% `signature_change` finding names the one changed
                    %% selector, not every selector a caller's class exposes.
                    [PrimaryAliasBin | _] = AliasNameBins,
                    Findings = [
                        to_finding_alias_change(OwnerBin, PrimaryAliasBin, D)
                     || D <- Diagnostics, relevant_diagnostic_alias_change(D)
                    ],
                    {ok, Findings};
                {error, Reason} ->
                    ?LOG_WARNING(
                        "Alias-change re-check compile failed for candidate caller (no findings reported)",
                        #{owner => OwnerBin, reason => Reason, domain => [beamtalk, runtime]}
                    ),
                    {failed, []}
            end
    end.

-doc """
Is `Diagnostic` relevant to an alias-change scan? Coarse, like
`relevant_image_diagnostic/1`/`relevant_diagnostic_leaf_change/2` — see
`trigger_alias_change/1`'s doc for why a message-text alias-name match
(`trigger/4`'s `removal` classification's approach) isn't reliable here:
a `matchExhaustive:` diagnostic names the *residual* structural type, not
the alias. `error`-severity is deliberately **not** dropped — mirrors
`relevant_diagnostic_leaf_change/2`'s exception to this module's usual rule:
a genuinely non-exhaustive `matchExhaustive:` over a redefined alias compiles
to `Error`, and dropping it would silently defeat the entire point of this
trigger (ADR 0108: "a live image holding a `matchExhaustive:` proof against
a stale alias member set is impossible after this lands").
""".
-spec relevant_diagnostic_alias_change(map()) -> boolean().
relevant_diagnostic_alias_change(#{category := Category}) when
    Category =:= <<"Dnu">>; Category =:= <<"Type">>
->
    true;
relevant_diagnostic_alias_change(_Diagnostic) ->
    false.

-doc """
Build an alias-change finding. `changed_class`/`selector` both name
`AliasNameBin` — there is no single call-site selector this is "about" (the
same shape/leaf-change precedent), and `sites` is always `[]` — the
`beamtalk_alias_xref` lookup that found this candidate is class-level, not
per-site (see that module's moduledoc for why: `AnalysisResult::
referenced_aliases` is a flat per-compile set, matching every other ADR
0105/0108 trigger's "re-check unit is the caller's whole class" granularity).
""".
-spec to_finding_alias_change(binary(), binary(), map()) -> finding().
to_finding_alias_change(
    OwnerBin,
    AliasNameBin,
    #{message := Message, severity := Severity, start := Start, 'end' := End} = Diagnostic
) ->
    #{
        owner => OwnerBin,
        changed_class => AliasNameBin,
        selector => AliasNameBin,
        classification => alias_change,
        severity => Severity,
        category => maps:get(category, Diagnostic, undefined),
        message => Message,
        note =>
            <<"type alias `", AliasNameBin/binary,
                "` was redefined live, invalidating an annotation-resolution or matchExhaustive: proof against its previous expansion (ADR 0108)">>,
        sites => [],
        start => Start,
        'end' => End
    }.
