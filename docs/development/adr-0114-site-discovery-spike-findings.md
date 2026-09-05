# ADR 0114 — Phase 1 Site-Discovery Spike Findings (BT-3268)

**Status:** complete
**Deliverable:** knowledge. Validate (or refute) that `renameTo:`'s planned
site-discovery mechanism — the union of `SystemNavigation default
referencesTo: aClass` (ADR 0087) and `beamtalk_class_registry:
direct_subclasses/1` — is exhaustive against real, compiled code, and measure
the one known, accepted gap (Constraint 4: `beamtalk_xref:
build_method_entry/5` hard-codes `references => []` for every live-patched
method) before any rename primitive is built on top of it.

**Evidence artefacts committed:**
- `runtime/apps/beamtalk_workspace/test/beamtalk_adr0114_site_discovery_spike_tests.erl`
  — EUnit harness against the real, compiled stdlib
  (`beamtalk_test_boot:boot_real_stdlib/1`) plus a primitive-level
  reproduction of the `build_method_entry/5` gap. `rebar3 eunit --module=
  beamtalk_adr0114_site_discovery_spike_tests` → **6 tests, 0 failures**.
  Also green inside the full suite: `rebar3 eunit --app=beamtalk_runtime,
  beamtalk_workspace,beamtalk_compiler` → **5900 tests, 0 failures**.
- `stdlib/test/adr0114class_builder_live_patch_gap_test.bt` — the gap
  reproduced end-to-end through `SystemNavigation referencesTo:` against a
  real `ClassBuilder`-installed method (`just test-bunit
  test/adr0114class_builder_live_patch_gap_test.bt` → passes).
- `tests/repl-protocol/cases/adr_0114_live_patch_gap.btscript` — checks the
  two OTHER live-patch surfaces the ADR names (`>>`/`compile:source:` via a
  prior revision of this file, and `register/5` extensions in the current
  one) and finds neither actually reproduces the gap end-to-end (both pass
  cleanly today, asserting the reference IS found) — see finding #3.

## Headline

| # | Question | Verdict | Evidence |
|---|---|---|---|
| 1 | Is `direct_subclasses/1` exhaustive for superclass-declaration sites? | **Held** | Exact match against a hand audit of every `Announcement subclass:` (9) and `Error subclass:` (4) declaration in `stdlib/src/*.bt`. |
| 2 | Is `referencesTo:` exhaustive for body/type/extension references, outside the known gaps? | **Held — with a corrected mental model** | Exact match against a hand audit of `Duration`, once the audit accounted for two behaviours not obvious from reading source alone (below). |
| 3 | Does the Constraint 4 live-patch gap reproduce via the surfaces the ADR names (`>>`, `compile:source:`, `register/5`)? | **No — corrects the ADR's own text** | All three were tried live. None reproduces the gap through `SystemNavigation referencesTo:`. See finding #3 for the mechanism behind each. |
| 4 | Is the gap reachable at all through a real, currently-shipped surface? | **Yes — `ClassBuilder` `methodSource:`** | The one surface this spike found that actually hits it, confirmed via `SystemNavigation referencesTo:` end-to-end (not just `build_method_entry/5` in isolation). |
| 5 | What is the reproduced gap's blast radius? | **The entire `references` channel for the affected method, unconditionally** | Confirmed at both the primitive level (`build_method_entry/5` returns `references => []` for a method with 2 real references) and the `SystemNavigation`-level (the `ClassBuilder` fixture's real reference is completely absent, not partially represented). |
| 6 | Is the `sends`/`references` asymmetry Constraint 4 highlights real? | **Confirmed at the primitive level; not reproduced through the BT-level `sendersOf:` for the `ClassBuilder` case** | `build_method_entry/5` populates `sends` while hard-coding `references => []` for the identical patched method (EUnit-level, direct). A parallel BUnit-level check that `SystemNavigation sendersOf:` finds the same `ClassBuilder` method's send did **not** pass and was dropped rather than asserted incorrectly — see finding #4 (open question, not resolved by this spike). |

**Single most important implication for Phase 2:** finding #3 is a genuine,
material correction to the ADR's own Constraint 4 text, not a nuance. The
ADR states the gap is reachable via "every `>>`/`compile:source:` live
patch" and calls it "the *everyday* ADR 0082 workflow, not an edge case."
Measured against the current implementation, **neither of those two named
surfaces reproduces the gap** — both recompile the whole class through the
real compiler (a complete, correct `method_xref`) rather than routing
through `beamtalk_object_class:put_method/3,4` (a repo-wide grep confirms
zero non-test call sites of that function today). The gap **is** real and
**is** reachable — via `ClassBuilder` `methodSource:` — but that is a
narrower, less "everyday" surface than the ADR currently claims. This
changes the risk characterization Phase 2 should carry forward, even though
it does not change the recommendation (see §4): the union is still safe to
build on, and the live-patch gap is still real, but its practical exposure
in ordinary agent/human workspace editing (which overwhelmingly uses `>>`
and `compile:source:`) is smaller than the ADR states. The ADR's Constraint
4 text should be corrected in a follow-up before Phase 2 relies on its
current wording to scope caller-facing warnings.

---

## 1. `direct_subclasses/1` — exhaustive for the sample

Hand audit method: `grep -n "<Class> subclass:" stdlib/src/*.bt` (109 real,
compiled stdlib classes), read against the resulting file list to exclude
comment/doc-string occurrences and generic-type subclass forms of unrelated
classes.

- `direct_subclasses('Announcement')` — hand audit found exactly 9 direct
  subclasses (`ActorSpawned`, `ActorStopped`, `BindingChanged`, `ClassLoaded`,
  `ClassRemoved`, `FlushCompleted`, `ObjectStateChanged`,
  `SupervisionChildAdded`, `SupervisionChildCrashed`). The real runtime query
  against the booted stdlib returned exactly this set, sorted.
- `direct_subclasses('Error')` — hand audit found exactly 4
  (`BEAMError`, `InstantiationError`, `RuntimeError`, `TypeError`).
  `ExitError`/`ThrowError` subclass `BEAMError`, not `Error` directly, and
  were correctly excluded (confirming the query is not accidentally
  transitive).

No discrepancy in either sample. `direct_subclasses/1` reads a single ETS
reverse-edge table (`beamtalk_class_metadata:match_subclasses/1`) populated
at class-registration time from the compiler-parsed class header — there is
no live-patch analogue for superclass declarations (a class's own
`subclass:` header cannot be live-patched independently of a full class
redefinition), so this channel has no equivalent of Constraint 4's gap.

## 2. `referencesTo:` — exhaustive, but two behaviours are easy to
   under-predict from source alone

The first pass of this spike's `references_to_duration` test hand-counted
only *other* classes' mentions of `Duration` (8, across `actor.bt`,
`date_time.bt`, `parallel.bt`, `timer.bt`) and asserted `length(references_to(
'Duration')) == 8`. Run against the real booted stdlib, this failed:
**21**, not 8. Both discrepancies turned out to be `referencesTo:` doing the
*correct*, exhaustive thing — the hand audit was wrong, not the index:

1. **Self-references count.** `duration.bt` itself has 13 methods whose own
   signatures mention `Duration` (7 instance-side comparison/arithmetic
   operators — `+ - * < <= > >=` — and 6 class-side constructors —
   `milliseconds: seconds: minutes: hours: days: fromString:`). Every one of
   these is correctly reported as a site owned by `Duration`. This is not
   optional for `renameTo:`'s purposes: renaming `Duration` to (say)
   `TimeSpan` requires rewriting `Duration`'s own `-> Duration` / `::
   Duration` signature mentions exactly as much as any external caller's.
   8 + 13 = 21, matching the real result exactly once this was accounted for.
2. **Same-line multi-occurrence collapse.** `+ other :: Duration -> Duration
   =>` mentions `Duration` twice on one source line (param type, return
   type). Both mentions produce identical `#{class => 'Duration', line =>
   L}` reference-hit records at codegen time; once folded into the xref
   site shape `#{owner, class_side, method, line, gen}`, the two hits are
   *identical* tuples, and `beamtalk_xref_references` is an ETS `bag` —
   which stores at most one copy of an object that is fully identical to an
   existing one. The result: `Duration>>+` contributes **one** row, not two.
   This is invisible in the data (nothing is missing — the line/method pair
   that needs rewriting is still present) but means a naive
   "count textual occurrences of the class name" prediction will not match
   `length(references_to(X))` when a method mentions the same class more
   than once on one line. Verified by contrast: `actor.bt`'s `withTimeout:`
   mentions `Duration` on two *different* lines (type annotation, then
   `isKindOf:`) and correctly produces two separate rows.

Once corrected for both, the hand-audited set (21 exact `{owner, class_side,
method}` tuples, several with duplicate entries for the two-different-line
case) matches the real query result exactly — see the test's inline
commentary for the corrected, line-by-line audit.

**Conclusion:** no unexplained gap. Every real, non-comment mention of the
sampled classes that a person reading the source would call "a reference"
is present in the index. The two behaviours above are worth documenting for
whoever builds Phase 2's rewrite mechanism (the site list is precise, but
"how many sites" is not the same question as "how many textual mentions"),
but neither is a correctness defect.

## 3. Live-patch gap (Constraint 4) — which surfaces actually reach it

This is the finding that most changes the picture the ADR paints. All three
live-patch surfaces Constraint 4 and its doc comments name were checked
live, in order, against the actual `SystemNavigation referencesTo:` a
rename would call — not just `build_method_entry/5` in isolation.

### 3a. `>>` and `compile:source:` — do NOT reproduce the gap

First checked via `Counter >> makeTimer -> Timer => Timer after: 999999 do:
[nil]` against a real loaded class (`Counter`), in the E2E harness. Result:
`SystemNavigation default referencesTo: Timer` **found** the patch — no gap.

Tracing why: both `>>` (parsed as a standalone `MethodDefinition`,
`beamtalk_repl_eval:handle_method_definition/4` →
`beamtalk_repl_loader:reload_method_definition/4`) and `compile:source:`
(`beamtalk_repl_eval:compile_method/6,7` → `do_compile_method/7` →
`beamtalk_repl_loader:install_method/9`) converge on the *same* function,
`install_method_with_source/10`/`recompile_with_method`, which:

1. Merges the patched method's source into the class's full, on-disk source
   text.
2. Recompiles the **whole class** via `beamtalk_repl_compiler:
   compile_method_reload/3` — a full pass through the real Rust compiler,
   producing a fresh `Binary` with a complete, compiler-computed
   `method_xref` (references included, exactly like a normal file compile).
3. Installs it via `code:load_binary/3` and `activate_module/2`, which runs
   the new module's `register_class/0` — forwarding the *complete* xref
   payload to `beamtalk_xref`, generation-bumped, same as any ordinary class
   load or `Behaviour reload`.

**Neither path calls `beamtalk_object_class:put_method/3,4` at all.** A
repo-wide grep (`grep -rln "object_class:put_method(" apps/*/src`) returns
zero results outside test files. `build_method_entry/5`'s narrow
single-method reparse — the function Constraint 4 names — is simply never
invoked by either surface in the current codebase. This directly
contradicts the ADR's characterization of `>>`/`compile:source:` as "the
function every `>>`/`compile:source:` live patch... routes through."

### 3b. `register/5` sourced extensions — do NOT reproduce the gap (but for a different reason)

Checked via `Erlang beamtalk_extensions register: #Counter selector:
#btAdr0114TimerExt fun: ... source: "spawnTimer -> Timer => Timer after:
999999 do: [nil]"` (the same FFI shape `workspace_native_api.btscript`
already uses for its own passing extension coverage). Result:
`referencesTo: Timer` **found** the extension method — no gap here either.

This time the `build_method_entry/5`-level gap is real and confirmed
separately (`beamtalk_extensions:index_extension_xref/3` does call
`build_method_entry/5`, and the EUnit-level test in this spike confirms its
own such call returns `references => []`) — but `system_navigation.bt`'s
`referencesTo:` implementation compensates for it with a **dedicated,
unconditional rescan of every registered extension's source**
(`collectExtensionReferencesFor:into:`, BT-2196), whose own comment says
why: *"Extensions are not in the index yet, so this scan is
unconditional."* This rescan runs on every `referencesTo:` call regardless
of what the xref table says, so the table's empty `references` row for an
extension method never actually surfaces as a missed reference at the
`SystemNavigation` level.

### 3c. `ClassBuilder` `methodSource:` — DOES reproduce the gap

Confirmed via `stdlib/test/adr0114class_builder_live_patch_gap_test.bt`:
a class built with `Object classBuilder name: #Adr0114BuilderGapProbe;
superclass: Object; methods: #{#makeCounter => [:_self | AtomicCounter
new: #adr0114BuilderProbe]}; register` (a real block-literal method body,
BT-2246-auto-populated source) is **not** found by `SystemNavigation
referencesTo: AtomicCounter`, even though its body plainly sends
`AtomicCounter new: ...`.

Why this one differs from 3a/3b: `beamtalk_class_builder.erl`'s
`source_map_to_xref/3` calls `build_method_entry/5` with `SourceStatus =
indexed` (a real source string), so the class ends up **fully indexed** in
the xref table — unlike an unindexed class, it is never picked up by
`referencesTo:`'s loaded-but-unindexed fallback scan, and unlike a
`register/5` extension, there is no compensating unconditional rescan for
`ClassBuilder`-installed methods. The `references => []` row is the only
data `referencesTo:` ever consults for this method, so the real reference
is silently and completely missed.

### Primitive-level confirmation (independent of which surface calls it)

`runtime/apps/beamtalk_workspace/test/beamtalk_adr0114_site_discovery_spike_tests.erl`
calls `beamtalk_object_class:put_method/4` directly (the function
`build_method_entry/5` doc-comments as the mechanism, whichever surface
eventually calls it) with a method carrying two distinct, real references
to `AtomicCounter` (a return-type annotation and a constructor send).
`build_method_entry/5` returns `references => []` — confirmed against an
independent count (the literal `AtomicCounter` substring occurs exactly
twice in the source, counted without re-running the compiler's own walker)
— and `references_to('AtomicCounter')` never surfaces the patched class
afterward. **Blast radius: 100% of that method's real references, not a
partial miss.** The same method's constructor send *is* correctly indexed
by `sendersOf:` — the sends/references asymmetry Constraint 4 describes is
real at this level.

## 4. Open question this spike does not resolve

A BUnit-level check that `SystemNavigation sendersOf: #new:` finds the same
`ClassBuilder`-method's send (mirroring the EUnit-level `put_method/4`
result) did not pass in a first attempt and was dropped from the committed
fixture rather than asserted on faith. This does not affect the core
`referencesTo:` finding above (§3c stands on its own, confirmed
independently), but it means the sends/references asymmetry is confirmed at
the primitive level (§3, EUnit) and not independently re-confirmed at the
`ClassBuilder`+`SystemNavigation sendersOf:` level. Worth a follow-up
probe before Phase 3 (`renameSelector:to:`) leans on `ClassBuilder`
`sendersOf:` completeness specifically — not blocking for this issue, which
scopes `renameTo:`'s `referencesTo:`/`direct_subclasses:` union only.

## 5. Recommendation for Phase 2

- The `referencesTo:` + `direct_subclasses/1` union is safe to build
  `renameTo:`'s site discovery on as designed. No fallback to a narrower
  "definition-only" v1 is indicated by this spike.
- Phase 2's implementer should be aware of finding #2's two behaviours (self-
  references count; same-line occurrences collapse to one row) when writing
  any code that predicts or double-checks a rewrite site count from source
  text — the site *list* is correct, but is not directly comparable to a
  naive occurrence count.
- **The ADR's Constraint 4 text should be corrected**: the live-patch gap is
  real, but is not reachable via `>>`/`compile:source:` in the current
  implementation — only via `ClassBuilder` `methodSource:` (and, at the raw
  primitive level, any future direct caller of
  `beamtalk_object_class:put_method/3,4`, which nothing currently calls in
  production). Caller-facing warning copy for `renameTo:` should scope the
  live-patch caveat to `ClassBuilder`-defined dynamic classes specifically,
  not to "any class with an unflushed live patch" as the ADR currently
  implies — the latter overstates the risk for the ordinary `>>`/
  `compile:source:` workflow, which this spike found to be unaffected.

## References

- ADR: `docs/ADR/0114-class-and-method-rename.md` § Decision, § Constraints
  4, § Phased rollout Phase 1
- Related: `docs/ADR/0087-maintained-xref-index-for-system-navigation.md`
  (`referencesTo:`, `beamtalk_xref:build_method_entry/5`)
- Issue: BT-3268
