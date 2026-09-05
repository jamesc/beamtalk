# ADR 0114: Class and Method Rename in the Live Workspace

## Status
Accepted (2026-08-25)

## Context

### Problem

ADR 0113 (Destructive Workspace Operations — File Deletion in Flush) ships the `confirmDestructive` flush tier, the two-phase-atomicity extension, and the undo model for the two *deletion* flows BT-2192 originally scoped. It explicitly left rename for a follow-up:

> This ADR originally also designed `renameTo:`/`renameSelector:to:` (class/method rename). During review, rename turned out to carry real, unresolved design risk unrelated to deletion... Rather than hold the small, safe half hostage to the harder open questions, rename was split out to BT-3204 as its own follow-up ADR.

This ADR is that follow-up. No primitive exists for either class rename or method rename today — unlike removal, rename has no existing partial implementation to extend from, and (unlike deletion) its correctness depends on infrastructure this ADR has to reason about carefully rather than reuse verbatim.

### Current State

| Concern | Today |
|---|---|
| Class rename | **Does not exist.** No primitive, no ChangeLog kind, no flush behaviour. |
| Method rename | **Does not exist.** No primitive, no ChangeLog kind, no flush behaviour. |
| `confirmDestructive` flush tier | Ships in ADR 0113, gating `remove-class`. This ADR reuses it for `rename-class`/`rename-method`, not redesigns it. |
| Two-phase flush protocol (Phase A validate/stage, Phase B commit) | Ships in ADR 0082, extended by ADR 0113 for single-file delete. This ADR extends it further for genuinely multi-file staging. |
| `SystemNavigation>>referencesTo:` | Already shipped (ADR 0087, BT-2302) — a maintained "class → referencing sites" xref index, sub-millisecond. This ADR's `renameTo:` reuses it directly rather than inventing a new query. |
| `SystemNavigation>>sendersOf:` | Already shipped (ADR 0087) — a maintained "selector → sending sites" xref index. This ADR's `renameSelector:to:` reuses it, with an important caveat (see *Decision*). |
| `beamtalk_class_registry:direct_subclasses/1` | Already exists, used by `removeFromSystem`'s subclass-refusal check (BT-785). This ADR's `renameTo:` reuses it for the one reference kind `referencesTo:` doesn't cover. |

### Constraints

1. **ADR 0032 chain-walk dispatch** makes method *removal* correct by construction (deleting a map entry is instantaneously and correctly visible — the inherited implementation reappears, ADR 0112). Rename does not get this for free: a compiled call site's selector is baked into its own bytecode. Renaming `#increment` to `#incrementBy:` does not change what any *existing* compiled sender sends; those sends keep going to the old, now-vacated selector and become live `does_not_understand` failures the moment the definition moves, unless senders are rewritten too. **Class rename has the identical problem, at the class-name level:** a class reference (`Counter new`, `Counter class`, a `:: Counter` type annotation, a `subclass: Counter` superclass reference, an extension declaration in another file) compiles to a runtime lookup by name atom (`beamtalk_class_registry:whereis_class/1`), and that atom is exactly as baked into the referencing call site's bytecode as a method selector is. Re-registering a class under a new name without rewriting those references produces the same class of silent, delayed `class_not_found`/`does_not_understand` failure — this ADR treats the two as one mechanism applied at two granularities, not two independent problems (see *Decision*).
2. **One-class-per-file convention** (ADR 0082's `newClass:at:` validation: declared class name must match the file's basename). A class rename that changes the name but leaves the file path alone violates this convention the instant it's flushed — so class rename is not just a ChangeLog-schema question, it is inherently also a *file move* question.
3. **`whereis_class/1` is a live per-call name lookup — but ordinary instance method dispatch never goes through it at all.** An actor's own state carries `__class_mod__` — set once at spawn (`beamtalk_actor.erl`'s `make_self/1`: `class_mod = maps:get('__class_mod__', State, undefined)`) — and every self-send resolves through it directly: `self_dispatch/2` reads `__class_mod__` out of `'$bt_actor_state'` and calls `ClassMod:safe_dispatch(Selector, Args, State)`; a cross-process send lands in the generated `handle_call`, which dispatches through the identical `safe_dispatch/3` generated for that class module. No step in either path calls `beamtalk_class_registry:whereis_class/1` or otherwise looks the class up by its registered *name* — `class_mod` is bound once, at spawn time, and is a different thing from the `class` name `renameTo:` re-registers in the class registry. (`local_call/3` and `ClassPid = erlang:element(4, Self)` elsewhere in this codebase are *not* evidence of how ordinary instances dispatch — `local_call/3` is BT-1664's class-*method* fast path, guarded by `beamtalk_class_registry:is_class_object/1`, and only applies when `Self` *is* the class object itself; it raises a type error for an ordinary instance receiver.) This matters for correctness scoping: renaming a class does not put *existing instances* of that class at risk — their own methods dispatch via the `class_mod` binding fixed at spawn, not a name lookup — only *other* code's compiled references to the old *name* are at risk.
4. **The xref index's `references` channel is unconditionally empty for live-patched methods.** `beamtalk_xref:build_method_entry/5` — the function every `>>`/`compile:source:` live patch and every sourced extension registration routes through — hard-codes `references => []`, with its own doc comment explaining why: "there is no runtime 'all references' walker... the class-reference channel is fully populated only at compile time." This means `referencesTo:`-based site discovery silently misses references inside methods that have been live-patched since last full compile — the *everyday* ADR 0082 workflow, not an edge case. By contrast, the *sends* channel `sendersOf:` relies on **is** computed for live patches (via `sends_from_source/1`, except for sourceless `unindexed_runtime_fun` entries), so `renameSelector:to:`'s completeness is measurably better than `renameTo:`'s for classes with pending live patches.
5. **`beamtalk_xref_senders` is keyed by selector name only, with no receiver-type narrowing** (`Selector -> Sites`, confirmed in ADR 0087's schema; the `Site` record's `recv_kind` field distinguishes only `self_recv | super_recv | erlang_ffi | other` — a syntactic category, not an inferred type). `sendersOf: #at:put:` returns *every* textual send of that selector anywhere in the project, regardless of which class's implementation the sender actually meant to call. This is a fundamentally different risk from ADR 0112's accepted "dangling sender" gap (a *missed* reference): auto-rewriting every result blindly is a **false-positive** risk — it would rewrite unrelated, correctly-working code that happens to share a selector name. This constraint is the crux of this ADR's `renameSelector:to:` design (see *Decision*).
6. **ADR 0082 Amendment 1** (human/agent, git-first/ChangeLog-first split) and **ADR 0113's `confirmDestructive` tiering** apply here unmodified — this ADR is a second consumer of both, not a redesign.
7. **Reproducible-build guarantee** and **surface parity** apply exactly as ADR 0082/0112/0113 already state.

## Decision

**Add two new sealed `Behaviour` primitives — `renameTo:` (class rename) and `renameSelector:to:` / `renameSelector:to:ifAbsent:` (method rename) — plus a `Workspace`-level `moveClass:to:` for the path-only-move case. Both rename primitives compute their rewrite sites from already-shipped xref infrastructure rather than a new query, and — because that infrastructure has different completeness guarantees for class names versus selector names (Constraints 4–5) — apply different auto-rewrite scopes: `renameTo:` rewrites every site `referencesTo:`/`direct_subclasses/1` find; `renameSelector:to:` auto-rewrites only structurally-unambiguous `self`/`super` sends and reports everything else as a manual-review candidate list. Both produce a `sites`-shaped ChangeLog entry, extending ADR 0113's `confirmDestructive` tier and ADR 0082's two-phase flush protocol (now genuinely multi-file) rather than building new mechanisms for either.**

### New primitives

```beamtalk
sealed renameTo: aNewName :: Symbol -> Behaviour =>
  @primitive "classRenameTo"

sealed renameSelector: aSelector :: Symbol to: aNewSelector :: Symbol -> Behaviour =>
  @primitive "classRenameSelector"

sealed renameSelector: aSelector :: Symbol to: aNewSelector :: Symbol
    ifAbsent: absentBlock :: Block(T) -> Behaviour | T =>
  @primitive "classRenameSelectorIfAbsent"
```

Both follow `removeSelector:`'s established shape exactly (ADR 0112): sealed class-side methods on `Behaviour`, return the receiver on success for chaining, raise a structured `#beamtalk_error{}` (`selector_not_found`, reusing ADR 0112's kind — not a new one) when the source selector is absent, and the bare form is paired with an `ifAbsent:` escape hatch rather than a boolean return, for the identical reasons ADR 0112 gives (DNU-convention, `at:`/`at:ifAbsent:` idiom, `Behaviour | T` union return). No independent steelman is repeated here for those two shapes — see ADR 0112's Steelman Analysis, which this ADR treats as settled precedent.

**Receiver and side**, for `renameSelector:to:`: same convention as `removeSelector:` — `Counter renameSelector: #a to: #b` touches the instance-side table, `Counter class renameSelector: #a to: #b` touches the class-side table.

```beamtalk
Counter renameSelector: #increment to: #incrementBy
Counter class renameSelector: #ofSize: to: #withCapacity:
Counter renameTo: #Accumulator
```

**A collision with an existing name is refused for both,** loudly, the same way `removeFromSystem` already refuses a name it can't act on — `renameTo: #Existing` when `Existing` is already a loaded class, or `renameSelector: #a to: #b` when `#b` is already locally defined, raises rather than silently overwriting:

```beamtalk
Counter renameTo: #Accumulator
// => error: cannot rename Counter to Accumulator — Accumulator already exists
//    hint: remove or rename the existing class first

Counter renameSelector: #increment to: #decrement
// => error: Counter already defines #decrement locally — refusing to overwrite
//    hint: removeSelector: #decrement first, or choose a different target name
```

### `renameTo:` rewrites cross-file references using `referencesTo:` + `direct_subclasses/1` — no new xref query

Class references (`Counter new`, `Counter class`, a `:: Counter` type annotation, a `subclass: Counter` superclass reference, an extension declaration in another file) compile to a runtime lookup by name atom, and that atom is baked into the referencing call site exactly as a method selector is baked into a sender (Constraint 1). A bare re-registration would turn every one of those into a silent, delayed `class_not_found` the next time it executes.

`renameTo:` therefore uses existing, already-shipped infrastructure for site discovery: **`SystemNavigation default referencesTo: aClass` (ADR 0087, BT-2302) already is** the "class → referencing sites" index — a maintained xref table populated at class-load time, sub-millisecond, already covering constructor/message sends, type annotations (including generic parameters like `List(Counter)`), and extension-method references (ADR 0066).

**One reference kind `referencesTo:` does *not* cover: superclass declarations.** `referencesTo:`'s own doc comment scopes it to "class-body reference sites" — occurrences inside a method's body or signature — and a class's own declaration header (`Object subclass: SpecialCounter ... `, naming `Counter` as an ancestor) is not inside any method body, so it is outside what the xref table indexes. Renaming `Counter` therefore needs a second, separate source for this one reference kind: `beamtalk_class_registry:direct_subclasses/1` returns exactly the classes that need their declaration header's superclass reference rewritten — only *direct* subclasses need touching, since a transitive subclass's own declaration names its direct superclass, not `Counter`.

`renameTo:`'s full site list is therefore the union of `referencesTo: aClass` (body/type/extension references) and `direct_subclasses(aClass)` (superclass-declaration references), not one query. Phase 1's validation spike (see *Phased rollout*) needs to confirm this union is exhaustive against real code — including a live-patched fixture, per Constraint 4 — before any primitive is wired up.

**The live-patch gap (Constraint 4) and plain string/comment occurrences are real, accepted residual risk, not silently generalized away.** A class live-patched via `>>` that references another class produces a `references: []` xref row, so `referencesTo:` will not surface it as a site — not because the reference is exotic, but because the reference-indexing half of ADR 0087's xref machinery was never extended to cover live edits. Closing this for real (extending `beamtalk_xref` with a runtime references walker) is out of scope for this ADR; `renameTo:` ships against the index as it exists today, with the gap documented and exercised by Phase 1's spike rather than hidden. Plain string/comment occurrences of a class's name (an error message, a test assertion, a doc comment) are invisible to any AST-based reference index by construction and are never rewritten — a "clean, N files touched" rename can still leave stale prose scattered through `stdlib/test/*.bt` or `docs/`. Both gaps are the same *category* of accepted risk ADR 0112 established for `removeSelector:`'s dangling senders (missed references, not corrupted ones — see *Alternatives Considered*), just concretely named rather than waved at generically.

### `renameSelector:to:` auto-rewrites only `self`/`super` sends — `sendersOf:` alone is not a safe basis for auto-rewriting arbitrary call sites

**The crux of this ADR: `renameTo:`'s site discovery is keyed by class name, a globally unique identifier, so every site it returns genuinely is a reference to the class being renamed. `renameSelector:to:`'s site discovery (`sendersOf:`) is keyed only by selector name** (Constraint 5) **— so `sendersOf: #at:put:` returns every textual send of that selector anywhere in the project, regardless of which class's `at:put:` the sender actually meant to call.** Blindly auto-rewriting every one of those sites is not an incompleteness risk (the *missed-reference* category ADR 0112 already accepts) — it is a *false-positive* risk: a rename of `Counter>>at:put:` to `Counter>>setAt:to:` would, unchecked, also rewrite `aDictionary at: k put: v` sends elsewhere in the project that have nothing to do with `Counter`, silently corrupting working, unrelated code. Missing a real reference and mangling an unrelated one are not the same failure class.

**The fix uses `recv_kind`, the site's `owner` field, and one more check against the already-shipped `implementors_of/1` query — all existing xref infrastructure (ADR 0087) — to split `sendersOf:`'s results into two tiers. `recv_kind: self_recv`/`super_recv` plus `owner`-in-hierarchy narrowing is *necessary but not sufficient*: neither one rules out an intervening override shadowing the call.**

- **Owner-in-hierarchy alone doesn't close the false-positive risk, it only shrinks it.** `recv_kind: self_recv`/`super_recv` proves the send resolves within the runtime receiver's own class hierarchy, but not *which* hierarchy — an unrelated class sharing the selector name (`Timer>>increment` self-sending `self increment`, no inheritance relationship to `Counter`) would be wrongly rewritten on `recv_kind` alone, so `owner` must also be `Counter` itself or a transitive member of `Counter`'s subclass tree (`direct_subclasses:`, the same closure `renameTo:`'s site discovery already builds).
- **But an override anywhere in `Counter`'s subclass tree still breaks a same-hierarchy `self`/`super` rewrite.** `self`/`super` dispatch starts at the *runtime receiver's actual class* (`self`) or the *sending method's static superclass* (`super`), not at `Counter`, and stops at the first class going upward that defines the selector. If `Sub1 subclass: Counter` overrides `#increment`, then `Sub2 subclass: Sub1` sending `self increment` (or `Sub2>>increment` sending `super increment`, the classic override-then-call-`super` idiom) resolves to **`Sub1`'s override today**, never reaching `Counter`'s implementation at all — `owner: Sub2` is correctly inside `Counter`'s hierarchy, but the send doesn't target `Counter`'s method. Rewriting the call site to the new selector doesn't touch `Sub1`'s untouched, still-`#increment`-named override, so post-rewrite dispatch searches past it (finds nothing named the new selector on `Sub1`) and lands directly on `Counter`'s renamed implementation instead — silently skipping the override, a real behavior change with no crash to catch it. The same failure applies even when `owner` is `Counter` itself: `self`/`super` dispatch is late-bound to the *runtime receiver's* class, so a self-send inside a method `Counter` itself defines is just as capable of resolving to a subclass override as one sent from a subclass.
- **The closure that's actually sound: `Counter`'s implementation must be the *only* implementation of the selector, on the same side, anywhere in `Counter`'s subclass tree.** No new query needed — `beamtalk_xref.erl` already exports `implementors_of/1`, returning every `{Class, ClassSide}` pair implementing a given selector (ADR 0087/BT-2300; `SystemNavigation>>implementorsOf:` is its Beamtalk-facing wrapper). The override-freedom check is `implementors_of(Selector)` filtered to `ClassSide` matching this rename's own `side`, then intersected with `direct_subclasses:`'s transitive closure of `Counter`, minus `Counter` itself: if that intersection is empty, no override can ever intercept a same-hierarchy `self`/`super` send before it reaches `Counter`, for any possible receiver, and auto-rewrite is sound. If the intersection is non-empty, auto-rewrite is unsound for the whole selector — not just for sites downstream of that specific override — because a single shared call-site text is executed polymorphically across every possible receiver class, and safety must hold for all of them, not just the common case.

**Auto-rewritten (`confirmed` sites):** the definition itself, plus every site with `recv_kind: self_recv` or `recv_kind: super_recv` whose `owner` is `Counter` itself or a transitive member of `Counter`'s subclass tree — **and only when the override-freedom check above holds for this selector**. When it doesn't hold, every self/super site for this selector — including ones whose `owner` is `Counter` itself — moves to `candidate_sites` instead; there is no partial auto-rewrite for a selector with any override present.

**Reported, not rewritten (`candidate` sites):** every site with `recv_kind: other` (an arbitrary expression receiver — the common "external caller" case, and also the most common shape of a real, intended sender) or `recv_kind: erlang_ffi`; every `self_recv`/`super_recv` site whose `owner` falls outside `Counter`'s hierarchy (the `Timer` case); and — when the selector is overridden anywhere in `Counter`'s subclass tree — every `self_recv`/`super_recv` site regardless of `owner`, since the override-freedom precondition for safe rewriting fails for the whole selector in that case. The xref index cannot resolve these further, so `renameSelector:to:` does not touch them. They are surfaced the same way ADR 0112's dangling-sender hint already surfaces risk — as a reported count and site list on the primitive's return value and the resulting ChangeLog entry — so the caller can inspect each one and, where it genuinely is a `Counter`-directed send unaffected by the override, patch it manually via the ordinary `compile:source:` path.

This makes `renameSelector:to:` auto-rewrite conservative by construction: it only ever fires when `Counter`'s implementation of the selector is structurally guaranteed unique across its whole subclass tree, which is common (most methods aren't overridden) but not universal — a class hierarchy that overrides the renamed selector anywhere gets zero auto-rewrite for that selector, falling back entirely to the reviewed `candidate_sites` list. This is a real, load-bearing narrowing versus the ADR's earlier (incorrect) `owner`-only rule, not a minor refinement — flagged as an explicit Phase 1 spike question (see *Phased rollout*): confirm `implementors_of/1` intersected with the subclass closure is cheap enough (one query per rename, not per site) to run unconditionally before any auto-rewrite decision.

This is a real scope reduction from "renaming a method automatically fixes every caller" to "renaming a method automatically fixes its own definition and hierarchy-internal calls, and hands you a reviewed list for everything else" — narrower than what a type-checked language's rename-refactoring tool can offer (see *Prior Art*), but it is the boundary the *existing* xref infrastructure can actually support without corrupting unrelated code. Extending `beamtalk_xref_senders` with inferred-receiver-type narrowing (leaning on ADR 0025's gradual typing where a receiver carries a `:: Counter` annotation) would let a future revision safely promote more `other`-kind sites into the auto-rewritten tier — flagged here as follow-up work, not designed by this ADR.

### `Workspace moveClass:to:` — pure file move, no identity change

`Workspace moveClass: aClass to: aNewPath` relocates a class's `.bt` file without changing the class's name. Unlike `renameTo:`, no cross-file reference needs rewriting — every call site still says `Counter`, only where `Counter.bt` lives on disk changes — so `moveClass:to:` has no sites beyond the single file being moved.

```beamtalk
sealed moveClass: aClass :: Behaviour to: aNewPath :: String -> Behaviour =>
  @primitive "workspaceMoveClass"
```

This is a `Workspace`-level operation, not a `Behaviour` primitive, for the same reason `newClass:at:` is (ADR 0082): it's a pure filesystem-organization concern, not a class-protocol message the class itself needs to understand or respond to differently. It produces a `kind: "rename-class"` entry with `old_class == class` and a `sites` list containing only the moved file's own declaration-line entry — never a foreign reference, since none needs rewriting. **Refusal/flushability** mirrors `renameTo:`'s row below with one difference: a dynamic (`ClassBuilder`) class has no file to move at all, so `moveClass:to:` raises `no_source_file` for it rather than `renameTo:`'s permissive `flushable: false` — moving *nothing* is not the same kind of legitimate in-memory action as patching a dynamic class's body is.

### Refusal vs flushability, decided per operation, not uniformly

ADR 0112 chose "flushable, not refusal" for `compile:source:`/`removeSelector:` (they install in memory unconditionally against stdlib/dynamic/dependency classes; only the disk write is gated). `removeFromSystem` chose the opposite — hard refusal before any memory mutation. This ADR asks, per primitive, whether the *in-memory* effect alone is safe against a class you don't own the source of:

| Primitive | Stdlib | Dynamic (ClassBuilder) | Dependency | Rationale |
|---|---|---|---|---|
| `renameTo:` | **Refuse** | Allowed, `flushable: false` (`"dynamic"`) | **Refuse** | The xref index only indexes in-project source, so the site-discovery mechanism above can only ever compute a *complete* reference list for a class the project actually owns the callers of — renaming a stdlib or dependency class would silently miss every reference living outside the project. A dynamic class has no external file-based referents the xref index would need to reach beyond what's already indexed (ADR 0038); it is the caller's own construction. |
| `renameSelector:to:` | Allowed, `flushable: false` (`"stdlib"`) | Allowed, `flushable: false` (`"dynamic"`) | Allowed, `flushable: false` (`"dependency:<path>"`) | Same granularity argument ADR 0112 already made for `removeSelector:` — a single-selector operation, not a whole-class identity change. Its confirmed-site list is subject to the identical in-project-only xref limitation, but a stray un-rewritten `self`/`super` sender is a narrower blast radius than a whole class silently losing referential integrity project-wide. |

### ChangeLog schema

Extending ADR 0113's `kind` enum (which itself extended ADR 0082/0112's):

```text
%% rename-class — appended by renameTo: / Workspace moveClass:to:
{ts, seq, epoch, class: "<new name>", selector: null,
 kind: "rename-class",
 side: null,
 old_class: "<old name>",
 old_path: "<path>" | null,
 new_path: "<path>" | null,                     % basename derived from new_class, same directory as old_path
 sites: [{sourceFile, span: {start, end}, source_ref, prev_source_ref}, ...],
   %% sites[0] is the class's own declaration line, UNLESS the class is
   %% dynamic (ClassBuilder, no backing file — flushable: false, "dynamic"
   %% below) — a dynamic class has nothing for sites[0] to point at, so its
   %% rename entry has sites[0] = null (in-memory identity change only,
   %% recorded for revert/audit but not a splice target) and sites[1..] are
   %% every current in-project cross-file reference (constructor/message
   %% sends, type annotations, superclass declarations, extension
   %% declarations) found via referencesTo:/direct_subclasses:
 source_ref: null, prev_source_ref: null,        % superseded by per-site refs above; no single-file body to record
 sourceFile: null,                                 % ambiguous for a multi-file entry — see sites
 span: null,
 intent: "durable",
 flushable: bool,                                  % true iff every entry in `sites` is in a flushable file
 not_flushable_reason: "dynamic" | null,
 author, author_kind}

%% rename-method — appended by renameSelector:to:
{ts, seq, epoch, class, selector: "<new selector>", old_selector: "<old selector>",
 kind: "rename-method",
 side: "instance" | "class",
 sites: [{sourceFile, span: {start, end}, source_ref, prev_source_ref}, ...],
   %% sites[0] is always the definition site; sites[1..] are self_recv/
   %% super_recv sends found via sendersOf: at rename time WHOSE owner is
   %% this class or a transitive member of its subclass tree (direct_
   %% subclasses:, same closure renameTo: uses) — AND ONLY IF no class in
   %% that subclass tree also defines this selector on the same side
   %% (implementors_of/1 filtered to this side, intersected with the
   %% subclass closure; an override anywhere means self/super dispatch
   %% can be
   %% intercepted before reaching this class, so NONE of this selector's
   %% self/super sites are safe to rewrite, not even ones owned by this
   %% class itself — see candidate_sites). These are the ONLY sites flush
   %% ever writes for this entry.
 candidate_sites: [{sourceFile, span: {start, end}}, ...],
   %% other_recv/erlang_ffi sends found via the same sendersOf: query;
   %% every self_recv/super_recv send whose owner is outside this class's
   %% hierarchy; and, when the selector is overridden anywhere in this
   %% class's subclass tree, EVERY self_recv/super_recv send for this
   %% selector regardless of owner. Reported for human/agent review, never
   %% auto-rewritten and never written by flush. No source_ref/
   %% prev_source_ref: nothing here is ever spliced, so there is no
   %% prior/new body to record.
 source_ref: null, prev_source_ref: null,
 sourceFile: null,
 span: null,
 intent: "durable",
 flushable: bool,                                  % true iff every entry in `sites` (not candidate_sites) is flushable
 not_flushable_reason: "stdlib" | "dynamic" | "dependency:<path>" | null,
 author, author_kind}
```

`rename-class` and `rename-method` are the two shapes that target a computed set rather than one file. For both, `flushable` is `true` only if **every entry in `sites`** resolves to a flushable file; `candidate_sites` never gates flushability either way, since flush never writes them — a stdlib class being an `other`-kind candidate sender does not block an otherwise-clean rename the way a confirmed stdlib site would (this is a deliberate design choice: without it, a single incidental stdlib candidate sender would leave the whole rename stuck forever, recreating the exact bug ADR 0113 exists to fix for `remove-method`).

### Flush — reusing ADR 0113's tier, extended to genuinely multi-file

`rename-class`/`rename-method` join `remove-class` in Tier 2 — `Workspace flushIncludingDestructive` / `flush: aClass confirmDestructive: true` apply them, exactly as ADR 0113 designed. What's new here is that atomicity is genuinely multi-file for the first time since ADR 0082:

| Operation | Phase A (stage) | Phase B (commit) |
|---|---|---|
| Class rename | Write `<new_path>.tmp` (declaration-line rewritten to the new name; rest of the file byte-identical) **and** `<file>.tmp` per *other* site file with the old-name reference rewritten | Rename `<new_path>.tmp` → `<new_path>`, `unlink <old_path>`, then rename each site `<file>.tmp` → `<file>`, in seq order |
| Method rename | Write `<file>.tmp` per affected file (definition site + every *confirmed* sender site, spans rewritten — `candidate_sites` are never staged or written) | Rename each `<file>.tmp` → `<file>`, in seq order — same sequential-commit, partial-failure-is-recoverable-via-re-flush shape ADR 0082 already documents for ordinary multi-file flush |

A Phase A failure (a target span no longer resolves) aborts the whole batch before anything in Phase B runs. A Phase B failure partway through leaves some files renamed and some not; the per-file status report tells the caller which, and re-issuing the same destructive flush call retries only what's left.

**This table covers the *disk* half of atomicity only. The *in-memory* half — rewriting confirmed sites across N separate class gen_servers before any flush happens at all — is a real, separate correctness question this ADR does not fully design, and downstream implementation must not treat it as solved by analogy to the table above.** Each site's recompile-and-hot-reload goes through its own class gen_server, and OTP has no cross-process transaction primitive spanning them — if rewriting confirmed site 5 of 10 fails partway through the in-memory step, the rename is left half-applied *in memory*, before flush is even in the picture, and (per ADR 0082's existing "cannot roll back a hot-reloaded module once live actors may hold references to it" precedent) there is no clean rollback. Downstream implementation needs an explicit answer here — most plausibly, validate every confirmed site's compile-ahead-of-mutation so a failure aborts before any class is actually re-installed — but this ADR does not specify that mechanism. Flagged as a required design decision for Phase 2's implementer, not resolved here.

### Undo

Both kinds restore each recorded `sites` entry back to its own `prev_source_ref` directly, against those recorded locations — not a fresh call to the public `renameTo:`/`renameSelector:to:` primitives, which would re-run xref discovery against post-rename state and could compute a different site list than the original rename touched (if a referencing file was independently edited in between). The class's own identity/path (`old_class`/`old_path`) needs no `prev_source_ref` for the re-registration half, matching `new-class`'s "add-removal needs no prior body" precedent — but the per-site reference rewrites do.

### Reproducible-build guarantee

Unaffected by construction: `renameTo:` refuses stdlib/dependency classes before any ChangeEntry exists; `renameSelector:to:`'s per-site `flushable` check means a rename touching even one stdlib/dependency confirmed site never reaches Phase A for *any* of its sites. Flush still never writes into the stdlib tree or a dependency cache.

### Surface

Per ADR 0082/0113's principle, every surface constructs a Beamtalk expression and submits via `evaluate`. `remove_class`/`rename_class`/`rename_method` MCP tools, `:rename-class`/`:rename-method` REPL meta-commands, and LSP's `confirmDestructive`-gated flush all follow ADR 0113's established per-surface confirmation shapes (REPL two-prompt, MCP required argument, LSP modal, browser second click) — no new confirmation pattern is introduced, only new expressions flowing through the existing gate.

## Prior Art

### Pharo / Squeak Smalltalk — Refactoring Browser

Pharo's Refactoring Engine (`RBRenameClassRefactoring`, `RBRenameMethodRefactoring`) computes every affected reference *before* applying anything, previews the full change set, and applies it as one atomic transaction across the image — rename in Pharo has never been "rewrite the definition and hope callers notice."

**Adopted:** the "compute every affected site before touching anything, apply as one transaction" shape for both primitives — this is exactly the `sites` list and Phase A validate-everything-before-writing-anything design above.
**Adapted:** Pharo's rename preview is an interactive image-browser step with no disk/memory distinction (there is no flush). This ADR's `confirmDestructive` gate is the same *intent* reshaped for a system where memory and disk are already two separate steps — the preview is `Workspace changes` queried *before* the confirming flush call.
**Not adopted:** Pharo's implicit assumption that "every sender" is safe to rewrite. Pharo's `sendersOf:` has the identical selector-global-scope shape this ADR's Constraint 5 describes, and in practice Pharo developers manually review the Rename Method preview for exactly this reason — Pharo's tooling doesn't solve the false-positive problem either, it just makes the human do the filtering interactively rather than splitting confirmed/candidate programmatically.

### Python / TypeScript — statically-indexed vs. textual rename tooling

This ADR's central risk (auto-rewrite is only as safe as the index behind it) is not Beamtalk-specific — it is the exact fault line separating rename tooling quality across the mainstream-language landscape. **TypeScript's "Rename Symbol"** (via `tsserver`) is built on the compiler's own semantic reference graph — a rename that misses a reference is a compiler bug, because the type-checked graph is exhaustive by construction for anything the compiler can see. **Python's rename tooling** (`rope`, PyCharm's refactor-rename) has no compiler-verified reference graph — it is AST/text-pattern-based, and every Python refactoring tool's documentation carries some version of the same warning this ADR gives `candidate_sites`: `getattr(obj, "method_name")`, monkey-patching, and dynamically imported modules are invisible and will silently break.

**Adopted:** the xref-index-driven `sites` mechanism is architecturally TypeScript's approach (a maintained, queryable reference graph feeding the rename) — Beamtalk already has this graph for other purposes, so this ADR extends an existing asset rather than building AST-pattern-matching from scratch.
**Adapted:** Beamtalk cannot get TypeScript's *soundness guarantee* because, like Python, it is dynamically dispatched — `perform:`, `Smalltalk at:`, and any string-built selector are invisible to xref exactly as `getattr` is invisible to `rope`. This ADR's posture is therefore Python's, not TypeScript's: best-effort against a real index, with an accepted, documented gap for dynamic access.
**Rejected:** neither TypeScript's "refuse to compile until every reference is fixed" gate (Beamtalk has no project-wide compile step that could enforce it) nor Python tooling's common fallback of dry-run-only with no automatic rewrite at all (see *Alternatives Considered*).

### LSP — `workspace/applyEdit` with `RenameFile`, revised to a custom `documentMoved` notification (BT-3285)

ADR 0113 already adopted `DeleteFile`/`CreateFile`. This ADR originally added `RenameFile` for class-file moves too, plus a `TextDocumentEdit` per confirmed method-rename site — the same typed-resource-operation machinery, its last consumer. The method-rename `TextDocumentEdit` shipped as designed and is unaffected by what follows; the class-file-move `RenameFile` op did not work as intended and was replaced (BT-3285).

**Why `RenameFile` was replaced.** By the time a `renameTo:`/`moveClass:to:` flush's `workspace/applyEdit` reaches the client, `beamtalk_workspace_flush`'s Phase B has already renamed the file on disk and unlinked the old path (see that module's moduledoc, "Atomicity (class rename)" — `complete_flush/5` announces only after Phase B's `Committed` list is final; this ordering is a crash-recovery guarantee this ADR does not revisit). So the `RenameFile` op's `old_uri` never exists on disk by the time a client receives it. In VS Code specifically, this is not a hard failure: `ignoreIfExists: true` (needed regardless, since the flush may also race an editor-side save) combined with the target already existing makes VS Code's `RenameOperation.perform()` skip the move step entirely rather than error — but skipping it also means VS Code performs *no editor-state retargeting*, so an open tab at the old path silently never followed the rename. No crash, no data loss (the correct content is already on disk under the new path), but the UX goal that motivated choosing `RenameFile` in the first place did not actually happen.

**The fix.** Reordering `beamtalk_workspace_flush`'s announcement to fire before Phase B's unlink was considered and rejected as too risky to the already-shipped crash-recovery ordering for a same-issue fix. Instead, the LSP server now sends a custom notification, `beamtalk-lsp/documentMoved` (server → client, `{oldUri, newUri}`), from the same call site the `RenameFile` op used to fire from — and no longer sends the `RenameFile` op at all, since (per the investigation above) it never achieved its purpose in VS Code and no other LSP client is known to depend on it.

- **This project's own VS Code extension** (`editors/vscode/`) handles `beamtalk-lsp/documentMoved` directly — the same custom-request precedent as `beamtalk-lsp/fetchContent` (used by `StdlibContentProvider`), just server-initiated rather than client-initiated. On receipt, it closes any open tab at `oldUri` and reopens `newUri` in its place, restoring the view column and cursor/scroll position captured from any visible editor that was showing the old path (`handleDocumentMoved` in `extension.ts`; the retargeting decision itself is pure, `vscode`-independent logic in `documentMoved.ts`, unit-tested in `editors/vscode/src/__tests__/documentMoved.test.ts`). This is the one LSP client where "an open tab follows the rename" actually happens today.
- **Any other LSP client** (an editor without Beamtalk-specific support for this notification) sees the documented degraded outcome: per the LSP spec, an unrecognised notification is simply ignored, so there is no error and no crash, but also no retargeting — an open tab at the old path stays open, pointed at a file that no longer exists, until the user closes or reopens it. This is the same no-crash/no-retarget outcome the dropped `RenameFile` op produced in VS Code, just without a filesystem-rename request that never actually accomplishes anything by the time the client sees it.

No changes were made to `beamtalk_workspace_flush`'s commit/crash-recovery ordering — this approach doesn't require any.

## User Impact

### Newcomer (from VSCode / Python / JS)

- "Rename Symbol" already exists as a concept in every editor a newcomer has used — VSCode's rename-symbol feature previews every affected file before applying. `confirmDestructive`'s LSP-surfaced modal matches that expectation directly for `renameTo:`.
- For `renameSelector:to:`, the `candidate_sites` list is the one place this ADR asks more of a newcomer than VSCode does: a TypeScript rename "just works," and this one sometimes hands back a follow-up list. This should be told plainly in tooling copy ("N call sites found automatically; M more may also need updating — review them") rather than presented as a silent partial success.

### Smalltalk developer

- `renameTo:`/`renameSelector:to:` read as exactly the kind of message-send-based class-protocol operation Smalltalk trains developers to expect (mirrors `removeSelector:`'s reception in ADR 0112).
- A Pharo developer used to the Refactoring Browser's "every sender" guarantee should be told plainly that `renameSelector:to:` only auto-applies to `self`/`super` sends and reports the rest as `candidate_sites` — this is narrower than Pharo's own tool, not equivalent to it, even though the *mechanism* (compute sites, apply as one transaction) is the same shape.

### Erlang/BEAM developer

- Rename recompiles a bounded set of modules through the existing hot-reload pipeline already used for `removeSelector:`'s recompile-based mechanism (ADR 0112). No new OTP pattern, though the in-memory multi-gen-server sequencing question above is a real, BEAM-specific open design item.
- A production release node never sees any of this — same "no workspace, no ChangeLog, no flush" guarantee ADR 0082/0112/0113 already give.

### Production operator

- Every rename is ChangeLog-audited the same way a patch is — "was this renamed, by whom, when" has a definitive answer via `Workspace changes`, matching ADR 0082's audit-trail guarantee.
- `confirmDestructive` being a call-site argument, reused unmodified from ADR 0113, means an operator sees the same explicit-consent signal for a rename's file move as for a class deletion.

### Tooling developer (LSP/MCP/browser)

- The method-rename `TextDocumentEdit` completes the typed-resource-operation set ADR 0113 started. The class-rename `RenameFile` op that was meant to complete it the same way didn't work as intended (BT-3285) and was replaced by a custom `documentMoved` notification — see "LSP" above.
- `remove_class`/`rename_class`/`rename_method` MCP tools and `:rename-class`/`:rename-method` REPL commands extend the existing tool surface with no new dispatch mechanism.

## Steelman Analysis

### Eager multi-site rewrite (this ADR's design) vs. keep the old name as a forwarding alias

- 🧑‍💻 **Newcomer:** "An alias means my old code just keeps working — I don't get a surprise `class_not_found` from some file I forgot about, and I don't get a confusing `candidate_sites` list to act on either."
- 🎩 **Smalltalk purist:** "GemStone and other production Smalltalks have used exactly this kind of alias/forwarding for schema evolution for decades — it's a well-trodden path, not a hack."
- ⚙️ **BEAM veteran:** "A redirect sidesteps the in-memory cross-gen-server transaction problem entirely — no sender needs to change atomically with the rename, because old callers keep working through the alias regardless of ordering."
- 🏭 **Operator:** "Zero risk of breaking production traffic mid-rename — the blast radius of an incomplete rewrite drops to zero because nothing actually stops working."
- 🎨 **Language designer:** "Decouples correctness from xref completeness entirely — the `sendersOf:`/`referencesTo:` gaps this ADR spends most of its design budget on stop being correctness risks and become pure cleanup convenience."
- **Why eager rewrite wins for v1 anyway:** the alias is not free — it needs its own lifecycle design (when does it expire? does it survive workspace restart? does `revert:` need to unregister it? does a second rename stack aliases?) that this ADR would then have to build from scratch, and it is exactly the `undef_method`-adjacent "keep answering under the old name forever" shape ADR 0112 explicitly declined to design ("a tombstone value... is left as a genuinely separate primitive to design from scratch"). It would also add a permanent dispatch-hot-path cost (every `whereis_class`/selector lookup needing an alias-table fallback check) that every other primitive in ADR 0082/0112/0113 was careful to avoid introducing. Given the eager-rewrite design already reduces to the safe self/super-only subset for methods and is fully safe for classes (name-keyed, no ambiguity), the incremental safety a redirect buys is real but narrower than it first appears. **This is flagged as the strongest candidate for reconsideration if Phase 2's real usage shows the `candidate_sites` manual-followup burden is too high** — see *Alternatives Considered*.

### `renameSelector:to:`'s narrowed auto-rewrite (self/super only) vs. full `sendersOf:`-driven rewrite (rejected as unsafe)

- 🧑‍💻 **Newcomer:** "I renamed a method and it didn't fix my caller — that's confusing if I don't know why."
- 🎩 **Smalltalk purist:** "Pharo's tool rewrites everything `sendersOf:` finds — matching that behaviour is matching the tool developers already know."
- ⚙️ **BEAM veteran:** "Less automatic rewriting means less to verify and less that can go silently wrong across N files — the conservative choice is the BEAM-idiomatic one."
- 🏭 **Operator:** "A `candidate_sites` list an agent/human must act on is an auditable, deliberate step — better than a rename that silently touched code nobody reviewed."
- 🎨 **Language designer:** "Correctness has to come before convenience. An automatic rewrite that can corrupt unrelated code is not a smaller version of the feature — it's a different, worse feature that happens to share a name."
- **Why the narrowed scope wins decisively, not just defensibly:** this isn't a case of competing legitimate preferences — full `sendersOf:`-driven rewrite is an active correctness bug (Constraint 5), not a design tradeoff with real upside. The Smalltalk-purist "match Pharo" argument doesn't actually hold once you check what Pharo does: its Rename Method tool surfaces the same over-broad sender list for human review rather than blind auto-apply (see *Prior Art*), so this ADR's design *is* the Pharo-faithful behaviour, not a departure from it.

### Tension points

- **Newcomer "just rename it" expectation vs. structural correctness limits:** the strongest real tension, and it doesn't fully resolve — a newcomer coming from TypeScript will find `renameSelector:to:` under-delivers relative to their mental model, and no amount of UX polish removes that gap without new type-narrowing infrastructure this ADR doesn't build. Named honestly in *Consequences* rather than smoothed over.
- **Alias/redirect's appeal vs. its own undesigned lifecycle cost:** resolved for v1 in favour of eager rewrite, but not dismissed — flagged explicitly as the first thing to revisit if usage data says the `candidate_sites` burden is too high.

## Alternatives Considered

### Alternative: do nothing — leave rename undesigned

A real workaround already exists without any new primitive: manually create the new class/method and delete the old one via `removeFromSystem`/`removeSelector:` (ADR 0113). Rejected as sufficient because it loses the class's identity continuity (existing instances, if any, aren't retargeted), loses in-place undo, and has no automatic reference-fixing story at all — strictly worse than this ADR's `candidate_sites`-assisted rewrite for the exact scenario rename exists to serve.

### Alternative: block `renameTo:`/`renameSelector:to:` on references outside the current in-project rename batch (never leave a partial rename)

Considered for the case where a rename's site list might miss a reference in a file the xref index hasn't indexed yet, or a dynamically-constructed lookup (`aClass perform: (aString asSymbol)`, `Smalltalk at: aSymbol`) that no static xref can see. Rejected as a hard block, for both primitives: ADR 0112 already accepted this exact risk category for `removeSelector:` and gave a considered reason not to block on it — the same reasoning applies to both rename primitives without a new argument. Both primitives instead surface the reference count found so the caller has visibility, without refusing an action ADR 0112 already decided is the caller's call to make.

### Alternative: language-level rename primitive omitted; rename is LSP/tooling-only

Rejected for the identical reason ADR 0112 rejected a workspace-only `removeSelector:` — it would violate ADR 0082's "every tool op is a structured invocation of a Beamtalk expression" principle, and every surface would end up separately re-implementing "find senders, rewrite them" logic against the xref index instead of sharing one primitive.

### Alternative: keep the old name registered as a forwarding alias instead of eagerly rewriting every site

See Steelman above. Rejected for v1, not dismissed as unsound — `beamtalk_alias_xref.erl` (ADR 0108, named union type aliases) is direct precedent that alias-as-first-class-mechanism has a home in this codebase, but a *dispatch-time* class-identity alias is a materially different runtime commitment (a permanent dispatch-hot-path cost every other primitive here was careful to avoid) and needs its own from-scratch lifecycle design (expiry, restart survival, `revert:` interaction, stacking). Flagged as the right design to revisit if Phase 2's real usage shows the eager-rewrite approach's `candidate_sites` burden is too high.

## Consequences

### Positive

- Closes the gap ADR 0113 explicitly deferred — `Behaviour` gains a complete patch/create/remove/rename set (`compile:source:`, `newClass:at:`, `removeSelector:`, `renameTo:`, `renameSelector:to:`) with consistent receiver, error, and ChangeLog conventions across all five.
- Reuses, rather than reinvents, five separate pieces of existing infrastructure: ADR 0082's two-phase flush protocol, ADR 0112's recompile-based method-removal mechanism, ADR 0113's `confirmDestructive` tier and undo model, the xref index's existing `sendersOf:` query, and ADR 0087's already-shipped `SystemNavigation>>referencesTo:` combined with `direct_subclasses/1`.
- The self/super-vs-candidate split for `renameSelector:to:` is a discovered, deliberate correctness fix, not a late compromise — closing a real bug (unbounded cross-class rewrite) rather than merely a completeness gap.

### Negative

- **`renameTo:` and `renameSelector:to:` are the most implementation-heavy primitives this codebase has added via an ADR to date** — both require recompiling not just the target definition but every current in-project reference, computed transactionally, with a `sites` list that has no precedent in the schema shape ADR 0082/0112/0113 established (every prior kind targets exactly one file).
- **The in-memory multi-gen-server transaction question is explicitly unresolved by this ADR** (see *Decision*) — downstream implementation carries real design risk here that this document does not close out.
- **`renameSelector:to:` auto-rewrites less than "rename fixes every caller" implies.** Only `self`/`super` sends are auto-applied; `other`-receiver sends — the common external-caller case — become a manual-review `candidate_sites` list. For widely-shared selector names (`size`, `at:put:`, `printOn:`) that list will be long and mostly irrelevant noise from unrelated classes. This is the correct trade-off given the xref index's current capabilities, but it is a real capability gap relative to what a newcomer from a statically-typed IDE will expect.
- **A rename can only ever rewrite references the xref index can see**, and the xref index's `references` channel is unconditionally empty for live-patched methods (Constraint 4) — a materially more common gap than "dynamically constructed lookup," since it's triggered by ADR 0082's own everyday live-patch workflow.
- **A `rename-class`/`rename-method` entry that partially fails at Phase A (one site's span no longer resolves) aborts the entire multi-file rename** — more disruptive than a single-file patch conflict, since the caller must resolve one stale site before *any* of the rename can flush.
- **Each rewritten sender's compiler-cache entry needs the same purge/reindex step** `beamtalk_class_lifecycle` already applies to the renamed class itself (BT-3105/BT-3107) — the existing single-class-keyed purge does not automatically reach sender files without this being wired in explicitly during implementation.
- **Renaming the same class twice before flushing is a documented limitation, not supported chaining (BT-3283).** `renameTo:` computes a rename entry's `old_path` from the class's *compiled* `beamtalk_source` module attribute, which a flush only refreshes on commit — so `Foo renameTo: #Bar` followed, with no intervening flush, by `Bar renameTo: #Baz` produces two pending entries that both resolve `old_path` to the original file, tripping the flush's rename-vs-rename collision guard. The batch aborts cleanly (no data loss, both entries stay pending) with a collision-specific reason explaining a flush is needed between renames of the same class, rather than being collapsed into one effective two-hop rename — collapsing was considered and rejected as too much atomicity risk in an area (this ADR, BT-3270, BT-3271, BT-3278) that has repeatedly had subtle bugs, for what would only be a UX nicety. Flush between renames of the same class.

### Neutral

- No ephemeral rename variant and no bulk rename primitive ship — same "not called for by any acceptance criterion, trivially added later" answer ADR 0112 gave for analogous cases.
- `moveClass:to:`'s path-only-move case reuses the `rename-class` schema rather than inventing a distinct kind — `old_class == class` signals "same identity, different path" to any reader.

### DDD Model Impact

- **Compilation context** gains the multi-site rewrite step both `renameTo:` and `renameSelector:to:` share (rewrite N spans across N files, each independently re-parsed and re-validated) — an extension of the existing byte-span resolver (ADR 0082), not a new subsystem, and one mechanism serving both primitives.
- **Workspace context** extends ADR 0113's Tier-2 staging logic to genuinely multi-file (write-then-rename-then-unlink-old for class rename; write-N-then-rename-N for method rename).
- **Runtime context** gains `classRenameTo`/`classRenameSelector`/`classRenameSelectorIfAbsent`/`workspaceMoveClass` primitives, and wires site discovery to the already-shipped `referencesTo:`/`sendersOf:`/`direct_subclasses/1` rather than new queries.

## Implementation

*(For downstream implementation work — this ADR does not implement any of the below.)*

### Affected components

| Layer | Change |
|---|---|
| `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` | New `classRenameTo/2`, `classRenameSelector/3`, `classRenameSelectorIfAbsent/4` primitives, modelled on the existing `classRemoveSelector`/`classRemoveSelectorIfAbsent` functions. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_interface_primitives.erl` (or a new sibling module) | New `workspaceMoveClass/2` backing `Workspace moveClass:to:`, modelled on `newClass:at:` — single-file move, no site discovery. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_eval.erl` / `beamtalk_repl_loader.erl` | One shared multi-site rewrite mechanism, generalizing ADR 0112's `remove_method/3` (a sibling `rewrite_sites/4` or equivalent). `renameSelector:to:` calls it with `sendersOf:`'s `self_recv`/`super_recv`-filtered results, narrowed to sites whose `owner` is in the target class's hierarchy *and* gated on `implementors_of/1` (already shipped, ADR 0087) confirming the selector is overridden nowhere else in that hierarchy (`other`/`erlang_ffi` sites, out-of-hierarchy `self_recv`/`super_recv` sites, and — when the override-freedom check fails — every `self_recv`/`super_recv` site for that selector all go to `candidate_sites`, never rewritten); `renameTo:` calls the *same* function with the union of `referencesTo:` and `direct_subclasses/1`. Each rewritten site's own class needs the same `purge_compiler_cache`/xref-reindex step `beamtalk_class_lifecycle` already applies to the renamed class (BT-3105/BT-3107) — not automatic, must be wired in explicitly. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_changelog.erl` | New `kind`s (`'rename-class'`, `'rename-method'`), new fields (`old_class`, `old_path`, `new_path`, `sites`, `candidate_sites`). `target_key/1` needs a variant for both multi-site shapes — likely keying shadow-detection per-site rather than per-entry, since two independent renames could touch overlapping reference files. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_flush.erl` | Extend ADR 0113's Tier-2 staging to genuinely multi-file per the *Flush* table above. |
| `stdlib/src/behaviour.bt` | `renameTo:`, `renameSelector:to:`, `renameSelector:to:ifAbsent:` — three new sealed methods, same pattern as `removeSelector:`/`removeSelector:ifAbsent:`. |
| `stdlib/src/Workspace.bt` | `moveClass:to:`. |
| `crates/beamtalk-lsp/src/server.rs` | Emits the custom `beamtalk-lsp/documentMoved` notification (class rename, BT-3285 — see "LSP" above) and per-site `TextDocumentEdit` (method rename, completing ADR 0113's typed-operation work) resource operations. |
| `crates/beamtalk-mcp/src/server.rs` | New tools: `rename_class` (wraps `renameTo:`), `rename_method` (wraps `renameSelector:to:`). |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | New meta-commands `:rename-class`, `:rename-method`, matching ADR 0113's two-prompt shape. |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.js` | "Rename" browser action with the destructive-dirty-indicator affordance ADR 0113 established. |
| `docs/development/surface-parity.md` | New expression-backed rows for the rename surfaces. |
| `docs/beamtalk-language-features.md` | Document `renameTo:`/`renameSelector:to:` alongside `removeSelector:`/`removeFromSystem`. |

### Phased rollout

| Phase | Scope | Effort | Tests |
|---|---|---|---|
| **1** | **Validation spike**, in ADR 0082 Phase 0's spirit: run `referencesTo:` + `direct_subclasses/1` against the full stdlib + examples corpus **and against a live-patched fixture** (a class with a `>>`-patched method referencing another class, exercising the `references => []`-for-live-patches gap) and confirm the combined site list matches a hand-audited sample, *before* wiring it into any primitive. This is the load-bearing new assumption this ADR introduces. | L | Corpus round-trip / reference-discovery accuracy tests, including the live-patch fixture. |
| **2** | If the spike holds: the shared multi-site rewrite mechanism (transactional in-memory rewrite of definition + sites, `sites`-shaped ChangeLog entry, multi-file flush) plus `renameTo:` as its first consumer — class-rename flush (file move, declaration-line rewrite, reference-site rewrite), `Workspace moveClass:to:`. Must include an explicit answer to the in-memory cross-gen-server atomicity question (see *Decision*). | L | EUnit: reference rewrite correctness against a fixture graph (class with 3 in-project references across 2 files) + rename-to-tmp-then-final-rename atomicity; BUnit: `Counter renameTo: #Accumulator` + flush produces the moved file, updated declaration, and rewritten references, with no dangling `class_not_found`. |
| **3** | `renameSelector:to:`/`ifAbsent:` — reuses Phase 2's mechanism with `sendersOf:`'s `self_recv`/`super_recv`-filtered results, narrowed by `owner` against the target class's hierarchy *and* gated on the `implementors_of/1`-based override-freedom check (see *Decision*), as its site-discovery step. Depends on Phase 2, not the reverse; smaller because the mechanism itself is already built. | M | BUnit: end-to-end method rename + flush + verify no dangling `does_not_understand`; regression check that Phase 2's fixture graph still passes through the shared code path; explicit test that an `other_recv` sender is reported as `candidate_sites` and never rewritten; explicit test for the cross-hierarchy false-positive case — an unrelated class with no inheritance relationship to the renamed method's class, defining the same selector and sending it via `self`, must land in `candidate_sites`, never `sites`; **explicit test for the intra-hierarchy override-shadowing case** — a subclass overriding the renamed selector, with a further subclass sending `self`/`super` to it (the override-then-call-`super` idiom), must cause every `self_recv`/`super_recv` site for that selector — including ones owned by the renamed class itself — to land in `candidate_sites`, not just the sites downstream of the override. |
| **4** | `revert:` extensions for both kinds (Undo section above). | M | BUnit: revert of each kind, pre-flush and (where applicable) documented as unsupported post-flush. |
| **5** | LSP `RenameFile` + per-site `TextDocumentEdit`; MCP tools; REPL meta-commands; browser actions. Surface-parity audit. | M | LSP command tests; MCP integration tests; browser e2e; surface-parity drift check passes. |

Total: ~L across 5 phases. Phase 1 (the validation spike) is the load-bearing risk — if `referencesTo:`/`direct_subclasses/1` prove unreliable against real code (partial coverage, xref gaps beyond what's already known), the design may need to fall back to a narrower v1 for *both* rename primitives (rewrite the definition only, leave references as a `Workspace changes` follow-up list for the caller to apply manually) before committing to the full transactional shape. Building the mechanism once, via the lower-blast-radius class-rename case, before Phase 3 reuses it for method rename, is a deliberate risk-ordering choice.

## Implementation Tracking

**Epic:** BT-3267
**Issues:** BT-3268, BT-3269, BT-3270, BT-3278, BT-3271, BT-3272, BT-3279, BT-3273, BT-3274, BT-3275, BT-3276, BT-3277
**Status:** Planned

## References
- Related issues: BT-3204 (this ADR), BT-2192 (ADR 0113 — the split predecessor, owns `confirmDestructive`/two-phase-flush/undo this ADR extends), BT-2191 / BT-3183 (ADR 0112 — method-level removal, the primitive-design template), BT-3105 / BT-3107 (single-class teardown / metadata-write paths this ADR's implementation plan reuses)
- Related ADRs: ADR 0113 (Destructive Workspace Operations: File Deletion in Flush — the direct predecessor and split source), ADR 0082 (Method-Level Edit and Save — ChangeLog/flush/Amendment-1 foundation), ADR 0112 (Method-Level Removal Language Primitive — the primitive-design template `renameTo:`/`renameSelector:to:` follow), ADR 0032 (Early Class Protocol — chain-walk dispatch, and the *limit* of its guarantee once senders carry baked-in selector/class-name text), ADR 0087 (Maintained Selector→Sites Cross-Reference Index — `referencesTo:`/`sendersOf:`, and the `build_method_entry/5` live-patch gap), ADR 0108 (Named Union Type Aliases — `beamtalk_alias_xref.erl`, precedent for the alias alternative), ADR 0066 (Open Class Extension Methods — extension-method attribution rules this ADR's rename logging reuses unmodified), ADR 0038 (Subclass/ClassBuilder Protocol — dynamic-class flushability precedent)
- Documentation: `docs/beamtalk-language-features.md`, `docs/development/surface-parity.md`
- LSP spec: `workspace/applyEdit` with `RenameFile` operations, <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_applyEdit>
