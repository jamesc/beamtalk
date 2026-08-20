# ADR 0115: Receiver-Type Key for `senders_of/1` Lookup

## Status
Accepted (2026-08-20)

## Context

### Problem

ADR 0105 (Live Image Re-Checking on Hot Reload) drives its reload dependent-lookup off `beamtalk_xref:senders_of/1` (ADR 0087), which is keyed by **selector name only**. `senders_of: #size` returns every syntactic send of `#size` anywhere in the project, regardless of which class's `size` the sender actually meant to call — a receiver-type filter only happens as a side effect of fully recompiling each candidate caller through the compiler port (`beamtalk_recheck:recheck_owner/6`). For a widely shared selector, that means paying a full compile per candidate just to discover most of them aren't real dependents.

ADR 0105 accepted this and added an interim guard instead of extending the schema: a numeric per-reload cap (`recheck_caller_cap`, default 20, `beamtalk_recheck:apply_cap/2`) that keeps the alphabetically-first N owner classes and drops the rest. `apply_cap/2`'s own doc comment names the limitation directly: candidates are "not relevance-ranked... an over-cap reload always drops the same alphabetically-last owners, every time, regardless of which caller is actually most likely to matter."

BT-2781's fan-out benchmark (`docs/development/benchmarks.md`, "Reload re-check fan-out"; harness `bench_recheck_fanout.escript`) quantified the failure mode: today's real stdlib fan-out is modest (worst selector `delegate`, 23 caller classes, barely over the cap), but a controlled synthetic benchmark with a 10%-real/90%-false-positive split showed that once a selector's true candidate pool reaches 10x the cap, the alphabetic cap silently drops **90% of genuine stale-caller findings**; at 2.5x the cap the loss is already 60%. ADR 0105's own Alternatives section recorded the fix and deferred it: "not implemented now, filed as a proactive (non-blocking) follow-up — BT-2798." This ADR is that follow-up.

### Current State

| Concern | Today |
|---|---|
| `beamtalk_xref_senders` schema | Bag ETS table, `{Selector, Site}` rows. `Site` carries `owner`, `class_side`, `method`, `line`, `recv_kind`, `target_module`, `gen` — no receiver-type field. |
| `recv_kind` | Purely **syntactic**: `self_recv \| super_recv \| erlang_ffi \| other`, set by `recv_to_recv_kind/1` from the compiler's `ReceiverKind` tag. Not a type — a receiver-*expression-shape* classification. |
| `senders_of/1` | Direct ETS keyed lookup on the selector atom — O(callers of that selector), not a table scan. This ADR extends it, not replaces it. |
| Type inference | Exists and is mature (ADR 0025, gradual typing, Implemented Phases 1–2). Produces `InferredType` per expression, including the established `Dynamic(DynamicReason)` sentinel for anything unresolved/ambiguous. Computed in `semantic_analysis/type_checker`, a *separate* compiler stage from the one that builds xref entries. |
| Compile-time xref-entry construction | `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs`'s `build_method_xref_list`/`build_method_xref_entry`, over `SendHit`s from `method_source_walker.rs` — a **pure syntactic AST walker**, deliberately positioned as a dependency-free leaf module below both Language Service and Codegen so neither depends on the other. It does not currently consult type-checker results. |
| Runtime live-patch xref-entry construction | `beamtalk_xref:build_method_entry/5`, invoked for `>>`/`compile:source:` live patches. Has **no type-checker access at all** — it re-parses the patched method's source via the same syntactic `find_all_sends_in_source/1` walker used at compile time, with no semantic-analysis pass. This is the identical infra gap `build_method_entry/5`'s own doc comment already names for `references => []`: "there is no runtime 'all references' walker... the class-reference channel is fully populated only at compile time." |
| Schema evolution | No formal migration mechanism exists or is needed: `beamtalk_xref.erl`'s generation-counter/atomic-install protocol means a class re-populates *all* its rows (any new field included) the next time it goes through `register_class/2` — which every recompile/hot-reload already triggers. Rows lacking a new field default via `maps:get(Field, Site, Default)`, exactly as `recv_kind`/`target_module` are already defaulted for legacy rows (`is_live_gen/3`: "a row with no `gen` field is treated as gen `0`"). |
| `beamtalk_recheck`'s dependent lookup | `do_trigger/4` calls `beamtalk_xref:senders_of(Selector)` directly, groups by `owner`, then applies the cap. No lighter-weight receiver-type pre-filter exists between the xref query and the full-class compiler-port re-check — narrowing today is entirely a byproduct of that expensive step. |

### Constraints

1. **Dispatch chain-walks, so an exact-class match on the receiver's static type is unsound, not just imprecise.** A receiver statically typed `T` may hold any runtime instance of `T` or a subclass of `T` at runtime (Beamtalk's gradual typing narrows to an upper bound, not an exact class). Given a changed class `C` and selector `S`, a send on a `T`-typed receiver can possibly resolve to `C`'s implementation of `S` if and only if `C` and `T` sit on the same ancestor/descendant line — `C == T`, `C` is an ancestor of `T` (some subtype `R` of `T` could inherit `S` from `C` via `T`'s own chain), or `C` is a descendant of `T` (picking runtime type `R = C` itself, a valid subtype of `T`). If `C` and `T` are in unrelated branches of the hierarchy, no valid runtime instance of `T` can ever dispatch to `C`'s method — safe to exclude. A read-path filter that only matched `recv_type =:= C` would silently drop every subclass-inherited dependent — a correctness regression, not a completeness trade-off.
2. **`Dynamic`/unresolved/`Object`-typed receivers cannot be narrowed and must never be excluded.** Anything the type checker didn't resolve to a single concrete class — union results, unannotated locals with ambiguous inference, explicit `Dynamic`, and FFI-shaped receivers — carries no information ruling out any class. These must always stay in the candidate set for every query, exactly as `Dynamic(_)` already means "no narrowing available" throughout the type checker's own lattice operations (`InferredType`'s `PartialEq` "ignores the reason — all `Dynamic` values are equal").
3. **The compile-time xref-entry builder is a deliberately dependency-free leaf module today.** `method_source_walker.rs`'s own module doc explains its leaf position exists specifically so Language Service and Codegen don't depend on each other through it. Threading type-checker results into xref-entry construction is new coupling this ADR must specify precisely (which stage, which representation) rather than wave at — flagged as the Phase 1 spike question (see *Implementation*).
4. **The runtime live-patch path has no type-checker access and this ADR does not add one.** `build_method_entry/5` already accepts an analogous gap for class references (`references => []`) for exactly this reason — no runtime "full semantic analysis of one patched method" walker exists, and building one is a materially larger scope than this ADR takes on. Live-patched sends get the same `dynamic` receiver type as any other unresolved case — a real, accepted asymmetry (compile-time-only precision), not silently generalized.
5. **No new ETS table or compound index is justified.** `senders_of/1`'s existing selector-keyed lookup is already the correctly-scoped entry point (O(callers of that selector)); adding `recv_type` as one more field on the existing `Site` map, filtered in Erlang after the keyed lookup, avoids inventing new index machinery for what a handful of hierarchy-membership checks over an already-small result set can do (see *Decision*).
6. **Reproducible-build guarantee and surface parity are unaffected** — this ADR touches only compile-time codegen (a build-input-dependent, already-covered concern) and Erlang query logic; it adds no new user-facing surface, syntax, or primitive.

## Decision

**Add `recv_type :: name() | dynamic` to the `beamtalk_xref_senders` `Site` schema, populated at compile time from the type checker's already-computed `InferredType` (coarsened to `dynamic` for anything not resolved to exactly one concrete class-or-protocol name), left as `dynamic` for runtime live-patched sends. `name()` covers both nominal classes and structural protocols (ADR 0068) — the type checker resolves both through the same `Known{class_name}` representation, and the runtime already exposes `beamtalk_protocol_registry:is_protocol/1` to tell them apart at read time. Add `senders_of/2`, a receiver-type-aware extension of `senders_of/1` that filters results to sites whose `recv_type` is relevant to the queried class — hierarchy-related (ancestor, descendant, or self) for a class-typed site, protocol-conformant (via the already-shipped `beamtalk_protocol_registry:conforms_to/2`) for a protocol-typed site — or unresolved (`dynamic`); never an exact-match filter, per Constraint 1. Wire `beamtalk_recheck`'s dependent lookup to call `senders_of/2`, keeping the existing numeric cap as a backstop rather than the primary defense.**

### Schema extension

```erlang
-type recv_type() :: name() | dynamic.
-type name() :: class_name() | protocol_name().  %% same atom space; is_protocol/1 disambiguates

-type site() :: #{
    owner := class_name(),
    class_side := class_side(),
    method := selector(),
    line := pos_integer(),
    recv_kind => recv_kind(),
    recv_type => recv_type(),        %% NEW — orthogonal to recv_kind; recv_kind is
                                      %% receiver *syntactic form* (self/super/ffi/other),
                                      %% recv_type is the receiver's *static class or protocol*,
                                      %% when resolved to exactly one name
    target_module => module() | undefined,
    gen := gen()
}.
```

`recv_type` is populated uniformly for every send, independent of `recv_kind` — a `self_recv` site's `recv_type` will typically resolve to its own `owner` (or `dynamic` if `self` isn't narrowed at that point), which duplicates information ADR 0114 already extracts from `owner` for the self/super case. The real value of this field is for `recv_kind: other` sites — arbitrary expression receivers, the case ADR 0114 explicitly reported as unnarrowable `candidate_sites` and flagged as this ADR's job: "extending `beamtalk_xref_senders` with inferred-receiver-type narrowing... is flagged here as follow-up work, not designed by this ADR" (ADR 0114, *Decision*).

### Write path — compile-time only, reusing the type checker's existing `Dynamic` sentinel

`build_method_xref_entry` (`methods.rs`) gains access to the type checker's per-expression `InferredType` results for the method being indexed (exact plumbing — new parameter vs. an already-available shared analysis result — is the Phase 1 spike question below). For each `SendHit`, project its receiver's `InferredType`:

- `Known{class_name, type_args}` — resolves to exactly one class-or-protocol name (the same variant represents both; ADR 0068's protocol names resolve through it identically to class names) → `recv_type: <that name>`, dropping `type_args`. A receiver typed `Collection(Integer)` still gets `recv_type: 'Collection'` — the generic parameter doesn't change which class or protocol the read path needs to reason about (Constraint 1's soundness argument), it only constrains what's *inside* the collection, which this ADR's site-relevance question never depends on.
- `Union`, `Intersection` (e.g. `Collection(Object) & Comparable`, ADR 0068 §Protocol Composition), `Negation`, `Dynamic(_)`, or otherwise unresolved → `recv_type: dynamic`. Composed/intersected types are a real, deliberate v1 simplification, not an oversight — see *Alternatives Considered*.

No new "unknown" vocabulary is introduced — `Dynamic` is the same sentinel ADR 0025 already established project-wide, reused rather than reinvented (per the project's leaf-module/no-duplication rule).

`build_method_entry/5` (the runtime live-patch path) is **not** extended to consult type-checker results — every send it indexes gets `recv_type: dynamic`, unconditionally, matching its own existing `references => []` precedent and for the identical reason (Constraint 4). This is a real, accepted asymmetry: receiver-type narrowing only benefits classes compiled through the normal pipeline, not the interval between a live patch and its next full recompile.

### Read path — `senders_of/2`, hierarchy-related / protocol-conformant / unresolved, never exact-match

```erlang
-spec senders_of(selector(), class_name()) -> [site()].
senders_of(Selector, ChangedClass) ->
    AllSites = senders_of(Selector),                    %% reuses the existing keyed lookup unchanged
    Related = hierarchy_related_classes(ChangedClass),   %% {ChangedClass} ∪ ancestors(ChangedClass)
                                                          %%   ∪ subclasses(ChangedClass), via the
                                                          %%   registry's existing superclass-chain-walk
                                                          %%   and direct_subclasses/1 closure — no new
                                                          %%   hierarchy algorithm
    [S || S <- AllSites, is_relevant(S, ChangedClass, Related)].

is_relevant(#{recv_type := dynamic}, _ChangedClass, _Related) ->
    true;
is_relevant(#{recv_type := T}, ChangedClass, Related) ->
    case beamtalk_protocol_registry:is_protocol(T) of
        true ->
            %% Protocol-typed receiver: any class conforming to T is a possible
            %% runtime type, so this site is relevant iff ChangedClass itself
            %% conforms — reuses the already-shipped runtime conformance check
            %% (ADR 0068), not a new algorithm.
            beamtalk_protocol_registry:conforms_to(ChangedClass, T);
        false ->
            sets:is_element(T, Related)
    end;
is_relevant(#{}, _ChangedClass, _Related) ->
    true.  %% no recv_type field at all (unmigrated legacy row) — safe default
```

`senders_of/1` is unchanged and remains the general-purpose query every existing consumer (`referencesTo:`-adjacent tooling, LSP, `SystemNavigation>>sendersOf:`) keeps using; `senders_of/2` is strictly additive, matching the issue's own framing ("new query or `senders_of/1` extension") and the project's "extend, don't duplicate" rule. Both branches of `is_relevant/3` reuse existing infrastructure rather than inventing new traversals: `hierarchy_related_classes/1` composes the registry's existing superclass-chain-walk and `direct_subclasses/1` transitive closure (the same closure ADR 0114 already reuses); the protocol branch calls `beamtalk_protocol_registry:conforms_to/2`, already shipped and already invoked at class-load time for the runtime `conformsTo:`/`protocols` queries (ADR 0068).

**Why "conforms" is the correct protocol-relevance test, by the same soundness argument as Constraint 1.** A receiver statically typed as protocol `P` may hold, at runtime, any instance of any class that structurally conforms to `P` — that's what protocol typing means (ADR 0068's "Automatic Conformance"). A send on that receiver can possibly resolve to `ChangedClass`'s implementation of the changed selector if and only if `ChangedClass` is one of the classes `P` admits — i.e., `ChangedClass` conforms to `P`. If it doesn't, no valid runtime instance of a `P`-typed variable can ever be a `ChangedClass` instance, so the site is safe to exclude — structurally the same "possible-runtime-type" reasoning as the nominal case, just against protocol conformance instead of the class hierarchy.

### `beamtalk_recheck` integration

`do_trigger/4` and `do_trigger_shape/2` (`beamtalk_recheck.erl`) switch their `beamtalk_xref:senders_of(Selector)` call to `beamtalk_xref:senders_of(Selector, ClassName)`, before `group_by_owner/1` and `apply_cap/2` run. This shrinks the candidate pool the cap ever has to act on, rather than replacing the cap: `dynamic`-typed and cross-hierarchy-ambiguous sends still flow through unchanged, so the cap remains the correct backstop for exactly the residual case it was already designed for (Constraint 2) — this ADR narrows the common case, it doesn't eliminate the guard.

### Migration — reuses the existing generation-bump mechanism, no new pass

Per Constraint's precedent (`beamtalk_xref.erl`'s generation-counter/atomic-install protocol), no distinct backfill module or migration script is needed:

- A class's rows pick up populated `recv_type` values the next time it's compiled/recompiled through `register_class/2` — which every hot-reload already does.
- Classes that haven't reloaded since this ships keep emitting rows with no `recv_type` field; `is_relevant/3`'s third clause defaults those to "always relevant," identically to how `recv_kind`/`target_module` are already defaulted for legacy rows. This is a **safe** default in exactly the sense Constraint 2 requires — a stale row never gets silently excluded, only fails to benefit from narrowing until its owner next reloads.
- `beamtalk_xref_senders`'s row *shape* changes (one more optional map key); its key structure (`{Selector, Site}` bag) does not, so no ETS table recreation or `register_class/2` call-site signature change beyond the payload's own new field is required.

## Prior Art

### Erlang/BEAM — Dialyzer success typing

Dialyzer's success typing is exactly this ADR's shape already accepted as idiomatic on this VM: best-effort, unsound-by-design inference that narrows without ever claiming completeness, falling back permissively rather than refusing to analyze when it can't resolve a type. **Adopted:** the same posture — `dynamic` is a permissive fallback, never a hard refusal, and this ADR makes no soundness claim beyond "narrows the common case." **Not adopted:** Dialyzer surfaces its findings as warnings to a human; this ADR's output feeds an automated filter decision (include/exclude a recheck candidate) with an existing, independent safety net (the cap) behind it — a stricter consequence for a wrong inclusion than a Dialyzer warning has, which is why Constraint 1's soundness (never wrongly *exclude*) is non-negotiable even though the *inclusion* side stays permissive.

### Gleam — full static typing on BEAM

Gleam's compiler-verified, non-gradual type system would make an exact-match `recv_type` filter sound outright — there's no `Dynamic` case to worry about. **Not adopted:** Beamtalk's typing is gradual by ADR 0025's own design (structural, optional, `Dynamic`-defaulting on ambiguity), so this ADR cannot borrow Gleam's confidence; every design choice here (the ancestor/descendant relatedness test, the `dynamic`-always-included rule) exists specifically because Beamtalk sits between Gleam's soundness and Smalltalk's total dynamism, not at either end.

### TypeScript's semantic reference graph vs. Python's AST/text-pattern tooling

ADR 0114 already drew this comparison for `renameSelector:to:`'s `sendersOf:`-narrowing problem and reached the same structural conclusion this ADR reaches independently: Beamtalk's posture is closer to Python's (`rope`) than TypeScript's (`tsserver`) — best-effort against a real but incomplete index, with an accepted, documented gap for what the index can't see (there, dynamic dispatch/`perform:`; here, `Dynamic`-typed and live-patched receivers). **Adopted:** this ADR is a small step *toward* TypeScript's soundness for the subset of code the type checker actually resolves, without claiming to close the gap ADR 0114 already named as structural to a gradually-typed, dynamically-dispatched language.

## User Impact

### Newcomer
No visible surface change — `senders_of:`/`SystemNavigation>>sendersOf:`'s public behavior is unchanged; this only affects internal reload re-check latency and completeness. A newcomer who hits the "N callers not checked" cap note today will see it less often for well-typed code, with no new concept to learn.

### Smalltalk developer
Nothing here departs from Smalltalk semantics — dispatch, chain-walking, and `senders_of:`'s meaning are all unchanged. The type-driven narrowing is invisible unless they inspect `beamtalk_recheck`'s internals.

### Erlang/BEAM developer
Reinforces an idiom already established by Dialyzer: a best-effort static analysis layered permissively over a fundamentally dynamic runtime, with a hard-cap safety net kept rather than removed. No new OTP pattern.

### Production operator
Reload re-check accuracy improves for typed code without any operator-visible configuration change; `recheck_caller_cap` remains the same tunable it already is, now protecting a narrower, more relevant candidate set.

### Tooling developer (LSP/MCP/browser)
No new surface. `senders_of/2` is an internal query, not exposed as a new Beamtalk-facing primitive — `SystemNavigation>>sendersOf:` keeps its current signature.

## Steelman Analysis

### Inferred type (this ADR's choice) vs. declared-annotation-only

- 🧑‍💻 **Newcomer:** "Declared-only would mean I never have to wonder whether some invisible inference step decided my receiver's type — what I wrote is what gets indexed."
- 🎩 **Smalltalk purist:** "Explicit-annotation-only keeps the mechanism legible without reaching into a separate compiler stage — simpler mental model, closer to how Smalltalk tooling historically avoided static-analysis dependencies entirely."
- ⚙️ **BEAM veteran:** "Declared-only needs zero new coupling between codegen and semantic_analysis — lower integration risk, smaller diff."
- 🏭 **Operator:** "A narrower, more predictable mechanism (declared-only) is easier to reason about when something goes wrong in production tooling."
- 🎨 **Language designer:** "Declared-only is the more conservative, easier-to-verify starting point — inferred can always be added later without a schema change, since the field already exists."
- **Why inferred wins anyway:** ADR 0025 makes assignment-site *inference* the default typing mechanism — `x := Counter spawn` narrows `x` to `Counter` with no annotation at all — and explicit `::` annotations are the exception, used "for precision" on top of inference, not the primary source of type information. A declared-only key would leave the overwhelming majority of real sends unresolved (`dynamic`), which defeats the actual goal: BT-2781's benchmark shows the failure mode manifests precisely at the fan-out scale ordinary, mostly-unannotated code produces. This isn't a case of two reasonable choices — declared-only doesn't solve the problem this ADR exists to solve.

### Hierarchy-related-or-unresolved filter (chosen) vs. exact-class-match filter

- 🧑‍💻 **Newcomer:** "Exact-match is simpler to explain: 'my receiver's type has to be exactly the changed class.'"
- 🎩 **Smalltalk purist:** "Simpler filters are easier to trust, and Smalltalk tooling has always favored transparency over cleverness."
- ⚙️ **BEAM veteran:** "Exact-match is a straight `=:=` comparison — cheaper than a hierarchy-closure computation, and cheap matters in a hot reload path."
- 🏭 **Operator:** "Fewer moving parts in a filter that gates correctness-sensitive recheck behavior is safer to operate."
- 🎨 **Language designer:** "A simpler predicate is easier to formally verify."
- **Why hierarchy-related wins decisively, not just defensibly:** this isn't a precision/cost trade-off — exact-match is **unsound** under chain-walk dispatch (Constraint 1). A receiver statically typed as a subclass of the changed class, or as an ancestor whose runtime instance happens to be the changed class, is a real dependent that exact-match would silently drop — turning a *completeness* gap (today's already-accepted category, e.g. `Dynamic`-typed sends) into a *correctness* gap (a `self`-typed, well-inferred site that's actually affected, wrongly excluded). The "simpler is safer" argument inverts once dispatch semantics are accounted for: the closure computation *is* the correct predicate, not an elaboration on it, and it costs one ancestor-chain walk plus one existing `direct_subclasses/1` closure per recheck trigger — bounded by hierarchy depth/breadth, not by candidate-set size.

### Tension points

- **Coverage ceiling is real and not fully resolved:** live-patched sends and `Dynamic`-typed/`other`-kind sends against widely-shared, commonly-implemented selectors (`printOn:`, `size`) get no narrowing benefit at all — the cap remains load-bearing for exactly that residual case, honestly scoped in *Consequences* rather than implied away.
- **New codegen ↔ semantic_analysis coupling is a genuine implementation risk**, not a design tension between cohorts — flagged as the Phase 1 spike, not resolved by this document.

## Alternatives Considered

### Do nothing — keep the alphabetic cap as the permanent mechanism

Already evaluated and explicitly deferred (not rejected) by ADR 0105 itself, pending exactly the fan-out evidence BT-2781 then produced. Rejected now because that evidence exists: a quantified 90%-loss failure mode at 10x the cap, with flat per-check cost meaning the fix is a pure win with no offsetting scaling cost once the threshold is crossed.

### Recheck-time syntactic heuristic instead of a persisted schema extension

Considered: reparse each candidate's call site for an obvious type annotation or class-literal pattern at recheck time, instead of indexing receiver type at compile time. Rejected: this re-derives, per reload, information the type checker already computes once at compile time — strictly more expensive per trigger than a persisted field, with no persistent index to short-circuit repeated reloads of the same unchanged code, and it doesn't satisfy the issue's own acceptance criteria (extend ADR 0087's schema, extend the write path) — it would leave `senders_of/1` exactly as imprecise as today for every other consumer of the xref index, not just `beamtalk_recheck`.

### Precise composed/intersected-type keying (index `Collection(Object) & Comparable`-style intersections precisely, instead of coarsening to `dynamic`)

An earlier draft of this ADR rejected protocol-aware keying entirely, on the premise that ADR 0025 Phase 3 (Protocols) was still unshipped. That premise was wrong: Phase 3 shipped under [ADR 0068](0068-parametric-types-and-protocols.md) some time ago (`Protocol define:`, structural conformance, `beamtalk_protocol_registry:conforms_to/2` — all live, and already invoked at class-load time for the runtime `conformsTo:`/`protocols` queries). Single-protocol-typed receivers are therefore **not** deferred — see *Decision*, which keys and narrows them with the same precision as nominal classes, reusing `conforms_to/2` rather than inventing anything.

What genuinely remains deferred is narrower: `InferredType::Intersection` (e.g. `Collection(Object) & Comparable`, ADR 0068's protocol-composition syntax) and `Union` both coarsen to `recv_type: dynamic` in this ADR's v1 (see *Write path*), rather than being stored and matched precisely. Considered storing the full member set and requiring conformance to *all* of them (or membership in the union), which would narrow further than single-name coarsening does. Rejected for v1: it's a real, bounded schema change (a list-valued `recv_type` variant instead of a single atom, plus a set-comparison read path) for a narrower slice of real code than the single-name case already covers — most receivers resolve to one class or protocol, not an explicit intersection or union annotation. Coarsening to `dynamic` here is the same safe, over-inclusive fallback this ADR already uses for every other unresolved case (Constraint 2) — it costs precision, never correctness. Flagged as natural follow-up if usage data shows composed-type receivers are common enough to matter, not designed here.

## Consequences

### Positive

- Closes ADR 0105's explicitly flagged follow-up with the quantified fan-out benchmark (BT-2781) as direct justification, rather than a speculative optimization.
- Makes `beamtalk_recheck`'s candidate filtering sound-by-construction for typed code instead of relying entirely on an acknowledged non-relevance-ranked cap.
- Covers protocol-typed receivers with the same precision as nominal classes, not just as a deferred follow-up — `beamtalk_protocol_registry:conforms_to/2` (ADR 0068) was already shipped and already invoked at class-load time, so this is reuse, not new scope.
- Reuses five pieces of already-shipped infrastructure rather than inventing new ones: the type checker's `Dynamic` sentinel, the generation-counter migration mechanism, the registry's ancestor-chain-walk and `direct_subclasses/1` closure (the same closure ADR 0114 already reuses for its own hierarchy reasoning), and `beamtalk_protocol_registry:conforms_to/2`.
- `senders_of/1` and every existing consumer are untouched — purely additive.

### Negative

- **Compile-time xref-entry construction gains a new dependency on type-checker results** — real new coupling this ADR does not fully specify (exact plumbing is the Phase 1 spike); downstream implementation carries genuine integration risk here.
- **Runtime live-patched sends never benefit** — `recv_type: dynamic` permanently for the live-patch path, the same asymmetry already accepted for `references => []`, but now a second instance of it rather than a one-off.
- **`other`-kind sends against widely-shared, `Dynamic`-typed protocol selectors get no narrowing** — the cap remains load-bearing for exactly this residual case; this ADR reduces but does not eliminate the failure mode BT-2781 quantified.
- **The hierarchy-relatedness computation is a new per-trigger cost** beyond today's plain keyed lookup — expected small (bounded by hierarchy size, not candidate count) but must be verified, not assumed, against `bench_recheck_fanout.escript` before shipping.
- **Legacy (unmigrated) rows degrade gracefully but silently** to "always relevant" until their owning class next reloads — correct, but means the narrowing benefit doesn't apply uniformly from day one across a large existing codebase; a class that never reloads keeps its old, unnarrowed fan-out indefinitely.

### Neutral

- No new ETS table, no compound index, no change to `beamtalk_xref_senders`'s key structure — `recv_type` is one additive map field.
- Does not touch `referencesTo:`/class-reference indexing (ADR 0114's concern) — scoped strictly to `senders_of`, matching BT-2798's stated scope.

## Implementation

*(For downstream implementation work — this ADR does not implement any of the below.)*

### Affected components

| Layer | Change |
|---|---|
| `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs` | `build_method_xref_entry`/`build_method_xref_list` gain access to per-send `InferredType` results and emit `recv_type` in the generated xref map literal, alongside the existing `recv_kind`. |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/` | No behavior change — this ADR *consumes* already-computed `InferredType` results; if they are not already retained/accessible at the xref-entry-construction point in the pipeline, the Phase 1 spike determines what minimal plumbing exposes them without re-running inference. |
| `runtime/apps/beamtalk_runtime/src/beamtalk_xref.erl` | `site()` type gains `recv_type`; new `senders_of/2`; new `hierarchy_related_classes/1` helper composing existing registry ancestor-walk + `direct_subclasses/1`; `is_relevant/3` branches on `beamtalk_protocol_registry:is_protocol/1` and calls `beamtalk_protocol_registry:conforms_to/2` for protocol-typed sites (both already shipped, ADR 0068 — no new dependency). `build_method_entry/5` (runtime live-patch path) explicitly emits `recv_type: dynamic` for every send, unchanged otherwise. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_recheck.erl` | `do_trigger/4`/`do_trigger_shape/2` call `senders_of/2` instead of `senders_of/1`, passing the changed class name. `apply_cap/2` unchanged — remains the backstop over the now-narrower candidate set. |
| `runtime/perf/bench_senders_xref.escript`, `runtime/perf/bench_recheck_fanout.escript` | Extended to exercise `senders_of/2` and confirm (a) no regression to `senders_of/1`'s existing profile, (b) the BT-2781 synthetic 90%-loss scenario is fixed for typed candidates. |
| `docs/ADR/0087-maintained-xref-index-for-system-navigation.md` | Schema note: `Site` gains `recv_type`. |
| `docs/ADR/0105-live-image-recheck-on-reload.md` | Alternatives section updated to reflect implementation, per that ADR's own note. |

### Phased rollout

| Phase | Scope | Effort | Tests |
|---|---|---|---|
| **1** | **Validation spike**: confirm whether `InferredType` results are already accessible at `build_method_xref_list`'s call site without re-running inference, and at what cost if not; confirm `hierarchy_related_classes/1`'s ancestor+descendant closure cost against real stdlib hierarchy depth/breadth; confirm the generation-bump default-fallback correctly leaves unmigrated rows "always relevant" rather than silently narrowing them. This is the load-bearing new assumption, in ADR 0114 Phase 1's spirit. | M | Spike report; no code change gated on it yet. |
| **2** | If the spike holds: schema extension (`recv_type` field) + compile-time write path only (runtime live-patch path ships `dynamic` unconditionally from the start, no separate phase needed for it). | M | EUnit: `recv_type` correctness against a small fixture graph (typed local, protocol-typed local, untyped/`Dynamic` local, `Union`/`Intersection`-typed local, self-send, FFI receiver) at both instance and class side. |
| **3** | `senders_of/2` read path + `hierarchy_related_classes/1` + protocol-conformance branch. | M | EUnit: relatedness predicate against a fixture hierarchy — same class, direct subclass, transitive subclass, direct/transitive ancestor, unrelated sibling branch, `dynamic` always-included; separately, protocol-conformance predicate against a fixture with a conforming class, a non-conforming class, and a class conforming via inheritance from `Object`'s default methods. |
| **4** | Wire `beamtalk_recheck:do_trigger/4`/`do_trigger_shape/2` to `senders_of/2`. | S | Integration test: a synthetic fan-out fixture mirroring BT-2781's benchmark shape (10% real/90% false-positive by unrelated type) shows the false-positive share dropping out of the candidate set pre-cap. |
| **5** | Benchmark validation + docs update. | S | `bench_senders_xref.escript`/`bench_recheck_fanout.escript` show no regression on `senders_of/1` and a measured improvement on the BT-2781 synthetic scenario; ADR 0087/0105 doc updates land. |

Total: ~M across 5 phases — materially smaller than ADR 0114, since this ADR adds a query filter over already-existing infrastructure rather than a new mutating primitive with cross-file rewrite/atomicity concerns. Phase 1 is still the load-bearing risk: if `InferredType` proves expensive or unavailable at the needed pipeline point, the design may need to fall back to computing types lazily/on-demand at xref-entry-construction time rather than assuming they're already there — a real cost increase, not a design change, so worth confirming before Phase 2 commits to the schema shape above.

## Implementation Tracking

**Epic:** BT-2798
**Issues:** BT-3216 (Phase 1 spike) → BT-3217 (Phase 2 schema/write path) → BT-3218 (Phase 3 read path) → BT-3219 (Phase 4 recheck integration) → BT-3220 (Phase 5 benchmarks/e2e/docs), sequentially blocked in that order. BT-3215 (precise `Union`/`Intersection` keying) tracked separately, deliberately out of this epic's scope.
**Status:** Planned

## References
- Related issues: BT-2798 (this ADR), BT-2781 (the fan-out benchmark and non-blocking-follow-up decision that produced this issue), BT-2778 (the re-check orchestration this optimises), BT-2780 / BT-2782 / BT-2783 (remaining ADR 0105 phases — not blocked on this ADR)
- Related ADRs: ADR 0105 (Live Image Re-Checking on Hot Reload — the direct predecessor and origin of this follow-up), ADR 0087 (Maintained Selector→Sites Cross-Reference Index — the schema this ADR extends), ADR 0025 (Gradual Typing and Protocols — `InferredType`/`Dynamic` sentinel this ADR reuses), ADR 0068 (Parametric Types and Protocols — shipped `Protocol define:`/structural conformance/`beamtalk_protocol_registry:conforms_to/2` this ADR's protocol branch reuses directly, and the source of the `Union`/`Intersection` composed-type shapes this ADR coarsens to `dynamic` in v1), ADR 0114 (Class and Method Rename in the Live Workspace — the sibling problem this ADR was explicitly flagged as follow-up work for, and the direct source of the hierarchy-closure/`direct_subclasses:` reuse pattern)
- Documentation: `docs/development/benchmarks.md` ("Reload re-check fan-out" section, BT-2781's data)
