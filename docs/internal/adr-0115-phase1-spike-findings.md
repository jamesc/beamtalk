# ADR 0115 — Phase 1 Validation Spike Findings (BT-3216)

**Status:** complete · **Deliverable:** knowledge, not code.
**Question:** can Phase 2 commit to ADR 0115's `recv_type` schema as written, or
does something load-bearing not hold?

**Evidence artefacts committed** (both are spike instrumentation, not production
code — neither runs in CI):

- `runtime/perf/bench_recv_type_spike.escript` — runs `hierarchy_related_classes/1`,
  `conforms_to/2` and `is_relevant/3` *transcribed verbatim from ADR 0115
  §"Read path"* against a live 109-class stdlib workspace. No schema or runtime
  change; the proposed functions live inside the escript.
  `cd runtime && escript perf/bench_recv_type_spike.escript`
- `crates/beamtalk-core/tests/bt3216_spike_probe.rs` — `#[ignore]`d reporting
  probe measuring `TypeMap` receiver-span coverage across `stdlib/src`.
  `cargo test -p beamtalk-core --test bt3216_spike_probe -- --ignored --nocapture`

Timings below are from an unloaded dev container, reported as the range across
three runs. They are order-of-magnitude evidence, not a regression baseline.

## Headline

| # | Assumption | Verdict | One-line evidence |
|---|---|---|---|
| 1 | `InferredType` reachable at `build_method_xref_list` | **Partial — blocked, but not for the reason the ADR predicted** | Inference already runs on the right module and the result is *discarded*; no second pass is needed. The blocker is that `build_method_xref_entry` walks a **re-unparsed, re-parsed copy** of the method, so its `SendHit`s have no key into a span-keyed `TypeMap`. |
| 2 | `hierarchy_related_classes/1` cheap | **Held, with a scaling caveat** | 474–508 µs for a root-class change, 6–7 µs otherwise — but `direct_subclasses/1` is a full-table `ets:match`, so the closure is **O(N²) in class count**. |
| 3 | `conforms_to/2` cheap enough to call per site | **Broke** | 59–135 µs per call (a `gen_server:call` per ancestor level, per required selector, halting on a hit). All-protocol worst case: `senders_of/2` costs **3.3–16.3 ms** vs `senders_of/1`'s 34–38 µs — a 100–400x regression on the hot-reload path the ADR exists to speed up. |
| 4 | Generation-bump default leaves legacy rows "always relevant" | **Held for the stated case; broke for two adjacent ones** | Missing-field rows correctly default to relevant. But a `recv_type` naming *anything the registries cannot resolve* — including **a protocol whose module is not currently registered** — is silently **excluded**. That is a correctness bug, not a precision gap. |

**Recommendation: Phase 2 may proceed, but not with the schema and read path
exactly as written.** Three amendments are required (§5). None of them
invalidate the ADR's decision; all of them are things this spike existed to
find.

---

## 1 — Is `InferredType` reachable at `build_method_xref_list`?

**Verdict: not today. But the ADR mis-identified the risk.**

ADR 0115 §Implementation warned: *"if `InferredType` proves expensive or
unavailable at the needed pipeline point, the design may need to fall back to
computing types lazily/on-demand at xref-entry-construction time — a real cost
increase."* That fallback is **not needed**. Inference is already run, on the
right module, in the right process, in both compile paths — and thrown away.

### 1a. The inference results already exist and are discarded

**Driver path** (`semantic_analysis/mod.rs:687-705`): `analyze` runs
`TypeChecker::check_module_with_protocols_and_aliases`, lifts
`take_method_return_types()` onto `AnalysisResult.method_return_types`, then
`take_type_map()` into a local, hands it to the `Analyser`, and drops it when
`analyze` returns. `AnalysisResult` (`semantic_analysis/mod.rs:102-161`) has no
`type_map` field. The `Analyser` already exposes it back
(`semantic_analysis/analyser.rs:44`, `fn type_map(&self) -> &TypeMap`).

**Self-sufficient path** (`codegen/core_erlang/mod.rs:944`): codegen calls
`infer_method_return_types`, which internally does a full `check_module` and
returns only the return-type map (`type_checker/mod.rs:337-348`).
**`infer_types_and_returns` already exists** (`type_checker/mod.rs:359-370`) and
returns `(TypeMap, method_return_types)` from that *same single pass* — a
literal drop-in with no extra inference.

**Retention cost is negligible.** `TypeMap` is `HashMap<Span, InferredType>`
with `Span` a `(u32, u32)` byte range. Measured over all 109 `stdlib/src` files:
**6,552 entries total**, ~60 per module.

### 1b. Receiver-span coverage is excellent — this validates the ADR's core bet

Across `stdlib/src` (109 files, 2,218 message-send receiver expressions):

| Receiver's `InferredType` | Count | Share |
|---|---|---|
| present in `TypeMap` at all | 2,196 | **99.0 %** |
| `Known { .. }` — would key `recv_type` | 1,778 | **80.2 %** |
| `Meta { class_name }` — class-object receiver | 303 | 13.7 % |
| `Union` | 53 | 2.4 % |
| `Dynamic(_)` | 62 | 2.8 % |
| `Never` / `Negation` / `Intersection` | 0 | 0.0 % |

80 % of stdlib sends would carry a concrete name rather than the `dynamic`
fallback. This is the number behind the ADR's Steelman claim that inferred
beats declared-annotation-only, and it holds comfortably.

### 1c. The actual blocker: a coordinate-space mismatch, not availability

`build_method_xref_entry` (`codegen/core_erlang/gen_server/methods.rs:2751-2833`)
does **not** walk the original AST. It calls
`extract_method_source(method)` → `unparse::unparse_method` — a *pretty-printed
re-render* of the method — and hands the string to
`find_all_sends_in_source` (`method_source_walker.rs:121-138`), which prefixes
`"Object subclass: __SyntheticAllSendsScope\n"`, **re-lexes and re-parses** it,
and walks that *fresh* AST.

`SendHit` (`method_source_walker.rs:92-106`) carries `selector`, `line`,
`receiver: ReceiverKind`, `target_module` — **no span**. Any span it could
carry would index the synthetic string, not the file. `TypeMap` is keyed by
`Span` = byte offsets into the **original source file**
(`source_analysis/span.rs:11-31`). **There is no join key today.**

Byte-offset arithmetic is not a viable rescue.
`source_analysis/method_span_corpus_tests.rs:550-600` shows the round trip only
reconstructs `disk[span]` after `dedent` → `unparse_method` →
`reindent_method_source` → `match_trailing_newline`, and only for the
formatter-clean stdlib/examples corpus. For user source that is not
`just fmt`-clean, unparse output is not byte-identical to disk at all, so an
offset map would be both fragile and per-line.

### 1d. Exact plumbing Phase 2 needs

1. Add `pub type_map: TypeMap` to `AnalysisResult`. `analyze` already computes
   it (`semantic_analysis/mod.rs:705`) and moves it into the `Analyser`, which
   exposes it back as `type_map(&self) -> &TypeMap` (`analyser.rs:44`). Note
   `analyser.result` is still consumed after that point
   (`check_block_capture_sendability`, then the `block_info`/`diagnostics`
   moves), so recover the map by destructuring the `Analyser` at the end rather
   than by a consuming accessor mid-way.
2. Stash it on `CoreErlangGenerator` via the existing
   `CodegenOptions::with_analysis` channel. Every production driver already
   hands off analysis (`beamtalk-cli/src/beam_compiler.rs:762`,
   `beamtalk-compiler-port/src/main.rs:1621/1696/1957/2207`).
3. In the self-sufficient path (`codegen/core_erlang/mod.rs:944`) swap
   `infer_method_return_types` → `infer_types_and_returns`. Zero extra passes.
4. **The real work — join `SendHit` to a receiver span.** Phase 2 must pick one
   explicitly:
   - **(A) AST-directed receiver walk over the original `&MethodDefinition`
     (recommended).** `build_method_xref_entry` already holds it, with
     file-absolute spans. Add a second, span-carrying pre-order walk and join to
     the existing `find_all_sends_in_source` hits **by pre-order ordinal**.
     Cheap — but it rests on two assumptions, not one: that unparse→reparse is
     pre-order-structure-preserving, *and* that the new walk replicates
     `collect_sends`'s exact traversal, including its cascade expansion (one hit
     per cascade message, receiver recorded before its own subtrees). The join
     must also run before `methods.rs`'s `MAX_ATOM_BYTES` selector filter, which
     drops hits after the walk. Per this repo's
     no-"keep-in-sync"-comment-without-a-test rule, those assumptions need a
     **corpus conformance test** (hit count and selector sequence identical
     between the two walks, over the same corpus
     `method_span_corpus_tests.rs` already uses), not a comment.
   - **(B) Add an optional `receiver_span` to `SendHit`,** populated only on an
     AST-directed invocation. Larger change to a module whose leaf position is
     deliberate (ADR 0115 Constraint 3), and the fallback/live-patch callers
     still have no spans — it converges on the same asymmetry as (A).
   - Not viable: byte-offset remapping (§1c).
5. Keep `method_source_walker` a leaf. The `&TypeMap` join belongs in
   `methods.rs` (codegen), not inside the walker — the walker must not gain a
   `semantic_analysis` dependency.

### 1e. Finding not in the ADR — `Meta{C}` receivers get no rule

13.7 % of stdlib send receivers infer to `InferredType::Meta { class_name }` — a
class-object receiver (`Counter spawn`, `Transcript showLine:`). ADR 0115's
write path enumerates `Known` → name, and "`Union`, `Intersection`,
`Negation`, `Dynamic(_)`, or otherwise unresolved" → `dynamic`. `Meta` lands in
the second bucket by omission, discarding narrowing on the **entire class-side
send population** for no stated reason.

The runtime already has a name for these: `beamtalk_class_registry:class_object_tag/1`
renders `'Foo'` as `'Foo class'`. Phase 2 should decide this deliberately —
it is a schema-shape decision, which is exactly what this spike exists to
de-risk — rather than inherit it silently.

---

## 2 — `hierarchy_related_classes/1` cost

**Verdict: held, but it degrades quadratically and the constant is larger than
"one chain walk plus one closure" suggests.**

Live workspace shape: **109 loaded classes, max ancestor depth 5**, root breadth
`ProtoObject` 108 / `Object` 105 descendants.

The **ancestor** half is genuinely cheap:
`beamtalk_class_metadata:lookup_superclass/1` is a keyed `set` ETS read, ≤5 deep.

The **descendant** half is not. `beamtalk_class_registry:all_subclasses/1`
(`beamtalk_class_registry.erl:670-680`) calls `direct_subclasses/1` once per node
in the subtree; `direct_subclasses/1` calls
`beamtalk_class_metadata:match_subclasses/1`
(`beamtalk_class_metadata.erl:349-375`), which is a **full-table `ets:match/2`**.
The table is a `set` keyed on `#class_metadata.name`
(`beamtalk_class_metadata.erl:126-133`), so the *superclass* column is
unindexed and every call scans all rows.

| Measurement | Result |
|---|---|
| `match_subclasses/1` (one full-table scan) | 4.1–4.7 µs |
| `all_subclasses('ProtoObject')` | 457–495 µs |
| implied scans per closure | **104–110** ≈ one per loaded class |
| `hierarchy_related_classes('ProtoObject')` | 474–508 µs |
| `hierarchy_related_classes(<mid or leaf>)` | 6–7 µs |
| `beamtalk_xref:senders_of/1`, hottest selector (`at:`, 70 sites) | 34–38 µs |
| `senders_of/2` with `ChangedClass = Object` | 536–556 µs |

So a root-class change makes the per-trigger lookup **~15x** today's. In
absolute terms that is fine at today's scale — sub-millisecond, dwarfed by the
~18.5 ms/class compile the filter exists to avoid (ADR 0105 Phase 0 spike). But
the work is **O(N²) in class count**: at 1,000 loaded classes the same code does
~100x the scanning (tens of ms) with no compile-side saving to match.

**For Phase 3:** compute `Related` once per trigger (the ADR already does), and
skip computing it at all when no site carries a nominal `recv_type`. Giving
`beamtalk_class_metadata` a superclass index so `direct_subclasses/1` stops
scanning is out of scope for ADR 0115 but is the change that makes this design
scale — filed as **BT-3221**.

---

## 3 — `conforms_to/2` cost when called per site

**Verdict: broke. This is the finding that most contradicts the ADR.**

ADR 0115 presents the protocol branch as free reuse: *"reuses the already-shipped
runtime conformance check (ADR 0068), not a new algorithm."* Reuse it is; free it
is not.

`beamtalk_protocol_registry:conforms_to/2`
(`beamtalk_protocol_registry.erl:187-212`) is a **structural** check. For every
required instance method (including those inherited via `extending`) it calls
`beamtalk_behaviour_intrinsics:classCanUnderstandFromName/2`
(`beamtalk_behaviour_intrinsics.erl:317-328`), which walks the ancestor chain
calling `beamtalk_object_class:has_method/2` — and `has_method/2`
(`beamtalk_object_class.erl:402-428`) is a **`gen_server:call` to the class
process**. The walk halts at the first level that answers, so the cost is one
`gen_server:call` per ancestor level *until a hit*, per required selector —
which means the **worst case is a class that does not conform**, walking every
selector to the root before returning `false`. There is **no memoisation
anywhere** on this path, and because it is process messaging it also contends
with whatever those class gen_servers are doing during the reload.

| Measurement (70-site selector, `ChangedClass = Collection`) | Result |
|---|---|
| `conforms_to/2`, single call (`Printable`, `JsonRepresentable`) | **59–135 µs** |
| `senders_of/1` today (no filter) | 34–38 µs |
| `senders_of/2`, all rows legacy (no `recv_type`) | 80–83 µs |
| `senders_of/2`, migrated, nominal-skewed types | 69–78 µs |
| **`senders_of/2`, migrated, all rows protocol-typed** | **3,346–16,272 µs** |
| same, with `is_protocol`/`conforms_to` memoised per distinct `recv_type` | **156–530 µs** |

The all-protocol worst case is a **~100–400x regression on a hot-reload path**,
and it scales *linearly with fan-out* — precisely the large-fan-out regime ADR
0115 exists to fix. A large-fan-out, protocol-typed selector would make
`senders_of/2` slower than the compile it is filtering out, inverting the ADR's
entire premise.

Two mitigations, both cheap:

1. **Memoise within the call.** `is_protocol/1` and `conforms_to/2` depend only
   on `(ChangedClass, T)`, and `ChangedClass` is fixed for the whole
   `senders_of/2` call. A fold with a small `#{recv_type => boolean()}` cache
   drops the worst case **20–30x** (measured above), and helps the realistic
   nominal case too (69–78 µs → 47–51 µs). The ADR's `is_relevant/3` as written
   is a stateless predicate inside a list comprehension, re-evaluating per site;
   it should be a fold. **Phase 3 must take this.**
2. **Consider a registry-level conformance cache.** `conforms_to/2` results are
   stable between class reloads. Caching them in the protocol registry, keyed
   `{ClassName, ProtocolName}` and invalidated on class/protocol
   (un)registration, would help every caller — including the class-load-time
   `conformsTo:`/`protocols` queries that already pay this cost. Out of scope
   for ADR 0115; filed as **BT-3222**.

---

## 4 — Generation-bump default fallback

**Verdict: the stated case holds. Two adjacent cases silently narrow, and the
ADR does not cover them.**

Fixture: `is_relevant/3` transcribed verbatim from ADR 0115 §"Read path", run
against the live registry with `ChangedClass = 'Integer'`
(escript §C).

| Site | Result | Correct? |
|---|---|---|
| legacy row, no `recv_type` key | `true` | ✅ as designed (3rd clause) |
| `recv_type => dynamic` | `true` | ✅ |
| `recv_type => 'Integer'` (self) | `true` | ✅ |
| `recv_type => 'Number'` (ancestor) | `true` | ✅ |
| `recv_type => 'Character'` (descendant) | `true` | ✅ |
| `recv_type => 'ThrowError'` (unrelated branch) | `false` | ✅ correctly excluded |
| **`recv_type => 'NoSuchClassXYZ'`** (name unknown to both registries) | **`false`** | ❌ **silently excluded** |
| **`recv_type => 'UnloadedProto'`** (protocol not currently registered) | **`false`** | ❌ **silently excluded** |

The ADR's safe-default reasoning covers rows *missing the field*. It does not
cover rows *carrying a name neither registry can resolve* — `is_relevant/3` as
written routes those to the nominal branch, `sets:is_element(T, Related)` →
`false` → **dropped**. Two realistic ways to land there:

1. **A protocol that is not registered at query time.** `is_protocol/1` reads
   the protocol ETS table, which is populated when the defining module loads
   (and cleared by `unregister_protocol/1`). During a hot reload, before a
   protocol's module has loaded, or after an unregister, a protocol-typed site
   takes the *nominal* branch and is excluded — the exact "silently narrowing a
   real dependent" failure ADR 0115 Constraint 2 forbids. This is a
   **correctness bug, not a precision gap**, and because it depends on module
   load order during a reload it will be **intermittent** and painful to
   diagnose.
2. **`Known { class_name }` names that are not runtime classes.** The type
   checker's `Known` also carries native/FFI type names (`NativeTypeRegistry`,
   ADR 0075) and alias display names (`TypeProvenance::Aliased`, ADR 0108).
   Neither has a `beamtalk_class_metadata` row.

**Required changes:**

- *Phase 3 read path:* invert the default. Exclude only when `T` is **positively
  known** to the class registry and positively unrelated; include whenever `T`
  cannot be resolved. Concretely, add an "unresolvable ⇒ relevant" arm before
  the `sets:is_element/2` fallback.
- *Phase 2 write path:* decide whether the compiler is allowed to emit names
  that are not runtime class or protocol names at all. Recommendation: coarsen
  native- and alias-provenance `Known` to `dynamic` at write time, so the read
  path never has to reason about them.

Two things that *did* check out and need no change:

- The ADR's `maps:get(recv_type, Site, dynamic)` phrasing and its three-clause
  `is_relevant/3` match are equivalent for the missing-field case — no
  discrepancy.
- The generation mechanism itself is sound: `senders_of/1`
  (`beamtalk_xref.erl:449-458`) already filters to each owner's live generation
  via `live_gen_sites/2`, so a re-register genuinely supersedes old rows and no
  distinct backfill pass is needed.

---

## 5 — Verdict for Phase 2

**Proceed — the ADR's decision survives — with three amendments, all of which
this spike exists to have found.**

1. **The `InferredType` plumbing is smaller than feared, but is not the
   plumbing the ADR described.** No lazy/on-demand inference, no second pass:
   add `type_map` to `AnalysisResult` and swap one function call. The real work
   is a **span join** between the type-checked AST and the re-parsed
   `SendHit` stream (§1c–1d) — Phase 2's estimate should be re-anchored on that,
   plus the conformance test that join needs.
2. **`recv_type` must not be able to hold a name the read path cannot resolve,
   and the read path must default unresolvable names to *relevant*** (§4).
   Without this the design has a correctness bug — an intermittent one — of the
   exact kind Constraint 1 declares non-negotiable.
3. **The protocol branch must be memoised per `senders_of/2` call** (§3), and
   `is_relevant/3` becomes a fold rather than a stateless predicate. Without
   this, the protocol-typed worst case is slower than the compile it replaces.

Also decide, rather than inherit: **`Meta{C}` receivers** (13.7 % of stdlib
sends) currently fall through to `dynamic` by omission (§1e).

Two follow-ups filed, both outside ADR 0115's scope but both limiting how far
this design scales:

- **BT-3221** — index the superclass column in `beamtalk_class_metadata` so
  `direct_subclasses/1` stops full-table-scanning (§2).
- **BT-3222** — cache `conforms_to/2` results in the protocol registry,
  invalidated on class/protocol (un)registration (§3).

## References

- ADR 0115 (`docs/ADR/0115-xref-receiver-type-key.md`) — the design under test
- ADR 0087 — the xref schema being extended · ADR 0105 — the reload re-check
  this optimises · ADR 0025 / ADR 0068 — `InferredType`, protocols,
  `conforms_to/2` · ADR 0114 — the sibling hierarchy-closure reuse
- `docs/internal/adr-0105-phase0-spike-findings.md` — the precedent spike (and
  source of the 18.5 ms/class re-check figure)
- Epic BT-2798 · this issue BT-3216 · next phase BT-3217 · follow-ups filed
  here: BT-3221, BT-3222
