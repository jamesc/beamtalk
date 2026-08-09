# ADR 0111: Mid-Level Lowered IR + Verifier for State Threading, Control Flow, and Non-Local Return

## Status
Proposed (2026-08-09)

## Context

### Problem statement

The 2026-08 compiler pipeline architecture review (reflected in this issue and
its siblings [BT-3111](https://linear.app/beamtalk/issue/BT-3111) and
[BT-3112](https://linear.app/beamtalk/issue/BT-3112); there is no standalone
document for the review itself — it exists only as the shared rationale cited
by these three issues) identified the AST→Core-Erlang-text gap as Beamtalk's
main divergence from state-of-the-art compiler shape. All of the semantically
hard lowering — actor/instance state threading (`State`/`State1`/`State2`
versioning), class-variable threading (`ClassVars`/`ClassVars1`/…),
value-type field threading (`Self`/`Self1`/…), non-local-return (NLR)
lowering, and the `{Value, StateAcc}` calling convention (ADR 0041) —
happens **interleaved with `Document` construction** (the ADR 0089
pretty-printer tree), coordinated through a handful of scattered
`debug_assert!`s that compile out of release builds and a set of
independently-implemented decisions that must agree with each other by
convention, not by construction.

This is not a hypothetical risk. [ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md)
is a real, shipped bug in exactly this class: a class method that mutated a
class variable and then handed a block to another method lost the mutation
when the block's `^` escaped through library code, while still returning the
*correct value*. The generated Core Erlang was well-formed the entire time —
every variable bound, every arity correct — so `core_lint` (which runs
unconditionally on every compile; see below) passed it without complaint.
The bug was purely semantic: the wrong-but-validly-typed `ClassVars` value
flowed to the reply. That is precisely the class of bug a structural linter
cannot see and a lowered IR + verifier is designed to catch mechanically,
before the code ships, not after a user reports silently-wrong behavior.

### Current state, as measured

BT-3122 estimated the in-scope surface as "the `control_flow/` cluster
(4.4K LOC), `threaded_expr.rs`, and the state-versioning parts of
`gen_server/methods.rs` (5.8K LOC)," guarded by "~16 scattered
`debug_assert!`s" inside a "~90-field `CoreErlangGenerator`." Direct
measurement during this ADR's research corrects all three figures — the
underlying problem is the same, but the ADR should proceed from accurate
numbers:

| Claimed | Measured | Detail |
|---|---|---|
| `control_flow/` cluster: 4.4K LOC | **16,329 LOC total** (≈11,264 excluding inline/separate test modules) across 13 files | 4.4K matches `control_flow/mod.rs` alone (4454 lines); the cluster also includes `conditionals.rs` (1127), `counted_loops.rs` (617), `dict_ops.rs` (641), `exception_handling.rs` (1272), `while_loops.rs` (1248), and `list_ops/` (6 files, ≈4310 lines led by `transform_ops.rs` at 2074 and `search_ops.rs` at 917) |
| `threaded_expr.rs`: implied to carry significant complexity | **416 LOC** | Confirmed small — it is a boundary *adapter* (`ThreadingBoundary::{ValueType, ClassMethod, Actor}`, `ThreadedExpr`), not where the bulk of state-threading logic lives. That logic (the `ThreadingPlan`/`StateAccFallbackReason` mode selection) is in `control_flow/mod.rs` |
| "~16 scattered `debug_assert!`s" | **6 in the named in-scope files** (29 repo-wide in `codegen/core_erlang/`, but 23 are arity/argument-count checks in `intrinsics.rs`/`erlang_types.rs` — expression/primitive codegen, explicitly out of scope per BT-3122's own "that's not where the failures are") | Full 6: `while_loops.rs:317`, `while_loops.rs:454`, `control_flow/mod.rs:2522`, `control_flow/mod.rs:2651` (all four assert an *optimized threading mode implies no `StateAcc` unpack code* — the same invariant, independently asserted at four call sites instead of centralized once); `gen_server/methods.rs:1258`, `gen_server/methods.rs:1450` (both assert that `classify_body_expr`'s upfront classification and `threaded_expr.rs`'s downstream recognizer *agree* on whether a construct routes through the Actor threaded emitter) |
| "`CoreErlangGenerator` ~90 fields" | **36 fields** on `CoreErlangGenerator` (`mod.rs:1288-1481`), ≈39 including its two state-carrying sub-structs (`VariableContext`, `StateThreading`) | Still large enough to be the "coordinates everything" bottleneck the issue describes — just not 90 |

None of these corrections weaken the case for this ADR; if anything the
`debug_assert!` finding sharpens it: **every one of the six in-scope
assertions is a "two independently-computed decisions must agree" check**,
not an arity or bounds check. That is exactly the shape of invariant a
verifier over a shared IR replaces with a structural guarantee instead of a
runtime spot-check that six people (or six future PRs) have to remember to
keep synchronized.

### The duplication this ADR's IR would also close

Research for this ADR surfaced a second, previously undocumented instance of
the CLAUDE.md/`architecture-principles.md` §6 duplication anti-pattern
("layer X can't depend on layer Y" leading to independent reimplementation
instead of a shared leaf module — see
[`docs/development/architecture-principles.md`](../development/architecture-principles.md)
§6): **three structurally identical, independently implemented
monotonic-version-counter services**, all producing `Prefix`, `Prefix1`,
`Prefix2`, … names via the same `util::versioned_var(prefix, version)`
helper, but tracked as three separate fields with three separate
save/restore disciplines:

| Counter | Type | Where | Produces |
|---|---|---|---|
| Actor/instance state | `StateThreading` struct (`state_codegen.rs:42`) | `CoreErlangGenerator.state_threading` field | `State`, `State1`, `State2`, … (renamed to `StateAcc*` inside loop bodies) |
| Class variables | `usize` field + `next_class_var()` (`mod.rs:1132`, `mod.rs:2088`) | `ClassContext.class_var_version` | `ClassVars`, `ClassVars1`, … (ADR 0110's mechanism) |
| Value-type fields | `usize` field + `next_self_var()` (`mod.rs:1211`, `mod.rs:2104`) | `ValueTypeContext.self_version` | `Self`, `Self1`, … |

`with_branch_context` (`mod.rs:2064-2080`, BT-1449/BT-1550) already has to
save, reset, and restore `state_version` *and* `class_var_version` together
whenever codegen enters a branch — but not `self_version`, because that
coupling is maintained by a developer remembering to update the function,
not by a shared type. (A fourth, unrelated "State" name,
`_BuilderStateN` in `gen_server/methods.rs:2514`'s `build_builder_state_doc`,
is a per-class positional index with no relation to mutation-version
threading at all — worth flagging so a future IR design doesn't conflate
it with the other three.) This ADR's lowered IR gives these three counters
one shared representation (a `VersionedVar` type — see Decision) instead of
three parallel structs that must be kept in sync by convention.

### What already exists and does *not* need to change

- **`core_lint`** (BT-3115) already runs unconditionally as part of every
  `compile:forms(Forms, [from_core | Opts])` call — independent of the
  `clint`/`no_lint` options, which only gate a different code path
  (compiling from Erlang *source*, which Beamtalk never does). It reliably
  catches unbound-variable and duplicate-variable well-formedness bugs
  (`docs/development/debugging.md` §"core_lint (BT-3115)"). It cannot and
  structurally will not catch semantic bugs like ADR 0110's — the generated
  code was well-formed. This ADR's verifier operates one layer earlier, on
  Beamtalk's own lowering decisions, and is explicitly a complement to
  `core_lint`, not a replacement.
- **`core_erlang_validity_tests.rs`** (part of BT-3112's epic) is a
  proptest suite over a fixed 20-element `FRAGMENTS` array checking three
  purely textual properties on rendered Core Erlang: balanced delimiters,
  module-name match, and absence of `Debug`/`Display`-format artifacts
  (the BT-875 class ADR 0089 closed). None of the three inspect
  state-threading correctness, NLR semantics, or variable-binding
  provenance — they cannot, because by the time text is rendered the
  structure this ADR's verifier needs (which version bound which, which
  mutation is reachable from which relay) no longer exists as data.
- **`document/` (ADR 0089)** is the closest existing precedent for
  "typed structure with its own discipline," but it solves a different
  class of bug by a different method: it makes the BT-875 string-escape
  vector *unrepresentable* (no `Document::String` variant exists to
  misuse), rather than building a checkable structure and running a
  separate verification pass over it. A grep for `Verifier`/`verify_ir`/
  `fn verify(` across `crates/beamtalk-core/src` returns nothing — there is
  no existing verifier-pattern precedent in this codebase. This ADR
  introduces one.

### Constraints

- **Narrow scope, as specified by BT-3122.** The lowered form covers only
  the control-flow + state-threading core: the `control_flow/` cluster
  (`conditionals.rs`, `counted_loops.rs`, `dict_ops.rs`,
  `exception_handling.rs`, `while_loops.rs`, `list_ops/`), `threaded_expr.rs`,
  and the state-versioning subset of `gen_server/methods.rs` (measured at
  roughly 1900–2400 of its 5805 lines — `BodyExprKind` classification,
  `generate_body_exprs_with_reply`, the tier-2/mutation-threading helpers,
  and the class-method `ClassVars` threading path; the remaining ~60% of
  the file is class/module registration scaffolding and ADR 0068 runtime
  reflection metadata that is not in scope). Expression and primitive
  codegen (`expressions.rs`, `intrinsics.rs`, `dispatch_codegen.rs`,
  `value_type_codegen.rs`, …) stays AST-directed.
- **Explicit non-goals** (BT-3122): no full-pipeline IR covering all of
  Core Erlang codegen — this was already considered and rejected once, as
  "Typed Core Erlang IR," in [ADR 0018](0018-document-tree-codegen.md)
  §Alternatives Considered #3, for being "over-engineered for our needs —
  we don't transform or optimize the IR, we just emit it." That reasoning
  still holds for the 90% of codegen this ADR does not touch. No cerl-wire
  change — [ADR 0088](0088-direct-cerl-emission.md) (Rust↔BEAM transport
  format) stays closed/withdrawn; this ADR's IR is entirely internal to
  `beamtalk-core` and never reaches the Port. No behavior change — every
  phase must produce byte-identical `Document`/`.core` output on the
  existing codegen snapshot suite, the same discipline ADR 0089's Phase B
  used.
- **The `Document`/text printer is unchanged.** The typed-leaf API (ADR
  0089) remains the only path from a lowered leaf to Core Erlang text. This
  ADR's IR sits *between* the AST and `Document` construction, not
  alongside or in place of it.
- **Coordinate with BT-3111/BT-3125, don't block on them.** BT-3125
  (currently In Progress) is moving the `apply_return_type_writeback`/
  `apply_supervisor_kind_writeback`/`apply_class_kind_writeback` trio out of
  codegen's discretion and into a driver-level `lower_module_for_codegen(&mut
  module, &analysis)` preparation step, explicitly leaving a note in its own
  issue: *"if the BT-3122 ADR is accepted, this preparation step is where it
  slots in — keep the seam clean."* This ADR's lowering entry point is
  designed to consume that prepared AST + threaded `AnalysisResult` once
  BT-3125 lands, rather than re-deriving semantic facts locally (the same
  anti-pattern BT-3111 is closing project-wide). Until BT-3125 ships, this
  ADR's Phase A/B work (IR types, verifier, migrating `while_loops.rs`/
  `counted_loops.rs`) does not depend on it — only the final wiring point
  (Phase E) does.

## Decision

**Introduce a small, narrowly-scoped lowered IR — `ThreadedIr` — covering
state-version bindings, threading-mode selection, and NLR relay
boundaries, plus a verifier that checks it before it reaches `Document`
construction.** Everything else in codegen (expressions, primitives,
dispatch) stays AST-directed and unaffected.

### Pipeline shape

```
Beamtalk AST
    │  (post BT-3125: lower_module_for_codegen(&mut module, &analysis))
    ▼
AST, prepared with threaded AnalysisResult / SemanticFacts
    │  (this ADR: lower_control_flow — control_flow/, threaded_expr.rs,
    │   and the state-versioning slice of gen_server/methods.rs only)
    ▼
ThreadedIr                      ◄── verify(&ThreadedIr) -> Vec<VerifyError>
    │  (unchanged: typed-leaf Document construction, ADR 0089)
    ▼
Document
    │  (unchanged: Wadler-Lindig pretty-printer, ADR 0018)
    ▼
Core Erlang text ──► core_lint (unconditional, OTP) ──► BEAM
```

Expression/primitive codegen continues to call directly into `Document`
construction as it does today; it never touches `ThreadedIr`. The lowered
form exists only for the constructs that already require multi-step
state-threading decisions — loops, conditionals with mutation, list-ops,
actor/class-method/value-type field mutation, and NLR relay.

### The IR

```rust
/// One of the three (formerly independent) monotonic version counters,
/// unified into a single representation. Replaces StateThreading's
/// ad-hoc `State{N}`, ClassContext::class_var_version's `ClassVars{N}`,
/// and ValueTypeContext::self_version's `Self{N}`.
struct VersionedVar {
    prefix: VersionPrefix,   // State | ClassVars | Self
    version: usize,
}

enum ThreadingMode { DirectParams, TupleAcc, Hybrid, StateAcc(StateAccFallbackReason) }

enum ThreadedStmt {
    /// A mutation: binds a fresh version from a prior one.
    /// `op` is Put { key, value } | Merge { .. } | Identity — never raw text.
    Bind { target: VersionedVar, source: VersionedVar, op: BindOp },

    /// A loop or mutation-carrying conditional, with its selected mode
    /// already resolved (by the existing ThreadingPlan logic, moved to
    /// run once during lowering instead of being re-derived at emission).
    Threaded { mode: ThreadingMode, body: Vec<ThreadedStmt>, produces: Vec<VersionedVar> },

    /// A non-local-return boundary — the throw/catch scaffolding's meaning,
    /// not its Document rendering. `carries` names the version that must be
    /// reachable from every mutation made before this point in the same frame.
    NlrRelay { boundary: NlrBoundary, token: TokenId, carries: VersionedVar },

    Return(ValueRef, VersionedVar),
}
```

`NlrBoundary` (`ActorReply | ClassMethod { has_class_vars: bool } | ValueType`)
is not new — it is the existing enum from `mod.rs:902-911`, reused as-is;
this ADR gives it a home in the IR instead of only existing implicitly in
the try/catch-emission code path.

### The verifier

```rust
enum VerifyError {
    /// Catches the unbound-`StateX` bug class — mechanically, before core_lint,
    /// with a Beamtalk-source-attributable diagnostic instead of a raw
    /// "unbound variable 'State3' in myMethod/2" from erlc.
    UnboundVersion { var: VersionedVar, at: Span },

    /// Every VersionedVar must be produced by exactly one Bind and consumed
    /// by exactly its immediate successor — catches reuse/skip bugs in the
    /// hand-written version-counter bookkeeping (the class of bug the three
    /// duplicated counters above are structurally prone to).
    NonLinearVersion { var: VersionedVar, producers: usize, consumers: usize },

    /// Replaces the four "unpack should emit no code" debug_asserts:
    /// an optimized ThreadingMode (DirectParams/TupleAcc/Hybrid) is
    /// structurally forbidden from containing an unpack Bind.
    ThreadingModeUnpackMismatch { mode: ThreadingMode, at: Span },

    /// The ADR-0110 bug class, generalized: a mutation made before an
    /// NlrRelay whose `carries` version does not descend from it.
    /// This is the check that would have caught ADR 0110's bug in CI,
    /// months before the production symptom, instead of after it shipped.
    MutationLostAcrossRelay { mutated: VersionedVar, boundary: NlrBoundary, at: Span },
}

fn verify(ir: &[ThreadedStmt]) -> Vec<VerifyError>;
```

The verifier runs unconditionally in every build profile (unlike
`debug_assert!`, which compiles out of `--release`) and in CI over every
compiled `stdlib/test/*.bt` and `stdlib/bootstrap-test/*.btscript` fixture,
the same way `core_lint` already runs unconditionally today. A verifier
failure is a compiler-internal bug (the lowering pass produced malformed
IR), reported the same way a panic in codegen is reported today — it is
not a new class of user-facing diagnostic and does not change what valid
Beamtalk programs compile to.

### Worked example: the bug class this closes

Recall [ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md)'s
motivating case:

```beamtalk
Value subclass: CollectionDriver
  classState: runs = 0

  class countedRun: aBlock over: aList -> Nil =>
    self.runs := self.runs + 1
    aList do: [:x | aBlock value: x]
    nil
```

A block escaping via `^` from inside `aList do: [...]` relays a foreign
NLR whose carried state is the *wrong* frame's state — the mutation to
`self.runs` is lost. Lowered to `ThreadedIr`, the method body looks
schematically like:

```
Bind { target: ClassVars1, source: ClassVars0, op: Put(runs, ...) }
NlrRelay { boundary: ClassMethod { has_class_vars: true }, token: T0, carries: ClassVars0 }
                                                                    ^^^^^^^^^ wrong — should be ClassVars1
```

`verify()` reports `MutationLostAcrossRelay { mutated: ClassVars1, boundary:
ClassMethod{..}, at: <line> }` — the exact shape of ADR 0110's bug, caught
mechanically at compile-time-of-the-compiler (i.e., against the compiler's
own test fixtures in CI) instead of via a user-filed production report.
ADR 0110's shipped fix (the process-dictionary shadow write-through) is
unaffected by this ADR — it is a runtime fix for a class of relay this IR
happens to make mechanically checkable *in the codegen layer*, catching
future variants of the same bug class before they reach runtime, not a
replacement for the runtime mechanism itself.

### Observable behavior — unchanged

Per the non-goals constraint, no Beamtalk program's compiled output or
REPL behavior changes:

```
st> CollectionDriver runCount
0
st> CollectionProbe escapeAfterCountedRun: #(1, 2, 3)
2
st> CollectionDriver runCount
1
```
(identical to ADR 0110's example — verified byte-for-byte via the existing
codegen snapshot suite at the end of each migration phase, the same
discipline ADR 0089 Phase B used.)

## Prior Art

| System | Approach | What we adopt / reject |
|---|---|---|
| **rustc: HIR → THIR → MIR** | Each IR is progressively simplified and single-purpose; MIR specifically exists to run flow-sensitive checks (the borrow checker) via a general dataflow framework (`rustc_mir_dataflow`), not to represent the whole language. [Source](https://github.com/rust-lang/rustc-dev-guide/blob/main/src/mir/index.md) | **Adopted:** the "small, single-purpose, narrowly-scoped IR whose job is to make one class of check possible" shape. rustc's precedent directly supports scoping this ADR to control-flow/state-threading only rather than building one IR that models everything, which is what makes the "no full-pipeline IR" non-goal defensible rather than merely expedient. |
| **Swift SIL ownership verifier** | A static verifier over an SSA-form IR checks ownership-model invariants (no use-after-free, no leaks) at compile time, explicitly framed as catching bugs in *SILGen and optimization passes* — i.e. compiler bugs, not user bugs. [Source](https://forums.swift.org/t/proposal-sil-ownership-model-verifier/4665) | **Adopted:** the framing that this verifier's job is to catch *codegen's own* bugs (drift between `classify_body_expr` and `threaded_expr.rs`, a lowering pass that loses a mutation across a relay), the same role SIL's verifier plays for SILGen — not a new class of end-user diagnostic. |
| **Cranelift's `verifier` module** | Cranelift IR (CLIF) ships an explicit, standard verifier pass checked in CI and (optionally) at each compilation, confirming CLIF invariants before lowering to machine code. | **Adopted:** "the verifier is a standard, expected part of a codegen backend that lowers through an internal IR," not a novel or heavyweight addition — reinforces this is ordinary compiler engineering for a project at Beamtalk's current maturity, not premature infrastructure. |
| **MLIR dialects + per-op `Verifier`** | MLIR's core insight is that a compiler doesn't need one universal IR — narrow, purpose-built "dialects" coexist, each with its own verification trait, and only the parts of a program that need a dialect's semantics are lowered into it. | **Adopted as the closest structural analogy:** `ThreadedIr` is, in MLIR's vocabulary, a small dialect for exactly the control-flow/state-threading subset of the program, verified on its own terms, coexisting with AST-directed codegen for everything else — rather than a monolithic IR the whole pipeline must funnel through. |
| **Erlang/OTP's `core_lint`** | Runs unconditionally as part of every `compile:forms([from_core \| Opts])` call, checking Core Erlang well-formedness (unbound/duplicate variables) — external to Beamtalk, downstream of code generation, syntactic only. | **Kept, complemented, not replaced.** `core_lint` is a real safety net for the well-formedness class of bug and stays exactly as-is (BT-3115). This ADR's verifier operates one layer earlier (Beamtalk's own lowering decisions) and catches the semantic class `core_lint` cannot see by construction — ADR 0110's bug was `core_lint`-clean. |
| **Gleam's codegen** | Gleam has no actor/mutable-state threading requirement analogous to Beamtalk's (no built-in process/gen_server state model compiled through tail-recursive loops with version-counted map threading) — its `Document`-tree codegen (the direct ancestor of ADR 0018's) doesn't face this problem at all. | **Not directly transferable.** Unlike ADR 0089, where Gleam was strong comparative prior art for leaf-typing, Gleam's absence of this exact problem means it offers no guidance on the IR/verifier question — noted for completeness, not as a design input. |
| **Pharo/Squeak Smalltalk** | Class-variable/instance-variable mutation is a direct memory write; a non-local return unwinds the real call stack via `BlockContext`, and there's no "commit" step for a mutation to survive — precisely the point ADR 0110 made about why this bug class is BEAM-specific, not Smalltalk-general. | **Confirms the constraint, not the solution.** Beamtalk inherits Smalltalk's mutable-variable *semantics* while compiling to an immutable substrate, so it must reconstruct "the assignment already happened" via functional threading — this ADR's IR/verifier targets exactly the reconstruction machinery that choice requires and that Smalltalk itself never needed. |

## User Impact

| Persona | Impact |
|---|---|
| **Newcomer** | None. No language syntax, semantics, or REPL output changes — every compiled Beamtalk program produces byte-identical output before and after. |
| **Smalltalk developer** | None directly; indirectly, this closes future variants of the ADR-0110 bug class (silently-lost mutations across `^`) before they ship, reinforcing the existing invariant that `^` is ordinary control flow, not a special case that can silently drop side effects. |
| **Erlang/BEAM developer** | None to generated `.beam` artifacts. The verifier's error shapes (`UnboundVersion`, `MutationLostAcrossRelay`, …) will look familiar to anyone who has read a `core_lint` "unbound variable" message — same spirit, one layer earlier, with Beamtalk-source-line attribution instead of a raw Core Erlang variable name. |
| **Production operator** | None. No runtime change; the verifier runs at compile time, in CI, against the compiler's own test fixtures — it is not part of the shipped runtime or the `beamtalk build` fast path beyond the (small, one-time-per-compile) cost of walking the lowered IR for the control-flow/state-threading subset of the module being compiled. |
| **Tooling developer (LSP, debugger)** | Mildly positive, longer-term. A structured `ThreadedIr` with span information is a better foundation for future BT-aware diagnostics or a codegen-decision inspector (e.g. "why did this loop fall back to StateAcc?") than re-deriving that information from `BEAMTALK_CODEGEN_DIAGNOSTICS=1` log lines, though no such tooling is proposed as part of this ADR. |
| **Compiler contributor** | The main audience. Adding a new stateful control-flow construct becomes "add a `ThreadedStmt` case + a verifier check" instead of "add Document-emission code and hope the four other `ThreadingPlan` call sites, the `BodyExprKind` classifier, and the `threaded_expr.rs` recognizer all still agree." The three duplicated version-counter services collapse into one `VersionedVar` type. Net positive with a real adjustment cost: this is new internal API surface (~S-to-M sized per migrated subsystem) that must be learned, on top of the existing `Document`/typed-leaf combinator surface (ADR 0089), which is unchanged. |

## Steelman Analysis

### Option A: Narrow lowered IR + verifier, phase-gated by subsystem (Recommended)

- 🧑‍💻 **Newcomer-to-compiler-internals contributor**: "When I add a new stateful loop variant, the compiler tells me at CI time — with a Beamtalk-source-line-attributed error — if I got the state threading wrong, instead of me discovering it three weeks later as a silently-wrong runtime value like ADR 0110's bug."
- 🎩 **Smalltalk purist**: "This is the same principle ADR 0110 already established for class vars — `^` must be ordinary control flow that never silently drops side effects — generalized from 'documented and runtime-patched for one case' to 'mechanically checked for the whole state-threading surface, before it ships.'"
- ⚙️ **BEAM veteran**: "`core_lint` already proved the value of an unconditional structural check baked into the pipeline. This is the same idea one layer up — narrowly scoped, doesn't touch the wire, doesn't touch `Document`, doesn't touch the 90% of codegen that doesn't need it."
- 🏭 **Operator**: "Zero runtime change, zero wire change. All risk is contained to compile-time internals, verified phase-by-phase against the existing byte-identical-snapshot discipline that already proved itself in ADR 0089's flag-day migration."
- 🎨 **Language/compiler designer**: "This is the textbook 'narrow, purpose-built IR + verifier' shape (rustc's MIR, Swift's SIL, MLIR's dialects) applied at exactly the size Beamtalk's current problem calls for — not the 58K-LOC full-pipeline rewrite ADR 0088 correctly rejected, and not zero, which leaves six independently-drifting invariants as the only defense against a real, previously-shipped bug class."

### Option B: Verifier without IR — strengthen the current generator with a post-hoc validator only

- 🧑‍💻 **Newcomer contributor**: "I don't have to learn a new IR type at all — the validator runs over what's already there."
- 🎩 **Smalltalk purist**: "Minimal new machinery is more in the spirit of pragmatic, incremental Smalltalk-style development than committing to a new typed layer up front."
- ⚙️ **BEAM veteran**: "`core_erlang_validity_tests.rs` already proved a post-hoc property-test validator has *some* value (BT-875 format-artifact detection) at very low cost. Why not extend that same style of check instead of adding a new internal representation?"
- 🏭 **Operator**: "Lowest-risk option on the table. No new pipeline stage, no new pass ordering to get wrong, ships incrementally with no coordination against BT-3125's `AnalysisResult` threading."
- 🎨 **Language designer (the sharpest form of this argument)**: "A verifier's value comes from what it checks, not from where the data it checks lives. If we can re-derive 'is this version linear, does this mutation survive this relay' by walking the *existing* generator state or rendered `Document` tree, we get the same safety without committing to a new IR that has to be kept in sync with codegen forever."

**Why A wins despite this steelman.** The sharpest form of B's argument
("value comes from the check, not the representation") is correct in the
abstract but fails on this codebase's specific problem: the information
`MutationLostAcrossRelay` needs (which version a relay's `carries` field
descends from) **does not exist** once code is lowered to `Document`/text —
it has to be *re-derived*, which means re-implementing the same
classification logic `classify_body_expr` and `ThreadingPlan` already
compute, a fourth time, in the validator. That is exactly the
`architecture-principles.md` §6 duplication anti-pattern this ADR's own
Context section documents happening three times already (`StateThreading`/
`class_var_version`/`self_version`). A validator that re-derives semantic
structure post-hoc is not lower-risk than an IR that carries that structure
through explicitly — it is the same risk, hidden one layer deeper, plus a
second implementation to keep in sync with the first. Building the small
IR is not overhead layered on top of the fix; constructing the IR *is* the
mechanism by which the classifier's and emitter's decisions stop being two
independently-maintained things that must agree.

### Option C: Status quo — `debug_assert!`s + `core_lint` + code review

- 🧑‍💻 **Newcomer contributor**: "The state-threading code has worked for many releases. Six `debug_assert!`s plus attentive code review caught real bugs before (they're how ADR 0110's bug class was eventually found and fixed)."
- 🎩 **Smalltalk purist**: "Smalltalk favors iterative refinement over up-front structure. Add checks where bugs are actually found, not speculatively."
- ⚙️ **BEAM veteran**: "`debug_assert!` is idiomatic Rust for this; every Rust codebase has invariants it doesn't formally verify. This isn't unusual."
- 🏭 **Operator**: "Zero migration cost, zero risk, ships nothing new."
- 🎨 **Language designer**: "The `control_flow/` cluster is 16K LOC and has shipped correct code for most of its history. The base rate of this specific bug class (mutation-lost-across-relay) is one documented occurrence (ADR 0110) in the codebase's lifetime — is a whole new IR proportionate to a rate-one bug?"

**Why A wins despite this steelman.** The rate-one framing undercounts the
risk: ADR 0110's bug was found because it was *reachable* through a common,
recommended pattern (`docs/beamtalk-language-features.md` itself
recommends the collection-driven-block shape that triggered it) — not
because it's rare in principle, but because most code doesn't happen to
combine class-var mutation with a block that escapes through library code.
`debug_assert!`s compile out of release builds entirely (the issue's
original framing is correct on this point even where its LOC/count
estimates were off), so they provide **zero** protection in the actual
shipped compiler binary — only in `cargo test`/dev builds. And the six that
do exist are concentrated on exactly one narrow slice (threading-mode/unpack
agreement, classifier/emitter agreement) — they provide no coverage at all
for the NLR-relay-loses-a-mutation class that ADR 0110 actually was. Status
quo is not "we have a check and choose not to add another" — it is "we have
no check for the bug class that has already shipped once."

### Option D: Full-pipeline typed Core Erlang IR (revisiting ADR 0018's rejected alternative)

Referenced for completeness, not re-litigated — this is explicitly excluded
as a non-goal by BT-3122. [ADR 0018](0018-document-tree-codegen.md)
§Alternatives Considered #3 already rejected "Typed Core Erlang IR" covering
all of codegen, for being "significantly more work... over-engineered for
our needs — we don't transform or optimize the IR, we just emit it," while
explicitly leaving the door open: *"A typed IR may become valuable later
(for optimization passes, multiple backends), but it's premature now."*
That reasoning holds unchanged for the ~90% of codegen (expressions,
primitives, dispatch) this ADR does not touch — those code paths don't
exhibit the multi-step, must-stay-synchronized decision structure that
makes control-flow/state-threading specifically worth lowering.

### Tension Points

- **Scope discipline is the whole argument.** BEAM veterans and operators
  broadly favor A over C once the ADR-0110 gap is made concrete, but would
  reject a version of A that crept toward D's full-pipeline scope — the
  ADR's phase-gating (below) exists specifically to keep each phase's
  migration cost visible and stoppable, learning directly from the ADR
  0088 lesson (a large, ungated migration that had to be withdrawn after
  Phase 0 measurement).
- **B vs A is a real disagreement about where duplication risk lives**, not
  a strawman — see the "why A wins" analysis above. If a future maintainer
  finds `ThreadedIr` drifting out of sync with the AST-directed codegen it
  sits next to (an IR that itself becomes stale relative to what it's
  supposed to represent), that is the risk B's steelman correctly
  identifies; this ADR's answer is that IR construction happens as an
  explicit lowering step immediately before verification and Document
  construction — there is no window where it can silently go stale,
  unlike a validator re-deriving structure after the fact.
- **Newcomers vs. compiler-internals contributors.** End users and
  newcomers see zero impact either way (User Impact table) — the entire
  cost/benefit of this decision is scoped to the small population of
  contributors touching `control_flow/`/`gen_server/methods.rs`, which
  argues for keeping the new API surface as small as BT-3122's scope
  allows rather than over-generalizing it.

## Alternatives Considered

### 1. Verifier without IR (post-hoc validator over current generator/Document state)

See Steelman Analysis. **Rejected**: the semantic structure the verifier
needs (version provenance, mode/unpack agreement, mutation reachability
across a relay) does not survive to `Document`/text — recovering it
post-hoc means re-implementing `classify_body_expr`/`ThreadingPlan`'s logic
a second time, the exact duplication this ADR's own IR unification is
meant to close. Not a lower-risk option; the same risk one layer deeper,
plus a second implementation to maintain.

### 2. Status quo — `debug_assert!`s + `core_lint` + code review

See Steelman Analysis. **Rejected**: `debug_assert!`s provide zero
protection in release builds (confirmed: none of the six in-scope
assertions run outside dev/test profiles), and none of the six cover the
NLR-relay-loses-a-mutation class that ADR 0110 already shipped once.
`core_lint` structurally cannot see this class of bug — the code it
checked in ADR 0110's case was well-formed.

### 3. Unify the three version counters only, no IR/verifier

A narrower fix: collapse `StateThreading`/`class_var_version`/
`self_version` into one `VersionedVar`-producing service (closing the
`architecture-principles.md` §6 duplication directly) without building a
lowered IR or verifier around it.

**Rejected as insufficient on its own** (though this ADR's `VersionedVar`
design absorbs it as a side effect): it removes the "three independently
implemented counters" duplication smell but does nothing for the six
`debug_assert!`-guarded "these two independently-computed decisions must
agree" invariants, which are the actual mechanism behind ADR 0110's bug
class. A shared counter type does not, by itself, check that a mutation
survives an NLR relay or that a threading mode and its unpack logic agree
— those checks require walking a structure that records the relationship
between binds and relays, which is what the IR is for. Worth doing either
way; this ADR folds it in rather than treating it as a separate, smaller
project, since the IR's `VersionedVar` type is the natural home for the
unification.

### 4. Full-pipeline typed Core Erlang IR

See Steelman Analysis Option D. **Rejected**: already rejected once (ADR
0018), for reasons that still hold for the 90% of codegen this ADR
deliberately excludes; explicitly a non-goal per BT-3122.

## Consequences

### Positive
- **Closes a real, previously-shipped bug class mechanically.** The
  `MutationLostAcrossRelay` check generalizes ADR 0110's fix from "one
  documented and runtime-patched case" to "checked at compile time for the
  entire control-flow/state-threading surface," before a future variant
  reaches production.
- **The six `debug_assert!`s become structural, always-on checks** instead
  of dev-build-only spot checks that silently vanish in release — the
  underlying invariants (threading-mode/unpack agreement,
  classifier/emitter agreement) are enforced by construction rather than
  by four/two independently-maintained runtime assertions.
- **Closes a real, previously undocumented duplication instance**
  (`StateThreading`/`class_var_version`/`self_version`) per
  `architecture-principles.md` §6, without a separate migration project.
- **`Document`, the typed-leaf API (ADR 0089), and the Wadler-Lindig
  printer (ADR 0018) are entirely unchanged** — this ADR adds a stage
  before them, not a replacement for anything already working.
- **No wire change, no runtime change, no behavior change** — every phase
  is gated on byte-identical `Document`/`.core` output, verified the same
  way ADR 0089's flag-day migration was.
- **Scoped and phase-gated**, learning directly from the ADR 0088 lesson:
  each phase has an independent exit criterion (byte-identical snapshots +
  verifier green + the corresponding `debug_assert!`s deleted), rather than
  one large, hard-to-review migration.

### Negative
- **New internal API surface to learn.** Contributors touching
  `control_flow/`/`gen_server/methods.rs`'s state-versioning slice must
  learn `ThreadedIr`/`ThreadedStmt`/`VersionedVar` in addition to the
  existing `Document`/typed-leaf combinator surface. Scoped narrowly
  (BT-3122's explicit non-goals) to limit this to the ~30% of codegen that
  actually exhibits the multi-step-decision problem.
- **Migration is multi-phase and multi-PR**, each touching working, tested
  code (`while_loops.rs`, `counted_loops.rs`, `list_ops/`'s six files,
  the actor/class-method threading slice of `gen_server/methods.rs`).
  Mitigated the same way ADR 0089 Phase B was: byte-identical snapshot
  parity as the hard gate on every phase, plus the full behavioral suite
  (`just test-stdlib`/`test-bunit`/`test-repl-protocol`).
- **The verifier can produce false confidence if its checks are
  incomplete.** `MutationLostAcrossRelay` as specified catches the
  ADR-0110 shape (a mutation whose version doesn't reach a relay's
  `carries` field); it does not claim to catch every possible semantic bug
  in state threading — only the class this ADR's research identified as
  currently unchecked. Extending its checks as new bug classes are found
  is expected, ongoing work, not a one-time deliverable.
- **Coordination risk with BT-3111/BT-3125.** This ADR's lowering entry
  point is designed to consume BT-3125's `lower_module_for_codegen`
  output once that epic lands (per the Constraints section), but BT-3125
  is independently in progress; if its design changes materially before
  landing, this ADR's Phase E wiring point may need to be revisited. Phases
  A–D do not depend on BT-3125 and are not blocked by this risk.

### Neutral
- **`NlrBoundary` is reused, not redesigned.** The existing enum
  (`ActorReply | ClassMethod{..} | ValueType`, `mod.rs:902-911`) and its
  builder `nlr_arm_result()` become inputs to `ThreadedStmt::NlrRelay`
  rather than being restructured.
- **`ThreadingPlan`'s mode-selection logic (`select_direct_params`/
  `select_tuple_acc`/`select_hybrid_params`, `BodyEffects` prescan,
  `StateAccFallbackReason`) is reused as-is** — it already computes the
  right answer; this ADR changes *when* that answer becomes durable data
  (as an IR node) rather than being immediately consumed by Document
  emission, and *what* checks run against it afterward.
- **`BEAMTALK_CODEGEN_DIAGNOSTICS=1`'s existing diagnostic categories**
  (`docs/development/debugging.md` §"Codegen Diagnostics") are unaffected;
  they report the same decisions, now sourced from `ThreadedIr` instead of
  inline generator state, with no change to their wording or gating env
  vars.

## Implementation

Phase-gated by subsystem, per BT-3122's requirement to avoid the ADR 0088
lesson (an unbounded, all-at-once migration). Each phase's exit criteria are
the same: byte-identical `Document`/`.core` output on the existing codegen
snapshot suite, verifier green on 100% of `stdlib/test/*.bt` and
`stdlib/bootstrap-test/*.btscript` fixtures, full behavioral suite
(`just test-stdlib`/`test-bunit`/`test-repl-protocol`) green, and the
`debug_assert!`s superseded by that phase's verifier checks *deleted from
source* (the forcing function that a phase is actually done, not merely
running in parallel with the old mechanism — the same discipline ADR 0089
used for `Document::String`/`Document::Eco`). The `/plan-adr` output
decomposes these into implementation issues; this section names the
commitment level.

### Phase A — IR types + verifier skeleton (S)
Define `VersionedVar`, `ThreadedStmt`, `VerifyError`, and `verify()` in a
new `crates/beamtalk-core/src/codegen/core_erlang/threaded_ir.rs`. Unify
`StateThreading`/`class_var_version`/`self_version` behind `VersionedVar`.
No codegen call site constructs `ThreadedIr` yet — this phase ships with
unit tests only (verifier logic against hand-built IR fixtures), zero
behavioral risk since nothing consumes it.

### Phase B — `while_loops.rs` + `counted_loops.rs` (M)
Migrate the `ThreadingPlan`-driven direct-params/tuple-acc/hybrid/StateAcc
modes for `whileTrue:`/`whileFalse:`/`to:do:`/`to:by:do:`/`timesRepeat:` to
construct `ThreadedIr` before Document emission. The four
"unpack should emit no code" `debug_assert!`s
(`while_loops.rs:317,454`, `control_flow/mod.rs:2522,2651`) are replaced by
`ThreadingModeUnpackMismatch` and deleted from source.

### Phase C — `list_ops/` (M)
Migrate `do:`/`collect:`/`select:`/`reject:`/`inject:into:`/`detect:`/
`anySatisfy:`/`allSatisfy:`/`flatMap:`/`takeWhile:`/`dropWhile:`/
`partition:`/`groupBy:`/`sort:` (the six `list_ops/` files, led by
`transform_ops.rs` at 2074 lines and `search_ops.rs` at 917) to the same IR.
Largest phase by LOC; no new invariant classes beyond Phase B's, so scoped
as a mechanical follow-on rather than combined with B to keep each PR
independently reviewable.

### Phase D — Actor/class-method state threading + NLR relay (L)
Migrate the state-versioning slice of `gen_server/methods.rs`
(`BodyExprKind` classification, `generate_body_exprs_with_reply`,
`classify_body_expr`, the tier-2/mutation-threading helpers), the
class-method `ClassVars` threading path (including ADR 0110's shadow-write
mechanism, unaffected at runtime but now covered by
`MutationLostAcrossRelay` at compile time), `conditionals.rs`'s
mutation-inlining, and `exception_handling.rs`'s try/catch-with-mutation
lowering. The two "must route through the Actor threaded emitter"
`debug_assert!`s (`methods.rs:1258,1450`) are replaced by
`UnboundVersion`/structural IR-construction guarantees and deleted — this
is the phase that actually removes the classifier/emitter duplication,
not merely checks for it. Largest and highest-risk phase; may itself split
into sub-PRs (actor state, then class-method state, then NLR relay) during
`/plan-adr` decomposition.

### Phase E — Wire to BT-3125's prepared-AST boundary (S, coordinate)
Once BT-3125 lands `lower_module_for_codegen(&mut module, &analysis)`, move
this ADR's lowering entry point to consume the threaded `AnalysisResult`/
`SemanticFacts` there, per the Constraints section, instead of any
interim local re-derivation used in Phases A–D. If BT-3125 has not yet
landed when Phases A–D are implemented, they read `AnalysisResult` the same
way current codegen does (via whatever re-derivation exists at that time)
— Phase E's job is exclusively to delete that interim path once the
shared boundary exists, not to block Phases A–D on BT-3125's timeline.

### Affected Components
- **New**: `crates/beamtalk-core/src/codegen/core_erlang/threaded_ir.rs` —
  `VersionedVar`, `ThreadedStmt`, `VerifyError`, `verify()`.
- **Modified**: `control_flow/mod.rs`, `control_flow/while_loops.rs`,
  `control_flow/counted_loops.rs`, `control_flow/list_ops/*.rs`,
  `control_flow/conditionals.rs`, `control_flow/exception_handling.rs`,
  `threaded_expr.rs` — construct `ThreadedIr` instead of emitting
  `Document` directly for state-threading constructs; the four/two
  `debug_assert!`s removed as their phases land.
- **Modified**: `gen_server/methods.rs` — the state-versioning slice only
  (Phase D); class/module registration scaffolding and ADR 0068 reflection
  metadata (the other ~60% of the file) unaffected.
- **Modified**: `mod.rs` — `StateThreading`/`class_var_version`/
  `self_version` fields collapse behind `VersionedVar`; `NlrBoundary`
  reused as-is inside `ThreadedStmt::NlrRelay`.
- **Unchanged**: `document/` (ADR 0089 typed-leaf API and printer),
  `dispatch_codegen.rs`, `expressions.rs`, `intrinsics.rs`,
  `value_type_codegen.rs` (expression/primitive codegen — AST-directed,
  out of scope), all Erlang-side runtime and wire format.

### Verification
- **Snapshot parity**: existing codegen snapshot suite (the same one ADR
  0089 Phase B verified byte-for-byte) must be unchanged after every phase.
- **Verifier coverage**: `just` target (name TBD in `/plan-adr`, e.g.
  `just verify-threaded-ir`) runs `verify()` over every compiled
  `stdlib/test/*.bt`/`stdlib/bootstrap-test/*.btscript` fixture in CI,
  reporting zero `VerifyError`s as a hard gate per phase.
  `core_erlang_validity_tests.rs`'s existing three text-shape properties
  are unaffected and continue running independently.
- **Behavioral suite**: `just test-stdlib`, `just test-bunit`,
  `just test-repl-protocol` green after every phase — the standard
  behavior-preservation bar this codebase already uses for codegen
  refactors (ADR 0089, ADR 0110).
- **Regression test for the motivating case**: ADR 0110's
  `CollectionDriver`/`CollectionProbe` repro (or an equivalent minimal
  case) added as a verifier unit test asserting `MutationLostAcrossRelay`
  fires on the *unfixed* shape and is silent on the *fixed* shape, so a
  future regression in either the runtime shadow-write mechanism or a new
  lowering path is caught by two independent layers (runtime behavior test
  + compile-time verifier).

## References

- Related issues:
  - [BT-3122](https://linear.app/beamtalk/issue/BT-3122) — this ADR
  - [BT-3111](https://linear.app/beamtalk/issue/BT-3111) — Epic:
    Analysis→codegen handoff (single semantic source of truth);
    coordination point for Phase E
  - [BT-3125](https://linear.app/beamtalk/issue/BT-3125) — Writeback
    passes consume threaded analysis; the `lower_module_for_codegen`
    preparation step this ADR's lowering entry point slots into
  - [BT-3112](https://linear.app/beamtalk/issue/BT-3112) — Epic:
    Generated-code correctness (core_lint, semantic fuzzing, real
    fixtures) — sibling correctness-net work; this ADR's verifier is a
    complementary, compile-time-internal layer, distinct from that epic's
    fuzzing/property-testing children
  - [BT-3035](https://linear.app/beamtalk/issue/BT-3035) — Epic that
    shipped ADR 0110's runtime fix; this ADR's `MutationLostAcrossRelay`
    check generalizes detection of that bug class to compile time
  - BT-3115 — `core_lint` readability fix (referenced in
    `docs/development/debugging.md`)
- Related ADRs:
  - [ADR 0018](0018-document-tree-codegen.md) — the `Document` tree this
    ADR's IR sits *before*; §Alternatives Considered #3 previously
    rejected a full-pipeline typed IR for reasons this ADR's narrower
    scope deliberately avoids re-triggering
  - [ADR 0041](0041-universal-state-threading-block-protocol.md) — the
    `{Value, StateAcc}` calling convention and 4-tuple NLR throw
    convention this ADR's `ThreadedStmt`/`NlrRelay` formalize as data
  - [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) —
    actor mutable state vs. value-type immutability, the semantic split
    `ThreadingBoundary`/`NlrBoundary` encode
  - [ADR 0088](0088-direct-cerl-emission.md) — cerl-as-wire-format
    proposal; stays closed/withdrawn; orthogonal to this ADR (wire
    transport vs. internal lowering stage)
  - [ADR 0089](0089-typed-document-leaves.md) — the typed-leaf `Document`
    API this ADR's printer stage is unchanged; also this codebase's most
    directly comparable prior large-scale phase-gated codegen migration
    (byte-identical-snapshot discipline adopted here)
  - [ADR 0109](0109-block-scoped-class-methods-run-blocks-in-the-caller.md)
    — block-runs-where-invoked semantics underlying the NLR relay problem
  - [ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md) — the
    shipped bug and runtime fix this ADR's verifier generalizes detection
    of to compile time; not superseded or affected at runtime
- Documentation:
  - `docs/development/debugging.md` §"Codegen Diagnostics", §"core_lint
    (BT-3115)"
  - `docs/development/architecture-principles.md` §6 "Duplication & the
    Shared-Leaf-Module Pattern" — the pattern this ADR's `VersionedVar`
    unification follows
- Code:
  - `crates/beamtalk-core/src/codegen/core_erlang/control_flow/` (13
    files, 16,329 LOC) — in scope
  - `crates/beamtalk-core/src/codegen/core_erlang/threaded_expr.rs` (416
    LOC) — in scope
  - `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs`
    (5805 LOC; ≈1900–2400 lines state-versioning-specific) — in scope
    (state-versioning slice only)
  - `crates/beamtalk-core/src/codegen/core_erlang/mod.rs` — `NlrBoundary`,
    `nlr_arm_result`, `ClassContext`, `ValueTypeContext`,
    `with_branch_context`
  - `crates/beamtalk-core/src/codegen/core_erlang/state_codegen.rs` —
    `StateThreading`
  - `crates/beamtalk-core/src/codegen/core_erlang/util.rs` —
    `versioned_var`
  - `crates/beamtalk-core/src/codegen/core_erlang_validity_tests.rs` —
    existing text-shape property suite, unaffected
  - `runtime/apps/beamtalk_compiler/src/beamtalk_compile_diagnostics.erl`
    — `core_lint` integration
