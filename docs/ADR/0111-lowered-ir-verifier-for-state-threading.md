# ADR 0111: Mid-Level Lowered IR + Verifier for State Threading, Control Flow, and Non-Local Return

## Status
Accepted (2026-08-10)

## Implementation Tracking

**Epic:** [BT-3128](https://linear.app/beamtalk/issue/BT-3128) — Lowered IR + Verifier for State Threading (ADR 0111)
**Status:** Planned

| Phase | Issue | Description | Size |
|---|---|---|---|
| 1 | [BT-3129](https://linear.app/beamtalk/issue/BT-3129) | ThreadedIr types + verifier + test shim, gated by Phase A0 measurement | M |
| 1 | [BT-3130](https://linear.app/beamtalk/issue/BT-3130) | Expand codegen snapshot corpus over the threading-mode matrix | M |
| 2 | [BT-3131](https://linear.app/beamtalk/issue/BT-3131) | Unify version counters behind typestate VersionedVar + RAII branch guard | M |
| 3 | [BT-3132](https://linear.app/beamtalk/issue/BT-3132) | Migrate while_loops + counted_loops; delete the 4 unpack debug_asserts | M |
| 4 | [BT-3133](https://linear.app/beamtalk/issue/BT-3133) | Migrate list_ops (fold-shaped + early-exit) | L |
| 4 | [BT-3134](https://linear.app/beamtalk/issue/BT-3134) | Migrate conditionals + exception_handling | M |
| 4 | [BT-3135](https://linear.app/beamtalk/issue/BT-3135) | Migrate gen_server state threading + NLR + ShadowWriteMissing contract; delete the 2 routing debug_asserts | L |
| 5 | [BT-3136](https://linear.app/beamtalk/issue/BT-3136) | Close-out: verifier CI wiring, docs, status → Implemented | S |

**Recommended start:** BT-3129 and BT-3130 (Phase 1, no dependencies, parallelizable). BT-3129 carries the Phase A0 measurement gate — if it fails, the epic descopes to Alternative 1b.

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
`debug_assert!`s and a set of independently-implemented decisions that must
agree with each other by convention, not by construction.

[ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md) is a real,
shipped bug in the adjacent class: a class method that mutated a class
variable and then handed a block to another method lost the mutation when the
block's `^` escaped through library code, while still returning the *correct
value*. The generated Core Erlang was well-formed the entire time — every
variable bound, every arity correct — so `core_lint` (which runs
unconditionally on every compile; see below) passed it without complaint.

**What this ADR honestly claims about that bug — and what it does not.**
A verifier catches violations of invariants someone has already stated; it
does not discover unknown-unknowns. ADR 0110's bug was an unknown-unknown:
before it was found, no lowering pass would have encoded the requirement
"every class-var mutation must be recoverable at every relay exit," because
that requirement was exactly the missing knowledge. The mechanism that hunts
that kind of bug is BT-3112's planned metamorphic/differential harness
(semantics-preserving transforms must preserve results — ADR 0110's
signature, "the returned value is correct; only the mutation is lost," is
the canonical metamorphic-oracle target). What this ADR's verifier does for
the 0110 class is narrower and still valuable: **now that the invariant is
known**, the IR models the shadow-write emission explicitly, and the
verifier enforces it at every existing and future mutation site — pinning
ADR 0110's fix against regression and extending it mechanically to code
paths its authors never saw (a new control-flow construct, a new mutation
emission site) instead of relying on each future author to remember the
rule. The two mechanisms are complementary and this ADR sequences itself
accordingly (see §Coordination).

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

Two honesty notes on the `debug_assert!` situation:

- **The six assertions are not the last line of defense.** In release
  builds, a violation of any of the six degrades into malformed Core
  Erlang — an unbound `StateAcc` reference (the four unpack asserts) or an
  assignment target/reply that was never emitted (the two routing asserts)
  — which `core_lint` catches unconditionally at compile time. The honest
  statement is *"these six invariants have a structural backstop that
  produces a bad, Erlang-level error message far from the real cause, not
  no backstop"* — the verifier's contribution for these six is diagnosis
  quality and centralization, not net-new detection.
- **Every one of the six is a "two independently-computed decisions must
  agree" check** (threading-mode vs. unpack logic; classifier vs.
  emitter). That is the shape of invariant an IR removes by construction
  — one IR node has one emitter — rather than merely checks.

### The duplication this ADR's IR would also close

Research for this ADR surfaced a second, previously undocumented instance of
the CLAUDE.md/`architecture-principles.md` §6 duplication anti-pattern
("layer X can't depend on layer Y" leading to independent reimplementation
instead of a shared leaf module — see
[`docs/development/architecture-principles.md`](../development/architecture-principles.md)
§6): **three structurally identical, independently implemented
monotonic-version-counter services**, all producing `Prefix`, `Prefix1`,
`Prefix2`, … names via the same `util::versioned_var(prefix, version)`
helper, but tracked as three separate fields with three separate — and
deliberately *different* — save/restore disciplines:

| Counter | Type | Where | Produces | Branch discipline (`with_branch_context`, `mod.rs:2064-2080`) |
|---|---|---|---|---|
| Actor/instance state | `StateThreading` struct (`state_codegen.rs:42`) | `CoreErlangGenerator.state_threading` field | `State`, `State1`, … — **rendered as `StateAcc{N}` inside non-hybrid loop bodies** (`mod.rs:1994-2028`) | reset to 0 on branch entry, restored on exit |
| Class variables | `usize` field + `next_class_var()` (`mod.rs:1132`, `mod.rs:2088`) | `ClassContext.class_var_version` | `ClassVars`, `ClassVars1`, … (ADR 0110's mechanism) | restored but **not** reset (BT-1550); `class_var_mutated` deliberately sticky, never restored |
| Value-type fields | `usize` field + `next_self_var()` (`mod.rs:1211`, `mod.rs:2104`) | `ValueTypeContext.self_version` | `Self`, `Self1`, … | **neither saved nor restored** — the live landmine |

These asymmetries are load-bearing (BT-1449/BT-1550), which means
**unifying the representation is not the same as unifying the discipline**:
a naive "one counter type for all three" would change generated variable
names and break byte-parity. The `VersionedVar` design below therefore
unifies *naming and identity only*; per-prefix scope discipline remains an
explicit, per-prefix policy, preserved exactly and pinned by tests (see
Phase A2). Note also that the `State` counter's prefix is a *render-time*
function of generator context (`in_loop_body`/`in_hybrid_loop` select
`StateAcc*` vs `State*` naming for the same counter), and `"StateAcc"`
additionally serves as an unversioned lambda *parameter* name in foldl
list-ops (`basic_ops.rs:84`, `dict_ops.rs:90`, …) — two distinct roles one
string currently covers, which the IR must model as distinct nodes.
(A fourth, unrelated "State" name, `_BuilderStateN` in
`gen_server/methods.rs:2514`'s `build_builder_state_doc`, is a per-class
positional index with no relation to mutation-version threading at all.)

### What already exists and does *not* need to change

- **`core_lint`** (BT-3115) already runs unconditionally as part of every
  `compile:forms(Forms, [from_core | Opts])` call — independent of the
  `clint`/`no_lint` options, which only gate a different code path
  (compiling from Erlang *source*, which Beamtalk never does). It reliably
  catches unbound-variable and duplicate-variable well-formedness bugs
  (`docs/development/debugging.md` §"core_lint (BT-3115)"). It cannot
  catch semantic bugs like ADR 0110's — the generated code was well-formed.
  This ADR's verifier operates one layer earlier, on Beamtalk's own
  lowering decisions, and is a complement to `core_lint`, not a
  replacement.
- **`core_erlang_validity_tests.rs`** (part of BT-3112's epic) is a
  proptest suite over a fixed 20-element `FRAGMENTS` array checking three
  purely textual properties on rendered Core Erlang: balanced delimiters,
  module-name match, and absence of `Debug`/`Display`-format artifacts
  (the BT-875 class ADR 0089 closed). None of the three inspect
  state-threading correctness, NLR semantics, or variable-binding
  provenance.
- **`document/` (ADR 0089)** is the closest existing precedent for
  "typed structure with its own discipline," but it solves a different
  class of bug by a different method: it makes the BT-875 string-escape
  vector *unrepresentable*, rather than building a checkable structure and
  running a separate verification pass over it. There is no existing
  verifier-pattern precedent in this codebase; this ADR introduces one.
- **The codegen snapshot corpus is currently too thin to gate this
  migration** — measured: of the 318 snapshots under
  `test-package-compiler/tests/snapshots/`, exactly **1** contains a
  `letrec` loop, **1** contains `$bt_nlr`, **1** contains `ClassVars1`,
  **1** contains `class_vars_shadow`, and **4** contain `StateAcc`. A
  "byte-identical snapshots" gate over that corpus is near-vacuous for
  precisely the constructs this ADR migrates. Expanding the corpus is
  therefore a *prerequisite deliverable* of this ADR (Phase A3), not an
  assumed pre-existing safety net. (ADR 0089's byte-parity discipline,
  which this ADR follows in spirit, was likewise not a standing harness —
  its `.beam`-parity check was an ad-hoc `cmp -l` during its flag-day PR.)

### Constraints

- **Narrow scope, as specified by BT-3122 — with one addition.** The
  lowered form covers the control-flow + state-threading core: the
  `control_flow/` cluster, `threaded_expr.rs`, the state-versioning subset
  of `gen_server/methods.rs` (measured at roughly 1900–2400 of its 5805
  lines), **and the version-`Bind`-emitting slices of `expressions.rs` and
  `dispatch_codegen.rs`**: `generate_field_assignment`
  (`expressions.rs:552` calls `next_class_var()`; the analogous field
  paths drive `next_self_var()`/`next_state_var()`) and
  `dispatch_codegen.rs:463`. These two sites are the *sole producers* of
  the version bindings the verifier checks; an IR that cannot see its own
  `Bind` producers cannot verify them. The rest of those two files
  (expression and dispatch codegen generally) stays AST-directed and out
  of scope, as do `intrinsics.rs` and `value_type_codegen.rs`.
- **Explicit non-goals** (BT-3122): no full-pipeline IR covering all of
  Core Erlang codegen — already considered and rejected once, as "Typed
  Core Erlang IR," in [ADR 0018](0018-document-tree-codegen.md)
  §Alternatives Considered #3 ("over-engineered for our needs — we don't
  transform or optimize the IR, we just emit it"); that reasoning still
  holds for the codegen this ADR does not touch. No cerl-wire change —
  [ADR 0088](0088-direct-cerl-emission.md) stays closed/withdrawn; this
  ADR's IR is entirely internal to `beamtalk-core` and never reaches the
  Port. No behavior change — every phase must produce byte-identical
  output over the *expanded* snapshot corpus (Phase A3) plus an ordered
  diagnostic-stream equality check (the `BEAMTALK_CODEGEN_DIAGNOSTICS`
  pipeline's `Vec<Diagnostic>` order and multiplicity are observable and
  moving `ThreadingPlan` selection into lowering changes when diagnostics
  fire — parity must be asserted, not assumed).
- **The `Document`/text printer is unchanged.** The typed-leaf API (ADR
  0089) remains the only path from a lowered leaf to Core Erlang text.
- **Coordination with BT-3111/BT-3125.** BT-3125 (In Progress, PR open at
  the time of writing) is moving the writeback trio out of codegen's
  discretion into a driver-level `lower_module_for_codegen(&mut module,
  &analysis)` preparation step, explicitly noting *"if the BT-3122 ADR is
  accepted, this preparation step is where it slots in."* Because BT-3125
  is landing imminently and on the same snapshot gate, **it is a soft
  prerequisite for this ADR's Phase B onward** — building an interim
  analysis re-derivation path only to delete it later would reproduce the
  exact anti-pattern BT-3111 is closing. Phase A (types, verifier,
  snapshot expansion, measurement) has no dependency on BT-3125 and can
  proceed in parallel.
- **Coordination with BT-3112.** The metamorphic/differential harness in
  that epic is the unknown-unknown hunter for this bug space; this ADR's
  verifier is the known-invariant enforcer. They are complementary, and
  the Phase A0 measurement gate below gives an explicit decision point to
  descope this ADR toward its cheaper fallback (Alternative 1b) if the
  harness lands first and changes the value calculus.

## Decision

**Introduce a small, narrowly-scoped lowered IR — `ThreadedIr` — covering
state-version bindings (with frame identity), threading-mode selection,
shadow-write emission, and NLR relay boundaries, plus a verifier that
checks it before `Document` construction.** Everything else in codegen
stays AST-directed and unaffected. The decision is gated: Phase A0
measures IR-construction/verification cost on a fixed fixture set against
a pre-declared threshold before any migration phase begins (the ADR 0088
lesson, applied to ourselves).

### Pipeline shape

```
Beamtalk AST
    │  (BT-3125: lower_module_for_codegen(&mut module, &analysis))
    ▼
AST, prepared with threaded AnalysisResult / SemanticFacts
    │  (this ADR: lower_control_flow — the in-scope slices only)
    ▼
ThreadedIr                      ◄── verify(&ThreadedIr) -> Vec<VerifyError>
    │  (unchanged: typed-leaf Document construction, ADR 0089)
    ▼
Document
    │  (unchanged: Wadler-Lindig pretty-printer, ADR 0018)
    ▼
Core Erlang text ──► core_lint (unconditional, OTP) ──► BEAM
```

### The IR

```rust
/// Frame identity — allocated at each method entry, with_branch_context
/// entry, builder-fun entry. Version linearity is PER FRAME: the existing
/// counters are deliberately not SSA (with_branch_context resets
/// state_version to 0 per branch arm, so sibling arms legitimately both
/// produce State1 in disjoint scopes). Without frame identity, a linearity
/// check false-positives on every branching method — this field is a
/// design requirement, not a nicety.
struct FrameId(u32);

/// One of the three (formerly independent) version counters, unified in
/// NAMING AND IDENTITY ONLY. Per-prefix scope discipline (state:
/// reset+restore per branch; class_vars: restore-only, mutated-flag
/// sticky; self: currently neither — see Phase A2) remains explicit
/// per-prefix policy, preserved exactly.
struct VersionedVar {
    prefix: VersionPrefix,   // State | ClassVars | SelfVt
    version: usize,
    frame: FrameId,
}
/// NOTE: the State counter renders as `StateAcc{N}` inside non-hybrid
/// loop bodies — prefix rendering is a function of (counter, loop
/// context), decided at Document-construction time, not stored in the IR.
/// The unversioned `StateAcc` lambda *parameter* of foldl list-ops is a
/// separate node (AccParam), not a VersionedVar.

enum ThreadingMode { DirectParams, TupleAcc, Hybrid, StateAcc(StateAccFallbackReason) }

enum ThreadedStmt {
    /// A mutation: binds a fresh version from a prior one in the same
    /// frame. `shadow_write: bool` records whether this Bind also emits
    /// the ADR 0110 process-dictionary shadow write — modeling the side
    /// channel explicitly is what makes the ShadowWriteMissing check
    /// (below) possible.
    Bind { target: VersionedVar, source: VersionedVar, op: BindOp, shadow_write: bool },

    /// A loop or mutation-carrying conditional, with its mode already
    /// resolved (by the existing ThreadingPlan logic, run once during
    /// lowering instead of re-derived at emission).
    Threaded { mode: ThreadingMode, frame: FrameId, body: Vec<ThreadedStmt>, produces: Vec<VersionedVar> },

    /// An NLR boundary. Faithful to what codegen actually emits
    /// (mod.rs:2480-2549): the token-MATCHING catch arm binds its state
    /// from the thrown 4-tuple's pattern variable (NlrCatchVars.state_var
    /// — a fresh pattern binding, not a versioned var), and the
    /// token-non-matching (foreign) arm carries nothing and re-raises.
    /// The IR does NOT pretend the relay "carries" a VersionedVar.
    NlrCatch { boundary: NlrBoundary, token: TokenId, frame: FrameId },

    Return(ValueRef, VersionedVar),
}
```

`NlrBoundary` (`ActorReply | ClassMethod { has_class_vars: bool } | ValueType`)
is the existing enum from `mod.rs:902-911`, reused as-is.

### The verifier

```rust
enum VerifyError {
    /// A versioned var referenced with no producing Bind in its frame (or
    /// a parent frame per the explicit frame-flow rule). Catches the
    /// unbound-StateX class one layer earlier than core_lint, with a
    /// Beamtalk-source-attributable message instead of erlc's raw
    /// "unbound variable 'State3' in myMethod/2". Diagnosis-quality
    /// improvement over an existing backstop, not net-new detection.
    UnboundVersion { var: VersionedVar, at: Span },

    /// Per-frame linearity: within one FrameId, each version produced by
    /// exactly one Bind, consumed as the source of at most one successor.
    /// Frame-scoped by design — see FrameId above.
    NonLinearVersion { var: VersionedVar, producers: usize, consumers: usize },

    /// Replaces the four "unpack should emit no code" debug_asserts as a
    /// structural property: an optimized ThreadingMode node cannot
    /// contain an unpack Bind. (In release today this failure degrades to
    /// a core_lint unbound-variable error; the gain is a correct,
    /// centralized, source-attributed diagnosis.)
    ThreadingModeUnpackMismatch { mode: ThreadingMode, at: Span },

    /// The ADR 0110 CONTRACT check — regression-pinning, not
    /// counterfactual detection (see "Verifier honesty" below). Now that
    /// ADR 0110 established the invariant, it is checkable structurally:
    /// a class-var Bind at frame depth 0 (method top frame) inside a
    /// method whose body can relay a foreign NLR (has_class_vars: true
    /// boundary present) MUST have shadow_write: true. Fires if a future
    /// emission path forgets the shadow write ADR 0110's fix depends on,
    /// or if a new mutation site is added without it.
    ShadowWriteMissing { mutated: VersionedVar, at: Span },
}

fn verify(ir: &[ThreadedStmt]) -> Vec<VerifyError>;
```

**Failure behavior.** The verifier runs in debug builds and CI
unconditionally. In release builds, a `VerifyError` does **not** panic —
per CLAUDE.md ("never panic on user input"; "always return
`(Result, Vec<Diagnostic>)`"), it is reported as an internal-error
diagnostic attached to the compile result while emission proceeds with the
generator's output (i.e., release behavior on a verifier bug is *no worse
than today*, plus a diagnostic; debug/CI behavior is a hard failure). A
false positive in a shipped compiler must degrade to a warning, not a
refusal to compile valid code.

### Verifier honesty — what this catches and what it cannot

These limits are structural, not incidental — they follow directly from how
the generator emits NLR scaffolding (`wrap_body_with_nlr_catch`,
`mod.rs:2480-2549`, and `NlrCatchVars`, `mod.rs:880-890`):

- **The verifier checks what the lowering pass said, not what it meant.**
  The lowering pass and the emitter share their source of truth
  (`current_class_var()`, `ThreadingPlan`, `classify_body_expr`'s
  outputs). A check comparing the generator against itself is silent when
  both are consistently wrong. The checks above are chosen to be
  *structural* — properties of the IR shape that don't depend on the
  generator's beliefs being correct: a Bind either has a shadow-write
  companion or it doesn't; a mode node either contains an unpack or it
  doesn't; a version either has a producer in its frame or it doesn't.
- **`ShadowWriteMissing` would not have caught ADR 0110's bug before it
  was known.** Pre-0110, the shadow write didn't exist, the invariant was
  unstated, and no verifier rule could have encoded it. The claim this ADR
  makes is regression-pinning and forward extension: the invariant, once
  paid for at production cost, becomes mechanically enforced at every
  current and future mutation site, rather than living in a code comment
  and one BUnit test. The unknown-unknown-hunting role belongs to
  BT-3112's metamorphic harness.
- **Part of the 0110 contract lives on the Erlang side and is outside any
  Rust-side verifier's reach.** The shadow key shape and erase discipline
  in `invoke_class_method/7` (`beamtalk_class_dispatch.erl`) must agree
  with what codegen emits. Per CLAUDE.md's cross-boundary rule, that
  agreement needs a **shared conformance fixture** (a compiled fixture
  whose shadow key/erase behavior both sides assert against), not a
  comment — added in Phase D alongside the `ShadowWriteMissing` check as
  the two halves of the same contract.

### Worked example: what the IR and verifier actually do for the 0110 class

Post-ADR-0110, `generate_field_assignment` (`expressions.rs:552`) emits,
for a top-frame class-var mutation:

```erlang
let ClassVars1 = call 'maps':'put'('runs', Val, ClassVars0) in
let _ = call 'erlang':'put'({'$bt_class_vars_shadow', ...}, ClassVars1) in
```

Lowered:

```
Bind { target: ClassVars1@F0, source: ClassVars0@F0, op: Put(runs, ...), shadow_write: true }
NlrCatch { boundary: ClassMethod { has_class_vars: true }, token: T0, frame: F0 }
```

`verify()` is silent. If a future refactor of the emission site (or a new
mutation path — say, a future `self.field +=`-style sugar lowered through a
different function) produces `shadow_write: false` in a
`has_class_vars: true` method, `verify()` reports `ShadowWriteMissing` at
the Beamtalk source line — in CI, on the day the regression is written.
That is the deliverable: the invariant ADR 0110 discovered at production
cost becomes structurally un-forgettable.

### Observable behavior — unchanged

No Beamtalk program's compiled output or REPL behavior changes:

```
st> CollectionDriver runCount
0
st> CollectionProbe escapeAfterCountedRun: #(1, 2, 3)
2
st> CollectionDriver runCount
1
```
(identical to ADR 0110's example — verified over the *expanded* snapshot
corpus, Phase A3, at the end of each migration phase.)

## Prior Art

| System | Approach | What we adopt / reject |
|---|---|---|
| **rustc: HIR → THIR → MIR** | Each IR is progressively simplified and single-purpose; MIR specifically exists to run flow-sensitive checks (the borrow checker) via a general dataflow framework (`rustc_mir_dataflow`), not to represent the whole language. [Source](https://github.com/rust-lang/rustc-dev-guide/blob/main/src/mir/index.md) | **Adopted:** the "small, single-purpose, narrowly-scoped IR whose job is to make one class of check possible" shape — what makes the "no full-pipeline IR" non-goal defensible rather than merely expedient. |
| **Swift SIL ownership verifier** | A static verifier over an SSA-form IR checks ownership-model invariants at compile time, explicitly framed as catching bugs in *SILGen and optimization passes* — compiler bugs, not user bugs. [Source](https://forums.swift.org/t/proposal-sil-ownership-model-verifier/4665) | **Adopted:** the framing that this verifier's job is to catch *codegen's own* bugs. **Adapted with a caveat SIL's docs are honest about and so is this ADR:** a verifier enforces stated invariants; SIL's ownership verifier postdates and encodes an ownership *model* — it did not discover the model. Same relationship as `ShadowWriteMissing` to ADR 0110. |
| **Cranelift's `verifier` module** | CLIF ships an explicit verifier pass checked in CI and optionally per-compilation. | **Adopted:** "the verifier is a standard, expected part of a codegen backend that lowers through an internal IR" — ordinary compiler engineering, not novel infrastructure. |
| **MLIR dialects + per-op `Verifier`** | Narrow, purpose-built dialects coexist, each with its own verification; only the program parts needing a dialect's semantics are lowered into it. | **Adopted as the closest structural analogy:** `ThreadedIr` is a small dialect for the control-flow/state-threading subset, verified on its own terms, coexisting with AST-directed codegen for everything else. |
| **Erlang/OTP's `core_lint`** | Runs unconditionally on every `from_core` compile, checking well-formedness (unbound/duplicate variables) — downstream of codegen, syntactic only. | **Kept, complemented, not replaced.** It also serves as the release-mode backstop for several of this ADR's checks (see §Verifier honesty) — the verifier's gain over it for those is diagnosis quality and source attribution, and this ADR says so rather than claiming net-new detection. |
| **Gleam's codegen** | No actor/mutable-state threading requirement analogous to Beamtalk's; its `Document`-tree codegen doesn't face this problem. | **Not directly transferable** — noted for completeness. |
| **Pharo/Squeak Smalltalk** | Mutation is a direct memory write; NLR unwinds a real stack; no "commit" step exists for a mutation to survive. | **Confirms the constraint, not the solution:** Beamtalk inherits mutable-variable semantics on an immutable substrate, so it must reconstruct "the assignment already happened" via functional threading — this ADR targets the reconstruction machinery Smalltalk never needed. |

## User Impact

| Persona | Impact |
|---|---|
| **Newcomer** | None. No language syntax, semantics, or REPL output changes. |
| **Smalltalk developer** | None directly; indirectly, the invariant that `^` never silently drops side effects — established for class vars by ADR 0110 — becomes mechanically pinned against regression rather than convention-guarded. |
| **Erlang/BEAM developer** | None to generated `.beam` artifacts. Verifier error shapes will read like `core_lint` messages one layer earlier, with Beamtalk-source attribution. |
| **Production operator** | None at runtime. Verifier cost is compile-time only, budgeted and measured in Phase A0 before any migration proceeds; in release builds a verifier finding degrades to an internal-error diagnostic, never a panic or a refusal to compile (CLAUDE.md error-recovery rules). |
| **Tooling developer (LSP, debugger)** | Mildly positive, longer-term: a structured `ThreadedIr` with spans is a better substrate for future "why did this loop fall back to StateAcc?" tooling than log-line parsing; no such tooling is proposed here. |
| **Compiler contributor** | The main audience, and honestly a mixed bag: (+) adding a stateful construct becomes "add a `ThreadedStmt` case + verifier check" with single-sourced classifier/emitter decisions; the three duplicated counters collapse into one frame-disciplined type. (−) new internal API surface to learn; a real test-migration cost (see Consequences — the existing `Document`-asserting tests survive only via the `lower_and_render` shim committed to in Phase A1); and the in-scope files are under active feature development, so phases carry a stated conflict protocol (see Implementation). |

## Steelman Analysis

### Option A: Narrow lowered IR + verifier, phase-gated with a measurement gate (Recommended)

- 🧑‍💻 **Newcomer-to-compiler-internals contributor**: "When I add a new stateful loop variant or a new mutation-emission path, CI tells me — with a Beamtalk-source-attributed error — if I forgot the shadow write or emitted an unpack in a direct-params loop, instead of me discovering it as an erlc unbound-variable message pointing at generated code, or worse, as silently-wrong runtime state."
- 🎩 **Smalltalk purist**: "ADR 0110 established that `^` must never silently drop side effects, at the cost of a shipped bug. This makes that invariant un-forgettable at every future mutation site instead of living in one test and a comment."
- ⚙️ **BEAM veteran**: "`core_lint` proved the value of an unconditional structural check in the pipeline. This is the same idea one layer up, and it's honest that for several checks it improves the diagnosis rather than adds detection — that's still worth having when the alternative is an erlc error naming a generated variable three layers from the cause."
- 🏭 **Operator**: "Zero runtime change, zero wire change, release-mode failure degrades to a diagnostic. The Phase A0 gate means if the IR's compile-time cost is real, the project stops before migrating anything — the ADR 0088 discipline applied in advance rather than in retrospect."
- 🎨 **Language/compiler designer**: "The three version counters with three inconsistent disciplines — one of which (`self_version`) is saved/restored *nowhere* — is a latent bug factory independent of everything else in this ADR. The IR is the natural home for fixing it with frame identity rather than a fourth convention."

### Option B: Verifier without IR

Two forms, split because they have opposite verdicts:

**B1 — re-derive post-hoc (from Document/text or generator state after the
fact).** Rejected: the needed structure (version provenance, frame
identity, mode membership) doesn't exist post-emission and re-deriving it means
re-implementing `classify_body_expr`/`ThreadingPlan` a second time — the
§6 duplication anti-pattern.

**B2 — record decisions as they are made (the strong form, and this ADR's
designated fallback).** `ThreadingPlan`, `BodyExprKind`, and the
version-counter calls all exist as in-memory values at emission time
today. Push each decision into a `Vec<Decision>` side-channel as it is
made; verify that. Re-derives nothing, needs no emission restructuring,
gets the mode/unpack and classifier/emitter checks always-on at a fraction
of the cost.

- 🧑‍💻 **Newcomer**: "No new pipeline stage to learn — the codegen I read is still the codegen that runs."
- ⚙️ **BEAM veteran**: "This is the 90/10 point. The checks that are actually sound (§Verifier honesty) are exactly the ones this gets."
- 🏭 **Operator**: "No migration freeze on 11K LOC of actively-developed files."
- 🎨 **Language designer (sharpest form)**: "On §Verifier honesty's own accounting, the verifier's *checks* don't require the IR — they require the *decisions*, which already exist as data. The IR's remaining unique contribution is single-sourcing (one node, one emitter) and the counter unification. Is that worth the migration?"

**Why A still wins — narrowly.** B2 checks that
two independently-computed decisions agree; A removes the second
computation. B2's recorder must itself be maintained at every decision
site (a site that forgets to record is invisible to the validator — the
same "remember to keep in sync" failure mode, relocated); A's IR is the
decision record *and* the emission input, so an unrecorded decision cannot
emit. And the counter unification with frame discipline (the
`self_version` landmine) needs a typed home either way. But the margin is
real only if the migration cost stays bounded — hence the Phase A0 gate,
and hence B2's designation as the **explicit descope target** if A0 fails
its threshold: everything in Phase A1 (types, checks, snapshot corpus)
transfers to B2 directly.

### Option C: Status quo — `debug_assert!`s + `core_lint` + code review

- 🧑‍💻 **Newcomer**: "The state-threading code has shipped correct for most of its history; review caught 0110 eventually."
- ⚙️ **BEAM veteran**: "In release, all six assert-guarded failures degrade to `core_lint` errors anyway — there's a backstop, not a void."
- 🏭 **Operator**: "Zero cost, zero risk, zero freeze."
- 🎨 **Language designer**: "One shipped bug in the codebase's lifetime for this class; is any of this proportionate?"

**Why A wins despite this steelman.** The six assertions *do*
have a release-mode backstop (`core_lint`), but it fires far from the
cause with an Erlang-level message about generated code, and it covers
only the failures that happen to produce malformed output — a
classification/emission drift that produces *well-formed but wrong* output
(the 0110 shape) has no backstop at all. The status quo's real gap is not
"zero protection" but "no protection for the silent-wrong class plus poor
diagnosis for the loud class," and — decisively — nothing in the status
quo addresses the `self_version` save/restore hole or prevents the next
mutation-emission site from forgetting the shadow write.

### Option E: Metamorphic/differential testing first (BT-3112), IR later or never

- 🧑‍💻 / ⚙️ / 🏭 / 🎨 (shared core argument): "BT-3112's planned metamorphic harness would have caught ADR 0110's actual bug — result-preservation under semantics-preserving transforms is precisely the oracle for 'right value, lost side effect.' It hunts unknown-unknowns, covers all of codegen (not just the in-scope 30%), requires no migration of working code, and is already committed roadmap. Run it first; buy the IR only if the harness finds bug clusters it can't localize."

**Why this ADR proceeds anyway — as a complement, with the sequencing
acknowledged.** The harness and the verifier answer different questions:
the harness detects *that* a semantic property broke somewhere in a
generated program; the verifier (and the IR's single-sourcing) prevents
and localizes specific, known, named invariant violations at the source
line that introduced them, and removes the classifier/emitter dual
computation that produces them. The harness also does nothing for the
counter-discipline defect, which is a compiler-internals problem invisible
to any black-box oracle. This ADR's Phase A runs in parallel with
BT-3112's early children, and the A0 gate provides the explicit
reconsideration point if the harness's early results change the calculus.
What this ADR does *not* claim is that the verifier substitutes for the
harness (§Verifier honesty).

### Tension Points

- **A vs. B2 is the live disagreement** — reasonable engineers land on
  either side of "single-sourcing is worth a phased migration of ~11K
  LOC." This ADR's answer is conditional (the A0 gate + B2 as designated
  descope), which is as honest as the disagreement allows.
- **Scope discipline remains the whole argument for A over D** (the
  full-pipeline IR): the phase gates and the measurement gate exist to
  keep A from becoming D by accretion.
- **Freeze cost vs. correctness payoff:** the in-scope files carry active
  feature work (BT-references spanning BT-1275→BT-3055). The conflict
  protocol (Implementation, below) is: feature work lands first, migration
  phases rebase; each phase has a wall-clock cap, after which it is split
  or abandoned rather than extended.

## Alternatives Considered

### 1a. Verifier without IR — re-derive post-hoc
See Steelman Option B1. **Rejected**: re-derivation duplicates the
classifier/plan logic — the §6 anti-pattern.

### 1b. Verifier without IR — record decisions as made
See Steelman Option B2. **Not rejected — designated descope target.** If
Phase A0's measurement fails its threshold, or if BT-3112's harness
results change the value calculus before Phase B begins, this ADR's
deliverables reduce to: Phase A1's types and checks re-hosted on a
decision side-channel + Phase A2's counter unification + Phase A3's
snapshot corpus. That outcome is explicitly acceptable and pre-planned,
not a failure mode.

### 2. Status quo
See Steelman Option C. **Rejected** (no
protection for the silent-wrong class; poor diagnosis for the loud class;
`self_version` hole and shadow-write forgettability unaddressed).

### 3. Unify the three version counters only
**Absorbed rather than rejected.** This is precisely Phase A2, and it is
independently justified — done as *naming-only* unification with
per-prefix discipline preserved and pinned by tests, plus the typestate
hardening of Alternative 5. What the full ADR adds beyond it is the
single-sourcing of classifier/emitter decisions and the shadow-write
contract check. (Note the six `debug_assert!`s are *not* an argument
against this alternative: none of the six is on ADR 0110's path — they
guard the mode/unpack and classifier/emitter invariants, which counter
unification alone indeed does not address.)

### 4. Full-pipeline typed Core Erlang IR
**Rejected** — per ADR 0018's still-valid reasoning and BT-3122's explicit
non-goal.

### 5. Typestate hardening in place (newtype `VersionedVar` + RAII branch guard, no IR)
Make the version counters' invariants unrepresentable in place: a
`VersionedVar` newtype whose only constructors are the `next_*` methods,
emitter signatures taking `VersionedVar` instead of `String` (an
unproduced version can no longer be named), and an RAII guard replacing
`with_branch_context`'s manual save/restore (closing the `self_version`
hole by construction). **Absorbed into Phase A2** — this is how the
counter unification should be built regardless of whether the full IR
lands, and it delivers `UnboundVersion`-by-construction (stronger than a
check) for the counter slice specifically. It does not cover mode/unpack
membership, classifier/emitter single-sourcing, or the shadow-write
contract, which is what the IR proper adds.

### 6. Erlang-side strengthening only
The dialyzer-visible `{nlr_relay, ...}` outcome variant
(ADR 0110 §Neutral) is today's strongest guard for the 0110 contract's
runtime half. Strengthening it (EUnit conformance on
`class_method_outcome()`, an FFI-rule check) is worth doing and Phase D's
shared conformance fixture does part of it — but it cannot see the Rust
side's emission decisions at all, so it is a complement, not an
alternative. **Partially adopted** (the fixture); rejected as a
substitute.

## Consequences

### Positive
- **ADR 0110's invariant becomes structurally un-forgettable** at every
  current and future class-var mutation site (`ShadowWriteMissing` +
  the cross-boundary conformance fixture), instead of living in one
  emission site's code and one BUnit test.
- **The classifier/emitter and mode/unpack dual-computations are removed
  by construction**, not just checked — one IR node, one emitter. The six
  `debug_assert!`s are deleted with their invariants absorbed.
- **The three-counter duplication and the `self_version` save/restore hole
  are closed** with frame identity and typestate constructors (Phase A2 —
  independently valuable even under the descope outcome).
- **Diagnosis quality:** failures that today surface as erlc messages
  about generated variables surface as Beamtalk-source-attributed verifier
  errors in CI.
- **`Document`, typed leaves, printer, wire, runtime: unchanged.**
- **The decision is measurement-gated (Phase A0) with a pre-planned
  descope target (Alternative 1b)** — the ADR 0088 lesson applied
  prospectively.

### Negative
- **New internal API surface** for contributors to the in-scope files.
- **A real test-migration cost:** ~1,484 `#[test]`s
  and 231 `to_pretty_string()` assertion sites live under `codegen/`
  (`tests/control_flow.rs` alone is 2,591 lines; `list_ops/tests.rs`
  2,660). Functions returning `ThreadedIr` instead of `Document` would
  break every test asserting on their output. **Mitigation, committed in
  Phase A1:** a `lower_and_render(…) -> Document` shim preserving existing
  test call shapes verbatim; tests migrate opportunistically, never as a
  phase precondition.
- **Compile-time cost is real until measured:** IR construction allocates
  a second representation for the hottest codegen paths. Phase A0 measures
  it on a fixed fixture set against a pre-declared threshold (proposed:
  abandon/descope if end-to-end `beamtalk build` on the fixture set
  regresses > 3%; tune in `/plan-adr`).
- **Migration freeze risk on actively-developed files.** Protocol:
  feature work lands first and phases rebase; each phase carries a
  wall-clock cap (proposed: one release cycle), after which it is split
  or handed to the descope path rather than extended.
- **The verifier can produce false confidence** — it enforces stated
  invariants only. Unknown-unknown discovery remains BT-3112's harness's
  job, and this ADR says so.
- **Release-mode verifier findings are diagnostics, not errors** — a real
  compiler-internal bug in release degrades to today's behavior plus a
  warning. This is the CLAUDE.md-compliant choice, but it means the hard
  guarantee is CI-time, not user-compile-time.

### Neutral
- **`NlrBoundary`, `ThreadingPlan`, `StateAccFallbackReason`,
  `BodyExprKind` are reused, not redesigned** — the IR changes *when*
  their outputs become durable data, not what they compute.
- **The `BEAMTALK_CODEGEN_DIAGNOSTICS` pipeline keeps its wording and env
  vars; firing *order* may shift** when plan selection moves into
  lowering — ordered diagnostic-stream equality is therefore an explicit
  per-phase exit criterion rather than an assumption.
- **ADR 0110's runtime fix is untouched**; this ADR adds the compile-time
  half of its contract, plus the shared conformance fixture the
  cross-boundary rule in CLAUDE.md requires.

## Implementation

Phase-gated with a measurement gate at the front. Exit criteria for every
migration phase (B–D): byte-identical output over the **expanded** snapshot
corpus (A3), ordered diagnostic-stream equality, verifier green on all
stdlib fixtures, full behavioral suite green, and the phase's superseded
`debug_assert!`s deleted from source. The `/plan-adr` output decomposes
into issues; this section names the commitment level.

### Phase A0 — Measurement gate (S)
Prototype the IR types and lower *one* construct (`whileTrue:`
direct-params) behind a flag; measure end-to-end `beamtalk build` on a
fixed fixture set. Pre-declared gate: proceed only if regression ≤ 3%
(threshold finalized in `/plan-adr`). On failure: descope to
Alternative 1b, carrying Phases A1–A3 forward on the decision-side-channel
design.

### Phase A1 — IR types + verifier + test shim (S)
`threaded_ir.rs`: `FrameId`, `VersionedVar`, `ThreadedStmt`,
`VerifyError`, `verify()`, plus the `lower_and_render` test shim. Unit
tests against hand-built IR fixtures. Nothing consumes the IR yet — this
sub-phase is genuinely zero-behavioral-risk.

### Phase A2 — Counter unification via typestate (S, snapshot-gated)
`VersionedVar` newtype with constructor-only production, RAII branch
guard replacing `with_branch_context`'s manual save/restore, per-prefix
discipline preserved exactly (state: reset+restore; class_vars:
restore-only + sticky mutated flag; self: **given** the save/restore it
currently lacks, as an explicit, snapshot-gated behavior decision — if
snapshots change, that's a bug find, not a regression). Discipline
asymmetries pinned by dedicated tests. *Not* zero-risk, hence separated
from A1 and gated on the corpus.

### Phase A3 — Snapshot corpus expansion (S–M, prerequisite for B–D)
Land fixtures covering the mode matrix — {DirectParams, TupleAcc, Hybrid,
StateAcc} × {Actor, ValueType, ClassMethod} × {NLR present/absent} ×
{class-var mutated/not} — before any migration phase. Measured baseline
today: of 318 snapshots, `letrec` 1, `$bt_nlr` 1, `ClassVars1` 1,
`class_vars_shadow` 1, `StateAcc` 4. Without this phase the parity gate is
vacuous and the plan's "learning from ADR 0088" claim is unsupported.

### Phase B — `while_loops.rs` + `counted_loops.rs` (M)
Migrate the letrec-loop family. Soft prerequisite: BT-3125 landed (so
lowering consumes the threaded `AnalysisResult` from day one instead of
building a to-be-deleted re-derivation). Deletes the four unpack
`debug_assert!`s.

### Phase C — `list_ops/` (L)
Migrate the foldl list-op family — sized **L**, not a mechanical
follow-on to Phase B: it introduces invariant classes Phase B never
exercises, each needing IR modeling and verifier coverage: (1) the flat `{FoldAcc, Var1..VarN}` positional-unpack
accumulator discipline (distinct from parameter threading); (2)
`select_tuple_acc`'s ValueType-context exclusion (`control_flow/mod.rs:448-466`);
(3) the recursive inter-construct `list_op_needs_stateacc_fallback`
analysis for nested list ops; (4) early-exit ops (`detect:`,
`anySatisfy:`, `takeWhile:`, …) changing accumulator liveness at the exit
point. May split fold-shaped vs. early-exit-shaped in `/plan-adr`.
`list_ops/tests.rs` (2,660 lines) migrates via the A1 shim.

### Phase D — Actor/class-method threading + NLR + shadow-write contract (L)
Migrate the state-versioning slice of `gen_server/methods.rs`
(`BodyExprKind`, `generate_body_exprs_with_reply`, tier-2 helpers), the
`ClassVars` threading path **including its `Bind`-emission sites in
`expressions.rs:552` and `dispatch_codegen.rs:463`** (these are the
producers of everything the verifier checks),
`conditionals.rs`, `exception_handling.rs`, and `threaded_expr.rs`'s
boundary adapter. Lands `ShadowWriteMissing` plus the **cross-boundary
conformance fixture** pinning codegen's shadow-write emission against
`invoke_class_method/7`'s key/erase discipline (CLAUDE.md cross-boundary
rule). Deletes the two routing `debug_assert!`s. May split into sub-PRs in
`/plan-adr`.

### Conflict protocol (all phases)
Feature work in the in-scope files lands first; migration phases rebase.
Each phase has a wall-clock cap (proposed: one release cycle); a phase
that exceeds it is split or routed to the Alternative 1b descope, not
extended.

### Affected Components
- **New**: `codegen/core_erlang/threaded_ir.rs` (types, verifier, shim);
  snapshot fixtures (A3); cross-boundary conformance fixture (D).
- **Modified**: `control_flow/` (all production files), `threaded_expr.rs`,
  `gen_server/methods.rs` (state-versioning slice), `mod.rs` (counters →
  `VersionedVar` + RAII guard), **`expressions.rs` and
  `dispatch_codegen.rs` (version-`Bind`/shadow-write emission sites only)**.
- **Unchanged**: `document/` (ADR 0089), `intrinsics.rs`,
  `value_type_codegen.rs`, the remainder of `expressions.rs`/
  `dispatch_codegen.rs`, all Erlang-side runtime code (the conformance
  fixture asserts against it but does not change it), wire format.

### Verification
- **Expanded snapshot corpus (A3) byte-parity** after every phase — the
  real gate; the pre-expansion corpus covers the in-scope constructs in
  only 1–4 of 318 snapshots and cannot serve as one.
- **Ordered diagnostic-stream equality** (`BEAMTALK_CODEGEN_DIAGNOSTICS`
  output as a sequence, not a set) after every phase.
- **Verifier green** on all `stdlib/test/*.bt` +
  `stdlib/bootstrap-test/*.btscript` fixtures in CI (`just` target named
  in `/plan-adr`).
- **Behavioral suite** (`just test-stdlib`/`test-bunit`/
  `test-repl-protocol`) green after every phase.
- **0110 contract, both halves**: `ShadowWriteMissing` unit tests (fires
  when a top-frame class-var Bind in a `has_class_vars` method lacks the
  shadow write; silent on the fixed shape) + the cross-boundary
  conformance fixture against `invoke_class_method/7`. Honest framing per
  §Verifier honesty: regression-pinning, not counterfactual detection.

## References

- Related issues:
  - [BT-3122](https://linear.app/beamtalk/issue/BT-3122) — this ADR
  - [BT-3111](https://linear.app/beamtalk/issue/BT-3111) /
    [BT-3125](https://linear.app/beamtalk/issue/BT-3125) — analysis→codegen
    handoff; BT-3125 is a soft prerequisite for Phase B onward
  - [BT-3112](https://linear.app/beamtalk/issue/BT-3112) — generated-code
    correctness epic; its metamorphic harness is the unknown-unknown
    hunter this ADR's verifier explicitly does not replace (§Verifier
    honesty, Steelman Option E)
  - [BT-3035](https://linear.app/beamtalk/issue/BT-3035) — ADR 0110's
    implementation epic; `ShadowWriteMissing` + the conformance fixture
    pin its invariant
  - BT-3115 — `core_lint` readability (the release-mode backstop this
    ADR's checks improve upon rather than replace)
- Related ADRs:
  - [ADR 0018](0018-document-tree-codegen.md) — `Document` tree; its
    §Alternatives #3 rejection of a full-pipeline IR still holds for
    everything out of this ADR's scope
  - [ADR 0041](0041-universal-state-threading-block-protocol.md) — the
    `{Value, StateAcc}` and 4-tuple NLR conventions formalized as IR data
  - [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) — the
    semantic split `NlrBoundary` encodes
  - [ADR 0088](0088-direct-cerl-emission.md) — wire-format proposal,
    closed; source of the measurement-gate discipline Phase A0 applies
  - [ADR 0089](0089-typed-document-leaves.md) — typed-leaf printer,
    unchanged; precedent for byte-parity migration (via ad-hoc `cmp -l`,
    not a standing harness — which is why A3 exists)
  - [ADR 0109](0109-block-scoped-class-methods-run-blocks-in-the-caller.md)
    — block-runs-where-invoked semantics underlying the relay problem
  - [ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md) — the
    shipped bug whose invariant this ADR pins; runtime fix untouched
- Documentation:
  - `docs/development/debugging.md` §"Codegen Diagnostics", §"core_lint"
  - `docs/development/architecture-principles.md` §6 (duplication /
    shared-leaf pattern), CLAUDE.md cross-boundary conformance rule
- Code (all under `crates/beamtalk-core/src/codegen/core_erlang/` unless
  noted):
  - `control_flow/` (13 files, 16,329 LOC), `threaded_expr.rs` (416),
    `gen_server/methods.rs` (5805; ≈1900–2400 in scope) — in scope
  - `expressions.rs:552`, `dispatch_codegen.rs:463` — the
    version-`Bind`/shadow-write emission sites (in scope)
  - `mod.rs` — `NlrBoundary` (:902), `NlrCatchVars` (:880),
    `wrap_body_with_nlr_catch` (:2480), `with_branch_context` (:2064),
    counters (:1132, :1211, :2088, :2104)
  - `state_codegen.rs:42` (`StateThreading`), `util.rs:38`
    (`versioned_var`)
  - `../core_erlang_validity_tests.rs` — existing text-shape properties,
    unaffected
  - `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` —
    `invoke_class_method/7`, the Erlang half of the 0110 contract
  - `runtime/apps/beamtalk_compiler/src/beamtalk_compile_diagnostics.erl`
    — `core_lint` integration
