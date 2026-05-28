# ADR 0088: Direct Core Erlang AST Emission via ETF

## Status

**Phases 1–4: Rejected (2026-05-28).** Phase 0c measured the
typed-Document-leaves alternative against the cerl-direct rewrite on the
same three functions used in Phase 0a — typed-leaves came in at −8.7%
aggregate char shrinkage vs cerl's −9.5% (within 0.8% on the aggregate;
typed-leaves wins on 2 of 3 function tiers). With typed-leaves capturing
essentially the same shrinkage at ~30× the migration cost ratio, the
58K-LOC cerl-direct migration does not pay back. The recommendation in
[Phase 0c memo](0088-phase-0c-typed-leaves.md) is **withdraw ADR 0088
in favour of the typed-leaves refactor** as the long-term fix for the
BT-875 recurrence vector.

**The wire-as-ETF question is Deferred, not rejected.** Phase 0b
([memo](0088-phase-0b-napkin.md)) found ETF encode+decode is only
~2.4–3.4% of per-compile cost — small enough that ETF is viable — and
the cerl wire beat the text wire end-to-end by ~7.7–10% on the same
fixtures because `core_scan` + `core_parse` cost ~4–4.5× more than
`binary_to_term`. If compile time becomes a real bottleneck on
larger projects, this win can be reclaimed by a much smaller
*independent* ADR (wire-only, no codegen restructure) — by then the
codegen will already be typed-leaves clean and only the wire question
remains. **Do not re-open ADR 0088 itself**; the bundled scope (typed
codegen + wire change + 58K LOC migration) is what's rejected.

**Phase 0 audit history (all merged):**

| Phase | Memo | PR | Finding |
|---|---|---|---|
| 0a — Codegen shrinkage audit | [`0088-phase-0a-audit.md`](0088-phase-0a-audit.md) | [#2348](https://github.com/jamesc/beamtalk/pull/2348) | ~9.5% projected shrinkage by char count — in-between the 5% / 15% gates; qualified |
| 0b — Wire-mechanism napkin | [`0088-phase-0b-napkin.md`](0088-phase-0b-napkin.md) | [#2350](https://github.com/jamesc/beamtalk/pull/2350) | ETF cost = 2.4–3.4% of per-compile time; pivot-to-Alternative-7 gate does not fire |
| 0c — Typed-leaves comparison | [`0088-phase-0c-typed-leaves.md`](0088-phase-0c-typed-leaves.md) | [#2352](https://github.com/jamesc/beamtalk/pull/2352) | Typed-leaves shrinkage within 0.8% of cerl on the aggregate; decisive against Phase 1 |

**Decision criteria that fired and why:**

- The ADR's Phase 0a "≥15% shrinkage proceeds unconditionally" gate did
  not fire (~9.5% < 15%).
- The ADR's Phase 0a "≤5% shrinkage withdraw" gate did not fire either
  (~9.5% > 5%); Phase 0a's verdict was qualified.
- Phase 0b's "pivot to Alternative 7" gate did not fire (ETF cost is
  small).
- Phase 0c's BT-2316 criterion *did* fire: "if typed-Document-leaves
  comes in within ~3% of cerl's shrinkage, ADR 0088 should be
  withdrawn in favour of the typed-leaves refactor." The measured
  delta is 0.8%, well inside that threshold.

**What is left intact for downstream work:**

- The Phase 0a + 0b throwaway audit modules (`cerl_audit.rs`, `cerl.rs`,
  the EUnit wire tests, the timing harnesses) live on as references
  but are explicitly marked throwaway. They can be deleted once
  typed-leaves is shipped.
- BT-aware stack traces (one of the original Phase 1+ motivators) are
  still a committed downstream consumer, but they do not require cerl
  on the wire — annotations can be carried in the typed-leaves
  approach via per-leaf metadata or a separate side-band channel.
  Plan as part of the typed-leaves rollout ADR.

**Downstream replacement ADR:** [ADR 0089](0089-typed-document-leaves.md)
turns the Phase 0c recommendation into a buildable plan — the
typed-Document-leaves refactor that closes the BT-875 recurrence
vector structurally without re-opening the cerl-wire question.

---

**Original Status (now superseded — Proposed, 2026-05-26):** Phase 0a + 0b only. Phases 1–4 are explicitly contingent on two decision gates:

- **Phase 0a (Codegen Shrinkage Audit)** — 3 representative codegen functions rewritten cerl-direct, projected LOC delta across the codebase. If the projection is ≥15% shrinkage, the migration pays back its own LOC cost and is justified on simplification grounds alone. If ≤5%, the typed-Document-leaves alternative (see *Why not just constrain `Document` leaves?* below) likely wins for the BT-875 vector, and the remaining justification rests on annotation-consumer roadmap.
- **Phase 0b (Napkin)** — ETF encode/decode timing on representative-sized modules. If ETF cost dominates, pivot to Alternative 7 (Port + NIF-for-conversion) before Phase 1.

BT-aware stack traces are now a committed downstream consumer (see *Downstream Consumers*), so the annotation-fidelity leg of the justification is no longer hypothetical. The full source-level debugger remains aspirational.

Approval of this ADR authorises Phases 0a + 0b and the data-gathering they enable; it does not pre-authorise the 58K LOC codegen migration.

## Context

ADR 0022 established the OTP Port architecture for the embedded compiler: the Rust `beamtalk-core` runs as a long-running Port subprocess, returns Core Erlang as a binary in an ETF response, and the Erlang side compiles it to BEAM bytecode fully in-memory. The current implementation lives in `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl`:

```erlang
%% beamtalk_compiler_server:compile_core_erlang/1
compile_core_erlang(CoreErlangBin) ->
    CoreErlangStr = binary_to_list(CoreErlangBin),
    case core_scan:string(CoreErlangStr) of
        {ok, Tokens, _} ->
            case core_parse:parse(Tokens) of
                {ok, CoreModule} ->
                    case compile:forms(CoreModule, [from_core, binary, return_errors]) of
                        {ok, ModuleName, Binary} -> {ok, ModuleName, Binary};
                        {ok, ModuleName, Binary, _Warnings} -> {ok, ModuleName, Binary};
                        {error, Errors, _Warnings} -> {error, {core_compile_error, Errors}}
                    end;
                {error, ParseError} -> {error, {core_parse_error, ParseError}}
            end;
        {error, ScanError, _Loc} -> {error, {core_scan_error, ScanError}}
    end.
```

ADR 0018 established the Wadler-Lindig `Document` tree as the only sanctioned way to construct Core Erlang output (BT-875 cleanup enforces this — no `format!()` or string concatenation in codegen). The Document tree is rendered to a Core Erlang text binary that is sent across the Port to the Erlang side.

### The serialization tax

The current pipeline encodes the same structural information three times:

1. **Rust codegen** builds a `Document` tree (~58K LOC of typed combinator calls across ~55 source files in `crates/beamtalk-core/src/codegen/core_erlang/`).
2. **Document renderer** flattens the tree to a Core Erlang text binary — pretty-printer machinery (`Group`, `Break`, `Nest`) exists to format text readably, but no human reads the output before it crosses the Port.
3. **Erlang compiler server** calls `core_scan:string/1` and `core_parse:parse/1` to rebuild a structured representation (`cerl` records) from the text, then passes those records to `compile:forms/2` for the rest of the lowering pipeline (kernel → asm → BEAM).

Each compile pays for steps 2 and 3 — building text from structure, then rebuilding structure from text — for no semantic gain. `compile:forms/2` accepts `cerl` records directly (the `from_core` option). If the Rust side produced `cerl`-shaped data, encoded it as ETF, and the Erlang side called `binary_to_term/1` followed by `compile:forms/2`, steps 2 and 3 disappear.

### What's already in place

- **ETF transport**: `crates/beamtalk-etf/` (459 LOC) already encodes Erlang terms from Rust. The Port channel established by ADR 0022 already speaks ETF for request/response framing.
- **In-memory compile path**: ADR 0022 already established that the Erlang side calls `compile:forms/2` in-process — there is no `erlc` subprocess to eliminate. The only wasted step is `core_scan` + `core_parse` reconstructing forms from text the Rust side just rendered.
- **Document API discipline**: BT-875 forced all codegen onto the `Document` combinator API — the *discipline* (build structured data, never strings; per-fragment unit testability) is what we keep. The Document *API surface* itself is text-rendering-shaped (its `nest`/`group`/`break` combinators are pretty-printer primitives with no analogue on a typed AST), so it does not survive the migration; codegen functions instead return typed `cerl::Expr` values directly.

### Constraints

- **Core Erlang remains the target IR** (ADR 0003). This decision is about *how* Core Erlang reaches `compile:forms/2`, not *whether* Core Erlang is the right IR.
- **ADR 0018 discipline preserved** (BT-875). The principle of building structured data (never strings) carries forward; codegen functions construct typed `cerl::Expr` values directly. The `Document` API surface itself is text-rendering-shaped and does not carry over — it survives only as a transitional adapter (`cerl_to_doc`) during Phase 3 and is deleted in Phase 4. No regression to ad-hoc string construction.
- **Port boundary preserved** (ADR 0022). No NIF embedding. The Rust ↔ BEAM seam stays at the OS process boundary.
- **Atom-table safety** (ADR 0022 implementation). The current `binary_to_term/1` calls on Port responses use `[safe]` to prevent atom-exhaustion attacks from a buggy or corrupted Rust payload. The new `{cerl, Etf}` decode must use `[safe]` too. This means every atom appearing in a cerl-ETF payload must already exist as an Erlang literal — feasible because the structural atoms (cerl record tags like `c_module`, `c_fun`, `c_var`, ...) are a fixed finite set, and Beamtalk-generated module/function/atom names are mangled into a stable namespace that's already pre-allocated for the text path. Phase 0b's ETF byte-equivalence tests must include a `binary_to_term(_, [safe])` round-trip assertion to catch any payload that introduces unknown atoms.
- **Codegen test discipline preserved**: ~245 inline `#[test]` codegen unit tests in `crates/beamtalk-core/src/codegen/core_erlang/tests/` (assert on `Document.to_pretty_string()` output) and the proptest parseability suite in `core_erlang_validity_tests.rs` must continue to pass during transition. End-to-end stdlib/BUnit/REPL test suites verify behaviour parity at the BEAM level.

## Decision

**Replace text-rendered Core Erlang on the Port wire with ETF-encoded `cerl` AST terms.**

Concretely:

1. Add a Rust mirror of the `cerl` record types (`c_module`, `c_fun`, `c_var`, `c_literal`, `c_cons`, `c_tuple`, `c_map`, `c_let`, `c_letrec`, `c_case`, `c_clause`, `c_apply`, `c_call`, `c_primop`, `c_try`, `c_catch`, `c_receive`, `c_binary`, plus annotation wrappers — ~20–30 node types).
2. Each Rust `cerl::*` node has an ETF encoder that produces the byte-for-byte equivalent of what `term_to_binary/1` would produce on the corresponding Erlang record.
3. Migrate codegen function return types from `Document` to `cerl::Expr` (or the appropriate cerl node type). The `Document` API itself is a Wadler-Lindig *pretty-printer* tree — its layout combinators (`nest`, `group`, `break`, `line`) only make sense when producing text and have no meaning on a typed AST. So the long-term shape is not "Document over cerl leaves" but cerl values composed directly: `let_expr(...)`, `case_expr(...)`, `apply(...)`. A small adapter `cerl_to_doc :: cerl::Expr -> Document` exists during the transition so a migrated function can be called from a still-text-emitting parent. ADR 0018's *discipline* (build structured data, never strings; per-fragment unit testability) carries forward; its *API surface* does not — the Document API is deleted in Phase 4 along with the text wire.
4. The Port message format gains a `cerl` response variant alongside the existing `core_erlang` text variant. The Erlang side decodes ETF, calls `compile:forms(Forms, [from_core, binary, return_errors])`, and `code:load_binary/3` as today.
5. Migrate codegen functions opportunistically (per ADR 0018's organic-migration model) — each function's return type changes from `Document` to the corresponding cerl node type. The full behavioural test suite (`just test-stdlib`, `just test-bunit`, `just test-repl-protocol`) verifies *behaviour* parity at every step; the `just codegen-diff` BEAM-diff harness runs as a diagnostic, but byte-for-byte BEAM parity is not enforced (see Consequences).
6. After all codegen has migrated, remove the text path: delete the `Document` type and its API, delete the `cerl_to_doc` transitional adapter, and delete the `core_scan`/`core_parse` calls in `beamtalk_compiler_server` and `beamtalk_build_worker`.

### Why not just constrain `Document` leaves?

The Option B "compiler architect" steelman raises the sharpest counter to this ADR: ADR 0018 already gave us typed codegen, and the BT-875 recurrence vector is the *one* typed-leaf escape hatch — `Document::String(...)` accepts arbitrary text. That escape hatch is closable in ~1–2K LOC by replacing `Document::String` with a sum of typed leaves (`Atom`, `VarName`, `StringLit`, `IntLit`, ...) and forbidding raw-string construction at module boundaries. If BT-875 elimination is the *primary* motivation, that change is roughly 30× cheaper than the 58K LOC migration this ADR proposes, and it preserves the Document combinator surface and all 245 unit tests unchanged.

We do not refute that argument — we **scope around** it. The typed-leaf refactor closes BT-875 but captures *none* of the other benefits this ADR targets: it does not remove the `core_scan`/`core_parse` round-trip, does not enable cerl-node annotation propagation to BEAM bytecode, does not give us the same idiomatic interface LFE uses, and does not eliminate the text/AST boundary at which source positions are lost today. BT-aware stack traces (see *Downstream Consumers* below) are now a committed near-term feature whose enabling infrastructure requires the cerl wire — they cannot be delivered by the typed-leaf refactor alone. So the contingency in the Status section narrows: the typed-leaf alternative wins only if **both** (a) the Phase 0a audit shows cerl-direct does not materially shrink codegen, **and** (b) Phase 0b's napkin timing data shows ETF is dominant cost (pushing toward Alternative 7 anyway). With at least one downstream consumer (stack traces) committed, the annotation-fidelity leg of the justification is no longer hypothetical.

### Downstream Consumers

The annotation-fidelity benefit only counts as a real argument for this ADR if scheduled work consumes those annotations. Today:

| Consumer | Status | What it needs from the cerl wire |
|---|---|---|
| **BT-aware stack traces** | Scheduled near-term | Cerl nodes carrying `{file, line}` annotations (Phase 1 covers this), `debug_info` chunks in the compiled BEAM (already true on `beamtalk_build_worker`; the compiler-server path would need it added), and Beamtalk's AST reliably propagating positions to every codegen call site (separate upstream work, partially complete today). Estimated weeks of work *after* Phase 1 lands. Payoff hits every runtime error a user sees. |
| **Source-level debugger** | Aspirational | Cerl annotations as above, plus breakpoint resolution (`.bt` line → BEAM instruction), variable-name remapping (cerl `c_var` ↔ Beamtalk identifier), step semantics that respect Beamtalk block/closure boundaries, and DAP/`int`/`dbg` integration. Multi-month effort; OTP's debugger ecosystem is thin. The cerl wire is necessary but a long way from sufficient. |
| **Dialyzer integration** | Not scheduled | Cerl annotations + Beamtalk-typed specs surfaced through the codegen. Listed as a possibility, not a roadmap item. |

The presence of **at least one scheduled consumer** (stack traces) is what flips the annotation-fidelity argument from hypothetical to material. Both Phase 0a (audit) and Phase 0b (napkin) decisions should weigh stack-trace delivery as a real downstream constraint, not a "nice if it works out" benefit. If the stack-traces roadmap item is descoped, this section should be revisited — the ADR's justification would meaningfully weaken.

### What this looks like

**Today** — codegen produces text, BEAM re-parses it:

```rust
// crates/beamtalk-core/src/codegen/core_erlang/expressions.rs (illustrative)
fn gen_call(module: &str, fun: &str, args: Vec<Document>) -> Document {
    docvec![
        "call '", module, "':'", fun, "'(",
        join(args, ", "),
        ")"
    ]
}
```

```erlang
%% compile_core_erlang/1 (simplified, error cases elided)
{ok, Tokens, _}        = core_scan:string(binary_to_list(CoreErlangBin)),
{ok, CoreModule}       = core_parse:parse(Tokens),
{ok, Mod, Bin}         = compile:forms(CoreModule, [from_core, binary, return_errors]).
```

**After** — codegen produces typed nodes, BEAM consumes them directly:

```rust
// crates/beamtalk-core/src/codegen/core_erlang/expressions.rs
fn gen_call(module: Atom, fun: Atom, args: Vec<cerl::Expr>) -> cerl::Expr {
    cerl::Expr::Call {
        module: Box::new(cerl::Expr::Literal(cerl::Literal::Atom(module))),
        name:   Box::new(cerl::Expr::Literal(cerl::Literal::Atom(fun))),
        args,
    }
}
```

```erlang
%% compile_core_erlang/1 — new cerl variant (simplified, [safe] decode)
CoreModule     = binary_to_term(CerlEtf, [safe]),
{ok, Mod, Bin} = compile:forms(CoreModule, [from_core, binary, return_errors]).
```

The Rust-side change is the larger one (return types of every codegen function over time, plus the new `cerl::*` module). The Erlang-side change is small in code size but is a contract change between `beamtalk_compiler_server` / `beamtalk_build_worker` and the Rust compiler — both Erlang call sites migrate in lockstep.

### What stays the same

- ADR 0018's **discipline**: build structured data, never strings; per-fragment unit testability; BT-875 invariant. Codegen call sites still construct values, not concatenate text — they just construct `cerl::Expr` values directly instead of `Document` values.
- The Port wire is still ETF; the existing length-prefixed framing and request/response shape don't change.
- `compile:forms/2` and `code:load_binary/3` are exactly the same calls.
- All optimization passes (`sys_core_fold`, kernel, asm) run as before — we're feeding them the same `cerl` they'd get from `core_parse:parse`.

### What changes

- The `Document` API surface (`docvec!`, `nest`, `group`, `break`, `line`) is **text-rendering-shaped** and does not carry over. Codegen functions return `cerl::Expr` directly. Migrated call sites read like cerl constructors (`let_expr(var, value, body)`) rather than like pretty-printer combinators (`docvec![...]`). The Document API is preserved only on the text-leaf transition path and is deleted in Phase 4.

## Prior Art

| Compiler | Approach | Notes |
|----------|----------|-------|
| **Pharo / Squeak (Smalltalk)** | Compiler lives entirely inside the image; parsing, codegen, and bytecode installation are all in-process method calls operating on objects. There is no wire format. | The "Smalltalk way" (ADR 0022) — no transport at all, because compiler and runtime share an address space. We *can't* match this directly (our compiler is Rust, our runtime is BEAM), but the principle that the compiler should communicate in structured data, never text, is the same principle. |
| **Elixir** | Builds Erlang AST (`erl_parse:abstract_form()`) directly via quoted forms; never goes through text. | Closest precedent for "skip the text serialization entirely." Elixir compiles quoted forms to BEAM in-process via the same `compile`-module entry points we'd use. |
| **LFE** | Compiles Lisp s-expressions to `cerl` records directly, calls `compile:forms/2`. | Demonstrates that targeting `cerl` (rather than Erlang abstract forms) is viable for non-Erlang-syntax languages. |
| **Erlang itself** | The `compile` module's own pipeline preserves annotations through `cerl` → kernel → asm → BEAM. Source positions, file/line info, and custom annotations on `cerl` nodes survive to the final bytecode. | Gives us a path to better diagnostics: nodes carrying `{file, Line, Col}` annotations enable downstream warnings (e.g., from `sys_core_fold`) to map back to `.bt` source. |
| **Gleam** | Renders Erlang **source text** to `.erl` files, then shells out to `erlc`. | Chose readability/portability over wire optimization. Gleam targets the Erlang source level for end-user readability; we target Core Erlang for codegen simplicity (ADR 0003), so the same trade-off doesn't apply. |
| **rustc** | Builds typed HIR/MIR in-memory throughout; never renders to text between phases. | Confirms that "structured all the way down" is the modern compiler default. Text emission is a debug/inspection feature, not a transport. |

**Pattern:** Compilers that share a host runtime with their target VM (Pharo in-image, Elixir on BEAM, LFE on BEAM, rustc to its own backends) keep structured data in-memory. Compilers that render text (Gleam, our current pipeline) do so when there's a strong reason — user readability or process isolation. Once ADR 0022 collapsed the daemon/erlc subprocess into an in-VM `compile:forms/2` call, the readability rationale stopped applying to us — no human reads the wire bytes. Our process-isolation reason (Port boundary) is independent of the wire format and is preserved.

## User Impact

| Persona | Impact |
|---------|--------|
| **Newcomer** | Invisible. Same language, same REPL output, same `.beam` artifacts. Marginally faster compile (skips `core_scan`/`core_parse`). |
| **Smalltalk developer** | Invisible. No semantic changes. |
| **Erlang/BEAM developer** | Mildly positive. The compiler now uses the same idiomatic interface (`cerl` + `compile:forms`) that LFE and other BEAM languages do. `.core` debug dumps can still be produced on demand by calling `core_pp:format/1` on the cerl term — same output, just opt-in. |
| **Production operator** | Marginally positive. Lower per-compile CPU (no scan+parse pass), lower per-compile memory (no intermediate text binary). No new failure modes — the Port boundary and supervision are unchanged. |
| **Compiler contributor** | Net positive but with adjustment cost. Codegen produces typed nodes instead of text fragments: type errors catch mis-shaped cerl at compile time instead of as `core_parse` errors at runtime. No string-escape bugs (cerl atoms and strings are values, not text needing escaping). Source-position annotations attach to cerl nodes uniformly. Per-fragment unit testing remains as ADR 0018 enabled it. **Adjustments:** Contributors must learn the cerl record shapes (well documented in OTP, but a new vocabulary), can no longer grep-debug generated source files directly (need `--emit-core` to dump pretty-printed text on demand), and unit-test assertions move from `assert_eq!(doc.to_pretty_string(), "...")` to structural matches on cerl AST values. |
| **Tooling developer** | Positive. Annotations on cerl nodes flow through to `sys_core_fold` warnings, `dialyzer`, and any future source-mapped debugger. Today's text path loses annotations at the `core_scan`/`core_parse` boundary unless we re-emit `-file`/`-line` directives manually. |

### Discoverability

No user-facing change. Contributors writing new codegen (post Phase 2) return `cerl::Expr` values directly — composed using cerl constructors (`let_expr(...)`, `case_expr(...)`, `apply(...)`) rather than the pretty-printer combinators (`docvec!`, `join`, `nest`) the text path uses today. The `cerl::*` types are new vocabulary but map 1:1 to OTP's existing `cerl` module (well-documented, stable across recent OTP versions). During Phase 3, the `cerl_to_doc` adapter lets migrated functions still embed into unmigrated parents; that adapter is deleted in Phase 4 along with the rest of the `Document` API.

## Steelman Analysis

### Option A: Direct cerl Emission via ETF (Recommended)

- 🧑‍💻 **Newcomer contributor**: "When I'm adding a new codegen function, the Rust type system tells me if I've assembled a malformed Core Erlang construct. I don't have to wait for `core_parse` to reject my string and decode an Erlang parse error to find the bug."
- 🎩 **Smalltalk purist**: "BT-875's cleanup commits — every `Document::String(format!(...))` violation, every escape-bug PR — only happen because the current `Document` API has a *typed leaf that accepts arbitrary text*. The discipline is convention; the type system permits it. `cerl::Expr` removes the escape hatch — there is no leaf that accepts a raw string, so the bug class becomes unrepresentable. That's the strongest argument: not 'cerl is fancier' but 'BT-875 keeps recurring because the type system doesn't actually prevent it.'"
- ⚙️ **BEAM veteran**: "`compile:forms/2` with `from_core` is *the* way to drive the Core Erlang compiler from another language on BEAM. It's what LFE does. We're already doing the in-memory `compile:forms` call (ADR 0022); we just have a needless `core_scan`/`core_parse` round-trip in front of it."
- 🏭 **Operator**: "Fewer codegen failure modes reach production. Mis-escaped atoms or malformed expressions become Rust type errors at build time instead of `core_parse` errors at runtime. Compile latency improvement is bonus — the real operator win is that the BT-875 class of bug becomes impossible to ship."
- 🎨 **Language designer**: "Annotations on cerl nodes survive to BEAM bytecode. This is the foundation for proper source-mapped diagnostics, dialyzer integration, and debugger support — none of which work cleanly when our generated text loses position information at the text/AST boundary. *Caveat*: this benefit requires Beamtalk's AST to carry source positions through to codegen call sites, which today it does only partially. The ADR enables the downstream end; the upstream end is separate work that must happen for the benefit to materialise."

### Option B: Keep Text Emission (Status Quo)

- 🧑‍💻 **Newcomer contributor**: "I can `cat module.core` and read what was generated. The text path is debuggable in a way structured terms aren't — diffing two Document outputs is a string diff; diffing two cerl ETF blobs is opaque without specialised tooling."
- 🎩 **Smalltalk purist**: "Don't fix what isn't broken. The ~245 codegen unit tests plus stdlib/BUnit/REPL end-to-end suites prove the text path produces correct BEAM. Ship features."
- ⚙️ **BEAM veteran**: "The `core_scan`/`core_parse` calls are stable OTP, they're not going away, and they're not slow enough to matter — the per-compile cost is dominated by Beamtalk's own analysis, not the Erlang side. The performance argument for cerl-direct is approximately zero in practice."
- 🏭 **Operator**: "Any refactor of 58K LOC of codegen is a regression risk. The pipeline is six months stable. Stability beats theoretical optimization, and the migration's '50 sites remaining' Phase 4 trigger is the same forcing function pattern ADR 0018 used — and ADR 0018's migration has lingered. Long transitions get half-done."
- 🎨 **Language designer**: "Text is a versioned, debuggable, interoperable wire format. cerl record layouts change between OTP versions (rare, but it has happened). Coupling our codegen to a private OTP record shape is a long-term maintenance bet."
- 🛠️ **Sharpest argument — Compiler architect**: "ADR 0018 *already* gave us typed codegen. The `Document` type already prevents string-concatenation bugs at the combinator level. BT-875 was about *reverts to string concatenation*, not about Document failing as an abstraction. Why is `cerl::Expr` worth a 58K LOC migration when `Document` is already typed? The proposed answer — that `Document::String(format!(...))` is a typed escape hatch — is real, but it's also fixable with a much cheaper change: forbid `Document::String` constructors at module boundaries, or replace `Document::String` with a `Atom`/`VarName`/`StringLit` sum type. The 'cerl-direct' migration is a hammer for a problem that has a screwdriver-shaped solution."

### Option C: NIF-based cerl Construction (Embed ERTS into Rust)

- 🧑‍💻 **Newcomer contributor**: "One fewer concept to learn. There's no ETF spec, no wire-format versioning, no encoder/decoder pair to keep in sync — the Rust function returns a value and the BEAM consumes it. Mental model maps directly to a function call across an FFI boundary, which is more familiar than 'serialize, ship, deserialize'."
- 🎩 **Smalltalk purist**: "The deeper Smalltalk principle isn't 'compiler in the image' — it's that the compiler is *reachable* and *modifiable* from the live environment. A NIF-embedded compiler is dlopened into the running BEAM; you can inspect its dispatch table, hot-reload it without restarting the runtime, instrument its entry points from BEAM tooling. That's strictly more accessible than a Port subprocess whose internals are opaque to `observer` and `recon`."
- ⚙️ **BEAM veteran**: "ADR 0022's NIF rejection was based on `compile:forms/2` being a long-running call that would block schedulers. That argument doesn't apply here — the NIF would only do the cerl Rust → cerl BEAM term conversion (a microsecond-scale pure data transform), then call `compile:forms/2` from the same BEAM process via the standard Erlang API. The actual compile remains on a normal Erlang scheduler, supervised normally."
- 🏭 **Operator**: "The 'a NIF crash kills the BEAM' framing is overbroad in 2026. Rustler with panic boundaries plus dirty NIFs for long operations is a different safety story than raw `enif_*` C code. The real failure mode for a *conversion-only* NIF is malformed input, which can be caught and returned as `{error, Reason}` without escaping the NIF — the same way the existing ETF decode path catches malformed terms today."
- 🎨 **Language designer**: "The strongest case for a NIF here isn't latency — it's *fidelity*. ETF is a *copy*: the Rust-side cerl tree is encoded, transmitted, decoded, and reconstructed as a fresh BEAM term tree. A NIF can construct the BEAM term directly in the destination heap, sharing literals (atom tables, common subtree literals like `nil`) by reference. For a large codegen output (thousands of nodes), that's a measurable memory and latency win that no amount of ETF encoder optimisation can match."
- 🪓 **Sharpest argument — Hybrid Port + NIF-for-conversion**: "The ADR addresses this in Alternative 7 but defers it. The deferral is reasonable *if* Phase 0b's napkin shows ETF encode/decode is a small share of per-compile cost — but for large modules with thousands of cerl nodes, heap-resident term construction with literal sharing could be a meaningful win that ETF can't match by design. If the napkin data turns out the other way (encode/decode dominates), the right move is not 'ship ETF anyway' but 'pivot to the hybrid'. The ADR's commitment to ETF first should be explicitly contingent on the napkin's timing data, not implicit."

### Tension Points

- **Debuggability of the wire vs. wire efficiency**: Text dumps are easier for humans; cerl terms are easier for machines. Mitigation: `core_pp:format/1` lets us reconstruct text on demand for debugging, so we keep human-readability as an opt-in tool, not the always-on wire format.
- **Coupling to OTP `cerl` record shapes**: Cerl records are documented in OTP but technically internal. Mitigation: the shapes have been stable since OTP 18 (2014); the migration's per-file structure means we could re-target a different Erlang AST shape if OTP ever broke us, with the same combinator surface.
- **Refactor cost vs. payoff**: 58K LOC of codegen migration is a large refactor. The compile-speed win is small; the type-checked-codegen win partially overlaps with what ADR 0018 already delivered (see compiler-architect steelman above). The genuinely *new* benefit is: (a) string-leaf escape-hatch elimination (closes the BT-875 recurrence vector), plus (b) annotation infrastructure for downstream tooling. Whether (a) + (b) justifies the migration is the decision's core risk. Mitigation: the per-file migration model means the cost is amortised across normal feature work, and Phase 0b's napkin produces a real cost estimate before any large commitment.
- **Tooling for inspecting generated cerl**: Today a contributor can read `.core` files. With cerl-terms-only, contributors need a `--emit-core` flag to dump `core_pp:format/1` output. Acceptable cost; small new tool.
- **Annotation fidelity depends on upstream work**: A primary advertised benefit (source positions surviving to BEAM) only materialises if Beamtalk's AST tracks source positions consistently to codegen call sites — which today it does only partially. This ADR enables the downstream half of that pipeline; the upstream half is unrelated work that must happen for the benefit to be real. The proposal should not be sold on annotation fidelity alone, because the cerl wire is necessary but not sufficient.
- **Long-transition risk**: The per-PR organic-migration model has a track record (ADR 0018) of stretching out for a year+ with a long tail of un-migrated call sites. The Phase 4 trigger (≤50 sites) is a soft forcing function; if it doesn't fire within a defined window, the codebase carries the maintenance cost of two parallel paths indefinitely. Mitigation: add a hard checkpoint at 12 months — if migration is <60% by then, escalate to a dedicated sprint rather than continuing organic.

## Alternatives Considered

### 1. Keep Text Emission (Status Quo)

Continue rendering `Document` trees to Core Erlang text, ship text across the Port, decode with `core_scan` + `core_parse` on the BEAM side.

**Rejected because:**
- The `core_scan`/`core_parse` round-trip is pure overhead — we render structure to text then immediately parse it back. No reader exists between the two steps.
- Source-position annotations are difficult to preserve through text emission. Today we'd have to manually insert `-file`/`-line` directives across ~55 codegen files; cerl annotations attach uniformly via the AST.
- The string-escape bug class (BT-875) is fundamentally a text-level problem. With cerl terms, atoms and binaries are values; no escape boundary exists.
- The "text is debuggable" advantage is preserved by `core_pp:format/1` — we keep readable output as a debug tool, not as the always-on transport.

### 2. Direct Erlang Abstract Forms (`erl_parse:abstract_form()`) Instead of cerl

Construct Erlang surface-syntax AST in Rust, encode via ETF, call `compile:forms/2` without `from_core`.

**Rejected because:**
- Erlang abstract forms are a *larger* surface area than cerl — they encode syntactic sugar (list comprehensions, records, macros) that the Erlang compiler then desugars to cerl.
- We'd be re-encoding decisions the compiler has already made: our codegen already produces post-desugar shapes (explicit case expressions for pattern matching, explicit gen_server dispatch trees). Targeting cerl matches the level we're already at.
- ADR 0003 already chose Core Erlang over Erlang source for codegen simplicity. The same argument applies to the AST: cerl is simpler and matches our intent.

### 3. Full NIF-based Compiler (Embed Rust Compiler into BEAM)

Use Rustler or hand-rolled NIFs to host the *entire Rust compiler* in the BEAM VM, constructing `cerl` terms in the BEAM heap directly and calling `compile:forms/2` from inside the NIF or from the calling Erlang process.

**Rejected because:**
- ADR 0022 deferred this specifically because a NIF crash kills the BEAM node (taking running actors with it). The same risk profile applies: the full Beamtalk compile pipeline is a 10–500 ms operation exercising complex code paths, exactly the wrong workload for an in-process NIF — and dirty NIFs only partially mitigate it (they avoid scheduler blocking but not crash propagation).
- The ETF encode/decode overhead is small relative to compile time on currently-sized modules.
- The Port boundary preserved by this ADR is the only thing keeping compiler bugs from being workspace-killers.

A *narrower* NIF variant — Port for the compiler, NIF only for cerl-term conversion — is a distinct alternative (see Alternative 7 below) with a meaningfully different risk profile.

### 4. Direct BEAM Bytecode Emission (Skip cerl Too)

Bypass `compile:forms/2` entirely and have Rust produce `.beam` binaries directly.

**Rejected because:**
- ADR 0003 rejected this thoroughly (BEAM file format is complex, opcodes change between OTP versions, we'd lose all `sys_core_fold` / kernel / asm optimizations). That analysis stands.
- The cerl path keeps all OTP's optimization passes; this path throws them away.

### 5. Text Wire + Annotation Side-Channel

Keep the text wire format. Add a parallel ETF map shipped alongside the Core Erlang text, mapping `{module, function, line}` triples in the generated text to source-level `{file, line, column}` spans. On the Erlang side, after `core_parse:parse/1` produces cerl forms, walk the AST and re-attach annotations from the side-channel map.

**Rejected because:**
- This recovers *only* the annotation-fidelity benefit, not the type-checked-codegen or string-escape-elimination benefits — and string-escape bugs (BT-875) are the more frequent failure surface.
- The post-parse annotation walk is brittle: it depends on `core_parse` preserving the surface-text line numbers stably, which is not a documented contract.
- It adds a second wire payload (text + map) and a third in-BEAM step (parse, then re-annotate, then compile), increasing rather than decreasing pipeline complexity.
- It leaves `core_scan`/`core_parse` in the hot path — the original motivating cost.
- However, this option does correctly identify that **annotation fidelity alone is not sufficient justification for the full migration**. The compounding case (type checking + string-escape elimination + annotation fidelity together) is what makes the cerl wire worthwhile.

### 6. Hybrid Wire — Text by Default, cerl as an Opt-In

Keep the text path as the default; add a `--cerl-wire` flag that opts into the cerl-ETF path.

**Rejected because:**
- A permanent dual path is the worst of both worlds — two codegen targets to maintain, two test surfaces, two failure modes.
- The ADR 0018 migration model already provides the right transition mechanism: the `Document` type carries the migration state internally; per-file migration moves leaves from text to cerl while the combinator surface is stable. No user-visible flag is needed.
- Acceptable as a *transition* state (the migration period necessarily has both paths coexisting), but not as a *permanent* configuration.

### 7. Hybrid Port + NIF-for-cerl-Conversion

Keep the OTP Port boundary for the compiler itself (preserving ADR 0022's crash-isolation guarantee for the long-running `compile:forms/2` call), but replace the *ETF encode/decode step alone* with a NIF that constructs BEAM-heap cerl terms directly from the Rust cerl representation. The NIF does pure data conversion (no compilation work, no scheduler blocking, no side effects); the compiler itself stays on its supervised Port.

**Rejected (for now) because:**
- The genuine win it captures — heap-resident term construction with literal sharing — is *measurable* (potentially significant for large modules) but is **second-order** relative to the type-safety and annotation-fidelity wins this ADR primarily targets. We can adopt cerl-direct via ETF first, measure the actual encode/decode cost on representative workloads, and revisit the NIF path *only if* that cost turns out to be a bottleneck.
- Introducing a NIF — even a conversion-only one — adds a `.so` per platform to the distribution surface, a new failure mode in the build pipeline, and a new operational concern (NIF-version vs OTP-version compatibility). That's real complexity for a benefit we haven't yet measured.
- The conversion NIF interacts with the Port in a non-obvious way: a Port message would still arrive on the Erlang side as a binary, get decoded via the NIF, then be passed to `compile:forms/2`. The Port's atomic-message guarantee no longer cleanly applies — the message bytes are merely a vehicle for the NIF call. This blurs the architectural boundary ADR 0022 established for the Port.
- **Honest acknowledgement**: the steelman for this alternative (raised in Option C's "sharpest argument") is the strongest case for revisiting NIFs in this ADR. We're deferring it, not refuting it. If Phase 0b's napkin shows ETF encode/decode dominating the per-compile cost for large modules, this alternative should be reopened as a follow-up ADR.

## Consequences

### Positive

- **Removes serialization round-trip.** `core_scan`/`core_parse` calls disappear from the per-compile hot path. Compile-time improvement is expected but not yet measured — Beamtalk's own analysis dominates per-compile latency, so the wire-format win is likely modest. Phase 0b's BEAM-diff harness will produce timing data for the actual delta before commitments are made.
- **Type-checked codegen.** Mis-shaped Core Erlang becomes a Rust type error at build time, not a `core_parse` error at runtime. Whole classes of codegen bugs (BT-875 string escapes) become impossible by construction.
- **Annotation fidelity.** Source positions on cerl nodes survive to BEAM bytecode unchanged. Future source-mapped diagnostics, dialyzer integration, and debugger support all benefit. Today's text path loses positions at the `core_scan` boundary unless we re-emit `-file` directives manually.
- **Preserves ADR 0018's discipline.** BT-875 invariant carries forward (no `format!()` for codegen fragments; structured-data-only). Per-fragment unit testability is preserved — assertions move from `assert_eq!(doc.to_pretty_string(), "...")` to structural matches on `cerl::Expr` values.
- **Eliminates string-escape failure surface.** Atoms, binaries, and string literals are values in cerl, not text fragments needing escaping.

### Negative

- **Large refactor of codegen.** ~58K LOC across ~55 source files in `crates/beamtalk-core/src/codegen/core_erlang/` need their leaves migrated from text fragments to cerl AST nodes. Mitigated by ADR 0018's organic-migration model: per-file migration during feature work, not a dedicated project.
- **Couples codegen to OTP `cerl` record shapes.** Cerl records are stable since OTP 18 but technically internal. Mitigated by per-file migration structure — if OTP ever broke us, the combinator surface lets us re-target a different shape without rewriting call sites.
- **Loses always-on text-wire debuggability.** Contributors can no longer `cat module.core` from a captured Port message. Mitigated by adding a `--emit-core` debug flag that pipes the cerl term through `core_pp:format/1` for human inspection. Same content, opt-in.
- **Migration hybrid state.** During Phase 3, some codegen functions return `Document` (text) and others return `cerl::Expr`. Mitigated by the `cerl_to_doc` adapter, which materialises a cerl expression as text for embedding in still-unmigrated parents. Final-state cleanup (Phase 4) removes the adapter and the text path together.
- **Error-location degradation during transition.** Today, when generated Core Erlang fails to parse, `core_parse` returns a line number into the generated text — imprecise but non-null. On the cerl wire, `core_parse` cannot fail (no parsing), but `sys_core_fold` and `compile:forms/2` errors reference cerl node annotations instead. During Phase 3 a module may contain a mix of annotated (migrated, source positions attached) and unannotated (text-rendered then cerl-converted via adapter, position = `0` or `compiler_generated`) nodes. Downstream warnings on the unannotated nodes will have *worse* location info than today's text-line numbers, not better. The migration order in Phase 3 deliberately prioritises high-error-frequency codegen first (expressions, control flow) so the worst regression window is short.
- **New `cerl::*` Rust types to maintain.** ~20–30 node types and their ETF encoders. Small (estimated 800–1500 LOC of mostly-mechanical data definitions) but non-zero.

### Neutral

- **No functional regression.** Verified via the full behavioral test suite (`just test-stdlib`, `just test-bunit`, `just test-repl-protocol`) run with both wire formats during the transition. Byte-for-byte `.beam` parity is **not** an invariant: `core_parse` applies normalisation (e.g., flattens nested sequences via `cerl:c_*` constructors) that the Rust ETF path cannot replicate without re-implementing it, and any source annotations the cerl path emits are themselves bytecode-visible (they're embedded in `debug_info`). The `just codegen-diff` harness is a *diagnostic* tool — it surfaces unexpected divergence during migration — but the contract enforced by CI is behaviour parity, not bytecode parity. The user-visible compiler *behaviour* is identical; the `.beam` bytes may differ in annotation chunks and chunk ordering.
- **Port architecture preserved (ADR 0022).** Same Port supervisor, same ETF framing, same crash-isolation guarantees.
- **Core Erlang as target IR preserved (ADR 0003).** No change to *what* IR we target; only *how* it crosses the wire.
- **No new dependencies.** `beamtalk-etf` already exists; `cerl` is an OTP standard module. No new crates or applications.
- **Stdlib build path unaffected.** `build_stdlib.rs` and the stdlib `.bt` compilation continue to work through the same compiler entry points.

## Implementation

### Phase 0a: Codegen Shrinkage Audit (Decision Gate — XS)

Before any wire-format work, measure how much of today's ~38.5K LOC of codegen (excluding tests; ~58K total) is **Document pretty-printer ceremony** vs. **irreducible language-lowering complexity**. The hypothesis behind ADR 0088 is that a meaningful fraction of current codegen exists to thread parens, commas, atom-escaping, and indentation through a text-rendering pipeline — work that disappears entirely under typed cerl construction.

- Pick 3 representative codegen functions covering different shapes:
  - A leaf utility (e.g., something in `selector_mangler.rs` or `erlang_types.rs`)
  - A medium-complexity expression builder (e.g., a case from `expressions.rs` or `intrinsics.rs`)
  - A high-complexity construct (e.g., a control-flow lowering from `control_flow/mod.rs` — the 4,237-LOC hot spot)
- Write the equivalent cerl-direct version of each (using a small handwritten cerl Rust mirror sufficient for these three functions — does not need to be the full Phase 1 mirror).
- Compare LOC, branching complexity, and number of helper calls. Project the result across the ~55 codegen files weighted by current size.
- Deliverable: a one-page memo with three before/after side-by-sides, a projected total-codegen LOC delta (rough — order of magnitude is enough), and a recommendation.

**Why this gates Phase 0b**: if the audit shows cerl-direct shrinks codegen materially (say, ≥15%), the migration's net cost story changes from "58K LOC of churn" to "the migration is itself a simplification that pays back in LOC". If the audit shows ≤5% shrinkage, the typed-Document-leaves alternative (see *Why not just constrain `Document` leaves?* above) becomes the right call for the BT-875 vector, and the cerl migration's remaining justification rests entirely on annotation-consumer roadmap (see *Downstream Consumers* below). Either outcome is decision-useful; the napkin's timing data is necessary but not sufficient on its own.

### Phase 0b: Napkin — Empty Module End-to-End (S)

The minimum proof that the wire contract works. Goal: compile a hand-constructed cerl `c_module` for `module 'bt_napkin' [] [] attributes [] end` from Rust → ETF → BEAM → loaded module that responds to `module_info/0`. No codegen migration yet.

- Add a minimal `crates/beamtalk-core/src/codegen/core_erlang/cerl.rs` with just enough nodes to express the empty module (`c_module`, `c_var`, `c_literal`, plus the annotation wrapper).
- Add an ETF encoder for those nodes; verify byte-equivalence against `term_to_binary(cerl:c_module(...))` on the Erlang side via a unit test.
- Add a Port message variant `{cerl, Etf}` in `beamtalk_compiler_server` and `beamtalk_build_worker` that `binary_to_term`s then calls `compile:forms(CoreModule, [from_core, binary, return_errors])`.
- Smoke test: Rust code sends an empty-module cerl term across the existing Port, BEAM compiles + loads it, `beamtalk_test_module:module_info(module) =:= bt_napkin`.
- Measure: time the round-trip vs. the equivalent text path on (i) the empty module and (ii) a hand-constructed large cerl tree (~thousands of nodes, representative of a real Beamtalk module). Record breakdown: ETF encode time, ETF decode time, `compile:forms` time, total.

This phase is the wire-check AND the timing-contingency check. If it works and ETF cost is a small share of total, Phase 1 proceeds as planned. If it works but ETF cost dominates for large modules, pivot to Alternative 7 (Port + NIF-for-conversion) before committing to the full ETF-direct migration. The commitment to ETF over NIF in this ADR is *contingent on the napkin's timing data*.

### Phase 1: Cerl Rust Mirror — Full Node Set + ETF Encoders (S)

Extend `cerl.rs` to cover every node used by current codegen:

- Function, fun-expression
- Literal (integer, float, binary, string), nil, cons, tuple, map
- Let, letrec, case (with clauses, guards, patterns)
- Apply (local call), call (remote), primop
- Try, catch, receive
- Binary (segments)
- Full annotation propagation

Unit tests verify each node's ETF output round-trips through `binary_to_term/1` + `cerl:c_*/n` constructors on the Erlang side and equals what `core_parse:parse` would produce from equivalent text.

### Phase 2: Cerl Adapter + Codegen Generator Dispatch (M)

The `Document` API is text-rendering-shaped and is **not** generalised over a leaf type — the layout combinators have no meaning on cerl. Instead, this phase adds the seams that let codegen functions migrate one-at-a-time:

- Add an adapter `pub fn cerl_to_doc(expr: &cerl::Expr) -> Document` that pretty-prints a cerl expression as Core Erlang text. Used by still-text-emitting parent functions to embed already-migrated children.
- Add the inverse hook on `CoreErlangGenerator`: it can now return either a `Document` (text wire) or a `cerl::Module` (cerl wire) and dispatch to the matching Port message variant.
- The text-leaf code path keeps its existing `Document.to_pretty_string()` test surface, so the ~245 codegen unit tests in `tests/` continue to pass during Phase 3. Migrated functions get new structural tests that assert on cerl AST shape directly.

**Type-system reality check**: this is a type-driven migration, not a `write!`-by-`write!` migration like ADR 0018's. When a codegen function changes its return type from `Document` to `cerl::Expr`, every call site that composes it must either (a) also migrate, or (b) wrap the result in `cerl_to_doc(...)`. The adapter exists specifically to enable (b) during the transition. Without the adapter, Phase 3 would be all-or-nothing.

### Phase 3: Per-Function Codegen Migration (Ongoing)

When touching a codegen function for a feature, migrate its return type from `Document` to the corresponding cerl node type. Behavioural suites verify no regression per PR.

**Migration is type-driven, not purely organic.** Unlike ADR 0018 (which incrementally replaced `write!` calls without changing function signatures), this phase changes return types — a Phase 3 migration of a single function touches every caller. Two valid strategies for each PR:

1. **Vertical slice**: migrate a leaf function + all its callers that don't compose with non-migrated peers (rare for highly-shared utilities, common for feature-specific codegen).
2. **Adapter-bridged**: migrate the leaf function, wrap its result in `cerl_to_doc(...)` at every caller; clean up the wrappers in a later PR when surrounding code migrates.

Strategy (1) is preferable when feasible — it leaves no `cerl_to_doc` callbacks. Strategy (2) is the fallback for utility functions used across the codebase.

Approximate order (following the ADR 0018 phase ordering):

| Subsystem | Files | Trigger | Strategy |
|-----------|-------|---------|----------|
| Leaf utilities (`erlang_types.rs`, `selector_mangler.rs`, `util.rs`) | 3 | First — foundational | Vertical slice; small caller fan-out |
| Expressions + intrinsics + operators | 3 | Stdlib feature work | Vertical slice |
| Control flow (`control_flow/`) | 7 | Block-semantics work | Vertical slice + adapter at exception boundaries |
| Primitives (`primitives/`) | 17 | Stdlib class-by-class | Adapter-bridged initially; vertical slice once neighbours migrate |
| Gen-server / actor / supervisor codegen (`gen_server/`, `actor_codegen.rs`, `supervisor_codegen.rs`) | ~10 | Actor runtime work | Adapter-bridged |
| Module assembly (`class_builder_source.rs`, `dispatch_codegen.rs`, `spec_codegen.rs`, `state_codegen.rs`, `value_type_codegen.rs`) | 5 | Metaclass / module-structure work | Last (consumes everyone else) |
| Mod / glue | remaining | Final cleanup PR | All-at-once |

### Phase 4: Text Path Removal (S)

When `≤50` text-leaf `Document` call sites remain (same threshold ADR 0018 used), schedule a dedicated cleanup PR:

- Remove the `Document` type, the `docvec!`/`nest`/`group` API, and the pretty-printer renderer.
- Remove the `cerl_to_doc` adapter (every caller is migrated by now).
- Remove the `core_scan`/`core_parse` call sites in **both** `beamtalk_compiler_server:compile_core_erlang/1` and `beamtalk_build_worker:compile_core_erlang/1` (the build-path copy uses the same logic and must migrate in lockstep).
- The public API of `beamtalk_compiler:compile_core_erlang/1` keeps its name and signature — only the body changes. No external API rename.
- Add `beamtalk_compiler:emit_core_dump(Module)` debug helper that pipes the cerl term through `core_pp:format/1` for opt-in human inspection.

### Affected Components

- New: `crates/beamtalk-core/src/codegen/core_erlang/cerl.rs` (Rust cerl mirror + ETF encoders)
- Modified: `crates/beamtalk-core/src/codegen/core_erlang/document.rs` — stays text-rendering-shaped (not generalised over a leaf type); gains the `cerl_to_doc` adapter for the transition period; deleted entirely in Phase 4
- Modified: every file under `crates/beamtalk-core/src/codegen/core_erlang/` (per-file leaf migration, over time — ~55 source files)
- Modified: `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl` — add cerl receive path to `compile_core_erlang/1`, eventually remove text path
- Modified: `runtime/apps/beamtalk_compiler/src/beamtalk_build_worker.erl` — parallel migration of its `compile_core_erlang/1` (stdlib build path; same logic, separate implementation). Note: the build-worker path passes `debug_info` to `compile:forms/2` while the server path does not, so its `debug_info` chunks will *include* the cerl annotations we add. Phase 0b napkin must verify both paths (with and without `debug_info`) work end-to-end so the build-worker doesn't surprise us later.
- Modified: `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl` — wire message variant
- Modified: `crates/beamtalk-core/src/codegen/core_erlang_validity_tests.rs` — extend proptest suite to the cerl path
- New: `just codegen-diff` task (BEAM-diff harness for the transition period)
- Reused: `crates/beamtalk-etf/` (no changes required — current encoder is sufficient for the cerl node set)
- Reused: `crates/beamtalk-core/src/codegen/core_erlang/tests/` (~245 inline codegen unit tests; assertions move from `to_pretty_string()` to structural cerl matches as each file migrates)

### Migration Health Checks

- **Tracking**: count remaining `Document`-returning codegen functions with `grep -rEc 'fn [a-z_]+.* -> Document\b' crates/beamtalk-core/src/codegen/core_erlang/`. Baseline measured at Phase 2 completion.
- **Checkpoint**: if after 6 months of feature work fewer than 25% of call sites have migrated, revisit whether organic migration is working or a dedicated sprint is warranted (mirrors ADR 0018's checkpoint policy).
- **Phase 4 trigger**: ≤50 text-leaf sites remaining.

### Verification

- **Codegen unit tests** in `crates/beamtalk-core/src/codegen/core_erlang/tests/` (~245 inline `#[test]` blocks asserting on `Document.to_pretty_string()`) continue to pass on the text-leaf code paths during the transition. New equivalents for migrated code paths assert on cerl-AST shape via direct field matching (no string rendering required).
- **BEAM-diff harness** added as part of Phase 0b: a small `just codegen-diff` task that compiles a fixed fixture set (`stdlib/test/*.bt`, REPL workspace snippets) via both wire formats and `cmp -l`s the resulting `.beam` files. Runs in CI during the transition period; deleted when Phase 4 removes the text path.
- **Property-based parseability tests** (`core_erlang_validity_tests.rs`) extended to the cerl path: round-trip cerl-Rust → ETF → `binary_to_term` → `cerl_lint`.
- **End-to-end suites** — `just test-stdlib`, `just test-bunit`, `just test-repl-protocol` — pass with both wire formats during transition.
- **ETF byte-equivalence**: new unit tests for the cerl Rust mirror compare against `term_to_binary(cerl:c_*/n(...))` invoked on the Erlang side, ensuring each Rust node encodes to the same ETF bytes as the corresponding OTP-constructed term.
- **CI matrix** runs the full test suite with `BEAMTALK_WIRE=text` and `BEAMTALK_WIRE=cerl` during the transition period.

## Migration Path

This is an internal refactor — no user-facing migration needed. Beamtalk source code (`.bt` files) is unaffected. Generated `.beam` artifacts are *behaviourally* identical (verified by the full behavioural test suite with both wire formats during transition); their bytes may differ in annotation chunks and chunk ordering, per the Consequences section. The `just codegen-diff` harness surfaces unexpected divergence as a diagnostic, not as a parity invariant.

For compiler contributors:
- New codegen functions: return `cerl::Expr` (or the appropriate cerl node type) from the start, once Phase 2 lands.
- Existing codegen functions: migrate when touched for feature work (Phase 3, ongoing). No dedicated migration sprint.
- Debug-time inspection of generated Core Erlang: use the `--emit-core` debug flag after Phase 4 (which pipes the cerl term through `core_pp:format/1`).

## References

- Related ADRs:
  - [ADR 0003](0003-core-erlang-vs-erlang-source.md) — Core Erlang as primary codegen target (unchanged; this ADR only changes the transport, not the target IR)
  - [ADR 0018](0018-document-tree-codegen.md) — Document tree codegen API (preserved; only leaf types change)
  - [ADR 0022](0022-embedded-compiler-via-otp-port.md) — Embedded compiler via OTP Port (extended; same Port channel carries the new wire format)
- Related cleanup: BT-875 (Document API discipline — preserved by this ADR)
- OTP `cerl` module: https://www.erlang.org/doc/man/cerl.html
- OTP `compile:forms/2` with `from_core`: https://www.erlang.org/doc/man/compile.html#forms-2
- OTP `core_pp` (for opt-in debug dumps): https://www.erlang.org/doc/man/core_pp.html
- Core Erlang specification: https://www.it.uu.se/research/group/hipe/cerl/
- LFE compiler driver (precedent for direct cerl emission): https://github.com/lfe/lfe/blob/develop/src/lfe_comp.erl

## Appendix A: Smoke-Test Audit Data

A two-file informal audit was performed during ADR review to sanity-check the Phase 0a hypothesis that cerl-direct shrinks codegen materially. **This is not a substitute for Phase 0a** — it's a lower-bound signal on whether running the real audit is worthwhile. Phase 0a must still rewrite three representative functions (including one from `control_flow/mod.rs`) and project across all ~55 files before any Phase 1 commitment.

### Files sampled

| File | Total LOC | Non-test LOC | Why chosen |
|---|---|---|---|
| `util.rs` | 449 | ~250 | Leaf utility — escape helpers, attribute fragment builders |
| `operators.rs` | 201 | ~150 | Small medium-complexity — binary ops, power, concat with runtime type dispatch |

### `util.rs` findings

Lines that **evaporate entirely** under cerl-direct (not just simplify):

| Construct | LOC affected | Why it disappears |
|---|---|---|
| `escape_core_erlang_string` + its 4 unit tests | ~25 | ETF encodes strings as length+bytes; no `\"` escaping needed |
| `escape_atom_chars` + its 3 unit tests | ~35 | ETF encodes atoms via `ATOM_EXT`; no `\'` escaping |
| Text plumbing in `beamtalk_class_attribute` | ~12 of 18 | Becomes a structured `c_module.attrs.push(c_tuple([c_atom(name), c_atom(super)]))` |
| Text plumbing in `file_attr` and `source_path_attr` | ~20 of 26 | Same — structured attribute constructors |
| `capture_expression` (renders to string for interpolation) | ~5 | Exists only because some callers want text; mostly deletable |

**Estimated shrinkage: ~95 LOC of 449 ≈ 21%** (or ~38% of non-test code).

### `operators.rs` findings

- `generate_binary_op`: ~4 LOC saved. Text-flat shape; modest win.
- `generate_power_op`: roughly a wash. Could grow slightly when spelling out nested `cerl::Call` constructors.
- `generate_concat_op` runtime-dispatch branch: ~22 LOC of manually-threaded `let X = Y in let Z = … in case … of <'true'> when 'true' -> … end` text, with `Document::String(var.clone())` appearing **four times** because the var name has to be re-interpolated at every textual reference. Under cerl-direct that's a `cerl::Let` / `cerl::Let` / `cerl::Case` tree where the var is one value referenced four times — no `String::clone`s, no comma threading. ~5–8 LOC saved, qualitative win larger than LOC delta.
- The CLAUDE.md-enforcement comment on lines 87–89 (explaining why `Document::Str` is "safe" here) evaporates — the type system would just enforce it. This comment genre exists throughout the codegen; small but real cleanup.

**Estimated shrinkage: ~12 LOC of 201 ≈ 6%.**

### Combined and caveats

**Combined: ~107 LOC of 650 ≈ 16%** — right at the ADR's 15% threshold.

Sample biases to be aware of when reading this number:

- **Biased upward**: `util.rs` is unusually heavy on escape/text-fragment helpers because that's its specific purpose. Most codegen files won't shrink this much.
- **Biased downward**: the sample omits `control_flow/mod.rs` (4,237 LOC), where multi-statement let-chains and nested case expressions are the dominant shape. That file is expected to show the biggest per-function wins from eliminating manual `let X = Y in let Z = …` plumbing — potentially a 5× amplification of the `generate_concat_op` pattern.
- **Second-order effects uncounted**: every call site of `escape_atom_chars`, `escape_core_erlang_string`, and the attribute helpers has surrounding plumbing that also simplifies. Dozens of those exist.
- **Test count drops alongside code**: ~7 unit tests in `util.rs` exist *only* to test escaping logic that won't exist post-migration.

### What the smoke test does and does not say

**Says**: A meaningful fraction (≈15–20%) of codegen LOC is text-rendering ceremony, not language-lowering logic. The hypothesis underlying Phase 0a is plausible and the real audit is worth running.

**Does not say**: That the migration is justified. The smoke-test sample is small and skewed toward a leaf-utility file. A real Phase 0a must include `control_flow/mod.rs` (or similar high-complexity file) before any Phase 1 decision — that's where the strongest evidence either way will come from.
