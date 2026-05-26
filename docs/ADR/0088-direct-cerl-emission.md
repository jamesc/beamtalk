# ADR 0088: Direct Core Erlang AST Emission via ETF

## Status
Proposed (2026-05-26)

## Context

ADR 0022 established the OTP Port architecture for the embedded compiler: the Rust `beamtalk-core` runs as a long-running Port subprocess, returns Core Erlang as a binary in an ETF response, and the Erlang side compiles it to BEAM bytecode fully in-memory:

```erlang
%% Today, in beamtalk_compiler_server:
compile_core_to_beam(CoreErlangBin, ModuleName) ->
    {ok, Tokens, _} = core_scan:string(binary_to_list(CoreErlangBin)),
    {ok, Forms} = core_parse:parse(Tokens),
    case compile:forms(Forms, [from_core, binary, return_errors]) of
        {ok, ModuleName, BeamBinary} ->
            code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", BeamBinary);
        {error, Errors, _Warnings} ->
            {error, Errors}
    end.
```

ADR 0018 established the Wadler-Lindig `Document` tree as the only sanctioned way to construct Core Erlang output (BT-875 cleanup enforces this — no `format!()` or string concatenation in codegen). The Document tree is rendered to a Core Erlang text binary that is sent across the Port to the Erlang side.

### The serialization tax

The current pipeline encodes the same structural information three times:

1. **Rust codegen** builds a `Document` tree (~58K LOC of typed combinator calls in `crates/beamtalk-core/src/codegen/core_erlang/`).
2. **Document renderer** flattens the tree to a Core Erlang text binary — pretty-printer machinery (`Group`, `Break`, `Nest`) exists to format text readably, but no human reads the output before it crosses the Port.
3. **Erlang compiler server** calls `core_scan:string/1` and `core_parse:parse/1` to rebuild a structured representation (`cerl` records) from the text, then passes those records to `compile:forms/2` for the rest of the lowering pipeline (kernel → asm → BEAM).

Each compile pays for steps 2 and 3 — building text from structure, then rebuilding structure from text — for no semantic gain. `compile:forms/2` accepts `cerl` records directly (the `from_core` option). If the Rust side produced `cerl`-shaped data, encoded it as ETF, and the Erlang side called `binary_to_term/1` followed by `compile:forms/2`, steps 2 and 3 disappear.

### What's already in place

- **ETF transport**: `crates/beamtalk-etf/` (459 LOC) already encodes Erlang terms from Rust. The Port channel established by ADR 0022 already speaks ETF for request/response framing.
- **In-memory compile path**: ADR 0022 already established that the Erlang side calls `compile:forms/2` in-process — there is no `erlc` subprocess to eliminate. The only wasted step is `core_scan` + `core_parse` reconstructing forms from text the Rust side just rendered.
- **Document API discipline**: BT-875 forced all codegen onto the `Document` combinator API. The combinators (`docvec!`, `join`, `nest`, `line`) are the right abstraction — they just currently produce text leaves. The migration target is for the same combinators to produce `cerl` AST leaves.

### Constraints

- **Core Erlang remains the target IR** (ADR 0003). This decision is about *how* Core Erlang reaches `compile:forms/2`, not *whether* Core Erlang is the right IR.
- **Document API contract preserved** (ADR 0018, BT-875). The combinator surface stays; only the leaf types change. No regression to ad-hoc string construction.
- **Port boundary preserved** (ADR 0022). No NIF embedding. The Rust ↔ BEAM seam stays at the OS process boundary.
- **Snapshot-test parity**: 196 codegen snapshot tests and 170 unit tests must continue to verify byte-for-byte identical `.beam` output before and after each migration step.

## Decision

**Replace text-rendered Core Erlang on the Port wire with ETF-encoded `cerl` AST terms.**

Concretely:

1. Add a Rust mirror of the `cerl` record types (`c_module`, `c_fun`, `c_var`, `c_literal`, `c_cons`, `c_tuple`, `c_map`, `c_let`, `c_letrec`, `c_case`, `c_clause`, `c_apply`, `c_call`, `c_primop`, `c_try`, `c_catch`, `c_receive`, `c_binary`, plus annotation wrappers — ~20–30 node types).
2. Each Rust `cerl::*` node has an ETF encoder that produces the byte-for-byte equivalent of what `term_to_binary/1` would produce on the corresponding Erlang record.
3. Convert the `Document` type from a *text* combinator (leaves = `Str`/`String`/`Line`/`Break`) to a *cerl-AST* combinator (leaves = `cerl::*` nodes). The combinator names (`docvec!`, `join`, `nest`) and call-site ergonomics stay; the leaves change.
4. The Port message format gains a `cerl` response variant alongside the existing `core_erlang` text variant. The Erlang side decodes ETF, calls `compile:forms(Forms, [from_core, binary, return_errors])`, and `code:load_binary/3` as today.
5. Migrate codegen files from text leaves to cerl leaves opportunistically (per ADR 0018's organic-migration model). Snapshot tests verify `.beam` parity at every step.
6. After all codegen has migrated, remove the text path (`Document::Str`, `Document::Line`, the pretty-printer renderer, and the `core_scan`/`core_parse` calls in `beamtalk_compiler_server`).

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
%% beamtalk_compiler_server
{ok, Tokens, _} = core_scan:string(binary_to_list(CoreErlangBin)),
{ok, Forms}    = core_parse:parse(Tokens),
{ok, Mod, Bin} = compile:forms(Forms, [from_core, binary]),
code:load_binary(Mod, FName, Bin).
```

**After** — codegen produces typed nodes, BEAM consumes them directly:

```rust
// crates/beamtalk-core/src/codegen/core_erlang/expressions.rs
fn gen_call(module: Atom, fun: Atom, args: Vec<CerlExpr>) -> CerlExpr {
    CerlExpr::Call {
        module: Box::new(CerlExpr::Literal(Literal::Atom(module))),
        name:   Box::new(CerlExpr::Literal(Literal::Atom(fun))),
        args,
    }
}
```

```erlang
%% beamtalk_compiler_server
Forms          = binary_to_term(CerlEtf),
{ok, Mod, Bin} = compile:forms(Forms, [from_core, binary]),
code:load_binary(Mod, FName, Bin).
```

The Rust-side change is the larger one (touches every codegen call site over time); the Erlang-side change is mechanical (delete two lines, add one).

### What stays the same

- The `Document` macro surface (`docvec!`, `join`, `nest`, `group`). Call sites read almost identically. BT-875 discipline preserved.
- The Port wire is still ETF; the existing length-prefixed framing and request/response shape don't change.
- `compile:forms/2` and `code:load_binary/3` are exactly the same calls.
- All optimization passes (`sys_core_fold`, kernel, asm) run as before — we're feeding them the same `cerl` they'd get from `core_parse:parse`.
- The generated `.beam` is byte-for-byte identical (verified by snapshot tests).

## Prior Art

| Compiler | Approach | Notes |
|----------|----------|-------|
| **Elixir** | Builds Erlang AST (`erl_parse:abstract_form()`) directly via quoted forms; never goes through text. | Closest precedent for "skip the text serialization entirely." Elixir compiles quoted forms to BEAM in-process via the same `compile`-module entry points we'd use. |
| **LFE** | Compiles Lisp s-expressions to `cerl` records directly, calls `compile:forms/2`. | Demonstrates that targeting `cerl` (rather than Erlang abstract forms) is viable for non-Erlang-syntax languages. |
| **Erlang itself** | The `compile` module's own pipeline preserves annotations through `cerl` → kernel → asm → BEAM. Source positions, file/line info, and custom annotations on `cerl` nodes survive to the final bytecode. | Gives us a path to better diagnostics: nodes carrying `{file, Line, Col}` annotations enable downstream warnings (e.g., from `sys_core_fold`) to map back to `.bt` source. |
| **Gleam** | Renders Erlang **source text** to `.erl` files, then shells out to `erlc`. | Chose readability/portability over wire optimization. Gleam targets the Erlang source level for end-user readability; we target Core Erlang for codegen simplicity (ADR 0003), so the same trade-off doesn't apply. |
| **rustc** | Builds typed HIR/MIR in-memory throughout; never renders to text between phases. | Confirms that "structured all the way down" is the modern compiler default. Text emission is a debug/inspection feature, not a transport. |

**Pattern:** Compilers that share a host runtime with their target VM (Elixir on BEAM, LFE on BEAM, rustc to its own backends) keep structured data in-memory. Compilers that render text (Gleam, our current pipeline) do so when there's a strong reason — user readability or process isolation. Once ADR 0022 collapsed the daemon/erlc subprocess into an in-VM `compile:forms/2` call, the readability rationale stopped applying to us — no human reads the wire bytes.

## User Impact

| Persona | Impact |
|---------|--------|
| **Newcomer** | Invisible. Same language, same REPL output, same `.beam` artifacts. Marginally faster compile (skips `core_scan`/`core_parse`). |
| **Smalltalk developer** | Invisible. No semantic changes. |
| **Erlang/BEAM developer** | Mildly positive. The compiler now uses the same idiomatic interface (`cerl` + `compile:forms`) that LFE and other BEAM languages do. `.core` debug dumps can still be produced on demand by calling `core_pp:format/1` on the cerl term — same output, just opt-in. |
| **Production operator** | Marginally positive. Lower per-compile CPU (no scan+parse pass), lower per-compile memory (no intermediate text binary). No new failure modes — the Port boundary and supervision are unchanged. |
| **Compiler contributor** | Significant positive. Codegen produces typed nodes instead of text fragments: type errors catch mis-shaped cerl at compile time instead of as `core_parse` errors at runtime. No string-escape bugs (cerl atoms and strings are values, not text needing escaping). Source-position annotations attach to cerl nodes uniformly. Per-fragment unit testing remains as ADR 0018 enabled it. |
| **Tooling developer** | Positive. Annotations on cerl nodes flow through to `sys_core_fold` warnings, `dialyzer`, and any future source-mapped debugger. Today's text path loses annotations at the `core_scan`/`core_parse` boundary unless we re-emit `-file`/`-line` directives manually. |

### Discoverability

No user-facing change. Contributors writing new codegen use the same `docvec!`/`join`/`nest` API as today. The `cerl::*` leaf types are new vocabulary but map 1:1 to OTP's existing `cerl` module (well-documented, stable across recent OTP versions).

## Steelman Analysis

### Option A: Direct cerl Emission via ETF (Recommended)

- 🧑‍💻 **Newcomer contributor**: "When I'm adding a new codegen function, the Rust type system tells me if I've assembled a malformed Core Erlang construct. I don't have to wait for `core_parse` to reject my string and decode an Erlang parse error to find the bug."
- 🎩 **Smalltalk purist**: "Compilation should be a function over structured data, not a text-templating operation. Smalltalk compilers manipulate ASTs internally — this aligns us with that tradition."
- ⚙️ **BEAM veteran**: "`compile:forms/2` with `from_core` is *the* way to drive the Core Erlang compiler from another language on BEAM. It's what LFE does. We're already doing the in-memory `compile:forms` call (ADR 0022); we just have a needless `core_scan`/`core_parse` round-trip in front of it."
- 🏭 **Operator**: "Fewer codegen failure modes reach production. Mis-escaped atoms or malformed expressions become Rust type errors at build time instead of `core_parse` errors at runtime. Compile latency drops a few percent — small but real."
- 🎨 **Language designer**: "Annotations on cerl nodes survive to BEAM bytecode. This is the foundation for proper source-mapped diagnostics, dialyzer integration, and debugger support — none of which work cleanly when our generated text loses position information at the text/AST boundary."

### Option B: Keep Text Emission (Status Quo)

- 🧑‍💻 **Newcomer contributor**: "I can `cat module.core` and read what was generated. With cerl-as-terms I'd need a pretty-printer I don't have memorized. The text path is debuggable in a way structured terms aren't."
- 🎩 **Smalltalk purist**: "Don't fix what isn't broken. ADR 0022's snapshot tests prove the text path produces correct BEAM. Ship features."
- ⚙️ **BEAM veteran**: "The `core_scan`/`core_parse` calls are stable OTP — they're not going away, they're not slow enough to matter. The compile-time cost is dominated by Beamtalk's own analysis, not the Erlang side."
- 🏭 **Operator**: "Any refactor of 58K LOC of codegen is a regression risk. The current pipeline is six months stable. Stability beats theoretical optimization."
- 🎨 **Language designer**: "Text is a versioned, debuggable, interoperable wire format. cerl record layouts change between OTP versions (rare, but it has happened). Coupling our codegen to a private OTP record shape is a long-term maintenance bet."

### Option C: NIF-based cerl Construction (Embed ERTS into Rust)

- 🧑‍💻 **Newcomer contributor**: "No serialization at all — the Rust compiler constructs BEAM terms directly in the VM's heap. Lowest possible latency."
- 🎩 **Smalltalk purist**: "Compiler and runtime sharing the same memory space is more 'Smalltalk' than any Port boundary."
- ⚙️ **BEAM veteran**: "Eliminates the ETF encode/decode entirely. If we're serious about low latency, this is the path."
- 🏭 **Operator**: "Strongly negative. A NIF crash in `compile:forms/2` kills the entire BEAM node, taking all running actors with it. ADR 0022 deferred NIF specifically for this reason; the same argument applies."
- 🎨 **Language designer**: "Couples codegen to ERTS internals. Distribution complexity (NIF .so per platform). Already rejected in ADR 0022."

### Tension Points

- **Debuggability of the wire vs. wire efficiency**: Text dumps are easier for humans; cerl terms are easier for machines. Mitigation: `core_pp:format/1` lets us reconstruct text on demand for debugging, so we keep human-readability as an opt-in tool, not the always-on wire format.
- **Coupling to OTP `cerl` record shapes**: Cerl records are documented in OTP but technically internal. Mitigation: the shapes have been stable since OTP 18 (2014); the migration's per-file structure means we could re-target a different Erlang AST shape if OTP ever broke us, with the same combinator surface.
- **Refactor cost vs. compile-speed win**: 58K LOC of codegen migration is a large refactor for a single-digit-percent compile speedup. Mitigation: the compile-speed win is not the primary motivation — annotation fidelity, type-checked codegen, and removing the string-escaping risk surface (BT-875 was a real bug) are the larger long-term wins. The refactor is organic, not a dedicated project, following the ADR 0018 model.
- **Tooling for inspecting generated cerl**: Today a contributor can read `.core` files. With cerl-terms-only, contributors need a `--emit-core` flag to dump `core_pp:format/1` output. Acceptable cost; small new tool.

## Alternatives Considered

### 1. Keep Text Emission (Status Quo)

Continue rendering `Document` trees to Core Erlang text, ship text across the Port, decode with `core_scan` + `core_parse` on the BEAM side.

**Rejected because:**
- The `core_scan`/`core_parse` round-trip is pure overhead — we render structure to text then immediately parse it back. No reader exists between the two steps.
- Source-position annotations are difficult to preserve through text emission. Today we'd have to manually insert `-file`/`-line` directives across 28 files; cerl annotations attach uniformly via the AST.
- The string-escape bug class (BT-875) is fundamentally a text-level problem. With cerl terms, atoms and binaries are values; no escape boundary exists.
- The "text is debuggable" advantage is preserved by `core_pp:format/1` — we keep readable output as a debug tool, not as the always-on transport.

### 2. Direct Erlang Abstract Forms (`erl_parse:abstract_form()`) Instead of cerl

Construct Erlang surface-syntax AST in Rust, encode via ETF, call `compile:forms/2` without `from_core`.

**Rejected because:**
- Erlang abstract forms are a *larger* surface area than cerl — they encode syntactic sugar (list comprehensions, records, macros) that the Erlang compiler then desugars to cerl.
- We'd be re-encoding decisions the compiler has already made: our codegen already produces post-desugar shapes (explicit case expressions for pattern matching, explicit gen_server dispatch trees). Targeting cerl matches the level we're already at.
- ADR 0003 already chose Core Erlang over Erlang source for codegen simplicity. The same argument applies to the AST: cerl is simpler and matches our intent.

### 3. NIF-based Cerl Construction (Embed ERTS into Rust)

Use Rustler or hand-rolled NIFs to construct `cerl` terms in the BEAM VM's heap directly, eliminating ETF encoding/decoding.

**Rejected because:**
- ADR 0022 deferred NIF specifically because a NIF crash kills the BEAM node (taking running actors with it). The same risk profile applies: `compile:forms/2` is a 10–500 ms operation exercising complex code paths, exactly the wrong workload for an in-process NIF.
- The ETF encode/decode overhead is small relative to compile time (~1–2 ms on modules taking 10–500 ms to compile).
- The Port boundary preserved by this ADR is the only thing keeping compiler bugs from being workspace-killers.

### 4. Direct BEAM Bytecode Emission (Skip cerl Too)

Bypass `compile:forms/2` entirely and have Rust produce `.beam` binaries directly.

**Rejected because:**
- ADR 0003 rejected this thoroughly (BEAM file format is complex, opcodes change between OTP versions, we'd lose all `sys_core_fold` / kernel / asm optimizations). That analysis stands.
- The cerl path keeps all OTP's optimization passes; this path throws them away.

### 5. Hybrid Wire — Text by Default, cerl as an Opt-In

Keep the text path as the default; add a `--cerl-wire` flag that opts into the cerl-ETF path.

**Rejected because:**
- A permanent dual path is the worst of both worlds — two codegen targets to maintain, two snapshot test suites, two failure modes.
- The ADR 0018 migration model already provides the right transition mechanism: the `Document` type carries the migration state internally; per-file migration moves leaves from text to cerl while the combinator surface is stable. No user-visible flag is needed.
- Acceptable as a *transition* state (the migration period necessarily has both paths coexisting), but not as a *permanent* configuration.

## Consequences

### Positive

- **Removes serialization round-trip.** `core_scan`/`core_parse` calls disappear from the per-compile hot path. Single-digit-percent compile-time improvement (the win is small because Beamtalk's own analysis dominates, but it's free and never regresses).
- **Type-checked codegen.** Mis-shaped Core Erlang becomes a Rust type error at build time, not a `core_parse` error at runtime. Whole classes of codegen bugs (BT-875 string escapes) become impossible by construction.
- **Annotation fidelity.** Source positions on cerl nodes survive to BEAM bytecode unchanged. Future source-mapped diagnostics, dialyzer integration, and debugger support all benefit. Today's text path loses positions at the `core_scan` boundary unless we re-emit `-file` directives manually.
- **Preserves the `Document` API.** ADR 0018's combinator surface (`docvec!`, `join`, `nest`) is unchanged; BT-875 discipline carries forward. Codegen call sites read almost identically after migration.
- **Eliminates string-escape failure surface.** Atoms, binaries, and string literals are values in cerl, not text fragments needing escaping.

### Negative

- **Large refactor of codegen.** ~58K LOC across 28 files in `crates/beamtalk-core/src/codegen/core_erlang/` need their leaves migrated from text fragments to cerl AST nodes. Mitigated by ADR 0018's organic-migration model: per-file migration during feature work, not a dedicated project.
- **Couples codegen to OTP `cerl` record shapes.** Cerl records are stable since OTP 18 but technically internal. Mitigated by per-file migration structure — if OTP ever broke us, the combinator surface lets us re-target a different shape without rewriting call sites.
- **Loses always-on text-wire debuggability.** Contributors can no longer `cat module.core` from a captured Port message. Mitigated by adding a `--emit-core` debug flag that pipes the cerl term through `core_pp:format/1` for human inspection. Same content, opt-in.
- **Migration hybrid state.** During the per-file migration, some codegen functions return text-leaf `Document`s and others return cerl-leaf `Document`s. Mitigated by a small adapter layer (parallels ADR 0018's `write_document` bridge). Final-state cleanup removes it.
- **New `cerl::*` Rust types to maintain.** ~20–30 node types and their ETF encoders. Small (estimated 800–1500 LOC of mostly-mechanical data definitions) but non-zero.

### Neutral

- **Generated BEAM bytecode unchanged.** The 196 codegen snapshot tests verify byte-for-byte parity at every migration step. The user-visible compiler output is identical.
- **Port architecture preserved (ADR 0022).** Same Port supervisor, same ETF framing, same crash-isolation guarantees.
- **Core Erlang as target IR preserved (ADR 0003).** No change to *what* IR we target; only *how* it crosses the wire.
- **No new dependencies.** `beamtalk-etf` already exists; `cerl` is an OTP standard module. No new crates or applications.
- **Stdlib build path unaffected.** `build_stdlib.rs` and the stdlib `.bt` compilation continue to work through the same compiler entry points.

## Implementation

### Phase 0: Cerl Rust Mirror + ETF Encoders (S)

Add `crates/beamtalk-core/src/codegen/core_erlang/cerl.rs` with Rust types mirroring OTP's `cerl` records. Add ETF encoders that emit byte-for-byte equivalents of `term_to_binary/1` on the corresponding records. Cover the node set used by current codegen:

- Module, function, fun-expression
- Var, literal (atom, integer, float, binary, string), nil, cons, tuple, map
- Let, letrec, case (with clauses, guards, patterns)
- Apply (local call), call (remote), primop
- Try, catch, receive
- Binary (segments)
- Annotation wrappers (`{ann, [Anno], Inner}`)

Unit tests verify each node's ETF output round-trips through `binary_to_term/1` + `cerl:c_*/n` constructors on the Erlang side and equals what `core_parse:parse` would produce from equivalent text.

### Phase 1: Erlang-Side Cerl Receive Path (S)

In `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl`, add a second Port message variant `{cerl, Etf}` alongside the existing `{core_erlang, Text}`. The cerl variant calls `binary_to_term/1` then `compile:forms(Forms, [from_core, binary, return_errors])` directly. Text variant unchanged.

### Phase 2: Document Type Generalization (M)

Refactor `crates/beamtalk-core/src/codegen/core_erlang/document.rs` so `Document` is generic over leaf type. Two concrete instantiations during migration:
- `Document<TextLeaf>` — current behavior, renders to text.
- `Document<CerlLeaf>` — new behavior, folds to a `cerl::*` AST node.

Combinator API (`docvec!`, `join`, `nest`, `group`, `line`) works on either. Bridge function on `CoreErlangGenerator` accepts either and dispatches to the appropriate Port message variant.

### Phase 3: Opportunistic Codegen Migration (Ongoing)

Per ADR 0018's organic model: when touching a codegen file for a feature, migrate its functions from returning `Document<TextLeaf>` to `Document<CerlLeaf>`. Snapshot tests verify `.beam` parity per file.

Approximate order (following the ADR 0018 phase ordering):

| Subsystem | Files | Trigger |
|-----------|-------|---------|
| Leaf utilities (`erlang_types`, `selector_mangler`) | 2 | First — foundational |
| Expressions + intrinsics | 2 | Stdlib feature work |
| Control flow | 5 | Block-semantics work |
| Gen-server / actor codegen | 5 | Actor runtime work |
| Module assembly | 5 | Metaclass / module-structure work |
| Tail | remaining | Final cleanup PR |

### Phase 4: Text Path Removal (S)

When `≤50` text-leaf `Document` call sites remain (same threshold ADR 0018 used), schedule a dedicated cleanup PR:

- Remove `Document<TextLeaf>` instantiation.
- Remove the pretty-printer renderer (`document.rs` text-rendering paths).
- Remove the `core_scan`/`core_parse` call site in `beamtalk_compiler_server`.
- Add `beamtalk_compiler:emit_core_dump(Module)` debug helper that pipes the cerl term through `core_pp:format/1` for opt-in human inspection.

### Affected Components

- New: `crates/beamtalk-core/src/codegen/core_erlang/cerl.rs` (Rust cerl mirror + ETF encoders)
- Modified: `crates/beamtalk-core/src/codegen/core_erlang/document.rs` (generic over leaf type)
- Modified: every file under `crates/beamtalk-core/src/codegen/core_erlang/` (per-file leaf migration, over time)
- Modified: `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl` (add cerl receive path, eventually remove text path)
- Modified: `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl` (wire message variant)
- Reused: `crates/beamtalk-etf/` (no changes required — current encoder is sufficient for the cerl node set)

### Migration Health Checks

- **Tracking**: count remaining text-leaf usage with `grep -c 'Document<TextLeaf>\|TextLeaf::' crates/beamtalk-core/src/codegen/core_erlang/**/*.rs`. Baseline measured at Phase 2 completion.
- **Checkpoint**: if after 6 months of feature work fewer than 25% of call sites have migrated, revisit whether organic migration is working or a dedicated sprint is warranted (mirrors ADR 0018's checkpoint policy).
- **Phase 4 trigger**: ≤50 text-leaf sites remaining.

### Verification

- All 196 codegen snapshot tests pass with byte-for-byte identical `.beam` output at every migration step.
- `just test-stdlib`, `just test-bunit`, `just test-repl-protocol` pass with both wire formats during transition.
- New unit tests for the cerl Rust mirror verify ETF byte-equivalence against `term_to_binary(cerl:c_*/n(...))`.
- CI runs the full test suite with `BEAMTALK_WIRE=text` and `BEAMTALK_WIRE=cerl` during the transition period.

## Migration Path

This is an internal refactor — no user-facing migration needed. Beamtalk source code (`.bt` files) is unaffected. Generated `.beam` artifacts are byte-for-byte identical.

For compiler contributors:
- New codegen functions: write against `Document<CerlLeaf>` from the start (Phase 1 onward).
- Existing codegen functions: migrate when touched for feature work. No dedicated migration sprint.
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
