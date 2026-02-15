# ADR 0011: Robustness Testing — Layered Fuzzing and Error Quality

## Status
Implemented (2026-02-15)

## Context

Beamtalk's parser is designed for error recovery — it **must** produce a partial AST with diagnostics even when input is malformed (critical for IDE support). However, the current test suite has almost no coverage of this capability:

- **Only 9 error test cases** exist in `tests/e2e/cases/errors.bt`
- **Zero fuzz testing** — no `cargo-fuzz`, `proptest`, or equivalent
- **No property-based testing** — parser invariants (always returns AST, spans always valid, diagnostics non-empty for errors) are untested
- **No near-miss syntax testing** — typos, wrong brackets, missing colons, misplaced keywords
- **No error message regression testing** — message quality can silently degrade

The parser has good infrastructure for recovery (synchronization at `.`, `]`, `)`, `}`, `;`, `^`) and the `(Module, Vec<Diagnostic>)` return type enforces that errors don't prevent AST construction. But **none of this is stress-tested**.

### Risk

A single panic on malformed input would crash:
- The REPL daemon (affecting all connected sessions)
- The LSP server (breaking IDE integration)
- The build command (blocking CI pipelines)

Since Beamtalk is interactive-first, users type incomplete and malformed syntax constantly during live coding. Parser robustness is not optional — it's a core UX requirement.

### Current State

| Area | Coverage | Gap |
|------|----------|-----|
| Parser crash safety | None | Any random input could panic |
| Parser recovery quality | 2 unit tests | Recovery produces AST but quality untested |
| Diagnostic span validity | None | Spans could point outside input |
| Error message quality | 9 E2E cases | Near-miss syntax, cascading errors untested |
| REPL error round-trip | 9 E2E cases | Error formatting, hint generation untested |
| Unicode handling | None | Emoji, multi-byte, invalid UTF-8 untested |

## Decision

Adopt a **three-layer robustness testing strategy**, where each layer catches a different class of bugs. Layers are independent and can be implemented incrementally.

### Layer 1: `cargo-fuzz` — Crash Safety

Coverage-guided fuzzing feeds random bytes to the parser. The parser must **never panic** on any input.

**Fuzz target:**
```rust
// fuzz/fuzz_targets/parse_arbitrary.rs
#![no_main]
use libfuzzer_sys::fuzz_target;
use beamtalk_core::source_analysis::{lex_with_eof, parse};

fuzz_target!(|data: &[u8]| {
    // Only test valid UTF-8 (parser expects strings)
    if let Ok(source) = std::str::from_utf8(data) {
        let tokens = lex_with_eof(source);
        let (_module, _diagnostics) = parse(tokens);
        // Success = no panic. That's the only assertion.
    }
});
```

**Corpus seeding:** All `.bt` files from `examples/` and `tests/e2e/cases/` provide realistic starting points for mutation.

**CI integration:** Run nightly (not per-PR — too slow). Store corpus in `fuzz/corpus/parse_arbitrary/`.

**What it catches:**
- Panics on unexpected token sequences
- Stack overflow on deeply nested input
- Infinite loops in error recovery
- Index-out-of-bounds on malformed token streams

### Layer 2: `proptest` — Grammar-Aware Properties

Generate syntactically-structured (but sometimes invalid) Beamtalk and verify parser invariants.

**Key properties:**

```rust
use proptest::prelude::*;
use beamtalk_core::source_analysis::{lex_with_eof, parse};

// Property 1: Parser never panics on any string
proptest! {
    #[test]
    fn parser_never_panics(input in "\\PC*") {
        let tokens = lex_with_eof(&input);
        let (_module, _diagnostics) = parse(tokens);
    }
}

// Property 2: Diagnostics always have valid spans
proptest! {
    #[test]
    fn diagnostic_spans_within_input(input in gen_near_valid_beamtalk()) {
        let tokens = lex_with_eof(&input);
        let (_module, diagnostics) = parse(tokens);
        for d in &diagnostics {
            prop_assert!(d.span.end() as usize <= input.len(),
                "Span {:?} exceeds input length {}", d.span, input.len());
        }
    }
}

// Property 3: Errors always produce non-empty diagnostics
proptest! {
    #[test]
    fn errors_produce_diagnostics(input in gen_invalid_beamtalk()) {
        let tokens = lex_with_eof(&input);
        let (_module, diagnostics) = parse(tokens);
        prop_assert!(
            !diagnostics.is_empty(),
            "Invalid input did not produce any diagnostics"
        );
    }
}

// Property 4: Error messages are non-empty and don't contain internal names
proptest! {
    #[test]
    fn error_messages_are_user_facing(input in gen_near_valid_beamtalk()) {
        let tokens = lex_with_eof(&input);
        let (_module, diagnostics) = parse(tokens);
        for d in &diagnostics {
            prop_assert!(!d.message.is_empty(), "Empty error message");
            prop_assert!(!d.message.contains("TokenKind"),
                "Internal type leaked in error: {}", d.message);
            prop_assert!(!d.message.contains("unwrap"),
                "Debug text leaked in error: {}", d.message);
        }
    }
}
```

**Grammar-aware generators** produce structured-but-mutated Beamtalk:

```rust
fn gen_near_valid_beamtalk() -> impl Strategy<Value = String> {
    prop_oneof![
        // Valid expressions with random mutations
        gen_valid_expr().prop_map(|e| mutate_random_char(e)),
        // Mismatched brackets
        gen_block().prop_map(|b| b.replace(']', ')')),
        // Missing keyword colons
        gen_keyword_msg().prop_map(|m| m.replace(':', "")),
        // Truncated input (character-based to avoid invalid UTF-8 slices)
        gen_valid_expr().prop_map(|e| {
            let mid = e.chars().count() / 2;
            e.chars().take(mid).collect()
        }),
        // Duplicated operators
        gen_binary_expr().prop_map(|e| e.replace("+", "+ +")),
    ]
}
```

**CI integration:** Run with `cargo test` (fast — seconds, not hours). Part of `just test-rust`.

**What it catches:**
- Span calculation errors
- Silent token dropping during recovery
- Internal names leaking into error messages
- Diagnostics missing for error AST nodes
- Recovery producing structurally invalid AST

### Layer 3: Curated Error E2E Suite — UX Regression

Hand-written test cases covering common user mistakes, with pinned expected error messages.

**Expand `tests/e2e/cases/errors.bt`** (or split into focused files):

```beamtalk
// === Near-miss syntax ===

// Missing colon in keyword message
Counter subclass Foo
// => ERROR: expected expression

// Wrong bracket type
[:x | x + 1)
// => ERROR: Expected ']' to close block

// Duplicate parameter names
[:x :x | x + 1]
// => ERROR:

// Extra closing bracket
Counter spawn]
// => ERROR:

// === Invalid literals ===

// Unclosed string
"hello
// => ERROR:

// Invalid number
3.14.15
// => ERROR:

// === Cascading errors ===

// Multiple errors in one expression
[:x | ] + ]
// => ERROR:

// === Unicode edge cases ===

// Emoji in identifier
counter🚀 := 0
// => ERROR:

// === REPL-specific ===

// Empty input (whitespace only)

// => _

// Very deeply nested
((((((((((1))))))))))
// => 1
```

**Snapshot testing for error messages:** Use `insta` to pin exact error message text. Any change requires explicit `cargo insta accept`.

**CI integration:** Run with `just test-e2e` (existing infrastructure).

**What it catches:**
- Error message regressions (text changes unexpectedly)
- Missing or unhelpful error messages for common mistakes
- REPL crashes on edge-case input
- Error formatting issues (missing spans, garbled output)

## Prior Art

### Rust Compiler (`rustc`)
- Uses `cargo-fuzz` extensively for parser and type-checker fuzzing
- Maintains a "trophy case" of bugs found by fuzzing
- Property-based testing validates AST invariants
- Comprehensive error message UI tests with `.stderr` snapshots

### Gleam Compiler
- Rust-based BEAM compiler with similar architecture
- Uses `cargo-fuzz` for parser crash safety
- Extensive error snapshot testing (each error has a `.stderr` file)
- Property-based testing for formatter round-trips

### TypeScript Compiler
- Massive suite of negative test cases (thousands of `.ts` files with expected errors)
- Error baseline files pinned in CI
- Fuzzing via community projects (TypeFuzz, comfort)

### Common Pattern
All mature language implementations use **both** automated fuzzing (crash safety) **and** curated error tests (UX quality). Neither alone is sufficient.

## User Impact

### 🧑‍💻 Newcomer
- **Positive:** Better error messages when learning syntax — near-miss errors get helpful diagnostics
- **Positive:** REPL never crashes, even with wild experimentation
- **Neutral:** No visible change to language features

### 🎩 Smalltalk Developer
- **Positive:** Interactive-first promise upheld — live coding tolerates syntax errors gracefully
- **Positive:** Error recovery quality tested, so partial ASTs are more useful in IDE

### ⚙️ BEAM Developer
- **Positive:** Daemon stability — parser can't crash the shared compilation service
- **Positive:** Fuzz testing catches bugs before they hit production REPL sessions

### 🏭 Operator
- **Positive:** Nightly fuzzing catches regressions early in the pipeline
- **Neutral:** Slightly longer CI time (proptest adds seconds, cargo-fuzz is nightly-only)

## Steelman Analysis

### Best argument for "proptest-only" (rejected)
> "Grammar-aware generation is the sweet spot — it finds deeper bugs than random bytes, and you get shrinking for free. cargo-fuzz mostly finds trivial panics that a few unit tests could catch. Skip the fuzzing infrastructure overhead."

**Rebuttal:** cargo-fuzz's coverage-guided mutation finds crash paths that structured generators miss — it explores token sequences no grammar generator would produce. The two are complementary, not competitive.

### Best argument for "cargo-fuzz + E2E only" (rejected)
> "proptest grammar generators are expensive to build and maintain — they essentially re-implement the grammar. For the same effort, you could write 200 curated test cases that cover more real-world scenarios."

**Rebuttal:** Grammar generators test **invariants** (spans valid, diagnostics present, no internal leaks) that hold across all inputs. Hand-written tests check specific cases but miss the combinatorial explosion. And generators don't need to cover the full grammar — even simple mutators find real bugs.

### Best argument for "do nothing yet" (rejected)
> "With only 9 error cases but no reported parser crashes, is this solving a real problem? Wait until users report crash bugs, then add targeted tests."

**Rebuttal:** The REPL daemon is shared infrastructure — a single crash affects all sessions. The cost of a crash in production (lost REPL state, killed sessions) far exceeds the cost of prevention. And fuzz testing is cheapest to add early, before the parser grows more complex.

### Tension points
- **Speed vs. thoroughness:** Nightly fuzzing is slow but finds rare bugs. proptest is fast but limited to generator quality.
- **Maintenance cost:** Grammar generators need updating as syntax evolves. Snapshot tests need updating when error messages improve.
- **Signal-to-noise:** cargo-fuzz on random bytes produces many "boring" crashes (bad UTF-8, empty input). Needs triage investment.

## Alternatives Considered

### Alternative: Grammar-Based Fuzzing (e.g., Grammarinator)
Generate inputs from a formal grammar definition, ensuring syntactic structure.

**Rejected because:**
- Requires maintaining a separate grammar definition (duplication of parser logic)
- Misses the "wrong syntax" cases we specifically want to test
- Over-generates valid inputs when we need invalid ones
- cargo-fuzz + proptest together cover this space more flexibly

### Alternative: Mutation-Based Testing (e.g., AFL)
Use AFL's mutation strategies instead of libFuzzer.

**Rejected because:**
- cargo-fuzz (libFuzzer) has better Rust integration and is the ecosystem standard
- AFL requires separate compilation pipeline
- Both find similar bug classes; pick the one with lower setup cost

### Alternative: Error Message Approval Tests Only
Just pin error messages with snapshot tests, no fuzzing.

**Rejected because:**
- Snapshot tests only cover cases you think to write
- Parser crashes on unexpected input would go undetected
- No property validation (spans, diagnostics, recovery quality)

## Consequences

### Positive
- Parser crash bugs found before users encounter them
- Error message quality maintained through snapshot regression tests
- Parser invariants (valid spans, non-empty diagnostics) continuously verified
- REPL daemon stability improved — shared service can't be crashed by bad input
- Incremental adoption — each layer adds value independently
- Grammar generators serve as executable documentation of valid/invalid syntax

### Negative
- New dev dependency: `proptest` (~30s added to `cargo test`)
- New tooling: `cargo-fuzz` (nightly-only, separate CI job)
- Grammar generators need maintenance as syntax evolves
- Snapshot tests need updating when error messages intentionally change
- Nightly fuzz CI job adds infrastructure cost

### Neutral
- No impact on language semantics or runtime behavior
- No impact on release artifacts (test-only changes)
- Fuzz corpus grows over time — needs occasional pruning
- May discover pre-existing parser bugs that need prioritization

## Implementation

### Phase 1: cargo-fuzz Setup (S)
- Add `fuzz/` directory with parse target
- Seed corpus from existing `.bt` files
- Add nightly CI job
- **Files:** `fuzz/Cargo.toml`, `fuzz/fuzz_targets/parse_arbitrary.rs`, `.github/workflows/`

### Phase 2: proptest Parser Properties (M)
- Add `proptest` dev-dependency to `beamtalk-core`
- Implement core properties (no-panic, valid spans, diagnostics present, user-facing messages)
- Add basic near-valid generators (truncation, bracket mutation, colon removal)
- **Files:** `Cargo.toml`, `crates/beamtalk-core/src/source_analysis/parser/property_tests.rs`

### Phase 3: Curated Error E2E Suite (M)
- Expand `tests/e2e/cases/errors.bt` with near-miss, unicode, nesting, cascading categories
- Add snapshot pinning for error message text
- Add REPL-specific edge cases (empty input, very long input)
- **Files:** `tests/e2e/cases/errors.bt` (or split into `errors_syntax.bt`, `errors_runtime.bt`, etc.)

### Phase 4: REPL Round-Trip Properties (S)
- proptest properties for REPL eval: compile errors return structured diagnostics, never crash daemon
- Test error formatting end-to-end (daemon → JSON → CLI display)
- **Files:** `crates/beamtalk-cli/tests/`, `runtime/apps/beamtalk_runtime/test/`

### Affected Components

| Component | Change | Phase |
|-----------|--------|-------|
| `crates/beamtalk-core` | proptest properties, fuzz targets | 1, 2 |
| `tests/e2e/cases/` | Expanded error test suite | 3 |
| `fuzz/` | New directory for cargo-fuzz | 1 |
| `.github/workflows/` | Nightly fuzz CI job | 1 |
| `crates/beamtalk-cli` | REPL round-trip property tests | 4 |

## Open Questions

1. **Lexer isolation fuzzing** — The current fuzz target chains `lex()` → `parse()`. Should we also fuzz the lexer in isolation? It handles raw bytes first and could have its own crash paths independent of parser recovery.

2. **Nightly fuzz budget** — How long should the CI fuzz job run per night? 10 minutes (cheap, catches easy crashes), 1 hour (good coverage), or 8 hours (thorough but expensive)? Longer runs find rarer bugs but cost more compute.

3. **Error E2E file organization** — Keep all error cases in one `errors.bt`, or split by category (`errors_syntax.bt`, `errors_runtime.bt`, `errors_unicode.bt`)? Single file is simpler; multiple files allow parallel test development.

4. **Snapshot pinning granularity** — Should error message snapshots pin the exact full text (brittle — any rewording breaks CI, but catches all regressions) or just key substrings (flexible — allows message improvements, but misses subtle UX degradation)?

5. **Crash triage policy** — When cargo-fuzz finds a parser crash, what's the severity? Release-blocker? Or file-and-fix-later? This determines whether nightly fuzz failures page someone or just create Linear issues.

6. **proptest generator depth** — How much grammar do we model? Full recursive expression tree generators (expensive to build and maintain as syntax evolves) or simple string mutators like truncation/bracket-swap (ships fast, less coverage)?

7. **Erlang runtime fuzzing** — This ADR covers the Rust-side parser and REPL protocol. Should we also fuzz the Erlang eval path (`beamtalk_repl_eval`)? That requires different tooling (PropEr or EQC) and is a separate implementation effort — possibly a follow-up ADR.

## References
- [Rust Fuzz Book](https://rust-fuzz.github.io/book/) — cargo-fuzz setup and best practices
- [proptest Book](https://altsysrq.github.io/proptest-book/) — property-based testing for Rust
- [Rust Compiler Fuzzing Guide](https://rustc-dev-guide.rust-lang.org/fuzzing.html) — how rustc uses fuzzing
- Related: `docs/development/testing-strategy.md` — current test pyramid
- Related: `docs/development/architecture-principles.md` — error recovery philosophy
- Related: ADR 0009 — OTP application structure (daemon stability)
- Related issues: BT-362 (Epic), BT-365 (cargo-fuzz), BT-363 (proptest), BT-366 (E2E suite), BT-364 (REPL round-trip)
