# ADR 0106: Opt-In Asserted `match:` Exhaustiveness (`matchExhaustive:`)

## Status
Implemented (2026-07-07)

## Context

### Problem statement

ADR 0102 §4 (BT-2745) gave `match:` an **advisory** exhaustiveness check:
when the type checker can prove a scrutinee is a *closed* union of `#symbol`
singletons, it warns about any uncovered members. That ADR explicitly flagged
a follow-up and left it out of scope:

> **Known discoverability cliff:** the warning silently vanishes when
> inference widens the scrutinee (one `Dynamic`-returning arm, a
> reassignment) — the user cannot distinguish "exhaustive" from "checker gave
> up". An opt-in assertion (a marker on `match:` requesting exhaustiveness,
> analogous to TypeScript's `satisfies never` idiom, which would then
> diagnose "cannot verify: scrutinee is Dynamic") is the natural follow-up;
> deliberately **out of scope** here and left to a future issue so this ADR
> stays advisory-only.

This is that follow-up (BT-2763). The gap is real: a compass-direction
`match:` that is exhaustive today can silently stop being checked the moment
a helper function upstream starts returning `Dynamic`, or the variable is
reassigned to a wider type — and nothing in the diagnostics output
distinguishes "provably exhaustive" from "the checker gave up". Advisory
warnings are the right *default* (ADR 0102 §4's reasoning — gradual typing,
open-world FFI — still holds), but a developer who *wants* a hard guarantee
at a specific `match:` site currently has no way to ask for one.

### Current state

- `match:` parses to `Expression::Match { value, arms, span }`
  (`crates/beamtalk-core/src/ast/expression.rs`), produced by
  `parse_match_expression` in
  `crates/beamtalk-core/src/source_analysis/parser/expressions.rs` — the
  parser special-cases the `match:` keyword token followed by `[` before
  falling into generic keyword-message parsing.
- `TypeChecker::check_singleton_match_exhaustiveness`
  (`crates/beamtalk-core/src/semantic_analysis/type_checker/inference.rs`)
  runs on every `Expression::Match` during inference. It is gated by
  `is_closed_singleton_union` (silent on `Dynamic`, open `Symbol`,
  `Negation`, or any union with a non-singleton member — ADR 0100's
  completeness ladder) and computes the residual via
  `InferredType::difference(scrutinee_type, covered_arms)`. A non-`Never`
  residual emits `Diagnostic::warning(...)` naming the uncovered members.
- This is entirely separate from BT-1299's pattern-based sealed-constructor
  exhaustiveness (`validators::match_validators::check_match_exhaustiveness`),
  which is unaffected by this ADR.
- ADR 0100 (Open-World Diagnostic Policy) establishes the governing rule for
  severity: escalating a diagnostic to a build-failing `Error` is **always
  opt-in, never the default** for an open-world language.

### Constraints

- Must not touch the advisory path's behaviour or severity — BT-2745 stays a
  `Warning`-only, silent-by-default check for plain `match:`.
- Must satisfy ADR 0100: an `Error` is only acceptable here because the user
  explicitly opts in by writing the new keyword — the checker itself may
  never *decide* to escalate.
- Must define behaviour for every scrutinee shape, not just the closed
  singleton-union case the checker already handles — "silently do nothing"
  is not an acceptable answer for an assertion the user explicitly wrote.

## Decision

Add `matchExhaustive:` as a second, parser-level keyword variant of `match:`
that asserts — at `Error` severity — that the checker can *prove* the match
is exhaustive.

```beamtalk
direction :: #north | #south | #east | #west := readHeading

// Compile error: matchExhaustive: proves this is NOT exhaustive
direction matchExhaustive: [
  #north -> 0;
  #south -> 180;
  #east  -> 90
]
// ⛔ Error: non-exhaustive matchExhaustive: `#west` is not handled
//    (residual type: `#west`)

// Fine: all four members covered
direction matchExhaustive: [
  #north -> 0;
  #south -> 180;
  #east  -> 90;
  #west  -> 270
]

// Fine: an unguarded wildcard is still full coverage
direction matchExhaustive: [
  #north -> 0;
  _      -> -1
]
```

### Surface syntax: a keyword variant of `match:`

`matchExhaustive:` parses exactly like `match:` — same arm syntax, same
guards, same destructuring patterns — the parser's existing special case for
`match:` followed by `[` is extended to also recognise `matchExhaustive:`.
The AST gains one field: `Expression::Match` now carries
`exhaustive: bool` (`true` for `matchExhaustive:`, `false` for `match:`),
threaded through unparse (round-trips the correct keyword) and through
`TypeChecker::infer_expr`'s `Expression::Match` arm, which now dispatches to
one of two checks based on the flag. Every other AST consumer (codegen,
lints, LSP queries, the BT-1299 pattern-based validator) pattern-matches
`Expression::Match { value, arms, .. }` and is completely unaffected — the
new field is inert everywhere except the one call site that reads it.

### Behaviour when the scrutinee is not a closed singleton union

This is the case ADR 0102 §4 left undefined, and the reason this needs its
own decision, not just a severity bump on the existing check. When
`matchExhaustive:` is used and the scrutinee is **not** a known-closed
`#symbol`-singleton union (`Dynamic`, a bare/open `Symbol`, a `Negation`
co-finite set, a union with any non-singleton member, or an ordinary nominal
type or union), the assertion **fails loudly**:

- **Severity: `Error`.** The user asked for a hard guarantee; "the checker
  doesn't know" cannot silently satisfy that request. This does not violate
  ADR 0100 — the user, not the checker, chose to escalate, by writing
  `matchExhaustive:` instead of `match:`.
- **Message names the actual inferred type**, so the fix is obvious:
  `cannot verify `matchExhaustive:` is exhaustive — scrutinee type
  `Dynamic (unknown)` is not a closed union of symbol singletons`, with a
  hint pointing at annotating the scrutinee or falling back to `match:`.
- This is a single, uniform rule regardless of *why* the type isn't closed —
  there is no special-cased message per shape (Dynamic vs. open Symbol vs.
  mixed union); the diagnostic always names the concrete inferred type, which
  already tells the user which case they hit.

When the scrutinee *is* a closed singleton union, `matchExhaustive:` reuses
BT-2745's residual computation (`difference(scrutinee, covered)`) verbatim,
at `Error` severity instead of `Warning`, naming the uncovered members. A
`Never` residual (fully covered, including via an unguarded wildcard or
variable-binding arm — the same coverage rule as the advisory check) means
the assertion holds, and — like a passing type check — is **completely
silent**: no diagnostic at all.

### Error examples

```beamtalk
// Dynamic scrutinee — cannot verify, fails loudly
x matchExhaustive: [#ok -> 1; _ -> 0]
// ⛔ Error: cannot verify `matchExhaustive:` is exhaustive — scrutinee type
//    `Dynamic` is not a closed union of symbol singletons
//    hint: matchExhaustive: only proves exhaustiveness over a closed union
//    of `#symbol` singletons (e.g. `x :: #north | #south`). Annotate the
//    scrutinee with such a type, or use `match:` if exhaustiveness cannot
//    be guaranteed statically.

// Closed union, one member missing
status :: #ok | #error := checkStatus
status matchExhaustive: [#ok -> "done"]
// ⛔ Error: non-exhaustive matchExhaustive: `#error` is not handled
//    (residual type: `#error`)
```

## Prior Art

| Language | Approach | What we take / leave |
|---|---|---|
| **TypeScript** | `const _exhaustive: never = value;` (the `satisfies never` idiom) — a *value-level* assertion computed from control-flow narrowing, reported as a type error at the assignment when the narrowed type isn't `never`. | **Adopt the spirit**: opt-in, `Error`-severity, names what's left over. **Diverge on mechanism**: TS repurposes an assignment; Beamtalk has no structural narrowing-to-`never` to piggyback on, so a first-class keyword on `match:` itself is more legible and puts the assertion exactly where the claim is being made, not in a throwaway binding after the fact. |
| **Rust** | `match` is exhaustive **by default** (compile error otherwise); `#[non_exhaustive]` opts a specific enum *out*. | Inverted from Rust's default-strict model — Beamtalk stays default-advisory (ADR 0102 §4's reasoning: open-world, gradual typing, FFI can't be trusted) and offers strictness as an explicit per-site opt-in instead of a language-wide default. |
| **Elixir/Gleam** | No opt-in assertion mechanism; Gleam's `case` is exhaustive by construction over closed nominal sum types (no gradual fallback needed). | Confirms there's no direct prior art for a *gradual* language's opt-in exhaustiveness marker — this is genuinely new territory for the BEAM ecosystem, following ADR 0102's Elixir-inspired atom-set model rather than Gleam's nominal one. |

## User Impact

- **Newcomer:** `matchExhaustive:` reads as a direct, discoverable escalation
  of `match:` — "same thing, but prove it." The error message on a `Dynamic`
  scrutinee teaches exactly why the guarantee can't be given yet (add a type
  annotation).
- **Existing BT-2745 users:** Zero change. Every existing `match:` keeps its
  advisory warning behaviour untouched; nothing needs migrating.
- **Author of a genuinely-closed-domain match** (compass directions, HTTP
  method enums, protocol states): gets a real, build-breaking guarantee
  instead of a warning that can silently stop firing.
- **Operator:** No runtime/codegen impact — `matchExhaustive:` compiles to
  the identical Core Erlang as `match:` (only the compile-time check
  differs); the AST flag never reaches codegen decisions.

## Steelman Analysis

### Chosen: `matchExhaustive:` keyword variant
- 🧑‍💻 **Newcomer:** "It's just `match:` with a longer, self-explanatory
  name — I don't need to learn a new syntax shape."
- 🎩 **Smalltalk purist:** "A keyword message is exactly how Smalltalk asks
  for a variant behaviour — no new punctuation, no pragma extension."
- 🏭 **Tooling author:** "One boolean field, threaded through one call site.
  Every other AST consumer is untouched by construction (`{ .., .. }`
  matches already ignore it)."

### Rejected A: marker arm (e.g. a trailing `exhaustive` pseudo-pattern)
- 🎨 **Language designer:** "Keeps `match:` itself unchanged; the assertion
  lives inside the arm list, next to what it's asserting about."
- **Why not chosen:** ambiguous with a legitimate variable-binding arm named
  `exhaustive`; ties the assertion's diagnostic span to one arm's position
  instead of the whole match; and the exhaustiveness check needs to run
  *before* iterating arms for coverage, not as a pseudo-arm within that same
  iteration — an awkward mechanical fit for no expressive gain over a keyword
  variant.

### Rejected B: pragma/annotation prefix (`@exhaustive x match: [...]`)
- 🧑‍💻 **Newcomer (from TS):** "Decorators/annotations are a familiar
  'opt into stricter checking' idiom."
- **Why not chosen:** `@primitive`-style pragmas are currently scoped to
  method-body primitive declarations (ADR 0007) and are not a general
  expression-modifier mechanism; extending pragma syntax to arbitrary
  expressions is separately-scoped surface-syntax work with its own parsing
  questions (what does a pragma prefixing an arbitrary receiver expression
  even bind to?) that this issue doesn't need to open.

### Rejected C: post-hoc assertion combinator (`(x match: [...]) assertExhaustive`)
- 🎨 **Language designer:** "Composable — works as a wrapper around any
  existing `match:` without new parser support."
- **Why not chosen:** by the time `match:` has been parsed and type-checked,
  the exhaustiveness fact is already fixed; a combinator applied to the
  *result* can't retroactively change the severity of a check that already
  ran (or didn't run) inside the inner `match:`, and it can't attach the
  diagnostic to the arms it's actually about. The assertion has to be known
  at the point `match:` itself is type-checked, which means it has to be
  part of that same expression — a keyword variant, not a wrapper.

### Tension points
Purists (§Rejected B's newcomer voice) want annotation-style opt-in;
Smalltalkers want a keyword message. The keyword-message answer wins because
it needs no new grammar production — it is a second string compared in the
same one `if` the parser already has for `match:`.

## Alternatives Considered

See Steelman Analysis above — marker arm, pragma/annotation prefix, and a
post-hoc combinator were all considered and rejected in favour of the keyword
variant, for the reasons given there.

## Consequences

### Positive
- Closes the ADR 0102 §4 discoverability cliff at the sites that opt in: an
  asserted `matchExhaustive:` cannot silently stop being checked without the
  build breaking.
- Reuses 100% of BT-2745's residual/coverage machinery — the only new logic
  is the non-closed-scrutinee loud-failure path and severity selection.
- Zero behavioural change to any existing `match:` — additive only.

### Negative
- Two keyword spellings for one control-flow construct (`match:` /
  `matchExhaustive:`) is one more thing to teach, though the naming makes the
  relationship self-evident.
- The non-closed-scrutinee `Error` can be surprising the first time a
  previously-exhaustive `matchExhaustive:` breaks the build after an
  upstream type widens — this is the intended behaviour (that's the whole
  point of asserting), but it does mean `matchExhaustive:` sites are more
  sensitive to inference changes elsewhere than plain `match:` sites.

### Neutral
- No codegen or runtime impact; `exhaustive` is a compile-time-only AST flag.

## Implementation

- **AST:** `Expression::Match` gains `exhaustive: bool`
  (`crates/beamtalk-core/src/ast/expression.rs`).
- **Parser:** `parse_match_expression` in
  `crates/beamtalk-core/src/source_analysis/parser/expressions.rs` accepts
  the keyword actually matched (`match:` vs. `matchExhaustive:`) and sets the
  flag accordingly; both keywords share one parsing path.
- **Unparse:** `unparse_match` in `crates/beamtalk-core/src/unparse/mod.rs`
  selects the keyword string from the flag, so formatting round-trips
  `matchExhaustive:` losslessly.
- **Type checker:** `crates/beamtalk-core/src/semantic_analysis/type_checker/inference.rs`
  — the `Expression::Match` arm of `infer_expr` dispatches to
  `check_singleton_match_exhaustiveness` (unchanged, `exhaustive == false`)
  or the new `check_asserted_match_exhaustiveness` (`exhaustive == true`).
  Both share a `singleton_match_residual` helper factored out of the
  original BT-2745 function so the coverage/residual logic has one
  implementation.
- **Tests:**
  `crates/beamtalk-core/src/source_analysis/parser/expressions.rs` (parsing
  and unparse round-trip),
  `crates/beamtalk-core/src/semantic_analysis/type_checker/tests/asserted_match_exhaustiveness.rs`
  (the `Error` path, the non-closed-scrutinee loud failure, and a regression
  pin that `singleton_match_exhaustiveness.rs`'s advisory `Warning` behaviour
  is untouched), plus BUnit runtime tests in `stdlib/test/`.
- **Docs:** `docs/beamtalk-language-features.md`, Pattern Matching section.
- **LSP:** `matchExhaustive:` added to keyword completions
  (`crates/beamtalk-core/src/queries/completion_provider.rs`) for surface
  parity with `match:`.

## References
- Related issues: BT-2763, BT-2745, BT-2738 (epic)
- Related ADRs: ADR 0102 §4 (advisory exhaustiveness — the check this
  assertion escalates), ADR 0100 (Open-World Diagnostic Policy — governs why
  this may be `Error`-severity only because it is opt-in), ADR 0007
  (pragma scope — why a pragma-based surface syntax was rejected)
- Documentation: `docs/beamtalk-language-features.md` §Pattern Matching
