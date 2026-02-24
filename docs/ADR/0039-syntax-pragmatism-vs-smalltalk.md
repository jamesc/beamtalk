# ADR 0039: Syntax Pragmatism vs Smalltalk

## Status
Implemented (2026-02-24)

## Context

Beamtalk is Smalltalk-**inspired**, not Smalltalk-**compatible**. The language preserves Smalltalk's core innovation — message-passing syntax with named parameters — while making pragmatic departures from Smalltalk-80 conventions that create unnecessary friction for modern developers.

These departures are individually small but collectively define Beamtalk's character. They were documented informally in `docs/beamtalk-syntax-rationale.md` during early design, but have never been captured as a formal architectural decision. This ADR consolidates them into a single record.

### Design Goal

> **Keep the soul of Smalltalk, remove the friction.**

Beamtalk's target audience includes developers from Erlang/BEAM, mainstream languages (Python, TypeScript, Rust), and Smalltalk. The syntax choices must balance familiarity across these cohorts while preserving message-passing elegance.

### What We Keep

These Smalltalk features are preserved without change:

- **Keyword messages**: `array at: 1 put: "hello"` — self-documenting, readable, unique
- **Blocks (closures)**: `[:x :y | x + y]` — concise, elegant
- **Cascades**: `builder add: item1; add: item2; build` — powerful for fluent APIs
- **Assignment with `:=`**: Clearly distinguishes assignment from equality
- **Unary/Binary/Keyword categories**: Clean mental model for message complexity

## Decision

Beamtalk departs from Smalltalk-80 in the following areas. Each departure follows a consistent principle: **where Smalltalk convention conflicts with universal developer expectations, choose pragmatism.**

### 1. Comments: `"..."` → `//` and `/* */`

**Smalltalk-80:**
```smalltalk
"This is a comment"
count := 0.  "inline comment"
```

**Beamtalk:**
```beamtalk
// This is a comment
count := 0  // inline comment

/*
  Multi-line comment
  for longer explanations
*/
```

**Rationale:**
- `"comment"` is unique to Smalltalk; every other mainstream language uses `//` or `#`
- Strings also use quotes, creating visual confusion when reading code
- `//` is universal and matches muscle memory for virtually all developers
- Block comments (`/* */`) provide multi-line support that Smalltalk lacks

### 2. Math Precedence: Left-to-Right → Standard (PEMDAS)

**Smalltalk-80:**
```smalltalk
2 + 3 * 4.   "=> 20 (evaluated left-to-right)"
```

**Beamtalk:**
```beamtalk
2 + 3 * 4    // => 14 (standard math precedence)
(2 + 3) * 4  // => 20 (explicit grouping)
```

**Rationale:**
- Left-to-right evaluation is a persistent source of bugs — developers universally expect `*` to bind tighter than `+`
- Requiring parentheses for basic arithmetic is hostile to new users
- Standard math precedence matches every C-family language, Python, Rust, and Erlang

**Precedence levels (highest to lowest):**
1. `**` (exponentiation, right-associative)
2. `*`, `/`, `%` (multiplicative)
3. `+`, `-`, `++` (additive and string concatenation)
4. `<`, `>`, `<=`, `>=` (comparison)
5. `=:=`, `==`, `/=`, `=/=` (equality — strict and loose)

**Note:** `&&`, `||`, `and`, `or` are **not** binary operators — they are keyword messages taking blocks for short-circuit evaluation: `condition and: [expensiveCheck()]`.

### 3. Statement Terminator: Required `.` → Optional (Newlines)

**Smalltalk-80:**
```smalltalk
count := 0.
count := count + 1.
self doSomething.
```

**Beamtalk:**
```beamtalk
count := 0
count := count + 1
self doSomething

// Semicolons optional for multiple statements on one line
count := 0; count := count + 1
```

**Rationale:**
- Period-as-terminator feels archaic to developers accustomed to Python, Ruby, Go, or Kotlin
- Newlines naturally end statements in most modern languages
- Reduces visual noise without sacrificing readability
- Semicolons remain available for the (rare) case of multiple statements on one line

### 4. Field Access: Add `self.field` Notation

Smalltalk has no field access syntax — all access is via messages. Beamtalk adds direct field access:

```beamtalk
// Direct field access within actor
self.value          // read field
self.value := 10    // write field
self.value := self.value + 1

// Equivalent message send (still works)
self getValue       // unary message
```

**Rationale:**
- `self.value` is universally understood as "access a field" across languages
- Less noisy than `self getValue` for simple state reads
- Makes the distinction between field access and message sends visible in the code

**Compilation:** `self.value` compiles to `maps:get('value', State)` — a direct lookup, not a message send.

### 5. Return Semantics: Implicit Returns, `^` for Early Returns Only

**Smalltalk-80:** `^` required for every return.

**Beamtalk:**
```beamtalk
// Implicit return of last expression (preferred)
getValue => self.value

// Explicit return ONLY for early returns
max: other =>
  self > other ifTrue: [^self]   // early return — use ^
  other                           // last expression — implicit return

// Blocks also use implicit return
[:x | x * 2]  // returns x * 2
```

**Rationale:**
- `^` is distinctive and clear, but requiring it for every expression is noisy
- Implicit last-expression return matches Ruby, Rust, Elixir, and Kotlin
- Reserving `^` for early returns makes it a visual signal: "something unusual is happening here"

**Style rule:** Use `^` ONLY for early returns (returning before the final expression). Never use `^` on the last line of a method or block.

### 6. Control-Flow Block Mutations with State Threading

**BEAM constraint:** Smalltalk-80 blocks capture variables by reference (shared mutable cells), so `whileTrue:` with mutations works naturally in Pharo and Squeak. But BEAM enforces single-assignment variables — there are no heap-allocated mutable cells. Without compiler support, a naïve translation of `whileTrue:` to Erlang would fail to propagate mutations:

```erlang
%% Naïve Erlang translation — Count never changes
Count = 0,
while_true(fun() -> Count < 10 end, fun() -> Count = Count + 1 end).
%% Count is still 0 — Erlang variables are immutable
```

**Beamtalk solution:** The compiler detects literal blocks in control-flow positions and generates tail-recursive loops with explicit state threading, restoring the Smalltalk mutation semantics that developers expect:

```beamtalk
count := 0
[count < 10] whileTrue: [count := count + 1]
// count is now 10

// Field mutations work too
[self.value < 10] whileTrue: [
    self.value := self.value + 1
]
```

**Rationale:**
1. Smalltalk idioms *require* mutations in control flow — `whileTrue:`, `timesRepeat:`, `do:` are central to Smalltalk style
2. BEAM's single-assignment model requires compiler support to achieve this — compilation to tail-recursive loops with state threading bridges the semantic gap
3. Better than alternatives: C-style loops lose message-passing elegance; immutable-only makes simple counters painful; mutable-everywhere loses reasoning guarantees

**The rule:**

> Literal blocks in control-flow positions can mutate. Stored/passed closures cannot.

```beamtalk
// Literal block in control flow — OK
[count < 10] whileTrue: [count := count + 1]

// Stored closure cannot mutate — WARNING or ERROR
myBlock := [count := count + 1]
10 timesRepeat: myBlock          // count won't change
```

**Detected control-flow constructs:** `whileTrue:`, `whileFalse:`, `timesRepeat:`, `to:do:`, `do:`, `collect:`, `select:`, `reject:`, `inject:into:`.

**Known limitations:**
- The set of recognised control-flow constructs is a **hardcoded whitelist** in the compiler. User-defined higher-order methods do not get state threading — mutations in blocks passed to custom methods will not propagate. This limits compositionality for library authors who want to write Smalltalk-style control-flow abstractions.
- Non-local returns (`^` inside blocks) compile to throw/catch on BEAM, which has measurable allocation cost compared to Erlang's tail-recursive early termination. This affects collection methods like `detect:`, `anySatisfy:`, and `includes:` that use `^` for early exit.
- Some abstract collection methods (e.g., `inject:into:`) require primitive implementations because a pure-Beamtalk implementation using nested control flow does not compose state threading correctly.

### Cross-Referenced Departures

The following departures have dedicated ADRs and are summarised here for completeness:

| Departure | ADR | Summary |
|-----------|-----|---------|
| String interpolation | [ADR 0023](0023-string-interpolation-and-binaries.md) | `"Hello, {name}!"` — double quotes only, `{expr}` syntax, compiles to binary append |
| Equality semantics | [ADR 0002](0002-use-erlang-comparison-operators.md) | Use Erlang's `==`, `/=`, `=:=`, `=/=` directly — structural equality, not identity-based |
| Class definition syntax | [ADR 0038](0038-subclass-classbuilder-protocol.md) | `Object subclass: Counter` is parsed as **syntax** (not a message send), compiled to `ClassBuilder` protocol |
| No compound assignment | [ADR 0001](0001-no-compound-assignment.md) | No `+=`, `-=` — use explicit `x := x + 1` to preserve message-passing purity |

## Prior Art

### Smalltalk-80 / Pharo
The baseline. All departures are measured against Smalltalk-80 conventions. Pharo has made its own pragmatic changes over the years (e.g., array literals, fluid class definition API) while maintaining core syntax. Beamtalk goes further.

### Newspeak (Gilad Bracha)
Also Smalltalk-inspired, also makes pragmatic departures. Newspeak keeps Smalltalk's `"..."` comments and left-to-right precedence. Beamtalk is more aggressive about modernising surface syntax while Newspeak focuses more on modularity and capability-based security.

### Ruby
Took Smalltalk's "everything is an object" philosophy and made it palatable to mainstream developers via familiar syntax (`def`, `end`, `#` comments). Ruby's success validates the approach of keeping the paradigm while modernising the syntax. Beamtalk goes further by keeping keyword messages and blocks.

### Erlang / Elixir
Beamtalk targets the same runtime. Elixir's success demonstrates that a modernised syntax on BEAM attracts developers. Beamtalk's equality operators, string handling, and compilation model follow Erlang conventions directly.

### Kotlin / Swift
Both modernised their predecessors (Java, Objective-C) by removing ceremony. Implicit returns, standard precedence, `//` comments — Beamtalk makes the same moves relative to Smalltalk.

## User Impact

### Newcomers (from Python/TypeScript/Rust)
**Positive.** Comments, math precedence, implicit returns, and string interpolation all work as expected. The learning curve focuses on keyword messages and blocks (Smalltalk's actual innovations) rather than on comment syntax and operator precedence (historical accidents).

### Smalltalk Developers (from Pharo/Squeak)
**Mixed.** Keyword messages, blocks, cascades, and `:=` assignment all feel familiar. However: `//` comments instead of `"..."`, standard math precedence instead of left-to-right, and implicit returns will require adjustment. The control-flow mutation semantics fix a genuine pain point in Smalltalk-80.

### Erlang/BEAM Developers
**Positive.** Equality operators map 1:1 to Erlang. `self.field` compiles to `maps:get/2` — transparent. String interpolation compiles to binary operations. No hidden runtime magic.

### Language Designers / Operators
**Neutral.** The departures are well-documented and internally consistent. Each follows the principle of "keep the paradigm, modernise the surface."

## Steelman Analysis

### For Smalltalk-80 Compatibility (Smalltalk cohort)
"Every departure from Smalltalk-80 makes Beamtalk harder to learn for existing Smalltalkers and reduces the value of Smalltalk educational materials. Left-to-right precedence is *simpler* (one rule, no precedence table to memorise). `"..."` comments are fine once you know them. The departures optimise for first impressions at the cost of paradigm coherence. Worse, the 'literal vs stored' block distinction has no Smalltalk precedent — in Pharo, *all* blocks can mutate outer variables. Beamtalk claims to preserve Smalltalk idioms but introduces a restriction that breaks the fundamental assumption that blocks are composable."

**Response:** Valid concerns on both counts. For surface syntax: Beamtalk's target audience is broader than the existing Smalltalk community. The departures remove barriers that prevent developers from *discovering* Smalltalk's actual innovations (message passing, live objects). A developer who bounces off `"comment"` syntax never reaches keyword messages. For block compositionality: this is a genuine limitation imposed by BEAM's single-assignment model. The hardcoded whitelist is an honest engineering tradeoff — it covers the common Smalltalk control-flow patterns at the cost of extensibility. Library authors writing custom higher-order methods will encounter this boundary. We accept this as a known limitation rather than hiding it.

### For Maximum Modernisation (Mainstream cohort)
"If you're already departing from Smalltalk, go further. Use `def` instead of `=>`, use `fn` for blocks, use `class Counter < Object` instead of `Object subclass: Counter`. Half-measures satisfy nobody."

**Response:** The departures are carefully scoped to remove *friction* without removing *identity*. Keyword messages, blocks, and cascades are what make Beamtalk worth using. Replacing them with mainstream syntax yields "another Ruby" — functional but undifferentiated. The retained Smalltalk features are the value proposition.

### For Pure BEAM Semantics (Erlang cohort)
"Control-flow mutations add hidden complexity. In Erlang, all variables are single-assignment — the semantics are transparent. State threading behind `whileTrue:` is magic that hides what the BEAM is actually doing. And it gets worse: non-local returns (`^` inside `do:` blocks) compile to throw/catch, which has measurable allocation cost and produces opaque crash dumps. The 'BEAM transparency' claimed elsewhere in this ADR is selectively true."

**Response:** The state threading compiles to explicit tail-recursive loops — no hidden mutable state at the BEAM level. The "magic" is syntactic sugar with a clear compilation story. The throw/catch concern for non-local returns is valid and acknowledged as a known performance cost (see Section 6 limitations). The alternative (requiring developers to write explicit recursive functions for simple counters) undermines Smalltalk's control-flow elegance, which is a core design goal. The debuggability concern for crash dumps is real — BEAM-level variable names like `StateAcc` do not map obviously to Beamtalk source — and is an area for future tooling improvement.

## Alternatives Considered

### Keep Full Smalltalk-80 Syntax
Description: Use `"..."` comments, left-to-right precedence, required `.` terminators, no field access, no string interpolation.

**Rejected because:** Creates unnecessary friction for 95% of potential users. Smalltalk-80 syntax is a barrier to adoption, not a feature. Developers who want full Smalltalk-80 can use Pharo.

### Adopt Ruby/Python Syntax Entirely
Description: Use `def`/`end`, `#` comments, `class Counter(Object):`, standard function call syntax.

**Rejected because:** Loses keyword messages, blocks, and cascades — the features that differentiate Beamtalk. Results in "another Ruby on BEAM" which has less value than a language that brings Smalltalk's unique features to modern developers.

### Make Departures Optional (Compatibility Mode)
Description: Support both `"..."` and `//` comments, both left-to-right and PEMDAS precedence (via a pragma), etc.

**Rejected because:** Two dialects doubles the testing surface, complicates tooling, and fragments the community. One syntax, consistently applied, is better than optional compatibility.

## Consequences

### Positive
- **Lower barrier to entry**: Developers from any background can read Beamtalk code without stumbling over unfamiliar comment syntax or unexpected precedence
- **Focus on innovations**: The learning curve centres on keyword messages and blocks (Smalltalk's actual contributions) rather than historical accidents
- **Modern tooling**: `//` comments, standard precedence, and implicit returns enable straightforward integration with editors, linters, and formatters
- **BEAM transparency**: Equality operators and compilation model map directly to Erlang — no hidden translation layer

### Negative
- **Smalltalk community friction**: Experienced Smalltalkers may view the departures as abandoning the paradigm, reducing willingness to adopt or contribute
- **Not a Smalltalk**: Educational materials, books, and courses for Smalltalk-80 do not apply directly — learners must mentally translate
- **Precedent for further departures**: Each departure weakens the argument for keeping the next Smalltalk convention, creating a "slippery slope" pressure toward full modernisation
- **Control-flow mutation is not composable**: The hardcoded whitelist of control-flow constructs means user-defined higher-order methods do not get state threading. This limits library authors and breaks the Smalltalk expectation that blocks are fully composable
- **Debuggability**: State-threaded loops generate BEAM variables (`StateAcc`, `Packed_0`) that do not map obviously to Beamtalk source, making crash dumps harder to read for BEAM developers

### Neutral
- **Identity**: Beamtalk occupies a unique niche — neither Smalltalk nor mainstream — which is a strength for differentiation but requires careful positioning in documentation and marketing
- **Reversibility**: Most departures are surface syntax (comments, terminators, precedence) and could theoretically be reverted without changing semantics, though this is unlikely

## Implementation

All departures documented here are already implemented in the compiler.

**Affected components:**
- **Lexer** (`crates/beamtalk-core/src/source_analysis/lexer.rs`): Comment tokens, operator precedence, statement termination
- **Parser** (`crates/beamtalk-core/src/source_analysis/parser/`): Precedence climbing, implicit returns, field access parsing
- **Codegen** (`crates/beamtalk-core/src/codegen/`): State threading for control-flow mutations, field access compilation
- **Runtime** (`runtime/apps/beamtalk_runtime/src/`): Control-flow constructs (`whileTrue:`, etc.)

No further implementation work is required. This ADR formalises decisions that were made incrementally during early development.

## References

- Related issues: BT-305
- Related ADRs: [ADR 0001](0001-no-compound-assignment.md) (no compound assignment), [ADR 0002](0002-use-erlang-comparison-operators.md) (equality operators), [ADR 0023](0023-string-interpolation-and-binaries.md) (string interpolation), [ADR 0038](0038-subclass-classbuilder-protocol.md) (class definition syntax)
- Documentation: `docs/beamtalk-syntax-rationale.md` (primary source), `docs/beamtalk-language-features.md`
- [Smalltalk-80 Blue Book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf)
- [Newspeak Language](https://newspeaklanguage.org/)
