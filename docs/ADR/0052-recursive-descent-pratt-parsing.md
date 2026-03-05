# ADR 0052: Recursive Descent Parser with Pratt Parsing for Binary Operators

## Status
Accepted | Implemented (2026-03-05)

## Context

### Problem Statement

Beamtalk's compiler is designed as a language service first — the parser must support IDE operations (completions, diagnostics, formatting, go-to-definition) with sub-100ms response times (Principle 12). This imposes hard constraints on parser design:

- **Error recovery is mandatory**: the parser must always produce a usable AST, even for incomplete or malformed input mid-keystroke
- **Trivia preservation**: comments, whitespace, and newlines must be preserved in the AST for formatting and doc extraction
- **Precise source spans**: every AST node must carry its exact source range for IDE diagnostics to point at the right token
- **Incremental-friendly**: parsing must be fast enough to re-run on every keystroke

### Binary Operator Precedence

ADR 0039 established that Beamtalk departs from Smalltalk's pure left-to-right binary evaluation in favour of standard math precedence (`*` binds tighter than `+`). This requires the parser to implement a full precedence hierarchy rather than a flat left-to-right rule.

The initial implementation handled binary precedence with three dedicated methods:

- `parse_comparison()`: `<`, `>`, `<=`, `>=`, `==`, `/=` (Erlang-style operators — ADR 0002)
- `parse_additive()`: `+`, `-`
- `parse_multiplicative()`: `*`, `/`, `%`

Each method was ~20 LOC, largely duplicated. Adding a new operator required creating a new method, threading it into the precedence call chain, and updating match arms — a ~20 LOC change across 2 files.

### Research Spike

BT-109 evaluated parser alternatives. The full findings are in `docs/internal/parser-architecture.md`.

## Decision

**Keep the hand-rolled recursive descent parser, enhanced with Pratt parsing for binary operator precedence.**

### Recursive Descent for Structure

The top-level parser structure remains hand-rolled recursive descent. One function per grammar production, straightforward control flow, error recovery via synchronisation at statement boundaries. This approach is well understood, easy to step through in a debugger, and gives full control over error messages and trivia attachment.

### Pratt Parsing for Binary Operators

Binary operator precedence is handled with a Pratt (top-down operator precedence) parser. A single binding power table maps operator tokens to numeric precedence levels. One recursive function — `parse_binary_with_pratt` — handles all binary operators:

```rust
/// Binding power table — the ONLY place to add or change binary operator precedence.
fn binary_binding_power(op: &str) -> Option<BindingPower> {
    match op {
        ">>"                         => Some(BindingPower::left_assoc(5)),  // Method lookup
        "==" | "/=" | "=:=" | "=/=" | "=" => Some(BindingPower::left_assoc(10)), // Equality
        "<"  | ">"  | "<="  | ">="  => Some(BindingPower::left_assoc(20)), // Comparison
        "+"  | "-"  | "++"           => Some(BindingPower::left_assoc(30)), // Additive
        "*"  | "/"  | "%"            => Some(BindingPower::left_assoc(40)), // Multiplicative
        "**"                         => Some(BindingPower::right_assoc(50)), // Exponentiation
        _ => None, // Unknown operator ends the binary expression
    }
}
```

Adding a new operator (e.g., bitwise `&`) is a single line:

```rust
"&" => Some(BindingPower::left_assoc(25)),
```

### What This Looks Like in Practice

Beamtalk programmers do not interact with the parser directly. The effects are visible through correct expression evaluation and accurate IDE diagnostics:

```beamtalk
// Pratt parsing ensures these evaluate as expected:
2 + 3 * 4        // => 14   (not 20 — * binds tighter than +)
10 - 2 - 3       // => 5    (left-associative: (10 - 2) - 3)
2 ** 3 ** 2      // => 512  (right-associative: 2 ** (3 ** 2))
x > 0 =:= y > 0 // => true if both positive (comparison before equality)

// Method lookup has lowest binary precedence:
Counter >> #increment  // => CompiledMethod object

// Errors point to the right token:
2 + * 4          // Error: expected expression, found `*` (column 5)
```

The IDE can report diagnostics in-flight while typing — the parser does not abort on the first error.

## Prior Art

### Recursive Descent: The IDE-First Standard

Hand-rolled recursive descent is the parser architecture of choice in production language tooling:

- **rust-analyzer**: full recursive descent, Pratt for expressions — cited as direct inspiration
- **TypeScript**: hand-rolled recursive descent throughout, enabling the TypeScript Language Service
- **Gleam**: hand-rolled recursive descent, same motivation (LSP-first)
- **Roslyn (C#)**: hand-rolled recursive descent for full error recovery and IDE integration

These projects share a common finding: the control and debuggability of hand-rolled parsers outweigh the abstraction benefits of combinators or generators when IDE responsiveness is a first-class requirement.

### Pratt Parsing: Well-Established Algorithm

Pratt parsing originates with Vaughan Pratt (1973) and has become standard for expression parsing:

- **rust-analyzer**, **TypeScript**, **V8** all use Pratt (or equivalent binding-power) for expressions
- matklad's tutorial "Simple but Powerful Pratt Parsing" (2020) is the direct reference for this implementation

### Traditional Smalltalk: No Precedence

Traditional Smalltalk (Pharo, Squeak, Smalltalk-80) evaluates binary messages strictly left-to-right with no numeric precedence hierarchy. `2 + 3 * 4` evaluates to `20`, not `14`. Beamtalk departs from this — see ADR 0039 for the full rationale. The Pratt implementation is what makes this departure possible at all.

## User Impact

**Newcomer (Python/JS/Rust background):**
Operators behave as expected. `2 + 3 * 4` is `14`. Errors point to the right token. IDE completions work mid-expression. The parser is invisible when it's working correctly, and helpful when it isn't.

**Smalltalk developer:**
The expression evaluation order differs from Pharo/Squeak — see ADR 0039. The message-passing syntax and three-tier message hierarchy (unary > binary > keyword) is preserved. Only the intra-binary precedence ordering changes.

**Erlang/BEAM developer:**
No impact. The parser is a Rust crate producing an AST; it does not affect generated BEAM code or runtime behaviour.

**Tooling developer (LSP, debugger, formatter):**
The AST preserves trivia (whitespace, comments) and source spans on every node. Error nodes appear in-place rather than halting the parse. This makes formatting, completion, and diagnostics straightforward to implement without special-casing partially-written code.

**Production operator:**
No direct impact. Parse errors are compiler-time, not runtime. Faster parsing means faster REPL iteration in production use.

## Steelman Analysis

### Option A: Parser Combinators (Chumsky)

- 🧑‍💻 **Newcomer**: "The combinator API feels like composable building blocks — I can see what each piece does without tracing call chains."
- 🎩 **Smalltalk purist**: "Combinators model message-passing naturally. `parse_number.then(parse_operator.then(parse_number))` reads like a grammar."
- ⚙️ **BEAM veteran**: "Libraries with strong abstractions are idiomatic in the Erlang/Elixir world. Chumsky's typed error recovery aligns with that philosophy."
- 🎨 **Language designer**: "Combinator parsers compose cleanly. Adding a new expression form means combining existing parsers, not threading a new function through a call chain."

**Why rejected anyway:** Chumsky uses Pratt parsing internally for its `pratt()` combinator — adopting it adds a dependency to get the same algorithm. Chumsky's generic types make error messages and stack traces harder to read. Error recovery is combinator-configured rather than explicit synchronisation points, which is harder to tune for IDE responsiveness. Chumsky was `1.0.0-alpha` at evaluation time.

### Option B: Parser Generators (lalrpop, pest)

- 🧑‍💻 **Newcomer**: "A grammar file reads like documentation. I can understand the entire language syntax from one file."
- ⚙️ **BEAM veteran**: "Declarative grammars have a long history in Erlang tooling (leex, yecc). Grammar-first feels disciplined."
- 🎨 **Language designer**: "The grammar is the single source of truth. Changes to the language surface automatically propagate to the parser."

**Why rejected anyway:** Parser generators (LALR, PEG) require all input to be valid-enough to reduce. Error recovery requires significant extra work to customise. Trivia handling needs a preprocessing layer. Smalltalk-style messages (context-sensitive keyword message arity, statement separators) don't map naturally to LR/PEG grammars without substantial encoding complexity.

### Option C: rowan / ungrammar (Lossless CST)

- 🧑‍💻 **Newcomer**: "The `ungrammar` file reads like a language reference — I can see every node type and its children without reading parser code."
- 🎩 **Smalltalk purist**: "A lossless CST preserves every token including whitespace — closer to Smalltalk's principle that the source is the truth and the system is fully introspectable."
- ⚙️ **BEAM veteran**: "This is what rust-analyzer actually uses. If it scales to Rust's grammar complexity, it can handle Beamtalk."
- 🏭 **Operator**: "Typed AST views generated from a grammar file are structurally correct by construction — no risk of forgetting to handle a new node type."
- 🎨 **Language designer**: "Comment attachment (ADR 0044), trivia preservation, and source spans are solved problems in rowan. We'd get them for free instead of implementing them per-production."

**Why rejected anyway:** Path-dependent — the working parser existed before rowan was evaluated. Adopting rowan would require rewriting the AST representation across ~35+ files. At the current codebase scale, the ceremony of typed accessor wrappers over an untyped CST is not yet justified by the grammar's complexity. If the grammar grows to the point where hand-rolled disambiguation becomes a maintenance burden (signals: ADR 0047, ADR 0048), rowan is the natural migration target.

### Option D: tree-sitter

- 🧑‍💻 **Newcomer**: "A grammar file is self-documenting — I can understand the entire language syntax in 200 lines, and I get syntax highlighting in every editor for free."
- 🎩 **Smalltalk purist**: "Smalltalk-80 tools were built around image inspection, not external parser generators. But tree-sitter at least gives us a concrete syntax tree that preserves the full source — closer to the Smalltalk spirit of total source accessibility."
- ⚙️ **BEAM veteran**: "tree-sitter is battle-tested across hundreds of languages. Its GLR algorithm handles ambiguous grammars that trip up simpler approaches. Neovim's tree-sitter support means Erlang/Elixir developers would get immediate editor integration."
- 🏭 **Operator**: "tree-sitter's incremental parsing is verifiably fast — microseconds per keystroke. The formal grammar makes it easier to audit parser behaviour."
- 🎨 **Language designer**: "The grammar file is the language spec. Any change to the grammar is immediately visible and reviewable in one place."

**Why rejected anyway:** tree-sitter produces a CST that must be lowered into an AST for compilation. This creates a dual-tree problem: two representations of the same source, two transformation steps, and divergence risk between the tree used for IDE queries and the tree used for codegen. Beamtalk's compiler-as-language-service principle depends on a single AST serving both purposes.

### Tension Points

Tooling developers would find tree-sitter compelling — it provides the strongest incremental-parsing story and free editor integration. The tension is between single-tree simplicity (hand-rolled: compiler AST serves IDE and codegen) and ecosystem reach (tree-sitter: immediate Neovim/Helix support from a grammar file). This project weights single-tree simplicity and IDE diagnostic quality higher. A tree-sitter grammar for syntax highlighting remains a viable future addition outside the compiler pipeline.

## Alternatives Considered

### Chumsky (Parser Combinator)

Chumsky is a Rust parser combinator library with error recovery support and a `pratt()` combinator for operator precedence.

**Why not adopted:**

| Factor | Chumsky | Hand-rolled + Pratt |
|--------|---------|---------------------|
| Precedence handling | Uses Pratt internally | Uses Pratt directly |
| Error recovery | Combinator-based (harder to tune) | Explicit sync points |
| Trivia handling | Requires token adapter layer | Integrated |
| Debugging | Complex generic types | Simple call stack |
| Dependencies | Adds external crate | None |
| API stability | 1.0.0-alpha at evaluation | Stable |

The core finding: Chumsky wraps the same algorithm with more indirection. There is no algorithmic advantage — only complexity.

### lalrpop / pest (Parser Generators)

LALR (lalrpop) and PEG (pest) generators produce parsers from grammar files.

**Why not adopted:**
- Grammar files describe valid input; error recovery for invalid input requires significant out-of-band work
- Smalltalk-style keyword messages (where arity is determined by colon count in a single selector, not grammar production) are awkward to express in LR grammars
- Trivia preservation would require a separate preprocessing stage
- Statement separators (optional newlines, optional `.`) are whitespace-sensitive in a way that LR grammars handle poorly

### rowan / ungrammar (Lossless CST)

rust-analyzer — the ADR's cited inspiration — does not use a hand-rolled AST in isolation. It uses `rowan`, a red-green tree library that provides a lossless concrete syntax tree preserving all trivia. A typed AST layer is generated from an `ungrammar` specification as a zero-cost view over the CST, not a separate tree. This eliminates the "dual-tree problem" cited against tree-sitter while retaining the benefits of a formal grammar description.

**Why not adopted:**
- Beamtalk's parser was written before rowan was evaluated; adopting it retroactively would require a substantial rewrite of the AST representation (~35+ files reference `Expression` and `Module` types directly)
- rowan's untyped CST (`SyntaxNode`) requires wrapping every access in typed accessors — acceptable for a large project like rust-analyzer, but higher ceremony than Beamtalk's AST enum at the current codebase scale
- The typed AST generated from `ungrammar` is a derived artefact, not hand-written — this trades hand-written parsing functions for hand-written grammar files, shifting complexity rather than removing it

**Honest assessment:** rowan is the strongest alternative not adopted. If Beamtalk's parser were being designed today from scratch, a rowan-based lossless CST would be a serious contender. The decision to keep the hand-rolled AST is partly path-dependent — the working parser existed before this alternative was evaluated. If future grammar complexity (see ADR 0047, ADR 0048) makes the hand-rolled approach untenable, rowan is the most likely migration target.

### tree-sitter

tree-sitter is a GLR parser generator designed specifically for editor tooling. It generates fast, incremental, error-recovering parsers from a grammar file, and is the basis for syntax highlighting in Neovim, Helix, GitHub code views, and many other tools.

tree-sitter is arguably the strongest IDE-focused alternative and warrants detailed evaluation.

**What tree-sitter provides:**
- Incremental re-parsing: only the changed region of the file is re-parsed on each keystroke
- Error recovery built into the GLR algorithm: always produces a tree
- Grammar file as a single authoritative description of the language
- A concrete syntax tree (CST) preserving all tokens including whitespace and comments
- Broad editor ecosystem: a `tree-sitter-beamtalk` grammar would immediately enable syntax highlighting in all editors with tree-sitter support

**Why not adopted:**

| Factor | tree-sitter | Hand-rolled + Pratt |
|--------|-------------|---------------------|
| Architecture | Dual-tree (CST + AST transformation) | Single AST for IDE and codegen |
| Language | Grammar file → C library (FFI from Rust) | Pure Rust |
| Diagnostics | Structural error nodes; semantic errors need separate pass | Full semantic diagnostics in one pass |
| Operator precedence | Supported in grammar | Pratt table |
| Trivia | Full CST (all tokens) | Selective trivia attachment |
| Debugging | Generated C code, harder to step through | Plain Rust call stack |
| Dependencies | C FFI (`tree-sitter` crate) | None |

The decisive factor is the **dual-tree problem**. tree-sitter produces a CST — every token including whitespace. For an IDE syntax highlighter this is ideal. For a compiler that also does type checking and code generation, the CST must be lowered into a separate AST. This means maintaining two tree representations, a transformation step, and two sources of truth for source positions. Beamtalk's compiler-as-language-service architecture (Principle 12) depends on the compiler's own AST serving IDE queries directly — adding a separate tree-sitter pass would either duplicate work or introduce a divergence risk between the two representations.

A tree-sitter grammar for Beamtalk may still be worth developing independently for editor syntax highlighting — that would not conflict with this decision. But it would be editor tooling, not the compiler pipeline.

## Consequences

### Positive

- **IDE-first by construction**: Error recovery, trivia preservation, and precise spans are built in, not bolted on — directly enabling the LSP and formatter
- **Operator extensibility**: Adding a new binary operator requires one line in `binary_binding_power`; adding right-associativity is equally declarative
- **Zero new dependencies**: No additional Rust crates required
- **Debuggable**: Plain recursive descent with a simple call stack; no combinator generic types or generated state machines to step through
- **Battle-tested algorithm**: Pratt parsing is used in rust-analyzer, TypeScript, and V8 — well-understood failure modes

### Negative

- **More code to maintain**: The parser is ~7,700 LOC across three modules (`mod.rs`, `expressions.rs`, `declarations.rs`), plus ~350 LOC of property-based tests. Parser combinators or generators would reduce the per-construct code, but the overall size reflects the language's complexity, not the parser approach. Every new language construct requires writing a new parsing function. When ADR 0044 (comment attachment) is implemented, every new *statement-position* parse function will also require calling `collect_comment_attachment()` and handling trailing comment attachment — increasing the per-construct authoring cost beyond what the single-function claim implies
- **Refactoring discipline required**: Without a grammar file as a single source of truth, the parser and the language spec can drift. The mitigation is social (documentation + test requirements), not structural — no CI check verifies that `docs/beamtalk-language-features.md` and the parser agree. This risk increases with grammar complexity
- **"Fast enough to re-run" is a pragmatic bet**: The parser currently re-runs fully on every keystroke rather than incrementally re-parsing the changed region. This works because Beamtalk follows Smalltalk's one-class-per-file convention, keeping files small. No parse-time budget has been established — if files grow large enough for full re-parse to exceed ~50ms, incremental parsing (rowan or tree-sitter) would need to be reconsidered
- **Context-sensitive disambiguation scales linearly with grammar complexity**: Pratt parsing addresses intra-binary precedence only. Context-sensitive disambiguation in declaration position (method selectors that look like structural tokens, keywords that double as identifiers) is handled by lookahead in the recursive descent. As the grammar acquires more token overloading (see ADR 0047 arrow disambiguation, ADR 0048 class keyword collision), this lookahead burden grows with each new construct

### Neutral

- The three-tier message hierarchy (unary > binary > keyword) is structural, not table-driven; only intra-binary precedence uses the Pratt table. The "one line to add a new operator" benefit applies solely to binary operators within this existing hierarchy — adding a new precedence tier or new operator categories (prefix, postfix, mixfix) would require structural parser changes, not table entries
- The `parse_binary_with_pratt` function handles all binary operators; adding non-binary forms (unary, keyword) still follows the existing recursive descent pattern

## Implementation

### Completed (BT-110)

The implementation is fully in place:

- `crates/beamtalk-core/src/source_analysis/parser/mod.rs`
  - `BindingPower` struct with `left_assoc` and `right_assoc` constructors
  - `binary_binding_power()` table mapping operator tokens to binding powers
- `crates/beamtalk-core/src/source_analysis/parser/expressions.rs`
  - `parse_binary_with_pratt()` — the single function handling all binary operator precedence
- 6 Pratt-specific unit tests covering precedence, associativity, and edge cases

The three original methods (`parse_comparison`, `parse_additive`, `parse_multiplicative`) were replaced by `parse_binary_with_pratt` with all existing tests passing unchanged.

### Extending the Parser

To add a new binary operator:

1. Add one entry to `binary_binding_power` in `parser/mod.rs`
2. Add a test to verify the new precedence level
3. Update `docs/beamtalk-language-features.md` if the operator is user-visible

No other files need to change.

## References

- Related issues: BT-109 (research spike), BT-110 (implementation), BT-300 (this ADR)
- Related ADRs: ADR 0039 (math precedence departure from Smalltalk), ADR 0044 (comments as first-class AST nodes), ADR 0047 (arrow token disambiguation), ADR 0048 (class-side method syntax)
- Source material: `docs/internal/parser-architecture.md`
- Pratt, V.R. (1973). "Top down operator precedence." POPL.
- matklad, "Simple but Powerful Pratt Parsing": https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- rust-analyzer parser: https://github.com/rust-lang/rust-analyzer/tree/master/crates/parser
