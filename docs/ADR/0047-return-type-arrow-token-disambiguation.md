# ADR 0047: Return Type Arrow Token Disambiguation

## Status
Proposed (2026-03-02)

## Context

Beamtalk uses `->` as the return type separator in method signatures (introduced in ADR 0025):

```beamtalk
balance -> Integer => self.balance
deposit: amount: Integer -> Integer => ...
```

The `->` characters are currently tokenised as `BinarySelector("->")` — the same token
kind used for any other binary operator. The parser distinguishes the return-type use
from a binary message send via a lookahead function (`is_return_type_then_fat_arrow`)
that checks whether `-> Identifier =>` follows the current position.

This lookahead fails in two confirmed cases discovered during BT-1003 (both are in the
stdlib; no user-defined `->` methods are currently affected):

### Ambiguity 1 — `->` as method selector

The `Object.->` method (Association creation) cannot be annotated with its return type:

```beamtalk
// Intended: binary method `->`, arg `value`, return type `Association`
-> value -> Association => @primitive "->"

// Parser sees: binary method `->` with TWO args — `value` and `Association`
// Codegen produces class_->/3 (self + value + Association); `Self` is unbound.
```

The root cause: the selector token `->` and the return-type separator token `->` are
indistinguishable. The lookahead cannot determine which `->` opens the return type.

### Ambiguity 2 — `class` as method name vs. class-side modifier

The `Class.class` method (returns the metaclass of a class) cannot be annotated:

```beamtalk
// Intended: sealed instance method `class`, return type `Metaclass`
sealed class -> Metaclass => @primitive "classClass"

// Parser sees: class-side binary method named `->` with arg `Metaclass`
// Codegen produces class_->/2; `Self` is unbound.
```

The root cause: `class` is both a valid method name (returning the receiver's class)
and the grammar keyword that introduces class-side method definitions. The parser
resolves `sealed class ->` as "sealed class-side binary method `->` with argument
`Metaclass`" rather than "sealed instance method `class` returning `Metaclass`".

This ambiguity is Beamtalk-specific. In Smalltalk, `class` is never a modifier
keyword — `(MyClass class) >> #myMethod` is a plain message send (`class` returns
the metaclass object; `>>` defines a method on it). Beamtalk's `class` modifier is
a syntactic convenience that collapses this two-step reflective pattern into a single
declaration, and that convenience is the source of the collision.

### Current workarounds

Both methods are left unannotated (`return_type: None` in `generated_builtins.rs`),
which reduces return-type coverage for chain resolution in the REPL (ADR 0045 Phase 1).

### Constraint

The `->` syntax for return types is well-established in the codebase (~590 annotated
methods across stdlib), well-precedented in comparable languages (Gleam, Rust, Swift),
and is the right choice semantically. The fix must not change user-visible syntax.

## Decision

Give `->` its own `TokenKind::Arrow` variant in the lexer, separate from
`BinarySelector`. In return-type position `->` is structural punctuation in a method
signature — no operands, no precedence — while in expression position (`x -> y`) it
remains a binary message send. Classifying it as `BinarySelector` conflates these two
roles; a dedicated token lets the parser resolve the role from position alone. The
parser is then updated to handle `Arrow` in all positions where `->` currently appears
— both as a return-type separator and as a binary method selector — using the
dedicated token to disambiguate.

### Lexer change

The lexer emits `TokenKind::Arrow` whenever it sees the two-character sequence `->`,
instead of wrapping it in `BinarySelector("->")`. `Arrow` is never a `BinarySelector`.

### Parser change — return type (no change to user syntax)

`parse_optional_return_type()` matches `TokenKind::Arrow` instead of
`BinarySelector("->")`. Behaviour is identical; only the internal token kind changes.

### Parser change — binary method selector with Arrow

`parse_method_selector()` is extended to recognise `TokenKind::Arrow` as a valid
binary method name. When the current token is `Arrow` the method selector is `"->"`.
After consuming `Arrow` and the argument name, `parse_optional_return_type()` is
called as normal. The two consecutive `Arrow` tokens are now unambiguous:

```
Arrow  argName  Arrow  TypeName  FatArrow
  ↑ selector      ↑ return type
```

This correctly handles:

```beamtalk
// Arrow selector, Arrow return-type separator — now unambiguous
-> value -> Association => @primitive "->"
```

### Parser change — `class` ambiguity

The parser's `parse_method_definition()` already handles the `class` modifier by
consuming it before parsing the selector. It must be updated so that when it sees
`class` followed by `Arrow TypeName FatArrow`, it treats `class` as the method name
(a unary selector) rather than as a class-side modifier, and does **not** consume it
as a modifier.

Concretely: before consuming `class` as a modifier, peek ahead. If the lookahead
matches `Arrow Identifier (Pipe Identifier)* FatArrow` after `class`, treat `class`
as the method selector and skip the modifier-consumption path.

```beamtalk
// class = unary method name, -> Metaclass = return type — now unambiguous
sealed class -> Metaclass => @primitive "classClass"
```

### Result

After this change, both previously-unannotatable methods can carry return types:

```beamtalk
-> value -> Association => @primitive "->"      // Object — Association creation
sealed class -> Metaclass => @primitive "classClass"  // Class — metaclass access
```

No user-visible syntax change. All existing annotations continue to work identically.

## Prior Art

**Gleam (BEAM language):** Uses `->` as the return type separator in function
signatures: `fn double(x: Int) -> Int`. Gleam does not have user-defined binary
operators, so `->` cannot be a function name — no ambiguity possible.

**Rust:** Uses `->` for function return types: `fn abs(x: i32) -> i32`. Custom
operators via `impl Trait` but not free binary `->` — no ambiguity.

**Newspeak:** Uses angle-bracket type annotations `<Type>` in method signatures.
No separator token that could double as a method selector.

**Pharo/Squeak:** No return type annotations in the core language. Type annotations
are layered via pragmas (`<return: #Integer>`) or external tools — avoids the
grammar problem entirely at the cost of out-of-band syntax. Notably, Pharo has no
`class` modifier keyword: class-side methods are defined by sending `class` to the
class object to navigate to its metaclass (`(MyClass class) >> #myMethod`), then
defining a method on that metaclass via `>>`. `class` has one meaning throughout —
a unary message returning the receiver's metaclass. The Ambiguity 2 collision is
absent because Smalltalk has no modifier syntax at all.

**Swift:** Uses `->` for return types. Operator overloading exists but `->` is not
overloadable — reserved entirely for the type position.

The common pattern among BEAM and modern languages is to reserve `->` exclusively
for type annotations. Beamtalk's Smalltalk heritage of arbitrary binary selectors
is the unique complicating factor; the dedicated-token approach resolves it without
abandoning either feature.

## User Impact

**Newcomer:** No visible change. The `->` syntax they learned from examples continues
to work. The error messages they would previously receive from accidentally writing
`-> value -> Type` are now gone — the code just works.

**Smalltalk developer:** The `->` binary message (Association creation) is a familiar
idiom. Keeping it working while also allowing return type annotation is the correct
outcome. No regression to Smalltalk mental model.

**Erlang/BEAM developer:** Unaffected. The generated Core Erlang is identical; the
fix is purely at the parse layer. Erlang `-spec` annotations generated from return
types become more complete (previously-unannotatable methods now carry specs).

**Tooling developer (LSP/IDE):** Positive. The LSP can now provide return type
information for `Object.->` and `Class.class` — two methods that appear frequently
in completions. The dedicated `Arrow` token also simplifies syntax highlighting rules
(one token kind for type arrows, one for binary selectors).

**Production operator:** No runtime change. Pure compile-time fix.

## Steelman Analysis

### Option B: Change separator to `^` (return keyword)

- 🧑‍💻 **Newcomer:** "The `^` already means return in Beamtalk — `^ Integer` reads
  'returns Integer' naturally. It's learnable from first principles without knowing
  the `->` convention."
- 🎩 **Smalltalk purist:** "Using `^` for return type is genuinely Smalltalk-flavoured.
  No other language uses `->` this way — we'd be original rather than derivative."
- ⚙️ **BEAM veteran:** "Doesn't matter — it's sugar over `-spec`. Either symbol works."
- 🎨 **Language designer:** "Reusing `^` exploits a concept already in the language
  rather than importing `->` from ML/Rust. Smaller conceptual surface area."

  **Why rejected:** `^` in expression position means early return. Readers scanning a
  method signature would see `balance ^ Integer =>` and momentarily parse `^` as a
  return statement. The disambiguation requires knowing you're in a signature, not a
  body — exactly the kind of context-dependent reading that makes languages harder to
  learn. The ~590 existing `->` annotations would require a mechanical migration.

### Option D: Parser-only fix (combined)

- 🧑‍💻 **Newcomer:** "No lexer change, no new token kind, no exhaustive-match cascade.
  The fix is invisible to anyone reading token.rs."
- ⚙️ **BEAM veteran:** "Token kinds are part of your compiler's public surface — fewer
  variants means less churn for downstream tooling (LSP, syntax highlighters, Tree-sitter
  grammars)."
- 🎨 **Language designer:** "Ambiguity 1 is already solvable without a new token: in
  `parse_method_selector`, seeing `BinarySelector(\"->\")` tells you unambiguously this
  is the selector; the next `->` must be the return-type arrow. Ambiguity 2 can be fixed
  with two extra lines in the existing `class`-keyword lookahead. Total diff: ~10 lines."

  **Why rejected:** The deeper problem is semantic, not syntactic. `->` plays two
  distinct roles: structural punctuation in signature position (no operands, no precedence)
  and a binary operator in expression position (`x -> y`). Lumping both into `BinarySelector`
  conflates them — every consumer must re-derive which role applies from surrounding context.
  `Arrow` gives the token stream a single, neutral identity; the parser then resolves the
  role from position, which is exactly where that decision belongs. A parser workaround
  patches the symptom; the dedicated token fixes the representation.

### Option C: Minimal workarounds per method

- 🧑‍💻 **Newcomer:** "If I never write a `->` binary method or a method named `class`,
  I'll never hit this. It's an edge case."
- 🎩 **Smalltalk purist:** "`class` being unannotatable is fine — Smalltalk never had
  return type annotations. Leave it alone."
- 🏭 **Operator:** "Fewer parser changes = fewer regressions. Just accept the two gaps."
- 🎨 **Language designer:** "The grammar is already complex. Don't add more lookahead."

  **Why rejected:** Leaves a permanent, documented limitation in the type system. Future
  binary methods (should `->` ever be used for something else) would hit the same wall.
  The fix is constrained to the parser layer and does not propagate complexity elsewhere.

### Tension points

Newcomers and operators both lean toward Option C (minimal change, minimal risk).
Language designers and LSP developers prefer the dedicated `Arrow` token approach
(correct fix, complete coverage, accurate representation).
The tiebreaker: the `Arrow` token approach is contained to ~50 lines in the parser and
lexer, with no observable behaviour change for any user.

## Alternatives Considered

### Alternative B: Change return type separator to `^`

Replace `->` with `^` for return type annotations throughout. See Steelman Analysis.
Rejected: visual confusion with early-return `^` in method bodies; migration cost.

### Alternative C: Leave specific methods unannotated

Accept `Object.->` and `Class.class` as permanently unannotatable. Document the
limitation. Remove `->` from Object as part of BT-1017 to eliminate ambiguity 1;
special-case `class` in parser documentation as a reserved method name.
Rejected: incomplete fix; leaves the grammar with a known, non-obvious limitation.

### Alternative D: Parser-only fix (count arrows in context)

Solve ambiguity 1 without a lexer change: in `parse_method_selector`, when the parser
sees `BinarySelector("->")` it knows this is the selector. After consuming the
parameter name, a second `BinarySelector("->")` must be the return type separator
(binary methods take exactly one parameter). This is deterministic.

Rejected: solves ambiguity 1 but not ambiguity 2 (`class` keyword). More fundamentally,
the return-type `->` is not a binary operator — it is structural punctuation with no
operands or precedence. Keeping it as `BinarySelector` is a misclassification that
all downstream consumers must compensate for individually. See Steelman Analysis.

### Alternative E: Parenthesise return types to disambiguate

```beamtalk
-> value (-> Association) => @primitive "->"
```

Rejected: syntactic noise; inconsistent with all existing annotations; leaks the
parser's internal ambiguity into user-visible syntax.

## Consequences

### Positive
- `Object.->` and `Class.class` gain return type annotations, improving REPL chain
  resolution coverage (ADR 0045 Phase 1)
- Future binary methods named `->` (unlikely but possible) work correctly
- `Arrow` gives `->` a neutral identity in the token stream; the parser resolves its
  role (structural punctuation in signature position, binary operator in expression
  position) from context — the right place for that decision
- Simplifies syntax highlighting and LSP tokenisation; consumers can distinguish
  return-type arrows from binary sends by parse position rather than string matching
- No user-visible syntax change; zero migration burden

### Negative
- Adds a new `TokenKind::Arrow` variant; all exhaustive `match` statements on
  `TokenKind` must be updated (compiler enforces exhaustiveness but not semantic
  correctness — each new arm must be reviewed individually)
- Expression-level binary message parsing (`expressions.rs`) must also handle `Arrow`
  alongside `BinarySelector` to preserve `x -> y` as an Association-creation send
- Parser `parse_method_selector`, `parse_method_definition`, and `is_at_method_definition`
  become slightly more complex to handle `Arrow` in selector position

### Neutral
- The `BinarySelector("->")` string value disappears from the token stream; any
  tooling that matched on it directly must match `Arrow` instead. This is an internal
  compiler/token-stream API change with no source-level syntax change, but downstream
  tooling that consumes the token stream (LSP, syntax highlighters, Tree-sitter grammars)
  will need to update their `->` handling.
- Ambiguity 2 (keyword-as-method-name) is a class of grammar problem. `sealed` and
  `override` are the other modifier keywords; neither is a plausible method name in
  existing or planned stdlib code, so no additional instances of this class are known.
  If future method names collide with modifiers, the same lookahead extension pattern
  applies.
- When generic type parameters land (e.g. `-> Map<K, V>`), `parse_optional_return_type`
  will need to handle parameterised types. The `Arrow` token fix is orthogonal to that
  work and does not make it harder.

## Implementation

**Phase 1 — Lexer (`token.rs`, `lexer.rs`):**
- Add `TokenKind::Arrow` variant to `token.rs`
- Implement `is_selector()`, `as_str()` (returns `Some("->")`), and `Display` for `Arrow`
- Update the lexer to emit `Arrow` for `->` instead of `BinarySelector("->")`.
  Insert the check in `lex_token_kind()` at the `-` branch: peek ahead for `>`
  and emit `Arrow`, analogous to how `=` peeks for `>` to emit `FatArrow`.
  This ensures `->` is intercepted before `lex_binary_selector()` accumulates
  it as a multi-character binary selector.
- Update all exhaustive `match` arms on `TokenKind` (compiler will identify them).
  Note: compiler enforces exhaustiveness but not semantic correctness — each arm
  must be reviewed for whether `Arrow` should be handled like `BinarySelector`
  or separately.

**Phase 2 — Parser declarations (`declarations.rs`):**
- `parse_optional_return_type()`: match `Arrow` instead of `BinarySelector("->")`
- `is_return_type_then_fat_arrow()`: match `Arrow` instead of `BinarySelector("->")`
  (three match sites within this function)
- `is_at_method_definition()`: add `Arrow` alongside `BinarySelector` in the binary
  method detection branch (line 267), so `-> value -> Association =>` is recognised
  as a method definition rather than falling through to "end of class body"
- `parse_method_selector()`: accept `Arrow` as a valid binary method selector name
- `parse_method_definition()`: extend the `class`-as-method-name check (line 619) to
  also break when `is_return_type_then_fat_arrow(1)` is true, reusing the existing
  lookahead infrastructure

**Phase 3 — Parser expressions (`expressions.rs`):**
- `parse_binary_with_pratt()` (line 300): extend the `while let BinarySelector`
  loop to also match `Arrow`, treating it as binary operator `"->"` in expression
  position
- `parse_cascade_message()` (line 193): extend binary message detection to match
  `Arrow` alongside `BinarySelector`
- Negative number pattern (line 863): no change needed — only matches `"-"`, not `"->"`

**Phase 4 — Stdlib annotations:**
- Add `-> Association` to `Object.->` in `Object.bt`
- Add `-> Metaclass` to `sealed class` in `Class.bt`
- Regenerate `generated_builtins.rs`

**Implementation scope:** The codebase has ~25 references to `BinarySelector` across
5 files (`token.rs`, `lexer.rs`, `declarations.rs`, `expressions.rs`, `mod.rs`). Of
these, 6 sites specifically match `BinarySelector(s) if s == "->"` and must be converted
to match `Arrow`; ~8 generic `BinarySelector(_)` sites must add `Arrow` alongside.
Comma-handling sites (`BinarySelector(s) if s == ","`) and the negative-number site
(`BinarySelector(op) if op == "-"`) are unaffected. The compiler enforces exhaustive
matching, so newly-added `TokenKind::Arrow` arms will be flagged at compile time.

**Phases 1–3 are a single atomic change** and must ship together; an `Arrow` token
emitted by the lexer without updated parser handling is a regression, not a partial
deliverable. Phase 4 (stdlib annotations) can follow in a subsequent commit.

**Affected components:** Lexer (`lexer.rs`, `token.rs`), parser (`declarations.rs`,
`expressions.rs`), stdlib sources, `generated_builtins.rs`. No codegen, runtime,
or REPL changes needed.

## References

- Related issues: BT-1018 (parser ambiguity cleanup), BT-1017 (Association/`->` on Object review)
- Related ADRs: ADR 0025 (Gradual Typing — introduced `->` return type syntax), ADR 0039 (Syntax Pragmatism)
- Discovered during: BT-1003 (stdlib return type annotation audit)
- BT-1017 note: if BT-1017 removes `->` from `Object`, Ambiguity 1 becomes moot, but the
  `Arrow` token remains the correct fix — any future binary method named `->` on any class
  would hit the same wall, and the semantic argument for a neutral token kind holds
  independently of whether `Object.->` exists.
