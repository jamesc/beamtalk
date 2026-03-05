# ADR 0053: Double-Colon Type Annotation Syntax

## Status
Proposed (2026-03-05)

## Context

ADR 0025 (Gradual Typing, Phase 2) specifies that parameter type annotations use a single colon (`:`) as the delimiter between parameter name and type:

```beamtalk
deposit: amount: Integer => ...
transfer: amount: Integer to: target: BankAccount => ...
detect: block: Block ifNone: noneBlock: Block -> Object => ...
```

The rationale was consistency with state declaration syntax (`state: balance: Integer = 0`).

### The Problem

In Smalltalk-style keyword messages, colons are already the selector delimiter. This creates a visual ambiguity: in `detect: block: Block ifNone: noneBlock: Block -> Object`, the colons after `block` and `noneBlock` look identical to the selector colons after `detect` and `ifNone`. A reader must mentally parse positional context to distinguish selector keywords from type annotations.

Compare:

```beamtalk
// Current — selector colons and type colons look the same
detect: block: Block ifNone: noneBlock: Block -> Object =>

// What a newcomer might read:
//   detect:block:Block:ifNone:noneBlock:Block  — is this a six-keyword selector?
```

This ambiguity compounds with complexity. A three-keyword method with typed parameters produces a wall of `identifier:` tokens where the reader must count positions to parse meaning.

Additionally, adding a type annotation *changes the parameter name* — `bindingName` becomes `bindingName:` — which is confusing since the colon is not part of the name but a type delimiter. The parameter name should be stable whether annotated or not.

### State Declarations

The same ambiguity exists in state declarations, though it's less acute because there's only one keyword (`state:`):

```beamtalk
state: balance: Integer = 0    // balance: looks like a keyword
```

### What's Not Changing

Return type syntax (`-> Type`) is not affected. The `->` arrow is already visually distinct from selector colons, and ADR 0047 recently formalised its tokenisation as `TokenKind::Arrow`.

## Decision

Use `::` (double colon) as the type annotation delimiter in method parameters and state declarations. Single `:` remains the keyword selector delimiter.

### Parameter Annotations

```beamtalk
// Unary — return type only (unchanged)
getBalance -> Integer => self.balance

// Keyword — one parameter, typed
deposit: amount :: Integer => self.balance := self.balance + amount

// Keyword — one parameter, typed, with return type
deposit: amount :: Integer -> Integer => self.balance := self.balance + amount

// Keyword — multiple parameters, all typed
transfer: amount :: Integer to: target :: BankAccount => ...

// Keyword — multiple parameters, mixed typed/untyped
inject: initial into: block :: Block => @primitive "inject:into:"

// Binary — type on operand
+ other :: Number -> Number => other

// Union types
maybeName: flag :: Boolean -> Integer | String => ...

// Self return type
collect: block :: Block -> Self => ...
```

### State Declarations

```beamtalk
state: balance :: Integer = 0
state: owner :: String = ""
state: direction :: Symbol
```

### Canonical Formatting

A space on both sides of `::` is the canonical form:

```beamtalk
// Canonical
deposit: amount :: Integer -> Integer => ...
state: balance :: Integer = 0

// Non-canonical but accepted
deposit: amount::Integer -> Integer => ...
deposit: amount ::Integer -> Integer => ...
```

The formatter should normalise to spaces on both sides.

### Complete Example — Collection Class

Before (current `:` syntax):

```beamtalk
abstract typed Object subclass: Collection
  class withAll: list: List -> Self => list
  size -> Integer => self subclassResponsibility
  do: _block: Block -> Nil => self subclassResponsibility
  includes: element: Object -> Boolean => ...
  inject: initial into: block: Block => @primitive "inject:into:"
  collect: block: Block -> Self => ...
  detect: block: Block -> Object | Nil => ...
  detect: block: Block ifNone: noneBlock: Block -> Object => ...
```

After (`::` syntax):

```beamtalk
abstract typed Object subclass: Collection
  class withAll: list :: List -> Self => list
  size -> Integer => self subclassResponsibility
  do: _block :: Block -> Nil => self subclassResponsibility
  includes: element :: Object -> Boolean => ...
  inject: initial into: block :: Block => @primitive "inject:into:"
  collect: block :: Block -> Self => ...
  detect: block :: Block -> Object | Nil => ...
  detect: block :: Block ifNone: noneBlock :: Block -> Object => ...
```

The selector `detect:ifNone:` is now immediately distinguishable from the parameter annotations `block :: Block` and `noneBlock :: Block`.

### Error Examples

```
// Missing type after ::
deposit: amount :: => ...
Error: Expected type name after '::', found '=>'

// Using single : (old syntax)
deposit: amount: Integer => ...
Warning: Single ':' type annotation syntax is deprecated, use '::' instead
         deposit: amount :: Integer => ...
```

## Prior Art

### Haskell
Uses `::` for type annotations: `factorial :: Integer -> Integer`. This is the strongest precedent for `::` meaning "has type" and is widely recognised in the functional programming community. Beamtalk adopts `::` with the same semantic meaning but in a different syntactic position (inline in method signatures rather than on a separate line).

### Scala
Uses `:` for type annotations: `def deposit(amount: Int): Int`. This works because Scala uses parentheses for parameters, so the `:` is unambiguous. In Beamtalk's keyword message syntax, parentheses are absent, making `:` ambiguous.

### Rust / Swift / TypeScript / Kotlin
All use `:` for type annotations in parameter lists. All have parenthesised parameters, so `:` is unambiguous. The lesson: `:` works when parameter boundaries are explicit. Keyword message syntax lacks explicit boundaries, requiring a more distinct delimiter.

### Newspeak
Uses `::` for module-level type declarations. This is the closest Smalltalk-family precedent for double-colon in a typing context.

### Erlang
Uses `::` in type specs: `-spec deposit(integer()) :: integer()`. Beamtalk's `::` aligns with Erlang's use of the same symbol for type annotation, which aids BEAM ecosystem familiarity.

### Pascal / Go
Use no delimiter for parameter types — type follows name with a space: `procedure deposit(amount Integer)` / `func deposit(amount int)`. This was considered but rejected as it makes the type annotation invisible (no syntactic marker).

## User Impact

### Newcomer (from Python/TypeScript/Rust)
The newcomer knows `:` for types from their home language. `::` is slightly unfamiliar, but the meaning is immediately guessable from context (`amount :: Integer` reads as "amount is of type Integer"). The benefit is that the newcomer no longer confuses type annotations with keyword selectors — the two most distinctive features of Beamtalk syntax are now visually distinct.

### Smalltalk Developer
There is no type annotation in Smalltalk-80, so no Smalltalk convention is being broken. `::` is new syntax for a new feature. The key benefit for Smalltalkers is that keyword message selectors remain visually pure — colons only appear as part of message names, never as type delimiters.

### Erlang/BEAM Developer
`::` is already the type annotation operator in Erlang's type spec syntax (`-spec foo(integer()) :: integer()`). Using the same symbol in Beamtalk creates natural familiarity and reinforces the BEAM ecosystem connection.

### Tooling Developer (LSP)
`::` is a distinct two-character token, making it trivially lexable. The parser never needs to disambiguate "is this `:` a keyword separator or a type delimiter?" — all colons are keyword separators, all double-colons are type annotations. This simplifies parser recovery for incomplete code (critical for LSP).

## Steelman Analysis

### For Single Colon `:` (Current Syntax — Rejected)

- **Newcomer**: "`:` is universal — Python, TypeScript, Rust, Kotlin all use it. `::` is unfamiliar and adds cognitive overhead for the most common typing annotation."
- **Smalltalk purist**: "Minimalism matters. `:` is one character, `::` is two. More syntax is more friction."
- **BEAM veteran**: "Erlang actually uses `::` AND `:` in specs — the argument that `::` is 'the Erlang way' oversimplifies."
- **Language designer**: "Consistency with state declarations (`state: balance: Integer = 0`) is a real benefit. Changing both parameter AND state syntax to `::` is a larger disruption than the ambiguity warrants."

**Response**: The ambiguity is not hypothetical — it manifests in every multi-keyword method with typed parameters, which is the common case. The consistency argument is preserved since state declarations also move to `::`. The familiarity argument is valid but mitigated: `::` is immediately readable in context, and the syntax it replaces (`: Type` after a keyword parameter) is itself unfamiliar to developers from `:` languages because those languages use parenthesised parameters.

### For No Delimiter (Space-Separated Types)

- **Language designer**: "The simplest syntax: `deposit: amount Integer`. No delimiter character at all."
- **Newcomer**: "Pascal and Go do this — it works."

**Response**: Without a delimiter, the boundary between parameter name and type is invisible. `inject: initial into: block Block` — is `block` a parameter name or a type? The delimiter serves as a visual anchor. Space-only separation works in languages with fixed parameter syntax (parenthesised lists) but not in Beamtalk's flexible keyword message structure.

### Tension Points

- **Newcomers** would slightly prefer `:` (familiar from other languages) but benefit most from `::` (resolves the keyword ambiguity that confuses them)
- **Smalltalk developers** are neutral — neither `:` nor `::` exists in Smalltalk for types
- **Erlang developers** mildly prefer `::` (matches Erlang spec syntax)
- **All cohorts agree** that the multi-keyword ambiguity is a real readability problem

## Alternatives Considered

### Alternative A: Keep Single Colon (Status Quo)

```beamtalk
detect: block: Block ifNone: noneBlock: Block -> Object => ...
state: balance: Integer = 0
```

Preserve the syntax from ADR 0025 Phase 2. Accept the visual ambiguity.

**Rejected because:** The ambiguity is a persistent readability problem in multi-keyword methods, which are common in Smalltalk-style APIs. Adding a type to a parameter changes the parameter's visual identity (`bindingName` → `bindingName:`), which is confusing.

### Alternative B: Space-Separated Types (No Delimiter)

```beamtalk
detect: block Block ifNone: noneBlock Block -> Object => ...
state: balance Integer = 0
```

Type follows parameter name with only a space. Similar to Pascal/Go.

**Rejected because:** The boundary between parameter name and type is invisible. In `inject: initial into: block Block`, `block` could be read as either a parameter name or a type. A syntactic marker is needed to make the annotation explicit.

### Alternative C: Angle Brackets or Parentheses

```beamtalk
detect: block<Block> ifNone: noneBlock<Block> -> Object => ...
// or
detect: block(Block) ifNone: noneBlock(Block) -> Object => ...
```

Use brackets to wrap the type.

**Rejected because:** Angle brackets are reserved for protocol types (`<Printable>`) per ADR 0025 Phase 3. Parentheses clash with expression grouping. Both add more visual noise than `::`.

## Consequences

### Positive
- Keyword selectors and type annotations are visually distinct — a reader can immediately identify selector keywords (single `:`) vs type annotations (double `::`)
- Parameter names are stable — `bindingName` remains `bindingName` whether annotated or not
- Aligns with Erlang's `::` type spec syntax, reinforcing BEAM ecosystem familiarity
- Simplifies parser implementation — no ambiguity between keyword colon and type colon
- LSP benefits from unambiguous tokenisation for error recovery in incomplete code
- `::` is a distinct two-character token, making it trivially recognisable in syntax highlighting

### Negative
- Departures from the `:` convention used by most mainstream typed languages (TypeScript, Rust, Kotlin, Swift) — though these languages all use parenthesised parameters where `:` is unambiguous
- Requires updating ~35 parameter annotations, ~13 state annotations across stdlib, plus ~87 test files and documentation
- Supersedes the parameter type syntax specified in ADR 0025 Phase 2

### Neutral
- `::` for namespaces (`Banking::Counter`) is a v0.2+ future consideration (ADR 0031). The syntactic contexts are distinct — `::` in a method signature is always a type annotation, `::` between two identifiers in expression position is a namespace qualifier. The parser can disambiguate by context. If this becomes a concern, the namespace ADR can choose a different separator.
- Return type syntax (`-> Type`) is unchanged

## Implementation

### Phase 1: Lexer and Parser Changes
- Add `TokenKind::DoubleColon` to the lexer
- Update `parse_type_annotation()` in `declarations.rs` to expect `::` instead of `:` for parameter types
- Update state declaration parsing to expect `::` for state types
- Update error messages to reference `::` syntax

### Phase 2: Migrate Stdlib and Tests
- Update all `.bt` files in `stdlib/src/` with new `::` syntax (~35 parameter annotations, ~13 state annotations)
- Update test `.bt` files
- Update example files in `examples/` and `test-package-compiler/cases/`

### Phase 3: Documentation
- Update `docs/beamtalk-language-features.md` (type annotations section)
- Update ADR 0025 to note superseded parameter type syntax
- Update CLAUDE.md examples if affected

### Phase 4: Deprecation Period (Optional)
- Accept both `:` and `::` for one release cycle with a deprecation warning
- Remove single-colon support in the following release

**Affected components:** Lexer, parser, formatter, documentation, stdlib `.bt` files, test `.bt` files, example files. No codegen changes (type annotations are erased before codegen). No runtime changes.

## Migration Path

All existing parameter type annotations (`param: Type`) and state type annotations (`state: field: Type = default`) must be updated to use `::`:

```beamtalk
// Before
deposit: amount: Integer -> Integer => ...
state: balance: Integer = 0

// After
deposit: amount :: Integer -> Integer => ...
state: balance :: Integer = 0
```

A deprecation period (Phase 4) can ease the transition by accepting both forms with a warning. Given that type annotations were introduced recently (ADR 0025 Phase 2), the migration surface is small.

## References
- Related ADRs:
  - [ADR 0025](0025-gradual-typing-and-protocols.md) — Gradual Typing and Protocols (supersedes Phase 2 parameter type syntax)
  - [ADR 0039](0039-syntax-pragmatism-vs-smalltalk.md) — Syntax Pragmatism vs Smalltalk (design philosophy)
  - [ADR 0047](0047-return-type-arrow-token-disambiguation.md) — Return Type Arrow Token Disambiguation (related tokenisation change)
  - [ADR 0031](0031-flat-namespace-for-v01.md) — Flat Namespace for v0.1 (future `::` namespace qualifier consideration)
- Documentation: `docs/beamtalk-language-features.md` (type annotations section)
- Prior art:
  - [Haskell type signatures](https://wiki.haskell.org/Type_signature) — `::` for "has type"
  - [Erlang type specs](https://www.erlang.org/doc/reference_manual/typespec) — `::` in `-spec` and `-type`
  - [Newspeak language spec](https://newspeaklanguage.org/) — `::` for module-level type declarations
