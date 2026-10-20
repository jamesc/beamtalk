# ADR 0136: Tuple Literal Expressions

## Status
Implemented (2026-10-20)

## Context

### The problem

`{a, b}` is accepted as a **pattern**, in `match:` arms and in destructuring assignment (`{k, v} := pair`), but there has been no tuple literal **expression**. A tuple value had to be built with `Tuple withAll: #(...)`, which allocates a list and dispatches a message.

FFI-heavy code builds Erlang option lists all the time. A `gen_tcp` listener looks like this today:

```beamtalk
(Erlang gen_tcp) listen: port with: #(
  #binary,
  Tuple withAll: #(#active, false),
  Tuple withAll: #(#reuseaddr, true),
  Tuple withAll: #(#backlog, 1024))
```

Thousand Island's `Transport.resolve_options/1` merges required, user and default option lists. The Catalina port (BT-3957) does the same in `TransportOptions` over about ten such tuples, and every `setopts:` call site adds another one.

### What earlier decisions said

- **ADR 0012** (collection literals), § Future Work, set aside bare braces `{1, 2, 3}` as the "likely choice since braces have no code use". It was meant for a *tagged Array* literal and deferred to its own ADR. That plan no longer applies: Array got `#[...]` and a map-backed representation (ADR 0090). Braces are free for the raw `Tuple`, which ADR 0012 already described as the class of raw Erlang tuples.
- `docs/beamtalk-syntax-rationale.md` § Complete Syntax Summary already listed `Tuple | {a, b, c}`. The docs promised the syntax, but the parser never got it.
- **ADR 0023** (string interpolation) relies on "`{` is not a valid expression start" in exactly one place. The lexer rejects `"{{...}}"` as a mistaken literal-brace escape.
- No ADR discussed or rejected Smalltalk's `{a. b}` brace arrays.

### Constraints

- The existing destructuring grammar must keep working unchanged, including pattern-only forms that are not expressions: `x :: Integer` type patterns, binary `<<...>>` patterns and nested map patterns.
- A tuple literal must be exactly an Erlang tuple. FFI calls get it unchanged.
- Codegen uses the `Document`/`docvec!` typed-leaf API (ADR 0089). Sub-expression ordering follows the literal-container sequencing rule (ADR 0118).

## Decision

`{e1, e2, ...}` **is a primary expression that evaluates to a raw Erlang tuple (class `Tuple`).** It is the expression twin of the existing tuple pattern.

### Grammar

```
primary      ::= ... | tupleLiteral
tupleLiteral ::= '{' [ element { ',' element } [ ',' ] ] '}'
element      ::= keywordExpr     // same level as #( ), #[ ], #{ } values
```

- **Separator: comma only.** This matches `#(...)`, `#[...]`, `#{...}`, the tuple pattern and Erlang. Smalltalk's period-separated `{a. b}` is rejected with a hint, because `.` is the statement separator.
- **Element precedence:** each element is a full keyword message, so `{#backlog, opts at: #backlog}` has two elements. A cascade or an assignment inside an element needs parentheses, as it does in a list.
- **Trailing comma:** allowed (`{#a, 1,}`), as in the other collection literals. The tuple *pattern* gets the same allowance so patterns and expressions stay symmetric. The formatter drops it.
- **`{x}` and `{}`:** both allowed, as in Erlang and in the pattern grammar. They are not ambiguous: blocks use `[ ]` and grouping uses `( )`. `#{}` is a different token (`MapOpen`), so `{}` and `#{}` never collide.
- **Precedence:** a tuple literal is a primary, so `{a, b} size`, `{a, b} =:= t`, `foo with: {a, b}`, `^ {a, b}` and a tuple map key `#{{1, 2} => x}` all work.

### `{a, b} := x` versus `{a, b} size` at the start of a statement

`parse_assignment` used to treat every leading `{` as a destructuring pattern. It now uses **bounded token lookahead**:

1. At a leading `{`, scan forward to the matching `}`, counting `{` and `#{` against `}` and stopping at `Eof`.
2. If the token after that `}` is `:=`, parse a destructure with the full pattern grammar.
3. Otherwise parse an ordinary expression, which reaches the new tuple-literal arm of `parse_primary`.

Consequences of the rule:

- `{a, b} := {b, a}` swaps: the right-hand side goes through `parse_assignment` again and has no `:=` after its `}`.
- `{a, b} :: T := x` still gives "Type annotations on destructuring assignments are not yet supported", now through the `::` branch of `parse_assignment`.
- An unbalanced `{` takes the expression path and gets "Expected '}' to close tuple literal".
- `({a, b}) := x` gets the existing "Assignment target must be an identifier or field access" error.
- `match:` arms are parsed by `parse_pattern`, and arm and block bodies by `parse_expression`, so they need no special handling.

### String interpolation

The lexer still rejects an opening `"{{"`. Writing `{{` for a literal brace is a far more common mistake than wanting to interpolate a tuple. To interpolate a tuple literal, add a space: `"pair: { {a, b} }"`. Only an *opening* `{{` is special; a closing `}}` already nests correctly. The doubled-brace error message now says how to interpolate a tuple literal, and the unparser pads the interpolation when an interpolated expression starts with `{`, so formatting round-trips.

### Semantics, codegen and types

- **Representation:** a raw BEAM tuple with no tag. `{#ok, 1} =:= (Tuple withAll: #(#ok, 1))` is `true`. Symbols and `true`/`false`/`nil` are atoms, so `{#active, false}` is exactly Erlang's `{active, false}`.
- **Not a Result:** `{#ok, x}` is a `Tuple`. ADR 0076 converts `{ok, V}` / `{error, R}` to `Result` only on FFI *returns*; arguments pass through unchanged.
- **Codegen:** a direct Core Erlang tuple `{E1, ..., En}`, not a `Tuple withAll:` send. Elements are threaded as one ordered sequence (`thread_subexprs`, then `close_prelude`), like `#(...)`, so evaluation is left to right even when an element opens a scope. `TupleLiteral` is a literal container for the ADR 0118 sequencing rule.
- **Types:** `{e1, ..., en}` infers as the positional `Tuple(T1, ..., Tn)` of ADR 0075, so `{#a, 1} at: 2` infers `Integer`. `{}` infers as bare `Tuple`.
- **Guards:** a tuple literal inside a `when:` guard stays unsupported, as list literals are. If guards ever need them, list and tuple literals should be added together.
- **Known caveat (not new):** a user tuple whose first element is a runtime tag atom (`#beamtalk_object`, `#beamtalk_supervisor`) is misclassified by `class_of`. `Tuple withAll:` has the same issue. It is documented, not checked.

### Examples

```beamtalk
(Erlang gen_tcp) listen: port with: #(#binary, {#active, false}, {#reuseaddr, true}, {#backlog, 1024})
opts := #({#local, path}, {#nodelay, true},)        // trailing comma OK
pair := {key, dict at: key}                          // keyword-send element
{k, v} := pair                                       // destructure (unchanged)
{a, b} := {b, a}                                     // swap
{a, b} size                                          // => 2 (statement-start expression)
single := {#ok}                                      // 1-tuple
empty := {}                                          // 0-tuple
"got { {a, b} }"                                     // interpolate a tuple: note the spaces
```

### Errors on misuse

```
x := {a. b}
     ^ Expected ',' or '}' in tuple literal, found .
       hint: tuple elements are separated by ',': write `{a, b}`, not Smalltalk's `{a. b}`

x := {1, 2
          ^ Expected '}' to close tuple literal

"{{a, b}}"
 ^ doubled {{ }} in string literal is not interpolation — use \{ and \} for literal braces;
   to interpolate a tuple literal write { {a, b} }
```

## Prior Art

- **Erlang:** `{A, B}` builds a tuple, and the same syntax is the pattern. `{}` and `{X}` are valid. Beamtalk now matches this exactly.
- **Elixir:** `{a, b}` is a tuple literal and pattern. Elixir also has no 1-tuple quirk.
- **Pharo / Squeak Smalltalk:** `{a. b}` is a *dynamic Array*: elements are statements separated by periods, and the result is an `Array`, not a tuple. We keep the braces but not the period separator or the Array semantics; see the alternatives below.
- **Gleam:** tuples are `#(a, b)`. Beamtalk's `#(...)` is already the List literal, so this form was not available.
- **Python:** a 1-tuple needs a trailing comma (`(x,)`) because parentheses also group. Braces have no grouping role in Beamtalk, so `{x}` is unambiguous and needs no comma.

## User Impact

- **Newcomer:** `{a, b}` reads the same as an expression and as a pattern, and the long `Tuple withAll: #(...)` form is no longer the only way to build a tuple. The space needed to interpolate a tuple (`"{ {a, b} }"`) is a small surprise, and the lexer error says how to write it.
- **Smalltalk developer:** in Pharo, braces build an `Array`. Here they build a `Tuple` (`#[...]` builds an `Array`). The `{a. b}` error hint points to the comma form.
- **Erlang/BEAM developer:** the syntax and the value are exactly Erlang's, and FFI option lists read like the Erlang documentation.
- **Tooling:** the formatter normalises trailing commas and keeps the interpolation padding. Hover, references, go-to-definition and signature help descend into tuple elements. `@expect`, lints and the type checker treat a tuple literal like the other collection literals.

## Steelman Analysis

- **For a `#`-prefixed form (`#t(...)`, `#<...>`):** ADR 0012 made `#` the "data literal" prefix, and a prefixed form would need no statement-start lookahead. But `#{` is taken by maps, the pattern is already bare `{a, b}`, and the syntax-rationale doc already promised bare braces. A prefixed expression next to a bare pattern would be asymmetric and gain nothing.
- **For Pharo brace arrays:** Smalltalk developers know `{a. b}`. But `.` is Beamtalk's statement separator, every other Beamtalk collection literal uses `,`, and Pharo's braces build a different class.
- **For desugaring to `Tuple withAll:` in the parser:** it would need no codegen change. But `beamtalk fmt` would rewrite the source into the long form, and every literal would cost a list allocation and a dispatch.
- **For parsing as an expression and converting to a pattern on `:=`** (the way `#[...] :=` works): it avoids a lookahead. But it loses the pattern-only forms (`x :: T`, binaries, nested maps) and would regress existing destructuring.

The main tension is between Smalltalk familiarity (braces as Array) and BEAM fidelity (braces as tuple). Beamtalk already gives Array its own literal, so braces follow Erlang.

## Alternatives Considered

### A `#`-prefixed tuple literal (`#t(...)`, `#<...>`)
Rejected: `#{` is taken by maps, the pattern is bare `{...}`, and the docs already promised bare braces. See the steelman above.

### Smalltalk brace arrays `{a. b}`
Rejected: `.` separates statements, all other collection literals use `,`, and Pharo braces build an Array rather than a tuple.

### Desugar to `Tuple withAll: #(...)` in the parser
Rejected: the formatter would rewrite user source into the long form, and it costs a list allocation and a message send at run time.

### Parse as an expression, then convert to a pattern on `:=`
Rejected: it loses `x :: T`, binary and map sub-patterns in destructuring.

### Disallow `{x}` and `{}`
Rejected: both are valid Erlang terms that FFI needs (`{}` appears in some OTP option APIs), and the pattern grammar already accepts both.

## Consequences

### Positive
- FFI option lists and other tuple-heavy code are much shorter and read like Erlang.
- Tuple patterns and tuple expressions use the same syntax.
- Tuple literals compile to a direct Core Erlang tuple with no allocation or dispatch overhead.
- `Tuple(T1, ..., Tn)` inference gives `at:` with a literal index a precise type.

### Negative
- A leading `{` at statement start needs a bounded lookahead to its matching `}` to choose between destructuring and an expression.
- Interpolating a tuple literal needs a space (`"{ {a, b} }"`), because `"{{"` stays an error.
- Readers coming from Pharo may expect braces to build an Array.

### Neutral
- `Tuple withAll:` remains and still works. Replacing existing uses with the literal is follow-up BT-3973.
- Tuple literals in `when:` guards remain unsupported, as list literals are.

## Implementation

- **AST:** `Expression::TupleLiteral { elements, span }` next to `ArrayLiteral`. Every walker (`ast/visitor.rs`, `ast_walker.rs`, `method_source_walker.rs`) and every semantic-analysis, lint and language-service match that handles `ArrayLiteral` handles `TupleLiteral` the same way, so captures, threading and references see tuple elements.
- **Parser:** a `LeftBrace` arm in `parse_primary` calls `parse_tuple_literal` (comma loop, trailing comma, `{a. b}` hint, recovery to the closing brace). `parse_assignment` takes the destructure path only when `leading_brace_is_destructure()` (in `parser/mod.rs`) sees `:=` after the matching `}`. `parse_tuple_pattern` accepts a trailing comma.
- **Lexer:** the doubled-brace message mentions `{ {a, b} }`. The `{{` behaviour does not change.
- **Unparser:** `unparse_tuple_literal`, and padding in `unparse_string_interpolation` when the interpolated expression starts with `{`. The `unnecessary_parens` lint keeps parentheses around a tuple literal in an interpolation for the same reason.
- **Type checker:** `infer_tuple_literal` returns `Tuple(T1, ..., Tn)`, or bare `Tuple` for `{}`.
- **Codegen:** `generate_tuple_literal` emits `{E1, ..., En}` with `docvec!`, threading elements like `generate_list_literal`. `literal_container_children` lists `TupleLiteral`.
- **Tests:** parser, lexer, unparser fixture, codegen (including an `erlc` compile and the value-position field-assignment corpus), type inference, hover and references, metamorphic rename, parser property fragments, and BUnit `stdlib/test/tuple_test.bt`.

## Migration Path

No migration is needed. Existing `{...} := expr` destructuring parses as before. Source that previously failed with "Tuple literals are not valid expressions" now compiles. Replacing `Tuple withAll: #(...)` with literals in docs, tests and examples is tracked as BT-3973.

## References
- Related issues: BT-3969 (this ADR and implementation), BT-3957 (Catalina stdlib epic), BT-3973 (replace `Tuple withAll:` uses)
- Related ADRs: [ADR 0012](0012-list-literal-syntax.md) § Future Work (superseded for the brace syntax), [ADR 0023](0023-string-interpolation-and-binaries.md) (`{{` rule), [ADR 0075](0075-erlang-ffi-type-definitions.md) (positional `Tuple(T1..Tn)`), [ADR 0076](0076-ok-error-tuple-to-result-at-ffi-boundary.md) (FFI ok/error to Result on returns), [ADR 0089](0089-typed-document-leaves.md) (typed leaves), [ADR 0090](0090-array-canonical-representation.md) (Array is `#[...]`), [ADR 0118](0118-expression-level-state-threading-preludes.md) (literal-container sequencing)
- Documentation: `docs/beamtalk-language-features.md` § Tuple literals, `docs/beamtalk-syntax-rationale.md`
