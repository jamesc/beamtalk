# Learning Beamtalk

A progressive language guide with runnable, tested examples.

Every code example in these chapters is verified by `just test-learn` — if an example
is wrong, CI catches it.

## How to use this guide

**Read linearly** — each chapter builds on the last. If you have prior Smalltalk
experience, chapters 1–3 will feel familiar; pay close attention to chapter 4
(Messages) and chapter 5 (Arithmetic) where Beamtalk diverges from classic
Smalltalk.

**Try it yourself** — start the REPL and type along:

```bash
beamtalk repl
```

**Run the examples** — all chapters are executable:

```bash
just test-learn
```

---

## Chapters

| # | Chapter | What you'll learn |
|---|---------|-------------------|
| [01](01-getting-started.md) | Getting Started | REPL, hello world, first expressions |
| [02](02-basic-types.md) | Basic Types | Integer, Float, String, Boolean, Nil, Symbol, Character |
| [03](03-variables.md) | Variables | `:=` assignment, temporary variables, scope |
| [04](04-messages.md) | Messages | **The core concept.** Unary, binary, keyword, cascade, precedence |
| [05](05-arithmetic-and-comparison.md) | Arithmetic & Comparison | Operators, math precedence, equality |
| [06](06-strings.md) | Strings | UTF-8, interpolation, graphemes, common methods |
| [07](07-blocks.md) | Blocks | Closures, arguments, variable capture, `value:` |
| [08](08-control-flow.md) | Control Flow | `ifTrue:`, `whileTrue:`, `timesRepeat:`, `to:do:` |
| [09](09-collections.md) | Collections | Array, Dictionary, `collect:`, `select:`, `inject:into:` |
| [10](10-value-classes.md) | Value Classes | `Value subclass:`, slots, constructors, immutability |
| [11](11-actors.md) | Actors | `Actor subclass:`, `spawn`, state mutation, concurrency |
| [12](12-error-handling.md) | Error Handling | Exceptions, `on:do:`, Result type, `doesNotUnderstand` |
| [13](13-testing.md) | Testing with BUnit | `TestCase`, `assert:equals:`, `setUp`/`tearDown` |
| [14](14-pattern-matching.md) | Pattern Matching | Array destructuring, binary patterns |
| [15](15-type-annotations.md) | Type Annotations | `::` syntax, gradual typing |
| [16](16-beam-interop.md) | BEAM Interop | Erlang FFI, atoms, calling `:module function` |

---

## Running the tests

```bash
just test-learn           # run all learning guide doctests
```


---

## Format conventions

**Expression chapters** (chapters 1–9, 15) — inline assertions:

```beamtalk
3 + 4           // => 7
"hello" size    // => 5
```

**Class chapters** (chapters 10–14, 16) — BUnit assertions:

```beamtalk
TestCase subclass: Ch10Examples
  testPointConstruction =>
    p := Point x: 3 y: 4
    self assert: p x equals: 3
```

Wildcard `_` matches any value (used for setup expressions):

```beamtalk
counter := Counter spawn  // => _
```

Error assertions:

```beamtalk
42 unknownMessage         // => ERROR:does_not_understand
```

---

## See also

- [`docs/beamtalk-language-features.md`](../beamtalk-language-features.md) — full language reference
- [`docs/beamtalk-syntax-rationale.md`](../beamtalk-syntax-rationale.md) — why the syntax is the way it is
- [`stdlib/test/`](../../stdlib/test/) — the full test suite
