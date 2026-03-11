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

### Part 1 — The Basics

The first five chapters cover the fundamentals you need before writing any real code:
types, variables, and the message-passing model that underlies everything in Beamtalk.

| # | Chapter | What you'll learn |
|---|---------|-------------------|
| [01](01-getting-started.md) | Getting Started | Fire up the REPL, send your first message, understand how expressions work |
| [02](02-basic-types.md) | Basic Types | Every value is an object: Integer, Float, String, Boolean, Nil, Symbol, Character |
| [03](03-variables.md) | Variables | `:=` assignment, temporary variables, lexical scope |
| [04](04-messages.md) | Messages | **The core concept.** Unary, binary, and keyword messages; cascade; precedence rules |
| [05](05-arithmetic-and-comparison.md) | Arithmetic & Comparison | Numeric operators, division, equality (`==`, `=:=`, `/=`), ordering |

### Part 2 — Working with Data

With the basics in hand, these chapters cover the types and structures you'll use most in everyday code.

| # | Chapter | What you'll learn |
|---|---------|-------------------|
| [06](06-strings.md) | Strings | UTF-8 text, `++` concatenation, `{interpolation}`, grapheme clusters, common methods |
| [07](07-blocks.md) | Blocks | Closures as first-class values; `value:`, `valueWithArguments:`; variable capture |
| [08](08-control-flow.md) | Control Flow | `ifTrue:ifFalse:`, `whileTrue:`, `timesRepeat:`, `to:do:`, `match:` |
| [09](09-collections.md) | Collections | Array, Dictionary, Bag, Set; `collect:`, `select:`, `inject:into:` |

### Part 3 — Object-Oriented Beamtalk

Classes, concurrency, and error handling — the three pillars of production Beamtalk code.

| # | Chapter | What you'll learn |
|---|---------|-------------------|
| [10](10-value-classes.md) | Value Classes | Immutable data objects with `Value subclass:`; slots, constructors, equality |
| [11](11-actors.md) | Actors | Concurrent objects with `Actor subclass:`; `spawn`, `send:`, mutable state |
| [12](12-error-handling.md) | Error Handling | `on:do:` exceptions, the `Result` type, `doesNotUnderstand` |
| [13](13-testing.md) | Testing with BUnit | `TestCase subclass:`, `assert:equals:`, `should:raise:`, `setUp`/`tearDown` |

### Part 4 — Advanced Features

Pattern matching, the type system, and BEAM interoperability for when you need the full power of the platform.

| # | Chapter | What you'll learn |
|---|---------|-------------------|
| [14](14-pattern-matching.md) | Pattern Matching | Destructuring arrays, maps, and tuples; binary patterns with `match:` |
| [15](15-type-annotations.md) | Type Annotations | The `::` syntax, gradual typing, method signatures |
| [16](16-beam-interop.md) | BEAM Interop | Calling Erlang from Beamtalk, atoms, tuples, `Erlang module function:` |
| [17](17-otp-supervisors.md) | OTP Supervisors | `Supervisor subclass:`, restart strategies, fault-tolerant trees |

### Part 5 — Practical Stdlib

The standard library APIs you'll reach for most often: file handling, text processing, and data interchange.

| # | Chapter | What you'll learn |
|---|---------|-------------------|
| [18](18-file-io.md) | File I/O | Reading, writing, directory operations, lazy file streams |
| [19](19-regex.md) | Regular Expressions | Pattern matching, find/replace, splitting, compiled regex |
| [20](20-json.md) | JSON | Parsing, generation, nested structures, round-tripping |
| [21](21-datetime.md) | DateTime & Time | Construction, formatting, arithmetic, comparisons, high-resolution timestamps |

### Part 6 — The Environment

The workspace globals you'll use in every REPL session.

| # | Chapter | What you'll learn |
|---|---------|-------------------|
| [22](22-workspace-globals.md) | Workspace & Globals | Transcript logging, Workspace introspection, Beamtalk reflection, custom bindings |

### Part 7 — Advanced Topics

Lazy evaluation, metaprogramming, and real-world networking.

| # | Chapter | What you'll learn |
|---|---------|-------------------|
| [23](23-streams.md) | Streams | Lazy evaluation, infinite sequences, stream pipelines, collection interop |
| [24](24-reflection.md) | Reflection & Metaprogramming | `respondsTo:`, `fieldAt:`, `perform:`, metaclasses, class hierarchy |
| [25](25-http.md) | HTTP Client | GET/POST/PUT/DELETE, response parsing, JSON APIs, configurable client actor |

---

## Running the tests

```bash
just test-learn           # run all learning guide doctests
```


---

## Format conventions

**Expression chapters** (chapters 1–9, 15, 19–21, 23) — inline assertions:

```beamtalk
3 + 4           // => 7
"hello" size    // => 5
```

**Class chapters** (chapters 10–14, 16, 18, 24) — BUnit assertions:

```beamtalk
TestCase subclass: Ch10Examples
  testPointConstruction =>
    p := Point x: 3 y: 4
    self assert: p x equals: 3
```

Wildcard `_` matches any value (used for setup expressions):

```
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
