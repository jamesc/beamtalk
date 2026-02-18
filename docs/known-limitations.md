# Known Limitations (v0.1)

Beamtalk is under active development. This page documents features that are **not yet supported** along with workarounds where available.

## Language Features

### No Module or Import System

All classes live in a flat global namespace. There is no `import`, `export`, or namespace syntax. Name collisions between user code and stdlib are possible.

**Workaround:** Use distinctive class names to avoid collisions with stdlib classes.

**Tracking:** [BT-714](https://linear.app/beamtalk/issue/BT-714)

### No Pipe Operators

The `|>` and `|>>` pipe operators are not implemented. Use message chaining or intermediate variables instead.

**Workaround:**
```beamtalk
// Instead of: list |> select: [:x | x > 0] |> collect: [:x | x * 2]
// Use chaining:
(list select: [:x | x > 0]) collect: [:x | x * 2]
```

**Tracking:** [BT-707](https://linear.app/beamtalk/issue/BT-707)

### No `try`/`catch` Keywords

Exception handling uses block-based syntax only. There are no `try`, `catch`, or `throw` keywords.

**Workaround:**
```beamtalk
[riskyOperation] on: Error do: [:e | e message]
[riskyOperation] ensure: [cleanup]
```

### No Destructuring Assignment

You cannot destructure tuples or collections in assignment. Use pattern matching in `match:` expressions instead.

**Workaround:**
```beamtalk
result := getCoordinates
x := result at: 1
y := result at: 2
```

### No Macros or Compile-Time Metaprogramming

There is no macro system. All code runs at runtime.

### No `become:` or `thisContext`

These Smalltalk reflection features cannot be implemented on the BEAM VM:
- **`become:`** — BEAM pids are immutable; object identity cannot be swapped. A proxy pattern is planned as a workaround.
- **`thisContext`** — BEAM does not expose stack frames as first-class objects.

### Classes Are Not Yet First-Class Actor Objects

Classes compile to Erlang modules. They are not yet actor objects that you can pass around, subclass dynamically, or introspect via the metaclass protocol. Reflection (`class`, `respondsTo:`, `instVarNames`) works, but classes don't participate in the actor model.

## Concurrency

### No Async Combinators

`Future all:` and `Future any:` are not yet implemented. You cannot wait on multiple futures simultaneously.

**Workaround:** Await futures sequentially.

**Tracking:** [BT-507](https://linear.app/beamtalk/issue/BT-507)

### No Supervision DSL

Actors are supervised internally via OTP, but there is no user-facing syntax for defining supervision trees or restart strategies.

**Workaround:** Actors automatically restart on crash via OTP defaults. For custom supervision, use `Erlang` FFI to call OTP supervisor APIs directly.

**Tracking:** [BT-448](https://linear.app/beamtalk/issue/BT-448)

## Type System

### Types Are Optional Warnings Only

Gradual typing (ADR 0025) is in Phase 1. Type annotations are parsed and checked, but:
- Type errors produce **warnings**, never compile errors
- Union and generic type annotations are not yet checked
- Dialyzer spec generation is partial

This is by design — types are documentation-first in Beamtalk.

## Standard Library Gaps

The following common utilities are not yet in the stdlib. All have clean workarounds via the `Erlang` FFI:

| Missing | Erlang FFI Workaround | Tracking |
|---------|----------------------|----------|
| Regular expressions | `Erlang re run: 'hello123' with: '[0-9]+'` | [BT-709](https://linear.app/beamtalk/issue/BT-709) |
| Date/Time | `Erlang os system_time: #second` | [BT-710](https://linear.app/beamtalk/issue/BT-710) |
| JSON | `Erlang json encode: #{#name => 'Ada'}` (OTP 27+) | [BT-711](https://linear.app/beamtalk/issue/BT-711) |
| Math functions | `Erlang math sqrt: 16` | [BT-712](https://linear.app/beamtalk/issue/BT-712) |
| Environment variables | `Erlang os getenv: 'HOME'` | [BT-713](https://linear.app/beamtalk/issue/BT-713) |
| HTTP client | `Erlang httpc` | — |

## Pattern Matching

Binary pattern matching is not yet implemented.

**Tracking:** [BT-663](https://linear.app/beamtalk/issue/BT-663)
