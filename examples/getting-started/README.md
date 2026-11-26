# Getting Started — Beamtalk Examples Workspace

Introductory Beamtalk examples covering the core language concepts.

## Starting the REPL

```bash
cd examples/getting-started
beamtalk repl
```

All source files in `src/` load automatically when you start the REPL.

## Examples

### hello.bt — Minimal class

```text
> Hello new
```

### counter.bt — Stateful actor

```text
> c := Counter spawn
> c increment
> c increment
> c getValue
2
```

### hanoi.bt — Recursion and multi-keyword messages

```text
> h := Hanoi new
> h solve: 3 from: 'A' to: 'C' via: 'B'
```

### logging_counter.bt — Inheritance and `super`

```text
> lc := LoggingCounter spawn
> lc increment
> lc getValue
```

### shipping.bt — Pattern matching on tagged data

Tuple and guard patterns, constructor patterns (`Result ok: v`), destructuring
assignment (`{total, rejected} := acc`) and `matchExhaustive:` over a closed
symbol type (`ShippingSpeed`).

```text
> Shipping quote: {#parcel, #express, 2500}
Result ok: 1500
> Shipping describe: (Shipping quote: {#parcel, #standard, 0})
"rejected: weight must be positive"
```

### report.bt — Bounded type parameter

`Report(T :: Printable)` accepts only elements that answer `asString`.

## Running Tests

```text
> :test
```

## What's Next?

- **[bank](../bank/)** — Actor coordination: bank accounts and transfers
- **[chat-room](../chat-room/)** — Multi-actor: actors, inheritance, collections
- **[Language reference](../../docs/beamtalk-language-features.md)**
- **[REPL tutorial](../repl-tutorial.md)**
