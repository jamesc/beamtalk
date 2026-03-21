# Beamtalk Syntax Rationale

**Status:** Complete — Syntax design decisions finalized. Updated 2026-03-20 to reflect ADR 0041 (universal block mutations), ADR 0036 (metaclass tower), ADR 0059 (supervision), and REPL inline class support.

> **tl;dr** — Beamtalk is Smalltalk-**like**, not Smalltalk-**compatible**. We use Smalltalk's message-passing syntax with modern pragmatic improvements: `//` comments, standard math precedence, optional statement terminators, field access, and string interpolation. This makes Beamtalk familiar to Smalltalkers while removing friction for modern developers.

This document explains the syntax choices for Beamtalk: why we keep certain Smalltalk conventions and why we diverge from others. For the formal architectural decision record with steelman analysis and consequences, see [ADR 0039](ADR/0039-syntax-pragmatism-vs-smalltalk.md).

See [beamtalk-language-features.md](beamtalk-language-features.md) for the complete syntax specification.

---

## Design Goal

**Keep the soul of Smalltalk, remove the friction.**

Smalltalk's message-passing syntax is its core innovation — code reads like English, with named parameters built into the language. We preserve this. But some historical choices create unnecessary friction for developers in 2026. We fix those.

**Result:** Beamtalk is Smalltalk-**inspired**, not Smalltalk-**compatible**. If you need full Smalltalk-80 semantics, use Pharo or Squeak. If you want Smalltalk's elegance on BEAM with modern ergonomics, use Beamtalk.

---

## What We Keep (And Why)

### Keyword Messages

```
array at: 1 put: "hello"
agent query: question context: history
```

**Why keep:** This IS the feature. Keyword messages are:
- Self-documenting: parameter names are part of the syntax
- Readable: flows like natural language
- Unique: no other mainstream language has this

Throwing this away gives you "another Ruby" with no differentiation.

### Blocks (Closures)

```
[:x :y | x + y]
[self doSomething]
[:item | item > 0]
```

**Why keep:** Concise, elegant, instantly recognizable. The `[:args | body]` syntax is beloved by Smalltalkers and compact enough to use inline.

### Cascades

```
Transcript show: "Hello"; cr; show: "World"
builder add: item1; add: item2; add: item3; build
```

**Why keep:** Powerful for builder patterns and fluent APIs. Send multiple messages to the same receiver without repeating it. Worth learning.

### Assignment with `:=`

```
count := 0
result := self compute
```

**Why keep:** Clearly distinguishes assignment from equality (`=`). Avoids the `=` vs `==` confusion from C-family languages.

### Unary, Binary, Keyword Message Categories

```
object message           // unary
3 + 4                    // binary
array at: 1              // keyword
```

**Why keep:** Clean mental model. Message complexity is visible in the syntax.

---

## What We Change (And Why)

### Comments: `"..."` → `//` and `/* */`

**Before (Smalltalk):**
```smalltalk
"This is a comment"
count := 0.  "inline comment"
```

**After (Beamtalk):**
```
// This is a comment
count := 0  // inline comment

/*
  Multi-line comment
  for longer explanations
*/
```

**Why change:**
- `"comment"` is unique to Smalltalk; every other language uses `//` or `#`
- Strings also use quotes, creating visual confusion
- `//` is universal and muscle memory for most developers

### Math Precedence: Left-to-Right → Standard (PEMDAS)

**Before (Smalltalk):**
```smalltalk
2 + 3 * 4.   "=> 20 (evaluated left-to-right)"
```

**After (Beamtalk):**
```
2 + 3 * 4    // => 14 (standard math precedence)
(2 + 3) * 4  // => 20 (explicit grouping)
```

**Why change:**
- Left-to-right precedence is a bug factory
- Every developer expects `*` to bind tighter than `+`
- Requiring parentheses for basic math is hostile

**Implementation:** Binary operators have precedence levels:
1. `**` (highest - exponentiation, right-associative)
2. `*`, `/`, `%` (multiplicative)
3. `+`, `-`, `++` (additive and string concatenation)
4. `<`, `>`, `<=`, `>=` (comparison)
5. `=:=`, `==`, `/=`, `=/=` (equality - strict and loose)

**Note:** `&&`, `||`, `and`, `or` are **not** binary operators - they are keyword messages that take blocks for short-circuit evaluation:
```
result := condition and: [self expensiveCheck]  // block only evaluated if condition is true
```

### Statement Terminator: Required `.` → Optional (Newlines Work)

**Before (Smalltalk):**
```smalltalk
count := 0.
count := count + 1.
self doSomething.
```

**After (Beamtalk):**
```
count := 0
count := count + 1
self doSomething

// Periods still work as explicit separators (useful inside blocks)
[count := 0. count := count + 1]
```

**Why change:**
- Period-as-terminator feels archaic
- Newlines naturally end statements in most modern languages
- Less visual noise
- Periods remain available as explicit separators where newlines are impractical (e.g., single-line blocks)

**Note:** Semicolons (`;`) are **not** statement separators — they are cascade operators (see [Cascades](#cascades) above).

### Return: `^value` → `^value` (Keep, But Implicit at End)

```
// Implicit return of last expression (preferred)
getValue => self.value

// Explicit return ONLY for early returns
max: other =>
  self > other ifTrue: [^self]   // early return - use ^
  other                           // last expression - implicit return

// Blocks also use implicit return
[:x | x * 2]  // returns x * 2
```

**Why:** `^` is distinctive and clear. But requiring it for every expression is noisy. Last expression in a method or block is implicitly returned.

**Style Rule:** Use `^` ONLY for early returns (returning before the final expression). Never use `^` on the last line of a method or block.

### Field Access: Add Dot Notation

**Smalltalk has no field access syntax** — all access is via messages. We add direct field access:

```
// Direct field access within actor
self.value          // read field
self.value := 10    // write field
self.value := self.value + 1  // explicit assignment

// Equivalent to message send (still works)
self getValue       // unary message
```

**Why:** Direct field access is clearer for state manipulation. `self.value` is less noisy than `self getValue` for simple reads.

**Compilation:** `self.value` compiles to `maps:get('value', State)` — it's a direct lookup, not a message send.

### String Interpolation: Add It

**Smalltalk has no string interpolation.** We add it:

```
name := "Alice"
greeting := "Hello, {name}!"  // => "Hello, Alice!"
```

**Why:** String interpolation is table stakes in 2026. Using `{expr}` inside double-quoted strings is clean and unambiguous. All strings use double quotes (ADR 0023). Single quotes are reserved for `#'quoted symbols'` only.

### Equality Semantics: Identity → Structural (Hybrid)

**Smalltalk's equality:**
- `==` tests identity (pointer equality)
- `=` tests value equality (overrideable)

**Beamtalk's equality ([ADR 0002](ADR/0002-use-erlang-comparison-operators.md)):**
- `==` tests structural equality (value-based with type coercion)
- `=:=` tests strict equality (no type coercion)
- `/=` tests loose inequality (negation of `==`)
- `=/=` tests strict inequality (negation of `=:=`)

```beamtalk
// Primitives with type coercion
1.0 == 1           // => true (coercion allowed)
1.0 =:= 1          // => false (strict, no coercion)

// Value types (compare map contents)
p1 := Point new: #{x => 3, y => 4}
p2 := Point new: #{x => 3, y => 4}
p1 == p2           // => true (same field values)

// Actors (compare pids - effectively identity)
c1 := Counter spawn
c2 := Counter spawn
c1 == c2           // => false (different processes)
```

**Why we differ from Smalltalk:**

1. **Erlang semantics are already correct** — `==` does the right thing for all types:
   - Primitives: value equality with sensible coercion
   - Maps (value types): structural equality
   - Pids (actors): identity comparison
   
2. **Value types need value semantics** — Two `Point` objects with the same coordinates should be equal. Smalltalk's identity-based `==` would make them unequal unless they're the same object reference.

3. **Actors automatically get identity** — Process pids are compared by identity in Erlang, which is the correct behavior for actors.

4. **Simplifies the mental model** — "Structural equality" is more intuitive than "identity for most things, but value equality for immutables."

**BEAM mapping:**
- `==` → Erlang's `==` (value equality with coercion)
- `=:=` → Erlang's `=:=` (strict equality, no coercion)
- `/=` → Erlang's `/=` (loose inequality, negation of `==`)
- `=/=` → Erlang's `=/=` (strict inequality, negation of `=:=`)

**Decision rationale:** This hybrid approach emerged from the value types design — we discovered that Erlang's `==` already provides the correct semantics for both value types and actors without any special handling.

### Control Flow Mutations: Make Them Work

**BEAM constraint:** Smalltalk-80 blocks capture variables by reference (shared mutable cells), so `whileTrue:` with mutations works naturally in Pharo and Squeak. But BEAM enforces single-assignment variables — there are no heap-allocated mutable cells. Without compiler support, mutations inside blocks would not propagate to the outer scope.

**Beamtalk solution:** The compiler detects literal blocks in control-flow positions and generates tail-recursive loops with explicit state threading, restoring the Smalltalk mutation semantics that developers expect:

```
// ✅ Works!
count := 0.
[count < 10] whileTrue: [count := count + 1].
// count is now 10

// ✅ Field mutations work too
[self.value < 10] whileTrue: [
    self.value := self.value + 1
].
```

**Why change:**

1. **Smalltalk idioms require it** — `whileTrue:`, `timesRepeat:`, `do:` are central to Smalltalk style
2. **BEAM enables it** — We compile to tail-recursive loops with state threading
3. **Universal composability** — User-defined higher-order methods work with mutating blocks, not just stdlib control flow
4. **Better than alternatives:**
   - **C-style loops** (`for`, `while`) lose message-passing elegance
   - **Immutable-only** makes simple counters painful
   - **Mutable-everywhere** loses reasoning guarantees

**How it works ([ADR 0041](ADR/0041-universal-state-threading-block-protocol.md)):**

The compiler uses a two-tier optimization:

- **Tier 1 (stdlib control flow):** `whileTrue:`, `do:`, `collect:`, `timesRepeat:`, etc. — the compiler generates inlined tail-recursive loops with versioned state variables. Zero overhead.
- **Tier 2 (user-defined HOMs):** All other methods that accept blocks — the compiler uses a universal `{Result, StateAcc}` protocol. Blocks that actually mutate pay ~65ns overhead per invocation; pure blocks (no mutations) compile to plain funs with no overhead.

```
// ✅ Stdlib control flow (Tier 1 — zero overhead)
[count < 10] whileTrue: [count := count + 1]

// ✅ User-defined higher-order methods also work (Tier 2)
items myCustomLoop: [:x | count := count + x]  // mutations propagate
```

This gives us Smalltalk's elegant control flow WITH mutations that actually work — universally, not just for a hardcoded set of selectors.

See [beamtalk-language-features.md](beamtalk-language-features.md#control-flow-and-mutations-bt-90) for full specification.

### Class Definition: Message Send → Syntax

**Before (Smalltalk):**
```smalltalk
Object subclass: #Counter
    instanceVariableNames: 'value'
    classVariableNames: ''
    package: 'MyApp'
```

In Smalltalk, `Object subclass: #Counter` is a **real message send** to the `Object` class object. The class is created dynamically at runtime. Classes are first-class objects with a full metaclass protocol.

**After (Beamtalk):**
```
Object subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1
```

In Beamtalk, `Object subclass: Counter` is **parsed as syntax**, not a message send. The parser recognizes this pattern and produces a `ClassDefinition` AST node. The class is compiled to a BEAM module.

**Why syntax, not a message send:**
- Compile-time class definitions enable static analysis, better error messages, and IDE support
- The `subclass:` syntax is familiar to Smalltalk developers
- The compiler generates BEAM modules from class definitions — the parser needs to know the full class structure upfront

**But the dynamic semantics work too:**

Despite being parsed as syntax, the compiler and runtime support dynamic class creation and modification:

- **REPL inline classes** — typing `Actor subclass: Counter ...` at the REPL compiles and hot-loads the class immediately. Redefining it replaces the live class; existing actors pick up new code on next dispatch.
- **Hot code reload** — `Counter reload` or `:reload Counter` recompiles from source and hot-swaps the module.
- **Live patching** — `Counter >> increment => ...` replaces individual methods without redefining the whole class.
- **Full metaclass tower** — `Counter class` returns a metaclass object (ADR 0036). Classes are first-class objects with `methods`, `superclass`, `allMethods`, and `respondsTo:`.

The design is: **syntax for ergonomics, dynamic semantics for liveness.** The parser treats `subclass:` as syntax to enable static analysis and fast compilation, but the runtime supports the dynamic class creation and modification that Smalltalk developers expect.

---

## Complete Syntax Summary

| Feature | Syntax | Example |
|---------|--------|---------|
| Comment (line) | `//` | `// this is a comment` |
| Comment (block) | `/* */` | `/* multi-line */` |
| Assignment | `:=` | `x := 5` |
| Field access | `self.field` | `self.value` |
| Return (early) | `^` | `^self` (only for early returns) |
| Unary message | `receiver message` | `counter increment` |
| Binary message | `a op b` | `3 + 4` |
| Keyword message | `receiver key: arg` | `array at: 1` |
| Cascade | `;` | `obj foo; bar; baz` |
| Block (no args) | `[body]` | `[self doIt]` |
| Block (with args) | `[:args \| body]` | `[:x :y \| x + y]` |
| String | `"..."` | `"hello"`, `"Hello, {name}!"` |
| Symbol | `#name` | `#ok`, `#error` |
| Tuple | `{a, b, c}` | `{1, "two", 3}` |
| List | `#(a, b, c)` | `#(1, 2, 3)` |
| Statement separator | newline or `.` | implicit |

---

## Message Precedence

**High to low:**

1. **Unary:** `3 factorial` → `(3 factorial)`
2. **Binary:** `3 + 4 * 2` → `3 + (4 * 2)` (with math precedence)
3. **Keyword:** `array at: 1 + 2` → `array at: (1 + 2)`

Within binary operators, standard math precedence applies.

Use parentheses to override: `(2 + 3) * 4`

---

## Comparison with Smalltalk

| Aspect | Smalltalk-80 | Beamtalk |
|--------|--------------|----------|
| Comments | `"comment"` | `// comment` |
| Math precedence | Left-to-right | Standard PEMDAS |
| Statement terminator | Required `.` | Optional (newline) |
| String interpolation | None | `"Hello, {name}!"` |
| Class definition | `Object subclass: #Counter` (message send) | `Object subclass: Counter` (syntax) |
| Keyword messages | ✅ Same | ✅ Same |
| Blocks | ✅ Same | ✅ Same |
| Cascades | ✅ Same | ✅ Same |
| Assignment | ✅ Same (`:=`) | ✅ Same |
| Return | ✅ Same (`^`) | ✅ Same |

---

## Examples in New Syntax

### Actor Definition

```
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1
  getValue => self.value                    // implicit return - no ^
  incrementBy: delta => self.value := self.value + delta
```

### Sync and Async Message Passing

```
// Synchronous (default) — blocks until reply
result := agent analyze: data

// Asynchronous (fire-and-forget) — returns nil immediately
agent logEvent: data!       // ! sends via gen_server:cast
```

Synchronous sends (`.`) use `gen_server:call`; async sends (`!`) use `gen_server:cast`. See [beamtalk-language-features.md](beamtalk-language-features.md#actor-message-passing) for full messaging semantics.

### Pattern Matching

```
// match: expression (implemented)
status match: [#ok -> "success"; #error -> "failure"; _ -> "unknown"]
42 match: [n -> n + 1]  // => 43
```

Destructuring assignment is also supported: `{x, y} := expr` for tuples, `#[a, ...rest] := expr` for arrays. See [beamtalk-language-features.md](beamtalk-language-features.md#pattern-matching) for the full pattern matching specification.

### Supervision Trees

Beamtalk provides declarative OTP supervision via `Supervisor subclass:` and `DynamicSupervisor subclass:` (ADR 0059). See [beamtalk-language-features.md](beamtalk-language-features.md#supervision-trees-adr-0059) for the full specification.

```
Supervisor subclass: WebApp
  class children => #(Database, HttpServer, WorkerPool)
```

---

## Rejected Alternatives

### Ruby-style `object.method(args)`

Considered, but it abandons keyword messages entirely. You get `array.put(1, "hello")` instead of `array at: 1 put: "hello"`. The named parameters are the point.

### Python-style Significant Whitespace

Rejected. Indentation-sensitivity is polarizing and complicates the parser for minimal benefit.

### Elixir-style `def function(args), do: body`

Rejected. Functional style fights the "actors are objects" mental model we want.

### Lisp-style S-expressions

Rejected. LFE already exists for BEAM. We're explicitly targeting the Smalltalk aesthetic.

---

## References

- [Smalltalk-80 Blue Book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf) — Original syntax
- [Newspeak Language](https://newspeaklanguage.org/) — Modern Smalltalk evolution
- [Ruby](https://ruby-lang.org) — Smalltalk aesthetics in production
