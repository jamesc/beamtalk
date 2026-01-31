# Beamtalk Syntax Rationale

This document explains the syntax choices for Beamtalk: why we keep certain Smalltalk conventions and why we diverge from others.

See [beamtalk-language-features.md](beamtalk-language-features.md) for the complete syntax specification.

---

## Design Goal

**Keep the soul of Smalltalk, remove the friction.**

Smalltalk's message-passing syntax is its core innovation — code reads like English, with named parameters built into the language. We preserve this. But some historical choices create unnecessary friction for developers in 2026. We fix those.

---

## What We Keep (And Why)

### Keyword Messages

```
array at: 1 put: 'hello'
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
Transcript show: 'Hello'; cr; show: 'World'
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
1. `*`, `/`, `%` (highest - multiplicative)
2. `+`, `-`, `++` (additive and string concatenation)
3. `<`, `>`, `<=`, `>=` (comparison)
4. `=`, `==`, `~=` (equality - strict and loose)

**Note:** `&&`, `||`, `and`, `or` are **not** binary operators - they are keyword messages that take blocks for short-circuit evaluation:
```
result := condition and: [expensiveCheck()]  // block only evaluated if condition is true
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

// Semicolons optional for multiple statements on one line
count := 0; count := count + 1
```

**Why change:**
- Period-as-terminator feels archaic
- Newlines naturally end statements in most modern languages
- Less visual noise

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
self.value += 1     // compound assignment

// Equivalent to message send (still works)
self getValue       // unary message
```

**Why:** Direct field access is clearer for state manipulation. `self.value` is less noisy than `self getValue` for simple reads. Compound assignment (`+=`) eliminates repetition.

**Compilation:** `self.value` compiles to `maps:get('value', State)` — it's a direct lookup, not a message send.

### String Interpolation: Add It

**Smalltalk has no string interpolation.** We add it:

```
name := 'Alice'
greeting := "Hello, {name}!"  // => "Hello, Alice!"

// Double quotes for interpolated strings
// Single quotes for literal strings
literal := 'No {interpolation} here'
```

**Why:** String interpolation is table stakes in 2026. Using `{expr}` inside double-quoted strings is clean and unambiguous.

---

## Complete Syntax Summary

| Feature | Syntax | Example |
|---------|--------|---------|
| Comment (line) | `//` | `// this is a comment` |
| Comment (block) | `/* */` | `/* multi-line */` |
| Assignment | `:=` | `x := 5` |
| Compound assignment | `+=`, `-=`, `*=`, `/=` | `x += 1` |
| Field access | `self.field` | `self.value` |
| Return (early) | `^` | `^self` (only for early returns) |
| Unary message | `receiver message` | `counter increment` |
| Binary message | `a op b` | `3 + 4` |
| Keyword message | `receiver key: arg` | `array at: 1` |
| Cascade | `;` | `obj foo; bar; baz` |
| Block (no args) | `[body]` | `[self doIt]` |
| Block (with args) | `[:args \| body]` | `[:x :y \| x + y]` |
| String (literal) | `'...'` | `'hello'` |
| String (interpolated) | `"..."` | `"Hello, {name}!"` |
| Symbol | `#name` | `#ok`, `#error` |
| Tuple | `{a, b, c}` | `{1, 'two', 3}` |
| List | `[a, b, c]` | `[1, 2, 3]` |
| Statement separator | newline or `;` | implicit |

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

  increment => self.value += 1
  decrement => self.value -= 1
  getValue => self.value                    // implicit return - no ^
  incrementBy: delta => self.value += delta
```

### Async Message Passing

```
// Returns immediately with a future
result := agent analyze: data

// Wait for value
value := result await

// Continuation style
agent analyze: data
  whenResolved: [:value | self process: value]
  whenRejected: [:error | self handle: error]
```

### Pattern Matching

```
// Match on message structure
handle: {#ok, value} => self process: value
handle: {#error, reason} => self logError: reason
handle: _ => self handleUnknown

// Destructuring assignment
{x, y, z} := point coordinates
```

### Supervision

```
Supervisor subclass: WebApp
  children: [
    {DatabasePool, scale: 10},
    HTTPRouter spawn,
    MetricsCollector spawn
  ]
  strategy: #oneForOne
```

### Pipes

```
data
  |> Transform with: options
  |> Filter where: [:x | x > 0]
  |> Sort by: #name
```

---

## Rejected Alternatives

### Ruby-style `object.method(args)`

Considered, but it abandons keyword messages entirely. You get `array.put(1, 'hello')` instead of `array at: 1 put: 'hello'`. The named parameters are the point.

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
