# Design: Tooling and IDE Experience

**Issue:** Future (TBD)  
**Status:** Early Thoughts — captures concerns from design review  
**Depends on:** BT-151 (Self-as-Object), metaprogramming design  
**Date:** 2026-02-01

## Executive Summary

This document captures tooling and IDE concerns raised during the BT-151 design review (channeling Anders Hejlsberg's perspective). Beamtalk's dynamic nature creates challenges for static tooling that TypeScript solved for JavaScript.

**Key question:** How do we make a dynamic, Smalltalk-inspired language *toolable*?

---

## Part 1: The Challenge

### Anders' Critique

> "You've designed a runtime, not a language experience. When I type `counter.` in my editor, what happens?"

**The problems:**

1. **Autocomplete:** What methods are available on `counter`?
2. **Dynamic methods:** Extensions and DNU can add methods at runtime
3. **Type inference:** What is `counter`? What can I send to it?
4. **Error detection:** Can we catch `counter foo` (unknown method) before runtime?

### Why TypeScript Won

TypeScript succeeded by making JavaScript *toolable*:
- Type annotations (optional but helpful)
- `.d.ts` declaration files for untyped libraries
- Language server with rich IDE integration
- Errors at compile time, not runtime

### Beamtalk's Dynamic Features

| Feature | Tooling Challenge |
|---------|-------------------|
| Duck typing | Can't know methods without type info |
| `doesNotUnderstand:` | Any message might be handled |
| Extension registry | Methods added at runtime |
| Hot code reload | Methods can change while running |
| Primitives | `42 foo` — does Integer have `foo`? |

---

## Part 2: Potential Approaches

### Approach 1: Gradual Typing (TypeScript-style)

Add optional type annotations:

```
// Untyped (works today)
increment =>
    self.value := self.value + 1

// Typed (optional)
increment -> Integer =>
    self.value := self.value + 1
    
// Parameter types
add: (x: Integer) -> Integer =>
    self.value + x
    
// Variable types
counter: Counter := Counter spawn
```

**Benefits:**
- IDE knows types when annotations present
- Can infer types from annotations
- Errors caught at compile time
- Gradual adoption (like TypeScript's `any`)

**Challenges:**
- Design type system that works with Smalltalk semantics
- Handle dynamic features (`doesNotUnderstand:`, extensions)
- Erlang interop typing

### Approach 2: Type Inference (Hindley-Milner style)

Infer types without annotations:

```
// No annotations needed
increment =>
    self.value := self.value + 1  // Infer: value is numeric
    
add: x =>
    self.value + x                 // Infer: x must support +
```

**Benefits:**
- No annotation burden
- Works with existing code

**Challenges:**
- Smalltalk's dynamic dispatch is hard to infer
- Message sends to unknown receivers
- Inference may be imprecise

### Approach 3: Flow Analysis (like Flow for JS)

Track types through control flow:

```
maybeCounter := something getValue

maybeCounter ifNotNil: [:c |
    // Inside block: c is known non-nil
    c increment   // IDE knows c has Counter methods
]
```

**Benefits:**
- Works with dynamic patterns
- No annotations for common cases

**Challenges:**
- Complex implementation
- May not scale to all dynamic patterns

### Approach 4: Declaration Files (like .d.ts)

Describe types separately from implementation:

```
// counter.bt.d (declaration file)
class Counter
    state: value: Integer
    
    spawn -> Counter
    increment -> Integer
    getValue -> Integer
```

**Benefits:**
- Don't modify existing code
- Can describe Erlang libraries
- IDE reads declarations for completion

**Challenges:**
- Declarations can drift from implementation
- Extra maintenance burden

### Approach 5: Smalltalk-style (Image-based)

The IDE *is* the environment — it knows everything because it runs everything:

```
// In Smalltalk:
// - IDE has live objects
// - Can query any object for its methods
// - Completion comes from runtime introspection
```

**Benefits:**
- Perfect accuracy (asking the actual objects)
- Works with all dynamic features
- True to Smalltalk philosophy

**Challenges:**
- Requires live environment
- Can't analyze files in isolation
- Different development model

---

## Part 3: Hybrid Proposal

### Core Idea: Static + Dynamic

1. **Static layer:** Infer/declare what we can at compile time
2. **Dynamic layer:** Query live environment for runtime additions
3. **Graceful degradation:** When static analysis fails, fall back to "any"

### Type Annotations (Optional)

```
// Level 0: No types (fully dynamic)
add: x => self.value + x

// Level 1: Return type only
add: x -> Integer => self.value + x

// Level 2: Full signature
add: (x: Integer) -> Integer => self.value + x

// Level 3: Generic
map: (block: Block[T, U]) -> Array[U] => ...
```

### IDE Features by Type Level

| Feature | No Types | Return Type | Full Types |
|---------|----------|-------------|------------|
| Syntax highlighting | ✅ | ✅ | ✅ |
| Basic autocomplete | ⚠️ (methods from class) | ✅ | ✅ |
| Parameter hints | ❌ | ⚠️ | ✅ |
| Type errors | ❌ | ⚠️ | ✅ |
| Refactoring | ⚠️ | ✅ | ✅ |

### Extension Registry Integration

```
// Extensions are registered with type info
beamtalk_extensions:register('String', 'json', Fun, #{
    owner => mylib,
    signature => "-> String",  // For tooling
    doc => "Convert to JSON string"
})
```

IDE queries extension registry for additional methods.

### Live Environment Connection

```
// Language server can connect to running BEAM
// For completion on unknown types, query live system:

LSP -> BEAM: "What methods does this pid respond to?"
BEAM -> LSP: [increment, getValue, ...]
```

---

## Part 4: Language Server Architecture

### Components

```
┌─────────────────────────────────────────────────────┐
│                    IDE / Editor                      │
└─────────────────┬───────────────────────────────────┘
                  │ LSP Protocol
┌─────────────────▼───────────────────────────────────┐
│              Beamtalk Language Server                │
│  ┌─────────────┐ ┌─────────────┐ ┌───────────────┐  │
│  │   Parser    │ │Type Checker │ │ Live Connector│  │
│  └─────────────┘ └─────────────┘ └───────────────┘  │
│  ┌─────────────┐ ┌─────────────┐ ┌───────────────┐  │
│  │Symbol Index │ │ Extension   │ │  Diagnostics  │  │
│  │             │ │  Registry   │ │               │  │
│  └─────────────┘ └─────────────┘ └───────────────┘  │
└─────────────────┬───────────────────────────────────┘
                  │ (optional)
┌─────────────────▼───────────────────────────────────┐
│              Running BEAM Node                       │
│         (for live introspection)                     │
└─────────────────────────────────────────────────────┘
```

### LSP Features

| Feature | Priority | Static/Dynamic |
|---------|----------|----------------|
| Syntax errors | P0 | Static |
| Go to definition | P0 | Static |
| Find references | P0 | Static |
| Autocomplete (known types) | P0 | Static |
| Autocomplete (unknown types) | P1 | Dynamic |
| Hover (type info) | P1 | Static |
| Hover (runtime value) | P2 | Dynamic |
| Rename symbol | P1 | Static |
| Type errors | P1 | Static |
| Inline hints | P2 | Static |

### Incremental Parsing

For responsive IDE, need incremental parsing:
- Re-parse only changed region
- Update symbol index incrementally
- Cache type inference results

---

## Part 5: Error Experience

### Compile-Time Errors (Static)

```
// Error: Unknown method
counter foo
       ^^^ Counter does not respond to 'foo'
           Did you mean: 'getValue'?
           
// Error: Type mismatch (if typed)
counter add: "hello"
             ^^^^^^^ Expected Integer, got String
             
// Warning: Unused variable
x := 42
^   'x' is never used
```

### Runtime Errors (Enhanced)

```
// Current: cryptic
{does_not_understand, 'Counter', foo, 0}

// Goal: helpful
BeamtalkError: Counter does not understand 'foo'
  
  counter foo
          ^^^ 
          
  Similar methods on Counter:
    - getValue
    - setValue:
    
  At: examples/test.bt:15:10
  Stack:
    - Counter.someMethod (counter.bt:42)
    - Main.run (main.bt:10)
```

### IDE Integration

- Errors appear inline as you type
- Quick fixes: "Did you mean...?" → one-click fix
- Type errors have explanations linking to docs

---

## Part 6: Autocomplete Deep Dive

### Scenario 1: Known Type

```
counter: Counter := Counter spawn
counter |  // Cursor here
```

**What we know:**
- `counter` has type `Counter`
- `Counter` class has methods: `increment`, `getValue`, `setValue:`

**Completion list:**
```
increment       -> Integer    Increment the counter
getValue        -> Integer    Get current value
setValue:       (Integer)     Set the value
class           -> Symbol     [inherited from Object]
respondsTo:     (Symbol) -> Boolean
...
```

### Scenario 2: Unknown Type

```
result := someService fetch: url
result |  // Cursor here — what is result?
```

**Options:**
1. Show nothing (unhelpful)
2. Show common methods (class, respondsTo:)
3. Query live environment if connected
4. Infer from usage elsewhere in file

### Scenario 3: Extensions

```
"hello" |  // Cursor here
```

**What we know:**
- `"hello"` is a String
- String has built-in methods
- Extension registry may have more methods

**Completion list:**
```
// Built-in
size            -> Integer
uppercase       -> String
...

// Extensions (from mylib)
json            -> String     [mylib] Convert to JSON
trim            -> String     [stdlib] Remove whitespace
```

### Scenario 4: Inside Block

```
collection do: [:each |
    each |  // Cursor here — what is each?
]
```

**Type inference:**
- `collection` is `Array[Counter]` (if known)
- Therefore `each` is `Counter`
- Show Counter methods

---

## Part 7: Implementation Roadmap

### Phase 1: Basic LSP (Syntax-level)
- [ ] Syntax highlighting
- [ ] Parse errors
- [ ] Go to definition (same file)
- [ ] Basic autocomplete (class methods)

### Phase 2: Cross-file Analysis
- [ ] Symbol index across project
- [ ] Go to definition (cross-file)
- [ ] Find all references
- [ ] Rename symbol

### Phase 3: Type Inference
- [ ] Infer types from assignments
- [ ] Infer types from method signatures
- [ ] Type error detection (where inferable)
- [ ] Richer autocomplete

### Phase 4: Optional Type Annotations
- [ ] Syntax for type annotations
- [ ] Type checker
- [ ] Full type errors
- [ ] Generic types

### Phase 5: Live Environment
- [ ] Connect to running BEAM
- [ ] Query objects for methods
- [ ] Show runtime values on hover
- [ ] Hot reload from IDE

---

## Part 8: Open Questions

1. **Type syntax:** What does Beamtalk type annotation syntax look like?
   - `x: Integer` (colon)
   - `x :: Integer` (double colon, Haskell-style)
   - `Integer x` (prefix, C-style)

2. **Generics:** Do we need generics? How complex?
   - `Array[T]`, `Block[T, U]`, `Future[T]`?

3. **Union types:** For Erlang interop
   - `{ok, Value} | {error, Reason}` → `ErlangResult[T, E]`?

4. **Structural vs nominal:** 
   - Structural: "anything with `increment` method"
   - Nominal: "must be Counter or subclass"

5. **Strictness levels:**
   - Strict mode (all types required)?
   - Loose mode (types optional)?
   - Per-file pragma?

---

## References

- [TypeScript Design Goals](https://github.com/Microsoft/TypeScript/wiki/TypeScript-Design-Goals)
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
- [Sorbet (Ruby type checker)](https://sorbet.org/)
- [Dialyzer (Erlang)](https://www.erlang.org/doc/man/dialyzer.html)
- [Pharo IDE Architecture](https://books.pharo.org/)
- [Gleam Type System](https://gleam.run/book/tour/type-annotations.html)
