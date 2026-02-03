# Type System Design

This document describes the type system design for Beamtalk, including implementation strategy and technical decisions.

**Status**: Design Phase  
**Related Docs**: [semantic-analysis.md](semantic-analysis.md), [beamtalk-language-features.md](../beamtalk-language-features.md)

---

## Table of Contents

- [Design Goals](#design-goals)
- [Type System Approach](#type-system-approach)
- [Background: Smalltalk vs Modern Typing](#background-smalltalk-vs-modern-typing)
- [Erlang Type Systems](#erlang-type-systems)
- [Hindley-Milner Type Inference](#hindley-milner-type-inference)
- [Implementation Strategy](#implementation-strategy)
- [Challenges for Beamtalk](#challenges-for-beamtalk)
- [References](#references)

---

## Design Goals

Beamtalk aims for **gradual typing** with strong tooling support:

1. **Optional type annotations** - types improve tooling but aren't required
2. **Type inference** - minimize annotations needed in practice
3. **IDE-friendly** - LSP can provide completions, hover info, refactoring
4. **BEAM interop** - handle Erlang's dynamic types and union types
5. **Incremental adoption** - add types gradually to existing code

**Philosophy**: Types serve the developer, not the compiler. They should make code easier to write and maintain, not harder.

---

## Type System Approach

### Gradual/Optional Static Typing

Beamtalk follows TypeScript/Python's approach - types are optional annotations that improve tooling:

```beamtalk
// Optional - types inferred
increment => self.value := self.value + 1

// Explicit when you want tooling/safety
getValue -> Integer => self.value

// Typed parameters for public APIs
transferFrom: source: Account amount: Money -> Boolean =>
  source withdraw: amount
  self deposit: amount
  true
```

### Features Planned

From [beamtalk-language-features.md](../beamtalk-language-features.md#optional-type-annotations-dylan-inspired):

- **Basic types**: `Integer`, `Float`, `String`, `Boolean`
- **Union types**: `Integer | Error` (for Erlang interop)
- **Singleton types**: `#north | #south | #east | #west` (atoms)
- **False-or types**: `Integer | False` (option/maybe pattern)
- **Generic types**: `Array[T]`, `Future[T]` (future)

---

## Background: Smalltalk vs Modern Typing

### How Smalltalk Handled Types

Smalltalk had **no static type detection** - types were discovered entirely at runtime:

**1. Duck Typing via Message Dispatch**
```smalltalk
obj doSomething
"Runtime looks up 'doSomething' in obj's class method dictionary"
"If found → execute"
"If not found → invoke doesNotUnderstand: #doSomething"
```

**2. Type is Class Membership**
```smalltalk
obj class            "Returns the class object"
obj isKindOf: Integer    "Explicit type check"
obj isMemberOf: String   "Exact class check"
```

But idiomatic Smalltalk **avoided** these checks - just send messages and let it fail.

**3. Runtime Errors, Not Compile Errors**
- Sending invalid message → `MessageNotUnderstood` exception at runtime
- Variables held references to any object; no compile-time constraints
- The image environment caught errors immediately during development

### Smalltalk Philosophy

**"Don't ask what type it is, ask what it can do."**

- No interfaces or protocols declared - just send messages
- If object responds correctly, it's the "right type"
- `doesNotUnderstand:` allowed dynamic message handling (metaprogramming)

### Why This Won't Work for Modern Tooling

Without static types, IDEs can't provide:
- **Autocomplete** - don't know what methods exist
- **Go to definition** - don't know where method is defined
- **Refactoring** - can't safely rename methods
- **Type errors** - catch bugs before running code

```beamtalk
// Without types - IDE knows nothing
doSomething: obj =>
  obj.???  // What methods exist? No idea!

// With types - IDE has full completion
doSomething: (obj: Counter) =>
  obj.  // Shows: increment, decrement, value, reset...
```

---

## Erlang Type Systems

The BEAM ecosystem has two main approaches to typing:

### 1. Dialyzer - "Success Typing"

**Philosophy**: Optimistic analysis - only warn on PROVABLE errors.

| Traditional Static Typing | Success Typing (Dialyzer) |
|---------------------------|---------------------------|
| "This MIGHT fail → reject" | "This WILL DEFINITELY fail → warn" |
| Conservative | Optimistic |
| False positives possible | Zero false positives |
| Blocks code from compiling | Post-compilation analysis |

**How it works:**

```erlang
-spec add(integer(), integer()) -> integer().
add(A, B) -> A + B.

add("hello", "world")  % Dialyzer: DEFINITE ERROR
```

**Inference from usage:**
```erlang
% No spec - Dialyzer infers from code
get_value(#{value := V}) -> V.
% Infers: "takes map with 'value' key, returns any type"
```

**Only warns on PROVABLE errors:**
```erlang
% DEFINITELY wrong - always crashes
bad() -> 
    X = "hello",
    X + 1.  % Dialyzer warns: strings can't be added

% MIGHT be wrong - no warning
maybe_bad(X) -> 
    X + 1.  % Could be string or number - Dialyzer silent
```

**Advantages:**
- ✅ No false positives - warnings are real
- ✅ Works on existing code - no rewrite needed
- ✅ Finds dead code and unreachable branches

**Limitations:**
- ⚠️ False negatives - misses bugs it can't prove
- ⚠️ Slow - whole-program analysis
- ⚠️ Cryptic errors - warnings hard to understand
- ⚠️ **Post-compilation** - not great for IDE tooling

### 2. Gradualizer - True Gradual Typing

Newer tool that enforces type specs more strictly than Dialyzer:

```erlang
-spec add(integer(), integer()) -> integer().
add(A, B) -> A + B.
% Gradualizer enforces specs at compile time, like TypeScript
```

More like TypeScript's approach - optional but enforced when present.

### Why Not Use Dialyzer for Beamtalk?

**Problem**: Dialyzer is a **post-compilation** tool:
1. Write Beamtalk code
2. Compile to Core Erlang → BEAM
3. Run Dialyzer on BEAM bytecode
4. Get type errors (maybe)

**Too late for IDE tooling** - we need types during editing, not after compilation.

---

## Hindley-Milner Type Inference

**Hindley-Milner (HM)** is the classical type inference algorithm used in ML, Haskell, and Gleam.

### Core Algorithm

**3 main steps:**

**1. Generate fresh type variables**
```rust
let x = 5        // x: T0 (unknown)
let y = x + 1    // y: T1, constraint: T0 = Int, T1 = Int
```

**2. Collect constraints**
```rust
fn add(a, b) { a + b }
// a: T0, b: T1, result: T2
// Constraint: T0 = Int, T1 = Int, T2 = Int (from + operator)
```

**3. Unify (solve constraints)**
```rust
// T0 = Int, T1 = Int → solve to concrete types
// Check for conflicts: T0 = Int AND T0 = String → ERROR
```

### Basic Rust Types

```rust
enum Type {
    Int,
    Bool,
    String,
    Var(u32),                      // Type variable T0, T1, etc.
    Func(Box<Type>, Box<Type>),    // a -> b
}
```

### Complexity Breakdown

#### Easy Parts (1-2 days)
✅ **Basic inference** - literals, simple expressions  
✅ **Unification algorithm** - well-documented  
✅ **Type variables** - just generate unique IDs

#### Medium Parts (1-2 weeks)
⚠️ **Let-polymorphism** - generalizing types  
⚠️ **Type environment** - tracking variable scopes  
⚠️ **Error messages** - making failures understandable

Example of let-polymorphism:
```beamtalk
// Polymorphic identity - works for any type
id := [:x | x]
id value: 5        // id: Integer -> Integer
id value: 'hello'  // id: String -> String (SAME block!)
```

#### Hard Parts (1-2 months)
🔴 **Subtyping** - inheritance hierarchies  
🔴 **Mutable references** - tracking borrowing/ownership  
🔴 **Row polymorphism** - record types with unknown fields  
🔴 **Type classes/traits** - ad-hoc polymorphism

### Implementation Complexity

**Realistic timeline:**
- **MVP (basic inference)**: 1-2 weeks
- **Practical (good errors, polymorphism)**: 1-2 months
- **Production (all features, fast)**: 3-6 months

---

## Implementation Strategy

### Phase 1: Basic Type Checking (No Inference)

**Goal**: Check explicit type annotations, no inference yet.

```beamtalk
// Error: parameter types don't match annotation
add: (x: Integer) (y: String) -> Integer =>
  x + y  // ERROR: y is String, not Integer
```

**Components needed:**
1. Parse type annotations → AST type nodes
2. Build symbol table with declared types
3. Check expressions against declared types
4. Report type errors with spans

**Estimated effort**: 2-3 weeks

### Phase 2: Basic Hindley-Milner Inference

**Goal**: Infer types for local variables and simple expressions.

```beamtalk
// Infer types from usage
count := 0           // count: Integer
name := 'Alice'      // name: String
result := count + 1  // result: Integer
```

**Components needed:**
1. Type variables and substitution
2. Constraint generation from AST
3. Unification algorithm
4. Error reporting with inferred types

**Reference implementations:**
- [tcr/rust-hindley-milner](https://github.com/tcr/rust-hindley-milner) - clean Rust HM
- [Gleam type checker](https://github.com/gleam-lang/gleam/tree/main/compiler-core/src/type_) - production Rust-to-BEAM

**Estimated effort**: 3-4 weeks

### Phase 3: Polymorphism and Generics

**Goal**: Support polymorphic functions and generic types.

```beamtalk
// Polymorphic block
identity := [:x | x]
identity value: 5        // Integer -> Integer
identity value: 'hello'  // String -> String

// Generic collection
Array[Integer] new
```

**Components needed:**
1. Type scheme generalization (forall quantifiers)
2. Type instantiation on usage
3. Generic type parameters
4. Constraint solving for polymorphic calls

**Estimated effort**: 4-6 weeks

### Phase 4: Subtyping and Classes

**Goal**: Support class hierarchies and inheritance.

```beamtalk
// Subclass relationship
Animal subclass: Dog
  methods: [
    bark => Transcript show: 'Woof!'
  ]

// Can pass Dog where Animal expected
greet: (animal: Animal) => animal speak
greet: myDog  // OK - Dog is subtype of Animal
```

**Components needed:**
1. Subtyping constraints
2. Method resolution with inheritance
3. Variance rules (covariance/contravariance)
4. Bounded polymorphism

**Estimated effort**: 6-8 weeks

---

## Challenges for Beamtalk

### 1. Actor Message Passing

What's the type of a message sent to an actor?

```beamtalk
// Sending message to another actor
counter increment
// What type is 'counter'? What type does 'increment' return?
```

**Options:**
- **Option A**: Messages typed by actor's interface
  ```beamtalk
  counter: Counter  // Counter class defines message types
  ```
- **Option B**: Messages are dynamically typed (like Erlang)
  ```beamtalk
  counter: Actor  // All actors have type Actor, messages unchecked
  ```
- **Option C**: Message protocols (like Elixir protocols)
  ```beamtalk
  counter: Incrementable  // Protocol defines message contract
  ```

**Recommendation**: Start with Option A (typed by class), consider protocols later.

### 2. Dynamic Features

Beamtalk supports `doesNotUnderstand:` for metaprogramming:

```beamtalk
doesNotUnderstand: message =>
  // Dynamically handle any message not defined in class
  self forwardTo: proxy message: message
```

**Problem**: Static type checker can't know what messages are valid.

**Solution**: Mark dynamic objects explicitly:
```beamtalk
proxy: Dynamic  // Opts out of static checking
```

### 3. BEAM Interop

Erlang is dynamically typed - how do we type Erlang calls?

```erlang
% Erlang returns {ok, Value} | {error, Reason}
{ok, Value} = erlang:some_function(Arg)
```

**Solution**: Use union types:
```beamtalk
result: {ok: Value} | {error: Reason} := 
  Erlang call: 'some_function' args: [arg]

// Pattern match on result
result match: [
  {ok: value} -> value,
  {error: reason} -> self handleError: reason
]
```

### 4. Hot Code Reloading

Type checking with live updates:

```beamtalk
// Change method signature while system is running
getValue -> Integer => self.value
// Change to:
getValue -> String => self.value toString
```

**Problem**: Type changes break callers at runtime.

**Solutions:**
- Gradual migration: old and new versions coexist
- Runtime type checks on messages
- Recompile dependent modules automatically

---

## References

### Type Theory
- **"Types and Programming Languages" (TAPL)** by Benjamin Pierce - comprehensive type theory textbook
- **"Algorithm W Step by Step"** - classic HM inference paper
- [Type Inference - veera.app](https://veera.app/type_inference.html) - approachable HM tutorial

### Rust Implementations
- [Ricky Han's Rust HM Tutorial](https://rickyhan.com/jekyll/update/2018/05/26/hindley-milner-tutorial-rust.html) - complete working example
- [tcr/rust-hindley-milner](https://github.com/tcr/rust-hindley-milner) - clean Rust implementation
- [nwoeanhinnogaehr/algorithmw-rust](https://github.com/nwoeanhinnogaehr/algorithmw-rust) - Algorithm W in Rust

### Reference Compilers
- [Gleam Type Checker](https://github.com/gleam-lang/gleam/tree/main/compiler-core/src/type_) - production HM for BEAM in Rust
- [TypeScript Compiler](https://github.com/microsoft/TypeScript) - gradual typing reference
- [Rust Compiler Type Inference](https://rustc-dev-guide.rust-lang.org/type-inference.html) - how rustc extends HM

### Erlang Type Systems
- [Dialyzer User Guide](https://www.erlang.org/doc/apps/dialyzer/dialyzer.html) - success typing documentation
- [Learn You Some Erlang - Dialyzer](https://learnyousomeerlang.com/dialyzer) - accessible explanation
- [Gradualizer](https://github.com/josefs/Gradualizer) - gradual typing for Erlang

### Related Beamtalk Docs
- [beamtalk-language-features.md](../beamtalk-language-features.md) - language design with type syntax
- [semantic-analysis.md](semantic-analysis.md) - compiler phase before type checking
- [design-tooling-ide.md](design-tooling-ide.md) - LSP integration needs type information
