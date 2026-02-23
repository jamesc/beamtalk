# ADR 0007: Compilable Standard Library with Primitive Injection

## Status
Implemented (2026-02-07) — Epic BT-286

## Context

### The Problem

Beamtalk's standard library in `stdlib/src/` currently exists as **API-only documentation files**. The `.bt` files define class hierarchies, method signatures, and contracts, but are not compilable. Primitive methods use a comment convention:

```beamtalk
+ other => // implemented by compiler - erlang:'+'
```

This creates several problems:

1. **Not compilable** — The files cannot be processed by the Beamtalk compiler. Methods with `// implemented by compiler` comments have no body the compiler can work with.
2. **Two sources of truth** — The actual primitive implementations live in Erlang runtime modules (`beamtalk_integer.erl`, `beamtalk_string.erl`, etc.) and in codegen dispatch tables (`dispatch_codegen.rs`), disconnected from the `.bt` API definitions.
3. **No user-extensible stdlib** — Because the stdlib isn't compiled, users cannot subclass stdlib classes, override methods, or add extensions through the normal Beamtalk compilation pipeline.
4. **Smalltalk principle violation** — In Smalltalk, the class library *is* the system. Classes define themselves including their primitive connections. Beamtalk's current approach treats the stdlib as external metadata rather than living code.

### Current Architecture

Today, primitive dispatch works through two tiers:

1. **Compile-time specialization** (`dispatch_codegen.rs`) — The codegen recognizes known patterns (e.g., `Integer + Integer`) and emits direct Erlang BIF calls.
2. **Runtime dispatch** (`beamtalk_primitive.erl` → `beamtalk_integer.erl`, etc.) — For messages not caught at compile time, `beamtalk_primitive:send/3` routes by type to per-type dispatch modules.

The `stdlib/src/*.bt` files are not involved in either path — they're purely documentation.

### What We Need

A mechanism to make `stdlib/src/*.bt` files compilable while:
- Connecting method signatures to their primitive implementations
- Allowing the compiler to generate optimized code for primitives
- Supporting fallback Beamtalk code when primitives fail
- Enabling users to extend stdlib classes through normal Beamtalk code
- Maintaining the "everything is a message" illusion

### Experimental Validation

We tested the current compiler against `stdlib/src/Integer.bt` to understand the real gaps. Three distinct problems emerged:

#### Problem 1: Class declaration syntax

`Integer.bt` starts with a bare `Integer` identifier on line 78. The parser requires `Superclass subclass: ClassName` syntax, so `Integer` is parsed as a top-level expression, not a class definition. All subsequent method definitions fail to parse because they're outside a class context.

**Fix:** Use `Object subclass: Integer` (the `sealed` modifier is future work — see below).

#### Problem 1a: Codegen routing for primitive types

Today, the codegen decides between Actor (gen_server) and Value Type (map-backed) based solely on the superclass name in `is_actor_class()` (`codegen/core_erlang/mod.rs:449`). `Object subclass: Integer` would take the value-type path, which auto-generates `new/0` returning `#{__class__ => 'Integer', ...}` — fundamentally wrong for a type backed by native Erlang integers.

**Fix:** With the pragma system, this solves itself. The stdlib **declares its own methods** — the compiler doesn't auto-generate `new/0` or `new/1`. The codegen needs a third routing path for "primitive classes" that:
1. Compiles declared methods (both pragma and pure Beamtalk)
2. Does NOT auto-generate `new/0`, `new/1`, or `init/1`
3. Generates a method table module, not an instantiation module

The routing logic becomes:
- **Actor** (superclass is Actor or Actor subclass) → gen_server codegen
- **Value Type** (superclass is Object, user-defined) → map-backed codegen with auto-generated `new`
- **Primitive Type** (Integer, String, Float, Boolean, etc.) → method-table-only codegen, no instantiation

This means `Object subclass: Integer` compiles successfully — it just produces methods, not a constructor. Integers are created by literals (`42`), not by `Integer new`.

#### Problem 2: Binary method ordering ambiguity

After a method body ending in an expression, the parser reads `+` as continuing the previous expression (binary operation), not as a new binary method definition:

```beamtalk
// ✅ This parses correctly — binary method first
+ other => nil
negated => 0 - self

// ❌ This FAILS — parser reads "self + other" as continuation
negated => 0 - self
+ other => nil
```

This is a pre-existing parser limitation, not specific to stdlib compilation. Binary method definitions must appear before unary methods whose bodies end with expressions, or be separated by periods.

#### Problem 3: Pure Beamtalk methods already work

Methods defined in terms of other messages compile correctly **today** with zero changes:

```beamtalk
Object subclass: Integer
  + other => nil                // placeholder — needs primitive binding
  negated => 0 - self           // ✅ Generates: call 'erlang':'-'(0, Self)
  abs => (self < 0) ifTrue: [self negated] ifFalse: [self]  // ✅ Correct conditional
  isZero => self = 0            // ✅ Generates: call 'erlang':'=:='(Self, 0)
  isEven => (self % 2) = 0      // ✅ Generates: erlang:'rem' + erlang:'=:='
  min: other => (self < other) ifTrue: [self] ifFalse: [other]  // ✅ Correct
```

The compiled Core Erlang output was verified running on BEAM:

```
5 + 3 = 8, negated(7) = -7, abs(-5) = 5, isZero(0) = true,
isEven(4) = true, isEven(7) = false, min(3,5) = 3, min(9,2) = 2
```

**Key finding:** The majority of stdlib methods (~60%) are pure Beamtalk and compile today. Only ~12 primitive bindings per class (arithmetic operators, type conversions, iteration intrinsics) need the pragma mechanism. This is far less work than initially assumed.

#### Scope note: `sealed` modifier

The `sealed` modifier (preventing subclassing of primitive types like Integer, String) is valuable but orthogonal to the compilable stdlib work. It should be treated as **future optimization work**:

- The stdlib files will use `Object subclass: Integer` (not `sealed Object subclass: Integer`)
- The compiler already prevents meaningful subclassing of primitives because they're BEAM values, not processes
- A future issue can add `sealed` as a parser/semantic check that emits a helpful error if someone tries to subclass Integer
- This keeps the current ADR focused on the primitive injection mechanism

## Decision

> **Audience note:** Pragmas are **stdlib-internal syntax**. Regular Beamtalk developers never write pragmas — they consume the stdlib through normal message sends (`42 + 3`, `'hello' size`, `Counter spawn`). The pragma mechanism, intrinsic registry, and naming conventions documented below are reference material for **stdlib maintainers only** (~3 people). From a general developer's perspective, nothing changes — error messages still use `#beamtalk_error{}`, REPL behavior is identical, and IDE completions improve (because the stdlib is now real compiled code instead of documentation stubs).

### Research: How Other Languages Solve This

We surveyed seven languages/systems that face the same problem: connecting high-level source definitions to low-level primitive implementations.

#### 1. Smalltalk/Pharo — Primitive Pragmas

Squeak and Pharo use **pragma annotations** inside method bodies:

```smalltalk
+ aNumber
    "Add the receiver to the argument."
    <primitive: 1>
    ^ super + aNumber
```

**Key properties:**
- `<primitive: N>` is a numbered VM primitive or `<primitive: 'name' module: 'mod'>` for named/pluggable primitives
- The VM tries the primitive first; if it fails, the Smalltalk code after the pragma executes as **fallback**
- Methods are compiled normally — the pragma is part of the method AST
- Fallback code provides graceful degradation (e.g., overflow handling, type coercion)

**Strengths:** Elegant, uniform, self-describing. The method *is* the complete definition — primitive binding + fallback.
**Weaknesses:** Numbered primitives are opaque. Requires VM changes for new primitives.

#### 2. Gleam — @external Annotation

Gleam uses `@external` annotations to bind functions to platform-specific implementations:

```gleam
@external(erlang, "erlang", "bit_size")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_bit_size")
pub fn bit_size(x: BitArray) -> Int
```

**Key properties:**
- Annotation specifies target platform, module, and function name
- No body needed — the external function *is* the implementation
- Supports multiple targets (Erlang + JavaScript) with different bindings
- The stdlib is compiled normally; `@external` methods become direct calls

**Strengths:** Explicit, multi-target, type-safe at the Gleam level.
**Weaknesses:** No fallback mechanism. All-or-nothing external binding.

#### 3. Rust — Lang Items + Intrinsics (Deep Dive)

Rust solves the "how does the stdlib define `i32 + i32`?" problem with a **three-layer** design.
This is the closest analogue to what Beamtalk needs.

**Layer 1: Lang items tag traits for the compiler.**

In `library/core/src/ops/arith.rs`, the `Add` trait is tagged so the compiler knows
"this trait IS the `+` operator":

```rust
#[lang = "add"]                    // compiler: "Add trait = the + operator"
pub trait Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}
```

**Layer 2: Macro-generated impls make it look like normal code.**

A macro generates `impl Add` for all 16 numeric types:

```rust
macro_rules! add_impl {
    ($($t:ty)*) => ($(
        impl Add for $t {
            type Output = $t;
            #[inline]
            fn add(self, other: $t) -> $t { self + other }  // ← looks circular!
        }
    )*)
}
add_impl! { usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 i128 f16 f32 f64 f128 }
```

**Layer 3: The compiler breaks the circularity.**

`self + other` inside `impl Add for i32` appears to define addition as addition —
but the compiler recognizes that for primitive types, `+` maps directly to an LLVM `add`
instruction. The `#[lang = "add"]` attribute is the link that tells the compiler
"when you see `+` on these types, emit machine code directly."

The result: Rust's stdlib **is real, compilable source code** — not stubs or documentation.
But certain method bodies are **hints to the compiler**, not real recursive calls.

For intrinsics that have no Rust equivalent at all, Rust uses a separate mechanism:

```rust
#[rustc_intrinsic]
pub const unsafe fn size_of<T>() -> usize;  // no body — compiler provides it

#[rustc_intrinsic]
pub unsafe fn transmute<Src, Dst>(src: Src) -> Dst;  // no body
```

Each `#[rustc_intrinsic]` function has an **explicit name** (`size_of`, `transmute`,
`unchecked_add`, `ctlz`, etc.) that the compiler matches against a finite registry.

**Direct parallel to Beamtalk:**

| Rust Mechanism | Beamtalk Equivalent |
|----------------|---------------------|
| `#[lang = "add"]` on trait | Compiler knows `+` is a binary message on Integer |
| `fn add(self, other) { self + other }` | `+ other => @primitive '+'` |
| `#[rustc_intrinsic] pub fn size_of<T>()` | `@primitive basicNew` |
| Compiler matches lang item → emits LLVM `add` | Compiler matches pragma → emits `beamtalk_integer:dispatch` |
| Finite intrinsic registry (~150 names) | Finite intrinsic registry (~35 names) |

**Key insight:** Rust needs THREE layers because it has operator overloading traits (lang items
bind traits to operators, macros generate impls, compiler emits instructions). Beamtalk is
**simpler** — `+` is always a message send, and the pragma directly tells the compiler what
to generate. We need only ONE mechanism (pragmas) where Rust needs three.

**Strengths:** Type-safe, zero-cost abstractions, stdlib is real source code, named intrinsics.
**Weaknesses:** Three-layer complexity, deep compiler integration, not user-extensible.

**What Beamtalk should take from Rust:**
1. **Named intrinsics with a finite registry** — each intrinsic has an explicit name
2. **Stdlib IS real source code** — not stubs, not documentation
3. **Compiler has special knowledge of primitives** — but this is invisible to users
4. **The registry is exhaustive** — if it's not in the list, it's not an intrinsic

#### 4. Elixir — Erlang Bootstrap + Special Forms

Elixir bootstraps its standard library using Erlang:

```erlang
%% elixir_bootstrap.erl — provides def, defmodule during bootstrap
```

**Key properties:**
- `Kernel.SpecialForms` are built into the compiler (not regular macros)
- `Kernel` module wraps Erlang BIFs with Elixir-idiomatic names
- Bootstrap phase uses Erlang to compile the first Elixir modules
- After bootstrap, everything is pure Elixir

**Strengths:** Clean bootstrap story, all stdlib is real compilable code after bootstrap.
**Weaknesses:** Requires separate bootstrap phase, special forms can't be overridden.

#### 5. Swift — Builtin Module

Swift provides a private `Builtin` module accessible only to the stdlib:

```swift
@frozen
public struct Bool {
    var _value: Builtin.Int1
}
```

**Key properties:**
- `Builtin.*` types map directly to LLVM IR types
- Only the stdlib (compiled with `-parse-stdlib`) can access `Builtin`
- Public types (`Int`, `Bool`) are thin wrappers around `Builtin` types
- Protocol conformances are on the wrapper types, not builtins

**Strengths:** Clean separation, public API is normal Swift, builtins are hidden.
**Weaknesses:** Requires special compiler flag, two-layer type system.

#### 6. Kotlin — Compiler Intrinsics + Built-in Symbols

Kotlin's compiler recognizes certain stdlib functions as intrinsics:

**Key properties:**
- `kotlin.jvm.internal.Intrinsics` class provides compiler-recognized operations
- Compiler emits JVM primitive types (`int`, `boolean`) where possible
- Built-in symbol table (`IrBuiltIns`) resolves core types across all targets
- Bootstrap: minimal stdlib compiles first, then full stdlib compiles with it

**Strengths:** Transparent optimization, works across JVM/JS/Native.
**Weaknesses:** Complex bootstrap, magic symbols require compiler knowledge.

#### 7. Newspeak/GraalVM Truffle — VM-Provided Primitives

Newspeak and Truffle languages inject primitives at language initialization:

**Key properties:**
- VM provides primitive operations that the language layers on top of
- Standard library written in the language itself, using VM primitives
- Context initialization populates global scope with primitives
- Serialized object graphs ("snapshots") capture the bootstrapped state

**Strengths:** Clean layering, language is self-describing.
**Weaknesses:** Requires VM modification for new primitives.

### Comparison Summary

| Approach | Compilable Stdlib? | Fallback Code? | User Extensible? | Multi-target? | Complexity |
|----------|-------------------|----------------|-------------------|---------------|------------|
| **Smalltalk pragmas** | ✅ | ✅ | ✅ | ❌ | Low |
| **Gleam @external** | ✅ | ❌ | ❌ | ✅ | Low |
| **Rust lang items + intrinsics** | ✅ | ❌ | ❌ | ❌ | High (3 layers) |
| **Elixir bootstrap** | ✅ | ❌ | ✅ | ❌ | Medium |
| **Swift Builtin** | ✅ | ❌ | ❌ | ❌ | Medium |
| **Kotlin intrinsics** | ✅ | ❌ | ❌ | ✅ | High |
| **Newspeak VM prims** | ✅ | ✅ | ✅ | ❌ | Medium |

### Recommended Approach: Named Intrinsics Only

We recommend a **Smalltalk-inspired pragma system adapted for BEAM** with a **single binding mode**: named compiler intrinsics.

#### Why Not Raw Erlang MFA?

An earlier draft proposed two binding modes: `@primitive erlang '+'` for direct Erlang calls and `@primitive intrinsic name` for compiler-generated code. We rejected the raw MFA mode because:

1. **Runtime wrappers add critical value.** The existing runtime modules (`beamtalk_integer.erl`, `beamtalk_string.erl`, etc.) wrap Erlang BIFs with type checking (guards), structured error handling (`#beamtalk_error{}` records with hints), immutability enforcement, and extension registry fallback. A raw `call 'erlang':'+'(Self, Other)` bypasses all of this — on type error, the user gets a raw `badarg` exception instead of a helpful `#beamtalk_error{kind = type_error, hint = <<"Expected a number">>}`.

2. **Misuse risk.** Exposing raw Erlang MFA calls gives stdlib maintainers (and potentially users) a way to bypass the safety net. A typo like `@primitive erlang 'element'` instead of going through `beamtalk_list:dispatch` would silently skip bounds checking, type validation, and error formatting. This is a class of bugs we can eliminate by design.

3. **Simpler model.** One binding mode is easier to learn, implement, and verify. The compiler has exactly one thing to do with a pragma: look up the intrinsic name in the registry and generate the corresponding code. No need to parse module/function pairs, validate Erlang module existence, or handle MFA vs intrinsic differently in the AST.

4. **The runtime dispatch IS the primitive.** Methods like `Integer + other` are already correctly implemented by `beamtalk_integer:dispatch('+', [Other], Self)`. The pragma just tells the compiler "this method's body is handled by the runtime dispatch module, not by compiling Beamtalk source." The intrinsic for arithmetic operations generates a call through the existing runtime wrappers, preserving all safety guarantees.

#### Primitive Annotation Syntax

```beamtalk
// Runtime-dispatch primitive — selector in quotes, routes through beamtalk_integer:dispatch
+ other => @primitive '+'
- other => @primitive '-'

// Structural intrinsic — unquoted name, compiler generates custom code
ifTrue: trueBlock ifFalse: falseBlock => @primitive conditional

// Generative intrinsic — needs class context (field names, module name)
new => @primitive basicNew
spawn => @primitive actorSpawn

// Optional fallback (Smalltalk-style):
// If primitive fails, this code runs
+ other => @primitive '+'
  ^super + other
```

> **Naming convention:** Quoted names (`'+'`, `'size'`, `'asFloat'`) are selector-based — the compiler routes them through the class's runtime dispatch module. Unquoted names (`basicNew`, `actorSpawn`, `conditional`) are structural intrinsics — the compiler generates custom code. This distinction is automatic: the compiler checks if the name is in its structural intrinsic registry.

> **Syntax:** `@primitive 'name'` (in-body, no parentheses). The `@` prefix is consistent with existing `@load` test directives, unambiguous with operators, and extensible for future pragmas.

#### Design Principles

1. **Pragma is part of the method** — Not a comment, not an annotation; it's a first-class AST node inside the method body. The compiler parses it as part of the method.

2. **Optional fallback code** — Code after the pragma executes if the primitive operation fails (wrong types, overflow, etc.). This follows Smalltalk's proven approach.

3. **Single binding mode: named intrinsics** — Each primitive method maps to exactly one named intrinsic from the compiler's finite registry. No raw Erlang MFA calls. This ensures all primitive operations go through the runtime safety layer (type checking, structured errors, extension registry).

4. ~~**Two categories of intrinsics, one keyword**~~ **SUPERSEDED by Amendment (2026-02-11): see `@intrinsic` keyword below.** The original design used one keyword with quoting to distinguish categories. This proved confusing in practice — see the amendment at the end of this document for the replacement: `@primitive 'selector'` for runtime dispatch, `@intrinsic name` for structural intrinsics.

5. **Stdlib becomes compilable** — The compiler processes `stdlib/src/*.bt` files through the normal pipeline. Pragma methods compile to intrinsic-generated code that routes through the runtime safety layer.

6. **Non-primitive methods compile normally** — Methods like `abs`, `min:`, `isEven` that are defined in terms of other messages compile as regular Beamtalk code. No pragma needed.

7. **Safety by construction** — By eliminating raw MFA calls, every primitive operation is guaranteed to go through runtime type checking, structured error handling, and extension registry dispatch. Bugs from accidentally bypassing the safety layer are impossible.

#### Intrinsic Registry

The compiler maintains a finite set of known intrinsics. Each maps to a code generation function that routes through the runtime safety layer.

**Selector-based primitives (runtime dispatch):**

For runtime-dispatch primitives, the pragma uses the selector itself in quotes. The compiler resolves the class context to route through the appropriate runtime dispatch module. No separate intrinsic names needed — the selector IS the name.

```
@primitive 'selector' inside class X  →  beamtalk_X:dispatch('selector', Args, Self)
```

This covers all arithmetic, comparison, conversion, and collection operations across Integer, String, Float, Boolean, List, Dictionary, etc. The runtime dispatch module provides type checking, structured errors, and extension registry fallback.

> **Phase 2 optimization:** The compiler MAY optimize well-known dispatch calls to direct BIF calls when it can prove type safety at compile time (e.g., `Integer + Integer` can skip the guard check and emit `call 'erlang':'+'` directly). This is a **transparent optimization**, not a different binding mode. The semantics remain "route through runtime dispatch."

**Structural intrinsics (compiler-generated code):**

Structural intrinsics use unquoted descriptive names. These are the ~20 cases where the compiler must generate custom code — the generated output depends on class context (field names, module name) or requires control flow structures (case expressions, loops, gen_server wrapping) that can't be expressed as a simple dispatch call.

**Object lifecycle:**

| Intrinsic Name | Class | Generated Code |
|----------------|-------|----------------|
| `basicNew` | Object | Map literal: `~{'__class__' => Class, field => default, ...}~` |
| `basicNewWith` | Object | `maps:merge(basicNew(), InitArgs)` |
| `actorSpawn` | Actor | `gen_server:start_link(Module, ~{}~, [])` → wrap pid in `{beamtalk_object, ...}` |
| `actorSpawnWith` | Actor | `gen_server:start_link(Module, InitArgs, [])` → wrap pid |

**Reflection (ProtoObject / Object):**

| Intrinsic Name | Generated Code |
|----------------|----------------|
| `classOf` | Type dispatch: integers→`'Integer'`, strings→`'String'`, actors→read `__class__` from state |
| `doesNotUnderstand` | Generate DNU handler with `#beamtalk_error{}` |
| `dynamicSend` | `gen_server:cast(Pid, {Selector, Args, Future})` |
| `respondsTo` | `beamtalk_primitive:responds_to(Receiver, Selector)` |
| `fieldNames` | Filter `maps:keys(State)` excluding `__` prefixed internals |
| `fieldAt` | `maps:get(Name, State)` with field-not-found check |
| `fieldAtPut` | `maps:put(Name, Value, State)` with field-not-found check |

**Control flow (Boolean / Block):**

| Intrinsic Name | Generated Code |
|----------------|----------------|
| `conditional` | `case Bool of 'true' -> apply TrueBlock(); 'false' -> apply FalseBlock() end` |
| `conditionalTrue` | `case Bool of 'true' -> apply Block(); 'false' -> Bool end` |
| `conditionalFalse` | `case Bool of 'true' -> Bool; 'false' -> apply Block() end` |
| `shortCircuitAnd` | `case Bool of 'true' -> apply Block(); 'false' -> 'false' end` |
| `shortCircuitOr` | `case Bool of 'true' -> 'true'; 'false' -> apply Block() end` |
| `booleanNot` | `case Bool of 'true' -> 'false'; 'false' -> 'true' end` |
| `blockValue` | `apply BlockVar()` (also `blockValue1`, `blockValue2`, `blockValue3` for arities) |
| `whileTrue` | Recursive loop with condition check |
| `whileFalse` | Recursive loop with negated condition |
| `repeat` | Infinite recursive tail-call |

**Iteration (Integer / List):**

| Intrinsic Name | Generated Code |
|----------------|----------------|
| `timesRepeat` | Counting loop from 1 to N |
| `toDo` | Range iteration: `Self` to `End` calling block with counter |
| `toByDo` | Range iteration with custom step |
| `listDo` | `lists:foreach` equivalent with mutation threading |
| `listCollect` | `lists:map` equivalent |
| `listSelect` | `lists:filter` equivalent |
| `listReject` | `lists:filter` with negated predicate |
| `listInjectInto` | `lists:foldl` equivalent |

**Future/Async:**

| Intrinsic Name | Generated Code |
|----------------|----------------|
| `futureAwait` | `beamtalk_future:resolve(Future)` |
| `futureAwaitTimeout` | `beamtalk_future:resolve_timeout(Future, Timeout)` |
| `futureAwaitForever` | `beamtalk_future:wait_forever(Future)` |

The compiler validates intrinsic names at compile time — an unknown name is a compile error. This catches typos in stdlib definitions early.

#### Concrete Example: Integer.bt

Based on experimental validation, here is what a compilable `Integer.bt` would look like. Note that binary method definitions must appear before unary methods due to the parser ordering constraint (Problem 2 above).

**Before (API-only, not compilable):**
```beamtalk
Integer
  + other => // implemented by compiler - erlang:'+'
  negated => 0 - self
  abs => (self < 0) ifTrue: [self negated] ifFalse: [self]
  isEven => (self % 2) = 0
```

**After (compilable with pragmas):**
```beamtalk
Object subclass: Integer
  // ── Primitive bindings (binary methods first — parser constraint) ──
  // Quoted selectors route through beamtalk_integer:dispatch/3,
  // which provides type guards, #beamtalk_error{} records, and extension fallback.
  + other => @primitive '+'
  - other => @primitive '-'
  * other => @primitive '*'
  / other => @primitive '/'
  % other => @primitive '%'
  =:= other => @primitive '=:='
  /= other => (self == other) not
  < other => @primitive '<'
  > other => @primitive '>'
  <= other => @primitive '<='
  >= other => @primitive '>='

  // ── Pure Beamtalk (compiles today, no changes needed) ──
  negated => 0 - self
  abs => (self < 0) ifTrue: [self negated] ifFalse: [self]
  isZero => self =:= 0
  isEven => (self % 2) =:= 0
  isOdd => (self % 2) /= 0
  isPositive => self > 0
  isNegative => self < 0
  min: other => (self < other) ifTrue: [self] ifFalse: [other]
  max: other => (self > other) ifTrue: [self] ifFalse: [other]

  // ── Structural intrinsics (loop constructs) ──
  timesRepeat: block => @primitive timesRepeat
  to: end do: block => @primitive toDo
  to: end by: step do: block => @primitive toByDo

  // ── Conversion (via runtime dispatch) ──
  asFloat => @primitive 'asFloat'
  asString => @primitive 'asString'
  describe => self asString
```

**What the compiler generates** (verified on BEAM):

| Method | Source | Generated Core Erlang |
|--------|--------|-----------------------|
| `+ other` | `@primitive '+'` | `beamtalk_integer:dispatch('+', [Other], Self)` |
| `negated` | `0 - self` | `call 'erlang':'-'(0, Self)` (pure Beamtalk — direct BIF) |
| `abs` | `(self < 0) ifTrue: [...]` | `case call 'erlang':'<'(Self, 0) of ...` |
| `isZero` | `self =:= 0` | `call 'erlang':'=:='(Self, 0)` |
| `isEven` | `(self % 2) =:= 0` | `call 'erlang':'=:='(call 'erlang':'rem'(Self, 2), 0)` |
| `min: other` | `(self < other) ifTrue: [...]` | `case call 'erlang':'<'(Self, Other) of ...` |

#### Generative Primitives: `new`, `spawn`, and the Class Hierarchy

Not all primitives are simple Erlang function calls. Some are **generative** — their implementation depends on the class they're defined in (class name, module name, field defaults, superclass). In Smalltalk, `basicNew` (`<primitive: 70>`) is the canonical example: the VM allocates an instance with the right number of slots for *that specific class*.

Beamtalk has the same pattern with `new`, `spawn`, `class`, and `doesNotUnderstand:args:`. The pragma system handles these via named intrinsics (e.g., `@primitive basicNew`), where the compiler uses its knowledge of the class definition to generate the right code.

**The full class hierarchy with pragmas:**

```beamtalk
// ── ProtoObject.bt ── (root of everything)
ProtoObject
  // Structural equality — selector-based, routes through runtime dispatch
  == other => @primitive '=='

  // Inequality — pure Beamtalk, no pragma needed
  /= other => (self == other) not

  // Class identity — structural intrinsic, compiler generates type dispatch
  class => @primitive classOf

  // Message fallback — structural intrinsic, compiler generates DNU dispatch
  doesNotUnderstand: selector args: arguments => @primitive doesNotUnderstand

  // Dynamic dispatch — structural intrinsic, compiler generates message send
  perform: selector withArguments: arguments => @primitive dynamicSend

  // Error signalling — selector-based, routes through runtime dispatch
  error: message => @primitive 'error:'


// ── Object.bt ── (value types — inherits ProtoObject)
ProtoObject subclass: Object
  // Instantiation — structural intrinsic, compiler generates map with __class__ + defaults
  // Equivalent to Smalltalk's Behavior>>basicNew (<primitive: 70>)
  new => @primitive basicNew
  new: initArgs => @primitive basicNewWith

  // Nil testing — pure Beamtalk, no pragmas
  isNil => false
  notNil => true
  yourself => self
  ifNil: nilBlock => self
  ifNotNil: notNilBlock => notNilBlock value: self
  ifNil: nilBlock ifNotNil: notNilBlock => notNilBlock value: self

  // Reflection — structural intrinsics
  respondsTo: selector => @primitive respondsTo
  fieldNames => @primitive fieldNames
  fieldAt: name => @primitive fieldAt
  fieldAt: name put: value => @primitive fieldAtPut

  // Debugging — pure Beamtalk
  describe => 'an Object'
  inspect => Transcript show: self describe


// ── Actor.bt ── (process-based — inherits Object)
Object subclass: Actor
  // Spawn — structural intrinsics, compiler generates gen_server wrapping
  spawn => @primitive actorSpawn
  spawnWith: initArgs => @primitive actorSpawnWith

  // Override new with error — pure Beamtalk! No pragma needed.
  // This is the Smalltalk pattern: subclass overrides inherited method.
  new => self error: 'Actors must use spawn, not new'
  new: initArgs => self error: 'Actors must use spawnWith:, not new:'

  describe => 'an Actor'
```

**What the compiler generates for each intrinsic:**

| Intrinsic | On Class | Generated Code |
|-----------|----------|---------------|
| `basicNew` | Object | `~{'__class__' => 'Point', 'x' => 0, 'y' => 0}~` (map with class name + field defaults) |
| `basicNewWith` | Object | `maps:merge(new(), InitArgs)` |
| `actorSpawn` | Actor | `gen_server:start_link(Module, ~{}~, [])` → wrap pid in `{beamtalk_object, Class, Module, Pid}` |
| `actorSpawnWith` | Actor | `gen_server:start_link(Module, InitArgs, [])` → wrap pid |
| `classOf` | ProtoObject | `maps:get('__class__', State)` (actors) or type tag (primitives) |
| `doesNotUnderstand` | ProtoObject | DNU dispatch + `#beamtalk_error{}` with hint |
| `respondsTo` | Object | `maps:is_key(Selector, Methods)` |

**Key insight: this mirrors Smalltalk exactly.** In Smalltalk:
- `Behavior>>basicNew` is `<primitive: 70>` — VM allocates instance with right shape
- `Behavior>>new` calls `self basicNew initialize` — pure Smalltalk
- Subclasses override `new` freely — no special treatment

In Beamtalk:
- `Object>>new` is `@primitive basicNew` — compiler generates class-shaped map
- `Actor>>new` overrides with `self error: ...` — pure Beamtalk
- `Actor>>spawn` is `@primitive actorSpawn` — compiler generates gen_server code
- User Actor subclasses inherit `spawn` — it just works

**This actually simplifies the compiler.** Today, `gen_server.rs` has separate functions (`generate_spawn_function()`, `generate_value_type_new()`, `generate_actor_new_error_method()`) that are called based on class type analysis. With pragmas, the stdlib *declares* which methods exist, and the compiler follows instructions. Actor's `new => self error: ...` is just compiled Beamtalk — no special case in the compiler at all.

#### Three Class Kinds: Actor, Value Type, and Primitive

A compilable stdlib introduces a critical distinction the compiler must understand: **not all `Object subclass:` declarations produce the same kind of module.** Today, the codegen has a binary choice — Actor (gen_server) vs Value Type (map-backed). The pragma system reveals a third kind that was always implicit: **Primitive Types** backed by native Erlang values.

| Class Kind | Backing | Instantiation | Method Dispatch | Example |
|------------|---------|---------------|-----------------|---------|
| **Actor** | Erlang process (gen_server) | `spawn` → `gen_server:start_link` | Async message send → `handle_cast` | `Counter`, `ChatRoom` |
| **Value Type** | Erlang map (`#{__class__ => ...}`) | `new` → map literal with defaults | Direct function call with `Self` as first arg | `Point`, `Color`, `Range` |
| **Primitive Type** | Native Erlang value (integer, binary, etc.) | Literals only (`42`, `'hello'`) — **no `new`** | Routes through runtime dispatch module | `Integer`, `String`, `Boolean`, `Float` |

**What the compiler generates for each kind:**

**Actor** — `Object subclass: Actor` or any `Actor subclass:`:
```beamtalk
Actor subclass: Counter
  state: count = 0
  spawn => @primitive actorSpawn           // gen_server:start_link → wrap pid
  spawnWith: init => @primitive actorSpawnWith
  increment => self.count := self.count + 1  // compiled as handle_cast
  count => self.count                        // compiled as handle_call
```
- Generates: module with `init/1`, `handle_call/3`, `handle_cast/3`, `spawn/0`, `spawn/1`
- State: held in gen_server process, accessed via `self.field`
- Methods: async by default (message sends), sync for queries

**Value Type** — user-defined `Object subclass:`:
```beamtalk
Object subclass: Point
  state: x = 0
  state: y = 0
  new => @primitive basicNew               // map: #{__class__ => 'Point', x => 0, y => 0}
  new: init => @primitive basicNewWith     // maps:merge(new(), init)
  x => self.x                               // maps:get(x, Self)
  distanceTo: other => ...                   // pure Beamtalk
```
- Generates: module with `new/0`, `new/1`, instance methods as functions
- State: immutable map with `__class__` tag + field values
- Methods: synchronous function calls, `Self` passed as first argument

**Primitive Type** — stdlib `Object subclass:` for built-in types:
```beamtalk
Object subclass: Integer
  // NO new/0, NO new/1 — integers are literals (42, not Integer new)
  // NO state declarations — backing is native Erlang integer
  + other => @primitive '+'                // beamtalk_integer:dispatch('+', [Other], Self)
  negated => 0 - self                        // pure Beamtalk, compiles normally
  abs => (self < 0) ifTrue: [self negated] ifFalse: [self]
  timesRepeat: block => @primitive 'timesRepeat'
```
- Generates: **method table only** — no `new`, no `init`, no state management
- State: the native Erlang value IS the object (no wrapping)
- Methods: route through runtime dispatch modules (`beamtalk_integer`, `beamtalk_string`, etc.) which provide type checking, structured errors, and extension registry

**How the compiler knows which kind:**

The compiler determines class kind from a combination of superclass and class name:

1. **Primitive Type** — class name matches a known set: `Integer`, `Float`, `String`, `Boolean`, `Symbol`, `Nil`, `List`, `Dictionary`, `Block`. These are the types with native Erlang backing and dedicated runtime dispatch modules.
2. **Actor** — superclass chain includes `Actor` (directly or transitively).
3. **Value Type** — everything else (superclass is `Object` or a user-defined value type).

This three-way routing replaces today's binary `is_actor_class()` check and is the key codegen change needed for a compilable stdlib.

> **Note:** ProtoObject and Object themselves are special — they define the shared protocol (reflection, nil-testing, error handling) but are never instantiated directly. Their methods are inherited by all three class kinds.

#### Compilation Strategy

When the compiler encounters an `@primitive` pragma:

1. **Parse** — The pragma is parsed as a `Primitive` AST node within the method body.
2. **Validate** — Check that the intrinsic name exists in the compiler's registry OR is a selector name.
3. **Codegen** — Determine if the pragma refers to a structural intrinsic (e.g., `basicNew`, `conditional`) or a runtime-dispatch selector (e.g., `'+'`). For runtime-dispatch selectors, generate a call through the class's runtime module:
   ```erlang
   %% + other => @primitive '+'
   %% Generates:
   call 'beamtalk_integer':'dispatch'('+', [Other], Self)
   ```
   For structural intrinsics (e.g., `basicNew`), generate inline code using class context:
   ```erlang
   %% new => @primitive basicNew
   %% Generates (for class Point with fields x, y):
   ~{'__class__' => 'Point', 'x' => 0, 'y' => 0}~
   ```
4. **Fallback** — If fallback code exists after the pragma, wrap in a try/catch:
   ```erlang
   try call 'beamtalk_integer':'dispatch'('+', [Other], Self)
   catch <_,_,_> -> FallbackCode
   ```
5. **Optimize** (future) — The compiler MAY inline well-known dispatch calls to direct BIF calls when it can prove type safety at compile time. This is a transparent optimization that preserves the semantics of routing through runtime dispatch.

#### Semantic Restrictions: Stdlib-Only Primitives

**Primitives are stdlib-internal syntax.** To prevent misuse and maintain safety guarantees, the compiler restricts `@primitive` to standard library code.

**Default behavior:** Semantic analysis produces an **error** if `@primitive` appears outside the stdlib:

```rust
// In semantic analysis
fn validate_primitive(&self, expr: &PrimitiveExpr, module: &Module) -> Result<()> {
    if !self.is_stdlib_module(module) && !self.flags.allow_primitives {
        return Err(Diagnostic::error(
            "Primitives can only be declared in the standard library",
            expr.span,
        ).with_hint("User code should call stdlib methods, not declare primitives. \
                     Use --allow-primitives flag only if implementing FFI bindings."));
    }
    
    if !self.is_stdlib_module(module) && self.flags.allow_primitives {
        self.diagnostics.push(Diagnostic::warning(
            "Using primitives outside stdlib - ensure you understand safety implications",
            expr.span,
        ));
    }
    
    Ok(())
}
```

**Escape hatch for advanced users:** `--allow-primitives` compiler flag permits pragmas in user code (with warning):

```bash
# ❌ Fails by default
beamtalk build MyExtension.bt
# Error: Primitives only allowed in stdlib (MyExtension.bt:42)

# ✅ Explicit opt-in for FFI
beamtalk build --allow-primitives MyExtension.bt
# Warning: Using primitives outside stdlib (MyExtension.bt:42)
```

**Stdlib detection mechanisms** (in priority order):

1. **Module metadata flag** - Stdlib modules set `is_stdlib: true` in compiled metadata (most robust)
2. **Source path heuristic** - Module path starts with `stdlib/src/` (simple, works for source builds)
3. **Compiler flag** - `--stdlib-mode` flag when compiling stdlib (bootstrap scenario)
4. **Package manifest** - `beamtalk.toml` declares `[package.stdlib = true]` (future package system)

The first available mechanism applies. This prevents:
- Accidental primitive use (syntax looks magical, users might try it)
- Security issues (bypassing runtime type checking)
- Atom table pollution (users inventing intrinsic names)
- Confusion (primitives failing because intrinsic doesn't exist)

**Rationale:** Swift's `-parse-stdlib` approach proves this is a solved problem. Primitives are a **compiler implementation detail**, not a user-facing feature. Users consume stdlib methods; only stdlib maintainers declare them.

#### Migration Path

1. **Phase 1:** Add `@primitive` to the parser and AST. Add semantic validation (stdlib-only restriction). Existing codegen continues to work via the current dispatch tables.
2. **Phase 2:** Convert `stdlib/src/*.bt` files from comment-based markers to pragmas. Compiler compiles stdlib with `--stdlib-mode` flag.
3. **Phase 3:** Move dispatch knowledge from `dispatch_codegen.rs` into the compiled stdlib. The codegen reads primitive bindings from compiled class metadata instead of hardcoded tables.
4. **Phase 4:** Remove hardcoded dispatch tables. All primitive knowledge lives in `stdlib/src/*.bt`.

#### Developer Workflow: Adding a New Primitive

After Phase 4, there are two paths for adding new primitives — and the common path doesn't touch the Rust compiler at all.

**Path A: Runtime-dispatch primitive (~90% of cases)**

Adding a new method that delegates to Erlang/runtime code (e.g., `Integer>>factorial`):

| Step | What | Where | Language |
|------|------|-------|----------|
| 1. Implement | Add `factorial` clause to dispatch function | `runtime/src/beamtalk_integer.erl` | Erlang |
| 2. Register | Add `intFactorial` → dispatch mapping | Compiler intrinsic registry (one line) | Rust |
| 3. Declare | `factorial => @primitive intFactorial` | `stdlib/src/Integer.bt` | Beamtalk |
| 4. Test | Add E2E test, run `just test-e2e` | `tests/e2e/cases/` | Beamtalk |

**No Rust compiler recompilation needed** — the intrinsic registry is a static lookup table. In practice, step 2 is adding one entry to a match arm. The runtime dispatch module provides type checking, error handling, and extension registry support automatically.

**Path B: Structural intrinsic (~10% of cases)**

Adding a new code generation pattern (e.g., a new iteration construct, new instantiation strategy):

| Step | What | Where | Language |
|------|------|-------|----------|
| 1. Implement codegen | Write the code generation function | `crates/beamtalk-core/src/codegen/` | Rust |
| 2. Register | Add intrinsic name → codegen function | Compiler intrinsic registry | Rust |
| 3. Declare | `newMethod: arg => @primitive newIntrinsic` | `stdlib/src/*.bt` | Beamtalk |
| 4. Test | Unit test for codegen + E2E test | `crates/*/tests/`, `tests/e2e/cases/` | Rust + Beamtalk |

This requires recompiling the Rust compiler, but it's rare — structural intrinsics are things like loop constructs, gen_server wrapping, and instantiation patterns. The set is small (~35 today) and grows slowly.

**Comparison with today:**

| Task | Today | With pragmas |
|------|-------|-------------|
| Add `Integer>>factorial` | Edit `builtins.rs` (Rust), recompile compiler | Edit `.erl` + `.bt`, no Rust recompile |
| Add `String>>reverse` | Edit `builtins.rs` (Rust), recompile compiler | Edit `.erl` + `.bt`, no Rust recompile |
| Add new loop construct | Edit `builtins.rs` (Rust), recompile compiler | Edit codegen (Rust), recompile compiler |
| Add pure Beamtalk method | Edit `builtins.rs` (Rust), recompile compiler | Edit `.bt` only, recompile stdlib |

The key insight: **most stdlib evolution becomes an Erlang + Beamtalk task, not a Rust task.** The Rust compiler is only involved when genuinely new code generation patterns are needed.

## Consequences

### Positive

- **Single source of truth** — Each stdlib method defines its own primitive binding. No disconnect between API docs and implementation.
- **Compilable stdlib** — `stdlib/src/*.bt` files go through the real compiler pipeline, catching syntax errors, enabling IDE features, and producing real BEAM code.
- **Smalltalk-aligned** — Primitive pragmas are a well-proven pattern from 40+ years of Smalltalk. Beamtalk users familiar with Squeak/Pharo will recognize the approach.
- **Fallback support** — Graceful degradation when primitives fail (type errors, overflow) without separate error handling logic.
- **Safety by construction** — Named intrinsics route through runtime dispatch modules, guaranteeing type checking, structured error handling, and extension registry support. No way to accidentally bypass the safety layer with a raw Erlang MFA call.
- **User extensibility** — Once stdlib classes are real compiled classes, users can extend them through the extension system (`beamtalk_extensions`).
- **IDE integration** — The LSP can provide completions, hover docs, and go-to-definition for stdlib methods because they're real compiled code.
- **Incremental migration** — The four-phase plan allows gradual transition without breaking existing functionality.
- **Smaller scope than expected** — Experimental validation showed ~60% of stdlib methods are pure Beamtalk and compile today. Only ~12 primitive bindings per class need the pragma mechanism.
- **Explicit three-kind class model** — The distinction between Actor (process-backed), Value Type (map-backed), and Primitive Type (native Erlang value) becomes explicit and driven by the stdlib declarations rather than hardcoded compiler heuristics. `Object subclass: Integer` works correctly because the stdlib declares what Integer IS, not because the compiler guesses.

### Negative

- **New syntax to learn** — The `@primitive` pragma adds syntax that doesn't exist today. However, only stdlib maintainers write pragmas — semantic analysis enforces this restriction.
- **Parser complexity** — The parser must handle pragmas as a new AST node type.
- **Semantic analysis required** — The compiler must validate that pragmas only appear in stdlib code (requires stdlib detection mechanism).
- **Bootstrap ordering** — The compiler must be able to compile the stdlib before user code. This introduces a three-step build chain: `build-rust` → `build-erlang` → `build-stdlib`. The stdlib is packaged as an OTP application (`beamtalk_stdlib`) for proper BEAM integration.
- **Binary method ordering constraint** — Due to a pre-existing parser ambiguity, binary method definitions (`+ other =>`) must appear before unary methods whose bodies end with bare expressions. This is a parser bug that should be fixed independently, but affects stdlib file organization in the meantime.

### Neutral

- **Runtime dispatch unchanged** — The existing `beamtalk_primitive:send/3` → `beamtalk_integer:dispatch/3` path continues to work. Named intrinsics generate calls *through* these runtime modules, preserving all existing safety guarantees (type checking, structured errors, extension registry).
- **Existing codegen unaffected initially** — Phase 1-2 can coexist with current hardcoded dispatch tables. Full migration happens in Phase 3-4.
- **Extension system compatible** — The `beamtalk_extensions` ETS-based extension registry works orthogonally to primitive pragmas. Runtime dispatch modules already consult the extension registry as a fallback.
- **No raw Erlang MFA escape hatch** — Unlike Smalltalk's named primitives (`<primitive: 'name' module: 'mod'>`), we deliberately omit raw MFA binding. This prevents a class of bugs where developers bypass type checking and error handling. If a new Erlang BIF needs binding, it goes through a runtime dispatch module first.
- **`sealed` modifier is future work** — The stdlib files will use `Object subclass: Integer` without `sealed`. The BEAM already prevents meaningful subclassing of primitives (they're values, not processes). Adding `sealed` as a parser/semantic check can be a separate issue that emits a helpful error like: *"Cannot subclass Integer — primitive types are sealed. Define an Actor subclass instead."*

### Compiler Impact Analysis

This is **not primarily a line-count reduction.** The codegen machinery (state threading, block analysis, dispatch routing, gen_server scaffolding — ~6,300 lines across `gen_server.rs`, `dispatch_codegen.rs`, `builtins.rs`, `mod.rs`) stays in the compiler because it does real work. What changes is **where knowledge lives**:

| Today (hardcoded) | Lines | After (pragma-driven) |
|--------------------|-------|-----------------------|
| `builtins.rs`: 60+ selector→Erlang mappings per type | ~400 | Moves to `stdlib/src/*.bt` as pragma declarations |
| `dispatch_codegen.rs`: special-cased selectors | ~60 | Moves to `stdlib/src/*.bt` as pragma declarations |
| `gen_server.rs`: auto-generated `spawn`/`new`/error methods | ~150 | Moves to `stdlib/src/Actor.bt` and `stdlib/src/Object.bt` |
| `mod.rs`: `is_actor_class()` heuristic | ~20 | Replaced by three-kind routing from class metadata |
| **Total knowledge lines that move** | **~630** | **Into `stdlib/src/*.bt` files** |

The real simplification is **architectural, not volumetric:**

1. **Stdlib changes don't require recompiling the Rust compiler.** Today, adding `Integer>>factorial` means editing `builtins.rs` in Rust. With pragmas, it's just editing `stdlib/src/Integer.bt` — a Beamtalk file.
2. **The compiler becomes generic.** It doesn't need to know what methods Integer has. It reads the pragma, looks up the intrinsic registry, and generates code. New types can be added without touching the compiler.
3. **Dispatch tables become declarative.** The 60+ match arms in `builtins.rs` (`try_generate_integer_message`, `try_generate_string_message`, etc.) become declarations in `.bt` files that the compiler processes uniformly.
4. **Class kind routing becomes data-driven.** Instead of `is_actor_class()` guessing from superclass name, the three-kind distinction (Actor/Value Type/Primitive) is driven by the compiled stdlib's class metadata.

## Alternatives Considered

### A. Gleam-style @external Annotations

```beamtalk
@external(erlang, "erlang", "+")
+ other
```

**Rejected because:** No fallback mechanism, annotation-before-method is not Smalltalk-idiomatic, and it creates a visual disconnect between the method signature and its binding.

### B. Compiler Magic (Status Quo, Formalized)

Keep `stdlib/src/*.bt` as documentation. Formalize the compiler's internal dispatch tables as the source of truth.

**Rejected because:** Perpetuates the two-source-of-truth problem. The stdlib remains non-compilable, and users cannot extend stdlib classes through normal Beamtalk code.

### C. Erlang FFI Module

```beamtalk
Integer
  + other => Erlang.call(erlang, '+', [self, other])
```

**Rejected because:** Too verbose, exposes Erlang internals in every primitive method, no compiler optimization opportunity, and no fallback mechanism.

### D. Separate Primitive Declaration Files

```
// integer.primitives
Integer.+ -> erlang:'+'
Integer.- -> erlang:'-'
```

**Rejected because:** Creates yet another file to maintain, disconnected from the class definition. However, this approach has a DevEx advantage: it keeps the language surface completely clean. If the pragma syntax proves problematic during implementation, this could be revisited.

### E. Two Binding Modes: Named Intrinsics + Raw Erlang MFA

An earlier draft proposed two binding modes:
- `@primitive intrinsic '+'` — Named compiler intrinsic
- `@primitive erlang '+'` — Direct Erlang MFA call (receiver as first arg)

```beamtalk
// Raw MFA — bypasses runtime dispatch
+ other => @primitive erlang '+'

// Named intrinsic — routes through runtime
spawn => @primitive intrinsic actorSpawn
```

**Rejected because:** Raw Erlang MFA calls bypass the runtime safety layer (`beamtalk_integer:dispatch/3`, etc.) which provides type checking with guards, structured `#beamtalk_error{}` records with actionable hints, immutability enforcement, and extension registry fallback. A direct `call 'erlang':'+'(Self, Other)` would give users raw `badarg` exceptions instead of helpful error messages. More dangerously, it's a misuse foothold — a typo like `@primitive erlang 'element'` instead of routing through `beamtalk_list:dispatch` would silently skip bounds checking and error formatting. Eliminating the MFA escape hatch makes safety bugs impossible by construction.

### F. Alternative Pragma Delimiters

The original `<primitive:>` angle-bracket syntax was considered but conflicts visually with `<` and `>` comparison operators. All delimiters were evaluated:

| Syntax | Example | Pros | Cons | Decision |
|--------|---------|------|------|----------|
| `<primitive: '+'>` | `< other => <primitive: '<'>` | Smalltalk-standard | Visual conflict with `<`, `>` operators | ❌ Rejected |
| `@primitive '+'` | `< other => @primitive '<'` | Clean, unambiguous, consistent with `@load` | Not in classic Smalltalk | ✅ **Selected** |
| `@primitive('+')` | `< other => @primitive('<')` | Function-like | Extra punctuation | ❌ Rejected |
| `native '+'` | `< other => native '<'` | Reads naturally | New keyword | ❌ Rejected |
| `{primitive: '+'}` | `< other => {primitive: '<'}` | Existing delimiters | Clashes with dict syntax | ❌ Rejected |

**Decision: `@primitive 'name'` (in-body, no parentheses).**

Rationale:
- **Consistent with existing syntax** - `@` already used for `@load` test directives
- **Clean and minimal** - No parentheses needed, keeps Smalltalk's minimal punctuation philosophy
- **Unambiguous** - No parser conflicts with `<`, `>`, or dictionary syntax
- **Clear fallback semantics** - Code after pragma executes only if primitive fails
- **Extensible** - Opens path for future pragmas (`@inline`, `@deprecated`, etc.)

Examples:
```beamtalk
+ other => @primitive '+'
  ^self slowAdd: other  // Fallback if primitive fails

abs => @primitive 'abs'
  (self < 0) ifTrue: [0 - self] ifFalse: [self]

size => @primitive 'size'  // No fallback - error if fails
```

## Build System Integration

### The Bootstrap Chain

Compiling the stdlib introduces a strict build ordering dependency:

```
┌──────────────────────────────────────┐
│  1. Rust Compiler (cargo build)      │  Contains intrinsic registry
│     target/debug/beamtalk            │  as Rust match arms
└──────────────┬───────────────────────┘
               │ must exist first
┌──────────────▼───────────────────────┐
│  2. Erlang Runtime (rebar3 compile)  │  beamtalk_integer.erl,
│     runtime/_build/.../ebin/         │  beamtalk_string.erl, etc.
└──────────────┬───────────────────────┘
               │ dispatch modules must exist for @primitive to route to
┌──────────────▼───────────────────────┐
│  3. Stdlib (beamtalk build lib/)     │  Integer.bt → integer.beam
│     runtime/_build/.../              │  String.bt → string.beam
│     beamtalk_stdlib/ebin/            │  etc.
└──────────────┬───────────────────────┘
               │ must be on code path
┌──────────────▼───────────────────────┐
│  4. User Code (beamtalk build app/)  │  Normal compilation
└──────────────────────────────────────┘
```

**No circularity.** The compiler knows about intrinsics via its Rust-compiled intrinsic registry — it doesn't need stdlib `.beam` files to *compile* the stdlib. The `.beam` files are needed at *runtime* (REPL, test execution).

### Stdlib as an OTP Application: `beamtalk_stdlib`

The compiled stdlib is packaged as a proper OTP application within a rebar3 umbrella project. The existing `runtime/` directory becomes a multi-app project containing both `beamtalk_runtime` (hand-written Erlang) and `beamtalk_stdlib` (compiled from `stdlib/src/*.bt`).

#### Umbrella Project Structure

```
runtime/                                  # Rebar3 umbrella project
├── rebar.config                          # Updated: {project_app_dirs, ["apps/*"]}
├── apps/
│   ├── beamtalk_runtime/                 # Existing Erlang code (moved from runtime/src/)
│   │   ├── src/
│   │   │   ├── beamtalk_runtime.app.src
│   │   │   ├── beamtalk_runtime_app.erl
│   │   │   ├── beamtalk_integer.erl
│   │   │   ├── beamtalk_string.erl
│   │   │   └── ...
│   │   ├── include/
│   │   │   └── beamtalk.hrl
│   │   └── test/
│   │       ├── beamtalk_actor_tests.erl
│   │       └── ...
│   └── beamtalk_stdlib/                  # NEW: compiled stdlib output
│       ├── src/
│       │   └── beamtalk_stdlib.app.src   # Generated by build-stdlib
│       └── ebin/
│           ├── beamtalk_stdlib.app       # Generated
│           ├── integer.beam              # Compiled from stdlib/src/Integer.bt
│           ├── string.beam               # Compiled from stdlib/src/String.bt
│           └── ...
└── _build/default/lib/
    ├── beamtalk_runtime/ebin/            # rebar3-compiled runtime
    ├── beamtalk_stdlib/ebin/             # Linked/copied from apps/
    └── jsx/ebin/

lib/                                      # Stdlib SOURCE (Beamtalk, not Erlang)
├── Integer.bt
├── String.bt
├── Object.bt
└── ...
```

**Key distinction:** `stdlib/src/*.bt` is the *source*. `runtime/apps/beamtalk_stdlib/ebin/` is the *output*. The Beamtalk compiler reads from `stdlib/src/`, the BEAM VM loads from `ebin/`. Rebar3 manages the runtime app; the Beamtalk compiler manages the stdlib app.

#### Umbrella rebar.config

```erlang
%% runtime/rebar.config (updated for umbrella)
{project_app_dirs, ["apps/*"]}.

{deps, [
    {jsx, "~> 3.0"}
]}.

%% ... existing profiles, hooks, dialyzer config ...
```

#### App Definitions

```erlang
%% runtime/apps/beamtalk_runtime/src/beamtalk_runtime.app.src
%% (unchanged from today, just moved)
{application, beamtalk_runtime, [
    {description, "Beamtalk Runtime - Actor and Future implementations"},
    {vsn, "0.1.0"},
    {modules, []},
    {registered, [beamtalk_classes]},
    {applications, [kernel, stdlib]},
    {mod, {beamtalk_runtime_app, []}},
    {env, [{repl_port, 9000}, {node_name, 'beamtalk@localhost'}]}
]}.
```

```erlang
%% runtime/apps/beamtalk_stdlib/src/beamtalk_stdlib.app.src
%% (auto-generated by beamtalk build-stdlib)
{application, beamtalk_stdlib, [
    {description, "Beamtalk Standard Library - compiled from stdlib/src/*.bt"},
    {vsn, "0.1.0"},
    {modules, [integer, string, object, actor, ...]},
    {applications, [kernel, stdlib, beamtalk_runtime]},
    {env, []}
]}.
```

`beamtalk_stdlib` declares a dependency on `beamtalk_runtime` — this ensures runtime dispatch modules are loaded before any stdlib code runs.

#### Why an OTP Umbrella (Not Loose `.beam` Files)

| Concern | Loose files in `stdlib/src/build/` | OTP umbrella app |
|---------|---------------------------|------------------|
| Code path | Manual `-pa` flag | Automatic via rebar3 |
| Dependencies | Manual load ordering | Declared in `.app.src` |
| Release packaging | Manual | `rebar3 release` just works |
| Hot code upgrade | Manual | OTP appup files |
| Dialyzer | Manual PLT | `rebar3 dialyzer` includes it |
| `observer` | Modules appear orphaned | Grouped under `beamtalk_stdlib` |

#### Migration: `runtime/` → Umbrella

The restructuring is mechanical:

```bash
# One-time migration (can be a single commit)
cd runtime
mkdir -p apps/beamtalk_runtime
mv src/ apps/beamtalk_runtime/
mv include/ apps/beamtalk_runtime/
mv test/ apps/beamtalk_runtime/
mv test_fixtures/ apps/beamtalk_runtime/

mkdir -p apps/beamtalk_stdlib/src apps/beamtalk_stdlib/ebin

# Update rebar.config to add {project_app_dirs, ["apps/*"]}
# Update CI paths that reference runtime/src/ or runtime/test/
```

This can happen independently of the pragma work — it's a pure restructuring with no behavior change.

### Build Pipeline

#### Justfile Changes

```just
# Build all targets (Rust + Erlang runtime + stdlib)
build: build-rust build-erlang build-stdlib

# Build Rust workspace
build-rust:
    @echo "🔨 Building Rust workspace..."
    cargo build --all-targets

# Build Erlang runtime
build-erlang:
    @echo "🔨 Building Erlang runtime..."
    cd runtime && rebar3 compile

# Build stdlib (stdlib/src/*.bt → beamtalk_stdlib OTP app)
build-stdlib: build-rust build-erlang
    @echo "🔨 Building stdlib..."
    target/debug/beamtalk build-stdlib

# CI: unchanged recipe name, now includes stdlib
ci: build lint test test-e2e
```

#### `beamtalk build-stdlib` Command

New CLI subcommand that:

1. **Finds** all `stdlib/src/*.bt` files
2. **Compiles** each through the normal pipeline with `--stdlib-mode` flag
3. **Outputs** `.beam` files to `runtime/_build/default/lib/beamtalk_stdlib/ebin/`
4. **Generates** `beamtalk_stdlib.app` with module list
5. **Skips** files whose `.beam` is newer than `.bt` source (incremental)

```rust
// beamtalk build-stdlib implementation (pseudocode)
fn build_stdlib() -> Result<()> {
    let lib_dir = find_lib_dir()?;          // <repo>/lib/
    let out_dir = find_stdlib_ebin()?;       // runtime/_build/.../beamtalk_stdlib/ebin/
    fs::create_dir_all(&out_dir)?;

    let mut modules = Vec::new();
    for bt_file in glob("stdlib/src/*.bt") {
        let beam_file = out_dir.join(module_name(&bt_file) + ".beam");

        // Incremental: skip if .beam is newer than .bt
        if beam_file.exists() && beam_file.modified()? > bt_file.modified()? {
            modules.push(module_name(&bt_file));
            continue;
        }

        // Compile with stdlib mode (allows @primitive, sets metadata)
        compile_file(&bt_file, &out_dir, CompilerFlags {
            stdlib_mode: true,
            ..default()
        })?;
        modules.push(module_name(&bt_file));
    }

    // Generate .app file
    generate_app_file(&out_dir, &modules)?;
    Ok(())
}
```

#### REPL Code Path Update

The REPL's `start_beam_node()` already adds runtime ebin to the code path via `-pa`. With the umbrella structure, rebar3 handles code paths for both apps automatically in `_build/`. The REPL just needs to add the stdlib ebin alongside the existing runtime ebin:

```rust
// In repl.rs start_beam_node()
let runtime_beam_dir = build_lib_dir.join("beamtalk_runtime/ebin");
let stdlib_beam_dir = build_lib_dir.join("beamtalk_stdlib/ebin");  // NEW
let jsx_beam_dir = build_lib_dir.join("jsx/ebin");

let mut args = vec![
    "-noshell".to_string(),
    "-pa".to_string(), runtime_beam_dir.to_string(),
    "-pa".to_string(), stdlib_beam_dir.to_string(),  // NEW
    "-pa".to_string(), jsx_beam_dir.to_string(),
];
```

### Incremental Rebuild Strategy

```bash
# Simple timestamp check (sufficient for 16 files)
# Each .bt compiles independently — no cross-file dependencies
for f in stdlib/src/*.bt; do
    beam="$STDLIB_EBIN/$(basename "$f" .bt).beam"
    if [ ! -f "$beam" ] || [ "$f" -newer "$beam" ]; then
        beamtalk build --stdlib-mode "$f" --output "$STDLIB_EBIN/"
    fi
done
```

**Why no dependency tracking?** Beamtalk stdlib files have **no compile-time dependencies on each other**. This is a fundamental property of the message-send model:

- `Object subclass: Integer` — the compiler uses `Object` as a string for codegen, it doesn't load or verify `Object.beam` exists
- Pure Beamtalk like `abs => (self < 0) ifTrue: [0 - self] ifFalse: [self]` — generates message sends resolved at runtime, not compile time
- `@primitive '+'` — looked up in the Rust intrinsic registry, not in another `.beam` file
- `super` calls — codegen uses the superclass name from the same file's class declaration

BEAM resolves inter-module calls lazily (on first call, not at load time), so `.beam` files can be loaded in any order. This means all 16 stdlib files can be compiled **in parallel** with no dependency graph — unlike Rust (where `mod` imports create a DAG) or C++ (where `#include` creates ordering).

### Phase Awareness

The build system changes can be rolled out incrementally alongside the migration phases:

| Phase | Build Change | User Impact |
|-------|-------------|-------------|
| **Phase 1** | No change — parser/AST only | None |
| **Phase 2** | Add `build-stdlib` to Justfile | `just build` slightly slower (~2s for 16 files) |
| **Phase 3** | Stdlib `.beam` required on code path | REPL needs `build-stdlib` before starting |
| **Phase 4** | Remove hardcoded dispatch; stdlib is required | `build-stdlib` is mandatory step |

**Important:** During Phases 1-3, the existing hardcoded dispatch tables in `builtins.rs` and `dispatch_codegen.rs` continue to work. The stdlib `.beam` files are additive — they provide class metadata and pure Beamtalk methods, but primitive dispatch still falls back to the hardcoded path. Only Phase 4 (removing hardcoded tables) makes `build-stdlib` mandatory.

### CI Pipeline

```yaml
# .github/workflows/ci.yml — updated job
steps:
  - name: Build
    run: just build              # Now includes build-stdlib

  - name: Lint
    run: just lint

  - name: Test
    run: just test test-e2e      # Stdlib .beam on path for E2E tests
```

No new CI jobs needed — `just build` already chains the three steps.

## Future Work

- **`sealed` modifier** — Add parser support for `sealed Object subclass: Integer` with a semantic check that prevents subclassing. Emit helpful error: *"Cannot subclass Integer — primitive types are sealed."* This is an optimization/safety feature, not required for compilable stdlib.
- **Binary method ordering** — Fix the parser ambiguity that requires binary methods to appear before unary methods. This is a general parser improvement, not specific to stdlib.
- **REPL introspection** — Define what `Integer >> #+` shows in the REPL for primitive methods. Should display the pragma binding and any fallback code.
- **Primitive validation** — The compiler could validate that referenced Erlang modules/functions actually exist, catching typos in primitive bindings at compile time.
- **Compile-time inlining optimization** — For well-known selectors like `'+'` where the compiler can prove type safety at compile time (e.g., both operands are literal integers or known Integer-typed), the compiler could skip runtime dispatch and emit direct `call 'erlang':'+'` for performance. This is a transparent optimization that preserves runtime dispatch semantics.
- **Package-level stdlib declaration** — When the package system exists (`beamtalk.toml`), allow packages to declare `[package.stdlib = true]` for third-party stdlib extensions. This would allow community-maintained stdlib packages (e.g., `beamtalk-crypto`, `beamtalk-json`) to use primitives with explicit opt-in.

## Documentation Impact

If this ADR is accepted, the following documents need updating:

### Must Update

| Document | What Changes | Severity |
|----------|-------------|----------|
| **`docs/development/common-tasks.md`** (lines 26-62) | "Add builtin handler in `builtins.rs`" workflow becomes "Add pragma in `stdlib/src/*.bt` + implement in runtime `.erl`". The `// implemented by compiler` marker convention is replaced by pragmas. The BT-176 example pattern needs updating. | **High** — describes the day-to-day developer workflow |
| **`docs/beamtalk-language-features.md`** (line 1227) | "Most reflection methods are implemented in the `dispatch/4` function...The `perform:` and `perform:withArguments:` methods are handled in the frontend code generator (`builtins.rs`)" — needs to describe pragma-based declarations instead of hardcoded `builtins.rs` | **High** — primary language reference |
| **`stdlib/src/*.bt` files** (all 16 files) | Convert from `// implemented by compiler` comments to `@primitive name` pragmas. This IS the migration itself — Phase 2 of the plan. | **High** — the core deliverable |

### Should Update (Cross-references)

| Document | What Changes | Severity |
|----------|-------------|----------|
| **`docs/internal/design-self-as-object.md`** (lines 15, 117-189) | Documents `beamtalk_primitive.erl` dispatch architecture. Still accurate (runtime dispatch is unchanged), but should add note: "Primitive method declarations now live in compiled `stdlib/src/*.bt` files via pragmas; runtime dispatch modules continue to handle execution." | **Low** — runtime architecture unchanged, just add cross-ref |
| **`docs/beamtalk-ddd-model.md`** | Missing a "Standard Library" bounded context. Should document that stdlib compilation is a distinct context feeding class metadata into Code Generation. | **Low** — additive, not corrective |
| **`docs/ADR/0005-beam-object-model-pragmatic-hybrid.md`** | References sealed primitives and auto-generated constructors. Should cross-reference ADR 0007 for how primitives are now declared (pragma-driven, not auto-generated). | **Low** — add cross-reference |
| **`docs/ADR/0006-unified-method-dispatch.md`** | Should note that class-kind routing (Actor/Value Type/Primitive) is now driven by compiled stdlib metadata, not `is_actor_class()` heuristic. | **Low** — add cross-reference |
| **`AGENTS.md`** (line 1058) | `stdlib` label description says "Standard library: collections, primitives, strings" with key directory `stdlib/src/`. Should note that `stdlib/src/` files are now compiled source, not API-only docs. | **Low** — one-line update |

### No Change Needed

| Document | Why |
|----------|-----|
| **`docs/beamtalk-syntax-rationale.md`** | Pragmas are compiler machinery, not language syntax. Correct scope. |
| **`docs/beamtalk-principles.md`** (line 88) | "No special syntax for primitives — even `+` is a message" — still true. Pragmas are internal to stdlib, invisible to users. |
| **`docs/beamtalk-architecture.md`** | High-level architecture unchanged (Rust compiler → Core Erlang → BEAM). |
| **`docs/development/erlang-guidelines.md`** | Runtime Erlang patterns unchanged. |
| **`docs/development/rust-guidelines.md`** | Rust coding standards unchanged. |
| **`docs/internal/operator-implementation-status.md`** | Operators still compile to same Erlang BIFs. |

## Open Questions (Resolved)

1. ~~**Arithmetic dispatch performance.**~~ **RESOLVED:** Pure Beamtalk expressions like `0 - self` already compile to direct `call 'erlang':'-'(0, Self)` — the compiler's `generate_binary_op()` in `dispatch_codegen.rs` emits BIF calls directly, bypassing runtime dispatch entirely. This proves the compiler already has the optimization capability. For pragma methods, we take a two-phase approach:
   - **Phase 1 (correctness):** Pragma methods route through runtime dispatch (`beamtalk_integer:dispatch`) for type checking and structured errors. This is the safe default.
   - **Phase 2 (optimization):** The compiler extends the existing `generate_binary_op()` fast path to pragma methods when it can prove type safety at compile time (e.g., both operands are literal integers, or the receiver is known to be Integer from class context). This is a transparent optimization — same semantics, direct BIF codegen. Not speculative; the machinery already exists.
   
   Net effect: arithmetic in user code (pure Beamtalk) is already fast. Pragma methods get the same speed in Phase 2. No BEAM performance concern.

2. ~~**Atom table pressure from intrinsic names.**~~ **RESOLVED:** Intrinsic names (`basicNew`, `conditional`, etc.) are **Rust strings in the compiler**, used at compile time to look up code generation functions. They never appear as Erlang atoms in generated Core Erlang. The generated code contains atoms that already exist (`'+'`, `'beamtalk_integer'`, `'dispatch'`, etc.). Zero atom table impact.

3. ~~**Should runtime-dispatch pragmas use a different keyword than structural intrinsics?**~~ ~~**RESOLVED: No — one keyword.**~~ **SUPERSEDED (2026-02-11): Yes — `@intrinsic` for structural intrinsics.** Experience showed that the quote-based distinction was confusing in practice (187 quoted vs 25 unquoted uses, visually near-identical). See the amendment at the end of this document.

4. ~~**Intrinsic naming convention.**~~ **RESOLVED: Use the selector itself for runtime-dispatch primitives.** The pragma is always inside a class definition, so the compiler knows the class context. Instead of `intAdd`, `stringSize`, etc., runtime-dispatch primitives use the **selector itself**:

   ```beamtalk
   Object subclass: Integer
     + other => @primitive '+'        // compiler resolves: Integer class + '+' selector → beamtalk_integer:dispatch('+', ...)
     asFloat => @primitive 'asFloat'  // compiler resolves: Integer class + 'asFloat' → beamtalk_integer:dispatch('asFloat', ...)
   
   Object subclass: String
     size => @primitive 'size'        // compiler resolves: String class + 'size' → beamtalk_string:dispatch('size', ...)
     ++ other => @primitive '++'      // compiler resolves: String class + '++' → beamtalk_string:dispatch('++', ...)
   ```

   The compiler resolves `@primitive 'X'` by: (1) check if `X` is a structural intrinsic name — if yes, use custom codegen; (2) otherwise, route through the class's runtime dispatch module (`className` → `beamtalk_className:dispatch`).

   Structural intrinsics keep their descriptive names because they don't map to a single selector:

   ```beamtalk
   new => @primitive basicNew           // structural — compiler generates map literal
   spawn => @primitive actorSpawn       // structural — compiler generates gen_server wrapping
   ifTrue: t ifFalse: f => @primitive conditional  // structural — compiler generates case expression
   ```

   **Result:** The intrinsic registry shrinks from ~50 entries to ~20 structural intrinsics. All runtime-dispatch primitives use the selector itself — self-documenting, no naming convention to learn. Structural intrinsic names are unquoted identifiers; selector-based names are quoted atoms. The compiler distinguishes the two cases automatically by checking quotes: `'+'` = selector (runtime dispatch), `basicNew` = structural intrinsic.

5. ~~**Where's `yourself`?**~~ **RESOLVED: Add it.** `yourself => self` belongs in `stdlib/src/Object.bt`. It's pure Beamtalk (no pragma needed) and is the canonical Smalltalk "this is a real class library" signal. It also has practical use in message cascades. Will be included in the Phase 2 stdlib conversion.

---

## Amendment: Separate `@intrinsic` keyword (2026-02-11)

### Problem

The original design used **one keyword** (`@primitive`) with **two meanings**, distinguished only by quoting:

```beamtalk
size => @primitive 'size'       // selector-based: runtime dispatch
new  => @primitive basicNew     // structural intrinsic: compiler-generated code
```

In practice, stdlib has **187** quoted `@primitive 'x'` (runtime dispatch) and **25** unquoted `@primitive x` (compiler intrinsics). The visual difference is a pair of quotes — easy to miss, impossible to grep for intent, and confusing when writing new stdlib classes.

**Concrete confusion:** `@primitive 'printString'` and `@primitive printString` would mean entirely different things, but a stdlib author has no syntactic signal to indicate which is which.

### Decision

Introduce `@intrinsic` as a separate keyword for structural intrinsics. `@primitive` retains its meaning for selector-based runtime dispatch.

**Before (ambiguous):**
```beamtalk
+ other => @primitive '+'          // runtime dispatch
size => @primitive 'size'          // runtime dispatch
new => @primitive basicNew         // compiler intrinsic (looks the same!)
whileTrue: body => @primitive whileTrue  // compiler intrinsic
```

**After (explicit):**
```beamtalk
+ other => @primitive '+'          // runtime dispatch (unchanged)
size => @primitive 'size'          // runtime dispatch (unchanged)
new => @intrinsic basicNew         // compiler intrinsic (clearly different)
whileTrue: body => @intrinsic whileTrue  // compiler intrinsic (clearly different)
```

### Scope

The 25 structural intrinsic names that move from `@primitive` to `@intrinsic`:

| Category | Intrinsic Names |
|----------|----------------|
| **Object lifecycle** | `basicNew`, `basicNewWith`, `actorSpawn`, `actorSpawnWith` |
| **Block evaluation** | `blockValue`, `blockValue1`, `blockValue2`, `blockValue3` |
| **Control flow** | `whileTrue`, `whileFalse`, `repeat`, `timesRepeat`, `toDo`, `toByDo` |
| **Exception handling** | `onDo`, `ensure`, `error` |
| **Reflection** | `respondsTo`, `fieldNames`, `fieldAt`, `fieldAtPut`, `dynamicSend`, `dynamicSendWithArgs` |
| **Object protocol** | `printString`, `hash` |

All 187 quoted `@primitive 'selector'` uses are **unchanged**.

### Implementation

1. **Parser**: Add `@intrinsic` as a new pragma keyword, producing a distinct AST node (or a flag on `Primitive`)
2. **Codegen**: `PrimitiveBinding::StructuralIntrinsic` matches `@intrinsic`; `PrimitiveBinding::SelectorBased` matches `@primitive`
3. **Stdlib files**: Change 25 unquoted `@primitive x` → `@intrinsic x` across `stdlib/src/*.bt`
4. **Deprecation**: Unquoted `@primitive x` emits a warning for one release, then becomes an error

### Consequences

**Positive:**
- Unambiguous: you can grep `@intrinsic` vs `@primitive` and know the intent
- Stdlib authors don't need to remember quoting rules
- Follows Rust precedent: `#[rustc_intrinsic]` is distinct from `#[lang = "add"]`

**Negative:**
- One more keyword to learn (but only for stdlib maintainers)
- 25 lines change in `stdlib/src/*.bt`

**Neutral:**
- Selector-based primitives are always written as `@primitive 'selector'`; unquoted `@primitive x` is deprecated in favor of `@intrinsic x`, so there is no change for existing quoted `@primitive` usage.

---

## References

- [Squeak primitive pragmas](https://wiki.squeak.org/squeak/3336)
- [Gleam @external documentation](https://gleam.run/documentation/)
- [Swift Builtin module](https://forums.swift.org/t/standard-library-builtin-stuff/20512)
- [Elixir bootstrap source](https://github.com/elixir-lang/elixir/blob/main/lib/elixir/src/elixir_bootstrap.erl)
- [Rust lang items reference](https://doc.rust-lang.org/reference/items/lang-items.html)
- [Rust `Add` trait source (`library/core/src/ops/arith.rs`)](https://github.com/rust-lang/rust/blob/master/library/core/src/ops/arith.rs) — `#[lang = "add"]` + `add_impl!` macro
- [Rust intrinsics source (`library/core/src/intrinsics/mod.rs`)](https://github.com/rust-lang/rust/blob/master/library/core/src/intrinsics/mod.rs) — `#[rustc_intrinsic]` declarations
- Related issues: BT-224 (auto-loading stdlib), BT-221 (value type instantiation)
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (object model), [ADR 0006](0006-unified-method-dispatch.md) (method dispatch)
