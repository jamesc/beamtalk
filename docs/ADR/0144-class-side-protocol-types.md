# ADR 0144: Class-Side Protocol Types — `P class` Means "a Class Conforming to `P`"

## Status
Accepted (2026-11-05); proposed 2026-11-04. Driven by BT-4413.

## Context

### Problem statement

A common Smalltalk idiom passes a **class** around as a strategy and calls its
class-side methods. Catalina's transports are the clearest case in this
codebase. `TCPTransport` and `SSLTransport` are sealed classes with class
methods only. A `Socket` or `ServerConfig` holds one of the two classes and
calls `transport send: socket data: bytes` on it. The `Transport` protocol
already declares that contract with `class` requirements (BT-1611):

```beamtalk
Protocol define: Transport
  class listen: port :: Integer options: options :: List(Symbol | Tuple) -> Result(Port | Tuple, Object)
  class send: socket :: Object data: data :: Object -> Result(Nil, Object)
  class close: socket :: Object -> Result(Nil, Object)
  // ...
```

There is no type for the field that holds the transport. Both obvious spellings
fail (BT-4413, checked on main `a202f85`):

- `field: transport :: Transport class = TCPTransport` is rejected: `Type
  mismatch: state transport declared as Transport class, default is
  TCPTransport class`. `Transport class` resolves to the metatype of the
  *protocol object* (`Meta{Transport}`, ADR 0083), and metatype compatibility
  is by class name and superclass chain, so no class's metatype is ever
  compatible with it.
- `field: transport :: Transport = TCPTransport` passes the assignment, but
  every send to the field then reports `Transport does not understand
  'send:data:'` (and `setopts:options:`, `recv:length:timeout:`, `close:` and
  so on). `:: Transport` is an instance-side protocol type, so the protocol's
  `class` requirements are not in scope.

So Catalina declares these fields `:: Dynamic`. That means:

- **No checking.** A misspelled `transport sned: s data: d`, a wrong argument
  type or a class that does not implement the protocol is found at run time,
  as `does_not_understand`.
- **No typed results.** `result := self.transport send: ...` is `Dynamic`, so
  `result isOk` is a dynamic send too. In Catalina's echo path that is one of
  the three dynamic sends left per message after epic BT-4291 (BT-4389).

### Current state

What exists already:

- **Class-side protocol requirements** (BT-1611): `class sel` lines in a
  `Protocol define:` body. `conformsTo:` and the type checker's conformance
  check (`ProtocolRegistry::missing_conformance_selectors`) already require
  every `class` requirement to resolve on the class side
  (`ClassHierarchy::resolves_class_selector`). `TCPTransport conformsTo:
  #Transport` answers `true` today.
- **Metatypes** (ADR 0083): `C class` is a precise type (`Meta{C}`). A class
  literal infers as its metatype, and sends to a `Meta{C}` resolve against
  `C`'s class-side methods. Subtyping is `Meta{C} <: Class <: Behaviour <:
  Object`.
- **Structural protocol conformance for class-object receivers**: BT-2761 and
  BT-2784 made protocol-typed *parameters* of class methods check structural
  conformance, including for `Meta`-typed receivers.
- **Protocol types for instances** (ADR 0025, ADR 0068): `x :: P` accepts any
  value whose class conforms to `P` (structurally), and sends to it resolve
  against `P`'s instance-side requirements and provisions.

What is missing is the class-side mirror of the last item: a type that
accepts any class object whose class conforms to `P`, and resolves sends
against `P`'s `class` requirements.

### Constraints

- **Gradual typing** (ADR 0025): a new type must never make an untyped program
  fail; it only adds checking where the user writes it.
- **ADR 0068 metatypes are name-only**: there is no `List(E) class`. A
  protocol's type parameters must not leak into metatypes either.
- **Hot reload and open classes**: conformance can change at run time
  (extensions, live patches). Like every other static type, the new type is
  erased at run time and checks only what the compiler sees.
- **One conformance rule**: the type must reuse the existing structural
  conformance check, not a second list of "what conforms".

## Decision

### The rule

For a protocol `P`, the type annotation **`P class`** denotes **the class
object of any class that conforms to `P`**.

```text
C class <: P class      iff      C conformsTo: P      (structural, as today)
```

This is the class-side mirror of the existing instance rule, and the two read
the same way:

| Annotation | Denotes | Accepts |
|---|---|---|
| `C` (class) | an instance of `C` or a subclass | `C new`, … |
| `C class` (class) | the class object of `C` or a subclass (ADR 0083) | `C`, a subclass of `C` |
| `P` (protocol) | an instance of any class conforming to `P` | any conforming instance |
| **`P class` (protocol)** | **the class object of any class conforming to `P`** | **any conforming class** |

"Conforms" is exactly the existing check
(`ProtocolRegistry::check_conformance`): every instance-side selector of `P`
(required and provided) resolves on `C`'s instance side, and every `class`
requirement resolves on its class side. Since there is still only one
conformance rule, a class that `conformsTo: #Transport` at run time is exactly
a class the compiler accepts for `Transport class`, as far as the compiler can
see its methods.

### Sends to a `P class` value

A send to a value of type `P class` resolves, in order:

1. against `P`'s **`class` requirements** (and those of protocols `P`
   extends), using their declared parameter and return types;
2. then against the class-object protocol every class answers (`Class` →
   `Behaviour` → `Object`): `name`, `printString`, `respondsTo:`,
   `conformsTo:`, …, exactly as for any metatype (ADR 0083).

A selector found in neither is reported as not understood, at the same
severity as for a `P`-typed instance today. `new` and `basicNew` on a
`P class` value infer `P` (an instance of some conforming class), not
`Dynamic`, when `P` declares no `class new` requirement of its own, and stay
`Dynamic` otherwise, matching ADR 0083's rule for abstract classes.

```beamtalk
Object subclass: Socket
  field: socket :: Port | Tuple
  field: transport :: Transport class = TCPTransport

  send: data :: Object -> Result(Nil, Object) =>
    result := self.transport send: self.socket data: data   // checked, typed
    result isOk ifTrue: [self countSent: data]               // `isOk` on Result
    result
```

### Assignment and errors

Assigning a class that does not conform reports the missing selectors, using
the same wording the existing conformance warning uses:

```text
> field: transport :: Transport class = Integer
warning: Integer class does not conform to Transport class
  missing: class listen:options:, class accept:, class send:data:, …
```

A class literal infers `C class` (ADR 0083), so `TCPTransport` flows into a
`Transport class` field with no annotation at the use site. A `P class` value
flows into `:: Class` and `:: Behaviour` (it is a class object). The reverse,
`Class` into `P class`, is not provable and is reported like any other
narrowing assignment (narrow with `conformsTo:` or match).

Open-world rules are unchanged (ADR 0100): a class whose method surface the
checker cannot see completely (a cross-file superclass, `doesNotUnderstand:`,
a parse-error file) conforms by default, as it already does for `:: P`.

### Unions, `Nil` and generics

- `P class | Nil` works like any union: `transport isNil ifFalse: [...]`
  narrows to `P class`.
- `P class & Q class` is the class object of a class conforming to both
  (ADR 0102 intersection of the two sets).
- A **generic** protocol's metatype is name-only, as ADR 0068 already requires
  for classes: `Collection class` is valid, `Collection(E) class` is not. The
  class requirements are checked with the protocol's type parameters as
  `Dynamic`.

### What `P class` used to mean

Today `P class` for a protocol `P` resolves to the metatype of the
protocol *object* (`Meta{P}`), whose class-side methods are those of the
protocol's own class (`Protocol` reflection). No code in the stdlib, the
examples, the tests or any `jamesc/beamtalk-*` package writes `:: P class`
for a protocol `P` (checked 2026-11-04). The reflective use, a value that *is*
a protocol object, is spelled `:: Protocol` and is unaffected.

### Out of scope

- **Code generation.** The type is erased at run time. Sends to a `P class`
  value stay dynamic class-side sends; only their *results* become typed, and
  that typing feeds the existing guarded direct calls (ADR 0025 amendment,
  epic BT-4291), for example `result isOk` on a `Result`. Compiling the
  class-side send itself to a guarded direct call (keyed on the class-object
  tuple's module) is a separate decision for a later ADR 0025 amendment.
- **Run-time checking.** No assertion is emitted on assignment, as for every
  other static type (ADR 0025).

## Prior Art

| Language | Mechanism | What we take | What does not transfer |
|---|---|---|---|
| **Pharo / Squeak** | Classes are objects; class-side strategies are duck-typed (`aTransport send: ...`). | The idiom itself, unchanged. | No static checking at all. |
| **Newspeak** | Classes are first-class and structural. | Structural conformance. | No metatype annotations. |
| **Swift** | `P.Type` / `any P.Type` metatypes; protocols declare `static` requirements, and `T.Type` conforms when `T: P`. | The exact relation: a metatype conforms when its instance type conforms; static requirements are callable on the metatype. | Swift's existential/opaque distinction (`any`/`some`) is not needed under gradual typing. |
| **Kotlin** | A `companion object` can implement an interface, so the "class" passed as a value is that companion. | The goal: a class-level value satisfying an interface. | Needs a separate companion object; Beamtalk's metaclass already is that object. |
| **TypeScript** | Interfaces cannot declare static members; a separate "constructor interface" is written by hand. | The motivation: the hand-written workaround is the pain point. | Two declarations per contract; we keep one protocol. |
| **Rust** | Traits with associated functions; a type parameter `T: Transport` calls `T::send(..)`. | Associated functions belong to the contract. | Monomorphised generics, not values; nothing to store in a field. |
| **Elixir / Erlang** | `@behaviour Transport` and a module atom passed as a value (Thousand Island does exactly this). | The BEAM shape of the idiom: a module as a strategy. | Dialyzer types the field as `module()` and checks nothing about the behaviour. |

Swift is the closest: `P class` here is Swift's `any P.Type`, with structural
instead of nominal conformance.

## User Impact

- **Newcomer**: "a Transport class" reads as English, and the error names the
  missing class methods. It is discoverable from the existing `C class`
  annotation: the same suffix on a protocol name.
- **Smalltalk developer**: the class-as-strategy idiom stays exactly as in
  Pharo; nothing changes at run time and nothing new is required. Only an
  optional annotation gains a meaning.
- **Erlang/Elixir developer**: this is a checked `@behaviour`. A Beamtalk class
  passed in from Erlang as a class-object tuple types as `Dynamic` at the FFI
  boundary, as today, and can be narrowed.
- **Production operator**: no run-time change, no new code paths, nothing to
  observe. Indirectly, typed results let more sends compile to guarded direct
  calls.
- **Tooling developer**: the LSP can complete `transport ` with the protocol's
  class requirements and their signatures, and hover shows `Transport class`
  instead of `Dynamic`.

## Steelman Analysis

### A. `P class` means "a class conforming to `P`" (chosen)
- 🧑‍💻 **Newcomer**: "I already write `Actor class`; `Transport class` is the same thing for a protocol."
- 🎩 **Smalltalk purist**: "The metaclass already is the object that answers class-side messages, so typing it by the protocol it answers is just honest."
- ⚙️ **BEAM veteran**: "It's `@behaviour` with the checking Dialyzer never gave me."
- 🏭 **Operator**: "Erased at run time; nothing new can fail in production."
- 🎨 **Language designer**: "One rule, `C class <: P class` iff `C conformsTo: P`, mirrors the instance rule and reuses the one conformance check."

### B. A new spelling, `Class(Transport)`
- 🧑‍💻 **Newcomer**: "Looks like a generic, which I know from TypeScript and Java."
- 🎩 **Smalltalk purist**: "It leaves `X class` meaning exactly what ADR 0083 says, and never reinterprets an existing form."
- 🎨 **Language designer**: "It keeps protocol objects' own metatypes expressible."

### C. `:: P` also admits class objects when `P` has only class requirements
- 🧑‍💻 **Newcomer**: "No new syntax: `:: Transport` just works."
- 🎩 **Smalltalk purist**: "The most duck-typed of the options."

### D. Status quo (`Dynamic` plus annotated wrapper methods)
- 🏭 **Operator**: "No change to the type system is the safest change."
- ⚙️ **BEAM veteran**: "Elixir lives without this."

### Tension points
- B protects the current meaning of `P class`, but that meaning has no users
  and no useful sends, while B adds a second metatype spelling and a
  parameterised `Class` that ADR 0068 deliberately avoided.
- C is attractive until a protocol has both instance and class requirements:
  then `:: P` would silently change meaning depending on the protocol's shape.
- D leaves exactly the gap BT-4413 measured: no checking, no typed results.

## Alternatives Considered

### `Class(P)` — a parameterised `Class` type
Rejected. ADR 0068 makes metatypes name-only on purpose, and a parameterised
`Class` would be the first parameterised metatype. It also introduces a second
way to say "a metatype" (`C class` and `Class(C)`), and the only thing it
protects is the current meaning of `P class`, which nothing uses.

### `:: P` admits conforming class objects when `P` is class-only
Rejected. The meaning of `:: P` would depend on whether `P` happens to declare
any instance requirements, so adding one instance requirement to a protocol
would silently retype every `:: P` field from "a class" to "an instance". A
mixed protocol has no answer at all.

### Keep `Dynamic`, type results through wrapper methods
Rejected as the answer, kept as today's workaround. Wrapping each transport
call in an annotated method (`rawSend: data -> Result(Nil, Object) =>
self.transport send: ...`) types the result but checks nothing about the
transport, and costs a method per call.

### Nominal conformance (`uses: Transport` required)
Rejected. Every existing protocol type is structural (ADR 0068, BT-2784);
making the class-side one nominal would split the rule.

## Consequences

### Positive
- Class-as-strategy fields, parameters and returns become checkable: the
  selectors, argument types and return types of a transport are checked at
  every call site.
- Typed results feed the existing guarded direct calls: in Catalina,
  `result isOk` after a transport send becomes a direct call (one of the
  three dynamic sends per echo message, BT-4389).
- One conformance rule serves `conformsTo:`, `:: P` and `:: P class`.
- LSP completion and hover improve for class-valued fields.

### Negative
- `P class` for a protocol changes meaning. It has no users today, but a
  future program that wanted the protocol *object's* metatype must now spell
  it `Protocol class`.
- The checker's metatype compatibility gains a protocol case, so
  `is_assignable_to` and send resolution each get one more branch to keep
  consistent with the instance-side protocol rules.
- Like every static type, it can be wrong after hot reload: a class that loses
  a class method at run time still type-checks at sites compiled before.

### Neutral
- No run-time or code generation change.
- Generic protocols' metatypes are name-only, as for classes (ADR 0068).

## Implementation

All in `beamtalk-core`'s type checker; no codegen or runtime work.

1. **Resolution** (`type_checker/type_resolver.rs`): `DeclaredType::ClassOf(n)`
   where `n` names a protocol in the `ProtocolRegistry` resolves to a new
   protocol metatype, for example `InferredType::Meta` with a
   `protocol: true` marker or a sibling `ProtocolMeta { protocol_name }`
   variant; the variant shape is an implementation choice, the rule is the
   Decision's.
2. **Assignability** (`type_checker/validation.rs`, `is_assignable_to*`):
   `Meta{C}` → `P class` holds iff `ProtocolRegistry::check_conformance(C, P)`
   is `Ok`, with the ADR 0100 open-world gate it already applies. `P class`
   → `Class` / `Behaviour` / `Object` holds. Unions and intersections fall out
   of the existing union and intersection arms.
3. **Send resolution** (`type_checker/validation.rs` and `protocol.rs`): a
   receiver of type `P class` resolves selectors against
   `ProtocolInfo::all_class_requirements` first, then the metatype fallback
   chain ADR 0083 already uses; parameter and return types come from the
   requirement's declared signature.
4. **Diagnostics**: the conformance warning names the missing `class`
   selectors (the existing `missing_conformance_selectors` output already
   prefixes them with `class `).
5. **Catalina** (`jamesc/beamtalk-catalina`): `Socket`, `ServerConfig` and
   `HandlerConfig` change `transport :: Dynamic` to `transport :: Transport
   class`, closing BT-4413's typing half. Measure `echo_c10_64B` before and
   after.
6. **Docs**: `docs/beamtalk-language-features.md` § Metatypes and § Class
   Method Requirements gain the `P class` row and an example.

Tests: type-checker unit tests for each rule above (conforming and
non-conforming assignment, sends resolved from class requirements, the
`Class`/`Behaviour` fallback, `| Nil` narrowing, intersection, open-world
receivers), a BUnit or package-compiler fixture with a two-implementation
strategy, and an LSP completion test.

Size: M.

## Implementation Tracking

**Epic:** BT-4434
**Issues:** BT-4435 (type checker, M), BT-4436 (LSP and REPL e2e, S, blocked by BT-4435), BT-4437 (Catalina migration, S, blocked by BT-4435)
**Status:** Planned

## Migration Path

None required. No existing code writes `:: P class` for a protocol `P`, and
the change only adds checking where the annotation is written.

## References

- BT-4413 — Catalina per-message dynamic sends; the two failing spellings
- BT-4389 — the measurement that found the gap
- BT-1611 — class methods in protocol definitions
- BT-2761, BT-2784 — structural conformance for class-side parameters and
  `Meta` receivers
- ADR 0025 — gradual typing and protocols (and its 2026-10-29 amendment,
  guarded static dispatch)
- ADR 0068 — parametric types and protocols (name-only metatypes)
- ADR 0083 — metaclass-aware type inference (`C class`)
- ADR 0100 — open-world conformance rules
- ADR 0102 — set-theoretic type operators
- Swift: metatype types and `static` protocol requirements
