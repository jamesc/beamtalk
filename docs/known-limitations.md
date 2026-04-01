# Known Limitations (v0.1)

Beamtalk is under active development. This page documents features that are **not yet supported** along with workarounds where available.

## Language Features

### No Module or Import System

Beamtalk v0.1 uses a **flat global namespace**: all classes are globally visible with no `import`, `export`, or namespace syntax. This is an explicit design decision documented in [ADR 0031](ADR/0031-flat-namespace-for-v01.md).

- Class names must be unique across all loaded packages
- When two packages define the same class name, the second `:load` hot-reloads the class and emits a warning: `Class 'X' redefined (was old_module, now new_module)`
- ADR 0016 prevents Erlang-level module collisions under the hood via `bt@package@class` naming
- A module/import system is planned for v0.2 (see [BT-714](https://linear.app/beamtalk/issue/BT-714))

**Workaround:** Use distinctive, package-specific class names to avoid collisions (e.g. `MyAppCounter` instead of `Counter`).

**References:** [ADR 0031](ADR/0031-flat-namespace-for-v01.md), [BT-714](https://linear.app/beamtalk/issue/BT-714)

### No `try`/`catch` Keywords

Beamtalk uses Smalltalk-style block-based exception handling instead of `try`/`catch` keywords. The `on:do:` and `ensure:` messages are the idiomatic way to handle exceptions — this is a deliberate design choice, not a missing feature.

```beamtalk
[riskyOperation] on: Error do: [:e | e message]
[riskyOperation] ensure: [cleanup]
```

### No Macros or Compile-Time Metaprogramming

There is no macro system. All code runs at runtime.

### No `become:` or `thisContext`

These Smalltalk reflection features cannot be implemented on the BEAM VM ([ADR 0074](ADR/0074-deferred-metaprogramming.md)):
- **`become:`** — BEAM processes have separate heaps; object identity cannot be swapped. Use the proxy pattern (`doesNotUnderstand:` delegation) or registry pattern instead.
- **`thisContext`** — BEAM does not expose stack frames as first-class objects. Post-exception stack traces are available via `Exception stackTrace` returning `StackFrame` objects.

### Classes Are First-Class Values, Not Actors

Classes are first-class objects — you can store them in variables, send messages to them, and introspect them via the full metaclass tower (ADR 0036). However, class objects are not actors: they are not supervised, have no `state:` declarations, and do not participate in OTP lifecycle. The backing `beamtalk_object_class` gen_server processes are runtime infrastructure, not user-visible actors. See [ADR 0074](ADR/0074-deferred-metaprogramming.md) for rationale.

```beamtalk
cls := Beamtalk classNamed: #Counter
cls spawn           // send messages to class objects
cls methods         // introspect via metaclass protocol
cls superclass      // => Actor
cls class           // => Counter class (metaclass object)
```

## Concurrency

### No Async Combinators

`Future all:` and `Future any:` are not yet implemented. You cannot wait on multiple futures simultaneously.

**Workaround:** Await futures sequentially.

**Tracking:** [BT-507](https://linear.app/beamtalk/issue/BT-507)

### Supervision API Is Low-Level

The `Supervisor` and `DynamicSupervisor` classes provide programmatic access to OTP supervision trees, but there is no declarative DSL for defining supervision hierarchies or restart strategies inline with class definitions.

**Current API:**
```beamtalk
sup := Supervisor start: {
  SupervisionSpec strategy: #one_for_one children: {
    SupervisionSpec child: MyWorker id: "worker1"
  }
}
```

## Type System

### Types Are Optional Warnings Only

Gradual typing (ADR 0025) is in Phase 1. Type annotations are parsed and checked, but:
- Type errors produce **warnings**, never compile errors
- Union and generic type annotations are not yet checked
- Dialyzer spec generation is partial

This is by design — types are documentation-first in Beamtalk.

## Standard Library Gaps

Most common utilities are now in the stdlib: `Regex`, `DateTime`, `JSON`, `Integer`/`Float` math (sqrt, trig, log), `System` (environment variables), `HTTPClient`, `Supervisor`, `DynamicSupervisor`.

## Pattern Matching

Binary pattern matching is not yet implemented.

**Tracking:** [BT-663](https://linear.app/beamtalk/issue/BT-663)
