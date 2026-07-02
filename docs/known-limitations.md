# Known Limitations

Beamtalk is under active development. This page documents features that are **not yet supported** along with workarounds where available.

## Language Features

### Package Dependencies: Path and Git Only (No Registry Yet)

Beamtalk has a package namespace and dependency system ([ADR 0070](ADR/0070-package-namespaces-and-dependencies.md)): declare dependencies in `beamtalk.toml`, reference a dependency's classes by their short name, and disambiguate with qualified `package@Class` names when needed. Class-name collisions between dependencies are a **compile error** (not silent shadowing) — resolve them with the qualified name. There are no per-file `import`/`export` statements: within a package (and the REPL workspace) all classes share a flat namespace by design.

The remaining gap is the **source** of dependencies: only `path` (local) and `git` dependencies are supported. Registry-based resolution (e.g. `json = "1.0"` via a Hex.pm-style registry) is deferred until the ecosystem warrants it.

**Workaround:** Vendor a package via a `git` or `path` dependency instead of a registry name.

**References:** [ADR 0070](ADR/0070-package-namespaces-and-dependencies.md) (package namespaces + dependencies), [ADR 0031](ADR/0031-flat-namespace-for-v01.md) (superseded — the original v0.1 flat-namespace decision), [Package Management guide](beamtalk-packages.md)

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
- **`thisContext`** — BEAM does not expose stack frames as first-class objects. Post-exception stack traces are available via `e stackTrace` on caught exception objects, returning `StackFrame` values.

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
