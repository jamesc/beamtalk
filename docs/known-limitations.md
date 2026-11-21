# Known Limitations

Beamtalk is under active development. This page documents features that are **not yet supported** along with workarounds where available.

## Language Features

### Flat Namespace Within a Package

Beamtalk has a package namespace and dependency system ([ADR 0070](ADR/0070-package-namespaces-and-dependencies.md)): declare dependencies in `beamtalk.toml` (registry, `path` or `git`), reference a dependency's classes by their short name, and disambiguate with qualified `package@Class` names when needed. Class-name collisions between dependencies are a **compile error** (not silent shadowing) — resolve them with the qualified name. There are no per-file `import`/`export` statements: within a package (and the REPL workspace) all classes share a flat namespace by design.

Registry dependencies pin an exact `major.minor.patch` version; version ranges are not supported (use a `git` dependency for a moving target).

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

### Supervision Trees Are Declared by Subclassing

`Supervisor` and `DynamicSupervisor` give programmatic access to OTP supervision trees. A static tree is a `Supervisor` subclass whose `childSpecs` (and optionally `strategy`, `maxRestarts`, `restartWindow`) are instance-side methods; there is no separate declarative DSL or inline restart-strategy syntax on actor classes (ADR 0059, ADR 0135).

```beamtalk
Supervisor subclass: WebApp
  childSpecs => #(DatabasePool HTTPRouter MetricsCollector)

app := (WebApp supervise) unwrap
```

## Type System

### Type Errors Are Warnings

Gradual typing (ADR 0025, ADR 0068) checks annotations, `typed` classes, unions, generics and narrowing, but a type problem is a **warning**, never a compile error. To make a build fail on warnings, pass `--warnings-as-errors` (`beamtalk build`, `check`, `test`); to accept a known one, use `@expect`.

This is by design — types are documentation-first in Beamtalk.

## Standard Library Gaps

Most common utilities are now in the stdlib: `Regex`, `DateTime`, `Duration`, `JSON`, `Integer`/`Float` math (sqrt, trig, log), `Digest` (hashing/HMAC), `System` (environment variables), `Supervisor`, `DynamicSupervisor`, `RetryPolicy`, `Parallel`. HTTP is **not** in stdlib — `HTTPClient`/`HTTPServer` live in the separate [`beamtalk-http`](https://github.com/jamesc/beamtalk-http) package (add it as a dependency; see the [Package Management guide](beamtalk-packages.md)).
