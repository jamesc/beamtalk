# ADR 0071: Class Visibility — `internal` Modifier for Package-Scoped Access Control

## Status
Proposed (2026-03-28)

## Context

ADR 0070 shipped the package system with all classes public — Smalltalk tradition. Section 9 of that ADR explicitly calls for a follow-up on visibility:

> Without visibility, I cannot draw a stable API boundary. Any class I ship becomes part of my API the moment a user references it — even `InternalJsonParserState`.

The problem is concrete: library authors cannot hide implementation-detail classes. Every class in a package is part of its public API, constraining internal refactoring. Renaming or removing an internal helper is a breaking change for any downstream package that happened to reference it.

### Current State

- One class per file, each class compiles to one BEAM module (ADR 0016: `bt@{package}@{class}`)
- Package is the visibility boundary (ADR 0070)
- Existing class modifiers: `sealed`, `abstract`, `typed` — all stackable, parsed before the superclass name
- Method-level modifier: `sealed` (slated for removal per ADR 0049)
- No visibility control exists today

### Constraints

1. **BEAM has no module-level access control** — any loaded module is callable by any process via `apply/3`. Runtime enforcement of visibility is impossible without owning the VM.
2. **Smalltalk heritage** — everything-is-public is a core tradition. Any departure must be justified.
3. **One class per file** — "module-private" and "file-private" are the same concept, which is different from "package-private."
4. **Keyword reservation** — choosing the right word now avoids burning a name that's needed later for a different concept.

## Decision

### 1. The `internal` Keyword

A new class modifier `internal` marks a class as visible only within its own package. Other packages cannot reference it, even if they declare a dependency.

```beamtalk
// Public (default) — available to any package that depends on this one
Value subclass: Parser
  parse: input => ...

// Package-internal — only visible within this package
internal Value subclass: ParserState
  // Implementation detail, not part of the public API
  buffer: state: buf :: String = ""
  position: state: pos :: Integer = 0
```

**Why `internal`, not `private`?** The word `private` carries strong expectations from Java, C#, Ruby, and Smalltalk — it means "only this class/object can see it." Using `private` for package-level visibility would:

- Confuse developers from those languages
- Burn the most natural keyword for future object-level encapsulation (`private` = self-only, `protected` = hierarchy-visible)

`internal` honestly describes what it is: visible within this package, not exported. It matches C# and Kotlin's terminology for the same concept. This reserves `private` and `protected` for potential future use if object-level encapsulation is ever needed.

### 2. Public by Default

All classes are public unless marked `internal`. This preserves Smalltalk's "everything is accessible" tradition for the common case while giving library authors an opt-out for implementation details.

### 3. Class-Level Only (v1)

Method-level `internal` is deferred. In v1, visibility applies only to whole classes. This covers the primary use case (hiding implementation-detail classes) while keeping the design simple.

If a public class has helper methods that shouldn't be called externally, the current approach is to extract those helpers into an `internal` class.

### 4. Compile-Time Enforcement

The Beamtalk compiler emits a hard error when code in one package references an `internal` class from another package:

```text
error[E0401]: Class 'ParserState' is internal to package 'json' and cannot be referenced from 'my_app'
  --> src/app.bt:5:12
   |
 5 |     state := json@ParserState new
   |              ^^^^^^^^^^^^^^^^
   |
   = note: 'ParserState' is declared 'internal' in package 'json'
   = help: use the package's public API instead
```

This is compile-time only. The BEAM runtime cannot enforce module-level access control — `apply/3` can call any loaded module. This is the same trade-off as:

- **Rust**: `pub`/private is compile-time; `unsafe` can read any memory
- **Go**: unexported identifiers enforced by compiler; `reflect` can read unexported fields
- **Erlang**: `-export()` is compile-time; `apply(Mod, Fun, Args)` works on anything
- **Java**: `private` enforced by compiler and verifier; `setAccessible(true)` bypasses it

If someone crafts raw Erlang calls to an internal class's BEAM module, they've voided the warranty.

### 5. REPL Respects Visibility

The REPL compiles every expression through the same compiler pipeline. Since `internal` is enforced at compile time, the REPL automatically respects it — no special case needed.

```text
beamtalk> json@ParserState new
error[E0401]: Class 'ParserState' is internal to package 'json'
  = help: use the package's public API instead

beamtalk> json@Parser parse: '{"a": 1}'
// => Dictionary("a" -> 1)
```

In v1, there is no escape hatch. A future version may add a `--privileged` REPL mode for production debugging, with an option for production deploys to disable it entirely:

```toml
# Future: beamtalk.toml on the production node
[repl]
allow-privileged = false   # no escape hatch, period
```

This is deferred until remote REPL attach lands, since that's when the production vs. development distinction actually matters.

### 6. Modifier Stacking

`internal` composes with existing modifiers. All combinations are valid:

```beamtalk
// Internal abstract base class — must be subclassed within the package
internal abstract Object subclass: InternalBase

// Internal sealed — cannot be subclassed even within the package
internal sealed Object subclass: CacheImpl

// Internal typed — requires type annotations, package-private
internal typed Object subclass: TypedHelper

// All three
internal abstract typed Object subclass: StrictInternalBase
```

`internal sealed` is valid but often redundant — if a class can't be seen from outside the package, it can't be subclassed from outside either. The combination is useful when you also want to prevent intra-package subclassing.

### 7. Metadata

The `__beamtalk_meta/0` function gains a `visibility` field:

```erlang
#{
  'class' => 'ParserState',
  'superclass' => 'Value',
  'visibility' => 'internal',    % or 'public'
  %% ... existing fields unchanged
}
```

The `.app` class registry also carries visibility:

```erlang
{env, [
  {classes, [
    #{name => 'Parser', visibility => public, ...},
    #{name => 'ParserState', visibility => internal, ...}
  ]}
]}
```

This enables tooling (LSP, MCP, REPL) to filter internal classes from cross-package completions, and provides the data for future Erlang→Beamtalk FFI enforcement.

### 8. Future Erlang→Beamtalk FFI Enforcement

There is currently no clean FFI from Erlang to Beamtalk. When one is built, it can enforce visibility at **Erlang compile time** by excluding internal classes from generated API wrappers:

```erlang
% Auto-generated wrapper for package 'json' — only public classes appear
% ParserState is internal, so no wrapper is generated for it
json_api:parser_parse(Input)   % works — Parser is public
% json_api:parser_state_new()  % doesn't exist — ParserState is internal
```

The enforcement stack:

| Call path | Enforcement | When |
|-----------|-------------|------|
| Beamtalk → Beamtalk | Beamtalk compiler | Compile time |
| Erlang → Beamtalk via FFI | Generated API excludes internal classes | Erlang compile time (future) |
| Raw `apply/3` to BEAM module | None | Voided warranty |

All compile-time, all zero runtime cost. The only escape is deliberately circumventing the toolchain.

## Prior Art

### Go — Case Convention, Package Boundary

Go uses uppercase/lowercase for exported/unexported. The package is the only visibility boundary — no `protected`, no hierarchy-based access. Go has been wildly successful with just two visibility levels.

**What we adopt:** Package as the sole visibility boundary. Binary public/internal. No `protected`.

**What we differ on:** Go's case convention doesn't work for Beamtalk — class names are always uppercase. We use an explicit keyword instead.

### Newspeak — Capability Security via Lexical Scoping

Newspeak has no global namespace. Everything is accessed through lexical scope — if a class isn't nested inside your class or passed as a parameter, it doesn't exist. This enables the principle of least authority: untrusted code can only access what it's explicitly given.

**What we learn:** The most secure visibility model, but it requires owning the entire runtime. On BEAM, any process can call any module — Newspeak-style encapsulation would be language-level fiction. Beamtalk chooses honest compile-time enforcement over a security guarantee it can't actually provide on BEAM.

### C# / Kotlin — `internal` Keyword

C# uses `internal` for assembly-scoped visibility. Kotlin uses `internal` for module-scoped. Both are compile-time enforced with reflection as the escape hatch.

**What we adopt:** The `internal` keyword and its semantics. Well-understood by industry developers. Leaves `private`/`protected` available for potential future object-level encapsulation.

### Rust — `pub` / Private by Default

Rust defaults to private, with `pub`, `pub(crate)`, and `pub(super)` for broader visibility. All compile-time. `unsafe` can bypass anything.

**What we differ on:** Beamtalk defaults to public (Smalltalk tradition). `internal` is the opt-out, not the default. This reduces ceremony for the common case.

### Erlang — `-export()` Declarations

Erlang modules export specific functions. Unexported functions are callable only within the module. However, `apply/3` bypasses this at runtime.

**What we learn:** BEAM's own visibility is already compile-time convention, not runtime enforcement. Our `internal` modifier follows the same pattern at a higher level (package, not module).

### Smalltalk (Pharo/Squeak) — No Enforced Visibility

Smalltalk has no visibility enforcement. Everything is public. `private` is a method category convention, not a language feature. This works in a single-image development model but breaks down for distributed packages — there's no stable API boundary, and Pharo's package ecosystem suffers from accidental coupling to implementation details.

**What we depart from:** Adding `internal` is an explicit departure from Smalltalk tradition, justified by the distributed package ecosystem that Beamtalk targets. The all-public model works when everyone develops in the same image; it doesn't work when packages are versioned, published, and depended upon by strangers.

## User Impact

### Newcomer (from Python/JS/Ruby)

- `internal` is familiar from C#/Kotlin/TypeScript. No surprise.
- Modifier syntax matches existing `sealed`/`abstract` — consistent to learn.
- Error messages clearly explain the violation and suggest using the public API.

### Smalltalk Developer

- Departure from "everything is public" tradition.
- Justified by package ecosystem needs — Pharo developers know the pain of accidental coupling.
- The default is still public — Smalltalk ergonomics preserved for the common case.

### Erlang/BEAM Developer

- Maps naturally to `-export()` concept at the package level.
- Understands compile-time enforcement and BEAM's escape hatches.
- No runtime cost — comfortable trade-off.

### Production Operator

- Cleaner API surfaces mean fewer accidental dependencies on internals.
- Would like future `--privileged` lockout for production REPLs.
- No performance impact — metadata is a compile-time constant.

### Tooling Developer (LSP, MCP)

- `__beamtalk_meta/0` `visibility` field enables filtering internal classes from cross-package completions.
- LSP can show "internal to package json" in hover tooltips.
- Static analysis benefits from explicit visibility declarations.

## Steelman Analysis

### Alternative: `private` Keyword Instead of `internal`

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "Every language I know uses `private`. I'd guess `private` without reading docs. `internal` is a C# term that most developers haven't encountered." |
| **Smalltalk purist** | "If we're going to depart from Smalltalk's all-public tradition, at least use the universally understood term. `private` is the most widely recognized visibility keyword in programming." |
| **Language designer** | "We may never need object-level `private`. Smalltalk survived 40 years without it. Reserving the keyword for a hypothetical future feature is over-engineering the naming." |

**Counter-argument:** The risk is real. If Beamtalk ever needs self-only method visibility, `private` is the only natural keyword. Renaming a visibility modifier after packages are published is a breaking change that affects every library. `internal` is slightly less familiar but precisely correct, and the cost of learning one keyword is far lower than the cost of a future keyword collision.

### Alternative: Method-Level `internal` in v1

| Cohort | Strongest argument |
|--------|-------------------|
| **Library author** | "I have a public class `Parser` with helper methods that callers shouldn't use. Without method-level internal, I must extract helpers into a separate internal class — increasing file count and indirection for a simple visibility concern." |
| **Erlang/BEAM veteran** | "Erlang's `-export()` is function-level, not module-level. Method-level visibility is the natural BEAM model." |

**Counter-argument:** Class-level `internal` covers the primary use case. Method-level adds complexity to dispatch, tooling, and the FFI story. Better to observe real usage patterns before designing method-level visibility. If extracting helpers into internal classes proves too painful, method-level `internal` can be added as a non-breaking enhancement.

### Alternative: REPL Always Exempt from Visibility

| Cohort | Strongest argument |
|--------|-------------------|
| **Smalltalk purist** | "The REPL is the heart of Smalltalk development. 'All objects are inspectable' is sacred. A REPL that refuses to show you something is broken by definition." |
| **Production debugger** | "When the system is on fire at 3am, I need to inspect everything. Visibility rules that prevent debugging are a liability, not a feature." |

**Counter-argument:** The REPL goes through the compiler, so enforcement is free — making it exempt would require *adding* special-case code. For v1, the simpler implementation is also the more correct one. The future `--privileged` flag provides the debugging escape hatch when remote REPL attach lands.

### Tension Points

- **Newcomers** prefer `private`; **language designers** prefer `internal` for future-proofing.
- **Smalltalk purists** want REPL exemption; **operators** want production boundary enforcement.
- **Library authors** want method-level visibility now; **pragmatists** want to ship class-level first and iterate.

## Alternatives Considered

### Alternative: `private` Keyword

```beamtalk
private Object subclass: ParserState
```

**Rejected because:** Burns the most natural keyword for future object-level encapsulation. If Beamtalk ever needs "only self can call this method," `private` is the only word that works. `internal` is precise and available. See Steelman Analysis above.

### Alternative: Convention-Based Visibility (Underscore Prefix)

```beamtalk
// Convention: underscore prefix means internal
Object subclass: _ParserState
```

**Rejected because:** Conventions are unenforceable. The compiler can't distinguish "oops, I used an internal class" from "I know what I'm doing." The whole point is compile-time enforcement for stable API boundaries. Python's `_` prefix convention is widely acknowledged as insufficient for published packages.

### Alternative: Method-Level `internal` in v1

```beamtalk
Object subclass: Parser
  internal helper: input => ...
  parse: input => self helper: input
```

**Deferred, not rejected.** Method-level visibility adds complexity to dispatch, FFI, and tooling. Class-level covers the primary use case. If real usage shows that extracting helpers into internal classes is too painful, method-level `internal` can be added as a non-breaking enhancement in a future ADR.

### Alternative: Explicit `public` Keyword (Private by Default)

```beamtalk
// Would require 'public' on every exported class
public Object subclass: Parser
Object subclass: ParserState  // private by default
```

**Rejected because:** Contradicts Smalltalk heritage. Adds ceremony to every public class. The common case (public) should be the default, requiring zero annotation. Rust and Kotlin chose private-by-default; Beamtalk chooses public-by-default, consistent with Smalltalk, Go (for exported names), and ADR 0070's design.

## Consequences

### Positive

- **Stable API boundaries** — library authors can refactor internal classes without breaking downstream packages. Internal class changes become patch releases, not major version bumps.
- **Zero runtime cost** — compile-time enforcement only. Metadata is a compile-time constant in `__beamtalk_meta/0`.
- **Composes cleanly** with existing modifiers (`sealed`, `abstract`, `typed`). Same parsing pattern, same stacking behavior.
- **Reserves `private`/`protected`** for potential future object-level encapsulation.
- **Enables tooling** — `visibility` field in metadata lets LSP filter completions, MCP filter class listings, and future FFI exclude internal classes from generated APIs.
- **Consistent enforcement model** — REPL goes through compiler, so visibility is automatically enforced without special-case code.

### Negative

- **Departure from Smalltalk tradition** — "everything is public" is a core Smalltalk value. This adds a concept that Pharo and Squeak don't have.
- **Compile-time only** — raw Erlang calls via `apply/3` bypass visibility. This is the BEAM's fundamental nature, shared by every visibility mechanism on the platform.
- **No method-level `internal` in v1** — helper methods on public classes remain exposed across packages. Workaround: extract into internal classes.
- **New keyword to learn** — though `internal` is well-known from C#/Kotlin.

### Neutral

- Existing single-package code requires no changes (public is the default).
- No runtime cost — `visibility` is a static field in `__beamtalk_meta/0`, same as `is_sealed` and `is_abstract`.
- REPL behavior is consistent with compiled code — no special exemption logic.

## Implementation

### Phase 1: Parser and AST

1. Add `internal` to the modifier parsing loop in `parse_class_definition()` (`declarations.rs`, line 77)
2. Add `is_internal: bool` to `ClassDefinition` (`ast.rs`, line 339)
3. Update `is_at_method_definition()` lookahead to skip `internal` (`declarations.rs`, line 373)

### Phase 2: Semantic Analysis

1. Add `is_internal: bool` to `ClassInfo` (`class_hierarchy/mod.rs`, line 105)
2. Add `check_internal_visibility()` — when resolving a class reference, if the target class is `internal` and belongs to a different package, emit `E0401`
3. Requires package context during compilation — the compiler must know which package owns each class (provided by ADR 0070's dependency resolution)

### Phase 3: Metadata and Codegen

1. Add `'visibility' => 'public' | 'internal'` to `build_meta_map_doc()` (`methods.rs`, line 2419)
2. Extend `.app` class registry to include `visibility` field
3. Propagate `is_internal` through `ClassIdentity` in codegen (`util.rs`, line 76)

### Phase 4: Tooling

1. LSP: filter `internal` classes from cross-package completions and imports
2. REPL: no special work needed (compiler enforcement applies automatically)
3. MCP: `package_classes` tool respects visibility — option to include/exclude internal classes
4. `:help` and `:describe` show visibility in class info

## Migration Path

No migration needed. All existing classes are public by default — this is a purely additive feature. Library authors opt in by adding `internal` to implementation-detail classes.

## References

- Related issues:
  - [BT-714](https://linear.app/beamtalk/issue/BT-714) — Design user-facing module/namespace syntax
  - [BT-716](https://linear.app/beamtalk/issue/BT-716) — Class visibility modifier
- Related ADRs:
  - [ADR 0016](0016-unified-stdlib-module-naming.md) — Universal Module Naming with `@` Separator
  - [ADR 0031](0031-flat-namespace-for-v01.md) — Flat Namespace for v0.1
  - [ADR 0049](0049-remove-method-level-sealed.md) — Remove Method-Level `sealed`
  - [ADR 0070](0070-package-namespaces-and-dependencies.md) — Package Namespaces and Dependencies (Section 9: Visibility)
