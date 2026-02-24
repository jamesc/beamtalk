# ADR 0031: Flat Namespace for v0.1

## Status
Implemented (2026-02-18)

## Context

Beamtalk currently has a **flat global namespace**: each class is a named process registered in the Erlang process registry (with lookup utilities in `beamtalk_class_registry`) and visible to all code. There is no `import`, `export`, or namespace syntax. Every class name must be globally unique.

This works today because:
- The stdlib uses distinctive names (`Integer`, `Dictionary`, `Actor`) that users are unlikely to collide with
- ADR 0016 provides internal BEAM module naming (`bt@package@module`) that prevents Erlang-level collisions between packages at compile time
- ADR 0026 defines a `beamtalk.toml` manifest with a package name that feeds into the `bt@{package}@{module}` naming scheme
- Most Beamtalk projects are single-file or small multi-file REPL explorations

However, BT-714 identifies this as the **biggest gap for users building beyond a single file**: no way to organize large projects, no visibility control, no import/export, and potential name collisions between user code and third-party packages.

The question is: **does v0.1 need a module system, or can we ship with the flat namespace and design imports properly for v0.2?**

### Constraints

1. **Interactive-first** (Principle 1): Any module system must work in the REPL, not just batch compilation. Users must be able to `:load` a file and immediately use its classes.
2. **Code lives in files** (Principle 5): One `.bt` file per class. The filesystem provides natural organization.
3. **BEAM-native** (Principle 9): Must map cleanly to BEAM modules. Erlang has a flat module namespace too — modules are atoms.
4. **Smalltalk heritage**: Pharo, the most widely-used Smalltalk, still uses a flat namespace with naming conventions. Newspeak introduced nested classes as modules, but this was a radical departure that most Smalltalks haven't followed.
5. **Language maturity**: Beamtalk's class system, inheritance, actors, and gradual typing are all working but still evolving. Building a module system on shifting foundations risks designing the wrong thing.

## Decision

**v0.1 ships with a flat global namespace.** All classes in a package are globally visible. There is no import, export, or namespace syntax.

This is an **explicit design decision**, not an oversight:

1. **Flat namespace is the v0.1 scope.** Class names must be unique across a package and its dependencies.
2. **ADR 0016 naming prevents Erlang-level collisions.** Two packages can both define a `Counter` class — they compile to `bt@my_app@counter` and `bt@other_app@counter` respectively. The Beamtalk user sees `Counter`; the BEAM sees distinct modules.
3. **Name collision within a workspace is silent.** If two loaded packages both define `Counter`, the second definition hot-reloads over the first via `update_class` — no warning is emitted. This is a consequence of the `register_class/0` on-load codegen, which calls `beamtalk_object_class:start` and falls through to `update_class` on `already_started`. See Implementation item 4 for planned collision warnings.
4. **v0.2 will introduce a module/import system.** The specific design (package-scoped imports vs. nested classes) is deferred until the language has more real-world usage to inform the decision. See Alternatives Considered below for the leading candidates.

### What v0.1 provides

```beamtalk
// All classes are globally visible — no import needed
Object subclass: Counter
  count => count
  increment => count := count + 1


// Reference any class by name
c := Counter new
c increment
c count
// => 1
```

### What happens on collision

```
// Loading a second package that also defines Counter:
:load other-package/counter.bt
// Currently: silently hot-reloads Counter with no warning
// Planned (v0.1): emit a warning like:
// ⚠️ Warning: Class 'Counter' redefined (was from my-app, now from other-package)
```

### What v0.1 does NOT provide

```beamtalk
// ❌ None of these exist in v0.1:
import json                        // No import syntax
import json.{JSON, JSONError}      // No selective imports
from "other-package" import Counter // No cross-package imports
namespace MyApp [...]              // No namespace blocks
```

## Prior Art

### Pharo Smalltalk — Flat namespace with conventions

Pharo uses a **single global namespace** (`Smalltalk` / `SystemDictionary`). All classes must have unique names. Organization is via *packages* (grouping for tools/browsers) and *naming conventions* (two-letter prefixes like `Mc` for Monticello). Packages do not create namespaces — they are metadata for tooling.

**What we adopt:** The flat namespace model. It works for Pharo's ecosystem of thousands of packages because class names tend to be descriptive and unique. Beamtalk's ecosystem is tiny by comparison.

**What we note:** Pharo has been discussing adding namespaces for years (pharo-project/pharo#13563) but hasn't shipped them. The flat model is surprisingly durable.

### Gleam — File-based modules with explicit imports

Each `.gleam` file is a module. Module name derives from file path (`src/utils/math.gleam` → `utils/math`). Cross-module access requires `import`:

```gleam
import gleam/io
pub fn main() { io.println("hello") }
```

Functions are private by default; `pub` makes them public. The BEAM module name uses `@` separator: `gleam@io`.

**What we'd adapt for v0.2:** Gleam's model maps well to Beamtalk. ADR 0016 already uses the same `@` separator. ADR 0026 already defines file-path-to-module mapping. The missing piece is user-facing `import` syntax.

### Newspeak — Nested classes as modules

Newspeak eliminates the global namespace entirely. Top-level classes serve as modules; nested classes provide namespace scoping. All dependencies are injected via constructors — there are no imports, no globals.

```newspeak
class DrawingModule = (
  class Shape = (...)
  class Circle = Shape (...)
)
```

**What we note:** This is philosophically beautiful and aligns with message-passing purity. However, it requires a fundamentally different class system (virtual nested classes, constructor-based dependency injection) and makes REPL exploration harder — you can't just type `Counter new` without first having a module instance.

### Elixir — Multi-keyword module system

Elixir provides four keywords: `alias` (shorten names), `import` (bring functions into scope), `require` (load macros), `use` (code injection). Modules use dot notation (`MyApp.Repo.User`).

**What we reject:** The four-keyword model is over-engineered for Beamtalk. Beamtalk has no macros (so no `require`), no `use`-style metaprogramming, and `alias` is just syntactic sugar. A single `import` keyword would suffice.

### Erlang — Flat with conventions

Erlang has a flat module namespace. Modules are atoms. The convention is `app_module` naming (e.g., `cowboy_handler`). No import syntax — all calls are fully qualified (`module:function(args)`).

**What we note:** Beamtalk is already more structured than Erlang thanks to ADR 0016 naming. The flat-at-language-level model mirrors Erlang's approach.

## User Impact

### Newcomer (from Python/JS/Ruby)
- **v0.1:** Surprised by lack of imports — "how do I use a library?" Answer: just use the class name, it's global.
- **Upside:** Zero ceremony. No `import` boilerplate. Type the class name, it works.
- **Downside:** May worry about name collisions. Documentation must explain the convention clearly.

### Smalltalk developer
- **v0.1:** Feels like home. Pharo works exactly this way. "Of course classes are global — where else would they be?"
- **Comfortable** with the flat model. May resist adding imports in v0.2.

### Erlang/Elixir developer
- **v0.1:** Understands flat namespaces from Erlang. Slightly uneasy without explicit imports — "how do I know where `Counter` comes from?"
- **Reassured** by ADR 0016 naming under the hood. Would appreciate imports in v0.2.

### Production operator
- **v0.1:** Concerned about name collisions between dependencies at scale. Acceptable for early adoption; would want imports before production use with many third-party packages.

## Steelman Analysis

### Option B: Package-Scoped Visibility with Minimal Imports

| Cohort | Strongest argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "Every modern language has imports. Without them, I can't tell where a class comes from when reading code. `import json` is one line — the ceremony is worth the clarity." |
| 🎩 **Smalltalk purist** | "Even Pharo has package-level organization. An `import` at file top is less magic than implicit global visibility. At least I know what my dependencies are." |
| ⚙️ **BEAM veteran** | "Erlang requires `module:function()` qualification. Gleam requires `import`. Flat globals are the exception on BEAM, not the rule. Ship imports now before the ecosystem grows and migration becomes painful." |
| 🏭 **Operator** | "Name collisions in production are silent bugs. If two deps define `Logger`, the last one loaded wins — that's terrifying. Explicit imports prevent this class of error entirely." |
| 🎨 **Language designer** | "ADR 0016 and 0026 already define the module naming. The `import` keyword is the last missing piece. It's 200 lines of parser + codegen — why defer what's already designed?" |

**Weakness:** Adds syntax surface area before we know the right design. Import semantics interact with hot code reloading, REPL `:load`, workspace bindings, and package resolution — all still evolving. Getting imports wrong is worse than not having them.

### Option C: Newspeak-Style Nested Classes as Modules

| Cohort | Strongest argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "Modules as classes means I only need to learn one concept. A module IS a class. That's elegant and minimal." |
| 🎩 **Smalltalk purist** | "This is the most Smalltalk-aligned option. Everything is an object, everything is a message. Newspeak proved this works. It's the logical evolution of the Smalltalk module system." |
| ⚙️ **BEAM veteran** | "Nested classes map to nested BEAM modules. Constructor injection is basically OTP application config. This is dependency injection done right on the BEAM." |
| 🏭 **Operator** | "No globals means no hidden coupling. Every dependency is explicit in the constructor. I can trace every class reference in the system. This is the most auditable option." |
| 🎨 **Language designer** | "Virtual nested classes enable framework extension patterns that are impossible with flat imports. You can override a single class inside a module without forking the whole thing. This is genuinely novel on BEAM." |

**Weakness:** Requires fundamental changes to the class system (nested class definitions, virtual classes, constructor-based DI). Breaks REPL ergonomics — you can't `Counter new` without first instantiating the module. Months of work before any user benefit. The language features it depends on (nested classes, class parameterization) don't exist yet.

### Tension Points

- **Newcomers and BEAM veterans** prefer Option B — familiar, pragmatic, ships fast.
- **Smalltalk purists and language designers** are drawn to Option C — elegant, powerful, novel.
- **Operators** split: B gives them explicit imports now; C gives them no-globals auditing later.
- **Everyone agrees** Option A is acceptable for v0.1 if it's explicitly temporary with a clear path forward.
- The strongest argument *against* shipping A is: **ecosystem lock-in.** If packages proliferate without imports, adding them later requires migrating every package. Counter-argument: the ecosystem is tiny in v0.1 — there are no third-party packages to migrate.

## Alternatives Considered

### Alternative B: Package-Scoped Visibility with Minimal Imports

Classes within a package see each other automatically. Cross-package references require an `import` declaration at the top of the file:

```beamtalk
// src/app.bt
import json        // Makes JSON class available from the json package
import http.{Request, Response}  // Selective import

Object subclass: MyApp
  run => JSON parse: (Request get: "https://api.example.com")

```

**Semantics:**
- `import packagename` makes all public classes from that package available by their short names
- `import packagename.{Class1, Class2}` imports only specific classes
- Classes without `import` are only visible within their own package
- No `pub`/`private` keywords on classes — all classes in a package are public to importers

**Why deferred:** The interaction between imports and REPL `:load`, hot code reloading, workspace bindings, and the package build system needs more design work. ADR 0026's build system doesn't resolve dependencies yet. Shipping imports before dependency resolution works end-to-end would create a half-working feature.

**Estimated effort:** L (parser, semantic analysis, codegen changes, REPL integration, package resolver)

### Alternative C: Newspeak-Style Nested Classes as Modules

Top-level classes define module boundaries. Classes nested within them form the module's API. Dependencies are passed as constructor parameters:

```beamtalk
Object subclass: DrawingModule

  class new: aShapeFactory => self basicNew shapes: aShapeFactory

  Object subclass: Circle
    area => Float pi * (radius * radius)

  Object subclass: Canvas
    draw: aShape => "..."

// Usage — explicit dependency wiring
drawing := DrawingModule new: ShapeFactory
circle := drawing Circle new
drawing Canvas new draw: circle
```

**Why deferred:** Requires nested class definitions (not yet in the parser or AST), virtual class semantics, constructor-based dependency injection patterns, and fundamental changes to class registration. The REPL experience degrades — `Counter new` no longer works without a module instance. This is a v0.3+ consideration that depends on language features not yet designed.

**Estimated effort:** XL (new AST nodes, class system redesign, codegen overhaul, REPL UX rethink)

## Consequences

### Positive
- Zero syntax surface area for v0.1 — nothing to learn, nothing to get wrong
- Matches Pharo/Squeak mental model — familiar to Smalltalk developers
- Preserves REPL simplicity — `:load` a file, use the classes immediately
- Avoids premature design — module system can be informed by real usage patterns
- ADR 0016 naming prevents Erlang-level module collisions under the hood

### Negative
- Name collisions between user code and third-party packages are possible (mitigated by small ecosystem)
- No visibility control — all classes are public, all methods are public
- Large projects have no organizational mechanism beyond filesystem conventions
- Silent hot-reload on duplicate class names may surprise users — no warning is emitted today
- Tooling (LSP, IDE) must resolve ambiguous class names without namespace context; "go to definition" and error messages cannot distinguish same-named classes from different packages
- A dependency could shadow stdlib class names (`Integer`, `Actor`) with no guardrail, creating a supply-chain risk

### Neutral
- ADR 0016 internal naming (`bt@package@module`) is unaffected — it continues to work as designed
- The decision to defer does not prevent early prototyping of import syntax on a branch
- Third-party package authors should use distinctive class names as a convention

## Implementation

v0.1 requires implementation of collision warnings plus documentation changes:

1. **Add collision warning** when a class is redefined during `:load` or module on-load. The `register_class/0` codegen currently falls through to `update_class` silently on `already_started` — add a `?LOG_WARNING` and surface the warning to REPL users (e.g., `⚠️ Class 'Counter' redefined`)
2. **Prevent stdlib shadowing** — emit an error (not just a warning) if user code redefines a stdlib class name (`Integer`, `String`, `Actor`, etc.)
3. **Document flat namespace as explicit design** in the language guide and getting-started docs
4. **Document naming conventions** for avoiding collisions (e.g., prefix package-specific classes)
5. **Document `:load` behavior** for loading multiple files — classes are globally registered
6. **Update known-limitations.md** to reference this ADR

For v0.2 (import system), the leading candidate is Option B (package-scoped imports), which builds naturally on ADR 0016 naming and ADR 0026 package definitions. A separate ADR will be drafted when v0.2 planning begins. Considerations for that ADR should include:
- A qualified-name escape hatch (e.g., `MyApp::Counter`) for disambiguation without full imports
- Whether collision policy should be configurable (error vs. warn vs. replace)
- How class identity surfaces in tooling, error messages, and serialization

## Implementation Tracking

**Epic:** [BT-736](https://linear.app/beamtalk/issue/BT-736) — Epic: Flat Namespace for v0.1 (ADR 0031)
**Issues:**
- [BT-737](https://linear.app/beamtalk/issue/BT-737) — Add class redefinition collision warning and document flat namespace
- [BT-738](https://linear.app/beamtalk/issue/BT-738) — Prevent stdlib class shadowing with structured error (blocked by BT-737)
**Status:** Planned

## Migration Path

No migration needed — this ADR formalizes the current behavior. When a module/import system is added in a future version, a migration path will be specified in that ADR.

## References
- Related issues: [BT-714](https://linear.app/beamtalk/issue/BT-714) — Design user-facing module/namespace syntax
- Related ADRs:
  - [ADR 0016](0016-unified-stdlib-module-naming.md) — Unified stdlib module naming with `@` separator
  - [ADR 0026](0026-package-definition-and-project-manifest.md) — Package definition and project manifest
- External:
  - [Pharo namespace discussion](https://github.com/pharo-project/pharo/issues/13563)
  - [Newspeak module system](http://bracha.org/newspeak-modules.pdf)
  - [Gleam modules](https://tour.gleam.run/basics/modules/)
  - `docs/known-limitations.md` — Flat namespace documented as known limitation
