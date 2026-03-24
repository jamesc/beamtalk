# ADR 0070: Package Namespaces and Dependencies

## Status
Proposed (2026-03-24)

## Context

Beamtalk v0.1 shipped with a flat global namespace (ADR 0031). All classes are globally visible, there is no import/export syntax, and class names must be unique across a workspace. This was an explicit decision — the ecosystem was too small and the language too young to design a module system.

The infrastructure for namespaced packaging already exists:

- **ADR 0016** defines BEAM module naming: `bt@{package}@{module}`, with `@` as separator (proven by Gleam's 5+ years of production use)
- **ADR 0026** defines `beamtalk.toml` as the package manifest, with a reserved `[dependencies]` section
- **ADR 0031** documents the flat namespace and identifies package-scoped imports as the leading v0.4 candidate
- **ADR 0067** defines three class kinds (Object/Value/Actor) whose metadata must be resolvable across package boundaries
- **ADR 0068** defines protocols with structural conformance — protocol names need a namespace story

The gap is now concrete:

1. No dependency declaration or resolution — the `[dependencies]` section is a placeholder
2. No collision detection — two packages defining `Parser` silently shadow each other
3. No disambiguation — no way to refer to "the `Parser` from the `json` package"
4. No cross-package metadata — ClassKind and protocol signatures aren't available to the compiler across package boundaries

### Design Principles

These constraints shape the solution space:

1. **Pragmatic over pure** — a working package system beats a theoretically perfect module system. Newspeak-style nested classes as modules (ADR 0031, Option C) is deferred.
2. **No per-file ceremony** — Smalltalk has no imports. If your package declares a dependency, its classes are available everywhere in your package. No `import` statements.
3. **All classes are public** — Smalltalk tradition. No visibility keywords on classes or methods in this ADR. Class-level `private` is a planned near-term follow-up (see Section 9).
4. **Interactive-first** — the REPL workspace remains a flat namespace where loaded classes are immediately usable.
5. **Collisions are errors, not surprises** — no silent shadowing.

## Decision

### 1. Package Dependencies in `beamtalk.toml`

Dependencies are declared in `beamtalk.toml` with two resolution strategies:

```toml
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
# Local path — for monorepos and local development
utils = { path = "../my-utils" }

# Git repository — for sharing packages
json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }
http = { git = "https://github.com/someone/beamtalk-http", branch = "main" }
```

Each dependency must point to a directory containing a `beamtalk.toml`. The compiler resolves dependencies transitively in topological order, compiles each, and places their `ebin/` on the BEAM code path.

A **lockfile** (`beamtalk.lock`) pins exact commit SHAs for git dependencies, ensuring reproducible builds. It is generated on first resolve and updated explicitly via a future `beamtalk deps update` command (not yet implemented). Path deps are not locked — they resolve to whatever is on disk, so they are only reproducible within a single repository checkout.

**Version policy:** Each package name may appear at most once in the resolved dependency graph. If two dependencies require different versions of the same transitive dependency, this is a **resolve error** — the user must align versions. This matches Hex.pm's single-version policy and avoids the complexity of multiple coexisting versions. Diamond dependency resolution with version ranges is deferred to the registry-based dependency phase.

Registry-based resolution (`json = "1.0"` via Hex.pm or a custom registry) is out of scope for this ADR and will be addressed when the ecosystem warrants it.

### 2. Class Visibility: Package-Scoped by Default

When a package declares a dependency, **all classes from that dependency are available by their short name** throughout the depending package:

```beamtalk
// my_app depends on json (declared in beamtalk.toml)
// No import statement needed — just use the class

Object subclass: MyApp
  parse: input => JSON parse: input
```

Within a package, all classes see each other (unchanged from v0.1). Across packages, you must declare the dependency in `beamtalk.toml` — undeclared cross-package class references are a compile error.

**Transitive dependencies are visible but warn.** If `my_app` depends on `json` and `json` depends on `utils`, `my_app` CAN reference classes from `utils` — the runtime sees them (they're on the BEAM code path), the REPL sees them, and the compiler allows it. But using a transitive dependency's class emits a **warning**:

```text
warning[W0302]: Class 'StringUtils' is from transitive dependency 'utils' (via 'json')
  --> src/app.bt:5:12
   |
   = help: add 'utils' to [dependencies] in beamtalk.toml to make this explicit
```

This preserves Smalltalk's "you can always reach the object" philosophy — if a class is running in your system, you can name it. But the compiler nudges you toward explicit dependency declarations for maintainability.

**Strict mode for library authors:** Library packages that need clean dependency boundaries can opt into strict checking:

```toml
[package]
name = "my_library"
strict-deps = true   # transitive dep usage is an error, not a warning
```

When `strict-deps = true`, referencing a transitive dependency's class is a compile error, not a warning. This defaults to `false` — applications get the Smalltalk-friendly loose model, library authors opt into strict boundaries when they need ecosystem-grade dependency hygiene. This follows TypeScript's progressive strictness model (`strict: true` in `tsconfig.json`).

### 3. Collision Detection: Error, Not Shadowing

If two dependencies export the same class name, the compiler emits an **error** — not a warning, not silent shadowing:

```text
error[E0301]: Class name 'Parser' is exported by multiple dependencies
  --> src/app.bt:5:12
   |
   = note: 'Parser' is defined in package 'json' (json@Parser)
   = note: 'Parser' is defined in package 'xml' (xml@Parser)
   = help: use qualified name: json@Parser or xml@Parser
```

This applies only to classes that are actually **referenced** in the depending package. If `json` and `xml` both export `Parser` but your code never mentions `Parser`, there is no error. The collision is detected lazily at use site, not eagerly at dependency resolution.

Stdlib class names (`Integer`, `String`, `Actor`, `Object`, `Value`, etc.) are reserved. A dependency that exports a class with a stdlib name triggers a compile error when depended upon — this extends the existing stdlib shadowing protection (ADR 0031, BT-738).

### 4. Qualified Names: `package@Class`

When a collision exists (or for explicitness), classes can be referenced by their package-qualified name using the `@` separator:

```beamtalk
Object subclass: MyApp
  convert: input =>
    jsonTree := json@Parser parse: input.
    xmlTree := xml@Parser parse: input.
    jsonTree
```

The `@` syntax was chosen because:

- It matches the existing BEAM module naming (`bt@json@parser`) — no new separator to learn
- It is visually distinctive — `json@Parser` clearly reads as "Parser from json"
- `::` is taken (type annotations per ADR 0053)
- `.` is the message send terminator

**Lexer note:** `@` is already used as a prefix for directives (`@primitive`, `@intrinsic`, `@expect`). However, directives always appear at the start of a line or statement with `@` as the first character, while qualified names have `@` between two identifiers (`identifier@Identifier`). The lexer can distinguish these by context: if `@` follows an identifier token, it is a package qualifier; if `@` begins a token, it is a directive prefix. This is a straightforward disambiguation — no ambiguity exists in practice.

Qualified names work everywhere a class name is accepted: message sends, subclassing, type annotations, pattern matching, and REPL expressions.

```beamtalk
// Subclassing
json@Parser subclass: LenientParser
  // ...

// Type annotation
parse: input :: String -> json@ParseResult =>
  json@Parser parse: input

// REPL
json@Parser parse: '{"key": "value"}'
// => Dictionary("key" -> "value")
```

### 5. Protocols Follow the Same Rules

Protocols (ADR 0068) are namespace citizens just like classes. A protocol defined in a package is available by name to any package that depends on it:

```beamtalk
// In package "collections":
Protocol define: Enumerable
  do: aBlock => "Iterate over elements"
  size => "Return number of elements"

// In package "my_app" (depends on "collections"):
// Enumerable is available by name — no import needed

// Structural conformance means you don't need the protocol in scope
// to *implement* it — your class just needs the right methods.
// You need the name only for type annotations:
printAll: items :: Enumerable => items do: [:item | Console println: item]
```

Collision rules apply identically: if two dependencies define a protocol with the same name, it's an error at the use site, resolvable with `package@ProtocolName`.

**Note on structural conformance:** Two packages may define protocols with the same name but different method sets (e.g., `json@Serializable` requires `toJson` while `xml@Serializable` requires `toXmlString`). A class conforming to one does not necessarily conform to the other. The qualified name syntax resolves the naming ambiguity in type annotations (`:: json@Serializable`), but library authors should choose distinctive protocol names to avoid confusion.

### 6. REPL Behavior

The REPL workspace remains a flat namespace:

- `Workspace load: "path/to/file.bt"` makes classes available immediately (unchanged)
- If the REPL is started in a package directory, all dependencies are loaded onto the code path
- Qualified names (`json@Parser`) work in the REPL
- Collisions in the REPL produce a warning (not an error) with a hint to use the qualified name — the REPL is exploratory, and hard errors would break the interactive flow

```text
beamtalk> json@Parser parse: '{"a": 1}'
// => Dictionary("a" -> 1)

beamtalk> Parser parse: '{"a": 1}'
// ⚠️ Ambiguous: 'Parser' is defined in both 'json' and 'xml'. Using json@Parser.
// Use json@Parser or xml@Parser to be explicit.
// => Dictionary("a" -> 1)
```

The REPL resolves ambiguous names by using the **first-loaded** package's definition (with a warning). This keeps interactive behavior deterministic while preserving visibility — the user always knows what happened.

**Hot reload of dependency classes** is not supported in the initial implementation. `Counter reload` continues to work for classes defined in the current package (recompile from source). For dependency classes, the BEAM's code loading model does not prevent hot reload — `code:load_binary/3` doesn't care where a `.beam` came from — so a future version could support reloading path dependencies from modified source or reloading any dependency from recompiled `.beam` files.

### 7. Cross-Package Class Metadata

The `.app` file's `{env, [{classes, [...]}]}` metadata is extended to include class kind and protocol conformance:

```erlang
{env, [
  {classes, [
    #{name => 'Parser',
      kind => object,        % object | value | actor
      type_params => []      % generic type parameters (ADR 0068)
    }
  ]}
]}
```

Protocol conformance is **not** stored in class metadata. Beamtalk uses structural conformance (ADR 0068) — a class conforms to a protocol if it has the right methods, with no explicit declaration. Pre-computing conformance in `.app` metadata would be stale the moment a new protocol is defined. Instead, the compiler checks conformance at the use site by comparing the class's method signatures (already in `__beamtalk_meta/0`) against the protocol's requirements (in the protocol's defining package).

This allows the compiler to:
- Propagate ClassKind across package boundaries (ADR 0067 hierarchy resolution)
- Check protocol conformance at use sites using method signatures from `__beamtalk_meta/0`
- Provide chain completion and `:help` for dependency classes in the REPL and LSP

### 8. Package Reflection: The `Package` Class

Packages are first-class objects, inspectable via messages like any other part of the system. The `Package` class ships with the package system — it is the Smalltalk answer to "where does this class come from?" and a core part of the tooling story for LSP, MCP, and AI agents.

Possible API (illustrative, not final):

```beamtalk
// Discover loaded packages
Package all
// => #(Package("json"), Package("utils"), Package("my_app"))

// Introspect a package
pkg := Package named: "json"
pkg name          // => "json"
pkg version       // => "1.0.0"
pkg classes       // => #(JSON, JSONError, Parser)
pkg dependencies  // => #(Package("utils"))

// Reverse lookup — which package owns this class?
JSON package      // => Package("json")
JSON packageName  // => "json"

// Workspace-level view
Workspace dependencies
// => Dictionary("json" -> Package("json"), "utils" -> Package("utils"))
```

This addresses the "where does this class come from?" discoverability gap without per-file imports — you ask the object. The class registry already knows the BEAM module name (`bt@json@parser`), so extracting the package segment is trivial.

For tooling, `Package` enables:
- **LSP:** cross-package go-to-definition and hover ("from package json")
- **MCP tools:** `list_packages`, `package_classes` without custom Erlang
- **AI agents:** discover available classes by querying the runtime, not parsing TOML
- **`:help`:** show package provenance alongside class documentation
- **Workspace:** `Workspace dependencies` returns the current package's dependency graph

### 9. Visibility: Planned Follow-Up

This ADR ships with all classes public — Smalltalk tradition. However, once packages are distributed and third-party libraries exist, library authors will need **stable API boundaries**. Without visibility control, every class in a package is part of its public API, constraining internal refactoring.

A follow-up ADR will address class (and potentially method) visibility as part of a broader review of the class/method modifier set. The likely direction is a `private` modifier on class declarations:

```beamtalk
// Public (default) — available to any package that depends on this one
Value subclass: Parser
  parse: input => ...

// Package-private — only visible within this package
private Value subclass: ParserState
  // Implementation detail, not part of the public API
```

Open questions for the follow-up:
- **Scope:** Class-level only, or method-level too? Smalltalk tradition says methods are always public if you have the object — but library authors may want private helper methods.
- **Modifier set:** How does `private` interact with existing modifiers (`sealed`, `abstract`, `typed`)? Should all modifiers be reviewed together?
- **REPL behavior:** Should private classes be accessible in the REPL for debugging? (Likely yes — compile-time restriction only.)
- **Enforcement:** Compiler error, or warning that can be promoted to error (like `strict-deps`)?

This should follow shortly — the ecosystem cost of adding visibility grows with every published package that exposes internal classes.

## Prior Art

### Smalltalk (Pharo/Squeak) — Flat namespace with packages as metadata

All classes live in `SystemDictionary`. Packages (Monticello, Metacello) are organizational metadata — they group classes for loading/versioning but do not create namespaces. Dependencies are declared in Metacello configurations (baselines). All classes from loaded packages are globally available.

**What we adopt:** Dependencies make classes available without per-file imports. The package is the unit of distribution, not the unit of visibility control.

**What we differ on:** Collisions are errors, not silent shadowing. Qualified names provide an explicit disambiguation mechanism that Pharo lacks.

### Gleam — File-based modules with explicit imports

Every `.gleam` file is a module. Cross-module access requires `import gleam/io`. Functions are private by default; `pub` makes them public.

**What we adopt:** File-path-based BEAM module naming (ADR 0016 already does this). Git-based dependency resolution.

**What we differ on:** No per-file `import` statements. No `pub`/private visibility. Beamtalk is Smalltalk-tradition — everything is accessible by name.

### Go — Implicit package-level visibility, explicit imports

Within a Go package, all exported names are visible. Cross-package requires `import "github.com/user/repo"`. Go started with GitHub URLs for dependencies before adding a module proxy.

**What we adopt:** The progression from URL-based deps to registry is sound. Go proved that git URLs are a viable starting point.

**What we differ on:** Go requires per-file imports. Beamtalk does not.

### Rust/Cargo — Path + Git + Registry dependencies

Cargo supports `path`, `git`, and `crates.io` dependency sources. The lockfile (`Cargo.lock`) pins exact versions. Qualified names use `::` (`json::Parser`).

**What we adopt:** The three-source dependency model (path, git, registry) with lockfile. Starting with path + git is a proven bootstrap strategy.

**What we differ on:** `@` not `::` for qualification (Beamtalk uses `::` for type annotations). No per-file `use` declarations.

### Elixir/Mix — Multi-keyword, Hex.pm registry

Elixir provides `alias`, `import`, `require`, `use` — four mechanisms for four different needs. Dependencies from Hex.pm or git in `mix.exs`.

**What we differ on:** Four keywords is over-engineered for Beamtalk. No keywords at all is the right starting point for a Smalltalk-heritage language.

## User Impact

### Newcomer (from Python/JS/Ruby)

- **Surprised by no imports** — "Where's the import statement?" Answer: you declared the dependency in `beamtalk.toml`, so its classes are just available.
- **Upside:** Zero ceremony. No boilerplate at the top of every file.
- **Adjustment:** Must learn that `beamtalk.toml` is where dependencies are declared, not source files.

### Smalltalk developer

- **Feels like home.** Dependencies loaded → classes available. Exactly how Monticello/Metacello works in Pharo.
- **Pleased by** collision detection — Pharo's silent shadowing is a known pain point.
- **Comfortable with** `json@Parser` qualified syntax — reads naturally.

### Erlang/BEAM developer

- **Understands** the model — it's similar to OTP applications providing modules.
- **Slightly uneasy** without per-file imports — "how do I know where `Parser` comes from?" Answer: it comes from your declared dependencies, and the LSP shows you.
- **Appreciates** that `@` matches the BEAM module naming they already see in crash dumps.

### Production operator

- **Reassured by** collision-as-error — no silent shadowing in production builds.
- **Wants** the lockfile for reproducible deploys.
- **Would eventually want** a registry for auditing dependency provenance.

## Steelman Analysis

### Per-File Imports (Gleam/Elixir style)

| Cohort | Strongest argument |
|--------|-------------------|
| Newcomer | "When I read a file, I want to see at the top which classes it uses. Without imports, I need to search `beamtalk.toml` and guess which dependency provides each class. Every other language I've used has imports — this is the one place Beamtalk feels alien." |
| BEAM veteran | "Every other BEAM language has per-file imports. Beamtalk is the odd one out. Gleam proved file-based modules with imports work beautifully on BEAM — zero-ceremony within a package, explicit across packages. We could adopt this without abandoning Smalltalk ergonomics." |
| Library author | "When I add a new public class to my library, it enters every user's namespace with no opt-out. Per-file imports give my users control: they see exactly what they're bringing in. Without imports, my users can't limit their exposure to my API surface." |
| Operator | "For SOC2 and supply-chain compliance, I need file-level dependency attribution. `grep import` gives me which files touch crypto, networking, or database code. With package-wide visibility, every file implicitly has access to the full transitive dependency closure — I cannot tell which files use which capabilities without running the compiler." |
| AI agent | "Without per-file imports, an AI generating code has no local signal for what names are in scope. It must parse `beamtalk.toml` plus all transitive dependencies to know what classes are available — a context-gathering step that imports eliminate." |

**Counter-argument:** Per-file imports add ceremony that the Smalltalk community has never wanted. The LSP can provide "where does this class come from?" via hover/go-to-definition without import statements — though this doesn't help in code review, CI output, or terminal reading where there is no LSP. The audit use case (which files use crypto?) is served by grepping for the class name rather than the import, at the cost of needing to know which class names to grep for. The AI agent concern is real but mitigated by LSP integration and `beamtalk.toml` being machine-readable. Notably, C# 10 moved *away* from per-file imports with `global using` and implicit usings — Microsoft found the per-file ceremony added noise without value. The trade-off is genuine: per-file imports provide better static analysability at the cost of per-file ceremony. We choose the Smalltalk tradition.

### Newspeak-Style Nested Classes as Modules

| Cohort | Strongest argument |
|--------|-------------------|
| Language designer | "Virtual nested classes are the most powerful modularity mechanism ever designed. You can override a nested class in a subclass and get automatic family polymorphism. No other BEAM language offers this." |
| Smalltalk purist | "Everything should be an object. Modules should be objects. Newspeak got this right." |
| Security-focused developer | "No global namespace means capability security is built in. Every dependency is explicit in the constructor — code cannot reach capabilities it wasn't given. This is the only module model that's secure by construction, not by convention." |

**Counter-argument:** Newspeak is a research language with minimal adoption. The "DI all the way down" verbosity problem is real. Users want to write `Counter new`, not `myModule Counter new`. The capability-security argument is compelling in theory but Beamtalk runs on the BEAM, where any process can call any module — Newspeak-style encapsulation would be a language-level fiction on this runtime. This can be revisited in a future version if the class system evolves to support nested definitions.

### Visibility Keywords (`pub`/`private`)

| Cohort | Strongest argument |
|--------|-------------------|
| Library author | "Without visibility, I cannot draw a stable API boundary. Any class I ship becomes part of my API the moment a user references it — even `InternalJsonParserState`. The `Internal` prefix convention is unenforceable: the compiler happily compiles references to it, the LSP happily autocompletes it, and the lockfile pins it. When I refactor my internals, I break users who depended on implementation details. In a distributed package ecosystem with version constraints, this is the difference between a patch release and a major version bump." |
| Operator | "Private classes reduce attack surface. If a class isn't public, it can't be exploited via the REPL or interop. With all-public packages, any vulnerability in any class is exposed." |
| Language designer | "Every successful package ecosystem — Cargo, npm, pip, Hex — has visibility control. You're not deferring an optional feature, you're deferring table stakes. The longer you wait, the more internal classes get baked into downstream code, and the more painful the migration when visibility arrives." |

**Counter-argument:** The library author and language designer arguments are the strongest in this entire ADR. Class-level `private` visibility is a planned near-term follow-up (see Section 9), to be designed alongside a broader review of the class/method modifier set. It is deferred from this ADR because the scope of visibility (class-only vs. method-level, enforcement model, modifier interactions) deserves its own focused design. But it must arrive before the ecosystem grows beyond a handful of packages — the cost grows with every published library that exposes internal classes. The REPL will remain open for debugging (private is compile-time only) — "all inspectable" is preserved.

## Alternatives Considered

### Alternative: Per-File `import` Declarations

```beamtalk
import json
import http.{Request, Response}

Object subclass: MyApp
  run => JSON parse: (Request get: "https://api.example.com")
```

**Rejected because:** It contradicts Beamtalk's Smalltalk heritage. No Smalltalk has per-file imports. The ceremony-per-file cost is low individually but adds up across a project. The information that imports provide ("where does this class come from?") is better served by LSP hover/go-to-definition and the `beamtalk.toml` manifest.

### Alternative: Dot-Separated Qualified Names (`json.Parser`)

**Rejected because:** `.` is the message send terminator in Beamtalk. `json.Parser parse: input` is ambiguous — is `json` a variable receiving the `Parser` message, or a package qualifier? Using `@` avoids this entirely.

### Alternative: Double-Colon Qualified Names (`json::Parser`)

**Rejected because:** `::` is used for type annotations (ADR 0053). `json::Parser` would be ambiguous with a type annotation on a variable named `json`.

### Alternative: Aliases for Collision Resolution

```toml
[aliases]
JsonParser = "json@Parser"
```

Or in source:
```beamtalk
alias json@Parser as: JsonParser
```

**Deferred, not rejected.** Aliases are sugar for "I use this qualified name often." The qualified name syntax (`json@Parser`) is sufficient for v0.4. If collisions turn out to be common enough that qualified names become noisy, aliases can be added as a non-breaking enhancement — likely in the manifest to keep source files import-free.

### Alternative: Registry-Based Dependencies (Hex.pm)

```toml
[dependencies]
json = "1.0"
```

**Deferred, not rejected.** Hex.pm is the right long-term answer (Gleam proved a non-Elixir language can use it). But it requires ecosystem maturity: published packages, version policies, a publishing workflow. Path + git dependencies are sufficient while the ecosystem bootstraps.

## Consequences

### Positive

- **Zero per-file ceremony** — no imports, no boilerplate. Declare once in `beamtalk.toml`, use everywhere.
- **Collisions are caught** — compiler error instead of silent shadowing. Explicit resolution via `package@Class`.
- **`@` syntax is consistent** — matches BEAM module naming (ADR 0016), visually distinctive, no parser ambiguity.
- **Incremental adoption** — path deps for local dev, git deps for sharing, registry later. Each stage is independently useful.
- **REPL stays simple** — flat workspace, immediate access, qualified names available if needed.
- **Lockfile from day one** — reproducible builds are table stakes.
- **Cross-package subclassing works with existing infrastructure** — `__beamtalk_meta/0` already emits class metadata (superclass, `is_value` flag, fields, field types, methods, generics, sealed/abstract flags), and `add_from_beam_meta` already injects this into the class hierarchy during compilation. ClassKind (Object/Value/Actor) is derived from the superclass chain and `is_value` flag, not stored as an explicit `kind` field. Dependency resolution just needs to put `.beam` files on the code path — the same mechanism that handles cross-file inheritance within a package handles cross-package inheritance for free.
- **Dependency caching** — compiled dependencies don't recompile unless their lockfile entry changes. While there's no within-package parallelism (all classes see each other), the package boundary provides a natural caching boundary that eliminates redundant compilation of stable libraries.

### Negative

- **"Where does this class come from?"** — without per-file imports, a reader must consult `beamtalk.toml` or the LSP to know which dependency provides a class. This is a real discoverability cost.
- **No visibility control** — library authors cannot hide internal classes. Naming conventions are the only guardrail until the visibility follow-up (Section 9).
- **Lazy collision detection** means you discover collisions when you use the name, not when you add the dependency. This could surprise users who add a dependency and find existing code breaks.
- **Git dependencies are brittle** — repositories can be deleted, force-pushed, or made private. The lockfile mitigates this but doesn't eliminate it. A registry is the long-term answer.
- **No aliasing** — qualified names can be verbose for heavily-used collision cases. This is a known trade-off, deferring aliases until real usage patterns emerge.
- **Dependency upgrades can break your code** — if a dependency adds a new class that collides with one of your class names, upgrading that dependency introduces a compile error. The fix (switching to a qualified name) may require changing your public API. This is more severe than a mere "discovery" issue — it's a breaking change imposed by a dependency update.
- **Extension method conflicts across packages are unresolvable** — ADR 0066's `{Class, Selector}` conflict detector fires if two dependencies both extend the same class with the same method (e.g., both ship `String >> toJson`). The user cannot rename a third-party extension, and `package@Class` qualifies class references but not extension method targets. The only recourse is to drop one of the conflicting packages. Extension-method-heavy packages are more likely to cause unresolvable dependency conflicts.

### Neutral

- ADR 0016 internal naming (`bt@{package}@{module}`) is unaffected
- ADR 0026 manifest structure gains `[dependencies]` content but the format is unchanged
- Existing single-file mode continues to work — no manifest required for `beamtalk run file.bt`
- The flat namespace within a package is preserved — this ADR adds cross-package structure, not intra-package

## Implementation

### Phase 1: Dependency Resolution

1. **Parse `[dependencies]`** in `beamtalk.toml` — path and git sources
2. **Path resolution** — resolve relative paths, locate `beamtalk.toml`, validate package name
3. **Git resolution** — clone to `_build/deps/{package}/`, checkout specified tag/branch/rev
4. **Lockfile** — generate `beamtalk.lock` with exact commit SHAs for git deps; path deps are not locked (they're local)
5. **Topological compilation** — build dependencies before the root package; detect cycles
6. **Code path** — add each dependency's `ebin/` to the BEAM code path at compile and runtime

Affected components: `crates/beamtalk-cli/` (dep fetching, lockfile), `crates/beamtalk-core/` (compilation ordering), `beamtalk.toml` schema

### Phase 2: Qualified Name Syntax

Qualified names must be parseable before collision detection can suggest them.

1. **Lexer** — when `@` follows an identifier token (infix position), emit it as a package qualifier separator; when `@` begins a token (prefix position), continue to lex it as a directive (`@primitive`, `@intrinsic`, `@expect`)
2. **Parser** — recognize `identifier @ Identifier` as a qualified class reference (package@Class)
3. **AST** — extend `ClassReference` node to carry an optional package qualifier; extend `StandaloneMethodDefinition.class_name` to support qualified targets for cross-package extension methods
4. **Semantic analysis** — resolve qualified names to BEAM module atoms via ADR 0016 naming
5. **Codegen** — emit the full `bt@{package}@{class}` atom for qualified references
6. **REPL** — support qualified names in expressions, `:help`, `:describe`, chain completion

Affected components: `crates/beamtalk-core/src/source_analysis/` (lexer, parser), `crates/beamtalk-core/src/ast.rs`, `crates/beamtalk-core/src/semantic_analysis/`, `crates/beamtalk-core/src/codegen/`

### Phase 3: Collision Detection

1. **Collect class registries** — after compiling dependencies, gather all exported class names from `.app` metadata
2. **Detect collisions** — at compile time, when a class name is referenced, check if it's ambiguous across dependencies
3. **Emit error** with qualified-name suggestion (qualified names are parseable from Phase 2)
4. **Stdlib reservation** — extend BT-738's stdlib shadowing check to dependency classes

Affected components: `crates/beamtalk-core/src/semantic_analysis/` (name resolution), `crates/beamtalk-core/src/codegen/` (qualified name mapping)

### Phase 4: Cross-Package Metadata

1. **Extend `.app` metadata** — add `kind` and `type_params` to the class registry entries (protocol conformance is checked at use sites via method signatures, not pre-computed in metadata)
2. **Emit metadata during compilation** — codegen writes extended class info to `.app.src`
3. **Read metadata during dependency resolution** — the compiler loads dependency `.app` files to resolve ClassKind and generic signatures; protocol conformance is checked by comparing `__beamtalk_meta/0` method signatures against protocol requirements
4. **LSP integration** — use metadata for cross-package hover, go-to-definition, and chain completion

Affected components: `crates/beamtalk-core/src/codegen/` (metadata emission), `runtime/apps/beamtalk_runtime/src/` (metadata format), `crates/beamtalk-lsp/` (cross-package navigation)

### Phase 5: Package Reflection

1. **`Package` stdlib class** — Erlang-backed Object class (class methods only, like `File` or `System`) wrapping the OTP application metadata and class registry
2. **Core API** — `Package all`, `Package named:`, `name`, `version`, `classes`, `dependencies`, `source`
3. **Reverse lookup** — `packageName` and `package` methods on `Metaclass`, so any class can report its owning package. Derived from the `bt@{package}@{class}` BEAM module name.
4. **LSP** — use `Package` metadata for "from package json" in hover tooltips and go-to-definition across packages
6. **MCP** — `list_packages` and `package_classes` tools backed by `Package` class methods
7. **REPL** — `:help ClassName` shows package provenance; `Workspace dependencies` returns the dependency graph

Affected components: `runtime/apps/beamtalk_stdlib/src/` (Package class), `lib/Package.bt` (Beamtalk wrapper), `crates/beamtalk-lsp/` (hover/go-to-def), `runtime/apps/beamtalk_runtime/src/` (MCP tools)

## Migration Path

### From v0.1 Flat Namespace

Existing single-package code requires **no changes**. The flat namespace within a package is preserved.

Projects that load multiple `.bt` files from different directories via `:load` should consider organizing into packages with explicit dependencies. The flat REPL loading continues to work but won't benefit from collision detection.

### Dependency Declaration

Existing multi-file projects that rely on implicit class availability across directories should:
1. Create a `beamtalk.toml` if one doesn't exist
2. Move shared libraries into separate package directories with their own `beamtalk.toml`
3. Declare dependencies in the consuming package's `[dependencies]`

This is opt-in — the old single-file and single-package workflows are unaffected.

## References

- Related issues:
  - [BT-714](https://linear.app/beamtalk/issue/BT-714) — Design user-facing module/namespace syntax for multi-file projects
  - [BT-742](https://linear.app/beamtalk/issue/BT-742) — Update class collision warning key design for package support
  - [BT-1153](https://linear.app/beamtalk/issue/BT-1153) — Draft ADR 0057: User Erlang Sources in Beamtalk Projects
- Related ADRs:
  - [ADR 0016](0016-unified-stdlib-module-naming.md) — Universal Module Naming with `@` Separator
  - [ADR 0026](0026-package-definition-and-project-manifest.md) — Package Definition and Project Manifest
  - [ADR 0031](0031-flat-namespace-for-v01.md) — Flat Namespace for v0.1
  - [ADR 0053](0053-double-colon-type-annotation-syntax.md) — Double-Colon Type Annotation Syntax
  - [ADR 0067](0067-separate-state-field-keywords-by-class-kind.md) — Separate state:/field: Keywords by Class Kind
  - [ADR 0068](0068-parametric-types-and-protocols.md) — Parametric Types and Protocols
- External:
  - [Gleam packages](https://gleam.run/writing-gleam/gleam-toml/) — file-based modules, Hex.pm + git dependencies
  - [Cargo dependency sources](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html) — path, git, registry model
  - [Go module history](https://go.dev/blog/using-go-modules) — GitHub URLs as bootstrap dependency mechanism
  - [Pharo namespace discussion](https://github.com/pharo-project/pharo/issues/13563) — ongoing, still unresolved
