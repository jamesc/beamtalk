# ADR 0075: Erlang FFI Type Definitions

## Status
Proposed (2026-04-02)

## Context

### The Problem

Beamtalk's Erlang FFI (ADR 0028) lets users call any Erlang function via message-send syntax:

```beamtalk
Erlang lists reverse: #(3, 2, 1)    // => #(1, 2, 3)
Erlang maps merge: a with: b        // => merged map
Erlang crypto hash: #sha256 with: "hello"
```

This works at runtime, but the **type checker has zero visibility** into Erlang function signatures. Every FFI call returns `Dynamic`, which means:

- No compile-time checking of argument types passed to Erlang functions
- No return type inference from FFI calls — downstream code loses type info
- LSP cannot provide typed completions or hover info for Erlang modules
- No warnings when passing wrong types to well-specced Erlang functions like `lists:reverse/1`

This is the **single largest gap** in typed coverage. A codebase that is fully typed in Beamtalk loses all type information the moment it touches Erlang — which is frequent, since OTP, Hex packages, and hand-written native modules all cross this boundary.

### Current State

**Type checker:** When the receiver is `Erlang` or `ErlangModule`, the type checker skips DNU warnings (because both classes override `doesNotUnderstand:args:`) and infers `Dynamic` for the return type. See `inference.rs` and `validation.rs`.

**Existing infrastructure that makes this solvable:**

1. **Beamtalk→Erlang type mapping** is already defined in `spec_codegen.rs` (lines 33–51) for generating Dialyzer `-spec` attributes. The reverse mapping (Erlang→Beamtalk) is straightforward:

   | Erlang type | Beamtalk type |
   |-------------|---------------|
   | `integer()` | `Integer` |
   | `float()` | `Float` |
   | `number()` | `Number` |
   | `binary()` | `String` |
   | `boolean()` | `Boolean` |
   | `atom()` | `Symbol` |
   | `list()` / `[T]` | `List` / `List(T)` |
   | `tuple()` | `Tuple` |
   | `map()` | `Dictionary` |
   | `pid()` | `Pid` |
   | `fun()` | `Block` |
   | `true` / `false` | `True` / `False` |
   | `nil` | `Nil` |
   | `term()` / `any()` | `Dynamic` |

2. **`beam_lib:chunks/2`** is already used in `beamtalk_module_activation.erl` to read BEAM file attributes. The same API reads `-spec` attributes from the `abstract_code` chunk (available when modules are compiled with `+debug_info`, which is the default for OTP, rebar3, and Beamtalk's own `compile.escript`).

3. **EEP-48 doc chunks** are already post-processed from `.beam` files (ADR 0008) — the build pipeline already does BEAM chunk extraction.

4. **OTP spec coverage** is excellent: core modules (`lists`, `maps`, `string`, `file`, `io`, `ets`, `gen_server`, `erlang`) are essentially 100% specced since OTP 18+. Coverage drops in obscure internal modules but is strong where it matters.

5. **`validate_specs.escript`** already extracts and processes spec attributes from compiled output — the tooling pattern exists.

6. **ADR 0028** explicitly identified spec extraction as future work: "Erlang parameter name introspection — with `+debug_info`, `beam_lib:chunks/2` can extract parameter names from the abstract code AST [...] **Requires a spike first.**"

### Constraints

1. **Must work for any BEAM module** — OTP, Hex packages, hand-written Erlang in `native/` (ADR 0072). Not just a curated OTP subset.
2. **Must not break existing code** — FFI calls without type info must continue to work (fall back to `Dynamic`).
3. **Must match user's actual environment** — type info should reflect the OTP version and dependencies actually installed, not a frozen snapshot.
4. **Quality must exceed auto-extraction** for commonly used modules — `term()` → `Dynamic` is technically correct but unhelpful for `lists:member/2` or `ets:lookup/2`.
5. **Must integrate with existing type checker** — `InferredType` enum, `TypeProvenance`, class hierarchy, and the gradual typing model (ADR 0025).
6. **Must support the package system** — packages (ADR 0070, 0072) can bundle type definitions for their native Erlang code.

## Decision

### Hybrid: Auto-Extract from `.beam` + `.d.bt` Override Files

A two-layer system that provides **automatic baseline typing** for all Erlang modules and **curated overrides** where precision matters.

### Layer 1: Auto-Extract from `.beam` Specs

At build time, the compiler reads `-spec` attributes from compiled `.beam` files on the code path and builds an `ErlangTypeRegistry` mapping module/function/arity to Beamtalk types.

**How it works:**

1. An Erlang helper module (`beamtalk_spec_reader.erl`) reads specs from the `abstract_code` chunk via `beam_lib:chunks(File, [abstract_code])` and extracts `{attribute, _, spec, ...}` forms. This chunk is present when `.beam` files are compiled with `+debug_info` (the default for OTP, rebar3, and Beamtalk's own compiler — see `+debug_info` section below)
2. The Rust compiler invokes it via the existing `beamtalk_build_worker` pattern (same mechanism used for `.core` → `.beam` compilation)
3. The Erlang→Beamtalk type mapping (reverse of `spec_codegen.rs`) converts Erlang abstract type representations to `InferredType` values
4. Results are cached per module and invalidated when `.beam` file timestamps change

**`+debug_info` dependency:**

Spec extraction requires the `abstract_code` chunk, which is only present in `.beam` files compiled with `+debug_info`. This is widely available but not universal:

| Source | `+debug_info` by default? | Notes |
|--------|--------------------------|-------|
| OTP modules | Yes | Shipped with debug info since OTP 14+ |
| rebar3 deps | Yes | `{erl_opts, [debug_info]}` is rebar3's default |
| Beamtalk native `.erl` | Yes | `compile.escript` includes `debug_info` |
| Mix deps (dev) | Yes | Mix includes `debug_info` by default |
| Mix release builds | **No** | `mix release` strips debug info since Elixir 1.9 |
| Hex packages (precompiled) | Varies | Some strip for size optimization |

**Policy:** When a `.beam` file lacks `abstract_code`, the spec reader returns no specs for that module — it silently falls through to `Dynamic` (layer 5 in the resolution chain). The build emits a one-time info-level diagnostic: `"Note: <module>.beam has no debug_info — auto-extracted types unavailable. Add a .d.bt stub for type coverage."` This is not a warning (it's expected for some packages) but gives users a path forward.

**Important:** This limitation only affects auto-extraction. Curated `.d.bt` stubs work regardless of `+debug_info` — they are the recommended path for packages known to strip debug info.

**Beamtalk-compiled `.beam` files:** Note that Beamtalk compiles via Core Erlang, and Core Erlang compilation does not preserve `-spec` attributes in the `abstract_code` chunk (documented in `validate_specs.escript`). However, this is not a problem: Beamtalk-compiled modules already have full type information in the Beamtalk type checker via `ClassHierarchy` — auto-extraction targets *foreign* (non-Beamtalk) `.beam` files.

**Example — what auto-extraction provides:**

```erlang
%% In lists.beam:
-spec reverse(List) -> List when List :: [T].
-spec seq(From, To) -> Seq
      when From :: integer(), To :: integer(), Seq :: [integer()].
-spec member(Elem, List) -> boolean()
      when Elem :: T, List :: [T].
```

Auto-extracts to:

| Function | Parameter types | Return type |
|----------|----------------|-------------|
| `reverse/1` | `List` | `List` |
| `seq/2` | `Integer, Integer` | `List(Integer)` |
| `member/2` | `Dynamic, List` | `Boolean` |

Note: `Elem :: T` with no constraint maps to `Dynamic` — correct but imprecise. The `.d.bt` override layer addresses this.

**Type variable handling:**

- Erlang type variables with constraints (e.g., `X :: integer()`) map to the constraint type
- Unconstrained type variables (e.g., `T` in `reverse/1`) map to a generic type parameter if the function signature is self-consistent, otherwise `Dynamic`
- `when` guards linking parameters (e.g., same `T` in input and output of `reverse/1`) preserve the relationship: input `List(T)` → output `List(T)`

**Edge cases:**

| Erlang pattern | Beamtalk mapping | Rationale |
|----------------|------------------|-----------|
| `term()` / `any()` | `Dynamic` | No useful type info |
| `non_neg_integer()` | `Integer` | Beamtalk has no subrange types (yet) |
| `iodata()` / `iolist()` | `Dynamic` | Recursive type, no Beamtalk equivalent |
| `-opaque` types | `Dynamic` | Opaque by design — don't expose internals |
| Multiple `-spec` clauses | Union of return types | Common for overloaded Erlang functions |
| `no_return()` | `Dynamic` | Functions that never return (throw/exit) |
| `map()` (untyped) | `Dictionary` | Beamtalk equivalent |
| `#{key := Type}` | `Dictionary` | Typed map keys not yet supported in Beamtalk |

### Layer 2: `.d.bt` Override Files

Hand-curated declaration files that override or supplement auto-extracted types. These add meaningful keyword names, tighter types, and type information for unspecced functions.

**Stub file format:**

```beamtalk
// stubs/otp/lists.d.bt

/// Type declarations for erlang module `lists`.
/// OTP compatibility: 25+
declare ErlangModule: lists

  /// Reverse a list.
  reverse: list :: List(T) -> List(T)

  /// Generate a sequence of integers.
  seq: from :: Integer to: to :: Integer -> List(Integer)
  seq: from :: Integer to: to :: Integer step: step :: Integer -> List(Integer)

  /// Test membership.
  member: elem :: T in: list :: List(T) -> Boolean

  /// Sort a list using the default comparison.
  sort: list :: List(T) -> List(T)

  /// Sort with a custom comparator.
  sort: list :: List(T) by: comparator :: Block(T, T -> Integer) -> List(T)

  /// Apply a function to each element.
  map: fun :: Block(A -> B) with: list :: List(A) -> List(B)

  /// Left fold.
  foldl: fun :: Block(T, Acc -> Acc) with: acc :: Acc with: list :: List(T) -> Acc
```

**Format rules:**

- `declare ErlangModule: <atom>` — names the Erlang module being typed
- Each line declares a function with Beamtalk-style keyword syntax and type annotations
- First keyword = Erlang function name, subsequent keywords provide meaningful parameter names
- Type parameters (e.g., `T`, `A`, `B`, `Acc`) are **method-scoped** — each function declaration introduces its own type variables, universally quantified per call. This differs from ADR 0068's class-level type params (e.g., `Result(T, E)`) which are positional and declared at the class site. Stub type params are more like Hindley-Milner `forall` — `reverse: list :: List(T) -> List(T)` means "for any type T, given a List(T), returns a List(T)." The type checker needs a per-call substitution pass for these, separate from the class-level generic substitution in ADR 0068
- `///` doc comments flow to LSP hover — they supplement (not replace) Erlang docs
- Functions not listed fall through to auto-extracted types
- Multiple arity variants are separate declarations

**Override semantics:**

A `.d.bt` declaration for `lists:reverse/1` completely replaces the auto-extracted type for that function/arity. If a function has a `.d.bt` declaration, auto-extracted types are ignored for that specific function/arity — not merged.

### Resolution Order

When the type checker encounters `Erlang <module> <function>: args`, it resolves the function's type signature using this precedence (highest wins):

```text
1. Package-bundled .d.bt    — library author ships types with their package
2. Project-local stubs/     — user overrides in their own project
3. Distribution stubs/      — curated stubs shipped with the Beamtalk compiler
4. Auto-extracted from .beam — read from compiled .beam at build time
5. Dynamic                  — no type info available (same as today)
```

**Each layer is a complete override at the function/arity level.** If `stubs/otp/lists.d.bt` declares `reverse/1`, that definition is used — the `.beam` spec is not consulted for that function. But `lists:nth/2`, not declared in the stub file, still uses the auto-extracted type.

### Type Checker Integration

The type checker gains an `ErlangTypeRegistry` that stores resolved function signatures:

```rust
/// Registry of Erlang function type signatures, populated at build time.
///
/// Resolution: .d.bt overrides > auto-extracted .beam specs > Dynamic
struct ErlangTypeRegistry {
    /// module → function_name → arity → FunctionSignature
    modules: HashMap<EcoString, HashMap<EcoString, Vec<FunctionSignature>>>,
}

struct FunctionSignature {
    params: Vec<ParamType>,
    return_type: InferredType,
    provenance: TypeProvenance,  // Declared(.d.bt span) or Inferred(.beam)
}

struct ParamType {
    keyword: Option<EcoString>,  // meaningful keyword name (from .d.bt only)
    type_: InferredType,
}
```

When the type checker encounters a message send on `ErlangModule`:

1. Extract the module name from the proxy receiver
2. Extract the function name from the first keyword and count arguments for arity
3. Look up `(module, function, arity)` in `ErlangTypeRegistry`
4. If found: check argument types against `params`, return the declared `return_type`
5. If not found: return `Dynamic` (same as today — no regression)

**Module identity tracking:**

The type checker must know *which* Erlang module a proxy represents to look up types. This works for two patterns:

- **Static inline chains** (`Erlang lists reverse: xs`): The compiler can see the module name `lists` directly in the AST. The `@intrinsic erlangModuleLookup` produces an `ErlangModule` with a known module name. The type checker infers `ErlangModule<lists>` — a `Known` type with the module name carried as a type argument (analogous to `List(Integer)`).

- **Variable-stored proxies** (`proxy := Erlang lists; proxy reverse: xs`): The type checker tracks the module identity through the assignment. `proxy` gets type `ErlangModule<lists>` from the right-hand side. Subsequent sends on `proxy` can look up the module name from the type.

- **Dynamic module names** (`proxy := Erlang (someVariable); proxy reverse: xs`): When the module name is a runtime value, the type checker cannot determine which module to look up. These fall back to `Dynamic` — the same behavior as today. This is consistent with ADR 0025's principle: "if the compiler can't determine a single type, it falls back to Dynamic."

This requires extending `ErlangModule`'s type representation from a simple `Known { class_name: "ErlangModule" }` to `Known { class_name: "ErlangModule", type_args: [Known("lists")] }` — using the existing generic type infrastructure from ADR 0068. The module name becomes a phantom type parameter.

**Diagnostics from typed FFI calls:**

```beamtalk
> Erlang lists reverse: 42
  ⚠️ Warning: lists:reverse/1 parameter 1 expects List, got Integer
  (type from stubs/otp/lists.d.bt:5)

> Erlang lists reverse: #(1, 2, 3)
  // type checker infers: List — downstream code gets typed
```

**Provenance tracking:** Types from `.d.bt` stubs have `TypeProvenance::Declared` (with span pointing into the stub file). Types from auto-extraction have a new `TypeProvenance::Extracted` variant. This distinction appears in diagnostics so users know where the type info came from.

### Stub Generation Tool

A `beamtalk stub-gen` command bootstraps `.d.bt` files from `.beam` specs:

```bash
# Generate stubs for specific modules
beamtalk stub-gen lists maps string file io ets

# Output: stubs/otp/lists.d.bt, stubs/otp/maps.d.bt, ...
# Auto-extracted types as starting point — human refines keyword names, tightens types
```

The generated stubs use `with:` for non-first parameters (matching the current FFI convention). The author then replaces `with:` with meaningful keyword names and tightens `Dynamic` params where appropriate.

**Example generated stub (before refinement):**

```beamtalk
// Auto-generated from lists.beam (OTP 27) — refine keyword names and types
declare ErlangModule: lists

  reverse: arg1 :: List -> List
  seq: arg1 :: Integer with: arg2 :: Integer -> List(Integer)
  member: arg1 :: Dynamic with: arg2 :: List -> Boolean
```

**After human refinement:**

```beamtalk
declare ErlangModule: lists

  reverse: list :: List(T) -> List(T)
  seq: from :: Integer to: to :: Integer -> List(Integer)
  member: elem :: T in: list :: List(T) -> Boolean
```

### Distribution Model

**Shipped with the compiler:**
- Curated `.d.bt` stubs for ~20 core OTP modules: `lists`, `maps`, `string`, `binary`, `file`, `io`, `ets`, `gen_server`, `supervisor`, `erlang`, `math`, `crypto`, `timer`, `os`, `re`, `unicode`, `proplists`, `sets`, `dict`, `queue`
- These live in the Beamtalk source tree under `stubs/otp/` and are installed with the compiler distribution
- Maintained by the Beamtalk project, tested against supported OTP versions

**Package-bundled stubs:**
- Beamtalk packages with native Erlang code (ADR 0072) can include `.d.bt` files for their native modules
- Declared in `beamtalk.toml`:

  ```toml
  [package]
  name = "http"
  version = "0.1.0"

  [stubs]
  # Stubs for this package's native Erlang modules
  path = "stubs/"
  ```

- Third-party packages can also publish stubs for Erlang libraries they don't own (analogous to `@types/xxx`):

  ```toml
  [package]
  name = "beamtalk-types-cowboy"
  version = "2.12.0"

  [stubs]
  path = "stubs/"
  provides-types-for = ["cowboy", "cowboy_req", "cowboy_router"]
  ```

**Auto-extract covers the rest:**
- Any `.beam` on the code path without a `.d.bt` override gets auto-extracted types
- This includes OTP modules without curated stubs, transitive Hex dependencies, and user Erlang code
- No manual work required — baseline typing is automatic

### REPL Experience

```beamtalk
> Erlang lists seq: 1 to: 10
#(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
// type: List(Integer) — from stubs/otp/lists.d.bt

> Erlang crypto hash: #sha256 with: "hello"
<<44, 242, ...>>
// type: String — auto-extracted from crypto.beam

> Erlang obscure_lib do_thing: 42
// type: Dynamic — no stub, no spec

> Erlang lists reverse: 42
⚠️ Warning: lists:reverse/1 parameter 1 expects List(T), got Integer
  (type from stubs/otp/lists.d.bt:5)
```

### Error Examples

**Stub parse error:**
```
error: Failed to parse stubs/otp/lists.d.bt:7
  Expected type annotation after '::', got ')'
  7 |   seq: from :: Integer to: to :: -> List(Integer)
                                         ^
```

**Arity mismatch between stub and actual function:**
```
⚠️ Warning: stubs/otp/lists.d.bt declares lists:seq/3 but lists.beam exports seq/2 and seq/3
  Stub declaration for lists:seq/3 will be used; lists:seq/2 has no stub (auto-extracted)
```

**Version drift detection:**
```
⚠️ Warning: stubs/otp/lists.d.bt declares lists:enumerate/1 but it is not exported from lists.beam
  This function may have been added in a newer OTP version
  Stub will be ignored for lists:enumerate/1
```

## Prior Art

### TypeScript — `.d.ts` and DefinitelyTyped

TypeScript's approach is the closest analogy. Libraries written in TypeScript auto-generate `.d.ts` declarations; JavaScript libraries get community-maintained type definitions via DefinitelyTyped (`@types/xxx` packages on npm).

**What we adopt:**
- Two-tier model: auto-generated baseline + hand-curated overrides
- Resolution chain with clear precedence (package types > `@types` > inferred)
- `stub-gen` tool analogous to `tsc --declaration`

**What we adapt:**
- We auto-extract from `.beam` specs instead of requiring everything to be hand-written. TypeScript has no equivalent of `-spec` in JavaScript — they must start from scratch. We have a rich starting point.
- We distribute core stubs with the compiler rather than a separate monorepo. DefinitelyTyped's monorepo at 8,000+ packages has serious scale problems. Beamtalk's community is small; bundling with the compiler is simpler.

**What we reject:**
- DefinitelyTyped's monorepo model — maintenance burden doesn't scale. Package-bundled stubs are preferred.

### Gleam — `@external` with Gleam Types

Gleam requires per-function `@external` declarations with Gleam type signatures for every Erlang function call. `gleam_stdlib` is essentially a curated, typed wrapper around OTP.

**What we adopt:**
- Gleam's insight that FFI boundaries are explicit trust boundaries — the compiler trusts declared signatures.

**What we reject:**
- Requiring declarations before calling Erlang. Beamtalk's `Erlang lists reverse: xs` works dynamically with zero setup. Stubs are additive — they improve type checking but aren't required. This preserves Principle 1 (Interactive-first).

### Kotlin/Native — `cinterop` Auto-Generation

Kotlin's `cinterop` tool reads C headers and auto-generates Kotlin stubs with mapped types. A `.def` file describes which headers to import.

**What we adopt:**
- Auto-generation from the foreign type system (C headers → Kotlin stubs, Erlang specs → Beamtalk types)
- Small descriptor files that point to what should be imported

**What we adapt:**
- Kotlin generates mandatory wrappers; we generate optional type info that improves checking without being required.

### Swift — Clang Module Maps

Swift imports C/Objective-C headers automatically via Clang's module system. Nullability annotations in C headers directly affect Swift's optional types.

**What we adopt:**
- The principle that quality of interop types depends on quality of source annotations. Well-specced Erlang modules get good types; poorly-specced ones fall back to `Dynamic`.

### Erlang Dialyzer — PLT Type Inference

Dialyzer builds a Persistent Lookup Table with success typings — types inferred from code analysis, supplementing declared `-spec` annotations.

**What we considered but deferred:**
- Reading PLT files to get inferred types for unspecced functions. PLT format is internal to Dialyzer and version-specific. The benefit (types for unspecced functions) doesn't justify the coupling. We fall back to `Dynamic` for unspecced functions instead.

## User Impact

### Newcomer
**Positive:** FFI calls that previously produced `Dynamic` now produce typed results. The LSP can offer typed completions when typing `Erlang lists` — showing `reverse: List(T) -> List(T)` instead of just `reverse/1`. Type errors at FFI boundaries are caught at compile time with actionable messages.

**Neutral:** Auto-extract means this works out of the box. No new concepts to learn — FFI calls look the same, they just get better checking.

**Risk:** Users may not understand why some Erlang functions have precise types and others return `Dynamic`. Mitigation: diagnostic messages include provenance ("type from lists.d.bt" vs "type from lists.beam -spec" vs "no type info available").

### Smalltalk Developer
**Positive:** The `.d.bt` stub format uses familiar Beamtalk keyword syntax. Meaningful keyword names in stubs make FFI calls read more like Smalltalk message sends: `Erlang lists seq: 1 to: 10` instead of `Erlang lists seq: 1 with: 10`.

**Concern:** A new file format (`.d.bt`) adds surface area. Mitigation: stubs are optional — the system works without them. Only library authors or power users need to write stubs.

### Erlang/BEAM Developer
**Positive:** Their existing `-spec` annotations are automatically used — no duplicate work. Types match their actual OTP version. Hex packages with good specs get automatic typing.

**Concern:** The Erlang→Beamtalk type mapping may lose precision (e.g., `non_neg_integer()` → `Integer`). Mitigation: this is a limitation of Beamtalk's type system, not the extraction mechanism. As Beamtalk's types become richer, the mapping improves.

### Operator
**Positive:** Auto-extraction means type info matches the deployed OTP version exactly. No risk of stubs claiming a function exists when it doesn't (or vice versa) — version drift detection warns on mismatches.

**Concern:** Build time increases slightly (reading `.beam` files for specs). Mitigation: results are cached per module; only re-extracted when `.beam` timestamps change. Incremental builds read zero `.beam` files.

### Tooling Developer
**Positive:** `ErlangTypeRegistry` is a clean, queryable data structure. The LSP can use it for completions, hover, and signature help on FFI calls. Provenance tracking enables "go to type definition" that jumps to the `.d.bt` file.

**Concern:** Two sources of truth (auto-extracted + stubs) means the registry merge logic must be correct. Mitigation: the merge is simple — stubs win per function/arity, everything else is auto-extracted.

## Steelman Analysis

### Best Argument for Pure `.d.bt` Stubs (No Auto-Extract)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "I can read the `.d.bt` file and see exactly what's available — it's self-documenting, like TypeScript's `.d.ts`" |
| **Smalltalk purist** | "Types should be curated by humans who understand the domain, not mechanically extracted from a foreign type system with its own quirks" |
| **BEAM veteran** | "Erlang specs are sometimes wrong or overly broad — `term()` everywhere. Hand-written stubs can be more honest about what a function actually accepts" |
| **Operator** | "No build-time dependency on reading `.beam` files; stubs are static, deterministic, cacheable" |
| **Language designer** | "Full control over the FFI type surface — we can evolve the stub format independently of Erlang's type system evolution" |

**Why we don't choose this:** The bootstrapping problem is severe. Beamtalk's community is small. Manually writing stubs for even 20 OTP modules is weeks of work, and the long tail of Hex packages would never get coverage. Auto-extract provides immediate, zero-effort baseline typing.

### Best Argument for Pure Auto-Extract (No Stubs)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "It just works — I don't need to find or install stub files. Every Erlang module is automatically typed" |
| **Smalltalk purist** | "The system should figure it out — I shouldn't write boilerplate declarations for things the machine already knows" |
| **BEAM veteran** | "My Erlang specs are already correct. Don't make me write them again in a different syntax" |
| **Operator** | "Types always match the actual OTP version deployed — no version drift, no surprises" |
| **Language designer** | "Minimal surface area — one mechanism, no new file format to design and maintain" |

**Why we don't choose this:** Auto-extract is limited by what Erlang specs express. `term()` maps to `Dynamic`. Keyword names are lost (everything is `with:`). The type quality ceiling is too low for a great developer experience on commonly-used modules.

### Tension Points

- **Newcomers and BEAM veterans** both prefer auto-extract — it requires zero effort. But **Smalltalk purists** want meaningful keyword names, which only stubs provide.
- **Operators** are split: auto-extract guarantees OTP version match, but stubs are deterministic and cacheable.
- **Language designers** worry about two-mechanism complexity, but acknowledge that neither layer alone is sufficient.
- The hybrid resolves these tensions: auto-extract for breadth, stubs for depth.

## Alternatives Considered

### Alternative A: Pure `.d.bt` Stubs (No Auto-Extract)

Require hand-written `.d.bt` files for every Erlang module that should have type info. Functions without stubs return `Dynamic`.

```beamtalk
// Must write this before lists:reverse gets typed
declare ErlangModule: lists
  reverse: list :: List(T) -> List(T)
  // ... hundreds more functions
```

**Rejected.** The bootstrapping problem is too severe for a small community. TypeScript could do this because millions of developers contributed to DefinitelyTyped. Beamtalk can't rely on community scale. Auto-extract gives immediate value for zero effort.

### Alternative B: Pure Auto-Extract (No Override Mechanism)

Read all types from `.beam` specs. No `.d.bt` files, no overrides.

**Rejected.** Quality ceiling is too low. Erlang specs use `term()` broadly, keyword names are lost, and overloaded specs can produce confusing unions. For the 20 most-used OTP modules, human curation meaningfully improves the developer experience. Without an override mechanism, there's no way to provide that.

### Alternative C: Wrapper Classes (Gleam-Style)

Require typed Beamtalk wrapper classes for every Erlang module:

```beamtalk
Object subclass: Lists
  reverse: list :: List(T) -> List(T) =>
    Erlang lists reverse: list
```

**Rejected.** Conflicts with Principle 1 (Interactive-first) — users must write a wrapper before calling any Erlang function. Beamtalk's `Erlang` proxy was specifically designed to avoid this. Also duplicates every function call through an unnecessary indirection layer.

### Alternative D: Dialyzer PLT Extraction

Read inferred types from Dialyzer's PLT files to supplement missing `-spec` annotations.

**Rejected (for now).** PLT format is internal to Dialyzer and changes between OTP versions. The coupling risk exceeds the benefit. Most commonly-used OTP functions have explicit `-spec` annotations. If we need types for unspecced functions in the future, this can be revisited as an additional layer below auto-extract in the resolution chain.

## Consequences

### Positive
- **Immediate baseline typing** for all specced Erlang functions — zero manual work via auto-extract
- **High-quality typing** for commonly-used OTP modules via curated stubs
- **Meaningful keyword names** in stubs improve documentation and LSP display (`seq: from to:` vs `seq: with:`). Note: keyword names in stubs are a **documentation feature** — the type checker matches by arity, not keyword names, because keyword names are stripped at dispatch (ADR 0028). `Erlang lists seq: 1 to: 10` and `Erlang lists seq: 1 with: 10` both resolve to the same `seq/2` stub entry
- **LSP integration** — typed completions, hover info, and signature help for Erlang modules
- **Compile-time FFI warnings** — catch type mismatches before runtime
- **Extensible** — packages can bundle stubs for their native Erlang code
- **Preserves interactive-first** — stubs are optional, FFI works without them
- **Version drift detection** — warns when stubs don't match actual `.beam` exports

### Negative
- **Two mechanisms** — auto-extract and stubs are two concepts to understand (mitigated by stubs being optional for most users)
- **Build time increase** — reading `.beam` specs adds time to first build (mitigated by caching)
- **Imprecise auto-extracted types** — `term()` → `Dynamic` means some functions get no useful type info from auto-extract alone
- **Stub maintenance** — curated OTP stubs must be updated when OTP adds/changes functions (mitigated by `stub-gen` tool and version drift detection)
- **New file format** — `.d.bt` is a new parser target, adding compiler surface area
- **Trust boundary** — stubs can declare incorrect types; the compiler trusts them. Incorrect stubs cause false diagnostics (mitigated by `stub-gen` producing correct starting points)
- **`+debug_info` dependency** — auto-extraction only works for `.beam` files compiled with `+debug_info`. Release-stripped packages and some precompiled Hex deps fall through to `Dynamic` (mitigated by `.d.bt` stubs for important packages)
- **Cold build cost** — first build must read `abstract_code` from all `.beam` files on the code path. For large dependency graphs (200+ modules), this may add several seconds. Results are cached in `_build/type_cache/` and invalidated by `.beam` timestamp changes — incremental builds have zero extraction overhead
- **Hot code reload staleness** — `ErlangTypeRegistry` is populated at build time. If an Erlang module is hot-reloaded mid-session with changed specs, the registry is stale until the next `beamtalk build`. This is a known limitation — type info is compile-time, dispatch is runtime. Consistent with ADR 0025's "compile-time only" principle

### Neutral
- Auto-extracted types have `TypeProvenance::Extracted` — diagnostics show the source
- Functions with no spec and no stub return `Dynamic` — identical to current behavior
- No runtime changes — this is entirely a compile-time/tooling feature
- The `.d.bt` parser is a subset of the existing Beamtalk parser (type annotations + method signatures, no bodies)

## Implementation

### Phase 0: Spec Extraction Spike

Validate the core assumption before building full infrastructure. ADR 0028 explicitly deferred this: "Requires a spike first."

1. Write `beamtalk_spec_reader.erl` that reads `abstract_code` from a single `.beam` file and extracts spec forms
2. Run it against `lists.beam` and `maps.beam` from the user's OTP installation — verify specs are present and parseable
3. Implement a minimal Erlang→Beamtalk type mapping for the extracted specs (just the core types: `integer()`, `list()`, `binary()`, `boolean()`, `atom()`)
4. Wire one end-to-end lookup: `Erlang lists reverse: #(1, 2, 3)` should resolve to return type `List` in the type checker
5. Verify against `.beam` files with and without `+debug_info` — confirm graceful fallback to `Dynamic`

**Validates:** `abstract_code` chunk availability, spec format parsing, type mapping correctness, build worker integration.

**Components:** `beamtalk_spec_reader.erl` (new), minimal `ErlangTypeRegistry` prototype, one type checker test

### Phase 1: Auto-Extract Infrastructure

Build the full Erlang spec reader and Rust integration:

1. **`beamtalk_spec_reader.erl`** — Extend the Phase 0 spike to handle all Erlang type forms, batch-process multiple modules, and emit results as structured terms via the build worker protocol
2. **Erlang→Beamtalk type mapping** — Rust module that converts the full range of Erlang abstract type representations to `InferredType` values (reverse of `spec_codegen.rs`), including generic type variables, union types, and the edge cases table above
3. **`ErlangTypeRegistry`** — new struct in the type checker that stores resolved function signatures
4. **Build integration** — invoke spec reader during `beamtalk build`, cache results per module
5. **Type checker integration** — look up FFI call types in the registry during inference

**Components:** `beamtalk_spec_reader.erl` (new), `crates/beamtalk-core/src/semantic_analysis/type_checker/erlang_types.rs` (new), `beam_compiler.rs` (extended), `inference.rs` (extended)

### Phase 2: `.d.bt` Stub Format and Parser

1. **Stub file format** — define the `.d.bt` grammar (subset of Beamtalk: `declare`, type annotations, method signatures)
2. **Stub parser** — parse `.d.bt` files into `FunctionSignature` entries
3. **Resolution chain** — merge stubs with auto-extracted types (stubs win per function/arity)
4. **Initial OTP stubs** — curate `.d.bt` files for 10 core modules: `lists`, `maps`, `string`, `file`, `io`, `ets`, `gen_server`, `erlang`, `math`, `crypto`

**Components:** `crates/beamtalk-core/src/parser/stub_parser.rs` (new), `stubs/otp/*.d.bt` (new), `ErlangTypeRegistry` (extended)

### Phase 3: `stub-gen` Tool and Package Integration

1. **`beamtalk stub-gen`** CLI command — reads `.beam` files and generates `.d.bt` stubs
2. **`beamtalk.toml` integration** — packages declare stubs for their native modules
3. **Dependency resolution** — collect stubs from transitive dependencies during build
4. **Expand curated stubs** to ~20 OTP modules

**Components:** `crates/beamtalk-cli/src/commands/stub_gen.rs` (new), `crates/beamtalk-cli/src/commands/build.rs` (extended), `beamtalk.toml` schema (extended)

### Phase 4: LSP Integration

Note: basic typed completions can optionally be pulled into Phase 1 or 2 since `ErlangTypeRegistry` is available as soon as auto-extract ships. The full LSP phase below covers the complete integration.

1. **Typed completions** for `Erlang <module>` — show function signatures with types
2. **Hover info** — display type signature and doc comment from `.d.bt` on hover
3. **Signature help** — show parameter types as user types arguments
4. **Go to type definition** — jump to `.d.bt` file for stub-typed functions
5. **Diagnostics** — surface type warnings from FFI calls in the editor

**Components:** `crates/beamtalk-lsp/src/completion_provider.rs` (extended), `crates/beamtalk-lsp/src/hover_provider.rs` (extended)

### Future Work

- **Richer type mapping** — as Beamtalk's type system grows (subrange types, typed maps, record types), the Erlang→Beamtalk mapping can become more precise
- **Elixir stubs** — `.d.bt` files for Elixir modules (`'Elixir.Enum'`, `'Elixir.String'`, etc.)
- **PLT integration** — read Dialyzer PLT for inferred types of unspecced functions (if needed)
- **AI-assisted stub generation** — use LLMs to generate meaningful keyword names from Erlang doc comments and parameter names in source
- **Community stub registry** — if the ecosystem grows large enough, a central registry (like DefinitelyTyped) may become worthwhile

## Migration Path

No existing behavior changes. This is purely additive:

- All existing FFI calls continue to work exactly as today
- Functions without type info (no stub, no spec) return `Dynamic` — same as current behavior
- Type warnings from FFI calls are new diagnostics — they don't prevent compilation
- No changes to runtime, codegen, or existing syntax

## References
- Related issues: BT-1823
- Related ADRs: ADR 0025 (gradual typing — the type system this plugs into), ADR 0028 (BEAM interop — the FFI mechanism this types), ADR 0055 (Erlang-backed classes — related FFI pattern), ADR 0068 (parametric types — generic type params in stubs), ADR 0070 (package namespaces — stub distribution via packages), ADR 0072 (user Erlang sources — native code that needs stubs)
- External: [TypeScript Declaration Files](https://www.typescriptlang.org/docs/handbook/declaration-files/introduction.html), [DefinitelyTyped](https://github.com/DefinitelyTyped/DefinitelyTyped), [Gleam External Functions](https://gleam.run/book/tour/external-functions.html), [Kotlin cinterop](https://kotlinlang.org/docs/native-c-interop.html)
