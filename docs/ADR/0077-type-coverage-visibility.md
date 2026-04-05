# ADR 0077: Type Coverage Visibility

## Status
Accepted (2026-04-05)

## Context

Beamtalk's gradual type system (ADR 0025) tracks rich type information internally: `InferredType` with `Known`, `Union`, and `Dynamic` variants; `TypeProvenance` distinguishing `Declared`, `Inferred`, `Substituted`, and `Extracted` sources; and a `TypeMap` mapping every expression span to its resolved type. The FFI layer (ADR 0075) auto-extracts Erlang specs into a `NativeTypeRegistry`. Return type writeback (BT-1005) infers method return types from bodies.

**Enforcement already exists.** ADR 0025 Phase 2b specified that `typed` classes warn on missing annotations. This is partially implemented — `check_typed_method_annotations` in `validation.rs` warns on missing parameter types and return types. Two gaps remain: state field annotations and Dynamic-inference warnings.

**Visibility does not exist.** Users have no way to see what the compiler knows:

- When an expression resolves to `Dynamic`, the LSP hover shows no type information at all — the type line is simply omitted. Users can't distinguish "the compiler knows the type" from "the compiler has no idea."
- There is no CLI command to report type coverage across a project.
- There is no way to track typing progress over time or gate CI on coverage regressions.

The enforcement (warnings on `typed` classes) tells developers what's wrong. The missing visibility would tell them what to *do* — and motivate them to do it. TypeScript's experience shows that making `any` visible on hover drove type adoption more effectively than `--noImplicitAny` enforcement. We already have enforcement; we need the visibility to complement it.

### Constraints

1. **Dynamic is valid** — `Dynamic` is not a bug; it's the correct type for genuinely dynamic code. Visibility must inform, not shame.
2. **Gradual adoption** — Teams add types incrementally. Tooling must support "start at 0%, improve over time."
3. **`typed` semantics are established** — ADR 0025 Phase 2b defines what `typed` means. This ADR completes the implementation and adds visibility, but does not change the contract.

## Decision

### 1. Add Dynamic Provenance

The `Dynamic` variant of `InferredType` gains a `DynamicReason` explaining *why* the type could not be determined. This is the foundation for all visibility features — hover, coverage detail, and diagnostic messages.

```rust
/// Why a type could not be determined.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynamicReason {
    /// Parameter has no type annotation.
    UnannotatedParam,
    /// Method has no return type annotation and body could not be inferred.
    UnannotatedReturn,
    /// Receiver is Dynamic, so message send result is Dynamic.
    DynamicReceiver,
    /// Control flow produces incompatible types (pre-union-narrowing fallback).
    AmbiguousControlFlow,
    /// Erlang FFI call with no spec or all-Dynamic spec.
    UntypedFfi,
    /// Fallback — no specific reason available.
    Unknown,
}

pub enum InferredType {
    Known { class_name, type_args, provenance },
    Union { members, provenance },
    Dynamic(DynamicReason),  // was: Dynamic (bare unit variant)
}
```

The `PartialEq` impl continues to ignore the reason (all `Dynamic` values are equal), matching how provenance is ignored on `Known`/`Union`. The `union_of` helper, which short-circuits to `Dynamic` when any member is Dynamic, will propagate the reason from the Dynamic member. When multiple Dynamic members are present or when the union itself is the source of ambiguity, the reason is `AmbiguousControlFlow`.

### 2. Show Dynamic in LSP Hover

When an expression's inferred type is `Dynamic`, the hover provider displays it explicitly with the reason:

```
Identifier: `handler` — Type: Dynamic (no return annotation on getHandler)
```

```
Identifier: `result` — Type: Dynamic (receiver is Dynamic)
```

```
Identifier: `data` — Type: Dynamic (parameter has no type annotation)
```

When the reason is `Unknown`, the hover shows just `Type: Dynamic`.

This replaces the current behavior of omitting the type line entirely. The reason tells you exactly what to fix: add a return type annotation, add a parameter type, or trace back to the source of a Dynamic receiver.

Note: `display_name()` currently returns `None` for `Dynamic`, which all callers interpret as "no type to display." This must change to return `Some("Dynamic")` (or a reason string). All callers of `display_name()` that branch on `None` must be audited — the hover provider at lines 563 and 639 of `hover_provider.rs` both use `.and_then(InferredType::display_name)` and will need updating.

### 3. `beamtalk type-coverage` CLI Command

A new CLI command reports type coverage statistics per class and per file:

```
$ beamtalk type-coverage
Type Coverage Report
====================

File                          Class              Coverage
src/AccountService.bt         AccountService     92.3%  (48/52 expressions)
src/Router.bt                 Router             87.1%  (54/62 expressions)
src/ConfigLoader.bt           ConfigLoader       71.4%  (30/42 expressions)
src/MyApp.bt                  MyApp              45.0%  (9/20 expressions)
──────────────────────────────────────────────────────────────────────
Total                                            80.1%  (141/176 expressions)
```

Coverage is defined as: **(expressions with non-Dynamic inferred type) / (total expressions in the TypeMap)**.

**What counts as an expression:** Every AST node that produces a `TypeMap` entry — identifiers, message sends, assignments, literals, block bodies, and binary operations. Sub-expressions are counted individually: `self balance + 1` contributes three entries (the `self balance` send, the literal `1`, and the `+` send). Literal values (integers, strings, booleans) always resolve to Known types and contribute positively. This matches the formula used by TypeScript's `type-coverage` and Flow's `flow coverage`, which also count sub-expressions.

**Scope:** Coverage reports only the project's own classes (files under the project source directories). Dependencies and stdlib classes are excluded. This prevents project coverage from being dominated by stdlib quality.

**Known limitation:** Beamtalk's block-as-first-class-object model means block parameters and block bodies create TypeMap entries that are often Dynamic (stored blocks lose type context). Classes using heavy collection/iteration idioms (`collect:`, `inject:into:`) may show systematically lower coverage than equivalent recursive code. This is a known distortion in the metric — the coverage percentage is a progress indicator, not a quality score.

#### Flags

- **`--detail`** — Show each Dynamic expression with file:line:col location and reason
- **`--format json`** — Machine-readable output for dashboards and CI integration
- **`--at-least N`** — Exit non-zero if total coverage < N% (ratchet for CI)
- **`--class ClassName`** — Filter to a specific class

#### Example: Detail Mode

```
$ beamtalk type-coverage --detail --class MyApp
Type Coverage: MyApp (src/MyApp.bt) — 45.0% (9/20)

  Dynamic expressions:
    src/MyApp.bt:12:5   handler := getHandler        (no return annotation on getHandler)
    src/MyApp.bt:15:9   result := handler process:    (receiver is Dynamic)
    src/MyApp.bt:18:5   items collect: [:x | x transform]  (receiver is Dynamic)
    ...
```

#### Example: CI Ratchet

```yaml
# In CI pipeline
- run: beamtalk type-coverage --at-least 75 --format json
```

```json
{
  "total_expressions": 176,
  "typed_expressions": 141,
  "coverage_percent": 80.1,
  "threshold": 75,
  "passed": true,
  "classes": [
    {
      "name": "AccountService",
      "file": "src/AccountService.bt",
      "total": 52,
      "typed": 48,
      "coverage_percent": 92.3
    }
  ]
}
```

Note: The `DynamicReason` strings in JSON output (e.g., `"no return annotation on getHandler"`) are informational and may change as the type checker improves. CI scripts should gate on `coverage_percent` and `passed`, not on specific reason strings.

### 4. Complete `typed` Class Diagnostics

ADR 0025 Phase 2b specifies that `typed` classes warn on missing annotations. Two gaps remain in the current implementation:

#### State field annotations (gap)

ADR 0025 Phase 2b: "All state fields should have type annotations (warn if missing)."

```beamtalk
typed Actor subclass: BankAccount
  state: balance = 0          // warning: missing type annotation for state field
                               //          `balance` in typed class `BankAccount`
  state: owner :: String       // OK
```

This parallels the existing `check_typed_method_annotations` logic — check `StateDeclaration.type_annotation.is_none()` when the class is typed.

#### Dynamic inference warning (gap)

When an expression in a `typed` class infers as Dynamic, warn — the user opted into thorough checking and should know where the compiler can't help.

```beamtalk
typed Actor subclass: BankAccount
  process: handler =>
    handler doWork    // warning: expression inferred as Dynamic in typed class
                      //          `BankAccount` (parameter has no type annotation)
```

This uses the new `DynamicReason` from Phase 1 to produce actionable messages. Where Dynamic dispatch is intentional, suppress with `@expect type`:

```beamtalk
typed Actor subclass: BankAccount
  process: handler =>
    @expect type
    handler doWork    // no warning — suppressed
```

## Prior Art

### TypeScript
- **Hover shows `any` explicitly** — the single most effective mechanism for driving type adoption. Users see the gap, understand the consequence, and add annotations because they want better tooling, not because the compiler demands it.
- **`type-coverage` npm package** (plantain-00): `(non-any identifiers) / (total identifiers)` = percentage. Supports `--at-least N` for CI, `--detail` for locations, `--json-output` for dashboards.
- **`--noImplicitAny`**: enforcement added years after visibility. Both work together — visibility motivates, enforcement catches regressions.
- **Adopted**: expression-count formula, `--at-least` ratchet, `--detail` flag, showing Dynamic in hover with reason.

### Sorbet (Ruby)
- **Per-file sigil system**: `# typed: false` → `true` → `strict` → `strong`. Five levels of increasing strictness.
- **Metrics JSON**: `srb tc --metrics-file` emits typed/untyped counts for dashboards.
- **Adopted**: metrics JSON concept for CI integration. **Not adopted**: sigil comments (Beamtalk uses the `typed` keyword in source).

### mypy (Python)
- **`--any-exprs-report`**: per-module table of `Anys / Exprs / Coverage%`.
- **Adopted**: per-module reporting format, expression-level coverage metric.

### Flow (Facebook)
- **`flow coverage file.js`**: per-file coverage with `--color` for terminal highlighting.
- **Adopted**: per-file coverage command with detail mode.

### Elixir
- **No coverage tooling**: Elixir's new set-theoretic types infer without user intervention but provide no visibility into what's typed vs dynamic.
- **Lesson**: invisible inference is great for correctness but leaves users unable to answer "am I writing good code?"

## User Impact

### Newcomer (from Python/JS/Ruby)
- **Hover showing Dynamic with reason**: immediately familiar from TypeScript's `any` display, but better — the reason tells you what to fix. No learning curve.
- **`type-coverage`**: clear metric for tracking progress, similar to tools they've used.
- **Risk**: may feel pressure to eliminate all Dynamic. Mitigation: docs should explain that Dynamic is correct for genuinely dynamic code.

### Smalltalk Developer
- **Minimal disruption**: coverage is purely informational. Dynamic code works exactly as before.
- **Hover with provenance**: fits the live, exploratory workflow — inspect why something is Dynamic without leaving the editor.
- **Potential concern**: "type coverage metrics" feels alien. Mitigation: frame as "what does the compiler know about your code?"

### Erlang/BEAM Developer
- **Familiar model**: similar to Dialyzer — reports what it can prove, stays silent otherwise.
- **FFI visibility**: `type-coverage` shows where Erlang call types resolve via `NativeTypeRegistry` vs fall back to Dynamic.
- **CI integration**: JSON output + `--at-least` maps to familiar CI patterns.

### Production Operator
- **Ratchet mechanism**: `--at-least N` prevents coverage regressions without demanding 100%.
- **JSON output**: integrates with existing dashboards.

## Steelman Analysis

The core decision — showing Dynamic on hover, adding a coverage CLI, completing the `typed` diagnostic gaps — is uncontroversial. No cohort would argue against making invisible information visible. The one real deferred decision is tiered strictness.

### For Tiered Strictness Now (`typed strict`, `typed strong`)

| Cohort | Argument |
|--------|----------|
| **Operator** | "I want `typed strong` for payment processing — zero Dynamic allowed. But my formatting helpers should stay loose. One tier forces me to choose between strictness on critical code and annotation noise on trivial code." |
| **Language designer** | "Sorbet proved tiered strictness works at scale (Stripe, Shopify). You're building the infrastructure (`DynamicReason`) that makes tiers trivial — why not ship them while you're in the code?" |
| **BEAM veteran** | "My gen_server callback module needs strict checking because message dispatch is safety-critical. The current `typed` warns on missing annotations but still allows Dynamic — that's not enough for code that handles money." |

**Why deferred**: The current `typed` semantics (warn on missing annotations, check message sends) cover the most common use case. Tiered strictness is a language design decision with naming, inheritance, and interaction implications that deserve their own ADR. The visibility tooling shipped here provides the usage data to design the right tiers — which classes do teams mark `typed`? What coverage levels do they target? Where do they use `@expect type`? Ship, observe, then design tiers with evidence.

## Alternatives Considered

### Alternative: Visibility Only (No Diagnostic Completion)
Ship hover + coverage CLI without completing the `typed` diagnostic gaps.

**Not chosen**: The state field warning is a few lines of code alongside the existing `check_typed_method_annotations`. The Dynamic-inference warning is trivial once `DynamicReason` exists. Deferring them creates artificial sequencing — the infrastructure this ADR builds (`DynamicReason`) makes them essentially free.

### Alternative: Global `--no-implicit-dynamic` Flag
A build flag that makes Dynamic inference a warning everywhere, not just in `typed` classes.

**Rejected**: This is the global strict mode that ADR 0025 explicitly rejected. "The same code behaves differently depending on who runs it." The `typed` modifier puts enforcement in the source where it's visible and consistent.

### Alternative: Per-File Sigils (Sorbet-style)
A comment at the top of each `.bt` file controlling strictness.

**Rejected**: Beamtalk already has the `typed` keyword on the class declaration. Adding file-level sigils creates two competing mechanisms. The class-level modifier is more granular (a file can contain both typed and untyped classes) and more visible.

### Alternative: IDE-Only Coverage (No CLI Command)
Show coverage only through LSP (hover, code lens, diagnostics) without a CLI tool.

**Rejected**: CI integration requires a CLI command. Teams need to track coverage over time, gate PRs on regressions, and generate reports for dashboards.

## Consequences

### Positive
- Users can see what the compiler knows — Dynamic is no longer invisible
- Provenance reasons answer "why is this Dynamic?" directly in the editor
- `typed` classes deliver on the full ADR 0025 Phase 2b contract (state fields + Dynamic inference)
- CI teams can ratchet coverage with `--at-least` — prevents regression without demanding perfection
- JSON output enables dashboard integration and progress tracking
- No behavior changes for untyped code — purely additive

### Negative
- Showing "Dynamic" on hover may initially confuse newcomers — needs documentation
- `type-coverage` command adds CLI surface area and maintenance burden
- Expression-count coverage can be misleading for block-heavy code (see Known Limitation above)
- Return type writeback (BT-1005) can improve coverage retroactively — adding annotations to method M may change coverage of M's callers in other files, causing unexplained jumps in tracking
- Dynamic-inference warning in `typed` classes may produce noise for intentionally dynamic dispatch — mitigated by `@expect type`

### Neutral
- Coverage percentage is a proxy metric, not a quality measure — 100% is neither required nor always desirable
- The formula measures compiler knowledge, not user annotations — good inference yields high coverage with zero annotations
- FFI coverage (from `NativeTypeRegistry`) contributes to the score — calling well-spec'd Erlang modules improves coverage automatically

## Implementation

### Phase 1: Dynamic Provenance + LSP Hover (M)

**Affected components**:
- `crates/beamtalk-core/src/semantic_analysis/type_checker/types.rs` — add `DynamicReason` enum; change `Dynamic` from bare variant to `Dynamic(DynamicReason)`; update `display_name()` to return `Some("Dynamic")` or `Some("Dynamic (reason)")` instead of `None`
- `crates/beamtalk-core/src/semantic_analysis/type_checker/inference.rs` — update each site producing `InferredType::Dynamic` (~58 sites) to supply the appropriate `DynamicReason`
- `crates/beamtalk-core/src/semantic_analysis/type_checker/validation.rs` — update `Dynamic` pattern matches (~9 sites)
- `crates/beamtalk-core/src/semantic_analysis/type_checker/native_types.rs` — update FFI Dynamic sites (~6 sites) to use `UntypedFfi`
- `crates/beamtalk-core/src/semantic_analysis/type_checker/types.rs` — `union_of` helper: propagate reason from Dynamic member; use `AmbiguousControlFlow` when multiple Dynamic members are present
- `crates/beamtalk-core/src/queries/hover_provider.rs` — display Dynamic with reason string; audit all `display_name()` callers for the `None` → `Some` behavior change

**Tests**: Update type checker tests (~16 sites) to use `Dynamic(DynamicReason::Unknown)` or the appropriate specific reason.

### Phase 2: Complete `typed` Diagnostics (S)

**Affected components**:
- `crates/beamtalk-core/src/semantic_analysis/type_checker/validation.rs` — add `check_typed_state_annotations` (parallel to existing `check_typed_method_annotations`); add Dynamic-inference warning for `typed` classes using `DynamicReason`
- `crates/beamtalk-core/src/semantic_analysis/type_checker/inference.rs` — call the new state annotation check when `is_typed`

**Tests**: Add test cases for state field warnings and Dynamic-inference warnings in `typed` classes, including `@expect type` suppression.

### Phase 3: CLI Command (M)

**Affected components**:
- `crates/beamtalk-cli/src/commands/` — new `type_coverage.rs` command module
- `crates/beamtalk-core/src/semantic_analysis/type_checker/mod.rs` — expose coverage calculation from `TypeMap`

Add a `CoverageReport` struct that walks the `TypeMap` and classifies each entry as typed (Known/Union) or untyped (Dynamic). The CLI command compiles the project (reusing the `build` pipeline), collects TypeMaps per module, and formats the report.

**Tests**: Integration tests compiling fixture `.bt` files with known type distributions, asserting expected coverage percentages and JSON output structure.

### Future Work

- **Tiered strictness** (`typed strict`, `typed strong`) — deferred to a future ADR informed by usage patterns
- **REPL `:coverage` command** — deferred until stale-state semantics are designed for incremental re-analysis
- **Code lens** — per-class/method coverage displayed inline in the editor (LSP `textDocument/codeLens`)
- **Differential coverage** — per-PR coverage delta for CI integration (like Codecov)

## Implementation Tracking

**Epic:** BT-1910
**Issues:**
- BT-1911: Add DynamicReason enum and update InferredType::Dynamic variant (M)
- BT-1912: Show Dynamic with reason in LSP hover (S) — blocked by BT-1911
- BT-1913: Warn on missing state field annotations in typed classes (S) — blocked by BT-1911
- BT-1914: Warn on Dynamic inference in typed classes (S) — blocked by BT-1911
- BT-1915: Add beamtalk type-coverage CLI command (M) — blocked by BT-1911
- BT-1916: Update docs for type coverage visibility (S) — blocked by BT-1912, BT-1913, BT-1914, BT-1915
**Status:** Planned

## References
- Related ADRs: [ADR 0025 (Gradual Typing)](0025-gradual-typing-and-protocols.md), [ADR 0045 (REPL Type Inference)](0045-repl-expression-completion-type-inference.md), [ADR 0053 (Type Annotation Syntax)](0053-double-colon-type-annotation-syntax.md), [ADR 0068 (Parametric Types)](0068-parametric-types-and-protocols.md), [ADR 0075 (FFI Type Definitions)](0075-erlang-ffi-type-definitions.md)
- External: [plantain-00/type-coverage](https://github.com/plantain-00/type-coverage), [Sorbet Metrics](https://sorbet.org/docs/metrics), [mypy coverage reports](https://mypy.readthedocs.io/en/stable/command_line.html#report-generation), [Flow coverage](https://flow.org/en/docs/cli/coverage/)
- Documentation: `docs/beamtalk-language-features.md` (Gradual Typing section)
