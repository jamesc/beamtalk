---
name: plan-refactor
description: Analyze the codebase and plan refactoring for code quality and maintainability. Use when user types /plan-refactor or asks to plan refactoring work.
---

# Plan Refactor Workflow

Perform a **whole-repo analysis** to identify refactoring opportunities that improve code quality, maintainability, and alignment with architectural principles. Produce **actionable Linear issues** ranked by impact.

**Key Philosophy:** This is analysis and planning, not implementation. The output is a prioritized set of well-specified issues that agents can pick up via `/pick-issue`. Focus on high-impact structural improvements, not cosmetic changes.

## Steps

1. **Establish scope**: Ask the user if they want to focus on a specific area or analyze the whole repo:
   - Whole repo (default)
   - Specific crate (`beamtalk-core`, `beamtalk-cli`)
   - Specific layer (parser, codegen, runtime, REPL)
   - Specific concern (DDD alignment, error handling, test coverage, etc.)

2. **Gather current state**: Run quick diagnostics to understand the codebase health:
   ```bash
   # Code metrics
   find crates/ -name '*.rs' | xargs wc -l | tail -1       # Rust LOC
   find runtime/apps/ -name '*.erl' | xargs wc -l | tail -1 # Erlang LOC
   
   # Module sizes (files > 500 lines are candidates for splitting)
   find crates/ -name '*.rs' -exec wc -l {} + | sort -rn | head -20
   find runtime/apps/ -name '*.erl' -exec wc -l {} + | sort -rn | head -20
   
   # Test coverage gaps (modules without corresponding test files)
   # Rust: check for #[cfg(test)] modules
   grep -rL '#\[cfg(test)\]' crates/*/src/**/*.rs 2>/dev/null | head -20
   
   # Clippy warnings (non-blocking)
   cargo clippy --all-targets 2>&1 | grep "warning:" | sort | uniq -c | sort -rn | head -20
   
   # TODO/FIXME/HACK markers
   grep -rn 'TODO\|FIXME\|HACK\|XXX' crates/ runtime/apps/ --include='*.rs' --include='*.erl' | head -30
   
   # Churn hotspots (files that change most often — poorly factored or central)
   git log --since="3 months ago" --format=format: --name-only | sort | uniq -c | sort -rn | head -20
   
   # Merge conflict magnets (large files with high churn)
   # Cross-reference churn hotspots with file sizes above
   ```

3. **Analyze architecture against principles**: Check each concern area:

   **a. DDD Alignment** (reference: `docs/beamtalk-ddd-model.md`)
   - Modules using generic names instead of domain terms
   - Missing bounded context annotations (`//! **DDD Context:**`)
   - Domain concepts in code that aren't in the DDD model doc
   - Cross-context dependencies (e.g., codegen importing runtime internals)

   **b. Code Organization**
   - God files (>500 lines) that should be split
   - Related functions scattered across unrelated modules
   - Inconsistent module structure between similar components
   - Dead code or unused imports

   **c. Error Handling**
   - Bare `unwrap()` on user input (should be proper error handling)
   - Missing error context (`.map_err()` without context)
   - Inconsistent error types across module boundaries
   - Runtime: bare tuple errors instead of `#beamtalk_error{}`

   **d. Test Quality**
   - Modules with no tests or low coverage
   - Tests that test implementation details instead of behavior
   - Missing E2E tests for user-facing features
   - Duplicated test setup that could be extracted

   **e. API Surface**
   - Public functions that should be private
   - Missing documentation on public APIs
   - Inconsistent function signatures for similar operations
   - Missing type annotations or specs

   **f. Performance Concerns**
   - Unnecessary allocations in hot paths
   - O(n²) or worse algorithms
   - Unnecessary cloning where borrows would work (Rust)
   - Unbounded data structures

   **g. Dependency Health**
   - Unused dependencies in Cargo.toml
   - Outdated dependencies with known issues
   - Missing `# why:` comments for non-obvious dependencies

   **h. Coupling & Cohesion**
   - Shotgun surgery: one logical change requires touching many unrelated files
   - Feature envy: functions that use another module's data more than their own
   - Internal details leaking across module boundaries (public API too wide)
   - Low cohesion: module has unrelated responsibilities that should be split
   - High churn files: if a file changes in every PR, it may be a coupling bottleneck

   **i. Change Velocity & Developer Friction**
   - Churn hotspots (files modified in most PRs) — central or poorly factored?
   - Merge conflict magnets (large files touched by multiple features simultaneously)
   - Onboarding friction: code that requires AGENTS.md to explain should be clearer
   - Boilerplate that could be reduced with macros, traits, or code generation

   **j. Compilation Pipeline Contracts** (Beamtalk-specific)
   - Pipeline stage boundaries (lexer → parser → semantic → codegen → runtime) — are they independently testable with clear interfaces?
   - Runtime/codegen contract: do they agree on calling conventions, state shape, error format? Drift here causes subtle bugs.
   - Generated Core Erlang patterns: are they consistent across similar AST nodes? Inconsistency means runtime bugs.
   - BEAM interop surface: are Erlang module/function names predictable and documented?

4. **Cross-reference with existing issues**: Check Linear for already-planned refactoring:
   ```
   Search Linear for: label:Refactor state:Backlog,Todo
   ```
   Avoid duplicating work that's already tracked. Note any existing issues that overlap.

5. **Rank findings by impact**: Score each finding on two axes:

   | Factor | Weight | Description |
   |--------|--------|-------------|
   | **Blast radius** | High | How many files/features does this affect? |
   | **Bug risk** | High | Could this cause bugs or security issues? |
   | **Developer friction** | Medium | Does this slow down development? |
   | **DDD drift** | Medium | Does this cause naming/structure confusion? |
   | **Performance** | Low | Is this in a hot path? |
   | **Cosmetic** | Skip | Pure style with no functional impact |

   **Skip cosmetic-only findings.** Only report things that have actionable impact.

6. **Present findings to user**: Show a ranked summary before creating issues:

   ```markdown
   ## Refactoring Analysis: [Scope]
   
   ### 🔴 High Impact (address soon)
   1. **[Finding]** - [file(s)] - [why it matters]
   2. ...
   
   ### 🟡 Medium Impact (plan for next cycle)
   3. **[Finding]** - [file(s)] - [why it matters]
   4. ...
   
   ### 🟢 Low Impact (nice to have)
   5. **[Finding]** - [file(s)] - [why it matters]
   6. ...
   
   ### Already Tracked
   - BT-XXX covers [finding] (skip)
   
   ### Skipped
   - [N] cosmetic-only findings omitted
   ```

   Ask the user which findings to create issues for before proceeding.

7. **Create Linear issues**: For each approved finding, create a well-specified issue using the create-issue skill patterns:
   - **Title**: Clear, action-oriented (e.g., "Refactor: Split god file parser/mod.rs into focused modules")
   - **Labels**: `Refactor` type + appropriate area label + size estimate
   - **Agent state**: `agent-ready` (refactoring should be self-contained)
   - **Body**: Context, specific files, acceptance criteria, what NOT to change
   - **Dependencies**: Set blocking relationships if order matters

   Group related findings into a single issue when they affect the same files or concern.

8. **Create Epic if needed**: If there are 5+ related issues, create an Epic to group them:
   - Title: `Epic: [Refactoring Theme]` (e.g., "Epic: DDD Alignment for Codegen Layer")
   - Link all child issues with blocking relationships
   - Add progress tracking template

9. **Summary**: Present final output:
   ```markdown
   ## Refactoring Plan Complete
   
   **Scope:** [what was analyzed]
   **Issues created:** [count] ([list with IDs])
   **Epic:** BT-XXX (if created)
   **Estimated total size:** [S/M/L/XL]
   **Recommended order:** [which issues to tackle first and why]
   ```

---

## Analysis Guidelines

### What Makes Good Refactoring

✅ **Good refactoring targets:**
- God files that are hard to navigate and modify
- Duplicated logic that causes bugs when only one copy is updated
- Wrong abstractions that make new features harder to add
- Missing error handling that causes confusing failures
- Test gaps in critical paths
- DDD violations that cause naming confusion
- High-churn files that cause merge conflicts across features
- Tight coupling that forces shotgun surgery on every change
- Leaky abstractions where internals bleed across module boundaries

❌ **Skip these:**
- Renaming for personal preference (unless DDD requires it)
- Rewriting working code in a "better" style
- Premature optimization without evidence of bottleneck
- Splitting small files that are already cohesive
- Adding abstractions that only have one implementation

### Safety Principles

Every refactoring issue MUST include these constraints:

1. **Refactor under test** — Never refactor code that isn't tested. If tests are missing, the issue should add them first (or depend on a test issue).
2. **Incremental delivery** — Each refactoring PR must leave the code working. No "big bang" rewrites. If a refactoring is too large for one PR, split into sequential issues.
3. **Behavioral preservation** — Refactoring changes structure, not behavior. If behavior changes, that's a feature or bugfix — create a separate issue. Acceptance criteria should include "all existing tests pass without modification" (unless tests were testing implementation details).
4. **Prove the problem** — Include evidence in the issue: churn count, file size, coupling example, or concrete developer friction. "This feels messy" is not enough.

### Size Estimation

| Size | Scope | Example |
|------|-------|---------|
| **S** | 1-2 files, <50 lines changed | Extract a helper function, add missing error context |
| **M** | 3-5 files, 50-200 lines | Split a module, consolidate duplicated logic |
| **L** | 5-10 files, 200-500 lines | Reorganize a subsystem, introduce new abstraction |
| **XL** | 10+ files, 500+ lines | Cross-cutting architectural change (Epic candidate) |

### Beamtalk-Specific Concerns

**Rust compiler crates:**
- Parser functions should be small and composable (one per grammar production)
- Codegen should mirror AST structure (one function per AST node type)
- Error recovery: parsers should return partial results + diagnostics, never panic
- Public API should use `(Result, Vec<Diagnostic>)` pattern

**Erlang runtime:**
- All errors must use `#beamtalk_error{}` records
- All logging must use OTP `logger` module
- Gen_server callbacks should be thin (delegate to helper functions)
- Specs (`-spec`) required on all public functions
- License headers on all files

**Cross-cutting:**
- Module names must match DDD ubiquitous language
- Bounded context annotations in module headers
- Architecture principles: `docs/development/architecture-principles.md`
