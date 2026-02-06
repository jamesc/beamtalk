---
name: review-code
description: Review current branch changes vs main. Use when user types /review-code or asks for a code review of their changes.
---

# Code Review Workflow

Perform a **deep, thorough code review** focused on **shipping high-quality code**. Take time to understand the code's intent and context. Prioritize catching issues missed during implementation: bugs, edge cases, security vulnerabilities, performance bottlenecks, and maintainability problems.

**Key Philosophy:** Complete improvements during review rather than deferring to future PRs. If something can be done well in <2 hours, implement it now. The goal is to ship excellent code the first time, not to create a backlog of technical debt.

## Steps

1. **Identify the base branch**: Determine the comparison point:
   ```bash
   git fetch origin main
   git merge-base HEAD origin/main
   ```

2. **Get the diff**: Fetch all changes between current branch and main:
   ```bash
   git --no-pager diff $(git merge-base HEAD origin/main)..HEAD
   ```

3. **List changed files**: Get a summary of what files were modified:
   ```bash
   git --no-pager diff --stat $(git merge-base HEAD origin/main)..HEAD
   ```

4. **Review each changed file** against the guidelines below.

5. **DDD compliance check**: For each new or renamed module/type/function:
   - Does the name match the ubiquitous language in `docs/beamtalk-ddd-model.md`?
   - Is the bounded context documented in the module header?
   - If a new domain concept is introduced, is it added to the DDD model doc?
   - Are dependency directions correct (core never imports cli/lsp)?
   
   If the DDD model doc needs updating, update it as part of the review.

6. **Test coverage check**: Explicitly ask yourself:
   - "Are there any new tests needed for these changes?"
   - "Can I write an E2E test for this?" (check `tests/e2e/cases/*.bt`)
   - "Are there edge cases in the changed code that aren't tested?"
   - "Did any existing tests need updating due to behavioral changes?"
   
   If tests are missing, add them as part of the review (see step 7).

7. **Implement recommended suggestions**: For anything that can be done well in <2 hours, implement it directly. This includes:
   - Fixing bugs and edge cases
   - Adding missing tests
   - Improving documentation
   - Refactoring for clarity
   - Adding error handling
   - Improving naming and DDD alignment
   - Updating `docs/beamtalk-ddd-model.md` for new domain concepts
   - **Philosophy:** Ship high quality the first time. Don't defer easy work to future PRs.

8. **Verify tests pass**: After implementing any changes, run the full CI suite:
   ```bash
   just ci
   ```
   This runs all CI checks (build, clippy, fmt-check, test, test-e2e).
   If fixes introduced new failures, address them before proceeding.

9. **Create issues ONLY for substantial changes**: Only create Linear issues for work that:
   - Requires architectural design decisions
   - Affects multiple components significantly
   - Needs cross-team coordination or breaking changes
   - Would fundamentally change the PR's scope
   - **Remember:** The bar for "separate PR" is HIGH. When in doubt, implement it now.

10. **Summary**: End with an overall assessment:
   - Is the code ready to merge?
   - What are the main strengths of the changes?
   - What changes were implemented during review
   - What issues were created for follow-up work

---

## Review Guidelines

### Action Philosophy

**Quality First: Complete work now, don't defer easy fixes.**

The goal is to ship high-quality code the first time. If something can be fixed or improved during review without significant architectural changes, do it immediately. Don't create technical debt by deferring straightforward work.

### Action Decision Matrix

**Implement immediately (fix in place):**
- Bugs, logic errors, security vulnerabilities
- Missing error handling or edge cases
- Formatting/style violations
- Unclear names or missing documentation
- Simple to moderate refactoring (extract function, reduce nesting, improve clarity)
- Missing unit tests for new code
- Incomplete error messages or logging
- Additional test cases for better coverage
- Type annotations, doc comments, examples
- Performance improvements with clear solutions
- **Rule of thumb:** If it can be done well in <2 hours, do it now. Don't create future work.

**Create Linear issue ONLY for:**
- Architectural refactoring affecting many files/components
- New features genuinely beyond PR scope
- Performance optimizations requiring extensive benchmarking/profiling/research
- Security improvements requiring design discussions with stakeholders
- Breaking API changes requiring coordination
- Changes that would 2-3x the PR size
- Work requiring significant new dependencies or infrastructure
- **Rule of thumb:** Only defer if it requires design decisions, cross-team coordination, or would fundamentally change the PR's purpose

**When in doubt, implement it.** Bias toward completing work rather than creating follow-up issues. The bar for "this needs a separate PR" should be high.

### General
- Flag unused variables, imports, or dead code.
- Check for null/undefined handling, bounds checking, and error propagation.
- Ensure consistent naming (camelCase for JS/TS, snake_case for Python/Rust), avoid abbreviations.
- Limit functions to <50 lines; suggest refactoring if violated.
- Avoid deeply nested conditionals (>3 levels); prefer early returns or guard clauses.

### Security Checks
- Validate/sanitize all inputs (user data, APIs, files).
- Prevent injection (SQL, XSS, command); flag string concatenation in queries.
- Use secure defaults: HTTPS, prepared statements, no hard-coded secrets.
- Check auth/authorization: role checks, token validation.
- Flag unsafe deserialization, regex DoS risks.

### Performance & Efficiency
- Optimize loops: prefer map/filter/reduce over imperative where appropriate.
- Avoid N+1 queries; suggest batching/indexing.
- Flag expensive regex/operations in hot paths.
- Use efficient data structures (e.g., Sets for lookups).

### Testing & Documentation
- Ensure new code has accompanying tests (unit/integration).
- Flag missing types (TS), docstrings (Python), or JSDoc.
- Suggest assertions for edge cases.

### Domain Driven Design (DDD) Compliance

**Reference:** `docs/beamtalk-ddd-model.md` defines the authoritative domain model.

**Naming & Ubiquitous Language:**
- Module/type/function names MUST use domain terms from the DDD model, not generic technical terms.
  - ✅ `CompletionProvider`, `DiagnosticProvider`, `ClassHierarchy`
  - ❌ `completions`, `diagnostics`, `class_utils`
- If a new domain term is introduced (new type, module, or concept), verify it exists in `docs/beamtalk-ddd-model.md`. If not, **add it** to the DDD model doc as part of the review.
- Flag names that drift from the ubiquitous language (e.g., using "handler" when the domain says "provider").

**Bounded Context Alignment:**
- Every new module must belong to a clear bounded context (Language Service, Compilation, Runtime, REPL).
- Flag modules that mix concerns across contexts (e.g., codegen logic in a runtime module).
- Check module-level doc comments include `//! **DDD Context:** <context name>`.

**Architectural Layering:**
- Dependencies must flow downward only: `beamtalk-core` must never import `beamtalk-cli` or `beamtalk-lsp`.
- Domain services should be stateless and operate on domain objects (AST, Module, ClassHierarchy).
- Value objects (Span, Position, Token) must be immutable.

**DDD Model Doc Updates:**
- If the PR introduces new domain concepts, types, or services not in the DDD model, update `docs/beamtalk-ddd-model.md` to include them.
- If the PR renames or reorganizes domain concepts, update the DDD model to match.
- This is **not optional** — the DDD model must stay in sync with the code.

---

## Language-Specific Guidelines

### Erlang
- Use proper OTP behaviors (`gen_server`, `gen_statem`, `supervisor`).
- Handle all message patterns; avoid catch-all clauses that swallow errors.
- Use guards and pattern matching over conditional logic.
- Avoid process dictionary; pass state explicitly.
- Check for missing `-spec` and `-type` declarations.
- Ensure proper supervision tree structure for fault tolerance.
- **Beamtalk-specific:**
  - ALL errors MUST use `#beamtalk_error{}` records — no bare tuple errors (`{error, reason}`). Use `beamtalk_error:new/2`, `with_selector/2`, `with_hint/2`.
  - ALL logging MUST use OTP `logger` module — no `io:format(standard_error, ...)` for diagnostics. Use `logger:error/2`, `logger:warning/2`, `logger:debug/2` with structured metadata maps.
  - ALL source files MUST include Apache 2.0 license header.

### JavaScript/TypeScript
- Use strict mode, const/let over var.
- Prefer async/await over .then chains.
- Enforce ESLint/Prettier rules.

### Rust
- Clippy lints must pass.
- Ownership/borrowing errors: suggest `&` or `Arc` where appropriate.
- Follow the repo's AGENTS.md Rust conventions.

---

## Output Format

### During Review

**Take action on issues found:**

1. **🔴 Critical Issues** - Fix immediately:
   - Bugs that would cause crashes or incorrect behavior
   - Security vulnerabilities
   - Logic errors or edge cases not handled
   - **Action:** Implement the fix, explain what was changed

2. **🟡 Recommended Improvements** - Implement if straightforward:
   - Missing tests for new functionality
   - Unclear variable names
   - Missing documentation
   - Simple refactoring (extract function, reduce nesting)
   - Formatting/style issues
   - **Action:** Implement the improvement, note what was changed

3. **🔵 Larger Improvements** - Create Linear issues:
   - Architectural changes that affect multiple components
   - New features beyond current PR scope
   - Performance optimizations requiring benchmarking
   - Security hardening requiring design decisions
   - Tech debt that would expand PR significantly
   - **Action:** Create Linear issue with context, acceptance criteria, labels

4. **✅ Strengths** - Note what's done well:
   - Good test coverage
   - Clear, maintainable code
   - Proper error handling
   - Good documentation

### Summary Format

```markdown
## Code Review Summary

### Changes Implemented
- [x] Fixed critical bug in X (file:line)
- [x] Added missing tests for Y
- [x] Improved documentation for Z
- [x] Refactored W for clarity

### Issues Created for Follow-up
- BT-XXX: Architectural improvement for [feature]
- BT-YYY: Performance optimization for [component]

### Assessment
- **Ready to merge:** Yes/No
- **Strengths:** [Key positive aspects]
- **Remaining concerns:** [Any blocking issues]
```

Apply repo standards from CONTRIBUTING.md, AGENTS.md, or security checklists when available.
