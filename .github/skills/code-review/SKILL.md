---
name: code-review
description: Review current branch changes vs main. Use when user types /code-review or asks for a code review of their changes.
---

# Code Review Workflow

Perform a **deep, thorough code review** focused on **shipping high-quality code**. Take time to understand the code's intent and context. Prioritize catching issues missed during implementation: bugs, edge cases, security vulnerabilities, performance bottlenecks, and maintainability problems.

**Key Philosophy:** Complete improvements during review rather than deferring to future PRs. If something can be done well in <2 hours, implement it now. The goal is to ship excellent code the first time, not to create a backlog of technical debt.

## Steps

1. **Identify the base branch**: Determine the comparison point:
   ```bash
   git merge-base HEAD main
   ```

2. **Get the diff**: Fetch all changes between current branch and main:
   ```bash
   git --no-pager diff $(git merge-base HEAD main)..HEAD
   ```

3. **List changed files**: Get a summary of what files were modified:
   ```bash
   git --no-pager diff --stat $(git merge-base HEAD main)..HEAD
   ```

4. **Review each changed file** against the guidelines below.

5. **Verify tests pass**: Run the test suite to ensure changes don't break anything:
   ```bash
   cargo build --all-targets && cargo clippy --all-targets -- -D warnings && cargo fmt --all -- --check && cargo test --all-targets
   ```
   For runtime changes:
   ```bash
   cd runtime && rebar3 eunit
   ```

6. **Implement recommended suggestions**: For anything that can be done well in <2 hours, implement it directly. This includes:
   - Fixing bugs and edge cases
   - Adding missing tests
   - Improving documentation
   - Refactoring for clarity
   - Adding error handling
   - Improving naming and structure
   - **Philosophy:** Ship high quality the first time. Don't defer easy work to future PRs.

7. **Create issues ONLY for substantial changes**: Only create Linear issues for work that:
   - Requires architectural design decisions
   - Affects multiple components significantly
   - Needs cross-team coordination or breaking changes
   - Would fundamentally change the PR's scope
   - **Remember:** The bar for "separate PR" is HIGH. When in doubt, implement it now.

8. **Summary**: End with an overall assessment:
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

---

## Language-Specific Guidelines

### Erlang
- Use proper OTP behaviors (`gen_server`, `gen_statem`, `supervisor`).
- Handle all message patterns; avoid catch-all clauses that swallow errors.
- Use guards and pattern matching over conditional logic.
- Avoid process dictionary; pass state explicitly.
- Check for missing `-spec` and `-type` declarations.
- Ensure proper supervision tree structure for fault tolerance.

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
