---
name: code-review
description: Review current branch changes vs main. Use when user types /code-review or asks for a code review of their changes.
---

# Code Review Workflow

Perform a **deep, thorough code review** focused on quality. Take time to understand the code's intent and context. Prioritize catching issues missed during implementation: bugs, edge cases, security vulnerabilities, performance bottlenecks, and maintainability problems.

This is not a surface-level review—dig into the logic, trace data flows, and consider how the code behaves under failure conditions.

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

6. **Provide feedback** using the output format below.

7. **Summary**: End with an overall assessment:
   - Is the code ready to merge?
   - What are the main strengths of the changes?
   - What are the key areas to address?

---

## Review Guidelines

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

Structure comments by severity:

**🔴 Critical:** [Description] [File:Line]  
**Fix:** ```code```  
**Why:** [Rationale]

**🟡 Suggestion:** [Improvement] [File:Line]  
**Alternative:** ```code```

**✅ Good:** [Strength observed in the code]

Apply repo standards from CONTRIBUTING.md, AGENTS.md, or security checklists when available.
