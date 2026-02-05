---
name: final-review
description: Comprehensive final review before merge. Use when user types /final-review to do a thorough code review and documentation review using a high-reasoning model.
model: gpt-5.2-codex (xhigh)
---

# Final Review Workflow

You are a senior software engineer performing code review, following: https://google.github.io/eng-practices/review/reviewer/looking-for.html

Perform a **comprehensive final review** before merging, using high-reasoning capabilities for deep analysis. This combines a thorough code review with a documentation review and REPL verification to ensure both code quality and docs are updated appropriately.

Prioritize: Beam scheduler, actor isolation, borrow checker, async perf.

**Model:** This skill uses `gpt-5.2-codex (xhigh)` for maximum reasoning depth on complex code analysis.

**Key Philosophy:** Ship excellent, well-documented code the first time. Complete all improvements during review rather than deferring. Ensure documentation accurately reflects code changes. **Verify in REPL** - Beamtalk is interactive-first.

---

## Phase 1: Code Review

### Steps

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

4. **Review each changed file** against the code review guidelines below.

5. **Verify tests pass**: Run the test suite to ensure changes don't break anything:
   ```bash
   just ci
   ```
   This runs all CI checks (build, clippy, fmt-check, test, test-e2e).

6. **Implement recommended suggestions**: For anything that can be done well in <2 hours, implement it directly:
   - Fixing bugs and edge cases
   - Adding missing tests
   - Improving code documentation
   - Refactoring for clarity
   - Adding error handling
   - Improving naming and structure

---

## Phase 2: Documentation Review

After completing the code review, perform a documentation audit.

### Steps

1. **Identify documentation files that may need updates**:
   ```bash
   # Check if any docs were already modified
   git --no-pager diff --stat $(git merge-base HEAD main)..HEAD -- "*.md" "docs/"
   
   # List all doc files for reference
   find docs/ -name "*.md" -type f | head -30
   ```

2. **Cross-reference code changes with documentation**:
   
   For each significant code change, ask:
   - Does this change any public API or behavior documented in `docs/`?
   - Does this add new features that need documentation?
   - Does this change existing behavior that's documented?
   - Are there affected areas in:
     - `docs/beamtalk-language-features.md` (language syntax/semantics)
     - `docs/beamtalk-syntax-rationale.md` (syntax decisions)
     - `docs/beamtalk-architecture.md` (system architecture)
     - `docs/development/*.md` (developer guidelines)
     - `AGENTS.md` (agent development guidelines)
     - `README.md` (project overview)
     - `examples/*.md` (tutorials and examples)

3. **Check for documentation gaps**:
   
   - New language features → Update `docs/beamtalk-language-features.md`
   - New CLI commands → Update CLI help and `docs/development/common-tasks.md`
   - New architecture decisions → Consider ADR in `docs/ADR/`
   - New skills → Update skill list in `AGENTS.md` and `.github/copilot-instructions.md`
   - New examples → Ensure `examples/README.md` is updated
   - Breaking changes → Document migration path

4. **Verify documentation accuracy**:
   
   - Code examples in docs still work after changes
   - Command-line examples still produce expected output
   - File paths referenced in docs still exist
   - Version numbers and feature flags are current

5. **Update documentation as needed**:
   
   Implement documentation updates directly. Don't defer docs to follow-up PRs unless the documentation requires extensive new content beyond the scope of the code changes.

---

## Code Review Guidelines

### Action Philosophy

**Quality First: Complete work now, don't defer easy fixes.**

### Action Decision Matrix

**Implement immediately:**
- Bugs, logic errors, security vulnerabilities
- Missing error handling or edge cases
- Formatting/style violations
- Unclear names or missing documentation
- Simple to moderate refactoring
- Missing unit tests for new code
- Performance improvements with clear solutions
- **Rule:** If it can be done well in <2 hours, do it now.

**Create Linear issue ONLY for:**
- Architectural refactoring affecting many files/components
- New features genuinely beyond PR scope
- Performance optimizations requiring extensive benchmarking
- Breaking API changes requiring coordination
- Work that would 2-3x the PR size

### General
- Flag unused variables, imports, or dead code
- Check for null/undefined handling, bounds checking, and error propagation
- Ensure consistent naming conventions
- Limit functions to <50 lines; suggest refactoring if violated
- Avoid deeply nested conditionals (>3 levels); prefer early returns

### Security Checks
- Validate/sanitize all inputs
- Prevent injection (SQL, XSS, command)
- Use secure defaults
- Check auth/authorization
- Flag unsafe deserialization

### Performance & Efficiency
- Optimize loops where appropriate
- Avoid N+1 queries
- Flag expensive operations in hot paths
- Use efficient data structures

### Testing & Documentation
- Ensure new code has accompanying tests
- Flag missing types, docstrings, or JSDoc
- Suggest assertions for edge cases

---

## Language-Specific Guidelines

### Erlang
- Use proper OTP behaviors (`gen_server`, `gen_statem`, `supervisor`)
- Handle all message patterns; avoid catch-all clauses
- Use guards and pattern matching over conditional logic
- Check for missing `-spec` and `-type` declarations
- Ensure proper supervision tree structure

### Rust
- Clippy lints must pass
- Ownership/borrowing correctness
- Follow AGENTS.md Rust conventions

### Beamtalk
- Verify syntax against `examples/` and `tests/e2e/cases/`
- Don't hallucinate syntax (see AGENTS.md "Syntax Verification")
- Use implicit returns (no `^` on last expression)
- Use `//` comments, not Smalltalk `"..."`

---

## Documentation Review Checklist

### For Language Changes
- [ ] `docs/beamtalk-language-features.md` updated with new syntax
- [ ] `docs/beamtalk-syntax-rationale.md` explains design decisions
- [ ] Examples in `examples/*.bt` demonstrate the feature
- [ ] E2E tests in `tests/e2e/cases/` validate the syntax

### For Architecture Changes
- [ ] `docs/beamtalk-architecture.md` reflects new design
- [ ] ADR created if decision is significant (`docs/ADR/`)
- [ ] `AGENTS.md` updated if it affects agent workflows

### For New Features
- [ ] Feature is documented where users will look for it
- [ ] Usage examples provided
- [ ] Error messages documented if applicable
- [ ] Migration guide if replacing existing functionality

### For Skills/Tooling
- [ ] Skill list in `AGENTS.md` updated
- [ ] Skill list in `.github/copilot-instructions.md` updated
- [ ] Skill description is clear about when to use it

---

## Output Format

### During Review

**Code Issues:**

1. **🔴 Critical Issues** - Fix immediately:
   - Bugs, security vulnerabilities, logic errors
   - **Action:** Implement the fix, explain what was changed

2. **🟡 Recommended Improvements** - Implement if straightforward:
   - Missing tests, unclear names, missing docs, simple refactoring
   - **Action:** Implement the improvement, note what was changed

3. **🔵 Larger Changes** - Create Linear issues:
   - Architectural changes, new features beyond scope
   - **Action:** Create Linear issue with context and labels

**Documentation Issues:**

4. **📝 Doc Updates Needed** - Implement during review:
   - Out-of-date documentation
   - Missing documentation for new features
   - Incorrect examples or commands
   - **Action:** Update the documentation

5. **📚 Doc Gaps Identified** - Note or create issues:
   - Extensive new documentation needed
   - Tutorial or guide creation
   - **Action:** Create issue if substantial, otherwise implement

### Summary Format

```markdown
## Final Review Summary

### Code Changes Implemented
- [x] Fixed critical bug in X (file:line)
- [x] Added missing tests for Y
- [x] Refactored W for clarity

### Documentation Updates
- [x] Updated docs/beamtalk-language-features.md with new syntax
- [x] Added example in examples/feature.bt
- [x] Updated AGENTS.md skill list

### REPL Verification
- [x] Tested feature X in REPL - works as expected
- [x] Error messages are user-friendly
- [ ] N/A - infrastructure-only changes

### Issues Created for Follow-up
- BT-XXX: Architectural improvement for [feature]

### Assessment
- **Ready to merge:** Yes/No
- **Code quality:** [Assessment]
- **Documentation:** Complete / Needs follow-up
- **REPL tested:** Yes / No / N/A
- **Strengths:** [Key positive aspects]
- **Remaining concerns:** [Any blocking issues]
```

---

## Phase 3: REPL Verification

**Beamtalk is an interactive-first language.** After code review and docs, verify changes work in the REPL where applicable.

### When to REPL Test

Test in REPL when changes affect:
- Language syntax or semantics
- Runtime behavior (actors, messages, state)
- Standard library features
- Error messages and formatting
- Any user-facing behavior

Skip REPL testing for:
- Pure infrastructure changes (CI, scripts, build)
- Rust-only refactoring with no runtime effect
- Documentation-only changes

### Steps

1. **Start the REPL**:
   ```bash
   beamtalk repl
   ```

2. **Load relevant fixtures/examples**:
   ```
   > :load examples/counter.bt
   > :load tests/e2e/fixtures/counter.bt
   ```

3. **Test the specific changes**:
   - For new syntax: Try the new syntax in REPL
   - For runtime changes: Spawn actors, send messages, verify behavior
   - For error handling: Trigger the error condition, verify message is helpful
   - For stdlib: Call the new/modified methods

4. **Verify error messages are user-friendly**:
   - Trigger expected errors
   - Check that messages explain what went wrong
   - Check that hints suggest how to fix it

5. **Document REPL session** in review summary:
   ```markdown
   ### REPL Verification
   ```
   > Counter spawn
   => <0.123.0>
   > c := Counter spawn
   > c increment
   => 1
   ```
   ✅ Actor spawn and messaging work correctly
   ```

### REPL Testing Examples

**Testing a new method:**
```
> :load examples/counter.bt
> c := Counter spawn
> c increment
=> 1
> c getValue
=> 1
```

**Testing error handling:**
```
> 42 foo
=> Error: Integer does not understand 'foo'
   Hint: Check method name spelling
```

**Testing control flow:**
```
> count := 0
> 3 timesRepeat: [count := count + 1]
> count
=> 3
```

---

## Final Checklist

Before approving for merge:

- [ ] All CI checks pass (`just ci`)
- [ ] No critical code issues remain
- [ ] Tests added for new functionality
- [ ] Documentation updated to reflect changes
- [ ] No outdated docs that contradict the changes
- [ ] REPL verification done (if applicable)
- [ ] Commit messages are clear and reference issues
- [ ] PR description summarizes the changes

