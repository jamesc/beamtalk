---
name: review-code
description: Review current branch changes vs main. Use when user types /review-code or asks for a code review of their changes.
---

# Code Review Workflow

A **three-pass code review** that progressively deepens analysis. Each pass uses a different lens to catch issues the previous pass missed.

**Key Philosophy:** Complete improvements during review rather than deferring to future PRs. If something can be done well in <2 hours, implement it now. The goal is to ship excellent code the first time.

## Invocation

```
/review-code              # Review current branch vs main
/review-code BT-123       # Find PR for Linear issue, checkout, review
/review-code #225         # Checkout PR #225, review
```

**When a Linear issue or PR number is provided:**
1. Find the associated PR: `gh pr list --repo jamesc/beamtalk --search "BT-123" --json number,headRefName`
2. Checkout the branch: `gh pr checkout <number>`
3. Proceed with the review

---

## Review Depth

Not every PR needs 3 passes. Auto-scale based on change size and type:

```bash
# Count changed lines (additions + deletions)
git --no-pager diff --shortstat $(git merge-base HEAD origin/main)..HEAD
```

| Change Size | Criteria | Passes | Rationale |
|-------------|----------|--------|-----------|
| **Small** | <50 lines changed, or docs/skills/config only | Pass 1 only | Surface review is sufficient |
| **Medium** | 50-300 lines, touches 1-2 components | Pass 1 + Pass 2 | Cross-component checks needed |
| **Large** | >300 lines, touches 3+ components, or new features | All 3 passes | Full review with adversarial |

**Always run all 3 passes for:**
- Changes touching codegen AND runtime (contract risk)
- New language features (parser + codegen + runtime)
- Changes the user explicitly asks for deep review on
- **Any change touching these security-sensitive components:**

| Component | Files | Risk |
|-----------|-------|------|
| **REPL server** | `beamtalk_repl_server.erl`, `beamtalk_repl.erl` | TCP listener, accepts connections, spawns per-client processes |
| **REPL eval** | `beamtalk_repl_eval.erl` | Compiles & executes user code, file I/O |
| **Workspace persistence** | `beamtalk_workspace_meta.erl` | Reads/writes files from user paths |
| **Atom creation** | `beamtalk_object_class.erl`, `beamtalk_repl_eval.erl` | `list_to_atom` from user input (DoS risk) |
| **File path handling** | `beamtalk_repl_eval.erl`, CLI file loading | Path traversal, unsanitized user paths |
| **Process spawning** | `beamtalk_actor.erl`, `beamtalk_repl.erl` | Resource exhaustion, unbounded process creation |

**The user can override:** `/review-code --deep` forces all 3 passes regardless of size.

---

## Pass 1: Diff Review (correctness, style, DDD)

The fast pass — read the diff line by line, catch surface issues.

### Steps

1. **Identify the base branch**:
   ```bash
   git fetch origin main
   git merge-base HEAD origin/main
   ```

2. **Get the diff and changed files**:
   ```bash
   git --no-pager diff --stat $(git merge-base HEAD origin/main)..HEAD
   git --no-pager diff $(git merge-base HEAD origin/main)..HEAD
   ```

3. **Review each changed file** against the Review Guidelines below.

4. **DDD compliance check**: For each new or renamed module/type/function:
   - Does the name match the ubiquitous language in `docs/beamtalk-ddd-model.md`?
   - Is the bounded context documented in the module header?
   - If a new domain concept is introduced, is it added to the DDD model doc?
   - Are dependency directions correct (core never imports cli/lsp)?

5. **Test coverage check**:
   - Are there new tests needed for these changes?
   - Can I write an E2E test? (check `tests/e2e/cases/*.bt`)
   - Are there edge cases not tested?
   - Did existing tests need updating?

6. **Implement fixes**: For anything that can be done in <2 hours:
   - Fix bugs, edge cases, security issues
   - Add missing tests
   - Improve naming, docs, DDD alignment
   - Update `docs/beamtalk-ddd-model.md` for new domain concepts

7. **Verify CI passes** after any changes:
   ```bash
   just ci
   ```

8. **Pass 1 summary**: Report findings using the severity levels (🔴🟡🔵✅).

---

## Pass 2: System Review (cross-repo impact, algorithms, edge cases)

Zoom out from the diff. Think about how these changes interact with the rest of the system.

### Steps

9. **Read surrounding code**: For each changed file, read the **unchanged** code around it. Understand the full module, not just the diff:
   ```bash
   # For each significantly changed file, read the whole thing
   cat <file>
   ```

10. **Cross-component analysis**: Trace the data flow across component boundaries:
    - **Parser → Codegen**: Does the parser produce AST that codegen correctly consumes? Are all new AST variants handled in codegen match arms?
    - **Codegen → Runtime**: Does generated Core Erlang call runtime functions that exist with the right arity? Do state shapes match expectations?
    - **Runtime → REPL**: Do runtime changes appear correctly in REPL output? Are error messages formatted for users?
    - **Semantic → LSP**: Do new semantic analysis features feed into LSP completions/diagnostics?

11. **Edge case analysis**: For each algorithm or logic change, systematically check:
    - **Empty/nil**: What happens with empty collections, nil values, missing keys?
    - **Boundaries**: Integer overflow, atom exhaustion, process limits, string encoding?
    - **Concurrency**: Race conditions between actors? Message ordering assumptions?
    - **Error cascades**: If this fails, does the caller handle it? Does the error reach the user with a helpful message?
    - **State machines**: Are all transitions covered? Can you get stuck in a state?
    - **Reentrancy**: Can this function be called recursively? From a callback? During init?

12. **Contract verification**: Check that implicit contracts between layers are maintained:
    - Generated Core Erlang function signatures match what runtime expects
    - Error records use `#beamtalk_error{}` consistently across codegen and runtime
    - State variable naming conventions are consistent (`State0`, `State1`, etc.)
    - Module naming in codegen matches what runtime resolves

13. **Documentation cross-reference**: Check if changes require updates to:
    - `docs/beamtalk-language-features.md` — language syntax changes
    - `docs/beamtalk-architecture.md` — system design changes
    - `docs/beamtalk-ddd-model.md` — domain model changes
    - `examples/*.bt` — new features need examples
    - `AGENTS.md` — workflow or convention changes

14. **Implement fixes** from Pass 2 findings, re-run CI if changes made.

---

## Pass 3: Adversarial Review (different model, challenge assumptions)

Use a **different model family** to challenge the design with fresh eyes. Different model families have different blind spots — that's the point.

### Steps

15. **Launch adversarial review** using the task tool with a model from a **different family** than your own. If you're Claude, use GPT; if you're GPT, use Claude:

    Launch via `task` with `agent_type: "general-purpose"` and `model: "gpt-5.2-codex"` (or `model: "claude-opus-4.6"` if you're a GPT model):
    
    ```
    You are a skeptical senior engineer reviewing a PR. Your job is to find
    issues the original reviewer missed. Be adversarial but constructive.
    
    Review this diff: <paste diff or key files>
    
    Focus on:
    1. ASSUMPTIONS: What assumptions is this code making that might not hold?
       - "This assumes X will always be Y — but what about Z?"
       - "This assumes single-threaded access — is that guaranteed?"
    
    2. FUTURE FRAGILITY: What would break this in 6 months?
       - "If someone adds a new AST node, they'll miss updating this match"
       - "This hard-codes a list that will grow — should it be derived?"
    
    3. SCALING: If this codebase 10x'd, would this approach still work?
       - "This is O(n²) which is fine now but won't scale"
       - "This holds everything in memory — what if the input is huge?"
    
    4. MISUSE: How could a user or developer misuse this?
       - "Nothing prevents calling this with a nil argument"
       - "The error message doesn't tell you which argument was wrong"
    
    5. TESTING GAPS: What scenarios aren't tested?
       - "No test for concurrent access to this shared state"
       - "No test for the error path when X fails"
    
    DO NOT comment on: style, formatting, naming conventions, or anything cosmetic.
    ONLY flag issues that could cause bugs, data loss, or significant maintenance burden.
    ```

16. **REPL verification** (for user-facing changes only — skip for infra-only):
    - Start REPL: `beamtalk repl`
    - Load relevant fixtures: `:load examples/counter.bt`
    - Test the specific changes interactively
    - Verify error messages are helpful and actionable
    - Document the REPL session in the summary

17. **Triage adversarial findings**: The adversarial review will surface many concerns. For each:
    - **Valid and fixable now** → implement the fix
    - **Security issue, can't fix now** → create Linear issue with `Bug` label + urgent priority. **Never drop security findings.**
    - **Valid but out of scope** → create Linear issue
    - **Theoretical, unlikely in practice** → note but don't act
    - **Wrong / already handled** → dismiss with explanation

18. **Final CI run** if any changes were made in Pass 3:
    ```bash
    just ci
    ```

---

## Summary

19. **Create follow-up issues**: Anything found during review that isn't fixed in this PR **must** get a Linear issue — findings should never be just "noted" and forgotten.

    **Always create an issue for:**
    - 🔴 **Security findings not fixed now** — label `Bug` + `urgent priority`. Never let security issues be silently dropped.
    - 🟡 **Valid concerns deferred** — out-of-scope improvements, performance concerns with evidence, architectural suggestions
    - 🔵 **Adversarial findings worth tracking** — assumptions that could break, scaling concerns, missing test scenarios
    
    **Don't create issues for:**
    - Theoretical concerns with no plausible trigger
    - Things already handled that the adversarial reviewer missed
    
    **The bar for "fix it now" is still high** — prefer implementing over deferring. But if you defer, track it.

20. **Final summary**:

```markdown
## Code Review Summary

### Pass 1: Diff Review
- [x] Fixed: [description] (file:line)
- [x] Added: [tests/docs] for [feature]
- ✅ DDD compliance: [status]

### Pass 2: System Review
- [x] Fixed: [cross-component issue]
- [x] Verified: [contract between X and Y]
- ✅ Documentation: [updated/already current]

### Pass 3: Adversarial Review
- [x] Fixed: [assumption that was wrong]
- ⚠️ Noted: [theoretical concern, not acting]
- ✅ REPL verified: [what was tested]

### Issues Created
- 🔴 BT-XXX: [security finding — urgent]
- BT-YYY: [deferred improvement]
- BT-XXX: [only for substantial architectural work]

### Assessment
- **Ready to merge:** Yes/No
- **Strengths:** [key positive aspects]
- **Remaining concerns:** [any blocking issues]
```

---

## Review Guidelines

### Action Decision Matrix

**Implement immediately (fix in place):**
- Bugs, logic errors, security vulnerabilities
- Missing error handling or edge cases
- Formatting/style violations, unclear names
- Missing unit tests, missing documentation
- Simple to moderate refactoring
- Performance improvements with clear solutions
- **Rule of thumb:** If it can be done well in <2 hours, do it now.

**Create Linear issue ONLY for:**
- Architectural refactoring affecting many files/components
- New features genuinely beyond PR scope
- Performance optimizations requiring extensive benchmarking
- Breaking API changes requiring coordination
- Work that would 2-3x the PR size
- **Rule of thumb:** Only defer if it requires design decisions or cross-team coordination.

**Security findings are NEVER optional.** If you find a security issue and can't fix it now, create a Linear issue with `Bug` label + urgent priority immediately. Security issues must not be "noted" without tracking.

**When in doubt, implement it.**

### General
- Flag unused variables, imports, or dead code
- Check for null/undefined handling, bounds checking, error propagation
- Ensure consistent naming (snake_case for Rust/Erlang), avoid abbreviations
- Limit functions to <50 lines; suggest refactoring if violated
- Avoid deeply nested conditionals (>3 levels); prefer early returns

### Security
- Validate/sanitize all inputs (user data, APIs, files)
- Prevent injection; flag string concatenation in queries
- No hard-coded secrets
- Flag unsafe deserialization, regex DoS risks

### Performance
- Avoid N+1 queries; suggest batching
- Flag expensive ops in hot paths
- Use efficient data structures (Sets for lookups)

### DDD Compliance

**Reference:** `docs/beamtalk-ddd-model.md`

- Module/type/function names MUST use domain terms, not generic technical terms
  - ✅ `CompletionProvider`, `DiagnosticProvider`, `ClassHierarchy`
  - ❌ `completions`, `diagnostics`, `class_utils`
- New domain terms must be added to `docs/beamtalk-ddd-model.md`
- Every module must belong to a clear bounded context
- Module-level doc comments must include `//! **DDD Context:** <context>`
- Dependencies flow downward only: core never imports cli/lsp
- DDD model doc must stay in sync with code — **not optional**

---

## Language-Specific Guidelines

### Erlang
- Use proper OTP behaviors (`gen_server`, `gen_statem`, `supervisor`)
- Handle all message patterns; avoid catch-all clauses that swallow errors
- Use guards and pattern matching over conditional logic
- Check for missing `-spec` and `-type` declarations
- **Beamtalk-specific:**
  - ALL errors MUST use `#beamtalk_error{}` records — no bare tuple errors
  - ALL logging MUST use OTP `logger` module — no `io:format`
  - ALL source files MUST include Apache 2.0 license header

### Rust
- Clippy must pass with `-D warnings`
- Correct ownership/borrowing
- Follow AGENTS.md conventions

### Beamtalk
- Verify syntax against `examples/` and `tests/e2e/cases/`
- Don't hallucinate syntax (see AGENTS.md verification checklist)
- Use `//` comments, implicit returns, no periods

---

## Severity Levels

1. **🔴 Critical** — Fix immediately: bugs, crashes, security vulnerabilities, logic errors
2. **🟡 Recommended** — Implement if straightforward: missing tests, unclear names, docs
3. **🔵 Larger** — Create Linear issue: architectural changes, major refactoring
4. **✅ Strengths** — Note what's done well: good coverage, clear code, proper error handling
