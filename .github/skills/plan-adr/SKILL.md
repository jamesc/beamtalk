---
name: plan-adr
description: Break an accepted ADR into implementation issues with an Epic. Use when user types /plan-adr or asks to plan implementation of an ADR.
---

# Plan ADR Implementation Workflow

Take an **accepted ADR** and break it into a set of **sequenced, agent-ready Linear issues** grouped under an Epic. Each issue should be independently implementable and testable.

**Key Philosophy:** ADRs describe *what* to build and *why*. This skill turns that into *how* — concrete, ordered work items that agents can pick up via `/pick-issue`.

## Steps

1. **Identify the ADR**: Ask the user for the ADR number or accept it from the command:
   ```bash
   cat docs/ADR/NNNN-*.md
   ```
   Verify its status is `Accepted`. If `Proposed`, suggest running `draft-adr` first.

2. **Analyze implementation scope**: From the ADR, identify:
   
   **a. Affected components** — which pipeline layers need changes:
   | Layer | Files | Example changes |
   |-------|-------|----------------|
   | Lexer | `source_analysis/lexer.rs` | New tokens |
   | Parser | `source_analysis/parser/mod.rs` | New grammar rules |
   | AST | `ast.rs` | New node types |
   | Semantic | `semantic_analysis/` | Validation rules |
   | Codegen | `codegen/` | Core Erlang generation |
   | Runtime | `runtime/apps/beamtalk_runtime/src/` | Erlang modules |
   | REPL | `beamtalk_repl*.erl` | Interactive support |
   | Stdlib | `lib/*.bt` | Standard library |
   | Tests | `tests/e2e/cases/`, `test/` | E2E and unit tests |
   | Docs | `docs/` | Language docs, examples |
   
   **b. Natural phases** — group changes into implementable chunks:
   - Bottom-up: runtime support → codegen → parser → tests
   - Or: infrastructure → core feature → integration → polish
   
   **c. Dependencies** — which pieces must be done before others:
   - Parser changes before codegen (can't generate what you can't parse)
   - Runtime support before codegen (codegen needs runtime functions to call)
   - Core feature before REPL integration

3. **Check for existing issues**: Search Linear for issues already tracking this ADR:
   ```
   Search Linear for: ADR NNNN or the ADR title
   ```
   Don't duplicate existing issues. Note which parts are already tracked.

4. **Design the issue breakdown**: Create a dependency graph:
   ```
   Phase 1 (foundation):
     BT-A: Runtime support for X
     BT-B: Add AST node for Y
   
   Phase 2 (core, depends on Phase 1):
     BT-C: Parser support for Y (blocked by BT-B)
     BT-D: Codegen for Y (blocked by BT-A, BT-B)
   
   Phase 3 (integration, depends on Phase 2):
     BT-E: E2E tests (blocked by BT-C, BT-D)
     BT-F: REPL integration (blocked by BT-D)
   
   Phase 4 (polish):
     BT-G: Documentation and examples (blocked by BT-E)
   ```

   **Issue sizing rules:**
   - Each issue should be **S or M** (completable in one session)
   - If an issue is **L or XL**, split it further
   - Each issue should be independently testable
   - Each issue should leave CI green when merged

5. **Present the plan to user**: Show the breakdown before creating issues:
   ```markdown
   ## ADR NNNN Implementation Plan
   
   ### Phase 1: Foundation
   1. **[Title]** (S) — [description] — Files: [list]
   2. **[Title]** (M) — [description] — Files: [list]
   
   ### Phase 2: Core
   3. **[Title]** (M) — [description] — Blocked by: #1, #2
   
   ### Phase 3: Integration
   4. **[Title]** (S) — [description] — Blocked by: #3
   
   **Total: X issues, estimated [size]**
   ```
   
   Wait for user approval before creating issues.

6. **Create the Epic**: Create a parent Epic in Linear:
   - **Title**: `Epic: <ADR title> (ADR NNNN)`
   - **Labels**: `Epic` + relevant area labels
   - **Body**: Overview, goals, phase breakdown, link to ADR
   - **References**: `docs/ADR/NNNN-*.md`

7. **Create child issues**: For each approved issue, create in Linear:
   
   **Required fields:**
   - **Title**: Action-oriented (e.g., "Add Primitive AST node to Expression enum")
   - **Labels**: `agent-ready` + area label + type (`Language Feature`, `Feature`, `Improvement`) + size
   - **Body**:
     ```markdown
     ## Context
     Part of ADR NNNN: <title>. Phase X of Y.
     [Why this specific piece matters]
     
     ## Acceptance Criteria
     - [ ] Specific, testable requirement 1
     - [ ] Specific, testable requirement 2
     - [ ] All existing tests pass (no regressions)
     - [ ] New tests added for [what]
     
     ## Files to Modify
     - `path/to/file.rs` — [what changes]
     - `path/to/other.rs` — [what changes]
     
     ## Dependencies
     - Blocked by: BT-XXX (if any)
     
     ## References
     - ADR: `docs/ADR/NNNN-*.md`
     - Related code: [links to relevant existing code]
     
     ## Out of Scope
     - [Things explicitly NOT part of this issue]
     ```

8. **Set up blocking relationships**: Use Linear GraphQL to create all dependency links:
   ```graphql
   mutation {
     issueRelationCreate(input: {
       issueId: "<blocker issue ID>"
       relatedIssueId: "<blocked issue ID>"
       type: blocks
     }) { success }
   }
   ```
   
   Also link Epic to all child issues.

9. **Update the ADR**: Add implementation tracking to the ADR file:
   ```markdown
   ## Implementation Tracking
   
   **Epic:** BT-XXX
   **Issues:** BT-A, BT-B, BT-C, ...
   **Status:** Planned
   ```

10. **Commit and push**:
    ```bash
    git add docs/ADR/NNNN-*.md
    git commit -m "docs: add implementation tracking to ADR NNNN BT-XXX"
    git push
    ```

11. **Summary**:
    ```markdown
    ## ADR NNNN Implementation Planned
    
    **Epic:** BT-XXX — <title>
    **Issues created:** X total (Y phases)
    **Recommended start:** BT-A (Phase 1, no dependencies)
    **Estimated total:** [S/M/L/XL]
    
    Run `/pick-issue BT-A` to start Phase 1.
    ```

---

## Guidelines

### Issue Quality Checklist

Every issue must be `agent-ready` — an agent should be able to pick it up with zero clarification:

- [ ] **Context** explains why this issue exists and how it fits into the ADR
- [ ] **Acceptance criteria** are specific and testable (not vague)
- [ ] **Files to modify** lists exact paths (verified they exist)
- [ ] **Dependencies** are explicit and linked in Linear
- [ ] **Out of scope** prevents scope creep
- [ ] **Size is S or M** — if larger, split it

### Phasing Principles

1. **Bottom-up by default**: Runtime → Codegen → Parser → Tests → Docs
   - Codegen can't generate calls to runtime functions that don't exist yet
   - Parser changes are useless without codegen to consume the AST
   
2. **Each phase is independently shippable**: After Phase 1 merges, the codebase is still healthy. You don't need Phase 2 to be "done."

3. **Tests with each phase, not at the end**: Every issue includes tests for its specific changes. Don't create a "write all the tests" issue at the end.

4. **REPL integration is not optional**: If the ADR affects user-facing behavior, include a REPL integration issue. Features aren't done until users can interact with them.

### Splitting Large Issues

If an issue feels like **L or XL**, split along these seams:
- **By AST node type**: One issue per new node if adding multiple
- **By runtime module**: One issue per Erlang module affected
- **By test layer**: Unit tests in one issue, E2E in another (if both are substantial)
- **By phase**: "Add support for X in Y" then "Add support for X in Z"
- **Infrastructure vs feature**: "Add helper function" then "Use helper in feature"
