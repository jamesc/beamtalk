---
name: draft-adr
description: Research a problem and draft an Architecture Decision Record. Use when user asks to design/draft/write an ADR or make an architectural decision.
---

# Draft ADR Workflow

Research a problem, explore trade-offs, and draft an **Architecture Decision Record** following the project's ADR conventions.

**Key Philosophy:** ADRs capture *why* decisions were made, not just *what* was decided. Invest time in understanding the problem space and documenting trade-offs so future developers (and agents) understand the reasoning.

## Steps

1. **Clarify the decision scope**: Ask the user:
   - What problem or question needs a decision?
   - Is there a Linear issue driving this? (link it)
   - Are there constraints or preferences already known?

2. **Research the problem space**: Gather context from the codebase and docs:

   **a. Existing decisions:**
   ```bash
   # Check for related ADRs
   ls docs/ADR/
   grep -rl "<relevant keywords>" docs/ADR/
   ```
   
   **b. Current implementation:**
   - How does the codebase handle this today (if at all)?
   - What patterns are used in related areas?
   - Are there TODO/FIXME markers related to this?
   
   **c. Design docs and principles:**
   - `docs/beamtalk-principles.md` — Does this align with core philosophy?
   - `docs/beamtalk-language-features.md` — Does this affect language semantics?
   - `docs/beamtalk-syntax-rationale.md` — Are there prior rejected alternatives?
   - `docs/development/architecture-principles.md` — Architectural constraints?
   - `docs/beamtalk-ddd-model.md` — DDD implications?
   
   **d. Prior art:**
   - How do reference languages handle this? (Smalltalk, Erlang, Gleam, Newspeak)
   - What does the BEAM VM support or constrain?
   - Are there relevant academic papers or blog posts?

3. **Identify options**: List 2-4 concrete approaches with trade-offs:
   
   For each option:
   - **Description**: What would this look like in practice?
   - **Code example**: Show how it would appear in Beamtalk syntax or implementation
   - **Pros**: Benefits, alignment with principles
   - **Cons**: Costs, complexity, limitations
   - **Affected components**: Which layers of the pipeline? (parser, codegen, runtime, REPL)
   - **Effort**: Rough size estimate (S/M/L/XL)

4. **Present options to user**: Show the options with a clear recommendation:
   ```markdown
   ## Option A: [Name] (Recommended)
   [Description, code example, pros/cons]
   
   ## Option B: [Name]
   [Description, code example, pros/cons]
   
   ## Recommendation
   Option A because [reasoning]. 
   ```
   
   Wait for the user to choose or discuss before writing the ADR.

5. **Determine next ADR number**:
   ```bash
   ls docs/ADR/*.md | grep -v README | sort | tail -1
   ```
   Increment from the highest existing number.

6. **Write the ADR**: Create `docs/ADR/NNNN-kebab-case-title.md` following the project format:

   ```markdown
   # ADR NNNN: Descriptive Title
   
   ## Status
   Accepted (YYYY-MM-DD)
   
   ## Context
   [Problem statement — why this decision is needed]
   [Current state — how things work today]
   [Constraints — what limits the solution space]
   
   ## Decision
   [Clear, concise statement of what was decided]
   [Code examples showing the decided approach]
   
   ## Alternatives Considered
   ### [Alternative Name]
   [Description and why it was rejected]
   
   ## Consequences
   ### Positive
   - [Benefits]
   
   ### Negative
   - [Costs and trade-offs]
   
   ### Neutral
   - [Other impacts]
   
   ## Implementation
   [High-level implementation approach]
   [Affected components and rough phases]
   
   ## Migration Path
   [If this changes existing behavior: how to migrate]
   
   ## References
   - Related issues: BT-XXX
   - Related ADRs: ADR NNNN
   - Documentation: [links]
   ```

7. **Update the ADR index**: Add the new ADR to `docs/ADR/README.md`.

8. **Commit the ADR**:
   ```bash
   git add docs/ADR/NNNN-*.md docs/ADR/README.md
   git commit -m "docs: add ADR NNNN - <title> BT-XXX"
   ```

9. **Summary**: Present the ADR to the user and suggest next steps:
   - "ADR NNNN written. Ready for `/plan-adr` to break this into implementation issues?"
   - Note any open questions or decisions deferred to implementation

---

## Guidelines

### What Belongs in an ADR

✅ **Create an ADR for:**
- Language syntax/semantics decisions
- Core architecture changes (module organization, pipeline)
- BEAM interoperability decisions
- Breaking changes to user-facing behavior
- Establishing new patterns or conventions

❌ **Don't create an ADR for:**
- Bug fixes, test additions, documentation updates
- Minor refactoring within existing patterns
- Dependency updates (unless changing a core dependency)
- Implementation details that don't affect the interface

### Writing Quality

- **Context section** should be understandable by someone unfamiliar with the current discussion
- **Decision section** should be unambiguous — a developer should know exactly what to implement
- **Consequences** should be honest about trade-offs, not just list benefits
- **Code examples** are mandatory — show real Beamtalk syntax, not pseudocode
- **References** should link to Linear issues, other ADRs, and external resources

### Beamtalk Design Principles

Always evaluate options against the core principles (`docs/beamtalk-principles.md`):
1. **Interactive-first** — Does this support live development and hot code reloading?
2. **Message-passing purity** — Does this preserve Smalltalk-style message semantics?
3. **BEAM-native** — Does this work well with Erlang/OTP patterns?
4. **Pragmatic departures** — If we deviate from Smalltalk, is the trade-off justified?
