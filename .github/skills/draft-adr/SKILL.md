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
   
   **d. Prior art — how do similar languages/platforms handle this?**
   
   Compare against these reference points (use web search when needed):
   
   | Category | Languages/Platforms | Why compare |
   |----------|-------------------|-------------|
   | **Smalltalk family** | Pharo, Squeak, Newspeak, GNU Smalltalk | Direct ancestors — what works, what we'd improve |
   | **BEAM languages** | Erlang, Elixir, Gleam, LFE | Same VM — what's possible, what's idiomatic on BEAM |
   | **Modern interactive** | Swift Playgrounds, Jupyter, Livebook | Interactive-first peers — UX expectations |
   | **Actor languages** | Pony, Akka (Scala), Dart (isolates) | Actor model — messaging, concurrency patterns |
   
   For each relevant comparison:
   - How does language X solve this?
   - What's good about their approach?
   - What doesn't translate to Beamtalk/BEAM?
   - What can we steal/adapt?

3. **User perspective analysis**: Evaluate the decision from each user persona's viewpoint:
   
   **a. Newcomer** (learning Beamtalk, coming from Python/JS/Ruby):
   - Is this intuitive? Would they guess the syntax?
   - What error messages would they see if they get it wrong?
   - Can they discover this feature through REPL exploration?
   
   **b. Smalltalk developer** (experienced, opinionated about Smalltalk purity):
   - Does this feel like Smalltalk? If not, is the departure justified?
   - Would they find this in Pharo/Squeak? If not, why are we adding it?
   - Does this preserve message-passing semantics?
   
   **c. Erlang/Elixir developer** (using Beamtalk for BEAM interop):
   - Does this work naturally with OTP patterns?
   - Can they call this from Erlang/Elixir? Can Beamtalk call their code?
   - Does this generate predictable, debuggable BEAM code?
   
   **d. Production operator** (running Beamtalk in production):
   - Does this affect hot code reloading?
   - Performance implications at scale?
   - Observable/debuggable with standard BEAM tools? (observer, recon, dbg)
   
   **e. Tooling developer** (building IDE support, LSP, debugger):
   - Can the LSP provide completions/diagnostics for this?
   - Does this make static analysis easier or harder?
   - Is the AST representation clean enough for tooling?

4. **DevEx validation**: Before finalizing options, check each against the DevEx checklist:
   - Can you demonstrate this in 1-2 lines of REPL code?
   - What does the error look like when used incorrectly?
   - Is the feature discoverable (via tab completion, help, reflection)?
   - Does it compose well with existing features?
   
   **If you can't write a compelling REPL example, the design needs more work.**

5. **Identify options**: List 2-4 concrete approaches with trade-offs:
   
   For each option:
   - **Description**: What would this look like in practice?
   - **Code example**: Show how it would appear in Beamtalk syntax or implementation
   - **REPL example**: 1-2 lines showing the feature in use interactively
   - **Error example**: What happens when used incorrectly?
   - **Pros**: Benefits, alignment with principles
   - **Cons**: Costs, complexity, limitations
   - **User impact**: How each persona (newcomer, Smalltalker, Erlang dev) would experience this
   - **Comparison**: Which reference language is this closest to? Where does it diverge?
   - **Affected components**: Which layers of the pipeline? (parser, codegen, runtime, REPL)
   - **Effort**: Rough size estimate (S/M/L/XL)

6. **Steelman each option**: For each option, argue *for* it from every user cohort's perspective. The goal is to find the strongest possible case for options you might otherwise dismiss.

   For each option, write the best argument a member of each cohort would make:
   
   | Cohort | Their strongest argument for this option |
   |--------|----------------------------------------|
   | **Newcomer** | "This is the best option because..." |
   | **Smalltalk purist** | "This preserves/improves on Smalltalk because..." |
   | **Erlang/BEAM veteran** | "This works best on BEAM because..." |
   | **Production operator** | "This is most reliable in production because..." |
   | **Language designer** | "This is the most elegant/composable because..." |
   
   **Rules for steelmanning:**
   - Argue *genuinely* — find the real strength, not a strawman
   - If you can't steelman an option from any cohort, it's probably a weak option
   - If an option you planned to reject gets a compelling steelman, reconsider
   - Note when different cohorts would choose different options — that tension is valuable signal
   
   **Present the steelman summary to the user:**
   ```markdown
   ## Steelman Analysis
   
   ### Option A: [Name]
   - 🧑‍💻 **Newcomer**: "This is most intuitive because [X] — I'd guess this syntax without reading docs"
   - 🎩 **Smalltalk purist**: "This stays true to message-passing because [Y]"
   - ⚙️ **BEAM veteran**: "This maps cleanly to OTP because [Z]"
   - 🏭 **Operator**: "This is observable and predictable because [W]"
   - 🎨 **Language designer**: "This composes well because [V]"
   
   ### Option B: [Name]
   - 🧑‍💻 **Newcomer**: "This matches what I know from Python/JS because [X]"
   - ...
   
   ### Tension Points
   - Newcomers prefer Option B but Smalltalk purists strongly prefer Option A
   - BEAM veterans and operators agree on Option A
   - Language designers are split: A is more elegant, B is more pragmatic
   ```

7. **Present options to user**: Show the options with steelman analysis and a clear recommendation:
   ```markdown
   ## Option A: [Name] (Recommended)
   [Description, code example, pros/cons]
   
   ## Option B: [Name]
   [Description, code example, pros/cons]
   
   ## Steelman Summary
   [Condensed from step 6]
   
   ## Recommendation
   Option A because [reasoning informed by steelman analysis].
   ```
   
   Wait for the user to choose or discuss before writing the ADR.

8. **Determine next ADR number**:
   ```bash
   ls docs/ADR/*.md | grep -v README | sort | tail -1
   ```
   Increment from the highest existing number.

9. **Write the ADR**: Copy `docs/ADR/TEMPLATE.md` to `docs/ADR/NNNN-kebab-case-title.md` and fill in all sections using the research, options analysis, and steelman results from previous steps.

   ```bash
   cp docs/ADR/TEMPLATE.md docs/ADR/NNNN-kebab-case-title.md
   ```
   
   Fill in every section. Remove Migration Path if not applicable. The Prior Art, User Impact, and Steelman Analysis sections should draw directly from steps 2d, 3, and 6.

10. **Update the ADR index**: Add the new ADR to `docs/ADR/README.md`.

11. **Commit the ADR**:
   ```bash
   git add docs/ADR/NNNN-*.md docs/ADR/README.md
   git commit -m "docs: add ADR NNNN - <title> BT-XXX"
   ```

12. **Summary**: Present the ADR to the user and suggest next steps:
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
