---
name: review-adr
description: Review an Architecture Decision Record for completeness, correctness, and quality. Use when user types /review-adr or asks to review an ADR.
---

# Review ADR Workflow

A **three-pass architecture document review** that validates an ADR's technical accuracy, completeness, and reasoning quality. Each pass uses a different lens.

**Key Philosophy:** ADRs are the institutional memory of the project. A weak ADR is worse than no ADR — it enshrines bad reasoning. Review rigorously, fix inline, and ensure the ADR would be useful to a developer encountering this decision in 6 months.

## Invocation

```
/review-adr                     # Review ADR files in current branch diff
/review-adr docs/ADR/0015-*.md  # Review specific ADR file
```

---

## Review Depth

Scale based on ADR scope:

| ADR Scope | Criteria | Passes | Rationale |
|-----------|----------|--------|-----------|
| **Narrow** | Single component, ≤1 alternative | Pass 1 only | Straightforward decision |
| **Medium** | 2-3 components, multiple alternatives | Pass 1 + Pass 2 | Cross-component reasoning needs verification |
| **Broad** | System-wide, new patterns, language design | All 3 passes | Full adversarial challenge needed |

**Always run all 3 passes for:**
- Language design decisions (syntax, semantics)
- Decisions affecting codegen AND runtime contracts
- New architectural patterns or conventions
- Decisions the user explicitly requests deep review on

---

## Pass 1: Structural and Factual Review

Verify the ADR is complete, internally consistent, and factually accurate.

### 1a. Template Completeness

Check every section from `docs/ADR/TEMPLATE.md` is present and substantive:

| Section | Check | Common Failures |
|---------|-------|-----------------|
| **Status** | Has status + date | Missing date |
| **Context** | Problem statement clear to an outsider | Assumes reader knows the current discussion |
| **Decision** | Unambiguous — a developer knows what to implement | Vague or hand-wavy on specifics |
| **Prior Art** | ≥3 reference languages compared | Only compares to Smalltalk, ignores BEAM ecosystem |
| **User Impact** | All 4 personas addressed (newcomer, Smalltalk dev, BEAM dev, operator) | Missing personas or superficial treatment |
| **Steelman Analysis** | Best argument FOR each rejected alternative, from each cohort | Strawman arguments, missing cohorts |
| **Alternatives** | ≥2 alternatives with code examples and rejection rationale | Alternatives are obviously weak (setup to be rejected) |
| **Consequences** | Honest negatives, not just benefits | No real negatives listed |
| **Implementation** | Phases with effort estimates and affected components | Too vague ("update the runtime") |
| **Migration Path** | Present if breaking change; absent with justification if not | Missing for a breaking change |
| **References** | Links to issues, ADRs, docs, external resources | No external references |

### 1b. Code Example Verification

**CRITICAL:** Every code example must be verified against the actual codebase. ADRs with wrong code examples are actively harmful.

For each code example in the ADR:

1. **"Current state" examples** — Verify the code actually exists and works as described:
   ```bash
   # Search for the pattern in the codebase
   grep -rn "<pattern from ADR>" <expected file>
   ```
   - Does the file exist at the stated path?
   - Does the function/pattern exist at approximately the stated line?
   - Is the behavior described accurate?

2. **"Proposed" examples** — Verify they're syntactically valid and consistent:
   - Does the proposed Beamtalk syntax match `docs/beamtalk-language-features.md`?
   - Does the proposed Erlang code use correct syntax?
   - Are Core Erlang examples valid Core Erlang?
   - Do REPL session examples use correct REPL syntax (`:load` not `@load`)?

3. **Counts and claims** — Verify quantitative claims:
   ```bash
   # "20+ call sites" — actually count them
   grep -rn "error(Error" runtime/apps/beamtalk_runtime/src/*.erl | wc -l
   ```

### 1c. Internal Consistency

- Do the consequences follow logically from the decision?
- Does the implementation plan cover everything in the decision?
- Do alternatives reference the same constraints as the context?
- Are error examples consistent with the proposed hierarchy/format?
- Does the migration path cover all breaking changes mentioned in consequences?

### 1d. Fixes

For issues found in Pass 1:
- **Fix directly** — typos, broken links, inaccurate code examples, wrong line numbers, missing sections
- **Flag for discussion** — factual claims you can't verify, design ambiguities

---

## Pass 2: Reasoning and Completeness Review

Evaluate the quality of the decision-making process. Is the reasoning sound? Are there gaps?

### 2a. Problem Framing

- **Is the problem real?** Is there evidence (code, user reports, design conflicts) that this decision is needed?
- **Is the problem correctly scoped?** Is it too narrow (missing related concerns) or too broad (bundling unrelated decisions)?
- **Are constraints accurate?** Verify each stated constraint against the actual codebase and BEAM platform.
- **Are there unstated constraints?** Check for:
  - Performance implications not mentioned
  - Erlang interop impacts not considered
  - Backward compatibility concerns not addressed
  - OTP supervision / hot code reload implications

### 2b. Prior Art Depth

- **Sufficient breadth?** Compare against at least:
  - One Smalltalk-family language (Pharo, Newspeak)
  - One BEAM language (Erlang, Elixir, Gleam)
  - One modern mainstream language (Ruby, Python, Swift, Kotlin)
- **Accurate comparisons?** Verify claims about how other languages work (use web search if needed)
- **Honest adoption/rejection?** For each comparison:
  - What was adopted and why?
  - What was rejected and why?
  - What was adapted (modified from the original)?
- **Missing comparisons?** Is there a language that handles this particularly well that wasn't mentioned?

### 2c. Alternative Quality

For each alternative considered:
- **Is it a real alternative?** Would a reasonable engineer advocate for it? Or is it a strawman?
- **Is the rejection rationale honest?** Could you argue against the rejection? If yes, the rationale is too weak.
- **Missing alternatives?** Are there obvious approaches not mentioned?
  - "Do nothing" / status quo (always worth considering explicitly)
  - Incremental approach (partial solution now, full solution later)
  - Different decomposition (solve a slightly different problem that achieves the same goal)

### 2d. Steelman Quality

For the steelman analysis:
- **Genuine advocacy?** Each argument should be the *best possible case* from that cohort's perspective, not a polite acknowledgment
- **All cohorts represented?** Minimum: newcomer, Smalltalk dev, BEAM dev, operator, language designer
- **Tension points identified?** Where do reasonable people disagree? This is the most valuable part of the steelman.
- **Did the steelman influence the decision?** If every steelman argues for the rejected alternatives and the decision ignores them, the reasoning is suspect.

### 2e. User Impact Completeness

For each user persona:
- **Specific enough?** "This is good for newcomers" is not useful. "Newcomers from Python will expect TypeError, matching their existing mental model" is.
- **Honest about downsides?** Each persona should have at least one concern or adjustment.
- **Actionable?** Does the user impact section suggest how to mitigate negatives? (docs, error messages, migration guides)

### 2f. Implementation Feasibility

- **Are phases correctly ordered?** Dependencies between phases must be explicit.
- **Is there a validation phase?** For broad ADRs introducing new infrastructure (new transport, new dependency, new protocol), check if the implementation starts with a **"napkin" / wire-check phase** — the smallest possible step that proves the core assumption works before building the full feature. If not, recommend adding one. Examples:
  - New WebSocket transport → Phase 0: single textarea + eval round-trip, before multi-pane UI
  - New compilation target → Phase 0: compile "hello world", before full language support
  - New runtime protocol → Phase 0: single message exchange, before full protocol implementation
- **Are effort estimates realistic?** Compare with similar past work in the codebase.
- **Are all affected components listed?** Cross-reference the decision with the compilation pipeline:
  ```
  Lexer → Parser → AST → Semantic Analysis → Codegen → Runtime → REPL
  ```
  If the decision affects codegen, does the implementation mention codegen files?
- **Are tests mentioned?** Every phase should mention which test suites are affected.

### 2g. Fixes

For issues found in Pass 2:
- **Fix directly** — strengthen weak alternatives, add missing prior art comparisons, flesh out user impact
- **Flag for author** — missing alternatives that need design input, incorrect constraints that need domain expertise

---

## Pass 3: Adversarial Review

Challenge the decision from outside the author's perspective. Use a **different model family** for fresh eyes.

### 3a. Launch Adversarial Review

Use the `task` tool with `agent_type: "general-purpose"` and a different model family:

```
You are a skeptical principal architect reviewing an ADR. Your job is to find
weaknesses in the reasoning that the author missed. Be adversarial but constructive.

Review this ADR: <paste full ADR content>

Focus on:

1. HIDDEN ASSUMPTIONS: What does this decision assume that might not hold?
   - "This assumes errors are always #beamtalk_error{} — but what about raw Erlang exceptions?"
   - "This assumes the class hierarchy is static — what about dynamic class loading?"

2. SECOND-ORDER EFFECTS: What consequences aren't in the Consequences section?
   - "If errors are objects at signal time, what happens to crash log tooling?"
   - "If we add 3 new stdlib classes, what's the compile time impact?"

3. ALTERNATIVES NOT CONSIDERED: What obvious approaches were missed?
   - "Why not a protocol/behavior approach instead of class hierarchy?"
   - "Why not defer the hierarchy and just fix the REPL display?"

4. FUTURE CONFLICTS: Will this decision conflict with planned features?
   - Check against active epics and ADRs in the codebase
   - "ADR 0006 (Unified Dispatch) assumes X — does this conflict?"

5. WEAKEST SECTION: Which section of the ADR is least convincing? Why?
   - "The steelman for Alternative A is weak — a BEAM veteran would actually argue..."
   - "The migration path ignores test fixtures that pattern-match on #beamtalk_error{}"

6. OVER-ENGINEERING: Is this decision more complex than needed?
   - "Could you get 80% of the value with 20% of the complexity?"
   - "Do you really need 4 phases, or could phases 1-2 be one?"

DO NOT comment on: writing style, markdown formatting, or section ordering.
ONLY flag issues that affect the quality of the decision or its implementation.
```

### 3b. Cross-Reference with Existing ADRs

Check for conflicts or redundancy with existing decisions:
```bash
ls docs/ADR/*.md | grep -v README | grep -v TEMPLATE
```

For each related ADR:
- Does this new ADR contradict any existing decision?
- Does it depend on an ADR that's still "Proposed" (not "Accepted")?
- Should it reference ADRs it doesn't currently mention?
- Does it supersede an existing ADR? (If so, mark the old one as Superseded)

### 3c. DevEx Validation

Verify the ADR meets the project's DevEx checklist:

1. **Can you demonstrate the feature in 1-2 lines of REPL code?**
   - Does the ADR include a REPL session example?
   - Is the example compelling and realistic?

2. **What does the error look like?**
   - Does the ADR show error output for misuse?
   - Are error messages actionable?

3. **Is it discoverable?**
   - Can a user find this feature via tab completion, `:help`, or reflection?
   - Does the ADR mention discoverability?

### 3d. Triage Adversarial Findings

For each finding from the adversarial review:
- **Valid and fixable** → update the ADR directly
- **Valid but needs author input** → flag with specific question
- **Theoretical / unlikely** → note but don't act
- **Wrong / already addressed** → dismiss with explanation

---

## Summary

### Output Format

```markdown
## ADR Review Summary: ADR NNNN — <Title>

### Pass 1: Structural & Factual
- [x] All template sections present and substantive
- [x] Code examples verified against codebase
  - ⚠️ Fixed: [description of correction]
- [x] Quantitative claims verified
  - ⚠️ Fixed: "20+ call sites" → actual count is 49
- [x] Internal consistency verified

### Pass 2: Reasoning & Completeness
- [x] Problem framing: [assessment]
- [x] Prior art: [sufficient/needs work]
- [x] Alternatives: [genuine/strawman concern]
- [x] Steelman quality: [assessment]
- [x] User impact: [complete/gaps]
- [x] Implementation feasibility: [realistic/concerns]

### Pass 3: Adversarial
- [x] Hidden assumptions: [findings]
- [x] Second-order effects: [findings]
- [x] Missing alternatives: [findings]
- [x] Conflict with existing ADRs: [none/found]
- [x] DevEx validation: [pass/fail]

### Fixes Applied
1. [description] — [what was changed and why]
2. [description] — [what was changed and why]

### Open Questions for Author
1. [question that needs domain expertise]
2. [design choice that could go either way]

### Assessment
- **Ready to accept:** Yes / Needs revision
- **Strengths:** [key positive aspects of the ADR]
- **Weaknesses:** [remaining concerns]
- **Recommendation:** [Accept / Revise section X / Needs discussion on Y]
```

---

## Review Guidelines

### What Makes a Good ADR

✅ **Strong ADRs have:**
- A problem statement understandable to someone with no context
- Code examples that actually compile/run (verified against codebase)
- Alternatives that a reasonable engineer would genuinely advocate for
- Steelman arguments that could change your mind
- Honest negatives in consequences (not just benefits)
- Implementation that maps clearly to codebase components
- REPL examples showing the feature in action

❌ **Weak ADRs have:**
- Problem statement that only makes sense if you were in the discussion
- Pseudocode instead of real syntax
- Strawman alternatives (obviously bad, set up to be rejected)
- Steelman arguments that are polite dismissals, not genuine advocacy
- No negatives, or negatives that are actually positives in disguise
- "Update the code" as an implementation plan
- No interactive examples

### Severity Levels

1. **🔴 Must fix before accepting** — Factually wrong, missing critical section, flawed reasoning that invalidates the decision
2. **🟡 Should fix** — Weak section, missing comparison, inaccurate code example, understated risk
3. **🔵 Nice to have** — Additional comparison, deeper steelman, more examples
4. **✅ Strength** — Well-reasoned section, excellent prior art, strong steelman

### Common ADR Anti-Patterns

| Anti-Pattern | Symptom | Fix |
|-------------|---------|-----|
| **Rubber stamp** | Only benefits listed, no real negatives | Add honest consequences section |
| **Foregone conclusion** | Alternatives are obviously worse | Find or construct a genuinely compelling alternative |
| **Scope creep** | ADR decides 3+ things at once | Split into focused ADRs, one decision each |
| **Missing the user** | All technical, no user impact | Add REPL examples and error messages |
| **Phantom prior art** | "Smalltalk does X" without verification | Verify with web search or source code |
| **Implementation masquerading as decision** | ADR is really a design doc / implementation plan | Separate the "what" (ADR) from the "how" (issues) |
| **Anchoring bias** | First option described in most detail, alternatives are thin | Give equal depth to all alternatives |
