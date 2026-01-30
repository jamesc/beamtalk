---
name: create-issue
description: Create a Linear issue with proper structure and blocking relationships. Use when creating new tasks, breaking down work, or setting up dependencies between issues.
---

# Creating Linear Issues

Follow this workflow when creating issues in Linear:

## Required Fields

Every issue **must** have:

| Field | Value |
|-------|-------|
| **Team** | `BT` |
| **Assignee** | `jamesc` (James Casey) |
| **Agent State Label** | `agent-ready` or `needs-spec` |
| **Estimate (Size)** | T-shirt size: S, M, L, XM |
| **Type** | See issue types below |

## Issue Types

| Type | Description |
|------|-------------|
| `Feature` | A chunk of customer visible work |
| `Bug` | Bugs, broken tests, broken code |
| `Improvement` | Incremental work on top of a feature |
| `Documentation` | Words that explain things to humans and non-humans |
| `Infra` | Tools, CI, dev environment configuration |
| `Language Feature` | New Beamtalk language syntax/semantics |
| `Refactor` | Code cleanups, tech debt |
| `Research` | Research projects, code spikes |
| `Samples` | Code, examples, things to help devs get started |

## Optional Fields

| Field | When to Use |
|-------|-------------|
| **Project** | If part of a larger initiative (e.g., "Stdlib Implementation") |
| **Priority** | 1 (Urgent), 2 (High), 3 (Medium), 4 (Low) - default is 3 |
| **Parent Issue** | If this is a sub-task of a larger issue |

## Issue Body Structure

Every issue description should include:

1. **Context** - Why this work matters, background info
2. **Acceptance Criteria** - Specific, testable requirements (checkboxes)
3. **Files to Modify** - Explicit paths to relevant files
4. **Dependencies** - Other issues that must complete first
5. **References** - Links to specs, examples, or related code

## Example Issue

```markdown
Title: Implement basic lexer token types

Context:
The lexer is the first phase of compilation. It needs to tokenize
Smalltalk-style message syntax including identifiers, numbers, and keywords.

Acceptance Criteria:
- [ ] Tokenize identifiers (letters, digits, underscores)
- [ ] Tokenize integers and floats
- [ ] Tokenize single and double quoted strings
- [ ] Tokenize message keywords ending in `:`
- [ ] Tokenize block delimiters `[` `]`
- [ ] All tokens include source span

Files to Modify:
- crates/beamtalk-core/src/parse/token.rs
- crates/beamtalk-core/src/parse/lexer.rs

Dependencies: None

References:
- See Gleam lexer: github.com/gleam-lang/gleam/blob/main/compiler-core/src/parse/lexer.rs
```

## Agent-State Labels

Always set one of these labels:
- `agent-ready` - Fully specified, all acceptance criteria clear, agent can start immediately
- `needs-spec` - Requires human clarification before work can start
- `blocked` - Waiting on external dependency or another issue

## Size Estimates (T-Shirt Sizing)

| Size | Description |
|------|-------------|
| **S** | Small change, few hours (add a test, simple refactor) |
| **M** | Medium change, ~1 day (new feature, moderate complexity) |
| **L** | Large change, 2-3 days (significant feature, multiple files) |
| **XM** | Extra large, consider breaking down (major feature, architectural change) |

## Creating via Linear Tool

```json
{
  "action": "create",
  "title": "Implement feature X",
  "team": "BT",
  "assignee": "jamesc",
  "body": "Context:\n...\n\nAcceptance Criteria:\n- [ ] ...",
  "labels": ["agent-ready", "Language Feature"],
  "priority": 3
}
```

After creation, use GraphQL to set estimate and project:
```json
{
  "action": "graphql",
  "graphql": "mutation { issueUpdate(id: \"<issue-id>\", input: { estimate: 3 }) { success } }"
}
```

## Creating Blocking Relationships

When issues have dependencies, **always** set up Linear's "blocks" relationships:

```json
{
  "action": "graphql",
  "graphql": "mutation { issueRelationCreate(input: { issueId: \"<blocker-id>\", relatedIssueId: \"<blocked-id>\", type: blocks }) { success } }"
}
```

## Rules

- **Always assign to jamesc** - all issues go to James Casey
- If issue A must be completed before issue B can start, then A "blocks" B
- Always create blocking relationships when dependencies are mentioned
- Set estimate based on complexity, not time
- Add to relevant project if one exists for the work area
