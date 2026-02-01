---
name: update-issues
description: Find and update Linear issues that need labels, blocking relationships, or metadata. Use when user says '/update-issues' with criteria like 'no labels', 'missing agent-ready', 'needs size', etc.
---

# Update Issues Automatically

This skill searches for issues matching criteria and applies updates automatically.

## Overview

When the user says `/update-issues for ones with no labels` or similar:
1. Parse the criteria (what to look for)
2. Search Linear for matching issues
3. Analyze each issue to determine needed updates
4. Apply updates automatically
5. Report what was changed

## Supported Criteria

| User Says | What to Search For |
|-----------|-------------------|
| "no labels" | Issues with empty or missing labels |
| "missing agent-ready" | Issues without agent-state labels |
| "missing area" | Issues without item-area labels |
| "missing type" | Issues without issue-type labels |
| "missing size" | Issues without size estimate labels |
| "not assigned" | Issues without assignee |
| "in backlog" | Issues in Backlog state needing triage |
| "all open" | All open issues (for bulk updates) |
| "BT-X through BT-Y" | Specific range of issue numbers |

## Step 1: Parse User Criteria

Extract what the user wants to update:

**Examples:**
- `/update-issues for ones with no labels` → Find issues with no labels
- `/update-issues for missing agent-ready` → Find issues without agent-state labels
- `/update-issues for issues in backlog` → Find issues in Backlog state
- `/update-issues for BT-21 through BT-30` → Specific range

## Step 2: Search for Issues

Use Linear search to find matching issues:

### Search Examples

**All open issues:**
```json
{
  "action": "search",
  "query": {}
}
```

**Issues in specific state:**
```json
{
  "action": "search",
  "query": {
    "state": { "name": { "eq": "Backlog" } }
  }
}
```

**Specific issue range (get each individually):**
```json
{
  "action": "get",
  "id": "BT-21"
}
```

## Step 3: Analyze Each Issue

For each issue found, determine what's missing:

1. **Check labels** - Does it have agent-state, item-area, issue-type, and size?
2. **Check assignee** - Should be `jamesc.000@gmail.com`
3. **Check description** - Does it have acceptance criteria?
4. **Check priority** - Should default to 3 (Medium) if not set

### Determine Missing Labels

**Agent State:** Must have one of `agent-ready`, `needs-spec`, `blocked`, `human-review`, `done`

**Item Area:** Must have one of `class-system`, `stdlib`, `repl`, `cli`, `codegen`, `runtime`, `parser`

**Issue Type:** Must have one of `Feature`, `Bug`, `Improvement`, `Documentation`, `Infra`, `Language Feature`, `Refactor`, `Research`, `Samples`

**Size:** Must have one of `S`, `M`, `L`, `XL`

### Smart Defaults

When labels are missing, infer from context:

**Agent State:**
- Has acceptance criteria + files to modify → `agent-ready`
- Vague or missing details → `needs-spec`
- Mentions "depends on", "waiting for" → `blocked`

**Item Area:**
- Mentions "parser", "lexer", "AST" → `parser`
- Mentions "codegen", "Core Erlang", "BEAM" → `codegen`
- Mentions "stdlib", "collections", "String" → `stdlib`
- Mentions "REPL", "interactive" → `repl`
- Mentions "CLI", "command" → `cli`
- Mentions "runtime", "actors", "OTP" → `runtime`
- Mentions "class", "methods" → `class-system`

**Issue Type:**
- Title starts with "Implement", "Add" → `Feature`
- Title starts with "Fix", "Bug" → `Bug`
- Title starts with "Refactor", "Clean up" → `Refactor`
- Title starts with "Document" → `Documentation`
- Title contains "Research", "Investigate" → `Research`

**Size:**
- Simple, single file → `S`
- Multiple files, moderate scope → `M`
- Large feature, many files → `L`
- Major architectural change → `XL`

## Step 4: Apply Updates

For each issue, apply all needed updates in one call:

```json
{
  "action": "update",
  "id": "BT-123",
  "labels": ["agent-ready", "Feature", "parser", "M"],
  "assignee": "jamesc.000@gmail.com",
  "priority": 3
}
```

**Preserve existing labels!** Merge inferred labels with existing ones:
```
existing_labels = ["Feature"]
inferred_labels = ["agent-ready", "parser", "M"]
final_labels = ["Feature", "agent-ready", "parser", "M"]
```

### Available Fields

| Field | Description | Example Values |
|-------|-------------|----------------|
| `labels` | Array of label names | `["agent-ready", "Feature", "parser"]` |
| `assignee` | Email address | `"jamesc.000@gmail.com"` |
| `priority` | Number 0-4 | `1` (Urgent), `2` (High), `3` (Medium), `4` (Low) |
| `state` | Workflow state | `"Backlog"`, `"Todo"`, `"In Progress"`, `"In Review"`, `"Done"` |
| `title` | Issue title | `"Implement lexer tokens"` |
| `body` | Issue description | Markdown text |

## Step 5: Report Changes

After updating, report what was changed:

```
Updated 5 issues:

✓ BT-21: Added labels [agent-ready, Feature, stdlib, M]
✓ BT-32: Added labels [agent-ready, Feature, stdlib, M], set assignee
✓ BT-33: Added labels [needs-spec, Feature, stdlib, M]
✓ BT-34: Added labels [agent-ready, Feature, stdlib, S]
✓ BT-35: Added labels [agent-ready, Feature, stdlib, M]
```

## Complete Example Workflows

### Scenario 1: `/update-issues for ones with no labels`

1. **Search for all open issues:**

```json
{
  "action": "search",
  "query": {}
}
```

2. **For each issue without labels:**
   - Get full issue details
   - Analyze title and description
   - Infer appropriate labels
   - Apply update

3. **Example update:**

Issue BT-21 "Implement String class core API"
- Has acceptance criteria → `agent-ready`
- Mentions "String" and "class" → `stdlib` (or `class-system`)
- Title starts with "Implement" → `Feature`
- Multiple methods to implement → `M`

```json
{
  "action": "update",
  "id": "BT-21",
  "labels": ["agent-ready", "Feature", "stdlib", "M"],
  "assignee": "jamesc.000@gmail.com",
  "priority": 3
}
```

### Scenario 2: `/update-issues for missing agent-ready`

1. **Search for open issues:**

```json
{
  "action": "search",
  "query": {}
}
```

2. **Filter to issues without agent-state labels:**
   - Check if labels include `agent-ready`, `needs-spec`, `blocked`, etc.
   - If not, analyze and add appropriate state

3. **Update each:**

```json
{
  "action": "update",
  "id": "BT-XX",
  "labels": ["agent-ready", "...existing labels..."]
}
```

### Scenario 3: `/update-issues for BT-21 through BT-40`

1. **Get each issue in range:**

```json
{
  "action": "get",
  "id": "BT-21"
}
```

2. **Analyze and update each one**

3. **Skip issues that are already complete or properly labeled**

## Label Inference Rules (Summary)

### Agent State
- ✅ Well-defined acceptance criteria + files → `agent-ready`
- ⚠️ Vague or incomplete → `needs-spec`
- 🚫 Mentions "depends on", "waiting for" → `blocked`

### Item Area (by keyword)
- "parser", "lexer", "token", "AST" → `parser`
- "codegen", "Core Erlang", "BEAM", "generate" → `codegen`
- "stdlib", "String", "Array", "collection" → `stdlib`
- "REPL", "interactive", "eval" → `repl`
- "CLI", "command", "flag" → `cli`
- "runtime", "actor", "OTP", "process" → `runtime`
- "class", "method", "object" → `class-system`

### Issue Type (by title)
- "Implement", "Add" → `Feature`
- "Fix", "Bug" → `Bug`
- "Refactor", "Clean up" → `Refactor`
- "Document", "Add docs" → `Documentation`
- "Research", "Investigate" → `Research`

### Size (by scope)
- Single file, simple change → `S`
- Few files, moderate feature → `M`
- Many files, complex feature → `L`
- Architectural, breaking change → `XL`

## Setting Up Blocking Relationships

If the user also mentions dependencies (e.g., "and set up blocking relationships"), use GraphQL:

1. **Get UUIDs for blocker and blocked issues**
2. **Create relationship:**

```json
{
  "action": "graphql",
  "graphql": "mutation($blockerId: String!, $blockedId: String!) { issueRelationCreate(input: { issueId: $blockerId, relatedIssueId: $blockedId, type: blocks }) { success } }",
  "variables": {
    "blockerId": "<UUID-of-blocker>",
    "blockedId": "<UUID-of-blocked>"
  }
}
```

### Example: BT-21 blocks multiple issues

```
BT-21 (API definitions) blocks:
- BT-32 (block evaluation)
- BT-33 (collections)
- BT-34 (strings)
```

For each blocked issue:
1. Get BT-21's UUID: `linear-linear get --id "BT-21"` → save `id` field
2. Get blocked issue's UUID: `linear-linear get --id "BT-32"` → save `id` field
3. Create relation with BT-21 as `blockerId`, BT-32 as `blockedId`

## Relationship Types

Linear supports these relationship types:

| Type | Description |
|------|-------------|
| `blocks` | This issue blocks another (dependency) |
| `blocked_by` | This issue is blocked by another (inverse) |
| `related` | Generic relationship |
| `duplicate` | Mark as duplicate |

**Note:** Use `blocks` type. Linear automatically creates the inverse `blocked_by` relationship.

## Tips

1. **Always get issue first** to retrieve UUID and existing data
2. **Preserve existing labels** when updating - merge with new labels, don't replace
3. **Use GraphQL for relationships** - update action doesn't support relations
4. **Set blocking relationships** whenever dependencies are mentioned
5. **Batch updates efficiently** - group related API calls in the same turn
6. **Skip done issues** - Don't update issues in Done or Canceled states
7. **Report clearly** - Show what changed for each issue

## Workflow States

Team BT uses these workflow states:

| State | Description |
|-------|-------------|
| `Backlog` | Idea captured, not yet specified |
| `Todo` | Ready to start, fully specified |
| `In Progress` | Actively being worked on |
| `In Review` | Code complete, needs verification |
| `Done` | Merged and verified |
| `Canceled` | Won't do |
| `Duplicate` | Duplicate of another issue |
