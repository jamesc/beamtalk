---
name: refresh-issue
description: Refresh a Linear issue to align with current docs and code state. Use when user types /refresh-issue BT-XX or asks to refresh/sync an issue with the codebase.
---

# Refresh Issue Workflow

When activated with a Linear issue ID (e.g., `/refresh-issue BT-34`), execute this workflow to ensure the issue accurately reflects the current state of the codebase.

## Purpose

Issues can become stale as the codebase evolves. This skill:
- Verifies the issue is still relevant
- Checks if it's already been fixed
- Aligns the description with current documentation
- Updates acceptance criteria based on current code state

## Steps

### 1. Determine Issue ID

Use the same resolution logic as `pick-issue` step 1:
- Explicit argument: `/refresh-issue BT-42` or `/refresh-issue 42`
- Fall back to worktree name (e.g., `/workspaces/BT-34` → `BT-34`)
- Fall back to branch name (e.g., `BT-10-implement-lexer` → `BT-10`)
- If none match, ask the user

### 2. Fetch the Issue

Get the full issue details from Linear:
```json
{
  "action": "get",
  "id": "BT-XX"
}
```

Note the following from the issue:
- Title and description
- Acceptance criteria (checklist items)
- Files to modify (mentioned paths)
- Referenced documentation
- Current state and labels

### 3. Review Current Documentation

Check relevant documentation files to ensure the issue aligns with current specs:

| Area | Documentation to Check |
|------|------------------------|
| Language features | `docs/beamtalk-language-features.md`, `docs/beamtalk-syntax-rationale.md` |
| Architecture | `docs/beamtalk-architecture.md` |
| BEAM interop | `docs/beamtalk-beam-interop.md` |
| Design principles | `docs/beamtalk-principles.md` |
| Agent guidelines | `AGENTS.md` |

Compare the issue's requirements against the documentation:
- Are the requirements still accurate?
- Has the spec changed since the issue was created?
- Are there new requirements that should be added?

### 4. Examine Relevant Code

For each file mentioned in "Files to Modify" or related to the issue:

1. **Check if file exists**: Use `view` or `glob` to locate the file
2. **Review current implementation**: Look for any existing code that addresses the issue
3. **Check test coverage**: Look for related tests that might indicate the feature is done
4. **Check DDD alignment**: Verify domain concepts in the issue match `docs/beamtalk-ddd-model.md`. If the issue uses outdated domain terms, flag them for update.

Key directories by area:
- **Parser**: `crates/beamtalk-core/src/source_analysis/`
- **AST**: `crates/beamtalk-core/src/ast.rs`
- **Codegen**: `crates/beamtalk-core/src/codegen/`
- **Runtime**: `runtime/apps/beamtalk_runtime/src/`
- **Stdlib**: `lib/`
- **CLI**: `crates/beamtalk-cli/`
- **Semantic Analysis**: `crates/beamtalk-core/src/semantic_analysis/`

### 5. Verify Issue Status

Determine if the issue is:

| Status | Criteria | Action |
|--------|----------|--------|
| **Already Fixed** | Code exists, tests pass, feature works | Mark as Done with comment explaining |
| **Partially Fixed** | Some acceptance criteria met | Update description noting what's done |
| **Still Open** | No implementation exists | Update description if needed |
| **Obsolete** | Requirements changed, no longer needed | Mark as Canceled with explanation |
| **Blocked** | Depends on unfinished work | Update state to Blocked, add blocking issues |

### 6. Update the Issue

Based on findings, update the issue appropriately:

#### If Already Fixed

```json
{
  "action": "update",
  "id": "BT-XX",
  "state": "Done"
}
```

Then add a comment:
```json
{
  "action": "comment",
  "id": "BT-XX",
  "body": "## Issue Review\n\n**Status**: Already implemented\n\n**Evidence**:\n- [describe where the implementation exists]\n- [reference specific files/lines]\n- [mention relevant tests]\n\nMarking as Done."
}
```

#### If Still Open (with updates needed)

Add a comment summarizing findings and update the description if needed:

```json
{
  "action": "comment",
  "id": "BT-XX",
  "body": "## Issue Review\n\n**Status**: Still open, description updated\n\n**Documentation Review**:\n- [note any spec changes or clarifications]\n\n**Code Review**:\n- [note current state of relevant code]\n- [identify any partial implementations]\n\n**Updated Acceptance Criteria**:\n- [list any changes to criteria]\n\n**Files to Modify** (updated):\n- [current list of relevant files]"
}
```

If the description needs significant updates:
```json
{
  "action": "update",
  "id": "BT-XX",
  "body": "[Updated description with current context, acceptance criteria, and file list]"
}
```

#### If Blocked

```json
{
  "action": "update",
  "id": "BT-XX",
  "state": "Backlog",
  "labels": ["blocked", "existing-area-label", "existing-type-label"]
}
```

Add comment explaining what's blocking:
```json
{
  "action": "comment",
  "id": "BT-XX",
  "body": "## Issue Review\n\n**Status**: Blocked\n\n**Blocking Issues**:\n- BT-YY: [description of dependency]\n\nMoving to Backlog with blocked label until dependencies are resolved."
}
```

#### If Obsolete

```json
{
  "action": "update",
  "id": "BT-XX",
  "state": "Canceled"
}
```

```json
{
  "action": "comment",
  "id": "BT-XX",
  "body": "## Issue Review\n\n**Status**: Obsolete\n\n**Reason**:\n- [explain why this is no longer needed]\n- [reference any spec changes or superseding issues]\n\nMarking as Canceled."
}
```

## Comment Format

Always use this structure for the review comment:

```markdown
## Issue Review

**Status**: [Already implemented | Still open | Blocked | Obsolete]

**Documentation Review**:
- [Findings from doc review]

**Code Review**:
- [Findings from code review]
- [List any existing implementations]
- [Note any partial progress]

**Acceptance Criteria Status**:
- [x] Criteria that are done
- [ ] Criteria still pending

**Recommendations**:
- [Any suggested changes to scope, approach, or priority]

**Files to Modify** (verified):
- [Current accurate list of files]
```

## Examples

### Example: Issue Already Fixed

```
/update-issue BT-42

[Agent finds the feature is already implemented]

Comment:
## Issue Review

**Status**: Already implemented

**Documentation Review**:
- Feature matches spec in docs/beamtalk-language-features.md

**Code Review**:
- Implementation exists in crates/beamtalk-core/src/source_analysis/lexer.rs:145-203
- Tests added in crates/beamtalk-core/src/source_analysis/parser/mod.rs
- Snapshot tests in test-package-compiler/cases/lexer/

**Acceptance Criteria Status**:
- [x] Tokenize identifiers
- [x] Tokenize keywords
- [x] Include source spans

Marking as Done.
```

### Example: Issue Needs Updates

```
/update-issue BT-55

[Agent finds description is outdated]

Comment:
## Issue Review

**Status**: Still open, description updated

**Documentation Review**:
- docs/beamtalk-language-features.md was updated to include new block syntax
- Issue description references old syntax style

**Code Review**:
- No implementation exists yet
- Related parsing infrastructure in place at crates/beamtalk-core/src/source_analysis/

**Updated Acceptance Criteria**:
- Added: Support new block parameter syntax `[:x :y |]`
- Removed: Old cascade syntax (moved to separate issue)

**Files to Modify** (verified):
- crates/beamtalk-core/src/source_analysis/lexer.rs
- crates/beamtalk-core/src/source_analysis/parser/mod.rs
- crates/beamtalk-core/src/ast.rs
```

## Notes

- Always preserve existing labels when updating (add `blocked` but keep area/type labels)
- If unsure whether something is fixed, err on the side of "still open" with a detailed comment
- Reference specific file paths and line numbers when describing existing code
- Check git history if needed to understand when/how code was added
