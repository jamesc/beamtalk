---
name: pick-issue
description: Start working on next Linear issue. Use when user types /pick-issue or asks to pick up the next task from the backlog.
---

# Next Issue Workflow

When activated, execute this workflow to start working on the next Linear issue:

## Steps

### 1. Determine Issue ID

The issue ID is determined in priority order:

1. **Explicit argument**: If user provides an issue ID, use that issue.
   - `/pick-issue BT-42` → issue BT-42
   - `/pick-issue 42` → issue BT-42 (BT- prefix added automatically)

2. **Worktree name**: If in a git worktree with a name matching `BT-{number}`, use that issue:
   ```bash
   # Check if this is a worktree
   git rev-parse --git-dir 2>/dev/null | grep -q "worktrees"
   
   # Extract issue ID from directory name
   basename "$(pwd)" | grep -oE '^BT-[0-9]+'
   ```
   Example: `/workspaces/BT-34` → issue `BT-34`

3. **Backlog query**: Query Linear for the highest priority `agent-ready` issue from the backlog that has no unresolved blockers (all blocking issues must be Done).

### 2. Fetch Issue Details

Get the full issue details from Linear:
```json
{
  "action": "get",
  "id": "BT-{number}"
}
```

### 3. Validate Issue State

Before proceeding, verify the issue is workable:
- **Must have `agent-ready` label** — if `needs-spec`, stop and tell the user the issue needs specification first.
- **Must not be `Done` or `In Progress`** — if already done, inform the user. If in progress by someone else, warn and ask for confirmation.
- **Must not be `blocked`** — check that all blocking issues are Done. If blockers remain, list them and stop.

If using backlog query (step 1.3), these checks are implicit. If using explicit ID or worktree, these checks are essential.

### 4. Check Working Tree

Before switching branches, verify no uncommitted changes would be lost:
```bash
git status --porcelain
```
If there are uncommitted changes, warn the user and suggest stashing or committing first. Do not proceed with `git checkout` until the working tree is clean.

### 5. Update Main Branch (if not already on issue branch)

Skip this step if:
- Already on a branch named `BT-{number}*`
- In a worktree for this issue

Otherwise:
```bash
git checkout main
git pull origin main
```

### 6. Create Feature Branch (if not already on issue branch)

Skip if already on a matching branch. Otherwise create:
- Format: `BT-{number}-{slug}`
- Example: `BT-7-implement-lexer`
- Slug: lowercase, hyphens, max 30 chars

```bash
git checkout -b BT-{number}-{slug}
```

### 7. Update Linear

Mark the issue as "In Progress".

### 8. Create Todo List

Break down the acceptance criteria into actionable tasks using the todo list tool.

### 9. Start Implementation

Begin working on the issue, following AGENTS.md guidelines.

### 10. Test Frequently

After each significant change, run the fast test suite:
```bash
just test
```
This runs unit tests (~10s). Save full CI for the final check before code review.

### 11. Commit Often

Make small, focused commits as you complete each task. Use conventional commit format with the issue ID:
```
type: description BT-{number}
```

### 12. Push Regularly

Push after each commit to keep the remote updated.

### 13. Run Full CI

Before completing, run the full CI suite to catch any issues:
```bash
just ci
```
This runs all CI checks (build, clippy, fmt-check, test, test-e2e). Fix any failures before proceeding.

### 14. Check for Existing PR

After completing implementation:
```bash
gh pr list --head $(git branch --show-current) --json number,state --jq '.[0]'
```

If a PR exists and has unresolved review comments, automatically chain to `resolve-pr` skill:
- Inform the user that PR review comments need to be addressed
- Activate the `resolve-pr` skill without waiting for user confirmation

If no PR exists or PR has no review comments, proceed to step 15.

### 15. Automatic Code Review

After implementation is complete (no existing PR or no review comments), **automatically chain to the `review-code` skill** without waiting for user confirmation:
- Inform the user: "Implementation complete. Running code review..."
- Activate the `review-code` skill

### 16. Automatic Done (if code review passes)

If the code review finds **no critical or recommended issues** (no 🔴 or 🟡 items that required changes), **automatically chain to the `done` skill** without waiting for user confirmation:
- Inform the user: "Code review passed. Completing work..."
- Activate the `done` skill

If the code review **did implement fixes or improvements**, run `just ci` to verify the fixes pass, then automatically chain to the `done` skill.

**Note:** Only stop and wait for user input if the code review identifies 🔵 issues that require user decisions (architectural choices, scope questions, etc.).
