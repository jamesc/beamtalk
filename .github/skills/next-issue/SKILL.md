---
name: next-issue
description: Start working on next Linear issue. Use when user types /next-issue or asks to pick up the next task from the backlog.
---

# Next Issue Workflow

When activated, execute this workflow to start working on the next Linear issue:

## Steps

### 1. Determine Issue ID

The issue ID is determined in priority order:

1. **Explicit argument**: If user provides an issue ID, use that issue.
   - `/next-issue BT-42` → issue BT-42
   - `/next-issue 42` → issue BT-42 (BT- prefix added automatically)

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

### 3. Update Main Branch (if not already on issue branch)

Skip this step if:
- Already on a branch named `BT-{number}*`
- In a worktree for this issue

Otherwise:
```bash
git checkout main
git pull origin main
```

### 4. Create Feature Branch (if not already on issue branch)

Skip if already on a matching branch. Otherwise create:
- Format: `BT-{number}-{slug}`
- Example: `BT-7-implement-lexer`
- Slug: lowercase, hyphens, max 30 chars

```bash
git checkout -b BT-{number}-{slug}
```

### 5. Update Linear

Mark the issue as "In Progress".

### 6. Create Todo List

Break down the acceptance criteria into actionable tasks using the todo list tool.

### 7. Start Implementation

Begin working on the issue, following AGENTS.md guidelines.

### 8. Test Frequently

After each significant change, run:
```bash
cargo build --all-targets && cargo clippy --all-targets -- -D warnings && cargo fmt --all -- --check && cargo test --all-targets
```
These are the exact same checks that run in CI (see `.github/workflows/ci.yml`).

### 9. Commit Often

Make small, focused commits as you complete each task. Use conventional commit format with the issue ID:
```
type: description BT-{number}
```

### 10. Push Regularly

Push after each commit to keep the remote updated.

### 11. Check for Existing PR

After completing implementation:
```bash
gh pr list --head $(git branch --show-current) --json number,state --jq '.[0]'
```

If a PR exists and has unresolved review comments, automatically chain to `pr-resolve` skill:
- Inform the user that PR review comments need to be addressed
- Activate the `pr-resolve` skill without waiting for user confirmation

If no PR exists or PR has no review comments, inform the user that implementation is complete and ready for `/done`.
