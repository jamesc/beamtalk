---
name: worktree-stop
description: Remove a git worktree. Use when user types /worktree-stop or wants to clean up a worktree after finishing work.
---

# Worktree Stop Workflow

When activated with a branch name, guide the user to remove a worktree:

## Steps

1. **Check current worktrees**: Run `git worktree list` to verify the worktree exists.

2. **Warn about unsaved work**: Remind the user to commit/push any changes in that worktree before removing it.

3. **Inform about the script**: Tell the user to run the worktree-rm script from the HOST machine:

   **Windows:**
   ```powershell
   .\scripts\worktree-rm.ps1 <branch>
   ```

   **Linux/Mac:**
   ```bash
   ./scripts/worktree-rm.sh <branch>
   ```

4. **Explain what happens**:
   - Fixes `.git` file if it has container paths
   - Removes the worktree via `git worktree remove`
   - Falls back to manual cleanup if needed
   - Optionally deletes the local branch

## Important Note

Worktrees used with devcontainers have their `.git` file modified to point to container paths. The script handles this automatically.
