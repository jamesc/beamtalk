---
name: worktree-start
description: Start working in a new git worktree. Use when user types /worktree-start or wants to create a new worktree for parallel development.
---

# Worktree Start Workflow

When activated with a branch name, guide the user to create a new worktree:

## Steps

1. **Check if already in a worktree**: Run `git worktree list` to see current worktrees.

2. **Inform about the script**: Tell the user to run the worktree-new script from the HOST machine (not inside a container):

   **Windows:**
   ```powershell
   .\scripts\worktree-new.ps1 <branch>
   ```

   **Linux/Mac:**
   ```bash
   ./scripts/worktree-new.sh <branch>
   ```

3. **Explain what happens**:
   - Creates worktree at `../<branch>/` (sibling to main repo)
   - Creates/checks out the branch
   - Starts a devcontainer
   - Connects to shell inside container

4. **After container starts**: Once in the new container, the user can start a new Copilot session there.

## Important Note

This command cannot create the worktree directly because worktree operations must run on the host, not inside a container.
