---
name: done
description: Complete work and push changes. Use when user types /done or says they are finished with the current task and ready to commit/push/create PR.
---

# Done Workflow

When activated, execute this workflow to complete work and push:

## Steps

1. **Determine Issue ID**: Extract the issue number from the branch name (e.g., `BT-10` from `BT-10-implement-erlang-codegen`). If not in branch name, try:
   
   a. **Worktree name**: If in a git worktree with a name matching `BT-{number}`:
      ```bash
      # Check if this is a worktree
      git rev-parse --git-dir 2>/dev/null | grep -q "worktrees"
      
      # Extract issue ID from directory name
      basename "$(pwd)" | grep -oE '^BT-[0-9]+'
      ```
      Example: `/workspaces/BT-34` → issue `BT-34`

2. **Check branch**: Verify we're NOT on `main` branch. If on main, stop and tell the user to create a feature branch first.

3. **Run static checks**:
   ```bash
   just ci
   ```
   This runs all CI checks (build, clippy, fmt-check, test, test-e2e) that must match exactly what CI runs.
   If any check fails, report the errors and stop.

4. **Stage changes**:
   ```bash
   git add -A
   ```

5. **Check for changes**: Run `git status`. If there's nothing to commit, inform the user and stop.

6. **Generate commit message**: Based on the staged diff (`git diff --cached`), create a conventional commit message:
   - Use format: `type: short description`
   - Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`
   - Keep first line under 72 characters
   - Add bullet points for details if multiple changes

7. **Commit**:
   ```bash
   git commit -m "<generated message>"
   ```

8. **Push**:
   ```bash
   git push -u origin HEAD
   ```

9. **Create Pull Request**: Use the issue ID from step 1. Fetch the Linear issue details. Create a PR:
   ```bash
   gh pr create --title "<Issue Title> (BT-{number})" --body "<Issue description with link to Linear issue>"
   ```
   The PR body should include:
   - Link to Linear issue: `https://linear.app/beamtalk/issue/BT-{number}`
   - Brief summary of what was implemented
   - List of key changes

10. **Update Linear acceptance criteria**: Get the Linear issue from step 1, review the acceptance criteria, and add a comment marking which criteria have been completed with checkmarks (✅). Format as a structured summary showing what was implemented.

11. **Update Linear state**: Mark the Linear issue as "In Review".

12. **Report success**: Confirm the commit was pushed, PR was created (include PR URL), and Linear was updated.

## When PR is merged

- Update issue state to "Done"
- Add `done` agent-state label to indicate completion
