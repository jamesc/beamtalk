---
name: done
description: Complete work and push changes. Use when user types /done or says they are finished with the current task and ready to commit/push/create PR.
---

# Done Workflow

When activated, execute this workflow to complete work and push:

## Steps

1. **Check branch**: Verify we're NOT on `main` branch. If on main, stop and tell the user to create a feature branch first.

2. **Run static checks**:
   ```bash
   cargo build --all-targets
   cargo clippy --all-targets -- -D warnings
   cargo fmt --all -- --check
   cargo test --all-targets
   ```
   These must match exactly what CI runs (see `.github/workflows/ci.yml`).
   If any check fails, report the errors and stop.

3. **Stage changes**:
   ```bash
   git add -A
   ```

4. **Check for changes**: Run `git status`. If there's nothing to commit, inform the user and stop.

5. **Generate commit message**: Based on the staged diff (`git diff --cached`), create a conventional commit message:
   - Use format: `type: short description`
   - Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`
   - Keep first line under 72 characters
   - Add bullet points for details if multiple changes

6. **Commit**:
   ```bash
   git commit -m "<generated message>"
   ```

7. **Push**:
   ```bash
   git push -u origin HEAD
   ```

8. **Create Pull Request**: Extract the issue number from the branch name (e.g., `BT-10` from `BT-10-implement-erlang-codegen`). Fetch the Linear issue details. Create a PR:
   ```bash
   gh pr create --title "<Issue Title> (BT-{number})" --body "<Issue description with link to Linear issue>"
   ```
   The PR body should include:
   - Link to Linear issue: `https://linear.app/beamtalk/issue/BT-{number}`
   - Brief summary of what was implemented
   - List of key changes

9. **Update Linear acceptance criteria**: Get the Linear issue for the current branch, review the acceptance criteria, and add a comment marking which criteria have been completed with checkmarks (✅). Format as a structured summary showing what was implemented.

10. **Update Linear state**: Mark the Linear issue as "In Review".

11. **Report success**: Confirm the commit was pushed, PR was created (include PR URL), and Linear was updated.

## When PR is merged

- Update issue state to "Done"
- Add `done` agent-state label to indicate completion
