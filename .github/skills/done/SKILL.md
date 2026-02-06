---
name: done
description: Complete work and push changes. Use when user types /done or says they are finished with the current task and ready to commit/push/create PR.
---

# Done Workflow

When activated, execute this workflow to complete work and push:

## Steps

1. **Determine Issue ID**: Use the same resolution logic as `next-issue` step 1:
   - Extract from branch name (e.g., `BT-10` from `BT-10-implement-erlang-codegen`)
   - Fall back to worktree name (e.g., `/workspaces/BT-34` → `BT-34`)
   - If neither works, ask the user

2. **Check branch**: Verify we're NOT on `main` branch. If on main, stop and tell the user to create a feature branch first.

3. **Stage changes**:
   ```bash
   git add -A
   ```

4. **Check for changes**: Run `git status`. If there's nothing to commit, inform the user and stop.

5. **Run static checks** (skip for doc-only changes):
   Check if the changeset is documentation/config-only:
   ```bash
   # Include both staged (uncommitted) and committed changes vs main
   CHANGED_FILES=$(git diff --cached --name-only 2>/dev/null; git diff --name-only main...HEAD 2>/dev/null || true)
   CHANGED_FILES=$(echo "$CHANGED_FILES" | sort -u)
   DOC_ONLY=true
   for f in $CHANGED_FILES; do
     [ -z "$f" ] && continue
     case "$f" in
       *.md|*.txt|*.json|*.yaml|*.yml|*.toml|Justfile|LICENSE|docs/*|.github/skills/*) ;;
       *) DOC_ONLY=false; break ;;
     esac
   done
   ```
   - If `DOC_ONLY=true`: Skip CI checks entirely (no build/test needed)
   - If `DOC_ONLY=false`: Run full CI checks:
   ```bash
   just ci
   ```
   This runs all CI checks (build, clippy, fmt-check, test, test-e2e) that must match exactly what CI runs.
   If any check fails, report the errors and stop.

6. **Generate commit message**: Based on the staged diff (`git diff --cached`), create a conventional commit message:
   - Use format: `type: short description BT-{number}`
   - Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`
   - Keep first line under 72 characters
   - Always include the issue ID (e.g., `feat: add lexer tokens BT-42`)
   - Add bullet points for details if multiple changes

7. **Commit**:
   ```bash
   git commit -m "<generated message>"
   ```

8. **Push**:
   ```bash
   git push -u origin HEAD
   ```

9. **Create or update Pull Request**: Check if a PR already exists for this branch:
   ```bash
   gh pr list --head $(git branch --show-current) --json number,url --jq '.[0]'
   ```
   
   **If PR exists:** Skip creation — the push in step 8 already updated it. Note the existing PR URL for reporting.
   
   **If no PR exists:** Use the issue ID from step 1. Fetch the Linear issue details. Create a PR:
   ```bash
   gh pr create --title "<Issue Title> (BT-{number})" --body "<Issue description with link to Linear issue>"
   ```
   The PR body should include:
   - Link to Linear issue: `https://linear.app/beamtalk/issue/BT-{number}`
   - Brief summary of what was implemented
   - List of key changes

10. **Update Linear acceptance criteria**: Get the Linear issue from step 1, review the acceptance criteria, and add a comment marking which criteria have been completed with checkmarks (✅). Format as a structured summary showing what was implemented.

11. **Update Linear state**: Mark the Linear issue as "In Review".

12. **Report success**: Confirm the commit was pushed, PR was created/updated (include PR URL), and Linear was updated.

## When PR is merged

> **Note:** These steps are manual — there is no automation to detect merge events yet.

- Update issue state to "Done"
- Add `done` agent-state label to indicate completion
