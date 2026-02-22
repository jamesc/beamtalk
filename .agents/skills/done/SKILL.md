---
name: done
description: Complete work and push changes. Use when user types /done or says they are finished with the current task and ready to commit/push/create PR.
---

# Done Workflow

When activated, execute this workflow to complete work and push:

## Steps

1. **Determine Issue ID**: Use the same resolution logic as `pick-issue` step 1:
   - Extract from branch name (e.g., `BT-10` from `BT-10-implement-erlang-codegen`)
   - Fall back to worktree name (e.g., `/workspaces/BT-34` → `BT-34`)
   - If branch name starts with `chore/`, `docs/`, or `refactor/` and has no `BT-` prefix, treat as a standalone chore — no issue ID needed. Skip steps 10 and 11 (Linear updates), and omit `BT-{number}` from commit message and PR title.
   - If neither works and not a chore branch, ask the user

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
   - If `DOC_ONLY=false`: Run fast static checks only (tests should have been run during development):
   ```bash
   just build && just clippy && just fmt-check
   ```
   If any check fails, report the errors and stop.

6. **Generate commit message**: Based on the staged diff (`git diff --cached`), create a conventional commit message:
   - Use format: `type: short description BT-{number}` (include issue ID when available)
   - Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`
   - Keep first line under 72 characters
   - For chore/docs/refactor branches without an issue, omit `BT-{number}` (e.g., `chore: clean up dead code`)
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
   
   **If no PR exists:** Use the issue ID from step 1 (if available). Fetch the Linear issue details. Create a PR:
   ```bash
   gh pr create --title "<Issue Title> (BT-{number})" --body "<Issue description with link to Linear issue>"
   ```
   The PR body should include:
   - Link to Linear issue: `https://linear.app/beamtalk/issue/BT-{number}` (if issue exists)
   - Brief summary of what was implemented
   - List of key changes
   
   For chore/docs/refactor branches without a Linear issue, use a descriptive title based on the commit message and omit the Linear link.

10. **Update Linear acceptance criteria**: Get the Linear issue from step 1, review the acceptance criteria, and add a comment marking which criteria have been completed with checkmarks (✅). Format as a structured summary showing what was implemented.

11. **Update Linear state**: Mark the Linear issue as "In Review".

12. **Wait for automated code reviews** (first review only): If a repository ruleset is configured to automatically request Copilot and/or CodeRabbit code review, poll for them after **initial PR creation only**. If the PR already existed (step 9), skip polling — these bots typically only review once and subsequent pushes don't trigger a new review.
    
    Poll for **both** Copilot and CodeRabbit reviews simultaneously:
    
    ```text
    Poll: check for reviews every 60 seconds, up to 10 attempts (10 minutes max)
    Stop polling once BOTH reviews have arrived (or timeout)
    ```
    
    Use `gh api` against the PR reviews endpoint to check for reviews by bot identity:
    
    ```bash
    # Check for Copilot review
    gh api repos/{owner}/{repo}/pulls/{pr}/reviews --paginate --jq '.[] | select(.user.login == "copilot-pull-request-reviewer[bot]")'
    
    # Check for CodeRabbit review
    gh api repos/{owner}/{repo}/pulls/{pr}/reviews --paginate --jq '.[] | select(.user.login == "coderabbitai[bot]")'
    ```
    
    Also check review threads using `get_review_comments` — both bots leave inline code comments as review threads.
    
    **Important:** Only gate on verified bot identity (`user.login`). Never match on review body content alone, as that can be spoofed by arbitrary reviewers.
    
    **Bot identities:**

    | Bot | `user.login` |
    |-----|-------------|
    | Copilot | `copilot-pull-request-reviewer[bot]` |
    | CodeRabbit | `coderabbitai[bot]` |

    **If reviews already exist** (PR was pre-existing):
    - Check if there are unresolved review threads from existing reviews
    - If all threads are resolved, report: "Copilot review already completed ✅" and/or "CodeRabbit review already completed ✅"
    - If unresolved threads exist, execute `/resolve-pr` workflow inline
    - Do NOT poll for new reviews
    
    **If a review arrives with comments** (new PR):
    - Inform the user: "{Bot} review received with N comments. Resolving..."
    - Execute the `/resolve-pr` workflow inline (steps 2-11 from resolve-pr skill):
      - Fetch and analyze all unresolved review threads
      - Plan fixes for each comment
      - Run tests, make changes, run tests again
      - Commit with message: `fix: address {Bot} review comments BT-{number}`
      - Push changes
      - Reply to each review comment explaining the fix
      - Report summary of all changes
    - After resolving, report the summary of changes made
    - If the second bot's review hasn't arrived yet, continue polling for it
    
    **If a review arrives with no comments (approved):**
    - Report: "{Bot} review passed with no comments ✅"
    
    **If timeout (no review after 10 minutes):**
    - Report which reviews were received and which timed out
    - Example: "Copilot review passed ✅. CodeRabbit review not received after 10 minutes. Run `/resolve-pr` later if comments arrive."
    - Continue to step 13 (do not block completion)

13. **Report success**: Confirm the commit was pushed, PR was created/updated (include PR URL), Linear was updated, and review status for both Copilot and CodeRabbit.

## When PR is merged

> **Note:** These steps are manual — there is no automation to detect merge events yet.

- Update issue state to "Done"
- Add `done` agent-state label to indicate completion
