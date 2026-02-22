---
name: resolve-pr
description: Address PR review comments systematically. Use when user types /resolve-pr or asks to fix/address PR feedback, review comments, or requested changes.
---

# PR Resolve Workflow

When activated, execute this workflow to systematically address all PR review comments:

## Steps

1. **Determine Issue ID**: Use the same resolution logic as `pick-issue` step 1:
   - Extract from branch name (e.g., `BT-10` from `BT-10-implement-erlang-codegen`)
   - Fall back to worktree name (e.g., `/workspaces/BT-34` → `BT-34`)
   - If neither works, ask the user

2. **Get PR review comments**: Fetch all unresolved review threads and general PR comments:

   **Review threads** (code-level comments): Use the GitHub MCP `pull_request_read` tool with method `get_review_comments` to get threads with `isResolved` and `isOutdated` metadata. Filter to unresolved, non-outdated threads.

   **General PR comments** (conversation-level): Use the GitHub MCP `pull_request_read` tool with method `get_comments` to get top-level PR comments.

   Focus on unresolved items — skip threads already marked as resolved or outdated.

3. **Analyze and plan**: For each review comment:
   - Understand what the reviewer is asking for
   - Determine if it needs a code fix, documentation, Linear issue, or just clarification
   - Create a todo list with all items to address

4. **Run tests first**: Verify current state passes all checks:
   ```bash
   just ci
   ```
   This runs all CI checks (build, clippy, fmt-check, test, test-e2e).

5. **Write tests for behavioral changes**: For code fixes that change logic or behavior:
   - Write a failing test that demonstrates the bug or missing behavior
   - Run the test to confirm it fails as expected
   - This ensures the fix is verifiable and prevents regressions
   
   **Skip tests for:** Naming changes, documentation updates, style/formatting fixes, comment improvements — these don't need TDD.

6. **Address each comment**: For each item in the plan:
   - Make the necessary code changes to make the test pass
   - Run tests after each significant change to catch regressions early
   - If a comment requires a follow-up Linear issue (e.g., "TODO for later"):
     - Create the Linear issue with full context
     - Add a TODO comment in the code referencing the issue number
   - Mark the todo item complete

7. **Run full test suite**: After all changes:
   ```bash
   just ci
   ```

8. **Commit changes**: Stage and commit with a descriptive message (using issue ID from step 1):
   ```bash
   git add -A
   git commit -m "fix: address PR review comments BT-{number}

   - Summary of each fix
   - Reference any Linear issues created"
   ```

9. **Push changes**:
   ```bash
   git push
   ```

10. **Reply to each comment**: For every review comment that was addressed, add a reply explaining what was done:
   ```bash
   gh api repos/{owner}/{repo}/pulls/{pr}/comments/{comment_id}/replies -f body="<explanation of fix, commit hash, any Linear issues created>"
   ```
   Include:
   - Commit hash where the fix was made
   - Brief description of the change
   - Links to any Linear issues created for follow-up work

11. **Resolve review threads**: After replying, resolve each addressed review thread using the GraphQL API:
    ```bash
    gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "<thread_node_id>"}) { thread { isResolved } } }'
    ```
    The thread node ID (e.g., `PRRT_kwDO...`) is available from the `get_review_comments` response in step 2. Only resolve threads where the fix has been committed and pushed.

12. **Report summary**: Provide a summary table of all comments and how they were resolved.

13. **Auto-chain to done**: If all review comments have been successfully resolved (no failures, no pending issues), automatically activate the `done` skill:
    - Inform the user that all PR comments have been addressed
    - Activate the `done` skill without waiting for user confirmation
    
    If there are any issues or manual steps needed, report them and wait for user input instead.
