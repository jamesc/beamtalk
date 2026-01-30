---
name: pr-resolve
description: Address PR review comments systematically. Use when user types /pr-resolve or asks to fix/address PR feedback, review comments, or requested changes.
---

# PR Resolve Workflow

When activated, execute this workflow to systematically address all PR review comments:

## Steps

1. **Get PR review comments**: Fetch all unresolved review comments from the active PR:
   ```bash
   gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '.[] | select(.in_reply_to_id == null) | {id, path, body}'
   ```

2. **Analyze and plan**: For each review comment:
   - Understand what the reviewer is asking for
   - Determine if it needs a code fix, documentation, Linear issue, or just clarification
   - Create a todo list with all items to address

3. **Run tests first**: Verify current state passes all checks:
   ```bash
   cargo build --all-targets && cargo clippy --all-targets -- -D warnings && cargo fmt --all -- --check && cargo test --all-targets
   ```
   Also run Erlang tests if runtime changes are involved:
   ```bash
   cd runtime && rebar3 eunit
   ```

4. **Write tests first (TDD)**: For each code fix needed:
   - Write a failing test that demonstrates the bug or missing behavior
   - Run the test to confirm it fails as expected
   - This ensures the fix is verifiable and prevents regressions

5. **Address each comment**: For each item in the plan:
   - Make the necessary code changes to make the test pass
   - Run tests after each significant change to catch regressions early
   - If a comment requires a follow-up Linear issue (e.g., "TODO for later"):
     - Create the Linear issue with full context
     - Add a TODO comment in the code referencing the issue number
   - Mark the todo item complete

6. **Run full test suite**: After all changes:
   ```bash
   cargo build --all-targets && cargo clippy --all-targets -- -D warnings && cargo fmt --all -- --check && cargo test --all-targets
   ```

7. **Commit changes**: Stage and commit with a descriptive message:
   ```bash
   git add -A
   git commit -m "fix: address PR review comments BT-{number}

   - Summary of each fix
   - Reference any Linear issues created"
   ```

8. **Push changes**:
   ```bash
   git push
   ```

9. **Reply to each comment**: For every review comment that was addressed, add a reply explaining what was done:
   ```bash
   gh api repos/{owner}/{repo}/pulls/{pr}/comments/{comment_id}/replies -f body="<explanation of fix, commit hash, any Linear issues created>"
   ```
   Include:
   - Commit hash where the fix was made
   - Brief description of the change
   - Links to any Linear issues created for follow-up work

10. **Report summary**: Provide a summary table of all comments and how they were resolved.
