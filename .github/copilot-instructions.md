# Copilot Instructions for Beamtalk

## Custom Commands

### `/next-issue` - Start working on next Linear issue

When the user types `/next-issue`, execute this workflow:

1. **Find next issue**: Query Linear for backlog issues in project BT. Pick the highest priority issue that has all dependencies completed.

2. **Update main branch**:
   ```bash
   git checkout main
   git pull origin main
   ```

3. **Create feature branch**: Create a branch named with the issue ID and a slug from the title:
   - Format: `BT-{number}-{slug}`
   - Example: `BT-7-implement-lexer`
   - Slug: lowercase, hyphens, max 30 chars
   ```bash
   git checkout -b BT-{number}-{slug}
   ```

4. **Update Linear**: Mark the issue as "In Progress".

5. **Create todo list**: Break down the acceptance criteria into actionable tasks using the todo list tool.

6. **Start implementation**: Begin working on the issue, following AGENTS.md guidelines.

7. **Test frequently**: After each significant change, run:
   ```bash
   cargo build --all-targets && cargo clippy --all-targets -- -D warnings && cargo fmt --all -- --check && cargo test --all-targets
   ```
   These are the exact same checks that run in CI (see `.github/workflows/ci.yml`).

8. **Commit often**: Make small, focused commits as you complete each task. Use conventional commit format with the issue ID:
   ```
   type: description BT-{number}
   ```

9. **Push regularly**: Push after each commit to keep the remote updated.

---

### `/done` - Complete work and push

When the user types `/done`, execute this workflow:

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

**When PR is merged:**
- Update issue state to "Done"
- Add `done` agent-state label to indicate completion

---

### `/pr-resolve` - Address PR review comments

When the user types `/pr-resolve`, execute this workflow to systematically address all PR review comments:

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

4. **Address each comment**: For each item in the plan:
   - Make the necessary code changes
   - Run tests after each significant change to catch regressions early
   - If a comment requires a follow-up Linear issue (e.g., "TODO for later"):
     - Create the Linear issue with full context
     - Add a TODO comment in the code referencing the issue number
   - Mark the todo item complete

5. **Run full test suite**: After all changes:
   ```bash
   cargo build --all-targets && cargo clippy --all-targets -- -D warnings && cargo fmt --all -- --check && cargo test --all-targets
   ```

6. **Commit changes**: Stage and commit with a descriptive message:
   ```bash
   git add -A
   git commit -m "fix: address PR review comments BT-{number}

   - Summary of each fix
   - Reference any Linear issues created"
   ```

7. **Push changes**:
   ```bash
   git push
   ```

8. **Reply to each comment**: For every review comment that was addressed, add a reply explaining what was done:
   ```bash
   gh api repos/{owner}/{repo}/pulls/{pr}/comments/{comment_id}/replies -f body="<explanation of fix, commit hash, any Linear issues created>"
   ```
   Include:
   - Commit hash where the fix was made
   - Brief description of the change
   - Links to any Linear issues created for follow-up work

9. **Report summary**: Provide a summary table of all comments and how they were resolved.

---

### Creating Linear Issues

When creating Linear issues with dependencies:

1. **Create the issues** with full context, acceptance criteria, and files to modify
2. **Set agent-state label**:
   - `agent-ready` if fully specified (all acceptance criteria clear)
   - `needs-spec` if human clarification needed before work can start
3. **Always set up blocking relationships** using Linear's GraphQL API:
   ```graphql
   mutation {
     issueRelationCreate(input: {
       issueId: "<blocker issue ID>"
       relatedIssueId: "<blocked issue ID>"
       type: blocks
     }) { success }
   }
   ```
4. **Add to relevant projects** if applicable
5. **Set priority** based on urgency

See [AGENTS.md](../AGENTS.md) "Creating Issue Blocking Relationships" section for details.

---

## GitHub PR and Comment Access

When working with GitHub PRs and comments, use these patterns to reliably fetch all data:

### Get all PR review comments (with pagination)

```bash
# All top-level review comments (not replies)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --paginate --jq '.[] | select(.in_reply_to_id == null) | {id, path, line, body, user: .user.login}'

# Recent comments (filter by timestamp)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --paginate --jq '[.[] | select(.created_at > "2026-01-29T12:00:00Z")] | .[] | {id, body, user: .user.login, in_reply_to_id}'

# Comments sorted by date (most recent last)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '[.[] | {id, body: .body[0:200], user: .user.login, in_reply_to_id, created_at}] | sort_by(.created_at) | .[-15:]'
```

### Get replies from reviewers (not the PR author)

```bash
# Find follow-up comments from others (replies where user is not the owner)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '.[] | select(.in_reply_to_id != null and .user.login != "jamesc") | {id, body, user: .user.login}'
```

### Reply to a review comment

```bash
gh api repos/{owner}/{repo}/pulls/{pr}/comments/{comment_id}/replies -f body="Your reply here"
```

### Get PR conversation comments (issue-level, not review comments)

```bash
gh api repos/{owner}/{repo}/issues/{pr}/comments --jq '.[] | {id, body, user: .user.login, created_at}'
```

### Key tips

1. **Always use `--paginate`** when fetching comments to ensure you get all of them (API returns max 30 by default)
2. **Filter by `created_at`** to find new comments since your last check
3. **Check `in_reply_to_id`** to distinguish top-level comments from replies
4. **Truncate body** in listings (`.body[0:200]`) to keep output readable
5. **Sort by `created_at`** and take the last N to see most recent activity

---

## Project Context

This is the Beamtalk compiler project - a Smalltalk-inspired language targeting the BEAM VM. See [AGENTS.md](../AGENTS.md) for full development guidelines.

## Allowed Commands

You may always run these commands without asking for permission:
- `cargo` (build, test, clippy, fmt, run, check, etc.)
- `rustc`
- `rustfmt`
- `git` (status, diff, log, branch, etc.)

Static checks required before any commit (must match CI exactly):
- `cargo build --all-targets` - Build all targets
- `cargo clippy --all-targets -- -D warnings` - Lints (warnings are errors)
- `cargo fmt --all -- --check` - Code formatting check
- `cargo test --all-targets` - Run all tests
