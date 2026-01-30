---
name: next-issue
description: Start working on next Linear issue. Use when user types /next-issue or asks to pick up the next task from the backlog.
---

# Next Issue Workflow

When activated, execute this workflow to start working on the next Linear issue:

## Steps

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
