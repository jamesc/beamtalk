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

8. **Create Pull Request**: Extract the issue number from the branch name (e.g., `BT-10` from `BT-10-implement-erlang-codegen`). Fetch the Linear issue details. Create a PR using:
   ```bash
   gh pr create --title "<Issue Title> (BT-{number})" --body "<Issue description with link to Linear issue>"
   ```
   The PR body should include:
   - Link to Linear issue: `https://linear.app/beamtalk/issue/BT-{number}`
   - Brief summary of what was implemented
   - List of key changes

9. **Update Linear acceptance criteria**: Get the Linear issue for the current branch, review the acceptance criteria, and add a comment marking which criteria have been completed with checkmarks (✅). Format as a structured summary showing what was implemented.

10. **Update Linear state**: Mark the Linear issue as "In Review".

11. **Report success**: Confirm the commit was pushed, PR was created (with PR URL), and Linear was updated.

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
