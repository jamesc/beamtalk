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
   cargo fmt && cargo clippy && cargo test
   ```

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
   cargo fmt --check && cargo clippy && cargo test
   ```
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

8. **Update Linear acceptance criteria**: Get the Linear issue for the current branch, review the acceptance criteria, and add a comment marking which criteria have been completed with checkmarks (✅). Format as a structured summary showing what was implemented.

9. **Update Linear state**: Mark the Linear issue as "In Review".

10. **Report success**: Confirm the commit was pushed with the branch name and commit hash.

## Project Context

This is the Beamtalk compiler project - a Smalltalk-inspired language targeting the BEAM VM. See [AGENTS.md](../AGENTS.md) for full development guidelines.

## Allowed Commands

You may always run these commands without asking for permission:
- `cargo` (build, test, clippy, fmt, run, check, etc.)
- `rustc`
- `rustfmt`
- `git` (status, diff, log, branch, etc.)

Static checks required before any commit:
- `cargo fmt --check` - Code formatting
- `cargo clippy` - Lints
- `cargo test` - All tests pass
