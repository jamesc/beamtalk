# Copilot Instructions for Beamtalk

## Custom Commands

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

8. **Report success**: Confirm the commit was pushed with the branch name and commit hash.

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
