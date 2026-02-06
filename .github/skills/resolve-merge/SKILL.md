---
name: resolve-merge
description: Update main branch, merge into current branch, and resolve conflicts. Use when user types /resolve-merge or asks to merge main/resolve conflicts.
---

# Merge Resolve Workflow

When activated, execute this workflow to update main and merge it into the current branch, resolving any conflicts:

## Steps

1. **Check current state**: Verify there are no uncommitted changes:
   ```bash
   git status --porcelain
   ```
   If there are uncommitted changes, commit them first (do not stash — stash pop after merge can introduce additional conflicts).

2. **Get current branch name**:
   ```bash
   git branch --show-current
   ```
   Verify we're NOT on `main`. If on main, stop and tell the user.

3. **Fetch and merge origin/main** (no branch switch needed):
   ```bash
   git fetch origin
   git merge origin/main
   ```
   
   If merge succeeds without conflicts:
   - Run full CI to verify: `just ci`
   - Push the merge commit: `git push origin HEAD`
   - Report success and skip to step 8

4. **If conflicts occur, analyze them**:
   ```bash
   git status
   git diff --name-only --diff-filter=U
   ```
   List all files with conflicts.

5. **For each conflicted file**:
   - Read the file to see conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`)
   - Understand what changed in both branches
   - Resolve the conflict by:
     - Keeping the feature branch changes if they're the intended behavior
     - Keeping main branch changes if they're bug fixes or improvements
     - Merging both if compatible
     - Manually editing to combine changes intelligently
   - Remove conflict markers
   - Stage the resolved file: `git add <file>`

6. **Verify resolution**:
   ```bash
   just ci
   ```
   This runs all CI checks (build, clippy, fmt-check, test, test-e2e).
   
   If tests fail, review and fix the merge resolution.
   
   If snapshot tests fail due to intentional changes from main:
   ```bash
   cargo insta accept
   ```
   Then re-run `just ci` to confirm.

7. **Complete the merge**:
    ```bash
    git commit  # Complete the merge commit
    git push origin HEAD
    ```

8. **Report summary**:
    - List files that had conflicts (if any)
    - Describe how each conflict was resolved
    - Confirm all tests pass
    - Show the merge commit hash

## Conflict Resolution Strategy

When resolving conflicts, follow these priorities:

1. **Documentation conflicts**: Usually safe to keep both changes, manually merge
2. **Test conflicts**: Keep both tests unless they're testing the same thing
3. **Code conflicts**:
   - If feature branch adds new functionality: keep feature changes
   - If main has bug fixes: incorporate the bug fix into feature code
   - If both modify same logic: manually merge to preserve both intents
4. **Dependency conflicts** (Cargo.toml, package.json):
   - Keep the higher version number
   - If both added different deps, keep both
5. **Generated code conflicts**: Regenerate if possible, otherwise keep feature branch

## Example Conflict Resolution

```rust
<<<<<<< HEAD (feature branch)
pub fn analyse(module: &Module) -> AnalysisResult {
    let mut analyser = Analyser::new();
    analyser.analyse_module(module);
    analyser.result
}
=======
pub fn analyze(module: &Module) -> AnalysisResult {
    // TODO: Implement semantic analysis
    AnalysisResult::new()
}
>>>>>>> main

// Resolution: Keep feature implementation, preserve any main branch improvements
pub fn analyse(module: &Module) -> AnalysisResult {
    let mut analyser = Analyser::new();
    analyser.analyse_module(module);
    analyser.result
}
```

## Edge Cases

- **Cargo.lock conflicts**: `git checkout --theirs Cargo.lock && cargo build` (cargo will add any missing deps from feature branch)
- **rebar.lock conflicts**: `cd runtime && git checkout --theirs rebar.lock && rebar3 upgrade --all` to regenerate
- **Deleted files**: Determine if deletion from main is intentional; if so, `git rm <file>`
- **Renamed files**: Git usually handles automatically; if not, manually apply feature changes to renamed file
- **Merge commit already exists**: Skip merge step, just report current state
- **Snapshot test failures**: Run `cargo insta accept` if changes from main are intentional, then re-verify

## Error Handling

If merge cannot be resolved automatically:
1. Document the conflict clearly
2. Ask user for guidance on specific conflicts
3. Do not guess or make assumptions about intent
4. If truly stuck, abort merge with `git merge --abort` and report the issue
