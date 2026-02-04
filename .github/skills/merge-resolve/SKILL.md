---
name: merge-resolve
description: Update main branch, merge into current branch, and resolve conflicts. Use when user types /merge-resolve or asks to merge main/resolve conflicts.
---

# Merge Resolve Workflow

When activated, execute this workflow to update main and merge it into the current branch, resolving any conflicts:

## Steps

1. **Check current state**: Verify there are no uncommitted changes:
   ```bash
   git status --porcelain
   ```
   If there are changes, commit or stash them first.

2. **Get current branch name**:
   ```bash
   git branch --show-current
   ```
   Save this to return to later.

3. **Fetch latest from origin**:
   ```bash
   git fetch origin
   ```

4. **Update main branch**:
   ```bash
   git checkout main
   git pull origin main
   ```

5. **Return to feature branch**:
   ```bash
   git checkout <feature-branch>
   ```

6. **Attempt merge**:
   ```bash
   git merge main
   ```
   
   If merge succeeds without conflicts:
   - Run tests to verify everything still works
   - Push the merge commit
   - Report success and skip to step 11

7. **If conflicts occur, analyze them**:
   ```bash
   git status
   git diff --name-only --diff-filter=U
   ```
   List all files with conflicts.

8. **For each conflicted file**:
   - Read the file to see conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`)
   - Understand what changed in both branches
   - Resolve the conflict by:
     - Keeping the feature branch changes if they're the intended behavior
     - Keeping main branch changes if they're bug fixes or improvements
     - Merging both if compatible
     - Manually editing to combine changes intelligently
   - Remove conflict markers
   - Stage the resolved file: `git add <file>`

9. **Verify resolution**:
   ```bash
   just ci
   ```
   This runs all CI checks (build, clippy, fmt-check, test, test-e2e).
   
   If tests fail, review and fix the merge resolution.

10. **Complete the merge**:
    ```bash
    git commit  # Complete the merge commit
    git push
    ```

11. **Report summary**:
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

- **Binary conflicts** (Cargo.lock): `git checkout --theirs Cargo.lock && cargo build`
- **Deleted files**: Determine if deletion from main is intentional; if so, `git rm <file>`
- **Renamed files**: Git usually handles automatically; if not, manually apply feature changes to renamed file
- **Merge commit already exists**: Skip merge step, just report current state

## Error Handling

If merge cannot be resolved automatically:
1. Document the conflict clearly
2. Ask user for guidance on specific conflicts
3. Do not guess or make assumptions about intent
4. If truly stuck, abort merge with `git merge --abort` and report the issue
