#!/bin/bash
# Script to install pre-commit hook for cargo fmt
# Run this once to set up automatic code formatting before commits

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Find the git directory (handles both normal repos and worktrees)
if [ -f "$REPO_ROOT/.git" ]; then
    # Worktree: read git directory from .git file
    GIT_DIR=$(grep "gitdir:" "$REPO_ROOT/.git" | cut -d' ' -f2)
    if [ ! -d "$GIT_DIR" ]; then
        echo "Error: Git directory not found: $GIT_DIR"
        exit 1
    fi
else
    # Normal repository
    GIT_DIR="$REPO_ROOT/.git"
fi

HOOKS_DIR="$GIT_DIR/hooks"
HOOK_FILE="$HOOKS_DIR/pre-commit"

# Create hooks directory if it doesn't exist
mkdir -p "$HOOKS_DIR"

# Create the pre-commit hook
cat > "$HOOK_FILE" << 'EOF'
#!/bin/bash
# Pre-commit hook: auto-format Rust code
# This prevents CI format check failures

echo "🔧 Running cargo fmt --all..."
cargo fmt --all

# Re-add any Rust files that were formatted
changed_rs_files=$(git diff --name-only --cached | grep '\.rs$' || true)

if [ -n "$changed_rs_files" ]; then
    echo "$changed_rs_files" | xargs git add
fi

echo "✅ Code formatted"
exit 0
EOF

chmod +x "$HOOK_FILE"

echo "✅ Pre-commit hook installed at: $HOOK_FILE"
echo "   Code will be automatically formatted before each commit."
