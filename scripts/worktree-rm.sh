#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Stop and remove a git worktree, fixing any container path issues.
#
# Usage:
#   ./stop-worktree.sh <branch-name> [-f|--force]
#
# Examples:
#   ./stop-worktree.sh BT-99-feature
#   ./stop-worktree.sh BT-99 --force

set -e

BRANCH="${1:-}"
FORCE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -f|--force)
            FORCE="--force"
            shift
            ;;
        *)
            if [ -z "$BRANCH" ]; then
                BRANCH="$1"
            fi
            shift
            ;;
    esac
done

if [ -z "$BRANCH" ]; then
    echo "Usage: $0 <branch-name> [-f|--force]"
    echo ""
    echo "Examples:"
    echo "  $0 BT-99-feature        # Remove worktree for BT-99-feature"
    echo "  $0 BT-99 --force        # Force remove even with uncommitted changes"
    exit 1
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
GRAY='\033[0;90m'
NC='\033[0m'

log_info() { echo -e "${CYAN}$1${NC}"; }
log_success() { echo -e "${GREEN}$1${NC}"; }
log_warn() { echo -e "${YELLOW}$1${NC}"; }
log_error() { echo -e "${RED}$1${NC}"; }
log_gray() { echo -e "${GRAY}$1${NC}"; }

# Find the main repo root
get_main_repo_root() {
    local git_path=".git"
    
    if [ -d "$git_path" ]; then
        pwd
    elif [ -f "$git_path" ]; then
        local gitdir
        gitdir=$(grep "gitdir:" "$git_path" | sed 's/gitdir: //')
        dirname "$(dirname "$(dirname "$gitdir")")"
    else
        git rev-parse --show-toplevel 2>/dev/null
    fi
}

# Get worktree path for a branch
get_worktree_path() {
    local branch="$1"
    git worktree list --porcelain 2>/dev/null | awk -v branch="$branch" '
        /^worktree / { wt = substr($0, 10) }
        /^branch refs\/heads\// { 
            b = substr($0, 20)
            if (b == branch) print wt
        }
    '
}

# Main script
log_info "🛑 Stopping worktree for branch: $BRANCH"

# Find main repo
MAIN_REPO=$(get_main_repo_root)
if [ -z "$MAIN_REPO" ]; then
    log_error "Error: Could not find git repository root"
    exit 1
fi
log_gray "📁 Main repo: $MAIN_REPO"

# Work from main repo
pushd "$MAIN_REPO" > /dev/null

# Find the worktree path
WORKTREE_PATH=$(get_worktree_path "$BRANCH")

if [ -z "$WORKTREE_PATH" ]; then
    log_warn "⚠️  No worktree found for branch: $BRANCH"
    log_gray "   Checking for orphaned worktree metadata..."
    
    # Check for orphaned metadata
    WORKTREE_META_PATH=".git/worktrees/$BRANCH"
    if [ -d "$WORKTREE_META_PATH" ]; then
        log_warn "🧹 Found orphaned metadata, cleaning up..."
        rm -rf "$WORKTREE_META_PATH"
        log_success "✅ Cleaned up orphaned worktree metadata"
    else
        log_error "❌ No worktree or metadata found for: $BRANCH"
    fi
    popd > /dev/null
    exit 0
fi

log_gray "📂 Worktree path: $WORKTREE_PATH"

# Check if the .git file in the worktree needs fixing
GIT_FILE="$WORKTREE_PATH/.git"
if [ -f "$GIT_FILE" ]; then
    GIT_CONTENT=$(cat "$GIT_FILE")
    
    # Check if it points to container path
    if echo "$GIT_CONTENT" | grep -q "/workspaces/"; then
        log_warn "🔧 Fixing container path in .git file..."
        
        # Extract worktree name
        WORKTREE_NAME=$(echo "$GIT_CONTENT" | grep -oP 'worktrees/\K[^/\s]+')
        if [ -n "$WORKTREE_NAME" ]; then
            CORRECT_PATH="$MAIN_REPO/.git/worktrees/$WORKTREE_NAME"
            echo "gitdir: $CORRECT_PATH" > "$GIT_FILE"
            log_success "✅ Fixed .git to point to: $CORRECT_PATH"
        fi
    fi
elif [ ! -e "$GIT_FILE" ]; then
    log_warn "⚠️  No .git file found in worktree (may be corrupted)"
fi

# Try standard worktree remove first
log_info "🗑️  Removing worktree..."
if git worktree remove $FORCE "$WORKTREE_PATH" 2>/dev/null; then
    log_success "✅ Worktree removed successfully"
else
    log_warn "⚠️  git worktree remove failed"
    log_info "🔧 Attempting manual cleanup..."
    
    # Manual cleanup
    WORKTREE_META_PATH=".git/worktrees/$BRANCH"
    WORKTREE_DIR_NAME=$(basename "$WORKTREE_PATH")
    WORKTREE_META_PATH_ALT=".git/worktrees/$WORKTREE_DIR_NAME"
    
    # Remove metadata
    for META_PATH in "$WORKTREE_META_PATH" "$WORKTREE_META_PATH_ALT"; do
        if [ -d "$META_PATH" ]; then
            log_gray "   Removing: $META_PATH"
            rm -rf "$META_PATH"
        fi
    done
    
    # Remove worktree directory
    if [ -d "$WORKTREE_PATH" ]; then
        log_gray "   Removing: $WORKTREE_PATH"
        rm -rf "$WORKTREE_PATH"
    fi
    
    # Prune stale entries
    git worktree prune
    
    log_success "✅ Manual cleanup complete"
fi

# Ask about deleting the branch
echo ""
read -p "Delete local branch '$BRANCH'? (y/N) " DELETE_BRANCH
if [ "$DELETE_BRANCH" = "y" ] || [ "$DELETE_BRANCH" = "Y" ]; then
    if git branch -d "$BRANCH" 2>/dev/null; then
        log_success "✅ Branch deleted"
    else
        read -p "Branch not fully merged. Force delete? (y/N) " FORCE_DELETE
        if [ "$FORCE_DELETE" = "y" ] || [ "$FORCE_DELETE" = "Y" ]; then
            git branch -D "$BRANCH"
            log_success "✅ Branch force deleted"
        fi
    fi
fi

popd > /dev/null

echo ""
log_success "✨ Done!"
