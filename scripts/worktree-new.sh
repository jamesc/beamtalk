#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Start a Copilot devcontainer session for a git worktree branch.
#
# Usage:
#   ./start-worktree.sh <branch-name> [base-branch]
#
# Examples:
#   ./start-worktree.sh BT-99-feature
#   ./start-worktree.sh BT-99 main

set -e

BRANCH="${1:-}"
BASE_BRANCH="${2:-main}"

if [ -z "$BRANCH" ]; then
    echo "Usage: $0 <branch-name> [base-branch]"
    echo ""
    echo "Examples:"
    echo "  $0 BT-99-feature        # Create/open worktree for BT-99-feature"
    echo "  $0 BT-99 main           # Create new branch BT-99 from main"
    exit 1
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
GRAY='\033[0;90m'
NC='\033[0m' # No Color

log_info() { echo -e "${CYAN}$1${NC}"; }
log_success() { echo -e "${GREEN}$1${NC}"; }
log_warn() { echo -e "${YELLOW}$1${NC}"; }
log_gray() { echo -e "${GRAY}$1${NC}"; }

# Find the main repo root
get_main_repo_root() {
    local git_path=".git"
    
    if [ -d "$git_path" ]; then
        # We're in the main repo
        pwd
    elif [ -f "$git_path" ]; then
        # We're in a worktree
        local gitdir
        gitdir=$(grep "gitdir:" "$git_path" | sed 's/gitdir: //')
        # gitdir points to .git/worktrees/name, go up to repo root
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

# Check if branch exists
branch_exists() {
    local branch="$1"
    git branch --list "$branch" 2>/dev/null | grep -q . && return 0
    git branch -r --list "origin/$branch" 2>/dev/null | grep -q . && return 0
    return 1
}

# Main script
log_info "🚀 Starting worktree session for branch: $BRANCH"

# Find main repo
MAIN_REPO=$(get_main_repo_root)
if [ -z "$MAIN_REPO" ]; then
    echo -e "${RED}Error: Could not find git repository root${NC}"
    exit 1
fi
log_gray "📁 Main repo: $MAIN_REPO"

# Worktree root is parent of main repo
WORKTREE_ROOT=$(dirname "$MAIN_REPO")

# Check if we're already on this branch
CURRENT_BRANCH=$(git branch --show-current 2>/dev/null || echo "")
CURRENT_DIR=$(pwd)

# Always fetch latest from origin first
log_info "🔄 Fetching latest from origin..."
pushd "$MAIN_REPO" > /dev/null
git fetch origin --prune 2>/dev/null || true
popd > /dev/null

if [ "$CURRENT_BRANCH" = "$BRANCH" ]; then
    log_success "✅ Already on branch $BRANCH in current directory"
    WORKTREE_PATH="$CURRENT_DIR"
else
    # Work from main repo to manage worktrees
    pushd "$MAIN_REPO" > /dev/null
    
    EXISTING_WORKTREE=$(get_worktree_path "$BRANCH")
    
    if [ -n "$EXISTING_WORKTREE" ]; then
        log_success "✅ Worktree already exists at: $EXISTING_WORKTREE"
        WORKTREE_PATH="$EXISTING_WORKTREE"
    else
        # Sanitize branch name for directory (replace / with -)
        DIR_NAME=$(echo "$BRANCH" | tr '/' '-')
        WORKTREE_PATH="$WORKTREE_ROOT/$DIR_NAME"
        
        if branch_exists "$BRANCH"; then
            log_warn "📌 Creating worktree for existing branch: $BRANCH"
            git worktree add "$WORKTREE_PATH" "$BRANCH"
        else
            log_warn "🌱 Creating worktree with new branch: $BRANCH (from $BASE_BRANCH)"
            git worktree add -b "$BRANCH" "$WORKTREE_PATH" "$BASE_BRANCH"
        fi
        
        log_success "✅ Worktree created at: $WORKTREE_PATH"
    fi
    
    popd > /dev/null
fi

# Update worktree with latest changes from remote
log_info "🔄 Updating worktree with latest changes..."
pushd "$WORKTREE_PATH" > /dev/null
if git rev-parse --abbrev-ref "@{upstream}" &>/dev/null; then
    if git pull --ff-only 2>/dev/null; then
        log_success "✅ Worktree updated"
    else
        log_warn "⚠️  Could not fast-forward, may need manual merge"
    fi
else
    log_gray "ℹ️  No upstream tracking branch, skipping pull"
fi
popd > /dev/null

# Check for devcontainer CLI
if ! command -v devcontainer &> /dev/null; then
    log_warn "⚠️  devcontainer CLI not found. Installing..."
    npm install -g @devcontainers/cli
fi

# Check BEAMTALK_MAIN_GIT_PATH is set
if [ -z "$BEAMTALK_MAIN_GIT_PATH" ]; then
    MAIN_GIT_PATH="$MAIN_REPO/.git"
    log_warn "⚠️  BEAMTALK_MAIN_GIT_PATH not set. Setting for this session..."
    export BEAMTALK_MAIN_GIT_PATH="$MAIN_GIT_PATH"
    log_gray "   Add to ~/.bashrc: export BEAMTALK_MAIN_GIT_PATH=\"$MAIN_GIT_PATH\""
fi

# Start devcontainer
echo ""
log_info "🐳 Starting devcontainer..."
log_gray "   Workspace: $WORKTREE_PATH"

# Build and start the container
devcontainer up --workspace-folder "$WORKTREE_PATH"

if [ $? -ne 0 ]; then
    echo -e "${RED}❌ Failed to start devcontainer${NC}"
    exit 1
fi

echo ""
log_success "✨ Container ready! Connecting..."

# Connect to the container
devcontainer exec --workspace-folder "$WORKTREE_PATH" bash
