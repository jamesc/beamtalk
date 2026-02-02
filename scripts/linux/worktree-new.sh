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

# Create .env file for devcontainer with port and node name derived from branch
log_info "⚙️  Creating .env file for devcontainer..."
ENV_PATH="$WORKTREE_PATH/.env"

# Derive port from branch name
# BT-190 -> 9190, BT-64 -> 9064, main -> 9000
PORT=9000
NODE_NAME="beamtalk@localhost"

if [[ "$BRANCH" =~ ^BT-([0-9]+) ]]; then
    ISSUE_NUM="${BASH_REMATCH[1]}"
    PORT=$((9000 + ISSUE_NUM))
    NODE_NAME="beamtalk_bt${ISSUE_NUM}@localhost"
    log_gray "   Port: $PORT (derived from $BRANCH)"
    log_gray "   Node: $NODE_NAME"
elif [ "$BRANCH" = "main" ]; then
    log_gray "   Port: $PORT (main branch default)"
    log_gray "   Node: $NODE_NAME (main branch default)"
else
    # Hash branch name to port range 9100-9999
    HASH=$(echo -n "$BRANCH" | md5sum | cut -d' ' -f1 | tr -d '[:alpha:]')
    PORT=$((9100 + (HASH % 900)))
    NODE_NAME="beamtalk_$(echo "$BRANCH" | tr -c '[:alnum:]' '_')@localhost"
    log_gray "   Port: $PORT (derived from branch hash)"
    log_gray "   Node: $NODE_NAME"
fi

# Write .env file
cat > "$ENV_PATH" << EOF
# Auto-generated by worktree-new.sh
# Configuration for Beamtalk REPL in this worktree
BEAMTALK_REPL_PORT=$PORT
BEAMTALK_NODE_NAME=$NODE_NAME
EOF

log_success "✅ Created .env file"


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
log_success "✨ Container ready!"

# Copy SSH signing key if configured
if [ -n "$GIT_SIGNING_KEY" ]; then
    SSH_KEY_PATH="$HOME/.ssh/$GIT_SIGNING_KEY"
    if [ -f "$SSH_KEY_PATH" ]; then
        log_info "🔑 Copying SSH signing key..."
        
        # Get container ID for this workspace
        CONTAINER_ID=$(docker ps --filter "label=devcontainer.local_folder=$WORKTREE_PATH" --format "{{.ID}}" 2>/dev/null)
        if [ -n "$CONTAINER_ID" ]; then
            if docker cp "$SSH_KEY_PATH" "$CONTAINER_ID:/home/vscode/.ssh/$GIT_SIGNING_KEY" 2>/dev/null; then
                log_success "✅ SSH key copied, re-running setup..."
                devcontainer exec --workspace-folder "$WORKTREE_PATH" bash .devcontainer/setup-ssh-signing.sh
            else
                log_warn "⚠️  Could not copy SSH key (container may not be ready)"
            fi
        fi
    else
        log_warn "⚠️  SSH key not found at: $SSH_KEY_PATH"
    fi
fi

echo ""
log_info "Starting Copilot..."

# Connect to the container and start Copilot in yolo mode with claude-sonnet-4.5
devcontainer exec --workspace-folder "$WORKTREE_PATH" copilot --yolo --model claude-sonnet-4.5
