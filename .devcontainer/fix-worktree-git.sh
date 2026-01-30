#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Fix git worktree paths for devcontainer use
# This script rewrites the .git file in worktrees to point to the mounted location

# Don't use set -e - we want to handle errors gracefully
WORKSPACE="${1:-$PWD}"
GIT_FILE="$WORKSPACE/.git"

# Trust all directories to avoid "dubious ownership" errors in containers
git config --global safe.directory '*'

echo "fix-worktree-git.sh: Checking $GIT_FILE"

# Check if .git is a file (worktree) or directory (main repo)
if [ -f "$GIT_FILE" ]; then
    echo "Detected git worktree, fixing path..."
    
    # Read and display current content
    ORIGINAL_CONTENT=$(cat "$GIT_FILE")
    echo "Current .git content: $ORIGINAL_CONTENT"
    
    # Extract the path (remove 'gitdir: ' prefix and any trailing whitespace)
    ORIGINAL_PATH=$(echo "$ORIGINAL_CONTENT" | sed 's/^gitdir:[[:space:]]*//' | tr -d '\r\n')
    echo "Original path: $ORIGINAL_PATH"
    
    # Extract worktree name - handle both Unix and Windows paths
    # Use parameter expansion to get the last component
    WORKTREE_NAME="${ORIGINAL_PATH##*/}"
    echo "Worktree name: $WORKTREE_NAME"
    
    # Check if main .git is mounted
    if [ -d "/workspaces/.beamtalk-git" ]; then
        NEW_PATH="/workspaces/.beamtalk-git/worktrees/$WORKTREE_NAME"
        echo "Expected worktree path: $NEW_PATH"
        
        if [ -d "$NEW_PATH" ]; then
            echo "gitdir: $NEW_PATH" > "$GIT_FILE"
            echo "Fixed .git to point to: $NEW_PATH"
            echo "Verification - new content: $(cat "$GIT_FILE")"
        else
            echo "Warning: Worktree directory not found at $NEW_PATH"
            echo "Available worktrees:"
            ls -la /workspaces/.beamtalk-git/worktrees/ 2>/dev/null || echo "  (none)"
        fi
    else
        echo "Warning: Main .git not mounted at /workspaces/.beamtalk-git"
        echo "Set BEAMTALK_MAIN_GIT_PATH env var to your main repo's .git directory"
    fi
elif [ -d "$GIT_FILE" ]; then
    echo "Main repo detected, no fix needed"
else
    echo "Warning: No .git found at $GIT_FILE"
fi
