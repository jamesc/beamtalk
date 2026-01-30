#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Fix git worktree paths for devcontainer use
# This script rewrites the .git file in worktrees to point to the mounted location

set -e

WORKSPACE="${1:-$PWD}"
GIT_FILE="$WORKSPACE/.git"

# Check if .git is a file (worktree) or directory (main repo)
if [ -f "$GIT_FILE" ]; then
    echo "Detected git worktree, fixing path..."
    
    # Extract worktree name from the path
    ORIGINAL_PATH=$(cat "$GIT_FILE" | sed 's/gitdir: //')
    WORKTREE_NAME=$(basename "$ORIGINAL_PATH")
    
    # Check if main .git is mounted
    if [ -d "/workspaces/.beamtalk-git" ]; then
        NEW_PATH="/workspaces/.beamtalk-git/worktrees/$WORKTREE_NAME"
        
        if [ -d "$NEW_PATH" ]; then
            echo "gitdir: $NEW_PATH" > "$GIT_FILE"
            echo "Fixed .git to point to: $NEW_PATH"
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
