#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Fix git worktree paths for devcontainer use
# This script rewrites the .git file in worktrees to point to the mounted location
#
# The problem: When a git worktree is created on Windows, the .git file contains
# a Windows path like: gitdir: C:/Users/james/source/beamtalk/.git/worktrees/BT-99
# Inside the devcontainer, this path doesn't exist. Instead, we mount the main
# repo's .git directory at /workspaces/.beamtalk-git, so we need to rewrite
# the path to: gitdir: /workspaces/.beamtalk-git/worktrees/BT-99

# Don't use set -e - we want to handle errors gracefully
WORKSPACE="${1:-$PWD}"
GIT_FILE="$WORKSPACE/.git"
MAX_RETRIES=3
RETRY_DELAY=1

log() {
    echo "[fix-worktree-git] $*"
}

log "Checking $GIT_FILE"

# Trust all directories first to avoid "dubious ownership" errors
git config --global safe.directory '*' 2>/dev/null
log "Configured git safe.directory"

# Check if .git is a file (worktree) or directory (main repo)
if [ -f "$GIT_FILE" ]; then
    # Read current content
    ORIGINAL_CONTENT=$(cat "$GIT_FILE")
    log "Current .git content: $ORIGINAL_CONTENT"
    
    # Check if it's already pointing to the container path
    if echo "$ORIGINAL_CONTENT" | grep -q "^gitdir: /workspaces/.beamtalk-git"; then
        log "Already fixed, skipping"
        exit 0
    fi
    
    log "Detected git worktree with host path, fixing..."
    
    # Extract the path (remove 'gitdir: ' prefix and any trailing whitespace/carriage returns)
    ORIGINAL_PATH=$(echo "$ORIGINAL_CONTENT" | sed 's/^gitdir:[[:space:]]*//' | tr -d '\r\n')
    log "Original path: $ORIGINAL_PATH"
    
    # Extract worktree name - handle both Unix (/) and Windows (\) path separators
    # First normalize backslashes to forward slashes, then get last component
    NORMALIZED_PATH=$(echo "$ORIGINAL_PATH" | tr '\\' '/')
    WORKTREE_NAME="${NORMALIZED_PATH##*/}"
    log "Worktree name: $WORKTREE_NAME"
    
    # Validate we got a worktree name
    if [ -z "$WORKTREE_NAME" ]; then
        log "ERROR: Could not extract worktree name from path"
        exit 1
    fi
    
    # Wait for mount to be ready (may not be available immediately on container start)
    RETRY=0
    while [ ! -d "/workspaces/.beamtalk-git" ] && [ $RETRY -lt $MAX_RETRIES ]; do
        log "Waiting for /workspaces/.beamtalk-git mount... (attempt $((RETRY+1))/$MAX_RETRIES)"
        sleep $RETRY_DELAY
        RETRY=$((RETRY+1))
    done
    
    # Check if main .git is mounted
    if [ -d "/workspaces/.beamtalk-git" ]; then
        NEW_PATH="/workspaces/.beamtalk-git/worktrees/$WORKTREE_NAME"
        log "Expected worktree path: $NEW_PATH"
        
        if [ -d "$NEW_PATH" ]; then
            echo "gitdir: $NEW_PATH" > "$GIT_FILE"
            log "SUCCESS: Fixed .git to point to: $NEW_PATH"
            
            # Also fix the gitdir file inside the worktree metadata
            # This file points back to the worktree directory
            GITDIR_FILE="$NEW_PATH/gitdir"
            if [ -f "$GITDIR_FILE" ]; then
                CONTAINER_WORKSPACE="/workspaces/$WORKTREE_NAME"
                echo "$CONTAINER_WORKSPACE" > "$GITDIR_FILE"
                log "Fixed gitdir to point to: $CONTAINER_WORKSPACE"
            fi
            
            # Verify git works
            if git status >/dev/null 2>&1; then
                log "Verified: git commands work"
            else
                log "WARNING: git commands still failing after fix"
            fi
        else
            log "ERROR: Worktree directory not found at $NEW_PATH"
            log "Available worktrees:"
            ls -1 /workspaces/.beamtalk-git/worktrees/ 2>/dev/null | sed 's/^/  - /' || echo "  (none)"
            
            # Try to find a matching worktree by partial name
            for wt in /workspaces/.beamtalk-git/worktrees/*/; do
                wt_name=$(basename "$wt")
                if [ "$wt_name" != "*" ]; then
                    log "  Found: $wt_name"
                fi
            done
            exit 1
        fi
    else
        log "ERROR: Main .git not mounted at /workspaces/.beamtalk-git"
        log "The worktree script should have mounted this automatically."
        log "Check that the devcontainer was started via worktree-new.ps1"
        exit 1
    fi
elif [ -d "$GIT_FILE" ]; then
    log "Main repo detected (not a worktree), no fix needed"
else
    log "WARNING: No .git found at $GIT_FILE"
fi
