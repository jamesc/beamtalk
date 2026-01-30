#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Configure git authentication and user identity for devcontainer
# Uses GH_TOKEN for GitHub auth and GIT_USER_NAME/GIT_USER_EMAIL for identity

log() {
    echo "[setup-git-auth] $*"
}

# Configure git user identity
if [ -n "$GIT_USER_NAME" ]; then
    git config --global user.name "$GIT_USER_NAME"
    log "Set user.name to: $GIT_USER_NAME"
fi

if [ -n "$GIT_USER_EMAIL" ]; then
    git config --global user.email "$GIT_USER_EMAIL"
    log "Set user.email to: $GIT_USER_EMAIL"
fi

# Configure GitHub authentication via gh CLI
# Priority: 1) GH_TOKEN from host, 2) VS Code credential helper, 3) manual auth
if [ -n "$GH_TOKEN" ]; then
    log "Using GH_TOKEN from environment"
else
    # Try to get token from VS Code's credential helper (works in devcontainers)
    VSCODE_TOKEN=$(printf "protocol=https\nhost=github.com\n" | git credential fill 2>/dev/null | grep password | cut -d= -f2)
    if [ -n "$VSCODE_TOKEN" ]; then
        export GH_TOKEN="$VSCODE_TOKEN"
        export GITHUB_TOKEN="$VSCODE_TOKEN"
        log "Using token from VS Code credential helper"
    else
        log "WARNING: No GitHub token available"
        log "Either set GH_TOKEN on your host or authenticate VS Code with GitHub"
    fi
fi

if [ -n "$GH_TOKEN" ]; then
    # Clear any existing GitHub credential helpers to avoid duplicates
    git config --global --unset-all credential.https://github.com.helper 2>/dev/null || true
    
    # Configure git to use gh as credential helper
    gh auth setup-git 2>/dev/null
    log "Configured git to use gh for GitHub authentication"
fi

# Trust the workspace directory
git config --global safe.directory '*'
