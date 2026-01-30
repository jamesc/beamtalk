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
# When GH_TOKEN is set, gh CLI uses it automatically - no login needed
if [ -n "$GH_TOKEN" ]; then
    # Clear any existing GitHub credential helpers to avoid duplicates
    git config --global --unset-all credential.https://github.com.helper 2>/dev/null || true
    
    # Configure git to use gh as credential helper
    gh auth setup-git 2>/dev/null
    log "Configured git to use gh for GitHub authentication"
else
    log "WARNING: GH_TOKEN not set, GitHub push will require manual authentication"
    log "Set GH_TOKEN environment variable on your host machine"
fi

# Trust the workspace directory
git config --global safe.directory '*'
