#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Configure SSH commit signing by copying only the public key from host
# Uses GIT_SIGNING_KEY env var to specify which key file to use

log() {
    echo "[setup-ssh-signing] $*"
}

# Configure SSH commit signing if GIT_SIGNING_KEY is set
if [ -n "$GIT_SIGNING_KEY" ]; then
    log "SSH commit signing requested with key: $GIT_SIGNING_KEY"
    
    # Path to the public key on Windows host (accessible via /mnt/c or direct Windows path)
    # Try multiple possible mount points for Windows home directory
    HOST_KEY=""
    
    # Try /mnt/c/Users/... (WSL style)
    if [ -f "/mnt/c/Users/${USER}/.ssh/${GIT_SIGNING_KEY}" ]; then
        HOST_KEY="/mnt/c/Users/${USER}/.ssh/${GIT_SIGNING_KEY}"
    # Try with USERNAME env var
    elif [ -n "$USERNAME" ] && [ -f "/mnt/c/Users/${USERNAME}/.ssh/${GIT_SIGNING_KEY}" ]; then
        HOST_KEY="/mnt/c/Users/${USERNAME}/.ssh/${GIT_SIGNING_KEY}"
    # Try resolving from Windows path via cmd.exe if available
    elif command -v cmd.exe >/dev/null 2>&1; then
        WIN_HOME=$(cmd.exe /c "echo %USERPROFILE%" 2>/dev/null | tr -d '\r' | sed 's|\\|/|g' | sed 's|C:|/mnt/c|')
        if [ -f "${WIN_HOME}/.ssh/${GIT_SIGNING_KEY}" ]; then
            HOST_KEY="${WIN_HOME}/.ssh/${GIT_SIGNING_KEY}"
        fi
    fi
    
    if [ -n "$HOST_KEY" ] && [ -f "$HOST_KEY" ]; then
        log "Found SSH public key at: $HOST_KEY"
        
        # Create .ssh directory in container
        mkdir -p ~/.ssh
        chmod 700 ~/.ssh
        
        # Copy ONLY the public key to container
        cp "$HOST_KEY" ~/.ssh/
        chmod 644 ~/.ssh/"$GIT_SIGNING_KEY"
        
        CONTAINER_KEY_PATH="$HOME/.ssh/$GIT_SIGNING_KEY"
        log "Copied public key to: $CONTAINER_KEY_PATH"
        
        # Configure git to use SSH signing
        git config --global gpg.format ssh
        git config --global user.signingkey "$CONTAINER_KEY_PATH"
        git config --global commit.gpgsign true
        git config --global tag.gpgsign true
        
        log "Configured git to sign commits and tags with SSH key"
        
        # Create allowed_signers file for verification (optional)
        if [ -n "$GIT_USER_EMAIL" ]; then
            mkdir -p ~/.config/git
            echo "$GIT_USER_EMAIL $(cat $CONTAINER_KEY_PATH)" > ~/.config/git/allowed_signers
            git config --global gpg.ssh.allowedSignersFile ~/.config/git/allowed_signers
            log "Created allowed_signers file for signature verification"
        fi
    else
        log "WARNING: SSH public key not found"
        log "Expected location: ~/.ssh/$GIT_SIGNING_KEY on host"
        log "Make sure the file exists and is a public key (.pub)"
    fi
else
    log "SSH commit signing not configured (GIT_SIGNING_KEY not set)"
fi
