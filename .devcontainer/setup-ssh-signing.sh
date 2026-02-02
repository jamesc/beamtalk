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
    
    # IMPORTANT: SSH signing is OPTIONAL and requires manual setup
    # Docker containers cannot reliably access host .ssh without explicit mounts,
    # and we cannot require mounts since they break when env vars are unset.
    # 
    # Manual setup: Copy your public key into the container:
    #   Windows: docker cp C:\Users\you\.ssh\id_rsa.pub <container>:/home/vscode/.ssh/
    #   Linux:   docker cp ~/.ssh/id_rsa.pub <container>:/home/vscode/.ssh/
    #
    # This script will configure git if it finds the key already present.
    
    # Check if key is already present in container
    log "Checking if SSH key is already present in container..."
    HOST_KEY=""
    
    if [ -f "$HOME/.ssh/$GIT_SIGNING_KEY" ]; then
        log "✓ Found SSH public key at: $HOME/.ssh/$GIT_SIGNING_KEY"
        HOST_KEY="$HOME/.ssh/$GIT_SIGNING_KEY"
    else
        log "SSH public key not found in container at: $HOME/.ssh/$GIT_SIGNING_KEY"
        log ""
        log "To enable SSH commit signing:"
        log "  1. Copy your public key into the container:"
        log "     docker cp C:\\Users\\${HOST_USERNAME}\\.ssh\\${GIT_SIGNING_KEY} <container>:/home/vscode/.ssh/"
        log "  2. Rebuild container or manually run: bash .devcontainer/setup-ssh-signing.sh"
        log ""
    fi
    
    if [ -n "$HOST_KEY" ] && [ -f "$HOST_KEY" ]; then
        CONTAINER_KEY_PATH="$HOST_KEY"
        
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
    fi
else
    log "SSH commit signing not configured (GIT_SIGNING_KEY not set)"
fi
