#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Beamtalk-specific worktree setup hook.
# Called inside the devcontainer after startup to configure project-specific resources.
# Currently: clones the skills repo into ~/.copilot/skills/ and ~/.config/agents/skills/

set -euo pipefail

log() {
    echo "[worktree-up-hook] $*"
}

SKILLS_DIR="$HOME/.copilot/skills"
SKILLS_REPO="https://github.com/jamesc/skills.git"

clone_or_update_skills() {
    local dir="$1"
    local repo="$2"
    if [ -d "$dir/.git" ]; then
        log "Skills repo already present at $dir, pulling latest..."
        git -C "$dir" pull --ff-only 2>/dev/null || log "Could not pull (offline or diverged), using existing"
    else
        if [ -d "$dir" ] && [ ! -d "$dir/.git" ] && [ "$(ls -A "$dir" 2>/dev/null)" ]; then
            log "Skills dir $dir exists but is not a git repo; moving aside"
            if ! mv "$dir" "${dir}.bak.$(date +%s)"; then
                log "Could not move $dir; skipping clone"
                return 0
            fi
        fi
        log "Cloning skills repo into $dir..."
        mkdir -p "$(dirname "$dir")"
        if git clone "$repo" "$dir"; then
            log "Skills repo cloned into $dir"
        else
            log "Could not clone skills repo (offline?), skipping $dir"
        fi
    fi
}

clone_or_update_skills "$SKILLS_DIR" "$SKILLS_REPO"
clone_or_update_skills "$HOME/.config/agents/skills" "$SKILLS_REPO"
