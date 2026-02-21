#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Beamtalk-specific worktree setup hook.
# Called inside the devcontainer after startup to configure project-specific resources.
# Currently: clones the skills repo into ~/.copilot/skills/

set -euo pipefail

log() {
    echo "[worktree-up-hook] $*"
}

SKILLS_DIR="$HOME/.copilot/skills"
SKILLS_REPO="https://github.com/jamesc/skills.git"

if [ -d "$SKILLS_DIR/.git" ]; then
    log "Skills repo already present, pulling latest..."
    git -C "$SKILLS_DIR" pull --ff-only 2>/dev/null || log "Could not pull (offline or diverged), using existing"
else
    log "Cloning skills repo into $SKILLS_DIR..."
    mkdir -p "$SKILLS_DIR"
    git clone "$SKILLS_REPO" "$SKILLS_DIR"
    log "Skills repo cloned"
fi
