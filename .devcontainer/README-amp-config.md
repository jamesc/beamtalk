# Amp CLI Configuration Template

**File:** `amp-settings.json`

**Purpose:** Template configuration for Amp CLI in devcontainers.

## How It Works

1. This template file is deployed on container start by `scripts/worktree-up.ps1`
2. During setup, `envsubst` copies this template to `~/.config/amp/settings.json` inside the container (no environment variable substitution currently occurs)
3. Amp reads `~/.config/amp/settings.json` automatically on startup

## Authentication

Set `AMP_API_KEY` on your host machine. It is forwarded into the container via `remoteEnv` in `devcontainer.json`.

To get an API key, sign in at [ampcode.com/install](https://ampcode.com/install) and follow the CLI setup instructions.

## Configuration Options

| Option | Value | Description |
|--------|-------|-------------|
| `amp.permissions` | `[{ "tool": "*", "action": "allow" }]` | Auto-allow all tool uses (yolo mode) |
| `amp.updates.mode` | `"warn"` | Show update notifications without auto-updating inside the container |

## Amp in This Project

Amp automatically reads `AGENTS.md` (and `CLAUDE.md`) from the repository root and parent directories. The project's `AGENTS.md` provides Amp with build/test commands, architecture guidance, and codebase conventions.

## Modification

To change the default configuration:
1. Edit `amp-settings.json` in `.devcontainer/`
2. Rebuild the devcontainer (or re-run worktree-up)
3. The new config will be applied on next container start

## License

Copyright 2026 James Casey  
SPDX-License-Identifier: Apache-2.0
