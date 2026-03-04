# Amp CLI Configuration Template

**File:** `amp-settings.json`

**Purpose:** Template configuration for Amp CLI in devcontainers.

## How It Works

1. This repository provides `amp-settings.json` in `.devcontainer/` as a template for your Amp CLI config.
2. Amp reads `~/.config/amp/settings.json` automatically on startup (if the file exists).
3. Currently, there is **no** `postStartCommand` or setup script wired up to copy this template for you — you must create the config manually inside the container, for example:
   ```bash
   mkdir -p ~/.config/amp
   cp .devcontainer/amp-settings.json ~/.config/amp/settings.json
   ```

The `~/.config/amp/` directory is persisted via a Docker volume (`beamtalk-amp-state`), so this only needs to be done once.

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
2. Re-copy it into the container: `cp .devcontainer/amp-settings.json ~/.config/amp/settings.json`

## License

Copyright 2026 James Casey  
SPDX-License-Identifier: Apache-2.0
