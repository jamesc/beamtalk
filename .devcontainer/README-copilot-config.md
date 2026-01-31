# Copilot CLI Configuration Template

**File:** `copilot-config.json`

**Purpose:** Template configuration for GitHub Copilot CLI in devcontainers.

## How It Works

1. This template file contains the placeholder `${containerWorkspaceFolder}`
2. During `postStartCommand`, `envsubst` replaces the placeholder with the actual container path
3. The result is written to `~/.copilot/config.json` inside the container

## Configuration Options

| Option | Value | Description |
|--------|-------|-------------|
| `banner` | `"never"` | Disable animated banner for cleaner automated sessions |
| `render_markdown` | `true` | Display formatted markdown in responses |
| `theme` | `"auto"` | UI theme (auto-detect based on terminal) |
| `model` | `"claude-sonnet-4.5"` | Default AI model |
| `trusted_folders` | `["${containerWorkspaceFolder}"]` | Auto-trust the workspace folder |
| `allowed_urls` | Array | Whitelisted URLs for documentation access |
| `experimental` | `true` | Enable experimental Copilot features |
| `log_level` | `"info"` | Logging verbosity |
| `default_permissions` | Object | Default permission prompts for tools/paths/urls |

## Modification

To change the default configuration:
1. Edit `copilot-config.json` in `.devcontainer/`
2. Rebuild the devcontainer
3. The new config will be applied on container start

## License

Copyright 2026 James Casey  
SPDX-License-Identifier: Apache-2.0
