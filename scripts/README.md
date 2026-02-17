# Scripts

Helper scripts for git worktree + devcontainer workflows.

Worktrees are created in `.worktrees/` subdirectory inside the repo (inspired by [Jesse Vincent's workflow](https://blog.fsck.com/2025/10/05/how-im-using-coding-agents-in-september-2025/)).

## Worktree Scripts

| Script | Description |
|--------|-------------|
| `worktree-up.ps1` | Create a worktree and start a devcontainer |
| `worktree-down.ps1` | Remove a worktree and clean up containers |
| `worktree-status.ps1` | Show status of all worktrees and containers |
| `worktree-cleanup.ps1` | Remove orphaned containers and project volumes |
| `smoke-test.sh` | Verify devcontainer is working (run inside container) |

## Other Scripts

| Script | Location | Description |
|--------|----------|-------------|
| `compile_fixtures.escript` | `runtime/apps/beamtalk_runtime/test_fixtures/` | Compile Beamtalk test fixtures for runtime tests (portable) |

---

## `worktree-up.ps1`

Start a Copilot devcontainer session for a git worktree branch. This enables running multiple parallel Copilot sessions, each in its own container working on a different branch.

### Usage

```powershell
.\scripts\worktree-up.ps1 BT-99-feature

# Create new branch from main
.\scripts\worktree-up.ps1 -Branch BT-123 -BaseBranch main

# Run bash instead of copilot
.\scripts\worktree-up.ps1 feature-branch -Command bash
```

### What it does

1. **Checks if worktree exists** for the branch
2. **Creates worktree** if needed (new branch from base, or existing branch)
3. **Starts the devcontainer** using `devcontainer up`
4. **Configures Copilot CLI** with config and MCP servers
5. **Launches Copilot** (or specified command) in the container

You'll get a shell inside the container where you can run Copilot CLI or other tools.

### Prerequisites

- Git with worktree support
- VS Code with Dev Containers extension
- `devcontainer` CLI (auto-installed if missing): `npm install -g @devcontainers/cli`
- `GH_TOKEN` environment variable set

---

## `worktree-down.ps1`

Stop and remove a git worktree, handling container path fixups automatically.

### Usage

```powershell
.\scripts\worktree-down.ps1 BT-99-feature

# Force remove even with uncommitted changes
.\scripts\worktree-down.ps1 -Branch BT-123 -Force
```

### What it does

1. **Finds the worktree** for the given branch
2. **Stops and removes the devcontainer**
3. **Fixes the .git file** if it was modified for container paths
4. **Removes the worktree** using `git worktree remove`
5. **Falls back to manual cleanup** if standard removal fails
6. **Optionally deletes the branch** (prompts you)

### Why this is needed

When a worktree is used inside a devcontainer, the `.git` file gets modified to point to container paths (`/workspaces/...`). This breaks `git worktree remove` on the host system. The script fixes the path before removal.

---

## `worktree-status.ps1`

Show status of all worktrees and their devcontainers.

```powershell
.\scripts\worktree-status.ps1
```

Displays a table showing each worktree's branch, folder name, and container status (🟢 Running, 🔴 Stopped, ⚪ No container).

---

## `worktree-cleanup.ps1`

Remove containers and project volumes from worktrees that were deleted without using `worktree-down.ps1`.

```powershell
.\scripts\worktree-cleanup.ps1        # Interactive
.\scripts\worktree-cleanup.ps1 -DryRun # Preview only
.\scripts\worktree-cleanup.ps1 -NoConfirm # Auto-confirm
.\scripts\worktree-cleanup.ps1 -All   # Remove ALL project containers
```

Shows orphaned containers with their worktree names and lets you confirm before removal. Volumes are matched by project name and skipped if in use.

---

## Port and Node Name Auto-Assignment

Each worktree automatically gets a unique Erlang node name and compiler daemon socket to avoid conflicts when running multiple parallel sessions.

### Port assignment

REPL ports are **OS-assigned** (ephemeral port 0) by default (BT-192). The OS picks an available port, eliminating all port conflict risk. The actual port is written to `~/.beamtalk/workspaces/<id>/port` and stored in `node.info` for reconnection.

To override: `beamtalk repl --port 9001` or set `BEAMTALK_REPL_PORT` env var.

### Node name derivation

- `BT-190` branch → `beamtalk_bt190@localhost`
- `BT-64` branch → `beamtalk_bt64@localhost`
- `main` branch → `beamtalk@localhost` (default)
- Other branches → `beamtalk_<sanitized_name>@localhost`

### Compiler daemon isolation

Each worktree also gets its own compiler daemon socket:

- `BT-190` branch → `~/.beamtalk/daemon-bt190.sock`
- `main` branch → `~/.beamtalk/daemon.sock` (default)
- Other branches → `~/.beamtalk/daemon-<sanitized_name>.sock`

For long or conflicting names, the script may truncate the sanitized name to keep the socket path unique and within OS path-length limits. This means each worktree can run its own compiler daemon without conflicts.

### Priority order (highest to lowest)

1. CLI flag: `beamtalk repl --port 9001 --node mynode@localhost`
2. Environment variable: `BEAMTALK_REPL_PORT`, `BEAMTALK_NODE_NAME`, `BEAMTALK_DAEMON_SOCKET`
3. Application config: `runtime/config/sys.config`
4. Default values

The worktree scripts automatically set these in a `.env` file, which is loaded by the devcontainer.

---

## Environment Variables

Set these on your Windows host before using the scripts:

| Variable | Required | Description |
|----------|----------|-------------|
| `GH_TOKEN` | Yes | GitHub auth token (`gh auth token`) |
| `GIT_USER_NAME` | No | Git commit author name |
| `GIT_USER_EMAIL` | No | Git commit author email |
| `GIT_SIGNING_KEY` | No | SSH key name for commit signing |

Example setup:

```powershell
$env:GH_TOKEN = (gh auth token)
$env:GIT_USER_NAME = "Your Name"
$env:GIT_USER_EMAIL = "your.email@example.com"
$env:GIT_SIGNING_KEY = "id_ed25519"
```

---

## Customization

### Adding Project-Specific Environment

Edit `devcontainer.json` to add environment variables via `containerEnv` or `remoteEnv`:

```json
"containerEnv": {
  "MY_VAR": "value"
},
"remoteEnv": {
  "MY_TOKEN": "${localEnv:MY_TOKEN}"
}
```

### Changing the Default Model

Edit `model` in `.devcontainer/copilot-config.json`:

```json
{
  "model": "gpt-4o"
}
```
