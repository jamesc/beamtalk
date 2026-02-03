# Scripts

Helper scripts for development.

## Worktree Scripts

| Script | Description |
|--------|-------------|
| `worktree-new.ps1` | Create a worktree and start a devcontainer |
| `worktree-rm.ps1` | Remove a worktree and clean up containers |
| `worktree-cleanup.ps1` | Remove orphaned containers from deleted worktrees |

## Other Scripts

| Script | Location | Description |
|--------|----------|-------------|
| `compile.sh` | `tests/fixtures/` | Compile Beamtalk test fixtures for runtime tests |

---

## Port and Node Name Auto-Assignment

Start a Copilot devcontainer session for a git worktree branch. This enables running multiple parallel Copilot sessions, each in its own container working on a different branch.

### Usage

```powershell
.\scripts\worktree-new.ps1 BT-99-feature

# Create new branch from main
.\scripts\worktree-new.ps1 -Branch BT-99 -BaseBranch main
```

### What it does

1. **Checks if worktree exists** for the branch
2. **Creates worktree** if needed (new branch from base, or existing branch)
3. **Starts the devcontainer** using `devcontainer up`
4. **Connects via bash** using `devcontainer exec`

You'll get a shell inside the container where you can run Copilot CLI or other tools.

### Prerequisites

- Git with worktree support
- VS Code with Dev Containers extension
- `devcontainer` CLI (auto-installed if missing): `npm install -g @devcontainers/cli`

---

## `worktree-rm.ps1`

Stop and remove a git worktree, handling container path fixups automatically.

### Usage

```powershell
.\scripts\worktree-rm.ps1 BT-99-feature

# Force remove even with uncommitted changes
.\scripts\worktree-rm.ps1 -Branch BT-99 -Force
```

### What it does

1. **Finds the worktree** for the given branch
2. **Fixes the .git file** if it was modified for container paths (points to `/workspaces/...`)
3. **Removes the worktree** using `git worktree remove`
4. **Falls back to manual cleanup** if standard removal fails
5. **Optionally deletes the branch** (prompts you)

### Why this is needed

When a worktree is used inside a devcontainer, the `.git` file gets modified to point to container paths (`/workspaces/.beamtalk-git/...`). This breaks `git worktree remove` on the host system. The stop-worktree script fixes the path before removal.

---

## `worktree-cleanup.ps1`

Remove containers from worktrees that were deleted without using `worktree-rm.ps1`.

```powershell
.\scripts\worktree-cleanup.ps1        # Interactive
.\scripts\worktree-cleanup.ps1 -DryRun # Preview only
.\scripts\worktree-cleanup.ps1 -NoConfirm # Auto-confirm
.\scripts\worktree-cleanup.ps1 -All   # Remove ALL project containers
```

Shows orphaned containers with their worktree names and lets you confirm before removal.

---

## `worktree-new.ps1`

Each worktree automatically gets a unique REPL port and Erlang node name to avoid conflicts when running multiple parallel sessions.

### Port derivation

- `BT-190` branch → port `49342` (49152 + issue number, capped at 999)
- `BT-64` branch → port `49216`
- `main` branch → port `49152` (default)
- Other branches → port `50152-51051` (hash-based, non-overlapping with BT range)

The base port 49152 is the start of the IANA ephemeral port range, avoiding conflicts with common services.

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
