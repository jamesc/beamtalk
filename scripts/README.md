# Scripts

Helper scripts for Beamtalk development.

## `start-worktree.ps1` / `start-worktree.sh`

Start a Copilot devcontainer session for a git worktree branch. This enables running multiple parallel Copilot sessions, each in its own container working on a different branch.

### Usage

**Windows (PowerShell):**
```powershell
.\scripts\start-worktree.ps1 BT-99-feature

# Create new branch from main
.\scripts\start-worktree.ps1 -Branch BT-99 -BaseBranch main
```

**Linux/Mac:**
```bash
./scripts/start-worktree.sh BT-99-feature

# Create new branch from main
./scripts/start-worktree.sh BT-99 main
```

### What it does

1. **Checks if worktree exists** for the branch
2. **Creates worktree** if needed (new branch from base, or existing branch)
3. **Sets up environment** (`BEAMTALK_MAIN_GIT_PATH` for git access)
4. **Starts the devcontainer** using `devcontainer up`
5. **Connects via bash** using `devcontainer exec`

You'll get a shell inside the container where you can run Copilot CLI or other tools.

### Prerequisites

- Git with worktree support
- VS Code with Dev Containers extension
- `devcontainer` CLI (auto-installed if missing): `npm install -g @devcontainers/cli`

### First-time setup

Set the `BEAMTALK_MAIN_GIT_PATH` environment variable permanently:

**Windows:**
```powershell
setx BEAMTALK_MAIN_GIT_PATH "C:\Users\you\source\beamtalk\.git"
```

**Linux/Mac:**
```bash
echo 'export BEAMTALK_MAIN_GIT_PATH="$HOME/source/beamtalk/.git"' >> ~/.bashrc
```

Then add the worktree mount to `.devcontainer/devcontainer.json` (see comments in that file).
