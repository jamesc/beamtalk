# Scripts

Helper scripts for Beamtalk development.

## `worktree-new.ps1` / `linux/worktree-new.sh`

Start a Copilot devcontainer session for a git worktree branch. This enables running multiple parallel Copilot sessions, each in its own container working on a different branch.

### Usage

**Windows (PowerShell):**
```powershell
.\scripts\worktree-new.ps1 BT-99-feature

# Create new branch from main
.\scripts\worktree-new.ps1 -Branch BT-99 -BaseBranch main
```

**Linux/Mac:**
```bash
./scripts/linux/worktree-new.sh BT-99-feature

# Create new branch from main
./scripts/linux/worktree-new.sh BT-99 main
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

---

## `worktree-rm.ps1` / `linux/worktree-rm.sh`

Stop and remove a git worktree, handling container path fixups automatically.

### Usage

**Windows (PowerShell):**
```powershell
.\scripts\worktree-rm.ps1 BT-99-feature

# Force remove even with uncommitted changes
.\scripts\worktree-rm.ps1 -Branch BT-99 -Force
```

**Linux/Mac:**
```bash
./scripts/linux/worktree-rm.sh BT-99-feature

# Force remove
./scripts/linux/worktree-rm.sh BT-99 --force
```

### What it does

1. **Finds the worktree** for the given branch
2. **Fixes the .git file** if it was modified for container paths (points to `/workspaces/...`)
3. **Removes the worktree** using `git worktree remove`
4. **Falls back to manual cleanup** if standard removal fails
5. **Optionally deletes the branch** (prompts you)

### Why this is needed

When a worktree is used inside a devcontainer, the `.git` file gets modified to point to container paths (`/workspaces/.beamtalk-git/...`). This breaks `git worktree remove` on the host system. The stop-worktree script fixes the path before removal.
