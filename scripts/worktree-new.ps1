# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

<#
.SYNOPSIS
    Start a Copilot devcontainer session for a git worktree branch.

.DESCRIPTION
    Creates a worktree for the given branch (if needed) and starts VS Code
    in a devcontainer. Each worktree gets its own container for parallel
    Copilot sessions.

.PARAMETER Branch
    The branch name to work on (e.g., "BT-99-feature" or "main")

.PARAMETER BaseBranch
    The base branch to create new branches from (default: "main")

.PARAMETER WorktreeRoot
    Directory where worktrees are created (default: parent of main repo)

.EXAMPLE
    .\start-worktree.ps1 BT-99-feature
    
.EXAMPLE
    .\start-worktree.ps1 -Branch BT-99 -BaseBranch main
#>

param(
    [Parameter(Mandatory=$true, Position=0)]
    [string]$Branch,
    
    [Parameter(Mandatory=$false)]
    [string]$BaseBranch = "main",
    
    [Parameter(Mandatory=$false)]
    [string]$WorktreeRoot = ""
)

$ErrorActionPreference = "Stop"

# Find the main repo root (where .git is a directory, not a file)
function Get-MainRepoRoot {
    $current = Get-Location
    
    # Check if we're in a worktree (has .git file) or main repo (has .git dir)
    $gitPath = Join-Path $current ".git"
    
    if (Test-Path $gitPath -PathType Container) {
        # We're in the main repo
        return $current.Path
    }
    elseif (Test-Path $gitPath -PathType Leaf) {
        # We're in a worktree, read the .git file to find main repo
        $gitContent = Get-Content $gitPath -Raw
        if ($gitContent -match "gitdir:\s*(.+)") {
            $gitDir = $matches[1].Trim()
            # gitDir points to .git/worktrees/name, go up to .git then to repo
            $mainGit = Split-Path (Split-Path $gitDir -Parent) -Parent
            return Split-Path $mainGit -Parent
        }
    }
    
    # Try to find it via git
    $gitRoot = git rev-parse --show-toplevel 2>$null
    if ($gitRoot) {
        return $gitRoot
    }
    
    throw "Could not find git repository root"
}

# Get current branch name
function Get-CurrentBranch {
    return (git branch --show-current 2>$null)
}

# Check if branch exists (local or remote)
function Test-BranchExists {
    param([string]$BranchName)
    
    $local = git branch --list $BranchName 2>$null
    if ($local) { return $true }
    
    $remote = git branch -r --list "origin/$BranchName" 2>$null
    if ($remote) { return $true }
    
    return $false
}

# Check if worktree exists for branch
function Get-WorktreePath {
    param([string]$BranchName)
    
    $worktrees = git worktree list --porcelain 2>$null
    foreach ($line in $worktrees) {
        if ($line -match "^worktree\s+(.+)") {
            $wtPath = $matches[1]
        }
        if ($line -match "^branch\s+refs/heads/(.+)" -and $matches[1] -eq $BranchName) {
            return $wtPath
        }
    }
    return $null
}

# Main script
Write-Host "🚀 Starting worktree session for branch: $Branch" -ForegroundColor Cyan

# Find main repo
$mainRepo = Get-MainRepoRoot
Write-Host "📁 Main repo: $mainRepo" -ForegroundColor Gray

# Set worktree root if not specified
if (-not $WorktreeRoot) {
    $WorktreeRoot = Split-Path $mainRepo -Parent
}

# Check if we're already on this branch in current directory
$currentBranch = Get-CurrentBranch
$currentDir = Get-Location

# Always fetch latest from origin first
Write-Host "🔄 Fetching latest from origin..." -ForegroundColor Cyan
Push-Location $mainRepo
try {
    git fetch origin --prune 2>$null
}
finally {
    Pop-Location
}

if ($currentBranch -eq $Branch) {
    Write-Host "✅ Already on branch $Branch in current directory" -ForegroundColor Green
    $worktreePath = $currentDir.Path
}
else {
    # Check if worktree already exists
    Push-Location $mainRepo
    try {
        $existingWorktree = Get-WorktreePath -BranchName $Branch
        
        if ($existingWorktree) {
            Write-Host "✅ Worktree already exists at: $existingWorktree" -ForegroundColor Green
            $worktreePath = $existingWorktree
        }
        else {
            # Create new worktree
            # Sanitize branch name for directory (replace / with -)
            $dirName = $Branch -replace '/', '-'
            $worktreePath = Join-Path $WorktreeRoot $dirName
            
            if (Test-BranchExists -BranchName $Branch) {
                Write-Host "📌 Creating worktree for existing branch: $Branch" -ForegroundColor Yellow
                git worktree add $worktreePath $Branch
            }
            else {
                Write-Host "🌱 Creating worktree with new branch: $Branch (from $BaseBranch)" -ForegroundColor Yellow
                git worktree add -b $Branch $worktreePath $BaseBranch
            }
            
            Write-Host "✅ Worktree created at: $worktreePath" -ForegroundColor Green
        }
    }
    finally {
        Pop-Location
    }
}

# Update worktree with latest changes from remote
Write-Host "🔄 Updating worktree with latest changes..." -ForegroundColor Cyan
Push-Location $worktreePath
try {
    $trackingBranch = git rev-parse --abbrev-ref "@{upstream}" 2>$null
    if ($trackingBranch) {
        git pull --ff-only 2>$null
        if ($LASTEXITCODE -eq 0) {
            Write-Host "✅ Worktree updated" -ForegroundColor Green
        }
        else {
            Write-Host "⚠️  Could not fast-forward, may need manual merge" -ForegroundColor Yellow
        }
    }
    else {
        Write-Host "ℹ️  No upstream tracking branch, skipping pull" -ForegroundColor Gray
    }
}
finally {
    Pop-Location
}

# Check for devcontainer CLI
$devcontainerCli = Get-Command devcontainer -ErrorAction SilentlyContinue
if (-not $devcontainerCli) {
    Write-Host "⚠️  devcontainer CLI not found. Installing..." -ForegroundColor Yellow
    npm install -g @devcontainers/cli
}

# Check BEAMTALK_MAIN_GIT_PATH is set
if (-not $env:BEAMTALK_MAIN_GIT_PATH) {
    $mainGitPath = Join-Path $mainRepo ".git"
    Write-Host "⚠️  BEAMTALK_MAIN_GIT_PATH not set. Setting for this session..." -ForegroundColor Yellow
    $env:BEAMTALK_MAIN_GIT_PATH = $mainGitPath
    Write-Host "   Set permanently with: setx BEAMTALK_MAIN_GIT_PATH `"$mainGitPath`"" -ForegroundColor Gray
}

# Start devcontainer
Write-Host "`n🐳 Starting devcontainer..." -ForegroundColor Cyan
Write-Host "   Workspace: $worktreePath" -ForegroundColor Gray

# Build and start the container - capture output to get container ID
Write-Host "Running: devcontainer up --workspace-folder $worktreePath" -ForegroundColor Gray
$output = devcontainer up --workspace-folder $worktreePath 2>&1 | ForEach-Object {
    Write-Host $_  # Display output in real-time
    $_  # Pass through to capture
}

if ($LASTEXITCODE -ne 0) {
    Write-Host "`n❌ Failed to start devcontainer" -ForegroundColor Red
    exit 1
}

# Extract container ID from output JSON (last line)
$containerIdFromOutput = $null
try {
    $lastLine = ($output | Select-Object -Last 1) -replace '\x1b\[[0-9;]*m', ''  # Strip ANSI codes
    $jsonOutput = $lastLine | ConvertFrom-Json -ErrorAction SilentlyContinue
    if ($jsonOutput.containerId) {
        $containerIdFromOutput = $jsonOutput.containerId
    }
} catch {
    # Ignore JSON parse errors
}

Write-Host "`n✨ Container ready!" -ForegroundColor Green

# Copy SSH signing key if configured
if ($env:GIT_SIGNING_KEY) {
    $sshKeyPath = Join-Path $env:USERPROFILE ".ssh\$env:GIT_SIGNING_KEY"
    if (Test-Path $sshKeyPath) {
        Write-Host "🔑 Copying SSH signing key..." -ForegroundColor Cyan
        
        # Use container ID from devcontainer up output
        $containerInfo = $containerIdFromOutput
        
        if ($containerInfo) {
            Write-Host "   Container ID: $containerInfo" -ForegroundColor Gray
            
            # Ensure .ssh directory exists in container with correct ownership
            docker exec $containerInfo mkdir -p /home/vscode/.ssh 2>$null
            docker exec $containerInfo chown -R vscode:vscode /home/vscode/.ssh 2>$null
            docker exec $containerInfo chmod 700 /home/vscode/.ssh 2>$null
            
            # Copy the key
            Write-Host "   Copying: $sshKeyPath" -ForegroundColor Gray
            Write-Host "   To: ${containerInfo}:/home/vscode/.ssh/$env:GIT_SIGNING_KEY" -ForegroundColor Gray
            
            $copyResult = docker cp $sshKeyPath "${containerInfo}:/home/vscode/.ssh/$env:GIT_SIGNING_KEY" 2>&1
            
            if ($LASTEXITCODE -eq 0) {
                # Fix ownership of the copied key
                docker exec $containerInfo chown vscode:vscode /home/vscode/.ssh/$env:GIT_SIGNING_KEY 2>$null
                docker exec $containerInfo chmod 644 /home/vscode/.ssh/$env:GIT_SIGNING_KEY 2>$null
                
                Write-Host "✅ SSH key copied, re-running setup..." -ForegroundColor Green
                devcontainer exec --workspace-folder $worktreePath bash .devcontainer/setup-ssh-signing.sh
            }
            else {
                Write-Host "⚠️  Could not copy SSH key to container" -ForegroundColor Yellow
                Write-Host "   Error: $copyResult" -ForegroundColor Red
            }
        }
        else {
            Write-Host "⚠️  Could not find running container" -ForegroundColor Yellow
        }
    }
    else {
        Write-Host "⚠️  SSH key not found at: $sshKeyPath" -ForegroundColor Yellow
    }
}

Write-Host "`nStarting Copilot..." -ForegroundColor Cyan

# Connect to the container and start Copilot in yolo mode with claude-sonnet-4.5
devcontainer exec --workspace-folder $worktreePath copilot --yolo --model claude-sonnet-4.5
