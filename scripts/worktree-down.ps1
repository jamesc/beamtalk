# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

<#
.SYNOPSIS
    Stop and remove a git worktree and its devcontainer.

.DESCRIPTION
    Removes a worktree that may have been used with devcontainers.
    
    This script:
    - Stops and removes the devcontainer for the worktree
    - Fixes any container path issues in .git file
    - Removes the worktree directory
    - Optionally deletes the branch
    
    Note: Path normalization is applied to handle git's forward slashes
    vs Docker's backslashes on Windows.

.PARAMETER Branch
    The branch name of the worktree to remove (e.g., "feature-branch")

.PARAMETER Force
    Force removal even if there are uncommitted changes

.EXAMPLE
    .\worktree-down.ps1 feature-branch
    
.EXAMPLE
    .\worktree-down.ps1 -Branch issue-123 -Force
#>

param(
    [Parameter(Position=0)]
    [string]$Branch,
    
    [Parameter(Mandatory=$false)]
    [switch]$Force
)

$ErrorActionPreference = "Stop"

# Show usage if no branch specified
if (-not $Branch) {
    Write-Host "Usage: worktree-down.ps1 <branch-name> [-Force]" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Examples:" -ForegroundColor Gray
    Write-Host "  .\scripts\worktree-down.ps1 feature-branch"
    Write-Host "  .\scripts\worktree-down.ps1 feature-branch -Force"
    exit 0
}

# Find the main repo root (where .git is a directory, not a file)
function Get-MainRepoRoot {
    $current = Get-Location
    
    $gitPath = Join-Path $current ".git"
    
    if (Test-Path $gitPath -PathType Container) {
        return $current.Path
    }
    elseif (Test-Path $gitPath -PathType Leaf) {
        $gitContent = Get-Content $gitPath -Raw
        if ($gitContent -match "gitdir:\s*(.+)") {
            $gitDir = $matches[1].Trim()
            $mainGit = Split-Path (Split-Path $gitDir -Parent) -Parent
            return Split-Path $mainGit -Parent
        }
    }
    
    $gitRoot = git rev-parse --show-toplevel 2>$null
    if ($gitRoot) {
        return $gitRoot
    }
    
    throw "Could not find git repository root"
}

# Get worktree path for a branch
function Get-WorktreePath {
    param(
        [string]$BranchName,
        [string]$WorktreeRoot
    )
    
    # Sanitize branch name to match directory name (same as worktree-up.ps1)
    $dirName = $BranchName -replace '/', '-'
    $expectedPath = Join-Path $WorktreeRoot $dirName
    
    # Check if this directory exists as a worktree
    $worktrees = git worktree list --porcelain 2>$null
    $wtPath = $null
    foreach ($line in $worktrees) {
        if ($line -match "^worktree\s+(.+)") {
            $wtPath = $matches[1]
            
            # Convert container path to host path if needed
            if ($wtPath -match "^/workspaces/(.+)$") {
                $folderName = $matches[1]
                $wtPath = Join-Path $WorktreeRoot $folderName
            }
            
            # Match by directory name, not branch name
            if ($wtPath -eq $expectedPath) {
                return $wtPath
            }
        }
    }
    return $null
}

# Stop and remove devcontainer for a worktree
function Remove-DevContainer {
    param([string]$WorktreePath)
    
    $folderName = Split-Path $WorktreePath -Leaf
    
    # Find containers by our custom git.worktree label first (most reliable)
    # Fall back to matching folder name in devcontainer.local_folder label
    $allContainers = docker ps -a --format '{{.ID}}' 2>$null
    $containers = @()
    foreach ($id in $allContainers) {
        if (-not $id) { continue }
        
        # Check our custom label first
        $worktreeLabel = docker inspect --format '{{index .Config.Labels "git.worktree"}}' $id 2>$null
        if ($worktreeLabel -eq $folderName) {
            $containerName = docker inspect --format '{{.Name}}' $id 2>$null
            $containers += "$id`t$containerName"
            continue
        }
        
        # Fallback to devcontainer.local_folder label
        $labelPath = docker inspect --format '{{index .Config.Labels "devcontainer.local_folder"}}' $id 2>$null
        if ($labelPath) {
            $labelFolderName = Split-Path $labelPath -Leaf
            if ($labelFolderName -eq $folderName) {
                $containerName = docker inspect --format '{{.Name}}' $id 2>$null
                $containers += "$id`t$containerName"
            }
        }
    }
    
    if ($containers) {
        Write-Host "🐳 Found devcontainer(s) for $folderName" -ForegroundColor Cyan
        foreach ($container in $containers) {
            $parts = $container -split "\t"
            $containerId = $parts[0]
            $containerName = $parts[1]
            
            Write-Host "   Stopping: $containerName" -ForegroundColor Gray
            docker stop $containerId 2>$null | Out-Null
            
            Write-Host "   Removing: $containerName" -ForegroundColor Gray
            docker rm $containerId 2>$null | Out-Null
        }
        Write-Host "✅ Devcontainer(s) removed" -ForegroundColor Green
    }
    else {
        Write-Host "ℹ️  No devcontainer found for $folderName" -ForegroundColor Gray
    }
}

# Main script
Write-Host "🛑 Stopping worktree for branch: $Branch" -ForegroundColor Cyan

# Find main repo
$mainRepo = Get-MainRepoRoot
Write-Host "📁 Main repo: $mainRepo" -ForegroundColor Gray

# Worktrees are in .worktrees/ subdirectory
$worktreeRoot = Join-Path $mainRepo ".worktrees"

# Work from main repo
Push-Location $mainRepo
try {
    # Find the worktree path
    $worktreePath = Get-WorktreePath -BranchName $Branch -WorktreeRoot $worktreeRoot
    
    if (-not $worktreePath) {
        Write-Host "⚠️  No worktree found for branch: $Branch" -ForegroundColor Yellow
        Write-Host "   Checking for orphaned worktree metadata..." -ForegroundColor Gray
        
        # Check if there's orphaned metadata
        $worktreeMetaPath = Join-Path ".git" "worktrees" $Branch
        if (Test-Path $worktreeMetaPath) {
            Write-Host "🧹 Found orphaned metadata, cleaning up..." -ForegroundColor Yellow
            Remove-Item -Recurse -Force $worktreeMetaPath
            Write-Host "✅ Cleaned up orphaned worktree metadata" -ForegroundColor Green
        }
        else {
            Write-Host "❌ No worktree or metadata found for: $Branch" -ForegroundColor Red
        }
        exit 0
    }
    
    Write-Host "📂 Worktree path: $worktreePath" -ForegroundColor Gray
    
    # Show what branch is actually checked out (informational)
    Push-Location $worktreePath
    try {
        $actualBranch = git branch --show-current 2>$null
        if ($actualBranch -and $actualBranch -ne $Branch) {
            Write-Host "   Note: Currently on branch '$actualBranch' (worktree was created as '$Branch')" -ForegroundColor Gray
        }
    }
    finally {
        Pop-Location
    }
    
    # Stop devcontainer FIRST (before removing worktree to release file locks)
    Remove-DevContainer -WorktreePath $worktreePath
    
    # Run project-specific hook if it exists (for custom cleanup like volumes)
    $hookScript = Join-Path $worktreePath ".devcontainer\worktree-down-hook.ps1"
    if (Test-Path $hookScript) {
        Write-Host "🔧 Running project cleanup hook..." -ForegroundColor Cyan
        try {
            & $hookScript -WorktreePath $worktreePath -Branch $Branch -MainRepo $mainRepo
            if ($LASTEXITCODE -eq 0) {
                Write-Host "✅ Project cleanup hook completed" -ForegroundColor Green
            }
            else {
                Write-Host "⚠️  Project cleanup hook returned non-zero exit code" -ForegroundColor Yellow
            }
        }
        catch {
            Write-Host "⚠️  Project cleanup hook failed: $_" -ForegroundColor Yellow
        }
    }
    
    # Check if the .git file in the worktree needs fixing
    $gitFile = Join-Path $worktreePath ".git"
    if (Test-Path $gitFile -PathType Leaf) {
        $gitContent = Get-Content $gitFile -Raw
        
        # Check if it points to container path
        if ($gitContent -match "/workspaces/") {
            Write-Host "🔧 Fixing container path in .git file..." -ForegroundColor Yellow
            
            # Find the worktree name from the path
            if ($gitContent -match "worktrees/([^/\r\n]+)") {
                $worktreeName = $matches[1]
                $correctPath = Join-Path $mainRepo ".git" "worktrees" $worktreeName
                $correctPath = $correctPath -replace "\\", "/"
                
                Set-Content -Path $gitFile -Value "gitdir: $correctPath" -NoNewline
                Write-Host "✅ Fixed .git to point to: $correctPath" -ForegroundColor Green
            }
        }
    }
    elseif (-not (Test-Path $gitFile)) {
        Write-Host "⚠️  No .git file found in worktree (may be corrupted)" -ForegroundColor Yellow
    }
    
    # Try standard worktree remove first
    $removeArgs = @("worktree", "remove", $worktreePath)
    if ($Force) {
        $removeArgs += "--force"
    }
    
    Write-Host "🗑️  Removing worktree..." -ForegroundColor Cyan
    $result = & git @removeArgs 2>&1
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Worktree removed successfully" -ForegroundColor Green
    }
    elseif ($result -match "modified or untracked files") {
        Write-Host "❌ Worktree has uncommitted changes" -ForegroundColor Red
        Write-Host "   Use -Force to delete anyway, or commit/stash your changes first." -ForegroundColor Yellow
        exit 1
    }
    else {
        Write-Host "⚠️  git worktree remove failed: $result" -ForegroundColor Yellow
        Write-Host "🔧 Attempting manual cleanup..." -ForegroundColor Cyan
        
        # Manual cleanup
        $worktreeMetaPath = Join-Path ".git" "worktrees" $Branch
        
        # Also try with the worktree directory name if different from branch
        $worktreeDirName = Split-Path $worktreePath -Leaf
        $worktreeMetaPathAlt = Join-Path ".git" "worktrees" $worktreeDirName
        
        # Remove metadata
        foreach ($metaPath in @($worktreeMetaPath, $worktreeMetaPathAlt)) {
            if (Test-Path $metaPath) {
                Write-Host "   Removing: $metaPath" -ForegroundColor Gray
                Remove-Item -Recurse -Force $metaPath
            }
        }
        
        # Remove worktree directory
        if (Test-Path $worktreePath) {
            Write-Host "   Removing: $worktreePath" -ForegroundColor Gray
            Remove-Item -Recurse -Force $worktreePath
        }
        
        # Prune stale entries
        git worktree prune
        
        Write-Host "✅ Manual cleanup complete" -ForegroundColor Green
    }
    
    # Optionally delete the branch
    Write-Host ""
    $deleteBranch = Read-Host "Delete local branch '$Branch'? (y/N)"
    if ($deleteBranch -eq 'y' -or $deleteBranch -eq 'Y') {
        # Update main branch to check against latest remote state
        Write-Host "🔄 Updating main branch..." -ForegroundColor Cyan
        git fetch origin main:main 2>$null
        if ($LASTEXITCODE -ne 0) {
            # Fallback if fast-forward fails
            git fetch origin main 2>$null
        }
        
        git branch -d $Branch 2>$null
        if ($LASTEXITCODE -ne 0) {
            Write-Host "⚠️  Branch not fully merged. Force delete? (y/N)" -ForegroundColor Yellow
            $forceDelete = Read-Host
            if ($forceDelete -eq 'y' -or $forceDelete -eq 'Y') {
                git branch -D $Branch
            }
        }
        if ($LASTEXITCODE -eq 0) {
            Write-Host "✅ Branch deleted" -ForegroundColor Green
        }
    }
}
finally {
    Pop-Location
}

Write-Host "`n✨ Done!" -ForegroundColor Green
