# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

<#
.SYNOPSIS
    Show status of all git worktrees and their devcontainers.

.DESCRIPTION
    Lists all worktrees in the repository and shows which ones have
    running or stopped devcontainers.

.EXAMPLE
    .\worktree-status.ps1
#>

$ErrorActionPreference = "Stop"

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

# Get container status for a worktree
function Get-ContainerStatus {
    param([string]$WorktreeName)
    
    # Fetch all container info including labels in a single docker ps call
    # Format: ID|git.worktree label|devcontainer.local_folder label|Status
    $allContainers = docker ps -a --format '{{.ID}}|{{.Label "git.worktree"}}|{{.Label "devcontainer.local_folder"}}|{{.Status}}' 2>$null
    foreach ($line in $allContainers) {
        if (-not $line) { continue }
        
        $parts = $line -split '\|'
        $id = $parts[0]
        $worktreeLabel = $parts[1]
        $localFolder = $parts[2]
        $status = $parts[3]
        
        # Check our custom git.worktree label
        if ($worktreeLabel -eq $WorktreeName) {
            if ($status -match "^Up") {
                return "🟢 Running"
            } else {
                return "🔴 Stopped"
            }
        }
        
        # Fallback to devcontainer.local_folder label
        if ($localFolder) {
            $labelName = Split-Path -Leaf $localFolder
            if ($labelName -eq $WorktreeName) {
                if ($status -match "^Up") {
                    return "🟢 Running"
                } else {
                    return "🔴 Stopped"
                }
            }
        }
    }
    
    return "⚪ No container"
}

# Main script
$mainRepo = Get-MainRepoRoot
$projectName = Split-Path $mainRepo -Leaf

Write-Host "📂 Worktree Status for: $projectName" -ForegroundColor Cyan
Write-Host ""

# Get all worktrees
Push-Location $mainRepo
try {
    $worktrees = git worktree list --porcelain 2>$null
    
    $currentWorktree = $null
    $currentBranch = $null
    $results = @()
    
    foreach ($line in $worktrees) {
        if ($line -match "^worktree\s+(.+)") {
            # New worktree entry - save previous if exists
            if ($currentWorktree) {
                $worktreeName = Split-Path $currentWorktree -Leaf
                $normalizedWorktree = $currentWorktree -replace '/', '\'
                $normalizedMain = $mainRepo -replace '/', '\'
                $isMain = $normalizedWorktree -eq $normalizedMain
                
                $results += @{
                    Path = $currentWorktree
                    Name = $worktreeName
                    Branch = if ($currentBranch) { $currentBranch } else { "(detached)" }
                    IsMain = $isMain
                    Container = if ($isMain) { "-" } else { Get-ContainerStatus -WorktreeName $worktreeName }
                }
            }
            $currentWorktree = $matches[1]
            $currentBranch = $null
        }
        elseif ($line -match "^branch\s+refs/heads/(.+)") {
            $currentBranch = $matches[1]
        }
    }
    
    # Don't forget the last entry
    if ($currentWorktree) {
        $worktreeName = Split-Path $currentWorktree -Leaf
        $normalizedWorktree = $currentWorktree -replace '/', '\'
        $normalizedMain = $mainRepo -replace '/', '\'
        $isMain = $normalizedWorktree -eq $normalizedMain
        
        $results += @{
            Path = $currentWorktree
            Name = $worktreeName
            Branch = if ($currentBranch) { $currentBranch } else { "(detached)" }
            IsMain = $isMain
            Container = if ($isMain) { "-" } else { Get-ContainerStatus -WorktreeName $worktreeName }
        }
    }
    
    # Display results
    if ($results.Count -eq 0) {
        Write-Host "No worktrees found." -ForegroundColor Gray
    }
    else {
        # Header
        Write-Host ("{0,-20} {1,-20} {2}" -f "BRANCH", "WORKTREE", "CONTAINER") -ForegroundColor White
        Write-Host ("{0,-20} {1,-20} {2}" -f "------", "--------", "---------") -ForegroundColor DarkGray
        
        foreach ($wt in $results) {
            $branchDisplay = $wt.Branch
            $nameDisplay = if ($wt.IsMain) { "(main repo)" } else { $wt.Name }
            $containerDisplay = $wt.Container
            
            $branchColor = if ($wt.IsMain) { "DarkGray" } else { "Yellow" }
            
            Write-Host ("{0,-20}" -f $branchDisplay) -ForegroundColor $branchColor -NoNewline
            Write-Host (" {0,-20}" -f $nameDisplay) -ForegroundColor Gray -NoNewline
            Write-Host (" {0}" -f $containerDisplay)
        }
    }
    
    Write-Host ""
}
finally {
    Pop-Location
}
