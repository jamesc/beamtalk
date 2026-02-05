# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

<#
.SYNOPSIS
    Clean up orphaned devcontainers and volumes for this repository's worktrees.

.DESCRIPTION
    Finds and removes Docker containers and volumes that were created for worktrees
    that no longer exist. Volumes in use are safely skipped.

.PARAMETER All
    Remove all containers for this project, even for active worktrees

.PARAMETER DryRun
    Show what would be removed without actually removing anything

.EXAMPLE
    .\worktree-cleanup.ps1
    
.EXAMPLE
    .\worktree-cleanup.ps1 -All
    
.EXAMPLE
    .\worktree-cleanup.ps1 -DryRun
#>

param(
    [Parameter(Mandatory=$false)]
    [switch]$All,
    
    [Parameter(Mandatory=$false)]
    [switch]$DryRun,
    
    [Parameter(Mandatory=$false)]
    [switch]$NoConfirm
)

$ErrorActionPreference = "Stop"

# Get project name from git repo
$repoRoot = git rev-parse --show-toplevel 2>$null
if (-not $repoRoot) {
    Write-Host "❌ Not in a git repository" -ForegroundColor Red
    exit 1
}
$projectName = Split-Path -Leaf $repoRoot

Write-Host "🐳 Container Cleanup for: $projectName" -ForegroundColor Cyan
Write-Host ""

# Get list of active worktree paths
$activeWorktrees = @()
$worktreeParentDir = $null
if (-not $All) {
    Write-Host "📂 Finding active worktrees..." -ForegroundColor Cyan
    $worktrees = git worktree list --porcelain 2>$null
    $currentPath = $null
    foreach ($line in $worktrees) {
        if ($line -match "^worktree\s+(.+)") {
            $currentPath = $matches[1]
            $activeWorktrees += $currentPath
            Write-Host "   ✓ $currentPath" -ForegroundColor Gray
            # Derive the parent directory
            if (-not $worktreeParentDir) {
                $worktreeParentDir = (Split-Path -Parent $currentPath) -replace "\\", "/"
            }
        }
    }
    Write-Host ""
}

# Find all project-related containers
Write-Host "🔍 Scanning Docker containers..." -ForegroundColor Cyan
$allContainers = docker ps -a --format "{{.ID}}|{{.Names}}|{{.Image}}|{{.Status}}" 2>$null

$projectContainers = @()
$orphanedContainers = @()

foreach ($line in $allContainers) {
    if (-not $line) { continue }
    
    $parts = $line -split "\|"
    $id = $parts[0]
    $name = $parts[1]
    $image = $parts[2]
    $status = $parts[3]
    
    # Check if this is a container for THIS project's worktrees
    # Match by: image name containing project, git.worktree label, or local_folder in our worktree parent directory
    $worktreeLabel = docker inspect --format '{{index .Config.Labels "git.worktree"}}' $id 2>$null
    $localFolder = docker inspect --format '{{index .Config.Labels "devcontainer.local_folder"}}' $id 2>$null
    $normalizedLocalFolder = if ($localFolder) { $localFolder -replace "\\", "/" } else { "" }
    
    # Only consider containers that belong to THIS project's worktree directory
    # Must match BOTH a project identifier AND be in our worktree directory structure
    $isThisProject = $false
    
    # Primary check: local_folder must be within this repo's directory structure
    $repoRootNormalized = $repoRoot -replace "\\", "/"
    $isInRepoDir = $normalizedLocalFolder -and $normalizedLocalFolder -match [regex]::Escape($repoRootNormalized)
    
    if ($isInRepoDir) {
        # Container's local_folder is within this repo - definitely ours
        $isThisProject = $true
    } elseif (($image -match [regex]::Escape($projectName) -or $image -match "vsc-$projectName") -and -not $normalizedLocalFolder) {
        # Image matches project name and no local_folder to check - assume ours
        # (legacy containers without labels)
        $isThisProject = $true
    }
    
    if ($isThisProject) {
        # Get worktree name from label or local_folder
        $worktreeName = if ($worktreeLabel) { 
            $worktreeLabel 
        } elseif ($localFolder) { 
            Split-Path -Leaf $localFolder 
        } else { 
            "unknown" 
        }
        
        $container = @{
            Id = $id
            Name = $name
            Image = $image
            Status = $status
            WorktreeLabel = $worktreeLabel
            WorktreeName = $worktreeName
        }
        $projectContainers += $container
        
        # Check if it's orphaned (if not running --all mode)
        if (-not $All) {
            $isOrphaned = $true
            
            # Check by our custom git.worktree label first (most reliable)
            if ($worktreeLabel) {
                foreach ($worktree in $activeWorktrees) {
                    $worktreeName = Split-Path -Leaf $worktree
                    if ($worktreeName -eq $worktreeLabel) {
                        $isOrphaned = $false
                        break
                    }
                }
            }
            # Fallback to devcontainer.local_folder label for containers created before label was added
            else {
                $labelJson = docker inspect --format '{{index .Config.Labels "devcontainer.local_folder"}}' $id 2>$null
                if ($labelJson) {
                    foreach ($worktree in $activeWorktrees) {
                        $worktreeName = Split-Path -Leaf $worktree
                        $labelName = Split-Path -Leaf $labelJson
                        $normalizedWorktree = $worktree -replace "\\", "/"
                        $normalizedLabel = $labelJson -replace "\\", "/"
                        
                        if ($worktreeName -eq $labelName -or $normalizedLabel -match [regex]::Escape($normalizedWorktree)) {
                            $isOrphaned = $false
                            break
                        }
                    }
                }
            }
            
            if ($isOrphaned) {
                $orphanedContainers += $container
            }
        }
    }
}

# Determine which containers to remove
$containersToRemove = if ($All) { $projectContainers } else { $orphanedContainers }

Write-Host "Found:" -ForegroundColor White
Write-Host "   Total project containers: $($projectContainers.Count)" -ForegroundColor Gray
if (-not $All) {
    Write-Host "   Active worktree containers: $($projectContainers.Count - $orphanedContainers.Count)" -ForegroundColor Green
    Write-Host "   Orphaned containers: $($orphanedContainers.Count)" -ForegroundColor Yellow
}
Write-Host ""

if ($containersToRemove.Count -eq 0) {
    Write-Host "✨ No containers to remove!" -ForegroundColor Green
    exit 0
}

# Display what will be removed
Write-Host "Will remove the following containers:" -ForegroundColor Yellow
foreach ($container in $containersToRemove) {
    $statusColor = if ($container.Status -match "Up") { "Red" } else { "Gray" }
    Write-Host "   [$($container.Status)]" -ForegroundColor $statusColor -NoNewline
    Write-Host " $($container.Name)" -ForegroundColor White -NoNewline
    Write-Host " ($($container.WorktreeName))" -ForegroundColor DarkGray
}
Write-Host ""

if ($DryRun) {
    Write-Host "🔍 DRY RUN - No changes made" -ForegroundColor Cyan
    exit 0
}

# Confirm before removing
if (-not $NoConfirm) {
    $confirm = Read-Host "Remove these containers? (y/N)"
    if ($confirm -ne 'y' -and $confirm -ne 'Y') {
        Write-Host "❌ Cancelled" -ForegroundColor Red
        exit 0
    }
}

# Remove containers
Write-Host ""
Write-Host "🗑️  Removing containers..." -ForegroundColor Cyan
$removed = 0
foreach ($container in $containersToRemove) {
    Write-Host "   Stopping: $($container.Name)" -ForegroundColor Gray
    docker stop $container.Id 2>$null | Out-Null
    
    Write-Host "   Removing: $($container.Name)" -ForegroundColor Gray
    docker rm $container.Id 2>$null | Out-Null
    $removed++
}
Write-Host "✅ Removed $removed containers" -ForegroundColor Green

# Find and remove orphaned volumes matching project name
Write-Host ""
Write-Host "🔍 Scanning for orphaned volumes..." -ForegroundColor Cyan
$allVolumes = docker volume ls --format "{{.Name}}" 2>$null

$projectVolumes = @()
foreach ($volume in $allVolumes) {
    # Devcontainer volumes typically include project name
    if ($volume -match [regex]::Escape($projectName)) {
        $projectVolumes += $volume
    }
}

if ($projectVolumes.Count -gt 0) {
    Write-Host "Found $($projectVolumes.Count) project-related volumes" -ForegroundColor Gray
    
    $confirmVolumes = if ($NoConfirm) { "y" } else { Read-Host "Remove unused volumes? (y/N)" }
    if ($confirmVolumes -eq 'y' -or $confirmVolumes -eq 'Y') {
        foreach ($volume in $projectVolumes) {
            # Try to remove - will fail if still in use
            $result = docker volume rm $volume 2>&1
            if ($LASTEXITCODE -eq 0) {
                Write-Host "   ✓ Removed: $volume" -ForegroundColor Green
            }
            else {
                Write-Host "   ⚠️ Skipped (in use): $volume" -ForegroundColor Yellow
            }
        }
    }
}
else {
    Write-Host "No orphaned volumes found" -ForegroundColor Gray
}

Write-Host ""
Write-Host "✨ Done!" -ForegroundColor Green
