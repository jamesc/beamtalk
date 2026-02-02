# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

<#
.SYNOPSIS
    Clean up orphaned beamtalk devcontainers and volumes.

.DESCRIPTION
    Finds and removes Docker containers that were created for worktrees
    that no longer exist. Optionally can remove all beamtalk containers.

.PARAMETER All
    Remove all beamtalk containers, even for active worktrees

.PARAMETER DryRun
    Show what would be removed without actually removing anything

.EXAMPLE
    .\cleanup-orphaned-containers.ps1
    
.EXAMPLE
    .\cleanup-orphaned-containers.ps1 -All
    
.EXAMPLE
    .\cleanup-orphaned-containers.ps1 -DryRun
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

Write-Host "🐳 Beamtalk Container Cleanup" -ForegroundColor Cyan
Write-Host ""

# Get list of active worktree paths
$activeWorktrees = @()
if (-not $All) {
    Write-Host "📂 Finding active worktrees..." -ForegroundColor Cyan
    $worktrees = git worktree list --porcelain 2>$null
    $currentPath = $null
    foreach ($line in $worktrees) {
        if ($line -match "^worktree\s+(.+)") {
            $currentPath = $matches[1]
            $activeWorktrees += $currentPath
            Write-Host "   ✓ $currentPath" -ForegroundColor Gray
        }
    }
    Write-Host ""
}

# Find all beamtalk-related containers
Write-Host "🔍 Scanning Docker containers..." -ForegroundColor Cyan
$allContainers = docker ps -a --format "{{.ID}}|{{.Names}}|{{.Image}}|{{.Status}}" 2>$null

$beamtalkContainers = @()
$orphanedContainers = @()

foreach ($line in $allContainers) {
    if (-not $line) { continue }
    
    $parts = $line -split "\|"
    $id = $parts[0]
    $name = $parts[1]
    $image = $parts[2]
    $status = $parts[3]
    
    # Check if this is a beamtalk container (by image name)
    if ($image -match "beamtalk|vsc-bt-") {
        $container = @{
            Id = $id
            Name = $name
            Image = $image
            Status = $status
        }
        $beamtalkContainers += $container
        
        # Check if it's orphaned (if not running --all mode)
        if (-not $All) {
            # Try to get the local_folder label
            $labelJson = docker inspect --format '{{index .Config.Labels "devcontainer.local_folder"}}' $id 2>$null
            
            $isOrphaned = $true
            
            # Safety: Never consider a running container as orphaned unless -All is specified
            # Running containers are actively being used
            if ($status -match "^Up") {
                $isOrphaned = $false
            }
            elseif ($labelJson) {
                foreach ($worktree in $activeWorktrees) {
                    # Extract just the branch/worktree name for comparison
                    # This handles cross-platform paths where:
                    # - Host sees: C:\Users\james\source\worktrees\BT-137 or /workspaces/BT-137
                    # - Container label: /workspaces/BT-137
                    $worktreeName = Split-Path -Leaf $worktree
                    $labelName = Split-Path -Leaf $labelJson
                    
                    # Also check if worktree path appears anywhere in label (for main repo)
                    $normalizedWorktree = $worktree -replace "\\", "/"
                    $normalizedLabel = $labelJson -replace "\\", "/"
                    
                    if ($worktreeName -eq $labelName -or $normalizedLabel -match [regex]::Escape($normalizedWorktree)) {
                        $isOrphaned = $false
                        break
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
$containersToRemove = if ($All) { $beamtalkContainers } else { $orphanedContainers }

Write-Host "Found:" -ForegroundColor White
Write-Host "   Total beamtalk containers: $($beamtalkContainers.Count)" -ForegroundColor Gray
if (-not $All) {
    Write-Host "   Active worktree containers: $($beamtalkContainers.Count - $orphanedContainers.Count)" -ForegroundColor Green
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
    Write-Host " $($container.Name)" -ForegroundColor White
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

# Find and remove orphaned volumes
Write-Host ""
Write-Host "🔍 Scanning for orphaned volumes..." -ForegroundColor Cyan
$allVolumes = docker volume ls --format "{{.Name}}" 2>$null

$beamtalkVolumes = @()
foreach ($volume in $allVolumes) {
    if ($volume -match "bt-\d+|beamtalk") {
        $beamtalkVolumes += $volume
    }
}

if ($beamtalkVolumes.Count -gt 0) {
    Write-Host "Found $($beamtalkVolumes.Count) beamtalk-related volumes" -ForegroundColor Gray
    
    $confirmVolumes = if ($NoConfirm) { "y" } else { Read-Host "Remove unused volumes? (y/N)" }
    if ($confirmVolumes -eq 'y' -or $confirmVolumes -eq 'Y') {
        foreach ($volume in $beamtalkVolumes) {
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

Write-Host ""
Write-Host "✨ Done!" -ForegroundColor Green
