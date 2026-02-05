# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

<#
.SYNOPSIS
    Beamtalk-specific worktree cleanup hook.

.DESCRIPTION
    Called by worktree-down.ps1 to clean up per-worktree resources.
    Removes the Rust target cache volume for this worktree.

.PARAMETER WorktreePath
    Path to the worktree directory

.PARAMETER Branch
    Branch name

.PARAMETER MainRepo
    Path to the main repository
#>

param(
    [Parameter(Mandatory=$true)]
    [string]$WorktreePath,
    
    [Parameter(Mandatory=$true)]
    [string]$Branch,
    
    [Parameter(Mandatory=$false)]
    [string]$MainRepo
)

# Remove the target cache volume for this worktree
$folderName = Split-Path $WorktreePath -Leaf
$volumeName = "$folderName-target-cache"

Write-Host "   Checking for target cache volume: $volumeName" -ForegroundColor Gray

$volumeExists = docker volume ls --format "{{.Name}}" 2>$null | Where-Object { $_ -eq $volumeName }
if ($volumeExists) {
    Write-Host "   🗑️  Removing target cache volume..." -ForegroundColor Cyan
    docker volume rm $volumeName 2>$null | Out-Null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "   ✅ Volume removed: $volumeName" -ForegroundColor Green
    }
    else {
        Write-Host "   ⚠️  Could not remove volume (may be in use)" -ForegroundColor Yellow
    }
}
else {
    Write-Host "   ℹ️  No target cache volume found" -ForegroundColor Gray
}
