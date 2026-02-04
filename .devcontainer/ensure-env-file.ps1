#!/usr/bin/env pwsh
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Ensure .env file exists for devcontainer --env-file requirement
# This script is called by devcontainer initializeCommand

$ErrorActionPreference = "Stop"

try {
    $envFile = Join-Path $PSScriptRoot ".." ".env"

    if (-not (Test-Path $envFile)) {
        Write-Host "Creating .env file for devcontainer..."
        @"
# Environment variables for devcontainer
# This file is auto-created by .devcontainer/ensure-env-file.ps1
# The worktree-up.ps1 script populates this with additional variables

"@ | Out-File -FilePath $envFile -Encoding utf8NoBOM
        Write-Host "Created: $envFile"
    } else {
        Write-Host ".env file already exists"
    }
    
    exit 0
} catch {
    Write-Error "Failed to ensure .env file: $_"
    exit 1
}
