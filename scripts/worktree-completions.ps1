# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

<#
.SYNOPSIS
    Tab completion for worktree scripts.

.DESCRIPTION
    Source this file in your PowerShell profile to enable tab completion:
    
    . C:\Users\james\source\beamtalk\scripts\worktree-completions.ps1

.NOTES
    Provides completion for:
    - worktree-up.ps1 -Branch: all local and remote branches
    - worktree-down.ps1 -Branch: only branches with active worktrees
    - worktree-up.ps1 -Command: common commands (copilot, bash, etc.)
#>

$script:WorktreeScriptDir = $PSScriptRoot

# Completion for worktree-up.ps1 -Branch parameter
# Shows all local and remote branches
Register-ArgumentCompleter -CommandName "$script:WorktreeScriptDir\worktree-up.ps1" -ParameterName Branch -ScriptBlock {
    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
    
    $branches = @()
    
    # Get local branches
    $local = git branch --format='%(refname:short)' 2>$null
    if ($local) { $branches += $local }
    
    # Get remote branches (strip origin/ prefix)
    $remote = git branch -r --format='%(refname:short)' 2>$null | 
        Where-Object { $_ -notmatch '/HEAD$' } |
        ForEach-Object { $_ -replace '^origin/', '' }
    if ($remote) { $branches += $remote }
    
    # Dedupe and filter
    $branches | Sort-Object -Unique | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
    }
}

# Completion for worktree-down.ps1 -Branch parameter
# Shows only branches that have active worktrees
Register-ArgumentCompleter -CommandName "$script:WorktreeScriptDir\worktree-down.ps1" -ParameterName Branch -ScriptBlock {
    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
    
    $worktreeBranches = @()
    
    # Parse git worktree list output
    $worktrees = git worktree list --porcelain 2>$null
    foreach ($line in $worktrees) {
        if ($line -match '^branch refs/heads/(.+)') {
            $worktreeBranches += $matches[1]
        }
    }
    
    $worktreeBranches | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
    }
}

# Completion for worktree-up.ps1 -Command parameter
# Shows common commands to run in container
Register-ArgumentCompleter -CommandName "$script:WorktreeScriptDir\worktree-up.ps1" -ParameterName Command -ScriptBlock {
    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
    
    $commands = @(
        @{ Name = 'copilot --yolo'; Desc = 'Copilot CLI in yolo mode (default)' }
        @{ Name = 'copilot'; Desc = 'Copilot CLI with prompts' }
        @{ Name = 'bash'; Desc = 'Interactive bash shell' }
        @{ Name = 'cargo build'; Desc = 'Build the project' }
        @{ Name = 'cargo test'; Desc = 'Run tests' }
        @{ Name = 'cargo clippy'; Desc = 'Run linter' }
    )
    
    $commands | Where-Object { $_.Name -like "$wordToComplete*" } | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new(
            $_.Name,
            $_.Name,
            'ParameterValue',
            $_.Desc
        )
    }
}

# Completion for worktree-up.ps1 -BaseBranch parameter
# Shows common base branches
Register-ArgumentCompleter -CommandName "$script:WorktreeScriptDir\worktree-up.ps1" -ParameterName BaseBranch -ScriptBlock {
    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
    
    # Get local branches, prioritizing main/master
    $branches = @('main', 'master')
    $local = git branch --format='%(refname:short)' 2>$null | 
        Where-Object { $_ -notin @('main', 'master') }
    if ($local) { $branches += $local }
    
    $branches | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
    }
}

Write-Host "✅ Worktree completions loaded" -ForegroundColor Green
