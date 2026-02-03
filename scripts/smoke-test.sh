#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Smoke test to verify devcontainer is working correctly

set -e

echo "🧪 Running devcontainer smoke test..."
echo ""

# Check required tools
check_tool() {
    if command -v "$1" &> /dev/null; then
        echo "✅ $1: $($1 --version 2>&1 | head -1)"
    else
        echo "❌ $1: NOT FOUND"
        exit 1
    fi
}

echo "Checking tools..."
check_tool git
check_tool node
check_tool npm
check_tool gh

# Check copilot CLI (different version flag)
if command -v copilot &> /dev/null; then
    echo "✅ copilot: installed"
else
    echo "❌ copilot: NOT FOUND"
    exit 1
fi

# Check Beamtalk-specific tools
echo ""
echo "Checking Beamtalk tools..."
check_tool rustc
check_tool cargo
check_tool erl
check_tool rebar3

echo ""

# Check git configuration
echo "Checking git config..."
if git config user.name &> /dev/null; then
    echo "✅ git user.name: $(git config user.name)"
else
    echo "⚠️  git user.name: not set"
fi

if git config user.email &> /dev/null; then
    echo "✅ git user.email: $(git config user.email)"
else
    echo "⚠️  git user.email: not set"
fi

echo ""

# Check GitHub auth
echo "Checking GitHub auth..."
if gh auth status &> /dev/null 2>&1; then
    echo "✅ gh auth: authenticated"
else
    echo "⚠️  gh auth: not authenticated (set GH_TOKEN)"
fi

echo ""
echo "✨ Smoke test passed!"
