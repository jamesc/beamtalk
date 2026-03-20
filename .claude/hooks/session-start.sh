#!/bin/bash
set -euo pipefail

# Install tessl CLI if not present (all environments)
command -v tessl &>/dev/null || npm install -g tessl >/dev/null 2>&1 || true

# Only run remaining setup in remote (web) environments
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Install CI tools (shellcheck, markdownlint-cli2)
if ! command -v shellcheck &>/dev/null; then
  if ! { apt-get update -qq && apt-get install -y -qq shellcheck; } >/dev/null 2>&1; then
    true
  fi
fi
command -v markdownlint-cli2 &>/dev/null || npm install -g markdownlint-cli2 >/dev/null 2>&1 || true

# Copy skills from repo to global skills directory
SKILLS_DIR="${HOME}/.claude/skills"
mkdir -p "$SKILLS_DIR"

for skill_dir in "$CLAUDE_PROJECT_DIR"/skills/*/; do
  [ -d "$skill_dir" ] || continue
  skill_name=$(basename "$skill_dir")
  if [ -f "$skill_dir/SKILL.md" ]; then
    mkdir -p "$SKILLS_DIR/$skill_name"
    cp -r "$skill_dir"/* "$SKILLS_DIR/$skill_name/"
  fi
done

# Copy agents from repo to global agents directory
AGENTS_DIR="${HOME}/.claude/agents"
mkdir -p "$AGENTS_DIR"

for agent_file in "$CLAUDE_PROJECT_DIR"/agents/*.md; do
  [ -f "$agent_file" ] || continue
  cp "$agent_file" "$AGENTS_DIR/"
done
