#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Publishes one or more shields.io endpoint-format coverage-badge JSON files
# to the shared `badges` orphan branch, via a git worktree so the caller's
# own checkout is never disturbed. ci.yml's `coverage` job (rust/erlang) and
# liveview.yml's `coverage` job (elixir) both call this — they can publish
# around the same time, so a plain fetch+commit+push can hit a non-fast-
# forward rejection; this retries (refetch + reapply + push) instead of
# failing the run outright (PR #3549 review).
#
# Usage: publish-coverage-badge.sh <commit-message> <file1> <content1> [<file2> <content2> ...]
set -euo pipefail

if [ "$#" -lt 3 ] || [ $(( ($# - 1) % 2 )) -ne 0 ]; then
  echo "Usage: $0 <commit-message> <file1> <content1> [<file2> <content2> ...]" >&2
  exit 2
fi

commit_message="$1"
shift

git config user.name "github-actions[bot]"
git config user.email "github-actions[bot]@users.noreply.github.com"

worktree=$(mktemp -d)
trap 'git worktree remove --force "$worktree" >/dev/null 2>&1 || true; rm -rf "$worktree"' EXIT

attempt=0
until [ "$attempt" -ge 5 ]; do
  attempt=$((attempt + 1))
  git worktree remove --force "$worktree" >/dev/null 2>&1 || true
  rm -rf "$worktree"
  git fetch origin badges
  git worktree add "$worktree" origin/badges >/dev/null

  files=()
  i=1
  while [ "$i" -le "$#" ]; do
    file="${!i}"
    content_idx=$((i + 1))
    content="${!content_idx}"
    echo "$content" > "$worktree/$file"
    files+=("$file")
    i=$((i + 2))
  done

  git -C "$worktree" add "${files[@]}"
  if git -C "$worktree" diff --staged --quiet; then
    echo "No badge changes to publish"
    exit 0
  fi
  git -C "$worktree" commit -m "$commit_message" >/dev/null
  if git -C "$worktree" push origin HEAD:badges; then
    echo "Badge published"
    exit 0
  fi
  echo "Push rejected (attempt ${attempt}), retrying..."
  sleep $((RANDOM % 5 + attempt))
done

echo "::error::Failed to push badge(s) after ${attempt} attempts (concurrent badge publisher?)"
exit 1
