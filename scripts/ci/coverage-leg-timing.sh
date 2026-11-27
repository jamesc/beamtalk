#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Reports how long a coverage.yml leg has been running and warns when it has
# used more of its timeout than the leg's budget allows (BT-5356). A leg that
# hits its job timeout is killed mid-step and reports nothing, so the warning
# has to come from a leg that still finished.
#
# Usage: coverage-leg-timing.sh <leg-name> <start-epoch-seconds> <budget-minutes>
set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <leg-name> <start-epoch-seconds> <budget-minutes>" >&2
  exit 2
fi

leg="$1"
start="$2"
budget="$3"

elapsed=$(( $(date +%s) - start ))
minutes=$(( (elapsed + 30) / 60 ))

echo "${leg}: ${minutes} min (budget ${budget} min)"
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  echo "**${leg} wall time:** ${minutes} min (budget ${budget} min)" >> "$GITHUB_STEP_SUMMARY"
fi
if [ "$minutes" -gt "$budget" ]; then
  echo "::warning title=${leg} over its time budget::${leg} took ${minutes} min, over its ${budget} min budget; the job timeout is close. Shard or trim it before it starts timing out."
fi
