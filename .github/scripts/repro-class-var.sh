#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Reproduce one failing program of the ADR 0130 class-variable agreement corpus
# for the fuzz-triage agent (fuzz-triage.yml): print its source and reference
# answer. Run from the root of the code under test:
#
#   bash .triage-config/.github/scripts/repro-class-var.sh <seed> <size> <shapes|all>
#
# The agent's tool allowlist cannot match a `VAR=value cargo test ...` command
# (env-prefixed commands are denied), so the variables are set here. The arguments
# come from untrusted job logs; they are validated before use and never reach a
# shell as anything but a quoted value.
set -euo pipefail

seed="${1:-}"
size="${2:-}"
shapes="${3:-}"

if ! [[ "$seed" =~ ^[0-9]{1,20}$ ]]; then echo "error: seed must be 1-20 digits" >&2; exit 2; fi
if ! [[ "$size" =~ ^[0-9]{1,3}$ ]]; then echo "error: size must be 1-3 digits" >&2; exit 2; fi
if ! [[ "$shapes" =~ ^[a-z_]+(,[a-z_]+)*$ ]]; then echo "error: shapes must be 'all' or comma-separated names" >&2; exit 2; fi
if ! [ -f Cargo.toml ] || ! [ -d crates/beamtalk-core ]; then
  echo "error: run from the root of the beamtalk checkout" >&2; exit 2
fi

CV_SEED="$seed" CV_SIZE="$size" CV_SHAPES="$shapes" \
  cargo test -p beamtalk-core --lib dump_program -- --ignored --nocapture
