#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# The one gate for the Rust corpus tier (`just test-corpus`: the ADR 0111/0131
# state-threading agreement and corpus proofs, ~13 min on 4 cores). Reads
# changed paths on stdin, one per line, and exits 0 when any of them can change
# the tier's verdict, 1 otherwise. `just ci-changed` and ci.yml's Linux `check`
# job both call it, so the path list lives here and nowhere else.
#
# The tier's tests call the compiler in-process (beamtalk-core, beamtalk-codegen,
# beamtalk-repl; the `corpus_tests` filterset in the Justfile is scoped to those
# three packages) and never touch the runtime, the CLI or the stdlib sources, so
# only those crates and the build/test configuration are listed.
set -euo pipefail

while IFS= read -r path; do
  [[ -z "$path" ]] && continue
  case "$path" in
    crates/beamtalk-core/*|crates/beamtalk-codegen/*|crates/beamtalk-repl/*) exit 0 ;;
    Cargo.toml|Cargo.lock|Justfile|.github/workflows/ci.yml|scripts/ci/corpus-gate.sh) exit 0 ;;
  esac
done
exit 1
