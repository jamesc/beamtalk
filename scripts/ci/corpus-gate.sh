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
# three packages) and never run the runtime or the CLI, so those crates and
# the build/test configuration are listed. They do read two kinds of input from
# outside those crates, and a change to either alone can flip their verdict
# (BT-4261):
#   - the Beamtalk sources under stdlib/ and examples/, which many of them
#     compile as their corpus (e.g. match_fallthrough_goto's
#     `goto_labels_are_applied_in_tail_position_over_the_corpus`,
#     escape_corpus_conformance, paren_transparency's stdlib/test corpus);
#   - the other Beamtalk test corpora class_var_census's
#     `census_over_repository_corpus` scans (test-package-compiler/cases,
#     tests/repl-protocol/cases and tests/repl-protocol/fixtures);
#   - the runtime's shared cross-language conformance fixtures
#     (runtime/apps/*/test/fixtures/), which the `*_matches_shared_corpus`
#     tests read (the `_corpus` in their name puts them in this tier).
# Both are listed by directory, failing closed rather than guessing which
# files a corpus reads.
set -euo pipefail

# Read every line before exiting: both callers pipe into this script under
# `pipefail`, and exiting on the first hit would SIGPIPE a producer still
# writing a long list, turning a match into a non-zero pipeline (a silent
# "skip" for the tier). The gate must fail closed, never open.
hit=1
while IFS= read -r path; do
  [[ -z "$path" ]] && continue
  case "$path" in
    crates/beamtalk-core/*|crates/beamtalk-codegen/*|crates/beamtalk-repl/*) hit=0 ;;
    stdlib/*|examples/*|runtime/apps/*/test/fixtures/*) hit=0 ;;
    test-package-compiler/cases/*|tests/repl-protocol/cases/*|tests/repl-protocol/fixtures/*) hit=0 ;;
    Cargo.toml|Cargo.lock|Justfile|.github/workflows/ci.yml|scripts/ci/corpus-gate.sh) hit=0 ;;
  esac
done
exit "$hit"
