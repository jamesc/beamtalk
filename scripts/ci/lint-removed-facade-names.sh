#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Guard (BT-3646, ADR 0129 Phase 5): the pre-ADR-0129 injected-singleton names
# must not reappear in tracked files. `Beamtalk`, `Workspace`, `Transcript` and
# `SystemNavigation` are class-side facades now; `Workspace bindings` replaces
# `Workspace globals`; `Beamtalk globals` and `SystemNavigation default` are gone.
#
# Historical records are exempt: ADRs (docs/ADR/), the changelog, and the
# generated example corpora (regenerate those, do not edit them). CLAUDE.md
# names the retired singletons in a "don't copy this" rule, and this script
# holds the patterns themselves. The class_side_facades REPL case is exempt
# because it asserts that `Beamtalk globals` raises does_not_understand.
#
# Usage: scripts/ci/lint-removed-facade-names.sh

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

PATTERN='BeamtalkInterface|WorkspaceInterface|TranscriptStream current|Beamtalk globals|Workspace globals|SystemNavigation default'

hits="$(git grep -nE "$PATTERN" -- . \
    ':(exclude)docs/ADR/' \
    ':(exclude)CHANGELOG.md' \
    ':(exclude)CLAUDE.md' \
    ':(exclude)crates/beamtalk-examples/corpus.json' \
    ':(exclude)crates/beamtalk-examples/class_corpus.json' \
    ':(exclude)scripts/ci/lint-removed-facade-names.sh' \
    ':(exclude)tests/repl-protocol/cases/class_side_facades.btscript' || true)"

if [[ -n "$hits" ]]; then
    echo "❌ Removed ADR 0129 facade names found (use the class-side facades; see docs/ADR/0129-class-side-system-facades.md):"
    echo "$hits"
    exit 1
fi
echo "✅ No removed facade names (BeamtalkInterface, WorkspaceInterface, TranscriptStream current, Beamtalk globals, Workspace globals, SystemNavigation default)"
