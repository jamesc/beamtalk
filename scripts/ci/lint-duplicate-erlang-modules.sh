#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Guard (BT-4671): every Erlang module name under runtime/apps/*/{src,test}
# is unique. The BEAM loads one module per name, so two `foo_tests.erl` in
# different apps compile fine but only one is ever loaded: the other's tests
# silently never run (this hid beamtalk_exec_port_tests and
# beamtalk_list_tests in both beamtalk_runtime/test and beamtalk_stdlib/test).
#
# The module name is the file's basename, which erlc requires to match its
# `-module(...)` attribute.
#
# To clear a failure: rename one of the files (and its `-module` attribute).
#
# Usage: scripts/ci/lint-duplicate-erlang-modules.sh

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

dupes="$(
    git ls-files -- 'runtime/apps/*/src/*.erl' 'runtime/apps/*/test/*.erl' \
        'runtime/apps/*/src/**/*.erl' 'runtime/apps/*/test/**/*.erl' \
        | sort -u \
        | awk -F/ '{ name = $NF; files[name] = files[name] "\n    " $0; count[name]++ }
                   END { for (n in count) if (count[n] > 1) print n files[n] }'
)"

if [[ -n "$dupes" ]]; then
    echo "❌ Erlang module names defined more than once under runtime/apps/*/{src,test}:"
    echo "$dupes"
    echo "Only one copy of each is loaded, so the other never runs. Rename one."
    exit 1
fi
echo "✅ Erlang module names under runtime/apps/*/{src,test} are unique"
