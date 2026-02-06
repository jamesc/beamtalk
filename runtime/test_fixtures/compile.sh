#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Compile test fixtures for runtime tests
# Called by rebar3 pre-hook before eunit

set -e

SCRIPT_DIR="$(dirname "$0")"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$REPO_ROOT"

# Ensure beamtalk binary is built (cargo will skip if up-to-date)
cargo build --bin beamtalk --quiet 2>/dev/null || cargo build --bin beamtalk --quiet

# Clean old artifacts to ensure fresh compilation
rm -f tests/e2e/fixtures/build/counter.beam tests/e2e/fixtures/build/counter.core
rm -f runtime/test_fixtures/build/logging_counter.beam runtime/test_fixtures/build/logging_counter.core

# Build counter fixture (using E2E fixture - BT-239)
./target/debug/beamtalk build tests/e2e/fixtures/counter.bt >/dev/null 2>&1

# Verify build succeeded
if [ ! -f tests/e2e/fixtures/build/counter.beam ]; then
    echo "✗ Failed to compile counter.bt"
    exit 1
fi

# Copy to rebar3 build directories (for test execution)
for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
    [ -d "$build_dir" ] && cp tests/e2e/fixtures/build/counter.beam "$build_dir/"
done

# Build logging_counter fixture (BT-108 - super keyword tests)
./target/debug/beamtalk build runtime/test_fixtures/logging_counter.bt >/dev/null 2>&1

# Verify build succeeded
if [ ! -f runtime/test_fixtures/build/logging_counter.beam ]; then
    echo "✗ Failed to compile logging_counter.bt"
    exit 1
fi

# Copy to rebar3 build directories
for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
    [ -d "$build_dir" ] && cp runtime/test_fixtures/build/logging_counter.beam "$build_dir/"
done
