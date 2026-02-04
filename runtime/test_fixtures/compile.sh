#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Compile test fixtures for runtime tests
# Called by rebar3 pre-hook before eunit

set -e

SCRIPT_DIR="$(dirname "$0")"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$REPO_ROOT"

echo "Compiling test fixtures..."

# Ensure beamtalk binary is built (cargo will skip if up-to-date)
echo "  Checking beamtalk binary..."
cargo build --bin beamtalk --quiet

# Build counter fixture
echo "  Building counter.bt..."
./target/debug/beamtalk build runtime/test_fixtures/counter.bt || true  # May report error even on success

# Copy to rebar3 build directories (for test execution)
if [ -f runtime/test_fixtures/build/counter.beam ]; then
    for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
        if [ -d "$build_dir" ]; then
            cp runtime/test_fixtures/build/counter.beam "$build_dir/"
            echo "  ✓ Copied counter.beam to $build_dir/"
        fi
    done
elif [ -f runtime/test_fixtures/build/counter.core ]; then
    # Try manual erlc compile if beamtalk build reports error but creates .core
    echo "  Compiling Core Erlang manually..."
    cd runtime/test_fixtures/build
    erlc counter.core
    cd ../../..
    
    # Copy to rebar3 build directories
    for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
        if [ -d "$build_dir" ]; then
            cp runtime/test_fixtures/build/counter.beam "$build_dir/"
            echo "  ✓ Copied counter.beam to $build_dir/"
        fi
    done
else
    echo "  ✗ Failed to compile counter.bt"
    exit 1
fi

# Build logging_counter fixture (BT-108 - super keyword tests)
echo "  Building logging_counter.bt..."
./target/debug/beamtalk build runtime/test_fixtures/logging_counter.bt || true

# Copy to rebar3 build directories
if [ -f runtime/test_fixtures/build/logging_counter.beam ]; then
    for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
        if [ -d "$build_dir" ]; then
            cp runtime/test_fixtures/build/logging_counter.beam "$build_dir/"
            echo "  ✓ Copied logging_counter.beam to $build_dir/"
        fi
    done
elif [ -f runtime/test_fixtures/build/logging_counter.core ]; then
    # Try manual erlc compile
    echo "  Compiling Core Erlang manually..."
    cd runtime/test_fixtures/build
    erlc logging_counter.core
    cd ../../..
    
    # Copy to rebar3 build directories
    for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
        if [ -d "$build_dir" ]; then
            cp runtime/test_fixtures/build/logging_counter.beam "$build_dir/"
            echo "  ✓ Copied logging_counter.beam to $build_dir/"
        fi
    done
else
    echo "  ✗ Failed to compile logging_counter.bt"
    exit 1
fi

echo "Done!"
