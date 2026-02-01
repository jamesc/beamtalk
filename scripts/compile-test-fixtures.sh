#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Compile test fixtures for runtime tests

set -e

cd "$(dirname "$0")/.."

echo "Compiling test fixtures..."

# Ensure beamtalk binary is built (cargo will skip if up-to-date)
echo "  Checking beamtalk binary..."
cargo build --bin beamtalk --quiet

# Build counter fixture
echo "  Building counter.bt..."
./target/debug/beamtalk build tests/fixtures/counter.bt || true  # May report error even on success

# Copy to runtime test directory (for source control and local dev)
if [ -f tests/fixtures/build/counter.beam ]; then
    cp tests/fixtures/build/counter.beam runtime/test/
    echo "  ✓ Copied counter.beam to runtime/test/"
    
    # Also copy to rebar3 build directories if they exist (for test execution)
    for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
        if [ -d "$build_dir" ]; then
            cp tests/fixtures/build/counter.beam "$build_dir/"
            echo "  ✓ Copied counter.beam to $build_dir/"
        fi
    done
elif [ -f tests/fixtures/build/counter.core ]; then
    # Try manual erlc compile if beamtalk build reports error but creates .core
    echo "  Compiling Core Erlang manually..."
    cd tests/fixtures/build
    erlc counter.core
    cd ../../..
    cp tests/fixtures/build/counter.beam runtime/test/
    echo "  ✓ Compiled and copied counter.beam to runtime/test/"
    
    # Also copy to rebar3 build directories
    for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
        if [ -d "$build_dir" ]; then
            cp tests/fixtures/build/counter.beam "$build_dir/"
            echo "  ✓ Copied counter.beam to $build_dir/"
        fi
    done
else
    echo "  ✗ Failed to compile counter.bt"
    exit 1
fi

# Build logging_counter fixture (BT-108 - super keyword tests)
echo "  Building logging_counter.bt..."
./target/debug/beamtalk build tests/fixtures/logging_counter.bt || true

# Copy to runtime test directory
if [ -f tests/fixtures/build/logging_counter.beam ]; then
    cp tests/fixtures/build/logging_counter.beam runtime/test/
    echo "  ✓ Copied logging_counter.beam to runtime/test/"
    
    # Also copy to rebar3 build directories
    for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
        if [ -d "$build_dir" ]; then
            cp tests/fixtures/build/logging_counter.beam "$build_dir/"
            echo "  ✓ Copied logging_counter.beam to $build_dir/"
        fi
    done
elif [ -f tests/fixtures/build/logging_counter.core ]; then
    # Try manual erlc compile
    echo "  Compiling Core Erlang manually..."
    cd tests/fixtures/build
    erlc logging_counter.core
    cd ../../..
    cp tests/fixtures/build/logging_counter.beam runtime/test/
    echo "  ✓ Compiled and copied logging_counter.beam to runtime/test/"
    
    # Also copy to rebar3 build directories
    for build_dir in runtime/_build/*/lib/beamtalk_runtime/test; do
        if [ -d "$build_dir" ]; then
            cp tests/fixtures/build/logging_counter.beam "$build_dir/"
            echo "  ✓ Copied logging_counter.beam to $build_dir/"
        fi
    done
else
    echo "  ✗ Failed to compile logging_counter.bt"
    exit 1
fi

echo "Done!"
