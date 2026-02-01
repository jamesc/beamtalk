#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Compile test fixtures for runtime tests

set -e

cd "$(dirname "$0")/.."

echo "Compiling test fixtures..."

# Check if beamtalk binary exists
if [ ! -f ./target/debug/beamtalk ]; then
    echo "  ⚠️  beamtalk binary not found, building..."
    cargo build --bin beamtalk
fi

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

echo "Done!"
