#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Compile test fixtures for runtime tests

set -e

cd "$(dirname "$0")/.."

echo "Compiling test fixtures..."

# Build counter fixture
echo "  Building counter.bt..."
./target/debug/beamtalk build tests/fixtures/counter.bt || true  # May report error even on success

# Copy to runtime test directory (erlc creates counter.beam in build/)
if [ -f tests/fixtures/build/counter.beam ]; then
    cp tests/fixtures/build/counter.beam runtime/test/
    echo "  ✓ Copied counter.beam to runtime/test/"
elif [ -f tests/fixtures/build/counter.core ]; then
    # Try manual erlc compile if beamtalk build reports error but creates .core
    echo "  Compiling Core Erlang manually..."
    cd tests/fixtures/build
    erlc counter.core
    cd ../../..
    cp tests/fixtures/build/counter.beam runtime/test/
    echo "  ✓ Compiled and copied counter.beam to runtime/test/"
else
    echo "  ✗ Failed to compile counter.bt"
    exit 1
fi

echo "Done!"
