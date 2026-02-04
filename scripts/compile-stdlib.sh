#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Compile standard library .bt files and copy to runtime ebin/
# Called by rebar3 pre-compile hook
#
# This script compiles Beamtalk standard library files (currently just beamtalk.bt)
# into BEAM bytecode and places them in the runtime's ebin directories so they're
# automatically available when the BEAM VM starts.
#
# Process:
# 1. Find beamtalk binary (debug or release)
# 2. Compile each .bt file to Core Erlang (.core)
# 3. Compile Core Erlang to BEAM bytecode (.beam)
# 4. Copy .beam files to runtime/_build/*/lib/beamtalk_runtime/ebin/
#
# Each stdlib module has an -on_load attribute that auto-registers its class
# with the beamtalk_class registry when loaded.

set -e

SCRIPT_DIR="$(dirname "$0")"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

echo "Compiling standard library..."

# Check if beamtalk binary exists
if [ ! -f "./target/debug/beamtalk" ] && [ ! -f "./target/release/beamtalk" ]; then
    echo "  Error: beamtalk binary not found. Run 'cargo build' first."
    exit 1
fi

# Use whichever binary exists
if [ -f "./target/release/beamtalk" ]; then
    BEAMTALK="./target/release/beamtalk"
else
    BEAMTALK="./target/debug/beamtalk"
fi

echo "  Using $BEAMTALK"

# Create build directory for stdlib
mkdir -p lib/build

# List of stdlib files to compile
STDLIB_FILES=(
    "lib/beamtalk.bt"
)

# Compile each file
for btfile in "${STDLIB_FILES[@]}"; do
    filename=$(basename "$btfile" .bt)
    echo "  Building $btfile..."
    
    # Compile to Core Erlang (outputs to lib/build/ by default)
    # Note: beamtalk build may report errors even on success due to erlc warnings
    # We check for the .core file existence instead of exit code
    $BEAMTALK build "$btfile" 2>&1 | grep -v "Failed to compile Core Erlang" || true
    
    # Compile Core Erlang to BEAM if .core file exists
    if [ -f "lib/build/$filename.core" ]; then
        (cd lib/build && erlc "$filename.core")
        if [ -f "lib/build/$filename.beam" ]; then
            echo "    ✓ Compiled $filename.beam"
        else
            echo "    ✗ Failed to compile Core Erlang to BEAM: $btfile"
            exit 1
        fi
    else
        echo "    ✗ Failed to generate Core Erlang: $btfile"
        exit 1
    fi
done

# Copy to rebar3 build ebin directories
for beam_file in lib/build/*.beam; do
    filename=$(basename "$beam_file")
    for ebin_dir in runtime/_build/*/lib/beamtalk_runtime/ebin; do
        if [ -d "$ebin_dir" ]; then
            cp "$beam_file" "$ebin_dir/"
            echo "    ✓ Copied $filename to $ebin_dir/"
        fi
    done
done

echo "Standard library compilation complete!"
