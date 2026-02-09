#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Compile standard library .bt files and copy to runtime ebin/
# Called by rebar3 pre-compile hook
#
# This script compiles Beamtalk standard library files
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
set -o pipefail

SCRIPT_DIR="$(dirname "$0")"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

echo "Compiling standard library..."

# Check if beamtalk binary exists, build if missing
if [ ! -f "./target/debug/beamtalk" ] && [ ! -f "./target/release/beamtalk" ]; then
    echo "  Beamtalk binary not found. Building..."
    cargo build --bin beamtalk --quiet 2>&1 | grep -v "Compiling\|Finished" || true
    
    # Verify build succeeded
    if [ ! -f "./target/debug/beamtalk" ] && [ ! -f "./target/release/beamtalk" ]; then
        echo "  Error: Failed to build beamtalk binary."
        exit 1
    fi
fi

# Use whichever binary exists (prefer release for speed)
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
    "lib/SystemDictionary.bt"
)

# Compile each file
for btfile in "${STDLIB_FILES[@]}"; do
    filename=$(basename "$btfile" .bt)
    echo "  Building $btfile..."
    
    # Clean stale artifacts to avoid masking compilation failures
    rm -f "lib/build/$filename.core" "lib/build/$filename.beam"
    
    # Compile to Core Erlang (outputs to lib/build/ by default)
    # Note: beamtalk build may report "Compilation failed" even on success due to erlc warnings
    # We verify success by checking for the .core file
    $BEAMTALK build "$btfile" > /dev/null 2>&1 || true
    
    # Verify Core Erlang was generated
    if [ ! -f "lib/build/$filename.core" ]; then
        echo "    ✗ Failed to generate Core Erlang: $btfile"
        $BEAMTALK build "$btfile"  # Show error output
        exit 1
    fi
    
    # Compile Core Erlang to BEAM
    (cd lib/build && erlc "$filename.core")
    if [ -f "lib/build/$filename.beam" ]; then
        echo "    ✓ Compiled $filename.beam"
    else
        echo "    ✗ Failed to compile Core Erlang to BEAM: $btfile"
        exit 1
    fi
done

# Copy to rebar3 build ebin directories
# Ensure at least the default profile ebin exists (for clean builds)
DEFAULT_EBIN="runtime/_build/default/lib/beamtalk_runtime/ebin"
mkdir -p "$DEFAULT_EBIN"

copied_count=0
for beam_file in lib/build/*.beam; do
    filename=$(basename "$beam_file")
    for ebin_dir in runtime/_build/*/lib/beamtalk_runtime/ebin; do
        if [ -d "$ebin_dir" ]; then
            cp "$beam_file" "$ebin_dir/"
            echo "    ✓ Copied $filename to $ebin_dir/"
            copied_count=$((copied_count + 1))
        fi
    done
done

if [ $copied_count -eq 0 ]; then
    echo "    ✗ Error: No target ebin directories found to copy stdlib modules"
    exit 1
fi

echo "Standard library compilation complete!"
