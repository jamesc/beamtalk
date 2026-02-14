#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Benchmark compilation latency for the Beamtalk compiler.
#
# Measures end-to-end compilation latency for file builds and REPL expressions,
# broken down by phase where possible. This establishes the baseline before
# migrating from daemon IPC to OTP Port (ADR 0022).
#
# Usage:
#   scripts/bench-compilation.sh              # Run all benchmarks (100 iterations)
#   scripts/bench-compilation.sh --iterations 10  # Quick run with fewer iterations
#   scripts/bench-compilation.sh --repl-only  # Only REPL benchmarks
#   scripts/bench-compilation.sh --file-only  # Only file compilation benchmarks
#
# Prerequisites:
#   - Built project: `just build`
#   - No other REPL/workspace instances running

set -euo pipefail

# ═══════════════════════════════════════════════════════════════════════════════
# Configuration
# ═══════════════════════════════════════════════════════════════════════════════

ITERATIONS=100
RUN_REPL=true
RUN_FILE=true
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BEAMTALK="$PROJECT_ROOT/target/debug/beamtalk"
RESULTS_DIR="$(mktemp -d)"
REPL_PID=""
REPL_PORT=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --iterations) ITERATIONS="$2"; shift 2 ;;
        --repl-only) RUN_FILE=false; shift ;;
        --file-only) RUN_REPL=false; shift ;;
        --help) echo "Usage: $0 [--iterations N] [--repl-only] [--file-only]"; exit 0 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

# ═══════════════════════════════════════════════════════════════════════════════
# Utility functions
# ═══════════════════════════════════════════════════════════════════════════════

cleanup() {
    if [ -n "$REPL_PID" ]; then
        kill "$REPL_PID" 2>/dev/null || true
        wait "$REPL_PID" 2>/dev/null || true
    fi
    rm -rf "$RESULTS_DIR"
}
trap cleanup EXIT

# Get high-resolution time in nanoseconds (Linux)
time_ns() {
    date +%s%N
}

# Calculate elapsed time in milliseconds from two nanosecond timestamps
elapsed_ms() {
    local start=$1 end=$2
    awk "BEGIN { printf \"%.3f\n\", ($end - $start) / 1000000 }"
}

# Compute percentiles from a file of numbers (one per line)
# Usage: percentile <file> <p> (where p is 50, 95, 99, etc.)
percentile() {
    local file=$1 p=$2
    local count
    count=$(wc -l < "$file")
    if [ "$count" -eq 0 ]; then
        echo "0"
        return
    fi
    local idx
    idx=$(awk "BEGIN { printf \"%d\", ($count * $p + 99) / 100 }")
    # Clamp to valid range
    if [ "$idx" -lt 1 ]; then idx=1; fi
    if [ "$idx" -gt "$count" ]; then idx=$count; fi
    sort -n "$file" | sed -n "${idx}p"
}

# Compute mean from a file of numbers
mean() {
    local file=$1
    awk '{ sum += $1; n++ } END { if (n > 0) printf "%.3f", sum/n; else print "0" }' "$file"
}

# Compute min from a file of numbers
minimum() {
    sort -n "$1" | head -1
}

# Compute max from a file of numbers
maximum() {
    sort -n "$1" | tail -1
}

# Print a benchmark result table row
print_result() {
    local label=$1 file=$2
    local p50 p95 p99 avg mn mx
    p50=$(percentile "$file" 50)
    p95=$(percentile "$file" 95)
    p99=$(percentile "$file" 99)
    avg=$(mean "$file")
    mn=$(minimum "$file")
    mx=$(maximum "$file")
    printf "| %-40s | %10s | %10s | %10s | %10s | %10s | %10s |\n" \
        "$label" "$p50" "$p95" "$p99" "$avg" "$mn" "$mx"
}

# Print table header
print_header() {
    printf "| %-40s | %10s | %10s | %10s | %10s | %10s | %10s |\n" \
        "Benchmark" "p50 (ms)" "p95 (ms)" "p99 (ms)" "mean (ms)" "min (ms)" "max (ms)"
    printf "|%-42s|%12s|%12s|%12s|%12s|%12s|%12s|\n" \
        "$(printf -- '-%.0s' {1..42})" \
        "$(printf -- '-%.0s' {1..12})" \
        "$(printf -- '-%.0s' {1..12})" \
        "$(printf -- '-%.0s' {1..12})" \
        "$(printf -- '-%.0s' {1..12})" \
        "$(printf -- '-%.0s' {1..12})" \
        "$(printf -- '-%.0s' {1..12})"
}

# ═══════════════════════════════════════════════════════════════════════════════
# Environment capture
# ═══════════════════════════════════════════════════════════════════════════════

echo "╔══════════════════════════════════════════════════════════════════╗"
echo "║           Beamtalk Compilation Latency Benchmark               ║"
echo "╚══════════════════════════════════════════════════════════════════╝"
echo ""
echo "Environment:"
echo "  OS:          $(uname -s) $(uname -r) ($(uname -m))"
echo "  CPU:         $(grep -m1 'model name' /proc/cpuinfo 2>/dev/null | cut -d: -f2 | xargs || echo 'unknown')"
echo "  CPU cores:   $(nproc 2>/dev/null || echo 'unknown')"
echo "  RAM:         $(free -h 2>/dev/null | awk '/Mem:/ {print $2}' || echo 'unknown')"
echo "  Rust:        $(rustc --version 2>/dev/null || echo 'not found')"
echo "  Erlang/OTP:  $(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null || echo 'not found')"
echo "  ERTS:        $(erl -eval 'io:format("~s", [erlang:system_info(version)]), halt().' -noshell 2>/dev/null || echo 'not found')"
echo "  Beamtalk:    $(git -C "$PROJECT_ROOT" rev-parse --short HEAD 2>/dev/null || echo 'unknown')"
echo "  Build:       debug"
echo "  Iterations:  $ITERATIONS"
echo "  Date:        $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo ""

# ═══════════════════════════════════════════════════════════════════════════════
# Pre-flight checks
# ═══════════════════════════════════════════════════════════════════════════════

if [ ! -x "$BEAMTALK" ]; then
    echo "❌ beamtalk binary not found at $BEAMTALK"
    echo "   Run 'just build' first."
    exit 1
fi

# Verify python3 is available (for REPL benchmarks)
if [ "$RUN_REPL" = true ] && ! command -v python3 &>/dev/null; then
    echo "❌ 'python3' not found (required for REPL benchmarks)"
    exit 1
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Benchmark 1: File Compilation (beamtalk build)
# ═══════════════════════════════════════════════════════════════════════════════

if [ "$RUN_FILE" = true ]; then
    echo "═══════════════════════════════════════════════════════════════════"
    echo " File Compilation Benchmarks (beamtalk build)"
    echo "═══════════════════════════════════════════════════════════════════"
    echo ""

    # Benchmark files
    BENCH_FILES=(
        "examples/counter.bt:Counter (26 lines, minimal)"
        "examples/chat_member.bt:ChatMember (55 lines, typical)"
        "examples/chat_room.bt:ChatRoom (69 lines, multi-actor)"
    )

    for entry in "${BENCH_FILES[@]}"; do
        FILE="${entry%%:*}"
        LABEL="${entry##*:}"
        FILEPATH="$PROJECT_ROOT/$FILE"
        FILESTEM=$(basename "$FILE" .bt)

        if [ ! -f "$FILEPATH" ]; then
            echo "⚠️  Skipping $FILE (not found)"
            continue
        fi

        echo "📊 Benchmarking: $LABEL"
        echo "   File: $FILE"

        # Phase-separated timing files
        TIMES_E2E="$RESULTS_DIR/file_e2e_${FILESTEM}.txt"
        TIMES_BEAM="$RESULTS_DIR/file_beam_${FILESTEM}.txt"
        : > "$TIMES_E2E"
        : > "$TIMES_BEAM"

        BUILDDIR="$PROJECT_ROOT/$(dirname "$FILE")/build"

        # Generate .core file once to find its path for BEAM-only measurement
        rm -rf "$BUILDDIR" 2>/dev/null || true
        "$BEAMTALK" build "$FILEPATH" >/dev/null 2>&1 || true
        CORE_FILE=$(find "$BUILDDIR" -name "*.core" -type f 2>/dev/null | head -1)

        for i in $(seq 1 "$ITERATIONS"); do
            rm -rf "$BUILDDIR" 2>/dev/null || true

            # Measure end-to-end (Rust binary startup + compile + BEAM)
            start=$(time_ns)
            "$BEAMTALK" build "$FILEPATH" >/dev/null 2>&1 || true
            end=$(time_ns)
            elapsed_ms "$start" "$end" >> "$TIMES_E2E"

            # Measure BEAM phase only (.core → .beam via erlc)
            # Re-find core file since build dir was recreated
            CORE_FILE=$(find "$BUILDDIR" -name "*.core" -type f 2>/dev/null | head -1)
            if [ -n "$CORE_FILE" ]; then
                # Remove .beam so erlc does real work
                rm -f "$BUILDDIR"/*.beam 2>/dev/null || true
                start=$(time_ns)
                erlc +from_core +debug_info -o "$BUILDDIR" "$CORE_FILE" >/dev/null 2>&1 || true
                end=$(time_ns)
                elapsed_ms "$start" "$end" >> "$TIMES_BEAM"
            fi

            rm -rf "$BUILDDIR" 2>/dev/null || true

            if (( i % 10 == 0 )); then
                printf "   Progress: %d/%d\r" "$i" "$ITERATIONS"
            fi
        done

        echo "   ✅ Complete ($ITERATIONS iterations)                    "
    done

    echo ""
    echo "### File Compilation Results — End-to-End"
    echo ""
    print_header
    for entry in "${BENCH_FILES[@]}"; do
        FILE="${entry%%:*}"
        LABEL="${entry##*:}"
        FILESTEM=$(basename "$FILE" .bt)
        TIMES_E2E="$RESULTS_DIR/file_e2e_${FILESTEM}.txt"
        if [ -f "$TIMES_E2E" ] && [ -s "$TIMES_E2E" ]; then
            print_result "$LABEL" "$TIMES_E2E"
        fi
    done
    echo ""

    echo "### File Compilation Results — BEAM Phase (.core → .beam)"
    echo ""
    echo "Measured independently using \`erlc +from_core\` on pre-generated .core files."
    echo "Rust phase ≈ End-to-End minus BEAM phase (process startup is included in E2E)."
    echo ""
    print_header
    for entry in "${BENCH_FILES[@]}"; do
        FILE="${entry%%:*}"
        LABEL="${entry##*:}"
        FILESTEM=$(basename "$FILE" .bt)
        TIMES_BEAM="$RESULTS_DIR/file_beam_${FILESTEM}.txt"
        if [ -f "$TIMES_BEAM" ] && [ -s "$TIMES_BEAM" ]; then
            print_result "$LABEL (BEAM)" "$TIMES_BEAM"
        fi
    done
    echo ""
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Benchmark 2: REPL Expression Compilation
# ═══════════════════════════════════════════════════════════════════════════════

if [ "$RUN_REPL" = true ]; then
    echo "═══════════════════════════════════════════════════════════════════"
    echo " REPL Expression Benchmarks"
    echo "═══════════════════════════════════════════════════════════════════"
    echo ""

    # Start REPL workspace on ephemeral port
    echo "🚀 Starting REPL workspace..."
    OUTFILE=$(mktemp)
    "$BEAMTALK" repl --port 0 > "$OUTFILE" 2>&1 &
    REPL_PID=$!

    # Wait for REPL to be ready
    REPL_PORT=""
    for i in $(seq 1 30); do
        REPL_PORT=$(grep -oP 'Connected to REPL backend on port \K[0-9]+' "$OUTFILE" 2>/dev/null || true)
        if [ -n "$REPL_PORT" ]; then break; fi
        sleep 1
    done
    rm -f "$OUTFILE"

    if [ -z "$REPL_PORT" ]; then
        echo "❌ REPL failed to start (no port detected)"
        REPL_PID=""
        exit 1
    fi

    echo "   REPL running on port $REPL_PORT (pid $REPL_PID)"
    echo ""

    # Helper: Send eval request via TCP and measure round-trip time
    # Uses a small Python script for reliable TCP communication
    repl_eval() {
        local code=$1
        local msg_id=$2
        local escaped_code
        escaped_code=$(printf '%s' "$code" | sed "s/'/\\\\'/g")
        python3 -c "
import socket, json, time

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.settimeout(30)
sock.connect(('127.0.0.1', $REPL_PORT))

request = json.dumps({'op': 'eval', 'id': '$msg_id', 'code': '$escaped_code'})
sock.sendall((request + '\n').encode())

# Read response (newline-delimited)
data = b''
while True:
    chunk = sock.recv(4096)
    if not chunk:
        break
    data += chunk
    if b'\n' in data:
        break

sock.close()
resp = json.loads(data.decode().strip())
print(json.dumps(resp))
" 2>/dev/null
    }

    # Warmup: send a few eval requests to warm up the JIT and caches
    echo "🔥 Warming up REPL (5 iterations)..."
    for i in $(seq 1 5); do
        repl_eval "1 + 1" "warmup-$i" >/dev/null 2>&1 || true
    done

    # REPL Benchmark expressions
    REPL_BENCHMARKS=(
        "1 + 2:Minimal expression (1 + 2)"
        "100 factorial:Factorial computation"
    )

    for entry in "${REPL_BENCHMARKS[@]}"; do
        CODE="${entry%%:*}"
        LABEL="${entry##*:}"

        echo "📊 Benchmarking: $LABEL"
        echo "   Expression: $CODE"

        TIMES="$RESULTS_DIR/repl_$(echo "$LABEL" | tr ' ()' '___').txt"
        : > "$TIMES"

        for i in $(seq 1 "$ITERATIONS"); do
            start=$(time_ns)
            repl_eval "$CODE" "bench-$i" >/dev/null 2>&1
            end=$(time_ns)

            elapsed_ms "$start" "$end" >> "$TIMES"

            if (( i % 10 == 0 )); then
                printf "   Progress: %d/%d\r" "$i" "$ITERATIONS"
            fi
        done

        echo "   ✅ Complete ($ITERATIONS iterations)                    "
    done

    # REPL with fixture load: Counter spawn
    echo "📊 Benchmarking: Counter spawn (with fixture load)"
    echo "   Loading fixture first..."

    # Load the counter fixture once
    python3 -c "
import socket, json

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.settimeout(30)
sock.connect(('127.0.0.1', $REPL_PORT))

request = json.dumps({'op': 'load-file', 'id': 'load-1', 'path': 'examples/counter.bt'})
sock.sendall((request + '\n').encode())

data = b''
while True:
    chunk = sock.recv(4096)
    if not chunk:
        break
    data += chunk
    if b'\n' in data:
        break

sock.close()
print(json.loads(data.decode().strip()).get('value', 'loaded'))
" 2>/dev/null || echo "   ⚠️  Failed to load counter fixture"

    TIMES="$RESULTS_DIR/repl_counter_spawn.txt"
    : > "$TIMES"

    for i in $(seq 1 "$ITERATIONS"); do
        start=$(time_ns)
        repl_eval "Counter spawn" "spawn-$i" >/dev/null 2>&1
        end=$(time_ns)

        elapsed_ms "$start" "$end" >> "$TIMES"

        if (( i % 10 == 0 )); then
            printf "   Progress: %d/%d\r" "$i" "$ITERATIONS"
        fi
    done
    echo "   ✅ Complete ($ITERATIONS iterations)                    "

    # Stop REPL
    echo ""
    echo "🛑 Stopping REPL..."
    kill "$REPL_PID" 2>/dev/null || true
    wait "$REPL_PID" 2>/dev/null || true
    REPL_PID=""

    echo ""
    echo "### REPL Expression Results"
    echo ""
    print_header
    for entry in "${REPL_BENCHMARKS[@]}"; do
        LABEL="${entry##*:}"
        TIMES="$RESULTS_DIR/repl_$(echo "$LABEL" | tr ' ()' '___').txt"
        if [ -f "$TIMES" ] && [ -s "$TIMES" ]; then
            print_result "$LABEL" "$TIMES"
        fi
    done
    TIMES="$RESULTS_DIR/repl_counter_spawn.txt"
    if [ -f "$TIMES" ] && [ -s "$TIMES" ]; then
        print_result "Counter spawn (fixture loaded)" "$TIMES"
    fi
    echo ""
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════════════

echo "═══════════════════════════════════════════════════════════════════"
echo " Benchmark Complete"
echo "═══════════════════════════════════════════════════════════════════"
echo ""
echo "To save results:"
echo "  scripts/bench-compilation.sh 2>&1 | tee docs/development/perf-baseline-daemon.md"
echo ""
echo "Raw data in: $RESULTS_DIR (cleaned up on exit)"
