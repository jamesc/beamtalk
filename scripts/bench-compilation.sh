#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Benchmark compilation latency for the Beamtalk compiler.
#
# Measures end-to-end compilation latency for file builds and REPL expressions,
# broken down by phase where possible. Connects to the REPL workspace via
# WebSocket (RFC 6455) using a minimal zero-dependency Python client.
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
BENCH_WS_NAME=""
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
    if [ -n "$BENCH_WS_NAME" ]; then
        "$BEAMTALK" workspace stop "$BENCH_WS_NAME" 2>/dev/null || true
    fi
    rm -rf "$RESULTS_DIR"
}
trap cleanup EXIT

# Get high-resolution time in nanoseconds
time_ns() {
    if date +%s%N | grep -q 'N$' 2>/dev/null; then
        # macOS/BSD: %N not supported, fall back to python3
        python3 -c 'import time; print(time.time_ns())'
    else
        date +%s%N
    fi
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
        "examples/getting-started/src/counter.bt:Counter (minimal)"
        "examples/chat-room/src/chat_member.bt:ChatMember (typical)"
        "examples/chat-room/src/chat_room.bt:ChatRoom (multi-actor)"
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

        # Generate .core file once to verify compilation works
        rm -rf "$BUILDDIR" 2>/dev/null || true
        if ! "$BEAMTALK" build "$FILEPATH" >/dev/null 2>&1; then
            echo "   ❌ beamtalk build failed for $FILE — skipping"
            continue
        fi
        CORE_FILE=$(find "$BUILDDIR" -name "*.core" -type f 2>/dev/null | head -1)
        if [ -z "$CORE_FILE" ]; then
            echo "   ❌ No .core file generated for $FILE — skipping"
            continue
        fi

        for i in $(seq 1 "$ITERATIONS"); do
            rm -rf "$BUILDDIR" 2>/dev/null || true

            # Measure end-to-end (Rust binary startup + compile + BEAM)
            start=$(time_ns)
            if ! "$BEAMTALK" build "$FILEPATH" >/dev/null 2>&1; then
                echo "   ❌ beamtalk build failed for $FILE"
                exit 1
            fi
            end=$(time_ns)
            elapsed_ms "$start" "$end" >> "$TIMES_E2E"

            # Measure BEAM phase only (.core → .beam via erlc)
            # Re-find core file since build dir was recreated
            CORE_FILE=$(find "$BUILDDIR" -name "*.core" -type f 2>/dev/null | head -1)
            if [ -n "$CORE_FILE" ]; then
                # Remove .beam so erlc does real work
                rm -f "$BUILDDIR"/*.beam 2>/dev/null || true
                start=$(time_ns)
                if ! erlc +from_core +debug_info -o "$BUILDDIR" "$CORE_FILE" >/dev/null 2>&1; then
                    echo "   ❌ erlc failed for $CORE_FILE"
                    exit 1
                fi
                end=$(time_ns)
                elapsed_ms "$start" "$end" >> "$TIMES_BEAM"
            else
                echo "   ❌ No .core file generated for $FILE"
                exit 1
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

    # Start a dedicated benchmark workspace in the background.
    # Using `workspace create --background` avoids the TTY requirement of
    # `beamtalk repl` and gives us a clean port number on stdout.
    BENCH_WS_NAME="bench-$$"
    echo "🚀 Starting benchmark workspace ($BENCH_WS_NAME)..."
    WS_OUTPUT=$("$BEAMTALK" workspace create "$BENCH_WS_NAME" --background --port 0 2>&1)
    REPL_PORT=$(echo "$WS_OUTPUT" | sed -n 's/.*Port:[[:space:]]*\([0-9][0-9]*\).*/\1/p')

    if [ -z "$REPL_PORT" ]; then
        echo "❌ Workspace failed to start"
        echo "$WS_OUTPUT"
        exit 1
    fi

    # Read the workspace cookie for WebSocket authentication.
    # Prefer the cookie file over parsing process listings (portable, reliable).
    COOKIE_FILE="${HOME}/.beamtalk/workspaces/${BENCH_WS_NAME}/cookie"
    if [ ! -f "$COOKIE_FILE" ]; then
        echo "❌ Failed to find workspace cookie file: $COOKIE_FILE"
        exit 1
    fi
    REPL_COOKIE="$(tr -d '[:space:]' < "$COOKIE_FILE")"
    if [ -z "$REPL_COOKIE" ]; then
        echo "❌ Workspace cookie file is empty: $COOKIE_FILE"
        exit 1
    fi

    echo "   Workspace running on port $REPL_PORT"
    echo ""

    # Generate a Python helper script that opens a persistent WebSocket
    # connection and runs all REPL benchmarks. A single long-lived connection
    # avoids per-request connection overhead and matches how real REPL
    # sessions work. The script communicates results back via a JSON-lines
    # output file.
    WS_BENCH_SCRIPT="$RESULTS_DIR/ws_bench.py"
    cat > "$WS_BENCH_SCRIPT" << 'PYEOF'
#!/usr/bin/env python3
"""WebSocket-based REPL benchmark runner.

Connects once, authenticates, warms up, then runs each benchmark
expression for the requested number of iterations, writing per-iteration
latency (in ms) to separate output files.
"""
import json, os, socket, struct, sys, time, base64

# ── Configuration from environment ──────────────────────────────────────
PORT       = int(os.environ["BENCH_PORT"])
COOKIE     = os.environ.get("BENCH_COOKIE", "")
ITERATIONS = int(os.environ["BENCH_ITERATIONS"])
RESULTS    = os.environ["BENCH_RESULTS_DIR"]
WARMUP     = int(os.environ.get("BENCH_WARMUP", "10"))

# ── Minimal WebSocket client (RFC 6455) ─────────────────────────────────
# We avoid external dependencies (websocket-client) so the script works
# on any system with Python 3.
class SimpleWebSocket:
    """Blocking WebSocket client — just enough for JSON text frames."""

    def __init__(self, host, port, path="/ws"):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.settimeout(30)
        self.sock.connect((host, port))
        # Perform HTTP upgrade handshake
        key = base64.b64encode(os.urandom(16)).decode()
        req = (
            f"GET {path} HTTP/1.1\r\n"
            f"Host: {host}:{port}\r\n"
            f"Upgrade: websocket\r\n"
            f"Connection: Upgrade\r\n"
            f"Sec-WebSocket-Key: {key}\r\n"
            f"Sec-WebSocket-Version: 13\r\n"
            f"\r\n"
        )
        self.sock.sendall(req.encode())
        # Read HTTP response headers (ends with \r\n\r\n)
        hdr = b""
        while b"\r\n\r\n" not in hdr:
            hdr += self.sock.recv(4096)
        if b"101" not in hdr.split(b"\r\n")[0]:
            raise RuntimeError(f"WebSocket upgrade failed: {hdr.decode()}")
        self._buf = hdr.split(b"\r\n\r\n", 1)[1]  # leftover after headers

    def _recv_exact(self, n):
        while len(self._buf) < n:
            self._buf += self.sock.recv(max(4096, n - len(self._buf)))
        data, self._buf = self._buf[:n], self._buf[n:]
        return data

    def recv(self):
        """Receive one text frame and return the payload string."""
        while True:
            b0, b1 = struct.unpack("!BB", self._recv_exact(2))
            masked = b1 & 0x80
            length = b1 & 0x7F
            if length == 126:
                length = struct.unpack("!H", self._recv_exact(2))[0]
            elif length == 127:
                length = struct.unpack("!Q", self._recv_exact(8))[0]
            if masked:
                mask = self._recv_exact(4)
            payload = self._recv_exact(length)
            if masked:
                payload = bytes(payload[i] ^ mask[i % 4] for i in range(length))
            opcode = b0 & 0x0F
            if opcode == 0x1:  # text
                return payload.decode()
            if opcode == 0x8:  # close
                raise RuntimeError("Server closed WebSocket")
            if opcode == 0x9:  # ping → pong (echo raw payload per RFC 6455)
                self.send(payload, opcode=0xA)
            # Skip other frames (continuation, binary, pong)

    def send(self, data, opcode=0x1):
        """Send a masked frame (clients MUST mask)."""
        payload = data if isinstance(data, bytes) else data.encode()
        mask = os.urandom(4)
        masked = bytes(payload[i] ^ mask[i % 4] for i in range(len(payload)))
        header = bytes([0x80 | opcode])
        plen = len(payload)
        if plen < 126:
            header += bytes([0x80 | plen])
        elif plen < 65536:
            header += bytes([0x80 | 126]) + struct.pack("!H", plen)
        else:
            header += bytes([0x80 | 127]) + struct.pack("!Q", plen)
        self.sock.sendall(header + mask + masked)

    def close(self):
        try:
            self.send("", opcode=0x8)
        except Exception:
            pass
        self.sock.close()

# ── Connect and authenticate ────────────────────────────────────────────
ws = SimpleWebSocket("127.0.0.1", PORT)

if not COOKIE:
    print("Missing REPL auth cookie; cannot run benchmarks. "
          "Ensure the workspace is running and BENCH_COOKIE is set.",
          file=sys.stderr)
    sys.exit(1)

# Protocol handshake: read auth-required → send auth → read auth_ok → read session-started
resp = json.loads(ws.recv())
if resp.get("op") != "auth-required":
    print(f"Expected auth-required, got: {resp}", file=sys.stderr)
    sys.exit(1)

ws.send(json.dumps({"type": "auth", "cookie": COOKIE}))

resp = json.loads(ws.recv())
if resp.get("type") == "auth_error":
    print(f"Auth failed: {resp}", file=sys.stderr)
    sys.exit(1)
if resp.get("type") != "auth_ok":
    print(f"Expected auth_ok, got: {resp}", file=sys.stderr)
    sys.exit(1)

resp = json.loads(ws.recv())
if resp.get("op") != "session-started":
    print(f"Expected session-started, got: {resp}", file=sys.stderr)
    sys.exit(1)

def is_result(msg):
    """Return True if this message is the final eval/load result.

    Eval results look like: {"id":"t1","status":["done"],"value":3}
    Load results: {"id":"load-1","status":["done"],...}
    Errors: {"id":"load-1","status":["done","error"],"error":"..."}
    We match on status containing "done" to catch all terminal messages.
    """
    status = msg.get("status", [])
    return isinstance(status, list) and "done" in status

# ── Warmup ──────────────────────────────────────────────────────────────
for i in range(WARMUP):
    ws.send(json.dumps({"op": "eval", "id": f"warmup-{i}", "code": "1 + 1"}))
    while True:
        msg = json.loads(ws.recv())
        if is_result(msg):
            break

print("warmup_done", flush=True)

# ── Benchmarks ──────────────────────────────────────────────────────────
benchmarks = [
    ("1 + 2",          "Minimal expression (1 + 2)",  "repl_Minimal_expression__1___2_"),
    ("100 factorial",  "Factorial computation",        "repl_Factorial_computation"),
]

for code, label, filename in benchmarks:
    times_path = os.path.join(RESULTS, filename + ".txt")
    with open(times_path, "w") as f:
        for i in range(ITERATIONS):
            req = json.dumps({"op": "eval", "id": f"bench-{i}", "code": code})
            t0 = time.monotonic()
            ws.send(req)
            while True:
                msg = json.loads(ws.recv())
                if is_result(msg):
                    break
            t1 = time.monotonic()
            f.write(f"{(t1 - t0) * 1000:.3f}\n")
    print(f"done:{filename}:{label}", flush=True)

# ── Counter spawn benchmark (load fixture first) ───────────────────────
PROJECT = os.environ.get("BENCH_PROJECT_ROOT", ".")
ws.send(json.dumps({"op": "load-file", "id": "load-1",
                     "path": os.path.join(PROJECT, "examples/getting-started/src/counter.bt")}))
while True:
    msg = json.loads(ws.recv())
    if is_result(msg):
        if "error" in msg.get("status", []):
            print(f"Fixture load failed: {msg.get('error', 'unknown')}",
                  file=sys.stderr)
            sys.exit(1)
        break
print("fixture_loaded", flush=True)

filename = "repl_counter_spawn"
times_path = os.path.join(RESULTS, filename + ".txt")
with open(times_path, "w") as f:
    for i in range(ITERATIONS):
        req = json.dumps({"op": "eval", "id": f"spawn-{i}",
                           "code": "Counter spawn"})
        t0 = time.monotonic()
        ws.send(req)
        while True:
            msg = json.loads(ws.recv())
            if is_result(msg):
                break
        t1 = time.monotonic()
        f.write(f"{(t1 - t0) * 1000:.3f}\n")
print(f"done:{filename}:Counter spawn (fixture loaded)", flush=True)

ws.close()
PYEOF

    # Run the WebSocket benchmark script, printing progress as it goes
    BENCH_PORT="$REPL_PORT" \
    BENCH_COOKIE="$REPL_COOKIE" \
    BENCH_ITERATIONS="$ITERATIONS" \
    BENCH_RESULTS_DIR="$RESULTS_DIR" \
    BENCH_PROJECT_ROOT="$PROJECT_ROOT" \
    BENCH_WARMUP=10 \
    python3 "$WS_BENCH_SCRIPT" | while IFS= read -r line; do
        case "$line" in
            warmup_done)
                echo "🔥 Warmup complete (10 iterations)" ;;
            fixture_loaded)
                echo "   Loaded Counter fixture" ;;
            done:*)
                label="${line##*:}"
                echo "   ✅ $label ($ITERATIONS iterations)" ;;
            *)
                echo "   $line" ;;
        esac
    done

    # Stop workspace
    echo ""
    echo "🛑 Stopping workspace..."
    "$BEAMTALK" workspace stop "$BENCH_WS_NAME" 2>/dev/null || true
    BENCH_WS_NAME=""

    echo ""
    echo "### REPL Expression Results"
    echo ""
    print_header
    REPL_BENCHMARKS=(
        "Minimal expression (1 + 2):repl_Minimal_expression__1___2_"
        "Factorial computation:repl_Factorial_computation"
        "Counter spawn (fixture loaded):repl_counter_spawn"
    )
    for entry in "${REPL_BENCHMARKS[@]}"; do
        LABEL="${entry%%:*}"
        FNAME="${entry##*:}"
        TIMES="$RESULTS_DIR/${FNAME}.txt"
        if [ -f "$TIMES" ] && [ -s "$TIMES" ]; then
            print_result "$LABEL" "$TIMES"
        fi
    done
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
