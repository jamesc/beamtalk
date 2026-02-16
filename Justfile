# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Beamtalk build tasks
# Run `just` to see all available recipes
# Run `just <recipe>` to execute a specific task

# Use bash on Unix and PowerShell on Windows for all commands
set shell := ["bash", "-uc"]
set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

# Default recipe (list all tasks)
default:
    @just --list

# ═══════════════════════════════════════════════════════════════════════════
# Quick Commands (CI equivalents)
# ═══════════════════════════════════════════════════════════════════════════

# Run local CI checks (build, lint, unit, integration & E2E tests)
ci: build lint test test-stdlib test-integration test-mcp test-e2e

# Clean all build artifacts (Rust, Erlang, VS Code, caches, examples)
clean: clean-rust clean-erlang clean-vscode
    @rm -rf runtime/_build 2>/dev/null || true
    @rm -rf target/llvm-cov 2>/dev/null || true
    @rm -rf examples/build 2>/dev/null || true
    @echo "✅ All build artifacts cleaned"

# ═══════════════════════════════════════════════════════════════════════════
# Build Tasks
# ═══════════════════════════════════════════════════════════════════════════

# Build all targets (Rust + Erlang + stdlib)
build: build-stdlib

# Build Rust workspace
build-rust:
    @echo "🔨 Building Rust workspace..."
    @cargo build --all-targets --quiet
    @echo "✅ Rust build complete"

# Build in release mode (Rust + Erlang)
build-release: build-rust-release build-erlang

# Build Rust in release mode
build-rust-release:
    @echo "🔨 Building Rust workspace (release)..."
    @cargo build --all-targets --release --quiet
    @echo "✅ Rust release build complete"

# Build the LSP server
build-lsp:
    @echo "🔨 Building LSP server..."
    @cargo build -p beamtalk-lsp --quiet
    @echo "✅ LSP server built: target/debug/beamtalk-lsp"

# Build Erlang runtime
build-erlang:
    @echo "🔨 Building Erlang runtime..."
    @cd runtime && rebar3 compile 2>&1 | grep -v "===>" || true
    @echo "✅ Erlang build complete"

# Build standard library (lib/*.bt → BEAM, incremental — skips if up to date)
build-stdlib: build-rust build-erlang
    @echo "🔨 Building standard library..."
    @cargo run --bin beamtalk --quiet -- build-stdlib
    @echo "✅ Stdlib build complete"

# ═══════════════════════════════════════════════════════════════════════════
# Lint and Format
# ═══════════════════════════════════════════════════════════════════════════

# Run all linting and formatting checks
lint: clippy fmt-check dialyzer

# Run clippy (Rust linter) - warnings are errors
clippy:
    @echo "🔍 Running clippy..."
    @cargo clippy --all-targets --quiet -- -D warnings
    @echo "✅ Clippy passed"

# Check Rust code formatting
fmt-check:
    @echo "📋 Checking Rust formatting..."
    cargo fmt --all -- --check

# Format all Rust code
fmt:
    @echo "✨ Formatting Rust code..."
    cargo fmt --all

# Run Dialyzer on Erlang runtime
dialyzer:
    @echo "🔬 Running Dialyzer type checking..."
    cd runtime && rebar3 dialyzer

# ═══════════════════════════════════════════════════════════════════════════
# Testing
# ═══════════════════════════════════════════════════════════════════════════

# Run fast tests (Rust unit/integration + stdlib + Erlang runtime, skip slow E2E)
test: test-rust test-stdlib test-runtime

# Run Rust tests (unit + integration, skip slow E2E)
test-rust:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "🧪 Running Rust tests (fast)..."
    cargo test --all-targets 2>&1 | awk '/Running.*\(/ { s=$0; sub(/.*Running /, "", s); sub(/unittests /, "", s); split(s, b, / \(/); src=b[1]; crate=b[2]; gsub(/.*\//, "", crate); sub(/-[a-f0-9]+\)$/, "", crate); label=crate "::" src } /^test result:/ { sub(/^test result: ok\. /, ""); printf "  %-45s %s\n", label, $0 } /^warning:/ { print } /^test result: FAILED/ { failed=1 } END { if (failed) exit 1 }'
    echo "✅ Rust tests complete"

# Run E2E tests (slow - full pipeline, ~50s)
test-e2e: build-stdlib _clean-daemon-state
    @echo "🧪 Running E2E tests (slow - ~50s)..."
    cargo test --test e2e -- --ignored

# Run workspace integration tests (requires Erlang/OTP runtime, ~10s)
test-integration: build-stdlib
    @echo "🧪 Running workspace integration tests..."
    cargo test --bin beamtalk -- --ignored --test-threads=1
    @echo "✅ Integration tests complete"

# Run MCP server integration tests (starts REPL, runs tests, tears down, ~15s)
test-mcp: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "🧪 Running MCP server integration tests..."

    # Start a REPL workspace on ephemeral port, capture stdout to discover it
    OUTFILE=$(mktemp)
    ./target/debug/beamtalk repl --port 0 > "$OUTFILE" 2>&1 &
    REPL_PID=$!

    # Ensure cleanup on exit
    cleanup() {
        kill "$REPL_PID" 2>/dev/null || true
        wait "$REPL_PID" 2>/dev/null || true
        rm -f "$OUTFILE"
    }
    trap cleanup EXIT

    # Wait for "Connected to REPL backend on port <N>" line
    MCP_TEST_PORT=""
    for i in $(seq 1 30); do
        MCP_TEST_PORT=$(grep -oP 'Connected to REPL backend on port \K[0-9]+' "$OUTFILE" 2>/dev/null || true)
        if [ -n "$MCP_TEST_PORT" ]; then break; fi
        sleep 1
    done

    if [ -z "$MCP_TEST_PORT" ]; then
        echo "❌ REPL failed to start (no port detected)"
        cat "$OUTFILE"
        exit 1
    fi

    echo "  REPL running on port $MCP_TEST_PORT (pid $REPL_PID)"

    # Run the MCP integration tests with discovered port
    BEAMTALK_TEST_PORT="$MCP_TEST_PORT" cargo test -p beamtalk-mcp -- --ignored --test-threads=1

    echo "✅ MCP integration tests complete"

# Run ALL tests (unit + integration + E2E + Erlang runtime)
test-all: test-rust test-stdlib test-integration test-mcp test-e2e test-runtime

# Smoke test installed layout (install to temp dir, verify REPL starts)
test-install: build-release build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "🧪 Smoke-testing installed layout..."
    TMPDIR=$(mktemp -d)
    OUTFILE=$(mktemp)

    just install "$TMPDIR"

    # Start REPL on ephemeral port, capture stdout to discover actual port
    "$TMPDIR/bin/beamtalk" repl --port 0 > "$OUTFILE" 2>&1 &
    REPL_PID=$!
    cleanup() { kill $REPL_PID 2>/dev/null || true; wait $REPL_PID 2>/dev/null || true; rm -rf "$TMPDIR" "$OUTFILE"; }
    trap cleanup EXIT

    # Wait for "Connected to REPL backend on port <N>" line
    PORT=""
    for i in $(seq 1 30); do
        PORT=$(grep -oP 'Connected to REPL backend on port \K[0-9]+' "$OUTFILE" 2>/dev/null || true)
        if [ -n "$PORT" ]; then break; fi
        sleep 1
    done

    if [ -z "$PORT" ]; then
        echo "❌ REPL failed to start (no port detected)"
        cat "$OUTFILE"
        exit 1
    fi

    # Evaluate 1 + 1 via TCP protocol
    RESPONSE=$(echo '{"op":"eval","id":"smoke","code":"1 + 1"}' | nc -w 5 127.0.0.1 "$PORT" || true)

    if echo "$RESPONSE" | grep -qE '"value":\s*"?2"?'; then
        echo "✅ Installed REPL evaluated 1 + 1 = 2 (port $PORT)"
    else
        echo "❌ Unexpected response: $RESPONSE"
        exit 1
    fi

# Run compiled stdlib tests (ADR 0014 Phase 1, ~14s)
test-stdlib: build-stdlib
    @echo "🧪 Running stdlib tests..."
    @cargo run --bin beamtalk --quiet -- test-stdlib
    @echo "✅ Stdlib tests complete"

# Clean up stale daemon state (internal helper)
_clean-daemon-state:
    @rm -f ~/.beamtalk/daemon.sock ~/.beamtalk/daemon.lock 2>/dev/null || true
    @rm -rf ~/.beamtalk/sessions/*/daemon.sock ~/.beamtalk/sessions/*/daemon.lock 2>/dev/null || true

# Run Erlang runtime unit tests
# Note: Auto-discovers all *_tests modules. New test files are included automatically.
test-runtime: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "🧪 Running Erlang runtime unit tests..."
    if OUTPUT=$(rebar3 eunit --app=beamtalk_runtime,beamtalk_workspace 2>&1); then
        echo "$OUTPUT" | grep -E "Finished in|[0-9]+ tests," || echo "✓ All tests passed"
    else
        echo "$OUTPUT"
        echo "❌ Runtime tests failed"
        exit 1
    fi

# Run performance benchmarks (separate from unit tests, ~30s)
perf: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "⏱️  Running performance benchmarks..."
    if OUTPUT=$(rebar3 eunit --dir=perf 2>&1); then
        echo "$OUTPUT" | grep -E "^PERF:|Finished in|tests,|FAILED" || true
        echo "✅ Performance benchmarks complete"
    else
        echo "$OUTPUT" | grep -E "^PERF:|Finished in|tests,|FAILED" || true
        echo "❌ Performance benchmarks failed"
        echo "$OUTPUT" | tail -20
        exit 1
    fi

# Run a specific Rust test by name
test-one TEST:
    @echo "🧪 Running test: {{TEST}}"
    cargo test --all-targets {{TEST}}

# Run fuzz testing on the parser for a configurable duration (default: 60 seconds)
fuzz DURATION="60":
    @echo "🔀 Fuzzing parser for {{DURATION}} seconds..."
    @echo "   Corpus: fuzz/corpus/parse_arbitrary/ (32 seed files)"
    @echo "   Target: parse_arbitrary (lexer + parser crash safety)"
    cargo +nightly fuzz run parse_arbitrary -- -rss_limit_mb=4096 -max_total_time={{DURATION}}
    @echo "✅ Fuzzing completed without crashes!"

# ═══════════════════════════════════════════════════════════════════════════
# Coverage
# ═══════════════════════════════════════════════════════════════════════════

# Generate coverage reports for both Rust and Erlang runtime
coverage: coverage-rust coverage-runtime
    @echo "✅ Coverage reports generated"
    @echo "  Rust:    target/llvm-cov/html/index.html"
    @echo "  Runtime: runtime/_build/test/cover/index.html"

# Generate Rust coverage (requires cargo-llvm-cov)
coverage-rust:
    @echo "📊 Generating Rust coverage..."
    cargo llvm-cov --all-targets --workspace --html
    @echo "  📁 HTML report: target/llvm-cov/html/index.html"

# Generate Erlang runtime coverage
# Note: Auto-discovers all *_tests modules. New test files are included automatically.
coverage-runtime:
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "📊 Generating Erlang runtime coverage..."
    if ! OUTPUT=$(rebar3 eunit --app=beamtalk_runtime,beamtalk_workspace --cover 2>&1); then
        echo "$OUTPUT"
        echo "❌ Runtime tests failed"
        exit 1
    fi
    echo "$OUTPUT" | grep -E "Finished in|[0-9]+ tests," || true
    rebar3 cover --verbose
    rebar3 covertool generate
    # Clean up covertool XML: remove empty phantom packages, shorten path-based names
    python3 ../scripts/clean-covertool-xml.py
    echo "  📁 HTML report: runtime/_build/test/cover/index.html"
    echo "  📁 XML reports: runtime/_build/test/covertool/*.covertool.xml"

# Collect E2E test coverage (runs E2E tests with Erlang cover instrumentation)
coverage-e2e: build-stdlib _clean-daemon-state
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📊 Running E2E tests with Erlang cover instrumentation..."
    echo "   (This is slower than normal E2E due to cover overhead)"
    # Allow test failures — coverdata is exported before BEAM shuts down
    E2E_COVER=1 cargo test --test e2e -- --ignored || true
    if [ -f runtime/_build/test/cover/e2e.coverdata ]; then
        SIZE=$(wc -c < runtime/_build/test/cover/e2e.coverdata)
        echo "  📁 Coverdata: runtime/_build/test/cover/e2e.coverdata (${SIZE} bytes)"
    else
        echo "⚠️  No E2E coverdata produced"
        exit 1
    fi

# Collect stdlib test coverage (runs stdlib tests with Erlang cover instrumentation)
coverage-stdlib: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📊 Running stdlib tests with Erlang cover instrumentation..."
    echo "   (This is slower than normal stdlib tests due to cover overhead)"
    STDLIB_COVER=1 cargo run --bin beamtalk --quiet -- test-stdlib || true
    if [ -f runtime/_build/test/cover/stdlib.coverdata ]; then
        SIZE=$(wc -c < runtime/_build/test/cover/stdlib.coverdata)
        echo "  📁 Coverdata: runtime/_build/test/cover/stdlib.coverdata (${SIZE} bytes)"
    else
        echo "⚠️  No stdlib coverdata produced"
        exit 1
    fi

# Generate combined Erlang coverage (eunit + E2E + stdlib)
# Runs eunit with --cover, then E2E with cover, then stdlib with cover, then merges all into one report.
coverage-all: coverage-runtime coverage-e2e coverage-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "📊 Merging eunit + E2E + stdlib coverage data..."
    # Merge all .coverdata files into eunit.coverdata so rebar3 covertool sees them
    # (covertool only imports eunit.coverdata, not e2e/stdlib coverdata)
    erl -noshell -eval '
        cover:start(),
        Files = filelib:wildcard("_build/test/cover/*.coverdata"),
        lists:foreach(fun(F) ->
            io:format("  Importing: ~s~n", [F]),
            cover:import(F)
        end, Files),
        ok = cover:export("_build/test/cover/eunit.coverdata"),
        io:format("  Merged ~p files into eunit.coverdata~n", [length(Files)]),
        cover:stop(),
        init:stop().
    '
    rebar3 cover --verbose
    rebar3 covertool generate
    python3 ../scripts/clean-covertool-xml.py
    echo "✅ Combined coverage report generated"
    echo "  📁 HTML report: runtime/_build/test/cover/index.html"
    echo "  📁 XML reports: runtime/_build/test/covertool/*.covertool.xml"

# Show Erlang coverage report from existing coverdata (no re-run)
coverage-report:
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    if ! ls _build/test/cover/*.coverdata >/dev/null 2>&1; then
        echo "❌ No coverdata found. Run 'just coverage-runtime' or 'just coverage-e2e' first."
        exit 1
    fi
    echo "📊 Coverage report from existing data:"
    ls _build/test/cover/*.coverdata | sed 's|^|  📁 |'
    echo ""
    rebar3 cover --verbose

# Open Rust coverage report in browser
coverage-open:
    #!/usr/bin/env bash
    echo "🌐 Opening Rust coverage report..."
    if [ -n "${BROWSER-}" ]; then
        "$BROWSER" target/llvm-cov/html/index.html
    elif command -v xdg-open >/dev/null 2>&1; then
        xdg-open target/llvm-cov/html/index.html
    elif command -v open >/dev/null 2>&1; then
        open target/llvm-cov/html/index.html
    else
        echo "❌ No browser found. Set BROWSER env var or install xdg-open/open"
        echo "   Report: target/llvm-cov/html/index.html"
    fi

# Open Erlang runtime coverage report in browser
coverage-runtime-open:
    #!/usr/bin/env bash
    echo "🌐 Opening Erlang coverage report..."
    if [ -n "${BROWSER-}" ]; then
        "$BROWSER" runtime/_build/test/cover/index.html
    elif command -v xdg-open >/dev/null 2>&1; then
        xdg-open runtime/_build/test/cover/index.html
    elif command -v open >/dev/null 2>&1; then
        open runtime/_build/test/cover/index.html
    else
        echo "❌ No browser found. Set BROWSER env var or install xdg-open/open"
        echo "   Report: runtime/_build/test/cover/index.html"
    fi

# ═══════════════════════════════════════════════════════════════════════════
# Clean Tasks
# ═══════════════════════════════════════════════════════════════════════════

# Clean Rust build artifacts (works with Docker volume mounts)
clean-rust:
    @echo "🧹 Cleaning Rust artifacts..."
    @if [ -d target ]; then find target -mindepth 1 -maxdepth 1 -exec rm -rf {} +; fi 2>/dev/null || true
    @echo "  ✅ Cleaned target/"

# Clean Erlang build artifacts
clean-erlang:
    @echo "🧹 Cleaning Erlang artifacts..."
    cd runtime && rebar3 clean
    @echo "  ✅ Cleaned runtime/_build/"

# Clean VS Code extension build artifacts
clean-vscode:
    @echo "🧹 Cleaning VS Code extension artifacts..."
    @rm -rf editors/vscode/out 2>/dev/null || true
    @rm -rf editors/vscode/node_modules 2>/dev/null || true
    @echo "  ✅ Cleaned editors/vscode/{out,node_modules}/"

# Purge global Cargo cache (affects all Rust projects!)
purge-cargo-cache:
    @echo "⚠️  This will delete ~/.cargo/registry/cache (affects all Rust projects)"
    @echo "Press Enter to continue or Ctrl+C to cancel..."
    @read _
    @rm -rf ~/.cargo/registry/cache 2>/dev/null || true
    @echo "  ✅ Cargo cache purged"

# ═══════════════════════════════════════════════════════════════════════════
# Development
# ═══════════════════════════════════════════════════════════════════════════

# Start the REPL (builds Rust first if needed)
repl: build-stdlib
    @echo "🚀 Starting Beamtalk REPL..."
    cargo run --bin beamtalk -- repl

# Start the compiler daemon
daemon-start: build-rust
    @echo "🚀 Starting compiler daemon..."
    cargo run --bin beamtalk -- daemon start

# Stop the compiler daemon
daemon-stop:
    @echo "🛑 Stopping compiler daemon..."
    cargo run --bin beamtalk -- daemon stop

# Check daemon status
daemon-status:
    @cargo run --bin beamtalk -- daemon status

# Stop the current project's workspace
workspace-stop:
    #!/usr/bin/env bash
    set -uo pipefail
    if ! STATUS_OUT=$(cargo run --bin beamtalk --quiet -- workspace status 2>&1); then
        echo "No running workspace found for this project."
        exit 0
    fi
    WS_ID=$(echo "$STATUS_OUT" | head -1 | awk '{print $2}')
    if [ -n "$WS_ID" ]; then
        cargo run --bin beamtalk --quiet -- workspace stop "$WS_ID" 2>&1 || echo "Workspace $WS_ID is not running."
    else
        echo "No running workspace found for this project."
    fi

# Show workspace status
workspace-status:
    @cargo run --bin beamtalk --quiet -- workspace status

# List all workspaces
workspace-list:
    @cargo run --bin beamtalk --quiet -- workspace list

# Run a Beamtalk file
run FILE: build-rust
    @echo "🚀 Running {{FILE}}..."
    cargo run --bin beamtalk -- run {{FILE}}

# Build a Beamtalk file (compile to .core and .beam)
compile FILE: build-rust
    @echo "🔨 Compiling {{FILE}}..."
    cargo run --bin beamtalk -- build {{FILE}}

# Watch for changes and run tests (requires cargo-watch)
watch:
    cargo watch -x 'test --all-targets'

# ═══════════════════════════════════════════════════════════════════════════
# Dependencies
# ═══════════════════════════════════════════════════════════════════════════

# Install development tools
install-tools:
    @echo "📦 Installing development tools..."
    @command -v cargo-llvm-cov >/dev/null 2>&1 || cargo install cargo-llvm-cov
    @command -v cargo-watch >/dev/null 2>&1 || cargo install cargo-watch
    @command -v just >/dev/null 2>&1 || cargo install just
    @echo "✅ Tools installed"

# Check for required tools
check-tools:
    @echo "🔍 Checking for required tools..."
    @command -v cargo >/dev/null 2>&1 || (echo "❌ cargo not found" && exit 1)
    @command -v rustc >/dev/null 2>&1 || (echo "❌ rustc not found" && exit 1)
    @command -v erl >/dev/null 2>&1 || (echo "❌ erl not found" && exit 1)
    @command -v rebar3 >/dev/null 2>&1 || (echo "❌ rebar3 not found" && exit 1)
    @command -v node >/dev/null 2>&1 || (echo "❌ node not found (needed for VS Code extension)" && exit 1)
    @command -v npm >/dev/null 2>&1 || (echo "❌ npm not found (needed for VS Code extension)" && exit 1)
    @command -v npx >/dev/null 2>&1 || (echo "❌ npx not found (needed for VS Code extension)" && exit 1)
    @echo "✅ All required tools found"

# ═══════════════════════════════════════════════════════════════════════════
# Release & Installation
# ═══════════════════════════════════════════════════════════════════════════

# Prepare for release (run all checks)
pre-release: clean ci coverage
    @echo "✅ Pre-release checks passed"

# Install beamtalk to PREFIX (default: /usr/local)
install PREFIX="/usr/local": build-release build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    PREFIX="{{PREFIX}}"
    echo "📦 Installing beamtalk to ${PREFIX}..."

    # Validate build artifacts exist
    if [ ! -f target/release/beamtalk ]; then
        echo "❌ Release binary not found. Run 'just build-release' first."
        exit 1
    fi
    if [ ! -f target/release/beamtalk-compiler-port ]; then
        echo "❌ Compiler port binary not found. Run 'just build-release' first."
        exit 1
    fi
    if [ ! -f target/release/beamtalk-lsp ]; then
        echo "❌ LSP server binary not found. Run 'just build-release' first."
        exit 1
    fi
    if [ ! -f target/release/beamtalk-mcp ]; then
        echo "❌ MCP server binary not found. Run 'just build-release' first."
        exit 1
    fi

    # Binaries
    install -d "${PREFIX}/bin"
    install -m 755 target/release/beamtalk "${PREFIX}/bin/beamtalk"
    install -m 755 target/release/beamtalk-compiler-port "${PREFIX}/bin/beamtalk-compiler-port"
    install -m 755 target/release/beamtalk-lsp "${PREFIX}/bin/beamtalk-lsp"
    install -m 755 target/release/beamtalk-mcp "${PREFIX}/bin/beamtalk-mcp"

    # OTP application ebin directories
    for app in beamtalk_runtime beamtalk_workspace beamtalk_compiler jsx; do
        SRC="runtime/_build/default/lib/${app}/ebin"
        if ! ls "${SRC}"/*.beam 1>/dev/null 2>&1; then
            echo "❌ No .beam files found in ${SRC}. Run 'just build-erlang' first."
            exit 1
        fi
        install -d "${PREFIX}/lib/beamtalk/lib/${app}/ebin"
        install -m 644 "${SRC}"/*.beam "${PREFIX}/lib/beamtalk/lib/${app}/ebin/"
        # Copy .app file if present
        if ls "${SRC}"/*.app 1>/dev/null 2>&1; then
            install -m 644 "${SRC}"/*.app "${PREFIX}/lib/beamtalk/lib/${app}/ebin/"
        fi
    done

    # Stdlib (built under apps/, not _build/)
    STDLIB_SRC="runtime/apps/beamtalk_stdlib/ebin"
    if ! ls "${STDLIB_SRC}"/*.beam 1>/dev/null 2>&1; then
        echo "❌ No stdlib .beam files found. Run 'just build-stdlib' first."
        exit 1
    fi
    install -d "${PREFIX}/lib/beamtalk/lib/beamtalk_stdlib/ebin"
    install -m 644 "${STDLIB_SRC}"/*.beam "${PREFIX}/lib/beamtalk/lib/beamtalk_stdlib/ebin/"
    if ls "${STDLIB_SRC}"/*.app 1>/dev/null 2>&1; then
        install -m 644 "${STDLIB_SRC}"/*.app "${PREFIX}/lib/beamtalk/lib/beamtalk_stdlib/ebin/"
    fi

    echo "✅ Installed beamtalk to ${PREFIX}"
    echo "   Binary:  ${PREFIX}/bin/beamtalk"
    echo "   LSP:     ${PREFIX}/bin/beamtalk-lsp"
    echo "   MCP:     ${PREFIX}/bin/beamtalk-mcp"
    echo "   Runtime: ${PREFIX}/lib/beamtalk/lib/"

# Uninstall beamtalk from PREFIX (default: /usr/local)
uninstall PREFIX="/usr/local":
    #!/usr/bin/env bash
    set -euo pipefail
    PREFIX="{{PREFIX}}"
    echo "🗑️  Uninstalling beamtalk from ${PREFIX}..."
    rm -f "${PREFIX}/bin/beamtalk" "${PREFIX}/bin/beamtalk-compiler-port" "${PREFIX}/bin/beamtalk-lsp" "${PREFIX}/bin/beamtalk-mcp"
    rm -rf "${PREFIX}/lib/beamtalk"
    echo "✅ Uninstalled beamtalk from ${PREFIX}"

# Build VS Code extension (.vsix)
dist-vscode:
    #!/usr/bin/env bash
    set -euo pipefail
    # Auto-detect host platform for vsce --target
    ARCH="$(uname -m)"
    OS="$(uname -s)"
    case "${OS}-${ARCH}" in
        Linux-x86_64)   TARGET="linux-x64" ;;
        Linux-aarch64)  TARGET="linux-arm64" ;;
        Darwin-x86_64)  TARGET="darwin-x64" ;;
        Darwin-arm64)   TARGET="darwin-arm64" ;;
        *)              echo "❌ Unsupported platform: ${OS}-${ARCH}"; exit 1 ;;
    esac
    just dist-vscode-platform "${TARGET}"

# Build VS Code extension for a specific platform target
# Usage: just dist-vscode-platform linux-x64
dist-vscode-platform target:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📦 Building VS Code extension for {{target}}..."
    if ! command -v npm >/dev/null 2>&1; then
        echo "❌ npm not found (needed for VS Code extension)"
        exit 1
    fi
    # Determine binary name
    case "{{target}}" in
        win32-*) BIN_NAME="beamtalk-lsp.exe" ;;
        *)       BIN_NAME="beamtalk-lsp" ;;
    esac
    # Map vsce target to Rust target triple
    case "{{target}}" in
        linux-x64)    RUST_TARGET="x86_64-unknown-linux-gnu" ;;
        linux-arm64)  RUST_TARGET="aarch64-unknown-linux-gnu" ;;
        darwin-x64)   RUST_TARGET="x86_64-apple-darwin" ;;
        darwin-arm64) RUST_TARGET="aarch64-apple-darwin" ;;
        win32-x64)    RUST_TARGET="x86_64-pc-windows-msvc" ;;
        *) echo "❌ Unknown target: {{target}}"; exit 1 ;;
    esac
    LSP_BIN="target/${RUST_TARGET}/release/${BIN_NAME}"
    if [ ! -f "${LSP_BIN}" ]; then
        echo "❌ Binary not found: ${LSP_BIN}"
        echo "   Build first: cargo build --release --bin beamtalk-lsp --target ${RUST_TARGET}"
        exit 1
    fi
    mkdir -p editors/vscode/bin
    cp "${LSP_BIN}" "editors/vscode/bin/${BIN_NAME}"
    chmod +x "editors/vscode/bin/${BIN_NAME}"
    echo "   Bundled ${BIN_NAME} ($(du -h editors/vscode/bin/${BIN_NAME} | cut -f1))"
    cd editors/vscode
    npm install --quiet
    npm run compile
    ln -sf ../../LICENSE LICENSE
    npx --yes @vscode/vsce package --target "{{target}}" --out "../../dist/beamtalk-{{target}}.vsix"
    rm -f LICENSE
    rm -rf bin
    echo "✅ VS Code extension: dist/beamtalk-{{target}}.vsix"

# Create a distributable install in dist/
dist: build-release build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📦 Creating distribution in dist/..."
    rm -rf dist
    just install dist
    just dist-vscode
    echo "✅ Distribution ready in dist/"
    echo "   Run: dist/bin/beamtalk repl"
    echo "   VS Code extension: dist/beamtalk-*.vsix"

# ═══════════════════════════════════════════════════════════════════════════
# Documentation
# ═══════════════════════════════════════════════════════════════════════════

# Generate and open Rust documentation
docs:
    @echo "📚 Generating Rust documentation..."
    cargo doc --workspace --no-deps --open

# Generate stdlib API documentation (HTML)
docs-api:
    @echo "📚 Generating stdlib API documentation..."
    cargo run --bin beamtalk -- doc lib/ --output docs/api/

# Check documentation for broken links
docs-check:
    @echo "🔍 Checking documentation..."
    cargo doc --workspace --no-deps

