# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Beamtalk build tasks
# Run `just` to see all available recipes
# Run `just <recipe>` to execute a specific task

# Use bash for all commands
set shell := ["bash", "-uc"]

# Default recipe (list all tasks)
default:
    @just --list

# ═══════════════════════════════════════════════════════════════════════════
# Quick Commands (CI equivalents)
# ═══════════════════════════════════════════════════════════════════════════

# Run local CI checks (build, lint, unit, integration & E2E tests)
ci: build lint test test-stdlib test-integration test-mcp test-e2e

# Full clean and rebuild everything
clean-all: clean clean-erlang
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

# Build Erlang runtime
build-erlang:
    @echo "🔨 Building Erlang runtime..."
    @cd runtime && rebar3 compile 2>&1 | grep -v "===>" || true
    @echo "✅ Erlang build complete"

# Build standard library (lib/*.bt → BEAM)
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
test: test-rust test-runtime test-stdlib

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

    # Start a REPL workspace in the background
    MCP_TEST_PORT=19876
    ./target/debug/beamtalk repl --port "$MCP_TEST_PORT" &
    REPL_PID=$!

    # Ensure cleanup on exit
    cleanup() {
        kill "$REPL_PID" 2>/dev/null || true
        wait "$REPL_PID" 2>/dev/null || true
    }
    trap cleanup EXIT

    # Wait for REPL to be ready
    for i in $(seq 1 30); do
        if ss -tlnp 2>/dev/null | grep -q ":${MCP_TEST_PORT} " || \
           nc -z 127.0.0.1 "$MCP_TEST_PORT" 2>/dev/null; then
            break
        fi
        sleep 1
    done

    # Verify connection
    if ! nc -z 127.0.0.1 "$MCP_TEST_PORT" 2>/dev/null; then
        echo "❌ REPL failed to start on port $MCP_TEST_PORT"
        exit 1
    fi

    echo "  REPL running on port $MCP_TEST_PORT (pid $REPL_PID)"

    # Run the MCP integration tests
    cargo test -p beamtalk-mcp -- --ignored --test-threads=1

    echo "✅ MCP integration tests complete"

# Run ALL tests (unit + integration + E2E + Erlang runtime)
test-all: test-rust test-stdlib test-integration test-mcp test-e2e test-runtime

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
test-runtime:
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
coverage-combined: coverage-runtime coverage-e2e coverage-stdlib
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
clean:
    @echo "🧹 Cleaning Rust artifacts..."
    @if [ -d target ]; then find target -mindepth 1 -maxdepth 1 -exec rm -rf {} +; fi 2>/dev/null || true
    @echo "  ✅ Cleaned target/"

# Clean Erlang build artifacts
clean-erlang:
    @echo "🧹 Cleaning Erlang artifacts..."
    cd runtime && rebar3 clean
    @echo "  ✅ Cleaned runtime/_build/"

# Deep clean (removes repo-local caches, coverage, examples)
deep-clean: clean clean-erlang
    @echo "🧹 Deep cleaning repo artifacts..."
    @rm -rf runtime/_build 2>/dev/null || true
    @rm -rf target/llvm-cov 2>/dev/null || true
    @rm -rf examples/build 2>/dev/null || true
    @echo "  ✅ Deep clean complete"

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
    @echo "✅ All required tools found"

# ═══════════════════════════════════════════════════════════════════════════
# Release & Installation
# ═══════════════════════════════════════════════════════════════════════════

# Prepare for release (run all checks)
pre-release: clean-all ci coverage
    @echo "✅ Pre-release checks passed"

# Install beamtalk to PREFIX (default: /usr/local)
install PREFIX="/usr/local": build-release build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    PREFIX="{{PREFIX}}"
    echo "📦 Installing beamtalk to ${PREFIX}..."

    # Binary
    install -d "${PREFIX}/bin"
    install -m 755 target/release/beamtalk "${PREFIX}/bin/beamtalk"

    # OTP application ebin directories
    for app in beamtalk_runtime beamtalk_workspace jsx; do
        install -d "${PREFIX}/lib/beamtalk/lib/${app}/ebin"
        install -m 644 runtime/_build/default/lib/${app}/ebin/*.beam "${PREFIX}/lib/beamtalk/lib/${app}/ebin/"
        # Copy .app file if present
        if ls runtime/_build/default/lib/${app}/ebin/*.app 1>/dev/null 2>&1; then
            install -m 644 runtime/_build/default/lib/${app}/ebin/*.app "${PREFIX}/lib/beamtalk/lib/${app}/ebin/"
        fi
    done

    # Stdlib (built under apps/, not _build/)
    install -d "${PREFIX}/lib/beamtalk/lib/beamtalk_stdlib/ebin"
    install -m 644 runtime/apps/beamtalk_stdlib/ebin/*.beam "${PREFIX}/lib/beamtalk/lib/beamtalk_stdlib/ebin/"
    if ls runtime/apps/beamtalk_stdlib/ebin/*.app 1>/dev/null 2>&1; then
        install -m 644 runtime/apps/beamtalk_stdlib/ebin/*.app "${PREFIX}/lib/beamtalk/lib/beamtalk_stdlib/ebin/"
    fi

    echo "✅ Installed beamtalk to ${PREFIX}"
    echo "   Binary:  ${PREFIX}/bin/beamtalk"
    echo "   Runtime: ${PREFIX}/lib/beamtalk/lib/"

# Uninstall beamtalk from PREFIX (default: /usr/local)
uninstall PREFIX="/usr/local":
    #!/usr/bin/env bash
    set -euo pipefail
    PREFIX="{{PREFIX}}"
    echo "🗑️  Uninstalling beamtalk from ${PREFIX}..."
    rm -f "${PREFIX}/bin/beamtalk"
    rm -rf "${PREFIX}/lib/beamtalk"
    echo "✅ Uninstalled beamtalk from ${PREFIX}"

# Create a distributable install in dist/
dist: build-release build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📦 Creating distribution in dist/..."
    rm -rf dist
    just install dist
    echo "✅ Distribution ready in dist/"
    echo "   Run: dist/bin/beamtalk repl"

# ═══════════════════════════════════════════════════════════════════════════
# Documentation
# ═══════════════════════════════════════════════════════════════════════════

# Generate and open Rust documentation
docs:
    @echo "📚 Generating Rust documentation..."
    cargo doc --workspace --no-deps --open

# Check documentation for broken links
docs-check:
    @echo "🔍 Checking documentation..."
    cargo doc --workspace --no-deps

