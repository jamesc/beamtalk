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

# Run local CI checks (build, lint, unit & E2E tests)
# Note: Runtime integration tests run only in GitHub Actions CI
ci: build lint test test-e2e

# Full clean and rebuild everything
clean-all: clean clean-erlang
    @echo "✅ All build artifacts cleaned"

# ═══════════════════════════════════════════════════════════════════════════
# Build Tasks
# ═══════════════════════════════════════════════════════════════════════════

# Build all targets (Rust + Erlang)
build: build-rust build-erlang

# Build Rust workspace
build-rust:
    @echo "🔨 Building Rust workspace..."
    cargo build --all-targets

# Build in release mode (Rust + Erlang)
build-release: build-rust-release build-erlang

# Build Rust in release mode
build-rust-release:
    @echo "🔨 Building Rust workspace (release)..."
    cargo build --all-targets --release

# Build Erlang runtime
build-erlang:
    @echo "🔨 Building Erlang runtime..."
    cd runtime && rebar3 compile

# ═══════════════════════════════════════════════════════════════════════════
# Lint and Format
# ═══════════════════════════════════════════════════════════════════════════

# Run all linting and formatting checks
lint: clippy fmt-check

# Run clippy (Rust linter) - warnings are errors
clippy:
    @echo "🔍 Running clippy..."
    cargo clippy --all-targets -- -D warnings

# Check Rust code formatting
fmt-check:
    @echo "📋 Checking Rust formatting..."
    cargo fmt --all -- --check

# Format all Rust code
fmt:
    @echo "✨ Formatting Rust code..."
    cargo fmt --all

# ═══════════════════════════════════════════════════════════════════════════
# Testing
# ═══════════════════════════════════════════════════════════════════════════

# Run fast tests (Rust unit/integration + Erlang runtime, skip slow E2E)
test: test-rust test-runtime

# Run Rust tests (unit + integration, skip slow E2E)
test-rust:
    @echo "🧪 Running Rust tests (fast)..."
    cargo test --all-targets

# Run E2E tests (slow - full pipeline, ~50s)
test-e2e: _clean-daemon-state
    @echo "🧪 Running E2E tests (slow - ~50s)..."
    cargo test --test e2e -- --ignored

# Run ALL tests (unit + integration + E2E + Erlang runtime)
test-all: test-rust test-e2e test-runtime

# Clean up stale daemon state (internal helper)
_clean-daemon-state:
    @rm -f ~/.beamtalk/daemon.sock ~/.beamtalk/daemon.lock 2>/dev/null || true

# Run Erlang runtime unit tests
# Note: Auto-discovers all *_tests modules. New test files are included automatically.
#
# Known failures: 6 tests for super dispatch (tracked in BT-235).
# We allow these specific failures to avoid blocking development while the fix is in progress.
# If more tests fail, it indicates a regression and the build will fail.
test-runtime:
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "🧪 Running Erlang runtime unit tests..."
    # rebar3 auto-discovers all *_tests.erl modules
    # Integration tests (beamtalk_repl_integration_tests) require daemon and are run separately
    if ! OUTPUT=$(rebar3 eunit 2>&1); then
        echo "$OUTPUT"
        # Check if any tests actually failed (as opposed to being cancelled/skipped)
        if echo "$OUTPUT" | grep -qE "Failed: [1-9]"; then
            echo "❌ Runtime tests failed"
            exit 1
        fi
        # Allow "cancelled" status for integration tests that need daemon
        if echo "$OUTPUT" | grep -q "One or more tests were cancelled"; then
            echo "✓ Unit tests passed (integration tests skipped - need daemon)"
        else
            echo "❌ rebar3 failed without test results"
            exit 1
        fi
    else
        echo "$OUTPUT"
    fi

# Run Erlang runtime integration tests (requires daemon)
test-runtime-integration:
    @echo "🧪 Running Erlang runtime integration tests..."
    @echo "⚠️  Make sure daemon is running: just daemon-start"
    cd runtime && rebar3 eunit --module=beamtalk_repl_integration_tests

# Run a specific Rust test by name
test-one TEST:
    @echo "🧪 Running test: {{TEST}}"
    cargo test --all-targets {{TEST}}

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
#
# Known failures: 6 tests for super dispatch (tracked in BT-235).
# Coverage is still generated despite these failures since they're expected.
coverage-runtime:
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "📊 Generating Erlang runtime coverage..."
    # rebar3 auto-discovers all *_tests.erl modules
    if ! OUTPUT=$(rebar3 eunit --cover 2>&1); then
        echo "$OUTPUT"
        # Allow exactly 6 known failures (BT-235 super dispatch tests)
        if echo "$OUTPUT" | grep -qE "Failed: ([7-9]|[1-9][0-9]+)\."; then
            echo "❌ More than 6 tests failed! Check for regressions (expected: 6 from BT-235)."
            exit 1
        fi
        # Tests ran (with known failures), continue to coverage
    else
        echo "$OUTPUT"
    fi
    rebar3 cover --verbose
    rebar3 covertool generate
    echo "  📁 HTML report: runtime/_build/test/cover/index.html"
    echo "  📁 XML report:  runtime/_build/test/covertool/beamtalk_runtime.covertool.xml"

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
repl: build-rust
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
# Release
# ═══════════════════════════════════════════════════════════════════════════

# Prepare for release (run all checks)
pre-release: clean-all ci coverage
    @echo "✅ Pre-release checks passed"

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

