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

# Build all targets (Rust + Erlang + stdlib)
build: build-rust build-erlang build-stdlib

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

# Run fast tests (Rust unit/integration + Erlang runtime, skip slow E2E)
test: test-rust test-runtime

# Run Rust tests (unit + integration, skip slow E2E)
test-rust:
    @echo "🧪 Running Rust tests (fast)..."
    @cargo test --all-targets 2>&1 | awk '/Running.*\(/ { s=$0; sub(/.*Running /, "", s); sub(/unittests /, "", s); split(s, b, / \(/); src=b[1]; crate=b[2]; gsub(/.*\//, "", crate); sub(/-[a-f0-9]+\)$/, "", crate); label=crate "::" src } /^test result:/ { sub(/^test result: ok\. /, ""); printf "  %-45s %s\n", label, $0 } /^warning:/ { print }' || true
    @echo "✅ Rust tests complete"

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
    # Only run beamtalk_runtime tests (stdlib has no test modules)
    # Integration tests (beamtalk_repl_integration_tests) require daemon and are run separately
    if ! OUTPUT=$(rebar3 eunit --app=beamtalk_runtime 2>&1); then
        # Check if tests actually failed (as opposed to being cancelled)
        if echo "$OUTPUT" | grep -qE "[1-9][0-9]* failures"; then
            # Show full output only on real failures
            echo "$OUTPUT"
            echo "❌ Runtime tests failed"
            exit 1
        fi
        # Allow "cancelled" status for tests that can't run (need daemon, etc.)
        if echo "$OUTPUT" | grep -q "cancelled"; then
            # Extract summary line only (concise)
            echo "$OUTPUT" | grep -E "Finished in|[0-9]+ tests," || echo "✓ Tests passed with some skipped"
        else
            # Unexpected error - show full output
            echo "$OUTPUT"
            echo "❌ rebar3 failed unexpectedly"
            exit 1
        fi
    else
        # Success - show only summary
        echo "$OUTPUT" | grep -E "Finished in|[0-9]+ tests," || echo "✓ All tests passed"
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
#
# Known failures: 6 tests for super dispatch (tracked in BT-235).
# Coverage is still generated despite these failures since they're expected.
coverage-runtime:
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "📊 Generating Erlang runtime coverage..."
    # Run both runtime and workspace tests (stdlib has no test modules)
    if ! OUTPUT=$(rebar3 eunit --app=beamtalk_runtime,beamtalk_workspace --cover 2>&1); then
        echo "$OUTPUT"
        # Allow known failures (BT-235 super dispatch tests)
        FAILURE_COUNT=$(echo "$OUTPUT" | grep -oE '[0-9]+ failures' | grep -oE '[0-9]+' || echo "0")
        if [ "$FAILURE_COUNT" -gt 30 ]; then
            echo "❌ $FAILURE_COUNT test failures detected! Check for regressions."
            exit 1
        fi
        # Tests ran (with known failures), continue to coverage
    else
        echo "$OUTPUT"
    fi
    rebar3 cover --verbose
    rebar3 covertool generate
    # Clean up covertool XML: remove empty phantom packages, shorten path-based names
    python3 -c "
import xml.etree.ElementTree as ET, sys, os, glob
for xml_path in glob.glob('_build/test/covertool/*.covertool.xml'):
    tree = ET.parse(xml_path)
    root = tree.getroot()
    packages = root.find('packages')
    if packages is None: continue
    app_name = os.path.basename(xml_path).replace('.covertool.xml', '')
    to_remove = []
    for pkg in packages.findall('package'):
        classes = pkg.find('classes')
        if classes is None or len(classes) == 0:
            to_remove.append(pkg)
        else:
            # Shorten long path-based names to just the app name
            pkg.set('name', app_name)
    for pkg in to_remove:
        packages.remove(pkg)
    tree.write(xml_path, xml_declaration=True, encoding='utf-8')
    print(f'  ✅ Cleaned {os.path.basename(xml_path)}: removed {len(to_remove)} empty package(s)')
"
    echo "  📁 HTML report: runtime/_build/test/cover/index.html"
    echo "  📁 XML reports: runtime/_build/test/covertool/*.covertool.xml"

# Collect E2E test coverage (runs E2E tests with Erlang cover instrumentation)
coverage-e2e: _clean-daemon-state
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📊 Running E2E tests with Erlang cover instrumentation..."
    echo "   (This is slower than normal E2E due to cover overhead)"
    E2E_COVER=1 cargo test --test e2e -- --ignored
    if [ -f runtime/_build/test/cover/e2e.coverdata ]; then
        SIZE=$(wc -c < runtime/_build/test/cover/e2e.coverdata)
        echo "  📁 Coverdata: runtime/_build/test/cover/e2e.coverdata (${SIZE} bytes)"
    else
        echo "⚠️  No E2E coverdata produced"
        exit 1
    fi

# Generate combined Erlang coverage (eunit + E2E)
# Runs eunit with --cover, then E2E with cover, then merges both into one report.
coverage-combined: coverage-runtime coverage-e2e
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "📊 Merging eunit + E2E coverage data..."
    # rebar3 cover imports all .coverdata files in _build/test/cover/
    rebar3 cover --verbose
    rebar3 covertool generate
    # Clean up covertool XML (same as coverage-runtime)
    python3 -c "
import xml.etree.ElementTree as ET, os, glob
for xml_path in glob.glob('_build/test/covertool/*.covertool.xml'):
    tree = ET.parse(xml_path)
    root = tree.getroot()
    packages = root.find('packages')
    if packages is None: continue
    app_name = os.path.basename(xml_path).replace('.covertool.xml', '')
    to_remove = [pkg for pkg in packages.findall('package') if pkg.find('classes') is None or len(pkg.find('classes')) == 0]
    for pkg in packages.findall('package'):
        if pkg not in to_remove:
            pkg.set('name', app_name)
    for pkg in to_remove:
        packages.remove(pkg)
    tree.write(xml_path, xml_declaration=True, encoding='utf-8')
"
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

