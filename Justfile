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

# On Unix: full CI including E2E and MCP tests (requires REPL workspace)
# On Windows: portable subset — skips E2E and MCP (workspace startup not yet supported)
# Run local CI checks (build, lint, test)
[unix]
ci: build lint test test-stdlib test-integration test-mcp test-e2e

[windows]
ci: build lint test test-stdlib test-integration

# Clean all build artifacts (Rust, Erlang, VS Code, caches, examples)
[unix]
clean: clean-rust clean-erlang clean-vscode
    @rm -rf runtime/_build 2>/dev/null || true
    @rm -rf target/llvm-cov 2>/dev/null || true
    @rm -rf examples/build 2>/dev/null || true
    @echo "✅ All build artifacts cleaned"

# Clean all build artifacts (Rust, Erlang, VS Code, caches, examples)
[windows]
clean: clean-rust clean-erlang clean-vscode
    if (Test-Path runtime\_build) { Remove-Item -Recurse -Force runtime\_build }
    if (Test-Path target\llvm-cov) { Remove-Item -Recurse -Force target\llvm-cov }
    if (Test-Path examples\build) { Remove-Item -Recurse -Force examples\build }
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

# Unix-only: uses chmod, du for atomic binary staging
# Build VS Code extension for local development (debug LSP, no .vsix packaging)
[unix]
build-vscode: build-lsp
    #!/usr/bin/env bash
    set -euo pipefail
    echo "🔨 Building VS Code extension for local development..."
    if ! command -v npm >/dev/null 2>&1; then
        echo "❌ npm not found (needed for VS Code extension)"
        exit 1
    fi
    if [ ! -f target/debug/beamtalk-lsp ]; then
        echo "❌ Debug LSP binary not found at target/debug/beamtalk-lsp"
        echo "   Build first: just build-lsp"
        exit 1
    fi
    mkdir -p editors/vscode/bin
    TMP_BIN="editors/vscode/bin/.beamtalk-lsp.tmp.$$"
    cp target/debug/beamtalk-lsp "${TMP_BIN}"
    chmod +x "${TMP_BIN}"
    mv -f "${TMP_BIN}" editors/vscode/bin/beamtalk-lsp
    echo "   Bundled debug beamtalk-lsp ($(du -h editors/vscode/bin/beamtalk-lsp | cut -f1))"
    cd editors/vscode
    npm install --quiet
    npm run compile
    echo "✅ VS Code extension built for local install from editors/vscode"

# Build VS Code extension for local development (debug LSP, no .vsix packaging)
[windows]
build-vscode: build-lsp
    @echo "🔨 Building VS Code extension for local development..."
    if (!(Get-Command npm -ErrorAction SilentlyContinue)) { Write-Error "npm not found"; exit 1 }
    if (!(Test-Path target\debug\beamtalk-lsp.exe)) { Write-Error "Debug LSP binary not found — run: just build-lsp"; exit 1 }
    New-Item -ItemType Directory -Force -Path editors\vscode\bin | Out-Null
    Copy-Item target\debug\beamtalk-lsp.exe editors\vscode\bin\beamtalk-lsp.exe
    @echo "   Bundled debug beamtalk-lsp.exe"
    Push-Location editors\vscode; try { npm install --quiet; if ($LASTEXITCODE -ne 0) { throw "npm install failed" }; npm run compile; if ($LASTEXITCODE -ne 0) { throw "npm run compile failed" } } finally { Pop-Location }
    @echo "✅ VS Code extension built for local install from editors/vscode"

# Build Erlang runtime
[working-directory: 'runtime']
build-erlang:
    @echo "🔨 Building Erlang runtime..."
    @rebar3 compile
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
[working-directory: 'runtime']
dialyzer:
    @echo "🔬 Running Dialyzer type checking..."
    rebar3 dialyzer

# ═══════════════════════════════════════════════════════════════════════════
# Testing
# ═══════════════════════════════════════════════════════════════════════════

# Run fast tests (Rust unit/integration + stdlib + Erlang runtime, skip slow E2E)
test: test-rust test-stdlib test-runtime

# Run Rust tests (unit + integration, skip slow E2E)
[unix]
test-rust:
    @echo "🧪 Running Rust tests (fast)..."
    @cargo test --all-targets --quiet
    @echo "✅ Rust tests complete"

[windows]
test-rust:
    @echo "🧪 Running Rust tests (fast)..."
    @cargo test --all-targets --quiet
    @echo "✅ Rust tests complete"

# Run E2E tests (slow - full pipeline, ~50s)
test-e2e: build-stdlib
    @echo "🧪 Running E2E tests (slow - ~50s)..."
    cargo test --test e2e -- --ignored

# Run workspace integration tests (requires Erlang/OTP runtime, ~10s)
test-integration: build-stdlib
    @echo "🧪 Running workspace integration tests..."
    cargo test --bin beamtalk -- --ignored --test-threads=1
    @echo "✅ Integration tests complete"

# Run MCP server integration tests (auto-starts REPL via test fixture, ~15s)
test-mcp: build
    @echo "🧪 Running MCP server integration tests..."
    @cargo test -p beamtalk-mcp -- --ignored --test-threads=1
    @echo "✅ MCP integration tests complete"

# Run ALL tests (unit + integration + E2E + Erlang runtime)
test-all: test-rust test-stdlib test-integration test-mcp test-e2e test-runtime

# Unix-only: uses mktemp, trap, kill, nc, process management
# Smoke test installed layout (install to temp dir, verify REPL starts)
[unix]
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
        PORT=$(sed -n 's/.*Connected to REPL backend on port \([0-9][0-9]*\).*/\1/p' "$OUTFILE" 2>/dev/null | tail -1)
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

# Note: Auto-discovers all *_tests modules. New test files are included automatically.
# Run Erlang runtime unit tests
[unix]
[working-directory: 'runtime']
test-runtime: build-stdlib
    @echo "🧪 Running Erlang runtime unit tests..."
    @rebar3 eunit --app=beamtalk_runtime,beamtalk_workspace
    @echo "✅ Runtime tests complete"

[windows]
[working-directory: 'runtime']
test-runtime: build-stdlib
    @echo "🧪 Running Erlang runtime unit tests..."
    @rebar3 eunit --app=beamtalk_runtime,beamtalk_workspace
    @echo "✅ Runtime tests complete"

# Run performance benchmarks (separate from unit tests, ~30s)
[working-directory: 'runtime']
perf: build-stdlib
    @echo "⏱️  Running performance benchmarks..."
    @rebar3 eunit --dir=perf
    @echo "✅ Performance benchmarks complete"

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

# Unix-only: depends on coverage-runtime (Unix-only)
# Generate coverage reports for both Rust and Erlang runtime
[unix]
coverage: coverage-rust coverage-runtime
    @echo "✅ Coverage reports generated"
    @echo "  Rust:    target/llvm-cov/html/index.html"
    @echo "  Runtime: runtime/_build/test/cover/index.html"

# Generate Rust coverage (requires cargo-llvm-cov)
coverage-rust:
    @echo "📊 Generating Rust coverage..."
    cargo llvm-cov --all-targets --workspace --html
    @echo "  📁 HTML report: target/llvm-cov/html/index.html"

# Unix-only: uses bash process substitution and piping
# Note: Auto-discovers all *_tests modules. New test files are included automatically.
# Generate Erlang runtime coverage
[unix]
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

# Unix-only: uses bash constructs (wc, file size checks)
# Collect E2E test coverage (runs E2E tests with Erlang cover instrumentation)
[unix]
coverage-e2e: build-stdlib
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

# Unix-only: uses bash constructs (wc, file size checks)
# Collect stdlib test coverage (runs stdlib tests with Erlang cover instrumentation)
[unix]
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

# Unix-only: depends on Unix-only coverage recipes
# Runs eunit with --cover, then E2E with cover, then stdlib with cover, then merges all into one report.
# Generate combined Erlang coverage (eunit + E2E + stdlib)
[unix]
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

# Unix-only: uses bash constructs (ls glob, sed)
# Show Erlang coverage report from existing coverdata (no re-run)
[unix]
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

# Unix-only: uses xdg-open/open
# Open Rust coverage report in browser
[unix]
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

# Unix-only: uses xdg-open/open
# Open Erlang runtime coverage report in browser
[unix]
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

# Clean Rust build artifacts
clean-rust:
    @echo "🧹 Cleaning Rust artifacts..."
    @cargo clean --quiet
    @echo "  ✅ Cleaned target/"

# Clean Erlang build artifacts
[working-directory: 'runtime']
clean-erlang:
    @echo "🧹 Cleaning Erlang artifacts..."
    rebar3 clean
    @echo "  ✅ Cleaned runtime/_build/"

# Clean VS Code extension build artifacts
[unix]
clean-vscode:
    @echo "🧹 Cleaning VS Code extension artifacts..."
    @rm -rf editors/vscode/out 2>/dev/null || true
    @rm -rf editors/vscode/node_modules 2>/dev/null || true
    @echo "  ✅ Cleaned editors/vscode/{out,node_modules}/"

# Clean VS Code extension build artifacts
[windows]
clean-vscode:
    @echo "🧹 Cleaning VS Code extension artifacts..."
    if (Test-Path editors/vscode/out) { Remove-Item -Recurse -Force editors/vscode/out }
    if (Test-Path editors/vscode/node_modules) { Remove-Item -Recurse -Force editors/vscode/node_modules }
    @echo "  ✅ Cleaned editors/vscode/{out,node_modules}/"

# Purge global Cargo cache (affects all Rust projects!)
[unix]
purge-cargo-cache:
    @echo "⚠️  This will delete ~/.cargo/registry/cache (affects all Rust projects)"
    @echo "Press Enter to continue or Ctrl+C to cancel..."
    @read _
    @rm -rf ~/.cargo/registry/cache 2>/dev/null || true
    @echo "  ✅ Cargo cache purged"

# Purge global Cargo cache (affects all Rust projects!)
[windows]
purge-cargo-cache:
    @echo "⚠️  This will delete $env:USERPROFILE\.cargo\registry\cache (affects all Rust projects)"
    $null = Read-Host "Press Enter to continue or Ctrl+C to cancel"
    $cachePath = "$env:USERPROFILE\.cargo\registry\cache"; if (Test-Path $cachePath) { Remove-Item -Recurse -Force $cachePath }
    @echo "  ✅ Cargo cache purged"

# ═══════════════════════════════════════════════════════════════════════════
# Development
# ═══════════════════════════════════════════════════════════════════════════

# Start the REPL (builds Rust first if needed)
repl: build-stdlib
    @echo "🚀 Starting Beamtalk REPL..."
    cargo run --bin beamtalk -- repl

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

# Unix-only: uses command -v (bash built-in)
# Install development tools
[unix]
install-tools:
    @echo "📦 Installing development tools..."
    @command -v cargo-llvm-cov >/dev/null 2>&1 || cargo install cargo-llvm-cov
    @command -v cargo-watch >/dev/null 2>&1 || cargo install cargo-watch
    @command -v just >/dev/null 2>&1 || cargo install just
    @echo "✅ Tools installed"

# Unix-only: uses command -v (bash built-in)
# Check for required tools
[unix]
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

# Unix-only: uses Unix install command with -d/-m flags
# Install beamtalk to PREFIX (default: /usr/local)
[unix]
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

# Unix-only: uses rm -f/-rf
# Uninstall beamtalk from PREFIX (default: /usr/local)
[unix]
uninstall PREFIX="/usr/local":
    #!/usr/bin/env bash
    set -euo pipefail
    PREFIX="{{PREFIX}}"
    echo "🗑️  Uninstalling beamtalk from ${PREFIX}..."
    rm -f "${PREFIX}/bin/beamtalk" "${PREFIX}/bin/beamtalk-compiler-port" "${PREFIX}/bin/beamtalk-lsp" "${PREFIX}/bin/beamtalk-mcp"
    rm -rf "${PREFIX}/lib/beamtalk"
    echo "✅ Uninstalled beamtalk from ${PREFIX}"

# Unix-only: uses uname for platform detection
# Build VS Code extension (.vsix)
[unix]
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

# Build VS Code extension (.vsix)
[windows]
dist-vscode:
    just dist-vscode-platform win32-x64

# Unix-only: uses chmod, du, case statements
# Usage: just dist-vscode-platform linux-x64
# Build VS Code extension for a specific platform target
[unix]
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
        # Fall back to default target path (when built without --target flag)
        LSP_BIN="target/release/${BIN_NAME}"
    fi
    if [ ! -f "${LSP_BIN}" ]; then
        echo "❌ Binary not found at target/${RUST_TARGET}/release/${BIN_NAME} or target/release/${BIN_NAME}"
        echo "   Build first: cargo build --release --bin beamtalk-lsp"
        exit 1
    fi
    mkdir -p editors/vscode/bin
    TMP_BIN="editors/vscode/bin/.${BIN_NAME}.tmp.$$"
    cp "${LSP_BIN}" "${TMP_BIN}"
    chmod +x "${TMP_BIN}"
    mv -f "${TMP_BIN}" "editors/vscode/bin/${BIN_NAME}"
    echo "   Bundled ${BIN_NAME} ($(du -h editors/vscode/bin/${BIN_NAME} | cut -f1))"
    cd editors/vscode
    npm install --quiet
    npm run compile
    mkdir -p ../../dist
    npx --yes @vscode/vsce package --target "{{target}}" --out "../../dist/beamtalk-{{target}}.vsix"
    rm -rf bin
    echo "✅ VS Code extension: dist/beamtalk-{{target}}.vsix"

# Usage: just dist-vscode-platform win32-x64
# Build VS Code extension for a specific platform target
[windows]
dist-vscode-platform target:
    @echo "📦 Building VS Code extension for {{target}}..."
    if (!(Get-Command npm -ErrorAction SilentlyContinue)) { Write-Error "npm not found"; exit 1 }
    $binName = if ("{{target}}" -like "win32-*") { "beamtalk-lsp.exe" } else { "beamtalk-lsp" }; $lspBin = "target\release\$binName"; if (!(Test-Path $lspBin)) { $lspBin = "target\x86_64-pc-windows-msvc\release\$binName" }; if (!(Test-Path $lspBin)) { Write-Error "LSP binary not found — run: cargo build --release --bin beamtalk-lsp"; exit 1 }; New-Item -ItemType Directory -Force -Path editors\vscode\bin | Out-Null; Copy-Item $lspBin "editors\vscode\bin\$binName"; Write-Host "   Bundled $binName"
    Push-Location editors\vscode; try { npm install --quiet; if ($LASTEXITCODE -ne 0) { throw "npm install failed" }; npm run compile; if ($LASTEXITCODE -ne 0) { throw "npm run compile failed" } } finally { Pop-Location }
    New-Item -ItemType Directory -Force -Path dist | Out-Null
    Push-Location editors\vscode; try { npx --yes @vscode/vsce package --target "{{target}}" --out "..\..\dist\beamtalk-{{target}}.vsix"; if ($LASTEXITCODE -ne 0) { throw "vsce package failed" } } finally { Pop-Location }
    Remove-Item -Recurse -Force editors\vscode\bin -ErrorAction SilentlyContinue
    @echo "✅ VS Code extension: dist/beamtalk-{{target}}.vsix"

# Unix-only: depends on Unix-only install and dist-vscode recipes
# Create a distributable install in dist/
[unix]
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

# Create a distributable install in dist/ (Windows)
[windows]
dist: build-release build-stdlib
    @echo "📦 Creating distribution in dist/..."
    if (Test-Path dist) { Remove-Item -Recurse -Force dist }
    New-Item -ItemType Directory -Force -Path dist/bin | Out-Null
    Copy-Item target/release/beamtalk.exe dist/bin/
    Copy-Item target/release/beamtalk-compiler-port.exe dist/bin/
    Copy-Item target/release/beamtalk-lsp.exe dist/bin/
    Copy-Item target/release/beamtalk-mcp.exe dist/bin/
    foreach ($app in @('beamtalk_runtime','beamtalk_workspace','beamtalk_compiler','jsx')) { $src = "runtime/_build/default/lib/$app/ebin"; if (!(Test-Path "$src/*.beam")) { Write-Error "No .beam files in $src"; exit 1 }; New-Item -ItemType Directory -Force -Path "dist/lib/beamtalk/lib/$app/ebin" | Out-Null; Copy-Item "$src/*.beam" "dist/lib/beamtalk/lib/$app/ebin/"; Copy-Item "$src/*.app" "dist/lib/beamtalk/lib/$app/ebin/" -ErrorAction SilentlyContinue }
    $stdlib = "runtime/apps/beamtalk_stdlib/ebin"; if (!(Test-Path "$stdlib/*.beam")) { Write-Error "No stdlib .beam files"; exit 1 }; New-Item -ItemType Directory -Force -Path "dist/lib/beamtalk/lib/beamtalk_stdlib/ebin" | Out-Null; Copy-Item "$stdlib/*.beam" "dist/lib/beamtalk/lib/beamtalk_stdlib/ebin/"; Copy-Item "$stdlib/*.app" "dist/lib/beamtalk/lib/beamtalk_stdlib/ebin/" -ErrorAction SilentlyContinue
    just dist-vscode
    @echo "✅ Distribution ready in dist/"
    @echo "   Run: dist\bin\beamtalk.exe repl"
    @echo "   VS Code extension: dist\beamtalk-*.vsix"

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

