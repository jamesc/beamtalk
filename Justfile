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

# Run local CI checks (combination of GitHub Actions check + test jobs)
# Matches: just build clippy fmt-check test (check job)
#        + just test-integration test-mcp test-e2e (test job extras)
#        + dialyzer if Erlang changed (skipped on Windows - known PATH issue)
[unix]
ci: build lint test test-integration test-mcp test-e2e check-corpus

[windows]
ci: build clippy fmt-check-rust test test-integration test-mcp test-e2e

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
    npm ci --quiet
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

# Build and deploy VS Code extension to the locally-installed extension directory.
# Finds the first matching beamtalk.beamtalk-* dir under ~/.vscode-server*/extensions/.
# Copies JS output and LSP binary so the extension host picks up changes on next reload.
[linux]
dev-deploy-vscode: build-vscode
    #!/usr/bin/env bash
    set -euo pipefail
    EXT_DIR=$(ls -d ~/.vscode-server*/extensions/beamtalk.beamtalk-* 2>/dev/null | head -1)
    if [ -z "${EXT_DIR}" ]; then
        echo "❌ No installed beamtalk extension found under ~/.vscode-server*/extensions/"
        echo "   Install the extension first, then re-run."
        exit 1
    fi
    echo "📦 Deploying to ${EXT_DIR}"
    cp editors/vscode/out/*.js "${EXT_DIR}/out/"
    cp target/debug/beamtalk-lsp "${EXT_DIR}/bin/beamtalk-lsp"
    echo "✅ Deployed JS + LSP to ${EXT_DIR}"
    echo "   Reload the VS Code extension host (Ctrl+Shift+P → 'Restart Extension Host') to pick up changes."

# Build Erlang runtime
[working-directory: 'runtime']
build-erlang:
    @echo "🔨 Building Erlang runtime..."
    @rebar3 compile
    @echo "✅ Erlang build complete"

# Build standard library (stdlib/src/*.bt → BEAM, incremental — skips if up to date)
build-stdlib: build-rust build-erlang
    @echo "🔨 Building standard library..."
    @cargo run --bin beamtalk --quiet -- build-stdlib --quiet
    @echo "✅ Stdlib build complete"

# Build all example programs (examples/**/*.bt → BEAM)
build-examples: build-stdlib
    @echo "🔨 Building examples..."
    @cargo run --bin beamtalk --quiet -- build examples/
    @echo "✅ Examples build complete"

# Run tests for every example package that has a test/ directory
[unix]
test-examples: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "🧪 Testing examples..."
    failed=0
    passed=0
    for dir in examples/*/; do
        name=$(basename "$dir")
        if [ ! -d "${dir}test" ]; then
            continue
        fi
        echo "  Testing ${name}..."
        if (cd "${dir}" && cargo run --bin beamtalk --quiet -- test 2>&1); then
            passed=$((passed + 1))
        else
            echo "❌ ${name} tests failed"
            failed=$((failed + 1))
        fi
    done
    if [ "$failed" -gt 0 ]; then
        echo "❌ ${failed} example(s) failed, ${passed} passed"
        exit 1
    fi
    echo "✅ All example tests passed (${passed} packages)"

# Generate the example corpus for the MCP search tool (ADR 0062)
build-corpus: build-rust
    @echo "📚 Building example corpus..."
    @cargo run --bin build-corpus --quiet
    @echo "✅ Corpus build complete"

# Check that the checked-in corpus.json is up to date (CI freshness check)
[unix]
check-corpus: build-rust
    #!/usr/bin/env bash
    set -euo pipefail
    cargo run --bin build-corpus --quiet
    if ! git diff --exit-code crates/beamtalk-examples/corpus.json > /dev/null 2>&1; then
        echo "❌ corpus.json is out of date — run 'just build-corpus' and commit"
        exit 1
    fi
    echo "✅ corpus.json is up to date"

# Evaluate search quality from structured MCP server logs (ADR 0062)
# Usage: just search-eval /path/to/mcp-server.log
[unix]
search-eval logfile:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ ! -f "{{logfile}}" ]; then
        echo "❌ Log file not found: {{logfile}}"
        exit 1
    fi

    echo "═══ Search Examples Eval Report ═══"
    echo ""

    # Extract search_examples log lines (structured tracing format)
    total=$(grep -c 'search_examples' "{{logfile}}" 2>/dev/null || true)
    echo "Total searches: $total"

    if [ "$total" -eq 0 ]; then
        echo "No search_examples calls found in log."
        exit 0
    fi

    # Zero-result queries
    zero=$(grep 'search_examples' "{{logfile}}" | grep -c 'result_count=0' 2>/dev/null || true)
    echo "Zero-result queries: $zero ($((zero * 100 / total))%)"
    echo ""

    if [ "$zero" -gt 0 ]; then
        echo "── Zero-result query hashes ──"
        grep 'search_examples' "{{logfile}}" | grep 'result_count=0' | sed -n 's/.*query_hash=\([a-f0-9]*\).*/\1/p' | sort | uniq -c | sort -rn | head -20
        echo ""
    fi

    # Low-score queries (top_score < 5)
    echo "── Low-score queries (top_score < 5) ──"
    grep 'search_examples' "{{logfile}}" | sed -n 's/.*top_score=\([0-9]*\).*/\1/p' | awk '$1 > 0 && $1 < 5' | wc -l | xargs -I{} echo "Count: {}"
    echo ""

    # Score distribution
    echo "── Score distribution ──"
    grep 'search_examples' "{{logfile}}" | sed -n 's/.*top_score=\([0-9]*\).*/\1/p' | sort -n | uniq -c | sort -rn | head -20
    echo ""

    # Query frequency (by hash)
    echo "── Top query hashes (by frequency) ──"
    grep 'search_examples' "{{logfile}}" | sed -n 's/.*query_hash=\([a-f0-9]*\).*/\1/p' | sort | uniq -c | sort -rn | head -20
    echo ""

    # Duration stats
    echo "── Duration (μs) ──"
    grep 'search_examples' "{{logfile}}" | sed -n 's/.*duration_us=\([0-9]*\).*/\1/p' | awk '{sum+=$1; count++; if($1>max)max=$1; if(min==""||$1<min)min=$1} END {if(count==0){print "min=n/a avg=n/a max=n/a count=0"; exit 0} printf "min=%d avg=%d max=%d count=%d\n", min, sum/count, max, count}'

# ═══════════════════════════════════════════════════════════════════════════
# Lint and Format
# ═══════════════════════════════════════════════════════════════════════════

# Run all linting and formatting checks
lint: lint-rust lint-erlang lint-js lint-beamtalk

# Lint Beamtalk: formatting check
lint-beamtalk: fmt-check-beamtalk

# Lint Rust: clippy + formatting check
lint-rust: clippy fmt-check-rust

# Lint Erlang: Dialyzer type checking + format check
lint-erlang: dialyzer fmt-check-erlang

# Lint JS/TS: Biome lint + format check
[working-directory: 'editors/vscode']
lint-js: fmt-check-js
    @echo "🔍 Running Biome lint..."
    npm run lint
    @echo "✅ Biome lint passed"

# Run clippy (Rust linter) - warnings are errors
clippy:
    @echo "🔍 Running clippy..."
    @cargo clippy --all-targets --quiet -- -D warnings
    @echo "✅ Clippy passed"

# Check Rust code formatting
fmt-check-rust:
    @echo "📋 Checking Rust formatting..."
    cargo fmt --all -- --check

# Check all code formatting
[unix]
fmt-check: fmt-check-rust fmt-check-erlang fmt-check-js fmt-check-beamtalk

# Windows: skip Erlang and JS format checks (platform-agnostic, covered by Linux CI)
[windows]
fmt-check: fmt-check-rust fmt-check-beamtalk

# Format all Rust code
fmt-rust:
    @echo "✨ Formatting Rust code..."
    cargo fmt --all

# Format all code (Rust + Erlang + JS + Beamtalk stdlib/test sources)
fmt: fmt-rust fmt-erlang fmt-js fmt-beamtalk

# Check JS/TS formatting (Biome)
[working-directory: 'editors/vscode']
fmt-check-js:
    @echo "📋 Checking JS/TS formatting..."
    npm ci --quiet
    npm run format:check
    @echo "✅ JS/TS formatting check passed"

# Format all JS/TS code (Biome)
[working-directory: 'editors/vscode']
fmt-js:
    @echo "✨ Formatting JS/TS code..."
    npm ci --quiet
    npm run format
    @echo "✅ JS/TS code formatted"

# Check Erlang code formatting
[working-directory: 'runtime']
fmt-check-erlang:
    @echo "📋 Checking Erlang formatting..."
    rebar3 fmt --check
    @echo "✅ Erlang formatting check passed"

# Format all Erlang code
[working-directory: 'runtime']
fmt-erlang:
    @echo "✨ Formatting Erlang code..."
    rebar3 fmt -w
    @echo "✅ Erlang code formatted"

# Format all Beamtalk source files
# stdlib/bootstrap-test/ uses identity formatting for .btscript files —
# only parse errors are reported; file content is never rewritten (BT-1016).
# tests/e2e/cases/ is excluded: those .btscript files contain REPL commands
# (e.g. :clear, :bindings) that are not valid Beamtalk syntax.
fmt-beamtalk:
    @echo "✨ Formatting Beamtalk source files..."
    @cargo run --bin beamtalk --quiet -- fmt stdlib/src/ stdlib/test/ stdlib/bootstrap-test/ tests/e2e/fixtures/
    @echo "✅ Beamtalk source files formatted"

# Check Beamtalk source file formatting
fmt-check-beamtalk:
    @echo "📋 Checking Beamtalk source formatting..."
    @cargo run --bin beamtalk --quiet -- fmt-check stdlib/src/ stdlib/test/ stdlib/bootstrap-test/ tests/e2e/fixtures/
    @echo "✅ Beamtalk formatting check passed"

# Run Dialyzer on Erlang runtime
[working-directory: 'runtime']
dialyzer:
    @echo "🔬 Running Dialyzer type checking..."
    rebar3 dialyzer

# ═══════════════════════════════════════════════════════════════════════════
# Testing
# ═══════════════════════════════════════════════════════════════════════════

# Run fast tests (Rust unit/integration + stdlib + BUnit + Erlang runtime, skip slow E2E)
# Typical time: ~4:30 (test-rust ~45s, test-stdlib ~20s, test-bunit ~97s, test-runtime ~1:40)
test: test-rust test-stdlib test-bunit test-runtime

# Run Rust tests (unit + integration, skip slow E2E)
# Output: summary lines + failures only (reduces ~74 lines to ~10)
[unix]
test-rust:
    #!/usr/bin/env bash
    set -o pipefail
    echo "🧪 Running Rust tests (fast)..."
    output=$(cargo test --all-targets --quiet 2>&1) && rc=0 || rc=$?
    echo "$output" | grep -E '^test result:|FAILED|^error' || true
    if [ $rc -ne 0 ]; then echo "$output"; exit $rc; fi
    echo "✅ Rust tests complete"

[windows]
test-rust:
    @echo "🧪 Running Rust tests (fast)..."
    @$output = cargo test --all-targets --quiet 2>&1 | Out-String; $exitCode = $LASTEXITCODE; $output -split "`n" | Select-String -Pattern "^test result:|FAILED|^error"; if ($exitCode -ne 0) { Write-Output $output; exit $exitCode }
    @echo "✅ Rust tests complete"

# Run E2E tests (slow - full pipeline, ~50s)
test-e2e: build-stdlib
    @echo "🧪 Running E2E tests (slow - ~50s)..."
    cargo test --test e2e -- --ignored

# Run workspace integration tests (requires Erlang/OTP runtime, ~10s)
# Output: summary only on success, full output on failure
[unix]
test-integration: build-stdlib
    #!/usr/bin/env bash
    set -o pipefail
    echo "🧪 Running workspace integration tests..."
    output=$(cargo test --bin beamtalk -- --ignored --test-threads=1 2>&1) && rc=0 || rc=$?
    echo "$output" | grep -E '^test result:|FAILED|^error' || true
    if [ $rc -ne 0 ]; then echo "$output"; exit $rc; fi
    echo "✅ Integration tests complete"

[windows]
test-integration: build-stdlib
    @echo "🧪 Running workspace integration tests..."
    @$output = cargo test --bin beamtalk -- --ignored --test-threads=1 2>&1 | Out-String; $exitCode = $LASTEXITCODE; $output -split "`n" | Select-String -Pattern "^test result:|FAILED|^error"; if ($exitCode -ne 0) { Write-Output $output; exit $exitCode }
    @echo "✅ Integration tests complete"

# Run MCP server integration tests (auto-starts REPL via test fixture, ~15s)
# Output: summary only on success, full output on failure
[unix]
test-mcp: build
    #!/usr/bin/env bash
    set -o pipefail
    echo "🧪 Running MCP server integration tests..."
    output=$(cargo test -p beamtalk-mcp -- --ignored --test-threads=1 2>&1) && rc=0 || rc=$?
    echo "$output" | grep -E '^test result:|FAILED|^error' || true
    if [ $rc -ne 0 ]; then echo "$output"; exit $rc; fi
    echo "✅ MCP integration tests complete"

[windows]
test-mcp: build
    @echo "🧪 Running MCP server integration tests..."
    @$output = cargo test -p beamtalk-mcp -- --ignored --test-threads=1 2>&1 | Out-String; $exitCode = $LASTEXITCODE; $output -split "`n" | Select-String -Pattern "^test result:|FAILED|^error"; if ($exitCode -ne 0) { Write-Output $output; exit $exitCode }
    @echo "✅ MCP integration tests complete"

# Run ALL tests (unit + integration + E2E + Erlang runtime)
test-all: test-rust test-stdlib test-bunit test-integration test-mcp test-e2e test-runtime

# Smoke test installed layout (install to temp dir, verify binary + compiler work)
[unix]
test-install: build-release build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "🧪 Smoke-testing installed layout..."
    TMPDIR=$(mktemp -d)
    cleanup() { rm -rf "$TMPDIR"; }
    trap cleanup EXIT

    just install "$TMPDIR"

    # 1. Verify binary runs
    "$TMPDIR/bin/beamtalk" --version
    echo "✅ beamtalk --version OK"

    # 2. Verify stdlib BEAM files are present
    STDLIB_DIR="$TMPDIR/lib/beamtalk/lib/beamtalk_stdlib/ebin"
    RUNTIME_DIR="$TMPDIR/lib/beamtalk/lib/beamtalk_runtime/ebin"
    test -d "$STDLIB_DIR"
    test -d "$RUNTIME_DIR"
    echo "✅ Stdlib and runtime directories present"

    # 3. Scaffold a project, add a class, and run it end-to-end
    (cd "$TMPDIR" && "$TMPDIR/bin/beamtalk" new smoke_project)
    echo "✅ beamtalk new smoke_project OK"
    printf 'Object subclass: SmokeTest\n  class run => 21 + 21\n' > "$TMPDIR/smoke_project/src/SmokeTest.bt"
    (cd "$TMPDIR/smoke_project" && "$TMPDIR/bin/beamtalk" run SmokeTest run)
    echo "✅ beamtalk run SmokeTest>>run OK"

    echo "✅ All smoke tests passed"

# Run .btscript expression tests (ADR 0014 Phase 1, ~20s) (also available as `beamtalk test-script`)
# Accepts optional path to run a single file: just test-stdlib bootstrap-test/arithmetic.btscript
# Output: summary only (--quiet suppresses per-file lines)
[working-directory: 'stdlib']
test-stdlib *ARGS: build-stdlib
    @echo "🧪 Running stdlib tests..."
    @cargo run --bin beamtalk --quiet -- test-stdlib --no-warnings --quiet {{ ARGS }}
    @echo "✅ Stdlib tests complete"

# Run BUnit TestCase tests (ADR 0014 Phase 2)
# Accepts optional path: just test-bunit test/dictionary_test.bt
[working-directory: 'stdlib']
test-bunit *ARGS: build-stdlib
    @echo "🧪 Running BUnit tests..."
    @cargo run --bin beamtalk --quiet -- test --warnings-as-errors {{ ARGS }}
    @echo "✅ BUnit tests complete"

# Run learning guide doctests (docs/learning/ — separate from stdlib tests)
# Extracts ```beamtalk blocks from Markdown chapters and runs them via test-docs
test-learn: build-stdlib
    @echo "📚 Running learning guide doctests..."
    @cargo run --bin beamtalk --quiet -- test-docs --no-warnings --quiet docs/learning/
    @echo "✅ Learning guide tests complete"

# Note: Auto-discovers all *_tests modules. New test files are included automatically.
# Run Erlang runtime unit tests
# Output: summary only on success, full output on failure
[unix]
[working-directory: 'runtime']
test-runtime: build-stdlib
    #!/usr/bin/env bash
    set -eo pipefail
    echo "🧪 Running Erlang runtime unit tests..."
    if OUTPUT=$(BEAMTALK_NO_FILE_LOG=1 rebar3 eunit --cover=false --app=beamtalk_runtime,beamtalk_workspace 2>&1); then
        echo "$OUTPUT" | tail -2
    else
        echo "$OUTPUT"
        exit 1
    fi
    if OUTPUT=$(BEAMTALK_NO_FILE_LOG=1 rebar3 eunit --cover=false --dir=apps/beamtalk_stdlib/test 2>&1); then
        echo "$OUTPUT" | tail -2
    else
        echo "$OUTPUT"
        exit 1
    fi
    echo "✅ Runtime tests complete"

[windows]
[working-directory: 'runtime']
test-runtime: build-stdlib
    @echo "🧪 Running Erlang runtime unit tests..."
    @$env:BEAMTALK_NO_FILE_LOG = "1"; $output = rebar3 eunit '--cover=false' '--app=beamtalk_runtime,beamtalk_workspace' 2>&1 | Out-String; $exitCode = $LASTEXITCODE; if ($exitCode -ne 0) { Write-Output $output; exit $exitCode } else { ($output -split "`n") | Select-Object -Last 3 }
    @$env:BEAMTALK_NO_FILE_LOG = "1"; $output = rebar3 eunit '--cover=false' '--dir=apps/beamtalk_stdlib/test' 2>&1 | Out-String; $exitCode = $LASTEXITCODE; if ($exitCode -ne 0) { Write-Output $output; exit $exitCode } else { ($output -split "`n") | Select-Object -Last 3 }
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

# Regenerate cover_excl_mods and dialyzer exclude_mods in runtime/rebar.config
# from the actual bt@stdlib@*.beam files produced by build-stdlib.
# Run this after adding or removing Beamtalk stdlib classes.
[unix]
update-stdlib-excludes: build-stdlib
    python3 scripts/gen-stdlib-excludes.py

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
# Two-pass EUnit strategy: Run 1 covers beamtalk_runtime + beamtalk_workspace (without
# listing beamtalk_stdlib as an explicit --app, so bt@*.beam files in
# apps/beamtalk_stdlib/ebin/ stay on the code path). Run 2 covers beamtalk_stdlib Erlang
# modules via --dir. Both coverdata files are merged into eunit.coverdata before reporting.
# Generate Erlang runtime coverage
[unix]
coverage-runtime: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    cd runtime
    echo "📊 Generating Erlang runtime coverage..."
    # Run 1: runtime + workspace tests.
    # bt@*.beam files are findable via apps/beamtalk_stdlib/ebin/ (project app ebin
    # stays on the code path when beamtalk_stdlib is not listed as an explicit --app).
    if ! OUTPUT=$(rebar3 eunit --app=beamtalk_runtime,beamtalk_workspace --cover 2>&1); then
        echo "$OUTPUT"
        echo "❌ EUnit tests (runtime+workspace) failed"
        exit 1
    fi
    echo "$OUTPUT" | grep -E "Finished in|[0-9]+ tests," || true
    # Save before Run 2 overwrites eunit.coverdata.
    cp _build/test/cover/eunit.coverdata _build/test/cover/eunit_runtime.coverdata
    # Run 2: stdlib Erlang module tests (using --dir, not --app, to avoid rebar3
    # treating bt@*.beam files in the ebin as test modules).
    if ! OUTPUT=$(rebar3 eunit --dir=apps/beamtalk_stdlib/test --cover 2>&1); then
        echo "$OUTPUT"
        echo "❌ EUnit tests (stdlib) failed"
        exit 1
    fi
    echo "$OUTPUT" | grep -E "Finished in|[0-9]+ tests," || true
    # Save stdlib coverdata before the merge overwrites eunit.coverdata.
    cp _build/test/cover/eunit.coverdata _build/test/cover/eunit_stdlib.coverdata
    # Merge both coverdata files so the final report includes all modules.
    erl -noshell -eval '
        cover:start(),
        lists:foreach(fun(F) ->
            cover:import(F)
        end, filelib:wildcard("_build/test/cover/eunit_*.coverdata")),
        ok = cover:export("_build/test/cover/eunit.coverdata"),
        cover:stop(),
        init:stop().
    '
    # Remove phase files so coverage-all's wildcard does not re-import them
    # (which would double-count the EUnit data on top of the merged eunit.coverdata).
    rm -f _build/test/cover/eunit_runtime.coverdata _build/test/cover/eunit_stdlib.coverdata
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
# [working-directory: 'stdlib'] ensures @load fixture paths (e.g. test/fixtures/counter.bt)
# resolve correctly, matching the test-stdlib recipe.
[unix]
[working-directory: 'stdlib']
coverage-stdlib: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📊 Running stdlib tests with Erlang cover instrumentation..."
    echo "   (This is slower than normal stdlib tests due to cover overhead)"
    STDLIB_COVER=1 cargo run --bin beamtalk --quiet -- test-stdlib --no-warnings bootstrap-test || true
    if [ -f ../runtime/_build/test/cover/stdlib.coverdata ]; then
        SIZE=$(wc -c < ../runtime/_build/test/cover/stdlib.coverdata)
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
[unix]
clean-rust:
    @echo "🧹 Cleaning Rust artifacts..."
    # Devcontainers mount target as a volume cache; avoid deleting the mount point.
    @if command -v mountpoint >/dev/null 2>&1 && mountpoint -q target; then rm -rf target/* 2>/dev/null || true; else cargo clean --quiet; fi
    @echo "  ✅ Cleaned target/"

# Clean Rust build artifacts
[windows]
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
    if [ ! -f target/release/beamtalk-exec ]; then
        echo "❌ beamtalk-exec binary not found. Run 'just build-release' first."
        exit 1
    fi

    # Binaries
    install -d "${PREFIX}/bin"
    install -m 755 target/release/beamtalk "${PREFIX}/bin/beamtalk"
    install -m 755 target/release/beamtalk-compiler-port "${PREFIX}/bin/beamtalk-compiler-port"
    install -m 755 target/release/beamtalk-lsp "${PREFIX}/bin/beamtalk-lsp"
    install -m 755 target/release/beamtalk-mcp "${PREFIX}/bin/beamtalk-mcp"
    install -m 755 target/release/beamtalk-exec "${PREFIX}/bin/beamtalk-exec"

    # OTP application ebin directories
    for app in beamtalk_runtime beamtalk_workspace beamtalk_compiler beamtalk_stdlib jsx cowboy cowlib ranch gun yamerl; do
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

    # Stdlib sources for LSP/tooling navigation
    STDLIB_SOURCE_SRC="stdlib/src"
    if [ -d "${STDLIB_SOURCE_SRC}" ] && compgen -G "${STDLIB_SOURCE_SRC}/*.bt" > /dev/null; then
        install -d "${PREFIX}/share/beamtalk/stdlib/src"
        install -m 644 "${STDLIB_SOURCE_SRC}"/*.bt "${PREFIX}/share/beamtalk/stdlib/src/"
    fi

    echo "✅ Installed beamtalk to ${PREFIX}"
    echo "   Binary:  ${PREFIX}/bin/beamtalk"
    echo "   LSP:     ${PREFIX}/bin/beamtalk-lsp"
    echo "   MCP:     ${PREFIX}/bin/beamtalk-mcp"
    echo "   Runtime: ${PREFIX}/lib/beamtalk/lib/"
    echo "   Sources: ${PREFIX}/share/beamtalk/stdlib/src/"

# Unix-only: uses rm -f/-rf
# Uninstall beamtalk from PREFIX (default: /usr/local)
[unix]
uninstall PREFIX="/usr/local":
    #!/usr/bin/env bash
    set -euo pipefail
    PREFIX="{{PREFIX}}"
    echo "🗑️  Uninstalling beamtalk from ${PREFIX}..."
    rm -f "${PREFIX}/bin/beamtalk" "${PREFIX}/bin/beamtalk-compiler-port" "${PREFIX}/bin/beamtalk-lsp" "${PREFIX}/bin/beamtalk-mcp" "${PREFIX}/bin/beamtalk-exec"
    rm -rf "${PREFIX}/lib/beamtalk"
    rm -rf "${PREFIX}/share/beamtalk"
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
    cd editors/vscode
    npm ci --quiet
    npx --yes @vscode/vsce package --target "{{target}}" --out "../../beamtalk-{{target}}.vsix"
    echo "✅ VS Code extension: beamtalk-{{target}}.vsix"

# Usage: just dist-vscode-platform win32-x64
# Build VS Code extension for a specific platform target
[windows]
dist-vscode-platform target:
    @echo "📦 Building VS Code extension for {{target}}..."
    if (!(Get-Command npm -ErrorAction SilentlyContinue)) { Write-Error "npm not found"; exit 1 }
    Push-Location editors\vscode; try { npm ci --quiet; if ($LASTEXITCODE -ne 0) { throw "npm ci failed" } } finally { Pop-Location }
    Push-Location editors\vscode; try { npx --yes @vscode/vsce package --target "{{target}}" --out "..\..\beamtalk-{{target}}.vsix"; if ($LASTEXITCODE -ne 0) { throw "vsce package failed" } } finally { Pop-Location }
    @echo "✅ VS Code extension: beamtalk-{{target}}.vsix"

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
    echo "   VS Code extension: beamtalk-*.vsix"

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
    Copy-Item target/release/beamtalk-exec.exe dist/bin/
    foreach ($app in @('beamtalk_runtime','beamtalk_workspace','beamtalk_compiler','jsx','cowboy','cowlib','ranch')) { $src = "runtime/_build/default/lib/$app/ebin"; if (!(Test-Path "$src/*.beam")) { Write-Error "No .beam files in $src"; exit 1 }; New-Item -ItemType Directory -Force -Path "dist/lib/beamtalk/lib/$app/ebin" | Out-Null; Copy-Item "$src/*.beam" "dist/lib/beamtalk/lib/$app/ebin/"; Copy-Item "$src/*.app" "dist/lib/beamtalk/lib/$app/ebin/" -ErrorAction SilentlyContinue }
    $stdlib = "runtime/apps/beamtalk_stdlib/ebin"; if (!(Test-Path "$stdlib/*.beam")) { Write-Error "No stdlib .beam files"; exit 1 }; New-Item -ItemType Directory -Force -Path "dist/lib/beamtalk/lib/beamtalk_stdlib/ebin" | Out-Null; Copy-Item "$stdlib/*.beam" "dist/lib/beamtalk/lib/beamtalk_stdlib/ebin/"; Copy-Item "$stdlib/*.app" "dist/lib/beamtalk/lib/beamtalk_stdlib/ebin/" -ErrorAction SilentlyContinue
    if (Test-Path "stdlib/src/*.bt") { New-Item -ItemType Directory -Force -Path "dist/share/beamtalk/stdlib/src" | Out-Null; Copy-Item "stdlib/src/*.bt" "dist/share/beamtalk/stdlib/src/" }
    just dist-vscode
    @echo "✅ Distribution ready in dist/"
    @echo "   Run: dist\bin\beamtalk.exe repl"
    @echo "   VS Code extension: beamtalk-*.vsix"

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
    cargo run --bin beamtalk -- doc stdlib/src/ --output docs/api/

# Check documentation for broken links
docs-check:
    @echo "🔍 Checking documentation..."
    cargo doc --workspace --no-deps

