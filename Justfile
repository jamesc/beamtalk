# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Beamtalk build tasks
# Run `just` to see all available recipes
# Run `just <recipe>` to execute a specific task

# Use bash on Unix and PowerShell on Windows for all commands
set shell := ["bash", "-uc"]
set windows-shell := ["powershell.exe", "-NoLogo", "-NoProfile", "-Command"]

# BT-3235: pin epmd's bind address for every recipe that shells out to
# rebar3/mix/erl (build-erlang, test-runtime, perf, dialyzer, ...). epmd is a
# per-user singleton daemon: whichever process starts it first wins its bind
# posture for the rest of the session, and epmd's own default (no -address)
# is *all* interfaces (ADR 0091 Decision 5, finding F1) — the same gap
# `beamtalk-workspace`'s `resolve_epmd_address` and `startup_command.rs`
# already close for workspace-node launches. On a shared dev box running
# concurrent toolchain invocations (several agent worktrees building/testing
# at once), an unpinned `rebar3 eunit`/`compile` can auto-start a genuinely
# promiscuous epmd before this pin ever gets a chance to apply — closing the
# gap here removes this project's own tooling as a source of that. Respects
# an operator-set ERL_EPMD_ADDRESS (e.g. a trusted private-network address)
# rather than clobbering it, matching `resolve_epmd_address`'s own fallback.
export ERL_EPMD_ADDRESS := env_var_or_default("ERL_EPMD_ADDRESS", "127.0.0.1")

# Default recipe (list all tasks)
default:
    @just --list

# Diagnose the dev environment (toolchain, cloud egress proxy, hex-bridge,
# BEAM package-fetch reachability). Fast, safe to run anytime; the decisive
# check is whether Erlang can actually fetch packages through the bridge —
# `curl` working does NOT mean rebar3/mix will. See scripts/cloud-doctor.sh.
[unix]
doctor:
    @bash scripts/cloud-doctor.sh

# ═══════════════════════════════════════════════════════════════════════════
# Quick Commands (CI equivalents)
# ═══════════════════════════════════════════════════════════════════════════

# Run local CI checks (combination of GitHub Actions check + test jobs)
# Matches: just build clippy fmt-check test (check job)
#        + just test-integration test-mcp test-repl-protocol (test job extras)
#        + dialyzer if Erlang changed (skipped on Windows - known PATH issue)
[unix]
ci: build lint test verify-threaded-ir test-integration test-mcp test-parity test-repl-protocol check-corpus check-generated-builtins check-codegen-boundary check-surface-drift test-grammar

[windows]
ci: build clippy fmt-check-rust test verify-threaded-ir test-integration test-mcp test-parity test-repl-protocol check-surface-drift

# Run local CI checks, skipping the slow workspace/MCP/REPL-protocol/parity
# suites when the diff (vs origin/main, plus uncommitted changes) doesn't touch
# paths that could affect them. GitHub Actions runs test-integration and
# test-parity as their own jobs in parallel with everything else regardless —
# skipping them here trades a same-machine safety net for faster local
# iteration, not a coverage gap. Force `just ci` instead when in doubt.
[unix]
ci-changed:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just lint
    # BT-3149: `verify-threaded-ir`'s prerequisites (test-stdlib, test-bunit)
    # are already run here as part of `test`'s own dependency graph, in the
    # dev profile (debug_assertions on) — any ThreadedIr (ADR 0111) `verify()`
    # violation across the codegen paths this changed set touches already
    # hard-panics right here. No separate `just verify-threaded-ir` call
    # follows: same "would just recompile the same corpus for zero new
    # detection" reasoning `verify-threaded-ir`'s own doc comment gives for
    # why ci.yml's `test-beam` job doesn't add one either — it applies
    # doubly here, since (unlike that job) `test` isn't even scoped by the
    # changed-files check below.
    just test
    just check-corpus
    just check-generated-builtins
    just check-codegen-boundary
    just check-surface-drift

    merge_base="$(git merge-base HEAD origin/main 2>/dev/null || true)"
    if [[ -n "$merge_base" ]]; then
      changed_files="$(git diff --name-only "$merge_base"...HEAD; git status --porcelain | awk '{print $2}')"
    else
      changed_files="$(git status --porcelain | awk '{print $2}')"
    fi

    matches_any() {
      local file
      while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        for pattern in "$@"; do
          # shellcheck disable=SC2053
          [[ "$file" == $pattern ]] && return 0
        done
      done <<< "$changed_files"
      return 1
    }

    # Scoped to the actual workspace/REPL server surface, not all of runtime/
    # or stdlib/ — those are already covered by `just test` (test-runtime,
    # test-stdlib, test-bunit) above, which run regardless.
    WORKSPACE_PATTERNS=(
      'runtime/apps/beamtalk_workspace/*'
      'crates/beamtalk-cli/src/commands/workspace/*'
      'crates/beamtalk-cli/src/repl_startup.rs'
      'crates/beamtalk-cli/tests/repl_protocol.rs'
      'crates/beamtalk-mcp/*' 'tests/repl-protocol/*'
    )
    PARITY_PATTERNS=(
      'crates/beamtalk-parity-tests/*' 'crates/beamtalk-lsp/*'
      'crates/beamtalk-mcp/*' 'runtime/apps/beamtalk_workspace/*'
      'crates/beamtalk-cli/src/commands/workspace/*'
    )

    if matches_any "${WORKSPACE_PATTERNS[@]}"; then
      just test-integration
      just test-mcp
      just test-repl-protocol
    else
      echo "⏭️  workspace/MCP/REPL-protocol paths untouched — skipping test-integration/test-mcp/test-repl-protocol (CI's test-integration job still runs them)"
    fi

    if matches_any "${PARITY_PATTERNS[@]}"; then
      just test-parity
    else
      echo "⏭️  REPL/MCP/CLI/LSP paths untouched — skipping test-parity (CI's test-parity job still runs it)"
    fi

# Windows: path-based skip logic below is unix-only for now — run the full suite.
[windows]
ci-changed: ci

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

# ═══════════════════════════════════════════════════════════════════════════
# LiveView IDE (editors/liveview — Attach topology, BT-2401)
# ═══════════════════════════════════════════════════════════════════════════

# Fetch the LiveView app's Mix deps (routes through the hex bridge in cloud
# sessions, where HEX_MIRROR points at the local proxy). Run once after clone.
[unix]
[working-directory: 'editors/liveview']
web-setup:
    @echo "📦 Fetching LiveView (editors/liveview) deps..."
    ELIXIR_ERL_OPTIONS="${ELIXIR_ERL_OPTIONS:-+fnu}" mix deps.get
    @echo "✅ Deps fetched. Run 'cd editors/liveview && mix assets.setup' once to"
    @echo "   install the esbuild + tailwind binaries (downloaded on first use)."

# Run the LiveView IDE against a running workspace (Attach topology).
# Discovers the workspace's node name + cookie via the CLI, exports
# BT_WORKSPACE_NODE / BT_WORKSPACE_COOKIE, then starts Phoenix on :4000.
#   just web <workspace-name>
[unix]
web name:
    #!/usr/bin/env bash
    set -euo pipefail
    # `|| true` so a missing/stopped workspace (non-zero exit) falls through to
    # the friendly message below instead of aborting under `set -e`/pipefail.
    node=$(cargo run --bin beamtalk --quiet -- workspace status "{{name}}" 2>/dev/null | awk '/^Node:/ {print $2}' || true)
    if [ -z "${node:-}" ]; then
      echo "❌ Workspace '{{name}}' is not running." >&2
      echo "   Start it first: beamtalk workspace create {{name}} --background --persistent" >&2
      exit 1
    fi
    # The cookie lives at ~/.beamtalk/workspaces/<id>/cookie, where <id> is the
    # node short name minus the beamtalk_workspace_ prefix.
    id="${node#beamtalk_workspace_}"; id="${id%@*}"
    cookie_file="${HOME}/.beamtalk/workspaces/${id}/cookie"
    if [ ! -f "${cookie_file}" ]; then
      echo "❌ Workspace cookie not found at ${cookie_file}" >&2
      exit 1
    fi
    export BT_WORKSPACE_NODE="${node}"
    export BT_WORKSPACE_COOKIE="$(cat "${cookie_file}")"
    export ELIXIR_ERL_OPTIONS="${ELIXIR_ERL_OPTIONS:-+fnu}"
    echo "🌐 LiveView IDE → ${node}  (http://localhost:4000)"
    cd editors/liveview
    exec mix phx.server

# Run the LiveView IDE as a NON-LOCALHOST authenticated front (ADR 0091).
# Like `web`, but starts the server in prod mode behind OIDC + HTTPS, with the
# dist cookie provisioned to Phoenix as an INFRASTRUCTURE SECRET (env), never
# exposed to a browser. Distribution stays internal (co-located/loopback by
# default; see docs/deployment/remote-liveview-ide.md for the private-interface
# option). Prereqs (fail fast if missing):
#   SECRET_KEY_BASE  — `mix phx.gen.secret` (signs the session cookie)
#   PHX_HOST         — the IDE's public hostname (behind your TLS terminator)
#   OIDC config      — ~/.beamtalk/ide.toml or BT_OIDC_* env (else boot fails closed)
#   just web-remote <workspace-name>
[unix]
web-remote name:
    #!/usr/bin/env bash
    set -euo pipefail
    : "${SECRET_KEY_BASE:?set SECRET_KEY_BASE — generate with: mix phx.gen.secret}"
    : "${PHX_HOST:?set PHX_HOST to the IDE's public hostname (TLS-terminated)}"
    node=$(cargo run --bin beamtalk --quiet -- workspace status "{{name}}" 2>/dev/null | awk '/^Node:/ {print $2}' || true)
    if [ -z "${node:-}" ]; then
      echo "❌ Workspace '{{name}}' is not running." >&2
      echo "   Start it first: beamtalk workspace create {{name}} --background --persistent" >&2
      exit 1
    fi
    id="${node#beamtalk_workspace_}"; id="${id%@*}"
    cookie_file="${HOME}/.beamtalk/workspaces/${id}/cookie"
    if [ ! -f "${cookie_file}" ]; then
      echo "❌ Workspace cookie not found at ${cookie_file}" >&2
      exit 1
    fi
    # The cookie is an infra secret shared between the two trusted hosts — it is
    # set in the server's env and used ONLY for the Phoenix↔workspace dist link;
    # it is never placed in a page, assign, or URL (browser ↔ Phoenix is HTTPS).
    export BT_WORKSPACE_NODE="${node}"
    export BT_WORKSPACE_COOKIE="$(cat "${cookie_file}")"
    export ELIXIR_ERL_OPTIONS="${ELIXIR_ERL_OPTIONS:-+fnu}"
    export PHX_SERVER=true
    export MIX_ENV=prod
    export PORT="${PORT:-8443}"
    echo "🔒 Remote LiveView IDE → ${node}  (https://${PHX_HOST}:${PORT}, OIDC)"
    echo "   Dist link is internal infrastructure; keep it off untrusted networks (ADR 0091)."
    cd editors/liveview
    mix deps.get --only prod
    mix assets.deploy
    exec mix phx.server

# Build a self-contained, ERTS-embedded release of the LiveView IDE (BT-2513).
# Bundles JS+CSS (minified, digested) and produces a runnable release in
# dist-liveview/ at the repo root — no Elixir/Mix needed on the host. This is
# the artifact the packaging lane ships (BT-2515 archive, BT-2516 Docker),
# separate from the core `just dist` bundle (BT-2512), which is why it does
# not share the dist/ directory (`just dist` wipes it).
#   just dist-liveview
[unix]
[working-directory: 'editors/liveview']
dist-liveview:
    #!/usr/bin/env bash
    set -euo pipefail
    export ELIXIR_ERL_OPTIONS="${ELIXIR_ERL_OPTIONS:-+fnu}"
    export MIX_ENV=prod
    echo "📦 Building LiveView IDE release (bt_attach)..."
    mix deps.get --only prod
    mix assets.setup
    mix assets.deploy
    rm -rf ../../dist-liveview
    mix release --overwrite --path ../../dist-liveview
    # phx.digest (part of assets.deploy) writes digested copies and
    # cache_manifest.json into priv/static/ alongside the tracked originals.
    # The release embeds its own copies, so restore the source tree.
    mix phx.digest.clean --all
    echo "✅ LiveView IDE release ready in dist-liveview/"
    echo "   Run: dist-liveview/bin/server <workspace-id>   (resolves node+cookie like 'just web')"
    echo "   Or:  PHX_SERVER=true SECRET_KEY_BASE=... dist-liveview/bin/bt_attach start"

# Windows counterpart of dist-liveview above (BT-2988) — same build, via
# PowerShell (a shebang recipe, like the bash version above, so each step
# shares one process/cwd — `just`'s non-shebang Windows recipes start a fresh
# shell per line, which would silently drop MIX_ENV between steps here).
#
# No `ELIXIR_ERL_OPTIONS=+fnu`: that flag tells Erlang to interpret filenames
# as UTF-8 on filesystems that don't tag an encoding (Linux); Windows
# filenames are always native UTF-16, so the flag is a Unix-only concern —
# not verified against a real Windows `mix release` boot (no Windows sandbox
# was available to develop this against), flagged per BT-2988's "document
# Windows-specific gaps" acceptance criterion rather than guessed at silently.
[windows]
[working-directory: 'editors/liveview']
dist-liveview:
    #!powershell.exe
    $ErrorActionPreference = "Stop"
    $env:MIX_ENV = "prod"
    Write-Output "📦 Building LiveView IDE release (bt_attach)..."
    mix deps.get --only prod
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    mix assets.setup
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    mix assets.deploy
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    if (Test-Path ../../dist-liveview) { Remove-Item -Recurse -Force ../../dist-liveview }
    mix release --overwrite --path ../../dist-liveview
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    # phx.digest (part of assets.deploy) writes digested copies and
    # cache_manifest.json into priv/static/ alongside the tracked originals.
    # The release embeds its own copies, so restore the source tree.
    mix phx.digest.clean --all
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    Write-Output "✅ LiveView IDE release ready in dist-liveview/"
    Write-Output "   Run: PORT=... BT_ATTACH_BIND_IP=... BT_ATTACH_NODE_SUFFIX=... BT_WORKSPACE_NODE=... BT_WORKSPACE_COOKIE=... SECRET_KEY_BASE=... PHX_SERVER=true dist-liveview\bin\bt_attach.bat start"
    Write-Output "   (no bin\server equivalent — beamtalk-desktop-broker's Windows spawn path sets these directly, BT-2988)"

# Usage: just dist-desktop-platform appimage,deb
# Build the desktop app (Tauri shell, ADR 0097) bundle(s) for specific Tauri
# bundle targets (BT-2987). Needs the Tauri toolchain — `cargo install
# tauri-cli --version "^2.0.0"` plus, on Linux, the webkit2gtk-4.1/glib/gtk3
# dev packages listed in desktop/README.md. Bundles a freshly-built
# dist-liveview/ release as a Tauri resource, so run `just dist-liveview`
# first (or after any editors/liveview change). Same recipe
# .github/workflows/desktop-release.yml runs in CI, per platform.
#
# The app version is overridden from the repo's VERSION file via `--config`
# (a merge patch, not an in-place edit of tauri.conf.json — its own committed
# "0.1.0" is just a fallback for `cargo tauri dev`) so shipped .deb/.dmg/.app
# metadata never drifts from the version every other release artifact uses.
[unix]
dist-desktop-platform bundles:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ ! -x dist-liveview/bin/server ]; then
        echo "❌ dist-liveview/bin/server not found. Run 'just dist-liveview' first."
        exit 1
    fi
    if ! command -v cargo-tauri >/dev/null 2>&1; then
        echo "❌ cargo-tauri not found. Install: cargo install tauri-cli --version \"^2.0.0\""
        exit 1
    fi
    VERSION="$(tr -d '[:space:]' < VERSION)"
    echo "📦 Building desktop app v${VERSION} ({{bundles}})..."
    cd desktop
    cargo tauri build --bundles "{{bundles}}" --config "{\"version\":\"${VERSION}\"}"
    echo "✅ Desktop app bundle(s) in desktop/src-tauri/target/release/bundle/"

# Usage: just dist-desktop-platform msi,nsis
# Windows counterpart of dist-desktop-platform above (BT-2988) — same recipe,
# via PowerShell. Tauri's Windows bundle targets are `msi` (WiX) and `nsis`
# (NSIS installer .exe); see desktop-release.yml's Windows leg for which of
# these actually ship. Checks for `dist-liveview\bin\bt_attach.bat` (the
# Windows entry point — no `bin\server`, BT-2988) rather than `bin/server`.
#
# Passes `--config` a temp *file path*, not an inline JSON string
# (adversarial-review follow-up): Windows PowerShell 5.1 (what
# `powershell.exe` is — not PowerShell 7's `pwsh`) does not re-escape
# embedded double quotes when building a native command's argv
# (`PSNativeCommandArgumentPassing` didn't exist before 7.2), so the
# bash sibling recipe's inline-JSON trick
# (`--config "{\"version\":\"$VERSION\"}"`) reaches `cargo-tauri.exe` here
# as literal `{version:0.4.0}` — invalid JSON, breaking the whole Windows
# build with a confusing serde error. Writing the JSON to a file with a
# PowerShell cmdlet (`Set-Content`, not a native exe) sidesteps that argv
# escaping entirely; `--config <path>` is Tauri CLI's documented alternate
# form for exactly this.
[windows]
dist-desktop-platform bundles:
    #!powershell.exe
    $ErrorActionPreference = "Stop"
    if (!(Test-Path dist-liveview/bin/bt_attach.bat)) {
        Write-Output "❌ dist-liveview/bin/bt_attach.bat not found. Run 'just dist-liveview' first."
        exit 1
    }
    if (!(Get-Command cargo-tauri -ErrorAction SilentlyContinue)) {
        Write-Output '❌ cargo-tauri not found. Install: cargo install tauri-cli --version "^2.0.0"'
        exit 1
    }
    $version = (Get-Content VERSION -Raw).Trim()
    Write-Output "📦 Building desktop app v$version ({{bundles}})..."
    Set-Location desktop
    $configPath = Join-Path $env:TEMP "beamtalk-tauri-config-$PID.json"
    Set-Content -Path $configPath -Value "{`"version`":`"$version`"}" -NoNewline
    try {
        cargo tauri build --bundles "{{bundles}}" --config $configPath
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    } finally {
        Remove-Item -Path $configPath -ErrorAction SilentlyContinue
    }
    Write-Output "✅ Desktop app bundle(s) in desktop/src-tauri/target/release/bundle/"

# Build the desktop app bundle for the host platform (BT-2987), auto-picking
# the bundle targets Tauri supports there (.AppImage/.deb on Linux, .app/.dmg
# on macOS — Windows is BT-2988).
[unix]
dist-desktop:
    #!/usr/bin/env bash
    set -euo pipefail
    case "$(uname -s)" in
        Linux)  just dist-desktop-platform appimage,deb ;;
        Darwin) just dist-desktop-platform app,dmg ;;
        *)      echo "❌ Unsupported platform: $(uname -s)"; exit 1 ;;
    esac

# Build the desktop app bundle for Windows (BT-2988), auto-picking Tauri's
# Windows bundle targets (msi + nsis — see dist-desktop-platform above).
[windows]
dist-desktop:
    just dist-desktop-platform msi,nsis

# Run beamtalk-desktop's (desktop/src-tauri, the Tauri app crate) own
# `#[cfg(test)]` unit tests (BT-3061). NOT part of `just test`/`just ci` —
# this crate is deliberately excluded from the root Cargo workspace (see
# root Cargo.toml's `exclude` and desktop/README.md) because it needs the
# Tauri toolchain (webkit2gtk-4.1/glib/gtk3 dev packages on Linux; WebView2
# on Windows and WKWebView on macOS need no extra packages) that the rest
# of the workspace doesn't require. A dedicated CI job in ci.yml installs
# that toolchain per-platform and calls this recipe directly; run it by
# hand once you have the same Tauri prerequisites `cargo tauri dev` needs
# (desktop/README.md).
#
# tauri-build's config validation (invoked from this crate's build.rs)
# checks that tauri.conf.json's `bundle.resources` source path
# (`../../dist-liveview`) exists on disk, but `cargo test` never invokes
# the actual bundler — an empty placeholder directory satisfies the check
# without a full `just dist-liveview` release build (Erlang/Elixir/Node),
# which would make this recipe as slow as `just dist-desktop-platform` for
# no test-coverage benefit. Verified locally: an empty dist-liveview/ is
# sufficient for `cargo test` to build and run this crate's unit tests.
[unix]
test-desktop:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p dist-liveview
    cd desktop/src-tauri
    cargo test --locked

[windows]
test-desktop:
    #!powershell.exe
    $ErrorActionPreference = "Stop"
    New-Item -ItemType Directory -Force -Path dist-liveview | Out-Null
    Set-Location desktop/src-tauri
    cargo test --locked
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

# Ensure the loopback hex-bridge proxy is up before any rebar3/mix dep fetch.
# Cloud containers only (gated on CLAUDE_CODE_REMOTE): a session can outlive the
# SessionStart launch, and a dead bridge makes `rebar3 compile` fail to fetch
# Hex packages. No-op on local/dev machines. See scripts/hex-bridge-proxy.py.
[unix]
_ensure-hex-bridge:
    #!/usr/bin/env bash
    set -euo pipefail
    [[ "${CLAUDE_CODE_REMOTE:-}" == "true" ]] || exit 0
    port="${HEX_BRIDGE_PORT:-18081}"
    script="scripts/hex-bridge-proxy.py"
    [[ -f "${script}" ]] || exit 0
    (exec 3<>"/dev/tcp/127.0.0.1/${port}") 2>/dev/null && exit 0
    command -v python3 >/dev/null 2>&1 || { echo "⚠ python3 not found — hex-bridge proxy cannot start" >&2; exit 0; }
    echo "↻ hex-bridge proxy down — starting on :${port}"
    setsid python3 "${script}" >/dev/null 2>&1 </dev/null &
    disown 2>/dev/null || true
    for _ in {1..20}; do
      (exec 3<>"/dev/tcp/127.0.0.1/${port}") 2>/dev/null && exit 0
      sleep 0.1
    done
    # Deliberately fall through to exit 0: let `rebar3 compile` fail with its own
    # connection error (more actionable) rather than aborting the build here.
    echo "⚠ hex-bridge proxy did not come up on :${port}" >&2

# Windows has no cloud-sandbox hex bridge; the dependency must still resolve so
# `build-erlang` parses. No-op.
[windows]
_ensure-hex-bridge:

# Build Erlang runtime. Lock-guarded on Linux (BT-2471): a fresh remote
# session's SessionStart hook may be running `rebar3 compile` against this
# same `runtime/_build/` in the background at the same moment — rebar3,
# unlike Cargo's locked `target/`, has no built-in guard against two
# concurrent writers to one output tree. `flock` (blocking, not `-n`) makes
# this recipe wait for that background warmer rather than race it. The
# background warmer only ever runs on `CLAUDE_CODE_REMOTE=true` Linux
# sandboxes (see `.claude/hooks/worktree-init.sh`), so macOS and Windows
# have no concurrent writer to guard against — and no preinstalled `flock`
# binary either (it's Linux-only; the macOS CI runner doesn't have it), so
# both skip the lock.
[linux]
[working-directory: 'runtime']
build-erlang: _ensure-hex-bridge
    @echo "🔨 Building Erlang runtime..."
    @flock .rebar3-compile.lock rebar3 compile
    @echo "✅ Erlang build complete"

[macos]
[working-directory: 'runtime']
build-erlang: _ensure-hex-bridge
    @echo "🔨 Building Erlang runtime..."
    @rebar3 compile
    @echo "✅ Erlang build complete"

[windows]
[working-directory: 'runtime']
build-erlang: _ensure-hex-bridge
    @echo "🔨 Building Erlang runtime..."
    @rebar3 compile
    @echo "✅ Erlang build complete"

# Build standard library (stdlib/src/*.bt → BEAM, incremental — skips if up to date)
build-stdlib: build-rust build-erlang
    @echo "🔨 Building standard library..."
    @cargo run --bin beamtalk --quiet -- build-stdlib --quiet --warnings-as-errors
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
        if (cd "${dir}" && cargo run --bin beamtalk --quiet -- test --warnings-as-errors 2>&1); then
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

# Check that the checked-in stdlib builtin-class artifacts (generated_builtins.rs
# and beamtalk_generated_builtins.hrl) are up to date (BT-3085 CI freshness
# check). Both are regenerated by build-stdlib, which `just build` (this
# recipe's transitive dependency, already run earlier in `ci`/`ci-changed`)
# has already run — so this only needs to diff, not regenerate.
[unix]
check-generated-builtins: build
    #!/usr/bin/env bash
    set -euo pipefail
    if ! git diff --exit-code \
        crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs \
        runtime/apps/beamtalk_runtime/include/beamtalk_generated_builtins.hrl \
        > /dev/null 2>&1; then
        echo "❌ generated stdlib builtin-class artifacts are out of date — run 'just build-stdlib' and commit"
        exit 1
    fi
    echo "✅ generated stdlib builtin-class artifacts are up to date"

# Guard the BT-3362 crate split's build-time win: beamtalk-lsp and beamtalk-lint
# analyze code but never generate it, so neither should depend on beamtalk-codegen
# (~90k lines) directly or transitively. `cargo tree -p <pkg> -i beamtalk-codegen`
# exits non-zero with "did not match any packages" when no such edge exists, and
# exits 0 and prints the dependency path when one does — so success here means a
# regression. beamtalk-mcp is deliberately NOT checked: it depends on beamtalk-cli
# for manifest/build-layout reuse (BT-2823), and beamtalk-cli genuinely drives
# codegen, so beamtalk-mcp compiling beamtalk-codegen transitively is expected,
# pre-existing coupling, not a regression (docs/development/architecture-principles.md §1).
[unix]
check-codegen-boundary:
    #!/usr/bin/env bash
    set -uo pipefail
    fail=0
    if ! cargo tree -p beamtalk-codegen >/dev/null 2>&1; then
        echo "❌ package 'beamtalk-codegen' no longer resolves — update check-codegen-boundary's target package"
        exit 1
    fi
    for pkg in beamtalk-lsp beamtalk-lint; do
        if ! cargo tree -p "$pkg" >/dev/null 2>&1; then
            echo "❌ package '$pkg' no longer resolves — update check-codegen-boundary's package list"
            fail=1
            continue
        fi
        output="$(cargo tree -p "$pkg" -i beamtalk-codegen 2>&1)"
        status=$?
        if [[ $status -eq 0 ]]; then
            echo "❌ $pkg depends on beamtalk-codegen — this regresses the BT-3362 crate split:"
            echo "$output"
            fail=1
        elif [[ "$output" != *"did not match any packages"* ]]; then
            echo "❌ unexpected error checking whether $pkg depends on beamtalk-codegen:"
            echo "$output"
            fail=1
        fi
    done
    if [[ $fail -ne 0 ]]; then
        exit 1
    fi
    echo "✅ beamtalk-lsp/beamtalk-lint do not depend on beamtalk-codegen"

# Check that REPL ops × CLI × MCP × LSP coverage matches docs/development/surface-parity.md (BT-2082).
# Fails if a new REPL op landed without a parity-doc row, or a binding listed
# in the doc has no corresponding code artifact (MCP tool / REPL meta-cmd / LSP capability).
check-surface-drift:
    @echo "🔎 Checking surface parity drift..."
    @cargo run -p beamtalk-surface-drift --quiet

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
# Benchmarks
# ═══════════════════════════════════════════════════════════════════════════

# Run criterion benchmarks for the compiler pipeline
bench:
    @echo "📊 Running criterion benchmarks..."
    @cargo bench -p beamtalk-core
    @echo "✅ Benchmarks complete (see target/criterion for HTML reports)"

# ═══════════════════════════════════════════════════════════════════════════
# Lint and Format
# ═══════════════════════════════════════════════════════════════════════════

# Lint Elixir: format check
lint-elixir: fmt-check-elixir

# Run all linting and formatting checks
lint: lint-rust lint-erlang lint-js lint-elixir lint-beamtalk lint-workaround-comments lint-binary-literal-encoding

# Lint: reject non-ASCII inside Erlang binary literals that lack /utf8 (BT-3026).
# Binary literals are bytes, so `<<"—">>` truncates U+2014 to 0x14 (a DC4 control
# character) with no compiler warning, and the mangled text reaches the user.
#
# To clear a failure: rewrite the literal in ASCII (use `;` where an em-dash
# joined two clauses), or append `/utf8` when the character is genuinely needed.
# Unix-only (the escript shells out to `git ls-files`); Linux CI covers it, so
# the Windows variant is a no-op, consistent with the other lint splits.
[unix]
lint-binary-literal-encoding:
    @escript scripts/ci/lint-binary-literal-encoding.escript

[windows]
lint-binary-literal-encoding:
    @echo "lint-binary-literal-encoding: skipped on Windows (covered by Linux CI)"

# Ratchet lint: flag workaround/limitation comments lacking a BT-NNNN tracking
# reference (BT-2347). Ships with an allowlist snapshot of pre-existing offenders
# (scripts/ci/workaround-comments-allowlist.txt) so CI is green on introduction;
# only NEW unreferenced workaround comments fail.
#
# To clear a failure: file/find a tracking issue and add its `BT-NNNN` reference
# to the offending comment line (or an adjacent line). To regenerate the
# allowlist (e.g. for a genuine false positive), run with `--update`:
#   scripts/ci/lint-workaround-comments.sh --update
# Unix-only (shells out to bash); Linux CI covers it, so the Windows variant
# is a no-op, consistent with the `[windows] ci`/`fmt-check` splits.
[unix]
lint-workaround-comments:
    @bash scripts/ci/lint-workaround-comments.sh

[windows]
lint-workaround-comments:
    @echo "lint-workaround-comments: skipped on Windows (covered by Linux CI)"

# Lint Beamtalk: formatting check
lint-beamtalk: fmt-check-beamtalk

# Lint Rust: clippy + formatting check
lint-rust: clippy fmt-check-rust

# Lint Erlang: Dialyzer type checking + format check + generated spec validation
lint-erlang: dialyzer dialyzer-specs fmt-check-erlang

# Lint JS/TS: Biome lint + format check
[working-directory: 'editors/vscode']
lint-js: fmt-check-js
    @echo "🔍 Running Biome lint..."
    npm run lint
    @echo "✅ Biome lint passed"

# BT-3261: TextMate section-divider grammar vs. parse_divider_name conformance.
# Runs the comment.line.double-slash.section-divider.beamtalk regex (parsed
# straight out of beamtalk.tmLanguage.json, never a hand-copied duplicate)
# through the real Oniguruma engine (vscode-oniguruma) against the shared
# fixture crates/beamtalk-core/tests/fixtures/section_divider_grammar_cases.json
# — the same fixture crates/beamtalk-core/tests/section_divider_grammar_conformance.rs
# checks against parse_divider_name directly, so the two recognizers can't
# silently drift apart again.
[unix]
[working-directory: 'editors/vscode']
test-grammar:
    @echo "🔍 Running TextMate section-divider grammar conformance test..."
    npm ci --quiet
    npm run test:grammar
    @echo "✅ TextMate section-divider grammar conformance test passed"

[windows]
test-grammar:
    @echo "test-grammar: skipped on Windows (covered by Linux CI)"

# Run clippy (Rust linter) - warnings are errors
clippy:
    @echo "🔍 Running clippy..."
    @cargo clippy --all-targets --quiet -- -D warnings
    @echo "✅ Clippy passed"

# Check Rust code formatting
fmt-check-rust:
    @echo "📋 Checking Rust formatting..."
    cargo fmt --all -- --check

# Check Elixir code formatting (LiveView IDE)
# `mix format` needs fetched deps (import_deps: [:phoenix] + HTML formatter
# plugin), so fetch them first — mirrors `fmt-elixir` and `fmt-check-js`'s
# `npm ci`, keeping `just lint-elixir` / the pre-push hook fresh-checkout-safe.
[working-directory: 'editors/liveview']
fmt-check-elixir:
    @echo "📋 Checking Elixir formatting..."
    mix deps.get --quiet
    mix format --check-formatted
    @echo "✅ Elixir formatting check passed"

# Check all code formatting
# Elixir formatting is enforced by the dedicated `liveview` CI job (which runs
# `mix deps.get` first, required by `import_deps: [:phoenix]`). The cross-platform
# `check` job has no Elixir deps fetched, so `fmt-check-elixir` is excluded here
# and run via `just lint-elixir` / `just fmt-check-elixir` locally instead.
[unix]
fmt-check: fmt-check-rust fmt-check-erlang fmt-check-js fmt-check-beamtalk

# Windows: skip Erlang, JS, and Elixir format checks (platform-agnostic, covered by Linux CI)
[windows]
fmt-check: fmt-check-rust fmt-check-beamtalk

# Format all Rust code
fmt-rust:
    @echo "✨ Formatting Rust code..."
    cargo fmt --all

# Format Elixir code (LiveView IDE)
# `.formatter.exs` declares `import_deps: [:phoenix]` and the LiveView HTML
# formatter plugin, both of which need fetched deps — so a fresh checkout must
# `mix deps.get` before `mix format` can run.
[working-directory: 'editors/liveview']
fmt-elixir:
    @echo "✨ Formatting Elixir code..."
    mix deps.get --quiet
    mix format
    @echo "✅ Elixir formatting complete"

# Format all code (Rust + Erlang + JS + Elixir + Beamtalk stdlib/test sources)
fmt: fmt-rust fmt-erlang fmt-js fmt-elixir fmt-beamtalk

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
# tests/repl-protocol/cases/ is excluded: those .btscript files contain REPL commands
# (e.g. :clear, :bindings) that are not valid Beamtalk syntax.
fmt-beamtalk:
    @echo "✨ Formatting Beamtalk source files..."
    @cargo run --bin beamtalk --quiet -- fmt stdlib/src/ stdlib/test/ stdlib/bootstrap-test/ tests/repl-protocol/fixtures/ examples/
    @echo "✅ Beamtalk source files formatted"

# Check Beamtalk source file formatting
fmt-check-beamtalk:
    @echo "📋 Checking Beamtalk source formatting..."
    @cargo run --bin beamtalk --quiet -- fmt-check stdlib/src/ stdlib/test/ stdlib/bootstrap-test/ tests/repl-protocol/fixtures/ examples/
    @echo "✅ Beamtalk formatting check passed"

# Run Dialyzer on Erlang runtime
[working-directory: 'runtime']
dialyzer:
    @echo "🔬 Running Dialyzer type checking..."
    rebar3 dialyzer

# Validate Dialyzer -spec attributes generated from Beamtalk type annotations.
# Compiles stdlib .bt sources to Core Erlang, extracts spec attributes, builds
# BEAM stubs with those specs embedded, and runs Dialyzer to verify well-formedness.
[unix]
dialyzer-specs: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "🔬 Validating generated -spec attributes..."
    # Compile stdlib to Core Erlang (preserves .core files)
    CORE_DIR=$(mktemp -d)
    trap 'rm -rf "$CORE_DIR"' EXIT
    # Copy .bt sources and build in temp dir to get .core files.
    # Flattened on purpose: stdlib module names come from the file stem, so any
    # stdlib/src/ subdirectories are irrelevant here (and unique stems are
    # enforced by build-stdlib's duplicate-stem check).
    find stdlib/src -type f -name '*.bt' -exec cp {} "$CORE_DIR/" \;
    cargo run --bin beamtalk --quiet -- build --stdlib-mode "$CORE_DIR/"
    # Run spec validation on the generated .core files
    escript scripts/validate_specs.escript "$CORE_DIR/build/"
    echo "✅ All generated specs are valid"

# Dialyzer spec validation (no-op on Windows — escript / bash not available)
[windows]
dialyzer-specs:
    @echo "⏭️  Skipping spec validation on Windows"

# ═══════════════════════════════════════════════════════════════════════════
# Testing
# ═══════════════════════════════════════════════════════════════════════════

# Run fast tests (Rust unit/integration + stdlib + BUnit + Erlang runtime + metamorphic, skip slow E2E)
# Typical time: ~4:35 (test-rust ~45s, test-stdlib ~20s, test-bunit ~97s, test-runtime ~1:40, test-metamorphic ~5s)
test: test-rust test-stdlib test-bunit test-runtime test-metamorphic

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

# Run REPL protocol tests (slow - full pipeline, ~50s)
test-repl-protocol: build-stdlib
    @echo "🧪 Running REPL protocol tests (slow - ~50s)..."
    cargo test --test repl_protocol -- --ignored

# Deprecated alias for test-repl-protocol — kept for one release cycle (BT-2085).
# Will be removed after the deprecation window.
test-e2e:
    @echo "⚠️  'just test-e2e' is deprecated; use 'just test-repl-protocol' instead."
    @just test-repl-protocol

# Run cross-surface parity tests (BT-2077, BT-2078, BT-2081)
# Drives the same input through REPL / MCP / CLI / LSP and asserts agreement.
# Single-threaded (--test-threads=1) because cases share one workspace.
# `parity` is the value/load/lint/test corpus; `diagnostic_parity` is the
# diagnostic-shape corpus added in BT-2078; `lsp_parity` is the LSP
# capability suite (hover/completion/definition/workspace symbol) added in
# BT-2081.
test-parity: build
    @echo "🧪 Running parity tests (REPL / MCP / CLI / LSP)..."
    cargo test -p beamtalk-parity-tests --test parity -- --ignored --test-threads=1
    cargo test -p beamtalk-parity-tests --test diagnostic_parity -- --ignored --test-threads=1
    cargo test -p beamtalk-parity-tests --test lsp_parity -- --ignored --test-threads=1
    @echo "✅ Parity tests complete"

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

# Run ALL tests (unit + integration + REPL protocol + Erlang runtime)
test-all: test-rust test-stdlib test-bunit test-integration test-mcp test-repl-protocol test-runtime

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

    # 3. Verify runtime include headers are present
    test -f "$TMPDIR/lib/beamtalk/lib/beamtalk_runtime/include/beamtalk.hrl"
    echo "✅ Runtime include headers present"

    # 4. Scaffold a project, add a class, and run it end-to-end
    (cd "$TMPDIR" && "$TMPDIR/bin/beamtalk" new smoke_project)
    echo "✅ beamtalk new smoke_project OK"
    printf 'Object subclass: SmokeTest\n  class run => 21 + 21\n' > "$TMPDIR/smoke_project/src/SmokeTest.bt"
    (cd "$TMPDIR/smoke_project" && "$TMPDIR/bin/beamtalk" run SmokeTest run)
    echo "✅ beamtalk run SmokeTest>>run OK"

    # 5. Verify native Erlang with -include_lib compiles in installed layout
    mkdir -p "$TMPDIR/smoke_project/native"
    cat > "$TMPDIR/smoke_project/native/smoke_native.erl" <<'ERLEOF'
    -module(smoke_native).
    -include_lib("beamtalk_runtime/include/beamtalk.hrl").
    -export([ok/0]).
    ok() -> ok.
    ERLEOF
    (cd "$TMPDIR/smoke_project" && "$TMPDIR/bin/beamtalk" build)
    echo "✅ Native Erlang with -include_lib compiles in installed layout"

    echo "✅ All smoke tests passed"

# Run .btscript expression tests (ADR 0014 Phase 1, ~20s) (also available as `beamtalk test-script`)
# Accepts optional path to run a single file: just test-stdlib bootstrap-test/arithmetic.btscript
# Output: summary only (--quiet suppresses per-file lines)
[working-directory: 'stdlib']
test-stdlib *ARGS: build-stdlib
    @echo "🧪 Running stdlib tests..."
    @cargo run --bin beamtalk --quiet -- test-stdlib --warnings-as-errors --quiet {{ ARGS }}
    @echo "✅ Stdlib tests complete"

# Run BUnit TestCase tests (ADR 0014 Phase 2)
# Accepts optional path: just test-bunit test/dictionary_test.bt
# Output: summary only (--quiet suppresses per-class lines)
[working-directory: 'stdlib']
test-bunit *ARGS: build-stdlib
    @echo "🧪 Running BUnit tests..."
    @cargo run --bin beamtalk --quiet -- test --warnings-as-errors --quiet {{ ARGS }}
    @echo "✅ BUnit tests complete"

# Verify ThreadedIr::verify() (ADR 0111) invariants hold across the full
# stdlib/test/*.bt + stdlib/bootstrap-test/*.btscript corpus (BT-3136
# close-out; BT-3424/ADR 0118 close-out corrected this comment's corpus
# claim below). Compiling that corpus via `test-stdlib`/`test-bunit`
# already runs every state-threading codegen path where
# `report_threaded_ir_verify_errors` (control_flow/mod.rs) checks each
# `threaded_ir::verify()` invariant via `debug_assert!` — live here because
# `cargo run` builds in the dev profile (debug_assertions on), so any
# violation hard-panics the build instead of only degrading to a
# diagnostic, as it would in a release build. This is a thin alias over
# those existing corpus-compiling recipes — a named, explicit CI gate for
# the verifier itself, not a new test harness.
#
# `stdlib/bootstrap-test/*.btscript` (exercised by `test-stdlib`) is
# bootstrap-primitive-only — arithmetic, booleans, equality, strings — and
# contains no Actor code, so it never reaches the actor-state-threading
# invariants (gen_server routing, class-var shadow-writes, self-send
# sequencing) this verifier spends most of its checks on. Those are
# exercised by `stdlib/test/*.bt` (via `test-bunit`): specifically
# `actor_self_send_position_matrix_test.bt`, `value_type_mutation_matrix_test.bt`,
# and `metamorphic_threading_test.bt`, plus every other Actor-bearing BUnit
# suite. `test-stdlib` stays a dependency because it's cheap and still
# covers the value-type/loop/conditional state-threading invariants that
# don't need an Actor.
#
# This recipe does not assert anything beyond what `test-stdlib`/`test-bunit`
# already assert when run directly — `debug_assert!` already fires during
# any dev-profile compile of this corpus, which is exactly what those two
# recipes do. Checked whether a cheap addition (e.g. a
# `BEAMTALK_THREADED_IR_STRICT` env var making `report_threaded_ir_verify_errors`
# hard-fail even in a release-profile compile, where `debug_assert!` is
# compiled out) could make this recipe assert something new: no such env
# var exists today, and adding the strict-release path would need a second,
# separate release build of the whole corpus just for this recipe — not
# cheap, and no CI job currently compiles this corpus in release mode for
# it to guard. Left as a thin alias; revisit if a release-mode compile of
# this corpus is ever added to CI.
#
# `just` dedupes shared dependencies within ONE invocation, so `just ci`
# (which lists `verify-threaded-ir` alongside `test`) doesn't recompile the
# corpus twice. That dedup does NOT apply across separate CI steps: ci.yml's
# `test-beam` job deliberately does not add a standalone "run
# verify-threaded-ir" step after its own `test-stdlib`/`test-bunit` steps,
# since those already exercise this exact corpus under debug_assertions —
# a third invocation there would recompile it for zero new detection.
verify-threaded-ir: test-stdlib test-bunit
    @echo "✅ ThreadedIr verifier: no invariant violations across stdlib + bootstrap-test corpus"

# Run learning guide doctests (docs/learning/ — separate from stdlib tests)
# Extracts ```beamtalk blocks from Markdown chapters and runs them via test-docs
test-learn: build-stdlib
    @echo "📚 Running learning guide doctests..."
    @cargo run --bin beamtalk --quiet -- test-docs --warnings-as-errors --quiet docs/learning/
    @echo "✅ Learning guide tests complete"

# Run the metamorphic testing harness (BT-3117): apply semantics-preserving
# AST transforms (block-wrap, rename-locals, redundant-temp) to every
# bootstrap-test `// =>` expression and assert the transformed variant still
# evaluates to the same expected result. ~5s over the full corpus.
# Accepts optional path: just test-metamorphic bootstrap-test/blocks.btscript
[working-directory: 'stdlib']
test-metamorphic *ARGS: build-stdlib
    @echo "🧬 Running metamorphic tests..."
    @cargo run --bin beamtalk --quiet -- test-metamorphic --warnings-as-errors --quiet {{ ARGS }}
    @echo "✅ Metamorphic tests complete"

# Note: Auto-discovers all *_tests modules. New test files are included automatically.
# Run Erlang runtime unit tests
# Output: summary only on success, full output on failure
[unix]
[working-directory: 'runtime']
test-runtime: build-stdlib
    #!/usr/bin/env bash
    set -eo pipefail
    echo "🧪 Running Erlang runtime unit tests..."
    if OUTPUT=$(BEAMTALK_NO_FILE_LOG=1 rebar3 eunit --cover=false --app=beamtalk_runtime,beamtalk_workspace,beamtalk_compiler 2>&1); then
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
    @$ErrorActionPreference = 'Continue'; $env:BEAMTALK_NO_FILE_LOG = "1"; $output = rebar3 eunit '--cover=false' '--app=beamtalk_runtime,beamtalk_workspace,beamtalk_compiler' 2>&1 | Out-String; $exitCode = $LASTEXITCODE; if ($exitCode -ne 0) { Write-Output $output; exit $exitCode } else { ($output -split "`n") | Select-Object -Last 3 }
    @$ErrorActionPreference = 'Continue'; $env:BEAMTALK_NO_FILE_LOG = "1"; $output = rebar3 eunit '--cover=false' '--dir=apps/beamtalk_stdlib/test' 2>&1 | Out-String; $exitCode = $LASTEXITCODE; if ($exitCode -ne 0) { Write-Output $output; exit $exitCode } else { ($output -split "`n") | Select-Object -Last 3 }
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

# Run fuzz testing for a configurable duration per target (default: 60 seconds each).
# Runs both targets: parse_arbitrary (lexer + parser crash safety) and
# compile_pipeline (full lex/parse/analyse/codegen pipeline + Core Erlang
# structural validity, BT-3124).
fuzz DURATION="60":
    @echo "🔀 Fuzzing parser for {{DURATION}} seconds..."
    @echo "   Corpus: fuzz/corpus/parse_arbitrary/ (35 seed files)"
    @echo "   Target: parse_arbitrary (lexer + parser crash safety)"
    cargo +nightly fuzz run parse_arbitrary -- -rss_limit_mb=4096 -max_total_time={{DURATION}}
    @echo "✅ parse_arbitrary completed without crashes!"
    @echo "🔀 Fuzzing compile pipeline for {{DURATION}} seconds..."
    @echo "   Seeds: stdlib/test/*.bt + tests/repl-protocol/cases/*.btscript (referenced live, not copied)"
    @echo "   Corpus: fuzz/corpus/compile_pipeline/ (fuzzer-grown findings only)"
    @echo "   Target: compile_pipeline (lex → parse → analyse → codegen, structural validity)"
    @mkdir -p fuzz/corpus/compile_pipeline
    cargo +nightly fuzz run compile_pipeline fuzz/corpus/compile_pipeline stdlib/test tests/repl-protocol/cases -- -rss_limit_mb=4096 -max_total_time={{DURATION}}
    @echo "✅ compile_pipeline completed without crashes!"

# Corpus-through-BEAM lint (BT-3124): generate .core text for every corpus
# file (stdlib/test + tests/repl-protocol/cases by default -- the same
# live-referenced dirs compile_pipeline fuzzes from, not a copied snapshot
# -- or override CORPUS_DIRS with a space-separated list to also cover a
# fuzzer-grown corpus dir) and batch-compile with erlc + core_lint -- the
# check that catches "beamtalk's own codegen thinks this is valid, but
# erlc/core_lint rejects it" without needing a full libFuzzer run.
fuzz-corpus-lint CORPUS_DIRS="stdlib/test tests/repl-protocol/cases":
    @echo "🔬 Generating .core corpus from: {{CORPUS_DIRS}}"
    cargo run --release --example compile_pipeline_corpus -p beamtalk-core -- \
        target/compile-pipeline-corpus {{CORPUS_DIRS}}
    @echo "🔬 Batch-compiling with erlc + core_lint..."
    escript scripts/compile-pipeline-corpus-lint.escript target/compile-pipeline-corpus

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

# Files excluded from Rust coverage: test harnesses and build-time tooling
# that cannot be meaningfully unit-tested, so counting them only deflates the
# product-code coverage number.
#   - beamtalk-parity-tests : cross-surface test harness (publish = false)
#   - beamtalk-build        : build-script helpers, run during build.rs
#   - build-corpus/main.rs  : one-shot corpus-generation tool (its sibling
#                             beamtalk-examples lib stays measured, ~99%)
llvm_cov_ignore := '(beamtalk-parity-tests/|beamtalk-build/|build-corpus/src/main\.rs)'

# Tests that are `--ignored` for reasons unrelated to coverage measurement:
# environment prerequisites the coverage job doesn't (and shouldn't) set up.
#   - valid_specs_pass_dialyzer_validation / negative_test_invalid_spec_detected_by_dialyzer:
#     both need a Dialyzer PLT, already covered by the separate `just dialyzer`
#     CI lint step. Unlike that job, the coverage job doesn't cache the PLT
#     (`ensure_plt()` self-heals by building one from scratch), so skipping
#     only the first and not the second would still eat a full from-scratch
#     PLT build into the coverage job's time budget for no reason.
#   - live_front's suite: needs a real `dist-liveview` release + a running
#     workspace, built only by manual setup (see the module doc comment in
#     crates/beamtalk-desktop-broker/tests/live_front.rs) — never CI.
coverage_rust_skip := "--skip valid_specs_pass_dialyzer_validation \
    --skip negative_test_invalid_spec_detected_by_dialyzer \
    --skip predict_node_name_matches_a_live_epmd_registration \
    --skip resolve_registered_node_name_matches_a_live_epmd_registration \
    --skip bad_cookie_readiness_resolves_within_the_default_budget \
    --skip a_real_port_conflict_exits_within_the_calibrated_grace_period \
    --skip detach_kills_the_front_and_it_exits_cleanly \
    --skip dead_workspace_readiness_resolves_to_dead_workspace_not_a_hang"

# `--lib --bins --tests` rather than `--all-targets`: the `--include-ignored`/
# `--skip`/`--test-threads` args below are libtest flags, but this
# workspace's `[[bench]]` targets (build_bench, compiler_pipeline) use
# `harness = false` (Criterion), which rejects unrecognized args outright
# rather than ignoring them. Excluding benches from what's
# coverage-instrumented here is fine — they exercise the same product code
# paths regular tests already cover.
#
# Explicit `clean` + `--no-report` on every run, rather than the default
# clean-before-run behavior: we need three separate test-running
# invocations (force-build the e2e binaries below, then two more passes)
# to all accumulate into the SAME profile pool, with only the first one
# resetting it. `--no-report`/`--no-clean` can't be combined (cargo-llvm-cov
# rejects it outright), so a single explicit `clean --workspace` up front
# is what makes accumulation across the rest possible.
#
# Two test passes sharing that one profile pool, not one `--include-ignored`
# run: the ignored suites (beamtalk-mcp's client.rs, beamtalk-cli's own
# integration tests, and the beamtalk-parity-tests e2e drivers) each spawn
# a real REPL/BEAM node per test. `just test-mcp`/`test-parity`/
# `test-integration` already run them with `--test-threads=1` for exactly
# this reason — running them at cargo's default parallelism alongside
# everything else starves them of CPU/RAM and produces spurious
# "Evaluation crashed: killed" failures. Forcing the *entire* workspace
# (beamtalk-core's ~5,300 tests included) single-threaded to accommodate
# them would blow past the coverage job's time budget for no reason, so
# the fast suites keep default parallelism in pass 1 and only the
# REPL-spawning ignored tests are serialized in pass 2.
#
# The e2e-binary force-build happens between the clean and the two test
# passes, not before: beamtalk-parity-tests' CLI/LSP/MCP e2e drivers spawn
# beamtalk/-lsp/-mcp/-compiler-port as subprocesses, locating them next to
# their own test binary (see beamtalk_binary() in
# crates/beamtalk-parity-tests/src/pool.rs). Neither `cargo test` nor
# `cargo llvm-cov` build a package's plain `[[bin]]` artifact unless
# something forces it (an in-crate integration test referencing
# CARGO_BIN_EXE_*, or an explicit `cargo build`) — normal `just test-parity`
# never notices because `test-parity: build` always runs a full build
# first, but `cargo llvm-cov` builds into its own separate target dir and
# has no such prerequisite. Without this, the parity suite's ignored e2e
# tests spawn nothing usable and contribute zero coverage for
# beamtalk-cli/-lsp/-mcp — silently, since the drivers report a normal
# "compiler/LSP/MCP not available" test failure, not a missing-binary error.
[private]
_coverage-rust-run-tests:
    cargo llvm-cov clean --workspace
    cargo llvm-cov run --no-report -p beamtalk-lsp --bin beamtalk-lsp -- --help > /dev/null 2>&1 || true
    cargo llvm-cov run --no-report -p beamtalk-mcp --bin beamtalk-mcp -- --help > /dev/null 2>&1 || true
    cargo llvm-cov run --no-report -p beamtalk-compiler-port --bin beamtalk-compiler-port -- --help > /dev/null 2>&1 || true
    cargo llvm-cov --no-report --lib --bins --tests --workspace
    cargo llvm-cov --no-report --lib --bins --tests --workspace -- --ignored --test-threads=1 {{coverage_rust_skip}}

# Generate Rust coverage (requires cargo-llvm-cov)
coverage-rust: _coverage-rust-run-tests
    @echo "📊 Generating Rust coverage..."
    cargo llvm-cov report --ignore-filename-regex '{{llvm_cov_ignore}}' --html
    @echo "  📁 HTML report: target/llvm-cov/html/index.html"

# Generate Rust coverage as Cobertura XML for the CI coverage badge.
coverage-rust-cobertura: _coverage-rust-run-tests
    @echo "📊 Generating Rust coverage (Cobertura)..."
    cargo llvm-cov report --ignore-filename-regex '{{llvm_cov_ignore}}' --cobertura --output-path coverage.cobertura.xml

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
    # Run 1: runtime + workspace + compiler tests.
    # bt@*.beam files are findable via apps/beamtalk_stdlib/ebin/ (project app ebin
    # stays on the code path when beamtalk_stdlib is not listed as an explicit --app).
    if ! OUTPUT=$(rebar3 eunit --app=beamtalk_runtime,beamtalk_workspace,beamtalk_compiler --cover 2>&1); then
        echo "$OUTPUT"
        echo "❌ EUnit tests (runtime+workspace+compiler) failed"
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
    E2E_COVER=1 cargo test --test repl_protocol -- --ignored || true
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
    STDLIB_COVER=1 cargo run --bin beamtalk --quiet -- test-stdlib --warnings-as-errors bootstrap-test || true
    if [ -f ../runtime/_build/test/cover/stdlib.coverdata ]; then
        SIZE=$(wc -c < ../runtime/_build/test/cover/stdlib.coverdata)
        echo "  📁 Coverdata: runtime/_build/test/cover/stdlib.coverdata (${SIZE} bytes)"
    else
        echo "⚠️  No stdlib coverdata produced"
        exit 1
    fi

# Collect BUnit test coverage (runs the .bt TestCase suite with Erlang cover
# instrumentation). The BUnit suite drives beamtalk_test_case, beamtalk_test_runner,
# and the full dispatch/object/class machinery via real TestCase classes — code that
# eunit/E2E/bootstrap barely touch. [working-directory: 'stdlib'] so `beamtalk test`
# discovers the default `test/` dir (matching the test-bunit recipe).
[unix]
[working-directory: 'stdlib']
coverage-bunit: build-stdlib
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📊 Running BUnit tests with Erlang cover instrumentation..."
    echo "   (This is slower than normal BUnit tests due to cover overhead)"
    BUNIT_COVER=1 cargo run --bin beamtalk --quiet -- test --warnings-as-errors || true
    if [ -f ../runtime/_build/test/cover/bunit.coverdata ]; then
        SIZE=$(wc -c < ../runtime/_build/test/cover/bunit.coverdata)
        echo "  📁 Coverdata: runtime/_build/test/cover/bunit.coverdata (${SIZE} bytes)"
    else
        echo "⚠️  No BUnit coverdata produced"
        exit 1
    fi

# Unix-only: depends on Unix-only coverage recipes
# Runs eunit with --cover, then E2E with cover, then stdlib + BUnit with cover, then merges all into one report.
# Generate combined Erlang coverage (eunit + E2E + stdlib + BUnit)
[unix]
coverage-all: coverage-runtime coverage-e2e coverage-stdlib coverage-bunit
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

# Generate LiveView IDE (editors/liveview) coverage via Elixir's built-in
# `mix test --cover` (OTP :cover — same tool family as coverage-runtime's
# rebar3 cover). Threshold/ignore_modules are configured in mix.exs (BT-3288).
# `mix deps.get` first mirrors fmt-check-elixir/fmt-elixir's fresh-checkout-safe pattern.
[working-directory: 'editors/liveview']
coverage-liveview:
    @echo "📊 Generating LiveView IDE (Elixir) coverage..."
    mix deps.get --quiet
    mix test --cover
    @echo "  📁 HTML report: editors/liveview/cover/index.html"

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

    # OTP application directories (discovered from rebar3 build output)
    OTP_APP_COUNT=0
    for ebin_dir in runtime/_build/default/lib/*/ebin; do
        app_dir="$(dirname "${ebin_dir}")"
        app="$(basename "${app_dir}")"
        if ! ls "${ebin_dir}"/*.beam 1>/dev/null 2>&1; then
            continue
        fi
        install -d "${PREFIX}/lib/beamtalk/lib/${app}/ebin"
        install -m 644 "${ebin_dir}"/*.beam "${PREFIX}/lib/beamtalk/lib/${app}/ebin/"
        if ls "${ebin_dir}"/*.app 1>/dev/null 2>&1; then
            install -m 644 "${ebin_dir}"/*.app "${PREFIX}/lib/beamtalk/lib/${app}/ebin/"
        fi
        # Copy priv/ directory if present (e.g. beamtalk_workspace browser UI)
        if [ -d "${app_dir}/priv" ]; then
            cp -rL "${app_dir}/priv" "${PREFIX}/lib/beamtalk/lib/${app}/priv"
        fi
        OTP_APP_COUNT=$((OTP_APP_COUNT + 1))
    done
    if [ "${OTP_APP_COUNT}" -eq 0 ]; then
        echo "❌ No OTP apps found in runtime/_build/default/lib/. Run 'just build-erlang' first."
        exit 1
    fi

    # OTP application include directories (for native Erlang compilation)
    for app in beamtalk_runtime; do
        INC_SRC="runtime/apps/${app}/include"
        if [ ! -d "${INC_SRC}" ] || ! ls "${INC_SRC}"/*.hrl 1>/dev/null 2>&1; then
            echo "❌ No .hrl headers found in ${INC_SRC}. Run 'just build-erlang' first."
            exit 1
        fi
        install -d "${PREFIX}/lib/beamtalk/lib/${app}/include"
        install -m 644 "${INC_SRC}"/*.hrl "${PREFIX}/lib/beamtalk/lib/${app}/include/"
    done

    # Bundled rebar3 escript (ADR 0072 — needed for `beamtalk build` hex deps)
    if [ ! -f "runtime/tools/rebar3" ]; then
        echo "❌ Bundled rebar3 not found at runtime/tools/rebar3."
        exit 1
    fi
    install -d "${PREFIX}/lib/beamtalk/tools"
    install -m 755 "runtime/tools/rebar3" "${PREFIX}/lib/beamtalk/tools/rebar3"

    # Stdlib sources for LSP/tooling navigation
    # Mirrors the source tree (including any subdirectories) so LSP
    # goto-definition resolves to the same relative layout as the repo.
    STDLIB_SOURCE_SRC="stdlib/src"
    if [ -d "${STDLIB_SOURCE_SRC}" ]; then
        find "${STDLIB_SOURCE_SRC}" -type f -name '*.bt' | while read -r bt; do
            rel="${bt#"${STDLIB_SOURCE_SRC}"/}"
            install -d "${PREFIX}/share/beamtalk/stdlib/src/$(dirname "${rel}")"
            install -m 644 "${bt}" "${PREFIX}/share/beamtalk/stdlib/src/${rel}"
        done
    fi

    # Curated distribution FFI type stubs (ADR 0075 layer 3), discovered at
    # runtime via the same sysroot convention as the stdlib sources above.
    # A no-op until curated stub content exists (BT-1848) — the repo ships
    # no root-level stubs/ directory yet.
    DIST_STUBS_SRC="stubs"
    if [ -d "${DIST_STUBS_SRC}" ]; then
        find "${DIST_STUBS_SRC}" -type f -name '*.bt' | while read -r bt; do
            rel="${bt#"${DIST_STUBS_SRC}"/}"
            install -d "${PREFIX}/share/beamtalk/stubs/$(dirname "${rel}")"
            install -m 644 "${bt}" "${PREFIX}/share/beamtalk/stubs/${rel}"
        done
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
    $appCount = 0; foreach ($ebinDir in (Get-ChildItem -Directory "runtime/_build/default/lib/*/ebin" -ErrorAction SilentlyContinue)) { $app = $ebinDir.Parent.Name; $appRoot = $ebinDir.Parent.FullName; if (!(Get-ChildItem "$($ebinDir.FullName)/*.beam" -ErrorAction SilentlyContinue)) { continue }; New-Item -ItemType Directory -Force -Path "dist/lib/beamtalk/lib/$app/ebin" | Out-Null; Copy-Item "$($ebinDir.FullName)/*.beam" "dist/lib/beamtalk/lib/$app/ebin/" -ErrorAction Stop; if (Get-ChildItem "$($ebinDir.FullName)/*.app" -ErrorAction SilentlyContinue) { Copy-Item "$($ebinDir.FullName)/*.app" "dist/lib/beamtalk/lib/$app/ebin/" -ErrorAction Stop }; $privSrc = Join-Path $appRoot "priv"; if (Test-Path $privSrc) { New-Item -ItemType Directory -Force -Path "dist/lib/beamtalk/lib/$app/priv" | Out-Null; Copy-Item "$privSrc/*" "dist/lib/beamtalk/lib/$app/priv/" -Recurse }; $appCount++ }; if ($appCount -eq 0) { Write-Error "No OTP apps found in runtime/_build/default/lib/. Run 'just build-erlang' first."; exit 1 }
    if (!(Test-Path "runtime/tools/rebar3")) { Write-Error "Bundled rebar3 not found at runtime/tools/rebar3."; exit 1 }; New-Item -ItemType Directory -Force -Path "dist/lib/beamtalk/tools" | Out-Null; Copy-Item "runtime/tools/rebar3" "dist/lib/beamtalk/tools/rebar3"
    if (Test-Path "stdlib/src") { $stdlibRoot = (Resolve-Path "stdlib/src").Path; foreach ($bt in (Get-ChildItem -Path "stdlib/src" -Recurse -File -Filter *.bt)) { $rel = $bt.FullName.Substring($stdlibRoot.Length).TrimStart('\', '/'); $dest = Join-Path "dist/share/beamtalk/stdlib/src" $rel; New-Item -ItemType Directory -Force -Path (Split-Path $dest -Parent) | Out-Null; Copy-Item $bt.FullName $dest } }
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

