# Remote (Claude Code on the Web) Session Setup

This covers what happens automatically when a Beamtalk session starts in a
remote/cloud sandbox (`CLAUDE_CODE_REMOTE=true`) — for local dev, none of
this runs.

## The `SessionStart` hook

`.claude/hooks/worktree-init.sh` is the repo-tracked `SessionStart` hook
(registered in `.claude/settings.json`). It runs once at the top of every
session and, in remote sandboxes, additionally:

- Bootstraps the pinned toolchain (`mise`, Elixir/`mix`) via
  `scripts/setup-cloud.sh` if it isn't already present.
- Starts a local hex-bridge proxy (`scripts/hex-bridge-proxy.py`) so
  `rebar3`/`mix` can reach hex.pm from sandboxes where the BEAM's own TLS
  client is rejected by the egress proxy — see the comments at the top of
  `worktree-init.sh` for the full root-cause writeup.
- Syncs the current git worktree branch with its `origin` counterpart.

Note: `.claude/hooks/session-start.sh` (if present on disk) is a separate,
generated file copied in from a skills repo at runtime — it is gitignored
and not part of this repository. Environment setup that should ship with
the Beamtalk repo belongs in `worktree-init.sh` instead.

## OTP type-spec cache warming (BT-2471)

`worktree-init.sh` also kicks off warming the shared, OTP-version-keyed FFI
type-spec cache (`<cache>/beamtalk/otp-specs/<otp>-<erts>/`, ADR 0075 /
BT-2470) in the background:

```bash
nohup bash -c '
  ...
  cargo build --quiet -p beamtalk-cli --bin beamtalk
  flock -n runtime/.rebar3-compile.lock -c "cd runtime && rebar3 compile"
  ./target/debug/beamtalk warm-otp-cache
' _ "${CLAUDE_PROJECT_DIR}" >/dev/null 2>&1 &
disown
```

Why: remote containers are cloned fresh and reclaimed after inactivity, so
the shared cache never survives between sessions on its own — without
warming it, the session's first `beamtalk build` pays the cost of extracting
type specs from every OTP `.beam` file (stdlib, kernel, erts, …) on the
interactive critical path.

- **Non-blocking:** `nohup ... & disown` detaches the warmer from the hook's
  own process, so it never delays session startup, and it survives the hook
  process exiting.
- **Cargo half is duplication-free by construction:** it builds
  `beamtalk-cli` itself rather than waiting for the session's own `just
  build` — Cargo keys its build cache by content, not by which process
  triggered the build, so whichever finishes an artifact first "wins" and
  the other reuses it.
- **Erlang half is `flock`-guarded, not just reordered:** unlike Cargo's
  locked `target/`, rebar3 has no built-in guard against two processes
  running `rebar3 compile` against the same `runtime/_build/` at once — and
  an agent's first move in a fresh session is often `just build`, which
  compiles that same tree. `flock -n runtime/.rebar3-compile.lock` (also
  taken by `build-erlang` in the Justfile) makes the two mutually exclusive;
  non-blocking here so the background warmer skips the runtime step
  entirely — and lets `warm-otp-cache` report "not warmed yet" below, which
  it already treats as non-fatal — rather than stall waiting on an
  interactive build that's already in flight.
- **Keyed by OTP/ERTS version, idempotent:** `beamtalk warm-otp-cache`
  (`crates/beamtalk-cli/src/commands/warm_otp_cache.rs`) is a thin,
  project-agnostic wrapper around the same
  `beamtalk_core::ffi_type_specs::extract_type_specs` that
  `beamtalk build`/`beamtalk lint` use for a real project. Once the shared
  cache is warm for the running OTP/ERTS version, re-running it is a fast
  no-op — no `.beam` files are read and no `beamtalk_build_worker` BEAM node
  is spawned (BT-2470's cache-hit path). Local dev is unaffected: the
  warming step only runs when `CLAUDE_CODE_REMOTE=true`, and the platform
  cache dir there already persists across sessions.
- **Override the cache location** with `BEAMTALK_CACHE_DIR` (used by CI
  cache mounts and by the manual verification below); otherwise it resolves
  via `dirs::cache_dir()` (`$XDG_CACHE_HOME` on Linux).

To verify warming worked, or to warm on demand outside the hook:

```bash
beamtalk warm-otp-cache
# OTP type-spec cache warm: 403 modules, 4240 functions
```

## Troubleshooting local and sandbox-only failures (BT-4787)

These pass in CI (which builds everything first, on a clean runner) but trip up
sandbox or local runs. None of them is a code bug.

### `mix deps.get`: "Unknown package dns_cluster in lockfile"

The pre-push hook's Elixir step (`just lint-elixir` → `fmt-check-elixir`) runs
`mix deps.get` in `editors/liveview`. It fails with this message when Hex or
rebar is not installed for the current Elixir, or when the Hex registry fetch
is truncated (a sandbox egress proxy, or a parallel registry download). It was
not reproducible from a clean `deps/` + `_build/` in a sandbox whose
`SessionStart` hook had started the hex-bridge (`HEX_MIRROR`/`HEX_CDN` point at
`127.0.0.1:18081`), so suspect a missing bridge or a flaky fetch. Fix:

```bash
cd editors/liveview
export HEX_HTTP_CONCURRENCY=1 HEX_HTTP_TIMEOUT=120
mix local.hex --force && mix local.rebar --force && mix deps.get
```

Then push again. `scripts/cloud-doctor.sh` reports whether the bridge is up.
Do not push with `--no-verify` to get past it.

### Tests that need the compiled stdlib

`rebar3 compile`, `rebar3 xref` and `rebar3 eunit` build only the hand-written
Erlang modules. Tests that run compiled Beamtalk (the stdlib classes
`bt@stdlib@*`) need `just build-stdlib` first. Without it
`fold_acc_shape_agreement` (`beamtalk-codegen`) used to fail with hundreds of
`undef`/`class_not_found` lines; it now falls back to compile-only, and fails
loudly under `BEAMTALK_REQUIRE_RUNTIME=1` (which CI sets). Run `just
build-stdlib` before `cargo test -p beamtalk-codegen` or `just test-runtime`.

### `rebar3 xref`

`rebar3 xref` exits 1 on this repo by design: it reports unused exports and
calls into optional dependencies (for example `otel_ctx`). It is not part of CI.
Read the warnings; they are not a crash. (A crash inside OTP's `erl_internal`
was reported on an earlier main and does not reproduce on OTP 28.5.)

## Upstream bug reports

Reproducers and report drafts for bugs in OTP and other upstream projects live
in `docs/development/upstream-reports/`, one directory each. The repository
owner files them.
