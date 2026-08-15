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
  (cd runtime && rebar3 compile)
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
- **No duplicated build work:** it builds `beamtalk-cli` and the Erlang
  runtime itself rather than waiting for the session's own `just build` —
  Cargo and rebar3 key their build caches by content, not by which process
  triggered the build, so whichever finishes an artifact first "wins" and
  the other reuses it.
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
