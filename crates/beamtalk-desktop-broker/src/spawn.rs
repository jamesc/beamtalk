// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Spawn a per-workspace front (ADR 0097 Decision / Broker §2).
//!
//! Concretely, on Unix: `PORT=<free-port> BT_ATTACH_BIND_IP=127.0.0.1
//! BT_ATTACH_NODE_SUFFIX=<id> RELEASE_DISTRIBUTION=none bin/server <id>`.
//!
//! `bin/server <id>` (`editors/liveview/rel/overlays/bin/server`) already
//! re-resolves `BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE` from
//! `~/.beamtalk/workspaces/<id>/metadata.json` + the sibling `cookie` file
//! and generates its own ephemeral `SECRET_KEY_BASE` when given a workspace
//! id — the ADR's "Local-only posture" section calls that ephemeral-per-boot
//! key *deliberate* (no durable session worth preserving on the
//! unauthenticated localhost lane), so this broker does not set it itself on
//! Unix. The broker's job there is exactly the four env vars above, plus
//! (this module's other duty) refusing to spawn at all when OIDC is
//! configured.
//!
//! **Deliberately not setting a cookie env var on Unix**, even though the
//! workspace's cookie is what makes the attach secure: the spike found that
//! `bin/server <id>` **unconditionally** re-resolves `BT_WORKSPACE_COOKIE`
//! from the on-disk `cookie` file whenever given the workspace-id positional
//! argument, silently overwriting any env override — so a broker-set cookie
//! env in this call shape would be a dead write, not a security gap. (The
//! spike's negative-path tests for a corrupted cookie instead invoke
//! `bin/server` with **no** positional argument — a diagnostic invocation
//! shape this module does not need, since the happy path always has a real
//! on-disk cookie to resolve.)
//!
//! **Windows has no `bin/server`** (BT-2988) — `bin/server` is a POSIX `sh`
//! script with no counterpart in a `mix release` bundle. There, this module
//! itself does what `bin/server` does on Unix before invoking the release's
//! own Windows entry point (`bin\bt_attach.bat start`) directly:
//! - resolves `BT_WORKSPACE_NODE` from `metadata.json`
//!   ([`crate::discovery::read_node_name`], the same real-JSON-parser path
//!   [`crate::discovery::discover_workspaces`] uses — not a shell `sed`)
//! - resolves `BT_WORKSPACE_COOKIE` from the sibling `cookie` file
//!   ([`beamtalk_workspace::read_cookie_file`])
//! - generates an ephemeral per-boot `SECRET_KEY_BASE` itself (same
//!   "deliberately ephemeral" rationale as `bin/server`'s — ADR 0097
//!   "Local-only posture")
//! - sets `PHX_SERVER=true` (what `bin/server` sets right before its own
//!   `exec bin/bt_attach start`)
//!
//! `RELEASE_DISTRIBUTION=none` is a hard requirement on every platform, not
//! an optimization — the spike (`docs/research/desktop-shell-spike.md`,
//! criterion (a)) found that `mix release`'s generated launcher boots the VM
//! **already** distributed under `-sname bt_attach` (`RELEASE_NODE` defaults
//! to `RELEASE_NAME`) before any Elixir code runs, which pre-empts
//! `ensure_distributed/0`'s `BT_ATTACH_NODE_SUFFIX` seeding entirely and
//! makes every spawned instance collide on the identical epmd registration.
//! Booting non-distributed hands control back to the front's own lazy,
//! correctly-seeded `ensure_distributed/0` on the first `/readiness` call.
//!
//! **Windows gap, documented rather than silently worked around (BT-2988
//! acceptance criteria):** the epmd/OS-process story is not fully verified on
//! Windows. `detach` (below) uses `CREATE_NEW_PROCESS_GROUP` as the Windows
//! analogue of Unix's `process_group(0)` — the standard way to stop a Ctrl-C
//! sent to the broker's own console from also reaching the front.
//!
//! **The process-tree question this paragraph used to leave open is now
//! closed by construction, not by observation** (adversarial-review
//! follow-up): `bin\bt_attach.bat` cannot be a direct `CreateProcessW` child
//! at all — Windows can only run a `.bat` via `cmd.exe`, so whatever
//! `Child::id()`/`Child::kill()` refer to here is `cmd.exe` (or an
//! equivalent wrapper), never `erl.exe` itself, no matter what a real build
//! turns out to do. Relying on `Child::kill()` alone would therefore
//! *always* orphan `erl.exe`, not just in some unverified branch — so
//! [`spawn_front`] additionally assigns the spawned process to a
//! [`crate::winjob::JobHandle`] (`JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE`),
//! which kills the *entire* process tree — `cmd.exe`, `erl.exe`, everything
//! spawned after assignment — together, the moment the job handle closes
//! (explicitly, or automatically when this broker process itself dies, no
//! cooperation required). See [`crate::winjob`]'s module doc for the
//! mechanism, its one residual (assign-after-spawn) race, and why that
//! race can't be fully closed without bypassing `std::process::Command`.
//! This has not been exercised against a real Windows boot — no Windows
//! sandbox was available to develop or test it against — but unlike the
//! rest of this paragraph's original framing, it is not a *guess* at what
//! Windows does; the job-object mechanism works the same way regardless of
//! how many hops sit between `cmd.exe` and `erl.exe`. [`crate::reap`]'s
//! PID-file sweep remains the fallback net for any process this mechanism
//! still misses (e.g. a crash in the narrow assign-after-spawn window).

use std::cell::RefCell;
use std::path::PathBuf;
use std::process::{Child, Command};
use std::time::Duration;

use crate::error::{BrokerError, Result};
use crate::oidc_guard::{default_ide_config_path, oidc_configured};
use crate::port::{self, SpawnAttempt};
use crate::sname::attach_node_suffix;

/// Everything needed to spawn one front.
#[derive(Debug, Clone)]
pub struct SpawnConfig {
    /// Path to the launcher executable — `bin/server` on Unix, invoked as
    /// `bin/server <id>` (it re-resolves everything else itself). On Windows
    /// (BT-2988, ADR 0097 Implementation §5b), this is instead the release's
    /// own `bin\bt_attach.bat`, invoked as `bin\bt_attach.bat start` with no
    /// positional workspace-id arg — [`spawn_front`] resolves and sets every
    /// env var `bin/server` would have on Unix before invoking it. Either
    /// way, callers resolve the right launcher path for their platform and
    /// bundle layout and pass it in here; this module only cares that it's
    /// the correct entry point for the current `cfg(unix)`/`cfg(windows)`.
    pub launcher: PathBuf,
    /// Workspace id to attach to: a positional arg to `bin/server` on Unix
    /// (which does its own env resolution from it), or on Windows the key
    /// this module resolves `BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE` by
    /// (not passed as an argv there — `bin\bt_attach.bat` takes the release's
    /// own `start` command instead). Either way it also seeds
    /// `BT_ATTACH_NODE_SUFFIX`.
    pub workspace_id: String,
    /// Port the front should bind Phoenix to.
    pub port: u16,
    /// Loopback bind address (ADR 0097 "Local-only posture" — always
    /// `127.0.0.1`/`::1`, never `0.0.0.0`).
    pub bind_ip: String,
    /// `ide.toml` path to check for OIDC config before spawning. Defaults to
    /// [`default_ide_config_path`] via [`SpawnConfig::new`].
    pub ide_toml_path: PathBuf,
}

impl SpawnConfig {
    /// Build a config with the standard loopback bind and default
    /// `ide.toml` path.
    #[must_use]
    pub fn new(launcher: PathBuf, workspace_id: impl Into<String>, port: u16) -> Self {
        Self {
            launcher,
            workspace_id: workspace_id.into(),
            port,
            bind_ip: "127.0.0.1".to_string(),
            ide_toml_path: default_ide_config_path(),
        }
    }
}

/// The env vars this broker sets on **every** platform, in a stable order
/// (for deterministic logging/tests — `Command::envs` doesn't care about
/// order). On Windows, [`build_launch_command`] sets five more
/// (`BT_WORKSPACE_NODE`, `BT_WORKSPACE_COOKIE`, `SECRET_KEY_BASE`,
/// `PHX_SERVER`, `RELEASE_TMP`) that don't go through this function — it
/// does not report the full env set there, only the cross-platform baseline.
#[must_use]
pub fn build_env(config: &SpawnConfig) -> Vec<(String, String)> {
    vec![
        ("PORT".to_string(), config.port.to_string()),
        ("BT_ATTACH_BIND_IP".to_string(), config.bind_ip.clone()),
        (
            "BT_ATTACH_NODE_SUFFIX".to_string(),
            attach_node_suffix(&config.workspace_id),
        ),
        ("RELEASE_DISTRIBUTION".to_string(), "none".to_string()),
    ]
}

/// A spawned front process. On Unix this is exactly [`Child`]; on Windows it
/// additionally carries the [`crate::winjob::JobHandle`] tying the front's
/// entire process tree (not just the immediate `cmd.exe`/wrapper child) to
/// this value's lifetime — see [`crate::winjob`]'s module doc and this
/// module's own doc comment for why that's necessary there. `Deref`/
/// `DerefMut` to [`Child`] so callers use it exactly like a `Child`
/// (`.id()`, `.kill()`, `.wait()`, `.try_wait()`) on every platform.
#[derive(Debug)]
pub struct SpawnedFront {
    child: Child,
    #[cfg(windows)]
    #[allow(dead_code)] // held for its Drop side effect only, never read
    job: crate::winjob::JobHandle,
}

impl std::ops::Deref for SpawnedFront {
    type Target = Child;
    fn deref(&self) -> &Child {
        &self.child
    }
}

impl std::ops::DerefMut for SpawnedFront {
    fn deref_mut(&mut self) -> &mut Child {
        &mut self.child
    }
}

/// Spawn the front described by `config`.
///
/// Refuses with [`BrokerError::OidcConfigured`] if OIDC config is present
/// (checked *before* touching the filesystem for the workspace, so the OIDC
/// refusal always wins over an "unknown workspace" error when both are true —
/// the security refusal should never be masked by an unrelated not-found).
/// Refuses with [`BrokerError::UnknownWorkspace`] if
/// `~/.beamtalk/workspaces/<id>/metadata.json` doesn't exist.
///
/// The child is spawned detached from this process's process group (Unix:
/// `process_group(0)`; Windows: `CREATE_NEW_PROCESS_GROUP` + `CREATE_NO_WINDOW`)
/// so a signal sent to the broker's own foreground group (e.g. Ctrl-C) does
/// not also kill the front, and (Windows only) so the front's console-
/// subsystem wrapper process doesn't pop a visible window of its own —
/// orphan-reaping ([`crate::reap`]) is the intended mechanism for cleaning up
/// fronts left behind by a dead broker, not accidental group signal
/// propagation or a user closing a stray console window.
///
/// On Windows, additionally assigns the spawned process to a fresh
/// [`crate::winjob::JobHandle`] before returning — see this module's doc
/// comment for why plain `Child::kill()` cannot reach `erl.exe` there.
///
/// # Errors
///
/// Returns [`BrokerError::OidcConfigured`] or [`BrokerError::UnknownWorkspace`]
/// per the refusal conditions above, or [`BrokerError::Io`] if the launcher
/// process fails to spawn or (Windows only) if assigning it to the job
/// object fails. [`build_launch_command`] can additionally return
/// [`BrokerError::LauncherPlatformMismatch`] (BT-3046 — `config.launcher`
/// doesn't look like a real entry point for the current platform) before any
/// process is spawned. On Windows, its workspace lookups and per-front
/// `RELEASE_TMP` setup can additionally propagate [`BrokerError::MissingCookie`]
/// (no `cookie` file), [`BrokerError::Json`] (malformed `metadata.json`),
/// [`BrokerError::Workspace`] (a `beamtalk-workspace` failure), or
/// [`BrokerError::Io`] (failed to create the `RELEASE_TMP` directory) before
/// any process is spawned.
pub fn spawn_front(config: &SpawnConfig) -> Result<SpawnedFront> {
    if let Some(source) = oidc_configured(&config.ide_toml_path) {
        return Err(BrokerError::OidcConfigured(source.to_string()));
    }

    if !beamtalk_workspace::workspace_dir(&config.workspace_id)
        .map(|d| d.join("metadata.json").exists())
        .unwrap_or(false)
    {
        return Err(BrokerError::UnknownWorkspace(config.workspace_id.clone()));
    }

    let mut cmd = build_launch_command(config)?;
    detach(&mut cmd);

    // Created before `cmd.spawn()` (not after) so there is no
    // JobHandle-creation-fails-after-spawn case to unwind: if this errors,
    // nothing has been spawned yet and the `?` below can propagate directly.
    #[cfg(windows)]
    let job = crate::winjob::JobHandle::new()?;

    // `mut` is only needed on the `#[cfg(windows)]` path below, which kills
    // `child` itself if job-object assignment fails; the non-Windows arm
    // never mutates it.
    #[cfg_attr(not(windows), allow(unused_mut))]
    let mut child = cmd.spawn()?;

    #[cfg(windows)]
    {
        // Best-effort-but-checked: assign right away (minimizes, per
        // `crate::winjob`'s doc, the residual pre-assignment race) and
        // surface a failure rather than silently returning a front this
        // job object does not actually protect. `cmd.spawn()` above already
        // succeeded, so a failure past this point must kill `child` itself
        // before propagating — otherwise the job object we failed to attach
        // never exists to reap it, and `crate::reap`'s PID-file sweep can't
        // catch it either (a PID file is only written after `spawn_front`
        // returns `Ok`), leaving a live, untracked `cmd.exe`/`erl.exe` tree
        // behind.
        if let Err(e) = job.assign(&child) {
            kill_orphaned_child(&mut child);
            return Err(e.into());
        }
        Ok(SpawnedFront { child, job })
    }
    #[cfg(not(windows))]
    {
        Ok(SpawnedFront { child })
    }
}

/// Kill and reap `child` after it was successfully spawned but
/// [`crate::winjob::JobHandle::assign`] then failed to attach it to the
/// job created before `spawn` — see the call site in [`spawn_front`].
/// Best-effort: swallows its own errors rather than shadowing the setup
/// failure that's already being propagated, since there's no better recovery
/// available for a failure to kill a process we're already trying to kill.
#[cfg(windows)]
fn kill_orphaned_child(child: &mut std::process::Child) {
    let _ = child.kill();
    let _ = child.wait();
}

/// Build the OS process invocation for `config`, platform-specific per this
/// module's doc comment: `bin/server <id>` on Unix (self-resolving), or on
/// Windows `bin\bt_attach.bat start` with every env var `bin/server` would
/// otherwise have set resolved and set here instead.
///
/// Shares a `Result`-returning signature with its `#[cfg(windows)]` sibling
/// below so `spawn_front` calls whichever one this build compiles through one
/// shared call site — this arm can now fail too (BT-3046's
/// [`check_launcher_platform`] guard), not only the Windows one (a missing
/// cookie file, or a `read_node_name` failure).
///
/// # Errors
///
/// Returns [`BrokerError::LauncherPlatformMismatch`] if `config.launcher`
/// doesn't look like a Unix entry point.
#[cfg(unix)]
fn build_launch_command(config: &SpawnConfig) -> Result<Command> {
    check_launcher_platform(&config.launcher)?;
    let mut cmd = Command::new(&config.launcher);
    cmd.arg(&config.workspace_id);
    for (key, value) in build_env(config) {
        cmd.env(key, value);
    }
    Ok(cmd)
}

/// Guard against [`SpawnConfig::launcher`] (or its `BEAMTALK_ATTACH_LAUNCHER`
/// override — see `desktop/src-tauri/src/launcher.rs`) pointing at the wrong
/// platform's entry point (BT-3046 adversarial-review follow-up): a Unix
/// `bin/server` shell script has no extension, while the Windows
/// `bin\bt_attach.bat` always does — a cheap, reliable discriminator without
/// needing to actually open or exec the file. Unix arm: refuses a `.bat`/
/// `.cmd`/`.exe` extension (all three are Windows-only conventions this
/// crate has no legitimate reason to see here — `.exe` isn't the *other*
/// platform's actual launcher, but it's an equally clear tell). Windows arm
/// (below): requires `.bat`/`.cmd`.
///
/// # Errors
///
/// Returns [`BrokerError::LauncherPlatformMismatch`] if `launcher` has a
/// `.bat`/`.cmd`/`.exe` extension — none of those make sense on Unix.
#[cfg(unix)]
fn check_launcher_platform(launcher: &std::path::Path) -> Result<()> {
    let ext = launcher
        .extension()
        .and_then(|e| e.to_str())
        .map(str::to_ascii_lowercase);
    match ext.as_deref() {
        Some("bat" | "cmd" | "exe") => Err(BrokerError::LauncherPlatformMismatch(
            launcher.display().to_string(),
            "this is a Unix build, but the launcher has a Windows .bat/.cmd/.exe extension",
        )),
        _ => Ok(()),
    }
}

/// Windows counterpart of the Unix `build_launch_command` above (BT-2988) —
/// see this module's doc comment for the full rationale. `bin/server` does
/// not exist on Windows, so this function does what it would have: resolve
/// `BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE` from the on-disk workspace
/// directory, generate an ephemeral `SECRET_KEY_BASE`, and set `PHX_SERVER`
/// — then invoke the release's own `bin\bt_attach.bat start` directly rather
/// than passing `workspace_id` as an argv the Windows launcher has no shell
/// script to interpret.
///
/// # Errors
///
/// Returns [`BrokerError::LauncherPlatformMismatch`] if `config.launcher`
/// doesn't look like a Windows entry point, [`BrokerError::MissingCookie`] if
/// the workspace directory has no (or an empty) `cookie` file, propagates a
/// [`crate::discovery::read_node_name`] failure (malformed `metadata.json` —
/// `spawn_front` already checked it exists before calling this), or
/// [`BrokerError::Io`] if the per-front [`release_tmp_dir`] can't be created.
#[cfg(windows)]
fn build_launch_command(config: &SpawnConfig) -> Result<Command> {
    check_launcher_platform(&config.launcher)?;
    let node_name = crate::discovery::read_node_name(&config.workspace_id)?;
    let cookie = beamtalk_workspace::read_cookie_file(&config.workspace_id)?
        .ok_or_else(|| BrokerError::MissingCookie(config.workspace_id.clone()))?;

    let mut cmd = Command::new(&config.launcher);
    cmd.arg("start");
    for (key, value) in build_env(config) {
        cmd.env(key, value);
    }
    cmd.env("BT_WORKSPACE_NODE", node_name);
    cmd.env("BT_WORKSPACE_COOKIE", cookie);
    cmd.env("SECRET_KEY_BASE", generate_secret_key_base());
    cmd.env("PHX_SERVER", "true");

    // BT-3046 adversarial-review follow-up: a bundled MSI/NSIS install lands
    // under `C:\Program Files\Beamtalk\...`, which a standard (non-admin)
    // Windows user cannot write to. Mix releases resolve `RELEASE_TMP`
    // (default `$RELEASE_ROOT/tmp`) and write the boot's resolved
    // `sys.config` there on every boot — left at the default, first attach
    // on a real install would fail with a permission error before the VM
    // even finishes booting. Point it at a per-user-writable, per-front
    // directory instead.
    let release_tmp = release_tmp_dir(&config.workspace_id, config.port);
    std::fs::create_dir_all(&release_tmp)?;
    cmd.env("RELEASE_TMP", release_tmp);

    Ok(cmd)
}

/// Guard against [`SpawnConfig::launcher`] pointing at the wrong platform's
/// entry point (BT-3046) — see the `#[cfg(unix)]` sibling above for the full
/// rationale. Windows arm: requires a `.bat`/`.cmd` extension, since
/// `bin\bt_attach.bat` (the only real Windows launcher) always has one and a
/// Unix `bin/server` shell script never does.
///
/// # Errors
///
/// Returns [`BrokerError::LauncherPlatformMismatch`] if `launcher` has no
/// `.bat`/`.cmd` extension.
#[cfg(windows)]
fn check_launcher_platform(launcher: &std::path::Path) -> Result<()> {
    let ext = launcher
        .extension()
        .and_then(|e| e.to_str())
        .map(str::to_ascii_lowercase);
    match ext.as_deref() {
        Some("bat" | "cmd") => Ok(()),
        _ => Err(BrokerError::LauncherPlatformMismatch(
            launcher.display().to_string(),
            "this is a Windows build, but the launcher has no .bat/.cmd extension",
        )),
    }
}

/// Per-front `RELEASE_TMP` directory (BT-3046) — see [`build_launch_command`]'s
/// doc comment for why the release's own default (under `RELEASE_ROOT`, which
/// a standard install puts under `Program Files`) isn't usable.
///
/// Prefers `dirs::cache_dir()` (`%LOCALAPPDATA%` on Windows — the same
/// per-user, app-local scratch location [`crate::reap::state_dir`] uses
/// `dirs::home_dir()` to build its own directory under) over the more
/// loosely-defined `std::env::temp_dir()`: the latter reads whatever `TMP`/
/// `TEMP` happen to resolve to for the current process, which under an
/// unusual environment (a service/SYSTEM context, an enterprise policy
/// redirecting `%TEMP%`) is not guaranteed to be exclusively
/// user-writable — and this directory ends up holding the resolved
/// `sys.config`, including the workspace's `SECRET_KEY_BASE`/cookie values,
/// so a predictable path under a *possibly*-shared location is worth
/// avoiding when a definitely-per-user one is one call away. Falls back to
/// `std::env::temp_dir()` (never `None`) only if `dirs::cache_dir()` itself
/// can't be resolved.
///
/// Scoped by workspace id *and* port (not just workspace id) so two fronts
/// for the same workspace — e.g. a stale one mid-reap while a new one spawns
/// on retry — never race on the same `sys.config`/boot files.
#[cfg(windows)]
fn release_tmp_dir(workspace_id: &str, port: u16) -> PathBuf {
    dirs::cache_dir()
        .unwrap_or_else(std::env::temp_dir)
        .join("beamtalk-release-tmp")
        .join(format!("{workspace_id}-{port}"))
}

/// Generate an ephemeral `SECRET_KEY_BASE` for one Windows front boot —
/// the Windows-side equivalent of `bin/server`'s `openssl rand -base64 48`
/// fallback (see this module's doc comment for why ephemeral-per-boot is
/// deliberate, not a shortcut). 48 random bytes, standard (padded) base64.
/// Unlike `beamtalk-cli`'s workspace-cookie generator (which must avoid a
/// leading `-`/`+`/`/` because that value is later parsed as an Erlang VM
/// arg), this value is never parsed that way — Phoenix only ever reads it
/// back as an opaque `SECRET_KEY_BASE` string — so no such rerolling is
/// needed here.
#[cfg(windows)]
fn generate_secret_key_base() -> String {
    use rand::Rng;
    let mut bytes = [0u8; 48];
    rand::rng().fill_bytes(&mut bytes);
    base64::Engine::encode(&base64::engine::general_purpose::STANDARD, bytes)
}

/// Config for [`spawn_front_with_port_retry`] — the fields [`SpawnConfig`]
/// needs, minus `port` (chosen per attempt) plus the retry knobs.
#[derive(Debug, Clone)]
pub struct SpawnAttemptConfig {
    pub launcher: PathBuf,
    pub workspace_id: String,
    pub bind_ip: String,
    pub ide_toml_path: PathBuf,
    /// Passed through to [`crate::port::allocate_port_with_retry`].
    pub max_port_attempts: u32,
    /// How long to wait after spawning before checking whether the child
    /// already exited — see [`spawn_front_with_port_retry`]'s doc comment
    /// for why this is a best-effort heuristic, not a proof.
    pub bind_failure_grace: Duration,
}

impl SpawnAttemptConfig {
    /// Build a config with the standard loopback bind, default `ide.toml`
    /// path, [`port::DEFAULT_MAX_ATTEMPTS`], and [`DEFAULT_BIND_FAILURE_GRACE`].
    #[must_use]
    pub fn new(launcher: PathBuf, workspace_id: impl Into<String>) -> Self {
        Self {
            launcher,
            workspace_id: workspace_id.into(),
            bind_ip: "127.0.0.1".to_string(),
            ide_toml_path: default_ide_config_path(),
            max_port_attempts: port::DEFAULT_MAX_ATTEMPTS,
            bind_failure_grace: DEFAULT_BIND_FAILURE_GRACE,
        }
    }
}

/// Default grace period for [`spawn_front_with_port_retry`]'s bind-failure
/// heuristic.
///
/// **Calibrated against a real `dist-liveview` release (BT-3004)**, racing
/// two fronts for the same port against a live workspace: a genuine
/// `:eaddrinuse` crash took 2.76s–3.40s (mean ~3.05s, n=3) to surface as a
/// process exit — the VM boots fully, the supervision tree's `Endpoint`
/// child fails to bind, the failure unwinds through `application_controller`,
/// a crash dump is written, and only then does the OS process exit. A
/// healthy front, by contrast, reaches HTTP-up in under 1s. The
/// previous 1.5s default sat *between* those two figures — comfortably
/// after a healthy bind, but routinely **before** a real conflict's crash
/// had actually happened, which would have handed
/// [`spawn_front_with_port_retry`]'s caller a doomed child (misclassified as
/// [`crate::port::SpawnAttempt::Bound`]) that then died ~1.5-2s later,
/// outside this heuristic's view. 5s gives ~1.6s of margin over the worst
/// observed crash latency, matching the safety-margin style used by
/// [`crate::readiness::ProbeTimeouts::default_local`].
///
/// This grace period is paid on **every** spawn attempt, successful or not
/// (`spawn_front_with_port_retry` always sleeps the full duration before
/// checking `try_wait`) — so raising it here trades ~3.5s of extra latency
/// on every attach for not silently handing back a front that is already
/// doomed. Correctness was judged more important than shaving that fixed
/// cost; a future revision could poll `try_wait` instead of sleeping once,
/// but that still can't return early on the *success* path (a still-running
/// process can't be told apart from one about to crash without waiting out
/// the full window), so it was not pursued here.
///
/// This also compounds with [`port::DEFAULT_MAX_ATTEMPTS`]: the pathological
/// worst case (every one of 10 fresh, OS-assigned candidate ports conflicts)
/// now takes up to 50s before [`spawn_front_with_port_retry`] gives up,
/// versus 15s at the old 1.5s grace. In practice each candidate is a
/// distinct ephemeral port from a fresh [`port::find_free_port`] call, not
/// the same port retried, so an all-10-conflict run is far less likely than
/// the single-attempt TOCTOU race [`port`]'s module docs describe — but a
/// future caller setting a tight overall UI timeout around the whole spawn
/// call should size it with this worst case in mind, not just the common
/// one-attempt path.
pub const DEFAULT_BIND_FAILURE_GRACE: Duration = Duration::from_secs(5);

/// Spawn a front with automatic retry on port conflict (ADR 0097 Broker §2;
/// ties [`crate::port::allocate_port_with_retry`] to [`spawn_front`] — see
/// [`crate::port`]'s module docs for the TOCTOU race this exists to work
/// around, which the spike flagged as unhandled in its own throwaway harness
/// and explicitly called out as real broker-core work).
///
/// **This is a heuristic, not a proof.** `spawn_front` returns as soon as
/// the launcher process execs; the actual `PORT` bind happens seconds later,
/// inside the BEAM VM's own boot sequence, and there is no synchronous
/// signal this function can observe for "the bind failed." What it *can*
/// observe is whether the spawned process is still alive after
/// `bind_failure_grace` — a process that exits almost immediately is a
/// reasonable (if imperfect) proxy for "the port was already taken and the
/// release's supervision tree gave up," while one still running is treated
/// as bound and handed to the caller. `bind_failure_grace` was calibrated
/// against a real `bin/server`/`dist-liveview` boot (BT-3004): a genuine
/// `:eaddrinuse` takes 2.76s–3.40s to surface as a process exit, versus well
/// under 1s for a healthy boot to reach HTTP-up — see
/// [`DEFAULT_BIND_FAILURE_GRACE`]'s doc comment for the full measurement and
/// the resulting default.
///
/// # Errors
///
/// Returns [`BrokerError::PortsExhausted`] if every port attempt looks like
/// a conflict, or propagates the first non-conflict error (OIDC refusal,
/// unknown workspace, spawn I/O failure, or a `try_wait` I/O error).
///
/// # Panics
///
/// Never in practice: [`port::allocate_port_with_retry`] only returns `Ok`
/// after the closure below returned [`SpawnAttempt::Bound`], which always
/// stores a child into `spawned` first — the `expect` documents that
/// invariant rather than guarding against a real failure mode.
pub fn spawn_front_with_port_retry(config: &SpawnAttemptConfig) -> Result<(SpawnedFront, u16)> {
    let spawned: RefCell<Option<SpawnedFront>> = RefCell::new(None);
    let port = port::allocate_port_with_retry(config.max_port_attempts, |candidate| {
        let spawn_config = SpawnConfig {
            launcher: config.launcher.clone(),
            workspace_id: config.workspace_id.clone(),
            port: candidate,
            bind_ip: config.bind_ip.clone(),
            ide_toml_path: config.ide_toml_path.clone(),
        };
        let mut child = spawn_front(&spawn_config)?;
        std::thread::sleep(config.bind_failure_grace);
        if let Some(_exit_status) = child.try_wait()? {
            Ok(SpawnAttempt::PortTaken)
        } else {
            *spawned.borrow_mut() = Some(child);
            Ok(SpawnAttempt::Bound)
        }
    })?;
    let child = spawned
        .into_inner()
        .expect("SpawnAttempt::Bound is only returned after storing the child above");
    Ok((child, port))
}

#[cfg(unix)]
fn detach(cmd: &mut Command) {
    use std::os::unix::process::CommandExt;
    // New process group, front as its own leader — survives a SIGINT/SIGTERM
    // sent to the broker's foreground process group.
    cmd.process_group(0);
}

#[cfg(windows)]
fn detach(cmd: &mut Command) {
    use std::os::windows::process::CommandExt;
    const CREATE_NEW_PROCESS_GROUP: u32 = 0x0000_0200;
    // Adversarial-review follow-up (BT-2988): `bin\bt_attach.bat` can only
    // run via a console-subsystem wrapper (`cmd.exe` — see this module's
    // doc comment), which without this flag pops a visible console window
    // per front, behind the desktop app's GUI, that a user could close and
    // kill their workspace out from under them. `CREATE_NO_WINDOW`
    // suppresses that; it does not affect `erl.exe`'s own I/O (the front
    // doesn't attach a console either way — `PHX_SERVER=true` boot has
    // nothing to print to one).
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;
    cmd.creation_flags(CREATE_NEW_PROCESS_GROUP | CREATE_NO_WINDOW);
}

#[cfg(not(any(unix, windows)))]
fn detach(_cmd: &mut Command) {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_env_sets_the_four_required_vars() {
        let config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123", 4567);
        let env = build_env(&config);
        assert_eq!(
            env,
            vec![
                ("PORT".to_string(), "4567".to_string()),
                ("BT_ATTACH_BIND_IP".to_string(), "127.0.0.1".to_string()),
                ("BT_ATTACH_NODE_SUFFIX".to_string(), "abc123".to_string()),
                ("RELEASE_DISTRIBUTION".to_string(), "none".to_string()),
            ]
        );
    }

    #[test]
    fn build_env_uses_configured_bind_ip() {
        let mut config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123", 4567);
        "::1".clone_into(&mut config.bind_ip);
        let env = build_env(&config);
        assert!(env.contains(&("BT_ATTACH_BIND_IP".to_string(), "::1".to_string())));
    }

    #[test]
    fn spawn_front_refuses_when_oidc_env_present() {
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        let tmp = tempfile::TempDir::new().unwrap();
        // SAFETY: guarded by ENV_LOCK above — serialized against every other
        // test in this crate that touches BT_OIDC_* env vars.
        unsafe { std::env::set_var("BT_OIDC_ISSUER", "https://idp.example.com") };

        let mut config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123", 4567);
        config.ide_toml_path = tmp.path().join("ide.toml");
        let result = spawn_front(&config);

        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_OIDC_ISSUER") };

        assert!(matches!(result, Err(BrokerError::OidcConfigured(_))));
    }

    #[test]
    fn spawn_front_refuses_unknown_workspace_when_oidc_absent() {
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        let tmp = tempfile::TempDir::new().unwrap();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_OIDC_ISSUER") };

        let mut config = SpawnConfig::new(
            PathBuf::from("/bin/true"),
            "__nonexistent_workspace_broker_test__",
            4567,
        );
        config.ide_toml_path = tmp.path().join("ide.toml");
        let result = spawn_front(&config);
        assert!(matches!(result, Err(BrokerError::UnknownWorkspace(_))));
    }

    // ── Windows launch command (BT-2988) ────────────────────────────────
    //
    // `build_launch_command`/`generate_secret_key_base` only exist on
    // `cfg(windows)` — these tests build a real, throwaway workspace
    // directory under the actual home dir (same pattern as the rest of this
    // file) and assert the resolved `Command`'s program/args/env, using
    // `std::process::Command`'s stable `get_program`/`get_args`/`get_envs`
    // introspection rather than actually spawning anything.

    #[cfg(windows)]
    struct WindowsTestWorkspaceDir {
        id: String,
    }

    #[cfg(windows)]
    impl WindowsTestWorkspaceDir {
        fn new(prefix: &str, node_name: Option<&str>, cookie: Option<&str>) -> Self {
            let id = format!("{prefix}_{}", std::process::id());
            let dir = beamtalk_workspace::workspace_dir(&id).unwrap();
            std::fs::create_dir_all(&dir).unwrap();
            let metadata = match node_name {
                Some(name) => format!(r#"{{"workspace_id":"{id}","node_name":"{name}"}}"#),
                None => format!(r#"{{"workspace_id":"{id}"}}"#),
            };
            std::fs::write(dir.join("metadata.json"), metadata).unwrap();
            if let Some(cookie) = cookie {
                std::fs::write(dir.join("cookie"), cookie).unwrap();
            }
            Self { id }
        }
    }

    #[cfg(windows)]
    impl Drop for WindowsTestWorkspaceDir {
        fn drop(&mut self) {
            if let Ok(dir) = beamtalk_workspace::workspace_dir(&self.id) {
                let _ = std::fs::remove_dir_all(dir);
            }
        }
    }

    #[cfg(windows)]
    #[test]
    fn build_launch_command_resolves_node_cookie_and_invokes_bt_attach_start() {
        let ws = WindowsTestWorkspaceDir::new(
            "win_launch_ok",
            Some("bt_attach_win_launch_ok_1@localhost"),
            Some("testcookie123"),
        );
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone(), 4567);

        let cmd = build_launch_command(&config).expect("should build a command");

        assert_eq!(
            cmd.get_program(),
            std::ffi::OsStr::new(r"bin\bt_attach.bat")
        );
        // No positional workspace-id arg on Windows — bin\bt_attach.bat takes
        // the release's own `start` command instead (see this module's doc
        // comment for why).
        assert_eq!(
            cmd.get_args().collect::<Vec<_>>(),
            vec![std::ffi::OsStr::new("start")]
        );

        let envs: std::collections::HashMap<String, String> = cmd
            .get_envs()
            .filter_map(|(k, v)| {
                v.map(|v| {
                    (
                        k.to_string_lossy().into_owned(),
                        v.to_string_lossy().into_owned(),
                    )
                })
            })
            .collect();
        assert_eq!(envs.get("PORT").map(String::as_str), Some("4567"));
        assert_eq!(
            envs.get("BT_ATTACH_BIND_IP").map(String::as_str),
            Some("127.0.0.1")
        );
        assert_eq!(
            envs.get("RELEASE_DISTRIBUTION").map(String::as_str),
            Some("none")
        );
        assert_eq!(
            envs.get("BT_WORKSPACE_NODE").map(String::as_str),
            Some("bt_attach_win_launch_ok_1@localhost")
        );
        assert_eq!(
            envs.get("BT_WORKSPACE_COOKIE").map(String::as_str),
            Some("testcookie123")
        );
        assert_eq!(envs.get("PHX_SERVER").map(String::as_str), Some("true"));
        assert!(
            envs.contains_key("SECRET_KEY_BASE"),
            "must set an ephemeral SECRET_KEY_BASE itself — there is no bin/server to do it"
        );
        // BT-3046: RELEASE_TMP must be a per-user-writable path, not the
        // release's own default under RELEASE_ROOT (a bundled install puts
        // that under Program Files, which a standard user can't write to).
        let release_tmp = envs
            .get("RELEASE_TMP")
            .expect("must set RELEASE_TMP to a writable directory");
        let expected_base = dirs::cache_dir().unwrap_or_else(std::env::temp_dir);
        assert!(
            PathBuf::from(release_tmp).starts_with(&expected_base),
            "RELEASE_TMP {release_tmp:?} should live under {expected_base:?} \
             (dirs::cache_dir(), i.e. %LOCALAPPDATA% on Windows)"
        );
        assert!(
            std::path::Path::new(release_tmp).is_dir(),
            "RELEASE_TMP {release_tmp:?} should already exist (created by build_launch_command) \
             — this also doubles as a real writability check: create_dir_all only succeeds if \
             the current user can actually write there"
        );
    }

    #[cfg(windows)]
    #[test]
    fn release_tmp_dir_is_scoped_by_workspace_and_port() {
        let a = release_tmp_dir("workspace_a", 4567);
        let b = release_tmp_dir("workspace_a", 4568);
        let c = release_tmp_dir("workspace_b", 4567);
        assert_ne!(
            a, b,
            "different ports for the same workspace must not collide"
        );
        assert_ne!(
            a, c,
            "different workspaces on the same port must not collide"
        );
    }

    #[cfg(windows)]
    #[test]
    fn build_launch_command_errors_on_launcher_platform_mismatch() {
        let ws = WindowsTestWorkspaceDir::new("win_launch_mismatch", None, Some("c"));
        // A Unix-shaped launcher path (no .bat/.cmd extension) handed to a
        // Windows build — the exact adversarial-review scenario BT-3046
        // flagged: this used to fail with an opaque OS error instead of a
        // named one.
        let config = SpawnConfig::new(PathBuf::from(r"bin\server"), ws.id.clone(), 4567);

        let result = build_launch_command(&config);

        assert!(
            matches!(result, Err(BrokerError::LauncherPlatformMismatch(_, _))),
            "expected LauncherPlatformMismatch, got {result:?}"
        );
    }

    #[cfg(windows)]
    #[test]
    fn build_launch_command_falls_back_to_default_node_name_when_metadata_lacks_one() {
        let ws = WindowsTestWorkspaceDir::new("win_launch_default_node", None, Some("c"));
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone(), 4567);

        let cmd = build_launch_command(&config).expect("should build a command");

        let envs: std::collections::HashMap<String, String> = cmd
            .get_envs()
            .filter_map(|(k, v)| {
                v.map(|v| {
                    (
                        k.to_string_lossy().into_owned(),
                        v.to_string_lossy().into_owned(),
                    )
                })
            })
            .collect();
        assert_eq!(
            envs.get("BT_WORKSPACE_NODE").map(String::as_str),
            Some(crate::discovery::default_node_name(&ws.id)).as_deref()
        );
    }

    #[cfg(windows)]
    #[test]
    fn build_launch_command_errors_when_cookie_file_missing() {
        let ws = WindowsTestWorkspaceDir::new("win_launch_no_cookie", None, None);
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone(), 4567);

        let result = build_launch_command(&config);

        assert!(matches!(result, Err(BrokerError::MissingCookie(_))));
    }

    #[cfg(windows)]
    #[test]
    fn generate_secret_key_base_produces_distinct_values_each_call() {
        let a = generate_secret_key_base();
        let b = generate_secret_key_base();
        assert_ne!(
            a, b,
            "must not reuse the same ephemeral secret across boots"
        );
        assert_eq!(
            a.len(),
            64,
            "expected standard (padded) base64 of a 48-byte buffer, which is always 64 chars"
        );
    }

    // ── spawn_front_with_port_retry ─────────────────────────────────────
    //
    // `spawn_front` itself refuses unless `~/.beamtalk/workspaces/<id>/`
    // exists, so these tests create (and clean up) a real, throwaway
    // workspace directory under the actual home dir — the same pattern
    // `beamtalk-cli`'s own tests use (there's no HOME-override hook in
    // `beamtalk_workspace`), rather than pointing at a fake filesystem root.

    // Only the #[cfg(unix)] spawn_front_with_port_retry_* tests below
    // construct this (write_launcher_script needs a real #!/bin/sh script,
    // which has no Windows equivalent) — gate it to match, or it's dead
    // code on Windows.
    #[cfg(unix)]
    struct TestWorkspaceDir {
        id: String,
    }

    #[cfg(unix)]
    impl TestWorkspaceDir {
        fn new(prefix: &str) -> Self {
            let id = format!("{prefix}_{}", std::process::id());
            let dir = beamtalk_workspace::workspace_dir(&id).unwrap();
            std::fs::create_dir_all(&dir).unwrap();
            std::fs::write(dir.join("metadata.json"), b"{}").unwrap();
            Self { id }
        }
    }

    #[cfg(unix)]
    impl Drop for TestWorkspaceDir {
        fn drop(&mut self) {
            if let Ok(dir) = beamtalk_workspace::workspace_dir(&self.id) {
                let _ = std::fs::remove_dir_all(dir);
            }
        }
    }

    /// Write an executable shell script to `dir` that ignores its argv
    /// (the workspace id `spawn_front` always appends) and runs `body`.
    ///
    /// The returned executable is produced by an out-of-process `cp` rather
    /// than by writing `dir/name` directly, to avoid `ETXTBSY` ("Text file
    /// busy") when it is later `exec`'d. A file *this* process has open for
    /// writing is inherited by any concurrent `fork()` — every other test in
    /// this binary that calls `Command::spawn` (`reap`'s `sleep` children,
    /// the sibling launcher tests) forks — and the forked child keeps that
    /// descriptor open until it `exec`s. The kernel refuses to `exec` a file
    /// any process holds open for writing, so a sibling fork landing inside
    /// the write window here makes our own spawn fail. Copying via a child
    /// process means the launcher inode is never write-open in this process,
    /// so no sibling fork can ever inherit a writer for it (`chmod` below
    /// opens nothing).
    #[cfg(unix)]
    fn write_launcher_script(dir: &std::path::Path, name: &str, body: &str) -> PathBuf {
        use std::os::unix::fs::PermissionsExt;
        let source = dir.join(format!("{name}.source"));
        std::fs::write(&source, format!("#!/bin/sh\n{body}\n")).unwrap();

        let path = dir.join(name);
        let status = Command::new("cp")
            .arg(&source)
            .arg(&path)
            .status()
            .expect("cp should be runnable");
        assert!(
            status.success(),
            "cp {source:?} -> {path:?} failed: {status}"
        );

        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
        path
    }

    #[cfg(unix)]
    #[test]
    fn build_launch_command_errors_on_launcher_platform_mismatch() {
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), "abc123", 4567);

        let result = build_launch_command(&config);

        assert!(
            matches!(result, Err(BrokerError::LauncherPlatformMismatch(_, _))),
            "expected LauncherPlatformMismatch, got {result:?}"
        );
    }

    #[cfg(unix)]
    #[test]
    fn build_launch_command_errors_on_exe_launcher_extension() {
        // Not `bin/server`'s actual Windows counterpart's extension, but an
        // equally clear tell that this launcher was built for Windows
        // (adversarial-review follow-up: the original denylist only caught
        // .bat/.cmd, silently accepting an obviously-wrong-platform .exe).
        let config = SpawnConfig::new(PathBuf::from("bin/bt_attach.exe"), "abc123", 4567);

        let result = build_launch_command(&config);

        assert!(
            matches!(result, Err(BrokerError::LauncherPlatformMismatch(_, _))),
            "expected LauncherPlatformMismatch, got {result:?}"
        );
    }

    #[cfg(unix)]
    #[test]
    fn spawn_front_with_port_retry_returns_a_live_child_on_the_first_attempt() {
        let ws = TestWorkspaceDir::new("port_retry_ok");
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher = write_launcher_script(tmp.path(), "server", "sleep 5");

        let mut config = SpawnAttemptConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC
        config.bind_failure_grace = Duration::from_millis(100);

        let (mut child, port) =
            spawn_front_with_port_retry(&config).expect("should spawn and bind on first try");
        assert!(port >= 1024);
        assert_eq!(
            child.try_wait().unwrap(),
            None,
            "child should still be running (the launcher sleeps)"
        );

        let _ = child.kill();
        let _ = child.wait();
    }

    #[cfg(unix)]
    #[test]
    fn spawn_front_with_port_retry_gives_up_after_max_attempts_when_launcher_always_exits() {
        let ws = TestWorkspaceDir::new("port_retry_fail");
        let tmp = tempfile::TempDir::new().unwrap();
        // Simulates an immediate `:eaddrinuse`-style crash on every attempt.
        let launcher = write_launcher_script(tmp.path(), "server", "exit 1");

        let mut config = SpawnAttemptConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml");
        config.bind_failure_grace = Duration::from_millis(50);
        config.max_port_attempts = 3;

        let result = spawn_front_with_port_retry(&config);
        assert!(
            matches!(result, Err(BrokerError::PortsExhausted(3))),
            "expected PortsExhausted(3), got {result:?}"
        );
    }

    #[cfg(unix)]
    #[test]
    fn spawn_front_with_port_retry_recovers_after_transient_conflicts() {
        let ws = TestWorkspaceDir::new("port_retry_recover");
        let tmp = tempfile::TempDir::new().unwrap();
        // Fails twice (via a counter file each invocation increments), then
        // stays up — exercises the actual retry-then-succeed path, not just
        // the all-fail / all-succeed extremes.
        let counter_path = tmp.path().join("attempts");
        std::fs::write(&counter_path, b"0").unwrap();
        let script = format!(
            "n=$(cat {0})\nn=$((n + 1))\necho \"$n\" > {0}\nif [ \"$n\" -lt 3 ]; then exit 1; fi\nsleep 5\n",
            counter_path.display()
        );
        let launcher = write_launcher_script(tmp.path(), "server", &script);

        let mut config = SpawnAttemptConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml");
        config.bind_failure_grace = Duration::from_millis(100);
        config.max_port_attempts = 5;

        let (mut child, _port) =
            spawn_front_with_port_retry(&config).expect("should eventually succeed");
        assert_eq!(
            child.try_wait().unwrap(),
            None,
            "third attempt should stay up"
        );
        let attempts: u32 = std::fs::read_to_string(&counter_path)
            .unwrap()
            .trim()
            .parse()
            .unwrap();
        assert_eq!(attempts, 3, "should have taken exactly 3 attempts");

        let _ = child.kill();
        let _ = child.wait();
    }

    // ── spawn_front_with_port_retry: real spawn (BT-3046) ───────────────
    //
    // The `build_launch_command_*` tests above only assert that the
    // `cmd.env(...)` calls that produced a `Command` ran, via
    // `get_program`/`get_args`/`get_envs` introspection — they never
    // actually spawn anything, so they can't catch anything about real
    // Windows process-spawning behavior (the `cmd.exe` wrapping a `.bat`
    // triggers per `Command`'s "BatBadBut" fix, whether `creation_flags`
    // actually applied, whether the job-object assignment in `spawn_front`
    // succeeds against a real child). `spawn_front_with_port_retry` — the
    // function the desktop app actually calls — previously had zero Windows
    // test coverage; these two mirror the `#[cfg(unix)]`
    // `spawn_front_with_port_retry_*` tests above, using a throwaway `.bat`
    // in place of a `#!/bin/sh` script.
    //
    // Both hold `ENV_LOCK` across the actual spawn: unlike the Unix
    // `write_launcher_script` tests (which exec their throwaway script
    // directly), a `.bat` here runs via `cmd.exe`, which resolves bare
    // commands like `ping` against the **current** `%PATH%` — and
    // `cli_ops::tests::resolve_cli_path_{finds_an_executable_on_path,errors_when_nothing_found}`
    // temporarily replace `PATH` process-globally (also under `ENV_LOCK`).
    // Without sharing that lock here, a `.bat`-launched `cmd.exe` can race
    // one of those tests and fail to find `ping` on a wiped `PATH` — this
    // was reproduced locally before adding the guard.

    /// Write a throwaway `.bat` launcher to `dir` that ignores its argv (the
    /// `start` positional arg `build_launch_command`'s Windows arm always
    /// appends) and runs `body`. Unlike the Unix `write_launcher_script`
    /// above, no `cp`-via-child-process dance is needed: Windows doesn't
    /// `fork()`, so there is no descriptor-inherited-by-a-sibling-fork
    /// `ETXTBSY`-equivalent to work around here.
    #[cfg(windows)]
    fn write_launcher_batch(dir: &std::path::Path, name: &str, body: &str) -> PathBuf {
        let path = dir.join(format!("{name}.bat"));
        std::fs::write(&path, format!("@echo off\r\n{body}\r\n")).unwrap();
        path
    }

    #[cfg(windows)]
    #[test]
    fn spawn_front_with_port_retry_returns_a_live_child_on_the_first_attempt_windows() {
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        let ws = WindowsTestWorkspaceDir::new(
            "port_retry_ok_win",
            Some("bt_attach_port_retry_ok_win@localhost"),
            Some("testcookie"),
        );
        let tmp = tempfile::TempDir::new().unwrap();
        // A short ping-based sleep — no coreutils `sleep` on Windows.
        let launcher = write_launcher_batch(tmp.path(), "bt_attach", "ping -n 6 127.0.0.1 >nul");

        let mut config = SpawnAttemptConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC
        config.bind_failure_grace = Duration::from_millis(300);

        let (mut child, port) =
            spawn_front_with_port_retry(&config).expect("should spawn and bind on first try");
        assert!(port >= 1024);
        assert_eq!(
            child.try_wait().unwrap(),
            None,
            "child should still be running (the launcher is mid-ping-sleep)"
        );

        let _ = child.kill();
        let _ = child.wait();
    }

    #[cfg(windows)]
    #[test]
    fn spawn_front_with_port_retry_gives_up_after_max_attempts_when_launcher_always_exits_windows()
    {
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        let ws = WindowsTestWorkspaceDir::new(
            "port_retry_fail_win",
            Some("bt_attach_port_retry_fail_win@localhost"),
            Some("testcookie"),
        );
        let tmp = tempfile::TempDir::new().unwrap();
        // Simulates an immediate `:eaddrinuse`-style crash on every attempt.
        let launcher = write_launcher_batch(tmp.path(), "bt_attach", "exit /b 1");

        let mut config = SpawnAttemptConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml");
        // Wider than the Unix counterpart's 50ms: a freshly-written `.bat` in
        // a fresh tempdir is a plausible target for Windows Defender's
        // real-time-scan-on-first-exec latency, on top of `cmd.exe`'s own
        // (larger than a POSIX shell's) process-creation overhead — a tight
        // grace here risks misclassifying a genuinely-crashing launcher as
        // still bound under CI load.
        config.bind_failure_grace = Duration::from_millis(750);
        config.max_port_attempts = 3;

        let result = spawn_front_with_port_retry(&config);
        assert!(
            matches!(result, Err(BrokerError::PortsExhausted(3))),
            "expected PortsExhausted(3), got {result:?}"
        );
    }

    // ── SpawnedFront's JobHandle actually kills the whole tree (BT-3046) ──
    //
    // Adversarial-review follow-up: the two tests above kill/wait `child`
    // (the `cmd.exe` wrapper `Child::id()` refers to — see this module's doc
    // comment) and treat that as proof the front is "reachable/killable
    // through spawn_front/SpawnedFront". That's true of `cmd.exe`, but it
    // was never proof about the process `crate::winjob::JobHandle` actually
    // exists to reach: `erl.exe` (here, `ping.exe`) underneath it. Every doc
    // comment in this module and `crate::winjob` about the job-object
    // mechanism says some variant of "this has not been exercised against a
    // real Windows boot — no Windows sandbox was available to test it
    // against." This test closes that: it finds the grandchild via a real
    // `CreateToolhelp32Snapshot` process-tree walk, drops `SpawnedFront`
    // (closing the job handle, which `JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE`
    // turns into killing every process still assigned to the job), and
    // asserts the grandchild is actually gone — not just the immediate
    // `cmd.exe` child.

    /// Find a live process whose `th32ParentProcessID` is `parent_pid`, via
    /// `CreateToolhelp32Snapshot` (no `wmic`/`PowerShell` dependency — a
    /// plain Win32 API call, consistent with how the rest of this crate
    /// talks to Windows directly rather than shelling out).
    #[cfg(windows)]
    fn find_child_pid(parent_pid: u32) -> Option<u32> {
        use windows_sys::Win32::Foundation::{CloseHandle, INVALID_HANDLE_VALUE};
        use windows_sys::Win32::System::Diagnostics::ToolHelp::{
            CreateToolhelp32Snapshot, PROCESSENTRY32W, Process32FirstW, Process32NextW,
            TH32CS_SNAPPROCESS,
        };

        // SAFETY: Windows API call with documented parameters — a snapshot
        // of all processes (0 as th32ProcessID, per the API contract) is
        // requested, and the returned handle is checked before further use.
        let snapshot = unsafe { CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0) };
        if snapshot == INVALID_HANDLE_VALUE {
            return None;
        }

        let mut entry = PROCESSENTRY32W {
            dwSize: u32::try_from(std::mem::size_of::<PROCESSENTRY32W>())
                .expect("struct size fits in u32"),
            ..Default::default()
        };
        let mut found = None;
        // SAFETY: `snapshot` is the just-created, still-owned handle above;
        // `entry` is a validly-initialized, correctly-sized buffer for this
        // call (`dwSize` set per the API's documented requirement).
        if unsafe { Process32FirstW(snapshot, &raw mut entry) } != 0 {
            loop {
                if entry.th32ParentProcessID == parent_pid {
                    found = Some(entry.th32ProcessID);
                    break;
                }
                // SAFETY: same snapshot/entry as above, called in a loop
                // until it reports no more entries.
                if unsafe { Process32NextW(snapshot, &raw mut entry) } == 0 {
                    break;
                }
            }
        }
        // SAFETY: `snapshot` is a valid handle owned by this function, not
        // used again after this point.
        unsafe { CloseHandle(snapshot) };
        found
    }

    /// Poll `condition` until it's true or `timeout` elapses, sleeping
    /// briefly between checks — used below in place of a single fixed sleep,
    /// since both "`ping.exe` has started" and "the job-object kill has
    /// propagated" are OS-scheduling-dependent, not deterministic delays.
    #[cfg(windows)]
    fn poll_until(timeout: Duration, mut condition: impl FnMut() -> bool) -> bool {
        let deadline = std::time::Instant::now() + timeout;
        loop {
            if condition() {
                return true;
            }
            if std::time::Instant::now() >= deadline {
                return false;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    #[cfg(windows)]
    #[test]
    fn dropping_spawned_front_kills_the_whole_process_tree_not_just_cmd_exe() {
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        let ws = WindowsTestWorkspaceDir::new(
            "job_kill_win",
            Some("bt_attach_job_kill_win@localhost"),
            Some("testcookie"),
        );
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher = write_launcher_batch(tmp.path(), "bt_attach", "ping -n 30 127.0.0.1 >nul");

        let mut config = SpawnConfig::new(launcher, ws.id.clone(), 54_321);
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC

        let front = spawn_front(&config).expect("should spawn");
        let cmd_pid = front.id();

        let ping_pid = {
            let mut found = None;
            poll_until(Duration::from_secs(5), || {
                found = find_child_pid(cmd_pid);
                found.is_some()
            });
            found.expect(
                "ping.exe should appear as a child of the cmd.exe wrapper within 5s — \
                 if this fails, either the .bat didn't launch ping in time, or Windows' \
                 BatBadBut cmd.exe-wrapping behavior this module's doc comment describes \
                 no longer holds",
            )
        };
        assert!(
            crate::reap::is_process_alive(ping_pid),
            "the discovered grandchild should be alive before the kill"
        );

        drop(front);

        let died = poll_until(Duration::from_secs(5), || {
            !crate::reap::is_process_alive(ping_pid)
        });
        assert!(
            died,
            "dropping SpawnedFront should kill the whole process tree via \
             crate::winjob::JobHandle's JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE — \
             ping.exe (pid {ping_pid}) survived cmd.exe (pid {cmd_pid}) being \
             dropped, which would mean every front's erl.exe orphans on detach"
        );
    }
}
