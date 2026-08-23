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
//! **Why this is a Rust reimplementation and not a `bin\server.bat` mirroring
//! `bin/server`'s shape (BT-3045 adversarial-review follow-up):** `bin/server`
//! resolves `BT_WORKSPACE_NODE` with a `sed` regex against `metadata.json`,
//! which works but is exactly the kind of fragile hand-rolled JSON access a
//! `.bat` equivalent (batch has no JSON parser at all — a working port would
//! mean an even more fragile `findstr`/`for /f` regex reimplementation) would
//! have to imitate to get *worse* correctness, not parity. Windows already
//! gets a strictly better implementation of the same resolution
//! ([`crate::discovery::read_node_name`], the real `serde_json` parser
//! [`crate::discovery::discover_workspaces`] itself uses) by staying in Rust
//! — introducing a second, `.bat`-based entry point purely for symmetry with
//! Unix would add a second-worse implementation of the one thing `bin/server`
//! does that actually needs mirroring, not remove the drift risk. What *was*
//! missing, and what BT-3045 actually closes, is a **test** tying the two
//! together: `spawn::tests::windows_launch_env_matches_bin_server_contract`
//! (Windows-only, since it exercises `build_launch_command`'s Windows arm)
//! reads `bin/server`'s source and asserts each of today's known contract env
//! vars still appears there as a real assignment, and is also set here — so
//! *removing or renaming* one of them in `bin/server` fails this crate's test
//! suite instead of silently drifting. It is a removal/rename guard, not
//! full behavioral-equivalence proof — see that test's own doc comment for
//! what it deliberately does not (and structurally cannot, without
//! duplicating `bin/server`'s logic) catch, e.g. a *new* var `bin/server`
//! starts setting that this test's hardcoded list hasn't been told about yet.
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
use std::fs::{File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

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
///
/// `ERL_EPMD_ADDRESS` here (resolved by
/// [`beamtalk_workspace::resolve_epmd_address`], shared with
/// `beamtalk-cli`'s own workspace startup — see that function's doc
/// comment for the full ADR 0091 Decision 5 rationale) keeps Erlang
/// distribution off untrusted networks by pinning the address epmd
/// binds/contacts to loopback, so a node that *starts* epmd (as the
/// front's own lazy `ensure_distributed/0` does, the first time anything
/// calls its `/readiness` endpoint) does not expose the port mapper on
/// `0.0.0.0`. Without this, a Tauri app launched from Finder/dock — which
/// does not inherit whatever `ERL_EPMD_ADDRESS` a user's shell profile
/// might set, unlike a terminal-launched `beamtalk workspace create` —
/// would leave epmd to its own un-pinned default the moment it's the first
/// thing on the machine to start one. `bin/server`
/// (`editors/liveview/rel/overlays/bin/server`) does not set this itself,
/// so it must come from here, same as every other var in this function; a
/// pre-existing promiscuous epmd from *other* tooling is a separate case,
/// caught by the CLI's own preflight check
/// (`beamtalk_cli::commands::workspace::epmd::check_epmd_loopback`), not by
/// anything this broker can control after the fact.
#[must_use]
pub fn build_env(config: &SpawnConfig) -> Vec<(String, String)> {
    let epmd_address = beamtalk_workspace::resolve_epmd_address();
    vec![
        ("PORT".to_string(), config.port.to_string()),
        ("BT_ATTACH_BIND_IP".to_string(), config.bind_ip.clone()),
        (
            "BT_ATTACH_NODE_SUFFIX".to_string(),
            attach_node_suffix(&config.workspace_id),
        ),
        ("RELEASE_DISTRIBUTION".to_string(), "none".to_string()),
        ("ERL_EPMD_ADDRESS".to_string(), epmd_address),
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
/// any process is spawned. Also returns [`BrokerError::LauncherNotFound`]
/// (BT-3056) if `config.launcher` looks right for this platform but no file
/// actually exists there — the common unpackaged-dev-build failure mode,
/// checked explicitly rather than left to surface as a generic `Io` error
/// out of `Command::spawn` below.
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

    // BT-3056 adversarial-review follow-up: checked here, immediately after
    // `build_launch_command` succeeds, rather than inside it — that function
    // has its own unit tests exercising synthetic non-existent launcher
    // paths (e.g. `bin\bt_attach.bat`, `bin/server`) purely for `Command`
    // introspection (program/args/env), which a check inside
    // `build_launch_command` itself would break. `config.launcher` and
    // `cmd`'s resolved program are always the same path on both platforms
    // (see `build_launch_command`'s two arms), so checking the former here
    // is equivalent to checking the latter.
    if !config.launcher.is_file() {
        // Second adversarial-review follow-up (BT-3056): on Windows,
        // `build_launch_command` above already created this attempt's
        // per-front `RELEASE_TMP` directory (`std::fs::create_dir_all`) —
        // returning here without removing it would leave that directory
        // behind forever: no `FrontRecord` is ever written for a
        // `spawn_front` call that fails this early, so `remove_record`'s own
        // `RELEASE_TMP` cleanup hook (BT-3046/BT-3059) never runs for it.
        // Same cleanup the job-object-assign failure path below already
        // does for the identical reason, just inlined here too.
        #[cfg(windows)]
        {
            let _ = std::fs::remove_dir_all(release_tmp_dir(&config.workspace_id, config.port));
        }
        return Err(BrokerError::LauncherNotFound(
            config.launcher.display().to_string(),
        ));
    }

    redirect_front_stdio(&mut cmd, &config.workspace_id);

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
            // Best-effort (BT-3046 adversarial-review follow-up, low
            // severity): `build_launch_command` above already created this
            // attempt's RELEASE_TMP directory. At this point in the boot
            // sequence the release has likely not written secrets into it
            // yet, but there's no reason to leave even an empty stray
            // directory behind — same cleanup `remove_record` does on the
            // success path, just inlined here since this failure never
            // reaches a saved `FrontRecord` for `remove_record` to key off.
            let _ = std::fs::remove_dir_all(release_tmp_dir(&config.workspace_id, config.port));
            return Err(e.into());
        }
        Ok(SpawnedFront { child, job })
    }
    #[cfg(not(windows))]
    {
        Ok(SpawnedFront { child })
    }
}

/// Redirect `cmd`'s stdout/stderr to `~/.beamtalk/workspaces/<id>/attach.log`
/// (append mode, accumulating across restarts — mirrors `workspace.log`'s own
/// convention on the workspace-node side) instead of the default inherited
/// stdio. Inherited stdio is the packaged Tauri app's own — i.e. nowhere, in
/// a release build (`windows_subsystem = "windows"` on Windows; macOS/Linux
/// app bundles have no attached terminal either) — so without this, a
/// spawned front's crash or an Elixir-side exception is invisible short of
/// running the release binary manually from a terminal (BT-3225).
///
/// Best-effort: if the log file can't be opened (e.g. the workspace
/// directory somehow isn't writable), stdout/stderr fall back to
/// [`Stdio::null`] rather than failing the whole spawn over a logging
/// nicety — `attach.log`'s absence is itself a signal something is wrong on
/// this machine, for whoever goes looking for it.
fn redirect_front_stdio(cmd: &mut Command, workspace_id: &str) {
    let stdio_pair = beamtalk_workspace::workspace_dir(workspace_id)
        .ok()
        .and_then(|dir| open_front_log(&dir.join("attach.log")).ok());

    if let Some((stdout_file, stderr_file)) = stdio_pair {
        cmd.stdout(Stdio::from(stdout_file));
        cmd.stderr(Stdio::from(stderr_file));
    } else {
        cmd.stdout(Stdio::null());
        cmd.stderr(Stdio::null());
    }
}

/// Open `path` for append (creating it if absent) and return two handles to
/// the same underlying file description — via [`File::try_clone`], not two
/// independent `open` calls — so stdout and stderr share one file offset and
/// interleave in write order rather than racing to overwrite each other.
///
/// Created `0o600` (owner-only) on Unix, matching every other
/// security-relevant file already in the same workspace directory
/// (`workspace.log`, `cookie`, `vm.args`, `pid`, `port` are all `0600`) —
/// the front's own `Logger` output can carry the same class of sensitive
/// runtime detail `workspace.log` does, so it gets the same posture rather
/// than the world-readable default (`create(true)`'s default mode, same as
/// e.g. `metadata.json` — appropriate there, not here). No Windows
/// equivalent is set here; it inherits the parent directory's ACLs, same as
/// this crate's other Windows gaps this module's doc comment already notes.
fn open_front_log(path: &Path) -> io::Result<(File, File)> {
    let mut options = OpenOptions::new();
    options.create(true).append(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let file = options.open(path)?;
    let cloned = file.try_clone()?;
    Ok((file, cloned))
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
/// directory, generate an ephemeral `SECRET_KEY_BASE`, set `PHX_SERVER`, and
/// `cd` to the release root — then invoke the release's own
/// `bin\bt_attach.bat start` directly rather than passing `workspace_id` as
/// an argv the Windows launcher has no shell script to interpret.
///
/// # Errors
///
/// Returns [`BrokerError::LauncherPlatformMismatch`] if `config.launcher`
/// doesn't look like a Windows entry point, [`BrokerError::MissingCookie`] if
/// the workspace directory has no (or an empty) `cookie` file, propagates a
/// [`crate::discovery::read_node_name`] failure — malformed `metadata.json`
/// (`spawn_front` already checked it exists before calling this), or
/// [`BrokerError::MissingNodeName`] when it exists but has no `node_name`
/// field yet (a workspace created but never started — BT-3060, matching
/// `bin/server`'s Unix fail-fast behavior instead of guessing) — or
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

    // BT-3045 adversarial-review follow-up: `bin/server`'s Unix arm always
    // `cd -P -- "$(dirname -- "$0")/.."`s to the release root before its
    // final `exec bin/bt_attach start` (see the sibling `#[cfg(unix)]`
    // `build_launch_command` above, which relies on that `exec` inheriting
    // the launcher's own cwd rather than setting one itself) — this arm
    // never had an equivalent, so the BEAM inherited whatever cwd the
    // spawning Tauri app process happened to have (unrelated to the release)
    // instead of the release root, affecting crash dumps and any
    // relative-path writes the release makes assuming it's running from
    // there. `release_root` mirrors that `dirname/..` computation; a
    // best-effort skip (not an error) when it can't be determined keeps this
    // function's error surface unchanged for callers/tests that pass a bare
    // relative launcher path with no real parent directory.
    if let Some(root) = release_root(&config.launcher) {
        cmd.current_dir(root);
    }

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

/// The release root a Windows launcher path implies — `bin\bt_attach.bat`'s
/// grandparent directory, mirroring `bin/server`'s own `dirname -- "$0")/..`
/// (this module's doc comment, BT-3045).
///
/// **Requires an absolute `launcher`, deliberately** (adversarial-review
/// follow-up) — `desktop/src-tauri/src/launcher.rs`'s `resolve_launcher_path`
/// resolves an *absolute* path in the normal bundled-app case, but its
/// documented fallbacks are not: `resource_dir()` failing, or a developer
/// setting `BEAMTALK_ATTACH_LAUNCHER` to a bare relative path (its own doc
/// comment's example, "against a `just dist-liveview` output"), both leave
/// `launcher` relative (e.g. `dist-liveview\bin\bt_attach.bat`). Computing a
/// *relative* release root from that and handing it to `Command::current_dir`
/// would be actively harmful, not just unhelpful: the `.bat` still routes
/// through `cmd.exe` (this module's doc comment, `BatBadBut`), and `cmd.exe`
/// resolves the still-relative launcher path it was told to run *against the
/// new current directory* — turning `dist-liveview\bin\bt_attach.bat` into
/// an attempted `dist-liveview\dist-liveview\bin\bt_attach.bat`, which
/// doesn't exist, so `cmd.exe` exits almost immediately. That early exit is
/// exactly what [`spawn_front_with_port_retry`]'s heuristic treats as a
/// suspected port conflict — silently turning a real (if unusual)
/// dev-workflow launcher path into a confusing `PortsExhausted`, the precise
/// failure mode this same issue's `PortsExhausted` diagnostic improvement
/// exists to prevent. Requiring `is_absolute()` keeps this a no-op (today's
/// behavior: no `current_dir` set, BEAM inherits the Tauri app's cwd) for
/// that dev/fallback shape instead.
#[cfg(windows)]
fn release_root(launcher: &std::path::Path) -> Option<PathBuf> {
    if !launcher.is_absolute() {
        return None;
    }
    let bin_dir = launcher.parent()?;
    let root = bin_dir.parent()?;
    if root.as_os_str().is_empty() {
        None
    } else {
        Some(root.to_path_buf())
    }
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
pub(crate) fn release_tmp_dir(workspace_id: &str, port: u16) -> PathBuf {
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
/// This grace period is paid in full on the **success** path of every spawn
/// attempt (`spawn_front_with_port_retry` polls `try_wait` up to this
/// deadline — see BT-3226 — but a still-running process can't be told apart
/// from one about to crash without waiting out the full window) — so
/// raising it here trades ~3.5s of extra latency on every attach for not
/// silently handing back a front that is already doomed. Correctness was
/// judged more important than shaving that fixed cost. The **conflict**
/// path (an early exit) can return sooner, as soon as the poll loop
/// observes it.
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
/// a conflict — carrying the *last* attempt's real launcher exit status as a
/// diagnostic (BT-3045; see that variant's doc comment for why: this
/// function's retry signal is a heuristic, so a genuine non-conflict launcher
/// failure that happens to exit early on every attempt would otherwise be
/// reported as an opaque attempt count with no clue it wasn't really a port
/// conflict) — or propagates the first non-conflict error (OIDC refusal,
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
        // BT-3226: poll try_wait() up to the deadline instead of a single
        // sleep-then-check. A single check exactly at the deadline can miss
        // a child that has *actually* exited but whose exit status the OS
        // hasn't yet made visible to this process under scheduler
        // contention (e.g. many parallel `cargo test` threads forking
        // processes, or a throttled/virtualized sandbox) — polling gives
        // several chances to observe the exit within the same total window,
        // and returns as soon as it's detected rather than always paying
        // the full grace period on the conflict path. The happy (bound)
        // path is unaffected: a still-running process can't be told apart
        // from one about to crash before the deadline, so that path still
        // pays the full `bind_failure_grace` either way.
        let exit_status = poll_try_wait_until(&mut child, config.bind_failure_grace)?;
        if let Some(exit_status) = exit_status {
            // BT-3045: carry the launcher's actual exit status through, so a
            // caller that exhausts every attempt sees more than a bare count
            // (see BrokerError::PortsExhausted's doc comment) — this is a
            // diagnostic only, not a reclassification: an early exit is still
            // just *treated* as a suspected port conflict per this
            // function's own heuristic doc comment above, whatever the real
            // cause turns out to be.
            Ok(SpawnAttempt::PortTaken(Some(exit_status.to_string())))
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

/// Poll `child.try_wait()` at short intervals until it reports an exit or
/// `grace` elapses, whichever comes first (BT-3226).
///
/// Returns `Ok(Some(status))` as soon as an exit is observed, or
/// `Ok(None)` once the deadline passes with the child still reported
/// running. Checks *before* sleeping on every iteration — including the
/// final one right at the deadline — so this observes strictly more
/// opportunities for the exit to become visible than a single
/// sleep-then-check, without extending the total wait beyond `grace`.
fn poll_try_wait_until(
    child: &mut Child,
    grace: Duration,
) -> io::Result<Option<std::process::ExitStatus>> {
    const POLL_INTERVAL: Duration = Duration::from_millis(15);
    let deadline = Instant::now() + grace;
    loop {
        if let Some(status) = child.try_wait()? {
            return Ok(Some(status));
        }
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return Ok(None);
        }
        std::thread::sleep(POLL_INTERVAL.min(remaining));
    }
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
    fn build_env_sets_the_five_required_vars() {
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("ERL_EPMD_ADDRESS") };
        let config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123", 4567);
        let env = build_env(&config);
        assert_eq!(
            env,
            vec![
                ("PORT".to_string(), "4567".to_string()),
                ("BT_ATTACH_BIND_IP".to_string(), "127.0.0.1".to_string()),
                ("BT_ATTACH_NODE_SUFFIX".to_string(), "abc123".to_string()),
                ("RELEASE_DISTRIBUTION".to_string(), "none".to_string()),
                ("ERL_EPMD_ADDRESS".to_string(), "127.0.0.1".to_string()),
            ]
        );
    }

    #[test]
    fn build_env_respects_an_operator_provided_erl_epmd_address() {
        // BT-3225 review follow-up: a trusted-private-network operator can
        // still override the loopback pin — see build_env's doc comment.
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::set_var("ERL_EPMD_ADDRESS", "10.0.0.5") };
        let config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123", 4567);
        let env = build_env(&config);
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("ERL_EPMD_ADDRESS") };
        assert!(env.contains(&("ERL_EPMD_ADDRESS".to_string(), "10.0.0.5".to_string())));
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
    fn build_launch_command_sets_current_dir_to_the_release_root() {
        // BT-3045: unlike the other Windows tests here, this one needs a
        // launcher path with a real parent chain (bin\bt_attach.bat under a
        // real release root) — `release_root` deliberately no-ops on the
        // bare relative `r"bin\bt_attach.bat"` the other tests use (see its
        // doc comment), so this uses an absolute tempdir path instead, the
        // same shape `desktop/src-tauri/src/launcher.rs::resolve_launcher_path`
        // always produces in production.
        let ws = WindowsTestWorkspaceDir::new(
            "win_launch_cwd",
            Some("bt_attach_win_launch_cwd@localhost"),
            Some("c"),
        );
        let release_root = tempfile::TempDir::new().unwrap();
        let launcher = release_root.path().join("bin").join("bt_attach.bat");
        let config = SpawnConfig::new(launcher, ws.id.clone(), 4567);

        let cmd = build_launch_command(&config).expect("should build a command");

        assert_eq!(
            cmd.get_current_dir(),
            Some(release_root.path()),
            "current_dir should be the launcher's grandparent directory \
             (bin\\bt_attach.bat -> bin -> release root), matching bin/server's \
             own `cd -P -- \"$(dirname \"$0\")/..\"` on Unix"
        );
    }

    #[cfg(windows)]
    #[test]
    fn release_root_is_none_for_a_launcher_with_no_real_parent_chain() {
        // The bare relative paths the other Windows tests in this file use
        // (e.g. r"bin\bt_attach.bat") have no real release root to cd into —
        // release_root must no-op (return None) rather than resolve to an
        // empty/current-directory PathBuf that `Command::current_dir` would
        // then be handed nonsensically.
        assert_eq!(
            release_root(std::path::Path::new(r"bin\bt_attach.bat")),
            None
        );
        assert_eq!(release_root(std::path::Path::new("bt_attach.bat")), None);
    }

    #[cfg(windows)]
    #[test]
    fn release_root_resolves_an_absolute_launcher_paths_grandparent() {
        assert_eq!(
            release_root(std::path::Path::new(
                r"C:\Program Files\Beamtalk\dist-liveview\bin\bt_attach.bat"
            )),
            Some(PathBuf::from(r"C:\Program Files\Beamtalk\dist-liveview"))
        );
    }

    /// Regression test (adversarial-review follow-up): a *relative* launcher
    /// path with a real multi-level parent chain — exactly the shape
    /// `desktop/src-tauri/src/launcher.rs::resolve_launcher_path` falls back
    /// to when `resource_dir()` fails, or a developer sets
    /// `BEAMTALK_ATTACH_LAUNCHER=dist-liveview/bin/bt_attach.bat` (that
    /// module's own doc-comment example) — must still resolve to `None`, not
    /// `Some("dist-liveview")`. Before this guard, `release_root` treated
    /// this as a valid release root and handed it to `Command::current_dir`;
    /// since the launcher path itself stays relative too, `cmd.exe` (which
    /// `.bat` launches always route through) would then resolve it against
    /// the *new* cwd, effectively looking for
    /// `dist-liveview\dist-liveview\bin\bt_attach.bat` — silently breaking
    /// this exact dev workflow with a confusing early `cmd.exe` exit
    /// misclassified as a port conflict. See `release_root`'s doc comment
    /// for the full failure chain.
    #[cfg(windows)]
    #[test]
    fn release_root_is_none_for_a_relative_launcher_even_with_a_real_parent_chain() {
        assert_eq!(
            release_root(std::path::Path::new(r"dist-liveview\bin\bt_attach.bat")),
            None,
            "a relative launcher path must never produce a current_dir — see this \
             function's doc comment for why that's actively harmful, not just unhelpful"
        );
    }

    /// BT-3045 adversarial-review follow-up: `bin/server`'s Unix arm and
    /// `build_launch_command`'s Windows arm are two independent
    /// implementations of "resolve `BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE`,
    /// generate `SECRET_KEY_BASE` if needed, set `PHX_SERVER`" — nothing
    /// previously compared them. This closes the specific gap that mattered
    /// most in practice: it reads `bin/server`'s actual source (not a
    /// duplicated copy of its logic) and asserts each of the four vars below
    /// still appears there as a real shell **assignment** (`"{var}="`), not
    /// merely mentioned in a comment — so deleting/renaming the functional
    /// block while leaving the header comment's prose untouched (which
    /// mentions these same names) still fails this test, unlike a bare
    /// substring search would.
    ///
    /// **What this does *not* catch**, so a reviewer doesn't over-trust it:
    /// `contract_vars` below is a hardcoded list of *today's* var names —
    /// this test cannot discover a wholly new env var `bin/server` starts
    /// setting in the future (a human still has to notice and add it here
    /// too), and it only checks *presence*, not exact semantics (e.g.
    /// `bin/server` only generates `SECRET_KEY_BASE` when unset, while the
    /// Windows arm always regenerates one — both end up ephemeral-per-boot
    /// for this broker's own call shape, since neither platform's broker
    /// code pre-sets that var itself, but this test would not notice if that
    /// stopped being true). It is a removal/rename guard, not a full
    /// behavioral-equivalence proof.
    ///
    /// Windows-only (like the rest of this section): it exercises
    /// `build_launch_command`'s `#[cfg(windows)]` arm, the half of the
    /// contract this crate can actually drift on — `bin/server` itself needs
    /// no equivalent check of its own code.
    #[cfg(windows)]
    #[test]
    fn windows_launch_env_matches_bin_server_contract() {
        // crates/beamtalk-desktop-broker -> crates -> repo root.
        let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let bin_server_path = repo_root
            .join("editors")
            .join("liveview")
            .join("rel")
            .join("overlays")
            .join("bin")
            .join("server");
        let bin_server = std::fs::read_to_string(&bin_server_path).unwrap_or_else(|e| {
            panic!(
                "could not read {}: {e} — has bin/server moved? update this test's path \
                 alongside it",
                bin_server_path.display()
            )
        });

        // Every env var bin/server's workspace-id branch sets/exports for
        // `bin/bt_attach start` to inherit, beyond the four broker-set vars
        // build_env's own test already covers cross-platform
        // (PORT/BT_ATTACH_BIND_IP/BT_ATTACH_NODE_SUFFIX/RELEASE_DISTRIBUTION
        // — bin/server never touches those itself, so they're out of scope
        // here).
        let contract_vars = [
            "BT_WORKSPACE_NODE",
            "BT_WORKSPACE_COOKIE",
            "SECRET_KEY_BASE",
            "PHX_SERVER",
        ];
        for var in contract_vars {
            // A real shell assignment ("VAR=..."), not just the var name
            // appearing somewhere (e.g. bin/server's own header-comment
            // prose mentions BT_WORKSPACE_NODE/BT_WORKSPACE_COOKIE narratively
            // without an "=" — a bare `.contains(var)` would still pass even
            // if the actual assignment lines below the comment were deleted).
            let assignment = format!("{var}=");
            assert!(
                bin_server.contains(&assignment),
                "bin/server no longer assigns {var} (looked for {assignment:?}) — either it \
                 was removed there (update this test's `contract_vars` list) or renamed \
                 (update both this test AND build_launch_command's Windows arm to match)"
            );
        }

        let ws = WindowsTestWorkspaceDir::new(
            "win_launch_contract",
            Some("bt_attach_win_launch_contract@localhost"),
            Some("c"),
        );
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone(), 4567);
        let cmd = build_launch_command(&config).expect("should build a command");
        let env_names: std::collections::HashSet<String> = cmd
            .get_envs()
            .filter_map(|(k, v)| v.map(|_| k.to_string_lossy().into_owned()))
            .collect();

        for var in contract_vars {
            assert!(
                env_names.contains(var),
                "build_launch_command's Windows arm doesn't set {var}, which bin/server sets \
                 on Unix — the two have drifted out of sync"
            );
        }
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

    /// BT-3056: `build_launch_command` on its own never checks the launcher
    /// actually exists (it only cares that the *path* looks right — see
    /// `check_launcher_platform`) — that check lives in `spawn_front`
    /// instead, deliberately, so it doesn't break the many
    /// `build_launch_command_*` tests in this file that pass synthetic
    /// non-existent launcher paths purely for `Command` introspection. This
    /// exercises `spawn_front` end-to-end with a real (never-written)
    /// absolute launcher path and a fully valid workspace, so the *only*
    /// thing standing between it and a real `Command::spawn` call is the
    /// missing file.
    ///
    /// Holds `ENV_LOCK` (adversarial-review follow-up): `spawn_front` reads
    /// `BT_OIDC_*` process-globals via `oidc_configured`, which
    /// `oidc_guard::tests` mutates concurrently in this same test binary —
    /// without the lock this could spuriously observe `OidcConfigured`
    /// instead of reaching the launcher-existence check this test exists to
    /// exercise, matching every other `spawn_front`-calling test in this
    /// file (e.g. `spawn_front_refuses_when_oidc_env_present`).
    #[cfg(windows)]
    #[test]
    fn spawn_front_errors_with_launcher_not_found_when_file_is_missing() {
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        let ws = WindowsTestWorkspaceDir::new(
            "win_launcher_missing",
            Some("bt_attach_win_launcher_missing@localhost"),
            Some("testcookie"),
        );
        let tmp = tempfile::TempDir::new().unwrap();
        // A well-formed, absolute .bat path — passes check_launcher_platform
        // — that is simply never written to disk.
        let launcher = tmp.path().join("bin").join("bt_attach.bat");
        let mut config = SpawnConfig::new(launcher.clone(), ws.id.clone(), 4567);
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_OIDC_ISSUER") };

        let result = spawn_front(&config);

        assert!(
            matches!(result, Err(BrokerError::LauncherNotFound(ref p)) if *p == launcher.display().to_string()),
            "expected LauncherNotFound({launcher:?}), got {result:?}"
        );
    }

    /// BT-3060: the Windows spawn path must hard-fail — not silently guess a
    /// node name via [`crate::discovery::default_node_name`] — when a
    /// workspace's `metadata.json` has no `node_name` yet (created but never
    /// started), matching `bin/server`'s Unix fail-fast behavior.
    #[cfg(windows)]
    #[test]
    fn build_launch_command_errors_when_metadata_lacks_node_name() {
        let ws = WindowsTestWorkspaceDir::new("win_launch_no_node_name", None, Some("c"));
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone(), 4567);

        let result = build_launch_command(&config);

        assert!(
            matches!(result, Err(BrokerError::MissingNodeName(ref w)) if w == &ws.id),
            "expected MissingNodeName({}), got {result:?}",
            ws.id
        );
    }

    #[cfg(windows)]
    #[test]
    fn build_launch_command_errors_when_cookie_file_missing() {
        let ws = WindowsTestWorkspaceDir::new(
            "win_launch_no_cookie",
            Some("bt_attach_win_launch_no_cookie@localhost"),
            None,
        );
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

    /// Exec `launcher` once, synchronously, and return its exit status —
    /// warms the OS's one-time "cold exec" cost for a brand-new script file
    /// (BT-3241, BT-3250) so a subsequent *timed* exec of the same file
    /// isn't racing that one-time cost against a tight grace window. Only
    /// suitable for a launcher that's expected to exit on its own quickly;
    /// see `spawn_front_with_port_retry_captures_front_stdout_and_stderr_to_attach_log`
    /// for the spawn-then-kill variant this doesn't cover (a launcher that
    /// never exits by itself).
    #[cfg(unix)]
    fn warm_up_launcher_exec(launcher: &std::path::Path) -> std::process::ExitStatus {
        Command::new(launcher)
            .status()
            .expect("warm-up exec of the launcher should run")
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

    /// BT-3056: see the Windows counterpart
    /// (`spawn_front_errors_with_launcher_not_found_when_file_is_missing`)
    /// for the full rationale, including why this holds `ENV_LOCK` — this
    /// checks the same `spawn_front`-level existence guard on the Unix arm,
    /// with a well-formed (no `.bat`/`.cmd`/`.exe` extension) but
    /// never-written launcher path.
    #[cfg(unix)]
    #[test]
    fn spawn_front_errors_with_launcher_not_found_when_file_is_missing() {
        let _guard = crate::test_support::ENV_LOCK.lock().unwrap();
        let ws = TestWorkspaceDir::new("launcher_missing");
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher = tmp.path().join("server"); // deliberately never written
        let mut config = SpawnConfig::new(launcher.clone(), ws.id.clone(), 4567);
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_OIDC_ISSUER") };

        let result = spawn_front(&config);

        assert!(
            matches!(result, Err(BrokerError::LauncherNotFound(ref p)) if *p == launcher.display().to_string()),
            "expected LauncherNotFound({launcher:?}), got {result:?}"
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
    fn spawn_front_with_port_retry_captures_front_stdout_and_stderr_to_attach_log() {
        // BT-3225: the spawned front's stdout/stderr must land in
        // `~/.beamtalk/workspaces/<id>/attach.log`, not the packaged app's
        // inherited (and, in a release build, nonexistent) stdio.
        let ws = TestWorkspaceDir::new("attach_log_capture");
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher = write_launcher_script(
            tmp.path(),
            "server",
            "echo stdout-marker; echo stderr-marker 1>&2; sleep 5",
        );

        // BT-3250: pay this exact file's first-exec tax here, untimed,
        // instead of racing it against the fixed 2s `wait_for_file_contents`
        // timeout below. Same root cause as BT-3241 (see the sibling
        // `..._recovers_after_transient_conflicts` test): the *first* exec of
        // a brand-new script file can take 200ms+ on a loaded macOS sandbox —
        // and under full-crate parallel `cargo test` scheduler contention,
        // occasionally much more — while every *subsequent* exec of that same
        // already-warm file consistently lands under 20ms. Unlike the
        // `exit 1`-style launchers in the sibling tests, this script never
        // exits on its own (it ends in `sleep 5`), so the warm-up here can't
        // wait for a natural exit — it only needs to force the one-time
        // cold-exec cost to happen now, so the timed spawn below execs an
        // already-warm file and its `echo`s land in `attach.log` well within
        // the 2s window. Stdio is discarded rather than left inherited, so
        // this warm-up run's own `echo` output doesn't leak into the test
        // process's stdout/stderr.
        let mut warmup = Command::new(&launcher)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("warm-up exec of the launcher should start");
        let _ = warmup.kill();
        let _ = warmup.wait();

        let mut config = SpawnAttemptConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC
        config.bind_failure_grace = Duration::from_millis(100);

        let (mut child, _port) =
            spawn_front_with_port_retry(&config).expect("should spawn and bind on first try");

        let log_path = beamtalk_workspace::workspace_dir(&ws.id)
            .unwrap()
            .join("attach.log");
        let contents = wait_for_file_contents(&log_path, Duration::from_secs(2));

        let _ = child.kill();
        let _ = child.wait();

        assert!(
            contents.contains("stdout-marker"),
            "expected attach.log to contain the launcher's stdout, got: {contents:?}"
        );
        assert!(
            contents.contains("stderr-marker"),
            "expected attach.log to contain the launcher's stderr, got: {contents:?}"
        );

        // attach.log can carry the same class of sensitive runtime detail
        // workspace.log does — must be owner-only, not the world-readable
        // default (BT-3225 review follow-up).
        let mode = std::os::unix::fs::PermissionsExt::mode(
            &std::fs::metadata(&log_path).unwrap().permissions(),
        ) & 0o777;
        assert_eq!(
            mode, 0o600,
            "attach.log should be created 0600, got {mode:o}"
        );
    }

    /// Poll `path` until it exists and is non-empty, or `timeout` elapses —
    /// the launcher script's `echo`s race this test process reading the file
    /// it just told the OS to create.
    #[cfg(unix)]
    fn wait_for_file_contents(path: &std::path::Path, timeout: Duration) -> String {
        let deadline = std::time::Instant::now() + timeout;
        loop {
            if let Ok(contents) = std::fs::read_to_string(path) {
                if !contents.is_empty() {
                    return contents;
                }
            }
            if std::time::Instant::now() >= deadline {
                return std::fs::read_to_string(path).unwrap_or_default();
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    /// BT-3226: `poll_try_wait_until` should observe an exit that happens
    /// well before the deadline without waiting out the full grace period —
    /// this is what lets the conflict path in
    /// `spawn_front_with_port_retry` return early instead of always paying
    /// the full `bind_failure_grace`.
    #[cfg(unix)]
    #[test]
    fn poll_try_wait_until_returns_promptly_once_the_child_exits() {
        let mut child = Command::new("/bin/sh")
            .arg("-c")
            .arg("exit 7")
            .spawn()
            .unwrap();

        let started = Instant::now();
        let status = poll_try_wait_until(&mut child, Duration::from_secs(5))
            .unwrap()
            .expect("child should have exited");
        let elapsed = started.elapsed();

        assert_eq!(status.code(), Some(7));
        assert!(
            elapsed < Duration::from_secs(1),
            "expected an early return well under the 5s grace period, took {elapsed:?}"
        );
    }

    /// BT-3226: a still-running child must not be misreported as exited —
    /// `poll_try_wait_until` should keep polling right up to (and
    /// including) the deadline and only then report `None`.
    #[cfg(unix)]
    #[test]
    fn poll_try_wait_until_returns_none_when_the_child_outlives_the_deadline() {
        let mut child = Command::new("/bin/sh")
            .arg("-c")
            .arg("sleep 5")
            .spawn()
            .unwrap();

        let started = Instant::now();
        let status = poll_try_wait_until(&mut child, Duration::from_millis(100)).unwrap();
        let elapsed = started.elapsed();

        assert_eq!(status, None, "child is still running, should report None");
        assert!(
            elapsed >= Duration::from_millis(100),
            "should have waited out the full grace period, took {elapsed:?}"
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

        // BT-3250: pay this exact file's first-exec tax here, untimed,
        // instead of racing it against `bind_failure_grace` below — same
        // fix as BT-3241's `..._recovers_after_transient_conflicts` sibling
        // test (see its comment for the full root-cause rationale). The
        // *first* exec of a brand-new script file can take 200ms+ on a
        // loaded macOS sandbox, comfortably exceeding this test's 250ms
        // grace and misclassifying attempt 1 as `Bound`; every *subsequent*
        // exec of the same already-warm file consistently lands under
        // 20ms. Running the launcher once here, synchronously and with no
        // deadline, forces that one-time tax to happen before the timed
        // retry logic below starts, so all three *measured* attempts exec
        // an already-warm file.
        let warmup_status = warm_up_launcher_exec(&launcher);
        assert_eq!(
            warmup_status.code(),
            Some(1),
            "warm-up exec should hit the script's own `exit 1` branch, got {warmup_status:?}"
        );

        let mut config = SpawnAttemptConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml");
        // BT-3226: the poll loop alone doesn't stabilize this in a sandboxed
        // CI/dev environment — the *first* exec of a brand-new tempdir path
        // (a fresh one every run, from `tempfile::TempDir::new()`) can pay a
        // cold-exec tax north of 50ms here, independent of how tightly
        // `try_wait` is polled within the window. 250ms (the spec's allowed
        // ceiling) comfortably covers that without masking a real hang.
        // BT-3250: the warm-up above removes the *first-exec* tax race; this
        // grace still covers ordinary scheduler jitter across all 3 timed
        // attempts, same as it always has.
        config.bind_failure_grace = Duration::from_millis(250);
        config.max_port_attempts = 3;

        let result = spawn_front_with_port_retry(&config);
        assert!(
            matches!(
                result,
                Err(BrokerError::PortsExhausted {
                    attempts: 3,
                    last_exit_status: Some(_)
                })
            ),
            "expected PortsExhausted with attempts=3 and a diagnostic exit status \
             (BT-3045 — the launcher's real exit status should be surfaced, not just \
             a bare count), got {result:?}"
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

        // BT-3241: pay this exact file's first-exec tax here, untimed,
        // instead of racing it against `bind_failure_grace` below. Root
        // cause (confirmed by instrumenting `spawn_front_with_port_retry`
        // locally): the *first* exec of a brand-new script file — even one
        // as small as this one — can take 200ms+ on a loaded macOS sandbox,
        // while every *subsequent* exec of that same already-warm file
        // consistently lands under 20ms. BT-3226's poll loop only shortens
        // *detection* latency once the process actually exits; it can't
        // shorten the process's own wall-clock time to reach `exit 1`, so
        // when that first-exec tax alone exceeds the 250ms grace, attempt 1
        // is still running (not yet even at the `cat`/arithmetic/`echo`
        // lines) when the deadline fires and gets misclassified as `Bound`
        // — the test then reads a `0`-or-`1`-attempts counter instead of the
        // expected `3`. Widening the grace further would only raise the bar
        // the tax has to clear, not remove the race (forbidden by this
        // issue's acceptance criteria) — running the launcher once here,
        // synchronously and with no deadline, forces that one-time tax to
        // happen before the timed retry logic below ever starts, so all
        // three *measured* attempts exec an already-warm file.
        let warmup_status = warm_up_launcher_exec(&launcher);
        assert!(
            warmup_status.success() || warmup_status.code() == Some(1),
            "warm-up exec should either succeed or hit the script's own \
             `exit 1` branch, got {warmup_status:?}"
        );
        // The warm-up run above incremented the counter file (it's the same
        // script, same file, counting the same way) — reset it so the timed
        // retry logic below starts counting from a clean `0`.
        std::fs::write(&counter_path, b"0").unwrap();

        let mut config = SpawnAttemptConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml");
        // BT-3226: see the sibling `..._always_exits` test above for why
        // this needed raising past the poll loop alone — same cold-exec tax
        // applies to this test's first attempt at a brand-new tempdir path.
        // BT-3241: the warm-up above removes the *first-exec* tax race; this
        // grace still covers ordinary scheduler jitter across all 3 timed
        // attempts, same as it always has.
        config.bind_failure_grace = Duration::from_millis(250);
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
            matches!(
                result,
                Err(BrokerError::PortsExhausted {
                    attempts: 3,
                    last_exit_status: Some(_)
                })
            ),
            "expected PortsExhausted with attempts=3 and a diagnostic exit status \
             (BT-3045 — the launcher's real exit status should be surfaced, not just \
             a bare count), got {result:?}"
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
