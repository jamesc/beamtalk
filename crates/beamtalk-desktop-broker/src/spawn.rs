// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Spawn a per-workspace front (ADR 0097 Decision / Broker §2).
//!
//! Concretely, on Unix: `PORT=0 BT_ATTACH_PORT_FILE=<file>
//! BT_ATTACH_BIND_IP=127.0.0.1 BT_ATTACH_NODE_SUFFIX=<id>
//! RELEASE_DISTRIBUTION=none bin/server <id>`. `PORT=0` makes the front's
//! own listener pick a free port as part of its bind, and the front reports
//! that port back through `<file>` — see [`crate::port`] for why the broker
//! no longer chooses the port itself (BT-4398).
//!
//! `bin/server <id>` (`editors/liveview/rel/overlays/bin/server`) already
//! re-resolves `BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE` from
//! `~/.beamtalk/workspaces/<id>/metadata.json` + the sibling `cookie` file
//! and generates its own ephemeral `SECRET_KEY_BASE` when given a workspace
//! id — the ADR's "Local-only posture" section calls that ephemeral-per-boot
//! key *deliberate* (no durable session worth preserving on the
//! unauthenticated localhost lane), so this broker does not set it itself on
//! Unix. The broker's job there is exactly the env vars above, plus
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
//! **Windows has no `bin/server`** — `bin/server` is a POSIX `sh`
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
//! `bin/server`'s shape:** `bin/server`
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
//! missing is a **test** tying the two
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
//! **Windows gap, documented rather than silently worked around:** the
//! epmd/OS-process story is not fully verified on Windows. `detach` (below)
//! uses `CREATE_NEW_PROCESS_GROUP` as the Windows
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
//! mechanism, and for why the child is spawned `CREATE_SUSPENDED` and only
//! resumed once it is inside the job (so `erl.exe` can never be created
//! before the assignment that would cover it — BT-3880). This has not been
//! exercised against a real Windows boot — no Windows sandbox was available
//! to develop or test it against — but unlike the rest of this paragraph's
//! original framing, it is not a *guess* at what Windows does; the
//! job-object mechanism works the same way regardless of how many hops sit
//! between `cmd.exe` and `erl.exe`, and the `.bat`-grandchild test at the
//! bottom of this module exercises it on CI. [`crate::reap`]'s PID-file
//! sweep remains the fallback net for any process this mechanism still
//! misses (e.g. a broker that dies between `spawn()` and the assignment).

use std::fs::{File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::Duration;

use crate::error::{BrokerError, Result};
use crate::oidc_guard::{default_ide_config_path, oidc_configured};
use crate::port;
use crate::sname::attach_node_suffix;

/// Everything needed to spawn one front.
#[derive(Debug, Clone)]
pub struct SpawnConfig {
    /// Path to the launcher executable — `bin/server` on Unix, invoked as
    /// `bin/server <id>` (it re-resolves everything else itself). On Windows
    /// (ADR 0097 Implementation §5b), this is instead the release's
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
    /// Identifies this one spawn: names the front's port file
    /// ([`crate::port::port_file_path`]) and, on Windows, its per-front
    /// `RELEASE_TMP` directory ([`release_tmp_dir`]). [`SpawnConfig::new`]
    /// generates a fresh one ([`crate::port::new_instance_id`]).
    pub instance_id: String,
    /// Loopback bind address (ADR 0097 "Local-only posture" — always
    /// `127.0.0.1`/`::1`, never `0.0.0.0`).
    pub bind_ip: String,
    /// `ide.toml` path to check for OIDC config before spawning. Defaults to
    /// [`default_ide_config_path`] via [`SpawnConfig::new`].
    pub ide_toml_path: PathBuf,
}

impl SpawnConfig {
    /// Build a config with a fresh instance id, the standard loopback bind
    /// and the default `ide.toml` path.
    #[must_use]
    pub fn new(launcher: PathBuf, workspace_id: impl Into<String>) -> Self {
        Self {
            launcher,
            workspace_id: workspace_id.into(),
            instance_id: port::new_instance_id(),
            bind_ip: "127.0.0.1".to_string(),
            ide_toml_path: default_ide_config_path(),
        }
    }
}

/// The env vars this broker sets on **every** platform, in a stable order
/// (for deterministic logging/tests — `Command::envs` doesn't care about
/// order). `port_file` is where the front reports the port it bound (see
/// [`crate::port`]). On Windows, [`build_launch_command`] sets five more
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
pub fn build_env(config: &SpawnConfig, port_file: &Path) -> Vec<(String, String)> {
    let epmd_address = beamtalk_workspace::resolve_epmd_address();
    vec![
        ("PORT".to_string(), "0".to_string()),
        (
            port::PORT_FILE_ENV.to_string(),
            port_file.display().to_string(),
        ),
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
    instance_id: String,
    port_file: PathBuf,
    #[cfg(windows)]
    #[allow(dead_code)] // held for its Drop side effect only, never read
    job: crate::winjob::JobHandle,
}

impl SpawnedFront {
    /// The [`SpawnConfig::instance_id`] this front was spawned with — persist
    /// it in its [`crate::reap::FrontRecord`] so cleanup can find its
    /// per-front `RELEASE_TMP` directory on Windows.
    #[must_use]
    pub fn instance_id(&self) -> &str {
        &self.instance_id
    }

    /// Where this front will report its bound port (see [`crate::port`]).
    #[must_use]
    pub fn port_file(&self) -> &Path {
        &self.port_file
    }
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
/// comment for why plain `Child::kill()` cannot reach `erl.exe` there. The
/// child is spawned `CREATE_SUSPENDED` and released only after that
/// assignment ([`crate::winjob::resume_initial_thread`]), so it cannot have
/// spawned `erl.exe` outside the job first (BT-3880); by the time this
/// returns `Ok`, the child is running.
///
/// # Errors
///
/// Returns [`BrokerError::OidcConfigured`] or [`BrokerError::UnknownWorkspace`]
/// per the refusal conditions above, or [`BrokerError::Io`] if the launcher
/// process fails to spawn or (Windows only) if assigning it to the job
/// object or resuming it afterwards fails. [`build_launch_command`] can
/// additionally return
/// [`BrokerError::LauncherPlatformMismatch`] (`config.launcher`
/// doesn't look like a real entry point for the current platform) before any
/// process is spawned. On Windows, its workspace lookups and per-front
/// `RELEASE_TMP` setup can additionally propagate [`BrokerError::MissingCookie`]
/// (no `cookie` file), [`BrokerError::Json`] (malformed `metadata.json`),
/// [`BrokerError::Workspace`] (a `beamtalk-workspace` failure), or
/// [`BrokerError::Io`] (failed to create the `RELEASE_TMP` directory) before
/// any process is spawned. Also returns [`BrokerError::LauncherNotFound`]
/// if `config.launcher` looks right for this platform but no file
/// actually exists there — the common unpackaged-dev-build failure mode,
/// checked explicitly rather than left to surface as a generic `Io` error
/// out of `Command::spawn` below. Resolving the front's port file
/// ([`crate::port::port_file_path`]) can return [`BrokerError::Workspace`]
/// (no home directory).
///
/// The front reports the port it binds through [`SpawnedFront::port_file`];
/// [`launch_front`] waits for it — call that rather than this unless you
/// handle the port report yourself.
pub fn spawn_front(config: &SpawnConfig) -> Result<SpawnedFront> {
    if let Some(source) = oidc_configured(&config.ide_toml_path) {
        return Err(BrokerError::OidcConfigured(source.to_string()));
    }

    if !beamtalk_workspace::workspace_dir(&config.workspace_id)
        .is_ok_and(|d| d.join("metadata.json").exists())
    {
        return Err(BrokerError::UnknownWorkspace(config.workspace_id.clone()));
    }

    let port_file = port::port_file_path(&config.workspace_id, &config.instance_id)?;
    // Fresh name per spawn, so nothing should be there — but a leftover from
    // a crashed broker must never be mistaken for this front's report.
    port::remove_port_file(&port_file);

    let mut cmd = build_launch_command(config, &port_file)?;

    // Checked here, immediately after
    // `build_launch_command` succeeds, rather than inside it — that function
    // has its own unit tests exercising synthetic non-existent launcher
    // paths (e.g. `bin\bt_attach.bat`, `bin/server`) purely for `Command`
    // introspection (program/args/env), which a check inside
    // `build_launch_command` itself would break. `config.launcher` and
    // `cmd`'s resolved program are always the same path on both platforms
    // (see `build_launch_command`'s two arms), so checking the former here
    // is equivalent to checking the latter.
    if !config.launcher.is_file() {
        // On Windows,
        // `build_launch_command` above already created this attempt's
        // per-front `RELEASE_TMP` directory (`std::fs::create_dir_all`) —
        // returning here without removing it would leave that directory
        // behind forever: no `FrontRecord` is ever written for a
        // `spawn_front` call that fails this early, so `remove_record`'s own
        // `RELEASE_TMP` cleanup hook never runs for it.
        // Same cleanup the job-object-assign failure path below already
        // does for the identical reason, just inlined here too.
        #[cfg(windows)]
        {
            let _ =
                std::fs::remove_dir_all(release_tmp_dir(&config.workspace_id, &config.instance_id));
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
        // `child` is still suspended here (`detach` passed
        // `CREATE_SUSPENDED`), so assigning it now covers every descendant
        // it will ever create — there is no window in which `cmd.exe` could
        // already have launched `erl.exe` outside the job (BT-3880; see
        // `crate::winjob`'s doc). Only once it is inside the job is it
        // released to run. Both steps are checked and surface a failure
        // rather than silently returning a front this job object does not
        // actually protect (or one that would sit suspended forever).
        // `cmd.spawn()` above already succeeded, so a failure past this
        // point must kill `child` itself before propagating — otherwise the
        // job object we failed to attach never exists to reap it, and
        // `crate::reap`'s PID-file sweep can't catch it either (a PID file
        // is only written after `spawn_front` returns `Ok`), leaving a live,
        // untracked `cmd.exe`/`erl.exe` tree behind.
        if let Err(e) = job
            .assign(&child)
            .and_then(|()| crate::winjob::resume_initial_thread(&child))
        {
            kill_orphaned_child(&mut child);
            // Best-effort (low
            // severity): `build_launch_command` above already created this
            // attempt's RELEASE_TMP directory. At this point in the boot
            // sequence the release has likely not written secrets into it
            // yet, but there's no reason to leave even an empty stray
            // directory behind — same cleanup `remove_record` does on the
            // success path, just inlined here since this failure never
            // reaches a saved `FrontRecord` for `remove_record` to key off.
            let _ =
                std::fs::remove_dir_all(release_tmp_dir(&config.workspace_id, &config.instance_id));
            return Err(e.into());
        }
        Ok(SpawnedFront {
            child,
            instance_id: config.instance_id.clone(),
            port_file,
            job,
        })
    }
    #[cfg(not(windows))]
    {
        Ok(SpawnedFront {
            child,
            instance_id: config.instance_id.clone(),
            port_file,
        })
    }
}

/// Redirect `cmd`'s stdout/stderr to `~/.beamtalk/workspaces/<id>/attach.log`
/// (append mode, accumulating across restarts — mirrors `workspace.log`'s own
/// convention on the workspace-node side) instead of the default inherited
/// stdio. Inherited stdio is the packaged Tauri app's own — i.e. nowhere, in
/// a release build (`windows_subsystem = "windows"` on Windows; macOS/Linux
/// app bundles have no attached terminal either) — so without this, a
/// spawned front's crash or an Elixir-side exception is invisible short of
/// running the release binary manually from a terminal.
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
/// job created before `spawn`, or [`crate::winjob::resume_initial_thread`]
/// failed to release it afterwards — see the call site in [`spawn_front`].
/// `TerminateProcess` (what `Child::kill` is on Windows) works on a
/// still-suspended process too, so both cases are handled the same way.
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
/// shared call site — this arm can now fail too (see
/// [`check_launcher_platform`]'s guard), not only the Windows one (a missing
/// cookie file, or a `read_node_name` failure).
///
/// # Errors
///
/// Returns [`BrokerError::LauncherPlatformMismatch`] if `config.launcher`
/// doesn't look like a Unix entry point.
#[cfg(unix)]
fn build_launch_command(config: &SpawnConfig, port_file: &Path) -> Result<Command> {
    check_launcher_platform(&config.launcher)?;
    let mut cmd = Command::new(&config.launcher);
    cmd.arg(&config.workspace_id);
    for (key, value) in build_env(config, port_file) {
        cmd.env(key, value);
    }
    Ok(cmd)
}

/// Guard against [`SpawnConfig::launcher`] (or its `BEAMTALK_ATTACH_LAUNCHER`
/// override — see `desktop/src-tauri/src/launcher.rs`) pointing at the wrong
/// platform's entry point: a Unix
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

/// Windows counterpart of the Unix `build_launch_command` above —
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
/// field yet (a workspace created but never started, matching
/// `bin/server`'s Unix fail-fast behavior instead of guessing) — or
/// [`BrokerError::Io`] if the per-front [`release_tmp_dir`] can't be created.
#[cfg(windows)]
fn build_launch_command(config: &SpawnConfig, port_file: &Path) -> Result<Command> {
    check_launcher_platform(&config.launcher)?;
    let node_name = crate::discovery::read_node_name(&config.workspace_id)?;
    let cookie = beamtalk_workspace::read_cookie_file(&config.workspace_id)?
        .ok_or_else(|| BrokerError::MissingCookie(config.workspace_id.clone()))?;

    let mut cmd = Command::new(&config.launcher);
    cmd.arg("start");
    for (key, value) in build_env(config, port_file) {
        cmd.env(key, value);
    }
    cmd.env("BT_WORKSPACE_NODE", node_name);
    cmd.env("BT_WORKSPACE_COOKIE", cookie);
    cmd.env("SECRET_KEY_BASE", generate_secret_key_base());
    cmd.env("PHX_SERVER", "true");

    // `bin/server`'s Unix arm always
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

    // A bundled MSI/NSIS install lands
    // under `C:\Program Files\Beamtalk\...`, which a standard (non-admin)
    // Windows user cannot write to. Mix releases resolve `RELEASE_TMP`
    // (default `$RELEASE_ROOT/tmp`) and write the boot's resolved
    // `sys.config` there on every boot — left at the default, first attach
    // on a real install would fail with a permission error before the VM
    // even finishes booting. Point it at a per-user-writable, per-front
    // directory instead.
    let release_tmp = release_tmp_dir(&config.workspace_id, &config.instance_id);
    std::fs::create_dir_all(&release_tmp)?;
    cmd.env("RELEASE_TMP", release_tmp);

    Ok(cmd)
}

/// The release root a Windows launcher path implies — `bin\bt_attach.bat`'s
/// grandparent directory, mirroring `bin/server`'s own `dirname -- "$0")/..`
/// (this module's doc comment).
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
/// doesn't exist, so `cmd.exe` exits almost immediately — surfacing as a
/// confusing [`BrokerError::FrontExitedBeforeReportingPort`] for what is
/// really a real (if unusual) dev-workflow launcher path. Requiring
/// `is_absolute()` keeps this a no-op (today's
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
/// entry point — see the `#[cfg(unix)]` sibling above for the full
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

/// Per-front `RELEASE_TMP` directory — see [`build_launch_command`]'s
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
/// Scoped by workspace id *and* spawn instance id (not just workspace id) so
/// two fronts for the same workspace — e.g. a stale one mid-reap while a new
/// one spawns — never race on the same `sys.config`/boot files. Keyed by
/// instance rather than port because the port isn't known until the front
/// has booted and reported it (see [`crate::port`]), long after
/// `RELEASE_TMP` must be set.
#[cfg(windows)]
pub(crate) fn release_tmp_dir(workspace_id: &str, instance_id: &str) -> PathBuf {
    dirs::cache_dir()
        .unwrap_or_else(std::env::temp_dir)
        .join("beamtalk-release-tmp")
        .join(format!("{workspace_id}-{instance_id}"))
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

/// Config for [`launch_front`] — the fields [`SpawnConfig`] needs, minus the
/// per-spawn instance id (generated per launch), plus how long to wait for
/// the front to report its port.
#[derive(Debug, Clone)]
pub struct LaunchConfig {
    pub launcher: PathBuf,
    pub workspace_id: String,
    pub bind_ip: String,
    pub ide_toml_path: PathBuf,
    /// Upper bound on waiting for a still-running front to report its bound
    /// port — see [`crate::port::wait_for_reported_port`].
    pub port_report_timeout: Duration,
}

impl LaunchConfig {
    /// Build a config with the standard loopback bind, default `ide.toml`
    /// path and [`port::DEFAULT_PORT_REPORT_TIMEOUT`].
    #[must_use]
    pub fn new(launcher: PathBuf, workspace_id: impl Into<String>) -> Self {
        Self {
            launcher,
            workspace_id: workspace_id.into(),
            bind_ip: "127.0.0.1".to_string(),
            ide_toml_path: default_ide_config_path(),
            port_report_timeout: port::DEFAULT_PORT_REPORT_TIMEOUT,
        }
    }
}

/// Spawn a front and wait for it to report the HTTP port it bound (ADR 0097
/// Broker §2): [`spawn_front`] with `PORT=0`, then
/// [`crate::port::wait_for_reported_port`]. Returns the running front and
/// the port to probe — the port our front's own listener holds, so there is
/// no check-then-use race to retry around (see [`crate::port`]'s module
/// docs, BT-4398).
///
/// On failure after the spawn, the front is killed and reaped (and on
/// Windows its `RELEASE_TMP` directory removed) before returning, so a
/// caller never has a process or secret-bearing directory to clean up from
/// an `Err`.
///
/// # Errors
///
/// Propagates [`spawn_front`]'s errors (OIDC refusal, unknown workspace,
/// launcher problems, spawn I/O), and
/// [`crate::port::wait_for_reported_port`]'s:
/// [`BrokerError::FrontExitedBeforeReportingPort`] if the front dies during
/// boot, [`BrokerError::PortReportTimedOut`] if it never reports, or
/// [`BrokerError::InvalidPortFile`].
pub fn launch_front(config: &LaunchConfig) -> Result<(SpawnedFront, u16)> {
    let spawn_config = SpawnConfig {
        launcher: config.launcher.clone(),
        workspace_id: config.workspace_id.clone(),
        instance_id: port::new_instance_id(),
        bind_ip: config.bind_ip.clone(),
        ide_toml_path: config.ide_toml_path.clone(),
    };
    let mut front = spawn_front(&spawn_config)?;
    let port_file = front.port_file().to_path_buf();
    match port::wait_for_reported_port(&mut front, &port_file, config.port_report_timeout) {
        Ok(port) => Ok((front, port)),
        Err(e) => {
            let _ = front.kill();
            let _ = front.wait();
            #[cfg(windows)]
            {
                // Closes the job (killing the whole tree) before removing the
                // directory erl.exe may still hold open.
                drop(front);
                let _ = std::fs::remove_dir_all(release_tmp_dir(
                    &spawn_config.workspace_id,
                    &spawn_config.instance_id,
                ));
            }
            Err(e)
        }
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
    // `bin\bt_attach.bat` can only
    // run via a console-subsystem wrapper (`cmd.exe` — see this module's
    // doc comment), which without this flag pops a visible console window
    // per front, behind the desktop app's GUI, that a user could close and
    // kill their workspace out from under them. `CREATE_NO_WINDOW`
    // suppresses that; it does not affect `erl.exe`'s own I/O (the front
    // doesn't attach a console either way — `PHX_SERVER=true` boot has
    // nothing to print to one).
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;
    // `CREATE_SUSPENDED` is not about detaching: it makes the child start
    // frozen so `spawn_front` can assign it to its job object before it
    // executes anything (and so before `cmd.exe` can launch `erl.exe`
    // outside that job — BT-3880). `spawn_front` resumes it via
    // `crate::winjob::resume_initial_thread` straight after the assignment.
    // It lives here only because `creation_flags` replaces rather than
    // ORs, so every flag the front needs must be set in one call.
    cmd.creation_flags(
        CREATE_NEW_PROCESS_GROUP | CREATE_NO_WINDOW | crate::winjob::CREATE_SUSPENDED,
    );
}

#[cfg(not(any(unix, windows)))]
fn detach(_cmd: &mut Command) {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_env_sets_the_required_vars() {
        let _guard = crate::test_support::lock_env();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("ERL_EPMD_ADDRESS") };
        let config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123");
        let env = build_env(&config, Path::new("front.port"));
        assert_eq!(
            env,
            vec![
                ("PORT".to_string(), "0".to_string()),
                ("BT_ATTACH_PORT_FILE".to_string(), "front.port".to_string()),
                ("BT_ATTACH_BIND_IP".to_string(), "127.0.0.1".to_string()),
                ("BT_ATTACH_NODE_SUFFIX".to_string(), "abc123".to_string()),
                ("RELEASE_DISTRIBUTION".to_string(), "none".to_string()),
                ("ERL_EPMD_ADDRESS".to_string(), "127.0.0.1".to_string()),
            ]
        );
    }

    #[test]
    fn build_env_respects_an_operator_provided_erl_epmd_address() {
        // A trusted-private-network operator can
        // still override the loopback pin — see build_env's doc comment.
        let _guard = crate::test_support::lock_env();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::set_var("ERL_EPMD_ADDRESS", "10.0.0.5") };
        let config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123");
        let env = build_env(&config, Path::new("front.port"));
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("ERL_EPMD_ADDRESS") };
        assert!(env.contains(&("ERL_EPMD_ADDRESS".to_string(), "10.0.0.5".to_string())));
    }

    #[test]
    fn build_env_uses_configured_bind_ip() {
        let mut config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123");
        "::1".clone_into(&mut config.bind_ip);
        let env = build_env(&config, Path::new("front.port"));
        assert!(env.contains(&("BT_ATTACH_BIND_IP".to_string(), "::1".to_string())));
    }

    #[test]
    fn spawn_front_refuses_when_oidc_env_present() {
        let _guard = crate::test_support::lock_env();
        let tmp = tempfile::TempDir::new().unwrap();
        // SAFETY: guarded by ENV_LOCK above — serialized against every other
        // test in this crate that touches BT_OIDC_* env vars.
        unsafe { std::env::set_var("BT_OIDC_ISSUER", "https://idp.example.com") };

        let mut config = SpawnConfig::new(PathBuf::from("/bin/true"), "abc123");
        config.ide_toml_path = tmp.path().join("ide.toml");
        let result = spawn_front(&config);

        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_OIDC_ISSUER") };

        assert!(matches!(result, Err(BrokerError::OidcConfigured(_))));
    }

    #[test]
    fn spawn_front_refuses_unknown_workspace_when_oidc_absent() {
        let _guard = crate::test_support::lock_env();
        let tmp = tempfile::TempDir::new().unwrap();
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_OIDC_ISSUER") };

        let mut config = SpawnConfig::new(
            PathBuf::from("/bin/true"),
            "__nonexistent_workspace_broker_test__",
        );
        config.ide_toml_path = tmp.path().join("ide.toml");
        let result = spawn_front(&config);
        assert!(matches!(result, Err(BrokerError::UnknownWorkspace(_))));
    }

    // ── Windows launch command ──────────────────────────────────────────
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
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone());

        let cmd =
            build_launch_command(&config, Path::new("front.port")).expect("should build a command");

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
        assert_eq!(envs.get("PORT").map(String::as_str), Some("0"));
        assert_eq!(
            envs.get("BT_ATTACH_PORT_FILE").map(String::as_str),
            Some("front.port")
        );
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
        // RELEASE_TMP must be a per-user-writable path, not the
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
        // Unlike the other Windows tests here, this one needs a
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
        let config = SpawnConfig::new(launcher, ws.id.clone());

        let cmd =
            build_launch_command(&config, Path::new("front.port")).expect("should build a command");

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

    /// `bin/server`'s Unix arm and
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
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone());
        let cmd =
            build_launch_command(&config, Path::new("front.port")).expect("should build a command");
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
    fn release_tmp_dir_is_scoped_by_workspace_and_instance() {
        let a = release_tmp_dir("workspace_a", "i1");
        let b = release_tmp_dir("workspace_a", "i2");
        let c = release_tmp_dir("workspace_b", "i1");
        assert_ne!(
            a, b,
            "different spawns of the same workspace must not collide"
        );
        assert_ne!(a, c, "different workspaces must not collide");
    }

    #[cfg(windows)]
    #[test]
    fn build_launch_command_errors_on_launcher_platform_mismatch() {
        let ws = WindowsTestWorkspaceDir::new("win_launch_mismatch", None, Some("c"));
        // A Unix-shaped launcher path (no .bat/.cmd extension) handed to a
        // Windows build — the exact scenario
        // flagged: this used to fail with an opaque OS error instead of a
        // named one.
        let config = SpawnConfig::new(PathBuf::from(r"bin\server"), ws.id.clone());

        let result = build_launch_command(&config, Path::new("front.port"));

        assert!(
            matches!(result, Err(BrokerError::LauncherPlatformMismatch(_, _))),
            "expected LauncherPlatformMismatch, got {result:?}"
        );
    }

    /// `build_launch_command` on its own never checks the launcher
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
        let _guard = crate::test_support::lock_env();
        let ws = WindowsTestWorkspaceDir::new(
            "win_launcher_missing",
            Some("bt_attach_win_launcher_missing@localhost"),
            Some("testcookie"),
        );
        let tmp = tempfile::TempDir::new().unwrap();
        // A well-formed, absolute .bat path — passes check_launcher_platform
        // — that is simply never written to disk.
        let launcher = tmp.path().join("bin").join("bt_attach.bat");
        let mut config = SpawnConfig::new(launcher.clone(), ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_OIDC_ISSUER") };

        let result = spawn_front(&config);

        assert!(
            matches!(result, Err(BrokerError::LauncherNotFound(ref p)) if *p == launcher.display().to_string()),
            "expected LauncherNotFound({launcher:?}), got {result:?}"
        );
    }

    /// The Windows spawn path must hard-fail — not silently guess a
    /// node name via [`crate::discovery::default_node_name`] — when a
    /// workspace's `metadata.json` has no `node_name` yet (created but never
    /// started), matching `bin/server`'s Unix fail-fast behavior.
    #[cfg(windows)]
    #[test]
    fn build_launch_command_errors_when_metadata_lacks_node_name() {
        let ws = WindowsTestWorkspaceDir::new("win_launch_no_node_name", None, Some("c"));
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone());

        let result = build_launch_command(&config, Path::new("front.port"));

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
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), ws.id.clone());

        let result = build_launch_command(&config, Path::new("front.port"));

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

    // Only the #[cfg(unix)] launch_front_* tests below
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
        let config = SpawnConfig::new(PathBuf::from(r"bin\bt_attach.bat"), "abc123");

        let result = build_launch_command(&config, Path::new("front.port"));

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
        let config = SpawnConfig::new(PathBuf::from("bin/bt_attach.exe"), "abc123");

        let result = build_launch_command(&config, Path::new("front.port"));

        assert!(
            matches!(result, Err(BrokerError::LauncherPlatformMismatch(_, _))),
            "expected LauncherPlatformMismatch, got {result:?}"
        );
    }

    /// See the Windows counterpart
    /// (`spawn_front_errors_with_launcher_not_found_when_file_is_missing`)
    /// for the full rationale, including why this holds `ENV_LOCK` — this
    /// checks the same `spawn_front`-level existence guard on the Unix arm,
    /// with a well-formed (no `.bat`/`.cmd`/`.exe` extension) but
    /// never-written launcher path.
    #[cfg(unix)]
    #[test]
    fn spawn_front_errors_with_launcher_not_found_when_file_is_missing() {
        let _guard = crate::test_support::lock_env();
        let ws = TestWorkspaceDir::new("launcher_missing");
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher = tmp.path().join("server"); // deliberately never written
        let mut config = SpawnConfig::new(launcher.clone(), ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC
        // SAFETY: guarded by ENV_LOCK above.
        unsafe { std::env::remove_var("BT_OIDC_ISSUER") };

        let result = spawn_front(&config);

        assert!(
            matches!(result, Err(BrokerError::LauncherNotFound(ref p)) if *p == launcher.display().to_string()),
            "expected LauncherNotFound({launcher:?}), got {result:?}"
        );
    }

    /// Shell snippet a test launcher runs to act like a booted front: write
    /// a port to `$BT_ATTACH_PORT_FILE` the way `BtAttach.PortReport` does
    /// (temp file, then rename).
    #[cfg(unix)]
    const REPORT_PORT_SH: &str = "echo 43210 > \"$BT_ATTACH_PORT_FILE.tmp\" && \
         mv \"$BT_ATTACH_PORT_FILE.tmp\" \"$BT_ATTACH_PORT_FILE\"";

    #[cfg(unix)]
    #[test]
    fn launch_front_returns_the_port_the_front_reports() {
        let ws = TestWorkspaceDir::new("launch_ok");
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher =
            write_launcher_script(tmp.path(), "server", &format!("{REPORT_PORT_SH}; sleep 5"));

        let mut config = LaunchConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC

        let (mut child, port) = launch_front(&config).expect("should spawn and report a port");
        assert_eq!(
            port, 43_210,
            "must hand back exactly the port the front reported"
        );
        assert_eq!(
            child.try_wait().unwrap(),
            None,
            "child should still be running (the launcher sleeps)"
        );
        assert!(
            !child.port_file().exists(),
            "the one-shot port file should be consumed once read"
        );
        assert!(
            child
                .port_file()
                .starts_with(beamtalk_workspace::workspace_dir(&ws.id).unwrap()),
            "the port file should live in the workspace dir"
        );

        let _ = child.kill();
        let _ = child.wait();
    }

    #[cfg(unix)]
    #[test]
    fn launch_front_captures_front_stdout_and_stderr_to_attach_log() {
        // The spawned front's stdout/stderr must land in
        // `~/.beamtalk/workspaces/<id>/attach.log`, not the packaged app's
        // inherited (and, in a release build, nonexistent) stdio.
        let ws = TestWorkspaceDir::new("attach_log_capture");
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher = write_launcher_script(
            tmp.path(),
            "server",
            &format!("echo stdout-marker; echo stderr-marker 1>&2; {REPORT_PORT_SH}; sleep 5"),
        );

        // Pay this exact file's first-exec tax here, untimed,
        // instead of racing it against the fixed 2s `wait_for_file_contents`
        // timeout below: the *first* exec of a brand-new script file can take 200ms+ on a loaded macOS sandbox —
        // and under full-crate parallel `cargo test` scheduler contention,
        // occasionally much more — while every *subsequent* exec of that same
        // already-warm file consistently lands under 20ms. This script never
        // exits on its own (it ends in `sleep 5`), so the warm-up here can't
        // wait for a natural exit — it only needs to force the one-time
        // cold-exec cost to happen now, so the timed spawn below execs an
        // already-warm file and its `echo`s land in `attach.log` well within
        // the 2s window. Stdio is discarded rather than left inherited, so
        // this warm-up run's own `echo` output doesn't leak into the test
        // process's stdout/stderr.
        let mut warmup = Command::new(&launcher)
            .env(port::PORT_FILE_ENV, tmp.path().join("warmup.port"))
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("warm-up exec of the launcher should start");
        let _ = warmup.kill();
        let _ = warmup.wait();

        let mut config = LaunchConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC

        let (mut child, _port) = launch_front(&config).expect("should spawn and report a port");

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
        // default.
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
            if let Ok(contents) = std::fs::read_to_string(path)
                && !contents.is_empty()
            {
                return contents;
            }
            if std::time::Instant::now() >= deadline {
                return std::fs::read_to_string(path).unwrap_or_default();
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    /// A front that dies during boot (bad release, config error, a bind
    /// failure) surfaces as a named error carrying its exit status — promptly,
    /// not after the port-report timeout, and with no retry.
    #[cfg(unix)]
    #[test]
    fn launch_front_reports_a_front_that_exits_before_reporting_its_port() {
        let ws = TestWorkspaceDir::new("launch_exits");
        let tmp = tempfile::TempDir::new().unwrap();
        let counter = tmp.path().join("runs");
        let launcher = write_launcher_script(
            tmp.path(),
            "server",
            &format!("echo run >> {}; exit 1", counter.display()),
        );

        let mut config = LaunchConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml");
        config.port_report_timeout = Duration::from_secs(20);

        let started = std::time::Instant::now();
        let result = launch_front(&config);

        assert!(
            matches!(
                result,
                Err(BrokerError::FrontExitedBeforeReportingPort { ref exit_status })
                    if exit_status.contains('1')
            ),
            "expected FrontExitedBeforeReportingPort with the launcher's exit status, \
             got {result:?}"
        );
        assert!(
            started.elapsed() < Duration::from_secs(10),
            "a dead front should be reported as soon as its exit is seen"
        );
        assert_eq!(
            std::fs::read_to_string(&counter).unwrap().lines().count(),
            1,
            "no retry: the front chooses its own port, so an exit is never a port race"
        );
    }

    /// A front that stays up but never reports its port is killed (not
    /// leaked) when the wait times out.
    #[cfg(unix)]
    #[test]
    fn launch_front_kills_a_front_that_never_reports_its_port() {
        let ws = TestWorkspaceDir::new("launch_silent");
        let tmp = tempfile::TempDir::new().unwrap();
        let pid_file = tmp.path().join("pid");
        let launcher = write_launcher_script(
            tmp.path(),
            "server",
            &format!("echo $$ > {}; exec sleep 30", pid_file.display()),
        );

        let mut config = LaunchConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml");
        config.port_report_timeout = Duration::from_millis(500);

        let result = launch_front(&config);
        assert!(
            matches!(result, Err(BrokerError::PortReportTimedOut { .. })),
            "got {result:?}"
        );

        // `launch_front` kill+waits before returning, so the pid (if the
        // script got far enough to record it) must already be gone.
        if let Ok(pid) = std::fs::read_to_string(&pid_file) {
            let pid: libc::pid_t = pid.trim().parse().unwrap();
            // SAFETY: signal 0 only checks for existence; sends nothing.
            let alive = unsafe { libc::kill(pid, 0) } == 0;
            assert!(
                !alive,
                "the silent front (pid {pid}) should have been killed"
            );
        }
    }

    // ── launch_front: real spawn on Windows ─────────────────────────────
    //
    // The `build_launch_command_*` tests above only assert that the
    // `cmd.env(...)` calls that produced a `Command` ran, via
    // `get_program`/`get_args`/`get_envs` introspection — they never
    // actually spawn anything, so they can't catch anything about real
    // Windows process-spawning behavior (the `cmd.exe` wrapping a `.bat`
    // triggers per `Command`'s "BatBadBut" fix, whether `creation_flags`
    // actually applied, whether the job-object assignment in `spawn_front`
    // succeeds against a real child, whether `BT_ATTACH_PORT_FILE` reaches
    // the batch file intact). These mirror the `#[cfg(unix)]` `launch_front_*`
    // tests above, using a throwaway `.bat` in place of a `#!/bin/sh` script.
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
    fn launch_front_returns_the_port_the_front_reports_windows() {
        let _guard = crate::test_support::lock_env();
        let ws = WindowsTestWorkspaceDir::new(
            "launch_ok_win",
            Some("bt_attach_launch_ok_win@localhost"),
            Some("testcookie"),
        );
        let tmp = tempfile::TempDir::new().unwrap();
        // Report a port the way BtAttach.PortReport does (temp file, then
        // rename), then a short ping-based sleep — no coreutils `sleep` on
        // Windows.
        let launcher = write_launcher_batch(
            tmp.path(),
            "bt_attach",
            "echo 43210>\"%BT_ATTACH_PORT_FILE%.tmp\"\r\n\
             move /y \"%BT_ATTACH_PORT_FILE%.tmp\" \"%BT_ATTACH_PORT_FILE%\" >nul\r\n\
             ping -n 6 127.0.0.1 >nul",
        );

        let mut config = LaunchConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC

        let (mut child, port) = launch_front(&config).expect("should spawn and report a port");
        assert_eq!(port, 43_210);
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
    fn launch_front_reports_a_front_that_exits_before_reporting_its_port_windows() {
        let _guard = crate::test_support::lock_env();
        let ws = WindowsTestWorkspaceDir::new(
            "launch_exits_win",
            Some("bt_attach_launch_exits_win@localhost"),
            Some("testcookie"),
        );
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher = write_launcher_batch(tmp.path(), "bt_attach", "exit /b 1");

        let mut config = LaunchConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml");

        let result = launch_front(&config);
        assert!(
            matches!(
                result,
                Err(BrokerError::FrontExitedBeforeReportingPort { .. })
            ),
            "expected FrontExitedBeforeReportingPort, got {result:?}"
        );
    }

    // ── SpawnedFront's JobHandle actually kills the whole tree ─────────────
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
    //
    // BT-3880 (the test failed once on CI with `ping.exe` surviving the
    // drop): the grandchild is selected by *job membership*
    // (`JobHandle::contains_process`), not merely by parent PID, and the
    // post-drop wait is a bounded `WaitForSingleObject` on the grandchild's
    // own process handle rather than a fixed-interval poll. A `ping.exe`
    // that is a child of `cmd.exe` but not in the job is exactly what the
    // pre-`CREATE_SUSPENDED` assign race produced (`cmd.exe` ran the `.bat`
    // before `AssignProcessToJobObject` covered it), so selecting by
    // membership turns that regression into a precise "never joined the
    // job" failure instead of a misleading "survived the kill" one — and
    // also ignores any unrelated process whose recorded parent PID happens
    // to be a recycled one.

    /// Find every live process whose `th32ParentProcessID` is `parent_pid`,
    /// via `CreateToolhelp32Snapshot` (no `wmic`/`PowerShell` dependency — a
    /// plain Win32 API call, consistent with how the rest of this crate
    /// talks to Windows directly rather than shelling out). Parent PIDs are
    /// recorded, not live-tracked, so an entry here may belong to a process
    /// whose real parent died and had its PID recycled — callers confirm
    /// membership some other way (job membership, below).
    #[cfg(windows)]
    fn find_child_pids(parent_pid: u32) -> Vec<u32> {
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
            return Vec::new();
        }

        let mut entry = PROCESSENTRY32W {
            dwSize: u32::try_from(std::mem::size_of::<PROCESSENTRY32W>())
                .expect("struct size fits in u32"),
            ..Default::default()
        };
        let mut found = Vec::new();
        // SAFETY: `snapshot` is the just-created, still-owned handle above;
        // `entry` is a validly-initialized, correctly-sized buffer for this
        // call (`dwSize` set per the API's documented requirement).
        if unsafe { Process32FirstW(snapshot, &raw mut entry) } != 0 {
            loop {
                if entry.th32ParentProcessID == parent_pid {
                    found.push(entry.th32ProcessID);
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

    /// How long the process-tree-kill test below waits for `ping.exe` to
    /// appear inside the job, and then for it to die after the drop.
    /// Bounded, and generous for a loaded CI runner — but each wait is well
    /// under the ~29s `ping -n 30` runs on its own, so a grandchild that
    /// merely *finishes* can never pass as one that was *killed*. Spent only
    /// on the failure path: the appear-poll returns as soon as `ping.exe`
    /// is in the job, and the post-drop wait returns the instant it dies.
    #[cfg(windows)]
    const GRANDCHILD_DEADLINE: Duration = Duration::from_secs(10);

    /// Poll `condition` until it's true or `timeout` elapses, sleeping
    /// briefly between checks — used below in place of a single fixed sleep,
    /// since "`ping.exe` has started" is OS-scheduling-dependent, not a
    /// deterministic delay. (The post-kill wait does not poll at all: it
    /// blocks on the grandchild's process handle — [`ProcessHandle::wait_exited`].)
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

    /// An open `SYNCHRONIZE | QUERY_LIMITED_INFORMATION` handle to a process,
    /// used to test liveness (and job membership) without trusting a bare
    /// PID (BT-3698). While this handle is open the kernel keeps the process
    /// object alive, so the PID cannot be reused by an unrelated process;
    /// `has_exited`/`wait_exited` therefore answer for the process we
    /// opened, not whatever later inherits its PID.
    #[cfg(windows)]
    struct ProcessHandle(windows_sys::Win32::Foundation::HANDLE);

    #[cfg(windows)]
    impl ProcessHandle {
        fn open(pid: u32) -> Option<Self> {
            use windows_sys::Win32::Foundation::FALSE;
            use windows_sys::Win32::System::Threading::{
                OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION, PROCESS_SYNCHRONIZE,
            };

            // SAFETY: plain Win32 call; a null return (failure) is handled below.
            let handle = unsafe {
                OpenProcess(
                    PROCESS_SYNCHRONIZE | PROCESS_QUERY_LIMITED_INFORMATION,
                    FALSE,
                    pid,
                )
            };
            if handle.is_null() {
                None
            } else {
                Some(Self(handle))
            }
        }

        /// The raw handle, for `JobHandle::contains_process`. Valid only
        /// while `self` lives.
        fn raw(&self) -> windows_sys::Win32::Foundation::HANDLE {
            self.0
        }

        /// True once the process has terminated (its handle is signalled).
        fn has_exited(&self) -> bool {
            self.wait_exited(Duration::ZERO)
        }

        /// Block until the process has terminated or `timeout` elapses,
        /// returning whether it terminated. A kernel wait on the process
        /// object, so it returns the instant the kill lands rather than at
        /// the next poll tick, and the timeout only bounds the failure case.
        fn wait_exited(&self, timeout: Duration) -> bool {
            use windows_sys::Win32::Foundation::WAIT_OBJECT_0;
            use windows_sys::Win32::System::Threading::WaitForSingleObject;

            let millis = u32::try_from(timeout.as_millis()).expect("timeout fits in u32 ms");
            // SAFETY: `self.0` is a valid handle owned by `self`, closed only in `Drop`.
            unsafe { WaitForSingleObject(self.0, millis) == WAIT_OBJECT_0 }
        }
    }

    #[cfg(windows)]
    impl Drop for ProcessHandle {
        fn drop(&mut self) {
            // SAFETY: `self.0` is a valid handle owned by `self`, not used afterwards.
            unsafe { windows_sys::Win32::Foundation::CloseHandle(self.0) };
        }
    }

    #[cfg(windows)]
    #[test]
    fn dropping_spawned_front_kills_the_whole_process_tree_not_just_cmd_exe() {
        let _guard = crate::test_support::lock_env();
        let ws = WindowsTestWorkspaceDir::new(
            "job_kill_win",
            Some("bt_attach_job_kill_win@localhost"),
            Some("testcookie"),
        );
        let tmp = tempfile::TempDir::new().unwrap();
        let launcher = write_launcher_batch(tmp.path(), "bt_attach", "ping -n 30 127.0.0.1 >nul");

        let mut config = SpawnConfig::new(launcher, ws.id.clone());
        config.ide_toml_path = tmp.path().join("ide.toml"); // doesn't exist: no OIDC

        let front = spawn_front(&config).expect("should spawn");
        let cmd_pid = front.id();

        // Find the grandchild by job membership, holding an open handle to
        // it from the moment it's found: liveness is then judged through
        // that handle, never by bare PID (once ping.exe exits, its PID can
        // be reused by an unrelated process — BT-3698 — which a PID-based
        // check would report as ping.exe "surviving").
        let mut found: Option<(u32, ProcessHandle)> = None;
        let mut saw_child_outside_job = false;
        poll_until(GRANDCHILD_DEADLINE, || {
            for pid in find_child_pids(cmd_pid) {
                let Some(handle) = ProcessHandle::open(pid) else {
                    continue;
                };
                match front.job.contains_process(handle.raw()) {
                    Ok(true) => {
                        found = Some((pid, handle));
                        return true;
                    }
                    Ok(false) => saw_child_outside_job = true,
                    Err(_) => {}
                }
            }
            false
        });
        let (ping_pid, ping) = found.unwrap_or_else(|| {
            panic!(
                "ping.exe should appear as a child of the cmd.exe wrapper (pid {cmd_pid}) \
                 *inside its job object* within {GRANDCHILD_DEADLINE:?} — \
                 saw a child outside the job: {saw_child_outside_job}. If true, the \
                 CREATE_SUSPENDED → JobHandle::assign → resume_initial_thread ordering in \
                 spawn_front has regressed (BT-3880) and cmd.exe launched its child before \
                 the job covered it; if false, either the .bat didn't launch ping in time, \
                 or Windows' BatBadBut cmd.exe-wrapping behavior this module's doc comment \
                 describes no longer holds"
            )
        });
        assert!(
            !ping.has_exited(),
            "the discovered grandchild should be alive before the kill"
        );

        drop(front);

        let died = ping.wait_exited(GRANDCHILD_DEADLINE);
        assert!(
            died,
            "dropping SpawnedFront should kill the whole process tree via \
             crate::winjob::JobHandle's JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE — \
             ping.exe (pid {ping_pid}), a confirmed member of the job, survived \
             cmd.exe (pid {cmd_pid}) being dropped for {GRANDCHILD_DEADLINE:?}, \
             which would mean every front's erl.exe orphans on detach"
        );
    }
}
