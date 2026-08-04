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
/// order). On Windows, [`build_launch_command`] sets four more
/// (`BT_WORKSPACE_NODE`, `BT_WORKSPACE_COOKIE`, `SECRET_KEY_BASE`,
/// `PHX_SERVER`) that don't go through this function — it does not
/// report the full env set there, only the cross-platform baseline.
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
/// process fails to spawn (Unix), or additionally if creating/assigning the
/// job object fails (Windows).
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
    let child = cmd.spawn()?;

    #[cfg(windows)]
    {
        let job = crate::winjob::JobHandle::new()?;
        // Best-effort-but-checked: assign right away (minimizes, per
        // `crate::winjob`'s doc, the residual pre-assignment race) and
        // surface a failure rather than silently returning a front this
        // job object does not actually protect.
        job.assign(&child)?;
        Ok(SpawnedFront { child, job })
    }
    #[cfg(not(windows))]
    {
        Ok(SpawnedFront { child })
    }
}

/// Build the OS process invocation for `config`, platform-specific per this
/// module's doc comment: `bin/server <id>` on Unix (self-resolving), or on
/// Windows `bin\bt_attach.bat start` with every env var `bin/server` would
/// otherwise have set resolved and set here instead.
///
/// Returns `Result` even though this Unix arm never fails: it must share a
/// signature with its `#[cfg(windows)]` sibling below, which does (a missing
/// cookie file, or a `read_node_name` failure) — `spawn_front` calls whichever
/// one this build compiles through one shared call site.
#[cfg(unix)]
#[allow(clippy::unnecessary_wraps)]
fn build_launch_command(config: &SpawnConfig) -> Result<Command> {
    let mut cmd = Command::new(&config.launcher);
    cmd.arg(&config.workspace_id);
    for (key, value) in build_env(config) {
        cmd.env(key, value);
    }
    Ok(cmd)
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
/// Returns [`BrokerError::MissingCookie`] if the workspace directory has no
/// (or an empty) `cookie` file, or propagates a [`crate::discovery::read_node_name`]
/// failure (malformed `metadata.json` — `spawn_front` already checked it
/// exists before calling this).
#[cfg(windows)]
fn build_launch_command(config: &SpawnConfig) -> Result<Command> {
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
    Ok(cmd)
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
/// heuristic. **Not calibrated against a real release build** (no built
/// `dist-liveview` target is available in the environment this crate was
/// developed in) — see that function's doc comment. Generous relative to how
/// fast a genuine `:eaddrinuse` crash should surface, so the heuristic errs
/// toward "assume it bound" rather than false-retrying a front that was just
/// slow to boot.
pub const DEFAULT_BIND_FAILURE_GRACE: Duration = Duration::from_millis(1500);

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
/// as bound and handed to the caller. Calibrating `bind_failure_grace`
/// against a real `bin/server`/`dist-liveview` boot (how fast does an
/// `:eaddrinuse` actually surface as a process exit, versus how long does a
/// slow-but-healthy boot take before the port is truly free of contention?)
/// requires a live release this crate could not build/run in the sandbox it
/// was developed in — tracked as a follow-up before this ships in a real
/// shell (BT-2986).
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
        assert!(
            a.len() >= 40,
            "expected a substantial base64-encoded 48-byte value, got {} chars",
            a.len()
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
}
