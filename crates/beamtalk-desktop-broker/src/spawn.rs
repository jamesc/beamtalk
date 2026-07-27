// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Spawn a per-workspace front (ADR 0097 Decision / Broker §2).
//!
//! Concretely: `PORT=<free-port> BT_ATTACH_BIND_IP=127.0.0.1
//! BT_ATTACH_NODE_SUFFIX=<id> RELEASE_DISTRIBUTION=none bin/server <id>`.
//!
//! `bin/server <id>` (`editors/liveview/rel/overlays/bin/server`) already
//! re-resolves `BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE` from
//! `~/.beamtalk/workspaces/<id>/metadata.json` + the sibling `cookie` file
//! and generates its own ephemeral `SECRET_KEY_BASE` when given a workspace
//! id — the ADR's "Local-only posture" section calls that ephemeral-per-boot
//! key *deliberate* (no durable session worth preserving on the
//! unauthenticated localhost lane), so this broker does not set it itself.
//! The broker's job is exactly the four env vars above, plus (this module's
//! other duty) refusing to spawn at all when OIDC is configured.
//!
//! **Deliberately not setting a cookie env var here**, even though the
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
//! `RELEASE_DISTRIBUTION=none` is a hard requirement, not an optimization —
//! the spike (`docs/research/desktop-shell-spike.md`, criterion (a)) found
//! that `mix release`'s generated launcher boots the VM **already**
//! distributed under `-sname bt_attach` (`RELEASE_NODE` defaults to
//! `RELEASE_NAME`) before any Elixir code runs, which pre-empts
//! `ensure_distributed/0`'s `BT_ATTACH_NODE_SUFFIX` seeding entirely and
//! makes every spawned instance collide on the identical epmd registration.
//! Booting non-distributed hands control back to the front's own lazy,
//! correctly-seeded `ensure_distributed/0` on the first `/readiness` call.

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
    /// Path to the launcher executable — `bin/server <id>` on Unix (see ADR
    /// 0097 Implementation §5b: on Windows there is no `sh` counterpart, so
    /// the eventual Windows broker (BT-2988) must invoke `bin/bt_attach`
    /// itself and set env directly; that platform split is packaging's call,
    /// not this crate's — callers resolve the right launcher path for their
    /// platform and bundle layout and pass it in here).
    pub launcher: PathBuf,
    /// Workspace id to attach to (positional arg to `bin/server`, and the
    /// `BT_ATTACH_NODE_SUFFIX` seed).
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

/// The env vars this broker sets on the spawned front, in a stable order
/// (for deterministic logging/tests — `Command::envs` doesn't care about
/// order).
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
/// `process_group(0)`; Windows: `CREATE_NEW_PROCESS_GROUP`) so a signal sent
/// to the broker's own foreground group (e.g. Ctrl-C) does not also kill the
/// front — orphan-reaping ([`crate::reap`]) is the intended mechanism for
/// cleaning up fronts left behind by a dead broker, not accidental group
/// signal propagation.
///
/// # Errors
///
/// Returns [`BrokerError::OidcConfigured`] or [`BrokerError::UnknownWorkspace`]
/// per the refusal conditions above, or [`BrokerError::Io`] if the launcher
/// process fails to spawn.
pub fn spawn_front(config: &SpawnConfig) -> Result<Child> {
    if let Some(source) = oidc_configured(&config.ide_toml_path) {
        return Err(BrokerError::OidcConfigured(source.to_string()));
    }

    if !beamtalk_workspace::workspace_dir(&config.workspace_id)
        .map(|d| d.join("metadata.json").exists())
        .unwrap_or(false)
    {
        return Err(BrokerError::UnknownWorkspace(config.workspace_id.clone()));
    }

    let mut cmd = Command::new(&config.launcher);
    cmd.arg(&config.workspace_id);
    for (key, value) in build_env(config) {
        cmd.env(key, value);
    }
    detach(&mut cmd);
    Ok(cmd.spawn()?)
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
pub fn spawn_front_with_port_retry(config: &SpawnAttemptConfig) -> Result<(Child, u16)> {
    let spawned: RefCell<Option<Child>> = RefCell::new(None);
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
    cmd.creation_flags(CREATE_NEW_PROCESS_GROUP);
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

    // ── spawn_front_with_port_retry ─────────────────────────────────────
    //
    // `spawn_front` itself refuses unless `~/.beamtalk/workspaces/<id>/`
    // exists, so these tests create (and clean up) a real, throwaway
    // workspace directory under the actual home dir — the same pattern
    // `beamtalk-cli`'s own tests use (there's no HOME-override hook in
    // `beamtalk_workspace`), rather than pointing at a fake filesystem root.

    struct TestWorkspaceDir {
        id: String,
    }

    impl TestWorkspaceDir {
        fn new(prefix: &str) -> Self {
            let id = format!("{prefix}_{}", std::process::id());
            let dir = beamtalk_workspace::workspace_dir(&id).unwrap();
            std::fs::create_dir_all(&dir).unwrap();
            std::fs::write(dir.join("metadata.json"), b"{}").unwrap();
            Self { id }
        }
    }

    impl Drop for TestWorkspaceDir {
        fn drop(&mut self) {
            if let Ok(dir) = beamtalk_workspace::workspace_dir(&self.id) {
                let _ = std::fs::remove_dir_all(dir);
            }
        }
    }

    /// Write an executable shell script to `dir` that ignores its argv
    /// (the workspace id `spawn_front` always appends) and runs `body`.
    #[cfg(unix)]
    fn write_launcher_script(dir: &std::path::Path, name: &str, body: &str) -> PathBuf {
        use std::os::unix::fs::PermissionsExt;
        let path = dir.join(name);
        std::fs::write(&path, format!("#!/bin/sh\n{body}\n")).unwrap();
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
