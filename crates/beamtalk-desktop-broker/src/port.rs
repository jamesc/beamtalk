// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Port handoff for spawned fronts (ADR 0097 Broker §2, BT-4398).
//!
//! **The front picks its own port.** The broker spawns every front with
//! `PORT=0` and `BT_ATTACH_PORT_FILE=<path>`: the front's HTTP listener binds
//! port 0, so the OS assigns a free port *as part of that bind*, and the
//! front then writes the port it actually got to `<path>`
//! (`editors/liveview/lib/bt_attach/port_report.ex`, called from
//! `BtAttach.Application.start/2` once the endpoint is listening). The broker
//! polls for that file ([`wait_for_reported_port`]) and probes exactly the
//! port the front reports.
//!
//! This replaces the original bind-then-drop handoff (bind `127.0.0.1:0`,
//! read the number, close, pass it to the front) that the spike
//! (`docs/research/desktop-shell-spike.md`, criterion (a)) flagged as a
//! check-then-use race: between the probe listener closing and the front's
//! own bind, any process — including a parallel front spawn — could take the
//! port. The retry loop that used to paper over that race could only detect
//! a conflict that made the front's launcher exit; a foreign process that
//! took the port and *accepted connections* went unnoticed, and the
//! readiness probe could have talked to the wrong server. With the front
//! choosing its own port there is no window to race, on any platform, so no
//! retry loop and no readiness nonce are needed: the reported port is, by
//! construction, the one our front's listener holds.
//!
//! Same shape as the workspace node's own `port` file
//! (`beamtalk_repl_server` writes it; `beamtalk workspace create` polls it).
//!
//! **Port-file location and lifetime.** `~/.beamtalk/workspaces/<id>/` (next
//! to `attach.log`), named per spawn by [`new_instance_id`] so two fronts for
//! the same workspace — a stale one being reaped while a new one starts —
//! never read each other's file. The front writes it atomically (temp file +
//! rename), so a present file is always complete. The broker deletes it once
//! read, and on every failure path ([`remove_port_file`]) — it is a one-shot
//! handoff, not a record of the front's state ([`crate::reap`]'s records are
//! that).
//!
//! **DDD Context:** Desktop Shell

use std::path::{Path, PathBuf};
use std::process::Child;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use crate::error::{BrokerError, Result};

/// Env var naming the file the front writes its bound port to. Must match
/// `BtAttach.PortReport.env_var/0` (`editors/liveview/lib/bt_attach/port_report.ex`).
pub const PORT_FILE_ENV: &str = "BT_ATTACH_PORT_FILE";

/// Default upper bound on how long [`wait_for_reported_port`] waits for a
/// still-running front to report its port.
///
/// A healthy front reaches HTTP-up in well under 1s (measured against a real
/// `dist-liveview` release, BT-3004), and the port file is written the moment
/// the endpoint is listening, so this is headroom for a cold first boot
/// (Windows real-time scanning of a fresh install, a loaded machine), not a
/// measured requirement. A front that *crashes* during boot is reported as
/// soon as its exit is observed, not after this timeout.
pub const DEFAULT_PORT_REPORT_TIMEOUT: Duration = Duration::from_secs(30);

/// How often [`wait_for_reported_port`] re-checks the file and the child.
const POLL_INTERVAL: Duration = Duration::from_millis(20);

/// A fresh, process-unique identifier for one front spawn — used to name its
/// port file ([`port_file_path`]) and, on Windows, its per-front
/// `RELEASE_TMP` directory (`crate::spawn::release_tmp_dir`).
///
/// Unique, not secret: it combines this broker's pid, the current time and a
/// per-process counter, so neither two spawns in this process nor two broker
/// processes can collide. Nothing relies on it being unguessable — the port
/// file lives in the user's own workspace directory.
#[must_use]
pub fn new_instance_id() -> String {
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_nanos());
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{:x}-{nanos:x}-{n:x}", std::process::id())
}

/// Where the front spawned as `instance_id` for `workspace_id` reports its
/// port: `~/.beamtalk/workspaces/<id>/attach-<instance_id>.port`.
///
/// # Errors
///
/// Returns an error if the workspace directory can't be resolved (no home
/// directory).
pub fn port_file_path(workspace_id: &str, instance_id: &str) -> Result<PathBuf> {
    let dir = beamtalk_workspace::workspace_dir(workspace_id)?;
    Ok(dir.join(format!("attach-{instance_id}.port")))
}

/// Read a port file the front wrote. `Ok(None)` while it doesn't exist yet.
///
/// # Errors
///
/// Returns [`BrokerError::InvalidPortFile`] if the file exists but doesn't
/// hold a single non-zero port number (the front writes atomically, so this
/// is never a half-written file), or an I/O error other than "not found".
pub fn read_port_file(path: &Path) -> Result<Option<u16>> {
    let content = match std::fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(e.into()),
    };
    match content.trim().parse::<u16>() {
        Ok(port) if port != 0 => Ok(Some(port)),
        _ => Err(BrokerError::InvalidPortFile {
            path: path.display().to_string(),
            content,
        }),
    }
}

/// Best-effort removal of a port file and any temp file the front's atomic
/// write may have left next to it. Never fails: a leftover file is harmless
/// (each spawn uses a fresh name), so there is nothing a caller could do
/// differently on error.
pub fn remove_port_file(path: &Path) {
    let _ = std::fs::remove_file(path);
    let mut tmp = path.as_os_str().to_owned();
    tmp.push(".tmp");
    let _ = std::fs::remove_file(PathBuf::from(tmp));
}

/// Wait for the front `child` to report its bound port through `port_file`.
///
/// Polls until the file appears (returning the port, and deleting the file),
/// the child exits (it crashed or was refused during boot — the details are
/// in its `attach.log`), or `timeout` passes. Liveness is checked before the
/// file on every iteration, so a front that wrote its port and then died is
/// reported as dead rather than handed back as ready. Removes the port file
/// on every exit path.
///
/// # Errors
///
/// Returns [`BrokerError::FrontExitedBeforeReportingPort`] if the child
/// exits first, [`BrokerError::PortReportTimedOut`] if `timeout` elapses,
/// [`BrokerError::InvalidPortFile`] for a malformed file, or an I/O error
/// from `try_wait`/reading the file.
pub fn wait_for_reported_port(
    child: &mut Child,
    port_file: &Path,
    timeout: Duration,
) -> Result<u16> {
    let result = poll_for_port(child, port_file, timeout);
    remove_port_file(port_file);
    result
}

fn poll_for_port(child: &mut Child, port_file: &Path, timeout: Duration) -> Result<u16> {
    let deadline = Instant::now() + timeout;
    loop {
        if let Some(status) = child.try_wait()? {
            return Err(BrokerError::FrontExitedBeforeReportingPort {
                exit_status: status.to_string(),
            });
        }
        if let Some(port) = read_port_file(port_file)? {
            return Ok(port);
        }
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return Err(BrokerError::PortReportTimedOut { timeout });
        }
        std::thread::sleep(POLL_INTERVAL.min(remaining));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn new_instance_id_is_unique_across_calls() {
        let ids: HashSet<String> = (0..100).map(|_| new_instance_id()).collect();
        assert_eq!(ids.len(), 100, "every spawn needs its own port-file name");
    }

    #[test]
    fn port_file_path_lives_in_the_workspace_dir_and_is_scoped_by_instance() {
        let a = port_file_path("ws_a", "i1").unwrap();
        let b = port_file_path("ws_a", "i2").unwrap();
        let c = port_file_path("ws_b", "i1").unwrap();
        assert_eq!(
            a.parent(),
            Some(beamtalk_workspace::workspace_dir("ws_a").unwrap().as_path())
        );
        assert_eq!(a.file_name().unwrap(), "attach-i1.port");
        assert_ne!(
            a, b,
            "two spawns for one workspace must not share a port file"
        );
        assert_ne!(a, c, "two workspaces must not share a port file");
    }

    #[test]
    fn read_port_file_is_none_until_the_file_exists() {
        let tmp = tempfile::TempDir::new().unwrap();
        assert_eq!(
            read_port_file(&tmp.path().join("front.port")).unwrap(),
            None
        );
    }

    #[test]
    fn read_port_file_parses_the_fronts_newline_terminated_port() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("front.port");
        std::fs::write(&path, "51234\n").unwrap();
        assert_eq!(read_port_file(&path).unwrap(), Some(51234));
    }

    #[test]
    fn read_port_file_rejects_garbage_and_port_zero() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("front.port");
        for bad in ["", "abc", "0\n", "70000", "-1"] {
            std::fs::write(&path, bad).unwrap();
            assert!(
                matches!(
                    read_port_file(&path),
                    Err(BrokerError::InvalidPortFile { .. })
                ),
                "{bad:?} should be rejected"
            );
        }
    }

    #[test]
    fn remove_port_file_removes_the_file_and_a_leftover_temp_file() {
        let tmp = tempfile::TempDir::new().unwrap();
        let path = tmp.path().join("front.port");
        let tmp_path = tmp.path().join("front.port.tmp");
        std::fs::write(&path, "1\n").unwrap();
        std::fs::write(&tmp_path, "1\n").unwrap();
        remove_port_file(&path);
        assert!(!path.exists());
        assert!(!tmp_path.exists());
        remove_port_file(&path); // already gone: still fine
    }

    /// A child that writes the port file the way the front does (temp file,
    /// then rename) and keeps running.
    #[cfg(unix)]
    fn spawn_reporting_child(port_file: &Path, port: u16) -> Child {
        std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg(format!(
                "sleep 0.1; echo {port} > \"$0.tmp\" && mv \"$0.tmp\" \"$0\"; sleep 5"
            ))
            .arg(port_file)
            .spawn()
            .unwrap()
    }

    #[cfg(unix)]
    #[test]
    fn wait_for_reported_port_returns_the_reported_port_and_consumes_the_file() {
        let tmp = tempfile::TempDir::new().unwrap();
        let port_file = tmp.path().join("front.port");
        let mut child = spawn_reporting_child(&port_file, 43_210);

        let port = wait_for_reported_port(&mut child, &port_file, Duration::from_secs(10));
        let _ = child.kill();
        let _ = child.wait();

        assert_eq!(port.unwrap(), 43_210);
        assert!(
            !port_file.exists(),
            "the one-shot port file should be deleted once read"
        );
    }

    #[cfg(unix)]
    #[test]
    fn wait_for_reported_port_reports_a_child_that_exits_first() {
        let tmp = tempfile::TempDir::new().unwrap();
        let port_file = tmp.path().join("front.port");
        let mut child = std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg("exit 3")
            .spawn()
            .unwrap();

        let started = Instant::now();
        let result = wait_for_reported_port(&mut child, &port_file, Duration::from_secs(10));

        assert!(
            matches!(
                result,
                Err(BrokerError::FrontExitedBeforeReportingPort { ref exit_status })
                    if exit_status.contains('3')
            ),
            "expected FrontExitedBeforeReportingPort carrying the exit status, got {result:?}"
        );
        assert!(
            started.elapsed() < Duration::from_secs(5),
            "a dead front should be reported promptly, not after the timeout"
        );
    }

    /// A front that wrote its port and then died must not be handed back as
    /// live — liveness is checked before the file.
    #[cfg(unix)]
    #[test]
    fn wait_for_reported_port_prefers_exit_over_a_file_from_a_dead_front() {
        let tmp = tempfile::TempDir::new().unwrap();
        let port_file = tmp.path().join("front.port");
        let mut child = std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg("echo 4321 > \"$0\"; exit 1")
            .arg(&port_file)
            .spawn()
            .unwrap();
        let _ = child.wait();

        let result = wait_for_reported_port(&mut child, &port_file, Duration::from_secs(10));
        assert!(
            matches!(
                result,
                Err(BrokerError::FrontExitedBeforeReportingPort { .. })
            ),
            "got {result:?}"
        );
        assert!(
            !port_file.exists(),
            "failure paths clean up the port file too"
        );
    }

    #[cfg(unix)]
    #[test]
    fn wait_for_reported_port_times_out_on_a_silent_live_child() {
        let tmp = tempfile::TempDir::new().unwrap();
        let port_file = tmp.path().join("front.port");
        let mut child = std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg("sleep 5")
            .spawn()
            .unwrap();

        let result = wait_for_reported_port(&mut child, &port_file, Duration::from_millis(150));
        let _ = child.kill();
        let _ = child.wait();

        assert!(
            matches!(result, Err(BrokerError::PortReportTimedOut { timeout }) if timeout == Duration::from_millis(150)),
            "got {result:?}"
        );
    }
}
