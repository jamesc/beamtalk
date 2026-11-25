// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Unix watchdog: kills the port's process groups when the port dies.
//!
//! **DDD Context:** Runtime
//!
//! Each child runs in its own session (`setsid`), so it is not in the port's
//! process group and nothing kills it when the port dies abnormally (BT-5195).
//! A SIGTERM handler covers a polite stop and `PR_SET_PDEATHSIG` covers the
//! direct child on Linux, but neither covers SIGKILL of the port on macOS,
//! nor grandchildren anywhere. So the port re-executes itself as
//! `beamtalk-exec --watchdog` (on its first `spawn`, so a port that never
//! spawns costs no extra process), keeping the write end of the watchdog's
//! stdin.
//! The port writes `+<pgid>` when it starts a child's group and `-<pgid>` when
//! the group is done. When the port dies, however it dies, the OS closes that
//! pipe; the watchdog reads EOF, SIGTERMs every group still registered, waits
//! the grace period, SIGKILLs any survivor and exits.
//!
//! The watchdog ignores SIGINT, SIGTERM, SIGHUP and SIGPIPE: a Ctrl+C or
//! hangup aimed at the BEAM's process group must not kill it before the port.

use std::collections::BTreeSet;
use std::io::{self, BufRead, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::Mutex;
use std::time::Duration;

use beamtalk_process::escalate::{Outcome, terminate_and_wait};
use tracing::warn;

/// Poll interval while waiting for the groups to go.
const POLL: Duration = Duration::from_millis(50);

/// The port's handle on its watchdog process.
pub(crate) struct Watchdog {
    pipe: Mutex<ChildStdin>,
    /// Held so the handle is not dropped; the watchdog outlives the port by
    /// design and is reaped by whoever inherits it.
    _process: Child,
}

impl Watchdog {
    /// Start `exe --watchdog` with a pipe on its stdin.
    pub(crate) fn start(exe: &std::path::Path) -> io::Result<Self> {
        let mut process = Command::new(exe)
            .arg("--watchdog")
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()?;
        let pipe = process
            .stdin
            .take()
            .ok_or_else(|| io::Error::other("watchdog stdin not captured"))?;
        Ok(Self {
            pipe: Mutex::new(pipe),
            _process: process,
        })
    }

    /// Tell the watchdog that `pgid` is a live child group.
    pub(crate) fn register(&self, pgid: libc::pid_t) {
        self.send(&format!("+{pgid}\n"));
    }

    /// Tell the watchdog that `pgid` is done and must not be signalled.
    pub(crate) fn unregister(&self, pgid: libc::pid_t) {
        self.send(&format!("-{pgid}\n"));
    }

    fn send(&self, line: &str) {
        if let Ok(mut pipe) = self.pipe.lock()
            && let Err(e) = pipe.write_all(line.as_bytes())
        {
            warn!("watchdog write failed: {e}");
        }
    }
}

/// Run `--watchdog` mode on the process's stdin, then exit.
pub(crate) fn run(grace: Duration) -> ! {
    for sig in [libc::SIGINT, libc::SIGTERM, libc::SIGHUP, libc::SIGPIPE] {
        // SAFETY: signal(2) with SIG_IGN installs no handler code.
        unsafe { libc::signal(sig, libc::SIG_IGN) };
    }
    watch(io::stdin().lock(), grace, std::thread::sleep);
    std::process::exit(0);
}

/// Track `+pgid` / `-pgid` lines from `input`; at EOF (or a read error), stop
/// every group still registered.
pub(crate) fn watch(input: impl BufRead, grace: Duration, sleep: impl FnMut(Duration)) {
    let mut groups = BTreeSet::new();
    for line in input.lines() {
        let Ok(line) = line else { break };
        let (op, pgid) = line.split_at(line.len().min(1));
        // A pgid <= 1 would signal init or every process we may signal.
        let Some(pgid) = pgid.parse::<libc::pid_t>().ok().filter(|&p| p > 1) else {
            continue;
        };
        match op {
            "+" => {
                groups.insert(pgid);
            }
            "-" => {
                groups.remove(&pgid);
            }
            _ => {}
        }
    }
    stop_groups(&groups, grace, sleep);
}

/// SIGTERM every group, wait up to `grace` for all of them to go, then
/// SIGKILL the survivors.
fn stop_groups(groups: &BTreeSet<libc::pid_t>, grace: Duration, sleep: impl FnMut(Duration)) {
    if groups.is_empty() {
        return;
    }
    let outcome = terminate_and_wait(
        grace,
        POLL,
        sleep,
        || signal_groups(groups, libc::SIGTERM),
        || groups.iter().all(|&pgid| !group_exists(pgid)),
    );
    if outcome == Outcome::GraceExpired {
        signal_groups(groups, libc::SIGKILL);
    }
}

fn signal_groups(groups: &BTreeSet<libc::pid_t>, sig: libc::c_int) {
    for &pgid in groups {
        // SAFETY: kill(2) to a process group; pgid > 1 is checked on input.
        unsafe { libc::kill(-pgid, sig) };
    }
}

/// Whether any process is still in process group `pgid`.
pub(crate) fn group_exists(pgid: libc::pid_t) -> bool {
    // SAFETY: kill(2) with signal 0 only checks for existence.
    let rc = unsafe { libc::kill(-pgid, 0) };
    rc == 0 || io::Error::last_os_error().raw_os_error() != Some(libc::ESRCH)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::process::{CommandExt, ExitStatusExt};

    /// `sleep 300` in its own session, as the port starts children.
    fn session_leader() -> Child {
        let mut cmd = Command::new("sleep");
        cmd.arg("300");
        cmd.process_group(0);
        cmd.spawn().expect("spawn sleep")
    }

    #[test]
    fn eof_terminates_registered_groups() {
        let mut child = session_leader();
        let pgid = libc::pid_t::try_from(child.id()).unwrap();
        let input = format!("+{pgid}\n");
        watch(input.as_bytes(), Duration::from_hours(1), |d| {
            // The child is ours, so it only goes once reaped: reap it here.
            if matches!(child.try_wait(), Ok(Some(_))) {
                return;
            }
            std::thread::sleep(d);
        });
        let status = child.wait().expect("wait");
        assert_eq!(status.signal(), Some(libc::SIGTERM));
    }

    #[test]
    fn unregistered_groups_are_left_alone() {
        let mut child = session_leader();
        let pgid = libc::pid_t::try_from(child.id()).unwrap();
        let input = format!("+{pgid}\n-{pgid}\n+1\n+0\n+-5\nnonsense\n");
        watch(input.as_bytes(), Duration::from_hours(1), |_| {
            panic!("nothing registered, so nothing to wait for");
        });
        assert!(child.try_wait().expect("try_wait").is_none());
        child.kill().expect("kill");
        child.wait().expect("wait");
    }

    #[test]
    fn survivors_of_the_grace_period_are_killed() {
        let mut cmd = Command::new("sh");
        // `trap '' TERM` ignores SIGTERM; `exec sleep` keeps that disposition.
        cmd.args(["-c", "trap '' TERM; echo ready; exec sleep 300"]);
        cmd.stdout(Stdio::piped());
        cmd.process_group(0);
        let mut child = cmd.spawn().expect("spawn");
        let mut ready = String::new();
        io::BufReader::new(child.stdout.take().unwrap())
            .read_line(&mut ready)
            .expect("read ready");
        let pgid = libc::pid_t::try_from(child.id()).unwrap();
        watch(format!("+{pgid}\n").as_bytes(), Duration::ZERO, |_| {});
        let status = child.wait().expect("wait");
        assert_eq!(status.signal(), Some(libc::SIGKILL));
    }
}
