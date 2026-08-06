// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Orphan reaping (ADR 0097 Broker §4 / Implementation §6g).
//!
//! If the broker dies uncleanly (`SIGKILL`, logout, crash), the fronts it
//! spawned are orphaned — cookie-bearing BEAM processes with nothing left to
//! detach them. The spike (`docs/research/desktop-shell-spike.md`, criterion
//! (g)) prototyped and validated **PID-file sweep** over process-group kill
//! or a parent-death watch (`PR_SET_PDEATHSIG`): the only one of the three
//! portable to Windows, which the ADR's packaging phase targets alongside
//! macOS/Linux.
//!
//! The spike also flagged a real gap in that mechanism, to harden here: a
//! naive PID-file sweep kills whatever process currently holds the recorded
//! PID with no check that it's still the *same* process — if the orphan
//! already died and the OS recycled its PID before the next sweep, sweeping
//! kills an unrelated process. This module closes that gap by recording each
//! front's process start time at spawn time (the same technique
//! `beamtalk-cli`'s `NodeInfo.start_time` already uses for its own PID-reuse
//! detection) and refusing to signal a PID whose *current* start time
//! doesn't match what was recorded. [`read_start_time`] was inert on Windows
//! until BT-3046 (`None` unconditionally, which `classify_record` treats as
//! "unknown, trust liveness" — every sweep matched, closing nothing); it now
//! reads a real value there via `GetProcessTimes`.
//!
//! **What "the recorded PID" means on Windows** (BT-2988): the caller
//! persists `Child::id()` after `spawn_front`, which per [`crate::winjob`]'s
//! doc comment is `cmd.exe`'s PID, not `erl.exe`'s (`.bat` launches can only
//! run via a console-subsystem wrapper). A `Reap` here terminates that
//! `cmd.exe` — usually enough on its own, since Windows tears down a process
//! whose parent has already exited, though not synchronously or
//! unconditionally the way [`crate::winjob::JobHandle`]'s kill-on-close
//! guarantees the whole tree dies. This module's PID-reuse hardening is
//! still real and worth having (it stops a *different*, unrelated process
//! from being killed), but a stale record reaching this sweep at all should
//! be rare in the crash scenario it exists for: `JobHandle`'s kill-on-close
//! already tears down the entire `cmd.exe`/`erl.exe` tree the moment a live
//! broker dies, before any future broker's sweep would ever run. This
//! module is the fallback net for what that misses — see `crate::winjob`'s
//! doc comment for its own residual (assign-after-spawn) race.
//!
//! **DDD Context:** Desktop Shell

use std::io::ErrorKind;
use std::path::{Path, PathBuf};
#[cfg(unix)]
use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::error::Result;

/// Bookkeeping for one spawned front, persisted so a future broker process
/// (after a crash) can find and reap it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FrontRecord {
    pub workspace_id: String,
    pub port: u16,
    pub pid: u32,
    pub node_name: String,
    /// Process start time at record time (Linux: `/proc/{pid}/stat` field 22
    /// clock ticks since boot; `None` on platforms without a cheap
    /// equivalent, or if it couldn't be read). Used to detect PID reuse —
    /// see the module docs.
    pub start_time: Option<u64>,
}

/// Directory the broker keeps its own state in — deliberately separate from
/// `~/.beamtalk/workspaces/` (owned by the Rust CLI; the spike's `broker.sh`
/// established this same separation with its own `STATE_DIR`).
///
/// # Errors
///
/// Returns an error if the home directory cannot be determined.
pub fn state_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| {
        crate::error::BrokerError::Io(std::io::Error::new(
            ErrorKind::NotFound,
            "could not determine home directory",
        ))
    })?;
    Ok(home.join(".beamtalk").join("desktop-broker"))
}

fn record_path(dir: &Path, workspace_id: &str, port: u16) -> PathBuf {
    dir.join(format!("{workspace_id}.{port}.json"))
}

/// Persist a front record so a future broker start can find it if this
/// broker dies before detaching it cleanly.
///
/// # Errors
///
/// Returns an error if `dir` can't be created or the record can't be written.
pub fn save_record(dir: &Path, record: &FrontRecord) -> Result<()> {
    std::fs::create_dir_all(dir)?;
    let path = record_path(dir, &record.workspace_id, record.port);
    let json = serde_json::to_string_pretty(record)?;
    std::fs::write(path, json)?;
    Ok(())
}

/// Remove a front record — called on clean detach, so a graceful stop
/// doesn't leave a stale record for the next sweep to trip over.
///
/// # Errors
///
/// Returns an error if the record file exists but can't be removed
/// (anything other than "already gone").
pub fn remove_record(dir: &Path, workspace_id: &str, port: u16) -> Result<()> {
    let path = record_path(dir, workspace_id, port);
    match std::fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e.into()),
    }
}

/// Load every front record currently on disk. Corrupt/unparsable record
/// files are skipped (logged) rather than failing the whole sweep — one bad
/// file must not hide every other orphan.
///
/// # Errors
///
/// Returns an error if listing `dir` fails for a reason other than "does
/// not exist".
pub fn load_all_records(dir: &Path) -> Result<Vec<FrontRecord>> {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) if e.kind() == ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(e.into()),
    };
    let mut records = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        let Ok(content) = std::fs::read_to_string(&path) else {
            continue;
        };
        match serde_json::from_str::<FrontRecord>(&content) {
            Ok(record) => records.push(record),
            Err(e) => {
                tracing::warn!(path = %path.display(), error = %e, "skipping unparsable front record");
            }
        }
    }
    Ok(records)
}

/// Disposition of one record, decided *before* any signal is sent — pure so
/// it's unit-testable without spawning real processes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disposition {
    /// Process alive and confirmed to be the same one that was recorded —
    /// safe to reap.
    Reap,
    /// No live process at that PID — just a stale record to clear.
    ClearStale,
    /// A process is alive at that PID, but its start time doesn't match the
    /// recorded one — the OS recycled the PID for something unrelated.
    /// Clear the stale record but do **not** signal the process.
    SkipPidReused,
}

/// Decide what to do with a record, given what's actually true about its PID
/// right now (`is_alive`, `actual_start_time` — obtained via
/// [`is_process_alive`]/[`read_start_time`], injected here so the decision
/// logic itself needs no process I/O to test).
///
/// When either the recorded or the observed start time is unavailable (a
/// platform without the cheap read, or an old record predating this field),
/// this falls back to trusting liveness alone — the same "best effort"
/// stance `beamtalk-cli`'s own `NodeInfo.start_time` handling takes for
/// backward compatibility. This means PID-reuse detection is
/// best-effort, not a hard guarantee, on platforms/records where start time
/// isn't available — matching the spike's own framing of this as hardening
/// a known gap, not eliminating it outright.
#[must_use]
pub fn classify_record(
    record: &FrontRecord,
    is_alive: bool,
    actual_start_time: Option<u64>,
) -> Disposition {
    if !is_alive {
        return Disposition::ClearStale;
    }
    if start_time_matches(record.start_time, actual_start_time) {
        Disposition::Reap
    } else {
        Disposition::SkipPidReused
    }
}

/// Pure comparison shared by [`classify_record`] and `terminate_process`'s
/// pre-`SIGKILL` re-check: is `actual` consistent with `expected`? `None` on
/// either side means "unknown" and is treated as a match — the same
/// best-effort stance `beamtalk-cli`'s own `NodeInfo.start_time` handling
/// takes when start time isn't available (old record, unsupported platform).
fn start_time_matches(expected: Option<u64>, actual: Option<u64>) -> bool {
    match (expected, actual) {
        (Some(e), Some(a)) => e == a,
        _ => true,
    }
}

/// Result of a full sweep.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SweepReport {
    pub reaped: Vec<FrontRecord>,
    pub stale_cleared: Vec<FrontRecord>,
    pub pid_reused_skipped: Vec<FrontRecord>,
}

/// Grace period between `SIGTERM` and a forced `SIGKILL` (Unix only —
/// Windows' `TerminateProcess` has no graceful-signal equivalent to wait on).
#[cfg(unix)]
const TERMINATE_GRACE: Duration = Duration::from_millis(300);

/// Sweep `dir` for orphaned fronts, killing (with a `SIGTERM` → `SIGKILL`
/// grace period) any record that [`classify_record`] says to [`Disposition::Reap`],
/// and clearing every record's file regardless of disposition (a
/// `SkipPidReused` record is just as stale as a dead one from this broker's
/// point of view — the PID it named is no longer "our" front either way).
///
/// Intended to run once at broker startup — a full broker restart means any
/// prior in-memory "what's attached" state is gone, so there is no
/// legitimate reason for a pre-existing front to still be tracked.
///
/// # Errors
///
/// Returns an error if loading or clearing records in `dir` fails.
pub fn sweep(dir: &Path) -> Result<SweepReport> {
    let mut report = SweepReport::default();
    for record in load_all_records(dir)? {
        let alive = is_process_alive(record.pid);
        let actual_start_time = if alive {
            read_start_time(record.pid)
        } else {
            None
        };
        match classify_record(&record, alive, actual_start_time) {
            Disposition::Reap => {
                tracing::info!(
                    workspace = %record.workspace_id,
                    pid = record.pid,
                    "sweep: reaping orphaned front"
                );
                terminate_process(record.pid, record.start_time);
                report.reaped.push(record.clone());
            }
            Disposition::ClearStale => {
                report.stale_cleared.push(record.clone());
            }
            Disposition::SkipPidReused => {
                tracing::warn!(
                    workspace = %record.workspace_id,
                    pid = record.pid,
                    "sweep: PID reused by an unrelated process — clearing the stale \
                     record without signaling it"
                );
                report.pid_reused_skipped.push(record.clone());
            }
        }
        remove_record(dir, &record.workspace_id, record.port)?;
    }
    Ok(report)
}

/// `SIGTERM`, wait [`TERMINATE_GRACE`], then `SIGKILL` if still alive (Unix).
/// `TerminateProcess` (Windows, no graceful-signal equivalent for an
/// arbitrary external process). Best-effort: does not surface an error for
/// "already gone by the time we got here" — that is a successful outcome for
/// a reap (the goal, the process not running, is achieved either way).
///
/// `expected_start_time` re-closes the exact PID-reuse gap [`classify_record`]
/// exists to close, on the kill path itself: `sweep` only calls this after
/// `classify_record` has already confirmed the PID is alive and (best-effort)
/// still the recorded process — but that confirmation is a snapshot from
/// *before* the kill signal. On Unix, the process could exit and the OS
/// could recycle its PID for something unrelated during [`TERMINATE_GRACE`];
/// on Windows, that same window exists (just smaller, with no grace-period
/// sleep) between `classify_record`'s check and `OpenProcess` here. Both
/// arms re-verify via [`still_same_process`] immediately before the actual
/// kill call, closing the window rather than just narrowing it.
fn terminate_process(pid: u32, expected_start_time: Option<u64>) {
    #[cfg(unix)]
    {
        if let Ok(pid_i) = i32::try_from(pid) {
            // SAFETY: kill(2) with a valid pid and standard signal number.
            unsafe { libc::kill(pid_i, libc::SIGTERM) };
            std::thread::sleep(TERMINATE_GRACE);
            if is_process_alive(pid) && still_same_process(pid, expected_start_time) {
                // SAFETY: kill(2) with a valid pid and standard signal number.
                unsafe { libc::kill(pid_i, libc::SIGKILL) };
            }
        }
    }
    #[cfg(windows)]
    {
        use windows_sys::Win32::Foundation::{CloseHandle, FALSE};
        use windows_sys::Win32::System::Threading::{
            OpenProcess, PROCESS_TERMINATE, TerminateProcess,
        };
        // No grace-window sleep on this path (a single forceful
        // TerminateProcess, no graceful-signal equivalent to wait out first),
        // so the PID-reuse window this re-check closes is inherently much
        // smaller than the Unix arm's `TERMINATE_GRACE` — but now that
        // `read_start_time` is real on Windows too (BT-3046), re-verifying
        // costs nothing and closes it anyway rather than merely accepting it
        // as "small enough", matching the Unix arm's own belt-and-suspenders
        // stance.
        // SAFETY: Windows API call with documented parameters; handle is
        // checked for null before use and closed afterward.
        unsafe {
            let handle = OpenProcess(PROCESS_TERMINATE, FALSE, pid);
            if !handle.is_null() {
                if still_same_process(pid, expected_start_time) {
                    TerminateProcess(handle, 1);
                }
                CloseHandle(handle);
            }
        }
    }
}

/// Re-read `pid`'s *current* start time and check it against
/// `expected_start_time` — the same [`start_time_matches`] comparison
/// [`classify_record`] uses, applied again immediately before killing to
/// close the PID-reuse window between `classify_record`'s check and the
/// actual kill call (see `terminate_process`'s doc comment). Available
/// wherever [`read_start_time`] returns a real value (Linux, Windows).
#[cfg(any(unix, windows))]
fn still_same_process(pid: u32, expected_start_time: Option<u64>) -> bool {
    start_time_matches(expected_start_time, read_start_time(pid))
}

/// Check whether a process is alive by PID.
///
/// Unix: `kill(pid, 0)` — signal 0 tests existence without signaling.
/// Windows: `OpenProcess` + `GetExitCodeProcess`.
#[must_use]
pub fn is_process_alive(pid: u32) -> bool {
    #[cfg(unix)]
    {
        let Ok(pid_i) = i32::try_from(pid) else {
            return false;
        };
        // SAFETY: kill(2) with signal 0 is a standard existence check.
        let ret = unsafe { libc::kill(pid_i, 0) };
        if ret == 0 {
            return true;
        }
        // EPERM means the process exists but we lack permission to signal
        // it — it is still alive.
        std::io::Error::last_os_error().raw_os_error() == Some(libc::EPERM)
    }

    #[cfg(windows)]
    {
        use windows_sys::Win32::Foundation::{CloseHandle, FALSE, STILL_ACTIVE};
        use windows_sys::Win32::System::Threading::{
            GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
        };
        // SAFETY: Windows API call with documented parameters.
        let handle = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid) };
        if handle.is_null() {
            return false;
        }
        let mut exit_code: u32 = 0;
        // SAFETY: handle is valid, exit_code is a local variable.
        let ok = unsafe { GetExitCodeProcess(handle, &raw mut exit_code) };
        // SAFETY: handle is valid, obtained from OpenProcess above.
        unsafe { CloseHandle(handle) };
        ok != FALSE && exit_code == STILL_ACTIVE as u32
    }

    #[cfg(not(any(unix, windows)))]
    {
        let _ = pid;
        false
    }
}

/// Read process start time from `/proc/{pid}/stat` (field 22 per proc(5)).
/// Linux-only: `/proc` doesn't exist on macOS/BSD/Windows, matching
/// `beamtalk-cli`'s own `read_proc_start_time` (this crate cannot reuse that
/// one directly — it is CLI-internal — so this is the same small technique
/// duplicated, not a novel mechanism).
#[cfg(target_os = "linux")]
#[must_use]
pub fn read_start_time(pid: u32) -> Option<u64> {
    let stat_path = format!("/proc/{pid}/stat");
    let content = std::fs::read_to_string(stat_path).ok()?;
    let after_comm = content.rsplit_once(')')?.1;
    let starttime_str = after_comm.split_whitespace().nth(19)?;
    starttime_str.parse::<u64>().ok()
}

/// Read process creation time via `GetProcessTimes` (BT-3046). Mirrors the
/// Linux `/proc` read above: without this, `classify_record`'s "unknown
/// start time falls back to trusting liveness" stance (see its doc comment)
/// meant the PID-reuse guard was **inert** on Windows — a stale on-disk
/// [`FrontRecord`] always classified as [`Disposition::Reap`], so a recycled
/// PID (Windows reuses them more aggressively than Linux) would get an
/// unconditional `TerminateProcess` against whatever unrelated process now
/// holds it.
///
/// Returns `None` if the process can't be opened (already gone, or a
/// permissions issue) or the call otherwise fails — same "unknown, fall back
/// to liveness" contract the Linux arm has.
#[cfg(windows)]
#[must_use]
pub fn read_start_time(pid: u32) -> Option<u64> {
    use windows_sys::Win32::Foundation::{CloseHandle, FALSE, FILETIME};
    use windows_sys::Win32::System::Threading::{
        GetProcessTimes, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
    };

    // SAFETY: Windows API call with documented parameters; handle is checked
    // for null before use and closed afterward. PROCESS_QUERY_LIMITED_INFORMATION
    // is sufficient per GetProcessTimes' own documented access-right
    // requirement (the same right `is_process_alive` above already requests).
    let handle = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid) };
    if handle.is_null() {
        return None;
    }

    let mut creation = FILETIME {
        dwLowDateTime: 0,
        dwHighDateTime: 0,
    };
    let mut exit = FILETIME {
        dwLowDateTime: 0,
        dwHighDateTime: 0,
    };
    let mut kernel = FILETIME {
        dwLowDateTime: 0,
        dwHighDateTime: 0,
    };
    let mut user = FILETIME {
        dwLowDateTime: 0,
        dwHighDateTime: 0,
    };
    // SAFETY: handle is valid (checked above); the four out-params are valid,
    // local, correctly-typed FILETIME buffers for the duration of this call.
    let ok = unsafe {
        GetProcessTimes(
            handle,
            &raw mut creation,
            &raw mut exit,
            &raw mut kernel,
            &raw mut user,
        )
    };
    // SAFETY: handle is valid, obtained from OpenProcess above.
    unsafe { CloseHandle(handle) };
    if ok == FALSE {
        return None;
    }
    Some((u64::from(creation.dwHighDateTime) << 32) | u64::from(creation.dwLowDateTime))
}

#[cfg(not(any(target_os = "linux", windows)))]
#[must_use]
pub fn read_start_time(_pid: u32) -> Option<u64> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    // `TERMINATE_GRACE` (crate-level import above) only exists on
    // `cfg(unix)`; this module's own `#[cfg(windows)]` tests below need
    // `Duration` too (BT-3046).
    #[cfg(windows)]
    use std::time::Duration;

    fn record(pid: u32, start_time: Option<u64>) -> FrontRecord {
        FrontRecord {
            workspace_id: "abc123".to_string(),
            port: 4567,
            pid,
            node_name: "bt_attach_abc123_4242@localhost".to_string(),
            start_time,
        }
    }

    // ── pure classification ─────────────────────────────────────────────

    #[test]
    fn dead_process_is_cleared_as_stale() {
        let disposition = classify_record(&record(1234, Some(100)), false, None);
        assert_eq!(disposition, Disposition::ClearStale);
    }

    #[test]
    fn alive_matching_start_time_is_reaped() {
        let disposition = classify_record(&record(1234, Some(100)), true, Some(100));
        assert_eq!(disposition, Disposition::Reap);
    }

    #[test]
    fn alive_mismatched_start_time_is_pid_reused_not_reaped() {
        // The exact gap the spike flagged: PID recycled by an unrelated
        // process between the orphan dying and the sweep running.
        let disposition = classify_record(&record(1234, Some(100)), true, Some(999));
        assert_eq!(disposition, Disposition::SkipPidReused);
    }

    #[test]
    fn alive_with_no_recorded_start_time_falls_back_to_reap() {
        // Backward compat: an older record with no start_time field, or a
        // platform without the cheap read — best-effort trusts liveness.
        let disposition = classify_record(&record(1234, None), true, Some(100));
        assert_eq!(disposition, Disposition::Reap);
    }

    #[test]
    fn alive_with_no_observed_start_time_falls_back_to_reap() {
        let disposition = classify_record(&record(1234, Some(100)), true, None);
        assert_eq!(disposition, Disposition::Reap);
    }

    #[test]
    fn alive_matching_zero_start_times_is_reaped() {
        // Boundary: 0 is a valid (if unlikely) start-time tick count, must
        // not be confused with "unavailable" (None).
        let disposition = classify_record(&record(1234, Some(0)), true, Some(0));
        assert_eq!(disposition, Disposition::Reap);
    }

    // ── record persistence round-trip ───────────────────────────────────

    #[test]
    fn save_and_load_round_trips_a_record() {
        let tmp = tempfile::TempDir::new().unwrap();
        let rec = record(4242, Some(555));
        save_record(tmp.path(), &rec).unwrap();

        let loaded = load_all_records(tmp.path()).unwrap();
        assert_eq!(loaded, vec![rec]);
    }

    #[test]
    fn remove_record_clears_it() {
        let tmp = tempfile::TempDir::new().unwrap();
        let rec = record(4242, Some(555));
        save_record(tmp.path(), &rec).unwrap();
        remove_record(tmp.path(), &rec.workspace_id, rec.port).unwrap();

        let loaded = load_all_records(tmp.path()).unwrap();
        assert!(loaded.is_empty());
    }

    #[test]
    fn remove_record_is_idempotent_when_already_gone() {
        let tmp = tempfile::TempDir::new().unwrap();
        // Removing a record that was never saved must not error.
        remove_record(tmp.path(), "nonexistent", 1).unwrap();
    }

    #[test]
    fn load_all_records_returns_empty_for_missing_dir() {
        let tmp = tempfile::TempDir::new().unwrap();
        let missing = tmp.path().join("does-not-exist");
        let loaded = load_all_records(&missing).unwrap();
        assert!(loaded.is_empty());
    }

    #[test]
    fn load_all_records_skips_unparsable_files_but_keeps_the_rest() {
        let tmp = tempfile::TempDir::new().unwrap();
        let good = record(4242, Some(555));
        save_record(tmp.path(), &good).unwrap();
        std::fs::write(tmp.path().join("garbage.json"), b"not json").unwrap();

        let loaded = load_all_records(tmp.path()).unwrap();
        assert_eq!(loaded, vec![good]);
    }

    #[test]
    fn load_all_records_ignores_non_json_files() {
        let tmp = tempfile::TempDir::new().unwrap();
        std::fs::write(tmp.path().join("notes.txt"), b"hello").unwrap();
        let loaded = load_all_records(tmp.path()).unwrap();
        assert!(loaded.is_empty());
    }

    // ── full sweep, using this test process's own PID as a live target ──

    #[test]
    fn sweep_clears_a_stale_record_for_a_pid_that_does_not_exist() {
        let tmp = tempfile::TempDir::new().unwrap();
        // A PID essentially guaranteed not to exist.
        let rec = record(u32::MAX, Some(1));
        save_record(tmp.path(), &rec).unwrap();

        let report = sweep(tmp.path()).unwrap();
        assert_eq!(report.stale_cleared, vec![rec]);
        assert!(report.reaped.is_empty());
        assert!(load_all_records(tmp.path()).unwrap().is_empty());
    }

    // Linux/Windows-only: `read_start_time` only returns a real value on
    // those two platforms (see its doc comments; BT-3046 added the Windows
    // `GetProcessTimes` arm) — `classify_record`'s fallback for a platform
    // where the *observed* start time is unavailable is `Reap` (best effort,
    // matching `beamtalk-cli`'s own stance), which on a CI runner without a
    // real `read_start_time` would make this test SIGTERM/TerminateProcess
    // its own test process. Gating this way keeps that real behavior safe to
    // exercise here; `classify_record`'s fallback branch itself is covered
    // platform-independently by `alive_with_no_observed_start_time_falls_back_to_reap`
    // above.
    #[cfg(any(target_os = "linux", windows))]
    #[test]
    fn sweep_skips_and_clears_a_pid_reused_record_without_reaping() {
        let tmp = tempfile::TempDir::new().unwrap();
        // This test process's own PID is alive, but its recorded start_time
        // is deliberately wrong — simulates PID reuse. `is_process_alive`
        // will report true; the actual observed start time (`/proc` on
        // Linux, `GetProcessTimes` on Windows) won't equal `Some(1)`, so
        // this must land in SkipPidReused rather than Reap (which would
        // SIGTERM/TerminateProcess this very test process — catastrophic if
        // this assertion is wrong).
        let own_pid = std::process::id();
        let rec = record(own_pid, Some(1));
        save_record(tmp.path(), &rec).unwrap();

        let report = sweep(tmp.path()).unwrap();
        assert!(
            report.reaped.is_empty(),
            "must never reap the test process itself"
        );
        assert_eq!(report.pid_reused_skipped, vec![rec]);
        assert!(
            is_process_alive(own_pid),
            "the test process must survive sweep"
        );
    }

    // ── terminate_process: PID-reuse re-check before SIGKILL ────────────
    //
    // Regression tests for the adversarial-review finding: `classify_record`
    // verifies start_time before `sweep` calls `terminate_process`, but the
    // SIGTERM→TERMINATE_GRACE→SIGKILL sequence has its own window in which
    // the PID could (in principle) be reused. `terminate_process` now
    // re-verifies immediately before SIGKILL — proven here against a real
    // spawned process rather than only the pure `start_time_matches` logic.

    #[cfg(target_os = "linux")]
    #[test]
    fn terminate_process_kills_when_start_time_matches() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .spawn()
            .expect("failed to spawn `sleep 30` — is coreutils installed?");
        let pid = child.id();
        // Give /proc a moment to have a stable stat entry before reading it.
        std::thread::sleep(Duration::from_millis(50));
        let real_start_time = read_start_time(pid);
        assert!(
            real_start_time.is_some(),
            "should be able to read the freshly-spawned child's start time"
        );

        terminate_process(pid, real_start_time);

        // wait() reaps the zombie and blocks until the process is actually gone.
        let status = child.wait().expect("wait on killed child should succeed");
        assert!(
            !status.success(),
            "child should have been signaled, not exit(0)"
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn terminate_process_does_not_kill_on_start_time_mismatch() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .spawn()
            .expect("failed to spawn `sleep 30` — is coreutils installed?");
        let pid = child.id();

        // A deliberately wrong expected start time — simulates the PID
        // having been reused by an unrelated process since the record was made.
        terminate_process(pid, Some(1));

        assert!(
            is_process_alive(pid),
            "must not signal a process whose start time doesn't match"
        );

        // Cleanup: the test owns this child and must not leak it regardless
        // of the assertion above.
        let _ = child.kill();
        let _ = child.wait();
    }

    // Windows counterparts of the two Linux `terminate_process` tests above
    // (BT-3046) — exercise the real `GetProcessTimes`/`TerminateProcess`
    // path rather than only `start_time_matches`'s pure logic. `ping` (not
    // `sleep`, which doesn't exist on Windows) gives a real, long-running
    // `.exe` to target — no `cmd.exe` wrapper involved, since `ping.exe` is
    // a real Win32 executable `Command::new` can exec directly. Windows now
    // re-verifies before `TerminateProcess` too (adversarial-review
    // follow-up during this same issue — `still_same_process` is no longer
    // Unix-only), so both cases apply here symmetrically with the Linux arm.

    #[cfg(windows)]
    #[test]
    fn terminate_process_kills_when_start_time_matches_windows() {
        let mut child = std::process::Command::new("ping")
            .args(["-n", "30", "127.0.0.1"])
            .spawn()
            .expect("failed to spawn `ping -n 30 127.0.0.1` — is ping.exe on PATH?");
        let pid = child.id();
        // Give the process a moment to be fully queryable before reading it.
        std::thread::sleep(Duration::from_millis(50));
        let real_start_time = read_start_time(pid);
        assert!(
            real_start_time.is_some(),
            "should be able to read the freshly-spawned child's start time"
        );

        terminate_process(pid, real_start_time);

        // wait() blocks until the process is actually gone.
        let status = child.wait().expect("wait on killed child should succeed");
        assert!(
            !status.success(),
            "child should have been terminated, not exit(0)"
        );
    }

    #[cfg(windows)]
    #[test]
    fn terminate_process_does_not_kill_on_start_time_mismatch_windows() {
        let mut child = std::process::Command::new("ping")
            .args(["-n", "30", "127.0.0.1"])
            .spawn()
            .expect("failed to spawn `ping -n 30 127.0.0.1` — is ping.exe on PATH?");
        let pid = child.id();

        // A deliberately wrong expected start time — simulates the PID
        // having been reused by an unrelated process since the record was made.
        terminate_process(pid, Some(1));

        assert!(
            is_process_alive(pid),
            "must not signal a process whose start time doesn't match"
        );

        // Cleanup: the test owns this child and must not leak it regardless
        // of the assertion above.
        let _ = child.kill();
        let _ = child.wait();
    }

    // ── read_start_time: sanity checks (BT-3046) ────────────────────────

    #[cfg(windows)]
    #[test]
    fn read_start_time_returns_some_for_a_live_process() {
        let mut child = std::process::Command::new("ping")
            .args(["-n", "5", "127.0.0.1"])
            .spawn()
            .expect("failed to spawn `ping -n 5 127.0.0.1` — is ping.exe on PATH?");
        let pid = child.id();
        std::thread::sleep(Duration::from_millis(50));

        assert!(
            read_start_time(pid).is_some(),
            "GetProcessTimes should succeed for a live, queryable process"
        );

        let _ = child.kill();
        let _ = child.wait();
    }

    /// Regression test for a mis-ordered `GetProcessTimes` out-param (BT-3046
    /// adversarial-review follow-up): the call takes four `*mut FILETIME`
    /// slots (creation, exit, kernel, user) and it's easy to read the wrong
    /// one back. `read_start_time_returns_some_for_a_live_process` above only
    /// asserts `.is_some()`, which a swapped-in `exit`/`kernel`/`user` read
    /// would still satisfy — `lpExitTime` is specifically documented to stay
    /// the zero `FILETIME` for a process that hasn't exited yet, so a
    /// same-process repeated read that's stable *and* non-zero is a much
    /// stronger signal that `dwLowDateTime`/`dwHighDateTime` came from the
    /// right (creation) slot.
    #[cfg(windows)]
    #[test]
    fn read_start_time_is_stable_and_nonzero_for_a_live_process() {
        let mut child = std::process::Command::new("ping")
            .args(["-n", "10", "127.0.0.1"])
            .spawn()
            .expect("failed to spawn `ping -n 10 127.0.0.1` — is ping.exe on PATH?");
        let pid = child.id();
        std::thread::sleep(Duration::from_millis(50));

        let first = read_start_time(pid);
        std::thread::sleep(Duration::from_millis(50));
        let second = read_start_time(pid);

        assert_ne!(
            first,
            Some(0),
            "a live process's creation time should never be the zero FILETIME \
             sentinel — Some(0) would indicate the wrong out-param (e.g. the \
             still-running process's zeroed lpExitTime) was read instead of \
             lpCreationTime"
        );
        assert_eq!(
            first, second,
            "repeated reads of the same live process's creation time must be stable"
        );

        let _ = child.kill();
        let _ = child.wait();
    }

    #[cfg(windows)]
    #[test]
    fn read_start_time_returns_none_for_a_nonexistent_pid() {
        assert_eq!(
            read_start_time(u32::MAX),
            None,
            "OpenProcess should fail for a PID essentially guaranteed not to exist"
        );
    }
}
