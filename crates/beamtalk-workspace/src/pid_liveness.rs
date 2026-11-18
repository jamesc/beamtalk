// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-platform process-liveness primitives.
//!
//! Shared leaf module: `beamtalk-cli` (workspace node-state polling, stale
//! cache-dir sweep) and `beamtalk-desktop-broker` (orphan-reaping sweep) both
//! need the same OS-specific PID checks. Neither can import from the other;
//! `beamtalk-workspace` sits below both in the dependency graph and is the
//! correct home for this logic (see `docs/development/architecture-principles.md`
//! § Duplication & the Shared-Leaf-Module Pattern).

/// Check whether a process is alive by PID.
///
/// On Unix: uses `kill(pid, 0)` — signal 0 tests process existence without
/// sending a signal.
/// On Windows: uses `OpenProcess` + `GetExitCodeProcess` to check for
/// `STILL_ACTIVE`.
/// On other platforms: returns `false` unconditionally (safe default).
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
        // EPERM means the process exists but we lack permission to signal it —
        // it is still alive.
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

/// Read the process start time for the given PID.
///
/// Returns `None` if the process doesn't exist, the start time can't be read,
/// or the platform doesn't support this primitive.
///
/// On Linux: reads field 22 of `/proc/{pid}/stat` (the `starttime` field per
/// `proc(5)`), which is monotonically unique per PID within a boot — two
/// processes that hold the same PID at different times will have different
/// start times.
/// On Windows: reads `GetProcessTimes` `lpCreationTime` as a 64-bit
/// 100-nanosecond tick count. Windows recycles PIDs more aggressively than
/// Linux, so this value is important for the PID-reuse guard in
/// `beamtalk-desktop-broker`'s orphan-reaping sweep.
/// On other platforms: always returns `None` ("unknown, trust liveness").
#[must_use]
pub fn proc_start_time(pid: u32) -> Option<u64> {
    #[cfg(target_os = "linux")]
    {
        let stat_path = format!("/proc/{pid}/stat");
        let content = std::fs::read_to_string(stat_path).ok()?;
        // Fields are space-separated, but comm (field 2) may contain spaces/parens.
        // Find the LAST ')' to handle pathological comm names.
        let after_comm = content.rsplit_once(')')?.1;
        // Fields after comm: state(3), ppid(4), ... starttime is field 22 (1-indexed),
        // which is the 20th field after comm (fields 3..22 = 20 fields).
        let starttime_str = after_comm.split_whitespace().nth(19)?;
        starttime_str.parse::<u64>().ok()
    }

    #[cfg(windows)]
    {
        use windows_sys::Win32::Foundation::{CloseHandle, FALSE, FILETIME};
        use windows_sys::Win32::System::Threading::{
            GetProcessTimes, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
        };

        // SAFETY: Windows API call with documented parameters; handle is
        // checked for null before use and closed afterward.
        // PROCESS_QUERY_LIMITED_INFORMATION is sufficient per GetProcessTimes'
        // documented access-right requirement.
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
        // SAFETY: handle is valid (checked above); the four out-params are
        // valid, local, correctly-typed FILETIME buffers for this call.
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
    {
        let _ = pid;
        None
    }
}

/// Pure comparison shared by the desktop broker's orphan sweep and every kill path's
/// pre-kill identity re-check: is `actual` consistent with `expected`?
///
/// - `(None, _)` — no recorded start time (old record predating this field,
///   or a platform `proc_start_time` never returns a real value on) —
///   treated as a match, the same best-effort stance `beamtalk-cli`'s own
///   `NodeInfo.start_time` handling takes when start time isn't available.
/// - `(Some(_), None)` — a start time *was* recorded, but this observation
///   came back empty. Unlike the case above, this is **not**
///   ambiguous: `expected` can only be `Some(_)` if this exact platform's
///   `proc_start_time` already produced a real value once, for this same
///   PID, when the record was saved — so a platform that genuinely can't
///   read start time would have recorded `None` to begin with, never
///   `Some(_)`. The only way to reach `Some(_)` here paired with an observed
///   `None` is `proc_start_time` racing the process actually exiting between
///   `sweep`'s `is_process_alive` check and its own call (see this module's
///   doc comment) — treated as a **mismatch**, so the caller skips signaling
///   rather than trusting a liveness snapshot that may already be stale.
/// - `(Some(e), Some(a))` — compared directly.
pub fn start_time_matches(expected: Option<u64>, actual: Option<u64>) -> bool {
    match (expected, actual) {
        (Some(e), Some(a)) => e == a,
        (Some(_), None) => false,
        (None, _) => true,
    }
}

/// Re-read `pid`'s *current* start time and check it against
/// `expected_start_time` — the same [`start_time_matches`] comparison
/// the broker's `classify_record` uses, applied again immediately before killing to
/// close the PID-reuse window between `classify_record`'s check and the
/// actual kill call (see the broker's `terminate_process`). Available
/// wherever `proc_start_time` returns a real value (Linux, Windows).
pub fn still_same_process(pid: u32, expected_start_time: Option<u64>) -> bool {
    start_time_matches(expected_start_time, proc_start_time(pid))
}

/// Terminate a process by PID. The one implementation behind
/// `beamtalk workspace stop --force` and the desktop broker's orphan reaper.
///
/// - **Unix:** with `grace = None`, `SIGKILL` at once (if `is_target()` holds).
///   With `Some(grace)`, `SIGTERM`, sleep `grace`, then `SIGKILL` if the
///   process is still alive and `is_target()` still holds (the `SIGTERM` result is ignored; a failure
///   of the final kill is what is reported).
/// - **Windows:** a single `TerminateProcess` (there is no graceful signal for
///   an arbitrary external process, so `grace` is ignored). `is_target()` is
///   checked *after* the handle is open, which closes the PID-reuse window.
///
/// `is_target` is a last-moment identity check (e.g. "its start time is still
/// the one recorded"); pass `|| true` to skip it. Returns `Ok(())` when the
/// process is already gone (`ESRCH` on Unix, `ERROR_INVALID_PARAMETER` or
/// `ERROR_ACCESS_DENIED` after the handle opened on Windows) — the goal, the
/// process not running, is achieved either way.
///
/// # Errors
///
/// Returns an error for PID 0 (the "unknown PID" sentinel; signalling it would
/// hit the caller's process group), a PID the platform cannot represent, or
/// when the OS refuses the kill.
pub fn kill_process(
    pid: u32,
    grace: Option<std::time::Duration>,
    is_target: impl Fn() -> bool,
) -> std::io::Result<()> {
    use std::io::{Error, ErrorKind};

    if pid == 0 {
        return Err(Error::new(
            ErrorKind::InvalidInput,
            "process ID unavailable (0)",
        ));
    }

    #[cfg(unix)]
    {
        let pid_i = i32::try_from(pid).map_err(|_| {
            Error::new(
                ErrorKind::InvalidInput,
                format!("PID {pid} too large for platform"),
            )
        })?;
        if let Some(grace) = grace {
            // SAFETY: kill(2) with a valid pid and standard signal number.
            unsafe { libc::kill(pid_i, libc::SIGTERM) };
            std::thread::sleep(grace);
            if !is_process_alive(pid) {
                return Ok(());
            }
        }
        // Last-moment identity check, with or without a grace period: a
        // recycled PID must never receive the SIGKILL.
        if !is_target() {
            return Ok(());
        }
        // SAFETY: kill(2) with a valid pid and standard signal number.
        let ret = unsafe { libc::kill(pid_i, libc::SIGKILL) };
        if ret == 0 {
            return Ok(());
        }
        let err = Error::last_os_error();
        if err.raw_os_error() == Some(libc::ESRCH) {
            // Already exited — e.g. a graceful shutdown that finished just
            // after a wait timeout.
            return Ok(());
        }
        Err(err)
    }

    #[cfg(windows)]
    {
        use windows_sys::Win32::Foundation::{CloseHandle, FALSE};
        use windows_sys::Win32::System::Threading::{
            OpenProcess, PROCESS_TERMINATE, TerminateProcess,
        };

        let _ = grace;
        // SAFETY: Windows API call with documented parameters.
        let handle = unsafe { OpenProcess(PROCESS_TERMINATE, FALSE, pid) };
        if handle.is_null() {
            let err = Error::last_os_error();
            // ERROR_INVALID_PARAMETER (87): the process no longer exists.
            if err.raw_os_error() == Some(87) {
                return Ok(());
            }
            return Err(err);
        }
        let mut result = Ok(());
        if is_target() {
            // SAFETY: handle is valid, obtained from OpenProcess above.
            let ret = unsafe { TerminateProcess(handle, 1) };
            if ret == FALSE {
                // Capture the error *before* CloseHandle, which may clobber it.
                let err = Error::last_os_error();
                // ERROR_ACCESS_DENIED (5): the process exited between
                // OpenProcess and TerminateProcess — analogous to ESRCH.
                if err.raw_os_error() != Some(5) {
                    result = Err(err);
                }
            }
        }
        // SAFETY: handle is valid, obtained from OpenProcess above.
        unsafe { CloseHandle(handle) };
        result
    }

    #[cfg(not(any(unix, windows)))]
    {
        let _ = (grace, is_target);
        Err(Error::new(
            ErrorKind::Unsupported,
            "terminating a process is not supported on this platform",
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn current_process_is_alive() {
        // The current process is always alive — exercises kill(pid,0)==0 → true branch.
        assert!(is_process_alive(std::process::id()));
    }

    #[test]
    fn nonexistent_pid_is_not_alive() {
        // PID 4_194_304 (4M) fits in i32 but no system ever has this many processes.
        // kill(pid, 0) returns ESRCH, so the EPERM fallback returns false.
        assert!(!is_process_alive(4_194_304));
    }

    #[test]
    fn pid_u32_overflow_is_not_alive() {
        // u32::MAX cannot be converted to i32, so the early `return false` on the
        // i32::try_from branch fires before any syscall.
        assert!(!is_process_alive(u32::MAX));
    }

    #[test]
    fn kill_process_rejects_pid_zero() {
        let err = kill_process(0, None, || true).unwrap_err();
        assert_eq!(err.kind(), std::io::ErrorKind::InvalidInput);
    }

    #[cfg(unix)]
    #[test]
    fn kill_process_kills_a_running_child_and_tolerates_it_being_gone() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .spawn()
            .unwrap();
        let pid = child.id();
        kill_process(pid, None, || true).unwrap();
        child.wait().unwrap();
        // Reaped: the PID no longer exists, which is still a success.
        kill_process(pid, None, || true).unwrap();
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn current_process_has_a_start_time() {
        assert!(proc_start_time(std::process::id()).is_some());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn nonexistent_pid_has_no_start_time() {
        assert!(proc_start_time(4_194_304).is_none());
    }
}
