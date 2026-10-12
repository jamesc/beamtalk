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
