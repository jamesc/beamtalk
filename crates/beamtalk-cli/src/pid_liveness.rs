// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-platform process-liveness check.
//!
//! Shared leaf module: used by `commands::workspace::node_state` (detached
//! BEAM node liveness polling) and by `tests/cli_common` (BT-3077's stale
//! test-cache-dir sweep) so neither has to duplicate the OS-specific PID
//! check.

/// Check whether a process is alive by PID.
///
/// On Unix: uses `kill(pid, 0)` — signal 0 tests process existence without sending a signal.
/// On Windows: uses `OpenProcess` + `GetExitCodeProcess` to check for `STILL_ACTIVE`.
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn current_process_is_alive() {
        // The current process is always alive — this exercises the kill(pid,0)==0 → true branch.
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
}
