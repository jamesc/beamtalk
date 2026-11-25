// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The shell convention for reporting a child's exit status as one integer.

use std::process::ExitStatus;

/// The exit code a shell would report for `status`, or `-1` when it has
/// neither a code nor a terminating signal. See [`status_code`].
#[must_use]
pub fn shell_exit_code(status: ExitStatus) -> i32 {
    status_code(status).unwrap_or(-1)
}

/// The exit code a shell would report for `status`, if it has one.
///
/// A normal exit reports its own code. On Unix a signal death has no code and
/// reports `128 + signal` (so SIGKILL is 137, SIGTERM 143). `None` when there
/// is neither.
#[must_use]
pub fn status_code(status: ExitStatus) -> Option<i32> {
    if let Some(code) = status.code() {
        return Some(code);
    }
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        status.signal().map(|sig| 128 + sig)
    }
    #[cfg(not(unix))]
    {
        None
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::process::ExitStatusExt;

    #[test]
    fn normal_exit_reports_its_code() {
        // Raw wait status: exit code in bits 8..16.
        assert_eq!(shell_exit_code(ExitStatus::from_raw(3 << 8)), 3);
        assert_eq!(shell_exit_code(ExitStatus::from_raw(0)), 0);
    }

    #[test]
    fn signal_death_reports_128_plus_signal() {
        // Raw wait status: terminating signal in the low 7 bits.
        assert_eq!(shell_exit_code(ExitStatus::from_raw(9)), 137);
        assert_eq!(shell_exit_code(ExitStatus::from_raw(15)), 143);
    }
}
