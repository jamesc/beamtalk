// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test helper: is a secret visible in a process's argument vector? (BT-5126)
//!
//! `ps` and `/proc/<pid>/cmdline` show every argument of every process to
//! every local user, so a cookie that reaches argv (directly, or through
//! `ERL_AFLAGS` / `-args_file`, which `erl` expands into the BEAM's argv) is
//! not secret. The tests that start a real node assert through this one
//! helper, shared by `beamtalk-cli`'s unit and integration suites. Test-only:
//! it panics on failure (like [`crate::test_cwd`]).

/// Panics if `secret` is in the argv of process `pid`, read the way another
/// local user would: `ps` and, on Linux, `/proc/<pid>/cmdline`. Also panics
/// unless the process is a BEAM, so a pid that already exited cannot make the
/// check pass vacuously. Unix only.
#[cfg(unix)]
pub fn assert_not_in_argv(pid: u32, secret: &str) {
    assert!(!secret.is_empty(), "an empty secret would match any argv");
    let ps = std::process::Command::new("ps")
        .args(["-ww", "-o", "args=", "-p", &pid.to_string()])
        .output()
        .expect("run ps");
    let ps_args = String::from_utf8_lossy(&ps.stdout).into_owned();
    assert!(
        ps_args.contains("beam") || ps_args.contains("erl"),
        "pid {pid} is not a live BEAM process; `ps` printed {ps_args:?}"
    );
    assert!(
        !ps_args.contains(secret),
        "secret visible in `ps` for pid {pid}: {ps_args}"
    );
    #[cfg(target_os = "linux")]
    {
        let cmdline: String = std::fs::read(format!("/proc/{pid}/cmdline"))
            .expect("read /proc/<pid>/cmdline")
            .iter()
            .map(|b| if *b == 0 { ' ' } else { char::from(*b) })
            .collect();
        assert!(
            !cmdline.contains(secret),
            "secret visible in /proc/{pid}/cmdline: {cmdline}"
        );
    }
}
