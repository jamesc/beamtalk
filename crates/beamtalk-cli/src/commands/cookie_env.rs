// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! How the CLI hands an Erlang cookie to an `erl` it starts (BT-5126).
//!
//! `-setcookie <c>`, `ERL_AFLAGS="-setcookie <c>"` and `-args_file` containing
//! `-setcookie <c>` all expand into the emulator's argv, so any local user can
//! read the cookie from `ps` or `/proc/<pid>/cmdline`. The process
//! *environment* is readable only by the owner (`/proc/<pid>/environ` is mode
//! 0400), so the cookie travels there instead:
//!
//! 1. the parent sets [`COOKIE_ENV`] on the child ([`hand_over`]);
//! 2. the child is started with a random, throwaway `-setcookie`
//!    ([`placeholder_args`]) so OTP never creates or reads `~/.erlang.cookie`;
//! 3. the node's first `-eval` runs [`ADOPT_COOKIE_EVAL`]
//!    (`beamtalk_ws_cookie:adopt_env_cookie/0`), which sets the real cookie and
//!    removes the variable from the node's environment.
//!
//! This is the one mechanism for every CLI-started node; the generated release
//! launchers follow the same recipe (their scripts are generated from
//! [`COOKIE_ENV`] / [`ADOPT_COOKIE_EVAL`] so the names cannot drift).
//!
//! **DDD Context:** CLI

use std::ffi::OsString;
use std::process::Command;

/// Environment variable carrying the cookie to a child `erl`. The Erlang side
/// is `beamtalk_ws_cookie:env_var/0`; a conformance test compares the two.
pub(crate) const COOKIE_ENV: &str = "BEAMTALK_ERL_COOKIE";

/// Erlang expression, first in a node's `-eval`, that adopts [`COOKIE_ENV`].
pub(crate) const ADOPT_COOKIE_EVAL: &str = "beamtalk_ws_cookie:adopt_env_cookie()";

/// `-setcookie <random>`: a placeholder that is *not* the real cookie, so what
/// reaches `ps` is worthless. Fresh per call; never reused as a credential.
pub(crate) fn placeholder_args() -> Vec<OsString> {
    vec![
        OsString::from("-setcookie"),
        OsString::from(super::workspace::storage::generate_cookie()),
    ]
}

/// Put `cookie` in the child's environment (owner-only visibility).
pub(crate) fn hand_over(cmd: &mut Command, cookie: &str) {
    cmd.env(COOKIE_ENV, cookie);
}

/// Prefix `eval` with [`ADOPT_COOKIE_EVAL`] (both are comma-separated
/// expression sequences ending in `.` or not; this only prepends).
pub(crate) fn adopt_then(eval: &str) -> String {
    format!("{ADOPT_COOKIE_EVAL}, {eval}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn placeholder_is_random_and_not_the_real_cookie() {
        let a = placeholder_args();
        let b = placeholder_args();
        assert_eq!(a[0], OsString::from("-setcookie"));
        assert_ne!(a[1], b[1], "placeholder must be fresh per call");
    }

    /// The Rust names and the Erlang side are one contract; there is no
    /// shared code across the boundary, so pin both ends to the source.
    #[test]
    fn names_match_the_erlang_adopt_function() {
        let src = std::fs::read_to_string(
            beamtalk_workspace::cargo_workspace_root!()
                .join("runtime/apps/beamtalk_workspace/src/beamtalk_ws_cookie.erl"),
        )
        .unwrap();
        assert!(
            src.contains(&format!("-define(COOKIE_ENV, \"{COOKIE_ENV}\").")),
            "beamtalk_ws_cookie.erl must read {COOKIE_ENV}"
        );
        let (module, rest) = ADOPT_COOKIE_EVAL.split_once(':').unwrap();
        let function = rest.strip_suffix("()").unwrap();
        assert_eq!(module, "beamtalk_ws_cookie");
        assert!(
            src.contains(&format!("{function}/0")),
            "beamtalk_ws_cookie must export {function}/0"
        );
    }

    #[test]
    fn adopt_then_prepends() {
        assert_eq!(
            adopt_then("halt()."),
            "beamtalk_ws_cookie:adopt_env_cookie(), halt()."
        );
    }

    /// BT-5126, real node: started the way every CLI path starts one
    /// (placeholder `-setcookie`, cookie in the environment, adopting `-eval`),
    /// the node ends up with the real cookie while neither `ps` nor
    /// `/proc/<pid>/cmdline` shows it, and the variable is gone from the node.
    #[cfg(unix)]
    #[test]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    fn real_node_adopts_cookie_and_hides_it_from_ps() {
        use std::io::{BufRead, BufReader};
        use std::process::Stdio;

        let runtime = beamtalk_cli::repl_startup::discover_beam_paths().unwrap();
        let cookie = crate::commands::workspace::storage::generate_cookie();
        let name = format!("bt5126_cookie_{}", std::process::id());
        let eval = adopt_then(&format!(
            "io:format(\"COOKIE:~s~nENV:~p~n\", \
             [atom_to_list(erlang:get_cookie()), os:getenv(\"{COOKIE_ENV}\")]), \
             timer:sleep(60000), halt()."
        ));
        let mut cmd = beamtalk_workspace::erl_command();
        cmd.args(["-noshell", "-sname", &name])
            .args(placeholder_args())
            .args(beamtalk_workspace::loopback_dist_args())
            .args(beamtalk_cli::repl_startup::beam_pa_args(&runtime.paths))
            .arg("-eval")
            .arg(eval)
            .stdout(Stdio::piped())
            .stderr(Stdio::null());
        hand_over(&mut cmd, &cookie);
        let mut child = cmd.spawn().expect("spawn erl");
        let mut lines = BufReader::new(child.stdout.take().unwrap()).lines();
        let reported = lines.next().and_then(Result::ok).unwrap_or_default();
        let env_line = lines.next().and_then(Result::ok).unwrap_or_default();
        let pid = child.id();
        let checked = std::panic::catch_unwind(|| {
            beamtalk_workspace::test_argv::assert_not_in_argv(pid, &cookie);
        });
        let _ = child.kill();
        let _ = child.wait();
        checked.unwrap();
        assert_eq!(reported, format!("COOKIE:{cookie}"));
        assert_eq!(
            env_line, "ENV:false",
            "the variable must be unset after adoption"
        );
    }
}
