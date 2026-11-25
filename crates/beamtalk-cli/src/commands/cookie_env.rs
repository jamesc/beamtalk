// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! How the CLI hands an Erlang cookie to an `erl` it starts (BT-5126,
//! BT-5185).
//!
//! `-setcookie`, `ERL_AFLAGS` and `-args_file` all expand into the emulator's
//! argv, so any local user can read them from `ps` or `/proc/<pid>/cmdline`,
//! and a node must never be reachable with a cookie that appears there. Nor
//! may there be a window, while the node boots, in which it holds some other
//! cookie: a connection made then stays authenticated after the cookie
//! changes. So the node boots with the real cookie already in place:
//!
//! 1. [`CookieHome::new`] writes the cookie to `.erlang.cookie` (0400) in a
//!    fresh private (0700) directory;
//! 2. [`CookieHome::apply`] makes that directory the child's home for
//!    `erlexec` (`HOME` on Unix, `HOMEDRIVE` + `HOMEPATH` on Windows: the only
//!    variables it derives `-home` from), records the values it replaced, and
//!    leaves no cookie in argv or the environment. OTP's `auth` reads the
//!    file as the kernel boots, before distribution accepts a connection;
//! 3. the node's first `-eval` runs [`RESTORE_HOME_EVAL`]
//!    (`beamtalk_cookie_home:restore/0`), which puts the home variables back
//!    and removes the file and directory. A release node does the same at the
//!    start of `beamtalk_runtime_app:start/2`.
//!
//! `HOME` also tells version-manager shims (mise, asdf) where their own
//! configuration is, so an `erl` shim started with the private home cannot
//! find its Erlang. [`erl_command`] therefore starts the real `erl` script
//! (`<code:root_dir()>/bin/erl`, resolved once with the caller's own
//! environment), which needs no `HOME`.
//!
//! The generated release launchers follow the same recipe; their scripts are
//! generated from [`COOKIE_HOME_ENV`], [`ORIG_PREFIX`] and
//! [`RESTORE_HOME_EVAL`], so the names cannot drift.
//!
//! **DDD Context:** CLI

use std::ffi::{OsStr, OsString};
use std::io::Write as _;
use std::path::Path;
use std::process::Command;

/// Environment variable naming the private cookie home (Erlang:
/// `beamtalk_cookie_home:home_env/0`; a conformance test compares them).
pub(crate) const COOKIE_HOME_ENV: &str = "BEAMTALK_COOKIE_HOME";

/// Prefix of the variables recording each replaced home variable, as
/// `set:<value>` or `unset` (Erlang: `beamtalk_cookie_home:orig_prefix/0`).
pub(crate) const ORIG_PREFIX: &str = "BEAMTALK_ORIG_";

/// Erlang expression, first in a node's `-eval`, that undoes the private home.
pub(crate) const RESTORE_HOME_EVAL: &str = "beamtalk_cookie_home:restore()";

/// The home variables `erlexec` reads on this platform.
#[cfg(not(windows))]
const HOME_VARS: &[&str] = &["HOME"];
#[cfg(windows)]
const HOME_VARS: &[&str] = &["HOMEDRIVE", "HOMEPATH"];

/// A private directory holding the cookie a child `erl` boots with. Keep it
/// alive until the node has booted (its first `-eval` has run); the node
/// removes it itself, and dropping this removes whatever is left.
pub(crate) struct CookieHome {
    dir: tempfile::TempDir,
}

impl CookieHome {
    /// Write `cookie` to `.erlang.cookie` (owner read-only) in a fresh
    /// private directory.
    pub(crate) fn new(cookie: &str) -> std::io::Result<Self> {
        // Owner-only on Unix (set explicitly: the umask decides `tempfile`'s
        // mode); on Windows it sits in the per-user temp directory, whose ACL
        // admits only its owner.
        let dir = tempfile::Builder::new()
            .prefix("beamtalk-cookie-")
            .tempdir()?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt as _;
            std::fs::set_permissions(dir.path(), std::fs::Permissions::from_mode(0o700))?;
        }
        let mut options = std::fs::OpenOptions::new();
        options.write(true).create_new(true);
        #[cfg(unix)]
        {
            use std::os::unix::fs::OpenOptionsExt as _;
            options.mode(0o400);
        }
        let mut file = options.open(dir.path().join(".erlang.cookie"))?;
        file.write_all(cookie.as_bytes())?;
        Ok(Self { dir })
    }

    /// The private directory.
    pub(crate) fn path(&self) -> &Path {
        self.dir.path()
    }

    /// Make the private directory `cmd`'s home for `erlexec`, recording the
    /// values replaced so [`RESTORE_HOME_EVAL`] can put them back. Call it
    /// after anything else that sets a home variable on `cmd`.
    pub(crate) fn apply(&self, cmd: &mut Command) {
        for var in HOME_VARS {
            let original = cmd
                .get_envs()
                .find(|(k, _)| *k == OsStr::new(var))
                .map_or_else(
                    || std::env::var_os(var),
                    |(_, v)| v.map(OsStr::to_os_string),
                );
            let mut recorded = OsString::new();
            match original {
                Some(value) => {
                    recorded.push("set:");
                    recorded.push(value);
                }
                None => recorded.push("unset"),
            }
            cmd.env(format!("{ORIG_PREFIX}{var}"), recorded);
        }
        set_home_vars(cmd, self.path());
        cmd.env(COOKIE_HOME_ENV, self.path());
    }
}

#[cfg(not(windows))]
fn set_home_vars(cmd: &mut Command, dir: &Path) {
    cmd.env("HOME", dir);
}

/// `erlexec` joins `HOMEDRIVE` and `HOMEPATH`; split a `C:\...` path there,
/// or put the whole path in `HOMEPATH` (a UNC path has no drive letter).
#[cfg(windows)]
fn set_home_vars(cmd: &mut Command, dir: &Path) {
    let text = dir.as_os_str().to_string_lossy().into_owned();
    let has_drive = text.len() >= 2 && text.as_bytes()[1] == b':';
    if has_drive {
        cmd.env("HOMEDRIVE", &text[..2]).env("HOMEPATH", &text[2..]);
    } else {
        cmd.env("HOMEDRIVE", "").env("HOMEPATH", text);
    }
}

/// `Command` for the real `erl` (see the module docs), with
/// `beamtalk_workspace::pin_epmd_loopback` applied: the constructor for every
/// node started with a [`CookieHome`].
pub(crate) fn erl_command() -> Command {
    let mut cmd = Command::new(erl_program());
    beamtalk_workspace::pin_epmd_loopback(&mut cmd);
    cmd
}

/// The real `erl` launcher, or plain `erl` if it cannot be resolved.
/// Resolved once per process.
pub(crate) fn erl_program() -> OsString {
    static PROGRAM: std::sync::OnceLock<OsString> = std::sync::OnceLock::new();
    PROGRAM
        .get_or_init(|| resolve_erl_program().unwrap_or_else(|| OsString::from("erl")))
        .clone()
}

/// Ask `erl` (through any shim, with this process's environment) where its
/// installation is, and return that installation's own `bin/erl`.
fn resolve_erl_program() -> Option<OsString> {
    let probe = "io:format(\"erl-root:~s~n\", [code:root_dir()]), halt().";
    let output = beamtalk_workspace::erl_command()
        .args([
            "-noshell",
            "-noinput",
            "-boot",
            "no_dot_erlang",
            "-eval",
            probe,
        ])
        .output()
        .ok()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let root = stdout
        .lines()
        .find_map(|line| line.trim().strip_prefix("erl-root:"))?;
    let program = Path::new(root)
        .join("bin")
        .join(if cfg!(windows) { "erl.exe" } else { "erl" });
    program.is_file().then(|| program.into_os_string())
}

/// Prefix `eval` with [`RESTORE_HOME_EVAL`] (both are comma-separated
/// expression sequences ending in `.` or not; this only prepends).
pub(crate) fn restore_then(eval: &str) -> String {
    format!("{RESTORE_HOME_EVAL}, {eval}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(unix)]
    #[test]
    fn cookie_home_is_private_and_holds_the_cookie() {
        use std::os::unix::fs::PermissionsExt as _;
        let home = CookieHome::new("s3cr3t").unwrap();
        let file = home.path().join(".erlang.cookie");
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "s3cr3t");
        let mode = |p: &Path| std::fs::metadata(p).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode(home.path()), 0o700);
        assert_eq!(mode(&file), 0o400);
        let dir = home.path().to_path_buf();
        drop(home);
        assert!(!dir.exists(), "dropping it removes what the node left");
    }

    #[cfg(unix)]
    #[test]
    fn apply_points_home_at_it_and_records_the_original() {
        let home = CookieHome::new("s3cr3t").unwrap();
        let mut cmd = Command::new("not-spawned");
        cmd.env_clear().env("HOME", "/real/home");
        home.apply(&mut cmd);
        let env = |k: &str| {
            cmd.get_envs()
                .find(|(n, _)| *n == OsStr::new(k))
                .and_then(|(_, v)| v)
                .map(|v| v.to_string_lossy().into_owned())
        };
        assert_eq!(env("HOME").as_deref(), home.path().to_str());
        assert_eq!(env(COOKIE_HOME_ENV).as_deref(), home.path().to_str());
        assert_eq!(
            env(&format!("{ORIG_PREFIX}HOME")).as_deref(),
            Some("set:/real/home")
        );
        assert!(cmd.get_args().all(|a| a != "-setcookie"));
        assert!(cmd.get_envs().all(|(_, v)| v.is_none_or(|v| v != "s3cr3t")));
    }

    /// The Rust names and the Erlang side are one contract; there is no
    /// shared code across the boundary, so pin both ends to the source.
    #[test]
    fn names_match_the_erlang_restore_module() {
        let src = std::fs::read_to_string(
            beamtalk_workspace::cargo_workspace_root!()
                .join("runtime/apps/beamtalk_runtime/src/beamtalk_cookie_home.erl"),
        )
        .unwrap();
        assert!(
            src.contains(&format!("-define(HOME_ENV, \"{COOKIE_HOME_ENV}\").")),
            "beamtalk_cookie_home.erl must read {COOKIE_HOME_ENV}"
        );
        assert!(
            src.contains(&format!("-define(ORIG_PREFIX, \"{ORIG_PREFIX}\").")),
            "beamtalk_cookie_home.erl must read {ORIG_PREFIX}*"
        );
        for var in HOME_VARS {
            assert!(
                src.contains(&format!("\"{var}\"")),
                "restore must cover {var}"
            );
        }
        let (module, rest) = RESTORE_HOME_EVAL.split_once(':').unwrap();
        let function = rest.strip_suffix("()").unwrap();
        assert_eq!(module, "beamtalk_cookie_home");
        assert!(
            src.contains(&format!("{function}/0")),
            "beamtalk_cookie_home must export {function}/0"
        );
    }

    /// The resolved `erl` is a real file (not a bare name left for `PATH`),
    /// so a version-manager shim never sees the private `HOME`.
    #[test]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    fn erl_program_is_the_installations_own_launcher() {
        let program = std::path::PathBuf::from(erl_program());
        assert!(program.is_absolute() && program.is_file(), "{program:?}");
    }

    #[test]
    fn restore_then_prepends() {
        assert_eq!(
            restore_then("halt()."),
            "beamtalk_cookie_home:restore(), halt()."
        );
    }

    /// BT-5126 / BT-5185, real node: started the way every CLI path starts
    /// one (private cookie home, restoring `-eval`), the node has the real
    /// cookie while neither `ps` nor `/proc/<pid>/cmdline` shows it, its `HOME`
    /// is the original again, and the private home is gone.
    #[cfg(unix)]
    #[test]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    fn real_node_boots_with_cookie_and_restores_home() {
        use std::io::{BufRead, BufReader};
        use std::process::Stdio;

        let runtime = beamtalk_cli::repl_startup::discover_beam_paths().unwrap();
        let cookie = crate::commands::workspace::storage::generate_cookie();
        let name = format!("bt5126_cookie_{}", std::process::id());
        let eval = restore_then(&format!(
            "io:format(\"COOKIE:~s~nHOME:~s~nENV:~p~n\", \
             [atom_to_list(erlang:get_cookie()), os:getenv(\"HOME\"), \
              os:getenv(\"{COOKIE_HOME_ENV}\")]), \
             timer:sleep(60000), halt()."
        ));
        let home = CookieHome::new(&cookie).unwrap();
        let private = home.path().to_path_buf();
        let mut cmd = erl_command();
        cmd.args(["-noshell", "-sname", &name])
            .args(beamtalk_workspace::loopback_dist_args())
            .args(beamtalk_cli::repl_startup::beam_pa_args(&runtime.paths))
            .arg("-eval")
            .arg(eval)
            .env("HOME", "/the/real/home")
            .stdout(Stdio::piped())
            .stderr(Stdio::null());
        home.apply(&mut cmd);
        let mut child = cmd.spawn().expect("spawn erl");
        let mut lines = BufReader::new(child.stdout.take().unwrap()).lines();
        let reported = lines.next().and_then(Result::ok).unwrap_or_default();
        let home_line = lines.next().and_then(Result::ok).unwrap_or_default();
        let env_line = lines.next().and_then(Result::ok).unwrap_or_default();
        let pid = child.id();
        let checked = std::panic::catch_unwind(|| {
            beamtalk_workspace::test_argv::assert_not_in_argv(pid, &cookie);
        });
        let _ = child.kill();
        let _ = child.wait();
        checked.unwrap();
        assert_eq!(reported, format!("COOKIE:{cookie}"));
        assert_eq!(home_line, "HOME:/the/real/home");
        assert_eq!(env_line, "ENV:false", "the variable must be unset");
        assert!(!private.exists(), "the node removes its private home");
    }

    /// The `-setcookie` values on `pid`'s command line (Linux `/proc`).
    #[cfg(target_os = "linux")]
    fn argv_cookies(pid: u32) -> Vec<String> {
        let raw = std::fs::read(format!("/proc/{pid}/cmdline")).expect("read cmdline");
        let args: Vec<String> = raw
            .split(|b| *b == 0)
            .map(|a| String::from_utf8_lossy(a).into_owned())
            .collect();
        args.windows(2)
            .filter(|w| w[0] == "-setcookie")
            .map(|w| w[1].clone())
            .collect()
    }

    /// Ping `node` from a throwaway hidden node holding `cookie`.
    #[cfg(target_os = "linux")]
    fn ping_with(node: &str, cookie: &str) -> String {
        let probe = format!(
            "bt5185_probe_{}_{}",
            std::process::id(),
            rand::random::<u32>()
        );
        let out = beamtalk_workspace::erl_command()
            .args([
                "-noshell",
                "-hidden",
                "-sname",
                &probe,
                "-setcookie",
                cookie,
            ])
            .args(beamtalk_workspace::loopback_dist_args())
            .arg("-eval")
            .arg(format!(
                "io:format(\"~p\", [net_adm:ping('{node}')]), halt()."
            ))
            .output()
            .expect("run probe erl");
        String::from_utf8_lossy(&out.stdout).trim().to_string()
    }

    /// BT-5185, real node: while a node started the CLI way is still booting
    /// (its first `-eval` has not run), nobody can connect to it with a cookie
    /// read from its command line; the real cookie works, so distribution is
    /// up and the check is not vacuous.
    #[cfg(target_os = "linux")]
    #[test]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    fn argv_cookie_never_reaches_a_booting_node() {
        use std::process::Stdio;

        let runtime = beamtalk_cli::repl_startup::discover_beam_paths().unwrap();
        let cookie = crate::commands::workspace::storage::generate_cookie();
        let name = format!("bt5185_boot_{}", std::process::id());
        let node = format!("{name}@localhost");
        // Hold the node in its boot window: the restoring step comes late.
        let eval = format!("timer:sleep(20000), {RESTORE_HOME_EVAL}, halt().");
        let home = CookieHome::new(&cookie).unwrap();
        let mut cmd = erl_command();
        cmd.args(["-noshell", "-sname", &node])
            .args(beamtalk_workspace::loopback_dist_args())
            .args(beamtalk_cli::repl_startup::beam_pa_args(&runtime.paths))
            .arg("-eval")
            .arg(eval)
            .stdout(Stdio::null())
            .stderr(Stdio::null());
        home.apply(&mut cmd);
        let mut child = cmd.spawn().expect("spawn erl");
        let checked = std::panic::catch_unwind(|| {
            let argv = argv_cookies(child.id());
            // Wait until the node answers to any cookie we know of.
            let mut answered = Vec::new();
            for _ in 0..50 {
                answered = argv
                    .iter()
                    .chain(std::iter::once(&cookie))
                    .filter(|c| ping_with(&node, c) == "pong")
                    .cloned()
                    .collect();
                if !answered.is_empty() {
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(200));
            }
            for argv_cookie in &argv {
                assert!(
                    !answered.contains(argv_cookie),
                    "connected with the argv cookie {argv_cookie} during boot"
                );
            }
            assert!(
                answered.contains(&cookie),
                "the booting node never answered to the real cookie"
            );
        });
        let _ = child.kill();
        let _ = child.wait();
        checked.unwrap();
    }
}
