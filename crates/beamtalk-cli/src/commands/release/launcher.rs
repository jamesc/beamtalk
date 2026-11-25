// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `bin/<name>` (POSIX `sh`) and `bin/<name>.cmd` (Windows) — the release's
//! operator interface (ADR 0125 §1.6/§1.7, BT-3573).
//!
//! Generated from the static templates in `templates/launcher.sh` and
//! `templates/launcher.cmd` by substituting the handful of values only
//! `beamtalk release` knows at build time (`__RELEASE_NAME__`,
//! `__RELEASE_VSN__`, `__ERTS_VERSION__`, `__INCLUDE_ERTS__`) — every other
//! path in the templates is resolved by the script itself, relative to its
//! own location, so neither template hardcodes an absolute path.
//!
//! The verbs (`foreground`/`stop`/`ping`/`remote_console`/`eval`/`rpc`/
//! `version`) dispatch into `beamtalk_release_launcher` (in
//! `beamtalk_workspace`) for everything that touches Beamtalk code — the
//! shared `beamtalk_repl_eval:dispatch_sync/3` core the WebSocket
//! `run-entry` op also runs — so the scripts themselves do only
//! process/argument wiring.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};

use crate::commands::cookie_env;

const LAUNCHER_SH_TEMPLATE: &str = include_str!("templates/launcher.sh");
const LAUNCHER_CMD_TEMPLATE: &str = include_str!("templates/launcher.cmd");

/// Write `bin/<name>` and `bin/<name>.cmd` into `release_dir`, returning
/// their paths. `erts_version` is only substituted, never read, when
/// `include_erts` is `false` — the script probes for a bundled ERTS at
/// runtime and falls back to the host `erl`/`erl.exe` either way, so a stale
/// `erts_version` in a `--no-include-erts` build is harmless.
///
/// # Errors
///
/// Returns an error if the `bin/` directory or either script cannot be
/// written, or (POSIX only) if the executable bit cannot be set.
pub fn write_launcher_scripts(
    release_dir: &Utf8Path,
    release_name: &str,
    release_vsn: &str,
    erts_version: &str,
    include_erts: bool,
) -> Result<(Utf8PathBuf, Utf8PathBuf)> {
    let bin_dir = release_dir.join("bin");
    std::fs::create_dir_all(bin_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create '{bin_dir}'"))?;

    let sh_content = render_template(
        LAUNCHER_SH_TEMPLATE,
        release_name,
        release_vsn,
        erts_version,
        include_erts,
    );
    let cmd_content = render_template(
        LAUNCHER_CMD_TEMPLATE,
        release_name,
        release_vsn,
        erts_version,
        include_erts,
    );

    let sh_path = bin_dir.join(release_name);
    std::fs::write(sh_path.as_std_path(), sh_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{sh_path}'"))?;
    set_executable(&sh_path)?;

    let cmd_path = bin_dir.join(format!("{release_name}.cmd"));
    // `\r\n` line endings: the `.cmd` template embeds Windows batch escape
    // sequences (`^`) that are line-ending sensitive under `cmd.exe`, and
    // this file is only ever read by `cmd.exe`.
    let cmd_content_crlf = to_crlf(&cmd_content);
    std::fs::write(cmd_path.as_std_path(), cmd_content_crlf)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{cmd_path}'"))?;

    Ok((sh_path, cmd_path))
}

fn render_template(
    template: &str,
    release_name: &str,
    release_vsn: &str,
    erts_version: &str,
    include_erts: bool,
) -> String {
    // A single left-to-right pass over `template`, substituting each
    // placeholder as it's found and resuming the scan *after* the
    // substituted value — never chained `.replace()` calls, each of which
    // would re-scan the *whole* string (including text a previous call just
    // substituted in). `[release] name`/`[package] version` are validated
    // (`validate_release_path_component`) but that only rejects path
    // separators/whitespace/control characters — a value that happens to
    // spell another placeholder verbatim (e.g. `[release] name =
    // "__RELEASE_VSN__"`) would otherwise have its own already-substituted
    // occurrence corrupted by a later `.replace()` in the chain.
    let placeholders: [(&str, &str); 9] = [
        // A literal TAB for `launcher.cmd`'s `RELEASE_COOKIE` check (BT-5107).
        ("__TAB__", "\t"),
        // The cookie handoff names come from `cookie_env`, the one source
        // for the CLI's own nodes too (BT-5126, BT-5185).
        ("__COOKIE_HOME_ENV__", cookie_env::COOKIE_HOME_ENV),
        ("__ORIG_PREFIX__", cookie_env::ORIG_PREFIX),
        ("__RESTORE_HOME_EVAL__", cookie_env::RESTORE_HOME_EVAL),
        ("__COOKIE_DIR_PREFIX__", cookie_env::COOKIE_DIR_PREFIX),
        ("__RELEASE_NAME__", release_name),
        ("__RELEASE_VSN__", release_vsn),
        ("__ERTS_VERSION__", erts_version),
        (
            "__INCLUDE_ERTS__",
            if include_erts { "true" } else { "false" },
        ),
    ];

    let mut out = String::with_capacity(template.len());
    let mut rest = template;
    'scan: while !rest.is_empty() {
        for (placeholder, value) in &placeholders {
            if let Some(after) = rest.strip_prefix(placeholder) {
                out.push_str(value);
                rest = after;
                continue 'scan;
            }
        }
        // No placeholder matches at this position — copy one char forward.
        // `char_indices` gives the byte length of the char at position 0 so
        // this stays UTF-8-safe (`vm.args`/error text can carry non-ASCII).
        let ch_len = rest.chars().next().map_or(1, char::len_utf8);
        out.push_str(&rest[..ch_len]);
        rest = &rest[ch_len..];
    }
    out
}

/// Normalise `content` to `\r\n` line endings — first strip any `\r` that
/// might already be present (so this is idempotent regardless of how the
/// template file itself is checked out / line-ending-normalised by git),
/// then re-insert `\r` before every `\n`.
fn to_crlf(content: &str) -> String {
    content.replace("\r\n", "\n").replace('\n', "\r\n")
}

#[cfg(unix)]
fn set_executable(path: &Utf8Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat '{path}'"))?
        .permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(path.as_std_path(), perms)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to set the executable bit on '{path}'"))
}

#[cfg(not(unix))]
#[allow(clippy::unnecessary_wraps)] // shares its signature with the `unix` fallible variant above
fn set_executable(_path: &Utf8Path) -> Result<()> {
    // Windows has no POSIX executable bit; `bin/<name>.cmd` is directly
    // runnable and `bin/<name>` is only ever invoked on a POSIX host.
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn write_scripts(include_erts: bool) -> (Utf8PathBuf, TempDir) {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        write_launcher_scripts(&root, "orders", "1.4.0", "16.0.2", include_erts).unwrap();
        (root, temp)
    }

    #[test]
    fn write_launcher_scripts_creates_both_files() {
        let (root, _temp) = write_scripts(true);
        assert!(root.join("bin/orders").is_file());
        assert!(root.join("bin/orders.cmd").is_file());
    }

    #[test]
    fn write_launcher_scripts_substitutes_placeholders() {
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        assert!(sh.contains("RELEASE_NAME=\"orders\""), "{sh}");
        assert!(sh.contains("RELEASE_VSN=\"1.4.0\""), "{sh}");
        assert!(sh.contains("ERTS_VSN=\"16.0.2\""), "{sh}");
        assert!(sh.contains("INCLUDE_ERTS=\"true\""), "{sh}");
        assert!(!sh.contains("__RELEASE_NAME__"), "{sh}");
        assert!(!sh.contains("__RELEASE_VSN__"), "{sh}");
        assert!(!sh.contains("__ERTS_VERSION__"), "{sh}");
        assert!(!sh.contains("__INCLUDE_ERTS__"), "{sh}");

        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        assert!(cmd.contains("RELEASE_NAME=orders"), "{cmd}");
        assert!(!cmd.contains("__RELEASE_NAME__"), "{cmd}");
    }

    /// A `[release] name`/`[package] version` value that happens to spell
    /// another placeholder verbatim must not have its own already-substituted
    /// occurrence corrupted by a later substitution — see `render_template`'s
    /// single-pass-scan doc comment for why chained `.replace()` calls were
    /// unsafe here.
    #[test]
    fn render_template_is_safe_when_a_value_spells_another_placeholder() {
        let sh = render_template(
            LAUNCHER_SH_TEMPLATE,
            "__RELEASE_VSN__",
            "1.4.0",
            "16.0.2",
            true,
        );
        assert!(
            sh.contains("RELEASE_NAME=\"__RELEASE_VSN__\""),
            "the release name's literal value must survive verbatim: {sh}"
        );
        assert!(
            sh.contains("RELEASE_VSN=\"1.4.0\""),
            "the actual version placeholder must still substitute correctly: {sh}"
        );
    }

    #[test]
    fn write_launcher_scripts_no_include_erts_substitutes_false() {
        let (root, _temp) = write_scripts(false);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        assert!(sh.contains("INCLUDE_ERTS=\"false\""), "{sh}");
    }

    #[test]
    fn write_launcher_scripts_no_hardcoded_absolute_paths() {
        // Neither script may embed the *host build machine's* absolute
        // release directory — everything resolves relative to the script's
        // own location at runtime (ADR 0125's own acceptance criterion).
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        assert!(
            !sh.contains(root.as_str()),
            "launcher.sh must not embed the build-time release dir: {sh}"
        );
        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        assert!(
            !cmd.contains(root.as_str()),
            "launcher.cmd must not embed the build-time release dir: {cmd}"
        );
    }

    /// BT-5127: every client verb that starts a distributed node pins its
    /// listener to loopback (the foreground node gets it from `vm.args`).
    #[test]
    fn write_launcher_scripts_pin_client_nodes_to_loopback() {
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        assert_eq!(sh.matches("-hidden -sname").count(), 4);
        assert_eq!(
            sh.matches(
                "-hidden -sname \\\"\\$SNAME\\\" -kernel inet_dist_use_interface '{127,0,0,1}'"
            )
            .count(),
            4,
            "launcher.sh client verbs must pin loopback: {sh}"
        );
        assert_eq!(cmd.matches("-hidden -sname").count(), 4);
        assert_eq!(
            cmd.matches("-kernel inet_dist_use_interface \"{127,0,0,1}\"")
                .count(),
            4,
            "launcher.cmd client verbs must pin loopback: {cmd}"
        );
    }

    /// BT-5233 (ADR 0126 amendment D2): under `RELEASE_DISTRIBUTION=tls`
    /// every verb that starts a distributed node runs the Erlang-side check
    /// first, then uses `-name`, `-proto_dist`, the option file and the
    /// checked interface, in both launchers.
    #[test]
    fn write_launcher_scripts_follow_the_tls_opt_in() {
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        for (name, script) in [("launcher.sh", &sh), ("launcher.cmd", &cmd)] {
            assert!(
                script.contains("RELEASE_DISTRIBUTION")
                    && script.contains("beamtalk_release_launcher:tls_preflight_main()"),
                "{name} must check the TLS opt-in: {script}"
            );
            assert!(
                script.contains("plain TCP off-host is not offered"),
                "{name}: {script}"
            );
        }
        // foreground + stop/ping/remote_console/rpc
        assert_eq!(
            sh.matches("\n        select_distribution\n").count(),
            5,
            "launcher.sh: every distributed verb runs the check: {sh}"
        );
        assert_eq!(
            cmd.matches("call :select_distribution\r\nif errorlevel 1 exit /b !errorlevel!")
                .count(),
            5,
            "launcher.cmd: every distributed verb runs the check: {cmd}"
        );
        assert_eq!(
            cmd.matches(
                "-hidden -name \"%SNAME%@!TLS_HOST!\" -proto_dist !TLS_PROTO! \
                 -ssl_dist_optfile \"!TLS_OPTFILE!\" -kernel inet_dist_use_interface \"!TLS_INTERFACE!\""
            )
            .count(),
            4,
            "launcher.cmd client verbs must follow the TLS opt-in: {cmd}"
        );
        assert!(
            cmd.contains(
                "-name \"%THIS_NODE%\" -proto_dist !TLS_PROTO! -ssl_dist_optfile \"!TLS_OPTFILE!\""
            ),
            "launcher.cmd foreground must follow the TLS opt-in: {cmd}"
        );
        assert!(
            cmd.contains("set \"ERL_EPMD_ADDRESS=!TLS_EPMD!\""),
            "launcher.cmd: epmd follows the interface: {cmd}"
        );
    }

    #[test]
    fn write_launcher_scripts_lists_every_verb() {
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        for verb in [
            "foreground",
            "stop",
            "ping",
            "remote_console",
            "eval",
            "rpc",
            "version",
        ] {
            assert!(sh.contains(verb), "launcher.sh missing verb '{verb}': {sh}");
        }
        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        for verb in [
            "foreground",
            "stop",
            "ping",
            "remote_console",
            "eval",
            "rpc",
            "version",
        ] {
            assert!(
                cmd.contains(verb),
                "launcher.cmd missing verb '{verb}': {cmd}"
            );
        }
    }

    /// The exact bug this guards against: `-sname` used to be baked into
    /// `vm.args` at build time — `erl` keeps the *first* `-sname` it sees
    /// and warns on a duplicate, so nothing the launcher passed afterwards
    /// could ever override it, making it impossible to run two instances
    /// of the same release on one host (ADR 0126 §9's "development,
    /// testing" same-host clustering). Both launchers must compute
    /// `-sname` themselves from `RELEASE_NODE` (falling back to the
    /// release name), and `foreground` must actually pass it on the `erl`
    /// command line — not just the client verbs.
    #[test]
    fn write_launcher_scripts_support_release_node_override() {
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        assert!(
            sh.contains("RELEASE_NODE"),
            "launcher.sh must read RELEASE_NODE: {sh}"
        );
        assert!(
            sh.contains("-sname \"$(node_sname)\""),
            "launcher.sh's foreground verb must pass -sname on the erl command line: {sh}"
        );

        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        assert!(
            cmd.contains("RELEASE_NODE"),
            "launcher.cmd must read RELEASE_NODE: {cmd}"
        );
        assert!(
            cmd.contains("-sname \"%THIS_NODE%\""),
            "launcher.cmd's foreground verb must pass -sname on the erl command line: {cmd}"
        );
    }

    /// The exact bug this guards against: a malformed `RELEASE_NODE`
    /// (verified live to contain e.g. `/`) reached `erl` unvalidated and
    /// crashed the whole node with a raw kernel crash dump ("Invalid node
    /// name!") instead of a clean, actionable error. Both launchers must
    /// reject it up front, before ever invoking `erl`/`erl.exe`.
    #[test]
    fn write_launcher_scripts_validate_release_node_before_invoking_erl() {
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        assert!(
            sh.contains("RELEASE_NODE") && sh.contains("not a valid instance name"),
            "launcher.sh must validate RELEASE_NODE: {sh}"
        );
        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        assert!(
            cmd.contains("RELEASE_NODE") && cmd.contains("not a valid instance name"),
            "launcher.cmd must validate RELEASE_NODE: {cmd}"
        );
    }

    /// Text-substring assertions (the test above) pass regardless of
    /// whether the validation logic actually *works* — exactly the false
    /// confidence that let a command-injection bug ship in `launcher.cmd`'s
    /// first version of this check (an unquoted `%RELEASE_NODE%` piped
    /// through `findstr`, exploitable via `RELEASE_NODE=x & calc.exe`).
    /// This test actually executes the generated `launcher.sh` with a
    /// malicious `RELEASE_NODE` and asserts it is rejected cleanly —
    /// `#[cfg(unix)]`, like its sibling `write_launcher_scripts_sh_is_executable`
    /// just below: `sh` isn't reliably resolvable the same way on the
    /// Windows CI runner `just test-rust` also runs on. There is no way to
    /// execute `.cmd` from this (POSIX) suite either, which is exactly why
    /// the injection bug lived in the `.cmd`-specific code path unexercised
    /// until manual review.
    #[cfg(unix)]
    #[test]
    fn write_launcher_scripts_sh_rejects_malicious_release_node_at_runtime() {
        let (root, _temp) = write_scripts(true);
        let sh_path = root.join("bin/orders");
        let marker_dir = TempDir::new().unwrap();
        let marker_path = marker_dir.path().join("beamtalk_launcher_test_pwned");

        let output = std::process::Command::new("sh")
            .arg(sh_path.as_std_path())
            .arg("foreground")
            .env(
                "RELEASE_NODE",
                format!("x; touch {}", marker_path.display()),
            )
            .env("RELEASE_COOKIE", "test-cookie")
            .output()
            .expect("failed to execute launcher.sh");

        assert_eq!(
            output.status.code(),
            Some(2),
            "malicious RELEASE_NODE must be rejected with exit code 2: stdout={} stderr={}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("not a valid instance name"),
            "stderr: {stderr}"
        );
        assert!(
            !marker_path.exists(),
            "shell metacharacters in RELEASE_NODE must never be executed"
        );
    }

    /// BT-5077: `launcher.cmd` must reject a `RELEASE_COOKIE` containing
    /// whitespace, quotes or backslashes, like `launcher.sh` does — otherwise
    /// a cookie with a space is split into two words. Text
    /// inspection only: there is no Windows runner for this test, so the
    /// check itself is exercised only by reading the script.
    #[test]
    fn write_launcher_cmd_rejects_unsafe_cookie_characters() {
        let (root, _temp) = write_scripts(true);
        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        for stripped in [
            r#"set "CK_CHECK=!CK_CHECK: =!""#,
            r#"set "CK_CHECK=!CK_CHECK:"=!""#,
            r#"set "CK_CHECK=!CK_CHECK:'=!""#,
            r#"set "CK_CHECK=!CK_CHECK:\=!""#,
        ] {
            assert!(cmd.contains(stripped), "launcher.cmd must strip {stripped}");
        }
        // `export_cookie` writes it with `set /p "=..."`, which writes nothing
        // for a leading `=`: reject that up front with a clear message.
        assert!(
            cmd.contains(r#"if "!RELEASE_COOKIE:~0,1!"=="=" goto :cookie_equals"#),
            "launcher.cmd must reject a leading '='"
        );
        let reject = cmd
            .find("RELEASE_COOKIE must not contain whitespace, quotes or backslashes")
            .expect("launcher.cmd must print a cookie rejection");
        let verb = cmd.find("set \"VERB=%~1\"").unwrap();
        let export = cmd.find("\n:export_cookie").unwrap();
        assert!(
            reject < verb && verb < export,
            "the cookie check must run before any verb dispatches"
        );
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        assert!(sh.contains("RELEASE_COOKIE must not contain whitespace, quotes or backslashes"));
    }

    /// BT-5126 / BT-5185: no verb may put a cookie on the `erl` command line,
    /// in `ERL_AFLAGS` or in an `-args_file` (all of them expand into the
    /// BEAM's argv): not the real one and not a placeholder. The cookie goes
    /// in a private cookie home; text inspection of the scripts here, a real
    /// node's `/proc/<pid>/cmdline` in `tests/cli/cli_release.rs`.
    #[test]
    fn write_launcher_scripts_boot_erl_from_a_private_cookie_home() {
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        for (name, script) in [("launcher.sh", &sh), ("launcher.cmd", &cmd)] {
            assert!(
                !script.contains("-setcookie"),
                "{name} must not put any cookie in argv"
            );
            assert!(
                !script.contains("ERL_AFLAGS="),
                "{name} must not put the cookie in ERL_AFLAGS"
            );
            for needed in [
                cookie_env::COOKIE_HOME_ENV,
                cookie_env::ORIG_PREFIX,
                cookie_env::RESTORE_HOME_EVAL,
                cookie_env::COOKIE_DIR_PREFIX,
            ] {
                assert!(script.contains(needed), "{name} must use {needed}");
            }
            for placeholder in [
                "__COOKIE_HOME_ENV__",
                "__ORIG_PREFIX__",
                "__RESTORE_HOME_EVAL__",
                "__COOKIE_DIR_PREFIX__",
            ] {
                assert!(!script.contains(placeholder), "{name}: {placeholder}");
            }
        }
        assert!(
            cmd.contains("set \"RELEASE_COOKIE=\""),
            "launcher.cmd must clear it"
        );
        assert!(
            sh.contains("unset RELEASE_COOKIE"),
            "launcher.sh must clear it"
        );
    }

    /// BT-5067 / BT-5126 / BT-5185: run each client verb against a fake `erl`
    /// from a release root containing a space. Every `-pa` path survives as
    /// one argument; no cookie is an argument, in `ERL_AFLAGS` or in the
    /// environment (`RELEASE_COOKIE` is cleared, so epmd and every other child
    /// inherits nothing); `erl` boots with `HOME` set to a private (0700)
    /// directory whose `.erlang.cookie` (0400) holds the cookie, and the
    /// original `HOME` is recorded for the node to restore.
    #[cfg(unix)]
    #[test]
    fn write_launcher_scripts_sh_handles_spaced_root_and_hides_cookie() {
        use std::os::unix::fs::PermissionsExt;
        let temp = TempDir::new().unwrap();
        let base = Utf8PathBuf::from_path_buf(temp.path().join("My App")).unwrap();
        write_launcher_scripts(&base, "orders", "1.0.0", "14.0", false).unwrap();
        std::fs::create_dir_all(base.join("lib/dep one/ebin").as_std_path()).unwrap();
        let fake = temp.path().join("fakebin");
        std::fs::create_dir_all(&fake).unwrap();
        let erl = fake.join("erl");
        std::fs::write(
            &erl,
            "#!/bin/sh\nfor a; do echo \"ARG[$a]\"; done\necho \"AFLAGS=$ERL_AFLAGS\"\n\
             echo \"RELEASECOOKIE=$RELEASE_COOKIE\"\necho \"HOME=$HOME\"\n\
             echo \"COOKIEHOME=$BEAMTALK_COOKIE_HOME\"\necho \"ORIG=$BEAMTALK_ORIG_HOME\"\n\
             echo \"FILE=$(cat \"$HOME/.erlang.cookie\")\"\n\
             echo \"DIRMODE=$(stat -c %a \"$HOME\" 2>/dev/null || stat -f %Lp \"$HOME\")\"\n\
             echo \"FILEMODE=$(stat -c %a \"$HOME/.erlang.cookie\" 2>/dev/null || stat -f %Lp \"$HOME/.erlang.cookie\")\"\n",
        )
        .unwrap();
        std::fs::set_permissions(&erl, std::fs::Permissions::from_mode(0o755)).unwrap();
        let path = format!("{}:{}", fake.display(), std::env::var("PATH").unwrap());
        let tmpdir = temp.path().join("tmp");
        std::fs::create_dir_all(&tmpdir).unwrap();

        for verb in ["ping", "stop"] {
            let out = std::process::Command::new("sh")
                .arg(base.join("bin/orders").as_std_path())
                .arg(verb)
                .env("PATH", &path)
                .env("HOME", "/the/real/home")
                .env("TMPDIR", &tmpdir)
                .env("RELEASE_COOKIE", "s3cr3t-cookie")
                .output()
                .unwrap();
            let stdout = String::from_utf8_lossy(&out.stdout);
            assert!(out.status.success(), "{verb}: {stdout}");
            assert!(
                stdout.contains("ARG[-pa]\nARG[") && stdout.contains("My App/lib/dep one/ebin]"),
                "{verb}: -pa path split on space: {stdout}"
            );
            let args: String = stdout
                .lines()
                .filter(|l| l.starts_with("ARG["))
                .collect::<Vec<_>>()
                .join("\n");
            assert!(!args.contains("s3cr3t"), "{verb}: {args}");
            assert!(!args.contains("-setcookie"), "{verb}: {args}");
            assert!(stdout.contains("AFLAGS=\n"), "{verb}: {stdout}");
            assert!(stdout.contains("RELEASECOOKIE=\n"), "{verb}: {stdout}");
            assert!(stdout.contains("FILE=s3cr3t-cookie\n"), "{verb}: {stdout}");
            assert!(
                stdout.contains("ORIG=set:/the/real/home\n"),
                "{verb}: {stdout}"
            );
            let home = stdout
                .lines()
                .find_map(|l| l.strip_prefix("HOME="))
                .unwrap()
                .to_string();
            assert!(
                stdout.contains(&format!("COOKIEHOME={home}\n")),
                "{verb}: {stdout}"
            );
            assert!(stdout.contains("DIRMODE=700\n"), "{verb}: {stdout}");
            assert!(stdout.contains("FILEMODE=400\n"), "{verb}: {stdout}");
            // The launcher removes the private home once erl exits, even
            // though this fake erl never ran the node's restore step.
            assert!(
                !std::path::Path::new(&home).exists(),
                "{verb}: private cookie home left behind"
            );
        }
    }

    #[test]
    fn write_launcher_scripts_cmd_uses_crlf_line_endings() {
        let (root, _temp) = write_scripts(true);
        let cmd_bytes = std::fs::read(root.join("bin/orders.cmd").as_std_path()).unwrap();
        let cmd = String::from_utf8(cmd_bytes).unwrap();
        assert!(cmd.contains("\r\n"), "expected CRLF line endings");
        // Every `\n` must be preceded by `\r` — no bare `\n` slipped through.
        assert!(
            !cmd.replace("\r\n", "").contains('\n'),
            "found a bare \\n not preceded by \\r"
        );
    }

    /// BT-5107: `launcher.cmd` rejects a TAB in `RELEASE_COOKIE` like
    /// `launcher.sh` does — the generated script carries a real TAB (never
    /// the placeholder) and strips it in the cookie check. Not executed on
    /// Windows from this suite (no `cmd.exe` here); the text is asserted.
    #[test]
    fn write_launcher_scripts_cmd_checks_tab_in_cookie() {
        let (root, _temp) = write_scripts(true);
        let cmd = std::fs::read_to_string(root.join("bin/orders.cmd").as_std_path()).unwrap();
        assert!(!cmd.contains("__TAB__"), "placeholder must be substituted");
        assert!(
            cmd.contains("set \"TAB=\t\"\r\n"),
            "TAB variable holds a real TAB"
        );
        assert!(cmd.contains("set \"CK_CHECK=!CK_CHECK:%TAB%=!\""));
        assert!(
            !LAUNCHER_CMD_TEMPLATE.contains('\t'),
            "the template itself must not contain a literal TAB"
        );
    }

    #[cfg(unix)]
    #[test]
    fn write_launcher_scripts_sh_is_executable() {
        use std::os::unix::fs::PermissionsExt;
        let (root, _temp) = write_scripts(true);
        let mode = std::fs::metadata(root.join("bin/orders").as_std_path())
            .unwrap()
            .permissions()
            .mode();
        assert_eq!(mode & 0o777, 0o755);
    }

    #[test]
    fn write_launcher_scripts_boot_check_gated_on_host_erts_only() {
        // ADR 0125 §3.2: the OTP-major boot check only applies when running
        // on a host ERTS, never when the release bundles its own.
        let (root, _temp) = write_scripts(true);
        let sh = std::fs::read_to_string(root.join("bin/orders").as_std_path()).unwrap();
        assert!(sh.contains("USING_HOST_ERTS"), "{sh}");
        assert!(
            sh.contains("required_otp") || sh.contains("check_otp_window"),
            "{sh}"
        );
    }

    /// BT-5134: a cookie starting with `-`/`+` is rejected, and a symlinked
    /// `bin/<name>` still finds its release root.
    #[test]
    #[cfg(unix)]
    fn launcher_sh_rejects_dash_cookie_and_follows_symlinked_bin() {
        use std::os::unix::fs::PermissionsExt;
        let temp = TempDir::new().unwrap();
        let base = Utf8PathBuf::from_path_buf(temp.path().join("rel")).unwrap();
        write_launcher_scripts(&base, "orders", "1.0.0", "14.0", false).unwrap();
        std::fs::create_dir_all(base.join("lib/dep/ebin").as_std_path()).unwrap();
        let fake = temp.path().join("fakebin");
        std::fs::create_dir_all(&fake).unwrap();
        let erl = fake.join("erl");
        std::fs::write(&erl, "#!/bin/sh\nfor a; do echo \"ARG[$a]\"; done\n").unwrap();
        std::fs::set_permissions(&erl, std::fs::Permissions::from_mode(0o755)).unwrap();
        let path = format!("{}:{}", fake.display(), std::env::var("PATH").unwrap());

        for bad in ["-sname", "+x"] {
            let out = std::process::Command::new("sh")
                .arg(base.join("bin/orders").as_std_path())
                .arg("ping")
                .env("PATH", &path)
                .env("RELEASE_COOKIE", bad)
                .output()
                .unwrap();
            assert_eq!(out.status.code(), Some(2), "{bad}");
            assert!(String::from_utf8_lossy(&out.stderr).contains("must not start with"));
        }

        let links = temp.path().join("links");
        std::fs::create_dir_all(&links).unwrap();
        let link = links.join("orders");
        std::os::unix::fs::symlink(base.join("bin/orders").as_std_path(), &link).unwrap();
        let out = std::process::Command::new("sh")
            .arg(&link)
            .arg("ping")
            .env("PATH", &path)
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert!(
            out.status.success(),
            "{stdout}{}",
            String::from_utf8_lossy(&out.stderr)
        );
        assert!(
            stdout.contains("/rel/"),
            "root must resolve through the symlink: {stdout}"
        );
    }

    /// A fake `erl` for the TLS opt-in tests: prints each argument and the
    /// `ERL_EPMD_ADDRESS` it was started with, and answers the
    /// `tls_preflight_main` check with `$FAKE_PREFLIGHT` (exit
    /// `$FAKE_PREFLIGHT_RC`).
    #[cfg(unix)]
    fn fake_tls_erl(dir: &std::path::Path) -> String {
        use std::os::unix::fs::PermissionsExt;
        let fake = dir.join("fakebin");
        std::fs::create_dir_all(&fake).unwrap();
        let erl = fake.join("erl");
        std::fs::write(
            &erl,
            "#!/bin/sh\n\
             case \"$*\" in *tls_preflight_main*)\n\
               printf '%s' \"$FAKE_PREFLIGHT\"; echo 'refused: details' >&2; exit \"${FAKE_PREFLIGHT_RC:-0}\" ;;\n\
             esac\n\
             for a; do echo \"ARG[$a]\"; done\necho \"EPMD=$ERL_EPMD_ADDRESS\"\n",
        )
        .unwrap();
        std::fs::set_permissions(&erl, std::fs::Permissions::from_mode(0o755)).unwrap();
        format!("{}:{}", fake.display(), std::env::var("PATH").unwrap())
    }

    /// BT-5233: with `RELEASE_DISTRIBUTION=tls`, the command lines `launcher.sh`
    /// generates for the node and every client verb carry `-name
    /// <instance>@RELEASE_HOST`, `-proto_dist`, the option file and the
    /// checked interface, never the loopback `-sname`, and epmd follows the
    /// interface. The values come from the Erlang-side check, faked here.
    #[cfg(unix)]
    #[test]
    fn launcher_sh_tls_opt_in_command_lines() {
        let temp = TempDir::new().unwrap();
        let base = Utf8PathBuf::from_path_buf(temp.path().join("rel")).unwrap();
        write_launcher_scripts(&base, "orders", "1.0.0", "14.0", false).unwrap();
        std::fs::create_dir_all(base.join("lib/dep/ebin").as_std_path()).unwrap();
        let path = fake_tls_erl(temp.path());
        let preflight = "tls-proto=inet_tls\ntls-interface={10,0,0,5}\ntls-epmd=10.0.0.5\n\
                         tls-host=node1.example.com\ntls-optfile=/etc/my app/ssl_dist.conf\n";
        let run = |verb: &str| {
            let out = std::process::Command::new("sh")
                .arg(base.join("bin/orders").as_std_path())
                .args([verb, "Smoke run"])
                .env("PATH", &path)
                .env("RELEASE_DISTRIBUTION", "tls")
                .env("RELEASE_NODE", "web1")
                .env("FAKE_PREFLIGHT", preflight)
                .env_remove("RELEASE_COOKIE")
                .output()
                .unwrap();
            let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
            assert!(
                out.status.success(),
                "{verb}: {stdout}{}",
                String::from_utf8_lossy(&out.stderr)
            );
            stdout
        };
        for verb in ["foreground", "stop", "ping", "remote_console", "rpc"] {
            let out = run(verb);
            let args: Vec<&str> = out
                .lines()
                .filter_map(|l| l.strip_prefix("ARG[")?.strip_suffix(']'))
                .collect();
            let after = |flag: &str| {
                args.iter()
                    .position(|a| *a == flag)
                    .and_then(|i| args.get(i + 1).copied())
                    .unwrap_or_else(|| panic!("{verb}: no {flag} in {args:?}"))
            };
            assert!(!args.contains(&"-sname"), "{verb}: {args:?}");
            assert_eq!(after("-proto_dist"), "inet_tls", "{verb}");
            assert_eq!(
                after("-ssl_dist_optfile"),
                "/etc/my app/ssl_dist.conf",
                "{verb}"
            );
            assert_eq!(after("inet_dist_use_interface"), "{10,0,0,5}", "{verb}");
            assert!(out.contains("EPMD=10.0.0.5\n"), "{verb}: {out}");
            let name = after("-name");
            if verb == "foreground" {
                assert_eq!(name, "web1@node1.example.com");
                // Ahead of `-args_file`: `-kernel` flags are first-wins.
                let iface = args.iter().position(|a| *a == "{10,0,0,5}").unwrap();
                let args_file = args.iter().position(|a| *a == "-args_file").unwrap();
                assert!(iface < args_file, "{args:?}");
            } else {
                assert!(args.contains(&"-hidden"), "{verb}: {args:?}");
                assert!(
                    name.starts_with("orders_") && name.ends_with("@node1.example.com"),
                    "{verb}: {name}"
                );
            }
            if verb == "remote_console" {
                assert!(
                    args.iter()
                        .any(|a| a.contains("{'web1@node1.example.com', {shell, start, []}}")),
                    "{args:?}"
                );
            } else if verb != "foreground" {
                assert!(args.contains(&"web1@node1.example.com"), "{verb}: {args:?}");
            }
        }
    }

    /// BT-5233: when the Erlang-side check refuses the opt-in, `launcher.sh`
    /// stops with its exit code before starting any node, and passes its
    /// explanation through; an unknown `RELEASE_DISTRIBUTION` is refused up
    /// front; and with no opt-in the check never runs.
    #[cfg(unix)]
    #[test]
    fn launcher_sh_refuses_a_rejected_tls_opt_in() {
        let temp = TempDir::new().unwrap();
        let base = Utf8PathBuf::from_path_buf(temp.path().join("rel")).unwrap();
        write_launcher_scripts(&base, "orders", "1.0.0", "14.0", false).unwrap();
        let path = fake_tls_erl(temp.path());
        for verb in ["foreground", "stop", "ping", "remote_console", "rpc"] {
            let out = std::process::Command::new("sh")
                .arg(base.join("bin/orders").as_std_path())
                .args([verb, "Smoke run"])
                .env("PATH", &path)
                .env("RELEASE_DISTRIBUTION", "tls")
                .env("FAKE_PREFLIGHT", "")
                .env("FAKE_PREFLIGHT_RC", "2")
                .output()
                .unwrap();
            assert_eq!(out.status.code(), Some(2), "{verb}");
            assert!(
                !String::from_utf8_lossy(&out.stdout).contains("ARG["),
                "{verb}: no node may start"
            );
            assert!(String::from_utf8_lossy(&out.stderr).contains("refused: details"));
        }

        let out = std::process::Command::new("sh")
            .arg(base.join("bin/orders").as_std_path())
            .arg("ping")
            .env("PATH", &path)
            .env("RELEASE_DISTRIBUTION", "tcp")
            .output()
            .unwrap();
        assert_eq!(out.status.code(), Some(2));
        assert!(String::from_utf8_lossy(&out.stderr).contains("plain TCP off-host is not offered"));
        assert!(!String::from_utf8_lossy(&out.stdout).contains("ARG["));
        // `version` starts no distribution, so it does not care.
        let out = std::process::Command::new("sh")
            .arg(base.join("bin/orders").as_std_path())
            .arg("version")
            .env("PATH", &path)
            .env("RELEASE_DISTRIBUTION", "tcp")
            .output()
            .unwrap();
        assert!(
            out.status.success(),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );

        // No opt-in: loopback, as before, and the check never runs.
        let out = std::process::Command::new("sh")
            .arg(base.join("bin/orders").as_std_path())
            .arg("ping")
            .env("PATH", &path)
            .env_remove("RELEASE_DISTRIBUTION")
            .env("FAKE_PREFLIGHT_RC", "2")
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert!(out.status.success(), "{stdout}");
        assert!(stdout.contains("ARG[-sname]"), "{stdout}");
        assert!(stdout.contains("ARG[{127,0,0,1}]"), "{stdout}");
        assert!(stdout.contains("EPMD=127.0.0.1\n"), "{stdout}");
        assert!(!stdout.contains("ARG[-proto_dist]"), "{stdout}");
    }

    /// BT-5187: a symlink cycle in `$0` ends with an error instead of
    /// looping forever. (`sh -c '. script' <name>` sets `$0` to the cycle;
    /// executing the cycle directly fails in the kernel before the script
    /// runs.)
    #[test]
    #[cfg(unix)]
    fn launcher_sh_symlink_resolution_is_bounded() {
        let temp = TempDir::new().unwrap();
        let base = Utf8PathBuf::from_path_buf(temp.path().join("rel")).unwrap();
        write_launcher_scripts(&base, "orders", "1.0.0", "14.0", false).unwrap();
        let a = temp.path().join("a");
        let b = temp.path().join("b");
        std::os::unix::fs::symlink(&b, &a).unwrap();
        std::os::unix::fs::symlink(&a, &b).unwrap();
        let mut child = std::process::Command::new("sh")
            .arg("-c")
            .arg(format!(". '{}'", base.join("bin/orders")))
            .arg(&a)
            .arg("version")
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .unwrap();
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(20);
        while child.try_wait().unwrap().is_none() {
            if std::time::Instant::now() > deadline {
                let _ = child.kill();
                let _ = child.wait();
                panic!("launcher.sh looped on a symlink cycle");
            }
            // sleep: polling a child process with a deadline.
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
        let out = child.wait_with_output().unwrap();
        assert_eq!(out.status.code(), Some(1));
        assert!(
            String::from_utf8_lossy(&out.stderr).contains("too many levels"),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}
