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
    template
        .replace("__RELEASE_NAME__", release_name)
        .replace("__RELEASE_VSN__", release_vsn)
        .replace("__ERTS_VERSION__", erts_version)
        .replace(
            "__INCLUDE_ERTS__",
            if include_erts { "true" } else { "false" },
        )
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
}
