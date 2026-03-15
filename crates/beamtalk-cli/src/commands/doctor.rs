// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk doctor` — verify environment setup and report actionable diagnostics.
//!
//! Checks all prerequisites for running Beamtalk: Erlang/OTP version, `erlc`,
//! sysroot validity, stdlib module count, and runtime apps.
//!
//! With `--dev`, additionally checks developer toolchain: `rebar3`, `just`, `rustc`.

use std::path::{Path, PathBuf};
use std::process::Command;

use miette::Result;

use beamtalk_cli::repl_startup::{self, RuntimeLayout, beam_paths_for_layout, has_beam_files};

/// Run all doctor checks and print results.
///
/// When `dev` is true, additionally checks developer toolchain tools
/// (`rebar3`, `just`, `rustc`).
///
/// Returns `Ok(())` if all required checks pass, or an error if any fail.
pub fn run(dev: bool) -> Result<()> {
    let mut has_failure = false;
    let mut has_warning = false;

    // Resolve runtime once — stdlib and runtime-app checks reuse this.
    let runtime_info = repl_startup::find_runtime_dir_with_layout().ok();

    let mut checks = vec![
        check_erl(),
        check_erlc(),
        check_runtime(runtime_info.as_ref()),
        check_stdlib(runtime_info.as_ref()),
        check_runtime_apps(runtime_info.as_ref()),
    ];

    if dev {
        checks.push(check_tool("rebar3"));
        checks.push(check_tool("just"));
        checks.push(check_rustc());
    }

    for result in checks {
        match result {
            CheckResult::Pass(msg) => println!("\u{2705} {msg}"),
            CheckResult::Fail(msg) => {
                println!("\u{274c} {msg}");
                has_failure = true;
            }
            CheckResult::Warn(msg) => {
                println!("\u{26a0}\u{fe0f}  {msg}");
                has_warning = true;
            }
        }
    }

    // --- Summary ---
    println!();
    if has_failure {
        println!("Some checks failed. See above for details.");
        print_install_instructions();
        miette::bail!("doctor found problems that must be fixed");
    } else if has_warning {
        println!("All required checks passed (with warnings).");
        Ok(())
    } else {
        println!("All checks passed!");
        Ok(())
    }
}

/// Result of a single doctor check.
enum CheckResult {
    Pass(String),
    Fail(String),
    Warn(String),
}

/// Resolved runtime directory and layout, computed once and shared across checks.
type RuntimeInfo = (PathBuf, RuntimeLayout);

// ---------------------------------------------------------------------------
// Individual checks
// ---------------------------------------------------------------------------

/// Check that `erl` is on PATH and OTP version is 27+.
fn check_erl() -> CheckResult {
    let Some(erl_path) = which("erl") else {
        return CheckResult::Fail("erl not found on PATH".into());
    };

    match get_otp_version() {
        Some(version_str) => match parse_otp_major(&version_str) {
            Some(major) if major >= 27 => {
                CheckResult::Pass(format!("Erlang/OTP {version_str} ({erl_path})"))
            }
            Some(major) => CheckResult::Fail(format!(
                "Erlang/OTP {version_str} ({erl_path}) — version {major} is too old, need 27+"
            )),
            None => CheckResult::Warn(format!(
                "Erlang/OTP found ({erl_path}) but could not parse version: {version_str}"
            )),
        },
        None => CheckResult::Warn(format!(
            "erl found ({erl_path}) but could not determine OTP version"
        )),
    }
}

/// Check that `erlc` is on PATH.
fn check_erlc() -> CheckResult {
    match which("erlc") {
        Some(p) => CheckResult::Pass(format!("erlc found ({p})")),
        None => CheckResult::Fail("erlc not found on PATH".into()),
    }
}

/// Check that the Beamtalk runtime directory can be found.
fn check_runtime(info: Option<&RuntimeInfo>) -> CheckResult {
    let Some((path, layout)) = info else {
        return CheckResult::Fail(
            "Beamtalk runtime not found — run from the repo root or set BEAMTALK_RUNTIME_DIR"
                .into(),
        );
    };

    let label = match layout {
        RuntimeLayout::Dev => "dev",
        RuntimeLayout::Installed => "installed",
    };
    let display_path = path.canonicalize().unwrap_or_else(|_| path.clone());
    CheckResult::Pass(format!(
        "Beamtalk runtime: {} ({label})",
        display_path.display()
    ))
}

/// Count stdlib `.beam` modules.
fn check_stdlib(info: Option<&RuntimeInfo>) -> CheckResult {
    let Some((path, layout)) = info else {
        return CheckResult::Fail("Stdlib: cannot check (runtime not found)".into());
    };

    let beam_paths = beam_paths_for_layout(path, *layout);
    let count = count_beam_files(&beam_paths.stdlib_ebin);

    if count > 0 {
        CheckResult::Pass(format!("Stdlib: {count} modules"))
    } else {
        CheckResult::Fail(format!(
            "Stdlib: no .beam files in {}",
            beam_paths.stdlib_ebin.display()
        ))
    }
}

/// Check that key runtime OTP apps have .beam files.
fn check_runtime_apps(info: Option<&RuntimeInfo>) -> CheckResult {
    let Some((path, layout)) = info else {
        return CheckResult::Fail("Runtime apps: cannot check (runtime not found)".into());
    };

    let beam_paths = beam_paths_for_layout(path, *layout);
    let mut present = Vec::new();
    let mut missing = Vec::new();

    for (name, ebin) in [
        ("beamtalk_runtime", &beam_paths.runtime_ebin),
        ("beamtalk_workspace", &beam_paths.workspace_ebin),
        ("beamtalk_compiler", &beam_paths.compiler_ebin),
    ] {
        if has_beam_files(ebin) {
            present.push(name);
        } else {
            missing.push(name);
        }
    }

    if missing.is_empty() {
        CheckResult::Pass(format!("Runtime: {}", present.join(", ")))
    } else {
        CheckResult::Fail(format!("Runtime: missing apps: {}", missing.join(", ")))
    }
}

// ---------------------------------------------------------------------------
// Dev-only checks
// ---------------------------------------------------------------------------

/// Check that a named tool is on PATH (used for dev toolchain checks).
fn check_tool(name: &str) -> CheckResult {
    match which(name) {
        Some(p) => CheckResult::Pass(format!("{name} found ({p})")),
        None => CheckResult::Fail(format!("{name} not found on PATH")),
    }
}

/// Check that `rustc` is on PATH and report its version.
fn check_rustc() -> CheckResult {
    let Some(rustc_path) = which("rustc") else {
        return CheckResult::Fail("rustc not found on PATH".into());
    };

    let version = Command::new("rustc")
        .arg("--version")
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string());

    match version {
        Some(v) => CheckResult::Pass(format!("{v} ({rustc_path})")),
        None => CheckResult::Warn(format!("rustc found ({rustc_path}) but could not get version")),
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Find an executable on PATH, returning its full path as a string.
fn which(name: &str) -> Option<String> {
    let cmd = if cfg!(windows) { "where" } else { "which" };
    Command::new(cmd)
        .arg(name)
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| {
            String::from_utf8_lossy(&o.stdout)
                .lines()
                .next()
                .unwrap_or("")
                .trim()
                .to_string()
        })
        .filter(|s| !s.is_empty())
}

/// Query the running OTP release version string (e.g. "27").
fn get_otp_version() -> Option<String> {
    let output = Command::new("erl")
        .args([
            "-noshell",
            "-eval",
            "io:format(\"~s\", [erlang:system_info(otp_release)]), halt().",
        ])
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if version.is_empty() {
        None
    } else {
        Some(version)
    }
}

/// Parse the major version number from an OTP release string like "27" or "27.2".
fn parse_otp_major(version: &str) -> Option<u32> {
    version.split('.').next()?.parse().ok()
}

/// Count `.beam` files in a directory.
fn count_beam_files(dir: &Path) -> usize {
    std::fs::read_dir(dir)
        .map(|entries| {
            entries
                .flatten()
                .filter(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
                .count()
        })
        .unwrap_or(0)
}

/// Print platform-specific installation instructions.
fn print_install_instructions() {
    println!();
    println!("Install Erlang/OTP 27+:");
    if cfg!(target_os = "macos") {
        println!("  brew install erlang        # Homebrew");
        println!("  asdf install erlang 27.2   # asdf version manager");
    } else if cfg!(target_os = "linux") {
        println!("  sudo apt install erlang    # Debian/Ubuntu (check version is 27+)");
        println!("  asdf install erlang 27.2   # asdf version manager");
    } else if cfg!(target_os = "windows") {
        println!("  Download from https://www.erlang.org/downloads");
        println!("  Or use: choco install erlang");
    } else {
        println!("  See https://www.erlang.org/downloads");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_otp_major_simple() {
        assert_eq!(parse_otp_major("27"), Some(27));
    }

    #[test]
    fn parse_otp_major_with_minor() {
        assert_eq!(parse_otp_major("27.2"), Some(27));
    }

    #[test]
    fn parse_otp_major_with_patch() {
        assert_eq!(parse_otp_major("27.2.1"), Some(27));
    }

    #[test]
    fn parse_otp_major_old_version() {
        assert_eq!(parse_otp_major("26"), Some(26));
    }

    #[test]
    fn parse_otp_major_empty() {
        assert_eq!(parse_otp_major(""), None);
    }

    #[test]
    fn parse_otp_major_garbage() {
        assert_eq!(parse_otp_major("abc"), None);
    }

    #[test]
    fn count_beam_files_nonexistent_dir() {
        assert_eq!(count_beam_files(Path::new("/nonexistent/path")), 0);
    }
}
