// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk release`: assemble a standard OTP release directory bootable
//! with `erl -boot` (ADR 0125 Part 1, Phase 1 / BT-3570).
//!
//! **DDD Context:** Build System — Packaging
//!
//! Compiles the project, computes the app closure (`closure.rs`), stages
//! every app into `lib/<app>-<vsn>/ebin/`, and writes `.rel`/`start.boot`/
//! `releases/RELEASES`/`sys.config`/`vm.args` (`assembly.rs`). Ships without
//! a launcher, ERTS bundling, or the tarball — `erl -boot … -boot_var
//! RELEASE_DIR <dir>` boots it directly; those pieces are BT-3571/BT-3573.

pub mod assembly;
pub mod closure;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use tracing::info;

use super::build_layout::BuildLayout;
use super::manifest;

/// Build a release for the project at `project_root`.
///
/// `output`, when given, overrides the release directory (otherwise
/// `_build/release/<name>-<vsn>/`, [`BuildLayout::release_dir`]).
///
/// `force_output` allows wiping a pre-existing `--output` directory that
/// does not look like a prior `beamtalk release` output — see
/// [`ensure_clean_release_dir`]'s doc comment for why that distinction
/// exists.
pub fn build_release(
    project_root: &Utf8Path,
    output: Option<&str>,
    options: &beamtalk_core::CompilerOptions,
    force: bool,
    force_output: bool,
) -> Result<()> {
    let Some(parsed) = manifest::find_manifest_full(project_root)? else {
        miette::bail!(
            "No 'beamtalk.toml' found in '{project_root}'.\n\
             `beamtalk release` requires a package manifest — run `beamtalk new <name> --app` \
             to create one."
        );
    };

    if parsed.application.is_none() {
        miette::bail!(
            "`beamtalk release` requires a root supervisor.\n\n\
             \x20 beamtalk.toml has no [application] section, so there is nothing for the\n\
             \x20 release to supervise. A release is a long-running service.\n\n\
             \x20 For a one-shot program, build an escript instead:\n\
             \x20     beamtalk build --escript --entry \"Main main:\"\n\n\
             \x20 To make this project a service, declare its root supervisor:\n\
             \x20     [application]\n\
             \x20     supervisor = \"AppSup\""
        );
    }

    // ADR 0125 §3.1: a release is an artifact whose validity the operator
    // cannot state if it was produced on an OTP major outside the declared
    // support window — unlike `beamtalk build`'s warning (`build::
    // warn_if_otp_out_of_window`), this is a hard error naming the window.
    check_otp_window()?;

    // Compile the project first — the release stages its **generated** .app
    // (ADR 0125 §1.3), and the app closure needs it and every dependency's
    // .app to already exist under `_build/`.
    eprintln!("Building...");
    super::build::build(project_root.as_str(), options, force)?;

    let layout = BuildLayout::new(project_root);
    let release_cfg = &parsed.release;
    let release_name = release_cfg
        .name
        .clone()
        .unwrap_or_else(|| parsed.package.name.clone());
    let release_vsn = parsed.package.version.clone();

    // `release_name`/`release_vsn` both end up as filesystem path segments
    // below (`layout.release_dir`, `rel_file = rel_dir.join("{name}.rel")`,
    // `releases/<vsn>/`) and `release_name` is also written raw into
    // `vm.args` as the node's `-sname` — a `/`/`..` in either is a
    // path-traversal opportunity, and embedded whitespace/newlines in
    // `release_name` would inject extra `vm.args` flags. `[release] name`
    // defaults to `[package] name` (already charset-validated by
    // `find_manifest_full`/`validate_package_name`), but an explicit
    // `[release] name` override, and `[package] version` (never charset
    // validated — see `manifest.rs`, versions are free-form strings), are
    // not otherwise checked before reaching this function, so both are
    // validated here regardless of where they came from. `assembly.rs`
    // additionally runs both through `escape_erlang_string` at every splice
    // site into the generated `.rel` term, the same defense-in-depth
    // already applied to `host_apps`/`staged_apps`/`bind`.
    validate_release_path_component("name", &release_name)?;
    validate_release_path_component("version", &release_vsn)?;

    let release_dir = match output {
        Some(dir) => Utf8PathBuf::from(dir),
        None => layout.release_dir(&release_name, &release_vsn),
    };
    // Absolutize once, here, and use this value for everything downstream
    // (staging, `.rel`/`RELEASE_DIR` writing, the printed boot command) —
    // see `assembly::absolutize`'s doc comment for why a *second*,
    // independently-derived absolute form of the same directory (even a
    // theoretically-equivalent one, like `canonicalize`) breaks the literal
    // string-prefix match `systools:make_script/2`'s `RELEASE_DIR`
    // substitution depends on.
    let release_dir = assembly::absolutize(&release_dir)?;

    ensure_clean_release_dir(&release_dir, output.is_some(), force_output, &layout)?;
    std::fs::create_dir_all(release_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create release dir '{release_dir}'"))?;
    // Windows only, and only now that the directory exists (see
    // `resolve_long_path`'s doc comment): rebind to the OS's own long-path
    // form so every path derived below — staging, `.rel`/`start.boot`
    // writing — agrees with what `systools:make_script/2`'s own
    // filesystem-backed `path` search reports for each staged app's
    // directory, instead of retaining a short-name (`RUNNER~1`-style)
    // component that only this string, and nothing the filesystem itself
    // reports, still uses.
    let release_dir = assembly::resolve_long_path(&release_dir)?;

    eprintln!("Computing app closure...");
    let app_closure = closure::compute_app_closure(&layout, &parsed.package, release_cfg)?;

    eprintln!(
        "Staging {} application(s)...",
        app_closure.staged_apps.len()
    );
    let staged_ebins = assembly::stage_apps(&release_dir, &app_closure)?;

    let release_config_dir = release_dir.join("releases").join(&release_vsn);
    std::fs::create_dir_all(release_config_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create '{release_config_dir}'"))?;

    assembly::generate_sys_config(
        project_root,
        &release_config_dir,
        &release_cfg.sys_config,
        release_cfg.console,
        &release_cfg.bind,
    )?;
    assembly::generate_vm_args(
        project_root,
        &release_config_dir,
        &release_cfg.vm_args,
        &release_name,
    )?;

    eprintln!("Writing .rel / start.boot / RELEASES...");
    assembly::write_rel_and_boot_script(
        &release_dir,
        &release_name,
        &release_vsn,
        &app_closure,
        &staged_ebins,
    )?;

    // `assembly.rs` bakes the `RELEASE_DIR` build-time prefix into the
    // `.script`/`.boot` as a forward-slashed string (the same Windows fix
    // `ebin_path_list` uses — see its doc comment): the `-boot_var
    // RELEASE_DIR` value a caller passes at boot time must be normalised
    // the same way, or `$RELEASE_DIR` substitution silently fails on
    // Windows and the release falls back to build-time absolute paths that
    // don't resolve. Print the same forward-slashed form here so the
    // command a Windows user copy-pastes actually boots.
    let release_dir_fwd = beamtalk_cli::path_util::to_forward_slash(release_dir.as_str());
    println!(
        "Built release {release_name}-{release_vsn}\n  → {release_dir}\n\n\
         Boot it: erl -boot {release_config_dir}/start -boot_var RELEASE_DIR {release_dir_fwd} \
         -config {release_config_dir}/sys"
    );
    info!(name = %release_name, vsn = %release_vsn, dir = %release_dir, "release built");
    Ok(())
}

/// ADR 0125 §3.1: refuse to produce a release on an OTP major outside the
/// declared support window (`otp-support.toml`). A no-op (never errors) when
/// the OTP major cannot be probed — fails open, matching every other
/// consumer of `build::current_otp_major`'s probe.
fn check_otp_window() -> Result<()> {
    if let Some(msg) = otp_out_of_window_error(super::build::current_otp_major()) {
        miette::bail!(msg);
    }
    Ok(())
}

/// Pure message-builder behind [`check_otp_window`], split out so the error
/// text is testable without depending on the host's actual OTP installation.
fn otp_out_of_window_error(major: Option<u32>) -> Option<String> {
    let major = major?;
    let window = beamtalk_cli::otp_support::window();
    if window.contains(major) {
        return None;
    }
    Some(format!(
        "Erlang/OTP {major} is outside the supported window ({}) declared in \
         otp-support.toml.\n\n\
         \x20 `beamtalk release` produces an artifact whose validity cannot be stated \
         \x20 when built outside the supported window. Install a supported OTP major, \
         \x20 or use `beamtalk build` to keep developing ahead of it.",
        window.display_range()
    ))
}

/// Validate that `value` (a `[release] name` or `[package] version`) is
/// safe to use as a single filesystem path segment and, for `name`, as a
/// `vm.args` `-sname` — never empty, never `.`/`..`, no path separator, and
/// no whitespace or control character (which would also let it smuggle
/// extra lines/flags into the generated `vm.args`).
fn validate_release_path_component(kind: &str, value: &str) -> Result<()> {
    if value.is_empty() {
        miette::bail!("[release] {kind} must not be empty");
    }
    if value == "." || value == ".." {
        miette::bail!(
            "[release] {kind} '{value}' is not a valid path segment — it is used to build a \
             filesystem path directly."
        );
    }
    if value.contains('/') || value.contains('\\') {
        miette::bail!(
            "[release] {kind} '{value}' must not contain '/' or '\\' — it is used to build \
             filesystem paths and must be a single path segment."
        );
    }
    if value.chars().any(|c| c.is_control() || c.is_whitespace()) {
        miette::bail!(
            "[release] {kind} '{value}' must not contain whitespace or control characters — \
             `name` is also written into `vm.args` as the node's `-sname`, where those \
             characters could inject additional flags."
        );
    }
    Ok(())
}

/// Make `release_dir` a clean, empty directory, wiping it first if it
/// already exists.
///
/// The internally-computed default (`_build/release/<name>-<vsn>/`) is
/// always safe to wipe unconditionally — it can only ever be a prior
/// release build's own output, the same way `beamtalk build`'s `_build/`
/// always is (asserted here, mirroring `clean.rs`'s own safety net for the
/// identical reason: the cost of a wrong assumption here is silent data
/// loss, so a path-construction bug that broke it should panic loudly
/// rather than quietly delete the wrong directory).
///
/// A user-supplied `--output <dir>` is a different risk: it can name *any*
/// path on disk, so a typo (`--output .`, `--output ..`) or a reused path
/// must not be wiped without confirmation. It is removed unconditionally
/// only when it already looks like a previous `beamtalk release` output
/// (has a `releases/` or `lib/` subdirectory — the two top-level
/// directories every release this command produces has); otherwise it is
/// refused unless `force_output` is set.
fn ensure_clean_release_dir(
    release_dir: &Utf8Path,
    user_supplied_output: bool,
    force_output: bool,
    layout: &BuildLayout,
) -> Result<()> {
    if !release_dir.exists() {
        return Ok(());
    }

    if user_supplied_output && !force_output {
        let looks_like_release_output =
            release_dir.join("releases").is_dir() || release_dir.join("lib").is_dir();
        if !looks_like_release_output {
            miette::bail!(
                "'--output {release_dir}' already exists and does not look like a previous \
                 `beamtalk release` output (no 'releases/' or 'lib/' subdirectory) — refusing \
                 to delete it.\n\n\
                 \x20 Pass a different --output path, remove '{release_dir}' yourself, or pass \
                 \x20 --force-output to delete it anyway."
            );
        }
    } else if !user_supplied_output {
        // Safety net 1: the internally-computed path must be under
        // `_build/`, the same invariant `clean.rs` asserts for every path
        // it removes — this can only fail on a programming error, and the
        // cost of being wrong here is deleting something that isn't a
        // build artifact. `release_dir` is already absolutized by the
        // caller, so `build_root()` is compared the same way (its own
        // relative-to-cwd form would never be a prefix of an absolute
        // path, which would make this assert fire on every ordinary run).
        let build_root = assembly::absolutize(&layout.build_root())?;
        assert!(
            release_dir.starts_with(&build_root),
            "computed release dir {release_dir} escapes build root {build_root}"
        );

        // Safety net 2: `starts_with` is purely lexical, so it cannot see
        // a symlinked `_build` (or `_build/release`) — `remove_dir_all` on
        // a *sub-path* of one would traverse the link and delete the
        // contents of its target, outside the project, exactly the
        // scenario `clean.rs`'s own `is_symlink(&build_root)` check exists
        // for. `has_symlink_in_chain` generalises that check to every path
        // component between `build_root` and `release_dir` (two new
        // segments here, `release/` and `<name>-<vsn>/`, either of which
        // could be the link), and — like `clean.rs` — refuses outright
        // rather than offering a force flag: a symlinked `_build` is
        // something to fix, not delete through.
        if beamtalk_cli::path_util::has_symlink_in_chain(release_dir, &build_root)? {
            miette::bail!(
                "Refusing to clean: '{release_dir}' (or a directory between it and \
                 '{build_root}') is a symlink.\n\n\
                 \x20 Remove the symlink manually, or pass an explicit --output pointing \
                 \x20 outside '{build_root}'."
            );
        }
    }

    std::fs::remove_dir_all(release_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to clean existing release dir '{release_dir}'"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn build_release_requires_application_supervisor() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::write(
            root.join("beamtalk.toml").as_std_path(),
            "[package]\nname = \"orders\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let err = build_release(&root, None, &options, false, false).unwrap_err();
        assert!(
            err.to_string().contains("requires a root supervisor"),
            "got: {err}"
        );
        assert!(
            err.to_string().contains("beamtalk build --escript"),
            "should point at the escript alternative: {err}"
        );
    }

    #[test]
    fn build_release_no_manifest_errors() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let err = build_release(&root, None, &options, false, false).unwrap_err();
        assert!(
            err.to_string().contains("No 'beamtalk.toml' found"),
            "got: {err}"
        );
    }

    // -- otp_out_of_window_error -----------------------------------------

    #[test]
    fn otp_out_of_window_error_none_when_major_unknown() {
        assert_eq!(otp_out_of_window_error(None), None);
    }

    #[test]
    fn otp_out_of_window_error_none_when_major_in_window() {
        let window = beamtalk_cli::otp_support::window();
        assert_eq!(otp_out_of_window_error(Some(window.min_major)), None);
        assert_eq!(otp_out_of_window_error(Some(window.max_major)), None);
    }

    #[test]
    fn otp_out_of_window_error_below_window() {
        let window = beamtalk_cli::otp_support::window();
        let msg = otp_out_of_window_error(Some(window.min_major - 1)).expect("should error");
        assert!(msg.contains("outside the supported window"), "{msg}");
        assert!(msg.contains(&window.display_range()), "{msg}");
    }

    #[test]
    fn otp_out_of_window_error_above_window() {
        let window = beamtalk_cli::otp_support::window();
        let msg = otp_out_of_window_error(Some(window.max_major + 1)).expect("should error");
        assert!(msg.contains("outside the supported window"), "{msg}");
    }

    // -- validate_release_path_component --------------------------------

    #[test]
    fn validate_release_path_component_accepts_ordinary_values() {
        validate_release_path_component("name", "orders").unwrap();
        validate_release_path_component("version", "1.0.0-dev+38a688d").unwrap();
    }

    #[test]
    fn validate_release_path_component_rejects_empty() {
        let err = validate_release_path_component("name", "").unwrap_err();
        assert!(err.to_string().contains("must not be empty"), "{err}");
    }

    #[test]
    fn validate_release_path_component_rejects_dot_and_dotdot() {
        assert!(validate_release_path_component("name", ".").is_err());
        assert!(validate_release_path_component("version", "..").is_err());
    }

    #[test]
    fn validate_release_path_component_rejects_path_traversal() {
        let err = validate_release_path_component("version", "../../etc/passwd").unwrap_err();
        assert!(err.to_string().contains("path segment"), "{err}");

        let err = validate_release_path_component("name", "a/../../b").unwrap_err();
        assert!(err.to_string().contains("path segment"), "{err}");

        let err = validate_release_path_component("name", "a\\b").unwrap_err();
        assert!(err.to_string().contains("path segment"), "{err}");
    }

    #[test]
    fn validate_release_path_component_rejects_whitespace_and_control_chars() {
        let err = validate_release_path_component("name", "orders\nextra_flag").unwrap_err();
        assert!(err.to_string().contains("-sname"), "{err}");

        let err = validate_release_path_component("name", "orders evil").unwrap_err();
        assert!(err.to_string().contains("-sname"), "{err}");
    }

    /// A malicious `[release] name`/`[package] version` must be refused
    /// before it ever reaches path construction or the generated `.rel`
    /// term — the concrete attack vectors coordinator review flagged:
    /// embedded `"` (breaks out of the `.rel` term's Erlang string
    /// literal), `/` (path traversal into `rel_file`/`releases/<vsn>/`),
    /// and embedded whitespace (extra `vm.args` flags via `-sname`).
    #[test]
    fn validate_release_path_component_rejects_malicious_values() {
        for malicious in [
            "orders\", {evil, true}, {x, \"",
            "../../../etc/cron.d/evil",
            "orders\nextra_flag value",
        ] {
            assert!(
                validate_release_path_component("name", malicious).is_err(),
                "expected '{malicious}' to be rejected"
            );
        }
    }

    // -- ensure_clean_release_dir --------------------------------------

    #[test]
    fn ensure_clean_release_dir_absent_dir_is_a_no_op() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let target = root.join("dist");
        ensure_clean_release_dir(&target, true, false, &layout).unwrap();
        assert!(!target.exists());
    }

    #[test]
    fn ensure_clean_release_dir_refuses_non_release_shaped_user_output_without_force() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let target = root.join("dist");
        fs::create_dir_all(target.as_std_path()).unwrap();
        fs::write(target.join("important.txt").as_std_path(), "keep me").unwrap();

        let err = ensure_clean_release_dir(&target, true, false, &layout).unwrap_err();
        assert!(
            err.to_string().contains("does not look like a previous"),
            "got: {err}"
        );
        assert!(err.to_string().contains("--force-output"), "got: {err}");
        // Refused: the directory and its contents must be untouched.
        assert!(target.join("important.txt").is_file());
    }

    #[test]
    fn ensure_clean_release_dir_force_output_wipes_a_non_release_shaped_dir() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let target = root.join("dist");
        fs::create_dir_all(target.as_std_path()).unwrap();
        fs::write(target.join("important.txt").as_std_path(), "keep me").unwrap();

        ensure_clean_release_dir(&target, true, true, &layout).unwrap();
        assert!(!target.exists());
    }

    #[test]
    fn ensure_clean_release_dir_wipes_a_release_shaped_user_output_without_force() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let target = root.join("dist");
        // Looks like a prior `beamtalk release` output (has `lib/`).
        fs::create_dir_all(target.join("lib").as_std_path()).unwrap();

        ensure_clean_release_dir(&target, true, false, &layout).unwrap();
        assert!(!target.exists());
    }

    #[test]
    fn ensure_clean_release_dir_wipes_internally_computed_dir_without_force() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let target = layout.release_dir("orders", "1.0.0");
        fs::create_dir_all(target.as_std_path()).unwrap();
        fs::write(target.join("stale.txt").as_std_path(), "old build").unwrap();

        // Not user-supplied (`output.is_some()` is false), so no
        // release-shape check and no --force-output needed.
        ensure_clean_release_dir(&target, false, false, &layout).unwrap();
        assert!(!target.exists());
    }

    /// A symlinked `_build` must be refused, not followed —
    /// `remove_dir_all` on a computed sub-path underneath it would
    /// otherwise traverse the link and delete the *target* directory's
    /// contents, which can be anywhere on disk. `starts_with` alone can't
    /// see this (it's a purely lexical check), which is exactly why
    /// `ensure_clean_release_dir` also calls `has_symlink_in_chain`.
    #[test]
    #[cfg(unix)]
    fn ensure_clean_release_dir_refuses_symlinked_build_root() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // A real directory elsewhere that must survive untouched.
        let real_target = root.join("outside_build_root");
        fs::create_dir_all(real_target.join("release/orders-1.0.0").as_std_path()).unwrap();
        fs::write(
            real_target
                .join("release/orders-1.0.0/precious.txt")
                .as_std_path(),
            "do not delete me",
        )
        .unwrap();

        // `_build` itself is a symlink into that other directory.
        let build_root = root.join("_build");
        std::os::unix::fs::symlink(real_target.as_std_path(), build_root.as_std_path()).unwrap();

        let layout = BuildLayout::new(&root);
        let target = layout.release_dir("orders", "1.0.0");

        let err = ensure_clean_release_dir(&target, false, false, &layout).unwrap_err();
        assert!(err.to_string().contains("symlink"), "{err}");
        assert!(
            real_target
                .join("release/orders-1.0.0/precious.txt")
                .is_file(),
            "the symlink target's contents must be untouched"
        );
    }
}
