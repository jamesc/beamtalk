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

    println!(
        "Built release {release_name}-{release_vsn}\n  → {release_dir}\n\n\
         Boot it: erl -boot {release_config_dir}/start -boot_var RELEASE_DIR {release_dir} \
         -config {release_config_dir}/sys"
    );
    info!(name = %release_name, vsn = %release_vsn, dir = %release_dir, "release built");
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
        // Safety net: the internally-computed path must be under
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
}
