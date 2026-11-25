// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk release`: assemble a standard OTP release directory bootable
//! with `erl -boot` (ADR 0125 Part 1, Phases 1/2 — BT-3570/BT-3571).
//!
//! **DDD Context:** Build System — Packaging
//!
//! Compiles the project, computes the app closure (`closure.rs`), stages
//! every app into `lib/<app>-<vsn>/ebin/`, and writes `.rel`/`start.boot`/
//! `releases/RELEASES`/`sys.config`/`vm.args` (`assembly.rs`, BT-3570).
//! BT-3571 adds ERTS bundling (`[release] include-erts`, the default),
//! `strip-beams`/`include-compiler`'s compiler-port staging, the
//! `<name>-<vsn>.tar.gz` tarball (`systools:make_tar/2`), and the two
//! self-describing manifests every release carries:
//! `releases/<vsn>/beamtalk-provenance.json` (`provenance.rs`) and
//! `releases/<vsn>/shapes.json` (`assembly::write_shapes_json`, driving
//! `beamtalk_release_shapes` in `beamtalk_runtime`). Still no launcher —
//! `erl -boot … -boot_var RELEASE_DIR <dir>` boots the unpacked directory
//! directly; `bin/<name>`/`bin/<name>.cmd` are BT-3573.

pub mod assembly;
pub mod closure;
pub mod launcher;
pub mod provenance;
pub mod upgrade;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use tracing::info;

use super::build_stamp;
use beamtalk_core::build_layout::BuildLayout;
use beamtalk_package::manifest;

/// Build a release for the project at `project_root`.
///
/// `output`, when given, overrides the release directory (otherwise
/// `_build/release/<name>-<vsn>/`, [`BuildLayout::release_dir`]).
///
/// `force_output` allows wiping a pre-existing `--output` directory that
/// does not look like a prior `beamtalk release` output — see
/// [`ensure_clean_release_dir`]'s doc comment for why that distinction
/// exists.
///
/// `no_include_erts` is `--no-include-erts` (ADR 0125 §1.1): when set, it
/// overrides `[release] include-erts` to `false` regardless of the
/// manifest — a CLI flag is a one-shot override, so it can only turn the
/// manifest's default *off*, never force it *on* (`[release] include-erts
/// = false` already does that from the manifest side).
///
/// `upgrade_from` is `--upgrade-from <prev-release-dir-or-tarball>`
/// (ADR 0125 §2.3, BT-3574): after this build finishes, runs the
/// shape-compatibility preflight against the named previous release and
/// prints its report. Any **error** finding fails the command (non-zero
/// exit) — see [`upgrade::run_upgrade_preflight`].
#[allow(clippy::too_many_lines)] // one straight-line assembly pipeline; manifest loading, upgrade preflight and the summary are split out
pub fn build_release(
    project_root: &Utf8Path,
    output: Option<&str>,
    options: &beamtalk_core::CompilerOptions,
    force: bool,
    force_output: bool,
    no_include_erts: bool,
    upgrade_from: Option<&str>,
) -> Result<()> {
    // The release stages artifacts from `_build/`; keep a concurrent build from
    // rewriting them mid-assembly (BT-5075). `build` below re-enters this lock.
    let _project_lock = crate::commands::dir_lock::lock_project_build(project_root)?;

    let parsed = load_release_manifest(project_root)?;

    // `--upgrade-from` is checked before the (slow) build, so a typo costs
    // nothing (BT-5134).
    if let Some(prev) = upgrade_from {
        validate_upgrade_from(prev)?;
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

    let tarball = release_tarball_path(&release_dir, &release_name, &release_vsn);
    // A run killed by a signal (or power loss) leaves the previous output
    // parked; put it back before anything else looks at the paths (BT-5134),
    // but never beside a protected `--output` (BT-5187).
    if output.is_some() {
        ensure_unprotected_output(&release_dir, &layout)?;
    }
    recover_interrupted_release(&release_dir, &tarball)?;
    ensure_clean_release_dir(&release_dir, output.is_some(), force_output, &layout)?;
    ensure_clean_tarball(&tarball, output.is_some(), force_output)?;
    // `--upgrade-from` may name the very output being replaced; resolve that
    // now, while the paths still exist, and read the parked copy instead.
    let upgrade_target =
        upgrade_from.map(|prev| classify_upgrade_from(prev, &release_dir, &tarball));
    // The previous output (directory and tarball) is set aside, not deleted,
    // until the new release is fully assembled: any failure below — an error
    // *or* a panic, since the guard restores on drop — puts it back
    // (BT-5071, BT-5107).
    let swap = ReleaseSwap::begin(&release_dir, tarball.clone())?;
    let upgrade_from = upgrade_target.map(|t| match t {
        UpgradeFrom::Path(p) => p,
        UpgradeFrom::ReplacedDir => swap
            .parked_dir
            .clone()
            .unwrap_or_else(|| release_dir.clone()),
        UpgradeFrom::ReplacedTarball => swap
            .parked_tarball
            .clone()
            .unwrap_or_else(|| tarball.clone()),
    });
    let assembled = assemble_release(
        &parsed,
        project_root,
        &layout,
        &release_dir,
        &tarball,
        no_include_erts,
        upgrade_from.as_deref(),
    );
    assembled?;
    swap.commit()
}

/// Assemble the release into the (now absent or empty) `release_dir`.
#[allow(clippy::too_many_lines)] // one straight-line assembly pipeline
fn assemble_release(
    parsed: &manifest::ParsedManifest,
    project_root: &Utf8Path,
    layout: &BuildLayout,
    release_dir: &Utf8Path,
    final_tarball: &Utf8Path,
    no_include_erts: bool,
    upgrade_from: Option<&Utf8Path>,
) -> Result<()> {
    let release_cfg = &parsed.release;
    let release_name = release_cfg
        .name
        .clone()
        .unwrap_or_else(|| parsed.package.name.clone());
    let release_vsn = parsed.package.version.clone();
    let release_dir = release_dir.to_path_buf();
    let layout = layout.clone();
    std::fs::create_dir_all(release_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create release dir '{release_dir}'"))?;
    write_release_marker(&release_dir)?;
    // Windows only, and only now that the directory exists (see
    // `resolve_long_path`'s doc comment): rebind to the OS's own long-path
    // form so every path derived below — staging, `.rel`/`start.boot`
    // writing — agrees with what `systools:make_script/2`'s own
    // filesystem-backed `path` search reports for each staged app's
    // directory, instead of retaining a short-name (`RUNNER~1`-style)
    // component that only this string, and nothing the filesystem itself
    // reports, still uses.
    let release_dir = assembly::resolve_long_path(&release_dir)?;

    let include_erts = release_cfg.include_erts && !no_include_erts;

    eprintln!("Computing app closure...");
    let app_closure = closure::compute_app_closure(&layout, &parsed.package, release_cfg)?;

    eprintln!(
        "Staging {} application(s)...",
        app_closure.staged_apps.len()
    );
    let staged_ebins = assembly::stage_apps(&release_dir, &app_closure)?;

    // Every staged app's own lib dir — the shape extractor needs the whole
    // closure *loaded* for ancestor resolution, but only classifies the
    // non-runtime-closure subset as `emit_dirs` (`closure::is_runtime_app`,
    // ADR 0125 §2.2's own project/dependency-vs-toolchain distinction).
    let mut runtime_dirs: Vec<Utf8PathBuf> = Vec::new();
    let mut emit_dirs: Vec<Utf8PathBuf> = Vec::new();
    for (app, ebin) in app_closure.staged_apps.iter().zip(staged_ebins.iter()) {
        if closure::is_runtime_app(&app.name) {
            runtime_dirs.push(ebin.clone());
        } else {
            emit_dirs.push(ebin.clone());
        }
    }

    eprintln!("Discovering ERTS...");
    let (erts_root, erts_version) = assembly::discover_erts_info()?;
    if include_erts {
        eprintln!("Copying ERTS {erts_version}...");
        assembly::copy_erts(&release_dir, &erts_root, &erts_version)?;
    }

    let release_config_dir = release_dir.join("releases").join(&release_vsn);
    std::fs::create_dir_all(release_config_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create '{release_config_dir}'"))?;

    assembly::generate_sys_config(
        project_root,
        &release_config_dir,
        &release_cfg.sys_config,
        &release_name,
        release_cfg.console,
        &release_cfg.bind,
        release_cfg.include_compiler,
        &parsed.package.name,
    )?;
    assembly::generate_vm_args(project_root, &release_config_dir, &release_cfg.vm_args)?;

    if release_cfg.include_compiler {
        eprintln!("Staging compiler port binary...");
        assembly::stage_compiler_port_binary(&release_dir)?;
    }

    eprintln!("Writing .rel / start.boot / RELEASES...");
    assembly::write_rel_and_boot_script(
        &release_dir,
        &release_name,
        &release_vsn,
        &app_closure,
        &staged_ebins,
        include_erts,
    )?;

    if release_cfg.strip_beams {
        eprintln!("Stripping debug_info...");
        assembly::strip_release_beams(&release_dir)?;
    }

    eprintln!("Writing launcher scripts...");
    launcher::write_launcher_scripts(
        &release_dir,
        &release_name,
        &release_vsn,
        &erts_version,
        include_erts,
    )?;

    eprintln!("Writing provenance/shape manifests...");
    let otp_release = build_stamp::current_otp_version().ok_or_else(|| {
        miette::miette!(
            "Could not determine the building OTP version (erl probe failed) — \
             beamtalk-provenance.json needs it for `required_otp`."
        )
    })?;
    provenance::write_provenance_json(
        &release_config_dir,
        &release_name,
        &release_vsn,
        build_stamp::current_beamtalk_version(),
        otp_release,
        include_erts,
        &erts_version,
        &provenance::current_platform(),
        &app_closure,
    )?;
    assembly::write_shapes_json(
        &runtime_dirs,
        &emit_dirs,
        &release_config_dir.join("shapes.json"),
        &release_vsn,
    )?;

    eprintln!("Building tarball...");
    // systools writes `<release_name>.tar.gz` into its `outdir`: give it a
    // private scratch directory beside the final location (same filesystem,
    // so the final move is atomic) rather than the shared output directory,
    // where that name could belong to an unrelated file, or be left behind
    // by a failed build (BT-5134).
    let tar_scratch = tempfile::Builder::new()
        .prefix(".beamtalk-tarball-")
        .tempdir_in(tarball_out_dir(&release_dir).as_std_path())
        .into_diagnostic()
        .wrap_err("Failed to create a scratch directory for the release tarball")?;
    let tar_scratch_dir = Utf8PathBuf::from_path_buf(tar_scratch.path().to_path_buf())
        .map_err(|p| miette::miette!("Scratch directory '{}' is not valid UTF-8", p.display()))?;
    let (scratch_tar, _) = assembly::make_tarball(
        &release_dir,
        &release_name,
        &release_vsn,
        &staged_ebins,
        &tar_scratch_dir,
        include_erts,
        &erts_root,
    )?;
    // ADR 0125 §2.3, BT-3574: `systools:make_tar/2` does not archive
    // shapes.json/beamtalk-provenance.json (see
    // `append_shape_manifests_to_tarball`'s doc) — append them so a
    // tarball this command produces is itself a valid `--upgrade-from`
    // input, with no on-the-fly-extraction fallback needed. Re-stat
    // afterward — the append step recompresses the tarball in place, so
    // `make_tarball`'s own returned size is stale for the printed summary.
    assembly::append_shape_manifests_to_tarball(
        &scratch_tar,
        &release_dir,
        &release_vsn,
        &emit_dirs,
    )?;
    // `ReleaseSwap` parked anything already at the final path, so this never
    // replaces an unrelated file.
    if final_tarball.symlink_metadata().is_ok() {
        miette::bail!("'{final_tarball}' appeared during the build — refusing to overwrite it.");
    }
    std::fs::rename(scratch_tar.as_std_path(), final_tarball.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| {
            format!("Failed to move '{scratch_tar}' into place at '{final_tarball}'")
        })?;
    let tar_path = final_tarball.to_path_buf();
    drop(tar_scratch);
    let tar_size = std::fs::metadata(tar_path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat '{tar_path}' after appending manifests"))?
        .len();

    // ADR 0125 §2.3, BT-3574: only after the new release is fully built (its
    // own shapes.json/beamtalk-provenance.json now exist) — the preflight
    // compares the two releases' finished manifests, never a partial build.
    if let Some(prev) = upgrade_from {
        check_upgrade_from(prev.as_str(), &release_dir, &release_name, &release_vsn)?;
    }

    println!(
        "{}",
        release_summary(&ReleaseSummary {
            name: &release_name,
            vsn: &release_vsn,
            dir: &release_dir,
            tarball: &tar_path,
            tarball_bytes: tar_size,
            include_erts,
            erts_version: &erts_version,
            otp_release,
        })
    );
    info!(name = %release_name, vsn = %release_vsn, dir = %release_dir, "release built");
    Ok(())
}

/// Load `beamtalk.toml` for a release build, rejecting a project that has no
/// manifest or no `[application]` root supervisor.
fn load_release_manifest(project_root: &Utf8Path) -> Result<manifest::ParsedManifest> {
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

    Ok(parsed)
}

/// Run the `--upgrade-from` shape-compatibility preflight against the previous
/// release at `prev`, print its report, and fail on any blocking finding.
fn check_upgrade_from(
    prev: &str,
    release_dir: &Utf8Path,
    release_name: &str,
    release_vsn: &str,
) -> Result<()> {
    let report = upgrade::run_upgrade_preflight(
        &Utf8PathBuf::from(prev),
        release_dir,
        release_name,
        release_vsn,
    )?;
    print!("\n{}", report.text);
    if report.has_error {
        miette::bail!(
            "Upgrade check found blocking shape errors — see the report above.\n\n\
             \x20 Add the missing migration hook(s), or bump shapeVersion: to acknowledge \
             \x20 the structural fallback, then re-run `beamtalk release --upgrade-from`."
        );
    }
    Ok(())
}

/// What the final "Built release ..." message reports.
struct ReleaseSummary<'a> {
    name: &'a str,
    vsn: &'a str,
    dir: &'a Utf8Path,
    tarball: &'a Utf8Path,
    tarball_bytes: u64,
    include_erts: bool,
    erts_version: &'a str,
    otp_release: &'a str,
}

/// Render the message printed after a successful release build.
fn release_summary(s: &ReleaseSummary<'_>) -> String {
    let ReleaseSummary {
        name: release_name,
        vsn: release_vsn,
        dir: release_dir,
        tarball: tar_path,
        tarball_bytes,
        include_erts,
        erts_version,
        otp_release,
    } = *s;
    let platform = provenance::current_platform();
    let erts_note = if include_erts {
        format!("with ERTS {erts_version}, {platform}")
    } else {
        format!("no ERTS bundled, {platform}")
    };
    let trailer = if include_erts {
        format!(
            "This release bundles ERTS and runs only on {platform}.\n\
             Build on the target platform, or use --no-include-erts to require a host \
             Erlang/OTP {min}, {mid} or {max} (ADR 0125 §3.2).",
            min = otp_release_major_or_unknown(otp_release),
            mid = otp_release_major_or_unknown(otp_release) + 1,
            max = otp_release_major_or_unknown(otp_release) + 2,
        )
    } else {
        format!(
            "This release ships no ERTS — requires a host Erlang/OTP {min}, {mid} or {max}.",
            min = otp_release_major_or_unknown(otp_release),
            mid = otp_release_major_or_unknown(otp_release) + 1,
            max = otp_release_major_or_unknown(otp_release) + 2,
        )
    };

    // The launcher (`bin/<name>`/`bin/<name>.cmd`, ADR 0125 §1.6/§1.7,
    // BT-3573) resolves the release root relative to its own location, so —
    // unlike the raw `erl -boot ... -boot_var RELEASE_DIR <dir>` invocation
    // this message used to print — the printed command needs no
    // Windows-forward-slash normalisation of `release_dir`. It does need the
    // right *launcher* extension per platform: `bin/<name>` (no extension,
    // a POSIX `sh` script) is not runnable as printed on Windows, and
    // `bin/<name>.cmd` is unnecessary noise on Unix — `beamtalk release`
    // itself only ever runs on the platform it's building for (cross-compile
    // is unsupported, §1.3), so `cfg!(windows)` here is the same "this build
    // machine" scope the rest of this function already assumes.
    let launcher_suffix = if cfg!(windows) { ".cmd" } else { "" };
    format!(
        "Built release {release_name}-{release_vsn} ({erts_note}).\n\
         \x20 → {release_dir}\n\
         \x20 → {tar_path}  ({tar_size})\n\n\
         {trailer}\n\n\
         Run it: {release_dir}/bin/{release_name}{launcher_suffix} foreground",
        tar_size = format_bytes(tarball_bytes),
    )
}

/// Parse the OTP build major out of `otp_release` (the compound
/// `<major>-<erts>` string), falling back to `0` — only reachable if
/// `build_stamp::current_otp_version()` ever returned a string in a
/// different shape than its own contract, so this is a display-only
/// fallback, never a silent correctness gap (the same value already went
/// through [`provenance::write_provenance_json`]'s own, error-returning
/// parse of the identical string moments earlier in this same call).
fn otp_release_major_or_unknown(otp_release: &str) -> u32 {
    build_stamp::otp_build_major(otp_release).unwrap_or(0)
}

/// Render a byte count as a human-readable size (`"48.2 MB"`,
/// `"512.0 KB"`) for the command's own printed tarball size — informational
/// only, so plain decimal (MB = `1_000_000` bytes) rather than binary
/// (MiB = `1_048_576`) matches what `ls -lh`/most package managers already
/// show a user for a download size.
fn format_bytes(bytes: u64) -> String {
    const MB: f64 = 1_000_000.0;
    const KB: f64 = 1_000.0;
    #[allow(clippy::cast_precision_loss)] // display-only; no correctness dependency on precision
    let bytes_f = bytes as f64;
    if bytes_f >= MB {
        format!("{:.1} MB", bytes_f / MB)
    } else if bytes_f >= KB {
        format!("{:.1} KB", bytes_f / KB)
    } else {
        format!("{bytes} B")
    }
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
    if value.contains('@') {
        miette::bail!(
            "[release] {kind} '{value}' must not contain '@' — `name` is written into \
             `vm.args` as `-sname {value}@localhost` (ADR 0125 §1.6), and a second '@' in \
             the name would make that an invalid, two-'@' short name the release could \
             never boot with."
        );
    }
    // BT-5067: the value is substituted into `RELEASE_NAME="…"` in the generated
    // `launcher.sh` / `launcher.cmd`, so shell- and cmd-metacharacters (`"`, `$`,
    // backtick, `%`, `&`, `^`, …) must never reach it. Restrict to the package-name
    // character set, plus `.` and `+` which a `[package] version` needs.
    if let Some(bad) = value
        .chars()
        .find(|c| !(c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | '.' | '+')))
    {
        miette::bail!(
            "[release] {kind} '{value}' contains '{bad}' — only letters, digits, '-', '_', \
             '.' and '+' are allowed, because the value is written into the generated \
             launcher scripts."
        );
    }
    Ok(())
}

/// Name of the marker file `beamtalk release` writes at the top of every
/// release directory it assembles. It is the only thing (besides the
/// provenance manifest older releases already carry) that lets a later run
/// recognise a user-supplied `--output` as its own previous output.
const RELEASE_MARKER: &str = ".beamtalk-release";

/// Suffix of the sibling directory a previous release is parked in while its
/// replacement is assembled.
const PREVIOUS_SUFFIX: &str = ".beamtalk-previous";

/// Suffix of the swap journal [`ReleaseSwap`] keeps beside the release
/// directory while a previous output is parked (BT-5187). It names the
/// parked tarball and the swap's state: `pending` from before anything is
/// parked until the new release is complete, then `committed` (written
/// atomically) *before* the parked copies are discarded. A `committed`
/// journal means the new release is complete, so a parked copy found beside
/// it is garbage to finish discarding — never a release to restore over the
/// new one.
const JOURNAL_SUFFIX: &str = ".beamtalk-swap";

/// First line of every swap journal; anything else at the journal path is
/// not ours and stops recovery without touching anything.
const JOURNAL_HEADER: &str = "beamtalk-release-swap v1";

fn write_release_marker(release_dir: &Utf8Path) -> Result<()> {
    let marker = release_dir.join(RELEASE_MARKER);
    std::fs::write(
        marker.as_std_path(),
        "Written by `beamtalk release`. Its presence lets a later `beamtalk release --output` \
         replace this directory without --force-output.\n",
    )
    .into_diagnostic()
    .wrap_err_with(|| format!("Failed to write '{marker}'"))
}

/// Whether `dir` was produced by `beamtalk release`: it carries the marker,
/// or (for releases built before the marker existed) a
/// `releases/<vsn>/beamtalk-provenance.json` written by this command.
fn is_previous_release_output(dir: &Utf8Path) -> bool {
    if dir.join(RELEASE_MARKER).is_file() {
        return true;
    }
    std::fs::read_dir(dir.join("releases").as_std_path()).is_ok_and(|entries| {
        entries
            .flatten()
            .any(|e| e.path().join("beamtalk-provenance.json").is_file())
    })
}

/// Project subdirectories holding hand-written sources; an `--output` naming
/// one, containing one, or inside one would delete or clobber them.
const SOURCE_DIRS: &[&str] = &["src", "test", "native", "priv", "deps", ".git"];

/// Directories no `--output` may ever name, even with `--force-output`: a
/// filesystem root, the home directory, the current directory, the project
/// root and every ancestor of any of them, and the project's source
/// directories together with everything inside them. A path that cannot be
/// resolved counts as protected.
fn is_protected_output(release_dir: &Utf8Path, project_root: &Utf8Path) -> bool {
    is_protected_output_with(
        release_dir,
        project_root,
        std::env::current_dir().ok().as_deref(),
        dirs::home_dir().as_deref(),
    )
}

fn is_protected_output_with(
    release_dir: &Utf8Path,
    project_root: &Utf8Path,
    cwd: Option<&std::path::Path>,
    home: Option<&std::path::Path>,
) -> bool {
    let Some(target) = canonicalize_lenient(release_dir.as_std_path()) else {
        return true; // unresolvable: refuse rather than guess
    };
    if target.parent().is_none() {
        return true; // a filesystem root
    }
    let source_dirs: Vec<_> = SOURCE_DIRS
        .iter()
        .filter_map(|d| canonicalize_lenient(project_root.join(d).as_std_path()))
        .collect();
    if source_dirs.iter().any(|dir| target.starts_with(dir)) {
        return true; // a source directory or something inside one
    }
    let mut roots = vec![project_root.as_std_path().to_path_buf()];
    roots.extend(cwd.map(std::path::Path::to_path_buf));
    roots.extend(home.map(std::path::Path::to_path_buf));
    roots
        .iter()
        .filter_map(|root| canonicalize_lenient(root))
        .chain(source_dirs.iter().cloned())
        .any(|root| root.starts_with(&target))
}

/// `canonicalize`, extended to paths that do not exist yet: the longest
/// existing prefix is canonicalized and the missing tail applied to it one
/// component at a time, re-resolving every component that exists (a `..` can
/// lead back into existing directories and their symlinks). `None` when no
/// prefix resolves.
fn canonicalize_lenient(path: &std::path::Path) -> Option<std::path::PathBuf> {
    use std::path::Component;
    let components: Vec<Component<'_>> = path.components().collect();
    for split in (1..=components.len()).rev() {
        let prefix: std::path::PathBuf = components[..split].iter().collect();
        let Ok(mut resolved) = std::fs::canonicalize(&prefix) else {
            continue;
        };
        for component in &components[split..] {
            match component {
                Component::Normal(name) => {
                    resolved.push(name);
                    // A `..` can lead the tail back into existing directories,
                    // whose symlinks must be resolved too.
                    if let Ok(real) = std::fs::canonicalize(&resolved) {
                        resolved = real;
                    }
                }
                Component::ParentDir => {
                    resolved.pop();
                }
                Component::CurDir => {}
                Component::Prefix(_) | Component::RootDir => return None,
            }
        }
        return Some(resolved);
    }
    None
}

/// Top-level entries a release directory holds: `bin/`, `lib/`,
/// `releases/`, `erts-*/` (and the marker, though a marked directory never
/// needs `--force-output`).
fn is_release_entry_name(name: &str) -> bool {
    matches!(name, "bin" | "lib" | "releases" | RELEASE_MARKER) || name.starts_with("erts-")
}

/// Whether `dir` looks like a release some tool assembled, though it lacks
/// the marker: it has a `releases/` directory and nothing at the top level
/// but release entries ([`is_release_entry_name`]). The one non-empty,
/// unmarked directory `--force-output` may replace.
fn is_release_shaped(dir: &Utf8Path) -> bool {
    if !dir.join("releases").is_dir() {
        return false;
    }
    std::fs::read_dir(dir.as_std_path()).is_ok_and(|entries| {
        entries
            .into_iter()
            .all(|e| e.is_ok_and(|e| e.file_name().to_str().is_some_and(is_release_entry_name)))
    })
}

fn is_empty_dir(dir: &Utf8Path) -> bool {
    std::fs::read_dir(dir.as_std_path()).is_ok_and(|mut d| d.next().is_none())
}

/// Refuse a user-supplied `--output` that [`is_protected_output`] — checked
/// even when the path does not exist yet (a release written inside a source
/// directory would be replaced, i.e. deleted, by the next run), and before
/// interrupted-run recovery touches anything beside it.
fn ensure_unprotected_output(release_dir: &Utf8Path, layout: &BuildLayout) -> Result<()> {
    if canonicalize_lenient(release_dir.as_std_path()).is_none() {
        miette::bail!(
            "'--output {release_dir}' cannot be resolved to a location on disk — refusing to \
             use it."
        );
    }
    if is_protected_output(release_dir, layout.project_root()) {
        miette::bail!(
            "'--output {release_dir}' is the current directory, the home directory, a filesystem \
             root, the project root or an ancestor of any of them, or one of the project's \
             source directories or a path inside one — refusing to use it, even with \
             --force-output.\n\n\
             \x20 Pass an --output path that is a new or dedicated directory."
        );
    }
    Ok(())
}

/// Validate that `release_dir` may be replaced by a new release build.
///
/// The internally-computed default (`_build/release/<name>-<vsn>/`) is
/// always safe to replace — it can only ever be a prior release build's own
/// output, the same way `beamtalk build`'s `_build/` always is (asserted
/// here, mirroring `clean.rs`'s own safety net for the identical reason: the
/// cost of a wrong assumption is silent data loss, so a path-construction
/// bug should panic loudly rather than quietly delete the wrong directory).
///
/// A user-supplied `--output <dir>` can name *any* path on disk, so:
///
/// * a filesystem root, the home directory, the current directory, the
///   project root and the ancestors of any of them, and the project's source
///   directories and everything inside them, are always refused, even with
///   `force_output` (see [`is_protected_output`]);
/// * an absent or empty directory is accepted;
/// * an existing one is replaced only if it carries the [`RELEASE_MARKER`]
///   (or an older release's provenance manifest);
/// * `force_output` adds exactly one more case: an unmarked directory that is
///   release-shaped ([`is_release_shaped`] — a `releases/` directory and
///   nothing but release entries at the top level). Any other directory
///   (`~/Documents`, `/usr/local`, …) is refused, forced or not.
///
/// This only validates; [`set_aside_previous_release`] does the replacing.
fn ensure_clean_release_dir(
    release_dir: &Utf8Path,
    user_supplied_output: bool,
    force_output: bool,
    layout: &BuildLayout,
) -> Result<()> {
    if user_supplied_output {
        ensure_unprotected_output(release_dir, layout)?;
        if release_dir.symlink_metadata().is_err()
            || (release_dir.is_dir() && is_empty_dir(release_dir))
            || is_previous_release_output(release_dir)
        {
            return Ok(());
        }
        if force_output && is_release_shaped(release_dir) {
            return Ok(());
        }
        if force_output {
            miette::bail!(
                "'--output {release_dir}' already exists, was not written by `beamtalk release` \
                 (no '{RELEASE_MARKER}' marker file) and does not look like a release (a \
                 'releases/' directory and only bin/, lib/, releases/, erts-*/ at the top) — \
                 refusing to delete it, even with --force-output.\n\n\
                 \x20 Pass a different --output path, or remove '{release_dir}' yourself."
            );
        }
        miette::bail!(
            "'--output {release_dir}' already exists and was not written by \
             `beamtalk release` (no '{RELEASE_MARKER}' marker file) — refusing to delete it.\n\n\
             \x20 Pass a different --output path or remove '{release_dir}' yourself. \
             \x20 --force-output replaces it only if it is an unmarked release directory."
        );
    }
    if !release_dir.exists() {
        return Ok(());
    }
    // Safety net 1: the internally-computed path must be under
    // `_build/`. `release_dir` is already absolutized by the caller, so
    // `build_root()` is compared the same way.
    let build_root = assembly::absolutize(&layout.build_root())?;
    assert!(
        release_dir.starts_with(&build_root),
        "computed release dir {release_dir} escapes build root {build_root}"
    );

    // Safety net 2: `starts_with` is purely lexical, so it cannot see
    // a symlinked `_build` (or `_build/release`) — removing a *sub-path*
    // of one would traverse the link and delete the contents of its
    // target, outside the project. Refuse outright rather than offering
    // a force flag: a symlinked `_build` is something to fix, not
    // delete through.
    if beamtalk_cli::path_util::has_symlink_in_chain(release_dir, &build_root)? {
        miette::bail!(
            "Refusing to clean: '{release_dir}' (or a directory between it and \
             '{build_root}') is a symlink.\n\n\
             \x20 Remove the symlink manually, or pass an explicit --output pointing \
             \x20 outside '{build_root}'."
        );
    }
    Ok(())
}

/// Directory `systools:make_tar/2`'s tarball is written to: the release
/// directory's parent (ADR 0125 §1.1: `_build/release/<name>-<vsn>.tar.gz`
/// beside `<name>-<vsn>/`).
fn tarball_out_dir(release_dir: &Utf8Path) -> Utf8PathBuf {
    release_dir
        .parent()
        .map_or_else(|| release_dir.to_path_buf(), Utf8Path::to_path_buf)
}

/// Where this build's `<name>-<vsn>.tar.gz` ends up (see `assembly::make_tarball`).
fn release_tarball_path(release_dir: &Utf8Path, name: &str, vsn: &str) -> Utf8PathBuf {
    tarball_out_dir(release_dir).join(format!("{name}-{vsn}.tar.gz"))
}

/// `<path>.beamtalk-previous`, where a previous output is parked.
fn parked_path(path: &Utf8Path) -> Utf8PathBuf {
    Utf8PathBuf::from(format!("{path}{PREVIOUS_SUFFIX}"))
}

/// `<release_dir>.beamtalk-swap`, the [`JOURNAL_SUFFIX`] journal.
fn journal_path(release_dir: &Utf8Path) -> Utf8PathBuf {
    Utf8PathBuf::from(format!("{release_dir}{JOURNAL_SUFFIX}"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SwapState {
    /// The previous output is (being) parked; the new release is not complete.
    Pending,
    /// The new release is complete; the parked copies are garbage.
    Committed,
}

/// The contents of a swap journal.
#[derive(Debug, Clone)]
struct SwapJournal {
    state: SwapState,
    /// The tarball path of the run that wrote the journal, which differs from
    /// the current run's after a version bump.
    tarball: Utf8PathBuf,
    /// Whether the release directory / tarball existed when the run began (so
    /// was, or was about to be, parked). One that did not was created by the
    /// run, and a `pending` recovery removes it rather than leaving a new
    /// output beside a restored old one.
    had_dir: bool,
    had_tarball: bool,
}

/// Atomically (write a temporary file, flush, rename) set the journal of the
/// swap of `release_dir`.
fn write_journal(release_dir: &Utf8Path, journal: &SwapJournal) -> Result<()> {
    let path = journal_path(release_dir);
    let tmp = Utf8PathBuf::from(format!("{path}.tmp"));
    let state_word = match journal.state {
        SwapState::Pending => "pending",
        SwapState::Committed => "committed",
    };
    let yes_no = |b: bool| if b { "yes" } else { "no" };
    let content = format!(
        "{JOURNAL_HEADER}\n{state_word}\n{}\nprevious-dir: {}\nprevious-tarball: {}\n",
        journal.tarball,
        yes_no(journal.had_dir),
        yes_no(journal.had_tarball)
    );
    let write = || -> std::io::Result<()> {
        let mut file = std::fs::File::create(tmp.as_std_path())?;
        std::io::Write::write_all(&mut file, content.as_bytes())?;
        file.sync_all()?;
        std::fs::rename(tmp.as_std_path(), path.as_std_path())
    };
    if let Err(e) = write() {
        let _ = std::fs::remove_file(tmp.as_std_path());
        return Err(e)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to write the release swap journal '{path}'"));
    }
    sync_parent_dir(&path);
    Ok(())
}

/// Best-effort flush of `path`'s directory entry (Unix only: a directory
/// cannot be opened for flushing on Windows).
fn sync_parent_dir(path: &Utf8Path) {
    #[cfg(unix)]
    if let Some(dir) = path
        .parent()
        .and_then(|parent| std::fs::File::open(parent.as_std_path()).ok())
    {
        let _ = dir.sync_all();
    }
    #[cfg(not(unix))]
    let _ = path;
}

/// Flush one file's data to disk.
///
/// On Windows `sync_all` is `FlushFileBuffers`, which needs a handle with
/// write access: a read-only `File::open` handle fails with "Access is
/// denied" (os error 5), which failed every release swap there. A file whose
/// read-only attribute refuses a write handle (copied that way from the
/// install) cannot be flushed this way at all, so it is skipped.
fn sync_file(path: &std::path::Path) -> std::io::Result<()> {
    #[cfg(windows)]
    {
        match std::fs::OpenOptions::new().write(true).open(path) {
            Ok(file) => file.sync_all(),
            Err(e)
                if e.kind() == std::io::ErrorKind::PermissionDenied
                    && std::fs::metadata(path).is_ok_and(|m| m.permissions().readonly()) =>
            {
                Ok(())
            }
            Err(e) => Err(e),
        }
    }
    #[cfg(not(windows))]
    {
        std::fs::File::open(path)?.sync_all()
    }
}

/// Flush the new release — every file under `release_dir`, the tarball, and
/// their directory entries — so a `committed` journal can never outlive, on
/// power loss, the release it vouches for.
fn sync_new_release(release_dir: &Utf8Path, tarball: &Utf8Path) -> Result<()> {
    fn sync_tree(dir: &std::path::Path) -> std::io::Result<()> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let file_type = entry.file_type()?;
            if file_type.is_dir() {
                sync_tree(&entry.path())?;
            } else if file_type.is_file() {
                sync_file(&entry.path())?;
            }
        }
        #[cfg(unix)]
        std::fs::File::open(dir)?.sync_all()?;
        Ok(())
    }
    if release_dir.is_dir() {
        sync_tree(release_dir.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to flush the new release '{release_dir}' to disk"))?;
    }
    if tarball.is_file() {
        sync_file(tarball.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to flush '{tarball}' to disk"))?;
    }
    sync_parent_dir(release_dir);
    sync_parent_dir(tarball);
    Ok(())
}

/// Read the journal of the swap of `release_dir`, if there is one. A journal
/// that is not one of ours, or names a tarball anywhere but beside
/// `release_dir`, is an error: recovery then touches nothing.
fn read_journal(release_dir: &Utf8Path) -> Result<Option<SwapJournal>> {
    let path = journal_path(release_dir);
    if path.symlink_metadata().is_err() {
        return Ok(None);
    }
    let unrecognised = || {
        miette::miette!(
            "'{path}' is not a `beamtalk release` swap journal, so the state of an \
             interrupted run cannot be recovered automatically. Check '{release_dir}' and any \
             '*{PREVIOUS_SUFFIX}' beside it, keep the release you want, remove the rest and \
             '{path}', then re-run."
        )
    };
    let text = if path.symlink_metadata().is_ok_and(|m| m.is_file()) {
        std::fs::read_to_string(path.as_std_path()).map_err(|_| unrecognised())?
    } else {
        return Err(unrecognised());
    };
    let mut lines = text.lines();
    if lines.next() != Some(JOURNAL_HEADER) {
        return Err(unrecognised());
    }
    let state = match lines.next() {
        Some("pending") => SwapState::Pending,
        Some("committed") => SwapState::Committed,
        _ => return Err(unrecognised()),
    };
    let tarball = Utf8PathBuf::from(lines.next().ok_or_else(unrecognised)?);
    let beside = tarball.parent() == Some(tarball_out_dir(release_dir).as_path())
        && tarball.file_name().is_some_and(|n| n.ends_with(".tar.gz"));
    let mut flag = |key: &str| match lines.next().and_then(|l| l.strip_prefix(key)) {
        Some("yes") => Some(true),
        Some("no") => Some(false),
        _ => None,
    };
    let (Some(had_dir), Some(had_tarball)) = (flag("previous-dir: "), flag("previous-tarball: "))
    else {
        return Err(unrecognised());
    };
    if !beside || lines.next().is_some() {
        return Err(unrecognised());
    }
    Ok(Some(SwapJournal {
        state,
        tarball,
        had_dir,
        had_tarball,
    }))
}

/// Remove `path`, a directory tree or a single file. A symlink is removed,
/// never followed (on Windows a directory link or junction needs
/// `remove_dir`, a file link `remove_file`).
fn remove_output_path(path: &Utf8Path) -> std::io::Result<()> {
    let meta = path.symlink_metadata()?;
    if meta.file_type().is_symlink() {
        std::fs::remove_file(path.as_std_path())
            .or_else(|_| std::fs::remove_dir(path.as_std_path()))
    } else if meta.is_dir() {
        std::fs::remove_dir_all(path.as_std_path())
    } else {
        std::fs::remove_file(path.as_std_path())
    }
}

/// What `--upgrade-from` names, relative to the output being replaced.
enum UpgradeFrom {
    Path(Utf8PathBuf),
    /// The output directory about to be parked.
    ReplacedDir,
    /// The output tarball about to be parked.
    ReplacedTarball,
}

/// Reject a missing `--upgrade-from` path before any work is done.
fn validate_upgrade_from(prev: &str) -> Result<()> {
    if !Utf8Path::new(prev).exists() {
        miette::bail!(
            "--upgrade-from '{prev}' does not exist.\n\n\
             \x20 Pass a previous `beamtalk release` output directory or its .tar.gz."
        );
    }
    Ok(())
}

/// Whether `--upgrade-from` names the directory or tarball this build is
/// about to replace (`beamtalk release --output dist --upgrade-from dist`).
fn classify_upgrade_from(prev: &str, release_dir: &Utf8Path, tarball: &Utf8Path) -> UpgradeFrom {
    let same = |a: &Utf8Path, b: &Utf8Path| {
        matches!(
            (std::fs::canonicalize(a.as_std_path()), std::fs::canonicalize(b.as_std_path())),
            (Ok(a), Ok(b)) if a == b
        )
    };
    let prev = Utf8Path::new(prev);
    if same(prev, release_dir) {
        UpgradeFrom::ReplacedDir
    } else if same(prev, tarball) {
        UpgradeFrom::ReplacedTarball
    } else {
        UpgradeFrom::Path(prev.to_path_buf())
    }
}

/// Whether the file at `path` is a gzip'd tar written by `beamtalk release`
/// (it carries `releases/<vsn>/beamtalk-provenance.json`).
fn is_previous_release_tarball(path: &Utf8Path) -> bool {
    let Ok(file) = std::fs::File::open(path.as_std_path()) else {
        return false;
    };
    let mut archive = tar::Archive::new(flate2::read::GzDecoder::new(file));
    let Ok(entries) = archive.entries() else {
        return false;
    };
    entries.flatten().any(|e| {
        e.path().is_ok_and(|p| {
            p.file_name()
                .is_some_and(|n| n == "beamtalk-provenance.json")
                && p.components().count() == 3
                && p.starts_with("releases")
        })
    })
}

/// Whether the file at `path` is a readable gzip'd tar archive (of any origin).
fn is_gzip_tar(path: &Utf8Path) -> bool {
    let Ok(file) = std::fs::File::open(path.as_std_path()) else {
        return false;
    };
    let mut archive = tar::Archive::new(flate2::read::GzDecoder::new(file));
    archive
        .entries()
        .is_ok_and(|mut entries| entries.next().is_some_and(|e| e.is_ok()))
}

/// Validate that an existing file at the tarball path may be replaced: it
/// must be a regular file and, for a user-supplied `--output`, a tarball an
/// earlier `beamtalk release` wrote — or, with `--force-output`, at least a
/// gzip'd tar archive (BT-5187: never an arbitrary file).
fn ensure_clean_tarball(
    tarball: &Utf8Path,
    user_supplied_output: bool,
    force_output: bool,
) -> Result<()> {
    let Ok(meta) = tarball.symlink_metadata() else {
        return Ok(());
    };
    if !meta.is_file() {
        miette::bail!(
            "'{tarball}' already exists and is not a regular file — refusing to replace it."
        );
    }
    if user_supplied_output && !is_previous_release_tarball(tarball) {
        if !force_output {
            miette::bail!(
                "'{tarball}' already exists and was not written by `beamtalk release` — \
                 refusing to replace it.\n\n\
                 \x20 Pass a different --output path, remove it yourself, or pass \
                 --force-output if it is a release tarball from another tool."
            );
        }
        if !is_gzip_tar(tarball) {
            miette::bail!(
                "'{tarball}' already exists and is not a gzip'd tar archive — refusing to \
                 replace it, even with --force-output."
            );
        }
    }
    Ok(())
}

/// Undo or finish an interrupted run (BT-5134, BT-5187).
///
/// `ReleaseSwap::begin` writes a `pending` journal, then parks the previous
/// output at `<path>.beamtalk-previous`; `ReleaseSwap::commit` flips the
/// journal to `committed` and only then discards the parked copies. So at
/// startup:
///
/// * a `committed` journal means the last run finished its new release and
///   died (or failed) while discarding the old one: the outputs are complete
///   and left alone, and the parked leftovers are removed;
/// * a `pending` journal, or a parked copy with no journal (left by a build
///   from before the journal existed), means the last run died before its
///   new release was complete: whatever partial output of ours sits at the
///   path (a marked or empty directory, or a tarball carrying this command's
///   provenance manifest) is removed and the parked copy restored. Anything
///   else at the path is left alone and the run refused.
///
/// The journal names the tarball of the run that wrote it, so a version bump
/// in between still finds that run's parked tarball.
fn recover_interrupted_release(release_dir: &Utf8Path, tarball: &Utf8Path) -> Result<()> {
    let journal = read_journal(release_dir)?;
    let tarball = journal.as_ref().map_or(tarball, |j| j.tarball.as_path());
    match &journal {
        Some(j) if j.state == SwapState::Committed => {
            finish_interrupted_commit(release_dir, tarball)?;
        }
        Some(j) => restore_parked_release(release_dir, tarball, Some((j.had_dir, j.had_tarball)))?,
        None => restore_parked_release(release_dir, tarball, None)?,
    }
    if journal.is_some() {
        let path = journal_path(release_dir);
        remove_output_path(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove the release swap journal '{path}'"))?;
    }
    Ok(())
}

/// Put back the previous output parked by a run that never committed.
/// `had` is the journal's (directory, tarball) existed-at-begin flags, when
/// there is a journal: an output that did not exist was created by the
/// interrupted run, so it is removed (if it is ours) rather than left beside
/// a restored previous output.
fn restore_parked_release(
    release_dir: &Utf8Path,
    tarball: &Utf8Path,
    had: Option<(bool, bool)>,
) -> Result<()> {
    // Only discard what a build of ours could have left: a marked (or still
    // empty) directory, or a tarball this command wrote.
    let is_ours = |path: &Utf8Path, meta: &std::fs::Metadata| {
        if path == release_dir {
            meta.is_dir() && (path.join(RELEASE_MARKER).is_file() || is_empty_dir(path))
        } else {
            meta.is_file() && is_previous_release_tarball(path)
        }
    };
    let had = had.map(|(d, t)| [d, t]);
    for (i, path) in [release_dir, tarball].into_iter().enumerate() {
        let parked = parked_path(path);
        if parked.symlink_metadata().is_err() {
            if had.is_some_and(|h| !h[i])
                && let Ok(meta) = path.symlink_metadata()
                && is_ours(path, &meta)
            {
                eprintln!("Removing the partial output of an interrupted run: '{path}'");
                remove_output_path(path)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("Failed to remove the partial output '{path}'"))?;
            }
            continue;
        }
        if let Ok(meta) = path.symlink_metadata() {
            if !is_ours(path, &meta) {
                miette::bail!(
                    "'{parked}' is left over from an interrupted `beamtalk release`, but \
                     '{path}' now holds something else. Move one of them, then re-run."
                );
            }
            remove_output_path(path)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to remove the partial output '{path}'"))?;
        }
        eprintln!("Restoring the previous release left by an interrupted run: '{path}'");
        std::fs::rename(parked.as_std_path(), path.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to restore '{parked}' to '{path}'"))?;
    }
    Ok(())
}

/// The last run committed but did not finish discarding the previous
/// output: discard what is left, never touching the committed outputs.
fn finish_interrupted_commit(release_dir: &Utf8Path, tarball: &Utf8Path) -> Result<()> {
    for path in [release_dir, tarball] {
        let parked = parked_path(path);
        if parked.symlink_metadata().is_err() {
            continue;
        }
        eprintln!("Removing the previous release left by an interrupted run: '{parked}'");
        remove_output_path(&parked)
            .into_diagnostic()
            .wrap_err_with(|| {
                format!(
                    "Failed to remove '{parked}', the previous release a completed `beamtalk \
                     release` replaced. Remove it yourself, then re-run."
                )
            })?;
    }
    Ok(())
}

/// Transactional replacement of a previous release's output: its directory
/// and its tarball (which lives *beside* the directory, so it is not covered
/// by parking the directory alone).
///
/// [`ReleaseSwap::begin`] writes a `pending` journal ([`JOURNAL_SUFFIX`]) and
/// parks both in `<path>.beamtalk-previous` siblings. [`ReleaseSwap::commit`]
/// marks the journal `committed`, then discards them (see
/// [`recover_interrupted_release`] for a run killed at any point). Dropping
/// the guard uncommitted — an `Err` return or a panic unwinding through the
/// assembly — discards the partial new output and puts the previous one back.
struct ReleaseSwap {
    release_dir: Utf8PathBuf,
    parked_dir: Option<Utf8PathBuf>,
    tarball: Utf8PathBuf,
    parked_tarball: Option<Utf8PathBuf>,
    /// Whether `release_dir`/`tarball` are this build's own (absent at
    /// `begin`, or parked away). Rollback never deletes a path it does not
    /// own: a half-done `begin` must leave the live previous output alone.
    owns_dir: bool,
    owns_tarball: bool,
    committed: bool,
}

impl ReleaseSwap {
    /// Park an existing (already validated) `release_dir` and `tarball` so the
    /// new release can be assembled in their place.
    fn begin(release_dir: &Utf8Path, tarball: Utf8PathBuf) -> Result<Self> {
        let dir_exists = release_dir.exists();
        let tar_exists = tarball.exists();
        // Refuse before moving anything if an interrupted run left anything
        // behind. (Recovery has already run, so normally there is none;
        // concurrent runs on one output are not detected — BT-5230.)
        let journal = journal_path(release_dir);
        if journal.symlink_metadata().is_ok() {
            miette::bail!(
                "'{journal}' is left over from an interrupted `beamtalk release` (or another run is \
                 writing this output); re-run once no other run is active and it is recovered."
            );
        }
        for (exists, path) in [(dir_exists, release_dir), (tar_exists, tarball.as_path())] {
            let parked = parked_path(path);
            if exists && parked.exists() {
                miette::bail!(
                    "'{parked}' is left over from an interrupted `beamtalk release`. It holds \
                     the previous release; move or remove it, then re-run."
                );
            }
        }
        let mut swap = Self {
            release_dir: release_dir.to_path_buf(),
            parked_dir: None,
            tarball,
            parked_tarball: None,
            owns_dir: !dir_exists,
            owns_tarball: !tar_exists,
            committed: false,
        };
        if dir_exists || tar_exists {
            write_journal(
                release_dir,
                &SwapJournal {
                    state: SwapState::Pending,
                    tarball: swap.tarball.clone(),
                    had_dir: dir_exists,
                    had_tarball: tar_exists,
                },
            )?;
        }
        // From here a failed step returns early and `Drop` restores what was parked.
        if dir_exists {
            let parked = parked_path(release_dir);
            std::fs::rename(release_dir.as_std_path(), parked.as_std_path())
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to move the existing '{release_dir}' aside"))?;
            swap.parked_dir = Some(parked);
            swap.owns_dir = true;
        }
        if tar_exists {
            let parked = parked_path(&swap.tarball);
            std::fs::rename(swap.tarball.as_std_path(), parked.as_std_path())
                .into_diagnostic()
                .wrap_err_with(|| {
                    format!("Failed to move the existing '{}' aside", swap.tarball)
                })?;
            swap.parked_tarball = Some(parked);
            swap.owns_tarball = true;
        }
        Ok(swap)
    }

    /// The new release is complete: mark the journal `committed`, then
    /// discard the parked previous output. If the journal cannot be updated,
    /// the swap is rolled back (the previous release restored) and the error
    /// returned.
    fn commit(self) -> Result<()> {
        self.commit_with(remove_output_path)
    }

    /// [`Self::commit`] with an injectable remover (tests use it to simulate a
    /// failed removal, or a kill — a panic — at each step). Every removal
    /// after the journal is committed is best-effort: what is left behind is
    /// finished by the next run's [`recover_interrupted_release`].
    fn commit_with(mut self, remove: impl Fn(&Utf8Path) -> std::io::Result<()>) -> Result<()> {
        // Nothing was parked, so there is no journal and nothing to discard.
        if self.parked_dir.is_none() && self.parked_tarball.is_none() {
            self.committed = true;
            return Ok(());
        }
        // On failure `self` drops uncommitted, restoring the previous output.
        sync_new_release(&self.release_dir, &self.tarball)?;
        write_journal(
            &self.release_dir,
            &SwapJournal {
                state: SwapState::Committed,
                tarball: self.tarball.clone(),
                had_dir: self.parked_dir.is_some(),
                had_tarball: self.parked_tarball.is_some(),
            },
        )?;
        // From here on the new release is the release: nothing rolls back.
        self.committed = true;
        let mut all_removed = true;
        for parked in [self.parked_dir.take(), self.parked_tarball.take()]
            .into_iter()
            .flatten()
        {
            if let Err(e) = remove(&parked) {
                all_removed = false;
                eprintln!(
                    "warning: could not remove the previous release at '{parked}': {e} — the \
                     next `beamtalk release` will remove it"
                );
            }
        }
        let journal = journal_path(&self.release_dir);
        if all_removed && let Err(e) = remove(&journal) {
            eprintln!("warning: could not remove '{journal}': {e}");
        }
        Ok(())
    }

    fn rollback(&mut self) {
        if self.owns_dir {
            let _ = std::fs::remove_dir_all(self.release_dir.as_std_path());
        }
        if self.owns_tarball {
            let _ = std::fs::remove_file(self.tarball.as_std_path());
        }
        let mut restored = true;
        for (parked, original) in [
            (self.parked_dir.take(), &self.release_dir),
            (self.parked_tarball.take(), &self.tarball),
        ] {
            let Some(parked) = parked else { continue };
            if let Err(e) = std::fs::rename(parked.as_std_path(), original.as_std_path()) {
                restored = false;
                eprintln!(
                    "error: could not restore the previous release; it is at '{parked}' and \
                     the next `beamtalk release` will restore it: {e}"
                );
            }
        }
        // A failed restore keeps the `pending` journal for the next run. (No
        // other run's journal can be here: `begin` refuses while one exists.)
        if restored {
            let _ = std::fs::remove_file(journal_path(&self.release_dir).as_std_path());
        }
    }
}

impl Drop for ReleaseSwap {
    fn drop(&mut self) {
        if !self.committed {
            self.rollback();
        }
    }
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
        let err = build_release(&root, None, &options, false, false, false, None).unwrap_err();
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
        let err = build_release(&root, None, &options, false, false, false, None).unwrap_err();
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

    /// `generate_vm_args` writes `-sname {name}@localhost` (ADR 0125
    /// §1.6); a `name` that itself contains `@` would produce a two-`@`
    /// short name `erl` refuses to boot with at all.
    #[test]
    fn validate_release_path_component_rejects_at_sign() {
        let err = validate_release_path_component("name", "orders@evil").unwrap_err();
        assert!(err.to_string().contains('@'), "{err}");
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

    /// BT-5067: shell/cmd metacharacters must not reach the generated launchers'
    /// `RELEASE_NAME="…"` line.
    #[test]
    fn validate_release_path_component_rejects_launcher_metacharacters() {
        for bad in [
            "a\"b", "a$b", "a`b", "a%b", "a&b", "a^b", "a;b", "a|b", "a'b", "ünï",
        ] {
            assert!(
                validate_release_path_component("name", bad).is_err(),
                "expected '{bad}' to be rejected"
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
        assert!(err.to_string().contains(RELEASE_MARKER), "got: {err}");
        assert!(err.to_string().contains("--force-output"), "got: {err}");
        // Refused: the directory and its contents must be untouched.
        assert!(target.join("important.txt").is_file());
    }

    /// BT-5187: `--force-output` no longer accepts an arbitrary directory
    /// (`--output ~/Documents`, `/usr/local`): only an unmarked one that is
    /// release-shaped.
    #[test]
    fn ensure_clean_release_dir_force_output_refuses_a_non_release_dir() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(root.join("proj"));

        let documents = root.join("Documents");
        fs::create_dir_all(documents.as_std_path()).unwrap();
        fs::write(documents.join("important.txt").as_std_path(), "keep me").unwrap();
        let usr_local = root.join("usr_local");
        for sub in ["bin", "lib", "share", "releases"] {
            fs::create_dir_all(usr_local.join(sub).as_std_path()).unwrap();
        }
        for target in [&documents, &usr_local] {
            let err = ensure_clean_release_dir(target, true, true, &layout).unwrap_err();
            assert!(
                err.to_string().contains("even with --force-output"),
                "{err}"
            );
        }
        assert!(documents.join("important.txt").is_file());
    }

    /// BT-5187: `--force-output` accepts an unmarked, release-shaped directory
    /// (a `releases/` directory and only release entries); without the flag
    /// it is refused. An empty directory needs no flag.
    #[test]
    fn ensure_clean_release_dir_force_output_accepts_only_release_shaped_dirs() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(root.join("proj"));

        let shaped = root.join("shaped");
        for sub in ["bin", "lib/app-1/ebin", "releases/1.0.0", "erts-14.0/bin"] {
            fs::create_dir_all(shaped.join(sub).as_std_path()).unwrap();
        }
        ensure_clean_release_dir(&shaped, true, true, &layout).unwrap();
        assert!(ensure_clean_release_dir(&shaped, true, false, &layout).is_err());

        let empty = root.join("empty");
        fs::create_dir_all(empty.as_std_path()).unwrap();
        ensure_clean_release_dir(&empty, true, false, &layout).unwrap();
    }

    /// BT-5187: an `--output` inside a source directory is refused, forced
    /// or not, existing (even marked) or not.
    #[test]
    fn ensure_clean_release_dir_refuses_output_inside_a_source_dir() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let project = root.join("proj");
        fs::create_dir_all(project.join("src").as_std_path()).unwrap();
        let layout = BuildLayout::new(&project);

        let marked = project.join("src/generated");
        fs::create_dir_all(marked.as_std_path()).unwrap();
        write_release_marker(&marked).unwrap();
        let missing = project.join("src/out/rel");
        let missing_src = project.join("test/rel");
        for target in [&marked, &missing, &missing_src] {
            for force in [false, true] {
                let err = ensure_clean_release_dir(target, true, force, &layout).unwrap_err();
                assert!(err.to_string().contains("source directories"), "{err}");
            }
        }
    }

    /// `canonicalize_lenient` resolves a missing tail under an existing
    /// prefix, including a `..` inside that tail (`--output build/../dist`
    /// with no `build/`).
    #[test]
    fn canonicalize_lenient_handles_missing_paths() {
        let temp = TempDir::new().unwrap();
        let base = std::fs::canonicalize(temp.path()).unwrap();
        assert_eq!(
            canonicalize_lenient(&temp.path().join("a/b")),
            Some(base.join("a/b"))
        );
        assert_eq!(
            canonicalize_lenient(&temp.path().join("a/../b")),
            Some(base.join("b"))
        );
        assert_eq!(
            canonicalize_lenient(&temp.path().join("a/../../src/x")),
            base.parent().map(|p| p.join("src/x"))
        );
    }

    /// BT-5071: a directory with only `lib/` is not a release (a project's
    /// own `lib/`, `~/opt`, `/usr/local`, …) — it must be refused.
    #[test]
    fn ensure_clean_release_dir_refuses_a_dir_with_only_lib_or_releases() {
        for sub in ["lib", "releases"] {
            let temp = TempDir::new().unwrap();
            let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
            let layout = BuildLayout::new(root.join("proj"));
            let target = root.join("dist");
            fs::create_dir_all(target.join(sub).as_std_path()).unwrap();
            fs::write(target.join("sentinel.txt").as_std_path(), "keep").unwrap();

            let err = ensure_clean_release_dir(&target, true, false, &layout).unwrap_err();
            assert!(err.to_string().contains(RELEASE_MARKER), "{err}");
            assert!(target.join("sentinel.txt").is_file());
        }
    }

    #[test]
    fn ensure_clean_release_dir_accepts_marked_and_legacy_provenance_output() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(root.join("proj"));

        let marked = root.join("marked");
        fs::create_dir_all(marked.as_std_path()).unwrap();
        write_release_marker(&marked).unwrap();
        ensure_clean_release_dir(&marked, true, false, &layout).unwrap();

        let legacy = root.join("legacy");
        fs::create_dir_all(legacy.join("releases/1.0.0").as_std_path()).unwrap();
        fs::write(
            legacy
                .join("releases/1.0.0/beamtalk-provenance.json")
                .as_std_path(),
            "{}",
        )
        .unwrap();
        ensure_clean_release_dir(&legacy, true, false, &layout).unwrap();
    }

    /// The project root and its ancestors are refused even with
    /// `--force-output`, marker or not.
    #[test]
    fn ensure_clean_release_dir_always_refuses_project_root_and_ancestors() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let project = root.join("proj");
        fs::create_dir_all(project.as_std_path()).unwrap();
        write_release_marker(&project).unwrap();
        write_release_marker(&root).unwrap();
        let layout = BuildLayout::new(&project);

        for target in [&project, &root] {
            for force in [false, true] {
                let err = ensure_clean_release_dir(target, true, force, &layout).unwrap_err();
                assert!(err.to_string().contains("project root"), "{err}");
            }
        }
    }

    #[test]
    fn ensure_clean_release_dir_accepts_internally_computed_dir_without_force() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let target = layout.release_dir("orders", "1.0.0");
        fs::create_dir_all(target.as_std_path()).unwrap();
        ensure_clean_release_dir(&target, false, false, &layout).unwrap();
    }

    fn write(path: &Utf8Path, content: &str) {
        fs::write(path.as_std_path(), content).unwrap();
    }

    /// A failed assembly puts the previous release (directory *and* tarball)
    /// back untouched; a successful one discards both.
    #[test]
    fn release_swap_restores_previous_on_failure_and_discards_on_success() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        fs::create_dir_all(target.as_std_path()).unwrap();
        write(&target.join("old.txt"), "old");
        write(&tarball, "old tarball");

        // Failure: dropping the guard uncommitted restores both.
        {
            let _swap = ReleaseSwap::begin(&target, tarball.clone()).unwrap();
            assert!(!target.exists() && !tarball.exists());
            fs::create_dir_all(target.as_std_path()).unwrap();
            write(&target.join("partial.txt"), "new");
            write(&tarball, "new tarball");
        }
        assert!(target.join("old.txt").is_file());
        assert!(!target.join("partial.txt").exists());
        assert_eq!(
            fs::read_to_string(tarball.as_std_path()).unwrap(),
            "old tarball"
        );
        assert!(!parked_path(&target).exists() && !parked_path(&tarball).exists());

        // Success: commit discards both parked outputs.
        let swap = ReleaseSwap::begin(&target, tarball.clone()).unwrap();
        fs::create_dir_all(target.as_std_path()).unwrap();
        write(&target.join("new.txt"), "new");
        write(&tarball, "new tarball");
        swap.commit().unwrap();
        assert!(target.join("new.txt").is_file());
        assert!(!target.join("old.txt").exists());
        assert_eq!(
            fs::read_to_string(tarball.as_std_path()).unwrap(),
            "new tarball"
        );
        assert!(!parked_path(&target).exists() && !parked_path(&tarball).exists());
        assert!(!journal_path(&target).exists());
    }

    /// BT-5107: a panic part-way through assembly must not skip the restore.
    #[test]
    fn release_swap_restores_previous_on_panic() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        fs::create_dir_all(target.as_std_path()).unwrap();
        write(&target.join("old.txt"), "old");
        write(&tarball, "old tarball");

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _swap = ReleaseSwap::begin(&target, tarball.clone()).unwrap();
            fs::create_dir_all(target.as_std_path()).unwrap();
            write(&tarball, "new tarball");
            panic!("boom");
        }));
        assert!(result.is_err());
        assert!(target.join("old.txt").is_file());
        assert_eq!(
            fs::read_to_string(tarball.as_std_path()).unwrap(),
            "old tarball"
        );
    }

    /// A guard dropped before it parked (or owned) anything — what a `begin`
    /// that fails part-way leaves behind — must not delete the live previous
    /// output. (Constructed by hand: a rename failure cannot be induced
    /// portably, e.g. permissions are ignored when running as root.)
    #[test]
    fn release_swap_dropped_before_parking_keeps_live_previous_output() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        fs::create_dir_all(target.as_std_path()).unwrap();
        write(&target.join("old.txt"), "old");
        write(&tarball, "old tarball");
        drop(ReleaseSwap {
            release_dir: target.clone(),
            parked_dir: None,
            tarball: tarball.clone(),
            parked_tarball: None,
            owns_dir: false,
            owns_tarball: false,
            committed: false,
        });
        assert!(target.join("old.txt").is_file());
        assert_eq!(
            fs::read_to_string(tarball.as_std_path()).unwrap(),
            "old tarball"
        );
    }

    /// With no previous output, a failure leaves nothing behind.
    #[test]
    fn release_swap_failure_without_previous_removes_partial_output() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        {
            let _swap = ReleaseSwap::begin(&target, tarball.clone()).unwrap();
            fs::create_dir_all(target.as_std_path()).unwrap();
            write(&tarball, "partial");
        }
        assert!(!target.exists() && !tarball.exists());
    }

    /// A parked leftover from an interrupted run refuses the build before
    /// anything is moved.
    #[test]
    fn release_swap_refuses_leftover_parked_tarball() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        fs::create_dir_all(target.as_std_path()).unwrap();
        write(&tarball, "current");
        write(&parked_path(&tarball), "leftover");
        let err = ReleaseSwap::begin(&target, tarball.clone()).err().unwrap();
        assert!(err.to_string().contains("interrupted"), "{err}");
        assert!(target.is_dir(), "nothing may be moved when refusing");
        assert_eq!(
            fs::read_to_string(tarball.as_std_path()).unwrap(),
            "current"
        );
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

    // -- BT-5134: upgrade-from, tarball, recovery, protection -----------

    #[test]
    fn validate_upgrade_from_rejects_missing_path() {
        let err = validate_upgrade_from("/definitely/not/here").unwrap_err();
        assert!(err.to_string().contains("does not exist"), "{err}");
    }

    /// `--upgrade-from dist` while replacing `dist` reads the parked copy.
    #[test]
    fn classify_upgrade_from_recognises_the_replaced_output() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let dir = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        let other = root.join("other");
        fs::create_dir_all(dir.as_std_path()).unwrap();
        fs::create_dir_all(other.as_std_path()).unwrap();
        write(&tarball, "x");
        assert!(matches!(
            classify_upgrade_from(dir.as_str(), &dir, &tarball),
            UpgradeFrom::ReplacedDir
        ));
        assert!(matches!(
            classify_upgrade_from(tarball.as_str(), &dir, &tarball),
            UpgradeFrom::ReplacedTarball
        ));
        assert!(matches!(
            classify_upgrade_from(other.as_str(), &dir, &tarball),
            UpgradeFrom::Path(_)
        ));
    }

    #[test]
    fn ensure_clean_tarball_refuses_unrelated_file_unless_forced() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let tarball = root.join("dist-1.0.0.tar.gz");
        ensure_clean_tarball(&tarball, true, false).unwrap(); // absent
        write(&tarball, "not a tarball");
        let err = ensure_clean_tarball(&tarball, true, false).unwrap_err();
        assert!(err.to_string().contains("not written by"), "{err}");
        // BT-5187: --force-output never replaces an arbitrary file …
        let err = ensure_clean_tarball(&tarball, true, true).unwrap_err();
        assert!(err.to_string().contains("not a gzip'd tar"), "{err}");
        ensure_clean_tarball(&tarball, false, false).unwrap(); // default location
        // … only a tar.gz, e.g. one another tool built.
        let mut builder = tar::Builder::new(flate2::write::GzEncoder::new(
            fs::File::create(tarball.as_std_path()).unwrap(),
            flate2::Compression::fast(),
        ));
        let mut header = tar::Header::new_gnu();
        header.set_size(1);
        header.set_mode(0o644);
        builder
            .append_data(&mut header, "bin/x", &b"x"[..])
            .unwrap();
        builder.into_inner().unwrap().finish().unwrap();
        assert!(ensure_clean_tarball(&tarball, true, false).is_err());
        ensure_clean_tarball(&tarball, true, true).unwrap();
        let dir = root.join("adir.tar.gz");
        fs::create_dir_all(dir.as_std_path()).unwrap();
        assert!(ensure_clean_tarball(&dir, false, true).is_err());
    }

    /// Write a minimal tarball carrying `releases/1.0.0/beamtalk-provenance.json`
    /// (so [`is_previous_release_tarball`] recognises it), with `tag` as its
    /// provenance content.
    fn write_release_tarball(tarball: &Utf8Path, tag: &str) {
        let mut builder = tar::Builder::new(flate2::write::GzEncoder::new(
            fs::File::create(tarball.as_std_path()).unwrap(),
            flate2::Compression::fast(),
        ));
        let mut header = tar::Header::new_gnu();
        header.set_size(tag.len() as u64);
        header.set_mode(0o644);
        builder
            .append_data(
                &mut header,
                "releases/1.0.0/beamtalk-provenance.json",
                tag.as_bytes(),
            )
            .unwrap();
        builder.into_inner().unwrap().finish().unwrap();
    }

    #[test]
    fn ensure_clean_tarball_accepts_a_previous_release_tarball() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let tarball = root.join("dist-1.0.0.tar.gz");
        write_release_tarball(&tarball, "{}");
        ensure_clean_tarball(&tarball, true, false).unwrap();
    }

    /// A parked copy left by a killed run is restored, whether the output
    /// path is absent or holds a partial new build.
    #[test]
    fn recover_interrupted_release_restores_parked_output() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");

        // Output absent.
        fs::create_dir_all(parked_path(&target).as_std_path()).unwrap();
        write(&parked_path(&target).join("old.txt"), "old");
        write(&parked_path(&tarball), "old tarball");
        recover_interrupted_release(&target, &tarball).unwrap();
        assert!(target.join("old.txt").is_file());
        assert_eq!(
            fs::read_to_string(tarball.as_std_path()).unwrap(),
            "old tarball"
        );
        assert!(!parked_path(&target).exists() && !parked_path(&tarball).exists());

        // Partial new output present.
        fs::rename(target.as_std_path(), parked_path(&target).as_std_path()).unwrap();
        fs::rename(tarball.as_std_path(), parked_path(&tarball).as_std_path()).unwrap();
        fs::create_dir_all(target.as_std_path()).unwrap();
        write_release_marker(&target).unwrap();
        write(&target.join("partial.txt"), "new");
        write_release_tarball(&tarball, "new");
        recover_interrupted_release(&target, &tarball).unwrap();
        assert!(target.join("old.txt").is_file() && !target.join("partial.txt").exists());
        assert_eq!(
            fs::read_to_string(tarball.as_std_path()).unwrap(),
            "old tarball"
        );
    }

    #[test]
    fn recover_interrupted_release_refuses_to_delete_foreign_content() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        fs::create_dir_all(parked_path(&target).as_std_path()).unwrap();
        fs::create_dir_all(target.as_std_path()).unwrap();
        write(&target.join("mine.txt"), "keep");
        let err = recover_interrupted_release(&target, &tarball).unwrap_err();
        assert!(err.to_string().contains("interrupted"), "{err}");
        assert!(target.join("mine.txt").is_file());
        assert!(parked_path(&target).is_dir());
    }

    /// BT-5187: with no commit record, recovery removes a file at the
    /// tarball path only if it is a tarball `beamtalk release` wrote.
    #[test]
    fn recover_interrupted_release_keeps_a_foreign_file_at_the_tarball_path() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        write(&parked_path(&tarball), "old tarball");
        write(&tarball, "someone else's file");
        let err = recover_interrupted_release(&target, &tarball).unwrap_err();
        assert!(err.to_string().contains("holds something else"), "{err}");
        assert_eq!(
            fs::read_to_string(tarball.as_std_path()).unwrap(),
            "someone else's file"
        );
        assert!(parked_path(&tarball).is_file());

        // A directory at the tarball path is not ours either.
        fs::remove_file(tarball.as_std_path()).unwrap();
        fs::create_dir_all(tarball.as_std_path()).unwrap();
        assert!(recover_interrupted_release(&target, &tarball).is_err());
        assert!(tarball.is_dir());
    }

    /// Previous release (`old`) parked by `begin`, and a complete new release
    /// (`new`) assembled in its place; returns the swap, the release dir and
    /// the tarball path.
    fn swap_with_new_release(root: &Utf8Path) -> (ReleaseSwap, Utf8PathBuf, Utf8PathBuf) {
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        fs::create_dir_all(target.as_std_path()).unwrap();
        write_release_marker(&target).unwrap();
        write(&target.join("old.txt"), "old");
        write_release_tarball(&tarball, "old");
        let swap = ReleaseSwap::begin(&target, tarball.clone()).unwrap();
        fs::create_dir_all(target.as_std_path()).unwrap();
        write_release_marker(&target).unwrap();
        write(&target.join("new.txt"), "new");
        write_release_tarball(&tarball, "new");
        (swap, target, tarball)
    }

    fn assert_new_release_kept(target: &Utf8Path, tarball: &Utf8Path) {
        assert!(target.join("new.txt").is_file(), "new release dir deleted");
        assert!(!target.join("old.txt").exists(), "old release restored");
        let mut archive = tar::Archive::new(flate2::read::GzDecoder::new(
            fs::File::open(tarball.as_std_path()).unwrap(),
        ));
        let mut entry = archive.entries().unwrap().next().unwrap().unwrap();
        let mut content = String::new();
        std::io::Read::read_to_string(&mut entry, &mut content).unwrap();
        assert_eq!(content, "new", "new tarball replaced");
    }

    /// BT-5187: a run killed at any step of `commit` (simulated by a panic
    /// from the remover, which runs after the commit record is written)
    /// leaves the complete new release in place: the next run's recovery
    /// finishes discarding the old one instead of restoring it.
    #[test]
    fn commit_killed_at_each_step_keeps_the_new_release() {
        // Remover calls, in order: parked dir, parked tarball, commit record.
        for kill_at in 0..3 {
            let temp = TempDir::new().unwrap();
            let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
            let (swap, target, tarball) = swap_with_new_release(&root);
            let calls = std::cell::Cell::new(0);
            let killed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                swap.commit_with(|path| {
                    assert!(calls.get() != kill_at, "killed");
                    calls.set(calls.get() + 1);
                    remove_output_path(path)
                })
            }));
            assert!(killed.is_err(), "step {kill_at}");
            assert_eq!(
                read_journal(&target).unwrap().unwrap().state,
                SwapState::Committed,
                "step {kill_at}"
            );
            assert_new_release_kept(&target, &tarball);

            recover_interrupted_release(&target, &tarball).unwrap();
            assert_new_release_kept(&target, &tarball);
            assert!(!parked_path(&target).exists(), "step {kill_at}");
            assert!(!parked_path(&tarball).exists(), "step {kill_at}");
            assert!(!journal_path(&target).exists(), "step {kill_at}");
            // And the next build proceeds normally.
            drop(ReleaseSwap::begin(&target, tarball.clone()).unwrap());
            assert_new_release_kept(&target, &tarball);
        }
    }

    /// BT-5187: a kill before `commit` (no record yet) still restores the
    /// previous release — the new one never finished.
    #[test]
    fn kill_before_commit_restores_the_previous_release() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let (swap, target, tarball) = swap_with_new_release(&root);
        std::mem::forget(swap); // a kill: no `Drop`, no rollback
        recover_interrupted_release(&target, &tarball).unwrap();
        assert!(target.join("old.txt").is_file() && !target.join("new.txt").exists());
        assert!(!parked_path(&target).exists() && !parked_path(&tarball).exists());
    }

    /// BT-5187: a failed (or partial) removal of the parked copy — a Windows
    /// lock, permissions — leaves the record, so the next run discards the
    /// leftover instead of restoring it over the new release.
    #[test]
    fn commit_with_failed_parked_removal_keeps_the_new_release() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let (swap, target, tarball) = swap_with_new_release(&root);
        let parked_dir = parked_path(&target);
        swap.commit_with(|path| {
            if path == parked_dir {
                // Half-deleted, then the removal fails.
                fs::remove_file(path.join("old.txt").as_std_path()).unwrap();
                return Err(std::io::Error::other("locked"));
            }
            remove_output_path(path)
        })
        .unwrap();
        assert!(parked_dir.is_dir(), "the failed removal left it");
        assert_eq!(
            read_journal(&target).unwrap().unwrap().state,
            SwapState::Committed
        );
        assert_new_release_kept(&target, &tarball);

        recover_interrupted_release(&target, &tarball).unwrap();
        assert_new_release_kept(&target, &tarball);
        assert!(!parked_dir.exists() && !journal_path(&target).exists());
    }

    /// BT-5187: if the journal cannot be marked `committed` (its temporary
    /// file cannot be written), nothing is discarded: the swap rolls back,
    /// the previous release is restored and no journal is left claiming a
    /// commit.
    #[test]
    fn commit_that_cannot_update_the_journal_rolls_back() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let (swap, target, tarball) = swap_with_new_release(&root);
        let journal_tmp = format!("{}.tmp", journal_path(&target));
        fs::create_dir_all(&journal_tmp).unwrap();
        assert!(swap.commit().is_err());
        assert!(target.join("old.txt").is_file() && !target.join("new.txt").exists());
        assert!(!parked_path(&target).exists() && !parked_path(&tarball).exists());
        assert!(!journal_path(&target).exists());
        recover_interrupted_release(&target, &tarball).unwrap();
        assert!(target.join("old.txt").is_file());
    }

    /// BT-5187 (review): the journal names the tarball of the run that wrote
    /// it, so recovery after a version bump still finds that run's parked
    /// tarball — whether it is restored (`pending`) or discarded
    /// (`committed`).
    #[test]
    fn recovery_after_a_version_bump_uses_the_journaled_tarball() {
        // Killed before commit: the 1.0.0 tarball is restored.
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let (swap, target, tarball) = swap_with_new_release(&root);
        std::mem::forget(swap);
        let bumped = root.join("dist-1.0.1.tar.gz");
        recover_interrupted_release(&target, &bumped).unwrap();
        assert!(target.join("old.txt").is_file());
        assert!(tarball.is_file() && !parked_path(&tarball).exists());
        assert!(!journal_path(&target).exists());

        // Killed during commit: the parked 1.0.0 tarball is discarded.
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let (swap, target, tarball) = swap_with_new_release(&root);
        let killed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            swap.commit_with(|_| panic!("killed"))
        }));
        assert!(killed.is_err());
        recover_interrupted_release(&target, &root.join("dist-1.0.1.tar.gz")).unwrap();
        assert_new_release_kept(&target, &tarball);
        assert!(!parked_path(&target).exists() && !parked_path(&tarball).exists());
        assert!(!journal_path(&target).exists());
    }

    /// BT-5187 (review): a run that parked the previous directory but found
    /// no previous tarball, killed after moving its new tarball into place,
    /// does not leave that new tarball beside the restored old directory.
    #[test]
    fn pending_recovery_removes_outputs_the_run_created() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        fs::create_dir_all(target.as_std_path()).unwrap();
        write_release_marker(&target).unwrap();
        write(&target.join("old.txt"), "old");
        let swap = ReleaseSwap::begin(&target, tarball.clone()).unwrap();
        fs::create_dir_all(target.as_std_path()).unwrap();
        write_release_marker(&target).unwrap();
        write_release_tarball(&tarball, "new");
        std::mem::forget(swap);
        recover_interrupted_release(&target, &tarball).unwrap();
        assert!(target.join("old.txt").is_file());
        assert!(
            !tarball.exists(),
            "the new tarball must not outlive the rollback"
        );

        // A live tarball the run had not yet parked (killed between the two
        // parks) is the previous one, and stays.
        let tarball2 = root.join("dist-2.0.0.tar.gz");
        write_release_tarball(&tarball2, "old");
        write_journal(
            &target,
            &SwapJournal {
                state: SwapState::Pending,
                tarball: tarball2.clone(),
                had_dir: true,
                had_tarball: true,
            },
        )
        .unwrap();
        recover_interrupted_release(&target, &tarball2).unwrap();
        assert!(tarball2.is_file() && target.join("old.txt").is_file());
    }

    /// A parked directory symlink is removed, never followed.
    #[test]
    #[cfg(unix)]
    fn remove_output_path_removes_a_directory_link_not_its_target() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::create_dir_all(root.join("real").as_std_path()).unwrap();
        write(&root.join("real/keep.txt"), "keep");
        std::os::unix::fs::symlink("real", root.join("link").as_std_path()).unwrap();
        remove_output_path(&root.join("link")).unwrap();
        assert!(root.join("link").symlink_metadata().is_err());
        assert!(root.join("real/keep.txt").is_file());
    }

    /// BT-5187 (review): a `..` after a missing component that leads back
    /// through a symlink into a source directory is still caught.
    #[test]
    #[cfg(unix)]
    fn is_protected_output_resolves_symlinks_after_a_missing_dotdot() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let project = root.join("proj");
        fs::create_dir_all(project.join("src").as_std_path()).unwrap();
        std::os::unix::fs::symlink("src", project.join("srclink").as_std_path()).unwrap();
        let sneaky = project.join("nope/../srclink/rel");
        assert!(is_protected_output_with(&sneaky, &project, None, None));
        assert!(!is_protected_output_with(
            &project.join("nope/../dist"),
            &project,
            None,
            None
        ));
    }

    /// A journal that is not ours (wrong header, or a tarball path anywhere
    /// but beside the release) stops recovery before anything is touched.
    #[test]
    fn recover_interrupted_release_refuses_an_unrecognised_journal() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let (swap, target, tarball) = swap_with_new_release(&root);
        std::mem::forget(swap);
        for bad in [
            "something else\n".to_string(),
            format!(
                "{JOURNAL_HEADER}\ncommitted\n/elsewhere/x.tar.gz\nprevious-dir: yes\n\
                 previous-tarball: yes\n"
            ),
            format!(
                "{JOURNAL_HEADER}\nmaybe\n{tarball}\nprevious-dir: yes\nprevious-tarball: yes\n"
            ),
            format!("{JOURNAL_HEADER}\ncommitted\n{tarball}\n"),
        ] {
            write(&journal_path(&target), &bad);
            let err = recover_interrupted_release(&target, &tarball).unwrap_err();
            assert!(
                err.to_string()
                    .contains("not a `beamtalk release` swap journal")
            );
            assert!(target.join("new.txt").is_file() && parked_path(&target).is_dir());
            assert!(parked_path(&tarball).is_file());
        }
    }

    /// A journal still present at `begin` (recovery normally consumed it)
    /// refuses the run without moving anything.
    #[test]
    fn release_swap_refuses_while_a_journal_exists() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let target = root.join("dist");
        let tarball = root.join("dist-1.0.0.tar.gz");
        fs::create_dir_all(target.as_std_path()).unwrap();
        write_journal(
            &target,
            &SwapJournal {
                state: SwapState::Pending,
                tarball: tarball.clone(),
                had_dir: true,
                had_tarball: false,
            },
        )
        .unwrap();
        let err = ReleaseSwap::begin(&target, tarball).err().unwrap();
        assert!(err.to_string().contains("interrupted"), "{err}");
        assert!(target.is_dir());
    }

    #[test]
    fn is_protected_output_refuses_home_root_and_source_dirs() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let home = root.join("home");
        let project = home.join("work/proj");
        let elsewhere = root.join("elsewhere");
        for d in [&project.join("src"), &project.join("dist"), &elsewhere] {
            fs::create_dir_all(d.as_std_path()).unwrap();
        }
        let cwd = elsewhere.as_std_path();
        let check = |dir: &Utf8Path| {
            is_protected_output_with(dir, &project, Some(cwd), Some(home.as_std_path()))
        };
        assert!(check(&home), "home directory");
        assert!(check(&home.join("work")), "ancestor of the project");
        assert!(check(&project.join("src")), "source directory");
        assert!(check(&project.join("src/generated")), "inside a source dir");
        assert!(
            check(&project.join("priv/missing/x")),
            "inside a missing one"
        );
        assert!(
            check(Utf8Path::new(std::path::MAIN_SEPARATOR_STR)),
            "filesystem root"
        );
        assert!(!check(&project.join("dist")));
    }
}
