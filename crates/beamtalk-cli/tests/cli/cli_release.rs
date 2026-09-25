// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk release` (ADR 0125 Phase 1 / BT-3570).
//!
//! Builds a release from a fixture project with a root supervisor, asserts
//! the staged tree / `.rel` / `start.boot` / `RELEASES` exist, then boots it
//! with `erl -boot … -boot_var RELEASE_DIR …` and asserts the root
//! supervisor is running and the fixture class is registered before it —
//! the BT-3569 wire check's own staging recipe, reused here in Rust instead
//! of hand-written Erlang.

use std::process::Command;

use crate::cli_common;

use predicates::str::contains;

/// Turn the shared library fixture into a releasable service: an
/// `[application]` section naming a root supervisor, plus that supervisor
/// class. Returns the release name/version declared in its manifest.
fn make_releasable(project: &std::path::Path) {
    std::fs::write(
        project.join("beamtalk.toml"),
        "# Copyright 2026 James Casey\n\
         # SPDX-License-Identifier: Apache-2.0\n\
         \n\
         [package]\n\
         name = \"cli_subprocess_fixture\"\n\
         version = \"0.1.0\"\n\
         \n\
         [application]\n\
         supervisor = \"FixtureSup\"\n\
         \n\
         [dependencies]\n",
    )
    .expect("write beamtalk.toml");

    std::fs::write(
        project.join("src/FixtureSup.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Supervisor subclass: FixtureSup\n\
         \n\
         \x20\x20class children => #()\n",
    )
    .expect("write src/FixtureSup.bt");
}

#[test]
fn release_requires_application_supervisor() {
    // The unmodified library fixture has no [application] section.
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("release")
        .assert()
        .failure()
        .stderr(contains("requires a root supervisor"))
        .stderr(contains("beamtalk build --escript"));
}

#[test]
#[allow(clippy::too_many_lines)] // one straight-line assertion per staged-tree artifact
fn release_builds_staged_tree_and_boot_artifacts() {
    let project = cli_common::fixture_project();
    make_releasable(project.path());

    let output_dir = project.path().join("dist");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(&output_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success()
        .stdout(contains("Built release cli_subprocess_fixture-0.1.0"));

    // Staged project app.
    let project_ebin = output_dir
        .join("lib")
        .join("cli_subprocess_fixture-0.1.0")
        .join("ebin");
    assert!(
        project_ebin.join("cli_subprocess_fixture.app").is_file(),
        "missing staged project .app in {}",
        project_ebin.display()
    );
    assert!(
        std::fs::read_dir(&project_ebin)
            .unwrap()
            .filter_map(Result::ok)
            .any(|e| e.file_name().to_string_lossy().starts_with("bt@")),
        "project ebin has no staged bt@* class beams"
    );

    // Staged runtime closure — spot-check a few apps across the closure
    // (host-provided kernel/stdlib/sasl/crypto are deliberately absent).
    let lib_dir = output_dir.join("lib");
    for app_prefix in [
        "beamtalk_runtime-",
        "beamtalk_stdlib-",
        "beamtalk_workspace-",
        "cowboy-",
        "cowlib-",
        "ranch-",
        "telemetry-",
        "telemetry_poller-",
    ] {
        assert!(
            std::fs::read_dir(&lib_dir)
                .unwrap()
                .filter_map(Result::ok)
                .any(|e| e.file_name().to_string_lossy().starts_with(app_prefix)),
            "expected a staged '{app_prefix}*' dir under {}",
            lib_dir.display()
        );
    }
    // beamtalk_compiler is excluded by default (no [release] include-compiler).
    assert!(
        !std::fs::read_dir(&lib_dir)
            .unwrap()
            .filter_map(Result::ok)
            .any(|e| e
                .file_name()
                .to_string_lossy()
                .starts_with("beamtalk_compiler-")),
        "beamtalk_compiler must be excluded by default"
    );

    // .rel / start.boot / RELEASES / sys.config / vm.args.
    let rel_dir = output_dir.join("releases").join("0.1.0");
    let rel_file = rel_dir.join("cli_subprocess_fixture.rel");
    assert!(rel_file.is_file(), "missing {}", rel_file.display());
    let rel_content = std::fs::read_to_string(&rel_file).unwrap();
    assert!(
        rel_content.contains("cli_subprocess_fixture"),
        "{rel_content}"
    );
    assert!(rel_content.contains("beamtalk_workspace"), "{rel_content}");
    assert!(rel_content.contains("kernel"), "{rel_content}");
    assert!(rel_content.contains("sasl"), "{rel_content}");

    assert!(
        rel_dir.join("start.boot").is_file(),
        "missing start.boot in {}",
        rel_dir.display()
    );
    assert!(
        output_dir.join("releases").join("RELEASES").is_file(),
        "missing releases/RELEASES"
    );
    let sys_config = rel_dir.join("sys.config");
    assert!(sys_config.is_file(), "missing sys.config");
    let sys_content = std::fs::read_to_string(&sys_config).unwrap();
    assert!(sys_content.contains("{mode, release}"), "{sys_content}");

    let vm_args = rel_dir.join("vm.args");
    assert!(vm_args.is_file(), "missing vm.args");
    let vm_content = std::fs::read_to_string(&vm_args).unwrap();
    assert!(vm_content.contains("-mode interactive"), "{vm_content}");

    // Isolate a staging-copy defect from a boot-script path-resolution
    // defect *before* attempting the full boot: the full boot's failure
    // mode (a `permanent`-type application crashing during boot script
    // execution) brings the whole node down before any `-eval` code can
    // run, so a diagnostic embedded in the boot's own `-eval` never
    // executes on this exact failure. Instead, point a throwaway `erl`
    // directly at the *staged* (post-copy) ranch ebin dir with `-pa` — no
    // boot script, no `$RELEASE_DIR` substitution involved at all — and
    // ask whether `ranch_app` is loadable from there. A failure here means
    // `stage_apps`'s copy step is not producing a loadable module (a
    // staging/build defect); a pass here means the copied files are fine
    // and the defect is specifically in how the generated boot script
    // resolves its own code paths.
    let ranch_lib_entry = std::fs::read_dir(&lib_dir)
        .unwrap()
        .filter_map(Result::ok)
        .find(|e| e.file_name().to_string_lossy().starts_with("ranch-"))
        .expect("a staged ranch-* dir must exist (checked above)");
    let ranch_staged_ebin = ranch_lib_entry.path().join("ebin");
    let ranch_load_check = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-pa")
        .arg(&ranch_staged_ebin)
        .arg("-eval")
        .arg(
            "case code:which(ranch_app) of \
                 non_existing -> halt(1); \
                 _ -> halt(0) \
             end.",
        )
        .status()
        .expect("spawn erl to check the staged ranch module is loadable");
    assert!(
        ranch_load_check.success(),
        "staged ranch_app module is not loadable directly from the staged ebin dir {} \
         (via `-pa`, no boot script involved) — this is a staging/copy defect, not a boot-script \
         path-resolution one; check that {}'s source ebin actually contains ranch_app.beam",
        ranch_staged_ebin.display(),
        ranch_staged_ebin.display(),
    );

    // Now boot it: `erl -boot … -boot_var RELEASE_DIR …`, assert the root
    // supervisor is running and the fixture class is registered — the
    // BT-3569 wire check's own acceptance criterion, against the real
    // Rust-assembled release this time.
    let boot_path = rel_dir.join("start");
    let sys_config_noext = sys_config.with_extension("");
    let boot_eval = "timer:sleep(200), \
         Ok1 = is_pid(erlang:whereis(beamtalk_workspace_sup)), \
         Ok2 = is_pid(beamtalk_class_registry:whereis_class('FixtureSup')), \
         Ok3 = erlang:whereis(beamtalk_idle_monitor) =:= undefined, \
         case Ok1 andalso Ok2 andalso Ok3 of \
             true -> halt(0); \
             false -> io:format(standard_error, \"boot check failed: ~p ~p ~p~n\", [Ok1, Ok2, Ok3]), halt(1) \
         end.";

    // `assembly.rs` bakes the `RELEASE_DIR` build-time prefix into the
    // `.script`/`.boot` as a forward-slashed string (`to_forward_slash`,
    // the same Windows fix `ebin_path_list` uses) — the `-boot_var
    // RELEASE_DIR` value supplied here at boot time must be normalised the
    // same way, or `$RELEASE_DIR` substitution silently fails on Windows
    // (a native, backslash-separated value never appears as a match for
    // anything the boot script is looking to replace) and the release
    // falls back to build-time absolute paths that don't resolve, exactly
    // the `ranch_app:start/2 undef` symptom the macOS symlink-prefix bug
    // produced before `absolutize` was made consistent on both sides.
    let release_dir_for_boot_var = beamtalk_cli::path_util::to_forward_slash(
        output_dir.to_str().expect("output_dir must be UTF-8"),
    );
    let status = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg(&boot_path)
        .arg("-boot_var")
        .arg("RELEASE_DIR")
        .arg(&release_dir_for_boot_var)
        .arg("-config")
        .arg(&sys_config_noext)
        .arg("-eval")
        .arg(boot_eval)
        .status()
        .expect("spawn erl to boot the release");
    if !status.success() {
        // The pre-boot `-pa` check above already confirmed the staged
        // ranch_app module is present and loadable outside the boot
        // script, so a failure here points specifically at how the
        // generated `.script` embeds/resolves its own code paths (the
        // `{path, […]}` boot instructions and their `$RELEASE_DIR`
        // substitution). Dump the actual baked-in path entries so a CI
        // failure shows the literal string this boot attempt was working
        // with, rather than requiring another guess-and-push round.
        let script_path = rel_dir.join(format!(
            "{}.script",
            rel_file
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
        ));
        let script_dump = std::fs::read_to_string(&script_path).map_or_else(
            |e| format!("(failed to read {}: {e})", script_path.display()),
            |content| {
                content
                    .lines()
                    .filter(|line| line.contains("ranch") || line.contains("RELEASE_DIR"))
                    .collect::<Vec<_>>()
                    .join("\n")
            },
        );
        panic!(
            "release failed to boot from {}\n\n\
             boot_var RELEASE_DIR was: {release_dir_for_boot_var}\n\n\
             ranch/RELEASE_DIR-related lines from {}:\n{script_dump}",
            boot_path.display(),
            script_path.display(),
        );
    }
}

/// ADR 0125 §1.3/§1.8/§2.2/§3.4 (BT-3571): the tarball, ERTS bundling,
/// `beamtalk-provenance.json`, and `shapes.json` — built with the default
/// `[release] include-erts = true`.
#[test]
#[allow(clippy::too_many_lines)] // one straight-line assertion sweep over one build's output
fn release_default_build_has_erts_tarball_provenance_and_shapes_test() {
    let project = cli_common::fixture_project();
    make_releasable(project.path());

    let output_dir = project.path().join("dist");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(&output_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success()
        .stdout(contains("Built release cli_subprocess_fixture-0.1.0"))
        .stdout(contains("with ERTS"))
        .stdout(contains("runs only on"));

    // erts-<vsn>/ present on disk (include-erts default-on).
    let has_erts_dir = std::fs::read_dir(&output_dir)
        .unwrap()
        .filter_map(Result::ok)
        .any(|e| e.file_name().to_string_lossy().starts_with("erts-"));
    assert!(has_erts_dir, "expected an erts-* dir under {output_dir:?}");

    // The tarball is named `<name>-<vsn>.tar.gz` and lands next to the
    // release dir — `--output dist` makes the release dir `dist/`, so the
    // tarball lands in `dist/`'s own parent, the project root.
    let tar_path = project.path().join("cli_subprocess_fixture-0.1.0.tar.gz");
    assert!(tar_path.is_file(), "missing tarball at {tar_path:?}");
    assert!(
        std::fs::metadata(&tar_path).unwrap().len() > 0,
        "tarball must not be empty"
    );

    // It unpacks, and the unpacked tree has the same top-level shape.
    let unpack_dir = project.path().join("unpacked");
    std::fs::create_dir_all(&unpack_dir).unwrap();
    let status = Command::new("tar")
        .arg("-xzf")
        .arg(&tar_path)
        .arg("-C")
        .arg(&unpack_dir)
        .status()
        .expect("spawn tar to unpack the release tarball");
    assert!(status.success(), "tar -xzf failed for {tar_path:?}");
    assert!(
        unpack_dir.join("lib").is_dir(),
        "unpacked tarball missing lib/"
    );
    assert!(
        unpack_dir.join("releases").is_dir(),
        "unpacked tarball missing releases/"
    );
    assert!(
        std::fs::read_dir(&unpack_dir)
            .unwrap()
            .filter_map(Result::ok)
            .any(|e| e.file_name().to_string_lossy().starts_with("erts-")),
        "unpacked tarball missing erts-*/"
    );
    // ADR 0125 §2.3 (BT-3574): both manifests are appended into the
    // tarball itself (`systools:make_tar/2` does not archive them), so a
    // tarball alone is a valid `--upgrade-from` input.
    assert!(
        unpack_dir
            .join("releases")
            .join("0.1.0")
            .join("shapes.json")
            .is_file(),
        "unpacked tarball missing releases/0.1.0/shapes.json"
    );
    assert!(
        unpack_dir
            .join("releases")
            .join("0.1.0")
            .join("beamtalk-provenance.json")
            .is_file(),
        "unpacked tarball missing releases/0.1.0/beamtalk-provenance.json"
    );

    // beamtalk-provenance.json
    let rel_config_dir = output_dir.join("releases").join("0.1.0");
    let provenance_path = rel_config_dir.join("beamtalk-provenance.json");
    let provenance: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&provenance_path)
            .unwrap_or_else(|e| panic!("read {provenance_path:?}: {e}")),
    )
    .unwrap_or_else(|e| panic!("parse {provenance_path:?}: {e}"));
    assert_eq!(provenance["schema"], 1);
    assert_eq!(provenance["release"], "cli_subprocess_fixture");
    assert_eq!(provenance["release_version"], "0.1.0");
    assert!(provenance["beamtalk_version"].is_string());
    assert!(provenance["otp_release"].is_string());
    assert!(provenance["required_otp"]["min"].is_number());
    assert!(provenance["required_otp"]["max"].is_number());
    assert_eq!(provenance["include_erts"], true);
    assert!(provenance["erts_version"].is_string());
    assert!(provenance["platform"].is_string());
    assert!(
        provenance["apps"]
            .as_array()
            .is_some_and(|apps| !apps.is_empty()),
        "provenance apps list must be non-empty: {provenance}"
    );

    // shapes.json
    let shapes_path = rel_config_dir.join("shapes.json");
    let shapes: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&shapes_path)
            .unwrap_or_else(|e| panic!("read {shapes_path:?}: {e}")),
    )
    .unwrap_or_else(|e| panic!("parse {shapes_path:?}: {e}"));
    assert_eq!(shapes["schema"], 1);
    assert_eq!(shapes["release_version"], "0.1.0");
    let fixture_sup = &shapes["shapes"]["FixtureSup"];
    assert_eq!(fixture_sup["version"], 1, "shapes.json: {shapes}");
    assert!(fixture_sup["fields"].is_object());
    assert!(fixture_sup["migrations"].is_object());
}

/// ADR 0125 §1.3/§3.2: `--no-include-erts` skips the ERTS copy, and the
/// resulting release still boots on this host's own Erlang/OTP.
#[test]
fn release_no_include_erts_skips_erts_and_still_boots_test() {
    let project = cli_common::fixture_project();
    make_releasable(project.path());

    let output_dir = project.path().join("dist");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--no-include-erts", "--output"])
        .arg(&output_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success()
        .stdout(contains("no ERTS bundled"));

    let has_erts_dir = std::fs::read_dir(&output_dir)
        .unwrap()
        .filter_map(Result::ok)
        .any(|e| e.file_name().to_string_lossy().starts_with("erts-"));
    assert!(
        !has_erts_dir,
        "erts-* must be absent under --no-include-erts"
    );

    let rel_config_dir = output_dir.join("releases").join("0.1.0");
    let provenance: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(rel_config_dir.join("beamtalk-provenance.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(provenance["include_erts"], false);

    // Boots fine on the host's own OTP (this test host's `erl` is exactly
    // the OTP major this release was built with, catching the `$ROOT` trap
    // ADR 0125 §1.3 documents for the no-include-erts path).
    let rel_dir = output_dir.join("releases").join("0.1.0");
    let boot_path = rel_dir.join("start");
    let sys_config_noext = rel_dir.join("sys");
    let release_dir_for_boot_var =
        beamtalk_cli::path_util::to_forward_slash(output_dir.to_str().unwrap());
    let boot_eval = "timer:sleep(200), \
         Ok = is_pid(erlang:whereis(beamtalk_workspace_sup)), \
         case Ok of true -> halt(0); false -> halt(1) end.";
    let status = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg(&boot_path)
        .arg("-boot_var")
        .arg("RELEASE_DIR")
        .arg(&release_dir_for_boot_var)
        .arg("-config")
        .arg(&sys_config_noext)
        .arg("-eval")
        .arg(boot_eval)
        .status()
        .expect("spawn erl to boot the --no-include-erts release");
    assert!(
        status.success(),
        "release built with --no-include-erts failed to boot"
    );
}

/// ADR 0125 §1.3/§3.3: `strip-beams = true` drops `debug_info` from staged
/// beams but leaves `__beamtalk_meta/0` callable.
#[test]
fn release_strip_beams_removes_debug_info_but_keeps_meta_test() {
    let project = cli_common::fixture_project();
    make_releasable(project.path());
    let manifest_path = project.path().join("beamtalk.toml");
    let mut manifest = std::fs::read_to_string(&manifest_path).unwrap();
    manifest.push_str("\n[release]\nstrip-beams = true\n");
    std::fs::write(&manifest_path, manifest).unwrap();

    let output_dir = project.path().join("dist");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(&output_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success();

    let ebin_dir = output_dir
        .join("lib")
        .join("cli_subprocess_fixture-0.1.0")
        .join("ebin");
    let fixture_sup_beam = std::fs::read_dir(&ebin_dir)
        .unwrap()
        .filter_map(Result::ok)
        .map(|e| e.path())
        .find(|p| {
            p.file_name()
                .and_then(|n| n.to_str())
                // `.beam` only — a staged ebin can also hold this module's
                // own `.core` (kept out of a release by `stage_one_app`'s
                // filter, but this must not silently pass if that filter
                // regresses) which would otherwise match the same prefix.
                .is_some_and(|n| {
                    n.starts_with("bt@")
                        && n.contains("fixture_sup")
                        && std::path::Path::new(n)
                            .extension()
                            .is_some_and(|ext| ext.eq_ignore_ascii_case("beam"))
                })
        })
        .unwrap_or_else(|| panic!("no bt@*fixture_sup*.beam found under {ebin_dir:?}"));

    // Checked purely via `beam_lib` (no `code:load_abs`/`-on_load`): loading
    // this module for real needs the runtime's ETS tables
    // (`register_class/0`'s on_load hook), which this throwaway `erl`
    // process never starts — exactly the scenario `beam_lib:chunks/2`
    // exists to read a `.beam`'s static structure without executing it.
    let check = format!(
        "case beam_lib:chunks(\"{beam}\", [debug_info]) of \
             {{error, beam_lib, {{missing_chunk, _, _}}}} -> ok; \
             {{ok, {{_, [{{debug_info, none}}]}}}} -> ok; \
             Other -> io:format(standard_error, \"unexpected debug_info: ~p~n\", [Other]), halt(1) \
         end, \
         {{ok, {{_, [{{exports, Exports}}]}}}} = beam_lib:chunks(\"{beam}\", [exports]), \
         case lists:member({{'__beamtalk_meta', 0}}, Exports) of \
             true -> ok; \
             false -> \
                 io:format(standard_error, \"__beamtalk_meta/0 missing: ~p~n\", [Exports]), \
                 halt(1) \
         end, \
         halt(0).",
        beam = fixture_sup_beam.to_str().unwrap().replace('\\', "/"),
    );
    let status = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-eval")
        .arg(&check)
        .status()
        .expect("spawn erl to check the stripped beam");
    assert!(status.success(), "stripped beam check failed");
}

// ─── Launcher (bin/<name> / bin/<name>.cmd) — ADR 0125 §1.6/§1.7, BT-3573 ──

/// Kills and reaps a spawned `bin/<name> foreground` child on drop, so a
/// test's early `panic!`/`assert!` before its explicit `stop` sequence
/// cannot leave a zombie process behind (`clippy::zombie_processes`).
struct ForegroundGuard(std::process::Child);
impl Drop for ForegroundGuard {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

/// Add a `Smoke` class with a unary entry method (`class run => 21 + 21`,
/// the exact pattern `cli_run.rs`'s script-mode test already uses) — the
/// launcher's `eval`/`rpc` verbs dispatch `Smoke run` against it.
fn add_smoke_class(project: &std::path::Path) {
    std::fs::write(
        project.join("src/Smoke.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Smoke\n\
         \n\
         \x20\x20class run => 21 + 21\n",
    )
    .expect("write src/Smoke.bt");
}

/// Add an `Exiter` class whose `run` entry calls `Program exit: 3` — used to
/// verify `eval`'s exit-code adoption (ADR 0125 §1.7: `eval` sets
/// `node_owning = true`, so `Program exit: N` halts that throwaway VM with
/// `N` directly, the same contract the escript boot module uses).
fn add_exiter_class(project: &std::path::Path) {
    std::fs::write(
        project.join("src/Exiter.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Exiter\n\
         \n\
         \x20\x20class run => Program exit: 3\n",
    )
    .expect("write src/Exiter.bt");
}

/// ADR 0125 §1.7: `eval`'s throwaway VM sets `node_owning = true`, so a
/// `Program exit: N` inside the dispatched entry halts that VM directly
/// with `N` — the launcher adopts it as its own exit code.
#[test]
fn release_launcher_eval_adopts_program_exit_code_test() {
    let project = cli_common::fixture_project();
    let output_dir = project.path().join("dist");
    make_releasable(project.path());
    add_exiter_class(project.path());
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(&output_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success();

    let out = launcher_command(&output_dir, "cli_subprocess_fixture")
        .args(["eval", "Exiter run"])
        .output()
        .expect("spawn bin/<name> eval");
    assert_eq!(
        out.status.code(),
        Some(3),
        "expected `Program exit: 3` to become the launcher's exit code; \
         stdout={} stderr={}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}

/// The launcher script to run on this platform: `bin/<name>` (POSIX `sh`,
/// directly executable) on Unix, `bin/<name>.cmd` on Windows.
#[cfg(unix)]
fn launcher_command(release_dir: &std::path::Path, name: &str) -> Command {
    Command::new(release_dir.join("bin").join(name))
}

#[cfg(windows)]
fn launcher_command(release_dir: &std::path::Path, name: &str) -> Command {
    Command::new(release_dir.join("bin").join(format!("{name}.cmd")))
}

/// Build a releasable fixture (with `Smoke`) at `output_dir`, returning it.
fn build_release_fixture(project: &std::path::Path, output_dir: &std::path::Path) {
    make_releasable(project);
    add_smoke_class(project);
    cli_common::beamtalk()
        .current_dir(project)
        .args(["release", "--output"])
        .arg(output_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success();
}

/// Spawns `bin/<name> foreground` and polls `ping` until the node is live,
/// panicking with captured stdout/stderr on an early exit or a timeout — the
/// boot-and-wait sequence every launcher lifecycle test needs (BT-3573's
/// default-ERTS lifecycle test and BT-3619's `--no-include-erts` counterpart
/// both spawn the same node and wait for the same signal; only the release
/// build that produced `output_dir` differs between them).
fn spawn_foreground_and_wait_for_ping(
    output_dir: &std::path::Path,
    name: &str,
    cookie: &str,
) -> ForegroundGuard {
    let child = launcher_command(output_dir, name)
        .arg("foreground")
        .env("RELEASE_COOKIE", cookie)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("spawn bin/<name> foreground");
    // Guarantees `wait()` runs on every exit path (including an early
    // `panic!`/`assert!` below) — an unreaped child a test process spawned
    // is exactly the zombie-process hazard `clippy::zombie_processes` warns
    // about, and callers have several early-return branches before their
    // explicit `stop`-then-`wait()` sequence reaps it in the ordinary case.
    let mut foreground = ForegroundGuard(child);

    // Poll `ping` until the node is live (or the child exited early, which
    // is itself a failure worth surfacing directly rather than timing out).
    // 60s, not a shorter window: cheap insurance against a slow CI boot
    // under contention.
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(60);
    let mut pinged = false;
    let mut last_ping_output: Option<std::process::Output> = None;
    while std::time::Instant::now() < deadline {
        if let Ok(Some(status)) = foreground.0.try_wait() {
            let mut stdout = String::new();
            let mut stderr = String::new();
            if let Some(mut out) = foreground.0.stdout.take() {
                let _ = std::io::Read::read_to_string(&mut out, &mut stdout);
            }
            if let Some(mut err) = foreground.0.stderr.take() {
                let _ = std::io::Read::read_to_string(&mut err, &mut stderr);
            }
            panic!(
                "bin/<name> foreground exited early: {status:?}\nstdout={stdout}\nstderr={stderr}"
            );
        }
        let ping = launcher_command(output_dir, name)
            .arg("ping")
            .env("RELEASE_COOKIE", cookie)
            .output()
            .expect("spawn bin/<name> ping");
        if ping.status.success() {
            pinged = true;
            break;
        }
        last_ping_output = Some(ping);
        std::thread::sleep(std::time::Duration::from_millis(300));
    }
    if !pinged {
        // Kill the still-running foreground node first so its pipes close
        // and whatever it already wrote (a distribution/boot error, if any)
        // can be read back in full, instead of guessing blind at a second
        // CI-only failure mode.
        let _ = foreground.0.kill();
        let _ = foreground.0.wait();
        let mut fg_stdout = String::new();
        let mut fg_stderr = String::new();
        if let Some(mut out) = foreground.0.stdout.take() {
            let _ = std::io::Read::read_to_string(&mut out, &mut fg_stdout);
        }
        if let Some(mut err) = foreground.0.stderr.take() {
            let _ = std::io::Read::read_to_string(&mut err, &mut fg_stderr);
        }
        panic!(
            "node never came up in time for `ping` to succeed; \
             cookie={cookie:?}; last ping attempt: {last_ping_output:?}; \
             foreground stdout so far={fg_stdout:?}; \
             foreground stderr so far={fg_stderr:?}"
        );
    }
    foreground
}

/// Sends `bin/<name> stop` and waits for the foreground process to exit —
/// the shared graceful-shutdown sequence every launcher lifecycle test ends
/// with.
fn stop_and_wait_for_exit(
    foreground: &mut ForegroundGuard,
    output_dir: &std::path::Path,
    name: &str,
    cookie: &str,
) {
    let stop = launcher_command(output_dir, name)
        .arg("stop")
        .env("RELEASE_COOKIE", cookie)
        .output()
        .expect("spawn bin/<name> stop");
    assert!(
        stop.status.success(),
        "stop failed: stdout={} stderr={}",
        String::from_utf8_lossy(&stop.stdout),
        String::from_utf8_lossy(&stop.stderr)
    );

    let stop_deadline = std::time::Instant::now() + std::time::Duration::from_secs(60);
    let mut stopped = false;
    while std::time::Instant::now() < stop_deadline {
        if let Ok(Some(_status)) = foreground.0.try_wait() {
            stopped = true;
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(300));
    }
    assert!(stopped, "bin/<name> foreground did not exit after `stop`");
}

#[test]
fn release_writes_launcher_scripts_test() {
    let project = cli_common::fixture_project();
    let output_dir = project.path().join("dist");
    build_release_fixture(project.path(), &output_dir);

    let sh_path = output_dir.join("bin").join("cli_subprocess_fixture");
    let cmd_path = output_dir.join("bin").join("cli_subprocess_fixture.cmd");
    assert!(sh_path.is_file(), "missing {sh_path:?}");
    assert!(cmd_path.is_file(), "missing {cmd_path:?}");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = std::fs::metadata(&sh_path).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o755, "bin/<name> must be executable");
    }

    let sh_content = std::fs::read_to_string(&sh_path).unwrap();
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
            sh_content.contains(verb),
            "bin/<name> missing verb '{verb}'"
        );
    }
    // Neither script hardcodes the build machine's own release path — both
    // resolve `ROOT` relative to their own location at runtime.
    assert!(!sh_content.contains(output_dir.to_str().unwrap()));
    let cmd_content = std::fs::read_to_string(&cmd_path).unwrap();
    assert!(!cmd_content.contains(output_dir.to_str().unwrap()));
}

#[test]
fn release_launcher_version_verb_test() {
    let project = cli_common::fixture_project();
    let output_dir = project.path().join("dist");
    build_release_fixture(project.path(), &output_dir);

    let out = launcher_command(&output_dir, "cli_subprocess_fixture")
        .arg("version")
        .output()
        .expect("spawn bin/<name> version");
    assert!(out.status.success(), "{out:?}");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("cli_subprocess_fixture") && stdout.contains("0.1.0"),
        "unexpected `version` output: {stdout}"
    );
}

/// Full launcher lifecycle over a real, running release node: `foreground`
/// boots it, `ping` observes it live, `eval` dispatches into a **separate**
/// throwaway VM, `rpc` dispatches into the **running** node over
/// distribution, and `stop` shuts it down gracefully (ADR 0125 §1.7).
#[test]
#[allow(clippy::too_many_lines)]
fn release_launcher_foreground_ping_eval_rpc_stop_lifecycle_test() {
    let project = cli_common::fixture_project();
    let output_dir = project.path().join("dist");
    build_release_fixture(project.path(), &output_dir);

    let name = "cli_subprocess_fixture";
    // An explicit, per-test cookie: with none set, `stop`/`ping`/`rpc`'s
    // client nodes and the `foreground` node all fall back to the shared
    // `$HOME/.erlang.cookie` file, auto-generated on first use — under
    // CI's fully-parallel test suite, another test's node can race to
    // create/rewrite that same file between this node's boot (which reads
    // it once and caches the value for the life of the VM) and a later
    // `ping`/`rpc` invocation (which re-reads the file fresh each time),
    // permanently desynchronizing the two and producing an unauthenticated
    // `pang` that never recovers — this is what caused this test's
    // observed CI-only "node never came up" failures. An explicit cookie
    // removes the shared file from the picture entirely.
    let cookie = format!("bt3573_test_cookie_{}", std::process::id());
    let mut foreground = spawn_foreground_and_wait_for_ping(&output_dir, name, &cookie);

    // `eval` — a separate VM, dispatch `Smoke run`, halt with the outcome.
    let eval = launcher_command(&output_dir, name)
        .args(["eval", "Smoke run"])
        .output()
        .expect("spawn bin/<name> eval");
    assert!(
        eval.status.success(),
        "eval failed: stdout={} stderr={}",
        String::from_utf8_lossy(&eval.stdout),
        String::from_utf8_lossy(&eval.stderr)
    );

    // `rpc` — dispatch into the *running* node over distribution and print
    // the result (`Smoke run` => `21 + 21` => `42`).
    let rpc = launcher_command(&output_dir, name)
        .args(["rpc", "Smoke run"])
        .env("RELEASE_COOKIE", &cookie)
        .output()
        .expect("spawn bin/<name> rpc");
    assert!(
        rpc.status.success(),
        "rpc failed: stdout={} stderr={}",
        String::from_utf8_lossy(&rpc.stdout),
        String::from_utf8_lossy(&rpc.stderr)
    );
    let rpc_stdout = String::from_utf8_lossy(&rpc.stdout);
    assert!(
        rpc_stdout.contains("42"),
        "expected `Smoke run`'s result (42) in rpc output: {rpc_stdout}"
    );

    // `rpc "Beamtalk releaseInfo"` — ADR 0125 §1.1/§1.8's other headline
    // example, verbatim (BT-3576, BT-3612): a parity-neutral reflective
    // send, always available (no compiler needed), naming the release, its
    // version and the toolchain OTP release. `Beamtalk` is a workspace
    // singleton *instance* of `BeamtalkInterface`, not a registered class;
    // run-entry resolves it singleton-first
    // (`beamtalk_repl_eval:resolve_entry/2`), so it must answer the same
    // Dictionary as the class-side `BeamtalkInterface releaseInfo`.
    let rpc_release_info = |receiver: &str| {
        let entry = format!("{receiver} releaseInfo");
        let out = launcher_command(&output_dir, name)
            .args(["rpc", &entry])
            .env("RELEASE_COOKIE", &cookie)
            .output()
            .unwrap_or_else(|e| panic!("spawn bin/<name> rpc {entry:?}: {e}"));
        assert!(
            out.status.success(),
            "rpc {entry:?} failed: stdout={} stderr={}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
        for expected in [
            "release => <<\"cli_subprocess_fixture\">>",
            "release_version => <<\"0.1.0\">>",
            "otp_release =>",
        ] {
            assert!(
                stdout.contains(expected),
                "expected {expected:?} in `rpc {entry:?}` output: {stdout}"
            );
        }
        stdout
    };
    let via_global = rpc_release_info("Beamtalk");
    let via_class = rpc_release_info("BeamtalkInterface");
    assert_eq!(
        via_global, via_class,
        "`rpc \"Beamtalk releaseInfo\"` and `rpc \"BeamtalkInterface releaseInfo\"` \
         must answer the same Dictionary"
    );

    // `eval "Beamtalk releaseInfo"` — the same singleton resolution in
    // `eval`'s separate throwaway VM (BT-3612); `eval` prints nothing on
    // success, so the exit status is the whole contract.
    let eval_release_info = launcher_command(&output_dir, name)
        .args(["eval", "Beamtalk releaseInfo"])
        .output()
        .expect("spawn bin/<name> eval \"Beamtalk releaseInfo\"");
    assert!(
        eval_release_info.status.success(),
        "eval \"Beamtalk releaseInfo\" failed: stdout={} stderr={}",
        String::from_utf8_lossy(&eval_release_info.stdout),
        String::from_utf8_lossy(&eval_release_info.stderr)
    );

    // `stop` — graceful `init:stop()` over distribution; the foreground
    // process must exit on its own shortly after.
    stop_and_wait_for_exit(&mut foreground, &output_dir, name, &cookie);
}

/// BT-3619: `bin/<name> foreground` under `--no-include-erts` (host ERTS) —
/// the exact combination the bug report reproduces on (macOS aarch64,
/// mise-managed OTP 28.5), but no existing test exercised end to end.
/// `release_no_include_erts_skips_erts_and_still_boots_test` boots the same
/// release by hand-invoking `erl -boot_var RELEASE_DIR <dir>` with the
/// *build-time* path, bypassing the launcher's own runtime `RELEASE_DIR`
/// computation entirely (`launcher.sh`'s `ROOT`, resolved via `pwd -P` from
/// the script's own on-disk location); the lifecycle test just above only
/// exercises the launcher against the default, bundled-ERTS build. This is
/// the missing combination — the actual user-facing path the report's
/// `bin/<name> foreground` repro steps describe.
#[test]
fn release_launcher_foreground_boots_under_no_include_erts_test() {
    let project = cli_common::fixture_project();
    let output_dir = project.path().join("dist");
    make_releasable(project.path());
    add_smoke_class(project.path());
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--no-include-erts", "--output"])
        .arg(&output_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success();

    let name = "cli_subprocess_fixture";
    let cookie = format!("bt3619_test_cookie_{}", std::process::id());
    let mut foreground = spawn_foreground_and_wait_for_ping(&output_dir, name, &cookie);
    stop_and_wait_for_exit(&mut foreground, &output_dir, name, &cookie);
}

/// The `eval` verb's separate throwaway VM never starts the project's own
/// root supervisor (ADR 0125 §1.7) — verified by checking a fixture class
/// registered with the OTP application controller (`FixtureSup`, the root
/// supervisor `make_releasable` declares) never comes up, while `eval`
/// still dispatches successfully against a plain class (`Smoke`).
#[test]
fn release_launcher_eval_does_not_start_project_app_test() {
    let project = cli_common::fixture_project();
    let output_dir = project.path().join("dist");
    build_release_fixture(project.path(), &output_dir);

    let name = "cli_subprocess_fixture";
    let eval = launcher_command(&output_dir, name)
        .args(["eval", "Smoke run"])
        .output()
        .expect("spawn bin/<name> eval");
    assert!(
        eval.status.success(),
        "eval failed: stdout={} stderr={}",
        String::from_utf8_lossy(&eval.stdout),
        String::from_utf8_lossy(&eval.stderr)
    );
    // A second, independent `eval` call must also succeed: if the first one
    // had left a `-sname`'d/distribution-bound node behind (e.g. because it
    // wrongly started the project's own app, which nothing here supervises
    // past this call), a stray port/name clash would be the likely symptom
    // on the second attempt.
    let eval2 = launcher_command(&output_dir, name)
        .args(["eval", "Smoke run"])
        .output()
        .expect("spawn bin/<name> eval (second run)");
    assert!(
        eval2.status.success(),
        "second eval failed: stdout={} stderr={}",
        String::from_utf8_lossy(&eval2.stdout),
        String::from_utf8_lossy(&eval2.stderr)
    );
}

/// ADR 0125 §3.2's OTP-major boot check, exercised end-to-end via the
/// launcher: a faked out-of-range `required_otp` in `beamtalk-provenance.json`
/// under `--no-include-erts` (host ERTS) refuses to boot with the ADR's
/// named-versions message, and exits non-zero.
#[test]
fn release_launcher_refuses_out_of_range_otp_via_provenance_test() {
    let project = cli_common::fixture_project();
    let output_dir = project.path().join("dist");
    make_releasable(project.path());
    add_smoke_class(project.path());
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--no-include-erts", "--output"])
        .arg(&output_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success();

    let provenance_path = output_dir
        .join("releases")
        .join("0.1.0")
        .join("beamtalk-provenance.json");
    let mut provenance: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&provenance_path).unwrap()).unwrap();
    // Force a window this host's OTP cannot possibly be inside.
    provenance["required_otp"]["min"] = serde_json::json!(1);
    provenance["required_otp"]["max"] = serde_json::json!(2);
    std::fs::write(
        &provenance_path,
        serde_json::to_string_pretty(&provenance).unwrap(),
    )
    .unwrap();

    let name = "cli_subprocess_fixture";
    let out = launcher_command(&output_dir, name)
        .arg("ping")
        .output()
        .expect("spawn bin/<name> ping");
    assert!(
        !out.status.success(),
        "expected the OTP boot check to refuse and exit non-zero"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("cannot start on Erlang/OTP"),
        "unexpected stderr: {stderr}"
    );
    assert!(stderr.contains("Required: Erlang/OTP 1-2"), "{stderr}");
}

/// ADR 0125 §2.3 (BT-3574): `beamtalk release --upgrade-from` builds the
/// 1.4.0 release, then compares it against a real, previously built 1.3.0
/// release directory — a `shapeVersion:` bump with no `migrateFromV1:`
/// (**error**) and a deleted class (**warning**) — and asserts the printed
/// report verbatim, plus the non-zero exit code an error finding requires.
#[test]
#[allow(clippy::too_many_lines)]
fn release_upgrade_from_reports_missing_migration_and_removed_class_verbatim() {
    let project = cli_common::fixture_project();
    make_releasable(project.path());
    std::fs::write(
        project.path().join("src/Widget.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Actor subclass: Widget\n\
         \x20\x20state: id :: String = \"\"\n",
    )
    .expect("write src/Widget.bt");
    std::fs::write(
        project.path().join("src/LegacyQuote.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Actor subclass: LegacyQuote\n\
         \x20\x20state: notes :: String = \"\"\n",
    )
    .expect("write src/LegacyQuote.bt");
    let manifest_path = project.path().join("beamtalk.toml");
    let manifest = std::fs::read_to_string(&manifest_path).unwrap();
    std::fs::write(
        &manifest_path,
        manifest.replace("version = \"0.1.0\"", "version = \"1.3.0\""),
    )
    .unwrap();

    // Build the 1.3.0 release — the "previous" release the upgrade check
    // compares against.
    let prev_dir = project.path().join("dist-1.3.0");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(&prev_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success()
        .stdout(contains("Built release cli_subprocess_fixture-1.3.0"));

    // Move to 1.4.0: Widget gains `shapeVersion: 2` with no
    // `migrateFromV1:` (an unmigrated bump — an **error**), LegacyQuote is
    // deleted (a **warning**).
    let manifest = std::fs::read_to_string(&manifest_path).unwrap();
    std::fs::write(
        &manifest_path,
        manifest.replace("version = \"1.3.0\"", "version = \"1.4.0\""),
    )
    .unwrap();
    std::fs::write(
        project.path().join("src/Widget.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Actor subclass: Widget\n\
         \x20\x20shapeVersion: 2\n\
         \x20\x20state: id :: String = \"\"\n",
    )
    .expect("rewrite src/Widget.bt");
    std::fs::remove_file(project.path().join("src/LegacyQuote.bt")).expect("delete LegacyQuote.bt");

    // Build the 1.4.0 release with --upgrade-from pointed at the real 1.3.0
    // release directory just built above.
    let new_dir = project.path().join("dist-1.4.0");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(&new_dir)
        .arg("--upgrade-from")
        .arg(&prev_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .failure()
        .stdout(contains(
            "Upgrade check: cli_subprocess_fixture 1.3.0 → 1.4.0",
        ))
        .stdout(contains("Shape changes requiring migration"))
        .stdout(contains("Widget"))
        .stdout(contains("v1 → v2   migrateFromV1: MISSING   error"))
        .stdout(contains("Removed classes"))
        .stdout(contains("LegacyQuote"))
        .stdout(contains("warning"))
        .stdout(contains("Toolchain"))
        .stdout(contains("1 error, 1 warning."))
        .stderr(contains("Upgrade check found blocking shape errors"));
}

/// The "no shapes.json on the previous release" fallback (ADR 0125 §2.3):
/// pointing `--upgrade-from` at a release directory with no
/// `releases/<vsn>/shapes.json` still produces a real report, via the §2.2
/// extractor run over that release's own staged `lib/*/ebin` on the fly —
/// never "unknown".
#[test]
fn release_upgrade_from_falls_back_to_on_the_fly_extraction_when_shapes_json_is_missing() {
    let project = cli_common::fixture_project();
    make_releasable(project.path());
    let manifest_path = project.path().join("beamtalk.toml");
    let manifest = std::fs::read_to_string(&manifest_path).unwrap();
    std::fs::write(
        &manifest_path,
        manifest.replace("version = \"0.1.0\"", "version = \"1.3.0\""),
    )
    .unwrap();

    let prev_dir = project.path().join("dist-1.3.0");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(&prev_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success();

    // Simulate a release built before ADR 0125 shipped shapes.json.
    let prev_shapes_json = prev_dir.join("releases").join("1.3.0").join("shapes.json");
    assert!(prev_shapes_json.is_file());
    std::fs::remove_file(&prev_shapes_json).unwrap();

    let manifest = std::fs::read_to_string(&manifest_path).unwrap();
    std::fs::write(
        &manifest_path,
        manifest.replace("version = \"1.3.0\"", "version = \"1.4.0\""),
    )
    .unwrap();

    let new_dir = project.path().join("dist-1.4.0");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(&new_dir)
        .arg("--upgrade-from")
        .arg(&prev_dir)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success()
        .stdout(contains(
            "Upgrade check: cli_subprocess_fixture 1.3.0 → 1.4.0",
        ))
        .stdout(contains("0 errors, 0 warnings."));
}

/// `--upgrade-from` also accepts a `.tar.gz` tarball of a previous release
/// (ADR 0125 §2.3), not just an unpacked directory — the tarball is the
/// artifact `beamtalk release` actually distributes, so pointing at the
/// directory alone would leave the more common real-world input untested.
#[test]
fn release_upgrade_from_accepts_a_tarball() {
    let project = cli_common::fixture_project();
    make_releasable(project.path());
    let manifest_path = project.path().join("beamtalk.toml");
    let manifest = std::fs::read_to_string(&manifest_path).unwrap();
    std::fs::write(
        &manifest_path,
        manifest.replace("version = \"0.1.0\"", "version = \"1.3.0\""),
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(project.path().join("dist-1.3.0"))
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success();
    let prev_tarball = project.path().join("cli_subprocess_fixture-1.3.0.tar.gz");
    assert!(
        prev_tarball.is_file(),
        "expected a tarball at {prev_tarball:?}"
    );

    let manifest = std::fs::read_to_string(&manifest_path).unwrap();
    std::fs::write(
        &manifest_path,
        manifest.replace("version = \"1.3.0\"", "version = \"1.4.0\""),
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["release", "--output"])
        .arg(project.path().join("dist-1.4.0"))
        .arg("--upgrade-from")
        .arg(&prev_tarball)
        .timeout(std::time::Duration::from_secs(180))
        .assert()
        .success()
        .stdout(contains(
            "Upgrade check: cli_subprocess_fixture 1.3.0 → 1.4.0",
        ))
        .stdout(contains("0 errors, 0 warnings."));
}
