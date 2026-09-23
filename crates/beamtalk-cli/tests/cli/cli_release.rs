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
