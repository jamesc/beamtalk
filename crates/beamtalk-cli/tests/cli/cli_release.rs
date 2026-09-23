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

    let status = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg(&boot_path)
        .arg("-boot_var")
        .arg("RELEASE_DIR")
        .arg(&output_dir)
        .arg("-config")
        .arg(&sys_config_noext)
        .arg("-eval")
        .arg(boot_eval)
        .status()
        .expect("spawn erl to boot the release");
    assert!(
        status.success(),
        "release failed to boot from {}",
        boot_path.display()
    );
}
