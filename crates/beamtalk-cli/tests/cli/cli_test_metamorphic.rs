// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk test-metamorphic` (BT-3356).
//!
//! `test_metamorphic.rs`'s pipeline functions (`compile_transform_variant`,
//! `compile_load_fixtures`, `process_test_file`, `run_tests`,
//! `report_metamorphic_results`) write real `.core`/`.erl` files to disk and
//! invoke `erlc`/`EUnit`, so -- following `test_stdlib.rs`'s own precedent
//! (its equivalent pipeline functions are exercised by `cli_test.rs`'s
//! `test_script_*` tests rather than unit-tested in isolation) -- these run
//! the real `test-metamorphic` subcommand end-to-end against hermetic
//! `.btscript` fixtures rather than re-testing already-covered pure AST
//! logic (`walk_names_ref`, `rename_names_mut`, `build_transformed_case`,
//! ..., covered by BT-3352's `#[cfg(test)]` unit tests in that file).

use crate::cli_common;

use predicates::prelude::*;
use predicates::str::contains;
use std::path::Path;

/// Writes `content` to `<project>/<rel_dir>/<name>`, creating `rel_dir` if
/// needed. Returns the file's path.
fn write_file(project: &Path, rel_dir: &str, name: &str, content: &str) -> std::path::PathBuf {
    let dir = project.join(rel_dir);
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(name);
    std::fs::write(&path, content).unwrap();
    path
}

#[test]
fn metamorphic_passes_for_multi_file_run() {
    // Two files, each with a case that binds nothing (arithmetic) and one
    // that does (a block parameter) -- exercises `process_test_file` and
    // `run_tests` folding more than one file's `compile_transform_variant`
    // output into the shared build-dir/EUnit-batch accumulators.
    let project = cli_common::fixture_project();
    write_file(
        project.path(),
        "scripts",
        "a.btscript",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         3 + 4\n\
         // => 7\n",
    );
    write_file(
        project.path(),
        "scripts",
        "b.btscript",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         [:x | x * 2] value: 5\n\
         // => 10\n",
    );

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-metamorphic", "--quiet", "scripts"])
        .assert()
        .success()
        .stdout(contains("0 failed"));
}

#[test]
fn metamorphic_compiles_load_fixture() {
    // Exercises `compile_load_fixtures`: a `// @load` directive naming a
    // real `.bt` fixture file, resolved relative to the current directory
    // (mirroring `stdlib/bootstrap-test/equality.btscript`'s `@load
    // test/fixtures/counter.bt` and the `[working-directory: 'stdlib']`
    // Justfile recipe that makes that path resolve).
    let project = cli_common::fixture_project();
    write_file(
        project.path(),
        "test/fixtures",
        "counter.bt",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Actor subclass: Counter\n\
         \x20\x20state: value = 0\n\
         \n\
         \x20\x20increment => self.value := self.value + 1\n\
         \n\
         \x20\x20getValue => self.value\n",
    );
    write_file(
        project.path(),
        "scripts",
        "load.btscript",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         // @load test/fixtures/counter.bt\n\
         \n\
         counter := Counter spawn\n\
         // => Actor(Counter, _)\n\
         \n\
         counter increment\n\
         // => 1\n\
         \n\
         counter getValue\n\
         // => 1\n",
    );

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-metamorphic", "--quiet", "scripts/load.btscript"])
        .assert()
        .success()
        .stdout(contains("0 failed"));
}

#[test]
fn metamorphic_reports_skipped_combinations_for_unbound_expression() {
    // `rename_locals` returns `None` for an expression that binds nothing
    // (nothing to alpha-rename), so `compile_transform_variant` counts it
    // as skipped for that one transform while `block_wrap`/`redundant_temp`
    // still apply and pass -- exercises the skip-counting path through
    // `process_test_file`/`run_tests` and its `!quiet` summary line in
    // `report_metamorphic_results`. `--quiet` is deliberately omitted so
    // that summary line is emitted.
    let project = cli_common::fixture_project();
    write_file(
        project.path(),
        "scripts",
        "unbound.btscript",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         1 + 2\n\
         // => 3\n",
    );

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-metamorphic", "scripts/unbound.btscript"])
        .assert()
        .success()
        .stdout(contains("expression/transform combination(s) skipped"));
}

#[test]
fn metamorphic_fails_hard_when_no_testable_expressions_found() {
    // A `.btscript` file that parses cleanly but contains zero `// =>`
    // cases (only comments) makes every (file, transform) combination in
    // `process_test_file` skip with an empty `transformed_cases`, so
    // `run_tests`'s `test_module_names` ends up empty across the whole run
    // -- the "would otherwise report success while verifying nothing" hard
    // failure in `run_tests`, distinct from the ordinary per-case skip path
    // above.
    let project = cli_common::fixture_project();
    write_file(
        project.path(),
        "scripts",
        "empty.btscript",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         // No executable assertions in this file (BT-3356 regression fixture).\n",
    );

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-metamorphic", "--quiet", "scripts/empty.btscript"])
        .assert()
        .failure()
        .stderr(contains("No metamorphic-testable expressions found"));
}

#[test]
fn metamorphic_reports_failure_details_for_wrong_expected_value() {
    // A deliberately-wrong `// =>` expectation: every transform variant
    // still (correctly) evaluates the expression to `2`, so both the
    // `block_wrap` and `redundant_temp` EUnit modules fail their single
    // assertion against the file's declared (wrong) `3` -- exercising
    // `report_metamorphic_results`'s failed-row printing, `failed_details`
    // accumulation, and its final `bail!` with a nonzero failure count.
    let project = cli_common::fixture_project();
    write_file(
        project.path(),
        "scripts",
        "wrong.btscript",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         1 + 1\n\
         // => 3\n",
    );

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-metamorphic", "--quiet", "scripts/wrong.btscript"])
        .assert()
        .failure()
        .stdout(contains("failed"))
        .stderr(contains("metamorphic test(s) failed"));
}

#[test]
fn metamorphic_fails_when_load_fixture_missing() {
    // Exercises `compile_load_fixtures`'s "fixture not found" bail: a
    // `// @load` directive naming a file that does not exist relative to
    // the current directory.
    let project = cli_common::fixture_project();
    write_file(
        project.path(),
        "scripts",
        "missing_load.btscript",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         // @load test/fixtures/does_not_exist.bt\n\
         \n\
         1 + 1\n\
         // => 2\n",
    );

    cli_common::beamtalk()
        .current_dir(project.path())
        .args([
            "test-metamorphic",
            "--quiet",
            "scripts/missing_load.btscript",
        ])
        .assert()
        .failure()
        .stderr(
            contains("Fixture file")
                .and(contains("not found"))
                .and(contains("does_not_exist.bt")),
        );
}

#[test]
fn metamorphic_fails_when_expression_missing_assertion() {
    // Exercises `process_test_file`'s "expression(s) without assertions"
    // bail (same corpus-hygiene enforcement as `test_stdlib`, BT-3117):
    // an expression line with no following `// =>` comment.
    let project = cli_common::fixture_project();
    write_file(
        project.path(),
        "scripts",
        "no_assertion.btscript",
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         1 + 1\n",
    );

    cli_common::beamtalk()
        .current_dir(project.path())
        .args([
            "test-metamorphic",
            "--quiet",
            "scripts/no_assertion.btscript",
        ])
        .assert()
        .failure()
        .stderr(contains("expression(s) without assertions"));
}

#[test]
fn metamorphic_fails_for_missing_test_path() {
    // Exercises `run_tests`'s "Test path not found" bail for a path that
    // does not exist at all (distinct from an existing-but-empty
    // directory, covered separately below).
    let project = cli_common::fixture_project();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-metamorphic", "--quiet", "scripts/does-not-exist"])
        .assert()
        .failure()
        .stderr(contains("not found"));
}

#[test]
fn metamorphic_reports_no_files_found_for_empty_directory() {
    // Exercises `run_tests`'s "No .btscript test files found" success path:
    // an existing directory with no `.btscript` files in it.
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("scripts/empty_dir")).unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-metamorphic", "--quiet", "scripts/empty_dir"])
        .assert()
        .success()
        .stdout(contains("No .btscript test files found"));
}
