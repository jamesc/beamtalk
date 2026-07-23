// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk test` (BT-2084).
//!
//! Verifies exit code mapping (pass = 0, fail = nonzero) and the text
//! summary format. Uses a hermetic fixture project per test.

use crate::cli_common;

use predicates::prelude::*;
use predicates::str::contains;

#[test]
fn test_passes_for_passing_suite() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .success()
        .stdout(contains("1 passed"))
        .stdout(contains("0 failed"));
}

#[test]
fn test_fails_for_failing_suite() {
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("test/FailTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: FailTest\n\
         \n\
         \x20\x20testFails => self assert: 1 equals: 2\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .failure()
        .stdout(contains("1 failed").or(contains("FAIL")));
}

#[test]
fn test_fails_with_e0402_for_internal_class_leak_in_test_file() {
    // Regression for BT-2922: `beamtalk test`'s BUnit test-file compilation
    // never set `current_package`, so E0401/E0402 visibility checks silently
    // emitted zero diagnostics on the direct test-file path (the
    // `build_packages` path was already covered by BT-2920). A test class
    // leaking an internal class through a public method signature must fail
    // the test run, matching `beamtalk build`'s behaviour.
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("src/TokenBuffer.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         internal Object subclass: TokenBuffer\n\
         \x20\x20data => nil\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("test/LeakTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: LeakTest\n\
         \n\
         \x20\x20tokenize: input :: String -> TokenBuffer => nil\n\
         \x20\x20testNothing => self assert: true\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .failure()
        // miette word-wraps the message to terminal width, so match the two
        // halves separately rather than the exact single-line string.
        .stderr(
            contains("Internal class 'TokenBuffer' appears in public signature of")
                .and(contains("tokenize:'")),
        );
}

#[test]
fn test_no_tests_found_succeeds() {
    let project = cli_common::fixture_project();
    // Remove the only test file to exercise the "no tests" path.
    std::fs::remove_file(project.path().join("test/GreeterTest.bt")).unwrap();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .success()
        .stdout(contains("No tests found"));
}

#[test]
fn test_script_runs_btscript_files() {
    // test-script reads `.btscript` files and asserts on `// =>` annotations.
    // Synthesize a tiny one in the fixture project to keep the test hermetic.
    let project = cli_common::fixture_project();
    let script_dir = project.path().join("scripts");
    std::fs::create_dir_all(&script_dir).unwrap();
    let script = script_dir.join("smoke.btscript");
    std::fs::write(
        &script,
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         3 + 4\n\
         // => 7\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-script", "--quiet"])
        .arg(&script)
        .assert()
        .success()
        .stdout(contains("1 passed").or(contains("0 failed")));
}

#[test]
fn test_script_fails_on_assertion_mismatch() {
    let project = cli_common::fixture_project();
    let script_dir = project.path().join("scripts");
    std::fs::create_dir_all(&script_dir).unwrap();
    let script = script_dir.join("bad.btscript");
    std::fs::write(
        &script,
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         3 + 4\n\
         // => 99\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-script", "--quiet"])
        .arg(&script)
        .assert()
        .failure();
}
