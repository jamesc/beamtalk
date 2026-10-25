// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk test`.
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
    // `beamtalk test`'s BUnit test-file compilation must set
    // `current_package` so E0401/E0402 visibility checks run on the direct
    // test-file path (not just the `build_packages` path). A test class
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

/// BT-3673: `beamtalk test` indexes the package `src/` itself; a typed call
/// (from a test class) to a trait-provided method on a `src/` class whose
/// trait lives in another `src/` file must not report "does not understand".
#[test]
fn test_resolves_cross_file_trait_provided_method_bt_3673() {
    let project = cli_common::fixture_project();
    cli_common::write_cross_file_trait_sources(&project.path().join("src"));
    std::fs::write(
        project.path().join("test/WidgetTagTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: WidgetTagTest\n\
         \n\
         \x20\x20describe: w :: Widget -> String => w tag\n\
         \x20\x20broken: w :: Widget => w bogus\n\
         \x20\x20testTag => self assert: 1 equals: 1\n",
    )
    .unwrap();

    let output = cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .output()
        .unwrap();
    let all = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        all.contains("does not understand 'bogus'"),
        "control: `bogus` must be reported as DNU, got:\n{all}"
    );
    assert!(
        !all.contains("does not understand 'tag'"),
        "provided method `tag` must resolve on a cross-file trait user, got:\n{all}"
    );
}

/// BT-4152: `beamtalk test` emits fixture modules unqualified
/// (`bt@<stem>`) while stamping the fixture's own class with
/// `package = Some(current_package)`. A sealed fixture Value whose
/// methods send to `self` (or to a same-class-typed parameter) must
/// direct-call the emitted module — not the never-emitted
/// `bt@<package>@<class>`, which raised `Undefined function` at runtime.
#[test]
fn test_sealed_fixture_value_self_sends_dispatch_bt_4152() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("test/fixtures")).unwrap();
    std::fs::write(
        project.path().join("test/fixtures/sealed_counter.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         sealed Value subclass: SealedCounter\n\
         \x20\x20field: count :: Integer = 21\n\
         \n\
         \x20\x20doubled -> Integer => self count + self count\n\
         \n\
         \x20\x20plus: other :: SealedCounter -> Integer =>\n\
         \x20\x20\x20\x20self count + other count\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("test/SealedCounterTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: SealedCounterTest\n\
         \n\
         \x20\x20testSelfSend =>\n\
         \x20\x20\x20\x20c := SealedCounter new\n\
         \x20\x20\x20\x20self assert: c doubled equals: 42\n\
         \n\
         \x20\x20testTypedParamSend =>\n\
         \x20\x20\x20\x20c := SealedCounter new\n\
         \x20\x20\x20\x20self assert: (c plus: SealedCounter new) equals: 42\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .success()
        .stdout(contains("3 passed"))
        .stdout(contains("0 failed"));
}

/// BT-4094: a test class calling a `sealed` dependency-package class method
/// must compile the call against `bt@<package>@<module>`, as `src/` and
/// fixtures do — not the `bt@<snake>` heuristic, which fails at runtime
/// with `Undefined function`.
///
/// Each test file compiles against its own package's class index, which
/// used to omit dependency classes (only the merged, fixture-facing index
/// had them), so the direct-call table resolved e.g. `Catalina start:` to
/// `bt@catalina:'class_start:'` instead of `bt@catalina@catalina`.
///
/// Uses a `path` dependency so the test is hermetic (no network access).
#[test]
fn test_file_calls_sealed_dependency_class_method_bt_4094() {
    let project = cli_common::fixture_project();
    let root = project.path();

    // Dependency package exposing a sealed class with nullary and keyword
    // class methods (mirroring `ServerConfig optionNames` / `Catalina start:`).
    std::fs::create_dir_all(root.join("depkit/src")).unwrap();
    std::fs::write(
        root.join("depkit/beamtalk.toml"),
        "# Copyright 2026 James Casey\n\
         # SPDX-License-Identifier: Apache-2.0\n\
         \n\
         [package]\n\
         name = \"depkit\"\n\
         version = \"0.1.0\"\n\
         \n\
         [dependencies]\n",
    )
    .unwrap();
    std::fs::write(
        root.join("depkit/src/DepWidget.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         sealed Object subclass: DepWidget\n\
         \x20\x20class sealed answer => 42\n\
         \x20\x20class sealed double: n => n + n\n",
    )
    .unwrap();

    // Depend on it from the fixture project via a relative path.
    let manifest = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
    assert!(
        manifest.ends_with("[dependencies]\n"),
        "fixture manifest shape changed; update this test's append"
    );
    std::fs::write(
        root.join("beamtalk.toml"),
        format!("{manifest}depkit = {{ path = \"depkit\" }}\n"),
    )
    .unwrap();

    std::fs::write(
        root.join("test/DepProbeTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: DepProbeTest\n\
         \n\
         \x20\x20testAnswer => self assert: DepWidget answer equals: 42\n\
         \x20\x20testDouble => self assert: (DepWidget double: 21) equals: 42\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(root)
        .args(["test", "--quiet"])
        .assert()
        .success()
        // GreeterTest (1) + DepProbeTest (2).
        .stdout(contains("3 passed"))
        .stdout(contains("0 failed"));
}
