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

/// A test file with a syntax error (BT-4304). Before the fix, discovery
/// dropped the parse diagnostics, so this read as "No tests found", exit 0.
const BROKEN_TEST_SOURCE: &str = "// Copyright 2026 James Casey\n\
     // SPDX-License-Identifier: Apache-2.0\n\
     \n\
     TestCase subclass: BrokenTest\n\
     \n\
     \x20\x20testBroken => self assert: (1 + ) equals: 3\n";

#[test]
fn test_single_file_with_parse_error_fails_with_diagnostics() {
    let project = cli_common::fixture_project();
    std::fs::remove_file(project.path().join("test/GreeterTest.bt")).unwrap();
    std::fs::write(
        project.path().join("test/BrokenTest.bt"),
        BROKEN_TEST_SOURCE,
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet", "test/BrokenTest.bt"])
        .assert()
        .failure()
        .stdout(contains("No tests found").not())
        .stderr(contains("Failed to parse test file").and(contains("BrokenTest.bt")));
}

#[test]
fn test_directory_with_one_unparsable_file_fails() {
    let project = cli_common::fixture_project();
    // GreeterTest.bt still parses; the broken file must not be skipped.
    std::fs::write(
        project.path().join("test/BrokenTest.bt"),
        BROKEN_TEST_SOURCE,
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .failure()
        .stderr(contains("Failed to parse test file").and(contains("BrokenTest.bt")));
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

/// BT-4446: `beamtalk test` must pre-load the package's `src/` protocols
/// as `beamtalk build` does, so a test file resolves `P class` (ADR 0144)
/// against the protocol's `class` requirements. Before, the protocol was
/// unknown in test compilation and `g :: Salutation class := Bonjour` failed as
/// a plain type mismatch under `--warnings-as-errors`.
#[test]
fn test_file_resolves_src_protocol_metatype_bt_4446() {
    let project = cli_common::fixture_project();
    let header = "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\n";
    std::fs::write(
        project.path().join("src/salutation.bt"),
        format!("{header}Protocol define: Salutation\n  class greet: n :: Integer -> String\n"),
    )
    .unwrap();
    std::fs::write(
        project.path().join("src/bonjour.bt"),
        format!(
            "{header}sealed Object subclass: Bonjour\n  \
             class sealed greet: n :: Integer -> String => \"bonjour {{n printString}}\"\n"
        ),
    )
    .unwrap();
    std::fs::write(
        project.path().join("test/SalutationTest.bt"),
        format!(
            "{header}TestCase subclass: SalutationTest\n\
             \n\
             \x20\x20run: g :: Salutation class -> String => g greet: 1\n\
             \n\
             \x20\x20testArgument => self assert: (self run: Bonjour) equals: \"bonjour 1\"\n\
             \n\
             \x20\x20testLocal =>\n\
             \x20\x20\x20\x20g :: Salutation class := Bonjour\n\
             \x20\x20\x20\x20self assert: (g greet: 2) equals: \"bonjour 2\"\n"
        ),
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet", "--warnings-as-errors"])
        .assert()
        .success()
        .stdout(contains("0 failed"));
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

/// BT-4022: `beamtalk test` must agree with `build`/`lint` — an FFI call
/// in a test file to the package's own `native/*.erl` module must not warn
/// `Unknown Erlang module`, while a genuinely unknown module still does.
/// The native call is executed (proving the module resolves at runtime);
/// the unknown call sits in a never-invoked helper so the suite still
/// passes while analysis still checks it.
#[test]
fn test_knows_own_native_module_but_warns_on_unknown() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("native")).expect("mkdir native");
    std::fs::write(
        project.path().join("native/my_nif.erl"),
        "%% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         -module(my_nif).\n\
         -moduledoc \"Fixture native module for the BT-4022 test.\".\n\
         -export([ping/0]).\n\
         -doc \"Returns `pong`.\".\n\
         ping() -> pong.\n",
    )
    .expect("write native/my_nif.erl");
    std::fs::write(
        project.path().join("test/NativeFfiTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: NativeFfiTest\n\
         \n\
         \x20\x20probeUnknown => Erlang definitely_unknown_xyz ping\n\
         \n\
         \x20\x20testNative => self assert: (Erlang my_nif ping) equals: #pong\n",
    )
    .expect("write test/NativeFfiTest.bt");

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .success()
        .stderr(contains("Unknown Erlang module `definitely_unknown_xyz`"))
        .stderr(contains("Unknown Erlang module `my_nif`").not());
}

/// BT-4723: `beamtalk test <file>` / `<dir>` for a test nested below `test/`
/// must compile the package's `test/fixtures/`, as a full run does, including
/// when the test still carries the deprecated `// @load test/fixtures/...`
/// directive (warns, no module-collision error). The `class_not_found` naming
/// is covered by `beamtalk_class_dispatch_tests`.
#[test]
fn test_scoped_run_loads_package_fixtures_bt_4723() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("test/fixtures")).unwrap();
    std::fs::create_dir_all(project.path().join("test/sub")).unwrap();
    std::fs::write(
        project.path().join("test/fixtures/scoped_fixture.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         sealed Value subclass: ScopedFixture\n\
         \x20\x20field: n :: Integer = 7\n\
         \n\
         \x20\x20seven -> Integer => self n\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("test/sub/ScopedFixtureTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: ScopedFixtureTest\n\
         \n\
         \x20\x20testUsesFixture =>\n\
         \x20\x20\x20\x20self assert: ScopedFixture new seven equals: 7\n",
    )
    .unwrap();

    for target in ["test/sub/ScopedFixtureTest.bt", "test/sub"] {
        cli_common::beamtalk()
            .current_dir(project.path())
            .args(["test", "--quiet", target])
            .assert()
            .success()
            .stdout(contains("1 passed"))
            .stdout(contains("0 failed"));
    }
}

/// BT-4723: a scoped run whose test still uses the deprecated `@load` of a file
/// under the package's `test/fixtures` warns instead of bailing with a
/// module-name collision.
#[test]
fn test_scoped_run_with_load_directive_into_fixtures_bt_4723() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("test/fixtures")).unwrap();
    std::fs::create_dir_all(project.path().join("test/sub")).unwrap();
    std::fs::write(
        project.path().join("test/fixtures/loaded_fixture.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         sealed Value subclass: LoadedFixture\n\
         \x20\x20field: n :: Integer = 9\n\
         \n\
         \x20\x20nine -> Integer => self n\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("test/sub/LoadedFixtureTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         // @load test/fixtures/loaded_fixture.bt\n\
         \n\
         TestCase subclass: LoadedFixtureTest\n\
         \n\
         \x20\x20testUsesFixture =>\n\
         \x20\x20\x20\x20self assert: LoadedFixture new nine equals: 9\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet", "test/sub/LoadedFixtureTest.bt"])
        .assert()
        .success()
        .stdout(contains("1 passed"))
        .stdout(contains("0 failed"));
}

/// A failing test whose stack is deeper than the default 3 frames.
fn write_deep_failure(project: &std::path::Path) {
    std::fs::write(
        project.join("test/DeepFailTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: DeepFailTest\n\
         \n\
         \x20\x20testDeep => #(1, 2, 3) do: [:each | Error signal: \"deep boom\"]\n",
    )
    .unwrap();
}

/// BT-4821: `--raw-mfa` lists each frame with its raw `module:function/arity`
/// beside the display name; the default output has none and names the switch
/// when frames were left out. (Failure details go to stderr.)
#[test]
fn test_raw_mfa_shows_raw_frames_bt_4821() {
    let project = cli_common::fixture_project();
    write_deep_failure(project.path());

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .failure()
        .stderr(contains("deep boom"))
        .stderr(contains("  [bt@").not())
        .stderr(contains("frames hidden; --raw-mfa shows all)"));

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet", "--raw-mfa"])
        .assert()
        .failure()
        .stderr(contains("deep boom"))
        .stderr(contains("[bt@deep_fail_test:testDeep/1]"))
        .stderr(contains("[beamtalk_test_case:run_test_method/5]"))
        .stderr(contains("--raw-mfa shows all").not());
}

/// Writes `native/my_native.erl` and a test that calls `outer/1` through it.
/// `empty_clause` is the body of the `inner([])` clause, the line that fails.
fn write_native_frame_fixture(project: &std::path::Path, empty_clause: &str) {
    std::fs::create_dir_all(project.join("native")).expect("mkdir native");
    std::fs::write(
        project.join("native/my_native.erl"),
        format!(
            "%% Copyright 2026 James Casey\n\
             %% SPDX-License-Identifier: Apache-2.0\n\
             -module(my_native).\n\
             -moduledoc \"Fixture native module for the BT-4822 test.\".\n\
             -export([outer/1]).\n\
             -doc \"Fails one frame down.\".\n\
             outer(X) ->\n\
             \x20\x20\x20\x20Y = inner(X),\n\
             \x20\x20\x20\x20{{ok, Y}}.\n\
             %% Two clauses, so the compiler cannot prove `inner/1` never returns and\n\
             %% turn the call in `outer/1` into a tail call that drops its frame.\n\
             inner([]) -> {empty_clause};\n\
             inner([H | _]) -> H.\n"
        ),
    )
    .expect("write native/my_native.erl");
    std::fs::write(
        project.join("test/NativeFrameTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: NativeFrameTest\n\
         \n\
         \x20\x20testNativeFails => Erlang my_native outer: (List new)\n",
    )
    .expect("write test/NativeFrameTest.bt");
}

/// BT-4822 (ADR 0139 decision 3): Erlang code a Beamtalk package ships in
/// `native/*.erl` is written by a person, so its frames show in the default
/// trace as `module:function/arity (file:line)`, while OTP plumbing stays hidden
/// and no compiler-made name (`bt@...`) appears.
#[test]
fn test_native_package_frames_show_in_default_trace_bt_4822() {
    let project = cli_common::fixture_project();
    write_native_frame_fixture(project.path(), "erlang:error({boom, 1})");

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .failure()
        .stderr(contains("my_native:outer/1 (native/my_native.erl:"))
        .stderr(contains("my_native:inner/1 (native/my_native.erl:"))
        .stderr(contains("NativeFrameTest>>testNativeFails"))
        .stderr(contains("bt@").not())
        .stderr(contains("beamtalk_test_case:").not())
        .stderr(contains("beamtalk_erlang_proxy:").not());
}

/// BT-4838: `badarg` (and `badarith`, `function_clause`) from FFI code take the
/// specific `type_error` path, which used to raise with a fresh stack and lose
/// the Erlang frame that failed. The `badarg` variant of the test above.
#[test]
fn test_native_badarg_frames_show_in_default_trace_bt_4838() {
    let project = cli_common::fixture_project();
    write_native_frame_fixture(project.path(), "hd(not_a_list_at_runtime([]))");
    let native = project.path().join("native/my_native.erl");
    let mut src = std::fs::read_to_string(&native).unwrap();
    src.push_str("not_a_list_at_runtime(X) -> X.\n");
    std::fs::write(&native, src).unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet"])
        .assert()
        .failure()
        .stderr(contains("Type error"))
        .stderr(contains("my_native:inner/1 (native/my_native.erl:"))
        .stderr(contains("my_native:outer/1 (native/my_native.erl:"))
        .stderr(contains("NativeFrameTest>>testNativeFails"))
        .stderr(contains("bt@").not())
        .stderr(contains("beamtalk_erlang_proxy:").not());
}

/// BT-4821: `--full-traces` still works as a hidden alias and says it is deprecated.
#[test]
fn test_full_traces_is_a_deprecated_alias_for_raw_mfa_bt_4821() {
    let project = cli_common::fixture_project();
    write_deep_failure(project.path());

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet", "--full-traces"])
        .assert()
        .failure()
        .stderr(contains("`--full-traces` is deprecated; use `--raw-mfa`"))
        .stderr(contains("[bt@deep_fail_test:testDeep/1]"));

    // The alias is hidden from help; the new flag is documented.
    cli_common::beamtalk()
        .args(["test", "--help"])
        .assert()
        .success()
        .stdout(contains("--raw-mfa"))
        .stdout(contains("--full-traces").not());
}
