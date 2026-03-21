// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Integration tests for Dialyzer spec validation (BT-1565).
//!
//! These tests verify the full round-trip:
//! `.bt` source → Core Erlang with `-spec` attributes → validation via escript
//!
//! Tests are `#[ignore]` because they require the beamtalk binary, `erlc`, and `escript`.
//! Run with: `cargo test --test spec_validation -- --ignored`

use std::path::{Path, PathBuf};
use std::process::Command;

fn project_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn beamtalk_binary() -> PathBuf {
    project_root().join("target/debug/beamtalk")
}

fn validate_specs_script() -> PathBuf {
    project_root().join("scripts/validate_specs.escript")
}

/// Compile a `.bt` file and return the path to the build directory containing `.core` files.
fn compile_bt_to_core(bt_source: &str, work_dir: &Path) -> PathBuf {
    let bt_path = work_dir.join("test_input.bt");
    std::fs::write(&bt_path, bt_source).expect("write .bt file");

    let output = Command::new(beamtalk_binary())
        .args(["build", bt_path.to_str().unwrap()])
        .output()
        .expect("run beamtalk build");

    assert!(
        output.status.success(),
        "beamtalk build failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    work_dir.join("build")
}

/// Run `validate_specs.escript` on a directory of `.core` files.
/// Returns (success, stdout, stderr).
fn run_validate_specs(core_dir: &Path) -> (bool, String, String) {
    let output = Command::new("escript")
        .args([
            validate_specs_script().to_str().unwrap(),
            core_dir.to_str().unwrap(),
        ])
        .output()
        .expect("run validate_specs.escript");

    (
        output.status.success(),
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

/// Round-trip test: `.bt` with type annotations -> Core Erlang -> Dialyzer validation.
/// Verifies that generated `-spec` attributes are well-formed and accepted by Dialyzer.
#[test]
#[ignore = "requires beamtalk binary, erlc, and escript"]
fn valid_specs_pass_dialyzer_validation() {
    let source = "\
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
Object subclass: SpecRoundTrip
  add: x :: Integer to: y :: Integer -> Integer =>
    x + y
  greet: name :: String -> String =>
    \"hello \" ++ name
  isEmpty -> Boolean => true
  half: n :: Number -> Float =>
    n / 2.0
  class create: label :: String -> Object =>
    self new
";
    let tmp = tempfile::tempdir().expect("create temp dir");
    let core_dir = compile_bt_to_core(source, tmp.path());
    assert!(core_dir.exists(), "build directory should exist");

    // Verify .core files were generated
    let core_files: Vec<_> = std::fs::read_dir(&core_dir)
        .expect("read core dir")
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "core"))
        .collect();
    assert!(!core_files.is_empty(), "should have generated .core files");

    // Verify spec attributes are present in the .core file
    let core_content = std::fs::read_to_string(core_files[0].path()).expect("read .core file");
    assert!(
        core_content.contains("'spec'"),
        "Core Erlang should contain spec attributes"
    );

    // Run the full Dialyzer validation
    let (success, stdout, stderr) = run_validate_specs(&core_dir);
    assert!(
        success,
        "validate_specs.escript should succeed.\nstdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stdout.contains("spec(s) are valid"),
        "should report specs as valid.\nstdout: {stdout}"
    );
}

/// Verify that Core Erlang spec attributes contain correct type mappings
/// for each Beamtalk type annotation.
#[test]
#[ignore = "requires beamtalk binary"]
fn core_erlang_specs_contain_correct_type_mappings() {
    let source = "\
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
Object subclass: TypeMappingTest
  intMethod: x :: Integer -> Integer => x
  floatMethod: x :: Float -> Float => x
  stringMethod: x :: String -> String => x
  boolMethod: x :: Boolean -> Boolean => x
  numberMethod: x :: Number -> Number => x
  listMethod: x :: List -> List => x
";
    let tmp = tempfile::tempdir().expect("create temp dir");
    let core_dir = compile_bt_to_core(source, tmp.path());

    let core_files: Vec<_> = std::fs::read_dir(&core_dir)
        .expect("read core dir")
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "core"))
        .collect();
    assert!(!core_files.is_empty());

    let content = std::fs::read_to_string(core_files[0].path()).expect("read .core");

    // Verify expected type mappings
    assert!(
        content.contains("'integer'"),
        "Integer should map to integer"
    );
    assert!(content.contains("'float'"), "Float should map to float");
    assert!(content.contains("'binary'"), "String should map to binary");
    assert!(
        content.contains("'boolean'"),
        "Boolean should map to boolean"
    );
    assert!(content.contains("'number'"), "Number should map to number");
    assert!(content.contains("'list'"), "List should map to list");
}

/// Negative test: an intentionally invalid spec type should be caught by
/// the validation script. We construct a `.core` file directly with an
/// unknown type to verify the escript reports an error.
#[test]
#[ignore = "requires escript and Dialyzer PLT"]
fn negative_test_invalid_spec_detected_by_dialyzer() {
    let tmp = tempfile::tempdir().expect("create temp dir");

    let bad_core = "\
module 'bt@bad_spec_test' ['bad_func'/1]
  attributes ['spec' =
        [{{'bad_func', 1}, [{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'nonexistent_type_XXXX', []}]}, {'type', 0, 'integer', []}]}]}]]

'bad_func'/1 = fun (X) ->
    X
end
";
    std::fs::write(tmp.path().join("bt@bad_spec_test.core"), bad_core)
        .expect("write bad .core file");

    let (success, stdout, stderr) = run_validate_specs(tmp.path());
    assert!(
        !success,
        "validate_specs.escript should FAIL for invalid spec type.\nstdout: {stdout}\nstderr: {stderr}"
    );
    // The error should mention the analysis failure or error
    let combined = format!("{stdout}{stderr}");
    assert!(
        combined.contains("error")
            || combined.contains("Error")
            || combined.contains("failed")
            || combined.contains("Analysis failed"),
        "output should indicate an error.\ncombined: {combined}"
    );
}
