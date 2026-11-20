// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Argument-validation tests for `validate_class_name`, `validate_erlang_module_name`, and `validate_selector`.
//!
//! BT-4908: one table per validator. Rejections assert the exact error message
//! (which also names the offending input) rather than only `is_err()`.

use super::*;

/// Asserts `f` returns `Ok` for every accepted input and an exact-message
/// `invalid_params` error for every rejected one.
fn check(
    validator: &str,
    f: fn(&str) -> Result<(), rmcp::ErrorData>,
    accepted: &[&str],
    rejected: &[(&str, String)],
) {
    for input in accepted {
        assert!(
            f(input).is_ok(),
            "{validator}({input:?}) should be accepted"
        );
    }
    for (input, expected) in rejected {
        let err = f(input).expect_err(&format!("{validator}({input:?}) should be rejected"));
        assert_eq!(err.message, *expected, "{validator}({input:?})");
        assert_eq!(
            err.code,
            rmcp::model::ErrorCode::INVALID_PARAMS,
            "{validator}({input:?})"
        );
    }
}

fn bad_class(name: &str) -> String {
    format!("Invalid class name: '{name}'. Must be an uppercase-starting identifier.")
}

fn bad_module(name: &str) -> String {
    format!("Invalid Erlang module name: '{name}'. Must be a lowercase identifier.")
}

#[test]
fn validate_class_name_accepts_and_rejects() {
    let rejected: Vec<(&str, String)> = [
        "", "counter", "1Counter", "My Class", "My-Class",
        "My:Class", // colons are for selectors only
        "Clàss",    // non-ASCII
        "bad name",
    ]
    .into_iter()
    .map(|n| (n, bad_class(n)))
    .collect();
    check(
        "validate_class_name",
        validate_class_name,
        &["Counter", "Counter2", "My_Class", "A"],
        &rejected,
    );
}

#[test]
fn validate_erlang_module_name_accepts_and_rejects() {
    let rejected: Vec<(&str, String)> = [
        "_private", // underscore-leading identifiers are variables, not atoms
        "",
        "Lists",
        "my module",
        "my-module",
        "BadModule",
    ]
    .into_iter()
    .map(|n| (n, bad_module(n)))
    .collect();
    check(
        "validate_erlang_module_name",
        validate_erlang_module_name,
        &["lists", "maps", "beamtalk_runtime"],
        &rejected,
    );
}

#[test]
fn validate_selector_accepts_and_rejects() {
    let invalid = |s: &str| format!("invalid selector: '{s}'");
    let rejected_inputs = [
        "+foo",     // operator char mixed with alphanumerics
        "at: put:", // space
        "#size",    // hash
        "bad selector!",
    ];
    let mut rejected: Vec<(&str, String)> =
        rejected_inputs.iter().map(|s| (*s, invalid(s))).collect();
    rejected.push(("", "selector must not be empty".to_string()));
    check(
        "validate_selector",
        validate_selector,
        &[
            // unary
            "increment",
            "size",
            "isEmpty",
            // keyword
            "at:",
            "at:put:",
            "ifTrue:ifFalse:",
            // digits are alphanumeric: selectors have no leading-char restriction
            "at1:",
            // binary
            "+",
            "-",
            "*",
            "/",
            "<",
            ">",
            "=",
            ">=",
            "<=",
            "**",
            "~=",
        ],
        &rejected,
    );
}

// --- BT-5070: path confinement ---

#[test]
fn path_policy_confines_to_project_root() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().canonicalize().unwrap();
    std::fs::create_dir_all(root.join("src")).unwrap();
    std::fs::write(root.join("src/a.bt"), "").unwrap();
    let outside = tempfile::tempdir().unwrap();
    std::fs::write(outside.path().join("secret.bt"), "").unwrap();
    let policy = PathPolicy::Confined(root.clone());

    let ok = policy
        .check(root.join("src/a.bt").to_str().unwrap())
        .unwrap();
    assert!(ok.starts_with(&root));
    // A not-yet-existing export target is judged by its parent.
    policy
        .check(root.join("traces.json").to_str().unwrap())
        .unwrap();
    // Outside, `..` traversal and a missing parent are all refused.
    assert!(
        policy
            .check(outside.path().join("secret.bt").to_str().unwrap())
            .is_err()
    );
    let traversal = format!("{}/src/../../{}", root.display(), "etc/passwd");
    assert!(policy.check(&traversal).is_err());
    // A deeper not-yet-existing target inside the root is fine; `..` in the
    // not-yet-existing tail is refused.
    policy
        .check(root.join("no/such/dir/x.json").to_str().unwrap())
        .unwrap();
    assert!(
        policy
            .check(&format!("{}/no/../../x.json", root.display()))
            .is_err()
    );
    // PathPolicy::Unconfined lets them through.
    PathPolicy::Unconfined
        .check(outside.path().join("secret.bt").to_str().unwrap())
        .unwrap();
}

#[cfg(unix)]
#[test]
fn path_policy_refuses_symlink_escaping_the_root() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().canonicalize().unwrap();
    let outside = tempfile::tempdir().unwrap();
    std::os::unix::fs::symlink(outside.path(), root.join("link")).unwrap();
    let policy = PathPolicy::Confined(root.clone());
    assert!(policy.check(root.join("link").to_str().unwrap()).is_err());
    assert!(
        policy
            .check(root.join("link/new.json").to_str().unwrap())
            .is_err()
    );
}

#[tokio::test]
async fn confined_tools_refuse_paths_outside_the_project_root() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().canonicalize().unwrap();
    let (_fake, mut mcp) = fake_mcp(Box::new(|_| serde_json::json!({}))).await;
    mcp.path_policy = PathPolicy::Confined(root);
    let outside = tempfile::tempdir().unwrap();
    let p = outside.path().to_str().unwrap().to_string();

    let r = mcp
        .lint(Parameters(LintParams {
            path: Some(p.clone()),
            warnings_as_errors: None,
        }))
        .await
        .unwrap();
    assert_eq!(r.is_error, Some(true));
    let r = mcp
        .diagnostic_summary(Parameters(DiagnosticSummaryParams {
            path: Some(p.clone()),
        }))
        .await
        .unwrap();
    assert_eq!(r.is_error, Some(true));
    let r = mcp
        .load_file(Parameters(LoadFileParams { path: p.clone() }))
        .await
        .unwrap();
    assert_eq!(r.is_error, Some(true));
}
