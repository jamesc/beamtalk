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

#[test]
fn complete_cursor_must_be_on_a_char_boundary() {
    // BT-5078: `{"code": "é", "cursor": 1}` used to slice inside the 2-byte
    // `é` and panic the handler.
    let err = code_up_to_cursor("é", Some(1)).expect_err("mid-character cursor is rejected");
    assert_eq!(err.code, rmcp::model::ErrorCode::INVALID_PARAMS);
    assert!(
        err.message.contains("character boundary"),
        "{}",
        err.message
    );

    assert_eq!(code_up_to_cursor("é", Some(2)).unwrap(), ("é", 2));
    assert_eq!(code_up_to_cursor("é", Some(0)).unwrap(), ("", 0));
    // Default is end of code; a cursor past the end is clamped.
    assert_eq!(code_up_to_cursor("aé", None).unwrap(), ("aé", 3));
    assert_eq!(code_up_to_cursor("abc", Some(99)).unwrap(), ("abc", 3));
}

// --- BT-5070: path confinement ---

#[test]
fn path_policy_confines_to_project_root() {
    let tmp = tempfile::tempdir().unwrap();
    let root = beamtalk_core::paths::canonicalize(tmp.path()).unwrap();
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

/// BT-5108: the checked path is in the same plain form `paths::canonicalize`
/// gives (never a Windows `\\?\` path), and a root passed in verbatim form is
/// compared in plain form, so confinement does not miss on Windows.
#[test]
fn path_policy_returns_plain_canonical_paths() {
    let tmp = tempfile::tempdir().unwrap();
    let root = beamtalk_core::paths::canonicalize(tmp.path()).unwrap();
    std::fs::write(root.join("a.bt"), "").unwrap();
    let policy = PathPolicy::Confined(root.clone());
    let got = policy.check(root.join("a.bt").to_str().unwrap()).unwrap();
    assert_eq!(got, root.join("a.bt"));
    assert!(!got.to_string_lossy().starts_with(r"\\?\"));
}

/// BT-5123: a root still in Windows verbatim (`\\?\`) form, as `std::fs::canonicalize`
/// gives, confines the same as its plain form.
#[cfg(windows)]
#[test]
fn path_policy_accepts_verbatim_root_on_windows() {
    let tmp = tempfile::tempdir().unwrap();
    let verbatim = tmp.path().canonicalize().unwrap();
    assert!(verbatim.to_string_lossy().starts_with(r"\\?\"));
    let plain = beamtalk_core::paths::canonicalize(tmp.path()).unwrap();
    std::fs::write(plain.join("a.bt"), "").unwrap();
    let policy = PathPolicy::Confined(verbatim);
    let got = policy.check(plain.join("a.bt").to_str().unwrap()).unwrap();
    assert!(got.starts_with(&plain));
}

#[cfg(unix)]
#[test]
fn path_policy_refuses_symlink_escaping_the_root() {
    let tmp = tempfile::tempdir().unwrap();
    let root = beamtalk_core::paths::canonicalize(tmp.path()).unwrap();
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

#[cfg(unix)]
#[test]
fn path_policy_refuses_dangling_symlink() {
    let tmp = tempfile::tempdir().unwrap();
    let root = beamtalk_core::paths::canonicalize(tmp.path()).unwrap();
    let outside = tempfile::tempdir().unwrap();
    let target = outside.path().join("file");
    std::os::unix::fs::symlink(&target, root.join("out.json")).unwrap();
    let policy = PathPolicy::Confined(root.clone());
    assert!(
        policy
            .check(root.join("out.json").to_str().unwrap())
            .is_err()
    );
    // A symlink loop (ELOOP) is not "not found" either.
    std::os::unix::fs::symlink(root.join("loop"), root.join("loop")).unwrap();
    assert!(policy.check(root.join("loop").to_str().unwrap()).is_err());
    // A genuinely absent tail is still fine.
    assert!(
        policy
            .check(root.join("new/x.json").to_str().unwrap())
            .is_ok()
    );
}

#[tokio::test]
async fn confined_tools_refuse_paths_outside_the_project_root() {
    let tmp = tempfile::tempdir().unwrap();
    let root = beamtalk_core::paths::canonicalize(tmp.path()).unwrap();
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

// --- BT-5174: no confinement to $HOME or `/` ---

#[test]
fn confine_to_root_refuses_home_and_filesystem_root() {
    let home = tempfile::tempdir().unwrap();
    let err = PathPolicy::confine_to_root(home.path(), Some(home.path()), false).unwrap_err();
    assert!(
        err.to_string().contains("no beamtalk project found"),
        "{err}"
    );
    assert!(PathPolicy::confine_to_root(std::path::Path::new("/"), None, true).is_err());

    // A project below home, or any directory when HOME is unset, is fine.
    let project = home.path().join("proj");
    std::fs::create_dir_all(&project).unwrap();
    assert!(PathPolicy::confine_to_root(&project, Some(home.path()), false).is_ok());
    assert!(PathPolicy::confine_to_root(home.path(), None, false).is_ok());
}

/// BT-5185: a directory *above* home (`/home`, `/Users`, `C:\Users`) holds
/// every user's home, so it is refused like home itself.
#[test]
fn confine_to_root_refuses_a_directory_containing_home() {
    let base = tempfile::tempdir().unwrap();
    let home = base.path().join("users").join("alice");
    std::fs::create_dir_all(&home).unwrap();
    let users = base.path().join("users");
    let err = PathPolicy::confine_to_root(&users, Some(&home), false).unwrap_err();
    assert!(
        err.to_string().contains("no beamtalk project found"),
        "{err}"
    );
    assert!(PathPolicy::confine_to_root(base.path(), Some(&home), false).is_err());
    // A real project (it has a marker) that contains a hermetic home is fine;
    // home itself never is.
    assert!(PathPolicy::confine_to_root(&users, Some(&home), true).is_ok());
    assert!(PathPolicy::confine_to_root(&home, Some(&home), true).is_err());
}
