// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tool-router registration checks and the small offline handlers (`diagnostic_summary`, `try_method_expr`, `compute_doc_method_categories`).

use super::*;

// --- tool registration: reload_class replaces reload_module ---

#[test]
fn reload_class_tool_registered() {
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"reload_class"),
        "reload_class should be in tool list, found: {tool_names:?}"
    );
    assert!(
        !tool_names.contains(&"reload_module"),
        "reload_module should not be in tool list (replaced by reload_class)"
    );
}

#[test]
fn precheck_method_and_recheck_image_tools_registered() {
    // ADR 0105 Phase 3: surface-parity entries for MCP.
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"precheck_method"),
        "precheck_method should be in tool list, found: {tool_names:?}"
    );
    assert!(
        tool_names.contains(&"recheck_image"),
        "recheck_image should be in tool list, found: {tool_names:?}"
    );
}

#[test]
fn list_modules_tool_not_registered() {
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        !tool_names.contains(&"list_modules"),
        "list_modules should not be in tool list (removed in this PR)"
    );
}

// ADR 0081 Phase 6: the get_bindings / clear tools were removed —
// session state is read and reset via `evaluate` (`Session current bindings
// keys`, `Session current clear`).
#[test]
fn session_state_tools_not_registered() {
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        !tool_names.contains(&"get_bindings"),
        "get_bindings should not be in tool list (removed; use evaluate)"
    );
    assert!(
        !tool_names.contains(&"clear"),
        "clear should not be in tool list (removed; use evaluate)"
    );
}

// --- tool registration: package tools (ADR 0070 Phase 5) ---

#[test]
fn list_packages_tool_registered() {
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"list_packages"),
        "list_packages should be in tool list, found: {tool_names:?}"
    );
}

#[test]
fn package_classes_tool_registered() {
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"package_classes"),
        "package_classes should be in tool list, found: {tool_names:?}"
    );
}

// --- diagnostic_summary ---

#[test]
fn diagnostic_summary_tool_registered() {
    let router = BeamtalkMcp::tool_router();
    let tools = router.list_all();
    let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
    assert!(
        tool_names.contains(&"diagnostic_summary"),
        "diagnostic_summary should be in tool list, found: {tool_names:?}"
    );
}

#[test]
fn compute_diagnostic_summary_nonexistent_path() {
    let result = compute_diagnostic_summary("/nonexistent/path/that/does/not/exist");
    assert_eq!(result["files_checked"], 0);
    assert_eq!(result["total"], 0);
    assert!(result["error"].is_string());
}

#[test]
fn compute_diagnostic_summary_clean_file() {
    // A well-formed source file should produce a summary with files_checked=1.
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("clean.bt");
    std::fs::write(&file, "Object subclass: Clean\n  class hello => 42\n").unwrap();
    let result = compute_diagnostic_summary(file.to_str().unwrap());
    assert_eq!(result["files_checked"], 1);
    // Total may include some diagnostics from semantic analysis depending
    // on the class environment, but files_checked must be correct.
    assert!(result["total"].is_number());
    assert!(result["type_coverage"]["typed"].is_number());
    assert!(result["type_coverage"]["total"].is_number());
    assert!(result["type_coverage"]["dynamic_percent"].is_number());
}

// --- ADR 0082 Phase 3: MCP ChangeLog / flush tool wiring ---
//
// The tools below pin the Beamtalk expression each MCP tool dispatches to,
// matching the REPL meta-command tests in
// crates/beamtalk-cli/src/commands/repl/handlers.rs. Surface drift in the
// expression mapping fails CI through these tests.

#[test]
fn try_method_expr_compiles_ephemeral_patch() {
    // `try_method` → `aClass tryCompile: #selector source: body`.
    assert_eq!(
        try_method_expr("Counter", "doubled", "^ self value * 2"),
        "Counter tryCompile: #doubled source: \"^ self value * 2\"",
    );
}

// `precheck_method_expr`, `remove_method_expr`,
// `remove_method_if_absent_expr`, and `flush_expr`/`FlushFilter` are
// defined in `beamtalk_core::tool_expr` and golden-tested
// there — that suite is the single source of truth both this crate and
// `beamtalk-lsp` call into, so there is nothing left to re-test here.

// --- compute_doc_method_categories ---

#[test]
fn compute_doc_method_categories_groups_by_divider() {
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("counter.bt");
    std::fs::write(
        &file,
        "Object subclass: Counter\n\
             \n\
             \x20\x20// === Construction ===\n\
             \x20\x20class new => self basicNew\n\
             \n\
             \x20\x20// === Arithmetic ===\n\
             \x20\x20increment => self.value := self.value + 1\n",
    )
    .unwrap();

    let result = compute_doc_method_categories(file.to_str().unwrap(), "Counter")
        .expect("categorization should succeed");
    assert_eq!(result["class"], "Counter");
    let categories = result["categories"].as_array().unwrap();
    assert_eq!(categories.len(), 2);
    assert_eq!(categories[0]["name"], "Construction");
    assert_eq!(categories[0]["methods"][0]["selector"], "new");
    assert_eq!(categories[0]["methods"][0]["side"], "class");
    assert_eq!(categories[1]["name"], "Arithmetic");
    assert_eq!(categories[1]["methods"][0]["selector"], "increment");
    assert_eq!(categories[1]["methods"][0]["side"], "instance");
}

#[test]
fn compute_doc_method_categories_no_dividers_is_single_unnamed_category() {
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("plain.bt");
    std::fs::write(&file, "Object subclass: Plain\n  foo => 1\n").unwrap();

    let result = compute_doc_method_categories(file.to_str().unwrap(), "Plain")
        .expect("categorization should succeed");
    let categories = result["categories"].as_array().unwrap();
    assert_eq!(categories.len(), 1);
    assert!(categories[0]["name"].is_null());
}

#[test]
fn compute_doc_method_categories_missing_file_is_none() {
    assert!(compute_doc_method_categories("/nonexistent/path/nope.bt", "Counter").is_none());
}

#[test]
fn compute_doc_method_categories_class_not_found_is_none() {
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("counter.bt");
    std::fs::write(&file, "Object subclass: Counter\n  foo => 1\n").unwrap();

    assert!(compute_doc_method_categories(file.to_str().unwrap(), "NoSuchClass").is_none());
}

// --- BT-5070: every tool declares read-only / destructive annotations ---

#[test]
fn every_tool_declares_read_only_and_destructive_hints() {
    let tools = BeamtalkMcp::tool_router().list_all();
    for t in &tools {
        let a = t
            .annotations
            .as_ref()
            .unwrap_or_else(|| panic!("tool {} has no annotations", t.name));
        let ro = a
            .read_only_hint
            .unwrap_or_else(|| panic!("tool {} has no readOnlyHint", t.name));
        if !ro {
            assert!(
                a.destructive_hint.is_some(),
                "non-read-only tool {} has no destructiveHint",
                t.name
            );
        }
    }
    let hints = |name: &str| {
        let t = tools.iter().find(|t| t.name == name).unwrap();
        let a = t.annotations.as_ref().unwrap();
        (a.read_only_hint, a.destructive_hint)
    };
    assert_eq!(hints("lint").0, Some(true));
    assert_eq!(hints("flush"), (Some(false), Some(true)));
    assert_eq!(hints("remove_class"), (Some(false), Some(true)));
    assert_eq!(hints("export_traces"), (Some(false), Some(true)));
}
