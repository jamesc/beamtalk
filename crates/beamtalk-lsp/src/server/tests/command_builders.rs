// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0082 Phase 3 (BT-2289): executeCommand expression-builder tests for flush, `save_class`, `precheck_method`, `recheck_image`, `remove_method`, plus class-name/selector validation and the beamtalk.* command-list constant check.

use super::*;
use beamtalk_core::unparse::escape_string_literal;

// ----------------------------------------------------------------------
// ADR 0082 Phase 3 (BT-2289): executeCommand expression builder + helpers
// ----------------------------------------------------------------------

#[test]
fn build_flush_with_no_arguments_returns_workspace_flush() {
    let expr = build_command_expression(CMD_FLUSH, &[]).expect("ok");
    assert_eq!(expr, "Workspace flush");
}

#[test]
fn build_flush_rejects_extra_arguments() {
    let err = build_command_expression(CMD_FLUSH, &[serde_json::json!("extra")]).expect_err("err");
    assert!(err.contains("expected no arguments"));
}

#[test]
fn build_flush_class_with_valid_name_returns_class_filter() {
    let expr =
        build_command_expression(CMD_FLUSH_CLASS, &[serde_json::json!("Counter")]).expect("ok");
    assert_eq!(expr, "Workspace flush: Counter");
}

#[test]
fn build_flush_class_rejects_lowercase_first_letter() {
    let err = build_command_expression(CMD_FLUSH_CLASS, &[serde_json::json!("counter")])
        .expect_err("err");
    assert!(err.contains("must start with an uppercase letter"));
}

#[test]
fn build_flush_class_rejects_special_chars() {
    let err = build_command_expression(CMD_FLUSH_CLASS, &[serde_json::json!("Counter; rm -rf /")])
        .expect_err("err");
    assert!(err.contains("invalid character"));
}

#[test]
fn build_flush_class_rejects_empty() {
    let err = build_command_expression(CMD_FLUSH_CLASS, &[serde_json::json!("")]).expect_err("err");
    assert!(err.contains("must not be empty"));
}

#[test]
fn build_flush_file_emits_symbol_keyed_dictionary() {
    let expr = build_command_expression(CMD_FLUSH_FILE, &[serde_json::json!("src/counter.bt")])
        .expect("ok");
    assert_eq!(expr, "Workspace flush: #{ #file => \"src/counter.bt\" }");
}

#[test]
fn build_flush_file_escapes_quotes_and_braces() {
    // A pathological path with `"`, `\`, and `{` to confirm the
    // Beamtalk string escape rules match `beamtalk-mcp`.
    let expr =
        build_command_expression(CMD_FLUSH_FILE, &[serde_json::json!("a\\b\"c{d")]).expect("ok");
    assert_eq!(expr, "Workspace flush: #{ #file => \"a\\\\b\\\"c\\{d\" }");
}

#[test]
fn build_flush_kind_accepts_bare_kind() {
    let expr =
        build_command_expression(CMD_FLUSH_KIND, &[serde_json::json!("new-class")]).expect("ok");
    assert_eq!(expr, "Workspace flush: #'new-class'");
}

#[test]
fn build_flush_kind_accepts_quoted_symbol() {
    let expr =
        build_command_expression(CMD_FLUSH_KIND, &[serde_json::json!("#'new-class'")]).expect("ok");
    assert_eq!(expr, "Workspace flush: #'new-class'");
}

#[test]
fn build_flush_kind_rejects_invalid_chars() {
    let err = build_command_expression(CMD_FLUSH_KIND, &[serde_json::json!("new class")])
        .expect_err("err");
    assert!(err.contains("must be an identifier"));
}

#[test]
fn build_save_class_emits_workspace_newclass_expression() {
    let source = "Object subclass: Greeter\n  instanceMethods\n  greet => 'hi'";
    let path = "src/greeter.bt";
    let expr = build_command_expression(
        CMD_SAVE_CLASS,
        &[serde_json::json!(source), serde_json::json!(path)],
    )
    .expect("ok");
    // Roundtrip the source through the Beamtalk escaper.
    let expected_source = escape_string_literal(source);
    let expected_path = escape_string_literal(path);
    assert_eq!(
        expr,
        format!("Workspace newClass: \"{expected_source}\" at: \"{expected_path}\"")
    );
}

#[test]
fn build_save_class_rejects_empty_source() {
    let err = build_command_expression(
        CMD_SAVE_CLASS,
        &[serde_json::json!(""), serde_json::json!("src/x.bt")],
    )
    .expect_err("err");
    assert!(err.contains("source"));
}

#[test]
fn build_save_class_rejects_empty_path() {
    let err = build_command_expression(
        CMD_SAVE_CLASS,
        &[
            serde_json::json!("Object subclass: X"),
            serde_json::json!(""),
        ],
    )
    .expect_err("err");
    assert!(err.contains("path"));
}

#[test]
fn build_save_class_accepts_object_wrapped_arguments() {
    // LSP clients sometimes pack arguments as `{"source": "...", "path":
    // "..."}` instead of a positional `[source, path]`. The handler
    // accepts either shape.
    let expr = build_command_expression(
        CMD_SAVE_CLASS,
        &[
            serde_json::json!({"source": "Object subclass: X"}),
            serde_json::json!({"path": "src/x.bt"}),
        ],
    )
    .expect("ok");
    assert!(expr.starts_with("Workspace newClass: \"Object subclass: X\""));
    assert!(expr.ends_with("at: \"src/x.bt\""));
}

#[test]
fn build_precheck_method_emits_precheck_compile_source_expression() {
    let expr = build_command_expression(
        CMD_PRECHECK_METHOD,
        &[
            serde_json::json!("Counter"),
            serde_json::json!("getCount"),
            serde_json::json!("getCount => \"nope\""),
        ],
    )
    .expect("ok");
    assert_eq!(
        expr,
        "Counter precheckCompile: #getCount source: \"getCount => \\\"nope\\\"\""
    );
}

#[test]
fn build_precheck_method_accepts_leading_hash_selector() {
    // Mirrors MCP's `precheck_method`: selectors are accepted with or
    // without a leading '#'.
    let expr = build_command_expression(
        CMD_PRECHECK_METHOD,
        &[
            serde_json::json!("Counter"),
            serde_json::json!("#getCount"),
            serde_json::json!("getCount => \"nope\""),
        ],
    )
    .expect("ok");
    assert_eq!(
        expr,
        "Counter precheckCompile: #getCount source: \"getCount => \\\"nope\\\"\""
    );
}

#[test]
fn build_precheck_method_rejects_bad_class_name() {
    let err = build_command_expression(
        CMD_PRECHECK_METHOD,
        &[
            serde_json::json!("not a class"),
            serde_json::json!("getCount"),
            serde_json::json!("getCount => 1"),
        ],
    )
    .expect_err("err");
    assert!(err.contains("class name"));
}

#[test]
fn build_precheck_method_rejects_bad_selector() {
    let err = build_command_expression(
        CMD_PRECHECK_METHOD,
        &[
            serde_json::json!("Counter"),
            serde_json::json!("bad selector!"),
            serde_json::json!("getCount => 1"),
        ],
    )
    .expect_err("err");
    assert!(err.contains("selector"));
}

#[test]
fn build_precheck_method_rejects_empty_source() {
    let err = build_command_expression(
        CMD_PRECHECK_METHOD,
        &[
            serde_json::json!("Counter"),
            serde_json::json!("getCount"),
            serde_json::json!(""),
        ],
    )
    .expect_err("err");
    assert!(err.contains("source"));
}

#[test]
fn build_recheck_image_emits_workspace_recheckimage_expression() {
    let expr = build_command_expression(CMD_RECHECK_IMAGE, &[]).expect("ok");
    assert_eq!(expr, "Workspace recheckImage");
}

#[test]
fn build_recheck_image_rejects_arguments() {
    let err =
        build_command_expression(CMD_RECHECK_IMAGE, &[serde_json::json!("x")]).expect_err("err");
    assert!(err.contains("expected no arguments"));
}

// --- ADR 0112 Phase 4 (BT-3188): beamtalk.removeMethod command wiring ---

#[test]
fn build_remove_method_emits_remove_selector_expression() {
    let expr = build_command_expression(
        CMD_REMOVE_METHOD,
        &[serde_json::json!("Counter"), serde_json::json!("increment")],
    )
    .expect("ok");
    assert_eq!(expr, "Counter removeSelector: #increment");
}

#[test]
fn build_remove_method_accepts_leading_hash_selector() {
    // Mirrors MCP's `remove_method`: selectors are accepted with or
    // without a leading '#'.
    let expr = build_command_expression(
        CMD_REMOVE_METHOD,
        &[
            serde_json::json!("Counter"),
            serde_json::json!("#increment"),
        ],
    )
    .expect("ok");
    assert_eq!(expr, "Counter removeSelector: #increment");
}

#[test]
fn build_remove_method_preserves_keyword_selectors() {
    let expr = build_command_expression(
        CMD_REMOVE_METHOD,
        &[serde_json::json!("Dict"), serde_json::json!("at:put:")],
    )
    .expect("ok");
    assert_eq!(expr, "Dict removeSelector: #at:put:");
}

#[test]
fn build_remove_method_with_if_absent_emits_fallback_block() {
    // The third argument is raw Beamtalk code embedded as the fallback
    // block's body verbatim — not an escaped String value (built via
    // `beamtalk_core::tool_expr::remove_method_if_absent_expr`).
    let expr = build_command_expression(
        CMD_REMOVE_METHOD,
        &[
            serde_json::json!("Counter"),
            serde_json::json!("bogus"),
            serde_json::json!("\"not found\""),
        ],
    )
    .expect("ok");
    assert_eq!(
        expr,
        "Counter removeSelector: #bogus ifAbsent: [\"not found\"]"
    );
}

#[test]
fn build_remove_method_treats_explicit_null_third_argument_as_no_fallback() {
    // Some JSON-RPC clients pad positional arguments with `null` rather
    // than omitting the trailing slot entirely.
    let expr = build_command_expression(
        CMD_REMOVE_METHOD,
        &[
            serde_json::json!("Counter"),
            serde_json::json!("increment"),
            serde_json::Value::Null,
        ],
    )
    .expect("ok");
    assert_eq!(expr, "Counter removeSelector: #increment");
}

#[test]
fn build_remove_method_rejects_bad_class_name() {
    let err = build_command_expression(
        CMD_REMOVE_METHOD,
        &[
            serde_json::json!("not a class"),
            serde_json::json!("increment"),
        ],
    )
    .expect_err("err");
    assert!(err.contains("class name"));
}

#[test]
fn build_remove_method_rejects_bad_selector() {
    let err = build_command_expression(
        CMD_REMOVE_METHOD,
        &[
            serde_json::json!("Counter"),
            serde_json::json!("bad selector!"),
        ],
    )
    .expect_err("err");
    assert!(err.contains("selector"));
}

#[test]
fn build_remove_method_accepts_object_wrapped_arguments() {
    let expr = build_command_expression(
        CMD_REMOVE_METHOD,
        &[
            serde_json::json!({"class": "Counter"}),
            serde_json::json!({"selector": "increment"}),
        ],
    )
    .expect("ok");
    assert_eq!(expr, "Counter removeSelector: #increment");
}

#[test]
fn build_unknown_command_returns_error() {
    let err = build_command_expression("not.a.real.command", &[]).expect_err("err");
    assert!(err.contains("unknown LSP command"));
}

#[test]
fn validate_class_name_accepts_pascal_case() {
    assert!(validate_class_name("Counter").is_ok());
    assert!(validate_class_name("FooBarBaz").is_ok());
    assert!(validate_class_name("X123").is_ok());
    assert!(validate_class_name("X_y").is_ok());
}

#[test]
fn validate_class_name_rejects_bad_shapes() {
    assert!(validate_class_name("").is_err());
    assert!(validate_class_name("lowercase").is_err());
    assert!(validate_class_name("With Space").is_err());
    assert!(validate_class_name("Bad!").is_err());
    assert!(validate_class_name("123Foo").is_err());
}

#[test]
fn beamtalk_lsp_commands_list_matches_constants() {
    // Surface-parity drift check fodder: the constants drive the
    // capability list. Verify there are no accidental omissions.
    let listed: HashSet<&str> = BEAMTALK_LSP_COMMANDS.iter().copied().collect();
    assert!(listed.contains(CMD_FLUSH));
    assert!(listed.contains(CMD_FLUSH_CLASS));
    assert!(listed.contains(CMD_FLUSH_FILE));
    assert!(listed.contains(CMD_FLUSH_KIND));
    assert!(listed.contains(CMD_SAVE_CLASS));
    assert!(listed.contains(CMD_PRECHECK_METHOD));
    assert!(listed.contains(CMD_RECHECK_IMAGE));
    assert!(listed.contains(CMD_REMOVE_METHOD));
    assert_eq!(listed.len(), 8);
}

#[test]
fn validate_selector_accepts_unary_keyword_and_binary() {
    assert!(validate_selector("size").is_ok());
    assert!(validate_selector("at:put:").is_ok());
    assert!(validate_selector("+").is_ok());
    assert!(validate_selector(">=").is_ok());
}

#[test]
fn validate_selector_rejects_bad_shapes() {
    assert!(validate_selector("").is_err());
    assert!(validate_selector("bad selector!").is_err());
    assert!(validate_selector("+foo").is_err());
}
