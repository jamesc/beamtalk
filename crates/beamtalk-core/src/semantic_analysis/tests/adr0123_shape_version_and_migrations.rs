// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0123 `shapeVersion:` / `migrateFromVN:` compile-time diagnostics:
//! `native:` refusal, the unreachable-migration warning, the
//! class-variable-access compile error, and the `Dictionary` return-type
//! check.

use super::*;

fn diags(src: &str) -> Vec<Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    analyse(&module).diagnostics
}

#[test]
fn plain_shape_version_and_migration_is_silent() {
    let src = concat!(
        "Actor subclass: Cart\n",
        "  shapeVersion: 2\n",
        "  state: items :: List = #()\n",
        "  state: total :: Integer = 0\n",
        "\n",
        "  class migrateFromV1: old -> Dictionary =>\n",
        "    old\n",
    );
    let diagnostics = diags(src);
    assert!(
        !diagnostics
            .iter()
            .any(|d| d.message.contains("migrateFromV1:") || d.message.contains("shapeVersion")),
        "well-formed shapeVersion:/migrateFromVN: must not diagnose, got: {diagnostics:?}"
    );
}

#[test]
fn native_class_rejects_shape_version() {
    let src = concat!(
        "Actor subclass: Native1 native: native1_impl\n", //
        "  shapeVersion: 2\n",
    );
    let diagnostics = diags(src);
    assert!(
        diagnostics.iter().any(|d| {
            d.message.contains("shapeVersion:")
                && d.message.contains("native")
                && d.category == Some(crate::source_analysis::DiagnosticCategory::Type)
        }),
        "shapeVersion: on a native: class must be a compile error, got: {diagnostics:?}"
    );
}

#[test]
fn native_class_rejects_migrate_from_vn() {
    let src = concat!(
        "Actor subclass: Native2 native: native2_impl\n",
        "\n",
        "  class migrateFromV1: old -> Dictionary =>\n",
        "    old\n",
    );
    let diagnostics = diags(src);
    assert!(
        diagnostics
            .iter()
            .any(|d| { d.message.contains("migrateFromV1:") && d.message.contains("native") }),
        "migrateFromVN: on a native: class must be a compile error, got: {diagnostics:?}"
    );
}

#[test]
fn unreachable_migration_at_current_version_warns() {
    let src = concat!(
        "Actor subclass: Cart\n",
        "  shapeVersion: 2\n",
        "  state: items = #()\n",
        "\n",
        "  class migrateFromV2: old -> Dictionary =>\n",
        "    old\n",
    );
    let diagnostics = diags(src);
    assert!(
        diagnostics.iter().any(|d| {
            d.severity == crate::source_analysis::Severity::Warning
                && d.message.contains("migrateFromV2:")
                && d.message.contains("unreachable")
        }),
        "migrateFromV2: with shapeVersion: 2 must warn as unreachable, got: {diagnostics:?}"
    );
}

#[test]
fn unreachable_migration_above_current_version_warns() {
    let src = concat!(
        "Actor subclass: Cart\n",
        "  shapeVersion: 2\n",
        "  state: items = #()\n",
        "\n",
        "  class migrateFromV7: old -> Dictionary =>\n",
        "    old\n",
    );
    let diagnostics = diags(src);
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("migrateFromV7:") && d.message.contains("unreachable")),
        "migrateFromV7: with shapeVersion: 2 must warn as unreachable, got: {diagnostics:?}"
    );
}

#[test]
fn reachable_migration_below_current_version_is_silent() {
    let src = concat!(
        "Actor subclass: Cart\n",
        "  shapeVersion: 3\n",
        "  state: items = #()\n",
        "\n",
        "  class migrateFromV1: old -> Dictionary =>\n",
        "    old\n",
    );
    let diagnostics = diags(src);
    assert!(
        !diagnostics
            .iter()
            .any(|d| d.message.contains("unreachable")),
        "migrateFromV1: below shapeVersion: 3 must not warn, got: {diagnostics:?}"
    );
}

#[test]
fn migration_reading_class_variable_is_compile_error() {
    let src = concat!(
        "Actor subclass: Cart\n",
        "  shapeVersion: 2\n",
        "  state: items = #()\n",
        "  classState: taxRate = 0.2\n",
        "\n",
        "  class migrateFromV1: old -> Dictionary =>\n",
        "    old at: #tax put: self.taxRate\n",
    );
    let diagnostics = diags(src);
    assert!(
        diagnostics.iter().any(|d| {
            d.severity == crate::source_analysis::Severity::Error
                && d.message.contains("class variable")
                && d.message.contains("taxRate")
        }),
        "reading a class variable in migrateFromV1: must be a compile error, got: {diagnostics:?}"
    );
}

#[test]
fn migration_writing_class_variable_is_compile_error() {
    let src = concat!(
        "Actor subclass: Cart\n",
        "  shapeVersion: 2\n",
        "  state: items = #()\n",
        "  classState: taxRate = 0.2\n",
        "\n",
        "  class migrateFromV1: old -> Dictionary =>\n",
        "    self.taxRate := 0.3\n",
        "    old\n",
    );
    let diagnostics = diags(src);
    assert!(
        diagnostics.iter().any(|d| {
            d.severity == crate::source_analysis::Severity::Error
                && d.message.contains("class variable")
                && d.message.contains("taxRate")
        }),
        "writing a class variable in migrateFromV1: must be a compile error, got: {diagnostics:?}"
    );
}

#[test]
fn migration_return_type_must_be_dictionary() {
    let src = concat!(
        "Actor subclass: Cart\n",
        "  shapeVersion: 2\n",
        "  state: items = #()\n",
        "\n",
        "  class migrateFromV1: old -> Integer =>\n",
        "    0\n",
    );
    let diagnostics = diags(src);
    assert!(
        diagnostics.iter().any(|d| {
            d.severity == crate::source_analysis::Severity::Error
                && d.message.contains("migrateFromV1:")
                && d.message.contains("Dictionary")
        }),
        "migrateFromV1: -> Integer must be a compile error, got: {diagnostics:?}"
    );
}

#[test]
fn instance_method_named_like_migration_is_ignored() {
    // Only a *class-side* `migrateFromVN:` participates — an instance
    // method with the same selector shape is ordinary user code.
    let src = concat!(
        "Actor subclass: Cart\n",
        "  shapeVersion: 1\n",
        "  state: items = #()\n",
        "\n",
        "  migrateFromV1: old => old\n",
    );
    let diagnostics = diags(src);
    assert!(
        !diagnostics
            .iter()
            .any(|d| d.message.contains("migrateFromV1:")),
        "an instance-side migrateFromV1: must not be treated as a migration step, got: {diagnostics:?}"
    );
}
