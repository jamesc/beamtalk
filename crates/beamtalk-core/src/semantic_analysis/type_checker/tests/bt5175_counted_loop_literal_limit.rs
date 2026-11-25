// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5175: a literal non-numeric limit on `to:do:` / `to:by:do:` is a
//! compile-time Type diagnostic. `1 to: nil do: [...]` is caught here because
//! the argument is a literal (so its type is `Known`); a limit that arrives
//! through an untyped parameter is `Dynamic` and is caught at runtime by the
//! `type_error` guard instead.

use super::common::*;

fn expects_diags_for(limit: &str) -> usize {
    let source = format!(
        "Object subclass: Looper\n  run =>\n    1 to: {limit} do: [:i | i]\n  runBy =>\n    1 to: {limit} by: 1 do: [:i | i]\n"
    );
    diags_with_expect(&source)
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .filter(|d| d.message.contains("expects Integer"))
        .count()
}

#[test]
fn bt5175_literal_nil_limit_is_type_diagnostic() {
    assert_eq!(expects_diags_for("nil"), 2);
}

#[test]
fn bt5175_literal_string_limit_is_type_diagnostic() {
    assert_eq!(expects_diags_for("\"x\""), 2);
}

#[test]
fn bt5175_literal_symbol_limit_is_type_diagnostic() {
    assert_eq!(expects_diags_for("#sym"), 2);
}

#[test]
fn bt5175_integer_limit_has_no_diagnostic() {
    assert_eq!(expects_diags_for("10"), 0);
}
