// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk-expression builders for REPL meta-commands, shared between the
//! `beamtalk` binary's dispatch (`commands/repl/mod.rs`) and the
//! `tests/repl_protocol.rs` integration-test harness.
//!
//! These live in the library target (rather than `commands/repl/mod.rs`,
//! which is private to the binary target) specifically so the integration
//! test — which links only the library target — can call the real parsing
//! logic instead of re-implementing it. A hand-copied "mirror" of this
//! logic in the test harness previously drifted from the real
//! implementation twice in a row (BT-3189); see BT-3196 for extracting
//! `:flush <sel>`'s equivalent duplication the same way.

/// Construct the `<Class> removeSelector: #<selector>` expression a
/// `:remove-method <Class> <selector>` REPL line dispatches to (ADR 0112
/// Phase 4, BT-3189).
///
/// Splits on the first run of whitespace: the first token is the class, the
/// remainder (trimmed) is the selector. A leading `#` on the selector is
/// stripped and the result re-trimmed, mirroring the MCP `remove_method`
/// tool's `selector` parameter and the LSP `beamtalk.removeMethod` command,
/// so `:remove-method Counter #increment` and `:remove-method Counter
/// increment` both work. Returns `None` when either half is missing or
/// empty (including a selector that strips down to nothing, e.g. bare `#`
/// or `#` followed only by whitespace), so the caller can print a usage
/// hint instead of evaluating a malformed expression.
pub fn remove_method_expr_for(arg: &str) -> Option<String> {
    let arg = arg.trim();
    let mut parts = arg.splitn(2, char::is_whitespace);
    let class = parts.next().unwrap_or("").trim();
    let selector = parts.next().unwrap_or("").trim();
    let selector = selector.strip_prefix('#').unwrap_or(selector).trim();
    if class.is_empty() || selector.is_empty() {
        return None;
    }
    Some(format!("{class} removeSelector: #{selector}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn translates_to_remove_selector_send() {
        assert_eq!(
            remove_method_expr_for("Counter increment"),
            Some("Counter removeSelector: #increment".to_string())
        );
    }

    #[test]
    fn preserves_keyword_selectors() {
        assert_eq!(
            remove_method_expr_for("Dict at:put:"),
            Some("Dict removeSelector: #at:put:".to_string())
        );
    }

    #[test]
    fn strips_leading_hash_on_selector() {
        assert_eq!(
            remove_method_expr_for("Counter #increment"),
            Some("Counter removeSelector: #increment".to_string())
        );
    }

    #[test]
    fn trims_surrounding_whitespace() {
        assert_eq!(
            remove_method_expr_for("  Counter   increment  "),
            Some("Counter removeSelector: #increment".to_string())
        );
    }

    #[test]
    fn with_missing_selector_reports_no_expression() {
        assert_eq!(remove_method_expr_for("Counter"), None);
        assert_eq!(remove_method_expr_for("Counter "), None);
    }

    #[test]
    fn with_empty_argument_reports_no_expression() {
        assert_eq!(remove_method_expr_for(""), None);
        assert_eq!(remove_method_expr_for("   "), None);
    }

    #[test]
    fn with_bare_hash_selector_reports_no_expression() {
        // A selector of just "#" strips down to empty and must hit the
        // usage-error path, not a malformed `Counter removeSelector: #` eval.
        assert_eq!(remove_method_expr_for("Counter #"), None);
    }

    #[test]
    fn with_only_whitespace_after_hash_reports_no_expression() {
        // "#" followed by only whitespace must re-trim to empty and hit the
        // usage-error path, not just the bare-hash case above.
        assert_eq!(remove_method_expr_for("Counter #  "), None);
    }

    #[test]
    fn strips_whitespace_between_hash_and_selector() {
        // Whitespace right after "#" (before real selector content) must be
        // trimmed away, producing a clean expression instead of the
        // malformed `Counter removeSelector: #  increment`.
        assert_eq!(
            remove_method_expr_for("Counter #  increment"),
            Some("Counter removeSelector: #increment".to_string())
        );
    }
}
