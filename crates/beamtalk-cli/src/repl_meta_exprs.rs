// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk-expression builders for REPL meta-commands, shared between the
//! `beamtalk` binary's dispatch (`commands/repl/mod.rs`) and the
//! `tests/repl_protocol.rs` integration-test harness.
//!
//! These live in the library target (rather than `commands/repl/mod.rs`,
//! which is private to the binary target) specifically so the integration
//! test — which links only the library target — can call the real parsing
//! logic instead of re-implementing it. A hand-copied "mirror" of
//! `remove_method_expr_for`'s logic in the test harness previously drifted
//! from the real implementation twice in a row (BT-3189); `:flush <sel>`'s
//! equivalent duplication was extracted the same way in BT-3196.

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

/// Construct the `Workspace flush: <selector>` expression a `:flush
/// <selector>` REPL line dispatches to (ADR 0082 Phase 3, BT-2287).
///
/// `selector` is passed through **verbatim** (only trimmed) into the
/// generated expression, so callers can pass a Class (`Counter`), a Symbol
/// kind (`#'new-class'`), or a Dictionary (`#{ #file => "path" }`) — unlike
/// [`remove_method_expr_for`], there is no leading-`#` stripping here since
/// a `#`-prefixed selector is a legitimate Symbol-kind argument, not a
/// Symbol-literal-decorated method name.
///
/// Returns `None` when `selector` is empty (including a selector that is
/// only whitespace, once trimmed), so the caller can print a usage hint
/// instead of evaluating a malformed bare `Workspace flush: ` send — this
/// emptiness check is exactly the kind of guard a hand-copied test-harness
/// mirror can silently drop (as `remove_method_expr_for`'s did for its own
/// `#`-stripping order in BT-3189); routing both callers through this one
/// function makes that class of drift structurally impossible.
pub fn flush_expr_for(selector: &str) -> Option<String> {
    let selector = selector.trim();
    if selector.is_empty() {
        return None;
    }
    Some(format!("Workspace flush: {selector}"))
}

/// Construct the `<Class> removeFromSystem` expression a `:remove-class
/// <Class>` REPL line dispatches to (ADR 0113 Phase 4, BT-3210).
///
/// `arg` is a single class name — trimmed, and returned as `None` when empty
/// (including whitespace-only) so the caller can print a usage hint instead
/// of evaluating a malformed bare `removeFromSystem` send. Unlike
/// [`remove_method_expr_for`], there is no second (selector) token to split
/// off and no leading-`#` stripping — a class name is never symbol-decorated.
///
/// The real REPL dispatch (`commands/repl/mod.rs`) prompts for `y/N`
/// confirmation before calling this — a destructive, memory-mutating
/// operation per ADR 0113's Surface section — but that terminal-only
/// confirmation step is orthogonal to expression construction, so it isn't
/// modeled here (matching how `flush_expr_for`/`remove_method_expr_for`
/// don't model any REPL-side confirmation either).
pub fn remove_class_expr_for(arg: &str) -> Option<String> {
    let class = arg.trim();
    if class.is_empty() {
        return None;
    }
    Some(format!("{class} removeFromSystem"))
}

/// The unscoped `Workspace flushIncludingDestructive` expression a bare
/// `:flush-destructive` REPL line dispatches to (ADR 0113 Phase 4, BT-3210).
///
/// A `const`, not a function, because the bare form takes no argument to
/// build from — mirroring `Flush`'s own bare-form handling in
/// `commands/repl/mod.rs` (`"Workspace flush"` is written as a literal there
/// too; this one is exposed as a shared constant instead purely so the
/// integration-test harness in `tests/repl_protocol.rs` can reference it by
/// name rather than re-typing the literal — see the module doc's
/// no-hand-copied-mirror rationale).
pub const FLUSH_INCLUDING_DESTRUCTIVE_EXPR: &str = "Workspace flushIncludingDestructive";

/// Construct the `Workspace flush: <selector> confirmDestructive: true`
/// expression a `:flush-destructive <selector>` REPL line dispatches to
/// (ADR 0113 Phase 4, BT-3210).
///
/// `selector` is passed through **verbatim** (only trimmed), exactly like
/// [`flush_expr_for`] — a Class, a Symbol kind (`#'remove-class'`), or a
/// Dictionary (`#{ #file => "path" }`) are all legal scopes for
/// `workspace_interface.bt`'s `flush: filter confirmDestructive: confirmDestructive`
/// keyword form. Returns `None` when `selector` is empty (including
/// whitespace-only), so the caller can print a usage hint instead of
/// evaluating a malformed bare `Workspace flush: confirmDestructive: true`
/// send — same emptiness guard `flush_expr_for` uses, for the same reason.
///
/// **After `:remove-class <Class>`, scope by `#Class` (a Symbol literal),
/// not the bare class name.** `removeFromSystem` already unbinds the name —
/// a bare `<Class>` argument here fails to *evaluate* (an unresolved-class
/// error) before the `flush:` send ever runs. `beamtalk_workspace_flush`'s
/// filter normalisation matches a Symbol against the `ChangeLog` entry's
/// recorded `class` field by name, needing no live class to resolve —
/// exactly the case a destructive flush of an already-removed class needs.
/// This is pre-existing `Workspace flush:` behaviour (its Class-vs-Symbol-
/// kind filter dispatch predates ADR 0113), not something new here; it just
/// becomes load-bearing for this REPL pairing specifically because
/// `:remove-class` always runs first.
pub fn flush_destructive_expr_for(selector: &str) -> Option<String> {
    let selector = selector.trim();
    if selector.is_empty() {
        return None;
    }
    Some(format!(
        "Workspace flush: {selector} confirmDestructive: true"
    ))
}

/// Construct the `<Class> renameTo: #<NewName>` expression a `:rename-class
/// <Class> <NewName>` REPL line dispatches to (ADR 0114 Phase 5, BT-3276).
///
/// Splits on whitespace into exactly two tokens — the current class name and
/// the new one. Unlike [`remove_method_expr_for`]'s selector, the first
/// token is never symbol-decorated (a class name is a bare identifier); the
/// second gets the same optional-leading-`#`-strip treatment for symmetry
/// with how the resulting Symbol literal is written. Returns `None` when the
/// argument doesn't split into exactly two non-empty tokens (after
/// `#`-stripping the second), so the caller can print a usage hint instead
/// of evaluating a malformed `renameTo:` send.
///
/// The real REPL dispatch (`commands/repl/mod.rs`) prompts for `y/N`
/// confirmation before calling this — a destructive, memory-mutating
/// operation per ADR 0114's Surface section (reusing ADR 0113's) — but that
/// terminal-only confirmation step is orthogonal to expression construction,
/// so it isn't modeled here, matching [`remove_class_expr_for`].
pub fn rename_class_expr_for(arg: &str) -> Option<String> {
    let mut parts = arg.split_whitespace();
    let old_name = parts.next()?;
    let new_name = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let new_name = new_name.strip_prefix('#').unwrap_or(new_name);
    if new_name.is_empty() {
        return None;
    }
    Some(format!("{old_name} renameTo: #{new_name}"))
}

/// Construct the `<Class> renameSelector: #<OldSelector> to: #<NewSelector>`
/// expression a `:rename-method <Class> <OldSelector> <NewSelector>` REPL
/// line dispatches to (ADR 0114 Phase 5, BT-3276). Instance-side only — sent
/// to a bare class name, this always touches the instance-side method table;
/// a class-side rename needs a direct `Counter class renameSelector: ... to:
/// ...` eval, the same chokepoint limitation [`remove_method_expr_for`] has
/// (`docs/development/surface-parity.md`'s `remove-method` row).
///
/// Splits on whitespace into exactly three tokens — the class, the current
/// selector, and the new selector — stripping an optional leading `#` off
/// each selector token (mirroring `remove_method_expr_for`), so
/// `:rename-method Counter increment incrementBy` and `:rename-method
/// Counter #increment #incrementBy` both work. Returns `None` when the
/// argument doesn't split into exactly three tokens, or either selector
/// strips down to empty (e.g. a bare `#`), so the caller can print a usage
/// hint instead of evaluating a malformed `renameSelector:to:` send.
pub fn rename_method_expr_for(arg: &str) -> Option<String> {
    let mut parts = arg.split_whitespace();
    let class = parts.next()?;
    let old_selector = parts.next()?;
    let new_selector = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let old_selector = old_selector.strip_prefix('#').unwrap_or(old_selector);
    let new_selector = new_selector.strip_prefix('#').unwrap_or(new_selector);
    if old_selector.is_empty() || new_selector.is_empty() {
        return None;
    }
    Some(format!(
        "{class} renameSelector: #{old_selector} to: #{new_selector}"
    ))
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

    // -----------------------------------------------------------------------
    // flush_expr_for
    // -----------------------------------------------------------------------

    #[test]
    fn flush_class_selector_translates_to_workspace_flush_message() {
        assert_eq!(
            flush_expr_for("Counter"),
            Some("Workspace flush: Counter".to_string())
        );
    }

    #[test]
    fn flush_symbol_kind_selector_translates_verbatim() {
        // A leading "#" here is a legitimate Symbol-kind argument (not a
        // decorated method name like `remove_method_expr_for`'s selector),
        // so it must survive untouched.
        assert_eq!(
            flush_expr_for("#'new-class'"),
            Some("Workspace flush: #'new-class'".to_string())
        );
    }

    #[test]
    fn flush_dictionary_selector_translates_verbatim() {
        assert_eq!(
            flush_expr_for("#{ #file => \"src/foo.bt\" }"),
            Some("Workspace flush: #{ #file => \"src/foo.bt\" }".to_string())
        );
    }

    #[test]
    fn flush_trims_surrounding_whitespace() {
        assert_eq!(
            flush_expr_for("  Counter  "),
            Some("Workspace flush: Counter".to_string())
        );
    }

    #[test]
    fn flush_with_empty_selector_reports_no_expression() {
        assert_eq!(flush_expr_for(""), None);
    }

    #[test]
    fn flush_with_whitespace_only_selector_reports_no_expression() {
        // The historical-bug-class risk this guards against: a hand-copied
        // test-harness mirror of `Workspace flush: {selector}` that skips
        // this emptiness check (as the old `tests/repl_protocol.rs` inline
        // duplicate did) would blindly eval a malformed bare
        // `Workspace flush: ` send instead of hitting the usage-error path.
        // Because both the real dispatch and the test harness now call this
        // one function, that drift can no longer happen.
        assert_eq!(flush_expr_for("   "), None);
    }

    // -----------------------------------------------------------------------
    // remove_class_expr_for
    // -----------------------------------------------------------------------

    #[test]
    fn remove_class_translates_to_remove_from_system_send() {
        assert_eq!(
            remove_class_expr_for("Counter"),
            Some("Counter removeFromSystem".to_string())
        );
    }

    #[test]
    fn remove_class_trims_surrounding_whitespace() {
        assert_eq!(
            remove_class_expr_for("  Counter  "),
            Some("Counter removeFromSystem".to_string())
        );
    }

    #[test]
    fn remove_class_with_empty_argument_reports_no_expression() {
        assert_eq!(remove_class_expr_for(""), None);
        assert_eq!(remove_class_expr_for("   "), None);
    }

    // -----------------------------------------------------------------------
    // flush_destructive_expr_for
    // -----------------------------------------------------------------------

    #[test]
    fn flush_destructive_class_selector_appends_confirm_destructive_keyword() {
        assert_eq!(
            flush_destructive_expr_for("Counter"),
            Some("Workspace flush: Counter confirmDestructive: true".to_string())
        );
    }

    #[test]
    fn flush_destructive_symbol_kind_selector_translates_verbatim() {
        assert_eq!(
            flush_destructive_expr_for("#'remove-class'"),
            Some("Workspace flush: #'remove-class' confirmDestructive: true".to_string())
        );
    }

    #[test]
    fn flush_destructive_trims_surrounding_whitespace() {
        assert_eq!(
            flush_destructive_expr_for("  Counter  "),
            Some("Workspace flush: Counter confirmDestructive: true".to_string())
        );
    }

    #[test]
    fn flush_destructive_with_empty_selector_reports_no_expression() {
        assert_eq!(flush_destructive_expr_for(""), None);
        assert_eq!(flush_destructive_expr_for("   "), None);
    }

    #[test]
    fn flush_including_destructive_expr_is_the_bare_unscoped_selector() {
        assert_eq!(
            FLUSH_INCLUDING_DESTRUCTIVE_EXPR,
            "Workspace flushIncludingDestructive"
        );
    }

    // -----------------------------------------------------------------------
    // rename_class_expr_for
    // -----------------------------------------------------------------------

    #[test]
    fn rename_class_translates_to_rename_to_send() {
        assert_eq!(
            rename_class_expr_for("Counter Accumulator"),
            Some("Counter renameTo: #Accumulator".to_string())
        );
    }

    #[test]
    fn rename_class_strips_leading_hash_on_new_name() {
        assert_eq!(
            rename_class_expr_for("Counter #Accumulator"),
            Some("Counter renameTo: #Accumulator".to_string())
        );
    }

    #[test]
    fn rename_class_trims_surrounding_whitespace() {
        assert_eq!(
            rename_class_expr_for("  Counter   Accumulator  "),
            Some("Counter renameTo: #Accumulator".to_string())
        );
    }

    #[test]
    fn rename_class_with_missing_new_name_reports_no_expression() {
        assert_eq!(rename_class_expr_for("Counter"), None);
        assert_eq!(rename_class_expr_for("Counter "), None);
    }

    #[test]
    fn rename_class_with_empty_argument_reports_no_expression() {
        assert_eq!(rename_class_expr_for(""), None);
        assert_eq!(rename_class_expr_for("   "), None);
    }

    #[test]
    fn rename_class_with_extra_token_reports_no_expression() {
        assert_eq!(rename_class_expr_for("Counter Accumulator Extra"), None);
    }

    #[test]
    fn rename_class_with_bare_hash_new_name_reports_no_expression() {
        assert_eq!(rename_class_expr_for("Counter #"), None);
    }

    // -----------------------------------------------------------------------
    // rename_method_expr_for
    // -----------------------------------------------------------------------

    #[test]
    fn rename_method_translates_to_rename_selector_to_send() {
        assert_eq!(
            rename_method_expr_for("Counter increment incrementBy"),
            Some("Counter renameSelector: #increment to: #incrementBy".to_string())
        );
    }

    #[test]
    fn rename_method_preserves_keyword_selectors() {
        assert_eq!(
            rename_method_expr_for("Dict at:put: atKey:put:"),
            Some("Dict renameSelector: #at:put: to: #atKey:put:".to_string())
        );
    }

    #[test]
    fn rename_method_strips_leading_hash_on_both_selectors() {
        assert_eq!(
            rename_method_expr_for("Counter #increment #incrementBy"),
            Some("Counter renameSelector: #increment to: #incrementBy".to_string())
        );
    }

    #[test]
    fn rename_method_trims_surrounding_whitespace() {
        assert_eq!(
            rename_method_expr_for("  Counter   increment   incrementBy  "),
            Some("Counter renameSelector: #increment to: #incrementBy".to_string())
        );
    }

    #[test]
    fn rename_method_with_missing_tokens_reports_no_expression() {
        assert_eq!(rename_method_expr_for("Counter"), None);
        assert_eq!(rename_method_expr_for("Counter increment"), None);
        assert_eq!(rename_method_expr_for(""), None);
        assert_eq!(rename_method_expr_for("   "), None);
    }

    #[test]
    fn rename_method_with_extra_token_reports_no_expression() {
        assert_eq!(
            rename_method_expr_for("Counter increment incrementBy Extra"),
            None
        );
    }

    #[test]
    fn rename_method_with_bare_hash_selector_reports_no_expression() {
        assert_eq!(rename_method_expr_for("Counter # incrementBy"), None);
        assert_eq!(rename_method_expr_for("Counter increment #"), None);
    }
}
