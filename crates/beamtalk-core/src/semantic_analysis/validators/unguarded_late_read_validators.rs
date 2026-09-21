// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Unguarded `late`-slot read reachable from `terminate:`/`handleInfo:`
//! (ADR 0124 §4d, Implementation B6).
//!
//! **DDD Context:** Semantic Analysis
//!
//! `terminate/2` wraps the `terminate:` dispatch in `try … catch -> 'ok'`
//! (`gen_server/callbacks.rs:1560`), and `handleInfo:` errors are logged and
//! the server continues (log-and-continue) — so an unguarded read of an
//! unassigned `late` slot in either raises `UninitializedStateError`, but
//! that error never surfaces as a crash: shutdown/message-handling
//! "completes" while whatever the read was for is silently skipped. This is
//! the real hazard in Symphony's `terminate: → stopProcess`
//! (`docs/beamtalk-language-features.md` § `late` Slots).
//!
//! [`check_unguarded_late_reads_in_lifecycle_hooks`] is a **per-class,
//! syntactic** walk — no dataflow, cheap by design (ADR 0124 §4d): for every
//! class with at least one `late` slot, it looks at that class's own
//! `terminate:`/`handleInfo:` method body, plus (one level only) the body of
//! any `self`-sent unary/keyword message that resolves to another of the
//! class's own instance methods, and reports every direct `self.slot` read
//! of a `late` slot that is not lexically inside the `ifTrue:` (true) arm of
//! `(self hasField: #slot) ifTrue: [...]` / `... ifTrue: [...] ifFalse:
//! [...]` for that same slot. A `self.slot := value` **assignment target**
//! is never a read — assigning is what makes the slot present, so it can
//! never raise `UninitializedStateError` — and is excluded even though
//! `self.slot :=` and `self.slot` share the same `FieldAccess` AST shape;
//! `value`, if it itself reads the slot (e.g. `self.proc := self.proc +
//! 1`), is a distinct node and stays a candidate.
//!
//! **Why only the `ifTrue:` arm counts as guarded.** `hasField:` answers
//! `true` exactly when the slot is present, so only the branch taken when it
//! answers `true` is a branch where a read is actually safe; the `ifFalse:`
//! arm of `ifTrue:ifFalse:` runs precisely when the slot is *absent*, so a
//! read there would still raise. A bare `(self hasField: #slot) ifFalse:
//! [...]` is therefore never treated as guarding a read inside its own
//! block; the common "guard clause" idiom (`ifFalse: [^nil]` followed by an
//! unguarded read later in the same sequence) is also not recognised — that
//! needs the reachability-through-an-early-return reasoning ADR 0124 §4d
//! explicitly scopes out ("no dataflow"), not the purely lexical
//! "does this read sit inside the guard's own block" check this module
//! does.
//!
//! **Category.** This is [`DiagnosticCategory::UnguardedLateRead`], a
//! sibling to [`DiagnosticCategory::DefiniteAssignment`] rather than reusing
//! it (ADR 0124 §4d, BT-3555 — see that variant's doc comment for the full
//! reasoning). Like `DefiniteAssignment`, it has no `@expect` category: the
//! guard is the fix, not a suppression.

use crate::ast::{
    ClassDefinition, Expression, ExpressionStatement, Literal, MethodDefinition, Module, SlotKind,
    WellKnownSelector,
};
use crate::ast_walker::walk_expression;
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::block_facts::is_self_reference;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;
use std::collections::BTreeSet;

/// The two lifecycle entry points ADR 0124 §4d names: both dispatch through
/// a wrapper that swallows a raised error rather than crashing the process.
const LIFECYCLE_SELECTORS: [&str; 2] = ["terminate:", "handleInfo:"];

/// Runs the ADR 0124 §4d unguarded-`late`-read check over every class in
/// `module` that declares (or inherits) at least one `late` slot.
pub(crate) fn check_unguarded_late_reads_in_lifecycle_hooks(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        let late_slots: BTreeSet<EcoString> = hierarchy
            .all_state(class_name)
            .into_iter()
            .filter(|field| hierarchy.state_field_kind(class_name, field) == SlotKind::Late)
            .collect();
        if late_slots.is_empty() {
            continue;
        }

        for method in &class.methods {
            if method.is_class_method {
                continue;
            }
            let selector_name = method.selector.name();
            if !LIFECYCLE_SELECTORS.contains(&selector_name.as_str()) {
                continue;
            }
            check_entry_method(method, &selector_name, &late_slots, class, diagnostics);
        }
    }
}

/// Checks one `terminate:`/`handleInfo:` method: its own body, plus (one
/// level only) the body of every distinct instance method of `class` that it
/// sends to `self`.
fn check_entry_method(
    entry_method: &MethodDefinition,
    entry_selector: &str,
    late_slots: &BTreeSet<EcoString>,
    class: &ClassDefinition,
    diagnostics: &mut Vec<Diagnostic>,
) {
    report_unguarded_reads(&entry_method.body, entry_selector, late_slots, diagnostics);

    let mut followed: BTreeSet<EcoString> = BTreeSet::new();
    for stmt in &entry_method.body {
        walk_expression(&stmt.expression, &mut |expr| {
            let Expression::MessageSend {
                receiver, selector, ..
            } = expr
            else {
                return;
            };
            if !is_self_reference(receiver) {
                return;
            }
            let callee_selector = selector.name();
            // One level only: don't re-enter the entry method itself (a
            // direct or indirect self-recursive call), and don't report the
            // same helper's body twice if the entry method sends to it more
            // than once.
            if callee_selector == *entry_selector || !followed.insert(callee_selector.clone()) {
                return;
            }
            if let Some(callee) = class
                .methods
                .iter()
                .find(|m| !m.is_class_method && m.selector.name() == callee_selector)
            {
                report_unguarded_reads(&callee.body, entry_selector, late_slots, diagnostics);
            }
        });
    }
}

/// Walks `body` (an entry method's or a followed helper's statements),
/// collecting every `self.slot` read of a `late` slot and every
/// `(self hasField: #slot) ifTrue: [...]` guard region, then reports every
/// read whose span isn't contained in a same-slot guard region.
fn report_unguarded_reads(
    body: &[ExpressionStatement],
    entry_selector: &str,
    late_slots: &BTreeSet<EcoString>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut guarded_regions: Vec<(EcoString, Span)> = Vec::new();
    let mut reads: Vec<(EcoString, Span)> = Vec::new();
    // Spans of `self.slot` nodes that are an assignment *target*
    // (`self.slot := value`) rather than a read — `walk_expression` visits
    // an `Assignment` node before recursing into its `target`, so by the
    // time the closure below reaches that same `FieldAccess` node (via the
    // generic recursion), its span is already recorded here and excluded
    // from `reads`. Writing never raises `UninitializedStateError` (it's
    // what makes the slot present), so it's never a hazard this check cares
    // about — only `value`, if it itself reads the slot (e.g. `self.proc :=
    // self.proc + 1`), stays a candidate, since that's a distinct node with
    // its own span.
    let mut write_target_spans: std::collections::HashSet<Span> = std::collections::HashSet::new();

    for stmt in body {
        walk_expression(&stmt.expression, &mut |expr| match expr {
            Expression::Assignment { target, .. } => {
                if let Expression::FieldAccess { receiver, span, .. } = target.as_ref() {
                    if is_self_reference(receiver) {
                        write_target_spans.insert(*span);
                    }
                }
            }
            Expression::FieldAccess {
                receiver,
                field,
                span,
            } => {
                if is_self_reference(receiver)
                    && late_slots.contains(&field.name)
                    && !write_target_spans.contains(span)
                {
                    reads.push((field.name.clone(), *span));
                }
            }
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                let selector_name = selector.name();
                if selector_name != "ifTrue:" && selector_name != "ifTrue:ifFalse:" {
                    return;
                }
                let Some(slot) = has_field_guard_slot(receiver, late_slots) else {
                    return;
                };
                if let Some(Expression::Block(true_block)) = arguments.first() {
                    guarded_regions.push((slot, true_block.span));
                }
            }
            _ => {}
        });
    }

    for (slot, span) in reads {
        let is_guarded = guarded_regions
            .iter()
            .any(|(guard_slot, guard_span)| *guard_slot == slot && guard_span.contains(span));
        if is_guarded {
            continue;
        }
        diagnostics.push(build_diagnostic(&slot, entry_selector, span));
    }
}

/// `true`-arm guard detector: if `receiver` (a conditional's own receiver,
/// stripped of any wrapping parentheses) is `self hasField: #slotName` for a
/// `slotName` in `late_slots`, returns that slot's name.
fn has_field_guard_slot(
    receiver: &Expression,
    late_slots: &BTreeSet<EcoString>,
) -> Option<EcoString> {
    let Expression::MessageSend {
        receiver: inner_receiver,
        selector,
        arguments,
        ..
    } = strip_parens(receiver)
    else {
        return None;
    };
    if selector.well_known() != Some(WellKnownSelector::HasField) {
        return None;
    }
    if !is_self_reference(inner_receiver) {
        return None;
    }
    let Some(Expression::Literal(Literal::Symbol(name), _)) = arguments.first() else {
        return None;
    };
    late_slots.contains(name).then(|| name.clone())
}

/// Unwraps any number of `Parenthesized` wrappers, e.g. the `(...)` around
/// `(self hasField: #proc) ifTrue: [...]`'s receiver.
fn strip_parens(expr: &Expression) -> &Expression {
    match expr {
        Expression::Parenthesized { expression, .. } => strip_parens(expression),
        other => other,
    }
}

fn build_diagnostic(slot: &str, entry_selector: &str, span: Span) -> Diagnostic {
    let message = format!(
        "Unguarded read of `late` slot `{slot}` reachable from `{entry_selector}` — if `{slot}` \
         is unassigned this raises `UninitializedStateError`, but `{entry_selector}`'s dispatch \
         swallows the error instead of crashing, so it silently skips whatever this read was for"
    );
    let hint = format!("Guard the read: `(self hasField: #{slot}) ifTrue: [ ... ]`");
    Diagnostic::warning(message, span)
        .with_hint(hint)
        .with_category(DiagnosticCategory::UnguardedLateRead)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::lex_with_eof;
    use crate::source_analysis::parse;

    fn diagnostics_for(src: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_unguarded_late_reads_in_lifecycle_hooks(&module, &hierarchy, &mut diagnostics);
        diagnostics
    }

    fn unguarded_findings(src: &str) -> Vec<Diagnostic> {
        diagnostics_for(src)
            .into_iter()
            .filter(|d| d.category == Some(DiagnosticCategory::UnguardedLateRead))
            .collect()
    }

    // ── unguarded → warns ─────────────────────────────────────────────────

    #[test]
    fn unguarded_read_in_terminate_warns() {
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\n\
                    terminate: reason => self.proc close";
        let diags = unguarded_findings(src);
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert!(diags[0].message.contains("`proc`"));
        assert!(diags[0].message.contains("terminate:"));
        assert!(
            diags[0]
                .hint
                .as_deref()
                .unwrap_or("")
                .contains("hasField: #proc")
        );
    }

    #[test]
    fn unguarded_read_in_handle_info_warns() {
        let src = "typed Server subclass: Watcher\n\
                    late state: sock :: Socket\n\n\
                    handleInfo: msg => self.sock close";
        let diags = unguarded_findings(src);
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert!(diags[0].message.contains("`sock`"));
        assert!(diags[0].message.contains("handleInfo:"));
    }

    // ── guarded → silent ─────────────────────────────────────────────────

    #[test]
    fn guarded_read_inside_if_true_is_silent() {
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\n\
                    terminate: reason =>\n  \
                      (self hasField: #proc)\n    \
                        ifTrue: [self.proc close]\n  \
                      nil";
        assert!(unguarded_findings(src).is_empty());
    }

    #[test]
    fn guarded_read_inside_if_true_if_false_true_arm_is_silent() {
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\n\
                    terminate: reason =>\n  \
                      (self hasField: #proc)\n    \
                        ifTrue: [self.proc close]\n    \
                        ifFalse: [nil]\n  \
                      nil";
        assert!(unguarded_findings(src).is_empty());
    }

    // ── guarded inside the helper → silent ──────────────────────────────

    #[test]
    fn unguarded_read_reachable_through_one_level_self_send_warns() {
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\n\
                    terminate: reason => self stopProcess\n\n\
                    stopProcess => self.proc close";
        let diags = unguarded_findings(src);
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        // The reported entry point is still `terminate:` — the read is
        // reachable from it, even though it textually lives in `stopProcess`.
        assert!(diags[0].message.contains("terminate:"));
    }

    #[test]
    fn guarded_read_inside_helper_reached_via_self_send_is_silent() {
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\n\
                    terminate: reason => self stopProcess\n\n\
                    stopProcess =>\n  \
                      (self hasField: #proc)\n    \
                        ifTrue: [self.proc close]\n  \
                      nil";
        assert!(unguarded_findings(src).is_empty());
    }

    // ── eager slot → silent ──────────────────────────────────────────────

    #[test]
    fn eager_slot_read_in_terminate_is_silent() {
        let src = "typed Actor subclass: Logger\n\
                    state: logFile :: LogHandle\n\n\
                    terminate: reason => self.logFile close";
        assert!(unguarded_findings(src).is_empty());
    }

    // ── other shapes ─────────────────────────────────────────────────────

    #[test]
    fn ordinary_method_other_than_lifecycle_hooks_is_not_checked() {
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\n\
                    sendLine: line => self.proc write: line";
        assert!(unguarded_findings(src).is_empty());
    }

    #[test]
    fn bare_if_false_guard_does_not_suppress_its_own_block() {
        // `ifFalse:`'s block runs precisely when the slot is absent, so a
        // read inside it is never guarded.
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\n\
                    terminate: reason =>\n  \
                      (self hasField: #proc)\n    \
                        ifFalse: [self.proc close]\n  \
                      nil";
        let diags = unguarded_findings(src);
        assert_eq!(
            diags.len(),
            1,
            "a read inside a bare ifFalse: block must still be reported: {diags:?}"
        );
    }

    #[test]
    fn guard_on_a_different_slot_does_not_suppress_this_one() {
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\
                    late state: sock :: Socket\n\n\
                    terminate: reason =>\n  \
                      (self hasField: #sock)\n    \
                        ifTrue: [self.sock close]\n  \
                      self.proc close";
        let diags = unguarded_findings(src);
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert!(diags[0].message.contains("`proc`"));
    }

    #[test]
    fn assignment_target_is_not_treated_as_a_read() {
        // `self.proc := 1` assigns (and so marks the slot present) — it
        // never raises `UninitializedStateError`, so it's not the hazard
        // this check warns about, unlike a genuine read of the slot.
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Integer\n\n\
                    terminate: reason => self.proc := 1";
        let diags = unguarded_findings(src);
        assert!(
            diags.is_empty(),
            "an assignment target must not be reported as a read: {diags:?}"
        );
    }

    #[test]
    fn assignment_value_that_reads_the_same_slot_is_still_reported() {
        // `self.proc := self.proc + 1` — the left-hand `self.proc` is a
        // write target (not reported), but the right-hand `self.proc`
        // inside the value expression is a genuine, distinct read and must
        // still be reported when unguarded.
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Integer\n\n\
                    terminate: reason => self.proc := self.proc + 1";
        let diags = unguarded_findings(src);
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }

    #[test]
    fn read_after_the_guard_block_is_still_unguarded() {
        // No dataflow: the guard only covers reads lexically inside its own
        // block, not later statements in the same sequence.
        let src = "typed Actor subclass: CodexClient\n\
                    late state: proc :: Subprocess\n\n\
                    terminate: reason =>\n  \
                      (self hasField: #proc)\n    \
                        ifTrue: [nil]\n  \
                      self.proc close";
        let diags = unguarded_findings(src);
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }
}
