// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3884: the analyser's view of a block literal sent a `value`-family
//! selector and codegen's lowering of it agree, over every such selector and
//! every block arity from 0 to 4, in class, value-type and actor methods.
//!
//! Codegen lowers the send one of three ways, and each has exactly one
//! analyser counterpart:
//!
//! - the body compiled in place: `local_threading_construct` names the send
//!   a `LocalThreadingFamily::BlockValue` construct;
//! - applied to a stand-in fun of the block's own arity (`apply _Arity`), the
//!   body never compiled: `block_literal_of_other_arity` (BT-3881), the one
//!   predicate the recognizer, codegen's lowering and the escape check read.
//!   It raises `arity_mismatch`, so the analyser reports no write for it, in
//!   any position;
//! - compiled as a closure (a selector codegen does not inline): the ADR 0133
//!   escaping-block check reports the block.

use super::codegen;
use beamtalk_core::ast::{Expression, WellKnownSelector};
use beamtalk_core::semantic_analysis::block_facts::{
    LocalThreadingFamily, block_literal_of_other_arity, local_threading_construct,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, lex_with_eof, parse};

/// The `value`-family selectors, by the number of arguments they send, and
/// `valueWithArguments:` (`None`: its arguments are one runtime list).
const SELECTORS: &[(&str, Option<usize>)] = &[
    ("value", Some(0)),
    ("value:", Some(1)),
    ("value:value:", Some(2)),
    ("value:value:value:", Some(3)),
    ("value:value:value:value:", Some(4)),
    ("valueWithArguments:", None),
];

const PARAMS: &[&str] = &["a", "b", "c", "d"];

/// The method kinds: the class header and method header of a class whose
/// method `m` holds the statements.
const KINDS: &[(&str, &str)] = &[
    ("class", "Object subclass: P\n  class m =>\n"),
    ("value type", "Value subclass: P\n  m =>\n"),
    ("actor", "Actor subclass: P\n  m =>\n"),
];

/// The block body's marker: the literal is compiled wherever the body is.
const MARKER: &str = "7919";

/// `[:a … | t := t + 7919] <selector> …`, a block of `arity` parameters that
/// writes the outer local `t`, sent `selector`.
fn send(selector: &str, args: Option<usize>, arity: usize) -> String {
    let params: Vec<String> = PARAMS[..arity].iter().map(|p| format!(":{p}")).collect();
    let block = if arity == 0 {
        format!("[t := t + {MARKER}]")
    } else {
        format!("[{} | t := t + {MARKER}]", params.join(" "))
    };
    match args {
        Some(0) => format!("{block} {selector}"),
        Some(n) => {
            let parts: Vec<String> = (1..=n).map(|i| format!("value: {i}")).collect();
            format!("{block} {}", parts.join(" "))
        }
        None => {
            let elems: Vec<String> = (1..=arity).map(|i| i.to_string()).collect();
            format!("{block} {selector} #({})", elems.join(", "))
        }
    }
}

fn parse_send(src: &str) -> Expression {
    let (module, diagnostics) = parse(lex_with_eof(src));
    assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
    module
        .expressions
        .into_iter()
        .next()
        .expect("one expression")
        .expression
}

/// How codegen lowered the block literal of the send in `code`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Lowering {
    /// The body compiled in place, with no `fun` for the block.
    Inlined,
    /// Applied to a stand-in fun of the block's arity; the body never compiled.
    Applied,
    /// The body compiled inside a `fun`: the block is a closure value.
    Closure,
}

fn lowering(code: &str) -> Lowering {
    let Some(marker) = code.find(MARKER) else {
        assert!(
            code.contains("apply _Arity"),
            "no body and no stand-in:\n{code}"
        );
        return Lowering::Applied;
    };
    let start = code[..marker]
        .rfind("let T = 0 in")
        .unwrap_or_else(|| panic!("no `t := 0` before the block body:\n{code}"));
    if code[start..marker].contains("fun (") {
        Lowering::Closure
    } else {
        Lowering::Inlined
    }
}

/// The analyser's counterpart of each [`Lowering`] for the send `src`: a
/// `BlockValue` construct, a block literal of another arity, or neither (a
/// block value).
fn analyser_view(src: &str) -> Lowering {
    let expr = parse_send(src);
    if local_threading_construct(&expr)
        .is_some_and(|construct| construct.family == LocalThreadingFamily::BlockValue)
    {
        return Lowering::Inlined;
    }
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = &expr
    else {
        panic!("{src}: not a send");
    };
    if block_literal_of_other_arity(receiver, selector, arguments.len()).is_some() {
        Lowering::Applied
    } else {
        Lowering::Closure
    }
}

fn is_block_value_selector(selector: &str) -> bool {
    WellKnownSelector::from_name(selector).is_some_and(WellKnownSelector::is_block_value)
}

/// The analyser's errors in `src` in `categories`.
fn errors_in(src: &str, categories: &[DiagnosticCategory]) -> Vec<String> {
    let (module, parse_diags) = parse(lex_with_eof(src));
    assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
    analyse_full(&module, AnalysisContext::default())
        .diagnostics
        .iter()
        .filter(|d| {
            d.severity == Severity::Error && d.category.is_some_and(|c| categories.contains(&c))
        })
        .map(|d| d.message.to_string())
        .collect()
}

#[test]
fn analyser_view_of_a_block_literal_send_agrees_with_codegen_lowering() {
    for &(selector, args) in SELECTORS {
        for arity in 0..=PARAMS.len() {
            let src = send(selector, args, arity);
            let view = analyser_view(&src);
            let expected = match (is_block_value_selector(selector), args == Some(arity)) {
                (true, true) => Lowering::Inlined,
                (true, false) => Lowering::Applied,
                (false, _) => Lowering::Closure,
            };
            assert_eq!(view, expected, "{src}: the analyser's view");
            for &(kind, header) in KINDS {
                let method = format!("{header}    t := 0\n    {src}\n    t\n");
                let code = codegen(&method);
                assert_eq!(
                    lowering(&code),
                    view,
                    "{kind}: {src}: codegen's lowering is the analyser's view\n{code}"
                );
                // The escape-check arm: the block is reported as escaping
                // exactly when codegen compiles it as a closure, never when it
                // is inlined or applied.
                let escapes = errors_in(&method, &[DiagnosticCategory::Tier2BlockNoReturnChannel]);
                assert_eq!(
                    !escapes.is_empty(),
                    view == Lowering::Closure,
                    "{kind}: {src}: escaping-block errors {escapes:?}"
                );
            }
        }
    }
}

/// The positions of a send in a method body: a statement, the value of an
/// assignment, a message receiver, a literal element and the last statement.
const POSITIONS: &[&str] = &[
    "t := 0\n    {s}\n    t\n",
    "t := 0\n    x := {s}\n    x printString\n    t\n",
    "t := 0\n    y := ({s}) printString\n    y size + t\n",
    "t := 0\n    z := #[{s}, t]\n    z size\n",
    "t := 0\n    {s}\n",
];

/// The messages of the analyser's errors about a local write in `src`: the
/// Phase 0 check and the ADR 0133 escaping-block check.
fn write_errors(src: &str) -> Vec<String> {
    errors_in(
        src,
        &[
            DiagnosticCategory::UnmigratedLocalThreading,
            DiagnosticCategory::Tier2BlockNoReturnChannel,
        ],
    )
}

#[test]
fn a_send_of_another_arity_reports_no_write_in_any_position() {
    for &(selector, args) in SELECTORS {
        if !is_block_value_selector(selector) {
            continue;
        }
        for arity in (0..=PARAMS.len()).filter(|&arity| args != Some(arity)) {
            let send = send(selector, args, arity);
            for &(kind, header) in KINDS {
                for position in POSITIONS {
                    let src = format!("{header}    {}", position.replace("{s}", &send));
                    let writes = write_errors(&src);
                    assert!(
                        writes.is_empty(),
                        "{kind}: {src}: a block of another arity never runs its body, \
                         so no write is reported: {writes:?}"
                    );
                }
            }
        }
    }
}

#[test]
fn a_parenthesized_block_of_another_arity_is_applied_like_a_bare_one() {
    // BT-3942: a parenthesized literal sent a `value`-family selector of
    // another arity is the same literal: applied to a stand-in fun, raising
    // `arity_mismatch`, with no write reported and no body compiled.
    for &(selector, args) in SELECTORS {
        if !is_block_value_selector(selector) {
            continue;
        }
        for arity in (0..=PARAMS.len()).filter(|&arity| args != Some(arity)) {
            let bare = send(selector, args, arity);
            let (block, rest) = bare.split_once("] ").expect("block then selector");
            let paren = format!("({block}]) {rest}");
            assert_eq!(analyser_view(&paren), Lowering::Applied, "{paren}");
            for &(kind, header) in KINDS {
                let src = format!("{header}    t := 0\n    {paren}\n    t\n");
                assert!(write_errors(&src).is_empty(), "{kind}: {src}");
                let code = codegen(&src);
                assert_eq!(
                    lowering(&code),
                    Lowering::Applied,
                    "{kind}: {paren}\n{code}"
                );
            }
        }
    }
}
