// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Structural Core Erlang constructors: `let`, `case`, `try`, `call`,
//! `primop` and `do` (BT-4685).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! [`leaf`](crate::leaf) types the *leaves* of the output (atoms, variables,
//! literals). This module types its *structure*: the handful of Core Erlang
//! forms that codegen otherwise writes as `"let "`, `" in "`, `"case "`,
//! `" of "`, `"catch <"`, `"call "` and `"primop "` string literals inside
//! `docvec!`.
//!
//! # The rule for new code
//!
//! **Structure goes through these constructors, not string literals.** A new
//! `let`, `case`, `try`, `do`, `call` or `primop` is built with
//! [`let_in`], [`case_of`] / [`clause`], [`try_of_catch`], [`seq`], [`call`]
//! or [`primop`]; never with a `"let "` / `" in "` / `"catch <"` fragment.
//! Existing literal sites move to the constructors as their files are
//! touched. This is a typed-*document* API, not a Core Erlang AST (ADR 0089):
//! operands are ordinary [`Document`]s.
//!
//! # Layout
//!
//! Each constructor owns its own layout and takes a [`Layout`]:
//!
//! * [`Layout::Flat`] puts the whole form on one line, single-space
//!   separated. This is exactly the text the historical string literals
//!   produced, so a flat migration is byte-identical.
//! * [`Layout::Block`] puts each part on its own line, indented by
//!   [`INDENT`](crate::INDENT) through `nest` / `line`, so the surrounding
//!   `nest` decides the indentation and no caller hard-codes spaces.
//!
//! [`call`] and [`primop`] have no layout choice: they are always one line.
//!
//! ```
//! use beamtalk_cerl_doc::form::{self, Layout};
//! use beamtalk_cerl_doc::leaf;
//!
//! let doc = form::let_in(
//!     leaf::var("X"),
//!     form::call("erlang", "self", []),
//!     leaf::var("X"),
//!     Layout::Flat,
//! );
//! assert_eq!(doc.to_pretty_string(), "let X = call 'erlang':'self'() in X");
//! ```

use crate::{Document, INDENT, docvec, leaf, line, nest};

/// How a structural form lays out its parts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Layout {
    /// Everything on one line, single-space separated.
    Flat,
    /// One part per line, nested with `nest` / `line`.
    Block,
}

/// `item, item, item` — comma-space separated, empty for no items.
fn comma_separated<'a>(items: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    crate::join(items, &Document::Str(", "))
}

/// `<A, B>` — the angle-bracketed tuple of patterns / variables that opens a
/// clause, an `of` branch or a `catch` branch.
fn angle<'a>(items: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    docvec!["<", comma_separated(items), ">"]
}

/// `call 'module':'function'(arg, …)`.
///
/// Module and function are atom names; arguments are already-built documents.
/// Use [`call_atoms`] when the module / function atoms are already documents.
#[must_use]
pub fn call<'a>(
    module: impl Into<String>,
    function: impl Into<String>,
    args: impl IntoIterator<Item = Document<'a>>,
) -> Document<'a> {
    call_atoms(leaf::atom(module), leaf::atom(function), args)
}

/// [`call`] for a module and function that are already atom documents
/// ([`leaf::atom`]), e.g. an FFI target resolved earlier in codegen.
#[must_use]
pub fn call_atoms<'a>(
    module: Document<'a>,
    function: Document<'a>,
    args: impl IntoIterator<Item = Document<'a>>,
) -> Document<'a> {
    docvec![
        "call ",
        module,
        ":",
        function,
        "(",
        comma_separated(args),
        ")",
    ]
}

/// `primop 'name'(arg, …)`.
#[must_use]
pub fn primop<'a>(
    name: impl Into<String>,
    args: impl IntoIterator<Item = Document<'a>>,
) -> Document<'a> {
    docvec!["primop ", leaf::atom(name), "(", comma_separated(args), ")",]
}

/// `let var = value in body`.
///
/// `var` is a variable document ([`leaf::var`]). `Flat` renders
/// `let V = value in body`; `Block` renders `let V = value in`, a line break,
/// then `body` at the same indentation (a chain of lets reads as a column).
#[must_use]
pub fn let_in<'a>(
    var: Document<'a>,
    value: Document<'a>,
    body: Document<'a>,
    layout: Layout,
) -> Document<'a> {
    let sep = match layout {
        Layout::Flat => Document::Str(" "),
        Layout::Block => line(),
    };
    docvec!["let ", var, " = ", value, " in", sep, body]
}

/// `do first second` — evaluate `first` for effect, then `second`.
#[must_use]
pub fn seq<'a>(first: Document<'a>, second: Document<'a>, layout: Layout) -> Document<'a> {
    let sep = match layout {
        Layout::Flat => Document::Str(" "),
        Layout::Block => line(),
    };
    docvec!["do ", first, sep, second]
}

/// One `<patterns> when guard -> body` clause of a [`case_of`].
///
/// `guard` of `None` is the always-true guard `'true'`. `Flat` renders
/// `<P> when G -> body`; `Block` renders `<P> when G ->` and nests `body` on
/// the next line.
#[must_use]
pub fn clause<'a>(
    patterns: impl IntoIterator<Item = Document<'a>>,
    guard: Option<Document<'a>>,
    body: Document<'a>,
    layout: Layout,
) -> Document<'a> {
    clause_with_head(angle(patterns), guard, body, layout)
}

/// [`clause`] for a single pattern written without the angle brackets:
/// `'true' when 'true' -> body`.
///
/// Core Erlang accepts both spellings; some historical emission sites use the
/// bare one, and migrating them with [`clause`] would change the generated
/// text. Prefer [`clause`] in new code.
#[must_use]
pub fn clause_bare<'a>(
    pattern: Document<'a>,
    guard: Option<Document<'a>>,
    body: Document<'a>,
    layout: Layout,
) -> Document<'a> {
    clause_with_head(pattern, guard, body, layout)
}

fn clause_with_head<'a>(
    pattern_head: Document<'a>,
    guard: Option<Document<'a>>,
    body: Document<'a>,
    layout: Layout,
) -> Document<'a> {
    let head = docvec![
        pattern_head,
        " when ",
        guard.unwrap_or_else(|| leaf::atom("true")),
        " ->",
    ];
    match layout {
        Layout::Flat => docvec![head, " ", body],
        Layout::Block => docvec![head, nest(INDENT, docvec![line(), body])],
    }
}

/// `case scrutinee of clause … end`.
///
/// Build each clause with [`clause`] using the same `layout`. `Flat` joins the
/// clauses with single spaces; `Block` nests them one per line between
/// `of` and `end`.
#[must_use]
pub fn case_of<'a>(
    scrutinee: Document<'a>,
    clauses: impl IntoIterator<Item = Document<'a>>,
    layout: Layout,
) -> Document<'a> {
    match layout {
        Layout::Flat => docvec![
            "case ",
            scrutinee,
            " of ",
            crate::join(clauses, &Document::Str(" ")),
            " end",
        ],
        Layout::Block => docvec![
            "case ",
            scrutinee,
            " of",
            nest(INDENT, docvec![line(), crate::join(clauses, &line())],),
            line(),
            "end",
        ],
    }
}

/// `try expr of <ok_vars> -> ok_body catch <class, reason, stack> -> handler`.
///
/// Core Erlang's `try` has no closing `end`. `Flat` renders the whole form on
/// one line; `Block` puts `try`, `of` and `catch` on their own lines with each
/// body nested beneath.
#[must_use]
pub fn try_of_catch<'a>(
    expr: Document<'a>,
    ok_vars: impl IntoIterator<Item = Document<'a>>,
    ok_body: Document<'a>,
    catch_vars: [Document<'a>; 3],
    handler: Document<'a>,
    layout: Layout,
) -> Document<'a> {
    let ok_head = docvec!["of ", angle(ok_vars), " ->"];
    let catch_head = docvec!["catch ", angle(catch_vars), " ->"];
    match layout {
        Layout::Flat => docvec![
            "try ", expr, " ", ok_head, " ", ok_body, " ", catch_head, " ", handler,
        ],
        Layout::Block => docvec![
            "try",
            nest(INDENT, docvec![line(), expr]),
            line(),
            ok_head,
            nest(INDENT, docvec![line(), ok_body]),
            line(),
            catch_head,
            nest(INDENT, docvec![line(), handler]),
        ],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(n: &str) -> Document<'static> {
        leaf::var(n)
    }

    #[test]
    fn call_and_primop_are_single_line() {
        assert_eq!(
            call("m", "f", [v("A"), v("B")]).to_pretty_string(),
            "call 'm':'f'(A, B)"
        );
        assert_eq!(call("m", "f", []).to_pretty_string(), "call 'm':'f'()");
        assert_eq!(
            primop("raw_raise", [v("T"), v("E"), v("S")]).to_pretty_string(),
            "primop 'raw_raise'(T, E, S)"
        );
    }

    #[test]
    fn let_in_layouts() {
        let flat = let_in(v("X"), v("Y"), v("X"), Layout::Flat);
        assert_eq!(flat.to_pretty_string(), "let X = Y in X");
        let block = nest(
            4,
            docvec![line(), let_in(v("X"), v("Y"), v("X"), Layout::Block)],
        );
        assert_eq!(block.to_pretty_string(), "\n    let X = Y in\n    X");
    }

    #[test]
    fn seq_layouts() {
        assert_eq!(
            seq(v("A"), v("B"), Layout::Flat).to_pretty_string(),
            "do A B"
        );
        assert_eq!(
            seq(v("A"), v("B"), Layout::Block).to_pretty_string(),
            "do A\nB"
        );
    }

    #[test]
    fn case_flat_matches_historical_literal() {
        let doc = case_of(
            v("M"),
            [
                clause([leaf::atom("true")], None, v("A"), Layout::Flat),
                clause(
                    [leaf::atom("false")],
                    Some(leaf::atom("true")),
                    v("B"),
                    Layout::Flat,
                ),
            ],
            Layout::Flat,
        );
        assert_eq!(
            doc.to_pretty_string(),
            "case M of <'true'> when 'true' -> A <'false'> when 'true' -> B end"
        );
    }

    #[test]
    fn case_block_nests_clauses() {
        let doc = case_of(
            v("M"),
            [
                clause([leaf::atom("a")], None, v("A"), Layout::Block),
                clause([leaf::atom("b")], Some(v("G")), v("B"), Layout::Block),
            ],
            Layout::Block,
        );
        assert_eq!(
            doc.to_pretty_string(),
            "case M of\n    <'a'> when 'true' ->\n        A\n    <'b'> when G ->\n        B\nend"
        );
    }

    #[test]
    fn bare_clause_has_no_angle_brackets() {
        let doc = clause_bare(leaf::atom("true"), None, v("A"), Layout::Flat);
        assert_eq!(doc.to_pretty_string(), "'true' when 'true' -> A");
    }

    #[test]
    fn multi_pattern_clause() {
        let doc = clause([v("A"), v("B")], None, v("C"), Layout::Flat);
        assert_eq!(doc.to_pretty_string(), "<A, B> when 'true' -> C");
    }

    #[test]
    fn try_layouts() {
        let mk = |layout| {
            try_of_catch(
                v("E"),
                [v("R")],
                v("R"),
                [v("C"), v("X"), v("S")],
                v("H"),
                layout,
            )
        };
        assert_eq!(
            mk(Layout::Flat).to_pretty_string(),
            "try E of <R> -> R catch <C, X, S> -> H"
        );
        assert_eq!(
            mk(Layout::Block).to_pretty_string(),
            "try\n    E\nof <R> ->\n    R\ncatch <C, X, S> ->\n    H"
        );
    }
}
