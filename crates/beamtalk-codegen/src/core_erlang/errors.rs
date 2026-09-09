// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared `#beamtalk_error{}` construction chain.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Shared leaf module (`architecture-principles.md` § Duplication & the
//! Shared-Leaf-Module Pattern): the `beamtalk_error:'new'` →
//! `'with_selector'` → `'with_hint'` three-step chain is the one way every
//! structured runtime error (DNU, arity/type mismatch, instantiation error,
//! immutable-value write, stateful-block-dispatch guard, ...) gets built —
//! see CLAUDE.md § Structured errors. Before this module existed, each call
//! site hand-wrote the same three `let`s, in whatever `Document`-building
//! idiom (plain literal strings, or `nest`/`line` pretty-printing) its own
//! function already used.

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// Builds the `let Err0 = call 'beamtalk_error':'new'(Kind, Class) in <sep>
/// let Err1 = call 'beamtalk_error':'with_selector'(Err0, Selector) in <sep>
/// let Err2 = call 'beamtalk_error':'with_hint'(Err1, Hint) in <sep>` chain
/// shared by every structured runtime error emission site, stopping right
/// after the final `in` + `sep` so the caller appends its own tail (`call
/// 'beamtalk_error':'raise'(Err2)`, `{'error', Err2, State}`, ...)
/// referencing `err2` itself.
///
/// `err0`/`err1`/`err2` are each call site's own established `let`-binding
/// text — a fixed name (`Document::Str("Error0")`) or a freshly minted one
/// (`leaf::var(self.fresh_var("Err"))`), bracketed or not, however that site
/// already spelled it — so collapsing onto this builder stays a
/// byte-identical extraction rather than a renaming. `err0_ref`/`err1_ref`
/// are the corresponding bare variable-reference text (identical to
/// `err0`/`err1` at every call site that does not use Core Erlang's
/// bracketed single-variable `let` form). `sep` is whatever separates one
/// `let ... in` step from the next at the call site: a literal `" "` for an
/// inline chain, `"\n"` plus the site's own indent for a manually indented
/// one, or [`beamtalk_cerl_doc::line`] for one built with `nest`/`line`.
#[allow(clippy::too_many_arguments)] // one param per varying piece of a fixed 3-step chain
pub(super) fn beamtalk_error_doc(
    err0: Document<'static>,
    err0_ref: Document<'static>,
    err1: Document<'static>,
    err1_ref: Document<'static>,
    err2: Document<'static>,
    kind: &'static str,
    class: Document<'static>,
    selector: Document<'static>,
    hint: Document<'static>,
    sep: Document<'static>,
) -> Document<'static> {
    docvec![
        "let ",
        err0,
        " = call 'beamtalk_error':'new'(",
        leaf::atom(kind),
        ", ",
        class,
        ") in",
        sep.clone(),
        "let ",
        err1,
        " = call 'beamtalk_error':'with_selector'(",
        err0_ref,
        ", ",
        selector,
        ") in",
        sep.clone(),
        "let ",
        err2,
        " = call 'beamtalk_error':'with_hint'(",
        err1_ref,
        ", ",
        hint,
        ") in",
        sep,
    ]
}
