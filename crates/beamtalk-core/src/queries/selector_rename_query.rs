// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Selector-rename span queries — exact byte spans for `renameSelector:to:`.
//!
//! **DDD Context:** Language Service / Compilation (shared leaf, per
//! `docs/development/architecture-principles.md` §6)
//!
//! Backs `Behaviour>>renameSelector:to:` (ADR 0114, BT-3279). Given a
//! specific `self`/`super` message send (or a method's own definition), a
//! whole-method span (`resolve_method_span`) is too coarse to splice — it
//! would let a caller corrupt the rest of the method body — and a plain
//! text/regex search is unsafe for a multi-keyword selector like `at:put:`
//! (arguments between keyword parts can contain arbitrary nested
//! expressions, including sends that coincidentally reuse the same keyword
//! text). These two queries resolve the exact selector-token span(s) via an
//! AST walk instead.
//!
//! # Implementation
//!
//! The implementation lives in [`crate::method_source_walker`], the same
//! shared leaf module [`super::all_sends_query`] re-exports from — see that
//! module's own doc for why this lives below both consuming contexts.

pub use crate::method_source_walker::{
    SelectorSendSpan, find_definition_selector_spans, find_selector_send_spans,
};
