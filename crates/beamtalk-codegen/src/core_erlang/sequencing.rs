// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Sub-expression sequencing primitives (ADR 0118).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Shared leaf module (see `architecture-principles.md` § Duplication &
//! the Shared-Leaf-Module Pattern): the "compile a group of sub-expressions,
//! hoisting into a `let`-prelude whichever ones need to run ahead of a
//! not-yet-emitted call, then splice that prelude back around the finished
//! call `Document`" idiom used throughout dispatch, operator, and expression
//! codegen. None of it is dispatch-specific — it previously lived in
//! `dispatch_codegen.rs` and `mod.rs` only because those were the first
//! callers, which is exactly the "sits below in the dependency graph" case
//! `architecture-principles.md` says is not a reason to duplicate a rule
//! rather than extract it.
//!
//! Also holds the ADR 0118 phase 1a "precompiled sub-expression" substitution
//! mechanism ([`PrecompiledSubexpr`]/[`PrecompiledScope`]/
//! [`CoreErlangGenerator::precompiled_subexprs`]): the `HashMap` a sequencing
//! pass registers an already-compiled child's value into, so the enclosing
//! parent's ordinary AST-directed compile substitutes that value instead of
//! compiling (and, for a self-send, dispatching) the child a second time.

use super::threaded_ir::ThreadedStmt;
use super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

/// One entry of [`CoreErlangGenerator::precompiled_subexprs`] — see that
/// field's doc comment.
pub(super) struct PrecompiledSubexpr {
    /// The already-compiled value to substitute for the node.
    doc: Document<'static>,
    /// Whether a hit should wrap `doc` in the source-line
    /// annotation `generate_expression` gives every closed message send —
    /// `true` only for a producer's own result reference (which never
    /// went through `generate_expression`), so a sequenced self-send
    /// renders byte-identically to the planner's substitution; `false` for
    /// a sequencing temp or a value `generate_expression` already built.
    ///
    /// INVARIANT: an `annotate: true` doc must be a *closed* expression —
    /// `( doc -| [line] )` around an open `let … in ` chain is invalid Core
    /// Erlang (the hazard the `MessageSend` arm's open-scope guard exists
    /// for). Today the only `true` registration is
    /// `CoreErlangGenerator::self_dispatch_result_value`'s fixed
    /// `call 'erlang':'element'(1, _SD)` shape; `take_precompiled_subexpr`
    /// additionally re-applies that arm's guard as defence in depth.
    annotate: bool,
    /// Set on the first hit; a never-hit entry is an invariant violation
    /// reported by `finish_precompiled_scope`.
    used: bool,
}

/// The set of [`CoreErlangGenerator::precompiled_subexprs`] entries one
/// sequencing pass registered — returned by the pass, handed back to
/// [`CoreErlangGenerator::finish_precompiled_scope`] once the parent has
/// been compiled. `#[must_use]`: dropping it leaks entries into the next
/// statement and skips the consulted-exactly check.
#[must_use = "hand this back to finish_precompiled_scope once the parent is compiled"]
pub(super) struct PrecompiledScope(Vec<Span>);

impl PrecompiledScope {
    pub(super) fn new() -> Self {
        Self(Vec::new())
    }
}

/// Collapses the repeated `thread_subexprs` → `docs.remove(0)` →
/// `close_prelude` idiom into one owned value.
///
/// [`CoreErlangGenerator::sequence_call`] threads a list of sub-expressions
/// up front; [`Self::next`] hands back each threaded document in the same
/// left-to-right order the exprs were given in; [`Self::close`] splices the
/// accumulated prelude around the finished `call_doc`. The prelude/docs are
/// owned here rather than borrowed from the generator, so a caller is free
/// to make further `&mut self` calls on the generator between `next()` and
/// `close()` while it assembles `call_doc`.
#[must_use = "call .close() to splice the threaded prelude around the finished call_doc"]
pub(super) struct SequencedCall {
    prelude: Vec<ThreadedStmt>,
    docs: std::collections::VecDeque<Document<'static>>,
}

impl SequencedCall {
    /// Returns the next threaded document, in the same order the exprs were
    /// given to [`CoreErlangGenerator::sequence_call`].
    ///
    /// # Panics
    ///
    /// Panics if called more times than there were sequenced expressions —
    /// every call site knows exactly how many it threaded.
    pub(super) fn next(&mut self) -> Document<'static> {
        self.docs
            .pop_front()
            .expect("SequencedCall::next called more times than exprs were threaded")
    }

    /// Drains every remaining threaded document, in order — for a call site
    /// that names only its first few operands (e.g. the receiver) via
    /// [`Self::next`] and passes the rest through
    /// [`CoreErlangGenerator::join_docs_with_commas`] as a comma-separated
    /// argument list.
    pub(super) fn rest(&mut self) -> Vec<Document<'static>> {
        self.docs.drain(..).collect()
    }

    /// Splices the threaded prelude around `call_doc` — see
    /// [`CoreErlangGenerator::close_prelude`].
    pub(super) fn close(
        self,
        generator: &mut CoreErlangGenerator,
        call_doc: Document<'static>,
        result_prefix: &str,
    ) -> Document<'static> {
        generator.close_prelude(&self.prelude, call_doc, result_prefix)
    }

    /// Escape hatch for a caller that splices the prelude itself instead of
    /// closing it around one finished `call_doc` — e.g. appending it (via
    /// [`CoreErlangGenerator::threaded_prelude_doc`]) to a shared `parts`
    /// accumulator, or carrying it alongside the doc in a `(prelude, doc)`
    /// pair for a later all-or-nothing hoisting decision. Consumes `self`
    /// since the prelude is only meaningful together with the docs
    /// [`Self::next`]/[`Self::rest`] already handed out.
    pub(super) fn into_prelude(self) -> Vec<ThreadedStmt> {
        self.prelude
    }
}

impl CoreErlangGenerator {
    /// ADR 0118 phase 1a: records `expr`'s already-sequenced
    /// value so the enclosing parent's ordinary compile substitutes it —
    /// see [`Self::precompiled_subexprs`]. Keyed by the paren-unwrapped
    /// span: `generate_expression`'s `Parenthesized` arm recurses, and
    /// every `unwrap_parens()`-first path reaches the inner node, so the
    /// inner span is the one every route converges on.
    ///
    /// `annotate` may be `true` only for a closed expression document —
    /// see [`PrecompiledSubexpr::annotate`]'s invariant.
    ///
    /// # Errors
    ///
    /// Two live scopes registering the same node would let the inner
    /// `finish_precompiled_scope` remove the entry out from under the
    /// outer one, whose consulted-exactly check would then pass vacuously
    /// while the parent compiled the child afresh — a double dispatch with
    /// no error. A duplicate registration is therefore a hard
    /// [`CodeGenError::Internal`](super::CodeGenError::Internal) in every
    /// build profile (a diagnostic in `codegen_warnings` would be discarded
    /// by the CLI's build path).
    pub(super) fn register_precompiled_subexpr(
        &mut self,
        scope: &mut PrecompiledScope,
        expr: &Expression,
        doc: Document<'static>,
        annotate: bool,
    ) -> Result<()> {
        let span = expr.unwrap_parens().span();
        if self.precompiled_subexprs.contains_key(&span) {
            return Err(super::CodeGenError::Internal(format!(
                "ADR 0118 sequencing: sub-expression at {span:?} registered twice"
            )));
        }
        self.precompiled_subexprs.insert(
            span,
            PrecompiledSubexpr {
                doc,
                annotate,
                used: false,
            },
        );
        scope.0.push(span);
        Ok(())
    }

    /// ADR 0118 phase 5b: `true` if `expr` (any nesting of
    /// parens) was already registered by an enclosing `sequence_children`
    /// call — a pure, non-consuming check for a caller deciding whether to
    /// re-thread `expr` itself (wrong: double-dispatch) or read the
    /// substitution back via the ordinary `expression_doc`/
    /// `take_precompiled_subexpr` path.
    pub(super) fn precompiled_subexprs_contains(&self, expr: &Expression) -> bool {
        self.precompiled_subexprs
            .contains_key(&expr.unwrap_parens().span())
    }

    /// The `generate_expression` entry hook for
    /// [`Self::precompiled_subexprs`]: `Some(doc)` if `expr` was
    /// pre-sequenced, marking the entry consulted.
    pub(super) fn take_precompiled_subexpr(
        &mut self,
        expr: &Expression,
    ) -> Option<Document<'static>> {
        if self.precompiled_subexprs.is_empty() {
            return None;
        }
        let span = expr.span();
        let (doc, annotate) = {
            let entry = self.precompiled_subexprs.get_mut(&span)?;
            entry.used = true;
            (entry.doc.clone(), entry.annotate)
        };
        // Never annotate while an open let-chain is in flight — an
        // annotated open chain is invalid Core Erlang. Defence in
        // depth over the closed-doc invariant on `PrecompiledSubexpr::annotate`.
        if annotate && self.can_annotate_closed_expression() {
            if let Some(line_num) = self.span_to_line(span) {
                return Some(self.annotate_with_line(doc, line_num));
            }
        }
        Some(doc)
    }

    /// Whether the expression just produced may be wrapped in a
    /// source-line annotation — only a CLOSED expression can be; an open
    /// let-chain (a class-method send, a class-var assignment, a
    /// direct-params list op) ends in a dangling `in ` that `( expr -|
    /// [annotation] )` would break. The single predicate behind
    /// `generate_expression`'s `MessageSend` arm and
    /// [`Self::take_precompiled_subexpr`], so a new open-scope side channel
    /// only has to be added here.
    pub(super) fn can_annotate_closed_expression(&self) -> bool {
        !self.loop_mode.direct_params_do_open_chain
            && self.loop_mode.direct_params_list_op_result.is_none()
    }

    /// Removes every entry `scope` registered, once the parent compile
    /// that was meant to consult them is done. An entry that was never
    /// consulted means that compile bypassed `generate_expression` for the
    /// child — its prelude already ran (or its temp is already bound) but
    /// the parent compiled the child afresh, so a state-effecting child
    /// would dispatch twice: an internal error, never a silent drop.
    pub(super) fn finish_precompiled_scope(&mut self, scope: PrecompiledScope) -> Result<()> {
        let mut unused = Vec::new();
        for span in scope.0 {
            if let Some(entry) = self.precompiled_subexprs.remove(&span) {
                if !entry.used {
                    unused.push(span);
                }
            }
        }
        if let Some(span) = unused.first() {
            return Err(super::CodeGenError::Internal(format!(
                "ADR 0118 sequencing: a pre-sequenced sub-expression at {span:?} was never \
                 substituted by its parent's compile (the parent's codegen path bypasses \
                 generate_expression for that child)"
            )));
        }
        Ok(())
    }

    /// Generates a comma-separated argument list for function/message calls.
    ///
    /// This is a shared helper that eliminates the repeated pattern of iterating
    /// over arguments with comma separation found throughout dispatch codegen.
    /// Captures a comma-separated argument list as a `Document` (ADR 0018 bridge).
    ///
    /// ADR 0118 phase 5b: each argument is compiled via
    /// [`Self::threaded_expression_doc`], which closes any `ClassVars`
    /// prelude inline (a same-class self-send/class-var-assignment
    /// argument, e.g. `self classMethod: x`, no longer needs a dedicated
    /// open/close dance — `close`-style rendering always produces a valid,
    /// self-contained `Document`). `class_var_version` is rolled back
    /// after each argument, matching this helper's pre-existing contract:
    /// safe only where a class-var mutation performed by a sub-expression
    /// argument does not need to stay visible afterward (actor-context
    /// dispatch sites). For class-method-context dispatch sites where the
    /// mutation must stay visible, use [`Self::thread_args`] instead and
    /// splice the returned prelude.
    pub(super) fn capture_argument_list_doc(
        &mut self,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let frame = self.current_frame();
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let saved_cv = self.class_var_version();
            // ADR 0118 phase 5b: see `subexpr_needs_prelude`'s doc
            // comment — an already-precompiled arg is read back via
            // `expression_doc`, never re-threaded.
            let doc = if self.precompiled_subexprs_contains(arg) {
                self.expression_doc(arg)?
            } else {
                self.threaded_expression_doc(arg, frame)?
            };
            self.set_class_var_version(saved_cv);
            parts.push(doc);
        }
        Ok(Document::Vec(parts))
    }

    /// ADR 0118 phase 5b: the `ThreadedValue`-based replacement
    /// for the deleted `capture_subexpr_sequence`/`hoist_subexpr_splits`/
    /// `split_subexpr_for_preamble` — same "decide once, hoist all or
    /// none" rule [`Self::sequence_children`] applies to a `MessageSend`'s
    /// own re-compiled children, adapted to a caller that builds its own
    /// `Document` directly rather than re-entering `generate_expression`.
    ///
    /// Returns `(prelude, docs)`: `docs` is one document per input
    /// expression, in order. `prelude` is empty in the common (no
    /// sub-expression needs one) case; otherwise every sub-expression up
    /// to and including the last one that needs a prelude is hoisted, in
    /// order, into `prelude` (a plain one via a fresh `let <prefix>N = ...
    /// in`), preserving left-to-right evaluation order exactly as
    /// `hoist_subexpr_splits` did.
    pub(super) fn thread_subexprs(
        &mut self,
        exprs: &[&Expression],
        prefix: &str,
    ) -> Result<(Vec<ThreadedStmt>, Vec<Document<'static>>)> {
        let frame = self.current_frame();
        let mut prelude: Vec<ThreadedStmt> = Vec::new();
        let Some(k) = exprs.iter().rposition(|e| self.subexpr_needs_prelude(e)) else {
            let mut docs = Vec::with_capacity(exprs.len());
            for e in exprs {
                docs.push(self.expression_doc(e)?);
            }
            return Ok((prelude, docs));
        };
        let mut docs: Vec<Document<'static>> = Vec::with_capacity(exprs.len());
        for (i, e) in exprs.iter().enumerate() {
            // ADR 0118 phase 5b: a child an ENCLOSING
            // `sequence_children` call already registered is read back via
            // the ordinary `expression_doc` (`take_precompiled_subexpr`)
            // instead of re-threading it — see `subexpr_needs_prelude`'s
            // doc comment for the double-dispatch this avoids.
            if i > k || self.precompiled_subexprs_contains(e) {
                docs.push(self.expression_doc(e)?);
                continue;
            }
            let tv = self.threaded_expression(e, frame)?;
            let must_bind = i < k && !tv.value_is_trivial();
            prelude.extend(tv.prelude);
            let value_doc = self.threaded_value_doc(&tv.value);
            if must_bind {
                let (binding, var) = self.bind_subexpr_to_temp(prefix, value_doc);
                prelude.push(ThreadedStmt::Statement(binding, e.unwrap_parens().span()));
                docs.push(leaf::var(var));
            } else {
                docs.push(value_doc);
            }
        }
        Ok((prelude, docs))
    }

    /// Threads `exprs` via [`Self::thread_subexprs`] and returns
    /// the result as a [`SequencedCall`] — the builder that collapses the
    /// `thread_subexprs` → `docs.remove(0)` → `close_prelude` idiom into a
    /// single `next()`/`close()` pair at each call site.
    pub(super) fn sequence_call(
        &mut self,
        exprs: &[&Expression],
        prefix: &str,
    ) -> Result<SequencedCall> {
        let (prelude, docs) = self.thread_subexprs(exprs, prefix)?;
        Ok(SequencedCall {
            prelude,
            docs: docs.into(),
        })
    }

    /// The one temp-binding step behind every "hoist an earlier
    /// sub-expression so a later one's effects can run ahead of it" rule:
    /// mints a fresh `<prefix>N` temp and returns the `let <temp> = <doc>
    /// in ` binding plus the temp's name. Shared by [`Self::thread_subexprs`]
    /// and `threaded_expression`'s sequencing rule (ADR 0118 §Decision 3)
    /// so the two cannot drift.
    pub(super) fn bind_subexpr_to_temp(
        &mut self,
        prefix: &str,
        doc: Document<'static>,
    ) -> (Document<'static>, String) {
        let var = self.fresh_temp_var(prefix);
        let binding = docvec!["let ", leaf::var(var.clone()), " = ", doc, " in "];
        (binding, var)
    }

    /// ADR 0118 phase 5b: the `ThreadedValue`-based replacement
    /// for the deleted `capture_args_with_preamble` — threads every
    /// argument via [`Self::thread_subexprs`] and joins the resulting docs
    /// with commas. Convenience wrapper for the common "no receiver, just
    /// args" pattern.
    ///
    /// Returns `(prelude, args_doc)` where `args_doc` is comma-separated.
    pub(super) fn thread_args(
        &mut self,
        arguments: &[Expression],
    ) -> Result<(Vec<ThreadedStmt>, Document<'static>)> {
        let exprs: Vec<&Expression> = arguments.iter().collect();
        let (prelude, var_docs) = self.thread_subexprs(&exprs, "Arg")?;
        Ok((prelude, Self::join_docs_with_commas(var_docs)))
    }

    /// ADR 0118 phase 5b: the `ThreadedValue`-based replacement
    /// for the deleted `bind_args_to_temps` — binds every argument
    /// expression to a fresh temp var via a prelude, returning `(prelude,
    /// arg_refs)`.
    ///
    /// Use this when an argument list is referenced multiple times in the
    /// generated code (e.g., both branches of an inline `case ... of`), to
    /// avoid double-evaluating side-effecting arguments.
    ///
    /// Unlike [`Self::thread_args`], this always emits let-bindings in the
    /// prelude (even in the fast path with no state effects) so the
    /// returned `arg_refs` are pure variable references with no side
    /// effects.
    pub(super) fn thread_args_bound(
        &mut self,
        arguments: &[Expression],
        prefix: &str,
    ) -> Result<(Vec<ThreadedStmt>, Vec<Document<'static>>)> {
        let frame = self.current_frame();
        let mut prelude: Vec<ThreadedStmt> = Vec::new();
        let mut arg_refs: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for arg in arguments {
            let span = arg.unwrap_parens().span();
            // ADR 0118 phase 5b: see `subexpr_needs_prelude`'s doc
            // comment — an already-precompiled arg is read back via
            // `expression_doc`, never re-threaded.
            let value_doc = if self.precompiled_subexprs_contains(arg) {
                self.expression_doc(arg)?
            } else {
                let tv = self.threaded_expression(arg, frame)?;
                prelude.extend(tv.prelude);
                self.threaded_value_doc(&tv.value)
            };
            let (binding, var) = self.bind_subexpr_to_temp(prefix, value_doc);
            prelude.push(ThreadedStmt::Statement(binding, span));
            arg_refs.push(leaf::var(var));
        }
        Ok((prelude, arg_refs))
    }

    /// Joins a list of documents into a comma-separated `Document::Vec`.
    pub(super) fn join_docs_with_commas(docs: Vec<Document<'static>>) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(docs.len() * 2);
        for (i, doc) in docs.into_iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(doc);
        }
        Document::Vec(parts)
    }

    /// ADR 0118 phase 5b: the `ThreadedValue`-based replacement
    /// for the deleted `finalize_dispatch_with_preamble` — wraps a closed
    /// dispatch `call_doc` with an optional threaded `prelude` from
    /// [`Self::thread_args`]/[`Self::thread_subexprs`] or a receiver's own
    /// prelude.
    ///
    /// If `prelude` is empty, returns `call_doc` unchanged (the original
    /// closed-expression behavior). Otherwise renders `prelude` through the
    /// same [`render`](super::threaded_ir::render) every spliced prelude
    /// goes through, followed by `let _ResultVar = call_doc in _ResultVar`
    /// — always a self-contained, closed `Document` (no side channel: the
    /// caller's own caller cannot observe a version this prelude bound,
    /// matching every other `Document`-returning consumer this issue
    /// migrates).
    pub(super) fn close_prelude(
        &mut self,
        prelude: &[ThreadedStmt],
        call_doc: Document<'static>,
        result_prefix: &str,
    ) -> Document<'static> {
        if prelude.is_empty() {
            return call_doc;
        }
        let prelude_doc = self.threaded_prelude_doc(prelude);
        let result_var = self.fresh_temp_var(result_prefix);
        docvec![
            prelude_doc,
            "let ",
            leaf::var(result_var.clone()),
            " = ",
            call_doc,
            " in ",
            leaf::var(result_var),
        ]
    }
}
