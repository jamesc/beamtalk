// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Message sending and dispatch compilation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This domain service handles the **core domain operation** of Beamtalk: message
//! sending. In Smalltalk and Beamtalk, message sending is THE fundamental operation,
//! not method calls or function invocation.
//!
//! # Message Send Protocol (ADR 0007 Phase 4)
//!
//! Messages are dispatched through the following strategy:
//!
//! 1. **Compiler intrinsics**: Language-level constructs that the compiler must
//!    generate inline code for (binary operators, block evaluation, spawn/await,
//!    class/nil testing). These are structural requirements, not type-specific dispatch.
//!
//! 2. **Runtime dispatch**: All other messages go through the unified entry point
//!    `beamtalk_message_dispatch:send/3` (BT-430), which routes to:
//!    - **Actors** (`beamtalk_object` records): Sync via `beamtalk_actor:sync_send/3` (BT-918 / ADR 0043)
//!    - **Class objects**: Sync via `beamtalk_object_class:class_send/3`
//!    - **Primitives** (everything else): Sync via `beamtalk_primitive:send/3`
//!
//! The primitive binding table from `lib/*.bt` (ADR 0007) drives stdlib method
//! compilation, while call-site dispatch uses runtime type checking since we
//! don't have static type information.
//!
//! # Special Cases (Compiler Intrinsics)
//!
//! - **Binary operators**: `+`, `-`, `*`, `/` → Direct Erlang arithmetic
//! - **Block evaluation**: `value`, `whileTrue:`, `repeat` → Direct function calls
//! - **ProtoObject/Object**: `class`, `isNil`, `respondsTo:` → Pattern matching
//! - **Spawn messages**: `Class spawn`, `Class spawnWith: args` → `gen_server:start_link`
//! - **Await messages**: `future await` → Blocking future resolution
//! - **Super sends**: `super methodName:` → Parent class dispatch

use super::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, VersionedVar,
};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, OpenScopeResult, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, Literal, MessageSelector, WellKnownSelector};
use beamtalk_core::source_analysis::Span;

/// Strips any number of `Parenthesized` wrappers to expose the syntactic
/// shape underneath — `(expr)`, `((expr))`, etc. all see through to `expr`.
///
/// Parentheses carry no runtime meaning (they only affect parse-time
/// precedence), so any codegen specialization that pattern-matches on the
/// *syntactic shape* of an expression (as [`is_character_typed_receiver`]
/// does) must look past them or a receiver as simple as `(Character value:
/// 10) asString` — parenthesized only to disambiguate the keyword send from
/// the trailing unary `asString` — would silently miss the fast path.
fn unwrap_parens(expr: &Expression) -> &Expression {
    let mut current = expr;
    while let Expression::Parenthesized { expression, .. } = current {
        current = expression;
    }
    current
}

/// BT-3214 (extends BT-2095): true if `expr`'s static type is Character,
/// determined purely from its syntactic shape — no general static type
/// inference exists in codegen, so this recognizes exactly the syntactic
/// forms that `Character.bt` declares as producing a Character: a Character
/// literal (`$A`), the class factory `Character value:`, and the two
/// instance methods with a `-> Character` return type, `uppercase` and
/// `lowercase` (applied recursively, since their own receiver must itself
/// be Character-typed — e.g. `$a uppercase lowercase`).
///
/// This distinction matters because Character values are bare integers at
/// the BEAM level (`Character` is declared `Integer subclass:`), so the
/// runtime `beamtalk_primitive:class_of/1` and `module_for_value/1` both
/// match `is_integer/1` unconditionally and route to `Integer`'s BIF module
/// — they cannot tell a Character-tagged integer from a `SmallInteger`,
/// because there is no runtime tag to tell them apart. BT-2095 fixed this
/// for the literal case (`$A asString`) by special-casing the receiver's
/// AST shape at codegen. `(Character value: 10) asString` and `$a uppercase
/// asString` are the same problem: the receiver is statically Character
/// (per the sender's declared `-> Character` return type) but was not
/// recognized because it isn't a literal, so it fell through to the generic
/// runtime-dispatch path and was misrouted to `Integer>>asString`,
/// producing `"10"` instead of a genuine 1-byte LF string. Recognizing
/// these additional shapes closes that gap without requiring general
/// static type inference in codegen.
fn is_character_typed_receiver(expr: &Expression) -> bool {
    match unwrap_parens(expr) {
        Expression::Literal(Literal::Character(_), _) => true,
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            let is_value_factory_call = arguments.len() == 1
                && matches!(
                    selector,
                    MessageSelector::Keyword(parts)
                        if parts.len() == 1 && parts[0].keyword == "value:"
                )
                && matches!(
                    unwrap_parens(receiver),
                    Expression::ClassReference { name, package: None, .. }
                        if name.name == "Character"
                );
            let is_character_returning_unary_send = arguments.is_empty()
                && matches!(
                    selector,
                    MessageSelector::Unary(name) if name == "uppercase" || name == "lowercase"
                )
                && is_character_typed_receiver(receiver);
            is_value_factory_call || is_character_returning_unary_send
        }
        _ => false,
    }
}

impl CoreErlangGenerator {
    /// BT-2816: Generates the `<{'error', ..., _}>` case clauses shared by all
    /// self-dispatch call sites (`safe_dispatch`/`dispatch` error branches).
    ///
    /// The dispatched call's error branch has two distinct shapes, mirroring the
    /// two `<{'error', ...}>` clauses already used at the `handle_cast`/
    /// `handle_info` boundary (see `gen_server/callbacks.rs`):
    ///
    /// 1. **Caught exception**: `safe_dispatch/3` packs a caught exception as a
    ///    3-tuple `{Type, Reason, Stacktrace}` in the middle element of its
    ///    `{'error', ..., State}` return (see `generate_safe_dispatch`). Passing
    ///    that whole triple straight to `beamtalk_error:'raise'/1` — which only
    ///    accepts a raw `#beamtalk_error{}` record — crashes with
    ///    `function_clause` instead of propagating the real error (BT-2816).
    ///    Destructuring the triple and routing it through
    ///    `beamtalk_exception_handler:'reraise'/4` mirrors the cross-actor call
    ///    boundary (`beamtalk_actor:sync_send_remote/3`), which correctly
    ///    classifies raw Erlang errors and preserves already-wrapped
    ///    `#beamtalk_error{}` values.
    /// 2. **Plain returned error**: `dispatch/4`'s DNU fallback (and other
    ///    non-exception error paths) *returns* `{'error', Error, State}` where
    ///    `Error` is a bare `#beamtalk_error{}` record — not a 3-tuple, so it
    ///    never reaches `safe_dispatch`'s try/catch at all. This fallback clause
    ///    must stay, or a self-send that resolves to DNU crashes with
    ///    `case_clause` instead of raising the DNU error.
    ///
    /// BT-2822: Passes a `selector`/`class` breadcrumb `Context` map to
    /// `reraise/4` — mirroring `sync_send_remote/3`'s
    /// `#{selector => Selector, class => Class}` construction — so a raw
    /// Erlang error escaping a self-send forwarding hop gets the same
    /// `ClassName>>selector: ...` location prefix (via `wrap_raw/2` /
    /// `located/3`) as the cross-actor equivalent.
    ///
    /// BT-2833: `class` is resolved via a *runtime* `beamtalk_actor:lookup_class/1`
    /// call on `self()`, not a compile-time literal atom. For a self-send inside
    /// a method a subclass inherits without overriding, the inherited method's
    /// code lives in the superclass module, so a literal `class_name()` atom
    /// would yield the superclass instead of the actor's actual runtime class —
    /// unlike `sync_send_remote/3`, which resolves the runtime class via the
    /// same `lookup_class/1` ETS reverse-lookup on `beamtalk_instance_registry`.
    /// Emitting the same runtime lookup here keeps the self-send breadcrumb's
    /// `class` value in parity with the cross-actor path for inherited methods.
    /// `selector_atom` is still known at compile time and stays a literal atom.
    ///
    /// Known caveat: `lookup_class/1` reads `beamtalk_instance_registry`,
    /// which the *spawner* populates only after `beamtalk_actor:safe_spawn/2`'s
    /// `await_initialize/1` confirms `handle_continue(initialize, _)` has
    /// finished (see `gen_server/spawn.rs`'s `instance_registration_doc`). A
    /// self-send that raises *during* `initialize` therefore runs before this
    /// actor's own registry entry exists, so `lookup_class(self())` falls
    /// back to `'unknown'` for that narrow window — trading a guaranteed-
    /// correct compile-time atom (for non-inherited classes only) for a
    /// safe-but-less-specific fallback, in exchange for a correct answer in
    /// every other case (including all inherited-method self-sends, the
    /// actual bug this fixes). `lookup_class/1` never crashes either way.
    ///
    /// Core Erlang map literals (`~{...}~`) cannot contain `call` expressions
    /// (see `lifecycle_start_telemetry_doc` in `gen_server/callbacks.rs` for the
    /// same constraint), so the lookup is hoisted into a `let` binding inside
    /// the clause body before the map is constructed. This only runs on the
    /// error path — no overhead on the success path.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///     let Class = call 'beamtalk_actor':'lookup_class'(call 'erlang':'self'()) in
    ///     call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace,
    ///         ~{'selector' => 'selector:', 'class' => Class}~)
    /// <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// <NoMatch> when 'true' -> call 'erlang':'error'({'case_clause', NoMatch})
    /// ```
    ///
    /// BT-3161: the trailing wildcard clause is not reachable at runtime
    /// (`safe_dispatch/3` only ever returns `{'reply', _, _}` or one of the
    /// two `{'error', _, _}` shapes matched above) but is required to make
    /// the `case` *statically* exhaustive — see `case_clause_fallback`'s
    /// doc comment for why an implicit fallback isn't good enough here.
    fn generate_self_dispatch_error_clause(
        &mut self,
        var_prefix: &str,
        selector_atom: &str,
    ) -> Document<'static> {
        let type_var = self.fresh_var(&format!("{var_prefix}Type"));
        let reason_var = self.fresh_var(&format!("{var_prefix}Reason"));
        let stack_var = self.fresh_var(&format!("{var_prefix}Stack"));
        let plain_error_var = self.fresh_var(&format!("{var_prefix}Plain"));
        let class_var = self.fresh_var(&format!("{var_prefix}Class"));
        let no_match_fallback = self.case_clause_fallback(&format!("{var_prefix}NoMatch"));
        docvec![
            "<{'error', {",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(reason_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "}, _}> when 'true' -> let ",
            leaf::var(class_var.clone()),
            " = call 'beamtalk_actor':'lookup_class'(call 'erlang':'self'()) in ",
            "call 'beamtalk_exception_handler':'reraise'(",
            leaf::var(type_var),
            ", ",
            leaf::var(reason_var),
            ", ",
            leaf::var(stack_var),
            ", ~{'selector' => ",
            leaf::atom(selector_atom.to_string()),
            ", 'class' => ",
            leaf::var(class_var),
            "}~) ",
            "<{'error', ",
            leaf::var(plain_error_var.clone()),
            ", _}> when 'true' -> call 'beamtalk_error':'raise'(",
            leaf::var(plain_error_var),
            ")",
            no_match_fallback,
            " ",
        ]
    }

    /// Generates a comma-separated argument list for function/message calls.
    ///
    /// This is a shared helper that eliminates the repeated pattern of iterating
    /// over arguments with comma separation found throughout dispatch codegen.
    /// Captures a comma-separated argument list as a `Document` (ADR 0018 bridge).
    ///
    /// BT-1935: Uses `expression_doc_with_open_scope` to detect and close any
    /// open let-chains produced by class method self-sends used as arguments.
    /// Without this, an argument like `(self classMethod: x)` embeds an open
    /// `let ... in ` chain inside the argument list, producing invalid Core Erlang.
    ///
    /// **WARNING (BT-1937):** This helper closes open let-chains inline and
    /// rolls back `class_var_version`, which causes class-var mutations
    /// performed by sub-expression class method self-sends to be silently
    /// dropped. Use this only for actor-context dispatch sites that never
    /// observe such open scopes (their args cannot mutate class vars). For
    /// class-method-context dispatch sites, use
    /// [`capture_args_with_preamble`](Self::capture_args_with_preamble) and
    /// emit the returned preamble before the dispatch call so the
    /// `ClassVarsN` bindings remain in scope at the outer level.
    fn capture_argument_list_doc(&mut self, arguments: &[Expression]) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let saved_cv = self.class_var_version();
            let (doc, open_scope) = self.expression_doc_with_open_scope(arg)?;
            match open_scope {
                Some(OpenScopeResult::Value(result_var)) => {
                    // Close the open scope inline: the let-chain + result_var forms
                    // a valid closed expression (e.g., `let X = ... in X`).
                    // Roll back class var version since the ClassVarsN binding is
                    // scoped inside the closed expression and not visible to
                    // subsequent code.
                    self.set_class_var_version(saved_cv);
                    parts.push(docvec![doc, leaf::var(result_var)]);
                }
                // BT-3053: e.g. a message argument that's itself `items do:
                // [...]` nested in a direct-params loop — no single value,
                // substitute do:'s own `nil` contract.
                Some(OpenScopeResult::NoValue) => {
                    self.set_class_var_version(saved_cv);
                    parts.push(docvec![doc, "'nil'"]);
                }
                None => {
                    parts.push(doc);
                }
            }
        }
        Ok(Document::Vec(parts))
    }

    /// BT-1937: Captures a sequence of sub-expressions, preserving left-to-right
    /// evaluation order **even when only some sub-expressions produce open scopes**
    /// from class method self-sends.
    ///
    /// Returns `(preamble, docs)` where `docs` is one document per input
    /// expression in the same order. If no sub-expression produces an open
    /// scope, the preamble is `Document::Nil` and each `doc` is the inline
    /// expression document — there is no hoisting overhead in the common case.
    ///
    /// If at least one sub-expression produces an open scope, **every**
    /// sub-expression is hoisted into the preamble in order:
    /// - Sub-expressions with their own open scope contribute their existing
    ///   let-chain (no rebinding — `result_var` is already in scope after the
    ///   chain).
    /// - Plain sub-expressions get a fresh `let _Var<i> = ... in ` binding.
    ///
    /// This is the key to preserving evaluation order: without the unconditional
    /// hoist, a hoisted later sub-expression would execute its preamble before
    /// the inline earlier sub-expression in the call site — reversing the
    /// observable order of side effects (BT-1937 review feedback).
    ///
    /// `class_var_version` is NOT rolled back. Subsequent code (the call,
    /// later sub-expressions, following statements) will see the advanced
    /// version, so references to `ClassVars` pick up earlier mutations.
    pub(super) fn capture_subexpr_sequence(
        &mut self,
        exprs: &[&Expression],
        prefix: &str,
    ) -> Result<(Document<'static>, Vec<Document<'static>>)> {
        // First pass: split each sub-expression into (its_preamble, its_doc).
        let mut splits: Vec<(Document<'static>, Document<'static>)> =
            Vec::with_capacity(exprs.len());
        for expr in exprs {
            splits.push(self.split_subexpr_for_preamble(expr)?);
        }

        let (any_hoisted, preamble_parts, docs) = self.hoist_subexpr_splits(splits, prefix);
        if any_hoisted {
            Ok((Document::Vec(preamble_parts), docs))
        } else {
            Ok((Document::Nil, docs))
        }
    }

    /// BT-3406 review follow-up: shared "decide once, hoist all or none" step
    /// behind both [`capture_subexpr_sequence`](Self::capture_subexpr_sequence)
    /// and `generate_cascade_args` (`expressions.rs`) — see the doc on
    /// `capture_subexpr_sequence` for the evaluation-order invariant this
    /// preserves.
    ///
    /// Given per-sub-expression `(preamble, value_doc)` splits (as produced by
    /// [`split_subexpr_for_preamble`](Self::split_subexpr_for_preamble) or
    /// `generate_field_assignment_open`), returns:
    /// - `(false, vec![], value_docs)` if no sub-expression needs hoisting —
    ///   `value_docs` are the original docs, safe to inline as-is.
    /// - `(true, preamble_parts, value_docs)` if at least one sub-expression
    ///   opened a scope — every sub-expression has been hoisted in order (a
    ///   plain one via a fresh `let <prefix>N = ... in`, an already-open one
    ///   by forwarding its existing preamble), and `value_docs` reference the
    ///   hoisted results. The caller is responsible for splicing
    ///   `preamble_parts` into its own preamble in order.
    pub(super) fn hoist_subexpr_splits(
        &mut self,
        splits: Vec<(Document<'static>, Document<'static>)>,
        prefix: &str,
    ) -> (bool, Vec<Document<'static>>, Vec<Document<'static>>) {
        let any_hoisted = splits.iter().any(|(p, _)| !matches!(p, Document::Nil));

        if !any_hoisted {
            let docs: Vec<_> = splits.into_iter().map(|(_, d)| d).collect();
            return (false, Vec::new(), docs);
        }

        let mut preamble_parts: Vec<Document<'static>> = Vec::with_capacity(splits.len());
        let mut var_docs: Vec<Document<'static>> = Vec::with_capacity(splits.len());
        for (expr_preamble, expr_doc) in splits {
            if matches!(expr_preamble, Document::Nil) {
                let (binding, var) = self.bind_subexpr_to_temp(prefix, expr_doc);
                preamble_parts.push(binding);
                var_docs.push(leaf::var(var));
            } else {
                preamble_parts.push(expr_preamble);
                var_docs.push(expr_doc);
            }
        }

        (true, preamble_parts, var_docs)
    }

    /// The one temp-binding step behind every "hoist an earlier
    /// sub-expression so a later one's effects can run ahead of it" rule:
    /// mints a fresh `<prefix>N` temp and returns the `let <temp> = <doc>
    /// in ` binding plus the temp's name. Shared by
    /// [`Self::hoist_subexpr_splits`] (the class-method open-scope
    /// protocol, BT-3406) and `threaded_expression`'s sequencing rule
    /// (ADR 0118 §Decision 3, BT-3415) so the two cannot drift.
    pub(super) fn bind_subexpr_to_temp(
        &mut self,
        prefix: &str,
        doc: Document<'static>,
    ) -> (Document<'static>, String) {
        let var = self.fresh_temp_var(prefix);
        let binding = docvec!["let ", leaf::var(var.clone()), " = ", doc, " in "];
        (binding, var)
    }

    /// BT-1937: Captures an argument list using
    /// [`capture_subexpr_sequence`](Self::capture_subexpr_sequence) and joins
    /// the resulting docs with commas. Convenience wrapper for the common
    /// "no receiver, just args" pattern.
    ///
    /// Returns `(preamble, args_doc)` where `args_doc` is comma-separated.
    pub(super) fn capture_args_with_preamble(
        &mut self,
        arguments: &[Expression],
    ) -> Result<(Document<'static>, Document<'static>)> {
        let exprs: Vec<&Expression> = arguments.iter().collect();
        let (preamble, var_docs) = self.capture_subexpr_sequence(&exprs, "Arg")?;
        Ok((preamble, Self::join_docs_with_commas(var_docs)))
    }

    /// BT-1942: Binds every argument expression to a fresh temp var via a
    /// preamble, returning `(preamble, arg_refs, any_open_scope)`.
    ///
    /// Use this when an argument list is referenced multiple times in the
    /// generated code (e.g., both branches of an inline `case ... of`),
    /// to avoid double-evaluating side-effecting arguments and to hoist any
    /// open let-chain produced by class method self-sends.
    ///
    /// Unlike [`capture_args_with_preamble`](Self::capture_args_with_preamble),
    /// this always emits let-bindings in the preamble (even in the fast path
    /// with no open scopes) so the returned `arg_refs` are pure variable
    /// references with no side effects.
    ///
    /// `any_open_scope` is `true` if any argument produced an open let-chain
    /// from a class method self-send — the caller should then propagate the
    /// scope upward via `last_open_scope_result`.
    pub(super) fn bind_args_to_temps(
        &mut self,
        arguments: &[Expression],
        prefix: &str,
    ) -> Result<(Document<'static>, Vec<Document<'static>>, bool)> {
        let mut preamble_parts: Vec<Document<'static>> = Vec::new();
        let mut arg_refs: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        let mut any_open_scope = false;
        for arg in arguments {
            let (arg_doc, open_scope) = self.expression_doc_with_open_scope(arg)?;
            let arg_var = self.fresh_temp_var(prefix);
            match open_scope {
                Some(OpenScopeResult::Value(result_var)) => {
                    any_open_scope = true;
                    preamble_parts.push(arg_doc);
                    preamble_parts.push(docvec![
                        "let ",
                        leaf::var(arg_var.clone()),
                        " = ",
                        leaf::var(result_var),
                        " in ",
                    ]);
                }
                // BT-3053: no single value — substitute do:'s own `nil` contract.
                Some(OpenScopeResult::NoValue) => {
                    any_open_scope = true;
                    preamble_parts.push(arg_doc);
                    preamble_parts.push(docvec![
                        "let ",
                        leaf::var(arg_var.clone()),
                        " = 'nil' in ",
                    ]);
                }
                None => {
                    preamble_parts.push(docvec![
                        "let ",
                        leaf::var(arg_var.clone()),
                        " = ",
                        arg_doc,
                        " in ",
                    ]);
                }
            }
            arg_refs.push(leaf::var(arg_var));
        }
        let preamble = if preamble_parts.is_empty() {
            Document::Nil
        } else {
            Document::Vec(preamble_parts)
        };
        Ok((preamble, arg_refs, any_open_scope))
    }

    /// BT-1937: Joins a list of documents into a comma-separated `Document::Vec`.
    fn join_docs_with_commas(docs: Vec<Document<'static>>) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(docs.len() * 2);
        for (i, doc) in docs.into_iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(doc);
        }
        Document::Vec(parts)
    }

    /// BT-1937: Splits a sub-expression into a hoisted preamble and the
    /// document used in its enclosing call/literal/operator.
    ///
    /// If the sub-expression produces an open let-chain (e.g., a class method
    /// self-send that mutates class vars), the chain becomes the preamble and
    /// the value used at the use site is just the result variable. Otherwise
    /// the preamble is `Document::Nil` and the original doc is used directly.
    /// `class_var_version` is NOT rolled back when a preamble is produced —
    /// the `ClassVarsN` binding remains in scope at the outer level so
    /// subsequent code (later args, the enclosing call, following statements)
    /// can reference the new version.
    pub(super) fn split_subexpr_for_preamble(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, Document<'static>)> {
        let (expr_doc, open_scope) = self.expression_doc_with_open_scope(expr)?;
        match open_scope {
            Some(OpenScopeResult::Value(result_var)) => Ok((expr_doc, leaf::var(result_var))),
            // BT-3053: no single value — substitute do:'s own `nil` contract.
            Some(OpenScopeResult::NoValue) => Ok((expr_doc, Document::Str("'nil'"))),
            None => Ok((Document::Nil, expr_doc)),
        }
    }

    /// BT-1937: Wraps a closed dispatch `call_doc` with an optional hoisted
    /// preamble from [`capture_args_with_preamble`](Self::capture_args_with_preamble)
    /// or from a receiver's open scope.
    ///
    /// If `preamble` is `Document::Nil`, returns `call_doc` unchanged (the
    /// original closed-expression behavior).
    ///
    /// If `preamble` is non-empty, returns
    /// `preamble + let _ResultVar = call_doc in ` (an open let-chain) and
    /// stores `_ResultVar` in `last_open_scope_result`. The enclosing
    /// expression context (statement, local-var binding, outer message send)
    /// must close or further propagate the open scope so that the `ClassVarsN`
    /// bindings stay visible to subsequent code.
    pub(super) fn finalize_dispatch_with_preamble(
        &mut self,
        preamble: Document<'static>,
        call_doc: Document<'static>,
        result_prefix: &str,
    ) -> Document<'static> {
        if matches!(preamble, Document::Nil) {
            return call_doc;
        }
        let result_var = self.fresh_temp_var(result_prefix);
        let doc = docvec![
            preamble,
            "let ",
            leaf::var(result_var.clone()),
            " = ",
            call_doc,
            " in ",
        ];
        self.last_open_scope_result = Some(OpenScopeResult::Value(result_var));
        doc
    }

    /// BT-412/BT-2007: Wrap a class-method call that may return either a
    /// plain value or a `{'class_var_result', Result, NewClassVars}` tuple,
    /// threading the new class-var binding and exposing the unwrapped result.
    ///
    /// Emits (preamble elided):
    ///
    /// ```erlang
    /// let _CMR       = <call_doc> in
    /// let ClassVarsN = case _CMR of
    ///                    <{'class_var_result', _MR, _CV}> when 'true' -> _CV
    ///                    <_PCV>                            when 'true' -> ClassVars<current>
    ///                  end in
    /// let _Unwrapped = case _CMR of
    ///                    <{'class_var_result', _WR, _}>    when 'true' -> _WR
    ///                    <_PR>                             when 'true' -> _PR
    ///                  end in
    /// ```
    ///
    /// ADR 0118 phase 5a (BT-3421): returns a [`ThreadedValue`] whose
    /// prelude carries the real `ClassVars` `Bind` this call rebinds —
    /// `_Unwrapped` is the value, with no consuming body of its own.
    /// Callers splice the prelude into their own frame or open-scope-convert
    /// it via [`Self::threaded_value_to_open_scope_doc`] so `ClassVarsN`
    /// stays visible to the continuation (the pre-ADR-0118 open-let-chain
    /// contract, preserved byte-for-byte). Shared by the local-class-method
    /// branch (branch 1) and the BT-2007 inherited-dispatch branch in
    /// [`generate_class_method_self_send`](Self::generate_class_method_self_send).
    pub(super) fn emit_class_var_result_unwrap(
        &mut self,
        args_preamble: Document<'static>,
        call_doc: Document<'static>,
    ) -> ThreadedValue {
        let call_result = self.fresh_temp_var("CMR");
        let cv = self.current_class_var();
        // BT-3148: the version numbers driving both verify() and the real
        // Bind rendered below — captured before minting, matching the old
        // `cv`/`new_cv` name-capture ordering exactly (fresh_temp_var call
        // order for CV/MR/PCV below is unaffected: `class_var_version`
        // mints from an entirely separate counter).
        let source_version = self.class_var_version();
        let inner_cv = self.fresh_temp_var("CV");
        let inner_res = self.fresh_temp_var("MR");
        let plain_cv = self.fresh_temp_var("PCV");

        // The class-var rebind's opaque RHS (ADR 0111 Addendum 2 "Gap 1"'s
        // `ValueRef::Doc` precedent): a `case` expression selecting the
        // inherited self-dispatch call's own returned class vars when
        // present, falling back to the current `ClassVars` otherwise — not
        // representable by a bare `ValueRef::Var`/`Version`/`Literal`.
        let class_var_case_doc = docvec![
            "case ",
            leaf::var(call_result.clone()),
            " of <{'class_var_result', ",
            leaf::var(inner_res),
            ", ",
            leaf::var(inner_cv.clone()),
            "}> when 'true' -> ",
            leaf::var(inner_cv.clone()),
            " <",
            leaf::var(plain_cv),
            "> when 'true' -> ",
            leaf::var(cv),
            " end",
        ];

        self.next_class_var();
        let target_version = self.class_var_version();

        // BT-3135 (ADR 0111 Phase D) / BT-3148: construct, verify, and
        // render this Bind through the real `threaded_ir` pipeline — no
        // second, hand-rolled `Document` reconstructs it. This site never
        // itself needs the ADR 0110 shadow write — it rebinds `ClassVarsN`
        // from an inherited self-dispatched call's own `class_var_result`
        // return, and that call runs in this same class's gen_server
        // process, so any mutation it made was already shadow-written by
        // the *callee's own* `generate_field_assignment` under the
        // identical `ClassSelf`-tagged key (see
        // `threaded_ir::construct_and_verify_class_var_bind`'s doc comment
        // / ADR 0110 §Runtime change). `shadow_write_eligible: false` is
        // what deliberately exempts this Bind from the shadow-write check
        // unconditionally, since it never carries a shadow-write obligation
        // of its own regardless of block_depth (ADR 0111 Addendum 9,
        // Question 2).
        //
        // BT-3169 (ADR 0111 Addendum 9, Questions 2/5): `frame` is
        // `FrameId::ROOT` at a class method's own top level, but this same
        // function is also reached from INSIDE a `do:`/`collect:`/`select:`/
        // `inject:into:` fold body's closure (a same-class self-send used or
        // discarded there compiles through the identical
        // `generate_class_method_self_send` path) — `self.in_loop_body`
        // distinguishes the two: a fold-body call site's real, already-minted
        // frame is `self.current_branch_frame()`, never `ROOT` (loop bodies
        // always run inside `with_branch_context`). Passing `ROOT` there
        // would be dishonest about this rebind's real nesting identity and
        // would let a second, unrelated mutation later in the SAME iteration
        // spuriously collide during verification (Question 2's own
        // `UnboundVersion` finding for the analogous top-frame field-write
        // call site).
        let frame = if self.in_loop_body {
            self.current_branch_frame()
        } else {
            super::threaded_ir::FrameId::ROOT
        };
        let (bind, rebind_errors) = super::threaded_ir::construct_and_verify_class_var_bind(
            super::threaded_ir::BindOp::Direct(super::threaded_ir::ValueRef::Doc(
                class_var_case_doc,
            )),
            false,
            frame,
            false,
            source_version,
            target_version,
            beamtalk_core::source_analysis::Span::default(),
        );
        self.report_threaded_ir_verify_errors(
            &rebind_errors,
            "class-var rebind from inherited self-dispatch result",
            beamtalk_core::source_analysis::Span::default(),
        );

        let result = self.fresh_temp_var("Unwrapped");
        let wrapped_res = self.fresh_temp_var("WR");
        let plain_res = self.fresh_temp_var("PR");

        // ADR 0118 phase 5a: the call-setup and unwrap steps stay opaque
        // `Statement`s (the SAME `Document` text this function always built,
        // byte-for-byte); `bind` is no longer rendered eagerly into the
        // middle of one big `Document` — it is a real, un-rendered
        // `ThreadedStmt::Bind` in the returned prelude, so the class-var
        // mutation this call rebinds is visible to whichever `ThreadedIr`
        // frame the caller splices the prelude into (ADR 0118 §Decision 4),
        // not just to this producer's own isolated `construct_and_verify_class_var_bind`
        // check above.
        let call_stmt_doc = docvec![
            args_preamble,
            "let ",
            leaf::var(call_result.clone()),
            " = ",
            call_doc,
            " in ",
        ];
        let unwrap_stmt_doc = docvec![
            "let ",
            leaf::var(result.clone()),
            " = case ",
            leaf::var(call_result),
            " of <{'class_var_result', ",
            leaf::var(wrapped_res.clone()),
            ", _}> when 'true' -> ",
            leaf::var(wrapped_res),
            " <",
            leaf::var(plain_res.clone()),
            "> when 'true' -> ",
            leaf::var(plain_res),
            " end in ",
        ];
        let span = beamtalk_core::source_analysis::Span::default();
        ThreadedValue {
            prelude: vec![
                ThreadedStmt::Statement(call_stmt_doc, span),
                bind,
                ThreadedStmt::Statement(unwrap_stmt_doc, span),
            ],
            value: ValueRef::Var(result),
        }
    }

    /// BT-3168 (ADR 0111 Addendum 9, Questions 2/3): rebinds `ClassVarsN`
    /// from an already-produced value Document — a Letrec loop construct's
    /// own returned tuple slot carrying the `ClassVars` mutations threaded
    /// through its recursive tail call (`while_loops.rs`/`counted_loops.rs`
    /// via `generate_counted_stateful_loop`). Mirrors
    /// [`Self::emit_class_var_result_unwrap`]'s inherited-self-dispatch
    /// rebind: never itself a shadow-write producer (each loop iteration's
    /// own class-var write, inside the loop body, already shadow-wrote it
    /// under the identical `ClassSelf`-tagged key — ADR 0110 §Runtime
    /// change) and never claims a real nested frame identity of its own
    /// (`FrameId::ROOT`, `shadow_write_eligible: false`, per ADR 0111
    /// Addendum 9 Question 2).
    pub(super) fn rebind_class_vars_from_doc(
        &mut self,
        value_doc: Document<'static>,
        span: beamtalk_core::source_analysis::Span,
    ) -> Document<'static> {
        let source_version = self.class_var_version();
        self.next_class_var();
        let target_version = self.class_var_version();
        let (bind, errors) = super::threaded_ir::construct_and_verify_class_var_bind(
            super::threaded_ir::BindOp::Direct(super::threaded_ir::ValueRef::Doc(value_doc)),
            false,
            super::threaded_ir::FrameId::ROOT,
            false,
            source_version,
            target_version,
            span,
        );
        self.report_threaded_ir_verify_errors(
            &errors,
            "class-var rebind from a loop construct's threaded result",
            span,
        );
        let mut ctx = super::threaded_ir::RenderCtx::new(self);
        super::threaded_ir::render(std::slice::from_ref(&bind), &mut ctx)
    }

    /// Generates code for a message send.
    ///
    /// This is the **main entry point** for message compilation. It dispatches
    /// to specialized handlers for different message patterns, and falls back
    /// to runtime dispatch via `beamtalk_message_dispatch:send/3` (BT-430)
    /// which handles actors, class objects, and primitives uniformly.
    ///
    /// # Message Dispatch Strategy (ADR 0007 Phase 4)
    ///
    /// 1. **Super sends** → `generate_super_send`
    /// 2. **Binary operators** → `generate_binary_op` (synchronous Erlang ops)
    /// 3. **`ProtoObject` messages** → `try_generate_protoobject_message` (synchronous)
    /// 4. **Object messages** → `try_generate_object_message` → delegates to nil protocol, error signaling, object identity, object reflection
    /// 5. **Block messages** → `try_generate_block_message` (structural intrinsics)
    /// 6. **Spawn/Await** → `try_handle_spawn_await` (spawn, await intrinsics)
    /// 7. **Erlang interop** → `try_handle_erlang_interop` (ADR 0028 direct call / proxy)
    /// 8. **Class references** → `try_handle_class_reference` (workspace bindings, class methods)
    /// 9. **Self-sends** → `try_handle_self_dispatch` (synchronous actor self-dispatch)
    /// 10. **Default** → Runtime dispatch (BT-223: actor vs primitive check)
    pub(super) fn generate_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Special case: super message send
        // Super calls invoke the superclass implementation
        if matches!(receiver, Expression::Super(_)) {
            return self.generate_super_send(selector, arguments);
        }

        // Compile-time type assertion: `expr asType: SomeClass` (ADR 0025 Phase 2b)
        // Erased at codegen — generates only the receiver expression (zero runtime cost)
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "asType:" && arguments.len() == 1 {
                return self.expression_doc(receiver);
            }
        }

        // For binary operators, use Erlang's built-in operators (these are synchronous)
        if let MessageSelector::Binary(op) = selector {
            // BT-101: Method lookup via `>>` operator (e.g., Counter >> #increment)
            // BT-323: Support `>>` on any expression, not just class literals
            if op.as_str() == ">>" {
                if let Expression::ClassReference { name, .. } = receiver {
                    return self.generate_method_lookup(&name.name, arguments);
                }
                // Runtime fallback: evaluate receiver and call method/2
                return self.generate_runtime_method_lookup(receiver, arguments);
            }

            let doc = self.generate_binary_op(op, receiver, arguments)?;
            return Ok(doc);
        }

        // BT-2095 / BT-3214: Reflective and class-introspection sends on a
        // Character-typed receiver (a Character literal, or a `Character
        // value:` factory call — see `is_character_typed_receiver`) must
        // honour the static type, not fall into the protoobject/object
        // handlers that key on runtime `class_of/1` (which returns `'Integer'`
        // for any integer receiver):
        //   * `$A class`            → must return `'Character'`
        //   * `$A respondsTo: #foo` → must consult Character's `has_method/1`
        //   * `$A perform: #foo`    → must dispatch through Character
        // The protoobject/object handlers run earlier than the general
        // Character-typed fallback below, so intercept them here.
        if is_character_typed_receiver(receiver) {
            match selector.well_known() {
                Some(WellKnownSelector::Class) => {
                    // BT-1937: Hoist any side effects in the receiver expression
                    // (none for a literal, but capture preserves the contract).
                    let (preamble, _) = self.capture_subexpr_sequence(&[receiver], "CharCls")?;
                    // Resolve to the Character class object so equality with
                    // the `Character` class reference holds — `class_of_object`
                    // for raw integer 65 would otherwise return Integer's
                    // class object via `class_of/1 == 'Integer'`.
                    let call_doc = Document::Str(
                        "call 'beamtalk_primitive':'class_of_object_by_name'('Character')",
                    );
                    return Ok(self.finalize_dispatch_with_preamble(
                        preamble,
                        call_doc,
                        "CharClsRes",
                    ));
                }
                Some(WellKnownSelector::RespondsTo) => {
                    let exprs: [&Expression; 2] = [receiver, &arguments[0]];
                    let (preamble, mut docs) = self.capture_subexpr_sequence(&exprs, "CharResp")?;
                    let _recv = docs.remove(0);
                    let sel_doc = docs.remove(0);
                    let call_doc =
                        docvec!["call 'bt@stdlib@character':'has_method'(", sel_doc, ")"];
                    return Ok(self.finalize_dispatch_with_preamble(
                        preamble,
                        call_doc,
                        "CharRespRes",
                    ));
                }
                Some(
                    WellKnownSelector::Perform
                    | WellKnownSelector::PerformWithArgs
                    | WellKnownSelector::PerformLocallyWithArgs,
                ) => {
                    return self.generate_character_typed_dispatch(receiver, selector, arguments);
                }
                _ => {}
            }
        }

        // Special case: ProtoObject methods - fundamental operations on all objects
        // class returns the class name for any object (primitives or actors)
        if let Some(doc) = self.try_generate_protoobject_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: Object methods - reflection and introspection
        // respondsTo:, fieldNames, fieldAt: enable runtime introspection
        if let Some(doc) = self.try_generate_object_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: Block evaluation messages (value, value:, whileTrue:, etc.)
        // These are synchronous function calls, not async actor messages
        if let Some(doc) = self.try_generate_block_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: Dictionary iteration messages (do:, doWithKey:, keysAndValuesDo:)
        // Must come before list messages so dictionary-specific selectors are handled correctly.
        if let Some(doc) = self.try_generate_dict_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: List iteration messages (do:, collect:, select:, reject:, inject:into:)
        // These are structural intrinsics that require inline code generation for proper
        // state threading when used inside actor methods with field mutations.
        if let Some(doc) = self.try_generate_list_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-915: Boolean conditionals (ifTrue:, ifFalse:, ifTrue:ifFalse:) in actor context
        // with field mutations. Generates inline case expressions that thread state correctly
        // through both branches.
        if let Some(doc) = self.try_generate_boolean_protocol(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: spawn, spawnWith:, await, awaitForever, await:
        if let Some(doc) = self.try_handle_spawn_await(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-677 / BT-682 / ADR 0028: Erlang interop — direct calls and proxy construction
        if let Some(doc) = self.try_handle_erlang_interop(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-1435: Logger intrinsics — inline logger:log/3 with domain metadata
        if let Some(doc) = self.try_generate_logger_intrinsic(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-374 / ADR 0010: Workspace binding dispatch + class method calls
        if let Some(doc) = self.try_handle_class_reference(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-412: Self-sends in class methods route through class_send
        if let Some(doc) = self.try_handle_class_method_self_send(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-330: Self-sends in actor methods use direct synchronous dispatch
        if let Some(doc) = self.try_handle_self_dispatch(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-2095 / BT-3214: Character-typed receivers (literals like `$A`,
        // and `Character value: N` factory calls) have static type Character,
        // but at the BEAM level they are plain integers — runtime `class_of/1`
        // returns `'Integer'` and routes them to `bt@stdlib@integer:dispatch/3`.
        // Specialize at codegen so `$A asInteger`, `$A printString`, `$A
        // uppercase`, `(Character value: 10) asString`, etc. reach the
        // Character module's dispatch (which delegates to Integer for inherited
        // methods like `+`, `-`, `bitAnd:`).
        if is_character_typed_receiver(receiver) {
            return self.generate_character_typed_dispatch(receiver, selector, arguments);
        }

        // BT-430: Unified dispatch via beamtalk_message_dispatch:send/3
        self.generate_runtime_dispatch(receiver, selector, arguments)
    }

    /// BT-2095 / BT-3214: Routes a non-binary message to the Character module's
    /// `dispatch/3`, for any receiver `is_character_typed_receiver` recognizes
    /// (a Character literal or a `Character value:` factory call).
    ///
    /// At the BEAM level, Character values are plain integers, so the default
    /// runtime dispatch path (keyed on `is_integer/1`) sends them to the Integer
    /// module. This codegen specialization restores the static type by emitting
    /// a direct call to `bt@stdlib@character:dispatch/3` instead. The receiver
    /// expression itself is compiled normally (`capture_subexpr_sequence`), so
    /// this works whether the receiver is a bare literal or an arbitrary
    /// expression statically known to produce a Character.
    fn generate_character_typed_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();

        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        let (preamble, mut docs) = self.capture_subexpr_sequence(&all_exprs, "CharDisp")?;
        let actual_receiver = docs.remove(0);
        let args_doc = Self::join_docs_with_commas(docs);

        let call_doc = docvec![
            "call 'bt@stdlib@character':'dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            actual_receiver,
            ")"
        ];

        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "CharDispRes"))
    }

    /// Generates a cast (fire-and-forget) message send (BT-920).
    ///
    /// Called when the AST `MessageSend` node has `is_cast: true` (the `!` suffix).
    ///
    /// # Dispatch Strategy
    ///
    /// - **Self-sends in actor context** (`self someMethod!`): Calls `safe_dispatch` directly
    ///   but discards both the result and any state update — fire-and-forget semantics
    ///   within the same process.
    /// - **All other sends**: Routes through `beamtalk_message_dispatch:cast/3`, which
    ///   extracts the actor PID and calls `beamtalk_actor:cast_send/3`. Non-actor
    ///   receivers are silently ignored.
    ///
    /// Cast sends always evaluate to `'ok'`.
    pub(super) fn generate_cast_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Self-sends with ! in actor context: direct dispatch, discard result.
        // BT-1475: Only use direct safe_dispatch when NOT inside a block (block_depth == 0).
        // Blocks may execute in a different process (Timer callbacks, cross-actor callbacks),
        // so self-cast sends inside blocks must route through the actor mailbox via
        // beamtalk_message_dispatch:cast/3 to reach the actor's gen_server process.
        if self.context == CodeGenContext::Actor && self.block_depth == 0 {
            if let Expression::Identifier(id) = receiver {
                if id.name == "self" {
                    return self.generate_self_cast_send(selector, arguments);
                }
            }
        }

        // Non-self cast sends (and self-casts inside blocks): route through
        // beamtalk_message_dispatch:cast/3
        self.generate_runtime_cast(receiver, selector, arguments)
    }

    /// Generates a self-cast send in actor context (BT-920).
    ///
    /// Calls `safe_dispatch` synchronously but discards the result (and any state
    /// mutation from the callee). Returns `'ok'` as the expression value.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _Cast0 = call 'module':'safe_dispatch'('selector', [Args], State) in 'ok'
    /// ```
    fn generate_self_cast_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();
        let discard_var = self.fresh_temp_var("Cast");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            "let ",
            leaf::var(discard_var),
            " = call ",
            leaf::atom(module),
            ":'safe_dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            leaf::var(current_state),
            ") in 'ok'",
        ];

        Ok(doc)
    }

    /// Generates unified runtime cast via `beamtalk_message_dispatch:cast/3` (BT-920).
    ///
    /// Fire-and-forget path: routes to the actor's message queue via
    /// `beamtalk_actor:cast_send/3`. Non-actor receivers are silently ignored.
    /// Always returns `'ok'`.
    fn generate_runtime_cast(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();
        // BT-1937: Capture receiver + args as one ordered sub-expression
        // sequence so left-to-right evaluation order is preserved when ANY
        // sub-expression has an open scope from a class method self-send.
        // capture_subexpr_sequence force-hoists every sub-expression in that
        // case; the fast path (no open scopes) leaves them inline.
        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        let (preamble, mut docs) = self.capture_subexpr_sequence(&all_exprs, "Cast")?;
        let actual_receiver = docs.remove(0);
        let args_doc = Self::join_docs_with_commas(docs);

        let call_doc = docvec![
            "call 'beamtalk_message_dispatch':'cast'(",
            actual_receiver,
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])",
        ];

        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "CastRes"))
    }

    /// Generates unified runtime dispatch via `beamtalk_message_dispatch:send/3` (BT-430).
    ///
    /// This is the fallback path for messages that don't match any compiler intrinsic.
    /// Routes through the unified entry point which handles actors (sync via `gen_server:call`),
    /// class objects (sync), and primitives (sync). Returns a value directly — no Future
    /// wrapping (BT-918 / ADR 0043).
    fn generate_runtime_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();
        if matches!(selector, MessageSelector::Binary(_)) {
            return Err(CodeGenError::Internal(format!(
                "unexpected binary selector in generate_message_send: {selector_atom}"
            )));
        }

        // BT-1343: Emit dynamic dispatch fallback diagnostic.
        if self.codegen_diagnostics_enabled {
            let span = receiver.span();
            let line_info = self
                .span_to_line(span)
                .map_or(String::new(), |l| format!(" at line {l}"));
            self.emit_codegen_diagnostic(
                format!(
                    "Send '{selector_atom}'{line_info}: dynamic dispatch (receiver type unknown)"
                ),
                span,
            );
        }

        // BT-1937: Capture receiver + args as one ordered sub-expression
        // sequence so left-to-right evaluation order is preserved.
        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        let (preamble, mut docs) = self.capture_subexpr_sequence(&all_exprs, "Disp")?;
        let actual_receiver = docs.remove(0);
        let args_doc = Self::join_docs_with_commas(docs);

        let call_doc = docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            actual_receiver,
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])"
        ];

        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "DispRes"))
    }

    /// Handles spawn, spawnWith:, await, awaitForever, and await: intrinsics.
    ///
    /// Returns `Some(())` if the message was handled, `None` if it should
    /// fall through to the next dispatch strategy.
    fn try_handle_spawn_await(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // Unary spawn/await messages
        if let MessageSelector::Unary(name) = selector {
            // BT-246: Only match ClassReference, not Identifier.
            if name == "spawn" && arguments.is_empty() {
                if let Expression::ClassReference { name, package, .. } = receiver {
                    let pkg = package.as_ref().map(|p| p.name.as_str());
                    let doc = self.generate_actor_spawn_qualified(&name.name, pkg, None)?;
                    return Ok(Some(doc));
                }
            }
            if name == "await" && arguments.is_empty() {
                let doc = self.generate_await(receiver)?;
                return Ok(Some(doc));
            }
            if name == "awaitForever" && arguments.is_empty() {
                let doc = self.generate_await_forever(receiver)?;
                return Ok(Some(doc));
            }
        }

        // Keyword await:/spawnWith: messages
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "await:" && arguments.len() == 1 {
                let doc = self.generate_await_with_timeout(receiver, &arguments[0])?;
                return Ok(Some(doc));
            }
            // BT-246: Only match ClassReference, not Identifier.
            if parts.len() == 1 && parts[0].keyword == "spawnWith:" && arguments.len() == 1 {
                if let Expression::ClassReference { name, package, .. } = receiver {
                    let pkg = package.as_ref().map(|p| p.name.as_str());
                    let doc =
                        self.generate_actor_spawn_qualified(&name.name, pkg, Some(&arguments[0]))?;
                    return Ok(Some(doc));
                }
            }
        }

        Ok(None)
    }

    /// BT-677 / ADR 0028: Handles `Erlang` class reference for BEAM interop.
    ///
    /// Two cases are handled:
    ///
    /// 1. **Direct call optimization (BT-682, ADR 0028 Phase 4):** When the
    ///    receiver is `MessageSend(ClassReference("Erlang"), Unary(module))` and
    ///    the outer selector is a function call, emits a direct BEAM call:
    ///    ```erlang
    ///    call 'lists':'reverse'(Xs)
    ///    ```
    ///    This eliminates proxy map allocation entirely.
    ///
    /// 2. **Proxy construction (BT-677):** When the receiver is
    ///    `ClassReference("Erlang")` and the message is a unary module name,
    ///    generates an inline `ErlangModule` proxy map:
    ///    ```erlang
    ///    ~{'$beamtalk_class' => 'ErlangModule', 'module' => 'lists'}~
    ///    ```
    ///    This fallback handles `proxy := Erlang lists` (standalone proxy).
    ///
    /// Standard class-protocol selectors (e.g. `class`, `new`, `superclass`)
    /// fall through to normal class dispatch so that `Erlang class` returns the
    /// metaclass rather than a proxy for module `'class'`.
    ///
    /// BT-3079: FFI receiver recognition (the class-protocol filter, the
    /// package-qualification check, and parenthesized-receiver peeling) is
    /// centralized in [`beamtalk_core::ffi_receiver`] — this is the only place those
    /// rules are implemented.
    fn try_handle_erlang_interop(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // BT-682: Direct call optimization — `Erlang lists reverse: xs` (and the
        // parenthesized `(Erlang lists) reverse: xs`) → `call 'lists':'reverse'(Xs)`
        // with no proxy map allocation. Only when the module name is a
        // compile-time literal (ClassReference path).
        if let Some(module_name) = beamtalk_core::ffi_receiver::erlang_module_of_receiver(receiver)
        {
            return self.generate_direct_erlang_call(module_name, selector, arguments);
        }

        // BT-677: Proxy construction — `Erlang lists` → inline proxy map
        if let Expression::ClassReference { name, package, .. } = receiver {
            if package.is_some() || name.name != "Erlang" {
                return Ok(None);
            }
            match selector {
                MessageSelector::Unary(module_name)
                    if !beamtalk_core::ffi_receiver::is_class_protocol_selector(module_name) =>
                {
                    let doc = docvec![
                        "~{'$beamtalk_class' => 'ErlangModule', 'module' => ",
                        leaf::atom(module_name.clone()),
                        "}~",
                    ];
                    Ok(Some(doc))
                }
                _ => {
                    // Keyword/binary on Erlang class itself, or a class-protocol
                    // selector, falls through to normal class dispatch.
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }

    /// BT-682: Generates a proxy-routed call for Erlang interop (BT-1127).
    ///
    /// Converts Beamtalk selectors to Erlang function names and routes through
    /// `beamtalk_erlang_proxy:direct_call/3` for automatic binary→charlist coercion:
    /// - Unary: `node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [])` (zero-arg)
    /// - Keyword single: `reverse:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'reverse', [Xs])`
    /// - Keyword multi: `seq:with:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'seq', [1, 10])`
    ///
    /// Returns `None` for selectors that are Object/ProtoObject protocol methods
    /// (e.g. `printString`, `asString`) — these must go through runtime dispatch
    /// so the proxy's inherited protocol methods are called, not a non-existent
    /// Erlang function.
    ///
    /// BT-855: Block arguments are automatically wrapped via
    /// [`generate_erlang_interop_wrapper`] to strip the Tier 2 `StateAcc` protocol.
    /// A diagnostic warning is emitted when a stateful block (one with captured
    /// mutations) crosses the Erlang boundary, since mutations will be dropped.
    fn generate_direct_erlang_call(
        &mut self,
        module_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        /// Object protocol selectors that must NOT be optimized as direct Erlang
        /// calls. These are inherited from ProtoObject/Object and handled by
        /// runtime dispatch. Selectors already handled as compiler intrinsics
        /// (class, isNil, notNil, hash, yourself, respondsTo:, error:) never
        /// reach here — they're intercepted earlier in the dispatch chain.
        const OBJECT_PROTOCOL_SELECTORS: &[&str] = &["printString", "asString", "inspect"];

        match selector {
            MessageSelector::Unary(function_name) => {
                if OBJECT_PROTOCOL_SELECTORS.contains(&function_name.as_str()) {
                    return Ok(None);
                }
                // BT-1127: Route zero-arg calls through proxy (consistent with keyword sends).
                // `Erlang erlang node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [])`
                let doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'(",
                    leaf::atom(module_name.to_string()),
                    ", ",
                    leaf::atom(function_name.to_string()),
                    ", [])"
                ];
                Ok(Some(doc))
            }
            MessageSelector::Keyword(parts) => {
                // Extract function name from first keyword (before the colon)
                let function_name = parts[0].keyword.trim_end_matches(':');

                // BT-855: Process arguments individually so Block arguments can be
                // wrapped via generate_erlang_interop_wrapper before crossing the
                // Erlang boundary. Non-block arguments pass through unchanged.
                let mut preamble_docs: Vec<Document<'static>> = Vec::new();
                let mut arg_parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());

                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        arg_parts.push(Document::Str(", "));
                    }
                    if let Some(block) = Self::extract_block_literal(arg) {
                        // BT-3151 review follow-up: a block crossing the Erlang
                        // interop boundary here goes through
                        // `generate_erlang_interop_wrapper` → `generate_block`,
                        // the same same-process, in-process closure mechanism as
                        // a `select:`/`do:` argument — see
                        // `check_no_unsafe_class_method_self_sends`'s doc
                        // comment.
                        let analysis = crate::core_erlang::block_analysis::analyze_block(block);
                        self.check_no_unsafe_class_method_self_sends(&analysis, block.span)?;
                        let (wrapped_doc, is_stateful) =
                            self.generate_erlang_interop_wrapper(block)?;
                        if is_stateful {
                            self.warn_stateful_block_at_erlang_boundary(
                                &format!("'{module_name}':'{function_name}'"),
                                block.span,
                            );
                        }
                        // Bind the wrapper to a temp var to avoid repeating complex exprs.
                        let wrapper_var = self.fresh_temp_var("ErlWrapper");
                        preamble_docs.push(docvec![
                            "let ",
                            leaf::var(wrapper_var.clone()),
                            " = ",
                            wrapped_doc,
                            " in "
                        ]);
                        arg_parts.push(leaf::var(wrapper_var));
                    } else {
                        arg_parts.push(self.expression_doc(arg)?);
                    }
                }

                // BT-1127: Route through beamtalk_erlang_proxy:direct_call/3 to
                // enable binary→charlist coercion for functions like os:cmd/1.
                // Args are wrapped in a list: call 'proxy':'direct_call'('M','F',[args])
                let call_doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'(",
                    leaf::atom(module_name.to_string()),
                    ", ",
                    leaf::atom(function_name.to_string()),
                    ", [",
                    Document::Vec(arg_parts),
                    "])"
                ];

                let doc = if preamble_docs.is_empty() {
                    call_doc
                } else {
                    docvec![Document::Vec(preamble_docs), call_doc]
                };

                Ok(Some(doc))
            }
            MessageSelector::Binary(_) => {
                // Binary operators on Erlang module proxy — fall through to runtime
                Ok(None)
            }
        }
    }

    /// BT-3018 / ADR 0109: lower `File open:do:` / `File open:mode:do:` to a
    /// direct call rather than a class send, so the block runs in the caller.
    ///
    /// A class send is a `gen_server:call` into the singleton class process, so
    /// the method body — *including the user's block* — executes there. For a
    /// block-scoped resource method that is three separate problems: the block
    /// cannot message `File` again (deadlock), it holds the class process for
    /// its whole duration (every `File` call in the node queues behind it), and
    /// it must finish inside the 60-second class-call timeout.
    ///
    /// Emitting the same `native_call` the class-method body would have emitted
    /// — just here, in the caller — removes all three. The Erlang side is
    /// unchanged: `beamtalk_file:open/2,3` still performs the open, the
    /// `try`/`after` and the close, so the intercepted path and the
    /// `perform:`-style dynamic path stay semantically identical.
    ///
    /// Deliberately a hard-coded selector list, per ADR 0109's "Not in scope":
    /// the general mechanism (a continuation protocol for any Block-taking class
    /// method) would change the hottest dispatch path in the language to benefit
    /// the ~1% of class methods that take a Block. A fourth block-scoped method
    /// is the trigger to revisit that.
    ///
    /// Scoped to the unqualified stdlib `File`: a package-qualified receiver
    /// (`mylib@File open: p do: blk`) is some other class that happens to share
    /// the name, and must keep its own implementation. Same reasoning as the
    /// `pkg.is_none()` guard on BT-773's self-send case below.
    fn try_generate_block_scoped_open(
        &mut self,
        class_name: &str,
        package: Option<&str>,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if class_name != "File" || package.is_some() {
            return Ok(None);
        }
        let selector_atom = selector.name().to_string();
        if !matches!(selector_atom.as_str(), "open:do:" | "open:mode:do:") {
            return Ok(None);
        }

        let mut arg_docs = Vec::with_capacity(arguments.len());
        for argument in arguments {
            arg_docs.push(self.expression_doc(argument)?);
        }
        let args = Self::join_docs_with_commas(arg_docs);

        // Mirrors `native_delegate_body_doc`: the Erlang shim name is the first
        // keyword without its colon, and the `{Class, Selector}` context makes a
        // wrapped error read `File>>open:mode:do:` rather than a bare MFA.
        Ok(Some(docvec![
            "call 'beamtalk_erlang_proxy':'native_call'(",
            leaf::atom("beamtalk_file"),
            ", ",
            leaf::atom("open"),
            ", [",
            args,
            "], {",
            leaf::atom("File"),
            ", ",
            leaf::atom(selector_atom),
            "})"
        ]))
    }

    /// Handles `ClassReference` receivers as class method calls.
    ///
    /// ADR 0019 Phase 3: In REPL context, checks REPL bindings first for
    /// convenience names (Transcript, Beamtalk, Workspace). If found in bindings,
    /// dispatches via `beamtalk_message_dispatch:send/3` (instance dispatch).
    /// Falls back to `class_send` for actual class names.
    ///
    /// In actor/value-type methods compiled in workspace mode, uses `class_send`
    /// with fallback to workspace binding for convenience names.
    ///
    /// Returns `Some(doc)` if the receiver is a `ClassReference`, `None` otherwise.
    fn try_handle_class_reference(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if let Expression::ClassReference { name, package, .. } = receiver {
            let pkg = package.as_ref().map(|p| p.name.as_str());
            // BT-3018 / ADR 0109: block-scoped `File open:…do:` must not reach
            // the File class gen_server, or the user's block runs there. Checked
            // ahead of every class-send path below, because the deadlock, the
            // serialization and the 60s class-call ceiling apply to all of them.
            if let Some(doc) =
                self.try_generate_block_scoped_open(&name.name, pkg, selector, arguments)?
            {
                return Ok(Some(doc));
            }
            // BT-773: When inside a class method and the explicit class name matches
            // the current class, use direct dispatch (same as `self` sends) to avoid
            // deadlock. The class actor is already processing the outer call, so
            // routing through class_send would deadlock on gen_server:call.
            if self.in_class_method() && name.name == self.class_name() && pkg.is_none() {
                // ADR 0118 phase 5a: the producer now returns a `ThreadedValue`;
                // convert back to the legacy open-Document + `last_open_scope_result`
                // contract this function's own caller (`generate_message_send`)
                // still expects.
                let tv = self.generate_class_method_self_send(selector, arguments)?;
                return Ok(Some(self.threaded_value_to_open_scope_doc(tv)));
            }
            if self.workspace_mode() && self.context == CodeGenContext::Repl {
                // REPL top-level: check session bindings first
                let doc =
                    self.generate_binding_aware_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            if self.workspace_mode() {
                // Actor/ValueType methods in workspace mode: try class_send,
                // fall back to workspace binding for convenience names
                let doc = self.generate_workspace_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            // ADR 0070 Phase 2: Class method calls always go through the class
            // registry using the short class name. The package qualifier doesn't
            // affect dispatch — it's used for module name resolution in spawns and
            // standalone references, not for class method calls.
            let doc = self.generate_class_method_call(&name.name, selector, arguments)?;
            return Ok(Some(doc));
        }
        Ok(None)
    }

    /// Handles self-sends inside actor methods (BT-330).
    ///
    /// Returns `Some(())` if the receiver is `self` in an Actor context, `None` otherwise.
    ///
    /// ADR 0118 phase 2b (BT-3418): every position that threads a
    /// dispatching self-send's `NewState` now compiles it through
    /// [`Self::generate_self_dispatch`]'s producer directly (via
    /// `threaded_expression`/`thread_ahead`), substituting the already-
    /// sequenced value via `precompiled_subexprs` before `generate_message_send`
    /// ever reaches this function — so a self-send that gets here always
    /// falls through to [`Self::generate_discarding_self_dispatch`], the
    /// same *discarding* dispatch every un-migrated position always used.
    fn try_handle_self_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if self.context == CodeGenContext::Actor {
            if let Expression::Identifier(id) = receiver {
                if id.name == "self" {
                    let doc = self.generate_discarding_self_dispatch(selector, arguments)?;
                    return Ok(Some(doc));
                }
            }
        }
        Ok(None)
    }

    /// BT-412: Handles self-sends in class method context.
    ///
    /// When a class method sends a message to `self` (the class object),
    /// we call the module function directly (not through `gen_server`) to avoid
    /// deadlock since class methods execute inside a `gen_server:call` handler.
    ///
    /// For user-defined class methods, generates `class_<selector>(ClassSelf, ClassVars, ...)`.
    /// For built-in exports (spawn, new, etc.), generates `module:selector(...)`.
    fn try_handle_class_method_self_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if !self.in_class_method() {
            return Ok(None);
        }
        if let Expression::Identifier(id) = receiver {
            if id.name == "self" {
                // ADR 0118 phase 5a (BT-3421): the producer now returns a
                // `ThreadedValue`; convert it back to the legacy open-Document
                // + `last_open_scope_result` contract this function's own
                // caller (`generate_message_send`) still expects — the ~80
                // open-scope consumers reached that way are ADR 0118 phase
                // 5b's to migrate, not this issue's.
                let tv = self.generate_class_method_self_send(selector, arguments)?;
                return Ok(Some(self.threaded_value_to_open_scope_doc(tv)));
            }
        }
        Ok(None)
    }

    /// Core logic for direct dispatch of class method calls.
    ///
    /// Used by both `self` sends and explicit class name sends (BT-773) within
    /// class methods. Generates direct module function calls to avoid deadlock
    /// since class methods execute inside a `gen_server:call` handler.
    ///
    /// ADR 0118 phase 5a (BT-3421): returns a [`ThreadedValue`] rather than
    /// an open `Document` + `last_open_scope_result` side write. Every
    /// branch's own class-var Bind (via [`Self::emit_class_var_result_unwrap`])
    /// is now a real prelude entry; branches with no class-var Bind of their
    /// own (instantiation intrinsics, reflective primitives, auto-exports,
    /// the slot constructor) still route through [`Self::finalize_dispatch_with_preamble`]
    /// (unmigrated — ADR 0118 phase 5b's to convert) and are adapted back
    /// via [`Self::legacy_doc_to_threaded_value`], which reads whatever that
    /// call left in `last_open_scope_result` (set when a NESTED sub-expression
    /// argument's own class-var producer opened a scope — this branch's own
    /// call never does).
    #[allow(clippy::too_many_lines)] // Multiple dispatch branches (BT-773/BT-893/BT-996/BT-2003/BT-2007) share args-capture scaffolding.
    pub(super) fn generate_class_method_self_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<ThreadedValue> {
        let selector_atom = selector.name().to_string();

        // ADR 0084 / BT-2267: inside a programmatic ClassBuilder class-method fun
        // there is no `class_<sel>` module export to call, so self-sends route
        // through the runtime dispatch helper (own runtime fun first, then the
        // super/inherited chain), threading ClassVars via the standard
        // `{class_var_result, …}` unwrap. Instantiation intrinsics (self new /
        // spawn) still use the process-dict-backed helpers (no export needed).
        if let Some(builder_class) = self.builder_class_method_class() {
            if let Some(doc) = self.try_instantiation_intrinsic(&selector_atom, arguments)? {
                return Ok(self.legacy_doc_to_threaded_value(doc));
            }
            let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
            let cv = self.current_class_var();
            let call_doc = docvec![
                "call 'beamtalk_class_dispatch':'class_self_dispatch_local'(",
                leaf::atom(builder_class),
                ", ",
                leaf::atom(selector_atom),
                ", ",
                leaf::var(cv),
                ", [",
                args_doc,
                "])"
            ];
            return Ok(self.emit_class_var_result_unwrap(args_preamble, call_doc));
        }

        if self.class_method_selectors().contains(&selector_atom) {
            // Route to class_<selector>(ClassSelf, ClassVars, ...)
            let module = self.module_name.clone();
            // BT-1937: Hoist any open let-chains from sub-expression class
            // method self-sends in the args. The preamble must be emitted
            // before our own `let _CMR = ...` so the ClassVarsN bindings it
            // produces stay in scope at the outer level. capture_args_with_preamble
            // does NOT roll back class_var_version, so the snapshot we take
            // afterwards (`cv`) reflects the post-args version — that is the
            // ClassVars binding to thread into the callee.
            let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
            let cv = self.current_class_var();
            let comma = if arguments.is_empty() { "" } else { ", " };

            // BT-1408 follow-up: apply the same atom-length guard used by the
            // keyword-constructor path below — long selectors must be hashed to
            // stay within Erlang's 255-char atom limit.
            let safe_fn = super::selector_mangler::safe_class_method_fn_name(&selector_atom);
            let call_doc = docvec![
                "call ",
                leaf::atom(module),
                ":",
                leaf::atom(safe_fn),
                "(ClassSelf, ",
                leaf::var(cv),
                comma,
                args_doc,
                ")"
            ];
            // NOTE: prelude is OPEN — caller splices or open-scope-converts it.
            return Ok(self.emit_class_var_result_unwrap(args_preamble, call_doc));
        }
        // BT-996: Auto-generated keyword constructor for Value subclass: classes.
        // `ClassName slot: value` inside a class method routes here when the selector
        // matches the auto-generated slot keyword constructor (e.g. `symName:` → `class_symName:/3`).
        // The constructor returns a plain map (not a `class_var_result` tuple), so no
        // class-var threading boilerplate is needed.
        if self
            .class_slot_constructor_selector()
            .map(String::as_str)
            .is_some_and(|kw| kw == selector_atom)
        {
            let module = self.module_name.clone();
            // BT-1937: Hoist preambles from sub-expression class var mutations
            // in the args. cv is read AFTER capture_args_with_preamble so it
            // reflects the post-args ClassVars version.
            let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
            let cv = self.current_class_var();
            let comma = if arguments.is_empty() { "" } else { ", " };
            // BT-1408: Hash long keyword constructor atoms to stay within
            // Erlang's 255-char atom limit.
            let safe_fn = super::selector_mangler::safe_class_method_fn_name(&selector_atom);
            let call_doc = docvec![
                "call ",
                leaf::atom(module),
                ":",
                leaf::atom(safe_fn),
                "(ClassSelf, ",
                leaf::var(cv),
                comma,
                args_doc,
                ")"
            ];
            let doc = self.finalize_dispatch_with_preamble(args_preamble, call_doc, "Slot");
            return Ok(self.legacy_doc_to_threaded_value(doc));
        }
        // BT-893: Instantiation selectors (new, new:, spawn, spawnWith:) must bypass
        // gen_server to avoid deadlock — route through class_self_new/class_self_spawn
        // (and BT-2004's class_self_spawn_as/class_self_spawn_with for the named-
        // registration variants).
        if let Some(doc) = self.try_instantiation_intrinsic(&selector_atom, arguments)? {
            return Ok(self.legacy_doc_to_threaded_value(doc));
        }

        // BT-3057: Behaviour-protocol reflective primitives (`superclass`,
        // `includesSelector:`, ...) are not compiled class exports — they are
        // `@primitive`-backed methods inherited from `Behaviour`/`Class` and
        // normally resolved via `try_class_chain_fallthrough`'s
        // `beamtalk_dispatch:lookup/5` walk. That walk needs a `ClassPid` that
        // isn't `self()` (it goes through `gen_server:call` for the class
        // method table), so it cannot run from inside the class's own
        // process. Route these selectors directly to the real
        // `beamtalk_behaviour_intrinsics` implementation instead, passing the
        // closure-captured `ClassSelf` as receiver — every entry here is
        // deadlock-safe specifically because its implementation resolves the
        // class module via `beamtalk_object_class:module_name_safe/1` (which
        // has a `ClassPid =:= self()` fast path reading the process
        // dictionary, BT-3054) and looks up class metadata from
        // `__beamtalk_meta/0` / ETS rather than calling back into this
        // process's own gen_server. `class_self_send_reflective_primitive`
        // must stay in sync with that safety property — do not add a
        // selector here whose intrinsic can call `gen_server:call(ClassPid,
        // ...)` unconditionally (e.g. `classSubclasses/1`,
        // `classAllSubclasses/1`, `className/1` as of this writing).
        if let Some(fun_name) =
            class_self_send_reflective_primitive(&selector_atom, arguments.len())
        {
            let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
            let comma = if arguments.is_empty() { "" } else { ", " };
            let call_doc = docvec![
                "call 'beamtalk_behaviour_intrinsics':",
                leaf::atom(fun_name),
                "(",
                leaf::var("ClassSelf"),
                comma,
                args_doc,
                ")"
            ];
            let doc = self.finalize_dispatch_with_preamble(
                args_preamble,
                call_doc,
                "ReflectivePrimitive",
            );
            return Ok(self.legacy_doc_to_threaded_value(doc));
        }

        // BT-2007: Inherited class method — walk the hierarchy at runtime and
        // apply the defining module's class_<sel>(ClassSelf, ClassVars, Args...).
        // The one remaining auto-generated 0-arity export reachable via plain
        // self-send (`class_name/0`) stays on the direct-call path because the
        // chain walker only looks at user-defined class_methods, and its
        // intrinsic (`className/1`) is not deadlock-safe from inside the
        // class's own process (see the reflective-primitive branch above) —
        // `class_name/0` sidesteps that by returning the raw atom directly
        // rather than going through the primitive. The other auto-exports
        // (`method_table/0`, `has_method/1`, `register_class/0`,
        // `__beamtalk_meta/0`) are codegen-internal reflection and metadata
        // accessors without a stable user-level API, so they are deliberately
        // NOT on the direct-call path — `is_class_auto_export_selector` must
        // stay in sync with the actual reachable set. Anything else that falls
        // through here — inherited or missing — routes through
        // class_self_dispatch/4, which raises a structured does_not_understand
        // error for genuine DNU.
        if is_class_auto_export_selector(&selector_atom, arguments.len()) {
            // BT-1937: Hoist preambles from sub-expression class var mutations.
            let module = self.module_name.clone();
            let fun_name = selector_atom.replace(':', "");
            let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;

            let call_doc = docvec![
                "call ",
                leaf::atom(module),
                ":",
                leaf::atom(fun_name),
                "(",
                args_doc,
                ")"
            ];
            let doc = self.finalize_dispatch_with_preamble(args_preamble, call_doc, "ClassFn");
            return Ok(self.legacy_doc_to_threaded_value(doc));
        }

        let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
        let cv = self.current_class_var();
        // BT-3047 / ADR 0109 amendment: derive the target class from `ClassSelf`
        // (closure-captured, so correct even when this self-send executes inside a
        // block running in a foreign class's process) instead of
        // `erlang:get('beamtalk_class_name')` (the *executing process's* identity,
        // which is only the same thing outside a block). `class_name_from_tag/1`
        // strips the `' class'` metaclass tag `element(2, ClassSelf)` carries.
        let call_doc = docvec![
            "call 'beamtalk_class_dispatch':'class_self_dispatch'(",
            "call 'beamtalk_primitive':'class_name_from_tag'(call 'erlang':'element'(2, ",
            leaf::var("ClassSelf"),
            ")), ",
            leaf::atom(selector_atom),
            ", ",
            leaf::var(cv),
            ", [",
            args_doc,
            "])"
        ];
        // NOTE: prelude is OPEN — caller splices or open-scope-converts it
        // (matches the local-class-method branch above).
        Ok(self.emit_class_var_result_unwrap(args_preamble, call_doc))
    }

    /// BT-3047 / ADR 0109 amendment: the class-name expression derived from
    /// `ClassSelf` (closure-captured, so correct even inside a block executing in
    /// a foreign class's process), for inlining at instantiation-intrinsic call
    /// sites. Deliberately inlined rather than let-bound: `finalize_dispatch_with_preamble`
    /// treats *any* non-`Nil` preamble as an open let-chain the caller must
    /// continue (setting `last_open_scope_result`), which only the argument-hoisting
    /// preamble from `capture_args_with_preamble` is guaranteed to be consumed
    /// correctly for — a zero-argument call (e.g. bare `self new`) produces a
    /// `Nil` args preamble and must stay a *closed* expression. Recomputing this
    /// cheap expression (a suffix check + `binary_to_existing_atom`) inline at
    /// each use — up to three times per call site for the `spawn`/`spawnAs:`/
    /// `spawnWith:as:` intrinsics, which also resolve `is_abstract` — is
    /// negligible and matches the pre-existing style at these same sites, which
    /// already repeated process-dictionary reads inline rather than hoisting
    /// them through a `let`.
    pub(super) fn class_self_name_doc() -> Document<'static> {
        docvec![
            "call 'beamtalk_primitive':'class_name_from_tag'(call 'erlang':'element'(2, ",
            leaf::var("ClassSelf"),
            "))"
        ]
    }

    /// BT-3047 / ADR 0109 amendment: the calling class's own compiled module,
    /// resolved by name via `beamtalk_class_metadata:lookup_module/1` — **not**
    /// `element(3, ClassSelf)` (`class_mod`). That field is not reliably "the
    /// calling class's own module": at the inherited-class-method dispatch site
    /// (`beamtalk_class_dispatch:apply_class_method_in_context/6`), `ClassSelf`
    /// is constructed with `class_mod = DefiningModule` (the ancestor whose code
    /// is executing), while `class` (used by `class_self_name_doc`) is correctly
    /// the calling subclass's own tag. Using `class_mod` here would construct an
    /// instance via the wrong module (e.g. `Point new: aMap` building a bare
    /// `Value`-shaped map missing `x`/`y` — caught as a regression while
    /// implementing this amendment). `Selector` names the call for the
    /// structured error `resolve_module_or_raise/2` raises on a metadata miss.
    pub(super) fn class_self_module_doc(selector_atom: &str) -> Document<'static> {
        docvec![
            "call 'beamtalk_class_instantiation':'resolve_module_or_raise'(",
            Self::class_self_name_doc(),
            ", ",
            leaf::atom(selector_atom),
            ")"
        ]
    }

    /// Lowers instantiation-like selectors (`new`, `spawn`, `spawnAs:`, ...) into
    /// direct calls on `beamtalk_class_instantiation`, bypassing the class
    /// `gen_server` to avoid deadlock from within a class method.
    ///
    /// BT-908: ClassName/Module create an instance of the CALLING class (the
    /// running class `gen_server` process) for inherited factory methods.
    ///
    /// BT-3047 / ADR 0109 amendment: ClassName/Module/IsAbstract are derived from
    /// `ClassSelf` (closure-captured) rather than read from the process
    /// dictionary, so a block invoked from a *different* class's process still
    /// resolves against the block's own lexical class — preserving BT-908's intent
    /// rather than overriding it (`ClassSelf` already carries the same value the
    /// process dictionary did in every non-block case).
    fn try_instantiation_intrinsic(
        &mut self,
        selector_atom: &str,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector_atom {
            "new" | "new:" => {
                // BT-1937: Hoist preambles from sub-expression class var mutations.
                let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
                let call_doc = docvec![
                    "call 'beamtalk_class_instantiation':'class_self_new'(",
                    Self::class_self_name_doc(),
                    ", ",
                    Self::class_self_module_doc(selector_atom),
                    ", [",
                    args_doc,
                    "])"
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    args_preamble,
                    call_doc,
                    "NewRes",
                )))
            }
            "spawn" | "spawnWith:" => {
                let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
                let call_doc = docvec![
                    "call 'beamtalk_class_instantiation':'class_self_spawn'(",
                    Self::class_self_name_doc(),
                    ", ",
                    Self::class_self_module_doc(selector_atom),
                    ", call 'beamtalk_class_instantiation':'resolve_is_abstract_or_raise'(",
                    Self::class_self_name_doc(),
                    ", ",
                    leaf::atom(selector_atom),
                    "), [",
                    args_doc,
                    "])"
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    args_preamble,
                    call_doc,
                    "SpawnRes",
                )))
            }
            // BT-2004: Named-registration spawn variants inherited from Actor.
            // Without these arms, the fallthrough in the caller emits
            // `call 'CURRENT_MODULE':'spawnAs' / 'spawnWithas'` — neither function
            // exists, so calls crash at runtime with `undef`.
            "spawnAs:" => Ok(Some(self.generate_class_self_named_spawn(
                "class_self_spawn_as",
                "SpawnAsRes",
                "spawnAs:",
                arguments,
            )?)),
            "spawnWith:as:" => Ok(Some(self.generate_class_self_named_spawn(
                "class_self_spawn_with",
                "SpawnWithAsRes",
                "spawnWith:as:",
                arguments,
            )?)),
            _ => Ok(None),
        }
    }

    /// BT-2004: Shared emitter for `self spawnAs:` and `self spawnWith:as:` in
    /// class-method context. Emits a call to `beamtalk_class_instantiation`'s
    /// Result-returning helper with ClassName/Module/IsAbstract derived from
    /// `ClassSelf` (BT-3047 / ADR 0109 amendment — see `try_instantiation_intrinsic`),
    /// followed by the Beamtalk-level arguments.
    fn generate_class_self_named_spawn(
        &mut self,
        helper: &'static str,
        result_prefix: &'static str,
        selector_atom: &'static str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
        let call_doc = docvec![
            "call 'beamtalk_class_instantiation':'",
            Document::Str(helper),
            "'(",
            Self::class_self_name_doc(),
            ", ",
            Self::class_self_module_doc(selector_atom),
            ", call 'beamtalk_class_instantiation':'resolve_is_abstract_or_raise'(",
            Self::class_self_name_doc(),
            ", ",
            leaf::atom(selector_atom),
            "), ",
            args_doc,
            ")"
        ];
        Ok(self.finalize_dispatch_with_preamble(args_preamble, call_doc, result_prefix))
    }

    /// Generates synchronous self-dispatch for actor self-sends (BT-330).
    ///
    /// When an actor method sends a message to `self`, we bypass the async
    /// `gen_server:cast` path and call `safe_dispatch/3` directly. This ensures
    /// the result is a value (not a Future), enabling recursive algorithms like
    /// factorial and fibonacci to work correctly.
    ///
    /// # Sealed Class Optimization (BT-403)
    ///
    /// ADR 0118 §Decision 2 (BT-3415): the state-effecting *producer* for
    /// an Actor self-send — the one place a dispatching self-send is
    /// compiled in a state-threading context. Returns a [`ThreadedValue`]
    /// whose prelude is
    ///
    /// ```text
    /// Statement(let _SDn = case call 'm':'safe_dispatch'('sel', [Args], StateK) of … end in)
    /// Bind(State{K+1} <- call 'erlang':'element'(2, _SDn))
    /// ```
    ///
    /// and whose value is `call 'erlang':'element'(1, _SDn)` — exactly the
    /// `Statement` + real `Bind` pair `dispatch_self_send_as_bind`
    /// (`control_flow/conditionals.rs`) built for the planner, now owned
    /// here; that function is a thin adapter over
    /// [`Self::generate_self_dispatch_parts`] since BT-3415.
    ///
    /// `arguments` are compiled by
    /// [`Self::generate_self_dispatch_call_doc_for`] exactly as before; a
    /// caller that must sequence state-effecting *arguments* ahead of the
    /// dispatch (the ADR §Decision 3 rule) does so before calling this —
    /// see `threaded_expression` (`util.rs`).
    ///
    /// `frame` is the [`FrameId`] the `Bind` belongs to — `FrameId::ROOT`
    /// for the flat Actor method body, a branch arm's own frame otherwise;
    /// `span` is the source span both prelude nodes are attributed to.
    pub(super) fn generate_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
        frame: FrameId,
        span: Span,
    ) -> Result<ThreadedValue> {
        let (prelude, dispatch_var) =
            self.generate_self_dispatch_parts(selector, arguments, frame, span)?;
        Ok(ThreadedValue {
            prelude,
            value: Self::self_dispatch_result_value(&dispatch_var),
        })
    }

    /// The pure `element(1, _SDn)` reference to a self-dispatch's reply —
    /// the single spelling shared by [`Self::generate_self_dispatch`]'s
    /// value and [`Self::try_handle_self_dispatch`]'s planner substitution.
    pub(super) fn self_dispatch_result_value(dispatch_var: &str) -> ValueRef {
        ValueRef::Doc(docvec![
            "call 'erlang':'element'(1, ",
            leaf::var(dispatch_var.to_string()),
            ")",
        ])
    }

    /// The `Statement` + `Bind` pair behind [`Self::generate_self_dispatch`],
    /// plus the dispatch tuple's variable name for callers that register
    /// it (the planner's `hoisted_self_send_results`). Mint order is
    /// unchanged from `dispatch_self_send_as_bind`: the dispatch temps and
    /// the `next_state_var` bump all happen inside
    /// [`Self::generate_self_dispatch_call_doc_for`], and the `Bind`'s
    /// source/target versions are read off the live counter either side
    /// of it.
    pub(super) fn generate_self_dispatch_parts(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
        frame: FrameId,
        span: Span,
    ) -> Result<(Vec<ThreadedStmt>, String)> {
        let source_version = self.state_version();
        let (call_doc, dispatch_var) =
            self.generate_self_dispatch_call_doc_for(selector, arguments)?;
        let target_version = self.state_version();
        let prelude = vec![
            ThreadedStmt::Statement(call_doc, span),
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'erlang':'element'(2, ",
                    leaf::var(dispatch_var.clone()),
                    ")",
                ])),
                shadow_write: false,
                span,
            },
        ];
        Ok((prelude, dispatch_var))
    }

    /// For sealed classes, we skip the `safe_dispatch/3` try/catch overhead and
    /// call `dispatch/4` directly. Since sealed classes have all methods known at
    /// compile time, the error isolation overhead is unnecessary.
    ///
    /// # Generated Code (normal)
    ///
    /// ```erlang
    /// case call 'module':'safe_dispatch'('selector', [Args], State) of
    ///   <{'reply', Result, _NewState}> when 'true' -> Result
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end
    /// ```
    ///
    /// # Generated Code (sealed class)
    ///
    /// ```erlang
    /// let Self = call 'beamtalk_actor':'make_self'(State) in
    /// case call 'module':'dispatch'('selector', [Args], Self, State) of
    ///   <{'reply', Result, _NewState}> when 'true' -> Result
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end
    /// ```
    ///
    /// ADR 0118 (BT-3415): this is the *discarding* form — the `NewState`
    /// the dispatch returns is dropped. It is reached only from
    /// [`Self::try_handle_self_dispatch`]'s fallback, i.e. for a self-send
    /// in a position no consumer has yet migrated to
    /// [`Self::generate_self_dispatch`]'s prelude form (a cascade message,
    /// a `match:` scrutinee, a `sort:` comparator, an interpolation
    /// segment after an effectful one, …) and that no planner pre-hoisted.
    /// Every such position is a row of
    /// `stdlib/test/actor_self_send_position_matrix_test.bt` gated on the
    /// ADR 0118 phase that migrates it; once phase 2b removes the last
    /// `Document`-only consumer this fallback becomes
    /// `generate_self_dispatch(..).close(.., CloseContext::Opaque)` and the
    /// drop a [`super::threaded_ir::VerifyError::StateEffectEscapesExpression`]
    /// (§Decision 5). Byte-identical to the pre-ADR-0118 output by
    /// construction, so un-migrated positions are untouched.
    fn generate_discarding_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // BT-403: Sealed class optimization — skip safe_dispatch try/catch
        if self.is_class_sealed() {
            return self.generate_sealed_self_dispatch(selector, arguments);
        }

        let selector_atom = selector.name().to_string();
        let result_var = self.fresh_var("SelfResult");
        let state_var = self.fresh_var("SelfState");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let error_clause = self.generate_self_dispatch_error_clause("SelfError", &selector_atom);

        let doc = docvec![
            "case call ",
            leaf::atom(module),
            ":'safe_dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            leaf::var(current_state),
            ") of ",
            "<{'reply', ",
            leaf::var(result_var.clone()),
            ", ",
            leaf::var(state_var),
            "}> when 'true' -> ",
            leaf::var(result_var),
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// BT-245: Generates self-dispatch with state threading (open binding pattern).
    ///
    /// Like `generate_self_dispatch`, but captures the new state from the dispatch
    /// result and advances the state version. The let binding is left open so
    /// subsequent expressions see the updated state.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _SD0 = case call 'module':'safe_dispatch'('sel', [Args], State) of
    ///   <{'reply', R, S}> when 'true' -> {R, S}
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end in let State1 = call 'erlang':'element'(2, _SD0) in
    /// ```
    ///
    /// The expression value `call 'erlang':'element'(1, _SD0)` is NOT emitted —
    /// it's discarded since this is used for non-last expressions in block bodies.
    ///
    /// Uses Document/docvec! (ADR 0018) for composable rendering.
    pub(super) fn generate_self_dispatch_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            return self.generate_self_dispatch_open_for(selector, arguments);
        }
        Err(CodeGenError::Internal(
            "generate_self_dispatch_open called on non-MessageSend expression".to_string(),
        ))
    }

    /// BT-3382: selector/arguments-based counterpart of
    /// [`Self::generate_self_dispatch_open`], for callers that only have the
    /// decomposed selector/arguments of a self-send, not the owning
    /// `Expression::MessageSend` node itself — e.g.
    /// `control_flow/conditionals.rs`'s `compile_conditional_receiver`, which
    /// destructures a conditional's (unwrapped-of-parens) receiver expression
    /// itself. `generate_self_dispatch_open` is now a thin wrapper over this.
    pub(super) fn generate_self_dispatch_open_for(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<(Document<'static>, String)> {
        let (call_doc, dispatch_var) =
            self.generate_self_dispatch_call_doc_for(selector, arguments)?;
        let new_state = self.current_state_var();
        let doc = docvec![
            call_doc,
            "let ",
            leaf::var(new_state),
            " = call 'erlang':'element'(2, ",
            leaf::var(dispatch_var.clone()),
            ") in "
        ];
        Ok((doc, dispatch_var))
    }

    /// ADR 0111 Addendum 5 (BT-3165, shape E2): the dispatch-call/
    /// case-clause portion of [`Self::generate_self_dispatch_open`],
    /// WITHOUT the trailing state-extraction `let` — factored out so
    /// `exception_handling.rs`'s per-arm `ThreadedIr` lowering can model the
    /// state-version bump as a real `Bind` (`BindOp::Direct`) instead of
    /// baking it into an opaque `Statement`'s text. Still mints the version
    /// bump itself (`next_state_var()`, right before
    /// `generate_self_dispatch_error_clause`, exactly where the un-split
    /// function always minted it) so every mint *after* this call keeps its
    /// original position — callers that need the bumped state's rendered
    /// name read it back via `current_state_var()` rather than consuming a
    /// return value, since a real `Bind`'s `render_bind` re-derives the same
    /// name from the version number, not from a string this function hands
    /// back. `generate_self_dispatch_open` is unchanged in every other
    /// respect (same mint order, same returned bytes) — it now simply
    /// delegates here and appends the extraction `let` it used to build
    /// inline.
    pub(super) fn generate_self_dispatch_call_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            return self.generate_self_dispatch_call_doc_for(selector, arguments);
        }
        Err(CodeGenError::Internal(
            "generate_self_dispatch_call_doc called on non-MessageSend expression".to_string(),
        ))
    }

    /// BT-3382: selector/arguments-based core of
    /// [`Self::generate_self_dispatch_call_doc`] — see that function's doc
    /// comment (ADR 0111 Addendum 5 / BT-3165) for the shape this builds and
    /// why the state-version bump (`next_state_var()`) mints exactly where it
    /// does. Factored out so [`Self::generate_self_dispatch_open_for`] (used
    /// by `compile_conditional_receiver`, which only has the decomposed
    /// selector/arguments of an unwrapped-of-parens receiver expression, not
    /// an owning `MessageSend` node) can reuse the exact same dispatch-call/
    /// state-threading shape instead of re-deriving a parallel
    /// implementation (CLAUDE.md's no-duplicate-implementations rule).
    #[allow(clippy::too_many_lines)] // Document-based sealed/normal dispatch branches
    pub(super) fn generate_self_dispatch_call_doc_for(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<(Document<'static>, String)> {
        {
            let selector_atom = selector.name().to_string();
            // BT-2822: `selector_atom` is moved into `call_doc` below (some
            // branches consume it via `leaf::atom`), so clone the value
            // needed for the error-clause breadcrumb before that happens.
            let selector_atom_for_error = selector_atom.clone();
            let dispatch_var = self.fresh_temp_var("SD");
            let result_var = self.fresh_var("SDResult");
            let state_var = self.fresh_var("SDState");
            let current_state = self.current_state_var();

            // Capture arguments via bridge (ADR 0018 Phase 0)
            let args_doc = self.capture_argument_list_doc(arguments)?;

            // Build the dispatch call (varies by sealed optimization level)
            let call_doc = if self.is_class_sealed() {
                let selector_name = selector.name().to_string();
                if self.sealed_method_selectors().contains(&selector_name) {
                    // Level 1: Direct __sealed_ call
                    let self_var = self.fresh_temp_var("SealedSelf");
                    let module = self.module_name.clone();
                    let comma = if arguments.is_empty() { "" } else { ", " };
                    docvec![
                        "let ",
                        leaf::var(self_var.clone()),
                        " = call 'beamtalk_actor':'make_self'(",
                        leaf::var(current_state.clone()),
                        ") in ",
                        "let ",
                        leaf::var(dispatch_var.clone()),
                        " = case call ",
                        leaf::atom(module),
                        ":",
                        leaf::atom(super::selector_mangler::sealed_fn_name(&selector_name)),
                        "(",
                        args_doc,
                        Document::Str(comma),
                        leaf::var(self_var),
                        ", ",
                        leaf::var(current_state),
                        ") of ",
                    ]
                } else {
                    // Level 2: Direct dispatch/4 call
                    let self_var = self.fresh_temp_var("SealedSelf");
                    let module = self.module_name.clone();
                    docvec![
                        "let ",
                        leaf::var(self_var.clone()),
                        " = call 'beamtalk_actor':'make_self'(",
                        leaf::var(current_state.clone()),
                        ") in ",
                        "let ",
                        leaf::var(dispatch_var.clone()),
                        " = case call ",
                        leaf::atom(module),
                        ":'dispatch'(",
                        leaf::atom(selector_atom),
                        ", [",
                        args_doc,
                        "], ",
                        leaf::var(self_var),
                        ", ",
                        leaf::var(current_state),
                        ") of ",
                    ]
                }
            } else {
                // Normal: safe_dispatch/3
                let module = self.module_name.clone();
                docvec![
                    "let ",
                    leaf::var(dispatch_var.clone()),
                    " = case call ",
                    leaf::atom(module),
                    ":'safe_dispatch'(",
                    leaf::atom(selector_atom),
                    ", [",
                    args_doc,
                    "], ",
                    leaf::var(current_state),
                    ") of ",
                ]
            };

            // Result/error clauses. BT-3165: the state-version bump
            // (`next_state_var()`) stays exactly here — mint-order fidelity
            // — but its returned name is no longer consumed for rendering;
            // `generate_self_dispatch_open` re-reads it via
            // `current_state_var()`, and the E2 `Bind`-based caller
            // (`exception_handling.rs`) re-derives it from the version
            // number via `render_bind`.
            let _ = self.next_state_var();
            let error_clause =
                self.generate_self_dispatch_error_clause("SDError", &selector_atom_for_error);
            let doc = docvec![
                call_doc,
                "<{'reply', ",
                leaf::var(result_var.clone()),
                ", ",
                leaf::var(state_var.clone()),
                "}> when 'true' -> {",
                leaf::var(result_var),
                ", ",
                leaf::var(state_var),
                "} ",
                error_clause,
                "end in ",
            ];

            Ok((doc, dispatch_var))
        }
    }

    /// BT-403: Sealed-class self-dispatch (value-discarding — see
    /// `generate_self_dispatch`'s call site).
    ///
    /// Two levels of optimization:
    /// 1. **Known sealed method**: Direct function call to `__sealed_{selector}`,
    ///    bypassing both `safe_dispatch/3` and `dispatch/4` case matching.
    /// 2. **Unknown method** (inherited): Direct `dispatch/4` call, skipping
    ///    only the `safe_dispatch/3` try/catch overhead.
    fn generate_sealed_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_name = selector.name().to_string();

        // Level 1: Direct call to standalone sealed method function
        if self.sealed_method_selectors().contains(&selector_name) {
            let selector_atom = selector.name().to_string();
            return self.generate_direct_sealed_call(&selector_name, &selector_atom, arguments);
        }

        // Level 2: Direct dispatch/4 call (skip safe_dispatch try/catch)
        let selector_atom = selector.name().to_string();
        let result_var = self.fresh_var("SealedResult");
        let self_var = self.fresh_temp_var("SealedSelf");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let error_clause = self.generate_self_dispatch_error_clause("SealedError", &selector_atom);

        let doc = docvec![
            "let ",
            leaf::var(self_var.clone()),
            " = call 'beamtalk_actor':'make_self'(",
            leaf::var(current_state.clone()),
            ") in ",
            "case call ",
            leaf::atom(module),
            ":'dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            leaf::var(self_var),
            ", ",
            leaf::var(current_state),
            ") of ",
            "<{'reply', ",
            leaf::var(result_var.clone()),
            ", _}> when 'true' -> ",
            leaf::var(result_var),
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// Generates a direct call to a sealed method's standalone function (BT-403).
    ///
    /// This is the most optimized self-dispatch path: calls `__sealed_{selector}`
    /// directly, bypassing `safe_dispatch`, dispatch, and case selector matching.
    fn generate_direct_sealed_call(
        &mut self,
        selector_name: &str,
        selector_atom: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let result_var = self.fresh_var("SealedResult");
        let self_var = self.fresh_temp_var("SealedSelf");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let comma = if arguments.is_empty() { "" } else { ", " };
        // BT-2822: `selector_atom` (from `MessageSelector::name`) is
        // the breadcrumb value — kept independent of `selector_name` (used
        // below for `sealed_fn_name` mangling) so a future change to either
        // mangling scheme can't silently desync the breadcrumb from the
        // dispatch atom used at the other call sites.
        let error_clause =
            self.generate_self_dispatch_error_clause("SealedDirectError", selector_atom);

        let doc = docvec![
            "let ",
            leaf::var(self_var.clone()),
            " = call 'beamtalk_actor':'make_self'(",
            leaf::var(current_state.clone()),
            ") in ",
            "case call ",
            leaf::atom(module),
            ":",
            leaf::atom(super::selector_mangler::sealed_fn_name(selector_name)),
            "(",
            args_doc,
            Document::Str(comma),
            leaf::var(self_var),
            ", ",
            leaf::var(current_state),
            ") of ",
            "<{'reply', ",
            leaf::var(result_var.clone()),
            ", _}> when 'true' -> ",
            leaf::var(result_var),
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }
    ///
    /// This is used to detect state mutations that require threading through
    /// control flow constructs.
    pub(super) fn is_field_assignment(expr: &Expression) -> bool {
        if let Expression::Assignment { target, .. } = expr {
            if let Expression::FieldAccess { receiver, .. } = target.as_ref() {
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    return recv_id.name == "self";
                }
            }
        }
        false
    }

    /// BT-2797: Checks if an expression is a self-field access (`self.field`).
    ///
    /// Used to scope the runtime Tier 1/Tier 2 discrimination for block value
    /// calls (`self.field value: ...`) to exactly the shape that needs it — a
    /// block stored in an instance field, whose Tier-ness can't be known
    /// statically since it may have been assigned from a different method.
    pub(super) fn is_self_field_access(expr: &Expression) -> bool {
        if let Expression::FieldAccess { receiver, .. } = expr {
            if let Expression::Identifier(recv_id) = receiver.as_ref() {
                return recv_id.name == "self";
            }
        }
        false
    }

    /// Checks if an expression is a class variable assignment (`self.classVar := value`).
    pub(super) fn is_class_var_assignment(&self, expr: &Expression) -> bool {
        if !self.in_class_method() {
            return false;
        }
        if let Expression::Assignment { target, .. } = expr {
            if let Expression::FieldAccess {
                receiver, field, ..
            } = target.as_ref()
            {
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    return recv_id.name == "self"
                        && self.class_var_names().contains(field.name.as_str());
                }
            }
        }
        false
    }

    /// Checks if an expression is a self-send to a class method (BT-412).
    /// These need special scoping in class method bodies because they may
    /// update `ClassVars` via `let ClassVarsN = ... in` which must not be wrapped.
    pub(super) fn is_class_method_self_send(&self, expr: &Expression) -> bool {
        if !self.in_class_method() || self.class_method_selectors().is_empty() {
            return false;
        }
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            if let Expression::Identifier(id) = receiver.as_ref() {
                if id.name == "self" {
                    let sel_atom = selector.name().to_string();
                    return self.class_method_selectors().contains(&sel_atom);
                }
            }
        }
        false
    }

    /// Checks if an expression is a local variable assignment (`identifier := value`).
    pub(super) fn is_local_var_assignment(expr: &Expression) -> bool {
        if let Expression::Assignment { target, .. } = expr {
            matches!(target.as_ref(), Expression::Identifier(_))
        } else {
            false
        }
    }

    /// Checks if an expression is a super message send (`super methodName:`).
    pub(super) fn is_super_message_send(expr: &Expression) -> bool {
        if let Expression::MessageSend { receiver, .. } = expr {
            matches!(receiver.as_ref(), Expression::Super(_))
        } else {
            false
        }
    }

    /// BT-245: Checks if an expression is a self-send in actor context.
    /// These may mutate actor state and need state threading in loop bodies.
    /// BT-920: Excludes cast sends (`self method!`), which are fire-and-forget
    /// and must not thread state through the loop accumulator.
    pub(super) fn is_actor_self_send(&self, expr: &Expression) -> bool {
        if self.context != super::CodeGenContext::Actor {
            return false;
        }
        if let Expression::MessageSend {
            receiver, is_cast, ..
        } = expr
        {
            if *is_cast {
                return false;
            }
            if let Expression::Identifier(id) = receiver.as_ref() {
                return id.name == "self";
            }
        }
        false
    }

    /// BT-1420: Checks if an expression is a self-send that goes through `safe_dispatch`
    /// (or sealed dispatch) and returns `{reply, Result, NewState}`.
    ///
    /// Excludes self-sends with selectors that are intercepted by handlers before
    /// `try_handle_self_dispatch` in `generate_message_send`:
    /// - Binary operators (`+`, `-`, `*`, etc.)
    /// - `asType:` (compile-time erasure)
    /// - `ProtoObject` messages (`class`, `perform:`, `perform:withArguments:`)
    /// - Object reflection (`fieldAt:`, `fieldAt:put:`, `fieldNames`, `respondsTo:`)
    /// - Nil protocol (`isNil`, `notNil`, `ifNil:`, etc.)
    /// - Identity (`yourself`, `hash`)
    /// - Error signaling (`error:`)
    /// - Block evaluation (`value`, `value:`, `repeat`, `whileTrue:`, etc.)
    pub(super) fn is_dispatching_actor_self_send(&self, expr: &Expression) -> bool {
        if !self.is_actor_self_send(expr) {
            return false;
        }
        if let Expression::MessageSend { selector, .. } = expr {
            return Self::selector_dispatches_via_self(selector);
        }
        true
    }

    /// The selector half of [`Self::is_dispatching_actor_self_send`]'s
    /// check — extracted (ADR 0118 phase 1b, BT-3416) so a caller that
    /// already knows the receiver is a bare `self` without owning an
    /// `Expression::MessageSend` node to hand back (a cascade message,
    /// whose selector/arguments come from `CascadeMessage` — see
    /// `util.rs`'s `cascade_self_dispatch_messages`) can reuse the exact
    /// same rule instead of copying it (CLAUDE.md: no duplicate
    /// implementations).
    ///
    /// Excludes selectors that are intercepted by handlers before
    /// `try_handle_self_dispatch` in `generate_message_send`:
    /// - Binary operators (`+`, `-`, `*`, etc.)
    /// - `asType:` (compile-time erasure)
    /// - `ProtoObject` messages (`class`, `perform:`, `perform:withArguments:`)
    /// - Object reflection (`fieldAt:`, `fieldAt:put:`, `fieldNames`, `respondsTo:`)
    /// - Nil protocol (`isNil`, `notNil`, `ifNil:`, etc.)
    /// - Identity (`yourself`, `hash`)
    /// - Error signaling (`error:`)
    /// - Block evaluation (`value`, `value:`, `repeat`, `whileTrue:`, etc.)
    pub(super) fn selector_dispatches_via_self(selector: &MessageSelector) -> bool {
        // Binary operators are always intercepted by generate_binary_op
        if matches!(selector, MessageSelector::Binary(_)) {
            return false;
        }
        // BT-2065/BT-2071/BT-2073: Well-known selectors that the intrinsics
        // layer **unconditionally** handles before `try_handle_self_dispatch`.
        // Covers ProtoObject (`class`, `perform:`/`perform:withArguments:`/
        // `performLocally:withArguments:`), Object reflection (`respondsTo:`,
        // `fieldAt:`, `fieldAt:put:`, `fieldNames`), Nil protocol
        // (`isNil`/`notNil`/`ifNil:`/`ifNotNil:`/`ifNil:ifNotNil:`/
        // `ifNotNil:ifNil:`), exception handling (`on:do:`, `ensure:`),
        // block application (`value`/`value:`/`value:value:`/
        // `value:value:value:`), block loops (`repeat`/`whileTrue:`/
        // `whileFalse:`), object identity (`hash`) and error signaling
        // (`error:`).
        //
        // NOTE: Boolean conditionals (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`)
        // are NOT included here — `try_generate_boolean_protocol` returns
        // `Ok(None)` (falls through) when no mutation-threading is needed,
        // allowing the send to reach self-dispatch.
        if let Some(wk) = selector.well_known() {
            if matches!(
                wk,
                WellKnownSelector::Class
                    | WellKnownSelector::RespondsTo
                    | WellKnownSelector::IsNil
                    | WellKnownSelector::NotNil
                    | WellKnownSelector::IfNil
                    | WellKnownSelector::IfNotNil
                    | WellKnownSelector::IfNilIfNotNil
                    | WellKnownSelector::IfNotNilIfNil
                    | WellKnownSelector::OnDo
                    | WellKnownSelector::Value
                    | WellKnownSelector::ValueColon
                    | WellKnownSelector::ValueValue
                    | WellKnownSelector::ValueValueValue
                    | WellKnownSelector::WhileTrue
                    | WellKnownSelector::WhileFalse
                    | WellKnownSelector::Repeat
                    | WellKnownSelector::Ensure
                    | WellKnownSelector::Hash
                    | WellKnownSelector::Error
                    | WellKnownSelector::FieldAt
                    | WellKnownSelector::FieldAtPut
                    | WellKnownSelector::FieldNames
                    | WellKnownSelector::Perform
                    | WellKnownSelector::PerformWithArgs
                    | WellKnownSelector::PerformLocallyWithArgs
            ) {
                return false;
            }
        }
        // Remaining intrinsics not modelled as `WellKnownSelector` variants
        // — these are class-specific or compile-time-only constructs that
        // do not warrant universal selector classification.
        let name = selector.name();
        if matches!(
            name.as_str(),
            // asType: (compile-time erasure)
            "asType:"
            // Identity
            | "yourself"
        ) {
            return false;
        }
        true
    }

    /// Checks if an expression is an `error:` message send.
    ///
    /// Since `erlang:error/1` never returns (always throws an exception),
    /// expressions ending with `error:` should not be wrapped in reply tuples.
    pub(super) fn is_error_message_send(expr: &Expression) -> bool {
        // BT-2073: classify via the well-known enum so a future rename of the
        // `Error` variant forces this site to update too. The classifier
        // guarantees keyword/arity = 1, but we still gate on arguments.len()
        // for the same defensive reason the original predicate did.
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            return matches!(selector.well_known(), Some(WellKnownSelector::Error))
                && arguments.len() == 1;
        }
        false
    }

    /// BT-2797: Generates the RHS `Document` for a `self.field := value`
    /// assignment, special-casing a block literal with field writes (and no
    /// captured-local mutations): it's generated via `generate_block_stateful`
    /// directly, bypassing `generate_block`'s "unsupported block" rejection.
    ///
    /// This is safe unconditionally for the field-writes-only case — no
    /// same-method-only safety analysis is needed here, unlike the local-var
    /// case in `gen_server/methods.rs`'s `prescan_tier2_local_vars` — because
    /// every `self.field value(:...)` call site now runtime-discriminates
    /// Tier 1 vs Tier 2 (`generate_block_value_call_runtime_discriminated`,
    /// `intrinsics.rs`), regardless of which method performs the call. The
    /// residual gap — reading the field into a local var and invoking *that*
    /// — is the same pre-existing class of gap as any other block value
    /// flowing through an untracked opaque channel.
    ///
    /// BT-2797 (PR #2899 review fix): a block that *also* captures and
    /// mutates an outer local (in addition to writing a field) is NOT safe
    /// here and must fall through to `expression_doc` → `generate_block` →
    /// `validate_stored_closure`/the block-analyzer diagnostic instead.
    /// `generate_block_stateful` reads a captured local's
    /// `'__local__<var>'` key from the calling method's `StateAcc`, falling
    /// back to the value closed over at block-*definition* time when that
    /// key is absent. A field-stored block can be invoked from a different
    /// method than the one that stored it, so that fallback fires forever
    /// (stale definition-time value) and, once the returned state is merged
    /// back into the actor's persistent state, the `'__local__<var>'` key
    /// leaks into it permanently. See the matching fix in
    /// `semantic_analysis/block_analyzer.rs`.
    ///
    /// Scoped to `Actor` context only, matching the call-site fix: `ValueType`
    /// field-write Tier 2 support has not been verified safe.
    pub(super) fn generate_field_assignment_value_doc(
        &mut self,
        value: &Expression,
    ) -> Result<Document<'static>> {
        if self.context == CodeGenContext::Actor {
            if let Expression::Block(block) = value {
                let captured_mutations = Self::captured_mutations_for_block(block);
                let field_writes = super::block_analysis::analyze_block(block).field_writes;
                if captured_mutations.is_empty() && !field_writes.is_empty() {
                    return self.generate_block_stateful(block, &[]);
                }
            }
        }
        self.expression_doc(value)
    }

    /// Generates the opening part of a field assignment with state threading.
    ///
    /// For `self.field := value`, generates:
    /// ```erlang
    /// let _Val = <value> in
    /// let StateN = call 'maps':'put'('field', _Val, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression (generating the body
    /// that uses the new state).
    ///
    /// BT-3180: the plain-`State` branch below carries `ThreadedIr`
    /// instrumentation (`check_simple_field_bind_invariant`, reused from
    /// `expressions.rs`) around its `next_state_var()` mint — chosen over
    /// promoting the mint to a real `Bind` (like the class-var branch
    /// already does): this function's `Document` is hand-built and returned
    /// directly to 7 different call sites with their own surrounding glue
    /// (loop bodies, conditional arms, intrinsics), so replacing it with a
    /// `ThreadedStmt::Bind` would touch every one of those emission paths
    /// and require re-verifying the whole snapshot corpus for a version-mint
    /// site that was never actually producing wrong output — instrumentation
    /// only, matching BT-3139's precedent for this construct family.
    pub(super) fn generate_field_assignment_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                // BT-3168 (ADR 0111 Addendum 9, Questions 2/3): a class-var
                // write directly inside a Letrec loop body that threads
                // `ClassVars` through the loop's own recursive tail call —
                // threaded via the SAME shared helper the method's own
                // top-frame write uses (`lower_class_var_field_assignment_bind`),
                // but tagged with the loop's real, already-minted frame
                // (`current_branch_frame()`) instead of `FrameId::ROOT`, per
                // Question 2's resolution. `loop_threads_class_vars` scopes
                // this to exactly the Letrec loop-body call path — see its
                // own doc comment for why it can never leak into a nested
                // Foldl body, conditional, or block literal (all of which
                // still hit `reject_class_var_field_assignment` below,
                // unchanged).
                if self.is_class_var_assignment(expr) && self.loop_threads_class_vars {
                    let frame = self.current_branch_frame();
                    let (preamble_doc, bind, val_var) =
                        self.lower_class_var_field_assignment_bind(&field.name, value, frame)?;
                    let bind_doc = {
                        let mut ctx = super::threaded_ir::RenderCtx::new(self);
                        super::threaded_ir::render(std::slice::from_ref(&bind), &mut ctx)
                    };
                    return Ok((docvec![preamble_doc, bind_doc], val_var));
                }
                self.reject_class_var_field_assignment(expr, field)?;
                // BT-1342: Full-extract mode — rebind field param instead of maps:put.
                // When the field is in hybrid_mutated_fields, the field has been extracted
                // to a direct fun parameter. We rebind it to a fresh variable and update
                // the readonly params map so subsequent reads use the new variable.
                if self.in_hybrid_loop && self.hybrid_mutated_fields.contains(field.name.as_str()) {
                    let val_var = self.fresh_temp_var("Val");
                    // Snapshot field params before evaluating RHS so nested field
                    // assignments (e.g. `self.x := (self.y := 42)`) don't leak
                    // inner updates past the outer assignment.
                    let saved_field_params = self.hybrid_readonly_field_params.clone();
                    let val_doc = self.expression_doc(value)?;
                    self.hybrid_readonly_field_params = saved_field_params;
                    let new_field_var = self
                        .fresh_temp_var(&format!("{}Field", Self::to_core_erlang_var(&field.name)));
                    // Update the param map so subsequent reads use the new var.
                    self.hybrid_readonly_field_params
                        .insert(field.name.to_string(), new_field_var.clone());
                    return Ok((
                        docvec![
                            "let ",
                            leaf::var(val_var.clone()),
                            " = ",
                            val_doc,
                            " in let ",
                            leaf::var(new_field_var),
                            " = ",
                            leaf::var(val_var.clone()),
                            " in ",
                        ],
                        val_var,
                    ));
                }

                let val_var = self.fresh_temp_var("Val");
                let current_state = self.current_state_var();
                let source_state_version = self.state_version();
                let val_doc = self.generate_field_assignment_value_doc(value)?;

                let new_state = self.next_state_var();
                let target_state_version = self.state_version();
                // BT-3180: this "open" (non-last-position) sibling of
                // `generate_field_assignment`'s plain-State branch had no
                // `ThreadedIr` instrumentation around its `next_state_var()`
                // mint — most of this function's call sites sit outside any
                // backfilled `Vec<ThreadedStmt>` body sequence, so nothing
                // else ever isolated-verifies this version step.
                self.check_simple_field_bind_invariant(
                    super::threaded_ir::VersionPrefix::State,
                    source_state_version,
                    target_state_version,
                    "actor State open field-assignment version bind",
                    value.span(),
                );

                let doc = docvec![
                    "let ",
                    leaf::var(val_var.clone()),
                    " = ",
                    val_doc,
                    " in let ",
                    leaf::var(new_state),
                    " = call 'maps':'put'(",
                    leaf::atom(field.name.clone()),
                    ", ",
                    leaf::var(val_var.clone()),
                    ", ",
                    leaf::var(current_state),
                    ") in ",
                ];

                // BT-884: Return the val var so callers (e.g. cascade codegen) can
                // reference the assigned value after hoisting the binding.
                return Ok((doc, val_var));
            }
        }
        Err(CodeGenError::Internal(
            "generate_field_assignment_open called on non-field-assignment expression".to_string(),
        ))
    }

    /// BT-1324: Checks if an expression is `self fieldAt: <name> put: <value>` in actor context.
    /// These need state threading via maps:put, similar to field assignments.
    pub(super) fn is_self_field_at_put(&self, expr: &Expression) -> bool {
        if self.context != super::CodeGenContext::Actor {
            return false;
        }
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            if let Expression::Identifier(id) = receiver.as_ref() {
                // BT-2073: classify via the well-known enum. The classifier
                // already guarantees the two-part keyword shape; arguments.len()
                // is checked defensively for parser-shape consistency.
                if id.name == "self"
                    && self.lookup_var("self").is_none()
                    && matches!(selector.well_known(), Some(WellKnownSelector::FieldAtPut))
                    && arguments.len() == 2
                {
                    return true;
                }
            }
        }
        false
    }

    /// BT-1324: Generates the opening part of a `self fieldAt: name put: value` with state threading.
    ///
    /// Similar to `generate_field_assignment_open` but with a dynamic field name.
    /// Generates:
    /// ```erlang
    /// let _Name = <name> in
    /// let _Val = <value> in
    /// let StateN = call 'maps':'put'(_Name, _Val, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression.
    pub(super) fn generate_self_field_at_put_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend { arguments, .. } = expr {
            let name_var = self.fresh_var("Name");
            let val_var = self.fresh_temp_var("Val");
            let name_code = self.expression_doc(&arguments[0])?;
            // Capture state before value expression, consistent with
            // generate_field_assignment_open. If the value expression itself
            // threads state (e.g., contains a nested field assignment), the
            // maps:put uses the pre-value state — same semantics as self.x := expr.
            let current_state = self.current_state_var();
            let val_code = self.expression_doc(&arguments[1])?;
            let new_state = self.next_state_var();

            let doc = docvec![
                "let ",
                leaf::var(name_var.clone()),
                " = ",
                name_code,
                " in let ",
                leaf::var(val_var.clone()),
                " = ",
                val_code,
                " in let ",
                leaf::var(new_state),
                " = call 'maps':'put'(",
                leaf::var(name_var),
                ", ",
                leaf::var(val_var.clone()),
                ", ",
                leaf::var(current_state),
                ") in ",
            ];

            return Ok((doc, val_var));
        }
        Err(CodeGenError::Internal(
            "generate_self_field_at_put_open called on non-fieldAt:put: expression".to_string(),
        ))
    }

    /// Generates code for a super message send.
    ///
    /// Super calls use `beamtalk_dispatch:super/5` to invoke the superclass
    /// implementation via hierarchy walking (ADR 0006).
    ///
    /// # Example
    ///
    /// ```beamtalk
    /// super increment
    /// super getValue
    /// super at: 1 put: value
    /// ```
    ///
    /// Generates:
    ///
    /// ```erlang
    /// call 'beamtalk_dispatch':'super'('increment', [], Self, State, 'Counter')
    /// call 'beamtalk_dispatch':'super'('getValue', [], Self, State, 'Counter')
    /// call 'beamtalk_dispatch':'super'('at:put:', [1, Value], Self, State, 'Counter')
    /// ```
    pub(super) fn generate_super_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();

        // ADR 0084 / BT-2267: `super` inside a builder class-method fun resolves
        // up the metaclass chain via the runtime helper, keyed on the builder
        // class name — `class_self_dispatch/4` begins the walk at that class's
        // superclass, which is exactly super semantics. The fun has no module
        // export, so this must not use the compiled `beamtalk_dispatch:super/5`
        // instance path below.
        if let Some(builder_class) = self.builder_class_method_class() {
            let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
            let cv = self.current_class_var();
            let call_doc = docvec![
                "call 'beamtalk_class_dispatch':'class_self_dispatch'(",
                leaf::atom(builder_class),
                ", ",
                leaf::atom(selector_atom),
                ", ",
                leaf::var(cv),
                ", [",
                args_doc,
                "])"
            ];
            // ADR 0118 phase 5a: the producer now returns a `ThreadedValue`;
            // convert it back to the legacy open-Document +
            // `last_open_scope_result` contract this function's own callers
            // still expect.
            let tv = self.emit_class_var_result_unwrap(args_preamble, call_doc);
            return Ok(self.threaded_value_to_open_scope_doc(tv));
        }

        let class_name = self.class_name();
        let args_doc = self.capture_argument_list_doc(arguments)?;

        // BT-2252: value/primitive-context funs (`fun(Args, Self) -> Result`)
        // have no `State` binding, so `super` must not reference one. Route to
        // `super_value/4`, which walks the same chain and returns a plain value.
        if self.context == CodeGenContext::ValueType {
            return Ok(docvec![
                "call 'beamtalk_dispatch':'super_value'(",
                leaf::atom(selector_atom),
                ", [",
                args_doc,
                "], Self, ",
                leaf::atom(class_name),
                ")",
            ]);
        }

        let current_state = self.current_state_var();
        let doc = docvec![
            "call 'beamtalk_dispatch':'super'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], Self, ",
            leaf::var(current_state),
            ", ",
            leaf::atom(class_name),
            ")",
        ];
        Ok(doc)
    }

    /// Generates code for actor spawn with conditional REPL registry integration.
    ///
    /// When spawning an actor in the REPL, check for `__repl_actor_registry__` in
    /// bindings and register the spawned actor. In all cases, the module's own
    /// `spawn/0` or `spawn/1` is called (which handles initialize protocol).
    /// In non-REPL contexts (regular code, tests), fall back to normal `module:spawn`.
    ///
    /// The emitted module atom is computed dynamically via `compiled_module_name`:
    /// - Package mode: `bt@{package}@{class}` (e.g. `bt@my_pkg@counter`)
    /// - Workspace/legacy mode: `bt@{class}` (e.g. `bt@counter`)
    ///
    /// # Arguments
    ///
    /// * `class_name` - The Beamtalk class name (e.g., "Counter")
    /// * `init_args` - Optional initialization arguments for spawnWith:
    ///
    /// # Generated Code (REPL context, package mode with package `my_pkg`)
    ///
    /// ```erlang
    /// case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of
    ///   <'undefined'> when 'true' ->
    ///     call 'bt@my_pkg@counter':'spawn'()
    ///   <RegistryPid> when 'true' ->
    ///     let SpawnResult = call 'bt@my_pkg@counter':'spawn'() in
    ///     let {'beamtalk_object', _, _, SpawnPid} = SpawnResult in
    ///     let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, 'Counter', 'bt@my_pkg@counter') in
    ///     SpawnResult
    /// end
    /// ```
    ///
    /// # Generated Code (non-REPL context, package mode with package `my_pkg`)
    ///
    /// ```erlang
    /// call 'bt@my_pkg@counter':'spawn'()
    /// ```
    /// Generates actor spawn with optional package qualifier (ADR 0070 Phase 2).
    ///
    /// When `package` is `Some`, uses `resolve_qualified_module_name` to compute
    /// the BEAM module name directly (e.g., `json@Parser` → `bt@json@parser`).
    /// When `None`, falls back to `compiled_module_name` for standard resolution.
    ///
    /// In REPL context, registers the spawned actor with the REPL actor registry.
    /// In non-REPL contexts, calls the module's `spawn/0` or `spawn/1` directly.
    pub(super) fn generate_actor_spawn_qualified(
        &mut self,
        class_name: &str,
        package: Option<&str>,
        init_args: Option<&Expression>,
    ) -> Result<Document<'static>> {
        let module_name = self.compiled_module_name_qualified(class_name, package);
        let in_repl_context = self.lookup_var("__bindings__").is_some();

        let args_doc = match init_args {
            Some(args) => self.expression_doc(args)?,
            None => Document::Nil,
        };

        if in_repl_context {
            let doc = docvec![
                "case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of ",
                "<'undefined'> when 'true' -> call ",
                leaf::atom(module_name.clone()),
                ":'spawn'(",
                args_doc.clone(),
                ") <RegistryPid> when 'true' -> let SpawnResult = call ",
                leaf::atom(module_name.clone()),
                ":'spawn'(",
                args_doc,
                ") in ",
                "let SpawnPid = call 'erlang':'element'(4, SpawnResult) in ",
                "let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, ",
                leaf::atom(class_name.to_string()),
                ", ",
                leaf::atom(module_name),
                ") in ",
                "SpawnResult ",
                "end"
            ];
            Ok(doc)
        } else {
            let doc = docvec!["call ", leaf::atom(module_name), ":'spawn'(", args_doc, ")"];
            Ok(doc)
        }
    }

    /// Generates a method lookup via `>>` operator (BT-101).
    ///
    /// `Counter >> #increment` compiles to:
    /// ```erlang
    /// call 'beamtalk_method_resolver':'resolve'('Counter', 'increment')
    /// ```
    ///
    /// Returns a `CompiledMethod` map with selector, source, and arity metadata.
    fn generate_method_lookup(
        &mut self,
        class_name: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        let arg_doc = self.expression_doc(&arguments[0])?;
        let doc = docvec![
            "call 'beamtalk_method_resolver':'resolve'(",
            leaf::atom(class_name.to_string()),
            ", ",
            arg_doc,
            ")"
        ];
        Ok(doc)
    }

    /// Generates a runtime method resolution via `>>` for non-class-literal receivers (BT-323).
    ///
    /// `cls >> #increment` (where cls holds a class object) compiles to:
    /// ```erlang
    /// call 'beamtalk_method_resolver':'resolve'(cls, 'increment')
    /// ```
    ///
    /// The `MethodResolver` domain service accepts pids, atoms, and class object tuples.
    fn generate_runtime_method_lookup(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        // BT-1937: Capture receiver + arg as one ordered sequence so
        // left-to-right evaluation order is preserved.
        let exprs: [&Expression; 2] = [receiver, &arguments[0]];
        let (preamble, mut docs) = self.capture_subexpr_sequence(&exprs, "Lookup")?;
        let arg_doc = docs.pop().expect("arg");
        let actual_receiver = docs.pop().expect("receiver");

        let call_doc = docvec![
            "call 'beamtalk_method_resolver':'resolve'(",
            actual_receiver,
            ", ",
            arg_doc,
            ")"
        ];
        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "MethodLookup"))
    }

    /// Generates a binding-aware class method call (ADR 0019 Phase 3).
    ///
    /// In workspace mode, checks REPL bindings first for convenience names.
    /// If the name is found in bindings, it's an instance (e.g., Transcript is a
    /// `TranscriptStream` actor), so dispatch via `beamtalk_message_dispatch:send/3`.
    /// If not found, fall back to direct call (BT-1639) or `class_send`.
    ///
    /// ```erlang
    /// case call 'maps':'find'('Name', State) of
    ///   <{'ok', BindingVal}> -> call 'beamtalk_message_dispatch':'send'(BindingVal, Sel, Args)
    ///   <'error'> -> call 'module':'class_selector'('nil', ~{}~, Args)  %% BT-1639 direct
    ///                %% OR: class_send fallback for non-eligible classes
    /// end
    /// ```
    fn generate_binding_aware_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // BT-1408: The binding branch dispatches to instances via
        // beamtalk_message_dispatch:send — use the raw selector (only hashed
        // if the selector itself exceeds the atom limit) so instance method
        // lookup works normally.  The class_send fallback uses the class-method
        // mangled selector which triggers earlier (when "class_" + selector
        // exceeds the limit).
        let raw = selector.name().to_string();
        let instance_selector = super::selector_mangler::safe_atom_name(&raw);
        let binding_val_var = self.fresh_var("BindingVal");
        let state_var = self.current_state_var();
        let lookup_var = self.fresh_temp_var("Lookup");

        // BT-1942: Preserve the "receiver first, then args" evaluation order
        // expected by Smalltalk/Beamtalk message-send semantics. The receiver
        // here is the class-binding lookup (`maps:find(ClassName, State)`),
        // which we bind to a temp BEFORE the arg preamble runs so a dispatch
        // whose class name is unresolved still evaluates the lookup first.
        // We then bind arguments to temp vars so they are evaluated exactly
        // once (fixing a pre-existing double-compilation of `args_doc` in both
        // `case` branches) and so open let-chains from class method self-sends
        // propagate to the surrounding scope.
        let (arg_preamble, arg_refs, any_open_scope) =
            self.bind_args_to_temps(arguments, "BindArg")?;
        let args_doc = Self::join_docs_with_commas(arg_refs);

        // BT-1639: Build the class-side fallback: direct call or gen_server
        let class_fallback: Document<'static> =
            if let Some(info) = self.direct_call_eligible.get(class_name) {
                if info.selectors.contains(&raw) {
                    let safe_fn = super::selector_mangler::safe_class_method_fn_name(&raw);
                    let comma = if arguments.is_empty() { "" } else { ", " };
                    docvec![
                        "call ",
                        leaf::atom(info.module_name.clone()),
                        ":",
                        leaf::atom(safe_fn),
                        "('nil', ~{}~",
                        comma,
                        args_doc.clone(),
                        ")"
                    ]
                } else {
                    self.generate_class_send_fallback(class_name, &raw, args_doc.clone())
                }
            } else {
                self.generate_class_send_fallback(class_name, &raw, args_doc.clone())
            };

        // BT-2365 (ADR 0081 Phase 1): resolve the receiver — session locals first,
        // then lazy singleton resolution — BEFORE the arg preamble runs, so the
        // receiver is fully determined ahead of any argument side effects (the
        // "receiver first, then args" evaluation order). Singletons
        // (Transcript/Beamtalk/Workspace) are no longer eagerly injected into the
        // session map, so `Workspace bind:as:` would otherwise mis-route to a
        // non-existent `Workspace` class. resolve_singleton_instance/1 returns
        // `{ok, Instance}` for a singleton name and `error` for any other name,
        // so real class names (`Counter someClassMethod`) still fall through to
        // class_fallback.
        //
        // The combined lookup binds `Lookup` to `{ok, Receiver}` (from locals or
        // singleton registry) or `error` (use class-side fallback):
        //
        //   let Lookup = case maps:find(ClassName, State) of
        //                  {ok, V} -> {ok, V}
        //                  error   -> resolve_singleton_instance(ClassName)
        //                end
        //   in <arg_preamble>
        //   case Lookup of
        //     {ok, Receiver} -> beamtalk_message_dispatch:send(Receiver, Sel, Args)
        //     error          -> class_fallback
        //   end
        let singleton_val_var = self.fresh_var("SingletonVal");
        let lookup_binding = docvec![
            "let ",
            leaf::var(lookup_var.clone()),
            " = case call 'maps':'find'(",
            leaf::atom(class_name.to_string()),
            ", ",
            leaf::var(state_var),
            ") of ",
            "<{'ok', ",
            leaf::var(singleton_val_var.clone()),
            "}> when 'true' -> {'ok', ",
            leaf::var(singleton_val_var),
            "} ",
            "<'error'> when 'true' -> call 'beamtalk_workspace':'resolve_singleton_instance'(",
            leaf::atom(class_name.to_string()),
            ") ",
            "end in ",
        ];
        let case_doc = docvec![
            "case ",
            leaf::var(lookup_var),
            " of ",
            "<{'ok', ",
            leaf::var(binding_val_var.clone()),
            "}> when 'true' -> ",
            "call 'beamtalk_message_dispatch':'send'(",
            leaf::var(binding_val_var),
            ", ",
            leaf::atom(instance_selector),
            ", [",
            args_doc,
            "]) ",
            "<'error'> when 'true' -> ",
            class_fallback,
            " end"
        ];

        // BT-1942: Propagate open scope upward if any arg mutated class vars.
        if any_open_scope {
            let result_var = self.fresh_temp_var("BindClassRes");
            let doc = docvec![
                lookup_binding,
                arg_preamble,
                "let ",
                leaf::var(result_var.clone()),
                " = ",
                case_doc,
                " in ",
            ];
            self.last_open_scope_result = Some(OpenScopeResult::Value(result_var));
            Ok(doc)
        } else {
            Ok(docvec![lookup_binding, arg_preamble, case_doc])
        }
    }

    /// Generates workspace-mode class send for actor/value-type methods.
    ///
    /// BT-1639: For sealed classes eligible for direct call, generates a direct
    /// function call instead of `gen_server` dispatch. Otherwise tries `class_send`
    /// first (for real class names like `Counter`), returns nil for unresolved names.
    /// ADR 0019 Phase 4: No `persistent_term` fallback — convenience names resolve
    /// via session bindings in REPL context.
    fn generate_workspace_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let raw_selector = selector.name().to_string();

        // BT-1639: Direct call optimization for sealed class methods
        if let Some(info) = self.direct_call_eligible.get(class_name) {
            if info.selectors.contains(&raw_selector) {
                return self.generate_direct_class_method_call(
                    &info.module_name.clone(),
                    &raw_selector,
                    arguments,
                );
            }
        }

        // BT-1408: Hash long selector atoms to stay within Erlang's 255-char atom limit.
        let selector_atom = super::selector_mangler::safe_class_method_selector(&raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        let lookup_var = self.fresh_temp_var("WsLookup");
        // BT-1942: Bind the class registry lookup to a temp BEFORE evaluating
        // args, preserving "receiver first, then args" message-send semantics.
        // Then bind args to temp vars so they are evaluated once and their open
        // let-chains propagate upward.
        let (arg_preamble, arg_refs, any_open_scope) =
            self.bind_args_to_temps(arguments, "WsArg")?;
        let args_doc = Self::join_docs_with_commas(arg_refs);

        let lookup_binding = docvec![
            "let ",
            leaf::var(lookup_var.clone()),
            " = call 'beamtalk_class_registry':'whereis_class'(",
            leaf::atom(class_name.to_string()),
            ") in ",
        ];
        let case_doc = docvec![
            "case ",
            leaf::var(lookup_var),
            " of ",
            "<'undefined'> when 'true' -> 'nil' ",
            "<",
            leaf::var(class_pid_var.clone()),
            "> when 'true' -> ",
            "call 'beamtalk_object_class':'class_send'(",
            leaf::var(class_pid_var),
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "]) end"
        ];

        if any_open_scope {
            let result_var = self.fresh_temp_var("WsClassRes");
            let doc = docvec![
                lookup_binding,
                arg_preamble,
                "let ",
                leaf::var(result_var.clone()),
                " = ",
                case_doc,
                " in ",
            ];
            self.last_open_scope_result = Some(OpenScopeResult::Value(result_var));
            Ok(doc)
        } else {
            Ok(docvec![lookup_binding, arg_preamble, case_doc])
        }
    }

    /// Generates a class-level method call (BT-215).
    ///
    /// For sealed classes with no class variables (BT-1639), generates a direct
    /// function call to `module:class_<selector>(nil, #{}, Args...)`, bypassing
    /// the `gen_server` round-trip. This is safe because the methods are pure functions.
    ///
    /// For all other classes (or unrecognized selectors), falls back to the
    /// `gen_server` dispatch path via `beamtalk_object_class:class_send/3`.
    ///
    /// # Generated Code (direct call, BT-1639)
    ///
    /// ```erlang
    /// call 'bt@stdlib@tracing':'class_setContext:'('nil', ~{}~, Ctx)
    /// ```
    ///
    /// # Generated Code (`gen_server` fallback)
    ///
    /// ```erlang
    /// let ClassPid = call 'beamtalk_class_registry':'whereis_class'('Tracing') in
    /// call 'beamtalk_object_class':'class_send'(ClassPid, 'setContext:', [Ctx])
    /// ```
    fn generate_class_method_call(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let raw_selector = selector.name().to_string();

        // BT-1639: Check if this class method is eligible for direct call optimization.
        if let Some(info) = self.direct_call_eligible.get(class_name) {
            if info.selectors.contains(&raw_selector) {
                return self.generate_direct_class_method_call(
                    &info.module_name.clone(),
                    &raw_selector,
                    arguments,
                );
            }
        }

        // Fallback: gen_server dispatch via class_send
        // BT-1408: Hash long selector atoms (e.g. keyword constructors with many
        // fields) to stay within Erlang's 255-char atom limit.
        // BT-1937: Hoist preambles from sub-expression class var mutations.
        let selector_atom = super::selector_mangler::safe_class_method_selector(&raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;

        let call_doc = docvec![
            "let ",
            leaf::var(class_pid_var.clone()),
            " = call 'beamtalk_class_registry':'whereis_class'(",
            leaf::atom(class_name.to_string()),
            ") in ",
            "call 'beamtalk_object_class':'class_send'(",
            leaf::var(class_pid_var),
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])"
        ];

        Ok(self.finalize_dispatch_with_preamble(args_preamble, call_doc, "ClassCall"))
    }

    /// BT-1639: Generates a direct function call to a sealed class method.
    ///
    /// Passes `nil` for `ClassSelf` and `#{}` for `ClassVars` since sealed classes
    /// with no class variables never reference these parameters.
    ///
    /// ```erlang
    /// call 'module':'class_<selector>'('nil', #{}, Args...)
    /// ```
    fn generate_direct_class_method_call(
        &mut self,
        module_name: &str,
        selector: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // BT-1408: Hash long selector atoms to stay within Erlang's 255-char atom limit.
        // BT-1937: Hoist preambles from sub-expression class var mutations.
        let safe_fn = super::selector_mangler::safe_class_method_fn_name(selector);
        let (args_preamble, args_doc) = self.capture_args_with_preamble(arguments)?;
        let comma = if arguments.is_empty() { "" } else { ", " };

        // Core Erlang empty map is ~{}~ (not #{} which is Erlang source syntax)
        let call_doc = docvec![
            "call ",
            leaf::atom(module_name.to_string()),
            ":",
            leaf::atom(safe_fn),
            "('nil', ~{}~",
            comma,
            args_doc,
            ")"
        ];

        Ok(self.finalize_dispatch_with_preamble(args_preamble, call_doc, "DirectCall"))
    }

    /// BT-1639: Generates the `gen_server` `class_send` fallback for binding-aware dispatch.
    ///
    /// Used when a class method is not eligible for direct call optimization.
    fn generate_class_send_fallback(
        &mut self,
        class_name: &str,
        raw_selector: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        let class_selector = super::selector_mangler::safe_class_method_selector(raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        docvec![
            "let ",
            leaf::var(class_pid_var.clone()),
            " = call 'beamtalk_class_registry':'whereis_class'(",
            leaf::atom(class_name.to_string()),
            ") in ",
            "call 'beamtalk_object_class':'class_send'(",
            leaf::var(class_pid_var),
            ", ",
            leaf::atom(class_selector),
            ", [",
            args_doc,
            "])"
        ]
    }

    /// BT-851: Pre-scans a class for self-sends that pass Tier 2 (stateful) block arguments.
    ///
    /// Walks all method bodies looking for `self <selector>: args` where an argument
    /// is a literal block with captured mutations (`captured_reads ∩ local_writes` non-empty).
    /// Records the target method selector and parameter position in `tier2_method_info`.
    pub(super) fn scan_class_for_tier2_blocks(
        &mut self,
        class: &beamtalk_core::ast::ClassDefinition,
    ) {
        use super::block_analysis::analyze_block;

        // Clear previous class's info to avoid cross-class pollution in multi-class modules
        self.tier2_method_info.clear();

        for method in &class.methods {
            for stmt in &method.body {
                self.scan_expr_for_tier2(&stmt.expression, &analyze_block);
            }
        }
    }

    /// BT-851: Recursively scans an expression for Tier 2 block arguments in self-sends.
    fn scan_expr_for_tier2(
        &mut self,
        expr: &Expression,
        analyze: &dyn Fn(
            &beamtalk_core::ast::Block,
        ) -> super::block_analysis::BlockMutationAnalysis,
    ) {
        match expr {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                // Check for self-sends
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if id.name == "self" {
                        let sel_name = selector.name().to_string();
                        for (i, arg) in arguments.iter().enumerate() {
                            if let Expression::Block(block) = arg {
                                let analysis = analyze(block);
                                let has_captured_mutations = analysis
                                    .local_writes
                                    .intersection(&analysis.captured_reads)
                                    .next()
                                    .is_some();
                                // BT-1140: Also promote blocks with field writes to Tier 2.
                                let has_field_writes = !analysis.field_writes.is_empty();
                                if has_captured_mutations || has_field_writes {
                                    self.tier2_method_info
                                        .entry(sel_name.clone())
                                        .or_default()
                                        .push(i);
                                }
                            }
                        }
                    }
                }
                // Recurse into receiver and arguments
                self.scan_expr_for_tier2(receiver, analyze);
                for arg in arguments {
                    self.scan_expr_for_tier2(arg, analyze);
                }
            }
            Expression::Assignment { target, value, .. } => {
                self.scan_expr_for_tier2(target, analyze);
                self.scan_expr_for_tier2(value, analyze);
            }
            Expression::Block(block) => {
                for body_stmt in &block.body {
                    self.scan_expr_for_tier2(&body_stmt.expression, analyze);
                }
            }
            Expression::Return { value, .. } => {
                self.scan_expr_for_tier2(value, analyze);
            }
            Expression::Parenthesized { expression, .. } => {
                self.scan_expr_for_tier2(expression, analyze);
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // Detect cascaded self-sends as Tier 2 call sites
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if id.name == "self" {
                        for msg in messages {
                            let sel_name = msg.selector.name().to_string();
                            for (i, arg) in msg.arguments.iter().enumerate() {
                                if let Expression::Block(block) = arg {
                                    let analysis = analyze(block);
                                    let has_captured_mutations = analysis
                                        .local_writes
                                        .intersection(&analysis.captured_reads)
                                        .next()
                                        .is_some();
                                    // BT-1140: Also promote blocks with field writes to Tier 2.
                                    let has_field_writes = !analysis.field_writes.is_empty();
                                    if has_captured_mutations || has_field_writes {
                                        self.tier2_method_info
                                            .entry(sel_name.clone())
                                            .or_default()
                                            .push(i);
                                    }
                                }
                            }
                        }
                    }
                }
                // Recurse into receiver and arguments
                self.scan_expr_for_tier2(receiver, analyze);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.scan_expr_for_tier2(arg, analyze);
                    }
                }
            }
            Expression::Match { value, arms, .. } => {
                self.scan_expr_for_tier2(value, analyze);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.scan_expr_for_tier2(guard, analyze);
                    }
                    self.scan_expr_for_tier2(&arm.body, analyze);
                }
            }
            _ => {}
        }
    }

    /// BT-851: Checks if an expression is a self-send with Tier 2 block arguments.
    ///
    /// Returns the captured-mutated variable names for each Tier 2 block argument
    /// if this is a Tier 2 self-send, or `None` if it's a regular self-send.
    ///
    /// BT-870: Also promotes literal Tier 1 blocks at call sites where the target
    /// method is a known Tier 2 HOM (present in `tier2_method_info`). A promoted
    /// block is compiled with the Tier 2 signature (`fun(Args, StateAcc) -> {Result, StateAcc}`)
    /// even though it has no captured mutations, ensuring the callee's arity expectation is met.
    pub(super) fn detect_tier2_self_send(
        &self,
        expr: &Expression,
    ) -> Option<Vec<(usize, Vec<String>)>> {
        use super::block_analysis::analyze_block;

        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            if let Expression::Identifier(id) = receiver.as_ref() {
                if id.name == "self" {
                    let sel_name = selector.name().to_string();
                    // BT-870: Collect positions the scanner identified as Tier 2 for this selector.
                    let hom_positions: std::collections::HashSet<usize> = self
                        .tier2_method_info
                        .get(&sel_name)
                        .map(|positions| positions.iter().copied().collect())
                        .unwrap_or_default();

                    let mut tier2_args = Vec::new();
                    for (i, arg) in arguments.iter().enumerate() {
                        if let Expression::Block(block) = arg {
                            let analysis = analyze_block(block);
                            let captured_mutations: Vec<String> = analysis
                                .local_writes
                                .intersection(&analysis.captured_reads)
                                .cloned()
                                .collect::<std::collections::BTreeSet<_>>()
                                .into_iter()
                                .collect();
                            if !captured_mutations.is_empty() {
                                tier2_args.push((i, captured_mutations));
                            } else if !analysis.field_writes.is_empty() {
                                // BT-1140: Field-write block — promote to Tier 2 with no local
                                // vars. The actor State IS the StateAcc; field reads/writes
                                // are threaded through it automatically inside the block body.
                                tier2_args.push((i, vec![]));
                            } else if hom_positions.contains(&i) {
                                // BT-870: Block has no mutations but this position is a known
                                // Tier 2 HOM param. Promote to Tier 2 with empty captured vars
                                // so it gets `fun(Args, StateAcc) -> {Result, StateAcc}` signature
                                // (StateAcc passthrough), matching the callee's arity expectation.
                                tier2_args.push((i, vec![]));
                            }
                        } else if let Expression::Identifier(arg_id) = arg {
                            // BT-912: If the argument is an identifier that is a known Tier 2
                            // block parameter of the current method, treat it as a Tier 2 HOM
                            // argument with no captured mutations. This handles nested HOMs where
                            // one method delegates a Tier 2 block to another (e.g.
                            // `outerEachItem: aBlock => self eachItem: aBlock`). The block was
                            // already compiled as Tier 2 by the outer caller; we need to ensure
                            // the returned state is threaded back through the delegation chain.
                            if self.tier2_block_params.contains(arg_id.name.as_str()) {
                                tier2_args.push((i, vec![]));
                            }
                        }
                    }
                    if !tier2_args.is_empty() {
                        return Some(tier2_args);
                    }
                }
            }
        }
        None
    }

    /// BT-851: Generates a self-dispatch with Tier 2 block arguments and state threading.
    ///
    /// Before the self-send:
    /// 1. Packs captured-mutated locals into State
    /// 2. Generates block arguments with Tier 2 stateful signature
    ///
    /// After the self-send:
    /// 1. Extracts captured-mutated locals from the returned State
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let State1 = call 'maps':'put'('__local__count', Count, State) in
    /// let _SD0 = case call 'module':'safe_dispatch'('applyBlock:to:',
    ///     [fun (X, StateAcc) -> ... {Result, StateAcc1} end, 5], State1) of
    ///   <{'reply', R, S}> when 'true' -> {R, S}
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end in let State2 = call 'erlang':'element'(2, _SD0) in
    /// let Count = call 'maps':'get'('__local__count', State2) in
    /// ```
    pub(super) fn generate_tier2_self_send_open(
        &mut self,
        expr: &Expression,
        tier2_args: &[(usize, Vec<String>)],
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            let mut docs: Vec<Document<'static>> = Vec::new();

            // Step 1: Pack captured-mutated locals into State
            for (_pos, captured_vars) in tier2_args {
                for var_name in captured_vars {
                    let core_var = self
                        .lookup_var(var_name)
                        .cloned()
                        .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                    let key = Self::local_state_key(var_name);
                    let current_state = self.current_state_var();
                    let new_state = self.next_state_var();
                    docs.push(docvec![
                        "let ",
                        leaf::var(new_state),
                        " = call 'maps':'put'(",
                        leaf::atom(key),
                        ", ",
                        leaf::var(core_var),
                        ", ",
                        leaf::var(current_state),
                        ") in "
                    ]);
                }
            }

            // Step 2: Generate argument list with Tier 2 blocks
            let selector_atom = selector.name().to_string();
            let dispatch_var = self.fresh_temp_var("SD");
            let result_var = self.fresh_var("SDResult");
            let state_var = self.fresh_var("SDState");
            let current_state = self.current_state_var();
            let module = self.module_name.clone();
            let args_doc = self.generate_tier2_args(arguments, tier2_args)?;

            // Step 3: Generate the self-dispatch (using safe_dispatch or sealed path)
            let call_doc = self.generate_tier2_dispatch_call(
                selector,
                arguments.is_empty(),
                &selector_atom,
                &dispatch_var,
                &current_state,
                &module,
                args_doc,
            );

            // Result/error clauses + state extraction
            let new_state = self.next_state_var();
            let error_clause = self.generate_self_dispatch_error_clause("SDError", &selector_atom);
            docs.push(docvec![
                call_doc,
                "<{'reply', ",
                leaf::var(result_var.clone()),
                ", ",
                leaf::var(state_var.clone()),
                "}> when 'true' -> {",
                leaf::var(result_var),
                ", ",
                leaf::var(state_var),
                "} ",
                error_clause,
                "end in let ",
                leaf::var(new_state),
                " = call 'erlang':'element'(2, ",
                leaf::var(dispatch_var.clone()),
                ") in "
            ]);

            // Step 4: Extract captured-mutated locals from the returned State
            let final_state = self.current_state_var();
            for (_pos, captured_vars) in tier2_args {
                for var_name in captured_vars {
                    let core_var = self
                        .lookup_var(var_name)
                        .cloned()
                        .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                    let key = Self::local_state_key(var_name);
                    docs.push(docvec![
                        "let ",
                        leaf::var(core_var),
                        " = call 'maps':'get'(",
                        leaf::atom(key),
                        ", ",
                        leaf::var(final_state.clone()),
                        ") in "
                    ]);
                }
            }

            return Ok((Document::Vec(docs), dispatch_var));
        }
        Err(CodeGenError::Internal(
            "generate_tier2_self_send_open called on non-MessageSend expression".to_string(),
        ))
    }

    /// BT-851: Builds argument list for a Tier 2 self-send, using stateful block
    /// generation for marked positions.
    fn generate_tier2_args(
        &mut self,
        arguments: &[Expression],
        tier2_args: &[(usize, Vec<String>)],
    ) -> Result<Document<'static>> {
        let tier2_positions: std::collections::HashSet<usize> =
            tier2_args.iter().map(|(pos, _)| *pos).collect();
        let tier2_vars_by_pos: std::collections::HashMap<usize, &Vec<String>> =
            tier2_args.iter().map(|(pos, vars)| (*pos, vars)).collect();

        let mut arg_parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                arg_parts.push(Document::Str(", "));
            }
            if tier2_positions.contains(&i) {
                if let Expression::Block(block) = arg {
                    let captured_vars = tier2_vars_by_pos[&i];
                    arg_parts.push(self.generate_block_stateful(block, captured_vars)?);
                } else {
                    arg_parts.push(self.expression_doc(arg)?);
                }
            } else {
                arg_parts.push(self.expression_doc(arg)?);
            }
        }
        Ok(Document::Vec(arg_parts))
    }

    /// BT-851: Generates the dispatch call for a Tier 2 self-send.
    ///
    /// Handles sealed (direct/dispatch) and non-sealed (`safe_dispatch`) paths.
    #[allow(clippy::too_many_arguments)]
    fn generate_tier2_dispatch_call(
        &mut self,
        selector: &MessageSelector,
        no_args: bool,
        selector_atom: &str,
        dispatch_var: &str,
        current_state: &str,
        module: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        if self.is_class_sealed() {
            let selector_name = selector.name().to_string();
            if self.sealed_method_selectors().contains(&selector_name) {
                let self_var = self.fresh_temp_var("SealedSelf");
                let comma = if no_args { "" } else { ", " };
                docvec![
                    "let ",
                    leaf::var(self_var.clone()),
                    " = call 'beamtalk_actor':'make_self'(",
                    leaf::var(current_state.to_string()),
                    ") in let ",
                    leaf::var(dispatch_var.to_string()),
                    " = case call ",
                    leaf::atom(module.to_string()),
                    ":",
                    leaf::atom(super::selector_mangler::sealed_fn_name(&selector_name)),
                    "(",
                    args_doc,
                    comma,
                    leaf::var(self_var),
                    ", ",
                    leaf::var(current_state.to_string()),
                    ") of "
                ]
            } else {
                let self_var = self.fresh_temp_var("SealedSelf");
                docvec![
                    "let ",
                    leaf::var(self_var.clone()),
                    " = call 'beamtalk_actor':'make_self'(",
                    leaf::var(current_state.to_string()),
                    ") in let ",
                    leaf::var(dispatch_var.to_string()),
                    " = case call ",
                    leaf::atom(module.to_string()),
                    ":'dispatch'(",
                    leaf::atom(selector_atom.to_string()),
                    ", [",
                    args_doc,
                    "], ",
                    leaf::var(self_var),
                    ", ",
                    leaf::var(current_state.to_string()),
                    ") of "
                ]
            }
        } else {
            docvec![
                "let ",
                leaf::var(dispatch_var.to_string()),
                " = case call ",
                leaf::atom(module.to_string()),
                ":'safe_dispatch'(",
                leaf::atom(selector_atom.to_string()),
                ", [",
                args_doc,
                "], ",
                leaf::var(current_state.to_string()),
                ") of "
            ]
        }
    }
}

// NOTE: class_method_module_name and related helpers (is_primitive_stdlib_class,
// is_bt_stdlib_class, is_erlang_stdlib_module) were removed in BT-411.
// Class dispatch now goes through runtime class_send/3 instead of
// compile-time module name resolution.

/// BT-2007 / BT-3057: Class-module auto-exports reachable via `self <sel>`
/// from inside a class method.
///
/// Every compiled class module carries a small set of 0-arity functions
/// generated by codegen (not user-defined), used for reflection. These are
/// not installed in `class_method_selectors` and the runtime chain walker
/// does not index them (it reads only user-defined `class_methods` on each
/// ancestor `gen_server`).
///
/// BT-3057: `superclass` moved off this path — its raw export returns the
/// bare class-name atom, not the `#beamtalk_object{}` a Beamtalk-level
/// `self superclass` must produce, and callers comparing it against a real
/// class object got a silent `equals:`/`==` mismatch. It is now handled by
/// `class_self_send_reflective_primitive`, which calls the real
/// `beamtalk_behaviour_intrinsics:classSuperclass/1` intrinsic. `class_name`
/// stays here: its intrinsic (`className/1`) always calls
/// `gen_server:call(ClassPid, class_name)`, which would deadlock from inside
/// the class's own process, so returning the raw atom directly is the
/// correct (deadlock-safe) behavior for this one, not a bug.
///
/// Intentionally narrow. The three audited exports on every user class
/// module are `superclass/0`, `class_name/0`, and `method_table/0` — but
/// only `class_name` is both plausibly reachable via Beamtalk source and
/// safe to serve from its raw export (`method_table` is a codegen-internal
/// metadata accessor without a stable user-level API; `__beamtalk_meta`,
/// `register_class`, `has_method` are similarly internal). `methods` was
/// initially in this list by mistake — no class module exports `methods/0`,
/// so including it would have compiled `self methods` to a non-existent
/// direct call (would crash with `undef`). Any auto-export added later that
/// is reachable via plain self-send needs to be added here (if its raw
/// export form is the desired behavior) or to
/// `class_self_send_reflective_primitive` (if it must produce the same
/// value the non-self-send Behaviour-protocol dispatch would). Unknown
/// selectors take the inherited-dispatch path, which raises structured DNU
/// on miss — strictly better than the pre-BT-2007 fallthrough (direct call
/// → runtime `undef`).
pub(super) fn is_class_auto_export_selector(selector_atom: &str, arity: usize) -> bool {
    arity == 0 && selector_atom == "class_name"
}

/// BT-3057: Behaviour-protocol reflective primitives that are safe to
/// dispatch directly from a class-method self-send.
///
/// Unlike `is_class_auto_export_selector`'s raw module exports, these
/// selectors are ordinary `@primitive`-backed methods inherited from
/// `Behaviour`/`Class` (see `stdlib/src/Behaviour.bt`) that non-self-send
/// dispatch resolves via `try_class_chain_fallthrough`'s
/// `beamtalk_dispatch:lookup/5` walk — a walk that itself is not reachable
/// from inside the class's own process (it round-trips through
/// `gen_server:call` for the class method table). Each selector listed here
/// is deadlock-safe to call directly with `ClassSelf` because its
/// `beamtalk_behaviour_intrinsics` implementation resolves everything it
/// needs (module, metadata) through `beamtalk_object_class:module_name_safe/1`
/// (which has a `ClassPid =:= self()` fast path, BT-3054) and
/// `__beamtalk_meta/0`/ETS lookups rather than an unconditional
/// `gen_server:call(ClassPid, ...)`.
///
/// Deliberately does NOT include every Behaviour-protocol selector:
/// `classSubclasses/1` and `classAllSubclasses/1` unconditionally call
/// `gen_server:call(ClassPid, class_name)` with no metadata-first fast
/// path, so adding `subclasses`/`allSubclasses` here today would trade a
/// `does_not_understand` for a real self-call deadlock. Extending this list
/// requires auditing the target intrinsic for that property first.
///
/// Note the metadata-first fast path itself still has a `not_available`
/// fallback in `classSuperclass/1`/`classIncludesSelector/2` that calls
/// `gen_server:call(ClassPid, ...)` — safe here only because every class
/// module this codegen emits unconditionally exports `__beamtalk_meta/0`
/// (see `native_facade.rs` / the `gen_server` codegen), so `meta_for_module/1`
/// never actually falls through to it for a compiled class. If that
/// invariant ever changes, this function's safety claim needs re-auditing.
pub(super) fn class_self_send_reflective_primitive(
    selector_atom: &str,
    arity: usize,
) -> Option<&'static str> {
    match (selector_atom, arity) {
        ("superclass", 0) => Some("classSuperclass"),
        ("includesSelector:", 1) => Some("classIncludesSelector"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        class_self_send_reflective_primitive, is_character_typed_receiver,
        is_class_auto_export_selector,
    };
    use crate::core_erlang::CoreErlangGenerator;
    use beamtalk_core::ast::{
        Expression, Identifier, KeywordPart, Literal, MessageSelector, MethodDefinition,
        TypeAnnotation,
    };
    use beamtalk_core::source_analysis::{Severity, Span, lex_with_eof, parse};
    use std::collections::BTreeSet;

    fn s() -> Span {
        Span::new(0, 0)
    }

    /// BT-2029 / BT-3057: the classifier must stay in sync with the actual
    /// reachable auto-exports on generated class modules. `class_name/0` is
    /// reachable via plain self-send and must short-circuit to a direct call;
    /// `superclass` moved to `class_self_send_reflective_primitive` (BT-3057)
    /// because its raw export returns a bare atom instead of a class object,
    /// so it must NOT be classified as an auto-export here anymore.
    /// `methods/0` does not exist on the current codegen (an earlier mistaken
    /// inclusion); `method_table/0` and `has_method/1` are codegen-internal
    /// reflection APIs with no Beamtalk surface and must NOT be classified as
    /// auto-exports (they would compile to a direct call that users cannot
    /// reach anyway, but including them would bypass the structured DNU path
    /// that catches typos). Arity mismatches must also return false so that,
    /// e.g., `self class_name: X` does not get hijacked into a direct call to
    /// the 0-arity `class_name/0`.
    #[test]
    fn is_class_auto_export_selector_matches_reachable_exports() {
        assert!(is_class_auto_export_selector("class_name", 0));

        // BT-3057: superclass now routes through the reflective-primitive
        // path (its raw export is unwrapped and identity-broken), not here.
        assert!(!is_class_auto_export_selector("superclass", 0));

        // Codegen-internal, not reachable via Beamtalk self-send.
        assert!(!is_class_auto_export_selector("method_table", 0));
        assert!(!is_class_auto_export_selector("has_method", 1));
        assert!(!is_class_auto_export_selector("register_class", 0));
        assert!(!is_class_auto_export_selector("__beamtalk_meta", 0));

        // Historical mistake — `methods/0` is not emitted by the current
        // codegen, so classifying it as auto-export would produce a call
        // to a non-existent function.
        assert!(!is_class_auto_export_selector("methods", 0));

        // Arity mismatches must not match.
        assert!(!is_class_auto_export_selector("superclass", 1));
        assert!(!is_class_auto_export_selector("class_name", 1));

        // Arbitrary user selectors must fall through to inherited dispatch.
        assert!(!is_class_auto_export_selector("increment", 0));
        assert!(!is_class_auto_export_selector("at:put:", 2));
    }

    /// BT-3057: `superclass` and `includesSelector:` must route to their
    /// real `beamtalk_behaviour_intrinsics` implementations so a
    /// class-method self-send produces the same value non-self-send dispatch
    /// would (a genuine `#beamtalk_object{}` for `superclass`, a proper
    /// dispatch instead of DNU for `includesSelector:`). Selectors whose
    /// intrinsic is not deadlock-safe from inside the class's own process
    /// (`subclasses`, `allSubclasses` — see the function doc) must NOT
    /// appear here; arity mismatches must not match either.
    #[test]
    fn class_self_send_reflective_primitive_matches_safe_selectors_only() {
        assert_eq!(
            class_self_send_reflective_primitive("superclass", 0),
            Some("classSuperclass")
        );
        assert_eq!(
            class_self_send_reflective_primitive("includesSelector:", 1),
            Some("classIncludesSelector")
        );

        // Arity mismatches must not match.
        assert_eq!(class_self_send_reflective_primitive("superclass", 1), None);
        assert_eq!(
            class_self_send_reflective_primitive("includesSelector:", 0),
            None
        );

        // Not deadlock-safe (unconditional gen_server:call in the intrinsic) —
        // must stay off this list until audited/fixed.
        assert_eq!(class_self_send_reflective_primitive("subclasses", 0), None);
        assert_eq!(
            class_self_send_reflective_primitive("allSubclasses", 0),
            None
        );

        // Arbitrary user selectors must fall through to inherited dispatch.
        assert_eq!(class_self_send_reflective_primitive("increment", 0), None);
    }

    /// BT-3018 / ADR 0109: `File open:…do:` is lowered at the call site so the
    /// user's block runs in the caller rather than the File class `gen_server`.
    /// The interception is keyed on the *unqualified* stdlib `File` — a
    /// package-qualified `mylib@File` is an unrelated class that happens to
    /// share the name, and must keep reaching its own implementation.
    #[test]
    fn block_scoped_file_open_is_lowered_only_for_unqualified_file() {
        /// Lowers `[package@]File <keywords>` with one argument per keyword.
        fn lower(package: Option<&str>, keywords: &[&str]) -> String {
            let mut generator = CoreErlangGenerator::new("test");
            let receiver = Expression::ClassReference {
                name: Identifier::new("File", s()),
                package: package.map(|p| Identifier::new(p, s())),
                span: s(),
            };
            let selector = MessageSelector::Keyword(
                keywords.iter().map(|k| KeywordPart::new(*k, s())).collect(),
            );
            let arguments: Vec<_> = keywords
                .iter()
                .map(|k| Expression::Identifier(Identifier::new(k.trim_end_matches(':'), s())))
                .collect();
            generator
                .generate_message_send(&receiver, &selector, &arguments)
                .unwrap()
                .to_pretty_string()
        }

        // Both intercepted selectors, so dropping either from the `matches!`
        // list fails here rather than silently reintroducing the deadlock.
        for keywords in [&["open:", "do:"][..], &["open:", "mode:", "do:"][..]] {
            let selector_atom = keywords.concat();

            let unqualified = lower(None, keywords);
            assert!(
                unqualified.contains("'native_call'(")
                    && unqualified.contains("'beamtalk_file'")
                    && unqualified.contains(&format!("'{selector_atom}'")),
                "unqualified File {selector_atom} should lower to a native_call in the \
                 caller. Got: {unqualified}"
            );
            assert!(
                !unqualified.contains("class_send"),
                "the block must not reach the class gen_server. Got: {unqualified}"
            );

            // A package-qualified receiver keeps the ordinary class-send path:
            // asserted positively, so an empty or otherwise-shaped lowering
            // cannot pass by merely lacking the stdlib module name.
            let qualified = lower(Some("mylib"), keywords);
            assert!(
                !qualified.contains("'beamtalk_file'"),
                "mylib@File {selector_atom} is a different class and must not be \
                 redirected to the stdlib File shim. Got: {qualified}"
            );
            assert!(
                qualified.contains("class_send"),
                "mylib@File {selector_atom} should fall through to a normal class \
                 send. Got: {qualified}"
            );
        }
    }

    #[test]
    fn test_generate_message_send_unary_uses_dispatch() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Identifier(Identifier::new("counter", s()));
        let selector = MessageSelector::Unary("increment".into());
        let doc = generator
            .generate_message_send(&receiver, &selector, &[])
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_message_dispatch':'send'("),
            "unary send should use unified dispatch. Got: {output}"
        );
        assert!(
            output.contains("'increment'"),
            "should include selector atom. Got: {output}"
        );
    }

    #[test]
    fn test_generate_cast_send_non_actor_routes_via_cast() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Identifier(Identifier::new("other", s()));
        let selector = MessageSelector::Unary("doIt".into());
        let doc = generator
            .generate_cast_send(&receiver, &selector, &[])
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_message_dispatch':'cast'("),
            "non-actor cast send should route through cast/3. Got: {output}"
        );
    }

    #[test]
    fn test_generate_super_send_uses_beamtalk_dispatch() {
        let mut generator = CoreErlangGenerator::new("test");
        let selector = MessageSelector::Unary("initialize".into());
        let doc = generator.generate_super_send(&selector, &[]).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_dispatch':'super'("),
            "super send should use beamtalk_dispatch:super. Got: {output}"
        );
        assert!(
            output.contains("'initialize'"),
            "should include selector. Got: {output}"
        );
    }

    /// BT-2252: in a value/primitive context the generated fun is
    /// `fun(Args, Self) -> Result` with no `State` binding, so `super` must
    /// lower to `super_value/4` rather than the state-threading `super/5`.
    /// Referencing the absent `State` produced invalid Core Erlang
    /// (variable 'State' is unbound).
    #[test]
    fn test_generate_super_send_value_context_uses_super_value() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.context = crate::core_erlang::CodeGenContext::ValueType;
        let selector = MessageSelector::Unary("printString".into());
        let doc = generator.generate_super_send(&selector, &[]).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_dispatch':'super_value'("),
            "value-context super should route to super_value/4. Got: {output}"
        );
        assert!(
            !output.contains("State"),
            "value-context super must not reference an unbound State. Got: {output}"
        );
        assert!(
            output.contains("'printString'"),
            "should include selector. Got: {output}"
        );
    }

    #[test]
    fn test_generate_actor_spawn_non_repl() {
        let mut generator = CoreErlangGenerator::new("test");
        let doc = generator
            .generate_actor_spawn_qualified("Counter", None, None)
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'spawn'()"),
            "spawn should call spawn/0. Got: {output}"
        );
        assert!(
            output.contains("counter"),
            "spawn should reference module. Got: {output}"
        );
    }

    #[test]
    fn test_generate_message_send_keyword_includes_selector() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Identifier(Identifier::new("obj", s()));
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", s()),
            KeywordPart::new("put:", s()),
        ]);
        let arguments = vec![
            Expression::Literal(Literal::Integer(1), s()),
            Expression::Literal(Literal::Integer(2), s()),
        ];
        let doc = generator
            .generate_message_send(&receiver, &selector, &arguments)
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'at:put:'"),
            "keyword send should combine selector parts. Got: {output}"
        );
    }

    #[test]
    fn test_generate_message_send_binary_op_addition() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Literal(Literal::Integer(3), s());
        let selector = MessageSelector::Binary("+".into());
        let arguments = vec![Expression::Literal(Literal::Integer(4), s())];
        let doc = generator
            .generate_message_send(&receiver, &selector, &arguments)
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("erlang':'+'("),
            "binary + should compile to erlang arithmetic. Got: {output}"
        );
    }

    #[test]
    fn test_generate_cast_send_actor_self_uses_safe_dispatch() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.context = crate::core_erlang::CodeGenContext::Actor;
        let receiver = Expression::Identifier(Identifier::new("self", s()));
        let selector = MessageSelector::Unary("doIt".into());
        let doc = generator
            .generate_cast_send(&receiver, &selector, &[])
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("safe_dispatch"),
            "actor self cast should use safe_dispatch. Got: {output}"
        );
        assert!(
            output.contains("'ok'"),
            "actor self cast should return 'ok'. Got: {output}"
        );
    }

    /// BT-1475: Self-cast inside a block must route through the actor mailbox,
    /// not call `safe_dispatch` directly, because the block may execute in a
    /// different process (Timer callback, cross-actor callback).
    #[test]
    fn test_generate_cast_send_actor_self_in_block_uses_mailbox() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.context = crate::core_erlang::CodeGenContext::Actor;
        generator.block_depth = 1; // Simulate being inside a block
        let receiver = Expression::Identifier(Identifier::new("self", s()));
        let selector = MessageSelector::Unary("bump".into());
        let doc = generator
            .generate_cast_send(&receiver, &selector, &[])
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_message_dispatch"),
            "self cast inside block should route through mailbox. Got: {output}"
        );
        assert!(
            output.contains("cast"),
            "self cast inside block should use cast dispatch. Got: {output}"
        );
        assert!(
            !output.contains("safe_dispatch"),
            "self cast inside block must NOT use safe_dispatch. Got: {output}"
        );
    }

    /// BT-3214: `is_character_typed_receiver` must recognize both syntactic
    /// shapes that statically produce a Character — a literal (`$A`) and a
    /// `Character value:` factory call — including through any number of
    /// parenthesizations, since `(Character value: 10) asString` parses the
    /// factory call as `Parenthesized(MessageSend(..))`. Everything else
    /// (plain integers, other class factory methods, a package-qualified
    /// `Character`) must NOT match, or the codegen would incorrectly route
    /// an actual Integer/other-class receiver through Character's dispatch.
    #[test]
    fn is_character_typed_receiver_matches_literal_and_value_factory() {
        let char_literal = Expression::Literal(Literal::Character('A'), s());
        assert!(is_character_typed_receiver(&char_literal));

        let character_value_call = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Character", s()),
                package: None,
                span: s(),
            }),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
            arguments: vec![Expression::Literal(Literal::Integer(10), s())],
            is_cast: false,
            span: s(),
        };
        assert!(is_character_typed_receiver(&character_value_call));

        // The reported bug's exact shape: `(Character value: 10)` as a
        // parenthesized receiver of a further send (`asString`).
        let parenthesized_once = Expression::Parenthesized {
            expression: Box::new(character_value_call.clone()),
            span: s(),
        };
        assert!(is_character_typed_receiver(&parenthesized_once));

        // Nested parens must also see through.
        let parenthesized_twice = Expression::Parenthesized {
            expression: Box::new(parenthesized_once),
            span: s(),
        };
        assert!(is_character_typed_receiver(&parenthesized_twice));

        // A parenthesized literal must match too (`($A) asString`).
        let parenthesized_literal = Expression::Parenthesized {
            expression: Box::new(char_literal),
            span: s(),
        };
        assert!(is_character_typed_receiver(&parenthesized_literal));
    }

    /// BT-3214: `uppercase`/`lowercase` also have a declared `-> Character`
    /// return type (`Character.bt`), so a chain like `$a uppercase asString`
    /// hits the identical bug as `(Character value: 10) asString` — the
    /// receiver of `asString` (`$a uppercase`) is statically Character but
    /// isn't a literal or a `value:` call. The check must recurse: applying
    /// `uppercase`/`lowercase` to an already Character-typed receiver stays
    /// Character-typed, however deep the chain (`$a uppercase lowercase`).
    #[test]
    fn is_character_typed_receiver_recurses_through_uppercase_lowercase() {
        fn unary_send(receiver: Expression, selector: &str) -> Expression {
            Expression::MessageSend {
                receiver: Box::new(receiver),
                selector: MessageSelector::Unary(selector.into()),
                arguments: vec![],
                is_cast: false,
                span: s(),
            }
        }

        let char_literal = Expression::Literal(Literal::Character('a'), s());
        let uppercased = unary_send(char_literal.clone(), "uppercase");
        assert!(is_character_typed_receiver(&uppercased));

        // Chains recurse arbitrarily deep.
        let round_tripped = unary_send(uppercased, "lowercase");
        assert!(is_character_typed_receiver(&round_tripped));

        // Also recognized on a `Character value:` receiver, not just a literal.
        let value_call = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Character", s()),
                package: None,
                span: s(),
            }),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
            arguments: vec![Expression::Literal(Literal::Integer(97), s())],
            is_cast: false,
            span: s(),
        };
        assert!(is_character_typed_receiver(&unary_send(
            value_call,
            "uppercase"
        )));

        // A non-Character-returning unary selector on a Character receiver
        // must NOT match — only `uppercase`/`lowercase` are Character-typed.
        assert!(!is_character_typed_receiver(&unary_send(
            char_literal.clone(),
            "asInteger"
        )));

        // `uppercase` on a receiver that is NOT itself Character-typed must
        // not match — recursion must terminate on a real Character source,
        // not accept any arbitrarily nested `uppercase` send.
        let int_literal = Expression::Literal(Literal::Integer(97), s());
        assert!(!is_character_typed_receiver(&unary_send(
            int_literal,
            "uppercase"
        )));
    }

    /// BT-3214: enforces the invariant `is_character_typed_receiver` depends
    /// on — that its hardcoded selector set (`value:` as the class factory,
    /// `uppercase`/`lowercase` as the Character-returning instance methods)
    /// is *exactly* the set of methods `stdlib/src/Character.bt` declares
    /// with a `-> Character` return type. This is the enforcing test
    /// architecture-principles.md requires for any "must stay in sync"
    /// coupling: parses the real `Character.bt` off disk and fails loudly if
    /// a future edit adds, removes, or renames a Character-returning method
    /// there without updating the codegen recognizer to match — silent drift
    /// here would silently reopen the exact bug this issue fixes for the new
    /// method (dispatch misrouted to Integer's BIF module).
    #[test]
    fn character_bt_character_returning_methods_match_codegen_recognizer() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("crates/")
            .parent()
            .expect("repo root")
            .to_path_buf();
        let character_bt_path = repo_root.join("stdlib/src/Character.bt");
        let Ok(source) = std::fs::read_to_string(&character_bt_path) else {
            eprintln!(
                "skipping: {} not present in this checkout",
                character_bt_path.display()
            );
            return;
        };

        let tokens = lex_with_eof(&source);
        let (module, diags) = parse(tokens);
        assert!(
            diags.iter().all(|d| d.severity != Severity::Error),
            "Character.bt must parse without errors: {diags:?}"
        );

        let character_class = module
            .classes
            .iter()
            .find(|c| c.name.name == "Character")
            .expect("Character.bt must define the Character class");

        let returns_character = |method: &MethodDefinition| -> bool {
            matches!(
                &method.return_type,
                Some(TypeAnnotation::Simple(id)) if id.name == "Character"
            )
        };

        let class_side: BTreeSet<String> = character_class
            .class_methods
            .iter()
            .filter(|m| returns_character(m))
            .map(|m| m.selector.name().to_string())
            .collect();
        let instance_side: BTreeSet<String> = character_class
            .methods
            .iter()
            .filter(|m| returns_character(m))
            .map(|m| m.selector.name().to_string())
            .collect();

        assert_eq!(
            class_side,
            BTreeSet::from(["value:".to_string()]),
            "is_character_typed_receiver's class-side factory-method list \
             (\"value:\") no longer matches Character.bt's actual \
             `-> Character` class methods — update the recognizer in \
             dispatch_codegen.rs to match"
        );
        assert_eq!(
            instance_side,
            BTreeSet::from(["uppercase".to_string(), "lowercase".to_string()]),
            "is_character_typed_receiver's instance-side selector list \
             (\"uppercase\", \"lowercase\") no longer matches Character.bt's \
             actual `-> Character` instance methods — update the recognizer \
             in dispatch_codegen.rs to match"
        );
    }

    #[test]
    fn is_character_typed_receiver_rejects_non_character_shapes() {
        // A bare integer literal is not Character-typed.
        assert!(!is_character_typed_receiver(&Expression::Literal(
            Literal::Integer(10),
            s()
        )));

        // A different class's factory method must not match.
        let other_factory = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Integer", s()),
                package: None,
                span: s(),
            }),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
            arguments: vec![Expression::Literal(Literal::Integer(10), s())],
            is_cast: false,
            span: s(),
        };
        assert!(!is_character_typed_receiver(&other_factory));

        // A different selector on Character itself must not match — only
        // the `value:` factory is statically known to return Character.
        let wrong_selector = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Character", s()),
                package: None,
                span: s(),
            }),
            selector: MessageSelector::Unary("someOtherMethod".into()),
            arguments: vec![],
            is_cast: false,
            span: s(),
        };
        assert!(!is_character_typed_receiver(&wrong_selector));

        // A package-qualified `Character` is a different, user-defined class
        // that merely shares the name — must not be special-cased.
        let package_qualified = Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Character", s()),
                package: Some(Identifier::new("mylib", s())),
                span: s(),
            }),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
            arguments: vec![Expression::Literal(Literal::Integer(10), s())],
            is_cast: false,
            span: s(),
        };
        assert!(!is_character_typed_receiver(&package_qualified));
    }

    /// BT-3214: codegen for `(Character value: 10) asString` must emit a
    /// direct call to `bt@stdlib@character:dispatch/3`, not fall through to
    /// the generic runtime-dispatch path (which would key on `is_integer/1`
    /// and misroute to `bt@stdlib@integer`, producing `"10"` instead of a
    /// genuine 1-byte LF string).
    #[test]
    fn character_value_factory_receiver_dispatches_to_character_module() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Parenthesized {
            expression: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::ClassReference {
                    name: Identifier::new("Character", s()),
                    package: None,
                    span: s(),
                }),
                selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
                arguments: vec![Expression::Literal(Literal::Integer(10), s())],
                is_cast: false,
                span: s(),
            }),
            span: s(),
        };
        let selector = MessageSelector::Unary("asString".into());
        let output = generator
            .generate_message_send(&receiver, &selector, &[])
            .unwrap()
            .to_pretty_string();

        assert!(
            output.contains("'bt@stdlib@character':'dispatch'"),
            "expected direct Character dispatch, got: {output}"
        );
        assert!(
            !output.contains("beamtalk_message_dispatch"),
            "must not fall through to generic runtime dispatch (which would \
             misroute via is_integer/1 to Integer). Got: {output}"
        );
    }
}
