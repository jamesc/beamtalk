// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expression code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This domain service handles code generation for Beamtalk expressions:
//! - Literals (integers, floats, strings, symbols) and string interpolation
//! - Identifiers and variable references
//! - Map/list/array literals
//! - Field access (`self.field`) and field/class-var assignment
//! - Await expressions
//! - Cascades
//!
//! Block (closure) compilation lives in [`super::blocks`]; pattern
//! matching (`match:`) and destructuring extraction live in
//! [`super::patterns`]. Message sending is handled by
//! [`super::dispatch_codegen`].

use super::control_flow::{Closure, FieldWriteSite};
use super::threaded_ir::{self, ThreadedStmt, ValueRef};
use super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{
    CascadeMessage, Expression, Identifier, Literal, MapPair, MessageSelector, SlotKind,
    StringSegment,
};
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// Generates code for a literal value.
    ///
    /// Maps Beamtalk literals to Core Erlang:
    /// - Integers: `42` → `42`
    /// - Floats: `3.14` → `3.14`
    /// - Strings: `"hello"` → binary syntax `#{#<104>(8,1,...), #<101>(8,1,...), ...}#`
    /// - Symbols: `#foo` → atom `'foo'`
    /// - Characters: `$a` → integer `97`
    /// - Arrays: `[1, 2, 3]` → list `[1, 2, 3]`
    #[allow(clippy::self_only_used_in_recursion)] // &self needed for method resolution
    pub(super) fn generate_literal(&self, lit: &Literal) -> Result<Document<'static>> {
        match lit {
            Literal::Integer(n) => Ok(leaf::int_lit(*n)),
            Literal::Float(f) => Ok(leaf::float_lit(*f)),
            Literal::String(s) => Ok(leaf::binary_lit(s)),
            Literal::Symbol(s) => Ok(leaf::atom(s.to_string())),
            Literal::Character(c) => Ok(leaf::int_lit(i64::from(*c as u32))),
            Literal::List(elements) => {
                let mut parts: Vec<Document<'static>> = vec![Document::Str("[")];
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(", "));
                    }
                    parts.push(self.generate_literal(elem)?);
                }
                parts.push(Document::Str("]"));
                Ok(Document::Vec(parts))
            }
        }
    }

    /// Generates code for a string interpolation expression (ADR 0023 Phase 3).
    ///
    /// Compiles `StringInterpolation` to Core Erlang binary construction:
    /// - Literal segments → byte sequences in the binary
    /// - Expression segments → evaluate, dispatch `displayString`, insert as binary segment
    ///
    /// Example: `"Hello, {name}!"` compiles to:
    /// ```text
    /// let _interpExpr1 = Name in
    ///   let _interpRaw2 = call 'beamtalk_message_dispatch':'send'(_interpExpr1, 'displayString', []) in
    ///     let _interpStr3 = call 'beamtalk_primitive':'display_string'(_interpRaw2) in
    ///       #{#<72>(8,1,...), ..., #<_interpStr3>('all',8,'binary',...), #<33>(8,1,...)}#
    /// ```
    pub(super) fn generate_string_interpolation(
        &mut self,
        segments: &[StringSegment],
    ) -> Result<Document<'static>> {
        // Collect let-bindings for expression segments and binary segments
        let mut let_bindings: Vec<Document<'static>> = Vec::with_capacity(segments.len());
        let mut binary_parts: Vec<Document<'static>> = Vec::with_capacity(segments.len());

        for segment in segments {
            match segment {
                StringSegment::Literal(s) => {
                    if !s.is_empty() {
                        // ADR 0089: `leaf::binary_segments` emits the *unwrapped*
                        // inner segments (no surrounding `#{...}#`) so they can be
                        // spliced between interpolation segments — `leaf::binary_lit`
                        // would add the wrapper and corrupt the construction.
                        binary_parts.push(leaf::binary_segments(s));
                    }
                }
                StringSegment::Interpolation(expr) => {
                    let expr_doc = self.expression_doc(expr)?;
                    let (chain_prefix, reference) = self.interpolation_segment_chain(expr_doc);
                    let_bindings.push(chain_prefix);
                    binary_parts.push(reference);
                }
            }
        }

        // Build the binary literal: #{seg1,seg2,...}#
        let mut binary_doc_parts: Vec<Document<'static>> = vec![Document::Str("#{")];
        for (i, part) in binary_parts.into_iter().enumerate() {
            if i > 0 {
                binary_doc_parts.push(Document::Str(","));
            }
            binary_doc_parts.push(part);
        }
        binary_doc_parts.push(Document::Str("}#"));
        let binary_doc = Document::Vec(binary_doc_parts);

        // Wrap with let-bindings
        let mut doc = Document::Vec(Vec::new());
        for binding in let_bindings {
            doc = docvec![doc, binding];
        }
        doc = docvec![doc, binary_doc];

        Ok(doc)
    }

    /// One interpolation segment's `let`-chain prefix (value →
    /// `displayString` dispatch → `display_string`) and its binary-part
    /// reference — the two pieces every consumer needs, kept in one place
    /// per CLAUDE.md's no-duplicate-implementations rule:
    /// [`Self::generate_string_interpolation`]'s all-inline compile
    /// (`value_doc` from a fresh `self.expression_doc(expr)`) and
    /// [`Self::threaded_string_interpolation`]'s threaded one (`value_doc`
    /// from an already-sequenced [`threaded_ir::ThreadedValue`]'s rendered
    /// value). The prefix ends in `in ` (an open Core Erlang `let` chain,
    /// like every other opaque `Statement`/preamble fragment this
    /// generator builds) so callers concatenate it directly ahead of
    /// whatever comes next.
    fn interpolation_segment_chain(
        &mut self,
        value_doc: Document<'static>,
    ) -> (Document<'static>, Document<'static>) {
        let interp_var = self.fresh_temp_var("interpExpr");
        let raw_str_var = self.fresh_temp_var("interpRaw");
        let str_var = self.fresh_temp_var("interpStr");

        // Evaluate the expression
        let prefix = docvec![
            "let ",
            leaf::var(interp_var.clone()),
            " = ",
            value_doc,
            " in ",
            // Dispatch displayString to convert to binary string
            // (user-facing representation).
            "let ",
            leaf::var(raw_str_var.clone()),
            " = call 'beamtalk_message_dispatch':'send'(",
            leaf::var(interp_var),
            ", 'displayString', []) in ",
            // Convert to binary string via display_string, which
            // recursively awaits futures. This handles both direct
            // futures (actor displayString returns future) and
            // double-futures (Object.displayString delegates to self
            // printString which is itself async for actors, returning a
            // nested future).
            "let ",
            leaf::var(str_var.clone()),
            " = call 'beamtalk_primitive':'display_string'(",
            leaf::var(raw_str_var),
            ") in ",
        ];
        // Add as binary segment: #<Var>('all',8,'binary',['unsigned'|['big']])
        let reference = docvec![
            "#<",
            leaf::var(str_var),
            ">('all',8,'binary',['unsigned'|['big']])",
        ];
        (prefix, reference)
    }

    /// ADR 0118 phase 1b: [`Self::generate_string_interpolation`],
    /// through the sequencing rule. Each segment's own `displayString`
    /// dispatch is itself an effect that always runs immediately after
    /// that segment's value (its own per-segment `let`-chain,
    /// [`Self::interpolation_segment_chain`]) — so, unlike an ordinary
    /// operand, a segment cannot be compiled in place once a LATER
    /// segment needs a prelude: its dispatch would then run AFTER that
    /// later prelude despite being textually first (`"{a}-{self bump}"`:
    /// `a`'s `displayString` must run before `bump` dispatches). Once any
    /// segment — at index `k`, the LAST one that does — needs threading,
    /// every interpolation segment up to and including `k` moves its
    /// whole `let`-chain into the returned prelude, in order; segments
    /// after `k` stay in the returned value's own document, compiled
    /// normally. When no segment needs threading this costs nothing
    /// beyond the existing `generate_string_interpolation` call — one
    /// document, no prelude.
    pub(super) fn threaded_string_interpolation(
        &mut self,
        segments: &[StringSegment],
        span: Span,
        frame: threaded_ir::FrameId,
    ) -> Result<threaded_ir::ThreadedValue> {
        let Some(k) = segments.iter().rposition(
            |seg| matches!(seg, StringSegment::Interpolation(e) if self.subexpr_needs_prelude(e)),
        ) else {
            let doc = self.generate_string_interpolation(segments)?;
            return Ok(threaded_ir::ThreadedValue {
                prelude: Vec::new(),
                value: ValueRef::Doc(doc),
            });
        };

        let mut prelude: Vec<ThreadedStmt> = Vec::new();
        let mut inline_prefixes: Vec<Document<'static>> = Vec::new();
        let mut binary_parts: Vec<Document<'static>> = Vec::new();

        for (i, seg) in segments.iter().enumerate() {
            match seg {
                StringSegment::Literal(s) => {
                    if !s.is_empty() {
                        binary_parts.push(leaf::binary_segments(s));
                    }
                }
                StringSegment::Interpolation(expr) => {
                    let value_doc = if i <= k {
                        let tv = self.threaded_expression(expr, frame)?;
                        prelude.extend(tv.prelude);
                        self.threaded_value_doc(&tv.value)
                    } else {
                        self.expression_doc(expr)?
                    };
                    let (chain_prefix, reference) = self.interpolation_segment_chain(value_doc);
                    if i <= k {
                        prelude.push(ThreadedStmt::Statement(chain_prefix, span));
                    } else {
                        inline_prefixes.push(chain_prefix);
                    }
                    binary_parts.push(reference);
                }
            }
        }

        let mut binary_doc_parts: Vec<Document<'static>> = vec![Document::Str("#{")];
        for (i, part) in binary_parts.into_iter().enumerate() {
            if i > 0 {
                binary_doc_parts.push(Document::Str(","));
            }
            binary_doc_parts.push(part);
        }
        binary_doc_parts.push(Document::Str("}#"));

        let mut value_doc = Document::Vec(inline_prefixes);
        value_doc = docvec![value_doc, Document::Vec(binary_doc_parts)];

        Ok(threaded_ir::ThreadedValue {
            prelude,
            value: ValueRef::Doc(value_doc),
        })
    }

    /// Generates code for an identifier reference.
    ///
    /// Handles three cases:
    /// 1. Reserved keywords (`true`, `false`, `nil`, `self`) → Core Erlang atoms/variables
    /// 2. Bound variables (in scope) → Core Erlang variable name
    /// 3. Unbound identifiers → Field access via `maps:get/2` from context-appropriate variable:
    ///    - **Actor context**: `State` or `StateAcc` (with threading)
    ///    - **`ValueType` context**: `Self` parameter
    ///    - **Repl context**: `State` from bindings
    pub(super) fn generate_identifier(&mut self, id: &Identifier) -> Result<Document<'static>> {
        // Handle special reserved identifiers as atoms
        match id.name.as_str() {
            "true" => Ok(Document::Str("'true'")),
            "false" => Ok(Document::Str("'false'")),
            "nil" => Ok(Document::Str("'nil'")),
            "self" => {
                // Check if self is explicitly bound (e.g., in class methods)
                if let Some(var_name) = self.lookup_var("self").cloned() {
                    Ok(docvec![leaf::var(var_name)])
                } else if self.context == super::CodeGenContext::ValueType {
                    // In value type context, self resolves to the latest Self{N}
                    // snapshot after any preceding field assignments.
                    Ok(leaf::var(self.current_self_var()))
                } else if self.context == super::CodeGenContext::Repl {
                    // ADR 0095 §1: at the top level of a REPL eval there is
                    // no enclosing-method receiver, so a bare `Self` would be an
                    // unbound Core Erlang variable. Resolve `self` from the bindings
                    // map instead, so the Inspector's value `evaluate:` can bind
                    // `self` to the inspected value by passing `#{self => Value}`.
                    // On a miss, raise `undefined_variable` directly rather
                    // than routing through `resolve_name`, whose `bind:as:` tier would
                    // let a user binding named `self` silently shadow the reserved
                    // word. A top-level `self` with no `#{self => _}` binding is
                    // genuinely undefined.
                    let state_var = self.current_state_var();
                    let resolved_var = self.fresh_var("Resolved");
                    Ok(docvec![
                        "case call 'maps':'find'('self', ",
                        leaf::var(state_var),
                        ") of ",
                        "<{'ok', ",
                        leaf::var(resolved_var.clone()),
                        "}> when 'true' -> ",
                        leaf::var(resolved_var),
                        " <'error'> when 'true' -> ",
                        "call 'beamtalk_workspace':'raise_undefined_variable'('self') ",
                        "end",
                    ])
                } else {
                    Ok(Document::Str("Self")) // self → Self parameter
                }
            }
            "super" => {
                // super alone is an error - must be used in message send (super method: args)
                Err(CodeGenError::UnsupportedFeature {
                    feature: "super used alone (must be in message send like 'super method: arg')"
                        .to_string(),
                    span: Some(id.span),
                })
            }
            _ => {
                // Check if it's a bound variable in current or outer scopes
                if let Some(var_name) = self.lookup_var(id.name.as_str()).cloned() {
                    Ok(docvec![leaf::var(var_name)])
                } else {
                    // In hybrid mode, check if this is a read-only field
                    // accessed implicitly (bare name without self. prefix).
                    if self.loop_mode.in_hybrid_loop {
                        if let Some(param_var) = self
                            .loop_mode
                            .hybrid_readonly_field_params
                            .get(id.name.as_str())
                        {
                            return Ok(leaf::var(param_var.clone()));
                        }
                    }
                    // Field access from state/self
                    // Context determines which variable to use
                    let state_var = match self.context {
                        super::CodeGenContext::ValueType => {
                            // Value types use the latest Self{N} snapshot
                            self.current_self_var()
                        }
                        super::CodeGenContext::Actor | super::CodeGenContext::Repl => {
                            // `current_field_read_state_var` already applies
                            // the StateAcc-in-loop-body/hybrid-State*/BT-3562
                            // direct-params-capture rules — see its own doc
                            // comment (CLAUDE.md: no duplicate "mirrors" of
                            // that same rule here).
                            self.current_field_read_state_var()
                        }
                    };
                    // ADR 0081 Phase 1: in REPL context a free
                    // identifier is no longer guaranteed to be present in State —
                    // workspace globals (singletons, bind:as:) are resolved lazily
                    // rather than eagerly injected into the session map. So instead
                    // of `maps:get/2` (which throws {badkey,_} on a miss) we look up
                    // the locals map and, on a miss, fall through to the shared
                    // runtime resolver, which checks bind:as: -> singletons ->
                    // classes -> undefined_variable. Actor/ValueType field access is
                    // unchanged (the field must exist in State/Self).
                    //
                    // This applies in loop/hybrid-loop bodies too: `state_var`
                    // above already resolves to the context-appropriate map
                    // (`StateAcc`/`StateAccN` for plain loops, `StateN` for hybrid
                    // loops), and a free workspace global referenced inside a
                    // `do:`/`collect:` block must resolve lazily rather than crash
                    // with `badkey`. Captured locals (the loop accumulator and any
                    // `__local__*` keys) are unpacked into bound vars at the loop
                    // body entry, so they are handled by the `lookup_var` branch
                    // above and never reach this fallthrough.
                    //
                    // Note: `resolve_name/2` does its own `maps:find(Name, Locals)`
                    // as tier 1, then falls through to the live workspace tiers, so
                    // passing `StateAcc`/`StateN` as the locals map is correct — a
                    // miss there proceeds to bind:as: -> singletons -> classes.
                    if self.context == super::CodeGenContext::Repl {
                        let resolved_var = self.fresh_var("Resolved");
                        Ok(docvec![
                            "case call 'maps':'find'(",
                            leaf::atom(id.name.to_string()),
                            ", ",
                            leaf::var(state_var.clone()),
                            ") of ",
                            "<{'ok', ",
                            leaf::var(resolved_var.clone()),
                            "}> when 'true' -> ",
                            leaf::var(resolved_var),
                            " <'error'> when 'true' -> call 'beamtalk_workspace':'resolve_name'(",
                            leaf::var(state_var),
                            ", ",
                            leaf::atom(id.name.to_string()),
                            ") ",
                            "end",
                        ])
                    } else if self.in_cell_closure() {
                        // ADR 0137 §3: an actor closure reads a bare field through the cell.
                        Ok(self.cell_field_read_doc(id.name.as_str()))
                    } else if self.guarded_op_take_reads() {
                        // BT-4121: inside a stateless loop that brackets its
                        // guarded operators, the entry `State` is stale for
                        // the slow paths' writes — read the live cell.
                        Ok(docvec![
                            "call 'maps':'get'(",
                            leaf::atom(id.name.to_string()),
                            ", ",
                            super::threaded_ir::cell_take_call(),
                            ")",
                        ])
                    } else {
                        Ok(docvec![
                            "call 'maps':'get'(",
                            leaf::atom(id.name.to_string()),
                            ", ",
                            leaf::var(state_var),
                            ")",
                        ])
                    }
                }
            }
        }
    }

    /// Generates code for a map literal: `~{key => value, ...}~`
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// ~{'name' => <<"Alice">>, 'age' => 30}~
    /// ```
    pub(super) fn generate_map_literal(&mut self, pairs: &[MapPair]) -> Result<Document<'static>> {
        if pairs.is_empty() {
            return Ok(Document::Str("~{}~"));
        }

        // Capture all keys and values as one ordered sequence so
        // capture_subexpr_sequence can force-hoist every sub-expression in
        // left-to-right order when ANY of them produces an open scope.
        // Preserves the source-order semantics: key1, val1, key2, val2, ...
        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(pairs.len() * 2);
        for pair in pairs {
            all_exprs.push(&pair.key);
            all_exprs.push(&pair.value);
        }
        let (preamble, mut docs) = self.thread_subexprs(&all_exprs, "MapLit")?;

        // Pair up (key, value) docs, then reuse the existing comma-join
        // helper (sequencing.rs) instead of hand-rolling the same separator
        // loop `generate_list_literal`/`generate_array_literal` use.
        let mut docs_iter = docs.drain(..);
        let mut pair_docs: Vec<Document<'static>> = Vec::with_capacity(pairs.len());
        for _ in pairs {
            let key_doc = docs_iter.next().expect("key doc");
            let val_doc = docs_iter.next().expect("value doc");
            pair_docs.push(docvec![key_doc, " => ", val_doc]);
        }
        let literal_doc = docvec!["~{ ", Self::join_docs_with_commas(pair_docs), " }~"];
        Ok(self.close_prelude(&preamble, literal_doc, "MapLit"))
    }

    /// Generates code for a list literal: `#(1, 2, 3)` → `[1, 2, 3]`
    ///
    /// Cons syntax `#(head | tail)` → `[head | tail]`
    pub(super) fn generate_list_literal(
        &mut self,
        elements: &[Expression],
        tail: Option<&Expression>,
    ) -> Result<Document<'static>> {
        // Capture all elements + optional tail as one ordered
        // sequence so evaluation order is preserved when sub-expressions have
        // open scopes.
        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(elements.len() + 1);
        for elem in elements {
            all_exprs.push(elem);
        }
        if let Some(t) = tail {
            all_exprs.push(t);
        }
        let (preamble, mut docs) = self.thread_subexprs(&all_exprs, "ListLit")?;

        // The tail doc (if any) was threaded last, so pop it off before
        // comma-joining the remaining element docs via the existing helper
        // (sequencing.rs) instead of a hand-rolled separator loop.
        let tail_doc = tail.and_then(|_| docs.pop());
        let mut parts: Vec<Document<'static>> = vec![Document::Str("[")];
        parts.push(Self::join_docs_with_commas(docs));
        if let Some(td) = tail_doc {
            if !elements.is_empty() {
                parts.push(Document::Str(" | "));
            }
            parts.push(td);
        }
        parts.push(Document::Str("]"));
        let literal_doc = Document::Vec(parts);
        Ok(self.close_prelude(&preamble, literal_doc, "ListLit"))
    }

    /// Generates code for an array literal: `#[1, 2, 3]`
    ///
    /// Compiles to a call to `beamtalk_array:from_list/1`:
    /// ```erlang
    /// call 'beamtalk_array':'from_list'([1, 2, 3])
    /// ```
    pub(super) fn generate_array_literal(
        &mut self,
        elements: &[Expression],
    ) -> Result<Document<'static>> {
        // Capture all elements as one ordered sequence so evaluation
        // order is preserved when sub-expressions have open scopes.
        let exprs: Vec<&Expression> = elements.iter().collect();
        let (preamble, docs) = self.thread_subexprs(&exprs, "ArrLit")?;

        // Reuse the existing comma-join helper (sequencing.rs) instead of a
        // hand-rolled separator loop.
        let literal_doc = docvec![
            "call 'beamtalk_array':'from_list'([",
            Self::join_docs_with_commas(docs),
            "])",
        ];
        Ok(self.close_prelude(&preamble, literal_doc, "ArrLit"))
    }

    /// Generates code for a tuple literal: `{a, b}` → `{A, B}` (ADR 0136).
    ///
    /// A direct Core Erlang tuple — no `Tuple withAll:` send, no list
    /// allocation. Elements are threaded as one ordered sequence (like
    /// `generate_list_literal`) so evaluation stays left-to-right when a
    /// sub-expression opens a scope (ADR 0118).
    pub(super) fn generate_tuple_literal(
        &mut self,
        elements: &[Expression],
    ) -> Result<Document<'static>> {
        let exprs: Vec<&Expression> = elements.iter().collect();
        let (preamble, docs) = self.thread_subexprs(&exprs, "TupLit")?;
        let literal_doc = docvec!["{", Self::join_docs_with_commas(docs), "}"];
        Ok(self.close_prelude(&preamble, literal_doc, "TupLit"))
    }

    /// ADR 0124 §3/B3: `true` when `field_name` is declared `late` on
    /// `class_name` or an ancestor, per the flattened `ClassHierarchy`
    /// metadata `state_field_kind` (BT-3547 B5a). Missing hierarchy metadata
    /// (no class compiled — e.g. a bare REPL expression with no enclosing
    /// class) degrades to `false`, the pre-ADR-0124 unguarded `maps:get`.
    pub(in crate::core_erlang) fn is_late_state_field(
        &self,
        class_name: &str,
        field_name: &str,
    ) -> bool {
        self.class_hierarchy
            .as_ref()
            .is_some_and(|h| h.state_field_kind(class_name, field_name) == SlotKind::Late)
    }

    /// Class-side counterpart to [`Self::is_late_state_field`], for a
    /// `classState:` slot read via `self.<var>` in a class method.
    fn is_late_class_var(&self, class_name: &str, var_name: &str) -> bool {
        self.class_hierarchy
            .as_ref()
            .is_some_and(|h| h.class_variable_kind(class_name, var_name) == SlotKind::Late)
    }

    /// ADR 0124 §3 (as amended by BT-4061): builds the
    /// `call 'beamtalk_error':'raise'(...)` for the `'error'` (absent-key)
    /// arm of [`Self::generate_late_field_read`]. Reuses the shared
    /// `beamtalk_error:'new'` → `'with_selector'` → `'with_hint'` chain
    /// (`errors.rs`) rather than hand-rolling it, per CLAUDE.md's
    /// no-duplicate-implementations rule.
    fn generate_uninitialized_state_raise(
        &mut self,
        class_name: &str,
        selector: &str,
        hint_text: &str,
    ) -> Document<'static> {
        let err0 = self.fresh_temp_var("LateErr");
        let err1 = self.fresh_temp_var("LateErr");
        let err2 = self.fresh_temp_var("LateErr");
        docvec![
            super::errors::beamtalk_error_doc(
                leaf::var(err0.clone()),
                leaf::var(err0),
                leaf::var(err1.clone()),
                leaf::var(err1),
                leaf::var(err2.clone()),
                "uninitialized_state_error",
                leaf::atom(class_name.to_string()),
                leaf::atom(selector.to_string()),
                leaf::binary_lit(hint_text),
                Document::Str(" "),
            ),
            "call 'beamtalk_error':'raise'(",
            leaf::var(err2),
            ")",
        ]
    }

    /// ADR 0124 §3/B3 (as amended by BT-4061): the guarded `late`-slot
    /// read — presence-only:
    /// ```erlang
    /// case maps:find(Slot, State) of
    ///   {ok, V} -> V
    ///   'error' -> <raise uninitialized_state_error>
    /// end
    /// ```
    /// Shared by `generate_field_access`'s instance branch (State/Self map)
    /// and the actor-closure cell view — both read the same slot the same
    /// way, differing only in which map variable and declared-type lookup
    /// feed it. Presence — not value — is assignment (ADR §2): a present
    /// key holding `nil` (reachable only through an open-world writer —
    /// `spawnWith:` with a non-literal map, `fieldAt:put:`, `perform:` —
    /// since B1 rejects `late` on every nil-admitting type) answers `nil`,
    /// exactly as `hasField:` reports it assigned. Stays pure — returns a
    /// value, threads no state — because reading never writes.
    fn generate_late_field_read(
        &mut self,
        field_name: &str,
        state_var: Document<'static>,
        declared_type: Option<beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType>,
    ) -> Document<'static> {
        self.generate_late_field_read_of(field_name, state_var, declared_type)
    }

    /// [`Self::generate_late_field_read`] over any state-map expression (a
    /// threaded `State` variable, or an actor closure's cell view).
    fn generate_late_field_read_of(
        &mut self,
        field_name: &str,
        state_var: Document<'static>,
        declared_type: Option<beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType>,
    ) -> Document<'static> {
        let class_name = self.class_name();
        let selector = self
            .current_method_selector
            .clone()
            .unwrap_or_else(|| "unknown".to_string());
        let type_name = declared_type.map_or_else(|| "Unknown".to_string(), |t| t.to_string());
        let hint_text = format!(
            "{class_name} field '{field_name}' (:: {type_name}) is declared `late` \
             and has not been assigned yet"
        );
        let ok_var = self.fresh_temp_var("LateVal");
        let absent_raise =
            self.generate_uninitialized_state_raise(&class_name, &selector, &hint_text);
        docvec![
            "case call 'maps':'find'(",
            leaf::atom(field_name.to_string()),
            ", ",
            state_var,
            ") of <{'ok', ",
            leaf::var(ok_var.clone()),
            "}> when 'true' -> ",
            leaf::var(ok_var),
            " <'error'> when 'true' -> ",
            absent_raise,
            " end",
        ]
    }

    /// Generates code for field access (e.g., `self.value`).
    ///
    /// Maps to Erlang `maps:get/2` call:
    /// ```erlang
    /// call 'maps':'get'('value', State)  // Actor context
    /// call 'maps':'get'('value', Self)   // ValueType context
    /// ```
    ///
    /// A `late` slot (ADR 0124 §1) instead emits the guarded
    /// [`Self::generate_late_field_read`] form — `maps:get`'s `badkey` crash
    /// on the slot's absent key would be strictly worse than the
    /// `UninitializedStateError` ADR 0124 §3 specifies.
    pub(super) fn generate_field_access(
        &mut self,
        receiver: &Expression,
        field: &Identifier,
    ) -> Result<Document<'static>> {
        // ADR 0130 §2: a class method reads a class variable in place from the
        // class process's dictionary (inlined, the runtime helper on a miss).
        if self.in_class_method() {
            if self.is_class_var_field_read(receiver, field) {
                let class_name = self.class_name();
                let late = self.is_late_class_var(&class_name, field.name.as_str());
                return Ok(self.class_var_read_doc(field.name.as_str(), late));
            }
            if Self::is_self_receiver(receiver) {
                if let Some(err) = self.inherited_class_var_error(field.name.as_str(), field.span) {
                    return Err(err);
                }
            }
            return Err(CodeGenError::UnsupportedFeature {
                feature: format!(
                    "cannot access instance field '{}' in a class method",
                    field.name
                ),
                span: Some(field.span),
            });
        }
        // Field access resolves against self's State/Self map. A non-self receiver
        // is not valid (Beamtalk enforces encapsulation) and is rejected with a
        // diagnostic in the fall-through below.
        if let Expression::Identifier(recv_id) = receiver {
            if recv_id.name == "self" {
                let class_name = self.class_name();
                let is_late = self.is_late_state_field(&class_name, field.name.as_str());
                // BT-4121: inside a stateless loop that brackets its guarded
                // operators, the entry `State` is stale for the slow paths'
                // writes — read the live cell instead. (Never in hybrid
                // mode: the mode selection routes those loops to `StateAcc`.)
                if self.guarded_op_take_reads() {
                    let cell = super::threaded_ir::cell_take_call();
                    if is_late {
                        let declared_type = self
                            .class_hierarchy
                            .as_ref()
                            .and_then(|h| h.state_field_type(&class_name, field.name.as_str()));
                        return Ok(self.generate_late_field_read_of(
                            field.name.as_str(),
                            cell,
                            declared_type,
                        ));
                    }
                    return Ok(docvec![
                        "call 'maps':'get'(",
                        leaf::atom(field.name.to_string()),
                        ", ",
                        cell,
                        ")",
                    ]);
                }
                // In hybrid mode, read-only fields are pre-extracted before the letrec.
                // Use the direct parameter variable instead of generating maps:get every
                // iteration. A `late` field is never pre-extracted this way (loop_mode.rs
                // withholds it from `hybrid_readonly_field_params`, ADR 0124 §4b), so this
                // never fires for one — the guarded read below runs instead, per-iteration.
                if self.loop_mode.in_hybrid_loop {
                    if let Some(param_var) = self
                        .loop_mode
                        .hybrid_readonly_field_params
                        .get(field.name.as_str())
                    {
                        return Ok(leaf::var(param_var.clone()));
                    }
                }
                // Use appropriate variable based on context. Actor uses
                // `current_field_read_state_var` (not `current_state_var`
                // directly) so a bare field read inside a non-hybrid
                // direct-params loop resolves to the captured pre-loop state
                // variable instead of an unbound `StateAcc` (BT-3562) — see
                // that accessor's own doc comment.
                // ADR 0137 §3: inside an actor closure a field is read through
                // the cell (the creation-time capture away from home).
                if self.in_cell_closure() {
                    if is_late {
                        let declared_type = self
                            .class_hierarchy
                            .as_ref()
                            .and_then(|h| h.state_field_type(&class_name, field.name.as_str()));
                        let view = self.cell_view_doc();
                        return Ok(self.generate_late_field_read_of(
                            field.name.as_str(),
                            view,
                            declared_type,
                        ));
                    }
                    return Ok(self.cell_field_read_doc(field.name.as_str()));
                }
                let state_var = match self.context {
                    super::CodeGenContext::ValueType => self.current_self_var(),
                    super::CodeGenContext::Actor => self.current_field_read_state_var(),
                    super::CodeGenContext::Repl => "State".to_string(),
                };
                if is_late {
                    let declared_type = self
                        .class_hierarchy
                        .as_ref()
                        .and_then(|h| h.state_field_type(&class_name, field.name.as_str()));
                    return Ok(self.generate_late_field_read(
                        field.name.as_str(),
                        leaf::var(state_var),
                        declared_type,
                    ));
                }
                return Ok(docvec![
                    "call 'maps':'get'(",
                    leaf::atom(field.name.to_string()),
                    ", ",
                    leaf::var(state_var),
                    ")",
                ]);
            }
        }

        let receiver_desc = match receiver {
            Expression::Identifier(id) => id.name.to_string(),
            _ => "receiver".to_string(),
        };
        Err(CodeGenError::UnsupportedFeature {
            feature: format!(
                "field access on a non-self receiver — use a getter method instead: \
                `{receiver_desc} {field_name}` rather than `{receiver_desc}.{field_name}`",
                field_name = field.name
            ),
            span: Some(receiver.span()),
        })
    }

    /// Generates code for a field assignment (`self.field := value`).
    ///
    /// Uses threading to simulate mutation in Core Erlang. The generated pattern varies
    /// by [`FieldWriteSite`]:
    ///
    /// - **Actor context**: `State{n}` threading via `maps:put`
    /// - **`ValueType` context**: `Self{n}` threading — each assignment produces
    ///   a new immutable snapshot; `self` in subsequent expressions resolves to `Self{n}`
    /// - **Class method**: an in-place `put` into the class process's
    ///   dictionary (ADR 0130 §2), handled before any threading
    ///
    /// ```erlang
    /// let _Val = <value> in
    /// let State{n} = call 'maps':'put'('fieldName', _Val, State{n-1}) in
    /// _Val
    /// ```
    ///
    /// The assignment expression evaluates to the assigned value (Smalltalk semantics).
    ///
    /// A thin, `Closure::Closed` wrapper around
    /// [`Self::lower_field_write`] — the single lowering core this, `dispatch_codegen.rs`'s
    /// `generate_field_assignment_open` (`Closure::Open`), and
    /// `control_flow::conditionals`'s `lower_field_assignment_bind` (the
    /// un-rendered-`Bind`-push consumption style) all share, instead of
    /// three independently hand-duplicated `maps:put` emissions — the
    /// missing abstraction the ADR 0110/0111 bug family traced back to.
    pub(super) fn generate_field_assignment(
        &mut self,
        field_name: &str,
        value: &Expression,
    ) -> Result<Document<'static>> {
        // ADR 0130 §2: a class-variable write is an in-place `put` that answers
        // the assigned value, an expression like any other (no rebinding).
        if self.in_class_method() {
            if !self.class_var_names().contains(field_name) {
                if let Some(err) = self.inherited_class_var_error(field_name, value.span()) {
                    return Err(err);
                }
                return Err(CodeGenError::UnsupportedFeature {
                    feature: format!(
                        "cannot assign to instance field '{field_name}' in a class method"
                    ),
                    span: Some(value.span()),
                });
            }
            let value_doc = self.expression_doc(value)?;
            return Ok(self.class_var_write_doc(field_name, value_doc));
        }
        // ADR 0137 §3: an actor closure writes a field in place, through the
        // cell; the assignment answers the assigned value and rebinds nothing.
        if self.in_cell_closure() {
            let value_doc = self.expression_doc(value)?;
            return Ok(self.cell_field_write_doc(field_name, value_doc));
        }
        let (site, frame) = (
            FieldWriteSite::for_context(self.context),
            super::threaded_ir::FrameId::ROOT,
        );
        let (doc, _val_var) =
            self.lower_field_write(site, Closure::Closed, field_name, value, frame)?;
        Ok(doc)
    }

    /// ADR 0111 coverage extension: construct + verify the
    /// just-emitted `Self{N}`/`State{N}` `Bind`'s `ThreadedIr` shape via the
    /// shared [`super::threaded_ir::verify_simple_bind`] helper — for the two
    /// `generate_field_assignment` sibling branches (value-type `Self` and
    /// instance-actor `State`) that, unlike the class-var branch above,
    /// otherwise construct no `ThreadedIr` fixture at all.
    ///
    /// `source_version`/`target_version` are the real `self_version()`/
    /// `state_version()` counter reads taken immediately before and after
    /// the caller's own `next_self_var()`/`next_state_var()` call — not
    /// re-derived here, so this checks what the generator actually produced,
    /// not a recomputation of it (ADR 0111 §Verifier honesty).
    ///
    /// `pub(super)`: also reused by `dispatch_codegen.rs`'s
    /// `generate_field_assignment_open` plain-`State` branch, the sibling
    /// "open" (non-last-position) mint site — same check, same reasoning, no
    /// reason to hand-roll a second copy (CLAUDE.md's no-duplicate-implementations rule).
    pub(super) fn check_simple_field_bind_invariant(
        &mut self,
        prefix: super::threaded_ir::VersionPrefix,
        source_version: usize,
        target_version: usize,
        invariant_label: &str,
        span: beamtalk_core::source_analysis::Span,
    ) {
        let errors = super::threaded_ir::verify_simple_bind(
            prefix,
            source_version,
            target_version,
            span,
            self.threaded_scope(),
        );
        self.report_threaded_ir_verify_errors(&errors, invariant_label, span);
    }

    /// Generates await expression.
    ///
    /// Delegates to `beamtalk_future:await/1` which blocks until the future
    /// is resolved or rejected (with 30-second default timeout):
    /// ```erlang
    /// call 'beamtalk_future':'await'(Future)
    /// ```
    pub(super) fn generate_await(&mut self, future: &Expression) -> Result<Document<'static>> {
        // Delegate to beamtalk_future:await/1, which uses 30s default timeout
        let future_doc = self.expression_doc(future)?;
        Ok(docvec!["call 'beamtalk_future':'await'(", future_doc, ")"])
    }

    /// Generates await with explicit timeout.
    ///
    /// Delegates to `beamtalk_future:await/2` with an explicit timeout value:
    /// ```erlang
    /// call 'beamtalk_future':'await'(Future, Timeout)
    /// ```
    pub(super) fn generate_await_with_timeout(
        &mut self,
        future: &Expression,
        timeout: &Expression,
    ) -> Result<Document<'static>> {
        let future_doc = self.expression_doc(future)?;
        let timeout_doc = self.expression_doc(timeout)?;
        Ok(docvec![
            "call 'beamtalk_future':'await'(",
            future_doc,
            ", ",
            timeout_doc,
            ")"
        ])
    }

    /// Generates awaitForever expression.
    ///
    /// Delegates to `beamtalk_future:await_forever/1` which waits indefinitely:
    /// ```erlang
    /// call 'beamtalk_future':'await_forever'(Future)
    /// ```
    pub(super) fn generate_await_forever(
        &mut self,
        future: &Expression,
    ) -> Result<Document<'static>> {
        let future_doc = self.expression_doc(future)?;
        Ok(docvec![
            "call 'beamtalk_future':'await_forever'(",
            future_doc,
            ")"
        ])
    }

    /// Generates code for cascade expressions.
    ///
    /// Cascades send multiple messages to the same receiver using semicolon separators.
    /// The receiver is evaluated once and each message is sent to that receiver.
    ///
    /// # Example
    ///
    /// ```beamtalk
    /// collection add: 1; add: 2; add: 3
    /// ```
    ///
    /// Generates:
    ///
    /// ```erlang
    /// let Receiver = <evaluate collection> in
    ///   let _ = <send add: 1 to Receiver> in
    ///   let _ = <send add: 2 to Receiver> in
    ///   <send add: 3 to Receiver>
    /// ```
    ///
    /// The cascade returns the result of the final message.
    pub(super) fn generate_cascade(
        &mut self,
        receiver: &Expression,
        messages: &[CascadeMessage],
    ) -> Result<Document<'static>> {
        let (prelude, value) = self.generate_cascade_parts(receiver, messages)?;
        if prelude.is_empty() {
            return Ok(value);
        }
        // BT-3802: this renders the cascade to one self-contained `Document`,
        // so a `State`/`SelfVt` rebind in its hoisted prelude is scoped out of
        // everything after the cascade (the BT-3791 `unbound variable
        // 'State1'`). A cascade whose operands need a prelude is a producer
        // (`threaded_expression` splices it into the caller's frame); reaching
        // this closed rendering with such a prelude is a lowering that failed
        // to splice, reported rather than silently dropped.
        let escaped = super::threaded_ir::opaque_escapes(&prelude, false);
        self.report_threaded_ir_verify_errors(
            &escaped,
            "cascade operand state rebind trapped in a closed cascade expression",
            receiver.span(),
        );
        Ok(docvec![self.threaded_prelude_doc(&prelude), value])
    }

    /// [`Self::generate_cascade`] split into the statements that run before
    /// the cascade's value (receiver binding, hoisted argument preludes,
    /// every non-final message send) and the final message's send, which is
    /// the cascade's value. BT-3791: a `State` rebind in a hoisted argument
    /// prelude must stay in scope for the consumer, so
    /// [`Self::threaded_expression`] splices `prelude` into the caller's
    /// statement sequence instead of nesting it inside a value binding.
    #[allow(clippy::too_many_lines)]
    pub(super) fn generate_cascade_parts(
        &mut self,
        receiver: &Expression,
        messages: &[CascadeMessage],
    ) -> Result<(Vec<ThreadedStmt>, Document<'static>)> {
        // ClassBuilder construction cascades with literal
        // block methods get synthesised source setters — methodSource: from
        // methods: (instance side) and classMethodSource: from classMethods:
        // (class side) — so builder-defined classes are indexable by
        // SystemNavigation on both sides.
        let augmented = super::class_builder_source::inject_method_source(receiver, messages);
        let messages: &[CascadeMessage] = augmented.as_deref().unwrap_or(messages);

        if messages.is_empty() {
            // Edge case: cascade with no messages just evaluates to the receiver
            return Ok((Vec::new(), self.generate_expression(receiver)?));
        }

        // The parser represents cascades such that `receiver` is the *first*
        // message send expression, e.g. for:
        //
        //   counter increment; increment; getValue
        //
        // `receiver` is a MessageSend for `counter increment`, and `messages`
        // holds the remaining cascade messages. We need to:
        //   1. Evaluate the underlying receiver expression (`counter`) once,
        //      bind it to a temp variable.
        //   2. Send the first message (`increment`) and all subsequent
        //      cascade messages to that same bound receiver.
        //
        // Normalize the cascade into (underlying_receiver, all_messages) so
        // both the MessageSend and non-MessageSend cases share one code path.
        let (underlying_receiver, all_messages): (
            &Expression,
            Vec<(&MessageSelector, &[Expression])>,
        ) = if let Expression::MessageSend {
            receiver: inner,
            selector: first_selector,
            arguments: first_arguments,
            ..
        } = receiver
        {
            let mut all: Vec<(&MessageSelector, &[Expression])> =
                Vec::with_capacity(messages.len() + 1);
            all.push((first_selector, first_arguments.as_slice()));
            for msg in messages {
                all.push((&msg.selector, msg.arguments.as_slice()));
            }
            (inner.as_ref(), all)
        } else {
            // Fallback: receiver is not a MessageSend — send all cascade
            // messages directly to it.
            let all: Vec<(&MessageSelector, &[Expression])> = messages
                .iter()
                .map(|msg| (&msg.selector, msg.arguments.as_slice()))
                .collect();
            (receiver, all)
        };

        // ADR 0084: A `classBuilder … classMethods: #{…}; register`
        // cascade gets its `classMethods:` block-literal values lowered as
        // class-method funs (see `generate_class_methods_map_arg`).
        let builder_ctx: Option<(String, Vec<String>)> =
            super::class_builder_source::builder_class_method_context(receiver, messages);

        let receiver_var = self.fresh_temp_var("Receiver");
        // Splice any receiver preamble into `docs` first (mirroring how
        // `capture_subexpr_sequence`/`bind_args_to_temps` already do this for
        // ordinary message-send receivers/arguments), so a rebind stays
        // visible at the same nesting level as everything else in `docs`.
        let mut seq = self.sequence_call(std::slice::from_ref(&underlying_receiver), "Recv")?;
        let receiver_value_doc = seq.next();
        let receiver_prelude = seq.into_prelude();
        let recv_span = underlying_receiver.span();
        let mut docs: Vec<ThreadedStmt> = receiver_prelude;
        docs.push(ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(receiver_var.clone()),
                " = ",
                receiver_value_doc,
                " in "
            ],
            recv_span,
        ));

        let total_messages = all_messages.len();
        for (index, (selector, arguments)) in all_messages.into_iter().enumerate() {
            let is_last = index == total_messages - 1;

            let selector_atom = selector.name().to_string();
            if matches!(selector, MessageSelector::Binary(_)) {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "binary selectors in cascades".to_string(),
                    span: Some(underlying_receiver.span()),
                });
            }

            // Hoist field-assignment arg bindings BEFORE the `let _ =`
            // wrapper so that StateN remains in scope for subsequent messages.
            // The `classMethods:` argument of a recognised builder
            // cascade gets its block values lowered as class-method funs.
            // `addClassMethod: #sel body: [block]` is the incremental
            // counterpart — its block (the second argument) is lowered the same
            // way, keeping the selector argument as an ordinary value.
            let arg_docs =
                match &builder_ctx {
                    Some((bclass, cvars)) if selector.name().as_str() == "classMethods:" => {
                        match arguments {
                            [Expression::MapLiteral { pairs, .. }] => {
                                vec![self.generate_class_methods_map_arg(
                                    pairs, bclass, cvars, &mut docs,
                                )?]
                            }
                            _ => self.generate_cascade_args(arguments, &mut docs)?,
                        }
                    }
                    Some((bclass, cvars)) if selector.name().as_str() == "addClassMethod:body:" => {
                        match arguments {
                            [
                                sel_arg @ Expression::Literal(Literal::Symbol(_), _),
                                Expression::Block(block),
                            ] => {
                                let sel_doc = self.generate_cascade_args(
                                    std::slice::from_ref(sel_arg),
                                    &mut docs,
                                )?;
                                let mut combined = sel_doc;
                                combined.push(self.generate_class_method_single_arg(
                                    sel_arg, block, bclass, cvars,
                                )?);
                                combined
                            }
                            _ => self.generate_cascade_args(arguments, &mut docs)?,
                        }
                    }
                    _ => self.generate_cascade_args(arguments, &mut docs)?,
                };

            let mut send: Vec<Document<'static>> = vec![docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(receiver_var.clone()),
                ", ",
                leaf::atom(selector_atom),
                ", [",
            ]];
            for (j, arg_doc) in arg_docs.into_iter().enumerate() {
                if j > 0 {
                    send.push(Document::Str(", "));
                }
                send.push(arg_doc);
            }
            send.push(Document::Str("])"));
            let send = Document::Vec(send);

            if is_last {
                return Ok((docs, send));
            }
            // For all but the last message, discard the result
            docs.push(ThreadedStmt::Statement(
                docvec!["let _ = ", send, " in "],
                recv_span,
            ));
        }

        // Unreachable: `messages` is non-empty, so the last iteration returns.
        Err(CodeGenError::Internal(
            "cascade lowered without a final message".to_string(),
        ))
    }

    /// Generates cascade arguments, hoisting field-assignment bindings to outer scope.
    ///
    /// This is a helper to avoid duplicating the hoisting logic across the
    /// `MessageSend` and fallback branches of `generate_cascade`.
    ///
    /// Hoisting only the argument(s) that need it
    /// while a *different*, side-effecting-but-plain argument in the same
    /// message stays inline can reverse their observable left-to-right
    /// evaluation order — the hoisted one's preamble runs before the
    /// whole `send(...)`, while the plain one only evaluates inline at call
    /// time. This is exactly the hazard
    /// [`Self::capture_subexpr_sequence`] exists to avoid for
    /// ordinary (non-cascade) argument lists: decide *once*, for the whole
    /// list, whether any argument needs hoisting — if so, hoist every
    /// argument (binding a plain one to a fresh `let` too), never just
    /// some. Splits every argument first (field assignments via
    /// [`Self::generate_field_assignment_open`], everything else via
    /// [`Self::split_subexpr_for_preamble`] — same conversion
    /// `capture_subexpr_sequence` itself uses) so each argument is compiled
    /// exactly once regardless of which branch below is taken, then makes
    /// that same all-or-nothing decision. `capture_subexpr_sequence` isn't
    /// reused directly here since it has no field-assignment case — cascade
    /// arguments are the one call site that needs both.
    pub(super) fn generate_cascade_args(
        &mut self,
        arguments: &[Expression],
        docs: &mut Vec<ThreadedStmt>,
    ) -> Result<Vec<Document<'static>>> {
        let mut splits: Vec<(Vec<ThreadedStmt>, Document<'static>)> =
            Vec::with_capacity(arguments.len());
        for arg in arguments {
            splits.push(self.split_cascade_arg(arg)?);
        }
        let span = arguments.first().map_or_else(
            || beamtalk_core::source_analysis::Span::new(0, 0),
            beamtalk_core::ast::Expression::span,
        );
        Ok(self.hoist_cascade_splits(splits, docs, span))
    }

    /// Splits one cascade argument into its `(prelude, value)` pair: a field
    /// assignment via [`Self::generate_field_assignment_open`], anything else
    /// via [`Self::sequence_call`].
    pub(super) fn split_cascade_arg(
        &mut self,
        arg: &Expression,
    ) -> Result<(Vec<ThreadedStmt>, Document<'static>)> {
        if self.is_field_assignment(arg) {
            // BT-3803: a real `Bind` (not an opaque `Statement`), so the
            // consumer of the cascade continues from the new `State`
            // version (`state_var_after_prelude` reads the prelude's `Bind`s).
            let mut prelude = Vec::new();
            let span = arg.unwrap_parens().span();
            let val_var = self.lower_field_assignment_bind(
                arg.unwrap_parens(),
                threaded_ir::FrameId::ROOT,
                span,
                &mut prelude,
            )?;
            Ok((prelude, leaf::var(val_var)))
        } else {
            let mut seq = self.sequence_call(std::slice::from_ref(&arg), "CascadeArg")?;
            let doc = seq.next();
            Ok((seq.into_prelude(), doc))
        }
    }

    /// The all-or-nothing hoisting decision of [`Self::generate_cascade_args`]
    /// over already-split arguments: when any carries a prelude, every
    /// prelude is spliced into `docs` (plain values bound to fresh temps) so
    /// a `StateN` rebind stays in scope for later cascade messages.
    pub(super) fn hoist_cascade_splits(
        &mut self,
        splits: Vec<(Vec<ThreadedStmt>, Document<'static>)>,
        docs: &mut Vec<ThreadedStmt>,
        span: beamtalk_core::source_analysis::Span,
    ) -> Vec<Document<'static>> {
        let any_hoisted = splits.iter().any(|(prelude, _)| !prelude.is_empty());
        if !any_hoisted {
            return splits.into_iter().map(|(_, doc)| doc).collect();
        }
        let mut arg_docs = Vec::with_capacity(splits.len());
        for (prelude, doc) in splits {
            if prelude.is_empty() {
                let (binding, var) = self.bind_subexpr_to_temp("CascadeArg", doc);
                docs.push(ThreadedStmt::Statement(binding, span));
                arg_docs.push(leaf::var(var));
            } else {
                docs.extend(prelude);
                arg_docs.push(doc);
            }
        }
        arg_docs
    }
}

#[cfg(test)]
mod tests;
