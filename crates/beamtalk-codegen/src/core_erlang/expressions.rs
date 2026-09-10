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
use super::threaded_ir::{self, ThreadedStmt, ThreadedValue, ValueRef};
use super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{
    CascadeMessage, Expression, Identifier, Literal, MapPair, MessageSelector, StringSegment,
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
                            // Use StateAcc when inside loop body
                            // Hybrid loops use State* naming, not StateAcc*
                            if self.loop_mode.in_hybrid_loop {
                                self.current_state_var()
                            } else if self.in_loop_body {
                                super::util::versioned_var("StateAcc", self.state_version())
                            } else {
                                self.current_state_var()
                            }
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

    /// Generates code for field access (e.g., `self.value`).
    ///
    /// Maps to Erlang `maps:get/2` call:
    /// ```erlang
    /// call 'maps':'get'('value', State)  // Actor context
    /// call 'maps':'get'('value', Self)   // ValueType context
    /// ```
    pub(super) fn generate_field_access(
        &mut self,
        receiver: &Expression,
        field: &Identifier,
    ) -> Result<Document<'static>> {
        // Class methods access class variables directly from ClassVars map
        if self.in_class_method() {
            if let Expression::Identifier(recv_id) = receiver {
                if recv_id.name == "self" && self.class_var_names().contains(field.name.as_str()) {
                    let cv = self.current_class_var();
                    return Ok(docvec![
                        "call 'maps':'get'(",
                        leaf::atom(field.name.to_string()),
                        ", ",
                        leaf::var(cv),
                        ")",
                    ]);
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
                // In hybrid mode, read-only fields are pre-extracted before the letrec.
                // Use the direct parameter variable instead of generating maps:get every iteration.
                if self.loop_mode.in_hybrid_loop {
                    if let Some(param_var) = self
                        .loop_mode
                        .hybrid_readonly_field_params
                        .get(field.name.as_str())
                    {
                        return Ok(leaf::var(param_var.clone()));
                    }
                }
                // Use appropriate variable based on context
                let state_var = match self.context {
                    super::CodeGenContext::ValueType => self.current_self_var(),
                    super::CodeGenContext::Actor => self.current_state_var(),
                    super::CodeGenContext::Repl => "State".to_string(),
                };
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
    /// - **Class method**: `ClassVars{n}` threading, with ADR 0110's shadow write
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
        // Class methods assign to class variables via ClassVars map
        // threading — reached through ordinary `generate_expression`, not
        // `threaded_expression`'s own producer recognition, so
        // `lower_field_write`'s `Closed` render (equivalent to closing the
        // prelude inline) is exactly what's wanted here.
        let (site, frame) = if self.in_class_method() {
            (FieldWriteSite::ClassVar, self.current_frame())
        } else {
            (
                FieldWriteSite::for_context(self.context),
                super::threaded_ir::FrameId::ROOT,
            )
        };
        let (doc, _val_var) =
            self.lower_field_write(site, Closure::Closed, field_name, value, frame)?;
        Ok(doc)
    }

    /// The class-var branch of [`Self::generate_field_assignment`]
    /// (`self.field := value` inside a class method) — extracted to its own
    /// function so the caller stays under clippy's `too_many_lines` budget
    /// alongside its two sibling branches (`ThreadedIr` instrumentation on
    /// those two grows the combined function past the limit).
    ///
    /// ADR 0118 phase 5a: the prelude's trailing `Bind` leaves
    /// `ClassVarsN` bound with no consuming body of its own — callers
    /// splice the prelude into their own frame (§Decision 4) or close it
    /// ([`Self::close_threaded_value_doc`]) so `ClassVarsN` stays visible to
    /// the continuation.
    ///
    /// Delegates the actual `Bind` construction (mint/
    /// version-capture/shadow-write/isolated-verify) to the shared
    /// [`Self::lower_class_var_field_assignment_bind`] — see its own doc
    /// comment for the full ADR 0110 shadow-write rationale — and returns
    /// it as a real, un-rendered [`ThreadedStmt::Bind`] in the prelude;
    /// `gen_server::methods`'s `lower_class_method_last_class_var_bind`
    /// promotes its own copy of this exact sequence to a real top-level
    /// `Bind` instead.
    pub(super) fn generate_class_var_field_assignment(
        &mut self,
        field_name: &str,
        value: &Expression,
        frame: super::threaded_ir::FrameId,
    ) -> Result<ThreadedValue> {
        let span = value.span();
        let (preamble_doc, bind, val_var) =
            self.lower_class_var_field_assignment_bind(field_name, value, frame)?;
        Ok(ThreadedValue {
            prelude: vec![ThreadedStmt::Statement(preamble_doc, span), bind],
            value: ValueRef::Var(val_var),
        })
    }

    /// Shared class-var assignment `Bind` construction —
    /// the `self.classVar := value` shape's core sequence (mint `Val`,
    /// capture `source_version`/`target_version` around
    /// `expression_doc(value)`/`next_class_var()`, derive the ADR 0110
    /// shadow-write gate, construct + isolated-verify the `Bind` via
    /// [`super::threaded_ir::construct_and_verify_class_var_bind`]) —
    /// extracted so [`Self::generate_class_var_field_assignment`] (every
    /// non-last-position or nested class-var assignment, which still
    /// renders its `Bind` immediately and keeps it inside an opaque
    /// `Statement`) and `gen_server::methods`'s
    /// `lower_class_method_last_class_var_bind` (the ONE case
    /// promoted to a real top-level `Bind` node) don't each hand-roll the
    /// same sequence (CLAUDE.md's no-duplicate-implementations rule).
    ///
    /// ADR 0110: shadow write-through so a foreign NLR
    /// (`^` belonging to another method's frame) relayed out of this class
    /// method does not lose the mutation — `invoke_class_method/7` reads
    /// the shadow back on the `{nlr_relay, ...}` path and erases it in
    /// `after` on every path. Gated on `block_depth == 0`: a block literal
    /// written in this method can execute in a *different* class's
    /// `gen_server` process (ADR 0109), where an unconditional write would
    /// corrupt that class's vars with this class's map. Top-frame-only
    /// also matches existing semantics — block-interior class-var
    /// mutations are already discarded on normal return.
    ///
    /// ADR 0110 amendment: keyed by `element(2, ClassSelf)` —
    /// this call's dynamic runtime class identity — not a single shared
    /// key. A mutating self-send inside a block invoked from a foreign
    /// class's process (`block_depth` resets to 0 on entering the
    /// self-sent method's own body) would otherwise write the *same*
    /// global key that process's own class method is using, clobbering it
    /// before that class's `invoke_class_method/7` reads it back.
    /// `element(2, ClassSelf)` (not the static `self.class_name()`) also
    /// keeps an inherited self-dispatch chain (`self otherClassMethod:`)
    /// tagged with the calling subclass's identity, not the defining
    /// ancestor's.
    ///
    /// This is NOT the only class-var write site —
    /// `whileTrue:`/`timesRepeat:` loop bodies (and other state-threaded
    /// constructs) never reach it; a class-var write there goes through
    /// `generate_field_assignment_open` (`dispatch_codegen.rs`), which
    /// threads via the generic State/StateAcc map and has no class-var
    /// branch at all, so `block_depth == 0` never even gets consulted for
    /// that shape. That gap is now a compile-time error
    /// (`CodeGenError::ClassVarAssignmentInThreadedBody`) rather than a
    /// silent runtime no-op — see ADR 0110's amendment above.
    ///
    /// ADR 0111 Phase D completion: this `Bind` is constructed
    /// and isolated-verified through the SAME `threaded_ir::ThreadedStmt::Bind`
    /// a `while_loops.rs`-style caller would — `threaded_ir::render`'s
    /// `BindOp::Put` arm is the only place the ADR 0110 shadow write is
    /// constructed (see [`super::threaded_ir::construct_and_verify_class_var_bind`]'s
    /// doc comment); no second, hand-rolled `Document` reconstructs it.
    ///
    /// Returns `(preamble_doc, bind, val_var)`: `preamble_doc` is `"let
    /// Val = <value> in "` — the caller supplies its own
    /// continuation/glue after (rendering the `Bind` immediately and
    /// appending, or pushing both as separate real `ThreadedStmt`s);
    /// `bind` is the real, not-yet-rendered `ThreadedStmt::Bind`;
    /// `val_var` is the minted temp variable name (both the `Bind`'s
    /// `Put` value and the expression's own logical result).
    ///
    /// `frame` is the real [`threaded_ir::FrameId`] this write's
    /// `Bind` is tagged with — `FrameId::ROOT` for the method's own
    /// top-frame write (`generate_class_var_field_assignment`,
    /// `lower_class_method_last_class_var_bind`), or the loop's real,
    /// already-minted frame (`current_branch_frame()`) for a class-var write
    /// directly inside a Letrec loop body that threads `ClassVars` through
    /// the loop's own recursive tail call (`dispatch_codegen.rs`'s
    /// `generate_field_assignment_open`) — per ADR 0111 Addendum 9, Question
    /// 2's resolution. `shadow_write`/`shadow_write_eligible` stay driven by
    /// `block_depth == 0` regardless of `frame`: a loop body never
    /// increments `block_depth` (it is control flow, not a lexical closure
    /// boundary), so this is `true` there exactly as it is at the method's
    /// own top level.
    pub(super) fn lower_class_var_field_assignment_bind(
        &mut self,
        field_name: &str,
        value: &Expression,
        frame: super::threaded_ir::FrameId,
    ) -> Result<(Document<'static>, super::threaded_ir::ThreadedStmt, String)> {
        if !self.class_var_names().contains(field_name) {
            return Err(CodeGenError::UnsupportedFeature {
                feature: format!(
                    "cannot assign to instance field '{field_name}' in a class method"
                ),
                span: Some(value.span()),
            });
        }
        let val_var = self.fresh_temp_var("Val");
        // The version numbers driving both the verify() call and
        // the real Bind rendered below — captured before/after minting,
        // rather than reconstructed from `current_cv`/`new_cv`-style names
        // by hand.
        let source_version = self.class_var_version();
        let val_doc = self.expression_doc(value)?;
        self.next_class_var();
        let target_version = self.class_var_version();
        let shadow_write = self.block_depth == 0;
        let (bind, verify_errors) = super::threaded_ir::construct_and_verify_class_var_bind(
            super::threaded_ir::BindOp::Put {
                field: field_name.to_string(),
                value: super::threaded_ir::ValueRef::Var(val_var.clone()),
                class_tag: super::threaded_ir::ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write,
            frame,
            self.block_depth == 0, // independently re-derived per ADR 0111 §Verifier honesty — must not reuse `shadow_write`
            source_version,
            target_version,
            value.span(),
        );
        self.report_threaded_ir_verify_errors(
            &verify_errors,
            "class-var mutation missing ADR 0110 shadow write",
            value.span(),
        );
        let preamble_doc = docvec!["let ", leaf::var(val_var.clone()), " = ", val_doc, " in ",];
        Ok((preamble_doc, bind, val_var))
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
        let errors =
            super::threaded_ir::verify_simple_bind(prefix, source_version, target_version, span);
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
    #[allow(clippy::too_many_lines)]
    pub(super) fn generate_cascade(
        &mut self,
        receiver: &Expression,
        messages: &[CascadeMessage],
    ) -> Result<Document<'static>> {
        // ClassBuilder construction cascades with literal
        // block methods get synthesised source setters — methodSource: from
        // methods: (instance side) and classMethodSource: from classMethods:
        // (class side) — so builder-defined classes are indexable by
        // SystemNavigation on both sides.
        let augmented = super::class_builder_source::inject_method_source(receiver, messages);
        let messages: &[CascadeMessage] = augmented.as_deref().unwrap_or(messages);

        if messages.is_empty() {
            // Edge case: cascade with no messages just evaluates to the receiver
            return self.generate_expression(receiver);
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

        // Snapshot the class-var version *before* generating
        // anything — the receiver included — so we can tell, once the last
        // message's own arguments have been generated, whether *any* part of
        // this cascade (the receiver or any message's arguments) hoisted a
        // `ClassVarsN` rebind (`class_var_version` is never rolled back after
        // a hoist — see `split_subexpr_for_preamble`). If so, the whole
        // cascade must stay an *open* let-chain (like an ordinary
        // class-method self-send) rather than a self-contained Document, so
        // the rebind stays visible to the caller instead of being scoped
        // only to this cascade's own value-defining subexpression.
        let class_var_version_before_cascade = self.class_var_version();

        let receiver_var = self.fresh_temp_var("Receiver");
        // Thread the receiver's open scope rather than close
        // it — `underlying_receiver` can itself rebind `ClassVarsN` (e.g. a
        // same-class self-send, or a nested cascade whose own last message
        // hoists a rebind, now that this function can produce one).
        // `closed_expression_doc` would splice `expr_doc, leaf::var(result_var)`
        // in as `recv_doc`'s own value, trapping the `ClassVarsN` binding
        // inside this `let Receiver = ... in` wrapper's closed subexpression —
        // invisible to the rest of `docs` and to the enclosing class method's
        // own closing `class_var_result` tuple, the exact failure class this
        // whole fix addresses, just relocated to receiver position. Instead,
        // splice any preamble into `docs` first (mirroring how
        // `capture_subexpr_sequence`/`bind_args_to_temps` already do this for
        // ordinary message-send receivers/arguments), so a rebind stays
        // visible at the same nesting level as everything else in `docs` —
        // and is caught by the `class_var_version_before_cascade` snapshot
        // above, taken before this call runs.
        let mut seq = self.sequence_call(std::slice::from_ref(&underlying_receiver), "Recv")?;
        let receiver_value_doc = seq.next();
        let receiver_prelude = seq.into_prelude();
        let mut docs: Vec<Document<'static>> = Vec::new();
        if !receiver_prelude.is_empty() {
            docs.push(self.threaded_prelude_doc(&receiver_prelude));
        }
        docs.push(docvec![
            "let ",
            leaf::var(receiver_var.clone()),
            " = ",
            receiver_value_doc,
            " in "
        ]);

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
            let arg_docs = match &builder_ctx {
                Some((bclass, cvars)) if selector.name().as_str() == "classMethods:" => {
                    match arguments {
                        [Expression::MapLiteral { pairs, .. }] => {
                            vec![self.generate_class_methods_map_arg(pairs, bclass, cvars)?]
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
                            let sel_doc = self
                                .generate_cascade_args(std::slice::from_ref(sel_arg), &mut docs)?;
                            let mut combined = sel_doc;
                            combined.push(
                                self.generate_class_method_single_arg(
                                    sel_arg, block, bclass, cvars,
                                )?,
                            );
                            combined
                        }
                        _ => self.generate_cascade_args(arguments, &mut docs)?,
                    }
                }
                _ => self.generate_cascade_args(arguments, &mut docs)?,
            };

            // ADR 0118 phase 5b: once the last message's
            // own (possibly hoisting) args are generated, `class_var_version`
            // reflects every rebind the whole cascade produced. If it
            // advanced, this last send is bound to a named result
            // (`let _CascadeResult = ... in _CascadeResult`) rather than
            // left as the cascade's bare tail value — `generate_cascade` is
            // reached through ordinary `generate_expression`, with no open
            // scope left to propagate, so the rebind is closed inline here
            // instead of escaping via a side channel.
            let last_rebind_result_var =
                if is_last && self.class_var_version() != class_var_version_before_cascade {
                    let result_var = self.fresh_temp_var("CascadeResult");
                    docs.push(docvec!["let ", leaf::var(result_var.clone()), " = "]);
                    Some(result_var)
                } else {
                    if !is_last {
                        // For all but the last message, discard the result
                        docs.push(Document::Str("let _ = "));
                    }
                    None
                };

            docs.push(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(receiver_var.clone()),
                ", ",
                leaf::atom(selector_atom),
                ", [",
            ]);
            for (j, arg_doc) in arg_docs.into_iter().enumerate() {
                if j > 0 {
                    docs.push(Document::Str(", "));
                }
                docs.push(arg_doc);
            }

            docs.push(Document::Str("])"));

            if !is_last || last_rebind_result_var.is_some() {
                docs.push(Document::Str(" in "));
            }

            if let Some(result_var) = last_rebind_result_var {
                docs.push(leaf::var(result_var));
            }
        }

        Ok(Document::Vec(docs))
    }

    /// Generates cascade arguments, hoisting field-assignment bindings to outer scope.
    ///
    /// This is a helper to avoid duplicating the hoisting logic across the
    /// `MessageSend` and fallback branches of `generate_cascade`.
    ///
    /// A non-field-assignment argument that is itself a same-class
    /// class-method call emits an *open* let-chain ending in `... in ` with
    /// no trailing value expression (`ClassVarsN` must stay visible to
    /// subsequent cascade messages — see `emit_class_var_result_unwrap`'s
    /// doc comment), relying on the caller to append the result variable and
    /// keep the chain's bindings in scope. An argument doc placed directly
    /// into a `send(...)` argument list with no such append would leave an
    /// open-scope arg dangling, producing malformed Core Erlang (a
    /// `let ... in` immediately followed by the list's closing `]`).
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
        docs: &mut Vec<Document<'static>>,
    ) -> Result<Vec<Document<'static>>> {
        let mut splits: Vec<(Vec<ThreadedStmt>, Document<'static>)> =
            Vec::with_capacity(arguments.len());
        for arg in arguments {
            if Self::is_field_assignment(arg) {
                let (doc, val_var) = self.generate_field_assignment_open(arg)?;
                splits.push((
                    vec![ThreadedStmt::Statement(doc, arg.unwrap_parens().span())],
                    leaf::var(val_var),
                ));
            } else {
                let mut seq = self.sequence_call(std::slice::from_ref(&arg), "CascadeArg")?;
                let doc = seq.next();
                splits.push((seq.into_prelude(), doc));
            }
        }

        let any_hoisted = splits.iter().any(|(prelude, _)| !prelude.is_empty());
        if !any_hoisted {
            return Ok(splits.into_iter().map(|(_, doc)| doc).collect());
        }
        let mut arg_docs = Vec::with_capacity(splits.len());
        for (prelude, doc) in splits {
            if prelude.is_empty() {
                let (binding, var) = self.bind_subexpr_to_temp("CascadeArg", doc);
                docs.push(binding);
                arg_docs.push(leaf::var(var));
            } else {
                docs.push(self.threaded_prelude_doc(&prelude));
                arg_docs.push(doc);
            }
        }
        Ok(arg_docs)
    }
}

#[cfg(test)]
mod tests;
