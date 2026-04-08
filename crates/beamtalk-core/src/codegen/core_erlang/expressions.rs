// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expression code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This domain service handles code generation for Beamtalk expressions:
//! - Literals (integers, floats, strings, symbols)
//! - Identifiers and variable references
//! - Map literals
//! - Field access (`self.field`)
//! - Field assignment (`self.field := value`)
//! - Blocks (closures)
//! - Await expressions
//! - Cascades
//!
//! Note: Message sending is handled by [`super::dispatch_codegen`].

use std::collections::HashSet;

use super::document::Document;
use super::selector_mangler::escape_atom_chars;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, CascadeMessage,
    Expression, Identifier, Literal, MapPair, MapPatternKey, MatchArm, MessageSelector, Pattern,
    StringSegment,
};
use crate::docvec;

/// Classification of how a block body expression should be handled.
/// Produced by [`CoreErlangGenerator::classify_block_expr`] and consumed
/// by [`CoreErlangGenerator::generate_block_expr`].
enum BlockExprKind {
    /// `{a, b} := expr` or `#[a, b] := expr` — destructure assignment.
    /// Carries `is_last` so the handler can append `'nil'` when the destructure
    /// is the final expression in the block.
    Destructure { is_last: bool },
    /// Last expression that is a class method self-send (BT-1397).
    LastClassMethodSelfSend,
    /// Last expression (general case) — its value is the block's result.
    LastExpr,
    /// `self.field := value` — direct field assignment (non-last).
    FieldAssignment,
    /// `var := expr` — local variable assignment (non-last).
    LocalAssignment,
    /// `whileTrue:` / `whileFalse:` / `timesRepeat:` with threaded vars (non-last).
    ControlFlowWithThreadedVars,
    /// Class method self-send as non-last expression (BT-1397).
    ClassMethodSelfSend,
    /// Expression evaluated for side effects only — result discarded.
    SideEffect,
}

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
            Literal::Integer(n) => Ok(Document::String(n.to_string())),
            Literal::Float(f) => {
                let s = f.to_string();
                // Ensure Core Erlang float literals always contain a decimal point,
                // otherwise Erlang interprets them as integers (e.g. 5.0 → "5" → int 5).
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    Ok(Document::String(s))
                } else {
                    Ok(docvec![Document::String(s), ".0"])
                }
            }
            Literal::String(s) => {
                let result = Self::binary_string_literal(s);
                Ok(docvec![result])
            }
            Literal::Symbol(s) => Ok(docvec!["'", Document::String(s.to_string()), "'"]),
            Literal::Character(c) => Ok(Document::String((*c as u32).to_string())),
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
                        binary_parts.push(Document::String(Self::binary_byte_segments(s)));
                    }
                }
                StringSegment::Interpolation(expr) => {
                    let expr_doc = self.expression_doc(expr)?;
                    let interp_var = self.fresh_temp_var("interpExpr");
                    let raw_str_var = self.fresh_temp_var("interpRaw");
                    let str_var = self.fresh_temp_var("interpStr");

                    // Evaluate the expression
                    let_bindings.push(docvec![
                        "let ",
                        Document::String(interp_var.clone()),
                        " = ",
                        expr_doc,
                        " in ",
                    ]);

                    // Dispatch displayString to convert to binary string (user-facing representation)
                    let_bindings.push(docvec![
                        "let ",
                        Document::String(raw_str_var.clone()),
                        " = call 'beamtalk_message_dispatch':'send'(",
                        Document::String(interp_var.clone()),
                        ", 'displayString', []) in ",
                    ]);

                    // Convert to binary string via display_string, which recursively awaits futures.
                    // This handles both direct futures (actor displayString returns future) and
                    // double-futures (Object.displayString delegates to self printString which is
                    // itself async for actors, returning a nested future).
                    let_bindings.push(docvec![
                        "let ",
                        str_var.clone(),
                        " = call 'beamtalk_primitive':'display_string'(",
                        raw_str_var.clone(),
                        ") in "
                    ]);

                    // Add as binary segment: #<Var>('all',8,'binary',['unsigned'|['big']])
                    binary_parts.push(docvec![
                        "#<",
                        Document::String(str_var),
                        ">('all',8,'binary',['unsigned'|['big']])",
                    ]);
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
                // BT-411: Check if self is explicitly bound (e.g., in class methods)
                if let Some(var_name) = self.lookup_var("self").cloned() {
                    Ok(docvec![var_name])
                } else if self.context == super::CodeGenContext::ValueType {
                    // BT-833: In value type context, self resolves to the latest Self{N}
                    // snapshot after any preceding field assignments.
                    Ok(Document::String(self.current_self_var()))
                } else {
                    Ok(Document::Str("Self")) // self → Self parameter (BT-161)
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
                    Ok(docvec![var_name])
                } else {
                    // BT-1326: In hybrid mode, check if this is a read-only field
                    // accessed implicitly (bare name without self. prefix).
                    if self.in_hybrid_loop {
                        if let Some(param_var) =
                            self.hybrid_readonly_field_params.get(id.name.as_str())
                        {
                            return Ok(Document::String(param_var.clone()));
                        }
                    }
                    // Field access from state/self
                    // BT-213: Context determines which variable to use
                    let state_var = match self.context {
                        super::CodeGenContext::ValueType => {
                            // BT-833: Value types use the latest Self{N} snapshot
                            self.current_self_var()
                        }
                        super::CodeGenContext::Actor | super::CodeGenContext::Repl => {
                            // BT-153: Use StateAcc when inside loop body
                            // BT-1326: Hybrid loops use State* naming, not StateAcc*
                            if self.in_hybrid_loop {
                                self.current_state_var()
                            } else if self.in_loop_body {
                                if self.state_version() == 0 {
                                    "StateAcc".to_string()
                                } else {
                                    format!("StateAcc{}", self.state_version())
                                }
                            } else {
                                self.current_state_var()
                            }
                        }
                    };
                    Ok(docvec![
                        "call 'maps':'get'('",
                        Document::String(id.name.to_string()),
                        "', ",
                        Document::String(state_var),
                        ")",
                    ])
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

        let mut parts: Vec<Document<'static>> = vec![Document::Str("~{ ")];

        for (i, pair) in pairs.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }

            // Generate the key
            parts.push(self.expression_doc(&pair.key)?);
            parts.push(Document::Str(" => "));

            // Generate the value
            parts.push(self.expression_doc(&pair.value)?);
        }

        parts.push(Document::Str(" }~"));
        Ok(Document::Vec(parts))
    }

    /// Generates code for a list literal: `#(1, 2, 3)` → `[1, 2, 3]`
    ///
    /// Cons syntax `#(head | tail)` → `[head | tail]`
    pub(super) fn generate_list_literal(
        &mut self,
        elements: &[Expression],
        tail: Option<&Expression>,
    ) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = vec![Document::Str("[")];
        for (i, elem) in elements.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(self.expression_doc(elem)?);
        }
        if let Some(t) = tail {
            if !elements.is_empty() {
                parts.push(Document::Str(" | "));
            }
            parts.push(self.expression_doc(t)?);
        }
        parts.push(Document::Str("]"));
        Ok(Document::Vec(parts))
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
        let mut parts: Vec<Document<'static>> =
            vec![Document::Str("call 'beamtalk_array':'from_list'([")];
        for (i, elem) in elements.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            // BT-1935: Close any open-scope let-chains from class method self-sends.
            let (doc, open_scope) = self.expression_doc_with_open_scope(elem)?;
            if let Some(result_var) = open_scope {
                parts.push(docvec![doc, Document::String(result_var)]);
            } else {
                parts.push(doc);
            }
        }
        parts.push(Document::Str("])"));
        Ok(Document::Vec(parts))
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
        // BT-412: Class methods access class variables directly from ClassVars map
        if self.in_class_method() {
            if let Expression::Identifier(recv_id) = receiver {
                if recv_id.name == "self" && self.class_var_names().contains(field.name.as_str()) {
                    let cv = self.current_class_var();
                    return Ok(docvec![
                        "call 'maps':'get'('",
                        Document::String(field.name.to_string()),
                        "', ",
                        Document::String(cv),
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
        // For now, assume receiver is 'self' and access from State/Self
        if let Expression::Identifier(recv_id) = receiver {
            if recv_id.name == "self" {
                // BT-1326: In hybrid mode, read-only fields are pre-extracted before the letrec.
                // Use the direct parameter variable instead of generating maps:get every iteration.
                if self.in_hybrid_loop {
                    if let Some(param_var) =
                        self.hybrid_readonly_field_params.get(field.name.as_str())
                    {
                        return Ok(Document::String(param_var.clone()));
                    }
                }
                // BT-213/BT-833: Use appropriate variable based on context
                let state_var = match self.context {
                    super::CodeGenContext::ValueType => self.current_self_var(),
                    super::CodeGenContext::Actor => self.current_state_var(),
                    super::CodeGenContext::Repl => "State".to_string(),
                };
                return Ok(docvec![
                    "call 'maps':'get'('",
                    Document::String(field.name.to_string()),
                    "', ",
                    Document::String(state_var),
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
    /// by context:
    ///
    /// - **Actor context**: `State{n}` threading via `maps:put`
    /// - **`ValueType` context** (BT-833): `Self{n}` threading — each assignment produces
    ///   a new immutable snapshot; `self` in subsequent expressions resolves to `Self{n}`
    ///
    /// ```erlang
    /// let _Val = <value> in
    /// let State{n} = call 'maps':'put'('fieldName', _Val, State{n-1}) in
    /// _Val
    /// ```
    ///
    /// The assignment expression evaluates to the assigned value (Smalltalk semantics).
    pub(super) fn generate_field_assignment(
        &mut self,
        field_name: &str,
        value: &Expression,
    ) -> Result<Document<'static>> {
        // BT-412: Class methods assign to class variables via ClassVars map threading.
        // The generated code leaves `let ClassVarsN = ...` open — the caller (sequential
        // expression handler) must provide the continuation.
        if self.in_class_method() {
            if self.class_var_names().contains(field_name) {
                let val_var = self.fresh_temp_var("Val");
                let current_cv = self.current_class_var();
                let val_doc = self.expression_doc(value)?;
                let new_cv = self.next_class_var();
                let doc = docvec![
                    "let ",
                    Document::String(val_var.clone()),
                    " = ",
                    val_doc,
                    " in let ",
                    Document::String(new_cv.clone()),
                    " = call 'maps':'put'('",
                    Document::String(field_name.to_string()),
                    "', ",
                    Document::String(val_var.clone()),
                    ", ",
                    Document::String(current_cv),
                    ") in ",
                ];
                // Store result var name for callers that need to reference it
                self.last_open_scope_result = Some(val_var);
                return Ok(doc);
            }
            return Err(CodeGenError::UnsupportedFeature {
                feature: format!(
                    "cannot assign to instance field '{field_name}' in a class method"
                ),
                span: Some(value.span()),
            });
        }
        // BT-833: Value type field assignment — Self-threading (immutable update).
        //
        // Mirrors the Actor state-threading pattern but uses Self{N} instead of State{N}.
        // Each `:=` produces a new Self snapshot via `maps:put`, so `self` in subsequent
        // expressions resolves to the latest Self{N} via `current_self_var()`.
        if matches!(self.context, super::CodeGenContext::ValueType) {
            let val_var = self.fresh_temp_var("Val");
            // Capture current Self BEFORE generating the value expression so that
            // RHS field reads (e.g., `self.x := self.x + 1`) see the current snapshot.
            let current_self = self.current_self_var();
            let val_doc = self.expression_doc(value)?;
            let new_self = self.next_self_var();
            let doc = docvec![
                "let ",
                Document::String(val_var.clone()),
                " = ",
                val_doc,
                " in let ",
                Document::String(new_self),
                " = call 'maps':'put'('",
                Document::String(field_name.to_string()),
                "', ",
                Document::String(val_var.clone()),
                ", ",
                Document::String(current_self),
                ") in ",
                Document::String(val_var),
            ];
            return Ok(doc);
        }

        let val_var = self.fresh_temp_var("Val");

        // Capture current state BEFORE generating value expression,
        // because the value expression may reference state (e.g., self.value + 1)
        let current_state = self.current_state_var();

        // Capture value expression (preserves side effects on state)
        let val_doc = self.expression_doc(value)?;

        // Now increment state version for the new state after assignment
        let new_state = self.next_state_var();

        let doc = docvec![
            "let ",
            Document::String(val_var.clone()),
            " = ",
            val_doc,
            " in let ",
            Document::String(new_state),
            " = call 'maps':'put'('",
            Document::String(field_name.to_string()),
            "', ",
            Document::String(val_var.clone()),
            ", ",
            Document::String(current_state),
            ") in ",
            Document::String(val_var),
        ];
        Ok(doc)
    }

    /// Generates code for a block (closure).
    ///
    /// BT-852: Automatically selects Tier 1 (plain) or Tier 2 (stateful) codegen
    /// based on `BlockMutationAnalysis`:
    ///
    /// - **Tier 2 (stateful):** blocks with captured variable mutations emit
    ///   `fun(Params..., StateAcc) -> {Result, NewStateAcc}`
    /// - **Plain fun:** pure blocks (no captured mutations) emit
    ///   `fun(Params...) -> Result` — zero overhead for stateless blocks
    ///
    /// Captured mutations = variables written inside the block that were also
    /// read from the outer scope (i.e. `local_writes ∩ captured_reads`).
    /// Field writes (`self.x := ...`) and self-sends are handled separately:
    /// - Field writes are threaded via `gen_server` State at the method level (BT-1140 for Tier 2).
    /// - Self-sends are pre-scanned via `generate_tier2_self_send_open` (BT-851).
    ///
    /// Extracts a block literal from an expression, unwrapping parentheses.
    ///
    /// Returns `Some(&Block)` for `Expression::Block` and for
    /// `Expression::Parenthesized` wrappers around a block (recursively).
    /// Returns `None` for all other expression kinds (identifiers, message sends, etc.).
    ///
    /// Used by Erlang interop sites to detect block arguments regardless of whether
    /// the caller wrote `[:x | x + 1]` or `([:x | x + 1])`.
    pub(super) fn extract_block_literal(expr: &Expression) -> Option<&Block> {
        match expr {
            Expression::Block(block) => Some(block),
            Expression::Parenthesized { expression, .. } => Self::extract_block_literal(expression),
            _ => None,
        }
    }

    /// This is the Tier 2 promotion check: non-empty → stateful block, empty → pure block.
    /// Centralised here so both `generate_block` and `generate_erlang_interop_wrapper`
    /// use identical logic and cannot drift independently.
    pub(super) fn captured_mutations_for_block(block: &Block) -> Vec<String> {
        use crate::codegen::core_erlang::block_analysis::analyze_block;
        let analysis = analyze_block(block);
        analysis
            .local_writes
            .intersection(&analysis.captured_reads)
            .cloned()
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect()
    }

    /// BT-1213: Returns captured mutation variable names if `expr` is a
    /// `[block] value`/`value:`/etc. with a literal block that mutates outer locals.
    pub(super) fn inline_block_captured_mutations(expr: &Expression) -> Option<Vec<String>> {
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            let is_value_selector = match selector {
                crate::ast::MessageSelector::Unary(name) => name == "value",
                crate::ast::MessageSelector::Keyword(parts) => {
                    let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                    matches!(
                        sel.as_str(),
                        "value:" | "value:value:" | "value:value:value:"
                    )
                }
                crate::ast::MessageSelector::Binary(_) => false,
            };
            if is_value_selector {
                if let Expression::Block(block) = receiver.as_ref() {
                    let mutations = Self::captured_mutations_for_block(block);
                    if !mutations.is_empty() {
                        return Some(mutations);
                    }
                }
            }
        }
        None
    }

    pub(super) fn generate_block(&mut self, block: &Block) -> Result<Document<'static>> {
        let captured_mutations = Self::captured_mutations_for_block(block);

        // BT-852: Blocks with captured local mutations use Tier 2 stateful calling convention.
        if !captured_mutations.is_empty() {
            return self.generate_block_stateful(block, &captured_mutations);
        }

        // Pure block: plain fun (no mutations to thread via Tier 2)
        self.push_scope();
        // BT-1475: Track block nesting so self-cast sends route through the mailbox
        self.block_depth += 1;
        // BT-1550: Save class_var_version so that self-calls inside the closure
        // don't leak ClassVars{N} bindings into the enclosing scope.  The closure
        // is a separate Core Erlang `fun`, so any let-bindings inside it are not
        // visible to the outer method body.
        let saved_class_var_version = self.class_var_version();

        let mut param_parts: Vec<Document<'static>> = Vec::new();
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                param_parts.push(Document::Str(", "));
            }
            let var_name = self.fresh_var(&param.name);
            param_parts.push(Document::String(var_name));
        }
        let header = docvec!["fun (", Document::Vec(param_parts), ") -> "];

        // Generate block body as Document.
        // BT-1475: Ensure block_depth and scope are restored even on error.
        let body_result = self.generate_block_body(block);
        self.block_depth -= 1;
        self.set_class_var_version(saved_class_var_version);
        self.pop_scope();
        Ok(docvec![header, body_result?])
    }

    /// BT-851: Generates a Tier 2 stateful block (ADR 0041 Phase 0).
    ///
    /// Emits a block with the stateful calling convention:
    /// `fun(Param1, ..., ParamN, StateAcc) -> {Result, NewStateAcc}`
    ///
    /// Captured-mutated variables are unpacked from `StateAcc` at the start,
    /// assignments thread through `StateAcc` via `maps:put`, and the final
    /// expression is wrapped in a `{Result, StateAccN}` tuple.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// fun (X, StateAcc) ->
    ///     let Count = call 'maps':'get'('__local__count', StateAcc) in
    ///     let _Sum = call 'erlang':'+'(Count, X) in
    ///     let StateAcc1 = call 'maps':'put'('__local__count', _Sum, StateAcc) in
    ///     {_Sum, StateAcc1}
    /// end
    /// ```
    pub(super) fn generate_block_stateful(
        &mut self,
        block: &Block,
        captured_vars: &[String],
    ) -> Result<Document<'static>> {
        self.push_scope();
        // BT-1475: Track block nesting so self-cast sends route through the mailbox
        self.block_depth += 1;

        // Bind block parameters
        let mut param_parts: Vec<Document<'static>> = Vec::new();
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                param_parts.push(Document::Str(", "));
            }
            let var_name = self.fresh_var(&param.name);
            param_parts.push(Document::String(var_name));
        }

        // Add StateAcc parameter
        if !block.parameters.is_empty() {
            param_parts.push(Document::Str(", "));
        }
        param_parts.push(Document::Str("StateAcc"));

        let header = docvec!["fun (", Document::Vec(param_parts), ") -> "];

        // Set up loop body context for StateAcc-based threading
        let docs = self.with_branch_context(|this| {
            let mut docs: Vec<Document<'static>> = Vec::new();

            // Unpack captured-mutated vars from StateAcc.
            // BT-909: Use maps:get/3 with the current outer value as fallback so the block
            // remains callable even when StateAcc was not pre-seeded with the local state keys
            // (e.g. when called through the runtime arity normalization wrapper).
            for var_name in captured_vars {
                let core_var = Self::to_core_erlang_var(var_name);
                let key = Self::local_state_key(var_name);
                // Look up the outer binding BEFORE rebinding so the fallback refers to the
                // value at block-definition time (not the newly introduced inner binding).
                // If no outer binding exists (e.g. REPL mode where vars come from state dict),
                // fall back to maps:get/2 (original behavior) to avoid referencing unbound vars.
                let outer_binding = this.lookup_var(var_name).cloned();
                this.bind_var(var_name, &core_var);
                if let Some(outer_var) = outer_binding {
                    docs.push(docvec![
                        "let ",
                        Document::String(core_var),
                        " = call 'maps':'get'('",
                        Document::String(key),
                        "', StateAcc, ",
                        Document::String(outer_var),
                        ") in "
                    ]);
                } else {
                    docs.push(docvec![
                        "let ",
                        Document::String(core_var),
                        " = call 'maps':'get'('",
                        Document::String(key),
                        "', StateAcc) in "
                    ]);
                }
            }

            // Generate body expressions with state threading
            this.generate_block_stateful_body(block, &mut docs)?;
            Ok::<_, crate::codegen::core_erlang::CodeGenError>(docs)
        });
        // BT-1475: Ensure block_depth and scope are restored even on error.
        self.block_depth -= 1;
        self.pop_scope();

        Ok(docvec![header, Document::Vec(docs?)])
    }

    /// BT-855: Generates an Erlang-compatible wrapper for a block at an Erlang call site.
    ///
    /// Erlang/Elixir code does not know about the Beamtalk state-threading protocol.
    /// When a Beamtalk block is passed to an Erlang call site (e.g. `lists:map/2`),
    /// this function generates a wrapper that strips the `StateAcc` protocol:
    ///
    /// ```erlang
    /// %% For a stateful block: fun(X, StateAcc) -> {X + Count, StateAcc1}
    /// let _BtBlock = fun(X, StateAcc) -> ... in
    /// fun(X) ->
    ///     let _WT = apply _BtBlock(X, CurrentState) in
    ///     let _WRes = call 'erlang':'element'(1, _WT) in _WRes
    /// ```
    ///
    /// The wrapper captures `CurrentState` from the enclosing scope so that reads of
    /// captured variables succeed. Mutations made inside the block are dropped — the
    /// updated `StateAcc` is not propagated back to the Beamtalk caller.
    ///
    /// For **pure blocks** (no captured mutations), returns the plain Tier 1 block
    /// `fun(Params...) -> Body` unchanged and `is_stateful = false`.
    ///
    /// For **stateful blocks** (captured mutations), returns the wrapper expression
    /// and `is_stateful = true`. Callers should emit a diagnostic warning because
    /// mutations will be silently dropped.
    pub(super) fn generate_erlang_interop_wrapper(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, bool)> {
        // Use the shared helper to ensure consistent Tier 2 promotion logic.
        let captured_mutations = Self::captured_mutations_for_block(block);

        if captured_mutations.is_empty() {
            // Pure block — plain Tier 1 fun, no wrapping needed.
            return Ok((self.generate_block(block)?, false));
        }

        // Stateful block (captured local mutations) — generate Tier 2 block,
        // then wrap it to strip the protocol so Erlang can call it as a plain fun.
        let bt_block_var = self.fresh_temp_var("BtBlock");

        // Generate the Tier 2 block: fun(Params..., StateAcc) -> {Result, NewStateAcc}
        let tier2_doc = self.generate_block_stateful(block, &captured_mutations)?;

        // Seed StateAcc with captured locals before passing to the Tier 2 block.
        // The Tier 2 block expects keys like "__local__count" in the StateAcc for
        // captured mutations. For ValueType context, start with an empty map since
        // there's no State var in the signature. For Actor/Repl, start from current_state.
        let mut seed_state = if matches!(self.context, CodeGenContext::ValueType) {
            self.fresh_temp_var("WStateAcc")
        } else {
            self.current_state_var().clone()
        };
        let mut seed_docs: Vec<Document<'static>> = Vec::new();

        // If ValueType, initialize StateAcc to empty map.
        if matches!(self.context, CodeGenContext::ValueType) {
            seed_docs.push(docvec![
                "let ",
                Document::String(seed_state.clone()),
                " = ~{}~ in "
            ]);
        }

        // Pack captured locals into StateAcc under "__local__*" keys.
        for var_name in &captured_mutations {
            let next_state = self.fresh_temp_var("WStateAcc");
            let key = Self::local_state_key(var_name);

            // BT-857: Generate code to fetch the captured variable.
            // If the variable is bound in the local scope, use it directly.
            // Otherwise, fetch it from the current State map (for REPL/Actor contexts).
            let var_ref = if let Some(bound_var) = self.lookup_var(var_name).cloned() {
                docvec![Document::String(bound_var)]
            } else {
                // Variable not in scope — fetch from State map
                // In REPL context, captured mutations are stored in State (the bindings map)
                let state_var = self.current_state_var();
                docvec![
                    "call 'maps':'get'('",
                    Document::String(var_name.clone()),
                    "', ",
                    Document::String(state_var),
                    ")"
                ]
            };

            seed_docs.push(docvec![
                "let ",
                Document::String(next_state.clone()),
                " = call 'maps':'put'('",
                Document::String(key),
                "', ",
                var_ref,
                ", ",
                Document::String(seed_state),
                ") in "
            ]);
            seed_state = next_state;
        }

        // Fresh parameter names for the wrapper fun (separate scope from the Tier 2 block).
        let n_params = block.parameters.len();
        let wrap_params: Vec<String> = (0..n_params).map(|_| self.fresh_temp_var("WArg")).collect();

        // Build wrapper parameter list: "WArg1, WArg2, ..."
        let mut param_parts: Vec<Document<'static>> = Vec::new();
        for (i, p) in wrap_params.iter().enumerate() {
            if i > 0 {
                param_parts.push(Document::Str(", "));
            }
            param_parts.push(Document::String(p.clone()));
        }

        // Build apply arguments: "WArg1, ..., WArgN, SeedStateAcc"
        let mut apply_parts: Vec<Document<'static>> = Vec::new();
        for (i, p) in wrap_params.iter().enumerate() {
            if i > 0 {
                apply_parts.push(Document::Str(", "));
            }
            apply_parts.push(Document::String(p.clone()));
        }
        if !wrap_params.is_empty() {
            apply_parts.push(Document::Str(", "));
        }
        apply_parts.push(Document::String(seed_state));

        // Emit:
        // let _BtBlock = fun(Params..., StateAcc) -> ... in
        // let WStateAcc0 = ~{}~ in  (if ValueType)
        // let WStateAcc1 = call 'maps':'put'('__local__count', Count, WStateAcc0) in
        // ... (for each captured local)
        // fun(WArg1, ..., WArgN) ->
        //     let _WT = apply _BtBlock(WArg1, ..., WArgN, WStateAccN) in
        //     let _WRes = call 'erlang':'element'(1, _WT) in _WRes
        //
        // NOTE 1: `let {_WRes, _} = apply ...` is invalid Core Erlang inside a fun body
        // (erlc rejects tuple patterns in let). Use element/2 extraction instead.
        // NOTE 2: In Core Erlang, `fun (Params) -> Body` does NOT use `end` to
        // terminate the fun — the Body expression ends the fun.

        let wrap_tuple = self.fresh_temp_var("WT");
        let wrap_result = self.fresh_temp_var("WRes");
        let wrapper_doc = docvec![
            "let ",
            Document::String(bt_block_var.clone()),
            " = ",
            tier2_doc,
            " in ",
            Document::Vec(seed_docs),
            "fun (",
            Document::Vec(param_parts),
            ") -> let ",
            Document::String(wrap_tuple.clone()),
            " = apply ",
            Document::String(bt_block_var),
            " (",
            Document::Vec(apply_parts),
            ") in let ",
            Document::String(wrap_result.clone()),
            " = call 'erlang':'element'(1, ",
            Document::String(wrap_tuple),
            ") in ",
            Document::String(wrap_result),
        ];

        Ok((wrapper_doc, true))
    }

    /// BT-851: Generates the body of a Tier 2 stateful block with state threading.
    #[allow(clippy::too_many_lines)]
    fn generate_block_stateful_body(
        &mut self,
        block: &Block,
        docs: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        let filtered_body = super::util::collect_body_exprs(&block.body);

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;

            if Self::is_field_assignment(expr) {
                let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
                if is_last {
                    // Return the assigned value and updated state.
                    // Extract the field name from the assignment target.
                    let state = self.current_state_var();
                    if let Expression::Assignment { target, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            docs.push(docvec![
                                "{call 'maps':'get'('",
                                Document::String(field.name.to_string()),
                                "', ",
                                Document::String(state.clone()),
                                "), ",
                                Document::String(state),
                                "}"
                            ]);
                        }
                    }
                }
            } else if Self::is_local_var_assignment(expr) {
                let (assign_doc, _val_var) = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);
                if let Expression::Assignment { target, .. } = expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let state = self.current_state_var();
                        let key = Self::local_state_key(&id.name);
                        if is_last {
                            // Last expression is an assignment — result is the assigned value.
                            // Read it from the updated state map (the _Val was put there).
                            docs.push(docvec![
                                "{call 'maps':'get'('",
                                Document::String(key.clone()),
                                "', ",
                                Document::String(state.clone()),
                                "), ",
                                Document::String(state),
                                "}"
                            ]);
                        } else {
                            // Non-last assignment: refresh scope binding so subsequent reads
                            // of this variable see the updated value (not the initial unpack).
                            let fresh = self.fresh_temp_var("Refreshed");
                            self.bind_var(&id.name, &fresh);
                            docs.push(docvec![
                                "let ",
                                Document::String(fresh),
                                " = call 'maps':'get'('",
                                Document::String(key),
                                "', ",
                                Document::String(state),
                                ") in "
                            ]);
                        }
                    }
                }
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    docs.push(d);
                }
                if is_last {
                    let state = self.current_state_var();
                    docs.push(docvec!["{'nil', ", Document::String(state), "}"]);
                }
            } else {
                // Non-assignment expression
                // BT-1397: Detect open-scope results from class method self-sends.
                let (doc, open_scope) = self.expression_doc_with_open_scope(expr)?;
                if is_last {
                    // Wrap result in {Result, StateAcc} tuple
                    let state = self.current_state_var();
                    // BT-1397: If the expression left an open scope, close it with
                    // the result variable then wrap in the Tier 2 tuple.
                    if let Some(result_var) = open_scope {
                        docs.push(docvec![
                            doc,
                            "{",
                            Document::String(result_var),
                            ", ",
                            Document::String(state),
                            "}"
                        ]);
                    } else {
                        let result_var = self.fresh_temp_var("T2Res");
                        docs.push(docvec![
                            "let ",
                            Document::String(result_var.clone()),
                            " = ",
                            doc,
                            " in {",
                            Document::String(result_var),
                            ", ",
                            Document::String(state),
                            "}"
                        ]);
                    }
                } else if let Some(result_var) = open_scope {
                    // BT-1397: Open scope from class method self-send — emit chain
                    // then discard the result.
                    docs.push(docvec![
                        doc,
                        "let _ = ",
                        Document::String(result_var),
                        " in "
                    ]);
                } else {
                    docs.push(Document::Str("let _ = "));
                    docs.push(doc);
                    docs.push(Document::Str(" in "));
                }
            }
        }
        Ok(())
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

        let receiver_var = self.fresh_temp_var("Receiver");
        let recv_doc = self.expression_doc(underlying_receiver)?;
        let mut docs: Vec<Document<'static>> = vec![docvec![
            "let ",
            Document::String(receiver_var.clone()),
            " = ",
            recv_doc,
            " in "
        ]];

        let total_messages = all_messages.len();
        for (index, (selector, arguments)) in all_messages.into_iter().enumerate() {
            let is_last = index == total_messages - 1;

            let selector_atom = selector.to_erlang_atom();
            if matches!(selector, MessageSelector::Binary(_)) {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "binary selectors in cascades".to_string(),
                    span: Some(underlying_receiver.span()),
                });
            }

            // BT-884: Hoist field-assignment arg bindings BEFORE the `let _ =`
            // wrapper so that StateN remains in scope for subsequent messages.
            let arg_docs = self.generate_cascade_args(arguments, &mut docs)?;

            if !is_last {
                // For all but the last message, discard the result
                docs.push(Document::Str("let _ = "));
            }

            docs.push(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                Document::String(receiver_var.clone()),
                ", '",
                Document::String(selector_atom),
                "', [",
            ]);
            for (j, arg_doc) in arg_docs.into_iter().enumerate() {
                if j > 0 {
                    docs.push(Document::Str(", "));
                }
                docs.push(arg_doc);
            }

            docs.push(Document::Str("])"));

            if !is_last {
                docs.push(Document::Str(" in "));
            }
        }

        Ok(Document::Vec(docs))
    }

    /// Generates the body of a block with proper state threading.
    ///
    /// Handles field assignments specially to keep State{n} variables in scope
    /// for subsequent expressions. See inline comments for threading details.
    pub(super) fn generate_block_body(&mut self, block: &Block) -> Result<Document<'static>> {
        if block.body.is_empty() {
            return Ok(Document::Str("'nil'"));
        }

        // Filter out @expect directives — they are compile-time only and generate no code.
        let body = super::util::collect_body_exprs(&block.body);

        self.generate_block_body_slice(&body)
    }

    /// Generates a slice of block body expressions with proper state threading.
    ///
    /// Handles field assignments specially to keep State{n} variables in scope
    /// for subsequent expressions. Called recursively for tuple destructuring.
    ///
    /// # State threading
    ///
    /// For a block like: `[self.value := self.value + 1. ^self.value]`
    /// We need:
    ///   `let _Val1 = ... in let State1 = ... in <return expression>`
    /// NOT:
    ///   `let _seq1 = (let _Val1 = ... in let State1 = ... in _Val1) in <return expression>`
    ///
    /// The difference is crucial: in the first form, `State1` is visible in `<return expression>`.
    ///
    /// For local variable assignments like: `[count := 0. count + 1]`
    /// We need:
    ///   `let Count = 0 in Count + 1`
    pub(super) fn generate_block_body_slice(
        &mut self,
        body: &[&Expression],
    ) -> Result<Document<'static>> {
        if body.is_empty() {
            return Ok(Document::Str("'nil'"));
        }

        let mut docs: Vec<Document<'static>> = Vec::with_capacity(body.len());

        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            let kind = Self::classify_block_expr(self, expr, is_last);
            let doc = self.generate_block_expr(expr, &kind)?;
            docs.push(doc);
        }

        Ok(Document::Vec(docs))
    }

    /// Classify a block body expression for dispatch.
    ///
    /// The order of checks matters: more specific patterns (e.g. destructuring,
    /// field assignment) must come before general ones (e.g. pure expression).
    fn classify_block_expr(&self, expr: &Expression, is_last: bool) -> BlockExprKind {
        if matches!(expr, Expression::DestructureAssignment { .. }) {
            return BlockExprKind::Destructure { is_last };
        }

        if is_last && self.is_class_method_self_send(expr) {
            return BlockExprKind::LastClassMethodSelfSend;
        }

        if is_last {
            return BlockExprKind::LastExpr;
        }

        if Self::is_field_assignment(expr) {
            return BlockExprKind::FieldAssignment;
        }

        if Self::is_local_var_assignment(expr) {
            return BlockExprKind::LocalAssignment;
        }

        if self.get_control_flow_threaded_vars(expr).is_some() {
            return BlockExprKind::ControlFlowWithThreadedVars;
        }

        if self.is_class_method_self_send(expr) {
            return BlockExprKind::ClassMethodSelfSend;
        }

        BlockExprKind::SideEffect
    }

    /// Generate code for a single block body expression, dispatching by kind.
    fn generate_block_expr(
        &mut self,
        expr: &Expression,
        kind: &BlockExprKind,
    ) -> Result<Document<'static>> {
        match *kind {
            BlockExprKind::Destructure { is_last } => {
                self.generate_block_destructure(expr, is_last)
            }
            BlockExprKind::LastClassMethodSelfSend => {
                // BT-1397: Class method self-send as last expression in a block body.
                // The generated code leaves an open scope ending with `in ` — close it
                // with the unwrapped result variable (same pattern as generate_class_method_body).
                let (expr_doc, open_scope) = self.expression_doc_with_open_scope(expr)?;
                if let Some(result_var) = open_scope {
                    Ok(docvec![expr_doc, Document::String(result_var)])
                } else {
                    Ok(expr_doc)
                }
            }
            BlockExprKind::LastExpr => {
                // Last expression: generate directly (its value is the block's result)
                self.generate_expression(expr)
            }
            BlockExprKind::FieldAssignment => {
                // Field assignment not at end: generate WITHOUT closing the value.
                // This leaves the let bindings open for subsequent expressions.
                let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
                Ok(doc)
            }
            BlockExprKind::LocalAssignment => self.generate_block_local_assignment(expr),
            BlockExprKind::ControlFlowWithThreadedVars => {
                self.generate_block_control_flow_threaded(expr)
            }
            BlockExprKind::ClassMethodSelfSend => {
                // BT-1397: Class method self-send as non-last expression in a block body.
                // The generated code leaves an open scope — emit it and discard the result.
                let (expr_doc, open_scope) = self.expression_doc_with_open_scope(expr)?;
                if let Some(result_var) = open_scope {
                    Ok(docvec![
                        expr_doc,
                        "let _Unit = ",
                        Document::String(result_var),
                        " in "
                    ])
                } else {
                    Ok(docvec!["let _Unit = ", expr_doc, " in "])
                }
            }
            BlockExprKind::SideEffect => {
                // Not an assignment or loop - generate and discard result
                let expr_doc = self.expression_doc(expr)?;
                Ok(docvec!["let _Unit = ", expr_doc, " in "])
            }
        }
    }

    /// Handle destructure assignment expressions in block bodies.
    ///
    /// Dispatches to sub-helpers by pattern kind: array, tuple, map.
    /// When `is_last` is true, appends `'nil'` as the block result value since
    /// destructuring only creates bindings and does not produce a value itself.
    fn generate_block_destructure(
        &mut self,
        expr: &Expression,
        is_last: bool,
    ) -> Result<Document<'static>> {
        let Expression::DestructureAssignment { pattern, value, .. } = expr else {
            unreachable!("caller guarantees DestructureAssignment");
        };

        match pattern {
            Pattern::Array { elements, rest, .. } => {
                let mut doc =
                    self.generate_block_array_destructure(elements, rest.as_deref(), value)?;
                if is_last {
                    doc = docvec![doc, "'nil'"];
                }
                Ok(doc)
            }
            Pattern::Tuple { .. } | Pattern::Map { .. } => {
                let mut docs = self.generate_destructure_bindings(pattern, value)?;
                if is_last {
                    docs.push(Document::Str("'nil'"));
                }
                Ok(Document::Vec(docs))
            }
            _ => Err(CodeGenError::UnsupportedFeature {
                feature: "Unsupported destructuring pattern kind".to_string(),
                span: Some(value.span()),
            }),
        }
    }

    /// Generate array destructure bindings in a block body.
    ///
    /// Example: `#[a, b] := expr` generates:
    ///   `let _Arr1 = <expr> in let A = send(_Arr1, 'at:', [1]) in let B = send(...) in`
    /// With rest: `#[a, ...rest] := expr` additionally generates:
    ///   `let Rest = beamtalk_array:slice_from(_Arr1, 2) in`
    fn generate_block_array_destructure(
        &mut self,
        elements: &[Pattern],
        rest: Option<&Pattern>,
        value: &Expression,
    ) -> Result<Document<'static>> {
        let arr_var = self.fresh_temp_var("Arr");
        let val_doc = self.expression_doc(value)?;
        let mut docs = vec![docvec![
            "let ",
            Document::String(arr_var.clone()),
            " = ",
            val_doc,
            " in "
        ]];

        for (idx, elem) in elements.iter().enumerate() {
            let one_based = Document::String((idx + 1).to_string());
            match elem {
                Pattern::Variable(id) => {
                    let core_var = Self::to_core_erlang_var(&id.name);
                    self.bind_var(&id.name, &core_var);
                    docs.push(docvec![
                        "let ",
                        Document::String(core_var),
                        " = call 'beamtalk_message_dispatch':'send'(",
                        Document::String(arr_var.clone()),
                        ", 'at:', [",
                        one_based,
                        "]) in "
                    ]);
                }
                Pattern::Literal(lit, _span) => {
                    // Guard-check: extract element and assert it equals the
                    // literal.  Raises `{badmatch, Array}` on mismatch.
                    let elem_var = self.fresh_temp_var("Elem");
                    let guard_ok_var = self.fresh_temp_var("GuardOk");
                    let mismatch_var = self.fresh_temp_var("Mismatch");
                    let lit_doc = self.generate_literal(lit)?;
                    docs.push(docvec![
                        "let ",
                        Document::String(elem_var.clone()),
                        " = call 'beamtalk_message_dispatch':'send'(",
                        Document::String(arr_var.clone()),
                        ", 'at:', [",
                        one_based,
                        "]) in "
                    ]);
                    docs.push(docvec![
                        "let ",
                        Document::String(guard_ok_var),
                        " = case ",
                        Document::String(elem_var),
                        " of <",
                        lit_doc,
                        "> when 'true' -> 'ok' <",
                        Document::String(mismatch_var),
                        "> when 'true' -> call 'erlang':'error'({'badmatch', ",
                        Document::String(arr_var.clone()),
                        "}) end in "
                    ]);
                }
                Pattern::Wildcard(_) => {} // no binding needed
                _ => {
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "Nested patterns in array destructuring".to_string(),
                        span: Some(elem.span()),
                    });
                }
            }
        }

        // Rest pattern: `...rest` binds remaining elements as a sub-array
        // Pattern::Wildcard — no binding needed, only Variable generates code.
        if let Some(Pattern::Variable(id)) = rest {
            let core_var = Self::to_core_erlang_var(&id.name);
            self.bind_var(&id.name, &core_var);
            let from_idx = Document::String((elements.len() + 1).to_string());
            docs.push(docvec![
                "let ",
                Document::String(core_var),
                " = call 'beamtalk_array':'slice_from'(",
                Document::String(arr_var.clone()),
                ", ",
                from_idx,
                ") in "
            ]);
        }

        Ok(Document::Vec(docs))
    }

    /// Handle local variable assignment in a block body (non-last position).
    fn generate_block_local_assignment(&mut self, expr: &Expression) -> Result<Document<'static>> {
        let Expression::Assignment { target, value, .. } = expr else {
            return Ok(Document::Nil);
        };
        let Expression::Identifier(id) = target.as_ref() else {
            return Ok(Document::Nil);
        };

        // BT-852: Stored blocks with mutations are now supported via Tier 2.
        // generate_block() handles stateful emission; no validation needed here.

        let var_name = &id.name;
        // Determine the Core Erlang variable name:
        // - If the variable is already bound (e.g. block parameter), reuse that Core var.
        // - Otherwise, create a new Core Erlang variable name.
        let core_var = self
            .lookup_var(var_name)
            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
        // Capture the value expression (preserves side effects)
        // Important: capture BEFORE updating the mapping,
        // so that any uses of the variable in the RHS see the previous binding.
        // BT-1397: Detect open-scope results from class method self-sends
        // in the value expression.
        let (val_doc, open_scope) = self.expression_doc_with_open_scope(value)?;
        // Now update the mapping so subsequent expressions see this binding.
        self.bind_var(var_name, &core_var);
        // BT-1397: If the RHS produced an open scope (class method self-send),
        // emit the open scope then bind the variable to its result.
        if let Some(open_scope_result) = open_scope {
            Ok(docvec![
                val_doc,
                "let ",
                Document::String(core_var),
                " = ",
                Document::String(open_scope_result),
                " in "
            ])
        } else {
            Ok(docvec![
                "let ",
                Document::String(core_var.clone()),
                " = ",
                val_doc,
                " in "
            ])
        }
    }

    /// Handle control flow with threaded variables (`whileTrue:`/`whileFalse:`/`timesRepeat:`).
    ///
    /// For single var, the loop returns its final value directly.
    /// For multiple vars, we'd need a tuple (not yet supported).
    fn generate_block_control_flow_threaded(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let threaded_vars = self
            .get_control_flow_threaded_vars(expr)
            .expect("caller guarantees control flow with threaded vars");

        if threaded_vars.len() == 1 {
            let var = &threaded_vars[0];
            // Get the Core Erlang variable name for this var
            let core_var = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            let expr_doc = self.expression_doc(expr)?;
            Ok(docvec![
                "let ",
                Document::String(core_var.clone()),
                " = ",
                expr_doc,
                " in "
            ])
        } else {
            // Multi-var case not supported yet
            Err(CodeGenError::UnsupportedFeature {
                feature: "Multiple threaded variables in control flow".to_string(),
                span: Some(expr.span()),
            })
        }
    }

    /// Generates flat `let` bindings for a destructuring assignment.
    ///
    /// This is the general-purpose helper used by all body-generation loops
    /// (`gen_server` methods, value-type methods, block bodies, etc.) to handle
    /// `DestructureAssignment` in non-last positions.
    ///
    /// For tuples, uses `erlang:element/2` to extract each position.
    /// For arrays, uses `beamtalk_message_dispatch:send` with `at:`.
    ///
    /// Returns a `Vec<Document>` of `let X = ... in` bindings.
    /// Variables are bound in the current scope for subsequent expressions.
    pub(super) fn generate_destructure_bindings(
        &mut self,
        pattern: &Pattern,
        value: &Expression,
    ) -> Result<Vec<Document<'static>>> {
        let (docs, _rhs_var, _bound) =
            self.generate_destructure_extractions(pattern, value, "let ", " in ")?;
        Ok(docs)
    }

    /// Like [`Self::generate_destructure_bindings`] but takes a pre-evaluated variable name
    /// instead of an expression.  Used when the RHS has already been unpacked from a
    /// `{Value, State}` tuple (e.g., `DestructureAssignmentControlFlow`).
    pub(super) fn generate_destructure_bindings_from_var(
        &mut self,
        pattern: &Pattern,
        rhs_var: &str,
    ) -> Result<Vec<Document<'static>>> {
        let (docs, _bound) =
            self.generate_pattern_extractions_from_var(pattern, rhs_var, "let ", " in ")?;
        Ok(docs)
    }

    /// Evaluates `value` into a fresh temporary variable, returning the binding document and var name.
    ///
    /// Shared first step for [`Self::generate_destructure_extractions`] (compiled-code) and
    /// [`CoreErlangGenerator::generate_repl_destructure`] (REPL), which both need to evaluate
    /// the RHS before extracting individual elements.
    ///
    /// Returns `(binding_doc, var_name)` where:
    /// - `binding_doc` is the `<indent><var> = <value><terminator>` document
    /// - `var_name` is the temp variable holding the evaluated RHS
    pub(super) fn eval_rhs_to_temp_var(
        &mut self,
        value: &Expression,
        prefix: &str,
        indent: &'static str,
        terminator: &'static str,
    ) -> Result<(Document<'static>, String)> {
        let var = self.fresh_temp_var(prefix);
        let val_doc = self.expression_doc(value)?;
        let binding_doc = docvec![
            indent,
            Document::String(var.clone()),
            " = ",
            val_doc,
            terminator
        ];
        Ok((binding_doc, var))
    }

    /// Core extraction logic shared between compiled-code and REPL destructuring.
    ///
    /// Evaluates `value` into a fresh temp var via [`Self::eval_rhs_to_temp_var`], then
    /// delegates to [`Self::generate_pattern_extractions_from_var`] for the element bindings.
    ///
    /// The `indent` and `terminator` parameters control formatting:
    /// - Non-REPL: `indent = "let "`, `terminator = " in "` (inline expression style)
    /// - REPL: `indent = "    let "`, `terminator = " in\n"` (module body style)
    ///
    /// Returns `(extraction_docs, rhs_var_name, bound_pairs)` where:
    /// - `extraction_docs` is the flat let-binding chain (RHS eval + element extractions)
    /// - `rhs_var_name` is the Core Erlang var holding the evaluated RHS
    /// - `bound_pairs` is `Vec<(beamtalk_name, core_erlang_var)>` for each `Pattern::Variable` bound
    #[allow(clippy::type_complexity)]
    pub(super) fn generate_destructure_extractions(
        &mut self,
        pattern: &Pattern,
        value: &Expression,
        indent: &'static str,
        terminator: &'static str,
    ) -> Result<(Vec<Document<'static>>, String, Vec<(String, String)>)> {
        let rhs_prefix = match pattern {
            Pattern::Array { .. } => "Arr",
            Pattern::Tuple { .. } => "Tup",
            Pattern::Map { .. } => "Map",
            _ => {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "Unsupported destructuring pattern kind".to_string(),
                    span: Some(pattern.span()),
                });
            }
        };
        let (rhs_doc, rhs_var) =
            self.eval_rhs_to_temp_var(value, rhs_prefix, indent, terminator)?;
        let mut docs = vec![rhs_doc];
        let (extraction_docs, bound_pairs) =
            self.generate_pattern_extractions_from_var(pattern, &rhs_var, indent, terminator)?;
        docs.extend(extraction_docs);
        Ok((docs, rhs_var, bound_pairs))
    }

    /// Generates element-extraction let-bindings for a pattern given a pre-evaluated RHS var.
    ///
    /// Unlike [`Self::generate_destructure_extractions`], this function does **not** evaluate
    /// the RHS expression — the caller is responsible for providing an already-evaluated
    /// `rhs_var` (e.g., after unwrapping a mutation-threaded result in REPL mode).
    ///
    /// Returns `(extraction_docs, bound_pairs)` where:
    /// - `extraction_docs` are the element-extraction let-bindings
    /// - `bound_pairs` is `Vec<(beamtalk_name, core_erlang_var)>` for each `Pattern::Variable` bound
    #[allow(clippy::too_many_lines, clippy::type_complexity)]
    pub(crate) fn generate_pattern_extractions_from_var(
        &mut self,
        pattern: &Pattern,
        rhs_var: &str,
        indent: &'static str,
        terminator: &'static str,
    ) -> Result<(Vec<Document<'static>>, Vec<(String, String)>)> {
        let mut docs: Vec<Document<'static>> = Vec::new();
        let mut bound_pairs: Vec<(String, String)> = Vec::new();

        match pattern {
            Pattern::Array { elements, rest, .. } => {
                for (idx, elem) in elements.iter().enumerate() {
                    let one_based = Document::String((idx + 1).to_string());
                    match elem {
                        Pattern::Variable(id) => {
                            let core_var = Self::to_core_erlang_var(&id.name);
                            self.bind_var(&id.name, &core_var);
                            docs.push(docvec![
                                indent,
                                Document::String(core_var.clone()),
                                " = call 'beamtalk_message_dispatch':'send'(",
                                Document::String(rhs_var.to_string()),
                                ", 'at:', [",
                                one_based,
                                "])",
                                terminator,
                            ]);
                            bound_pairs.push((id.name.to_string(), core_var));
                        }
                        Pattern::Literal(lit, _span) => {
                            // Guard-check: extract the element and assert it equals the literal.
                            // If it doesn't match, raise `{badmatch, Array}` mirroring tuple
                            // literal destructuring.
                            let elem_var = self.fresh_temp_var("Elem");
                            let guard_ok_var = self.fresh_temp_var("GuardOk");
                            let mismatch_var = self.fresh_temp_var("Mismatch");
                            let lit_doc = self.generate_literal(lit)?;
                            docs.push(docvec![
                                indent,
                                Document::String(elem_var.clone()),
                                " = call 'beamtalk_message_dispatch':'send'(",
                                Document::String(rhs_var.to_string()),
                                ", 'at:', [",
                                one_based,
                                "])",
                                terminator,
                            ]);
                            docs.push(docvec![
                                indent,
                                Document::String(guard_ok_var),
                                " = case ",
                                Document::String(elem_var),
                                " of <",
                                lit_doc,
                                "> when 'true' -> 'ok' <",
                                Document::String(mismatch_var),
                                "> when 'true' -> call 'erlang':'error'({'badmatch', ",
                                Document::String(rhs_var.to_string()),
                                "}) end",
                                terminator,
                            ]);
                        }
                        Pattern::Wildcard(_) => {}
                        _ => {
                            return Err(CodeGenError::UnsupportedFeature {
                                feature: "Nested patterns in array destructuring".to_string(),
                                span: Some(elem.span()),
                            });
                        }
                    }
                }
                // Rest pattern: `...rest` binds remaining elements as a sub-array
                if let Some(rest_pat) = rest {
                    if let Pattern::Variable(id) = rest_pat.as_ref() {
                        let core_var = Self::to_core_erlang_var(&id.name);
                        self.bind_var(&id.name, &core_var);
                        let from_idx = Document::String((elements.len() + 1).to_string());
                        docs.push(docvec![
                            indent,
                            Document::String(core_var.clone()),
                            " = call 'beamtalk_array':'slice_from'(",
                            Document::String(rhs_var.to_string()),
                            ", ",
                            from_idx,
                            ")",
                            terminator,
                        ]);
                        bound_pairs.push((id.name.to_string(), core_var));
                    }
                    // Pattern::Wildcard — no binding needed
                }
            }
            Pattern::Tuple { elements, .. } => {
                // Arity check: verify the tuple has the expected number of elements.
                let expected_arity = Document::String(elements.len().to_string());
                let size_ok_var = self.fresh_temp_var("SizeOk");
                let bad_arity_var = self.fresh_temp_var("BadArity");
                docs.push(docvec![
                    indent,
                    Document::String(size_ok_var),
                    " = case call 'erlang':'tuple_size'(",
                    Document::String(rhs_var.to_string()),
                    ") of <",
                    expected_arity,
                    "> when 'true' -> 'ok' <",
                    Document::String(bad_arity_var),
                    "> when 'true' -> call 'erlang':'error'({'badmatch', ",
                    Document::String(rhs_var.to_string()),
                    "}) end",
                    terminator,
                ]);
                for (idx, elem) in elements.iter().enumerate() {
                    let one_based = Document::String((idx + 1).to_string());
                    match elem {
                        Pattern::Variable(id) => {
                            let core_var = Self::to_core_erlang_var(&id.name);
                            self.bind_var(&id.name, &core_var);
                            docs.push(docvec![
                                indent,
                                Document::String(core_var.clone()),
                                " = call 'erlang':'element'(",
                                one_based,
                                ", ",
                                Document::String(rhs_var.to_string()),
                                ")",
                                terminator,
                            ]);
                            bound_pairs.push((id.name.to_string(), core_var));
                        }
                        Pattern::Literal(lit, _span) => {
                            // Guard-check: extract the element and assert it equals the literal.
                            // If it doesn't match, raise `{badmatch, Tuple}` to mirror the arity
                            // check error format above.
                            let elem_var = self.fresh_temp_var("Elem");
                            let guard_ok_var = self.fresh_temp_var("GuardOk");
                            let mismatch_var = self.fresh_temp_var("Mismatch");
                            let lit_doc = self.generate_literal(lit)?;
                            docs.push(docvec![
                                indent,
                                Document::String(elem_var.clone()),
                                " = call 'erlang':'element'(",
                                one_based,
                                ", ",
                                Document::String(rhs_var.to_string()),
                                ")",
                                terminator,
                            ]);
                            docs.push(docvec![
                                indent,
                                Document::String(guard_ok_var),
                                " = case ",
                                Document::String(elem_var),
                                " of <",
                                lit_doc,
                                "> when 'true' -> 'ok' <",
                                Document::String(mismatch_var),
                                "> when 'true' -> call 'erlang':'error'({'badmatch', ",
                                Document::String(rhs_var.to_string()),
                                "}) end",
                                terminator,
                            ]);
                        }
                        Pattern::Wildcard(_) => {}
                        _ => {
                            return Err(CodeGenError::UnsupportedFeature {
                                feature: "Nested patterns in tuple destructuring".to_string(),
                                span: Some(elem.span()),
                            });
                        }
                    }
                }
            }
            Pattern::Map { pairs, .. } => {
                for pair in pairs {
                    match &pair.value {
                        Pattern::Variable(id) => {
                            let core_var = Self::to_core_erlang_var(&id.name);
                            self.bind_var(&id.name, &core_var);
                            let key_doc = Self::map_pattern_key_doc(&pair.key);
                            docs.push(docvec![
                                indent,
                                Document::String(core_var.clone()),
                                " = call 'erlang':'map_get'(",
                                key_doc,
                                ", ",
                                Document::String(rhs_var.to_string()),
                                ")",
                                terminator,
                            ]);
                            bound_pairs.push((id.name.to_string(), core_var));
                        }
                        Pattern::Wildcard(_) => {}
                        _ => {
                            return Err(CodeGenError::UnsupportedFeature {
                                feature: "Nested patterns in map destructuring".to_string(),
                                span: Some(pair.value.span()),
                            });
                        }
                    }
                }
            }
            _ => {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "Unsupported destructuring pattern kind".to_string(),
                    span: Some(pattern.span()),
                });
            }
        }

        Ok((docs, bound_pairs))
    }

    /// Generates cascade arguments, hoisting field-assignment bindings to outer scope.
    ///
    /// BT-884: This is a helper to avoid duplicating the hoisting logic across the
    /// `MessageSend` and fallback branches of `generate_cascade`.
    pub(super) fn generate_cascade_args(
        &mut self,
        arguments: &[Expression],
        docs: &mut Vec<Document<'static>>,
    ) -> Result<Vec<Document<'static>>> {
        let mut arg_docs: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for arg in arguments {
            if Self::is_field_assignment(arg) {
                // Push hoisted binding directly to docs (preserves source order).
                let (doc, val_var) = self.generate_field_assignment_open(arg)?;
                docs.push(doc);
                arg_docs.push(Document::String(val_var));
            } else {
                arg_docs.push(self.expression_doc(arg)?);
            }
        }
        Ok(arg_docs)
    }

    /// Generates code for a match expression.
    ///
    /// `value match: [pattern -> body ...]` compiles to Core Erlang:
    /// `let _Match1 = <value> in case _Match1 of <Pattern1> when Guard1 -> Body1 ... end`
    ///
    /// When the match contains a `Pattern::Array` arm, each arm is compiled as a
    /// chain of conditional case expressions instead of a single native case, because
    /// Beamtalk arrays are opaque tagged maps that cannot be matched structurally in
    /// Core Erlang patterns. See [`Self::generate_match_chain`].
    pub(super) fn generate_match(
        &mut self,
        value: &Expression,
        arms: &[MatchArm],
    ) -> Result<Document<'static>> {
        if arms.is_empty() {
            return Err(CodeGenError::UnsupportedFeature {
                feature: "match expression with no arms".to_string(),
                span: Some(value.span()),
            });
        }

        let match_var = self.fresh_temp_var("Match");
        let value_doc = self.expression_doc(value)?;

        // Guard against unsupported pattern forms reaching array-match codegen.
        for arm in arms {
            if let Pattern::Array {
                list_syntax: true,
                span,
                ..
            } = &arm.pattern
            {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "List-syntax pattern `#(...)` in match: arm is not yet supported"
                        .to_string(),
                    span: Some(*span),
                });
            }
            // Rest patterns in match arms are not yet supported (BT-1251 only adds
            // rest to destructuring assignments).
            if let Pattern::Array {
                rest: Some(_),
                span,
                ..
            } = &arm.pattern
            {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "Rest pattern `...` in match: arm is not yet supported".to_string(),
                    span: Some(*span),
                });
            }
        }

        let has_array_arm = arms
            .iter()
            .any(|arm| matches!(arm.pattern, Pattern::Array { .. }));

        let inner_doc = if has_array_arm {
            self.generate_match_chain(&match_var, arms)?
        } else {
            // All arms use native Core Erlang patterns (literals, variables, tuples, maps).
            let mut parts: Vec<Document<'static>> = Vec::new();
            parts.push(docvec![
                "case ",
                Document::String(match_var.clone()),
                " of "
            ]);
            for (i, arm) in arms.iter().enumerate() {
                if i > 0 {
                    parts.push(Document::Str(" "));
                }
                let pattern_doc = self.generate_pattern(&arm.pattern)?;
                parts.push(Document::Str("<"));
                parts.push(pattern_doc);
                parts.push(Document::Str(">"));
                self.push_scope();
                Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
                    self.bind_var(name, core_var);
                });
                if let Some(guard) = &arm.guard {
                    parts.push(Document::Str(" when "));
                    let guard_doc = self.generate_guard_expression(guard)?;
                    parts.push(guard_doc);
                } else {
                    parts.push(Document::Str(" when 'true'"));
                }
                parts.push(Document::Str(" -> "));
                let body_doc = self.expression_doc(&arm.body)?;
                parts.push(body_doc);
                self.pop_scope();
            }
            parts.push(Document::Str(" end"));
            Document::Vec(parts)
        };

        Ok(docvec![
            "let ",
            Document::String(match_var.clone()),
            " = ",
            value_doc,
            " in ",
            inner_doc
        ])
    }

    /// Compiles match arms as a chain of nested case expressions.
    ///
    /// Used when any arm contains a `Pattern::Array` (opaque tagged map — cannot be
    /// matched natively in Core Erlang). Each arm becomes its own case expression with
    /// a wildcard fallthrough to the next arm.
    ///
    /// For `Pattern::Array` arms the generated structure is:
    /// ```text
    /// case is_map(_Match) of
    ///   <'true'> when 'true' ->
    ///     case maps:get('$beamtalk_class', _Match, 'undefined') of
    ///       <'Array'> when 'true' ->
    ///         case beamtalk_array:size(_Match) of
    ///           <N> when 'true' ->
    ///             let Elem1 = at(_Match, 1) in ... body
    ///           <_NoMatch> when 'true' -> [rest]
    ///         end
    ///       <_NoMatch> when 'true' -> [rest]
    ///     end
    ///   <'false'> when 'true' -> [rest]
    /// end
    /// ```
    ///
    /// For all other arms:
    /// `case _Match of <pattern> when guard -> body <_FT> when 'true' -> [rest] end`
    fn generate_match_chain(
        &mut self,
        match_var: &str,
        arms: &[MatchArm],
    ) -> Result<Document<'static>> {
        if arms.is_empty() {
            return Ok(docvec![
                "call 'erlang':'error'({'case_clause', ",
                Document::String(match_var.to_string()),
                "})"
            ]);
        }

        let arm = &arms[0];
        let rest = &arms[1..];

        if let Pattern::Array { elements, .. } = &arm.pattern {
            // Generate current arm body FIRST so that state_version and temp-var counters
            // are not advanced by later arms before we codegen the current one.
            return self.generate_array_match_arm(match_var, arm, elements, rest);
        }

        // Native arm: case _Match of <pattern> when guard -> body <_FT> when 'true' -> rest end
        // Compute current arm first — body codegen must not see state advanced by later arms.
        let fallthrough_var = self.fresh_temp_var("NoMatch");
        let pattern_doc = self.generate_pattern(&arm.pattern)?;

        self.push_scope();
        Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let guard_part = if let Some(guard) = &arm.guard {
            let gd = self.generate_guard_expression(guard)?;
            docvec![" when ", gd]
        } else {
            Document::Str(" when 'true'")
        };
        let body_doc = self.expression_doc(&arm.body)?;
        self.pop_scope();

        // Rest is generated AFTER the current arm to prevent state leakage.
        let rest_doc = self.generate_match_chain(match_var, rest)?;

        Ok(docvec![
            "case ",
            Document::String(match_var.to_string()),
            " of ",
            "<",
            pattern_doc,
            ">",
            guard_part,
            " -> ",
            body_doc,
            " <",
            Document::String(fallthrough_var),
            "> when 'true' -> ",
            rest_doc,
            " end"
        ])
    }

    /// Compiles a single `Pattern::Array` match arm.
    ///
    /// Emits a three-level conditional check (`is_map` → class → size), then element
    /// extractions via `at:` dispatch. `rest_doc` is cloned for all failure branches.
    ///
    /// Takes `rest_arms` (not a pre-built `rest_doc`) so that the current arm's body
    /// is fully generated before any later arm's codegen advances generator state.
    fn generate_array_match_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        elements: &[Pattern],
        rest_arms: &[MatchArm],
    ) -> Result<Document<'static>> {
        let n = elements.len();

        // Bind all pattern variables (including from nested arrays) into scope
        // before generating guard/body so they can reference the bound names.
        // This MUST happen before rest_arms codegen so state is not leaked.
        self.push_scope();
        Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let guard_doc_opt = if let Some(guard) = &arm.guard {
            Some(self.generate_guard_expression(guard)?)
        } else {
            None
        };
        let body_doc = self.expression_doc(&arm.body)?;
        self.pop_scope();

        // Generate rest AFTER the current arm body to prevent state leakage.
        let rest_doc = self.generate_match_chain(match_var, rest_arms)?;

        // Build body section: element extractions + optional guard check.
        // Use `case 'true' of <'true'> when GUARD ->` (Core Erlang guard position) so that
        // guard evaluation errors silently fail and fall through, matching Erlang guard semantics.
        // A plain `case GUARD of <'true'>` would propagate evaluation errors as exceptions.
        let success_doc = if let Some(guard_doc) = guard_doc_opt {
            docvec![
                "case 'true' of ",
                "<'true'> when ",
                guard_doc,
                " -> ",
                body_doc,
                " <'true'> when 'true' -> ",
                rest_doc.clone(),
                " end"
            ]
        } else {
            body_doc
        };

        // Recursively build element extraction + nested array checks
        let mut bound_vars: HashSet<String> = HashSet::new();
        let inner_body = self.build_array_arm_body(
            match_var,
            elements,
            0,
            success_doc,
            &rest_doc,
            &mut bound_vars,
        )?;

        let no_match_size = self.fresh_temp_var("NoMatch");
        let no_match_class = self.fresh_temp_var("NoMatch");

        Ok(docvec![
            "case call 'erlang':'is_map'(",
            Document::String(match_var.to_string()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            Document::String(match_var.to_string()),
            ", 'undefined') of ",
            "<'Array'> when 'true' -> ",
            "case call 'beamtalk_array':'size'(",
            Document::String(match_var.to_string()),
            ") of ",
            "<",
            Document::String(n.to_string()),
            "> when 'true' -> ",
            inner_body,
            " <",
            Document::String(no_match_size),
            "> when 'true' -> ",
            rest_doc.clone(),
            " end ",
            "<",
            Document::String(no_match_class),
            "> when 'true' -> ",
            rest_doc.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            rest_doc,
            " end"
        ])
    }

    /// Handles a `Pattern::Array` element inside an outer array match arm.
    ///
    /// Extracts the element, verifies it is an `Array` of the right size, then
    /// recurses into its sub-elements before continuing with the outer arm.
    #[allow(clippy::too_many_arguments)]
    fn build_nested_array_element(
        &mut self,
        outer_array_var: &str,
        outer_elements: &[Pattern],
        start: usize,
        one_based: usize,
        inner_elems: &[Pattern],
        continuation: Document<'static>,
        failure_doc: &Document<'static>,
        already_bound: &mut HashSet<String>,
    ) -> Result<Document<'static>> {
        let nested_var = self.fresh_temp_var("ArrElem");
        let n_inner = inner_elems.len();

        let after_nested = self.build_array_arm_body(
            outer_array_var,
            outer_elements,
            start + 1,
            continuation,
            failure_doc,
            already_bound,
        )?;
        let inner_body = self.build_array_arm_body(
            &nested_var,
            inner_elems,
            0,
            after_nested,
            failure_doc,
            already_bound,
        )?;

        let no_match_size_n = self.fresh_temp_var("NoMatch");
        let no_match_class_n = self.fresh_temp_var("NoMatch");

        Ok(docvec![
            "let ",
            Document::String(nested_var.clone()),
            " = call 'beamtalk_message_dispatch':'send'(",
            Document::String(outer_array_var.to_string()),
            ", 'at:', [",
            Document::String(one_based.to_string()),
            "]) in ",
            "case call 'erlang':'is_map'(",
            Document::String(nested_var.clone()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            Document::String(nested_var.clone()),
            ", 'undefined') of ",
            "<'Array'> when 'true' -> ",
            "case call 'beamtalk_array':'size'(",
            Document::String(nested_var.clone()),
            ") of ",
            "<",
            Document::String(n_inner.to_string()),
            "> when 'true' -> ",
            inner_body,
            " <",
            Document::String(no_match_size_n),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end ",
            "<",
            Document::String(no_match_class_n),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            failure_doc.clone(),
            " end"
        ])
    }

    /// Emits the equality-check extraction for a duplicate `Pattern::Variable` in an
    /// array match arm: `let VarDup = at:[N] in case erlang:=:=(Var, VarDup) of ...`
    fn build_array_variable_element(
        &mut self,
        array_var: &str,
        core_var: String,
        one_based: usize,
        next: Document<'static>,
        failure_doc: &Document<'static>,
    ) -> Document<'static> {
        let dup_var = self.fresh_temp_var(&format!("{core_var}Dup"));
        let mismatch_var = self.fresh_temp_var("Mismatch");
        docvec![
            "let ",
            Document::String(dup_var.clone()),
            " = call 'beamtalk_message_dispatch':'send'(",
            Document::String(array_var.to_string()),
            ", 'at:', [",
            Document::String(one_based.to_string()),
            "]) in ",
            "case call 'erlang':'=:='(",
            Document::String(core_var),
            ", ",
            Document::String(dup_var),
            ") of ",
            "<'true'> when 'true' -> ",
            next,
            " <",
            Document::String(mismatch_var),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end"
        ]
    }

    /// Recursively builds element-extraction `let`-bindings for an array pattern arm.
    ///
    /// Handles nested `Pattern::Array` elements by wrapping the continuation in a
    /// sub-array check. Variables are pre-registered in scope by
    /// [`Self::generate_array_match_arm`] via [`Self::collect_pattern_variables`].
    ///
    /// `continuation` is what to execute after all elements are extracted.
    /// `failure_doc` is what to execute if any nested array check fails.
    /// `already_bound` tracks variable names already extracted in this arm; a second
    /// occurrence emits an `erlang:=:=` equality check rather than a new binding.
    fn build_array_arm_body(
        &mut self,
        array_var: &str,
        elements: &[Pattern],
        start: usize,
        continuation: Document<'static>,
        failure_doc: &Document<'static>,
        already_bound: &mut HashSet<String>,
    ) -> Result<Document<'static>> {
        if start >= elements.len() {
            return Ok(continuation);
        }

        let one_based = start + 1;
        match &elements[start] {
            Pattern::Variable(id) => {
                let core_var = Self::to_core_erlang_var(&id.name);
                if already_bound.contains(id.name.as_str()) {
                    // Duplicate: build rest first, then wrap with equality check.
                    let next = self.build_array_arm_body(
                        array_var,
                        elements,
                        start + 1,
                        continuation,
                        failure_doc,
                        already_bound,
                    )?;
                    Ok(self.build_array_variable_element(
                        array_var,
                        core_var,
                        one_based,
                        next,
                        failure_doc,
                    ))
                } else {
                    // First occurrence: register BEFORE recursing so later positions
                    // with the same name are recognised as duplicates.
                    already_bound.insert(id.name.to_string());
                    let next = self.build_array_arm_body(
                        array_var,
                        elements,
                        start + 1,
                        continuation,
                        failure_doc,
                        already_bound,
                    )?;
                    Ok(docvec![
                        "let ",
                        Document::String(core_var),
                        " = call 'beamtalk_message_dispatch':'send'(",
                        Document::String(array_var.to_string()),
                        ", 'at:', [",
                        Document::String(one_based.to_string()),
                        "]) in ",
                        next
                    ])
                }
            }
            Pattern::Wildcard(_) => self.build_array_arm_body(
                array_var,
                elements,
                start + 1,
                continuation,
                failure_doc,
                already_bound,
            ),
            Pattern::Array {
                elements: inner_elems,
                ..
            } => {
                let inner_elems_clone: Vec<Pattern> = inner_elems.clone();
                self.build_nested_array_element(
                    array_var,
                    elements,
                    start,
                    one_based,
                    &inner_elems_clone,
                    continuation,
                    failure_doc,
                    already_bound,
                )
            }
            elem => Err(CodeGenError::UnsupportedFeature {
                feature: "Unsupported pattern in Array match arm element".to_string(),
                span: Some(elem.span()),
            }),
        }
    }

    /// Generates a Core Erlang document for a map pattern key.
    ///
    /// Symbol keys emit an atom: `'key'`.
    /// String keys emit a Core Erlang binary literal via the Document pipeline.
    fn map_pattern_key_doc(key: &MapPatternKey) -> Document<'static> {
        match key {
            MapPatternKey::Symbol(s) => {
                let key_atom = escape_atom_chars(s.as_str());
                docvec!["'", Document::String(key_atom), "'"]
            }
            MapPatternKey::StringLit(s) => {
                docvec![Self::binary_string_literal(s.as_str())]
            }
        }
    }

    /// Generates a Core Erlang pattern from a Pattern AST node.
    pub(super) fn generate_pattern(&mut self, pattern: &Pattern) -> Result<Document<'static>> {
        match pattern {
            Pattern::Wildcard(_) => Ok(Document::Str("_")),
            Pattern::Variable(id) => {
                let var_name = Self::to_core_erlang_var(&id.name);
                Ok(Document::String(var_name))
            }
            Pattern::Literal(lit, _) => self.generate_literal(lit),
            Pattern::Tuple { elements, .. } => {
                let mut parts = vec![Document::Str("{")];
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(", "));
                    }
                    parts.push(self.generate_pattern(elem)?);
                }
                parts.push(Document::Str("}"));
                Ok(Document::Vec(parts))
            }
            Pattern::Array { .. } => {
                // Beamtalk arrays are opaque tagged maps — they cannot appear as a
                // native Core Erlang sub-pattern (inside tuples, lists, binary segments, etc.).
                // Top-level array patterns in `match:` arms are handled by
                // `generate_array_match_arm` and never reach this path.
                Err(CodeGenError::UnsupportedFeature {
                    feature: "Array pattern nested inside a composite native pattern (tuple/list) is not supported".to_string(),
                    span: Some(pattern.span()),
                })
            }
            Pattern::List { elements, tail, .. } => {
                if elements.is_empty() && tail.is_none() {
                    return Ok(Document::Str("[]"));
                }
                let mut parts = vec![Document::Str("[")];
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(", "));
                    }
                    parts.push(self.generate_pattern(elem)?);
                }
                if let Some(tail_pat) = tail {
                    parts.push(Document::Str(" | "));
                    parts.push(self.generate_pattern(tail_pat)?);
                }
                parts.push(Document::Str("]"));
                Ok(Document::Vec(parts))
            }
            Pattern::Binary { segments, .. } => {
                let mut parts = vec![Document::Str("#{")];
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(","));
                    }
                    parts.push(self.generate_binary_segment(seg)?);
                }
                parts.push(Document::Str("}#"));
                Ok(Document::Vec(parts))
            }
            Pattern::Map { pairs, .. } => {
                // Beamtalk Dictionaries are plain Erlang maps — emit a Core Erlang map pattern.
                // `#{#k => v}` compiles to `~{ 'k' := V }~`   (symbol key → atom)
                // `#{"k" => v}` compiles to `~{ <binary literal for "k"> := V }~` (string key → binary)
                // `:=` is the Core Erlang match-binding form.
                if pairs.is_empty() {
                    return Ok(Document::Str("~{}~"));
                }
                let mut parts: Vec<Document<'static>> = vec![Document::Str("~{ ")];
                for (i, pair) in pairs.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::Str(", "));
                    }
                    let key_doc = Self::map_pattern_key_doc(&pair.key);
                    parts.push(key_doc);
                    parts.push(Document::Str(" := "));
                    parts.push(self.generate_pattern(&pair.value)?);
                }
                parts.push(Document::Str(" }~"));
                Ok(Document::Vec(parts))
            }
            Pattern::Constructor {
                class,
                keywords,
                span,
            } => self.generate_constructor_pattern(class, keywords, *span),
        }
    }

    /// Generates a Core Erlang map pattern for a sealed-type constructor pattern.
    ///
    /// Maps `Result ok: v` to:
    /// ```text
    /// ~{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := V}~
    /// ```
    ///
    /// The field mapping is looked up from [`sealed_constructor_fields`].
    fn generate_constructor_pattern(
        &mut self,
        class: &crate::ast::Identifier,
        keywords: &[(crate::ast::Identifier, Pattern)],
        span: crate::source_analysis::Span,
    ) -> Result<Document<'static>> {
        // Build the full selector from keyword parts (e.g. "ok:" or "attempt:identifier:")
        let selector: String = keywords.iter().map(|(kw, _)| kw.name.as_str()).collect();

        let fields = sealed_constructor_fields(&class.name, &selector).ok_or_else(|| {
            CodeGenError::UnsupportedFeature {
                feature: format!(
                    "Constructor pattern `{} {}` — '{}' is not a known sealed type or \
                     '{}' is not a recognised constructor. \
                     Only stdlib sealed types (e.g. Result) support constructor patterns in this release.",
                    class.name, selector, class.name, selector
                ),
                span: Some(span),
            }
        })?;

        // Validate arity
        if keywords.len() != fields.binding_fields.len() {
            return Err(CodeGenError::UnsupportedFeature {
                feature: format!(
                    "Constructor pattern `{} {}` has {} argument(s) but constructor expects {}",
                    class.name,
                    selector,
                    keywords.len(),
                    fields.binding_fields.len()
                ),
                span: Some(span),
            });
        }

        // ~{'$beamtalk_class' := 'ClassName', 'discriminator' := Value, ..., 'field' := Binding}~
        let mut parts: Vec<Document<'static>> = vec![Document::Str("~{")];

        // Class tag
        parts.push(Document::Str("'$beamtalk_class' := '"));
        parts.push(Document::String(class.name.to_string()));
        parts.push(Document::Str("'"));

        // Discriminator fields (fixed values distinguishing variants)
        for (field_name, field_value) in &fields.discriminators {
            parts.push(Document::Str(", '"));
            parts.push(Document::String(field_name.to_string()));
            parts.push(Document::Str("' := "));
            parts.push(Document::String(field_value.to_string()));
        }

        // Binding fields — one per keyword argument. Wildcards still emit the
        // field key (requiring the key to exist in the map) but bind to `_`.
        for ((_, binding_pattern), field_name) in keywords.iter().zip(fields.binding_fields.iter())
        {
            parts.push(Document::Str(", '"));
            parts.push(Document::String(field_name.to_string()));
            parts.push(Document::Str("' := "));
            parts.push(self.generate_pattern(binding_pattern)?);
        }

        parts.push(Document::Str("}~"));
        Ok(Document::Vec(parts))
    }

    /// Generates a Core Erlang binary segment: `#<Value>(Size,Unit,Type,Flags)`.
    ///
    /// Core Erlang binary segment format:
    /// ```text
    /// #<VarName>(Size, Unit, Type, ['flag1'|['flag2']])
    /// ```
    ///
    /// Defaults: type=integer, signedness=unsigned, endianness=big,
    /// size=8 for integer/float, 'all' for binary, 'undefined' for utf8.
    /// Unit=1 for integer/float, 8 for binary, 'undefined' for utf8.
    pub(super) fn generate_binary_segment(
        &mut self,
        seg: &BinarySegment,
    ) -> Result<Document<'static>> {
        // Value: the variable or wildcard
        let value_doc = self.generate_pattern(&seg.value)?;

        // Resolve type (default: integer)
        let seg_type = seg.segment_type.unwrap_or(BinarySegmentType::Integer);

        // Size: explicit, or default based on type
        let size_doc = if let Some(size_expr) = &seg.size {
            self.expression_doc(size_expr)?
        } else {
            match seg_type {
                BinarySegmentType::Binary => Document::Str("'all'"),
                BinarySegmentType::Utf8 => Document::Str("'undefined'"),
                BinarySegmentType::Integer => Document::Str("8"),
                BinarySegmentType::Float => Document::Str("64"),
            }
        };

        // Unit: 8 for binary, 'undefined' for utf8, 1 for integer/float
        let unit_doc = match seg_type {
            BinarySegmentType::Binary => Document::Str("8"),
            BinarySegmentType::Utf8 => Document::Str("'undefined'"),
            BinarySegmentType::Integer | BinarySegmentType::Float => Document::Str("1"),
        };

        // Type atom
        let type_doc = match seg_type {
            BinarySegmentType::Integer => Document::Str("'integer'"),
            BinarySegmentType::Float => Document::Str("'float'"),
            BinarySegmentType::Binary => Document::Str("'binary'"),
            BinarySegmentType::Utf8 => Document::Str("'utf8'"),
        };

        // Flags: cons-cell list ['signedness'|['endianness']]
        // Defaults: unsigned, big
        let sign_str = match seg.signedness.unwrap_or(BinarySignedness::Unsigned) {
            BinarySignedness::Signed => "'signed'",
            BinarySignedness::Unsigned => "'unsigned'",
        };
        let end_str = match seg.endianness.unwrap_or(BinaryEndianness::Big) {
            BinaryEndianness::Big => "'big'",
            BinaryEndianness::Little => "'little'",
            BinaryEndianness::Native => "'native'",
        };
        let flags_doc = docvec![
            "[",
            Document::Str(sign_str),
            "|[",
            Document::Str(end_str),
            "]]"
        ];

        Ok(docvec![
            "#<", value_doc, ">(", size_doc, ",", unit_doc, ",", type_doc, ",", flags_doc, ")"
        ])
    }

    /// Generates a Core Erlang guard expression.
    ///
    /// Core Erlang guards only allow a restricted set of BIFs:
    /// comparisons, arithmetic, and type checks.
    fn generate_guard_expression(&mut self, expr: &Expression) -> Result<Document<'static>> {
        match expr {
            Expression::Literal(lit, _) => self.generate_literal(lit),
            Expression::Identifier(id) => match id.name.as_str() {
                "true" => Ok(Document::Str("'true'")),
                "false" => Ok(Document::Str("'false'")),
                "nil" => Ok(Document::Str("'nil'")),
                _ => {
                    if let Some(var_name) = self.lookup_var(&id.name) {
                        Ok(Document::String(var_name.clone()))
                    } else {
                        let var_name = Self::to_core_erlang_var(&id.name);
                        Ok(Document::String(var_name))
                    }
                }
            },
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                let left = self.generate_guard_expression(receiver)?;
                let right = self.generate_guard_expression(&arguments[0])?;
                let erlang_op = match op.as_str() {
                    ">" => ">",
                    "<" => "<",
                    ">=" => ">=",
                    "<=" => "=<",
                    "=:=" => "=:=",
                    "/=" => "/=",
                    "=/=" => "=/=",
                    "+" => "+",
                    "-" => "-",
                    "*" => "*",
                    "/" => "/",
                    _ => {
                        return Err(CodeGenError::UnsupportedFeature {
                            feature: format!("operator '{op}' in guard expression"),
                            span: Some(expr.span()),
                        });
                    }
                };
                Ok(docvec![
                    "call 'erlang':'",
                    Document::String(erlang_op.to_string()),
                    "'(",
                    left,
                    ", ",
                    right,
                    ")",
                ])
            }
            _ => Err(CodeGenError::UnsupportedFeature {
                feature: "complex guard expression (only comparisons and arithmetic allowed)"
                    .to_string(),
                span: Some(expr.span()),
            }),
        }
    }

    /// Collects all variable names from a pattern and calls the callback
    /// with the Beamtalk name and the corresponding Core Erlang variable name.
    fn collect_pattern_variables(pattern: &Pattern, mut bind: impl FnMut(&str, &str)) {
        Self::collect_pattern_variables_inner(pattern, &mut bind);
    }

    fn collect_pattern_variables_inner(pattern: &Pattern, bind: &mut impl FnMut(&str, &str)) {
        match pattern {
            Pattern::Variable(id) => {
                let core_var = Self::to_core_erlang_var(&id.name);
                bind(&id.name, &core_var);
            }
            Pattern::Tuple { elements, .. } | Pattern::List { elements, .. } => {
                for elem in elements {
                    Self::collect_pattern_variables_inner(elem, bind);
                }
                if let Pattern::List { tail: Some(t), .. } = pattern {
                    Self::collect_pattern_variables_inner(t, bind);
                }
            }
            Pattern::Array { elements, rest, .. } => {
                for elem in elements {
                    Self::collect_pattern_variables_inner(elem, bind);
                }
                if let Some(rest_pat) = rest {
                    Self::collect_pattern_variables_inner(rest_pat, bind);
                }
            }
            Pattern::Binary { segments, .. } => {
                for seg in segments {
                    Self::collect_pattern_variables_inner(&seg.value, bind);
                }
            }
            Pattern::Map { pairs, .. } => {
                for pair in pairs {
                    Self::collect_pattern_variables_inner(&pair.value, bind);
                }
            }
            Pattern::Constructor { keywords, .. } => {
                for (_, binding) in keywords {
                    Self::collect_pattern_variables_inner(binding, bind);
                }
            }
            Pattern::Wildcard(_) | Pattern::Literal(_, _) => {}
        }
    }
}

/// Field layout for a sealed-type constructor pattern (Phase 1: stdlib types only).
///
/// `discriminators` — fixed field values that distinguish variants of the same sealed type
///   (e.g. `isOk => true` for `Result ok:`).
/// `binding_fields` — ordered list of map keys that receive the constructor arguments.
struct ConstructorPatternFields {
    discriminators: Vec<(&'static str, &'static str)>,
    binding_fields: Vec<&'static str>,
}

/// Returns the Core Erlang field layout for a known sealed-type constructor.
///
/// Returns `None` for unknown classes or unknown selectors (caller emits a compile error).
///
/// # Phase 1 scope
/// Only stdlib sealed types are supported. User-defined sealed types require the
/// Phase 2 `[pattern: ...]` annotation (tracked separately).
fn sealed_constructor_fields(class: &str, selector: &str) -> Option<ConstructorPatternFields> {
    match (class, selector) {
        ("Result", "ok:") => Some(ConstructorPatternFields {
            discriminators: vec![("isOk", "'true'")],
            binding_fields: vec!["okValue"],
        }),
        ("Result", "error:") => Some(ConstructorPatternFields {
            discriminators: vec![("isOk", "'false'")],
            binding_fields: vec!["errReason"],
        }),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        Block, BlockParameter, Expression, ExpressionStatement, Identifier, Literal, MapPair,
    };
    use crate::codegen::core_erlang::CoreErlangGenerator;
    use crate::source_analysis::Span;

    fn s() -> Span {
        Span::new(0, 0)
    }

    fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    #[test]
    fn test_generate_empty_map_literal() {
        let mut generator = CoreErlangGenerator::new("test");
        let doc = generator.generate_map_literal(&[]).unwrap();
        assert_eq!(doc.to_pretty_string(), "~{}~");
    }

    #[test]
    fn test_generate_map_literal_with_symbol_key() {
        let mut generator = CoreErlangGenerator::new("test");
        let pairs = vec![MapPair::new(
            Expression::Literal(Literal::Symbol("x".into()), s()),
            Expression::Literal(Literal::Integer(1), s()),
            s(),
        )];
        let doc = generator.generate_map_literal(&pairs).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'x'"),
            "map key should be atom. Got: {output}"
        );
        assert!(
            output.contains("=> 1"),
            "map value should be integer. Got: {output}"
        );
        assert!(
            output.contains("=>"),
            "map should use => syntax. Got: {output}"
        );
    }

    #[test]
    fn test_generate_empty_list_literal() {
        let mut generator = CoreErlangGenerator::new("test");
        let doc = generator.generate_list_literal(&[], None).unwrap();
        assert_eq!(doc.to_pretty_string(), "[]");
    }

    #[test]
    fn test_generate_list_literal_with_elements() {
        let mut generator = CoreErlangGenerator::new("test");
        let elements = vec![
            Expression::Literal(Literal::Integer(1), s()),
            Expression::Literal(Literal::Integer(2), s()),
        ];
        let doc = generator.generate_list_literal(&elements, None).unwrap();
        assert_eq!(doc.to_pretty_string(), "[1, 2]");
    }

    #[test]
    fn test_generate_array_literal_calls_from_list() {
        let mut generator = CoreErlangGenerator::new("test");
        let elements = vec![Expression::Literal(Literal::Integer(42), s())];
        let doc = generator.generate_array_literal(&elements).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_array':'from_list'"),
            "array should call from_list. Got: {output}"
        );
        assert!(
            output.contains("42"),
            "array should contain element. Got: {output}"
        );
    }

    #[test]
    fn test_generate_identifier_reserved_words() {
        let mut generator = CoreErlangGenerator::new("test");
        let t = generator
            .generate_identifier(&Identifier::new("true", s()))
            .unwrap();
        assert_eq!(t.to_pretty_string(), "'true'");
        let f = generator
            .generate_identifier(&Identifier::new("false", s()))
            .unwrap();
        assert_eq!(f.to_pretty_string(), "'false'");
        let n = generator
            .generate_identifier(&Identifier::new("nil", s()))
            .unwrap();
        assert_eq!(n.to_pretty_string(), "'nil'");
    }

    #[test]
    fn test_generate_block_no_params() {
        let mut generator = CoreErlangGenerator::new("test");
        let block = Block::new(
            vec![],
            vec![bare(Expression::Literal(Literal::Integer(99), s()))],
            s(),
        );
        let doc = generator.generate_block(&block).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("fun ()"),
            "block header should be 'fun ()'. Got: {output}"
        );
        assert!(
            output.contains("99"),
            "block body should contain literal. Got: {output}"
        );
    }

    #[test]
    fn test_generate_block_with_param() {
        let mut generator = CoreErlangGenerator::new("test");
        let block = Block::new(
            vec![BlockParameter::new("x", s())],
            vec![bare(Expression::Literal(Literal::Integer(0), s()))],
            s(),
        );
        let doc = generator.generate_block(&block).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("fun ("),
            "block should start with 'fun ('. Got: {output}"
        );
        assert!(
            output.contains("->"),
            "block should have arrow. Got: {output}"
        );
    }
}
