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

use super::document::Document;
use super::{CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{
    Block, CascadeMessage, Expression, Identifier, Literal, MapPair, MatchArm, MessageSelector,
    Pattern, StringSegment,
};
use crate::docvec;

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
            Literal::Float(f) => Ok(Document::String(f.to_string())),
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
    /// - Expression segments → evaluate, dispatch `printString`, insert as binary segment
    ///
    /// Example: `"Hello, {name}!"` compiles to:
    /// ```text
    /// let _interpExpr1 = Name in
    ///   let _interpRaw2 = call 'beamtalk_message_dispatch':'send'(_interpExpr1, 'printString', []) in
    ///     let _interpStr3 =
    ///       case call 'beamtalk_future':'is_future'(_interpRaw2) of
    ///         <'true'>  when 'true' -> call 'beamtalk_future':'await'(_interpRaw2)
    ///         <'false'> when 'true' -> _interpRaw2
    ///       end
    ///     in
    ///       #{#<72>(8,1,...), ..., #<_interpStr3>('all',8,'binary',...), #<33>(8,1,...)}#
    /// ```
    pub(super) fn generate_string_interpolation(
        &mut self,
        segments: &[StringSegment],
    ) -> Result<Document<'static>> {
        // Collect let-bindings for expression segments and binary segments
        let mut let_bindings: Vec<Document<'static>> = Vec::new();
        let mut binary_parts: Vec<Document<'static>> = Vec::new();

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

                    // Dispatch printString to convert to binary string
                    let_bindings.push(docvec![
                        "let ",
                        Document::String(raw_str_var.clone()),
                        " = call 'beamtalk_message_dispatch':'send'(",
                        Document::String(interp_var.clone()),
                        ", 'printString', []) in ",
                    ]);

                    // Auto-await if the result is a tagged future (actor dispatch returns {beamtalk_future, Pid})
                    let_bindings.push(docvec![
                        "let ",
                        str_var.clone(),
                        " = case call 'beamtalk_future':'is_future'(",
                        raw_str_var.clone(),
                        ") of <'true'> when 'true' -> call 'beamtalk_future':'await'(",
                        raw_str_var.clone(),
                        ") <'false'> when 'true' -> ",
                        raw_str_var.clone(),
                        " end in "
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
                    location: format!("byte offset {}", id.span.start()),
                })
            }
            _ => {
                // Check if it's a bound variable in current or outer scopes
                if let Some(var_name) = self.lookup_var(id.name.as_str()).cloned() {
                    Ok(docvec![var_name])
                } else {
                    // Field access from state/self
                    // BT-213: Context determines which variable to use
                    let state_var = match self.context {
                        super::CodeGenContext::ValueType => {
                            // BT-833: Value types use the latest Self{N} snapshot
                            self.current_self_var()
                        }
                        super::CodeGenContext::Actor => {
                            // Actors use State with threading
                            // BT-153: Use StateAcc when inside loop body
                            if self.in_loop_body {
                                if self.state_version() == 0 {
                                    "StateAcc".to_string()
                                } else {
                                    format!("StateAcc{}", self.state_version())
                                }
                            } else {
                                self.current_state_var()
                            }
                        }
                        super::CodeGenContext::Repl => {
                            // REPL uses State from bindings, but StateAcc in loops
                            // BT-153: Use StateAcc when inside loop body
                            if self.in_loop_body {
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
    /// Compiles to a call to `beamtalk_array_ops:from_list/1`:
    /// ```erlang
    /// call 'beamtalk_array_ops':'from_list'([1, 2, 3])
    /// ```
    pub(super) fn generate_array_literal(
        &mut self,
        elements: &[Expression],
    ) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> =
            vec![Document::Str("call 'beamtalk_array_ops':'from_list'([")];
        for (i, elem) in elements.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(self.expression_doc(elem)?);
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
        if self.in_class_method {
            if let Expression::Identifier(recv_id) = receiver {
                if recv_id.name == "self" && self.class_var_names.contains(field.name.as_str()) {
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
                location: format!("{:?}", field.span),
            });
        }
        // For now, assume receiver is 'self' and access from State/Self
        if let Expression::Identifier(recv_id) = receiver {
            if recv_id.name == "self" {
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

        Err(CodeGenError::UnsupportedFeature {
            feature: "complex field access".to_string(),
            location: format!("{:?}", receiver.span()),
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
        if self.in_class_method {
            if self.class_var_names.contains(field_name) {
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
                location: "class method body".to_string(),
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
    /// - **Tier 1 (plain):** pure blocks (no captured mutations) emit
    ///   `fun(Params...) -> Result` — zero overhead path unchanged
    ///
    /// Captured mutations = variables written inside the block that were also
    /// read from the outer scope (i.e. `local_writes ∩ captured_reads`).
    /// Field writes (`self.x := ...`) and self-sends are handled separately:
    /// - Field writes are threaded via `gen_server` State at the method level (BT-860 for Tier 2).
    /// - Self-sends are pre-scanned via `generate_tier2_self_send_open` (BT-851).
    ///
    /// Tier 1 blocks handled by dedicated generators (whileTrue:, do:, collect:, etc.)
    /// never reach this function — they call `generate_block_body()` directly.
    pub(super) fn generate_block(&mut self, block: &Block) -> Result<Document<'static>> {
        use crate::codegen::core_erlang::block_analysis::analyze_block;

        let analysis = analyze_block(block);

        // Captured mutations: variables read from outer scope AND written in block
        let captured_mutations: Vec<String> = analysis
            .local_writes
            .intersection(&analysis.captured_reads)
            .cloned()
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();

        // BT-852: Blocks with captured local mutations use Tier 2 stateful calling convention.
        // Field-write-only blocks (self.x := ...) are NOT yet promoted to Tier 2 here because
        // the caller side (detect_tier2_self_send, HOM dispatch) does not yet detect them as
        // Tier 2 — promoting them would emit {Result, NewState} tuples that callers unpack as
        // plain values, causing a runtime crash. Field-write Tier 2 support is tracked in BT-860.
        if !captured_mutations.is_empty() {
            return self.generate_block_stateful(block, &captured_mutations);
        }

        // Pure block: plain fun (Tier 1 optimization)
        self.push_scope();

        let mut param_parts: Vec<Document<'static>> = Vec::new();
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                param_parts.push(Document::Str(", "));
            }
            let var_name = self.fresh_var(&param.name);
            param_parts.push(Document::String(var_name));
        }
        let header = docvec!["fun (", Document::Vec(param_parts), ") -> "];

        // Generate block body as Document
        let body_doc = self.generate_block_body(block)?;

        // Pop the scope when done with the block
        self.pop_scope();
        Ok(docvec![header, body_doc])
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
        let saved_state_version = self.state_version();
        self.set_state_version(0);
        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Unpack captured-mutated vars from StateAcc
        for var_name in captured_vars {
            let core_var = Self::to_core_erlang_var(var_name);
            let key = Self::local_state_key(var_name);
            self.bind_var(var_name, &core_var);
            docs.push(docvec![
                "let ",
                Document::String(core_var),
                " = call 'maps':'get'('",
                Document::String(key),
                "', StateAcc) in "
            ]);
        }

        // Generate body expressions with state threading
        self.generate_block_stateful_body(block, &mut docs)?;

        // Restore state
        self.set_state_version(saved_state_version);
        self.in_loop_body = previous_in_loop_body;
        self.pop_scope();

        Ok(docvec![header, Document::Vec(docs)])
    }

    /// BT-851: Generates the body of a Tier 2 stateful block with state threading.
    fn generate_block_stateful_body(
        &mut self,
        block: &Block,
        docs: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        let filtered_body: Vec<&Expression> = block
            .body
            .iter()
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;

            if Self::is_field_assignment(expr) {
                let doc = self.generate_field_assignment_open(expr)?;
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
                let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
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
            } else {
                // Non-assignment expression
                if !is_last {
                    docs.push(Document::Str("let _ = "));
                }
                let doc = self.generate_expression(expr)?;
                if is_last {
                    // Wrap result in {Result, StateAcc} tuple
                    let result_var = self.fresh_temp_var("T2Res");
                    let state = self.current_state_var();
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
                } else {
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
        if let Expression::MessageSend {
            receiver: underlying_receiver,
            selector: first_selector,
            arguments: first_arguments,
            ..
        } = receiver
        {
            let receiver_var = self.fresh_temp_var("Receiver");
            let recv_doc = self.expression_doc(underlying_receiver)?;
            let mut docs: Vec<Document<'static>> = vec![docvec![
                "let ",
                Document::String(receiver_var.clone()),
                " = ",
                recv_doc,
                " in "
            ]];

            // Total number of messages in the cascade: first + remaining
            let total_messages = messages.len() + 1;

            for index in 0..total_messages {
                let is_last = index == total_messages - 1;

                // Determine which selector/arguments to use:
                // index 0 -> first message from the initial MessageSend
                // index > 0 -> messages[index - 1]
                let (selector, arguments): (&MessageSelector, &[Expression]) = if index == 0 {
                    (first_selector, first_arguments.as_slice())
                } else {
                    let msg = &messages[index - 1];
                    (&msg.selector, msg.arguments.as_slice())
                };

                // Unified message dispatch to the bound receiver
                let selector_atom = selector.to_erlang_atom();
                if matches!(selector, MessageSelector::Binary(_)) {
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "binary selectors in cascades".to_string(),
                        location: "cascade message with binary selector".to_string(),
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
        } else {
            // Fallback: if the receiver is not a MessageSend (which should not
            // happen for well-formed cascades), preserve the previous behavior:
            // evaluate the receiver once and send all cascade messages to it.
            let receiver_var = self.fresh_temp_var("Receiver");
            let recv_doc = self.expression_doc(receiver)?;
            let mut docs: Vec<Document<'static>> = vec![docvec![
                "let ",
                Document::String(receiver_var.clone()),
                " = ",
                recv_doc,
                " in "
            ]];

            // Generate each message send, discarding intermediate results
            for (i, message) in messages.iter().enumerate() {
                let is_last = i == messages.len() - 1;

                // Unified message dispatch to the bound receiver
                let selector_atom = message.selector.to_erlang_atom();
                if matches!(message.selector, MessageSelector::Binary(_)) {
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "binary selectors in cascades".to_string(),
                        location: "cascade message with binary selector".to_string(),
                    });
                }

                // BT-884: Hoist field-assignment arg bindings BEFORE `let _ =`.
                let arg_docs = self.generate_cascade_args(&message.arguments, &mut docs)?;

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
    }

    /// Generates the body of a block with proper state threading.
    ///
    /// Handles field assignments specially to keep State{n} variables in scope
    /// for subsequent expressions. See inline comments for threading details.
    pub(super) fn generate_block_body(&mut self, block: &Block) -> Result<Document<'static>> {
        if block.body.is_empty() {
            return Ok(Document::Str("'nil'"));
        }

        // Generate body expressions in sequence
        // For state threading to work correctly, field assignments must leave
        // their let bindings OPEN so that State{n} is visible to subsequent expressions.
        //
        // For a block like: [self.value := self.value + 1. ^self.value]
        // We need:
        //   let _Val1 = ... in let State1 = ... in <return expression>
        // NOT:
        //   let _seq1 = (let _Val1 = ... in let State1 = ... in _Val1) in <return expression>
        //
        // The difference is crucial: in the first form, State1 is visible in <return expression>.
        //
        // Similarly, for local variable assignments like: [count := 0. count + 1]
        // We need:
        //   let Count = 0 in Count + 1
        // NOT:
        //   let _seq1 = 0 in <expression that can't see Count>

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Filter out @expect directives — they are compile-time only and generate no code.
        let body: Vec<&Expression> = block
            .body
            .iter()
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();

        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);
            let is_local_assignment = Self::is_local_var_assignment(expr);

            if is_last {
                // Last expression: generate directly (its value is the block's result)
                docs.push(self.generate_expression(expr)?);
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                // This leaves the let bindings open for subsequent expressions
                docs.push(self.generate_field_assignment_open(expr)?);
            } else if is_local_assignment {
                // Local variable assignment: generate with proper binding
                if let Expression::Assignment { target, value, .. } = expr {
                    // BT-852: Stored blocks with mutations are now supported via Tier 2.
                    // generate_block() handles stateful emission; no validation needed here.

                    if let Expression::Identifier(id) = target.as_ref() {
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
                        let val_doc = self.expression_doc(value)?;
                        // Now update the mapping so subsequent expressions see this binding.
                        self.bind_var(var_name, &core_var);
                        docs.push(docvec![
                            "let ",
                            Document::String(core_var.clone()),
                            " = ",
                            val_doc,
                            " in "
                        ]);
                    }
                }
            } else if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(expr) {
                // whileTrue:/whileFalse:/timesRepeat: with mutations - need to rebind threaded vars after loop
                // For single var, the loop returns its final value directly
                // For multiple vars, we'd need a tuple (not yet supported)
                if threaded_vars.len() == 1 {
                    let var = &threaded_vars[0];
                    // Get the Core Erlang variable name for this var
                    let core_var = self
                        .lookup_var(var)
                        .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                    let expr_doc = self.expression_doc(expr)?;
                    docs.push(docvec![
                        "let ",
                        Document::String(core_var.clone()),
                        " = ",
                        expr_doc,
                        " in "
                    ]);
                } else {
                    // Multi-var case not supported yet
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "Multiple threaded variables in control flow".to_string(),
                        location: "generate_block_body".to_string(),
                    });
                }
            } else {
                // Not an assignment or loop - generate and discard result
                let expr_doc = self.expression_doc(expr)?;
                docs.push(docvec!["let _Unit = ", expr_doc, " in "]);
            }
        }

        Ok(Document::Vec(docs))
    }

    /// Generates cascade arguments, hoisting field-assignment bindings to outer scope.
    ///
    /// BT-884: This is a helper to avoid duplicating the hoisting logic across the
    /// `MessageSend` and fallback branches of `generate_cascade`.
    fn generate_cascade_args(
        &mut self,
        arguments: &[Expression],
        docs: &mut Vec<Document<'static>>,
    ) -> Result<Vec<Document<'static>>> {
        let mut hoisted_docs: Vec<Document<'static>> = Vec::new();
        let mut arg_docs: Vec<Document<'static>> = Vec::new();
        for arg in arguments {
            if Self::is_field_assignment(arg) {
                hoisted_docs.push(self.generate_field_assignment_open(arg)?);
                let val_var = self
                    .last_open_scope_result
                    .take()
                    .expect("generate_field_assignment_open should set last_open_scope_result");
                arg_docs.push(Document::String(val_var));
            } else {
                arg_docs.push(self.expression_doc(arg)?);
            }
        }
        for hoisted in hoisted_docs {
            docs.push(hoisted);
        }
        Ok(arg_docs)
    }

    /// Generates code for a match expression.
    ///
    /// `value match: [pattern -> body ...]` compiles to Core Erlang:
    /// `let _Match1 = <value> in case _Match1 of <Pattern1> when Guard1 -> Body1 ... end`
    pub(super) fn generate_match(
        &mut self,
        value: &Expression,
        arms: &[MatchArm],
    ) -> Result<Document<'static>> {
        if arms.is_empty() {
            return Err(CodeGenError::UnsupportedFeature {
                feature: "match expression with no arms".to_string(),
                location: format!("{:?}", value.span()),
            });
        }

        let match_var = self.fresh_temp_var("Match");
        let value_doc = self.expression_doc(value)?;

        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(docvec!["let ", Document::String(match_var.clone()), " = "]);
        parts.push(value_doc);
        parts.push(docvec![" in case ", Document::String(match_var), " of "]);

        for (i, arm) in arms.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(" "));
            }

            // Generate pattern
            let pattern_doc = self.generate_pattern(&arm.pattern)?;
            parts.push(Document::Str("<"));
            parts.push(pattern_doc);
            parts.push(Document::Str(">"));

            // Push scope and bind pattern variables so the guard and body
            // can reference them directly instead of looking up the bindings map.
            self.push_scope();
            Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
                self.bind_var(name, core_var);
            });

            // Generate guard
            if let Some(guard) = &arm.guard {
                parts.push(Document::Str(" when "));
                let guard_doc = self.generate_guard_expression(guard)?;
                parts.push(guard_doc);
            } else {
                parts.push(Document::Str(" when 'true'"));
            }

            // Generate body
            parts.push(Document::Str(" -> "));
            let body_doc = self.expression_doc(&arm.body)?;
            parts.push(body_doc);

            self.pop_scope();
        }

        parts.push(Document::Str(" end"));
        Ok(Document::Vec(parts))
    }

    /// Generates a Core Erlang pattern from a Pattern AST node.
    fn generate_pattern(&self, pattern: &Pattern) -> Result<Document<'static>> {
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
            Pattern::Binary { .. } => Err(CodeGenError::UnsupportedFeature {
                feature: "binary pattern matching".to_string(),
                location: format!("{:?}", pattern.span()),
            }),
        }
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
                            location: format!("{:?}", expr.span()),
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
                location: format!("{:?}", expr.span()),
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
            Pattern::Wildcard(_) | Pattern::Literal(_, _) | Pattern::Binary { .. } => {}
        }
    }
}
