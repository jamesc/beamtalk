// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compiler intrinsics for language-level constructs.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! These are message patterns that the compiler must generate inline code for,
//! regardless of receiver type. They represent language-level semantics, not
//! type-specific dispatch:
//!
//! - **Block evaluation**: `value`, `whileTrue:`, `repeat` → Function application & loops
//! - **`ProtoObject`**: `class` → Type introspection via pattern matching
//! - **Object protocol** (split into domain-aligned groups):
//!   - **Nil protocol**: `isNil`, `notNil`, `ifNil:`, `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:` → Nil-testing boolean protocol
//!   - **Error signaling**: `error:` → Error construction and signaling
//!   - **Object identity**: `yourself`, `hash` → Identity and representation
//!   - **Object reflection**: `respondsTo:`, `fieldNames`, `fieldAt:`, `fieldAt:put:` → Runtime introspection
//! - **Dynamic dispatch**: `perform:`, `perform:withArguments:` → Runtime type-based dispatch (actors → async/Future, primitives → sync/value)
//!
//! Unlike type-specific dispatch (which goes through `beamtalk_primitive:send/3`
//! at runtime), these intrinsics generate efficient inline code because they are
//! fundamental language operations that cannot be deferred to runtime dispatch.

use super::document::{Document, join, leaf};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression, MessageSelector, WellKnownSelector};
use crate::docvec;

/// Hint shown when a structural intrinsic reached via generic dispatch
/// (`perform:`/`perform:withArguments:`) can't tell — from an
/// `erlang:is_function/2` arity check alone — whether it's looking at a
/// genuine Tier 2 (stateful, ADR-0041) block or a Tier 1 block called with
/// the wrong argument count. Shared by BT-2812's `value*` fallback, BT-2908's
/// loop/exception-handling fallbacks (`while_loops.rs`, `exception_handling.rs`),
/// and BT-2888's List/Collection guard (which uses its own, differently-worded
/// variant — see `generate_stateful_block_guard`).
pub(in crate::codegen::core_erlang) const STATEFUL_BLOCK_DISPATCH_HINT: &str = "Wrong argument count, or the block captures mutable state and must be invoked directly instead of via perform:";

/// Returns the arity of a block expression, or `None` if the expression is not a block literal.
fn block_arity(expr: &Expression) -> Option<usize> {
    match expr {
        Expression::Block(b) => Some(b.parameters.len()),
        _ => None,
    }
}

/// BT-493: Validates that a block has exactly the expected arity.
/// Non-literal blocks are assumed correct (can't check at compile time).
/// Returns `Ok(())` if valid, `Err(BlockArityError)` if wrong arity.
pub(in crate::codegen::core_erlang) fn validate_block_arity_exact(
    expr: &Expression,
    expected: usize,
    selector: &str,
    hint: &str,
) -> Result<()> {
    if let Some(actual) = block_arity(expr) {
        if actual != expected {
            return Err(CodeGenError::BlockArityError {
                selector: selector.to_string(),
                expected: expected.to_string(),
                actual,
                hint: hint.to_string(),
            });
        }
    }
    Ok(())
}

/// BT-493: Validates that a block has arity within a range (inclusive).
/// Non-literal blocks are assumed correct (can't check at compile time).
/// Returns `Ok(())` if valid, `Err(BlockArityError)` if out of range.
#[cfg(test)]
fn validate_block_arity_range(
    expr: &Expression,
    min: usize,
    max: usize,
    selector: &str,
    hint: &str,
) -> Result<()> {
    if let Some(actual) = block_arity(expr) {
        if actual < min || actual > max {
            return Err(CodeGenError::BlockArityError {
                selector: selector.to_string(),
                expected: format!("{min} or {max}"),
                actual,
                hint: hint.to_string(),
            });
        }
    }
    Ok(())
}

/// BT-493: Validates that an `on:do:` handler block has arity 0 or 1.
/// Returns `true` if the handler takes an argument (arity 1 or non-literal), `false` for arity 0.
/// Follows the same pattern as `validate_if_not_nil_block`.
pub(in crate::codegen::core_erlang) fn validate_on_do_handler(
    expr: &Expression,
    selector: &str,
) -> Result<bool> {
    match block_arity(expr) {
        Some(0) => Ok(false),
        Some(n) if n > 1 => Err(CodeGenError::BlockArityError {
            selector: selector.to_string(),
            expected: "0 or 1".to_string(),
            actual: n,
            hint: "Fix: The handler block must take 0 or 1 arguments (the exception):\n\
                   \x20 [...] on: Exception do: [:e | e message]\n\
                   \x20 [...] on: Exception do: ['error occurred']"
                .to_string(),
        }),
        // Arity 1 or non-literal block — pass the exception
        _ => Ok(true),
    }
}

/// Validates that an `ifNotNil:` block has arity 0 or 1.
/// Returns `true` if the block takes an argument (arity 1 or non-literal), `false` for arity 0.
fn validate_if_not_nil_block(expr: &Expression, selector: &str) -> Result<bool> {
    match block_arity(expr) {
        Some(0) => Ok(false),
        Some(n) if n > 1 => Err(CodeGenError::BlockArityMismatch {
            selector: selector.to_string(),
            arity: n,
        }),
        // Arity 1 or non-literal block — pass the receiver (Smalltalk convention)
        _ => Ok(true),
    }
}

/// Generates a Core Erlang `apply` call that adapts to block arity.
/// For 0-arg blocks: `apply BlockVar ()`, for 1-arg blocks: `apply BlockVar (RecvVar)`.
fn not_nil_apply(block_var: &str, recv_var: &str, block_takes_arg: bool) -> Document<'static> {
    if block_takes_arg {
        docvec![
            "apply ",
            leaf::var(block_var.to_string()),
            " (",
            leaf::var(recv_var.to_string()),
            ")"
        ]
    } else {
        docvec!["apply ", leaf::var(block_var.to_string()), " ()"]
    }
}

impl CoreErlangGenerator {
    /// Tries to generate code for Block evaluation and loop intrinsics.
    ///
    /// These are structural intrinsics that the compiler generates inline code for.
    /// This function:
    ///
    /// - Returns `Ok(Some(doc))` if the message was an intrinsic and code was generated
    /// - Returns `Ok(None)` if the message is NOT an intrinsic (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Block Evaluation
    ///
    /// - `value` (0 args) → evaluate block with no arguments
    /// - `value:` (1 arg) → evaluate block with one argument
    /// - `value:value:` (2 args) → evaluate block with two arguments  
    /// - `value:value:value:` (3 args) → evaluate block with three arguments
    ///
    /// # Loop Constructs
    ///
    /// - `repeat` (0 args) → infinite loop
    /// - `whileTrue:` (1 arg) → loop while condition block returns true
    /// - `whileFalse:` (1 arg) → loop while condition block returns false
    /// - `timesRepeat:` (1 arg) → arity validation + mutating-block state threading only;
    ///   non-mutating falls through to pure-BT Integer method (BT-1054)
    /// - `to:do:` (2 args) → arity validation + mutating-block state threading only (BT-1054)
    /// - `to:by:do:` (3 args) → arity validation + mutating-block state threading only (BT-1054)
    pub(in crate::codegen::core_erlang) fn try_generate_block_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // Block unary `value` is a well-known selector — dispatch via the enum.
        if matches!(selector.well_known(), Some(WellKnownSelector::Value)) {
            return self.try_generate_block_value_unary(receiver, arguments);
        }

        // BT-2073: `repeat` is well-known; dispatch via the enum.
        if matches!(selector.well_known(), Some(WellKnownSelector::Repeat)) {
            let doc = self.generate_repeat(receiver)?;
            return Ok(Some(doc));
        }

        match selector {
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                self.try_generate_block_keyword_message(
                    receiver,
                    selector,
                    arguments,
                    &selector_name,
                )
            }

            _ => Ok(None),
        }
    }

    /// Generates code for unary `value` message on a block receiver.
    ///
    /// BT-335: When the receiver is a block literal, use fast inline apply.
    /// For other receivers, generate a runtime type check to handle both
    /// blocks (apply) and non-blocks (runtime dispatch via send).
    fn try_generate_block_value_unary(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // BT-2095: A bare class name as receiver (e.g. `Foo value`) is a
        // class-method send, not block application. Fall through so the
        // class-reference handler routes to `class_send` instead of generating
        // a runtime is_function guard that would call
        // `beamtalk_primitive:send(ClassObject, 'value', [])` and crash on a
        // non-matching `handle_call` clause. (Mirror of the keyword `value:`
        // bypass below.)
        if matches!(receiver, Expression::ClassReference { .. }) {
            return Ok(None);
        }
        // BT-851: Check if receiver is a Tier 2 block parameter (zero-arg value)
        // BT-2797: or a local var this method assigned a Tier 2 block literal to.
        if let Expression::Identifier(id) = receiver {
            if self.tier2_block_params.contains(id.name.as_str())
                || self.tier2_local_vars.contains(id.name.as_str())
            {
                let tuple_doc = self.generate_block_value_call_stateful(receiver, arguments)?;
                return Ok(Some(self.close_tier2_value_subexpr_doc(tuple_doc)));
            }
        }
        // BT-1213: Inline Tier 2 block literal with mutations.
        if let Expression::Block(block) = receiver
            && !Self::captured_mutations_for_block(block).is_empty()
        {
            let doc = self.generate_block_value_inline_with_mutations(block, &[])?;
            return Ok(Some(doc));
        }
        // BT-1481: Block literal with field mutations (actor state threading).
        if let Expression::Block(block) = receiver {
            let analysis = block_analysis::analyze_block(block);
            if self.needs_mutation_threading(&analysis) {
                let doc = self.generate_block_value_inline_with_mutations(block, &[])?;
                return Ok(Some(doc));
            }
        }
        // BT-2814: `self.field value` in sub-expression position (e.g.
        // `self log: (self.field value)`). `generate_block_value_call_runtime_discriminated`
        // always returns a raw `{Result, NewState}` tuple; this function
        // (reached via the generic `expression_doc` dispatch) is called from
        // *any* expression position, where nothing else would unpack it — so
        // unpack it here, discarding NewState. The block's own field/local
        // mutation is NOT threaded forward to later statements in this
        // position (same documented, pre-existing limitation noted in
        // `close_tier2_value_subexpr_doc` — a bare Tier2ValueCall *statement*
        // still gets full state threading via `BodyExprKind::Tier2ValueCall`).
        if self.context == CodeGenContext::Actor && Self::is_self_field_access(receiver) {
            let tuple_doc =
                self.generate_block_value_call_runtime_discriminated(receiver, arguments, "value")?;
            return Ok(Some(self.close_tier2_value_subexpr_doc(tuple_doc)));
        }
        let doc = if matches!(receiver, Expression::Block { .. }) {
            self.generate_block_value_call(receiver, &[])?
        } else {
            // BT-1942: Hoist open-scope receiver (e.g. class method self-send).
            let (preamble, mut docs) = self.capture_subexpr_sequence(&[receiver], "ValRecv")?;
            let recv_doc = docs.remove(0);
            let recv_var = self.fresh_temp_var("ValRecv");
            // BT-2914: arity-discriminate before applying — a Tier 2
            // (stateful) zero-arg block is an arity-1 fun whose `StateAcc`
            // this call site cannot supply; raise the clear
            // `stateful_block_dispatch` error instead of a raw `badarity`
            // (see `generate_value_keyword_guard`'s matching branch).
            let stateful_branch = self.generate_stateful_block_dispatch_error(
                "value",
                "Block",
                "Wrong argument count, or the block captures mutable state and must be invoked directly instead of via perform:/dynamic dispatch",
            );
            let call_doc = docvec![
                "let ",
                leaf::var(recv_var.clone()),
                " = ",
                recv_doc,
                " in case call 'erlang':'is_function'(",
                leaf::var(recv_var.clone()),
                ", 0) of 'true' when 'true' -> apply ",
                leaf::var(recv_var.clone()),
                " () 'false' when 'true' -> case call 'erlang':'is_function'(",
                leaf::var(recv_var.clone()),
                ", 1) of 'true' when 'true' -> ",
                stateful_branch,
                " 'false' when 'true' -> case call 'erlang':'is_function'(",
                leaf::var(recv_var.clone()),
                // Only arity >= 2 funs reach this branch (0 and 1 were ruled
                // out above), so this apply always badarity-crashes — the
                // pre-BT-2914 behaviour for wrong-arity plain functions,
                // preserved deliberately.
                ") of 'true' when 'true' -> apply ",
                leaf::var(recv_var.clone()),
                " () 'false' when 'true' -> call 'beamtalk_primitive':'send'(",
                leaf::var(recv_var),
                ", 'value', []) end end end",
            ];
            self.finalize_dispatch_with_preamble(preamble, call_doc, "ValRes")
        };
        Ok(Some(doc))
    }

    /// Generates code for keyword block messages (`value:`, `whileTrue:`, `timesRepeat:`, etc.).
    ///
    /// Well-known keyword selectors (`value:`/`value:value:`/`value:value:value:`,
    /// `on:do:`, `whileTrue:`/`whileFalse:`, `ensure:`) route through the
    /// `WellKnownSelector` enum so arity is checked structurally by the
    /// classifier (see BT-1260 / BT-2065 epic / BT-2073). The remaining keyword
    /// selectors (`timesRepeat:`, `to:do:`, `to:by:do:`) are class-specific
    /// loop helpers and stay as string matches.
    fn try_generate_block_keyword_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        selector_name: &str,
    ) -> Result<Option<Document<'static>>> {
        // Well-known block-application / loop / exception selectors.
        // The classifier handles arity matching — intercept before the FFI path.
        match selector.well_known() {
            Some(
                WellKnownSelector::ValueColon
                | WellKnownSelector::ValueValue
                | WellKnownSelector::ValueValueValue,
            ) => {
                return self.try_generate_block_value_keyword(receiver, arguments, selector_name);
            }
            Some(WellKnownSelector::OnDo) => {
                debug_assert_eq!(arguments.len(), 2);
                let doc = self.generate_on_do(receiver, &arguments[0], &arguments[1])?;
                return Ok(Some(doc));
            }
            Some(WellKnownSelector::WhileTrue) => {
                debug_assert_eq!(arguments.len(), 1);
                let doc = self.generate_while_true(receiver, &arguments[0])?;
                return Ok(Some(doc));
            }
            Some(WellKnownSelector::WhileFalse) => {
                debug_assert_eq!(arguments.len(), 1);
                let doc = self.generate_while_false(receiver, &arguments[0])?;
                return Ok(Some(doc));
            }
            Some(WellKnownSelector::Ensure) => {
                debug_assert_eq!(arguments.len(), 1);
                let doc = self.generate_ensure(receiver, &arguments[0])?;
                return Ok(Some(doc));
            }
            _ => {}
        }

        match selector_name {
            "timesRepeat:" => self.try_generate_times_repeat(receiver, arguments),

            "to:do:" if arguments.len() == 2 => self.try_generate_to_do(receiver, arguments),

            "to:by:do:" if arguments.len() == 3 => self.try_generate_to_by_do(receiver, arguments),

            // BT-2803: valueWithArguments: (call-site-intercepted @intrinsic,
            // was previously a bare @primitive with no access to the calling
            // method's state — see stdlib/src/Block.bt).
            "valueWithArguments:" if arguments.len() == 1 => {
                self.try_generate_block_value_with_arguments_keyword(receiver, &arguments[0])
            }

            _ => Ok(None),
        }
    }

    /// Generates code for `valueWithArguments:` on a block receiver.
    ///
    /// BT-2803: mirrors `try_generate_block_value_keyword`'s generic
    /// (non-Tier2) fallback path. Deliberately does NOT special-case
    /// `tier2_block_params`/`tier2_local_vars`/`self.field` receivers here —
    /// `generate_block_value_with_arguments_call_runtime_discriminated`
    /// always returns a raw `{Result, NewState}` tuple, and this function is
    /// reached from the generic, any-position `expression_doc` dispatch,
    /// which has nothing to unpack it. Those shapes are intercepted before
    /// reaching here by the top-level `Tier2ValueCall` classification in
    /// `gen_server/methods.rs` (`is_tier2_value_call`/
    /// `generate_tier2_value_call_doc`), the only place that unpacks the
    /// tuple (same scoping as `try_generate_block_value_keyword`'s BT-2797
    /// comment for `self.field value:`).
    fn try_generate_block_value_with_arguments_keyword(
        &mut self,
        receiver: &Expression,
        args_expr: &Expression,
    ) -> Result<Option<Document<'static>>> {
        // BT-2095: A bare class name as receiver is a class-method send, not
        // block application — fall through (mirrors `value:`'s bypass).
        if matches!(receiver, Expression::ClassReference { .. }) {
            return Ok(None);
        }
        // BT-1260: Compile-time Erlang FFI receiver → fall through.
        if Self::is_erlang_ffi_receiver(receiver) {
            return Ok(None);
        }
        // Fast path: block literal receiver never needs the runtime
        // is_function guard. Literal-block-with-mutations is out of scope
        // (BT-2803) — valueWithArguments: on a literal block is an
        // unmotivated shape; use value:/value:value:/... for those instead.
        if matches!(receiver, Expression::Block(_)) {
            let fun_var = self.fresh_temp_var("Fun");
            let recv_code = self.expression_doc(receiver)?;
            let args_code = self.expression_doc(args_expr)?;
            let doc = docvec![
                "let ",
                leaf::var(fun_var.clone()),
                " = ",
                recv_code,
                " in call 'erlang':'apply'(",
                leaf::var(fun_var),
                ", ",
                args_code,
                ")",
            ];
            return Ok(Some(doc));
        }
        let doc = self.generate_block_value_with_arguments_call(receiver, args_expr)?;
        Ok(Some(doc))
    }

    /// Generates code for keyword `value:` variants on a block receiver.
    fn try_generate_block_value_keyword(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
        selector_name: &str,
    ) -> Result<Option<Document<'static>>> {
        // BT-851: Check if receiver is a Tier 2 block parameter
        // BT-2797: or a local var this method assigned a Tier 2 block literal to.
        if let Expression::Identifier(id) = receiver {
            if self.tier2_block_params.contains(id.name.as_str())
                || self.tier2_local_vars.contains(id.name.as_str())
            {
                let tuple_doc = self.generate_block_value_call_stateful(receiver, arguments)?;
                return Ok(Some(self.close_tier2_value_subexpr_doc(tuple_doc)));
            }
        }
        // BT-1213: Inline Tier 2 block literal with mutations (keyword variant)
        if let Expression::Block(block) = receiver
            && !Self::captured_mutations_for_block(block).is_empty()
        {
            let doc = self.generate_block_value_inline_with_mutations(block, arguments)?;
            return Ok(Some(doc));
        }
        // BT-1481: Block literal with field mutations (actor state threading)
        if let Expression::Block(block) = receiver {
            let analysis = block_analysis::analyze_block(block);
            if self.needs_mutation_threading(&analysis) {
                let doc = self.generate_block_value_inline_with_mutations(block, arguments)?;
                return Ok(Some(doc));
            }
        }
        // Fast path: block literal receiver -> fast inline apply
        if matches!(receiver, Expression::Block(_)) {
            let doc = self.generate_block_value_call(receiver, arguments)?;
            return Ok(Some(doc));
        }
        // BT-1260: Compile-time Erlang FFI receiver → fall through
        if Self::is_erlang_ffi_receiver(receiver) {
            return Ok(None);
        }
        // BT-2095: A bare class name as receiver (e.g. `Character value: 65`)
        // is a class-method send, not block application. Fall through so the
        // class-reference handler routes to `class_send` instead of generating
        // a runtime is_function guard that ends up calling
        // `beamtalk_primitive:send(ClassObject, 'value:', [65])` and hitting
        // the actor-style `sync_send` path with no matching handle_call clause.
        if matches!(receiver, Expression::ClassReference { .. }) {
            return Ok(None);
        }
        // BT-2814: `self.field value: ...` in sub-expression position — see
        // the matching comment (and `close_tier2_value_subexpr_doc`) in
        // `try_generate_block_value_unary`.
        if self.context == CodeGenContext::Actor && Self::is_self_field_access(receiver) {
            let tuple_doc = self.generate_block_value_call_runtime_discriminated(
                receiver,
                arguments,
                selector_name,
            )?;
            return Ok(Some(self.close_tier2_value_subexpr_doc(tuple_doc)));
        }
        // BT-1260: Unknown receiver → runtime is_function guard with fallback
        let doc = self.generate_value_keyword_guard(receiver, arguments, selector_name)?;
        Ok(Some(doc))
    }

    /// Generates code for `timesRepeat:` with mutation threading.
    ///
    /// BT-1054: Only intercept for mutating blocks; non-mutating cases
    /// fall through to the pure-BT tail-recursive Integer method.
    fn try_generate_times_repeat(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        use super::block_analysis;
        validate_block_arity_exact(
            &arguments[0],
            0,
            "timesRepeat:",
            "Fix: Use a zero-arg block. If you need the iteration index, use to:do: instead:\n\
             \x20 3 timesRepeat: [self doSomething]\n\
             \x20 1 to: 3 do: [:i | self doSomethingWith: i]",
        )?;
        if let Expression::Block(body_block) = &arguments[0] {
            let analysis = block_analysis::analyze_block(body_block);
            // BT-1329: Also check for nested list ops with cross-scope mutations.
            // BT-2308: Also thread when the body mutates an outer local (including
            // write-only mutations like `[last := i]`) that `needs_mutation_threading`
            // misses in value-type/class-method context. `compute_threaded_locals_for_loop`
            // is the canonical set the loop codegen actually packs into `StateAcc`.
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
                || !self
                    .compute_threaded_locals_for_loop(body_block, None)
                    .is_empty()
            {
                let doc = self.generate_times_repeat_with_mutations(receiver, body_block)?;
                return Ok(Some(doc));
            }
        }
        Ok(None)
    }

    /// Generates code for `to:do:` with mutation threading.
    ///
    /// BT-1054: Only intercept for mutating blocks; non-mutating cases
    /// fall through to the pure-BT tail-recursive Integer method.
    fn try_generate_to_do(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        use super::block_analysis;
        validate_block_arity_exact(
            &arguments[1],
            1,
            "to:do:",
            "Fix: The body block must take one argument (the iteration index):\n\
             \x20 1 to: 10 do: [:i | i printString]",
        )?;
        if let Expression::Block(body_block) = &arguments[1] {
            let analysis = block_analysis::analyze_block(body_block);
            // BT-1329: Also check for nested list ops with cross-scope mutations.
            // BT-2308: Also thread write-only outer-local mutations (see try_generate_times_repeat).
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
                || !self
                    .compute_threaded_locals_for_loop(body_block, None)
                    .is_empty()
            {
                let doc =
                    self.generate_to_do_with_mutations(receiver, &arguments[0], body_block)?;
                return Ok(Some(doc));
            }
        }
        Ok(None)
    }

    /// Generates code for `to:by:do:` with mutation threading.
    ///
    /// BT-1054: Only intercept for mutating blocks; non-mutating cases
    /// fall through to the pure-BT tail-recursive Integer method.
    fn try_generate_to_by_do(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        use super::block_analysis;
        validate_block_arity_exact(
            &arguments[2],
            1,
            "to:by:do:",
            "Fix: The body block must take one argument (the iteration index):\n\
             \x20 1 to: 10 by: 2 do: [:i | i printString]",
        )?;
        if let Expression::Block(body_block) = &arguments[2] {
            let analysis = block_analysis::analyze_block(body_block);
            // BT-1329: Also check for nested list ops with cross-scope mutations.
            // BT-2308: Also thread write-only outer-local mutations (see try_generate_times_repeat).
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
                || !self
                    .compute_threaded_locals_for_loop(body_block, None)
                    .is_empty()
            {
                let doc = self.generate_to_by_do_with_mutations(
                    receiver,
                    &arguments[0],
                    &arguments[1],
                    body_block,
                )?;
                return Ok(Some(doc));
            }
        }
        Ok(None)
    }

    /// Tries to generate code for List/Array methods.
    ///
    /// List methods are structural intrinsics that require inline code generation
    /// for proper state threading when used inside actor methods with field mutations.
    ///
    /// **BT-416**: This intrinsic now checks the receiver type to avoid intercepting
    /// String primitive methods. String literals use `@primitive` codegen that delegates
    /// to `beamtalk_string`, not `lists:map/filter`.
    ///
    /// - Returns `Ok(Some(doc))` if the message was a List method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a List method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # List Methods
    ///
    /// - `do:` (1 arg block) → iterate over elements with side effects
    /// - `collect:` (1 arg block) → map to new list
    /// - `select:` (1 arg block) → filter elements
    /// - `reject:` (1 arg block) → filter out elements
    /// - `anySatisfy:` (1 arg block) → boolean predicate (any match)
    /// - `allSatisfy:` (1 arg block) → boolean predicate (all match)
    /// - `detect:` (1 arg block) → find first matching element
    /// - `detect:ifNone:` (1 arg block + 0 arg block) → find first or default
    /// - `count:` (1 arg block) → count matching elements
    /// - `flatMap:` (1 arg block) → map and flatten
    /// - `inject:into:` (2 args) → fold with accumulator
    pub(in crate::codegen::core_erlang) fn try_generate_list_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // BT-1489: String receivers are no longer skipped here. The
        // non-mutating simple-list-op path already falls back to
        // `beamtalk_primitive:send(recv, selector, [Body])` for non-list
        // receivers (which dispatches to beamtalk_string helpers). The
        // mutating foldl paths now add an `is_binary` result wrapper so
        // collect:/select:/reject: return a binary string when the
        // receiver was a string.

        match selector {
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "do:" if arguments.len() == 1 => {
                        let doc = self.generate_list_do(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "collect:" if arguments.len() == 1 => {
                        let doc = self.generate_list_collect(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "select:" if arguments.len() == 1 => {
                        let doc = self.generate_list_select(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "reject:" if arguments.len() == 1 => {
                        let doc = self.generate_list_reject(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "anySatisfy:" if arguments.len() == 1 => {
                        let doc = self.generate_list_any_satisfy(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "allSatisfy:" if arguments.len() == 1 => {
                        let doc = self.generate_list_all_satisfy(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "inject:into:" if arguments.len() == 2 => {
                        let doc =
                            self.generate_list_inject(receiver, &arguments[0], &arguments[1])?;
                        Ok(Some(doc))
                    }
                    "detect:" if arguments.len() == 1 => {
                        let doc = self.generate_list_detect(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "detect:ifNone:" if arguments.len() == 2 => {
                        let doc = self.generate_list_detect_if_none(
                            receiver,
                            &arguments[0],
                            &arguments[1],
                        )?;
                        Ok(Some(doc))
                    }
                    "count:" if arguments.len() == 1 => {
                        let doc = self.generate_list_count(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "flatMap:" if arguments.len() == 1 => {
                        let doc = self.generate_list_flat_map(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    // BT-1487: Medium-risk list selectors
                    "takeWhile:" if arguments.len() == 1 => {
                        let doc = self.generate_list_take_while(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "dropWhile:" if arguments.len() == 1 => {
                        let doc = self.generate_list_drop_while(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "groupBy:" if arguments.len() == 1 => {
                        let doc = self.generate_list_group_by(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "partition:" if arguments.len() == 1 => {
                        let doc = self.generate_list_partition(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "sort:" if arguments.len() == 1 => {
                        let doc = self.generate_list_sort(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    // BT-2703: `eachWithIndex:`/`do:separatedBy:` are self-hosted in
                    // Collection.bt. In an actor method that mutates state, desugar to a
                    // stateful `inject:into:` fold so the mutation threads; otherwise
                    // return `None` and let the ordinary dispatch reach the Collection.bt
                    // method.
                    "eachWithIndex:" if arguments.len() == 1 => {
                        self.try_generate_each_with_index(receiver, &arguments[0])
                    }
                    "do:separatedBy:" if arguments.len() == 2 => {
                        self.try_generate_do_separated_by(receiver, &arguments[0], &arguments[1])
                    }
                    _ => Ok(None),
                }
            }

            _ => Ok(None),
        }
    }

    /// BT-1488: Dictionary iteration intrinsics — `do:`, `doWithKey:`, `keysAndValuesDo:`.
    ///
    /// These are structural intrinsics for dictionary iteration that require inline
    /// code generation for proper state threading when used inside actor methods with
    /// field mutations.
    ///
    /// For `do:`, this handler only fires when the receiver is a `MapLiteral` expression
    /// (dictionary literal syntax `#{...}`). Non-literal receivers fall through to the
    /// list `do:` handler, which handles non-list collections via `beamtalk_collection:to_list`.
    ///
    /// For `doWithKey:` and `keysAndValuesDo:`, these are dictionary-specific selectors
    /// that always require dictionary-specific codegen.
    ///
    /// - Returns `Ok(Some(doc))` if dictionary-specific code was generated
    /// - Returns `Ok(None)` if the message should be handled by another handler
    pub(in crate::codegen::core_erlang) fn try_generate_dict_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector {
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    // For `do:`, only intercept when receiver is a dictionary literal.
                    // Non-literal receivers are handled by the list `do:` handler.
                    "do:"
                        if arguments.len() == 1
                            && matches!(receiver, Expression::MapLiteral { .. }) =>
                    {
                        let doc = self.generate_dict_do(receiver, &arguments[0])?;
                        // If generate_dict_do returned Nil, no mutations were found;
                        // fall through to runtime dispatch.
                        if matches!(doc, Document::Nil) {
                            Ok(None)
                        } else {
                            Ok(Some(doc))
                        }
                    }
                    "doWithKey:" | "keysAndValuesDo:" if arguments.len() == 1 => {
                        let doc = self.generate_dict_do_with_key(receiver, &arguments[0])?;
                        if matches!(doc, Document::Nil) {
                            Ok(None)
                        } else {
                            Ok(Some(doc))
                        }
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    /// Generates a block value call: `let _Fun = <receiver> in apply _Fun (Args...)`.
    ///
    /// In Core Erlang, we bind the receiver to a variable first to ensure proper
    /// evaluation order and handle complex receiver expressions correctly.
    ///
    /// ```erlang
    /// let _Fun1 = <receiver-expr> in apply _Fun1 (Arg1, Arg2, ...)
    /// ```
    pub(in crate::codegen::core_erlang) fn generate_block_value_call(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let fun_var = self.fresh_temp_var("Fun");

        // BT-1270: Evaluate receiver first, then hoist field-assignment arguments.
        // This preserves evaluation order: `let _Fun = recv in [hoisted args] apply _Fun (args)`.
        let recv_code = self.expression_doc(receiver)?;
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len() + 2);
        parts.push(docvec![
            "let ",
            leaf::var(fun_var.clone()),
            " = ",
            recv_code,
            " in ",
        ]);
        let arg_docs = self.generate_cascade_args(arguments, &mut parts)?;

        let args_doc = join(arg_docs, &Document::Str(", "));

        parts.push(docvec!["apply ", leaf::var(fun_var), " (", args_doc, ")",]);
        Ok(Document::Vec(parts))
    }

    /// BT-1260: Returns true if `expr` is a compile-time Erlang FFI proxy expression.
    ///
    /// Matches `(Erlang module_name)` — i.e., a `MessageSend` whose inner receiver is
    /// `ClassReference("Erlang")` and whose selector is a unary module name.  These
    /// expressions should **not** be treated as blocks by the `value:` handler; instead
    /// they fall through to `try_handle_erlang_interop` (step 7 of dispatch).
    fn is_erlang_ffi_receiver(expr: &Expression) -> bool {
        /// Class-protocol selectors excluded from Erlang FFI proxy construction.
        /// Mirrors `CLASS_PROTOCOL_SELECTORS` in `dispatch_codegen.rs` so that
        /// e.g. `(Erlang class) value: x` falls through to the `is_function` guard
        /// instead of being treated as a module proxy (consistent with step 7).
        const CLASS_PROTOCOL_SELECTORS: &[&str] = &[
            "new",
            "spawn",
            "class",
            "methods",
            "superclass",
            "subclasses",
            "allSubclasses",
            "class_name",
            "module_name",
            "printString",
        ];
        // BT-2685: peel `(Erlang mod)` parentheses so a parenthesized FFI receiver
        // is recognised (the canonical `(Erlang beamtalk_console) error: x` form).
        if let Expression::MessageSend {
            receiver: inner_receiver,
            selector: MessageSelector::Unary(module_name),
            ..
        } = Self::peel_parens(expr)
        {
            if let Expression::ClassReference { name, .. } = Self::peel_parens(inner_receiver) {
                return name.name == "Erlang"
                    && !CLASS_PROTOCOL_SELECTORS.contains(&module_name.as_str());
            }
        }
        false
    }

    /// BT-1260: Generates a runtime `erlang:is_function/1` guard for keyword `value:` sends.
    ///
    /// When the receiver is not a compile-time-known block or Erlang FFI expression,
    /// emits a runtime check:
    ///
    /// ```erlang
    /// let _ValRecv = <recv> in
    /// let _ValArg0 = <arg0> in
    /// case call 'erlang':'is_function'(_ValRecv) of
    ///   'true' when 'true' -> apply _ValRecv (_ValArg0)
    ///   'false' when 'true' -> call 'beamtalk_primitive':'send'(_ValRecv, 'value:', [_ValArg0])
    /// end
    /// ```
    ///
    /// BT-1942: Hoists a receiver operand that may open a class-method
    /// self-send scope, binding it to a fresh `prefix`-named temp var.
    /// Appends the necessary `let`-binding(s) to `parts` and sets
    /// `any_open_scope` if this operand's evaluation opened a class-method
    /// scope. Shared by `generate_value_keyword_guard` and
    /// `generate_block_value_with_arguments_call` (BT-2803) — both hoist
    /// their receiver the same way, before any argument hoisting.
    fn hoist_open_scope_receiver(
        &mut self,
        receiver: &Expression,
        prefix: &str,
        parts: &mut Vec<Document<'static>>,
        any_open_scope: &mut bool,
    ) -> Result<String> {
        let (preamble, mut docs) = self.capture_subexpr_sequence(&[receiver], prefix)?;
        let code = docs.remove(0);
        if !matches!(preamble, Document::Nil) {
            *any_open_scope = true;
            parts.push(preamble);
        }
        let var = self.fresh_temp_var(prefix);
        parts.push(docvec!["let ", leaf::var(var.clone()), " = ", code, " in ",]);
        Ok(var)
    }

    /// BT-1270/BT-1942: Hoists an argument-position operand, special-casing a
    /// field-assignment argument (`self.field := x`) so its `StateN` binding
    /// lands outside the let-chain rather than nested inside it. Appends the
    /// necessary `let`-binding(s) to `parts` and sets `any_open_scope` if
    /// this operand's evaluation opened a class-method scope. Shared by
    /// `generate_value_keyword_guard` and
    /// `generate_block_value_with_arguments_call` (BT-2803).
    fn hoist_open_scope_argument(
        &mut self,
        arg: &Expression,
        prefix: &str,
        parts: &mut Vec<Document<'static>>,
        any_open_scope: &mut bool,
    ) -> Result<String> {
        let var = self.fresh_temp_var(prefix);
        if Self::is_field_assignment(arg) {
            let (doc, val_var) = self.generate_field_assignment_open(arg)?;
            parts.push(doc);
            parts.push(docvec![
                "let ",
                leaf::var(var.clone()),
                " = ",
                leaf::var(val_var),
                " in ",
            ]);
        } else {
            let (preamble, mut docs) = self.capture_subexpr_sequence(&[arg], prefix)?;
            let code = docs.remove(0);
            if !matches!(preamble, Document::Nil) {
                *any_open_scope = true;
                parts.push(preamble);
            }
            parts.push(docvec!["let ", leaf::var(var.clone()), " = ", code, " in ",]);
        }
        Ok(var)
    }

    /// This mirrors the runtime guard emitted for the unary `value` case (BT-335).
    ///
    /// BT-2914: the function branch discriminates arity before applying. A
    /// Tier 2 (stateful, ADR-0041) block compiles to an (N+1)-arg fun expecting
    /// a live `StateAcc` this statically-unknown call site cannot supply — it
    /// reaches here when a stateful block flows through generic dispatch into a
    /// self-hosted method's block parameter (e.g. `perform: #eachWithIndex:`,
    /// whose Collection body invokes `block value:value:`). Applying it N-ary
    /// used to crash with a raw `badarity`; now it raises the same clear
    /// `stateful_block_dispatch` error BT-2812/BT-2888 established. Any other
    /// arity mismatch keeps the pre-existing `badarity` behaviour (the plain
    /// apply in the fallthrough branch), and non-function receivers still fall
    /// back to `beamtalk_primitive:send/3`.
    fn generate_value_keyword_guard(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
        selector_name: &str,
    ) -> Result<Document<'static>> {
        let mut arg_vars: Vec<String> = Vec::with_capacity(arguments.len());
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len() * 2 + 3);

        // BT-1942: Hoist open-scope receiver (e.g. class method self-send) inline so
        // its ClassVarsN binding remains visible to subsequent arg bindings.
        // Each sub-expression is bound sequentially, so per-sub-expression inline
        // hoisting preserves left-to-right evaluation order.
        let mut any_open_scope = false;
        let recv_var =
            self.hoist_open_scope_receiver(receiver, "ValRecv", &mut parts, &mut any_open_scope)?;

        // BT-1270: Hoist field-assignment arguments before their _ValArgN bindings so
        // the StateN binding is in scope after the let-chain, not nested inside it.
        for arg in arguments {
            let arg_var =
                self.hoist_open_scope_argument(arg, "ValArg", &mut parts, &mut any_open_scope)?;
            arg_vars.push(arg_var);
        }

        let arg_var_docs: Vec<Document<'static>> =
            arg_vars.iter().map(|v| leaf::var(v.clone())).collect();
        let apply_args = join(arg_var_docs.clone(), &Document::Str(", "));

        let send_list = docvec!["[", join(arg_var_docs, &Document::Str(", ")), "]"];

        // BT-2914: same hedged hint as `generate_stateful_block_guard` —
        // `is_function/2` can't distinguish a genuine Tier 2 block from a pure
        // fun called with one argument too few, and the block can arrive here
        // through any dynamic dispatch, not just `perform:`.
        let stateful_branch = self.generate_stateful_block_dispatch_error(
            selector_name,
            "Block",
            "Wrong argument count, or the block captures mutable state and must be invoked directly instead of via perform:/dynamic dispatch",
        );
        let arity = arguments.len();

        let case_doc = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(recv_var.clone()),
            ", ",
            leaf::int_lit(i64::try_from(arity).unwrap_or(i64::MAX)),
            ") of 'true' when 'true' -> apply ",
            leaf::var(recv_var.clone()),
            " (",
            apply_args.clone(),
            ") 'false' when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(recv_var.clone()),
            ", ",
            leaf::int_lit(i64::try_from(arity.saturating_add(1)).unwrap_or(i64::MAX)),
            ") of 'true' when 'true' -> ",
            stateful_branch,
            " 'false' when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(recv_var.clone()),
            ") of 'true' when 'true' -> apply ",
            leaf::var(recv_var.clone()),
            " (",
            apply_args,
            ") 'false' when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(recv_var),
            ", ",
            leaf::atom(selector_name.to_string()),
            ", ",
            send_list,
            ") end end end",
        ];

        // BT-1942: If any sub-expression produced an open scope from a class
        // method self-send, wrap the case result and propagate the open scope
        // upward so the enclosing context can see the advanced ClassVarsN.
        if any_open_scope {
            let result_var = self.fresh_temp_var("ValRes");
            parts.push(docvec![
                "let ",
                leaf::var(result_var.clone()),
                " = ",
                case_doc,
                " in ",
            ]);
            self.last_open_scope_result = Some(result_var);
        } else {
            parts.push(case_doc);
        }

        Ok(Document::Vec(parts))
    }

    /// BT-2814: Closes a Tier 2 value call's raw `{Result, NewState}` tuple
    /// doc for use in *sub-expression* position (e.g. `10 + (blk value: x)`,
    /// `self log: (self.field value)`), where the caller has no place to
    /// thread `NewState` forward — it can only use a single plain value.
    /// Extracts and returns just `Result`, in a syntactically self-contained
    /// (closed) expression: `let T2SubTupleN = <tuple_doc> in
    /// call 'erlang':'element'(1, T2SubTupleN)`.
    ///
    /// This intentionally does NOT thread the block's field/captured-local
    /// mutation forward to later statements — the same documented,
    /// pre-existing limitation BT-2797 first called out for
    /// `self.field value(:...)` in argument position, now also correctly
    /// computing the right *value* instead of leaking the raw tuple into the
    /// caller (which crashed with badarith/badarity — BT-2814). A Tier 2
    /// value call used as a bare STATEMENT (not nested in another
    /// expression) is unaffected: it's classified as
    /// `BodyExprKind::Tier2ValueCall` and still gets full state threading via
    /// `generate_tier2_value_call_doc` at the top-level method-body/
    /// conditional-branch/loop-body sequencers — this fallback only applies
    /// where a plain value, not a state-threading statement, is expected.
    fn close_tier2_value_subexpr_doc(&mut self, tuple_doc: Document<'static>) -> Document<'static> {
        let tuple_var = self.fresh_temp_var("T2SubTuple");
        docvec![
            "let ",
            leaf::var(tuple_var.clone()),
            " = ",
            tuple_doc,
            " in call 'erlang':'element'(1, ",
            leaf::var(tuple_var),
            ")",
        ]
    }

    /// BT-851: Generates a Tier 2 stateful block value call (ADR 0041 Phase 0).
    ///
    /// Calls a Tier 2 block using the stateful protocol:
    /// `apply _Fun(Args..., State) → {Result, NewState}`
    ///
    /// Returns a `{Result, NewState}` tuple (like control flow with mutations).
    /// The method body generator must unpack this tuple to extract the result
    /// and thread the new state through to the reply tuple.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _Fun = <receiver> in apply _Fun (Arg1, ..., ArgN, State)
    /// ```
    ///
    /// Returns `{Result, NewState}` tuple from the block.
    pub(in crate::codegen::core_erlang) fn generate_block_value_call_stateful(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let fun_var = self.fresh_temp_var("Fun");
        let recv_code = self.expression_doc(receiver)?;
        let current_state = self.current_state_var();

        let mut arg_docs: Vec<Document<'static>> = Vec::with_capacity(arguments.len() + 1);
        for arg in arguments {
            arg_docs.push(self.expression_doc(arg)?);
        }
        // Append State as last argument
        arg_docs.push(leaf::var(current_state));
        let args_doc = join(arg_docs, &Document::Str(", "));

        let doc = docvec![
            "let ",
            leaf::var(fun_var.clone()),
            " = ",
            recv_code,
            " in apply ",
            leaf::var(fun_var),
            " (",
            args_doc,
            ")",
        ];
        Ok(doc)
    }

    /// BT-2797: Generates a runtime Tier 1/Tier 2 discriminated block value call.
    ///
    /// Used when the receiver's Tier-ness can't be determined statically — the
    /// motivating case is a block stored in an instance field and invoked from
    /// a *different* method than the one that assigned it, so no static
    /// local/param tracking (`tier2_block_params` / `tier2_local_vars`, both
    /// scoped to a single method) can see it. Generalizes the BT-909
    /// `erlang:is_function/2` arity-discrimination pattern (used there for
    /// Erlang FFI interop) to Beamtalk-level block value calls.
    ///
    /// Deliberately scoped to `self.field value(:...)` receivers only (see the
    /// `is_tier2_value_call`/call-site callers) — not every opaque receiver —
    /// to keep the blast radius contained to the one shape that genuinely
    /// needs it, leaving the far more common "block passed as a Tier 1 method
    /// parameter" call sites untouched.
    ///
    /// Always returns a raw `{Result, NewState}` tuple, same contract as
    /// `generate_block_value_call_stateful`:
    /// - Tier 1 (`is_function(Fun, N)`, N = arity of `arguments`): synthesizes
    ///   `{ApplyResult, State}` — state is unchanged.
    /// - Tier 2 (`is_function(Fun, N + 1)`): returns the block's own
    ///   `{Result, NewState}` tuple directly (Tier 2 funs already return this
    ///   shape — see `generate_block_stateful`).
    /// - Non-function receiver: falls back to `beamtalk_primitive:send/3`
    ///   (DNU-style dispatch, mirroring `generate_value_keyword_guard`'s
    ///   fallback), wrapped as `{SendResult, State}`.
    ///
    /// Callers must unpack this tuple. `is_tier2_value_call` (extended for
    /// BT-2797 to recognize `self.field` receivers) is what makes
    /// `classify_body_expr` route the statement calling this function to
    /// `BodyExprKind::Tier2ValueCall`/`LocalAssignTier2`.
    ///
    /// BT-2797 (PR #2899 review): called ONLY from
    /// `gen_server/methods.rs`'s `generate_tier2_value_call_doc` — the single
    /// place that actually unpacks this tuple. Deliberately not wired into
    /// the generic `try_generate_block_value_unary`/`try_generate_block_value_keyword`
    /// dispatch (reached via `expression_doc` from *any* expression
    /// position): a `self.field value(:...)` in sub-expression position
    /// (e.g. an argument to another call) has no tuple-unpacking caller, so
    /// intercepting it there would silently hand the raw tuple to code
    /// expecting a plain value — a regression for a Tier 1 (pure) block that
    /// worked correctly before this function existed.
    pub(in crate::codegen::core_erlang) fn generate_block_value_call_runtime_discriminated(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
        selector_name: &str,
    ) -> Result<Document<'static>> {
        let fun_var = self.fresh_temp_var("Fun");
        let recv_code = self.expression_doc(receiver)?;
        let current_state = self.current_state_var();
        let arity = arguments.len();

        let mut parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len() + 2);
        parts.push(docvec![
            "let ",
            leaf::var(fun_var.clone()),
            " = ",
            recv_code,
            " in ",
        ]);
        let mut arg_vars: Vec<String> = Vec::with_capacity(arguments.len());
        for arg in arguments {
            let arg_var = self.fresh_temp_var("Arg");
            let arg_code = self.expression_doc(arg)?;
            parts.push(docvec![
                "let ",
                leaf::var(arg_var.clone()),
                " = ",
                arg_code,
                " in ",
            ]);
            arg_vars.push(arg_var);
        }

        let arg_var_docs: Vec<Document<'static>> =
            arg_vars.iter().map(|v| leaf::var(v.clone())).collect();
        let tier1_apply_args = join(arg_var_docs.clone(), &Document::Str(", "));
        let mut tier2_arg_docs = arg_var_docs.clone();
        tier2_arg_docs.push(leaf::var(current_state.clone()));
        let tier2_apply_args = join(tier2_arg_docs, &Document::Str(", "));
        let send_list = docvec!["[", join(arg_var_docs, &Document::Str(", ")), "]"];

        let case_doc = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(fun_var.clone()),
            ", ",
            leaf::int_lit(i64::try_from(arity).unwrap_or(i64::MAX)),
            ") of 'true' when 'true' -> {apply ",
            leaf::var(fun_var.clone()),
            " (",
            tier1_apply_args,
            "), ",
            leaf::var(current_state.clone()),
            "} 'false' when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(fun_var.clone()),
            ", ",
            leaf::int_lit(i64::try_from(arity + 1).unwrap_or(i64::MAX)),
            ") of 'true' when 'true' -> apply ",
            leaf::var(fun_var.clone()),
            " (",
            tier2_apply_args,
            ") 'false' when 'true' -> {call 'beamtalk_primitive':'send'(",
            leaf::var(fun_var),
            ", ",
            leaf::atom(selector_name.to_string()),
            ", ",
            send_list,
            "), ",
            leaf::var(current_state),
            "} end end",
        ];
        parts.push(case_doc);

        Ok(Document::Vec(parts))
    }

    /// BT-2812: Generates the fallback method body for `blockValue`/`blockValue1`/
    /// `blockValue2`/`blockValue3` — Block's `value`/`value:`/`value:value:`/
    /// `value:value:value:`. This body is reached only when something bypasses the
    /// call-site interception these selectors normally get (e.g. `perform:`/
    /// `perform:withArguments:`), the same gap `blockValueWithArguments`'s fallback
    /// (`generate_primitive`, `mod.rs`) closed for real under BT-2803 — but unlike
    /// that selector, these can't unconditionally `erlang:apply`: under ADR-0041's
    /// universal state-threading protocol, a Tier 2 (stateful) block compiles to
    /// `fun(Args..., StateAcc) -> {Result, NewStateAcc}`, and invoking it correctly
    /// requires a live `StateAcc` that only a call-site-aware caller has (see this
    /// file's `generate_block_value_call_runtime_discriminated`, which this mirrors
    /// via the same `erlang:is_function/2` arity-discrimination idiom). Generic
    /// dispatch has no such state, so:
    /// - Tier 1 (`Self` is a plain fun of the expected arity): real `erlang:apply`
    ///   fix — this is the common, pure-block case.
    /// - Tier 2 (`Self` is a fun of arity+1): raises a clear `#beamtalk_error{}`
    ///   (`stateful_block_dispatch`) instead of silently self-dispatching to the
    ///   wrong intrinsic name via the placeholder path. Note: `erlang:is_function/2`
    ///   can only compare arity — it can't distinguish a genuine Tier 2 block from
    ///   a Tier 1 block simply called with the wrong argument count (both present
    ///   as arity+1 relative to the selector's expected arity), so the error
    ///   message/hint deliberately don't assert a specific cause.
    /// - Neither (defensive; Block is `sealed` so `Self` is always a fun in
    ///   practice): falls through to the original runtime-dispatch placeholder,
    ///   unchanged from before this fix.
    ///
    /// Adversarial review (BT-2812): the arity ambiguity above cuts both ways.
    /// A genuinely Tier 2 block whose *declared* arity is one less than the
    /// selector's (e.g. a 0-arg stateful block `[count := count + 1]`, raw
    /// arity 1, sent `#value:`) satisfies the Tier 1 `is_function(Self, N)`
    /// check and gets `erlang:apply`'d with the caller's argument bound into
    /// its `StateAcc` parameter instead of a map. This does *not* crash raw —
    /// the block's internal `maps:get`/`maps:put` calls fail with `badarg`,
    /// which an outer safety net already converts to a clean, catchable
    /// `#beamtalk_error{kind = type_error}` — but the resulting message
    /// ("expected a Dictionary...") doesn't explain the real cause the way
    /// `stateful_block_dispatch` does. Precisely disambiguating would require
    /// a try/catch around the Tier 1 `apply` that intercepts only internal
    /// map-shape crashes and reraises everything else untouched (real
    /// `#beamtalk_error{}`s and non-local-return throws must not be masked);
    /// deferred as a separately-scoped follow-up rather than risking a rushed
    /// version of that here. See BT-2892.
    pub(in crate::codegen::core_erlang) fn generate_block_value_structural_fallback(
        &mut self,
        intrinsic_name: &str,
        arity: usize,
        real_selector: &str,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let params_doc = join(
            self.current_method_params
                .iter()
                .map(|p| leaf::var(p.clone())),
            &Document::Str(", "),
        );

        let stateful_branch = self.generate_stateful_block_dispatch_error(
            real_selector,
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );

        let runtime_module =
            super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(class_name);
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'(",
            leaf::atom(intrinsic_name),
            ", [",
            params_doc.clone(),
            "], ",
            Document::Str(self_var),
            ")",
        ];

        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", ",
            leaf::int_lit(i64::try_from(arity).unwrap_or(i64::MAX)),
            ") of <'true'> when 'true' -> call 'erlang':'apply'(",
            Document::Str(self_var),
            ", [",
            params_doc,
            "]) <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", ",
            leaf::int_lit(i64::try_from(arity + 1).unwrap_or(i64::MAX)),
            ") of <'true'> when 'true' -> ",
            stateful_branch,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
    }

    /// Shared `#beamtalk_error{kind = stateful_block_dispatch}` construction used
    /// by every structural-intrinsic fallback that can't tell (from an
    /// `erlang:is_function/2` arity check alone) whether it's looking at a
    /// genuine Tier 2 (stateful, ADR-0041) block or a Tier 1 block called with
    /// the wrong argument count — see `generate_block_value_structural_fallback`'s
    /// doc comment above for why the hint is deliberately hedged rather than
    /// asserting a specific cause.
    pub(in crate::codegen::core_erlang) fn generate_stateful_block_dispatch_error(
        &mut self,
        real_selector: &str,
        class_name: &str,
        hint_text: &str,
    ) -> Document<'static> {
        let error_base = self.fresh_temp_var("Err");
        let error_sel = self.fresh_temp_var("Err");
        let error_hint = self.fresh_temp_var("Err");
        let hint = leaf::binary_lit(hint_text);
        docvec![
            "let ",
            leaf::var(error_base.clone()),
            " = call 'beamtalk_error':'new'('stateful_block_dispatch', ",
            leaf::atom(class_name),
            ") in let ",
            leaf::var(error_sel.clone()),
            " = call 'beamtalk_error':'with_selector'(",
            leaf::var(error_base),
            ", ",
            leaf::atom(real_selector),
            ") in let ",
            leaf::var(error_hint.clone()),
            " = call 'beamtalk_error':'with_hint'(",
            leaf::var(error_sel),
            ", ",
            hint,
            ") in call 'beamtalk_error':'raise'(",
            leaf::var(error_hint),
            ")",
        ]
    }

    /// BT-2888: Guards a List/Collection iteration primitive's block argument
    /// (`do:`/`collect:`/`select:`/`reject:`/`inject:into:`) against being a
    /// Tier 2 (stateful) block.
    ///
    /// Unlike Block's `value*` structural intrinsics (BT-2812), these
    /// selectors' compiled method bodies are real, already-correct BIF
    /// lowerings (`tier1_doc`, e.g. `lists:map/2`, `beamtalk_list:do/2`) — a
    /// Tier 1 (pure) block reached via generic dispatch (`perform:`) already
    /// works. But that BIF body applies the block with a plain N-arg
    /// `erlang:apply`; a Tier 2 block compiles to an (N+1)-arg fun expecting a
    /// live `StateAcc` (ADR-0041) that generic dispatch has no way to supply,
    /// which today hits a raw Erlang arity crash (confirmed empirically)
    /// instead of a clear diagnostic. This wraps the existing correct Tier 1
    /// body with the same `stateful_block_dispatch` error BT-2812
    /// established, rather than replacing a placeholder.
    ///
    /// Selector-keyed, not class-keyed: applies wherever a class declares one
    /// of these exact selectors as a bare-inferred `@primitive` with a real
    /// BIF lowering (confirmed to include List, Array, Binary, Dictionary,
    /// Set, Tuple, Collection, and String's `collect:`/`select:`/`reject:`).
    ///
    /// Same arity-only ambiguity BT-2812 documented for `blockValue*`
    /// (`generate_block_value_structural_fallback`): `erlang:is_function/2`
    /// can't distinguish a genuinely stateful block from a *pure* block
    /// simply called with one fewer argument than the selector expects (both
    /// present as arity `pure_arity + 1`) — hence the hedged hint text rather
    /// than asserting the block is stateful.
    pub(in crate::codegen::core_erlang) fn generate_stateful_block_guard(
        &mut self,
        block_param: &str,
        pure_arity: usize,
        real_selector: &str,
        class_name: &str,
        tier1_doc: Document<'static>,
    ) -> Document<'static> {
        // Hint text deliberately differs from `STATEFUL_BLOCK_DISPATCH_HINT`
        // ("...via perform:") — List/Collection selectors are ordinary
        // methods, reachable through *any* dynamic dispatch (not just
        // `perform:`/`perform:withArguments:` the way a structural intrinsic
        // like `value*`/`whileTrue:`/`on:do:` is), so the hint says
        // "perform:/dynamic dispatch" to cover both.
        let stateful_branch = self.generate_stateful_block_dispatch_error(
            real_selector,
            class_name,
            "Wrong argument count, or the block captures mutable state and must be invoked directly instead of via perform:/dynamic dispatch",
        );

        docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(block_param.to_string()),
            ", ",
            leaf::int_lit(i64::try_from(pure_arity + 1).unwrap_or(i64::MAX)),
            ") of <'true'> when 'true' -> ",
            stateful_branch,
            " <'false'> when 'true' -> ",
            tier1_doc,
            " end",
        ]
    }

    /// BT-2803: Generates a runtime `erlang:is_function/1` guard for
    /// `valueWithArguments:` sends.
    ///
    /// Mirrors `generate_value_keyword_guard`, but the argument is a single
    /// runtime list rather than N individually-hoisted arguments, so the
    /// true branch uses `erlang:apply/2` (fun + arg-list) instead of a
    /// positional `apply`.
    ///
    /// ```erlang
    /// let _ValRecv = <recv> in
    /// let _ValArgs = <args> in
    /// case call 'erlang':'is_function'(_ValRecv) of
    ///   'true' when 'true' -> call 'erlang':'apply'(_ValRecv, _ValArgs)
    ///   'false' when 'true' -> call 'beamtalk_primitive':'send'(_ValRecv, 'valueWithArguments:', [_ValArgs])
    /// end
    /// ```
    fn generate_block_value_with_arguments_call(
        &mut self,
        receiver: &Expression,
        args_expr: &Expression,
    ) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(4);
        let mut any_open_scope = false;

        let recv_var =
            self.hoist_open_scope_receiver(receiver, "ValRecv", &mut parts, &mut any_open_scope)?;
        let args_var =
            self.hoist_open_scope_argument(args_expr, "ValArgs", &mut parts, &mut any_open_scope)?;

        let case_doc = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(recv_var.clone()),
            ") of 'true' when 'true' -> call 'erlang':'apply'(",
            leaf::var(recv_var.clone()),
            ", ",
            leaf::var(args_var.clone()),
            ") 'false' when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(recv_var),
            ", ",
            leaf::atom("valueWithArguments:"),
            ", [",
            leaf::var(args_var),
            "]) end",
        ];

        if any_open_scope {
            let result_var = self.fresh_temp_var("ValRes");
            parts.push(docvec![
                "let ",
                leaf::var(result_var.clone()),
                " = ",
                case_doc,
                " in ",
            ]);
            self.last_open_scope_result = Some(result_var);
        } else {
            parts.push(case_doc);
        }

        Ok(Document::Vec(parts))
    }

    /// BT-2803: Generalizes `generate_block_value_call_runtime_discriminated`
    /// to `valueWithArguments:`, whose argument count is a runtime list
    /// length rather than a compile-time-known static arity.
    ///
    /// Same contract and scoping as
    /// `generate_block_value_call_runtime_discriminated` — reached only from
    /// `gen_server/methods.rs`'s `generate_tier2_value_call_doc` (the tuple
    /// unpacker), never from generic `expression_doc` dispatch. Always
    /// returns a raw `{Result, NewState}` tuple:
    /// - Tier 1 (`is_function(Fun, length(Args))`): synthesizes
    ///   `{erlang:apply(Fun, Args), State}` — state unchanged.
    /// - Tier 2 (`is_function(Fun, length(Args) + 1)`): appends the calling
    ///   method's live state as the trailing element of `Args` and returns
    ///   the block's own `{Result, NewState}` tuple directly (Tier 2 funs
    ///   already return this shape).
    /// - Non-function receiver: falls back to `beamtalk_primitive:send/3`,
    ///   wrapped as `{SendResult, State}`.
    pub(in crate::codegen::core_erlang) fn generate_block_value_with_arguments_call_runtime_discriminated(
        &mut self,
        receiver: &Expression,
        args_expr: &Expression,
    ) -> Result<Document<'static>> {
        let fun_var = self.fresh_temp_var("Fun");
        let recv_code = self.expression_doc(receiver)?;
        let args_var = self.fresh_temp_var("Args");
        let args_code = self.expression_doc(args_expr)?;
        let current_state = self.current_state_var();
        let tier1_arity_var = self.fresh_temp_var("ArgsLen");
        let tier2_arity_var = self.fresh_temp_var("ArgsLenPlusOne");

        let preamble = docvec![
            "let ",
            leaf::var(fun_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(args_var.clone()),
            " = ",
            args_code,
            " in let ",
            leaf::var(tier1_arity_var.clone()),
            " = call 'erlang':'length'(",
            leaf::var(args_var.clone()),
            ") in let ",
            leaf::var(tier2_arity_var.clone()),
            " = call 'erlang':'+'(",
            leaf::var(tier1_arity_var.clone()),
            ", ",
            leaf::int_lit(1),
            ") in ",
        ];

        let case_doc = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(fun_var.clone()),
            ", ",
            leaf::var(tier1_arity_var),
            ") of 'true' when 'true' -> {call 'erlang':'apply'(",
            leaf::var(fun_var.clone()),
            ", ",
            leaf::var(args_var.clone()),
            "), ",
            leaf::var(current_state.clone()),
            "} 'false' when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(fun_var.clone()),
            ", ",
            leaf::var(tier2_arity_var),
            ") of 'true' when 'true' -> call 'erlang':'apply'(",
            leaf::var(fun_var.clone()),
            ", call 'erlang':'++'(",
            leaf::var(args_var.clone()),
            ", [",
            leaf::var(current_state.clone()),
            "])) 'false' when 'true' -> {call 'beamtalk_primitive':'send'(",
            leaf::var(fun_var),
            ", ",
            leaf::atom("valueWithArguments:"),
            ", [",
            leaf::var(args_var),
            "]), ",
            leaf::var(current_state),
            "} end end",
        ];

        Ok(docvec![preamble, case_doc])
    }

    /// BT-1213: Generates inline code for `[block_with_mutations] value` (or `value:`).
    ///
    /// When a block literal has captured mutations (variables read AND written, like
    /// `[errors := errors add: #foo]`), this inlines the block body with state threading
    /// instead of generating a Tier 2 fun + apply (which would produce a fun with an extra
    /// `StateAcc` parameter, causing a badarity crash when called with `value`).
    ///
    /// Returns a `{Result, NewState}` tuple (same contract as `generate_if_true_with_mutations`).
    /// Callers must unpack via `is_inline_tier2_block_value` / `repl_loop_mutated`.
    ///
    /// For `value:` variants, block parameters are bound to the provided arguments
    /// before inlining the body.
    fn generate_block_value_inline_with_mutations(
        &mut self,
        block: &Block,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // For value: variants, bind block parameters to argument values.
        // Use a scope for parameter bindings so they don't leak.
        let mut arg_bindings: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        if !arguments.is_empty() {
            self.push_scope();
        }
        for (i, param) in block.parameters.iter().enumerate() {
            if i < arguments.len() {
                let arg_var = self.fresh_temp_var(&param.name);
                let arg_doc = self.expression_doc(&arguments[i])?;
                arg_bindings.push(docvec![
                    "let ",
                    leaf::var(arg_var.clone()),
                    " = ",
                    arg_doc,
                    " in ",
                ]);
                self.bind_var(&param.name, &arg_var);
            }
        }

        // ValueType context: inline block body directly as sequential let-bindings.
        // No StateAcc needed — mutations are just Core Erlang variable shadowing.
        // Don't push/pop scope — the let-bindings must be visible to subsequent
        // method body expressions (the whole point of the mutation).
        if self.context == CodeGenContext::ValueType && !self.in_loop_body {
            let body = super::util::collect_body_exprs(&block.body);

            let mut parts: Vec<Document<'static>> =
                Vec::with_capacity(arg_bindings.len() + body.len());
            parts.extend(arg_bindings);

            for (i, expr) in body.iter().enumerate() {
                let is_last = i == body.len() - 1;
                if Self::is_local_var_assignment(expr) {
                    // Local assignment: shadow the variable with the new value
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let core_var = self
                                .lookup_var(&id.name)
                                .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
                            let val_doc = self.expression_doc(value)?;
                            if is_last {
                                // Last assignment: bind AND return the new value
                                parts.push(docvec![
                                    "let ",
                                    leaf::var(core_var.clone()),
                                    " = ",
                                    val_doc,
                                    " in ",
                                    leaf::var(core_var.clone()),
                                ]);
                            } else {
                                parts.push(docvec![
                                    "let ",
                                    leaf::var(core_var.clone()),
                                    " = ",
                                    val_doc,
                                    " in ",
                                ]);
                            }
                            // Bind in the OUTER scope so subsequent reads see the update
                            self.bind_var(&id.name, &core_var);
                        }
                    }
                } else if is_last {
                    // Last non-assignment: block result value
                    let doc = self.expression_doc(expr)?;
                    parts.push(doc);
                } else {
                    // Non-last non-assignment: sequence with let
                    let tmp = self.fresh_temp_var("seq");
                    let doc = self.expression_doc(expr)?;
                    parts.push(docvec!["let ", leaf::var(tmp), " = ", doc, " in ",]);
                }
            }

            if !arguments.is_empty() {
                self.pop_scope();
            }
            return Ok(Document::Vec(parts));
        }

        // Actor/REPL context: use StateAcc-based inlining (generate_conditional_branch_inline)
        let outer_state = self.current_state_var();

        let (branch_doc, _) = self.with_branch_context(|this| {
            // Set repl_loop_mutated so the REPL module unpacks the {Result, State} tuple
            if this.is_repl_mode() {
                this.set_repl_loop_mutated(true);
            }
            this.generate_conditional_branch_inline(block)
        })?;

        let mut parts: Vec<Document<'static>> = Vec::with_capacity(arg_bindings.len() + 2);
        parts.extend(arg_bindings);
        // Seed StateAcc from outer state
        parts.push(docvec!["let StateAcc = ", leaf::var(outer_state), " in ",]);
        parts.push(branch_doc);

        if !arguments.is_empty() {
            self.pop_scope();
        }
        Ok(Document::Vec(parts))
    }

    /// Tries to generate code for `ProtoObject` methods.
    ///
    /// `ProtoObject` methods are fundamental operations available on all objects:
    ///
    /// - `class` - Returns the class name (atom) of the receiver
    /// - `perform:withArguments:` - Dynamic message dispatch
    ///
    /// This function:
    /// - Returns `Ok(Some(doc))` if the message was a `ProtoObject` method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a `ProtoObject` method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # `ProtoObject` Methods
    ///
    /// ## class
    ///
    /// Returns the class name of any object. For primitives (Integer, String, Boolean),
    /// uses pattern matching. For actors, extracts the class from the object record.
    ///
    /// ```core-erlang
    /// % For primitives:
    /// case Receiver of
    ///   <I> when call 'erlang':'is_integer'(I) -> 'Integer'
    ///   <S> when call 'erlang':'is_binary'(S) -> 'String'
    ///   <'true'> when 'true' -> 'True'
    ///   <'false'> when 'true' -> 'False'
    ///   <'nil'> when 'true' -> 'Nil'
    ///   <Obj> when 'true' -> call 'erlang':'element'(2, Obj)  % Extract from record
    /// end
    /// ```
    ///
    /// ## perform:withArguments:
    ///
    /// Performs dynamic message dispatch - sends a message to an object at runtime.
    /// This is used by doesNotUnderstand handlers to forward messages.
    ///
    /// ```core-erlang
    /// % For actors (objects):
    /// let Pid = call 'erlang':'element'(4, Receiver) in
    /// call 'beamtalk_actor':'sync_send'(Pid, Selector, Arguments)
    /// ```
    #[allow(clippy::too_many_lines)] // one arm per ProtoObject intrinsic message
    pub(in crate::codegen::core_erlang) fn try_generate_protoobject_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // BT-412: `class` (unary, well-known) returns class as first-class object.
        if matches!(selector.well_known(), Some(WellKnownSelector::Class)) {
            // BT-1942: Hoist open-scope receiver (e.g. class method self-send).
            let (preamble, mut docs) = self.capture_subexpr_sequence(&[receiver], "Obj")?;
            let recv_doc = docs.remove(0);
            let call_doc = docvec![
                "call 'beamtalk_primitive':'class_of_object'(",
                recv_doc,
                ")",
            ];
            return Ok(Some(
                self.finalize_dispatch_with_preamble(preamble, call_doc, "ClassRes"),
            ));
        }

        // BT-2073: `perform:` family routes through the `WellKnownSelector` enum;
        // the classifier guarantees arity, so explicit `arguments.len()` guards
        // become `debug_assert_eq!` for parser-shape invariants.
        match selector.well_known() {
            Some(WellKnownSelector::PerformWithArgs) => {
                debug_assert_eq!(arguments.len(), 2);
                // BT-1942: Hoist open-scope receiver + args (e.g. class method self-sends).
                let (preamble, mut docs) = self
                    .capture_subexpr_sequence(&[receiver, &arguments[0], &arguments[1]], "Perf")?;
                let recv_doc = docs.remove(0);
                let sel_doc = docs.remove(0);
                let args_doc = docs.remove(0);
                let call_doc = docvec![
                    "call 'beamtalk_message_dispatch':'send'(",
                    recv_doc,
                    ", ",
                    sel_doc,
                    ", ",
                    args_doc,
                    ")",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble, call_doc, "PerfRes",
                )))
            }
            // BT-1664: Execute a class method in the caller's process,
            // bypassing the class object's gen_server.
            Some(WellKnownSelector::PerformLocallyWithArgs) => {
                debug_assert_eq!(arguments.len(), 2);
                // BT-1942: Hoist open-scope receiver + args (e.g. class method self-sends).
                let (preamble, mut docs) = self.capture_subexpr_sequence(
                    &[receiver, &arguments[0], &arguments[1]],
                    "PerfLoc",
                )?;
                let recv_doc = docs.remove(0);
                let sel_doc = docs.remove(0);
                let args_doc = docs.remove(0);
                let call_doc = docvec![
                    "call 'beamtalk_object_class':'local_call'(",
                    recv_doc,
                    ", ",
                    sel_doc,
                    ", ",
                    args_doc,
                    ")",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble,
                    call_doc,
                    "PerfLocRes",
                )))
            }
            Some(WellKnownSelector::Perform) => {
                debug_assert_eq!(arguments.len(), 1);
                // BT-1942: Hoist open-scope receiver + selector arg.
                let (preamble, mut docs) =
                    self.capture_subexpr_sequence(&[receiver, &arguments[0]], "Perf")?;
                let recv_doc = docs.remove(0);
                let sel_doc = docs.remove(0);
                let call_doc = docvec![
                    "call 'beamtalk_message_dispatch':'send'(",
                    recv_doc,
                    ", ",
                    sel_doc,
                    ", [])",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble, call_doc, "PerfRes",
                )))
            }
            _ => Ok(None),
        }
    }

    /// Tries to generate code for Object protocol methods.
    ///
    /// **DDD Context:** Object Protocol — Orchestrator
    ///
    /// Delegates to domain-aligned intrinsic groups:
    /// - Nil protocol (`isNil`, `notNil`, `ifNil:`, `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:`)
    /// - Error signaling (`error:`)
    /// - Object identity (`yourself`, `hash`)
    /// - Object reflection (`respondsTo:`, `fieldNames`, `fieldAt:`, `fieldAt:put:`)
    ///
    /// - Returns `Ok(Some(doc))` if the message was an Object method and code was generated
    /// - Returns `Ok(None)` if the message is NOT an Object method (caller should continue)
    /// - Returns `Err(...)` on error
    pub(in crate::codegen::core_erlang) fn try_generate_object_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if let Some(doc) = self.try_generate_nil_protocol(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        if let Some(doc) = self.try_generate_error_signaling(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        if let Some(doc) = self.try_generate_object_identity(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        if let Some(doc) = self.try_generate_object_reflection(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        Ok(None)
    }

    /// Generates code for nil-testing protocol methods.
    ///
    /// **DDD Context:** Object Protocol — Nil Testing
    ///
    /// - `isNil` — Returns true only for nil, false for everything else
    /// - `notNil` — Returns false only for nil, true for everything else
    /// - `ifNil:` — Conditional execution if nil
    /// - `ifNotNil:` — Conditional execution if not nil
    /// - `ifNil:ifNotNil:` / `ifNotNil:ifNil:` — Two-way conditional
    #[expect(
        clippy::too_many_lines,
        reason = "Six nil-testing variants (isNil, notNil, ifNil:, ifNotNil:, ifNil:ifNotNil:, ifNotNil:ifNil:) in single match"
    )]
    fn try_generate_nil_protocol(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // BT-2065/BT-2071: Dispatch nil-protocol intrinsics via the `WellKnownSelector`
        // enum. The classifier guarantees arity (e.g. `ifNil:ifNotNil:` requires
        // exactly two keyword parts), so explicit `arguments.len()` guards become
        // redundant — we keep one `debug_assert` per arm for paranoia.
        match selector.well_known() {
            Some(WellKnownSelector::IsNil) => {
                debug_assert!(arguments.is_empty());
                // BT-1942: Hoist open-scope receiver (e.g. class method self-send).
                let (preamble, mut docs) = self.capture_subexpr_sequence(&[receiver], "Obj")?;
                let recv_doc = docs.remove(0);
                let recv_var = self.fresh_temp_var("Obj");
                let call_doc = docvec![
                    "let ",
                    leaf::var(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in case ",
                    leaf::var(recv_var),
                    " of <'nil'> when 'true' -> 'true' <_> when 'true' -> 'false' end",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble, call_doc, "IsNilRes",
                )))
            }
            Some(WellKnownSelector::NotNil) => {
                debug_assert!(arguments.is_empty());
                // BT-1942: Hoist open-scope receiver (e.g. class method self-send).
                let (preamble, mut docs) = self.capture_subexpr_sequence(&[receiver], "Obj")?;
                let recv_doc = docs.remove(0);
                let recv_var = self.fresh_temp_var("Obj");
                let call_doc = docvec![
                    "let ",
                    leaf::var(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in case ",
                    leaf::var(recv_var),
                    " of <'nil'> when 'true' -> 'false' <_> when 'true' -> 'true' end",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble,
                    call_doc,
                    "NotNilRes",
                )))
            }
            Some(WellKnownSelector::IfNil) => {
                debug_assert_eq!(arguments.len(), 1);
                // BT-1942: Hoist open-scope receiver/block (e.g. class method self-sends).
                let (preamble, mut docs) =
                    self.capture_subexpr_sequence(&[receiver, &arguments[0]], "IfNil")?;
                let recv_doc = docs.remove(0);
                let block_doc = docs.remove(0);
                let recv_var = self.fresh_temp_var("Obj");
                let block_var = self.fresh_temp_var("NilBlk");
                let call_doc = docvec![
                    "let ",
                    leaf::var(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in let ",
                    leaf::var(block_var.clone()),
                    " = ",
                    block_doc,
                    " in case ",
                    leaf::var(recv_var.clone()),
                    " of <'nil'> when 'true' -> apply ",
                    leaf::var(block_var),
                    " () <_> when 'true' -> ",
                    leaf::var(recv_var),
                    " end",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble, call_doc, "IfNilRes",
                )))
            }
            Some(WellKnownSelector::IfNotNil) => {
                debug_assert_eq!(arguments.len(), 1);
                let selector_name = WellKnownSelector::IfNotNil.as_str();
                // BT-1226: When in actor context or loop body, check if the block
                // contains field mutations. If so, generate inline state-threaded code
                // instead of a closure to ensure mutations persist correctly.
                if self.context == CodeGenContext::Actor || self.in_loop_body {
                    if let Expression::Block(block) = &arguments[0] {
                        let analysis = block_analysis::analyze_block(block);
                        // BT-2356: inline when a nested list op mutates an outer local (see IfTrue).
                        let needs_threading = self.needs_mutation_threading(&analysis)
                            || self.body_has_list_op_cross_scope_mutations(block)
                            || (self.in_loop_body && !analysis.local_writes.is_empty());
                        if needs_threading {
                            // Validate arity before generating mutation-threaded code.
                            // This ensures a block with >1 params still raises
                            // BlockArityMismatch rather than producing invalid Core Erlang.
                            validate_if_not_nil_block(&arguments[0], selector_name)?;
                            let doc = self.generate_if_not_nil_with_mutations(receiver, block)?;
                            return Ok(Some(doc));
                        }
                    }
                }
                // If the block has 0 parameters, don't pass the receiver (avoids badarity)
                // BT-1942: Hoist open-scope receiver/block (e.g. class method self-sends).
                let block_takes_arg = validate_if_not_nil_block(&arguments[0], selector_name)?;
                let (preamble, mut docs) =
                    self.capture_subexpr_sequence(&[receiver, &arguments[0]], "IfNotNil")?;
                let recv_doc = docs.remove(0);
                let block_doc = docs.remove(0);
                let recv_var = self.fresh_temp_var("Obj");
                let block_var = self.fresh_temp_var("NotNilBlk");
                let apply = not_nil_apply(&block_var, &recv_var, block_takes_arg);
                let call_doc = docvec![
                    "let ",
                    leaf::var(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in let ",
                    leaf::var(block_var),
                    " = ",
                    block_doc,
                    " in case ",
                    leaf::var(recv_var),
                    " of <'nil'> when 'true' -> 'nil' <_> when 'true' -> ",
                    apply,
                    " end",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble,
                    call_doc,
                    "IfNotNilRes",
                )))
            }
            Some(WellKnownSelector::IfNilIfNotNil) => {
                debug_assert_eq!(arguments.len(), 2);
                let selector_name = WellKnownSelector::IfNilIfNotNil.as_str();
                // If the notNil block has 0 parameters, don't pass the receiver
                // BT-1942: Hoist open-scope sub-expressions (e.g. class method self-sends).
                let block_takes_arg = validate_if_not_nil_block(&arguments[1], selector_name)?;
                let (preamble, mut docs) = self.capture_subexpr_sequence(
                    &[receiver, &arguments[0], &arguments[1]],
                    "IfNilNotNil",
                )?;
                let recv_doc = docs.remove(0);
                let nil_doc = docs.remove(0);
                let not_nil_doc = docs.remove(0);
                let recv_var = self.fresh_temp_var("Obj");
                let nil_var = self.fresh_temp_var("NilBlk");
                let not_nil_var = self.fresh_temp_var("NotNilBlk");
                let apply = not_nil_apply(&not_nil_var, &recv_var, block_takes_arg);
                let call_doc = docvec![
                    "let ",
                    leaf::var(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in let ",
                    leaf::var(nil_var.clone()),
                    " = ",
                    nil_doc,
                    " in let ",
                    leaf::var(not_nil_var),
                    " = ",
                    not_nil_doc,
                    " in case ",
                    leaf::var(recv_var),
                    " of <'nil'> when 'true' -> apply ",
                    leaf::var(nil_var),
                    " () <_> when 'true' -> ",
                    apply,
                    " end",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble,
                    call_doc,
                    "IfNilNotNilRes",
                )))
            }
            Some(WellKnownSelector::IfNotNilIfNil) => {
                debug_assert_eq!(arguments.len(), 2);
                let selector_name = WellKnownSelector::IfNotNilIfNil.as_str();
                // If the notNil block has 0 parameters, don't pass the receiver
                // BT-1942: Hoist open-scope sub-expressions (e.g. class method self-sends).
                let block_takes_arg = validate_if_not_nil_block(&arguments[0], selector_name)?;
                let (preamble, mut docs) = self.capture_subexpr_sequence(
                    &[receiver, &arguments[0], &arguments[1]],
                    "IfNotNilNil",
                )?;
                let recv_doc = docs.remove(0);
                let not_nil_doc = docs.remove(0);
                let nil_doc = docs.remove(0);
                let recv_var = self.fresh_temp_var("Obj");
                let not_nil_var = self.fresh_temp_var("NotNilBlk");
                let nil_var = self.fresh_temp_var("NilBlk");
                let apply = not_nil_apply(&not_nil_var, &recv_var, block_takes_arg);
                let call_doc = docvec![
                    "let ",
                    leaf::var(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in let ",
                    leaf::var(not_nil_var.clone()),
                    " = ",
                    not_nil_doc,
                    " in let ",
                    leaf::var(nil_var.clone()),
                    " = ",
                    nil_doc,
                    " in case ",
                    leaf::var(recv_var),
                    " of <'nil'> when 'true' -> apply ",
                    leaf::var(nil_var),
                    " () <_> when 'true' -> ",
                    apply,
                    " end",
                ];
                Ok(Some(self.finalize_dispatch_with_preamble(
                    preamble,
                    call_doc,
                    "IfNotNilNilRes",
                )))
            }
            // Not a nil-protocol intrinsic — let the caller continue.
            _ => Ok(None),
        }
    }

    /// Generates code for error signaling methods.
    ///
    /// **DDD Context:** Object Protocol — Error Signaling
    ///
    /// - `error:` — Smalltalk-style error signaling with receiver's class
    fn try_generate_error_signaling(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // BT-1254: `error:` on a ClassReference is a class method call (e.g. `Result error: reason`),
        // not the Object#error: error-signaling intrinsic. Skip so class method dispatch handles it.
        // Also covers `Logger error: "msg"` — see `WellKnownSelector::Error` rustdoc
        // for the dual-use caveat (Object >> error: vs Logger class >> error:).
        if matches!(receiver, Expression::ClassReference { .. }) {
            return Ok(None);
        }
        // BT-2685: `(Erlang mod) error: arg` / `error:metadata:` is an Erlang FFI call to a
        // function named `error` (e.g. `Console error:` delegating to `beamtalk_console:error/1`),
        // not the Object#error: error-signaling intrinsic. Skip so the FFI proxy handles it.
        if Self::is_erlang_ffi_receiver(receiver) {
            return Ok(None);
        }
        // BT-2073: `error:` is well-known; dispatch via the enum.
        if !matches!(selector.well_known(), Some(WellKnownSelector::Error)) {
            return Ok(None);
        }
        debug_assert_eq!(arguments.len(), 1);
        // BT-1942: Hoist open-scope receiver + message (e.g. class method self-sends).
        let (preamble, mut docs) =
            self.capture_subexpr_sequence(&[receiver, &arguments[0]], "Err")?;
        let recv_doc = docs.remove(0);
        let msg_doc = docs.remove(0);
        let recv_var = self.fresh_temp_var("Obj");
        let msg_var = self.fresh_temp_var("Msg");
        let class_var = self.fresh_temp_var("Class");
        let err0 = self.fresh_temp_var("Err");
        let err1 = self.fresh_temp_var("Err");

        let call_doc = docvec![
            "let ",
            leaf::var(recv_var.clone()),
            " = ",
            recv_doc,
            " in let ",
            leaf::var(msg_var.clone()),
            " = ",
            msg_doc,
            " in let ",
            leaf::var(class_var.clone()),
            " = call 'beamtalk_primitive':'class_of'(",
            leaf::var(recv_var),
            ") in let ",
            leaf::var(err0.clone()),
            " = call 'beamtalk_error':'new'('user_error', ",
            leaf::var(class_var),
            ") in let ",
            leaf::var(err1.clone()),
            " = call 'beamtalk_error':'with_message'(",
            leaf::var(err0),
            ", ",
            leaf::var(msg_var),
            ") in call 'beamtalk_error':'raise'(",
            leaf::var(err1),
            ")",
        ];
        Ok(Some(self.finalize_dispatch_with_preamble(
            preamble, call_doc, "ErrRes",
        )))
    }

    /// Generates code for object identity and representation methods.
    ///
    /// **DDD Context:** Object Protocol — Object Identity
    ///
    /// - `yourself` — Identity: returns the receiver unchanged
    /// - `hash` — Hash using `erlang:phash2/1`
    fn try_generate_object_identity(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // BT-2073: `hash` (well-known) — dispatch via the enum so the
        // classifier validates kind/arity.
        if matches!(selector.well_known(), Some(WellKnownSelector::Hash)) {
            debug_assert!(arguments.is_empty());
            // BT-1942: Hoist open-scope receiver (e.g. class method self-send).
            let (preamble, mut docs) = self.capture_subexpr_sequence(&[receiver], "Obj")?;
            let recv_doc = docs.remove(0);
            let recv_var = self.fresh_temp_var("Obj");
            let call_doc = docvec![
                "let ",
                leaf::var(recv_var.clone()),
                " = ",
                recv_doc,
                " in call 'erlang':'phash2'(",
                leaf::var(recv_var),
                ")",
            ];
            return Ok(Some(
                self.finalize_dispatch_with_preamble(preamble, call_doc, "HashRes"),
            ));
        }
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "yourself" if arguments.is_empty() => {
                    // Identity: just return the receiver
                    // BT-1942: Preserve open scope (e.g. class method self-send) so the
                    // mutated ClassVarsN binding propagates upward. Wrap as a dispatch
                    // value so `finalize_dispatch_with_preamble` can emit the preamble.
                    let (preamble, mut docs) =
                        self.capture_subexpr_sequence(&[receiver], "Yourself")?;
                    let recv_doc = docs.remove(0);
                    Ok(Some(self.finalize_dispatch_with_preamble(
                        preamble,
                        recv_doc,
                        "YourselfRes",
                    )))
                }
                // BT-477: printString removed as intrinsic — now uses polymorphic
                // dispatch via Object >> printString and per-class overrides.
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    /// Generates code for object reflection and introspection methods.
    ///
    /// **DDD Context:** Object Protocol — Object Reflection
    ///
    /// - `respondsTo:` — Check if object responds to a selector
    /// - `fieldNames` — Get list of instance variable names (actors only)
    /// - `fieldAt:` — Read instance variable by name
    /// - `fieldAt:put:` — Write instance variable by name
    #[expect(
        clippy::too_many_lines,
        reason = "fieldAt: and fieldAt:put: require verbose type guards for actor vs primitive dispatch"
    )]
    fn try_generate_object_reflection(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // BT-2073: `fieldNames` (well-known unary). The classifier validates
        // kind/arity, so explicit `arguments.is_empty()` becomes a `debug_assert!`.
        if matches!(selector.well_known(), Some(WellKnownSelector::FieldNames)) {
            debug_assert!(arguments.is_empty());
            // BT-1321: Fast-path for `self` receiver in actor context.
            // `Self` is a `#beamtalk_object{..., pid: self()}` tuple, so the
            // normal is_tuple branch would call sync_send(self()) which is
            // gen_server:call(self(), ...) → deadlock.
            // Use beamtalk_primitive:send(State, ...) directly instead.
            if let Expression::Identifier(id) = receiver {
                if id.name == "self"
                    && self.context == super::CodeGenContext::Actor
                    && self.lookup_var("self").is_none()
                {
                    let doc = docvec![
                        "call 'beamtalk_primitive':'send'(",
                        leaf::var(self.current_state_var()),
                        ", 'fieldNames', [])",
                    ];
                    return Ok(Some(doc));
                }
            }

            // BT-1942: Hoist open-scope receiver (e.g. class method self-send).
            let (preamble, mut docs) = self.capture_subexpr_sequence(&[receiver], "FNames")?;
            let recv_doc = docs.remove(0);
            let receiver_var = self.fresh_var("Receiver");
            let pid_var = self.fresh_var("Pid");

            // BT-924: If receiver is a map (value object or stdlib tagged map),
            // delegate to beamtalk_primitive:send which routes through the
            // correct dispatch/3 for the receiver's class kind.
            // Actor (tuple) receivers use sync_send (ADR-0043).
            let call_doc = docvec![
                "let ",
                leaf::var(receiver_var.clone()),
                " = ",
                recv_doc,
                " in case call 'erlang':'is_map'(",
                leaf::var(receiver_var.clone()),
                ") of <'true'> when 'true' -> call 'beamtalk_primitive':'send'(",
                leaf::var(receiver_var.clone()),
                ", 'fieldNames', []) <_> when 'true' -> let ",
                leaf::var(pid_var.clone()),
                " = call 'erlang':'element'(4, ",
                leaf::var(receiver_var),
                ") in call 'beamtalk_actor':'sync_send'(",
                leaf::var(pid_var),
                ", 'fieldNames', []) end",
            ];
            return Ok(Some(self.finalize_dispatch_with_preamble(
                preamble,
                call_doc,
                "FNamesRes",
            )));
        }
        match selector {
            MessageSelector::Keyword(_) => {
                // `respondsTo:` is well-known; dispatch via the enum first so arity
                // correctness is guaranteed by the classifier.
                if matches!(selector.well_known(), Some(WellKnownSelector::RespondsTo)) {
                    debug_assert_eq!(arguments.len(), 1);
                    // BT-1942: Hoist open-scope receiver + selector (e.g. class method self-sends).
                    let (preamble, mut docs) =
                        self.capture_subexpr_sequence(&[receiver, &arguments[0]], "RespTo")?;
                    let recv_doc = docs.remove(0);
                    let sel_doc = docs.remove(0);
                    let receiver_var = self.fresh_var("Receiver");
                    let selector_var = self.fresh_var("Selector");

                    let call_doc = docvec![
                        "let ",
                        leaf::var(receiver_var.clone()),
                        " = ",
                        recv_doc,
                        " in let ",
                        leaf::var(selector_var.clone()),
                        " = ",
                        sel_doc,
                        " in call 'beamtalk_primitive':'responds_to'(",
                        leaf::var(receiver_var),
                        ", ",
                        leaf::var(selector_var),
                        ")",
                    ];
                    return Ok(Some(self.finalize_dispatch_with_preamble(
                        preamble,
                        call_doc,
                        "RespToRes",
                    )));
                }

                // BT-2073: `fieldAt:` and `fieldAt:put:` route via the enum so
                // the classifier validates kind/arity (`fieldAt:put:` requires
                // exactly two keyword parts, not a flattened single part).
                match selector.well_known() {
                    Some(WellKnownSelector::FieldAt) => {
                        debug_assert_eq!(arguments.len(), 1);
                        // BT-1321: Fast-path for `self` receiver in actor context.
                        // Avoids sync_send(self()) → gen_server:call(self(), ...) → deadlock.
                        if let Expression::Identifier(id) = receiver {
                            if id.name == "self"
                                && self.context == super::CodeGenContext::Actor
                                && self.lookup_var("self").is_none()
                            {
                                let name_var = self.fresh_var("Name");
                                let name_code = self.expression_doc(&arguments[0])?;
                                let doc = docvec![
                                    "let ",
                                    leaf::var(name_var.clone()),
                                    " = ",
                                    name_code,
                                    " in call 'beamtalk_primitive':'send'(",
                                    leaf::var(self.current_state_var()),
                                    ", 'fieldAt:', [",
                                    leaf::var(name_var),
                                    "])",
                                ];
                                return Ok(Some(doc));
                            }
                        }

                        // BT-1942: Hoist open-scope receiver + name (e.g. class method self-sends).
                        let (preamble, mut docs) =
                            self.capture_subexpr_sequence(&[receiver, &arguments[0]], "FAt")?;
                        let recv_doc = docs.remove(0);
                        let name_doc = docs.remove(0);
                        let receiver_var = self.fresh_var("Receiver");
                        let name_var = self.fresh_var("Name");
                        let pid_var = self.fresh_var("Pid");
                        let class_var = self.fresh_var("Class");
                        let error_base = self.fresh_var("Err");
                        let error_sel = self.fresh_var("Err");
                        let error_hint = self.fresh_var("Err");
                        // BT-924: primitive value types (Integer, String, etc.) have no slots;
                        // user-defined value objects are maps and use beamtalk_reflection.
                        let hint = leaf::binary_lit("Value types have no instance variables");

                        // BT-924: The non-actor branch is split:
                        //   - map receiver → delegate to beamtalk_primitive:send (routes
                        //     through dispatch/3 which respects ClassKind::Value vs Object)
                        //   - other (primitive literal) → raise immutable_value
                        let call_doc = docvec![
                            "let ",
                            leaf::var(receiver_var.clone()),
                            " = ",
                            recv_doc,
                            " in let ",
                            leaf::var(name_var.clone()),
                            " = ",
                            name_doc,
                            " in case case call 'erlang':'is_tuple'(",
                            leaf::var(receiver_var.clone()),
                            ") of <'true'> when 'true' -> case call 'erlang':'=='(call 'erlang':'tuple_size'(",
                            leaf::var(receiver_var.clone()),
                            "), 4) of <'true'> when 'true' -> call 'erlang':'=='(call 'erlang':'element'(1, ",
                            leaf::var(receiver_var.clone()),
                            "), 'beamtalk_object') <_> when 'true' -> 'false' end <_> when 'true' -> 'false' end of ",
                            "<'true'> when 'true' -> let ",
                            leaf::var(pid_var.clone()),
                            " = call 'erlang':'element'(4, ",
                            leaf::var(receiver_var.clone()),
                            ") in call 'beamtalk_actor':'sync_send'(",
                            leaf::var(pid_var),
                            ", 'fieldAt:', [",
                            leaf::var(name_var.clone()),
                            "])",
                            // Non-actor: delegate map receivers to beamtalk_primitive:send
                            // which routes through dispatch/3 (respects ClassKind)
                            " <_> when 'true' -> case call 'erlang':'is_map'(",
                            leaf::var(receiver_var.clone()),
                            ") of <'true'> when 'true' -> call 'beamtalk_primitive':'send'(",
                            leaf::var(receiver_var.clone()),
                            ", 'fieldAt:', [",
                            leaf::var(name_var),
                            "]) <_> when 'true' -> let ",
                            leaf::var(class_var.clone()),
                            " = call 'beamtalk_primitive':'class_of'(",
                            leaf::var(receiver_var),
                            ") in let ",
                            leaf::var(error_base.clone()),
                            " = call 'beamtalk_error':'new'('immutable_value', ",
                            leaf::var(class_var),
                            ") in let ",
                            leaf::var(error_sel.clone()),
                            " = call 'beamtalk_error':'with_selector'(",
                            leaf::var(error_base),
                            ", 'fieldAt:') in let ",
                            leaf::var(error_hint.clone()),
                            " = call 'beamtalk_error':'with_hint'(",
                            leaf::var(error_sel),
                            ", ",
                            hint,
                            ") in call 'beamtalk_error':'raise'(",
                            leaf::var(error_hint),
                            ") end end",
                        ];
                        Ok(Some(self.finalize_dispatch_with_preamble(
                            preamble, call_doc, "FAtRes",
                        )))
                    }
                    Some(WellKnownSelector::FieldAtPut) => {
                        debug_assert_eq!(arguments.len(), 2);
                        // BT-1321: Fast-path for `self` receiver in actor context.
                        // Avoids sync_send(self()) → gen_server:call(self(), ...) → deadlock.
                        //
                        // BT-1324: Method-body-level state threading is handled by
                        // generate_method_body_with_reply via is_self_field_at_put/
                        // generate_self_field_at_put_open, which intercepts before
                        // expression_doc is called. This intrinsic path is a
                        // deadlock-avoidance fallback for contexts where method-body
                        // threading is unavailable (e.g., inside blocks). It delegates
                        // to beamtalk_primitive:send(State, 'fieldAt:put:', ...) which
                        // returns the assigned value but does NOT thread the updated
                        // state back into the actor's State variable.
                        if let Expression::Identifier(id) = receiver {
                            if id.name == "self"
                                && self.context == super::CodeGenContext::Actor
                                && self.lookup_var("self").is_none()
                            {
                                let name_var = self.fresh_var("Name");
                                let value_var = self.fresh_var("Value");
                                let name_code = self.expression_doc(&arguments[0])?;
                                let value_code = self.expression_doc(&arguments[1])?;
                                let doc = docvec![
                                    "let ",
                                    leaf::var(name_var.clone()),
                                    " = ",
                                    name_code,
                                    " in let ",
                                    leaf::var(value_var.clone()),
                                    " = ",
                                    value_code,
                                    " in call 'beamtalk_primitive':'send'(",
                                    leaf::var(self.current_state_var()),
                                    ", 'fieldAt:put:', [",
                                    leaf::var(name_var),
                                    ", ",
                                    leaf::var(value_var),
                                    "])",
                                ];
                                return Ok(Some(doc));
                            }
                        }

                        // BT-1942: Hoist open-scope receiver + name + value (e.g. class method self-sends).
                        let (preamble, mut docs) = self.capture_subexpr_sequence(
                            &[receiver, &arguments[0], &arguments[1]],
                            "FAtPut",
                        )?;
                        let recv_doc = docs.remove(0);
                        let name_doc = docs.remove(0);
                        let value_doc = docs.remove(0);
                        let receiver_var = self.fresh_var("Receiver");
                        let name_var = self.fresh_var("Name");
                        let value_var = self.fresh_var("Value");
                        let pid_var = self.fresh_var("Pid");
                        let class_var = self.fresh_var("Class");
                        let error_base = self.fresh_var("Err");
                        let error_sel = self.fresh_var("Err");
                        let error_hint = self.fresh_var("Err");
                        let hint = leaf::binary_lit(
                            "Cannot modify slot on value type \u{2014} use withSlot: to create a new instance",
                        );

                        let call_doc = docvec![
                            "let ",
                            leaf::var(receiver_var.clone()),
                            " = ",
                            recv_doc,
                            " in let ",
                            leaf::var(name_var.clone()),
                            " = ",
                            name_doc,
                            " in let ",
                            leaf::var(value_var.clone()),
                            " = ",
                            value_doc,
                            " in case case call 'erlang':'is_tuple'(",
                            leaf::var(receiver_var.clone()),
                            ") of <'true'> when 'true' -> case call 'erlang':'=='(call 'erlang':'tuple_size'(",
                            leaf::var(receiver_var.clone()),
                            "), 4) of <'true'> when 'true' -> call 'erlang':'=='(call 'erlang':'element'(1, ",
                            leaf::var(receiver_var.clone()),
                            "), 'beamtalk_object') <_> when 'true' -> 'false' end <_> when 'true' -> 'false' end of ",
                            "<'true'> when 'true' -> let ",
                            leaf::var(pid_var.clone()),
                            " = call 'erlang':'element'(4, ",
                            leaf::var(receiver_var.clone()),
                            ") in call 'beamtalk_actor':'sync_send'(",
                            leaf::var(pid_var),
                            ", 'fieldAt:put:', [",
                            leaf::var(name_var),
                            ", ",
                            leaf::var(value_var),
                            "])",
                            " <_> when 'true' -> let ",
                            leaf::var(class_var.clone()),
                            " = call 'beamtalk_primitive':'class_of'(",
                            leaf::var(receiver_var),
                            ") in let ",
                            leaf::var(error_base.clone()),
                            " = call 'beamtalk_error':'new'('immutable_value', ",
                            leaf::var(class_var),
                            ") in let ",
                            leaf::var(error_sel.clone()),
                            " = call 'beamtalk_error':'with_selector'(",
                            leaf::var(error_base),
                            ", 'fieldAt:put:') in let ",
                            leaf::var(error_hint.clone()),
                            " = call 'beamtalk_error':'with_hint'(",
                            leaf::var(error_sel),
                            ", ",
                            hint,
                            ") in call 'beamtalk_error':'raise'(",
                            leaf::var(error_hint),
                            ") end",
                        ];
                        Ok(Some(self.finalize_dispatch_with_preamble(
                            preamble,
                            call_doc,
                            "FAtPutRes",
                        )))
                    }
                    _ => Ok(None),
                }
            }
            MessageSelector::Unary(_) | MessageSelector::Binary(_) => Ok(None),
        }
    }
    /// BT-915: Tries to generate inline code for Boolean conditionals with field mutation
    /// state threading.
    ///
    /// Handles `ifTrue:`, `ifFalse:`, and `ifTrue:ifFalse:` in actor context when at
    /// least one block argument contains field mutations (`self.slot :=`).
    ///
    /// - Returns `Ok(Some(doc))` generating `{Result, NewState}` tuple when mutations detected
    /// - Returns `Ok(None)` otherwise (fall through to `beamtalk_message_dispatch:send`)
    pub(in crate::codegen::core_erlang) fn try_generate_boolean_protocol(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        use super::CodeGenContext;
        use super::block_analysis;

        // Only applies in actor context, REPL context, or when inside a loop body
        // (BT-1053: value-type methods may have inline conditionals mutating captured
        // locals inside do: blocks).
        // BT-1392: Allow REPL context — the REPL State map threads bindings the same
        // way as actor State, so inline case expressions work directly.
        if self.context != CodeGenContext::Actor
            && self.context != CodeGenContext::Repl
            && !self.in_loop_body
        {
            return Ok(None);
        }

        match selector.well_known() {
            Some(WellKnownSelector::IfTrue) => {
                debug_assert_eq!(arguments.len(), 1);
                if let Expression::Block(block) = &arguments[0] {
                    let analysis = block_analysis::analyze_block(block);
                    // BT-1053: When inside a loop body, also trigger for any local write
                    // (the outer loop has already determined which locals need threading).
                    // BT-2356: also inline when the branch contains a nested list op that
                    // mutates an outer local — otherwise the conditional falls through to a
                    // runtime `send` whose `nil`-on-false result breaks the `{Value, State}`
                    // contract the method-body sequencer expects (badarg on element/2).
                    let needs_threading = self.needs_mutation_threading(&analysis)
                        || self.body_has_list_op_cross_scope_mutations(block)
                        || (self.in_loop_body && !analysis.local_writes.is_empty());
                    if needs_threading {
                        // BT-1392: Set repl_loop_mutated so the REPL unpacks {Result, State}
                        if self.is_repl_mode() {
                            self.set_repl_loop_mutated(true);
                        }
                        let doc = self.generate_if_true_with_mutations(receiver, block)?;
                        return Ok(Some(doc));
                    }
                }
            }
            Some(WellKnownSelector::IfFalse) => {
                debug_assert_eq!(arguments.len(), 1);
                if let Expression::Block(block) = &arguments[0] {
                    let analysis = block_analysis::analyze_block(block);
                    // BT-2356: inline when a nested list op mutates an outer local (see IfTrue).
                    let needs_threading = self.needs_mutation_threading(&analysis)
                        || self.body_has_list_op_cross_scope_mutations(block)
                        || (self.in_loop_body && !analysis.local_writes.is_empty());
                    if needs_threading {
                        if self.is_repl_mode() {
                            self.set_repl_loop_mutated(true);
                        }
                        let doc = self.generate_if_false_with_mutations(receiver, block)?;
                        return Ok(Some(doc));
                    }
                }
            }
            Some(WellKnownSelector::IfTrueIfFalse) => {
                debug_assert_eq!(arguments.len(), 2);
                if let (Expression::Block(true_block), Expression::Block(false_block)) =
                    (&arguments[0], &arguments[1])
                {
                    let true_analysis = block_analysis::analyze_block(true_block);
                    let false_analysis = block_analysis::analyze_block(false_block);
                    // BT-2356: inline when either branch has a nested list op mutating an
                    // outer local (see IfTrue).
                    let needs_threading = self.needs_mutation_threading(&true_analysis)
                        || self.needs_mutation_threading(&false_analysis)
                        || self.body_has_list_op_cross_scope_mutations(true_block)
                        || self.body_has_list_op_cross_scope_mutations(false_block)
                        || (self.in_loop_body
                            && (!true_analysis.local_writes.is_empty()
                                || !false_analysis.local_writes.is_empty()));
                    if needs_threading {
                        if self.is_repl_mode() {
                            self.set_repl_loop_mutated(true);
                        }
                        let doc = self.generate_if_true_if_false_with_mutations(
                            receiver,
                            true_block,
                            false_block,
                        )?;
                        return Ok(Some(doc));
                    }
                }
            }
            _ => {}
        }

        Ok(None)
    }

    /// BT-1435: Tries to generate inline `logger:log/3` calls for Logger class sends.
    ///
    /// Recognizes `Logger debug:`, `Logger info:`, `Logger warn:`, `Logger error:`
    /// (and their `*:metadata:` variants) and generates direct OTP `logger:log/3`
    /// calls with domain metadata injected at the call site.
    ///
    /// Non-logging selectors fall through to normal class dispatch.
    ///
    /// - Returns `Ok(Some(doc))` if the message was a Logger intrinsic
    /// - Returns `Ok(None)` if not a Logger message (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Generated Code
    ///
    /// For `Logger debug: "msg"`:
    /// ```erlang
    /// call 'logger':'log'('debug', {"~ts", [<<"msg">>]}, #{
    ///     'domain' => ['beamtalk', 'user'],
    ///     'beamtalk_class' => 'Counter',
    ///     'beamtalk_selector' => 'increment'
    /// })
    /// ```
    ///
    /// For `Logger debug: "msg" metadata: #{"key" => val}`:
    /// ```erlang
    /// call 'logger':'log'('debug', {"~ts", [<<"msg">>]}, call 'maps':'merge'(UserMeta, #{
    ///     'domain' => ['beamtalk', 'user'],
    ///     'beamtalk_class' => 'Counter',
    ///     'beamtalk_selector' => 'increment'
    /// }))
    /// ```
    pub(in crate::codegen::core_erlang) fn try_generate_logger_intrinsic(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // Only intercept ClassReference receivers named "Logger"
        let class_name = match receiver {
            Expression::ClassReference { name, .. } if name.name == "Logger" => &name.name,
            _ => return Ok(None),
        };

        // Map Beamtalk selectors to OTP logger levels
        let (level, has_metadata) = match selector {
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                match selector_name.as_str() {
                    "debug:" if arguments.len() == 1 => ("debug", false),
                    "info:" if arguments.len() == 1 => ("info", false),
                    "warn:" if arguments.len() == 1 => ("warning", false),
                    "error:" if arguments.len() == 1 => ("error", false),
                    "debug:metadata:" if arguments.len() == 2 => ("debug", true),
                    "info:metadata:" if arguments.len() == 2 => ("info", true),
                    "warn:metadata:" if arguments.len() == 2 => ("warning", true),
                    "error:metadata:" if arguments.len() == 2 => ("error", true),
                    // any other selector — fall through to normal dispatch
                    _ => return Ok(None),
                }
            }
            _ => return Ok(None),
        };

        // Get the message argument, wrapped as a format+args tuple so OTP's
        // logger formatter can handle it.  Beamtalk strings are binaries, but
        // logger:log/3 expects a char-list string, a report, or {Format, Args}.
        // Passing a binary directly causes the formatter to crash with
        // "FORMATTER CRASH: {string, <<\"...\">>}".
        //
        // BT-1942: Hoist message arg (and optional metadata arg) so class method
        // self-sends in sub-expression position (e.g. `Logger info: (self tick)`)
        // thread their class var mutations through to the enclosing scope.
        let arg_exprs: Vec<&Expression> = arguments.iter().collect();
        let (preamble, mut arg_docs) = self.capture_subexpr_sequence(&arg_exprs, "LogArg")?;
        let raw_msg_doc = arg_docs.remove(0);
        let msg_doc = docvec!["{\"~ts\", [", raw_msg_doc, "]}"];

        // Build domain metadata map
        // The class name comes from the compilation context (the class being compiled)
        let ctx_class = self.class_name();
        let ctx_selector = self
            .current_method_selector
            .clone()
            .unwrap_or_else(|| "unknown".to_string());

        let _ = class_name; // Logger class name itself is not needed in the metadata

        let metadata_map_doc = docvec![
            "~{",
            "'domain' => ['beamtalk' | ['user']], ",
            "'beamtalk_class' => ",
            leaf::atom(ctx_class),
            ", ",
            "'beamtalk_selector' => ",
            leaf::atom(ctx_selector),
            "}~",
        ];

        // logger:log/3 returns 'ok', but Beamtalk Logger methods return 'nil'.
        // Bind the result to a temp var and return 'nil'.
        let discard_var = self.fresh_temp_var("LogOk");

        let log_call_doc = if has_metadata {
            // With user metadata: merge user map with compiler-injected map
            // BT-1942: user_meta_doc is the hoisted doc for arguments[1].
            let user_meta_doc = arg_docs.remove(0);
            let merge_var = self.fresh_temp_var("LogMeta");
            docvec![
                "let ",
                leaf::var(merge_var.clone()),
                " = call 'maps':'merge'(",
                user_meta_doc,
                ", ",
                metadata_map_doc,
                ") in call 'logger':'log'(",
                leaf::atom(level),
                ", ",
                msg_doc,
                ", ",
                leaf::var(merge_var),
                ")"
            ]
        } else {
            // Without user metadata: use compiler-injected map directly
            docvec![
                "call 'logger':'log'(",
                leaf::atom(level),
                ", ",
                msg_doc,
                ", ",
                metadata_map_doc,
                ")"
            ]
        };

        let call_doc = docvec![
            "let ",
            leaf::var(discard_var),
            " = ",
            log_call_doc,
            " in 'nil'"
        ];

        Ok(Some(self.finalize_dispatch_with_preamble(
            preamble, call_doc, "LogRes",
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, BlockParameter, ExpressionStatement, Literal};
    use crate::source_analysis::Span;

    #[test]
    fn test_validate_if_not_nil_block_zero_args() {
        let block = Expression::Block(Block {
            parameters: vec![],
            body: vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                Span::new(1, 2),
            ))],
            span: Span::new(0, 3),
        });
        assert!(!validate_if_not_nil_block(&block, "ifNotNil:").unwrap());
    }

    #[test]
    fn test_validate_if_not_nil_block_one_arg() {
        let block = Expression::Block(Block {
            parameters: vec![BlockParameter::new("v", Span::new(1, 2))],
            body: vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                Span::new(5, 6),
            ))],
            span: Span::new(0, 7),
        });
        assert!(validate_if_not_nil_block(&block, "ifNotNil:").unwrap());
    }

    #[test]
    fn test_validate_if_not_nil_block_two_args_errors() {
        let block = Expression::Block(Block {
            parameters: vec![
                BlockParameter::new("a", Span::new(1, 2)),
                BlockParameter::new("b", Span::new(3, 4)),
            ],
            body: vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                Span::new(7, 8),
            ))],
            span: Span::new(0, 9),
        });
        let result = validate_if_not_nil_block(&block, "ifNotNil:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityMismatch { selector, arity }) = result {
            assert_eq!(selector, "ifNotNil:");
            assert_eq!(arity, 2);
        } else {
            panic!("Expected BlockArityMismatch error");
        }
    }

    #[test]
    fn test_validate_if_not_nil_block_non_literal() {
        // Non-block expression (variable) — assumes 1 arg
        let expr = Expression::Identifier(crate::ast::Identifier::new("myBlock", Span::new(0, 7)));
        assert!(validate_if_not_nil_block(&expr, "ifNotNil:").unwrap());
    }

    // BT-493: Tests for validate_block_arity_exact

    fn make_block(arity: usize) -> Expression {
        let params: Vec<BlockParameter> = (0..arity)
            .map(|i| {
                #[allow(clippy::cast_possible_truncation)]
                // synthetic span positions for generated block params
                let span = Span::new((i * 2 + 1) as u32, (i * 2 + 2) as u32);
                BlockParameter::new(format!("p{i}"), span)
            })
            .collect();
        Expression::Block(Block {
            parameters: params,
            body: vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                Span::new(1, 2),
            ))],
            span: Span::new(0, 10),
        })
    }

    #[test]
    fn test_validate_exact_correct_arity() {
        assert!(validate_block_arity_exact(&make_block(0), 0, "timesRepeat:", "hint").is_ok());
        assert!(validate_block_arity_exact(&make_block(1), 1, "to:do:", "hint").is_ok());
        assert!(validate_block_arity_exact(&make_block(2), 2, "inject:into:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_wrong_arity() {
        let result = validate_block_arity_exact(&make_block(1), 0, "timesRepeat:", "use to:do:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError {
            selector,
            expected,
            actual,
            hint,
        }) = result
        {
            assert_eq!(selector, "timesRepeat:");
            assert_eq!(expected, "0");
            assert_eq!(actual, 1);
            assert!(hint.contains("to:do:"));
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_exact_non_literal_passes() {
        // Non-block expressions can't be checked at compile time — always pass
        let expr = Expression::Identifier(crate::ast::Identifier::new("myBlock", Span::new(0, 7)));
        assert!(validate_block_arity_exact(&expr, 0, "timesRepeat:", "hint").is_ok());
        assert!(validate_block_arity_exact(&expr, 1, "do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_times_repeat_zero_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(0), 0, "timesRepeat:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_times_repeat_one_arg_error() {
        let result = validate_block_arity_exact(&make_block(1), 0, "timesRepeat:", "hint");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_exact_to_do_one_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(1), 1, "to:do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_to_do_zero_arg_error() {
        let result = validate_block_arity_exact(&make_block(0), 1, "to:do:", "hint");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_exact_inject_into_two_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(2), 2, "inject:into:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_inject_into_one_arg_error() {
        let result = validate_block_arity_exact(&make_block(1), 2, "inject:into:", "hint");
        assert!(result.is_err());
    }

    // BT-493: Tests for validate_block_arity_range

    #[test]
    fn test_validate_range_within_bounds() {
        assert!(validate_block_arity_range(&make_block(0), 0, 1, "on:do:", "hint").is_ok());
        assert!(validate_block_arity_range(&make_block(1), 0, 1, "on:do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_range_out_of_bounds() {
        let result = validate_block_arity_range(&make_block(2), 0, 1, "on:do:", "hint");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError {
            selector,
            expected,
            actual,
            ..
        }) = result
        {
            assert_eq!(selector, "on:do:");
            assert_eq!(expected, "0 or 1");
            assert_eq!(actual, 2);
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_range_non_literal_passes() {
        let expr = Expression::Identifier(crate::ast::Identifier::new("myBlock", Span::new(0, 7)));
        assert!(validate_block_arity_range(&expr, 0, 1, "on:do:", "hint").is_ok());
    }

    // BT-493: Tests for validate_on_do_handler

    #[test]
    fn test_validate_on_do_handler_zero_args() {
        // 0-arg handler: valid, returns false (don't pass exception)
        assert!(!validate_on_do_handler(&make_block(0), "on:do:").unwrap());
    }

    #[test]
    fn test_validate_on_do_handler_one_arg() {
        // 1-arg handler: valid, returns true (pass exception)
        assert!(validate_on_do_handler(&make_block(1), "on:do:").unwrap());
    }

    #[test]
    fn test_validate_on_do_handler_two_args_errors() {
        let result = validate_on_do_handler(&make_block(2), "on:do:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError { actual, .. }) = result {
            assert_eq!(actual, 2);
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_on_do_handler_non_literal() {
        // Non-block expression — assumes 1 arg (pass exception)
        let expr = Expression::Identifier(crate::ast::Identifier::new("myBlock", Span::new(0, 7)));
        assert!(validate_on_do_handler(&expr, "on:do:").unwrap());
    }
}
