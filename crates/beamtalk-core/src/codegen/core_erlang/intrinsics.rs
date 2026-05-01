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

use super::document::{Document, join};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression, MessageSelector, WellKnownSelector};
use crate::docvec;

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
            Document::String(block_var.to_string()),
            " (",
            Document::String(recv_var.to_string()),
            ")"
        ]
    } else {
        docvec!["apply ", Document::String(block_var.to_string()), " ()"]
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
        if let Expression::Identifier(id) = receiver {
            if self.tier2_block_params.contains(id.name.as_str()) {
                let doc = self.generate_block_value_call_stateful(receiver, arguments)?;
                return Ok(Some(doc));
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
        let doc = if matches!(receiver, Expression::Block { .. }) {
            self.generate_block_value_call(receiver, &[])?
        } else {
            // BT-1942: Hoist open-scope receiver (e.g. class method self-send).
            let (preamble, mut docs) = self.capture_subexpr_sequence(&[receiver], "ValRecv")?;
            let recv_doc = docs.remove(0);
            let recv_var = self.fresh_temp_var("ValRecv");
            let call_doc = docvec![
                "let ",
                Document::String(recv_var.clone()),
                " = ",
                recv_doc,
                " in case call 'erlang':'is_function'(",
                Document::String(recv_var.clone()),
                ") of 'true' when 'true' -> apply ",
                Document::String(recv_var.clone()),
                " () 'false' when 'true' -> call 'beamtalk_primitive':'send'(",
                Document::String(recv_var),
                ", 'value', []) end",
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

            _ => Ok(None),
        }
    }

    /// Generates code for keyword `value:` variants on a block receiver.
    fn try_generate_block_value_keyword(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
        selector_name: &str,
    ) -> Result<Option<Document<'static>>> {
        // BT-851: Check if receiver is a Tier 2 block parameter
        if let Expression::Identifier(id) = receiver {
            if self.tier2_block_params.contains(id.name.as_str()) {
                let doc = self.generate_block_value_call_stateful(receiver, arguments)?;
                return Ok(Some(doc));
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
            // BT-1329: Also check for nested list ops with cross-scope mutations
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
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
            // BT-1329: Also check for nested list ops with cross-scope mutations
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
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
            // BT-1329: Also check for nested list ops with cross-scope mutations
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
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
            Document::String(fun_var.clone()),
            " = ",
            recv_code,
            " in ",
        ]);
        let arg_docs = self.generate_cascade_args(arguments, &mut parts)?;

        let args_doc = join(arg_docs, &Document::Str(", "));

        parts.push(docvec![
            "apply ",
            Document::String(fun_var),
            " (",
            args_doc,
            ")",
        ]);
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
        if let Expression::MessageSend {
            receiver: inner_receiver,
            selector: MessageSelector::Unary(module_name),
            ..
        } = expr
        {
            if let Expression::ClassReference { name, .. } = inner_receiver.as_ref() {
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
    /// This mirrors the runtime guard emitted for the unary `value` case (BT-335).
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
        let (recv_preamble, mut recv_docs) =
            self.capture_subexpr_sequence(&[receiver], "ValRecv")?;
        let recv_doc = recv_docs.remove(0);
        if !matches!(recv_preamble, Document::Nil) {
            any_open_scope = true;
            parts.push(recv_preamble);
        }
        let recv_var = self.fresh_temp_var("ValRecv");
        parts.push(docvec![
            "let ",
            Document::String(recv_var.clone()),
            " = ",
            recv_doc,
            " in ",
        ]);

        // BT-1270: Hoist field-assignment arguments before their _ValArgN bindings so
        // the StateN binding is in scope after the let-chain, not nested inside it.
        for arg in arguments {
            let arg_var = self.fresh_temp_var("ValArg");
            if Self::is_field_assignment(arg) {
                let (doc, val_var) = self.generate_field_assignment_open(arg)?;
                parts.push(doc);
                parts.push(docvec![
                    "let ",
                    Document::String(arg_var.clone()),
                    " = ",
                    Document::String(val_var),
                    " in ",
                ]);
            } else {
                // BT-1942: Hoist open-scope arg (e.g. class method self-send).
                let (arg_preamble, mut arg_docs) =
                    self.capture_subexpr_sequence(&[arg], "ValArg")?;
                let arg_code = arg_docs.remove(0);
                if !matches!(arg_preamble, Document::Nil) {
                    any_open_scope = true;
                    parts.push(arg_preamble);
                }
                parts.push(docvec![
                    "let ",
                    Document::String(arg_var.clone()),
                    " = ",
                    arg_code,
                    " in ",
                ]);
            }
            arg_vars.push(arg_var);
        }

        let arg_var_docs: Vec<Document<'static>> = arg_vars
            .iter()
            .map(|v| Document::String(v.clone()))
            .collect();
        let apply_args = join(arg_var_docs.clone(), &Document::Str(", "));

        let send_list = docvec!["[", join(arg_var_docs, &Document::Str(", ")), "]"];

        let case_doc = docvec![
            "case call 'erlang':'is_function'(",
            Document::String(recv_var.clone()),
            ") of 'true' when 'true' -> apply ",
            Document::String(recv_var.clone()),
            " (",
            apply_args,
            ") 'false' when 'true' -> call 'beamtalk_primitive':'send'(",
            Document::String(recv_var),
            ", '",
            Document::String(selector_name.to_string()),
            "', ",
            send_list,
            ") end",
        ];

        // BT-1942: If any sub-expression produced an open scope from a class
        // method self-send, wrap the case result and propagate the open scope
        // upward so the enclosing context can see the advanced ClassVarsN.
        if any_open_scope {
            let result_var = self.fresh_temp_var("ValRes");
            parts.push(docvec![
                "let ",
                Document::String(result_var.clone()),
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
        arg_docs.push(Document::String(current_state));
        let args_doc = join(arg_docs, &Document::Str(", "));

        let doc = docvec![
            "let ",
            Document::String(fun_var.clone()),
            " = ",
            recv_code,
            " in apply ",
            Document::String(fun_var),
            " (",
            args_doc,
            ")",
        ];
        Ok(doc)
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
                    Document::String(arg_var.clone()),
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
                                    Document::String(core_var.clone()),
                                    " = ",
                                    val_doc,
                                    " in ",
                                    Document::String(core_var.clone()),
                                ]);
                            } else {
                                parts.push(docvec![
                                    "let ",
                                    Document::String(core_var.clone()),
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
                    parts.push(docvec!["let ", Document::String(tmp), " = ", doc, " in ",]);
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
        parts.push(docvec![
            "let StateAcc = ",
            Document::String(outer_state),
            " in ",
        ]);
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
                    Document::String(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in case ",
                    Document::String(recv_var),
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
                    Document::String(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in case ",
                    Document::String(recv_var),
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
                    Document::String(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in let ",
                    Document::String(block_var.clone()),
                    " = ",
                    block_doc,
                    " in case ",
                    Document::String(recv_var.clone()),
                    " of <'nil'> when 'true' -> apply ",
                    Document::String(block_var),
                    " () <_> when 'true' -> ",
                    Document::String(recv_var),
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
                        let needs_threading = self.needs_mutation_threading(&analysis)
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
                    Document::String(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in let ",
                    Document::String(block_var),
                    " = ",
                    block_doc,
                    " in case ",
                    Document::String(recv_var),
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
                    Document::String(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in let ",
                    Document::String(nil_var.clone()),
                    " = ",
                    nil_doc,
                    " in let ",
                    Document::String(not_nil_var),
                    " = ",
                    not_nil_doc,
                    " in case ",
                    Document::String(recv_var),
                    " of <'nil'> when 'true' -> apply ",
                    Document::String(nil_var),
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
                    Document::String(recv_var.clone()),
                    " = ",
                    recv_doc,
                    " in let ",
                    Document::String(not_nil_var.clone()),
                    " = ",
                    not_nil_doc,
                    " in let ",
                    Document::String(nil_var.clone()),
                    " = ",
                    nil_doc,
                    " in case ",
                    Document::String(recv_var),
                    " of <'nil'> when 'true' -> apply ",
                    Document::String(nil_var),
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
            Document::String(recv_var.clone()),
            " = ",
            recv_doc,
            " in let ",
            Document::String(msg_var.clone()),
            " = ",
            msg_doc,
            " in let ",
            Document::String(class_var.clone()),
            " = call 'beamtalk_primitive':'class_of'(",
            Document::String(recv_var),
            ") in let ",
            Document::String(err0.clone()),
            " = call 'beamtalk_error':'new'('user_error', ",
            Document::String(class_var),
            ") in let ",
            Document::String(err1.clone()),
            " = call 'beamtalk_error':'with_message'(",
            Document::String(err0),
            ", ",
            Document::String(msg_var),
            ") in call 'beamtalk_error':'raise'(",
            Document::String(err1),
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
                Document::String(recv_var.clone()),
                " = ",
                recv_doc,
                " in call 'erlang':'phash2'(",
                Document::String(recv_var),
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
                        Document::String(self.current_state_var()),
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
                Document::String(receiver_var.clone()),
                " = ",
                recv_doc,
                " in case call 'erlang':'is_map'(",
                Document::String(receiver_var.clone()),
                ") of <'true'> when 'true' -> call 'beamtalk_primitive':'send'(",
                Document::String(receiver_var.clone()),
                ", 'fieldNames', []) <_> when 'true' -> let ",
                Document::String(pid_var.clone()),
                " = call 'erlang':'element'(4, ",
                Document::String(receiver_var),
                ") in call 'beamtalk_actor':'sync_send'(",
                Document::String(pid_var),
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
                        Document::String(receiver_var.clone()),
                        " = ",
                        recv_doc,
                        " in let ",
                        Document::String(selector_var.clone()),
                        " = ",
                        sel_doc,
                        " in call 'beamtalk_primitive':'responds_to'(",
                        Document::String(receiver_var),
                        ", ",
                        Document::String(selector_var),
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
                                    Document::String(name_var.clone()),
                                    " = ",
                                    name_code,
                                    " in call 'beamtalk_primitive':'send'(",
                                    Document::String(self.current_state_var()),
                                    ", 'fieldAt:', [",
                                    Document::String(name_var),
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
                        let hint =
                            Self::binary_string_literal("Value types have no instance variables");

                        // BT-924: The non-actor branch is split:
                        //   - map receiver → delegate to beamtalk_primitive:send (routes
                        //     through dispatch/3 which respects ClassKind::Value vs Object)
                        //   - other (primitive literal) → raise immutable_value
                        let call_doc = docvec![
                            "let ",
                            Document::String(receiver_var.clone()),
                            " = ",
                            recv_doc,
                            " in let ",
                            Document::String(name_var.clone()),
                            " = ",
                            name_doc,
                            " in case case call 'erlang':'is_tuple'(",
                            Document::String(receiver_var.clone()),
                            ") of <'true'> when 'true' -> case call 'erlang':'=='(call 'erlang':'tuple_size'(",
                            Document::String(receiver_var.clone()),
                            "), 4) of <'true'> when 'true' -> call 'erlang':'=='(call 'erlang':'element'(1, ",
                            Document::String(receiver_var.clone()),
                            "), 'beamtalk_object') <_> when 'true' -> 'false' end <_> when 'true' -> 'false' end of ",
                            "<'true'> when 'true' -> let ",
                            Document::String(pid_var.clone()),
                            " = call 'erlang':'element'(4, ",
                            Document::String(receiver_var.clone()),
                            ") in call 'beamtalk_actor':'sync_send'(",
                            Document::String(pid_var),
                            ", 'fieldAt:', [",
                            Document::String(name_var.clone()),
                            "])",
                            // Non-actor: delegate map receivers to beamtalk_primitive:send
                            // which routes through dispatch/3 (respects ClassKind)
                            " <_> when 'true' -> case call 'erlang':'is_map'(",
                            Document::String(receiver_var.clone()),
                            ") of <'true'> when 'true' -> call 'beamtalk_primitive':'send'(",
                            Document::String(receiver_var.clone()),
                            ", 'fieldAt:', [",
                            Document::String(name_var),
                            "]) <_> when 'true' -> let ",
                            Document::String(class_var.clone()),
                            " = call 'beamtalk_primitive':'class_of'(",
                            Document::String(receiver_var),
                            ") in let ",
                            Document::String(error_base.clone()),
                            " = call 'beamtalk_error':'new'('immutable_value', ",
                            Document::String(class_var),
                            ") in let ",
                            Document::String(error_sel.clone()),
                            " = call 'beamtalk_error':'with_selector'(",
                            Document::String(error_base),
                            ", 'fieldAt:') in let ",
                            Document::String(error_hint.clone()),
                            " = call 'beamtalk_error':'with_hint'(",
                            Document::String(error_sel),
                            ", ",
                            Document::String(hint),
                            ") in call 'beamtalk_error':'raise'(",
                            Document::String(error_hint),
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
                                    Document::String(name_var.clone()),
                                    " = ",
                                    name_code,
                                    " in let ",
                                    Document::String(value_var.clone()),
                                    " = ",
                                    value_code,
                                    " in call 'beamtalk_primitive':'send'(",
                                    Document::String(self.current_state_var()),
                                    ", 'fieldAt:put:', [",
                                    Document::String(name_var),
                                    ", ",
                                    Document::String(value_var),
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
                        let hint = Self::binary_string_literal(
                            "Cannot modify slot on value type \u{2014} use withSlot: to create a new instance",
                        );

                        let call_doc = docvec![
                            "let ",
                            Document::String(receiver_var.clone()),
                            " = ",
                            recv_doc,
                            " in let ",
                            Document::String(name_var.clone()),
                            " = ",
                            name_doc,
                            " in let ",
                            Document::String(value_var.clone()),
                            " = ",
                            value_doc,
                            " in case case call 'erlang':'is_tuple'(",
                            Document::String(receiver_var.clone()),
                            ") of <'true'> when 'true' -> case call 'erlang':'=='(call 'erlang':'tuple_size'(",
                            Document::String(receiver_var.clone()),
                            "), 4) of <'true'> when 'true' -> call 'erlang':'=='(call 'erlang':'element'(1, ",
                            Document::String(receiver_var.clone()),
                            "), 'beamtalk_object') <_> when 'true' -> 'false' end <_> when 'true' -> 'false' end of ",
                            "<'true'> when 'true' -> let ",
                            Document::String(pid_var.clone()),
                            " = call 'erlang':'element'(4, ",
                            Document::String(receiver_var.clone()),
                            ") in call 'beamtalk_actor':'sync_send'(",
                            Document::String(pid_var),
                            ", 'fieldAt:put:', [",
                            Document::String(name_var),
                            ", ",
                            Document::String(value_var),
                            "])",
                            " <_> when 'true' -> let ",
                            Document::String(class_var.clone()),
                            " = call 'beamtalk_primitive':'class_of'(",
                            Document::String(receiver_var),
                            ") in let ",
                            Document::String(error_base.clone()),
                            " = call 'beamtalk_error':'new'('immutable_value', ",
                            Document::String(class_var),
                            ") in let ",
                            Document::String(error_sel.clone()),
                            " = call 'beamtalk_error':'with_selector'(",
                            Document::String(error_base),
                            ", 'fieldAt:put:') in let ",
                            Document::String(error_hint.clone()),
                            " = call 'beamtalk_error':'with_hint'(",
                            Document::String(error_sel),
                            ", ",
                            Document::String(hint),
                            ") in call 'beamtalk_error':'raise'(",
                            Document::String(error_hint),
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
                    let needs_threading = self.needs_mutation_threading(&analysis)
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
                    let needs_threading = self.needs_mutation_threading(&analysis)
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
                    let needs_threading = self.needs_mutation_threading(&true_analysis)
                        || self.needs_mutation_threading(&false_analysis)
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
    /// `Logger setLevel:` is NOT inlined — it continues through normal class dispatch
    /// to `beamtalk_logger.erl`.
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
                    // setLevel: and any other selector — fall through to normal dispatch
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
            "'beamtalk_class' => '",
            Document::String(ctx_class),
            "', ",
            "'beamtalk_selector' => '",
            Document::String(ctx_selector),
            "'",
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
                Document::String(merge_var.clone()),
                " = call 'maps':'merge'(",
                user_meta_doc,
                ", ",
                metadata_map_doc,
                ") in call 'logger':'log'('",
                Document::String(level.to_string()),
                "', ",
                msg_doc,
                ", ",
                Document::String(merge_var),
                ")"
            ]
        } else {
            // Without user metadata: use compiler-injected map directly
            docvec![
                "call 'logger':'log'('",
                Document::String(level.to_string()),
                "', ",
                msg_doc,
                ", ",
                metadata_map_doc,
                ")"
            ]
        };

        let call_doc = docvec![
            "let ",
            Document::String(discard_var),
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
