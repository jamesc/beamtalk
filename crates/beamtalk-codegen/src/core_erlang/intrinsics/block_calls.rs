// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block and Tier 2 value-call intrinsics.
//!
//! Lowers block evaluation (`value`, `value:` …), loops (`timesRepeat:`,
//! `to:do:`), `perform:` on blocks, and the stateful (Tier 2) block value
//! calls, plus the list and dictionary message intrinsics that share their
//! arity validation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::{
    Block, CodeGenContext, CoreErlangGenerator, Document, Expression, MessageSelector, Result,
    STATEFUL_BLOCK_DISPATCH_HINT, WellKnownSelector, docvec, join, leaf,
    validate_block_arity_exact,
};

use beamtalk_cerl_doc::form::{self, Layout, let_in};

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
    ///   non-mutating falls through to pure-BT Integer method
    /// - `to:do:` (2 args) → arity validation + mutating-block state threading only
    /// - `to:by:do:` (3 args) → arity validation + mutating-block state threading only
    pub(in crate::core_erlang) fn try_generate_block_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // Block unary `value` is a well-known selector — dispatch via the enum.
        if matches!(selector.well_known(), Some(WellKnownSelector::Value)) {
            return self.try_generate_block_value_unary(receiver, selector, arguments);
        }

        // `repeat` is well-known; dispatch via the enum.
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

    /// BT-4510: whether a `value`/`value:`… send leaves the block intrinsic to
    /// the typed send path, because the receiver's trusted static type is a
    /// class other than `Block` that has a guarded direct route
    /// ([`Self::typed_send_target`]): `aResult value` is `Result>>value`, not
    /// block evaluation, and the intrinsic's `is_function` tests and dynamic
    /// fallback would send it through `beamtalk_message_dispatch:send/3`
    /// every time. A value that is a fun after all (a lying annotation)
    /// misses the route's guard, which rejects every fun, and takes the
    /// dynamic send, which evaluates the block as the intrinsic's fallback
    /// would — except a Tier 2 (arity-1, stateful) fun, which the intrinsic
    /// rejects with `stateful_block_dispatch` and the dynamic send rejects
    /// with its own error. Block literals and `Block`-typed receivers keep
    /// the intrinsic.
    ///
    /// BT-4630: a *protocol*-typed receiver never yields, decided on the
    /// static class being a protocol rather than on the target's shape: a
    /// protocol with one guardable conformer resolves to `Tagged`, yet a real
    /// `Block` may conform structurally and is never an arm, so yielding would
    /// forfeit the inline apply (and `stateful_block_dispatch` for Tier 2).
    fn block_value_yields_to_typed_send(
        &self,
        receiver: &Expression,
        selector: &MessageSelector,
    ) -> bool {
        if matches!(receiver, Expression::Block(_)) {
            return false;
        }
        let Ok(class) = self.trusted_static_class(receiver) else {
            return false;
        };
        class.as_str() != "Block"
            // A protocol-typed receiver may be a real block (`Block` is never
            // a tagged arm), whatever shape its target resolves to.
            && !self.is_protocol_class(class.as_str())
            && self.typed_send_target(receiver, selector).is_ok()
    }

    /// Whether `class` names a protocol in scope.
    fn is_protocol_class(&self, class: &str) -> bool {
        self.protocol_registry
            .as_ref()
            .is_some_and(|r| r.has_protocol(class))
    }

    /// Generates code for unary `value` message on a block receiver.
    ///
    /// When the receiver is a block literal, use fast inline apply.
    /// For other receivers, generate a runtime type check to handle both
    /// blocks (apply) and non-blocks (runtime dispatch via send).
    fn try_generate_block_value_unary(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // A bare class name as receiver (e.g. `Foo value`) is a
        // class-method send, not block application. Fall through so the
        // class-reference handler routes to `class_send` instead of generating
        // a runtime is_function guard that would call
        // `beamtalk_primitive:send(ClassObject, 'value', [])` and crash on a
        // non-matching `handle_call` clause. (Mirror of the keyword `value:`
        // bypass below.)
        if matches!(receiver, Expression::ClassReference { .. }) {
            return Ok(None);
        }
        // Check if receiver is a local var this method assigned a Tier 2
        // block literal to (zero-arg value).
        if let Expression::Identifier(id) = receiver {
            if self.tier2_local_vars.contains(id.name.as_str()) {
                let tuple_doc = self.generate_block_value_call_stateful(receiver, arguments)?;
                return Ok(Some(self.close_tier2_value_subexpr_doc(
                    receiver, selector, arguments, tuple_doc,
                )));
            }
        }
        if self.block_value_yields_to_typed_send(receiver, selector) {
            return Ok(None);
        }
        // BT-3881: a block literal of another arity is applied, never
        // inlined, so it raises `badarity` (in the REPL and in a method).
        if let Some(block) =
            beamtalk_core::semantic_analysis::block_facts::block_literal_of_other_arity(
                receiver, selector, 0,
            )
        {
            return Ok(Some(self.generate_block_arity_mismatch(block, arguments)?));
        }
        // Inline Tier 2 block literal with mutations: one that writes a
        // captured local, writes a field or sends to `self` (actor state
        // threading), or, in the REPL, one of this arity that writes a binding
        // anywhere, a nested construct included (BT-3823: inlined over the
        // bindings map as a REPL arm; `beamtalk_core`'s `repl_evaluated_block`).
        if let Expression::Block(block) = receiver
            && self.evaluated_block_inlines_with_mutations(block)
        {
            let doc = self.generate_block_value_inline_with_mutations(block, &[])?;
            return Ok(Some(doc));
        }
        let doc = if matches!(receiver, Expression::Block { .. }) {
            self.generate_block_value_call(receiver, &[])?
        } else {
            // BT-4596: the non-fun fallback below is a dynamic site.
            self.report_block_value_fallback(receiver, selector);
            // Hoist open-scope receiver (e.g. class method self-send).
            let mut seq = self.sequence_call(&[receiver], "ValRecv")?;
            let recv_doc = seq.next();
            let recv_var = self.fresh_temp_var("ValRecv");
            // arity-discriminate before applying — a Tier 2
            // (stateful) zero-arg block is an arity-1 fun whose `StateAcc`
            // this call site cannot supply; raise the clear
            // `stateful_block_dispatch` error instead of a raw `badarity`
            // (see `generate_value_keyword_guard`'s matching branch).
            let stateful_branch = self.generate_stateful_block_dispatch_error(
                "value",
                "Block",
                "Wrong argument count, or the block captures mutable state and must be invoked directly instead of via perform:/dynamic dispatch",
            );
            // BT-4356: each control-transferring node carries the send's line
            // (see `annotate_call_node_with_line`).
            let at = receiver.span();
            let apply_doc = self.annotate_call_node_with_line(
                docvec!["apply ", leaf::var(recv_var.clone()), " ()"],
                at,
            );
            // The same apply in the arity >= 2 branch (0 and 1 are ruled out
            // before it), where it always badarity-crashes — the existing
            // behaviour for wrong-arity plain functions, preserved
            // deliberately.
            let badarity_apply_doc = apply_doc.clone();
            let send_doc = self.annotate_call_node_with_line(
                form::call(
                    "beamtalk_message_dispatch",
                    "send",
                    [
                        leaf::var(recv_var.clone()),
                        leaf::atom("value"),
                        Document::Str("[]"),
                    ],
                ),
                at,
            );
            let call_doc = docvec![
                "let ",
                leaf::var(recv_var.clone()),
                " = ",
                recv_doc,
                " in case call 'erlang':'is_function'(",
                leaf::var(recv_var.clone()),
                ", 0) of 'true' when 'true' -> ",
                apply_doc,
                " 'false' when 'true' -> case call 'erlang':'is_function'(",
                leaf::var(recv_var.clone()),
                ", 1) of 'true' when 'true' -> ",
                stateful_branch,
                " 'false' when 'true' -> case call 'erlang':'is_function'(",
                leaf::var(recv_var),
                ") of 'true' when 'true' -> ",
                badarity_apply_doc,
                " 'false' when 'true' -> ",
                send_doc,
                " end end end",
            ];
            seq.close(self, call_doc, "ValRes")
        };
        Ok(Some(doc))
    }

    /// Generates code for keyword block messages (`value:`, `whileTrue:`, `timesRepeat:`, etc.).
    ///
    /// Well-known keyword selectors (`value:`/`value:value:`/`value:value:value:`,
    /// `on:do:`, `whileTrue:`/`whileFalse:`, `ensure:`) route through the
    /// `WellKnownSelector` enum so arity is checked structurally by the
    /// classifier. The remaining keyword
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
                return self.try_generate_block_value_keyword(
                    receiver,
                    selector,
                    arguments,
                    selector_name,
                );
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

            // valueWithArguments: (call-site-intercepted @intrinsic,
            // was previously a bare @primitive with no access to the calling
            // method's state — see stdlib/src/block.bt).
            "valueWithArguments:" if arguments.len() == 1 => self
                .try_generate_block_value_with_arguments_keyword(receiver, selector, &arguments[0]),

            _ => Ok(None),
        }
    }

    /// Generates code for `valueWithArguments:` on a block receiver.
    ///
    /// mirrors `try_generate_block_value_keyword`'s generic
    /// (non-Tier2) fallback path. Deliberately does NOT special-case
    /// `tier2_local_vars` receivers here —
    /// `generate_block_value_with_arguments_call_runtime_discriminated`
    /// always returns a raw `{Result, NewState}` tuple, and this function is
    /// reached from the generic, any-position `expression_doc` dispatch,
    /// which has nothing to unpack it. Those shapes are intercepted before
    /// reaching here by the top-level `Tier2ValueCall` classification in
    /// `blocks/tier2_value_calls.rs` (`is_tier2_value_call`/
    /// `generate_tier2_value_call_doc`), the only place that unpacks the
    /// tuple.
    fn try_generate_block_value_with_arguments_keyword(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        args_expr: &Expression,
    ) -> Result<Option<Document<'static>>> {
        // A bare class name as receiver is a class-method send, not
        // block application — fall through (mirrors `value:`'s bypass).
        if matches!(receiver, Expression::ClassReference { .. }) {
            return Ok(None);
        }
        // Compile-time Erlang FFI receiver → fall through.
        if Self::is_erlang_ffi_receiver(receiver) {
            return Ok(None);
        }
        // Fast path: block literal receiver never needs the runtime
        // is_function guard. Literal-block-with-mutations is out of scope
        //  — valueWithArguments: on a literal block is an
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
        // The non-fun fallback is a dynamic site (BT-4596).
        self.report_block_value_fallback(receiver, selector);
        let doc = self.generate_block_value_with_arguments_call(receiver, args_expr)?;
        Ok(Some(doc))
    }

    /// Generates code for keyword `value:` variants on a block receiver.
    fn try_generate_block_value_keyword(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        selector_name: &str,
    ) -> Result<Option<Document<'static>>> {
        // Check if receiver is a local var this method assigned a Tier 2
        // block literal to.
        if let Expression::Identifier(id) = receiver {
            if self.tier2_local_vars.contains(id.name.as_str()) {
                let tuple_doc = self.generate_block_value_call_stateful(receiver, arguments)?;
                return Ok(Some(self.close_tier2_value_subexpr_doc(
                    receiver, selector, arguments, tuple_doc,
                )));
            }
        }
        if self.block_value_yields_to_typed_send(receiver, selector) {
            return Ok(None);
        }
        // BT-3881: a block literal of another arity is applied, never
        // inlined, so it raises `badarity` (in the REPL and in a method).
        if let Some(block) =
            beamtalk_core::semantic_analysis::block_facts::block_literal_of_other_arity(
                receiver,
                selector,
                arguments.len(),
            )
        {
            return Ok(Some(self.generate_block_arity_mismatch(block, arguments)?));
        }
        // Inline Tier 2 block literal with mutations (keyword variant, the
        // same predicate as the unary one).
        if let Expression::Block(block) = receiver
            && self.evaluated_block_inlines_with_mutations(block)
        {
            let doc = self.generate_block_value_inline_with_mutations(block, arguments)?;
            return Ok(Some(doc));
        }
        // Fast path: block literal receiver -> fast inline apply
        if matches!(receiver, Expression::Block(_)) {
            let doc = self.generate_block_value_call(receiver, arguments)?;
            return Ok(Some(doc));
        }
        // Compile-time Erlang FFI receiver → fall through
        if Self::is_erlang_ffi_receiver(receiver) {
            return Ok(None);
        }
        // A bare class name as receiver (e.g. `Character value: 65`)
        // is a class-method send, not block application. Fall through so the
        // class-reference handler routes to `class_send` instead of generating
        // a runtime is_function guard that ends up calling
        // `beamtalk_primitive:send(ClassObject, 'value:', [65])` and hitting
        // the actor-style `sync_send` path with no matching handle_call clause.
        if matches!(receiver, Expression::ClassReference { .. }) {
            return Ok(None);
        }
        // Unknown receiver → runtime is_function guard with fallback, a
        // dynamic site (BT-4596).
        self.report_block_value_fallback(receiver, selector);
        let doc = self.generate_value_keyword_guard(receiver, arguments, selector_name)?;
        Ok(Some(doc))
    }

    /// Generates code for `timesRepeat:` with mutation threading.
    ///
    /// Only intercept for mutating blocks; non-mutating cases
    /// fall through to the pure-BT tail-recursive Integer method.
    fn try_generate_times_repeat(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        validate_block_arity_exact(
            &arguments[0],
            0,
            "timesRepeat:",
            "Fix: Use a zero-arg block. If you need the iteration index, use to:do: instead:\n\
             \x20 3 timesRepeat: [self doSomething]\n\
             \x20 1 to: 3 do: [:i | self doSomethingWith: i]",
        )?;
        if let Expression::Block(body_block) = &arguments[0] {
            // The one loop/fold gate (BT-3834): the body's mutations, or a
            // write to an outer local (including write-only `[last := i]`)
            // the mutation analysis misses, and only when the loop has
            // something to carry.
            if self.loop_body_has_carried_mutations(body_block) {
                let doc = self.generate_times_repeat_with_mutations(receiver, body_block)?;
                return Ok(Some(doc));
            }
        }
        Ok(None)
    }

    /// Generates code for `to:do:` with mutation threading.
    ///
    /// Only intercept for mutating blocks; non-mutating cases
    /// fall through to the pure-BT tail-recursive Integer method.
    fn try_generate_to_do(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        validate_block_arity_exact(
            &arguments[1],
            1,
            "to:do:",
            "Fix: The body block must take one argument (the iteration index):\n\
             \x20 1 to: 10 do: [:i | i printString]",
        )?;
        if let Expression::Block(body_block) = &arguments[1] {
            // The one loop/fold gate (see `try_generate_times_repeat`).
            if self.loop_body_has_carried_mutations(body_block) {
                let doc =
                    self.generate_to_do_with_mutations(receiver, &arguments[0], body_block)?;
                return Ok(Some(doc));
            }
        }
        Ok(None)
    }

    /// Generates code for `to:by:do:` with mutation threading.
    ///
    /// Only intercept for mutating blocks; non-mutating cases
    /// fall through to the pure-BT tail-recursive Integer method.
    fn try_generate_to_by_do(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        validate_block_arity_exact(
            &arguments[2],
            1,
            "to:by:do:",
            "Fix: The body block must take one argument (the iteration index):\n\
             \x20 1 to: 10 by: 2 do: [:i | i printString]",
        )?;
        if let Expression::Block(body_block) = &arguments[2] {
            // The one loop/fold gate (see `try_generate_times_repeat`).
            if self.loop_body_has_carried_mutations(body_block) {
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
    /// This intrinsic now checks the receiver type to avoid intercepting
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
    pub(in crate::core_erlang) fn try_generate_list_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // String receivers are no longer skipped here. The
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
                    // Medium-risk list selectors
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
                    // `eachWithIndex:`/`do:separatedBy:` are self-hosted in
                    // collection.bt. In an actor method that mutates state, desugar to a
                    // stateful `inject:into:` fold so the mutation threads; otherwise
                    // return `None` and let the ordinary dispatch reach the collection.bt
                    // method.
                    "eachWithIndex:" if arguments.len() == 1 => {
                        self.try_generate_each_with_index(receiver, selector, arguments)
                    }
                    "do:separatedBy:" if arguments.len() == 2 => {
                        self.try_generate_do_separated_by(receiver, selector, arguments)
                    }
                    _ => Ok(None),
                }
            }

            _ => Ok(None),
        }
    }

    /// Dictionary iteration intrinsics — `do:`, `doWithKey:`, `keysAndValuesDo:`.
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
    pub(in crate::core_erlang) fn try_generate_dict_message(
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
                        let doc = self.generate_dict_do_with_key(
                            receiver,
                            &arguments[0],
                            &selector_name,
                        )?;
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
    pub(in crate::core_erlang) fn generate_block_value_call(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let fun_var = self.fresh_temp_var("Fun");

        // Evaluate receiver first, then hoist field-assignment arguments.
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
        let mut hoisted = Vec::new();
        let arg_docs = self.generate_cascade_args(arguments, &mut hoisted)?;
        if !hoisted.is_empty() {
            parts.push(self.threaded_prelude_doc(&hoisted));
        }

        let args_doc = join(arg_docs, &Document::Str(", "));

        // BT-4356: the apply node itself carries the send's line.
        parts.push(self.annotate_call_node_with_line(
            docvec!["apply ", leaf::var(fun_var), " (", args_doc, ")",],
            receiver.span(),
        ));
        Ok(Document::Vec(parts))
    }

    /// BT-3823, BT-3881: a block literal sent a `value` selector of another
    /// arity, in the REPL or a method (`block_literal_of_other_arity`):
    /// its arguments are evaluated
    /// and applied to a stand-in `fun` of the block's own arity, so the send
    /// raises the same structured `badarity` error (`Wrong number of
    /// arguments: block expects N but was called with M`) as any other block
    /// called with the wrong number of arguments. The block itself is never
    /// compiled: its body cannot run.
    ///
    /// ```erlang
    /// let _Arity = fun (_P1, ..., _Pn) -> 'nil' in apply _Arity (<args>)
    /// ```
    fn generate_block_arity_mismatch(
        &mut self,
        block: &Block,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let fun_var = self.fresh_temp_var("Arity");
        let params: Vec<Document<'static>> = block
            .parameters
            .iter()
            .map(|_| leaf::var(self.fresh_temp_var("P")))
            .collect();
        let mut arg_docs = Vec::with_capacity(arguments.len());
        for arg in arguments {
            arg_docs.push(self.expression_doc(arg)?);
        }
        // BT-4356: the `badarity` is raised by the apply node, so it carries
        // the send's line.
        let apply_doc = self.annotate_call_node_with_line(
            docvec![
                "apply ",
                leaf::var(fun_var.clone()),
                " (",
                join(arg_docs, &Document::Str(", ")),
                ")",
            ],
            block.span,
        );
        Ok(docvec![
            "let ",
            leaf::var(fun_var),
            " = fun (",
            join(params, &Document::Str(", ")),
            ") -> 'nil' in ",
            apply_doc,
        ])
    }

    /// Returns true if `expr` is a compile-time Erlang FFI proxy expression.
    ///
    /// Matches `(Erlang module_name)` — i.e., a `MessageSend` whose inner receiver is
    /// `ClassReference("Erlang")` and whose selector is a unary module name.  These
    /// expressions should **not** be treated as blocks by the `value:` handler; instead
    /// they fall through to `try_handle_erlang_interop` (step 7 of dispatch).
    ///
    /// delegates to the single shared recognizer in
    /// [`beamtalk_core::ffi_receiver`] (class-protocol filter, package check, and
    /// parenthesized-receiver peeling all live there now).
    pub(super) fn is_erlang_ffi_receiver(expr: &Expression) -> bool {
        beamtalk_core::ffi_receiver::erlang_module_of_receiver(expr).is_some()
    }

    /// Generates a runtime `erlang:is_function/1` guard for keyword `value:` sends.
    ///
    /// When the receiver is not a compile-time-known block or Erlang FFI expression,
    /// emits a runtime check:
    ///
    /// ```erlang
    /// let _ValRecv = <recv> in
    /// let _ValArg0 = <arg0> in
    /// case call 'erlang':'is_function'(_ValRecv) of
    ///   'true' when 'true' -> apply _ValRecv (_ValArg0)
    ///   'false' when 'true' -> call 'beamtalk_message_dispatch':'send'(_ValRecv, 'value:', [_ValArg0])
    /// end
    /// ```
    ///
    /// ADR 0118 phase 5b: hoists a receiver operand (which may
    /// need a state prelude) binding it to a fresh
    /// `prefix`-named temp var. Appends the necessary `let`-binding(s) to
    /// `parts` in order — `parts` is always a self-contained sequence
    /// (each entry a `let ... in` prefix), so its caller needs no separate
    /// "did anything need hoisting" signal any more. Shared by
    /// `generate_value_keyword_guard` and
    /// `generate_block_value_with_arguments_call` — both hoist
    /// their receiver the same way, before any argument hoisting.
    fn thread_value_call_receiver(
        &mut self,
        receiver: &Expression,
        prefix: &str,
        parts: &mut Vec<Document<'static>>,
    ) -> Result<String> {
        let mut seq = self.sequence_call(&[receiver], prefix)?;
        let code = seq.next();
        let prelude = seq.into_prelude();
        if !prelude.is_empty() {
            parts.push(self.threaded_prelude_doc(&prelude));
        }
        let var = self.fresh_temp_var(prefix);
        parts.push(let_in(
            leaf::var(var.clone()),
            code,
            Document::Nil,
            Layout::Flat,
        ));
        Ok(var)
    }

    /// ADR 0118 phase 5b: hoists an argument-position
    /// operand, special-casing a field-assignment argument (`self.field :=
    /// x`) so its `StateN` binding lands outside the let-chain rather than
    /// nested inside it. Appends the necessary `let`-binding(s) to `parts`
    /// in order. Shared by `generate_value_keyword_guard` and
    /// `generate_block_value_with_arguments_call`.
    fn thread_value_call_argument(
        &mut self,
        arg: &Expression,
        prefix: &str,
        parts: &mut Vec<Document<'static>>,
    ) -> Result<String> {
        let var = self.fresh_temp_var(prefix);
        if self.is_field_assignment(arg) {
            let (doc, val_var) = self.generate_field_assignment_open(arg)?;
            parts.push(doc);
            parts.push(let_in(
                leaf::var(var.clone()),
                leaf::var(val_var),
                Document::Nil,
                Layout::Flat,
            ));
        } else {
            let mut seq = self.sequence_call(&[arg], prefix)?;
            let code = seq.next();
            let prelude = seq.into_prelude();
            if !prelude.is_empty() {
                parts.push(self.threaded_prelude_doc(&prelude));
            }
            parts.push(let_in(
                leaf::var(var.clone()),
                code,
                Document::Nil,
                Layout::Flat,
            ));
        }
        Ok(var)
    }

    /// This mirrors the runtime guard emitted for the unary `value` case.
    ///
    /// the function branch discriminates arity before applying. A
    /// Tier 2 (stateful, ADR-0041) block compiles to an (N+1)-arg fun expecting
    /// a live `StateAcc` this statically-unknown call site cannot supply — it
    /// reaches here when a stateful block flows through generic dispatch into a
    /// self-hosted method's block parameter (e.g. `perform: #eachWithIndex:`,
    /// whose Collection body invokes `block value:value:`). Applying it N-ary
    /// used to crash with a raw `badarity`; now it raises the same clear
    /// `stateful_block_dispatch` error other stateful-block guards establish. Any other
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

        // Hoist the receiver (e.g. class method self-send) inline so
        // its binding remains visible to subsequent arg bindings.
        // Each sub-expression is bound sequentially, so per-sub-expression inline
        // hoisting preserves left-to-right evaluation order.
        let recv_var = self.thread_value_call_receiver(receiver, "ValRecv", &mut parts)?;

        // Hoist field-assignment arguments before their _ValArgN bindings so
        // the StateN binding is in scope after the let-chain, not nested inside it.
        for arg in arguments {
            let arg_var = self.thread_value_call_argument(arg, "ValArg", &mut parts)?;
            arg_vars.push(arg_var);
        }

        let arg_var_docs: Vec<Document<'static>> =
            arg_vars.iter().map(|v| leaf::var(v.clone())).collect();
        let apply_args = join(arg_var_docs.clone(), &Document::Str(", "));

        let send_list = docvec!["[", join(arg_var_docs, &Document::Str(", ")), "]"];

        // same hedged hint as `generate_stateful_block_guard` —
        // `is_function/2` can't distinguish a genuine Tier 2 block from a pure
        // fun called with one argument too few, and the block can arrive here
        // through any dynamic dispatch, not just `perform:`.
        let stateful_branch = self.generate_stateful_block_dispatch_error(
            selector_name,
            "Block",
            "Wrong argument count, or the block captures mutable state and must be invoked directly instead of via perform:/dynamic dispatch",
        );
        let arity = arguments.len();

        // BT-4356: each control-transferring node carries the send's line
        // (see `annotate_call_node_with_line`). The arity-matched apply and
        // the fall-through (badarity) apply are the same node.
        let at = receiver.span();
        let apply_doc = self.annotate_call_node_with_line(
            docvec!["apply ", leaf::var(recv_var.clone()), " (", apply_args, ")"],
            at,
        );
        let send_doc = self.annotate_call_node_with_line(
            form::call(
                "beamtalk_message_dispatch",
                "send",
                [
                    leaf::var(recv_var.clone()),
                    leaf::atom(selector_name.to_string()),
                    send_list,
                ],
            ),
            at,
        );

        let case_doc = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(recv_var.clone()),
            ", ",
            leaf::int_lit(i64::try_from(arity).unwrap_or(i64::MAX)),
            ") of 'true' when 'true' -> ",
            apply_doc.clone(),
            " 'false' when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(recv_var.clone()),
            ", ",
            leaf::int_lit(i64::try_from(arity.saturating_add(1)).unwrap_or(i64::MAX)),
            ") of 'true' when 'true' -> ",
            stateful_branch,
            " 'false' when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(recv_var),
            ") of 'true' when 'true' -> ",
            apply_doc,
            " 'false' when 'true' -> ",
            send_doc,
            " end end end",
        ];

        // ADR 0118 phase 5b: `parts` is already a self-contained
        // `let ... in` prefix sequence regardless of whether any receiver/
        // argument needed a prelude — append the case as the final value.
        parts.push(case_doc);

        Ok(Document::Vec(parts))
    }

    /// Closes a Tier 2 value call's raw `{Result, NewState}` tuple
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
    /// caller (which crashed with badarith/badarity). A Tier 2
    /// value call used as a bare STATEMENT (not nested in another
    /// expression) is unaffected: it's classified as
    /// `BodyExprKind::Tier2ValueCall` and still gets full state threading via
    /// `generate_tier2_value_call_doc` at the top-level method-body/
    /// conditional-branch/loop-body sequencers — this fallback only applies
    /// where a plain value, not a state-threading statement, is expected.
    fn close_tier2_value_subexpr_doc(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        tuple_doc: Document<'static>,
    ) -> Document<'static> {
        // BT-4122: an operand Tier 2 call is noted like any other
        // user-code send — a no-op inside its own window (which every
        // operand Tier 2 call has), the unbracketed gap under the
        // no-brackets fault, where nothing wraps it.
        self.note_send(receiver, selector, arguments, false, receiver.span());
        // A may-write call merges into its bracket: hand the raw tuple
        // through (the `synced_call_value` wrap binds element 1 as the
        // value, reloads the merged state the fun's exit stored, and
        // rebinds the call's locals). Anything else keeps the legacy
        // drop — a locals-only call, a call in a cell closure (no
        // bracket wraps there; the fun's exit store still persists its
        // field writes through the cell), or the no-brackets fault.
        if let Expression::Identifier(id) = receiver
            && self.tier2_operand_merges(id.name.as_str())
        {
            return tuple_doc;
        }
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

    /// Generates a Tier 2 stateful block value call (ADR 0041 Phase 0).
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
    pub(in crate::core_erlang) fn generate_block_value_call_stateful(
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

        // BT-4356: the apply node itself carries the send's line.
        let apply_doc = self.annotate_call_node_with_line(
            docvec!["apply ", leaf::var(fun_var.clone()), " (", args_doc, ")"],
            receiver.span(),
        );
        let doc = docvec![
            "let ",
            leaf::var(fun_var),
            " = ",
            recv_code,
            " in ",
            apply_doc
        ];
        Ok(doc)
    }

    /// Generates the fallback method body for `blockValue`/`blockValue1`/
    /// `blockValue2`/`blockValue3` — Block's `value`/`value:`/`value:value:`/
    /// `value:value:value:`. This body is reached only when something bypasses the
    /// call-site interception these selectors normally get (e.g. `perform:`/
    /// `perform:withArguments:`), the same gap `blockValueWithArguments`'s fallback
    /// (`generate_primitive`, `mod.rs`) closed for real — but unlike
    /// that selector, these can't unconditionally `erlang:apply`: under ADR-0041's
    /// universal state-threading protocol, a Tier 2 (stateful) block compiles to
    /// `fun(Args..., StateAcc) -> {Result, NewStateAcc}`, and invoking it correctly
    /// requires a live `StateAcc` that only a call-site-aware caller has (see this
    /// file's `generate_block_value_with_arguments_call_runtime_discriminated`, which
    /// uses the same `erlang:is_function/2` arity-discrimination idiom). Generic
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
    /// Adversarial review: the arity ambiguity above cuts both ways.
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
    /// version of that here.
    pub(in crate::core_erlang) fn generate_block_value_structural_fallback(
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
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
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
    pub(in crate::core_erlang) fn generate_stateful_block_dispatch_error(
        &mut self,
        real_selector: &str,
        class_name: &str,
        hint_text: &str,
    ) -> Document<'static> {
        let error_base = self.fresh_temp_var("Err");
        let error_sel = self.fresh_temp_var("Err");
        let error_hint = self.fresh_temp_var("Err");
        docvec![
            super::super::errors::beamtalk_error_doc(
                leaf::var(error_base.clone()),
                leaf::var(error_base),
                leaf::var(error_sel.clone()),
                leaf::var(error_sel),
                leaf::var(error_hint.clone()),
                "stateful_block_dispatch",
                leaf::atom(class_name.to_string()),
                leaf::atom(real_selector.to_string()),
                leaf::binary_lit(hint_text),
                Document::Str(" "),
            ),
            "call 'beamtalk_error':'raise'(",
            leaf::var(error_hint),
            ")",
        ]
    }

    /// Guards a List/Collection iteration primitive's block argument
    /// (`do:`/`collect:`/`select:`/`reject:`/`inject:into:`) against being a
    /// Tier 2 (stateful) block.
    ///
    /// Unlike Block's `value*` structural intrinsics, these
    /// selectors' compiled method bodies are real, already-correct BIF
    /// lowerings (`tier1_doc`, e.g. `lists:map/2`, `beamtalk_list:do/2`) — a
    /// Tier 1 (pure) block reached via generic dispatch (`perform:`) already
    /// works. But that BIF body applies the block with a plain N-arg
    /// `erlang:apply`; a Tier 2 block compiles to an (N+1)-arg fun expecting a
    /// live `StateAcc` (ADR-0041) that generic dispatch has no way to supply,
    /// which today hits a raw Erlang arity crash (confirmed empirically)
    /// instead of a clear diagnostic. This wraps the existing correct Tier 1
    /// body with the same `stateful_block_dispatch` error already
    /// established, rather than replacing a placeholder.
    ///
    /// Selector-keyed, not class-keyed: applies wherever a class declares one
    /// of these exact selectors as a bare-inferred `@primitive` with a real
    /// BIF lowering (confirmed to include List, Array, Binary, Dictionary,
    /// Set, Tuple, Collection, and String's `collect:`/`select:`/`reject:`).
    ///
    /// The same arity-only ambiguity documented for `blockValue*`
    /// (`generate_block_value_structural_fallback`): `erlang:is_function/2`
    /// can't distinguish a genuinely stateful block from a *pure* block
    /// simply called with one fewer argument than the selector expects (both
    /// present as arity `pure_arity + 1`) — hence the hedged hint text rather
    /// than asserting the block is stateful.
    pub(in crate::core_erlang) fn generate_stateful_block_guard(
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

    /// Generates a runtime `erlang:is_function/1` guard for
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
    ///   'false' when 'true' -> call 'beamtalk_message_dispatch':'send'(_ValRecv, 'valueWithArguments:', [_ValArgs])
    /// end
    /// ```
    fn generate_block_value_with_arguments_call(
        &mut self,
        receiver: &Expression,
        args_expr: &Expression,
    ) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(4);

        let recv_var = self.thread_value_call_receiver(receiver, "ValRecv", &mut parts)?;
        let args_var = self.thread_value_call_argument(args_expr, "ValArgs", &mut parts)?;

        // BT-4356: each control-transferring node carries the send's line.
        let at = receiver.span();
        let apply_doc = self.annotate_call_node_with_line(
            form::call(
                "erlang",
                "apply",
                [leaf::var(recv_var.clone()), leaf::var(args_var.clone())],
            ),
            at,
        );
        let send_doc = self.annotate_call_node_with_line(
            form::call(
                "beamtalk_message_dispatch",
                "send",
                [
                    leaf::var(recv_var.clone()),
                    leaf::atom("valueWithArguments:"),
                    docvec!["[", leaf::var(args_var), "]"],
                ],
            ),
            at,
        );
        let case_doc = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(recv_var),
            ") of 'true' when 'true' -> ",
            apply_doc,
            " 'false' when 'true' -> ",
            send_doc,
            " end",
        ];

        parts.push(case_doc);

        Ok(Document::Vec(parts))
    }

    /// The runtime Tier 1/Tier 2 discriminated `valueWithArguments:` call
    /// (the argument count is a runtime list length rather than a
    /// compile-time-known static arity). Reached only from
    /// `blocks/tier2_value_calls.rs`'s `generate_tier2_value_call_doc` (the tuple
    /// unpacker), never from generic `expression_doc` dispatch. Always
    /// returns a raw `{Result, NewState}` tuple:
    /// - Tier 1 (`is_function(Fun, length(Args))`): synthesizes
    ///   `{erlang:apply(Fun, Args), State}` — state unchanged.
    /// - Tier 2 (`is_function(Fun, length(Args) + 1)`): appends the calling
    ///   method's live state as the trailing element of `Args` and returns
    ///   the block's own `{Result, NewState}` tuple directly (Tier 2 funs
    ///   already return this shape).
    /// - Non-function receiver: falls back to `beamtalk_message_dispatch:send/3`
    ///   (`beamtalk_primitive:send/3` is wrong here: the receiver may be a
    ///   live actor), wrapped as `{SendResult, State}`.
    pub(in crate::core_erlang) fn generate_block_value_with_arguments_call_runtime_discriminated(
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

        // BT-4356: each control-transferring node carries the send's line.
        let at = receiver.span();
        let tier1_apply = self.annotate_call_node_with_line(
            form::call(
                "erlang",
                "apply",
                [leaf::var(fun_var.clone()), leaf::var(args_var.clone())],
            ),
            at,
        );
        let tier2_apply = self.annotate_call_node_with_line(
            form::call(
                "erlang",
                "apply",
                [
                    leaf::var(fun_var.clone()),
                    form::call(
                        "erlang",
                        "++",
                        [
                            leaf::var(args_var.clone()),
                            docvec!["[", leaf::var(current_state.clone()), "]"],
                        ],
                    ),
                ],
            ),
            at,
        );
        let send_doc = self.annotate_call_node_with_line(
            form::call(
                "beamtalk_message_dispatch",
                "send",
                [
                    leaf::var(fun_var.clone()),
                    leaf::atom("valueWithArguments:"),
                    docvec!["[", leaf::var(args_var), "]"],
                ],
            ),
            at,
        );
        let case_doc = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(fun_var.clone()),
            ", ",
            leaf::var(tier1_arity_var),
            ") of 'true' when 'true' -> {",
            tier1_apply,
            ", ",
            leaf::var(current_state.clone()),
            "} 'false' when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(fun_var),
            ", ",
            leaf::var(tier2_arity_var),
            ") of 'true' when 'true' -> ",
            tier2_apply,
            " 'false' when 'true' -> {",
            send_doc,
            ", ",
            leaf::var(current_state),
            "} end end",
        ];

        Ok(docvec![preamble, case_doc])
    }

    /// The `let <param> = <argument> in` bindings of an inlined block's
    /// parameters, in order, each bound in the current scope.
    ///
    /// ADR 0137: an argument that is a call that may run user code is
    /// bracketed in this construct's own statement sequence; the `State`
    /// versions its brackets mint live inside the expression the caller
    /// builds, so the caller rolls the counter back once it is complete.
    fn bind_inline_block_value_args(
        &mut self,
        block: &Block,
        arguments: &[Expression],
    ) -> Result<Vec<Document<'static>>> {
        let mut arg_bindings: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for (i, param) in block.parameters.iter().enumerate() {
            if i < arguments.len() {
                let arg_var = self.fresh_temp_var(&param.name);
                if self.is_sync_site(&arguments[i]) {
                    let frame = self.current_frame();
                    let tv = self.threaded_expression(&arguments[i], frame)?;
                    let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
                    let value_doc = self.threaded_value_doc(&tv.value);
                    arg_bindings.push(docvec![
                        prelude_doc,
                        "let ",
                        leaf::var(arg_var.clone()),
                        " = ",
                        value_doc,
                        " in ",
                    ]);
                } else {
                    let arg_doc = self.expression_doc(&arguments[i])?;
                    arg_bindings.push(docvec![
                        "let ",
                        leaf::var(arg_var.clone()),
                        " = ",
                        arg_doc,
                        " in ",
                    ]);
                }
                self.bind_var(&param.name, &arg_var);
            }
        }
        Ok(arg_bindings)
    }

    /// Generates inline code for `[block_with_mutations] value` (or `value:`).
    ///
    /// When a block literal has captured mutations (variables read AND written, like
    /// `[errors := errors add: #foo]`), this inlines the block body with state threading
    /// instead of generating a Tier 2 fun + apply (which would produce a fun with an extra
    /// `StateAcc` parameter, causing a badarity crash when called with `value`).
    ///
    /// Returns a `{Result, NewState}` tuple (same contract as `generate_solo_boolean_with_mutations`).
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
        if !arguments.is_empty() {
            self.push_scope();
        }
        let version_on_entry = self.state_version();
        let arg_bindings = self.bind_inline_block_value_args(block, arguments)?;

        // ValueType context: inline block body directly as sequential let-bindings.
        // No StateAcc needed — mutations are just Core Erlang variable shadowing.
        // Don't push/pop scope — the let-bindings must be visible to subsequent
        // method body expressions (the whole point of the mutation).
        if self.context == CodeGenContext::ValueType && !self.in_loop_body {
            let body = super::super::util::collect_body_exprs(&block.body);

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
                    parts.push(let_in(leaf::var(tmp), doc, Document::Nil, Layout::Flat));
                }
            }

            if !arguments.is_empty() {
                self.pop_scope();
            }
            return Ok(Document::Vec(parts));
        }

        // BT-3982: for a block the actor producer claims
        // (`actor_producer_claims_block`), the raw tuple below is the value
        // unless a body sequencer consumes this site as a Tier 2 statement:
        // the producer gate and this lowering disagree.
        if self.actor_producer_claims_block(block)
            && !self.tier2_evaluated_block_sites.contains(&block.span)
        {
            let errors = [
                super::super::threaded_ir::VerifyError::UnthreadedEvaluatedBlock { at: block.span },
            ];
            self.report_threaded_ir_verify_errors(
                &errors,
                "an evaluated block that threads state must be lowered as a producer",
                block.span,
            );
        }

        // Actor/REPL context: use StateAcc-based inlining (`generate_repl_arm_inline`,
        // which is `generate_conditional_branch_inline` outside the REPL)
        let outer_state = self.current_state_var();

        // ADR 0111 Addendum 5: REPL-mode conditional-branch
        // inlining — reaches the SAME single-arm helper
        // (`generate_repl_arm_inline`, BT-3823), which builds, `verify()`s, and `render()`s
        // this arm's real per-frame ThreadedIr internally; the scalar
        // `check_branch_frame_linearity` scaffolding check that used to run
        // here is gone.
        let (branch_doc, _branch_final) = self.with_branch_context(|this| {
            // Set repl_loop_mutated so the REPL module unpacks the {Result, State} tuple
            if this.is_repl_mode() {
                this.set_repl_loop_mutated(true);
            }
            // BT-3823: in the REPL the block is a REPL arm, whose producers
            // adopt the bindings map (`generate_repl_arm_inline`).
            this.generate_repl_arm_inline(block)
        })?;

        let mut parts: Vec<Document<'static>> = Vec::with_capacity(arg_bindings.len() + 2);
        parts.extend(arg_bindings);
        // Seed StateAcc from outer state
        parts.push(let_in(
            Document::Str("StateAcc"),
            leaf::var(outer_state),
            Document::Nil,
            Layout::Flat,
        ));
        parts.push(branch_doc);

        if !arguments.is_empty() {
            self.pop_scope();
        }
        if self.synced_calls_active() {
            self.set_state_version(version_on_entry);
        }
        Ok(Document::Vec(parts))
    }
}
