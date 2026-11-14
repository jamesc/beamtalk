// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Transform/aggregate list operations: `inject:into:`, `flatMap:`, `count:`,
//! `takeWhile:`, `dropWhile:`, `partition:`, `groupBy:`, and `sort:`.

use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CoreErlangGenerator, Result};
use super::super::{BodyKind, ThreadingPlan};
use super::fold_driver::FoldOp;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

impl CoreErlangGenerator {
    /// Generates code for `list count:` with mutation analysis.
    ///
    /// Without mutations: falls through to `lists:filter` + `erlang:length`.
    /// With mutations: uses `lists:foldl` with a count accumulator, processing
    /// all elements for state threading.
    pub(in crate::core_erlang) fn generate_list_count(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "count:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list count: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_count_with_mutations(receiver, body_block);
        }
        // No mutations: use lists:filter + erlang:length
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "count:", 1)?;
        let filtered_var = self.fresh_temp_var("temp");

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> let ",
            leaf::var(filtered_var.clone()),
            " = call 'lists':'filter'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") in call 'erlang':'length'(",
            leaf::var(filtered_var),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'count:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `count:` using `lists:foldl` with state threading
    /// and a count accumulator.
    ///
    /// All elements are processed. Accumulator is `{Count, StateVars...}`.
    pub(in crate::core_erlang) fn generate_list_count_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(receiver, body, FoldOp::Count)
    }

    /// Generates code for `list flatMap:` with mutation analysis.
    ///
    /// Without mutations: falls through to `lists:flatmap/2`.
    /// With mutations: uses `lists:foldl` like `collect:` (accumulates reversed
    /// sub-lists), then `lists:append(lists:reverse(...))` to flatten.
    pub(in crate::core_erlang) fn generate_list_flat_map(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "flatMap:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list flatMap: [:item | #(item, item * 2)]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_flat_map_with_mutations(receiver, body_block);
        }

        // No mutations: use lists:flatmap
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "flatMap:", 1)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'lists':'flatmap'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'flatMap:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `flatMap:` using `lists:foldl` with state threading.
    ///
    /// Uses `FoldlCollect` body kind to build a reversed list of sub-lists, then
    /// reverses and appends (flattens) the result.
    ///
    /// Accumulator is `{ResultList, StateVars...}` (tuple) or `{ResultList, StateAcc}` (map).
    pub(in crate::core_erlang) fn generate_list_flat_map_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(receiver, body, FoldOp::FlatMap)
    }

    pub(in crate::core_erlang) fn generate_list_inject(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        self.generate_list_inject_for(receiver, initial, body, "inject:into:")
    }

    /// [`Self::generate_list_inject`] for a fold that stands in for
    /// `guard_selector` (BT-3916): `inject:into:` for the message itself, or
    /// the user's `eachWithIndex:` / `do:separatedBy:` that
    /// `enumeration_ops.rs` lowers through a synthetic `inject:into:`. The
    /// receiver guard of a writing fold checks that selector.
    pub(in crate::core_erlang) fn generate_list_inject_for(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Expression,
        guard_selector: &str,
    ) -> Result<Document<'static>> {
        // Validate body block arity (must be 2-arg: accumulator and element)
        validate_block_arity_exact(
            body,
            2,
            "inject:into:",
            "Fix: The body block must take two arguments (accumulator and element):\n\
             \x20 list inject: 0 into: [:sum :each | sum + each]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_inject_with_mutations(
                receiver,
                initial,
                body_block,
                guard_selector,
            );
        }
        // Pure-block fast path — emit inline lists:foldl instead of
        // calling beamtalk_collection:inject_into at runtime.
        // This eliminates: (1) the runtime function call overhead, (2) the to_list
        // indirection when receiver is already a list.
        //
        // lists:foldl calls Fun(Elem, Acc) but Beamtalk convention is Block(Acc, Elem).
        // For literal blocks, we compile the body directly with swapped parameter order
        // (Elem, Acc) to avoid any wrapper overhead. For non-literal callables, we fall
        // back to beamtalk_collection:inject_into which handles arg swapping at runtime.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let init_var = self.fresh_temp_var("temp");
        let init_code = self.expression_doc(initial)?;
        let lambda_var = self.fresh_temp_var("temp");

        // Generate the foldl fun: for literal blocks, compile body with swapped
        // parameter order; for non-literal, use runtime arg-swap wrapper.
        let foldl_fun_doc = if let Expression::Block(body_block) = body {
            // Literal block: compile directly with foldl parameter order (Elem, Acc).
            // Block params: [0] = acc, [1] = elem (Beamtalk convention)
            // Foldl fun params: (Elem, Acc) (Erlang convention)
            let acc_param = body_block
                .parameters
                .first()
                .map_or("_", |p| p.name.as_str());
            let elem_param = body_block
                .parameters
                .get(1)
                .map_or("_", |p| p.name.as_str());
            let acc_var = Self::to_core_erlang_var(acc_param);
            let elem_var = Self::to_core_erlang_var(elem_param);

            // A closed `fun`: its body's state-version bumps (BT-3737: a
            // conditional with a block-local write) must not leak out. It is an
            // actor closure (ADR 0137 §3): it reads the cell, and the call to
            // `lists:foldl` is the bracket.
            self.with_actor_closure(body_block.span, |this| {
                let body_doc = this.with_closed_fun_scope(|this| {
                    // Bind in swapped order: foldl's first param is Elem, second is Acc
                    if let Some(param) = body_block.parameters.get(1) {
                        this.bind_var(&param.name, &elem_var);
                    }
                    if let Some(param) = body_block.parameters.first() {
                        this.bind_var(&param.name, &acc_var);
                    }
                    this.generate_block_body(body_block)
                })?;

                // fun (Elem, Acc) -> <body>
                Ok(docvec![
                    "fun (",
                    leaf::var(elem_var.clone()),
                    ", ",
                    leaf::var(acc_var.clone()),
                    ") -> ",
                    body_doc,
                ])
            })?
        } else {
            // Non-literal callable: wrap to swap args at runtime.
            let body_var = self.fresh_temp_var("temp");
            let body_code = self.non_literal_callable_doc(body, "inject:into:", 2)?;
            docvec![
                "let ",
                leaf::var(body_var.clone()),
                " = ",
                body_code,
                " in fun (Elem, Acc) -> ",
                self.annotate_current_send_call(docvec![
                    "apply ",
                    leaf::var(body_var),
                    " (Acc, Elem)"
                ]),
            ]
        };

        Ok(docvec![
            super::list_recv_to_safe_list_doc(recv_code, None, list_var, safe_list_var.clone()),
            "let ",
            leaf::var(init_var.clone()),
            " = ",
            init_code,
            " in let ",
            leaf::var(lambda_var.clone()),
            " = ",
            foldl_fun_doc,
            " in ",
            ThreadingPlan::foldl_expr_doc(&lambda_var, leaf::var(init_var), &safe_list_var),
        ])
    }

    pub(in crate::core_erlang) fn generate_list_inject_with_mutations(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Block,
        guard_selector: &str,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(
            receiver,
            body,
            FoldOp::Inject {
                initial,
                guard: guard_selector,
            },
        )
    }

    // ── takeWhile:/dropWhile:/groupBy:/partition:/sort: ─────────

    /// Generates code for `list takeWhile:` with mutation analysis.
    ///
    /// Without mutations: falls through to `lists:takewhile/2`.
    /// With mutations: uses `lists:foldl` processing ALL elements, tracking
    /// a `StillTaking` flag that flips to false on first predicate failure.
    pub(in crate::core_erlang) fn generate_list_take_while(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "takeWhile:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list takeWhile: [:item | item < 10]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_take_while_with_mutations(receiver, body_block);
        }

        // No mutations: use lists:takewhile
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "takeWhile:", 1)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'lists':'takewhile'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'takeWhile:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `takeWhile:` using `lists:foldl` with state threading.
    ///
    /// Accumulator is `{ResultList, StillTaking, StateVars...}`.
    /// All elements are processed. Once predicate returns false, `StillTaking` flips
    /// to false and all subsequent elements are excluded regardless of predicate.
    pub(in crate::core_erlang) fn generate_list_take_while_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(receiver, body, FoldOp::TakeWhile)
    }

    /// Generates code for `list dropWhile:` with mutation analysis.
    ///
    /// Without mutations: falls through to `lists:dropwhile/2`.
    /// With mutations: uses `lists:foldl` processing ALL elements, tracking
    /// a `StillDropping` flag that flips to false on first predicate failure.
    pub(in crate::core_erlang) fn generate_list_drop_while(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "dropWhile:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list dropWhile: [:item | item < 10]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_drop_while_with_mutations(receiver, body_block);
        }

        // No mutations: use lists:dropwhile
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "dropWhile:", 1)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'lists':'dropwhile'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'dropWhile:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `dropWhile:` using `lists:foldl` with state threading.
    ///
    /// Accumulator is `{ResultList, StillDropping, StateVars...}`.
    pub(in crate::core_erlang) fn generate_list_drop_while_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(receiver, body, FoldOp::DropWhile)
    }

    /// Generates code for `list partition:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:partition/2`.
    /// With mutations: uses `lists:foldl` routing each element to one of two lists.
    pub(in crate::core_erlang) fn generate_list_partition(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "partition:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list partition: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_partition_with_mutations(receiver, body_block);
        }

        // No mutations: use beamtalk_list:partition
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "partition:", 1)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'beamtalk_list':'partition'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::var(body_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'partition:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `partition:` using `lists:foldl` with state threading.
    ///
    /// Accumulator is `{MatchList, NoMatchList, StateVars...}`.
    /// Result is `{lists:reverse(MatchList), lists:reverse(NoMatchList)}` (a 2-tuple, converted to
    /// `#(matches, nonMatches)` by the runtime).
    pub(in crate::core_erlang) fn generate_list_partition_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(receiver, body, FoldOp::Partition)
    }

    /// Generates code for `list groupBy:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:group_by/2`.
    /// With mutations: uses `lists:foldl` building a map where each key
    /// maps to the list of elements that produced that key.
    pub(in crate::core_erlang) fn generate_list_group_by(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "groupBy:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list groupBy: [:item | item isEven]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_group_by_with_mutations(receiver, body_block);
        }

        // No mutations: use beamtalk_list:group_by
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "groupBy:", 1)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'beamtalk_list':'group_by'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::var(body_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'groupBy:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `groupBy:` using `lists:foldl` with state threading.
    ///
    /// Accumulator is `{GroupMap, StateVars...}`.
    /// The key block result is used to group elements into a map.
    pub(in crate::core_erlang) fn generate_list_group_by_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(receiver, body, FoldOp::GroupBy)
    }

    /// Generates code for `list sort:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:sort_with/2`.
    /// With mutations: uses process dictionary to thread state through the
    /// sort comparator, since `lists:sort/2` controls comparison order and
    /// the comparator must return a plain boolean.
    pub(in crate::core_erlang) fn generate_list_sort(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            2,
            "sort:",
            "Fix: The body block must take two arguments (elements to compare):\n\
             \x20 list sort: [:a :b | a < b]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_sort_with_mutations(receiver, body_block);
        }

        // No mutations: use beamtalk_list:sort_with
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "sort:", 2)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'beamtalk_list':'sort_with'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::var(body_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'sort:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `sort:` using process dictionary for state threading.
    ///
    /// Strategy:
    /// 1. Pack current state into a process dictionary key unique to this invocation
    ///    (a fresh `erlang:make_ref/0`, not a fixed atom — nested/recursive
    ///    calls to the same call site each get their own key, so they can't stomp
    ///    each other's state).
    /// 2. Build a wrapper comparator that reads state, runs the block body
    ///    (a [`BodyKind::FoldlSort`] fold body), writes updated state back,
    ///    returns boolean result
    /// 3. Call `lists:sort/2` with the wrapper, inside a `try` whose catch arm
    ///    erases the key before re-raising (BT-3779: a comparator `^` or
    ///    exception must not leak the per-call key)
    /// 4. Read final state from process dictionary and erase the key
    ///    (`erlang:erase/1` does both)
    ///
    /// This approach is necessary because `lists:sort` controls comparison order
    /// and the comparator must return a plain boolean — we can't use foldl.
    ///
    /// Always uses the map-accumulator path (the plan is built without a
    /// tuple accumulator): the state crossing the process dictionary is the
    /// `StateAcc` map.
    pub(in crate::core_erlang) fn generate_list_sort_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new(self, body, None);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let wrapper_var = self.fresh_temp_var("temp");

        let param_a = body.parameters.first().map_or("_A", |p| p.name.as_str());
        let param_b = body.parameters.get(1).map_or("_B", |p| p.name.as_str());
        let var_a = Self::to_core_erlang_var(param_a);
        let var_b = Self::to_core_erlang_var(param_b);

        // `state_key_var` names the Core Erlang variable bound to the
        // `make_ref()` key at runtime; `state_key_doc` is a Document
        // referencing that variable (not an atom literal).
        let state_key_var = self.fresh_temp_var("SortStateKey");
        let state_key_doc = leaf::var(state_key_var.clone());

        // Map-accumulator path (always used for sort — see doc comment above).
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            Some("sort:"),
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
            leaf::var(state_key_var.clone()),
            " = call 'erlang':'make_ref'() in let _ = call 'erlang':'put'(",
            state_key_doc.clone(),
            ", ",
            leaf::var(init_state),
            ") in let ",
            leaf::var(wrapper_var.clone()),
            " = fun (",
            leaf::var(var_a.clone()),
            ", ",
            leaf::var(var_b.clone()),
            ") -> let StateAcc = call 'erlang':'get'(",
            state_key_doc.clone(),
            ") in ",
        ]);

        // BT-3771: the comparator is a closed `fun` whose own entry state is
        // the `StateAcc` it just read from the process dictionary, so its
        // body lowers like every other fold body: through
        // `generate_foldl_loop_body` (one verified `Threaded` node, inside
        // `with_branch_context`, so the body starts from `StateAcc` and the
        // enclosing scope's `State`/`StateAcc` version is restored on exit).
        // Hand-lowered against the enclosing version instead, a write here
        // read the enclosing `State{N}` rather than `StateAcc` (dropping the
        // previous comparisons' writes), and a nested loop's `StateAcc{N}`
        // references were left unbound.
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &var_a);
        }
        if let Some(param) = body.parameters.get(1) {
            self.bind_var(&param.name, &var_b);
        }
        let (body_doc, _) = self.generate_foldl_loop_body(
            body,
            &plan,
            &BodyKind::FoldlSort { state_key_var },
            "StateAcc",
            0,
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let sorted_var = self.fresh_temp_var("SortedList");
        let sort_result_var = self.fresh_temp_var("SortResult");
        let type_var = self.fresh_temp_var("SortType");
        let error_var = self.fresh_temp_var("SortError");
        let stack_var = self.fresh_temp_var("SortStack");
        let state_out = self.fresh_temp_var("StOut");

        // BT-3779: the per-call key must be erased on every exit path out of
        // `lists:sort`. A `^` in the comparator throws `$bt_nlr` (and any
        // other exception propagates) straight through `lists:sort`, so the
        // key is erased in a catch-all arm that then re-raises the original
        // class/reason/stacktrace unchanged via `raw_raise` — the NLR term
        // (which already carries the comparator's state) is untouched. On the
        // normal path `erlang:erase/1` both reads the final state and removes
        // the key.
        docs.push(docvec![
            " in let ",
            leaf::var(sorted_var.clone()),
            " = try call 'lists':'sort'(",
            leaf::var(wrapper_var),
            ", ",
            leaf::var(safe_list_var),
            ") of <",
            leaf::var(sort_result_var.clone()),
            "> -> ",
            leaf::var(sort_result_var),
            " catch <",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(error_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> let _ = call 'erlang':'erase'(",
            state_key_doc.clone(),
            ") in ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
            " in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'erase'(",
            state_key_doc,
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(sorted_var),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }
}
