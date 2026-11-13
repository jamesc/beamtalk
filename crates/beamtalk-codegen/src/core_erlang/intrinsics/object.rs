// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ProtoObject`, `Object`, nil and reflection intrinsics.
//!
//! Lowers `class`, `isKindOf:`, the nil-testing protocol, `error:`
//! signalling, `yourself`/`hash` identity, and runtime reflection
//! (`respondsTo:`, `fieldNames`, `fieldAt:`, `fieldAt:put:`).
//!
//! **DDD Context:** Compilation — Code Generation

use super::{
    ClassHierarchy, CodeGenContext, CoreErlangGenerator, Document, ErrorSignalReceiver, Expression,
    MessageSelector, PERFORM_WITH_ARGS_TIMEOUT, Result, WellKnownSelector, class_var_fn, docvec,
    leaf, not_nil_apply, primitives, validate_if_not_nil_block,
};

/// The `beamtalk_behaviour_intrinsics` functions the `isKindOf:` lowerings
/// call. A typed set, so the runtime export contract
/// (`runtime_export_contract_tests`) checks every name codegen can emit.
#[derive(Debug, Clone, Copy)]
pub(in crate::core_erlang) enum KindOfFn {
    /// `isKindOfBuiltin/3`: a sealed built-in literal and its type test.
    Builtin,
    /// `isKindOfNamed/2`: a user class literal, answered by name (BT-4757);
    /// `undecided` means the caller takes the send (BT-4769).
    Named,
}

impl KindOfFn {
    pub(in crate::core_erlang) const fn name(self) -> &'static str {
        match self {
            Self::Builtin => "isKindOfBuiltin",
            Self::Named => "isKindOfNamed",
        }
    }

    /// The arity the lowering calls it with, for the export contract.
    #[cfg(test)]
    pub(in crate::core_erlang) const fn arity(self) -> usize {
        match self {
            Self::Builtin => 3,
            Self::Named => 2,
        }
    }

    /// Every function above, for the export contract.
    #[cfg(test)]
    pub(in crate::core_erlang) const ALL: &[Self] = &[Self::Builtin, Self::Named];
}

impl CoreErlangGenerator {
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
    /// % For primitives (BT-4170: class is construction type — tagged
    /// % `String`s vs bare `Binary` binaries, no UTF-8 sniffing):
    /// case Receiver of
    ///   <I> when call 'erlang':'is_integer'(I) -> 'Integer'
    ///   <{'beamtalk_string', _}> when 'true' -> 'String'
    ///   <S> when call 'erlang':'is_binary'(S) -> 'Binary'
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
    pub(in crate::core_erlang) fn try_generate_protoobject_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // `class` (unary, well-known) returns class as first-class object.
        if matches!(selector.well_known(), Some(WellKnownSelector::Class)) {
            // BT-4127: a receiver typed as a tagged-map class gets the class
            // object inline, behind the BT-4069 tag guard.
            if let Ok(super::super::dispatch_codegen::TypedSendTarget::Tagged(target)) =
                self.typed_send_target(receiver, selector)
            {
                return self.generate_typed_class_of(receiver, &target).map(Some);
            }
            // Hoist open-scope receiver (e.g. class method self-send).
            let mut seq = self.sequence_call(&[receiver], "Obj")?;
            let recv_doc = seq.next();
            let call_doc = Self::class_of_object_call_doc(recv_doc);
            return Ok(Some(seq.close(self, call_doc, "ClassRes")));
        }

        // BT-3968: `perform:withArguments:timeout:` lowers like
        // `perform:withArguments:` below, straight to the call-site entry
        // point `send/4` (which applies the timeout to an actor's
        // gen_server:call and ignores it for value types) — the same thing
        // `send/3` already does when it intercepts the selector at runtime.
        // Without this an actor's own `self perform:withArguments:timeout:`
        // self-dispatched into `ProtoObject`'s method body instead. Matched
        // by name, like `asType:`: it is not a `WellKnownSelector`.
        if selector.name() == PERFORM_WITH_ARGS_TIMEOUT && arguments.len() == 3 {
            let mut seq = self.sequence_call(
                &[receiver, &arguments[0], &arguments[1], &arguments[2]],
                "PerfTo",
            )?;
            let recv_doc = seq.next();
            let sel_doc = seq.next();
            let args_doc = seq.next();
            let timeout_doc = seq.next();
            let call_doc = docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                recv_doc,
                ", ",
                sel_doc,
                ", ",
                args_doc,
                ", ",
                timeout_doc,
                ")",
            ];
            return Ok(Some(seq.close(self, call_doc, "PerfToRes")));
        }

        // `perform:` family routes through the `WellKnownSelector` enum;
        // the classifier guarantees arity, so explicit `arguments.len()` guards
        // become `debug_assert_eq!` for parser-shape invariants.
        match selector.well_known() {
            Some(WellKnownSelector::PerformWithArgs) => {
                debug_assert_eq!(arguments.len(), 2);
                // Hoist open-scope receiver + args (e.g. class method self-sends).
                let mut seq =
                    self.sequence_call(&[receiver, &arguments[0], &arguments[1]], "Perf")?;
                let recv_doc = seq.next();
                let sel_doc = seq.next();
                let args_doc = seq.next();
                let call_doc = docvec![
                    "call 'beamtalk_message_dispatch':'send'(",
                    recv_doc,
                    ", ",
                    sel_doc,
                    ", ",
                    args_doc,
                    ")",
                ];
                Ok(Some(seq.close(self, call_doc, "PerfRes")))
            }
            // Execute a class method in the caller's process,
            // bypassing the class object's gen_server.
            Some(WellKnownSelector::PerformLocallyWithArgs) => {
                debug_assert_eq!(arguments.len(), 2);
                // Hoist open-scope receiver + args (e.g. class method self-sends).
                let mut seq =
                    self.sequence_call(&[receiver, &arguments[0], &arguments[1]], "PerfLoc")?;
                let recv_doc = seq.next();
                let sel_doc = seq.next();
                let args_doc = seq.next();
                let call_doc = docvec![
                    "call 'beamtalk_object_class':'local_call'(",
                    recv_doc,
                    ", ",
                    sel_doc,
                    ", ",
                    args_doc,
                    ")",
                ];
                Ok(Some(seq.close(self, call_doc, "PerfLocRes")))
            }
            Some(WellKnownSelector::Perform) => {
                debug_assert_eq!(arguments.len(), 1);
                // Hoist open-scope receiver + selector arg.
                let mut seq = self.sequence_call(&[receiver, &arguments[0]], "Perf")?;
                let recv_doc = seq.next();
                let sel_doc = seq.next();
                let call_doc = docvec![
                    "call 'beamtalk_message_dispatch':'send'(",
                    recv_doc,
                    ", ",
                    sel_doc,
                    ", [])",
                ];
                Ok(Some(seq.close(self, call_doc, "PerfRes")))
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
    pub(in crate::core_erlang) fn try_generate_object_message(
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
        if let Some(doc) = self.try_generate_builtin_is_kind_of(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        Ok(None)
    }

    /// BT-4473 / BT-4764: `x isKindOf: C` where `C` is a class literal naming a
    /// sealed built-in whose instances (with its subclasses') are exactly the
    /// values passing its routing-fixture type test — the test a
    /// `binding :: C` match arm uses ([`routing::type_test`], rendered by the
    /// same [`Self::render_type_test`], so the two cannot disagree). The test
    /// is applied to `x` inline behind the node-wide override check; see
    /// [`Self::emit_builtin_kind_of`] for the emitted shape and why each exit
    /// is sound. Anything the test cannot answer for a plain value takes
    /// `beamtalk_behaviour_intrinsics:isKindOfBuiltin/3` or the ordinary send.
    ///
    /// Sound because a plain value's class is a stdlib built-in, and no
    /// built-in below `Object` defines `isKindOf:` — checked here against the
    /// class hierarchy, so a stdlib override would turn this lowering off.
    ///
    /// `self`/`super` and class-literal receivers keep their own lowering
    /// (the same exclusion as [`Self::try_generate_pure_boolean_conditional`]):
    /// an actor's `self`, or a class literal inside a class method of a class
    /// with class variables, would make the fallback send a call back into
    /// the process the method is running in.
    fn try_generate_builtin_is_kind_of(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if !matches!(selector.well_known(), Some(WellKnownSelector::IsKindOf)) {
            return Ok(None);
        }
        let [class_arg] = arguments else {
            return Ok(None);
        };
        if matches!(
            receiver,
            Expression::Super(_) | Expression::ClassReference { .. }
        ) || super::super::expr_shape::is_self_identifier(receiver)
        {
            return Ok(None);
        }
        let Some(test) = self.builtin_kind_test(class_arg) else {
            // BT-4757: a user class literal takes the name-only lowering. The
            // built-in lowering above stays first, so a later built-in
            // generalisation (BT-4764) slots in beside it with the same
            // override guard in the runtime.
            //
            // Not in the REPL: a capitalised name there resolves through the
            // session map (`Workspace bind:as:`, ADR 0129 §7) before the class
            // registry, so the evaluated class need not be the one the
            // spelling names.
            if self.context != CodeGenContext::Repl
                && let Some(class_name) = Self::user_class_literal_name(class_arg)
            {
                return self.emit_named_kind_of(receiver, class_arg, &class_name);
            }
            return Ok(None);
        };
        let Expression::ClassReference { name, .. } = class_arg else {
            return Ok(None);
        };
        self.emit_builtin_kind_of(receiver, class_arg, &name.name, test)
    }

    /// BT-4764: the inline lowering of `x isKindOf: C` for a sealed built-in
    /// `C` whose type test is `test`:
    ///
    /// ```erlang
    /// let R = <x> in
    /// case is_pid(whereis('beamtalk_class_C')) and not overridden_anywhere('isKindOf:') of
    ///   'true'  -> case <C's test on R> of
    ///                'true'  -> 'true'  % or isKindOfBuiltin(R, K, 'true'), see below
    ///                'false' -> 'false' % for a non-tuple, non-map R, else the call below
    ///              end
    ///   'false' -> let K = <C> in send(R, 'isKindOf:', [K])  % override, or dead class
    /// end
    /// ```
    ///
    /// The fast path builds no class reference, sends nothing and makes no
    /// runtime call besides the one `persistent_term` read and a `whereis`
    /// that keeps today's `class_not_found` for a dead class process (the
    /// literal used to be evaluated first; no new behaviour difference). The node-wide
    /// override flag is read on every execution (never cached: a later
    /// override must take effect, ADR 0142 §2).
    ///
    /// Everything the runtime's `plain_value/1` screens out is a tuple or a
    /// map, so:
    ///
    /// - a **hit** is `'true'` outright when [`Self::kind_test_hit_proves_plain`]
    ///   holds for the strategy (the value passing the test cannot be an actor,
    ///   supervisor, future or tagged map); otherwise the hit takes
    ///   `isKindOfBuiltin(R, K, 'true')`, which still runs `plain_value/1`;
    /// - a **miss** is `'false'` outright for any receiver that is neither a
    ///   tuple nor a map; a tuple or map takes `isKindOfBuiltin(R, K, 'false')`,
    ///   which answers `'false'` for a plain value and sends otherwise.
    fn emit_builtin_kind_of(
        &mut self,
        receiver: &Expression,
        class_arg: &Expression,
        class_name: &str,
        test: super::super::routing::Guard,
    ) -> Result<Option<Document<'static>>> {
        let mut seq = self.sequence_call(&[receiver, class_arg], "KindOf")?;
        let recv_doc = seq.next();
        let class_doc = seq.next();
        let recv_var = self.fresh_temp_var("KindOfRecv");

        let runtime_answer = |this: &mut Self, answer: &'static str| {
            let class_var = this.fresh_temp_var("KindOfClass");
            docvec![
                "let ",
                leaf::var(class_var.clone()),
                " = ",
                class_doc.clone(),
                " in call 'beamtalk_behaviour_intrinsics':",
                leaf::atom(KindOfFn::Builtin.name().to_string()),
                "(",
                leaf::var(recv_var.clone()),
                ", ",
                leaf::var(class_var),
                ", ",
                Document::Str(answer),
                ")"
            ]
        };
        let hit = if Self::kind_test_hit_proves_plain(test) {
            Document::Str("'true'")
        } else {
            runtime_answer(self, "'true'")
        };
        let miss_slow = runtime_answer(self, "'false'");
        let miss = docvec![
            "case call 'erlang':'or'(call 'erlang':'is_tuple'(",
            leaf::var(recv_var.clone()),
            "), call 'erlang':'is_map'(",
            leaf::var(recv_var.clone()),
            ")) of <'true'> when 'true' -> ",
            miss_slow,
            " <'false'> when 'true' -> 'false' end"
        ];
        // The test as a boolean, then the hit/miss split: the miss path is
        // written once however many exits the strategy's test has.
        let test_doc = self.render_type_test(
            test,
            &recv_var,
            Document::Str("'true'"),
            &Document::Str("'false'"),
        );
        let fast = docvec![
            "case ",
            test_doc,
            " of <'true'> when 'true' -> ",
            hit,
            " <'false'> when 'true' -> ",
            miss,
            " end"
        ];

        // The plain send, behind the class literal's own evaluation: used when
        // an override exists, and when the class process is dead, where the
        // literal raises `class_not_found` exactly as it always did.
        let send = self.kind_of_plain_send_doc(&recv_var, class_doc);
        let call_doc = docvec![
            "let ",
            leaf::var(recv_var),
            " = ",
            recv_doc,
            " in case ",
            Self::kind_of_fast_guard_doc(class_name),
            " of ",
            "<'true'> when 'true' -> ",
            fast,
            " <'false'> when 'true' -> ",
            send,
            " end"
        ];
        Ok(Some(seq.close(self, call_doc, "KindOfRes")))
    }

    /// Whether a value passing `test` is certainly one the runtime's
    /// `plain_value/1` accepts, so a hit may be answered without the call.
    /// Exhaustive on purpose: a new strategy must decide.
    ///
    /// - `Bif` (integer, float, list, binary, boolean, function…), the
    ///   bitstring test, `Symbol` and `ExactAtom`: the value is not a tuple
    ///   or map, which `plain_value/1` accepts unconditionally.
    /// - `StringTag`, `BinaryOrStringTag`: the only tuple admitted is the
    ///   `String` tag, the one reserved head `plain_value/1` accepts.
    /// - `UntaggedMap`: a map with neither `'$beamtalk_class'` nor
    ///   `'__class_mod__'`, i.e. exactly `not is_tagged/1`.
    /// - `UnreservedTuple` does not prove it: its test rejects a reserved tag
    ///   only at the tag's own tuple size, while `plain_value/1` rejects it at
    ///   every size.
    const fn kind_test_hit_proves_plain(test: super::super::routing::Guard) -> bool {
        use super::super::routing::Guard;
        match test {
            Guard::Bif(_)
            | Guard::StringTag
            | Guard::BinaryOrStringTag
            | Guard::UnalignedBitstring
            | Guard::Symbol
            | Guard::ExactAtom(_)
            | Guard::UntaggedMap => true,
            Guard::UnreservedTuple => false,
        }
    }

    /// `call 'erlang':'is_pid'(whereis('beamtalk_class_<Name>'))`: whether the
    /// class literal's process is registered and alive. The one liveness probe
    /// of both `isKindOf:` fast paths (BT-4764 built-in, BT-4757 name-only,
    /// BT-4768): a dead class takes the plain send, which evaluates the
    /// literal and raises `class_not_found` exactly as it always did.
    fn class_process_alive_doc(class_name: &str) -> Document<'static> {
        docvec![
            "call 'erlang':'is_pid'(",
            Self::registered_class_pid_doc(class_name),
            ")"
        ]
    }

    /// The plain `isKindOf:` send behind the class literal's own evaluation
    /// (`let K = <class> in send(R, 'isKindOf:', [K])`), shared by both
    /// fast-path lowerings for their dead-class / override exit.
    fn kind_of_plain_send_doc(
        &mut self,
        recv_var: &str,
        class_doc: Document<'static>,
    ) -> Document<'static> {
        let send_class = self.fresh_temp_var("KindOfClass");
        // The call node carries the send's source line, as every dispatch
        // call does (`call_site_lines`).
        let send_call = self.annotate_current_send_call(docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            leaf::var(recv_var.to_string()),
            ", 'isKindOf:', [",
            leaf::var(send_class.clone()),
            "])"
        ]);
        docvec![
            "let ",
            leaf::var(send_class),
            " = ",
            class_doc,
            " in ",
            send_call
        ]
    }

    /// The guard of both fast paths: the literal's class process is alive
    /// ([`Self::class_process_alive_doc`]) and nothing but `Object` has
    /// defined `isKindOf:` ([`Self::override_flag_read_doc`], read on every
    /// execution).
    fn kind_of_fast_guard_doc(class_name: &str) -> Document<'static> {
        docvec![
            "call 'erlang':'and'(",
            Self::class_process_alive_doc(class_name),
            ", call 'erlang':'not'(",
            Self::override_flag_read_doc("isKindOf:"),
            "))"
        ]
    }

    /// BT-4757 / BT-4768 / BT-4769: the name-only lowering of
    /// `x isKindOf: UserClass`:
    ///
    /// ```erlang
    /// let R = <x> in
    /// let A = case <guard> of
    ///           'true'  -> isKindOfNamed(R, 'C')   % true | false | undecided
    ///           'false' -> 'undecided'
    ///         end in
    /// case A of
    ///   'true' -> 'true'
    ///   'false' -> 'false'
    ///   _ -> let K = <C> in send(R, 'isKindOf:', [K])  % override, dead class, anything else
    /// end
    /// ```
    ///
    /// The class reference is built only on the send exit, where the literal
    /// is evaluated and raises `class_not_found` for an unregistered class
    /// exactly as before; the fast path builds no reference, sends nothing and
    /// makes the one runtime call. The guard is the one the built-in lowering
    /// uses, so a dead class process raises as it did before the name-only
    /// path existed.
    fn emit_named_kind_of(
        &mut self,
        receiver: &Expression,
        class_arg: &Expression,
        class_name: &str,
    ) -> Result<Option<Document<'static>>> {
        let mut seq = self.sequence_call(&[receiver, class_arg], "KindOf")?;
        let recv_doc = seq.next();
        let class_doc = seq.next();
        let recv_var = self.fresh_temp_var("KindOfRecv");
        let answer_var = self.fresh_temp_var("KindOfAnswer");
        let send = self.kind_of_plain_send_doc(&recv_var, class_doc);
        let call_doc = docvec![
            "let ",
            leaf::var(recv_var.clone()),
            " = ",
            recv_doc,
            " in let ",
            leaf::var(answer_var.clone()),
            " = case ",
            Self::kind_of_fast_guard_doc(class_name),
            " of <'true'> when 'true' -> call 'beamtalk_behaviour_intrinsics':",
            leaf::atom(KindOfFn::Named.name().to_string()),
            "(",
            leaf::var(recv_var),
            ", ",
            leaf::atom(class_name.to_string()),
            ") <'false'> when 'true' -> 'undecided' end in case ",
            leaf::var(answer_var),
            " of <'true'> when 'true' -> 'true' <'false'> when 'true' -> 'false' ",
            "<_> when 'true' -> ",
            send,
            " end"
        ];
        Ok(Some(seq.close(self, call_doc, "KindOfRes")))
    }

    /// BT-4757: the class name when `class_arg` is an unqualified literal
    /// naming a user class (not a stdlib built-in).
    fn user_class_literal_name(class_arg: &Expression) -> Option<String> {
        let Expression::ClassReference {
            name,
            package: None,
            ..
        } = class_arg
        else {
            return None;
        };
        let class_name = name.name.as_str();
        (!ClassHierarchy::is_builtin_class(class_name)).then(|| class_name.to_string())
    }

    /// BT-4473: the routing-fixture type test of `class_arg` when it is an
    /// unqualified class literal naming a sealed built-in that has one, and
    /// no built-in class other than `Object` defines `isKindOf:` — the
    /// conditions [`Self::try_generate_builtin_is_kind_of`] relies on.
    fn builtin_kind_test(&self, class_arg: &Expression) -> Option<super::super::routing::Guard> {
        let Expression::ClassReference {
            name,
            package: None,
            ..
        } = class_arg
        else {
            return None;
        };
        let class_name = name.name.as_str();
        if !ClassHierarchy::is_builtin_class(class_name) || self.is_shadowed_class(class_name) {
            return None;
        }
        let hierarchy = self.class_hierarchy.as_ref()?;
        if !hierarchy.get_class(class_name)?.is_sealed {
            return None;
        }
        let overridden = hierarchy.builtin_other_than_object_defines("isKindOf:");
        if overridden {
            return None;
        }
        super::super::routing::type_test(class_name)
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
        // Dispatch nil-protocol intrinsics via the `WellKnownSelector`
        // enum. The classifier guarantees arity (e.g. `ifNil:ifNotNil:` requires
        // exactly two keyword parts), so explicit `arguments.len()` guards become
        // redundant — we keep one `debug_assert` per arm for paranoia.
        match selector.well_known() {
            Some(WellKnownSelector::IsNil) => {
                debug_assert!(arguments.is_empty());
                // Hoist open-scope receiver (e.g. class method self-send).
                let mut seq = self.sequence_call(&[receiver], "Obj")?;
                let recv_doc = seq.next();
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
                Ok(Some(seq.close(self, call_doc, "IsNilRes")))
            }
            Some(WellKnownSelector::NotNil) => {
                debug_assert!(arguments.is_empty());
                // Hoist open-scope receiver (e.g. class method self-send).
                let mut seq = self.sequence_call(&[receiver], "Obj")?;
                let recv_doc = seq.next();
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
                Ok(Some(seq.close(self, call_doc, "NotNilRes")))
            }
            Some(WellKnownSelector::IfNil) => {
                debug_assert_eq!(arguments.len(), 1);
                // ADR 0118 phase 4: same inline mutation-threading
                // check as `ifNotNil:` below — a self-send or field mutation
                // inside the `ifNil:` block must not be lost to a plain
                // closure.
                if self.nil_conditional_inlines()
                    && let Expression::Block(block) = &arguments[0]
                    && self.conditional_needs_mutation_threading(receiver, &[block])
                {
                    let tuple_doc = self.generate_nil_conditional_with_mutations_tuple(
                        receiver,
                        Some(block),
                        None,
                        self.conditional_scratch(self.current_frame()),
                        false,
                    )?;
                    return Ok(Some(tuple_doc));
                }
                // Hoist open-scope receiver/block (e.g. class method self-sends).
                let mut seq = self.sequence_call(&[receiver, &arguments[0]], "IfNil")?;
                let recv_doc = seq.next();
                let block_doc = seq.next();
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
                    " of <'nil'> when 'true' -> ",
                    // BT-4366: nested in the nil-test `case`, so the apply carries the
                    // send's line itself.
                    self.annotate_current_send_call(docvec!["apply ", leaf::var(block_var), " ()"]),
                    " <_> when 'true' -> ",
                    leaf::var(recv_var),
                    " end",
                ];
                Ok(Some(seq.close(self, call_doc, "IfNilRes")))
            }
            Some(WellKnownSelector::IfNotNil) => {
                debug_assert_eq!(arguments.len(), 1);
                let selector_name = WellKnownSelector::IfNotNil.as_str();
                // When in actor context or loop body, check if the block
                // contains field mutations. If so, generate inline state-threaded code
                // instead of a closure to ensure mutations persist correctly.
                if self.nil_conditional_inlines()
                    && let Expression::Block(block) = &arguments[0]
                {
                    // inline when a nested list op mutates an outer local (see IfTrue).
                    // also inline for a self-send receiver (see IfTrue above).
                    let needs_threading =
                        self.conditional_needs_mutation_threading(receiver, &[block]);
                    if needs_threading {
                        // Validate arity before generating mutation-threaded code.
                        // This ensures a block with >1 params still raises
                        // BlockArityMismatch rather than producing invalid Core Erlang.
                        validate_if_not_nil_block(&arguments[0], selector_name)?;
                        let doc = self.generate_nil_conditional_with_mutations_tuple(
                            receiver,
                            None,
                            Some(block),
                            self.conditional_scratch(self.current_frame()),
                            false,
                        )?;
                        return Ok(Some(doc));
                    }
                }
                // If the block has 0 parameters, don't pass the receiver (avoids badarity)
                // Hoist open-scope receiver/block (e.g. class method self-sends).
                let block_takes_arg = validate_if_not_nil_block(&arguments[0], selector_name)?;
                let mut seq = self.sequence_call(&[receiver, &arguments[0]], "IfNotNil")?;
                let recv_doc = seq.next();
                let block_doc = seq.next();
                let recv_var = self.fresh_temp_var("Obj");
                let block_var = self.fresh_temp_var("NotNilBlk");
                let apply = self.annotate_current_send_call(not_nil_apply(
                    &block_var,
                    &recv_var,
                    block_takes_arg,
                ));
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
                Ok(Some(seq.close(self, call_doc, "IfNotNilRes")))
            }
            Some(WellKnownSelector::IfNilIfNotNil) => {
                debug_assert_eq!(arguments.len(), 2);
                let selector_name = WellKnownSelector::IfNilIfNotNil.as_str();
                // ADR 0118 phase 4: inline mutation-threading
                // check, same as the single-block `ifNil:`/`ifNotNil:`
                // arms above.
                if self.nil_conditional_inlines()
                    && let (Expression::Block(nil_block), Expression::Block(not_nil_block)) =
                        (&arguments[0], &arguments[1])
                    && self
                        .conditional_needs_mutation_threading(receiver, &[nil_block, not_nil_block])
                {
                    validate_if_not_nil_block(&arguments[1], selector_name)?;
                    let tuple_doc = self.generate_nil_conditional_with_mutations_tuple(
                        receiver,
                        Some(nil_block),
                        Some(not_nil_block),
                        self.conditional_scratch(self.current_frame()),
                        false,
                    )?;
                    return Ok(Some(tuple_doc));
                }
                // If the notNil block has 0 parameters, don't pass the receiver
                // Hoist open-scope sub-expressions (e.g. class method self-sends).
                let block_takes_arg = validate_if_not_nil_block(&arguments[1], selector_name)?;
                let mut seq =
                    self.sequence_call(&[receiver, &arguments[0], &arguments[1]], "IfNilNotNil")?;
                let recv_doc = seq.next();
                let nil_doc = seq.next();
                let not_nil_doc = seq.next();
                let recv_var = self.fresh_temp_var("Obj");
                let nil_var = self.fresh_temp_var("NilBlk");
                let not_nil_var = self.fresh_temp_var("NotNilBlk");
                let apply = self.annotate_current_send_call(not_nil_apply(
                    &not_nil_var,
                    &recv_var,
                    block_takes_arg,
                ));
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
                    " of <'nil'> when 'true' -> ",
                    // BT-4366: nested in the nil-test `case`, so the apply carries the
                    // send's line itself.
                    self.annotate_current_send_call(docvec!["apply ", leaf::var(nil_var), " ()"]),
                    " <_> when 'true' -> ",
                    apply,
                    " end",
                ];
                Ok(Some(seq.close(self, call_doc, "IfNilNotNilRes")))
            }
            Some(WellKnownSelector::IfNotNilIfNil) => {
                debug_assert_eq!(arguments.len(), 2);
                let selector_name = WellKnownSelector::IfNotNilIfNil.as_str();
                // ADR 0118 phase 4: inline mutation-threading
                // check, same as the single-block `ifNil:`/`ifNotNil:`
                // arms above.
                if self.nil_conditional_inlines()
                    && let (Expression::Block(not_nil_block), Expression::Block(nil_block)) =
                        (&arguments[0], &arguments[1])
                    && self
                        .conditional_needs_mutation_threading(receiver, &[not_nil_block, nil_block])
                {
                    validate_if_not_nil_block(&arguments[0], selector_name)?;
                    let tuple_doc = self.generate_nil_conditional_with_mutations_tuple(
                        receiver,
                        Some(nil_block),
                        Some(not_nil_block),
                        self.conditional_scratch(self.current_frame()),
                        false,
                    )?;
                    return Ok(Some(tuple_doc));
                }
                // If the notNil block has 0 parameters, don't pass the receiver
                // Hoist open-scope sub-expressions (e.g. class method self-sends).
                let block_takes_arg = validate_if_not_nil_block(&arguments[0], selector_name)?;
                let mut seq =
                    self.sequence_call(&[receiver, &arguments[0], &arguments[1]], "IfNotNilNil")?;
                let recv_doc = seq.next();
                let not_nil_doc = seq.next();
                let nil_doc = seq.next();
                let recv_var = self.fresh_temp_var("Obj");
                let not_nil_var = self.fresh_temp_var("NotNilBlk");
                let nil_var = self.fresh_temp_var("NilBlk");
                let apply = self.annotate_current_send_call(not_nil_apply(
                    &not_nil_var,
                    &recv_var,
                    block_takes_arg,
                ));
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
                    " of <'nil'> when 'true' -> ",
                    // BT-4366: nested in the nil-test `case`, so the apply carries the
                    // send's line itself.
                    self.annotate_current_send_call(docvec!["apply ", leaf::var(nil_var), " ()"]),
                    " <_> when 'true' -> ",
                    apply,
                    " end",
                ];
                Ok(Some(seq.close(self, call_doc, "IfNotNilNilRes")))
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
        // `error:` on a ClassReference is a class method call (e.g. `Result error: reason`),
        // not the Object#error: error-signaling intrinsic. Skip so class method dispatch handles it.
        // Also covers `Logger error: "msg"` — see `WellKnownSelector::Error` rustdoc
        // for the dual-use caveat (Object >> error: vs Logger class >> error:).
        if matches!(receiver, Expression::ClassReference { .. }) {
            return Ok(None);
        }
        // `(Erlang mod) error: arg` / `error:metadata:` is an Erlang FFI call to a
        // function named `error` (e.g. `Console error:` delegating to `beamtalk_console:error/1`),
        // not the Object#error: error-signaling intrinsic. Skip so the FFI proxy handles it.
        if Self::is_erlang_ffi_receiver(receiver) {
            return Ok(None);
        }
        // `error:` is well-known; dispatch via the enum.
        if !matches!(selector.well_known(), Some(WellKnownSelector::Error)) {
            return Ok(None);
        }
        debug_assert_eq!(arguments.len(), 1);
        // BT-4075: the raise is `Object>>error:`, an *instance*-side method. A
        // class object (`cls := Result. cls error: #nope`) with its own
        // class-side `error:` (`Result class>>error:`, `Logger class>>error:`)
        // must receive the send instead, so only a receiver known to be an
        // instance raises unconditionally.
        let receiver_kind = self.error_signal_receiver_kind(receiver);
        // Hoist open-scope receiver + message (e.g. class method self-sends).
        let mut seq = self.sequence_call(&[receiver, &arguments[0]], "Err")?;
        let recv_doc = seq.next();
        let msg_doc = seq.next();
        let recv_var = self.fresh_temp_var("Obj");
        let msg_var = self.fresh_temp_var("Msg");
        let raise_doc = self.object_error_raise_doc(&recv_var, &msg_var);
        // Possibly a class object: one whose class side understands `error:`
        // receives the send; anything else (an instance, or a class object
        // without a class-side `error:`) raises in place, exactly as the
        // intrinsic always has.
        let body_doc = match receiver_kind {
            ErrorSignalReceiver::Instance => raise_doc,
            // A class-method self-send runs where its method runs (ADR 0142),
            // never back through the class process.
            ErrorSignalReceiver::ClassSelf => self.class_side_error_guard(
                "call 'beamtalk_class_dispatch':'class_self_send'(",
                &recv_var,
                &msg_var,
                receiver,
                raise_doc,
            ),
            ErrorSignalReceiver::Unknown => self.class_side_error_guard(
                "call 'beamtalk_message_dispatch':'send'(",
                &recv_var,
                &msg_var,
                receiver,
                raise_doc,
            ),
        };

        let call_doc = docvec![
            "let ",
            leaf::var(recv_var),
            " = ",
            recv_doc,
            " in let ",
            leaf::var(msg_var),
            " = ",
            msg_doc,
            " in ",
            body_doc,
        ];
        Ok(Some(seq.close(self, call_doc, "ErrRes")))
    }

    /// The `Object>>error:` raise: a `user_error` naming the receiver's class,
    /// carrying `msg_var` as its message. The one lowering of the raise, shared
    /// by the call-site intrinsic above and by the compiled `Object>>error:`
    /// method body (`IntrinsicBody::Error`), which generic dispatch reaches
    /// (`perform:`, and a class object without a class-side `error:` through
    /// the class-object protocol fallthrough, BT-4417).
    pub(in crate::core_erlang) fn object_error_raise_doc(
        &mut self,
        recv_var: &str,
        msg_var: &str,
    ) -> Document<'static> {
        let class_var = self.fresh_temp_var("Class");
        let err0 = self.fresh_temp_var("Err");
        let err1 = self.fresh_temp_var("Err");
        docvec![
            "let ",
            leaf::var(class_var.clone()),
            " = call 'beamtalk_primitive':'class_of'(",
            leaf::var(recv_var.to_string()),
            ") in let ",
            leaf::var(err0.clone()),
            " = call 'beamtalk_error':'new'('user_error', ",
            leaf::var(class_var),
            ") in let ",
            leaf::var(err1.clone()),
            " = call 'beamtalk_error':'with_message'(",
            leaf::var(err0),
            ", ",
            leaf::var(msg_var.to_string()),
            ") in call 'beamtalk_error':'raise'(",
            leaf::var(err1),
            ")",
        ]
    }

    /// BT-4075: `case class_object_understands(Recv, 'error:') of 'true' ->
    /// <send_fn>(Recv, 'error:', [Msg]) 'false' -> <raise_doc> end` — the
    /// class-side `error:` send when the receiver is a class object that
    /// defines one, the `Object>>error:` raise otherwise.
    fn class_side_error_guard(
        &mut self,
        send_fn: &'static str,
        recv_var: &str,
        msg_var: &str,
        receiver: &Expression,
        raise_doc: Document<'static>,
    ) -> Document<'static> {
        let send_doc = self.annotate_call_node_with_line(
            docvec![
                send_fn,
                leaf::var(recv_var.to_string()),
                ", ",
                leaf::atom("error:"),
                ", [",
                leaf::var(msg_var.to_string()),
                "])"
            ],
            receiver.span(),
        );
        docvec![
            "case call 'beamtalk_class_dispatch':'class_object_understands'(",
            leaf::var(recv_var.to_string()),
            ", ",
            leaf::atom("error:"),
            ") of 'true' when 'true' -> ",
            send_doc,
            " 'false' when 'true' -> ",
            raise_doc,
            " end",
        ]
    }

    /// BT-4075: what `receiver` is statically known to be for an `error:`
    /// send — see [`ErrorSignalReceiver`]. Starts from the shared syntactic
    /// rule ([`beamtalk_core::ast::is_error_signal_instance_receiver`]: only
    /// `self` can be an instance) and refines it with the method context.
    /// Every codegen consumer that treats `error:` as never returning (the
    /// actor method-body `ErrorSend` classification) asks this.
    pub(in crate::core_erlang) fn error_signal_receiver_kind(
        &self,
        receiver: &Expression,
    ) -> ErrorSignalReceiver {
        if !beamtalk_core::ast::is_error_signal_instance_receiver(receiver) {
            return ErrorSignalReceiver::Unknown;
        }
        if !super::super::expr_shape::is_self_identifier(receiver) {
            // A literal: never a class object.
            return ErrorSignalReceiver::Instance;
        }
        // A top-level REPL `self` is whatever the eval binds it to (the
        // Inspector's `evaluate:` binds the inspected value, possibly a class).
        if self.lookup_var("self").is_none() && self.context == CodeGenContext::Repl {
            return ErrorSignalReceiver::Unknown;
        }
        // `self` is the class object in a class method (including a
        // `ClassBuilder` class-method block), an instance everywhere else.
        if self.in_class_method() || self.builder_class_method_class().is_some() {
            ErrorSignalReceiver::ClassSelf
        } else {
            ErrorSignalReceiver::Instance
        }
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
        // `hash` (well-known) — dispatch via the enum so the
        // classifier validates kind/arity.
        if matches!(selector.well_known(), Some(WellKnownSelector::Hash)) {
            debug_assert!(arguments.is_empty());
            // Hoist open-scope receiver (e.g. class method self-send).
            let mut seq = self.sequence_call(&[receiver], "Obj")?;
            let recv_doc = seq.next();
            let recv_var = self.fresh_temp_var("Obj");
            let call_doc = docvec![
                "let ",
                leaf::var(recv_var.clone()),
                " = ",
                recv_doc,
                " in ",
                primitives::phash2_doc(recv_var),
            ];
            return Ok(Some(seq.close(self, call_doc, "HashRes")));
        }
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "yourself" if arguments.is_empty() => {
                    // Identity: just return the receiver. It is bound rather
                    // than returned bare: the receiver's own document is
                    // already line-annotated (`( X -| [Line, File] )`), and
                    // the enclosing send annotates this result again, which
                    // Core Erlang rejects as a directly nested annotation.
                    let mut seq = self.sequence_call(&[receiver], "Yourself")?;
                    let recv_doc = seq.next();
                    let recv_var = self.fresh_temp_var("Yourself");
                    let identity_doc = docvec![
                        "let ",
                        leaf::var(recv_var.clone()),
                        " = ",
                        recv_doc,
                        " in ",
                        leaf::var(recv_var),
                    ];
                    Ok(Some(seq.close(self, identity_doc, "YourselfRes")))
                }
                // printString removed as intrinsic — now uses polymorphic
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
    /// - `fieldNames` — Get list of instance variable names (empty for primitive values)
    /// - `fieldAt:` — Read instance variable by name
    /// - `fieldAt:put:` — Write instance variable by name
    /// - `hasField:` — Presence test for a named field (ADR 0124 §1/B4); never raises
    /// - `clearField:` — Return a `late` field to unassigned (ADR 0124 §1/B4)
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
        // `fieldNames` (well-known unary). The classifier validates
        // kind/arity, so explicit `arguments.is_empty()` becomes a `debug_assert!`.
        if matches!(selector.well_known(), Some(WellKnownSelector::FieldNames)) {
            debug_assert!(arguments.is_empty());
            // Fast-path for `self` receiver in actor context.
            // `Self` is a `#beamtalk_object{..., pid: self()}` tuple, so the
            // normal is_tuple branch would call sync_send(self()) which is
            // gen_server:call(self(), ...) → deadlock.
            // Use beamtalk_primitive:send(State, ...) directly instead.
            if let Expression::Identifier(id) = receiver
                && id.name == "self"
                && self.context == super::super::CodeGenContext::Actor
                && self.lookup_var("self").is_none()
            {
                let doc = docvec![
                    "call 'beamtalk_primitive':'send'(",
                    leaf::var(self.current_state_var()),
                    ", 'fieldNames', [])",
                ];
                return Ok(Some(doc));
            }

            // Hoist open-scope receiver (e.g. class method self-send).
            let mut seq = self.sequence_call(&[receiver], "FNames")?;
            let recv_doc = seq.next();
            let receiver_var = self.fresh_var("Receiver");

            // Every receiver kind goes through
            // `beamtalk_message_dispatch:send/3`, the same router the dynamic
            // `perform: #fieldNames` path uses, so the two cannot disagree:
            // its `classify_receiver` sends an actor `sync_send` (ADR 0043),
            // a class object `class_send` (BT-4470), and every other value —
            // maps (value objects, tagged stdlib maps) and raw BEAM terms
            // (Integer, Binary, Bitstring, List, ...) — `beamtalk_primitive:send`.
            // Guessing "not a map ⇒ actor" here sent `element(4, R)` a
            // non-tuple and raised `badarg` (BT-4264). The call is nested in
            // a `let`, so it carries its own line annotation (BT-4356).
            let dispatch_call = self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(receiver_var.clone()),
                ", 'fieldNames', [])",
            ]);
            let call_doc = docvec![
                "let ",
                leaf::var(receiver_var),
                " = ",
                recv_doc,
                " in ",
                dispatch_call,
            ];
            return Ok(Some(seq.close(self, call_doc, "FNamesRes")));
        }
        match selector {
            MessageSelector::Keyword(_) => {
                // `respondsTo:` is well-known; dispatch via the enum first so arity
                // correctness is guaranteed by the classifier.
                if matches!(selector.well_known(), Some(WellKnownSelector::RespondsTo)) {
                    debug_assert_eq!(arguments.len(), 1);
                    // Hoist open-scope receiver + selector (e.g. class method self-sends).
                    let mut seq = self.sequence_call(&[receiver, &arguments[0]], "RespTo")?;
                    let recv_doc = seq.next();
                    let sel_doc = seq.next();
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
                    return Ok(Some(seq.close(self, call_doc, "RespToRes")));
                }

                // `fieldAt:` and `fieldAt:put:` route via the enum so
                // the classifier validates kind/arity (`fieldAt:put:` requires
                // exactly two keyword parts, not a flattened single part).
                match selector.well_known() {
                    Some(WellKnownSelector::FieldAt) => {
                        debug_assert_eq!(arguments.len(), 1);
                        // Class-method `self fieldAt: k` reads the class
                        // variable in place, like `self.x` (BT-4421):
                        // `beamtalk_class_vars:field_at(ClassSelf, Name)`,
                        // which applies the `Cls fieldAt:` read rule
                        // (declared `late`, declared set) at run time, the
                        // name being dynamic. A `class_send` would re-enter
                        // the class's own gen_server (`dispatch_error`).
                        if self.is_class_side_self_reflection(receiver) {
                            let name_var = self.fresh_var("Name");
                            let name_code = self.expression_doc(&arguments[0])?;
                            let doc = docvec![
                                "let ",
                                leaf::var(name_var.clone()),
                                " = ",
                                name_code,
                                " in ",
                                self.class_var_read_helper_call_doc(
                                    class_var_fn::FIELD_AT,
                                    vec![leaf::var(name_var)],
                                ),
                            ];
                            return Ok(Some(doc));
                        }
                        // Fast-path for `self` receiver in actor context.
                        // Avoids sync_send(self()) → gen_server:call(self(), ...) → deadlock.
                        if let Expression::Identifier(id) = receiver
                            && id.name == "self"
                            && self.context == super::super::CodeGenContext::Actor
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

                        // Hoist open-scope receiver + name (e.g. class method self-sends).
                        let mut seq = self.sequence_call(&[receiver, &arguments[0]], "FAt")?;
                        let recv_doc = seq.next();
                        let name_doc = seq.next();
                        let receiver_var = self.fresh_var("Receiver");
                        let name_var = self.fresh_var("Name");
                        let class_var = self.fresh_var("Class");
                        let error_base = self.fresh_var("Err");
                        let error_sel = self.fresh_var("Err");
                        let error_hint = self.fresh_var("Err");
                        // primitive value types (Integer, String, etc.) have no slots;
                        // user-defined value objects are maps and use beamtalk_reflection.
                        let hint = leaf::binary_lit("Value types have no instance variables");

                        // A `#beamtalk_object{}` receiver — an actor instance OR a
                        // class object, which share that shape — goes through
                        // `beamtalk_message_dispatch:send/3`, whose
                        // `classify_receiver` tells them apart (actor →
                        // `sync_send`; class object → `class_send`, reaching
                        // `dispatch_class_method`'s `fieldAt:` clause; BT-4148).
                        // Sending a class object the actor message shape
                        // instead crashed the class gen_server.
                        //
                        // The non-object branch is split:
                        //   - map receiver → delegate to beamtalk_primitive:send (routes
                        //     through dispatch/3 which respects ClassKind::Value vs Object)
                        //   - other (primitive literal) → raise immutable_value inline,
                        //     keeping this intrinsic's own hint (the dynamic path's
                        //     primitive `fieldAt:` arm words it differently).
                        // The dispatch call is nested in a `case`, so it carries its
                        // own line annotation (BT-4356).
                        let dispatch_call = self.annotate_current_send_call(docvec![
                            "call 'beamtalk_message_dispatch':'send'(",
                            leaf::var(receiver_var.clone()),
                            ", 'fieldAt:', [",
                            leaf::var(name_var.clone()),
                            "])",
                        ]);
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
                            "<'true'> when 'true' -> ",
                            dispatch_call,
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
                            ") in ",
                            super::super::errors::beamtalk_error_doc(
                                leaf::var(error_base.clone()),
                                leaf::var(error_base),
                                leaf::var(error_sel.clone()),
                                leaf::var(error_sel),
                                leaf::var(error_hint.clone()),
                                "immutable_value",
                                leaf::var(class_var),
                                Document::Str("'fieldAt:'"),
                                hint,
                                Document::Str(" "),
                            ),
                            "call 'beamtalk_error':'raise'(",
                            leaf::var(error_hint),
                            ") end end",
                        ];
                        Ok(Some(seq.close(self, call_doc, "FAtRes")))
                    }
                    Some(WellKnownSelector::FieldAtPut) => {
                        debug_assert_eq!(arguments.len(), 2);
                        // Class-method `self fieldAt: k put: v` writes the
                        // class variable in place, like `self.x := v`
                        // (BT-4421): `beamtalk_class_vars:field_at_put`,
                        // `put/3` after the `Cls fieldAt:put:` declared-set
                        // check. Not a state-threaded write
                        // (`is_self_field_at_put` excludes class methods,
                        // ADR 0130); its order against sibling operands is
                        // pinned by `is_class_side_effect`.
                        if self.is_class_side_self_reflection(receiver) {
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
                                " in ",
                                Self::class_var_helper_call_doc(
                                    class_var_fn::FIELD_AT_PUT,
                                    vec![leaf::var(name_var), leaf::var(value_var)],
                                ),
                            ];
                            return Ok(Some(doc));
                        }
                        // Fast-path for `self` receiver in actor context.
                        // Avoids sync_send(self()) → gen_server:call(self(), ...) → deadlock.
                        //
                        // Method-body-level state threading is handled by
                        // lower_body_exprs_with_reply via is_self_field_at_put/
                        // generate_self_field_at_put_open, which intercepts before
                        // expression_doc is called. This intrinsic path is a
                        // deadlock-avoidance fallback for contexts where method-body
                        // threading is unavailable (e.g., inside blocks). It delegates
                        // to beamtalk_primitive:send(State, 'fieldAt:put:', ...) which
                        // returns the assigned value but does NOT thread the updated
                        // state back into the actor's State variable.
                        if let Expression::Identifier(id) = receiver
                            && id.name == "self"
                            && self.context == super::super::CodeGenContext::Actor
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

                        // Hoist open-scope receiver + name + value (e.g. class method self-sends).
                        let mut seq = self
                            .sequence_call(&[receiver, &arguments[0], &arguments[1]], "FAtPut")?;
                        let recv_doc = seq.next();
                        let name_doc = seq.next();
                        let value_doc = seq.next();
                        let receiver_var = self.fresh_var("Receiver");
                        let name_var = self.fresh_var("Name");
                        let value_var = self.fresh_var("Value");
                        let class_var = self.fresh_var("Class");
                        let error_base = self.fresh_var("Err");
                        let error_sel = self.fresh_var("Err");
                        let error_hint = self.fresh_var("Err");
                        let hint =
                            leaf::binary_lit(super::super::errors::VALUE_IMMUTABLE_SLOT_HINT);

                        // As `fieldAt:` above: a `#beamtalk_object{}` receiver
                        // (actor instance or class object) goes through
                        // `beamtalk_message_dispatch:send/3` (BT-4148); anything
                        // else raises `immutable_value` inline. The dispatch call
                        // carries its own line annotation (BT-4356).
                        let dispatch_call = self.annotate_current_send_call(docvec![
                            "call 'beamtalk_message_dispatch':'send'(",
                            leaf::var(receiver_var.clone()),
                            ", 'fieldAt:put:', [",
                            leaf::var(name_var.clone()),
                            ", ",
                            leaf::var(value_var.clone()),
                            "])",
                        ]);
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
                            "<'true'> when 'true' -> ",
                            dispatch_call,
                            " <_> when 'true' -> let ",
                            leaf::var(class_var.clone()),
                            " = call 'beamtalk_primitive':'class_of'(",
                            leaf::var(receiver_var),
                            ") in ",
                            super::super::errors::beamtalk_error_doc(
                                leaf::var(error_base.clone()),
                                leaf::var(error_base),
                                leaf::var(error_sel.clone()),
                                leaf::var(error_sel),
                                leaf::var(error_hint.clone()),
                                "immutable_value",
                                leaf::var(class_var),
                                Document::Str("'fieldAt:put:'"),
                                hint,
                                Document::Str(" "),
                            ),
                            "call 'beamtalk_error':'raise'(",
                            leaf::var(error_hint),
                            ") end",
                        ];
                        Ok(Some(seq.close(self, call_doc, "FAtPutRes")))
                    }
                    // ADR 0124 §1/§9/B4: `hasField:` — a presence test that
                    // never raises. Routed via the enum so the classifier
                    // validates arity.
                    Some(WellKnownSelector::HasField) => {
                        debug_assert_eq!(arguments.len(), 1);
                        // Class-method `self hasField: #x` is a presence test
                        // on the class variables, in place (ADR 0130 §2):
                        // `beamtalk_class_vars:has(ClassSelf, Name)`, which
                        // never raises on the name.
                        if self.is_class_side_self_reflection(receiver) {
                            let name_var = self.fresh_var("Name");
                            let name_code = self.expression_doc(&arguments[0])?;
                            let doc = docvec![
                                "let ",
                                leaf::var(name_var.clone()),
                                " = ",
                                name_code,
                                " in ",
                                self.class_var_read_helper_call_doc(
                                    class_var_fn::HAS,
                                    vec![leaf::var(name_var)],
                                ),
                            ];
                            return Ok(Some(doc));
                        }
                        // Fast-path for `self` receiver in actor instance
                        // context. Avoids sync_send(self()) → deadlock.
                        if let Expression::Identifier(id) = receiver
                            && id.name == "self"
                            && self.context == super::super::CodeGenContext::Actor
                            && !self.in_class_method()
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
                                ", 'hasField:', [",
                                leaf::var(name_var),
                                "])",
                            ];
                            return Ok(Some(doc));
                        }

                        // General dispatch: delegate to
                        // `beamtalk_message_dispatch:send/3`, which already
                        // classifies the receiver correctly (actor instance
                        // → `sync_send`; class object → `beamtalk_object_class:class_send`,
                        // reaching this issue's `dispatch_class_method`
                        // `'hasField:'` clause; value/tagged-map/primitive
                        // → `beamtalk_primitive:send`, reaching the
                        // `hasField:` arm every class's `dispatch/3` now
                        // carries). A class object is also
                        // `#beamtalk_object{}`-shaped, so treating it as an
                        // actor instance would send it the wrong message
                        // shape and crash the class gen_server;
                        // `hasField:`/`clearField:` reuse the SAME general
                        // dispatcher `perform:` already does (as
                        // `fieldAt:`/`fieldAt:put:` do for their
                        // `#beamtalk_object{}` receivers, BT-4148), so
                        // `classify_receiver`'s actor/class/value
                        // distinction lives in exactly one place.
                        let mut seq = self.sequence_call(&[receiver, &arguments[0]], "HasF")?;
                        let recv_doc = seq.next();
                        let name_doc = seq.next();
                        let call_doc = docvec![
                            "call 'beamtalk_message_dispatch':'send'(",
                            recv_doc,
                            ", 'hasField:', [",
                            name_doc,
                            "])",
                        ];
                        Ok(Some(seq.close(self, call_doc, "HasFRes")))
                    }
                    // ADR 0124 §1/§4f/B4: `clearField:` — returns a `late`
                    // slot to unassigned. A write; lowers exactly as
                    // `fieldAt:put:` does.
                    Some(WellKnownSelector::ClearField) => {
                        debug_assert_eq!(arguments.len(), 1);
                        // Class-side `self clearField: #x` removes the class
                        // variable in place at any nesting depth (ADR 0130
                        // §2): `beamtalk_class_vars:clear(ClassSelf, Name)`,
                        // which answers `self`. Routing it through
                        // `class_send` instead would deadlock on the class's
                        // own process.
                        if self.is_class_side_self_reflection(receiver) {
                            let name_var = self.fresh_var("Name");
                            let name_code = self.expression_doc(&arguments[0])?;
                            let doc = docvec![
                                "let ",
                                leaf::var(name_var.clone()),
                                " = ",
                                name_code,
                                " in ",
                                Self::class_var_helper_call_doc(
                                    class_var_fn::CLEAR,
                                    vec![leaf::var(name_var)],
                                ),
                            ];
                            return Ok(Some(doc));
                        }
                        // Fast-path for `self` receiver in actor instance
                        // context. Avoids sync_send(self()) → deadlock.
                        // Method-body-level state threading is handled by
                        // `lower_body_exprs_with_reply`/`is_self_clear_field`/
                        // `generate_self_clear_field_open`, which intercepts
                        // before `expression_doc` is called. This intrinsic
                        // path is a deadlock-avoidance fallback for contexts
                        // where method-body threading is unavailable (e.g.,
                        // inside blocks) — mirrors `fieldAt:put:` above. It
                        // delegates to `beamtalk_primitive:send(State,
                        // 'clearField:', ...)`, the same pre-existing
                        // state-threading gap fieldAt:put: has (BT-1324):
                        // it does NOT thread the updated state back into the
                        // actor's `State` variable.
                        if let Expression::Identifier(id) = receiver
                            && id.name == "self"
                            && self.context == super::super::CodeGenContext::Actor
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
                                ", 'clearField:', [",
                                leaf::var(name_var),
                                "])",
                            ];
                            return Ok(Some(doc));
                        }

                        // General dispatch: delegate to
                        // `beamtalk_message_dispatch:send/3`, exactly like
                        // `hasField:` above and for the identical reason —
                        // a hand-rolled receiver check cannot tell a
                        // class-object `#beamtalk_object{}` apart from an
                        // actor-instance one, so it would send a live class
                        // gen_server the wrong message shape and crash it.
                        // A value/primitive receiver reaches the class's
                        // own `dispatch/3`, whose `clearField:`
                        // arm always raises `immutable_value` (added
                        // unconditionally, not only for `ClassKind::Value`
                        // — see `generate_primitive_dispatch`), so no
                        // hand-rolled error construction is needed here
                        // either.
                        let mut seq = self.sequence_call(&[receiver, &arguments[0]], "ClrF")?;
                        let recv_doc = seq.next();
                        let name_doc = seq.next();
                        let call_doc = docvec![
                            "call 'beamtalk_message_dispatch':'send'(",
                            recv_doc,
                            ", 'clearField:', [",
                            name_doc,
                            "])",
                        ];
                        Ok(Some(seq.close(self, call_doc, "ClrFRes")))
                    }
                    _ => Ok(None),
                }
            }
            MessageSelector::Unary(_) | MessageSelector::Binary(_) => Ok(None),
        }
    }
}
