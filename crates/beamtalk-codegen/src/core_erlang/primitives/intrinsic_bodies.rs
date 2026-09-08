// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `@primitive` selector lowering and Logger `@intrinsic` body emission.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! [`CoreErlangGenerator::generate_primitive`] lowers a quoted
//! `@primitive "selector"` to an inline BIF call; the Logger intrinsic body
//! helpers here are shared with `ClassBuilder` registration.

use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::primitive_bindings::PrimitiveBindingTable;
use crate::core_erlang::{CodeGenContext, CodeGenError, Result, primitives};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory};

impl CoreErlangGenerator {
    /// Generates code for an `@primitive` expression (ADR 0007 Phase 3).
    ///
    /// For **selector-based** primitives (quoted, e.g., `@primitive "+"`), generates
    /// direct Erlang BIF calls when a known implementation exists. This makes the
    /// compiled stdlib module self-sufficient — no delegation to hand-written
    /// Erlang dispatch modules.
    ///
    /// Falls back to a `does_not_understand` error for selectors with no known
    /// BIF implementation.
    ///
    /// For **structural intrinsics** (bare, e.g., `@primitive blockValue`),
    /// these are handled at the call site by `dispatch_codegen`, not here.
    /// The method body for structural intrinsics is never directly called.
    #[allow(clippy::too_many_lines)] // BT-1763: Erlang interop intrinsics add essential branches
    pub(in crate::core_erlang) fn generate_primitive(
        &mut self,
        name: &str,
        is_quoted: bool,
        span: beamtalk_core::source_analysis::Span,
    ) -> Result<Document<'static>> {
        let class_name = self
            .class_identity()
            .map(|id| id.class_name().to_string())
            .ok_or_else(|| {
                CodeGenError::Internal(format!(
                    "@primitive \"{name}\" used outside of a class context"
                ))
            })?;

        // ADR 0038: ClassBuilder register intrinsic — emits a call to
        // beamtalk_class_builder:register/1 with the builder's gen_server state.
        if !is_quoted && name == "classBuilderRegister" {
            return Ok(self.generate_class_builder_register());
        }

        // BT-1548: basicNew/basicNewWith intrinsics in class method context.
        // When Value defines `class sealed new => @intrinsic basicNew`, the class
        // method body needs to call class_self_new (which routes through handle_new
        // to the target class's auto-generated new/0).
        //
        // BT-3047 / ADR 0109 amendment: ClassName is derived from `ClassSelf`
        // (closure-captured, correct even inside a block executing in a foreign
        // class's process) rather than the process dictionary — the same fix
        // applied to the instantiation intrinsics in `dispatch_codegen.rs`. Module
        // is resolved by name via `beamtalk_class_instantiation:resolve_module_or_raise/2`
        // rather than `element(3, ClassSelf)` (`class_mod`), which is not reliably
        // the calling class's own module for an inherited class method — see that
        // function's doc for why (discovered as a `Value does not understand 'x'`-
        // shaped regression while implementing this amendment).
        if !is_quoted && self.in_class_method() {
            match name {
                "basicNew" => {
                    return Ok(docvec![
                        "call 'beamtalk_class_instantiation':'class_self_new'(",
                        Self::class_self_name_doc(),
                        ", ",
                        Self::class_self_module_doc("new"),
                        ", [])",
                    ]);
                }
                "basicNewWith" => {
                    let param = self
                        .current_method_params
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "InitArgs".to_string());
                    return Ok(docvec![
                        "call 'beamtalk_class_instantiation':'class_self_new'(",
                        Self::class_self_name_doc(),
                        ", ",
                        Self::class_self_module_doc("new:"),
                        ", [",
                        leaf::var(param),
                        "])",
                    ]);
                }
                _ => {}
            }
        }

        // BT-2803 (adversarial review): `blockValueWithArguments`'s compiled
        // method body is real, not a placeholder — unlike `blockValue`/
        // `blockValue1`/etc. (which truly are call-site-only, since a Tier 2
        // block's extra state argument can only come from a calling method's
        // live `State`/`StateAcc`), a plain `erlang:apply(Self, Args)` is
        // correct for *any* receiver reached via generic runtime dispatch
        // (`beamtalk_primitive:send/3`, `perform:withArguments:`, …) — a
        // Tier 2 block can never correctly reach this path in the first
        // place (see `is_tier2_value_call`'s scoping in
        // `gen_server/methods.rs`), so there's no state to thread here.
        // Restores the exact behaviour `valueWithArguments:`'s `@primitive`
        // form had before being converted to a call-site-intercepted
        // `@intrinsic`, fixing `send_block_valueWithArguments_test_` in
        // `beamtalk_primitive_tests.erl`.
        if !is_quoted && name == "blockValueWithArguments" {
            let args_param = self
                .current_method_params
                .first()
                .cloned()
                .unwrap_or_else(|| "_Args".to_string());
            return Ok(docvec![
                "call 'erlang':'apply'(Self, ",
                leaf::var(args_param),
                ")",
            ]);
        }

        // BT-2812: `blockValue`/`blockValue1`/`blockValue2`/`blockValue3` — Block's
        // `value`/`value:`/`value:value:`/`value:value:value:`. Same gap as
        // `blockValueWithArguments` above, but these can't unconditionally
        // `erlang:apply` (a Tier 2/stateful block needs a live `StateAcc` this
        // generic dispatch site doesn't have — see ADR-0041). See
        // `generate_block_value_structural_fallback` for the Tier 1/Tier 2
        // discrimination.
        //
        // Adversarial review (BT-2812): this match is intentionally exhaustive
        // over today's four `value*` arities, not pattern-derived from them. A
        // hypothetical 5th arity (`blockValue4`) or a change to ADR-0041's
        // `Args..., StateAcc` shape (extra state args, different position)
        // would silently fall through to the unfixed placeholder below rather
        // than fail to compile — there's no static check tying this arm list
        // to `STRUCTURAL_INTRINSICS` or to the block.bt method declarations.
        if !is_quoted {
            let block_value_info = match name {
                "blockValue" => Some((0usize, "value")),
                "blockValue1" => Some((1usize, "value:")),
                "blockValue2" => Some((2usize, "value:value:")),
                "blockValue3" => Some((3usize, "value:value:value:")),
                _ => None,
            };
            if let Some((arity, real_selector)) = block_value_info {
                return Ok(self.generate_block_value_structural_fallback(
                    name,
                    arity,
                    real_selector,
                    &class_name,
                ));
            }
        }

        // BT-2908: `whileTrue`/`whileFalse`/`repeat`/`onDo`/`ensure` — Block's
        // `whileTrue:`/`whileFalse:`/`repeat`/`on:do:`/`ensure:`. The other half
        // of the gap BT-2812's audit found but deliberately left unfixed above
        // (see the BT-2812 comment on this match's sibling below): unlike
        // `value*`, these need real loop/exception-handling semantics, not a
        // bare `erlang:apply`. Feasible generically for the Tier 1 (pure) case
        // because Core Erlang's `case`/`try`/`catch` mechanics don't themselves
        // need the block's AST — only ADR-0041's state-threading convention
        // does, and a Tier 2 (stateful) receiver/argument raises the same
        // `stateful_block_dispatch` error `generate_block_value_structural_fallback`
        // established rather than being reimplemented generically.
        //
        // Adversarial review (BT-2908): like BT-2812's `value*` match above,
        // the `is_function/2` arity+1 check each of these five functions runs
        // is not statically tied to ADR-0041's `Args..., StateAcc` convention
        // — a future change to that shape (extra state args, different
        // position) would silently degrade Tier 2 detection here too, with no
        // compile-time signal.
        if !is_quoted {
            match name {
                "whileTrue" => {
                    return Ok(self.generate_while_structural_fallback(
                        name,
                        false,
                        "whileTrue:",
                        &class_name,
                    ));
                }
                "whileFalse" => {
                    return Ok(self.generate_while_structural_fallback(
                        name,
                        true,
                        "whileFalse:",
                        &class_name,
                    ));
                }
                "repeat" => {
                    return Ok(self.generate_repeat_structural_fallback(&class_name));
                }
                "onDo" => {
                    return Ok(self.generate_on_do_structural_fallback(&class_name));
                }
                "ensure" => {
                    return Ok(self.generate_ensure_structural_fallback(&class_name));
                }
                _ => {}
            }
        }

        // BT-1763: Erlang interop DNU intrinsics — forward selector/args to
        // the handler module's dispatch/3 rather than passing the intrinsic name.
        // doesNotUnderstand:args: receives (Self, Selector, Args) and we need to
        // forward Selector and Args as the dispatch selector and argument list.
        if !is_quoted {
            match name {
                "erlangApply" => {
                    let params = &self.current_method_params;
                    let selector_param = params
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Selector".to_string());
                    let args_param = params
                        .get(1)
                        .cloned()
                        .unwrap_or_else(|| "Arguments".to_string());
                    return Ok(docvec![
                        "call 'beamtalk_erlang_proxy':'dispatch'(",
                        leaf::var(selector_param),
                        ", ",
                        leaf::var(args_param),
                        ", Self)"
                    ]);
                }
                "erlangModuleLookup" => {
                    let params = &self.current_method_params;
                    let selector_param = params
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Selector".to_string());
                    let args_param = params
                        .get(1)
                        .cloned()
                        .unwrap_or_else(|| "Arguments".to_string());
                    return Ok(docvec![
                        "call 'beamtalk_erlang_class':'dispatch'(",
                        leaf::var(selector_param),
                        ", ",
                        leaf::var(args_param),
                        ", Self)"
                    ]);
                }
                _ => {}
            }
        }

        // BT-1478: Logger intrinsics — generate inline logger:log/3 calls.
        // These are the method bodies for logger.bt's @intrinsic declarations.
        // Direct `Logger warn:` calls are intercepted at the call site by
        // try_generate_logger_intrinsic (which injects the caller's class/selector
        // as metadata). This body path is reached only via indirect dispatch
        // (e.g., perform:), in which case Logger's own class/selector metadata
        // is appropriate.
        if !is_quoted {
            if let Some(doc) = self.try_generate_logger_body_intrinsic(name) {
                return Ok(doc);
            }
        }

        // BT-340: For selector-based primitives, try to emit a direct BIF call
        // instead of delegating through a hand-written dispatch module.
        if is_quoted {
            let params = self.current_method_params.clone();
            if let Some(code) = primitives::generate_primitive_bif(&class_name, name, &params) {
                // BT-2888: `do:`/`collect:`/`select:`/`reject:`/`inject:into:` are
                // real BIF-lowered bodies, already correct for a Tier 1 (pure)
                // block via generic dispatch. Guard against a Tier 2 (stateful)
                // block hitting a raw arity crash instead of a clear error — see
                // `generate_stateful_block_guard`.
                // BT-2913: `doWithKey:` (2-arg-block convention, same shape as
                // `inject:into:`'s block) has the identical gap — extend the
                // same guard. `keysAndValuesDo:` is self-hosted as `self
                // doWithKey: block` (dictionary.bt), so it inherits the fix
                // once `doWithKey:` itself is guarded.
                let stateful_guard_block_param = match name {
                    "do:" | "collect:" | "select:" | "reject:" | "doWithKey:" => params.first(),
                    "inject:into:" => params.get(1),
                    _ => None,
                };
                if let Some(block_param) = stateful_guard_block_param {
                    let pure_arity = if matches!(name, "inject:into:" | "doWithKey:") {
                        2
                    } else {
                        1
                    };
                    return Ok(self.generate_stateful_block_guard(
                        block_param,
                        pure_arity,
                        name,
                        &class_name,
                        code,
                    ));
                }
                return Ok(code);
            }

            // BT-2233: An unmapped quoted @primitive in a stdlib value-type class
            // is a bug — it would silently fall back to the runtime-dispatch path
            // below and raise does_not_understand at runtime (the BT-2232
            // regression). Fail the build instead. The check is scoped so it has
            // no false positives:
            //  - Stdlib mode only. User/FFI @primitive (via --allow-primitives)
            //    keeps BT-938's warn-and-fallback behavior for runtime dispatch.
            //  - Value-type context only. Actor classes legitimately route
            //    unmapped quoted primitives/intrinsics through their
            //    hand-written `beamtalk_X:dispatch` module (e.g. Actor's
            //    `actorPid`, now a quoted `@intrinsic`).
            //  - Excluding a small set of call-site-intercepted reflective /
            //    identity / dynamic operations whose method body is only a
            //    runtime-dispatch placeholder (see
            //    `primitives::is_runtime_dispatched_primitive`).
            if self.stdlib_mode()
                && self.context == CodeGenContext::ValueType
                && !primitives::is_runtime_dispatched_primitive(&class_name, name)
            {
                return Err(CodeGenError::UnmappedPrimitive {
                    class: class_name.clone(),
                    selector: name.to_string(),
                    span: Some(span),
                });
            }
        }

        // Fallback: delegate to runtime dispatch module.
        // This path is used for:
        // - Structural intrinsics (unquoted) — handled at call site, body is placeholder
        // - Selector-based primitives with no known BIF (unimplemented or complex)
        //
        // BT-2803 follow-up: for a structural intrinsic, this placeholder body is
        // never a real implementation of the selector's semantics — it self-calls
        // `<runtime_module>:dispatch(<intrinsic_name_atom>, Args, Self)`, passing
        // the *intrinsic name* (e.g. `blockValue`), not the real selector. Any
        // call path that reaches the compiled method body directly instead of
        // through the call-site interception — e.g.
        // `[42] perform: #value withArguments: #()` — resolves to this
        // placeholder and raises `does_not_understand` for the intrinsic name.
        //
        // BT-2812: Block's `value`/`value:`/`value:value:`/`value:value:value:`/
        // `valueWithArguments:` are now special-cased above and no longer hit this
        // placeholder — those were the concrete repro. BT-2812's audit found the
        // same root cause (wrong dispatch key) also applies, in principle, to
        // every other structural intrinsic without a special case here — e.g.
        // `whileTrue`/`whileFalse`/`repeat`/`onDo`/`ensure` (Block) and
        // `listDo`/`listCollect`/`listSelect`/`listReject`/`listInjectInto`
        // (List/Collection). BT-2888 fixed the List/Collection family (guarding
        // their existing correct Tier 1 bodies against a Tier 2 receiver) and
        // BT-2908 fixed the Block loop/exception-handling family above (a real
        // generic reimplementation for Tier 1, a clear error for Tier 2) — this
        // placeholder now only remains live for primitives with no BIF lowering
        // and no structural special-case, not for any of the originally-audited
        // selectors.
        let runtime_module = PrimitiveBindingTable::runtime_module_for_class(&class_name);

        // BT-938: Validate that the target dispatch module exists in the known stdlib
        // module set. Only check when binding data is available (non-empty binding table).
        // An empty table means no stdlib was loaded, so we skip validation silently.
        if is_quoted && !self.primitive_bindings.is_empty() {
            let known = self.primitive_bindings.known_runtime_modules();
            if !known.contains(&runtime_module) {
                self.add_codegen_warning(
                    Diagnostic::warning(
                        format!(
                            "@primitive \"{name}\" references module '{runtime_module}' which has not been compiled — ensure the class is included in the stdlib build"
                        ),
                        span,
                    )
                    .with_hint(format!("Add the '{runtime_module}' module to the stdlib build, or check the @primitive name for typos"))
                    .with_category(DiagnosticCategory::Type),
                );
            }
        }

        let params_doc = beamtalk_cerl_doc::join(
            self.current_method_params
                .iter()
                .map(|p| leaf::var(p.clone())),
            &Document::Str(", "),
        );
        // BT-677: In class methods, self is bound to ClassSelf, not Self
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        Ok(docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'(",
            leaf::atom(name.to_string()),
            ", [",
            params_doc,
            "], ",
            Document::Str(self_var),
            ")"
        ])
    }

    /// BT-1478: Generates inline `logger:log/3` code for Logger @intrinsic bodies.
    ///
    /// Maps intrinsic names to OTP logger levels:
    /// - `loggerDebug` / `loggerDebugMeta` → `debug`
    /// - `loggerInfo` / `loggerInfoMeta` → `info`
    /// - `loggerWarn` / `loggerWarnMeta` → `warning`
    /// - `loggerError` / `loggerErrorMeta` → `error`
    ///
    /// Returns `None` for non-logger intrinsic names.
    pub(in crate::core_erlang) fn try_generate_logger_body_intrinsic(
        &mut self,
        name: &str,
    ) -> Option<Document<'static>> {
        let (level, has_metadata) = match name {
            "loggerDebug" => ("debug", false),
            "loggerInfo" => ("info", false),
            "loggerWarn" => ("warning", false),
            "loggerError" => ("error", false),
            "loggerDebugMeta" => ("debug", true),
            "loggerInfoMeta" => ("info", true),
            "loggerWarnMeta" => ("warning", true),
            "loggerErrorMeta" => ("error", true),
            _ => return None,
        };

        let params = self.current_method_params.clone();
        let msg_param = params
            .first()
            .cloned()
            .unwrap_or_else(|| "Message".to_string());

        let ctx_class = self.class_name();
        let ctx_selector = self
            .current_method_selector
            .clone()
            .unwrap_or_else(|| "unknown".to_string());

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

        let discard_var = self.fresh_temp_var("LogOk");

        let log_call_doc = if has_metadata {
            let meta_param = params.get(1).cloned().unwrap_or_else(|| "Meta".to_string());
            let merge_var = self.fresh_temp_var("LogMeta");
            docvec![
                "let ",
                leaf::var(merge_var.clone()),
                " = call 'maps':'merge'(",
                leaf::var(meta_param),
                ", ",
                metadata_map_doc,
                ") in call 'logger':'log'(",
                leaf::atom(level.to_string()),
                ", ",
                leaf::var(msg_param),
                ", ",
                leaf::var(merge_var),
                ")"
            ]
        } else {
            docvec![
                "call 'logger':'log'(",
                leaf::atom(level.to_string()),
                ", ",
                leaf::var(msg_param),
                ", ",
                metadata_map_doc,
                ")"
            ]
        };

        let doc = docvec![
            "let ",
            leaf::var(discard_var),
            " = ",
            log_call_doc,
            " in 'nil'"
        ];

        Some(doc)
    }

    /// ADR 0038: Generates code for the `classBuilderRegister` intrinsic.
    ///
    /// Emits a call to `beamtalk_class_builder:register/1` with the builder's
    /// `gen_server` state augmented with the builder's own PID (for cleanup).
    ///
    /// On success: returns the canonical class-object record built by
    /// `beamtalk_class_registry:class_object_from_pid/1`, i.e.
    /// `#beamtalk_object{class = '<Name> class', class_mod = ModuleName, pid = Pid}`
    /// — the same shape produced by `generate_class_reference` and
    /// `beamtalk_interface:handle_class_named/1`, so the value is dispatchable
    /// and `==` to the registry reference (BT-2258).
    /// On error: raises the structured error via `beamtalk_error:raise/1`
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _Pid = call 'erlang':'self'() in
    /// let _BS = call 'maps':'put'('builderPid', _Pid, State) in
    /// case call 'beamtalk_class_builder':'register'(_BS) of
    ///   <{'ok', _CP}> when 'true' ->
    ///     call 'beamtalk_class_registry':'class_object_from_pid'(_CP)
    ///   <{'error', _Err}> when 'true' ->
    ///     call 'beamtalk_error':'raise'(_Err)
    /// end
    /// ```
    pub(in crate::core_erlang) fn generate_class_builder_register(&mut self) -> Document<'static> {
        let pid_var = self.fresh_temp_var("BuilderPid");
        let state_var = self.fresh_temp_var("BuilderState");
        let class_pid_var = self.fresh_temp_var("ClassPid");
        let error_var = self.fresh_temp_var("RegErr");
        let current_state = self.current_state_var();

        docvec![
            "let ",
            leaf::var(pid_var.clone()),
            " = call 'erlang':'self'() in ",
            "let ",
            leaf::var(state_var.clone()),
            " = call 'maps':'put'('builderPid', ",
            leaf::var(pid_var),
            ", ",
            leaf::var(current_state),
            ") in ",
            "case call 'beamtalk_class_builder':'register'(",
            leaf::var(state_var),
            ") of ",
            "<{'ok', ",
            leaf::var(class_pid_var.clone()),
            "}> when 'true' -> ",
            // BT-2258: return the canonical class-object shape
            // {'beamtalk_object', <Name> ++ " class", ModuleName, ClassPid}
            // built by the runtime helper, instead of an unusable hardcoded
            // {'beamtalk_object', 'Class', 'beamtalk_class_bt', ClassPid} wrapper.
            "call 'beamtalk_class_registry':'class_object_from_pid'(",
            leaf::var(class_pid_var),
            ") ",
            "<{'error', ",
            leaf::var(error_var.clone()),
            "}> when 'true' -> ",
            "call 'beamtalk_error':'raise'(",
            leaf::var(error_var),
            ") ",
            "end"
        ]
    }
}
