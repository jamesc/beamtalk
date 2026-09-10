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
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};

/// Which compiler-generated body [`CoreErlangGenerator::generate_primitive`]
/// emits for one structural (`!is_quoted`) intrinsic name. [`INTRINSIC_BODIES`]
/// below is keyed from the same name set `STRUCTURAL_INTRINSICS` validates
/// against, and `every_structural_intrinsic_has_a_body_entry` below asserts
/// the two sets match exactly — closing the "a new arity/name is added to
/// the registry but the codegen match is forgotten" gap.
enum IntrinsicBody {
    /// `classBuilderRegister` (ADR 0038).
    ClassBuilderRegister,
    /// `basicNew` — only inside `class`-side method context
    /// (`self.in_class_method()`); [`IntrinsicBody::Placeholder`]'s runtime-
    /// dispatch body otherwise.
    BasicNew,
    /// `basicNewWith` — same context gating as `BasicNew`.
    BasicNewWith,
    /// `blockValueWithArguments` — Block's `valueWithArguments:`.
    BlockValueWithArguments,
    /// `blockValue`/`blockValue1`/`blockValue2`/`blockValue3` — Block's
    /// `value`/`value:`/`value:value:`/`value:value:value:`.
    BlockValue {
        arity: usize,
        real_selector: &'static str,
    },
    /// `whileTrue`/`whileFalse` — Block's `whileTrue:`/`whileFalse:`.
    While {
        negate: bool,
        real_selector: &'static str,
    },
    /// `repeat` — Block's `repeat`.
    Repeat,
    /// `onDo` — Block's `on:do:`.
    OnDo,
    /// `ensure` — Block's `ensure:`.
    Ensure,
    /// `erlangApply` — DNU-forwarding Erlang interop intrinsic.
    ErlangApply,
    /// `erlangModuleLookup` — same shape as `ErlangApply`, different target module.
    ErlangModuleLookup,
    /// No compiler-generated body of its own — this structural intrinsic is
    /// always intercepted at the call site (`dispatch_codegen`), so the
    /// compiled method body is reached only via generic/reflective dispatch
    /// (e.g. `perform:`) and simply re-enters runtime dispatch, exactly like
    /// an unmapped quoted selector.
    Placeholder,
}

/// Data table driving [`CoreErlangGenerator::generate_primitive`]'s
/// structural-intrinsic dispatch — see [`IntrinsicBody`]'s doc.
static INTRINSIC_BODIES: &[(&str, IntrinsicBody)] = &[
    ("classOf", IntrinsicBody::Placeholder),
    ("doesNotUnderstand", IntrinsicBody::Placeholder),
    ("dynamicSend", IntrinsicBody::Placeholder),
    ("dynamicSendWithArgs", IntrinsicBody::Placeholder),
    ("respondsTo", IntrinsicBody::Placeholder),
    ("fieldNames", IntrinsicBody::Placeholder),
    ("fieldAt", IntrinsicBody::Placeholder),
    ("fieldAtPut", IntrinsicBody::Placeholder),
    ("printString", IntrinsicBody::Placeholder),
    ("hash", IntrinsicBody::Placeholder),
    ("conditional", IntrinsicBody::Placeholder),
    ("conditionalTrue", IntrinsicBody::Placeholder),
    ("conditionalFalse", IntrinsicBody::Placeholder),
    ("shortCircuitAnd", IntrinsicBody::Placeholder),
    ("shortCircuitOr", IntrinsicBody::Placeholder),
    ("booleanNot", IntrinsicBody::Placeholder),
    ("basicNew", IntrinsicBody::BasicNew),
    ("basicNewWith", IntrinsicBody::BasicNewWith),
    (
        "blockValue",
        IntrinsicBody::BlockValue {
            arity: 0,
            real_selector: "value",
        },
    ),
    (
        "blockValue1",
        IntrinsicBody::BlockValue {
            arity: 1,
            real_selector: "value:",
        },
    ),
    (
        "blockValue2",
        IntrinsicBody::BlockValue {
            arity: 2,
            real_selector: "value:value:",
        },
    ),
    (
        "blockValue3",
        IntrinsicBody::BlockValue {
            arity: 3,
            real_selector: "value:value:value:",
        },
    ),
    (
        "blockValueWithArguments",
        IntrinsicBody::BlockValueWithArguments,
    ),
    (
        "whileTrue",
        IntrinsicBody::While {
            negate: false,
            real_selector: "whileTrue:",
        },
    ),
    (
        "whileFalse",
        IntrinsicBody::While {
            negate: true,
            real_selector: "whileFalse:",
        },
    ),
    ("repeat", IntrinsicBody::Repeat),
    ("listDo", IntrinsicBody::Placeholder),
    ("listCollect", IntrinsicBody::Placeholder),
    ("listSelect", IntrinsicBody::Placeholder),
    ("listReject", IntrinsicBody::Placeholder),
    ("listInjectInto", IntrinsicBody::Placeholder),
    ("futureAwait", IntrinsicBody::Placeholder),
    ("futureAwaitTimeout", IntrinsicBody::Placeholder),
    ("futureAwaitForever", IntrinsicBody::Placeholder),
    ("onDo", IntrinsicBody::OnDo),
    ("ensure", IntrinsicBody::Ensure),
    ("error", IntrinsicBody::Placeholder),
    ("erlangModuleLookup", IntrinsicBody::ErlangModuleLookup),
    ("erlangApply", IntrinsicBody::ErlangApply),
    ("classBuilderRegister", IntrinsicBody::ClassBuilderRegister),
];

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

        // Every structural (`!is_quoted`) intrinsic name compiles
        // via the `INTRINSIC_BODIES` table — see its doc for the full
        // strategy breakdown, and each `IntrinsicBody` variant's doc for the
        // rationale behind that particular shape.
        if !is_quoted {
            if let Some((_, body)) = INTRINSIC_BODIES.iter().find(|(n, _)| *n == name) {
                return Ok(self.render_intrinsic_body(body, name, &class_name, span));
            }

            // Logger intrinsics — generate inline logger:log/3 calls.
            // These are the method bodies for logger.bt's @intrinsic declarations.
            // Direct `Logger warn:` calls are intercepted at the call site by
            // try_generate_logger_intrinsic (which injects the caller's class/selector
            // as metadata). This body path is reached only via indirect dispatch
            // (e.g., perform:), in which case Logger's own class/selector metadata
            // is appropriate.
            //
            // Not an `INTRINSIC_BODIES` entry: logger names aren't in
            // `STRUCTURAL_INTRINSICS` (a `class`-side `@intrinsic` bypasses
            // `primitive_validator`'s name check — it only walks
            // `class.methods`, not `class.class_methods`), so they're outside
            // the completeness invariant that table enforces.
            if let Some(doc) = self.try_generate_logger_body_intrinsic(name) {
                return Ok(doc);
            }
        }

        // For selector-based primitives, try to emit a direct BIF call
        // instead of delegating through a hand-written dispatch module.
        if is_quoted {
            let params = self.current_method_params.clone();
            if let Some(code) = primitives::generate_primitive_bif(&class_name, name, &params) {
                // `do:`/`collect:`/`select:`/`reject:`/`inject:into:` are
                // real BIF-lowered bodies, already correct for a Tier 1 (pure)
                // block via generic dispatch. Guard against a Tier 2 (stateful)
                // block hitting a raw arity crash instead of a clear error — see
                // `generate_stateful_block_guard`.
                // `doWithKey:` (2-arg-block convention, same shape as
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

            // An unmapped quoted @primitive in a stdlib value-type class
            // is a bug — it would silently fall back to the runtime-dispatch path
            // below and raise does_not_understand at runtime. Fail the build
            // instead. The check is scoped so it has no false positives:
            //  - Stdlib mode only. User/FFI @primitive (via --allow-primitives)
            //    keeps the warn-and-fallback behavior for runtime dispatch.
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

        // Fallback: delegate to runtime dispatch module. Used for a quoted
        // selector-based primitive with no known BIF (unimplemented or
        // complex) — see `generate_primitive_placeholder_body`'s doc for the
        // structural-intrinsic half of this same fallback.
        Ok(self.generate_primitive_placeholder_body(name, &class_name, is_quoted, span))
    }

    /// Dispatches an [`IntrinsicBody`] to its renderer — the single
    /// match `INTRINSIC_BODIES`-driven strategies share.
    #[allow(clippy::too_many_lines)] // one arm per structural-intrinsic body shape, each documented in place
    fn render_intrinsic_body(
        &mut self,
        body: &IntrinsicBody,
        name: &str,
        class_name: &str,
        span: Span,
    ) -> Document<'static> {
        match body {
            IntrinsicBody::ClassBuilderRegister => self.generate_class_builder_register(),

            // basicNew/basicNewWith intrinsics in class method context.
            // When Value defines `class sealed new => @intrinsic basicNew`, the class
            // method body needs to call class_self_new (which routes through handle_new
            // to the target class's auto-generated new/0).
            //
            // ADR 0109 amendment: ClassName is derived from `ClassSelf`
            // (closure-captured, correct even inside a block executing in a foreign
            // class's process) rather than the process dictionary — the same fix
            // applied to the instantiation intrinsics in `dispatch_codegen.rs`. Module
            // is resolved by name via `beamtalk_class_instantiation:resolve_module_or_raise/2`
            // rather than `element(3, ClassSelf)` (`class_mod`), which is not reliably
            // the calling class's own module for an inherited class method — see that
            // function's doc for why (discovered as a `Value does not understand 'x'`-
            // shaped regression while implementing this amendment).
            //
            // Outside class-method context, `basicNew`/`basicNewWith` fall
            // back to the same placeholder body as an ordinary
            // `IntrinsicBody::Placeholder` entry.
            IntrinsicBody::BasicNew => {
                if self.in_class_method() {
                    docvec![
                        "call 'beamtalk_class_instantiation':'class_self_new'(",
                        Self::class_self_name_doc(),
                        ", ",
                        Self::class_self_module_doc("new"),
                        ", [])",
                    ]
                } else {
                    self.generate_primitive_placeholder_body(name, class_name, false, span)
                }
            }
            IntrinsicBody::BasicNewWith => {
                if self.in_class_method() {
                    let param = self
                        .current_method_params
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "InitArgs".to_string());
                    docvec![
                        "call 'beamtalk_class_instantiation':'class_self_new'(",
                        Self::class_self_name_doc(),
                        ", ",
                        Self::class_self_module_doc("new:"),
                        ", [",
                        leaf::var(param),
                        "])",
                    ]
                } else {
                    self.generate_primitive_placeholder_body(name, class_name, false, span)
                }
            }

            // `blockValueWithArguments`'s compiled
            // method body is real, not a placeholder — unlike `blockValue`/
            // `blockValue1`/etc. (which truly are call-site-only, since a Tier 2
            // block's extra state argument can only come from a calling method's
            // live `State`/`StateAcc`), a plain `erlang:apply(Self, Args)` is
            // correct for *any* receiver reached via generic runtime dispatch
            // (`beamtalk_primitive:send/3`, `perform:withArguments:`, …) — a
            // Tier 2 block can never correctly reach this path in the first
            // place (see `is_tier2_value_call`'s scoping in
            // `gen_server/methods.rs`), so there's no state to thread here.
            // Covered by `send_block_valueWithArguments_test_` in
            // `beamtalk_primitive_tests.erl`.
            IntrinsicBody::BlockValueWithArguments => {
                let args_param = self
                    .current_method_params
                    .first()
                    .cloned()
                    .unwrap_or_else(|| "_Args".to_string());
                docvec!["call 'erlang':'apply'(Self, ", leaf::var(args_param), ")",]
            }

            // `blockValue`/`blockValue1`/`blockValue2`/`blockValue3` — Block's
            // `value`/`value:`/`value:value:`/`value:value:value:`. Same gap as
            // `blockValueWithArguments` above, but these can't unconditionally
            // `erlang:apply` (a Tier 2/stateful block needs a live `StateAcc` this
            // generic dispatch site doesn't have — see ADR-0041). See
            // `generate_block_value_structural_fallback` for the Tier 1/Tier 2
            // discrimination.
            IntrinsicBody::BlockValue {
                arity,
                real_selector,
            } => self.generate_block_value_structural_fallback(
                name,
                *arity,
                real_selector,
                class_name,
            ),

            // `whileTrue`/`whileFalse`/`repeat`/`onDo`/`ensure` — Block's
            // `whileTrue:`/`whileFalse:`/`repeat`/`on:do:`/`ensure:`. The other half
            // of the gap an earlier audit found but deliberately left unfixed for
            // `value*` above: unlike `value*`, these need real loop/exception-handling
            // semantics, not a bare `erlang:apply`. Feasible generically for the
            // Tier 1 (pure) case because Core Erlang's `case`/`try`/`catch`
            // mechanics don't themselves need the block's AST — only ADR-0041's
            // state-threading convention does, and a Tier 2 (stateful)
            // receiver/argument raises the same `stateful_block_dispatch` error
            // `generate_block_value_structural_fallback` established rather than
            // being reimplemented generically.
            IntrinsicBody::While {
                negate,
                real_selector,
            } => self.generate_while_structural_fallback(name, *negate, real_selector, class_name),
            IntrinsicBody::Repeat => self.generate_repeat_structural_fallback(class_name),
            IntrinsicBody::OnDo => self.generate_on_do_structural_fallback(class_name),
            IntrinsicBody::Ensure => self.generate_ensure_structural_fallback(class_name),

            // Erlang interop DNU intrinsics — forward selector/args to
            // the handler module's dispatch/3 rather than passing the intrinsic name.
            // doesNotUnderstand:args: receives (Self, Selector, Args) and we need to
            // forward Selector and Args as the dispatch selector and argument list.
            IntrinsicBody::ErlangApply => {
                let params = &self.current_method_params;
                let selector_param = params
                    .first()
                    .cloned()
                    .unwrap_or_else(|| "Selector".to_string());
                let args_param = params
                    .get(1)
                    .cloned()
                    .unwrap_or_else(|| "Arguments".to_string());
                docvec![
                    "call 'beamtalk_erlang_proxy':'dispatch'(",
                    leaf::var(selector_param),
                    ", ",
                    leaf::var(args_param),
                    ", Self)"
                ]
            }
            IntrinsicBody::ErlangModuleLookup => {
                let params = &self.current_method_params;
                let selector_param = params
                    .first()
                    .cloned()
                    .unwrap_or_else(|| "Selector".to_string());
                let args_param = params
                    .get(1)
                    .cloned()
                    .unwrap_or_else(|| "Arguments".to_string());
                docvec![
                    "call 'beamtalk_erlang_class':'dispatch'(",
                    leaf::var(selector_param),
                    ", ",
                    leaf::var(args_param),
                    ", Self)"
                ]
            }

            IntrinsicBody::Placeholder => {
                self.generate_primitive_placeholder_body(name, class_name, false, span)
            }
        }
    }

    /// Fallback body shared by every structural intrinsic with no
    /// compiler-generated body of its own (`IntrinsicBody::Placeholder`, and
    /// `BasicNew`/`BasicNewWith` outside class-method context) and by a
    /// quoted selector-based primitive with no known BIF lowering —
    /// delegates to the class's runtime dispatch module.
    ///
    /// For a structural intrinsic, this placeholder body is
    /// never a real implementation of the selector's semantics — it self-calls
    /// `<runtime_module>:dispatch(<intrinsic_name_atom>, Args, Self)`, passing
    /// the *intrinsic name* (e.g. `blockValue`), not the real selector. Any
    /// call path that reaches the compiled method body directly instead of
    /// through the call-site interception — e.g.
    /// `[42] perform: #value withArguments: #()` — resolves to this
    /// placeholder and raises `does_not_understand` for the intrinsic name.
    ///
    /// [`IntrinsicBody`]'s variants short-circuit before reaching here for
    /// the concrete cases that would otherwise miscompile through this
    /// placeholder (`value*`, the Block loop/exception family, and
    /// List/Collection).
    fn generate_primitive_placeholder_body(
        &mut self,
        name: &str,
        class_name: &str,
        is_quoted: bool,
        span: Span,
    ) -> Document<'static> {
        let runtime_module = PrimitiveBindingTable::runtime_module_for_class(class_name);

        // Validate that the target dispatch module exists in the known stdlib
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
        // In class methods, self is bound to ClassSelf, not Self
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'(",
            leaf::atom(name.to_string()),
            ", [",
            params_doc,
            "], ",
            Document::Str(self_var),
            ")"
        ]
    }

    /// Generates inline `logger:log/3` code for Logger @intrinsic bodies.
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
    /// and `==` to the registry reference.
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
            // Return the canonical class-object shape
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

#[cfg(test)]
mod intrinsic_bodies_tests {
    use super::INTRINSIC_BODIES;
    use beamtalk_core::semantic_analysis::primitive_validator::STRUCTURAL_INTRINSICS;
    use std::collections::HashSet;

    /// Closes the "a fifth `blockValue` arity (or any other new
    /// structural intrinsic) is added to `STRUCTURAL_INTRINSICS` but the
    /// codegen body table is forgotten" hole by construction — the two name
    /// sets must match exactly, in both directions, or this fails.
    #[test]
    fn every_structural_intrinsic_has_a_body_entry() {
        let registry: HashSet<&str> = STRUCTURAL_INTRINSICS.iter().copied().collect();
        let table: HashSet<&str> = INTRINSIC_BODIES.iter().map(|(name, _)| *name).collect();

        let missing_from_table: Vec<&str> = registry.difference(&table).copied().collect();
        assert!(
            missing_from_table.is_empty(),
            "STRUCTURAL_INTRINSICS name(s) with no INTRINSIC_BODIES entry: {missing_from_table:?}"
        );

        let stale_in_table: Vec<&str> = table.difference(&registry).copied().collect();
        assert!(
            stale_in_table.is_empty(),
            "INTRINSIC_BODIES entry name(s) no longer in STRUCTURAL_INTRINSICS: {stale_in_table:?}"
        );
    }

    /// The completeness test above de-duplicates through a `HashSet`, which
    /// would silently absorb a second entry for the same name — checked
    /// separately here so a duplicate (permanently dead: `Iterator::find`
    /// only ever reaches the first match) fails loudly instead of just
    /// passing the set-equality check above.
    #[test]
    fn no_duplicate_names() {
        let mut seen = HashSet::new();
        for (name, _) in INTRINSIC_BODIES {
            assert!(
                seen.insert(*name),
                "duplicate INTRINSIC_BODIES entry: {name:?}"
            );
        }
    }
}
