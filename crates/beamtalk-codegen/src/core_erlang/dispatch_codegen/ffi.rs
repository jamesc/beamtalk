// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Erlang interop sends.
//!
//! Lowers sends to the `Erlang` FFI proxy to direct `call 'module':'function'`
//! expressions, including the proved-direct and runtime-guarded forms.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::exception_forms::{CatchVars, try_catch_doc};
use crate::core_erlang::{CoreErlangGenerator, Result};

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MessageSelector};
use beamtalk_core::semantic_analysis::type_checker::{
    FFI_ARG_BINARY_GUARD, FFI_ARG_RUNTIME_GUARDS, ffi_arg_may_be_binary, ffi_arg_passes_unchanged,
};

/// BT-4485: the Core Erlang guard that holds when every one of `args` passes
/// one of the FFI type table's [`FFI_ARG_RUNTIME_GUARDS`]. An argument whose
/// static type may be a `String`/`Binary` (BT-4759, the `bool`) also passes the
/// table's [`FFI_ARG_BINARY_GUARD`]; a scalar-typed argument is checked exactly
/// as before.
fn ffi_arg_runtime_guard(args: &[(String, bool)]) -> Document<'static> {
    args.iter()
        .map(|(name, may_be_binary)| {
            let bifs = FFI_ARG_RUNTIME_GUARDS
                .iter()
                .copied()
                .chain(may_be_binary.then_some(FFI_ARG_BINARY_GUARD));
            bifs.map(|bif| {
                docvec![
                    "call 'erlang':",
                    leaf::atom(bif),
                    "(",
                    leaf::var(name.clone()),
                    ")"
                ]
            })
            .reduce(|acc, test| docvec!["call 'erlang':'or'(", acc, ", ", test, ")"])
            .unwrap_or(Document::Str("'true'"))
        })
        .reduce(|acc, test| docvec!["call 'erlang':'and'(", acc, ", ", test, ")"])
        .unwrap_or(Document::Str("'true'"))
}

impl CoreErlangGenerator {
    /// ADR 0028: Handles `Erlang` class reference for BEAM interop.
    ///
    /// Two cases are handled:
    ///
    /// 1. **Direct call optimization (ADR 0028 Phase 4):** When the
    ///    receiver is `MessageSend(ClassReference("Erlang"), Unary(module))` and
    ///    the outer selector is a function call, emits a direct BEAM call:
    ///    ```erlang
    ///    call 'lists':'reverse'(Xs)
    ///    ```
    ///    This eliminates proxy map allocation entirely.
    ///
    /// 2. **Proxy construction:** When the receiver is
    ///    `ClassReference("Erlang")` and the message is a unary module name,
    ///    generates an inline `ErlangModule` proxy map:
    ///    ```erlang
    ///    ~{'$beamtalk_class' => 'ErlangModule', 'module' => 'lists'}~
    ///    ```
    ///    This fallback handles `proxy := Erlang lists` (standalone proxy).
    ///
    /// Standard class-protocol selectors (e.g. `class`, `new`, `superclass`)
    /// fall through to normal class dispatch so that `Erlang class` returns the
    /// metaclass rather than a proxy for module `'class'`.
    ///
    /// FFI receiver recognition (the class-protocol filter, the
    /// package-qualification check, and parenthesized-receiver peeling) is
    /// centralized in [`beamtalk_core::ffi_receiver`] — this is the only place those
    /// rules are implemented.
    pub(super) fn try_handle_erlang_interop(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // Direct call optimization — `Erlang lists reverse: xs` (and the
        // parenthesized `(Erlang lists) reverse: xs`) → `call 'lists':'reverse'(Xs)`
        // with no proxy map allocation. Only when the module name is a
        // compile-time literal (ClassReference path).
        if let Some(module_name) = beamtalk_core::ffi_receiver::erlang_module_of_receiver(receiver)
        {
            return self.generate_direct_erlang_call(module_name, selector, arguments);
        }

        // Proxy construction — `Erlang lists` → inline proxy map
        if let Expression::ClassReference { name, package, .. } = receiver {
            if package.is_some() || name.name != "Erlang" {
                return Ok(None);
            }
            match selector {
                MessageSelector::Unary(module_name)
                    if !beamtalk_core::ffi_receiver::is_class_protocol_selector(module_name) =>
                {
                    let doc = docvec![
                        "~{'$beamtalk_class' => 'ErlangModule', 'module' => ",
                        leaf::atom(module_name.clone()),
                        "}~",
                    ];
                    Ok(Some(doc))
                }
                _ => {
                    // Keyword/binary on Erlang class itself, or a class-protocol
                    // selector, falls through to normal class dispatch.
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }

    /// Generates a proxy-routed call for Erlang interop.
    ///
    /// Converts Beamtalk selectors to Erlang function names and routes through
    /// `beamtalk_erlang_proxy:direct_call/4` for automatic binary→charlist coercion:
    /// - Unary: `node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [], 'stock')` (zero-arg)
    /// - Keyword single: `reverse:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'reverse', [Xs], 'stock')`
    /// - Keyword multi: `seq:with:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'seq', [1, 10], 'stock')`
    ///
    /// The trailing mode atom is the `stock`/`beamtalk` coercion mode,
    /// precomputed from the statically-known module (BT-4153).
    ///
    /// Returns `None` for selectors that are Object/ProtoObject protocol methods
    /// (e.g. `printString`, `asString`) — these must go through runtime dispatch
    /// so the proxy's inherited protocol methods are called, not a non-existent
    /// Erlang function.
    ///
    /// Block literal arguments are generated by
    /// [`Self::generate_erlang_interop_block`]: always Tier 1, since a block
    /// that writes a captured local is a compile error (ADR 0133, BT-3917).
    fn generate_direct_erlang_call(
        &mut self,
        module_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        /// Object protocol selectors that must NOT be optimized as direct Erlang
        /// calls. These are inherited from ProtoObject/Object and handled by
        /// runtime dispatch. Selectors already handled as compiler intrinsics
        /// (class, isNil, notNil, hash, yourself, respondsTo:, error:) never
        /// reach here — they're intercepted earlier in the dispatch chain.
        const OBJECT_PROTOCOL_SELECTORS: &[&str] = &["printString", "asString", "inspect"];

        match selector {
            MessageSelector::Unary(function_name) => {
                if OBJECT_PROTOCOL_SELECTORS.contains(&function_name.as_str()) {
                    return Ok(None);
                }
                if let Some(doc) = self.proved_direct_ffi_call(module_name, function_name, &[])? {
                    return Ok(Some(doc));
                }
                // Route zero-arg calls through proxy (consistent with keyword sends).
                // `Erlang erlang node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [], 'stock')`
                let mode = crate::core_erlang::util::ffi_coercion_mode(module_name);
                let doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'(",
                    leaf::atom(module_name.to_string()),
                    ", ",
                    leaf::atom(function_name.to_string()),
                    ", [], ",
                    leaf::atom(mode),
                    ")"
                ];
                Ok(Some(doc))
            }
            MessageSelector::Keyword(parts) => {
                // Extract function name from first keyword (before the colon)
                let function_name = parts[0].keyword.trim_end_matches(':');

                if let Some(doc) =
                    self.proved_direct_ffi_call(module_name, function_name, arguments)?
                {
                    return Ok(Some(doc));
                }

                // Process arguments individually so Block arguments can be
                // generated as Tier 1 funs (generate_erlang_interop_block) before crossing the
                // Erlang boundary. Non-block arguments pass through unchanged.
                let mut preamble_docs: Vec<Document<'static>> = Vec::new();
                let mut arg_parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
                // BT-4296: the arguments land in a list, which erlc evaluates
                // last-first, so every argument before the last one that may
                // run code is bound to a temp in source order.
                let arg_exprs: Vec<&Expression> = arguments.iter().collect();
                let racing_last = self.last_racing_operand(&arg_exprs);

                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        arg_parts.push(Document::Str(", "));
                    }
                    if let Some(block) = Self::extract_block_literal(arg) {
                        let wrapped_doc = self.generate_erlang_interop_block(block)?;
                        // Bind the wrapper to a temp var to avoid repeating complex exprs.
                        let wrapper_var = self.fresh_temp_var("ErlWrapper");
                        preamble_docs.push(docvec![
                            "let ",
                            leaf::var(wrapper_var.clone()),
                            " = ",
                            wrapped_doc,
                            " in "
                        ]);
                        arg_parts.push(leaf::var(wrapper_var));
                    } else if racing_last.is_some_and(|last| i < last)
                        && !Self::is_trivial_subexpr(arg)
                    {
                        let doc = self.expression_doc(arg)?;
                        let (binding, var) = self.bind_subexpr_to_temp("ErlArg", doc);
                        preamble_docs.push(binding);
                        arg_parts.push(leaf::var(var));
                    } else {
                        arg_parts.push(self.expression_doc(arg)?);
                    }
                }

                // Route through beamtalk_erlang_proxy:direct_call/4 to
                // enable binary→charlist coercion for functions like os:cmd/1.
                // Args are wrapped in a list: call 'proxy':'direct_call'('M','F',[args],Mode)
                let mode = crate::core_erlang::util::ffi_coercion_mode(module_name);
                let call_doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'(",
                    leaf::atom(module_name.to_string()),
                    ", ",
                    leaf::atom(function_name.to_string()),
                    ", [",
                    Document::Vec(arg_parts),
                    "], ",
                    leaf::atom(mode),
                    ")"
                ];

                let doc = if preamble_docs.is_empty() {
                    call_doc
                } else {
                    docvec![Document::Vec(preamble_docs), call_doc]
                };

                Ok(Some(doc))
            }
            MessageSelector::Binary(_) => {
                // Binary operators on Erlang module proxy — fall through to runtime
                Ok(None)
            }
        }
    }

    /// BT-4485: the direct remote call for an inline FFI send whose result the
    /// imported spec proves needs no coercion and whose arguments are proved
    /// to reach the callee unchanged — `None` keeps the proxy.
    ///
    /// Both proofs are the FFI type table's
    /// ([`NativeTypeRegistry::proves_no_result_coercion`] and
    /// [`ffi_arg_passes_unchanged`] on each argument's checked type); this
    /// only asks them. Static types are not enforced at run time, so each
    /// argument is also checked with the table's
    /// [`FFI_ARG_RUNTIME_GUARDS`] and the call falls back to
    /// `direct_call/4` when one fails. The call keeps the proxy's error
    /// translation: a raise is handed, with the arguments and the call
    /// site's frame (read only on that path), to
    /// `beamtalk_erlang_proxy:ffi_call_raised/8`, which runs the same
    /// classifier (and `badarg` charlist retry) as `direct_call/4`.
    /// Arguments are bound before the `try`, so a raise while evaluating one
    /// is not translated, as on the proxy path.
    ///
    /// ```text
    /// let <A1> = <arg1> in
    /// case <> of
    ///   <> when is_number(A1) or is_atom(A1) or is_pid(A1) ->
    ///     try call 'M':'F'(A1)
    ///     of <R> -> R
    ///     catch <C, E, Raw> ->
    ///       let S = primop 'build_stacktrace'(Raw) in
    ///       let Site = call 'erlang':'process_info'(self(), 'current_stacktrace') in
    ///       call 'beamtalk_erlang_proxy':'ffi_call_raised'(C, E, S, 'M', 'F', [A1], Mode, Site)
    ///       (a `String`/`Binary`-typed argument adds `is_binary` to its guard and a
    ///       `let Snap = beamtalk_class_vars:snapshot()` before the `try`, passed as
    ///       the last argument: `ffi_call_raised/9`)
    ///   <> when 'true' -> call 'beamtalk_erlang_proxy':'direct_call'('M', 'F', [A1], Mode)
    /// end
    /// ```
    ///
    /// [`NativeTypeRegistry::proves_no_result_coercion`]: beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::proves_no_result_coercion
    /// [`ffi_arg_passes_unchanged`]: beamtalk_core::semantic_analysis::type_checker::ffi_arg_passes_unchanged
    /// [`FFI_ARG_RUNTIME_GUARDS`]: beamtalk_core::semantic_analysis::type_checker::FFI_ARG_RUNTIME_GUARDS
    fn proved_direct_ffi_call(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        let Some(registry) = self.native_type_registry.as_ref() else {
            return Ok(None);
        };
        let Ok(arity) = u8::try_from(arguments.len()) else {
            return Ok(None);
        };
        if !registry.proves_no_result_coercion(module_name, function_name, arity) {
            return Ok(None);
        }
        let args_proved = arguments.iter().all(|arg| {
            self.type_map
                .get(arg.span())
                .is_some_and(ffi_arg_passes_unchanged)
        });
        if !args_proved {
            return Ok(None);
        }

        let mut bindings: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        let mut arg_vars: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        let mut arg_names: Vec<(String, bool)> = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            let var = self.fresh_temp_var("FfiArg");
            bindings.push(docvec![
                "let ",
                leaf::var(var.clone()),
                " = ",
                self.expression_doc(arg)?,
                " in "
            ]);
            if i > 0 {
                arg_vars.push(Document::Str(", "));
            }
            arg_vars.push(leaf::var(var.clone()));
            let may_be_binary = self
                .type_map
                .get(arg.span())
                .is_some_and(ffi_arg_may_be_binary);
            arg_names.push((var, may_be_binary));
        }
        // BT-4759: a binary argument lets the `badarg` charlist retry run, and
        // a retry is a fresh attempt (ADR 0130 §4), so the call site keeps the
        // pre-call class-variable snapshot for it, as the proxy does. A call
        // with only scalar arguments never retries and takes none.
        let any_binary = arg_names.iter().any(|(_, b)| *b);
        let snap_var = any_binary.then(|| self.fresh_temp_var("FfiSnap"));
        let module_atom = leaf::atom(module_name.to_string());
        let function_atom = leaf::atom(function_name.to_string());
        let mode_atom = leaf::atom(crate::core_erlang::util::ffi_coercion_mode(module_name));
        let direct = self.ffi_try_direct_doc(
            &module_atom,
            &function_atom,
            &mode_atom,
            &arg_vars,
            snap_var.as_deref(),
        );
        if arg_names.is_empty() {
            return Ok(Some(docvec![Document::Vec(bindings), direct]));
        }
        // Static types are not enforced at run time: check each argument with
        // the table's runtime guards and keep the proxy for anything else.
        let guard = ffi_arg_runtime_guard(&arg_names);
        let snap_binding = match &snap_var {
            Some(snap) => docvec![
                "let ",
                leaf::var(snap.clone()),
                " = call 'beamtalk_class_vars':'snapshot'() in "
            ],
            None => Document::Str(""),
        };
        Ok(Some(docvec![
            Document::Vec(bindings),
            "case <> of <> when ",
            guard,
            " -> ",
            snap_binding,
            direct,
            " <> when 'true' -> call 'beamtalk_erlang_proxy':'direct_call'(",
            module_atom,
            ", ",
            function_atom,
            ", [",
            Document::Vec(arg_vars),
            "], ",
            mode_atom,
            ") end",
        ]))
    }

    /// The `try` of a direct FFI call (see [`Self::proved_direct_ffi_call`]):
    /// the remote call, and on any exception the hand-off to
    /// `beamtalk_erlang_proxy:ffi_call_raised/8` with the built stacktrace and
    /// the call site's frame.
    fn ffi_try_direct_doc(
        &mut self,
        module_atom: &Document<'static>,
        function_atom: &Document<'static>,
        mode_atom: &Document<'static>,
        arg_vars: &[Document<'static>],
        snap_var: Option<&str>,
    ) -> Document<'static> {
        let result_var = self.fresh_temp_var("FfiRes");
        let cls_var = self.fresh_temp_var("FfiCls");
        let err_var = self.fresh_temp_var("FfiErr");
        let stk_var = self.fresh_temp_var("FfiStk");
        let built_var = self.fresh_temp_var("FfiBuiltStk");
        let self_var = self.fresh_temp_var("FfiSelf");
        let site_var = self.fresh_temp_var("FfiSite");
        let catch = CatchVars {
            class: leaf::var(cls_var.clone()),
            reason: leaf::var(err_var.clone()),
            stack: leaf::var(stk_var.clone()),
        };
        let mut raised_args = vec![
            leaf::var(cls_var),
            leaf::var(err_var),
            leaf::var(built_var.clone()),
            module_atom.clone(),
            function_atom.clone(),
            docvec!["[", Document::Vec(arg_vars.to_vec()), "]"],
            mode_atom.clone(),
            leaf::var(site_var.clone()),
        ];
        raised_args.extend(snap_var.map(|snap| leaf::var(snap.to_string())));
        let raised = form::call("beamtalk_erlang_proxy", "ffi_call_raised", raised_args);
        let handler = form::let_in(
            leaf::var(built_var),
            form::primop("build_stacktrace", [leaf::var(stk_var)]),
            form::let_in(
                leaf::var(self_var.clone()),
                form::call("erlang", "self", []),
                form::let_in(
                    leaf::var(site_var),
                    form::call(
                        "erlang",
                        "process_info",
                        [leaf::var(self_var), leaf::atom("current_stacktrace")],
                    ),
                    raised,
                    Layout::Flat,
                ),
                Layout::Flat,
            ),
            Layout::Flat,
        );
        try_catch_doc(
            form::call_atoms(
                module_atom.clone(),
                function_atom.clone(),
                [Document::Vec(arg_vars.to_vec())],
            ),
            leaf::var(result_var),
            catch,
            handler,
            Layout::Flat,
        )
    }
}
