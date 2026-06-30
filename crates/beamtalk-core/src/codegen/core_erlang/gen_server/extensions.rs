// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Foreign cross-class extension method registration (ADR 0066, BT-2250).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Beamtalk lets a file add methods to a class declared elsewhere via
//! `TargetClass >> selector => body` (an "open class extension"). When the
//! target class is declared in the *same* file ("self-extension") the method is
//! folded into the host class. When the target class is **foreign** (declared in
//! stdlib or another file — e.g. `Array >> doubled`, `String >> shout`), the
//! method has no host module to live in. This module emits load-time
//! `beamtalk_extensions:register/5` calls — carrying a callable fun and the
//! method body source — so foreign extensions reach the runtime registry and
//! become invocable through the normal dispatch path
//! (`beamtalk_extensions:lookup`).
//!
//! ## Calling convention
//!
//! The runtime invokes the registered fun differently depending on the
//! receiver's kind (see `beamtalk_dispatch:invoke_extension/4` and the
//! generated value-type / actor `dispatch` arms):
//!
//! - **Value-type / primitive receivers** (Array, String, Integer, …):
//!   `fun(Args, Self) -> Result` (2-arity).
//! - **Actor receivers**: `fun(Args, Self, State) -> {Result, NewState}`
//!   (3-arity).
//!
//! The arity is chosen from the (best-effort) class kind of the target class.
//! Foreign targets are usually value/primitive types, so 2-arity is the default;
//! a target known to be an `Actor` subclass gets the 3-arity state-threading
//! shape.

use super::super::document::{Document, INDENT, leaf, line, nest};
use super::super::{CodeGenContext, CoreErlangGenerator, Result};
use crate::ast::{MethodKind, Module, StandaloneMethodDefinition};
use crate::docvec;

impl CoreErlangGenerator {
    /// Returns the foreign cross-class extension methods declared in `module`.
    ///
    /// A standalone method definition (`TargetClass >> sel`) is *foreign* when
    /// its target class is **not** declared in this module — i.e. it extends a
    /// class defined in stdlib or another file. Self-extensions (target class
    /// declared in the same file) are excluded: they are handled by the host
    /// class and must not be re-registered here.
    ///
    /// Only `Primary` methods are returned (abstract/signature-only entries have
    /// no body to register).
    pub(in crate::codegen::core_erlang) fn foreign_extension_methods(
        module: &Module,
    ) -> Vec<&StandaloneMethodDefinition> {
        module
            .method_definitions
            .iter()
            .filter(|md| {
                md.method.kind == MethodKind::Primary
                    && !module
                        .classes
                        .iter()
                        .any(|c| c.name.name == md.class_name.name)
            })
            .collect()
    }

    /// Returns true when `module` declares at least one foreign extension method.
    pub(in crate::codegen::core_erlang) fn has_foreign_extensions(module: &Module) -> bool {
        !Self::foreign_extension_methods(module).is_empty()
    }

    /// Generates the load-time `beamtalk_extensions:register/5` registration
    /// chain for every foreign extension method in `module` (BT-2250).
    ///
    /// Each registration is emitted as an open `let _Ext{N} = <register call> in`
    /// fragment so the calls compose into the surrounding `register_class/0`
    /// `try` body (the trailing expression after the chain is supplied by the
    /// caller). Returns [`Document::Nil`] when there are no foreign extensions.
    ///
    /// The registration carries:
    /// - `Class` — the target class tag atom. For class-side extensions
    ///   (`Target class >> sel`) this is the metaclass tag `'Target class'`
    ///   (note the space — the established display/tag convention).
    /// - `Selector` — the method selector atom.
    /// - `Fun` — a generated callable fun (2-arity for value/primitive targets,
    ///   3-arity for actor targets — see [`Self::generate_extension_fun`]).
    /// - `Owner` — the contributing package atom (ADR 0070).
    /// - `Source` — the method body source binary, or `'undefined'` when the
    ///   source cannot be recovered (graceful degradation — the extension still
    ///   registers, just without source-text navigation support).
    pub(in crate::codegen::core_erlang) fn generate_foreign_extension_registrations(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let extensions = Self::foreign_extension_methods(module);
        if extensions.is_empty() {
            return Ok(Document::Nil);
        }

        let mut parts: Vec<Document<'static>> = Vec::with_capacity(extensions.len());
        for (idx, ext) in extensions.iter().enumerate() {
            parts.push(self.generate_extension_registration(idx, ext)?);
        }
        Ok(Document::Vec(parts))
    }

    /// Generates a single `let _Ext{idx} = beamtalk_extensions:register(...) in`
    /// fragment for one foreign extension method.
    fn generate_extension_registration(
        &mut self,
        idx: usize,
        ext: &StandaloneMethodDefinition,
    ) -> Result<Document<'static>> {
        let class_tag = Self::extension_class_tag(ext);
        let selector = ext.method.selector.name();
        let owner = self.extension_owner(ext);
        let fun_doc = self.generate_extension_fun(ext)?;
        let source_doc = self.extension_source_doc(ext);

        Ok(docvec![
            line(),
            "let ",
            leaf::var(format!("_Ext{idx}")),
            " = call 'beamtalk_extensions':'register'(",
            leaf::atom(class_tag),
            ", ",
            leaf::atom(selector),
            ", ",
            fun_doc,
            ", ",
            leaf::atom(owner),
            ", ",
            source_doc,
            ") in",
        ])
    }

    /// Computes the extension registry class tag for `ext`.
    ///
    /// Instance-side extensions register against the bare class name
    /// (`'Array'`). Class-side extensions (`Target class >> sel`) register
    /// against the metaclass tag `'Target class'` — the same `name ++ " class"`
    /// convention used elsewhere for metaclass tags.
    fn extension_class_tag(ext: &StandaloneMethodDefinition) -> String {
        let name = ext.class_name.name.to_string();
        if ext.is_class_method {
            format!("{name} class")
        } else {
            name
        }
    }

    /// Determines the owner (contributing package, ADR 0070) for `ext`.
    ///
    /// Priority:
    /// 1. The explicit `package@Target >> sel` qualifier when present.
    /// 2. The compiling module's package, derived from the `bt@<package>@<class>`
    ///    module-name convention.
    /// 3. The full module name as a last-resort unique owner atom (graceful
    ///    fallback for unprefixed/single-file modules — the extension still
    ///    dispatches; it just won't surface under a loaded `Package` in
    ///    `extendersOf:`).
    fn extension_owner(&self, ext: &StandaloneMethodDefinition) -> String {
        if let Some(pkg) = &ext.package {
            return pkg.name.to_string();
        }
        super::methods::extract_package_from_module_name(&self.module_name)
            .unwrap_or_else(|| self.module_name.to_string())
    }

    /// Builds the `Source` argument document for `register/5`.
    ///
    /// Returns the method body source as a Core Erlang binary literal, or the
    /// atom `'undefined'` when the source text cannot be recovered (preserving
    /// `register/4` semantics — register without source rather than crash).
    fn extension_source_doc(&self, ext: &StandaloneMethodDefinition) -> Document<'static> {
        let source = self.extract_method_source(&ext.method);
        if source.is_empty() {
            Document::Str("'undefined'")
        } else {
            leaf::binary_lit(&source)
        }
    }

    /// Generates the callable fun for a foreign extension method.
    ///
    /// Actor targets use the state-threading shape
    /// `fun(_ExtArgs, Self, State) -> {Result, NewState}` (3-arity); all other
    /// (value/primitive) targets use `fun(_ExtArgs, Self) -> Result` (2-arity).
    /// The runtime selects the right path via the fun's arity.
    fn generate_extension_fun(
        &mut self,
        ext: &StandaloneMethodDefinition,
    ) -> Result<Document<'static>> {
        let target_is_actor = self
            .class_hierarchy
            .as_ref()
            .is_some_and(|h| h.is_actor_subclass(ext.class_name.name.as_str()));

        // BT-2250: scope the generator's class identity to the extension's
        // TARGET class while generating the body, so a `super` send inside the
        // extension walks the target class's hierarchy rather than the host
        // module's (which `class_name()` would otherwise return). The bare class
        // name matches the convention used by normal instance- and class-side
        // methods (`generate_super_send` emits `class_name()` as `CurrentClass`
        // without appending " class"). Restored afterwards so the host module's
        // own registration chain is unaffected.
        let prev_identity = self.class_identity().cloned();
        self.set_class_identity(Some(super::super::util::ClassIdentity::new(
            ext.class_name.name.as_str(),
        )));

        let result = if target_is_actor {
            self.generate_actor_extension_fun(ext)
        } else {
            self.generate_value_extension_fun(ext)
        };

        self.set_class_identity(prev_identity);
        result
    }

    /// Generates a 2-arity value/primitive extension fun:
    /// `fun (_ExtArgs, Self) -> let P0 = hd(_ExtArgs) in ... <body>`.
    ///
    /// Mirrors `generate_value_type_method`: `Self` is the receiver (version 0,
    /// so body field/`self` reads resolve to the `Self` parameter) and user
    /// parameters are bound from positions in the `Args` list. The body returns
    /// a plain value (no state threading) per the value-type calling convention.
    fn generate_value_extension_fun(
        &mut self,
        ext: &StandaloneMethodDefinition,
    ) -> Result<Document<'static>> {
        let method = &ext.method;
        let prev_context = self.context;
        self.context = CodeGenContext::ValueType;
        self.reset_self_version();
        self.push_scope();
        self.current_method_params.clear();
        // BT-2709: Reset arithmetic fast-path parameter-type tracking.
        self.clear_method_param_types();
        // BT-2710 follow-up: extension bodies have no access to the target
        // class's state declarations, so clear field types — `self.<field>`
        // reads fall back to the bare BIF. Known limitation (BT-2710): an extension
        // method comparing an object-typed field silently term-orders rather
        // than dispatching (status quo, unchanged from pre-BT-2710). Threading
        // the target class's state into extension codegen is a tracked
        // follow-up under the BT-2708 operators-as-messages epic.
        self.clear_class_field_types();
        let prev_selector = self.current_method_selector.take();
        self.current_method_selector = Some(method.selector.name().to_string());

        let arg_prelude = self.bind_extension_params(method);

        let needs_nlr = self
            .semantic_facts
            .has_block_nlr_or_walk(&method.span, &method.body);
        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.set_current_nlr_token(Some(token_var.clone()));
            Some(token_var)
        } else {
            None
        };

        let body = super::super::util::collect_body_exprs(&method.body);
        let has_nlr = nlr_token_var.is_some();
        let body_result = if body.is_empty() {
            Ok(self.generate_vt_empty_body(has_nlr))
        } else {
            self.generate_vt_body_exprs(&body, has_nlr)
        };

        self.pop_scope();
        self.current_method_selector = prev_selector;
        self.set_current_nlr_token(None);
        self.context = prev_context;

        let body_parts = body_result?;
        let body_doc = Document::Vec(body_parts);

        let fun_doc = if let Some(token_var) = nlr_token_var {
            let catch_vars = self.wrap_value_type_body_with_nlr_catch(&token_var);
            docvec![
                "fun (_ExtArgs, Self) ->",
                nest(INDENT, docvec![line(), arg_prelude]),
                "\n",
                catch_vars.format_try_prefix(),
                body_doc,
                catch_vars.format_catch_suffix(),
            ]
        } else {
            docvec![
                "fun (_ExtArgs, Self) ->",
                nest(INDENT, docvec![line(), arg_prelude]),
                "\n",
                body_doc,
            ]
        };

        Ok(self.maybe_annotate_extension_fun(fun_doc, method.span))
    }

    /// Generates a 3-arity actor extension fun:
    /// `fun (_ExtArgs, Self, State) -> let P0 = hd(_ExtArgs) in ... {Result, NewState}`.
    ///
    /// Mirrors the sealed-method actor body shape: the body is generated with a
    /// reply tuple (`{Result, State}`) so the actor dispatch path can unwrap and
    /// thread state, matching `apply ExtFun(Args, Self, State)`.
    fn generate_actor_extension_fun(
        &mut self,
        ext: &StandaloneMethodDefinition,
    ) -> Result<Document<'static>> {
        let method = &ext.method;
        let prev_context = self.context;
        self.context = CodeGenContext::Actor;
        self.reset_state_version();
        self.push_scope();
        self.current_method_params.clear();
        // BT-2709: Reset arithmetic fast-path parameter-type tracking.
        self.clear_method_param_types();
        // BT-2710 follow-up: extension bodies have no access to the target
        // class's state declarations, so clear field types — `self.<field>`
        // reads fall back to the bare BIF. Known limitation (BT-2710): an extension
        // method comparing an object-typed field silently term-orders rather
        // than dispatching (status quo, unchanged from pre-BT-2710). Threading
        // the target class's state into extension codegen is a tracked
        // follow-up under the BT-2708 operators-as-messages epic.
        self.clear_class_field_types();

        let arg_prelude = self.bind_extension_params(method);

        let needs_nlr = self
            .semantic_facts
            .has_block_nlr_or_walk(&method.span, &method.body);
        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.set_current_nlr_token(Some(token_var.clone()));
            Some(token_var)
        } else {
            None
        };

        let body_result = self.generate_method_definition_body_with_reply(method);
        self.set_current_nlr_token(None);

        let body_doc = match body_result {
            Ok(doc) => doc,
            Err(e) => {
                self.pop_scope();
                self.context = prev_context;
                return Err(e);
            }
        };

        let body_doc = if let Some(ref token_var) = nlr_token_var {
            self.wrap_actor_body_with_nlr_catch(body_doc, token_var, false)
        } else {
            body_doc
        };

        self.pop_scope();
        self.context = prev_context;

        // The actor method body generator emits a gen_server `{reply, Result,
        // NewState}` 3-tuple. The extension calling convention expects the
        // `{Result, NewState}` 2-tuple (`fun(Args, Self, State) -> {Result,
        // NewState}` — see beamtalk_dispatch:invoke_extension/4 and the
        // simulation-test funs), so convert the 3-tuple to the 2-tuple.
        let converted_body = docvec![
            "let _ExtReply = ",
            body_doc,
            line(),
            "in {call 'erlang':'element'(2, _ExtReply), call 'erlang':'element'(3, _ExtReply)}",
        ];

        let fun_doc = docvec![
            "fun (_ExtArgs, Self, State) ->",
            nest(INDENT, docvec![line(), arg_prelude]),
            "\n",
            nest(INDENT, docvec![line(), converted_body]),
        ];

        Ok(self.maybe_annotate_extension_fun(fun_doc, method.span))
    }

    /// Binds each user parameter of `method` from the `_ExtArgs` list, returning
    /// the `let P0 = hd(_ExtArgs) in let P1 = hd(tl(_ExtArgs)) in ...` prelude.
    ///
    /// Each parameter is bound via `fresh_var` so body references resolve to the
    /// generated Core Erlang variable, and recorded in `current_method_params`
    /// for `@primitive` codegen consistency with the regular method paths.
    /// Returns [`Document::Nil`] for unary methods (no parameters).
    fn bind_extension_params(
        &mut self,
        method: &crate::ast::MethodDefinition,
    ) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(method.parameters.len());
        for (i, param) in method.parameters.iter().enumerate() {
            let var_name = self.fresh_var(&param.name.name);
            self.current_method_params.push(var_name.clone());
            // BT-2709: Record declared type for the arithmetic fast path.
            self.record_method_param_type(&param.name.name, param.type_annotation.as_ref());

            // Access the i-th element of _ExtArgs: hd(tl(tl(...(_ExtArgs))))
            let mut access: Document<'static> = Document::Str("_ExtArgs");
            for _ in 0..i {
                access = docvec!["call 'erlang':'tl'(", access, ")"];
            }
            parts.push(docvec![
                "let <",
                leaf::var(var_name),
                "> = call 'erlang':'hd'(",
                access,
                ") in",
            ]);
            if i + 1 < method.parameters.len() {
                parts.push(line());
            }
        }
        if parts.is_empty() {
            Document::Nil
        } else {
            Document::Vec(parts)
        }
    }

    /// Annotates the extension fun expression with its source line when the span
    /// resolves, matching how the regular method paths annotate funs (BT-940).
    fn maybe_annotate_extension_fun(
        &self,
        fun_doc: Document<'static>,
        span: crate::source_analysis::Span,
    ) -> Document<'static> {
        if let Some(line_num) = self.span_to_line(span) {
            self.annotate_with_line(fun_doc, line_num)
        } else {
            fun_doc
        }
    }
}
