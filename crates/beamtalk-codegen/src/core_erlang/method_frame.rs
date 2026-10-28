// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `MethodFrame`: RAII guard for the method-body codegen prologue/epilogue
//! duplicated across the actor/value-type/class-method entry points.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Every method-body generator repeats the same ritual on entry — reset the
//! relevant version counter, push a fresh variable scope, clear per-method
//! parameter tracking, record the current selector, and bind each parameter
//! via `fresh_var` — and the same cleanup on exit: pop the scope and restore
//! the previous selector. Because that cleanup must run identically whether
//! the body lowers successfully or bails out through `?`, hand-rolling it at
//! each call site means an explicit `match { Err(e) => { <cleanup>; return
//! Err(e); } }` wherever the body generation can fail — easy to get out of
//! order, and only checked by eyeballing the diff.
//!
//! `MethodFrame` moves that cleanup into a `Drop` impl instead: it borrows
//! the generator exclusively for the guarded region — call sites reach the
//! generator's other methods and fields through `Deref`/`DerefMut` (e.g.
//! `frame.foo()` rather than `self.foo()`) — and its `Drop::drop` runs the
//! epilogue exactly once the frame value goes out of scope, on a normal
//! return *and* on an early `?` return alike, reproducing the order the
//! converted call site's hand-rolled prologue used.
//!
//! Every method-body entry point that repeated this ritual — actor instance
//! methods, value-type instance methods, class-side methods, sealed-method
//! functions, and the two extension-fun shapes (now one function
//! parameterised by [`MethodBoundary`]) — now goes through `MethodFrame`,
//! each converted and verified independently (byte-identical codegen corpus
//! output) so a regression is bisectable to a single call site.

use super::CoreErlangGenerator;
use super::field_types::trusted_param_types;
use super::generator::body_state::BodyState;
use beamtalk_core::ast::MethodDefinition;

/// The class-object variable every class-method body reads as `self`.
pub(in crate::core_erlang) const CLASS_SELF_VAR: &str = "ClassSelf";

/// BT-4029: the first parameter of a direct-called class method (`nil` on
/// the direct path, or the class object a sibling class method forwards).
/// When the body reads `ClassSelf`, its prologue rebinds `ClassSelf` from it
/// (`direct_class_self_prologue_doc` in `gen_server::methods`).
pub(in crate::core_erlang) const DIRECT_CLASS_SELF_PARAM: &str = "_DirectClassSelf";

/// Which version counter [`MethodFrame::enter`] resets on entry — the one
/// axis that varies across the method-codegen call sites this guard
/// replaces. Grows one variant per call site as each is converted and
/// verified independently.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum MethodBoundary {
    /// Actor instance methods (`generate_method_dispatch`): resets the
    /// `gen_server` `State` version counter.
    Actor,
    /// Value-type instance methods (`generate_value_type_method`): resets
    /// the `Self{N}` snapshot version counter instead of the `State` one.
    ValueType,
    /// Class-side methods (`generate_class_method_functions`): resets the
    /// `State` counter, binds `self` to `ClassSelf` in the fresh scope, and marks
    /// `in_class_method` for the guarded region — all undone together on
    /// drop.
    ClassMethod,
}

/// RAII guard for a single method body's codegen scope. See the module docs
/// for the shape it replaces.
pub(in crate::core_erlang) struct MethodFrame<'a> {
    generator: &'a mut CoreErlangGenerator,
    prev_selector: Option<String>,
    /// The method span current before `enter` (BT-4059 peel scope).
    prev_method_span: Option<beamtalk_core::source_analysis::Span>,
    boundary: MethodBoundary,
    /// `(source_text, source_path)` in effect before `enter` swapped in a
    /// flattened provision's protocol source (ADR 0127 §3); `None` when the
    /// method is the class's own and nothing was swapped.
    prev_source: Option<(Option<String>, Option<String>)>,
    /// ADR 0133 Phase 3 (BT-3911): the enclosing body's per-body generator
    /// state ([`BodyState`]), parked here for the method's body, which starts
    /// from the default (a body lowered inside another one, a `ClassBuilder`
    /// fun's cascade in a loop body, inherits nothing). `None` once restored.
    outer_body: Option<BodyState>,
}

impl<'a> MethodFrame<'a> {
    /// Enters a method body's codegen scope: resets `boundary`'s version
    /// counter(s), pushes a fresh variable scope, clears per-method
    /// parameter tracking, records `method`'s selector as the current selector
    /// (restored to whatever it was before `enter` when the returned guard
    /// drops), and binds each of its parameters via `fresh_var`, recording
    /// the declared type of each one [`trusted_param_types`] trusts (a name
    /// the body never rebinds, BT-3921) for the operator fast-path
    /// classifiers. For
    /// [`MethodBoundary::ClassMethod`], also binds `self` to `ClassSelf` in
    /// the fresh scope and marks `in_class_method` (cleared again on drop)
    /// — both must happen after the scope push and before the parameter
    /// loop, matching the hand-rolled prologue this replaces. Returns the
    /// guard together with the bound parameter variable names, in
    /// declaration order.
    pub(in crate::core_erlang) fn enter(
        generator: &'a mut CoreErlangGenerator,
        method: &MethodDefinition,
        boundary: MethodBoundary,
    ) -> (Self, Vec<String>) {
        let outer_body = generator.park_body_state();
        match boundary {
            MethodBoundary::Actor => generator.reset_state_version(),
            MethodBoundary::ValueType => generator.reset_self_version(),
            MethodBoundary::ClassMethod => {
                generator.reset_state_version();
            }
        }
        generator.push_scope();
        generator.current_method_params.clear();
        generator.clear_method_param_types();
        let selector_name = method.selector.name();
        let prev_selector = generator
            .current_method_selector
            .replace(selector_name.to_string());
        let prev_method_span = generator.current_method_span.replace(method.span);

        if boundary == MethodBoundary::ClassMethod {
            generator.bind_var("self", CLASS_SELF_VAR);
            generator.set_in_class_method(true);
        }

        let param_vars = method
            .parameters
            .iter()
            .map(|p| {
                let var_name = generator.fresh_var(&p.name.name);
                generator.current_method_params.push(var_name.clone());
                var_name
            })
            .collect();
        for (name, type_name) in trusted_param_types(method) {
            generator.record_method_param_type(name, type_name);
        }

        // A `uses:`-flattened method's spans are offsets into its protocol's
        // file, so for the duration of the frame map lines (and stamp the
        // `file` of line annotations) through that protocol's source.
        let prev_source = generator
            .flattened_method_source(
                selector_name.as_str(),
                boundary == MethodBoundary::ClassMethod,
            )
            .map(|protocol_source| {
                let prev_path = generator.source_path.clone();
                let prev_text = generator
                    .source_text
                    .replace(protocol_source.text.to_string());
                if let Some(path) = protocol_source.path {
                    generator.source_path = Some(path.to_string());
                }
                (prev_text, prev_path)
            });

        (
            Self {
                generator,
                prev_selector,
                prev_method_span,
                boundary,
                prev_source,
                outer_body: Some(outer_body),
            },
            param_vars,
        )
    }
}

impl std::ops::Deref for MethodFrame<'_> {
    type Target = CoreErlangGenerator;

    fn deref(&self) -> &Self::Target {
        self.generator
    }
}

impl std::ops::DerefMut for MethodFrame<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.generator
    }
}

impl Drop for MethodFrame<'_> {
    /// Pops the scope pushed by `enter`, restores the selector that was
    /// current before it, and — for [`MethodBoundary::ClassMethod`] — clears
    /// `in_class_method` again. The same cleanup the converted call sites'
    /// hand-rolled prologues ran on both their success and error paths, now
    /// run unconditionally once the guard goes out of scope regardless of
    /// which path the guarded region took.
    fn drop(&mut self) {
        self.generator.pop_scope();
        if self.boundary == MethodBoundary::ClassMethod {
            self.generator.set_in_class_method(false);
        }
        self.generator.current_method_selector = self.prev_selector.take();
        self.generator.current_method_span = self.prev_method_span.take();
        if let Some((text, path)) = self.prev_source.take() {
            self.generator.source_text = text;
            self.generator.source_path = path;
        }
        if let Some(outer) = self.outer_body.take() {
            self.generator.unpark_body_state(outer);
        }
    }
}
