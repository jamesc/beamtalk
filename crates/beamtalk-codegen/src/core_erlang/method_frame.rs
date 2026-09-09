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
use beamtalk_core::ast::ParameterDefinition;

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
    /// `State`, `ClassVars` and class-var-mutated-tracking counters, binds
    /// `self` to `ClassSelf` in the fresh scope, and marks
    /// `in_class_method` for the guarded region — all undone together on
    /// drop.
    ClassMethod,
}

/// RAII guard for a single method body's codegen scope. See the module docs
/// for the shape it replaces.
pub(in crate::core_erlang) struct MethodFrame<'a> {
    generator: &'a mut CoreErlangGenerator,
    prev_selector: Option<String>,
    boundary: MethodBoundary,
}

impl<'a> MethodFrame<'a> {
    /// Enters a method body's codegen scope: resets `boundary`'s version
    /// counter(s), pushes a fresh variable scope, clears per-method
    /// parameter tracking, records `selector_name` as the current selector
    /// (restored to whatever it was before `enter` when the returned guard
    /// drops), and binds each of `parameters` via `fresh_var`, recording its
    /// declared type for the arithmetic fast-path classifier. For
    /// [`MethodBoundary::ClassMethod`], also binds `self` to `ClassSelf` in
    /// the fresh scope and marks `in_class_method` (cleared again on drop)
    /// — both must happen after the scope push and before the parameter
    /// loop, matching the hand-rolled prologue this replaces. Returns the
    /// guard together with the bound parameter variable names, in
    /// declaration order.
    pub(in crate::core_erlang) fn enter(
        generator: &'a mut CoreErlangGenerator,
        selector_name: &str,
        parameters: &[ParameterDefinition],
        boundary: MethodBoundary,
    ) -> (Self, Vec<String>) {
        match boundary {
            MethodBoundary::Actor => generator.reset_state_version(),
            MethodBoundary::ValueType => generator.reset_self_version(),
            MethodBoundary::ClassMethod => {
                generator.reset_state_version();
                generator.set_class_var_version(0);
                generator.set_class_var_mutated(false);
            }
        }
        generator.push_scope();
        generator.current_method_params.clear();
        generator.clear_method_param_types();
        let prev_selector = generator
            .current_method_selector
            .replace(selector_name.to_string());

        if boundary == MethodBoundary::ClassMethod {
            generator.bind_var("self", "ClassSelf");
            generator.set_in_class_method(true);
        }

        let param_vars = parameters
            .iter()
            .map(|p| {
                let var_name = generator.fresh_var(&p.name.name);
                generator.current_method_params.push(var_name.clone());
                generator.record_method_param_type(&p.name.name, p.type_annotation.as_ref());
                var_name
            })
            .collect();

        (
            Self {
                generator,
                prev_selector,
                boundary,
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
    }
}
