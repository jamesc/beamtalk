// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 outer-local threading diagnostics (BT-3745).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A block that writes a local of its enclosing method (an *outer local*) is
//! ordinary Smalltalk. Beamtalk threads such writes back through the
//! construct's `{Value, StateAcc}` tuple (ADR 0041), and today that only works
//! for some constructs in some positions. ADR 0131 makes every
//! local-threading construct a `ThreadedValue` producer (phases 1-4); for the
//! shapes no producer lowers yet, this module turns the ones that crash or
//! silently answer wrong into compile errors, so nothing is silently wrong.
//! Two checks share
//! one walk and one fact, [`outer_local_writes`]:
//!
//! - **ADR 0133: a block that assigns a captured variable and escapes**
//!   ([`DiagnosticCategory::Tier2BlockNoReturnChannel`], permanent; it began
//!   as ADR 0131 §6). A block may assign a local it captured only where the
//!   compiler lowers the block in place: as the direct argument (or receiver,
//!   for `value` and the loops) of a control construct or a compiler-known
//!   collection message ([`crate::state_threading_selectors`], the one list,
//!   which [`threading_constructs_note`] also reads). A block literal that
//!   does so and **escapes** (is stored in a local, returned, stored in a
//!   field or class variable, placed in a collection literal, passed to any
//!   other method, a cascade message included, passed as an Erlang FFI
//!   argument (BT-3917: Erlang calls it as a plain fun and drops the write;
//!   ADR 0041 §Erlang Interop Boundary), or sent a message other than
//!   `value`) is reported at the assignment, naming the shape ([`Escape`]),
//!   in every context, the REPL root included (BT-3858). Reads of captured
//!   variables and `^` inside an escaping block are unaffected.
//! - **The retired Phase 0 position check**
//!   ([`DiagnosticCategory::UnthreadedLocalWrite`], retired by BT-3913:
//!   ADR 0133 Phase 3 lowered every statement shape through frames, and the
//!   733-shape oracle reports zero silent drops and zero `erlc` failures).
//!   A local-threading construct ([`local_threading_construct`], of a family
//!   threaded today) whose threaded set ([`construct_outer_local_writes`],
//!   the same recognizer and set codegen's `threaded_locals_of` reads) is not
//!   empty is accepted as a statement wherever it sits. At any other
//!   (operand) position it is accepted only where a local-threading producer
//!   lowers it — a method-root operand site, a frame site, or a REPL site
//!   (checked below) — and anywhere else it is an error naming the construct,
//!   the position and BT-3913.
//!
//! Two body-level checks run after the walk: the `ensure:` cleanup that would
//! read a stale local after a `^` or an error ([`report_nlr_out_of_ensure`],
//! permanent, ADR 0133), and the `^` shapes that answer wrong silently
//! ([`report_stale_returns`], BT-4003).
//!
//! Layout: [`types`] (the shared `MethodContext` / `ConstructKind` / `Container` /
//! `Position` vocabulary), [`walker`] (the escape and position walk),
//! [`escapes`] (the `Escape` shapes and their message), [`ensure_nlr`] and
//! [`stale_returns`] (the two body-level checks). This file holds the entry point.

mod ensure_nlr;
mod escapes;
mod stale_returns;
mod types;
mod walker;

use crate::ast::{ExpressionStatement, Module, WellKnownSelector};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::type_checker::TypeMap;
use crate::source_analysis::Diagnostic;
use ecow::EcoString;

use self::ensure_nlr::report_nlr_out_of_ensure;
use self::stale_returns::report_stale_returns;
use self::types::MethodContext;
use self::walker::Walker;

use MethodContext::{Actor, Class, Repl, ValueType};

/// Runs both ADR 0131 checks over every method body and module-level
/// expression list in `module` (see the module docs).
pub(crate) fn check_local_threading(
    module: &Module,
    hierarchy: &ClassHierarchy,
    type_map: Option<&TypeMap>,
    known_vars: &[&str],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let facts = |class_name: &str, class_side: bool| ReceiverFacts {
        hierarchy,
        type_map,
        self_is_dictionary: !class_side
            && (class_name == "Dictionary"
                || hierarchy
                    .superclass_chain(class_name)
                    .iter()
                    .any(|c| c == "Dictionary")),
        class_name: EcoString::from(class_name),
    };
    let instance_context = |class_name: &str| {
        if hierarchy.is_actor_subclass(class_name) {
            Actor
        } else {
            ValueType
        }
    };
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        let context = instance_context(class_name);
        for method in &class.methods {
            check_method(context, method, &facts(class_name, false), diagnostics);
        }
        for method in &class.class_methods {
            check_method(Class, method, &facts(class_name, true), diagnostics);
        }
    }
    for standalone in &module.method_definitions {
        let context = if standalone.is_class_method {
            Class
        } else {
            instance_context(standalone.class_name.name.as_str())
        };
        check_method(
            context,
            &standalone.method,
            &facts(
                standalone.class_name.name.as_str(),
                standalone.is_class_method,
            ),
            diagnostics,
        );
    }
    if !module.expressions.is_empty() {
        let known: Vec<EcoString> = known_vars.iter().map(|v| EcoString::from(*v)).collect();
        check_body(
            Repl,
            None,
            &known,
            &module.expressions,
            &facts("", true),
            diagnostics,
        );
    }
}

/// What the lookup-receiver rule
/// ([`receiver_statically_not_dictionary`]) and the escaping-block message
/// ([`Walker::passed_escape`]) read for one method body.
#[derive(Clone)]
struct ReceiverFacts<'a> {
    hierarchy: &'a ClassHierarchy,
    /// The inferred types of the module's expressions; `None` when the caller
    /// has none (`beamtalk-lint`'s query), so only `self`/`super` are judged.
    type_map: Option<&'a TypeMap>,
    /// Whether `self` may be a `Dictionary`: an instance method of `Dictionary`
    /// or a subclass.
    self_is_dictionary: bool,
    /// The class the method belongs to (empty at the REPL root): where a
    /// class-side `self` send goes.
    class_name: EcoString,
}

/// [`check_body`] for one method.
fn check_method(
    context: MethodContext,
    method: &crate::ast::MethodDefinition,
    receivers: &ReceiverFacts<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    check_body(
        context,
        Some(method.selector.name()),
        &param_names(method),
        &method.body,
        receivers,
        diagnostics,
    );
}

/// Walks one method body (`selector` its method's), or the module-level
/// expression list (`selector` `None`), whose outermost frame binds `params`.
fn check_body(
    context: MethodContext,
    selector: Option<EcoString>,
    params: &[EcoString],
    body: &[ExpressionStatement],
    receivers: &ReceiverFacts<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut walker = Walker::new(context, body, diagnostics);
    walker.receivers = Some(receivers.clone());
    walker.method_selector = selector;
    walker.scope.push(params.iter().cloned().collect());
    walker.body(body);
    walker.finish();
    report_nlr_out_of_ensure(context, params, body, diagnostics);
    report_stale_returns(context, params, body, diagnostics);
}

fn param_names(method: &crate::ast::MethodDefinition) -> Vec<EcoString> {
    method
        .parameters
        .iter()
        .map(|p| p.name.name.clone())
        .collect()
}

/// The conditional selectors whose plain arm is a frame (BT-4367): the selectors
/// the runtime grid `stdlib/test/bt4367arm_local_grid_test.bt` proves, which
/// `plain_arm_selectors_are_all_in_the_runtime_grid` checks. `and:`/`or:` are
/// not in it (not exercised).
pub(crate) const PLAIN_ARM_SELECTORS: &[WellKnownSelector] = &[
    WellKnownSelector::IfTrueIfFalse,
    WellKnownSelector::IfTrue,
    WellKnownSelector::IfFalse,
    WellKnownSelector::IfNilIfNotNil,
    WellKnownSelector::IfNotNilIfNil,
    WellKnownSelector::IfNil,
    WellKnownSelector::IfNotNil,
];
