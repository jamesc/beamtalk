// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for Core Erlang code generation.
//!
//! Tests are organized into domain-focused sub-modules:
//! - [`expressions`] — literals, binary ops, maps, string interpolation
//! - [`dispatch`] — message sends, futures, spawn, erlang interop, cascades
//! - [`control_flow`] — loops, stored closures, if-true/false conditionals
//! - [`gen_server`] — module/class codegen, REPL modules, value subclasses
//! - [`primitives`] — primitive selector and intrinsic codegen
//! - [`branch_context`] — `with_branch_context`'s per-prefix
//!   save/reset/restore discipline (state/`class_vars`/self)
//! - [`late_reconcile_conformance`] — ADR 0124 §8/B9 (BT-3556): the
//!   cross-boundary `late`-slot reconcile decision-table fixture, asserted
//!   against `beamtalk_shape_migration_tests.erl`
//! - [`recv_type`] — ADR 0115 Phase 2: the xref `recv_type`
//!   write-path fixture matrix (typed/protocol/dynamic/union/native/alias
//!   locals, `Meta{C}`, self-send, FFI receiver)
//! - [`trait_flattening`] — ADR 0127 §3/§10 (BT-3590): a `uses:` class's
//!   flattened provisions reaching compiled output through both the
//!   self-sufficient and driver-handoff codegen entry points

pub use super::*;
pub use beamtalk_core::ast::*;
pub use beamtalk_core::source_analysis::Span;

pub(crate) fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

/// Asserts that the given Core Erlang source compiles successfully through `erlc`.
///
/// Uses `tempfile::tempdir()` for an isolated temp directory per invocation,
/// avoiding filename collisions when tests run in parallel. The temp dir and
/// all its contents are automatically cleaned up on drop.
///
/// If `erlc` is not found in PATH, prints a skip notice and returns without
/// failing.
///
/// ADR 0117 Decision step 5: relocated here from
/// `beamtalk_core::test_helpers` — every caller (`control_flow`,
/// `expressions`, `gen_server` below) now lives in this crate, and
/// `#[cfg(test)]` in `beamtalk-core` only applies to that crate's own build,
/// not to a dependent crate's tests, so keeping it there would have meant
/// widening its gate to `#[cfg(any(test, feature = "test"))]` and promoting
/// `tempfile` from a dev-dependency to a `test`-feature-gated regular
/// dependency purely to serve a helper nothing in `beamtalk-core` itself
/// calls any more. Relocating avoids both, per the "No duplicate
/// implementations" rule.
///
/// # Panics
///
/// Panics if `erlc` exits with a non-zero status, including the full Core
/// Erlang source in the panic message to aid debugging.
pub(crate) fn assert_compiles_through_erlc(module_name: &str, core_erlang: &str) {
    use std::fs;
    use std::process::Command;

    let tmp_dir = tempfile::tempdir().expect("failed to create temp dir for erlc test");
    let core_file = tmp_dir.path().join(format!("{module_name}.core"));
    fs::write(&core_file, core_erlang).expect("should write core erlang file");

    let output = Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(tmp_dir.path())
        .arg(&core_file)
        .output();

    match output {
        Ok(output) => {
            assert!(
                output.status.success(),
                "erlc compilation failed for module '{module_name}':\nstdout: {}\nstderr: {}\n\nGenerated Core Erlang:\n{core_erlang}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            );
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            println!("Skipping erlc compilation test for '{module_name}' - erlc not in PATH");
        }
        Err(e) => {
            panic!("failed to invoke erlc for module '{module_name}': {e}");
        }
    }
    // tmp_dir is dropped here, auto-cleaning .core and .beam files
}

/// Whether `erlc` can be invoked. [`assert_compiles_through_erlc`] silently passes without
/// it, so a test whose verdict depends on `erlc` rejecting generated code must check this
/// before asserting that a case which only fails at `erlc` is still failing.
/// Any error other than "not found" (e.g. a permissions failure) counts as available, so the
/// failure surfaces in `assert_compiles_through_erlc` instead of silently skipping the check.
pub(crate) fn erlc_available() -> bool {
    match std::process::Command::new("erlc").arg("-v").output() {
        Ok(_) => true,
        Err(e) => e.kind() != std::io::ErrorKind::NotFound,
    }
}

/// Parse `src` as a single-class Beamtalk module and run codegen in workspace mode.
///
/// Shared by control-flow and dispatch test modules to avoid copy-pasting this
/// boilerplate across every sub-module.
pub(crate) fn codegen(src: &str) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    crate::core_erlang::generate_module(&module, crate::core_erlang::CodegenOptions::new("test"))
        .expect("codegen should succeed")
}

/// The debug-build panic message of compiling `src` with `fault` active (for a verifier
/// finding, `report_threaded_ir_verify_errors`'s), or `None` when the compile succeeds.
///
/// Shared by the verifier corpora that prove a check fires under fault injection.
pub(crate) fn verifier_report(
    src: &str,
    fault: Option<crate::core_erlang::threaded_expr::fault::LocalRebindFault>,
) -> Option<String> {
    let compile = || std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| codegen(src)));
    let result = match fault {
        Some(fault) => crate::core_erlang::threaded_expr::fault::with(fault, compile),
        None => compile(),
    };
    result.err().map(|payload| {
        payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default()
    })
}

/// Builds a Module with a `Value subclass: Point` with x and y slots.
pub(crate) fn make_value_subclass_point() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![
            StateDeclaration {
                name: Identifier::new("x", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                slot_kind: beamtalk_core::ast::SlotKind::default(),
                span: Span::new(0, 0),
            },
            StateDeclaration {
                name: Identifier::new("y", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                slot_kind: beamtalk_core::ast::SlotKind::default(),
                span: Span::new(0, 0),
            },
        ],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

mod analysis_handoff;
mod branch_context;
mod cascade_operand_state_corpus;
mod closed_fun_state_version;
mod control_flow;
mod dispatch;
mod expect_directive_statement;
mod expressions;
mod field_assignment_value_position;
mod fold_locals_carried_corpus;
mod gen_server;
mod group_by_fold_gate_agreement;
mod hybrid_field_rebind_corpus;
mod late_reconcile_conformance;
mod local_producer_agreement;
mod local_producer_agreement_conditional;
mod local_producer_agreement_exception;
mod local_producer_agreement_lookup;
mod local_producer_agreement_repl;
mod local_rebind_verifier_corpus;
mod loop_body_self_send_operand_corpus;
mod nlr;
mod primitives;
mod protocol_registration_conformance;
mod recv_type;
mod sibling_snapshot;
mod supervisor;
mod threaded_locals;
mod threaded_locals_agreement;
mod trait_flattening;
mod value_position_assignment_agreement;
mod value_type_conditional_family_verifier_corpus;
mod value_type_fold_family_corpus;
mod verifier_surfacing;
mod while_condition_class_method;
