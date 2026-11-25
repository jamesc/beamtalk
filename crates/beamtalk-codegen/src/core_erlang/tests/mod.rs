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
//! - [`value_immutable_slot_hint_conformance`] — BT-4273: the Value
//!   `immutable_value` slot hint fixture, asserted against
//!   `beamtalk_primitive_tests.erl`
//! - [`typed_protocol`] — BT-4486: sends to protocol-typed receivers become
//!   multi-arm class-tag guards over the protocol's known conformers
//! - [`typed_sealed`] — BT-4069: sends to sealed-Value-typed receivers
//!   compiling to a guarded direct `Module:dispatch/3` call

pub use super::*;
pub use beamtalk_core::ast::*;
pub use beamtalk_core::source_analysis::Span;

pub(crate) use beamtalk_core::test_helpers::test_support::{bare, empty_module};

/// Asserts that the given Core Erlang source compiles successfully through `erlc`.
///
/// A one-row [`assert_all_compile_through_erlc`]: an isolated temp directory
/// per invocation, so parallel tests never collide, removed afterwards. Use the
/// batch form when compiling several rows in a loop.
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
    assert_all_compile_through_erlc(&[(module_name, core_erlang)]);
}

/// Compiles every `(row name, Core Erlang)` pair through `erlc +from_core` in
/// one batched invocation (BT-4886), instead of one tempdir + `erlc` spawn per
/// row, and panics if any row is rejected.
///
/// The panic names each rejected row (its name is the `.core` file name `erlc`
/// reports against) and includes that row's generated Core Erlang. Skips, like
/// [`assert_compiles_through_erlc`], when `erlc` is not installed.
///
/// # Panics
///
/// Panics if `erlc` rejects any row.
pub(crate) fn assert_all_compile_through_erlc<N: AsRef<str>, C: AsRef<str>>(rows: &[(N, C)]) {
    if let Err((rejected, output)) = erlc_rejected_rows(rows) {
        let shown: Vec<String> = rejected
            .iter()
            .map(|&i| {
                format!(
                    "--- row '{}' ---\n{}",
                    rows[i].0.as_ref(),
                    rows[i].1.as_ref()
                )
            })
            .collect();
        panic!(
            "erlc compilation failed for {} of {} row(s):\n{output}\n\nGenerated Core Erlang of the rejected row(s):\n{}",
            rejected.len(),
            rows.len(),
            shown.join("\n")
        );
    }
}

/// The non-panicking core of [`assert_all_compile_through_erlc`], for callers
/// that classify rejected rows themselves rather than failing on the first
/// (BT-4886): one batched `erlc` run over `rows`, `Ok(())` when every row
/// compiles or `erlc` is not installed, otherwise `Err((indices of the
/// rejected rows, erlc's combined output))`.
///
/// On a batch failure each row is recompiled alone to find exactly which are
/// rejected; if none fails alone every row is reported, so a failure is never
/// silently narrowed to nothing.
pub(crate) fn erlc_rejected_rows<N: AsRef<str>, C: AsRef<str>>(
    rows: &[(N, C)],
) -> std::result::Result<(), (Vec<usize>, String)> {
    use beamtalk_core::test_helpers::test_support::{ERLC_BATCH, erlc_batched_in_temp_dir};
    if rows.is_empty() {
        return Ok(());
    }
    if !erlc_available() {
        println!("Skipping erlc compilation test - erlc not in PATH");
        return Ok(());
    }
    // Index-prefix and sanitise the file name so row labels with spaces or
    // punctuation (or duplicates) still map to distinct files.
    let modules: Vec<(String, String)> = rows
        .iter()
        .enumerate()
        .map(|(i, (name, code))| {
            let safe: String = name
                .as_ref()
                .chars()
                .take(40) // the `row{i}_` prefix keeps it unique; stay under path limits
                .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
                .collect();
            // `erlc +from_core` requires the file stem to equal the module
            // name, and rows commonly share one (`test`), so each row gets its
            // own directory; the path in `erlc`'s output then names the row.
            let module = code
                .as_ref()
                .split_once("module '")
                .and_then(|(_, rest)| rest.split_once('\''))
                .map_or("test", |(name, _)| name);
            (format!("row{i}_{safe}/{module}"), code.as_ref().to_string())
        })
        .collect();
    let Err(batch) = erlc_batched_in_temp_dir("codegen_erlc_rows", &modules) else {
        return Ok(());
    };
    // `erlc` stops at the first failing file of an invocation and prints
    // warnings for files that compile, so its batch output cannot say which
    // rows are rejected. Recompile one row at a time (the failing path only)
    // for the exact list.
    let mut rejected = Vec::new();
    let mut output = String::new();
    // Only the chunks (of `ERLC_BATCH`) that fail need the row-by-row pass.
    for (c, chunk) in modules.chunks(ERLC_BATCH).enumerate() {
        if erlc_batched_in_temp_dir("codegen_erlc_chunk", chunk).is_ok() {
            continue;
        }
        for (j, module) in chunk.iter().enumerate() {
            if let Err(why) =
                erlc_batched_in_temp_dir("codegen_erlc_row", std::slice::from_ref(module))
            {
                rejected.push(c * ERLC_BATCH + j);
                output.push_str(&why);
                output.push('\n');
            }
        }
    }
    if rejected.is_empty() {
        // The batch failed but no row fails alone (e.g. a spawn failure):
        // never narrow a failure to nothing.
        rejected = (0..rows.len()).collect();
        output = format!("the batched erlc run failed, but every row compiles alone:\n{batch}");
    }
    Err((rejected, output))
}

/// Whether `erlc` can be invoked. [`assert_compiles_through_erlc`] silently passes without
/// it, so a test whose verdict depends on `erlc` rejecting generated code must check this
/// before asserting that a case which only fails at `erlc` is still failing.
///
/// The one shared probe lives in `beamtalk_core::test_helpers::test_support` (BT-4678).
pub(crate) use beamtalk_core::test_helpers::test_support::{erlang_available, erlc_available};

/// The repository root, derived from this crate's own manifest directory
/// rather than the test process's current directory (which `cargo test`
/// does not guarantee is the workspace root).
pub(crate) fn repo_root() -> std::path::PathBuf {
    beamtalk_workspace::cargo_workspace_root!()
}

/// Collects every `.bt` / `.btscript` file under each of `dirs` (relative to
/// `root`) into one sorted vec.  Panics if any directory cannot be walked —
/// all corpus directories exist in the repo, so a walk error means something
/// unexpected went wrong and the test should not silently pass with fewer
/// files than expected.
pub(crate) fn corpus_files(root: &std::path::Path, dirs: &[&str]) -> Vec<std::path::PathBuf> {
    let mut files: Vec<std::path::PathBuf> = Vec::new();
    for dir in dirs {
        files.extend(
            // Every `.bt`/`.btscript` file, whatever `.gitignore` says:
            // what the corpus proofs cover must not depend on it (BT-4135).
            beamtalk_core::file_walker::FileWalker::format_files()
                .respect_gitignore(false)
                .walk_pathbuf(&root.join(dir))
                .unwrap_or_else(|e| panic!("walking corpus dir {dir}: {e:?}")),
        );
    }
    files
}

/// Parses `src` and normalises it the way analysis and codegen see it: with
/// redundant send-operand parentheses peeled (`ast::normalize`, BT-4059).
///
/// For the agreement harnesses that apply a core rule to the parsed tree
/// directly and compare it with what codegen lowered: both sides must read
/// the same canonical tree, as `analyse_full` and codegen do.
pub(crate) fn parse_normalised(
    src: &str,
) -> (
    beamtalk_core::ast::Module,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
) {
    let (mut module, diagnostics) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    beamtalk_core::ast::normalize::peel_operand_parens(&mut module);
    (module, diagnostics)
}

/// Parse `src` as a single-class Beamtalk module and run codegen with an explicit module name.
///
/// Use this when the generated module name matters (e.g. supervisor tests that assert on
/// `'bt@leaf'` or actor tests that use a name like `"stopper"`).  For tests that do not
/// care about the module name, prefer [`codegen`], which hard-codes `"test"`.
pub(crate) fn codegen_named(src: &str, module_name: &str) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new(module_name),
    )
    .expect("codegen should succeed")
}

/// Asserts that `src` parses with no diagnostics at all.
///
/// [`codegen_named`] discards parse diagnostics and emits whatever survived
/// error recovery, so a source typo (`=` is not an operator; `=:=` is) silently
/// drops statements and lets agreement/corpus tests pass vacuously. Corpus and
/// agreement tests call this on every generated program first. The rule is
/// "any diagnostic" (warnings included): generated rows are meant to be
/// well-formed, so anything the parser flags is a generator slip.
pub(crate) fn assert_parses(src: &str) {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (_, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        diags.is_empty(),
        "program has parse diagnostics {:?}:\n{src}",
        diags
            .iter()
            .map(|d| d.message.to_string())
            .collect::<Vec<_>>()
    );
}

/// BT-4679: asserts every needle occurs in `code`. A failure names the table row
/// (`label`) and prints the full generated code after "Got:".
pub(crate) fn assert_has(label: &str, code: &str, needles: &[&str]) {
    for needle in needles {
        assert!(
            code.contains(needle),
            "[{label}] expected output to contain {needle:?}. Got:\n{code}"
        );
    }
}

/// BT-4679: asserts no needle occurs in `code`; the counterpart of [`assert_has`].
pub(crate) fn assert_lacks(label: &str, code: &str, needles: &[&str]) {
    for needle in needles {
        assert!(
            !code.contains(needle),
            "[{label}] expected output NOT to contain {needle:?}. Got:\n{code}"
        );
    }
}

/// Parse `src` as a single-class Beamtalk module and run codegen in workspace mode.
///
/// Shared by control-flow and dispatch test modules to avoid copy-pasting this
/// boilerplate across every sub-module.
pub(crate) fn codegen(src: &str) -> String {
    codegen_named(src, "test")
}

/// BT-4244: asserts that every runtime send of `selector` in `code` (outside the
/// `methodXref` metadata line) is an inline boolean conditional's non-Boolean
/// fallback: the body of its `CondNoMatch` wildcard clause, sent to the
/// scrutinised `Cond` variable. The `<'true'>`/`<'false'>` arms stay inline.
pub(crate) fn assert_selector_sent_only_by_fallback(code: &str, selector: &str) {
    let needle = format!("'{selector}', [");
    for line in code.lines().filter(|line| !line.contains("'methodXref'")) {
        for (at, _) in line.match_indices(&needle) {
            let before = &line[..at];
            let Some(start) = before.rfind("CondNoMatch") else {
                panic!("`{selector}` sent outside a CondNoMatch clause:\n{code}")
            };
            let clause = &before[start..];
            assert!(
                !clause.contains("<'true'>") && !clause.contains("<'false'>"),
                "`{selector}` sent from a Boolean arm, not the CondNoMatch fallback:\n{code}"
            );
            assert!(
                clause.contains("'beamtalk_message_dispatch':'send'(_Cond@"),
                "the CondNoMatch fallback should send `{selector}` to the Cond var:\n{code}"
            );
        }
    }
}

/// Parse `src` as a Beamtalk module and run codegen, answering the codegen error's message
/// instead of panicking (for the corpora that assert a shape is a compile error).
pub(crate) fn compile(src: &str) -> std::result::Result<String, String> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    crate::core_erlang::generate_module(&module, crate::core_erlang::CodegenOptions::new("test"))
        .map_err(|e| e.to_string())
}

/// The debug-build panic message of compiling `src` with `fault` active (for a verifier
/// finding, `report_threaded_ir_verify_errors`'s), or `None` when the compile succeeds.
///
/// Shared by the verifier corpora that prove a check fires under fault injection.
pub(crate) fn verifier_report(
    src: &str,
    fault: Option<crate::core_erlang::fault::Fault>,
) -> Option<String> {
    let compile = || std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| codegen(src)));
    let result = match fault {
        Some(fault) => crate::core_erlang::fault::with(fault, compile),
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
        state: vec![
            StateDeclaration {
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                slot_kind: beamtalk_core::ast::SlotKind::default(),
                ..StateDeclaration::test("x")
            },
            StateDeclaration {
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                slot_kind: beamtalk_core::ast::SlotKind::default(),
                ..StateDeclaration::test("y")
            },
        ],
        ..ClassDefinition::test("Point", Some("Value"), ClassKind::Value)
    };
    Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
    }
}

mod analysis_handoff;
mod arm_local_slot_verifier_corpus;
mod arm_operand_carrier_corpus;
mod binary_construction;
mod block_value_arity_agreement;
mod branch_context;
mod call_site_lines;
mod cascade_operand_state_corpus;
mod class_literal_signal;
mod class_side_operand_producer_corpus;
mod closed_fun_state_version;
mod closure_self_send_state_guard_corpus;
mod constant_class_methods;
mod control_flow;
mod cross_file_abstract;
mod cross_file_self_delegate;
mod dispatch;
mod duplicate_methods;
mod dynamic_site_report;
mod ensure_relay_corpus;
mod evaluated_block_arg_local_producer;
mod evaluated_block_grid_corpus;
mod evaluated_block_producer;
mod expect_directive_statement;
mod expressions;
mod ffi_conditional_selector_corpus;
mod ffi_delegate_direct_call;
mod ffi_direct_call;
mod field_assignment_value_position;
mod field_write_sites;
mod fold_acc_shape_agreement;
mod fold_locals_carried_corpus;
mod fresh_temp_namespace;
mod gen_server;
mod guard_returns;
mod guarded_op_loops;
mod hybrid_field_rebind_corpus;
mod hybrid_loop_nlr_corpus;
mod inline_block_local_in_loop_corpus;
mod keyed_fold_corpus;
mod late_reconcile_conformance;
mod local_assignment_producer;
mod local_producer_agreement;
mod local_producer_agreement_conditional;
mod local_producer_agreement_exception;
mod local_producer_agreement_lookup;
mod local_producer_agreement_repl;
mod local_rebind_verifier_corpus;
mod loop_body_conditional_receiver_corpus;
mod loop_body_self_send_operand_corpus;
mod match_fallthrough_goto;
mod match_fallthrough_sharing;
mod match_scrutinee_state_corpus;
mod module_info;
mod nlr;
mod nlr_closure_call_corpus;
mod number_comparison_right_operand;
mod open_reach;
mod operand_evaluation_order;
mod paren_arm_statements;
mod paren_peel_guard;
mod paren_transparency;
mod pipeline;
mod plain_arm_nested_construct_corpus;
mod primitives;
mod protocol_registration_conformance;
mod racing_operand_corpus;
mod recv_type;
mod repl_cached_binding_corpus;
mod residual_dynamic_sends;
mod shadowed_class_direct_calls;
mod sibling_snapshot;
mod stale_read_sites;
mod super_send_outside_producer;
mod super_send_value_position_corpus;
mod supervisor;
mod synced_call_sites_corpus;
mod synced_calls;
mod threaded_locals;
mod threaded_locals_agreement;
mod tier2_block_local_corpus;
mod tier2_block_local_inner_loop;
mod tier2_merge;
mod trait_flattening;
mod tuple_acc_state_read_corpus;
mod typed_protocol;
mod typed_sealed;
mod value_immutable_slot_hint_conformance;
mod value_inherited_target;
mod value_position_assignment_agreement;
mod value_type_conditional_family_verifier_corpus;
mod value_type_fold_family_corpus;
mod verifier_surfacing;
mod while_condition_binding_corpus;
mod while_condition_carrier_corpus;

#[test]
fn erlc_rejected_rows_names_exactly_the_bad_rows() {
    if !erlc_available() {
        return;
    }
    let good = "module 'm' [] attributes []\nend\n";
    let bad = "module 'm' [] attributes []\n'f'/0 = fun () -> Unbound\nend\n";
    // A good row after the first bad one is never reached by a batched
    // `erlc` (it stops at the first failure), and the rows share a module name.
    let rows = [("a", good), ("b", bad), ("c", good), ("d", bad)];
    let (rejected, _) = erlc_rejected_rows(&rows).expect_err("rows b and d are invalid");
    assert_eq!(rejected, vec![1, 3]);
    assert!(erlc_rejected_rows(&[("a", good), ("c", good)]).is_ok());
}
