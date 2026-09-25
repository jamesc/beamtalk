// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0127 §3/§10 (BT-3590): a `uses:` class's flattened provisions must
//! actually reach compiled Core Erlang output — through *both* codegen
//! entry points:
//!
//! - the self-sufficient path (`generate_module` with no `AnalysisResult`
//!   handed off — unit tests, ad-hoc codegen, REPL trace mode), which now
//!   flattens its own from-scratch hierarchy (`driver.rs`'s `else` arm);
//! - the driver-handoff path (`analyse_full` → `lower_module_for_codegen`
//!   → `generate_module().with_analysis(..)` — the CLI build pipeline),
//!   which now flattens the driver's own module inside
//!   `lower_module_for_codegen` (see that function's module doc, "Closing
//!   the flattening/codegen boundary").
//!
//! Before BT-3590, a `uses:` class type-checked correctly (`analyse_full`
//! flattened its own internal clone) but its provisions never reached
//! either codegen path — `ClassDefinition.methods`, which both
//! `generate_value_type_module` and `generate_actor_module` read directly,
//! never saw them.

use super::*;
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full, lower_module_for_codegen};

fn parse_fixture(src: &str) -> beamtalk_core::ast::Module {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .collect();
    assert!(errors.is_empty(), "fixture failed to parse: {errors:?}");
    module
}

/// Self-sufficient path, Value user: `generate_module` with no analysis
/// handed off must still emit the flattened `describe` function.
#[test]
fn self_sufficient_codegen_emits_a_flattened_provision_for_a_value_user() {
    let src = concat!(
        "Protocol define: Describable\n",
        "  describe -> String => \"a describable thing\"\n\n",
        "Value subclass: Report\n",
        "  uses: Describable\n",
    );
    let module = parse_fixture(src);
    let code = generate_module(&module, CodegenOptions::new("report"))
        .expect("codegen should succeed for a flattened Value class");
    assert!(
        code.contains("describe"),
        "expected the flattened `describe` provision to appear in generated code, got:\n{code}"
    );
}

/// Self-sufficient path, Actor user: same as above, but for an Actor
/// subclass (routes through `generate_actor_module`/`gen_server` codegen
/// instead of the value-type generator — ADR 0127 §10's "unchanged
/// generators" requirement covers both).
#[test]
fn self_sufficient_codegen_emits_a_flattened_provision_for_an_actor_user() {
    let src = concat!(
        "Protocol define: Greetable\n",
        "  greet -> String => \"hi\"\n\n",
        "Actor subclass: Greeter\n",
        "  uses: Greetable\n",
    );
    let module = parse_fixture(src);
    let code = generate_module(&module, CodegenOptions::new("greeter"))
        .expect("codegen should succeed for a flattened Actor class");
    assert!(
        code.contains("greet"),
        "expected the flattened `greet` provision to appear in generated code, got:\n{code}"
    );
}

/// A flattened provision whose *own* body needs state-threading (a
/// `timesRepeat:` loop mutating actor state) — pins that the self-sufficient
/// path flattens `uses:` **before** `compute_semantic_facts`/
/// `ClassHierarchy::build` run, not just before codegen proper. Both of
/// those walk `module.classes` directly (never `module.protocols`), so a
/// provision flattened only after they've already run would compile with
/// no `SemanticFacts` entry for its own body — silently degrading (rather
/// than erroring on) state-effect/dispatch-kind classification for exactly
/// the kind of body ADR 0127 says must "compile correctly in all three"
/// class kinds. A trivial one-line provision (this file's other tests)
/// can't exercise that ordering bug; a mutating loop can.
#[test]
fn self_sufficient_codegen_threads_state_correctly_inside_a_flattened_provision_loop() {
    let src = concat!(
        "Protocol define: Bumpable\n",
        "  bumpBy: n => n timesRepeat: [self.count := self.count + 1]\n\n",
        "Actor subclass: Widget\n",
        "  uses: Bumpable\n",
        "  state: count = 0\n",
    );
    let module = parse_fixture(src);
    let code = generate_module(&module, CodegenOptions::new("widget"))
        .expect("codegen should succeed for a flattened provision with a stateful loop");
    assert!(
        code.contains("bumpBy"),
        "expected the flattened `bumpBy:` provision in generated code, got:\n{code}"
    );
    // The loop's `self.count := self.count + 1` must actually thread state
    // back out of the block (ADR 0111/0118) — not just parse/compile.
    // Empirically confirmed regression fingerprint (reverting this fix's
    // reordering and diffing the generated Core Erlang for this exact
    // fixture): with facts computed on the *unflattened* module, this
    // `timesRepeat:` loop's `{Result, NewState}` return tuple is never
    // unpacked at all — the reply keeps using the actor's *original*
    // `State`, silently discarding the loop's `self.count` mutation on
    // every call. `'element'(2, …)` extracting the loop's returned state
    // is the fixed version's distinguishing feature; the buggy ordering
    // produces zero occurrences of it for this fixture.
    assert!(
        code.contains("'element'(2,"),
        "expected the loop's returned {{Result, NewState}} tuple to be \
         unpacked (state threaded back to the reply), got:\n{code}"
    );
}

/// The canonical CLI build pipeline: `analyse_full` →
/// `lower_module_for_codegen` → `generate_module(..).with_analysis(..)`.
/// This is the path `beam_compiler.rs` actually drives, and the one
/// BT-3590's module doc names as the gap ("the caller's own `Module`
/// … is never mutated").
///
/// **Single-file fixture, not a real package layout.** ADR 0127 §Constraints
/// ("One top-level definition per file") and its own body text ("It stays
/// one protocol per file … compiles to one module by the ADR 0119 rule")
/// both say a protocol and its user always live in *separate* `.bt` files
/// in real usage — `module_validator::validate_single_definition` enforces
/// exactly that, unconditionally, inside `analyse_full` itself. But
/// `trait_expansion::expand_module` (BT-3588) only ever resolves a `uses:`
/// against `module.protocols` — i.e. only a *same-file* protocol —
/// deliberately, until BT-3591 ("build-graph track protocol-user edges in
/// every compile path", explicitly Out of Scope for BT-3590) carries a
/// cross-file protocol's AST to its users. So there is no fixture shape
/// that is simultaneously (a) ADR-0127-legal (protocol and user in
/// different files) and (b) actually flattened by today's same-module-only
/// `expand_module`. This test deliberately puts both in one module purely
/// to exercise the `analyse_full` → `lower_module_for_codegen` →
/// `generate_module` hand-off wiring this issue closes, and filters out
/// the one expected "class and a protocol cannot be in the same file"
/// diagnostic that fixture shape necessarily trips — see this repo's
/// BT-3590 completion notes for the cross-file follow-up this implies.
#[test]
fn handed_off_analysis_pipeline_emits_a_flattened_provision() {
    let src = concat!(
        "Protocol define: Describable\n",
        "  describe -> String => \"a describable thing\"\n\n",
        "Value subclass: Report\n",
        "  uses: Describable\n",
    );
    let mut module = parse_fixture(src);
    let analysis = analyse_full(&module, AnalysisContext::default());
    let unexpected: Vec<_> = analysis
        .diagnostics
        .iter()
        .filter(|d| !d.message.contains("cannot be in the same file"))
        .collect();
    assert!(
        unexpected.is_empty(),
        "unexpected analysis diagnostics: {unexpected:?}"
    );

    lower_module_for_codegen(
        &mut module,
        &analysis.class_hierarchy,
        &analysis.method_return_types,
        &analysis.external_protocols,
    );
    // The driver's own module must already carry the flattened method
    // before codegen ever runs — this is the AST-level half of the fix.
    let report = module
        .classes
        .iter()
        .find(|c| c.name.name == "Report")
        .expect("Report class present");
    assert!(
        report
            .methods
            .iter()
            .any(|m| m.selector.name() == "describe"),
        "expected lower_module_for_codegen to splice the flattened `describe` \
         provision into the driver's own module"
    );

    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("report").with_analysis(analysis),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("describe"),
        "expected the flattened `describe` provision in the generated code, got:\n{code}"
    );
}

/// Phase 0 pin (Linear AC): a subclass override of a flattened provision
/// must be the one actually reached from an inherited actor method's
/// self-send — records the dynamic-dispatch binding ADR 0127 §6 relies on
/// (flattening only splices the provision into the *class that uses the
/// protocol*; a further subclass overriding that same selector must still
/// win via ordinary self-send dispatch, exactly as it would for a
/// hand-written method).
#[test]
fn subclass_override_of_a_flattened_provision_is_reached_from_inherited_self_send() {
    let src = concat!(
        "Protocol define: Describable\n",
        "  describe -> String => \"protocol default\"\n\n",
        "  announce -> String => self describe\n\n",
        "Actor subclass: Base\n",
        "  uses: Describable\n\n",
        "Base subclass: Override\n",
        "  describe -> String => \"overridden\"\n",
    );
    let module = parse_fixture(src);
    let code =
        generate_module(&module, CodegenOptions::new("override")).expect("codegen should succeed");

    // `Override` must define its own `describe/1` (the flattened
    // `announce` stays inherited from `Base` — Beamtalk classes don't
    // re-splice a superclass's already-flattened methods into every
    // subclass, only ordinary inheritance applies from here on).
    assert!(
        code.contains("describe"),
        "expected Override's own describe/1 override in generated code, got:\n{code}"
    );
    assert!(
        code.contains("announce"),
        "expected Base's flattened announce/1 (self describe) in generated code, got:\n{code}"
    );
}
