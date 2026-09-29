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

/// Parses a protocol file and returns its provision-bearing protocol.
fn parse_protocol_file(src: &str) -> beamtalk_core::ast::ProtocolDefinition {
    parse_fixture(src)
        .protocols
        .into_iter()
        .find(|p| !p.provided_methods.is_empty())
        .expect("fixture must declare a provision-bearing protocol")
}

const DESCRIBABLE_FILE: &str = concat!(
    "// header comment so the provision is not on line 1\n",
    "// second header line\n\n",
    "Protocol define: Describable\n",
    "  describe -> String => \"a describable thing\"\n",
);

const REPORT_FILE: &str = concat!("Value subclass: Report\n", "  uses: Describable\n");

/// Runs the CLI build pipeline for `user_src` against one cross-file
/// protocol: `analyse_full` (protocol carried as `pre_loaded_protocol_defs`)
/// → `lower_module_for_codegen` → `generate_module(..).with_analysis(..)`.
fn generate_cross_file(
    user_src: &str,
    protocol_src: &str,
    protocol_path: &str,
    with_protocol_source: bool,
) -> String {
    let protocol = parse_protocol_file(protocol_src);
    let mut module = parse_fixture(user_src);
    let analysis = analyse_full(
        &module,
        AnalysisContext::default().with_pre_loaded_protocol_defs(vec![protocol.clone()]),
    );
    let errors: Vec<_> = analysis
        .diagnostics
        .iter()
        .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "a cross-file `uses:` is legal — no analysis errors expected: {errors:?}"
    );
    lower_module_for_codegen(
        &mut module,
        &analysis.class_hierarchy,
        &analysis.method_return_types,
        &analysis.external_protocols,
    );
    let mut options = CodegenOptions::new("report")
        .with_source(user_src)
        .with_source_path_opt(Some("report.bt"))
        .with_analysis(analysis);
    if with_protocol_source {
        options = options.with_protocol_sources(
            [(
                protocol.name.name.clone(),
                beamtalk_core::semantic_analysis::ProtocolSource {
                    path: Some(protocol_path.into()),
                    text: protocol_src.into(),
                },
            )]
            .into_iter()
            .collect(),
        );
    }
    generate_module(&module, options).expect("codegen should succeed")
}

/// The canonical CLI build pipeline over a *legal* cross-file layout (ADR
/// 0127: protocol and user in separate files): `analyse_full` →
/// `lower_module_for_codegen` → `generate_module(..).with_analysis(..)`.
/// The user's own module must carry the flattened method before codegen
/// runs, and codegen must emit it.
#[test]
fn handed_off_analysis_pipeline_emits_a_flattened_cross_file_provision() {
    let protocol = parse_protocol_file(DESCRIBABLE_FILE);
    let mut module = parse_fixture(REPORT_FILE);
    let analysis = analyse_full(
        &module,
        AnalysisContext::default().with_pre_loaded_protocol_defs(vec![protocol]),
    );
    assert!(
        analysis.diagnostics.is_empty(),
        "unexpected analysis diagnostics: {:?}",
        analysis.diagnostics
    );
    lower_module_for_codegen(
        &mut module,
        &analysis.class_hierarchy,
        &analysis.method_return_types,
        &analysis.external_protocols,
    );
    assert!(
        module.classes[0]
            .methods
            .iter()
            .any(|m| m.selector.name() == "describe"),
        "expected lower_module_for_codegen to splice the flattened `describe` \
         provision into the driver's own module"
    );
    let code = generate_module(
        &module,
        CodegenOptions::new("report").with_analysis(analysis),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("describe"),
        "expected the flattened `describe` provision in the generated code, got:\n{code}"
    );
}

/// ADR 0127 §3 "Source locations": a flattened method's BEAM line
/// annotation must name the *protocol's* file and a line of *that* file.
/// `describe` is on line 5 of `describable.bt`; `report.bt` has only two
/// lines, so a line mapped through the user's source could never say 5.
#[test]
fn flattened_provision_line_annotation_points_into_the_protocol_file() {
    let code = generate_cross_file(REPORT_FILE, DESCRIBABLE_FILE, "pkg/describable.bt", true);
    assert!(
        code.contains("{'file', \"pkg/describable.bt\"}") && code.contains("[5,"),
        "expected the flattened `describe` to be annotated with \
         describable.bt line 5, got:\n{code}"
    );
}

/// Counterpart: without the protocol's source (a dependency checkout the
/// build did not carry, say) codegen falls back to the pre-BT-3625
/// behaviour rather than failing — no protocol path appears.
#[test]
fn flattened_provision_without_protocol_source_still_compiles() {
    let code = generate_cross_file(REPORT_FILE, DESCRIBABLE_FILE, "pkg/describable.bt", false);
    assert!(
        !code.contains("pkg/describable.bt"),
        "no protocol path expected when no protocol source was supplied, got:\n{code}"
    );
    assert!(code.contains("describe"));
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

/// ADR 0127 §12 (BT-3594): `generate_protocol_registrations` must bake the
/// protocol's own *provided* methods into its `register_protocol/1` call —
/// `beamtalk_protocol_registry:provided_methods/1` (backing `Protocol
/// providedMethods:` and, indirectly, browse/reflection tooling built on
/// it) reads the `provided_methods` key straight off the registered map.
/// Before this fix the registration call carried only `required_methods`/
/// `required_class_methods`, so `Protocol providedMethods: #AnyRealTrait`
/// silently answered `[]` for every protocol with real provisions.
#[test]
fn protocol_registration_bakes_provided_methods_key() {
    let src = concat!(
        "Protocol define: Bt3594RegComparable\n",
        "  < other :: Self -> Boolean\n",
        "  > other :: Self -> Boolean => other < self\n",
    );
    let module = parse_fixture(src);
    let code = generate_module(&module, CodegenOptions::new("bt3594_reg_comparable"))
        .expect("codegen should succeed");
    assert!(
        code.contains("'provided_methods' => [~{'selector' => '>'"),
        "expected the provided `>` method to be baked into the protocol's \
         'provided_methods' registration key, got:\n{code}"
    );
}

/// BT-3626: on the self-sufficient path, `expand_module`'s diagnostics must
/// be surfaced as warnings, not discarded — no `analyse_full` ran to report
/// an unknown `uses:` protocol.
#[test]
fn self_sufficient_codegen_surfaces_expand_module_diagnostics() {
    let src = concat!("Value subclass: Report\n", "  uses: NoSuchProtocol\n");
    let module = parse_fixture(src);
    let generated =
        crate::core_erlang::generate_module_with_warnings(&module, CodegenOptions::new("report"))
            .expect("codegen should still succeed");
    assert!(
        generated
            .warnings
            .iter()
            .any(|d| d.message.contains("NoSuchProtocol")),
        "expected an unknown-protocol diagnostic in warnings, got: {:?}",
        generated.warnings
    );
}

/// BT-3626: a flattened provision with no return-type annotation must get an
/// inferred return type and a `provenance => protocol` origin on the
/// self-sufficient path, like on the driver-handoff path.
#[test]
fn self_sufficient_codegen_infers_return_type_and_origin_for_flattened_provision() {
    let src = concat!(
        "Protocol define: Answerable\n",
        "  answer => 42\n\n",
        "Value subclass: Oracle\n",
        "  uses: Answerable\n",
    );
    let module = parse_fixture(src);
    let code =
        generate_module(&module, CodegenOptions::new("oracle")).expect("codegen should succeed");
    assert!(
        code.contains("'provenance' => 'protocol'"),
        "expected the flattened provision's origin to be stamped, got:\n{code}"
    );
    assert!(
        code.contains(
            "'answer' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer'"
        ),
        "expected an inferred return type for `answer`, got:\n{code}"
    );
}
