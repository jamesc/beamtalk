// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for the `cross_file_resolution` side of the stdlib build.
//!
//! **DDD Context:** CLI / Build System

use super::super::metadata::*;
use super::super::orchestrate::*;
use super::temp_utf8_dir;
use crate::beam_compiler::{ClassHierarchyContext, CompileContext, compile_source_with_bindings};
use std::fs;

/// End-to-end regression fixture for stdlib's own cross-file
/// type-alias resolution, proving the full round trip
/// `build_stdlib.rs` relies on: `collect_stdlib_alias_sources` (a live,
/// same-run pre-pass — *not* a seed from a previously-persisted
/// `generated_builtins.rs` snapshot, see that function's doc for why)
/// → `ClassHierarchyContext::pre_loaded_aliases` → cross-file resolution
/// during a real compile, in the very same run the alias was first
/// declared in. Mirrors the shape of `build.rs`'s
/// `test_cross_file_alias_resolution_no_false_type_mismatch`,
/// adapted to `build_stdlib`'s own manifest-less compile path
/// (`compile_source_with_bindings`, no package/`build_class_module_index`).
#[test]
fn test_cross_file_alias_resolution_seeds_pre_loaded_aliases() {
    let (_temp, lib_dir) = temp_utf8_dir();

    // File 1: declares the alias and a class with a method that returns
    // it. `A` is a `Value` subclass so `A new` is instantiable (`Object`
    // classes are abstract and not directly instantiable).
    fs::write(
        lib_dir.join("AliasFixtureA.bt"),
        "type Direction = #north | #south | #east | #west\n\
         Value subclass: AliasFixtureA\n  heading -> Direction => #north\n",
    )
    .unwrap();

    // File 2: consumes A's alias-typed return value as an argument to a
    // parameter typed with the spelled-out equivalent union — the exact
    // shape of the real `RestartStrategy`/`Timeout` stdlib bug this
    // fixture guards against.
    fs::write(
        lib_dir.join("AliasFixtureB.bt"),
        "Object subclass: AliasFixtureB\n  \
         useDirection: d :: #north | #south | #east | #west => d\n  \
         test => self useDirection: AliasFixtureA new heading\n",
    )
    .unwrap();

    let file_a = lib_dir.join("AliasFixtureA.bt");
    let file_b = lib_dir.join("AliasFixtureB.bt");

    // Mirrors build_stdlib()'s own per-file class extraction.
    let source_a = fs::read_to_string(&file_a).unwrap();
    let tokens_a = beamtalk_core::source_analysis::lex_with_eof(&source_a);
    let (module_a, _diags) = beamtalk_core::source_analysis::parse(tokens_a);
    let pre_loaded_classes =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::extract_class_infos(
            &module_a,
        );
    assert_eq!(pre_loaded_classes.len(), 1, "expected one class in file A");

    // The real function under test, called the way `build_stdlib()`
    // actually calls it — over *all* source files in one pass, before
    // any compile happens — proving this is a live scan (file A's alias
    // is seeded immediately, in the same call, with no dependency on any
    // prior "generated" snapshot existing).
    let alias_sources = collect_stdlib_alias_sources(&[file_a.clone(), file_b.clone()]).unwrap();
    assert_eq!(
        alias_sources.len(),
        1,
        "expected exactly one alias declaration, from file A (file B declares none)"
    );
    assert!(alias_sources[0].text.starts_with("type Direction ="));
    assert_eq!(alias_sources[0].info.name.as_str(), "Direction");

    let pre_loaded_aliases = stdlib_pre_loaded_aliases(&alias_sources);
    assert_eq!(pre_loaded_aliases.len(), 1);
    assert_eq!(pre_loaded_aliases[0].name.as_str(), "Direction");

    let options = stdlib_compiler_options(false);
    let bindings = beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new();

    // Compile B with cross-file class info AND cross-file alias info —
    // should produce no "Argument ... expects" type-mismatch warning.
    let core_file = lib_dir.join("b.core");
    let diagnostics = compile_source_with_bindings(
        &file_b,
        "bt@stdlib@alias_fixture_b",
        &core_file,
        &options,
        &bindings,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                pre_loaded_classes: pre_loaded_classes.clone(),
                pre_loaded_aliases: pre_loaded_aliases.clone(),
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("cross-file alias-typed argument should compile without errors");

    assert!(
        diagnostics.iter().all(|d| !d.message.contains("Argument")),
        "expected no false argument-type-mismatch diagnostic once cross-file \
         aliases are seeded, got: {diagnostics:?}"
    );

    // Negative control: WITHOUT pre_loaded_aliases (but still with
    // pre_loaded_classes), the same compile reproduces the false
    // positive cross-file alias resolution fixes — proving this test
    // actually exercises the fix rather than a scenario that never warned.
    let core_file_unfixed = lib_dir.join("b_unfixed.core");
    let diagnostics_unfixed = compile_source_with_bindings(
        &file_b,
        "bt@stdlib@alias_fixture_b_unfixed",
        &core_file_unfixed,
        &options,
        &bindings,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                pre_loaded_classes,
                // No pre_loaded_aliases — reproduces the unresolved-alias gap.
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("compile should still succeed even with the false-positive warning");

    assert!(
        diagnostics_unfixed
            .iter()
            .any(|d| d.message.contains("Argument")),
        "expected the negative control (no pre_loaded_aliases) to reproduce \
         the pre-BT-2935 false positive, got: {diagnostics_unfixed:?}"
    );
}

/// An `internal type` stdlib alias must resolve cross-file
/// *within* stdlib. `stdlib_pre_loaded_aliases` stamps every entry
/// `package: Some("stdlib")` and `build_stdlib()`'s `CompilerOptions`
/// set `current_package: Some("stdlib")` to match —
/// `AliasRegistry::add_pre_loaded`'s seeding-boundary exclusion
/// (ADR 0108) drops an `internal` entry whose `package` differs from
/// `current_package`, so a mismatch between the two identities would
/// silently drop the alias and surface as a confusing unresolved-type
/// diagnostic in the consuming file.
#[test]
fn test_internal_alias_resolves_cross_file_within_stdlib() {
    let (_temp, lib_dir) = temp_utf8_dir();

    // File A declares a package-private alias.
    let file_a = lib_dir.join("InternalAliasFixtureA.bt");
    fs::write(
        &file_a,
        "internal type Direction = #north | #south | #east | #west\n\
         Object subclass: InternalAliasFixtureA\n  noop => nil\n",
    )
    .unwrap();

    // File B consumes it in an `internal` method signature — the one
    // position ADR 0108 permits an internal alias outside its declaring
    // file (a public signature would be an E0402 leak).
    let file_b = lib_dir.join("InternalAliasFixtureB.bt");
    fs::write(
        &file_b,
        "Object subclass: InternalAliasFixtureB\n  \
         internal go: d :: Direction -> Symbol => d\n",
    )
    .unwrap();

    let alias_sources = collect_stdlib_alias_sources(&[file_a.clone(), file_b.clone()]).unwrap();
    assert_eq!(alias_sources.len(), 1, "expected file A's alias only");
    assert!(alias_sources[0].info.is_internal);

    // The real stamping function under test.
    let pre_loaded_aliases = stdlib_pre_loaded_aliases(&alias_sources);
    assert_eq!(pre_loaded_aliases[0].package.as_deref(), Some("stdlib"));

    // `build.rs`'s `package_identity` names the *other* stdlib
    // compile path — manifest-less `beamtalk build --stdlib-mode <dir>`,
    // what `just dialyzer-specs` runs — with `STDLIB_PACKAGE_MARKER`. The
    // literal hardcoded here and that constant must stay the same string,
    // or the two paths would disagree about stdlib's package identity and
    // only one of them would seed `internal` aliases.
    assert_eq!(
        pre_loaded_aliases[0].package.as_deref(),
        Some(beamtalk_language_service::STDLIB_PACKAGE_MARKER),
        "stdlib_pre_loaded_aliases' stamp must match STDLIB_PACKAGE_MARKER"
    );
    assert_eq!(
        stdlib_compiler_options(false).current_package.as_deref(),
        Some(beamtalk_language_service::STDLIB_PACKAGE_MARKER),
        "stdlib_compiler_options' current_package must match STDLIB_PACKAGE_MARKER"
    );

    // The exact options `build_stdlib()` runs with — crucially with
    // `current_package: Some("stdlib")` matching the stamp above.
    let options = stdlib_compiler_options(false);
    let bindings = beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new();

    let core_file = lib_dir.join("internal_b.core");
    let diagnostics = compile_source_with_bindings(
        &file_b,
        "bt@stdlib@internal_alias_fixture_b",
        &core_file,
        &options,
        &bindings,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                pre_loaded_aliases: pre_loaded_aliases.clone(),
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("internal stdlib alias should resolve cross-file within stdlib");
    assert!(
        diagnostics.iter().all(|d| !d.message.contains("Direction")),
        "expected no diagnostic about `Direction`, got: {diagnostics:?}"
    );

    // Negative control at the analysis level: a mismatched
    // `current_package` makes the seeding-boundary exclusion drop the
    // internal alias — proving the compile above passes because the two
    // "stdlib" identities line up, not because the boundary is
    // unenforced.
    let source_b = fs::read_to_string(&file_b).unwrap();
    let tokens_b = beamtalk_core::source_analysis::lex_with_eof(&source_b);
    let (module_b, _diags) = beamtalk_core::source_analysis::parse(tokens_b);
    let extensions = beamtalk_core::compilation::extension_index::ExtensionIndex::default();

    let mismatched = beamtalk_core::CompilerOptions {
        current_package: Some("not_stdlib".into()),
        ..options.clone()
    };
    let result = beamtalk_core::semantic_analysis::analyse_full(
        &module_b,
        beamtalk_core::semantic_analysis::AnalysisContext::default()
            .with_options(&mismatched)
            .with_pre_loaded_aliases(pre_loaded_aliases.clone())
            .with_cross_file_extensions(&extensions),
    );
    assert!(
        !result.alias_registry.has_alias("Direction"),
        "a mismatched current_package must drop the internal stdlib alias \
         at the seeding boundary"
    );

    let result = beamtalk_core::semantic_analysis::analyse_full(
        &module_b,
        beamtalk_core::semantic_analysis::AnalysisContext::default()
            .with_options(&options)
            .with_pre_loaded_aliases(pre_loaded_aliases)
            .with_cross_file_extensions(&extensions),
    );
    assert!(
        result.alias_registry.has_alias("Direction"),
        "the matching \"stdlib\" current_package must seed the internal alias"
    );
}

/// End-to-end regression fixture for stdlib's own cross-file
/// protocol resolution, mirroring
/// `test_internal_alias_resolves_cross_file_within_stdlib`/
/// `test_cross_file_alias_resolution_seeds_pre_loaded_aliases` above but
/// for `pre_loaded_protocols` instead of `pre_loaded_aliases`. Proves the
/// full round trip `build_stdlib()` relies on:
/// `collect_stdlib_protocol_infos` (a live, same-run pre-pass — same
/// rationale as `collect_stdlib_alias_sources`) →
/// `ClassHierarchyContext::pre_loaded_protocols` → the protocol getting
/// registered into file B's own `ProtocolRegistry` during semantic
/// analysis, in the very same run the protocol was first declared in.
///
/// This is the exact shape of the real stdlib bug: `console.bt`/`json.bt`
/// reference `Printable` (declared in `printable.bt`) in a `::
/// Printable` parameter annotation — a different file entirely, compiled
/// independently. Without this pre-pass, `ProtocolRegistry::has_protocol` for a
/// cross-file protocol name is always `false` while compiling any other
/// stdlib file, which — per `check_protocol_argument_conformance`'s early
/// return when `protocol_registry.get(name)` is `None` — makes structural
/// protocol-conformance checking on `Printable`-typed arguments silently
/// a no-op *everywhere* in stdlib, with no diagnostic at all (not even a
/// warning): a silent degrade-to-unresolved failure mode. That's why this test asserts on
/// `protocol_registry.has_protocol` directly instead of scanning for a
/// diagnostic message — there is no diagnostic to find; the whole point
/// of the bug is that the check never ran.
#[test]
fn test_cross_file_protocol_resolution_seeds_pre_loaded_protocols() {
    let (_temp, lib_dir) = temp_utf8_dir();

    // File A: declares the protocol, mirroring `printable.bt`.
    let file_a = lib_dir.join("ProtocolFixtureA.bt");
    fs::write(
        &file_a,
        "Protocol define: ProtocolFixtureA\n  asString -> String\n",
    )
    .unwrap();

    // File B: references it in a parameter type annotation — the exact
    // shape of `console.bt`'s `printLine: aValue :: Printable -> Nil`.
    let file_b = lib_dir.join("ProtocolFixtureB.bt");
    fs::write(
        &file_b,
        "Object subclass: ProtocolFixtureB\n  \
         show: aValue :: ProtocolFixtureA -> Nil => nil\n",
    )
    .unwrap();

    // The real function under test, called the way `build_stdlib()`
    // actually calls it — over *all* source files in one pass, before
    // any compile happens (a live scan, not a seed from any prior
    // "generated" snapshot).
    let (protocol_infos, _protocol_defs, _sources) =
        collect_stdlib_protocol_infos(&[file_a.clone(), file_b.clone()]);
    assert_eq!(
        protocol_infos.len(),
        1,
        "expected exactly one protocol declaration, from file A"
    );
    assert_eq!(protocol_infos[0].name.as_str(), "ProtocolFixtureA");

    let options = stdlib_compiler_options(false);
    let bindings = beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new();

    // Compile B the way `build_stdlib()` actually does — through
    // `compile_source_with_bindings` with `pre_loaded_protocols` seeded
    // via `CompileContext`/`ClassHierarchyContext` — and confirm the
    // cross-file `:: ProtocolFixtureA` parameter annotation compiles
    // cleanly end to end.
    let core_file = lib_dir.join("b.core");
    compile_source_with_bindings(
        &file_b,
        "bt@stdlib@protocol_fixture_b",
        &core_file,
        &options,
        &bindings,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                pre_loaded_protocols: protocol_infos.clone(),
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("cross-file protocol-typed parameter should compile without errors");

    // The real mechanism under test: does file B's own semantic analysis
    // actually *know about* the cross-file protocol afterwards?
    // `compile_source_with_bindings` doesn't expose the `ProtocolRegistry`
    // it built, so re-run semantic analysis directly (same
    // `pre_loaded_protocols` input `compile_ctx.hierarchy` carries) to
    // inspect it.
    let source_b = fs::read_to_string(&file_b).unwrap();
    let tokens_b = beamtalk_core::source_analysis::lex_with_eof(&source_b);
    let (module_b, _diags) = beamtalk_core::source_analysis::parse(tokens_b);
    let extensions = beamtalk_core::compilation::extension_index::ExtensionIndex::default();

    let result = beamtalk_core::semantic_analysis::analyse_full(
        &module_b,
        beamtalk_core::semantic_analysis::AnalysisContext::default()
            .with_options(&options)
            .with_pre_loaded_protocols(protocol_infos)
            .with_cross_file_extensions(&extensions),
    );
    assert!(
        result.protocol_registry.has_protocol("ProtocolFixtureA"),
        "expected pre_loaded_protocols to seed the cross-file protocol into \
         file B's own protocol registry"
    );

    // Negative control: WITHOUT pre_loaded_protocols, the cross-file
    // protocol never gets registered — proving the
    // assertion above exercises the fix rather than a tautology.
    let result_unfixed = beamtalk_core::semantic_analysis::analyse_full(
        &module_b,
        beamtalk_core::semantic_analysis::AnalysisContext::default()
            .with_options(&options)
            .with_cross_file_extensions(&extensions),
    );
    assert!(
        !result_unfixed
            .protocol_registry
            .has_protocol("ProtocolFixtureA"),
        "expected the negative control (no pre_loaded_protocols) to reproduce \
         the pre-BT-3034 gap: the cross-file protocol should NOT be registered"
    );
}

/// ADR 0127 §10a / BT-3591: a stdlib class's `uses:` line naming a
/// *trait* (a provision-bearing protocol) declared in a different
/// stdlib file must flatten — `collect_stdlib_protocol_infos`'s
/// `ProtocolInfo` pre-pass alone (the test above) only carries
/// name/signature metadata, not the provided method bodies
/// `trait_expansion::expand_module` needs to splice in.
#[test]
fn test_cross_file_trait_flattening_seeds_pre_loaded_protocol_defs() {
    let (_temp, lib_dir) = temp_utf8_dir();

    // File A: declares a trait (a provision-bearing protocol), mirroring
    // a hypothetical stdlib `comparable.bt`.
    let file_a = lib_dir.join("TraitFixtureA.bt");
    fs::write(
        &file_a,
        "Protocol define: TraitFixtureA\n  \
         < other :: Self -> Boolean\n\n  \
         max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]\n",
    )
    .unwrap();

    // File B: `uses:` it, the exact cross-file shape ordinary
    // `beamtalk build`/the LSP already flatten via
    // `pre_loaded_protocol_defs` — stdlib needed the same wiring.
    let file_b = lib_dir.join("TraitFixtureB.bt");
    fs::write(
        &file_b,
        "Value subclass: TraitFixtureB\n  \
         uses: TraitFixtureA\n  \
         field: n :: Integer = 0\n\n  \
         < other :: TraitFixtureB -> Boolean => self.n < other n\n",
    )
    .unwrap();

    let (_protocol_infos, protocol_defs, _sources) =
        collect_stdlib_protocol_infos(&[file_a.clone(), file_b.clone()]);
    assert_eq!(
        protocol_defs.len(),
        1,
        "expected exactly one provision-bearing protocol, from file A"
    );

    let options = stdlib_compiler_options(false);
    let bindings = beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new();
    let core_file = lib_dir.join("b.core");

    // Fixed (with pre_loaded_protocol_defs): compiles cleanly, `max:`
    // flattens in.
    let diags = compile_source_with_bindings(
        &file_b,
        "bt@stdlib@trait_fixture_b",
        &core_file,
        &options,
        &bindings,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                pre_loaded_protocol_defs: protocol_defs.clone(),
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("cross-file trait usage should compile without errors");
    assert!(
        !diags.iter().any(|d| d.message.contains("unknown protocol")),
        "expected the cross-file trait to resolve, got: {diags:?}"
    );

    // Negative control: WITHOUT pre_loaded_protocol_defs, `uses:
    // TraitFixtureA` cannot resolve — an "unknown protocol" error, which
    // `compile_source_with_bindings` surfaces as an `Err` (unlike a
    // warning/hint, an error-severity diagnostic fails the compile) —
    // proving the assertion above exercises the fix rather than a
    // tautology.
    let result_unfixed = compile_source_with_bindings(
        &file_b,
        "bt@stdlib@trait_fixture_b",
        &core_file,
        &options,
        &bindings,
        &CompileContext::default(),
        None,
    );
    assert!(
        result_unfixed.is_err(),
        "expected the negative control (no pre_loaded_protocol_defs) to fail \
         to compile — `uses: TraitFixtureA` cannot resolve without it"
    );
}
