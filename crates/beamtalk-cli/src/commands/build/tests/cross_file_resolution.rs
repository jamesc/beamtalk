// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-file class/alias resolution tests: the class-module index, subdirectory actor spawns, inheritance, alias resolution (including stdlib-mode and manifest-less builds), and dependency protocol/alias-info merging.

use super::*;

#[test]
fn test_build_class_module_index_skips_unreadable_file() {
    // A path that does not exist on disk — simulates an unreadable file.
    // The function should succeed (not error) and return an empty index.
    let nonexistent = Utf8PathBuf::from("/nonexistent/no_such_file.bt");
    let result = build_class_module_index(&[nonexistent], None, "my_app");
    assert!(result.is_ok());
    let (module_index, superclass_index, _class_infos, _extensions, _cached_asts) = result.unwrap();
    assert!(module_index.is_empty());
    assert!(superclass_index.is_empty());
}

#[test]
fn test_build_class_module_index_handles_parse_errors_gracefully() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let bad_file = project_path.join("bad.bt");
    // Write a file with a syntax error — parse diagnostics should be non-empty.
    write_test_file(&bad_file, "class Foo { @@@@invalid syntax }");

    // Should succeed and return whatever classes (if any) were parsed before the error.
    let result = build_class_module_index(&[bad_file], None, "my_app");
    assert!(result.is_ok());
}

/// BT-906: Verify that `build_class_module_index` correctly maps actor classes
/// defined in subdirectories to their full subdirectory-qualified module names.
///
/// When a package has `src/observer/event_bus.bt` defining `EventBus`, the index
/// must map `EventBus → bt@gang_of_four@observer@event_bus` (not the heuristic
/// fallback `bt@gang_of_four@event_bus` which drops the `observer@` segment).
#[test]
fn test_build_class_module_index_subdirectory_actor_class() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    let observer_path = src_path.join("observer");
    fs::create_dir_all(&observer_path).unwrap();

    write_test_file(
        &observer_path.join("event_bus.bt"),
        "Actor subclass: EventBus\n  notify: e => nil\n",
    );

    let source_files = vec![observer_path.join("event_bus.bt")];
    let (index, _superclass, _class_infos, _extensions, _cached_asts) =
        build_class_module_index(&source_files, Some(&src_path), "gang_of_four").unwrap();

    assert_eq!(
        index.get("EventBus").map(String::as_str),
        Some("bt@gang_of_four@observer@event_bus"),
        "EventBus in observer/ subdirectory must map to full subdirectory-qualified module name"
    );
}

/// BT-906: Verify end-to-end that cross-file actor spawn uses the correct
/// subdirectory-qualified module path in the generated Core Erlang.
///
/// When `src/main.bt` calls `EventBus spawn` and `EventBus` is defined in
/// `src/observer/event_bus.bt`, the generated Core Erlang must reference
/// `bt@gang_of_four@observer@event_bus` (not `bt@gang_of_four@event_bus`).
#[test]
fn test_cross_file_subdirectory_actor_spawn_uses_correct_module() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    let observer_path = src_path.join("observer");
    fs::create_dir_all(&observer_path).unwrap();

    // File 1: EventBus actor in observer/ subdirectory
    write_test_file(
        &observer_path.join("event_bus.bt"),
        "Actor subclass: EventBus\n  notify: e => nil\n",
    );

    // File 2: main.bt that references EventBus spawn
    write_test_file(
        &src_path.join("main.bt"),
        "Actor subclass: Main\n  run => EventBus spawn\n",
    );

    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();

    let source_files = vec![observer_path.join("event_bus.bt"), src_path.join("main.bt")];
    let (class_module_index, class_superclass_index, all_class_infos, _extensions, _cached_asts) =
        build_class_module_index(&source_files, Some(&src_path), "gang_of_four").unwrap();

    // Build the class index — EventBus should map to observer subdir module
    assert_eq!(
        class_module_index.get("EventBus").map(String::as_str),
        Some("bt@gang_of_four@observer@event_bus"),
        "Index pass should map EventBus to subdirectory module"
    );

    // Compile main.bt with the index
    let core_file = build_dir.join("bt@gang_of_four@main.core");
    let options = default_options();
    compile_file(
        &src_path.join("main.bt"),
        "bt@gang_of_four@main",
        &core_file,
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index: class_module_index.clone(),
                class_superclass_index: class_superclass_index.clone(),
                pre_loaded_classes: all_class_infos.clone(),
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .unwrap();

    let core_content = fs::read_to_string(&core_file).unwrap();

    // The spawn call must reference the full subdirectory-qualified module name
    assert!(
        core_content.contains("bt@gang_of_four@observer@event_bus"),
        "Spawn call should use observer@ subdirectory module. Generated Core Erlang:\n{core_content}"
    );
    assert!(
        !core_content.contains("'bt@gang_of_four@event_bus'"),
        "Should NOT use heuristic module name that drops observer@ segment. Generated:\n{core_content}"
    );
}

/// BT-1523: Verify cross-file class hierarchy resolution.
///
/// When `InheritingCounter` (file 2) subclasses `Counter` (file 1) and calls
/// `self getValue`, the type checker should NOT emit a DNU warning because
/// `getValue` is inherited from `Counter` via the cross-file `ClassInfo` injection.
#[test]
fn test_cross_file_inheritance_no_false_dnu_warning() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();

    // File 1: Counter with getValue method and count state
    write_test_file(
        &src_path.join("counter.bt"),
        "Actor subclass: Counter\n  state: count = 0\n  getValue => self.count\n  increment => self.count := self.count + 1\n",
    );

    // File 2: InheritingCounter that calls inherited getValue
    write_test_file(
        &src_path.join("inheriting_counter.bt"),
        "Counter subclass: InheritingCounter\n  getDoubled => self getValue * 2\n",
    );

    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();

    let source_files = vec![
        src_path.join("counter.bt"),
        src_path.join("inheriting_counter.bt"),
    ];
    let (class_module_index, class_superclass_index, all_class_infos, _extensions, _cached_asts) =
        build_class_module_index(&source_files, Some(&src_path), "test_pkg").unwrap();

    // Verify ClassInfo extraction captured Counter's methods
    assert!(
        all_class_infos.iter().any(|ci| ci.name == "Counter"),
        "Pass 1 should extract ClassInfo for Counter"
    );
    let counter_info = all_class_infos
        .iter()
        .find(|ci| ci.name == "Counter")
        .unwrap();
    assert!(
        counter_info
            .methods
            .iter()
            .any(|m| m.selector == "getValue"),
        "Counter ClassInfo should include getValue method"
    );

    // Compile InheritingCounter with cross-file class infos — should succeed
    // (no false DNU error for getValue which is inherited from Counter)
    let core_file = build_dir.join("bt@test_pkg@inheriting_counter.core");
    let options = default_options();
    compile_file(
        &src_path.join("inheriting_counter.bt"),
        "bt@test_pkg@inheriting_counter",
        &core_file,
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index: class_module_index.clone(),
                class_superclass_index: class_superclass_index.clone(),
                pre_loaded_classes: all_class_infos.clone(),
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("Cross-file inheritance should compile without errors");

    assert!(core_file.exists());
}

/// BT-2928: cross-file/package type-alias resolution for a manifest-based
/// package build's Pass 1.
///
/// File A declares `type Direction = ...` and a method returning it;
/// file B calls that method and passes the result as an argument to a
/// parameter typed with the *same* union spelled out directly (mirroring
/// the real `stdlib/src/actor.bt` / `stdlib/src/supervision_spec.bt`
/// `RestartStrategy` bug this issue fixes: the declared parameter side
/// already resolved correctly same-file, but the cross-file argument's
/// inferred type — read back from `A`'s `ClassInfo`/`MethodInfo` as an
/// opaque `"Direction"` string — never expanded through the alias table,
/// so it never matched the union's members). Before BT-2928's
/// `resolve_type_name_string` alias-awareness fix, this produced a
/// spurious "Argument 1 of 'useDirection:' ... expects ..., got
/// Direction" warning; after the fix, `A new heading`'s type expands to
/// the same union and no warning fires.
#[test]
fn test_cross_file_alias_resolution_no_false_type_mismatch() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();

    // File 1: declares the alias and a method that returns it. `A` is a
    // `Value` subclass so `A new` is instantiable (Object classes are
    // abstract and not directly instantiable).
    write_test_file(
        &src_path.join("a.bt"),
        "type Direction = #north | #south | #east | #west\n\
             Value subclass: A\n  heading -> Direction => #north\n",
    );

    // File 2: consumes A's alias-typed return value as an argument to a
    // parameter typed with the spelled-out equivalent union.
    write_test_file(
        &src_path.join("b.bt"),
        "Object subclass: B\n  \
             useDirection: d :: #north | #south | #east | #west => d\n  \
             test => self useDirection: A new heading\n",
    );

    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();

    let source_files = vec![src_path.join("a.bt"), src_path.join("b.bt")];
    let (class_module_index, class_superclass_index, all_class_infos, _extensions, _cached_asts) =
        build_class_module_index(&source_files, Some(&src_path), "test_pkg").unwrap();
    let all_alias_infos = collect_project_alias_infos(&source_files, "test_pkg");

    assert!(
        all_alias_infos.iter().any(|a| a.name == "Direction"),
        "Pass 1 should extract the Direction alias from a.bt"
    );

    // Compile B with cross-file class infos AND cross-file alias infos —
    // should produce no "Type mismatch" / "Argument ... expects" warning.
    let core_file = build_dir.join("bt@test_pkg@b.core");
    let options = default_options();
    let diagnostics = compile_file(
        &src_path.join("b.bt"),
        "bt@test_pkg@b",
        &core_file,
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index: class_module_index.clone(),
                class_superclass_index: class_superclass_index.clone(),
                pre_loaded_classes: all_class_infos.clone(),
                pre_loaded_aliases: all_alias_infos.clone(),
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("Cross-file alias-typed argument should compile without errors");

    assert!(core_file.exists());
    assert!(
        diagnostics.iter().all(|d| !d.message.contains("Argument")),
        "Expected no false argument-type-mismatch diagnostic once cross-file \
             aliases are seeded, got: {diagnostics:?}"
    );

    // Negative control: WITHOUT `pre_loaded_aliases`, the same compile
    // reproduces the pre-BT-2928 false positive — proving this test
    // actually exercises the fix rather than a scenario that never warned.
    let core_file_unfixed = build_dir.join("bt@test_pkg@b_unfixed.core");
    let diagnostics_unfixed = compile_file(
        &src_path.join("b.bt"),
        "bt@test_pkg@b_unfixed",
        &core_file_unfixed,
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index,
                class_superclass_index,
                pre_loaded_classes: all_class_infos,
                // No pre_loaded_aliases — reproduces the pre-fix gap.
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("Compile should still succeed (warning, not error)");
    assert!(
        diagnostics_unfixed
            .iter()
            .any(|d| d.message.contains("Argument") && d.message.contains("Direction")),
        "Expected the negative control (no pre_loaded_aliases) to reproduce \
             the false positive, got: {diagnostics_unfixed:?}"
    );
}

/// BT-2928 (review follow-up): `collect_project_alias_infos` scans every
/// source file in the compilation unit, including the one currently being
/// compiled — unlike `ClassHierarchy::cross_file_class_infos`, which
/// explicitly filters out the current file's own classes before
/// injection. So compiling `a.bt` (which declares `type Direction = ...`)
/// with `pre_loaded_aliases` that *also* contains `a.bt`'s own `Direction`
/// entry must not raise a "Duplicate type alias definition" diagnostic —
/// `analyse_full`'s merge order must let the module's own declaration take
/// precedence over its duplicate pre-loaded entry.
#[test]
fn test_cross_file_alias_resolution_no_false_duplicate_for_own_alias() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();

    write_test_file(
        &src_path.join("a.bt"),
        "type Direction = #north | #south | #east | #west\n\
             Value subclass: A\n  heading -> Direction => #north\n",
    );

    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();

    let source_files = vec![src_path.join("a.bt")];
    let (class_module_index, class_superclass_index, all_class_infos, _extensions, _cached_asts) =
        build_class_module_index(&source_files, Some(&src_path), "test_pkg").unwrap();
    // Includes a.bt's own `Direction` alias, exactly as the real Pass 1
    // scan would when compiling a.bt itself.
    let all_alias_infos = collect_project_alias_infos(&source_files, "test_pkg");

    let core_file = build_dir.join("bt@test_pkg@a.core");
    let options = default_options();
    let diagnostics = compile_file(
        &src_path.join("a.bt"),
        "bt@test_pkg@a",
        &core_file,
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index,
                class_superclass_index,
                pre_loaded_classes: all_class_infos,
                pre_loaded_aliases: all_alias_infos,
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("Compiling a file whose own alias is also pre-loaded should succeed");

    assert!(core_file.exists());
    assert!(
        diagnostics
            .iter()
            .all(|d| !d.message.contains("Duplicate type alias")),
        "Expected no false duplicate-alias diagnostic when a file's own \
             alias is also present in pre_loaded_aliases, got: {diagnostics:?}"
    );
}

/// BT-2965: `package_identity` is the single source of truth for "which
/// package is this build compiling", consumed by `build` (for
/// `CompilerOptions::current_package`) and `build_class_index` (for the
/// `AliasInfo::package` stamp). Those two must agree or
/// `AliasRegistry::add_pre_loaded`'s seeding-boundary check silently drops
/// every `internal` alias, so pin the mapping directly.
#[test]
fn package_identity_names_stdlib_for_manifest_less_stdlib_mode() {
    let manifest = manifest::PackageManifest {
        name: "my_pkg".to_string(),
        version: "0.1.0".to_string(),
        description: None,
        licenses: None,
        strict_deps: false,
    };

    // A manifest always wins — `--stdlib-mode` never renames a real package.
    assert_eq!(package_identity(Some(&manifest), false), Some("my_pkg"));
    assert_eq!(package_identity(Some(&manifest), true), Some("my_pkg"));

    // Manifest-less: only `--stdlib-mode` has a package boundary, and it is
    // named the same way `build_stdlib::stdlib_compiler_options` (BT-2964)
    // and the LSP's `STDLIB_PACKAGE_MARKER` name it.
    assert_eq!(
        package_identity(None, true),
        Some(beamtalk_language_service::STDLIB_PACKAGE_MARKER)
    );
    assert_eq!(package_identity(None, false), None);
}

/// BT-2965 regression: `beamtalk build --stdlib-mode <dir>` over a bare
/// directory (no `beamtalk.toml`) must still collect cross-file type
/// aliases. This is exactly what `just dialyzer-specs` does — it copies
/// `stdlib/src/*.bt` flat into a temp dir and builds it — and before the
/// fix `build_class_index` returned an empty `all_alias_infos` for every
/// manifest-less build, so `supervision_spec.bt`'s `field: restart ::
/// RestartStrategy = #temporary` never saw `actor.bt`'s `type
/// RestartStrategy = ...` and drew a false state-default type mismatch.
#[test]
fn stdlib_mode_manifest_less_build_collects_cross_file_aliases() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    // Flat layout with no `beamtalk.toml` and no `src/`, mirroring
    // `dialyzer-specs`' temp dir.
    write_test_file(
        &project_path.join("actor.bt"),
        "type RestartStrategy = #permanent | #transient | #temporary\n\
             typed Value subclass: Policy\n  \
             default -> RestartStrategy => #temporary\n",
    );
    write_test_file(
        &project_path.join("supervision_spec.bt"),
        "typed Value subclass: Spec\n  \
             field: restart :: RestartStrategy = #temporary\n",
    );

    let env = setup_build_environment(project_path.as_str()).unwrap();
    assert!(
        env.pkg_manifest().is_none(),
        "test fixture must stay manifest-less to exercise the fixed path"
    );
    let dep_ctx = DependencyContext {
        resolved_deps: Vec::new(),
        has_native_deps: false,
    };

    let stdlib_options = beamtalk_core::CompilerOptions {
        stdlib_mode: true,
        ..default_options()
    };
    let index = build_class_index(&env, &dep_ctx, &stdlib_options, true).unwrap();
    let restart_alias = index
        .all_alias_infos
        .iter()
        .find(|a| a.name == "RestartStrategy")
        .expect("stdlib-mode build should collect cross-file aliases");
    assert_eq!(
        restart_alias.package.as_deref(),
        Some(beamtalk_language_service::STDLIB_PACKAGE_MARKER),
        "the alias stamp must match the `current_package` `build` sets, or \
             `add_pre_loaded`'s boundary check drops internal stdlib aliases"
    );

    // Control: the same manifest-less directory *without* `--stdlib-mode`
    // keeps the pre-existing same-file-only behaviour — this fix widens
    // the scope for the stdlib build only.
    let plain_index = build_class_index(&env, &dep_ctx, &default_options(), true).unwrap();
    assert!(
        plain_index.all_alias_infos.is_empty(),
        "a plain manifest-less directory build has no package boundary and \
             must keep same-file-only alias resolution"
    );
}

/// BT-2965 regression, symptom level: with cross-file aliases seeded, a
/// `field: x :: SomeAlias = <member>` default whose alias is declared in
/// *another* file draws no "Type mismatch: state ... declared as ...,
/// default is ..." warning. The negative control (no `pre_loaded_aliases`)
/// reproduces the original false positive, proving the assertion is load
/// bearing.
#[test]
fn state_default_with_cross_file_alias_draws_no_type_mismatch() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();

    write_test_file(
        &src_path.join("policy.bt"),
        "type RestartStrategy = #permanent | #transient | #temporary\n\
             typed Value subclass: Policy\n  \
             default -> RestartStrategy => #temporary\n",
    );
    write_test_file(
        &src_path.join("spec.bt"),
        "typed Value subclass: Spec\n  \
             field: restart :: RestartStrategy = #temporary\n",
    );

    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();

    let source_files = vec![src_path.join("policy.bt"), src_path.join("spec.bt")];
    let (class_module_index, class_superclass_index, all_class_infos, _extensions, _cached) =
        build_class_module_index(&source_files, Some(&src_path), "test_pkg").unwrap();
    let all_alias_infos = collect_project_alias_infos(&source_files, "test_pkg");

    let options = default_options();
    let diagnostics = compile_file(
        &src_path.join("spec.bt"),
        "bt@test_pkg@spec",
        &build_dir.join("bt@test_pkg@spec.core"),
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index: class_module_index.clone(),
                class_superclass_index: class_superclass_index.clone(),
                pre_loaded_classes: all_class_infos.clone(),
                pre_loaded_aliases: all_alias_infos,
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("state default typed with a cross-file alias should compile");
    assert!(
        diagnostics
            .iter()
            .all(|d| !d.message.contains("Type mismatch: state")),
        "Expected no false state-default mismatch once cross-file aliases \
             are seeded, got: {diagnostics:?}"
    );

    // Negative control: without `pre_loaded_aliases`, `RestartStrategy`
    // stays an opaque name and the original false positive returns.
    let diagnostics_unfixed = compile_file(
        &src_path.join("spec.bt"),
        "bt@test_pkg@spec_unfixed",
        &build_dir.join("bt@test_pkg@spec_unfixed.core"),
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index,
                class_superclass_index,
                pre_loaded_classes: all_class_infos,
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("Compile should still succeed (warning, not error)");
    assert!(
        diagnostics_unfixed.iter().any(|d| {
            d.message.contains("Type mismatch: state `restart`")
                && d.message.contains("RestartStrategy")
        }),
        "Expected the negative control (no pre_loaded_aliases) to reproduce \
             the false positive, got: {diagnostics_unfixed:?}"
    );
}

/// BT-2910: `collect_project_protocol_infos`/`collect_all_protocol_infos`
/// and `collect_all_alias_infos` are the merge helpers `build_class_index`
/// uses to combine same-package cross-file protocol/alias metadata with
/// a dependency's exported protocol/alias metadata (`ResolvedDependency`).
/// This test exercises that merge directly — mirroring
/// `test_cross_file_alias_resolution_no_false_type_mismatch`'s pattern
/// for aliases, but for a *dependency-sourced* protocol and alias rather
/// than a same-package cross-file one — and confirms the merged sets,
/// once threaded through `pre_loaded_protocols`/`pre_loaded_aliases`,
/// let a consumer file resolve a protocol name and an alias-typed
/// annotation it never declares itself.
#[test]
fn dependency_protocol_and_alias_infos_merge_and_resolve() {
    use beamtalk_core::semantic_analysis::alias_registry::AliasInfo;
    use beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo;

    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();

    // Consumer file: no protocol/alias declarations of its own, only a
    // reference to a dependency-only protocol name and a parameter
    // annotated with a dependency-only alias.
    write_test_file(
        &src_path.join("consumer.bt"),
        "Object subclass: Consumer\n  \
             useStatus: s :: Status => s\n  \
             greetableInfo => Greetable requiredMethods\n",
    );

    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();

    let source_files = vec![src_path.join("consumer.bt")];
    let (class_module_index, class_superclass_index, all_class_infos, _extensions, _cached) =
        build_class_module_index(&source_files, Some(&src_path), "test_pkg").unwrap();

    // Same-package cross-file collections are empty here (single file,
    // no protocol/alias declarations) — this test's focus is the
    // dependency-sourced side of the merge.
    let source_protocol_infos = collect_project_protocol_infos(&source_files);
    let source_alias_infos = collect_project_alias_infos(&source_files, "test_pkg");
    assert!(source_protocol_infos.is_empty());
    assert!(source_alias_infos.is_empty());

    // Simulate a resolved dependency exporting a `Greetable` protocol and
    // a public `Status` alias (`ResolvedDependency.protocol_infos` /
    // `.alias_infos`, as populated by `build_dep_class_index`).
    let dep_protocol_infos = vec![ProtocolInfo {
        name: "Greetable".into(),
        type_params: vec![],
        type_param_bounds: vec![],
        extending: None,
        methods: vec![],
        class_methods: vec![],
        span: beamtalk_core::source_analysis::Span::default(),
    }];
    let dep_alias_infos = vec![AliasInfo {
        name: "Status".into(),
        annotation: beamtalk_core::ast::TypeAnnotation::Simple(beamtalk_core::ast::Identifier {
            name: "Symbol".into(),
            span: beamtalk_core::source_analysis::Span::default(),
        }),
        is_internal: false,
        package: Some("dep_types".into()),
        span: beamtalk_core::source_analysis::Span::default(),
    }];

    let all_protocol_infos =
        collect_all_protocol_infos(&[&source_protocol_infos, &dep_protocol_infos]);
    let all_alias_infos = collect_all_alias_infos(&[&source_alias_infos, &dep_alias_infos]);
    assert_eq!(all_protocol_infos.len(), 1);
    assert_eq!(all_alias_infos.len(), 1);

    let core_file = build_dir.join("bt@test_pkg@consumer.core");
    let options = default_options();
    let diagnostics = compile_file(
        &src_path.join("consumer.bt"),
        "bt@test_pkg@consumer",
        &core_file,
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index,
                class_superclass_index,
                pre_loaded_classes: all_class_infos,
                pre_loaded_protocols: all_protocol_infos,
                pre_loaded_aliases: all_alias_infos,
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("Compiling with dependency-merged protocol/alias infos should succeed");

    assert!(core_file.exists());
    assert!(
        diagnostics.iter().all(|d| d.category
            != Some(beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass)),
        "Dependency-sourced Greetable/Status must not be reported unresolved \
             once merged into pre_loaded_protocols/pre_loaded_aliases, got: {diagnostics:?}"
    );
}

/// BT-2932: cross-module `AliasRegistry` wiring into codegen — the
/// codegen counterpart of `test_cross_file_alias_resolution_no_false_type_mismatch`
/// above. File A declares `type Direction = ...`; file B has no alias
/// declarations of its own, only a method parameter explicitly
/// annotated `:: Direction`. Before this issue, `compile_file`'s codegen
/// call (`write_core_erlang_with_bindings` → `CodegenOptions`) only ever
/// built `AliasRegistry::from_module_declarations(module)` — B's own
/// (empty) `type_aliases` — so this parameter's generated `-spec` fell
/// through to `any()` even though semantic analysis (BT-2928) already
/// resolved the reference correctly. With `pre_loaded_aliases` threaded
/// through (`ClassHierarchyContext::pre_loaded_aliases` →
/// `codegen_hierarchy` → `CodegenOptions::with_pre_loaded_aliases`), B's
/// generated `.core` file must contain a `user_type` reference to the
/// alias declared in A, plus the matching named `-type` declaration (an
/// `erlc` compile error otherwise).
#[test]
fn test_cross_file_alias_reference_emits_user_type_in_generated_core_erlang() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();

    write_test_file(
        &src_path.join("a.bt"),
        "type Direction = #north | #south | #east | #west\n\
             Value subclass: A\n  heading -> Direction => #north\n",
    );
    write_test_file(
        &src_path.join("b.bt"),
        "Object subclass: B\n  useDirection: d :: Direction => d\n",
    );

    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();

    let source_files = vec![src_path.join("a.bt"), src_path.join("b.bt")];
    let (class_module_index, class_superclass_index, all_class_infos, _extensions, _cached_asts) =
        build_class_module_index(&source_files, Some(&src_path), "test_pkg").unwrap();
    let all_alias_infos = collect_project_alias_infos(&source_files, "test_pkg");

    let core_file = build_dir.join("bt@test_pkg@b.core");
    let options = default_options();
    compile_file(
        &src_path.join("b.bt"),
        "bt@test_pkg@b",
        &core_file,
        &options,
        &CompileContext {
            hierarchy: ClassHierarchyContext {
                class_module_index,
                class_superclass_index,
                pre_loaded_classes: all_class_infos,
                pre_loaded_aliases: all_alias_infos,
                ..ClassHierarchyContext::default()
            },
            ..CompileContext::default()
        },
        None,
    )
    .expect("Cross-file alias-typed parameter should compile without errors");

    let core_src = fs::read_to_string(&core_file).unwrap();
    assert!(
        core_src.contains("{'user_type', 0, 'direction', []}"),
        "parameter typed with a cross-file alias should emit a user_type reference in the \
             generated Core Erlang. Got:\n{core_src}"
    );
    assert!(
        core_src.contains("'direction'"),
        "module must declare the matching named -type for the cross-file alias. \
             Got:\n{core_src}"
    );
}
