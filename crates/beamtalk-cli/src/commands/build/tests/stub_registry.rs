// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Stub-registry resolution tests (ADR 0075 Phase 2, BT-1847): project/package/distribution/auto-extract precedence, version-drift detection, and cross-package collision checks for dependency stub registries.

use super::*;

// ── load_project_stub_registry tests (ADR 0075 Phase 2, BT-1847) ────────

#[test]
fn load_project_stub_registry_none_when_no_stubs_dir() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);

    assert!(load_project_stub_registry(&project_path, None, OutputFormat::Text).is_none());
}

#[test]
fn load_project_stub_registry_none_when_stubs_dir_has_no_bt_files() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let stubs_path = project_path.join("stubs");
    fs::create_dir_all(&stubs_path).unwrap();
    write_test_file(&stubs_path.join("README.md"), "not a stub");

    assert!(load_project_stub_registry(&project_path, None, OutputFormat::Text).is_none());
}

#[test]
fn load_project_stub_registry_populates_registry_from_stub_file() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let stubs_path = project_path.join("stubs");
    fs::create_dir_all(&stubs_path).unwrap();
    write_test_file(
        &stubs_path.join("lists.bt"),
        "declare native: lists\n  reverse: list :: List -> List\n",
    );

    let (registry, diags) =
        load_project_stub_registry(&project_path, None, OutputFormat::Text).unwrap();

    assert!(diags.is_empty());
    let sig = registry.lookup("lists", "reverse", 1).unwrap();
    assert_eq!(
        sig.params[0].type_,
        beamtalk_core::semantic_analysis::type_checker::InferredType::known("List")
    );
}

#[test]
fn load_project_stub_registry_combines_multiple_files_for_same_module() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let stubs_path = project_path.join("stubs");
    fs::create_dir_all(&stubs_path).unwrap();
    write_test_file(
        &stubs_path.join("lists_a.bt"),
        "declare native: lists\n  reverse: list -> List\n",
    );
    write_test_file(
        &stubs_path.join("lists_b.bt"),
        "declare native: lists\n  sort: list -> List\n",
    );

    let (registry, _diags) =
        load_project_stub_registry(&project_path, None, OutputFormat::Text).unwrap();

    assert_eq!(registry.module_functions("lists").unwrap().len(), 2);
    assert!(registry.lookup("lists", "reverse", 1).is_some());
    assert!(registry.lookup("lists", "sort", 1).is_some());
}

#[test]
fn load_project_stub_registry_flags_version_drift_against_auto_extract() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let stubs_path = project_path.join("stubs");
    fs::create_dir_all(&stubs_path).unwrap();
    write_test_file(
        &stubs_path.join("lists.bt"),
        "declare native: lists\n  reverse: list -> List\n  bogus: x -> Integer\n",
    );

    let mut auto_extract =
        beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();
    auto_extract.register_module(
        "lists",
        vec![
            beamtalk_core::semantic_analysis::type_checker::FunctionSignature {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![],
                return_type: beamtalk_core::semantic_analysis::type_checker::InferredType::known(
                    "List",
                ),
                provenance:
                    beamtalk_core::semantic_analysis::type_checker::TypeProvenance::Extracted,
                line: None,
            },
        ],
    );

    let (registry, diags) =
        load_project_stub_registry(&project_path, Some(&auto_extract), OutputFormat::Text).unwrap();

    // `bogus/1` isn't a real `lists` export — one drift warning.
    assert_eq!(diags.len(), 1);
    assert!(diags[0].message.contains("bogus"));
    assert!(diags[0].message.contains("out of date"));
    // Both stub functions are still registered — drift is a warning,
    // not a rejection.
    assert!(registry.lookup("lists", "reverse", 1).is_some());
    assert!(registry.lookup("lists", "bogus", 1).is_some());
}

#[test]
fn load_project_stub_registry_no_drift_for_module_unknown_to_auto_extract() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let stubs_path = project_path.join("stubs");
    fs::create_dir_all(&stubs_path).unwrap();
    write_test_file(
        &stubs_path.join("beamtalk_http.bt"),
        "declare native: beamtalk_http\n  get: url -> Dynamic\n",
    );
    let auto_extract = beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();

    let (_registry, diags) =
        load_project_stub_registry(&project_path, Some(&auto_extract), OutputFormat::Text).unwrap();

    assert!(diags.is_empty());
}

// ── distribution_stubs_dir tests (ADR 0075 layer 3) ──────────────────

#[test]
#[serial_test::serial(beamtalk_stubs_env)]
fn distribution_stubs_dir_uses_env_override_when_dir_exists() {
    let temp = TempDir::new().unwrap();
    let stubs_dir = temp.path().join("stubs");
    fs::create_dir_all(&stubs_dir).unwrap();
    // SAFETY: serialised via #[serial]; the var is removed before returning.
    unsafe {
        std::env::set_var("BEAMTALK_STUBS_DIR", &stubs_dir);
    }
    let result = distribution_stubs_dir();
    // SAFETY: serialised via #[serial]; restores the unset state.
    unsafe {
        std::env::remove_var("BEAMTALK_STUBS_DIR");
    }
    assert_eq!(result, Some(Utf8PathBuf::from_path_buf(stubs_dir).unwrap()));
}

#[test]
#[serial_test::serial(beamtalk_stubs_env)]
fn distribution_stubs_dir_env_override_none_when_dir_missing() {
    let temp = TempDir::new().unwrap();
    let missing_dir = temp.path().join("does_not_exist");
    // SAFETY: serialised via #[serial]; the var is removed before returning.
    unsafe {
        std::env::set_var("BEAMTALK_STUBS_DIR", &missing_dir);
    }
    let result = distribution_stubs_dir();
    // SAFETY: serialised via #[serial]; restores the unset state.
    unsafe {
        std::env::remove_var("BEAMTALK_STUBS_DIR");
    }
    assert!(
        result.is_none(),
        "a non-existent BEAMTALK_STUBS_DIR override should yield None"
    );
}

// ── ADR 0075 full resolution chain (BT-3394) ─────────────────────────

/// A function declared at all four layers must resolve according to the
/// ADR 0075 precedence order — project-local stub, then package-bundled
/// stub, then distribution stub, then auto-extracted — mirroring the
/// exact merge sequence `execute_build_passes`/`run_lint` perform.
#[test]
fn stub_resolution_order_project_over_package_over_distribution_over_auto_extract() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);

    // Layer 4 (lowest precedence): auto-extracted.
    let mut auto_extract =
        beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();
    auto_extract.register_module(
        "sample_mod",
        vec![
            beamtalk_core::semantic_analysis::type_checker::FunctionSignature {
                name: "thing".to_string(),
                arity: 1,
                params: vec![],
                return_type: beamtalk_core::semantic_analysis::type_checker::InferredType::known(
                    "ExtractMarker",
                ),
                provenance:
                    beamtalk_core::semantic_analysis::type_checker::TypeProvenance::Extracted,
                line: None,
            },
        ],
    );

    // Layer 3: distribution stubs.
    let dist_dir = Utf8PathBuf::from_path_buf(temp.path().join("dist_stubs")).unwrap();
    fs::create_dir_all(&dist_dir).unwrap();
    write_test_file(
        &dist_dir.join("sample_mod.bt"),
        "declare native: sample_mod\n  thing: x -> DistMarker\n",
    );

    // Layer 2: a dependency's own package-bundled stubs.
    let dep_root = Utf8PathBuf::from_path_buf(temp.path().join("dep_pkg")).unwrap();
    let dep_stubs_dir = dep_root.join("stubs");
    fs::create_dir_all(&dep_stubs_dir).unwrap();
    write_test_file(
        &dep_stubs_dir.join("sample_mod.bt"),
        "declare native: sample_mod\n  thing: x -> PackageMarker\n",
    );
    let resolved_deps = vec![crate::commands::deps::path::ResolvedDependency {
        name: "dep_pkg".to_string(),
        root: dep_root.clone(),
        ebin_path: dep_root.join("ebin"),
        class_module_index: HashMap::new(),
        class_infos: Vec::new(),
        protocol_infos: Vec::new(),
        alias_infos: Vec::new(),
        is_direct: true,
        via_chain: Vec::new(),
        stubs_dir: Some(dep_stubs_dir),
    }];

    // Layer 1 (highest precedence): project-local stubs.
    let project_stubs_dir = project_path.join("stubs");
    fs::create_dir_all(&project_stubs_dir).unwrap();
    write_test_file(
        &project_stubs_dir.join("sample_mod.bt"),
        "declare native: sample_mod\n  thing: x -> ProjectMarker\n",
    );

    let marker =
        |registry: &beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry| -> String {
            registry
                .lookup("sample_mod", "thing", 1)
                .unwrap()
                .return_type
                .display_for_diagnostic()
                .unwrap()
                .to_string()
        };

    // Distribution alone overrides auto-extract.
    let (dist_layer, _) = load_dependency_stub_registries(
        &[],
        Some(dist_dir.as_path()),
        Some(&auto_extract),
        OutputFormat::Text,
    )
    .unwrap();
    let mut merged = auto_extract.clone();
    merged.apply_overrides(dist_layer);
    assert_eq!(marker(&merged), "DistMarker");

    // Package-bundled beats distribution when both are present.
    let (dep_layer, _) = load_dependency_stub_registries(
        &resolved_deps,
        Some(dist_dir.as_path()),
        Some(&auto_extract),
        OutputFormat::Text,
    )
    .unwrap();
    let mut merged = auto_extract.clone();
    merged.apply_overrides(dep_layer);
    assert_eq!(marker(&merged), "PackageMarker");

    // Project-local beats everything, including package-bundled.
    let (project_layer, _) =
        load_project_stub_registry(&project_path, Some(&auto_extract), OutputFormat::Text).unwrap();
    merged.apply_overrides(project_layer);
    assert_eq!(marker(&merged), "ProjectMarker");
}

/// Two sibling dependencies each declaring their own package-bundled
/// stub (ADR 0075 layer 2) for the same `(module, function, arity)` is
/// unexpected — package stubs are meant to cover only that package's own
/// native code — so it must be flagged, naming both packages, rather
/// than resolving silently.
#[test]
fn load_dependency_stub_registries_flags_collision_between_sibling_package_stubs() {
    let temp = TempDir::new().unwrap();

    let alpha_dep_root = Utf8PathBuf::from_path_buf(temp.path().join("dep_a")).unwrap();
    let alpha_dep_stubs = alpha_dep_root.join("stubs");
    fs::create_dir_all(&alpha_dep_stubs).unwrap();
    write_test_file(
        &alpha_dep_stubs.join("shared_mod.bt"),
        "declare native: shared_mod\n  thing: x -> Integer\n",
    );

    let beta_dep_root = Utf8PathBuf::from_path_buf(temp.path().join("dep_b")).unwrap();
    let beta_dep_stubs = beta_dep_root.join("stubs");
    fs::create_dir_all(&beta_dep_stubs).unwrap();
    write_test_file(
        &beta_dep_stubs.join("shared_mod.bt"),
        "declare native: shared_mod\n  thing: x -> String\n",
    );

    let resolved_deps = vec![
        crate::commands::deps::path::ResolvedDependency {
            name: "dep_a".to_string(),
            root: alpha_dep_root.clone(),
            ebin_path: alpha_dep_root.join("ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: Some(alpha_dep_stubs),
        },
        crate::commands::deps::path::ResolvedDependency {
            name: "dep_b".to_string(),
            root: beta_dep_root.clone(),
            ebin_path: beta_dep_root.join("ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: Some(beta_dep_stubs),
        },
    ];

    let (_registry, diags) =
        load_dependency_stub_registries(&resolved_deps, None, None, OutputFormat::Text).unwrap();

    assert_eq!(
        diags.len(),
        1,
        "exactly one collision diagnostic expected, got: {diags:?}"
    );
    assert!(diags[0].message.contains("dep_a"), "{}", diags[0].message);
    assert!(diags[0].message.contains("dep_b"), "{}", diags[0].message);
    assert!(
        diags[0].message.contains("shared_mod:thing/1"),
        "{}",
        diags[0].message
    );
}

/// Two sibling dependencies declaring *different* functions (even in the
/// same native module) is not a collision — no diagnostic.
#[test]
fn load_dependency_stub_registries_no_collision_for_disjoint_functions() {
    let temp = TempDir::new().unwrap();

    let alpha_dep_root = Utf8PathBuf::from_path_buf(temp.path().join("dep_a")).unwrap();
    let alpha_dep_stubs = alpha_dep_root.join("stubs");
    fs::create_dir_all(&alpha_dep_stubs).unwrap();
    write_test_file(
        &alpha_dep_stubs.join("shared_mod.bt"),
        "declare native: shared_mod\n  a_thing: x -> Integer\n",
    );

    let beta_dep_root = Utf8PathBuf::from_path_buf(temp.path().join("dep_b")).unwrap();
    let beta_dep_stubs = beta_dep_root.join("stubs");
    fs::create_dir_all(&beta_dep_stubs).unwrap();
    write_test_file(
        &beta_dep_stubs.join("shared_mod.bt"),
        "declare native: shared_mod\n  b_thing: x -> String\n",
    );

    let resolved_deps = vec![
        crate::commands::deps::path::ResolvedDependency {
            name: "dep_a".to_string(),
            root: alpha_dep_root.clone(),
            ebin_path: alpha_dep_root.join("ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: Some(alpha_dep_stubs),
        },
        crate::commands::deps::path::ResolvedDependency {
            name: "dep_b".to_string(),
            root: beta_dep_root.clone(),
            ebin_path: beta_dep_root.join("ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: Some(beta_dep_stubs),
        },
    ];

    let (registry, diags) =
        load_dependency_stub_registries(&resolved_deps, None, None, OutputFormat::Text).unwrap();

    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    assert!(registry.lookup("shared_mod", "a_thing", 1).is_some());
    assert!(registry.lookup("shared_mod", "b_thing", 1).is_some());
}
