// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2800: LSP application of beamtalk.toml's [diagnostics] table, including the absent-manifest no-op and multi-root collision (later root wins) cases.

use super::*;

// ---- BT-2800: LSP applies beamtalk.toml [diagnostics] table ----

#[tokio::test]
async fn load_diagnostics_table_promotes_dnu_hint_to_error() {
    // ADR 0100 Rule 3 surface-parity regression: a package that sets
    // `dnu = "error"` in beamtalk.toml must see the LSP report the same
    // Error severity `beamtalk build` would, not the Rule 1 default Hint.
    use beamtalk_core::source_analysis::DiagnosticCategory;

    let temp = unique_temp_dir("beamtalk_lsp_diagnostics_table");
    fs::create_dir_all(&temp).expect("create project root");
    fs::write(
        temp.join("beamtalk.toml"),
        "[package]\nname = \"t\"\nversion = \"0.1.0\"\n\n[diagnostics]\ndnu = \"error\"\n",
    )
    .expect("write beamtalk.toml");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .load_diagnostics_table(std::slice::from_ref(&temp))
        .await;

    let source_path = Utf8PathBuf::from_path_buf(temp.join("dnu.bt")).unwrap();
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(source_path.clone(), "\"hello\" frobnicate".to_string());
    }

    let diags = {
        let svc = backend.service.lock().expect("service lock poisoned");
        svc.diagnostics(&source_path)
    };
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Dnu) && d.severity == Severity::Error),
        "dnu = \"error\" in beamtalk.toml must promote the LSP's Dnu hint to \
             Error, matching `beamtalk build`: {diags:?}"
    );

    let _ = fs::remove_dir_all(&temp);
}

#[tokio::test]
async fn load_diagnostics_table_absent_manifest_is_noop() {
    // No beamtalk.toml at all (or no [diagnostics] section) must leave
    // Rule 1 defaults untouched — a Dnu hint stays a Hint.
    use beamtalk_core::source_analysis::DiagnosticCategory;

    let temp = unique_temp_dir("beamtalk_lsp_diagnostics_table_absent");
    fs::create_dir_all(&temp).expect("create project root");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .load_diagnostics_table(std::slice::from_ref(&temp))
        .await;

    let source_path = Utf8PathBuf::from_path_buf(temp.join("dnu.bt")).unwrap();
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(source_path.clone(), "\"hello\" frobnicate".to_string());
    }

    let diags = {
        let svc = backend.service.lock().expect("service lock poisoned");
        svc.diagnostics(&source_path)
    };
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Dnu) && d.severity == Severity::Hint),
        "absent beamtalk.toml must preserve the Rule 1 default Hint severity: {diags:?}"
    );

    let _ = fs::remove_dir_all(&temp);
}

#[tokio::test]
async fn load_diagnostics_table_multi_root_collision_later_root_wins() {
    // BT-2800 review follow-up: when two workspace roots set the same
    // [diagnostics] category to different severities, the collision is
    // now logged (see load_diagnostics_table's per-key merge loop), but
    // the resulting behavior is unchanged — the later root's value wins
    // for the whole session, matching set_has_package_dependencies.
    use beamtalk_core::source_analysis::DiagnosticCategory;

    let temp = unique_temp_dir("beamtalk_lsp_diagnostics_table_collision");
    let root_a = temp.join("a");
    let root_b = temp.join("b");
    fs::create_dir_all(&root_a).expect("create root a");
    fs::create_dir_all(&root_b).expect("create root b");
    fs::write(
        root_a.join("beamtalk.toml"),
        "[package]\nname = \"a\"\nversion = \"0.1.0\"\n\n[diagnostics]\ndnu = \"hint\"\n",
    )
    .expect("write root a manifest");
    fs::write(
        root_b.join("beamtalk.toml"),
        "[package]\nname = \"b\"\nversion = \"0.1.0\"\n\n[diagnostics]\ndnu = \"error\"\n",
    )
    .expect("write root b manifest");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .load_diagnostics_table(&[root_a.clone(), root_b.clone()])
        .await;

    let source_path = Utf8PathBuf::from_path_buf(root_b.join("dnu.bt")).unwrap();
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(source_path.clone(), "\"hello\" frobnicate".to_string());
    }

    let diags = {
        let svc = backend.service.lock().expect("service lock poisoned");
        svc.diagnostics(&source_path)
    };
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Dnu) && d.severity == Severity::Error),
        "the later root (b, dnu = error) must win the whole-session merge: {diags:?}"
    );

    let _ = fs::remove_dir_all(&temp);
}
