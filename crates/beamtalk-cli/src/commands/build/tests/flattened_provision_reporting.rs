// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0127 §3 / BT-3663: a diagnostic in a flattened trait provision is
//! collected across the users of the protocol and reported once, against the
//! protocol's file.

use super::*;
use beamtalk_core::semantic_analysis::{ProtocolSource, ProtocolSourceMap};
use beamtalk_core::source_analysis::{Diagnostic, merge_provision_diagnostics};

const BROKEN: &str = "Protocol define: Broken\n  name -> String\n\n  probe -> Integer => 3 bogus\n";

#[test]
fn shared_provision_diagnostic_is_collected_once_across_users() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();

    let protocol_path = src_path.join("broken.bt");
    write_test_file(&protocol_path, BROKEN);
    for user in ["Alpha", "Beta"] {
        write_test_file(
            &src_path.join(format!("{}.bt", user.to_lowercase())),
            &format!("Object subclass: {user}\n  uses: Broken\n  name -> String => \"{user}\"\n"),
        );
    }

    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(BROKEN));
    let sources: ProtocolSourceMap = [(
        "Broken".into(),
        ProtocolSource {
            path: Some(protocol_path.as_str().into()),
            text: BROKEN.into(),
            package: Some("test_pkg".into()),
        },
    )]
    .into_iter()
    .collect();
    let ctx = CompileContext {
        hierarchy: ClassHierarchyContext {
            pre_loaded_protocol_defs: module.protocols,
            pre_loaded_protocol_sources: sources,
            ..ClassHierarchyContext::default()
        },
        provision_sink: crate::beam_compiler::ProvisionSink::collecting(),
        ..CompileContext::default()
    };

    let options = default_options();
    for user in ["alpha", "beta"] {
        let diagnostics = compile_file(
            &src_path.join(format!("{user}.bt")),
            &format!("bt@test_pkg@{user}"),
            &build_dir.join(format!("bt@test_pkg@{user}.core")),
            &options,
            &ctx,
            None,
        )
        .expect("a provision type error is a hint/warning, not a build failure");
        assert!(
            diagnostics.iter().any(|d| d.provision.is_some()),
            "each user's own compile still sees the provision diagnostic: {diagnostics:?}"
        );
    }

    let collected = merge_provision_diagnostics(ctx.provision_sink.drain());
    assert_eq!(collected.len(), 1, "{collected:?}");
    let origin = collected[0].provision.as_ref().unwrap();
    assert_eq!(origin.protocol.as_str(), "Broken");
    assert_eq!(
        origin.users,
        vec![
            ecow::EcoString::from("Alpha"),
            ecow::EcoString::from("Beta")
        ]
    );
}

/// Compiles two users of a shared broken provision with a collecting sink and
/// returns every file's diagnostics (as `all_build_diags` would hold them).
fn compile_two_users(temp: &TempDir) -> (CompileContext<'static>, Vec<Diagnostic>) {
    let project_path = create_test_project(temp);
    let src_path = project_path.join("src");
    let build_dir = project_path.join("_build/dev/ebin");
    fs::create_dir_all(&build_dir).unwrap();
    write_test_file(&src_path.join("broken.bt"), BROKEN);
    for user in ["Alpha", "Beta"] {
        write_test_file(
            &src_path.join(format!("{}.bt", user.to_lowercase())),
            &format!("Object subclass: {user}\n  uses: Broken\n  name -> String => \"{user}\"\n"),
        );
    }
    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(BROKEN));
    let sources: ProtocolSourceMap = [(
        "Broken".into(),
        ProtocolSource {
            path: Some(src_path.join("broken.bt").as_str().into()),
            text: BROKEN.into(),
            package: Some("test_pkg".into()),
        },
    )]
    .into_iter()
    .collect();
    let ctx = CompileContext {
        hierarchy: ClassHierarchyContext {
            pre_loaded_protocol_defs: module.protocols,
            pre_loaded_protocol_sources: sources,
            ..ClassHierarchyContext::default()
        },
        provision_sink: crate::beam_compiler::ProvisionSink::collecting(),
        ..CompileContext::default()
    };
    let options = default_options();
    let mut all = Vec::new();
    for user in ["alpha", "beta"] {
        all.extend(
            compile_file(
                &src_path.join(format!("{user}.bt")),
                &format!("bt@test_pkg@{user}"),
                &build_dir.join(format!("bt@test_pkg@{user}.core")),
                &options,
                &ctx,
                None,
            )
            .unwrap(),
        );
    }
    (ctx, all)
}

#[test]
fn build_summary_counts_shared_provision_diagnostic_once() {
    let temp = TempDir::new().unwrap();
    let (_ctx, all) = compile_two_users(&temp);
    let per_user = all.iter().filter(|d| d.provision.is_some()).count();
    assert!(per_user >= 2, "one copy per user expected: {all:?}");

    let deduped = dedupe_provision_diagnostics(all.clone());
    let merged_count = deduped.iter().filter(|d| d.provision.is_some()).count();
    assert_eq!(merged_count, per_user / 2, "{deduped:?}");
    assert_eq!(
        deduped.len(),
        all.len() - (per_user - merged_count),
        "untagged diagnostics are untouched"
    );
}

#[test]
fn flush_provision_on_error_flushes_on_failure_only() {
    let temp = TempDir::new().unwrap();
    let (ctx, all) = compile_two_users(&temp);
    let options = default_options();

    // Success passes through and leaves the sink for the end-of-build flush.
    ctx.provision_sink
        .report(&all, &ctx.hierarchy.pre_loaded_protocol_sources, &options);
    let ok: Result<u32> = flush_provision_on_error(Ok(7), &ctx, &options);
    assert_eq!(ok.unwrap(), 7);
    let held = ctx.provision_sink.drain();
    assert!(!held.is_empty(), "success must not flush the sink");

    // Failure flushes (drains) the sink and still propagates the error.
    ctx.provision_sink
        .report(&all, &ctx.hierarchy.pre_loaded_protocol_sources, &options);
    let err: Result<u32> = flush_provision_on_error(
        Err(miette::miette!("unrelated compile error")),
        &ctx,
        &options,
    );
    assert!(err.is_err());
    assert!(
        ctx.provision_sink.drain().is_empty(),
        "failure must flush collected provision diagnostics"
    );
}
