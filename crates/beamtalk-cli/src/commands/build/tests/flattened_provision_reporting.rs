// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0127 §3 / BT-3663: a diagnostic in a flattened trait provision is
//! collected across the users of the protocol and reported once, against the
//! protocol's file.

use super::*;
use beamtalk_core::semantic_analysis::{ProtocolSource, ProtocolSourceMap};
use beamtalk_core::source_analysis::merge_provision_diagnostics;

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
