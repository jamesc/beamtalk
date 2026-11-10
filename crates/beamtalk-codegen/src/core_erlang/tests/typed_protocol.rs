// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4486: sends to a receiver typed by a *protocol* compile to a
//! multi-arm class-tag guarded direct call over the protocol's known
//! conformers, with the dynamic send as the fallback for any other value.
//!
//! Protocols are structural, so the conformer list is only a heuristic; the
//! guard is the contract. These tests run the full-analysis path, the only
//! one that sees the protocol registry.

use super::{assert_compiles_through_erlc, parse_normalised};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};

fn codegen_with_full_analysis(src: &str) -> String {
    let (module, diagnostics) = parse_normalised(src);
    assert!(
        diagnostics
            .iter()
            .all(|d| d.severity != beamtalk_core::source_analysis::Severity::Error),
        "fixture failed to parse: {diagnostics:?}"
    );
    let analysis = analyse_full(&module, AnalysisContext::default());
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_analysis(analysis),
    )
    .expect("codegen should succeed")
}

/// The caller (first class: `generate_module` compiles only that one) sends
/// `protoLabel` to a `Namer`-typed parameter.
const CALLER: &str = concat!(
    "Object subclass: Caller\n",
    "  useIt: n :: Namer -> String =>\n",
    "    n protoLabel\n\n",
);
const PROTOCOL: &str = "Protocol define: Namer\n  protoLabel -> String\n\n";

fn source(conformers: &[&str]) -> String {
    let mut src = format!("{CALLER}{PROTOCOL}");
    for class in conformers {
        src.push_str(class);
        src.push_str("\n\n");
    }
    src
}

fn method_conformer(name: &str) -> String {
    format!("sealed Value subclass: {name}\n  protoLabel -> String => \"{name}\"\n")
}

/// The `Namer` send site's `case`: from the first `is_map` guard to the end.
fn send_site(code: &str) -> &str {
    let start = code
        .find("call 'erlang':'is_map'(")
        .unwrap_or_else(|| panic!("no tag guard emitted. Got:\n{code}"));
    &code[start..]
}

fn assert_still_dynamic(code: &str) {
    assert!(
        code.contains("call 'beamtalk_message_dispatch':'send'(")
            && !code.contains("':'dispatch'('protoLabel'"),
        "send should stay dynamic. Got:\n{code}"
    );
}

#[test]
fn single_conformer_is_a_single_tag_guard() {
    let code = codegen_with_full_analysis(&source(&[&method_conformer("Alpha")]));
    assert!(
        code.contains("call 'bt@alpha':'dispatch'('protoLabel'"),
        "{code}"
    );
    assert!(send_site(&code).contains("<'Alpha'> when 'true'"), "{code}");
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "{code}"
    );
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn several_conformers_get_sorted_arms_and_a_dynamic_fallback() {
    // Declared out of order: arms must come out sorted by class name.
    let code = codegen_with_full_analysis(&source(&[
        &method_conformer("Charlie"),
        &method_conformer("Alpha"),
        &method_conformer("Bravo"),
    ]));
    let site = send_site(&code);
    let positions: Vec<usize> = ["<'Alpha'>", "<'Bravo'>", "<'Charlie'>"]
        .iter()
        .map(|arm| {
            site.find(arm)
                .unwrap_or_else(|| panic!("missing arm {arm}. Got:\n{code}"))
        })
        .collect();
    assert!(
        positions.windows(2).all(|w| w[0] < w[1]),
        "unsorted: {code}"
    );
    for module in ["bt@alpha", "bt@bravo", "bt@charlie"] {
        assert!(
            code.contains(&format!("call '{module}':'dispatch'('protoLabel'")),
            "no direct call to {module}. Got:\n{code}"
        );
    }
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "{code}"
    );
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn field_getter_conformer_counts_as_implementing_the_selector() {
    // `Alpha>>protoLabel` is a `field:` getter, not a method; it must still make
    // `Alpha` conform to `Namer` (the Buccaneer `HTTP1Socket>>version` shape).
    let code = codegen_with_full_analysis(&source(&[
        "sealed Value subclass: Alpha\n  field: protoLabel :: String = \"a\"\n",
        &method_conformer("Bravo"),
    ]));
    let site = send_site(&code);
    assert!(
        site.contains("<'Alpha'>") && site.contains("<'Bravo'>"),
        "both conformers should be arms. Got:\n{code}"
    );
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn abstract_actor_and_non_conforming_classes_are_skipped() {
    let code = codegen_with_full_analysis(&source(&[
        &method_conformer("Alpha"),
        &method_conformer("Bravo"),
        "abstract Value subclass: Abstracto\n  protoLabel -> String => \"x\"\n",
        "Actor subclass: Worker\n  protoLabel -> String => \"w\"\n",
        "sealed Value subclass: Other\n  size -> Integer => 1\n",
    ]));
    let site = send_site(&code);
    assert!(
        site.contains("<'Alpha'>") && site.contains("<'Bravo'>"),
        "{code}"
    );
    for skipped in ["<'Abstracto'>", "<'Worker'>", "<'Other'>"] {
        assert!(
            !site.contains(skipped),
            "{skipped} must not be an arm. Got:\n{code}"
        );
    }
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn zero_conformers_stays_dynamic() {
    let code = codegen_with_full_analysis(&source(&[
        "sealed Value subclass: Other\n  size -> Integer => 1\n",
    ]));
    assert_still_dynamic(&code);
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn too_many_conformers_stays_dynamic() {
    let names = ["A1", "A2", "A3", "A4", "A5"];
    let conformers: Vec<String> = names.iter().map(|n| method_conformer(n)).collect();
    let refs: Vec<&str> = conformers.iter().map(String::as_str).collect();
    let code = codegen_with_full_analysis(&source(&refs));
    assert_still_dynamic(&code);
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn exactly_max_arms_is_still_guarded() {
    let names = ["A1", "A2", "A3", "A4"];
    let conformers: Vec<String> = names.iter().map(|n| method_conformer(n)).collect();
    let refs: Vec<&str> = conformers.iter().map(String::as_str).collect();
    let code = codegen_with_full_analysis(&source(&refs));
    assert!(send_site(&code).contains("<'A4'> when 'true'"), "{code}");
    assert_compiles_through_erlc("test", &code);
}

/// BT-4630: a protocol with one tagged conformer resolves to a single
/// `Tagged` target, but a real `Block` can conform structurally and is never
/// an arm, so `t value` keeps the block intrinsic's inline `apply`. The
/// protocol also requires a unique selector so that `Const` is its only
/// conformer (`value` alone is implemented by too many stdlib classes).
#[test]
fn protocol_typed_value_send_keeps_block_intrinsic() {
    let src = concat!(
        "Object subclass: Caller\n",
        "  run: t :: Thunk -> Integer =>\n",
        "    t value\n\n",
        "Protocol define: Thunk\n  value -> Integer\n  thunkProbe -> Integer\n\n",
        "sealed Value subclass: Const\n  value -> Integer => 1\n  thunkProbe -> Integer => 2\n\n",
    );
    let code = codegen_with_full_analysis(src);
    assert!(code.contains("call 'erlang':'is_function'("), "{code}");
    assert!(code.contains("apply "), "{code}");
    assert_compiles_through_erlc("test", &code);
}
