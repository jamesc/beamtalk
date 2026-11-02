// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4356: a block-evaluation send's line annotation sits on the `apply`
//! (or `call`) node that transfers control, not only on the `let`/`case`
//! wrapper the send lowers to.
//!
//! The OTP compiler takes a call's stack-frame location from the annotation
//! on the call node itself; one on an enclosing `let`/`case` never reaches
//! it. A `block value` on a non-literal receiver lowers to `let _ValRecv =
//! Recv in case is_function(_ValRecv, 0) of 'true' -> apply _ValRecv () ...`,
//! so before this fix the frame of the method evaluating the block (e.g.
//! `Integer>>timesRepeat:`) carried no `file:line` at all.
//! `stdlib/test/block_call_site_line_test.bt` checks the runtime frames end
//! to end; these tests pin the emitted Core Erlang.

use super::*;

const PATH: &str = "runner.bt";

/// Compiles `src` with its source text and path attached, as the CLI build
/// does, so sends carry `[Line, {'file', Path}]` annotations.
fn codegen_with_lines(src: &str) -> String {
    let (module, diagnostics) = parse_normalised(src);
    assert!(
        diagnostics
            .iter()
            .all(|d| d.severity != beamtalk_core::source_analysis::Severity::Error),
        "fixture failed to parse: {diagnostics:?}"
    );
    generate_module(
        &module,
        CodegenOptions::new("runner")
            .with_source(src)
            .with_source_path_opt(Some(PATH)),
    )
    .expect("codegen should succeed")
}

/// The annotation suffix a node on `line` of [`PATH`] ends with.
fn line_anno(line: u32) -> String {
    format!(" -| [{line}, {{'file', \"{PATH}\"}}] )")
}

/// Every occurrence of `node` (an `apply`/`call` prefix) in `code` must be
/// the first thing inside an annotation wrapper `( node ... -| [line, ...] )`.
fn assert_every_node_annotated(code: &str, node: &str, line: u32) {
    let occurrences: Vec<usize> = code.match_indices(node).map(|(at, _)| at).collect();
    assert!(!occurrences.is_empty(), "expected `{node}` in:\n{code}");
    let anno = line_anno(line);
    for at in occurrences {
        assert!(
            code[..at].ends_with("( "),
            "`{node}` at byte {at} is not wrapped in its own line annotation:\n{code}"
        );
        let rest = &code[at..];
        let close = rest
            .find(" -| [")
            .unwrap_or_else(|| panic!("no annotation after `{node}`:\n{code}"));
        assert!(
            rest[close..].starts_with(&anno),
            "`{node}` at byte {at} should be annotated with line {line}, got `{}`:\n{code}",
            &rest[close..close + anno.len().min(rest.len() - close)]
        );
    }
}

#[test]
fn a_unary_value_send_annotates_its_apply_node() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk =>\n",
        "    blk value\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "apply _ValRecv", 3);
    assert_every_node_annotated(&code, "call 'beamtalk_message_dispatch':'send'(_ValRecv", 3);
}

#[test]
fn a_keyword_value_send_annotates_its_apply_node() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk with: x =>\n",
        "    blk value: x\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "apply _ValRecv", 3);
    assert_every_node_annotated(&code, "call 'beamtalk_message_dispatch':'send'(_ValRecv", 3);
}

#[test]
fn a_value_with_arguments_send_annotates_its_apply_node() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk with: args =>\n",
        "    blk valueWithArguments: args\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "call 'erlang':'apply'(_ValRecv", 3);
    assert_every_node_annotated(&code, "call 'beamtalk_message_dispatch':'send'(_ValRecv", 3);
}

/// The send's line, not the method's: a send on a later line of the body
/// annotates its apply with that line.
#[test]
fn the_apply_carries_the_send_line_not_the_method_line() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk =>\n",
        "    x := 1\n",
        "    y := 2\n",
        "    blk value\n",
        "    x + y\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "apply _ValRecv", 5);
}

/// The annotated output is still well-formed Core Erlang.
#[test]
fn annotated_block_sends_compile_through_erlc() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk =>\n",
        "    blk value\n",
        "    nil\n",
        "  run: blk with: x =>\n",
        "    blk value: x\n",
        "    nil\n",
        "  run: blk withAll: args =>\n",
        "    blk valueWithArguments: args\n",
        "    nil\n",
    );
    assert_compiles_through_erlc("runner", &codegen_with_lines(src));
}
