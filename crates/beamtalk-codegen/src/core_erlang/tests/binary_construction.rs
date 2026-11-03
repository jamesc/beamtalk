// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0143 Phase 1 (BT-4346): binary construction parses, but lowering is
//! Phase 2 (BT-4347). Until then compiling one is a reported
//! `UnsupportedFeature` error at the construction's span, never a panic or
//! silently wrong Core Erlang.

use crate::core_erlang::{CodeGenError, CodegenOptions, generate_module_with_warnings};
use beamtalk_core::source_analysis::{Span, lex_with_eof, parse};

fn compile(src: &str) -> crate::core_erlang::Result<String> {
    let (module, diagnostics) = parse(lex_with_eof(src));
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    generate_module_with_warnings(&module, CodegenOptions::new("test")).map(|r| r.code)
}

fn assert_not_yet_implemented(src: &str, construction: &str) {
    let err = compile(src).expect_err("binary construction must not compile yet");
    let CodeGenError::UnsupportedFeature { feature, span } = &err else {
        panic!("expected UnsupportedFeature, got {err:?}");
    };
    assert!(
        feature.contains("binary construction") && feature.contains("BT-4347"),
        "{feature}"
    );
    let start = src.find(construction).expect("construction in source");
    assert_eq!(
        *span,
        Some(Span::new(
            u32::try_from(start).unwrap(),
            u32::try_from(start + construction.len()).unwrap()
        ))
    );
}

#[test]
fn construction_in_a_method_body_is_unsupported() {
    assert_not_yet_implemented(
        "Object subclass: Framer\n  frame: p => <<16:8, p/binary>>\n",
        "<<16:8, p/binary>>",
    );
}

#[test]
fn construction_as_a_send_argument_is_unsupported() {
    assert_not_yet_implemented(
        "Object subclass: Framer\n  frame: p => p ++ <<(p size):8>>\n",
        "<<(p size):8>>",
    );
}

#[test]
fn construction_in_a_block_is_unsupported() {
    assert_not_yet_implemented(
        "Object subclass: Framer\n  frames: ps => ps collect: [:p | <<p:8>>]\n",
        "<<p:8>>",
    );
}
