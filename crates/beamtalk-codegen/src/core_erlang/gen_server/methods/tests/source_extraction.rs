// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `extract_method_source` must not leak an inference-written
//! `-> Type` return-type annotation into the image-resident `__source__`
//! text, while still round-tripping a genuine user-written annotation
//! untouched. See `clear_return_type_writeback_for_key`'s doc for the
//! full root-cause story (`ChangeLog`'s canonical `source_ref` is unparsed
//! pre-writeback; without this, browsed source was unparsed
//! post-writeback, so a save -> revert -> re-save of an unchanged buffer
//! recorded a spurious annotation-only `ChangeLog` diff).

use super::*;
use beamtalk_core::ast::TypeAnnotation;
use beamtalk_core::semantic_analysis::{InferredType, MethodReturnKey, TypeProvenance};
use ecow::EcoString;

#[test]
fn extract_method_source_strips_inferred_return_type_annotation() {
    let mut method = simple_unary_method("greeting");
    method.return_type = Some(TypeAnnotation::simple(EcoString::from("Hello"), s()));

    let mut generator = CoreErlangGenerator::new("test");
    let key: MethodReturnKey = (EcoString::from("Hello"), EcoString::from("greeting"), false);
    generator.method_return_types_written_back.insert(
        key,
        InferredType::Known {
            class_name: EcoString::from("Hello"),
            type_args: vec![],
            provenance: TypeProvenance::Inferred(s()),
        },
    );

    let source = generator.extract_method_source("Hello", false, &method);
    assert!(
        !source.contains("->"),
        "inferred return-type annotation leaked into extracted source: {source:?}"
    );
    // `method`'s own AST is left untouched — codegen elsewhere (specs,
    // `method_return_types` metadata) still needs the inferred type.
    assert!(
        method.return_type.is_some(),
        "extract_method_source must not mutate the method it was given"
    );
}

#[test]
fn extract_method_source_preserves_explicit_return_type_annotation() {
    let mut method = simple_unary_method("greeting");
    method.return_type = Some(TypeAnnotation::simple(EcoString::from("Hello"), s()));

    // No entry in `method_return_types_written_back`: this key was never
    // written by inference, so the annotation is the user's own.
    let generator = CoreErlangGenerator::new("test");
    let source = generator.extract_method_source("Hello", false, &method);
    assert!(
        source.contains("-> Hello"),
        "explicit user-written return-type annotation was stripped: {source:?}"
    );
}
