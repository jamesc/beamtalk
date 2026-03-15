// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive BIF mappings for the Metaclass stdlib class (ADR 0036 Phase 2).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "metaclassXxx"` declarations in `stdlib/src/Metaclass.bt`
//! to direct calls into `beamtalk_behaviour_intrinsics`. These are thin
//! data-access functions following the pattern established in ADR 0032.

use super::super::document::Document;
use crate::docvec;

/// Zero-arg Metaclass intrinsics that take Self.
const METACLASS_ZERO_ARG: &[&str] = &[
    "metaclassThisClass",
    "metaclassSuperclass",
    "metaclassAllMethods",
    "metaclassClassMethods",
    "metaclassLocalClassMethods",
];

/// Generates Core Erlang for a Metaclass primitive.
///
/// These back the `@primitive "metaclassXxx"` method bodies in `Metaclass.bt`.
/// Each maps to a direct call to `beamtalk_behaviour_intrinsics:Func(Self)`.
pub fn generate_metaclass_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    // Check zero-arg intrinsics table first
    if METACLASS_ZERO_ARG.contains(&selector) {
        return Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'",
            selector.to_owned(),
            "'(Self)"
        ]);
    }

    match selector {
        "metaclassIncludesSelector" => {
            let sel = params.first()?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'metaclassIncludesSelector'(Self, ",
                sel.clone(),
                ")"
            ])
        }
        // Used by `class sealed new => @primitive "metaclassNew"`.
        // Generates a no-arg call; the delegating new/0 wrapper supplies no Self.
        "metaclassNew" => Some(Document::Str(
            "call 'beamtalk_behaviour_intrinsics':'metaclassNew'()",
        )),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    #[test]
    fn test_metaclass_this_class() {
        let result = doc_to_string(generate_metaclass_bif("metaclassThisClass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassThisClass'(Self)".to_string())
        );
    }

    #[test]
    fn test_metaclass_superclass() {
        let result = doc_to_string(generate_metaclass_bif("metaclassSuperclass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassSuperclass'(Self)".to_string())
        );
    }

    #[test]
    fn test_metaclass_all_methods() {
        let result = doc_to_string(generate_metaclass_bif("metaclassAllMethods", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassAllMethods'(Self)".to_string())
        );
    }

    #[test]
    fn test_metaclass_class_methods() {
        let result = doc_to_string(generate_metaclass_bif("metaclassClassMethods", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassClassMethods'(Self)".to_string())
        );
    }

    #[test]
    fn test_metaclass_local_class_methods() {
        let result = doc_to_string(generate_metaclass_bif("metaclassLocalClassMethods", &[]));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'metaclassLocalClassMethods'(Self)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_metaclass_includes_selector() {
        let result = doc_to_string(generate_metaclass_bif(
            "metaclassIncludesSelector",
            &["Selector".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'metaclassIncludesSelector'(Self, Selector)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_unknown_selector() {
        let result = doc_to_string(generate_metaclass_bif("unknownMethod", &[]));
        assert!(result.is_none());
    }
}
