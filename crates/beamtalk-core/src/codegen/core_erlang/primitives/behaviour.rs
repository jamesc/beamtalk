// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive BIF mappings for Behaviour and Class stdlib classes (ADR 0032 Phase 2).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "classXxx"` declarations in `lib/Behaviour.bt` and `lib/Class.bt`
//! to direct calls into `beamtalk_behaviour_intrinsics`. These are thin data-access
//! functions — hierarchy-walking logic lives in pure Beamtalk.

use super::super::document::Document;
use crate::docvec;

/// Generates Core Erlang for a Behaviour primitive.
///
/// These back the `@primitive "classXxx"` method bodies in `lib/Behaviour.bt`.
/// Each maps to a direct call to `beamtalk_behaviour_intrinsics:Func(Self)`.
pub fn generate_behaviour_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "classSuperclass" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classSuperclass'(Self)"
        ]),
        "classSubclasses" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classSubclasses'(Self)"
        ]),
        "classAllSubclasses" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classAllSubclasses'(Self)"
        ]),
        "classLocalMethods" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classLocalMethods'(Self)"
        ]),
        "classIncludesSelector" => {
            let sel = params.first()?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classIncludesSelector'(Self, ",
                sel.clone(),
                ")",
            ])
        }
        "classInstVarNames" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classInstVarNames'(Self)"
        ]),
        "classAllSuperclasses" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classAllSuperclasses'(Self)"
        ]),
        "classMethods" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classMethods'(Self)"
        ]),
        "classCanUnderstand" => {
            let sel = params.first()?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classCanUnderstand'(Self, ",
                sel.clone(),
                ")",
            ])
        }
        "classInheritsFrom" => {
            let cls = params.first()?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classInheritsFrom'(Self, ",
                cls.clone(),
                ")",
            ])
        }
        "classIncludesBehaviour" => {
            let beh = params.first()?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classIncludesBehaviour'(Self, ",
                beh.clone(),
                ")",
            ])
        }
        "classWhichIncludesSelector" => {
            let sel = params.first()?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classWhichIncludesSelector'(Self, ",
                sel.clone(),
                ")",
            ])
        }
        "classAllInstVarNames" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classAllInstVarNames'(Self)"
        ]),
        // BT-785: Class removal (removeFromSystem)
        "classRemoveFromSystem" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classRemoveFromSystem'(Self)"
        ]),
        // BT-845 / ADR 0040 Phase 2: source file and reload
        "classSourceFile" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classSourceFile'(Self)"
        ]),
        "classReload" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classReload'(Self)"
        ]),
        // ADR 0033: Runtime-embedded documentation
        "classDoc" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classDoc'(Self)"
        ]),
        "classSetDoc" => {
            let doc = params.first()?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classSetDoc'(Self, ",
                doc.clone(),
                ")",
            ])
        }
        "classSetMethodDoc" => {
            let sel = params.first()?;
            let doc = params.get(1)?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classSetMethodDoc'(Self, ",
                sel.clone(),
                ", ",
                doc.clone(),
                ")",
            ])
        }
        _ => None,
    }
}

/// Generates Core Erlang for a Class primitive.
///
/// These back the `@primitive "classXxx"` method bodies in `lib/Class.bt`.
pub fn generate_class_bif(selector: &str, _params: &[String]) -> Option<Document<'static>> {
    match selector {
        "className" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'className'(Self)"
        ]),
        "classClass" => Some(docvec![
            "call 'beamtalk_behaviour_intrinsics':'classClass'(Self)"
        ]),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn doc_to_string(doc: Option<Document<'_>>) -> Option<String> {
        doc.map(|d| d.to_pretty_string())
    }

    #[test]
    fn test_class_superclass() {
        let result = doc_to_string(generate_behaviour_bif("classSuperclass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSuperclass'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_subclasses() {
        let result = doc_to_string(generate_behaviour_bif("classSubclasses", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSubclasses'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_all_subclasses() {
        let result = doc_to_string(generate_behaviour_bif("classAllSubclasses", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classAllSubclasses'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_local_methods() {
        let result = doc_to_string(generate_behaviour_bif("classLocalMethods", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classLocalMethods'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_includes_selector() {
        let result = doc_to_string(generate_behaviour_bif(
            "classIncludesSelector",
            &["Selector".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classIncludesSelector'(Self, Selector)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_class_inst_var_names() {
        let result = doc_to_string(generate_behaviour_bif("classInstVarNames", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classInstVarNames'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_name() {
        let result = doc_to_string(generate_class_bif("className", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'className'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_class() {
        let result = doc_to_string(generate_class_bif("classClass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classClass'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_doc() {
        let result = doc_to_string(generate_behaviour_bif("classDoc", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classDoc'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_set_doc() {
        let result = doc_to_string(generate_behaviour_bif(
            "classSetDoc",
            &["DocStr".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSetDoc'(Self, DocStr)".to_string())
        );
    }

    #[test]
    fn test_class_set_method_doc() {
        let result = doc_to_string(generate_behaviour_bif(
            "classSetMethodDoc",
            &["Selector".to_string(), "DocStr".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classSetMethodDoc'(Self, Selector, DocStr)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_class_remove_from_system() {
        let result = doc_to_string(generate_behaviour_bif("classRemoveFromSystem", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classRemoveFromSystem'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_source_file() {
        let result = doc_to_string(generate_behaviour_bif("classSourceFile", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSourceFile'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_reload() {
        let result = doc_to_string(generate_behaviour_bif("classReload", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classReload'(Self)".to_string())
        );
    }

    #[test]
    fn test_unknown_behaviour_selector() {
        let result = doc_to_string(generate_behaviour_bif("unknownMethod", &[]));
        assert!(result.is_none());
    }

    #[test]
    fn test_unknown_class_selector() {
        let result = doc_to_string(generate_class_bif("unknownMethod", &[]));
        assert!(result.is_none());
    }
}
