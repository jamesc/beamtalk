// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive BIF mappings for the Behaviour / Class / Metaclass tower
//! (ADR 0032 Phase 2, ADR 0036 Phase 2).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "classXxx"` (declared in `Behaviour.bt` / `Class.bt`) and
//! `@primitive "metaclassXxx"` (declared in `Metaclass.bt`) to direct calls
//! into `beamtalk_behaviour_intrinsics`. These are thin data-access functions —
//! hierarchy-walking logic lives in pure Beamtalk.
//!
//! BT-2234: the whole tower shares **one** selector table
//! ([`generate_tower_bif`]), reached for all three classes via the primitive
//! registry in [`super`]. A primitive's lowering therefore follows the
//! primitive, not the class that declares it, so moving a method up or down the
//! tower can't silently drop it to runtime dispatch (the BT-2232 `className`
//! regression). Tower selectors are uniquely named (`classXxx` / `metaclassXxx`)
//! so the merged table has no collisions.
//!
//! Exception: `@primitive "methodLookup"` (BT-1735) maps to
//! `beamtalk_method_resolver:resolve/2` instead, since method lookup is a
//! separate domain service.

use super::super::document::Document;
use super::super::document::leaf;
use crate::docvec;

/// Zero-arg tower intrinsics: the selector name is also the
/// `beamtalk_behaviour_intrinsics` function name, called as `func(Self)`.
const TOWER_ZERO_ARG: &[&str] = &[
    // --- Behaviour / Class (`classXxx`) ---
    "className",
    "classClass",
    "classSuperclass",
    "classSubclasses",
    "classAllSubclasses",
    "classLocalMethods",
    "classInstVarNames",
    "classAllSuperclasses",
    "classMethods",
    "classAllInstVarNames",
    "classFieldNames",
    "classAllFieldNames",
    "classClassVarNames",
    "classAllClassVarNames",
    "classRemoveFromSystem",
    "classSourceFile",
    "classReload",
    "classDoc",
    "classProtocols",
    // --- Metaclass (`metaclassXxx`) ---
    "metaclassThisClass",
    "metaclassSuperclass",
    "metaclassAllMethods",
    "metaclassClassMethods",
    "metaclassLocalClassMethods",
];

/// Generates a `call 'beamtalk_behaviour_intrinsics':'func'(Self)` Document.
fn intrinsic_self(func: &str) -> Document<'static> {
    docvec![
        "call 'beamtalk_behaviour_intrinsics':'",
        leaf::var(func.to_owned()),
        "'(Self)"
    ]
}

/// Generates a `call 'beamtalk_behaviour_intrinsics':'func'(Self, Arg)` Document.
fn intrinsic_self_arg(func: &str, arg: &str) -> Document<'static> {
    docvec![
        "call 'beamtalk_behaviour_intrinsics':'",
        leaf::var(func.to_owned()),
        "'(Self, ",
        leaf::var(arg.to_owned()),
        ")"
    ]
}

/// Generates Core Erlang for a Behaviour / Class / Metaclass tower primitive.
///
/// One table for the whole metaclass tower (BT-2234): the registry routes all
/// three classes here, so lowering follows the primitive, not the declaring
/// class.
pub fn generate_tower_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    if TOWER_ZERO_ARG.contains(&selector) {
        return Some(intrinsic_self(selector));
    }

    match selector {
        "classIncludesSelector"
        | "classDocForMethod"
        | "classSetDoc"
        | "classConformsTo"
        | "metaclassIncludesSelector" => {
            let arg = params.first()?;
            Some(intrinsic_self_arg(selector, arg))
        }
        "methodLookup" => {
            let arg = params.first()?;
            Some(docvec![
                "call 'beamtalk_method_resolver':'resolve'(Self, ",
                leaf::var(arg.clone()),
                ")"
            ])
        }
        "classSetMethodDoc" => {
            let sel = params.first()?;
            let doc = params.get(1)?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'classSetMethodDoc'(Self, ",
                leaf::var(sel.clone()),
                ", ",
                leaf::var(doc.clone()),
                ")"
            ])
        }
        // ADR 0082 Phase 1 (BT-2283): live method patch primitives. Both take a
        // selector Symbol and a body String *as values* and share one
        // compile-and-install path; only the intent differs (`durable` vs
        // `ephemeral`), which the intrinsic encodes when it logs the ChangeEntry.
        // ADR 0105 Phase 3 (BT-2782): the read-only pre-save advisory precheck
        // shares the same (Self, selector, source) shape — nothing installs,
        // but the intrinsic still needs the pending selector + body values.
        "classCompileSource" | "classTryCompileSource" | "classPrecheckCompileSource" => {
            let sel = params.first()?;
            let source = params.get(1)?;
            Some(docvec![
                "call 'beamtalk_behaviour_intrinsics':'",
                leaf::var(selector.to_owned()),
                "'(Self, ",
                leaf::var(sel.clone()),
                ", ",
                leaf::var(source.clone()),
                ")"
            ])
        }
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

    // --- Behaviour / Class tower selectors ---

    #[test]
    fn test_class_name() {
        let result = doc_to_string(generate_tower_bif("className", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'className'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_class() {
        let result = doc_to_string(generate_tower_bif("classClass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classClass'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_superclass() {
        let result = doc_to_string(generate_tower_bif("classSuperclass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSuperclass'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_subclasses() {
        let result = doc_to_string(generate_tower_bif("classSubclasses", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSubclasses'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_all_subclasses() {
        let result = doc_to_string(generate_tower_bif("classAllSubclasses", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classAllSubclasses'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_local_methods() {
        let result = doc_to_string(generate_tower_bif("classLocalMethods", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classLocalMethods'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_includes_selector() {
        let result = doc_to_string(generate_tower_bif(
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
        let result = doc_to_string(generate_tower_bif("classInstVarNames", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classInstVarNames'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_field_names() {
        let result = doc_to_string(generate_tower_bif("classFieldNames", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classFieldNames'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_all_field_names() {
        let result = doc_to_string(generate_tower_bif("classAllFieldNames", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classAllFieldNames'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_class_var_names() {
        let result = doc_to_string(generate_tower_bif("classClassVarNames", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classClassVarNames'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_all_class_var_names() {
        let result = doc_to_string(generate_tower_bif("classAllClassVarNames", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classAllClassVarNames'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_doc() {
        let result = doc_to_string(generate_tower_bif("classDoc", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classDoc'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_set_doc() {
        let result = doc_to_string(generate_tower_bif("classSetDoc", &["DocStr".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSetDoc'(Self, DocStr)".to_string())
        );
    }

    #[test]
    fn test_class_set_method_doc() {
        let result = doc_to_string(generate_tower_bif(
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
    fn test_class_doc_for_method() {
        let result = doc_to_string(generate_tower_bif(
            "classDocForMethod",
            &["Selector".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classDocForMethod'(Self, Selector)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_class_remove_from_system() {
        let result = doc_to_string(generate_tower_bif("classRemoveFromSystem", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classRemoveFromSystem'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_source_file() {
        let result = doc_to_string(generate_tower_bif("classSourceFile", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classSourceFile'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_reload() {
        let result = doc_to_string(generate_tower_bif("classReload", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'classReload'(Self)".to_string())
        );
    }

    #[test]
    fn test_class_compile_source() {
        let result = doc_to_string(generate_tower_bif(
            "classCompileSource",
            &["Selector".to_string(), "Source".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classCompileSource'(Self, Selector, Source)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_class_try_compile_source() {
        let result = doc_to_string(generate_tower_bif(
            "classTryCompileSource",
            &["Selector".to_string(), "Source".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classTryCompileSource'(Self, Selector, Source)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_class_precheck_compile_source() {
        let result = doc_to_string(generate_tower_bif(
            "classPrecheckCompileSource",
            &["Selector".to_string(), "Source".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_behaviour_intrinsics':'classPrecheckCompileSource'(Self, Selector, Source)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_method_lookup() {
        let result = doc_to_string(generate_tower_bif(
            "methodLookup",
            &["Selector".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_method_resolver':'resolve'(Self, Selector)".to_string())
        );
    }

    // --- Metaclass tower selectors ---

    #[test]
    fn test_metaclass_this_class() {
        let result = doc_to_string(generate_tower_bif("metaclassThisClass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassThisClass'(Self)".to_string())
        );
    }

    #[test]
    fn test_metaclass_superclass() {
        let result = doc_to_string(generate_tower_bif("metaclassSuperclass", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassSuperclass'(Self)".to_string())
        );
    }

    #[test]
    fn test_metaclass_all_methods() {
        let result = doc_to_string(generate_tower_bif("metaclassAllMethods", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassAllMethods'(Self)".to_string())
        );
    }

    #[test]
    fn test_metaclass_class_methods() {
        let result = doc_to_string(generate_tower_bif("metaclassClassMethods", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassClassMethods'(Self)".to_string())
        );
    }

    #[test]
    fn test_metaclass_local_class_methods() {
        let result = doc_to_string(generate_tower_bif("metaclassLocalClassMethods", &[]));
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
        let result = doc_to_string(generate_tower_bif(
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
    fn test_metaclass_new() {
        let result = doc_to_string(generate_tower_bif("metaclassNew", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'metaclassNew'()".to_string())
        );
    }

    #[test]
    fn test_unknown_tower_selector() {
        let result = doc_to_string(generate_tower_bif("unknownMethod", &[]));
        assert!(result.is_none());
    }
}
