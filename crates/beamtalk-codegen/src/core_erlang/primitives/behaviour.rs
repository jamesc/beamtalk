// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive BIF mappings for the Behaviour / Class / Metaclass tower
//! (ADR 0032 Phase 2, ADR 0036 Phase 2).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "classXxx"` (declared in `behaviour.bt` / `class.bt`) and
//! `@primitive "metaclassXxx"` (declared in `metaclass.bt`) to direct calls
//! into `beamtalk_behaviour_intrinsics` (or, for the live-refactoring
//! primitives, `beamtalk_class_refactor` / `beamtalk_selector_refactor`; see
//! `intrinsic_module`). These are thin data-access functions —
//! hierarchy-walking logic lives in pure Beamtalk.
//!
//! The whole tower shares **one** selector table
//! ([`generate_tower_bif`]), reached for all three classes via the primitive
//! registry in [`super`]. A primitive's lowering therefore follows the
//! primitive, not the class that declares it, so moving a method up or down the
//! tower can't silently drop it to runtime dispatch. Tower selectors are
//! uniquely named (`classXxx` / `metaclassXxx`) so the merged table has no
//! collisions.
//!
//! Exception: `@primitive "methodLookup"` maps to
//! `beamtalk_method_resolver:resolve/2` instead, since method lookup is a
//! separate domain service.

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

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
    "classAllSuperclasses",
    "classMethods",
    "classFieldNames",
    "classAllFieldNames",
    // ADR 0124 §9/B5b (BT-3550): field name -> eager|late kind reflection.
    "classFieldKinds",
    "classAllFieldKinds",
    "classClassVarNames",
    "classAllClassVarNames",
    "classRemoveFromSystem",
    "classSourceFile",
    "classReload",
    "classDoc",
    "classProtocols",
    // ADR 0127 §12: this class's own `uses:` list, in declaration order.
    "classUsedProtocols",
    // ADR 0123 §1: declared shape version (default 1 read at the intrinsic).
    "classShapeVersion",
    // --- Metaclass (`metaclassXxx`) ---
    "metaclassThisClass",
    "metaclassSuperclass",
    "metaclassAllMethods",
    "metaclassClassMethods",
    "metaclassLocalClassMethods",
];

/// The runtime module that implements a Behaviour/Class/Metaclass tower
/// primitive.
///
/// The live-refactoring primitives live outside `beamtalk_behaviour_intrinsics`
/// (BT-4703): method removal, class rename and live method patch in
/// `beamtalk_class_refactor`, method rename in `beamtalk_selector_refactor`.
/// Everything else is class introspection.
fn intrinsic_module(func: &str) -> &'static str {
    match func {
        "classRemoveSelector"
        | "classRemoveSelectorIfAbsent"
        | "classRenameTo"
        | "classCompileSource"
        | "classTryCompileSource"
        | "classPrecheckCompileSource" => "beamtalk_class_refactor",
        "classRenameSelector" | "classRenameSelectorIfAbsent" => "beamtalk_selector_refactor",
        _ => "beamtalk_behaviour_intrinsics",
    }
}

/// The `call 'module':'func'` head of an intrinsic call, with the module
/// chosen by [`intrinsic_module`].
fn call_head(func: &str) -> Document<'static> {
    docvec![
        "call ",
        leaf::atom(intrinsic_module(func).to_owned()),
        ":",
        leaf::atom(func.to_owned())
    ]
}

/// Generates a `call '<intrinsic_module(func)>':'func'(Self)` Document.
fn intrinsic_self(func: &str) -> Document<'static> {
    docvec![call_head(func), "(Self)"]
}

/// Generates a `call '<intrinsic_module(func)>':'func'(Self, Arg)` Document.
fn intrinsic_self_arg(func: &str, arg: &str) -> Document<'static> {
    docvec![call_head(func), "(Self, ", leaf::var(arg.to_owned()), ")"]
}

/// Generates a `call '<intrinsic_module(func)>':'func'(Self, Arg1, Arg2)` Document.
fn intrinsic_self_arg2(func: &str, arg1: &str, arg2: &str) -> Document<'static> {
    docvec![
        call_head(func),
        "(Self, ",
        leaf::var(arg1.to_owned()),
        ", ",
        leaf::var(arg2.to_owned()),
        ")"
    ]
}

/// Generates Core Erlang for a Behaviour / Class / Metaclass tower primitive.
///
/// One table for the whole metaclass tower: the registry routes all
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
        | "classRemoveSelector"
        // ADR 0114 Phase 2: renameTo: — a single Symbol argument,
        // same shape as classRemoveSelector.
        | "classRenameTo"
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
        // ADR 0112 Phase 2: removeSelector:ifAbsent: — the selector
        // Symbol plus the block (a compiled Block passed as a value, evaluated
        // by the intrinsic directly like any block argument to a Behaviour
        // primitive).
        "classRemoveSelectorIfAbsent" => {
            let sel = params.first()?;
            let absent_block = params.get(1)?;
            Some(docvec![
                call_head("classRemoveSelectorIfAbsent"),
                "(Self, ",
                leaf::var(sel.clone()),
                ", ",
                leaf::var(absent_block.clone()),
                ")"
            ])
        }
        // ADR 0114 Phase 3: renameSelector:to: — two Symbol arguments (old
        // selector, new selector). ADR 0123 §3: migrateShape:from: — a
        // Dictionary of raw fields plus the version they're at, wrapping
        // the whole `beamtalk_shape_migration:migrate/3` chain + reconcile.
        // Same two-positional-argument shape as classRenameSelector, so one
        // arm covers both.
        "classRenameSelector" | "classMigrateShapeFrom" => {
            let arg1 = params.first()?;
            let arg2 = params.get(1)?;
            Some(intrinsic_self_arg2(selector, arg1, arg2))
        }
        // ADR 0114 Phase 3: renameSelector:to:ifAbsent: — old/new
        // selector Symbols plus the block, mirroring
        // classRemoveSelectorIfAbsent's shape above.
        "classRenameSelectorIfAbsent" => {
            let old_sel = params.first()?;
            let new_sel = params.get(1)?;
            let absent_block = params.get(2)?;
            Some(docvec![
                call_head("classRenameSelectorIfAbsent"),
                "(Self, ",
                leaf::var(old_sel.clone()),
                ", ",
                leaf::var(new_sel.clone()),
                ", ",
                leaf::var(absent_block.clone()),
                ")"
            ])
        }
        // ADR 0082 Phase 1: live method patch primitives. Both take a
        // selector Symbol and a body String *as values* and share one
        // compile-and-install path; only the intent differs (`durable` vs
        // `ephemeral`), which the intrinsic encodes when it logs the ChangeEntry.
        // ADR 0105 Phase 3: the read-only pre-save advisory precheck
        // shares the same (Self, selector, source) shape — nothing installs,
        // but the intrinsic still needs the pending selector + body values.
        "classCompileSource" | "classTryCompileSource" | "classPrecheckCompileSource" => {
            let sel = params.first()?;
            let source = params.get(1)?;
            Some(docvec![
                call_head(selector),
                "(Self, ",
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
    use super::super::{assert_bif_none_table, assert_bif_table};
    use super::*;

    #[test]
    #[allow(clippy::too_many_lines)] // one contiguous table; splitting hurts readability
    fn generate_tower_bif_table() {
        assert_bif_table(
            generate_tower_bif,
            &[
                // --- Behaviour / Class tower selectors ---
                (
                    "test_class_name",
                    "className",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'className'(Self)",
                ),
                (
                    "test_class_class",
                    "classClass",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classClass'(Self)",
                ),
                (
                    "test_class_superclass",
                    "classSuperclass",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classSuperclass'(Self)",
                ),
                (
                    "test_class_subclasses",
                    "classSubclasses",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classSubclasses'(Self)",
                ),
                (
                    "test_class_all_subclasses",
                    "classAllSubclasses",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classAllSubclasses'(Self)",
                ),
                (
                    "test_class_local_methods",
                    "classLocalMethods",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classLocalMethods'(Self)",
                ),
                (
                    "test_class_includes_selector",
                    "classIncludesSelector",
                    &["Selector"],
                    "call 'beamtalk_behaviour_intrinsics':'classIncludesSelector'(Self, Selector)",
                ),
                (
                    "test_class_field_names",
                    "classFieldNames",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classFieldNames'(Self)",
                ),
                (
                    "test_class_all_field_names",
                    "classAllFieldNames",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classAllFieldNames'(Self)",
                ),
                (
                    "test_class_class_var_names",
                    "classClassVarNames",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classClassVarNames'(Self)",
                ),
                (
                    "test_class_all_class_var_names",
                    "classAllClassVarNames",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classAllClassVarNames'(Self)",
                ),
                (
                    "test_class_used_protocols",
                    "classUsedProtocols",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classUsedProtocols'(Self)",
                ),
                (
                    "test_class_doc",
                    "classDoc",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classDoc'(Self)",
                ),
                (
                    "test_class_set_doc",
                    "classSetDoc",
                    &["DocStr"],
                    "call 'beamtalk_behaviour_intrinsics':'classSetDoc'(Self, DocStr)",
                ),
                (
                    "test_class_set_method_doc",
                    "classSetMethodDoc",
                    &["Selector", "DocStr"],
                    "call 'beamtalk_behaviour_intrinsics':'classSetMethodDoc'(Self, Selector, DocStr)",
                ),
                (
                    "test_class_doc_for_method",
                    "classDocForMethod",
                    &["Selector"],
                    "call 'beamtalk_behaviour_intrinsics':'classDocForMethod'(Self, Selector)",
                ),
                (
                    "test_class_remove_from_system",
                    "classRemoveFromSystem",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classRemoveFromSystem'(Self)",
                ),
                (
                    "test_class_source_file",
                    "classSourceFile",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classSourceFile'(Self)",
                ),
                (
                    "test_class_reload",
                    "classReload",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'classReload'(Self)",
                ),
                (
                    "test_class_compile_source",
                    "classCompileSource",
                    &["Selector", "Source"],
                    "call 'beamtalk_class_refactor':'classCompileSource'(Self, Selector, Source)",
                ),
                (
                    "test_class_try_compile_source",
                    "classTryCompileSource",
                    &["Selector", "Source"],
                    "call 'beamtalk_class_refactor':'classTryCompileSource'(Self, Selector, Source)",
                ),
                (
                    "test_class_precheck_compile_source",
                    "classPrecheckCompileSource",
                    &["Selector", "Source"],
                    "call 'beamtalk_class_refactor':'classPrecheckCompileSource'(Self, Selector, Source)",
                ),
                (
                    "test_class_remove_selector",
                    "classRemoveSelector",
                    &["Selector"],
                    "call 'beamtalk_class_refactor':'classRemoveSelector'(Self, Selector)",
                ),
                (
                    "test_class_rename_to",
                    "classRenameTo",
                    &["NewName"],
                    "call 'beamtalk_class_refactor':'classRenameTo'(Self, NewName)",
                ),
                (
                    "test_class_remove_selector_if_absent",
                    "classRemoveSelectorIfAbsent",
                    &["Selector", "AbsentBlock"],
                    "call 'beamtalk_class_refactor':'classRemoveSelectorIfAbsent'(Self, Selector, AbsentBlock)",
                ),
                (
                    "test_method_lookup",
                    "methodLookup",
                    &["Selector"],
                    "call 'beamtalk_method_resolver':'resolve'(Self, Selector)",
                ),
                // --- Metaclass tower selectors ---
                (
                    "test_metaclass_this_class",
                    "metaclassThisClass",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'metaclassThisClass'(Self)",
                ),
                (
                    "test_metaclass_superclass",
                    "metaclassSuperclass",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'metaclassSuperclass'(Self)",
                ),
                (
                    "test_metaclass_all_methods",
                    "metaclassAllMethods",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'metaclassAllMethods'(Self)",
                ),
                (
                    "test_metaclass_class_methods",
                    "metaclassClassMethods",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'metaclassClassMethods'(Self)",
                ),
                (
                    "test_metaclass_local_class_methods",
                    "metaclassLocalClassMethods",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'metaclassLocalClassMethods'(Self)",
                ),
                (
                    "test_metaclass_includes_selector",
                    "metaclassIncludesSelector",
                    &["Selector"],
                    "call 'beamtalk_behaviour_intrinsics':'metaclassIncludesSelector'(Self, Selector)",
                ),
                (
                    "test_metaclass_new",
                    "metaclassNew",
                    &[],
                    "call 'beamtalk_behaviour_intrinsics':'metaclassNew'()",
                ),
            ],
        );
    }

    #[test]
    fn generate_tower_bif_none_table() {
        assert_bif_none_table(
            generate_tower_bif,
            &[("test_unknown_tower_selector", "unknownMethod", &[])],
        );
    }
}
