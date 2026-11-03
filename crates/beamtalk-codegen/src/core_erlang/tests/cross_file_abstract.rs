// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4391: a typed send to an `abstract` class declared in another file of
//! the same build stays dynamic for reason `abstract`, exactly as for a
//! same-file abstract class (`dynamic_site_report::abstract_class_reports_abstract`).
//!
//! The declaring file reaches the caller's codegen the way every multi-file
//! driver hands it over: as the full Pass 1 `ClassInfo`
//! (`ClassHierarchy::extract_class_infos`, the build's `pre_loaded_classes`;
//! BEAM metadata in the REPL) plus the class→module and class→superclass
//! indexes. That `ClassInfo` carries `is_abstract`, and the driver folds it in
//! before the superclass-index backfill (`add_external_superclasses` fills
//! only vacant names), so no cross-file class is ever seen as a flag-less stub
//! when its declaration is in scope.

use std::collections::HashMap;

use beamtalk_core::semantic_analysis::ClassHierarchy;

const PACKAGE: &str = "xf";

/// Parse `src` as one file of package [`PACKAGE`].
fn parse(src: &str) -> beamtalk_core::ast::Module {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    module
}

/// Compile `caller_src` as `bt@xf@caller` against `other_src` declared in
/// `bt@xf@<other_stem>`, fed through the same inputs a package build hands
/// codegen. Returns (dynamic-dispatch hints, generated Core Erlang).
fn compile_against(caller_src: &str, other_src: &str, other_stem: &str) -> (Vec<String>, String) {
    let other = parse(other_src);
    let other_module = format!("bt@{PACKAGE}@{other_stem}");
    // Pass 1 (`build_class_module_index`): per-file infos, package-stamped,
    // plus the two name indexes.
    let mut class_infos = ClassHierarchy::extract_class_infos(&other);
    ClassHierarchy::stamp_package_on_infos(&mut class_infos, PACKAGE);
    let mut class_module_index = HashMap::new();
    let mut class_superclass_index = HashMap::new();
    for class in &other.classes {
        class_module_index.insert(class.name.name.to_string(), other_module.clone());
        if let Some(superclass) = &class.superclass {
            class_superclass_index.insert(class.name.name.to_string(), superclass.name.to_string());
        }
    }
    class_module_index.insert("Caller".to_string(), format!("bt@{PACKAGE}@caller"));

    let caller = parse(caller_src);
    let result = crate::core_erlang::generate_module_with_warnings(
        &caller,
        crate::core_erlang::CodegenOptions::new(&format!("bt@{PACKAGE}@caller"))
            .with_class_module_index(class_module_index)
            .with_class_superclass_index(class_superclass_index)
            .with_class_hierarchy(class_infos)
            .with_codegen_diagnostics(true),
    )
    .expect("codegen should succeed");
    let hints = result
        .warnings
        .into_iter()
        .map(|w| w.message.to_string())
        .filter(|m| m.contains("dynamic dispatch"))
        .collect();
    (hints, result.code)
}

const CALLER: &str = "Object subclass: Caller\n  run: s :: Shape => s area\n";

#[test]
fn cross_file_abstract_class_reports_abstract() {
    let (hints, code) = compile_against(
        CALLER,
        "abstract Value subclass: Shape\n  area => self subclassResponsibility\n",
        "shape",
    );
    let area: Vec<&String> = hints
        .iter()
        .filter(|h| h.starts_with("Send 'area'"))
        .collect();
    assert_eq!(area.len(), 1, "{hints:?}");
    assert!(area[0].contains("reason: abstract — Shape"), "{}", area[0]);
    // No class-tag guard (it could never hit) and no direct call.
    assert!(!code.contains("'bt@xf@shape':'dispatch'"), "{code}");
    assert!(!code.contains("_TTaggedRecv"), "{code}");
}

#[test]
fn cross_file_concrete_class_takes_the_guarded_call() {
    // Contrast: the same inputs for a concrete class do take the guarded
    // direct call, so the test above is not passing for an unrelated gate.
    let (hints, code) = compile_against(CALLER, "Value subclass: Shape\n  area => 0\n", "shape");
    assert!(
        !hints.iter().any(|h| h.starts_with("Send 'area'")),
        "{hints:?}"
    );
    assert!(code.contains("'bt@xf@shape':'dispatch'('area'"), "{code}");
}
