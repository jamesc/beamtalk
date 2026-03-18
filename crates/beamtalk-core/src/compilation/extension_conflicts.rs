// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Extension method conflict detection — compile-time duplicate checking.
//!
//! **DDD Context:** Compilation
//!
//! Part of ADR 0066 Phase 3. This module checks the [`ExtensionIndex`] for
//! duplicate `{Class, Side, Selector}` registrations and produces diagnostics.
//! Two extensions conflict if they target the same class, same side (instance
//! or class), and same selector — regardless of which file they appear in.

use crate::ast::Module;
use crate::compilation::extension_index::{
    ExtensionIndex, ExtensionKey, ExtensionLocation, MethodSide,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use std::path::Path;

/// An extension conflict: two or more definitions of the same `{Class, Side, Selector}`.
#[derive(Debug, Clone)]
pub struct ExtensionConflict {
    /// The conflicting extension key.
    pub key: ExtensionKey,
    /// All locations where this extension is defined (at least 2).
    pub locations: Vec<ExtensionLocation>,
}

/// Detects duplicate extension method definitions in the index.
///
/// Returns one [`ExtensionConflict`] for each `{Class, Side, Selector}` that
/// has more than one definition. The caller should convert these into
/// diagnostics using [`conflict_diagnostics`].
#[must_use]
pub fn detect_extension_conflicts(index: &ExtensionIndex) -> Vec<ExtensionConflict> {
    index
        .conflicts()
        .into_iter()
        .map(|(key, locs)| ExtensionConflict {
            key: key.clone(),
            locations: locs.to_vec(),
        })
        .collect()
}

/// Converts extension conflicts into compiler diagnostics.
///
/// For each conflict, produces one error diagnostic per duplicate location
/// (all locations after the first). Each error spans the duplicate's location,
/// and the hint references the first definition's location.
///
/// When all definitions are in the same file, the hint says "also defined at
/// offset N in this file". For cross-file conflicts, it includes the file path.
#[must_use]
pub fn conflict_diagnostics(conflicts: &[ExtensionConflict]) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for conflict in conflicts {
        let key = &conflict.key;
        let side_label = match key.side {
            MethodSide::Instance => "",
            MethodSide::Class => " class",
        };
        let display_key = format!("{}{}>>{}", key.class_name, side_label, key.selector);

        // Sort locations for deterministic output (by file path, then span start)
        let mut locs = conflict.locations.clone();
        locs.sort_by(|a, b| {
            a.file_path
                .cmp(&b.file_path)
                .then_with(|| a.span.start().cmp(&b.span.start()))
        });

        // The first location is the "original" definition.
        // All subsequent locations get an error pointing to them, with a hint
        // referencing the first definition.
        let Some(first) = locs.first() else {
            continue;
        };

        for duplicate in &locs[1..] {
            let message = format!("extension conflict on `{display_key}`: duplicate definition");

            let hint = format_conflict_hint(first, duplicate);

            diagnostics.push(
                Diagnostic::error(message, duplicate.span)
                    .with_hint(hint)
                    .with_category(DiagnosticCategory::ExtensionConflict),
            );
        }
    }

    diagnostics
}

/// Formats the hint text for a conflict diagnostic.
///
/// If both definitions are in the same file, the hint references the offset.
/// For cross-file conflicts, it includes the file path.
fn format_conflict_hint(first: &ExtensionLocation, duplicate: &ExtensionLocation) -> String {
    if first.file_path == duplicate.file_path {
        format!(
            "first defined at offset {} in this file; rename one of the extensions to avoid the conflict",
            first.span.start()
        )
    } else {
        format!(
            "first defined in {}; rename one of the extensions to avoid the conflict",
            display_path(&first.file_path)
        )
    }
}

/// Detects extension methods that shadow a local class method.
///
/// When a class defines a method `foo` and an extension also defines
/// `ClassName >> foo`, the local method always wins at runtime. This
/// function emits a warning for each such shadowing so the developer
/// knows the extension will be ignored for instances of that class.
///
/// Only checks classes defined in the provided modules — extensions
/// targeting classes from dependencies are not flagged (the local
/// definition is not visible here).
#[must_use]
pub fn shadowing_diagnostics(index: &ExtensionIndex, modules: &[&Module]) -> Vec<Diagnostic> {
    use std::collections::HashSet;

    // Build a set of (class_name, side, selector) for all locally-defined methods.
    let mut local_methods: HashSet<(ecow::EcoString, MethodSide, ecow::EcoString)> = HashSet::new();

    for module in modules {
        for class in &module.classes {
            let class_name = &class.name.name;
            for method in &class.methods {
                local_methods.insert((
                    class_name.clone(),
                    MethodSide::Instance,
                    method.selector.name(),
                ));
            }
            for method in &class.class_methods {
                local_methods.insert((
                    class_name.clone(),
                    MethodSide::Class,
                    method.selector.name(),
                ));
            }
        }
    }

    let mut diagnostics = Vec::new();

    for (key, locs) in index.entries() {
        let local_key = (
            key.class_name.clone(),
            key.side.clone(),
            key.selector.clone(),
        );
        if local_methods.contains(&local_key) {
            let side_label = match key.side {
                MethodSide::Instance => "",
                MethodSide::Class => " class",
            };
            let display_key = format!("{}{}>>{}", key.class_name, side_label, key.selector);

            for loc in locs {
                diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "extension `{display_key}` shadows a local class method (local always wins)"
                        ),
                        loc.span,
                    )
                    .with_hint(
                        "the local method defined in the class body takes precedence; \
                         this extension will only apply to subclasses that don't override it"
                            .to_string(),
                    )
                    .with_category(DiagnosticCategory::ExtensionConflict),
                );
            }
        }
    }

    diagnostics
}

/// Displays a path for diagnostic messages.
///
/// Uses the display representation, which is platform-appropriate.
fn display_path(path: &Path) -> String {
    path.display().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compilation::extension_index::collect_extensions;
    use crate::source_analysis::Severity;
    use crate::test_helpers::test_support::parse_bt;
    use std::path::PathBuf;

    #[test]
    fn no_conflicts_produces_no_diagnostics() {
        let module_a = parse_bt("String >> shout => self\n");
        let module_b = parse_bt("Integer >> double => self\n");
        let path_a = PathBuf::from("String+Shout.bt");
        let path_b = PathBuf::from("Integer+Math.bt");

        let index = collect_extensions(vec![
            (path_a.as_path(), &module_a),
            (path_b.as_path(), &module_b),
        ]);

        let conflicts = detect_extension_conflicts(&index);
        assert!(conflicts.is_empty());

        let diagnostics = conflict_diagnostics(&conflicts);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn cross_file_conflict_produces_error() {
        let module_a = parse_bt("String >> json => self\n");
        let module_b = parse_bt("String >> json => self\n");
        let path_a = PathBuf::from("String+JSON.bt");
        let path_b = PathBuf::from("String+Serialization.bt");

        let index = collect_extensions(vec![
            (path_a.as_path(), &module_a),
            (path_b.as_path(), &module_b),
        ]);

        let conflicts = detect_extension_conflicts(&index);
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].locations.len(), 2);

        let diagnostics = conflict_diagnostics(&conflicts);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("extension conflict"));
        assert!(diagnostics[0].message.contains("String>>json"));
        assert!(diagnostics[0].hint.as_ref().unwrap().contains("rename one"));
    }

    #[test]
    fn same_file_conflict_produces_error() {
        let module = parse_bt("String >> json => self\nString >> json => self\n");
        let path = PathBuf::from("String+JSON.bt");

        let index = collect_extensions(vec![(path.as_path(), &module)]);

        let conflicts = detect_extension_conflicts(&index);
        assert_eq!(conflicts.len(), 1);

        let diagnostics = conflict_diagnostics(&conflicts);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .unwrap()
                .contains("in this file")
        );
    }

    #[test]
    fn different_classes_same_selector_no_conflict() {
        let module_a = parse_bt("String >> json => self\n");
        let module_b = parse_bt("Array >> json => self\n");
        let path_a = PathBuf::from("String+JSON.bt");
        let path_b = PathBuf::from("Array+JSON.bt");

        let index = collect_extensions(vec![
            (path_a.as_path(), &module_a),
            (path_b.as_path(), &module_b),
        ]);

        let conflicts = detect_extension_conflicts(&index);
        assert!(conflicts.is_empty());
    }

    #[test]
    fn instance_and_class_side_same_selector_no_conflict() {
        let module = parse_bt("String >> json => self\nString class >> json => self\n");
        let path = PathBuf::from("String+JSON.bt");

        let index = collect_extensions(vec![(path.as_path(), &module)]);

        let conflicts = detect_extension_conflicts(&index);
        assert!(conflicts.is_empty());
    }

    #[test]
    fn class_side_conflict_detected() {
        let module_a = parse_bt("String class >> fromJson => self\n");
        let module_b = parse_bt("String class >> fromJson => self\n");
        let path_a = PathBuf::from("String+JSON.bt");
        let path_b = PathBuf::from("String+Parse.bt");

        let index = collect_extensions(vec![
            (path_a.as_path(), &module_a),
            (path_b.as_path(), &module_b),
        ]);

        let conflicts = detect_extension_conflicts(&index);
        assert_eq!(conflicts.len(), 1);

        let diagnostics = conflict_diagnostics(&conflicts);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("class>>fromJson"));
    }

    #[test]
    fn three_way_conflict_produces_two_diagnostics() {
        let module_a = parse_bt("String >> json => self\n");
        let module_b = parse_bt("String >> json => self\n");
        let module_c = parse_bt("String >> json => self\n");
        let path_a = PathBuf::from("a.bt");
        let path_b = PathBuf::from("b.bt");
        let path_c = PathBuf::from("c.bt");

        let index = collect_extensions(vec![
            (path_a.as_path(), &module_a),
            (path_b.as_path(), &module_b),
            (path_c.as_path(), &module_c),
        ]);

        let conflicts = detect_extension_conflicts(&index);
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].locations.len(), 3);

        let diagnostics = conflict_diagnostics(&conflicts);
        // One diagnostic per duplicate (all after the first)
        assert_eq!(diagnostics.len(), 2);
        for diag in &diagnostics {
            assert_eq!(diag.severity, Severity::Error);
        }
    }

    #[test]
    fn keyword_selector_conflict_detected() {
        let module_a = parse_bt("Array >> chunksOf: n => self\n");
        let module_b = parse_bt("Array >> chunksOf: size => self\n");
        let path_a = PathBuf::from("Array+Chunking.bt");
        let path_b = PathBuf::from("Array+Utils.bt");

        let index = collect_extensions(vec![
            (path_a.as_path(), &module_a),
            (path_b.as_path(), &module_b),
        ]);

        let conflicts = detect_extension_conflicts(&index);
        assert_eq!(conflicts.len(), 1);

        let diagnostics = conflict_diagnostics(&conflicts);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("Array>>chunksOf:"));
    }

    #[test]
    fn cross_file_hint_includes_file_path() {
        let module_a = parse_bt("String >> json => self\n");
        let module_b = parse_bt("String >> json => self\n");
        let path_a = PathBuf::from("src/String+JSON.bt");
        let path_b = PathBuf::from("src/String+Serialization.bt");

        let index = collect_extensions(vec![
            (path_a.as_path(), &module_a),
            (path_b.as_path(), &module_b),
        ]);

        let conflicts = detect_extension_conflicts(&index);
        let diagnostics = conflict_diagnostics(&conflicts);
        assert_eq!(diagnostics.len(), 1);

        let hint = diagnostics[0].hint.as_ref().unwrap();
        // The hint should mention the first file's path
        assert!(
            hint.contains("src/String+JSON.bt") || hint.contains("src/String+Serialization.bt"),
            "hint should contain a file path, got: {hint}"
        );
    }

    // ── Shadowing tests ─────────────────────────────────────────────────

    #[test]
    fn extension_shadowing_local_method_produces_warning() {
        // Class defines `json`, extension also defines `MyClass >> json`
        let class_module = parse_bt("Object subclass: MyClass\n  json => \"local\"\n");
        let ext_module = parse_bt("MyClass >> json => \"extension\"\n");
        let path_ext = PathBuf::from("MyClass+JSON.bt");

        let index = collect_extensions(vec![(path_ext.as_path(), &ext_module)]);

        let diagnostics = shadowing_diagnostics(&index, &[&class_module]);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0]
                .message
                .contains("shadows a local class method")
        );
        assert!(diagnostics[0].message.contains("MyClass>>json"));
    }

    #[test]
    fn extension_not_shadowing_produces_no_warning() {
        // Class defines `json`, extension defines `MyClass >> shout` (different selector)
        let class_module = parse_bt("Object subclass: MyClass\n  json => \"local\"\n");
        let ext_module = parse_bt("MyClass >> shout => \"extension\"\n");
        let path_ext = PathBuf::from("MyClass+Shout.bt");

        let index = collect_extensions(vec![(path_ext.as_path(), &ext_module)]);

        let diagnostics = shadowing_diagnostics(&index, &[&class_module]);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn extension_targeting_different_class_no_shadowing() {
        // Class MyClass defines `json`, extension targets OtherClass >> json
        let class_module = parse_bt("Object subclass: MyClass\n  json => \"local\"\n");
        let ext_module = parse_bt("OtherClass >> json => \"extension\"\n");
        let path_ext = PathBuf::from("OtherClass+JSON.bt");

        let index = collect_extensions(vec![(path_ext.as_path(), &ext_module)]);

        let diagnostics = shadowing_diagnostics(&index, &[&class_module]);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn class_side_shadowing_detected() {
        let class_module = parse_bt("Object subclass: MyClass\n  class fromJson => \"local\"\n");
        let ext_module = parse_bt("MyClass class >> fromJson => \"extension\"\n");
        let path_ext = PathBuf::from("MyClass+JSON.bt");

        let index = collect_extensions(vec![(path_ext.as_path(), &ext_module)]);

        let diagnostics = shadowing_diagnostics(&index, &[&class_module]);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("class>>fromJson"));
    }
}
