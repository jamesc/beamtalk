// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Extension method index — compile-time collection of `StandaloneMethodDefinition` nodes.
//!
//! **DDD Context:** Compilation
//!
//! Part of ADR 0066 Phase 3. This module scans parsed ASTs across a project,
//! collecting all standalone method definitions (`ClassName >> selector => body`)
//! into an index keyed by `(ClassName, MethodSide, Selector)`. The index feeds
//! the conflict detector and type metadata emitter in subsequent compilation passes.

use crate::ast::{Module, TypeAnnotation};
use crate::source_analysis::Span;
use ecow::EcoString;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Whether an extension targets the instance side or the class (metaclass) side.
///
/// Instance-side and class-side methods are distinct and do not conflict:
/// `String >> json` and `String class >> json` target different dispatch tables.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MethodSide {
    /// An instance method: `ClassName >> selector => ...`
    Instance,
    /// A class method: `ClassName class >> selector => ...`
    Class,
}

/// A unique key identifying an extension method registration.
///
/// Two extensions conflict if and only if they share the same key.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionKey {
    /// The target class name (e.g., `"String"`, `"Integer"`).
    pub class_name: EcoString,
    /// Whether this targets the instance or class side.
    pub side: MethodSide,
    /// The full selector name (e.g., `"json"`, `"at:put:"`).
    pub selector: EcoString,
}

/// Source location of an extension method definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtensionLocation {
    /// The file containing the extension definition.
    pub file_path: PathBuf,
    /// The span of the `StandaloneMethodDefinition` within the file.
    pub span: Span,
    /// Type signature extracted from the method definition.
    pub type_info: ExtensionTypeInfo,
}

/// Type signature metadata for an extension method.
///
/// Extracted from the `StandaloneMethodDefinition` AST at collection time.
/// Used by the type checker to see extension methods as part of a class's
/// typed method surface. Unannotated parameters and return types are `None`,
/// representing `Dynamic` in the gradual type system.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtensionTypeInfo {
    /// Number of arguments (0 for unary, 1 for binary, N for keyword).
    pub arity: usize,
    /// Parameter type annotations. `None` elements mean the parameter is untyped (Dynamic).
    pub param_types: Vec<Option<EcoString>>,
    /// Return type annotation. `None` means untyped (Dynamic).
    pub return_type: Option<EcoString>,
}

/// An index of all extension method definitions across a project.
///
/// Maps each unique `(ClassName, Side, Selector)` to all source locations
/// where that extension is defined. When there are multiple entries for the
/// same key, the conflict detector should report a compile error.
#[derive(Debug, Clone, Default)]
pub struct ExtensionIndex {
    /// The extension definitions, keyed by `(ClassName, Side, Selector)`.
    entries: HashMap<ExtensionKey, Vec<ExtensionLocation>>,
}

impl ExtensionIndex {
    /// Creates a new, empty extension index.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Scans a parsed `Module` AST for `StandaloneMethodDefinition` nodes and
    /// adds them to the index.
    ///
    /// `file_path` identifies the source file containing the module. Each
    /// standalone method definition in `module.method_definitions` is indexed
    /// by its `(class_name, side, selector)` key.
    pub fn add_module(&mut self, module: &Module, file_path: &Path) {
        for defn in &module.method_definitions {
            let key = ExtensionKey {
                class_name: defn.class_name.name.clone(),
                side: if defn.is_class_method {
                    MethodSide::Class
                } else {
                    MethodSide::Instance
                },
                selector: defn.method.selector.name(),
            };
            let type_info = ExtensionTypeInfo {
                arity: defn.method.selector.arity(),
                param_types: defn
                    .method
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.as_ref().map(TypeAnnotation::type_name))
                    .collect(),
                return_type: defn
                    .method
                    .return_type
                    .as_ref()
                    .map(TypeAnnotation::type_name),
            };
            let location = ExtensionLocation {
                file_path: file_path.to_path_buf(),
                span: defn.span,
                type_info,
            };
            self.entries.entry(key).or_default().push(location);
        }
    }

    /// Returns all entries in the index.
    #[must_use]
    pub fn entries(&self) -> &HashMap<ExtensionKey, Vec<ExtensionLocation>> {
        &self.entries
    }

    /// Returns a mutable reference to the entries map.
    ///
    /// Only available in tests — used to construct indexes without parsing source files.
    #[cfg(test)]
    pub fn entries_mut(&mut self) -> &mut HashMap<ExtensionKey, Vec<ExtensionLocation>> {
        &mut self.entries
    }

    /// Looks up all definitions for the given extension key.
    ///
    /// Returns `None` if no extension with that key has been indexed.
    #[must_use]
    pub fn lookup(&self, key: &ExtensionKey) -> Option<&[ExtensionLocation]> {
        self.entries.get(key).map(Vec::as_slice)
    }

    /// Returns all extension keys that have more than one definition.
    ///
    /// These represent conflicts that should be reported as compile errors.
    #[must_use]
    pub fn conflicts(&self) -> Vec<(&ExtensionKey, &[ExtensionLocation])> {
        self.entries
            .iter()
            .filter(|(_, locs)| locs.len() > 1)
            .map(|(key, locs)| (key, locs.as_slice()))
            .collect()
    }

    /// Returns the total number of unique extension keys in the index.
    #[must_use]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns `true` if the index contains no extensions.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Builds an [`ExtensionIndex`] from multiple parsed modules.
///
/// This is the primary entry point for the extension collection pass. It
/// takes an iterator of `(file_path, module)` pairs — typically all `.bt`
/// files in the project's `beamtalk.toml` source list — and returns a
/// fully populated index.
#[must_use]
pub fn collect_extensions<'a>(
    modules: impl IntoIterator<Item = (&'a Path, &'a Module)>,
) -> ExtensionIndex {
    let mut index = ExtensionIndex::new();
    for (path, module) in modules {
        index.add_module(module, path);
    }
    index
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Identifier, MessageSelector, MethodDefinition, StandaloneMethodDefinition};
    use crate::source_analysis::Span;
    use crate::test_helpers::test_support::{parse_bt, test_span};
    use std::path::PathBuf;

    fn make_standalone(
        class_name: &str,
        selector: &str,
        is_class_method: bool,
        span: Span,
    ) -> StandaloneMethodDefinition {
        StandaloneMethodDefinition {
            class_name: Identifier::new(class_name, span),
            is_class_method,
            method: MethodDefinition::new(
                MessageSelector::Unary(selector.into()),
                Vec::new(),
                Vec::new(),
                span,
            ),
            span,
        }
    }

    #[test]
    fn empty_module_produces_empty_index() {
        let module = parse_bt("Object subclass: Empty\n  noop => 42\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("empty.bt"));
        assert!(index.is_empty());
        assert_eq!(index.len(), 0);
    }

    #[test]
    fn single_extension_is_indexed() {
        let module = parse_bt("String >> shout => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("String+Shout.bt"));

        assert_eq!(index.len(), 1);
        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "shout".into(),
        };
        let locs = index.lookup(&key).expect("should find extension");
        assert_eq!(locs.len(), 1);
        assert_eq!(locs[0].file_path, PathBuf::from("String+Shout.bt"));
    }

    #[test]
    fn class_method_extension_is_indexed_separately() {
        let module = parse_bt("String >> json => self\nString class >> fromJson => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("String+JSON.bt"));

        assert_eq!(index.len(), 2);

        let instance_key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "json".into(),
        };
        let class_key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Class,
            selector: "fromJson".into(),
        };

        assert!(index.lookup(&instance_key).is_some());
        assert!(index.lookup(&class_key).is_some());
        assert!(index.conflicts().is_empty());
    }

    #[test]
    fn duplicate_extension_detected_as_conflict() {
        let module_a = parse_bt("String >> json => self\n");
        let module_b = parse_bt("String >> json => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module_a, Path::new("String+JSON.bt"));
        index.add_module(&module_b, Path::new("String+Serialization.bt"));

        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "json".into(),
        };
        let locs = index.lookup(&key).expect("should find extension");
        assert_eq!(locs.len(), 2);

        let conflicts = index.conflicts();
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].0, &key);
    }

    #[test]
    fn different_classes_same_selector_no_conflict() {
        let module_a = parse_bt("String >> json => self\n");
        let module_b = parse_bt("Array >> json => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module_a, Path::new("String+JSON.bt"));
        index.add_module(&module_b, Path::new("Array+JSON.bt"));

        assert_eq!(index.len(), 2);
        assert!(index.conflicts().is_empty());
    }

    #[test]
    fn collect_extensions_from_multiple_modules() {
        let module_a = parse_bt("String >> shout => self\n");
        let module_b = parse_bt("Integer >> double => self\n");
        let path_a = PathBuf::from("String+Shout.bt");
        let path_b = PathBuf::from("Integer+Math.bt");

        let modules: Vec<(&Path, &Module)> =
            vec![(path_a.as_path(), &module_a), (path_b.as_path(), &module_b)];
        let index = collect_extensions(modules);

        assert_eq!(index.len(), 2);
        assert!(index.conflicts().is_empty());
    }

    #[test]
    fn keyword_selector_extension_is_indexed() {
        let module = parse_bt("Array >> chunksOf: n => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("Array+Chunking.bt"));

        assert_eq!(index.len(), 1);
        let key = ExtensionKey {
            class_name: "Array".into(),
            side: MethodSide::Instance,
            selector: "chunksOf:".into(),
        };
        assert!(index.lookup(&key).is_some());
    }

    #[test]
    fn mixed_classes_and_extensions_only_indexes_extensions() {
        let module =
            parse_bt("Object subclass: Counter\n  increment => 42\n\nString >> shout => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("mixed.bt"));

        // Only the standalone method definition should be indexed
        assert_eq!(index.len(), 1);
        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "shout".into(),
        };
        assert!(index.lookup(&key).is_some());
    }

    #[test]
    fn instance_and_class_side_same_selector_no_conflict() {
        // Per ADR 0066: instance-side and class-side are distinct
        let span = test_span();
        let mut module = parse_bt("// empty\n");
        module.method_definitions = vec![
            make_standalone("String", "json", false, span),
            make_standalone("String", "json", true, span),
        ];

        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("String+JSON.bt"));

        assert_eq!(index.len(), 2);
        assert!(
            index.conflicts().is_empty(),
            "instance and class side should not conflict"
        );
    }

    #[test]
    fn lookup_missing_key_returns_none() {
        let index = ExtensionIndex::new();
        let key = ExtensionKey {
            class_name: "Nonexistent".into(),
            side: MethodSide::Instance,
            selector: "missing".into(),
        };
        assert!(index.lookup(&key).is_none());
    }

    // --- Type info collection tests (BT-1517) ---

    #[test]
    fn type_info_unary_method_no_annotations() {
        let module = parse_bt("String >> shout => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("String+Shout.bt"));

        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "shout".into(),
        };
        let locs = index.lookup(&key).unwrap();
        assert_eq!(locs[0].type_info.arity, 0);
        assert!(locs[0].type_info.param_types.is_empty());
        assert!(
            locs[0].type_info.return_type.is_none(),
            "unannotated = Dynamic"
        );
    }

    #[test]
    fn type_info_keyword_method_with_param_types() {
        let module = parse_bt("Array >> chunksOf: n :: Integer => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("Array+Chunking.bt"));

        let key = ExtensionKey {
            class_name: "Array".into(),
            side: MethodSide::Instance,
            selector: "chunksOf:".into(),
        };
        let locs = index.lookup(&key).unwrap();
        assert_eq!(locs[0].type_info.arity, 1);
        assert_eq!(locs[0].type_info.param_types, vec![Some("Integer".into())]);
    }

    #[test]
    fn type_info_keyword_method_with_return_type() {
        let module = parse_bt("String >> reversed -> String => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("String+Reverse.bt"));

        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "reversed".into(),
        };
        let locs = index.lookup(&key).unwrap();
        assert_eq!(locs[0].type_info.arity, 0);
        assert_eq!(locs[0].type_info.return_type.as_deref(), Some("String"));
    }

    #[test]
    fn type_info_mixed_annotated_params() {
        let module = parse_bt("Map >> at: key :: String put: value => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("Map+AtPut.bt"));

        let key = ExtensionKey {
            class_name: "Map".into(),
            side: MethodSide::Instance,
            selector: "at:put:".into(),
        };
        let locs = index.lookup(&key).unwrap();
        assert_eq!(locs[0].type_info.arity, 2);
        assert_eq!(
            locs[0].type_info.param_types,
            vec![Some("String".into()), None]
        );
    }

    #[test]
    fn type_info_binary_method() {
        let module = parse_bt("Integer >> + other :: Integer -> Integer => self\n");
        let mut index = ExtensionIndex::new();
        index.add_module(&module, Path::new("Integer+Add.bt"));

        let key = ExtensionKey {
            class_name: "Integer".into(),
            side: MethodSide::Instance,
            selector: "+".into(),
        };
        let locs = index.lookup(&key).unwrap();
        assert_eq!(locs[0].type_info.arity, 1);
        assert_eq!(locs[0].type_info.param_types, vec![Some("Integer".into())]);
        assert_eq!(locs[0].type_info.return_type.as_deref(), Some("Integer"));
    }
}
