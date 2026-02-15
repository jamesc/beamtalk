// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-file project index for the language service.
//!
//! **DDD Context:** Language Service — Aggregate Root
//!
//! `ProjectIndex` manages a merged [`ClassHierarchy`] across all open files,
//! pre-indexed with stdlib classes from `lib/*.bt`. It supports incremental
//! updates (add/remove/update file) and provides the foundation for cross-file
//! go-to-definition, find-references, and class-aware completions.
//!
//! # Architecture (ADR 0024, Phase 1)
//!
//! ```text
//! ProjectIndex (Aggregate Root)
//! ├── merged ClassHierarchy (builtins + stdlib + all open files)
//! ├── per-file class names (for incremental updates)
//! └── stdlib class names (pre-indexed from lib/*.bt)
//! ```

use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::{lex_with_eof, parse};
use camino::Utf8PathBuf;
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

/// Cross-file project index holding a merged class hierarchy.
///
/// **DDD Context:** Language Service — Aggregate Root
///
/// The `ProjectIndex` aggregates class information from all open files
/// and the stdlib, providing a project-wide view used by providers
/// (definition, completion, diagnostics).
#[derive(Debug, Clone)]
pub struct ProjectIndex {
    /// Merged class hierarchy (builtins + stdlib + all open files).
    merged_hierarchy: ClassHierarchy,
    /// Per-file tracking of which class names came from which file.
    file_classes: HashMap<Utf8PathBuf, Vec<EcoString>>,
    /// Per-file class hierarchies for correct rebuild on conflicts.
    file_hierarchies: HashMap<Utf8PathBuf, ClassHierarchy>,
    /// Class names from stdlib (never removed during file updates).
    stdlib_class_names: HashSet<EcoString>,
}

impl ProjectIndex {
    /// Create a new empty `ProjectIndex` with only built-in classes.
    #[must_use]
    pub fn new() -> Self {
        Self {
            merged_hierarchy: ClassHierarchy::with_builtins(),
            file_classes: HashMap::new(),
            file_hierarchies: HashMap::new(),
            stdlib_class_names: HashSet::new(),
        }
    }

    /// Create a `ProjectIndex` pre-indexed with stdlib classes from source files.
    ///
    /// Each `(path, source)` pair is parsed and its class definitions are
    /// merged into the project-wide hierarchy.
    #[must_use]
    pub fn with_stdlib(stdlib_sources: &[(Utf8PathBuf, String)]) -> Self {
        let mut index = Self::new();
        for (path, source) in stdlib_sources {
            let tokens = lex_with_eof(source);
            let (module, _diagnostics) = parse(tokens);
            let file_hierarchy = ClassHierarchy::build(&module).0;

            // Track which classes came from this stdlib file
            let class_names: Vec<EcoString> = file_hierarchy
                .class_names()
                .into_iter()
                .filter(|name| !ClassHierarchy::is_builtin_class(name))
                .collect();
            index.stdlib_class_names.extend(class_names.iter().cloned());
            index.file_classes.insert(path.clone(), class_names);
            index
                .file_hierarchies
                .insert(path.clone(), file_hierarchy.clone());
            index.merged_hierarchy.merge(&file_hierarchy);
        }
        index
    }

    /// Returns the merged class hierarchy across all files.
    #[must_use]
    pub fn hierarchy(&self) -> &ClassHierarchy {
        &self.merged_hierarchy
    }

    /// Add or update a file in the index.
    ///
    /// If the file was previously indexed, its old class definitions are
    /// removed and then re-merged from remaining files to handle conflicts
    /// correctly (two files defining the same class name).
    pub fn update_file(&mut self, file: Utf8PathBuf, hierarchy: &ClassHierarchy) {
        // Remove old classes from this file (if any)
        if let Some(old_names) = self.file_classes.remove(&file) {
            self.merged_hierarchy.remove_classes(&old_names);
            self.file_hierarchies.remove(&file);

            // Re-merge classes from other files that may have been shadowed
            for name in &old_names {
                for other_hierarchy in self.file_hierarchies.values() {
                    if let Some(info) = other_hierarchy.classes().get(name) {
                        self.merged_hierarchy
                            .classes_mut()
                            .insert(name.clone(), info.clone());
                        break;
                    }
                }
            }
        }

        // Track new class names from this file
        let new_names: Vec<EcoString> = hierarchy
            .class_names()
            .into_iter()
            .filter(|name| !ClassHierarchy::is_builtin_class(name))
            .collect();

        self.file_classes.insert(file.clone(), new_names);
        self.file_hierarchies.insert(file, hierarchy.clone());
        self.merged_hierarchy.merge(hierarchy);
    }

    /// Remove a file from the index.
    ///
    /// Classes defined only in this file are removed from the merged hierarchy.
    /// Classes also defined in other files are preserved (re-merged).
    pub fn remove_file(&mut self, file: &Utf8PathBuf) {
        if let Some(old_names) = self.file_classes.remove(file) {
            self.file_hierarchies.remove(file);
            // Only remove non-stdlib classes
            let to_remove: Vec<EcoString> = old_names
                .iter()
                .filter(|n| !self.stdlib_class_names.contains(n.as_str()))
                .cloned()
                .collect();
            self.merged_hierarchy.remove_classes(&to_remove);

            // Re-merge classes from remaining files that shared a name
            for name in &to_remove {
                for other_hierarchy in self.file_hierarchies.values() {
                    if let Some(info) = other_hierarchy.classes().get(name) {
                        self.merged_hierarchy
                            .classes_mut()
                            .insert(name.clone(), info.clone());
                        break;
                    }
                }
            }
        }
    }

    /// Returns the set of files currently indexed.
    #[must_use]
    pub fn indexed_files(&self) -> Vec<&Utf8PathBuf> {
        self.file_classes.keys().collect()
    }

    /// Returns the class names contributed by a specific file.
    #[must_use]
    pub fn classes_in_file(&self, file: &Utf8PathBuf) -> Option<&[EcoString]> {
        self.file_classes.get(file).map(Vec::as_slice)
    }
}

impl Default for ProjectIndex {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_has_only_builtins() {
        let index = ProjectIndex::new();
        // Should have built-in classes (Object, Integer, etc.)
        assert!(index.hierarchy().has_class("Object"));
        assert!(index.hierarchy().has_class("Integer"));
        assert!(index.file_classes.is_empty());
    }

    #[test]
    fn with_stdlib_indexes_classes() {
        let stdlib = vec![(
            Utf8PathBuf::from("lib/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let index = ProjectIndex::with_stdlib(&stdlib);
        assert!(index.hierarchy().has_class("Counter"));
        assert!(
            index
                .stdlib_class_names
                .contains(&EcoString::from("Counter"))
        );
    }

    #[test]
    fn update_file_adds_classes() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("test.bt");

        let tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0;
        index.update_file(file.clone(), &hierarchy);

        assert!(index.hierarchy().has_class("Foo"));
        assert_eq!(
            index.classes_in_file(&file).unwrap(),
            &[EcoString::from("Foo")]
        );
    }

    #[test]
    fn update_file_replaces_old_classes() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("test.bt");

        // First version defines Foo
        let tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0;
        index.update_file(file.clone(), &hierarchy);
        assert!(index.hierarchy().has_class("Foo"));

        // Second version defines Bar instead
        let tokens = lex_with_eof("Object subclass: Bar\n  baz => 2");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0;
        index.update_file(file.clone(), &hierarchy);
        assert!(index.hierarchy().has_class("Bar"));
        assert!(!index.hierarchy().has_class("Foo"));
    }

    #[test]
    fn remove_file_removes_classes() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("test.bt");

        let tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0;
        index.update_file(file.clone(), &hierarchy);
        assert!(index.hierarchy().has_class("Foo"));

        index.remove_file(&file);
        assert!(!index.hierarchy().has_class("Foo"));
    }

    #[test]
    fn remove_file_preserves_stdlib() {
        let stdlib = vec![(
            Utf8PathBuf::from("lib/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let mut index = ProjectIndex::with_stdlib(&stdlib);
        assert!(index.hierarchy().has_class("Counter"));

        // Remove the stdlib file — Counter should persist because it's stdlib
        index.remove_file(&Utf8PathBuf::from("lib/Counter.bt"));
        // Stdlib classes are tracked separately
        assert!(
            index
                .stdlib_class_names
                .contains(&EcoString::from("Counter"))
        );
    }

    #[test]
    fn cross_file_classes_visible() {
        let mut index = ProjectIndex::new();

        let tokens_a = lex_with_eof("Object subclass: ClassA\n  methodA => 1");
        let (module_a, _) = parse(tokens_a);
        let hierarchy_a = ClassHierarchy::build(&module_a).0;
        index.update_file(Utf8PathBuf::from("a.bt"), &hierarchy_a);

        let tokens_b = lex_with_eof("Object subclass: ClassB\n  methodB => 2");
        let (module_b, _) = parse(tokens_b);
        let hierarchy_b = ClassHierarchy::build(&module_b).0;
        index.update_file(Utf8PathBuf::from("b.bt"), &hierarchy_b);

        // Both classes visible in merged hierarchy
        assert!(index.hierarchy().has_class("ClassA"));
        assert!(index.hierarchy().has_class("ClassB"));
    }
}
