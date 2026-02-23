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

use crate::semantic_analysis::{ClassHierarchy, SemanticError};
use crate::source_analysis::{Diagnostic, lex_with_eof, parse};
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
    /// Tracks which files were loaded as stdlib sources.
    stdlib_files: HashSet<Utf8PathBuf>,
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
            stdlib_files: HashSet::new(),
        }
    }

    /// Create a `ProjectIndex` pre-indexed with stdlib classes from source files.
    ///
    /// Each `(path, source)` pair is parsed and its class definitions are
    /// merged into the project-wide hierarchy.
    ///
    /// Returns `(Result<Self, SemanticError>, Vec<Diagnostic>)` following the
    /// project convention for fallible operations, even though in practice
    /// `ClassHierarchy::build` is infallible and will always return `Ok`.
    ///
    /// # Errors
    ///
    /// Returns [`SemanticError`] if `ClassHierarchy::build` fails for any
    /// stdlib source file (cannot happen in practice).
    pub fn with_stdlib(
        stdlib_sources: &[(Utf8PathBuf, String)],
    ) -> (Result<Self, SemanticError>, Vec<Diagnostic>) {
        let mut index = Self::new();
        let mut all_diagnostics = Vec::new();
        for (path, source) in stdlib_sources {
            let tokens = lex_with_eof(source);
            let (module, _parse_diagnostics) = parse(tokens);
            let (file_hierarchy_result, hierarchy_diags) = ClassHierarchy::build(&module);
            all_diagnostics.extend(hierarchy_diags);
            let file_hierarchy = match file_hierarchy_result {
                Ok(h) => h,
                Err(e) => return (Err(e), all_diagnostics),
            };

            // Track which classes came from this stdlib file
            let class_names: Vec<EcoString> = file_hierarchy
                .class_names()
                .filter(|name| !ClassHierarchy::is_builtin_class(name))
                .cloned()
                .collect();
            index.stdlib_class_names.extend(class_names.iter().cloned());
            index.stdlib_files.insert(path.clone());
            index.file_classes.insert(path.clone(), class_names);
            index
                .file_hierarchies
                .insert(path.clone(), file_hierarchy.clone());
            index.merged_hierarchy.merge(&file_hierarchy);
        }
        (Ok(index), all_diagnostics)
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
            self.remerge_classes(&old_names);
        }

        // Track new class names from this file
        let new_names: Vec<EcoString> = hierarchy
            .class_names()
            .filter(|name| !ClassHierarchy::is_builtin_class(name))
            .cloned()
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
        if self.stdlib_files.contains(file) {
            // Stdlib files are never truly removed — preserve their hierarchy
            return;
        }
        if let Some(old_names) = self.file_classes.remove(file) {
            self.file_hierarchies.remove(file);

            // Remove all classes from this file (including stdlib overrides)
            self.merged_hierarchy.remove_classes(&old_names);

            // Re-merge classes from remaining files that shared a name
            // (this restores stdlib definitions if they were shadowed)
            self.remerge_classes(&old_names);
        }
    }

    /// Re-merge class definitions from remaining files for the given class names.
    ///
    /// When a file is removed/updated, classes it defined may also exist in
    /// other files. This method restores the first definition found, using
    /// deterministic (sorted by path) iteration order.
    fn remerge_classes(&mut self, names: &[EcoString]) {
        // Sort file paths for deterministic conflict resolution
        let mut sorted_paths: Vec<&Utf8PathBuf> = self.file_hierarchies.keys().collect();
        sorted_paths.sort();

        for name in names {
            for path in &sorted_paths {
                if let Some(info) = self.file_hierarchies[*path].classes().get(name) {
                    self.merged_hierarchy
                        .classes_mut()
                        .insert(name.clone(), info.clone());
                    break;
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
            Utf8PathBuf::from("stdlib/src/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let (index_result, _) = ProjectIndex::with_stdlib(&stdlib);
        let index = index_result.unwrap();
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
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
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
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        assert!(index.hierarchy().has_class("Foo"));

        // Second version defines Bar instead
        let tokens = lex_with_eof("Object subclass: Bar\n  baz => 2");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
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
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        assert!(index.hierarchy().has_class("Foo"));

        index.remove_file(&file);
        assert!(!index.hierarchy().has_class("Foo"));
    }

    #[test]
    fn remove_file_preserves_stdlib() {
        let stdlib = vec![(
            Utf8PathBuf::from("stdlib/src/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let mut index = ProjectIndex::with_stdlib(&stdlib).0.unwrap();
        assert!(index.hierarchy().has_class("Counter"));

        // Remove the stdlib file — Counter should persist because it's stdlib
        index.remove_file(&Utf8PathBuf::from("stdlib/src/Counter.bt"));
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
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();
        index.update_file(Utf8PathBuf::from("a.bt"), &hierarchy_a);

        let tokens_b = lex_with_eof("Object subclass: ClassB\n  methodB => 2");
        let (module_b, _) = parse(tokens_b);
        let hierarchy_b = ClassHierarchy::build(&module_b).0.unwrap();
        index.update_file(Utf8PathBuf::from("b.bt"), &hierarchy_b);

        // Both classes visible in merged hierarchy
        assert!(index.hierarchy().has_class("ClassA"));
        assert!(index.hierarchy().has_class("ClassB"));
    }

    #[test]
    fn remerge_deterministic_on_conflict() {
        // Two files define the same class name with different methods.
        // After removing one, the surviving definition should be deterministic
        // (alphabetically first file wins).
        let mut index = ProjectIndex::new();

        // a.bt defines Dup with methodA
        let tokens_a = lex_with_eof("Object subclass: Dup\n  methodA => 1");
        let (module_a, _) = parse(tokens_a);
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();
        index.update_file(Utf8PathBuf::from("a.bt"), &hierarchy_a);

        // b.bt defines Dup with methodB
        let tokens_b = lex_with_eof("Object subclass: Dup\n  methodB => 2");
        let (module_b, _) = parse(tokens_b);
        let hierarchy_b = ClassHierarchy::build(&module_b).0.unwrap();
        index.update_file(Utf8PathBuf::from("b.bt"), &hierarchy_b);

        // b.bt was last to merge, so Dup currently has methodB
        let class = index.hierarchy().get_class("Dup").unwrap();
        assert!(class.methods.iter().any(|m| m.selector == "methodB"));

        // Remove b.bt — a.bt's definition should survive (deterministic)
        index.remove_file(&Utf8PathBuf::from("b.bt"));
        assert!(index.hierarchy().has_class("Dup"));
        let class = index.hierarchy().get_class("Dup").unwrap();
        assert!(class.methods.iter().any(|m| m.selector == "methodA"));
    }

    #[test]
    fn stdlib_restored_after_user_override_removed() {
        // Stdlib defines Counter with increment.
        // User file shadows Counter with decrement.
        // Removing the user file should restore the stdlib Counter.
        let stdlib = vec![(
            Utf8PathBuf::from("stdlib/src/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let mut index = ProjectIndex::with_stdlib(&stdlib).0.unwrap();
        assert!(index.hierarchy().has_class("Counter"));

        // User file shadows stdlib Counter
        let user_file = Utf8PathBuf::from("user/counter.bt");
        let tokens = lex_with_eof("Object subclass: Counter\n  decrement => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(user_file.clone(), &hierarchy);

        // User's Counter (with decrement) is now the active definition
        let class = index.hierarchy().get_class("Counter").unwrap();
        assert!(class.methods.iter().any(|m| m.selector == "decrement"));

        // Remove the user file — stdlib Counter should be restored
        index.remove_file(&user_file);
        assert!(index.hierarchy().has_class("Counter"));
        let class = index.hierarchy().get_class("Counter").unwrap();
        assert!(class.methods.iter().any(|m| m.selector == "increment"));
    }
}
