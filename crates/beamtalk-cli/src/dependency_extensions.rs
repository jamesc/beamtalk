// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The extension methods a package's dependencies contribute (ADR 0070 WS3,
//! BT-4608).
//!
//! **DDD Context:** Build System
//!
//! A dependency can extend any class — including `Object`, which every
//! receiver's chain reaches — with `Target >> selector => …`. Until the
//! consumer's type checker sees those extensions, no receiver's method
//! surface is provably complete, so ADR 0100's pre-WS3 dependency guard
//! keeps every receiver `Open` (no does-not-understand hints, no protocol
//! conformance warnings).
//!
//! Dependency extensions reach the checker the same way dependency classes,
//! protocols and aliases do: from the dependency's *source*, parsed by the
//! scan that already reads every dependency file. Both dependency scanners
//! (`build_dep_class_index` for `build`/`check`/`lint`/`test`, and
//! [`crate::dependency_classes`] for MCP lint) collect a
//! [`DependencyExtensions`] per dependency through [`DependencyExtensions::add_file`],
//! and every surface merges them with [`merge_dependency_extensions`] — one
//! rule for "complete", in one place.
//!
//! **Completeness is what lifts the guard.** A dependency's extensions are
//! complete only when every one of its source files was read and parsed
//! without errors: error recovery can drop a definition, and an unreadable
//! file hides whatever it holds. The guard lifts only when *every* dependency
//! is complete and dependency resolution itself succeeded — anything less
//! keeps it on, so a missing extension can never become a false
//! does-not-understand.

use beamtalk_core::ast::Module;
use beamtalk_core::compilation::extension_index::ExtensionIndex;
use beamtalk_core::source_analysis::{Diagnostic, Severity};
use std::path::Path;

/// The extensions one dependency's source contributes, and whether that is
/// all of them.
///
/// The [`Default`] is *unknown*: empty and incomplete. It is what a
/// dependency gets when its scan failed, so it keeps the guard on.
#[derive(Debug, Clone, Default)]
pub struct DependencyExtensions {
    index: ExtensionIndex,
    complete: bool,
}

impl DependencyExtensions {
    /// Starts a scan: empty and complete until a file says otherwise. A
    /// dependency with no `.bt` source at all stays complete — it has no
    /// extensions to contribute at runtime either.
    #[must_use]
    pub fn scanning() -> Self {
        Self {
            index: ExtensionIndex::new(),
            complete: true,
        }
    }

    /// Records one parsed source file. A file with a parse *error* makes
    /// the result incomplete, since error recovery can drop extension
    /// definitions; its surviving extensions are still recorded.
    pub fn add_file(&mut self, module: &Module, file: &Path, diagnostics: &[Diagnostic]) {
        self.index.add_module(module, file);
        if diagnostics.iter().any(|d| d.severity == Severity::Error) {
            self.complete = false;
        }
    }

    /// Records a source file that could not be read: whatever it defines is
    /// unknown.
    pub fn mark_unreadable(&mut self) {
        self.complete = false;
    }

    /// The extensions found.
    #[must_use]
    pub fn index(&self) -> &ExtensionIndex {
        &self.index
    }

    /// Whether [`Self::index`] holds every extension the dependency defines.
    #[must_use]
    pub fn is_complete(&self) -> bool {
        self.complete
    }
}

/// Merges every dependency's extensions into `into` (the consumer's
/// cross-file extension index) and returns whether all of them were
/// complete. An empty `deps` is complete; callers whose dependency
/// resolution *failed* must treat that as incomplete themselves (see
/// [`dependency_extensions_unknown`]).
pub fn merge_dependency_extensions<'a>(
    deps: impl IntoIterator<Item = &'a DependencyExtensions>,
    into: &mut ExtensionIndex,
) -> bool {
    let mut complete = true;
    for dep in deps {
        into.merge(dep.index());
        complete &= dep.is_complete();
    }
    complete
}

/// The ADR 0100 pre-WS3 guard's input
/// (`CompilerOptions::dependency_extensions_unknown`): a package with
/// dependencies keeps the guard on unless resolution succeeded and every
/// dependency's extensions were loaded completely.
#[must_use]
pub fn dependency_extensions_unknown(
    has_package_dependencies: bool,
    resolved: bool,
    all_extensions_complete: bool,
) -> bool {
    has_package_dependencies && !(resolved && all_extensions_complete)
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::source_analysis::{lex_with_eof, parse};

    fn scanned(source: &str) -> DependencyExtensions {
        let (module, diagnostics) = parse(lex_with_eof(source));
        let mut extensions = DependencyExtensions::scanning();
        extensions.add_file(&module, Path::new("dep.bt"), &diagnostics);
        extensions
    }

    #[test]
    fn default_is_unknown() {
        let unknown = DependencyExtensions::default();
        assert!(!unknown.is_complete());
        assert!(unknown.index().is_empty());
    }

    #[test]
    fn a_clean_file_is_complete_and_indexed() {
        let extensions = scanned("Object >> shout => 1\nString class >> make => 2\n");
        assert!(extensions.is_complete());
        assert_eq!(extensions.index().len(), 2);
    }

    #[test]
    fn a_parse_error_makes_it_incomplete() {
        let extensions = scanned("Object >> shout => 1\nObject >> ((( =>\n");
        assert!(!extensions.is_complete());
    }

    #[test]
    fn an_unreadable_file_makes_it_incomplete() {
        let mut extensions = DependencyExtensions::scanning();
        extensions.mark_unreadable();
        assert!(!extensions.is_complete());
    }

    #[test]
    fn merge_is_complete_only_when_every_dependency_is() {
        let complete = scanned("Object >> a => 1\n");
        let also_complete = scanned("Object >> b => 1\n");
        let mut index = ExtensionIndex::new();
        assert!(merge_dependency_extensions(
            [&complete, &also_complete],
            &mut index
        ));
        assert_eq!(index.len(), 2);

        let unknown = DependencyExtensions::default();
        let mut index = ExtensionIndex::new();
        assert!(!merge_dependency_extensions(
            [&complete, &unknown],
            &mut index
        ));
        assert_eq!(index.len(), 1, "what was seen is still merged");
    }

    #[test]
    fn guard_lifts_only_for_a_complete_successful_resolution() {
        assert!(!dependency_extensions_unknown(false, false, false));
        assert!(!dependency_extensions_unknown(true, true, true));
        assert!(dependency_extensions_unknown(true, false, true));
        assert!(dependency_extensions_unknown(true, true, false));
    }
}
