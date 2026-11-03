// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Loading `declare native:` FFI type stubs (ADR 0075).
//!
//! **DDD Context:** Compilation
//!
//! Every stub tier of ADR 0075's resolution chain — distribution,
//! package-bundled, project-local — is a directory of `.bt` stub files merged
//! into a [`NativeTypeRegistry`]. This module is that merge, shared by
//! `beamtalk build`/`lint`/`test` (which wrap it to render each file's
//! diagnostics) and the compiler port's project scope (BT-4378), so a
//! package's FFI calls type the same on the REPL `load_project` path as in a
//! build.

use camino::{Utf8Path, Utf8PathBuf};
use tracing::warn;

use crate::file_walker::FileWalker;
use crate::semantic_analysis::type_checker::{
    NativeTypeRegistry, TypeProvenance, load_native_declarations,
};
use crate::source_analysis::{Diagnostic, Span};

/// The project-local stub directory (ADR 0075 layer 1), relative to a
/// package root. Auto-discovered: no `beamtalk.toml` configuration.
pub const PROJECT_STUBS_DIR: &str = "stubs";

/// One stub file's contribution to a [`StubRegistryLoad`]: its source (so a
/// caller can render diagnostics against it) and its own diagnostics.
#[derive(Debug)]
pub struct StubFileLoad {
    /// The stub file.
    pub path: Utf8PathBuf,
    /// The file's source text.
    pub source: String,
    /// Parse diagnostics, skipped-signature warnings, and version-drift
    /// warnings against the caller's auto-extracted registry.
    pub diagnostics: Vec<Diagnostic>,
}

/// A stub directory merged into one registry.
#[derive(Debug)]
pub struct StubRegistryLoad {
    /// Every file's declarations, later files winning per function/arity.
    pub registry: NativeTypeRegistry,
    /// Each readable stub file, in merge order.
    pub files: Vec<StubFileLoad>,
}

impl StubRegistryLoad {
    /// The merged registry plus every file's diagnostics, in merge order.
    #[must_use]
    pub fn into_parts(self) -> (NativeTypeRegistry, Vec<Diagnostic>) {
        let diagnostics = self
            .files
            .into_iter()
            .flat_map(|file| file.diagnostics)
            .collect();
        (self.registry, diagnostics)
    }
}

/// Parse and merge every `declare native:` stub file under `stubs_dir` into a
/// single [`NativeTypeRegistry`], checking each file against `auto_extract`
/// for version drift.
///
/// Pass `auto_extract` as the *pre-merge* auto-extracted registry — the drift
/// check ([`NativeTypeRegistry::detect_stub_drift`]) needs it as ground truth
/// for what a `.beam` module actually exports, which only holds before stub
/// overrides are applied on top of it. `None` skips the drift check.
///
/// Returns `None` if `stubs_dir` does not exist, cannot be read, or contains
/// no `.bt` files, so callers can tell "nothing to merge" apart from "stubs
/// exist but declare nothing". An unreadable file is skipped with a warning.
#[must_use]
pub fn load_stub_registry_from_dir(
    stubs_dir: &Utf8Path,
    auto_extract: Option<&NativeTypeRegistry>,
) -> Option<StubRegistryLoad> {
    if !stubs_dir.exists() {
        return None;
    }
    let stub_files = match FileWalker::source_files().walk(stubs_dir) {
        Ok(files) => files,
        Err(e) => {
            warn!(dir = %stubs_dir, error = %e, "Cannot read stubs/ directory; skipping stub resolution");
            return None;
        }
    };
    if stub_files.is_empty() {
        return None;
    }

    let mut registry = NativeTypeRegistry::new();
    let mut files = Vec::new();

    for file in stub_files {
        let source = match std::fs::read_to_string(&file) {
            Ok(s) => s,
            Err(e) => {
                warn!(file = %file, error = %e, "Cannot read stub file; skipping");
                continue;
            }
        };
        let tokens = crate::source_analysis::lex_with_eof(&source);
        let (module, mut diagnostics) = crate::source_analysis::parse(tokens);

        // This file's own declarations, checked for drift against
        // `auto_extract` *before* merging into the accumulating `registry` —
        // scoping the check to one file's functions lets each warning render
        // against that file's own source, rather than losing track of which
        // stub file a merged function came from.
        let mut file_registry = NativeTypeRegistry::new();
        diagnostics.extend(load_native_declarations(
            &module.native_declarations,
            &mut file_registry,
        ));
        if let Some(auto_extract) = auto_extract {
            for (module_name, sig) in auto_extract.detect_stub_drift(&file_registry) {
                let span = match sig.provenance {
                    TypeProvenance::Declared(s) => s,
                    _ => Span::default(),
                };
                diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Stub declares `{module_name}:{}/{}`, which was not found in the \
                             compiled `{module_name}.beam` exports — the stub may be out of date",
                            sig.name, sig.arity
                        ),
                        span,
                    )
                    .with_hint(
                        "Regenerate this stub with `beamtalk generate stubs`, or remove the \
                         stale declaration.",
                    ),
                );
            }
        }
        // Per-function upsert (not `merge`'s whole-module keep-on-collision):
        // two stub files declaring the same Erlang module must combine, with
        // the later file's function/arity winning on overlap.
        registry.apply_overrides(file_registry);

        files.push(StubFileLoad {
            path: file,
            source,
            diagnostics,
        });
    }

    Some(StubRegistryLoad { registry, files })
}

/// The project-local stub tier: [`load_stub_registry_from_dir`] over
/// `project_root/stubs/` ([`PROJECT_STUBS_DIR`]).
///
/// The one place both `beamtalk build` and the compiler port's project scope
/// locate a package's own stubs, so they agree on which files type its FFI.
#[must_use]
pub fn load_project_stub_registry(
    project_root: &Utf8Path,
    auto_extract: Option<&NativeTypeRegistry>,
) -> Option<StubRegistryLoad> {
    load_stub_registry_from_dir(&project_root.join(PROJECT_STUBS_DIR), auto_extract)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn missing_or_empty_stub_dir_is_none() {
        let dir = tempfile::tempdir().unwrap();
        let root = Utf8Path::from_path(dir.path()).unwrap();
        assert!(load_project_stub_registry(root, None).is_none());
        std::fs::create_dir(root.join(PROJECT_STUBS_DIR)).unwrap();
        assert!(load_project_stub_registry(root, None).is_none());
    }

    #[test]
    fn project_stubs_merge_into_one_registry() {
        let dir = tempfile::tempdir().unwrap();
        let root = Utf8Path::from_path(dir.path()).unwrap();
        let stubs = root.join(PROJECT_STUBS_DIR);
        std::fs::create_dir(&stubs).unwrap();
        std::fs::write(
            stubs.join("my_native.bt"),
            "declare native: my_native\n  ping -> Integer\n",
        )
        .unwrap();

        let load = load_project_stub_registry(root, None).expect("stubs/ has a stub file");
        assert_eq!(load.files.len(), 1);
        assert!(
            load.registry.lookup("my_native", "ping", 0).is_some(),
            "stub declaration must land in the registry: {:?}",
            load.files[0].diagnostics
        );
    }
}
