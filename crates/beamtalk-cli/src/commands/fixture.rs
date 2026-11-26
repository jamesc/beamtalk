// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The one compiler for fixture files, shared by `beamtalk test` and
//! `just test-stdlib` (`test_stdlib`, `test_docs`, `test_metamorphic`).
//!
//! A fixture is a `.bt` source compiled to `bt@{stem}` (ADR 0026) before the
//! tests that use it run. The two commands differ only in the compile
//! environment, which [`FixtureScope`] names; module naming, the
//! `CompilerOptions` / `CompileContext` shaping, the `compile_source_with_bindings`
//! call and the `.core` → `.beam` batch are written once here.
//!
//! **DDD Context:** Compilation (CLI surface for test fixtures).

use beamtalk_core::semantic_analysis::alias_registry::AliasInfo;
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use beamtalk_core::semantic_analysis::{ModuleRootKind, module_name_for_path};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, Result};
use std::sync::Arc;

use crate::beam_compiler::{
    BeamCompiler, ClassHierarchyContext, CompileContext, compile_source_with_bindings,
};

/// The compile environment a fixture (or test file) is analysed in.
pub(crate) enum FixtureScope<'a> {
    /// `beamtalk test`: the file belongs to a package (or to none), and is
    /// analysed with the class hierarchy the test pipeline assembled from the
    /// package, its dependencies and the fixtures. The checker scope comes from
    /// the file's own manifest, as for `beamtalk build` (BT-4334).
    Package {
        hierarchy: &'a ClassHierarchyContext,
        current_package: Option<&'a str>,
        known_erlang_modules: &'a beamtalk_core::ffi_type_specs::KnownErlangModules,
        /// `--no-warnings`.
        suppress_warnings: bool,
    },
    /// `test-stdlib` / `test-docs` / the metamorphic harness: bootstrap fixtures
    /// with no manifest, no package and no dependencies. Module-expression lint
    /// is skipped (fixtures pair top-level expressions with `// =>` comments)
    /// and only the installed OTP modules count as known (BT-4724).
    Bootstrap {
        /// Type aliases from a sibling `src/` directory (BT-3563).
        pre_loaded_aliases: &'a [AliasInfo],
        suppress_warnings: bool,
    },
}

/// Everything [`compile_fixture`] / [`generate_core_file`] need besides the
/// source path.
pub(crate) struct FixtureCompiler<'a> {
    pub(crate) output_dir: &'a Utf8Path,
    pub(crate) warnings_as_errors: bool,
    /// Stop after writing `.core` (BT-3887 snapshot mode): the `.beam` is only
    /// ever used at test execution, which never happens there.
    pub(crate) snapshot_only: bool,
    /// ADR 0075 FFI typing registry; without it every `(Erlang mod) fn:` call
    /// type-checks as `Dynamic`.
    pub(crate) native_type_registry: Option<&'a Arc<NativeTypeRegistry>>,
    pub(crate) scope: FixtureScope<'a>,
}

/// The BEAM module name of a fixture file (`counter.bt` → `bt@counter`),
/// through the one shared path-to-module rule (`module_name_for_path`,
/// out-of-tree root, ADR 0026), so a fixture whose name `beamtalk build` or the
/// REPL would reject (`my-helper.bt`) is rejected here too.
pub(crate) fn fixture_module_name(fixture_path: &Utf8Path) -> Result<String> {
    module_name_for_path(fixture_path, "", ModuleRootKind::OutOfTree)
        .map_err(|e| miette::miette!("Cannot name fixture '{fixture_path}': {e}"))
}

/// The checker scope for a test/fixture file (BT-4334): by the shared
/// [`manifest::package_checker_scope`](beamtalk_package::manifest::package_checker_scope)
/// rule, a file inside a package is analysed with project-complete knowledge, a
/// manifest-less file keeps the conservative `ModuleOnly` default - the same split `build` makes.
pub(crate) fn test_compile_scope(
    source_path: &Utf8Path,
) -> beamtalk_package::manifest::PackageCheckerScope {
    beamtalk_package::manifest::package_checker_scope(
        beamtalk_package::lint_pipeline::find_package_root(source_path).as_deref(),
    )
}

impl<'a> FixtureCompiler<'a> {
    /// The compiler for bootstrap fixtures (`test-stdlib`, `test-docs`, the
    /// metamorphic harness), which always build `.beam` files.
    pub(crate) fn bootstrap(
        output_dir: &'a Utf8Path,
        suppress_warnings: bool,
        warnings_as_errors: bool,
        pre_loaded_aliases: &'a [AliasInfo],
        native_type_registry: Option<&'a Arc<NativeTypeRegistry>>,
    ) -> Self {
        Self {
            output_dir,
            warnings_as_errors,
            snapshot_only: false,
            native_type_registry,
            scope: FixtureScope::Bootstrap {
                pre_loaded_aliases,
                suppress_warnings,
            },
        }
    }
}

impl FixtureCompiler<'_> {
    /// Compile `source_path` to `{output_dir}/{module_name}.core` (no BEAM step,
    /// so callers can batch several files into one `erlc` start-up).
    pub(crate) fn generate_core_file(
        &self,
        source_path: &Utf8Path,
        module_name: &str,
    ) -> Result<Utf8PathBuf> {
        let core_file = self.output_dir.join(format!("{module_name}.core"));
        let (options, hierarchy) = match &self.scope {
            FixtureScope::Package {
                hierarchy,
                current_package,
                known_erlang_modules,
                suppress_warnings,
            } => {
                // BT-4334: declare the checker scope `build` does. Without
                // `ProjectComplete`, cross-file receivers stay `Open` and their
                // DNU/Type diagnostics are suppressed, so `test
                // --warnings-as-errors` would pass code the REPL rejects.
                // `current_package: Some(_)` makes the E0401/E0402 visibility
                // checks fire.
                let scope = test_compile_scope(source_path);
                let options = beamtalk_core::CompilerOptions {
                    stdlib_mode: false,
                    allow_primitives: false,
                    warnings_as_errors: self.warnings_as_errors,
                    suppress_warnings: *suppress_warnings,
                    current_package: current_package.map(str::to_string),
                    knowledge_scope: scope.knowledge_scope,
                    // ADR 0070 Phase 2: `pkg@Class` must name a declared
                    // dependency, the package itself or the stdlib.
                    known_packages: scope.known_packages.clone(),
                    // BT-4022: project-known Erlang modules so `unresolved_ffi`
                    // agrees with `build`/`lint` on hex-dep and own-`native/`.
                    known_erlang_modules: (*known_erlang_modules).clone(),
                    ..Default::default()
                };
                (options, (*hierarchy).clone())
            }
            FixtureScope::Bootstrap {
                pre_loaded_aliases,
                suppress_warnings,
            } => {
                let options = beamtalk_core::CompilerOptions {
                    stdlib_mode: false,
                    allow_primitives: false,
                    suppress_warnings: *suppress_warnings,
                    // Bootstrap fixtures use top-level expressions as test
                    // assertions paired with `// =>` comments; skip the
                    // module-expression lint to avoid false positives.
                    skip_module_expression_lint: true,
                    warnings_as_errors: self.warnings_as_errors,
                    current_package: None,
                    knowledge_scope: beamtalk_core::semantic_analysis::KnowledgeScope::default(),
                    // No manifest, so no package set to validate against.
                    known_packages: None,
                    // No project on disk whose dependency/native modules could
                    // be collected (BT-4022), but the installed OTP applies.
                    known_erlang_modules:
                        beamtalk_core::ffi_type_specs::collect_installed_erlang_modules(),
                };
                let hierarchy = ClassHierarchyContext {
                    pre_loaded_aliases: pre_loaded_aliases.to_vec(),
                    ..Default::default()
                };
                (options, hierarchy)
            }
        };
        let ctx = CompileContext {
            hierarchy,
            native_type_registry: self.native_type_registry.cloned(),
            ..CompileContext::default()
        };
        compile_source_with_bindings(
            source_path,
            module_name,
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &ctx,
            None,
        )
        .wrap_err_with(|| format!("Failed to compile '{source_path}'"))?;
        Ok(core_file)
    }

    /// Compile one fixture to `.core` and (unless `snapshot_only`) `.beam`;
    /// returns its module name so callers need not recompute it.
    pub(crate) fn compile_fixture(&self, fixture_path: &Utf8Path) -> Result<String> {
        let module_name = fixture_module_name(fixture_path)?;
        let core_file = self
            .generate_core_file(fixture_path, &module_name)
            .wrap_err_with(|| format!("Failed to compile fixture '{fixture_path}'"))?;
        if !self.snapshot_only {
            BeamCompiler::new(self.output_dir.to_owned())
                .compile_batch(&[core_file])
                .wrap_err_with(|| format!("Failed to compile fixture BEAM for '{fixture_path}'"))?;
        }
        Ok(module_name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temp_dir() -> (tempfile::TempDir, Utf8PathBuf) {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        (temp, dir)
    }

    /// Both commands name a fixture through the one path-to-module rule.
    #[test]
    fn fixture_module_name_is_bt_stem_and_rejects_invalid_segments() {
        assert_eq!(
            fixture_module_name(Utf8Path::new("a/b/counter.bt")).unwrap(),
            "bt@counter"
        );
        assert!(fixture_module_name(Utf8Path::new("a/my-helper.bt")).is_err());
    }

    /// A bootstrap fixture in snapshot mode writes its `.core` and returns the
    /// module name without starting `erlc`.
    #[test]
    fn bootstrap_fixture_snapshot_writes_core_only() {
        let (_temp, dir) = temp_dir();
        let source = dir.join("probe.bt");
        std::fs::write(&source, "Object subclass: Probe\n  answer => 42\n").unwrap();
        let out = dir.join("out");
        std::fs::create_dir_all(&out).unwrap();
        let mut compiler = FixtureCompiler::bootstrap(&out, true, false, &[], None);
        compiler.snapshot_only = true;
        assert_eq!(compiler.compile_fixture(&source).unwrap(), "bt@probe");
        assert!(out.join("bt@probe.core").is_file());
        assert!(!out.join("bt@probe.beam").exists());
    }
}
