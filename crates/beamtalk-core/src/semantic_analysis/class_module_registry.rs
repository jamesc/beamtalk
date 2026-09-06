// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The single source of truth for "what compiled Erlang module is class X in?"
//!
//! **DDD Context:** Semantic Analysis
//!
//! ADR 0119 (BT-3435, Phase 1 of 3): today this question is independently
//! re-derived — or guessed — in at least seven places across
//! `beamtalk-cli`/`beamtalk-codegen`/`beamtalk-repl`/`beamtalk-compiler-port`,
//! each added ad hoc as a new codegen need arose. That duplication already
//! shipped three real bugs (BT-3081, BT-3431, BT-3432). This module is the
//! one authority those call sites converge on, extending ADR 0089's
//! typed-leaf discipline to this leaf kind: [`ModuleName`] wraps a resolved
//! module name instead of every caller carrying its own ad hoc `String`.
//!
//! **Phase 1 scope (this module):** the data structure and its construction
//! primitives only — nothing in the compiler consumed this registry when it
//! was added. **Phase 2 (BT-3436)** wired the actual consumers up to it:
//! `beamtalk-codegen`'s `compiled_module_name`/`compiled_module_name_qualified`
//! query it (falling back to the best-effort convention only on a genuine
//! miss), `module_matches_class` was deleted in favor of a direct
//! `CoreErlangGenerator::current_class` identity check (ADR 0040: no
//! module-name comparison needed), and `beamtalk-compiler-port`'s
//! `derive_class_module_name` mints names via [`ClassModuleRegistry::assign`]
//! instead of a closed-form `format!`. `beamtalk-repl` and the REPL/compiler-
//! port wire format (`class_module_index`, ADR 0050) are unchanged — see
//! `beamtalk-repl/src/codegen.rs`'s doc comments for how that flows through
//! the registry downstream instead of an independent resolution path.
//!
//! See `docs/ADR/0119-class-module-name-resolution-registry.md` for the full
//! design rationale, prior art, and the open questions left for later phases.

use crate::ast::to_module_name;
use camino::Utf8Path;
use std::collections::HashMap;

/// A validated, compiled module name — either a generated `bt@...` module or
/// a hand-written native backing module (ADR 0056), e.g. `beamtalk_future`.
///
/// Constructed by resolving a class through a [`ClassModuleRegistry`], or
/// reconstructed at a trust boundary (the Pass-1 build cache, the
/// compiler-port wire format) that is itself backed by a registry build.
/// Per ADR 0119's Consequences/Negative: this is a real but partial safety
/// net, not a compiler-enforced wall — `beamtalk-cerl-doc`'s `leaf::atom`
/// still accepts any string, and those trust boundaries reconstruct a
/// `ModuleName` from a plain string outside the registry's control. The
/// newtype's value is narrower but real: the handful of functions that
/// *resolve* a class to a module return one typed value instead of five ad
/// hoc `String`s.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleName {
    /// A compiler-generated module, e.g. `"bt@stdlib@ordered_collection"` or
    /// `"bt@sicp@scheme@env"`.
    Generated(String),
    /// A hand-written native backing module (ADR 0016's `beamtalk_*`
    /// underscore namespace reserved for native runtime code), e.g.
    /// `"beamtalk_future"` for a builtin with no `stdlib/src/*.bt` source.
    ///
    /// BT-3435 (ADR 0119 Phase 1): the variant exists so `ModuleName` can
    /// represent this case, but nothing seeds or resolves one yet — no
    /// current call site actually resolves such a class's module at all (see
    /// `compute_direct_call_eligible`'s gate 3 in `beamtalk-codegen`), so
    /// building seeding/lookup logic for it now would be speculative code
    /// for a path nothing reaches. Revisit if/when a no-`.bt`-source builtin
    /// gains a real call site that needs its module resolved.
    Native(String),
}

impl ModuleName {
    /// The Erlang atom text this module name renders as, with no quoting or
    /// leaf-escaping applied — callers emitting Core Erlang must still route
    /// this through `leaf::atom` (ADR 0089), never through `format!`/string
    /// concatenation.
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            ModuleName::Generated(name) | ModuleName::Native(name) => name,
        }
    }
}

/// Identifies which package a class was declared in, so two classes with the
/// same bare name (a user package's `Set` shadowing stdlib's) do not collide
/// in the registry.
///
/// ADR 0026 currently forbids that exact case outright (a compile error),
/// but the key still carries package identity today so the registry does not
/// have to be re-keyed if that restriction is ever lifted.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PackageId {
    /// The stdlib package (`stdlib/src/*.bt`, compiled to `bt@stdlib@{snake}`).
    Stdlib,
    /// A named user package (ADR 0026), compiled to `bt@{name}@{snake}`.
    Package(String),
    /// A single `.bt` file with no package manifest, compiled to `bt@{snake}`.
    SingleFile,
}

/// How a compilation unit assigns module names to source files it has not
/// seen yet — used by [`ClassModuleRegistry::assign`] to mint a name for a
/// class with no `.bt` source file (a REPL `subclass:`, or a hot-reloaded
/// inline class definition arriving over the compiler-port). Not used for
/// classes discovered from real files — those already know their file path
/// and, for package mode, its position under the source root (see
/// [`relative_module_segments`]).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleNamingScheme {
    /// `bt@{snake(class_name)}` — matches `beamtalk build <file>.bt`.
    SingleFile,
    /// `bt@{name}@{snake(class_name)}` — matches `beamtalk build` in a
    /// package with a `beamtalk.toml` manifest. A class *assigned* under this
    /// scheme has no source file yet, so it is always minted at the
    /// package's root; a real package source file with a subdirectory
    /// position uses [`relative_module_segments`] directly instead.
    Package {
        /// The package name from `beamtalk.toml`.
        name: String,
    },
    /// `bt@stdlib@{snake(class_name)}` — closed-form, not a free lookup: the
    /// Erlang runtime derives this same prefix with no registry to consult
    /// (ADR 0119 Context), so the compiler's stdlib naming can never become
    /// path-based or otherwise diverge from this rule.
    Stdlib,
}

/// One resolved (package, class, module) fact — the smallest unit the
/// incremental Pass-1 build cache can store and merge, one per class
/// declaration discovered in a single source file. `beamtalk-cli`'s
/// `.beamtalk-pass1-cache.json` construction generalizes around entries
/// shaped like this rather than a monolithic re-parse-everything call, so an
/// unchanged file's classes can be restored from cache without re-deriving
/// module names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryEntry {
    /// The package the class was declared in.
    pub package: PackageId,
    /// The Beamtalk class name, e.g. `"OrderedCollection"`.
    pub class_name: String,
    /// The resolved compiled module.
    pub module: ModuleName,
}

/// Error computing or validating a module name.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ClassModuleRegistryError {
    /// A package-relative path segment contained characters other than
    /// ASCII alphanumerics and underscores (mirrors `to_module_name`'s
    /// input contract).
    #[error(
        "invalid directory name '{segment}' in source path '{path}': must contain only \
         alphanumeric characters and underscores"
    )]
    InvalidPathSegment {
        /// The offending path segment.
        segment: String,
        /// The full path it was found in.
        path: String,
    },
    /// A stdlib class's path-derived module name (from its file stem)
    /// disagrees with the closed-form name computed from its real, parsed
    /// class name — the exact BT-3432 bug shape (a file renamed without
    /// renaming, or vice versa, the class it declares).
    #[error(
        "stdlib class '{class_name}' is declared in a file whose derived module name \
         ('{path_derived}') disagrees with the closed-form name computed from the class's own \
         name ('{name_derived}') — the file name and class name must agree (ADR 0016)"
    )]
    StdlibNameMismatch {
        /// The real, parsed class name.
        class_name: String,
        /// The module name derived from the file's path/stem.
        path_derived: String,
        /// The module name computed from the class name via the closed-form
        /// `bt@stdlib@{snake}` rule.
        name_derived: String,
    },
}

/// Splits a package-relative source path into `@`-joined, `snake_cased`
/// segments, e.g. `scheme/Env.bt` under source root `src/` becomes
/// `["scheme", "env"]` (joined by callers as `"scheme@env"`).
///
/// Generalizes the segment-validation and per-segment `to_module_name`
/// conversion at the heart of `beamtalk-cli`'s `build::compute_relative_module`
/// so package-mode Pass-1 construction and any other caller share exactly
/// one implementation of "how a file path becomes module-name segments"
/// (CLAUDE.md's "No duplicate implementations" rule) — `compute_relative_module`
/// itself now delegates here instead of re-implementing the loop.
///
/// Returns one segment per path component with the extension stripped from
/// the final component. Every segment must contain only ASCII alphanumerics
/// and underscores before case conversion, matching `to_module_name`'s input
/// contract.
///
/// # Errors
///
/// Returns [`ClassModuleRegistryError::InvalidPathSegment`] if any path
/// component contains a character other than an ASCII alphanumeric or `_`.
pub fn relative_module_segments(
    relative_path: &Utf8Path,
) -> Result<Vec<String>, ClassModuleRegistryError> {
    let without_ext = relative_path.with_extension("");
    without_ext
        .components()
        .map(|c| {
            let segment = c.as_str();
            if !segment
                .chars()
                .all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
            {
                return Err(ClassModuleRegistryError::InvalidPathSegment {
                    segment: segment.to_string(),
                    path: relative_path.to_string(),
                });
            }
            Ok(to_module_name(segment))
        })
        .collect()
}

/// Validates a stdlib class's path-derived module name against the
/// closed-form name computed from its real, parsed class name (ADR 0119
/// step 2 / BT-3432).
///
/// The stdlib arm never becomes a free lookup — the Erlang runtime derives
/// `bt@stdlib@{snake}` closed-form with no registry to consult (ADR 0119
/// Context) — so this only ever *validates* the file-stem-derived name
/// callers already computed (`build_stdlib.rs::module_name_from_path`)
/// against the name a real parse would produce, catching a file/class-name
/// disagreement instead of silently trusting the file stem.
///
/// # Errors
///
/// Returns [`ClassModuleRegistryError::StdlibNameMismatch`] if
/// `path_derived_module` disagrees with the closed-form `bt@stdlib@{snake}`
/// name computed from `class_name`.
pub fn validate_stdlib_module_name(
    class_name: &str,
    path_derived_module: &ModuleName,
) -> Result<(), ClassModuleRegistryError> {
    let name_derived = format!("bt@stdlib@{}", to_module_name(class_name));
    if path_derived_module.as_str() != name_derived {
        return Err(ClassModuleRegistryError::StdlibNameMismatch {
            class_name: class_name.to_string(),
            path_derived: path_derived_module.as_str().to_string(),
            name_derived,
        });
    }
    Ok(())
}

/// The class-name↔module-name resolution authority (ADR 0119).
///
/// Wraps two inverse maps so both directions of the question — "what module
/// is class X in" and "what class does module Y back" — are O(1) lookups
/// rather than a re-derivation.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ClassModuleRegistry {
    class_to_module: HashMap<(PackageId, String), ModuleName>,
    module_to_class: HashMap<ModuleName, (PackageId, String)>,
}

impl ClassModuleRegistry {
    /// An empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Records `class_name` (declared in `pkg`) as compiling to `module`,
    /// overwriting any existing entry for the same `(pkg, class_name)` key.
    ///
    /// The low-level insertion primitive: [`Self::assign`] computes a fresh
    /// `ModuleName` and calls this; batch construction from a file list (or
    /// from cached [`RegistryEntry`] rows) calls this once per discovered
    /// class.
    ///
    /// Assumes `module` is unique per `(pkg, class_name)` key, which the
    /// `bt@`/`bt@{pkg}@`/`bt@stdlib@` naming schemes guarantee by
    /// construction. If two different `(pkg, class_name)` keys were ever
    /// inserted with the *same* `module` — the exact naming collision this
    /// ADR exists to prevent — the second call's entry silently wins in
    /// `module_to_class`, so [`Self::class_for_module`] would answer for the
    /// wrong class while [`Self::module_for_class`] still answers correctly
    /// for both. Not asserted here: nothing in Phase 1 constructs such a
    /// collision, and enforcing it is a caller-level concern once a real
    /// build path calls this at scale (BT-3436).
    pub fn insert(&mut self, pkg: PackageId, class_name: impl Into<String>, module: ModuleName) {
        let class_name = class_name.into();
        self.module_to_class
            .insert(module.clone(), (pkg.clone(), class_name.clone()));
        self.class_to_module.insert((pkg, class_name), module);
    }

    /// Inserts a batch of [`RegistryEntry`] rows, e.g. restored from the
    /// incremental Pass-1 build cache.
    pub fn extend_from_entries(&mut self, entries: impl IntoIterator<Item = RegistryEntry>) {
        for entry in entries {
            self.insert(entry.package, entry.class_name, entry.module);
        }
    }

    /// Merges `other`'s entries into `self`, keeping `self`'s entry whenever
    /// both registries have one for the same `(package, class_name)` key.
    ///
    /// Used to compose the merge-precedence chain ADR 0119 specifies (own
    /// package, then dependencies, then stdlib): build a registry for the
    /// current package first, then `merge` in each dependency's registry,
    /// then `merge` in the stdlib registry — earlier merges always win,
    /// matching today's tier-1-first `compiled_module_name` behavior where
    /// `class_module_index` (built from the current package's own Pass 1) is
    /// consulted before the stdlib naming convention.
    ///
    /// Because every key already carries package identity, a same-named
    /// class declared in two *different* packages never collides here at
    /// all — `merge`'s tie-breaking only matters for the degenerate case of
    /// merging two registries that both assign the exact same
    /// `(package, class_name)` pair, e.g. re-merging a stdlib registry that
    /// was already partially present.
    pub fn merge(&mut self, other: &Self) {
        for (key, module) in &other.class_to_module {
            if !self.class_to_module.contains_key(key) {
                let (pkg, class_name) = key.clone();
                self.insert(pkg, class_name, module.clone());
            }
        }
    }

    /// Resolves `class_name`, declared in `pkg`, to its compiled module.
    ///
    /// Search order: an exact `(pkg, class_name)` entry first; if `pkg`
    /// itself is not [`PackageId::Stdlib`] and no exact entry exists, falls
    /// back to a `(Stdlib, class_name)` entry — the "own package shadows
    /// stdlib" precedence ADR 0119 requires (ADR 0026 forbids this collision
    /// outright today, but the fallback exists structurally so the registry
    /// does not need re-keying if that restriction is ever lifted).
    ///
    /// A cross-package reference to a *dependency's* class (not `pkg` and
    /// not stdlib) is not modeled by this two-argument lookup — the caller
    /// must query that dependency's own `PackageId` directly (or the
    /// follow-up wiring issue's consumer threads a full precedence-ordered
    /// package list through `compiled_module_name`). A miss here is not
    /// proof of a bug either way: per ADR 0100's open-world policy, a class
    /// absent from the registry falls back to the caller's existing
    /// best-effort convention — this method only ever returns `None`, never
    /// panics.
    #[must_use]
    pub fn module_for_class(&self, pkg: &PackageId, class_name: &str) -> Option<&ModuleName> {
        if let Some(module) = self
            .class_to_module
            .get(&(pkg.clone(), class_name.to_string()))
        {
            return Some(module);
        }
        if !matches!(pkg, PackageId::Stdlib) {
            return self
                .class_to_module
                .get(&(PackageId::Stdlib, class_name.to_string()));
        }
        None
    }

    /// The inverse of [`Self::module_for_class`]: which package and class
    /// name compiled to `module`.
    #[must_use]
    pub fn class_for_module(&self, module: &ModuleName) -> Option<(&PackageId, &str)> {
        self.module_to_class
            .get(module)
            .map(|(pkg, name)| (pkg, name.as_str()))
    }

    /// Mints a fresh module name for a class the caller is defining right
    /// now with no `.bt` source file — a REPL `subclass:` sent interactively,
    /// or a hot-reloaded inline class definition arriving over the
    /// compiler-port (ADR 0119's `derive_class_module_name` replacement).
    ///
    /// Not a lookup: this is how a class enters the registry in the first
    /// place outside batch compilation. Always mints a
    /// [`ModuleName::Generated`] name at the package's root — a class
    /// assigned this way has no file path to derive a subdirectory position
    /// from (see [`ModuleNamingScheme::Package`]'s doc).
    pub fn assign(
        &mut self,
        pkg: &PackageId,
        class_name: &str,
        naming: &ModuleNamingScheme,
    ) -> ModuleName {
        let snake = to_module_name(class_name);
        let module = ModuleName::Generated(match naming {
            ModuleNamingScheme::SingleFile => format!("bt@{snake}"),
            ModuleNamingScheme::Package { name } => format!("bt@{name}@{snake}"),
            ModuleNamingScheme::Stdlib => format!("bt@stdlib@{snake}"),
        });
        self.insert(pkg.clone(), class_name, module.clone());
        module
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assign_single_file_scheme() {
        let mut registry = ClassModuleRegistry::new();
        let module = registry.assign(
            &PackageId::SingleFile,
            "Counter",
            &ModuleNamingScheme::SingleFile,
        );
        assert_eq!(module, ModuleName::Generated("bt@counter".to_string()));
    }

    #[test]
    fn assign_package_scheme() {
        let mut registry = ClassModuleRegistry::new();
        let pkg = PackageId::Package("sicp".to_string());
        let module = registry.assign(
            &pkg,
            "SchemeEnv",
            &ModuleNamingScheme::Package {
                name: "sicp".to_string(),
            },
        );
        assert_eq!(
            module,
            ModuleName::Generated("bt@sicp@scheme_env".to_string())
        );
    }

    #[test]
    fn assign_stdlib_scheme() {
        let mut registry = ClassModuleRegistry::new();
        let module = registry.assign(
            &PackageId::Stdlib,
            "OrderedCollection",
            &ModuleNamingScheme::Stdlib,
        );
        assert_eq!(
            module,
            ModuleName::Generated("bt@stdlib@ordered_collection".to_string())
        );
    }

    #[test]
    fn module_for_class_round_trips_with_class_for_module() {
        let mut registry = ClassModuleRegistry::new();
        let pkg = PackageId::Package("bank".to_string());
        let module = registry.assign(
            &pkg,
            "Account",
            &ModuleNamingScheme::Package {
                name: "bank".to_string(),
            },
        );

        assert_eq!(registry.module_for_class(&pkg, "Account"), Some(&module));
        assert_eq!(registry.class_for_module(&module), Some((&pkg, "Account")));
    }

    #[test]
    fn module_for_class_returns_none_on_genuine_miss() {
        // ADR 0100's open-world policy: a class absent from the registry is
        // not an error here, just a `None` the caller falls back from.
        let registry = ClassModuleRegistry::new();
        assert_eq!(
            registry.module_for_class(&PackageId::SingleFile, "NoSuchClass"),
            None
        );
    }

    #[test]
    fn own_package_shadows_stdlib() {
        let mut registry = ClassModuleRegistry::new();
        registry.assign(&PackageId::Stdlib, "Set", &ModuleNamingScheme::Stdlib);
        let pkg = PackageId::Package("mylib".to_string());
        let own_module = registry.assign(
            &pkg,
            "Set",
            &ModuleNamingScheme::Package {
                name: "mylib".to_string(),
            },
        );

        // Own package wins over stdlib (ADR 0119 merge precedence).
        assert_eq!(registry.module_for_class(&pkg, "Set"), Some(&own_module));
        // Querying stdlib directly still finds stdlib's own entry.
        assert_eq!(
            registry.module_for_class(&PackageId::Stdlib, "Set"),
            Some(&ModuleName::Generated("bt@stdlib@set".to_string()))
        );
    }

    #[test]
    fn falls_back_to_stdlib_when_own_package_has_no_entry() {
        let mut registry = ClassModuleRegistry::new();
        registry.assign(
            &PackageId::Stdlib,
            "OrderedCollection",
            &ModuleNamingScheme::Stdlib,
        );
        let pkg = PackageId::Package("mylib".to_string());

        assert_eq!(
            registry.module_for_class(&pkg, "OrderedCollection"),
            Some(&ModuleName::Generated(
                "bt@stdlib@ordered_collection".to_string()
            ))
        );
    }

    #[test]
    fn merge_keeps_self_entry_on_conflict() {
        let mut own = ClassModuleRegistry::new();
        own.insert(
            PackageId::Stdlib,
            "Set",
            ModuleName::Generated("bt@stdlib@set".to_string()),
        );

        let mut stale = ClassModuleRegistry::new();
        stale.insert(
            PackageId::Stdlib,
            "Set",
            ModuleName::Generated("bt@stdlib@set_STALE".to_string()),
        );
        stale.insert(
            PackageId::Stdlib,
            "Bag",
            ModuleName::Generated("bt@stdlib@bag".to_string()),
        );

        own.merge(&stale);

        // Own entry wins on conflict...
        assert_eq!(
            own.module_for_class(&PackageId::Stdlib, "Set"),
            Some(&ModuleName::Generated("bt@stdlib@set".to_string()))
        );
        // ...but a non-conflicting entry from the merged registry is added.
        assert_eq!(
            own.module_for_class(&PackageId::Stdlib, "Bag"),
            Some(&ModuleName::Generated("bt@stdlib@bag".to_string()))
        );
    }

    #[test]
    fn extend_from_entries_batch_inserts() {
        let mut registry = ClassModuleRegistry::new();
        registry.extend_from_entries(vec![
            RegistryEntry {
                package: PackageId::Stdlib,
                class_name: "List".to_string(),
                module: ModuleName::Generated("bt@stdlib@list".to_string()),
            },
            RegistryEntry {
                package: PackageId::Stdlib,
                class_name: "Dictionary".to_string(),
                module: ModuleName::Generated("bt@stdlib@dictionary".to_string()),
            },
        ]);

        assert_eq!(
            registry.module_for_class(&PackageId::Stdlib, "List"),
            Some(&ModuleName::Generated("bt@stdlib@list".to_string()))
        );
        assert_eq!(
            registry.module_for_class(&PackageId::Stdlib, "Dictionary"),
            Some(&ModuleName::Generated("bt@stdlib@dictionary".to_string()))
        );
    }

    #[test]
    fn native_module_name_is_a_distinct_registry_entry() {
        // BT-3435 (ADR 0119 Phase 1): `Native` exists so `ModuleName` can
        // represent a hand-written-Erlang-backed class, but nothing seeds or
        // resolves one yet (see the type's doc). This only exercises the
        // variant's equality/hash/round-trip behavior through the registry
        // generically — it is not a `Future` regression test.
        let mut registry = ClassModuleRegistry::new();
        let native = ModuleName::Native("beamtalk_widget".to_string());
        registry.insert(PackageId::Stdlib, "Widget", native.clone());

        assert_eq!(
            registry.module_for_class(&PackageId::Stdlib, "Widget"),
            Some(&native)
        );
        assert_eq!(
            registry.class_for_module(&native),
            Some((&PackageId::Stdlib, "Widget"))
        );
        assert_ne!(
            ModuleName::Native("beamtalk_widget".to_string()),
            ModuleName::Generated("beamtalk_widget".to_string())
        );
    }

    #[test]
    fn bt_3081_regression_class_for_module_preserves_acronym_case() {
        // BT-3081 (Erlang runtime side, Done): `beamtalk_stack_frame`'s
        // `module_to_class`/`snake_to_class` inverse rebuilt a class name
        // from its snake_case module suffix by naive title-casing —
        // provably lossy for acronym-cased names: "BEAMError" ->
        // "beam_error" -> 'Beamerror' (wrong). Fixed there by routing
        // through the live class registry instead of re-deriving.
        //
        // `ClassModuleRegistry` is this ADR's Rust-side analogue of that
        // same "one class-name<->module-name authority" question, and BT-3437
        // asks for this exact scenario reproduced against it:
        // `class_for_module` must return the real class name recorded at
        // `assign`/`insert` time, never a re-derivation from the module
        // string, so the same lossy-inverse bug shape cannot recur here.
        // `to_module_name` only inserts an `_` on a lowercase->uppercase
        // transition, so consecutive capitals collapse together — exactly
        // the fold that makes "BEAMError" and "Beamerror" both compile to
        // "beamerror" and makes the *inverse* direction lossy if it tries to
        // re-derive a class name from that snake_case string instead of
        // consulting a real authority (BT-3081's bug).
        let mut registry = ClassModuleRegistry::new();
        let module = registry.assign(&PackageId::Stdlib, "BEAMError", &ModuleNamingScheme::Stdlib);
        assert_eq!(
            module,
            ModuleName::Generated("bt@stdlib@beamerror".to_string())
        );

        let (pkg, class_name) = registry
            .class_for_module(&module)
            .expect("module was just assigned");
        assert_eq!(pkg, &PackageId::Stdlib);
        assert_eq!(
            class_name, "BEAMError",
            "class_for_module must return the exact original class name, not a \
             lossy case-fold reconstruction from the snake_case module suffix \
             (BT-3081's bug shape: 'beamerror' -> 'Beamerror')"
        );
    }

    #[test]
    fn bt_3437_future_native_backing_module_resolves_to_real_module_not_a_guessed_bt_module() {
        // ADR 0119 Decision / BT-3435: `Future` is a runtime-only builtin
        // backed by hand-written `beamtalk_future.erl` (ADR 0056), with no
        // `stdlib/src/Future.bt` source file. Nothing seeds it into the
        // registry today — no live call site actually resolves `Future`'s
        // module (see `ModuleName::Native`'s doc and
        // `compute_direct_call_eligible`'s gate) — but the design must
        // still resolve it *correctly* were it ever registered: to its real
        // `Native("beamtalk_future")` backing module, never a guessed
        // `Generated("bt@stdlib@future")`/`Generated("bt@future")` module
        // that doesn't exist (the class of mistake the old four-tier
        // best-effort convention would make for any class with no `.bt`
        // source to derive a path from).
        let mut registry = ClassModuleRegistry::new();
        let native = ModuleName::Native("beamtalk_future".to_string());
        registry.insert(PackageId::Stdlib, "Future", native.clone());

        assert_eq!(
            registry.module_for_class(&PackageId::Stdlib, "Future"),
            Some(&native),
            "Future must resolve to its real native backing module"
        );
        assert_eq!(
            registry.class_for_module(&native),
            Some((&PackageId::Stdlib, "Future"))
        );
        // Not the nonexistent generated modules a naming-convention guess
        // would produce for a class with no source file to derive from.
        assert_ne!(
            registry.module_for_class(&PackageId::Stdlib, "Future"),
            Some(&ModuleName::Generated("bt@stdlib@future".to_string()))
        );
        assert_ne!(
            registry.module_for_class(&PackageId::Stdlib, "Future"),
            Some(&ModuleName::Generated("bt@future".to_string()))
        );
    }

    #[test]
    fn relative_module_segments_joins_subdirectories() {
        let segments =
            relative_module_segments(Utf8Path::new("scheme/Env.bt")).expect("valid path");
        assert_eq!(segments, vec!["scheme".to_string(), "env".to_string()]);
    }

    #[test]
    fn relative_module_segments_rejects_invalid_characters() {
        let err = relative_module_segments(Utf8Path::new("sc-heme/env.bt"))
            .expect_err("hyphen is not a valid segment character");
        assert!(matches!(
            err,
            ClassModuleRegistryError::InvalidPathSegment { .. }
        ));
    }

    #[test]
    fn validate_stdlib_module_name_accepts_agreeing_name() {
        assert!(
            validate_stdlib_module_name(
                "OrderedCollection",
                &ModuleName::Generated("bt@stdlib@ordered_collection".to_string())
            )
            .is_ok()
        );
    }

    #[test]
    fn validate_stdlib_module_name_rejects_disagreeing_name() {
        // The BT-3432 bug shape: `TestCase.bt` renamed to `test_case.bt`
        // (path-derived: bt@stdlib@test_case) while the class stays `TestCase`
        // — the two must always agree, so a real mismatch is a hard error.
        //
        // BT-3437 (this exact scenario against the unified registry): this
        // is the literal historical rename that broke `TestCase` resolution
        // via the old file-stem-scanning `STDLIB_CLASS_NAMES` (deleted in
        // BT-3435). `build_stdlib.rs::compile_all_stdlib_files` calls this
        // same validator on every real stdlib source file at
        // `beamtalk build-stdlib` time, so this bug shape is now caught at
        // build time, not silently mis-resolved at codegen time.
        let err = validate_stdlib_module_name(
            "TestCase",
            &ModuleName::Generated("bt@stdlib@wrong_name".to_string()),
        )
        .expect_err("mismatched names must be rejected");
        assert!(matches!(
            err,
            ClassModuleRegistryError::StdlibNameMismatch { .. }
        ));
    }
}
