// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Native type registry for Erlang FFI type information (ADR 0075).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Stores type information extracted from `.beam` abstract code by
//! `beamtalk_spec_reader.erl`. Provides typed signatures for Erlang
//! functions, enabling the type checker to infer return types from FFI
//! calls and the LSP to show type info in completions/hover.
//!
//! ## Registry Structure
//!
//! ```text
//! module → function_name → arity → FunctionSignature
//! ```
//!
//! ## Resolution Order (ADR 0075)
//!
//! 1. Project-local stubs/ (user overrides)
//! 2. Package-bundled stubs/ (library author)
//! 3. Distribution stubs/ (shipped with compiler)
//! 4. Auto-extracted (.beam `abstract_code`)
//! 5. Dynamic (no type info)
//!
//! This registry handles all five layers: [`NativeTypeRegistry::apply_overrides`]
//! implements the function/arity-level stub-over-auto-extract merge shared by
//! layers 1–3 (BT-1847); layer 1 (project-local `stubs/`) is populated by
//! `beamtalk build` via [`super::native_types::load_native_declarations`].
//! Layers 2–3 (package-bundled and compiler-distribution stubs) are not yet
//! wired into the build — see BT-1848 and the follow-up filed alongside
//! BT-1847 for package-bundled stub discovery.

#[cfg(test)]
use super::types::DynamicReason;
use super::types::{InferredType, TypeProvenance};
use ecow::EcoString;
use std::collections::HashMap;
use std::sync::Arc;

/// Type signature for a single Erlang function.
///
/// Stores parameter types and names alongside the return type,
/// with provenance tracking for diagnostic messages.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    /// Erlang function name.
    pub name: String,
    /// Erlang arity.
    pub arity: u8,
    /// Parameter types with optional keyword names.
    pub params: Vec<ParamType>,
    /// Beamtalk return type.
    pub return_type: InferredType,
    /// Where this signature came from (Extracted for .beam, Declared for stubs).
    pub provenance: TypeProvenance,
    /// Source line number of the function definition (1-based), if available.
    ///
    /// Extracted from `{function, Line, _, _, _}` forms in `.beam` abstract code.
    /// Used by goto-definition to navigate to the exact function in `.erl` files.
    pub line: Option<u32>,
}

impl FunctionSignature {
    /// Format as a Beamtalk-style type signature for display.
    ///
    /// Uses keyword names when available, falling back to the param type name.
    ///
    /// ## Examples
    ///
    /// - `reverse: list :: List -> List`
    /// - `seq: from :: Integer to: to :: Integer -> List`
    /// - `node -> Symbol` (nullary)
    #[must_use]
    pub fn display_signature(&self) -> String {
        if self.params.is_empty() {
            let ret_display = self
                .return_type
                .display_for_diagnostic()
                .unwrap_or_else(|| EcoString::from("Dynamic"));
            return format!("{} -> {ret_display}", self.name);
        }

        let mut parts = Vec::new();
        for (i, param) in self.params.iter().enumerate() {
            let keyword = if i == 0 {
                format!("{}:", self.name)
            } else {
                match &param.keyword {
                    Some(kw) => format!("{kw}:"),
                    None => "with:".to_string(),
                }
            };
            let param_name = param.keyword.as_deref().unwrap_or("arg");
            let type_display = param
                .type_
                .display_for_diagnostic()
                .unwrap_or_else(|| EcoString::from("Dynamic"));
            parts.push(format!("{keyword} {param_name} :: {type_display}"));
        }

        let ret_display = self
            .return_type
            .display_for_diagnostic()
            .unwrap_or_else(|| EcoString::from("Dynamic"));
        format!("{} -> {ret_display}", parts.join(" "))
    }
}

/// A single parameter in a native function signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamType {
    /// Beamtalk keyword name (lowercased from spec variable name).
    ///
    /// `None` when no meaningful name is available (positional fallback).
    pub keyword: Option<EcoString>,
    /// Beamtalk type for this parameter.
    pub type_: InferredType,
}

/// Registry of type information for native (Erlang) modules.
///
/// Keyed by module name → list of function type signatures. Provides
/// lookup by (module, function, arity) for the type checker and LSP.
///
/// BT-2867: `modules` is `Arc`-wrapped so `Clone` is a cheap refcount bump.
/// `infer_types`/`infer_types_and_returns`/`infer_method_return_types` and
/// every LSP query provider that owns only a borrowed
/// `Option<&NativeTypeRegistry>` clone the registry to hand `TypeChecker` an
/// owned copy (`impl Into<Arc<NativeTypeRegistry>>`) — on the hover/
/// completion/signature-help hot path, that used to deep-copy the whole
/// `module → signatures` map on every call.
#[derive(Debug, Clone, Default)]
pub struct NativeTypeRegistry {
    /// Module name → list of function type signatures.
    modules: Arc<HashMap<String, Vec<FunctionSignature>>>,
}

impl NativeTypeRegistry {
    /// Creates an empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self {
            modules: Arc::new(HashMap::new()),
        }
    }

    /// Registers type information for an Erlang module.
    ///
    /// Replaces any existing signatures for this module.
    pub fn register_module(&mut self, module_name: &str, functions: Vec<FunctionSignature>) {
        Arc::make_mut(&mut self.modules).insert(module_name.to_string(), functions);
    }

    /// Merges all modules from `other` into `self`, keeping `self`'s entry on
    /// a module-name collision.
    ///
    /// Precedence is caller-defined via merge direction: merging a dependency
    /// registry into an OTP registry keeps the OTP signatures ahead of any
    /// same-named dependency module (mirroring the OTP-first de-duplication in
    /// `beamtalk build`'s spec extraction).
    pub fn merge(&mut self, other: NativeTypeRegistry) {
        let other_modules =
            Arc::try_unwrap(other.modules).unwrap_or_else(|shared| (*shared).clone());
        let self_modules = Arc::make_mut(&mut self.modules);
        for (module, functions) in other_modules {
            self_modules.entry(module).or_insert(functions);
        }
    }

    /// Adds or replaces `functions` in `module_name`'s signature list,
    /// matched by `(name, arity)` — every other function already registered
    /// for that module is left untouched (ADR 0075 Phase 2, BT-1847).
    ///
    /// Unlike [`Self::register_module`] (whole-module replace) and
    /// [`Self::merge`] (whole-module keep-on-collision), this is the
    /// function/arity-level upsert stub declarations need: a `stubs/lists.bt`
    /// overriding `reverse/1` must not discard `lists`'s other
    /// auto-extracted functions.
    pub fn upsert_functions(&mut self, module_name: &str, functions: Vec<FunctionSignature>) {
        let self_modules = Arc::make_mut(&mut self.modules);
        let entry = self_modules.entry(module_name.to_string()).or_default();
        for func in functions {
            if let Some(existing) = entry
                .iter_mut()
                .find(|f| f.name == func.name && f.arity == func.arity)
            {
                *existing = func;
            } else {
                entry.push(func);
            }
        }
    }

    /// Applies every function in `overrides` as a function/arity-level
    /// override onto `self` via [`Self::upsert_functions`] (ADR 0075 Phase 2,
    /// BT-1847) — the stub-over-auto-extract merge direction: `overrides`
    /// (higher-precedence stubs) wins per function/arity, everything else
    /// `self` already has (auto-extracted) is preserved.
    pub fn apply_overrides(&mut self, overrides: NativeTypeRegistry) {
        let override_modules =
            Arc::try_unwrap(overrides.modules).unwrap_or_else(|shared| (*shared).clone());
        for (module, functions) in override_modules {
            self.upsert_functions(&module, functions);
        }
    }

    /// Looks up the type signature for a specific function.
    #[must_use]
    pub fn lookup(&self, module: &str, function: &str, arity: u8) -> Option<&FunctionSignature> {
        self.modules
            .get(module)
            .and_then(|fns| fns.iter().find(|f| f.name == function && f.arity == arity))
    }

    /// Returns all function signatures for a module.
    #[must_use]
    pub fn module_functions(&self, module: &str) -> Option<&[FunctionSignature]> {
        self.modules.get(module).map(Vec::as_slice)
    }

    /// Returns `true` if the registry contains type info for the given module.
    #[must_use]
    pub fn has_module(&self, module: &str) -> bool {
        self.modules.contains_key(module)
    }

    /// Returns the number of modules in the registry.
    #[must_use]
    pub fn module_count(&self) -> usize {
        self.modules.len()
    }

    /// Returns the total number of function signatures across all modules.
    #[must_use]
    pub fn function_count(&self) -> usize {
        self.modules.values().map(Vec::len).sum()
    }

    /// Returns an iterator over all module names in the registry.
    pub fn module_names(&self) -> impl Iterator<Item = &str> {
        self.modules.keys().map(String::as_str)
    }

    /// Version drift detection (ADR 0075 Phase 2, BT-1847): compares `stubs`
    /// against `self` — the auto-extracted registry, treated as ground truth
    /// for what a `.beam` module actually exports — and returns every stub
    /// function/arity that doesn't exist there, paired with its owning
    /// module name.
    ///
    /// Call with the *pre-merge* auto-extracted registry (before
    /// [`Self::apply_overrides`] applies the same stubs) — merging first
    /// would make every stub function trivially "exist".
    ///
    /// A stub module `self` has no entry for at all (not on the code path,
    /// a typo'd module name, or a package's own native module not yet
    /// compiled) has nothing to compare against and is silently skipped —
    /// only a *known* module's *missing* function/arity is drift.
    #[must_use]
    pub fn detect_stub_drift<'a>(
        &self,
        stubs: &'a NativeTypeRegistry,
    ) -> Vec<(&'a str, &'a FunctionSignature)> {
        let mut drift = Vec::new();
        for module in stubs.module_names() {
            let Some(actual_functions) = self.module_functions(module) else {
                continue;
            };
            let Some(stub_functions) = stubs.module_functions(module) else {
                continue;
            };
            for stub_fn in stub_functions {
                let exists = actual_functions
                    .iter()
                    .any(|f| f.name == stub_fn.name && f.arity == stub_fn.arity);
                if !exists {
                    drift.push((module, stub_fn));
                }
            }
        }
        drift
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn extracted_sig(
        name: &str,
        arity: u8,
        params: Vec<ParamType>,
        return_type: InferredType,
    ) -> FunctionSignature {
        FunctionSignature {
            name: name.to_string(),
            arity,
            params,
            return_type,
            provenance: TypeProvenance::Extracted,
            line: None,
        }
    }

    fn param(keyword: &str, type_name: &str) -> ParamType {
        ParamType {
            keyword: Some(EcoString::from(keyword)),
            type_: InferredType::known(type_name),
        }
    }

    #[test]
    fn empty_registry_returns_none() {
        let reg = NativeTypeRegistry::new();
        assert!(reg.lookup("lists", "reverse", 1).is_none());
        assert!(!reg.has_module("lists"));
        assert_eq!(reg.module_count(), 0);
        assert_eq!(reg.function_count(), 0);
    }

    #[test]
    fn merge_keeps_self_on_collision_and_adds_new_modules() {
        let mut otp = NativeTypeRegistry::new();
        otp.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );

        let mut deps = NativeTypeRegistry::new();
        // Same module name as OTP — must NOT overwrite the OTP entry.
        deps.register_module(
            "lists",
            vec![extracted_sig(
                "bogus",
                0,
                vec![],
                InferredType::known("Dynamic"),
            )],
        );
        // A module unique to deps — must be added.
        deps.register_module(
            "my_dep",
            vec![extracted_sig(
                "go",
                0,
                vec![],
                InferredType::known("Integer"),
            )],
        );

        otp.merge(deps);

        assert_eq!(otp.module_count(), 2);
        // OTP `lists` wins the collision.
        assert!(otp.lookup("lists", "reverse", 1).is_some());
        assert!(otp.lookup("lists", "bogus", 0).is_none());
        // The dep-only module is present.
        assert!(otp.lookup("my_dep", "go", 0).is_some());
    }

    #[test]
    fn register_and_lookup() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );

        assert!(reg.has_module("lists"));
        assert_eq!(reg.module_count(), 1);
        assert_eq!(reg.function_count(), 1);

        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.return_type, InferredType::known("List"));
        assert_eq!(sig.params.len(), 1);
        assert_eq!(sig.params[0].keyword, Some(EcoString::from("list")));
    }

    #[test]
    fn display_signature_unary() {
        let sig = extracted_sig(
            "reverse",
            1,
            vec![param("list", "List")],
            InferredType::known("List"),
        );
        assert_eq!(sig.display_signature(), "reverse: list :: List -> List");
    }

    #[test]
    fn display_signature_binary() {
        let sig = extracted_sig(
            "seq",
            2,
            vec![param("from", "Integer"), param("to", "Integer")],
            InferredType::known("List"),
        );
        assert_eq!(
            sig.display_signature(),
            "seq: from :: Integer to: to :: Integer -> List"
        );
    }

    #[test]
    fn display_signature_nullary() {
        let sig = extracted_sig("node", 0, vec![], InferredType::known("Symbol"));
        assert_eq!(sig.display_signature(), "node -> Symbol");
    }

    #[test]
    fn display_signature_dynamic_return() {
        let sig = extracted_sig(
            "apply",
            1,
            vec![param("fun", "Block")],
            InferredType::Dynamic(DynamicReason::DynamicSpec),
        );
        assert_eq!(
            sig.display_signature(),
            "apply: fun :: Block -> Dynamic (FFI spec is Dynamic)"
        );
    }

    #[test]
    fn display_signature_no_keyword() {
        let sig = extracted_sig(
            "foo",
            1,
            vec![ParamType {
                keyword: None,
                type_: InferredType::known("Integer"),
            }],
            InferredType::known("Integer"),
        );
        assert_eq!(sig.display_signature(), "foo: arg :: Integer -> Integer");
    }

    #[test]
    fn lookup_wrong_arity_returns_none() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );
        assert!(reg.lookup("lists", "reverse", 2).is_none());
    }

    #[test]
    fn module_functions_returns_all() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![
                extracted_sig(
                    "reverse",
                    1,
                    vec![param("list", "List")],
                    InferredType::known("List"),
                ),
                extracted_sig(
                    "sort",
                    1,
                    vec![param("list", "List")],
                    InferredType::known("List"),
                ),
            ],
        );

        let fns = reg.module_functions("lists").unwrap();
        assert_eq!(fns.len(), 2);
    }

    #[test]
    fn register_module_replaces_existing() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );
        assert_eq!(reg.function_count(), 1);

        // Re-register with different functions
        reg.register_module(
            "lists",
            vec![
                extracted_sig(
                    "sort",
                    1,
                    vec![param("list", "List")],
                    InferredType::known("List"),
                ),
                extracted_sig(
                    "nth",
                    2,
                    vec![param("n", "Integer"), param("list", "List")],
                    InferredType::known("Dynamic"),
                ),
            ],
        );
        assert_eq!(reg.function_count(), 2);
        assert!(reg.lookup("lists", "reverse", 1).is_none());
        assert!(reg.lookup("lists", "sort", 1).is_some());
    }

    #[test]
    fn multiple_modules() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );
        reg.register_module(
            "maps",
            vec![extracted_sig(
                "get",
                2,
                vec![param("key", "Dynamic"), param("map", "Dictionary")],
                InferredType::Dynamic(DynamicReason::DynamicSpec),
            )],
        );

        assert_eq!(reg.module_count(), 2);
        assert_eq!(reg.function_count(), 2);
        assert!(reg.has_module("lists"));
        assert!(reg.has_module("maps"));
        assert!(!reg.has_module("string"));
    }

    #[test]
    fn provenance_is_extracted() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );

        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.provenance, TypeProvenance::Extracted);
    }

    // ── upsert_functions / apply_overrides (BT-1847) ────────────────────────

    fn declared_sig(name: &str, arity: u8, return_type: InferredType) -> FunctionSignature {
        FunctionSignature {
            name: name.to_string(),
            arity,
            params: vec![],
            return_type,
            provenance: TypeProvenance::Declared(crate::span::Span::new(0, 0)),
            line: None,
        }
    }

    #[test]
    fn upsert_functions_overrides_matching_arity_only() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![
                extracted_sig(
                    "reverse",
                    1,
                    vec![param("list", "List")],
                    InferredType::known("List"),
                ),
                extracted_sig(
                    "member",
                    2,
                    vec![],
                    InferredType::Dynamic(DynamicReason::DynamicSpec),
                ),
            ],
        );

        reg.upsert_functions(
            "lists",
            vec![declared_sig("member", 2, InferredType::known("Boolean"))],
        );

        assert_eq!(reg.module_functions("lists").unwrap().len(), 2);
        // Overridden function has the stub's tightened type + Declared provenance.
        let member = reg.lookup("lists", "member", 2).unwrap();
        assert_eq!(member.return_type, InferredType::known("Boolean"));
        assert!(matches!(member.provenance, TypeProvenance::Declared(_)));
        // Untouched function survives unchanged.
        let reverse = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(reverse.return_type, InferredType::known("List"));
        assert_eq!(reverse.provenance, TypeProvenance::Extracted);
    }

    #[test]
    fn upsert_functions_adds_new_function_to_existing_module() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );

        reg.upsert_functions(
            "lists",
            vec![declared_sig("seq", 2, InferredType::known("List"))],
        );

        assert_eq!(reg.module_functions("lists").unwrap().len(), 2);
        assert!(reg.lookup("lists", "seq", 2).is_some());
    }

    #[test]
    fn upsert_functions_creates_module_when_absent() {
        let mut reg = NativeTypeRegistry::new();

        reg.upsert_functions(
            "brand_new",
            vec![declared_sig("go", 0, InferredType::known("Integer"))],
        );

        assert!(reg.has_module("brand_new"));
        assert!(reg.lookup("brand_new", "go", 0).is_some());
    }

    #[test]
    fn apply_overrides_leaves_other_modules_untouched() {
        let mut auto_extract = NativeTypeRegistry::new();
        auto_extract.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );
        auto_extract.register_module(
            "maps",
            vec![extracted_sig(
                "keys",
                1,
                vec![],
                InferredType::known("List"),
            )],
        );

        let mut stubs = NativeTypeRegistry::new();
        stubs.register_module(
            "lists",
            vec![declared_sig("reverse", 1, InferredType::known("List"))],
        );

        auto_extract.apply_overrides(stubs);

        assert_eq!(auto_extract.module_count(), 2);
        assert!(matches!(
            auto_extract
                .lookup("lists", "reverse", 1)
                .unwrap()
                .provenance,
            TypeProvenance::Declared(_)
        ));
        assert_eq!(
            auto_extract.lookup("maps", "keys", 1).unwrap().provenance,
            TypeProvenance::Extracted
        );
    }

    // ── detect_stub_drift (BT-1847) ──────────────────────────────────────────

    #[test]
    fn detect_stub_drift_flags_function_missing_from_known_module() {
        let mut auto_extract = NativeTypeRegistry::new();
        auto_extract.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );

        let mut stubs = NativeTypeRegistry::new();
        stubs.register_module(
            "lists",
            vec![
                declared_sig("reverse", 1, InferredType::known("List")),
                // Not a real `lists` export — a stale/typo'd stub entry.
                declared_sig("bogus", 2, InferredType::known("Dynamic")),
            ],
        );

        let drift = auto_extract.detect_stub_drift(&stubs);

        assert_eq!(drift.len(), 1);
        assert_eq!(drift[0].0, "lists");
        assert_eq!(drift[0].1.name, "bogus");
        assert_eq!(drift[0].1.arity, 2);
    }

    #[test]
    fn detect_stub_drift_skips_module_unknown_to_auto_extract() {
        // `beamtalk_http` is the package's own not-yet-compiled native
        // module — nothing to compare against, so no false-positive drift.
        let auto_extract = NativeTypeRegistry::new();

        let mut stubs = NativeTypeRegistry::new();
        stubs.register_module(
            "beamtalk_http",
            vec![declared_sig("get", 1, InferredType::known("Dynamic"))],
        );

        let drift = auto_extract.detect_stub_drift(&stubs);

        assert!(drift.is_empty());
    }

    #[test]
    fn detect_stub_drift_empty_when_every_stub_function_exists() {
        let mut auto_extract = NativeTypeRegistry::new();
        auto_extract.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );

        let mut stubs = NativeTypeRegistry::new();
        stubs.register_module(
            "lists",
            vec![declared_sig("reverse", 1, InferredType::known("List"))],
        );

        assert!(auto_extract.detect_stub_drift(&stubs).is_empty());
    }
}
