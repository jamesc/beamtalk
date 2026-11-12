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
//! layers 1–3; layer 1 (project-local `stubs/`) is populated by
//! `beamtalk build` via [`super::native_types::load_native_declarations`].
//! Layers 2–3 (package-bundled and compiler-distribution stubs) are not yet
//! wired into the build — package-bundled stub discovery is a separate,
//! later concern.

#[cfg(test)]
use super::types::DynamicReason;
use super::types::{InferredType, TypeProvenance};
use ecow::EcoString;
use std::collections::{HashMap, HashSet};
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
/// `modules` is `Arc`-wrapped so `Clone` is a cheap refcount bump.
/// `infer_types`/`infer_types_and_returns`/`infer_method_return_types` and
/// every LSP query provider that owns only a borrowed
/// `Option<&NativeTypeRegistry>` clone the registry to hand `TypeChecker` an
/// owned copy (`impl Into<Arc<NativeTypeRegistry>>`) — on the hover/
/// completion/signature-help hot path, a deep clone of the whole
/// `module → signatures` map on every call would be wasteful.
#[derive(Debug, Clone, Default)]
pub struct NativeTypeRegistry {
    /// Module name → list of function type signatures.
    modules: Arc<HashMap<String, Vec<FunctionSignature>>>,
    /// Module name → exported `(name, arity)` pairs that have *no* signature
    /// in `modules` (an export without a `-spec`, BT-4574). Carries no type
    /// information: it exists so [`Self::detect_stub_drift`] can tell "not
    /// exported" apart from "exported but unspec'd".
    exports_without_signature: Arc<HashMap<String, HashSet<(String, u8)>>>,
    /// Modules whose extracted `-spec` result types may prove an inline FFI
    /// call needs no coercion (BT-4507). Dialyzer specs are not enforced, so
    /// only specs the package author does not merely inherit from third
    /// parties qualify: OTP, runtime `beamtalk_*` and the package's own
    /// `native/` modules. Fails closed: an empty set proves nothing.
    trusted_result_modules: Arc<HashSet<String>>,
}

impl NativeTypeRegistry {
    /// Creates an empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Registers type information for an Erlang module.
    ///
    /// Replaces any existing signatures for this module.
    pub fn register_module(&mut self, module_name: &str, functions: Vec<FunctionSignature>) {
        Arc::make_mut(&mut self.modules).insert(module_name.to_string(), functions);
    }

    /// Records `exports` — exported `(name, arity)` pairs with no `-spec` —
    /// for `module_name`, replacing any previously recorded for it (BT-4574).
    ///
    /// These are not signatures: [`Self::lookup`] and
    /// [`Self::module_functions`] never see them. Only
    /// [`Self::exports_function`] (and so the stub drift check) does.
    pub fn register_module_exports(
        &mut self,
        module_name: &str,
        exports: impl IntoIterator<Item = (String, u8)>,
    ) {
        let exports: HashSet<(String, u8)> = exports.into_iter().collect();
        let map = Arc::make_mut(&mut self.exports_without_signature);
        if exports.is_empty() {
            map.remove(module_name);
        } else {
            map.insert(module_name.to_string(), exports);
        }
    }

    /// Marks every module currently registered as trusted to prove
    /// no-result-coercion FFI calls (BT-4507). Call after registering a
    /// trusted source (OTP, runtime, the package's own `native/` beams) and
    /// before merging untrusted ones.
    pub fn trust_all_modules(&mut self) {
        let trusted = Arc::make_mut(&mut self.trusted_result_modules);
        trusted.extend(self.modules.keys().cloned());
    }

    /// Whether `module`'s extracted specs may prove a result needs no
    /// coercion (BT-4507).
    #[must_use]
    pub fn is_result_trusted(&self, module: &str) -> bool {
        self.trusted_result_modules.contains(module)
    }

    /// Whether `module:function/arity` is known to be exported: it has a
    /// signature, or was recorded by [`Self::register_module_exports`].
    #[must_use]
    pub fn exports_function(&self, module: &str, function: &str, arity: u8) -> bool {
        self.lookup(module, function, arity).is_some()
            || self
                .exports_without_signature
                .get(module)
                .is_some_and(|exports| exports.contains(&(function.to_string(), arity)))
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
        let mut other_exports = Arc::try_unwrap(other.exports_without_signature)
            .unwrap_or_else(|shared| (*shared).clone());
        let self_modules = Arc::make_mut(&mut self.modules);
        let self_exports = Arc::make_mut(&mut self.exports_without_signature);
        for (module, functions) in other_modules {
            // A module's export list travels with its signatures: whichever
            // side's signatures win the collision supplies both.
            if self_modules.contains_key(&module) {
                continue;
            }
            if let Some(exports) = other_exports.remove(&module) {
                self_exports.insert(module.clone(), exports);
            }
            // Trust travels with the winning side's signatures too.
            if other.trusted_result_modules.contains(&module) {
                Arc::make_mut(&mut self.trusted_result_modules).insert(module.clone());
            }
            self_modules.insert(module, functions);
        }
    }

    /// Adds or replaces `functions` in `module_name`'s signature list,
    /// matched by `(name, arity)` — every other function already registered
    /// for that module is left untouched (ADR 0075 Phase 2).
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
    /// override onto `self` via [`Self::upsert_functions`] (ADR 0075 Phase 2)
    /// — the stub-over-auto-extract merge direction: `overrides`
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

    /// Version drift detection (ADR 0075 Phase 2): compares `stubs`
    /// against `self` — the auto-extracted registry, treated as ground truth
    /// for what a `.beam` module actually exports — and returns every stub
    /// function/arity that doesn't exist there, paired with its owning
    /// module name.
    ///
    /// "Exists" means [`Self::exports_function`]: a spec'd signature *or* an
    /// export without a `-spec` (BT-4574). Checking signatures alone would
    /// flag every stub for a module with no specs — exactly the modules
    /// ADR 0075 says to hand-write stubs for.
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
            if !self.has_module(module) {
                continue;
            }
            let Some(stub_functions) = stubs.module_functions(module) else {
                continue;
            };
            for stub_fn in stub_functions {
                if !self.exports_function(module, &stub_fn.name, stub_fn.arity) {
                    drift.push((module, stub_fn));
                }
            }
        }
        drift
    }

    /// Whether the spec of `module:function/arity` proves that an inline FFI
    /// call to it needs no result coercion (BT-4485).
    ///
    /// True when the return type extracted from the function's `-spec` is
    /// made only of types that `beamtalk_erlang_proxy:coerce_ffi_result/3`
    /// passes through unchanged in both coercion modes
    /// ([`ffi_result_passes_unchanged`]). Unknown functions (no spec) are not
    /// proved, and neither are hand-written stub signatures: a stub that
    /// misstates a return type may cost a wrong warning, never a dropped
    /// coercion. Only modules marked by [`Self::trust_all_modules`] can prove
    /// a call: specs are not enforced, so a wrong third-party (hex / path
    /// dependency) `-spec` must not change what a call returns (BT-4507).
    #[must_use]
    pub fn proves_no_result_coercion(&self, module: &str, function: &str, arity: u8) -> bool {
        self.is_result_trusted(module)
            && self.lookup(module, function, arity).is_some_and(|sig| {
                sig.provenance == TypeProvenance::Extracted
                    && ffi_result_passes_unchanged(&sig.return_type)
            })
    }

    /// Sorted `module:function/arity` keys of every function
    /// [`Self::proves_no_result_coercion`] holds for. Generated code depends
    /// on exactly this set, so `beamtalk build` folds it into each file's
    /// incremental cache key: a spec change that adds or removes a proof
    /// recompiles the callers.
    #[must_use]
    pub fn proved_no_coercion_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self
            .modules
            .iter()
            .flat_map(|(module, fns)| {
                fns.iter()
                    .filter(|sig| self.proves_no_result_coercion(module, &sig.name, sig.arity))
                    .map(move |sig| format!("{module}:{}/{}", sig.name, sig.arity))
            })
            .collect();
        keys.sort_unstable();
        keys
    }
}

// ── FFI coercion proofs (BT-4485) ──────────────────────────────────────────
//
// The one place that decides when an inline `Erlang module fn: …` call may
// skip `beamtalk_erlang_proxy:direct_call/4` and be emitted as a direct remote
// call. Codegen asks these and never re-derives the rule. Both lists are
// pinned to the runtime by the shared fixture
// `runtime/apps/beamtalk_runtime/test/fixtures/ffi_passthrough_conformance.json`:
// the Erlang side (`beamtalk_erlang_proxy_tests`) checks every sample value
// really crosses the proxy unchanged, the Rust side (tests below) checks these
// lists match the fixture.

/// Classes whose values are scalars: never a tagged `String`, never a list,
/// tuple or map, and never a fun or anything holding one.
const FFI_SCALAR_CLASSES: &[&str] = &[
    "Integer",
    "Float",
    "Number",
    "Boolean",
    "True",
    "False",
    "UndefinedObject",
    "Nil",
    "Pid",
];

/// Extra classes a *result* may have: their values are not scalars, but
/// `coerce_ffi_result/3` returns them unchanged in both modes: maps, funs,
/// binaries (`Binary`) and Strings (`String`: a bare binary or the tagged
/// `{beamtalk_string, Bin}` 2-tuple, which is neither `{ok, _}` nor
/// `{error, _}`). `String` must be here for `binary()` to count: the spec
/// reader maps `binary()` to the union `String | Binary` (BT-4755), and a
/// union is proved only when every member is. `string()` maps to
/// `String | List`, so a charlist result stays on the proxy via `List`.
/// Arguments are different: a `String`/`Binary` *argument* is unwrapped or
/// retried by the proxy, so these two stay out of the argument rule.
const FFI_RESULT_EXTRA_CLASSES: &[&str] = &["Dictionary", "Block", "String", "Binary"];

/// Extra classes an *argument* may have. `Symbol` is an atom; as a result it
/// is not proved, because `atom()` includes `ok` and `error`, which the proxy
/// coerces to a `Result`.
const FFI_ARG_EXTRA_CLASSES: &[&str] = &["Symbol"];

/// The runtime check that backs [`ffi_arg_passes_unchanged`]: a value that
/// passes one of these `erlang` guard BIFs is a number, atom or pid, so it is
/// never a tagged `String`, a binary or a fun. Static types are not enforced at
/// run time (gradual typing), so a direct call also checks each argument with
/// these and falls back to the proxy when one fails.
pub const FFI_ARG_RUNTIME_GUARDS: &[&str] = &["is_number", "is_atom", "is_pid"];

/// Every member of `ty` (a single type or a union) satisfies `member_ok`.
fn all_members(ty: &InferredType, member_ok: &dyn Fn(&str) -> bool) -> bool {
    match ty {
        InferredType::Known { class_name, .. } => member_ok(class_name),
        InferredType::Union { members, .. } => members.iter().all(|m| all_members(m, member_ok)),
        _ => false,
    }
}

/// A result of type `ty` passes through `coerce_ffi_result/3` unchanged in
/// both coercion modes: no list (charlist coercion), no tuple or `Result` (`{ok, _}`/`{error, _}` coercion), no atom that may be
/// `ok`/`error`, and no `Dynamic`/`Object`.
#[must_use]
pub fn ffi_result_passes_unchanged(ty: &InferredType) -> bool {
    all_members(ty, &|name| {
        if crate::semantic_analysis::string_utils::is_symbol_singleton_name(name) {
            let atom = &name[1..];
            return atom != "ok" && atom != "error";
        }
        FFI_SCALAR_CLASSES.contains(&name) || FFI_RESULT_EXTRA_CLASSES.contains(&name)
    })
}

/// An argument of static type `ty` reaches the callee unchanged and cannot
/// carry Beamtalk code: never a tagged `String` (the proxy's stock-mode unwrap
/// would change it) and never a fun or a container that may hold one (a block
/// run by the callee would change how a raised error is classified). The
/// `badarg` charlist retry is a no-op for these too: no binaries.
#[must_use]
pub fn ffi_arg_passes_unchanged(ty: &InferredType) -> bool {
    all_members(ty, &|name| {
        crate::semantic_analysis::string_utils::is_symbol_singleton_name(name)
            || FFI_SCALAR_CLASSES.contains(&name)
            || FFI_ARG_EXTRA_CLASSES.contains(&name)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ffi_result_map_shape_is_not_a_passthrough_atom() {
        assert!(ffi_result_passes_unchanged(&InferredType::known("#done")));
        assert!(!ffi_result_passes_unchanged(&InferredType::known(
            "#{#a => Integer}"
        )));
        assert!(!ffi_arg_passes_unchanged(&InferredType::known(
            "#{#a => Integer}"
        )));
    }

    /// BT-4485: the FFI pass-through rule matches the shared fixture the
    /// runtime's `beamtalk_erlang_proxy_tests` checks sample values against.
    /// The class lists must be exactly the fixture's types (no type accepted
    /// here without runtime samples), and every `not_proven` type is rejected.
    #[test]
    fn ffi_passthrough_matches_shared_fixture() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(
            "../../runtime/apps/beamtalk_runtime/test/fixtures/ffi_passthrough_conformance.json",
        );
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read fixture {}: {e}", path.display()));
        let fixture: serde_json::Value = serde_json::from_str(&raw).expect("fixture is JSON");
        let types = |key: &str| -> Vec<String> {
            fixture[key]
                .as_array()
                .unwrap_or_else(|| panic!("fixture key {key}"))
                .iter()
                .map(|v| {
                    v.get("type")
                        .unwrap_or(v)
                        .as_str()
                        .expect("type name")
                        .to_string()
                })
                .collect()
        };
        let ty = |name: &str| InferredType::known(name);

        let result_ok = types("result_passthrough");
        let arg_ok = types("arg_passthrough");
        for name in &result_ok {
            assert!(ffi_result_passes_unchanged(&ty(name)), "result {name}");
        }
        for name in types("result_not_proven") {
            assert!(!ffi_result_passes_unchanged(&ty(&name)), "result {name}");
        }
        for name in &arg_ok {
            assert!(ffi_arg_passes_unchanged(&ty(name)), "arg {name}");
        }
        for name in types("arg_not_proven") {
            assert!(!ffi_arg_passes_unchanged(&ty(&name)), "arg {name}");
        }
        let guards: Vec<String> = fixture["arg_runtime_guards"]
            .as_array()
            .expect("arg_runtime_guards")
            .iter()
            .map(|v| v.as_str().expect("guard name").to_string())
            .collect();
        assert_eq!(
            guards, FFI_ARG_RUNTIME_GUARDS,
            "runtime guards match the fixture"
        );
        // Every class the rule names has runtime samples in the fixture.
        for class in FFI_SCALAR_CLASSES.iter().chain(FFI_RESULT_EXTRA_CLASSES) {
            assert!(
                result_ok.iter().any(|t| t == class),
                "no result samples for {class}"
            );
        }
        for class in FFI_SCALAR_CLASSES.iter().chain(FFI_ARG_EXTRA_CLASSES) {
            assert!(
                arg_ok.iter().any(|t| t == class),
                "no arg samples for {class}"
            );
        }
    }

    #[test]
    fn ffi_passthrough_unions_need_every_member() {
        let int_or_nil = InferredType::simple_union(&["Integer", "UndefinedObject"]);
        assert!(ffi_result_passes_unchanged(&int_or_nil));
        assert!(ffi_arg_passes_unchanged(&int_or_nil));
        let int_or_list = InferredType::simple_union(&["Integer", "List"]);
        assert!(!ffi_result_passes_unchanged(&int_or_list));
        assert!(!ffi_arg_passes_unchanged(&int_or_list));
        // `String` is proved as a result but not as an argument.
        let int_or_string = InferredType::simple_union(&["Integer", "String"]);
        assert!(ffi_result_passes_unchanged(&int_or_string));
        assert!(!ffi_arg_passes_unchanged(&int_or_string));
        let ok_or_other = InferredType::simple_union(&["#ok", "#other"]);
        assert!(!ffi_result_passes_unchanged(&ok_or_other));
        assert!(ffi_arg_passes_unchanged(&ok_or_other));
    }

    /// BT-4755: `binary()` reaches the checker as `String | Binary`, so both
    /// members must be proved for a binary result to skip the proxy. A
    /// `string()` result (`String | List`, a charlist) must not.
    #[test]
    fn binary_spec_result_is_proved_but_charlist_is_not() {
        let binary = InferredType::simple_union(&["String", "Binary"]);
        assert!(ffi_result_passes_unchanged(&binary));
        assert!(ffi_result_passes_unchanged(&InferredType::known("Binary")));
        assert!(ffi_result_passes_unchanged(&InferredType::known("String")));
        let charlist = InferredType::simple_union(&["String", "List"]);
        assert!(!ffi_result_passes_unchanged(&charlist));
        // Arguments keep the proxy: it unwraps a tagged String and retries
        // a binary as a charlist on `badarg`.
        assert!(!ffi_arg_passes_unchanged(&binary));
    }

    #[test]
    fn proves_no_result_coercion_reads_the_spec() {
        let mut registry = NativeTypeRegistry::new();
        registry.register_module(
            "erlang",
            vec![
                extracted_sig("monotonic_time", 0, vec![], InferredType::known("Integer")),
                extracted_sig(
                    "get",
                    1,
                    vec![],
                    InferredType::Dynamic(DynamicReason::DynamicSpec),
                ),
            ],
        );
        registry.trust_all_modules();
        assert!(registry.proves_no_result_coercion("erlang", "monotonic_time", 0));
        assert!(!registry.proves_no_result_coercion("erlang", "get", 1));
        assert!(!registry.proves_no_result_coercion("erlang", "monotonic_time", 1));
        assert!(!registry.proves_no_result_coercion("lists", "reverse", 1));
        assert_eq!(
            registry.proved_no_coercion_keys(),
            vec!["erlang:monotonic_time/0"]
        );

        // A hand-written stub never removes a coercion.
        let mut stubbed = NativeTypeRegistry::new();
        let mut stub = extracted_sig("answer", 0, vec![], InferredType::known("Integer"));
        stub.provenance = TypeProvenance::Declared(crate::span::Span::new(0, 0));
        stubbed.register_module("native_mod", vec![stub]);
        stubbed.trust_all_modules();
        assert!(!stubbed.proves_no_result_coercion("native_mod", "answer", 0));
        assert!(stubbed.proved_no_coercion_keys().is_empty());
    }

    #[test]
    fn untrusted_extracted_spec_is_not_proved() {
        let mut registry = NativeTypeRegistry::new();
        registry.register_module(
            "dep_mod",
            vec![extracted_sig(
                "f",
                0,
                vec![],
                InferredType::known("Integer"),
            )],
        );
        assert!(!registry.proves_no_result_coercion("dep_mod", "f", 0));
        assert!(registry.proved_no_coercion_keys().is_empty());
        registry.trust_all_modules();
        assert!(registry.proves_no_result_coercion("dep_mod", "f", 0));
    }

    #[test]
    fn merge_keeps_result_trust_from_the_winning_side() {
        let sig = || {
            vec![extracted_sig(
                "f",
                0,
                vec![],
                InferredType::known("Integer"),
            )]
        };
        let mut trusted = NativeTypeRegistry::new();
        trusted.register_module("shared", sig());
        trusted.trust_all_modules();
        let mut untrusted = NativeTypeRegistry::new();
        untrusted.register_module("shared", sig());
        untrusted.register_module("only_dep", sig());

        // Trusted side wins the collision: keeps trust; new module stays untrusted.
        trusted.merge(untrusted.clone());
        assert!(trusted.proves_no_result_coercion("shared", "f", 0));
        assert!(!trusted.proves_no_result_coercion("only_dep", "f", 0));

        // Untrusted side wins: no trust; trusted-only module is carried with trust.
        let mut base = NativeTypeRegistry::new();
        base.register_module("shared", sig());
        let mut other = NativeTypeRegistry::new();
        other.register_module("native_mod", sig());
        other.trust_all_modules();
        base.merge(other);
        assert!(!base.proves_no_result_coercion("shared", "f", 0));
        assert!(base.proves_no_result_coercion("native_mod", "f", 0));
    }

    #[test]
    fn stub_upsert_into_trusted_module_keeps_other_proofs() {
        let mut registry = NativeTypeRegistry::new();
        registry.register_module(
            "m",
            vec![
                extracted_sig("a", 0, vec![], InferredType::known("Integer")),
                extracted_sig("b", 0, vec![], InferredType::known("Integer")),
            ],
        );
        registry.trust_all_modules();
        let mut stub = extracted_sig("b", 0, vec![], InferredType::known("Integer"));
        stub.provenance = TypeProvenance::Declared(crate::span::Span::new(0, 0));
        registry.upsert_functions("m", vec![stub]);
        assert!(registry.proves_no_result_coercion("m", "a", 0));
        assert!(!registry.proves_no_result_coercion("m", "b", 0));
    }

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

    // ── upsert_functions / apply_overrides ───────────────────────────────────

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

    // ── detect_stub_drift ─────────────────────────────────────────────────────

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

    /// BT-4574: a module with no `-spec`s (e.g. `jose_jwk`) has no
    /// signatures, only exports. A stub for an exported-but-unspec'd function
    /// is not drift; a stub for a function the `.beam` doesn't export still is.
    #[test]
    fn detect_stub_drift_accepts_exports_without_specs() {
        let mut auto_extract = NativeTypeRegistry::new();
        auto_extract.register_module(
            "jose_jwk",
            vec![extracted_sig(
                "from_map",
                1,
                vec![param("map", "Dictionary")],
                InferredType::known("Tuple"),
            )],
        );
        auto_extract.register_module_exports(
            "jose_jwk",
            [
                ("to_public".to_string(), 1),
                ("generate_key".to_string(), 1),
            ],
        );

        let mut stubs = NativeTypeRegistry::new();
        stubs.register_module(
            "jose_jwk",
            vec![
                declared_sig("from_map", 1, InferredType::known("Tuple")),
                declared_sig("to_public", 1, InferredType::known("Tuple")),
                declared_sig("generate_key", 1, InferredType::known("Tuple")),
                // Right name, wrong arity: not exported.
                declared_sig("to_public", 2, InferredType::known("Tuple")),
            ],
        );

        let drift = auto_extract.detect_stub_drift(&stubs);

        assert_eq!(drift.len(), 1);
        assert_eq!(drift[0].0, "jose_jwk");
        assert_eq!(drift[0].1.name, "to_public");
        assert_eq!(drift[0].1.arity, 2);
        // Export-only entries add no type information.
        assert!(auto_extract.lookup("jose_jwk", "to_public", 1).is_none());
        assert!(auto_extract.exports_function("jose_jwk", "to_public", 1));
    }

    /// A module with exports but no signatures at all still counts as known,
    /// so a stub for a function it doesn't export warns.
    #[test]
    fn detect_stub_drift_flags_unexported_function_of_specless_module() {
        let mut auto_extract = NativeTypeRegistry::new();
        auto_extract.register_module("jose_jwk", vec![]);
        auto_extract.register_module_exports("jose_jwk", [("to_public".to_string(), 1)]);

        let mut stubs = NativeTypeRegistry::new();
        stubs.register_module(
            "jose_jwk",
            vec![
                declared_sig("to_public", 1, InferredType::known("Tuple")),
                declared_sig("bogus", 0, InferredType::known("Tuple")),
            ],
        );

        let drift = auto_extract.detect_stub_drift(&stubs);

        assert_eq!(drift.len(), 1);
        assert_eq!(drift[0].1.name, "bogus");
    }

    #[test]
    fn merge_keeps_exports_with_the_winning_module() {
        let mut otp = NativeTypeRegistry::new();
        otp.register_module("shared", vec![]);
        otp.register_module_exports("shared", [("otp_fn".to_string(), 0)]);

        let mut dep = NativeTypeRegistry::new();
        dep.register_module("shared", vec![]);
        dep.register_module_exports("shared", [("dep_fn".to_string(), 0)]);
        dep.register_module("dep_only", vec![]);
        dep.register_module_exports("dep_only", [("f".to_string(), 1)]);

        otp.merge(dep);

        assert!(otp.exports_function("shared", "otp_fn", 0));
        assert!(!otp.exports_function("shared", "dep_fn", 0));
        assert!(otp.exports_function("dep_only", "f", 1));
    }
}
