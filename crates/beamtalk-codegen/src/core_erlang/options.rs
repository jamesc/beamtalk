// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code generation options and results.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! [`CodegenOptions`] is the builder callers use to configure a
//! [`generate_module`](super::generate_module) call; [`GeneratedModule`] is
//! its warnings-carrying result.

use crate::core_erlang::primitive_bindings::PrimitiveBindingTable;
use beamtalk_core::source_analysis::Diagnostic;
use ecow::EcoString;

/// Options for Core Erlang code generation.
///
/// Replaces the combinatorial explosion of `generate_with_*` functions
/// with a single options struct. Use [`CodegenOptions::new`] to create
/// default options, then chain builder methods to customize.
///
/// # Example
///
/// ```no_run
/// use beamtalk_codegen::core_erlang::{CodegenOptions, generate_module};
/// use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let code = generate_module(&module, CodegenOptions::new("counter")
///     .with_source("value := 0")
///     .with_workspace_mode(true))?;
/// # Ok::<(), beamtalk_codegen::core_erlang::CodeGenError>(())
/// ```
#[derive(Debug, Clone)]
pub struct CodegenOptions {
    /// The Erlang module name to generate (ref-counted for O(1) clone).
    pub(in crate::core_erlang) module_name: EcoString,
    /// Original source text for `CompiledMethod` introspection.
    pub(in crate::core_erlang) source_text: Option<String>,
    /// Primitive binding table from compiled stdlib (ADR 0007).
    pub(in crate::core_erlang) bindings: Option<PrimitiveBindingTable>,
    /// Whether workspace bindings are available (REPL/workspace context).
    pub(in crate::core_erlang) workspace_mode: bool,
    /// Class name → compiled module name index for resolving cross-file class
    /// references in package mode.
    ///
    /// When populated, `compiled_module_name` checks this map first before
    /// falling back to the heuristic prefix approach. This allows classes in
    /// package subdirectories (e.g. `bt@pkg@sub@dir@class`) to be resolved
    /// correctly by all files in the package, regardless of where the caller
    /// lives in the directory tree.
    pub(in crate::core_erlang) class_module_index: std::collections::HashMap<String, String>,
    /// Class name → direct superclass name for all classes across all files.
    ///
    /// Populated during Pass 1 of package compilation alongside `class_module_index`.
    /// Used to enrich the per-file `ClassHierarchy` with cross-file inheritance
    /// information so that `is_actor_class` can resolve the full superclass chain
    /// even when the parent class is defined in another file.
    pub(in crate::core_erlang) class_superclass_index: std::collections::HashMap<String, String>,
    /// Source file path to embed as `beamtalk_source` module attribute.
    ///
    /// When set, the generated Core Erlang module includes:
    ///   `'beamtalk_source' = ["path/to/file.bt"]`
    /// This survives workspace restarts and is the definitive source of truth
    /// for `Behaviour >> sourceFile`. Absent for stdlib and `ClassBuilder` classes.
    pub(in crate::core_erlang) source_path: Option<String>,
    /// Whether this module is being compiled in stdlib mode.
    ///
    /// When true, the generated `register_class/0` emits `stdlibMode => true` in
    /// the builder state map, which tells `beamtalk_class_builder:register/1` to
    /// bypass the sealed-superclass check. This allows stdlib classes like Character
    /// (which extends sealed Integer) to load correctly via their `on_load` hooks.
    pub(in crate::core_erlang) stdlib_mode: bool,
    /// ADR 0050 Phase 4: pre-loaded class entries from BEAM metadata.
    /// Injected into the `ClassHierarchy` before codegen so user-defined REPL
    /// classes are visible to `is_actor_class` and related checks.
    pub(in crate::core_erlang) pre_class_hierarchy:
        Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// Override for codegen diagnostics flag.
    /// `None` = read from `BEAMTALK_CODEGEN_DIAGNOSTICS` env var at generator creation.
    /// `Some(true/false)` = override the env var (used by tests).
    pub(in crate::core_erlang) codegen_diagnostics: Option<bool>,
    /// ADR 0098 Phase 3: producing `BEAMTALK_VERSION` to bake into `__beamtalk_meta`.
    /// Set by the CLI via [`CodegenOptions::with_provenance`]; absent for REPL/tests.
    pub(in crate::core_erlang) beamtalk_version: Option<String>,
    /// ADR 0098 Phase 3: producing compound OTP version (`<release>-<erts>`) to
    /// bake into `__beamtalk_meta`. Set alongside `beamtalk_version`.
    pub(in crate::core_erlang) otp_release: Option<String>,
    /// Optional FFI type registry (ADR 0075) threaded to the
    /// return-type writeback pass so methods whose body type is inferred
    /// purely via an FFI call (e.g. `foo => Erlang lists reverse: x`) get
    /// `List` written back to `method_return_types` before codegen.
    pub(in crate::core_erlang) native_type_registry:
        Option<std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>>,
    /// Type alias declarations (`type Name = ...`) from other
    /// modules in the same compilation unit — the codegen counterpart of
    /// `AnalysisContext`/`ClassHierarchyContext`'s `pre_loaded_aliases`.
    /// Merged with this module's own `module.type_aliases` via
    /// `AliasRegistry::from_module_declarations_with_pre_loaded` so a
    /// cross-module alias reference resolves to a `user_type` reference in
    /// generated `-spec`/`-type` attributes instead of falling through to
    /// `any()`.
    pub(in crate::core_erlang) pre_loaded_aliases:
        Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    /// Pre-computed analysis outputs from the driver's own
    /// `analyse_full` call. See [`Self::with_analysis`].
    pub(in crate::core_erlang) analysis: Option<beamtalk_core::semantic_analysis::AnalysisResult>,
}

impl CodegenOptions {
    /// Creates default options with the given module name.
    pub fn new(module_name: &str) -> Self {
        Self {
            module_name: EcoString::from(module_name),
            source_text: None,
            bindings: None,
            workspace_mode: false,
            class_module_index: std::collections::HashMap::new(),
            class_superclass_index: std::collections::HashMap::new(),
            source_path: None,
            stdlib_mode: false,
            pre_class_hierarchy: Vec::new(),
            codegen_diagnostics: None,
            beamtalk_version: None,
            otp_release: None,
            native_type_registry: None,
            pre_loaded_aliases: Vec::new(),
            analysis: None,
        }
    }

    /// ADR 0098 Phase 3: set the producing-toolchain identity baked into each
    /// module's `__beamtalk_meta/0` map. `beamtalk_version` is the full
    /// `BEAMTALK_VERSION`; `otp_release` is the compound `<release>-<erts>` key
    /// (the same the build stamp uses), `None` when OTP could not be probed.
    #[must_use]
    pub fn with_provenance(mut self, beamtalk_version: &str, otp_release: Option<&str>) -> Self {
        self.beamtalk_version = Some(beamtalk_version.to_string());
        self.otp_release = otp_release.map(String::from);
        self
    }

    /// Sets the source text for `CompiledMethod` introspection.
    #[must_use]
    pub fn with_source(mut self, source: &str) -> Self {
        self.source_text = Some(source.to_string());
        self
    }

    /// Sets the source text from an optional value.
    #[must_use]
    pub fn with_source_opt(mut self, source: Option<&str>) -> Self {
        self.source_text = source.map(String::from);
        self
    }

    /// Sets the primitive binding table (ADR 0007).
    #[must_use]
    pub fn with_bindings(mut self, bindings: PrimitiveBindingTable) -> Self {
        self.bindings = Some(bindings);
        self
    }

    /// Enables or disables workspace mode (ADR 0010 / ADR 0019).
    #[must_use]
    pub fn with_workspace_mode(mut self, enabled: bool) -> Self {
        self.workspace_mode = enabled;
        self
    }

    /// Explicitly enable or disable codegen diagnostics, overriding the env var.
    #[must_use]
    pub fn with_codegen_diagnostics(mut self, enabled: bool) -> Self {
        self.codegen_diagnostics = Some(enabled);
        self
    }

    /// Sets the class module index for resolving cross-file class references.
    ///
    /// Maps Beamtalk class names (e.g. `"SchemeEnv"`) to their compiled Erlang
    /// module names (e.g. `"bt@sicp_example@scheme@env"`). When set, these
    /// mappings take precedence over the heuristic prefix approach in
    /// `compiled_module_name`, fixing subdirectory class dispatch.
    #[must_use]
    pub fn with_class_module_index(
        mut self,
        index: std::collections::HashMap<String, String>,
    ) -> Self {
        self.class_module_index = index;
        self
    }

    /// Sets the class superclass index for resolving cross-file inheritance.
    ///
    /// Maps Beamtalk class names to their direct superclass names. Used to
    /// enrich the per-file hierarchy so that `is_actor_class` can determine
    /// the correct codegen context for classes whose parents are in other files.
    #[must_use]
    pub fn with_class_superclass_index(
        mut self,
        index: std::collections::HashMap<String, String>,
    ) -> Self {
        self.class_superclass_index = index;
        self
    }

    /// ADR 0050 Phase 4: pre-load user-class entries from BEAM metadata into
    /// the `CodegenOptions` so `generate_module` injects them into the hierarchy.
    #[must_use]
    pub fn with_class_hierarchy(
        mut self,
        classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    ) -> Self {
        self.pre_class_hierarchy = classes;
        self
    }

    /// Sets the source file path from an optional value.
    #[must_use]
    pub fn with_source_path_opt(mut self, path: Option<&str>) -> Self {
        self.source_path = path.map(String::from);
        self
    }

    /// Enables stdlib mode: generated `register_class/0` emits `stdlibMode => true`
    /// so the runtime bypasses the sealed-superclass check for stdlib loading.
    #[must_use]
    pub fn with_stdlib_mode(mut self, enabled: bool) -> Self {
        self.stdlib_mode = enabled;
        self
    }

    /// Sets the native FFI type registry (ADR 0075) used by the
    /// return-type writeback pass, from an optional value.
    #[must_use]
    pub fn with_native_type_registry(
        mut self,
        registry: Option<
            std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
        >,
    ) -> Self {
        self.native_type_registry = registry;
        self
    }

    /// Sets pre-loaded type alias declarations from other modules
    /// in the same compilation unit, so `generate_module` can resolve a
    /// cross-module alias reference to a `user_type` reference in generated
    /// `-spec`/`-type` attributes instead of falling through to `any()`.
    /// Mirrors [`Self::with_class_hierarchy`]'s pre-loaded-metadata shape.
    #[must_use]
    pub fn with_pre_loaded_aliases(
        mut self,
        aliases: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    ) -> Self {
        self.pre_loaded_aliases = aliases;
        self
    }

    /// Threads a driver's already-computed [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult)
    /// into codegen, so `generate_module`/`generate_module_with_warnings` consume
    /// the same class hierarchy, semantic facts, and inferred method return types
    /// the driver's own `analyse_full` call already produced for diagnostics,
    /// instead of re-deriving all three from scratch (see ADR 0006).
    ///
    /// `None` (the default) preserves the previous self-sufficient behaviour —
    /// codegen computes its own analysis internally, including running the
    /// pre-codegen writeback trio (`semantic_analysis::lower_module_for_codegen`)
    /// on its own clone of `module`. Callers that already run `analyse_full`
    /// before codegen (e.g. the CLI build pipeline via
    /// `compile_source_with_bindings`) should always supply it here to avoid
    /// running the type checker twice per compiled module.
    ///
    /// **Hand-off contract:** when supplying `Some`, the caller is expected to
    /// have already called [`beamtalk_core::semantic_analysis::lower_module_for_codegen`]
    /// on its own `module` — using this same `analysis.class_hierarchy` and
    /// `analysis.method_return_types` — *before* passing `module` to
    /// `generate_module`. Codegen trusts that hand-off (skipping the writeback
    /// trio itself, and the `module.clone()` it used to require) whenever no
    /// cross-file enrichment from `with_class_hierarchy`/
    /// `with_class_superclass_index` adds anything the driver's own hierarchy
    /// didn't already have; codegen still prepares the AST itself in the
    /// rarer case that enrichment invalidates the hand-off. Skipping the
    /// `lower_module_for_codegen` call while still supplying `Some` silently
    /// produces a module missing inferred return types / corrected
    /// `class_kind` / `supervisor_kind` in the common case — see
    /// `generate_module_with_warnings`'s hand-off-contract comment.
    #[must_use]
    pub fn with_analysis(
        mut self,
        analysis: beamtalk_core::semantic_analysis::AnalysisResult,
    ) -> Self {
        self.analysis = Some(analysis);
        self
    }
}

/// Result of code generation including diagnostic warnings.
///
/// Returned by [`generate_module_with_warnings`]. Callers that need to surface
/// warnings (e.g., stateful blocks at Erlang boundaries) should use that function.
/// Callers that only need the generated code can use [`generate_module`] instead.
#[derive(Debug)]
pub struct GeneratedModule {
    /// The generated Core Erlang code.
    pub code: String,
    /// Diagnostic warnings emitted during code generation.
    ///
    /// Each entry is a structured [`Diagnostic`] with severity, source span, and
    /// message. Examples:
    /// - A stateful Beamtalk block was passed to an Erlang call site — mutations
    ///   inside the block will be silently dropped since Erlang cannot propagate
    ///   the updated `StateAcc` back to the Beamtalk caller.
    pub warnings: Vec<Diagnostic>,
}
