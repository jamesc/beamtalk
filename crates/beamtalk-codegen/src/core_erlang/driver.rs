// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code generation driver — the entry points that turn a [`Module`] into
//! Core Erlang source text.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! [`generate_module`] and [`generate_module_with_warnings`] (the
//! analysis-to-codegen handoff decision tree) drive a single module's
//! generation; [`generate`] is the default-options convenience wrapper. The
//! two [`CoreErlangGenerator`]-associated helpers only the driver calls
//! (`compute_direct_call_eligible`, `is_actor_class`) live here too.

use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::{
    CodeGenError, CodegenOptions, DirectCallClassInfo, GeneratedModule, Result,
};
use beamtalk_core::ast::{ClassKind, Module};
use ecow::EcoString;

/// Generates Core Erlang code from a Beamtalk module.
///
/// This is the main entry point for code generation. It transforms
/// the parsed AST into Core Erlang text that can be compiled by `erlc`.
///
/// # BT-213: Value Types vs Actors
///
/// Routes to different code generators based on class hierarchy:
/// - **Actor subclasses** → `generate_actor_module` (`gen_server` with mailbox)
/// - **Object subclasses** → `generate_value_type_module` (plain Erlang maps)
///
/// # Errors
///
/// Returns [`CodeGenError`] if:
/// - The module uses unsupported features
/// - Code generation encounters an internal error
/// - Formatting fails
///
/// # Example
///
/// ```no_run
/// use beamtalk_codegen::core_erlang::{CodegenOptions, generate_module};
/// use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
///
/// # let module = Module::new(Vec::new(), Span::new(0, 0));
/// let core_erlang = generate_module(&module, CodegenOptions::new("counter"))?;
/// println!("{}", core_erlang);
/// # Ok::<(), beamtalk_codegen::core_erlang::CodeGenError>(())
/// ```
pub fn generate_module(module: &Module, options: CodegenOptions) -> Result<String> {
    generate_module_with_warnings(module, options).map(|m| m.code)
}

/// Generates Core Erlang for a module, returning the code and any diagnostic warnings.
///
/// Like [`generate_module`] but also returns warnings emitted during generation.
/// Use this when you need to surface warnings (e.g., for IDE diagnostics or compiler output).
///
/// # Errors
///
/// Returns [`CodeGenError`] if:
/// - The module uses unsupported features
/// - Code generation encounters an internal error
/// - Formatting fails
pub fn generate_module_with_warnings(
    module: &Module,
    options: CodegenOptions,
) -> Result<GeneratedModule> {
    let mut generator = if let Some(bindings) = options.bindings {
        CoreErlangGenerator::with_bindings(&options.module_name, bindings)
    } else {
        CoreErlangGenerator::new(&options.module_name)
    };
    generator.source_text = options.source_text;
    generator.set_workspace_mode(options.workspace_mode);
    generator.set_stdlib_mode(options.stdlib_mode);
    generator.set_class_module_index(options.class_module_index);
    generator.source_path = options.source_path;
    // ADR 0098 Phase 3: bake the producing-toolchain identity into `__beamtalk_meta`.
    generator.beamtalk_version = options.beamtalk_version.map(EcoString::from);
    generator.otp_release = options.otp_release.map(EcoString::from);
    // BT-1343: Override codegen diagnostics flag if explicitly set in options.
    if let Some(enabled) = options.codegen_diagnostics {
        generator.codegen_diagnostics_enabled = enabled;
    }

    // BT-3123: Consume the driver's already-computed analysis when supplied
    // (`CodegenOptions::with_analysis`) instead of re-deriving semantic facts,
    // the class hierarchy, and inferred method return types from scratch —
    // eliminating a second full type-checking pass per compiled module. `None`
    // preserves the previous self-sufficient behaviour for callers that don't
    // run analysis separately (unit tests, ad-hoc codegen).
    let (mut hierarchy, analysis_handed_off, mut driver_method_return_types) =
        if let Some(analysis) = options.analysis {
            generator.semantic_facts = analysis.semantic_facts;
            // BT-3217: carry the driver's already-computed `TypeMap` through
            // for `recv_type` projection. May be superseded below if this
            // generation's own cross-file enrichment invalidates the
            // hand-off and forces a fuller re-inference pass.
            generator.type_map = analysis.type_map;
            (
                analysis.class_hierarchy,
                true,
                Some(analysis.method_return_types),
            )
        } else {
            // BT-1288: Compute semantic facts before codegen begins.
            generator.semantic_facts =
                beamtalk_core::semantic_analysis::compute_semantic_facts(module);

            // Build hierarchy once for the entire generation (ADR 0006)
            let (hierarchy_result, _) =
                beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(module);
            let hierarchy = hierarchy_result
                .map_err(|e| CodeGenError::Internal(format!("hierarchy: {e:?}")))?;
            (hierarchy, false, None)
        };

    // ADR 0050 Phase 4: inject richer user-class entries from BEAM metadata first,
    // so that add_external_superclasses (which uses contains_key before inserting)
    // does not overwrite BEAM data with partial stubs. Both calls are no-ops for
    // classes the handed-off analysis hierarchy already carries (BT-1523/BT-894
    // insert only into vacant entries) — the common case, since a driver that
    // hands off analysis typically fed the same cross-file class metadata to
    // `AnalysisContext::with_pre_loaded_classes`. `class_superclass_index`
    // (BT-894) is codegen-only and has no `AnalysisContext` counterpart, so it
    // can still add genuinely new stub entries analysis never saw; both calls
    // report whether they did so the lowering step below knows whether the
    // handed-off AST preparation is still trustworthy.
    let added_beam_meta = hierarchy.add_from_beam_meta(options.pre_class_hierarchy);

    // BT-894: Backfill missing cross-file superclass stubs (only for classes not
    // already present from build() or BEAM metadata).
    let added_superclasses = hierarchy.add_external_superclasses(&options.class_superclass_index);

    // BT-3125: A driver that handed off `AnalysisResult` is expected to have
    // already called `semantic_analysis::lower_module_for_codegen` on its own
    // module — using the very same `class_hierarchy`/`method_return_types` —
    // *before* invoking `generate_module`, per `CodegenOptions::with_analysis`'s
    // contract. When that hand-off is still trustworthy (no cross-file
    // enrichment above added anything the driver's own hierarchy didn't have),
    // codegen no longer schedules the writeback trio itself: it trusts the
    // already-prepared `module` it was given and skips the clone entirely.
    //
    // Two cases still require preparing the AST here, exactly as before
    // BT-3125: no analysis was handed off at all (self-sufficient codegen —
    // unit tests, ad-hoc codegen, REPL trace mode), or this generation's own
    // cross-file enrichment (above) added stub classes the driver's
    // `lower_module_for_codegen` call never saw, making its writeback
    // possibly incomplete for this generation's fuller view of the hierarchy.
    let mut module_owned;
    let module: &Module = if analysis_handed_off && !added_beam_meta && !added_superclasses {
        // BT-3249: `module` is used exactly as the driver prepared it (no
        // clone/re-infer below), so the driver's own `method_return_types`
        // map is precisely "which methods did inference write a return type
        // into" for *this* `module` — record it for `extract_method_source`
        // to strip before baking image-resident `__source__` text. `.take()`
        // rather than `.clone()`: `driver_method_return_types` is only read
        // again in the `else` arm below, which this branch never executes.
        generator.method_return_types_written_back =
            driver_method_return_types.take().unwrap_or_default();
        module
    } else {
        module_owned = module.clone();
        // A driver that handed off `AnalysisResult` already wrote its
        // (narrower-hierarchy) inference into `module_owned` before we got
        // here — but `added_beam_meta`/`added_superclasses` just proved that
        // hand-off stale. Undo exactly those driver-written entries before
        // re-inferring: both `infer_method_return_types` (via
        // `resolve_self_delegate_return_type`, which trusts an
        // already-populated `return_type` as a declared annotation) and
        // `apply_return_type_writeback_from_map` only fill in a `None`
        // `return_type`, so without this the "fuller hierarchy" recompute
        // below would silently be a no-op for every method the driver's
        // pass already answered. `written_by` only ever contains
        // inference-derived keys, so this can never clear a genuine user
        // annotation (see `clear_return_type_writeback_for_keys`'s doc).
        if let Some(written_by) = &driver_method_return_types {
            beamtalk_core::semantic_analysis::clear_return_type_writeback_for_keys(
                &mut module_owned,
                written_by,
            );
        }
        // BT-3217 (ADR 0115 Phase 2 spike §1d): `infer_types_and_returns`
        // returns both `TypeMap` and `method_return_types` from the same
        // single `TypeChecker` pass `infer_method_return_types` already ran
        // — zero extra inference. Refreshes `generator.type_map` for this
        // (possibly fuller, re-inferred) hierarchy, superseding whatever the
        // `Some(analysis)` branch above set from the driver's now-stale
        // hand-off.
        let (type_map, method_return_types) =
            beamtalk_core::semantic_analysis::type_checker::infer_types_and_returns(
                &module_owned,
                &hierarchy,
                options.native_type_registry.as_deref(),
            );
        generator.type_map = type_map;
        beamtalk_core::semantic_analysis::lower_module_for_codegen(
            &mut module_owned,
            &hierarchy,
            &method_return_types,
        );
        // BT-3249: record which methods *this* (re-)inference wrote a
        // return type into, for `extract_method_source` to strip before
        // emitting image-resident `__source__` text — see the field's doc.
        generator.method_return_types_written_back = method_return_types;
        &module_owned
    };

    // BT-2932: build the alias registry once, merging this module's own
    // `type_aliases` with any pre-loaded aliases from other modules in the
    // same compilation unit, so a cross-module alias reference resolves to
    // a `user_type` reference instead of falling through to `any()` in
    // generated `-spec`/`-type` attributes.
    generator.alias_registry =
        beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::from_module_declarations_with_pre_loaded(
            module,
            &options.pre_loaded_aliases,
        );

    // ADR 0065 / BT-1457: Set Server subclass flag for handle_info codegen dispatch.
    if let Some(class) = module.classes.first() {
        generator.is_server_subclass = hierarchy.is_server_subclass(&class.name.name);
    }

    // BT-1639: Pre-compute direct-call eligible class methods from the hierarchy.
    // For sealed classes with no class variables, their class methods can be called
    // directly (bypassing gen_server dispatch). This is safe because the methods
    // are pure functions that don't mutate class state.
    generator.direct_call_eligible =
        CoreErlangGenerator::compute_direct_call_eligible(&hierarchy, &generator);

    // BT-1951: Stash the hierarchy for use by actor callback generation
    // (auto-chained initialize dispatch in handle_continue and inherited
    // typed-no-default field validation).
    generator.class_hierarchy = Some(hierarchy.clone());

    // BT-213: Route based on whether class is actor or value type
    let doc = if CoreErlangGenerator::is_actor_class(module, &hierarchy) {
        generator.generate_actor_module(module)?
    } else {
        generator.generate_value_type_module(module)?
    };

    Ok(GeneratedModule {
        code: doc.to_pretty_string(),
        warnings: generator.codegen_warnings,
    })
}

/// Generates Core Erlang code with default module name `bt_module`.
///
/// Convenience wrapper around [`generate_module`] for simple use cases.
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate(module: &Module) -> Result<String> {
    generate_module(module, CodegenOptions::new("bt_module"))
}

impl CoreErlangGenerator {
    /// BT-213: Determines if a class is an actor (process-based) or value type (plain term).
    /// BT-1639: Computes the set of sealed classes whose class methods are eligible
    /// for direct calls (bypassing `gen_server` dispatch).
    ///
    /// A class method is eligible when all four conditions hold:
    /// 1. The class is sealed (all methods visible at compile time)
    /// 2. The class has no class variables (no state to mutate)
    /// 3. The method is a class method (not instance-side)
    /// 4. The selector is not a supervisor constructor (`startLink`, `startLink:`)
    ///
    /// Returns a mapping from class name to `DirectCallClassInfo` with the module
    /// name and set of eligible selectors.
    pub(in crate::core_erlang) fn compute_direct_call_eligible(
        hierarchy: &beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy,
        generator: &CoreErlangGenerator,
    ) -> std::collections::HashMap<String, DirectCallClassInfo> {
        // Selectors that depend on gen_server process state and must NOT be
        // called directly: supervisor constructors (`startLink` family) and
        // `basicNew`/`basicNewWith` constructors (`new`/`new:`) which read
        // `beamtalk_class_name`/`beamtalk_class_module` from the process dictionary.
        let excluded_selectors: std::collections::HashSet<&str> =
            ["startLink", "startLink:", "new", "new:"]
                .into_iter()
                .collect();
        let mut result = std::collections::HashMap::new();

        for (class_name, class_info) in hierarchy.classes() {
            // Gate 1: Class must be sealed
            if !class_info.is_sealed {
                continue;
            }
            // Gate 2: Class must have no class variables
            if !class_info.class_variables.is_empty() {
                continue;
            }
            // Gate 3: Class must have class methods
            //
            // BT-3435 (ADR 0119 Context): this is the only place that
            // iterates *every* hierarchy class unconditionally, including
            // `Future` (a runtime-only builtin with no `stdlib/src/Future.bt`
            // source — see `class_hierarchy/builtins.rs`). `Future`'s
            // hardcoded `ClassInfo` always has empty `class_methods` today,
            // so this gate is what keeps it from reaching
            // `compiled_module_name` here and emitting a reference to a
            // nonexistent `bt@...` module. If BT-507 ever gives `Future` (or
            // another no-`.bt`-source builtin) real class methods, this gate
            // stops protecting it and its module resolution needs a real
            // registry answer (`ClassModuleRegistry`'s `ModuleName::Native`
            // variant exists for exactly this) — not implemented
            // speculatively now, since no current call site resolves
            // `Future`'s module at all.
            if class_info.class_methods.is_empty() {
                continue;
            }

            let mut selectors = std::collections::HashSet::new();
            for method in &class_info.class_methods {
                // Gate 4: Skip selectors that depend on gen_server process state
                if excluded_selectors.contains(method.selector.as_str()) {
                    continue;
                }
                // Gate 5: Only optimize `class sealed` methods (is_sealed=true).
                // Non-sealed class methods may reference `self` (the class object)
                // for factory patterns or delegation, which would break with nil ClassSelf.
                if !method.is_sealed {
                    continue;
                }
                selectors.insert(method.selector.to_string());
            }

            if !selectors.is_empty() {
                let module_name = EcoString::from(generator.compiled_module_name(class_name));
                result.insert(
                    class_name.to_string(),
                    DirectCallClassInfo {
                        module_name,
                        selectors,
                    },
                );
            }
        }

        result
    }

    ///
    /// **Actor classes:** Inherit from Actor or its subclasses. Generate `gen_server` code.
    /// **Value types:** Inherit from Object or Value (but not Actor). Generate plain Erlang
    /// maps/records via `generate_value_type_module`.
    ///
    /// # Implementation Note
    ///
    /// BT-3086: Delegates to `ClassHierarchy::resolve_class_kind`, the single authority for
    /// actor/value classification (see its doc comment for the walk + default-to-`Object`
    /// policy on a fully-known chain). This used to be a third, independent implementation
    /// that re-walked the chain itself and consulted a hand-maintained list of "known value
    /// roots" (`Object`, `Exception`, `RuntimeError`, ...) that went stale every time the
    /// exception hierarchy grew. Both `ClassKind::Value` and `ClassKind::Object` route to
    /// `generate_value_type_module` here — the Value/Object distinction only matters for
    /// auto-slot codegen *within* the value-type path, not for actor-vs-value routing.
    ///
    /// The one case `resolve_class_kind` cannot see through is a genuinely incomplete
    /// ancestor chain — a superclass that isn't registered in this `ClassHierarchy` at all
    /// (e.g. compiling a subclass file independently of its parent). `resolve_class_kind`
    /// resolves that to `ClassKind::Object` (neither `Actor` nor `Value` literal is found),
    /// but codegen has historically defaulted such classes to *actor* instead, for backward
    /// compatibility with independent per-file compilation. `ClassHierarchy::has_cross_file_parent`
    /// is the existing, already-tested predicate for "this chain has an unregistered
    /// ancestor" — reused here rather than re-deriving the same fact from a hardcoded list.
    ///
    /// # Returns
    ///
    /// - `true` if class inherits from Actor anywhere in the (fully-known) chain
    /// - `true` if a concrete (non-abstract) Supervisor/DynamicSupervisor subclass (BT-1220)
    /// - `true` if the chain has an unregistered ancestor (incomplete-chain default, above)
    /// - `false` if class resolves to Value or Object on a fully-known chain
    /// - `true` if module contains no class (backward compatibility for REPL)
    pub(in crate::core_erlang) fn is_actor_class(
        module: &Module,
        hierarchy: &beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy,
    ) -> bool {
        let Some(class) = module.classes.first() else {
            return true;
        };
        // BT-1220: Concrete Supervisor/DynamicSupervisor subclasses use supervisor codegen,
        // routed through generate_actor_module which delegates to supervisor_codegen.
        // Abstract base classes (Supervisor, DynamicSupervisor themselves) remain value types.
        if class.supervisor_kind.is_some() && !class.is_abstract {
            return true;
        }
        let name = class.name.name.as_str();
        match hierarchy.resolve_class_kind(name) {
            ClassKind::Actor => true,
            ClassKind::Value => false,
            ClassKind::Object => hierarchy.has_cross_file_parent(name),
        }
    }
}
