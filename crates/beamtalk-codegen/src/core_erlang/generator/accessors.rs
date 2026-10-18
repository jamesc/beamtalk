// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Small accessor/mutator methods on [`CoreErlangGenerator`] — field
//! getters/setters and the context-default-providing wrappers around the
//! `Option<ReplContext>`/`Option<ClassContext>`/`Option<ValueTypeContext>`
//! fields.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::CodeGenContext;
use crate::core_erlang::class_meta;
use crate::core_erlang::field_types::{
    BareOperator, COMPARABLE_TYPES, FieldTypeFact, NUMERIC_TYPES, classify_fields,
};
use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::generator::{ClassContext, ReplContext, ValueTypeContext};
use crate::core_erlang::threaded_ir::ThreadedBodyKind;
use crate::core_erlang::util;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

impl CoreErlangGenerator {
    /// ADR 0098 Phase 3: the producing-toolchain identity to bake into
    /// `__beamtalk_meta`. Borrows the generator's version fields; both are `None`
    /// unless the CLI supplied them via [`CodegenOptions::with_provenance`].
    pub(in crate::core_erlang) fn meta_provenance(&self) -> class_meta::MetaProvenance<'_> {
        class_meta::MetaProvenance {
            beamtalk_version: self.beamtalk_version.as_deref(),
            otp_release: self.otp_release.as_deref(),
            direct_class_methods: &self.own_direct_class_methods,
        }
    }

    /// Returns `true` if REPL mode is active.
    // widened from `pub(crate)` — `beamtalk-repl` queries/sets this
    // around its own generation calls.
    pub fn is_repl_mode(&self) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.is_repl_mode)
    }

    /// Sets the REPL mode flag, initialising the context if absent.
    pub fn set_is_repl_mode(&mut self, value: bool) {
        self.repl_context_mut().is_repl_mode = value;
    }

    /// Returns `true` if REPL loop mutation tracking has been flagged.
    pub(in crate::core_erlang) fn repl_loop_mutated(&self) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.repl_loop_mutated)
    }

    /// Sets the REPL loop mutated flag, initialising the context if absent.
    pub(in crate::core_erlang) fn set_repl_loop_mutated(&mut self, value: bool) {
        self.repl_context_mut().repl_loop_mutated = value;
    }

    /// Marks `site` as the expression the REPL root unpacks a construct tuple
    /// from ([`ReplContext::repl_unpack_site`]), returning the previous mark.
    pub(in crate::core_erlang) fn set_repl_unpack_site(
        &mut self,
        site: Option<Span>,
    ) -> Option<Span> {
        std::mem::replace(&mut self.repl_context_mut().repl_unpack_site, site)
    }

    /// The span of the expression the REPL root unpacks a construct tuple from
    /// when it compiles `expr`: `expr` itself (parentheses peeled), or the
    /// value of an identifier assignment, which compiles to just its value.
    pub(in crate::core_erlang) fn repl_unpack_span(expr: &beamtalk_core::ast::Expression) -> Span {
        use beamtalk_core::ast::Expression;
        match expr.unwrap_parens() {
            Expression::Assignment { target, value, .. }
                if matches!(target.as_ref(), Expression::Identifier(_)) =>
            {
                value.unwrap_parens().span()
            }
            other => other.span(),
        }
    }

    /// Whether `span` is the REPL root's unpack site.
    pub(in crate::core_erlang) fn is_repl_unpack_site(&self, span: Span) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.repl_unpack_site == Some(span))
    }

    /// Returns a mutable reference to the REPL context, creating it if absent.
    pub(in crate::core_erlang) fn repl_context_mut(&mut self) -> &mut ReplContext {
        self.repl_context.get_or_insert_with(ReplContext::new)
    }

    /// Returns a reference to the class identity, if any.
    pub(in crate::core_erlang) fn class_identity(&self) -> Option<&util::ClassIdentity> {
        self.class_context
            .as_ref()
            .and_then(|ctx| ctx.class_identity.as_ref())
    }

    /// Sets the class identity, initialising the context if absent.
    pub(in crate::core_erlang) fn set_class_identity(
        &mut self,
        identity: Option<util::ClassIdentity>,
    ) {
        self.class_context_mut().class_identity = identity;
    }

    /// Returns a reference to the class variable names set.
    pub(in crate::core_erlang) fn class_var_names(&self) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.class_var_names)
    }

    /// Sets the class-side extension's target class (BT-3762), returning the
    /// previous value so the caller can restore it.
    pub(in crate::core_erlang) fn replace_extension_target_class(
        &mut self,
        value: Option<String>,
    ) -> Option<String> {
        std::mem::replace(&mut self.class_context_mut().extension_target_class, value)
    }

    /// The class whose class variables the current class method sees: the
    /// `ClassBuilder` class, else the extension target, else the class being
    /// compiled (BT-3762). The innermost scope wins (BT-3804): a builder
    /// `classMethods:` block nested in a class-side extension sees the
    /// builder's `classVars:`, not the extension target's variables.
    pub(in crate::core_erlang) fn class_var_home(&self) -> String {
        self.builder_class_method_class()
            .or_else(|| {
                self.class_context
                    .as_ref()
                    .and_then(|ctx| ctx.extension_target_class.clone())
            })
            .unwrap_or_else(|| self.class_name())
    }

    /// Returns a mutable reference to the class variable names set.
    pub(in crate::core_erlang) fn class_var_names_mut(
        &mut self,
    ) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().class_var_names
    }

    /// Returns a reference to the class method selectors set.
    pub(in crate::core_erlang) fn class_method_selectors(
        &self,
    ) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.class_method_selectors)
    }

    /// Returns a mutable reference to the class method selectors set.
    pub(in crate::core_erlang) fn class_method_selectors_mut(
        &mut self,
    ) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().class_method_selectors
    }

    /// Returns the selectors of `class sealed` class methods (cannot be overridden).
    pub(in crate::core_erlang) fn sealed_class_method_selectors(
        &self,
    ) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.sealed_class_method_selectors)
    }

    /// Mutable access to the `class sealed` class-method selector set.
    pub(in crate::core_erlang) fn sealed_class_method_selectors_mut(
        &mut self,
    ) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().sealed_class_method_selectors
    }

    /// Derives this generation unit's own [`PackageId`] from `self.module_name`
    /// (ADR 0119): `bt@stdlib@...` is [`PackageId::Stdlib`],
    /// `bt@{pkg}@...` is [`PackageId::Package`], anything else (a bare
    /// `bt@{snake}` or an unprefixed test-fixture name) is
    /// [`PackageId::SingleFile`]. Used both to key registry entries in
    /// [`Self::set_class_module_index`] and, on a registry miss, to compute
    /// the existing best-effort naming convention directly in
    /// `compiled_module_name` — replacing the deleted `user_package_prefix`,
    /// which re-derived the same package name by string-parsing an
    /// already-computed module name instead of typing it once here.
    ///
    /// [`PackageId`]: beamtalk_core::semantic_analysis::PackageId
    pub(in crate::core_erlang) fn own_package_id(
        &self,
    ) -> beamtalk_core::semantic_analysis::PackageId {
        use beamtalk_core::semantic_analysis::PackageId;
        match self
            .module_name
            .strip_prefix("bt@")
            .and_then(|rest| rest.split_once('@'))
        {
            Some(("stdlib", _)) => PackageId::Stdlib,
            Some((pkg, _)) => PackageId::Package(pkg.to_string()),
            None => PackageId::SingleFile,
        }
    }

    /// Returns a reference to the class → compiled module resolution
    /// registry for this generation unit (ADR 0119).
    pub(in crate::core_erlang) fn class_module_registry(
        &self,
    ) -> &beamtalk_core::semantic_analysis::ClassModuleRegistry {
        static EMPTY: std::sync::LazyLock<beamtalk_core::semantic_analysis::ClassModuleRegistry> =
            std::sync::LazyLock::new(beamtalk_core::semantic_analysis::ClassModuleRegistry::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.class_module_registry)
    }

    /// Sets the class module index, building the [`ClassModuleRegistry`]
    /// entries it backs (keyed under this unit's own [`PackageId`] — see
    /// [`Self::own_package_id`]) and initialising the context if absent.
    ///
    /// `index` may (per `beamtalk-cli`'s dependency-merge path) already
    /// contain a dependency package's own classes alongside this unit's own
    /// package — every entry is keyed under `own_package_id()` regardless,
    /// same as the flat `HashMap` this registry replaces did no keying at
    /// all. That is a harmless simplification for the exact-match lookup
    /// [`Self::compiled_module_name`] does (`module_for_class(&own_pkg,
    /// class_name)` finds a merged dependency entry exactly as
    /// `class_module_index.get(class_name)` used to), but it does mean a
    /// package-qualified reference to a *different* package
    /// (`compiled_module_name_qualified`'s `PackageId::Package(pkg)` lookup)
    /// won't find a dependency entry mis-keyed under this unit's own
    /// package — falling back to `resolve_qualified_module_name`'s
    /// closed-form composition there, exactly as before this ADR (which
    /// never consulted the index for a qualified reference at all). A
    /// precedence-ordered, per-dependency-`PackageId` registry merge is the
    /// documented follow-up (ADR 0119 `module_for_class`'s doc), not this
    /// wiring pass.
    ///
    /// [`ClassModuleRegistry`]: beamtalk_core::semantic_analysis::ClassModuleRegistry
    /// [`PackageId`]: beamtalk_core::semantic_analysis::PackageId
    // widened from `pub(crate)` — `beamtalk-repl` sets this before
    // generating a REPL module so cross-class self-sends resolve.
    pub fn set_class_module_index(&mut self, index: std::collections::HashMap<String, String>) {
        use beamtalk_core::semantic_analysis::{ClassModuleRegistry, ModuleName};
        let pkg = self.own_package_id();
        let mut registry = ClassModuleRegistry::new();
        for (class_name, module_name) in index {
            registry.insert(pkg.clone(), class_name, ModuleName::Generated(module_name));
        }
        self.class_context_mut().class_module_registry = registry;
    }

    /// Computes and installs the direct-call eligibility table from `hierarchy`.
    ///
    /// Reuses [`CoreErlangGenerator::compute_direct_call_eligible`] (the single
    /// implementation of the eligibility gates) so REPL expression codegen makes
    /// the same decision as module codegen (ADR 0129 §2, Phase 0a). Call this
    /// *after* [`Self::set_class_module_index`] so the target module names resolve
    /// against the REPL's class-module index.
    pub fn set_direct_call_eligible_from_hierarchy(
        &mut self,
        hierarchy: &beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy,
    ) {
        self.direct_call_eligible = Self::compute_direct_call_eligible(hierarchy, self);
    }

    /// Returns a reference to the sealed method selectors set.
    pub(in crate::core_erlang) fn sealed_method_selectors(
        &self,
    ) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.sealed_method_selectors)
    }

    /// Returns a mutable reference to the sealed method selectors set.
    pub(in crate::core_erlang) fn sealed_method_selectors_mut(
        &mut self,
    ) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().sealed_method_selectors
    }

    /// Returns the class slot constructor selector, if any.
    pub(in crate::core_erlang) fn class_slot_constructor_selector(&self) -> Option<&String> {
        self.class_context
            .as_ref()
            .and_then(|ctx| ctx.class_slot_constructor_selector.as_ref())
    }

    /// Sets the class slot constructor selector.
    pub(in crate::core_erlang) fn set_class_slot_constructor_selector(
        &mut self,
        sel: Option<String>,
    ) {
        self.class_context_mut().class_slot_constructor_selector = sel;
    }

    /// Returns whether we're in a class method body.
    pub(in crate::core_erlang) fn in_class_method(&self) -> bool {
        self.class_context
            .as_ref()
            .is_some_and(|ctx| ctx.in_class_method)
    }

    /// Sets the in-class-method flag.
    pub(in crate::core_erlang) fn set_in_class_method(&mut self, value: bool) {
        self.class_context_mut().in_class_method = value;
    }

    /// ADR 0132 §2: the kind of threaded entry body being lowered, or `None`
    /// outside one (every method today). Derived, never stored: a class
    /// method's body is [`ThreadedBodyKind::Class`], a value-type instance
    /// method's [`ThreadedBodyKind::ValueType`], an actor instance method's
    /// [`ThreadedBodyKind::Actor`]. REPL context compiles no threaded entry.
    pub(in crate::core_erlang) fn threaded_body_kind(&self) -> Option<ThreadedBodyKind> {
        if !self.in_threaded_body {
            return None;
        }
        if self.in_class_method() {
            return Some(ThreadedBodyKind::Class);
        }
        match self.context {
            CodeGenContext::ValueType => Some(ThreadedBodyKind::ValueType),
            CodeGenContext::Actor => Some(ThreadedBodyKind::Actor),
            CodeGenContext::Repl => None,
        }
    }

    /// ADR 0084: the builder class name when lowering a programmatic
    /// `ClassBuilder` class-method block into an anonymous fun, else `None`.
    pub(in crate::core_erlang) fn builder_class_method_class(&self) -> Option<String> {
        self.class_context
            .as_ref()
            .and_then(|ctx| ctx.builder_class_method_class.clone())
    }

    /// Sets (or clears) the builder class-method class name.
    pub(in crate::core_erlang) fn set_builder_class_method_class(&mut self, value: Option<String>) {
        self.class_context_mut().builder_class_method_class = value;
    }

    /// Clears per-method parameter-type tracking. Call alongside
    /// `current_method_params.clear()` at every method-body entry so a prior
    /// method's `:: Number` annotations never leak into the next and cause a
    /// spurious bare-BIF fast path.
    pub(in crate::core_erlang) fn clear_method_param_types(&mut self) {
        self.current_method_param_types.clear();
    }

    /// Records a method parameter's declared simple type for the operator
    /// fast-path classifiers (keyed by **source** name → simple type name).
    /// Callers record only the parameters
    /// [`trusted_param_types`](crate::core_erlang::field_types::trusted_param_types)
    /// answers — a `Simple` annotation on a name the body never rebinds — so
    /// anything else is left absent and the classifier falls back to the
    /// runtime guard, which is always correct.
    pub(in crate::core_erlang) fn record_method_param_type(
        &mut self,
        source_name: &str,
        type_name: &str,
    ) {
        self.current_method_param_types
            .insert(source_name.to_string(), type_name.to_string());
    }

    /// Whether `name` refers to a `:: Integer/Float/Number`-annotated
    /// parameter of the current method.
    pub(in crate::core_erlang) fn param_is_numeric(&self, name: &str) -> bool {
        self.current_method_param_types
            .get(name)
            .is_some_and(|ty| NUMERIC_TYPES.contains(&ty.as_str()))
    }

    /// Whether `name` refers to a parameter declared with a builtin
    /// comparable type. A superset of [`Self::param_is_numeric`]: bare
    /// comparison BIFs are correct for `Character`/`String` too (both define
    /// `< <=` as `@primitive`), so a `:: Character`/`:: String` param stays on
    /// the bare-BIF fast path and skips the runtime comparison guard.
    pub(in crate::core_erlang) fn param_is_comparable(&self, name: &str) -> bool {
        self.current_method_param_types
            .get(name)
            .is_some_and(|ty| COMPARABLE_TYPES.contains(&ty.as_str()))
    }

    /// Records a [`FieldTypeFact`] for every instance field and class variable
    /// of `class`, for the operator fast-path classifiers. Replaces any
    /// previously-recorded facts (call once per class at codegen entry).
    ///
    /// Own fields and `classState:` variables are classified by their
    /// declaration alone ([`classify_fields`]): a declared type is recorded in
    /// full (unions included), an untyped field is always guarded, whatever
    /// its default or the class's own writes (BT-3925 — `spawnWith:`, keyword
    /// constructors, subclasses and extensions write fields too). Inherited
    /// fields get their declared type from the class hierarchy; an inherited
    /// untyped field gets no fact and is guarded (BT-3892).
    pub(in crate::core_erlang) fn set_class_field_types(
        &mut self,
        class: &beamtalk_core::ast::ClassDefinition,
    ) {
        let mut fields = classify_fields(&class.state);
        if let (Some(hierarchy), Some(superclass)) =
            (self.class_hierarchy.as_ref(), class.superclass.as_ref())
        {
            for name in hierarchy.all_state(&superclass.name) {
                if fields.contains_key(name.as_str()) {
                    continue;
                }
                if let Some(ty) = hierarchy.state_field_type(&superclass.name, &name) {
                    fields.insert(name.to_string(), FieldTypeFact::Declared(ty));
                }
            }
        }
        self.current_class_field_types = fields;
        self.current_class_var_types = classify_fields(&class.class_variables);
    }

    /// Populates instance-field facts for an **extension** method from the
    /// *target* class's declared state types, resolved via the class hierarchy
    /// (the target's AST is unavailable at extension-codegen time). Declared
    /// fields — own or inherited, unions included — record their type, exactly
    /// like the in-class path; untyped fields get no fact (their writers are
    /// not visible here) and are guarded. Class-variable facts are cleared, so
    /// class-side `self.<name>` reads in an extension are guarded too.
    pub(in crate::core_erlang) fn set_extension_target_field_types(&mut self, target_class: &str) {
        let field_types: Vec<(String, FieldTypeFact)> = self
            .class_hierarchy
            .as_ref()
            .map(|h| {
                h.all_state(target_class)
                    .iter()
                    .filter_map(|name| {
                        h.state_field_type(target_class, name)
                            .map(|ty| (name.to_string(), FieldTypeFact::Declared(ty)))
                    })
                    .collect()
            })
            .unwrap_or_default();
        self.current_class_field_types = field_types.into_iter().collect();
        self.current_class_var_types.clear();
    }

    /// Whether a `self.<name>` read is known to hold a value on which the
    /// operator BIF for `op` is correct, so the fast path may stay
    /// bare. Inside a class method `self.<name>` is a class variable, so the
    /// class-side facts are consulted. A name with no fact is guarded — the
    /// runtime guard is always correct (BT-3892).
    pub(in crate::core_erlang) fn field_is_bare(&self, name: &str, op: BareOperator) -> bool {
        let facts = if self.in_class_method() {
            &self.current_class_var_types
        } else {
            &self.current_class_field_types
        };
        facts.get(name).is_some_and(|fact| fact.is_bare(op))
    }

    /// `self.<field>` is comparison-bare when known primitive-ordered
    /// (numeric, `Character`, or `String`) and not nilable (BT-3924).
    pub(in crate::core_erlang) fn field_is_comparable(&self, name: &str) -> bool {
        self.field_is_bare(name, BareOperator::Comparison)
    }

    /// `self.<field>` is arithmetic-bare when known numeric.
    pub(in crate::core_erlang) fn field_is_numeric(&self, name: &str) -> bool {
        self.field_is_bare(name, BareOperator::Arithmetic)
    }

    /// Returns whether stdlib mode is active.
    pub(in crate::core_erlang) fn stdlib_mode(&self) -> bool {
        self.class_context
            .as_ref()
            .is_some_and(|ctx| ctx.stdlib_mode)
    }

    /// Sets stdlib mode.
    pub(in crate::core_erlang) fn set_stdlib_mode(&mut self, value: bool) {
        self.class_context_mut().stdlib_mode = value;
    }

    /// Sets the protocol sources flattened provisions map through (ADR 0127
    /// §3, "Source locations") and which of `module`'s `uses:` lines resolved
    /// to the stdlib's own protocols (BT-3915) — taken from `analysis` when
    /// the driver handed one off, else re-derived the way the self-sufficient
    /// path flattens (nothing carried). The stdlib's own build has no stdlib
    /// fallback, so call this after [`Self::set_stdlib_mode`].
    pub(in crate::core_erlang) fn set_protocol_sources(
        &mut self,
        protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap,
        module: &beamtalk_core::ast::Module,
        analysis: Option<&beamtalk_core::semantic_analysis::AnalysisResult>,
    ) {
        self.protocol_sources = protocol_sources;
        self.stdlib_protocol_uses = match analysis {
            _ if self.stdlib_mode() => std::collections::HashSet::new(),
            Some(analysis) => analysis.stdlib_protocol_uses.clone(),
            None => beamtalk_core::semantic_analysis::trait_expansion::stdlib_resolved_uses(
                module,
                &std::collections::HashMap::new(),
                None,
            ),
        };
    }

    /// Returns a mutable reference to the class context, creating it if absent.
    pub(in crate::core_erlang) fn class_context_mut(&mut self) -> &mut ClassContext {
        self.class_context.get_or_insert_with(ClassContext::new)
    }

    /// Returns a mutable reference to the value type context, creating it if absent.
    pub(in crate::core_erlang) fn value_type_context_mut(&mut self) -> &mut ValueTypeContext {
        self.value_type_context
            .get_or_insert_with(ValueTypeContext::new)
    }

    /// Pushes a new scope for variable bindings.
    // widened from `pub(crate)` — `beamtalk-repl` pushes/pops its
    // own scopes around REPL binding generation.
    pub fn push_scope(&mut self) {
        self.var_context.push_scope();
    }

    /// Pops the current scope, discarding its bindings.
    pub fn pop_scope(&mut self) {
        self.var_context.pop_scope();
    }

    /// Looks up a variable binding in the current scope stack.
    pub(in crate::core_erlang) fn lookup_var(&self, name: &str) -> Option<&String> {
        self.var_context.lookup(name)
    }

    /// Binds an identifier to a Core Erlang variable name in the current scope.
    // widened from `pub(crate)` — `beamtalk-repl` binds
    // `__bindings__`/workspace variable names before generating a REPL body.
    pub fn bind_var(&mut self, name: &str, core_var: &str) {
        self.var_context.bind(name, core_var);
    }

    /// Records a structured diagnostic warning for the current module.
    ///
    /// Warnings are returned to callers via [`generate_module_with_warnings`].
    pub(in crate::core_erlang) fn add_codegen_warning(&mut self, diag: Diagnostic) {
        self.codegen_warnings.push(diag);
    }

    /// Drains the diagnostics recorded so far. For drivers that build their own
    /// generator (the REPL assembler, BT-3778) and forward only the `ThreadedIr`
    /// verifier ones via [`GeneratedModule::into_code_and_verifier_diagnostics`].
    pub fn take_codegen_warnings(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.codegen_warnings)
    }

    /// Emits a codegen diagnostic (gated by `BEAMTALK_CODEGEN_DIAGNOSTICS=1`).
    ///
    /// These are informational diagnostics about codegen decisions (calling conventions,
    /// dynamic dispatch, NLR throw/catch, etc.). Emitted as `Diagnostic::hint` by default.
    pub(in crate::core_erlang) fn emit_codegen_diagnostic(&mut self, message: String, span: Span) {
        if self.codegen_diagnostics_enabled {
            self.add_codegen_warning(
                Diagnostic::hint(message, span).with_category(DiagnosticCategory::Type),
            );
        }
    }

    /// Emits a `StateAcc` fallback diagnostic, gated by `BEAMTALK_CODEGEN_DIAGNOSTICS=1`.
    ///
    /// Promoted to `Diagnostic::warning` when `BEAMTALK_WARN_STATEACC=1` is also set.
    pub(in crate::core_erlang) fn emit_stateacc_fallback_diagnostic(
        &mut self,
        message: String,
        span: Span,
    ) {
        if self.codegen_diagnostics_enabled {
            if self.warn_stateacc {
                self.add_codegen_warning(
                    Diagnostic::warning(message, span)
                        .with_hint("Extract the expression into a local variable or method to avoid state-accumulator fallback")
                        .with_category(DiagnosticCategory::Type),
                );
            } else {
                self.add_codegen_warning(
                    Diagnostic::hint(message, span)
                        .with_hint("Extract the expression into a local variable or method to avoid state-accumulator fallback")
                        .with_category(DiagnosticCategory::Type),
                );
            }
        }
    }

    /// Emits the standard warning for a stateful block at an Erlang call boundary.
    ///
    /// Both `generate_simple_list_op` and `generate_direct_erlang_call` call this helper
    /// to ensure consistent warning messages across all Erlang interop sites.
    ///
    /// `erlang_target` is a human-readable call target, e.g. `"'lists':'map'"` or
    /// `"'mymod':'myfun'"`.
    /// `span` is the source span of the block literal that crosses the boundary.
    pub(in crate::core_erlang) fn warn_stateful_block_at_erlang_boundary(
        &mut self,
        erlang_target: &str,
        span: Span,
    ) {
        self.add_codegen_warning(
            Diagnostic::warning(
                format!(
                    "stateful block passed to Erlang {erlang_target} — mutations inside \
                     the block will be silently dropped (Erlang cannot propagate the updated \
                     StateAcc back to the Beamtalk caller)"
                ),
                span,
            )
            .with_hint("Extract the block body into a method, or use a stateless block")
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// Emits a warning for a non-literal callable at an Erlang call boundary.
    pub(in crate::core_erlang) fn warn_non_literal_callable_at_erlang_boundary(
        &mut self,
        erlang_target: &str,
        span: Span,
    ) {
        self.add_codegen_warning(
            Diagnostic::warning(
                format!(
                    "non-literal callable passed to Erlang {erlang_target} — if this is a \
                     stateful block, mutations inside the block will be silently dropped \
                     (runtime arity check inserted to prevent badarity crash)"
                ),
                span,
            )
            .with_hint("Use a block literal directly, or extract into a method to avoid ambiguity")
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// The protocol source a `uses:`-flattened method must be mapped through
    /// (ADR 0127 §3, "Source locations"), or `None` when `selector` on the
    /// current class is not a flattened provision (or its protocol's source
    /// is neither the stdlib's — for a `uses:` in `stdlib_protocol_uses`,
    /// BT-3915 — nor supplied via `CodegenOptions::with_protocol_sources`).
    ///
    /// Provenance comes from `MethodInfo::origin`, stamped by
    /// `trait_expansion::apply_origins`. Provisions are instance-side only
    /// in v1, so `class_side` methods never match.
    pub(in crate::core_erlang) fn flattened_method_source(
        &self,
        selector: &str,
        class_side: bool,
    ) -> Option<beamtalk_core::semantic_analysis::ProtocolSource> {
        if class_side || (self.protocol_sources.is_empty() && self.stdlib_protocol_uses.is_empty())
        {
            return None;
        }
        let class_name = self.class_identity()?.class_name();
        let origin = self
            .class_hierarchy
            .as_ref()?
            .get_class(class_name)?
            .methods
            .iter()
            .find(|m| m.selector.as_str() == selector)?
            .origin
            .as_ref()?;
        // A provision from the stdlib's own protocol (BT-3915) maps through
        // the stdlib file — never through a same-named carried protocol.
        if self
            .stdlib_protocol_uses
            .contains(&(EcoString::from(class_name), origin.clone()))
        {
            return beamtalk_core::semantic_analysis::stdlib_protocols::stdlib_protocol_source(
                origin,
            )
            .cloned();
        }
        self.protocol_sources.get(origin).cloned()
    }

    /// Converts a byte-offset `Span` to a 1-based line number.
    ///
    /// Uses `self.source_text` to count newlines before the span's start offset.
    /// Returns `None` if source text is unavailable or the span is out of range.
    pub(in crate::core_erlang) fn span_to_line(&self, span: Span) -> Option<u32> {
        let source = self.source_text.as_deref()?;
        if span.start() as usize > source.len() {
            return None;
        }
        Some(span.line_number(source))
    }

    /// Human-readable source location for a `CodeGenError`'s `location` field.
    ///
    /// `"line 12"` when [`Self::span_to_line`] can resolve the span against the
    /// available source text, `"offset 431"` when it cannot (no source text
    /// attached, or a span out of range). Every codegen rejection that carries a
    /// `location: String` formats it this way, so this is the one place the
    /// wording lives.
    pub(in crate::core_erlang) fn location_label(&self, span: Span) -> String {
        self.span_to_line(span).map_or_else(
            || format!("offset {}", span.start()),
            |l| format!("line {l}"),
        )
    }

    /// Wraps a Document with a Core Erlang line annotation.
    ///
    /// Delegates to [`leaf::annotated`] for the `[Line, {'file', Path}]` shape
    /// which the BEAM compiler preserves into the Line chunk.
    /// The VM surfaces this as `[{file, "path.bt"}, {line, N}]` in stacktrace
    /// frames. Falls back to a bare `[Line]` annotation when no source path is
    /// known (e.g. compiling from a string with no backing file).
    pub(in crate::core_erlang) fn annotate_with_line(
        &self,
        doc: Document<'static>,
        line_num: u32,
    ) -> Document<'static> {
        Self::annotate_with_line_in(self.source_path.as_deref(), doc, line_num)
    }

    /// [`Self::annotate_with_line`] against an explicit `source_path` — for a
    /// caller that captured the path while a flattened method's protocol
    /// source was still in scope (see `MethodFrame`) but annotates after the
    /// frame is gone.
    pub(in crate::core_erlang) fn annotate_with_line_in(
        source_path: Option<&str>,
        doc: Document<'static>,
        line_num: u32,
    ) -> Document<'static> {
        match source_path {
            Some(path) => leaf::annotated(doc, &leaf::BtSpan::new(path, line_num)),
            None => {
                // No source path — use bare line number annotation
                docvec![
                    "( ",
                    doc,
                    " -| [",
                    leaf::int_lit(i64::from(line_num)),
                    "] )"
                ]
            }
        }
    }
}
