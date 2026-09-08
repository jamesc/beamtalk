// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Small accessor/mutator methods on [`CoreErlangGenerator`] — field
//! getters/setters and the context-default-providing wrappers around the
//! `Option<ReplContext>`/`Option<ClassContext>`/`Option<ValueTypeContext>`
//! fields.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::gen_server;
use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::generator::{ClassContext, ReplContext, ValueTypeContext};
use crate::core_erlang::util;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};

impl CoreErlangGenerator {
    /// ADR 0098 Phase 3: the producing-toolchain identity to bake into
    /// `__beamtalk_meta`. Borrows the generator's version fields; both are `None`
    /// unless the CLI supplied them via [`CodegenOptions::with_provenance`].
    pub(in crate::core_erlang) fn meta_provenance(&self) -> gen_server::MetaProvenance<'_> {
        gen_server::MetaProvenance {
            beamtalk_version: self.beamtalk_version.as_deref(),
            otp_release: self.otp_release.as_deref(),
        }
    }

    /// Returns `true` if REPL mode is active.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` queries/sets this
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

    /// Returns `true` if workspace mode is active.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` queries/sets this
    // around its own generation calls.
    pub fn workspace_mode(&self) -> bool {
        self.repl_context
            .as_ref()
            .is_some_and(|ctx| ctx.workspace_mode)
    }

    /// Sets workspace mode, initialising the context if absent.
    pub fn set_workspace_mode(&mut self, value: bool) {
        self.repl_context_mut().workspace_mode = value;
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

    /// BT-3151: Returns a reference to the class-var-mutating selectors set.
    pub(in crate::core_erlang) fn class_var_mutating_selectors(
        &self,
    ) -> &std::collections::HashSet<String> {
        static EMPTY: std::sync::LazyLock<std::collections::HashSet<String>> =
            std::sync::LazyLock::new(std::collections::HashSet::new);
        self.class_context
            .as_ref()
            .map_or(&*EMPTY, |ctx| &ctx.class_var_mutating_selectors)
    }

    /// BT-3151: Returns a mutable reference to the class-var-mutating selectors set.
    pub(in crate::core_erlang) fn class_var_mutating_selectors_mut(
        &mut self,
    ) -> &mut std::collections::HashSet<String> {
        &mut self.class_context_mut().class_var_mutating_selectors
    }

    /// Returns the class variable version counter.
    pub(in crate::core_erlang) fn class_var_version(&self) -> usize {
        self.class_context
            .as_ref()
            .map_or(0, |ctx| ctx.class_var_version.version())
    }

    /// Sets the class variable version counter.
    pub(in crate::core_erlang) fn set_class_var_version(&mut self, version: usize) {
        self.class_context_mut()
            .class_var_version
            .set_version(version);
    }

    /// Returns whether class variables were mutated in the current method.
    pub(in crate::core_erlang) fn class_var_mutated(&self) -> bool {
        self.class_context
            .as_ref()
            .is_some_and(|ctx| ctx.class_var_mutated)
    }

    /// Sets the class variable mutated flag.
    pub(in crate::core_erlang) fn set_class_var_mutated(&mut self, value: bool) {
        self.class_context_mut().class_var_mutated = value;
    }

    /// Derives this generation unit's own [`PackageId`] from `self.module_name`
    /// (ADR 0119 / BT-3436): `bt@stdlib@...` is [`PackageId::Stdlib`],
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
    /// registry for this generation unit (ADR 0119 / BT-3436).
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` sets this before
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

    /// ADR 0084 / BT-2267: the builder class name when lowering a programmatic
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

    /// BT-2709: Clears per-method parameter-type tracking. Call alongside
    /// `current_method_params.clear()` at every method-body entry so a prior
    /// method's `:: Number` annotations never leak into the next and cause a
    /// spurious bare-BIF fast path.
    pub(in crate::core_erlang) fn clear_method_param_types(&mut self) {
        self.current_method_param_types.clear();
    }

    /// BT-2709: Records a method parameter's declared type for the arithmetic
    /// fast-path classifier (keyed by **source** name → simple type name).
    /// Only `Simple` annotations are recorded; anything else is left absent so
    /// the classifier falls back to the runtime `is_number` guard, which is
    /// always correct.
    pub(in crate::core_erlang) fn record_method_param_type(
        &mut self,
        source_name: &str,
        annotation: Option<&beamtalk_core::ast::TypeAnnotation>,
    ) {
        if let Some(beamtalk_core::ast::TypeAnnotation::Simple(id)) = annotation {
            self.current_method_param_types
                .insert(source_name.to_string(), id.name.to_string());
        }
    }

    /// BT-2709: Whether `name` refers to a `:: Integer/Float/Number`-annotated
    /// parameter of the current method.
    pub(in crate::core_erlang) fn param_is_numeric(&self, name: &str) -> bool {
        self.current_method_param_types
            .get(name)
            .is_some_and(|ty| matches!(ty.as_str(), "Integer" | "Float" | "Number"))
    }

    /// BT-2710: Whether `name` refers to a parameter declared with a builtin
    /// comparable type. A superset of [`Self::param_is_numeric`]: bare
    /// comparison BIFs are correct for `Character`/`String` too (both define
    /// `< <=` as `@primitive`), so a `:: Character`/`:: String` param stays on
    /// the bare-BIF fast path and skips the `is_object` guard.
    pub(in crate::core_erlang) fn param_is_comparable(&self, name: &str) -> bool {
        self.current_method_param_types.get(name).is_some_and(|ty| {
            matches!(
                ty.as_str(),
                "Integer" | "Float" | "Number" | "Character" | "String"
            )
        })
    }

    /// BT-2710 follow-up: Records each instance field's declared `Simple` type
    /// from a class's state declarations, for the operator fast-path
    /// classifiers. Replaces any previously-recorded set (call once per class at
    /// codegen entry). Only `Simple` annotations are recorded; untyped fields
    /// are deliberately absent so they keep the bare-BIF status quo.
    pub(in crate::core_erlang) fn set_class_field_types(
        &mut self,
        state: &[beamtalk_core::ast::StateDeclaration],
    ) {
        self.current_class_field_types.clear();
        for decl in state {
            if let Some(beamtalk_core::ast::TypeAnnotation::Simple(id)) =
                decl.type_annotation.as_ref()
            {
                self.current_class_field_types
                    .insert(decl.name.name.to_string(), id.name.to_string());
            }
        }
    }

    /// BT-2728: Populates instance-field type tracking for an **extension**
    /// method from the *target* class's declared state types, resolved via the
    /// class hierarchy. The target class is foreign (declared in another
    /// module), so its AST `state` is unavailable at extension-codegen time, but
    /// its [`ClassInfo`] carries the field-type strings. This lets an extension
    /// method's `self.<field>` operator dispatch be type-aware, matching in-class
    /// methods (which use [`Self::set_class_field_types`]).
    ///
    /// Mirrors `set_class_field_types`'s filtering: only *simple* named types are
    /// recorded, so generic/union/singleton-typed fields keep the bare-BIF
    /// status quo (parity with the in-class path, which records only
    /// `TypeAnnotation::Simple`). When the target class is not in the hierarchy,
    /// the map is cleared — the bare-BIF fallback, unchanged status quo.
    ///
    /// [`ClassInfo`]: beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo
    pub(in crate::core_erlang) fn set_extension_target_field_types(&mut self, target_class: &str) {
        let field_types: Vec<(String, String)> = self
            .class_hierarchy
            .as_ref()
            .and_then(|h| h.get_class(target_class))
            .map(|info| {
                info.state_types
                    .iter()
                    .filter(|(_, ty)| Self::is_simple_type_name(ty))
                    .map(|(name, ty)| (name.to_string(), ty.to_string()))
                    .collect()
            })
            .unwrap_or_default();
        self.current_class_field_types.clear();
        for (name, ty) in field_types {
            self.current_class_field_types.insert(name, ty);
        }
    }

    /// Whether `ty` is a *simple* named type (a bare identifier such as `Money`
    /// or `Integer`), as opposed to a generic (`List(Integer)`), union
    /// (`Integer | String`), singleton (`#north`), or metatype (`Foo class`).
    ///
    /// So extension-method field typing matches the in-class path (which
    /// records only `TypeAnnotation::Simple` fields, see
    /// [`Self::set_class_field_types`]). `Self` needs no explicit exclusion
    /// here: unlike the pre-BT-3076 string-rendered check, `Self` is
    /// [`DeclaredType::SelfType`], never `Simple("Self")`, so it already
    /// falls through to `false`.
    ///
    /// [`ClassInfo::state_types`]: beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo::state_types
    pub(in crate::core_erlang) fn is_simple_type_name(
        ty: &beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType,
    ) -> bool {
        matches!(
            ty,
            beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::Simple(_)
        )
    }

    /// BT-2710 follow-up: Whether `self.<name>` is known to hold a value with a
    /// builtin total order / numeric type, so the comparison/arithmetic fast
    /// path may stay bare. True when the field is **untyped** (no info — keep
    /// the status quo) or its declared type is in `primitive_set`; false only
    /// when the field has an explicit non-primitive (object) type, which then
    /// routes through the runtime guard and dispatches.
    pub(in crate::core_erlang) fn field_is_bare(&self, name: &str, primitive_set: &[&str]) -> bool {
        match self.current_class_field_types.get(name) {
            Some(ty) => primitive_set.contains(&ty.as_str()),
            None => true,
        }
    }

    /// BT-2710 follow-up: `self.<field>` is comparison-bare when untyped or a
    /// primitive-ordered type (numeric, `Character`, or `String`).
    pub(in crate::core_erlang) fn field_is_comparable(&self, name: &str) -> bool {
        self.field_is_bare(name, &["Integer", "Float", "Number", "Character", "String"])
    }

    /// BT-2709 / BT-2710 follow-up: `self.<field>` is arithmetic-bare when
    /// untyped or a numeric type.
    pub(in crate::core_erlang) fn field_is_numeric(&self, name: &str) -> bool {
        self.field_is_bare(name, &["Integer", "Float", "Number"])
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` pushes/pops its
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` binds
    // `__bindings__`/workspace variable names before generating a REPL body.
    pub fn bind_var(&mut self, name: &str, core_var: &str) {
        self.var_context.bind(name, core_var);
    }

    /// BT-855: Records a structured diagnostic warning for the current module.
    ///
    /// Warnings are returned to callers via [`generate_module_with_warnings`].
    pub(in crate::core_erlang) fn add_codegen_warning(&mut self, diag: Diagnostic) {
        self.codegen_warnings.push(diag);
    }

    /// BT-1343: Emits a codegen diagnostic (gated by `BEAMTALK_CODEGEN_DIAGNOSTICS=1`).
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

    /// BT-1343: Emits a `StateAcc` fallback diagnostic, gated by `BEAMTALK_CODEGEN_DIAGNOSTICS=1`.
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

    /// BT-855: Emits the standard warning for a stateful block at an Erlang call boundary.
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

    /// BT-909: Emits a warning for a non-literal callable at an Erlang call boundary.
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

    /// BT-940: Converts a byte-offset `Span` to a 1-based line number.
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

    /// BT-940/BT-3127: Wraps a Document with a Core Erlang line annotation.
    ///
    /// Delegates to [`leaf::annotated`] for the `[Line, {'file', Path}]` shape
    /// (BT-3119 spike), which the BEAM compiler preserves into the Line chunk.
    /// The VM surfaces this as `[{file, "path.bt"}, {line, N}]` in stacktrace
    /// frames. Falls back to a bare `[Line]` annotation when no source path is
    /// known (e.g. compiling from a string with no backing file).
    pub(in crate::core_erlang) fn annotate_with_line(
        &self,
        doc: Document<'static>,
        line_num: u32,
    ) -> Document<'static> {
        match &self.source_path {
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
