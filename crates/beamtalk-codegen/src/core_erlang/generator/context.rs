// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Context structs: fields grouped by when they're relevant, wrapped as
//! `Option<T>` on [`CoreErlangGenerator`] and set only while that context
//! applies.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::threaded_ir::VersionCounter;
use crate::core_erlang::util;
use ecow::EcoString;

/// REPL-specific codegen state.
///
/// Groups fields that are only relevant when generating REPL evaluation code.
/// Wrapped as `Option<ReplContext>` on the generator — `Some` when in REPL mode,
/// `None` during batch compilation. Accessor methods on `CoreErlangGenerator`
/// provide defaults when the context is absent.
#[derive(Debug, Clone)]
pub(crate) struct ReplContext {
    /// Whether we're generating REPL code (vs module code).
    /// In REPL mode, local variable assignments should update bindings.
    pub is_repl_mode: bool,
    /// Internal flag for REPL mutation-threaded expressions.
    ///
    /// Set deep inside `generate_expression` when mutation-threaded control flow
    /// (loops, conditionals, exception handlers, inline value calls) produces a
    /// `{Result, State}` tuple that the REPL must unpack.
    ///
    /// External callers should use `expression_doc_with_repl_mutation_tracking()`
    /// instead of reading this field directly.
    pub repl_loop_mutated: bool,
    /// ADR 0131 Phase 4 (BT-3750): the span of the one expression whose
    /// `{Value, Bindings}` tuple the REPL root unpacks (a statement or an
    /// assignment's value compiled by `expression_doc_with_repl_mutation_tracking`,
    /// or a REPL producer's construct). `Result tryDo:` compiles to that tuple
    /// only there; anywhere else it is an ordinary send.
    pub repl_unpack_site: Option<beamtalk_core::source_analysis::Span>,
    /// BT-4288: the non-local-return boundary of the input being compiled
    /// (`repl_nlr`).
    pub eval_boundary: crate::core_erlang::repl_nlr::ReplEvalBoundary,
}

impl ReplContext {
    /// Creates a new `ReplContext` with default values.
    pub(crate) fn new() -> Self {
        Self {
            is_repl_mode: false,
            repl_loop_mutated: false,
            repl_unpack_site: None,
            eval_boundary: crate::core_erlang::repl_nlr::ReplEvalBoundary::default(),
        }
    }
}

/// Information about a sealed class eligible for direct-call optimization.
///
/// A class is eligible only if it is sealed and declares **no** class variables.
/// In that case, its `class sealed` methods can be called directly (bypassing
/// `gen_server` dispatch) since they don't mutate class state. This avoids the
/// ~5-10us `gen_server` round-trip overhead for utility-style class methods
/// (e.g., `File exists:`, `Json parse:`). The implementation does not inspect
/// individual method bodies for class-variable access; any presence of class
/// variables on the class makes the entire class ineligible.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct DirectCallClassInfo {
    /// The compiled Erlang module name (e.g., `bt@stdlib@tracing`).
    pub module_name: EcoString,
    /// Set of selector names eligible for direct call (e.g., `{"setContext:", "context", ...}`).
    /// Excludes `startLink`-family selectors; user methods must be `class
    /// sealed`. Synthesized keyword constructors join unconditionally:
    /// their body ignores `ClassSelf`. Never contains `new`/`new:`.
    pub selectors: std::collections::HashSet<String>,
    /// `Class new` builds via a direct `Module:'new'()` call.
    pub direct_new0: bool,
    /// `Class new: map` builds via a direct `Module:'new'(Map)` call.
    /// Consulted for `__beamtalk_meta` baking (dynamic sends);
    /// static sends keep the `class_send` fallback so the chain verdict
    /// stays single-sourced in the baked metadata.
    pub direct_new1: bool,
}

/// The direct-call eligibility table, answering per class on demand.
///
/// Module codegen used to scan every class of the hierarchy up front, which
/// made each file's incremental-build key depend on the whole class surface
/// (BT-4690). A module only asks about the classes it names, so the table is
/// lazy: [`CoreErlangGenerator::direct_call_info`] computes an entry from
/// `class_hierarchy` when first asked and memoises it. REPL expression codegen
/// has no stashed hierarchy and fills the table eagerly instead.
#[derive(Debug, Clone, Default)]
pub(in crate::core_erlang) struct DirectCallTable {
    /// Entries known so far; `None` records "not eligible".
    entries: std::cell::RefCell<
        std::collections::HashMap<String, Option<std::rc::Rc<DirectCallClassInfo>>>,
    >,
    /// Classes compiled here whose own explicit class-side `new` builds in the
    /// caller (BT-4464), applied on top of the computed entry.
    explicit_new0: std::collections::HashSet<String>,
    /// Whether a miss is computed from `class_hierarchy` (module codegen).
    lazy: bool,
}

impl DirectCallTable {
    /// A table whose misses are computed on demand from the hierarchy.
    pub(in crate::core_erlang) fn lazy() -> Self {
        Self {
            lazy: true,
            ..Self::default()
        }
    }

    /// A table holding exactly `entries`; a miss means "not eligible".
    pub(in crate::core_erlang) fn eager(
        entries: std::collections::HashMap<String, DirectCallClassInfo>,
    ) -> Self {
        Self {
            entries: std::cell::RefCell::new(
                entries
                    .into_iter()
                    .map(|(name, info)| (name, Some(std::rc::Rc::new(info))))
                    .collect(),
            ),
            ..Self::default()
        }
    }

    /// Records a ready-made entry for `class_name` (tests only).
    #[cfg(test)]
    pub(in crate::core_erlang) fn insert(&mut self, class_name: String, info: DirectCallClassInfo) {
        self.entries
            .get_mut()
            .insert(class_name, Some(std::rc::Rc::new(info)));
    }

    /// Marks `class_name`'s own explicit class-side `new` as building in the caller.
    pub(in crate::core_erlang) fn mark_explicit_new0(&mut self, class_name: String) {
        self.entries.get_mut().remove(&class_name);
        self.explicit_new0.insert(class_name);
    }

    pub(in crate::core_erlang) fn is_lazy(&self) -> bool {
        self.lazy
    }

    pub(in crate::core_erlang) fn is_explicit_new0(&self, class_name: &str) -> bool {
        self.explicit_new0.contains(class_name)
    }

    /// `None`: not asked yet. `Some(None)`: asked, and not eligible.
    #[allow(clippy::option_option)]
    pub(in crate::core_erlang) fn known(
        &self,
        class_name: &str,
    ) -> Option<Option<std::rc::Rc<DirectCallClassInfo>>> {
        self.entries.borrow().get(class_name).cloned()
    }

    pub(in crate::core_erlang) fn remember(
        &self,
        class_name: &str,
        info: Option<std::rc::Rc<DirectCallClassInfo>>,
    ) {
        self.entries
            .borrow_mut()
            .insert(class_name.to_string(), info);
    }
}

/// Class/actor-specific codegen state.
///
/// Groups fields that are only relevant when compiling a class definition
/// (actor or value type). Wrapped as `Option<ClassContext>` on the generator —
/// `Some` when a class is being compiled, `None` for standalone REPL expressions.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct ClassContext {
    /// Identity of the class currently being compiled (if any).
    /// Set from the AST class definition at the start of module generation.
    pub(in crate::core_erlang) class_identity: Option<util::ClassIdentity>,
    /// Names of class variables in the current class.
    /// Used to distinguish class variable access from instance field access in class methods.
    pub class_var_names: std::collections::HashSet<String>,
    /// Selector names of class methods in the current class.
    /// Used to route self-sends to class method functions vs module exports.
    pub class_method_selectors: std::collections::HashSet<String>,
    /// Class → compiled module resolution authority for this generation unit
    /// (ADR 0119).
    ///
    /// Built once from `CodegenOptions::class_module_index` by
    /// [`CoreErlangGenerator::set_class_module_index`] before generation
    /// begins, keyed under this module's own [`PackageId`](beamtalk_core::semantic_analysis::PackageId)
    /// (derived from `self.module_name`). `compiled_module_name` queries this
    /// registry first, falling back to the best-effort naming convention only
    /// on a genuine miss — see that method's doc for the full resolution
    /// order.
    pub class_module_registry: beamtalk_core::semantic_analysis::ClassModuleRegistry,
    /// Class names declared by more than one package in this generation's
    /// scope (BT-4151) — see
    /// [`beamtalk_core::semantic_analysis::shadowed_class_names`]. Such a
    /// name collides into one last-wins runtime class, so the compile-time
    /// package's module is not necessarily the one a dynamic send reaches.
    /// Read only through [`CoreErlangGenerator::is_shadowed_class`], which
    /// keeps these sends dynamic: typed tagged-class instance sends
    /// (`typed_tagged_target`, BT-4151) and sends to a class reference
    /// that would direct-call its module — `class sealed` class methods,
    /// keyword constructors and nullary `new` (`class_reference_direct_call_info`,
    /// BT-4271). Own-class self-sends are unaffected (`self` is this module's
    /// own class). The spawn family on an unqualified class reference
    /// (`spawn`/`spawnWith:`/`spawnMonitored`/`spawnMonitoredWith:`, and the
    /// ADR 0135 supervisor start) keeps its caller-side spawn but resolves
    /// the module at call time from the class registry instead of pinning
    /// it (`spawn_module_resolved_at_runtime`, BT-4278). Set once by the
    /// driver before generation.
    ///
    /// [`CoreErlangGenerator::is_shadowed_class`]: crate::core_erlang::CoreErlangGenerator::is_shadowed_class
    pub shadowed_class_names: beamtalk_core::semantic_analysis::ShadowedClassNames,
    /// The class this unit compiles (the module's first class), set by the
    /// driver via [`CoreErlangGenerator::register_own_class_module`]. Its
    /// compiled module is this unit's own `module_name`, whatever the index
    /// or the package-qualified derivation says — `beamtalk test` emits a
    /// test file's class unqualified (`bt@<stem>`) with no index entry while
    /// stamping it with the current package (BT-4293). Unlike
    /// `class_identity`, it is not re-scoped while an extension body
    /// generates.
    ///
    /// [`CoreErlangGenerator::register_own_class_module`]: crate::core_erlang::CoreErlangGenerator::register_own_class_module
    pub own_class_name: Option<ecow::EcoString>,
    /// Selectors of sealed methods in the current class.
    /// Used to generate standalone functions and direct call dispatch.
    pub sealed_method_selectors: std::collections::HashSet<String>,
    /// BT-4473: the instance selectors the unit's own Value class answers
    /// from a method arm of its own `dispatch/3` — its methods and its
    /// auto-generated slot accessors — with how each arm calls the method.
    /// A typed send to an instance of the own class calls that function
    /// directly behind the class-tag guard instead of going through
    /// `dispatch/3`. Set by `generate_value_type_module` before any method
    /// body is generated; empty for every other kind of unit.
    pub own_dispatch_arms:
        std::collections::HashMap<String, crate::core_erlang::value_type_codegen::OwnDispatchArm>,
    /// Auto-generated keyword constructor selector for Value subclass: classes.
    /// E.g. `"symName:"` for a single-slot class, `"x:y:"` for two slots.
    /// Set during class method codegen to route `ClassName slot: value` to the correct
    /// class-side constructor instead of the instance-side getter.
    pub class_slot_constructor_selector: Option<String>,
    /// Whether we're currently generating a class-side method body.
    /// When true, field access/assignment should produce a compile error.
    pub in_class_method: bool,
    /// Whether this module is being compiled in stdlib mode.
    /// When true, `generate_register_class` emits `stdlibMode => true` in the builder
    /// state so the runtime can bypass the sealed-superclass check for stdlib loading.
    pub stdlib_mode: bool,
    /// ADR 0084: When `Some(ClassName)`, we are lowering a programmatic
    /// `ClassBuilder` class-method block into an anonymous fun. Such a fun has no
    /// `class_<sel>` module export, so self-sends and `super` route through the
    /// runtime dispatch helpers (`class_self_dispatch_local` for self-sends,
    /// `class_super_dispatch` for `super`) keyed on this class name, not
    /// through a direct module call.
    pub builder_class_method_class: Option<String>,
    /// BT-3762: the class-side extension's *target* class while its method is
    /// lowered (the extension module's own class identity is not the target).
    /// Read through [`CoreErlangGenerator::class_var_home`]: it names the class
    /// in the "class variables are per class" diagnostic and keys a class-side
    /// `super` send (BT-4001).
    pub extension_target_class: Option<String>,
}

/// ADR 0084: Snapshot of the class-method-relevant `ClassContext`
/// fields, captured when entering a programmatic `ClassBuilder` class-method
/// lowering and restored on exit. `had_context` records whether a `ClassContext`
/// existed beforehand, so a context created solely for a standalone builder
/// cascade is dropped rather than leaked.
#[derive(Debug)]
pub(in crate::core_erlang) struct SavedClassMethodCtx {
    had_context: bool,
    in_class_method: bool,
    class_var_names: std::collections::HashSet<String>,
    class_method_selectors: std::collections::HashSet<String>,
    class_slot_constructor_selector: Option<String>,
    builder_class_method_class: Option<String>,
    // the instance-`State` version
    // counter lives outside `ClassContext` (it's shared by every class, not
    // per-class-context), so nothing else captures it even though
    // `generate_class_method_fun_from_block` unconditionally resets it. A
    // builder cascade nested inside an enclosing method's own field-assignment
    // value (e.g. `self.x := Object classBuilder … addClassMethod:body: […]; register`)
    // would silently clobber the enclosing method's in-progress `State`
    // version, producing two bindings for the same version — caught by the
    // ADR-0111 verifier as `NonLinearVersion`.
    state_version: usize,
    // same unguarded-reset shape as `state_version` above, for the
    // enclosing method's own parameter list/types. `generate_class_method_fun_from_block`
    // unconditionally clears both (`current_method_params.clear()` /
    // `clear_method_param_types()`) to give the class-method fun its own fresh
    // set — but they too live outside `ClassContext`, so a builder cascade
    // nested inside an enclosing method's body would wipe out the enclosing
    // method's real parameters/types for any later statement that reads them
    // (e.g. the `erlangApply`/`erlangModuleLookup` FFI intrinsics, or
    // primitive-BIF codegen).
    current_method_params: Vec<String>,
    current_method_param_types: std::collections::HashMap<String, String>,
}

impl ClassContext {
    /// Creates a new `ClassContext` with default values.
    pub(in crate::core_erlang) fn new() -> Self {
        Self {
            class_identity: None,
            class_var_names: std::collections::HashSet::new(),
            class_method_selectors: std::collections::HashSet::new(),
            class_module_registry: beamtalk_core::semantic_analysis::ClassModuleRegistry::new(),
            shadowed_class_names: beamtalk_core::semantic_analysis::ShadowedClassNames::default(),
            own_class_name: None,
            sealed_method_selectors: std::collections::HashSet::new(),
            own_dispatch_arms: std::collections::HashMap::new(),
            class_slot_constructor_selector: None,
            in_class_method: false,
            stdlib_mode: false,
            builder_class_method_class: None,
            extension_target_class: None,
        }
    }
}

/// Value-type-specific codegen state.
///
/// Groups fields that are only relevant when compiling value type (non-actor)
/// class methods. Wrapped as `Option<ValueTypeContext>` on the generator —
/// `Some` when compiling value type code, `None` otherwise.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct ValueTypeContext {
    /// Self-threading version counter for value type field assignments.
    ///
    /// Mirrors `state_threading` for value types. Each field assignment increments
    /// this counter: `Self` → `Self1` → `Self2` → ... so that `self` in expression
    /// position always resolves to the latest immutable snapshot.
    ///
    /// Not `pub` (unlike `current_nlr_token`) — [`VersionCounter`] is
    /// `pub(super)` within `threaded_ir`, narrower than `ValueTypeContext`'s
    /// own `pub(super)`; all access stays inside `mod.rs` via the
    /// `self_version()`/`set_self_version()` accessor methods, exactly as
    /// before.
    pub(in crate::core_erlang) self_version: VersionCounter,
    /// Core Erlang variable name holding the non-local return token for the current
    /// value type method, or `None` when no NLR infrastructure is active.
    ///
    /// Set by `generate_value_type_method` when the method body contains blocks with `^`.
    /// When set, `generate_expression` for `Expression::Return` generates a throw instead
    /// of a plain value, causing the return to escape from the enclosing block closure.
    pub current_nlr_token: Option<String>,
}

impl ValueTypeContext {
    /// Creates a new `ValueTypeContext` with default values.
    pub(in crate::core_erlang) fn new() -> Self {
        Self {
            self_version: VersionCounter::new(),
            current_nlr_token: None,
        }
    }
}

impl CoreErlangGenerator {
    /// ADR 0084: Enter the class-method lowering context for a
    /// programmatic `ClassBuilder` cascade's `classMethods:` funs, returning the
    /// prior state to restore. Sets `in_class_method`, the class-variable names
    /// (from the cascade's `classVars:` keys), and the builder class name used
    /// for runtime self/`super` dispatch. Safe whether or not an enclosing class
    /// is being compiled — a context created here is dropped on exit.
    pub(in crate::core_erlang) fn enter_builder_class_method_context(
        &mut self,
        class_name: &str,
        class_var_names: &[String],
    ) -> SavedClassMethodCtx {
        let saved = SavedClassMethodCtx {
            had_context: self.class_context.is_some(),
            in_class_method: self.in_class_method(),
            class_var_names: self.class_var_names().clone(),
            class_method_selectors: self.class_method_selectors().clone(),
            class_slot_constructor_selector: self.class_slot_constructor_selector().cloned(),
            builder_class_method_class: self.builder_class_method_class(),
            state_version: self.state_version(),
            current_method_params: self.current_method_params.clone(),
            current_method_param_types: self.current_method_param_types.clone(),
        };
        self.set_in_class_method(true);
        *self.class_var_names_mut() = class_var_names.iter().cloned().collect();
        // class_method_selectors is intentionally left empty: in builder mode
        // `generate_class_method_self_send` routes EVERY self-send through
        // `class_self_dispatch_local` (the fun has no `class_<sel>` export) before
        // it ever consults this set, so it is not needed for dispatch.
        self.class_method_selectors_mut().clear();
        self.set_class_slot_constructor_selector(None);
        self.set_builder_class_method_class(Some(class_name.to_string()));
        saved
    }

    /// Restore the class-method context saved by
    /// [`enter_builder_class_method_context`](Self::enter_builder_class_method_context).
    pub(in crate::core_erlang) fn exit_builder_class_method_context(
        &mut self,
        saved: SavedClassMethodCtx,
    ) {
        if saved.had_context {
            self.set_in_class_method(saved.in_class_method);
            *self.class_var_names_mut() = saved.class_var_names;
            *self.class_method_selectors_mut() = saved.class_method_selectors;
            self.set_class_slot_constructor_selector(saved.class_slot_constructor_selector);
            self.set_builder_class_method_class(saved.builder_class_method_class);
        } else {
            // No enclosing class context — drop the one we created so standalone
            // (REPL) builder cascades don't leak a class context.
            self.class_context = None;
        }
        // `state_version` lives outside `ClassContext`, so it must be
        // restored unconditionally here — independent of `had_context` — or a
        // builder cascade nested inside an enclosing method's field-assignment
        // value leaves that method's `State` version counter reset to 0
        // instead of wherever it legitimately was.
        self.set_state_version(saved.state_version);
        // same reasoning as `state_version` above — restore
        // unconditionally, independent of `had_context`.
        self.current_method_params = saved.current_method_params;
        self.current_method_param_types = saved.current_method_param_types;
    }
}
