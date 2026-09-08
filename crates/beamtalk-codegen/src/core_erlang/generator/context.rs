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

/// BT-1461: REPL-specific codegen state.
///
/// Groups fields that are only relevant when generating REPL evaluation code.
/// Wrapped as `Option<ReplContext>` on the generator — `Some` when in REPL mode,
/// `None` during batch compilation. Accessor methods on `CoreErlangGenerator`
/// provide defaults when the context is absent.
#[derive(Debug, Clone)]
pub(crate) struct ReplContext {
    /// BT-153: Whether we're generating REPL code (vs module code).
    /// In REPL mode, local variable assignments should update bindings.
    pub is_repl_mode: bool,
    /// BT-245/BT-1448: Internal flag for REPL mutation-threaded expressions.
    ///
    /// Set deep inside `generate_expression` when mutation-threaded control flow
    /// (loops, conditionals, exception handlers, inline value calls) produces a
    /// `{Result, State}` tuple that the REPL must unpack.
    ///
    /// External callers should use `expression_doc_with_repl_mutation_tracking()`
    /// instead of reading this field directly.
    pub repl_loop_mutated: bool,
    /// BT-374 / ADR 0010 / ADR 0019: Whether workspace bindings are available.
    /// When true (REPL/workspace context), class references resolve through
    /// session bindings or class registry. When false (batch compile),
    /// class references go directly to the class registry.
    pub workspace_mode: bool,
}

impl ReplContext {
    /// Creates a new `ReplContext` with default values.
    pub(crate) fn new() -> Self {
        Self {
            is_repl_mode: false,
            repl_loop_mutated: false,
            workspace_mode: false,
        }
    }
}

/// BT-1639: Information about a sealed class eligible for direct-call optimization.
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
    /// Excludes `startLink`-family selectors and non-sealed class methods.
    pub selectors: std::collections::HashSet<String>,
}

/// BT-1461: Class/actor-specific codegen state.
///
/// Groups fields that are only relevant when compiling a class definition
/// (actor or value type). Wrapped as `Option<ClassContext>` on the generator —
/// `Some` when a class is being compiled, `None` for standalone REPL expressions.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct ClassContext {
    /// Identity of the class currently being compiled (if any).
    /// Set from the AST class definition at the start of module generation.
    pub(in crate::core_erlang) class_identity: Option<util::ClassIdentity>,
    /// BT-412: Names of class variables in the current class.
    /// Used to distinguish class variable access from instance field access in class methods.
    pub class_var_names: std::collections::HashSet<String>,
    /// BT-412: Selector names of class methods in the current class.
    /// Used to route self-sends to class method functions vs module exports.
    pub class_method_selectors: std::collections::HashSet<String>,
    /// BT-3151: Selector names of class methods (in the current class) that are
    /// known or suspected to mutate a class variable, directly or transitively
    /// — see `block_analysis::compute_class_var_mutating_selectors`. Used to
    /// let a self-send to a provably pure class method compile inside a bare,
    /// unthreaded block (`select:`/`collect:`/`do:`/etc.) while rejecting one
    /// that may mutate class state there, where BT-3150's `Letrec`-only guard
    /// doesn't reach.
    pub class_var_mutating_selectors: std::collections::HashSet<String>,
    /// BT-412/BT-3131: State version counter for class variable threading.
    ///
    /// Not `pub` (unlike this struct's other fields) — [`VersionCounter`] is
    /// `pub(super)` within `threaded_ir`, narrower than `ClassContext`'s own
    /// `pub(super)` (= `pub(in crate)`); all access stays inside
    /// `mod.rs` via the `class_var_version()`/`set_class_var_version()`
    /// accessor methods, exactly as before.
    pub(in crate::core_erlang) class_var_version: VersionCounter,
    /// BT-412: Whether class variables were mutated in the current method.
    pub class_var_mutated: bool,
    /// Class → compiled module resolution authority for this generation unit
    /// (ADR 0119 / BT-3436).
    ///
    /// Built once from `CodegenOptions::class_module_index` by
    /// [`CoreErlangGenerator::set_class_module_index`] before generation
    /// begins, keyed under this module's own [`PackageId`](beamtalk_core::semantic_analysis::PackageId)
    /// (derived from `self.module_name`). `compiled_module_name` queries this
    /// registry first, falling back to the best-effort naming convention only
    /// on a genuine miss — see that method's doc for the full resolution
    /// order.
    pub class_module_registry: beamtalk_core::semantic_analysis::ClassModuleRegistry,
    /// BT-403: Selectors of sealed methods in the current class.
    /// Used to generate standalone functions and direct call dispatch.
    pub sealed_method_selectors: std::collections::HashSet<String>,
    /// BT-996: Auto-generated keyword constructor selector for Value subclass: classes.
    /// E.g. `"symName:"` for a single-slot class, `"x:y:"` for two slots.
    /// Set during class method codegen to route `ClassName slot: value` to the correct
    /// class-side constructor instead of the instance-side getter.
    pub class_slot_constructor_selector: Option<String>,
    /// BT-426: Whether we're currently generating a class-side method body.
    /// When true, field access/assignment should produce a compile error.
    pub in_class_method: bool,
    /// BT-791: Whether this module is being compiled in stdlib mode.
    /// When true, `generate_register_class` emits `stdlibMode => true` in the builder
    /// state so the runtime can bypass the sealed-superclass check for stdlib loading.
    pub stdlib_mode: bool,
    /// ADR 0084 / BT-2267: When `Some(ClassName)`, we are lowering a programmatic
    /// `ClassBuilder` class-method block into an anonymous fun. Such a fun has no
    /// `class_<sel>` module export, so self-sends and `super` route through the
    /// runtime dispatch helpers (`class_self_dispatch_local`/`class_self_dispatch`)
    /// keyed on this class name, not through a direct module call.
    pub builder_class_method_class: Option<String>,
}

/// ADR 0084 / BT-2267: Snapshot of the class-method-relevant `ClassContext`
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
    class_var_mutating_selectors: std::collections::HashSet<String>,
    class_var_version: usize,
    class_var_mutated: bool,
    class_slot_constructor_selector: Option<String>,
    builder_class_method_class: Option<String>,
    // BT-3289: unlike `class_var_version` above, the instance-`State` version
    // counter lives outside `ClassContext` (it's shared by every class, not
    // per-class-context), so nothing captured it here even though
    // `generate_class_method_fun_from_block` unconditionally resets it. A
    // builder cascade nested inside an enclosing method's own field-assignment
    // value (e.g. `self.x := Object classBuilder … addClassMethod:body: […]; register`)
    // would silently clobber the enclosing method's in-progress `State`
    // version, producing two bindings for the same version — caught by the
    // ADR-0111 verifier as `NonLinearVersion`.
    state_version: usize,
    // BT-3300: same unguarded-reset shape as `state_version` above, for the
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
            class_var_mutating_selectors: std::collections::HashSet::new(),
            class_var_version: VersionCounter::new(),
            class_var_mutated: false,
            class_module_registry: beamtalk_core::semantic_analysis::ClassModuleRegistry::new(),
            sealed_method_selectors: std::collections::HashSet::new(),
            class_slot_constructor_selector: None,
            in_class_method: false,
            stdlib_mode: false,
            builder_class_method_class: None,
        }
    }
}

/// BT-1461: Value-type-specific codegen state.
///
/// Groups fields that are only relevant when compiling value type (non-actor)
/// class methods. Wrapped as `Option<ValueTypeContext>` on the generator —
/// `Some` when compiling value type code, `None` otherwise.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct ValueTypeContext {
    /// BT-833/BT-3131: Self-threading version counter for value type field assignments.
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
    /// BT-754: Core Erlang variable name holding the non-local return token for the current
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
    /// ADR 0084 / BT-2267: Enter the class-method lowering context for a
    /// programmatic `ClassBuilder` cascade's `classMethods:` funs, returning the
    /// prior state to restore. Sets `in_class_method`, the class-variable names
    /// (from the cascade's `classVars:` keys), and the builder class name used
    /// for runtime self/`super` dispatch. Safe whether or not an enclosing class
    /// is being compiled — a context created here is dropped on exit.
    ///
    /// BT-3131: `class_var_version`'s save-reset-restore here rides the same
    /// unified `VersionCounter` mechanism as [`BranchContextGuard`] — a
    /// distinct *reset* policy (this is a fresh method context, not a branch:
    /// the counter resets to 0 here, whereas `with_branch_context` restores
    /// without resetting), but through the identical counter implementation
    /// and accessor methods (`class_var_version`/`set_class_var_version`).
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
            class_var_mutating_selectors: self.class_var_mutating_selectors().clone(),
            class_var_version: self.class_var_version(),
            class_var_mutated: self.class_var_mutated(),
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
        // it ever consults this set, so it is not needed for dispatch. Class-var
        // threading across such self-sends rides on the open scope that
        // `emit_class_var_result_unwrap` always produces, not on this set.
        self.class_method_selectors_mut().clear();
        // BT-3151: also cleared, for the same reason plus one more — an empty
        // `class_method_selectors` already makes `generate_block`'s bare-block
        // self-send check treat every self-send here as unresolvable (so
        // conservatively unsafe) regardless of this set's contents, since a
        // programmatic `ClassBuilder` cascade has no static `ClassDefinition`
        // to run `compute_class_var_mutating_selectors` over in the first
        // place. Clearing just avoids leaking the enclosing class's own
        // mutating-selector set into an unrelated builder class's selector
        // namespace.
        self.class_var_mutating_selectors_mut().clear();
        self.set_class_var_version(0);
        self.set_class_var_mutated(false);
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
            *self.class_var_mutating_selectors_mut() = saved.class_var_mutating_selectors;
            self.set_class_var_version(saved.class_var_version);
            self.set_class_var_mutated(saved.class_var_mutated);
            self.set_class_slot_constructor_selector(saved.class_slot_constructor_selector);
            self.set_builder_class_method_class(saved.builder_class_method_class);
        } else {
            // No enclosing class context — drop the one we created so standalone
            // (REPL) builder cascades don't leak a class context.
            self.class_context = None;
        }
        // BT-3289: `state_version` lives outside `ClassContext`, so it must be
        // restored unconditionally here — independent of `had_context` — or a
        // builder cascade nested inside an enclosing method's field-assignment
        // value leaves that method's `State` version counter reset to 0
        // instead of wherever it legitimately was.
        self.set_state_version(saved.state_version);
        // BT-3300: same reasoning as `state_version` above — restore
        // unconditionally, independent of `had_context`.
        self.current_method_params = saved.current_method_params;
        self.current_method_param_types = saved.current_method_param_types;
    }
}
