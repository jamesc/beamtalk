// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value type module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for value type classes — plain Erlang
//! terms (maps) with no process. They are created with `new` and `new:`,
//! not `spawn`, and methods are synchronous functions operating on maps.
//!
//! This file is the module shell: the module exports and attributes, the `new`
//! / `new:` constructors, and the shared helper types and module-naming helpers.
//! The rest lives in submodules, split by concern:
//!
//! - [`body`]: method-body statement classification, field assignments, loops
//!   and the "open" threading layer
//! - [`conditionals`]: value-type conditional (`ifTrue:` family) lowering
//! - [`dispatch`]: `dispatch/3`, `dispatch/4`, reflection arms and `has_method`

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Write as FmtWrite;

use super::module_shell::core_module;
use super::spec_codegen;
use super::threaded_ir::{ThreadedStmt, ValueRef, VersionPrefix};
use super::util::ClassIdentity;
use super::value_accessors::{AutoSlotMethods, has_opaque_native_representation};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, INDENT, concat, join, leaf, line, nest};
use beamtalk_core::ast::{ClassDefinition, ClassKind, MethodKind, Module, TypeAnnotation};

/// Classification of how a value-type method body expression should be handled
/// for Self-threading.  Produced by [`CoreErlangGenerator::classify_vt_body_expr`]
/// and consumed by the unified body loop in [`CoreErlangGenerator::generate_value_type_method`].
enum VtBodyExprKind {
    /// `^ value` — early return from method.
    EarlyReturn,
    /// `self.field := value` — direct field assignment (Self-threading).
    FieldAssignment,
    /// `var := expr` — local variable assignment.
    LocalAssignment,
    /// `{a, b} := expr` — destructure assignment.
    DestructureAssignment,
    /// Non-last `do:` loop that mutates captured outer locals.
    DoWithLocalThreading,
    /// Non-last `to:do:` / `to:by:do:` / `timesRepeat:` counted loop that mutates
    /// captured outer locals.
    CountedLoopWithLocalThreading,
    /// `collect:` / `select:` / `reject:` / `inject:into:` foldl list-op that mutates
    /// captured outer locals. Unlike the loops, element 1 of the threaded
    /// `{value, StateAcc}` tuple is a *meaningful* result (the collected/folded value).
    FoldlListOpWithLocalThreading,
    /// Non-last `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` with local mutations.
    ConditionalWithLocalThreading,
    /// non-last `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` whose branch
    /// writes a value-type `self.field := ...` but mutates NO outer local —
    /// so [`ConditionalWithLocalThreading`](Self::ConditionalWithLocalThreading)
    /// does not catch it, yet it still needs the same inline-`case` treatment
    /// (the generic Tier 1 closure path rejects a field write outright). See
    /// [`CoreErlangGenerator::is_conditional_with_vt_self_field_threading`]
    /// for why this is a separate kind rather than a widening of that one.
    ConditionalWithSelfFieldThreading,
    /// Non-last block value with captured mutations.
    BlockWithCapturedMutations(Vec<String>),
    /// Non-last `whileTrue:` / `whileFalse:` with local mutations.
    WhileWithLocalThreading,
    /// Non-last `on:do:`/`ensure:` with local mutations and/or a
    /// value-type `self.field := ...` write.
    ExceptionConstructWithThreading,
    /// Regular expression with no special Self-threading needs.
    Pure,
}

/// Intermediate pieces for one arm (true or false) of a vt-conditional's
/// open-scope case, before the arm's final return value is finalized.
///
/// Finalizing (via [`CoreErlangGenerator::finish_vt_conditional_branch`]) is deferred
/// until both sibling arms are known, because whether the return value needs a
/// trailing `Self` slot — and hence whether it's a bare value or a tuple —
/// depends on whether *either* arm mutated `Self`, a fact only known once both
/// arms have been generated. `local_values` must be resolved eagerly, while the arm's
/// own scope (pushed by [`CoreErlangGenerator::build_vt_conditional_branch_pieces`])
/// is still active.
struct VtBranchPieces {
    /// The arm's own `let`-preamble (empty for a passthrough arm).
    preamble: Vec<Document<'static>>,
    /// One resolved value-doc per `all_mutations[i]`, in the same order.
    local_values: Vec<Document<'static>>,
    /// `Some(version)` if a value-type `self.field:=...` write in
    /// this arm's body advanced the `SelfVt` version past the baseline it
    /// started from — the input to the trailing `Self{N}` tuple slot both
    /// arms must agree on.
    self_mutated_version: Option<usize>,
    /// BT-3821: the arm's own value (its last statement's), when the conditional
    /// is lowered in value position ([`CoreErlangGenerator::lower_vt_conditional`]
    /// with `capture_value`); `None` for the statement form, which discards it.
    value: Option<Document<'static>>,
}

/// BT-3821: a value-type conditional lowered to its [`ThreadedStmt`] prelude
/// ([`CoreErlangGenerator::lower_vt_conditional`]) — the one lowering the
/// statement form ([`CoreErlangGenerator::generate_vt_conditional_open`]
/// renders it) and the value-position producer
/// ([`CoreErlangGenerator::vt_conditional_value_producer`] splices it) share.
struct VtConditionalLowered {
    prelude: Vec<ThreadedStmt>,
    /// The conditional's own value, `element(1, <carrier>)`, when requested.
    value: Option<ValueRef>,
}

/// the `SelfVt` version number live immediately
/// BEFORE a value-type conditional's `case` — captured once, then handed to
/// every arm so both are true siblings starting from the same baseline, and
/// to the post-`case` rebind so the merged version is the single successor of
/// the pre-`case` one rather than of whichever arm generated last.
///
/// ADR 0122 / BT-3513: the family half of what used to be a hand-rolled
/// `VtCondSlots` (a `{self_vt: bool}`) is now a real
/// [`ThreadedFamilies`], computed once both arms are known (unchanged timing
/// — see [`CoreErlangGenerator::generate_vt_conditional_open`]). This
/// baseline struct survives that migration unchanged: `ThreadedFamilies`
/// only says WHICH families the conditional threads, never their version
/// numbers, so the pre-`case` snapshot each arm/the post-`case` rebind reads
/// still needs its own home. [`Self::version_for`] is the one place that
/// maps a [`VersionPrefix`] onto the matching field, so every call site
/// shares it instead of re-matching by hand.
#[derive(Debug, Clone, Copy)]
struct VtCondBaseline {
    self_vt: usize,
}

impl VtCondBaseline {
    /// The baseline version for `prefix` — `SelfVt` only; a value-type
    /// conditional's trailing family slot never carries any other
    /// [`VersionPrefix`] (class methods thread no family, ADR 0130).
    fn version_for(self, prefix: &VersionPrefix) -> usize {
        match prefix {
            VersionPrefix::SelfVt => self.self_vt,
            other => unreachable!(
                "a value-type conditional's trailing family slot only ever carries \
                 SelfVt, got {other:?}"
            ),
        }
    }
}

/// the class-side selectors that actually produce an instance of
/// `class`, for the `instantiation_error` hint raised by `rejected` .
///
/// A class method counts when its declared return type mentions the class
/// anywhere — directly (`now -> DateTime`) or nested in a generic
/// (`from: -> Result(Regex, Error)`). That deliberately skips the class-side
/// utilities these classes also carry (`DateTime monotonicNow -> Integer`,
/// `Uuid isValid: -> Boolean`).
///
/// Only the `rejected` selector itself is filtered out, not both `new` and
/// `new:`: `Queue` declares a working `class sealed new -> Queue` but no
/// `new:`, so its `new:` refusal should — and does — point back at `Queue new`.
///
/// Declaration order is preserved so the hint is deterministic, and the list is
/// capped at [`MAX_HINTED_CONSTRUCTORS`] so a wide class-side API cannot turn
/// one error message into a wall of text.
fn native_constructor_selectors(class: &ClassDefinition, rejected: &str) -> Vec<String> {
    class
        .class_methods
        .iter()
        .filter(|m| m.kind == MethodKind::Primary)
        .filter(|m| m.selector.name() != rejected)
        .filter(|m| {
            m.return_type
                .as_ref()
                .is_some_and(|t| type_mentions_class(t, class.name.name.as_str()))
        })
        .map(|m| m.selector.name().to_string())
        .collect()
}

/// BT-4473: the selectors a value type's `dispatch/3` answers ahead of its
/// method arms, in [`CoreErlangGenerator::generate_primitive_dispatch`]'s
/// fixed prefix. A method of
/// one of these names is never reached through `dispatch/3`, so it is never a
/// direct-call target. Pinned to the emitted prefix by
/// `dispatch_prefix_selectors_match_emitted_arms`.
pub(in crate::core_erlang) const DISPATCH_PREFIX_SELECTORS: &[&str] = &[
    "class",
    "respondsTo:",
    "fieldNames",
    "fieldAt:",
    "hasField:",
    "fieldAt:put:",
    "clearField:",
    "perform:",
    "perform:withArguments:",
];

/// BT-4473: an own-class `dispatch/3` method arm, which calls its function
/// as `Module:'selector'(Self, Arg1, …)` and answers its return as is
/// (BT-4503: a value-type method function answers the send's result, block
/// `^` or not).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) struct OwnDispatchArm {
    /// The selector's argument count (the function's arity less `Self`).
    pub(in crate::core_erlang) arity: usize,
}

/// How many constructors the `instantiation_error` hint names before
/// it stops. `DateTime` — the widest in the stdlib — declares seven.
const MAX_HINTED_CONSTRUCTORS: usize = 6;

/// Whether `annotation` names `class_name` at any depth.
///
/// `Self` counts too: in return position it resolves to the receiver class.
fn type_mentions_class(annotation: &TypeAnnotation, class_name: &str) -> bool {
    match annotation {
        TypeAnnotation::Simple(id) => id.name == class_name,
        TypeAnnotation::SelfType { .. } => true,
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            base.name == class_name
                || parameters
                    .iter()
                    .any(|p| type_mentions_class(p, class_name))
        }
        TypeAnnotation::Union { types, .. } => {
            types.iter().any(|t| type_mentions_class(t, class_name))
        }
        TypeAnnotation::FalseOr { inner, .. } => type_mentions_class(inner, class_name),
        TypeAnnotation::Difference { base, .. } => type_mentions_class(base, class_name),
        TypeAnnotation::Intersection { left, right, .. } => {
            type_mentions_class(left, class_name) || type_mentions_class(right, class_name)
        }
        TypeAnnotation::Shape { fields, .. } => fields
            .iter()
            .any(|f| type_mentions_class(&f.value, class_name)),
        // A metaclass return (`Self class`, `Foo class`) is not an instance,
        // and a singleton type never names a class.
        TypeAnnotation::SelfClass { .. }
        | TypeAnnotation::ClassOf { .. }
        | TypeAnnotation::Singleton { .. } => false,
    }
}

impl CoreErlangGenerator {
    /// Generates a value type module.
    ///
    /// Value types are plain Erlang terms (maps/records) with no process.
    /// They are created with `new` and `new:`, not `spawn`.
    ///
    /// ## Generated Structure
    ///
    /// ```erlang
    /// module 'point' ['new'/0, 'new'/1, 'plus'/2, ...]
    ///   attributes []
    ///
    /// 'new'/0 = fun () ->
    ///     ~{'$beamtalk_class' => 'Point', 'x' => 0, 'y' => 0}~
    ///
    /// 'new'/1 = fun (InitArgs) ->
    ///     DefaultState = call 'point':'new'(),
    ///     CheckedArgs = call 'beamtalk_class_instantiation':'check_init_args'('Point', 'new:', DefaultState, InitArgs, []),
    ///     call 'maps':'merge'(DefaultState, CheckedArgs)
    ///
    /// 'plus'/2 = fun (Self, Other) ->
    ///     % Direct function call, no gen_server
    ///     NewX = call 'erlang':'+'(
    ///         call 'maps':'get'('x', Self),
    ///         call 'maps':'get'('x', Other)
    ///     ),
    ///     ...
    ///     ~{'$beamtalk_class' => 'Point', 'x' => NewX, 'y' => NewY}~
    /// ```
    ///
    /// ## Key Differences from Actors
    ///
    /// - No `gen_server` behavior
    /// - No `spawn/0` or `spawn/1` - use `new/0` and `new/1` instead
    /// - Methods are synchronous functions operating on maps
    /// - No state threading (value types are immutable)
    /// - Methods return new instances rather than mutating
    pub(super) fn generate_value_type_module(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Set context to ValueType for this module
        self.context = CodeGenContext::ValueType;

        let class = module
            .classes
            .first()
            .ok_or_else(|| CodeGenError::Internal("Value type module has no class".to_string()))?;

        // Set class identity early so that class_name() returns the AST
        // class name rather than deriving from the module name.
        // BT-3666: carry `sealed`/`abstract` so class-side self-sends can stay
        // statically bound in a sealed class (nothing can override them).
        self.set_class_identity(Some(ClassIdentity::from_class_def(
            &class.name.name,
            class.is_sealed,
            class.is_abstract,
        )));
        // record field declared types so `self.<field>`
        // comparisons/arithmetic on object-typed fields route through the guard.
        self.set_class_field_types(class);

        // Check if the class explicitly defines new/new: methods
        // (e.g., object.bt defines `new => @primitive basicNew`)
        // If so, skip auto-generating constructors to avoid duplicate definitions
        // Only check Primary methods — check both instance and class methods
        let cf = self.semantic_facts.class_facts(&class.name.name);
        let has_explicit_new = cf
            .and_then(|cf| cf.instance_method_index("new"))
            .is_some_and(|i| class.methods[i].kind == MethodKind::Primary);
        let has_explicit_class_new = cf
            .and_then(|cf| cf.class_method_index("new"))
            .is_some_and(|i| class.class_methods[i].kind == MethodKind::Primary);
        let has_explicit_new_with = cf
            .and_then(|cf| cf.instance_method_index("new:"))
            .is_some_and(|i| class.methods[i].kind == MethodKind::Primary)
            || cf
                .and_then(|cf| cf.class_method_index("new:"))
                .is_some_and(|i| class.class_methods[i].kind == MethodKind::Primary);

        // Compute auto-generated slot methods for `Value subclass:` classes.
        // This is `None` for `ClassKind::Object` and `ClassKind::Actor`.
        let auto_methods = self.auto_slot_methods_and_own_arms(class);

        // The module's exports and attributes; `module_shell::core_module`
        // (below) builds the header around them.
        let exports = Self::build_value_type_exports(
            module,
            class,
            auto_methods.as_ref(),
            has_explicit_new,
            has_explicit_new_with,
        );
        let attributes = self.build_value_type_module_attributes(module, class);
        let mut docs: Vec<Document<'static>> = vec![Document::Str("\n")];

        // Generate new/0 - creates instance with default field values
        if !has_explicit_new {
            if has_explicit_class_new {
                // Class defines `class sealed new => @primitive "..."`, so new/0
                // delegates to the class method's primitive via class_new/2.
                docs.push(self.generate_delegating_new(class)?);
            } else {
                docs.push(self.generate_value_type_new(class)?);
            }
            docs.push(Document::Str("\n"));
        }

        // Generate new/1 - creates instance with initialization arguments.
        // Suppressed when a unary `new` instance method exists (has_explicit_new),
        // because that method compiles to 'new'/1 (Self parameter), which would clash
        // with the auto-generated 'new'/1 (InitArgs parameter).
        if !has_explicit_new_with && !has_explicit_new {
            docs.push(self.generate_value_type_new_with_args(class)?);
            docs.push(Document::Str("\n"));
        }

        // Generate instance methods as pure functions
        for method in &class.methods {
            docs.push(self.generate_value_type_method(method, class)?);
            docs.push(Document::Str("\n"));
        }

        // Auto-generate getter and with*: setter functions for Value subclass:
        let class_name_for_kw = self.class_name().clone();
        if let Some(ref auto) = auto_methods {
            for field in &auto.getters {
                docs.push(Self::generate_slot_getter(field));
                docs.push(Document::Str("\n"));
            }
            for field in &auto.setters {
                docs.push(Self::generate_slot_setter(field));
                docs.push(Document::Str("\n"));
            }
            docs.push(Self::generate_is_slot(&auto.getters));
            docs.push(Document::Str("\n"));
        }

        // Generate dispatch/3 and has_method/1 for all value types
        // (superclass delegation chain — same pattern as actors)
        docs.push(self.generate_primitive_dispatch(class, auto_methods.as_ref())?);
        docs.push(Document::Str("\n"));
        // Generate dispatch/4 for actor hierarchy walk.
        // The dispatch service only calls modules that export dispatch/4.
        docs.push(self.generate_dispatch_4(class)?);
        docs.push(Document::Str("\n"));
        docs.push(self.generate_primitive_has_method(class, auto_methods.as_ref())?);
        docs.push(Document::Str("\n"));

        // Generate superclass/0 for reflection
        docs.push(self.generate_superclass_function(module)?);

        // Generate class-side method functions
        if !class.class_methods.is_empty() {
            docs.push(self.generate_class_method_functions(class)?);
            docs.push(Document::Str("\n"));
        }

        // Auto-generate keyword constructor for Value subclass:
        // Value sub-subclasses delegate to new: to include inherited fields.
        let is_sub_subclass = class.class_kind == ClassKind::Value && {
            let sc = class.superclass_name();
            sc != "Value" && sc != "none"
        };
        if let Some(ref auto) = auto_methods {
            if let Some(ref kw_sel) = auto.keyword_constructor {
                let slots: Vec<String> = class
                    .state
                    .iter()
                    .map(|s| s.name.name.to_string())
                    .collect();
                docs.push(Self::generate_keyword_constructor_fn(
                    &class_name_for_kw,
                    kw_sel,
                    &slots,
                    is_sub_subclass,
                    &self.module_name,
                ));
                docs.push(Document::Str("\n"));
            }
        }

        // Register value type class with the class system for dynamic dispatch
        // Value types never need supervisionSpec synthesis (they are not actors).
        docs.push(self.generate_register_class(module, false)?);
        docs.push(Document::Str("\n"));

        // Generate __beamtalk_meta/0 for zero-process reflection
        docs.push(self.generate_meta_function(module, false)?);

        let (name, body) = (self.module_name.clone(), Document::Vec(docs));
        Ok(core_module(name, exports, attributes, body))
    }

    /// Builds the export list for a value type module header.
    ///
    /// Includes constructors (`new/0`, `new/1`), instance methods, auto-generated
    /// slot methods (`dispatch/3`, `has_method/1`, `superclass/0`, `register_class/0`), and class methods.
    fn build_value_type_exports(
        module: &Module,
        class: &ClassDefinition,
        auto_methods: Option<&AutoSlotMethods>,
        has_explicit_new: bool,
        has_explicit_new_with: bool,
    ) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        // new/0 is always needed: either the default constructor or a delegating one
        if !has_explicit_new {
            parts.push(Document::Str("'new'/0"));
        }
        // new/1 (the auto-generated initializer) is suppressed if the class
        // explicitly defines new: on either side (the class-side method
        // compiles to 'class_new'/2 instead) OR unary instance-side new
        // (which emits 'new'/1 itself via its Self parameter).
        if !has_explicit_new_with && !has_explicit_new {
            parts.push(Document::Str("'new'/1"));
        }

        // Add instance method exports (each takes Self as first parameter)
        for method in &class.methods {
            let arity = method.parameters.len() + 1; // +1 for Self parameter
            let mangled = method.selector.name().to_string();
            parts.push(leaf::fname(mangled, arity));
        }

        // Auto-generated getter and with*: setter exports for Value subclass:
        if let Some(auto) = auto_methods {
            for field in &auto.getters {
                parts.push(leaf::fname(field.clone(), 1));
            }
            for field in &auto.setters {
                let with_sel = AutoSlotMethods::with_star_selector(field);
                parts.push(leaf::fname(with_sel, 2));
            }
            // BT-4279: the slot-read fast path's question.
            parts.push(leaf::fname("__is_slot".to_string(), 1));
            if let Some(ref kw_sel) = auto.keyword_constructor {
                let num_slots = class.state.len();
                let arity = num_slots + 1; // ClassSelf + N slot args
                // Hash long keyword constructor atoms to stay within Erlang's 255-char atom limit.
                let safe_fn = super::selector_mangler::safe_class_method_fn_name(kw_sel);
                parts.push(leaf::fname(safe_fn, arity));
            }
        }

        // All value types export dispatch/3 and has_method/1
        // for runtime dispatch via superclass delegation chain
        parts.push(Document::Str("'dispatch'/3"));
        // All value types also export dispatch/4 for actor hierarchy walk.
        parts.push(Document::Str("'dispatch'/4"));
        parts.push(Document::Str("'has_method'/1"));
        // BT-4260: the strictly-local probe `resolve_owner/2` walks with
        // (always emitted by `generate_primitive_has_method`).
        parts.push(Document::Str("'has_method_local'/1"));

        // All classes export superclass/0 for reflection
        parts.push(Document::Str("'superclass'/0"));

        // Value types register with class system for dynamic dispatch
        parts.push(Document::Str("'register_class'/0"));

        // Static reflection metadata for zero-process queries
        parts.push(Document::Str("'__beamtalk_meta'/0"));

        // Class method exports: the one shared builder (hashes over-long selectors)
        docvec![
            join(parts, &Document::Str(", ")),
            Self::build_class_method_export_doc(module)
        ]
    }

    /// Builds the module's attribute list (the contents of `attributes [...]`).
    ///
    /// Assembles spec attributes, type aliases, and source-path metadata,
    /// including `on_load` and `beamtalk_class` annotations.
    fn build_value_type_module_attributes(
        &mut self,
        module: &Module,
        class: &ClassDefinition,
    ) -> Document<'static> {
        // Generate spec attributes from type annotations
        // use the generator's cross-module-aware alias
        // registry (this module's own `type_aliases` merged with any
        // pre-loaded aliases from other modules in the same compilation
        // unit — see `CoreErlangGenerator::alias_registry`'s doc) so an
        // alias-named annotation resolves to a `user_type` reference (ADR
        // 0108) instead of falling through to `any()`, regardless of which
        // module declared the alias.
        // tracks which alias names the specs/type alias below
        // actually reference, so `generate_alias_type_attrs` only emits
        // `-type` declarations for those (plus transitive deps) instead of
        // every pre-loaded alias in the compilation unit.
        let referenced_aliases: RefCell<HashSet<_>> = RefCell::new(HashSet::new());
        let spec_attrs = spec_codegen::generate_class_specs(
            class,
            true,
            Some(&self.alias_registry),
            Some(&referenced_aliases),
        );
        let spec_suffix: Document<'static> = spec_codegen::format_spec_attributes(&spec_attrs)
            .map_or(Document::Nil, |s| docvec![",\n     ", s]);

        // Generate -type t() alias for Value classes with state: declarations.
        let class_name_for_type = self.class_name();
        let type_alias_opt = spec_codegen::generate_type_alias(
            class,
            &class_name_for_type,
            Some(&self.alias_registry),
            Some(&referenced_aliases),
        );
        let export_type_suffix: Document<'static> = if type_alias_opt.is_some() {
            Document::Str(",\n     'export_type' = [{'t', 0}]")
        } else {
            Document::Nil
        };
        let type_alias_suffix: Document<'static> =
            type_alias_opt.map_or(Document::Nil, |s| docvec![",\n     ", s]);

        // every class module that could contain a `user_type`
        // reference must also declare the matching named `-type` in the same
        // module attribute list (an `erlc` compile error otherwise) — empty
        // for a module with no `type_aliases`, so this is a no-op change for
        // the common case.
        let alias_type_attrs =
            spec_codegen::generate_alias_type_attrs(&self.alias_registry, &referenced_aliases);
        let alias_type_suffix: Document<'static> =
            spec_codegen::format_alias_type_attributes(&alias_type_attrs)
                .map_or(Document::Nil, |s| docvec![",\n     ", s]);

        // Build beamtalk_class attribute for dependency-ordered bootstrap
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);
        // Source-path and file attributes for stacktraces
        let source_path_attr = self.source_path_attr();
        let file_attr = self.file_attr();

        docvec![
            "'on_load' = [{'register_class', 0}]",
            beamtalk_class_attr,
            file_attr,
            source_path_attr,
            alias_type_suffix,
            type_alias_suffix,
            export_type_suffix,
            spec_suffix,
        ]
    }

    /// Generates the `new/0` function for a value type.
    ///
    /// - Non-instantiable primitives (Integer, String, etc.): raises `instantiation_error`
    /// - Collection primitives (Dictionary, List, Tuple): returns empty native value
    /// - `native:` classes carrying no declared fields: raises `instantiation_error`
    /// - Other value types: creates an instance map with `$beamtalk_class` and defaults
    fn generate_value_type_new(&mut self, class: &ClassDefinition) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        if Self::is_non_instantiable_primitive(class_name.as_str()) {
            return self.generate_primitive_new_error(class_name.as_str(), "new", 0);
        }
        if let Some(empty_val) = Self::collection_empty_value(class_name.as_str()) {
            return Ok(Self::generate_collection_new(empty_val));
        }
        // a hollow-instance `new` is worse than no `new` at all.
        if has_opaque_native_representation(class) {
            return Ok(Self::generate_native_new_error(class, "new", 0));
        }

        // For Value sub-subclasses (superclass is not "Value" itself),
        // chain to the parent's new/0 and merge own fields on top.  This ensures
        // inherited fields are included in the default instance map.
        // Only applies to ClassKind::Value — Object subclasses don't chain.
        let superclass_name = class.superclass_name();
        if class.class_kind == ClassKind::Value
            && superclass_name != "Value"
            && superclass_name != "none"
        {
            if let Some(super_mod) = self.superclass_module_name(superclass_name) {
                let mut own_field_parts: Vec<Document<'static>> = Vec::new();
                own_field_parts.push(docvec![
                    "'$beamtalk_class' => ",
                    leaf::atom(class_name.clone()),
                ]);
                // BT-4591: defaults that run code are bound in source order.
                let states: Vec<&beamtalk_core::ast::StateDeclaration> =
                    class.state.iter().collect();
                let (prelude, values) = self.thread_default_values(&states)?;
                for (field, default_code) in class.state.iter().zip(values) {
                    own_field_parts.push(docvec![
                        leaf::atom(field.name.name.clone()),
                        " => ",
                        default_code,
                    ]);
                }
                return Ok(docvec![
                    "'new'/0 = fun () ->\n",
                    "    let ParentState = call ",
                    leaf::atom(super_mod),
                    ":'new'() in\n",
                    "    ",
                    prelude,
                    "call 'maps':'merge'(ParentState, ~{",
                    join(own_field_parts, &Document::Str(", ")),
                    "}~)\n",
                    "\n",
                ]);
            }
        }

        // Direct Value subclass: build a flat map with all own fields.
        let mut field_parts: Vec<Document<'static>> = Vec::new();

        // Add each field with its default value. BT-4591: defaults that run
        // code are bound in source order ahead of the map.
        let states: Vec<&beamtalk_core::ast::StateDeclaration> = class.state.iter().collect();
        let (prelude, values) = self.thread_default_values(&states)?;
        for (field, default_code) in class.state.iter().zip(values) {
            field_parts.push(docvec![
                ", ",
                leaf::atom(field.name.name.clone()),
                " => ",
                default_code,
            ]);
        }

        Ok(docvec![
            "'new'/0 = fun () ->\n",
            "    ",
            prelude,
            "~{'$beamtalk_class' => '",
            leaf::var(class_name),
            "'",
            concat(field_parts),
            "}~\n",
            "\n",
        ])
    }

    /// Generates `new/0` that delegates to a class method `new`.
    ///
    /// When a class defines `class sealed new => <body>`, the auto-generated
    /// `new/0` must call through to the implementation rather than creating
    /// an empty tagged map. This ensures `handle_new_compiled` (which calls
    /// `Module:new/0`) uses the correct constructor.
    ///
    /// Two cases:
    /// 1. Body is `@primitive "selector"` — inline the BIF call directly.
    /// 2. Body is an FFI call (ADR 0055) — delegate to the compiled
    ///    `class_new/1` function with `undefined` for `ClassSelf`.
    fn generate_delegating_new(&mut self, class: &ClassDefinition) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();

        // Find the class method `new`
        let new_method = self
            .semantic_facts
            .class_facts(&class_name)
            .and_then(|cf| cf.class_method_index("new"))
            .and_then(|i| {
                let m = &class.class_methods[i];
                (m.kind == MethodKind::Primary).then_some(m)
            })
            .ok_or_else(|| {
                CodeGenError::Internal("Expected class method 'new' not found".to_string())
            })?;

        // Check whether the method body is a @primitive annotation.
        let prim_name = new_method.body.iter().find_map(|stmt| {
            if let beamtalk_core::ast::Expression::Primitive { name, .. } = &stmt.expression {
                Some(name.clone())
            } else {
                None
            }
        });

        if let Some(prim_name) = prim_name {
            // basicNew intrinsic = standard value constructor.
            // Generate the normal auto-generated new/0 (map constructor) instead
            // of trying to inline a BIF. The class_new/1 function (generated
            // separately by generate_class_method_functions) handles class-side
            // dispatch via class_self_new.
            if prim_name.as_str() == "basicNew" {
                return self.generate_value_type_new(class);
            }

            // Original path: inline the BIF call for the @primitive.
            let bif_doc = super::primitives::generate_primitive_bif(&class_name, &prim_name, &[])
                .ok_or_else(|| {
                CodeGenError::Internal(format!(
                    "Failed to generate BIF for primitive '{prim_name}' in class method 'new'"
                ))
            })?;
            Ok(docvec![
                "'new'/0 = fun () ->\n",
                "    ",
                bif_doc,
                "\n",
                "\n",
            ])
        } else {
            // ADR 0055 FFI path: delegate to the compiled class_new/1 function.
            // class_new/1 is generated from the `class sealed new => (Erlang M) fn`
            // method body. `ClassSelf` is unused for Object subclasses.
            let module_name = self.module_name.clone();
            Ok(docvec![
                "'new'/0 = fun () ->\n",
                "    call ",
                leaf::atom(module_name),
                ":'class_new'('undefined')\n",
                "\n",
            ])
        }
    }

    /// Generates the `new/1` function for a value type.
    ///
    /// - Non-instantiable primitives: raises `instantiation_error`
    /// - Dictionary: returns `InitArgs` directly (dictionary IS a map)
    /// - List, Tuple: raises `instantiation_error` (no meaningful init-from-map)
    /// - `native:` classes carrying no declared fields: raises `instantiation_error`
    /// - Other value types: validates initialization arguments against the
    ///   declared fields (BT-4030 `#unknown_field`), then merges
    fn generate_value_type_new_with_args(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        if Self::is_non_instantiable_primitive(class_name.as_str()) {
            return self.generate_primitive_new_error(class_name.as_str(), "new:", 1);
        }
        // Dictionary new: #{key => val} returns the map directly
        if class_name == "Dictionary" {
            return Ok(Self::generate_collection_new_with_args());
        }
        // List and Tuple have no meaningful init-from-map pattern
        if Self::collection_empty_value(class_name.as_str()).is_some() {
            return self.generate_primitive_new_error(class_name.as_str(), "new:", 1);
        }
        // `new:` merges over what `new` builds, so an opaque native
        // representation leaves it just as hollow — same error, same guidance.
        if has_opaque_native_representation(class) {
            return Ok(Self::generate_native_new_error(class, "new:", 1));
        }

        let module_name = self.module_name.clone();
        Ok(docvec![
            "'new'/1 = fun (InitArgs) ->\n",
            "    let DefaultState = call '",
            leaf::var(module_name),
            "':'new'() in\n",
            // BT-4030: validate against the declared fields (resolved from
            // metadata at runtime, so inherited fields stay accepted).
            "    let CheckedArgs = ",
            super::errors::check_init_args_call_doc(
                leaf::atom(class_name.clone()),
                "new:",
                leaf::var("DefaultState".to_string()),
                leaf::var("InitArgs".to_string()),
                &[],
            ),
            " in\n",
            "    call 'maps':'merge'(DefaultState, CheckedArgs)\n",
            "\n",
        ])
    }

    /// Generates a `new` or `new:` function that raises `instantiation_error`
    /// for primitive types that cannot be instantiated with `new`.
    #[allow(clippy::unnecessary_wraps, clippy::unused_self)] // uniform codegen interface
    fn generate_primitive_new_error(
        &mut self,
        class_name: &str,
        selector: &str,
        arity: usize,
    ) -> Result<Document<'static>> {
        let hint =
            format!("{class_name} is a primitive type and cannot be instantiated with {selector}");

        let function_head = if arity == 0 {
            "'new'/0 = fun () ->"
        } else {
            "'new'/1 = fun (_InitArgs) ->"
        };

        Ok(docvec![
            function_head,
            nest(
                INDENT,
                docvec![
                    line(),
                    Self::instantiation_error_expr(class_name, selector, &hint)
                ]
            ),
            "\n",
            "\n",
        ])
    }

    /// generates a `new`/`new:` function that raises `instantiation_error`
    /// for a `native:` class whose instances only the backing module can build.
    ///
    /// The hint names the class's own class-side constructors (see
    /// [`native_constructor_selectors`]) so the message points at the way out
    /// rather than just refusing, e.g.
    ///
    /// ```text
    /// DateTime instances are built by the beamtalk_datetime module, not from
    /// field defaults — `new` cannot produce a usable one. Use one of:
    /// DateTime now, DateTime year:month:day:, DateTime fromTimestamp:
    /// ```
    fn generate_native_new_error(
        class: &ClassDefinition,
        selector: &str,
        arity: usize,
    ) -> Document<'static> {
        let class_name = class.name.name.as_str();
        let hint = Self::native_new_error_hint(class, selector);

        let function_head = if arity == 0 {
            "'new'/0 = fun () ->"
        } else {
            "'new'/1 = fun (_InitArgs) ->"
        };

        docvec![
            function_head,
            nest(
                INDENT,
                docvec![
                    line(),
                    Self::instantiation_error_expr(class_name, selector, &hint)
                ]
            ),
            "\n",
            "\n",
        ]
    }

    /// the hint text baked into [`Self::generate_native_new_error`].
    ///
    /// Split out so it can be asserted on directly — in the emitted Core Erlang
    /// it is a binary literal, i.e. a per-byte segment list.
    fn native_new_error_hint(class: &ClassDefinition, selector: &str) -> String {
        let class_name = class.name.name.as_str();
        let backing = class
            .backing_module
            .as_ref()
            .map_or("its backing", |m| m.name.as_str());

        let mut hint = format!(
            "{class_name} instances are built by the {backing} module, not from field \
             defaults — `{selector}` cannot produce a usable one."
        );
        let constructors = native_constructor_selectors(class, selector);
        if constructors.is_empty() {
            hint.push_str(" It has no class-side constructor.");
            return hint;
        }
        let _ = write!(
            hint,
            " Use one of: {}",
            constructors
                .iter()
                .take(MAX_HINTED_CONSTRUCTORS)
                .map(|sel| format!("{class_name} {sel}"))
                .collect::<Vec<_>>()
                .join(", ")
        );
        if constructors.len() > MAX_HINTED_CONSTRUCTORS {
            let _ = write!(
                hint,
                ", and {} more",
                constructors.len() - MAX_HINTED_CONSTRUCTORS
            );
        }
        hint
    }

    /// Generates `new/0` for a collection type that returns an empty native value.
    fn generate_collection_new(empty_value: &str) -> Document<'static> {
        docvec![
            "'new'/0 = fun () ->\n",
            "    ",
            leaf::var(empty_value.to_string()),
            "\n",
            "\n",
        ]
    }

    /// Generates `new/1` for Dictionary: returns `InitArgs` directly.
    fn generate_collection_new_with_args() -> Document<'static> {
        docvec!["'new'/1 = fun (InitArgs) ->\n", "    InitArgs\n", "\n",]
    }

    /// Returns true if the class is a non-instantiable primitive type.
    ///
    /// These types have no sensible default/empty value and cannot be
    /// instantiated with `new`. For example, `Integer new` makes no sense —
    /// integers are created via literals (`42`).
    ///
    /// Collection types (Dictionary, List, Tuple) are NOT included here
    /// because they have sensible empty values (e.g., `Dictionary new` → `#{}`).
    fn is_non_instantiable_primitive(class_name: &str) -> bool {
        matches!(
            class_name,
            "Integer"
                | "Float"
                | "String"
                | "True"
                | "False"
                | "UndefinedObject"
                | "Block"
                | "Symbol"
                | "CompiledMethod"
                | "Erlang"
                | "ErlangModule"
        )
    }

    /// Returns the Core Erlang literal for a collection type's empty value,
    /// or `None` if the class is not a collection primitive.
    fn collection_empty_value(class_name: &str) -> Option<&'static str> {
        match class_name {
            "Dictionary" => Some("~{}~"),
            "List" => Some("[]"),
            "Tuple" => Some("{}"),
            _ => None,
        }
    }

    /// Returns true if the class is a known stdlib type (ADR 0016).
    ///
    /// All stdlib types compile to `bt@stdlib@{snake_case}` modules.
    ///
    /// ADR 0119 step 0: delegates to
    /// `ClassHierarchy::is_generated_builtin_class`, the one correct,
    /// already-parsed answer (from `beamtalk build-stdlib`'s real class
    /// metadata) — replacing the deleted `STDLIB_CLASS_NAMES` (`build.rs`'s
    /// file-stem directory scan), which included protocol-only files
    /// declaring no class and trusted file stems over parsed names.
    fn is_known_stdlib_type(class_name: &str) -> bool {
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::is_generated_builtin_class(
            class_name,
        )
    }

    /// Computes the compiled module name for a class (ADR 0016 / ADR 0026;
    /// registry lookup per ADR 0119).
    ///
    /// Resolution order:
    /// 1. [`ClassModuleRegistry::module_for_class`] — built from
    ///    `class_module_index` (the explicit mapping from two-pass
    ///    compilation) keyed under this unit's own `PackageId`
    ///    ([`CoreErlangGenerator::own_package_id`]). This correctly handles
    ///    classes in package subdirectories (e.g. `SchemeEnv` →
    ///    `bt@sicp_example@scheme@env`) — no guessing, no fallback tiers.
    /// 2. On a genuine miss (ADR 0100's open-world policy: an unregistered
    ///    class reference is a warning, not an error, and must still reach
    ///    codegen), the existing best-effort convention: stdlib classes →
    ///    `bt@stdlib@{snake_case}`; a user-defined class in package mode →
    ///    `bt@{package}@{snake_case}`; otherwise → `bt@{snake_case}`.
    ///
    /// [`ClassModuleRegistry::module_for_class`]: beamtalk_core::semantic_analysis::ClassModuleRegistry::module_for_class
    pub fn compiled_module_name(&self, class_name: &str) -> String {
        let pkg = self.own_package_id();
        if let Some(module) = self
            .class_module_registry()
            .module_for_class(&pkg, class_name)
        {
            return module.as_str().to_string();
        }
        let snake = super::util::to_module_name(class_name);
        if Self::is_known_stdlib_type(class_name) {
            format!("bt@stdlib@{snake}")
        } else if let beamtalk_core::semantic_analysis::PackageId::Package(name) = pkg {
            format!("bt@{name}@{snake}")
        } else {
            format!("bt@{snake}")
        }
    }

    /// Computes the compiled module name for a package-qualified class reference
    /// (ADR 0070 Phase 2; registry lookup per ADR 0119).
    ///
    /// When a class reference has an explicit package qualifier (e.g., `json@Parser`),
    /// first queries the registry under the *referenced* package's `PackageId` —
    /// closing the divergence where a qualified reference to a class in a
    /// package subdirectory used to disagree with the same class's unqualified
    /// resolution (ADR 0119 Context item 5). On a registry miss, falls back to
    /// `resolve_qualified_module_name`'s deterministic `bt@{package}@{snake_case}`
    /// composition — `resolve_qualified_module_name` itself is unchanged; its
    /// `None`-arm contract is deliberate and tested (ADR 0119 Decision).
    ///
    /// When no package qualifier is present (`package` is `None`), falls back to
    /// `compiled_module_name` for backward-compatible resolution.
    pub fn compiled_module_name_qualified(
        &self,
        class_name: &str,
        package: Option<&str>,
    ) -> String {
        match package {
            Some(pkg) => {
                let target = beamtalk_core::semantic_analysis::PackageId::Package(pkg.to_string());
                self.class_module_registry()
                    .module_for_class(&target, class_name)
                    .map_or_else(
                        || beamtalk_core::ast::resolve_qualified_module_name(class_name, Some(pkg)),
                        |module| module.as_str().to_string(),
                    )
            }
            None => self.compiled_module_name(class_name),
        }
    }

    /// Computes the module name for a superclass in the dispatch chain.
    ///
    /// Returns `None` only for `ProtoObject`'s own superclass (`nil`, spelled
    /// `"none"` by `ClassDefinition::superclass_name`) — `ProtoObject` is the root
    /// of the hierarchy, so its `dispatch/3` raises `does_not_understand` and
    /// there is no module to delegate to. `Object` (and any other `ProtoObject
    /// subclass:`) delegates to `bt@stdlib@proto_object`, so the methods
    /// `ProtoObject` defines (`==`, `/=`, `=:=`, `=/=`, …) are reachable by a
    /// dynamic send to any value (BT-3919). For stdlib classes, applies the
    /// ADR 0016 naming convention: all stdlib types → `bt@stdlib@{snake_case}`.
    ///
    /// For user-defined classes in package mode, uses `bt@{package}@{snake_case}`
    /// prefix.
    pub(super) fn superclass_module_name(&self, superclass: &str) -> Option<String> {
        if superclass == "none" {
            return None;
        }
        Some(self.compiled_module_name(superclass))
    }
}

mod body;
mod conditionals;
mod dispatch;

#[cfg(test)]
mod tests;
