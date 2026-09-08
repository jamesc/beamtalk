// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `__beamtalk_meta/0` construction (ADR 0068/0070/0098).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Static reflection metadata embedded directly in the compiled BEAM module:
//! class name, superclass, fields, instance/class method signatures
//! (including generic-type and toolchain-provenance encoding), and the
//! `BuilderState.meta` variant `generate_register_class` bakes into
//! `register_class/0` for use during `init/1` (before `__beamtalk_meta/0`
//! itself is callable — ADR 0050 Phase 5).
//!
//! Kind-agnostic: called for actor, value-type, and native-facade classes
//! alike — split out of `gen_server/methods.rs`, which was actor-specific
//! in name but carried this shared logic.

use super::gen_server::extract_package_from_module_name;
use super::value_accessors::{AutoSlotMethods, compute_auto_slot_methods};
use super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::{
    ClassDefinition, ClassKind, MethodDefinition, MethodKind, Module, StateDeclaration,
    TypeParamDecl,
};
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;

/// ADR 0098 Phase 3: producing-toolchain identity baked into a module's
/// `__beamtalk_meta/0` map so a *loaded* module is self-describing — consumers
/// (workspace attach, tooling) can detect staleness without re-reading the
/// on-disk stamp.
///
/// Both values are compile-time literals supplied by the CLI
/// (`beam_compiler.rs`), never a runtime `erlang:system_info/1` call in the
/// generated module: the latter would bake the bare OTP release (`"27"`) rather
/// than the compound key the stamp uses (`"27-15.0.1"`). `None` (REPL, tests, or
/// an older toolchain) omits the key entirely.
#[derive(Clone, Copy, Default)]
pub(crate) struct MetaProvenance<'a> {
    /// The producing `BEAMTALK_VERSION`, verbatim.
    pub beamtalk_version: Option<&'a str>,
    /// The producing compound OTP version (`<release>-<erts>`).
    pub otp_release: Option<&'a str>,
}

/// Representation of a type in runtime meta (`method_info` `return_type` / `param_types`).
///
/// ADR 0068: Generic classes emit `{type_param, Name, Index}` tagged tuples
/// for type parameters and `{generic, Base, [Params]}` for parameterised types,
/// rather than flat atom strings.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum MetaTypeRepr {
    /// No type annotation — rendered as `'none'`.
    None,
    /// A concrete named type — rendered as `'TypeName'`.
    Atom(String),
    /// A reference to a class-level type parameter — rendered as
    /// `{'type_param', 'Name', Index}`.
    TypeParam { name: String, index: i32 },
    /// A parameterised type — rendered as
    /// `{'generic', 'Base', [Param1, Param2, ...]}`.
    Generic {
        base: String,
        parameters: Vec<MetaTypeRepr>,
    },
    /// A union type (BT-3076) — rendered as `{'union', [Member1, Member2, ...]}`.
    Union(Vec<MetaTypeRepr>),
    /// A singleton/literal type (BT-3076), e.g. `#north` — rendered as
    /// `{'singleton', 'north'}` (the name, without the leading `#`).
    Singleton(String),
}

/// Tuple representing a method entry for `method_info` / `class_method_info` meta maps.
///
/// Fields: (`erlang_selector`, `arity`, `return_type`, `param_types`, `is_sealed`, `is_internal`)
pub(super) type MethodInfoEntry = (String, usize, MetaTypeRepr, Vec<MetaTypeRepr>, bool, bool);

impl CoreErlangGenerator {
    /// Generates the `__beamtalk_meta/0` function (BT-942).
    ///
    /// Embeds static reflection metadata directly in the compiled BEAM module.
    /// This enables zero-process reflection queries for structural data:
    /// class name, superclass, fields, instance methods, and class methods.
    ///
    /// Dynamic classes created via `beamtalk_class_builder` do not have this function;
    /// the runtime falls back to `gen_server` calls when `erlang:function_exported/3` (BIF)
    /// returns false.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// '__beamtalk_meta'/0 = fun () ->
    ///     ~{'class' => 'Counter',
    ///       'superclass' => 'Actor',
    ///       'fields' => ['value'],
    ///       'class_fields' => ['total'],
    ///       'methods' => [{'increment', 0}, {'decrement', 0}, {'getValue', 0}],
    ///       'class_methods' => [{'new', 0}]
    ///     }~
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_meta_function(
        &self,
        module: &Module,
        synthesize_supervision_spec: bool,
    ) -> Result<Document<'static>> {
        let Some(class) = module.classes.first() else {
            return Ok(Document::Nil);
        };

        // ADR 0070 Phase 4: Extract package name from BEAM module name (bt@{package}@{class})
        let package_name = extract_package_from_module_name(&self.module_name);

        Ok(docvec![
            "'__beamtalk_meta'/0 = fun () ->\n",
            "    ",
            // include_standalone: false — standalone methods are runtime-patched, not static
            Self::build_meta_map_doc(
                class,
                module,
                false,
                synthesize_supervision_spec,
                package_name.as_deref(),
                self.meta_provenance(),
            ),
            "\n\n",
        ])
    }

    /// Builds the Core Erlang map document for the static class metadata.
    ///
    /// Used by both `generate_meta_function` (for `__beamtalk_meta/0`) and
    /// `generate_register_class` (for the `'meta'` key in `BuilderState`).
    ///
    /// ADR 0050 Phase 5: `erlang:function_exported/3` returns `false` during `on_load`,
    /// so `__beamtalk_meta/0` cannot be called from within the `on_load` callback chain.
    /// Including this map literal in `BuilderState` makes the data available during `init/1`.
    ///
    /// When `include_standalone` is `false` (used for `__beamtalk_meta/0`), standalone
    /// Tonel-style methods (`module.method_definitions`) are excluded — they are
    /// runtime-patched and deliberately absent from the static meta. When `true`
    /// (used for `BuilderState.meta`), standalone methods are included so that
    /// return-type information is available to `init/1` during `on_load`.
    pub(super) fn build_meta_map_doc(
        class: &ClassDefinition,
        module: &Module,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
        package_name: Option<&str>,
        provenance: MetaProvenance<'_>,
    ) -> Document<'static> {
        Self::build_meta_map_doc_with_extra(
            class,
            module,
            include_standalone,
            synthesize_supervision_spec,
            Document::Nil,
            package_name,
            provenance,
        )
    }

    /// Like `build_meta_map_doc` but appends extra map entries before closing the map.
    ///
    /// Used by native facade codegen to add `'native'` and `'backing_module'` keys
    /// while reusing the standard meta map structure.
    #[allow(clippy::too_many_lines)] // one contiguous map literal; splitting hurts readability
    pub(super) fn build_meta_map_doc_with_extra(
        class: &ClassDefinition,
        module: &Module,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
        extra_entries: Document<'static>,
        package_name: Option<&str>,
        provenance: MetaProvenance<'_>,
    ) -> Document<'static> {
        let class_name = class.name.name.to_string();
        let superclass_name = class
            .superclass
            .as_ref()
            .map_or_else(|| "nil".to_string(), |s| s.name.to_string());

        // Build fields list from instance state declarations
        let fields: Vec<String> = class
            .state
            .iter()
            .map(|s| s.name.name.to_string())
            .collect();

        let fields_doc = Self::meta_atom_list(&fields);

        // BT-2238: Build class-side field list from `classState:` declarations so
        // class-side slots are reflectable (`Behaviour>>classVarNames` /
        // `allClassVarNames`). The instance `fields` key above carries instance
        // state only.
        let class_fields: Vec<String> = class
            .class_variables
            .iter()
            .map(|s| s.name.name.to_string())
            .collect();

        let class_fields_doc = Self::meta_atom_list(&class_fields);

        // Boolean flags
        let is_sealed_doc = Self::meta_bool(class.is_sealed);
        let is_abstract_doc = Self::meta_bool(class.is_abstract);
        let is_value_doc = Self::meta_bool(class.class_kind == ClassKind::Value);
        let is_typed_doc = Self::meta_bool(class.is_typed);
        let is_internal_doc = Self::meta_bool(class.is_internal);

        // ADR 0071 Phase 4: Emit class-level visibility
        let visibility_doc: Document<'static> = if class.is_internal {
            Document::Str("'internal'")
        } else {
            Document::Str("'public'")
        };

        // field_types: map of field name → declared type atom or 'none'
        let field_types_doc = Self::meta_field_types_map(&class.state);

        // BT-1976: field_has_default — map of field name → 'true' | 'false'.
        // Cross-file consumers use this to identify typed-no-default fields
        // without the AST (post-initialize validation in gen_server codegen).
        let field_has_default_doc = Self::meta_field_has_default_map(&class.state);

        // Compute auto-slot methods once and share across method_info / class_method_info
        let auto = compute_auto_slot_methods(class);
        let method_info_doc = Self::meta_method_info_map(&Self::meta_instance_method_entries(
            class,
            module,
            auto.as_ref(),
            include_standalone,
        ));
        let class_method_info_doc = Self::meta_method_info_map(&Self::meta_class_method_entries(
            class,
            module,
            auto.as_ref(),
            include_standalone,
            synthesize_supervision_spec,
        ));

        // ADR 0068: Emit type_params list for generic classes
        let type_params_doc = Self::meta_atom_list(
            &class
                .type_params
                .iter()
                .map(|tp| tp.name.name.to_string())
                .collect::<Vec<_>>(),
        );

        // ADR 0070 Phase 4: Emit package name as compile-time constant
        let package_doc: Document<'static> = match package_name {
            Some(pkg) => leaf::atom(pkg.to_string()),
            None => Document::Str("'none'"),
        };

        // ADR 0070 Phase 4: Emit ClassKind as atom (object | value | actor)
        let kind_doc: Document<'static> = match class.class_kind {
            ClassKind::Object => Document::Str("'object'"),
            ClassKind::Value => Document::Str("'value'"),
            ClassKind::Actor => Document::Str("'actor'"),
        };

        // ADR 0103: emit the declared sendability handle scope as an atom, only
        // when present — keeps meta output stable for the vast majority of
        // classes that declare none (mirrors the provenance keys' pattern).
        let handle_scope_doc: Document<'static> = match &class.handle_scope {
            Some(sym) => docvec![
                ",\n      'handle_scope' => ",
                leaf::atom(sym.name.to_string()),
            ],
            None => Document::Nil,
        };

        docvec![
            "~{'class' => ",
            leaf::atom(class_name),
            ",\n      'superclass' => ",
            leaf::atom(superclass_name),
            ",\n      'package' => ",
            package_doc,
            ",\n      'kind' => ",
            kind_doc,
            ",\n      'fields' => ",
            fields_doc,
            ",\n      'class_fields' => ",
            class_fields_doc,
            ",\n      'is_sealed' => ",
            is_sealed_doc,
            ",\n      'is_abstract' => ",
            is_abstract_doc,
            ",\n      'is_value' => ",
            is_value_doc,
            ",\n      'is_typed' => ",
            is_typed_doc,
            ",\n      'is_internal' => ",
            is_internal_doc,
            ",\n      'visibility' => ",
            visibility_doc,
            ",\n      'type_params' => ",
            type_params_doc,
            ",\n      'field_types' => ",
            field_types_doc,
            ",\n      'field_has_default' => ",
            field_has_default_doc,
            ",\n      'method_info' => ",
            method_info_doc,
            ",\n      'class_method_info' => ",
            class_method_info_doc,
            // ADR 0103: sendability handle scope (omitted when undeclared).
            handle_scope_doc,
            // ADR 0098 Phase 3: producing-toolchain identity (omitted when unknown).
            Self::meta_provenance_entries(provenance),
            extra_entries,
            "\n    }~",
        ]
    }

    /// ADR 0098 Phase 3: emit the `beamtalk_version` / `otp_release` provenance
    /// keys for `__beamtalk_meta`, as binary string literals.
    ///
    /// Each key is emitted only when known: an older toolchain (and REPL/test
    /// codegen) leaves them absent, which `__beamtalk_meta` readers treat as a
    /// provenance miss (stale → recompile), never an error. Both values are
    /// compile-time literals from the CLI — never a runtime `erlang:system_info/1`
    /// call, which would bake the bare OTP release rather than the compound key.
    fn meta_provenance_entries(provenance: MetaProvenance<'_>) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        if let Some(version) = provenance.beamtalk_version {
            parts.push(Document::Str(",\n      'beamtalk_version' => "));
            parts.push(leaf::binary_lit(version));
        }
        if let Some(otp_release) = provenance.otp_release {
            parts.push(Document::Str(",\n      'otp_release' => "));
            parts.push(leaf::binary_lit(otp_release));
        }
        Document::Vec(parts)
    }

    /// Builds a Core Erlang atom list document from a slice of string names.
    ///
    /// Example: `["field1", "field2"]` → `['field1', 'field2']`
    /// Empty slice → `[]`
    pub(super) fn meta_atom_list(names: &[String]) -> Document<'static> {
        if names.is_empty() {
            return Document::Str("[]");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("["));
        for (i, name) in names.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(leaf::atom(name.clone()));
        }
        parts.push(Document::Str("]"));
        Document::Vec(parts)
    }

    /// Produces the Core Erlang atom for a boolean value.
    pub(super) fn meta_bool(b: bool) -> Document<'static> {
        if b {
            Document::Str("'true'")
        } else {
            Document::Str("'false'")
        }
    }

    /// BT-1976: Builds a field-has-default map for `__beamtalk_meta/0`.
    ///
    /// Example: `[StateDecl{name: "count", default: Some(0)}]` → `~{'count' => 'true'}~`
    /// Empty slice → `~{}~`
    ///
    /// Cross-file consumers read this to identify typed-no-default fields when
    /// the class's AST is not in the current compilation unit.
    pub(super) fn meta_field_has_default_map(state: &[StateDeclaration]) -> Document<'static> {
        if state.is_empty() {
            return Document::Str("~{}~");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("~{"));
        for (i, s) in state.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let flag = if s.default_value.is_some() {
                Document::Str("'true'")
            } else {
                Document::Str("'false'")
            };
            parts.push(docvec![leaf::atom(s.name.name.to_string()), " => ", flag,]);
        }
        parts.push(Document::Str("}~"));
        Document::Vec(parts)
    }

    /// Builds a Core Erlang map of field name → declared type atom or `'none'`.
    ///
    /// Example: `[StateDecl{name: "value", type: Integer}]` → `~{'value' => 'Integer'}~`
    /// Empty slice → `~{}~`
    pub(super) fn meta_field_types_map(state: &[StateDeclaration]) -> Document<'static> {
        if state.is_empty() {
            return Document::Str("~{}~");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("~{"));
        for (i, s) in state.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let type_doc = match &s.type_annotation {
                Some(ta) => leaf::atom(ta.type_name().to_string()),
                None => Document::Str("'none'"),
            };
            parts.push(docvec![
                leaf::atom(s.name.name.to_string()),
                " => ",
                type_doc,
            ]);
        }
        parts.push(Document::Str("}~"));
        Document::Vec(parts)
    }

    pub(super) fn meta_instance_method_entries(
        class: &ClassDefinition,
        module: &Module,
        auto: Option<&AutoSlotMethods>,
        include_standalone: bool,
    ) -> Vec<MethodInfoEntry> {
        let sealed = class.is_sealed;
        let type_params = &class.type_params;
        let mut entries: Vec<MethodInfoEntry> = class
            .methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| Self::meta_method_entry(m, type_params))
            .collect();
        // BT-1005: Standalone methods are excluded from __beamtalk_meta/0 (runtime-patched)
        // but included in BuilderState.meta so init/1 can register their return types.
        if include_standalone {
            for standalone in module.method_definitions.iter().filter(|m| {
                m.class_name.name == class.name.name
                    && !m.is_class_method
                    && m.method.kind == MethodKind::Primary
            }) {
                entries.push(Self::meta_method_entry(&standalone.method, type_params));
            }
        }
        if let Some(auto) = auto {
            for field in &auto.getters {
                entries.push((field.clone(), 0, MetaTypeRepr::None, vec![], sealed, false));
            }
            for field in &auto.setters {
                entries.push((
                    AutoSlotMethods::with_star_selector(field),
                    1,
                    MetaTypeRepr::None,
                    vec![MetaTypeRepr::None],
                    sealed,
                    false,
                ));
            }
        }
        entries
    }

    /// Collects `MethodInfoEntry` tuples for all primary class methods of `class`,
    /// including the auto-generated keyword constructor for Value subclasses.
    pub(super) fn meta_class_method_entries(
        class: &ClassDefinition,
        module: &Module,
        auto: Option<&AutoSlotMethods>,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
    ) -> Vec<MethodInfoEntry> {
        let sealed = class.is_sealed;
        let type_params = &class.type_params;
        let mut entries: Vec<MethodInfoEntry> = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| Self::meta_method_entry(m, type_params))
            .collect();
        // BT-1005: Standalone methods are excluded from __beamtalk_meta/0 (runtime-patched)
        // but included in BuilderState.meta so init/1 can register their return types.
        if include_standalone {
            for standalone in module.method_definitions.iter().filter(|m| {
                m.class_name.name == class.name.name
                    && m.is_class_method
                    && m.method.kind == MethodKind::Primary
            }) {
                entries.push(Self::meta_method_entry(&standalone.method, type_params));
            }
        }
        if let Some(auto) = auto {
            if let Some(kw_sel) = &auto.keyword_constructor {
                let arity = class.state.len();
                // BT-1408: Hash long keyword constructor selectors to stay within
                // Erlang's 255-char atom limit. The meta selector must match what
                // class_send emits so runtime dispatch finds the method.
                let safe_sel =
                    crate::core_erlang::selector_mangler::safe_class_method_selector(kw_sel);
                entries.push((
                    safe_sel,
                    arity,
                    MetaTypeRepr::None,
                    vec![MetaTypeRepr::None; arity],
                    sealed,
                    false,
                ));
            }
        }
        // BT-1218: Register the synthesized supervisionSpec so class dispatch finds it locally
        // rather than walking the chain to Actor's version (which always returns #temporary).
        if synthesize_supervision_spec {
            entries.push((
                "supervisionSpec".to_string(),
                0,
                MetaTypeRepr::Atom("SupervisionSpec".to_string()),
                vec![],
                sealed,
                false,
            ));
        }
        entries
    }

    /// Converts a `MethodDefinition` into a `MethodInfoEntry`.
    ///
    /// ADR 0068: When `class_type_params` is non-empty, type annotations that reference
    /// a class-level type parameter emit `MetaTypeRepr::TypeParam` instead of a flat atom.
    ///
    /// BT-3367: the serialized `is_sealed` bit is `m.is_sealed` alone — the same
    /// per-method flag `ClassInfo::from_class_definition` (`semantic_analysis/
    /// class_hierarchy/class_info.rs`) records for a fresh-AST compile, with no OR
    /// against the class-level `sealed` flag. `is_sealed` here means two different
    /// things to two different readers: `compute_direct_call_eligible`'s Gate 5
    /// (`codegen/core_erlang/mod.rs`) treats it as "this method body never references
    /// self/its own class for construction, so it's safe to call directly with a
    /// literal `nil` `ClassSelf`" — true only for a method individually declared
    /// `class sealed`, per that gate's own doc comment. A class being sealed only
    /// means "cannot be subclassed" (`can_be_subclassed`/`check_sealed_superclass`);
    /// it says nothing about whether an *individual* method's body is self-free.
    /// OR'ing in `class_is_sealed` previously made this producer disagree with the
    /// fresh-AST one — a class-side factory method of a sealed class (e.g. `class
    /// ok: a details: b => Self checkName: a details: b`) that itself constructs a
    /// new instance would round-trip through this BEAM-metadata path (the one the
    /// REPL uses to see an already-loaded project class) as `is_sealed = true` even
    /// though the method was never individually sealed, wrongly clearing Gate 5 for
    /// it. The generated direct call then hard-codes `ClassSelf = 'nil'`
    /// (`generate_direct_class_method_call`, `dispatch_codegen.rs`), and the method's
    /// own `self`/class-name construction dereferences that `nil` as a tuple —
    /// `erlang:element(2, 'nil')` — raising exactly the reported `badarg` ("invalid
    /// argument"). See BT-3367.
    fn meta_method_entry(
        m: &MethodDefinition,
        class_type_params: &[TypeParamDecl],
    ) -> MethodInfoEntry {
        let return_type = m.return_type.as_ref().map_or(MetaTypeRepr::None, |rt| {
            Self::type_annotation_to_meta_repr(rt, class_type_params)
        });
        let param_types: Vec<MetaTypeRepr> = m
            .parameters
            .iter()
            .map(|p| {
                p.type_annotation.as_ref().map_or(MetaTypeRepr::None, |ta| {
                    Self::type_annotation_to_meta_repr(ta, class_type_params)
                })
            })
            .collect();
        (
            m.selector.name().to_string(),
            m.selector.arity(),
            return_type,
            param_types,
            m.is_sealed,
            m.is_internal,
        )
    }

    /// Converts a `TypeAnnotation` into a `MetaTypeRepr`.
    ///
    /// Thin wrapper (BT-3076) around [`Self::declared_type_to_meta_repr`] —
    /// converts to the span-free [`DeclaredType`] first and delegates, so the
    /// AST and the structured `MethodInfo`/generator paths share one
    /// conversion. See that function's doc for the per-variant rules.
    fn type_annotation_to_meta_repr(
        ta: &beamtalk_core::ast::TypeAnnotation,
        class_type_params: &[TypeParamDecl],
    ) -> MetaTypeRepr {
        Self::declared_type_to_meta_repr(&DeclaredType::from(ta), class_type_params)
    }

    /// Converts a [`DeclaredType`] into a `MetaTypeRepr` (BT-3076).
    ///
    /// ADR 0068: If a bare `Simple` name matches one of the class-level type
    /// parameters, it becomes a `TypeParam { name, index }`. A single
    /// uppercase-letter `Simple` name not among `class_type_params` becomes a
    /// method-local `TypeParam` (index `-1`). `Generic` types with
    /// parameters become `Generic { base, parameters }`, recursively.
    ///
    /// BT-3076: `Union` and `Singleton` now convert structurally too
    /// (`MetaTypeRepr::Union` / `MetaTypeRepr::Singleton`), rather than
    /// degrading to a flat atom of the rendered string — the wire-format
    /// extension this stage adds. `FalseOr`, `Difference`, `Intersection`,
    /// `SelfType`, `SelfClass`, and `ClassOf` are rare in method signatures
    /// and still fall back to a flat `Atom` of the rendered string (old
    /// readers of a new artifact degrade gracefully; the format is internal
    /// — see this module's `MetaTypeRepr` doc). The self-type renderings
    /// (`'Self'`, `'Self class'`, `'<Name> class'`) are recognised by
    /// `DeclaredType::parse` on the reader side, so they round-trip
    /// structurally despite the flat encoding (compiler-port's
    /// `self_type_return_survives_etf_meta`).
    fn declared_type_to_meta_repr(
        dt: &DeclaredType,
        class_type_params: &[TypeParamDecl],
    ) -> MetaTypeRepr {
        match dt {
            DeclaredType::Simple(name) => {
                // Check if this is a class-level type parameter
                if let Some(index) = class_type_params
                    .iter()
                    .position(|tp| tp.name.name == *name)
                {
                    MetaTypeRepr::TypeParam {
                        name: name.to_string(),
                        index: i32::try_from(index).unwrap_or(0),
                    }
                } else if name.len() == 1
                    && name.chars().next().is_some_and(|c| c.is_ascii_uppercase())
                {
                    // Single uppercase letter not in class type params → method-local type param
                    MetaTypeRepr::TypeParam {
                        name: name.to_string(),
                        index: -1,
                    }
                } else {
                    MetaTypeRepr::Atom(name.to_string())
                }
            }
            DeclaredType::Generic { base, parameters } => {
                let params: Vec<MetaTypeRepr> = parameters
                    .iter()
                    .map(|p| Self::declared_type_to_meta_repr(p, class_type_params))
                    .collect();
                MetaTypeRepr::Generic {
                    base: base.to_string(),
                    parameters: params,
                }
            }
            DeclaredType::Union(members) => MetaTypeRepr::Union(
                members
                    .iter()
                    .map(|m| Self::declared_type_to_meta_repr(m, class_type_params))
                    .collect(),
            ),
            DeclaredType::Singleton(name) => MetaTypeRepr::Singleton(name.to_string()),
            // FalseOr, Difference, Intersection, SelfType, SelfClass, ClassOf
            // → fall back to flat atom string (see doc above).
            _ => MetaTypeRepr::Atom(dt.to_string()),
        }
    }

    /// Renders a `MetaTypeRepr` as a Core Erlang document.
    ///
    /// - `None` → `'none'`
    /// - `Atom("T")` → `'T'`
    /// - `TypeParam { name: "T", index: 0 }` → `{'type_param', 'T', 0}`
    /// - `Generic { base: "Result", params: [TypeParam T, Atom E] }` →
    ///   `{'generic', 'Result', [{'type_param', 'T', 0}, 'E']}`
    /// - `Union([Atom A, Atom B])` → `{'union', ['A', 'B']}` (BT-3076)
    /// - `Singleton("north")` → `{'singleton', 'north'}` (BT-3076)
    pub(super) fn meta_type_repr_doc(repr: &MetaTypeRepr) -> Document<'static> {
        match repr {
            MetaTypeRepr::None => Document::Str("'none'"),
            MetaTypeRepr::Atom(name) => leaf::atom(name.clone()),
            MetaTypeRepr::TypeParam { name, index } => docvec![
                "{'type_param', ",
                leaf::atom(name.clone()),
                ", ",
                leaf::int_lit(i64::from(*index)),
                "}"
            ],
            MetaTypeRepr::Generic { base, parameters } => {
                docvec![
                    "{'generic', ",
                    leaf::atom(base.clone()),
                    ", ",
                    Self::meta_type_repr_list_doc(parameters),
                    "}"
                ]
            }
            MetaTypeRepr::Union(members) => {
                docvec!["{'union', ", Self::meta_type_repr_list_doc(members), "}"]
            }
            MetaTypeRepr::Singleton(name) => {
                docvec!["{'singleton', ", leaf::atom(name.clone()), "}"]
            }
        }
    }

    /// Renders a `[MetaTypeRepr, ...]` Core Erlang list — the shared
    /// bracket/comma-join helper `Generic` and `Union` (BT-3076) both use.
    fn meta_type_repr_list_doc(items: &[MetaTypeRepr]) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("["));
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(Self::meta_type_repr_doc(item));
        }
        parts.push(Document::Str("]"));
        Document::Vec(parts)
    }

    /// Builds a Core Erlang map of selector → method info map.
    ///
    /// Each entry: `'selector' => ~{'arity' => N, 'param_types' => [...], 'return_type' => ...}~`
    /// Empty slice → `~{}~`
    ///
    /// ADR 0068: `return_type` and `param_types` can now be tagged tuples for generic types.
    pub(super) fn meta_method_info_map(methods: &[MethodInfoEntry]) -> Document<'static> {
        if methods.is_empty() {
            return Document::Str("~{}~");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("~{"));
        for (i, (sel, arity, return_type, param_types, is_sealed, is_internal)) in
            methods.iter().enumerate()
        {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let param_types_doc = if param_types.is_empty() {
                Document::Str("[]")
            } else {
                let mut pts: Vec<Document<'static>> = Vec::new();
                pts.push(Document::Str("["));
                for (j, pt) in param_types.iter().enumerate() {
                    if j > 0 {
                        pts.push(Document::Str(", "));
                    }
                    pts.push(Self::meta_type_repr_doc(pt));
                }
                pts.push(Document::Str("]"));
                Document::Vec(pts)
            };
            let return_type_doc = Self::meta_type_repr_doc(return_type);
            let is_sealed_doc: Document<'static> = Self::meta_bool(*is_sealed);
            let visibility_doc: Document<'static> = if *is_internal {
                Document::Str("'internal'")
            } else {
                Document::Str("'public'")
            };
            parts.push(docvec![
                leaf::atom(sel.clone()),
                " => ~{'arity' => ",
                leaf::int_lit(i64::try_from(*arity).unwrap_or(0)),
                ", 'param_types' => ",
                param_types_doc,
                ", 'return_type' => ",
                return_type_doc,
                ", 'is_sealed' => ",
                is_sealed_doc,
                ", 'visibility' => ",
                visibility_doc,
                "}~",
            ]);
        }
        parts.push(Document::Str("}~"));
        Document::Vec(parts)
    }
}

#[cfg(test)]
mod tests;
