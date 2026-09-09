// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Auto-generated value-class accessor rule, emission, metadata and xref.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ADR 0042: which slot getters (`field/1`), `with*:` copy-setters, and
//! keyword constructors a `Value subclass:` class gets for free, plus their
//! Core Erlang emission, their compiler-derived `__signature__`/`__doc__`
//! metadata, and their `method_xref` rows (ADR 0087 Phase 6) — unified here
//! because all four are the same "what does this synthetic accessor look
//! like" question asked from different angles, and previously spread across
//! `value_type_codegen.rs` (rule + emission) and `gen_server/methods.rs`
//! (metadata + xref), which is what created the `methods.rs` ↔
//! `value_type_codegen.rs` import cycle.

use super::{CoreErlangGenerator, xref::MAX_ATOM_BYTES};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, concat, join, leaf};
use beamtalk_core::ast::{ClassDefinition, ClassKind, StateDeclaration, TypeAnnotation};
use beamtalk_core::unparse::unparse_type_annotation_display;
use std::fmt::Write as FmtWrite;

/// Auto-generated slot methods for `Value subclass:` classes (ADR 0042).
///
/// Only populated when `class_kind == ClassKind::Value`.
/// Skips any slot whose getter/setter selector the user has already defined.
pub(super) struct AutoSlotMethods {
    /// Field names for which a getter `fieldName/1` is auto-generated.
    pub(super) getters: Vec<String>,
    /// Field names for which a `withFieldName:/2` setter is auto-generated.
    pub(super) setters: Vec<String>,
    /// Keyword constructor selector (e.g., `"x:y:"` for a Point with slots x, y),
    /// `None` if the class has no slots or the user already defined it.
    pub(super) keyword_constructor: Option<String>,
}

impl AutoSlotMethods {
    /// Computes the `with*:` selector name for a slot.
    ///
    /// Delegates to [`beamtalk_core::synthetic_selectors::with_star_selector`], the shared
    /// naming authority for value-class synthetics.
    pub(super) fn with_star_selector(field_name: &str) -> String {
        beamtalk_core::synthetic_selectors::with_star_selector(field_name)
    }

    /// Returns the keyword constructor selector for the given slot names.
    ///
    /// E.g. `["x", "y"]` → `"x:y:"`. Delegates to the shared naming authority in
    /// [`beamtalk_core::synthetic_selectors`].
    fn keyword_selector(slots: &[String]) -> String {
        beamtalk_core::synthetic_selectors::keyword_constructor_selector(
            slots.iter().map(String::as_str),
        )
    }
}

/// Computes which slot methods to auto-generate for a `Value subclass:` class.
///
/// Returns `None` for `ClassKind::Object` and `ClassKind::Actor` — only
/// `ClassKind::Value` classes get auto-generated slot accessors.
pub(super) fn compute_auto_slot_methods(class: &ClassDefinition) -> Option<AutoSlotMethods> {
    if class.class_kind != ClassKind::Value {
        return None;
    }

    // Collect selectors the user has already explicitly defined
    let user_instance_selectors: std::collections::HashSet<String> = class
        .methods
        .iter()
        .map(|m| m.selector.name().to_string())
        .collect();
    let user_class_selectors: std::collections::HashSet<String> = class
        .class_methods
        .iter()
        .map(|m| m.selector.name().to_string())
        .collect();

    let getters: Vec<String> = class
        .state
        .iter()
        .filter(|s| !user_instance_selectors.contains(s.name.name.as_str()))
        .map(|s| s.name.name.to_string())
        .collect();

    let setters: Vec<String> = class
        .state
        .iter()
        .filter(|s| {
            let with_name = AutoSlotMethods::with_star_selector(s.name.name.as_str());
            !user_instance_selectors.contains(&with_name)
        })
        .map(|s| s.name.name.to_string())
        .collect();

    let keyword_constructor = if class.state.is_empty() {
        None
    } else {
        let all_slots: Vec<String> = class
            .state
            .iter()
            .map(|s| s.name.name.to_string())
            .collect();
        let sel = AutoSlotMethods::keyword_selector(&all_slots);
        if user_class_selectors.contains(&sel) {
            None
        } else {
            Some(sel)
        }
    };

    Some(AutoSlotMethods {
        getters,
        setters,
        keyword_constructor,
    })
}

/// Whether this class's instances are opaque terms owned entirely by
/// a paired Erlang module, so the inherited `basicNew` cannot build one.
///
/// `Value class>>new` is `@intrinsic basicNew`, which compiles to a map of
/// `$beamtalk_class` plus every declared field's default. That is a complete
/// instance for an ordinary value type — but a `native:` class keeps its state
/// in the shape its backing module defines (`beamtalk_datetime`'s calendar
/// tuple, `beamtalk_uuid`'s 16 raw bytes, …) and declares no BT fields to
/// stand in for it. `basicNew` therefore yields `~{'$beamtalk_class' => 'X'}~`:
/// correctly tagged, so dispatch accepts it, and empty, so the very first
/// method call dies inside the Erlang module on an unrelated-looking
/// `function_clause`. Codegen raises a clear `instantiation_error` instead.
///
/// Deliberately narrow on both counts:
///
/// * A `native:` class that *does* declare fields (`Package`,
///   `SupervisionNode`) has a real default instance, so `basicNew` is right.
/// * A class that declares its own `new` (`Random`, `Queue`) already routes
///   `new/0` through `generate_delegating_new` to that method; this predicate
///   is only consulted on the auto-generated path.
pub(in crate::core_erlang) fn has_opaque_native_representation(class: &ClassDefinition) -> bool {
    class.backing_module.is_some() && class.state.is_empty()
}

/// Compiler-derived `__signature__` / `__doc__` selector-map entries
/// for a value class's auto-generated accessors, split by dispatch side.
///
/// Each `Vec` holds ready-to-embed `'selector' => <binary>` fragments (built by
/// [`CoreErlangGenerator::synthetic_selector_map_entry`]). Instance-side entries
/// feed the `methodSignatures` / `methodDocs` maps; class-side entries feed the
/// `classMethodSignatures` / `classMethodDocs` maps (the keyword constructor).
#[derive(Default)]
pub(in crate::core_erlang) struct SyntheticAccessorMetadata {
    pub(in crate::core_erlang) instance_sigs: Vec<Document<'static>>,
    pub(in crate::core_erlang) instance_docs: Vec<Document<'static>>,
    pub(in crate::core_erlang) class_sigs: Vec<Document<'static>>,
    pub(in crate::core_erlang) class_docs: Vec<Document<'static>>,
}

/// One compiler-derived accessor's readable metadata:
/// `(selector, signature, doc)`. The pure, unit-testable intermediate produced
/// by [`CoreErlangGenerator::synthetic_value_accessor_entries`] before it is
/// rendered into Core Erlang `'selector' => <binary>` map fragments.
type SyntheticAccessorEntry = (String, String, String);

/// A value class's synthetic-accessor metadata, split by dispatch side.
/// `instance` holds slot getters and `with*:` setters; `class` holds the keyword
/// constructor.
#[derive(Default)]
struct SyntheticAccessorEntries {
    instance: Vec<SyntheticAccessorEntry>,
    class: Vec<SyntheticAccessorEntry>,
}

/// Collects the class names referenced by a type annotation into `out`
/// (ADR 0087 Phase 6).
///
/// Mirrors `collect_all_type_refs` in
/// [`beamtalk_core::method_source_walker`] — the walker hand-written-method
/// `references` rows use — so a synthetic accessor on a typed slot reports the
/// same referenced class names a hand-written accessor with the same type
/// signature would. `Singleton` / `Self` / `Self class` annotations carry no
/// class reference and are skipped.
fn collect_type_annotation_class_names(annotation: &TypeAnnotation, out: &mut Vec<String>) {
    match annotation {
        TypeAnnotation::Simple(id) => out.push(id.name.to_string()),
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            out.push(base.name.to_string());
            for param in parameters {
                collect_type_annotation_class_names(param, out);
            }
        }
        TypeAnnotation::Union { types, .. } => {
            for ty in types {
                collect_type_annotation_class_names(ty, out);
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            collect_type_annotation_class_names(inner, out);
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            collect_type_annotation_class_names(base, out);
            collect_type_annotation_class_names(excluded, out);
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            collect_type_annotation_class_names(left, out);
            collect_type_annotation_class_names(right, out);
        }
        TypeAnnotation::ClassOf { class_name, .. } => out.push(class_name.name.to_string()),
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. } => {}
    }
}

impl CoreErlangGenerator {
    /// Generates an auto-getter function for a single slot.
    ///
    /// ```erlang
    /// 'x'/1 = fun (Self) -> call 'maps':'get'('x', Self)
    /// ```
    pub(in crate::core_erlang) fn generate_slot_getter(field_name: &str) -> Document<'static> {
        docvec![
            leaf::fname(field_name.to_string(), 1),
            " = fun (Self) ->\n",
            "    call 'maps':'get'(",
            leaf::atom(field_name.to_string()),
            ", Self)\n",
            "\n",
        ]
    }

    /// Generates an auto `with*:` functional setter for a single slot.
    ///
    /// ```erlang
    /// 'withX:'/2 = fun (Self, NewVal) -> call 'maps':'put'('x', NewVal, Self)
    /// ```
    pub(in crate::core_erlang) fn generate_slot_setter(field_name: &str) -> Document<'static> {
        let with_sel = AutoSlotMethods::with_star_selector(field_name);
        docvec![
            leaf::fname(with_sel, 2),
            " = fun (Self, NewVal) ->\n",
            "    call 'maps':'put'(",
            leaf::atom(field_name.to_string()),
            ", NewVal, Self)\n",
            "\n",
        ]
    }

    /// Generates the all-fields keyword constructor class method.
    ///
    /// For direct `Value subclass:` classes, builds a flat map:
    /// ```erlang
    /// 'class_x:y:'/4 = fun (ClassSelf, ClassVars, X, Y) ->
    ///     ~{'$beamtalk_class' => 'Point', 'x' => X, 'y' => Y}~
    /// ```
    ///
    /// For sub-subclasses, delegates to `new:` so inherited fields
    /// from the parent are included:
    /// ```erlang
    /// 'class_y:'/3 = fun (_ClassSelf, _ClassVars, SlotArg0) ->
    ///     call 'child':'new'(~{'y' => SlotArg0}~)
    /// ```
    pub(in crate::core_erlang) fn generate_keyword_constructor_fn(
        class_name: &str,
        kw_selector: &str,
        slots: &[String],
        is_sub_subclass: bool,
        module_name: &str,
    ) -> Document<'static> {
        let arity = slots.len() + 2; // _ClassSelf + _ClassVars + N slot args

        // Pre-compute slot argument names once; write! instead of format! per codegen rules.
        let slot_arg_names: Vec<String> = (0..slots.len())
            .map(|i| {
                let mut name = String::from("SlotArg");
                let _ = write!(&mut name, "{i}");
                name
            })
            .collect();

        // Extra slot parameters appended after "_ClassSelf, _ClassVars": ", SlotArg0", ...
        let slot_param_docs: Vec<Document<'static>> = slot_arg_names
            .iter()
            .flat_map(|name| [Document::Str(", "), leaf::var(name.clone())])
            .collect();

        // Hash long keyword constructor atoms to stay within Erlang's
        // 255-char atom limit.
        let safe_fn_name = super::selector_mangler::safe_class_method_fn_name(kw_selector);

        // Sub-subclasses delegate to new: to include inherited fields.
        if is_sub_subclass {
            // Build a map of own slot args: ~{'slot0' => SlotArg0, 'slot1' => SlotArg1}~
            let mut map_parts: Vec<Document<'static>> = Vec::new();
            for (i, slot_name) in slots.iter().enumerate() {
                if i > 0 {
                    map_parts.push(Document::Str(", "));
                }
                map_parts.extend([
                    leaf::atom(slot_name.clone()),
                    Document::Str(" => "),
                    leaf::var(slot_arg_names[i].clone()),
                ]);
            }

            return docvec![
                leaf::fname(safe_fn_name, arity),
                " = fun (_ClassSelf, _ClassVars",
                concat(slot_param_docs),
                ") ->\n",
                "    call ",
                leaf::atom(module_name.to_string()),
                ":'new'(~{",
                concat(map_parts),
                "}~)\n",
                "\n",
            ];
        }

        // Direct Value subclass: build a flat map with all own fields.
        let mut map_field_docs: Vec<Document<'static>> = vec![
            Document::Str("'$beamtalk_class' => "),
            leaf::atom(class_name.to_string()),
        ];
        for (i, slot_name) in slots.iter().enumerate() {
            map_field_docs.extend([
                Document::Str(", "),
                leaf::atom(slot_name.clone()),
                Document::Str(" => "),
                leaf::var(slot_arg_names[i].clone()),
            ]);
        }

        docvec![
            leaf::fname(safe_fn_name, arity),
            " = fun (_ClassSelf, _ClassVars",
            concat(slot_param_docs),
            ") ->\n",
            "    ~{",
            concat(map_field_docs),
            "}~\n",
            "\n",
        ]
    }

    /// Generates dispatch arms for auto-generated getter and `with*:` setter methods.
    ///
    /// Each arm follows the same pattern as user-defined methods in `generate_primitive_dispatch`.
    pub(in crate::core_erlang) fn generate_auto_slot_dispatch_arms(
        mod_name: &str,
        auto: &AutoSlotMethods,
    ) -> Vec<Document<'static>> {
        let mut arms: Vec<Document<'static>> = Vec::new();

        for field in &auto.getters {
            arms.push(docvec![
                "        <",
                leaf::atom(field.clone()),
                "> when 'true' ->\n",
            ]);
            arms.push(docvec![
                "            call ",
                leaf::atom(mod_name.to_string()),
                ":",
                leaf::atom(field.clone()),
                "(Self)\n",
            ]);
        }

        for field in &auto.setters {
            let with_sel = AutoSlotMethods::with_star_selector(field);
            arms.push(docvec![
                "        <",
                leaf::atom(with_sel.clone()),
                "> when 'true' ->\n",
            ]);
            arms.push(Document::Str(
                "            let <DispArg0> = call 'erlang':'hd'(Args) in\n",
            ));
            arms.push(docvec![
                "            call ",
                leaf::atom(mod_name.to_string()),
                ":",
                leaf::atom(with_sel),
                "(Self, DispArg0)\n",
            ]);
        }

        arms
    }

    /// Builds the four Core Erlang selector-map entry lists for a value
    /// class's auto-generated accessors, ready to inject into the
    /// `methodSignatures` / `methodDocs` (instance) and
    /// `classMethodSignatures` / `classMethodDocs` (class-side) maps.
    ///
    /// Value-type slot getters, `with*:` copy-setters, and the keyword constructor
    /// are emitted by `value_type_codegen` with no AST `MethodDefinition`, so they
    /// never reach those maps and their runtime `__doc__` / `__signature__` would
    /// be `nil`. Wrapping [`Self::synthetic_value_accessor_entries`], this renders
    /// each `(selector, signature, doc)` triple into `'selector' => <binary>`
    /// entries so the synthetics carry the same self-describing metadata every
    /// reflective surface reads (reusing the existing resolver — no new read path).
    /// Builds a single `'selector' => <binary>` selector-map entry for a
    /// compiler-derived signature or doc string. The value is a human-readable
    /// data string (not a Core Erlang structural fragment), so it is wrapped once
    /// in a `binary_lit` typed leaf — mirroring how the AST-driven maps embed
    /// `unparse_method_display_signature` / `doc_comment` strings.
    fn synthetic_selector_map_entry(selector: &str, value: &str) -> Document<'static> {
        docvec![
            leaf::atom(selector.to_string()),
            " => ",
            leaf::binary_lit(value),
        ]
    }

    pub(in crate::core_erlang) fn build_synthetic_value_accessor_metadata(
        class: &ClassDefinition,
    ) -> SyntheticAccessorMetadata {
        let raw = Self::synthetic_value_accessor_entries(class);
        let mut md = SyntheticAccessorMetadata::default();
        for (selector, sig, doc) in &raw.instance {
            md.instance_sigs
                .push(Self::synthetic_selector_map_entry(selector, sig));
            md.instance_docs
                .push(Self::synthetic_selector_map_entry(selector, doc));
        }
        for (selector, sig, doc) in &raw.class {
            md.class_sigs
                .push(Self::synthetic_selector_map_entry(selector, sig));
            md.class_docs
                .push(Self::synthetic_selector_map_entry(selector, doc));
        }
        md
    }

    /// Computes the readable `(selector, signature, doc)` triples for a
    /// value class's compiler-generated accessors — the pure, unit-testable core
    /// of [`Self::build_synthetic_value_accessor_metadata`].
    ///
    /// The auto-accessor set and slot types come from the same sources
    /// [`Self::build_synthetic_accessor_xref_entries`] uses:
    /// [`compute_auto_slot_methods`] (which slots the user has *not* overridden)
    /// and each slot's `StateDeclaration` type annotation. `instance` holds the
    /// getters and `with*:` setters; `class` holds the keyword constructor.
    /// Returns all-empty for non-`Value` classes and for value classes with no
    /// auto-generated accessors.
    fn synthetic_value_accessor_entries(class: &ClassDefinition) -> SyntheticAccessorEntries {
        let mut entries = SyntheticAccessorEntries::default();
        let Some(auto) = compute_auto_slot_methods(class) else {
            return entries;
        };
        let class_name = class.name.name.as_str();

        // Getters: `field -> <SlotType>`. The return type is the slot's declared
        // type (falling back to `Object` for an untyped slot).
        for field in &auto.getters {
            let Some(slot) = class.state.iter().find(|s| s.name.name.as_str() == field) else {
                continue;
            };
            let slot_type = Self::synthetic_slot_type_display(slot);
            entries.instance.push((
                field.clone(),
                format!("{field} -> {slot_type}"),
                format!("Compiler-derived accessor. Returns the value of slot `{field}`."),
            ));
        }

        // Setters: `withField: aValue -> <ClassName>` (returns a copy).
        for field in &auto.setters {
            if !class.state.iter().any(|s| s.name.name.as_str() == field) {
                continue;
            }
            let with_sel = AutoSlotMethods::with_star_selector(field);
            entries.instance.push((
                with_sel.clone(),
                format!("{with_sel} aValue -> {class_name}"),
                format!(
                    "Compiler-derived copy-setter. Returns a copy with slot `{field}` replaced."
                ),
            ));
        }

        // Keyword constructor (class-side): `slot0: slot0 slot1: slot1 -> <ClassName>`.
        // The selector's keyword parts are the slot names in declaration order, so
        // the same names serve as the display parameter names.
        if let Some(kw_sel) = auto.keyword_constructor {
            let sig_parts: Vec<String> = class
                .state
                .iter()
                .map(|s| {
                    let n = s.name.name.as_str();
                    format!("{n}: {n}")
                })
                .collect();
            // The map *key* must be the same atom the runtime dispatch and
            // `__beamtalk_meta/0` entry use (`safe_class_method_selector` — hashed
            // once "class_" + selector would exceed Erlang's 255-char atom limit),
            // so a many-field Value class's keyword constructor doesn't blow the
            // atom limit here even though it already gets hashed for dispatch.
            // The signature/doc *text* keeps the full readable selector — it is a
            // binary literal, not an atom, so it carries no length limit.
            let safe_kw_sel = super::selector_mangler::safe_class_method_selector(&kw_sel);
            entries.class.push((
                safe_kw_sel,
                format!("{} -> {class_name}", sig_parts.join(" ")),
                format!(
                    "Compiler-derived keyword constructor. Returns a new {class_name} from the given slot values."
                ),
            ));
        }

        entries
    }

    /// Display form of a slot's declared type for a synthetic accessor
    /// signature, falling back to `Object` when the slot carries no annotation.
    fn synthetic_slot_type_display(slot: &StateDeclaration) -> String {
        slot.type_annotation
            .as_ref()
            .map_or_else(|| "Object".to_string(), unparse_type_annotation_display)
    }

    /// ADR 0087 Phase 6: Builds `method_xref` rows for the
    /// compiler-generated auto-accessors of a `Value subclass:` class.
    ///
    /// For each auto-generated slot getter (`field/1`) and `with*:` setter
    /// (`withField:/2`) — i.e. those the user did *not* hand-define — one row is
    /// emitted with:
    /// - `source_status => synthetic` (the parity-exception marker),
    /// - `synthetic_origin => N`, the 1-based source line of the generating
    ///   `field:` / `state:` slot declaration (falling back to the class header
    ///   line when the slot span cannot be resolved),
    /// - `line => N` mirroring the origin so LSP / System Browser navigation has
    ///   a target,
    /// - an empty `sends` list — accessors delegate to runtime map primitives
    ///   (`maps:get` / `maps:put`), not Beamtalk sends — and
    /// - a `references` list carrying the slot's declared type (e.g. a slot
    ///   `state: count :: Integer` yields a reference to `Integer` on both its
    ///   getter and its `withCount:` setter).
    ///
    /// Returns an empty vector for non-`Value` classes (only value types get
    /// auto-accessors) and for classes with no auto-generated accessors.
    pub(in crate::core_erlang) fn build_synthetic_accessor_xref_entries(
        &self,
        class: &ClassDefinition,
    ) -> Vec<Document<'static>> {
        let Some(auto) = compute_auto_slot_methods(class) else {
            return Vec::new();
        };

        // Map field name -> its slot declaration so each accessor can derive its
        // origin line and type references from the generating declaration.
        let mut entries: Vec<Document<'static>> = Vec::new();

        for field in &auto.getters {
            if let Some(slot) = class.state.iter().find(|s| s.name.name.as_str() == field) {
                entries.push(self.build_synthetic_accessor_entry(field, slot, class));
            }
        }
        for field in &auto.setters {
            if let Some(slot) = class.state.iter().find(|s| s.name.name.as_str() == field) {
                let with_sel = AutoSlotMethods::with_star_selector(field);
                entries.push(self.build_synthetic_accessor_entry(&with_sel, slot, class));
            }
        }

        entries
    }

    /// Builds a single synthetic auto-accessor `method_xref` row
    /// (ADR 0087 Phase 6).
    ///
    /// `selector` is the accessor selector (`field` or `withField:`), `slot` the
    /// generating slot declaration that supplies the derived origin line and the
    /// referenced type.
    fn build_synthetic_accessor_entry(
        &self,
        selector: &str,
        slot: &StateDeclaration,
        class: &ClassDefinition,
    ) -> Document<'static> {
        // Derived location: the 1-based line of the generating slot declaration,
        // falling back to the class-header line when the slot span cannot be
        // resolved to a source line.
        let origin_line = self
            .span_to_line(slot.span)
            .or_else(|| self.span_to_line(class.span))
            .unwrap_or(1);

        // References: the slot's declared type names (e.g. `Integer`). Accessors
        // have no Beamtalk sends, but their type signature mentions the slot type
        // exactly like a hand-written `field :: Integer` accessor would.
        let mut ref_class_names: Vec<String> = Vec::new();
        if let Some(ref ann) = slot.type_annotation {
            collect_type_annotation_class_names(ann, &mut ref_class_names);
        }
        let refs_doc = {
            let ref_docs: Vec<Document<'static>> = ref_class_names
                .iter()
                .filter(|name| name.len() <= MAX_ATOM_BYTES)
                .map(|name| {
                    docvec![
                        "~{'class' => ",
                        leaf::atom(name.clone()),
                        ", 'line' => ",
                        leaf::int_lit(i64::from(origin_line)),
                        "}~",
                    ]
                })
                .collect();
            docvec!["[", join(ref_docs, &Document::Str(", ")), "]"]
        };

        docvec![
            "~{'class_side' => 'false', 'selector' => ",
            leaf::atom(selector.to_string()),
            ", 'line' => ",
            leaf::int_lit(i64::from(origin_line)),
            ", 'sends' => [], 'references' => ",
            refs_doc,
            ", 'source_status' => 'synthetic', 'synthetic_origin' => ",
            leaf::int_lit(i64::from(origin_line)),
            "}~",
        ]
    }
}

#[cfg(test)]
mod tests;
