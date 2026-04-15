// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Core data structures for class hierarchy metadata.
//!
//! Contains `MethodInfo`, `SuperclassTypeArg`, and `ClassInfo` — the value objects
//! that describe classes and methods in the static hierarchy.

use crate::ast::{ClassDefinition, ClassKind, Expression, Literal, MethodKind, TypeAnnotation};
use ecow::EcoString;
use std::collections::HashMap;

use super::ClassHierarchy;

/// Information about a method in the hierarchy.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MethodInfo {
    /// Full selector name (e.g., "at:put:", "+", "size").
    pub selector: EcoString,
    /// Number of arguments.
    pub arity: usize,
    /// Method kind.
    pub kind: MethodKind,
    /// Class that defines this method.
    pub defined_in: EcoString,
    /// Whether this method is sealed (cannot be overridden).
    pub is_sealed: bool,
    /// Whether this method is internal (package-scoped visibility, ADR 0071).
    ///
    /// Internal methods are only callable from within the same package.
    pub is_internal: bool,
    /// Whether this method spawns its block argument in a separate BEAM process.
    ///
    /// When `true`, self-sends inside the block are safe for Actor classes
    /// because they execute in a different process and cannot re-enter the
    /// actor's own call-stack (`calling_self` dispatch does not apply).
    pub spawns_block: bool,
    /// Inferred return type (e.g., "Integer", "String", "Boolean").
    /// `None` means the return type is unknown (Dynamic).
    pub return_type: Option<EcoString>,
    /// Parameter type annotations (e.g., `vec![Some("Number")]` for `+ other: Number`).
    /// Empty for unary methods. `None` elements mean the parameter type is unknown.
    pub param_types: Vec<Option<EcoString>>,
    /// Documentation string for this method.
    /// Populated for compiler-synthesized methods and stdlib methods with `///` doc comments;
    /// `None` for user-written methods without doc comments.
    pub doc: Option<EcoString>,
}

impl MethodInfo {
    /// Returns `true` if `self` (a subclass method) can override `ancestor`.
    ///
    /// A method cannot override a sealed method with the same selector.
    /// Different selectors are never overrides.
    #[must_use]
    pub fn can_override(&self, ancestor: &MethodInfo) -> bool {
        if self.selector != ancestor.selector {
            return false;
        }
        !ancestor.is_sealed
    }
}

/// How a subclass maps one of its type parameters (or a concrete type) to a
/// superclass type parameter position.
///
/// Used in `ClassInfo::superclass_type_args` to track the mapping established by
/// `Collection(E) subclass: Array(E)` or `Collection(Integer) subclass: IntArray`.
///
/// **References:** ADR 0068 Challenge 4
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum SuperclassTypeArg {
    /// The subclass forwards one of its own type params to the superclass.
    ///
    /// Example: `Collection(E) subclass: Array(E)` — Array's `E` (at `param_index` 0)
    /// maps to Collection's position 0.
    ParamRef {
        /// Index into this class's `type_params` vec.
        param_index: usize,
    },
    /// A concrete (fixed) type is supplied to the superclass.
    ///
    /// Example: `Collection(Integer) subclass: IntArray` — `Integer` is fixed.
    Concrete {
        /// The concrete type name (e.g., "Integer").
        type_name: EcoString,
    },
}

/// Information about a class in the hierarchy.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[allow(clippy::struct_excessive_bools)]
pub struct ClassInfo {
    /// Class name.
    pub name: EcoString,
    /// Superclass name (`None` for `ProtoObject`).
    pub superclass: Option<EcoString>,
    /// Whether this class is sealed (cannot be subclassed).
    pub is_sealed: bool,
    /// Whether this class is abstract.
    pub is_abstract: bool,
    /// Whether this class has the explicit `typed` modifier.
    /// Use `ClassHierarchy::is_typed()` to check inherited typed status.
    pub is_typed: bool,
    /// Whether this class is internal (package-scoped visibility, ADR 0071).
    ///
    /// Internal classes are only visible within their declaring package.
    pub is_internal: bool,
    /// Package that declares this class (ADR 0070/0071).
    ///
    /// Extracted from the BEAM module name pattern `bt@{package}@{class}`
    /// via [`add_from_beam_meta`], or set from the build pipeline's current
    /// package context. `None` for REPL classes and bootstrap builtins.
    pub package: Option<EcoString>,
    /// Whether this class is a Value subclass (ADR 0042, BT-1528).
    ///
    /// `true` for classes that directly or indirectly inherit from `Value`.
    /// Initially set from the direct superclass name, then corrected by
    /// [`ClassHierarchy::propagate_class_kind`] after the full hierarchy is built.
    pub is_value: bool,
    /// Whether this class is a native Actor (declared with `native:` keyword, ADR 0056).
    ///
    /// Native actors delegate to a backing Erlang module and cannot declare
    /// `state:` fields (state is owned by the Erlang `gen_server`).
    pub is_native: bool,
    /// State (instance variable) names.
    pub state: Vec<EcoString>,
    /// Declared type annotations for state fields (field name → type name).
    /// Only populated for fields with explicit type annotations.
    pub state_types: HashMap<EcoString, EcoString>,
    /// Which state fields carry an explicit default value (field name → has default).
    /// Populated for every declared state field (both typed and untyped).
    ///
    /// BT-1976: Needed so cross-file consumers can identify typed-no-default
    /// fields (for post-initialize validation in `gen_server` codegen) even
    /// when the defining class's AST is not present in the current compilation.
    pub state_has_default: HashMap<EcoString, bool>,
    /// Methods defined directly on this class (instance-side).
    pub methods: Vec<MethodInfo>,
    /// Class-side methods defined on this class.
    pub class_methods: Vec<MethodInfo>,
    /// Class variable names (declared with `classState:`).
    pub class_variables: Vec<EcoString>,
    /// Type parameters for generic classes (e.g., `["T", "E"]` for `Result(T, E)`).
    ///
    /// Empty for non-generic classes.
    pub type_params: Vec<EcoString>,
    /// Protocol bounds for each type parameter (ADR 0068 Phase 2d).
    ///
    /// Parallel to `type_params`: `type_param_bounds[i]` is the bound for `type_params[i]`.
    /// `None` means the parameter is unbounded (accepts any type).
    /// `Some("Printable")` means the concrete type arg must conform to that protocol.
    pub type_param_bounds: Vec<Option<EcoString>>,
    /// How this class's type params (or concrete types) map to the superclass's type params.
    ///
    /// Empty when the superclass is not generic or no type args are applied.
    ///
    /// Example: `Collection(E) subclass: Array(E)` → `[ParamRef { param_index: 0 }]`
    /// Example: `Collection(Integer) subclass: IntArray` → `[Concrete { type_name: "Integer" }]`
    ///
    /// **References:** ADR 0068 Challenge 4
    pub superclass_type_args: Vec<SuperclassTypeArg>,
}

impl ClassInfo {
    /// Returns `true` if this class can be subclassed.
    #[must_use]
    pub fn can_be_subclassed(&self) -> bool {
        !self.is_sealed
    }

    /// Build a `ClassInfo` from a parsed `ClassDefinition` AST node.
    ///
    /// Shared by `extract_class_infos` and `add_module_classes` to avoid
    /// duplicating the field-mapping logic.
    #[allow(clippy::too_many_lines)] // field-mapping function — length is proportional to struct fields
    pub(super) fn from_class_definition(class: &ClassDefinition) -> Self {
        let mut instance_methods: Vec<MethodInfo> = class
            .methods
            .iter()
            .map(|m| MethodInfo {
                selector: m.selector.name(),
                arity: m.selector.arity(),
                kind: m.kind,
                defined_in: class.name.name.clone(),
                is_sealed: m.is_sealed,
                is_internal: m.is_internal,
                spawns_block: false,
                return_type: m.return_type.as_ref().map(TypeAnnotation::type_name),
                param_types: m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.as_ref().map(TypeAnnotation::type_name))
                    .collect(),
                doc: m.doc_comment.clone().map(Into::into),
            })
            .collect();

        let mut class_methods: Vec<MethodInfo> = class
            .class_methods
            .iter()
            .map(|m| MethodInfo {
                selector: m.selector.name(),
                arity: m.selector.arity(),
                kind: m.kind,
                defined_in: class.name.name.clone(),
                is_sealed: m.is_sealed,
                is_internal: m.is_internal,
                spawns_block: false,
                return_type: m.return_type.as_ref().map(TypeAnnotation::type_name),
                param_types: m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.as_ref().map(TypeAnnotation::type_name))
                    .collect(),
                doc: m.doc_comment.clone().map(Into::into),
            })
            .collect();

        // BT-923: For `Value subclass:` classes, synthesize auto-generated slot
        // methods so the type checker and cross-file consumers can resolve them.
        if class.class_kind == ClassKind::Value {
            ClassHierarchy::add_value_auto_methods(
                class,
                &mut instance_methods,
                &mut class_methods,
            );
        }

        Self {
            name: class.name.name.clone(),
            superclass: class.superclass.as_ref().map(|s| s.name.clone()),
            is_sealed: class.is_sealed,
            is_abstract: class.is_abstract,
            is_typed: class.is_typed,
            is_internal: class.is_internal,
            package: None, // Populated later by the build pipeline or BEAM metadata
            is_value: class.class_kind == ClassKind::Value,
            is_native: class.backing_module.is_some(),
            state: class.state.iter().map(|s| s.name.name.clone()).collect(),
            state_types: class
                .state
                .iter()
                .filter_map(|s| {
                    s.type_annotation
                        .as_ref()
                        .map(|ty| (s.name.name.clone(), ty.type_name()))
                })
                .collect(),
            state_has_default: class
                .state
                .iter()
                .map(|s| (s.name.name.clone(), s.default_value.is_some()))
                .collect(),
            methods: instance_methods,
            class_methods,
            class_variables: class
                .class_variables
                .iter()
                .map(|cv| cv.name.name.clone())
                .collect(),
            type_params: class
                .type_params
                .iter()
                .map(|tp| tp.name.name.clone())
                .collect(),
            type_param_bounds: class
                .type_params
                .iter()
                .map(|tp| tp.bound.as_ref().map(|b| b.name.clone()))
                .collect(),
            superclass_type_args: {
                let own_params: Vec<EcoString> = class
                    .type_params
                    .iter()
                    .map(|tp| tp.name.name.clone())
                    .collect();
                class
                    .superclass_type_args
                    .iter()
                    .map(|ta| {
                        let name = ta.type_name();
                        if let Some(idx) = own_params.iter().position(|p| p == &name) {
                            SuperclassTypeArg::ParamRef { param_index: idx }
                        } else {
                            SuperclassTypeArg::Concrete { type_name: name }
                        }
                    })
                    .collect()
            },
        }
    }
}

/// Formats an AST expression as a compact string for use in generated doc comments.
///
/// Only handles simple literal values (integer, float, string, boolean, nil).
/// Complex expressions fall back to `"..."`.
pub(super) fn format_default_value(expr: &Expression) -> String {
    match expr {
        Expression::Literal(lit, _) => match lit {
            Literal::Integer(n) => n.to_string(),
            Literal::Float(f) => {
                let rendered = f.to_string();
                // Preserve decimal point so 1.0 stays as "1.0" not "1"
                if rendered.contains('.') || rendered.contains('e') || rendered.contains('E') {
                    rendered
                } else {
                    format!("{rendered}.0")
                }
            }
            Literal::String(s) => format!("{s:?}"),
            Literal::Symbol(s) => format!("#{s}"),
            Literal::Character(c) => format!("${}", c.escape_default()),
            Literal::List(_) => "...".to_string(),
        },
        Expression::Identifier(ident) if ident.name == "nil" => "nil".to_string(),
        Expression::Identifier(ident) if ident.name == "true" => "true".to_string(),
        Expression::Identifier(ident) if ident.name == "false" => "false".to_string(),
        _ => "...".to_string(),
    }
}
