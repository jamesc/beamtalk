// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dialyzer `-spec` and `-type` generation from type annotations.
//!
//! **DDD Context:** Code Generation
//!
//! Generates Core Erlang `'spec'` and `'type'` module attributes from Beamtalk
//! type annotations. This enables Dialyzer to perform cross-language type
//! checking at the BEAM level.
//!
//! - [`generate_class_specs`] / [`format_spec_attributes`] — emit `-spec`
//!   annotations for each method with type-annotated parameters or return type
//! - [`generate_type_alias`] — emit a `-type t()` map alias for Value classes
//!   with `state:` declarations, so Erlang producers can reference `Module:t()`
//!   in their own `-spec` annotations (requires a paired `-export_type([t/0])`
//!   emitted by `value_type_codegen.rs`)
//! - [`generate_alias_type_attrs`] / [`format_alias_type_attributes`] — emit a
//!   named `-type` per Beamtalk `type Name = ...` alias (ADR 0108, BT-2900),
//!   so annotation sites referencing the alias can emit a `user_type`
//!   reference instead of re-expanding the alias inline. Every function above
//!   takes an `Option<&AliasRegistry>`: `None` preserves pre-ADR-0108
//!   behaviour exactly (an alias name falls through to `any()` like any other
//!   unrecognised class name), `Some(registry)` resolves alias references to
//!   named `user_type` forms.
//!
//! ## Core Erlang Spec Format
//!
//! Each spec is encoded as an abstract type representation in the module
//! attributes list:
//!
//! ```erlang
//! 'spec' = [{{'function_name', Arity},
//!            [{'type', 0, 'fun',
//!              [{'type', 0, 'product', [ParamTypes...]}
//!               , ReturnType]}]}]
//! ```
//!
//! ## Type Mapping
//!
//! | Beamtalk Type | Erlang Type |
//! |---------------|-------------|
//! | `Integer` | `integer()` |
//! | `Float` | `float()` |
//! | `Number` | `number()` |
//! | `String` | `binary()` |
//! | `Boolean` | `boolean()` |
//! | `Symbol` | `atom()` |
//! | `Character` | `integer()` |
//! | `List` | `list()` |
//! | `Tuple` | `tuple()` |
//! | `Dictionary` | `map()` |
//! | `Set` | `map()` |
//! | `Nil` / `UndefinedObject` | `nil` (atom) |
//! | `True` | `true` (atom) |
//! | `False` | `false` (atom) |
//! | `Object` / custom | `any()` |
//! | Union | `union(...)` |
//! | Singleton `#foo` | atom `foo` |

use std::cell::RefCell;
use std::collections::HashSet;

use ecow::EcoString;

use super::document::leaf::{atom, int_lit};
use super::document::{Document, join};
use super::selector_mangler::safe_class_method_fn_name;
use crate::ast::{ClassDefinition, MethodDefinition, MethodKind, TypeAnnotation, to_module_name};
use crate::docvec;
use crate::semantic_analysis::alias_registry::AliasRegistry;

/// Converts a `TypeAnnotation` to its Core Erlang abstract type representation.
///
/// `aliases`, when present, is consulted for `TypeAnnotation::Simple` names:
/// a name registered in the alias table emits a `user_type` reference to the
/// alias's named `-type` (see [`alias_type_reference`]) instead of falling
/// through to `any()`. `None` reproduces pre-ADR-0108 behaviour exactly.
///
/// `referenced`, when present, records every alias name this walk actually
/// emits a `user_type` reference for (BT-2940) — callers use this to scope
/// [`generate_alias_type_attrs`]'s emission to only the aliases a module's
/// specs/state fields reference, instead of every pre-loaded alias.
fn type_annotation_to_spec(
    annotation: &TypeAnnotation,
    aliases: Option<&AliasRegistry>,
    referenced: Option<&RefCell<HashSet<EcoString>>>,
) -> Document<'static> {
    match annotation {
        TypeAnnotation::Simple(id) => {
            if aliases.is_some_and(|registry| registry.has_alias(id.name.as_str())) {
                if let Some(tracker) = referenced {
                    tracker.borrow_mut().insert(id.name.clone());
                }
                alias_type_reference(id.name.as_str())
            } else {
                simple_type_to_spec(id.name.as_str())
            }
        }
        TypeAnnotation::Union { types, .. } => {
            let type_specs: Vec<Document<'static>> = types
                .iter()
                .map(|t| type_annotation_to_spec(t, aliases, referenced))
                .collect();
            docvec![
                "{'type', 0, 'union', [",
                join(type_specs, &Document::Str(", ")),
                "]}"
            ]
        }
        TypeAnnotation::Singleton { name, .. } => {
            docvec!["{'atom', 0, ", atom(name.to_string()), "}"]
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => generic_type_to_spec(base.name.as_str(), parameters, aliases, referenced),
        TypeAnnotation::FalseOr { inner, .. } => {
            let inner_spec = type_annotation_to_spec(inner, aliases, referenced);
            docvec![
                "{'type', 0, 'union', [",
                inner_spec,
                ", {'atom', 0, 'false'}]}"
            ]
        }
        // Difference (`base \ excluded`) has no precise Erlang spec form; the
        // excluded set only narrows `base`, so the base spec is a sound (if
        // wider) over-approximation for the generated `-spec`. This same
        // widening applies when `base \ excluded` is an alias RHS (ADR 0108
        // Consequences — Positive): the alias's own `-type` ends up widened
        // too, with the exclusion lost.
        TypeAnnotation::Difference { base, .. } => {
            type_annotation_to_spec(base, aliases, referenced)
        }
        // Intersection (`left & right`, ADR 0102 §1/§3) has no Erlang spec
        // form either (Erlang `-spec` has no intersection-type constructor).
        // Any single member's spec is a sound (if wider) over-approximation —
        // a value of the intersection type is, in particular, a value of
        // `left` — so pick the left operand, mirroring `Difference` above.
        TypeAnnotation::Intersection { left, .. } => {
            type_annotation_to_spec(left, aliases, referenced)
        }
        // Self / Self class / <Name> class resolve to a receiver class at call
        // sites; in specs, treat as any()
        TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. }
        | TypeAnnotation::ClassOf { .. } => Document::Str("{'type', 0, 'any', []}"),
    }
}

/// Converts an alias name to its generated Erlang `-type` name.
///
/// Mirrors [`to_module_name`]'s `CamelCase` → `snake_case` convention (e.g.
/// `RestartStrategy` → `restart_strategy`) so the emitted `-type` reads as
/// idiomatic Erlang, matching the ADR 0108 goal of a more idiomatic FFI
/// boundary for Erlang/Elixir consumers.
fn alias_erlang_type_name(alias_name: &str) -> String {
    to_module_name(alias_name)
}

/// Builds a `user_type` reference to a named alias `-type` (e.g.
/// `{'user_type', 0, 'restart_strategy', []}`), for use at annotation sites
/// that reference the alias by name (ADR 0108 Codegen: "annotation sites
/// reference it" rather than re-expanding inline). Aliases are never
/// parametric (ADR 0108 defers `type Option(T) = ...`), so the argument list
/// is always empty.
fn alias_type_reference(alias_name: &str) -> Document<'static> {
    docvec![
        "{'user_type', 0, ",
        atom(alias_erlang_type_name(alias_name)),
        ", []}"
    ]
}

/// Maps a simple Beamtalk type name to its Core Erlang abstract type representation.
fn simple_type_to_spec(name: &str) -> Document<'static> {
    Document::Str(match name {
        "Integer" | "Character" => "{'type', 0, 'integer', []}",
        "Float" => "{'type', 0, 'float', []}",
        "Number" => "{'type', 0, 'number', []}",
        "String" => "{'type', 0, 'binary', []}",
        "Boolean" => "{'type', 0, 'boolean', []}",
        "Symbol" => "{'type', 0, 'atom', []}",
        "List" => "{'type', 0, 'list', []}",
        "Tuple" => "{'type', 0, 'tuple', 'any'}",
        "Dictionary" | "Set" => "{'type', 0, 'map', 'any'}",
        "Nil" | "UndefinedObject" => "{'atom', 0, 'nil'}",
        "True" => "{'atom', 0, 'true'}",
        "False" => "{'atom', 0, 'false'}",
        "Block" => "{'type', 0, 'fun', []}",
        _ => "{'type', 0, 'any', []}",
    })
}

/// Maps a generic `TypeAnnotation::Generic` to its Core Erlang abstract type representation.
///
/// For known parameterized types, incorporates the type parameters into the spec:
///
/// - `List(T)` → `{'type', 0, 'list', [T_spec]}`
/// - `Block(A, R)` → `{'type', 0, 'fun', [{'type', 0, 'product', [A_spec]}, R_spec]}`
/// - `Block(R)` → `{'type', 0, 'fun', [{'type', 0, 'product', []}, R_spec]}`
///
/// For types where Dialyzer does not support parameterization (Dictionary, Set, Tuple),
/// the base type is emitted without parameters. For unknown/custom classes, `any()` is used.
///
/// Unresolved type parameters (bare names like `T`, `E` that do not match a known class)
/// are recursively resolved — ultimately falling through to `any()` via [`simple_type_to_spec`].
fn generic_type_to_spec(
    base_name: &str,
    parameters: &[TypeAnnotation],
    aliases: Option<&AliasRegistry>,
    referenced: Option<&RefCell<HashSet<EcoString>>>,
) -> Document<'static> {
    match base_name {
        // List(T) → list(T) — Erlang list type supports an element type parameter
        "List" => {
            if let Some(elem) = parameters.first() {
                let elem_spec = type_annotation_to_spec(elem, aliases, referenced);
                docvec!["{'type', 0, 'list', [", elem_spec, "]}"]
            } else {
                Document::Str("{'type', 0, 'list', []}")
            }
        }
        // Block(...) → fun type with product args and return type
        // Block(R) = zero-arg block returning R
        // Block(A, R) = one-arg block with arg A, returning R
        // Block(A, B, R) = two-arg block, etc.
        // Last parameter is always the return type; preceding ones are arg types.
        "Block" => {
            if parameters.is_empty() {
                Document::Str("{'type', 0, 'fun', []}")
            } else {
                let (arg_params, return_param) = parameters.split_at(parameters.len() - 1);
                let return_spec = type_annotation_to_spec(&return_param[0], aliases, referenced);
                let arg_specs: Vec<Document<'static>> = arg_params
                    .iter()
                    .map(|p| type_annotation_to_spec(p, aliases, referenced))
                    .collect();
                let product = if arg_specs.is_empty() {
                    Document::Str("{'type', 0, 'product', []}")
                } else {
                    docvec![
                        "{'type', 0, 'product', [",
                        join(arg_specs, &Document::Str(", ")),
                        "]}"
                    ]
                };
                docvec!["{'type', 0, 'fun', [", product, ", ", return_spec, "]}"]
            }
        }
        // Dictionary, Set, Tuple — Dialyzer doesn't support parameterized forms
        // for these in the abstract type representation, so emit the base type.
        "Dictionary" | "Set" => Document::Str("{'type', 0, 'map', 'any'}"),
        "Tuple" => Document::Str("{'type', 0, 'tuple', 'any'}"),
        // Any other generic type (custom classes like Result, Array, etc.)
        // maps to any() — the base class is not a built-in Erlang type.
        // BT-2900: if the base name is itself a registered alias (e.g. a
        // non-parametric `type MyList = List` applied as `MyList(Integer)`),
        // reference it by name instead — mirrors the `Simple` case in
        // `type_annotation_to_spec`. The (illegitimate, since ADR 0108 defers
        // parametric aliases) type argument is dropped, same as any other
        // unresolvable generic application.
        _ => {
            if aliases.is_some_and(|registry| registry.has_alias(base_name)) {
                if let Some(tracker) = referenced {
                    tracker.borrow_mut().insert(base_name.into());
                }
                alias_type_reference(base_name)
            } else {
                simple_type_to_spec(base_name)
            }
        }
    }
}

/// Generates the spec attribute for a single method.
///
/// Returns `None` if the method has no type annotations at all.
/// When at least one annotation exists, unannotated parts default to `any()`.
///
/// For actor methods, the `dispatch_arity` is the selector arity (no Self/State params).
/// For value type methods, the arity includes the Self parameter.
///
/// `aliases`, when present, resolves alias-named annotations to `user_type`
/// references (ADR 0108, BT-2900) — see [`type_annotation_to_spec`].
///
/// `referenced`, when present, accumulates the alias names this method's
/// annotations actually reference (BT-2940) — see [`type_annotation_to_spec`].
pub fn generate_method_spec(
    method: &MethodDefinition,
    is_value_type: bool,
    aliases: Option<&AliasRegistry>,
    referenced: Option<&RefCell<HashSet<EcoString>>>,
) -> Option<Document<'static>> {
    let has_any_annotation = method.return_type.is_some()
        || method
            .parameters
            .iter()
            .any(|p| p.type_annotation.is_some());

    if !has_any_annotation {
        return None;
    }

    let erlang_name = method.selector.to_erlang_atom();

    let mut param_types: Vec<Document<'static>> = Vec::new();

    if is_value_type {
        param_types.push(Document::Str("{'type', 0, 'map', 'any'}"));
    }

    for param in &method.parameters {
        let type_spec = param
            .type_annotation
            .as_ref()
            .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
                type_annotation_to_spec(ann, aliases, referenced)
            });
        param_types.push(type_spec);
    }

    let return_spec = method
        .return_type
        .as_ref()
        .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
            type_annotation_to_spec(ann, aliases, referenced)
        });

    let arity = if is_value_type {
        method.parameters.len() + 1
    } else {
        method.parameters.len()
    };
    // Method arities are bounded by the parameter count and always fit in i64.
    let arity = i64::try_from(arity).unwrap_or(i64::MAX);

    let product = if param_types.is_empty() {
        Document::Str("{'type', 0, 'product', []}")
    } else {
        docvec![
            "{'type', 0, 'product', [",
            join(param_types, &Document::Str(", ")),
            "]}"
        ]
    };

    Some(docvec![
        "{",
        atom(erlang_name),
        ", ",
        int_lit(arity),
        "}",
        ", [{'type', 0, 'fun', [",
        product,
        ", ",
        return_spec,
        "]}]"
    ])
}

/// Generates the spec attribute for a class-side method.
///
/// Class methods have two implicit parameters (`ClassSelf`, `ClassVars`)
/// and use the `class_{selector}` naming convention.
///
/// `aliases`, when present, resolves alias-named annotations to `user_type`
/// references (ADR 0108, BT-2900) — see [`type_annotation_to_spec`].
///
/// `referenced`, when present, accumulates the alias names this method's
/// annotations actually reference (BT-2940) — see [`type_annotation_to_spec`].
fn generate_class_method_spec(
    method: &MethodDefinition,
    aliases: Option<&AliasRegistry>,
    referenced: Option<&RefCell<HashSet<EcoString>>>,
) -> Option<Document<'static>> {
    let has_any_annotation = method.return_type.is_some()
        || method
            .parameters
            .iter()
            .any(|p| p.type_annotation.is_some());

    if !has_any_annotation {
        return None;
    }

    let erlang_name = safe_class_method_fn_name(&method.selector.to_erlang_atom());

    let mut param_types: Vec<Document<'static>> = vec![
        Document::Str("{'type', 0, 'any', []}"),    // ClassSelf
        Document::Str("{'type', 0, 'map', 'any'}"), // ClassVars
    ];

    for param in &method.parameters {
        let type_spec = param
            .type_annotation
            .as_ref()
            .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
                type_annotation_to_spec(ann, aliases, referenced)
            });
        param_types.push(type_spec);
    }

    let return_spec = method
        .return_type
        .as_ref()
        .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
            type_annotation_to_spec(ann, aliases, referenced)
        });

    let arity = method.parameters.len() + 2;
    // Method arities are bounded by the parameter count and always fit in i64.
    let arity = i64::try_from(arity).unwrap_or(i64::MAX);

    let product = docvec![
        "{'type', 0, 'product', [",
        join(param_types, &Document::Str(", ")),
        "]}"
    ];

    Some(docvec![
        "{",
        atom(erlang_name),
        ", ",
        int_lit(arity),
        "}",
        ", [{'type', 0, 'fun', [",
        product,
        ", ",
        return_spec,
        "]}]"
    ])
}

/// Generates all spec attributes for methods in a class definition.
///
/// Returns a vector of spec attribute Documents, one per annotated method.
/// Only primary methods generate specs.
/// Includes both instance methods and class-side methods.
///
/// BT-1944: For actor classes (`is_value_type: false`), instance method specs
/// are skipped because actor methods are dispatch clauses inside `safe_dispatch/3`,
/// not standalone functions — a spec referencing a non-existent function is invalid.
/// Value type methods ARE standalone functions, so their specs are valid.
///
/// `aliases`, when present, resolves alias-named annotations to `user_type`
/// references (ADR 0108, BT-2900) — see [`type_annotation_to_spec`].
///
/// `referenced`, when present, accumulates the alias names this class's
/// method specs actually reference (BT-2940) — pass the same accumulator to
/// [`generate_alias_type_attrs`] to scope its emission to just those names.
pub fn generate_class_specs(
    class: &ClassDefinition,
    is_value_type: bool,
    aliases: Option<&AliasRegistry>,
    referenced: Option<&RefCell<HashSet<EcoString>>>,
) -> Vec<Document<'static>> {
    // BT-1944: Only generate instance method specs for value types where methods
    // are standalone functions. Actor instance methods live inside safe_dispatch/3.
    let instance_specs: Box<dyn Iterator<Item = Document<'static>>> = if is_value_type {
        Box::new(
            class
                .methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .filter_map(move |m| {
                    generate_method_spec(m, is_value_type, aliases, referenced)
                        .map(|spec| docvec!["'spec' =\n        [{", spec, "}]"])
                }),
        )
    } else {
        Box::new(std::iter::empty())
    };

    let class_specs = class
        .class_methods
        .iter()
        .filter(|m| m.kind == MethodKind::Primary)
        .filter_map(move |m| {
            generate_class_method_spec(m, aliases, referenced)
                .map(|spec| docvec!["'spec' =\n        [{", spec, "}]"])
        });

    instance_specs.chain(class_specs).collect()
}

/// Formats spec attributes for inclusion in the module `attributes [...]` list.
///
/// Returns `None` if there are no specs to generate.
pub fn format_spec_attributes(specs: &[Document<'static>]) -> Option<Document<'static>> {
    if specs.is_empty() {
        return None;
    }
    Some(join(specs.to_vec(), &Document::Str(",\n     ")))
}

/// Generates the `-type t()` module attribute for a Value class with `state:` fields.
///
/// Produces a map type that includes the `$beamtalk_class` tag field (as a required
/// `:=` association) followed by each declared `state:` field with its Erlang type.
///
/// Returns `None` if the class has no `state:` declarations.
///
/// ## Generated Format
///
/// ```erlang
/// 'type' =
///         [{'t', {'type', 0, 'map', [
///           {'type', 0, 'map_field_exact', [{'atom', 0, '$beamtalk_class'}, {'atom', 0, 'ClassName'}]},
///           {'type', 0, 'map_field_exact', [{'atom', 0, 'field1'}, FieldType1]},
///           ...]}, []}]
/// ```
///
/// `aliases`, when present, resolves alias-named field annotations to
/// `user_type` references (ADR 0108, BT-2900) — see [`type_annotation_to_spec`].
///
/// `referenced`, when present, accumulates the alias names this class's
/// `state:` fields actually reference (BT-2940) — see [`type_annotation_to_spec`].
pub fn generate_type_alias(
    class: &ClassDefinition,
    class_name: &str,
    aliases: Option<&AliasRegistry>,
    referenced: Option<&RefCell<HashSet<EcoString>>>,
) -> Option<Document<'static>> {
    if class.state.is_empty() {
        return None;
    }

    let mut field_types: Vec<Document<'static>> = Vec::new();

    // '$beamtalk_class' tag field: always present, value = the class atom
    field_types.push(docvec![
        "{'type', 0, 'map_field_exact', [{'atom', 0, '$beamtalk_class'}, {'atom', 0, ",
        atom(class_name.to_string()),
        "}]}"
    ]);

    // One required field entry per declared state: field
    for field in &class.state {
        let fname = atom(field.name.name.to_string());
        let ftype = field
            .type_annotation
            .as_ref()
            .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
                type_annotation_to_spec(ann, aliases, referenced)
            });
        field_types.push(docvec![
            "{'type', 0, 'map_field_exact', [{'atom', 0, ",
            fname,
            "}, ",
            ftype,
            "]}"
        ]);
    }

    Some(docvec![
        "'type' =\n        [{'t', {'type', 0, 'map', [",
        join(field_types, &Document::Str(", ")),
        "]}, []}]"
    ])
}

/// Generates the named `-type` attribute for a single type alias declaration
/// (ADR 0108, BT-2900).
///
/// Produces `'type' = [{alias_name, <expansion-spec>, []}]`, where
/// `<expansion-spec>` is the alias RHS's Erlang abstract type representation
/// — identical to what `type_annotation_to_spec` would produce for the
/// spelled-out annotation, since aliases are transparent (structural, not
/// nominal — ADR 0108 Semantics). A `\`/`&` RHS emits the same widened
/// over-approximation `type_annotation_to_spec` already applies to anonymous
/// `Difference`/`Intersection` (ADR 0108 Consequences — Positive).
fn generate_alias_type_attr(
    alias_name: &str,
    annotation: &TypeAnnotation,
    aliases: &AliasRegistry,
    referenced: Option<&RefCell<HashSet<EcoString>>>,
) -> Document<'static> {
    let erlang_name = atom(alias_erlang_type_name(alias_name));
    let expansion_spec = type_annotation_to_spec(annotation, Some(aliases), referenced);
    docvec![
        "'type' =\n        [{",
        erlang_name,
        ", ",
        expansion_spec,
        ", []}]"
    ]
}

/// Generates named `-type` attributes for the alias names in `referenced`,
/// transitively closed over any further aliases those aliases' own
/// expansions reference.
///
/// One `Document` per alias, in alias-name sorted order (deterministic
/// output — iteration order over `referenced`'s `HashSet` is unspecified).
/// Pass the result to [`format_alias_type_attributes`] to join them for
/// inclusion in the module `attributes [...]` list, mirroring
/// [`generate_class_specs`] / [`format_spec_attributes`]'s shape for
/// `'spec'` entries.
///
/// Called from the module-level codegen drivers (`actor_codegen.rs`,
/// `value_type_codegen.rs`, `supervisor_codegen.rs`,
/// `gen_server/native_facade.rs`; ADR 0108, BT-2909) after their
/// `generate_class_specs`/`generate_method_spec`/`generate_type_alias` call
/// sites have populated `referenced` by walking the module's own
/// annotations with `Some(&referenced)` — every class module that could
/// contain a `user_type` reference must declare the corresponding named
/// `-type` in the same module attribute list, since a reference to an
/// undeclared type is an `erlc` compile error, not just a Dialyzer warning.
///
/// BT-2940: emits a `-type` only for names actually referenced (directly or
/// transitively) by the module's own specs/state fields, not every alias in
/// `aliases` — before this, `aliases` being the full pre-loaded registry
/// seeded from every source file in the compilation unit (BT-2932) meant a
/// module using one cross-module alias declared `-type` attributes for the
/// full project's alias count (`A` aliases × `M` modules), growing Dialyzer's
/// PLT scan well past the actual reference count at project scale.
///
/// Transitive closure: `referenced` may grow while this function runs (e.g.
/// `type B = A | #z` — a module referencing only `B` must also emit `A`'s
/// `-type`, discovered by walking `B`'s own expansion) — so aliases already
/// in `referenced` are walked too, feeding any further alias names they
/// reference back into `referenced`, until a fixed point is reached.
pub fn generate_alias_type_attrs(
    aliases: &AliasRegistry,
    referenced: &RefCell<HashSet<EcoString>>,
) -> Vec<Document<'static>> {
    let mut processed: HashSet<EcoString> = HashSet::new();
    loop {
        let pending: Vec<EcoString> = referenced
            .borrow()
            .iter()
            .filter(|name| !processed.contains(*name))
            .cloned()
            .collect();
        if pending.is_empty() {
            break;
        }
        for name in pending {
            if let Some(info) = aliases.get(&name) {
                // Walking the alias's own expansion records any further
                // alias names it references into `referenced` too.
                let _ = type_annotation_to_spec(&info.annotation, Some(aliases), Some(referenced));
            }
            processed.insert(name);
        }
    }

    let mut names: Vec<EcoString> = referenced.borrow().iter().cloned().collect();
    names.sort_unstable();

    names
        .into_iter()
        .filter_map(|name| {
            aliases.get(&name).map(|info| {
                generate_alias_type_attr(&name, &info.annotation, aliases, Some(referenced))
            })
        })
        .collect()
}

/// Formats a list of alias `'type'` attribute Documents (from
/// [`generate_alias_type_attrs`]) for inclusion in the module
/// `attributes [...]` list.
///
/// Returns `None` if there are no alias types to generate. Shares
/// [`format_spec_attributes`]'s join shape (`,\n     ` between entries) since
/// both are lists of `Key = [...]` module attribute blocks.
///
pub fn format_alias_type_attributes(
    alias_types: &[Document<'static>],
) -> Option<Document<'static>> {
    if alias_types.is_empty() {
        return None;
    }
    Some(join(alias_types.to_vec(), &Document::Str(",\n     ")))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ClassModifiers, Identifier, KeywordPart, MessageSelector, MethodDefinition,
        ParameterDefinition, TypeAnnotation,
    };
    use crate::semantic_analysis::alias_registry::AliasInfo;
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn render(doc: &Document<'_>) -> String {
        doc.to_pretty_string()
    }

    #[test]
    fn integer_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Integer", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'integer', []}"
        );
    }

    #[test]
    fn string_type_maps_to_binary() {
        let ann = TypeAnnotation::simple("String", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'binary', []}"
        );
    }

    #[test]
    fn float_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Float", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'float', []}"
        );
    }

    #[test]
    fn boolean_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Boolean", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'boolean', []}"
        );
    }

    #[test]
    fn symbol_type_maps_to_atom() {
        let ann = TypeAnnotation::simple("Symbol", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'atom', []}"
        );
    }

    #[test]
    fn nil_maps_to_atom_nil() {
        let ann = TypeAnnotation::simple("Nil", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'atom', 0, 'nil'}"
        );
    }

    #[test]
    fn true_maps_to_atom_true() {
        let ann = TypeAnnotation::simple("True", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'atom', 0, 'true'}"
        );
    }

    #[test]
    fn false_maps_to_atom_false() {
        let ann = TypeAnnotation::simple("False", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'atom', 0, 'false'}"
        );
    }

    #[test]
    fn list_type_maps_correctly() {
        let ann = TypeAnnotation::simple("List", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'list', []}"
        );
    }

    #[test]
    fn dictionary_maps_to_map() {
        let ann = TypeAnnotation::simple("Dictionary", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn tuple_maps_correctly() {
        let ann = TypeAnnotation::simple("Tuple", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'tuple', 'any'}"
        );
    }

    #[test]
    fn custom_class_maps_to_any() {
        let ann = TypeAnnotation::simple("Counter", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'any', []}"
        );
    }

    #[test]
    fn union_type_maps_correctly() {
        let ann = TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("Nil", span()),
            ],
            span(),
        );
        let result = render(&type_annotation_to_spec(&ann, None, None));
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []}, {'atom', 0, 'nil'}]}"
        );
    }

    #[test]
    fn singleton_type_maps_to_atom() {
        let ann = TypeAnnotation::singleton("north", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'atom', 0, 'north'}"
        );
    }

    #[test]
    fn false_or_type_maps_correctly() {
        let ann = TypeAnnotation::false_or(TypeAnnotation::simple("Integer", span()), span());
        let result = render(&type_annotation_to_spec(&ann, None, None));
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []}, {'atom', 0, 'false'}]}"
        );
    }

    #[test]
    fn unary_method_with_return_type_generates_spec() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, None, None).unwrap());
        assert_eq!(
            spec,
            "{'getBalance', 0}, [{'type', 0, 'fun', [{'type', 0, 'product', []}, {'type', 0, 'integer', []}]}]"
        );
    }

    #[test]
    fn keyword_method_with_typed_param_generates_spec() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("amount", span()),
                TypeAnnotation::simple("Integer", span()),
            )],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, None, None).unwrap());
        assert_eq!(
            spec,
            "{'deposit:', 1}, [{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}]}, {'type', 0, 'integer', []}]}]"
        );
    }

    #[test]
    fn method_without_annotations_returns_none() {
        let method = MethodDefinition::new(
            MessageSelector::Unary("increment".into()),
            vec![],
            vec![],
            span(),
        );

        assert!(generate_method_spec(&method, false, None, None).is_none());
    }

    #[test]
    fn method_with_only_return_type_uses_any_for_params() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getValue".into()),
            vec![ParameterDefinition::new(Identifier::new("x", span()))],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, None, None).unwrap());
        assert!(spec.contains("'any'"));
        assert!(spec.contains("'integer'"));
    }

    #[test]
    fn value_type_method_includes_self_param() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("size".into()),
            vec![],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, true, None, None).unwrap());
        assert!(spec.contains("{'size', 1}"));
        assert!(spec.contains("'map', 'any'"));
    }

    #[test]
    fn multiple_keyword_params() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![
                KeywordPart::new("transfer:", span()),
                KeywordPart::new("to:", span()),
            ]),
            vec![
                ParameterDefinition::with_type(
                    Identifier::new("amount", span()),
                    TypeAnnotation::simple("Integer", span()),
                ),
                ParameterDefinition::with_type(
                    Identifier::new("target", span()),
                    TypeAnnotation::simple("String", span()),
                ),
            ],
            vec![],
            TypeAnnotation::simple("Boolean", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, None, None).unwrap());
        assert!(spec.contains("'transfer:to:'"));
        assert!(spec.contains("'integer'"));
        assert!(spec.contains("'binary'"));
        assert!(spec.contains("'boolean'"));
    }

    #[test]
    fn union_return_type() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("find:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("key", span()),
                TypeAnnotation::simple("Symbol", span()),
            )],
            vec![],
            TypeAnnotation::union(
                vec![
                    TypeAnnotation::simple("Integer", span()),
                    TypeAnnotation::simple("Nil", span()),
                ],
                span(),
            ),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, None, None).unwrap());
        assert!(spec.contains("'union'"));
        assert!(spec.contains("'integer'"));
        assert!(spec.contains("'nil'"));
    }

    #[test]
    fn block_type_maps_to_fun() {
        let ann = TypeAnnotation::simple("Block", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'fun', []}"
        );
    }

    #[test]
    fn number_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Number", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'number', []}"
        );
    }

    #[test]
    fn character_maps_to_integer() {
        let ann = TypeAnnotation::simple("Character", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'integer', []}"
        );
    }

    #[test]
    fn set_maps_to_map() {
        let ann = TypeAnnotation::simple("Set", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn format_spec_attributes_empty() {
        assert!(format_spec_attributes(&[]).is_none());
    }

    #[test]
    fn format_spec_attributes_single() {
        let specs = vec![Document::Str("'spec' =\n        [{{spec}}]")];
        let result = render(&format_spec_attributes(&specs).unwrap());
        assert_eq!(result, "'spec' =\n        [{{spec}}]");
    }

    #[test]
    fn format_spec_attributes_multiple() {
        let specs = vec![
            Document::Str("'spec' =\n        [{{spec1}}]"),
            Document::Str("'spec' =\n        [{{spec2}}]"),
        ];
        let result = render(&format_spec_attributes(&specs).unwrap());
        assert!(result.contains("spec1"));
        assert!(result.contains("spec2"));
    }

    #[test]
    fn value_type_method_with_two_params_generates_correct_cons() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![
                KeywordPart::new("at:", span()),
                KeywordPart::new("put:", span()),
            ]),
            vec![
                ParameterDefinition::with_type(
                    Identifier::new("key", span()),
                    TypeAnnotation::simple("Symbol", span()),
                ),
                ParameterDefinition::with_type(
                    Identifier::new("value", span()),
                    TypeAnnotation::simple("Object", span()),
                ),
            ],
            vec![],
            TypeAnnotation::simple("Object", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, true, None, None).unwrap());
        assert!(spec.contains("{'at:put:', 3}"));
        assert!(
            spec.contains(
                "{'type', 0, 'map', 'any'}, {'type', 0, 'atom', []}, {'type', 0, 'any', []}"
            ),
            "Product should use proper cons nesting: got {spec}"
        );
    }

    #[test]
    fn three_element_union_generates_correct_cons() {
        let ann = TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Nil", span()),
            ],
            span(),
        );
        let result = render(&type_annotation_to_spec(&ann, None, None));
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []}, {'type', 0, 'binary', []}, {'atom', 0, 'nil'}]}"
        );
    }

    #[test]
    fn class_method_spec_includes_class_self_and_class_vars() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("from:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("start", span()),
                TypeAnnotation::simple("Integer", span()),
            )],
            vec![],
            TypeAnnotation::simple("Stream", span()),
            span(),
        );

        let spec = render(&generate_class_method_spec(&method, None, None).unwrap());
        assert!(
            spec.contains("{'class_from:', 3}"),
            "Expected class_ prefix and arity 3 (ClassSelf + ClassVars + 1 param), got: {spec}"
        );
        assert!(spec.contains("'any'"));
        assert!(spec.contains("'map'"));
        assert!(spec.contains("'integer'"));
    }

    #[test]
    fn class_method_without_annotations_returns_none() {
        let method = MethodDefinition::new(
            MessageSelector::Unary("create".into()),
            vec![],
            vec![],
            span(),
        );
        assert!(generate_class_method_spec(&method, None, None).is_none());
    }

    // Tests for generate_type_alias

    fn make_class_with_state(
        class_name: &str,
        fields: Vec<(&str, Option<TypeAnnotation>)>,
    ) -> ClassDefinition {
        use crate::ast::StateDeclaration;
        let state = fields
            .into_iter()
            .map(|(name, ann)| match ann {
                Some(a) => StateDeclaration::with_type(Identifier::new(name, span()), a, span()),
                None => StateDeclaration::new(Identifier::new(name, span()), span()),
            })
            .collect();
        ClassDefinition::with_modifiers(
            Identifier::new(class_name, span()),
            Some(Identifier::new("Value", span())),
            ClassModifiers::default(),
            state,
            vec![],
            span(),
        )
    }

    #[test]
    fn type_alias_returns_none_for_empty_state() {
        let class = make_class_with_state("Empty", vec![]);
        assert!(generate_type_alias(&class, "Empty", None, None).is_none());
    }

    #[test]
    fn type_alias_includes_beamtalk_class_tag_field() {
        let class = make_class_with_state(
            "Point",
            vec![("x", Some(TypeAnnotation::simple("Integer", span())))],
        );
        let result = render(&generate_type_alias(&class, "Point", None, None).unwrap());
        assert!(
            result.contains("'$beamtalk_class'"),
            "Should include $beamtalk_class tag field, got: {result}"
        );
        assert!(
            result.contains("'Point'"),
            "Should include class name atom, got: {result}"
        );
    }

    #[test]
    fn type_alias_maps_integer_field() {
        let class = make_class_with_state(
            "Counter",
            vec![("count", Some(TypeAnnotation::simple("Integer", span())))],
        );
        let result = render(&generate_type_alias(&class, "Counter", None, None).unwrap());
        assert!(
            result.contains("'count'"),
            "Should include field name, got: {result}"
        );
        assert!(
            result.contains("'integer'"),
            "Should include integer type, got: {result}"
        );
        assert!(
            result.contains("'map_field_exact'"),
            "Should use map_field_exact for required fields, got: {result}"
        );
    }

    #[test]
    fn type_alias_http_response_fields() {
        let class = make_class_with_state(
            "HTTPResponse",
            vec![
                ("status", Some(TypeAnnotation::simple("Integer", span()))),
                ("headers", Some(TypeAnnotation::simple("List", span()))),
                ("body", Some(TypeAnnotation::simple("String", span()))),
            ],
        );
        let result = render(&generate_type_alias(&class, "HTTPResponse", None, None).unwrap());
        assert!(
            result.contains("'HTTPResponse'"),
            "Should contain class name atom"
        );
        assert!(result.contains("'status'"), "Should contain status field");
        assert!(result.contains("'headers'"), "Should contain headers field");
        assert!(result.contains("'body'"), "Should contain body field");
        assert!(result.contains("'integer'"), "status should be integer");
        assert!(result.contains("'list'"), "headers should be list");
        assert!(
            result.contains("'binary'"),
            "body (String) should be binary"
        );
        assert!(
            result.starts_with("'type' ="),
            "Should start with type attribute"
        );
        assert!(
            result.contains("'map'"),
            "Should use map type, got: {result}"
        );
    }

    #[test]
    fn type_alias_unannotated_field_uses_any() {
        let class = make_class_with_state("Pair", vec![("value", None)]);
        let result = render(&generate_type_alias(&class, "Pair", None, None).unwrap());
        assert!(
            result.contains("'any'"),
            "Unannotated field should use any(), got: {result}"
        );
    }

    // Tests for generic type spec generation (BT-1574)

    #[test]
    fn generic_list_with_integer_element() {
        let ann = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'list', [{'type', 0, 'integer', []}]}"
        );
    }

    #[test]
    fn generic_list_with_string_element() {
        let ann = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("String", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'list', [{'type', 0, 'binary', []}]}"
        );
    }

    #[test]
    fn generic_list_no_params_falls_back_to_unparameterized() {
        let ann = TypeAnnotation::generic(Identifier::new("List", span()), vec![], span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'list', []}"
        );
    }

    #[test]
    fn generic_block_zero_arg_with_return_type() {
        // Block(Integer) = zero-arg block returning Integer
        let ann = TypeAnnotation::generic(
            Identifier::new("Block", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'fun', [{'type', 0, 'product', []}, {'type', 0, 'integer', []}]}"
        );
    }

    #[test]
    fn generic_block_one_arg() {
        // Block(Integer, String) = one-arg block: Integer → String
        let ann = TypeAnnotation::generic(
            Identifier::new("Block", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
            ],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}]}, {'type', 0, 'binary', []}]}"
        );
    }

    #[test]
    fn generic_block_two_args() {
        // Block(Integer, String, Boolean) = two-arg block: (Integer, String) → Boolean
        let ann = TypeAnnotation::generic(
            Identifier::new("Block", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Boolean", span()),
            ],
            span(),
        );
        let result = render(&type_annotation_to_spec(&ann, None, None));
        assert_eq!(
            result,
            "{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}, {'type', 0, 'binary', []}]}, {'type', 0, 'boolean', []}]}"
        );
    }

    #[test]
    fn generic_block_no_params_falls_back() {
        let ann = TypeAnnotation::generic(Identifier::new("Block", span()), vec![], span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'fun', []}"
        );
    }

    #[test]
    fn generic_dictionary_ignores_params() {
        // Dictionary(String, Integer) → map() (Dialyzer doesn't parameterize maps)
        let ann = TypeAnnotation::generic(
            Identifier::new("Dictionary", span()),
            vec![
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Integer", span()),
            ],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn generic_set_ignores_params() {
        let ann = TypeAnnotation::generic(
            Identifier::new("Set", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn generic_tuple_ignores_params() {
        let ann = TypeAnnotation::generic(
            Identifier::new("Tuple", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
            ],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'tuple', 'any'}"
        );
    }

    #[test]
    fn generic_custom_class_maps_to_any() {
        // Result(Integer, Error) — custom class maps to any()
        let ann = TypeAnnotation::generic(
            Identifier::new("Result", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("Error", span()),
            ],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'any', []}"
        );
    }

    #[test]
    fn unresolved_type_param_maps_to_any() {
        // Bare type variable T maps to any() via simple_type_to_spec catch-all
        let ann = TypeAnnotation::simple("T", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'any', []}"
        );
    }

    #[test]
    fn generic_list_with_unresolved_type_param() {
        // List(T) — T is unresolved, maps to any()
        let ann = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("T", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'list', [{'type', 0, 'any', []}]}"
        );
    }

    #[test]
    fn nested_generic_list_of_lists() {
        // List(List(Integer)) — nested generic
        let inner = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        let ann = TypeAnnotation::generic(Identifier::new("List", span()), vec![inner], span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann, None, None)),
            "{'type', 0, 'list', [{'type', 0, 'list', [{'type', 0, 'integer', []}]}]}"
        );
    }

    #[test]
    fn generic_block_with_nested_generic_return() {
        // Block(Integer, List(String)) — block taking Integer, returning List(String)
        let return_type = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("String", span())],
            span(),
        );
        let ann = TypeAnnotation::generic(
            Identifier::new("Block", span()),
            vec![TypeAnnotation::simple("Integer", span()), return_type],
            span(),
        );
        let result = render(&type_annotation_to_spec(&ann, None, None));
        assert_eq!(
            result,
            "{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}]}, {'type', 0, 'list', [{'type', 0, 'binary', []}]}]}"
        );
    }

    #[test]
    fn method_with_generic_param_type_generates_spec() {
        // Method: process: items :: List(Integer) -> Integer
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("process:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("items", span()),
                TypeAnnotation::generic(
                    Identifier::new("List", span()),
                    vec![TypeAnnotation::simple("Integer", span())],
                    span(),
                ),
            )],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, None, None).unwrap());
        assert!(
            spec.contains("'list', [{'type', 0, 'integer', []}]"),
            "Should contain parameterized list type, got: {spec}"
        );
        assert!(
            spec.contains("{'process:', 1}"),
            "Should have correct selector and arity, got: {spec}"
        );
    }

    #[test]
    fn method_with_generic_return_type_generates_spec() {
        // Method: getItems -> List(String)
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getItems".into()),
            vec![],
            vec![],
            TypeAnnotation::generic(
                Identifier::new("List", span()),
                vec![TypeAnnotation::simple("String", span())],
                span(),
            ),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, None, None).unwrap());
        assert!(
            spec.contains("'list', [{'type', 0, 'binary', []}]"),
            "Return type should be parameterized list, got: {spec}"
        );
    }

    #[test]
    fn method_with_block_param_generates_fun_spec() {
        // Method: do: block :: Block(Integer, Object) -> Nil
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("do:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("block", span()),
                TypeAnnotation::generic(
                    Identifier::new("Block", span()),
                    vec![
                        TypeAnnotation::simple("Integer", span()),
                        TypeAnnotation::simple("Object", span()),
                    ],
                    span(),
                ),
            )],
            vec![],
            TypeAnnotation::simple("Nil", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, None, None).unwrap());
        assert!(
            spec.contains("'fun'"),
            "Block param should generate fun type, got: {spec}"
        );
        assert!(
            spec.contains("'integer'"),
            "Block arg type should be integer, got: {spec}"
        );
    }

    #[test]
    fn type_alias_with_generic_field_type() {
        // Value class with a field typed as List(Integer)
        use crate::ast::StateDeclaration;
        let state = vec![StateDeclaration::with_type(
            Identifier::new("items", span()),
            TypeAnnotation::generic(
                Identifier::new("List", span()),
                vec![TypeAnnotation::simple("Integer", span())],
                span(),
            ),
            span(),
        )];
        let class = ClassDefinition::with_modifiers(
            Identifier::new("Container", span()),
            Some(Identifier::new("Value", span())),
            ClassModifiers::default(),
            state,
            vec![],
            span(),
        );
        let result = render(&generate_type_alias(&class, "Container", None, None).unwrap());
        assert!(
            result.contains("'list', [{'type', 0, 'integer', []}]"),
            "Field type should be parameterized list, got: {result}"
        );
    }

    // ---- Named `-type` emission for type aliases (ADR 0108, BT-2900) ----

    /// Builds an `AliasRegistry` containing a single alias `name = annotation`.
    fn alias_registry_with(name: &str, annotation: TypeAnnotation) -> AliasRegistry {
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: name.into(),
            annotation,
            is_internal: false,
            package: None,
            span: span(),
        });
        registry
    }

    fn restart_strategy_expansion() -> TypeAnnotation {
        TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::singleton("temporary", span()),
                TypeAnnotation::singleton("transient", span()),
                TypeAnnotation::singleton("permanent", span()),
            ],
            span: span(),
        }
    }

    #[test]
    fn alias_reference_emits_user_type_reference() {
        // A `Simple` annotation naming a registered alias must emit a
        // `user_type` reference, not `any()` and not an inlined re-expansion
        // of the union.
        let registry = alias_registry_with("RestartStrategy", restart_strategy_expansion());
        let ann = TypeAnnotation::simple("RestartStrategy", span());

        let result = render(&type_annotation_to_spec(&ann, Some(&registry), None));
        assert_eq!(result, "{'user_type', 0, 'restart_strategy', []}");
    }

    #[test]
    fn alias_reference_without_registry_falls_back_to_any() {
        // `None` (no alias registry supplied) reproduces pre-ADR-0108
        // behaviour exactly: an unrecognised `Simple` name is any().
        let ann = TypeAnnotation::simple("RestartStrategy", span());
        let result = render(&type_annotation_to_spec(&ann, None, None));
        assert_eq!(result, "{'type', 0, 'any', []}");
    }

    #[test]
    fn alias_reference_with_registry_not_containing_name_falls_back_to_any() {
        // A registry that doesn't have this particular name behaves like `None`
        // for that name — only registered aliases get `user_type` treatment.
        let registry = alias_registry_with("Timeout", TypeAnnotation::simple("Integer", span()));
        let ann = TypeAnnotation::simple("Counter", span());
        let result = render(&type_annotation_to_spec(&ann, Some(&registry), None));
        assert_eq!(result, "{'type', 0, 'any', []}");
    }

    #[test]
    fn alias_reference_inside_union_member_uses_user_type() {
        // Nested `Simple` positions (e.g. a member of a larger union) must
        // also consult the alias table, mirroring resolve_type_annotation's
        // recursive alias lookup (BT-2895).
        let registry = alias_registry_with("Timeout", TypeAnnotation::simple("Integer", span()));
        let ann = TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Timeout", span()),
                TypeAnnotation::simple("Nil", span()),
            ],
            span(),
        );
        let result = render(&type_annotation_to_spec(&ann, Some(&registry), None));
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'user_type', 0, 'timeout', []}, {'atom', 0, 'nil'}]}"
        );
    }

    #[test]
    fn alias_reference_as_generic_base_uses_user_type() {
        // A non-parametric alias applied generically (e.g. `MyList(Integer)`
        // where `type MyList = List`) is ill-formed, but the generic-base
        // catch-all should still reference the alias by name (dropping the
        // type argument) rather than falling through to any() — mirrors the
        // `Simple` case for symmetry.
        let registry = alias_registry_with("MyList", TypeAnnotation::simple("List", span()));
        let ann = TypeAnnotation::generic(
            Identifier::new("MyList", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        let result = render(&type_annotation_to_spec(&ann, Some(&registry), None));
        assert_eq!(result, "{'user_type', 0, 'my_list', []}");
    }

    #[test]
    fn generate_alias_type_attr_for_singleton_union() {
        // `type RestartStrategy = #temporary | #transient | #permanent`
        let registry = alias_registry_with("RestartStrategy", restart_strategy_expansion());
        let result = render(&generate_alias_type_attr(
            "RestartStrategy",
            &restart_strategy_expansion(),
            &registry,
            None,
        ));
        assert_eq!(
            result,
            "'type' =\n        [{'restart_strategy', {'type', 0, 'union', \
             [{'atom', 0, 'temporary'}, {'atom', 0, 'transient'}, {'atom', 0, 'permanent'}]}, []}]"
        );
    }

    #[test]
    fn generate_alias_type_attr_widens_difference() {
        // `type PublicTag = Symbol \ (#reserved | #internal)` — no exact
        // Erlang spec form for difference; widens to the base (Symbol),
        // same over-approximation `type_annotation_to_spec` already applies
        // to anonymous `Difference` (ADR 0108 Consequences — Positive).
        let excluded = TypeAnnotation::union(
            vec![
                TypeAnnotation::singleton("reserved", span()),
                TypeAnnotation::singleton("internal", span()),
            ],
            span(),
        );
        let rhs = TypeAnnotation::Difference {
            base: Box::new(TypeAnnotation::simple("Symbol", span())),
            excluded: Box::new(excluded),
            span: span(),
        };
        let registry = alias_registry_with("PublicTag", rhs.clone());
        let result = render(&generate_alias_type_attr(
            "PublicTag",
            &rhs,
            &registry,
            None,
        ));
        assert_eq!(
            result,
            "'type' =\n        [{'public_tag', {'type', 0, 'atom', []}, []}]"
        );
    }

    #[test]
    fn generate_alias_type_attr_widens_intersection() {
        // `type X = Collection(Object) & Comparable` — widens to the left
        // operand, same as anonymous `Intersection`.
        let rhs = TypeAnnotation::Intersection {
            left: Box::new(TypeAnnotation::simple("Integer", span())),
            right: Box::new(TypeAnnotation::simple("Comparable", span())),
            span: span(),
        };
        let registry = alias_registry_with("X", rhs.clone());
        let result = render(&generate_alias_type_attr("X", &rhs, &registry, None));
        assert_eq!(
            result,
            "'type' =\n        [{'x', {'type', 0, 'integer', []}, []}]"
        );
    }

    #[test]
    fn generate_alias_type_attr_references_another_alias_by_name() {
        // `type B = A | #z`, `type A = #x | #y` — B's generated `-type` must
        // reference `a()` via `user_type`, not re-expand A inline (ADR 0108
        // Codegen: "annotation sites reference it").
        let a_expansion = TypeAnnotation::union(
            vec![
                TypeAnnotation::singleton("x", span()),
                TypeAnnotation::singleton("y", span()),
            ],
            span(),
        );
        let b_expansion = TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("A", span()),
                TypeAnnotation::singleton("z", span()),
            ],
            span(),
        );
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "A".into(),
            annotation: a_expansion,
            is_internal: false,
            package: None,
            span: span(),
        });
        registry.register_test_alias(AliasInfo {
            name: "B".into(),
            annotation: b_expansion.clone(),
            is_internal: false,
            package: None,
            span: span(),
        });

        let result = render(&generate_alias_type_attr(
            "B",
            &b_expansion,
            &registry,
            None,
        ));
        assert!(
            result.contains("{'user_type', 0, 'a', []}"),
            "B's -type should reference A by name, not inline it, got: {result}"
        );
        assert!(
            !result.contains("'x'") && !result.contains("'y'"),
            "B's -type should not contain A's expansion inline, got: {result}"
        );
    }

    #[test]
    fn generate_alias_type_attrs_sorted_deterministic() {
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "Zeta".into(),
            annotation: TypeAnnotation::simple("Integer", span()),
            is_internal: false,
            package: None,
            span: span(),
        });
        registry.register_test_alias(AliasInfo {
            name: "Alpha".into(),
            annotation: TypeAnnotation::simple("String", span()),
            is_internal: false,
            package: None,
            span: span(),
        });

        // BT-2940: both names must be marked referenced for this test —
        // `generate_alias_type_attrs` now only emits names present in the
        // `referenced` accumulator, not every name in `aliases`.
        let referenced = RefCell::new(HashSet::from(["Zeta".into(), "Alpha".into()]));
        let attrs: Vec<String> = generate_alias_type_attrs(&registry, &referenced)
            .iter()
            .map(render)
            .collect();
        assert_eq!(attrs.len(), 2);
        assert!(
            attrs[0].contains("'alpha'"),
            "Alpha should sort before Zeta, got: {attrs:?}"
        );
        assert!(
            attrs[1].contains("'zeta'"),
            "Zeta should sort after Alpha, got: {attrs:?}"
        );
    }

    #[test]
    fn generate_alias_type_attrs_empty_registry_returns_empty_vec() {
        let registry = AliasRegistry::new();
        let referenced = RefCell::new(HashSet::new());
        assert!(generate_alias_type_attrs(&registry, &referenced).is_empty());
    }

    #[test]
    fn generate_alias_type_attrs_only_emits_referenced_names() {
        // BT-2940: a registry with several pre-loaded aliases, only one of
        // which is actually referenced, must only emit a `-type` for that
        // one — not every alias in the registry (the A×M scaling bug).
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "Used".into(),
            annotation: TypeAnnotation::simple("Integer", span()),
            is_internal: false,
            package: None,
            span: span(),
        });
        registry.register_test_alias(AliasInfo {
            name: "Unused".into(),
            annotation: TypeAnnotation::simple("String", span()),
            is_internal: false,
            package: None,
            span: span(),
        });

        let referenced = RefCell::new(HashSet::from(["Used".into()]));
        let attrs: Vec<String> = generate_alias_type_attrs(&registry, &referenced)
            .iter()
            .map(render)
            .collect();
        assert_eq!(
            attrs.len(),
            1,
            "only the referenced alias should get a -type declaration, got: {attrs:?}"
        );
        assert!(attrs[0].contains("'used'"), "got: {attrs:?}");
        assert!(
            !attrs[0].contains("'unused'"),
            "unreferenced alias must not be emitted, got: {attrs:?}"
        );
    }

    #[test]
    fn generate_alias_type_attrs_includes_transitive_alias_dependency() {
        // BT-2940: `type B = A | #z` — a module referencing only `B` must
        // still get `A`'s `-type` declaration too, since B's own expansion
        // names it via `user_type`. A third, wholly unrelated alias must
        // still be excluded.
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "A".into(),
            annotation: TypeAnnotation::union(
                vec![
                    TypeAnnotation::singleton("x", span()),
                    TypeAnnotation::singleton("y", span()),
                ],
                span(),
            ),
            is_internal: false,
            package: None,
            span: span(),
        });
        registry.register_test_alias(AliasInfo {
            name: "B".into(),
            annotation: TypeAnnotation::union(
                vec![
                    TypeAnnotation::simple("A", span()),
                    TypeAnnotation::singleton("z", span()),
                ],
                span(),
            ),
            is_internal: false,
            package: None,
            span: span(),
        });
        registry.register_test_alias(AliasInfo {
            name: "Unrelated".into(),
            annotation: TypeAnnotation::simple("Integer", span()),
            is_internal: false,
            package: None,
            span: span(),
        });

        // Only B is directly referenced by the module's own specs.
        let referenced = RefCell::new(HashSet::from(["B".into()]));
        let attrs: Vec<String> = generate_alias_type_attrs(&registry, &referenced)
            .iter()
            .map(render)
            .collect();
        assert_eq!(
            attrs.len(),
            2,
            "B and its transitive dependency A should be emitted, got: {attrs:?}"
        );
        assert!(attrs.iter().any(|a| a.contains("'a'")), "got: {attrs:?}");
        assert!(attrs.iter().any(|a| a.contains("'b'")), "got: {attrs:?}");
        assert!(
            !attrs.iter().any(|a| a.contains("'unrelated'")),
            "unrelated alias must not be emitted, got: {attrs:?}"
        );
    }

    #[test]
    fn format_alias_type_attributes_empty() {
        assert!(format_alias_type_attributes(&[]).is_none());
    }

    #[test]
    fn format_alias_type_attributes_joins_multiple() {
        let attrs = vec![
            Document::Str("'type' =\n        [{'a', {'type', 0, 'integer', []}, []}]"),
            Document::Str("'type' =\n        [{'b', {'type', 0, 'binary', []}, []}]"),
        ];
        let result = render(&format_alias_type_attributes(&attrs).unwrap());
        assert!(result.contains("'a'"));
        assert!(result.contains("'b'"));
        assert!(result.contains(",\n     "));
    }

    // ---- Annotation-site reference correctness (method/class/field specs) ----

    #[test]
    fn method_param_annotated_with_alias_emits_user_type_reference() {
        // `deposit: strategy :: RestartStrategy -> Nil`
        let registry = alias_registry_with("RestartStrategy", restart_strategy_expansion());
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("strategy", span()),
                TypeAnnotation::simple("RestartStrategy", span()),
            )],
            vec![],
            TypeAnnotation::simple("Nil", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false, Some(&registry), None).unwrap());
        assert!(
            spec.contains("{'user_type', 0, 'restart_strategy', []}"),
            "Method param typed with an alias should reference the named -type, got: {spec}"
        );
        assert!(
            !spec.contains("'temporary'"),
            "Should not inline the alias's expansion, got: {spec}"
        );
    }

    #[test]
    fn method_without_alias_registry_unaffected_by_unrelated_aliases() {
        // Confirms `generate_class_specs`/`generate_method_spec` output is
        // byte-identical whether an (unrelated or absent) alias registry is
        // supplied, when the method's own annotations never reference an
        // alias — the only codegen surface aliases touch is `type_annotation_to_spec`
        // resolving a name that IS in the table (ADR 0108 Consequences).
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );
        let registry = alias_registry_with("RestartStrategy", restart_strategy_expansion());

        let without_registry = render(&generate_method_spec(&method, false, None, None).unwrap());
        let with_unrelated_registry =
            render(&generate_method_spec(&method, false, Some(&registry), None).unwrap());
        assert_eq!(without_registry, with_unrelated_registry);
    }

    #[test]
    fn class_method_param_annotated_with_alias_emits_user_type_reference() {
        let registry = alias_registry_with("RestartStrategy", restart_strategy_expansion());
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("from:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("strategy", span()),
                TypeAnnotation::simple("RestartStrategy", span()),
            )],
            vec![],
            TypeAnnotation::simple("Nil", span()),
            span(),
        );

        let spec = render(&generate_class_method_spec(&method, Some(&registry), None).unwrap());
        assert!(
            spec.contains("{'user_type', 0, 'restart_strategy', []}"),
            "Class method param typed with an alias should reference the named -type, got: {spec}"
        );
    }

    #[test]
    fn value_type_field_annotated_with_alias_emits_user_type_reference() {
        let registry = alias_registry_with("RestartStrategy", restart_strategy_expansion());
        let class = make_class_with_state(
            "Child",
            vec![(
                "strategy",
                Some(TypeAnnotation::simple("RestartStrategy", span())),
            )],
        );
        let result = render(&generate_type_alias(&class, "Child", Some(&registry), None).unwrap());
        assert!(
            result.contains("{'user_type', 0, 'restart_strategy', []}"),
            "State field typed with an alias should reference the named -type, got: {result}"
        );
    }

    #[test]
    fn generate_class_specs_byte_identical_with_and_without_unrelated_alias_registry() {
        // ADR 0108 Consequences: "generated Core Erlang for message dispatch,
        // field access, etc. is byte-identical with or without the alias" —
        // for a class whose signatures never reference the alias, the spec
        // output (the only surface aliases touch) must also be unaffected.
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("size".into()),
            vec![],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );
        let class = ClassDefinition::with_modifiers(
            Identifier::new("Bag", span()),
            Some(Identifier::new("Value", span())),
            ClassModifiers::default(),
            vec![],
            vec![method],
            span(),
        );
        let registry = alias_registry_with("RestartStrategy", restart_strategy_expansion());

        let without: Vec<String> = generate_class_specs(&class, true, None, None)
            .iter()
            .map(render)
            .collect();
        let with: Vec<String> = generate_class_specs(&class, true, Some(&registry), None)
            .iter()
            .map(render)
            .collect();
        assert_eq!(without, with);
    }
}
