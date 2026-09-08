// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `DispatchSpec` — the shared `has_method/1` emitter for actor and
//! value-type classes (BT-3467, ADR 0006).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! `gen_server/dispatch.rs` (actor `has_method/1`) and `value_type_codegen.rs`
//! (`generate_primitive_has_method`/`generate_minimal_has_method`) implement
//! the same reflection surface — "does this class understand `Selector`,
//! locally, via a foreign extension, or via an ancestor?" — and had drifted:
//! the actor version only checked its own primary methods, silently omitting
//! the extension-registry check, superclass delegation, and the
//! catch-all-DNU short-circuit that the value-type version already had. That
//! made `respondsTo:` answer differently for an actor than for a value type
//! given the identical situation (an extension method, an inherited method,
//! or a class whose `doesNotUnderstand:args:` accepts everything).
//!
//! [`DispatchSpec`] captures the *only* things that differ between the two
//! call sites — which selectors are checked directly and which class/module
//! names to consult — so [`generate_has_method_from_spec`] is the one
//! emitter both `gen_server::dispatch::generate_has_method` and
//! `value_type_codegen::generate_primitive_has_method`/
//! `generate_minimal_has_method` render through.

use super::value_accessors::AutoSlotMethods;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};
use beamtalk_core::ast::{ClassDefinition, Expression};

/// How `has_method/1` delegates to its superclass when `Selector` isn't
/// found locally or in the extension registry.
///
/// The two kinds render different Core Erlang, not just a different name:
/// - [`Static`](SuperclassDelegation::Static) calls the superclass's
///   *compiled module* directly (`call Module:'has_method'(Selector)`),
///   resolved once at this class's own compile time.
/// - [`Dynamic`](SuperclassDelegation::Dynamic) calls
///   `beamtalk_dispatch:'responds_to'(Selector, ClassName)` — the runtime,
///   class-registry-keyed hierarchy walk (ADR 0006, ADR 0032 Phase 3) —
///   which re-resolves the superclass from the live registry on every call.
///
/// Value types use `Static`: `beamtalk_primitive:value_type_responds_to/2`
/// calls a value type's compiled `has_method/1` directly with no dynamic
/// hierarchy-walk fallback, so `has_method/1` itself must walk statically
/// (unchanged by BT-3467 — byte-for-byte the prior hand-written output).
///
/// Actors use `Dynamic`: actor message dispatch (`dispatch/4`'s default
/// case) and `respondsTo:` (`beamtalk_primitive:responds_to/2`) already
/// delegate to a superclass via the live registry, not a compiled module
/// reference — a `Static` actor `has_method/1` would answer from the
/// module compiled at *this* class's compile time even after the named
/// ancestor is hot-reloaded (BT-845) to a different method set or
/// superclass, diverging from what `dispatch/4` (and a fresh `respondsTo:`
/// walk) would actually resolve — exactly the actor/dispatch mismatch
/// BT-3467 exists to eliminate, just reintroduced via hot reload instead of
/// a missing extension/superclass check.
pub(in crate::core_erlang) enum SuperclassDelegation<'a> {
    /// A compiled module name (e.g. `bt@stdlib@actor`).
    Static(&'a str),
    /// A class name (e.g. `Bt3467Base`), looked up in the live class
    /// registry at call time.
    Dynamic(&'a str),
}

/// The kind-specific inputs that drive `has_method/1` emission.
///
/// The emitted shape is always the same (selector-membership check, then
/// extension registry, then superclass delegation or catch-all-DNU
/// short-circuit); only these values differ between an actor class and a
/// value-type class.
pub(in crate::core_erlang) struct DispatchSpec<'a> {
    /// Selectors handled by hard-coded reflection dispatch arms (`class`,
    /// `respondsTo:`, `fieldNames`, …), as bare (unquoted) selector names.
    ///
    /// Empty for actors: per ADR 0006 Phase 1b, an actor's reflection
    /// selectors are inherited from Object via superclass delegation rather
    /// than listed locally.
    pub(in crate::core_erlang) reflection: &'a [&'static str],
    /// The class name for the `beamtalk_extensions:has/2` foreign-extension check.
    pub(in crate::core_erlang) class_name: &'a str,
    /// How to delegate `has_method/1` to the superclass when `Selector`
    /// isn't found locally or in the extension registry. `None` at the root
    /// of the hierarchy (no further delegation).
    pub(in crate::core_erlang) superclass: Option<SuperclassDelegation<'a>>,
    /// True when the class defines a catch-all `doesNotUnderstand:args:`
    /// handler (BT-1763) — such a class accepts every selector, so
    /// `has_method/1` short-circuits to `true` unconditionally.
    pub(in crate::core_erlang) dnu: bool,
    /// Auto-generated slot getter/`with*:` setter selectors (ADR 0042).
    /// Only ever populated for `Value subclass:` classes — always `None` for
    /// actors and `ClassKind::Object` classes.
    pub(in crate::core_erlang) auto_slots: Option<&'a AutoSlotMethods>,
}

/// Returns true if `class` defines `doesNotUnderstand:args:` with a
/// structural (unquoted) intrinsic body (BT-1763).
///
/// Such a definition acts as a catch-all DNU handler (e.g. `ErlangModule`,
/// `Erlang`) rather than the error-raising default in `ProtoObject`
/// (`@primitive "doesNotUnderstand:args:"`). Both the dispatch function and
/// the `has_method` function need this same predicate for both actor and
/// value-type classes, so it lives here to avoid drift between the sites.
pub(in crate::core_erlang) fn class_has_catch_all_dnu(class: &ClassDefinition) -> bool {
    class.methods.iter().any(|m| {
        m.selector.name() == "doesNotUnderstand:args:"
            && m.body.len() == 1
            && matches!(
                &m.body[0].expression,
                Expression::Primitive {
                    is_quoted: false,
                    ..
                }
            )
    })
}

/// Emits `has_method/1` from a [`DispatchSpec`] plus the class's own
/// locally-defined method selectors (already mangled, in declaration order).
///
/// # Generated Code
///
/// ```erlang
/// 'has_method'/1 = fun (Selector) ->
///     case call 'lists':'member'(Selector, ['class', 'respondsTo:', 'increment']) of
///         <'true'> when 'true' -> 'true'
///         <'false'> when 'true' ->
///             case call 'beamtalk_extensions':'has'('Counter', Selector) of
///                 <'true'> when 'true' -> 'true'
///                 <'false'> when 'true' -> call 'bt@stdlib@actor':'has_method'(Selector)
///             end
///     end
/// ```
///
/// A catch-all-DNU spec (`spec.dnu`) short-circuits to an unconditional
/// `'true'`, skipping the selector list and extension/superclass checks
/// entirely — such a class accepts every message.
pub(in crate::core_erlang) fn generate_has_method_from_spec(
    own_methods: &[String],
    spec: &DispatchSpec<'_>,
) -> Document<'static> {
    if spec.dnu {
        return docvec![
            "'has_method'/1 = fun (_Selector) ->\n",
            "    'true'\n",
            "\n",
        ];
    }

    let mut selectors: Vec<Document<'static>> =
        spec.reflection.iter().map(|s| leaf::atom(*s)).collect();
    for name in own_methods {
        selectors.push(leaf::atom(name.clone()));
    }
    if let Some(auto) = spec.auto_slots {
        for field in &auto.getters {
            selectors.push(leaf::atom(field.clone()));
        }
        for field in &auto.setters {
            selectors.push(leaf::atom(AutoSlotMethods::with_star_selector(field)));
        }
    }

    let false_branch: Document<'static> = match spec.superclass {
        Some(SuperclassDelegation::Static(super_mod)) => docvec![
            "<'false'> when 'true' -> call ",
            leaf::atom(super_mod.to_string()),
            ":'has_method'(Selector)\n",
        ],
        Some(SuperclassDelegation::Dynamic(super_class)) => docvec![
            "<'false'> when 'true' -> call 'beamtalk_dispatch':'responds_to'(Selector, ",
            leaf::atom(super_class.to_string()),
            ")\n",
        ],
        None => Document::Str("<'false'> when 'true' -> 'false'\n"),
    };

    docvec![
        "'has_method'/1 = fun (Selector) ->\n",
        "    case call 'lists':'member'(Selector, [",
        join(selectors, &Document::Str(", ")),
        "]) of\n",
        "        <'true'> when 'true' -> 'true'\n",
        "        <'false'> when 'true' ->\n",
        "            case call 'beamtalk_extensions':'has'(",
        leaf::atom(spec.class_name.to_string()),
        ", Selector) of\n",
        "                <'true'> when 'true' -> 'true'\n",
        "                ",
        false_branch,
        "            end\n",
        "    end\n",
        "\n",
    ]
}

#[cfg(test)]
mod tests;
