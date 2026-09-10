// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `DispatchSpec` — the shared `has_method/1` emitter for actor and
//! value-type classes (ADR 0006).
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
/// (byte-for-byte the prior hand-written output).
///
/// Actors use `Dynamic`: actor message dispatch (`dispatch/4`'s default
/// case) and `respondsTo:` (`beamtalk_primitive:responds_to/2`) already
/// delegate to a superclass via the live registry, not a compiled module
/// reference — a `Static` actor `has_method/1` would answer from the
/// module compiled at *this* class's compile time even after the named
/// ancestor is hot-reloaded to a different method set or
/// superclass, diverging from what `dispatch/4` (and a fresh `respondsTo:`
/// walk) would actually resolve — exactly the actor/dispatch mismatch
/// this module exists to eliminate, just reintroduced via hot reload
/// instead of a missing extension/superclass check.
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
    /// handler — such a class accepts every selector, so
    /// `has_method/1` short-circuits to `true` unconditionally.
    pub(in crate::core_erlang) dnu: bool,
    /// Auto-generated slot getter/`with*:` setter selectors (ADR 0042).
    /// Only ever populated for `Value subclass:` classes — always `None` for
    /// actors and `ClassKind::Object` classes.
    pub(in crate::core_erlang) auto_slots: Option<&'a AutoSlotMethods>,
    /// Also emit a strictly-local `has_method_local/1` alongside
    /// `has_method/1` — same own-methods-or-extension check, but the false
    /// branch always answers `'false'`, regardless of `superclass`, instead
    /// of delegating.
    ///
    /// `beamtalk_dispatch.erl`'s `class_chain_step/6` (the per-node probe
    /// behind `lookup_in_class_chain/5`) relies on a class's `has_method/1`
    /// answering "does *this exact class* define `Selector`", advancing to
    /// the superclass itself on `false` — its own node-by-node walk is
    /// already the hierarchy traversal. `has_method/1`'s
    /// [`SuperclassDelegation::Dynamic`] breaks that contract: it answers
    /// `true` as soon as *any* ancestor has `Selector`, so `class_chain_step`
    /// stops at the wrong node and `invoke_method` redispatches one level at
    /// a time via `super/5`, each hop repeating the same dynamic walk —
    /// turning an O(depth) walk into O(depth²). `has_method_local/1` gives
    /// `class_chain_step` a probe that matches its contract again.
    ///
    /// Only actor classes set this (`gen_server::dispatch::generate_has_method`,
    /// the only [`SuperclassDelegation::Dynamic`] call site) — value-type
    /// `has_method/1` uses [`SuperclassDelegation::Static`], a
    /// compile-time-resolved direct module call with no live registry walk,
    /// so it doesn't compound the same way.
    pub(in crate::core_erlang) emit_local_probe: bool,
}

/// Returns true if `class` defines `doesNotUnderstand:args:` with a
/// structural (unquoted) intrinsic body.
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
        let has_method = docvec![
            "'has_method'/1 = fun (_Selector) ->\n",
            "    'true'\n",
            "\n",
        ];
        if !spec.emit_local_probe {
            return has_method;
        }
        // A catch-all-DNU class handles every selector at its own dispatch/4
        // (its `doesNotUnderstand:args:` intrinsic), so it's a local `true`
        // too — class_chain_step should stop here, not advance further.
        let has_method_local = docvec![
            "'has_method_local'/1 = fun (_Selector) ->\n",
            "    'true'\n",
            "\n",
        ];
        return docvec![has_method, has_method_local];
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

    let has_method = render_has_method_fn("has_method", &selectors, spec.class_name, false_branch);

    if !spec.emit_local_probe {
        return has_method;
    }

    // The local-only variant never delegates on a false membership
    // check — it always answers `'false'`, so class_chain_step's own walk
    // (not this function's) is what advances to the superclass.
    let local_false_branch = Document::Str("<'false'> when 'true' -> 'false'\n");
    let has_method_local = render_has_method_fn(
        "has_method_local",
        &selectors,
        spec.class_name,
        local_false_branch,
    );

    docvec![has_method, has_method_local]
}

/// Shared body renderer for `has_method/1` and `has_method_local/1`
/// — both check `Selector` against `selectors` then the extension registry;
/// they differ only in `fn_name` and what happens when neither matches.
fn render_has_method_fn(
    fn_name: &'static str,
    selectors: &[Document<'static>],
    class_name: &str,
    false_branch: Document<'static>,
) -> Document<'static> {
    docvec![
        leaf::fname(fn_name, 1),
        " = fun (Selector) ->\n",
        "    case call 'lists':'member'(Selector, [",
        join(selectors.to_vec(), &Document::Str(", ")),
        "]) of\n",
        "        <'true'> when 'true' -> 'true'\n",
        "        <'false'> when 'true' ->\n",
        "            case call 'beamtalk_extensions':'has'(",
        leaf::atom(class_name.to_string()),
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
