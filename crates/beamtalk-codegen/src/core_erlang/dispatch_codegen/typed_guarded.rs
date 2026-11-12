// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Typed and guarded sends.
//!
//! Lowers a send whose receiver has a statically-known class: a direct
//! `Module:dispatch/3` call (or a representation-routed built-in call) behind a
//! runtime class-tag or representation guard, with the dynamic send as the
//! fallback. Also holds the character-typed receiver handler and the
//! `StaysDynamic` reasons a send is left dynamic.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::expr_shape::is_character_typed_receiver;
use crate::core_erlang::routing;
use crate::core_erlang::{CodeGenError, CoreErlangGenerator, Result};

use super::HANDLERS;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{ClassKind, Expression, MessageSelector, WellKnownSelector};
use beamtalk_core::semantic_analysis::TypeProvenance;
use beamtalk_core::semantic_analysis::class_hierarchy::{ClassHierarchy, ClassInfo};

// BT-4069's exclusion of classes whose instances are never
// `'$beamtalk_class'`-tagged maps carrying their own name (raw Erlang terms,
// the untagged-map `Dictionary`, the `Class`/`Metaclass` records, the
// `ErlangModule` native proxy), and the exception-family root, live in the
// routing table generated from the shared fixture
// `runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json`
// (BT-4200, BT-4292): see `routing::is_non_tagged_map_class` and
// `routing::EXCEPTION_ROOT_CLASS`. The `EUnit`
// `beamtalk_tagged_map_routing_conformance_tests` checks the runtime routes
// each fixture row as the fixture says. `Future` is deliberately not a row
// (runtime-only, no compiled module — ruled out by the builtin/generated
// check in [`CoreErlangGenerator::typed_tagged_target`]), and neither
// is the rest of the `Exception` family (ruled out by a superclass-chain walk
// there, so future exception subclasses are covered without listing them).
// The fixture's supervisor-family rows are not non-tagged-map exclusions
// either: their subclasses share the `beamtalk_supervisor` tuple, so they are
// ruled out by a superclass-chain walk too ([`supervisor_family_root`]).
//
// BT-4294: a non-tagged-map row is not only an exclusion. When the fixture
// gives it a representation test and a single target module
// (`routing::builtin_send_route`), a typed send to it becomes a direct call
// guarded by that test ([`TypedSendTarget::Builtin`]) — for every selector
// outside the row's `special_selectors` (BT-4390).

/// BT-4292: why a send whose receiver did not resolve to a
/// [`TypedSendTarget`] stays dynamic — one variant per gate of
/// [`CoreErlangGenerator::typed_send_target`], so the dynamic-site
/// report (`BEAMTALK_CODEGEN_DIAGNOSTICS=1`) names the gate the send failed
/// rather than re-deriving it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum StaysDynamic {
    /// No trusted static type: none recorded, `Dynamic`, a negation, a
    /// metatype, or a single `Known` type of untrusted provenance
    /// (`Extracted`). A union has its own gates
    /// ([`Self::UnionMemberUntyped`], [`Self::UnionMember`],
    /// [`Self::UnionTooManyArms`]).
    Untyped,
    /// BT-4595: a union-typed receiver one of whose members has no
    /// `Known` class (`Dynamic`, a negation, a metatype, …); the string is
    /// the member's rendered type.
    UnionMemberUntyped(String),
    /// BT-4595: a union-typed receiver one of whose `Known` members has no
    /// guarded route; the whole union stays dynamic and the report names the
    /// member and its own gate.
    UnionMember {
        class: ecow::EcoString,
        why: Box<StaysDynamic>,
    },
    /// BT-4595: a union-typed receiver with more than [`MAX_PROTOCOL_ARMS`]
    /// distinct guard arms.
    UnionTooManyArms,
    /// A binary selector: lowered by the operator path, never a guarded call.
    BinarySelector,
    /// The `perform:` family, or a selector the dynamic entry point rewrites
    /// before classifying the receiver (`perform:withArguments:timeout:`,
    /// from the routing fixture): the dynamic send does not reach the
    /// receiver's `dispatch/3` with that selector (BT-4294).
    PerformFamily,
    /// A class the routing table says is not a tagged map of its own name
    /// and that has no guarded direct route, with the fixture's reason
    /// (BT-4294).
    RepresentationRouted(routing::NoBuiltinRoute),
    /// A runtime-only built-in with no compiled module (`Future`).
    RuntimeOnlyBuiltin,
    /// The class is not in the hierarchy this unit sees.
    UnknownClass,
    /// An `abstract` class: it has no instances of its own, so no value
    /// carries its tag and a guard on it could never hit (BT-4293).
    Abstract,
    /// An ancestor of a representation-routed built-in (`Object`, `Value`,
    /// `ProtoObject`, …): the static type admits integers, strings and the
    /// rest, which no class-tag guard matches, so a guard would almost never
    /// hit (BT-4293).
    RepresentationAncestor,
    /// `Supervisor`, `DynamicSupervisor` or a subclass: instances are
    /// `{beamtalk_supervisor, …}` tuples, never tagged maps (BT-4293).
    SupervisorFamily,
    /// The class name is declared by more than one package in scope
    /// (BT-4151).
    AmbiguousPackage,
    /// A module-less (`ClassBuilder`) class (BT-4149).
    ModuleLess,
    /// Not a `Value`/`Object` kind: an actor, whose instances are
    /// `#beamtalk_object{}` records.
    NotValueKind,
    /// The `Exception` family, routed to `bt@stdlib@exception:dispatch`.
    ExceptionFamily,
    /// An ancestor is declared in another file, so the chain is not fully
    /// known.
    CrossFileParent,
    /// A protocol-typed receiver none of whose known conformers is a
    /// guardable tagged class (BT-4486).
    ProtocolNoConformers,
    /// A protocol-typed receiver with more than [`MAX_PROTOCOL_ARMS`]
    /// guardable conformers: a guard that long costs more than it saves
    /// (BT-4486).
    ProtocolTooManyConformers,
}

/// BT-4486: the most conformer arms a protocol-typed send's class-tag guard
/// emits. Past this the send stays dynamic.
pub(in crate::core_erlang) const MAX_PROTOCOL_ARMS: usize = 4;

/// Whether `class` is `Exception` or one of its subclasses — instances route
/// to `bt@stdlib@exception:dispatch`, so a guarded direct call never applies.
/// The one rule behind [`StaysDynamic::ExceptionFamily`], shared with the
/// dynamic-site report (BT-4292).
pub(in crate::core_erlang) fn is_exception_family(hierarchy: &ClassHierarchy, class: &str) -> bool {
    class == routing::EXCEPTION_ROOT_CLASS
        || hierarchy
            .superclass_chain(class)
            .iter()
            .any(|ancestor| ancestor.as_str() == routing::EXCEPTION_ROOT_CLASS)
}

/// The routing-fixture class whose `beamtalk_supervisor` tuple represents
/// `class`'s instances — `class` itself or the nearest ancestor with a
/// supervisor row (`Supervisor`, `DynamicSupervisor`) — if any. The one rule
/// behind [`StaysDynamic::SupervisorFamily`], shared with the dynamic-site
/// report; the representation fact is read from the routing fixture, not
/// restated here (BT-4293).
pub(in crate::core_erlang) fn supervisor_family_root(
    hierarchy: &ClassHierarchy,
    class: &str,
) -> Option<ecow::EcoString> {
    std::iter::once(class.into())
        .chain(hierarchy.superclass_chain(class))
        .find(|ancestor: &ecow::EcoString| {
            routing::row(ancestor.as_str())
                .is_some_and(|row| row.representation == routing::Representation::SupervisorTuple)
        })
}

/// Whether `class` is a proper ancestor of a routing-fixture class whose
/// instances are not tagged maps of its own name (`Object` above `Symbol`,
/// `Value` above `Integer`, …). Such a class is not abstract, but a value of
/// its static type is almost never an instance carrying its own tag, so the
/// typed-tagged path leaves it dynamic ([`StaysDynamic::RepresentationAncestor`]).
/// Read from the routing fixture, so a new row needs no change here (BT-4293).
pub(in crate::core_erlang) fn is_representation_ancestor(
    hierarchy: &ClassHierarchy,
    class: &str,
) -> bool {
    routing::rows().iter().any(|row| {
        hierarchy
            .superclass_chain(row.class)
            .iter()
            .any(|ancestor| ancestor.as_str() == class)
    })
}

/// BT-4069: a send receiver's statically-known tagged-map dispatch target — the
/// class tag to guard on plus the module to direct-call. Resolved by
/// [`CoreErlangGenerator::typed_tagged_target`]; BT-4127 reuses the
/// same hook to inline `obj class` for typed receivers.
pub(in crate::core_erlang) struct TypedTaggedTarget {
    /// The class name: the `'$beamtalk_class'` tag the guard matches.
    pub(in crate::core_erlang) class_name: String,
    /// The class's compiled module: this unit's own registry entry when
    /// present (the module the unit actually emits — BT-4152), else the
    /// qualified derivation (registry under the referenced package, else
    /// the deterministic `bt@{package}@{snake}` composition).
    pub(in crate::core_erlang) module_name: String,
}

/// BT-4294: the guarded direct call a typed send compiles to — the one
/// decision [`CoreErlangGenerator::typed_send_target`] makes, shared by the
/// `typed_guarded` handler and the dynamic-site report.
pub(in crate::core_erlang) enum TypedSendTarget {
    /// A tagged-map class: guard on its `'$beamtalk_class'` tag (BT-4069).
    Tagged(TypedTaggedTarget),
    /// A representation-routed built-in: guard on the representation test
    /// the dynamic path routes on, call the module it would call.
    Builtin(routing::BuiltinRoute),
    /// BT-4486: a protocol-typed receiver (two to [`MAX_PROTOCOL_ARMS`]
    /// guardable conformers, sorted by class name): one tag arm per
    /// conformer. Protocols are structural, so the conformer list is only a
    /// heuristic; the guard is the contract — a value tagged with none of
    /// them takes the dynamic send. A protocol with exactly one conformer
    /// resolves to [`Self::Tagged`].
    Conformers(Vec<TypedTaggedTarget>),
    /// BT-4595: a union-typed receiver (two to [`MAX_PROTOCOL_ARMS`]
    /// distinct member classes, each resolving to [`Self::Tagged`] or
    /// [`Self::Builtin`]): one guard per member, chained so each miss falls
    /// to the next, the dynamic send last. Member guards do not overlap, so
    /// the order never changes meaning; a value matching no member takes the
    /// dynamic send exactly as before. Members of any provenance (including
    /// `-spec`-extracted) are accepted — the guard, not the annotation, is
    /// the contract (ADR 0025 amendment).
    Union(Vec<TypedSendTarget>),
}

impl CoreErlangGenerator {
    /// Character-typed-receiver dispatch —
    /// `HANDLERS`' first (and only) entry that ever fires on
    /// `is_character_typed_receiver` (a Character literal, a `Character
    /// value:` factory call, an `uppercase`/`lowercase` chain, or a
    /// statically Character-typed `ifTrue:ifFalse:` conditional). Collapses
    /// what used to be two separate
    /// `is_character_typed_receiver` checks in `generate_message_send` (one
    /// preempting [`Self::try_generate_protoobject_message`]'s `class` and
    /// [`Self::try_generate_object_message`]'s `respondsTo:`/`perform:`
    /// family for a Character receiver, one catching every other selector
    /// after the full non-character priority chain ran) into the one table
    /// entry `HANDLERS`' ordering now makes explicit:
    ///
    /// 1. `class`/`respondsTo:`/`perform:` family MUST be decided here,
    ///    before the rest of `HANDLERS` runs — `try_generate_protoobject_message`/
    ///    `try_generate_object_message` handle those selectors generically,
    ///    keyed on runtime `class_of/1` (which returns `'Integer'` for any
    ///    Character receiver, at the BEAM level a plain integer), for every
    ///    *other* receiver shape.
    /// 2. Any other selector runs the normal non-character priority chain
    ///    first (every other `HANDLERS` entry, skipping this one by name) —
    ///    e.g. `isNil`/`hash`/`error:` are receiver-agnostic Object-protocol
    ///    methods and must stay generic even for a Character receiver.
    /// 3. Only once nothing in that chain claims the selector does this
    ///    fall back to [`Self::generate_character_typed_dispatch`], instead
    ///    of [`Self::generate_runtime_dispatch`]'s `class_of/1`-keyed
    ///    fallback (which would treat the receiver as an `Integer`).
    pub(super) fn try_handle_character_typed_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if !is_character_typed_receiver(receiver) {
            return Ok(None);
        }

        match selector.well_known() {
            Some(WellKnownSelector::Class) => {
                // Hoist any side effects in the receiver expression
                // (none for a literal, but capture preserves the contract).
                let (preamble, _) = self.thread_subexprs(&[receiver], "CharCls")?;
                // Resolve to the Character class object so equality with
                // the `Character` class reference holds — `class_of_object`
                // for raw integer 65 would otherwise return Integer's
                // class object via `class_of/1 == 'Integer'`.
                let call_doc = Document::Str(
                    "call 'beamtalk_primitive':'class_of_object_by_name'('Character')",
                );
                return Ok(Some(self.close_prelude(&preamble, call_doc, "CharClsRes")));
            }
            Some(WellKnownSelector::RespondsTo) => {
                let exprs: [&Expression; 2] = [receiver, &arguments[0]];
                let mut seq = self.sequence_call(&exprs, "CharResp")?;
                let _recv = seq.next();
                let sel_doc = seq.next();
                let call_doc = docvec!["call 'bt@stdlib@character':'has_method'(", sel_doc, ")"];
                return Ok(Some(seq.close(self, call_doc, "CharRespRes")));
            }
            Some(
                WellKnownSelector::Perform
                | WellKnownSelector::PerformWithArgs
                | WellKnownSelector::PerformLocallyWithArgs,
            ) => {
                return Ok(Some(self.generate_character_typed_dispatch(
                    receiver, selector, arguments,
                )?));
            }
            _ => {}
        }

        // Skip by name, not position (`&HANDLERS[1..]`) — this entry's own
        // position in `HANDLERS` is exactly what's being looked up, so
        // slicing past index 0 would silently self-recurse if a reorder
        // ever moved `character_typed` elsewhere in the table.
        for (name, handler) in HANDLERS {
            if *name == "character_typed" {
                continue;
            }
            if let Some(doc) = handler(self, receiver, selector, arguments)? {
                return Ok(Some(doc));
            }
        }

        // at the BEAM level a Character value is a plain
        // integer, so the generic fallback (`generate_runtime_dispatch`,
        // keyed on runtime `class_of/1`) would route it to
        // `bt@stdlib@integer:dispatch/3`. Reach the Character module's
        // dispatch instead, e.g. for `$A asInteger`, `$A printString`, `$A
        // uppercase`, `(Character value: 10) asString`.
        Ok(Some(self.generate_character_typed_dispatch(
            receiver, selector, arguments,
        )?))
    }

    /// Routes a non-binary message to the Character module's
    /// `dispatch/3`, for any receiver `is_character_typed_receiver` recognizes
    /// (a Character literal, a `Character value:` factory call, an
    /// `uppercase`/`lowercase` chain, or a statically Character-typed
    /// `ifTrue:ifFalse:` conditional).
    ///
    /// At the BEAM level, Character values are plain integers, so the default
    /// runtime dispatch path (keyed on `is_integer/1`) sends them to the Integer
    /// module. This codegen specialization restores the static type by emitting
    /// a direct call to `bt@stdlib@character:dispatch/3` instead. The receiver
    /// expression itself is compiled normally (`capture_subexpr_sequence`), so
    /// this works whether the receiver is a bare literal or an arbitrary
    /// expression statically known to produce a Character.
    fn generate_character_typed_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();

        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        let mut seq = self.sequence_call(&all_exprs, "CharDisp")?;
        let actual_receiver = seq.next();
        let args_doc = Self::join_docs_with_commas(seq.rest());

        let call_doc = docvec![
            "call 'bt@stdlib@character':'dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            actual_receiver,
            ")"
        ];

        Ok(seq.close(self, call_doc, "CharDispRes"))
    }

    /// BT-4069, BT-4294: the receiver's trusted static class — the class
    /// the type checker's `TypeMap` records as `Known` for the receiver's
    /// span with a trusted provenance (`Declared`, `Inferred`, `Substituted`
    /// or `Aliased` — a dispatch-owned rule: xref wants recall while dispatch
    /// wants precision, so the two must not share one predicate) — or
    /// [`StaysDynamic::Untyped`].
    ///
    /// BT-4412: `Aliased` is trusted because the type checker attaches it
    /// only at the point a user-written annotation naming an alias is
    /// expanded (ADR 0108), so it is a declared type under another name.
    /// `Extracted` (`-spec`-derived) stays untrusted.
    pub(in crate::core_erlang) fn trusted_static_class(
        &self,
        receiver: &Expression,
    ) -> std::result::Result<&ecow::EcoString, StaysDynamic> {
        use beamtalk_core::semantic_analysis::InferredType;
        // A map shape (ADR 0147) is a `Dictionary` for every send that is not
        // shape-aware, so it dispatches statically as one.
        static DICTIONARY: ecow::EcoString = ecow::EcoString::inline("Dictionary");
        let (class_name, provenance) = match self
            .type_map
            .get(receiver.span())
            .ok_or(StaysDynamic::Untyped)?
        {
            InferredType::Known {
                class_name,
                provenance,
                ..
            } => (class_name, provenance),
            InferredType::Shape { provenance, .. } => (&DICTIONARY, provenance),
            _ => return Err(StaysDynamic::Untyped),
        };
        if !matches!(
            provenance,
            TypeProvenance::Declared(_)
                | TypeProvenance::Inferred(_)
                | TypeProvenance::Substituted(_)
                | TypeProvenance::Aliased { .. }
        ) {
            return Err(StaysDynamic::Untyped);
        }
        Ok(class_name)
    }

    /// BT-4069, BT-4293: resolves a receiver's trusted static class that is
    /// not a non-tagged routing-fixture row to a static tagged-map dispatch
    /// target, or the [`StaysDynamic`] gate it failed, unless every condition
    /// holds:
    ///
    /// 1. The receiver has a trusted static class
    ///    ([`Self::trusted_static_class`]).
    /// 2. The class is in the hierarchy and `Value`- or `Object`-kind (both
    ///    are `'$beamtalk_class'`-tagged maps with the same dynamic path).
    ///    It need **not** be sealed (BT-4293, ADR 0025 amendment §1): the tag
    ///    names the exact class, so a subclass instance simply misses the
    ///    guard and takes the dynamic fallback — `sealed` only changes how
    ///    often the guard hits, never what the send means.
    /// 3. Its instances really are tagged maps of its own name: not in the
    ///    routing table ([`routing::is_non_tagged_map_class`], decided by
    ///    [`Self::typed_send_target`] before this runs), not a
    ///    runtime-only builtin without a compiled module (`Future`), not
    ///    `abstract` (no instance carries an abstract class's tag:
    ///    `Collection`, `Number`, `Boolean`, …), not an ancestor of a
    ///    representation-routed built-in ([`is_representation_ancestor`]:
    ///    `Object`, `Value`, whose static types admit raw terms), not in the
    ///    supervisor family
    ///    ([`supervisor_family_root`]: `beamtalk_supervisor` tuples, read
    ///    from the routing fixture), and not the `Exception` family (whose
    ///    dynamic sends route via `bt@stdlib@exception:dispatch`, a different
    ///    entry point). The abstract, ancestor and supervisor gates are not
    ///    needed for soundness — a guard on them would (almost) never hit —
    ///    but they keep the emitted code free of dead guards.
    /// 4. Its ancestry is fully known (`has_cross_file_parent` fails closed —
    ///    a missing link could hide an `Exception` ancestor).
    /// 5. It has a compiled module (`is_module_less` fails closed — a live
    ///    `ClassBuilder` class dispatches through funs in its class process,
    ///    so a direct `Module:dispatch/3` would `undef`).
    /// 6. Its name is declared by only one package in scope (BT-4151). Two
    ///    packages' same-named classes are legal under qualified references
    ///    (`json@Parser` + `xml@Parser`) but collide into one last-wins
    ///    runtime class, so the tag guard cannot tell their instances apart:
    ///    a direct call would follow the compile-time package where dynamic
    ///    dispatch follows load order.
    ///
    /// Why this is sound (the guard, not the annotation, is trusted):
    ///
    /// - The guard verifies the runtime value's tag; a mismatch (violated
    ///   annotation, rebound parameter, `asType:` lie) falls back to a
    ///   byte-identical dynamic send, so no rebind analysis is needed.
    /// - A tag match means the dynamic path would route to the same module's
    ///   `dispatch/3` (`tagged_map_dispatch` → `tagged_class_dispatch`, whose
    ///   only divergences — proxy, exception entry, cross-package
    ///   shadowing — are excluded above; stdlib shadowing is a build error),
    ///   and `dispatch/3` preserves DNU, extensions, superclass
    ///   delegation and `perform:` identically — which is why no
    ///   method-resolution check is needed. (`fieldAt:put:` needs no
    ///   exclusion: its `immutable_value` hint is the same on every path,
    ///   BT-4273, and the object-reflection intrinsic claims it first.)
    /// - Every hierarchy entry that can reach here has a compiled module
    ///   exporting `dispatch/3`: batch hierarchies contain only compiled
    ///   classes, and live module-less (`ClassBuilder`) classes arriving via
    ///   a REPL or `compile:source:` ambient hierarchy are excluded by
    ///   condition 5 (only bare REPL *expressions* leave `type_map` empty;
    ///   inline class definitions and compiled methods populate it). The
    ///   remote `Mod:dispatch/3` call is late-bound, so hot reload needs no
    ///   invalidation — provided the module *name* is unchanged. A class
    ///   redefined into a different module is handled by pinning the unit's
    ///   own class to its emitted module (`register_own_class_module`,
    ///   BT-4200) and by deciding module-lessness from the live index first.
    ///
    /// Known limitation (BT-4151): condition 6 sees the scope its producer
    /// hands in. A batch build or `beamtalk test` passes every package's
    /// declarations; a REPL or `compile:source:` unit sees the live node,
    /// where a name already holds one (last-wins) class, so a same-named
    /// class from another package loaded *after* this unit compiled is not
    /// visible — the runtime's collision warning is then the only signal.
    ///
    /// Known limitation (review A3, BT-4149; shared with `class sealed` direct calls):
    /// the target module is pinned at compile time, so if it is removed
    /// afterwards (`removeFromSystem`) a tag-matching send raises raw `undef`
    /// where the dynamic path would degrade to `does_not_understand`.
    fn typed_tagged_target(
        &self,
        class_name: &ecow::EcoString,
    ) -> std::result::Result<TypedTaggedTarget, StaysDynamic> {
        let hierarchy = self
            .class_hierarchy
            .as_ref()
            .ok_or(StaysDynamic::UnknownClass)?;
        if ClassHierarchy::is_builtin_class(class_name)
            && !ClassHierarchy::is_generated_builtin_class(class_name)
        {
            return Err(StaysDynamic::RuntimeOnlyBuiltin);
        }
        let info = hierarchy
            .get_class(class_name)
            .ok_or(StaysDynamic::UnknownClass)?;
        if info.is_abstract {
            return Err(StaysDynamic::Abstract);
        }
        if is_representation_ancestor(hierarchy, class_name) {
            return Err(StaysDynamic::RepresentationAncestor);
        }
        if supervisor_family_root(hierarchy, class_name).is_some() {
            return Err(StaysDynamic::SupervisorFamily);
        }
        if self.is_shadowed_class(class_name) {
            return Err(StaysDynamic::AmbiguousPackage);
        }
        // BT-4200: one consistent source for the module-less gate. The live
        // class-module index (REPL: `build_class_module_index/0`, read from
        // the running class processes) is fresher than `info.is_module_less`,
        // which comes from compiler-server metadata that a `ClassBuilder`
        // re-registration over an existing compiled class never refreshes
        // (`update_class` finds no `__beamtalk_meta/0` to re-notify with).
        // So when the index has an entry, it decides (by the same rule the
        // meta decode applies, `registered_module_is_module_less`); metadata
        // is only the fallback for a class the index does not know.
        let indexed_module = self
            .class_module_registry()
            .module_for_class(&self.own_package_id(), class_name);
        let module_less = indexed_module.map_or(info.is_module_less, |module| {
            ClassInfo::registered_module_is_module_less(module.as_str(), class_name.as_str())
        });
        if module_less {
            return Err(StaysDynamic::ModuleLess);
        }
        if !matches!(
            hierarchy.resolve_class_kind(class_name),
            ClassKind::Value | ClassKind::Object
        ) {
            return Err(StaysDynamic::NotValueKind);
        }
        if is_exception_family(hierarchy, class_name) {
            return Err(StaysDynamic::ExceptionFamily);
        }
        if hierarchy.has_cross_file_parent(class_name) {
            return Err(StaysDynamic::CrossFileParent);
        }
        // BT-4152: resolve through this unit's own registry entry first —
        // it names the module the unit actually emits. `beamtalk test`
        // emits fixture/test modules unqualified (`bt@<stem>`, keyed under
        // this unit's own `PackageId`) while stamping the file's own class
        // with `package = Some(current_package)`; re-deriving from
        // `info.package` alone would target the never-emitted
        // `bt@<package>@<class>`. The qualified derivation stays as the
        // fallback for a packaged class with no index entry.
        //
        // BT-4293: the unit's own class comes first of all — its `dispatch/3`
        // is in the module this unit emits. `beamtalk test` emits a test
        // file's class unqualified with no index entry, so neither the
        // registry nor the qualified derivation names it (with the sealed
        // gate gone, every `TestCase` self-send reaches here).
        let module_name = self.own_class_module(class_name).unwrap_or_else(|| {
            self.class_module_registry()
                .module_for_class(&self.own_package_id(), class_name)
                .map_or_else(
                    || self.compiled_module_name_qualified(class_name, info.package.as_deref()),
                    |module| module.as_str().to_string(),
                )
        });
        Ok(TypedTaggedTarget {
            class_name: class_name.to_string(),
            module_name,
        })
    }

    /// BT-4292, BT-4294: the one decision whether a send becomes a guarded
    /// direct call, and to what — shared by the `typed_guarded` handler and
    /// the dynamic-site report so the report can never disagree with what was
    /// emitted.
    ///
    /// The selector gates come first. Binary operators never reach the
    /// dynamic fallthrough (preserving `generate_runtime_dispatch`'s internal
    /// error exactly). `fieldAt:put:` needs no gate: the object-reflection
    /// intrinsic always claims it, and its hint agrees on every path
    /// (BT-4273). The `perform:` family is claimed by intrinsics first too, and the
    /// fixture's rewritten selectors (`perform:withArguments:timeout:`) never
    /// reach a `dispatch/3` through the dynamic path, which turns them into a
    /// `send/4` of the target selector.
    ///
    /// Then the receiver's trusted static class decides the target. A
    /// routing-fixture row whose instances are not tagged maps of its own
    /// name ([`routing::is_non_tagged_map_class`]) takes the fixture's
    /// built-in route, or stays dynamic for the fixture's reason
    /// ([`routing::builtin_send_route`]); every other class is a tagged-map
    /// candidate ([`Self::typed_tagged_target`]).
    ///
    /// Why a built-in route is sound: its guard is the clause the dynamic
    /// path (`beamtalk_message_dispatch:send/3` → `beamtalk_primitive:send/3`
    /// → `module_for_value/2`, or `send_map/3` → `tagged_map_dispatch/1`)
    /// takes to reach `target_module:dispatch/3`, which the runtime half of
    /// the routing fixture checks; a value outside it (a lying annotation, a
    /// reserved tuple, `nil` typed as `Symbol`) misses and takes the dynamic
    /// send. A selector the row's dynamic path answers elsewhere (`Pid`'s
    /// future protocol, `String`'s shared byte-level selectors — the
    /// fixture's `special_selectors`, BT-4390) stays dynamic; every other
    /// selector on those rows is routed. A row whose test spans two routes
    /// (`Binary`: a bare binary or the `String` tag) gets both, guarded one
    /// after the other (BT-4510). Rows that carry a `dynamic_only` reason
    /// (`Boolean`, `Bitstring`, `Character`, the records, the proxy) stay
    /// dynamic. A stdlib built-in's module can be neither removed nor
    /// redefined into another module, so the pinned target never goes stale.
    pub(in crate::core_erlang) fn typed_send_target(
        &self,
        receiver: &Expression,
        selector: &MessageSelector,
    ) -> std::result::Result<TypedSendTarget, StaysDynamic> {
        if matches!(selector, MessageSelector::Binary(_)) {
            return Err(StaysDynamic::BinarySelector);
        }
        if let Some(
            WellKnownSelector::Perform
            | WellKnownSelector::PerformWithArgs
            | WellKnownSelector::PerformLocallyWithArgs,
        ) = selector.well_known()
        {
            return Err(StaysDynamic::PerformFamily);
        }
        if routing::rewritten_selectors().contains(&selector.name().as_str()) {
            return Err(StaysDynamic::PerformFamily);
        }
        if let Some(beamtalk_core::semantic_analysis::InferredType::Union { members, .. }) =
            self.type_map.get(receiver.span())
        {
            return self.union_send_target(members, selector);
        }
        let class_name = self.trusted_static_class(receiver)?;
        self.class_send_target(class_name, selector)
    }

    /// The guarded target for a receiver whose class is `class_name`: a
    /// representation-routed built-in, a protocol's conformers, or a tagged
    /// map class. Provenance is the caller's concern.
    fn class_send_target(
        &self,
        class_name: &ecow::EcoString,
        selector: &MessageSelector,
    ) -> std::result::Result<TypedSendTarget, StaysDynamic> {
        if routing::is_non_tagged_map_class(class_name.as_str()) {
            return match routing::builtin_send_route(class_name.as_str(), &selector.name()) {
                Some(Ok(route)) => Ok(TypedSendTarget::Builtin(route)),
                Some(Err(why)) => Err(StaysDynamic::RepresentationRouted(why)),
                None => Err(StaysDynamic::RepresentationRouted(
                    routing::NoBuiltinRoute::NoSingleRoute,
                )),
            };
        }
        if let Some(conformers) = self.protocol_conformers_of(class_name.as_str()) {
            return self.protocol_send_target(&conformers);
        }
        self.typed_tagged_target(class_name)
            .map(TypedSendTarget::Tagged)
    }

    /// BT-4595: the guarded target for a union-typed receiver. Members are
    /// deduped by class, each resolved through [`Self::class_send_target`]
    /// with any `Known` provenance accepted (see [`TypedSendTarget::Union`]);
    /// a member with no route keeps the whole union dynamic, and so does a
    /// chain of more than [`MAX_PROTOCOL_ARMS`] arms. A protocol member
    /// contributes its conformer arms.
    fn union_send_target(
        &self,
        members: &[beamtalk_core::semantic_analysis::InferredType],
        selector: &MessageSelector,
    ) -> std::result::Result<TypedSendTarget, StaysDynamic> {
        let mut seen: Vec<&ecow::EcoString> = Vec::new();
        let mut arms: Vec<TypedSendTarget> = Vec::new();
        for member in members {
            let beamtalk_core::semantic_analysis::InferredType::Known { class_name, .. } = member
            else {
                return Err(StaysDynamic::UnionMemberUntyped(
                    member.display_name().to_string(),
                ));
            };
            if seen.contains(&class_name) {
                continue;
            }
            seen.push(class_name);
            let target = self
                .class_send_target(class_name, selector)
                .map_err(|why| StaysDynamic::UnionMember {
                    class: class_name.clone(),
                    why: Box::new(why),
                })?;
            match target {
                TypedSendTarget::Conformers(tagged) => {
                    arms.extend(tagged.into_iter().map(TypedSendTarget::Tagged));
                }
                other => arms.push(other),
            }
        }
        // Distinct arms only: two members may share a conformer.
        let mut distinct: Vec<TypedSendTarget> = Vec::with_capacity(arms.len());
        for arm in arms {
            if let TypedSendTarget::Tagged(t) = &arm
                && distinct.iter().any(
                    |d| matches!(d, TypedSendTarget::Tagged(o) if o.class_name == t.class_name),
                )
            {
                continue;
            }
            distinct.push(arm);
        }
        match distinct.len() {
            0 => Err(StaysDynamic::Untyped),
            1 => Ok(distinct.remove(0)),
            n if n > MAX_PROTOCOL_ARMS => Err(StaysDynamic::UnionTooManyArms),
            _ => Ok(TypedSendTarget::Union(distinct)),
        }
    }

    /// BT-4486/BT-4630: the known conformers of `protocol` (non-protocol
    /// classes whose known surface satisfies it), sorted by name; `None` when
    /// `protocol` is not a protocol in scope. Structural, so a heuristic —
    /// the emitted tag guard is the contract. Computed on first use and
    /// memoised per protocol.
    pub(in crate::core_erlang) fn protocol_conformers_of(
        &self,
        protocol: &str,
    ) -> Option<std::rc::Rc<Vec<ecow::EcoString>>> {
        let registry = self.protocol_registry.as_ref()?;
        if !registry.has_protocol(protocol) {
            return None;
        }
        if let Some(hit) = self.protocol_conformers_memo.borrow().get(protocol) {
            return Some(std::rc::Rc::clone(hit));
        }
        let hierarchy = self.class_hierarchy.as_ref()?;
        let name = ecow::EcoString::from(protocol);
        let mut conformers: Vec<ecow::EcoString> = hierarchy
            .class_names()
            .filter(|class| {
                !hierarchy.is_protocol_class(class)
                    && registry.conforms_by_known_surface(class, &name, hierarchy)
            })
            .cloned()
            .collect();
        conformers.sort();
        let conformers = std::rc::Rc::new(conformers);
        self.protocol_conformers_memo
            .borrow_mut()
            .insert(name, std::rc::Rc::clone(&conformers));
        Some(conformers)
    }

    /// BT-4486: the guarded target for a receiver typed by a protocol —
    /// each known conformer through [`Self::typed_tagged_target`] (so the
    /// shadowing, module-less, exception, abstract and built-in exclusions
    /// apply per arm), keeping those that resolve.
    fn protocol_send_target(
        &self,
        conformers: &[ecow::EcoString],
    ) -> std::result::Result<TypedSendTarget, StaysDynamic> {
        let mut targets: Vec<TypedTaggedTarget> = conformers
            .iter()
            .filter_map(|class| self.typed_tagged_target(class).ok())
            .collect();
        match targets.len() {
            0 => Err(StaysDynamic::ProtocolNoConformers),
            1 => Ok(TypedSendTarget::Tagged(targets.remove(0))),
            n if n > MAX_PROTOCOL_ARMS => Err(StaysDynamic::ProtocolTooManyConformers),
            _ => Ok(TypedSendTarget::Conformers(targets)),
        }
    }

    /// BT-4069: `HANDLERS`' last entry. Converts a send to a receiver
    /// statically typed as a tagged-map class (sealed or open, BT-4293) or a
    /// representation-routed built-in (BT-4294) into a guarded direct
    /// `Module:dispatch/3` call ([`Self::typed_send_target`] decides). Only
    /// fires when nothing earlier claimed the send, so it purely converts
    /// would-be-dynamic sends.
    pub(super) fn try_handle_typed_guarded_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        let Ok(target) = self.typed_send_target(receiver, selector) else {
            return Ok(None);
        };
        Ok(Some(self.generate_typed_guarded_send(
            receiver, selector, arguments, &target,
        )?))
    }

    /// BT-4069, BT-4294: emits the guarded direct call for a
    /// [`TypedSendTarget`]. A built-in route's guard is its representation
    /// test, rendered by the type-pattern renderer of its fixture kind
    /// (`render_send_guard`), with the dynamic send in every miss arm. A
    /// [`TypedTaggedTarget`]'s guard is an `is_map` + `maps:get/3`-default case nest
    /// (the `match_lowering` idiom — `maps:get/3` never raises, so no guard
    /// position is needed) selecting between the direct `Module:dispatch/3`
    /// call and a dynamic-send fallback in each mismatch arm:
    ///
    /// ```erlang
    /// case call 'erlang':'is_map'(R) of
    ///     <'true'> when 'true' ->
    ///         case call 'maps':'get'('$beamtalk_class', R, 'undefined') of
    ///             <'SealedWidget'> when 'true' ->
    ///                 call 'bt@test@sealed_widget':'dispatch'('x', [], R)
    ///             <NoMatch> when 'true' ->
    ///                 call 'beamtalk_message_dispatch':'send'(R, 'x', [])
    ///         end
    ///     <'false'> when 'true' ->
    ///         call 'beamtalk_message_dispatch':'send'(R, 'x', [])
    ///     <NoMatch> when 'true' -> call 'erlang':'error'({'case_clause', NoMatch})
    /// end
    /// ```
    ///
    /// Receiver and arguments are sequenced once in evaluation order, then
    /// bound to fresh temporaries (mirroring `guarded_op_doc`): each operand
    /// is referenced up to four times below, so unlike the single-use dynamic
    /// send they cannot stay inline. The outer boolean case carries an
    /// explicit `case_clause_fallback` like `guarded_op_doc`'s guards; the
    /// inner case is statically exhaustive via its wildcard arm.
    pub(super) fn generate_typed_guarded_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        target: &TypedSendTarget,
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();

        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        all_exprs.extend(arguments.iter());
        // BT-4633: no `^`-closure pre-store here either; the runtime
        // dispatch's argument holds (`generate_runtime_dispatch`), and the
        // same test-build log checks it. A `no_user_code` send to a receiver
        // of statically known class reaches this guarded send, not that one.
        #[cfg(test)]
        self.note_dispatch_nlr_literals(receiver, selector, arguments);
        let (preamble, mut docs) = self.thread_subexprs(&all_exprs, "TTagged")?;
        let mut docs_iter = docs.drain(..);
        let receiver_doc = docs_iter.next().ok_or_else(|| {
            CodeGenError::Internal("thread_subexprs dropped the receiver".to_string())
        })?;
        let arg_docs: Vec<Document<'static>> = docs_iter.collect();

        let receiver_var = self.fresh_temp_var("TTaggedRecv");
        let arg_vars: Vec<String> = arg_docs
            .iter()
            .map(|_| self.fresh_temp_var("TTaggedArg"))
            .collect();
        let case_doc = self.guarded_send_on_vars(target, &selector_atom, &receiver_var, &arg_vars);

        // BT-4279: a unary send that may be an auto-generated slot getter of a
        // tagged Value class. Its fast path reads the slot with no bracket
        // when the receiver is tagged exactly that class and the loaded
        // module's `__is_slot/1` says the selector is still a plain slot read
        // (a reload that turns it into a method answers `false`).
        if let TypedSendTarget::Tagged(tagged) = target
            && arguments.is_empty()
            && preamble.is_empty()
            && self.class_declares_value_slot(&tagged.class_name, &selector_atom)
        {
            let test = Self::slot_read_test_doc(&receiver_var, tagged, &selector_atom);
            let fast = docvec![
                "call 'maps':'get'(",
                leaf::atom(selector_atom.clone()),
                ", ",
                leaf::var(receiver_var.clone()),
                ")",
            ];
            let bindings = docvec![
                "let ",
                leaf::var(receiver_var.clone()),
                " = ",
                receiver_doc.clone(),
                " in ",
            ];
            self.offer_fast_path(receiver, bindings, test, fast, case_doc.clone());
        }

        let mut body = case_doc;
        for (var, doc) in arg_vars.into_iter().zip(arg_docs).rev() {
            body = docvec!["let ", leaf::var(var), " = ", doc, " in ", body];
        }
        body = docvec![
            "let ",
            leaf::var(receiver_var),
            " = ",
            receiver_doc,
            " in ",
            body
        ];
        Ok(self.close_prelude(&preamble, body, "TTaggedRes"))
    }

    /// BT-4069, BT-4294, BT-4510: the guarded direct call of `selector_atom`
    /// to the value bound to `receiver_var`, with `arg_vars` as arguments,
    /// for a [`TypedSendTarget`]: the target's guard (a class tag, or a
    /// built-in's representation test, then its `String`-tag half if any)
    /// around the direct call, and the dynamic send in every miss arm. Shared
    /// by [`Self::generate_typed_guarded_send`] and the sends the compiler
    /// makes itself on an already-bound value (a string interpolation
    /// segment's `displayString`).
    pub(in crate::core_erlang) fn guarded_send_on_vars(
        &mut self,
        target: &TypedSendTarget,
        selector_atom: &str,
        receiver_var: &str,
        arg_vars: &[String],
    ) -> Document<'static> {
        let args_list_doc = Self::join_docs_with_commas(
            arg_vars.iter().map(|var| leaf::var(var.clone())).collect(),
        );
        let fallback_doc = self.annotate_current_send_call(docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            leaf::var(receiver_var.to_string()),
            ", ",
            leaf::atom(selector_atom.to_string()),
            ", [",
            args_list_doc.clone(),
            "])",
        ]);
        self.guard_around(
            target,
            selector_atom,
            receiver_var,
            arg_vars,
            &args_list_doc,
            fallback_doc,
        )
    }

    /// The guard(s) of one [`TypedSendTarget`] around its direct call(s),
    /// with `miss` (the dynamic send, or the next link of a union chain) in
    /// every miss arm.
    fn guard_around(
        &mut self,
        target: &TypedSendTarget,
        selector_atom: &str,
        receiver_var: &str,
        arg_vars: &[String],
        args_list_doc: &Document<'static>,
        miss: Document<'static>,
    ) -> Document<'static> {
        let fallback_doc = miss;
        match target {
            TypedSendTarget::Union(members) => self.union_chain_doc(
                members,
                selector_atom,
                receiver_var,
                arg_vars,
                args_list_doc,
                fallback_doc,
            ),
            TypedSendTarget::Conformers(conformers) => {
                // BT-4486: one tag arm per conformer, each a direct call.
                let arms: Vec<(String, Document<'static>)> = conformers
                    .iter()
                    .map(|tagged| {
                        let call = self.tagged_direct_call(
                            tagged,
                            selector_atom,
                            receiver_var,
                            arg_vars,
                            args_list_doc,
                        );
                        (
                            tagged.class_name.clone(),
                            self.annotate_current_send_call(call),
                        )
                    })
                    .collect();
                self.class_tags_guarded_doc(receiver_var, arms, fallback_doc)
            }
            TypedSendTarget::Tagged(tagged) => {
                let call = self.tagged_direct_call(
                    tagged,
                    selector_atom,
                    receiver_var,
                    arg_vars,
                    args_list_doc,
                );
                // BT-4366: both calls sit inside the type-test `case`, so each
                // carries the send's line itself.
                let direct_doc = self.annotate_current_send_call(call);
                self.class_tag_guarded_doc(
                    receiver_var,
                    &tagged.class_name,
                    direct_doc,
                    fallback_doc,
                )
            }
            TypedSendTarget::Builtin(route) => {
                let direct_doc = self.annotate_current_send_call(Self::dispatch3_call_doc(
                    route.target_module.to_string(),
                    selector_atom,
                    args_list_doc,
                    receiver_var,
                ));
                // BT-4510: a `Binary` receiver that is a `String` at run time
                // misses the bare-binary guard and tries the `String` route
                // before the dynamic send.
                let miss_doc = match route.string_tag_route {
                    Some(string_route) => {
                        let string_call =
                            self.annotate_current_send_call(Self::dispatch3_call_doc(
                                string_route.target_module.to_string(),
                                selector_atom,
                                args_list_doc,
                                receiver_var,
                            ));
                        self.render_send_guard(
                            string_route.guard,
                            receiver_var,
                            string_call,
                            &fallback_doc,
                        )
                    }
                    None => fallback_doc,
                };
                self.render_send_guard(route.guard, receiver_var, direct_doc, &miss_doc)
            }
        }
    }

    /// BT-4595: the guard chain of a [`TypedSendTarget::Union`].
    fn union_chain_doc(
        &mut self,
        members: &[TypedSendTarget],
        selector_atom: &str,
        receiver_var: &str,
        arg_vars: &[String],
        args_list_doc: &Document<'static>,
        fallback_doc: Document<'static>,
    ) -> Document<'static> {
        // BT-4595: tag arms share one `select_val` case; each
        // built-in guard wraps it, built right to left so the
        // dynamic send is the last miss. Guards do not overlap, so
        // grouping the tag arms changes no meaning.
        let (tagged, others): (Vec<&TypedSendTarget>, Vec<&TypedSendTarget>) = members
            .iter()
            .partition(|m| matches!(m, TypedSendTarget::Tagged(_)));
        // Each guard renderer repeats its miss document in every miss arm, so
        // a chain of built-in guards would multiply the dynamic send. Bind it
        // once as a nullary fun and have every miss arm apply it.
        let miss_fun = (!others.is_empty()).then(|| self.fresh_temp_var("TUnionMiss"));
        let mut chain = match &miss_fun {
            Some(name) => {
                self.annotate_current_send_call(docvec!["apply ", leaf::var(name.clone()), "()"])
            }
            None => fallback_doc.clone(),
        };
        if !tagged.is_empty() {
            let arms: Vec<(String, Document<'static>)> = tagged
                .iter()
                .filter_map(|m| match m {
                    TypedSendTarget::Tagged(t) => Some(t),
                    _ => None,
                })
                .map(|t| {
                    let call = self.tagged_direct_call(
                        t,
                        selector_atom,
                        receiver_var,
                        arg_vars,
                        args_list_doc,
                    );
                    (t.class_name.clone(), self.annotate_current_send_call(call))
                })
                .collect();
            chain = self.class_tags_guarded_doc(receiver_var, arms, chain);
        }
        for member in others.into_iter().rev() {
            chain = self.guard_around(
                member,
                selector_atom,
                receiver_var,
                arg_vars,
                args_list_doc,
                chain,
            );
        }
        match miss_fun {
            Some(name) => docvec![
                "let ",
                leaf::var(name),
                " = fun () -> ",
                fallback_doc,
                " in ",
                chain
            ],
            None => chain,
        }
    }

    /// BT-4473: the direct call for a receiver known to be tagged `tagged`:
    /// the own-class method arm when this unit's own class answers the
    /// selector, else the class module's `dispatch/3`.
    fn tagged_direct_call(
        &self,
        tagged: &TypedTaggedTarget,
        selector_atom: &str,
        receiver_var: &str,
        arg_vars: &[String],
        args_list_doc: &Document<'static>,
    ) -> Document<'static> {
        match self.own_dispatch_arm(&tagged.class_name, selector_atom, arg_vars.len()) {
            Some(arm) => Self::own_arm_call_doc(selector_atom, arm, receiver_var, arg_vars),
            None => Self::dispatch3_call_doc(
                tagged.module_name.clone(),
                selector_atom,
                args_list_doc,
                receiver_var,
            ),
        }
    }

    /// `call 'Module':'dispatch'('sel', [Args], Recv)` — the remote
    /// `dispatch/3` call every guarded direct send makes (BT-4510).
    fn dispatch3_call_doc(
        module_name: String,
        selector_atom: &str,
        args_list_doc: &Document<'static>,
        receiver_var: &str,
    ) -> Document<'static> {
        docvec![
            "call ",
            leaf::atom(module_name),
            ":'dispatch'(",
            leaf::atom(selector_atom.to_string()),
            ", [",
            args_list_doc.clone(),
            "], ",
            leaf::var(receiver_var.to_string()),
            ")",
        ]
    }

    /// BT-4473: the own-class `dispatch/3` method arm that answers
    /// `selector` with `arity` arguments for an instance of `class_name`, when
    /// `class_name` is the class this unit compiles and the code being
    /// generated is that class's own (not an extension body re-scoped to
    /// another class).
    ///
    /// Why calling the arm's function is the send: behind the class-tag guard
    /// the receiver is an instance of exactly this class, so the dynamic send
    /// reaches this module's `dispatch/3` (see [`Self::typed_tagged_target`]),
    /// which answers the selector from that arm before its default arm — so
    /// no extension or superclass is consulted, and nothing installed later
    /// can change that. The function is called locally, so caller and callee
    /// are always the same module version and the arm this unit compiled
    /// against is the one it calls. One difference from
    /// `dispatch/3`, whose arms are remote calls: code still running in the
    /// old version after a reload (a long loop, an old block) keeps calling
    /// the old method, where `dispatch/3` would have reached the new one.
    /// Calls made after the reload, from new code, reach the new method.
    fn own_dispatch_arm(
        &self,
        class_name: &str,
        selector: &str,
        arity: usize,
    ) -> Option<crate::core_erlang::value_type_codegen::OwnDispatchArm> {
        self.own_class_module(class_name)?;
        if self.class_name() != class_name {
            return None;
        }
        self.class_context
            .as_ref()?
            .own_dispatch_arms
            .get(selector)
            .copied()
            .filter(|arm| arm.arity == arity)
    }

    /// BT-4473: the local call of an own-class method arm's function —
    /// `apply 'sel'/N(R, A1, …)`, which answers the send's result (BT-4503).
    fn own_arm_call_doc(
        selector: &str,
        arm: crate::core_erlang::value_type_codegen::OwnDispatchArm,
        receiver_var: &str,
        arg_vars: &[String],
    ) -> Document<'static> {
        let mut call_args: Vec<Document<'static>> = vec![leaf::var(receiver_var.to_string())];
        call_args.extend(arg_vars.iter().map(|var| leaf::var(var.clone())));
        docvec![
            "apply ",
            leaf::fname(selector.to_string(), arm.arity + 1),
            "(",
            Self::join_docs_with_commas(call_args),
            ")",
        ]
    }

    /// BT-4279: whether `class_name` is a Value class that declares a field
    /// named `selector`, so a unary send of it may reach the class's
    /// auto-generated getter (whether it does is the loaded module's
    /// `__is_slot/1` answer at run time).
    fn class_declares_value_slot(&self, class_name: &str, selector: &str) -> bool {
        self.class_hierarchy.as_ref().is_some_and(|h| {
            h.get_class(class_name)
                .is_some_and(|c| c.is_value && c.state.iter().any(|f| f.as_str() == selector))
        })
    }

    /// BT-4279: the slot-read fast path's test: the receiver is a map tagged
    /// exactly the target class, and that class's loaded module answers
    /// `true` from `__is_slot/1` for the selector. A module compiled before
    /// `__is_slot/1` existed answers `false` (the export check).
    fn slot_read_test_doc(
        receiver_var: &str,
        tagged: &TypedTaggedTarget,
        selector: &str,
    ) -> Document<'static> {
        let module = leaf::atom(tagged.module_name.clone());
        docvec![
            "case call 'erlang':'is_map'(",
            leaf::var(receiver_var.to_string()),
            ") of <'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            leaf::var(receiver_var.to_string()),
            ", 'undefined') of <",
            leaf::atom(tagged.class_name.clone()),
            "> when 'true' -> ",
            "case call 'erlang':'function_exported'(",
            module.clone(),
            ", '__is_slot', 1) of <'true'> when 'true' -> call ",
            module,
            ":'__is_slot'(",
            leaf::atom(selector.to_string()),
            ") <_> when 'true' -> 'false' end ",
            "<_> when 'true' -> 'false' end ",
            "<_> when 'true' -> 'false' end",
        ]
    }

    /// The tagged-map guard of [`Self::generate_typed_guarded_send`]:
    /// `direct_doc` when `receiver_var` is a map tagged exactly `class_name`,
    /// `fallback_doc` otherwise.
    fn class_tag_guarded_doc(
        &mut self,
        receiver_var: &str,
        class_name: &str,
        direct_doc: Document<'static>,
        fallback_doc: Document<'static>,
    ) -> Document<'static> {
        self.class_tags_guarded_doc(
            receiver_var,
            vec![(class_name.to_string(), direct_doc)],
            fallback_doc,
        )
    }

    /// BT-4486: [`Self::class_tag_guarded_doc`] over several classes — one
    /// atom clause per `(class, direct_doc)` arm in the inner `case` (a
    /// `select_val`), `fallback_doc` for any other tag or a non-map.
    fn class_tags_guarded_doc(
        &mut self,
        receiver_var: &str,
        arms: Vec<(String, Document<'static>)>,
        fallback_doc: Document<'static>,
    ) -> Document<'static> {
        let no_match_inner = self.fresh_temp_var("TTaggedNoMatch");
        let no_match_outer = self.case_clause_fallback("TTaggedOuterNoMatch");
        let arm_docs: Vec<Document<'static>> = arms
            .into_iter()
            .map(|(class_name, direct_doc)| {
                docvec![
                    "<",
                    leaf::atom(class_name),
                    "> when 'true' -> ",
                    direct_doc,
                    " "
                ]
            })
            .collect();
        docvec![
            "case call 'erlang':'is_map'(",
            leaf::var(receiver_var.to_string()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            leaf::var(receiver_var.to_string()),
            ", 'undefined') of ",
            Document::Vec(arm_docs),
            "<",
            leaf::var(no_match_inner),
            "> when 'true' -> ",
            fallback_doc.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            fallback_doc,
            no_match_outer,
            " end",
        ]
    }

    /// `call 'beamtalk_primitive':'class_of_object'(Receiver)`: the dynamic
    /// `obj class` — the protoobject `class` arm's plain lowering and the
    /// miss arm of [`Self::generate_typed_class_of`].
    pub(in crate::core_erlang) fn class_of_object_call_doc(
        receiver_doc: Document<'static>,
    ) -> Document<'static> {
        docvec![
            "call 'beamtalk_primitive':'class_of_object'(",
            receiver_doc,
            ")",
        ]
    }

    /// BT-4127: `obj class` for a receiver typed as a tagged-map class
    /// ([`TypedSendTarget::Tagged`], decided by [`Self::typed_send_target`]):
    /// the class object built inline behind [`Self::class_tag_guarded_doc`]'s
    /// tag guard, instead of `class_of_object/1`'s derived-cache lookup.
    ///
    /// ```erlang
    /// let R = <receiver> in
    /// case call 'erlang':'is_map'(R) of
    ///     <'true'> when 'true' ->
    ///         case call 'maps':'get'('$beamtalk_class', R, 'undefined') of
    ///             <'Pt'> when 'true' ->
    ///                 case call 'erlang':'whereis'('beamtalk_class_Pt') of
    ///                     <'undefined'> when 'true' ->
    ///                         call 'beamtalk_primitive':'class_of_object'(R)
    ///                     <Pid> when 'true' ->
    ///                         {'beamtalk_object', 'Pt class', 'bt@pt', Pid}
    ///                 end
    ///             <NoMatch> when 'true' -> call 'beamtalk_primitive':'class_of_object'(R)
    ///         end
    ///     <'false'> when 'true' -> call 'beamtalk_primitive':'class_of_object'(R)
    ///     ...
    /// end
    /// ```
    ///
    /// On a tag match this is the tuple `class_of_object_by_name/1` derives
    /// (`resolve_class_object/1`): the tag is `metaclass_tag` of the class
    /// name (pinned to `class_object_tag/1` by the shared conformance
    /// fixture), the pid is looked up live on every evaluation (a restarted
    /// class process is found), and the module is the compile-time pin of
    /// BT-4069's direct `dispatch/3` with the same trust model (see
    /// [`Self::typed_tagged_target`]): a same-module redefinition or reload
    /// keeps it current. A removed class (`whereis` answers `'undefined'`)
    /// and every guard miss take the dynamic call, so a lying annotation, a
    /// subclass instance or a future changes nothing.
    ///
    /// Known limitation (BT-4200, shared with the direct `dispatch/3`): a class
    /// redefined into a *different* module after this unit compiled leaves
    /// the pin stale, and the answer then differs from the class reference
    /// in its module field until the caller is recompiled.
    pub(in crate::core_erlang) fn generate_typed_class_of(
        &mut self,
        receiver: &Expression,
        target: &TypedTaggedTarget,
    ) -> Result<Document<'static>> {
        let mut seq = self.sequence_call(&[receiver], "Obj")?;
        let receiver_doc = seq.next();
        let receiver_var = self.fresh_temp_var("TClassRecv");
        let pid_var = self.fresh_temp_var("TClassPid");
        let fallback_doc = Self::class_of_object_call_doc(leaf::var(receiver_var.clone()));
        let class_object_doc = docvec![
            "case ",
            Self::registered_class_pid_doc(&target.class_name),
            " of <'undefined'> when 'true' -> ",
            fallback_doc.clone(),
            " <",
            leaf::var(pid_var.clone()),
            "> when 'true' -> ",
            Self::class_object_tuple_doc(
                &target.class_name,
                leaf::atom(target.module_name.clone()),
                leaf::var(pid_var),
            ),
            " end",
        ];
        let case_doc = self.class_tag_guarded_doc(
            &receiver_var,
            &target.class_name,
            class_object_doc,
            fallback_doc,
        );
        let body = docvec![
            "let ",
            leaf::var(receiver_var),
            " = ",
            receiver_doc,
            " in ",
            case_doc
        ];
        Ok(seq.close(self, body, "ClassRes"))
    }
}
