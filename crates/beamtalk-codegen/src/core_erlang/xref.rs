// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `method_xref` / `state_var_xref` construction for `register_class/0`.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ADR 0087 send/reference indexing for the VS Code Workspace Explorer
//! sidebar and `referencesTo:`/xref queries, plus the ADR 0115 Phase 2
//! `recv_type` write-path projection ([`RecvType`], [`project_recv_type`])
//! that classifies a send receiver's inferred type into the vocabulary the
//! xref read path expects.
//!
//! Kind-agnostic: called for actor, value-type, and native-facade classes
//! alike — split out of `gen_server/methods.rs`, which was actor-specific
//! in name but carried this shared logic.

use super::CoreErlangGenerator;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};
use beamtalk_core::ast::{ClassDefinition, MethodDefinition};
use beamtalk_core::semantic_analysis::{InferredType, TypeProvenance};
use ecow::EcoString;

/// Erlang's hard cap on atom size (ERTS source: `MAX_ATOM_CHARACTERS` / UTF-8
/// bytes).  Selectors or class names that exceed this limit cannot be emitted
/// as legal Core Erlang atoms and are dropped from xref / dispatch tables.
pub(in crate::core_erlang) const MAX_ATOM_BYTES: usize = 255;

/// BT-3217 (ADR 0115 Phase 2): the xref `recv_type` write-path vocabulary a
/// message send's receiver `InferredType` projects onto — see
/// [`project_recv_type`] and `build_method_xref_entry`'s doc for the full
/// rule, and this PR's description for the `Meta{C}` decision.
enum RecvType {
    /// A concrete class-or-protocol name — `Known` resolving to exactly one
    /// name whose provenance the read path can trust (`Declared`,
    /// `Inferred`, or `Substituted`; ADR 0068's protocol names resolve
    /// through the same `Known` variant as classes).
    Name(EcoString),
    /// A class-object (metaclass) receiver (`InferredType::Meta{C}`, e.g.
    /// `Counter spawn`), rendered with the same `'<C> class'` convention as
    /// `beamtalk_class_registry:class_object_tag/1` rather than falling into
    /// the "otherwise unresolved" bucket by omission (spike §1e).
    ClassObject(EcoString),
    /// BT-3215: a `Union{members}` receiver where *every* member itself
    /// resolves to a single name or class-object tag (never `Dynamic`) —
    /// the already-rendered atoms, sorted and deduplicated. The read path's
    /// `is_relevant/3` treats this with OR-semantics: relevant iff *any*
    /// member is relevant, since the receiver could be any one of them at
    /// runtime.
    Union(Vec<EcoString>),
    /// BT-3215: same resolution rule as [`RecvType::Union`], for
    /// `Intersection{members}`. The read path uses AND-semantics: relevant
    /// only if *every* member is relevant, since the receiver must
    /// simultaneously satisfy all of them.
    Intersection(Vec<EcoString>),
    /// Everything else: `Negation`/`Dynamic`/`Never`, a `Union`/
    /// `Intersection` with at least one member that doesn't resolve to a
    /// single name (nested composed type, `Dynamic`, oversized atom, …),
    /// no `TypeMap` entry at all, or a `Known` whose provenance is
    /// `Extracted` (native/FFI, ADR 0075) or `Aliased` (ADR 0108) — neither
    /// of those two names has a `beamtalk_class_metadata` row, so coarsening
    /// them here (rather than deferring to the read path, which has no way
    /// to tell them apart from a genuine class) is the spike §4 fix.
    Dynamic,
}

/// BT-3217 (ADR 0115 Phase 2) write-path projection rule (spike §1e/§4):
/// projects one message send's receiver `InferredType` — looked up from the
/// type checker's `TypeMap` by the receiver's span — onto [`RecvType`].
fn project_recv_type(ty: &InferredType) -> RecvType {
    match ty {
        InferredType::Known {
            class_name,
            provenance,
            ..
        } => match provenance {
            // Neither an FFI/native type name nor an alias display name has
            // a `beamtalk_class_metadata` row the Phase 3 read path could
            // resolve — coarsen to `dynamic` at write time rather than let
            // the read path discover an unresolvable name (spike §4).
            TypeProvenance::Extracted | TypeProvenance::Aliased { .. } => RecvType::Dynamic,
            TypeProvenance::Declared(_)
            | TypeProvenance::Inferred(_)
            | TypeProvenance::Substituted(_) => RecvType::Name(class_name.clone()),
        },
        InferredType::Meta { class_name, .. } => RecvType::ClassObject(class_name.clone()),
        // BT-3215: project each member the same way a single-name receiver
        // would be projected; if every member resolves cleanly, key on the
        // member list instead of coarsening the whole composed type away.
        InferredType::Union { members, .. } => project_composed(members, RecvType::Union),
        InferredType::Intersection { members, .. } => {
            project_composed(members, RecvType::Intersection)
        }
        InferredType::Dynamic(_) | InferredType::Never | InferredType::Negation { .. } => {
            RecvType::Dynamic
        }
    }
}

/// BT-3215: shared `Union`/`Intersection` projection — resolves each member
/// to the same single atom [`recv_type_atom`] would render for it as a
/// standalone receiver, via [`project_recv_type`] recursively. A nested
/// composed type or anything else that isn't a clean single name (`Dynamic`,
/// an oversized atom, a native/alias-coarsened `Known`, …) makes any member
/// unresolvable — per Constraint 2 ("cannot be narrowed and must never be
/// excluded"), a *partial* member list would be unsound: dropping an
/// unresolvable member and keying on only the resolvable ones would make the
/// read path wrongly exclude a real dependent whose receiver happens to be
/// typed as that dropped member at runtime. So any unresolvable member
/// coarsens the *entire* composed type to `dynamic`, exactly like a
/// single-name receiver that doesn't resolve.
fn project_composed(members: &[InferredType], make: fn(Vec<EcoString>) -> RecvType) -> RecvType {
    let mut names: Vec<EcoString> = Vec::with_capacity(members.len());
    for member in members {
        let resolved = match project_recv_type(member) {
            RecvType::Name(name) if name.len() <= MAX_ATOM_BYTES => name,
            RecvType::ClassObject(name) if name.len() + " class".len() <= MAX_ATOM_BYTES => {
                EcoString::from(super::util::metaclass_tag(&name))
            }
            RecvType::Name(_)
            | RecvType::ClassObject(_)
            | RecvType::Union(_)
            | RecvType::Intersection(_)
            | RecvType::Dynamic => return RecvType::Dynamic,
        };
        names.push(resolved);
    }
    names.sort();
    names.dedup();
    make(names)
}

/// Renders a [`RecvType`] as the Core Erlang literal baked into a
/// `method_xref` send entry's `recv_type` field: a bare atom for
/// `Name`/`ClassObject`/`Dynamic`, or a `{'union' | 'intersection',
/// [Atom, ...]}` tuple for a composed type (BT-3215). Falls back to
/// `'dynamic'` for a name that would exceed the `MAX_ATOM_BYTES` cap —
/// `project_composed` already
/// enforces this per member, so `Union`/`Intersection` never reach here with
/// an oversized member atom.
fn recv_type_atom(recv_type: &RecvType) -> Document<'static> {
    match recv_type {
        RecvType::Name(name) if name.len() <= MAX_ATOM_BYTES => leaf::atom(name.to_string()),
        RecvType::ClassObject(name) if name.len() + " class".len() <= MAX_ATOM_BYTES => {
            leaf::atom(super::util::metaclass_tag(name))
        }
        RecvType::Union(names) => docvec!["{'union', ", recv_type_name_list_doc(names), "}"],
        RecvType::Intersection(names) => {
            docvec!["{'intersection', ", recv_type_name_list_doc(names), "}"]
        }
        RecvType::Name(_) | RecvType::ClassObject(_) | RecvType::Dynamic => leaf::atom("dynamic"),
    }
}

/// Renders a `[Atom, ...]` Core Erlang list of already-resolved member
/// names for a `Union`/`Intersection` `recv_type` (BT-3215) — the shared
/// bracket/comma-join helper the two `recv_type_atom` composed-type arms
/// use, mirroring `meta_type_repr_list_doc`'s bracket/join pattern.
fn recv_type_name_list_doc(names: &[EcoString]) -> Document<'static> {
    let parts: Vec<Document<'static>> = names.iter().map(|n| leaf::atom(n.to_string())).collect();
    docvec!["[", join(parts, &Document::Str(", ")), "]"]
}

impl CoreErlangGenerator {
    /// ADR 0087 Phase 2 (BT-2298): Builds the `method_xref` list document baked
    /// into `register_class/0`'s `ClassInfo` (via `BuilderState.methodXref`).
    ///
    /// One entry per primary method (instance- and class-side). Each entry
    /// records the method's defining line, the selectors it sends (with
    /// receiver kind), and the classes it references — the per-method rows
    /// `beamtalk_xref:register_class/2` fans out into the senders / references /
    /// methods ETS tables at class-load time.
    ///
    /// The send / reference data comes from the existing AST walkers
    /// ([`beamtalk_core::method_source_walker::find_all_sends_in_source`] and
    /// [`beamtalk_core::method_source_walker::find_all_references_in_source`]).
    /// Those operate on a plain `unparse_method(method)` of the method — *not*
    /// [`Self::extract_method_source`], which (BT-3249) strips any
    /// writeback-inferred `-> Type` annotation for the human-facing browsable
    /// source. xref/`referencesTo:` deliberately keeps such annotations (an
    /// inferred return type is still a real type reference), so this walk's
    /// source can differ in *content* (an extra `-> Type` token) from what
    /// `SystemNavigation`'s miss-policy fallback shows — but never in *line
    /// count* (the annotation is inline on the signature line), so baked line
    /// numbers stay method-relative and consistent with the fallback. No port
    /// round-trip; one in-process walk per method.
    ///
    /// Hand-written rows carry `source_status => indexed` and *omit* the
    /// optional `synthetic_origin` key (never emitted as a `null` sentinel).
    ///
    /// ADR 0087 Phase 6 (BT-2304): compiler-generated auto-accessors for
    /// `Value subclass:` classes (the `field/1` getters and `withField:/2`
    /// setters emitted by `value_type_codegen.rs`) have no user source text but
    /// are fully known to the compiler. They ride this same write path: their
    /// rows carry `source_status => synthetic` and a derived `synthetic_origin`
    /// line pointing at the generating slot declaration (or the class header).
    /// Included by default so `implementorsOf: #value` on an auto-accessor is
    /// non-empty — a documented parity exception, not a regression.
    pub(in crate::core_erlang) fn build_method_xref_list(
        &self,
        class: &ClassDefinition,
        instance_methods: &[&MethodDefinition],
        class_methods: &[&MethodDefinition],
    ) -> Document<'static> {
        let mut entries: Vec<Document<'static>> = Vec::new();
        for method in instance_methods {
            entries.push(self.build_method_xref_entry(method, false));
        }
        for method in class_methods {
            entries.push(self.build_method_xref_entry(method, true));
        }
        // ADR 0087 Phase 6 (BT-2304): synthetic auto-accessor rows.
        entries.extend(self.build_synthetic_accessor_xref_entries(class));
        // BT-3073: actor class-side `new`/`new:`/`spawn`/`spawnWith:` no longer
        // get synthetic per-subclass rows here — BT-3071/BT-3072 lifted their
        // bodies into real, source-backed class methods on `Actor` itself
        // (`stdlib/src/actor.bt`), so a subclass genuinely *inherits* them
        // rather than *defining* them. `Actor`'s own compilation indexes them
        // through the normal `build_method_xref_entry` path above (real
        // `MethodDefinition`s, `source_status => indexed`); subclasses simply
        // have no row for them, which is the honest Smalltalk answer — see
        // BT-2614 (introduced the now-removed rows) and BT-3073 (retired them).
        docvec!["[", join(entries, &Document::Str(", ")), "]"]
    }

    /// BT-3439: Builds the `state_var_xref` list document baked into
    /// `register_class/0`'s `ClassInfo` (via `BuilderState.stateVarXref`),
    /// analogous to [`Self::build_method_xref_list`] but for instance-variable
    /// (`state:`/`field:`) declarations rather than methods.
    ///
    /// One entry per declared instance variable, carrying its name and
    /// 1-based declaration line (derived from [`StateDeclaration::span`] via
    /// [`Self::span_to_line`]) — `beamtalk_xref:register_state_vars/2` uses
    /// this so the VS Code Workspace Explorer sidebar's field goto
    /// (`beamtalk.navigateToStateVar`) can jump to the real declaration
    /// instead of guessing via source-text regex (BT-3439).
    ///
    /// A slot whose span cannot be resolved to a line (should not happen for
    /// real source, only a defensive fallback) is skipped rather than
    /// emitting a misleading line 1.
    ///
    /// Its only caller is [`Self::generate_register_class`] in
    /// `class_registry.rs`, which (like this function) lives directly under
    /// `core_erlang`, not under `gen_server`.
    pub(in crate::core_erlang) fn build_state_var_xref_list(
        &self,
        class: &ClassDefinition,
    ) -> Document<'static> {
        let entries: Vec<Document<'static>> = class
            .state
            .iter()
            .filter_map(|slot| {
                let line = self.span_to_line(slot.span)?;
                Some(docvec![
                    "~{'name' => ",
                    leaf::atom(slot.name.name.to_string()),
                    ", 'line' => ",
                    leaf::int_lit(i64::from(line)),
                    "}~",
                ])
            })
            .collect();
        docvec!["[", join(entries, &Document::Str(", ")), "]"]
    }

    /// Builds one `method_xref` entry map for a single method (ADR 0087 Phase 2).
    fn build_method_xref_entry(
        &self,
        method: &MethodDefinition,
        class_side: bool,
    ) -> Document<'static> {
        use beamtalk_core::method_source_walker::{
            ReceiverKind, collect_receiver_spans, find_all_references_in_source,
            find_all_sends_in_source,
        };

        // Erlang atoms cap at 255 bytes. A selector / class name longer than
        // that (e.g. a 20-keyword auto-constructor selector) can never exist as
        // a runtime dispatch atom, so a send / reference to it would never match
        // an xref query. Drop such entries rather than emitting an illegal atom
        // that fails `core_scan` at BEAM-compile time.

        // Unlike `extract_method_source` (used for the *browsable* `methodSource`/
        // `classMethodSource` maps, BT-3249), this xref walk deliberately keeps any
        // writeback-inferred `-> Type` annotation: `find_all_references_in_source`
        // explicitly walks `method.return_type` to record type references for
        // `referencesTo:`/xref queries, and an inferred-but-unannotated return type
        // is still a real reference the method's compiled behavior carries — only
        // the human-facing source text should hide it, not the xref data derived
        // from the full (annotated) AST.
        let source = beamtalk_core::unparse::unparse_method(method);

        // The method definition's line within its own (bare) source is line 1:
        // `unparse_method` emits the signature first (after any doc comment /
        // @expect lines the unparser prepends). The xref `line` field is the
        // method-relative definition line, so the first send/ref lines are
        // already in the same coordinate space.
        let def_line = Self::method_def_line(&source);

        let sends = find_all_sends_in_source(&source);

        // BT-3217 (ADR 0115 Phase 2): a second, span-carrying walk over the
        // *original* `method` (file-absolute spans, unlike `sends` above,
        // which comes from a re-unparsed/re-parsed synthetic copy — see the
        // ADR 0115 Phase 1 spike, docs/internal/adr-0115-phase1-spike-findings.md
        // §1c). Joined to `sends` **by pre-order ordinal**, before the
        // `MAX_ATOM_BYTES` filter below (a filter afterward would skew the
        // pairing) — the two walks are required to stay structurally
        // identical, verified by the corpus conformance test in
        // `source_analysis::method_span_corpus_tests`, not merely asserted
        // by this comment.
        let receiver_spans = collect_receiver_spans(method);
        // Defensive fallback for a divergence shape the corpus test doesn't
        // cover (see the comment above): a length mismatch means the
        // pre-order-ordinal pairing can't be trusted for *any* entry in this
        // method, so degrade the whole method to `dynamic` rather than risk
        // silently attributing a `recv_type` to the wrong selector.
        let spans_aligned = sends.len() == receiver_spans.len();
        let recv_types: Vec<RecvType> = sends
            .iter()
            .enumerate()
            .map(|(i, _hit)| {
                if !spans_aligned {
                    return RecvType::Dynamic;
                }
                receiver_spans
                    .get(i)
                    .and_then(|span_hit| self.type_map.get(span_hit.span))
                    .map_or(RecvType::Dynamic, project_recv_type)
            })
            .collect();

        let sends_doc = {
            let send_docs: Vec<Document<'static>> = sends
                .iter()
                .zip(recv_types.iter())
                .filter(|(hit, _)| hit.selector.len() <= MAX_ATOM_BYTES)
                .map(|(hit, recv_type)| {
                    let recv_kind = match hit.receiver {
                        ReceiverKind::SelfReceiver => "self_recv",
                        ReceiverKind::SuperReceiver => "super_recv",
                        ReceiverKind::ErlangFfi => "erlang_ffi",
                        ReceiverKind::Other => "other",
                    };
                    docvec![
                        "~{'selector' => ",
                        leaf::atom(hit.selector.clone()),
                        ", 'line' => ",
                        leaf::int_lit(i64::from(hit.line)),
                        ", 'recv_kind' => ",
                        leaf::atom(recv_kind),
                        ", 'recv_type' => ",
                        recv_type_atom(recv_type),
                        "}~",
                    ]
                })
                .collect();
            docvec!["[", join(send_docs, &Document::Str(", ")), "]"]
        };

        let references = find_all_references_in_source(&source);
        let refs_doc = {
            let ref_docs: Vec<Document<'static>> = references
                .iter()
                .filter(|hit| hit.class.len() <= MAX_ATOM_BYTES)
                .map(|hit| {
                    docvec![
                        "~{'class' => ",
                        leaf::atom(hit.class.clone()),
                        ", 'line' => ",
                        leaf::int_lit(i64::from(hit.line)),
                        "}~",
                    ]
                })
                .collect();
            docvec!["[", join(ref_docs, &Document::Str(", ")), "]"]
        };

        docvec![
            "~{'class_side' => ",
            if class_side { "'true'" } else { "'false'" },
            ", 'selector' => ",
            leaf::atom(method.selector.name().to_string()),
            ", 'line' => ",
            leaf::int_lit(i64::from(def_line)),
            ", 'sends' => ",
            sends_doc,
            ", 'references' => ",
            refs_doc,
            ", 'source_status' => 'indexed'}~",
        ]
    }

    /// Determine the method-relative definition line for an unparsed bare-method
    /// source: the first non-blank line that is not a leading doc comment
    /// (`///`), block/line comment, or `@expect`/`@`-directive line the unparser
    /// may prepend before the signature. Returns 1 if none is found.
    ///
    /// Multi-line block comments are tracked across lines so a continuation line
    /// (e.g. `   still inside the comment */`) is not mistaken for the signature.
    /// In practice the unparser emits `///`/`//` doc and line comments rather than
    /// `/* */` blocks before a signature, so this is defensive (per BT-2298 review).
    fn method_def_line(source: &str) -> u32 {
        let mut in_block_comment = false;
        for (idx, raw) in source.lines().enumerate() {
            let trimmed = raw.trim_start();
            if in_block_comment {
                if trimmed.contains("*/") {
                    in_block_comment = false;
                }
                continue;
            }
            if trimmed.starts_with("/*") {
                // A single-line `/* ... */` is fully consumed here; an unterminated
                // opener enters block-comment mode for subsequent lines.
                if !trimmed.contains("*/") {
                    in_block_comment = true;
                }
                continue;
            }
            if trimmed.is_empty()
                || trimmed.starts_with("///")
                || trimmed.starts_with("//")
                || trimmed.starts_with('@')
            {
                continue;
            }
            #[allow(clippy::cast_possible_truncation)]
            return (idx as u32) + 1;
        }
        1
    }
}

#[cfg(test)]
mod tests;
