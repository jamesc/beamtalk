// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0100 Rule 1 — the receiver-knowledge completeness ladder.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Centralises the "how completely does the checker know this receiver's
//! method surface?" decision behind a single classifier, so
//! `type_checker/validation.rs` and `protocol_registry.rs` cannot disagree
//! about the same call site (ADR 0100, "Implementation" section).
//!
//! History: BT-2793 centralised the decision (pure refactor); BT-2796 added
//! the parse-error guard and [`KnowledgeScope`]; BT-2795 made project-wide
//! extensions visible; BT-2794 completed ADR 0100 Rule 2. With WS1/WS2
//! landed, `has_cross_file_parent` fires only when an ancestor is genuinely
//! absent from project-complete knowledge (or in `ModuleOnly` contexts) —
//! suppression replaced by resolution, staying conservative where knowledge
//! is genuinely incomplete.

use super::class_hierarchy::ClassHierarchy;

/// How complete the knowledge injected into an analysis run is (BT-2796, WS2).
///
/// The orchestrator that assembles cross-file knowledge (CLI build Pass 1,
/// CLI lint's package walk, the LSP `ProjectIndex` after workspace preload)
/// declares how far that knowledge extends. The classifier uses this to
/// decide whether "a parent class is missing from the hierarchy" means
/// *incomplete knowledge* (single-file analysis — stay quiet) or *genuinely
/// unresolved* (project-complete analysis — still conservative, but eligible
/// for ADR 0100 Rule 2's suppression removal, BT-2794).
///
/// This is the per-hierarchy realisation of ADR 0100's sequencing-guard
/// "feature flag": no orchestrator claims [`ProjectComplete`](Self::ProjectComplete)
/// until its assembly genuinely covers the whole project, and any context
/// that cannot claim it keeps today's conservative behaviour automatically.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum KnowledgeScope {
    /// Only the current module (plus whatever was injected) is known.
    /// Today's conservative rules apply unchanged. This is the default for
    /// isolated single-file analysis, the REPL/compiler-port, and any
    /// context that has not walked the whole project.
    #[default]
    ModuleOnly,
    /// The orchestrator walked the entire project: every project file's
    /// classes are present in the hierarchy (extensions and protocols ride
    /// the same channel as they land — WS1/BT-2795).
    ProjectComplete,
}

/// How completely the checker knows a receiver's method surface (ADR 0100 Rule 1).
///
/// A conservative classification consulted before validation code decides
/// whether an unresolved selector deserves a diagnostic. "Absence of
/// evidence is not evidence of absence in an open world" — when the
/// classifier cannot prove the surface is fully known, it downgrades to
/// [`Open`](Self::Open) rather than risk a false positive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReceiverKnowledge {
    /// The receiver's class is not statically known at all (e.g. not present
    /// in the hierarchy). No diagnostic is possible — there is nothing to
    /// check against.
    Dynamic,
    /// A known class, but the checker's view of its method surface is
    /// incomplete or intentionally left open: a `doesNotUnderstand:args:`
    /// override, or a cross-file parent whose inherited methods the checker
    /// cannot enumerate. Diagnostics are suppressed — silence, not evidence
    /// of absence.
    Open,
    /// A known, concrete class whose full method surface the checker can
    /// enumerate with certainty (no DNU override, no unknown ancestor). An
    /// unresolved selector against a `ClosedComplete` receiver is safe to
    /// diagnose (as a `Hint`, per Rule 1).
    ClosedComplete,
}

impl ReceiverKnowledge {
    /// Returns `true` only for [`ClosedComplete`](Self::ClosedComplete) —
    /// the one state in which emitting an unresolved-selector diagnostic is
    /// warranted.
    #[must_use]
    pub fn is_closed_complete(self) -> bool {
        matches!(self, Self::ClosedComplete)
    }
}

/// Classify how completely the checker knows `class_name`'s method surface.
///
/// `is_class_side` selects between the class-side DNU rule
/// (`has_class_dnu_override`) and the instance-side DNU rule
/// (`has_instance_dnu_override`).
///
/// Downgrade reasons folded into `Open` here:
/// - A `doesNotUnderstand:args:` override on the relevant side.
/// - [`ClassHierarchy::has_cross_file_parent`] — the ancestor chain includes
///   a class the checker cannot see. With project-scoped compilation
///   (WS1/WS2) this fires only for genuinely-unresolved parents or in
///   `ModuleOnly` contexts; in a project-complete build every intra-project
///   parent is injected, so resolution has replaced suppression
///   (ADR 0100 Rule 2, BT-2794).
/// - [`ClassHierarchy::has_incomplete_surface_in_chain`] — a chain class was
///   extracted from a file with parse errors (BT-2796).
/// - The pre-WS3 dependency guard — `ProjectComplete` scope with
///   `dependency_extensions_unknown` set (BT-2794; see `classify_receiver`
///   body).
///
/// **Not folded in here:** BT-1763's "sealed value type dispatches
/// class-side messages through instance dispatch" carve-out
/// (`is_sealed_with_instance_dnu`) is specific to the class-side DNU *hint*
/// decision in `validation.rs::check_class_side_send` — it does not apply to
/// `protocol_registry.rs`'s class-side conformance check, which has never
/// granted that carve-out. Folding it into this shared classifier would
/// silently change conformance behaviour for sealed classes, so BT-2793
/// keeps it call-site-local rather than widening it to all four sites.
#[must_use]
pub fn classify_receiver(
    class_name: &str,
    hierarchy: &ClassHierarchy,
    is_class_side: bool,
) -> ReceiverKnowledge {
    if !hierarchy.has_class(class_name) {
        return ReceiverKnowledge::Dynamic;
    }

    let has_dnu_override = if is_class_side {
        hierarchy.has_class_dnu_override(class_name)
    } else {
        hierarchy.has_instance_dnu_override(class_name)
    };
    if has_dnu_override {
        return ReceiverKnowledge::Open;
    }

    // BT-1736 / BT-2793: cross-file inheritance — if the parent class is not
    // in the hierarchy, the checker can't know the full method set.
    if hierarchy.has_cross_file_parent(class_name) {
        return ReceiverKnowledge::Open;
    }

    // BT-2796 (WS2 parse-error guard): a class extracted from a file with
    // parse errors may have an under-recovered method surface — error
    // recovery can silently drop method definitions. Treat the receiver as
    // Open if any class in its superclass chain carries that mark, so a
    // half-parsed file degrades only its own classes (and their subclasses)
    // to silence rather than emitting hints against a surface the checker
    // never fully saw.
    if hierarchy.has_incomplete_surface_in_chain(class_name) {
        return ReceiverKnowledge::Open;
    }

    // BT-2794 (ADR 0100 Rule 2, pre-WS3 guard): until cross-package
    // extension metadata is loaded (WS3, ADR 0070 amendment), a dependency
    // can extend *any* class — including `Object`, which every receiver's
    // chain reaches — so a package with dependencies has no receiver whose
    // surface is provably complete. Only applied under `ProjectComplete`:
    // `ModuleOnly` contexts (REPL, isolated files) keep today's behaviour.
    if hierarchy.knowledge_scope() == KnowledgeScope::ProjectComplete
        && hierarchy.dependency_extensions_unknown()
    {
        return ReceiverKnowledge::Open;
    }

    ReceiverKnowledge::ClosedComplete
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::MethodKind;
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use ecow::EcoString;
    use std::collections::HashMap;

    fn eco(s: &str) -> EcoString {
        EcoString::from(s)
    }

    fn dnu_method(class_name: &str) -> MethodInfo {
        MethodInfo {
            selector: eco("doesNotUnderstand:args:"),
            arity: 2,
            kind: MethodKind::Primary,
            defined_in: eco(class_name),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![None, None],
            doc: None,
        }
    }

    fn base_class_info(name: &str, superclass: &str) -> ClassInfo {
        ClassInfo {
            surface_incomplete: false,
            name: eco(name),
            superclass: Some(eco(superclass)),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            handle_scope: None,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        }
    }

    #[test]
    fn unknown_class_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        assert_eq!(
            classify_receiver("Nowhere", &hierarchy, false),
            ReceiverKnowledge::Dynamic
        );
    }

    #[test]
    fn plain_known_class_is_closed_complete() {
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![base_class_info("Plain", "Object")]);
        assert_eq!(
            classify_receiver("Plain", &hierarchy, false),
            ReceiverKnowledge::ClosedComplete
        );
        assert_eq!(
            classify_receiver("Plain", &hierarchy, true),
            ReceiverKnowledge::ClosedComplete
        );
    }

    #[test]
    fn instance_dnu_override_is_open_on_instance_side() {
        let mut hierarchy = ClassHierarchy::with_builtins();
        let mut info = base_class_info("Proxy", "Object");
        info.methods.push(dnu_method("Proxy"));
        hierarchy.add_from_beam_meta(vec![info]);
        assert_eq!(
            classify_receiver("Proxy", &hierarchy, false),
            ReceiverKnowledge::Open
        );
    }

    #[test]
    fn class_dnu_override_is_open_on_class_side_only() {
        let mut hierarchy = ClassHierarchy::with_builtins();
        let mut info = base_class_info("ErlangLike", "Object");
        info.class_methods.push(dnu_method("ErlangLike"));
        hierarchy.add_from_beam_meta(vec![info]);
        assert_eq!(
            classify_receiver("ErlangLike", &hierarchy, true),
            ReceiverKnowledge::Open
        );
        // A class-side DNU override does not suppress instance-side checks.
        assert_eq!(
            classify_receiver("ErlangLike", &hierarchy, false),
            ReceiverKnowledge::ClosedComplete
        );
    }

    #[test]
    fn sealed_instance_dnu_does_not_open_class_side_here() {
        // BT-1763's sealed+instance-DNU carve-out is call-site-local to
        // `validation.rs::check_class_side_send`, not part of the shared
        // classifier (see the doc comment on `classify_receiver`) — folding
        // it in here would change `protocol_registry.rs`'s conformance
        // behaviour for sealed classes, which BT-2793 must not do.
        let mut hierarchy = ClassHierarchy::with_builtins();
        let mut info = base_class_info("SealedProxy", "Value");
        info.is_sealed = true;
        info.methods.push(dnu_method("SealedProxy"));
        hierarchy.add_from_beam_meta(vec![info]);
        assert_eq!(
            classify_receiver("SealedProxy", &hierarchy, true),
            ReceiverKnowledge::ClosedComplete
        );
    }

    #[test]
    fn cross_file_parent_is_open() {
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![base_class_info("Child", "UnknownParent")]);
        assert_eq!(
            classify_receiver("Child", &hierarchy, false),
            ReceiverKnowledge::Open
        );
        assert_eq!(
            classify_receiver("Child", &hierarchy, true),
            ReceiverKnowledge::Open
        );
    }

    #[test]
    fn surface_incomplete_class_is_open() {
        // BT-2796: a class extracted from a file with parse errors may have
        // an under-recovered method surface — never diagnose against it.
        let mut hierarchy = ClassHierarchy::with_builtins();
        let mut info = base_class_info("HalfParsed", "Object");
        info.surface_incomplete = true;
        hierarchy.add_from_beam_meta(vec![info]);
        assert_eq!(
            classify_receiver("HalfParsed", &hierarchy, false),
            ReceiverKnowledge::Open
        );
        assert_eq!(
            classify_receiver("HalfParsed", &hierarchy, true),
            ReceiverKnowledge::Open
        );
    }

    #[test]
    fn surface_incomplete_parent_makes_subclass_open() {
        // BT-2796: the guard is transitive — a subclass inherits from an
        // under-recovered surface, so it cannot be ClosedComplete either.
        let mut hierarchy = ClassHierarchy::with_builtins();
        let mut parent = base_class_info("HalfParsedParent", "Object");
        parent.surface_incomplete = true;
        let child = base_class_info("CleanChild", "HalfParsedParent");
        hierarchy.add_from_beam_meta(vec![parent, child]);
        assert_eq!(
            classify_receiver("CleanChild", &hierarchy, false),
            ReceiverKnowledge::Open
        );
    }

    #[test]
    fn clean_chain_stays_closed_complete() {
        // BT-2796: the guard must not fire for fully-parsed chains.
        let mut hierarchy = ClassHierarchy::with_builtins();
        let parent = base_class_info("CleanParent", "Object");
        let child = base_class_info("CleanChild2", "CleanParent");
        hierarchy.add_from_beam_meta(vec![parent, child]);
        assert_eq!(
            classify_receiver("CleanChild2", &hierarchy, false),
            ReceiverKnowledge::ClosedComplete
        );
    }

    #[test]
    fn dependency_guard_opens_everything_under_project_complete() {
        // BT-2794 pre-WS3 guard: a dependency can extend any class
        // (including Object), so with deps present no receiver's surface is
        // provably complete.
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![base_class_info("LocalThing", "Object")]);
        hierarchy.set_knowledge_scope(KnowledgeScope::ProjectComplete);
        hierarchy.set_dependency_extensions_unknown(true);
        assert_eq!(
            classify_receiver("LocalThing", &hierarchy, false),
            ReceiverKnowledge::Open
        );
        assert_eq!(
            classify_receiver("String", &hierarchy, false),
            ReceiverKnowledge::Open
        );
    }

    #[test]
    fn no_dependencies_stays_closed_complete_under_project_complete() {
        // BT-2794: dependency-free packages get full Hint precision.
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![base_class_info("LocalThing", "Object")]);
        hierarchy.set_knowledge_scope(KnowledgeScope::ProjectComplete);
        assert_eq!(
            classify_receiver("LocalThing", &hierarchy, false),
            ReceiverKnowledge::ClosedComplete
        );
    }

    #[test]
    fn dependency_guard_not_applied_under_module_only() {
        // BT-2794: ModuleOnly contexts (REPL, isolated files) keep today's
        // behaviour — the ADR 0100 REPL example (`"hello" reverssed` hints)
        // must survive even if a deps flag were somehow set.
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.set_dependency_extensions_unknown(true);
        assert_eq!(
            classify_receiver("String", &hierarchy, false),
            ReceiverKnowledge::ClosedComplete
        );
    }

    #[test]
    fn is_closed_complete_helper() {
        assert!(ReceiverKnowledge::ClosedComplete.is_closed_complete());
        assert!(!ReceiverKnowledge::Open.is_closed_complete());
        assert!(!ReceiverKnowledge::Dynamic.is_closed_complete());
    }
}
