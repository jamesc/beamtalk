// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-file record of the cross-file analysis surface a compilation consulted
//! (BT-4586, phase 1 of per-file incremental dependency edges).
//!
//! **DDD Context:** Semantic Analysis
//!
//! The incremental build keys every file on a package-wide surface hash, so any
//! signature change anywhere rebuilds everything. A *static* reference closure
//! is not sound: analysis and codegen also scan the whole hierarchy
//! ("does any class define selector X?"). Instead the build records what each
//! file's analysis and codegen **actually consulted**:
//!
//! - a by-name lookup (`get_class`, `has_class`, protocol/alias lookups, …)
//!   records that name;
//! - a whole-collection scan (`class_names`, iteration, "any class …" queries)
//!   sets [`ConsultedSurface::scanned_all`];
//! - a read that depends on package-level state that is not fully loaded
//!   (`dependency_extensions_unknown`) sets
//!   [`ConsultedSurface::extensions_unknown`].
//!
//! The stores (`ClassHierarchy`'s class map, `ProtocolRegistry`,
//! `AliasRegistry`) keep their maps private, so no *external* read can bypass
//! recording. Inside the stores, registration and cycle-check passes and the
//! `ClassMap::*_unrecorded` construction helpers read the raw maps on purpose.
//!
//! Known over-recording for phase 2 to narrow: hierarchy construction inside a
//! file's scope (for example `add_external_superclasses`, which reads every
//! name of the cross-file superclass index) records those names, and often a
//! scan. That is a real dependency on the injected index, so it is left
//! recorded rather than hidden; the surface is only logged in phase 1.
//!
//! Recording is thread-local and scoped by [`ConsultedScope`]. With no scope
//! installed (the REPL and LSP) every hook is a single flag check.
//!
//! Phase 1 only records: the result is logged and never used for the cache key.

use ecow::EcoString;
use std::cell::{Cell, RefCell};
use std::collections::BTreeSet;

/// What one file's analysis and codegen consulted of the cross-file surface.
///
/// Names are bare class, protocol and alias names, as passed to the lookup
/// (a name may be absent from the store; an absent lookup still depends on it).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ConsultedSurface {
    /// Names looked up individually.
    pub names: BTreeSet<EcoString>,
    /// A whole-collection scan happened: the file depends on the whole surface.
    pub scanned_all: bool,
    /// An extension-related read happened while dependency extensions were
    /// not all loaded: a later phase must treat the file as depending on everything.
    pub extensions_unknown: bool,
}

impl ConsultedSurface {
    /// True when nothing cross-file was consulted.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.names.is_empty() && !self.scanned_all && !self.extensions_unknown
    }
}

thread_local! {
    static ACTIVE: Cell<bool> = const { Cell::new(false) };
    static RECORDER: RefCell<Option<ConsultedSurface>> = const { RefCell::new(None) };
}

/// Guard that records consulted names on the current thread while alive.
///
/// Scopes nest: dropping (or finishing) a scope restores the enclosing one.
/// One scope covers one file's analysis and codegen; the files of a build are
/// compiled one at a time per thread, so each gets its own scope.
#[derive(Debug)]
pub struct ConsultedScope {
    previous: Option<ConsultedSurface>,
    finished: bool,
    /// Scopes restore thread-local state in LIFO order, so one must end on the
    /// thread that began it: this makes the guard `!Send`.
    _not_send: std::marker::PhantomData<*const ()>,
}

impl ConsultedScope {
    /// Start recording on the current thread.
    #[must_use]
    pub fn begin() -> Self {
        let previous = RECORDER.with(|r| r.borrow_mut().replace(ConsultedSurface::default()));
        ACTIVE.with(|a| a.set(true));
        Self {
            previous,
            finished: false,
            _not_send: std::marker::PhantomData,
        }
    }

    /// Stop recording and return what was consulted.
    #[must_use]
    pub fn finish(mut self) -> ConsultedSurface {
        self.finished = true;
        self.restore()
    }

    fn restore(&mut self) -> ConsultedSurface {
        let previous = self.previous.take();
        ACTIVE.with(|a| a.set(previous.is_some()));
        RECORDER
            .with(|r| std::mem::replace(&mut *r.borrow_mut(), previous))
            .unwrap_or_default()
    }
}

impl Drop for ConsultedScope {
    fn drop(&mut self) {
        if !self.finished {
            let _ = self.restore();
        }
    }
}

/// Record a by-name lookup. A flag check when no scope is installed.
#[inline]
pub(crate) fn record_name(name: &str) {
    if ACTIVE.with(Cell::get) {
        record_name_slow(name);
    }
}

#[inline(never)]
fn record_name_slow(name: &str) {
    RECORDER.with(|r| {
        if let Some(s) = r.borrow_mut().as_mut() {
            if !s.names.contains(name) {
                s.names.insert(name.into());
            }
        }
    });
}

/// Record a whole-collection scan. A flag check when no scope is installed.
#[inline]
pub(crate) fn record_scan() {
    if ACTIVE.with(Cell::get) {
        RECORDER.with(|r| {
            if let Some(s) = r.borrow_mut().as_mut() {
                s.scanned_all = true;
            }
        });
    }
}

/// Record an extension-related read made while dependency extensions are unknown.
#[inline]
pub(crate) fn record_extensions_unknown() {
    if ACTIVE.with(Cell::get) {
        RECORDER.with(|r| {
            if let Some(s) = r.borrow_mut().as_mut() {
                s.extensions_unknown = true;
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn records_only_inside_a_scope() {
        record_name("Outside");
        let scope = ConsultedScope::begin();
        record_name("A");
        record_name("A");
        record_scan();
        let s = scope.finish();
        assert_eq!(
            s.names.iter().map(EcoString::as_str).collect::<Vec<_>>(),
            ["A"]
        );
        assert!(s.scanned_all);
        record_name("After");
        assert!(!ACTIVE.with(Cell::get));
    }

    #[test]
    fn scopes_nest_and_restore() {
        let outer = ConsultedScope::begin();
        record_name("Outer");
        {
            let inner = ConsultedScope::begin();
            record_name("Inner");
            let s = inner.finish();
            assert!(s.names.contains("Inner") && !s.names.contains("Outer"));
        }
        {
            let _dropped = ConsultedScope::begin();
            record_name("Dropped");
        }
        record_name("Outer2");
        let s = outer.finish();
        assert!(s.names.contains("Outer") && s.names.contains("Outer2"));
        assert!(!s.names.contains("Inner") && !s.names.contains("Dropped"));
    }

    use crate::semantic_analysis::alias_registry::AliasRegistry;
    use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    #[test]
    fn method_lookup_records_the_class_and_the_supertypes_it_resolved_through() {
        let hierarchy = ClassHierarchy::with_builtins();
        let chain = hierarchy.superclass_chain("Integer");
        assert!(chain.len() >= 2, "{chain:?}");
        let scope = ConsultedScope::begin();
        // `printString` is defined on a supertype, so the lookup walks the chain.
        let found = hierarchy.find_method("Integer", "printString");
        let s = scope.finish();
        assert!(found.is_some());
        assert!(s.names.contains("Integer"), "{s:?}");
        let owner = found.map(|m| m.defined_in.clone()).unwrap();
        assert!(
            s.names.contains(&owner),
            "supertype {owner} must be recorded: {s:?}"
        );
        assert!(!s.names.contains("Dictionary"), "{s:?}");
        assert!(!s.scanned_all, "{s:?}");
    }

    #[test]
    fn no_lookups_record_an_empty_surface() {
        let _hierarchy = ClassHierarchy::with_builtins();
        let scope = ConsultedScope::begin();
        let s = scope.finish();
        assert!(s.is_empty(), "{s:?}");
    }

    #[test]
    fn whole_hierarchy_reads_record_scanned_all() {
        let hierarchy = ClassHierarchy::with_builtins();
        for scan in [
            (|h: &ClassHierarchy| {
                let _ = h.class_names().count();
            }) as fn(&ClassHierarchy),
            |h| {
                let _ = h.iter_classes().count();
            },
        ] {
            let scope = ConsultedScope::begin();
            scan(&hierarchy);
            let s = scope.finish();
            assert!(s.scanned_all, "{s:?}");
            assert!(s.names.is_empty(), "{s:?}");
        }
    }

    #[test]
    fn by_name_lookups_record_in_every_store() {
        let hierarchy = ClassHierarchy::with_builtins();
        let protocols = ProtocolRegistry::new();
        let aliases = AliasRegistry::new();
        let scope = ConsultedScope::begin();
        let _ = hierarchy.get_class("Integer");
        let _ = hierarchy.has_class("Missing");
        let _ = hierarchy.is_protocol_class("Printable");
        let _ = hierarchy.has_own_class_method("OwnerClass", "new:");
        let _ = protocols.get("Proto");
        let _ = protocols.has_protocol("Proto2");
        let _ = aliases.get("Alias");
        let _ = aliases.has_alias("Alias2");
        let s = scope.finish();
        for name in [
            "Integer",
            "Missing",
            "Printable",
            "OwnerClass",
            "Proto",
            "Proto2",
            "Alias",
            "Alias2",
        ] {
            assert!(s.names.contains(name), "{name} missing from {s:?}");
        }
        assert!(!s.scanned_all);
    }

    #[test]
    fn registry_scans_record_scanned_all_and_unknown_extensions_are_flagged() {
        let protocols = ProtocolRegistry::new();
        let aliases = AliasRegistry::new();
        let mut hierarchy = ClassHierarchy::with_builtins();
        let scope = ConsultedScope::begin();
        let _ = protocols.protocols().count();
        let s = scope.finish();
        assert!(s.scanned_all);
        let scope = ConsultedScope::begin();
        let _ = aliases.aliases().count();
        assert!(scope.finish().scanned_all);

        let scope = ConsultedScope::begin();
        let _ = hierarchy.dependency_extensions_unknown();
        assert!(scope.finish().is_empty(), "known extensions record nothing");
        hierarchy.set_dependency_extensions_unknown(true);
        let scope = ConsultedScope::begin();
        let _ = hierarchy.dependency_extensions_unknown();
        let s = scope.finish();
        assert!(s.extensions_unknown, "{s:?}");
    }
}
