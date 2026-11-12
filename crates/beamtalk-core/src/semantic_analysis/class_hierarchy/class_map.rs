// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `ClassHierarchy` class map, with every read recorded (BT-4586).
//!
//! **DDD Context:** Semantic Analysis
//!
//! The map is wrapped so that no read can bypass the per-file consulted-surface
//! recorder ([`crate::semantic_analysis::consulted`]): by-name reads record the
//! name, whole-map reads record a scan. Writes (`insert`, `get_mut`, `remove`,
//! `entry`, …) are not recorded. [`ClassMap::iter_unrecorded`] exists only for
//! hierarchy *construction* passes (selector-index rebuilds, merges) that read
//! the map in order to write it.

use super::ClassInfo;
use crate::semantic_analysis::consulted::{ScanKind, record_name, record_scan};
use ecow::EcoString;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(super) struct ClassMap(HashMap<EcoString, ClassInfo>);

impl From<HashMap<EcoString, ClassInfo>> for ClassMap {
    fn from(map: HashMap<EcoString, ClassInfo>) -> Self {
        Self(map)
    }
}

impl ClassMap {
    /// By-name read: records `name`.
    pub(super) fn get(&self, name: &str) -> Option<&ClassInfo> {
        record_name(name);
        self.0.get(name)
    }

    /// By-name read: records `name`.
    pub(super) fn contains_key(&self, name: &str) -> bool {
        record_name(name);
        self.0.contains_key(name)
    }

    /// Construction-time read of one entry; **not recorded** (see
    /// [`Self::iter_unrecorded`]).
    pub(super) fn get_unrecorded(&self, name: &str) -> Option<&ClassInfo> {
        self.0.get(name)
    }

    /// Construction-time membership test; **not recorded** (see
    /// [`Self::iter_unrecorded`]).
    pub(super) fn contains_key_unrecorded(&self, name: &str) -> bool {
        self.0.contains_key(name)
    }

    /// Whole-map read: records a scan.
    pub(super) fn iter(&self) -> std::collections::hash_map::Iter<'_, EcoString, ClassInfo> {
        record_scan(ScanKind::Classes);
        self.0.iter()
    }

    /// Whole-map read: records a scan.
    pub(super) fn keys(&self) -> std::collections::hash_map::Keys<'_, EcoString, ClassInfo> {
        record_scan(ScanKind::Classes);
        self.0.keys()
    }

    /// Whole-map read: records a scan.
    pub(super) fn values(&self) -> std::collections::hash_map::Values<'_, EcoString, ClassInfo> {
        record_scan(ScanKind::Classes);
        self.0.values()
    }

    /// Construction-time read of the whole map; **not recorded**. Only for
    /// passes that read the map in order to rebuild or merge into it.
    pub(super) fn iter_unrecorded(
        &self,
    ) -> std::collections::hash_map::Iter<'_, EcoString, ClassInfo> {
        self.0.iter()
    }

    // Writes: not recorded.

    pub(super) fn get_mut(&mut self, name: &str) -> Option<&mut ClassInfo> {
        self.0.get_mut(name)
    }

    pub(super) fn insert(&mut self, name: EcoString, info: ClassInfo) -> Option<ClassInfo> {
        self.0.insert(name, info)
    }

    pub(super) fn remove(&mut self, name: &str) -> Option<ClassInfo> {
        self.0.remove(name)
    }

    pub(super) fn entry(&mut self, name: EcoString) -> Entry<'_, EcoString, ClassInfo> {
        self.0.entry(name)
    }

    pub(super) fn iter_mut(
        &mut self,
    ) -> std::collections::hash_map::IterMut<'_, EcoString, ClassInfo> {
        self.0.iter_mut()
    }
}

impl<'a> IntoIterator for &'a ClassMap {
    type Item = (&'a EcoString, &'a ClassInfo);
    type IntoIter = std::collections::hash_map::Iter<'a, EcoString, ClassInfo>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut ClassMap {
    type Item = (&'a EcoString, &'a mut ClassInfo);
    type IntoIter = std::collections::hash_map::IterMut<'a, EcoString, ClassInfo>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}
