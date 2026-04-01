// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method resolution: finding, resolving, and walking methods across the MRO.
//!
//! Contains `all_methods`, `find_method`, `resolves_selector`, sealed-method
//! ancestor checks, DNU override detection, and return-type inference application.

use crate::ast::MethodKind;
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

use super::ClassHierarchy;
use super::class_info::{ClassInfo, MethodInfo};

impl ClassHierarchy {
    /// Returns all methods available on a class (local + inherited).
    ///
    /// Methods are returned in MRO order: local methods first, then inherited.
    /// If a subclass overrides a superclass method (same selector), only the
    /// subclass version is included.
    ///
    /// Handles cycles gracefully by tracking visited classes.
    #[must_use]
    pub fn all_methods(&self, class_name: &str) -> Vec<MethodInfo> {
        let mut seen_selectors: HashMap<EcoString, usize> = HashMap::new();
        let mut methods = Vec::new();
        let mut visited = HashSet::new();

        // Walk MRO: class itself, then superclass chain
        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break; // Cycle detected
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                for method in &info.methods {
                    if !seen_selectors.contains_key(&method.selector) {
                        seen_selectors.insert(method.selector.clone(), methods.len());
                        methods.push(method.clone());
                    }
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }

        methods
    }

    /// Returns all class-side methods for a class, walking the MRO.
    ///
    /// Class-side methods are defined with the `class` prefix in Beamtalk
    /// (e.g., `class spawn => ...`). They are methods on the class object itself,
    /// not on instances.
    #[must_use]
    pub fn all_class_methods(&self, class_name: &str) -> Vec<MethodInfo> {
        let mut seen_selectors: HashMap<EcoString, usize> = HashMap::new();
        let mut methods = Vec::new();
        let mut visited = HashSet::new();

        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                for method in &info.class_methods {
                    if method.kind == MethodKind::Primary {
                        if !seen_selectors.contains_key(&method.selector) {
                            seen_selectors.insert(method.selector.clone(), methods.len());
                            methods.push(method.clone());
                        }
                    } else {
                        methods.push(method.clone());
                    }
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }

        methods
    }

    /// Check if a class can respond to a given selector (local or inherited).
    ///
    /// Handles cycles gracefully by tracking visited classes.
    ///
    /// When an intermediate class in the superclass chain is not found in the
    /// hierarchy (e.g. it is defined in a separately-compiled file), the walk
    /// falls through to `Object`.  All Beamtalk classes ultimately inherit from
    /// `Object`, so this correctly exposes Object-level methods (e.g.
    /// `subclassResponsibility`) without producing false-positive DNU warnings
    /// for cross-file class hierarchies (BT-889).
    #[must_use]
    pub fn resolves_selector(&self, class_name: &str, selector: &str) -> bool {
        let mut current = Some(class_name.to_string());
        let mut visited = HashSet::new();
        let mut traversed_known = false;
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break; // Cycle detected
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                traversed_known = true;
                if self
                    .method_indexes
                    .get(name.as_str())
                    .is_some_and(|idx| idx.contains_key(selector))
                {
                    return true;
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                // Unknown class in the superclass chain — likely an external class
                // defined in a separately-compiled file.  Since all Beamtalk classes
                // ultimately inherit from Object, fall through to the Object chain so
                // that Object-level methods (e.g. subclassResponsibility) are visible.
                // Guard: only fall through if we already traversed at least one known
                // class, so that a completely-unknown root class does not incorrectly
                // resolve Object methods (BT-889).
                current = if traversed_known {
                    Some("Object".to_string())
                } else {
                    None
                };
            }
        }
        false
    }

    /// Check if a class can respond to a given class-side selector (local or inherited).
    ///
    /// BT-1611: Used by protocol conformance checking to verify class method requirements.
    /// Walks the superclass chain checking `class_methods` at each level.
    #[must_use]
    pub fn resolves_class_selector(&self, class_name: &str, selector: &str) -> bool {
        self.find_class_method(class_name, selector).is_some()
    }

    /// Find a method by selector in a class (including inherited methods).
    ///
    /// Returns the first matching primary method found in MRO order.
    ///
    /// When an intermediate class in the superclass chain is not found in the
    /// hierarchy, the walk falls through to `Object` (BT-889).
    #[must_use]
    pub fn find_method(&self, class_name: &str, selector: &str) -> Option<MethodInfo> {
        let mut current = Some(class_name.to_string());
        let mut visited = HashSet::new();
        let mut traversed_known = false;
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                traversed_known = true;
                if let Some(&idx) = self
                    .method_indexes
                    .get(name.as_str())
                    .and_then(|mi| mi.get(selector))
                {
                    let method = &info.methods[idx];
                    if method.kind == MethodKind::Primary {
                        return Some(method.clone());
                    }
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                // Unknown class in the superclass chain — fall through to Object (BT-889).
                // Guard: only when at least one known class was already traversed.
                current = if traversed_known {
                    Some("Object".to_string())
                } else {
                    None
                };
            }
        }
        None
    }

    /// Find a class-side method by selector (including inherited class methods).
    ///
    /// When an intermediate class in the superclass chain is not found in the
    /// hierarchy, the walk falls through to `Object` (BT-889).
    #[must_use]
    pub fn find_class_method(&self, class_name: &str, selector: &str) -> Option<MethodInfo> {
        let mut current = Some(class_name.to_string());
        let mut visited = HashSet::new();
        let mut traversed_known = false;
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                traversed_known = true;
                if let Some(&idx) = self
                    .class_method_indexes
                    .get(name.as_str())
                    .and_then(|mi| mi.get(selector))
                {
                    let method = &info.class_methods[idx];
                    if method.kind == MethodKind::Primary {
                        return Some(method.clone());
                    }
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                // Unknown class in the superclass chain — fall through to Object (BT-889).
                // Guard: only when at least one known class was already traversed.
                current = if traversed_known {
                    Some("Object".to_string())
                } else {
                    None
                };
            }
        }
        None
    }

    /// Collects the set of selectors that have `spawns_block: true` anywhere
    /// in the hierarchy.
    ///
    /// Used by the self-capture validator to precompute which selectors should
    /// skip false-positive warnings. Call once before walking the AST, then
    /// use `HashSet::contains` for O(1) lookups per expression node.
    #[must_use]
    pub fn spawns_block_selectors(&self) -> HashSet<EcoString> {
        self.classes
            .values()
            .flat_map(|info| info.methods.iter().chain(info.class_methods.iter()))
            .filter(|m| m.spawns_block)
            .map(|m| m.selector.clone())
            .collect()
    }

    /// Check if a class explicitly overrides `doesNotUnderstand:args:` on the instance side.
    ///
    /// Returns true only if the class itself (not an ancestor) defines the method.
    /// Classes with instance-side DNU override accept any instance message.
    #[must_use]
    pub fn has_instance_dnu_override(&self, class_name: &str) -> bool {
        self.has_dnu_override_in(class_name, |info| &info.methods)
    }

    /// Check if a class explicitly overrides `doesNotUnderstand:args:` on the class side.
    ///
    /// Returns true only if the class itself (not an ancestor) defines the method.
    /// Classes with class-side DNU override accept any class message (e.g., `Erlang`).
    #[must_use]
    pub fn has_class_dnu_override(&self, class_name: &str) -> bool {
        self.has_dnu_override_in(class_name, |info| &info.class_methods)
    }

    fn has_dnu_override_in(
        &self,
        class_name: &str,
        get_methods: impl Fn(&ClassInfo) -> &Vec<MethodInfo>,
    ) -> bool {
        self.classes.get(class_name).is_some_and(|info| {
            get_methods(info).iter().any(|m| {
                m.selector.as_str() == "doesNotUnderstand:args:"
                    && m.kind == MethodKind::Primary
                    && m.defined_in.as_str() == class_name
            })
        })
    }

    /// Walk the superclass chain looking for a sealed primary method with the given selector.
    /// Returns `Some((class_name, MethodInfo))` if found, `None` otherwise.
    pub(super) fn find_sealed_method_in_ancestors(
        &self,
        start_class: &str,
        selector: &str,
    ) -> Option<(EcoString, MethodInfo)> {
        let mut current = Some(start_class.to_string());
        let mut visited = HashSet::new();
        let mut fell_back_to_object = false;
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                for method in &info.methods {
                    if method.kind == MethodKind::Primary
                        && method.selector.as_str() == selector
                        && method.is_sealed
                    {
                        return Some((info.name.clone(), method.clone()));
                    }
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                // Unknown/external class: fall back to Object to ensure we check
                // Object's sealed methods, then stop. This prevents sealed overrides
                // from being bypassed when an external class is in the inheritance chain.
                current = if fell_back_to_object {
                    None
                } else {
                    fell_back_to_object = true;
                    Some("Object".to_string())
                };
            }
        }
        None
    }

    /// Walk the superclass chain looking for a sealed primary class-side method with the given selector.
    /// Returns `Some((class_name, MethodInfo))` if found, `None` otherwise.
    pub(super) fn find_sealed_class_method_in_ancestors(
        &self,
        start_class: &str,
        selector: &str,
    ) -> Option<(EcoString, MethodInfo)> {
        let mut current = Some(start_class.to_string());
        let mut visited = HashSet::new();
        let mut fell_back_to_object = false;
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                for method in &info.class_methods {
                    if method.kind == MethodKind::Primary
                        && method.selector.as_str() == selector
                        && method.is_sealed
                    {
                        return Some((info.name.clone(), method.clone()));
                    }
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                // Unknown/external class: fall back to Object to ensure we check
                // Object's sealed methods, then stop. This prevents sealed overrides
                // from being bypassed when an external class is in the inheritance chain.
                current = if fell_back_to_object {
                    None
                } else {
                    fell_back_to_object = true;
                    Some("Object".to_string())
                };
            }
        }
        None
    }

    /// Applies inferred method return types to user-defined classes.
    ///
    /// Updates `MethodInfo.return_type` for methods that have no explicit annotation
    /// but whose return type was inferred by [`crate::semantic_analysis::infer_method_return_types`].
    ///
    /// This enriches the hierarchy so that type-aware completions and hover can
    /// resolve receiver types for chains involving methods on any class, including
    /// built-ins (BT-1014).
    /// Only methods without an existing `return_type` are updated (explicit annotations
    /// are never overwritten).
    ///
    /// # Arguments
    ///
    /// * `inferred` — map from `(ClassName, Selector, IsClassMethod)` to return type name,
    ///   as returned by `infer_method_return_types`.
    pub fn apply_inferred_return_types(
        &mut self,
        inferred: &HashMap<(EcoString, EcoString, bool), EcoString>,
    ) {
        for ((class_name, selector, is_class_method), return_type) in inferred {
            if let Some(class_info) = self.classes.get_mut(class_name) {
                let methods = if *is_class_method {
                    &mut class_info.class_methods
                } else {
                    &mut class_info.methods
                };
                if let Some(method) = methods
                    .iter_mut()
                    .find(|m| m.kind == MethodKind::Primary && m.selector == *selector)
                {
                    if method.return_type.is_none() {
                        method.return_type = Some(return_type.clone());
                    }
                }
            }
        }
    }
}
