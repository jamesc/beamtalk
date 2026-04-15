// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//! Static class hierarchy for compile-time method resolution.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module implements the `ClassHierarchy` domain service from ADR 0006 Phase 1a.
//! It provides compile-time knowledge of the full class hierarchy, consumed by:
//! - **Codegen**: method table generation, actor detection, fast-path decisions
//! - **LSP**: completions with inherited methods, hover info
//! - **Diagnostics**: sealed class enforcement, method existence validation
//!
//! The hierarchy uses simple depth-first MRO (no multiple inheritance).
use crate::ast::{ClassDefinition, MethodKind, Module};
use crate::compilation::extension_index::{ExtensionIndex, MethodSide};
use crate::semantic_analysis::SemanticError;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use ecow::EcoString;
use std::collections::{HashMap, HashSet};
use std::sync::OnceLock;
mod builtins;
pub(crate) mod class_info;
mod hierarchy_queries;
mod method_resolution;
#[cfg(test)]
mod tests;
pub use class_info::{ClassInfo, MethodInfo, SuperclassTypeArg};
/// Per-class selector index: maps class name → (selector → method vec position).
type SelectorIndexMap = HashMap<EcoString, HashMap<EcoString, usize>>;
/// Static class hierarchy built during semantic analysis.
///
/// Provides compile-time knowledge of the full class hierarchy,
/// including built-in classes and user-defined classes from the AST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassHierarchy {
    classes: HashMap<EcoString, ClassInfo>,
    /// Per-class selector → method-vec index for instance methods.
    /// Enables O(1) selector lookups instead of linear scans.
    method_indexes: SelectorIndexMap,
    /// Per-class selector → method-vec index for class methods.
    class_method_indexes: SelectorIndexMap,
    /// Names of synthetic protocol class entries (BT-1933).
    ///
    /// Tracked so `merge()` can prefer real class definitions over
    /// synthetic protocol entries when files define both.
    protocol_classes: HashSet<EcoString>,
}
impl ClassHierarchy {
    /// Returns true if the given class name is a built-in class.
    ///
    /// This is a fast O(1) check suitable for hot paths.
    #[must_use]
    pub fn is_builtin_class(name: &str) -> bool {
        builtins::is_builtin_class(name)
    }
    /// Returns true if the given class name has runtime shadowing protection.
    ///
    /// Only stdlib classes with the `bt@stdlib@` module prefix are protected
    /// at runtime. Runtime-only built-ins like `Future` are excluded (BT-750).
    #[must_use]
    pub fn is_runtime_protected_class(name: &str) -> bool {
        builtins::is_runtime_protected_class(name)
    }
    /// Build a selector-to-index map from a method vec.
    fn build_selector_index(methods: &[MethodInfo]) -> HashMap<EcoString, usize> {
        let mut index = HashMap::with_capacity(methods.len());
        for (i, m) in methods.iter().enumerate() {
            index.entry(m.selector.clone()).or_insert(i);
        }
        index
    }
    /// Build selector indexes for all classes in the map.
    fn build_all_indexes(
        classes: &HashMap<EcoString, ClassInfo>,
    ) -> (SelectorIndexMap, SelectorIndexMap) {
        let mut mi = HashMap::with_capacity(classes.len());
        let mut cmi = HashMap::with_capacity(classes.len());
        for (name, info) in classes {
            mi.insert(name.clone(), Self::build_selector_index(&info.methods));
            cmi.insert(
                name.clone(),
                Self::build_selector_index(&info.class_methods),
            );
        }
        (mi, cmi)
    }
    /// Rebuild selector indexes for all classes.
    ///
    /// Call after bulk mutations (e.g. `add_module_classes`, `merge`, `add_from_beam_meta`)
    /// so that `resolves_selector` / `find_method` use O(1) lookups.
    pub fn rebuild_all_indexes(&mut self) {
        let (mi, cmi) = Self::build_all_indexes(&self.classes);
        self.method_indexes = mi;
        self.class_method_indexes = cmi;
    }
    /// Build a class hierarchy from built-in definitions and a parsed module.
    ///
    /// Returns `(Ok(hierarchy), diagnostics)` where diagnostics may include
    /// errors (e.g., sealed class violations). The `Result` is always `Ok`
    /// since hierarchy construction is infallible; the `Result` wrapper follows
    /// the project convention for user-facing operations.
    pub fn build(module: &Module) -> (Result<Self, SemanticError>, Vec<Diagnostic>) {
        Self::build_with_options(module, false)
    }
    /// Build a class hierarchy with explicit stdlib compilation mode.
    ///
    /// When `stdlib_mode` is true, built-in classes (e.g. `Character`) are
    /// permitted to subclass sealed classes. This exemption is **not** granted
    /// based on class name alone — only when the compiler knows it is compiling
    /// stdlib sources (BT-791).
    ///
    /// Returns `(Ok(hierarchy), diagnostics)` per the project convention for
    /// user-facing operations that produce both a result and diagnostics.
    pub fn build_with_options(
        module: &Module,
        stdlib_mode: bool,
    ) -> (Result<Self, SemanticError>, Vec<Diagnostic>) {
        let mut hierarchy = Self::with_builtins();
        let diagnostics = hierarchy.add_module_classes(module, stdlib_mode);
        hierarchy.rebuild_all_indexes();
        (Ok(hierarchy), diagnostics)
    }
    /// Register protocol definitions as synthetic class entries (BT-1933).
    ///
    /// Each protocol definition (e.g., `protocol Printable`) gets a synthetic
    /// `ClassInfo` entry as a sealed abstract subclass of `Protocol`, with
    /// class-side methods `requiredMethods` and `conformingClasses`.
    ///
    /// This enables LSP features (completions, `has_class` checks) to work
    /// with protocol names the same way they work with class names.
    ///
    /// Called from the language service layer *after* `build()`, not during it,
    /// to avoid interfering with `ProtocolRegistry::register_module`'s
    /// namespace collision checks.
    pub fn register_protocol_classes(&mut self, module: &Module) {
        let mut inserted = false;
        for protocol_def in &module.protocols {
            let name = &protocol_def.name.name;
            // Don't overwrite real classes (namespace collision is diagnosed elsewhere)
            if self.classes.contains_key(name.as_str()) {
                continue;
            }
            let class_methods = vec![
                MethodInfo {
                    selector: "requiredMethods".into(),
                    arity: 0,
                    kind: MethodKind::Primary,
                    defined_in: name.clone(),
                    is_sealed: true,
                    is_internal: false,
                    spawns_block: false,
                    return_type: Some("List".into()),
                    param_types: vec![],
                    doc: Some(
                        format!("Return the required method selectors for the `{name}` protocol.")
                            .into(),
                    ),
                },
                MethodInfo {
                    selector: "conformingClasses".into(),
                    arity: 0,
                    kind: MethodKind::Primary,
                    defined_in: name.clone(),
                    is_sealed: true,
                    is_internal: false,
                    spawns_block: false,
                    return_type: Some("List".into()),
                    param_types: vec![],
                    doc: Some(
                        format!("Return the classes conforming to the `{name}` protocol.").into(),
                    ),
                },
            ];
            self.classes.insert(
                name.clone(),
                ClassInfo {
                    name: name.clone(),
                    superclass: Some("Protocol".into()),
                    is_sealed: true,
                    is_abstract: true,
                    is_typed: true,
                    is_internal: false,
                    package: None,
                    is_value: false,
                    is_native: false,
                    state: vec![],
                    state_types: HashMap::new(),
                    state_has_default: HashMap::new(),
                    methods: vec![],
                    class_methods,
                    class_variables: vec![],
                    type_params: vec![],
                    type_param_bounds: vec![],
                    superclass_type_args: vec![],
                },
            );
            self.protocol_classes.insert(name.clone());
            inserted = true;
        }
        if inserted {
            self.rebuild_all_indexes();
        }
    }
    /// Create a hierarchy with only built-in classes.
    ///
    /// The built-in class map is computed once and cached in a `OnceLock`.
    /// Each call clones the cached map so callers can add user-defined
    /// classes without mutating the shared original. (BT-1677)
    #[must_use]
    pub fn with_builtins() -> Self {
        static BUILTIN_HIERARCHY: OnceLock<ClassHierarchy> = OnceLock::new();
        BUILTIN_HIERARCHY
            .get_or_init(|| {
                let classes = builtins::builtin_classes();
                let (method_indexes, class_method_indexes) = Self::build_all_indexes(&classes);
                Self {
                    classes,
                    method_indexes,
                    class_method_indexes,
                    protocol_classes: HashSet::new(),
                }
            })
            .clone()
    }
    /// Look up a class by name.
    #[must_use]
    pub fn get_class(&self, name: &str) -> Option<&ClassInfo> {
        self.classes.get(name)
    }
    /// Check if a class exists in the hierarchy.
    #[must_use]
    pub fn has_class(&self, name: &str) -> bool {
        self.classes.contains_key(name)
    }
    /// Check if `name` is a synthetic protocol class entry (BT-1933).
    ///
    /// Returns `true` when `register_protocol_classes` inserted the entry for a
    /// protocol definition. Returns `false` for real classes (even if those
    /// classes extend `Protocol` directly) and for names that aren't in the
    /// hierarchy at all.
    ///
    /// Used by goto-definition to distinguish "name refers to a protocol" from
    /// "name refers to a class we haven't loaded a source file for" when a
    /// protocol and a real class share a name across files (BT-1936).
    #[must_use]
    pub fn is_protocol_class(&self, name: &str) -> bool {
        self.protocol_classes.contains(name)
    }
    /// Returns an iterator over all class names in the hierarchy.
    pub fn class_names(&self) -> impl Iterator<Item = &EcoString> {
        self.classes.keys()
    }
    /// BT-894: Add minimal class entries from a cross-file superclass index.
    ///
    /// For each class in the index that isn't already in the hierarchy, adds a
    /// stub `ClassInfo` with just the name and superclass. This allows
    /// `superclass_chain` to resolve the full inheritance chain for classes
    /// whose parents are defined in other files.
    ///
    /// BT-1528: After all entries are inserted, walks the ancestor chain for
    /// each new class to resolve `is_value` correctly for indirect subclasses.
    pub fn add_external_superclasses(&mut self, index: &std::collections::HashMap<String, String>) {
        // BT-1545: Track which classes are newly inserted so the is_value
        // fixup loop only walks them and their descendants, not everything.
        let mut newly_inserted: HashSet<EcoString> = HashSet::new();
        for (class_name, superclass_name) in index {
            if !self.classes.contains_key(class_name.as_str()) {
                let eco_name = EcoString::from(class_name.as_str());
                newly_inserted.insert(eco_name.clone());
                self.classes.insert(
                    eco_name,
                    ClassInfo {
                        name: EcoString::from(class_name.as_str()),
                        superclass: Some(EcoString::from(superclass_name.as_str())),
                        is_sealed: false,
                        is_abstract: false,
                        is_typed: false,
                        is_internal: false,
                        package: None,
                        // Temporarily set from direct superclass; fixed below.
                        is_value: superclass_name == "Value",
                        is_native: false,
                        state: Vec::new(),
                        state_types: HashMap::new(),
                        state_has_default: HashMap::new(),
                        methods: Vec::new(),
                        class_methods: Vec::new(),
                        class_variables: Vec::new(),
                        type_params: Vec::new(),
                        type_param_bounds: Vec::new(),
                        superclass_type_args: Vec::new(),
                    },
                );
            }
        }
        if newly_inserted.is_empty() {
            return;
        }
        // BT-1528: Fix is_value for indirect Value subclasses now that all
        // entries are in the hierarchy and superclass_chain can walk them.
        // BT-1545: Only check newly-inserted classes and existing classes
        // whose direct superclass is newly inserted (transitively). Use a
        // worklist to find all affected descendants without walking every
        // class in the hierarchy.
        let mut to_check: Vec<EcoString> = newly_inserted.iter().cloned().collect();
        // Find existing classes whose direct superclass is newly inserted,
        // and transitively their descendants too.
        let mut visited: HashSet<EcoString> = newly_inserted.clone();
        let mut i = 0;
        while i < to_check.len() {
            let parent = to_check[i].clone();
            i += 1;
            // Find all classes whose direct superclass is `parent`.
            for (name, info) in &self.classes {
                if !visited.contains(name) {
                    if let Some(ref sup) = info.superclass {
                        if sup == &parent {
                            visited.insert(name.clone());
                            to_check.push(name.clone());
                        }
                    }
                }
            }
        }
        for class_name in &to_check {
            let is_value = self.is_value_subclass(class_name.as_str());
            if let Some(info) = self.classes.get_mut(class_name.as_str()) {
                if is_value && !info.is_value {
                    info.is_value = true;
                }
            }
        }
    }
    /// Populate the hierarchy with user-class entries pre-deserialized from
    /// `__beamtalk_meta/0` maps (ADR 0050 Phase 4).
    ///
    /// Skips builtins (stdlib has richer data) and classes already present
    /// in the hierarchy (AST-derived entries from `build()` are authoritative
    /// over cached BEAM metadata, which may be stale during redefinition).
    pub fn add_from_beam_meta(&mut self, classes: Vec<ClassInfo>) {
        let mut changed = false;
        for info in classes {
            if !Self::is_builtin_class(&info.name) {
                if let std::collections::hash_map::Entry::Vacant(e) =
                    self.classes.entry(info.name.clone())
                {
                    e.insert(info);
                    changed = true;
                }
            }
        }
        if changed {
            self.rebuild_all_indexes();
        }
    }
    /// Set the `package` field on all non-builtin classes that don't already
    /// have a package assigned (ADR 0071, BT-1700).
    ///
    /// Called after `build_with_options` to stamp the current compilation unit's
    /// package name onto AST-derived classes. Classes loaded from BEAM metadata
    /// already carry their own package from the `__beamtalk_meta/0` map.
    pub fn stamp_package(&mut self, package: &str) {
        let pkg: EcoString = EcoString::from(package);
        for (name, info) in &mut self.classes {
            if info.package.is_none() && !Self::is_builtin_class(name) {
                info.package = Some(pkg.clone());
            }
        }
    }
    /// Extract `ClassInfo` entries from a parsed module without diagnostics.
    ///
    /// BT-1523: Same ClassInfo-building logic as `add_module_classes` Pass 1,
    /// but without duplicate-method checks or inserting into a hierarchy.
    /// Used by the build's Pass 1 to collect class metadata from all source files
    /// for cross-file hierarchy resolution in Pass 2.
    #[must_use]
    pub fn extract_class_infos(module: &Module) -> Vec<ClassInfo> {
        module
            .classes
            .iter()
            .map(ClassInfo::from_class_definition)
            .collect()
    }
    /// Filter `all_class_infos` to exclude classes defined in `module`,
    /// returning only cross-file class metadata suitable for injection into
    /// semantic analysis.
    ///
    /// Used by both `lint` and `build` to avoid duplicating classes that the
    /// current module's `ClassHierarchy::build()` will add from the AST.
    #[must_use]
    pub fn cross_file_class_infos(
        all_class_infos: &[ClassInfo],
        module: &Module,
    ) -> Vec<ClassInfo> {
        let current: std::collections::HashSet<&str> = module
            .classes
            .iter()
            .map(|c| c.name.name.as_str())
            .collect();
        all_class_infos
            .iter()
            .filter(|ci| !current.contains(ci.name.as_str()))
            .cloned()
            .collect()
    }
    /// Registers extension methods from the [`ExtensionIndex`] into the hierarchy.
    ///
    /// Each extension entry is converted to a [`MethodInfo`] and appended to the
    /// target class's instance or class method list. Unannotated parameters and
    /// return types remain `None` (Dynamic in the gradual type system), so the
    /// type checker will skip validation for those positions while still knowing
    /// the method exists.
    ///
    /// Extensions targeting classes not present in the hierarchy are silently
    /// skipped — they will be caught by the conflict detector or reported as
    /// errors elsewhere.
    ///
    /// For duplicate extensions (same key, multiple locations), only the first
    /// definition is registered. The conflict detector handles reporting errors
    /// for duplicates.
    pub fn register_extensions(&mut self, index: &ExtensionIndex) {
        let mut changed = false;
        for (key, locations) in index.entries() {
            // Only register the first definition; conflicts are handled separately.
            let Some(first) = locations.first() else {
                continue;
            };
            let method_info = MethodInfo {
                selector: key.selector.clone(),
                arity: first.type_info.arity,
                kind: MethodKind::Primary,
                defined_in: key.class_name.clone(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: first.type_info.return_type.clone(),
                param_types: first.type_info.param_types.clone(),
                doc: None,
            };
            if let Some(class_info) = self.classes.get_mut(key.class_name.as_str()) {
                let methods = match key.side {
                    MethodSide::Instance => &mut class_info.methods,
                    MethodSide::Class => &mut class_info.class_methods,
                };
                // Don't add if a method with the same selector already exists
                // (the class's own definition takes precedence over extensions).
                let already_exists = methods.iter().any(|m| m.selector == key.selector);
                if !already_exists {
                    methods.push(method_info);
                    changed = true;
                }
            }
        }
        if changed {
            self.rebuild_all_indexes();
        }
    }
    /// Returns a reference to the underlying class map.
    #[must_use]
    pub fn classes(&self) -> &HashMap<EcoString, ClassInfo> {
        &self.classes
    }
    /// Returns a mutable reference to the underlying class map.
    ///
    /// Used by `ProjectIndex` for conflict resolution when re-merging.
    ///
    /// **Important:** After mutating the map, call [`rebuild_all_indexes()`](Self::rebuild_all_indexes)
    /// to keep selector indexes consistent with the class data.
    pub fn classes_mut(&mut self) -> &mut HashMap<EcoString, ClassInfo> {
        &mut self.classes
    }
    /// Merge another hierarchy's user-defined classes into this one.
    ///
    /// Built-in classes from `other` are skipped (they already exist in `self`).
    /// User-defined classes from `other` overwrite any existing entry with the
    /// same name, allowing incremental file updates.
    ///
    /// BT-1933: Synthetic protocol class entries never overwrite real class
    /// definitions. A real incoming class *does* overwrite a synthetic protocol.
    pub fn merge(&mut self, other: &ClassHierarchy) {
        let mut changed = false;
        for (name, info) in &other.classes {
            if builtins::is_builtin_class(name) {
                continue;
            }
            let incoming_is_protocol = other.protocol_classes.contains(name);
            let existing_is_real =
                self.classes.contains_key(name) && !self.protocol_classes.contains(name);
            // Don't let a synthetic protocol entry overwrite a real class
            if incoming_is_protocol && existing_is_real {
                continue;
            }
            // If a real class replaces a synthetic protocol, unmark it
            if !incoming_is_protocol && self.protocol_classes.contains(name) {
                self.protocol_classes.remove(name);
            }
            // Track synthetic protocol entries
            if incoming_is_protocol {
                self.protocol_classes.insert(name.clone());
            }
            self.classes.insert(name.clone(), info.clone());
            changed = true;
        }
        if changed {
            self.rebuild_all_indexes();
        }
    }
    /// Remove all classes that were defined in the given set of class names.
    ///
    /// Built-in classes are never removed.
    pub fn remove_classes(&mut self, names: &[EcoString]) {
        for name in names {
            if !builtins::is_builtin_class(name) {
                self.classes.remove(name);
                self.method_indexes.remove(name);
                self.class_method_indexes.remove(name);
                self.protocol_classes.remove(name);
            }
        }
    }
    /// Check for duplicate methods within a list of methods.
    ///
    /// Methods are considered duplicates based on their `(selector, kind)` tuple.
    fn check_duplicate_methods(
        methods: &[crate::ast::MethodDefinition],
        class_name: &EcoString,
        is_class_method: bool,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        use crate::ast::MethodKind;
        let mut seen: HashMap<(EcoString, MethodKind), crate::source_analysis::Span> =
            HashMap::new();
        let label = if is_class_method {
            "class method"
        } else {
            "method"
        };
        for method in methods {
            let selector = method.selector.name();
            let key = (selector.clone(), method.kind);
            if let Some(&first_span) = seen.get(&key) {
                diagnostics.push(
                    Diagnostic::error(
                        format!("Duplicate {label} `{selector}` in class `{class_name}`"),
                        method.span,
                    )
                    .with_hint(format!(
                        "A {label} with this selector is already defined at offset {}",
                        first_span.start()
                    ))
                    .with_category(DiagnosticCategory::Type),
                );
            } else {
                seen.insert(key, method.span);
            }
        }
    }
    /// Check whether a class is allowed to subclass its declared superclass.
    ///
    /// Returns a diagnostic if the superclass is sealed and the subclass is not
    /// exempt. Exemptions are only granted to built-in classes when compiling
    /// in stdlib mode (BT-791).
    ///
    /// Note: This gates on `stdlib_mode` (compilation context) rather than
    /// class names. Runtime-protected classes (BT-778) are an additional layer
    /// that applies at runtime; this compile-time check is stricter.
    fn check_sealed_superclass(
        &self,
        class: &ClassDefinition,
        stdlib_mode: bool,
    ) -> Option<Diagnostic> {
        let superclass = class.superclass.as_ref()?;
        let super_info = self.classes.get(superclass.name.as_str())?;
        if super_info.can_be_subclassed() {
            return None;
        }
        // BT-791: Only exempt built-in classes when compiling stdlib.
        // A user-defined class with the same name (e.g. `Character`)
        // must still respect sealed enforcement.
        if stdlib_mode && Self::is_builtin_class(class.name.name.as_str()) {
            return None;
        }
        Some(
            Diagnostic::error(
                format!("Cannot subclass sealed class `{}`", superclass.name),
                superclass.span,
            )
            .with_hint(format!(
                "Class `{}` is sealed and cannot be extended — use composition instead",
                superclass.name
            )),
        )
    }
    /// Add classes from a parsed module. Returns diagnostics for errors.
    ///
    /// Uses a two-pass approach to avoid definition-order sensitivity:
    /// Pass 1 registers all classes so ancestor lookups are complete;
    /// Pass 2 runs sealed-override and superclass checks.
    #[allow(clippy::too_many_lines)]
    fn add_module_classes(&mut self, module: &Module, stdlib_mode: bool) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        // Pass 1: Build and register ClassInfo for every class in the module.
        // Duplicate-selector checks only need the class's own methods, so they
        // run here too. Sealed checks are deferred to Pass 2.
        for class in &module.classes {
            // Check for duplicate method selectors (no ancestor lookup needed)
            Self::check_duplicate_methods(
                &class.methods,
                &class.name.name,
                false,
                &mut diagnostics,
            );
            Self::check_duplicate_methods(
                &class.class_methods,
                &class.name.name,
                true,
                &mut diagnostics,
            );
            let class_info = ClassInfo::from_class_definition(class);
            self.classes.insert(class.name.name.clone(), class_info);
        }
        // Pass 1.5 (BT-1528): Propagate ClassKind through the hierarchy.
        // Classes whose direct superclass is not "Actor"/"Value" but whose
        // ancestors include Actor or Value need their is_value flag corrected
        // and auto-slot methods synthesized.
        self.propagate_class_kind(module);
        // Pass 2: All classes in this module are now registered. Run sealed
        // superclass and method-override checks so ancestor lookups are
        // deterministic regardless of class definition order within the module.
        for class in &module.classes {
            // Check sealed class enforcement (skip for root classes with no superclass).
            // Still register the class so downstream passes (codegen routing,
            // completions) can reason about it despite the error.
            if let Some(diag) = self.check_sealed_superclass(class, stdlib_mode) {
                diagnostics.push(diag);
            }
            // Check sealed method override enforcement.
            // BT-803: Built-in stdlib classes (e.g. Metaclass) are allowed to override
            // sealed methods when compiling in stdlib mode.
            // BT-807: Abstract stdlib classes (e.g. Behaviour) are also allowed to override
            // sealed methods, as they provide class-side dispatch methods whose
            // selectors coincide with sealed instance-side methods on Object, but are
            // dispatched through a separate runtime namespace.
            if let Some(ref superclass) = class.superclass {
                let is_stdlib_builtin =
                    stdlib_mode && Self::is_builtin_class(class.name.name.as_str());
                let is_abstract_stdlib = stdlib_mode && class.is_abstract;
                // Instance-method sealed checks: exempt builtin AND abstract stdlib classes.
                if !is_stdlib_builtin && !is_abstract_stdlib {
                    for method in class
                        .methods
                        .iter()
                        .filter(|m| m.kind == MethodKind::Primary)
                    {
                        let selector = method.selector.name();
                        if let Some((sealed_class, _)) = self
                            .find_sealed_method_in_ancestors(superclass.name.as_str(), &selector)
                        {
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "Cannot override sealed method `{selector}` from class `{sealed_class}`"
                                ),
                                method.span,
                            ).with_hint(format!("Method `{selector}` is sealed in `{sealed_class}` — use a different method name")));
                        }
                    }
                }
                // Class-method sealed checks: only exempt builtin stdlib classes.
                // Abstract stdlib classes must still respect sealed class-method overrides.
                if !is_stdlib_builtin {
                    for method in class
                        .class_methods
                        .iter()
                        .filter(|m| m.kind == MethodKind::Primary)
                    {
                        let selector = method.selector.name();
                        if let Some((sealed_class, _)) = self.find_sealed_class_method_in_ancestors(
                            superclass.name.as_str(),
                            &selector,
                        ) {
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "Cannot override sealed method `{selector}` from class `{sealed_class}`"
                                ),
                                method.span,
                            ).with_hint(format!("Method `{selector}` is sealed in `{sealed_class}` — use a different method name")));
                        }
                    }
                }
            }
        }
        diagnostics
    }
}
impl Default for ClassHierarchy {
    fn default() -> Self {
        Self::with_builtins()
    }
}
