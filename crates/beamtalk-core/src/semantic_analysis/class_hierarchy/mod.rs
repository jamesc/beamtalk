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

use crate::ast::{
    ClassDefinition, ClassKind, Expression, Literal, MethodKind, Module, TypeAnnotation,
};
use crate::compilation::extension_index::{ExtensionIndex, MethodSide};
use crate::semantic_analysis::SemanticError;
use crate::source_analysis::Diagnostic;
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

mod builtins;

/// Information about a method in the hierarchy.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodInfo {
    /// Full selector name (e.g., "at:put:", "+", "size").
    pub selector: EcoString,
    /// Number of arguments.
    pub arity: usize,
    /// Method kind.
    pub kind: MethodKind,
    /// Class that defines this method.
    pub defined_in: EcoString,
    /// Whether this method is sealed (cannot be overridden).
    pub is_sealed: bool,
    /// Whether this method spawns its block argument in a separate BEAM process.
    ///
    /// When `true`, self-sends inside the block are safe for Actor classes
    /// because they execute in a different process and cannot re-enter the
    /// actor's own call-stack (`calling_self` dispatch does not apply).
    pub spawns_block: bool,
    /// Inferred return type (e.g., "Integer", "String", "Boolean").
    /// `None` means the return type is unknown (Dynamic).
    pub return_type: Option<EcoString>,
    /// Parameter type annotations (e.g., `vec![Some("Number")]` for `+ other: Number`).
    /// Empty for unary methods. `None` elements mean the parameter type is unknown.
    pub param_types: Vec<Option<EcoString>>,
    /// Documentation string for this method.
    /// Populated for compiler-synthesized methods and stdlib methods with `///` doc comments;
    /// `None` for user-written methods without doc comments.
    pub doc: Option<EcoString>,
}

impl MethodInfo {
    /// Returns `true` if `self` (a subclass method) can override `ancestor`.
    ///
    /// A method cannot override a sealed method with the same selector.
    /// Different selectors are never overrides.
    #[must_use]
    pub fn can_override(&self, ancestor: &MethodInfo) -> bool {
        if self.selector != ancestor.selector {
            return false;
        }
        !ancestor.is_sealed
    }
}

/// How a subclass maps one of its type parameters (or a concrete type) to a
/// superclass type parameter position.
///
/// Used in `ClassInfo::superclass_type_args` to track the mapping established by
/// `Collection(E) subclass: Array(E)` or `Collection(Integer) subclass: IntArray`.
///
/// **References:** ADR 0068 Challenge 4
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SuperclassTypeArg {
    /// The subclass forwards one of its own type params to the superclass.
    ///
    /// Example: `Collection(E) subclass: Array(E)` — Array's `E` (at `param_index` 0)
    /// maps to Collection's position 0.
    ParamRef {
        /// Index into this class's `type_params` vec.
        param_index: usize,
    },
    /// A concrete (fixed) type is supplied to the superclass.
    ///
    /// Example: `Collection(Integer) subclass: IntArray` — `Integer` is fixed.
    Concrete {
        /// The concrete type name (e.g., "Integer").
        type_name: EcoString,
    },
}

/// Information about a class in the hierarchy.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::struct_excessive_bools)]
pub struct ClassInfo {
    /// Class name.
    pub name: EcoString,
    /// Superclass name (`None` for `ProtoObject`).
    pub superclass: Option<EcoString>,
    /// Whether this class is sealed (cannot be subclassed).
    pub is_sealed: bool,
    /// Whether this class is abstract.
    pub is_abstract: bool,
    /// Whether this class has the explicit `typed` modifier.
    /// Use `ClassHierarchy::is_typed()` to check inherited typed status.
    pub is_typed: bool,
    /// Whether this class is a Value subclass (ADR 0042, BT-1528).
    ///
    /// `true` for classes that directly or indirectly inherit from `Value`.
    /// Initially set from the direct superclass name, then corrected by
    /// [`ClassHierarchy::propagate_class_kind`] after the full hierarchy is built.
    pub is_value: bool,
    /// Whether this class is a native Actor (declared with `native:` keyword, ADR 0056).
    ///
    /// Native actors delegate to a backing Erlang module and cannot declare
    /// `state:` fields (state is owned by the Erlang `gen_server`).
    pub is_native: bool,
    /// State (instance variable) names.
    pub state: Vec<EcoString>,
    /// Declared type annotations for state fields (field name → type name).
    /// Only populated for fields with explicit type annotations.
    pub state_types: HashMap<EcoString, EcoString>,
    /// Methods defined directly on this class (instance-side).
    pub methods: Vec<MethodInfo>,
    /// Class-side methods defined on this class.
    pub class_methods: Vec<MethodInfo>,
    /// Class variable names (declared with `classState:`).
    pub class_variables: Vec<EcoString>,
    /// Type parameters for generic classes (e.g., `["T", "E"]` for `Result(T, E)`).
    ///
    /// Empty for non-generic classes.
    pub type_params: Vec<EcoString>,
    /// How this class's type params (or concrete types) map to the superclass's type params.
    ///
    /// Empty when the superclass is not generic or no type args are applied.
    ///
    /// Example: `Collection(E) subclass: Array(E)` → `[ParamRef { param_index: 0 }]`
    /// Example: `Collection(Integer) subclass: IntArray` → `[Concrete { type_name: "Integer" }]`
    ///
    /// **References:** ADR 0068 Challenge 4
    pub superclass_type_args: Vec<SuperclassTypeArg>,
}

impl ClassInfo {
    /// Returns `true` if this class can be subclassed.
    #[must_use]
    pub fn can_be_subclassed(&self) -> bool {
        !self.is_sealed
    }

    /// Build a `ClassInfo` from a parsed `ClassDefinition` AST node.
    ///
    /// Shared by `extract_class_infos` and `add_module_classes` to avoid
    /// duplicating the field-mapping logic.
    fn from_class_definition(class: &ClassDefinition) -> Self {
        let mut instance_methods: Vec<MethodInfo> = class
            .methods
            .iter()
            .map(|m| MethodInfo {
                selector: m.selector.name(),
                arity: m.selector.arity(),
                kind: m.kind,
                defined_in: class.name.name.clone(),
                is_sealed: m.is_sealed,
                spawns_block: false,
                return_type: m.return_type.as_ref().map(TypeAnnotation::type_name),
                param_types: m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.as_ref().map(TypeAnnotation::type_name))
                    .collect(),
                doc: m.doc_comment.clone().map(Into::into),
            })
            .collect();

        let mut class_methods: Vec<MethodInfo> = class
            .class_methods
            .iter()
            .map(|m| MethodInfo {
                selector: m.selector.name(),
                arity: m.selector.arity(),
                kind: m.kind,
                defined_in: class.name.name.clone(),
                is_sealed: m.is_sealed,
                spawns_block: false,
                return_type: m.return_type.as_ref().map(TypeAnnotation::type_name),
                param_types: m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.as_ref().map(TypeAnnotation::type_name))
                    .collect(),
                doc: m.doc_comment.clone().map(Into::into),
            })
            .collect();

        // BT-923: For `Value subclass:` classes, synthesize auto-generated slot
        // methods so the type checker and cross-file consumers can resolve them.
        if class.class_kind == ClassKind::Value {
            ClassHierarchy::add_value_auto_methods(
                class,
                &mut instance_methods,
                &mut class_methods,
            );
        }

        Self {
            name: class.name.name.clone(),
            superclass: class.superclass.as_ref().map(|s| s.name.clone()),
            is_sealed: class.is_sealed,
            is_abstract: class.is_abstract,
            is_typed: class.is_typed,
            is_value: class.class_kind == ClassKind::Value,
            is_native: class.backing_module.is_some(),
            state: class.state.iter().map(|s| s.name.name.clone()).collect(),
            state_types: class
                .state
                .iter()
                .filter_map(|s| {
                    s.type_annotation
                        .as_ref()
                        .map(|ty| (s.name.name.clone(), ty.type_name()))
                })
                .collect(),
            methods: instance_methods,
            class_methods,
            class_variables: class
                .class_variables
                .iter()
                .map(|cv| cv.name.name.clone())
                .collect(),
            type_params: class.type_params.iter().map(|tp| tp.name.clone()).collect(),
            superclass_type_args: {
                let own_params: Vec<EcoString> =
                    class.type_params.iter().map(|tp| tp.name.clone()).collect();
                class
                    .superclass_type_args
                    .iter()
                    .map(|ta| {
                        let name = ta.type_name();
                        if let Some(idx) = own_params.iter().position(|p| p == &name) {
                            SuperclassTypeArg::ParamRef { param_index: idx }
                        } else {
                            SuperclassTypeArg::Concrete { type_name: name }
                        }
                    })
                    .collect()
            },
        }
    }
}

/// Static class hierarchy built during semantic analysis.
///
/// Provides compile-time knowledge of the full class hierarchy,
/// including built-in classes and user-defined classes from the AST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassHierarchy {
    classes: HashMap<EcoString, ClassInfo>,
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
        (Ok(hierarchy), diagnostics)
    }

    /// Create a hierarchy with only built-in classes.
    #[must_use]
    pub fn with_builtins() -> Self {
        Self {
            classes: builtins::builtin_classes(),
        }
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

    /// Returns an iterator over all class names in the hierarchy.
    pub fn class_names(&self) -> impl Iterator<Item = &EcoString> {
        self.classes.keys()
    }

    /// Returns true if the named class is abstract (cannot be instantiated).
    #[must_use]
    pub fn is_abstract(&self, name: &str) -> bool {
        self.classes.get(name).is_some_and(|info| info.is_abstract)
    }

    /// Returns true if the named class requires type annotations (typed modifier
    /// on itself or any ancestor). Walks the superclass chain on demand.
    #[must_use]
    pub fn is_typed(&self, name: &str) -> bool {
        if self.classes.get(name).is_some_and(|info| info.is_typed) {
            return true;
        }
        self.superclass_chain(name).iter().any(|s| {
            self.classes
                .get(s.as_str())
                .is_some_and(|info| info.is_typed)
        })
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
                        // Temporarily set from direct superclass; fixed below.
                        is_value: superclass_name == "Value",
                        is_native: false,
                        state: Vec::new(),
                        state_types: HashMap::new(),
                        methods: Vec::new(),
                        class_methods: Vec::new(),
                        class_variables: Vec::new(),
                        type_params: Vec::new(),
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
        for info in classes {
            if !Self::is_builtin_class(&info.name) {
                self.classes.entry(info.name.clone()).or_insert(info);
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
                }
            }
        }
    }

    /// Returns the ordered superclass chain for a class (excluding the class itself).
    ///
    /// Example: `superclass_chain("Counter")` → `["Actor", "Object", "ProtoObject"]`
    ///
    /// Handles cycles gracefully by tracking visited classes.
    #[must_use]
    pub fn superclass_chain(&self, class_name: &str) -> Vec<EcoString> {
        let mut chain = Vec::new();
        let mut current = class_name.to_string();
        let mut visited = HashSet::new();
        visited.insert(class_name.to_string());

        loop {
            let Some(info) = self.classes.get(current.as_str()) else {
                break;
            };
            let Some(ref superclass) = info.superclass else {
                break;
            };
            if !visited.insert(superclass.to_string()) {
                break; // Cycle detected
            }
            chain.push(superclass.clone());
            current = superclass.to_string();
        }

        chain
    }

    /// Returns true if the named class is Actor or a subclass of Actor.
    #[must_use]
    pub fn is_actor_subclass(&self, class_name: &str) -> bool {
        if class_name == "Actor" {
            return true;
        }
        self.superclass_chain(class_name)
            .iter()
            .any(|s| s.as_str() == "Actor")
    }

    /// Returns true if the named class is Supervisor or a subclass of Supervisor (BT-1218).
    #[must_use]
    pub fn is_supervisor_subclass(&self, class_name: &str) -> bool {
        if class_name == "Supervisor" {
            return true;
        }
        self.superclass_chain(class_name)
            .iter()
            .any(|s| s.as_str() == "Supervisor")
    }

    /// Returns true if the named class is Server or a subclass of Server (ADR 0065).
    #[must_use]
    pub fn is_server_subclass(&self, class_name: &str) -> bool {
        if class_name == "Server" {
            return true;
        }
        self.superclass_chain(class_name)
            .iter()
            .any(|s| s.as_str() == "Server")
    }

    /// Returns true if the named class is `DynamicSupervisor` or a subclass (BT-1218).
    #[must_use]
    pub fn is_dynamic_supervisor_subclass(&self, class_name: &str) -> bool {
        if class_name == "DynamicSupervisor" {
            return true;
        }
        self.superclass_chain(class_name)
            .iter()
            .any(|s| s.as_str() == "DynamicSupervisor")
    }

    /// Returns true if the named class is a native Actor (ADR 0056).
    ///
    /// Native actors are declared with `native: module_name` and delegate
    /// to a backing Erlang module. They cannot declare `state:` fields.
    #[must_use]
    pub fn is_native(&self, class_name: &str) -> bool {
        self.classes
            .get(class_name)
            .is_some_and(|info| info.is_native)
    }

    /// Returns true if any ancestor in the class's superclass chain is not in
    /// the hierarchy (defined in a separately-compiled file).
    ///
    /// Walks the full chain transitively: `GrandChild → Child → (external Parent)`
    /// returns true for `GrandChild` even though `Child` is known locally.
    ///
    /// When true, the type checker cannot know the full method set and should
    /// suppress "does not understand" hints for selectors that might be inherited.
    #[must_use]
    pub fn has_cross_file_parent(&self, class_name: &str) -> bool {
        let mut current = class_name;
        let mut visited = HashSet::new();

        while let Some(info) = self.classes.get(current) {
            let Some(ref superclass) = info.superclass else {
                return false; // Reached root — fully known chain
            };

            if !visited.insert(current.to_string()) {
                return false; // Cycle guard
            }

            // First unknown ancestor means the chain is cross-file/incomplete.
            if !self.classes.contains_key(superclass.as_str()) {
                return true;
            }

            current = superclass.as_str();
        }

        false
    }

    /// Returns true if the named class is Value or a subclass of Value (ADR 0042).
    ///
    /// Value subclasses use immutable value semantics — `self.slot :=` is a
    /// compile error inside their methods. Use `self withSlot: newValue` instead.
    #[must_use]
    pub fn is_value_subclass(&self, class_name: &str) -> bool {
        if class_name == "Value" {
            return true;
        }
        self.superclass_chain(class_name)
            .iter()
            .any(|s| s.as_str() == "Value")
    }

    /// Returns true if the named class is `TestCase` or a subclass of `TestCase` (BT-1533).
    ///
    /// `TestCase` is a Value subclass with assertion methods that intentionally
    /// return Nil (side-effecting by design). This check allows validators to
    /// exempt `TestCase` subclasses from the Value `-> Nil` lint.
    #[must_use]
    pub fn is_testcase_subclass(&self, class_name: &str) -> bool {
        if class_name == "TestCase" {
            return true;
        }
        self.superclass_chain(class_name)
            .iter()
            .any(|s| s.as_str() == "TestCase")
    }

    /// Resolve the `ClassKind` for a class by walking its ancestor chain (BT-1528).
    ///
    /// Returns `ClassKind::Actor` if any ancestor is Actor, `ClassKind::Value` if
    /// any ancestor is Value, or `ClassKind::Object` otherwise.
    ///
    /// This supersedes `ClassKind::from_superclass_name` which only checks the
    /// direct superclass literal.
    #[must_use]
    pub fn resolve_class_kind(&self, class_name: &str) -> ClassKind {
        if self.is_actor_subclass(class_name) {
            ClassKind::Actor
        } else if self.is_value_subclass(class_name) {
            ClassKind::Value
        } else {
            ClassKind::Object
        }
    }

    /// BT-1540: Check whether a class defines its own class-side method with the given selector.
    ///
    /// Returns `true` if the class has a class method matching `selector` defined
    /// directly on it (not inherited). Used to exempt Object-kind classes that
    /// define their own factory constructors (e.g. `class new:` on `AtomicCounter`).
    #[must_use]
    pub fn has_own_class_method(&self, class_name: &str, selector: &str) -> bool {
        self.classes.get(class_name).is_some_and(|info| {
            info.class_methods
                .iter()
                .any(|m| m.selector.as_str() == selector)
        })
    }

    /// BT-1528: After all classes in a module are registered, propagate `is_value`
    /// by walking each class's ancestor chain. Classes that indirectly inherit
    /// from Value (e.g. `Value subclass: MyBase` then `MyBase subclass: MyChild`)
    /// get their `is_value` flag corrected and their auto-slot methods
    /// synthesized.
    pub(crate) fn propagate_class_kind(&mut self, module: &Module) {
        // Collect names of classes that need is_value = true but don't have it yet.
        let classes_to_fix: Vec<EcoString> = module
            .classes
            .iter()
            .filter(|class| {
                class.class_kind != ClassKind::Value
                    && self.is_value_subclass(class.name.name.as_str())
            })
            .map(|class| class.name.name.clone())
            .collect();

        for class_name in &classes_to_fix {
            if let Some(info) = self.classes.get_mut(class_name.as_str()) {
                info.is_value = true;
            }
        }

        // Synthesize auto-slot methods for newly-discovered Value subclasses.
        // We need the AST `ClassDefinition` for slot info, so look it up.
        for class in &module.classes {
            if classes_to_fix.contains(&class.name.name) {
                if let Some(info) = self.classes.get_mut(class.name.name.as_str()) {
                    Self::add_value_auto_methods(class, &mut info.methods, &mut info.class_methods);
                }
            }
        }
    }

    /// BT-1559: Re-propagate `is_value` for ALL classes in the hierarchy,
    /// including cross-file classes injected via `add_from_beam_meta`.
    ///
    /// Unlike `propagate_class_kind` which only processes the current module's
    /// AST classes, this method uses the already-stored `ClassInfo::state` to
    /// synthesize auto-slot methods for newly-discovered Value subclasses.
    pub(crate) fn propagate_cross_file_class_kind(&mut self) {
        // Pass 1: Collect all classes that need is_value fixup.
        let classes_to_fix: Vec<EcoString> = self
            .classes
            .iter()
            .filter(|(_, info)| {
                !info.is_value && self.ancestors_include_value(info.superclass.as_ref())
            })
            .map(|(name, _)| name.clone())
            .collect();

        if classes_to_fix.is_empty() {
            return;
        }

        // Pass 2: Set is_value flag.
        for class_name in &classes_to_fix {
            if let Some(info) = self.classes.get_mut(class_name.as_str()) {
                info.is_value = true;
            }
        }

        // Pass 3: Synthesize auto-slot methods from stored state fields.
        // We don't have the AST, so we build synthetic slot info from ClassInfo.
        for class_name in &classes_to_fix {
            // Extract state and existing selectors without holding a mutable borrow.
            let slot_data: Option<Vec<(EcoString, Option<EcoString>)>> =
                self.classes.get(class_name.as_str()).map(|info| {
                    info.state
                        .iter()
                        .map(|s| {
                            let ty = info.state_types.get(s).cloned();
                            (s.clone(), ty)
                        })
                        .collect()
                });
            let existing_instance: Option<HashSet<EcoString>> = self
                .classes
                .get(class_name.as_str())
                .map(|info| info.methods.iter().map(|m| m.selector.clone()).collect());
            let existing_class: Option<HashSet<EcoString>> =
                self.classes.get(class_name.as_str()).map(|info| {
                    info.class_methods
                        .iter()
                        .map(|m| m.selector.clone())
                        .collect()
                });

            if let (Some(slots), Some(inst_sels), Some(cls_sels)) =
                (slot_data, existing_instance, existing_class)
            {
                let info = self.classes.get_mut(class_name.as_str()).unwrap();
                Self::synthesize_auto_methods_from_state(
                    class_name,
                    &slots,
                    &inst_sels,
                    &cls_sels,
                    &mut info.methods,
                    &mut info.class_methods,
                );
            }
        }
    }

    /// Check if the superclass chain includes Value.
    fn ancestors_include_value(&self, superclass: Option<&EcoString>) -> bool {
        let mut current = superclass.map(ToString::to_string);
        let mut visited = HashSet::new();
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if name == "Value" {
                return true;
            }
            current = self
                .classes
                .get(name.as_str())
                .and_then(|info| info.superclass.as_ref().map(ToString::to_string));
        }
        false
    }

    /// Synthesize auto-generated slot methods from `ClassInfo` state fields.
    ///
    /// This is the `ClassInfo`-only equivalent of `add_value_auto_methods` which
    /// works from the AST `ClassDefinition`.
    fn synthesize_auto_methods_from_state(
        class_name: &EcoString,
        slots: &[(EcoString, Option<EcoString>)],
        existing_instance_selectors: &HashSet<EcoString>,
        existing_class_selectors: &HashSet<EcoString>,
        instance_methods: &mut Vec<MethodInfo>,
        class_methods: &mut Vec<MethodInfo>,
    ) {
        for (slot_name, slot_type) in slots {
            // Auto-getter
            if !existing_instance_selectors.contains(slot_name) {
                instance_methods.push(MethodInfo {
                    selector: slot_name.clone(),
                    arity: 0,
                    kind: MethodKind::Primary,
                    defined_in: class_name.clone(),
                    is_sealed: false,
                    spawns_block: false,
                    return_type: slot_type.clone(),
                    param_types: vec![],
                    doc: Some(EcoString::from("*(compiler-generated)*")),
                });
            }

            // Auto with*: setter
            let with_sel = {
                let mut chars = slot_name.chars();
                match chars.next() {
                    None => EcoString::from("with:"),
                    Some(first) => {
                        let cap: String = first.to_uppercase().collect();
                        EcoString::from(format!("with{}{}:", cap, chars.as_str()))
                    }
                }
            };
            if !existing_instance_selectors.contains(&with_sel) {
                instance_methods.push(MethodInfo {
                    selector: with_sel,
                    arity: 1,
                    kind: MethodKind::Primary,
                    defined_in: class_name.clone(),
                    is_sealed: false,
                    spawns_block: false,
                    return_type: Some(class_name.clone()),
                    param_types: vec![None],
                    doc: Some(EcoString::from("*(compiler-generated)*")),
                });
            }
        }

        // Keyword constructor (class method)
        if !slots.is_empty() {
            let kw_sel: EcoString = {
                let mut s = String::new();
                for (name, _) in slots {
                    s.push_str(name);
                    s.push(':');
                }
                EcoString::from(s)
            };
            if !existing_class_selectors.contains(&kw_sel) {
                class_methods.push(MethodInfo {
                    selector: kw_sel,
                    arity: slots.len(),
                    kind: MethodKind::Primary,
                    defined_in: class_name.clone(),
                    is_sealed: false,
                    spawns_block: false,
                    return_type: Some(class_name.clone()),
                    param_types: vec![None; slots.len()],
                    doc: Some(EcoString::from("*(compiler-generated)*")),
                });
            }
        }
    }

    /// Returns true if `class_name` is a numeric type (Integer, Float, Number,
    /// or any subclass thereof, e.g. Character which inherits from Integer).
    #[must_use]
    pub fn is_numeric_type(&self, class_name: &str) -> bool {
        matches!(class_name, "Integer" | "Float" | "Number")
            || self
                .superclass_chain(class_name)
                .iter()
                .any(|s| matches!(s.as_str(), "Integer" | "Float" | "Number"))
    }

    /// Returns all class variable names for a class, including inherited ones.
    #[must_use]
    pub fn class_variable_names(&self, class_name: &str) -> Vec<EcoString> {
        let mut vars = Vec::new();
        let mut visited = HashSet::new();
        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                vars.extend(info.class_variables.iter().cloned());
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }
        vars
    }

    /// Returns all state (instance variable) names for a class,
    /// including inherited state from the superclass chain.
    #[must_use]
    pub fn all_state(&self, class_name: &str) -> Vec<EcoString> {
        let mut state = Vec::new();
        let mut visited = HashSet::new();
        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                state.extend(info.state.iter().cloned());
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }
        state
    }

    /// Returns the declared type annotation for a state field, walking
    /// the superclass chain to find inherited field types.
    ///
    /// Returns `None` if the field has no type annotation, the field does not
    /// exist, or the class is unknown.
    #[must_use]
    pub fn state_field_type(&self, class_name: &str, field_name: &str) -> Option<EcoString> {
        let mut visited = HashSet::new();
        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                // If this class declares the field, return its type (or None if untyped).
                // This handles shadowing: a subclass redeclaring a field without a type
                // should NOT inherit the parent's type annotation.
                if info.state.iter().any(|s| s == field_name) {
                    return info.state_types.get(field_name).cloned();
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }
        None
    }

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
                if info.methods.iter().any(|m| m.selector.as_str() == selector) {
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
                if let Some(method) = info
                    .methods
                    .iter()
                    .find(|m| m.selector.as_str() == selector && m.kind == MethodKind::Primary)
                {
                    return Some(method.clone());
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
                if let Some(method) = info
                    .class_methods
                    .iter()
                    .find(|m| m.selector.as_str() == selector && m.kind == MethodKind::Primary)
                {
                    return Some(method.clone());
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
    fn find_sealed_method_in_ancestors(
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
    fn find_sealed_class_method_in_ancestors(
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

    /// Returns a reference to the underlying class map.
    #[must_use]
    pub fn classes(&self) -> &HashMap<EcoString, ClassInfo> {
        &self.classes
    }

    /// Returns a mutable reference to the underlying class map.
    ///
    /// Used by `ProjectIndex` for conflict resolution when re-merging.
    pub fn classes_mut(&mut self) -> &mut HashMap<EcoString, ClassInfo> {
        &mut self.classes
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

    /// Merge another hierarchy's user-defined classes into this one.
    ///
    /// Built-in classes from `other` are skipped (they already exist in `self`).
    /// User-defined classes from `other` overwrite any existing entry with the
    /// same name, allowing incremental file updates.
    pub fn merge(&mut self, other: &ClassHierarchy) {
        for (name, info) in &other.classes {
            if builtins::is_builtin_class(name) {
                continue;
            }
            self.classes.insert(name.clone(), info.clone());
        }
    }

    /// Remove all classes that were defined in the given set of class names.
    ///
    /// Built-in classes are never removed.
    pub fn remove_classes(&mut self, names: &[EcoString]) {
        for name in names {
            if !builtins::is_builtin_class(name) {
                self.classes.remove(name);
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
                    )),
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
        Some(Diagnostic::error(
            format!("Cannot subclass sealed class `{}`", superclass.name),
            superclass.span,
        ))
    }

    /// Synthesizes auto-generated slot methods for a `Value subclass:` class
    /// and appends them to `instance_methods` / `class_methods`.
    ///
    /// Mirrors the selector-computation logic in
    /// `codegen::core_erlang::value_type_codegen::compute_auto_slot_methods` so
    /// that the class hierarchy (and therefore the type checker) knows about
    /// these methods before codegen runs.
    fn add_value_auto_methods(
        class: &ClassDefinition,
        instance_methods: &mut Vec<MethodInfo>,
        class_methods: &mut Vec<MethodInfo>,
    ) {
        let user_instance_selectors: HashSet<EcoString> = instance_methods
            .iter()
            .map(|m| m.selector.clone())
            .collect();
        let user_class_selectors: HashSet<EcoString> =
            class_methods.iter().map(|m| m.selector.clone()).collect();

        let class_name: EcoString = class.name.name.clone();

        for slot in &class.state {
            let slot_name = slot.name.name.as_str();
            let default_str = slot
                .default_value
                .as_ref()
                .map_or_else(|| "nil".to_string(), format_default_value);

            // Auto-getter: unary method returning the slot value
            if !user_instance_selectors.contains(slot_name) {
                let getter_doc = format!(
                    "Returns the `{slot_name}` field value. Default: `{default_str}`.\n\n*(compiler-generated)*"
                );
                instance_methods.push(MethodInfo {
                    selector: slot.name.name.clone(),
                    arity: 0,
                    kind: MethodKind::Primary,
                    defined_in: class_name.clone(),
                    is_sealed: false,
                    spawns_block: false,
                    return_type: slot.type_annotation.as_ref().map(TypeAnnotation::type_name),
                    param_types: vec![],
                    doc: Some(getter_doc.into()),
                });
            }

            // Auto with*: setter: keyword method returning a new instance
            let with_sel = {
                let mut chars = slot_name.chars();
                match chars.next() {
                    None => EcoString::from("with:"),
                    Some(first) => {
                        let cap: String = first.to_uppercase().collect();
                        EcoString::from(format!("with{}{}:", cap, chars.as_str()))
                    }
                }
            };
            if !user_instance_selectors.contains(&with_sel) {
                let setter_doc = format!(
                    "Returns a new `{class_name}` with `{slot_name}` set to the given value.\n\n*(compiler-generated)*"
                );
                instance_methods.push(MethodInfo {
                    selector: with_sel,
                    arity: 1,
                    kind: MethodKind::Primary,
                    defined_in: class_name.clone(),
                    is_sealed: false,
                    spawns_block: false,
                    return_type: Some(class_name.clone()),
                    param_types: vec![None],
                    doc: Some(setter_doc.into()),
                });
            }
        }

        // Auto keyword constructor on the class side (e.g., `x:y:`)
        if !class.state.is_empty() {
            let slot_names: Vec<&str> = class.state.iter().map(|s| s.name.name.as_str()).collect();
            let kw_sel: EcoString = {
                let mut s = String::new();
                for name in &slot_names {
                    s.push_str(name);
                    s.push(':');
                }
                EcoString::from(s)
            };
            let arity = class.state.len();
            if !user_class_selectors.contains(&kw_sel) {
                let args_desc: String = class
                    .state
                    .iter()
                    .map(|s| {
                        let dv = s
                            .default_value
                            .as_ref()
                            .map_or_else(|| "nil".to_string(), format_default_value);
                        format!("{} (default: {})", s.name.name, dv)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let ctor_doc = format!(
                    "Creates a new `{class_name}`. Args: {args_desc}.\n\n*(compiler-generated)*"
                );
                class_methods.push(MethodInfo {
                    selector: kw_sel,
                    arity,
                    kind: MethodKind::Primary,
                    defined_in: class_name.clone(),
                    is_sealed: false,
                    spawns_block: false,
                    return_type: Some(class_name.clone()),
                    param_types: vec![None; arity],
                    doc: Some(ctor_doc.into()),
                });
            }
        }
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
                            ));
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
                            ));
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

/// Formats an AST expression as a compact string for use in generated doc comments.
///
/// Only handles simple literal values (integer, float, string, boolean, nil).
/// Complex expressions fall back to `"..."`.
fn format_default_value(expr: &Expression) -> String {
    match expr {
        Expression::Literal(lit, _) => match lit {
            Literal::Integer(n) => n.to_string(),
            Literal::Float(f) => {
                let rendered = f.to_string();
                // Preserve decimal point so 1.0 stays as "1.0" not "1"
                if rendered.contains('.') || rendered.contains('e') || rendered.contains('E') {
                    rendered
                } else {
                    format!("{rendered}.0")
                }
            }
            Literal::String(s) => format!("{s:?}"),
            Literal::Symbol(s) => format!("#{s}"),
            Literal::Character(c) => format!("${}", c.escape_default()),
            Literal::List(_) => "...".to_string(),
        },
        Expression::Identifier(ident) if ident.name == "nil" => "nil".to_string(),
        Expression::Identifier(ident) if ident.name == "true" => "true".to_string(),
        Expression::Identifier(ident) if ident.name == "false" => "false".to_string(),
        _ => "...".to_string(),
    }
}

#[cfg(test)]
mod tests {
    //! Tests for class hierarchy construction, method resolution, and inheritance rules.
    use super::builtins::builtin_method;
    use super::*;
    use crate::ast::{
        ClassDefinition, ClassKind, CommentAttachment, DeclaredKeyword, Identifier,
        MethodDefinition, ParameterDefinition, StateDeclaration, TypeAnnotation,
    };
    use crate::semantic_analysis::test_helpers::test_span;
    use crate::source_analysis::Span;

    // --- Value object method tests ---

    #[test]
    fn method_info_can_override_non_sealed() {
        let child = builtin_method("printString", 0, "Counter");
        let ancestor = builtin_method("printString", 0, "Object");
        assert!(child.can_override(&ancestor));
    }

    #[test]
    fn method_info_cannot_override_sealed() {
        let child = builtin_method("printString", 0, "Counter");
        let ancestor = MethodInfo {
            is_sealed: true,
            ..builtin_method("printString", 0, "Actor")
        };
        assert!(!child.can_override(&ancestor));
    }

    #[test]
    fn method_info_different_selectors_not_override() {
        let child = builtin_method("increment", 0, "Counter");
        let sealed_ancestor = MethodInfo {
            is_sealed: true,
            ..builtin_method("printString", 0, "Actor")
        };
        // Different selectors → not an override (regardless of sealed status)
        assert!(!child.can_override(&sealed_ancestor));
    }

    #[test]
    fn class_info_can_be_subclassed() {
        let h = ClassHierarchy::with_builtins();
        assert!(h.get_class("Actor").unwrap().can_be_subclassed());
        assert!(h.get_class("Object").unwrap().can_be_subclassed());
        assert!(!h.get_class("Integer").unwrap().can_be_subclassed());
        assert!(!h.get_class("String").unwrap().can_be_subclassed());
    }

    // --- Hierarchy structure tests ---

    #[test]
    fn builtins_include_core_classes() {
        let h = ClassHierarchy::with_builtins();
        assert!(h.has_class("ProtoObject"));
        assert!(h.has_class("Object"));
        assert!(h.has_class("Actor"));
        assert!(h.has_class("Integer"));
        assert!(h.has_class("Float"));
        assert!(h.has_class("String"));
        assert!(h.has_class("List"));
        assert!(h.has_class("Dictionary"));
        assert!(h.has_class("Set"));
        assert!(h.has_class("Block"));
        assert!(h.has_class("UndefinedObject"));
        assert!(h.has_class("True"));
        assert!(h.has_class("False"));
        assert!(h.has_class("Collection"));
    }

    #[test]
    fn proto_object_has_no_superclass() {
        let h = ClassHierarchy::with_builtins();
        let proto = h.get_class("ProtoObject").unwrap();
        assert!(proto.superclass.is_none());
    }

    #[test]
    fn object_extends_proto_object() {
        let h = ClassHierarchy::with_builtins();
        let obj = h.get_class("Object").unwrap();
        assert_eq!(obj.superclass.as_deref(), Some("ProtoObject"));
    }

    #[test]
    fn actor_extends_object() {
        let h = ClassHierarchy::with_builtins();
        let actor = h.get_class("Actor").unwrap();
        assert_eq!(actor.superclass.as_deref(), Some("Object"));
    }

    #[test]
    fn integer_is_sealed() {
        let h = ClassHierarchy::with_builtins();
        let int = h.get_class("Integer").unwrap();
        assert!(int.is_sealed);
    }

    #[test]
    fn float_is_sealed() {
        let h = ClassHierarchy::with_builtins();
        let float = h.get_class("Float").unwrap();
        assert!(float.is_sealed);
    }

    // --- Superclass chain tests ---

    #[test]
    fn superclass_chain_for_actor() {
        let h = ClassHierarchy::with_builtins();
        let chain = h.superclass_chain("Actor");
        assert_eq!(
            chain,
            vec![EcoString::from("Object"), EcoString::from("ProtoObject")]
        );
    }

    #[test]
    fn superclass_chain_for_proto_object_is_empty() {
        let h = ClassHierarchy::with_builtins();
        let chain = h.superclass_chain("ProtoObject");
        assert!(chain.is_empty());
    }

    #[test]
    fn superclass_chain_for_integer() {
        let h = ClassHierarchy::with_builtins();
        let chain = h.superclass_chain("Integer");
        assert_eq!(
            chain,
            vec![
                EcoString::from("Number"),
                EcoString::from("Value"),
                EcoString::from("Object"),
                EcoString::from("ProtoObject"),
            ]
        );
    }

    #[test]
    fn superclass_chain_for_unknown_class() {
        let h = ClassHierarchy::with_builtins();
        let chain = h.superclass_chain("DoesNotExist");
        assert!(chain.is_empty());
    }

    // --- all_methods tests ---

    #[test]
    fn all_methods_includes_inherited() {
        let h = ClassHierarchy::with_builtins();
        let methods = h.all_methods("Actor");
        let selectors: Vec<&str> = methods.iter().map(|m| m.selector.as_str()).collect();

        // Actor instance methods (spawn/spawnWith: are class-side, not instance-side)
        assert!(selectors.contains(&"stop"));
        assert!(selectors.contains(&"isAlive"));
        assert!(
            !selectors.contains(&"spawn"),
            "spawn must NOT be in instance methods"
        );
        assert!(
            !selectors.contains(&"spawnWith:"),
            "spawnWith: must NOT be in instance methods"
        );
        // Note: `new` is still in Object's instance methods and thus inherited here

        // Inherited from Object
        assert!(selectors.contains(&"isNil"));
        assert!(selectors.contains(&"respondsTo:"));

        // Inherited from ProtoObject
        assert!(selectors.contains(&"class"));
        assert!(selectors.contains(&"=="));
    }

    #[test]
    fn actor_spawn_methods_are_class_side() {
        let h = ClassHierarchy::with_builtins();
        let class_methods = h.all_class_methods("Actor");
        let selectors: Vec<&str> = class_methods.iter().map(|m| m.selector.as_str()).collect();

        assert!(
            selectors.contains(&"spawn"),
            "spawn must be in class_methods"
        );
        assert!(
            selectors.contains(&"spawnWith:"),
            "spawnWith: must be in class_methods"
        );
        // BT-1524: new/new: overrides removed from Actor — no longer class-side
        assert!(
            !selectors.contains(&"new"),
            "new must NOT be in Actor class_methods after BT-1524"
        );
        assert!(
            !selectors.contains(&"new:"),
            "new: must NOT be in Actor class_methods after BT-1524"
        );

        // Instance methods must NOT appear on class side
        assert!(
            !selectors.contains(&"stop"),
            "stop must NOT be in class_methods"
        );
        assert!(
            !selectors.contains(&"isAlive"),
            "isAlive must NOT be in class_methods"
        );

        // Actor class methods carry doc strings from generated_builtins.rs
        // (sourced from Actor.bt via build-stdlib)
        let spawn = class_methods
            .iter()
            .find(|m| m.selector == "spawn")
            .unwrap();
        let spawn_doc = spawn.doc.as_deref().unwrap();
        assert!(
            spawn_doc.contains("actor process"),
            "spawn doc should describe actor process creation: {spawn_doc}"
        );

        let spawn_with = class_methods
            .iter()
            .find(|m| m.selector == "spawnWith:")
            .unwrap();
        assert!(
            spawn_with.doc.as_deref().unwrap().contains("actor"),
            "spawnWith: doc should describe actor creation"
        );

        // BT-1524: new/new: overrides removed — no longer in Actor class_methods
    }

    #[test]
    fn all_methods_overrides_use_most_specific() {
        let h = ClassHierarchy::with_builtins();
        let methods = h.all_methods("Actor");

        // Object defines 'printString', Actor inherits it
        // Only the most-specific (Object's) version should appear once
        let print_methods: Vec<&MethodInfo> = methods
            .iter()
            .filter(|m| m.selector == "printString")
            .collect();
        assert_eq!(print_methods.len(), 1);
        assert_eq!(print_methods[0].defined_in.as_str(), "Object");
    }

    #[test]
    fn all_methods_for_unknown_class_is_empty() {
        let h = ClassHierarchy::with_builtins();
        let methods = h.all_methods("DoesNotExist");
        assert!(methods.is_empty());
    }

    // --- resolves_selector tests ---

    #[test]
    fn resolves_selector_local() {
        let h = ClassHierarchy::with_builtins();
        assert!(h.resolves_selector("Integer", "+"));
        assert!(h.resolves_selector("Integer", "abs"));
    }

    #[test]
    fn resolves_selector_inherited() {
        let h = ClassHierarchy::with_builtins();
        // Integer inherits from Object which has isNil
        assert!(h.resolves_selector("Integer", "isNil"));
        // Integer inherits from ProtoObject which has class
        assert!(h.resolves_selector("Integer", "class"));
    }

    #[test]
    fn resolves_selector_unknown_returns_false() {
        let h = ClassHierarchy::with_builtins();
        assert!(!h.resolves_selector("Integer", "nonExistentMethod"));
    }

    #[test]
    fn resolves_selector_unknown_class_returns_false() {
        let h = ClassHierarchy::with_builtins();
        assert!(!h.resolves_selector("Nope", "anything"));
        // Guard: Object methods must NOT resolve for a completely-unknown root class.
        assert!(!h.resolves_selector("Nope", "isNil"));
        assert!(!h.resolves_selector("Nope", "subclassResponsibility"));
    }

    /// BT-889: When a class inherits from an external class (defined in a
    /// separately-compiled file and therefore absent from the hierarchy), the
    /// walk must fall through to Object so that Object-level methods are
    /// still visible.
    #[test]
    fn resolves_selector_through_unknown_external_parent_to_object() {
        // Simulate file 2 compiling CondimentDecorator (extends Beverage from file 1).
        // Beverage is NOT in the hierarchy — only CondimentDecorator is.
        // subclassResponsibility lives on Object, two hops above CondimentDecorator.
        let module = Module {
            classes: vec![make_user_class("CondimentDecorator", "Beverage")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, _diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();

        // Object method must be reachable even though Beverage is unknown.
        assert!(
            h.resolves_selector("CondimentDecorator", "subclassResponsibility"),
            "subclassResponsibility (on Object) should resolve through unknown Beverage parent"
        );
        assert!(
            h.resolves_selector("CondimentDecorator", "isNil"),
            "isNil (on Object) should resolve through unknown Beverage parent"
        );
        // A genuinely non-existent method must still return false.
        assert!(
            !h.resolves_selector("CondimentDecorator", "totallyBogusMethod"),
            "unknown selector should still return false"
        );
        // find_method must also traverse through the unknown parent.
        let method = h.find_method("CondimentDecorator", "subclassResponsibility");
        assert!(
            method.is_some(),
            "find_method should locate subclassResponsibility via Object"
        );
        assert_eq!(method.unwrap().defined_in.as_str(), "Object");
    }

    // --- User-defined class tests ---

    fn make_user_class(name: &str, superclass: &str) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, test_span()),
            superclass: Some(Identifier::new(superclass, test_span())),
            class_kind: ClassKind::from_superclass_name(superclass),
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![StateDeclaration {
                name: Identifier::new("count", test_span()),
                type_annotation: None,
                default_value: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: test_span(),
            }],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        }
    }

    #[test]
    fn user_defined_class_added_to_hierarchy() {
        let module = Module {
            classes: vec![make_user_class("Counter", "Actor")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();
        assert!(diags.is_empty());
        assert!(h.has_class("Counter"));

        let counter = h.get_class("Counter").unwrap();
        assert_eq!(counter.superclass.as_deref(), Some("Actor"));
        assert_eq!(counter.state, vec![EcoString::from("count")]);
        assert_eq!(counter.methods.len(), 1);
        assert_eq!(counter.methods[0].selector.as_str(), "increment");
    }

    #[test]
    fn user_class_inherits_from_actor_chain() {
        let module = Module {
            classes: vec![make_user_class("Counter", "Actor")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let h = ClassHierarchy::build(&module).0.unwrap();

        let chain = h.superclass_chain("Counter");
        assert_eq!(
            chain,
            vec![
                EcoString::from("Actor"),
                EcoString::from("Object"),
                EcoString::from("ProtoObject"),
            ]
        );
    }

    #[test]
    fn user_class_resolves_inherited_selectors() {
        let module = Module {
            classes: vec![make_user_class("Counter", "Actor")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let h = ClassHierarchy::build(&module).0.unwrap();

        // Local method
        assert!(h.resolves_selector("Counter", "increment"));
        // spawn is class-side on Actor — verify via all_class_methods
        assert!(
            h.all_class_methods("Counter")
                .iter()
                .any(|m| m.selector.as_str() == "spawn"),
            "Counter should inherit class-side spawn from Actor"
        );
        // Inherited from Object
        assert!(h.resolves_selector("Counter", "respondsTo:"));
        // Inherited from ProtoObject
        assert!(h.resolves_selector("Counter", "class"));
    }

    // --- Sealed class enforcement tests ---

    #[test]
    fn sealed_class_subclassing_rejected() {
        let module = Module {
            classes: vec![make_user_class("MyInt", "Integer")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed"));
        assert!(diags[0].message.contains("Integer"));
        // Class IS still registered (so codegen can route correctly despite error)
        assert!(h.has_class("MyInt"));
    }

    #[test]
    fn sealed_string_subclassing_rejected() {
        let module = Module {
            classes: vec![make_user_class("MyString", "String")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (_, diags) = ClassHierarchy::build(&module);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed"));
    }

    #[test]
    fn non_sealed_subclassing_allowed() {
        let module = Module {
            classes: vec![make_user_class("MyActor", "Actor")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();
        assert!(diags.is_empty());
        assert!(h.has_class("MyActor"));
    }

    // --- BT-791: stdlib_mode gating tests ---

    #[test]
    fn stdlib_mode_exempts_builtin_class_from_sealed_check() {
        // In stdlib_mode, a class named "Character" extending sealed "Integer" is allowed.
        let module = Module {
            classes: vec![make_user_class("Character", "Integer")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build_with_options(&module, true);
        let h = h.unwrap();
        assert!(
            diags.is_empty(),
            "stdlib_mode should exempt builtin class: {diags:?}"
        );
        assert!(h.has_class("Character"));
    }

    #[test]
    fn user_code_character_subclassing_integer_rejected() {
        // Without stdlib_mode, even a class named "Character" is rejected.
        let module = Module {
            classes: vec![make_user_class("Character", "Integer")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build_with_options(&module, false);
        let h = h.unwrap();
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed"));
        assert!(diags[0].message.contains("Integer"));
        // Class is still registered despite the error
        assert!(h.has_class("Character"));
    }

    #[test]
    fn stdlib_mode_does_not_exempt_non_builtin_class() {
        // Even in stdlib_mode, a non-builtin name extending a sealed class is rejected.
        let module = Module {
            classes: vec![make_user_class("MyCustomClass", "Integer")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (_, diags) = ClassHierarchy::build_with_options(&module, true);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed"));
    }

    // --- BT-778: Character hierarchy tests ---

    #[test]
    fn character_superclass_chain_includes_integer() {
        // BT-778: Character inherits from Integer in the builtin hierarchy.
        let h = ClassHierarchy::with_builtins();
        let chain = h.superclass_chain("Character");
        assert!(
            chain.contains(&"Integer".into()),
            "Chain should include Integer: {chain:?}"
        );
        assert!(
            chain.contains(&"Number".into()),
            "Chain should include Number: {chain:?}"
        );
    }

    #[test]
    fn character_resolves_integer_selectors() {
        // BT-778: Character should resolve Integer methods via inheritance.
        let h = ClassHierarchy::with_builtins();
        assert!(
            h.resolves_selector("Character", "+"),
            "Character should understand '+'"
        );
        assert!(
            h.resolves_selector("Character", "-"),
            "Character should understand '-'"
        );
        assert!(
            h.resolves_selector("Character", "*"),
            "Character should understand '*'"
        );
        assert!(
            h.resolves_selector("Character", "isLetter"),
            "Character should understand 'isLetter'"
        );
        assert!(
            !h.resolves_selector("Character", "bogusMethod"),
            "Character should NOT understand 'bogusMethod'"
        );
    }

    #[test]
    fn character_is_numeric_type() {
        // BT-778: Character inherits from Integer which inherits from Number,
        // so it should be treated as numeric for operand-type checks.
        let h = ClassHierarchy::with_builtins();
        assert!(h.is_numeric_type("Integer"), "Integer is numeric");
        assert!(h.is_numeric_type("Float"), "Float is numeric");
        assert!(h.is_numeric_type("Number"), "Number is numeric");
        assert!(
            h.is_numeric_type("Character"),
            "Character is numeric (inherits Integer)"
        );
        assert!(!h.is_numeric_type("String"), "String is not numeric");
        assert!(!h.is_numeric_type("Boolean"), "Boolean is not numeric");
    }

    // --- Sealed method override enforcement tests ---

    fn make_class_with_sealed_method(
        name: &str,
        superclass: &str,
        method_name: &str,
        sealed: bool,
    ) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, test_span()),
            superclass: Some(Identifier::new(superclass, test_span())),
            class_kind: ClassKind::from_superclass_name(superclass),
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary(method_name.into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: sealed,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        }
    }

    fn make_class_with_sealed_class_method(
        name: &str,
        superclass: &str,
        method_name: &str,
        sealed: bool,
    ) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, test_span()),
            superclass: Some(Identifier::new(superclass, test_span())),
            class_kind: ClassKind::from_superclass_name(superclass),
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![],
            class_methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary(method_name.into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: sealed,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        }
    }

    #[test]
    fn sealed_method_override_rejected() {
        // Parent defines sealed method "doCustomWork", child tries to override it
        let parent = make_class_with_sealed_method("Parent", "Actor", "doCustomWork", true);
        let child = make_class_with_sealed_method("Child", "Parent", "doCustomWork", false);

        let module = Module {
            classes: vec![parent, child],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed method"));
        assert!(diags[0].message.contains("`doCustomWork`"));
        assert!(diags[0].message.contains("`Parent`"));
        // Child class is still added despite the error
        assert!(h.has_class("Child"));
    }

    #[test]
    fn non_sealed_method_override_allowed() {
        // Parent defines non-sealed method, child overrides it — no diagnostic
        let parent = make_class_with_sealed_method("Parent", "Actor", "doWork", false);
        let child = make_class_with_sealed_method("Child", "Parent", "doWork", false);

        let module = Module {
            classes: vec![parent, child],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (_, diags) = ClassHierarchy::build(&module);
        assert!(diags.is_empty());
    }

    #[test]
    fn sealed_method_in_grandparent_enforced() {
        // Grandparent defines sealed method, grandchild tries to override it
        let grandparent = make_class_with_sealed_method("GrandParent", "Actor", "locked", true);
        let parent = make_class_with_sealed_method("Parent", "GrandParent", "doWork", false);
        let grandchild = make_class_with_sealed_method("GrandChild", "Parent", "locked", false);

        let module = Module {
            classes: vec![grandparent, parent, grandchild],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed method"));
        assert!(diags[0].message.contains("`locked`"));
        assert!(diags[0].message.contains("`GrandParent`"));
        assert!(h.has_class("GrandChild"));
    }

    #[test]
    fn builtin_sealed_method_override_rejected() {
        // Object's respondsTo: is sealed in builtins — user class cannot override it
        let child = make_class_with_sealed_method("MyObj", "Object", "respondsTo:", false);

        let module = Module {
            classes: vec![child],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed method"));
        assert!(diags[0].message.contains("`respondsTo:`"));
        assert!(diags[0].message.contains("`Object`"));
        assert!(h.has_class("MyObj"));
    }

    #[test]
    fn builtin_actor_spawn_override_rejected() {
        // Actor's class-side spawn is sealed in builtins — subclass cannot override it
        let child = make_class_with_sealed_class_method("MyActor", "Actor", "spawn", false);

        let module = Module {
            classes: vec![child],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("sealed method"));
        assert!(diags[0].message.contains("`spawn`"));
        assert!(diags[0].message.contains("`Actor`"));
        assert!(h.has_class("MyActor"));
    }

    // --- MRO verification ---

    #[test]
    fn mro_is_depth_first() {
        // Counter -> Actor -> Object -> ProtoObject
        let module = Module {
            classes: vec![make_user_class("Counter", "Actor")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let h = ClassHierarchy::build(&module).0.unwrap();
        let methods = h.all_methods("Counter");

        // First method should be from Counter (most specific)
        assert_eq!(methods[0].defined_in.as_str(), "Counter");

        // Actor methods should come before Object methods
        let actor_idx = methods
            .iter()
            .position(|m| m.defined_in == "Actor")
            .unwrap();
        let object_idx = methods
            .iter()
            .position(|m| m.defined_in == "Object")
            .unwrap();
        assert!(actor_idx < object_idx);
    }

    // --- Edge case tests ---

    #[test]
    fn cycle_detection_in_superclass_chain() {
        // Manually create a cycle: A -> B -> A
        let mut h = ClassHierarchy::with_builtins();
        h.classes.insert(
            "A".into(),
            ClassInfo {
                name: "A".into(),
                superclass: Some("B".into()),
                is_sealed: false,
                is_abstract: false,
                is_typed: false,
                is_value: false,
                is_native: false,
                state: vec![],
                state_types: HashMap::new(),
                methods: vec![builtin_method("methodA", 0, "A")],
                class_methods: vec![],
                class_variables: vec![],
                type_params: vec![],
                superclass_type_args: vec![],
            },
        );
        h.classes.insert(
            "B".into(),
            ClassInfo {
                name: "B".into(),
                superclass: Some("A".into()),
                is_sealed: false,
                is_abstract: false,
                is_typed: false,
                is_value: false,
                is_native: false,
                state: vec![],
                state_types: HashMap::new(),
                methods: vec![builtin_method("methodB", 0, "B")],
                class_methods: vec![],
                class_variables: vec![],
                type_params: vec![],
                superclass_type_args: vec![],
            },
        );

        // Should not loop forever
        let chain = h.superclass_chain("A");
        assert!(chain.len() <= 2);

        let methods = h.all_methods("A");
        assert!(!methods.is_empty());

        // Should not hang
        let _ = h.resolves_selector("A", "methodB");
    }

    #[test]
    fn multiple_user_classes_in_module() {
        let base = ClassDefinition {
            name: Identifier::new("Base", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("baseMethod".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };
        let derived = ClassDefinition {
            name: Identifier::new("Derived", test_span()),
            superclass: Some(Identifier::new("Base", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("derivedMethod".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![base, derived],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();
        assert!(diags.is_empty());

        // Derived should inherit from Base -> Actor -> Object -> ProtoObject
        assert!(h.resolves_selector("Derived", "derivedMethod"));
        assert!(h.resolves_selector("Derived", "baseMethod"));
        // spawn is class-side on Actor — verify via all_class_methods
        assert!(
            h.all_class_methods("Derived")
                .iter()
                .any(|m| m.selector.as_str() == "spawn"),
            "Derived should inherit class-side spawn from Actor"
        );
        assert!(h.resolves_selector("Derived", "class"));
    }

    #[test]
    fn user_class_with_unknown_superclass() {
        let module = Module {
            classes: vec![make_user_class("Orphan", "NonExistent")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();

        // No diagnostic for unknown superclass (sealed check passes)
        assert!(diags.is_empty());
        // Class is still added
        assert!(h.has_class("Orphan"));
        // Local method resolves
        assert!(h.resolves_selector("Orphan", "increment"));
        // Chain stops at unknown superclass
        let chain = h.superclass_chain("Orphan");
        assert_eq!(chain, vec![EcoString::from("NonExistent")]);
    }

    // --- Duplicate method detection tests ---

    #[test]
    fn duplicate_instance_method_detected() {
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![
                MethodDefinition {
                    selector: crate::ast::MessageSelector::Unary("increment".into()),
                    parameters: vec![],
                    body: vec![],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: Span::new(10, 20),
                },
                MethodDefinition {
                    selector: crate::ast::MessageSelector::Unary("increment".into()),
                    parameters: vec![],
                    body: vec![],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: Span::new(30, 40),
                },
            ],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (_, diags) = ClassHierarchy::build(&module);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Duplicate method"));
        assert!(diags[0].message.contains("`increment`"));
        assert!(diags[0].message.contains("`Counter`"));
        assert!(diags[0].hint.is_some());
        assert!(diags[0].hint.as_ref().unwrap().contains("already defined"));
    }

    #[test]
    fn duplicate_class_method_detected() {
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![],
            class_methods: vec![
                MethodDefinition {
                    selector: crate::ast::MessageSelector::Unary("create".into()),
                    parameters: vec![],
                    body: vec![],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: Span::new(10, 20),
                },
                MethodDefinition {
                    selector: crate::ast::MessageSelector::Unary("create".into()),
                    parameters: vec![],
                    body: vec![],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: Span::new(30, 40),
                },
            ],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (_, diags) = ClassHierarchy::build(&module);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Duplicate class method"));
        assert!(diags[0].message.contains("`create`"));
    }

    #[test]
    fn no_duplicate_for_different_selectors() {
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Actor", test_span())),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![
                MethodDefinition {
                    selector: crate::ast::MessageSelector::Unary("increment".into()),
                    parameters: vec![],
                    body: vec![],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: test_span(),
                },
                MethodDefinition {
                    selector: crate::ast::MessageSelector::Unary("decrement".into()),
                    parameters: vec![],
                    body: vec![],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: test_span(),
                },
            ],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (_, diags) = ClassHierarchy::build(&module);

        assert!(diags.is_empty());
    }

    // --- Typed class inheritance tests (BT-587) ---

    #[test]
    fn typed_class_is_typed() {
        let mut class = make_class_with_sealed_method("StrictCounter", "Actor", "increment", false);
        class.is_typed = true;

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        assert!(
            hierarchy.is_typed("StrictCounter"),
            "explicitly typed class should be typed"
        );
    }

    #[test]
    fn typed_class_inheritance() {
        // Parent is typed, child inherits typed
        let mut parent = make_class_with_sealed_method("TypedParent", "Actor", "method", false);
        parent.is_typed = true;

        let child = make_class_with_sealed_method("UntypedChild", "TypedParent", "method", false);

        let module = Module {
            classes: vec![parent, child],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        assert!(hierarchy.is_typed("TypedParent"), "parent should be typed");
        assert!(
            hierarchy.is_typed("UntypedChild"),
            "child of typed class should inherit typed"
        );
    }

    #[test]
    fn typed_class_inheritance_reverse_order() {
        // Child defined BEFORE typed parent — should still inherit typed
        let mut parent = make_class_with_sealed_method("TypedParent", "Actor", "method", false);
        parent.is_typed = true;

        let child = make_class_with_sealed_method("UntypedChild", "TypedParent", "method", false);

        // Note: child comes first in the vec! (reverse definition order)
        let module = Module {
            classes: vec![child, parent],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        assert!(hierarchy.is_typed("TypedParent"), "parent should be typed");
        assert!(
            hierarchy.is_typed("UntypedChild"),
            "child of typed class should inherit typed even when defined before parent"
        );
    }

    #[test]
    fn non_typed_class_not_inherited() {
        // Parent is NOT typed, child should not be typed
        let parent = make_class_with_sealed_method("Parent", "Actor", "method", false);
        let child = make_class_with_sealed_method("Child", "Parent", "method", false);

        let module = Module {
            classes: vec![parent, child],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();

        assert!(!hierarchy.is_typed("Parent"));
        assert!(!hierarchy.is_typed("Child"));
    }

    // --- Type annotation propagation tests ---

    #[test]
    fn stdlib_integer_plus_has_return_type() {
        let h = ClassHierarchy::with_builtins();
        let method = h
            .find_method("Integer", "+")
            .expect("Integer >> + should exist");
        assert_eq!(method.return_type.as_deref(), Some("Integer"));
    }

    #[test]
    fn stdlib_integer_plus_has_param_types() {
        let h = ClassHierarchy::with_builtins();
        let method = h
            .find_method("Integer", "+")
            .expect("Integer >> + should exist");
        assert_eq!(method.param_types, vec![Some("Number".into())]);
    }

    #[test]
    fn stdlib_unary_method_has_empty_param_types() {
        let h = ClassHierarchy::with_builtins();
        let method = h
            .find_method("Integer", "asFloat")
            .expect("Integer >> asFloat should exist");
        assert!(method.param_types.is_empty());
    }

    #[test]
    fn user_class_return_type_propagated() {
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![],
                return_type: Some(TypeAnnotation::simple("Integer", test_span())),
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            supervisor_kind: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let method = hierarchy
            .find_method("Counter", "getValue")
            .expect("Counter >> getValue should exist");
        assert_eq!(method.return_type.as_deref(), Some("Integer"));
    }

    #[test]
    fn user_class_param_types_propagated() {
        let class = ClassDefinition {
            name: Identifier::new("Counter", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                    keyword: "add:".into(),
                    span: test_span(),
                }]),
                parameters: vec![ParameterDefinition::with_type(
                    Identifier::new("amount", test_span()),
                    TypeAnnotation::simple("Integer", test_span()),
                )],
                body: vec![],
                return_type: Some(TypeAnnotation::simple("Counter", test_span())),
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            supervisor_kind: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let method = hierarchy
            .find_method("Counter", "add:")
            .expect("Counter >> add: should exist");
        assert_eq!(method.return_type.as_deref(), Some("Counter"));
        assert_eq!(method.param_types, vec![Some("Integer".into())]);
    }

    // --- State field type tests ---

    fn make_typed_state_class(name: &str, superclass: &str) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, test_span()),
            superclass: Some(Identifier::new(superclass, test_span())),
            class_kind: ClassKind::from_superclass_name(superclass),
            is_abstract: false,
            is_sealed: false,
            is_typed: true,
            supervisor_kind: None,
            state: vec![
                StateDeclaration::with_type(
                    Identifier::new("count", test_span()),
                    TypeAnnotation::simple("Integer", test_span()),
                    test_span(),
                ),
                StateDeclaration::new(Identifier::new("label", test_span()), test_span()),
            ],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        }
    }

    #[test]
    fn state_field_type_returns_type_for_annotated_field() {
        let module = Module {
            classes: vec![make_typed_state_class("Counter", "Actor")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();
        assert!(diags.is_empty());
        assert_eq!(
            h.state_field_type("Counter", "count"),
            Some(EcoString::from("Integer"))
        );
    }

    #[test]
    fn state_field_type_returns_none_for_untyped_field() {
        let module = Module {
            classes: vec![make_typed_state_class("Counter", "Actor")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let h = ClassHierarchy::build(&module).0.unwrap();
        assert_eq!(h.state_field_type("Counter", "label"), None);
    }

    #[test]
    fn state_field_type_returns_none_for_unknown_field() {
        let module = Module {
            classes: vec![make_typed_state_class("Counter", "Actor")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let h = ClassHierarchy::build(&module).0.unwrap();
        assert_eq!(h.state_field_type("Counter", "nonexistent"), None);
    }

    #[test]
    fn state_field_type_returns_none_for_unknown_class() {
        let h = ClassHierarchy::with_builtins();
        assert_eq!(h.state_field_type("DoesNotExist", "count"), None);
    }

    #[test]
    fn state_field_type_inherited_from_parent() {
        let parent = make_typed_state_class("TypedParent", "Actor");
        let child = ClassDefinition {
            name: Identifier::new("Child", test_span()),
            superclass: Some(Identifier::new("TypedParent", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![StateDeclaration::with_type(
                Identifier::new("extra", test_span()),
                TypeAnnotation::simple("String", test_span()),
                test_span(),
            )],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![parent, child],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (h, diags) = ClassHierarchy::build(&module);
        let h = h.unwrap();
        assert!(diags.is_empty());

        // Child's own typed field
        assert_eq!(
            h.state_field_type("Child", "extra"),
            Some(EcoString::from("String"))
        );
        // Inherited typed field from parent
        assert_eq!(
            h.state_field_type("Child", "count"),
            Some(EcoString::from("Integer"))
        );
        // Inherited untyped field from parent
        assert_eq!(h.state_field_type("Child", "label"), None);
    }

    #[test]
    fn state_field_type_builtin_classes_return_none() {
        let h = ClassHierarchy::with_builtins();
        // Built-in classes have no typed state currently
        assert_eq!(h.state_field_type("Integer", "anything"), None);
        assert_eq!(h.state_field_type("Actor", "anything"), None);
    }

    #[test]
    fn state_field_type_shadowed_untyped_field() {
        // Parent declares `count: Integer`, child redeclares `count` without type.
        // The child's untyped declaration should shadow the parent's type.
        let parent = make_typed_state_class("TypedParent", "Actor");
        let child = ClassDefinition {
            name: Identifier::new("Child", test_span()),
            superclass: Some(Identifier::new("TypedParent", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![StateDeclaration::new(
                Identifier::new("count", test_span()),
                test_span(),
            )],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![parent, child],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let h = ClassHierarchy::build(&module).0.unwrap();

        // Child's untyped `count` shadows parent's typed `count: Integer`
        assert_eq!(h.state_field_type("Child", "count"), None);
        // Parent's typed `count` is still accessible on the parent
        assert_eq!(
            h.state_field_type("TypedParent", "count"),
            Some(EcoString::from("Integer"))
        );
    }

    #[test]
    fn has_class_dnu_override_detects_erlang() {
        // Erlang has doesNotUnderstand:args: as a class method.
        let h = ClassHierarchy::with_builtins();
        assert!(
            h.has_class_dnu_override("Erlang"),
            "Erlang class-side DNU override should be detected"
        );
        // Erlang has no instance-side DNU
        assert!(
            !h.has_instance_dnu_override("Erlang"),
            "Erlang should not have instance-side DNU override"
        );
    }

    #[test]
    fn has_instance_dnu_override_detects_erlang_module() {
        // ErlangModule has doesNotUnderstand:args: as an instance method.
        let h = ClassHierarchy::with_builtins();
        assert!(
            h.has_instance_dnu_override("ErlangModule"),
            "ErlangModule instance-side DNU override should be detected"
        );
        // ErlangModule has no class-side DNU
        assert!(
            !h.has_class_dnu_override("ErlangModule"),
            "ErlangModule should not have class-side DNU override"
        );
    }

    #[test]
    fn has_dnu_override_false_for_normal_class() {
        let h = ClassHierarchy::with_builtins();
        assert!(
            !h.has_instance_dnu_override("Integer"),
            "Integer should not have instance DNU override"
        );
        assert!(
            !h.has_class_dnu_override("Integer"),
            "Integer should not have class DNU override"
        );
    }

    // --- BT-894: Cross-file superclass enrichment tests ---

    #[test]
    fn add_external_superclasses_resolves_value_object_chain() {
        let mut h = ClassHierarchy::with_builtins();
        // Simulate: File A defines "MyParent" as Object subclass
        // File B defines "MyChild" as MyParent subclass
        // When compiling File B, only MyChild is in the hierarchy.
        // The external superclass index provides the missing link.
        let mut index = HashMap::new();
        index.insert("MyParent".to_string(), "Object".to_string());
        h.add_external_superclasses(&index);

        // MyParent should now resolve through to Object
        let chain = h.superclass_chain("MyParent");
        assert_eq!(
            chain,
            vec![EcoString::from("Object"), EcoString::from("ProtoObject")]
        );

        // A child class added to the hierarchy should now resolve through MyParent → Object
        assert!(!h.is_actor_subclass("MyParent"));
    }

    #[test]
    fn add_external_superclasses_resolves_actor_chain() {
        let mut h = ClassHierarchy::with_builtins();
        // Simulate: File A defines "MyActor" as Actor subclass
        // File B defines "MySpecialActor" as MyActor subclass
        let mut index = HashMap::new();
        index.insert("MyActor".to_string(), "Actor".to_string());
        h.add_external_superclasses(&index);

        // MyActor should resolve through to Actor
        assert!(h.is_actor_subclass("MyActor"));
    }

    // --- is_value_subclass tests (ADR 0042) ---

    #[test]
    fn value_itself_is_value_subclass() {
        let h = ClassHierarchy::with_builtins();
        assert!(h.is_value_subclass("Value"));
    }

    #[test]
    fn object_is_not_value_subclass() {
        let h = ClassHierarchy::with_builtins();
        assert!(!h.is_value_subclass("Object"));
    }

    #[test]
    fn actor_is_not_value_subclass() {
        let h = ClassHierarchy::with_builtins();
        assert!(!h.is_value_subclass("Actor"));
    }

    #[test]
    fn user_value_subclass_detected() {
        let mut h = ClassHierarchy::with_builtins();
        let mut index = HashMap::new();
        index.insert("Point".to_string(), "Value".to_string());
        h.add_external_superclasses(&index);
        assert!(h.is_value_subclass("Point"));
    }

    #[test]
    fn user_object_subclass_is_not_value_subclass() {
        let mut h = ClassHierarchy::with_builtins();
        let mut index = HashMap::new();
        index.insert("Plain".to_string(), "Object".to_string());
        h.add_external_superclasses(&index);
        assert!(!h.is_value_subclass("Plain"));
    }

    #[test]
    fn add_external_superclasses_does_not_overwrite_existing() {
        let mut h = ClassHierarchy::with_builtins();
        // Object is already in the hierarchy — external index should not overwrite it
        let mut index = HashMap::new();
        index.insert("Object".to_string(), "SomethingElse".to_string());
        h.add_external_superclasses(&index);

        // Object should still resolve to ProtoObject (built-in), not SomethingElse
        let chain = h.superclass_chain("Object");
        assert_eq!(chain, vec![EcoString::from("ProtoObject")]);
    }

    // --- ClassKind / is_value integration tests (BT-922) ---

    #[test]
    fn value_subclass_sets_is_value_flag() {
        let class = ClassDefinition {
            name: Identifier::new("Point", test_span()),
            superclass: Some(Identifier::new("Value", test_span())),
            class_kind: ClassKind::Value,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };
        let module = Module::new(vec![], test_span());
        let mut module_with_class = module;
        module_with_class.classes.push(class);
        let (Ok(h), _) = ClassHierarchy::build(&module_with_class) else {
            panic!("build should succeed");
        };
        let info = h.get_class("Point").expect("Point should be registered");
        assert!(info.is_value, "Value subclass should have is_value = true");
        assert!(h.is_value_subclass("Point"));
    }

    #[test]
    fn value_subclass_auto_generates_slot_methods() {
        let class = ClassDefinition {
            name: Identifier::new("Point", test_span()),
            superclass: Some(Identifier::new("Value", test_span())),
            class_kind: ClassKind::Value,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![
                StateDeclaration::new(Identifier::new("x", test_span()), test_span()),
                StateDeclaration::new(Identifier::new("y", test_span()), test_span()),
            ],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };
        let module = Module {
            classes: vec![class],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        let info = h.get_class("Point").expect("Point should be registered");

        // Auto-generated instance methods: getters (x, y) + setters (withX:, withY:)
        let instance_sels: Vec<&str> = info.methods.iter().map(|m| m.selector.as_str()).collect();
        assert!(
            instance_sels.contains(&"x"),
            "getter x missing: {instance_sels:?}"
        );
        assert!(
            instance_sels.contains(&"y"),
            "getter y missing: {instance_sels:?}"
        );
        assert!(
            instance_sels.contains(&"withX:"),
            "setter withX: missing: {instance_sels:?}"
        );
        assert!(
            instance_sels.contains(&"withY:"),
            "setter withY: missing: {instance_sels:?}"
        );

        // Auto-generated class method: keyword constructor x:y:
        let class_sels: Vec<&str> = info
            .class_methods
            .iter()
            .map(|m| m.selector.as_str())
            .collect();
        assert!(
            class_sels.contains(&"x:y:"),
            "keyword constructor x:y: missing: {class_sels:?}"
        );
    }

    #[test]
    fn value_subclass_auto_methods_have_generated_docs() {
        use crate::ast::{Expression, Literal};
        let state = vec![StateDeclaration::with_default(
            Identifier::new("x", test_span()),
            Expression::Literal(Literal::Integer(0), test_span()),
            test_span(),
        )];
        let class = ClassDefinition {
            name: Identifier::new("Point", test_span()),
            superclass: Some(Identifier::new("Value", test_span())),
            class_kind: ClassKind::Value,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state,
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };
        let module = Module {
            classes: vec![class],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        let info = h.get_class("Point").expect("Point should be registered");

        // Getter doc contains field name, default, and compiler-generated tag
        let getter = info.methods.iter().find(|m| m.selector == "x").unwrap();
        let getter_doc = getter.doc.as_deref().unwrap();
        assert!(getter_doc.contains("`x`"), "getter doc missing field name");
        assert!(
            getter_doc.contains("`0`"),
            "getter doc missing default value"
        );
        assert!(
            getter_doc.contains("*(compiler-generated)*"),
            "getter doc missing tag"
        );

        // Setter doc mentions class name and field name
        let setter = info
            .methods
            .iter()
            .find(|m| m.selector == "withX:")
            .unwrap();
        let setter_doc = setter.doc.as_deref().unwrap();
        assert!(
            setter_doc.contains("`Point`"),
            "setter doc missing class name"
        );
        assert!(setter_doc.contains("`x`"), "setter doc missing field name");
        assert!(
            setter_doc.contains("*(compiler-generated)*"),
            "setter doc missing tag"
        );

        // Keyword constructor doc mentions class name and field arg with default
        let ctor = info
            .class_methods
            .iter()
            .find(|m| m.selector == "x:")
            .unwrap();
        let ctor_doc = ctor.doc.as_deref().unwrap();
        assert!(ctor_doc.contains("`Point`"), "ctor doc missing class name");
        assert!(
            ctor_doc.contains("x (default: 0)"),
            "ctor doc missing arg desc"
        );
        assert!(
            ctor_doc.contains("*(compiler-generated)*"),
            "ctor doc missing tag"
        );
    }

    #[test]
    fn value_subclass_auto_methods_respect_user_overrides() {
        let class = ClassDefinition {
            name: Identifier::new("Point", test_span()),
            superclass: Some(Identifier::new("Value", test_span())),
            class_kind: ClassKind::Value,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![StateDeclaration::new(
                Identifier::new("x", test_span()),
                test_span(),
            )],
            // User already defines getter `x`
            methods: vec![MethodDefinition::new(
                crate::ast::MessageSelector::Unary(EcoString::from("x")),
                vec![],
                vec![],
                test_span(),
            )],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };
        let module = Module {
            classes: vec![class],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        let info = h.get_class("Point").expect("Point should be registered");

        // User-defined `x` should appear once, not duplicated by auto-generation
        let x_count = info.methods.iter().filter(|m| m.selector == "x").count();
        assert_eq!(x_count, 1, "getter `x` should appear exactly once");

        // Auto-generated withX: should still be present
        assert!(
            info.methods.iter().any(|m| m.selector == "withX:"),
            "withX: should be auto-generated"
        );
    }

    #[test]
    fn actor_subclass_does_not_set_is_value_flag() {
        let module = Module {
            classes: vec![make_user_class("Counter", "Actor")],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        let info = h
            .get_class("Counter")
            .expect("Counter should be registered");
        assert!(!info.is_value, "Actor subclass should not have is_value");
    }

    /// BT-1056: Test sealed method override detection with external superclasses.
    /// When a class inherits from an external (unknown) class, ancestor lookup
    /// should not break early; it should continue walking up the chain
    /// using the `external_superclasses` data.
    #[test]
    fn sealed_override_checks_with_external_superclasses() {
        // ClassB (in module) has sealed method foo
        let sealed_base = ClassDefinition {
            name: Identifier::new("ClassB", test_span()),
            superclass: Some(Identifier::new("Object", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("foo".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: true,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        // ClassC (in module) inherits from UnknownClass (external)
        // UnknownClass will later be declared to inherit from ClassB
        let override_class = ClassDefinition {
            name: Identifier::new("ClassC", test_span()),
            superclass: Some(Identifier::new("UnknownClass", test_span())),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: crate::ast::MessageSelector::Unary("foo".into()),
                parameters: vec![],
                body: vec![],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: test_span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: test_span(),
        };

        let module = Module {
            classes: vec![sealed_base, override_class],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };

        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };

        // Register external class relationship: UnknownClass <- ClassB
        let mut external_index = std::collections::HashMap::new();
        external_index.insert("UnknownClass".to_string(), "ClassB".to_string());
        h.add_external_superclasses(&external_index);

        // After adding external superclass info, UnknownClass should be in the hierarchy
        // and we should be able to walk from it to ClassB.
        assert!(
            h.get_class("UnknownClass").is_some(),
            "UnknownClass should exist after add_external_superclasses"
        );
        assert_eq!(
            h.superclass_chain("UnknownClass"),
            vec![
                EcoString::from("ClassB"),
                EcoString::from("Object"),
                EcoString::from("ProtoObject")
            ],
            "UnknownClass superclass chain should reach ClassB"
        );
    }

    // --- ADR 0050 Phase 4: add_from_beam_meta tests ---

    #[test]
    fn add_from_beam_meta_inserts_non_builtin_class() {
        let mut h = ClassHierarchy::with_builtins();
        let info = ClassInfo {
            name: EcoString::from("Counter"),
            superclass: Some(EcoString::from("Actor")),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_value: false,
            is_native: false,
            state: vec![EcoString::from("count")],
            state_types: HashMap::new(),
            methods: vec![MethodInfo {
                selector: EcoString::from("value"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: EcoString::from("Counter"),
                is_sealed: false,
                spawns_block: false,
                return_type: Some(EcoString::from("Integer")),
                param_types: vec![],
                doc: None,
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
        };
        h.add_from_beam_meta(vec![info]);
        assert!(h.has_class("Counter"));
        let cls = h.get_class("Counter").unwrap();
        assert_eq!(cls.superclass.as_deref(), Some("Actor"));
        assert_eq!(cls.methods.len(), 1);
        assert_eq!(cls.methods[0].selector.as_str(), "value");
    }

    #[test]
    fn add_from_beam_meta_preserves_existing_entries() {
        let mut h = ClassHierarchy::with_builtins();
        // Simulate AST-derived entry (from ClassHierarchy::build)
        let ast_info = ClassInfo {
            name: EcoString::from("Counter"),
            superclass: Some(EcoString::from("Actor")),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_value: false,
            is_native: false,
            state: vec![EcoString::from("count")],
            state_types: HashMap::new(),
            methods: vec![MethodInfo {
                selector: EcoString::from("increment"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: EcoString::from("Counter"),
                is_sealed: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
        };
        h.classes.insert(EcoString::from("Counter"), ast_info);

        // Stale cache entry with different method
        let cache_info = ClassInfo {
            name: EcoString::from("Counter"),
            superclass: Some(EcoString::from("Object")),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![MethodInfo {
                selector: EcoString::from("old_method"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: EcoString::from("Counter"),
                is_sealed: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
        };
        h.add_from_beam_meta(vec![cache_info]);

        // AST-derived entry should win
        let cls = h.get_class("Counter").unwrap();
        assert_eq!(cls.superclass.as_deref(), Some("Actor"));
        assert_eq!(cls.methods[0].selector.as_str(), "increment");
    }

    #[test]
    fn add_from_beam_meta_skips_builtins() {
        let mut h = ClassHierarchy::with_builtins();
        let original_method_count = h.get_class("Integer").unwrap().methods.len();
        let stub = ClassInfo {
            name: EcoString::from("Integer"),
            superclass: None,
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
        };
        h.add_from_beam_meta(vec![stub]);
        // Built-in should be unchanged
        assert_eq!(
            h.get_class("Integer").unwrap().methods.len(),
            original_method_count
        );
    }

    // --- extract_class_infos tests (BT-1523) ---

    #[test]
    fn extract_class_infos_from_empty_module() {
        let module = Module::new(vec![], crate::source_analysis::Span::default());
        let infos = ClassHierarchy::extract_class_infos(&module);
        assert!(infos.is_empty());
    }

    #[test]
    fn extract_class_infos_captures_methods_and_state() {
        let source = "Actor subclass: Counter\n  state: count :: Integer = 0\n  increment => self.count := self.count + 1\n  getValue => self.count\n";
        let tokens = crate::source_analysis::lex_with_eof(source);
        let (module, _) = crate::source_analysis::parse(tokens);

        let infos = ClassHierarchy::extract_class_infos(&module);
        assert_eq!(infos.len(), 1);

        let info = &infos[0];
        assert_eq!(info.name.as_str(), "Counter");
        assert_eq!(info.superclass.as_deref(), Some("Actor"));
        assert!(info.state.iter().any(|s| s == "count"));
        assert!(info.methods.iter().any(|m| m.selector == "increment"));
        assert!(info.methods.iter().any(|m| m.selector == "getValue"));
    }

    // --- Extension method registration tests (BT-1517) ---

    #[test]
    fn register_extensions_adds_instance_method() {
        use crate::compilation::extension_index::{
            ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
        };
        use std::path::PathBuf;

        let mut h = ClassHierarchy::with_builtins();
        let original_count = h.all_methods("String").len();

        let mut index = ExtensionIndex::new();
        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "shout".into(),
        };
        index.entries_mut().insert(
            key,
            vec![ExtensionLocation {
                file_path: PathBuf::from("String+Shout.bt"),
                span: test_span(),
                type_info: ExtensionTypeInfo {
                    arity: 0,
                    param_types: vec![],
                    return_type: Some("String".into()),
                },
            }],
        );
        h.register_extensions(&index);

        let methods = h.all_methods("String");
        assert_eq!(methods.len(), original_count + 1);
        let ext = methods.iter().find(|m| m.selector == "shout").unwrap();
        assert_eq!(ext.arity, 0);
        assert_eq!(ext.return_type.as_deref(), Some("String"));
        assert_eq!(ext.defined_in.as_str(), "String");
    }

    #[test]
    fn register_extensions_adds_class_method() {
        use crate::compilation::extension_index::{
            ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
        };
        use std::path::PathBuf;

        let mut h = ClassHierarchy::with_builtins();
        let original_count = h.all_class_methods("String").len();

        let mut index = ExtensionIndex::new();
        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Class,
            selector: "fromJson:".into(),
        };
        index.entries_mut().insert(
            key,
            vec![ExtensionLocation {
                file_path: PathBuf::from("String+JSON.bt"),
                span: test_span(),
                type_info: ExtensionTypeInfo {
                    arity: 1,
                    param_types: vec![Some("String".into())],
                    return_type: Some("String".into()),
                },
            }],
        );
        h.register_extensions(&index);

        let methods = h.all_class_methods("String");
        assert_eq!(methods.len(), original_count + 1);
        let ext = methods.iter().find(|m| m.selector == "fromJson:").unwrap();
        assert_eq!(ext.arity, 1);
        assert_eq!(ext.param_types, vec![Some("String".into())]);
    }

    #[test]
    fn register_extensions_unannotated_uses_dynamic() {
        use crate::compilation::extension_index::{
            ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
        };
        use std::path::PathBuf;

        let mut h = ClassHierarchy::with_builtins();

        let mut index = ExtensionIndex::new();
        let key = ExtensionKey {
            class_name: "Array".into(),
            side: MethodSide::Instance,
            selector: "shuffle".into(),
        };
        index.entries_mut().insert(
            key,
            vec![ExtensionLocation {
                file_path: PathBuf::from("Array+Shuffle.bt"),
                span: test_span(),
                type_info: ExtensionTypeInfo {
                    arity: 0,
                    param_types: vec![],
                    return_type: None,
                },
            }],
        );
        h.register_extensions(&index);

        let ext = h
            .find_method("Array", "shuffle")
            .expect("extension should be resolvable");
        assert!(ext.return_type.is_none(), "unannotated = Dynamic");
    }

    #[test]
    fn register_extensions_skips_unknown_class() {
        use crate::compilation::extension_index::{
            ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
        };
        use std::path::PathBuf;

        let mut h = ClassHierarchy::with_builtins();

        let mut index = ExtensionIndex::new();
        let key = ExtensionKey {
            class_name: "NonexistentClass".into(),
            side: MethodSide::Instance,
            selector: "doStuff".into(),
        };
        index.entries_mut().insert(
            key,
            vec![ExtensionLocation {
                file_path: PathBuf::from("Nonexistent+Stuff.bt"),
                span: test_span(),
                type_info: ExtensionTypeInfo {
                    arity: 0,
                    param_types: vec![],
                    return_type: None,
                },
            }],
        );
        // Should not panic
        h.register_extensions(&index);
        assert!(!h.has_class("NonexistentClass"));
    }

    #[test]
    fn register_extensions_does_not_override_existing_method() {
        use crate::compilation::extension_index::{
            ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
        };
        use std::path::PathBuf;

        let mut h = ClassHierarchy::with_builtins();
        // "size" already exists on String
        let original = h.find_method("String", "size").unwrap();

        let mut index = ExtensionIndex::new();
        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "size".into(),
        };
        index.entries_mut().insert(
            key,
            vec![ExtensionLocation {
                file_path: PathBuf::from("String+Size.bt"),
                span: test_span(),
                type_info: ExtensionTypeInfo {
                    arity: 0,
                    param_types: vec![],
                    return_type: Some("Integer".into()),
                },
            }],
        );
        h.register_extensions(&index);

        // Original method should be unchanged
        let after = h.find_method("String", "size").unwrap();
        assert_eq!(after.defined_in, original.defined_in);
    }

    #[test]
    fn register_extensions_resolves_selector() {
        use crate::compilation::extension_index::{
            ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
        };
        use std::path::PathBuf;

        let mut h = ClassHierarchy::with_builtins();
        assert!(
            !h.resolves_selector("String", "toJson"),
            "precondition: toJson doesn't exist"
        );

        let mut index = ExtensionIndex::new();
        let key = ExtensionKey {
            class_name: "String".into(),
            side: MethodSide::Instance,
            selector: "toJson".into(),
        };
        index.entries_mut().insert(
            key,
            vec![ExtensionLocation {
                file_path: PathBuf::from("String+JSON.bt"),
                span: test_span(),
                type_info: ExtensionTypeInfo {
                    arity: 0,
                    param_types: vec![],
                    return_type: Some("String".into()),
                },
            }],
        );
        h.register_extensions(&index);

        assert!(
            h.resolves_selector("String", "toJson"),
            "extension should be resolvable via resolves_selector"
        );
    }

    // --- BT-1528: ClassKind hierarchy propagation tests ---

    #[test]
    fn resolve_class_kind_for_direct_value_subclass() {
        let mut h = ClassHierarchy::with_builtins();
        let mut index = HashMap::new();
        index.insert("Point".to_string(), "Value".to_string());
        h.add_external_superclasses(&index);
        assert_eq!(h.resolve_class_kind("Point"), ClassKind::Value);
    }

    #[test]
    fn resolve_class_kind_for_indirect_value_subclass() {
        let mut h = ClassHierarchy::with_builtins();
        let mut index = HashMap::new();
        // MyValueBase inherits from Value, MyValueChild inherits from MyValueBase
        index.insert("MyValueBase".to_string(), "Value".to_string());
        index.insert("MyValueChild".to_string(), "MyValueBase".to_string());
        h.add_external_superclasses(&index);
        assert_eq!(h.resolve_class_kind("MyValueChild"), ClassKind::Value);
        assert!(h.is_value_subclass("MyValueChild"));
    }

    #[test]
    fn resolve_class_kind_for_direct_actor_subclass() {
        let mut h = ClassHierarchy::with_builtins();
        let mut index = HashMap::new();
        index.insert("Counter".to_string(), "Actor".to_string());
        h.add_external_superclasses(&index);
        assert_eq!(h.resolve_class_kind("Counter"), ClassKind::Actor);
    }

    #[test]
    fn resolve_class_kind_for_indirect_actor_subclass() {
        let mut h = ClassHierarchy::with_builtins();
        let mut index = HashMap::new();
        index.insert("MyActor".to_string(), "Actor".to_string());
        index.insert("SpecialActor".to_string(), "MyActor".to_string());
        h.add_external_superclasses(&index);
        assert_eq!(h.resolve_class_kind("SpecialActor"), ClassKind::Actor);
    }

    #[test]
    fn resolve_class_kind_supervisor_stays_object() {
        // Supervisor inherits from Object (not Actor, not Value)
        let h = ClassHierarchy::with_builtins();
        assert_eq!(h.resolve_class_kind("Supervisor"), ClassKind::Object);
    }

    #[test]
    fn indirect_value_subclass_sets_is_value_in_hierarchy() {
        // MyValueBase inherits from Value, MyValueChild inherits from MyValueBase.
        // MyValueChild should get is_value = true even though its direct superclass
        // is MyValueBase, not Value.
        let module = Module {
            classes: vec![
                make_user_class("MyValueBase", "Value"),
                make_user_class("MyValueChild", "MyValueBase"),
            ],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };

        let base_info = h
            .get_class("MyValueBase")
            .expect("MyValueBase should exist");
        assert!(
            base_info.is_value,
            "MyValueBase (direct Value subclass) should have is_value = true"
        );

        let child_info = h
            .get_class("MyValueChild")
            .expect("MyValueChild should exist");
        assert!(
            child_info.is_value,
            "MyValueChild (indirect Value subclass via MyValueBase) should have is_value = true"
        );
    }

    #[test]
    fn indirect_value_subclass_gets_auto_methods() {
        // MyValueChild inherits from MyValueBase which inherits from Value.
        // MyValueChild should get auto-slot methods synthesized.
        let module = Module {
            classes: vec![
                make_user_class("MyValueBase", "Value"),
                make_user_class("MyValueChild", "MyValueBase"),
            ],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };

        let child_info = h
            .get_class("MyValueChild")
            .expect("MyValueChild should exist");
        // make_user_class gives a "count" state field, so we expect an auto-getter
        assert!(
            child_info
                .methods
                .iter()
                .any(|m| m.selector.as_str() == "count"),
            "MyValueChild should have auto-generated 'count' getter"
        );
        assert!(
            child_info
                .methods
                .iter()
                .any(|m| m.selector.as_str() == "withCount:"),
            "MyValueChild should have auto-generated 'withCount:' setter"
        );
    }

    #[test]
    fn testcase_indirect_value_subclass() {
        // TestCase inherits from Value, MyTest inherits from TestCase.
        // MyTest should get is_value = true.
        let module = Module {
            classes: vec![
                make_user_class("TestCase", "Value"),
                make_user_class("MyTest", "TestCase"),
            ],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };

        assert!(
            h.is_value_subclass("MyTest"),
            "MyTest should be a Value subclass"
        );
        let info = h.get_class("MyTest").expect("MyTest should exist");
        assert!(info.is_value, "MyTest should have is_value = true");
    }

    #[test]
    fn supervisor_subclass_stays_object() {
        // Supervisor inherits from Object. WebApp inherits from Supervisor.
        // WebApp should remain ClassKind::Object.
        let module = Module {
            classes: vec![make_user_class("WebApp", "Supervisor")],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };

        let info = h.get_class("WebApp").expect("WebApp should exist");
        assert!(
            !info.is_value,
            "WebApp (Supervisor subclass) should not have is_value"
        );
        assert_eq!(
            h.resolve_class_kind("WebApp"),
            ClassKind::Object,
            "WebApp should resolve to ClassKind::Object"
        );
    }

    #[test]
    fn cross_file_indirect_value_subclass() {
        // Simulate: File A defines MyValueBase as Value subclass (external).
        // File B defines MyChild as MyValueBase subclass (in module).
        let module = Module {
            classes: vec![make_user_class("MyChild", "MyValueBase")],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };

        // At this point, MyValueBase is unknown, so MyChild has is_value = false
        let info_before = h.get_class("MyChild").expect("MyChild should exist");
        assert!(
            !info_before.is_value,
            "MyChild should not have is_value before external info"
        );

        // Add external superclass info: MyValueBase inherits from Value
        let mut external = HashMap::new();
        external.insert("MyValueBase".to_string(), "Value".to_string());
        h.add_external_superclasses(&external);

        // Now MyChild should resolve as a Value subclass through the hierarchy
        assert!(
            h.is_value_subclass("MyChild"),
            "MyChild should be detected as Value subclass after cross-file info added"
        );
        // The is_value flag should also be propagated
        let info_after = h.get_class("MyChild").expect("MyChild should exist");
        assert!(
            info_after.is_value,
            "MyChild should have is_value = true after external info added"
        );
    }

    #[test]
    fn add_external_superclasses_indirect_value_is_value() {
        // Both classes are added via external index:
        // MyValueBase -> Value, MyValueChild -> MyValueBase
        let mut h = ClassHierarchy::with_builtins();
        let mut index = HashMap::new();
        index.insert("MyValueBase".to_string(), "Value".to_string());
        index.insert("MyValueChild".to_string(), "MyValueBase".to_string());
        h.add_external_superclasses(&index);

        let base_info = h
            .get_class("MyValueBase")
            .expect("MyValueBase should exist");
        assert!(
            base_info.is_value,
            "MyValueBase should have is_value = true"
        );

        let child_info = h
            .get_class("MyValueChild")
            .expect("MyValueChild should exist");
        assert!(
            child_info.is_value,
            "MyValueChild (indirect Value subclass) should have is_value = true via external superclasses"
        );
    }

    /// BT-1559: Cross-file Value sub-subclass should find `new:` via hierarchy walk
    /// and `propagate_cross_file_class_kind` should synthesize auto-slot methods.
    ///
    /// Simulates: file1 has `Value subclass: Base`, file2 has `Base subclass: Child`.
    /// When Child's file is compiled, Base is injected via `add_from_beam_meta`.
    #[test]
    fn cross_file_value_sub_subclass_finds_new() {
        // Build hierarchy from a module containing only Child (extends Base)
        // Give Child a field so auto-slot methods can be verified.
        let mut child_class = make_user_class("Child", "Base");
        child_class.state.push(crate::ast::StateDeclaration {
            name: crate::ast::Identifier {
                name: EcoString::from("count"),
                span: test_span(),
            },
            default_value: None,
            type_annotation: None,
            declared_keyword: crate::ast::DeclaredKeyword::Field,
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        });
        let module = Module {
            classes: vec![child_class],
            method_definitions: vec![],
            protocols: Vec::new(),
            expressions: vec![],
            span: test_span(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };

        // Inject cross-file Base class (from another file: Value subclass: Base)
        let base_info = ClassInfo {
            name: EcoString::from("Base"),
            superclass: Some(EcoString::from("Value")),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_value: true,
            is_native: false,
            state: vec![EcoString::from("x")],
            state_types: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
        };
        h.add_from_beam_meta(vec![base_info]);

        // Before propagation, Child should NOT be marked as Value
        let child_before = h.get_class("Child").unwrap();
        assert!(
            !child_before.is_value,
            "Child should not be is_value before propagation"
        );

        // Run cross-file propagation
        h.propagate_cross_file_class_kind();

        // After propagation, Child should be marked as Value with auto-slot methods
        let child = h.get_class("Child").unwrap();
        assert!(child.is_value, "Child should be is_value after propagation");
        assert!(
            child.methods.iter().any(|m| m.selector == "count"),
            "Child should have auto-generated 'count' getter"
        );
        assert!(
            child.methods.iter().any(|m| m.selector == "withCount:"),
            "Child should have auto-generated 'withCount:' setter"
        );

        // find_class_method should walk: Child → Base → Value and find new:
        let result = h.find_class_method("Child", "new:");
        assert!(
            result.is_some(),
            "find_class_method('Child', 'new:') should find new: on Value via Base"
        );
        assert_eq!(result.unwrap().defined_in.as_str(), "Value");
    }
}
