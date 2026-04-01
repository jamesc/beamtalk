// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Classification predicates and hierarchy traversal queries.
//!
//! Methods on `ClassHierarchy` that walk the superclass chain to answer
//! questions about class relationships (is-actor, is-value, is-typed, etc.)
//! and aggregate inherited state/class-variable information.

use crate::ast::{ClassDefinition, ClassKind, MethodKind, Module};
use ecow::EcoString;
use std::collections::HashSet;

use super::ClassHierarchy;
use super::class_info::MethodInfo;

impl ClassHierarchy {
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

    /// Returns true if the named generic class has covariant type parameters (ADR 0068 Phase 2f).
    ///
    /// A class is covariant in its type parameters when:
    /// - It is a sealed Value subclass (immutable, no state mutation)
    /// - It has type parameters
    ///
    /// Actor classes are always invariant because their state fields can be mutated,
    /// which would break soundness if covariance were allowed.
    ///
    /// Non-sealed classes are treated as invariant (conservative) because subclasses
    /// could add mutating methods on the type parameter.
    ///
    /// **References:** ADR 0068 Phase 2f
    #[must_use]
    pub fn is_covariant_class(&self, class_name: &str) -> bool {
        let Some(class_info) = self.get_class(class_name) else {
            return false;
        };
        // Must have type params to be relevant
        if class_info.type_params.is_empty() {
            return false;
        }
        // Actors are invariant — state mutation breaks covariance
        if self.is_actor_subclass(class_name) {
            return false;
        }
        // Sealed Value subclasses are covariant (immutable)
        class_info.is_sealed && self.is_value_subclass(class_name)
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
        self.class_method_indexes
            .get(class_name)
            .is_some_and(|idx| idx.contains_key(selector))
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
        if !classes_to_fix.is_empty() {
            self.rebuild_all_indexes();
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
        self.rebuild_all_indexes();
    }

    /// Check if the superclass chain includes Value.
    pub(super) fn ancestors_include_value(&self, superclass: Option<&EcoString>) -> bool {
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
    pub(super) fn synthesize_auto_methods_from_state(
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
                    is_internal: false,
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
                    is_internal: false,
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
                    is_internal: false,
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

    /// Synthesizes auto-generated slot methods for a `Value subclass:` class
    /// and appends them to `instance_methods` / `class_methods`.
    ///
    /// Mirrors the selector-computation logic in
    /// `codegen::core_erlang::value_type_codegen::compute_auto_slot_methods` so
    /// that the class hierarchy (and therefore the type checker) knows about
    /// these methods before codegen runs.
    #[allow(clippy::too_many_lines)] // field-mapping function — length is proportional to slot count
    pub(super) fn add_value_auto_methods(
        class: &ClassDefinition,
        instance_methods: &mut Vec<MethodInfo>,
        class_methods: &mut Vec<MethodInfo>,
    ) {
        use super::class_info::format_default_value;

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
                    is_internal: false,
                    spawns_block: false,
                    return_type: slot
                        .type_annotation
                        .as_ref()
                        .map(crate::ast::TypeAnnotation::type_name),
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
                    is_internal: false,
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
                    is_internal: false,
                    spawns_block: false,
                    return_type: Some(class_name.clone()),
                    param_types: vec![None; arity],
                    doc: Some(ctor_doc.into()),
                });
            }
        }
    }
}
