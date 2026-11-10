// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Classification predicates and hierarchy traversal queries.
//!
//! Methods on `ClassHierarchy` that walk the superclass chain to answer
//! questions about class relationships (is-actor, is-value, is-typed, etc.)
//! and aggregate inherited state/class-variable information.

use crate::ast::{ClassDefinition, ClassKind, MethodKind, Module, SlotKind};
use ecow::EcoString;
use std::collections::{BTreeSet, HashSet};
use std::ops::ControlFlow;

use super::ClassHierarchy;
use super::class_info::{ClassInfo, DirectConstructors, InitializeAssignsSummary, MethodInfo};
use super::declared_type::DeclaredType;

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

        while let Some(info) = self.classes.get(current.as_str()) {
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

    /// Whether `class_name`'s compiled instance methods thread actor `State`
    /// (ADR 0137 amended §1): a non-native class below `Actor` (not `Actor`
    /// itself, whose methods are compiled without actor `State`).
    ///
    /// The one rule behind the static `CompiledSelfSend` and `super`-send
    /// classifications and the `'threads_actor_state'` flag in
    /// `__beamtalk_meta/0`, which `beamtalk_dispatch:inherited_target/2` reads
    /// at run time (BT-4280), so the Rust and Erlang sides cannot disagree.
    #[must_use]
    pub fn threads_actor_state(&self, class_name: &str) -> bool {
        self.get_class(class_name).is_some_and(|c| !c.is_native)
            && class_name != "Actor"
            && self
                .superclass_chain(class_name)
                .iter()
                .any(|c| c.as_str() == "Actor")
    }

    /// Returns the names of all classes whose immediate superclass is
    /// `class_name`.
    ///
    /// Counterpart to `Behaviour subclasses` in the stdlib. The
    /// order is unspecified (it follows hash iteration order); callers that
    /// need a stable presentation sort the result.
    ///
    /// Example: `direct_subclasses("Actor")` → `["Counter", "Logger", ...]`
    /// (every class declared as `Actor subclass: ...`).
    ///
    /// Powers `typeHierarchy/subtypes`. Answers come from the
    /// in-process `ClassHierarchy` index; the LSP
    /// `typeHierarchy/{supertypes,subtypes}` handlers call this directly with
    /// no runtime-attached path — a runtime-attached `nav-query` mode is
    /// deliberately out of scope, kept to the cold-file path.
    #[must_use]
    pub fn direct_subclasses(&self, class_name: &str) -> Vec<EcoString> {
        self.classes
            .iter()
            .filter_map(|(name, info)| {
                info.superclass
                    .as_ref()
                    .filter(|s| s.as_str() == class_name)
                    .map(|_| name.clone())
            })
            .collect()
    }

    /// Returns every class transitively descended from `class_name` (children,
    /// grandchildren, ...), in breadth-first order. The receiver itself is
    /// not included.
    ///
    /// Counterpart to `Behaviour allSubclasses` in the stdlib. The
    /// breadth-first order is stable across runs *within a level* only by
    /// `direct_subclasses`'s hash iteration order; consumers that need a
    /// fully-stable presentation sort the result.
    ///
    /// Cycles are guarded by a `visited` set (a malformed hierarchy with
    /// `A -> B -> A` would otherwise loop forever).
    ///
    /// Powers `typeHierarchy/subtypes`. Answers come from the
    /// in-process `ClassHierarchy` index; the LSP
    /// `typeHierarchy/{supertypes,subtypes}` handlers call this directly with
    /// no runtime-attached path. See [`Self::direct_subclasses`] for the
    /// scope-keeping rationale.
    #[must_use]
    pub fn all_subclasses(&self, class_name: &str) -> Vec<EcoString> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        visited.insert(class_name.to_string());
        let mut queue: std::collections::VecDeque<EcoString> =
            self.direct_subclasses(class_name).into_iter().collect();

        while let Some(name) = queue.pop_front() {
            if !visited.insert(name.to_string()) {
                continue;
            }
            // Enqueue this class's own children before pushing it onto the
            // result — that's what gives the BFS its level-by-level shape.
            for child in self.direct_subclasses(name.as_str()) {
                if !visited.contains(child.as_str()) {
                    queue.push_back(child);
                }
            }
            result.push(name);
        }

        result
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
    ///
    /// codegen (`dispatch_type_pattern_strategy`) trusts this to
    /// decide the actor-tuple tag-test strategy for `x :: SomeClass`, and
    /// relies on it staying consistent with the runtime's own
    /// `beamtalk_supervisor:is_supervisor/1`-style classification of a
    /// *live* value. That consistency holds even when `Actor` is several
    /// hops up the chain and every intermediate ancestor is declared in a
    /// different file: `superclass_chain` walks through the stub `ClassInfo`
    /// entries `add_external_superclasses` inserts for cross-file parents
    /// exactly the same way it walks real ones, and package-wide
    /// compilation always backfills the *whole* project's class → superclass
    /// edges (not just the current file's direct references) before
    /// codegen runs — see `crates/beamtalk-cli/src/commands/build.rs`'s
    /// Pass 1 (`build_class_module_index`). The chain only fails to resolve
    /// (see `has_cross_file_parent`) when an ancestor is missing entirely —
    /// a distinct, pre-existing failure mode this function does not guard
    /// against.
    #[must_use]
    pub fn is_actor_subclass(&self, class_name: &str) -> bool {
        if class_name == "Actor" {
            return true;
        }
        self.superclass_chain(class_name)
            .iter()
            .any(|s| s.as_str() == "Actor")
    }

    /// Whether a standalone extension method (`Target >> sel => ...`) on
    /// `class_name` compiles with the actor method boundary. Anything else
    /// lowers through the value-type body path. Single authority for codegen's
    /// extension lowering and the type checker's value-type method rules
    /// (BT-4235).
    #[must_use]
    pub fn extension_target_is_actor(&self, class_name: &str) -> bool {
        self.is_actor_subclass(class_name)
    }

    /// Returns true if the named class is Supervisor or a subclass of Supervisor.
    ///
    /// Same cross-file-stub-chain invariant as
    /// [`Self::is_actor_subclass`] — see its doc comment.
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

    /// Returns true if the named class is `DynamicSupervisor` or a subclass.
    ///
    /// Same cross-file-stub-chain invariant as
    /// [`Self::is_actor_subclass`] — see its doc comment.
    #[must_use]
    pub fn is_dynamic_supervisor_subclass(&self, class_name: &str) -> bool {
        if class_name == "DynamicSupervisor" {
            return true;
        }
        self.superclass_chain(class_name)
            .iter()
            .any(|s| s.as_str() == "DynamicSupervisor")
    }

    /// Returns true if the named class is declared `native:` (ADR 0056, ADR 0101).
    ///
    /// Native classes are declared with `native: module_name` and delegate to a
    /// backing Erlang module. This covers both native Actors (ADR 0056), which
    /// cannot declare `state:` fields, and native stateless Objects (ADR 0101),
    /// whose `self delegate` methods lower to direct in-process calls.
    #[must_use]
    pub fn is_native(&self, class_name: &str) -> bool {
        self.classes
            .get(class_name)
            .is_some_and(|info| info.is_native)
    }

    /// Whether codegen compiles `class` as an actor (`gen_server`) module rather
    /// than a value-type module — the single authority for that routing, read
    /// by codegen's `is_actor_class` and by the type checker's value-type
    /// method rules (BT-4235: a trailing `self.x := v` answers the updated
    /// `self`).
    ///
    /// - A concrete (non-abstract) `Supervisor`/`DynamicSupervisor` subclass is
    ///   an actor (supervisor codegen is routed through the actor path).
    /// - Otherwise [`Self::resolve_class_kind`] decides: `Actor` is an actor,
    ///   `Value` is not, and `Object` is not on a fully-known chain. A chain
    ///   with an unregistered ancestor ([`Self::has_cross_file_parent`])
    ///   defaults to an actor, for independent per-file compilation.
    #[must_use]
    pub fn compiles_as_actor(&self, class: &ClassDefinition) -> bool {
        if class.supervisor_kind.is_some() && !class.is_abstract {
            return true;
        }
        let name = class.name.name.as_str();
        match self.resolve_class_kind(name) {
            ClassKind::Actor => true,
            ClassKind::Value => false,
            ClassKind::Object => self.has_cross_file_parent(name),
        }
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

    /// Returns true if any class in the superclass chain (including the class
    /// itself) was extracted from a file with parse errors.
    ///
    /// Parser error recovery can silently drop method definitions, so a
    /// marked class's recorded method surface may be incomplete. The
    /// receiver-knowledge classifier (ADR 0100 Rule 1) treats such receivers
    /// as `Open` so an under-recovered surface never produces false
    /// unresolved-selector hints. The walk stops at the first unknown
    /// ancestor — a missing parent is `has_cross_file_parent`'s concern.
    #[must_use]
    pub fn has_incomplete_surface_in_chain(&self, class_name: &str) -> bool {
        let mut current = class_name;
        let mut visited = HashSet::new();

        while let Some(info) = self.classes.get(current) {
            if info.surface_incomplete {
                return true;
            }

            let Some(ref superclass) = info.superclass else {
                return false; // Reached root — whole chain checked
            };

            if !visited.insert(current.to_string()) {
                return false; // Cycle guard
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

    /// Returns true if `self.slot :=` in an instance method of the named class
    /// is the "Cannot assign to slot" compile error: a `Value` subclass that is
    /// not a `TestCase` subclass (`TestCase` is a permanent exception, see
    /// `check_value_slot_assignment`). The one decision both that validator and
    /// the ADR 0133 escaping-block check read (BT-3922).
    #[must_use]
    pub fn rejects_slot_assignment(&self, class_name: &str) -> bool {
        self.is_value_subclass(class_name) && !self.is_testcase_subclass(class_name)
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

    /// Returns true if the named class is `TestCase` or a subclass of `TestCase`.
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

    /// Resolve the `ClassKind` for a class by walking its ancestor chain.
    ///
    /// This is the **single authority** for actor/value classification
    /// — the only place that should decide whether a class is `Actor`, `Value`,
    /// or `Object`. Returns `ClassKind::Actor` if any ancestor is `Actor`,
    /// `ClassKind::Value` if any ancestor is `Value`, or `ClassKind::Object`
    /// otherwise, walking the *full* transitive chain regardless of how many
    /// hops away the deciding ancestor is or what order sibling classes were
    /// declared in (`add_module_classes` registers every class in a module
    /// before any chain is walked, so within-module order never matters).
    ///
    /// This supersedes `ClassKind::from_superclass_name`, which is a
    /// pre-analysis placeholder that only checks the direct superclass
    /// literal — nothing downstream of `apply_class_kind_writeback` should
    /// treat that placeholder as authoritative once this method is available.
    ///
    /// **Incomplete-chain default:** when the chain terminates before
    /// reaching a registered root (an ancestor is missing from this
    /// `ClassHierarchy` entirely — see [`Self::has_cross_file_parent`]),
    /// neither the `Actor` nor `Value` literal can be found, so this method
    /// returns `ClassKind::Object`. That is the one default this method
    /// documents; callers that need a *different* fallback for a genuinely
    /// incomplete chain (for example, codegen's `is_actor_class` defaults
    /// such classes to actor, for backward compatibility with independent
    /// per-file compilation — see its doc comment) must layer that policy on
    /// top of this method's result themselves, using
    /// [`Self::has_cross_file_parent`] to detect the incomplete-chain case,
    /// rather than re-deriving it from a separate hardcoded list.
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

    /// Resolve a class's declared sendability handle scope (ADR 0103).
    ///
    /// Returns the bare scope symbol (e.g. `"process"`, `"node"`) declared via
    /// a class-side `handleScope:` clause on the class or, failing that, the
    /// nearest ancestor that declares one — handle scope is inherited like
    /// other class metadata. Returns `None` when neither the class nor any
    /// ancestor declares a scope (the class stays tier `Unknown`, silent).
    #[must_use]
    pub fn handle_scope(&self, class_name: &str) -> Option<&EcoString> {
        if let Some(scope) = self
            .get_class(class_name)
            .and_then(|c| c.handle_scope.as_ref())
        {
            return Some(scope);
        }
        self.superclass_chain(class_name)
            .iter()
            .find_map(|s| self.get_class(s).and_then(|c| c.handle_scope.as_ref()))
    }

    /// Check whether a class defines its own class-side method with the given selector.
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

    /// After all classes in a module are registered, propagate `is_value`
    /// by walking each class's ancestor chain. Classes that indirectly inherit
    /// from Value (e.g. `Value subclass: MyBase` then `MyBase subclass: MyChild`)
    /// get their `is_value` flag corrected and their auto-slot methods
    /// synthesized.
    pub(crate) fn propagate_class_kind(&mut self, module: &Module) {
        // Collect names of classes that need is_value = true but don't have it yet.
        // A class an earlier run already fixed is skipped: re-synthesizing
        // would find its keyword constructor present and misread it as a
        // user collision, clearing the recorded keyword decision.
        let classes_to_fix: Vec<EcoString> = module
            .classes
            .iter()
            .filter(|class| {
                class.class_kind != ClassKind::Value
                    && self
                        .classes
                        .get(class.name.name.as_str())
                        .is_some_and(|info| !info.is_value)
                    && self.is_value_subclass(class.name.name.as_str())
            })
            .map(|class| class.name.name.clone())
            .collect();

        for class_name in &classes_to_fix {
            if let Some(info) = self.classes.get_mut(class_name.as_str()) {
                info.is_value = true;
            }
        }

        // Synthesize auto-slot methods for newly-discovered Value subclasses
        // and record their constructor anchor (BT-4132): codegen gives them
        // full value treatment (`apply_class_kind_writeback`), so they are as
        // eligible for direct construction as a direct `Value subclass:`.
        // We need the AST `ClassDefinition` for slot info, so look it up.
        // `Value` itself (reached here when compiling `value.bt`) is not a
        // Value subclass: it keeps its methods and its `None` anchor.
        for class in &module.classes {
            if classes_to_fix.contains(&class.name.name) {
                let anchored = self
                    .classes
                    .get(class.name.name.as_str())
                    .is_some_and(|info| self.ancestors_include_value(info.superclass.as_ref()));
                if let Some(info) = self.classes.get_mut(class.name.name.as_str()) {
                    if anchored {
                        info.synthesize_value_surface(class);
                    } else {
                        let _ = Self::add_value_auto_methods(
                            class,
                            &mut info.methods,
                            &mut info.class_methods,
                        );
                    }
                }
            }
        }
        if !classes_to_fix.is_empty() {
            self.rebuild_all_indexes();
        }
    }

    /// Re-propagate `is_value` for ALL classes in the hierarchy,
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
            let slot_data: Option<Vec<(EcoString, Option<DeclaredType>)>> =
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
                let keyword_decision = Self::synthesize_auto_methods_from_state(
                    class_name,
                    &slots,
                    &inst_sels,
                    &cls_sels,
                    &mut info.methods,
                    &mut info.class_methods,
                );
                // BT-4132: anchor only where the stored state determines the
                // keyword decision, and never over an anchor the info already
                // carries (e.g. a generated stdlib builtin's, decided from its
                // AST); otherwise leave `None` (fail closed).
                if info.auto_ctors.is_none() {
                    if let KeywordDecision::Known(keyword_constructor) = keyword_decision {
                        info.record_constructor_anchor(keyword_constructor);
                    }
                }
            }
        }
        self.rebuild_all_indexes();
    }

    /// Check if the superclass chain includes Value.
    pub(super) fn ancestors_include_value(&self, superclass: Option<&EcoString>) -> bool {
        superclass_chain_includes_value(superclass.map(EcoString::as_str), |name| {
            self.classes
                .get(name)
                .and_then(|info| info.superclass.as_deref())
        })
    }

    /// Constructor direct-call eligibility for the named class.
    ///
    /// A `Value` constructor builds a plain map without touching the class
    /// process, so when the class is a stateless non-native `Value` whose
    /// constructors are all synthesized, sends to them compile (static) or
    /// dispatch (dynamic) to a direct module call in the caller:
    /// - `new` needs no ancestor walk: the `{new, []}` handler calls the
    ///   class's own `Module:new/0` unconditionally, so an ancestor's user
    ///   `new` is shadowed identically in both paths.
    /// - `new:` dispatches through the chain at runtime
    ///   (`has_class_new_in_chain`), so any user `new:` in the class or its
    ///   ancestors disables the fast path — except `Value`'s own, which is
    ///   the `@intrinsic basicNewWith` router that just forwards to the
    ///   receiver's `new/1` (pinned by a test parsing `value.bt`). The
    ///   ancestors' part is checked again at run time, from the facts the
    ///   ancestors publish (`defines_user_class_new1`, BT-4409), so an
    ///   ancestor that gains a user `new:` after this class was compiled is
    ///   honoured; this compile-time walk can only keep the verdict off.
    /// - keyword constructors ignore `ClassSelf` and resolve own-first, so
    ///   only the class's own synthesis record matters.
    ///
    /// Abstractness is decided here: an abstract class gets no direct
    /// constructors (BT-4200). A caller's static `X new` is guarded by the
    /// BT-4316 static call key `{Module, new, Tag}` (BT-4409), installed only
    /// while the class's published `direct_constructors` still lists `new`.
    /// Reloading the class as `abstract` publishes `[]`, which erases the key
    /// (`beamtalk_class_metadata:put_direct_constructors/2`), so the next
    /// send takes `class_send` and gets the `gen_server` `{new, _}` abstract
    /// rejection without recompiling the caller. The callee cannot reject it
    /// itself, because a Value subclass's generated `new/0` chains through
    /// its superclass module's `new/0` to collect inherited field defaults
    /// (and `beamtalk_class_instantiation:ancestor_compiled_defaults/1` calls
    /// it too), so an abstract class's `new/0` must stay a plain map builder.
    ///
    /// Unknown classes, unknown ancestors, cycles, chains deeper than the
    /// runtime's cap, and parse-incomplete surfaces (where a missing user
    /// `new` would be misread as "synthesized") all fail closed.
    #[must_use]
    pub fn direct_constructors(&self, class_name: &str) -> DirectConstructors {
        let mut result = DirectConstructors::default();
        let Some(info) = self.classes.get(class_name) else {
            return result;
        };
        if info.surface_incomplete {
            return result;
        }
        let Some(auto_ctors) = info.auto_ctors.as_ref() else {
            return result;
        };
        if !info.class_variables.is_empty() {
            return result;
        }
        result.keyword.clone_from(&auto_ctors.keyword_constructor);
        if info.is_abstract || info.is_native {
            return result;
        }
        let has_own = |selector: &str| {
            info.methods.iter().any(|m| m.selector.as_str() == selector)
                || info
                    .class_methods
                    .iter()
                    .any(|m| m.selector.as_str() == selector)
        };
        if !has_own("new") {
            result.new0 = true;
        }
        if !has_own("new:") && !self.chain_has_user_new_init(class_name) {
            result.new1 = true;
        }
        result
    }

    /// BT-4464: whether `class`'s own explicit class-side `new` may build in
    /// the caller, the explicit-constructor counterpart of the synthesized
    /// `new0` verdict of [`Self::direct_constructors`]: the `new` case of
    /// [`Self::explicit_class_constructor_builds_in_caller`].
    ///
    /// A class that defines `class new => <body>` (and no instance-side
    /// `new`) compiles a `new/0` that delegates to the body
    /// (`generate_delegating_new` in codegen: `class_new('undefined')`), and
    /// the class process's `{new, []}` handler only ever calls that `new/0`.
    /// Codegen bakes the verdict into `direct_constructors` and every direct
    /// `new` is guarded at run time (BT-4409,
    /// `beamtalk_class_dispatch:lookup_direct_constructor/3`).
    #[must_use]
    pub fn explicit_class_new_builds_in_caller(&self, class: &ClassDefinition) -> bool {
        self.explicit_class_constructor_builds_in_caller(class, "new")
    }

    /// BT-4475: whether `class`'s own explicit class-side `new:` may run in
    /// the caller as an ordinary class method: the `new:` case of
    /// [`Self::explicit_class_constructor_builds_in_caller`].
    ///
    /// `new:` with a non-map argument has no direct constructor
    /// (`Module:new/1` is the synthesized map initialiser), and the class
    /// process's `{new, [Arg]}` handler only calls the class method `new:`
    /// (`has_class_new_in_chain`). So codegen drops `new:` from this class's
    /// published `process_bound_class_selectors`, and a `new:` send takes the
    /// ADR 0142 in-caller path (`beamtalk_class_dispatch:in_caller_owner/2`)
    /// with the real class reference as `self`, behind the same run-time
    /// guard (`beamtalk_class_shadow_flags:direct_call_ok/2`) as every
    /// in-caller class method. The published list only ever speaks for the
    /// receiving class: a subclass inheriting this `new:` publishes its own,
    /// complete list and keeps the class process.
    #[must_use]
    pub fn explicit_class_new1_runs_in_caller(&self, class: &ClassDefinition) -> bool {
        self.explicit_class_constructor_builds_in_caller(class, "new:")
    }

    /// The one rule behind [`Self::explicit_class_new_builds_in_caller`]
    /// (`new`, BT-4464) and [`Self::explicit_class_new1_runs_in_caller`]
    /// (`new:`, BT-4475): whether `class`'s own explicit class-side
    /// `selector` needs nothing from the class process. Both selectors are
    /// process-bound (ADR 0142 rule 3), yet the class process adds nothing
    /// but a serialisation point whenever the body does not need it: the body
    /// is a single statement that is
    ///
    /// - (`new` only) `self delegate` on a `native:` class (lowered to
    ///   `beamtalk_erlang_proxy:native_call/5` on the backing module). Not
    ///   for `new:`: no `new:` delegate qualifies today, and the one in the
    ///   stdlib (`AtomicCounter new:`) creates a named ETS table owned by
    ///   whichever process runs it, which must stay the class process;
    /// - a direct `(Erlang mod) fn` FFI call whose arguments are all literals
    ///   (or, for `new:`, the method's own parameter) and which is not a row
    ///   of the curated class-method FFI table read by the
    ///   `class-process-ffi` / `class-method-process-resource` lints
    ///   (`ets new:`, `erlang self`, `spawn_link`, `gen_udp open:`, ...):
    ///   those keep the class process, which owns what they create. FFI
    ///   outside that table is not recognised, exactly as for the lints, and
    ///   runs in the caller like any other in-caller class method;
    /// - (`new:` only) a send to `self`, with literal or parameter
    ///   arguments, of one of the class's own class methods that already runs
    ///   in the caller ([`Self::is_direct_call_eligible`]: not process-bound,
    ///   no class variables in the chain) and whose own body passes this test
    ///   one level down: a plain FFI forward as above, or its own selector
    ///   primitive (`class withAll: list => @primitive`, the target of the
    ///   `List class new: elements => self withAll: elements` shape). The
    ///   target's body is checked, not just its routing: a target that is
    ///   `self delegate` or a process-sensitive FFI call would hand what it
    ///   creates to the caller exactly as a direct one would. A forward to a
    ///   forward is not recognised. `new` cannot forward to `self`: its
    ///   direct path is the `new/0` delegate, whose `self` is `undefined`;
    ///
    /// so it reads no class variable and nothing from the class process's
    /// dictionary (`basicNew` does, which is why `self basicNew` is never
    /// eligible). The class must also be concrete, compile as a non-actor,
    /// have a fully known surface, no instance-side method of the same
    /// selector and no class variables in its chain. This only makes the
    /// class eligible to *try* the in-caller path: a class-side extension or
    /// ADR 0084 runtime fun of the selector still reaches the class process
    /// through the run-time guard. As for any in-caller class method
    /// (ADR 0142), a resource the body creates belongs to the caller.
    #[must_use]
    pub fn explicit_class_constructor_builds_in_caller(
        &self,
        class: &ClassDefinition,
        selector: &str,
    ) -> bool {
        Self::explicit_class_constructor_shape(class, selector)
            && self.constructor_chain_allows_in_caller(class)
    }

    /// The chain-dependent half of
    /// [`Self::explicit_class_constructor_builds_in_caller`]: what only the
    /// hierarchy can answer about `class` (no class variables in the chain,
    /// not an actor, a fully known surface). Evaluated at query time against
    /// whatever hierarchy is in hand, so a sibling-file ancestor injected
    /// later is seen (BT-4640).
    fn constructor_chain_allows_in_caller(&self, class: &ClassDefinition) -> bool {
        let class_name = class.name.name.as_str();
        !self.compiles_as_actor(class)
            && !self.chain_has_class_variables(class_name)
            && self
                .get_class(class_name)
                .is_some_and(|info| !info.surface_incomplete)
    }

    /// [`Self::constructor_chain_allows_in_caller`] for a class known only by
    /// its published [`ClassInfo`] (a sibling file's, injected cross-file):
    /// the same conditions, but `ClassInfo` carries no `supervisor_kind`, which
    /// [`Self::explicit_class_constructor_shape`] has already folded into the
    /// published fact.
    pub(super) fn info_constructor_chain_allows_in_caller(&self, class_name: &str) -> bool {
        let Some(info) = self.get_class(class_name) else {
            return false;
        };
        !info.surface_incomplete
            && !info.is_abstract
            && !self.chain_has_class_variables(class_name)
            && match self.resolve_class_kind(class_name) {
                ClassKind::Actor => false,
                ClassKind::Value => true,
                ClassKind::Object => !self.has_cross_file_parent(class_name),
            }
    }

    /// The chain-independent half of
    /// [`Self::explicit_class_constructor_builds_in_caller`]: everything
    /// decided by `class`'s own AST (BT-4640). It is what `from_class_definition`
    /// publishes as [`ClassInfo::in_caller_new1`] for `new:`, so it survives
    /// Pass 1 extraction and cross-file callers combine it with their own view
    /// of the chain ([`Self::info_constructor_chain_allows_in_caller`]).
    #[must_use]
    pub fn explicit_class_constructor_shape(class: &ClassDefinition, selector: &str) -> bool {
        use crate::ast::Expression;
        fn primary<'m>(
            methods: &'m [crate::ast::MethodDefinition],
            selector: &str,
        ) -> Option<&'m crate::ast::MethodDefinition> {
            methods
                .iter()
                .find(|m| m.kind == MethodKind::Primary && m.selector.name() == selector)
        }
        let keyword = match selector {
            "new" => false,
            "new:" => true,
            _ => return false,
        };
        if class.is_abstract || class.supervisor_kind.is_some() {
            return false;
        }
        if primary(&class.methods, selector).is_some() {
            return false;
        }
        let Some(method) = primary(&class.class_methods, selector) else {
            return false;
        };
        if !keyword && class.backing_module.is_some() && method.is_self_delegate() {
            return true;
        }
        let [stmt] = method.body.as_slice() else {
            return false;
        };
        if plain_ffi_forward(method, &stmt.expression, keyword) {
            return true;
        }
        // `new:` only: `self <sel> <args>`, `<sel>` one of the class's own
        // in-caller class methods whose own body passes the same test one
        // level down (a plain FFI forward, or its selector primitive). Never
        // further: a forward to a forward is not recognised.
        let Expression::MessageSend {
            receiver,
            selector: sent,
            arguments,
            ..
        } = &stmt.expression
        else {
            return false;
        };
        let sent = sent.name();
        keyword
            && matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
            && arguments
                .iter()
                .all(|arg| plain_argument(method, arg, keyword))
            && !ClassInfo::is_process_bound_class_selector(&sent)
            && !ClassInfo::is_class_var_reflection_selector(&sent)
            && primary(&class.class_methods, &sent).is_some_and(|target| {
                matches!(target.body.as_slice(), [only] if
                plain_ffi_forward(target, &only.expression, true)
                || matches!(&only.expression, Expression::Primitive {
                    is_inferred: true,
                    is_intrinsic: false,
                    ..
                }))
            })
    }

    /// Whether any ancestor of the named class defines a class-side `new:`
    /// that is real user code ([`Self::defines_user_class_new1`], asked of
    /// each ancestor).
    ///
    /// The compile-time half of the `new:` chain rule: it only ever keeps the
    /// baked `direct_constructors` verdict off. The run-time half,
    /// `beamtalk_class_metadata:chain_has_user_new1/1`, walks the same
    /// per-class fact as each ancestor publishes it (BT-4409), so an ancestor
    /// reloaded with a user `new:` turns a descendant's fast path off without
    /// recompiling the descendant.
    /// Mirrors the runtime's `has_class_new_in_chain` depth cap (50);
    /// anything unknown fails closed so the `new:` fast path stays off.
    fn chain_has_user_new_init(&self, class_name: &str) -> bool {
        let mut current = self
            .classes
            .get(class_name)
            .and_then(|info| info.superclass.clone());
        let mut visited = HashSet::new();
        let mut depth = 0;
        while let Some(name) = current.as_ref() {
            if depth >= 50 || !visited.insert((*name).clone()) {
                return true;
            }
            depth += 1;
            if self.defines_user_class_new1(name.as_str()) {
                return true;
            }
            let Some(info) = self.classes.get(name.as_str()) else {
                return true;
            };
            current.clone_from(&info.superclass);
        }
        false
    }

    /// Whether the named class itself defines a class-side `new:` that is
    /// real user code: the one per-class rule behind both the compile-time
    /// chain walk ([`Self::chain_has_user_new_init`]) and the fact baked into
    /// each class's `__beamtalk_meta/0` as `defines_user_class_new1`, which
    /// the runtime's `new:` chain check walks (BT-4409).
    ///
    /// `Value` answers `false`: its `new:` is the `@intrinsic basicNewWith`
    /// router, not an override (skipping by name is sound: `Value` is a
    /// runtime-protected class name, so no user class can shadow it). An
    /// unknown class, or one whose surface is incomplete (error recovery can
    /// drop method definitions, so it may hide a user `new:`), answers `true`:
    /// fail closed, like the leaf check in `direct_constructors`.
    #[must_use]
    pub fn defines_user_class_new1(&self, class_name: &str) -> bool {
        let Some(info) = self.classes.get(class_name) else {
            return true;
        };
        if info.surface_incomplete {
            return true;
        }
        class_name != "Value"
            && info
                .class_methods
                .iter()
                .any(|m| m.selector.as_str() == "new:")
    }

    /// Synthesize auto-generated slot methods from `ClassInfo` state fields.
    ///
    /// This is the `ClassInfo`-only equivalent of `add_value_auto_methods` which
    /// works from the AST `ClassDefinition`.
    ///
    /// Returns the keyword decision (BT-4132): known for a slot-less class
    /// (no keyword constructor) and when the keyword constructor was
    /// synthesized here; ambiguous when the class side already holds the
    /// selector, which may be a user method or an earlier synthesis, so the
    /// caller leaves the anchor unset.
    pub(super) fn synthesize_auto_methods_from_state(
        class_name: &EcoString,
        slots: &[(EcoString, Option<DeclaredType>)],
        existing_instance_selectors: &HashSet<EcoString>,
        existing_class_selectors: &HashSet<EcoString>,
        instance_methods: &mut Vec<MethodInfo>,
        class_methods: &mut Vec<MethodInfo>,
    ) -> KeywordDecision {
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
                    origin: None,
                    primitive: None,
                    constant_value: None,
                });
            }

            // Auto with*: setter
            let with_sel =
                EcoString::from(crate::synthetic_selectors::with_star_selector(slot_name));
            if !existing_instance_selectors.contains(&with_sel) {
                instance_methods.push(MethodInfo {
                    selector: with_sel,
                    arity: 1,
                    kind: MethodKind::Primary,
                    defined_in: class_name.clone(),
                    is_sealed: false,
                    is_internal: false,
                    spawns_block: false,
                    return_type: Some(crate::synthetic_selectors::synthesized_copy_return_type(
                        class_name,
                    )),
                    param_types: vec![None],
                    doc: Some(EcoString::from("*(compiler-generated)*")),
                    origin: None,
                    primitive: None,
                    constant_value: None,
                });
            }
        }

        // Keyword constructor (class method). Without the AST, a selector
        // already on the class side is either a user collision or an earlier
        // synthesis: the keyword decision is not derivable, so no anchor.
        if slots.is_empty() {
            return KeywordDecision::Known(None);
        }
        let Some(kw_sel) = crate::synthetic_selectors::synthesized_keyword_constructor(
            slots.iter().map(|(name, _)| name.as_str()),
            |sel| existing_class_selectors.contains(sel),
        ) else {
            return KeywordDecision::Ambiguous;
        };
        let kw_sel = EcoString::from(kw_sel);
        class_methods.push(MethodInfo {
            selector: kw_sel.clone(),
            arity: slots.len(),
            kind: MethodKind::Primary,
            defined_in: class_name.clone(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(crate::synthetic_selectors::synthesized_copy_return_type(
                class_name,
            )),
            param_types: vec![None; slots.len()],
            doc: Some(EcoString::from("*(compiler-generated)*")),
            origin: None,
            primitive: None,
            constant_value: None,
        });
        KeywordDecision::Known(Some(kw_sel))
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

    /// Returns the class variables `class_name` itself declares with
    /// `classState:` — own declarations only, never an ancestor's.
    ///
    /// Class variables are per class, not inherited (ADR 0013 §1; ADR 0130
    /// amendment "Not inherited, per-class ownership", BT-3762). This is the
    /// single answer to "which class variables does the class being compiled
    /// have?", shared by class-side extensions, the `class-state-abroad` lint
    /// and the `classState:` / migration checks, so codegen and the lint cannot
    /// disagree. An inherited method running through a subclass reads and writes
    /// the *subclass's* variables, which is why the subclass redeclares them.
    #[must_use]
    pub fn own_class_variable_names(&self, class_name: &str) -> Vec<EcoString> {
        self.classes
            .get(class_name)
            .map(|info| {
                info.class_variables
                    .iter()
                    .map(|cv| cv.name.clone())
                    .collect()
            })
            .unwrap_or_default()
    }

    /// The nearest strict ancestor of `class_name` that declares the class
    /// variable `var_name`, if any. Diagnostic-only: it names the class whose
    /// variable a subclass method tried to reach, so the error can say "redeclare
    /// it with `classState:`" (BT-3762). It never makes `var_name` accessible.
    #[must_use]
    pub fn inherited_class_variable_owner(
        &self,
        class_name: &str,
        var_name: &str,
    ) -> Option<EcoString> {
        self.superclass_chain(class_name)
            .into_iter()
            .find(|ancestor| {
                self.classes.get(ancestor.as_str()).is_some_and(|info| {
                    info.class_variables
                        .iter()
                        .any(|cv| cv.name.as_str() == var_name)
                })
            })
    }

    /// Returns all state (instance variable) names for a class,
    /// including inherited state from the superclass chain.
    ///
    /// A field a subclass shadows (redeclares under the same name) appears
    /// only once, at its most-derived occurrence — one field name is one
    /// storage slot per instance regardless of how many ancestors in the
    /// chain redeclare it, and every by-name query (`state_field_type`,
    /// `state_field_kind`, `state_field_has_default`) already resolves a
    /// shadowed name to that same most-derived declaration. Without this,
    /// a caller that emits one diagnostic per name returned here (e.g.
    /// [`TypeChecker::check_value_construction_definite_assignment`]) would
    /// double-report a shadowed field.
    #[must_use]
    pub fn all_state(&self, class_name: &str) -> Vec<EcoString> {
        let mut state = Vec::new();
        let mut seen_names = HashSet::new();
        let mut visited = HashSet::new();
        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                for field in &info.state {
                    if seen_names.insert(field.clone()) {
                        state.push(field.clone());
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
        state
    }

    /// Shared superclass-chain walk behind [`Self::state_field_type`],
    /// [`Self::state_field_kind`], [`Self::class_variable_kind`] and
    /// [`Self::class_variable_type`] (AGENTS.md "No duplicate
    /// implementations": these four used to each hand-roll the identical
    /// `visited`/`current` cycle-guarded loop below).
    ///
    /// Calls `settle` on each ancestor's [`ClassInfo`] in order (`class_name`
    /// itself first, then each superclass). `settle` returns
    /// `ControlFlow::Break(value)` from the first class that "owns" the
    /// thing being looked up — the shared shadowing rule: a subclass
    /// redeclaring a field/variable is its owner, even when its own value is
    /// absent (e.g. an untyped redeclaration must NOT fall through to an
    /// ancestor's type) — and the walk stops there, returning `value` as-is
    /// (which may itself be `None`). `ControlFlow::Continue(())` keeps
    /// walking up. Returns `None` if the chain is exhausted, or a cycle or
    /// missing class is hit, without any class settling the matter; each
    /// caller applies its own default (`SlotKind::Eager` for the two
    /// kind-returning queries, plain `None` for the two type-returning
    /// ones).
    fn walk_superclass_chain<R>(
        &self,
        class_name: &str,
        mut settle: impl FnMut(&ClassInfo) -> ControlFlow<Option<R>>,
    ) -> Option<R> {
        let mut visited = HashSet::new();
        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            let Some(info) = self.classes.get(name.as_str()) else {
                break;
            };
            if let ControlFlow::Break(value) = settle(info) {
                return value;
            }
            current = info
                .superclass
                .as_ref()
                .map(std::string::ToString::to_string);
        }
        None
    }

    /// Returns the declared type annotation for a state field, walking
    /// the superclass chain to find inherited field types.
    ///
    /// Returns `None` if the field has no type annotation, the field does not
    /// exist, or the class is unknown.
    #[must_use]
    pub fn state_field_type(&self, class_name: &str, field_name: &str) -> Option<DeclaredType> {
        // If this class declares the field, its type settles the matter (or
        // `None` if untyped) — a subclass redeclaring a field without a type
        // must NOT inherit the parent's type annotation.
        self.walk_superclass_chain(class_name, |info| {
            if info.state.iter().any(|s| s == field_name) {
                ControlFlow::Break(info.state_types.get(field_name).cloned())
            } else {
                ControlFlow::Continue(())
            }
        })
    }

    /// Returns whether a state field carries an explicit default value,
    /// walking the superclass chain to find the declaring class — the same
    /// shadowing rule as [`Self::state_field_type`]/[`Self::state_field_kind`]:
    /// a subclass redeclaring a field is the field's owner.
    ///
    /// Missing metadata for a known field (an older cross-file
    /// `__beamtalk_meta/0` predating `state_has_default`) degrades to `true`
    /// — conservative, so stale metadata never produces a false
    /// definite-assignment diagnostic — mirroring
    /// `beamtalk-codegen`'s `inherited_typed_no_default_fields` cross-file
    /// arm (ADR 0124 §6, §7). An unknown class/field also degrades to `true`
    /// for the same reason.
    #[must_use]
    pub fn state_field_has_default(&self, class_name: &str, field_name: &str) -> bool {
        let mut visited = HashSet::new();
        let mut current = Some(class_name.to_string());
        while let Some(name) = current {
            if !visited.insert(name.clone()) {
                break;
            }
            if let Some(info) = self.classes.get(name.as_str()) {
                if info.state.iter().any(|s| s == field_name) {
                    return info
                        .state_has_default
                        .get(field_name)
                        .copied()
                        .unwrap_or(true);
                }
                current = info
                    .superclass
                    .as_ref()
                    .map(std::string::ToString::to_string);
            } else {
                break;
            }
        }
        true
    }

    /// Returns the slot kind (`#eager` | `#late`, ADR 0124 §1) for a state
    /// field, walking the superclass chain to find inherited fields.
    ///
    /// Mirrors [`Self::state_field_type`]'s shadowing rule: a subclass
    /// redeclaring a field is the field's owner, so the walk stops at the
    /// first class that declares the field, not the first class with a
    /// `state_kinds` entry for it. Missing metadata (an older cross-file
    /// `__beamtalk_meta/0` predating this key, or an unknown class/field)
    /// degrades to [`SlotKind::Eager`] — the pre-ADR-0124 behaviour every
    /// slot had.
    #[must_use]
    pub fn state_field_kind(&self, class_name: &str, field_name: &str) -> SlotKind {
        self.walk_superclass_chain(class_name, |info| {
            if info.state.iter().any(|s| s == field_name) {
                ControlFlow::Break(Some(
                    info.state_kinds
                        .get(field_name)
                        .copied()
                        .unwrap_or(SlotKind::Eager),
                ))
            } else {
                ControlFlow::Continue(())
            }
        })
        .unwrap_or(SlotKind::Eager)
    }

    /// Returns the slot kind (`#eager` | `#late`, ADR 0124 §1) for a class
    /// variable (`classState:` declaration), walking the superclass chain.
    ///
    /// Class-side counterpart to [`Self::state_field_kind`]; same shadowing
    /// and missing-metadata-degrades-to-`#eager` rules.
    #[must_use]
    pub fn class_variable_kind(&self, class_name: &str, var_name: &str) -> SlotKind {
        self.walk_superclass_chain(class_name, |info| {
            info.class_variables
                .iter()
                .find(|cv| cv.name == var_name)
                .map_or(ControlFlow::Continue(()), |cv| {
                    ControlFlow::Break(Some(cv.kind))
                })
        })
        .unwrap_or(SlotKind::Eager)
    }

    /// Returns the declared type annotation for a class variable
    /// (`classState:` declaration), walking the superclass chain.
    ///
    /// Class-side counterpart to [`Self::state_field_type`]; same shadowing
    /// rule (a subclass redeclaring the variable is its owner) and `None`
    /// result for an untyped variable, a variable that doesn't exist, or an
    /// unknown class. ADR 0124 B3 uses this to name the declared type in a
    /// `late` class variable's `UninitializedStateError` hint.
    #[must_use]
    pub fn class_variable_type(&self, class_name: &str, var_name: &str) -> Option<DeclaredType> {
        self.walk_superclass_chain(class_name, |info| {
            info.class_variables
                .iter()
                .find(|cv| cv.name == var_name)
                .map_or(ControlFlow::Continue(()), |cv| {
                    ControlFlow::Break(cv.ty.clone())
                })
        })
    }

    /// Composed "`initialize` definitely assigns" summary for a class,
    /// flattened parent-first over the ADR 0078 chain (ADR 0124 A2a) —
    /// unions each chain link's own [`ClassInfo::initialize_assigns`], the
    /// same walk [`Self::superclass_chain`]-based accessors already use and
    /// the same composition `inherited_typed_no_default_fields`
    /// (`beamtalk-codegen`) already performs for `state_has_default`.
    ///
    /// Union, not intersection: each class's own summary only covers what
    /// *its own* `initialize` assigns, but ADR 0078 auto-chains every
    /// ancestor's `initialize` to run, parent-first, before the leaf's own —
    /// so a slot an ancestor definitely assigns is definitely assigned by the
    /// time the whole chain finishes, regardless of the leaf.
    ///
    /// [`InitializeAssignsSummary::incomplete`] is set when a `native:`
    /// ancestor (ADR 0056 — its slots belong to a backing `gen_server` with
    /// no `initialize` AST to analyse) appears anywhere in the chain, or the
    /// chain has a missing link (see [`Self::has_cross_file_parent`]) — both
    /// mean `assigned` may be missing slots a caller would otherwise expect,
    /// not merely that nothing is assigned.
    #[must_use]
    pub fn all_initialize_assigns(&self, class_name: &str) -> InitializeAssignsSummary {
        let mut ordered: Vec<EcoString> = self
            .superclass_chain(class_name)
            .into_iter()
            .rev()
            .collect();
        ordered.push(EcoString::from(class_name));

        let mut assigned = BTreeSet::new();
        let mut incomplete = self.has_cross_file_parent(class_name);
        let mut has_dynamic_writer = false;
        for name in &ordered {
            if matches!(name.as_str(), "Actor" | "Object" | "ProtoObject") {
                continue;
            }
            let Some(info) = self.classes.get(name.as_str()) else {
                incomplete = true;
                continue;
            };
            if info.is_native {
                incomplete = true;
            }
            if info.has_dynamic_field_writer {
                has_dynamic_writer = true;
            }
            assigned.extend(info.initialize_assigns.iter().cloned());
        }

        InitializeAssignsSummary {
            assigned,
            incomplete,
            has_dynamic_writer,
        }
    }

    /// Synthesizes auto-generated slot methods for a `Value subclass:` class
    /// and appends them to `instance_methods` / `class_methods`.
    ///
    /// Selector names come from [`crate::synthetic_selectors`], the shared naming
    /// authority also used by `codegen::core_erlang::value_type_codegen`, so the
    /// class hierarchy (and therefore the type checker) agrees byte-for-byte with
    /// the methods codegen emits — without duplicating the naming logic.
    #[allow(clippy::too_many_lines)] // field-mapping function — length is proportional to slot count
    /// Returns the synthesized keyword constructor selector, if one was added.
    ///
    /// The return value is the constructor-emission record: callers
    /// building a fresh `ClassInfo` store it as the `auto_ctors` anchor.
    pub(super) fn add_value_auto_methods(
        class: &ClassDefinition,
        instance_methods: &mut Vec<MethodInfo>,
        class_methods: &mut Vec<MethodInfo>,
    ) -> Option<EcoString> {
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
                    return_type: crate::synthetic_selectors::synthesized_getter_return_type(slot),
                    param_types: vec![],
                    doc: Some(getter_doc.into()),
                    origin: None,
                    primitive: None,
                    constant_value: None,
                });
            }

            // Auto with*: setter: keyword method returning a new instance
            let with_sel =
                EcoString::from(crate::synthetic_selectors::with_star_selector(slot_name));
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
                    return_type: Some(crate::synthetic_selectors::synthesized_copy_return_type(
                        &class_name,
                    )),
                    param_types: vec![None],
                    doc: Some(setter_doc.into()),
                    origin: None,
                    primitive: None,
                    constant_value: None,
                });
            }
        }

        // Auto keyword constructor on the class side (e.g., `x:y:`)
        let kw_sel: EcoString =
            EcoString::from(crate::synthetic_selectors::synthesized_keyword_constructor(
                class.state.iter().map(|s| s.name.name.as_str()),
                |sel| user_class_selectors.contains(sel),
            )?);
        let arity = class.state.len();
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
        let ctor_doc =
            format!("Creates a new `{class_name}`. Args: {args_desc}.\n\n*(compiler-generated)*");
        class_methods.push(MethodInfo {
            selector: kw_sel.clone(),
            arity,
            kind: MethodKind::Primary,
            defined_in: class_name.clone(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(crate::synthetic_selectors::synthesized_copy_return_type(
                &class_name,
            )),
            param_types: vec![None; arity],
            doc: Some(ctor_doc.into()),
            origin: None,
            primitive: None,
            constant_value: None,
        });
        Some(kw_sel)
    }
}

/// Whether the superclass chain starting at `superclass` reaches `Value`,
/// following `superclass_of` and stopping at a cycle or an unknown class: the
/// strict "is a Value subclass" test over a class's ancestors (`Value` itself
/// does not pass, since its own chain never reaches it).
///
/// Shared by [`ClassHierarchy`]'s propagation passes and the stdlib builtins
/// generator (`build_stdlib.rs`), which has only its per-file metadata, not a
/// hierarchy: both anchor the constructor record on this rule (BT-4132).
pub fn superclass_chain_includes_value<'a>(
    superclass: Option<&'a str>,
    superclass_of: impl Fn(&str) -> Option<&'a str>,
) -> bool {
    let mut current = superclass;
    let mut visited = HashSet::new();
    while let Some(name) = current {
        if !visited.insert(name) {
            break;
        }
        if name == "Value" {
            return true;
        }
        current = superclass_of(name);
    }
    false
}

/// A Value class's synthesized keyword-constructor decision as
/// [`ClassHierarchy::synthesize_auto_methods_from_state`] can derive it from
/// stored state (BT-4132).
pub(super) enum KeywordDecision {
    /// The decision is known: the synthesized selector, or `None` when the
    /// class has no keyword constructor.
    Known(Option<EcoString>),
    /// The class side already holds the selector (a user method or an
    /// earlier synthesis): not derivable, so no anchor.
    Ambiguous,
}

/// An argument of a constructor forward that reads nothing: a literal, or
/// (when `params_ok`, for `new:`) one of `method`'s own parameters
/// (`ClassHierarchy::explicit_class_constructor_builds_in_caller`).
fn plain_argument(
    method: &crate::ast::MethodDefinition,
    arg: &crate::ast::Expression,
    params_ok: bool,
) -> bool {
    match arg {
        crate::ast::Expression::Literal(..) => true,
        crate::ast::Expression::Identifier(id) => {
            params_ok && method.parameters.iter().any(|p| p.name.name == id.name)
        }
        _ => false,
    }
}

/// Whether `expr`, the whole body of `method`, is a direct `(Erlang mod) fn`
/// call with plain arguments ([`plain_argument`]) that is not a row of the
/// curated process-sensitive class-method FFI table
/// (`ClassHierarchy::explicit_class_constructor_builds_in_caller`).
fn plain_ffi_forward(
    method: &crate::ast::MethodDefinition,
    expr: &crate::ast::Expression,
    params_ok: bool,
) -> bool {
    let crate::ast::Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = expr
    else {
        return false;
    };
    crate::ffi_receiver::erlang_module_of_receiver(receiver).is_some_and(|module| {
        !crate::semantic_analysis::validators::ffi_call_is_process_sensitive(module, selector)
    }) && arguments
        .iter()
        .all(|arg| plain_argument(method, arg, params_ok))
}
