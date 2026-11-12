// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Protocol registry and conformance checking (ADR 0068 Phase 2b).
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module implements:
//! - A protocol registry mapping protocol names to their required message sets
//! - A conformance engine that checks class method tables against protocol requirements
//! - Three-tier conformance model: compile-time hierarchy, REPL workspace, DNU bypass
//!
//! **Key design decisions:**
//! - Structural conformance: classes conform by having the right methods, not by declaration
//! - Protocols and classes share a namespace — collisions are compile errors
//! - DNU override classes automatically conform to all protocols
//! - Warnings, never errors, for type-level conformance failures (ADR 0025)
//!
//! **References:**
//! - `docs/ADR/0068-parametric-types-and-protocols.md` — Stage 2
//! - `docs/ADR/0025-gradual-typing-and-protocols.md` — Diagnostic philosophy

use crate::ast::{
    MessageSelector, Module, ParameterDefinition, ProtocolDefinition, TypeAnnotation,
};
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::receiver_knowledge;
use crate::semantic_analysis::trait_expansion;
use crate::semantic_analysis::type_checker::TypeChecker;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;
use std::collections::HashMap;

/// Information about a required method in a protocol.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolMethodRequirement {
    /// Full selector name (e.g., "asString", "do:", "< ").
    pub selector: EcoString,
    /// Number of arguments.
    pub arity: usize,
    /// Optional return type annotation.
    pub return_type: Option<EcoString>,
    /// Parameter type annotations. `None` elements mean the parameter type is unspecified.
    pub param_types: Vec<Option<EcoString>>,
}

impl ProtocolMethodRequirement {
    /// The requirement a declared signature states — one source for how a
    /// required (`ProtocolMethodSignature`) and a provided
    /// (`MethodDefinition`) method become a `ProtocolMethodRequirement`.
    fn from_signature(
        selector: &MessageSelector,
        return_type: Option<&TypeAnnotation>,
        parameters: &[ParameterDefinition],
    ) -> Self {
        Self {
            selector: selector.name(),
            arity: selector.arity(),
            return_type: return_type.map(TypeAnnotation::type_name),
            param_types: parameters
                .iter()
                .map(|p| p.type_annotation.as_ref().map(TypeAnnotation::type_name))
                .collect(),
        }
    }

    /// The requirement's declared return type, when it has one that can be
    /// judged without a substitution context: `None` when it is
    /// unannotated or names one of `type_params` (the type parameters of
    /// the protocol and those it extends). The one gate every
    /// return-type-refinement check against a requirement applies (BT-4175,
    /// BT-4441).
    #[must_use]
    pub fn judged_return_type(&self, type_params: &[EcoString]) -> Option<DeclaredType> {
        self.return_type
            .as_deref()
            .map(DeclaredType::parse)
            .filter(|ty| !ty.mentions_any(type_params))
    }
}

/// A protocol requirement a class answers with an incompatible declared
/// return type — see [`ProtocolRegistry::conformance_return_type_mismatches`].
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnTypeMismatch {
    /// The requirement's (or provision's) selector (e.g. `asString`).
    pub selector: EcoString,
    /// `true` for a `class` requirement, resolved against the class's
    /// class-side methods (BT-4441); `false` for an instance requirement.
    pub class_side: bool,
    /// The class whose method the selector resolves to.
    pub defined_in: EcoString,
    /// The resolved method's declared return type.
    pub actual: DeclaredType,
    /// The protocol requirement's or provision's declared return type.
    pub expected: DeclaredType,
}

/// Information about a protocol in the registry.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolInfo {
    /// Protocol name (e.g., "Printable", "Collection").
    pub name: EcoString,
    /// Type parameters for generic protocols (e.g., `["E"]` for `Collection(E)`).
    ///
    /// Empty for non-generic protocols.
    pub type_params: Vec<EcoString>,
    /// Protocol bounds for each type parameter (ADR 0068 Phase 2d).
    ///
    /// Parallel to `type_params`. `None` = unbounded, `Some("Printable")` = bounded.
    pub type_param_bounds: Vec<Option<EcoString>>,
    /// Protocol this one extends, if any (e.g., `Comparable` for `Sortable`).
    pub extending: Option<EcoString>,
    /// Required instance method signatures.
    pub methods: Vec<ProtocolMethodRequirement>,
    /// Required class method signatures.
    pub class_methods: Vec<ProtocolMethodRequirement>,
    /// Provided (instance-side) method signatures (ADR 0127 §1, §8) — the
    /// selector and declared types of each provision. A provision's body
    /// implements it, so only its declared *return type* is checked against
    /// a conformer (BT-4490,
    /// [`ProtocolRegistry::conformance_return_type_mismatches`]); parameter
    /// types are not. A
    /// protocol's *type* is required ∪ provided (§8, decided), so their
    /// selectors — via [`Self::all_conformance_selectors`] — join `methods`
    /// in every conformance check; `Protocol requiredMethods:` keeps
    /// answering only `methods`, unaffected. The declared return type is
    /// what lets a provision refine a requirement of another protocol in
    /// [`Self::subsumes_conformance_to`] (BT-4490). Class-side provisions
    /// are post-v1 (§9), so there is no class-side counterpart.
    pub provided: Vec<ProtocolMethodRequirement>,
    /// The dependency package this protocol was carried in from, or `None`
    /// for the package being compiled (and for a REPL/fixture entry). Stamped
    /// by the loader that carries a dependency's protocols, like
    /// [`ProtocolDefinition::package`]; [`ProtocolRegistry::add_pre_loaded`]
    /// reads it to apply the `uses:` shadowing rule (BT-3941).
    pub package: Option<EcoString>,
    /// Source span of the protocol definition (for diagnostics).
    pub span: Span,
}

impl ProtocolInfo {
    /// Build a `ProtocolInfo` from a parsed `ProtocolDefinition` AST node.
    fn from_definition(def: &ProtocolDefinition) -> Self {
        let make_req = |sig: &crate::ast::ProtocolMethodSignature| {
            ProtocolMethodRequirement::from_signature(
                &sig.selector,
                sig.return_type.as_ref(),
                &sig.parameters,
            )
        };

        let methods = def.method_signatures.iter().map(make_req).collect();
        let provided = def
            .provided_methods
            .iter()
            .map(|m| {
                ProtocolMethodRequirement::from_signature(
                    &m.selector,
                    m.return_type.as_ref(),
                    &m.parameters,
                )
            })
            .collect();
        let class_methods = def.class_method_signatures.iter().map(make_req).collect();

        Self {
            name: def.name.name.clone(),
            type_params: def
                .type_params
                .iter()
                .map(|tp| tp.name.name.clone())
                .collect(),
            type_param_bounds: def
                .type_params
                .iter()
                .map(|tp| tp.bound.as_ref().map(|b| b.name.clone()))
                .collect(),
            extending: def.extending.as_ref().map(|id| id.name.clone()),
            methods,
            class_methods,
            provided,
            package: def.package.clone(),
            span: def.span,
        }
    }

    /// Returns all required selectors, including those inherited from extended protocols.
    ///
    /// When a protocol extends another (e.g., `Sortable extending: Comparable`),
    /// the conforming class must implement methods from both protocols.
    pub fn all_required_selectors<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
    ) -> Vec<&'a EcoString> {
        let mut selectors: Vec<&EcoString> = self.methods.iter().map(|m| &m.selector).collect();
        let mut visited = std::collections::HashSet::new();
        self.collect_parent_selectors(registry, &mut selectors, &mut visited);
        selectors
    }

    /// Returns all provided selectors, including those inherited from
    /// extended protocols (ADR 0127 §5, "`extending:` a protocol with
    /// provisions" — `Q extending: P`, P's provisions are part of Q's type
    /// too, though they flatten only into a class that `uses: P` directly,
    /// per §1's "one protocol using another is post-v1").
    #[must_use]
    pub fn all_provided_selectors<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
    ) -> Vec<&'a EcoString> {
        self.all_provided(registry)
            .into_iter()
            .map(|req| &req.selector)
            .collect()
    }

    /// Returns all provided signatures, including those inherited from
    /// extended protocols — [`Self::all_provided_selectors`] with the
    /// declared types kept (BT-4490). A provision of this protocol shadows
    /// a same-named one of a parent.
    #[must_use]
    pub fn all_provided<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
    ) -> Vec<&'a ProtocolMethodRequirement> {
        let mut provided: Vec<&ProtocolMethodRequirement> = self.provided.iter().collect();
        let mut visited = std::collections::HashSet::new();
        visited.insert(self.name.clone());
        self.collect_parent_provided(registry, &mut provided, &mut visited);
        provided
    }

    /// Recursively collects provided signatures from parent protocols, with
    /// cycle detection — the provided mirror of
    /// [`Self::collect_parent_selectors`].
    fn collect_parent_provided<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
        provided: &mut Vec<&'a ProtocolMethodRequirement>,
        visited: &mut std::collections::HashSet<EcoString>,
    ) {
        if let Some(ref parent_name) = self.extending {
            if !visited.insert(parent_name.clone()) {
                return; // Cycle detected — stop recursion
            }
            if let Some(parent) = registry.get(parent_name) {
                for req in &parent.provided {
                    if !provided.iter().any(|r| r.selector == req.selector) {
                        provided.push(req);
                    }
                }
                parent.collect_parent_provided(registry, provided, visited);
            }
        }
    }

    /// Returns the protocol's full *type* — required ∪ provided selectors,
    /// both transitively through `extending:` (ADR 0127 §8, decided). This
    /// is the set every conformance check (`conformsTo:`, `:: T`
    /// annotations, intersections and bounds) uses; `Protocol
    /// requiredMethods:` and [`Self::all_required_selectors`] keep
    /// answering only the required half.
    #[must_use]
    pub fn all_conformance_selectors<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
    ) -> Vec<&'a EcoString> {
        let mut selectors = self.all_required_selectors(registry);
        for sel in self.all_provided_selectors(registry) {
            if !selectors.contains(&sel) {
                selectors.push(sel);
            }
        }
        selectors
    }

    /// Recursively collects selectors from parent protocols, with cycle detection.
    fn collect_parent_selectors<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
        selectors: &mut Vec<&'a EcoString>,
        visited: &mut std::collections::HashSet<EcoString>,
    ) {
        if let Some(ref parent_name) = self.extending {
            if !visited.insert(parent_name.clone()) {
                return; // Cycle detected — stop recursion
            }
            if let Some(parent) = registry.get(parent_name) {
                for sel in parent.methods.iter().map(|m| &m.selector) {
                    if !selectors.contains(&sel) {
                        selectors.push(sel);
                    }
                }
                parent.collect_parent_selectors(registry, selectors, visited);
            }
        }
    }

    /// This protocol followed by every protocol it transitively `extending:`s,
    /// nearest first, stopping at a cycle or an unknown parent.
    fn extending_chain<'a>(&'a self, registry: &'a ProtocolRegistry) -> Vec<&'a ProtocolInfo> {
        let mut chain = vec![self];
        let mut visited = std::collections::HashSet::new();
        visited.insert(self.name.clone());
        let mut current = self;
        while let Some(parent) = current
            .extending
            .as_ref()
            .filter(|name| visited.insert((*name).clone()))
            .and_then(|name| registry.get(name))
        {
            chain.push(parent);
            current = parent;
        }
        chain
    }

    /// `true` when every class conforming to this protocol provably conforms
    /// to `other` too (ADR 0144: `P class <: Q class`): this protocol's
    /// instance-side conformance selectors and class-side required selectors
    /// each include `other`'s. Structural, like
    /// [`ProtocolRegistry::check_conformance`] — the same two selector sets
    /// `missing_conformance_selectors` checks a class against — so a protocol
    /// that `extending:`s `other` qualifies, and so does one that merely
    /// restates `other`'s requirements.
    ///
    /// Return types count too (BT-4441): every requirement of `other`, on
    /// either side, that declares a return type must be matched by a
    /// requirement of this protocol whose declared return type refines it —
    /// the same covariance rule a class satisfying a requirement meets
    /// ([`ProtocolRegistry::conformance_return_type_mismatches`],
    /// [`TypeChecker::is_return_type_refinement`]). A requirement of this
    /// protocol with no declared return type promises nothing, so it does
    /// not refine a declared one. A *provided* method (ADR 0127) counts as a
    /// requirement here too, with its declared return type (BT-4490): a
    /// conformer must answer it, and it is the protocol's own body that
    /// supplies the type, so `P` providing `name -> String` subsumes `Q`
    /// requiring `name -> String`. The same goes for `other`'s side: its
    /// provisions' declared return types are judged like its requirements',
    /// matching [`ProtocolRegistry::conformance_return_type_mismatches`]. A
    /// return type naming a type parameter of
    /// either protocol has no substitution context here and is not judged.
    #[must_use]
    pub fn subsumes_conformance_to(
        &self,
        other: &ProtocolInfo,
        registry: &ProtocolRegistry,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        let instance = self.all_conformance_selectors(registry);
        let class_side = self.all_required_class_selectors(registry);
        let selectors_subsumed = other
            .all_conformance_selectors(registry)
            .iter()
            .all(|sel| instance.contains(sel))
            && other
                .all_required_class_selectors(registry)
                .iter()
                .all(|sel| class_side.contains(sel));
        if !selectors_subsumed {
            return false;
        }
        let mut type_params = self.all_type_params(registry);
        type_params.extend(other.all_type_params(registry));
        let refines = |mine: Vec<&ProtocolMethodRequirement>,
                       theirs: Vec<&ProtocolMethodRequirement>| {
            theirs.iter().all(|req| {
                let Some(expected) = req.judged_return_type(&type_params) else {
                    return true;
                };
                // Selector presence was checked above, and `mine` holds this
                // protocol's provisions as well as its requirements.
                let mut entries = mine
                    .iter()
                    .filter(|r| r.selector == req.selector)
                    .peekable();
                if entries.peek().is_none() {
                    return false;
                }
                // Every entry of the protocol's type binds a conformer, so
                // the strongest typed one is what a user of `mine` can rely
                // on: some entry must refine `expected`.
                entries.any(|own| {
                    own.return_type
                        .as_deref()
                        .map(DeclaredType::parse)
                        .is_some_and(|actual| {
                            actual.mentions_any(&type_params)
                                || TypeChecker::is_return_type_refinement(
                                    &actual, &expected, hierarchy,
                                )
                        })
                })
            })
        };
        // Requirements and provisions side by side: a provision's own type
        // is not hidden by an untyped requirement of the same selector.
        let mut instance_side = self.all_requirements(registry);
        instance_side.extend(self.all_provided(registry));
        // `other`'s provisions carry declared types too (ADR 0127 §8), the
        // same entries `conformance_return_type_mismatches` judges a class by.
        let mut theirs = other.all_requirements(registry);
        theirs.extend(other.all_provided(registry));
        refines(instance_side, theirs)
            && refines(
                self.all_class_requirements(registry),
                other.all_class_requirements(registry),
            )
    }

    /// The type parameters of this protocol and of every protocol it
    /// transitively extends — the names a requirement from
    /// [`Self::all_requirements`] may mention without a substitution
    /// context (BT-4239).
    #[must_use]
    pub fn all_type_params(&self, registry: &ProtocolRegistry) -> Vec<EcoString> {
        let mut params: Vec<EcoString> = Vec::new();
        for protocol in self.extending_chain(registry) {
            for param in &protocol.type_params {
                if !params.contains(param) {
                    params.push(param.clone());
                }
            }
        }
        params
    }

    /// Returns all method requirements, including those inherited from extended protocols.
    pub fn all_requirements<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
    ) -> Vec<&'a ProtocolMethodRequirement> {
        let mut reqs: Vec<&ProtocolMethodRequirement> = self.methods.iter().collect();

        if let Some(ref parent_name) = self.extending {
            if let Some(parent) = registry.get(parent_name) {
                for req in parent.all_requirements(registry) {
                    if !reqs.iter().any(|r| r.selector == req.selector) {
                        reqs.push(req);
                    }
                }
            }
        }

        reqs
    }

    /// Returns all required class method selectors, including from extended protocols.
    pub fn all_required_class_selectors<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
    ) -> Vec<&'a EcoString> {
        let mut selectors: Vec<&EcoString> =
            self.class_methods.iter().map(|m| &m.selector).collect();

        if let Some(ref parent_name) = self.extending {
            if let Some(parent) = registry.get(parent_name) {
                for sel in parent.all_required_class_selectors(registry) {
                    if !selectors.contains(&sel) {
                        selectors.push(sel);
                    }
                }
            }
        }

        selectors
    }

    /// Returns all class method requirements, including from extended protocols.
    pub fn all_class_requirements<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
    ) -> Vec<&'a ProtocolMethodRequirement> {
        let mut reqs: Vec<&ProtocolMethodRequirement> = self.class_methods.iter().collect();

        if let Some(ref parent_name) = self.extending {
            if let Some(parent) = registry.get(parent_name) {
                for req in parent.all_class_requirements(registry) {
                    if !reqs.iter().any(|r| r.selector == req.selector) {
                        reqs.push(req);
                    }
                }
            }
        }

        reqs
    }

    /// The `class` requirement named `selector`, own or inherited (see
    /// [`Self::all_class_requirements`]) — the requirement a send of
    /// `selector` to a `P class` value resolves against (ADR 0144).
    #[must_use]
    pub fn find_class_requirement<'a>(
        &'a self,
        registry: &'a ProtocolRegistry,
        selector: &str,
    ) -> Option<&'a ProtocolMethodRequirement> {
        self.all_class_requirements(registry)
            .into_iter()
            .find(|req| req.selector.as_str() == selector)
    }
}

/// Registry of protocol definitions for compile-time conformance checking.
///
/// **DDD Context:** Semantic Analysis — Domain Service
///
/// Maps protocol names to their required message sets. Built during
/// semantic analysis alongside the `ClassHierarchy`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProtocolRegistry {
    protocols: HashMap<EcoString, ProtocolInfo>,
}

impl ProtocolRegistry {
    /// Creates an empty protocol registry.
    #[must_use]
    pub fn new() -> Self {
        Self {
            protocols: HashMap::new(),
        }
    }

    /// Extract `ProtocolInfo` entries from a parsed module without registering them.
    ///
    /// Mirrors `ClassHierarchy::extract_class_infos` — used by the
    /// `BUnit` test pipeline (and any other caller that pre-scans fixture
    /// files) to collect protocol metadata ahead of compiling a downstream
    /// module that references those protocol names.
    #[must_use]
    pub fn extract_protocol_infos(module: &Module) -> Vec<ProtocolInfo> {
        module
            .protocols
            .iter()
            .map(ProtocolInfo::from_definition)
            .collect()
    }

    /// Seed the registry with protocols pre-compiled from other source files.
    ///
    /// The `BUnit` compile path parses fixture files, extracts their
    /// `ProtocolInfo`s via `extract_protocol_infos`, and injects them here so
    /// the unresolved-class validator and type checker recognise fixture-only
    /// protocol names when analysing the test module. Skips entries whose
    /// names are already registered (current-module definitions win).
    ///
    /// Returns diagnostics for namespace collisions — pre-loaded protocol
    /// names that match a class in the current hierarchy. Colliding entries
    /// are skipped so resolution cannot pick them up (classes win over
    /// cross-file protocols that shadow them).
    ///
    /// A bare name follows the `uses:` precedence (BT-3941): an entry that
    /// would let a dependency's protocol shadow a stdlib protocol of the same
    /// name (`trait_expansion::is_dependency_shadow`) is replaced by the
    /// stdlib's own protocol, exactly as `uses: Name` resolves. `current_package`
    /// is the package being compiled (`None` when unknown).
    pub fn add_pre_loaded(
        &mut self,
        protocols: Vec<ProtocolInfo>,
        hierarchy: &ClassHierarchy,
        current_package: Option<&str>,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        for info in protocols {
            let info = if trait_expansion::is_dependency_shadow(
                &info.name,
                info.package.as_deref(),
                current_package,
            ) {
                match trait_expansion::resolve_stdlib_protocol(&info.name, current_package) {
                    Some(stdlib) => ProtocolInfo::from_definition(stdlib),
                    None => continue,
                }
            } else {
                info
            };
            if hierarchy.has_class(&info.name) {
                // Skip collision when the existing "class" is a synthetic
                // protocol class entry (superclass is Protocol) — this is just the
                // class-side representation of the same protocol from a prior load.
                let is_protocol_class = hierarchy
                    .get_class(&info.name)
                    .and_then(|ci| ci.superclass.as_deref())
                    .is_some_and(|sc| sc == "Protocol");
                if !is_protocol_class {
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "Pre-loaded protocol `{}` collides with class of the same name — \
                                 protocols and classes share a namespace",
                                info.name
                            ),
                            info.span,
                        )
                        .with_hint(format!(
                            "Rename the protocol in its defining file to avoid conflicting with class `{}`",
                            info.name
                        ))
                        .with_category(DiagnosticCategory::Type),
                    );
                    continue;
                }
            }
            self.protocols.entry(info.name.clone()).or_insert(info);
        }
        diagnostics
    }

    /// Seeds the registry with `pre_loaded` cross-file protocols, then
    /// registers `module`'s own — the one registration order every surface
    /// uses (analysis Phase 0.5, and the language service's hover/completion
    /// type pass).
    ///
    /// A pre-loaded entry named like a protocol `module` itself declares is
    /// dropped, so the current module's definition wins and is not reported
    /// as a duplicate (a fixture file compiled against its own pre-scanned
    /// protocols, or an editor buffer whose saved copy is also indexed).
    /// `current_package` is passed to [`Self::add_pre_loaded`]. Returns the
    /// diagnostics of both steps.
    pub fn seed_and_register_module(
        &mut self,
        module: &Module,
        pre_loaded: Vec<ProtocolInfo>,
        hierarchy: &ClassHierarchy,
        current_package: Option<&str>,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        if !pre_loaded.is_empty() {
            let current_names: std::collections::HashSet<&EcoString> =
                module.protocols.iter().map(|p| &p.name.name).collect();
            let cross_file: Vec<_> = pre_loaded
                .into_iter()
                .filter(|p| !current_names.contains(&p.name))
                .collect();
            diagnostics.extend(self.add_pre_loaded(cross_file, hierarchy, current_package));
        }
        if !module.protocols.is_empty() {
            diagnostics.extend(self.register_module(module, hierarchy));
        }
        diagnostics
    }

    /// Registers protocols from a parsed module.
    ///
    /// Returns diagnostics for:
    /// - Namespace collisions (protocol name matches an existing class name)
    /// - Duplicate protocol definitions
    /// - Unknown parent protocols in `extending:` clauses
    pub fn register_module(
        &mut self,
        module: &Module,
        hierarchy: &ClassHierarchy,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for protocol_def in &module.protocols {
            let name = &protocol_def.name.name;

            // Namespace collision: protocol name matches a class name.
            // Allow re-registration when the existing "class" is actually
            // a synthetic protocol class entry from a previous load (superclass is
            // `Protocol`). This happens during hot-reload when the compiler server's
            // class cache still contains the protocol from the first surface that
            // loaded it. Real class vs. protocol collisions (superclass != Protocol)
            // remain errors.
            if hierarchy.has_class(name) {
                let is_protocol_class = hierarchy
                    .get_class(name)
                    .and_then(|info| info.superclass.as_deref())
                    .is_some_and(|sc| sc == "Protocol");
                if !is_protocol_class {
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "`{name}` is already defined as a class — \
                                 protocols and classes share a namespace"
                            ),
                            protocol_def.name.span,
                        )
                        .with_hint(format!(
                            "Rename the protocol to avoid conflicting with class `{name}`"
                        ))
                        .with_category(DiagnosticCategory::Type),
                    );
                    continue;
                }
            }

            // Duplicate protocol definition (pre-loaded entries from other
            // files are in the map, so this read is a consulted dependency)
            crate::semantic_analysis::consulted::record_name(name);
            if self.protocols.contains_key(name) {
                diagnostics.push(
                    Diagnostic::error(
                        format!("Duplicate protocol definition: `{name}`"),
                        protocol_def.name.span,
                    )
                    .with_hint(
                        "Remove the duplicate definition — each protocol can only be defined once",
                    )
                    .with_category(DiagnosticCategory::Type),
                );
                continue;
            }

            let info = ProtocolInfo::from_definition(protocol_def);

            // Validate extending clause
            if let Some(ref parent_name) = info.extending {
                crate::semantic_analysis::consulted::record_name(parent_name);
                if !self.protocols.contains_key(parent_name) {
                    diagnostics.push(
                        Diagnostic::error(
                            format!("Protocol `{name}` extends unknown protocol `{parent_name}`"),
                            protocol_def.span,
                        )
                        .with_hint(format!(
                            "Define protocol `{parent_name}` first, or check the name for typos"
                        ))
                        .with_category(DiagnosticCategory::Type),
                    );
                }
            }

            self.protocols.insert(name.clone(), info);
        }

        diagnostics
    }

    /// Looks up a protocol by name.
    #[must_use]
    pub fn get(&self, name: &str) -> Option<&ProtocolInfo> {
        crate::semantic_analysis::consulted::record_name(name);
        self.protocols.get(name)
    }

    /// Checks if a protocol exists in the registry.
    #[must_use]
    pub fn has_protocol(&self, name: &str) -> bool {
        crate::semantic_analysis::consulted::record_name(name);
        self.protocols.contains_key(name)
    }

    /// Returns an iterator over all protocol names in the registry.
    pub fn protocol_names(&self) -> impl Iterator<Item = &EcoString> {
        crate::semantic_analysis::consulted::record_scan(
            crate::semantic_analysis::consulted::ScanKind::Protocols,
        );
        self.protocols.keys()
    }

    /// Returns an iterator over all protocols in the registry.
    pub fn protocols(&self) -> impl Iterator<Item = (&EcoString, &ProtocolInfo)> {
        crate::semantic_analysis::consulted::record_scan(
            crate::semantic_analysis::consulted::ScanKind::Protocols,
        );
        self.protocols.iter()
    }

    /// Finds protocols that require a given selector (instance methods only).
    ///
    /// Used by `respondsTo:` narrowing (ADR 0068 Phase 2e) to refine the
    /// narrowed type from `Dynamic` to a specific protocol when exactly one
    /// protocol in the registry requires the tested selector. Returns `None`
    /// if zero or multiple protocols match (ambiguous — fall back to Dynamic).
    #[must_use]
    pub fn find_unique_protocol_for_selector(&self, selector: &str) -> Option<&EcoString> {
        crate::semantic_analysis::consulted::record_scan(
            crate::semantic_analysis::consulted::ScanKind::Protocols,
        );
        let mut found: Option<&EcoString> = None;
        for (name, info) in &self.protocols {
            let selectors = info.all_required_selectors(self);
            if selectors.iter().any(|s| s.as_str() == selector) {
                if found.is_some() {
                    // Ambiguous — multiple protocols require this selector
                    return None;
                }
                found = Some(name);
            }
        }
        found
    }

    /// Check if a class conforms to a protocol.
    ///
    /// Implements the three-tier conformance model from ADR 0068:
    /// - **Tier 1**: Compile-time hierarchy — walks full superclass chain
    /// - **Tier 3**: DNU bypass — classes with `doesNotUnderstand:` override conform to all
    ///
    /// Returns `Ok(())` if the class conforms, or `Err(missing_selectors)` listing
    /// the selectors the class is missing.
    ///
    /// Tier 2 (REPL workspace) is handled by the caller providing an enriched hierarchy.
    ///
    /// # Errors
    ///
    /// Returns `Err(Vec<EcoString>)` listing the missing required selectors
    /// when the class does not conform to the protocol.
    pub fn check_conformance(
        &self,
        class_name: &str,
        protocol_name: &str,
        hierarchy: &ClassHierarchy,
    ) -> Result<(), Vec<EcoString>> {
        let Some(protocol) = self.get(protocol_name) else {
            // Unknown protocol — can't check, assume conformance
            return Ok(());
        };

        self.check_conformance_to_protocol(class_name, protocol, hierarchy)
    }

    /// Check conformance against a specific `ProtocolInfo` (internal).
    fn check_conformance_to_protocol(
        &self,
        class_name: &str,
        protocol: &ProtocolInfo,
        hierarchy: &ClassHierarchy,
    ) -> Result<(), Vec<EcoString>> {
        // ADR 0100 Rule 1: an unknown, DNU-overridden, or
        // cross-file-parent receiver conforms by default — the checker
        // cannot prove non-conformance from an incomplete method surface
        // (Tier 3 DNU bypass folded into the classifier's `Open` state).
        if !receiver_knowledge::classify_receiver(class_name, hierarchy, false).is_closed_complete()
        {
            return Ok(());
        }

        let missing = self.missing_conformance_selectors(class_name, protocol, hierarchy);
        if missing.is_empty() {
            Ok(())
        } else {
            Err(missing)
        }
    }

    /// The selectors of `protocol` that `class_name`'s *known* method surface
    /// does not resolve — the strict structural core shared by
    /// [`check_conformance_to_protocol`](Self::check_conformance_to_protocol)
    /// (which consults it only behind the closed-complete gate) and
    /// [`conforms_by_known_surface`](Self::conforms_by_known_surface) (which
    /// consults it directly).
    fn missing_conformance_selectors(
        &self,
        class_name: &str,
        protocol: &ProtocolInfo,
        hierarchy: &ClassHierarchy,
    ) -> Vec<EcoString> {
        // ADR 0127 §8 (decided): a protocol's type is required ∪ provided,
        // so conformance needs every provided selector too, not just the
        // required ones — a class that `uses:` the protocol already has
        // them after flattening, and a structural conformer that skips one
        // is exactly the case decision 2 turns into a warning here instead
        // of a silent `does_not_understand` at the call site.
        let conformance_selectors = protocol.all_conformance_selectors(self);
        let mut missing = Vec::new();

        for selector in conformance_selectors {
            if !hierarchy.resolves_selector(class_name, selector) {
                missing.push(selector.clone());
            }
        }

        // Check class method requirements against the metaclass
        let required_class = protocol.all_required_class_selectors(self);
        for selector in required_class {
            if !hierarchy.resolves_class_selector(class_name, selector) {
                // Prefix with "class " to distinguish in error messages
                missing.push(EcoString::from(format!("class {selector}")));
            }
        }

        missing
    }

    /// The protocol requirements `class_name` resolves by selector but with a
    /// declared return type that does not refine the requirement's declared
    /// return type (BT-4175) — the return-type half of conformance that
    /// [`missing_conformance_selectors`](Self::missing_conformance_selectors)
    /// (selector presence only) does not cover.
    ///
    /// Kept separate from [`check_conformance`](Self::check_conformance)
    /// rather than folded into its `Err(missing)`: a class with a widened
    /// `asString` still *responds* to every selector (reflection,
    /// `conformsTo:`, generic bounds and the "missing method" warning keep
    /// their structural meaning); this is a distinct, warning-severity
    /// soundness finding a caller reports alongside it. Same open-world gate
    /// as `check_conformance` (unknown / DNU / cross-file receivers answer no
    /// mismatches), and the same covariance rule as an inherited override —
    /// [`TypeChecker::is_return_type_refinement`](crate::semantic_analysis::type_checker::TypeChecker::is_return_type_refinement).
    /// Requirements or methods without a declared return type are skipped,
    /// as is any requirement whose return type names a type parameter of the
    /// protocol or of any protocol it extends (no substitution context here). A mismatch that an
    /// override introduces by widening an inherited, conforming return type
    /// is skipped: the override check reports it once at the definition.
    /// Both sides: `class` requirements are judged against the class's
    /// class-side methods (BT-4441), with no override skip (the override
    /// check judges instance methods only).
    #[must_use]
    pub fn conformance_return_type_mismatches(
        &self,
        class_name: &str,
        protocol_name: &str,
        hierarchy: &ClassHierarchy,
    ) -> Vec<ReturnTypeMismatch> {
        let Some(protocol) = self.get(protocol_name) else {
            return Vec::new();
        };
        if !receiver_knowledge::classify_receiver(class_name, hierarchy, false).is_closed_complete()
        {
            return Vec::new();
        }
        let type_params = protocol.all_type_params(self);
        let mut mismatches = Vec::new();
        // A provision's declared return type is part of the protocol's type
        // too (ADR 0127 §8; BT-4490), so a class is judged against every
        // typed requirement *and* provision — the same entries
        // `subsumes_conformance_to` relies on, so the two cannot disagree.
        // That includes a `uses:` class whose own override widens a
        // provision: it is reported here at each `:: P` call site as well as
        // at the override's definition, because the class really does answer
        // a type the protocol does not promise.
        for req in protocol
            .all_requirements(self)
            .into_iter()
            .chain(protocol.all_provided(self))
        {
            let Some(expected) = req.judged_return_type(&type_params) else {
                continue;
            };
            let Some(method) = hierarchy.find_method(class_name, &req.selector) else {
                continue;
            };
            let Some(ref actual) = method.return_type else {
                continue;
            };
            if TypeChecker::is_return_type_refinement(actual, &expected, hierarchy) {
                continue;
            }
            // An override that widens an inherited return type which already
            // satisfies the protocol is reported once, at the override's
            // definition, where `@expect type` can suppress a deliberate
            // widening such as `Binary>>asString`. Re-reporting it at every
            // call site that passes the class to a protocol-typed parameter
            // would be noise the caller cannot fix. Skip only when the
            // override check reports it (same decision, shared).
            if TypeChecker::override_return_widening(
                &method.defined_in,
                &req.selector,
                actual,
                hierarchy,
            )
            .is_some_and(|w| {
                TypeChecker::is_return_type_refinement(&w.parent_rt, &expected, hierarchy)
            }) {
                continue;
            }
            let mismatch = ReturnTypeMismatch {
                selector: req.selector.clone(),
                class_side: false,
                defined_in: method.defined_in.clone(),
                actual: actual.clone(),
                expected,
            };
            // A requirement and a provision may state the same type.
            if !mismatches.contains(&mismatch) {
                mismatches.push(mismatch);
            }
        }
        for req in protocol.all_class_requirements(self) {
            let Some(expected) = req.judged_return_type(&type_params) else {
                continue;
            };
            let Some(method) = hierarchy.find_class_method(class_name, &req.selector) else {
                continue;
            };
            let Some(actual) = method.return_type.clone() else {
                continue;
            };
            if TypeChecker::is_return_type_refinement(&actual, &expected, hierarchy) {
                continue;
            }
            mismatches.push(ReturnTypeMismatch {
                selector: req.selector.clone(),
                class_side: true,
                defined_in: method.defined_in.clone(),
                actual,
                expected,
            });
        }
        mismatches
    }

    /// `true` when `class_name`'s *known* method surface resolves every
    /// selector protocol `protocol_name` requires — strict structural
    /// conformance with no closed-complete gate and no assume-conformance
    /// fallback.
    ///
    /// Unlike [`check_conformance`](Self::check_conformance), which answers
    /// "can the checker prove NON-conformance" (unknown receiver/protocol ⇒
    /// `Ok`, the conservative answer for a warning-severity assignability
    /// check), this answers "can the checker prove conformance" — the
    /// direction an Error-severity diagnostic needs. ADR 0124 B1 (BT-4061)
    /// uses it to decide whether a protocol-typed `late` slot admits `nil`:
    /// `late state: x :: P` is an error only when `UndefinedObject`'s known
    /// surface provably satisfies `P`. An unknown protocol name proves
    /// nothing and answers `false`.
    #[must_use]
    pub fn conforms_by_known_surface(
        &self,
        class_name: &str,
        protocol_name: &str,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        let Some(protocol) = self.get(protocol_name) else {
            return false;
        };
        self.missing_conformance_selectors(class_name, protocol, hierarchy)
            .is_empty()
    }

    /// Check if the *class side* of a class conforms to a protocol.
    ///
    /// This is the metatype counterpart of [`check_conformance`]: it asks
    /// whether the class *object* `C` — a value of type `C class`, inferred as
    /// `Meta{C}` (ADR 0083) — satisfies the protocol, rather than whether
    /// *instances* of `C` do.
    ///
    /// A callee holding a `:: P`-typed parameter sends `P`'s required
    /// *instance-side* selectors to the argument. When the argument is a class
    /// object, those sends dispatch to:
    /// - `C`'s class-side methods (including inherited class methods), or
    /// - the metaclass tower's instance protocol
    ///   (`Metaclass → Class → Behaviour → Object → ProtoObject`), which every
    ///   class object responds to at runtime (e.g. `printString`, `name`).
    ///
    /// So the rule is: each required instance-side selector of `P` (including
    /// those inherited from extended protocols) must resolve either on `C`'s
    /// class side or on the `Metaclass` tower.
    ///
    /// `P`'s *class-side* requirements (`class` signatures) are NOT checked
    /// here: for a class-object argument they would constrain the argument's
    /// own class — the metaclass of `C` — which the static hierarchy does not
    /// model per-class. They are conservatively assumed satisfied (open-world),
    /// matching the checker's warnings-not-errors philosophy (ADR 0025).
    ///
    /// Same open-world gating as [`check_conformance`]: unknown classes,
    /// cross-file parents, and class-side `doesNotUnderstand:args:` overrides
    /// all conform.
    ///
    /// [`check_conformance`]: Self::check_conformance
    ///
    /// # Errors
    ///
    /// Returns `Err(Vec<EcoString>)` listing the required selectors that the
    /// class side does not resolve.
    pub fn check_class_side_conformance(
        &self,
        class_name: &str,
        protocol_name: &str,
        hierarchy: &ClassHierarchy,
    ) -> Result<(), Vec<EcoString>> {
        let Some(protocol) = self.get(protocol_name) else {
            // Unknown protocol — can't check, assume conformance
            return Ok(());
        };

        // ADR 0100 Rule 1: an unknown, class-side-DNU-overridden,
        // or cross-file-parent receiver conforms by default (Tier 3 DNU
        // bypass folded into the classifier's `Open` state).
        if !receiver_knowledge::classify_receiver(class_name, hierarchy, true).is_closed_complete()
        {
            return Ok(());
        }

        // ADR 0127 §8: required ∪ provided, same as `check_conformance_to_protocol`.
        let mut missing = Vec::new();
        for selector in protocol.all_conformance_selectors(self) {
            if !hierarchy.resolves_class_selector(class_name, selector)
                && !hierarchy.resolves_selector("Metaclass", selector)
            {
                missing.push(selector.clone());
            }
        }

        if missing.is_empty() {
            Ok(())
        } else {
            Err(missing)
        }
    }

    /// Check conformance for a generic protocol with concrete type arguments.
    ///
    /// For `Collection(Integer)`, substitutes `E → Integer` in the required
    /// signatures before checking. Currently only checks selector presence
    /// (arity match), not type-level compatibility of signatures.
    ///
    /// # Errors
    ///
    /// Returns `Err(Vec<EcoString>)` listing the missing required selectors
    /// when the class does not conform to the protocol.
    pub fn check_generic_conformance(
        &self,
        class_name: &str,
        protocol_name: &str,
        _type_args: &[EcoString],
        hierarchy: &ClassHierarchy,
    ) -> Result<(), Vec<EcoString>> {
        // Generic conformance currently checks selector presence only.
        // Type-level signature matching (e.g., `do:` block param type) is
        // deferred to Phase 2d (type parameter bounds).
        self.check_conformance(class_name, protocol_name, hierarchy)
    }

    /// Returns the list of protocols a class conforms to.
    #[must_use]
    pub fn conforming_protocols(
        &self,
        class_name: &str,
        hierarchy: &ClassHierarchy,
    ) -> Vec<EcoString> {
        crate::semantic_analysis::consulted::record_scan(
            crate::semantic_analysis::consulted::ScanKind::Protocols,
        );
        self.protocols
            .keys()
            .filter(|proto_name| {
                self.check_conformance(class_name, proto_name, hierarchy)
                    .is_ok()
            })
            .cloned()
            .collect()
    }

    /// Returns the list of classes that conform to a protocol.
    #[must_use]
    pub fn conforming_classes(
        &self,
        protocol_name: &str,
        hierarchy: &ClassHierarchy,
    ) -> Vec<EcoString> {
        hierarchy
            .class_names()
            .filter(|class_name| {
                self.check_conformance(class_name, protocol_name, hierarchy)
                    .is_ok()
            })
            .cloned()
            .collect()
    }

    /// Register a protocol directly for testing purposes (ADR 0068 Phase 2f).
    ///
    /// Bypasses the module registration path — useful for unit tests that need
    /// a protocol without constructing a full `Module` AST.
    #[cfg(test)]
    pub fn register_test_protocol(&mut self, protocol: ProtocolInfo) {
        self.protocols.insert(protocol.name.clone(), protocol);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ClassDefinition, CommentAttachment, ExpressionStatement, KeywordPart, MessageSelector,
        MethodDefinition, MethodKind, Module, ParameterDefinition, ProtocolDefinition,
        ProtocolMethodSignature, TypeAnnotation, TypeParamDecl,
    };

    use crate::test_helpers::test_support::unit_span as span;

    use crate::test_helpers::test_support::unit_ident as ident;

    fn make_protocol(name: &str, methods: Vec<(&str, usize, Option<&str>)>) -> ProtocolDefinition {
        ProtocolDefinition {
            name: ident(name),
            type_params: vec![],
            extending: None,
            method_signatures: methods
                .into_iter()
                .map(|(sel, arity, ret_ty)| {
                    let selector = if arity == 0 {
                        MessageSelector::Unary(sel.into())
                    } else if sel.contains(':') {
                        let parts: Vec<&str> = sel.split(':').filter(|s| !s.is_empty()).collect();
                        MessageSelector::Keyword(
                            parts
                                .into_iter()
                                .map(|p| KeywordPart::new(format!("{p}:"), span()))
                                .collect(),
                        )
                    } else {
                        MessageSelector::Binary(sel.into())
                    };

                    ProtocolMethodSignature {
                        selector,
                        parameters: (0..arity)
                            .map(|i| ParameterDefinition::new(ident(&format!("arg{i}"))))
                            .collect(),
                        return_type: ret_ty.map(|t| TypeAnnotation::Simple(ident(t))),
                        comments: CommentAttachment::default(),
                        doc_comment: None,
                        span: span(),
                    }
                })
                .collect(),
            class_method_signatures: vec![],
            provided_methods: vec![],
            package: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }
    }

    fn make_protocol_with_type_params(
        name: &str,
        type_params: Vec<&str>,
        methods: Vec<(&str, usize, Option<&str>)>,
    ) -> ProtocolDefinition {
        let mut proto = make_protocol(name, methods);
        proto.type_params = type_params
            .into_iter()
            .map(|tp| TypeParamDecl::unbounded(ident(tp)))
            .collect();
        proto
    }

    fn make_extending_protocol(
        name: &str,
        extending: &str,
        methods: Vec<(&str, usize, Option<&str>)>,
    ) -> ProtocolDefinition {
        let mut proto = make_protocol(name, methods);
        proto.extending = Some(ident(extending));
        proto
    }

    fn empty_module() -> Module {
        Module::new(vec![], span())
    }

    fn make_class(name: &str, superclass: &str, methods: Vec<&str>) -> ClassDefinition {
        ClassDefinition::new(
            ident(name),
            ident(superclass),
            vec![],
            methods
                .into_iter()
                .map(|sel| {
                    let selector = if sel.contains(':') {
                        let parts: Vec<&str> = sel.split(':').filter(|s| !s.is_empty()).collect();
                        MessageSelector::Keyword(
                            parts
                                .into_iter()
                                .map(|p| KeywordPart::new(format!("{p}:"), span()))
                                .collect(),
                        )
                    } else {
                        MessageSelector::Unary(sel.into())
                    };
                    MethodDefinition {
                        pipeline_sends: crate::ast::PipelineSends::default(),
                        literal_comments: crate::ast::LiteralComments::default(),
                        selector,
                        parameters: vec![],
                        body: vec![ExpressionStatement::bare(
                            crate::ast::Expression::Identifier(ident("self")),
                        )],
                        return_type: None,
                        kind: MethodKind::Primary,
                        is_sealed: false,
                        is_internal: false,
                        is_class_method: false,
                        expect: None,
                        comments: CommentAttachment::default(),
                        doc_comment: None,
                        span: span(),
                    }
                })
                .collect(),
            span(),
        )
    }

    /// Builds a trivial provided method (ADR 0127 §1) for a `ProtocolInfo`
    /// conformance test — the body's content never matters at this level,
    /// only that the selector lives in `provided_methods` rather than
    /// `method_signatures`.
    fn make_provided_method(sel: &str, arity: usize) -> MethodDefinition {
        let selector = if arity == 0 {
            MessageSelector::Unary(sel.into())
        } else if sel.contains(':') {
            let parts: Vec<&str> = sel.split(':').filter(|s| !s.is_empty()).collect();
            MessageSelector::Keyword(
                parts
                    .into_iter()
                    .map(|p| KeywordPart::new(format!("{p}:"), span()))
                    .collect(),
            )
        } else {
            MessageSelector::Binary(sel.into())
        };
        MethodDefinition {
            pipeline_sends: crate::ast::PipelineSends::default(),
            literal_comments: crate::ast::LiteralComments::default(),
            selector,
            parameters: (0..arity)
                .map(|i| ParameterDefinition::new(ident(&format!("arg{i}"))))
                .collect(),
            body: vec![ExpressionStatement::bare(
                crate::ast::Expression::Identifier(ident("self")),
            )],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }
    }

    // ---- Basic Registration Tests ----

    #[test]
    fn register_simple_protocol() {
        let module = Module {
            protocols: vec![make_protocol(
                "Printable",
                vec![("asString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert!(diags.is_empty());
        assert!(registry.has_protocol("Printable"));
        let info = registry.get("Printable").unwrap();
        assert_eq!(info.methods.len(), 1);
        assert_eq!(info.methods[0].selector.as_str(), "asString");
    }

    #[test]
    fn register_generic_protocol() {
        // Use "Iterable" not "Collection" — Collection is a built-in class
        let module = Module {
            protocols: vec![make_protocol_with_type_params(
                "Iterable",
                vec!["E"],
                vec![
                    ("size", 0, Some("Integer")),
                    ("do:", 1, None),
                    ("collect:", 1, None),
                ],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert!(diags.is_empty());
        let info = registry.get("Iterable").unwrap();
        assert_eq!(info.type_params, vec![EcoString::from("E")]);
        assert_eq!(info.methods.len(), 3);
    }

    #[test]
    fn register_extending_protocol() {
        let module = Module {
            protocols: vec![
                make_protocol("Comparable", vec![("<", 1, Some("Boolean"))]),
                make_extending_protocol(
                    "Sortable",
                    "Comparable",
                    vec![("sortKey", 0, Some("Object"))],
                ),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert!(diags.is_empty());
        let sortable = registry.get("Sortable").unwrap();
        let all_selectors = sortable.all_required_selectors(&registry);
        assert_eq!(all_selectors.len(), 2); // sortKey + <
    }

    // ---- Namespace Collision Tests ----

    #[test]
    fn namespace_collision_class_and_protocol() {
        // Integer is a built-in class, so defining a protocol named Integer should error
        let module = Module {
            protocols: vec![make_protocol(
                "Integer",
                vec![("asString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("already defined as a class"));
        assert!(!registry.has_protocol("Integer"));
    }

    #[test]
    fn duplicate_protocol_definition() {
        let module = Module {
            protocols: vec![
                make_protocol("Printable", vec![("asString", 0, Some("String"))]),
                make_protocol("Printable", vec![("display", 0, None)]),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Duplicate protocol"));
    }

    #[test]
    fn extending_unknown_protocol() {
        let module = Module {
            protocols: vec![make_extending_protocol(
                "Sortable",
                "UnknownProtocol",
                vec![("sortKey", 0, None)],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("extends unknown protocol"));
    }

    // ---- Conformance Checking Tests ----

    #[test]
    fn conformance_builtin_class_has_as_string() {
        let module = Module {
            protocols: vec![make_protocol(
                "Printable",
                vec![("asString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        // Integer has asString (from Object hierarchy)
        let result = registry.check_conformance("Integer", "Printable", &hierarchy);
        assert!(result.is_ok(), "Integer should conform to Printable");

        // String has asString
        let result = registry.check_conformance("String", "Printable", &hierarchy);
        assert!(result.is_ok(), "String should conform to Printable");
    }

    #[test]
    fn non_conformance_missing_selector() {
        let module = Module {
            protocols: vec![make_protocol(
                "Sortable",
                vec![("sortKey", 0, Some("Object")), ("<", 1, Some("Boolean"))],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        // String does not have sortKey
        let result = registry.check_conformance("String", "Sortable", &hierarchy);
        assert!(result.is_err());
        let missing = result.unwrap_err();
        assert!(missing.contains(&EcoString::from("sortKey")));
    }

    #[test]
    fn dnu_bypass_conforms_to_all() {
        // ErlangModule has doesNotUnderstand: override
        let module = Module {
            protocols: vec![make_protocol(
                "AnythingGoes",
                vec![("veryObscureMethod", 0, None)],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        // ErlangModule overrides doesNotUnderstand: — should conform to all
        let result = registry.check_conformance("ErlangModule", "AnythingGoes", &hierarchy);
        assert!(
            result.is_ok(),
            "DNU override class should conform to all protocols"
        );
    }

    #[test]
    fn conformance_with_user_class() {
        let module = Module {
            classes: vec![make_class(
                "MyPrintable",
                "Object",
                vec!["asString", "display"],
            )],
            protocols: vec![make_protocol(
                "Printable",
                vec![("asString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result = registry.check_conformance("MyPrintable", "Printable", &hierarchy);
        assert!(result.is_ok(), "MyPrintable has asString — should conform");
    }

    #[test]
    fn conformance_requires_provided_selectors_too_for_a_structural_conformer() {
        // ADR 0127 §8 (decided): a protocol's type is required ∪ provided.
        // A class that implements only the required `<` — not the provided
        // `max:` — no longer structurally conforms to `Comparable`.
        let mut comparable = make_protocol("Comparable", vec![("<", 1, Some("Boolean"))]);
        comparable.provided_methods = vec![make_provided_method("max:", 1)];

        let module = Module {
            classes: vec![make_class("PartialComparable", "Object", vec!["<"])],
            protocols: vec![comparable],
            ..empty_module()
        };
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result = registry.check_conformance("PartialComparable", "Comparable", &hierarchy);
        assert!(
            result.is_err(),
            "a structural conformer missing a provided selector must not conform"
        );
        assert!(result.unwrap_err().contains(&EcoString::from("max:")));
    }

    #[test]
    fn conformance_holds_once_uses_flattens_the_provision_in() {
        // A class that `uses:` the trait conforms once BT-3588's flattening
        // pass has copied the provided `max:` into its own body — proving
        // `trait_expansion::expand_module`'s output and this module's
        // required ∪ provided conformance rule (§8) agree with each other.
        let source = "Protocol define: Comparable
  < other :: Self -> Boolean
  max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]

Value subclass: Version
  uses: Comparable
  field: major :: Integer = 0

  < other :: Version -> Boolean => self.major < other major";

        let tokens = crate::source_analysis::lex_with_eof(source);
        let (mut module, diagnostics) = crate::source_analysis::parse(tokens);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");

        let (expansion_diags, _origins) =
            crate::semantic_analysis::trait_expansion::expand_module(&mut module, &HashMap::new());
        assert!(expansion_diags.is_empty(), "{expansion_diags:?}");

        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result = registry.check_conformance("Version", "Comparable", &hierarchy);
        assert!(
            result.is_ok(),
            "Version uses Comparable, so max: is flattened in: {result:?}"
        );
    }

    #[test]
    fn extending_a_protocol_with_provisions_widens_the_childs_conformance_type() {
        // ADR 0127 §5, §8: `Sortable extending: Comparable`, where
        // `Comparable` has a provision — `Comparable`'s provided selectors
        // are part of `Sortable`'s own required ∪ provided type too.
        let mut comparable = make_protocol("Comparable", vec![("<", 1, Some("Boolean"))]);
        comparable.provided_methods = vec![make_provided_method("max:", 1)];
        let sortable =
            make_extending_protocol("Sortable", "Comparable", vec![("sortKey", 0, None)]);

        let module = Module {
            protocols: vec![comparable, sortable],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);
        assert!(diags.is_empty());

        let sortable_info = registry.get("Sortable").unwrap();
        let conformance = sortable_info.all_conformance_selectors(&registry);
        let names: Vec<&str> = conformance.iter().map(|s| s.as_str()).collect();
        assert!(names.contains(&"sortKey"), "{names:?}");
        assert!(names.contains(&"<"), "{names:?}");
        assert!(
            names.contains(&"max:"),
            "Comparable's provided max: must be part of Sortable's type too: {names:?}"
        );

        // `Protocol requiredMethods:` (`all_required_selectors`) must keep
        // answering only the *required* half — unaffected by this change.
        let required: Vec<&str> = sortable_info
            .all_required_selectors(&registry)
            .iter()
            .map(|s| s.as_str())
            .collect();
        assert!(required.contains(&"sortKey"));
        assert!(required.contains(&"<"));
        assert!(
            !required.contains(&"max:"),
            "requiredMethods: must not include a provided selector: {required:?}"
        );
    }

    #[test]
    fn conformance_through_inheritance() {
        // SubClass inherits from MyBase which has asString
        let module = Module {
            classes: vec![
                make_class("MyBase", "Object", vec!["asString", "helper"]),
                make_class("SubClass", "MyBase", vec!["extra"]),
            ],
            protocols: vec![make_protocol(
                "Printable",
                vec![("asString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result = registry.check_conformance("SubClass", "Printable", &hierarchy);
        assert!(
            result.is_ok(),
            "SubClass inherits asString from MyBase — should conform"
        );
    }

    #[test]
    fn extending_protocol_conformance() {
        let module = Module {
            classes: vec![make_class("MyClass", "Object", vec!["asString", "sortKey"])],
            protocols: vec![
                make_protocol("Printable", vec![("asString", 0, Some("String"))]),
                make_extending_protocol(
                    "PrintableAndSortable",
                    "Printable",
                    vec![("sortKey", 0, Some("Object"))],
                ),
            ],
            ..empty_module()
        };
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        // MyClass has both asString and sortKey
        let result = registry.check_conformance("MyClass", "PrintableAndSortable", &hierarchy);
        assert!(result.is_ok());

        // Integer has asString but not sortKey — should fail
        let result = registry.check_conformance("Integer", "PrintableAndSortable", &hierarchy);
        assert!(result.is_err());
    }

    #[test]
    fn generic_protocol_conformance() {
        // Use "Iterable" not "Collection" — Collection is a built-in class
        let module = Module {
            protocols: vec![make_protocol_with_type_params(
                "Iterable",
                vec!["E"],
                vec![("size", 0, Some("Integer")), ("do:", 1, None)],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        // Array has size and do:
        let result = registry.check_generic_conformance(
            "Array",
            "Iterable",
            &[EcoString::from("Integer")],
            &hierarchy,
        );
        assert!(result.is_ok(), "Array should conform to Iterable(Integer)");
    }

    #[test]
    fn conforming_protocols_lists_all() {
        let module = Module {
            protocols: vec![
                make_protocol("Printable", vec![("asString", 0, Some("String"))]),
                make_protocol("HasSize", vec![("size", 0, Some("Integer"))]),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        // String has both asString and size
        let protos = registry.conforming_protocols("String", &hierarchy);
        assert!(protos.contains(&EcoString::from("Printable")));
        assert!(protos.contains(&EcoString::from("HasSize")));
    }

    #[test]
    fn unknown_protocol_conformance_ok() {
        let hierarchy = ClassHierarchy::with_builtins();
        let registry = ProtocolRegistry::new();

        // Unknown protocol — should not error
        let result = registry.check_conformance("Integer", "NotAProtocol", &hierarchy);
        assert!(result.is_ok());
    }

    #[test]
    fn unknown_class_conformance_ok() {
        let module = Module {
            protocols: vec![make_protocol(
                "Printable",
                vec![("asString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        // Unknown class — should not error (conservative)
        let result = registry.check_conformance("NotAClass", "Printable", &hierarchy);
        assert!(result.is_ok());
    }

    // ---- Class-Side (Metatype) Conformance Tests ----

    /// Builds a hierarchy containing a `Gadget` class whose class side has
    /// the given selectors (arity 0), on top of the builtins.
    fn hierarchy_with_gadget_class_methods(selectors: Vec<&str>) -> ClassHierarchy {
        use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

        let mut hierarchy = ClassHierarchy::with_builtins();
        let gadget = ClassInfo {
            surface_incomplete: false,
            in_caller_new1: false,
            is_module_less: false,
            name: "Gadget".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: true,
            is_native: false,
            auto_ctors: None,
            handle_scope: None,
            state: vec![],
            state_types: std::collections::HashMap::new(),
            state_has_default: std::collections::HashMap::new(),
            state_kinds: HashMap::new(),
            initialize_assigns: std::collections::BTreeSet::new(),
            has_dynamic_field_writer: false,
            methods: vec![],
            class_methods: selectors
                .into_iter()
                .map(|sel| MethodInfo {
                    selector: sel.into(),
                    arity: 0,
                    kind: MethodKind::Primary,
                    defined_in: "Gadget".into(),
                    is_sealed: false,
                    is_internal: false,
                    spawns_block: false,
                    return_type: None,
                    param_types: vec![],
                    doc: None,
                    origin: None,
                    primitive: None,
                    constant_value: None,
                    answers_receiver: false,
                })
                .collect(),
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        };
        hierarchy.add_from_beam_meta(vec![gadget]);
        hierarchy
    }

    #[test]
    fn class_side_conformance_via_class_method() {
        let module = Module {
            protocols: vec![make_protocol("Serializable", vec![("serialize", 0, None)])],
            ..empty_module()
        };
        let hierarchy = hierarchy_with_gadget_class_methods(vec!["serialize"]);
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result = registry.check_class_side_conformance("Gadget", "Serializable", &hierarchy);
        assert!(
            result.is_ok(),
            "Gadget's class side has `serialize` — the class object should conform"
        );
    }

    #[test]
    fn class_side_non_conformance_missing_selector() {
        let module = Module {
            protocols: vec![make_protocol("Serializable", vec![("serialize", 0, None)])],
            ..empty_module()
        };
        let hierarchy = hierarchy_with_gadget_class_methods(vec![]);
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result = registry.check_class_side_conformance("Gadget", "Serializable", &hierarchy);
        assert!(result.is_err(), "Gadget's class side lacks `serialize`");
        assert!(result.unwrap_err().contains(&EcoString::from("serialize")));

        // ...even though *instances* of Gadget wouldn't conform either, the
        // instance-side and class-side checks are asking different questions:
        // a Gadget with an instance-side `serialize` still fails class-side.
        let hierarchy2 = {
            use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
            let mut h = ClassHierarchy::with_builtins();
            h.add_from_beam_meta(vec![ClassInfo {
                surface_incomplete: false,
                in_caller_new1: false,
                is_module_less: false,
                name: "Gadget".into(),
                superclass: Some("Object".into()),
                is_sealed: false,
                is_abstract: false,
                is_typed: false,
                is_internal: false,
                package: None,
                is_value: true,
                is_native: false,
                auto_ctors: None,
                handle_scope: None,
                state: vec![],
                state_types: std::collections::HashMap::new(),
                state_has_default: std::collections::HashMap::new(),
                state_kinds: HashMap::new(),
                initialize_assigns: std::collections::BTreeSet::new(),
                has_dynamic_field_writer: false,
                methods: vec![MethodInfo {
                    selector: "serialize".into(),
                    arity: 0,
                    kind: MethodKind::Primary,
                    defined_in: "Gadget".into(),
                    is_sealed: false,
                    is_internal: false,
                    spawns_block: false,
                    return_type: None,
                    param_types: vec![],
                    doc: None,
                    origin: None,
                    primitive: None,
                    constant_value: None,
                    answers_receiver: false,
                }],
                class_methods: vec![],
                class_variables: vec![],
                type_params: vec![],
                type_param_bounds: vec![],
                superclass_type_args: vec![],
            }]);
            h
        };
        assert!(
            registry
                .check_conformance("Gadget", "Serializable", &hierarchy2)
                .is_ok(),
            "instance side conforms"
        );
        assert!(
            registry
                .check_class_side_conformance("Gadget", "Serializable", &hierarchy2)
                .is_err(),
            "class side must NOT inherit instance-side conformance"
        );
    }

    #[test]
    fn class_side_conformance_via_metaclass_tower() {
        // Every class object responds to `printString` via the metaclass tower
        // (Metaclass → Class → Behaviour → Object), even with an empty class side.
        let module = Module {
            protocols: vec![make_protocol(
                "Describable",
                vec![("printString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let hierarchy = hierarchy_with_gadget_class_methods(vec![]);
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result = registry.check_class_side_conformance("Gadget", "Describable", &hierarchy);
        assert!(
            result.is_ok(),
            "class objects respond to printString via the metaclass tower: {result:?}"
        );
    }

    #[test]
    fn class_side_conformance_unknown_class_assumed_ok() {
        // Open-world gating: an unknown class can't be verified — assume ok.
        let module = Module {
            protocols: vec![make_protocol("Serializable", vec![("serialize", 0, None)])],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result =
            registry.check_class_side_conformance("NoSuchClass", "Serializable", &hierarchy);
        assert!(result.is_ok(), "unknown class must be assumed conforming");
    }

    #[test]
    fn class_side_conformance_includes_extended_protocol_requirements() {
        // Sortable extends Comparable — the class side must satisfy both
        // protocols' instance-side requirements.
        let module = Module {
            protocols: vec![
                make_protocol("Comparable", vec![("compareTo:", 1, Some("Integer"))]),
                make_extending_protocol("Sortable", "Comparable", vec![("sortKey", 0, None)]),
            ],
            ..empty_module()
        };
        let hierarchy = hierarchy_with_gadget_class_methods(vec!["sortKey"]);
        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        let result = registry.check_class_side_conformance("Gadget", "Sortable", &hierarchy);
        assert!(result.is_err(), "class side lacks inherited `compareTo:`");
        assert!(result.unwrap_err().contains(&EcoString::from("compareTo:")));
    }

    // ---- Class Method Protocol Tests ----

    #[test]
    fn register_protocol_with_class_methods() {
        let mut proto = make_protocol("Serializable", vec![("asString", 0, Some("String"))]);
        proto.class_method_signatures = vec![ProtocolMethodSignature {
            selector: MessageSelector::Keyword(vec![KeywordPart::new(
                "fromString:".to_string(),
                span(),
            )]),
            parameters: vec![ParameterDefinition::new(ident("aString"))],
            return_type: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }];

        let module = Module {
            protocols: vec![proto],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert!(diags.is_empty());
        let info = registry.get("Serializable").unwrap();
        assert_eq!(info.methods.len(), 1);
        assert_eq!(info.class_methods.len(), 1);
        assert_eq!(info.class_methods[0].selector.as_str(), "fromString:");
    }

    #[test]
    fn class_method_conformance_missing() {
        // Protocol requires class method `fromString:` — no class has it by default
        let mut proto = make_protocol("Serializable", vec![("asString", 0, Some("String"))]);
        proto.class_method_signatures = vec![ProtocolMethodSignature {
            selector: MessageSelector::Keyword(vec![KeywordPart::new(
                "fromString:".to_string(),
                span(),
            )]),
            parameters: vec![ParameterDefinition::new(ident("aString"))],
            return_type: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }];

        let module = Module {
            classes: vec![make_class("MyClass", "Object", vec!["asString"])],
            protocols: vec![proto],
            ..empty_module()
        };

        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();

        let mut registry = ProtocolRegistry::new();
        registry.register_module(&module, &hierarchy);

        // MyClass has asString but not class fromString: — should fail
        let result = registry.check_conformance("MyClass", "Serializable", &hierarchy);
        assert!(result.is_err());
        let missing = result.unwrap_err();
        assert!(missing.iter().any(|s| s.as_str() == "class fromString:"));
    }

    #[test]
    fn all_required_class_selectors_includes_parent() {
        let mut registry = ProtocolRegistry::new();

        // Parent protocol with class method
        let parent_info = ProtocolInfo {
            name: "Base".into(),
            type_params: vec![],
            type_param_bounds: vec![],
            extending: None,
            methods: vec![],
            class_methods: vec![ProtocolMethodRequirement {
                selector: "create".into(),
                arity: 0,
                return_type: None,
                param_types: vec![],
            }],
            package: None,
            span: span(),

            provided: vec![],
        };
        registry.register_test_protocol(parent_info);

        // Child protocol extending Base with its own class method
        let child_info = ProtocolInfo {
            name: "Extended".into(),
            type_params: vec![],
            type_param_bounds: vec![],
            extending: Some("Base".into()),
            methods: vec![],
            class_methods: vec![ProtocolMethodRequirement {
                selector: "createWith:".into(),
                arity: 1,
                return_type: None,
                param_types: vec![None],
            }],
            package: None,
            span: span(),

            provided: vec![],
        };
        registry.register_test_protocol(child_info);

        let extended = registry.get("Extended").unwrap();
        let class_sels = extended.all_required_class_selectors(&registry);
        assert_eq!(class_sels.len(), 2);
        assert!(class_sels.iter().any(|s| s.as_str() == "create"));
        assert!(class_sels.iter().any(|s| s.as_str() == "createWith:"));
    }

    // ---- Pre-loaded protocol namespace collision ----

    #[test]
    fn add_pre_loaded_accepts_non_colliding_protocol() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();

        let protocol = ProtocolInfo {
            name: "Tickable".into(),
            type_params: vec![],
            type_param_bounds: vec![],
            extending: None,
            methods: vec![],
            class_methods: vec![],
            package: None,
            span: span(),

            provided: vec![],
        };

        let diags = registry.add_pre_loaded(vec![protocol], &hierarchy, None);
        assert!(diags.is_empty());
        assert!(registry.has_protocol("Tickable"));
    }

    #[test]
    fn seed_and_register_module_lets_the_module_win_over_pre_loaded() {
        // A pre-loaded copy of a protocol the module itself declares (an
        // indexed saved copy of an edited buffer) is dropped, not reported as
        // a duplicate; other pre-loaded protocols are seeded.
        let (module, parse_diags) =
            crate::source_analysis::parse(crate::source_analysis::lex_with_eof(
                "Protocol define: Wire\n  class label -> String\n",
            ));
        assert!(parse_diags.is_empty(), "{parse_diags:?}");
        let stale = |name: &str| ProtocolInfo {
            name: name.into(),
            type_params: vec![],
            type_param_bounds: vec![],
            extending: None,
            methods: vec![],
            class_methods: vec![],
            package: None,
            span: span(),
            provided: vec![],
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();
        let diags = registry.seed_and_register_module(
            &module,
            vec![stale("Wire"), stale("Other")],
            &hierarchy,
            None,
        );
        assert!(diags.is_empty(), "{diags:?}");
        assert!(registry.has_protocol("Other"));
        let wire = registry.get("Wire").expect("Wire registered");
        assert_eq!(wire.class_methods.len(), 1, "the module's definition wins");
    }

    #[test]
    fn add_pre_loaded_reports_collision_with_class() {
        // `Integer` is a built-in class — a pre-loaded protocol of the same
        // name must be rejected and must not shadow the class in resolution.
        let hierarchy = ClassHierarchy::with_builtins();
        let mut registry = ProtocolRegistry::new();

        let protocol = ProtocolInfo {
            name: "Integer".into(),
            type_params: vec![],
            type_param_bounds: vec![],
            extending: None,
            methods: vec![],
            class_methods: vec![],
            package: None,
            span: span(),

            provided: vec![],
        };

        let diags = registry.add_pre_loaded(vec![protocol], &hierarchy, None);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("collides with class"));
        assert!(!registry.has_protocol("Integer"));
    }

    // ---- Protocol hot-reload with synthetic protocol class ----

    #[test]
    fn register_module_allows_protocol_when_hierarchy_has_protocol_class() {
        // Simulate hot-reload: the hierarchy already contains a synthetic
        // protocol class entry (superclass = Protocol) from a previous load.
        // Registering the same protocol name should succeed — it's not a
        // real namespace collision.
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.insert_class(
            "Printable".into(),
            crate::semantic_analysis::class_hierarchy::ClassInfo {
                surface_incomplete: false,
                in_caller_new1: false,
                is_module_less: false,
                name: "Printable".into(),
                superclass: Some("Protocol".into()),
                is_sealed: true,
                is_abstract: true,
                is_typed: true,
                is_internal: false,
                package: None,
                is_value: false,
                is_native: false,
                auto_ctors: None,
                handle_scope: None,
                state: vec![],
                state_types: HashMap::new(),
                state_has_default: HashMap::new(),
                state_kinds: HashMap::new(),
                initialize_assigns: std::collections::BTreeSet::new(),
                has_dynamic_field_writer: false,
                methods: vec![],
                class_methods: vec![],
                class_variables: vec![],
                type_params: vec![],
                type_param_bounds: vec![],
                superclass_type_args: vec![],
            },
        );

        let module = Module {
            protocols: vec![make_protocol(
                "Printable",
                vec![("asString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert!(
            diags.is_empty(),
            "Re-registering a protocol whose class entry has superclass Protocol \
             should not produce namespace collision diagnostics, got: {diags:?}"
        );
        assert!(registry.has_protocol("Printable"));
    }

    #[test]
    fn register_module_still_rejects_real_class_collision() {
        // A real class (not superclass Protocol) should still collide.
        let hierarchy = ClassHierarchy::with_builtins();
        let module = Module {
            protocols: vec![make_protocol(
                "Integer",
                vec![("asString", 0, Some("String"))],
            )],
            ..empty_module()
        };
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&module, &hierarchy);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("already defined as a class"));
        assert!(!registry.has_protocol("Integer"));
    }

    #[test]
    fn add_pre_loaded_allows_protocol_class_entry() {
        // Pre-loaded protocol should not collide with a synthetic
        // protocol class entry in the hierarchy.
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.insert_class(
            "Tickable".into(),
            crate::semantic_analysis::class_hierarchy::ClassInfo {
                surface_incomplete: false,
                in_caller_new1: false,
                is_module_less: false,
                name: "Tickable".into(),
                superclass: Some("Protocol".into()),
                is_sealed: true,
                is_abstract: true,
                is_typed: true,
                is_internal: false,
                package: None,
                is_value: false,
                is_native: false,
                auto_ctors: None,
                handle_scope: None,
                state: vec![],
                state_types: HashMap::new(),
                state_has_default: HashMap::new(),
                state_kinds: HashMap::new(),
                initialize_assigns: std::collections::BTreeSet::new(),
                has_dynamic_field_writer: false,
                methods: vec![],
                class_methods: vec![],
                class_variables: vec![],
                type_params: vec![],
                type_param_bounds: vec![],
                superclass_type_args: vec![],
            },
        );

        let mut registry = ProtocolRegistry::new();
        let protocol = ProtocolInfo {
            name: "Tickable".into(),
            type_params: vec![],
            type_param_bounds: vec![],
            extending: None,
            methods: vec![],
            class_methods: vec![],
            package: None,
            span: span(),

            provided: vec![],
        };

        let diags = registry.add_pre_loaded(vec![protocol], &hierarchy, None);
        assert!(
            diags.is_empty(),
            "Pre-loaded protocol should not collide with synthetic protocol class, got: {diags:?}"
        );
        assert!(registry.has_protocol("Tickable"));
    }
}
