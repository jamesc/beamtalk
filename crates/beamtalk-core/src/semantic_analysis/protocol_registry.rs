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

use crate::ast::{Module, ProtocolDefinition, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
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
    /// Required class method signatures (BT-1611).
    pub class_methods: Vec<ProtocolMethodRequirement>,
    /// Source span of the protocol definition (for diagnostics).
    pub span: Span,
}

impl ProtocolInfo {
    /// Build a `ProtocolInfo` from a parsed `ProtocolDefinition` AST node.
    fn from_definition(def: &ProtocolDefinition) -> Self {
        let make_req = |sig: &crate::ast::ProtocolMethodSignature| ProtocolMethodRequirement {
            selector: sig.selector.name(),
            arity: sig.selector.arity(),
            return_type: sig.return_type.as_ref().map(TypeAnnotation::type_name),
            param_types: sig
                .parameters
                .iter()
                .map(|p| p.type_annotation.as_ref().map(TypeAnnotation::type_name))
                .collect(),
        };

        let methods = def.method_signatures.iter().map(make_req).collect();
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

    /// Returns all required class method selectors, including from extended protocols (BT-1611).
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

    /// Returns all class method requirements, including from extended protocols (BT-1611).
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
    /// BT-2006: Mirrors `ClassHierarchy::extract_class_infos` — used by the
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
    /// BT-2006: The `BUnit` compile path parses fixture files, extracts their
    /// `ProtocolInfo`s via `extract_protocol_infos`, and injects them here so
    /// the unresolved-class validator and type checker recognise fixture-only
    /// protocol names when analysing the test module. Skips entries whose
    /// names are already registered (current-module definitions win).
    ///
    /// Returns diagnostics for namespace collisions — pre-loaded protocol
    /// names that match a class in the current hierarchy. Colliding entries
    /// are skipped so resolution cannot pick them up (classes win over
    /// cross-file protocols that shadow them).
    pub fn add_pre_loaded(
        &mut self,
        protocols: Vec<ProtocolInfo>,
        hierarchy: &ClassHierarchy,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        for info in protocols {
            if hierarchy.has_class(&info.name) {
                // BT-2088: Skip collision when the existing "class" is a synthetic
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
            // BT-2088: Allow re-registration when the existing "class" is actually
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

            // Duplicate protocol definition
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
        self.protocols.get(name)
    }

    /// Checks if a protocol exists in the registry.
    #[must_use]
    pub fn has_protocol(&self, name: &str) -> bool {
        self.protocols.contains_key(name)
    }

    /// Returns an iterator over all protocol names in the registry.
    pub fn protocol_names(&self) -> impl Iterator<Item = &EcoString> {
        self.protocols.keys()
    }

    /// Returns an iterator over all protocols in the registry.
    pub fn protocols(&self) -> impl Iterator<Item = (&EcoString, &ProtocolInfo)> {
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
        // Tier 3: DNU bypass — classes overriding doesNotUnderstand: conform to all protocols
        if hierarchy.has_instance_dnu_override(class_name) {
            return Ok(());
        }

        // If the class isn't known to the hierarchy, we can't verify — assume ok
        if !hierarchy.has_class(class_name) {
            return Ok(());
        }

        // Cross-file inheritance: if the parent class is not in the hierarchy,
        // we can't know the full method set — assume conformance
        if hierarchy.has_cross_file_parent(class_name) {
            return Ok(());
        }

        let required = protocol.all_required_selectors(self);
        let mut missing = Vec::new();

        for selector in required {
            if !hierarchy.resolves_selector(class_name, selector) {
                missing.push(selector.clone());
            }
        }

        // BT-1611: Check class method requirements against the metaclass
        let required_class = protocol.all_required_class_selectors(self);
        for selector in required_class {
            if !hierarchy.resolves_class_selector(class_name, selector) {
                // Prefix with "class " to distinguish in error messages
                missing.push(EcoString::from(format!("class {selector}")));
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
        ClassDefinition, CommentAttachment, ExpressionStatement, Identifier, KeywordPart,
        MessageSelector, MethodDefinition, MethodKind, Module, ParameterDefinition,
        ProtocolDefinition, ProtocolMethodSignature, TypeAnnotation, TypeParamDecl,
    };
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: span(),
        }
    }

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
                        selector,
                        parameters: vec![],
                        body: vec![ExpressionStatement::bare(
                            crate::ast::Expression::Identifier(ident("self")),
                        )],
                        return_type: None,
                        kind: MethodKind::Primary,
                        is_sealed: false,
                        is_internal: false,
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

    // ---- BT-1611: Class Method Protocol Tests ----

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
            span: span(),
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
            span: span(),
        };
        registry.register_test_protocol(child_info);

        let extended = registry.get("Extended").unwrap();
        let class_sels = extended.all_required_class_selectors(&registry);
        assert_eq!(class_sels.len(), 2);
        assert!(class_sels.iter().any(|s| s.as_str() == "create"));
        assert!(class_sels.iter().any(|s| s.as_str() == "createWith:"));
    }

    // ---- BT-2006: Pre-loaded protocol namespace collision ----

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
            span: span(),
        };

        let diags = registry.add_pre_loaded(vec![protocol], &hierarchy);
        assert!(diags.is_empty());
        assert!(registry.has_protocol("Tickable"));
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
            span: span(),
        };

        let diags = registry.add_pre_loaded(vec![protocol], &hierarchy);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("collides with class"));
        assert!(!registry.has_protocol("Integer"));
    }

    // ---- BT-2088: Protocol hot-reload with synthetic protocol class ----

    #[test]
    fn register_module_allows_protocol_when_hierarchy_has_protocol_class() {
        // Simulate hot-reload: the hierarchy already contains a synthetic
        // protocol class entry (superclass = Protocol) from a previous load.
        // Registering the same protocol name should succeed — it's not a
        // real namespace collision.
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.classes_mut().insert(
            "Printable".into(),
            crate::semantic_analysis::class_hierarchy::ClassInfo {
                name: "Printable".into(),
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
        // BT-2088: Pre-loaded protocol should not collide with a synthetic
        // protocol class entry in the hierarchy.
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.classes_mut().insert(
            "Tickable".into(),
            crate::semantic_analysis::class_hierarchy::ClassInfo {
                name: "Tickable".into(),
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
            span: span(),
        };

        let diags = registry.add_pre_loaded(vec![protocol], &hierarchy);
        assert!(
            diags.is_empty(),
            "Pre-loaded protocol should not collide with synthetic protocol class, got: {diags:?}"
        );
        assert!(registry.has_protocol("Tickable"));
    }
}
