// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Export-time expansion of a package's own `internal type` aliases
//! (BT-4615).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A dependency's method signatures reach a consumer as type *names*, resolved
//! in the consumer's alias scope. A dependency's `internal type` aliases are
//! deliberately never seeded into that scope ([`AliasRegistry::add_pre_loaded`]
//! drops them, ADR 0108 Phase 5), so a signature mentioning one would resolve
//! to an unknown nominal type in the consumer. The dependency is the only place
//! that knows its own alias table, so it expands them *when its signatures are
//! exported*: every reference to one of its internal aliases is replaced by the
//! alias's (recursively expanded) annotation.
//!
//! [`InternalAliasExpander`] is the one shared implementation, used for both
//! class-method signatures ([`ClassInfo`]) and extension-method signatures
//! (which are stored as strings — [`InternalAliasExpander::expand_str`]).
//! Protocol *ASTs* (`ProtocolDefinition`, which a consumer's own
//! `uses: dep@Proto` flattens provisions from) are rewritten too, by
//! [`InternalAliasExpander::expand_protocol_definition`] (BT-4707): it runs
//! the same `DeclaredType` expansion over each `TypeAnnotation` and rebuilds
//! the annotation, so there is one implementation of the expansion rules.
//!
//! Public aliases are left alone; they keep resolving by name through the
//! seeded registry.
//!
//! [`AliasRegistry::add_pre_loaded`]: super::alias_registry::AliasRegistry::add_pre_loaded

use std::collections::HashMap;

use ecow::EcoString;

use super::alias_registry::AliasInfo;
use super::class_hierarchy::{ClassInfo, DeclaredShapeField, DeclaredType, MethodInfo};
use super::protocol_registry::ProtocolInfo;
use crate::ast::{Expression, MethodDefinition, ProtocolDefinition, TypeAnnotation};
use crate::ast_walker::walk_expression_mut;

/// Expands a package's `internal type` aliases inside exported signatures.
#[derive(Debug, Clone, Default)]
pub struct InternalAliasExpander {
    aliases: HashMap<EcoString, DeclaredType>,
}

impl InternalAliasExpander {
    /// Builds an expander from a package's own alias infos; only
    /// `is_internal` aliases are expanded.
    #[must_use]
    pub fn from_alias_infos(infos: &[AliasInfo]) -> Self {
        Self {
            aliases: infos
                .iter()
                .filter(|info| info.is_internal)
                .map(|info| (info.name.clone(), DeclaredType::from(&info.annotation)))
                .collect(),
        }
    }

    /// `true` when there is nothing to expand.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.aliases.is_empty()
    }

    /// Replaces every internal-alias reference in `ty` by its expansion.
    /// An alias cycle is cut at the repeated name, which stays a bare name.
    #[must_use]
    pub fn expand(&self, ty: &DeclaredType) -> DeclaredType {
        self.expand_shadowing(ty, &[])
    }

    /// [`Self::expand`], except a *direct* reference to one of `shadowed`
    /// stays a bare name. A protocol's own type parameter shadows a
    /// same-named internal alias (`uses:` later maps it to the consumer's type
    /// argument); only direct references in the protocol's annotations are
    /// shadowed — an alias body still means what it meant at module scope.
    #[must_use]
    pub fn expand_shadowing(&self, ty: &DeclaredType, shadowed: &[EcoString]) -> DeclaredType {
        self.expand_inner(ty, &mut Vec::new(), shadowed)
    }

    fn expand_inner<'a>(
        &'a self,
        ty: &DeclaredType,
        active: &mut Vec<&'a EcoString>,
        shadowed: &[EcoString],
    ) -> DeclaredType {
        match ty {
            DeclaredType::Simple(name) => {
                if active.is_empty() && shadowed.contains(name) {
                    return ty.clone();
                }
                let Some((key, target)) = self.aliases.get_key_value(name) else {
                    return ty.clone();
                };
                if active.contains(&key) {
                    return ty.clone();
                }
                active.push(key);
                let expanded = self.expand_inner(target, active, shadowed);
                active.pop();
                expanded
            }
            DeclaredType::Union(members) => {
                let mut out = Vec::with_capacity(members.len());
                for member in members {
                    let expanded = match self.expand_inner(member, active, shadowed) {
                        DeclaredType::Union(inner) => inner,
                        other => vec![other],
                    };
                    for m in expanded {
                        // `internal type Opt = Integer | Nil` used as
                        // `Opt | Nil` must not print `Integer | Nil | Nil`.
                        if !out.contains(&m) {
                            out.push(m);
                        }
                    }
                }
                DeclaredType::Union(out)
            }
            DeclaredType::Generic { base, parameters } => DeclaredType::Generic {
                base: base.clone(),
                parameters: parameters
                    .iter()
                    .map(|p| self.expand_inner(p, active, shadowed))
                    .collect(),
            },
            DeclaredType::FalseOr(inner) => {
                DeclaredType::FalseOr(Box::new(self.expand_inner(inner, active, shadowed)))
            }
            DeclaredType::Shape { fields, open } => DeclaredType::Shape {
                fields: fields
                    .iter()
                    .map(|f| DeclaredShapeField {
                        key: f.key.clone(),
                        value: self.expand_inner(&f.value, active, shadowed),
                        optional: f.optional,
                    })
                    .collect(),
                open: *open,
            },
            DeclaredType::Difference { base, excluded } => DeclaredType::Difference {
                base: Box::new(self.expand_inner(base, active, shadowed)),
                excluded: Box::new(self.expand_inner(excluded, active, shadowed)),
            },
            DeclaredType::Intersection { left, right } => DeclaredType::Intersection {
                left: Box::new(self.expand_inner(left, active, shadowed)),
                right: Box::new(self.expand_inner(right, active, shadowed)),
            },
            DeclaredType::Singleton(_)
            | DeclaredType::SelfType
            | DeclaredType::SelfClass
            | DeclaredType::ClassOf(_) => ty.clone(),
        }
    }

    /// [`Self::expand`] for a flattened signature string (the form
    /// `ExtensionTypeInfo` stores). A string that mentions no internal alias
    /// is returned unchanged, not re-rendered.
    ///
    /// [`DeclaredType::parse`] cannot read `\` or `&`, so a string using them
    /// is re-read by the real type-annotation parser; an alias inside a
    /// difference or intersection is then expanded like any other (BT-4707).
    #[must_use]
    pub fn expand_str(&self, signature: &EcoString) -> EcoString {
        self.expand_str_shadowing(signature, &[])
    }

    /// [`Self::expand_str`] with [`Self::expand_shadowing`]'s shadow set.
    #[must_use]
    pub fn expand_str_shadowing(&self, signature: &EcoString, shadowed: &[EcoString]) -> EcoString {
        if self.is_empty() {
            return signature.clone();
        }
        let mut parsed = DeclaredType::parse(signature);
        if signature.contains(['\\', '&']) {
            if let Some(real) = parse_type_with_parser(signature) {
                parsed = real;
            }
        }
        let expanded = self.expand_shadowing(&parsed, shadowed);
        if expanded == parsed {
            signature.clone()
        } else {
            EcoString::from(expanded.to_string())
        }
    }

    /// [`Self::expand`] for a `TypeAnnotation`. An annotation that mentions
    /// no internal alias is returned untouched (spans intact); otherwise the
    /// expansion replaces it whole, every node carrying the annotation's
    /// original span so diagnostics still point into the dependency's own
    /// source rather than at the alias declaration.
    #[must_use]
    pub fn expand_annotation(&self, ann: &TypeAnnotation) -> TypeAnnotation {
        self.expand_annotation_shadowing(ann, &[])
    }

    /// [`Self::expand_annotation`] with [`Self::expand_shadowing`]'s shadow set.
    #[must_use]
    pub fn expand_annotation_shadowing(
        &self,
        ann: &TypeAnnotation,
        shadowed: &[EcoString],
    ) -> TypeAnnotation {
        let declared = DeclaredType::from(ann);
        let expanded = self.expand_shadowing(&declared, shadowed);
        if expanded == declared {
            ann.clone()
        } else {
            expanded.to_annotation(ann.span())
        }
    }

    /// Expands the dependency's own internal aliases in a protocol AST that
    /// is exported for a consumer's `uses: dep@Proto` to flatten (BT-4707):
    /// required and provided method signatures, and the typed assignments and
    /// block parameters inside provided method bodies.
    pub fn expand_protocol_definition(&self, protocol: &mut ProtocolDefinition) {
        if self.is_empty() {
            return;
        }
        let shadowed: Vec<EcoString> = protocol
            .type_params
            .iter()
            .map(|tp| tp.name.name.clone())
            .collect();
        for sig in protocol
            .method_signatures
            .iter_mut()
            .chain(&mut protocol.class_method_signatures)
        {
            for param in &mut sig.parameters {
                if let Some(ty) = &mut param.type_annotation {
                    *ty = self.expand_annotation_shadowing(ty, &shadowed);
                }
            }
            if let Some(ty) = &mut sig.return_type {
                *ty = self.expand_annotation_shadowing(ty, &shadowed);
            }
        }
        for method in &mut protocol.provided_methods {
            self.expand_provided_method(method, &shadowed);
        }
    }

    fn expand_provided_method(&self, method: &mut MethodDefinition, shadowed: &[EcoString]) {
        for param in &mut method.parameters {
            if let Some(ty) = &mut param.type_annotation {
                *ty = self.expand_annotation_shadowing(ty, shadowed);
            }
        }
        if let Some(ty) = &mut method.return_type {
            *ty = self.expand_annotation_shadowing(ty, shadowed);
        }
        for stmt in &mut method.body {
            walk_expression_mut(&mut stmt.expression, &mut |expr| match expr {
                Expression::Assignment {
                    type_annotation: Some(ty),
                    ..
                } => *ty = self.expand_annotation_shadowing(ty, shadowed),
                Expression::Block(block) => {
                    for param in &mut block.parameters {
                        if let Some(ty) = &mut param.type_annotation {
                            *ty = self.expand_annotation_shadowing(ty, shadowed);
                        }
                    }
                }
                _ => {}
            });
        }
    }

    /// Expands every exported signature of `info`: method and class-method
    /// parameter/return types, state field types and class-variable types.
    pub fn expand_class_info(&self, info: &mut ClassInfo) {
        if self.is_empty() {
            return;
        }
        for method in info.methods.iter_mut().chain(&mut info.class_methods) {
            self.expand_method(method);
        }
        for ty in info.state_types.values_mut() {
            *ty = self.expand(ty);
        }
        for var in &mut info.class_variables {
            if let Some(ty) = &mut var.ty {
                *ty = self.expand(ty);
            }
        }
    }

    /// Expands a protocol's required and provided method signatures.
    pub fn expand_protocol_info(&self, info: &mut ProtocolInfo) {
        if self.is_empty() {
            return;
        }
        // Same shadowing rule as `expand_protocol_definition`, so the exported
        // `ProtocolInfo` and the exported AST agree.
        let shadowed = info.type_params.clone();
        let all = info
            .methods
            .iter_mut()
            .chain(&mut info.class_methods)
            .chain(&mut info.provided);
        for req in all {
            for ty in req.param_types.iter_mut().flatten() {
                *ty = self.expand_str_shadowing(ty, &shadowed);
            }
            if let Some(ty) = &mut req.return_type {
                *ty = self.expand_str_shadowing(ty, &shadowed);
            }
        }
    }

    fn expand_method(&self, method: &mut MethodInfo) {
        if let Some(ty) = &mut method.return_type {
            *ty = self.expand(ty);
        }
        for ty in method.param_types.iter_mut().flatten() {
            *ty = self.expand(ty);
        }
    }
}

/// Reads a flattened type string with the real parser, by wrapping it as a
/// method return type. `None` when it does not parse cleanly.
fn parse_type_with_parser(signature: &str) -> Option<DeclaredType> {
    use crate::source_analysis::{Severity, lex_with_eof, parse};
    let source = format!("Object subclass: AliasExpansionProbe\n  probe -> {signature} => nil\n");
    let (module, diagnostics) = parse(lex_with_eof(&source));
    if diagnostics.iter().any(|d| d.severity == Severity::Error) {
        return None;
    }
    let ann = module
        .classes
        .first()?
        .methods
        .first()?
        .return_type
        .as_ref()?;
    Some(DeclaredType::from(ann))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Identifier;
    use crate::source_analysis::Span;

    fn alias(name: &str, rhs: &str, is_internal: bool) -> AliasInfo {
        AliasInfo {
            name: name.into(),
            annotation: TypeAnnotation::Simple(Identifier::new(rhs, Span::new(0, 0))),
            is_internal,
            package: Some("dep".into()),
            span: Span::new(0, 0),
        }
    }

    fn expander(pairs: &[(&str, DeclaredType)]) -> InternalAliasExpander {
        InternalAliasExpander {
            aliases: pairs
                .iter()
                .map(|(n, t)| ((*n).into(), t.clone()))
                .collect(),
        }
    }

    #[test]
    fn expands_simple_and_chain() {
        let e = expander(&[
            ("A", DeclaredType::simple("B")),
            ("B", DeclaredType::simple("Integer")),
        ]);
        assert_eq!(
            e.expand(&DeclaredType::simple("A")),
            DeclaredType::simple("Integer")
        );
    }

    #[test]
    fn expands_generic_argument_and_union_member() {
        let e = expander(&[("Secret", DeclaredType::simple("Integer"))]);
        assert_eq!(
            e.expand(&DeclaredType::parse("List(Secret)")).to_string(),
            "List(Integer)"
        );
        assert_eq!(
            e.expand(&DeclaredType::parse("Secret | Nil")).to_string(),
            "Integer | Nil"
        );
    }

    #[test]
    fn union_alias_flattens_into_union() {
        let e = expander(&[("U", DeclaredType::parse("Integer | String"))]);
        assert_eq!(
            e.expand(&DeclaredType::parse("U | Nil")).to_string(),
            "Integer | String | Nil"
        );
    }

    #[test]
    fn union_expansion_dedupes_members() {
        let e = expander(&[("Opt", DeclaredType::parse("Integer | Nil"))]);
        assert_eq!(
            e.expand(&DeclaredType::parse("Opt | Nil")).to_string(),
            "Integer | Nil"
        );
    }

    #[test]
    fn cycle_terminates() {
        let e = expander(&[
            ("A", DeclaredType::simple("B")),
            ("B", DeclaredType::simple("A")),
        ]);
        // Terminates; the repeated name stays a bare name.
        assert_eq!(
            e.expand(&DeclaredType::simple("A")),
            DeclaredType::simple("A")
        );
    }

    #[test]
    fn public_aliases_and_classes_are_untouched() {
        let e = InternalAliasExpander::from_alias_infos(&[
            alias("Pub", "Integer", false),
            alias("Priv", "String", true),
        ]);
        assert_eq!(
            e.expand(&DeclaredType::simple("Pub")),
            DeclaredType::simple("Pub")
        );
        assert_eq!(
            e.expand(&DeclaredType::simple("Counter")),
            DeclaredType::simple("Counter")
        );
        assert_eq!(
            e.expand(&DeclaredType::simple("Priv")),
            DeclaredType::simple("String")
        );
    }

    #[test]
    fn expand_str_rewrites_only_when_needed() {
        let e = expander(&[("Secret", DeclaredType::simple("Integer"))]);
        assert_eq!(e.expand_str(&"List(Secret)".into()), "List(Integer)");
        assert_eq!(e.expand_str(&"Foo|Bar".into()), "Foo|Bar");
    }

    #[test]
    fn expand_str_reads_difference_and_intersection() {
        let e = expander(&[
            ("Secret", DeclaredType::simple("String")),
            ("Tagged", DeclaredType::parse("Symbol")),
        ]);
        assert_eq!(e.expand_str(&"Secret \\ #foo".into()), "String \\ #foo");
        assert_eq!(
            e.expand_str(&"Collection(Secret) & Comparable".into()),
            "Collection(String) & Comparable"
        );
        // Untouched when no alias is mentioned.
        assert_eq!(e.expand_str(&"Symbol \\ #foo".into()), "Symbol \\ #foo");
    }

    #[test]
    fn alias_expanding_to_difference_is_structured_in_annotations() {
        let e = expander(&[(
            "Name",
            DeclaredType::Difference {
                base: Box::new(DeclaredType::simple("Symbol")),
                excluded: Box::new(DeclaredType::singleton("nil")),
            },
        )]);
        let ann = TypeAnnotation::Simple(Identifier::new("Name", Span::new(4, 8)));
        let out = e.expand_annotation(&ann);
        assert_eq!(out.type_name(), "Symbol \\ #nil");
        assert_eq!(out.span(), Span::new(4, 8));
    }

    #[test]
    fn expand_annotation_keeps_untouched_annotation_verbatim() {
        let e = expander(&[("Secret", DeclaredType::simple("Integer"))]);
        let ann = TypeAnnotation::Simple(Identifier::new("Counter", Span::new(2, 9)));
        assert_eq!(e.expand_annotation(&ann), ann);
    }

    fn first_protocol(source: &str) -> ProtocolDefinition {
        let (module, diagnostics) =
            crate::source_analysis::parse(crate::source_analysis::lex_with_eof(source));
        assert!(
            diagnostics
                .iter()
                .all(|d| d.severity != crate::source_analysis::Severity::Error),
            "{diagnostics:?}"
        );
        module.protocols[0].clone()
    }

    #[test]
    fn expands_protocol_definition_signatures_and_provisions() {
        let e = expander(&[("Secret", DeclaredType::simple("String"))]);
        let mut proto = first_protocol(
            "Protocol define: Tagged\n  \
             lookup: key :: Secret -> Secret\n  \
             tag -> Secret => \"x\"\n  \
             wrap: v :: Secret -> Secret =>\n    \
             y :: Secret := v\n    \
             #(1) do: [:i :: Secret | i]\n    \
             y\n",
        );
        e.expand_protocol_definition(&mut proto);
        let sig = &proto.method_signatures[0];
        assert_eq!(
            sig.parameters[0]
                .type_annotation
                .as_ref()
                .unwrap()
                .type_name(),
            "String"
        );
        assert_eq!(sig.return_type.as_ref().unwrap().type_name(), "String");
        let tag = &proto.provided_methods[0];
        assert_eq!(tag.return_type.as_ref().unwrap().type_name(), "String");
        let wrap = &proto.provided_methods[1];
        assert_eq!(
            wrap.parameters[0]
                .type_annotation
                .as_ref()
                .unwrap()
                .type_name(),
            "String"
        );
        let mut seen = Vec::new();
        for stmt in &mut wrap.clone().body {
            walk_expression_mut(&mut stmt.expression, &mut |expr| match expr {
                Expression::Assignment {
                    type_annotation: Some(ty),
                    ..
                } => seen.push(ty.type_name()),
                Expression::Block(b) => {
                    for p in &b.parameters {
                        if let Some(ty) = &p.type_annotation {
                            seen.push(ty.type_name());
                        }
                    }
                }
                _ => {}
            });
        }
        assert_eq!(seen, ["String", "String"]);
    }

    #[test]
    fn protocol_type_param_shadows_same_named_internal_alias() {
        let e = expander(&[
            ("Key", DeclaredType::simple("Symbol")),
            ("Secret", DeclaredType::simple("String")),
        ]);
        let mut proto = first_protocol(
            "Protocol define: Mapping(Key, Value)\n  \
             at: k :: Key -> Value\n\n  \
             pick: k :: Key -> Secret => nil\n",
        );
        e.expand_protocol_definition(&mut proto);
        let sig = &proto.method_signatures[0];
        assert_eq!(
            sig.parameters[0]
                .type_annotation
                .as_ref()
                .unwrap()
                .type_name(),
            "Key"
        );
        let pick = &proto.provided_methods[0];
        assert_eq!(
            pick.parameters[0]
                .type_annotation
                .as_ref()
                .unwrap()
                .type_name(),
            "Key"
        );
        // Other aliases still expand.
        assert_eq!(pick.return_type.as_ref().unwrap().type_name(), "String");
    }

    #[test]
    fn shadowing_applies_to_direct_references_only() {
        let e = expander(&[
            ("Key", DeclaredType::simple("Symbol")),
            ("Secret", DeclaredType::parse("Key | Nil")),
        ]);
        let mut proto = first_protocol(
            "Protocol define: Mapping(Key, V)\n  \
             pick: k :: Key -> Secret => nil\n",
        );
        e.expand_protocol_definition(&mut proto);
        let pick = &proto.provided_methods[0];
        assert_eq!(
            pick.parameters[0]
                .type_annotation
                .as_ref()
                .unwrap()
                .type_name(),
            "Key"
        );
        // `Secret` was declared at module scope, where `Key` is the alias.
        assert_eq!(
            pick.return_type.as_ref().unwrap().type_name(),
            "Symbol | Nil"
        );
    }

    #[test]
    fn protocol_info_applies_the_same_shadowing() {
        use crate::semantic_analysis::protocol_registry::ProtocolMethodRequirement;
        let e = expander(&[("Key", DeclaredType::simple("Symbol"))]);
        let mut info = ProtocolInfo {
            name: "Mapping".into(),
            type_params: vec!["Key".into()],
            type_param_bounds: vec![None],
            extending: None,
            methods: vec![ProtocolMethodRequirement {
                selector: "at:".into(),
                arity: 1,
                param_types: vec![Some("Key".into())],
                return_type: Some("Key".into()),
            }],
            class_methods: Vec::new(),
            provided: Vec::new(),
            package: None,
            span: Span::new(0, 0),
        };
        e.expand_protocol_info(&mut info);
        assert_eq!(info.methods[0].param_types[0].as_deref(), Some("Key"));
        assert_eq!(info.methods[0].return_type.as_deref(), Some("Key"));
    }
}
