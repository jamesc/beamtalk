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
//! Public aliases are left alone; they keep resolving by name through the
//! seeded registry.
//!
//! [`AliasRegistry::add_pre_loaded`]: super::alias_registry::AliasRegistry::add_pre_loaded

use std::collections::HashMap;

use ecow::EcoString;

use super::alias_registry::AliasInfo;
use super::class_hierarchy::{ClassInfo, DeclaredType, MethodInfo};

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
        self.expand_inner(ty, &mut Vec::new())
    }

    fn expand_inner<'a>(
        &'a self,
        ty: &DeclaredType,
        active: &mut Vec<&'a EcoString>,
    ) -> DeclaredType {
        match ty {
            DeclaredType::Simple(name) => {
                let Some((key, target)) = self.aliases.get_key_value(name) else {
                    return ty.clone();
                };
                if active.contains(&key) {
                    return ty.clone();
                }
                active.push(key);
                let expanded = self.expand_inner(target, active);
                active.pop();
                expanded
            }
            DeclaredType::Union(members) => {
                let mut out = Vec::with_capacity(members.len());
                for member in members {
                    match self.expand_inner(member, active) {
                        DeclaredType::Union(inner) => out.extend(inner),
                        other => out.push(other),
                    }
                }
                DeclaredType::Union(out)
            }
            DeclaredType::Generic { base, parameters } => DeclaredType::Generic {
                base: base.clone(),
                parameters: parameters
                    .iter()
                    .map(|p| self.expand_inner(p, active))
                    .collect(),
            },
            DeclaredType::FalseOr(inner) => {
                DeclaredType::FalseOr(Box::new(self.expand_inner(inner, active)))
            }
            DeclaredType::Difference { base, excluded } => DeclaredType::Difference {
                base: Box::new(self.expand_inner(base, active)),
                excluded: Box::new(self.expand_inner(excluded, active)),
            },
            DeclaredType::Intersection { left, right } => DeclaredType::Intersection {
                left: Box::new(self.expand_inner(left, active)),
                right: Box::new(self.expand_inner(right, active)),
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
    #[must_use]
    pub fn expand_str(&self, signature: &EcoString) -> EcoString {
        if self.is_empty() {
            return signature.clone();
        }
        let parsed = DeclaredType::parse(signature);
        let expanded = self.expand(&parsed);
        if expanded == parsed {
            signature.clone()
        } else {
            EcoString::from(expanded.to_string())
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

    fn expand_method(&self, method: &mut MethodInfo) {
        if let Some(ty) = &mut method.return_type {
            *ty = self.expand(ty);
        }
        for ty in method.param_types.iter_mut().flatten() {
            *ty = self.expand(ty);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Identifier, TypeAnnotation};
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
}
