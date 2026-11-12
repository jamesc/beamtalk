// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared "which class is the cursor in, and what class is the receiver"
//! resolution for the position-based providers (hover, completion,
//! go-to-definition, signature help).
//!
//! Each provider used to carry its own copy of the context enum, the finder
//! and the receiver resolver (BT-4682). They live here once so the providers
//! cannot drift apart.

use beamtalk_core::ast::normalize::canonical_operand;
use beamtalk_core::ast::{ClassDefinition, Expression, Module};
use beamtalk_core::semantic_analysis::type_checker::TypeMap;
use beamtalk_core::semantic_analysis::{ClassHierarchy, InferredType};
use beamtalk_core::source_analysis::Span;
use ecow::EcoString;

use super::type_of_parsed;

/// The class context at a cursor position.
#[derive(Debug, Clone)]
pub(crate) enum ClassContext<'a> {
    /// Inside an instance method of the class.
    InstanceMethod(&'a ClassDefinition),
    /// Inside a class-side method of the class.
    ClassMethod(&'a ClassDefinition),
    /// Inside a standalone instance method (`Counter >> method => ...`).
    StandaloneInstanceMethod(&'a str),
    /// Inside a standalone class method (`Counter class >> method => ...`).
    StandaloneClassMethod(&'a str),
    /// Inside a class body but not inside any method (where `field:` /
    /// `state:` go). `self` has no meaning here.
    ClassBody(&'a ClassDefinition),
    /// Not inside any class.
    TopLevel,
}

impl ClassContext<'_> {
    /// The class `self` denotes here, and whether it is the class side.
    /// `None` outside a method body.
    pub(crate) fn self_class(&self) -> Option<(EcoString, bool)> {
        match self {
            Self::InstanceMethod(class) => Some((class.name.name.clone(), false)),
            Self::ClassMethod(class) => Some((class.name.name.clone(), true)),
            Self::StandaloneInstanceMethod(name) => Some((EcoString::from(*name), false)),
            Self::StandaloneClassMethod(name) => Some((EcoString::from(*name), true)),
            Self::ClassBody(_) | Self::TopLevel => None,
        }
    }

    /// The class `super` denotes here (the enclosing class's superclass),
    /// and whether it is the class side. Standalone methods look the
    /// superclass up in `hierarchy`.
    pub(crate) fn superclass_name(&self, hierarchy: &ClassHierarchy) -> Option<(EcoString, bool)> {
        match self {
            Self::InstanceMethod(class) => class
                .superclass
                .as_ref()
                .map(|superclass| (superclass.name.clone(), false)),
            Self::ClassMethod(class) => class
                .superclass
                .as_ref()
                .map(|superclass| (superclass.name.clone(), true)),
            Self::StandaloneInstanceMethod(_) | Self::StandaloneClassMethod(_) => {
                let (class_name, class_side) = self.self_class()?;
                hierarchy
                    .get_class(class_name.as_str())
                    .and_then(|info| info.superclass.clone())
                    .map(|super_name| (super_name, class_side))
            }
            Self::ClassBody(_) | Self::TopLevel => None,
        }
    }
}

/// Which class/method `offset` is inside of.
///
/// `probe_back` also tests `offset - 1` against each span, so a cursor sitting
/// exactly at the (exclusive) end of a method still counts as inside it. That
/// is what completion wants, since the cursor is after the text being typed;
/// hover, go-to-definition and signature help act on the token under the
/// cursor and pass `false`.
pub(crate) fn find_class_context(
    module: &Module,
    offset: u32,
    probe_back: bool,
) -> ClassContext<'_> {
    let probe = if probe_back {
        offset.saturating_sub(1)
    } else {
        offset
    };
    let in_span = |span: Span| {
        (offset >= span.start() && offset < span.end())
            || (probe >= span.start() && probe < span.end())
    };
    for class in &module.classes {
        for method in &class.methods {
            if in_span(method.span) {
                return ClassContext::InstanceMethod(class);
            }
        }
        for method in &class.class_methods {
            if in_span(method.span) {
                return ClassContext::ClassMethod(class);
            }
        }
        if in_span(class.span) {
            return ClassContext::ClassBody(class);
        }
    }
    for smd in &module.method_definitions {
        if in_span(smd.method.span) {
            if smd.is_class_method {
                return ClassContext::StandaloneClassMethod(smd.class_name.name.as_str());
            }
            return ClassContext::StandaloneInstanceMethod(smd.class_name.name.as_str());
        }
    }
    ClassContext::TopLevel
}

/// Resolves the class of a receiver expression, and whether it is the class
/// side (`Foo bar` is class-side on `Foo`; an instance's type is instance-side).
///
/// The receiver is taken as analysis sees it (BT-4237): `(self) foo:` resolves
/// like `self foo:`, while `(super) foo:` stays an ordinary send.
pub(crate) fn resolve_receiver_class(
    receiver: &Expression,
    context: &ClassContext<'_>,
    hierarchy: &ClassHierarchy,
    type_map: &TypeMap,
) -> Option<(EcoString, bool)> {
    match canonical_operand(receiver) {
        Expression::ClassReference { name, .. } => Some((name.name.clone(), true)),
        Expression::Super(_) => context.superclass_name(hierarchy),
        Expression::Identifier(ident) if ident.name == "self" => context.self_class(),
        // A map shape (`#{#a => 1}`, ADR 0147) resolves as its `Dictionary`.
        _ => type_of_parsed(type_map, receiver)
            .and_then(InferredType::instance_class)
            .map(|(name, _)| (name.clone(), false)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::queries::parse_source;

    const SRC: &str = "Object subclass: Counter\n  state: n :: Integer = 0\n  inc => 1\n  class make => 2\n\nCounter >> twice => 3\nCounter class >> mk => 4\n";

    fn offset_of(needle: &str) -> u32 {
        u32::try_from(SRC.find(needle).expect("needle")).unwrap()
    }

    fn kind(ctx: &ClassContext<'_>) -> &'static str {
        match ctx {
            ClassContext::InstanceMethod(_) => "instance",
            ClassContext::ClassMethod(_) => "class",
            ClassContext::StandaloneInstanceMethod(_) => "standalone-instance",
            ClassContext::StandaloneClassMethod(_) => "standalone-class",
            ClassContext::ClassBody(_) => "body",
            ClassContext::TopLevel => "top",
        }
    }

    #[test]
    fn finds_every_context_kind() {
        let module = parse_source(SRC);
        let at = |needle: &str| kind(&find_class_context(&module, offset_of(needle) + 1, false));
        assert_eq!(at("inc"), "instance");
        assert_eq!(at("class make"), "class");
        assert_eq!(at("=> 3"), "standalone-instance");
        assert_eq!(at("=> 4"), "standalone-class");
        assert_eq!(at("state:"), "body");
    }

    #[test]
    fn probe_back_matches_a_cursor_at_the_exclusive_span_end() {
        let module = parse_source(SRC);
        let end = module.method_definitions[0].method.span.end();
        assert_eq!(
            kind(&find_class_context(&module, end, false)),
            "top",
            "hover/definition/signature help use the half-open span"
        );
        assert_eq!(
            kind(&find_class_context(&module, end, true)),
            "standalone-instance",
            "completion probes one byte back"
        );
    }

    #[test]
    fn self_and_super_follow_the_context() {
        let module = parse_source(SRC);
        let hierarchy = ClassHierarchy::with_builtins();
        let ctx = find_class_context(&module, offset_of("inc") + 1, false);
        assert_eq!(ctx.self_class(), Some((EcoString::from("Counter"), false)));
        assert_eq!(
            ctx.superclass_name(&hierarchy),
            Some((EcoString::from("Object"), false))
        );
        let ctx = find_class_context(&module, offset_of("class make") + 1, false);
        assert_eq!(ctx.self_class(), Some((EcoString::from("Counter"), true)));
        let body = find_class_context(&module, offset_of("state:") + 1, false);
        assert_eq!(body.self_class(), None);
        assert_eq!(ClassContext::TopLevel.self_class(), None);
    }
}
