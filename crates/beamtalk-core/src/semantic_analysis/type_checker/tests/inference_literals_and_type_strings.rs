// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Literal-index tuple `at:`, `infer_literal`, `resolve_type_annotation`, and
//! plain `resolve_type_string` inference.

use super::super::*;
use super::common::*;

// ---- BT-2254: literal-index tuple `at:` ----

fn tuple_args(names: &[&str]) -> Vec<InferredType> {
    names.iter().map(|n| InferredType::known(*n)).collect()
}

#[test]
fn literal_index_tuple_at_in_range() {
    let args = tuple_args(&["Symbol", "Integer", "Symbol"]);
    // `at: 1` → first element type
    assert_eq!(
        TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[int_lit(1)]),
        Some(InferredType::known("Symbol"))
    );
    // `at: 2` → second element type
    assert_eq!(
        TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[int_lit(2)]),
        Some(InferredType::known("Integer"))
    );
}

#[test]
fn literal_index_tuple_at_out_of_range_is_dynamic() {
    let args = tuple_args(&["Symbol", "Integer"]);
    // Index 3 is out of range (only 2 elements) → None (falls back to Dynamic)
    assert_eq!(
        TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[int_lit(3)]),
        None
    );
    // Index 0 is out of range (1-based) → None
    assert_eq!(
        TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[int_lit(0)]),
        None
    );
}

#[test]
fn literal_index_tuple_at_non_literal_is_dynamic() {
    let args = tuple_args(&["Symbol", "Integer"]);
    // Non-literal index (a variable) → None
    assert_eq!(
        TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[var("i")]),
        None
    );
}

#[test]
fn literal_index_tuple_at_bare_tuple_is_dynamic() {
    // No positional element types (bare `Tuple`) → None
    assert_eq!(
        TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &[], &[int_lit(1)]),
        None
    );
}

#[test]
fn literal_index_tuple_at_non_tuple_receiver_ignored() {
    let args = tuple_args(&["Integer"]);
    // Not a Tuple receiver → None (List uses its own at: handling)
    assert_eq!(
        TypeChecker::infer_literal_index_tuple_at("List", "at:", &args, &[int_lit(1)]),
        None
    );
}

// ---- infer_literal ----

#[test]
fn infer_literal_integer() {
    assert_eq!(
        TypeChecker::infer_literal(&Literal::Integer(42)),
        InferredType::known("Integer")
    );
}

#[test]
fn infer_literal_float() {
    assert_eq!(
        TypeChecker::infer_literal(&Literal::Float(2.5)),
        InferredType::known("Float")
    );
}

#[test]
fn infer_literal_string() {
    assert_eq!(
        TypeChecker::infer_literal(&Literal::String("hello".into())),
        InferredType::known("String")
    );
}

#[test]
fn infer_literal_symbol() {
    assert_eq!(
        TypeChecker::infer_literal(&Literal::Symbol("ok".into())),
        InferredType::known("#ok")
    );
}

#[test]
fn infer_literal_character() {
    assert_eq!(
        TypeChecker::infer_literal(&Literal::Character('x')),
        InferredType::known("Character")
    );
}

#[test]
fn infer_literal_list() {
    assert_eq!(
        TypeChecker::infer_literal(&Literal::List(vec![])),
        InferredType::known("List")
    );
}

// ---- resolve_type_annotation ----

#[test]
fn resolve_simple_type_annotation() {
    let ann = TypeAnnotation::Simple(ident("Integer"));
    assert_eq!(
        TypeChecker::resolve_type_annotation(&ann),
        InferredType::known("Integer")
    );
}

#[test]
fn resolve_nil_keyword_annotation() {
    let ann = TypeAnnotation::Simple(ident("nil"));
    assert_eq!(
        TypeChecker::resolve_type_annotation(&ann),
        InferredType::known("UndefinedObject")
    );
}

#[test]
fn resolve_false_keyword_annotation() {
    let ann = TypeAnnotation::Simple(ident("false"));
    assert_eq!(
        TypeChecker::resolve_type_annotation(&ann),
        InferredType::known("False")
    );
}

#[test]
fn resolve_true_keyword_annotation() {
    let ann = TypeAnnotation::Simple(ident("true"));
    assert_eq!(
        TypeChecker::resolve_type_annotation(&ann),
        InferredType::known("True")
    );
}

#[test]
fn resolve_generic_type_annotation() {
    let ann = TypeAnnotation::Generic {
        base: ident("Result"),
        parameters: vec![
            TypeAnnotation::Simple(ident("Integer")),
            TypeAnnotation::Simple(ident("String")),
        ],
        span: span(),
    };
    let result = TypeChecker::resolve_type_annotation(&ann);
    match result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Result");
            assert_eq!(type_args.len(), 2);
            assert_eq!(type_args[0], InferredType::known("Integer"));
            assert_eq!(type_args[1], InferredType::known("String"));
        }
        other => panic!("Expected Known, got {other:?}"),
    }
}

#[test]
fn resolve_union_type_annotation() {
    let ann = TypeAnnotation::Union {
        types: vec![
            TypeAnnotation::Simple(ident("String")),
            TypeAnnotation::Simple(ident("nil")),
        ],
        span: span(),
    };
    let result = TypeChecker::resolve_type_annotation(&ann);
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&InferredType::known("String")));
            assert!(members.contains(&InferredType::known("UndefinedObject")));
        }
        other => panic!("Expected Union, got {other:?}"),
    }
}

#[test]
fn resolve_false_or_type_annotation() {
    let ann = TypeAnnotation::FalseOr {
        inner: Box::new(TypeAnnotation::Simple(ident("Integer"))),
        span: span(),
    };
    let result = TypeChecker::resolve_type_annotation(&ann);
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&InferredType::known("Integer")));
            assert!(members.contains(&InferredType::known("False")));
        }
        other => panic!("Expected Union, got {other:?}"),
    }
}

#[test]
fn resolve_self_type_annotation() {
    let ann = TypeAnnotation::SelfType { span: span() };
    assert_eq!(
        TypeChecker::resolve_type_annotation(&ann),
        InferredType::Dynamic(DynamicReason::Unknown)
    );
}

#[test]
fn resolve_self_class_type_annotation() {
    let ann = TypeAnnotation::SelfClass { span: span() };
    assert_eq!(
        TypeChecker::resolve_type_annotation(&ann),
        InferredType::Dynamic(DynamicReason::Unknown)
    );
}

#[test]
fn resolve_singleton_type_annotation() {
    let ann = TypeAnnotation::Singleton {
        name: "north".into(),
        span: span(),
    };
    assert_eq!(
        TypeChecker::resolve_type_annotation(&ann),
        InferredType::known("#north")
    );
}

// ---- resolve_type_string (plain) ----

#[test]
fn resolve_type_string_simple() {
    assert_eq!(
        type_resolver::resolve_declared_type(
            &DeclaredType::parse("Integer"),
            &type_resolver::SubstitutionMap::new(),
            None,
            None,
            TypeStringContext::Declared
        ),
        InferredType::known("Integer")
    );
}

#[test]
fn resolve_type_string_nil_keyword() {
    assert_eq!(
        type_resolver::resolve_declared_type(
            &DeclaredType::parse("nil"),
            &type_resolver::SubstitutionMap::new(),
            None,
            None,
            TypeStringContext::Declared
        ),
        InferredType::known("UndefinedObject")
    );
}

#[test]
fn resolve_type_string_union() {
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("String | nil"),
        &type_resolver::SubstitutionMap::new(),
        None,
        None,
        TypeStringContext::Declared,
    );
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&InferredType::known("String")));
            assert!(members.contains(&InferredType::known("UndefinedObject")));
        }
        other => panic!("Expected Union, got {other:?}"),
    }
}

#[test]
fn resolve_type_string_three_way_union() {
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Integer | String | nil"),
        &type_resolver::SubstitutionMap::new(),
        None,
        None,
        TypeStringContext::Declared,
    );
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 3);
            assert!(members.contains(&InferredType::known("Integer")));
            assert!(members.contains(&InferredType::known("String")));
            assert!(members.contains(&InferredType::known("UndefinedObject")));
        }
        other => panic!("Expected Union, got {other:?}"),
    }
}

/// BT-2928: a cross-file alias name (as stored raw in a `ClassInfo`/
/// `MethodInfo` string, e.g. a `MethodInfo::return_type` extracted from
/// another file's `-> RestartStrategy` annotation) expands to its
/// declared union through the alias registry, instead of staying an
/// opaque, unresolved nominal class.
#[test]
fn resolve_type_string_expands_alias_from_registry() {
    use crate::ast::{Module, TypeAliasDefinition};
    use crate::semantic_analysis::alias_registry::AliasRegistry;
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let ann = TypeAnnotation::Union {
        types: vec![
            TypeAnnotation::Singleton {
                name: "permanent".into(),
                span: span(),
            },
            TypeAnnotation::Singleton {
                name: "temporary".into(),
                span: span(),
            },
        ],
        span: span(),
    };
    let module = Module {
        type_aliases: vec![TypeAliasDefinition {
            name: ident("RestartStrategy"),
            annotation: ann,
            is_internal: false,
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let protocol_registry = ProtocolRegistry::new();
    let mut registry = AliasRegistry::new();
    let diags = registry.register_module(&module, &hierarchy, &protocol_registry);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");

    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("RestartStrategy"),
        &type_resolver::SubstitutionMap::new(),
        None,
        Some(&registry),
        TypeStringContext::Declared,
    );
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&InferredType::known("#permanent")));
            assert!(members.contains(&InferredType::known("#temporary")));
        }
        other => panic!("Expected Union (alias expansion), got {other:?}"),
    }

    // Without the registry, the same raw string stays an opaque nominal
    // class — the pre-BT-2928 behaviour this test guards against
    // regressing back to.
    let unresolved = type_resolver::resolve_declared_type(
        &DeclaredType::parse("RestartStrategy"),
        &type_resolver::SubstitutionMap::new(),
        None,
        None,
        TypeStringContext::Declared,
    );
    assert_eq!(unresolved, InferredType::known("RestartStrategy"));
}

/// BT-2936: a `self.field` narrowing lookup (e.g. the `self.field isNil
/// ifFalse: [...]` shape [`Self::resolve_narrowing_variable_type`]
/// serves) on an alias-typed field expands the alias through the
/// threaded `alias_registry`, instead of leaving it an opaque nominal
/// class — the deferred half of BT-2928's `resolve_type_name_string`
/// fix for the narrowing call chain specifically.
#[test]
fn resolve_narrowing_variable_type_expands_alias_for_self_field() {
    use crate::semantic_analysis::alias_registry::AliasRegistry;
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let tokens = crate::source_analysis::lex_with_eof(
        "type RestartStrategy = #permanent | #temporary\n\
         Object subclass: Supervisor\n  state: strategy :: RestartStrategy = nil\n",
    );
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "parse failed: {parse_diags:?}");
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let protocol_registry = ProtocolRegistry::new();
    let mut registry = AliasRegistry::new();
    let diags = registry.register_module(&module, &hierarchy, &protocol_registry);
    assert!(diags.is_empty(), "unexpected alias diagnostics: {diags:?}");

    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Supervisor"));
    let var_key = EnvKey::self_field("strategy");

    let result =
        TypeChecker::resolve_narrowing_variable_type(&var_key, &env, &hierarchy, Some(&registry));
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&InferredType::known("#permanent")));
            assert!(members.contains(&InferredType::known("#temporary")));
        }
        other => panic!("Expected Union (alias expansion), got {other:?}"),
    }

    // Without the registry, the field's raw type name stays opaque — the
    // pre-BT-2936 fallback this test guards against regressing back to.
    let unresolved = TypeChecker::resolve_narrowing_variable_type(&var_key, &env, &hierarchy, None);
    assert_eq!(unresolved, InferredType::known("RestartStrategy"));
}
