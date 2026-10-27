// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4210: a method-local (or class-level) type variable named in a method
//! parameter or block parameter annotation resolves through the enclosing
//! method's substitution. Inside a method body that substitution is the
//! identity: `:: E` resolves to the rigid placeholder `Known("E")`, the same
//! type a `-> E` return annotation resolves to. It never collapses to
//! `Dynamic`, never picks up a callee's binding for a same-named variable,
//! and is compatible with anything in mismatch checks.

use super::common::*;

fn diagnostics(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

fn assert_placeholder(ty: &InferredType, name: &str, what: &str) {
    match ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), name, "{what}: got {ty:?}");
            assert!(type_args.is_empty(), "{what}: got {ty:?}");
        }
        other => panic!("{what}: expected placeholder Known({name}), got {other:?}"),
    }
}

/// The first block literal in `class_name>>selector`'s body (searching
/// receivers, arguments and assignment values), with its `type_map` entry.
fn first_block_type<'a>(
    module: &'a Module,
    type_map: &'a TypeMap,
    class_name: &str,
    selector: &str,
) -> &'a InferredType {
    fn find(expr: &Expression) -> Option<&Block> {
        match expr {
            Expression::Block(block) => Some(block),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => find(receiver).or_else(|| arguments.iter().find_map(find)),
            Expression::Assignment { value, .. } => find(value),
            _ => None,
        }
    }
    let method = module
        .classes
        .iter()
        .find(|c| c.name.name == class_name)
        .and_then(|c| c.methods.iter().find(|m| m.selector.name() == selector))
        .unwrap_or_else(|| panic!("{class_name}>>{selector} not found"));
    let block = method
        .body
        .iter()
        .find_map(|s| find(&s.expression))
        .unwrap_or_else(|| panic!("no block literal in {class_name}>>{selector}"));
    type_map
        .get(block.span)
        .unwrap_or_else(|| panic!("no type_map entry for block at {:?}", block.span))
}

fn block_param_type(ty: &InferredType) -> &InferredType {
    match ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } if class_name == "Block" && type_args.len() >= 2 => &type_args[0],
        other => panic!("expected Block(P, R), got {other:?}"),
    }
}

// ---- Method parameters ----

#[test]
fn method_param_type_variable_resolves_to_placeholder_not_dynamic() {
    let params = vec![
        ParameterDefinition {
            name: ident("x"),
            type_annotation: Some(TypeAnnotation::Simple(ident("E"))),
        },
        ParameterDefinition {
            name: ident("r"),
            type_annotation: Some(TypeAnnotation::Generic {
                base: ident("Result"),
                parameters: vec![
                    TypeAnnotation::Simple(ident("T")),
                    TypeAnnotation::Simple(ident("E")),
                ],
                span: span(),
            }),
        },
    ];
    let mut env = TypeEnv::new();
    TypeChecker::set_param_types(
        &mut env,
        &params,
        None,
        &mut type_resolver::ResolutionContext::new(None, &mut std::collections::HashSet::new()),
    );

    assert_placeholder(&env.get_local("x").unwrap(), "E", "x :: E");
    match env.get_local("r").unwrap() {
        InferredType::Known { type_args, .. } => {
            assert_placeholder(&type_args[0], "T", "Result(T, _)");
            assert_placeholder(&type_args[1], "E", "Result(_, E)");
        }
        other => panic!("expected Result(T, E), got {other:?}"),
    }
}

#[test]
fn method_param_type_variable_flows_to_matching_return() {
    let diags = diagnostics(
        r"
typed Object subclass: H
  pass: x :: E -> E => x
  wrap: r :: Result(T, E) -> Result(T, E) => r
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn method_param_type_variable_into_annotated_local_does_not_warn() {
    let diags = diagnostics(
        r"
typed Object subclass: H
  a: x :: E -> Integer =>
    n :: Integer := x.
    n
  b: x :: E -> E =>
    y :: E := 3.
    x
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn class_level_type_variable_param_resolves_to_placeholder() {
    let diags = diagnostics(
        r"
typed Object subclass: Box(T)
  field: item :: T
  put: x :: T -> T => [:y :: T | y] value: x
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

// ---- Block parameters ----

#[test]
fn block_param_type_variable_resolves_like_method_param() {
    let source = r"
typed Object subclass: H
  pass: x :: E -> E => [:y :: E | y] value: x
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        checker.diagnostics().is_empty(),
        "got: {:#?}",
        checker.diagnostics()
    );
    let block_ty = first_block_type(&module, checker.type_map(), "H", "pass:");
    assert_placeholder(block_param_type(block_ty), "E", "[:y :: E | ...]");
}

/// `collect:` pushes `Integer` (its own substitution binds the callee's
/// element type). The annotation names the *enclosing* method's `E`, a
/// different variable: it wins (stays the placeholder, not `Integer`) and
/// raises no false "declared as E, got Integer" mismatch.
#[test]
fn block_param_type_variable_vs_pushed_concrete_type_no_mismatch() {
    let source = r"
typed Object subclass: H
  each: x :: E -> Integer =>
    #(1, 2) collect: [:y :: E | y].
    1
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    assert!(
        checker.diagnostics().is_empty(),
        "got: {:#?}",
        checker.diagnostics()
    );
    let block_ty = first_block_type(&module, checker.type_map(), "H", "each:");
    assert_placeholder(block_param_type(block_ty), "E", "[:y :: E | ...]");
}

/// A concrete annotation against a concrete pushed type still warns: the
/// placeholder exemption is for type variables only.
#[test]
fn concrete_block_param_mismatch_still_warns() {
    let diags = diagnostics(
        r"
typed Object subclass: H
  each: x :: E -> Integer =>
    #(1, 2) collect: [:y :: String | y].
    1
",
    );
    assert!(
        diags.iter().any(|d| d.message.contains("Type mismatch")),
        "got: {diags:#?}"
    );
}
