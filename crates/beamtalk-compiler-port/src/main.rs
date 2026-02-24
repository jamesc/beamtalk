// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP Port binary for the Beamtalk compiler (ADR 0022).
//!
//! **DDD Context:** Compilation (Anti-Corruption Layer boundary)
//!
//! Reads ETF-encoded requests from stdin ({packet, 4} framing),
//! calls beamtalk-core compile functions, and writes ETF-encoded
//! responses to stdout.
//!
//! Supports commands: `compile_expression`, `compile`, `diagnostics`, `version`.

use std::io::{self, Read, Write};

use clap::{ArgAction, Parser};
use tracing_subscriber::{self, EnvFilter};

use eetf::{Atom, Binary, List, Map, Term};

/// Read a {packet, 4} framed message from stdin.
fn read_packet(stdin: &mut impl Read) -> io::Result<Option<Vec<u8>>> {
    let mut len_buf = [0u8; 4];
    match stdin.read_exact(&mut len_buf) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    }
    let len = u32::from_be_bytes(len_buf) as usize;
    // Guard against unreasonably large packets (>64 MiB)
    if len > 64 * 1024 * 1024 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("packet too large: {len} bytes"),
        ));
    }
    let mut buf = vec![0u8; len];
    stdin.read_exact(&mut buf)?;
    Ok(Some(buf))
}

/// Write a `{packet, 4}` framed message to stdout.
fn write_packet(stdout: &mut impl Write, data: &[u8]) -> io::Result<()> {
    let len = u32::try_from(data.len())
        .expect("packet too large for {packet, 4} framing")
        .to_be_bytes();
    stdout.write_all(&len)?;
    stdout.write_all(data)?;
    stdout.flush()
}

/// Helper to create an atom Term.
fn atom(name: &str) -> Term {
    Term::from(Atom::from(name))
}

/// Helper to create a binary Term from a string.
fn binary(s: &str) -> Term {
    Term::from(Binary::from(s.as_bytes()))
}

/// Extract a binary string value from a Term.
fn term_to_string(term: &Term) -> Option<String> {
    match term {
        Term::Binary(b) => String::from_utf8(b.bytes.clone()).ok(),
        // Erlang may send short strings as ByteList
        Term::ByteList(bl) => String::from_utf8(bl.bytes.clone()).ok(),
        _ => None,
    }
}

/// Extract a list of strings from a Term (for `known_vars`).
fn term_to_string_list(term: &Term) -> Option<Vec<String>> {
    match term {
        Term::List(list) => {
            let mut result = Vec::new();
            for elem in &list.elements {
                result.push(term_to_string(elem)?);
            }
            Some(result)
        }
        _ => None,
    }
}

/// Look up a key (atom) in an ETF map.
fn map_get<'a>(map: &'a Map, key: &str) -> Option<&'a Term> {
    map.map.get(&atom(key))
}

/// Extract a string→string map from a Term (for `class_module_index`).
/// Returns `Err` if the term is present but contains non-string keys or values,
/// so callers can surface the problem rather than silently falling back.
fn term_to_string_map(term: &Term) -> Result<std::collections::HashMap<String, String>, String> {
    match term {
        Term::Map(m) => {
            let mut result = std::collections::HashMap::new();
            for (k, v) in &m.map {
                let key = term_to_string(k)
                    .ok_or_else(|| format!("class_module_index key is not a string: {k:?}"))?;
                let val = term_to_string(v).ok_or_else(|| {
                    format!("class_module_index value for '{key}' is not a string: {v:?}")
                })?;
                result.insert(key, val);
            }
            Ok(result)
        }
        _ => Err(format!(
            "class_module_index must be a map of string→string, got: {term:?}"
        )),
    }
}

/// Extract a boolean value from a Term.
fn term_to_bool(term: &Term) -> Option<bool> {
    match term {
        Term::Atom(a) => match a.name.as_str() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// Merge a method into a method list, replacing any existing method with the same selector and kind.
fn merge_method(
    methods: &mut Vec<beamtalk_core::ast::MethodDefinition>,
    method: beamtalk_core::ast::MethodDefinition,
) {
    let selector = method.selector.name();
    if let Some(existing) = methods
        .iter_mut()
        .find(|m| m.selector.name() == selector && m.kind == method.kind)
    {
        *existing = method;
    } else {
        methods.push(method);
    }
}

/// Build a response map for a successful `compile_expression`.
fn ok_response(core_erlang: &str, warnings: &[String]) -> Term {
    let warning_terms: Vec<Term> = warnings.iter().map(|w| binary(w)).collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("warnings"), Term::from(List::from(warning_terms))),
    ]))
}

/// Build a response map for a successful inline class definition in REPL.
/// BT-839: includes `dynamic_class_expr` for Path 2 (`ClassBuilder`) evaluation.
fn class_definition_ok_response(
    core_erlang: &str,
    module_name: &str,
    classes: &[(String, String)],
    warnings: &[String],
    dynamic_class_expr: &str,
) -> Term {
    let warning_terms: Vec<Term> = warnings.iter().map(|w| binary(w)).collect();
    let class_terms: Vec<Term> = classes
        .iter()
        .map(|(name, superclass)| {
            Term::from(Map::from([
                (atom("name"), binary(name)),
                (atom("superclass"), binary(superclass)),
            ]))
        })
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("kind"), atom("class_definition")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("module_name"), binary(module_name)),
        (atom("classes"), Term::from(List::from(class_terms))),
        (atom("warnings"), Term::from(List::from(warning_terms))),
        (atom("dynamic_class_expr"), binary(dynamic_class_expr)),
    ]))
}

/// Build a response map for a successful standalone method definition in REPL.
fn method_definition_ok_response(
    class_name: &str,
    selector: &str,
    is_class_method: bool,
    method_source: &str,
    warnings: &[String],
) -> Term {
    let warning_terms: Vec<Term> = warnings.iter().map(|w| binary(w)).collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("kind"), atom("method_definition")),
        (atom("class_name"), binary(class_name)),
        (atom("selector"), binary(selector)),
        (
            atom("is_class_method"),
            if is_class_method {
                atom("true")
            } else {
                atom("false")
            },
        ),
        (atom("method_source"), binary(method_source)),
        (atom("warnings"), Term::from(List::from(warning_terms))),
    ]))
}

/// Build a response map for a successful `compile` (file compilation).
fn compile_ok_response(
    core_erlang: &str,
    module_name: &str,
    classes: &[(String, String)],
    warnings: &[String],
) -> Term {
    let warning_terms: Vec<Term> = warnings.iter().map(|w| binary(w)).collect();
    let class_terms: Vec<Term> = classes
        .iter()
        .map(|(name, superclass)| {
            Term::from(Map::from([
                (atom("name"), binary(name)),
                (atom("superclass"), binary(superclass)),
            ]))
        })
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("module_name"), binary(module_name)),
        (atom("classes"), Term::from(List::from(class_terms))),
        (atom("warnings"), Term::from(List::from(warning_terms))),
    ]))
}

/// Build a response map for a successful `diagnostics` query.
fn diagnostics_ok_response(diagnostics: &[DiagInfo]) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| {
            Term::from(Map::from([
                (atom("message"), binary(&d.message)),
                (atom("severity"), binary(&d.severity)),
                (
                    atom("start"),
                    Term::from(eetf::FixInteger::from(
                        i32::try_from(d.start).unwrap_or(i32::MAX),
                    )),
                ),
                (
                    atom("end"),
                    Term::from(eetf::FixInteger::from(
                        i32::try_from(d.end).unwrap_or(i32::MAX),
                    )),
                ),
            ]))
        })
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
    ]))
}

/// Build a response map for a compilation error.
fn error_response(diagnostics: &[String]) -> Term {
    let diag_terms: Vec<Term> = diagnostics.iter().map(|d| binary(d)).collect();
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
    ]))
}

/// Structured diagnostic info returned in compilation responses.
struct DiagInfo {
    /// Human-readable diagnostic message.
    message: String,
    /// Severity level (`"error"` or `"warning"`).
    severity: String,
    /// Byte offset where the diagnosed span begins.
    start: u32,
    /// Byte offset where the diagnosed span ends.
    end: u32,
}

/// Separate diagnostics into errors and warnings, returning structured info.
fn partition_diagnostics(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
) -> (Vec<DiagInfo>, Vec<DiagInfo>) {
    let errors = diagnostics
        .iter()
        .filter(|d| matches!(d.severity, beamtalk_core::source_analysis::Severity::Error))
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: "error".to_string(),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();
    let warnings = diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.severity,
                beamtalk_core::source_analysis::Severity::Warning
                    | beamtalk_core::source_analysis::Severity::Hint
            )
        })
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: match d.severity {
                beamtalk_core::source_analysis::Severity::Hint => "hint".to_string(),
                _ => "warning".to_string(),
            },
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();
    (errors, warnings)
}

/// Handle a single `compile_expression` request.
fn handle_compile_expression(request: &Map) -> Term {
    // Extract required fields
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let Some(module_name) = map_get(request, "module").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'module' field".to_string()]);
    };

    let known_vars = map_get(request, "known_vars")
        .and_then(term_to_string_list)
        .unwrap_or_default();

    // Parse the expression
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis with known REPL variables
    let known_var_refs: Vec<&str> = known_vars.iter().map(String::as_str).collect();
    let mut all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars(
            &module,
            parse_diagnostics,
            &known_var_refs,
        );

    // Run @primitive validation
    let options = beamtalk_core::CompilerOptions::default();
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    // Separate errors and warnings
    let errors: Vec<String> = all_diagnostics
        .iter()
        .filter(|d| matches!(d.severity, beamtalk_core::source_analysis::Severity::Error))
        .map(|d| d.message.to_string())
        .collect();

    let warnings: Vec<String> = all_diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.severity,
                beamtalk_core::source_analysis::Severity::Warning
                    | beamtalk_core::source_analysis::Severity::Hint
            )
        })
        .map(|d| d.message.to_string())
        .collect();

    if !errors.is_empty() {
        return error_response(&errors);
    }

    // BT-571: If the parsed module contains class definitions, use compile path
    if !module.classes.is_empty() {
        return handle_inline_class_definition(module, &source, &warnings);
    }

    // BT-571: If the parsed module contains standalone method definitions, return method info
    if !module.method_definitions.is_empty() {
        if module.method_definitions.len() > 1 {
            return error_response(&[
                "Multiple standalone method definitions in a single expression are not supported. \
                 Define each method separately, or use a class definition with inline methods."
                    .to_string(),
            ]);
        }
        let method_def = &module.method_definitions[0];
        let class_name = method_def.class_name.name.to_string();
        let selector = method_def.method.selector.name().to_string();
        return method_definition_ok_response(
            &class_name,
            &selector,
            method_def.is_class_method,
            &source,
            &warnings,
        );
    }

    if module.expressions.is_empty() {
        return error_response(&["No expressions to compile".to_string()]);
    }

    // BT-780: Generate Core Erlang for all expressions (multi-statement support)
    match beamtalk_core::erlang::generate_repl_expressions(&module.expressions, &module_name) {
        Ok(code) => ok_response(&code, &warnings),
        Err(e) => error_response(&[format!("Code generation failed: {e}")]),
    }
}

// ============================================================================
// BT-839: Dynamic ClassBuilder expression generation (Path 2)
// ============================================================================

/// Generate a Beamtalk `ClassBuilder` cascade expression for Path 2 dynamic class creation.
///
/// Transforms a `ClassDefinition` AST into an expression such as:
/// ```text
/// (Object classBuilder
///   name: #Foo;
///   addField: #x default: 0;
///   addMethod: #myMethod body: [:btSelf | (btSelf at: #x)];
///   register) await
/// ```
///
/// The REPL evaluates this expression instead of loading a compiled BEAM module,
/// creating the class via the `ClassBuilder` protocol (ADR 0038 Path 2).
///
/// Returns an empty string if the class cannot be safely expressed as Path 2
/// (e.g., it has field mutations in methods, which require the compiled protocol).
fn generate_dynamic_class_expr(class: &beamtalk_core::ast::ClassDefinition) -> String {
    // Fall back to Path 1 (compiled module) when the class uses features that
    // the ClassBuilder dynamic protocol doesn't support:
    // - Field mutations: dynamic closures cannot update gen_server state
    // - Class methods: `addMethod:body:` only handles instance methods
    // - Class variables: ClassBuilder has no `addClassState:` API
    // - Unsupported expressions: `super`, `@primitive`, `match:` in method bodies
    if class_has_field_mutations(class)
        || !class.class_methods.is_empty()
        || !class.class_variables.is_empty()
        || class
            .methods
            .iter()
            .any(|m| m.body.iter().any(expr_has_unsupported_for_path2))
    {
        return String::new();
    }

    let class_name = &class.name.name;
    let superclass = class.superclass_name();

    // Note: `classBuilder name:` must NOT be separated by `;` — `name:` is a keyword
    // message sent to the ClassBuilder (returned by `classBuilder`), not a cascade to
    // the superclass. The subsequent `;` cascades target the ClassBuilder.
    let mut parts: Vec<String> = vec![format!("({superclass} classBuilder name: #{class_name}")];

    for field in &class.state {
        let field_name = &field.name.name;
        let default = field
            .default_value
            .as_ref()
            .map_or_else(|| "nil".to_string(), print_expr_dynamic);
        parts.push(format!("addField: #{field_name} default: {default}"));
    }

    for method in &class.methods {
        let selector_str = format_selector_as_symbol(&method.selector);
        let block = generate_method_block(&method.parameters, &method.body);
        parts.push(format!("addMethod: {selector_str} body: {block}"));
    }

    parts.push("register) await".to_string());
    parts.join("; ")
}

/// Format a method selector as a Beamtalk symbol literal for `addMethod:body:`.
///
/// Unary `size` → `#size`, Binary `+` → `#'+'`, Keyword `at:put:` → `#'at:put:'`
fn format_selector_as_symbol(selector: &beamtalk_core::ast::MessageSelector) -> String {
    use beamtalk_core::ast::MessageSelector;
    let name = selector.name();
    match selector {
        MessageSelector::Unary(_) => format!("#{name}"),
        MessageSelector::Binary(_) | MessageSelector::Keyword(_) => format!("#'{name}'"),
    }
}

/// Generate a block expression `[:btSelf :p1 ... | body]` for a method body.
///
/// `btSelf` receives the instance state map in the dynamic dispatch protocol.
/// Additional block params correspond to the method's declared parameters.
fn generate_method_block(
    parameters: &[beamtalk_core::ast::ParameterDefinition],
    body: &[beamtalk_core::ast::Expression],
) -> String {
    let body_str = if body.is_empty() {
        "nil".to_string()
    } else if body.len() == 1 {
        print_expr_dynamic(&body[0])
    } else {
        body.iter()
            .map(print_expr_dynamic)
            .collect::<Vec<_>>()
            .join(". ")
    };

    let references_self = body.iter().any(expr_references_self);
    let has_params = !parameters.is_empty();

    if !references_self && !has_params {
        // Arity-0 constant block — no self or params needed
        format!("[{body_str}]")
    } else {
        let mut param_parts = vec!["btSelf".to_string()];
        for p in parameters {
            param_parts.push(p.name.name.to_string());
        }
        let param_str = param_parts
            .iter()
            .map(|p| format!(":{p}"))
            .collect::<Vec<_>>()
            .join(" ");
        format!("[{param_str} | {body_str}]")
    }
}

/// Returns `true` if the class has any field mutations (`self.field := value`) in its methods.
///
/// Classes with field mutations require the compiled `gen_server` dispatch protocol (Path 1)
/// because dynamic method closures cannot update instance state.
fn class_has_field_mutations(class: &beamtalk_core::ast::ClassDefinition) -> bool {
    class
        .methods
        .iter()
        .any(|m| m.body.iter().any(expr_has_field_mutation))
}

/// Returns `true` if the expression contains a field mutation (`self.field := value`).
fn expr_has_field_mutation(expr: &beamtalk_core::ast::Expression) -> bool {
    use beamtalk_core::ast::Expression;
    match expr {
        Expression::Assignment { target, value, .. } => {
            // Check if the target is a FieldAccess on self
            matches!(
                target.as_ref(),
                Expression::FieldAccess {
                    receiver,
                    ..
                } if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
            ) || expr_has_field_mutation(value)
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => expr_has_field_mutation(receiver) || arguments.iter().any(expr_has_field_mutation),
        Expression::Block(block) => block.body.iter().any(expr_has_field_mutation),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            expr_has_field_mutation(receiver)
                || messages
                    .iter()
                    .any(|m| m.arguments.iter().any(expr_has_field_mutation))
        }
        Expression::Parenthesized { expression, .. } => expr_has_field_mutation(expression),
        Expression::Return { value, .. } => expr_has_field_mutation(value),
        Expression::Match { value, arms, .. } => {
            expr_has_field_mutation(value)
                || arms.iter().any(|arm| expr_has_field_mutation(&arm.body))
        }
        Expression::ListLiteral { elements, tail, .. } => {
            elements.iter().any(expr_has_field_mutation)
                || tail.as_deref().is_some_and(expr_has_field_mutation)
        }
        Expression::ArrayLiteral { elements, .. } => elements.iter().any(expr_has_field_mutation),
        Expression::MapLiteral { pairs, .. } => pairs
            .iter()
            .any(|p| expr_has_field_mutation(&p.key) || expr_has_field_mutation(&p.value)),
        Expression::StringInterpolation { segments, .. } => {
            use beamtalk_core::ast::StringSegment;
            segments.iter().any(|seg| match seg {
                StringSegment::Interpolation(e) => expr_has_field_mutation(e),
                StringSegment::Literal(_) => false,
            })
        }
        Expression::FieldAccess { receiver, .. } => expr_has_field_mutation(receiver),
        _ => false,
    }
}

/// Returns `true` if an expression references `self` anywhere.
fn expr_references_self(expr: &beamtalk_core::ast::Expression) -> bool {
    use beamtalk_core::ast::Expression;
    match expr {
        Expression::Identifier(id) => id.name == "self",
        Expression::FieldAccess { receiver, .. } => expr_references_self(receiver),
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => expr_references_self(receiver) || arguments.iter().any(expr_references_self),
        Expression::Assignment { target, value, .. } => {
            expr_references_self(target) || expr_references_self(value)
        }
        Expression::Return { value, .. } => expr_references_self(value),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            expr_references_self(receiver)
                || messages
                    .iter()
                    .any(|m| m.arguments.iter().any(expr_references_self))
        }
        Expression::Parenthesized { expression, .. } => expr_references_self(expression),
        Expression::Block(block) => block.body.iter().any(expr_references_self),
        Expression::Match { value, arms, .. } => {
            expr_references_self(value) || arms.iter().any(|arm| expr_references_self(&arm.body))
        }
        Expression::ListLiteral { elements, tail, .. } => {
            elements.iter().any(expr_references_self)
                || tail.as_deref().is_some_and(expr_references_self)
        }
        Expression::ArrayLiteral { elements, .. } => elements.iter().any(expr_references_self),
        Expression::MapLiteral { pairs, .. } => pairs
            .iter()
            .any(|p| expr_references_self(&p.key) || expr_references_self(&p.value)),
        Expression::StringInterpolation { segments, .. } => {
            use beamtalk_core::ast::StringSegment;
            segments.iter().any(|seg| match seg {
                StringSegment::Interpolation(e) => expr_references_self(e),
                StringSegment::Literal(_) => false,
            })
        }
        _ => false,
    }
}

/// Returns `true` if the expression contains constructs that Path 2 (`ClassBuilder`)
/// cannot handle: `super` sends, `@primitive` annotations, and `match:` expressions.
fn expr_has_unsupported_for_path2(expr: &beamtalk_core::ast::Expression) -> bool {
    use beamtalk_core::ast::Expression;
    match expr {
        Expression::Super(_) | Expression::Primitive { .. } | Expression::Match { .. } => true,
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            expr_has_unsupported_for_path2(receiver)
                || arguments.iter().any(expr_has_unsupported_for_path2)
        }
        Expression::Assignment { target, value, .. } => {
            expr_has_unsupported_for_path2(target) || expr_has_unsupported_for_path2(value)
        }
        Expression::Return { value, .. } => expr_has_unsupported_for_path2(value),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            expr_has_unsupported_for_path2(receiver)
                || messages
                    .iter()
                    .any(|m| m.arguments.iter().any(expr_has_unsupported_for_path2))
        }
        Expression::Parenthesized { expression, .. } => expr_has_unsupported_for_path2(expression),
        Expression::Block(block) => block.body.iter().any(expr_has_unsupported_for_path2),
        Expression::FieldAccess { receiver, .. } => expr_has_unsupported_for_path2(receiver),
        Expression::ListLiteral { elements, tail, .. } => {
            elements.iter().any(expr_has_unsupported_for_path2)
                || tail.as_deref().is_some_and(expr_has_unsupported_for_path2)
        }
        Expression::ArrayLiteral { elements, .. } => {
            elements.iter().any(expr_has_unsupported_for_path2)
        }
        Expression::MapLiteral { pairs, .. } => pairs.iter().any(|p| {
            expr_has_unsupported_for_path2(&p.key) || expr_has_unsupported_for_path2(&p.value)
        }),
        Expression::StringInterpolation { segments, .. } => {
            use beamtalk_core::ast::StringSegment;
            segments.iter().any(|seg| match seg {
                StringSegment::Interpolation(e) => expr_has_unsupported_for_path2(e),
                StringSegment::Literal(_) => false,
            })
        }
        _ => false,
    }
}

/// Print a Beamtalk expression for use in a dynamic class method body.
///
/// Key transformations:
/// - `self` (bare identifier) → `btSelf`
/// - `self.field` (field access on self) → `(btSelf at: #field)`
/// - `receiver.field` → `(receiver at: #field)` for any other receiver
#[allow(clippy::too_many_lines)]
fn print_expr_dynamic(expr: &beamtalk_core::ast::Expression) -> String {
    use beamtalk_core::ast::{Expression, MessageSelector, StringSegment};
    match expr {
        Expression::Literal(lit, _) => print_literal_dynamic(lit),

        Expression::Identifier(id) => {
            if id.name == "self" {
                "btSelf".to_string()
            } else {
                id.name.to_string()
            }
        }

        Expression::ClassReference { name, .. } => name.name.to_string(),

        Expression::Super(_) => "super".to_string(),

        Expression::FieldAccess {
            receiver, field, ..
        } => {
            let recv = print_expr_dynamic(receiver);
            format!("({recv} at: #{})", field.name)
        }

        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => match selector {
            MessageSelector::Unary(name) => {
                format!("({} {name})", print_expr_dynamic(receiver))
            }
            MessageSelector::Binary(op) => {
                format!(
                    "({} {op} {})",
                    print_expr_dynamic(receiver),
                    print_expr_dynamic(&arguments[0])
                )
            }
            MessageSelector::Keyword(parts) => {
                let mut result = format!("({}", print_expr_dynamic(receiver));
                for (part, arg) in parts.iter().zip(arguments.iter()) {
                    result.push(' ');
                    result.push_str(&part.keyword);
                    result.push(' ');
                    result.push_str(&print_expr_dynamic(arg));
                }
                result.push(')');
                result
            }
        },

        Expression::Block(block) => {
            let params = block
                .parameters
                .iter()
                .map(|p| format!(":{}", p.name))
                .collect::<Vec<_>>()
                .join(" ");
            let body = block
                .body
                .iter()
                .map(print_expr_dynamic)
                .collect::<Vec<_>>()
                .join(". ");
            if block.parameters.is_empty() {
                format!("[{body}]")
            } else {
                format!("[{params} | {body}]")
            }
        }

        Expression::Assignment { target, value, .. } => {
            format!(
                "{} := {}",
                print_expr_dynamic(target),
                print_expr_dynamic(value)
            )
        }

        Expression::Return { value, .. } => {
            format!("^{}", print_expr_dynamic(value))
        }

        Expression::Cascade {
            receiver, messages, ..
        } => {
            let recv = print_expr_dynamic(receiver);
            let msg_strs: Vec<String> = messages
                .iter()
                .map(|m| match &m.selector {
                    MessageSelector::Unary(name) => name.to_string(),
                    MessageSelector::Binary(op) => {
                        format!("{op} {}", print_expr_dynamic(&m.arguments[0]))
                    }
                    MessageSelector::Keyword(parts) => {
                        let mut s = String::new();
                        for (part, arg) in parts.iter().zip(m.arguments.iter()) {
                            if !s.is_empty() {
                                s.push(' ');
                            }
                            s.push_str(&part.keyword);
                            s.push(' ');
                            s.push_str(&print_expr_dynamic(arg));
                        }
                        s
                    }
                })
                .collect();
            format!("({recv} {})", msg_strs.join("; "))
        }

        Expression::Parenthesized { expression, .. } => {
            format!("({})", print_expr_dynamic(expression))
        }

        Expression::MapLiteral { pairs, .. } => {
            if pairs.is_empty() {
                "#{}".to_string()
            } else {
                let inner = pairs
                    .iter()
                    .map(|p| {
                        format!(
                            "{} => {}",
                            print_expr_dynamic(&p.key),
                            print_expr_dynamic(&p.value)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("#{{{inner}}}")
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            let elems = elements
                .iter()
                .map(print_expr_dynamic)
                .collect::<Vec<_>>()
                .join(", ");
            if let Some(t) = tail {
                format!("#({elems} | {})", print_expr_dynamic(t))
            } else {
                format!("#({elems})")
            }
        }

        Expression::ArrayLiteral { elements, .. } => {
            let elems = elements
                .iter()
                .map(print_expr_dynamic)
                .collect::<Vec<_>>()
                .join(", ");
            format!("#[{elems}]")
        }

        Expression::StringInterpolation { segments, .. } => {
            let inner = segments
                .iter()
                .map(|seg| match seg {
                    StringSegment::Literal(s) => s.to_string(),
                    StringSegment::Interpolation(e) => {
                        format!("{{{}}}", print_expr_dynamic(e))
                    }
                })
                .collect::<String>();
            format!("\"{}\"", inner.replace('"', "\"\""))
        }

        Expression::Match { value, arms, .. } => {
            // Emit a simplified match — just preserve the structure
            let val = print_expr_dynamic(value);
            let arms_str = arms
                .iter()
                .map(|arm| format!("... -> {}", print_expr_dynamic(&arm.body)))
                .collect::<Vec<_>>()
                .join("; ");
            format!("({val} match: [{arms_str}])")
        }

        Expression::Primitive {
            name, is_quoted, ..
        } => {
            if *is_quoted {
                format!("@primitive '{name}'")
            } else {
                format!("@primitive {name}")
            }
        }

        Expression::ExpectDirective { .. } => String::new(),

        Expression::Error { message, .. } => {
            format!("/* error: {message} */")
        }
    }
}

/// Print a literal value as Beamtalk source.
fn print_literal_dynamic(lit: &beamtalk_core::ast::Literal) -> String {
    use beamtalk_core::ast::Literal;
    match lit {
        Literal::Integer(n) => n.to_string(),
        Literal::Float(f) => format!("{f}"),
        Literal::String(s) => {
            // Beamtalk strings use double quotes; escape embedded double quotes as ""
            format!("\"{}\"", s.replace("\"", "\"\""))
        }
        Literal::Symbol(sym) => {
            if sym
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == ':')
            {
                if sym.contains(':') {
                    format!("#'{sym}'")
                } else {
                    format!("#{sym}")
                }
            } else {
                format!("#'{sym}'")
            }
        }
        Literal::List(elems) => {
            let inner = elems
                .iter()
                .map(print_literal_dynamic)
                .collect::<Vec<_>>()
                .join(", ");
            format!("#({inner})")
        }
        Literal::Character(c) => format!("${c}"),
    }
}

/// BT-571: Handle inline class definition in REPL expression context.
/// Merges any standalone method definitions into the class, generates code,
/// and returns a `class_definition` response.
fn handle_inline_class_definition(
    module: beamtalk_core::ast::Module,
    source: &str,
    warnings: &[String],
) -> Term {
    let mut module = module;
    let mut warnings = warnings.to_vec();
    if !module.method_definitions.is_empty() {
        let method_defs = std::mem::take(&mut module.method_definitions);
        for method_def in method_defs {
            let target_class = method_def.class_name.name.as_str();
            if let Some(class) = module
                .classes
                .iter_mut()
                .find(|c| c.name.name == target_class)
            {
                let methods = if method_def.is_class_method {
                    &mut class.class_methods
                } else {
                    &mut class.methods
                };
                merge_method(methods, method_def.method);
            } else {
                warnings.push(format!(
                    "Standalone method targets unknown class `{target_class}` in this module"
                ));
            }
        }
    }

    let base_name = beamtalk_core::erlang::to_module_name(&module.classes[0].name.name);
    let class_module_name = format!("bt@{base_name}");

    let classes: Vec<(String, String)> = module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect();

    // BT-839: Generate a dynamic ClassBuilder expression for Path 2 evaluation.
    // This is returned alongside the compiled Core Erlang so the REPL can choose
    // to evaluate via ClassBuilder (no module compilation) instead of loading the binary.
    // Only attempt Path 2 for single-class modules; multi-class input falls back to Path 1.
    let dynamic_class_expr = if module.classes.len() == 1 {
        generate_dynamic_class_expr(&module.classes[0])
    } else {
        String::new()
    };

    match beamtalk_core::erlang::generate_module(
        &module,
        beamtalk_core::erlang::CodegenOptions::new(&class_module_name)
            .with_workspace_mode(true)
            .with_source(source),
    ) {
        Ok(code) => class_definition_ok_response(
            &code,
            &class_module_name,
            &classes,
            &warnings,
            &dynamic_class_expr,
        ),
        Err(e) => error_response(&[format!("Code generation failed: {e}")]),
    }
}

/// Handle a `compile` request (file/class compilation).
#[allow(clippy::too_many_lines)]
fn handle_compile(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let stdlib_mode = map_get(request, "stdlib_mode")
        .and_then(term_to_bool)
        .unwrap_or(false);

    let workspace_mode = map_get(request, "workspace_mode")
        .and_then(term_to_bool)
        .unwrap_or(true);

    // Parse the source
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis
    let mut all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars(
            &module,
            parse_diagnostics,
            &[],
        );

    // Run @primitive validation
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode,
        allow_primitives: false,
        workspace_mode,
        suppress_warnings: false,
    };
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    // BT-738: Warn when user code shadows a stdlib class name (not for stdlib itself).
    if !stdlib_mode {
        let mut stdlib_shadow_diags = Vec::new();
        beamtalk_core::semantic_analysis::check_stdlib_name_shadowing(
            &module,
            &mut stdlib_shadow_diags,
        );
        all_diagnostics.extend(stdlib_shadow_diags);
    }

    let (errors, mut warnings) = partition_diagnostics(&all_diagnostics);

    if !errors.is_empty() {
        let error_msgs: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
        return error_response(&error_msgs);
    }

    // BT-571: Merge standalone method definitions into their target classes
    let mut module = module;
    if !module.method_definitions.is_empty() {
        let method_defs = std::mem::take(&mut module.method_definitions);
        for method_def in method_defs {
            let target_class = method_def.class_name.name.as_str();
            if let Some(class) = module
                .classes
                .iter_mut()
                .find(|c| c.name.name == target_class)
            {
                let methods = if method_def.is_class_method {
                    &mut class.class_methods
                } else {
                    &mut class.methods
                };
                merge_method(methods, method_def.method);
            } else {
                warnings.push(DiagInfo {
                    message: format!(
                        "Standalone method targets unknown class `{target_class}` in this module"
                    ),
                    severity: "warning".to_string(),
                    start: method_def.span.start(),
                    end: method_def.span.end(),
                });
            }
        }
    }

    // BT-775: Accept optional module_name override from caller.
    // When provided, use it directly instead of deriving from the class name.
    // This allows the REPL/MCP load path to produce package-qualified names
    // matching the build system (e.g., bt@my_app@scheme@symbol).
    let module_name_override = map_get(request, "module_name").and_then(term_to_string);

    let module_name = if let Some(override_name) = module_name_override {
        override_name
    } else {
        // Derive module name from first class in AST (ADR 0016)
        let base_name = if let Some(first_class) = module.classes.first() {
            beamtalk_core::erlang::to_module_name(&first_class.name.name)
        } else {
            // No class definition — use a fallback
            return error_response(&["No class definition found in source".to_string()]);
        };

        if stdlib_mode {
            format!("bt@stdlib@{base_name}")
        } else {
            format!("bt@{base_name}")
        }
    };

    // Extract class info
    let classes: Vec<(String, String)> = module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect();

    // Extract optional class module index for resolving subdirectory class references.
    let class_module_index = match map_get(request, "class_module_index") {
        None => std::collections::HashMap::new(),
        Some(term) => match term_to_string_map(term) {
            Ok(map) => map,
            Err(e) => return error_response(&[e]),
        },
    };

    // Generate Core Erlang
    let warning_msgs: Vec<String> = warnings.iter().map(|w| w.message.clone()).collect();
    match beamtalk_core::erlang::generate_module(
        &module,
        beamtalk_core::erlang::CodegenOptions::new(&module_name)
            .with_workspace_mode(workspace_mode)
            .with_source(&source)
            .with_class_module_index(class_module_index),
    ) {
        Ok(code) => compile_ok_response(&code, &module_name, &classes, &warning_msgs),
        Err(e) => error_response(&[format!("Code generation failed: {e}")]),
    }
}

/// Handle a `diagnostics` request (syntax/semantic check only).
fn handle_diagnostics(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    // Parse the source
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis
    let all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars(
            &module,
            parse_diagnostics,
            &[],
        );

    let all_diags: Vec<DiagInfo> = all_diagnostics
        .iter()
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: match d.severity {
                beamtalk_core::source_analysis::Severity::Error => "error".to_string(),
                beamtalk_core::source_analysis::Severity::Warning => "warning".to_string(),
                beamtalk_core::source_analysis::Severity::Hint => "hint".to_string(),
            },
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();

    diagnostics_ok_response(&all_diags)
}

/// Handle a `version` request.
fn handle_version() -> Term {
    let version = env!("CARGO_PKG_VERSION");
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("version"), binary(version)),
    ]))
}

/// Handle a single request and return a response Term.
fn handle_request(request_term: &Term) -> Term {
    let Term::Map(map) = request_term else {
        return error_response(&["Request must be a map".to_string()]);
    };

    // Extract command atom
    let command = match map_get(map, "command") {
        Some(Term::Atom(a)) => a.name.as_str(),
        _ => return error_response(&["Missing or invalid 'command' field".to_string()]),
    };

    match command {
        "compile_expression" => handle_compile_expression(map),
        "compile" => handle_compile(map),
        "diagnostics" => handle_diagnostics(map),
        "version" => handle_version(),
        _ => error_response(&[format!("Unknown command: {command}")]),
    }
}

#[derive(Debug, Parser)]
#[command(name = "beamtalk-compiler-port", about = "Beamtalk compiler port")]
struct Cli {
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,
}

fn main() {
    let cli = Cli::parse();

    // Only initialize tracing when explicitly requested.
    // The compiler port is spawned by the Erlang runtime without args, so
    // default (verbose=0) must produce no stderr output to avoid interfering
    // with the OTP port protocol.
    let has_rust_log = std::env::var("RUST_LOG").is_ok();
    if has_rust_log || cli.verbose > 0 {
        let env_filter = if has_rust_log {
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("warn"))
        } else {
            EnvFilter::new(directive_for_verbosity(cli.verbose))
        };
        let _ = tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .with_writer(std::io::stderr)
            .with_ansi(false)
            .try_init();
    }

    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();

    loop {
        // Read next request
        let packet = match read_packet(&mut stdin) {
            Ok(Some(data)) => data,
            Ok(None) => break, // EOF — port closed
            Err(e) => {
                eprintln!("Failed to read packet: {e}");
                break;
            }
        };

        // Decode ETF
        let term = match Term::decode(io::Cursor::new(&packet)) {
            Ok(t) => t,
            Err(e) => {
                // Send error response for decode failures
                let response = error_response(&[format!("ETF decode error: {e}")]);
                let mut buf = Vec::new();
                if response.encode(&mut buf).is_ok() {
                    let _ = write_packet(&mut stdout, &buf);
                }
                continue;
            }
        };

        // Handle the request
        let response = handle_request(&term);

        // Encode and send response
        let mut buf = Vec::new();
        match response.encode(&mut buf) {
            Ok(()) => {
                if let Err(e) = write_packet(&mut stdout, &buf) {
                    eprintln!("Failed to write response: {e}");
                    break;
                }
            }
            Err(e) => {
                eprintln!("Failed to encode response: {e}");
                break;
            }
        }
    }
}

fn directive_for_verbosity(v: u8) -> &'static str {
    // Target must match Rust module paths (`beamtalk_compiler_port`, `beamtalk_core`).
    // `beamtalk=…` only matches `beamtalk::*`, not `beamtalk_compiler_port`.
    match v {
        0 => "beamtalk_compiler_port=info,beamtalk_core=info",
        1 => "beamtalk_compiler_port=debug,beamtalk_core=debug",
        _ => "beamtalk_compiler_port=trace,beamtalk_core=trace",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn directive_defaults() {
        assert_eq!(
            directive_for_verbosity(0),
            "beamtalk_compiler_port=info,beamtalk_core=info"
        );
        assert_eq!(
            directive_for_verbosity(1),
            "beamtalk_compiler_port=debug,beamtalk_core=debug"
        );
        assert_eq!(
            directive_for_verbosity(2),
            "beamtalk_compiler_port=trace,beamtalk_core=trace"
        );
    }
}

// ============================================================================
// Property-based tests (ADR 0011 Phase 4)
// ============================================================================

#[cfg(test)]
mod property_tests {
    //! Property-based tests for the compiler port compile round-trip.
    //!
    //! These tests use `proptest` to verify that `handle_compile()` and
    //! `handle_compile_expression()` never panic on arbitrary input and
    //! always return well-formed ETF responses.
    //!
    //! **DDD Context:** Compilation (Anti-Corruption Layer)
    //!
    //! ADR 0011 Phase 4.

    use super::*;
    use proptest::prelude::*;

    /// Build an ETF Map for a `compile` request with the given source.
    fn compile_request(source: &str) -> Map {
        Map::from([
            (atom("command"), atom("compile")),
            (atom("source"), binary(source)),
        ])
    }

    /// Build an ETF Map for a `compile_expression` request with the given source.
    fn compile_expression_request(source: &str) -> Map {
        Map::from([
            (atom("command"), atom("compile_expression")),
            (atom("source"), binary(source)),
            (atom("module"), binary("bt@test_module")),
        ])
    }

    /// Extract the status atom from a response Term.
    fn response_status(term: &Term) -> Option<String> {
        if let Term::Map(map) = term {
            map_get(map, "status").and_then(|t| {
                if let Term::Atom(a) = t {
                    Some(a.name.clone())
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    /// Extract the diagnostics list from a response Term.
    fn response_diagnostics(term: &Term) -> Option<&List> {
        if let Term::Map(map) = term {
            if let Some(Term::List(list)) = map_get(map, "diagnostics") {
                return Some(list);
            }
        }
        None
    }

    /// Near-valid Beamtalk source fragments (reuses patterns from parser property tests).
    const FRAGMENTS: &[&str] = &[
        "42",
        "3.14",
        "\"hello\"",
        "true",
        "false",
        "nil",
        "x := 42",
        "x + y",
        "arr at: 1",
        "[:x | x + 1]",
        "(3 + 4)",
        "^42",
        "self",
        "#(1, 2, 3)",
        "#{a => 1}",
        "Object subclass: Counter\n  state: count = 0\n  increment => count := count + 1",
        "3 timesRepeat: [x := x + 1]",
    ];

    /// Generates a near-valid Beamtalk input using one of several mutation strategies.
    fn near_valid_beamtalk() -> impl Strategy<Value = String> {
        prop_oneof![
            // Valid fragments
            prop::sample::select(FRAGMENTS).prop_map(std::string::ToString::to_string),
            // Truncated valid expressions
            prop::sample::select(FRAGMENTS).prop_flat_map(|s| {
                let len = s.len();
                if len <= 1 {
                    Just(s.to_string()).boxed()
                } else {
                    (1..len)
                        .prop_map(move |cut| {
                            let safe_cut = s.floor_char_boundary(cut);
                            if safe_cut == 0 {
                                s.to_string()
                            } else {
                                s[..safe_cut].to_string()
                            }
                        })
                        .boxed()
                }
            }),
            // Mismatched brackets
            prop::sample::select(FRAGMENTS).prop_map(|s| {
                s.chars()
                    .map(|ch| match ch {
                        '[' => '(',
                        ']' => '}',
                        '(' => '[',
                        _ => ch,
                    })
                    .collect()
            }),
        ]
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            cases: 512,
            .. ProptestConfig::default()
        })]

        /// Property 1a: `handle_compile` never panics on arbitrary string input.
        #[test]
        fn compile_never_panics(input in "\\PC{0,500}") {
            let request = compile_request(&input);
            let response = handle_compile(&request);
            let status = response_status(&response);
            prop_assert!(
                status.is_some(),
                "Response must have a status field for input: {:?}",
                input,
            );
            let status = status.unwrap();
            prop_assert!(
                status == "ok" || status == "error",
                "Status must be 'ok' or 'error', got {:?} for input: {:?}",
                status,
                input,
            );
        }

        /// Property 1b: `handle_compile_expression` never panics on arbitrary string input.
        #[test]
        fn compile_expression_never_panics(input in "\\PC{0,500}") {
            let request = compile_expression_request(&input);
            let response = handle_compile_expression(&request);
            let status = response_status(&response);
            prop_assert!(
                status.is_some(),
                "Response must have a status field for input: {:?}",
                input,
            );
            let status = status.unwrap();
            prop_assert!(
                status == "ok" || status == "error",
                "Status must be 'ok' or 'error', got {:?} for input: {:?}",
                status,
                input,
            );
        }

        /// Property 1c: `handle_compile` never panics on near-valid structured input.
        #[test]
        fn compile_never_panics_near_valid(input in near_valid_beamtalk()) {
            let request = compile_request(&input);
            let response = handle_compile(&request);
            let status = response_status(&response);
            prop_assert!(status.is_some());
        }

        /// Property 1d: `handle_compile_expression` never panics on near-valid structured input.
        #[test]
        fn compile_expression_never_panics_near_valid(input in near_valid_beamtalk()) {
            let request = compile_expression_request(&input);
            let response = handle_compile_expression(&request);
            let status = response_status(&response);
            prop_assert!(status.is_some());
        }

        /// Property 2: Error responses have non-empty diagnostics.
        ///
        /// When status is "error", the diagnostics list must be non-empty.
        #[test]
        fn error_responses_have_diagnostics(input in "\\PC{0,500}") {
            // Test both compile paths
            for response in [
                handle_compile(&compile_request(&input)),
                handle_compile_expression(&compile_expression_request(&input)),
            ] {
                if response_status(&response).as_deref() == Some("error") {
                    let diags = response_diagnostics(&response);
                    prop_assert!(
                        diags.is_some(),
                        "Error response must have 'diagnostics' field for input: {:?}",
                        input,
                    );
                    prop_assert!(
                        !diags.unwrap().elements.is_empty(),
                        "Error diagnostics must be non-empty for input: {:?}",
                        input,
                    );
                }
            }
        }

        /// Property 3: Diagnostic entries are non-empty valid UTF-8 strings.
        ///
        /// Every diagnostic in an error response must be a non-empty binary
        /// that decodes to valid UTF-8.
        #[test]
        fn diagnostics_are_nonempty_strings(input in "\\PC{0,500}") {
            for response in [
                handle_compile(&compile_request(&input)),
                handle_compile_expression(&compile_expression_request(&input)),
            ] {
                if response_status(&response).as_deref() == Some("error") {
                    if let Some(diags) = response_diagnostics(&response) {
                        for (i, diag_term) in diags.elements.iter().enumerate() {
                            if let Term::Binary(b) = diag_term {
                                let text = String::from_utf8(b.bytes.clone());
                                prop_assert!(
                                    text.is_ok(),
                                    "Diagnostic {} is not valid UTF-8 for input: {:?}",
                                    i,
                                    input,
                                );
                                prop_assert!(
                                    !text.unwrap().is_empty(),
                                    "Diagnostic {} is empty for input: {:?}",
                                    i,
                                    input,
                                );
                            } else {
                                prop_assert!(
                                    false,
                                    "Diagnostic {} is not a Binary term for input: {:?}",
                                    i,
                                    input,
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}
