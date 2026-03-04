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

/// Extract an atom name string from a Term.
fn term_to_atom(term: &Term) -> Option<String> {
    match term {
        Term::Atom(a) => Some(a.name.clone()),
        _ => None,
    }
}

/// Extract an unsigned integer from a Term (`FixInteger` only).
fn term_to_usize(term: &Term) -> Option<usize> {
    match term {
        Term::FixInteger(n) => usize::try_from(n.value).ok(),
        _ => None,
    }
}

/// Extract a list of atom strings from a Term.
fn term_to_atom_list(term: &Term) -> Vec<ecow::EcoString> {
    match term {
        Term::List(list) => list
            .elements
            .iter()
            .filter_map(term_to_atom)
            .map(|s| ecow::EcoString::from(s.as_str()))
            .collect(),
        _ => vec![],
    }
}

/// Extract an atom→atom map from a Term (for `field_types`).
fn term_to_atom_atom_map(
    term: &Term,
) -> std::collections::HashMap<ecow::EcoString, ecow::EcoString> {
    match term {
        Term::Map(m) => m
            .map
            .iter()
            .filter_map(|(k, v)| {
                let key = term_to_atom(k)?;
                let val = term_to_atom(v)?;
                Some((
                    ecow::EcoString::from(key.as_str()),
                    ecow::EcoString::from(val.as_str()),
                ))
            })
            .collect(),
        _ => std::collections::HashMap::new(),
    }
}

/// Parse method infos from a `method_info` or `class_method_info` ETF map.
///
/// Each entry: `selector_atom => #{arity => int, param_types => [atom...], return_type => atom}`.
/// Returns empty Vec on missing key or malformed data (graceful degradation).
fn parse_method_infos_from_map(
    m: &Map,
    key: &str,
    class_name: &str,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::MethodInfo> {
    use beamtalk_core::ast::MethodKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::MethodInfo;

    let Some(Term::Map(method_map)) = map_get(m, key) else {
        return vec![];
    };
    method_map
        .map
        .iter()
        .filter_map(|(sel_term, info_term)| {
            let selector = term_to_atom(sel_term)?;
            let Term::Map(info_map) = info_term else {
                return None;
            };
            let arity = map_get(info_map, "arity")
                .and_then(term_to_usize)
                .unwrap_or(0);
            let return_type = map_get(info_map, "return_type")
                .and_then(term_to_atom)
                .and_then(|s| {
                    if s == "none" {
                        None
                    } else {
                        Some(ecow::EcoString::from(s.as_str()))
                    }
                });
            let param_types: Vec<Option<ecow::EcoString>> = match map_get(info_map, "param_types") {
                Some(Term::List(list)) => list
                    .elements
                    .iter()
                    .map(|t| {
                        term_to_atom(t).and_then(|s| {
                            if s == "none" {
                                None
                            } else {
                                Some(ecow::EcoString::from(s.as_str()))
                            }
                        })
                    })
                    .collect(),
                _ => vec![],
            };
            Some(MethodInfo {
                selector: ecow::EcoString::from(selector.as_str()),
                arity,
                kind: MethodKind::Primary,
                defined_in: ecow::EcoString::from(class_name),
                is_sealed: false,
                return_type,
                param_types,
            })
        })
        .collect()
}

/// Deserialize a single `__beamtalk_meta/0` ETF map into a `ClassInfo`.
///
/// Returns `None` if `term` is not a map. Degrades gracefully on missing keys
/// (old-format modules without `method_info` etc.).
fn parse_class_info_from_meta_term(
    class_name: &str,
    term: &Term,
) -> Option<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;

    let Term::Map(m) = term else { return None };

    let superclass = map_get(m, "superclass")
        .and_then(term_to_atom)
        .and_then(|s| {
            if s == "none" {
                None
            } else {
                Some(ecow::EcoString::from(s.as_str()))
            }
        });

    let is_sealed = map_get(m, "is_sealed")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_abstract = map_get(m, "is_abstract")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_value = map_get(m, "is_value")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_typed = map_get(m, "is_typed")
        .and_then(term_to_bool)
        .unwrap_or(false);

    let state = map_get(m, "fields")
        .map(term_to_atom_list)
        .unwrap_or_default();
    let state_types = map_get(m, "field_types")
        .map(term_to_atom_atom_map)
        .unwrap_or_default();
    let class_variables = map_get(m, "class_variables")
        .map(term_to_atom_list)
        .unwrap_or_default();

    let methods = parse_method_infos_from_map(m, "method_info", class_name);
    let class_methods = parse_method_infos_from_map(m, "class_method_info", class_name);

    Some(ClassInfo {
        name: ecow::EcoString::from(class_name),
        superclass,
        is_sealed,
        is_abstract,
        is_value,
        is_typed,
        state,
        state_types,
        methods,
        class_methods,
        class_variables,
    })
}

/// Parse a `class_hierarchy` ETF term (`#{atom() => meta_map()}`) into `Vec<ClassInfo>`.
///
/// Skips stdlib builtins — the Rust `ClassHierarchy::with_builtins()` already has
/// richer data for them. Degrades gracefully on malformed entries (silently skipped).
fn parse_class_hierarchy_from_term(
    term: &Term,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    let Term::Map(m) = term else { return vec![] };
    m.map
        .iter()
        .filter_map(|(name_term, meta_term)| {
            let class_name = term_to_atom(name_term)?;
            if beamtalk_core::semantic_analysis::ClassHierarchy::is_builtin_class(&class_name) {
                return None;
            }
            parse_class_info_from_meta_term(&class_name, meta_term)
        })
        .collect()
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
/// BT-885: `trailing_core_erlang` is Some when trailing expressions follow the class body.
fn class_definition_ok_response(
    core_erlang: &str,
    module_name: &str,
    classes: &[(String, String)],
    trailing_core_erlang: Option<&str>,
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
    let mut map: std::collections::HashMap<Term, Term> = std::collections::HashMap::from([
        (atom("status"), atom("ok")),
        (atom("kind"), atom("class_definition")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("module_name"), binary(module_name)),
        (atom("classes"), Term::from(List::from(class_terms))),
        (atom("warnings"), Term::from(List::from(warning_terms))),
    ]);
    if let Some(trailing) = trailing_core_erlang {
        map.insert(atom("trailing_core_erlang"), binary(trailing));
    }
    Term::from(Map::from(map))
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
#[expect(
    clippy::too_many_lines,
    reason = "expression compilation handles class defs, method defs, and plain expressions with indexes"
)]
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

    // BT-907: Extract optional class superclass index for cross-file value-object inheritance.
    // When an inline class definition inherits from a class loaded via a file, the compiler
    // needs the index to determine whether the parent is a value object or an actor.
    let class_superclass_index = match map_get(request, "class_superclass_index") {
        None => std::collections::HashMap::new(),
        Some(term) => match term_to_string_map(term) {
            Ok(map) => map,
            Err(e) => return error_response(&[e]),
        },
    };

    // Extract optional class module index for resolving package-qualified class references.
    // Without this, `Counter spawn` in a workspace with package "getting_started" generates
    // `call 'bt@counter':'spawn'()` instead of `call 'bt@getting_started@counter':'spawn'()`.
    let class_module_index = match map_get(request, "class_module_index") {
        None => std::collections::HashMap::new(),
        Some(term) => match term_to_string_map(term) {
            Ok(map) => map,
            Err(e) => return error_response(&[e]),
        },
    };

    // ADR 0050 Phase 4: Extract optional class_hierarchy for REPL session user classes.
    let pre_class_hierarchy = match map_get(request, "class_hierarchy") {
        None => vec![],
        Some(term) => parse_class_hierarchy_from_term(term),
    };

    // Parse the expression
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis with known REPL variables
    let known_var_refs: Vec<&str> = known_vars.iter().map(String::as_str).collect();
    let mut all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars_and_classes(
            &module,
            parse_diagnostics,
            &known_var_refs,
            pre_class_hierarchy.clone(),
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
                    // BT-979: Include lint warnings so REPL users see effect-free statement hints.
                    | beamtalk_core::source_analysis::Severity::Lint
            )
        })
        .map(|d| d.message.to_string())
        .collect();

    if !errors.is_empty() {
        return error_response(&errors);
    }

    // BT-571: If the parsed module contains class definitions, use compile path
    if !module.classes.is_empty() {
        return handle_inline_class_definition(
            module,
            &source,
            &module_name,
            &warnings,
            &class_superclass_index,
            class_module_index,
            pre_class_hierarchy,
        );
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
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    match beamtalk_core::erlang::generate_repl_expressions_with_index(
        &expressions,
        &module_name,
        class_module_index,
    ) {
        Ok(code) => ok_response(&code, &warnings),
        Err(e) => error_response(&[format!("Code generation failed: {e}")]),
    }
}

/// BT-571: Handle inline class definition in REPL expression context.
/// Merges any standalone method definitions into the class, generates code,
/// and returns a `class_definition` response.
/// BT-885: Also compiles any trailing expressions and includes them in the response.
/// BT-907: Accepts `class_superclass_index` to resolve cross-file inheritance chains.
/// Accepts `class_module_index` for package-qualified class references in trailing
/// expressions and the class body itself.
fn handle_inline_class_definition(
    module: beamtalk_core::ast::Module,
    source: &str,
    expr_module_name: &str,
    warnings: &[String],
    class_superclass_index: &std::collections::HashMap<String, String>,
    class_module_index: std::collections::HashMap<String, String>,
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
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

    // BT-885: Compile trailing expressions (after class body) so the Erlang side
    // can evaluate them and return their result instead of the class name.
    let trailing_core_erlang = if module.expressions.is_empty() {
        None
    } else {
        let trailing_exprs: Vec<_> = module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect();
        match beamtalk_core::erlang::generate_repl_expressions_with_index(
            &trailing_exprs,
            expr_module_name,
            class_module_index.clone(),
        ) {
            Ok(code) => Some(code),
            Err(e) => {
                return error_response(&[format!(
                    "Trailing expression code generation failed: {e}"
                )]);
            }
        }
    };

    match beamtalk_core::erlang::generate_module(
        &module,
        beamtalk_core::erlang::CodegenOptions::new(&class_module_name)
            .with_workspace_mode(true)
            .with_source(source)
            .with_class_superclass_index(class_superclass_index.clone())
            .with_class_module_index(class_module_index)
            .with_class_hierarchy(pre_class_hierarchy),
    ) {
        Ok(code) => class_definition_ok_response(
            &code,
            &class_module_name,
            &classes,
            trailing_core_erlang.as_deref(),
            &warnings,
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

    // ADR 0050 Phase 4: Extract optional class_hierarchy for REPL session user classes.
    let pre_class_hierarchy = match map_get(request, "class_hierarchy") {
        None => vec![],
        Some(term) => parse_class_hierarchy_from_term(term),
    };

    // Parse the source
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis
    let mut all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars_and_classes(
            &module,
            parse_diagnostics,
            &[],
            pre_class_hierarchy.clone(),
        );

    // Run @primitive validation
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode,
        allow_primitives: false,
        workspace_mode,
        suppress_warnings: false,
        ..Default::default()
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

    // BT-905: Extract optional class superclass index for resolving cross-file
    // value-object inheritance. Without this, a child class whose parent is defined
    // in another file defaults to Actor codegen.
    let class_superclass_index = match map_get(request, "class_superclass_index") {
        None => std::collections::HashMap::new(),
        Some(term) => match term_to_string_map(term) {
            Ok(map) => map,
            Err(e) => return error_response(&[e]),
        },
    };

    // BT-845/BT-860: Extract optional source file path to embed as beamtalk_source attribute.
    let source_path = map_get(request, "source_path").and_then(term_to_string);

    // Generate Core Erlang
    let warning_msgs: Vec<String> = warnings.iter().map(|w| w.message.clone()).collect();
    match beamtalk_core::erlang::generate_module(
        &module,
        beamtalk_core::erlang::CodegenOptions::new(&module_name)
            .with_workspace_mode(workspace_mode)
            .with_source(&source)
            .with_class_module_index(class_module_index)
            .with_class_superclass_index(class_superclass_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_source_path_opt(source_path.as_deref()),
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
                beamtalk_core::source_analysis::Severity::Lint => "lint".to_string(),
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

    /// BT-907: Inline class definition with cross-file superclass index must compile
    /// as a value type, not an Actor, when the parent's chain resolves to Object.
    #[test]
    fn inline_class_definition_with_superclass_index_compiles_as_value_type() {
        // Build a compile_expression request for `Shape subclass: Triangle`
        // where Shape's superclass (Object) is provided via class_superclass_index.
        let superclass_index_map = Map::from([(binary("Shape"), binary("Object"))]);
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary(
                    "Shape subclass: Triangle\n  state: base = 1.0\n  class withBase: b => self new: #{base => b}",
                ),
            ),
            (atom("module"), binary("bt@triangle")),
            (atom("known_vars"), Term::from(eetf::List::from(vec![]))),
            (
                atom("class_superclass_index"),
                Term::from(superclass_index_map),
            ),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };

        // Must succeed
        let status = map_get(m, "status");
        assert_eq!(
            status,
            Some(&atom("ok")),
            "Expected ok status, got: {response:?}"
        );

        // Must be a class_definition response
        let kind = map_get(m, "kind");
        assert_eq!(
            kind,
            Some(&atom("class_definition")),
            "Expected class_definition kind, got: {response:?}"
        );

        // The generated Core Erlang must NOT contain gen_server (value type, not Actor)
        let core_erlang = map_get(m, "core_erlang")
            .and_then(term_to_string)
            .expect("core_erlang field must be present");
        assert!(
            !core_erlang.contains("'gen_server'"),
            "Triangle should be a value type (Object chain), not an Actor. \
             Shape→Object means Triangle→Shape→Object. Got core_erlang:\n{core_erlang}"
        );
    }

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

    /// ADR 0050 Phase 4: roundtrip — construct ETF `class_hierarchy` map, deserialize,
    /// verify `ClassInfo` fields match.
    #[test]
    fn parse_class_hierarchy_from_term_roundtrip() {
        use eetf::{FixInteger, List};

        let value_method_map = Map::from([
            (atom("arity"), Term::from(FixInteger::from(0))),
            (atom("param_types"), Term::from(List::from(vec![]))),
            (atom("return_type"), atom("Integer")),
        ]);
        let method_info_map = Map::from([(atom("value"), Term::from(value_method_map))]);

        let new_class_method_map = Map::from([
            (atom("arity"), Term::from(FixInteger::from(0))),
            (atom("param_types"), Term::from(List::from(vec![]))),
            (atom("return_type"), atom("counter")),
        ]);
        let class_method_info_map = Map::from([(atom("new"), Term::from(new_class_method_map))]);

        let field_types_map = Map::from([(atom("count"), atom("Integer"))]);

        let meta_map = Map::from([
            (atom("class"), atom("counter")),
            (atom("superclass"), atom("Actor")),
            (atom("meta_version"), Term::from(FixInteger::from(2))),
            (atom("is_sealed"), atom("false")),
            (atom("is_abstract"), atom("false")),
            (atom("is_value"), atom("false")),
            (atom("is_typed"), atom("false")),
            (atom("fields"), Term::from(List::from(vec![atom("count")]))),
            (atom("field_types"), Term::from(field_types_map)),
            (atom("method_info"), Term::from(method_info_map)),
            (atom("class_method_info"), Term::from(class_method_info_map)),
            (atom("class_variables"), Term::from(List::from(vec![]))),
        ]);

        let class_hierarchy_term = Term::from(Map::from([(atom("counter"), Term::from(meta_map))]));

        let classes = parse_class_hierarchy_from_term(&class_hierarchy_term);
        assert_eq!(classes.len(), 1, "Should parse one class");

        let info = &classes[0];
        assert_eq!(info.name.as_str(), "counter");
        assert_eq!(info.superclass.as_deref(), Some("Actor"));
        assert!(!info.is_sealed);
        assert!(!info.is_abstract);
        assert!(!info.is_value);
        assert!(!info.is_typed);
        assert_eq!(info.state.len(), 1);
        assert_eq!(info.state[0].as_str(), "count");
        assert_eq!(
            info.state_types.get("count").map(ecow::EcoString::as_str),
            Some("Integer")
        );
        assert_eq!(info.methods.len(), 1);
        assert_eq!(info.methods[0].selector.as_str(), "value");
        assert_eq!(info.methods[0].arity, 0);
        assert_eq!(info.methods[0].return_type.as_deref(), Some("Integer"));
        assert_eq!(info.class_methods.len(), 1);
        assert_eq!(info.class_methods[0].selector.as_str(), "new");
        assert_eq!(info.class_methods[0].arity, 0);
    }

    /// ADR 0050 Phase 4: `class_hierarchy` in `compile_expression` request is accepted
    /// and does not cause errors (backward-compatible optional key).
    #[test]
    fn compile_expression_accepts_class_hierarchy_key() {
        use eetf::{FixInteger, List};

        let method_info = Map::from([(
            atom("value"),
            Term::from(Map::from([
                (atom("arity"), Term::from(FixInteger::from(0))),
                (atom("param_types"), Term::from(List::from(vec![]))),
                (atom("return_type"), atom("Integer")),
            ])),
        )]);
        let counter_meta = Map::from([
            (atom("class"), atom("Counter")),
            (atom("superclass"), atom("Object")),
            (atom("meta_version"), Term::from(FixInteger::from(2))),
            (atom("is_sealed"), atom("false")),
            (atom("is_abstract"), atom("false")),
            (atom("is_value"), atom("false")),
            (atom("is_typed"), atom("false")),
            (atom("fields"), Term::from(List::from(vec![]))),
            (atom("field_types"), Term::from(Map::from([]))),
            (atom("method_info"), Term::from(method_info)),
            (atom("class_method_info"), Term::from(Map::from([]))),
            (atom("class_variables"), Term::from(List::from(vec![]))),
        ]);
        let class_hierarchy_term =
            Term::from(Map::from([(atom("Counter"), Term::from(counter_meta))]));

        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (atom("source"), binary("1 + 1.")),
            (atom("module"), binary("bt@test_repl")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
            (atom("class_hierarchy"), class_hierarchy_term),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "compile_expression with class_hierarchy should succeed: {response:?}"
        );
    }

    #[test]
    fn parse_class_hierarchy_skips_builtins() {
        use eetf::{FixInteger, List};

        let meta_map = Map::from([
            (atom("class"), atom("Integer")),
            (atom("superclass"), atom("Number")),
            (atom("meta_version"), Term::from(FixInteger::from(2))),
            (atom("is_sealed"), atom("false")),
            (atom("is_abstract"), atom("false")),
            (atom("is_value"), atom("false")),
            (atom("is_typed"), atom("false")),
            (atom("fields"), Term::from(List::from(vec![]))),
            (atom("field_types"), Term::from(Map::from([]))),
            (atom("method_info"), Term::from(Map::from([]))),
            (atom("class_method_info"), Term::from(Map::from([]))),
            (atom("class_variables"), Term::from(List::from(vec![]))),
        ]);
        let term = Term::from(Map::from([(atom("Integer"), Term::from(meta_map))]));
        let classes = parse_class_hierarchy_from_term(&term);
        assert!(
            classes.is_empty(),
            "Integer is a builtin — should be skipped"
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
