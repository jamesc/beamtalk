// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generate a skeleton Erlang `gen_server` module from a `native:` Actor class.
//!
//! Reads a `.bt` file, extracts the `native:` module declaration and all
//! `self delegate` methods, and writes a skeleton `.erl` file with matching
//! `handle_call/3` clauses.

use beamtalk_core::ast::{ClassDefinition, MessageSelector, MethodDefinition, TypeAnnotation};
use camino::Utf8PathBuf;
use miette::{Context, Result};
use std::fmt::Write as _;
use std::fs;

/// Run the `gen-native` command: parse a `.bt` file and generate a skeleton `.erl` file.
pub fn run(class_name: &str) -> Result<()> {
    // Find the .bt file for the given class name
    let bt_path = find_bt_file(class_name)?;

    let source = fs::read_to_string(bt_path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{bt_path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Report parse errors
    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        let errors: Vec<String> = diagnostics
            .iter()
            .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
            .map(|d| d.message.to_string())
            .collect();
        miette::bail!("Parse errors in '{bt_path}':\n{}", errors.join("\n"));
    }

    // Find the class matching the requested name
    let class = module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == class_name)
        .ok_or_else(|| miette::miette!("Class '{class_name}' not found in '{bt_path}'"))?;

    // Ensure the class has a native: declaration
    let backing_module = class.backing_module.as_ref().ok_or_else(|| {
        miette::miette!(
            "Class '{class_name}' has no `native:` declaration. \
                 Only native Actor classes can generate Erlang stubs.\n\
                 Expected: Actor subclass: {class_name} native: <module_name>"
        )
    })?;

    let module_name = backing_module.name.as_str();

    // Collect delegate methods (instance methods with `self delegate` body)
    let delegate_methods: Vec<&MethodDefinition> = class
        .methods
        .iter()
        .filter(|m| m.is_self_delegate())
        .collect();

    if delegate_methods.is_empty() {
        eprintln!(
            "warning: Class '{class_name}' has native: declaration but no `self delegate` methods"
        );
    }

    // Generate the Erlang source
    let erlang_source = generate_erlang_stub(class, module_name, &delegate_methods);

    // Write the output file
    let output_path = Utf8PathBuf::from(format!("{module_name}.erl"));
    fs::write(output_path.as_std_path(), &erlang_source)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{output_path}'"))?;

    println!("Generated {output_path} from {bt_path}");
    println!(
        "  {} handle_call/3 clause(s) for delegate methods",
        delegate_methods.len()
    );

    Ok(())
}

/// Find the `.bt` file for the given class name.
///
/// Searches in order:
/// 1. `{ClassName}.bt` in current directory
/// 2. `src/{ClassName}.bt`
/// 3. `lib/{ClassName}.bt`
fn find_bt_file(class_name: &str) -> Result<Utf8PathBuf> {
    let candidates = [
        Utf8PathBuf::from(format!("{class_name}.bt")),
        Utf8PathBuf::from(format!("src/{class_name}.bt")),
        Utf8PathBuf::from(format!("lib/{class_name}.bt")),
    ];

    for candidate in &candidates {
        if candidate.exists() {
            return Ok(candidate.clone());
        }
    }

    miette::bail!(
        "Cannot find '{class_name}.bt'. Searched:\n  {}\n  {}\n  {}",
        candidates[0],
        candidates[1],
        candidates[2]
    );
}

/// Generate the Erlang `gen_server` skeleton source.
fn generate_erlang_stub(
    class: &ClassDefinition,
    module_name: &str,
    delegate_methods: &[&MethodDefinition],
) -> String {
    let class_name = class.name.name.as_str();
    let mut out = String::new();

    // Header comment
    writeln!(
        out,
        "%% Generated from {class_name}.bt — fill in implementations"
    )
    .unwrap();
    writeln!(
        out,
        "%% @doc Backing gen_server for the {class_name} native Actor."
    )
    .unwrap();
    writeln!(out).unwrap();

    // Module declaration
    writeln!(out, "-module({module_name}).").unwrap();
    writeln!(out, "-behaviour(gen_server).").unwrap();
    writeln!(out).unwrap();

    // Exports
    writeln!(out, "-export([start_link/1]).").unwrap();
    writeln!(
        out,
        "-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2])."
    )
    .unwrap();
    writeln!(out).unwrap();

    // Type spec and start_link
    writeln!(
        out,
        "-spec start_link(map()) -> {{ok, pid()}} | {{error, term()}}."
    )
    .unwrap();
    writeln!(out, "start_link(Config) ->").unwrap();
    writeln!(out, "    gen_server:start_link(?MODULE, Config, []).").unwrap();
    writeln!(out).unwrap();

    // init/1
    writeln!(out, "-spec init(map()) -> {{ok, map()}}.").unwrap();
    writeln!(out, "init(_Config) ->").unwrap();
    writeln!(out, "    %% TODO: initialise state from Config").unwrap();
    writeln!(out, "    {{ok, #{{}}}}.").unwrap();
    writeln!(out).unwrap();

    // handle_call/3 clauses
    writeln!(out, "%% --- Delegate methods from {class_name}.bt ---").unwrap();
    writeln!(out).unwrap();

    if delegate_methods.is_empty() {
        writeln!(out, "handle_call(_Request, _From, State) ->").unwrap();
        writeln!(out, "    {{reply, {{error, not_implemented}}, State}}.").unwrap();
    } else {
        for method in delegate_methods {
            write_handle_call_clause(&mut out, method);
        }
        // Catch-all clause
        writeln!(out).unwrap();
        writeln!(out, "handle_call(_Request, _From, State) ->").unwrap();
        writeln!(out, "    {{reply, {{error, not_implemented}}, State}}.").unwrap();
    }
    writeln!(out).unwrap();

    // handle_cast/2
    writeln!(out, "handle_cast(_Msg, State) ->").unwrap();
    writeln!(out, "    {{noreply, State}}.").unwrap();
    writeln!(out).unwrap();

    // handle_info/2
    writeln!(out, "handle_info(_Info, State) ->").unwrap();
    writeln!(out, "    {{noreply, State}}.").unwrap();
    writeln!(out).unwrap();

    // terminate/2
    writeln!(out, "terminate(_Reason, _State) ->").unwrap();
    writeln!(out, "    ok.").unwrap();

    out
}

/// Write a single `handle_call/3` clause for a delegate method.
///
/// All delegate clauses end with `;` because a catch-all clause always follows.
fn write_handle_call_clause(out: &mut String, method: &MethodDefinition) {
    let selector_name = method.selector.name();

    // Build the argument pattern
    let arg_vars = build_arg_variables(method);
    let args_pattern = if arg_vars.is_empty() {
        "[]".to_string()
    } else {
        format!("[{}]", arg_vars.join(", "))
    };

    // Quote the selector atom if it contains special characters
    let atom = if needs_quoting(&selector_name) {
        format!("'{selector_name}'")
    } else {
        selector_name.to_string()
    };

    // Add doc comment with return type if available
    if let Some(ref ret_type) = method.return_type {
        let type_str = format_type_annotation(ret_type);
        writeln!(out, "%% {selector_name} -> {type_str}").unwrap();
    }

    writeln!(
        out,
        "handle_call({{{atom}, {args_pattern}}}, _From, State) ->"
    )
    .unwrap();

    writeln!(out, "    %% TODO: implement {selector_name}").unwrap();
    writeln!(out, "    {{reply, {{ok, todo}}, State}};").unwrap();
}

/// Returns `true` if the atom name needs single-quoting in Erlang.
fn needs_quoting(name: &str) -> bool {
    // Erlang atoms starting with lowercase and containing only [a-z0-9_@]
    // don't need quoting. Everything else does.
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_lowercase() => {}
        _ => return true,
    }
    name.chars()
        .any(|c| !c.is_ascii_alphanumeric() && c != '_' && c != '@')
}

/// Build Erlang variable names from method parameters.
fn build_arg_variables(method: &MethodDefinition) -> Vec<String> {
    match &method.selector {
        MessageSelector::Unary(_) => vec![],
        MessageSelector::Binary(_) => {
            // Binary methods have one parameter
            if let Some(param) = method.parameters.first() {
                vec![capitalize_erlang_var(&param.name.name)]
            } else {
                vec!["_Arg".to_string()]
            }
        }
        MessageSelector::Keyword(parts) => {
            // Use parameter names if available, else derive from keyword parts
            if method.parameters.len() == parts.len() {
                method
                    .parameters
                    .iter()
                    .map(|p| capitalize_erlang_var(&p.name.name))
                    .collect()
            } else {
                parts
                    .iter()
                    .map(|kw| {
                        let name = kw.keyword.trim_end_matches(':');
                        capitalize_erlang_var(name)
                    })
                    .collect()
            }
        }
    }
}

/// Capitalize first letter to make a valid Erlang variable name.
fn capitalize_erlang_var(name: &str) -> String {
    let mut chars = name.chars();
    match chars.next() {
        None => "_Arg".to_string(),
        Some(first) => {
            let upper: String = first.to_uppercase().collect();
            format!("{upper}{}", chars.as_str())
        }
    }
}

/// Format a type annotation as a human-readable string (for comments).
fn format_type_annotation(ty: &TypeAnnotation) -> String {
    match ty {
        TypeAnnotation::Simple(id) => id.name.to_string(),
        TypeAnnotation::Singleton { name, .. } => format!("#{name}"),
        TypeAnnotation::Union { types, .. } => {
            let parts: Vec<String> = types.iter().map(format_type_annotation).collect();
            parts.join(" | ")
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            let params: Vec<String> = parameters.iter().map(format_type_annotation).collect();
            format!("{}({})", base.name, params.join(", "))
        }
        // For other variants, just produce a placeholder
        _ => "Object".to_string(),
    }
}

use miette::IntoDiagnostic;

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse a `.bt` source string and return the first class.
    fn parse_class(source: &str) -> beamtalk_core::ast::ClassDefinition {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        module.classes.into_iter().next().expect("no class found")
    }

    #[test]
    fn generates_module_and_behaviour() {
        let source = r"
Actor subclass: MyActor native: my_actor
  getValue -> Integer => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        assert!(output.contains("-module(my_actor)."));
        assert!(output.contains("-behaviour(gen_server)."));
    }

    #[test]
    fn generates_start_link_and_init() {
        let source = r"
Actor subclass: MyActor native: my_actor
  getValue -> Integer => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        assert!(output.contains("start_link(Config) ->"));
        assert!(output.contains("gen_server:start_link(?MODULE, Config, [])."));
        assert!(output.contains("init(_Config) ->"));
    }

    #[test]
    fn generates_handle_call_for_unary_delegate() {
        let source = r"
Actor subclass: MyActor native: my_actor
  status -> String => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        assert!(output.contains("handle_call({status, []}, _From, State) ->"));
        assert!(output.contains("{reply, {ok, todo}, State};"));
        assert!(output.contains("%% TODO: implement status"));
    }

    #[test]
    fn generates_handle_call_for_keyword_delegate() {
        let source = r"
Actor subclass: MyActor native: my_actor
  writeLine: data -> Nil => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        assert!(
            output.contains("handle_call({'writeLine:', [Data]}, _From, State) ->"),
            "Got:\n{output}"
        );
    }

    #[test]
    fn generates_handle_call_for_multi_keyword_delegate() {
        let source = r"
Actor subclass: MyActor native: my_actor
  at: index put: value -> Nil => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        assert!(
            output.contains("handle_call({'at:put:', [Index, Value]}, _From, State) ->"),
            "Got:\n{output}"
        );
    }

    #[test]
    fn skips_non_delegate_methods() {
        let source = r"
Actor subclass: MyActor native: my_actor
  class create => self spawnWith: #{}
  getValue -> Integer => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        // Should have getValue but not create
        assert!(output.contains("handle_call({getValue, []}, _From, State) ->"));
        assert!(!output.contains("create"));
    }

    #[test]
    fn includes_return_type_comments() {
        let source = r"
Actor subclass: MyActor native: my_actor
  status -> String => self delegate
  count -> Integer => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        assert!(output.contains("%% status -> String"));
        assert!(output.contains("%% count -> Integer"));
    }

    #[test]
    fn generates_catch_all_clause() {
        let source = r"
Actor subclass: MyActor native: my_actor
  getValue -> Integer => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        assert!(output.contains("handle_call(_Request, _From, State) ->"));
        assert!(output.contains("{reply, {error, not_implemented}, State}."));
    }

    #[test]
    fn generates_ok_wrapped_replies() {
        let source = r"
Actor subclass: MyActor native: my_actor
  getValue -> Integer => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        // Delegate clauses use {ok, todo} wrapping
        assert!(output.contains("{reply, {ok, todo}, State}"));
    }

    #[test]
    fn capitalize_erlang_var_works() {
        assert_eq!(capitalize_erlang_var("data"), "Data");
        assert_eq!(capitalize_erlang_var("timeout"), "Timeout");
        assert_eq!(capitalize_erlang_var("x"), "X");
        assert_eq!(capitalize_erlang_var(""), "_Arg");
    }

    #[test]
    fn needs_quoting_works() {
        assert!(!needs_quoting("status"));
        assert!(!needs_quoting("getValue"));
        assert!(needs_quoting("writeLine:"));
        assert!(needs_quoting("at:put:"));
        assert!(needs_quoting("+"));
        assert!(needs_quoting(">="));
    }

    #[test]
    fn header_mentions_class_name() {
        let source = r"
Actor subclass: MyActor native: my_actor
  getValue -> Integer => self delegate
";
        let class = parse_class(source);
        let delegates: Vec<&MethodDefinition> = class
            .methods
            .iter()
            .filter(|m| m.is_self_delegate())
            .collect();
        let output = generate_erlang_stub(&class, "my_actor", &delegates);

        assert!(output.contains("Generated from MyActor.bt"));
    }
}
