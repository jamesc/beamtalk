// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generate Beamtalk stub definitions from `.beam` abstract code (ADR 0075 Phase 3).
//!
//! **DDD Context:** Compilation
//!
//! Reads `-spec` attributes and parameter names from compiled `.beam` files
//! and generates `.bt` stub files with `declare native:` forms. Supports
//! both named OTP modules and local native `.beam` directories.
//!
//! ## Usage
//!
//! ```bash
//! # Named OTP modules
//! beamtalk generate stubs lists maps string
//!
//! # Native directory
//! beamtalk generate stubs --native-dir native/
//!
//! # Custom output directory
//! beamtalk generate stubs lists -o `my_stubs`/
//! ```

use beamtalk_core::semantic_analysis::type_checker::FunctionSignature;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write as _;
use std::process::Command;

/// Run the `generate stubs` command.
///
/// Locates `.beam` files for the requested modules, extracts `-spec` attributes
/// via the build worker, and writes `.bt` stub files to the output directory.
pub fn run(modules: &[String], native_dir: Option<&str>, output: &str) -> Result<()> {
    if modules.is_empty() && native_dir.is_none() {
        miette::bail!(
            "No modules specified.\n\
             Usage: beamtalk generate stubs <module1> <module2> ...\n\
             Or:    beamtalk generate stubs --native-dir native/"
        );
    }

    // Collect (module_name, beam_path) pairs from both sources.
    let mut beam_files: Vec<(String, Utf8PathBuf)> = Vec::new();

    // Resolve named modules via OTP code path.
    for module in modules {
        match locate_beam_file(module)? {
            BeamLocation::Found(path) => {
                eprintln!("Reading {module}.beam ...");
                beam_files.push((module.clone(), path));
            }
            BeamLocation::NotFound => {
                eprintln!("info: {module}.beam not found on code path — skipping");
            }
            BeamLocation::Preloaded => {
                eprintln!(
                    "info: {module} is a preloaded module — no .beam file on disk. \
                     Write stubs/{module}.bt manually."
                );
            }
        }
    }

    // Collect .beam files from --native-dir.
    if let Some(dir) = native_dir {
        let dir_path = Utf8Path::new(dir);
        if !dir_path.exists() {
            miette::bail!("Native directory '{dir}' does not exist");
        }

        let entries = std::fs::read_dir(dir_path.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?;

        for entry in entries {
            let entry = entry
                .into_diagnostic()
                .wrap_err_with(|| format!("I/O error reading entry in '{dir}'"))?;
            let path = entry.path();
            if path.extension().is_some_and(|e| e == "beam") {
                if let Ok(utf8_path) = Utf8PathBuf::from_path_buf(path) {
                    let module_name = utf8_path.file_stem().unwrap_or("unknown").to_string();
                    if !is_valid_module_name(&module_name) {
                        eprintln!(
                            "warning: Skipping '{module_name}.beam' — \
                             not a valid Erlang module name"
                        );
                        continue;
                    }
                    eprintln!("Reading {module_name}.beam ...");
                    beam_files.push((module_name, utf8_path));
                }
            }
        }
    }

    if beam_files.is_empty() {
        miette::bail!("No .beam files found for the requested modules");
    }

    // Extract specs via the build worker.
    let beam_paths: Vec<Utf8PathBuf> = beam_files.iter().map(|(_, p)| p.clone()).collect();
    let temp_cache = std::env::temp_dir().join("beamtalk_stub_cache");
    let cache_dir = Utf8PathBuf::from_path_buf(temp_cache)
        .map_err(|p| miette::miette!("Non-UTF8 temp directory: {}", p.display()))?;

    let registry = crate::beam_compiler::extract_beam_specs(&beam_paths, &cache_dir)?;

    // Create output directory.
    let output_path = Utf8Path::new(output);
    std::fs::create_dir_all(output_path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create output directory '{output}'"))?;

    // Generate stub files.
    let mut generated_count = 0;
    for (module_name, _beam_path) in &beam_files {
        match registry.module_functions(module_name) {
            Some(functions) if !functions.is_empty() => {
                let stub_content = format_stub_file(module_name, functions);
                let stub_path = output_path.join(format!("{module_name}.bt"));

                std::fs::write(stub_path.as_std_path(), &stub_content)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("Failed to write '{stub_path}'"))?;

                eprintln!("  {module_name}.beam ... {} specs found", functions.len());
                generated_count += 1;
            }
            Some(_) => {
                eprintln!("  {module_name}.beam ... 0 specs found — skipping");
            }
            None => {
                eprintln!(
                    "info: {module_name}.beam has no debug_info — \
                     auto-extracted types unavailable. \
                     Add a stub file in stubs/ for type coverage."
                );
            }
        }
    }

    if generated_count > 0 {
        eprintln!();
        eprintln!("Generated {generated_count} stub file(s) in {output}/");
    } else {
        eprintln!();
        eprintln!("No stubs generated — no specs found in the requested modules.");
    }

    Ok(())
}

/// Result of locating a `.beam` file for a module name.
enum BeamLocation {
    /// Found a `.beam` file at this path.
    Found(Utf8PathBuf),
    /// Module does not exist on the code path.
    NotFound,
    /// Module is preloaded (built into the BEAM VM, no `.beam` file on disk).
    Preloaded,
}

/// Locate the `.beam` file for a named module via the Erlang code path.
///
/// Uses `erl -eval 'code:which(Module)'` to find the absolute path.
fn locate_beam_file(module_name: &str) -> Result<BeamLocation> {
    // Validate module name: must be a valid Erlang atom (lowercase start, alnum + _)
    if !is_valid_module_name(module_name) {
        miette::bail!("Invalid module name: '{module_name}' — must be a valid Erlang atom");
    }

    let eval_expr = format!(
        "case code:which({module_name}) of \
         non_existing -> io:format(\"non_existing\"), halt(1); \
         preloaded -> io:format(\"preloaded\"), halt(2); \
         cover_compiled -> io:format(\"cover_compiled\"), halt(1); \
         Path -> io:format(\"~s\", [Path]), halt(0) \
         end."
    );

    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg("no_dot_erlang")
        .arg("-eval")
        .arg(&eval_expr)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to locate beam file")?;

    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();

    if stdout == "preloaded" {
        return Ok(BeamLocation::Preloaded);
    }

    if !output.status.success() || stdout == "non_existing" || stdout == "cover_compiled" {
        return Ok(BeamLocation::NotFound);
    }

    if stdout.is_empty() {
        return Ok(BeamLocation::NotFound);
    }

    if let Ok(path) = Utf8PathBuf::try_from(std::path::PathBuf::from(&stdout)) {
        Ok(BeamLocation::Found(path))
    } else {
        eprintln!("warning: Non-UTF8 path for {module_name}: {stdout}");
        Ok(BeamLocation::NotFound)
    }
}

/// Checks whether a derived module name is a valid Erlang atom.
///
/// A valid module name starts with a lowercase ASCII letter and contains
/// only ASCII alphanumeric characters and underscores. This prevents
/// file stems with dashes, dots, or other special characters from being
/// treated as module names.
fn is_valid_module_name(name: &str) -> bool {
    !name.is_empty()
        && name.chars().next().is_some_and(|c| c.is_ascii_lowercase())
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Format a complete `.bt` stub file for a module.
///
/// Generates a valid Beamtalk `declare native:` form with all function
/// signatures formatted as keyword message type annotations.
fn format_stub_file(module_name: &str, functions: &[FunctionSignature]) -> String {
    let mut out = String::new();

    // Header comment
    writeln!(out, "// Auto-generated by `beamtalk generate stubs`").unwrap();
    writeln!(out, "// Source: {module_name}.beam").unwrap();
    writeln!(
        out,
        "// Edit to refine types (e.g., replace Dynamic with specific types)"
    )
    .unwrap();
    writeln!(out).unwrap();
    writeln!(out, "declare native: {module_name}").unwrap();

    // Sort functions by name then arity for stable output.
    let mut sorted: Vec<&FunctionSignature> = functions.iter().collect();
    sorted.sort_by(|a, b| a.name.cmp(&b.name).then(a.arity.cmp(&b.arity)));

    // Track the last function name to add blank lines between different functions.
    let mut last_name: Option<&str> = None;

    for sig in &sorted {
        if last_name.is_some_and(|prev| prev != sig.name.as_str()) {
            writeln!(out).unwrap();
        }
        last_name = Some(&sig.name);

        write!(out, "  ").unwrap();
        write!(out, "{}", format_signature(sig)).unwrap();
        writeln!(out).unwrap();
    }

    out
}

/// Format a single function signature as a Beamtalk stub declaration.
///
/// Uses the same keyword message syntax as protocol definitions (ADR 0068):
/// - Nullary: `functionName -> ReturnType`
/// - Unary:   `functionName: param :: Type -> ReturnType`
/// - Binary+: `functionName: param1 :: Type1 keyword2: param2 :: Type2 -> ReturnType`
fn format_signature(sig: &FunctionSignature) -> String {
    let ret_type = format_type(&sig.return_type);

    if sig.params.is_empty() {
        return format!("{} -> {ret_type}", sig.name);
    }

    let mut parts = Vec::new();
    for (i, param) in sig.params.iter().enumerate() {
        let param_type = format_type(&param.type_);

        if i == 0 {
            // First param: keyword is the function name
            let param_name = param.keyword.as_deref().unwrap_or("arg");
            parts.push(format!("{}: {} :: {}", sig.name, param_name, param_type));
        } else {
            // Subsequent params: use keyword name or fallback to `with:`
            let keyword = param.keyword.as_deref().unwrap_or("with");
            let param_name = param.keyword.as_deref().unwrap_or("arg");
            parts.push(format!("{keyword}: {param_name} :: {param_type}"));
        }
    }

    format!("{} -> {ret_type}", parts.join(" "))
}

/// Format an `InferredType` as a Beamtalk type annotation string.
fn format_type(ty: &beamtalk_core::semantic_analysis::type_checker::InferredType) -> String {
    use beamtalk_core::semantic_analysis::type_checker::InferredType;

    match ty {
        InferredType::Dynamic(_) => "Dynamic".to_string(),
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            if type_args.is_empty() {
                class_name.to_string()
            } else {
                let args: Vec<String> = type_args.iter().map(format_type).collect();
                format!("{}({})", class_name, args.join(", "))
            }
        }
        InferredType::Union { members, .. } => {
            let parts: Vec<String> = members.iter().map(format_type).collect();
            parts.join(" | ")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::semantic_analysis::type_checker::{
        DynamicReason, FunctionSignature, InferredType, ParamType, TypeProvenance,
    };

    fn sig(
        name: &str,
        arity: u8,
        params: Vec<ParamType>,
        return_type: InferredType,
    ) -> FunctionSignature {
        FunctionSignature {
            name: name.to_string(),
            arity,
            params,
            return_type,
            provenance: TypeProvenance::Extracted,
            line: None,
        }
    }

    fn param(keyword: &str, type_name: &str) -> ParamType {
        ParamType {
            keyword: Some(keyword.into()),
            type_: InferredType::known(type_name),
        }
    }

    fn param_no_keyword(type_name: &str) -> ParamType {
        ParamType {
            keyword: None,
            type_: InferredType::known(type_name),
        }
    }

    #[test]
    fn format_nullary_signature() {
        let s = sig("node", 0, vec![], InferredType::known("Symbol"));
        assert_eq!(format_signature(&s), "node -> Symbol");
    }

    #[test]
    fn format_unary_signature() {
        let s = sig(
            "reverse",
            1,
            vec![param("list", "List")],
            InferredType::known("List"),
        );
        assert_eq!(format_signature(&s), "reverse: list :: List -> List");
    }

    #[test]
    fn format_binary_signature() {
        let s = sig(
            "seq",
            2,
            vec![param("from", "Integer"), param("to", "Integer")],
            InferredType::known("List"),
        );
        assert_eq!(
            format_signature(&s),
            "seq: from :: Integer to: to :: Integer -> List"
        );
    }

    #[test]
    fn format_signature_with_dynamic_return() {
        let s = sig(
            "apply",
            1,
            vec![param("fun", "Block")],
            InferredType::Dynamic(DynamicReason::DynamicSpec),
        );
        assert_eq!(format_signature(&s), "apply: fun :: Block -> Dynamic");
    }

    #[test]
    fn format_signature_no_keyword_uses_with() {
        let s = sig(
            "foo",
            2,
            vec![param_no_keyword("Integer"), param_no_keyword("String")],
            InferredType::known("Boolean"),
        );
        assert_eq!(
            format_signature(&s),
            "foo: arg :: Integer with: arg :: String -> Boolean"
        );
    }

    #[test]
    fn format_stub_file_has_header() {
        let functions = vec![sig(
            "reverse",
            1,
            vec![param("list", "List")],
            InferredType::known("List"),
        )];
        let content = format_stub_file("lists", &functions);

        assert!(content.contains("// Auto-generated by `beamtalk generate stubs`"));
        assert!(content.contains("// Source: lists.beam"));
        assert!(content.contains("declare native: lists"));
    }

    #[test]
    fn format_stub_file_contains_signatures() {
        let functions = vec![
            sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            ),
            sig(
                "seq",
                2,
                vec![param("from", "Integer"), param("to", "Integer")],
                InferredType::known("List"),
            ),
        ];
        let content = format_stub_file("lists", &functions);

        assert!(
            content.contains("  reverse: list :: List -> List"),
            "Expected reverse signature, got:\n{content}"
        );
        assert!(
            content.contains("  seq: from :: Integer to: to :: Integer -> List"),
            "Expected seq signature, got:\n{content}"
        );
    }

    #[test]
    fn format_stub_file_sorted_by_name() {
        let functions = vec![
            sig(
                "zip",
                2,
                vec![param("a", "List"), param("b", "List")],
                InferredType::known("List"),
            ),
            sig(
                "append",
                2,
                vec![param("a", "List"), param("b", "List")],
                InferredType::known("List"),
            ),
        ];
        let content = format_stub_file("lists", &functions);

        let append_pos = content.find("append:").unwrap();
        let zip_pos = content.find("zip:").unwrap();
        assert!(
            append_pos < zip_pos,
            "Functions should be sorted alphabetically"
        );
    }

    #[test]
    fn format_stub_file_blank_lines_between_functions() {
        let functions = vec![
            sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            ),
            sig(
                "seq",
                2,
                vec![param("from", "Integer"), param("to", "Integer")],
                InferredType::known("List"),
            ),
            sig(
                "seq",
                3,
                vec![
                    param("from", "Integer"),
                    param("to", "Integer"),
                    param("step", "Integer"),
                ],
                InferredType::known("List"),
            ),
        ];
        let content = format_stub_file("lists", &functions);

        // seq/2 and seq/3 should be adjacent (same name), but reverse and seq
        // should have a blank line between them.
        let lines: Vec<&str> = content.lines().collect();
        let reverse_idx = lines.iter().position(|l| l.contains("reverse:")).unwrap();
        let seq2_idx = lines
            .iter()
            .position(|l| l.contains("seq: from :: Integer to:") && !l.contains("step:"))
            .unwrap();

        // There should be an empty line between reverse and seq
        let between = &lines[reverse_idx + 1..seq2_idx];
        assert!(
            between.iter().any(|l| l.trim().is_empty()),
            "Expected blank line between different functions, got:\n{content}"
        );
    }

    #[test]
    fn format_union_return_type() {
        let ret = InferredType::simple_union(&["Integer", "String"]);
        let s = sig("foo", 1, vec![param("x", "Dynamic")], ret);
        let formatted = format_signature(&s);
        assert!(
            formatted.contains("Integer | String"),
            "Expected union type in: {formatted}"
        );
    }

    #[test]
    fn locate_beam_file_rejects_invalid_names() {
        // Starting with uppercase
        assert!(locate_beam_file("Lists").is_err());
        // Empty
        assert!(locate_beam_file("").is_err());
        // Special chars
        assert!(locate_beam_file("foo-bar").is_err());
    }

    #[test]
    fn is_valid_module_name_accepts_valid_names() {
        assert!(is_valid_module_name("lists"));
        assert!(is_valid_module_name("my_app"));
        assert!(is_valid_module_name("gen_server2"));
        assert!(is_valid_module_name("a"));
    }

    #[test]
    fn is_valid_module_name_rejects_invalid_names() {
        // Empty
        assert!(!is_valid_module_name(""));
        // Starts with uppercase
        assert!(!is_valid_module_name("Lists"));
        // Contains dash
        assert!(!is_valid_module_name("foo-bar"));
        // Contains dot
        assert!(!is_valid_module_name("my.module"));
        // Starts with number
        assert!(!is_valid_module_name("1bad"));
        // Contains space
        assert!(!is_valid_module_name("foo bar"));
    }
}
