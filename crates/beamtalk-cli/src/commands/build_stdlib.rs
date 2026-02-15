// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build the Beamtalk standard library.
//!
//! **DDD Context:** CLI / Build System
//!
//! Compiles all `lib/*.bt` files through the normal pipeline with `--stdlib-mode`
//! and outputs `.beam` files to `runtime/apps/beamtalk_stdlib/ebin/`.
//! Always performs a full rebuild since source file mtime does not reflect
//! compiler or codegen changes.
//!
//! Part of ADR 0007 (Compilable Stdlib with Primitive Injection).

use crate::beam_compiler::{BeamCompiler, compile_source_with_bindings};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write;
use std::fs;
use tracing::{debug, info, instrument};

/// Default path to stdlib source files (relative to project root).
const STDLIB_SOURCE_DIR: &str = "lib";

/// Default output path for compiled stdlib BEAM files (relative to project root).
const STDLIB_EBIN_DIR: &str = "runtime/apps/beamtalk_stdlib/ebin";

/// Build the standard library.
///
/// Finds all `.bt` files in `lib/`, compiles them with stdlib mode enabled,
/// and writes `.beam` files to `runtime/apps/beamtalk_stdlib/ebin/`.
/// Always performs a full rebuild to ensure correctness after compiler changes.
#[instrument(skip_all)]
pub fn build_stdlib() -> Result<()> {
    info!("Starting stdlib build");

    let lib_dir = Utf8PathBuf::from(STDLIB_SOURCE_DIR);
    let ebin_dir = Utf8PathBuf::from(STDLIB_EBIN_DIR);

    if !lib_dir.exists() {
        miette::bail!("Standard library source directory '{}' not found", lib_dir);
    }

    // Find all .bt files in lib/
    let source_files = find_stdlib_files(&lib_dir)?;

    if source_files.is_empty() {
        println!("No .bt source files found in '{lib_dir}'");
        return Ok(());
    }

    info!(count = source_files.len(), "Found stdlib source files");

    // Create ebin directory
    fs::create_dir_all(&ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create ebin directory '{ebin_dir}'"))?;

    // Clean stale .beam files from previous builds (e.g. renamed/removed .bt sources)
    clean_ebin_dir(&ebin_dir)?;

    println!("Compiling {} stdlib module(s)...", source_files.len());

    // Create a temporary directory for .core files
    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory for Core Erlang files")?;
    let temp_path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;

    // Compiler options: stdlib mode enabled
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: true,
        allow_primitives: false,
        workspace_mode: false,
    };

    // BT-295 / ADR 0007 Phase 3: Build primitive binding table from ALL stdlib sources.
    // This is used during compilation so that @primitive expressions in method bodies
    // can reference the runtime dispatch modules.
    info!("Building primitive binding table from stdlib sources");
    let bindings =
        beamtalk_core::erlang::primitive_bindings::load_from_directory(lib_dir.as_std_path());
    info!(
        binding_count = bindings.len(),
        "Loaded primitive bindings from stdlib"
    );

    // Compile each .bt file to .core (files are independent, no ordering required)
    let mut core_files = Vec::new();
    let mut class_metadata = Vec::new();
    for source_file in &source_files {
        let module_name = module_name_from_path(source_file)?;
        let core_file = temp_path.join(format!("{module_name}.core"));

        // Extract class metadata (class_name, superclass) before compilation
        let meta = extract_class_metadata(source_file, &module_name)?;
        class_metadata.push(meta);

        compile_stdlib_file(source_file, &module_name, &core_file, &options, &bindings)?;
        core_files.push(core_file);
    }

    // Batch compile .core → .beam into ebin directory
    info!("Compiling Core Erlang to BEAM");
    let compiler = BeamCompiler::new(ebin_dir.clone());
    compiler
        .compile_batch(&core_files)
        .wrap_err("Failed to compile stdlib Core Erlang to BEAM")?;

    generate_app_file(&ebin_dir, &source_files, &class_metadata)?;
    // Also update .app.src so rebar3 picks up the classes env
    let app_src_dir = Utf8PathBuf::from("runtime/apps/beamtalk_stdlib/src");
    if app_src_dir.exists() {
        generate_app_src_file(&app_src_dir, &class_metadata)?;
    }

    // Generate Rust builtins file from parsed class metadata
    generate_builtins_rs(&class_metadata)?;

    println!("Built {} stdlib modules", source_files.len());

    Ok(())
}

/// Remove all `.beam` and `.app` files from the ebin directory.
///
/// Ensures no stale artifacts remain from renamed or removed `.bt` sources.
fn clean_ebin_dir(ebin_dir: &Utf8Path) -> Result<()> {
    for entry in fs::read_dir(ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read ebin directory '{ebin_dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if let Some(ext) = path.extension() {
            if ext == "beam" || ext == "app" {
                fs::remove_file(&path)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("Failed to remove '{}'", path.display()))?;
            }
        }
    }
    Ok(())
}

/// Find all `.bt` files in the stdlib source directory.
fn find_stdlib_files(lib_dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();

    for entry in fs::read_dir(lib_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{lib_dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", lib_dir))?;

        if path.extension() == Some("bt") {
            files.push(path);
        }
    }

    files.sort();
    Ok(files)
}

/// Extract the module name from a `.bt` file path.
///
/// ADR 0016: All stdlib classes use `bt@stdlib@{snake_case}` prefix.
/// The `@` separator is legal in unquoted Erlang atoms and follows
/// the Gleam convention (`gleam@list`, `gleam@string`).
fn module_name_from_path(path: &Utf8Path) -> Result<String> {
    let stem = path
        .file_stem()
        .ok_or_else(|| miette::miette!("File '{}' has no name", path))?;

    // Validate module name
    if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
        miette::bail!(
            "Invalid module name '{}': must contain only alphanumeric characters and underscores",
            stem
        );
    }

    let snake = beamtalk_core::codegen::core_erlang::to_module_name(stem);

    // ADR 0016: All stdlib modules use bt@stdlib@ prefix
    Ok(format!("bt@stdlib@{snake}"))
}

/// Compile a single stdlib `.bt` file to Core Erlang.
fn compile_stdlib_file(
    path: &Utf8Path,
    module_name: &str,
    core_file: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
) -> Result<()> {
    println!("  Compiling {path}...");
    compile_source_with_bindings(path, module_name, core_file, options, bindings)
}

/// Metadata for a single stdlib class, used to generate the load-order file
/// and the generated builtins module.
#[allow(clippy::struct_field_names)] // domain names like class_name match the domain model
struct ClassMeta {
    module_name: String,
    class_name: String,
    superclass_name: String,
    is_sealed: bool,
    is_abstract: bool,
    state: Vec<String>,
    methods: Vec<MethodMeta>,
    class_methods: Vec<MethodMeta>,
    class_variables: Vec<String>,
}

/// Metadata for a single method, extracted from the AST.
struct MethodMeta {
    selector: String,
    arity: usize,
    kind: MethodKindMeta,
    is_sealed: bool,
}

/// Simplified method kind for code generation.
enum MethodKindMeta {
    Primary,
    Before,
    After,
    Around,
}

impl MethodKindMeta {
    fn from_ast(kind: beamtalk_core::ast::MethodKind) -> Self {
        match kind {
            beamtalk_core::ast::MethodKind::Primary => Self::Primary,
            beamtalk_core::ast::MethodKind::Before => Self::Before,
            beamtalk_core::ast::MethodKind::After => Self::After,
            beamtalk_core::ast::MethodKind::Around => Self::Around,
        }
    }

    fn to_rust_expr(&self) -> &'static str {
        match self {
            Self::Primary => "MethodKind::Primary",
            Self::Before => "MethodKind::Before",
            Self::After => "MethodKind::After",
            Self::Around => "MethodKind::Around",
        }
    }
}

/// Extract class metadata from a `.bt` source file.
///
/// Parses the full class definition to extract the class name, superclass,
/// flags, state declarations, and method signatures. Each stdlib file
/// contains exactly one class definition.
fn extract_class_metadata(path: &Utf8Path, module_name: &str) -> Result<ClassMeta> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let class = module
        .classes
        .first()
        .ok_or_else(|| miette::miette!("No class definition in '{path}'"))?;

    if module.classes.len() > 1 {
        miette::bail!(
            "Expected exactly one class in '{path}', found {}",
            module.classes.len()
        );
    }

    let methods = class
        .methods
        .iter()
        .map(|m| MethodMeta {
            selector: m.selector.name().to_string(),
            arity: m.selector.arity(),
            kind: MethodKindMeta::from_ast(m.kind),
            is_sealed: m.is_sealed,
        })
        .collect();

    let class_methods = class
        .class_methods
        .iter()
        .map(|m| MethodMeta {
            selector: m.selector.name().to_string(),
            arity: m.selector.arity(),
            kind: MethodKindMeta::from_ast(m.kind),
            is_sealed: m.is_sealed,
        })
        .collect();

    let state = class
        .state
        .iter()
        .map(|s| s.name.name.to_string())
        .collect();

    let class_variables = class
        .class_variables
        .iter()
        .map(|cv| cv.name.name.to_string())
        .collect();

    Ok(ClassMeta {
        module_name: module_name.to_string(),
        class_name: class.name.name.to_string(),
        superclass_name: class.superclass_name().to_string(),
        is_sealed: class.is_sealed,
        is_abstract: class.is_abstract,
        state,
        methods,
        class_methods,
        class_variables,
    })
}

/// Generate the `beamtalk_stdlib.app` file in the ebin directory.
///
/// Lists all modules and embeds class hierarchy metadata in the `env` section.
/// The metadata is used by `beamtalk_stdlib` to load modules in dependency order.
fn generate_app_file(
    ebin_dir: &Utf8Path,
    source_files: &[Utf8PathBuf],
    class_metadata: &[ClassMeta],
) -> Result<()> {
    let module_names: Vec<String> = source_files
        .iter()
        .map(|f| module_name_from_path(f))
        .collect::<Result<_>>()?;

    let modules_list = module_names
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    // Generate class hierarchy entries for env
    let classes_list = class_metadata
        .iter()
        .map(|m| {
            format!(
                "{{'{module}', '{class}', '{super}'}}",
                module = m.module_name,
                class = m.class_name,
                super = m.superclass_name
            )
        })
        .collect::<Vec<_>>()
        .join(",\n                    ");

    let app_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, \"0.1.0\"}},\n\
         \x20   {{modules, [{modules_list}]}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, beamtalk_runtime]}},\n\
         \x20   {{env, [\n\
         \x20       {{classes, [{classes_list}]}}\n\
         \x20   ]}}\n\
         ]}}.\n"
    );

    let app_file = ebin_dir.join("beamtalk_stdlib.app");
    fs::write(&app_file, app_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{app_file}'"))?;

    debug!("Generated {}", app_file);
    Ok(())
}

/// Generate/update the `.app.src` file so rebar3 picks up the classes metadata.
///
/// The `.app.src` uses `{modules, []}` (rebar3 auto-fills modules) but embeds
/// the `{classes, [...]}` env for the runtime to read via `application:get_env`.
fn generate_app_src_file(src_dir: &Utf8Path, class_metadata: &[ClassMeta]) -> Result<()> {
    let classes_list = class_metadata
        .iter()
        .map(|m| {
            format!(
                "{{'{module}', '{class}', '{super}'}}",
                module = m.module_name,
                class = m.class_name,
                super = m.superclass_name
            )
        })
        .collect::<Vec<_>>()
        .join(",\n        ");

    let app_src_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, \"0.1.0\"}},\n\
         \x20   {{modules, []}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, beamtalk_runtime]}},\n\
         \x20   {{env, [\n\
         \x20       {{classes, [{classes_list}]}}\n\
         \x20   ]}}\n\
         ]}}.\n"
    );

    let app_src_file = src_dir.join("beamtalk_stdlib.app.src");
    fs::write(&app_src_file, &app_src_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{app_src_file}'"))?;

    debug!("Generated {}", app_src_file);
    Ok(())
}

/// Default path for generated builtins file (relative to project root).
const GENERATED_BUILTINS_PATH: &str =
    "crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs";

/// Generate the `generated_builtins.rs` file from parsed stdlib class metadata.
///
/// This produces a Rust source file that defines `generated_builtin_classes()` and
/// `is_generated_builtin_class()`, replacing the hand-written tables in `builtins.rs`.
fn generate_builtins_rs(class_metadata: &[ClassMeta]) -> Result<()> {
    let mut code = String::new();

    code.push_str(
        "// AUTO-GENERATED from lib/*.bt by `beamtalk build-stdlib` — do not edit manually.\n\
         // Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         //! Generated built-in class definitions derived from `lib/*.bt` stdlib sources.\n\
         //!\n\
         //! This file is regenerated by `beamtalk build-stdlib`. Any manual edits\n\
         //! will be overwritten on the next build.\n\
         \n\
         use super::super::{ClassInfo, MethodInfo};\n\
         use crate::ast::MethodKind;\n\
         use ecow::EcoString;\n\
         use std::collections::HashMap;\n\
         \n",
    );

    // Generate is_generated_builtin_class()
    code.push_str(
        "/// Returns true if the given class name is a stdlib built-in class.\n\
         ///\n\
         /// Auto-generated from `lib/*.bt` file names.\n\
         pub(super) fn is_generated_builtin_class(name: &str) -> bool {\n\
         \x20   matches!(\n\
         \x20       name,\n",
    );

    let mut sorted_meta: Vec<&ClassMeta> = class_metadata.iter().collect();
    sorted_meta.sort_by_key(|m| &m.class_name);

    for (i, meta) in sorted_meta.iter().enumerate() {
        if i == 0 {
            let _ = write!(code, "        \"{}\"", meta.class_name);
        } else {
            let _ = write!(code, "\n            | \"{}\"", meta.class_name);
        }
    }
    code.push_str("\n    )\n}\n\n");

    // Generate generated_builtin_classes()
    code.push_str(
        "/// Returns all stdlib built-in class definitions.\n\
         ///\n\
         /// Auto-generated from parsed `lib/*.bt` ASTs.\n\
         #[allow(clippy::too_many_lines)] // auto-generated from 32 stdlib classes\n\
         #[rustfmt::skip] // preserve compact generated layout\n\
         pub(super) fn generated_builtin_classes() -> HashMap<EcoString, ClassInfo> {\n\
         \x20   let mut classes = HashMap::new();\n\n",
    );

    for meta in &sorted_meta {
        generate_class_entry(&mut code, meta);
    }

    code.push_str("    classes\n}\n");

    let dest = Utf8PathBuf::from(GENERATED_BUILTINS_PATH);

    // Only write if content changed to avoid unnecessary recompilation
    let needs_write = match fs::read_to_string(&dest) {
        Ok(existing) => existing != code,
        Err(_) => true,
    };

    if needs_write {
        fs::write(&dest, &code)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to write '{dest}'"))?;
        debug!("Generated {}", dest);
    } else {
        debug!("Generated builtins unchanged, skipping write");
    }
    Ok(())
}

/// Generate a single class entry for `generated_builtin_classes()`.
fn generate_class_entry(code: &mut String, meta: &ClassMeta) {
    let superclass = if meta.superclass_name == "none" {
        "None".to_string()
    } else {
        format!("Some(\"{}\".into())", meta.superclass_name)
    };

    let _ = write!(
        code,
        "    classes.insert(\n\
         \x20       \"{name}\".into(),\n\
         \x20       ClassInfo {{\n\
         \x20           name: \"{name}\".into(),\n\
         \x20           superclass: {superclass},\n\
         \x20           is_sealed: {sealed},\n\
         \x20           is_abstract: {abstract_},\n",
        name = meta.class_name,
        sealed = meta.is_sealed,
        abstract_ = meta.is_abstract,
    );

    // State
    if meta.state.is_empty() {
        code.push_str("            state: vec![],\n");
    } else {
        code.push_str("            state: vec![");
        for (i, s) in meta.state.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "\"{s}\".into()");
        }
        code.push_str("],\n");
    }

    // Instance methods
    generate_method_list(code, "methods", &meta.methods, &meta.class_name);
    // Class methods
    generate_method_list(code, "class_methods", &meta.class_methods, &meta.class_name);

    // Class variables
    if meta.class_variables.is_empty() {
        code.push_str("            class_variables: vec![],\n");
    } else {
        code.push_str("            class_variables: vec![");
        for (i, cv) in meta.class_variables.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "\"{cv}\".into()");
        }
        code.push_str("],\n");
    }

    code.push_str("        },\n    );\n\n");
}

/// Generate a method list field (`methods` or `class_methods`).
fn generate_method_list(
    code: &mut String,
    field_name: &str,
    methods: &[MethodMeta],
    class_name: &str,
) {
    if methods.is_empty() {
        let _ = writeln!(code, "            {field_name}: vec![],");
        return;
    }

    let _ = writeln!(code, "            {field_name}: vec![");
    for m in methods {
        let kind = m.kind.to_rust_expr();
        // Escape backslashes and quotes in selector for Rust string literals
        let selector = m.selector.replace('\\', "\\\\").replace('"', "\\\"");
        let _ = writeln!(
            code,
            "                MethodInfo {{ selector: \"{selector}\".into(), arity: {arity}, \
             kind: {kind}, defined_in: \"{class}\".into(), is_sealed: {sealed} }},",
            arity = m.arity,
            class = class_name,
            sealed = m.is_sealed,
        );
    }
    code.push_str("            ],\n");
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_find_stdlib_files() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        fs::write(lib_dir.join("Integer.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("String.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("README.md"), "not a bt file").unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("Integer.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("String.bt")));
    }

    #[test]
    fn test_find_stdlib_files_sorted() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        fs::write(lib_dir.join("Zebra.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("Alpha.bt"), "// stub").unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files[0].as_str() < files[1].as_str());
    }

    #[test]
    fn test_find_stdlib_files_empty_dir() {
        let temp = TempDir::new().unwrap();
        let lib_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert!(files.is_empty());
    }

    #[test]
    fn test_module_name_from_path() {
        // ADR 0016: All stdlib modules use bt@stdlib@ prefix
        let path = Utf8PathBuf::from("lib/Integer.bt");
        assert_eq!(module_name_from_path(&path).unwrap(), "bt@stdlib@integer");
    }

    #[test]
    fn test_module_name_from_path_non_primitive() {
        let path = Utf8PathBuf::from("lib/SystemDictionary.bt");
        assert_eq!(
            module_name_from_path(&path).unwrap(),
            "bt@stdlib@system_dictionary"
        );
    }

    #[test]
    fn test_module_name_from_path_multi_word() {
        let path = Utf8PathBuf::from("lib/ProtoObject.bt");
        assert_eq!(
            module_name_from_path(&path).unwrap(),
            "bt@stdlib@proto_object"
        );
    }

    #[test]
    fn test_module_name_from_path_invalid() {
        let path = Utf8PathBuf::from("lib/my-module.bt");
        assert!(module_name_from_path(&path).is_err());
    }

    #[test]
    fn test_clean_ebin_dir() {
        let temp = TempDir::new().unwrap();
        let ebin_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        fs::write(ebin_dir.join("beamtalk_integer.beam"), "fake").unwrap();
        fs::write(ebin_dir.join("beamtalk_stdlib.app"), "fake").unwrap();
        fs::write(ebin_dir.join("keep_me.txt"), "keep").unwrap();

        clean_ebin_dir(&ebin_dir).unwrap();

        assert!(!ebin_dir.join("beamtalk_integer.beam").exists());
        assert!(!ebin_dir.join("beamtalk_stdlib.app").exists());
        assert!(ebin_dir.join("keep_me.txt").exists());
    }

    #[test]
    fn test_generate_app_file() {
        let temp = TempDir::new().unwrap();
        let ebin_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_files = vec![
            Utf8PathBuf::from("lib/Integer.bt"),
            Utf8PathBuf::from("lib/String.bt"),
        ];

        generate_app_file(&ebin_dir, &source_files, &[]).unwrap();

        let app_file = ebin_dir.join("beamtalk_stdlib.app");
        assert!(app_file.exists());

        let content = fs::read_to_string(app_file).unwrap();
        assert!(content.contains("beamtalk_stdlib"));
        // ADR 0016: Module names use bt@stdlib@ prefix
        assert!(content.contains("'bt@stdlib@integer'"));
        assert!(content.contains("'bt@stdlib@string'"));
    }

    #[test]
    fn test_generate_app_file_empty() {
        let temp = TempDir::new().unwrap();
        let ebin_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        generate_app_file(&ebin_dir, &[], &[]).unwrap();

        let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
        assert!(content.contains("{modules, []}"));
    }

    #[test]
    fn test_generate_app_file_invalid_name_errors() {
        let temp = TempDir::new().unwrap();
        let ebin_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_files = vec![
            Utf8PathBuf::from("lib/Integer.bt"),
            Utf8PathBuf::from("lib/my-bad-name.bt"),
        ];

        let result = generate_app_file(&ebin_dir, &source_files, &[]);
        assert!(result.is_err());
    }

    fn sample_class_meta() -> ClassMeta {
        ClassMeta {
            module_name: "bt@stdlib@counter".to_string(),
            class_name: "Counter".to_string(),
            superclass_name: "Actor".to_string(),
            is_sealed: false,
            is_abstract: false,
            state: vec!["count".to_string()],
            methods: vec![
                MethodMeta {
                    selector: "increment".to_string(),
                    arity: 0,
                    kind: MethodKindMeta::Primary,
                    is_sealed: false,
                },
                MethodMeta {
                    selector: "add:".to_string(),
                    arity: 1,
                    kind: MethodKindMeta::Primary,
                    is_sealed: true,
                },
            ],
            class_methods: vec![MethodMeta {
                selector: "default".to_string(),
                arity: 0,
                kind: MethodKindMeta::Primary,
                is_sealed: false,
            }],
            class_variables: vec![],
        }
    }

    #[test]
    fn test_generate_class_entry_emits_correct_structure() {
        let meta = sample_class_meta();
        let mut code = String::new();
        generate_class_entry(&mut code, &meta);

        assert!(code.contains("\"Counter\".into()"));
        assert!(code.contains("Some(\"Actor\".into())"));
        assert!(code.contains("is_sealed: false"));
        assert!(code.contains("is_abstract: false"));
        assert!(code.contains("\"count\".into()"));
        assert!(code.contains("selector: \"increment\".into()"));
        assert!(code.contains("selector: \"add:\".into()"));
        assert!(code.contains("arity: 1"));
        assert!(code.contains("selector: \"default\".into()"));
    }

    #[test]
    fn test_generate_class_entry_sealed_methods() {
        let meta = sample_class_meta();
        let mut code = String::new();
        generate_class_entry(&mut code, &meta);

        // The add: method is sealed
        assert!(code.contains("is_sealed: true"));
    }

    #[test]
    fn test_generate_class_entry_root_class() {
        let meta = ClassMeta {
            module_name: "bt@stdlib@proto_object".to_string(),
            class_name: "ProtoObject".to_string(),
            superclass_name: "none".to_string(),
            is_sealed: false,
            is_abstract: true,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
        };
        let mut code = String::new();
        generate_class_entry(&mut code, &meta);

        assert!(code.contains("superclass: None"));
        assert!(code.contains("is_abstract: true"));
        assert!(code.contains("methods: vec![],"));
    }

    #[test]
    fn test_generate_method_list_empty() {
        let mut code = String::new();
        generate_method_list(&mut code, "methods", &[], "Test");
        assert!(code.contains("methods: vec![],"));
    }

    #[test]
    fn test_generate_builtins_sorted_deterministic() {
        let meta = [
            ClassMeta {
                module_name: "bt@stdlib@zebra".to_string(),
                class_name: "Zebra".to_string(),
                superclass_name: "Object".to_string(),
                is_sealed: false,
                is_abstract: false,
                state: vec![],
                methods: vec![],
                class_methods: vec![],
                class_variables: vec![],
            },
            ClassMeta {
                module_name: "bt@stdlib@alpha".to_string(),
                class_name: "Alpha".to_string(),
                superclass_name: "Object".to_string(),
                is_sealed: true,
                is_abstract: false,
                state: vec![],
                methods: vec![],
                class_methods: vec![],
                class_variables: vec![],
            },
        ];

        let mut code = String::new();
        // Simulate the sorted generation from generate_builtins_rs
        let mut sorted: Vec<&ClassMeta> = meta.iter().collect();
        sorted.sort_by_key(|m| &m.class_name);

        for m in &sorted {
            generate_class_entry(&mut code, m);
        }

        // Alpha should appear before Zebra
        let alpha_pos = code.find("\"Alpha\"").unwrap();
        let zebra_pos = code.find("\"Zebra\"").unwrap();
        assert!(
            alpha_pos < zebra_pos,
            "Classes should be sorted alphabetically"
        );
    }
}
