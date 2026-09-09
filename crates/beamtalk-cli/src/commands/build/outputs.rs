// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP application packaging: the `.app` file, the generated application
//! callback module, and per-package corpus files for MCP discovery.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;
use tracing::info;

use crate::beam_compiler::ClassHierarchyContext;
use crate::commands::util::to_forward_slash;
use crate::commands::{app_file, manifest};

/// Collected build outputs needed for OTP application packaging.
///
/// Groups the module name lists and source file paths that
/// `generate_package_outputs` requires, reducing its parameter count.
pub(crate) struct PackageBuildOutputs<'a> {
    /// All compiled Beamtalk module names (e.g. `"bt@my_app@main"`).
    pub(crate) module_names: &'a [String],
    /// Native Erlang module names compiled from `native/*.erl`.
    pub(crate) native_module_names: &'a [String],
    /// Names of Beamtalk path dependencies (for OTP `{applications}` list).
    pub(crate) bt_dep_names: &'a [String],
    /// Names of hex dependencies (for OTP `{applications}` list).
    pub(crate) hex_dep_names: &'a [String],
    /// All `.bt` source files in the package.
    pub(crate) source_files: &'a [Utf8PathBuf],
}

/// Generate OTP application artefacts for a package build.
///
/// Emits the `.app` file and, when `[application] supervisor` is set, the
/// OTP application callback module (`beamtalk_{appname}_app.erl` + `.beam`).
///
/// `hierarchy.class_module_index` maps Beamtalk class names to their compiled
/// Erlang module names (e.g. `"AppSup"` → `"bt@my_app@supervision@app_sup"`).
/// Used to resolve the supervisor class's actual module regardless of source
/// file path.
pub(crate) fn generate_package_outputs(
    build_dir: &Utf8Path,
    project_root: &Utf8PathBuf,
    pkg: &manifest::PackageManifest,
    hierarchy: &ClassHierarchyContext,
    outputs: &PackageBuildOutputs<'_>,
) -> Result<()> {
    let class_metadata = build_class_metadata(
        &hierarchy.pre_loaded_classes,
        &hierarchy.class_module_index,
        &pkg.name,
    );
    let alias_metadata = build_alias_metadata(outputs.source_files);

    // Generate OTP application callback when [application] supervisor is set.
    let app_callback_module =
        if let Some(ref app_config) = manifest::find_application_config(project_root)? {
            let cb_module_name = format!("beamtalk_{}_app", pkg.name);
            // Resolve the supervisor's actual Erlang module via the class index.
            // This correctly handles classes in subdirectories (e.g. src/app/app_sup.bt).
            let sup_module = hierarchy
                .class_module_index
                .get(&app_config.supervisor)
                .ok_or_else(|| {
                    miette::miette!(
                        "Cannot find compiled module for supervisor class '{}'. \
                         Ensure the class is defined in a .bt source file in this package.",
                        app_config.supervisor
                    )
                })?;
            generate_otp_app_callback(
                build_dir,
                &app_config.supervisor,
                sup_module,
                &cb_module_name,
            )?;
            info!(
                supervisor = %app_config.supervisor,
                module = %cb_module_name,
                "Generated OTP application callback"
            );
            Some(cb_module_name)
        } else {
            None
        };

    // Include the generated callback module in the .app modules list so release
    // tooling (appup generation, etc.) can account for it.
    let all_modules: Vec<String> = if let Some(ref cb) = app_callback_module {
        let mut v = outputs.module_names.to_vec();
        v.push(cb.clone());
        v
    } else {
        outputs.module_names.to_vec()
    };

    app_file::generate_app_file(
        build_dir,
        pkg,
        &all_modules,
        &class_metadata,
        app_callback_module.as_deref(),
        outputs.native_module_names,
        outputs.bt_dep_names,
        outputs.hex_dep_names,
        &alias_metadata,
    )?;
    info!(name = %pkg.name, "Generated .app file");

    // Generate per-package corpus files for MCP discovery.
    // The corpus_dir is _build/dev/ (parent of ebin/) so MCP can find it
    // alongside the build output.
    let corpus_dir = build_dir.parent().unwrap_or(build_dir);
    generate_package_corpus(
        corpus_dir,
        &pkg.name,
        &hierarchy.pre_loaded_classes,
        outputs.source_files,
    )?;

    Ok(())
}

/// Generate per-package corpus files for MCP discovery.
///
/// Produces two files in `corpus_dir`:
/// - `class_corpus.json` — class metadata (name, superclass, methods, doc)
/// - `corpus.json` — source code examples from the package's `.bt` files
///
/// These are loaded at runtime by the MCP server to augment the bundled
/// stdlib corpus with package-specific classes and examples.
pub(crate) fn generate_package_corpus(
    corpus_dir: &Utf8Path,
    package_name: &str,
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    source_files: &[Utf8PathBuf],
) -> Result<()> {
    generate_class_corpus(corpus_dir, all_class_infos)?;
    generate_example_corpus(corpus_dir, package_name, source_files)?;
    Ok(())
}

/// Generate `class_corpus.json` from parsed class metadata.
fn generate_class_corpus(
    corpus_dir: &Utf8Path,
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
) -> Result<()> {
    let class_entries: Vec<serde_json::Value> = all_class_infos
        .iter()
        .filter(|ci| !ci.is_internal) // Skip internal classes (ADR 0071)
        .map(|ci| {
            let methods: Vec<String> = ci
                .methods
                .iter()
                .chain(ci.class_methods.iter())
                .filter(|m| !m.is_internal)
                .map(|m| m.selector.to_string())
                .collect();
            serde_json::json!({
                "name": ci.name.as_str(),
                "superclass": ci.superclass.as_deref().unwrap_or("Object"),
                "doc": serde_json::Value::Null,
                "methods": methods,
                "is_sealed": ci.is_sealed,
                "is_abstract": ci.is_abstract,
            })
        })
        .collect();

    if !class_entries.is_empty() {
        let class_json = serde_json::to_string_pretty(&class_entries).into_diagnostic()?;
        let class_corpus_path = corpus_dir.join("class_corpus.json");
        fs::write(&class_corpus_path, format!("{class_json}\n"))
            .into_diagnostic()
            .wrap_err("Failed to write class_corpus.json")?;
        info!(
            path = %class_corpus_path,
            count = class_entries.len(),
            "Generated package class corpus"
        );
    }
    Ok(())
}

/// Generate `corpus.json` from package source files.
#[allow(clippy::too_many_lines)] // Entry extraction loop — splitting further would obscure the flow
fn generate_example_corpus(
    corpus_dir: &Utf8Path,
    package_name: &str,
    source_files: &[Utf8PathBuf],
) -> Result<()> {
    let mut corpus_entries: Vec<serde_json::Value> = Vec::new();
    for file in source_files {
        let Ok(source) = fs::read_to_string(file.as_std_path()) else {
            continue;
        };
        let stem = file.file_stem().unwrap_or_default();

        // Strip license header from source
        let clean_source = source
            .lines()
            .skip_while(|line| {
                let trimmed = line.trim();
                trimmed.starts_with("// Copyright")
                    || trimmed.starts_with("// SPDX")
                    || trimmed.is_empty()
            })
            .collect::<Vec<_>>()
            .join("\n")
            .trim()
            .to_string();
        if clean_source.is_empty() {
            continue;
        }

        // Extract leading doc comments as explanation
        let explanation = extract_leading_comments(&source);

        // Parse the source to extract class names for tags and title
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let mut tags: Vec<String> = Vec::new();
        let mut class_names: Vec<String> = Vec::new();
        for class in &module.classes {
            class_names.push(class.name.name.to_string());
            tags.push(class.name.name.to_string());
            if let Some(ref superclass) = class.superclass {
                tags.push(superclass.name.to_string());
            }
            for method in &class.methods {
                let name = method.selector.name();
                if !name.is_empty() {
                    tags.push(name.to_string());
                }
            }
        }
        tags.push(package_name.to_string());
        tags.sort();
        tags.dedup();

        let title = if class_names.is_empty() {
            stem.replace(['_', '-'], " ")
        } else {
            class_names.join(", ")
        };

        let id = format!(
            "pkg-{}-{}",
            package_name,
            stem.to_lowercase()
                .replace(|c: char| !c.is_alphanumeric() && c != '-', "-")
        );

        corpus_entries.push(serde_json::json!({
            "id": id,
            "title": title,
            "category": format!("package-{package_name}"),
            "tags": tags,
            "source": clean_source,
            "explanation": explanation,
        }));
    }

    if !corpus_entries.is_empty() {
        corpus_entries.sort_by(|a, b| {
            let a_id = a["id"].as_str().unwrap_or("");
            let b_id = b["id"].as_str().unwrap_or("");
            a_id.cmp(b_id)
        });
        let corpus = serde_json::json!({ "entries": corpus_entries });
        let json = serde_json::to_string_pretty(&corpus).into_diagnostic()?;
        let corpus_path = corpus_dir.join("corpus.json");
        fs::write(&corpus_path, format!("{json}\n"))
            .into_diagnostic()
            .wrap_err("Failed to write corpus.json")?;
        info!(
            path = %corpus_path,
            count = corpus_entries.len(),
            "Generated package example corpus"
        );
    }
    Ok(())
}

/// Extract leading doc comments from source, skipping license headers.
fn extract_leading_comments(source: &str) -> String {
    source
        .lines()
        .filter(|line| {
            let trimmed = line.trim();
            !trimmed.starts_with("// Copyright") && !trimmed.starts_with("// SPDX")
        })
        .skip_while(|line| line.trim().is_empty())
        .take_while(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("// ") || trimmed == "//"
        })
        .map(|line| {
            line.trim()
                .strip_prefix("// ")
                .or_else(|| line.trim().strip_prefix("//"))
                .unwrap_or("")
        })
        .collect::<Vec<_>>()
        .join(" ")
        .trim()
        .to_string()
}

/// Generate an OTP application callback module (`beamtalk_{appname}_app.erl`).
///
/// The generated module implements the OTP `application` behaviour, calling
/// the Beamtalk supervisor's `start_link` from `start/2`. It is compiled to
/// BEAM and placed alongside the other package modules in the build directory.
fn generate_otp_app_callback(
    build_dir: &Utf8Path,
    supervisor_class: &str,
    sup_module: &str,
    cb_module_name: &str,
) -> Result<()> {
    let src = format!(
        "%% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         %%\n\
         %% Generated OTP application callback.\n\
         %% Do not edit — regenerated by `beamtalk build`.\n\
         -module({cb_module_name}).\n\
         -behaviour(application).\n\
         -export([start/2, stop/1]).\n\
         \n\
         start(_Type, _Args) ->\n\
             case '{sup_module}':'start_link'() of\n\
                 {{ok, Pid}} = Ok ->\n\
                     SupTuple = {{beamtalk_supervisor, '{supervisor_class}', '{sup_module}', Pid}},\n\
                     beamtalk_supervisor:register_root(SupTuple),\n\
                     Ok;\n\
                 Err ->\n\
                     Err\n\
             end.\n\
         \n\
         stop(_State) -> ok.\n"
    );

    // Write the .erl source next to the .core files so erlc can pick it up
    let erl_path = build_dir.join(format!("{cb_module_name}.erl"));
    fs::write(&erl_path, src)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write OTP app callback '{erl_path}'"))?;

    // Compile the generated .erl with erlc
    beamtalk_cli::erlc::ErlcInvocation::new(build_dir)
        .source_file(erl_path)
        .run_status(&format!("OTP app callback '{cb_module_name}' compilation"))?;

    Ok(())
}

/// Build `.app`-file class metadata from compiled `ClassInfo` entries.
///
/// Converts the compiler's `ClassInfo` into the `ClassMetadata` structs
/// that `generate_app_file` writes into `{env, [{classes, [...]}]}`.
/// Only classes belonging to `package_name` are included.
///
/// The `kind` field is resolved using a `ClassHierarchy` that includes both
/// the user/dep classes and stdlib builtins, so `Server subclass: MyServer`
/// correctly resolves to kind `"actor"`.
pub(crate) fn build_class_metadata(
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    class_module_index: &std::collections::HashMap<String, String>,
    package_name: &str,
) -> Vec<app_file::ClassMetadata> {
    // Start from the cached stdlib hierarchy and add user/dep classes so
    // the superclass chain can be resolved across all sources.
    let mut hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(all_class_infos.to_vec());

    let module_prefix = format!("bt@{package_name}@");
    all_class_infos
        .iter()
        .filter(|ci| !ci.is_internal)
        .filter_map(|ci| {
            let module = class_module_index.get(ci.name.as_str())?;
            // Scope to this package using the module naming convention
            // bt@{package}@{class}. ClassInfo.package may be None when
            // extracted from source AST rather than BEAM metadata.
            if !module.starts_with(&module_prefix) {
                return None;
            }
            let kind = hierarchy.resolve_class_kind(&ci.name);
            Some(app_file::ClassMetadata {
                module: module.clone(),
                class_name: ci.name.to_string(),
                parent_class: ci
                    .superclass
                    .as_deref()
                    .unwrap_or("ProtoObject")
                    .to_string(),
                package: package_name.to_string(),
                kind: kind.as_str().to_string(),
                type_params: ci.type_params.iter().map(ToString::to_string).collect(),
            })
        })
        .collect()
}

/// Build `.app`-file type-alias metadata for a package (ADR 0108 Phase 8).
///
/// Independent of the incremental Pass 1 class-index cache
/// (`build_class_module_index`/`build_cache.rs`): that cache exists to skip
/// re-parsing unchanged files for `ClassInfo` extraction, but persisting
/// alias metadata across incremental runs would mean extending its on-disk
/// cache schema for a small, cheap-to-reparse surface (a package's `type`
/// declarations are typically a handful of lines total). This always
/// re-parses every file in `source_files` fresh, so `browse-type-aliases`
/// (`beamtalk_repl_ops_browse.erl`) sees a complete, correct alias list on
/// every build — including an incremental build that skipped Pass 1 for
/// files whose classes didn't change.
///
/// A file that fails to read or has parse errors contributes no aliases
/// (best-effort, matching `build_class_module_index`'s handling of unreadable
/// files) rather than failing the whole build — alias metadata is a browse-op
/// convenience, not required for compilation to succeed.
///
/// Returned in `source_files` order, **not** sorted by name —
/// [`app_file::format_type_aliases_entry`] owns the sort for deterministic
/// `.app` output, so sorting here too would be redundant.
pub(crate) fn build_alias_metadata(source_files: &[Utf8PathBuf]) -> Vec<app_file::AliasMetadata> {
    let mut result = Vec::new();
    for file in source_files {
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        for alias_def in &module.type_aliases {
            result.push(app_file::AliasMetadata {
                name: alias_def.name.name.to_string(),
                expansion: beamtalk_core::unparse::unparse_type_annotation_display(
                    &alias_def.annotation,
                ),
                doc: alias_def.doc_comment.clone(),
                source_file: to_forward_slash(file.as_str()),
                internal: alias_def.is_internal,
            });
        }
    }
    result
}
