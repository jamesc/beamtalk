// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source file discovery and class/method info extraction.
//!
//! **DDD Context:** CLI / Documentation

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use tracing::{debug, warn};

/// Information about a documented class.
pub struct ClassInfo {
    /// The class name (e.g., `Counter`).
    pub name: String,
    /// The superclass name, if any (e.g., `Actor`).
    pub superclass: Option<String>,
    /// Whether this is a sealed class (cannot be subclassed).
    pub is_sealed: bool,
    /// Whether this is an abstract class (cannot be instantiated).
    pub is_abstract: bool,
    /// The class-level doc comment extracted from source.
    pub doc_comment: Option<String>,
    /// Instance methods defined on the class.
    pub methods: Vec<MethodInfo>,
    /// Class-side methods defined on the class.
    pub class_methods: Vec<MethodInfo>,
    pub source_file: Option<String>,
    pub source_root: Option<String>,
}

/// Information about a documented method.
pub struct MethodInfo {
    /// The formatted method signature (e.g., `at: index put: value`).
    pub signature: String,
    /// The method-level doc comment extracted from source.
    pub doc_comment: Option<String>,
    pub line_number: Option<usize>,
    /// Whether this method is sealed (cannot be overridden).
    pub is_sealed: bool,
}

/// Find all `.bt` source files in a path, recursing into subdirectories.
///
/// Matches `build::find_source_files`, so `beamtalk doc` documents every class
/// that `beamtalk build` compiles — including classes in `src/` subdirectories
/// (user packages) or `stdlib/src/` subdirectories.
///
/// Build/VCS directories are excluded (`FileWalker::source_files()`, BT-3043)
/// — `build::find_source_files` avoids them by preferring the package's
/// `src/` subdirectory, but `doc` walks whatever path it is handed, so
/// pointing it at a package root would otherwise pull in every dependency's
/// classes from `_build/deps/`.
pub(super) fn find_source_files(path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    beamtalk_core::file_walker::FileWalker::source_files().walk(path)
}

/// Parse a `.bt` source file and extract class documentation info.
pub(super) fn parse_class_info(root: &Utf8Path, path: &Utf8Path) -> Result<Option<ClassInfo>> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        warn!("Skipping '{}': parse errors detected", path);
        return Ok(None);
    }

    let Some(class) = module.classes.first() else {
        debug!("No class definition in '{}'", path);
        return Ok(None);
    };

    let source_file = path
        .strip_prefix(root)
        .ok()
        .map(|p| p.as_str().to_string())
        .or_else(|| path.file_name().map(String::from));

    let make_method_info = |m: &beamtalk_core::ast::MethodDefinition| {
        let line_number = {
            let offset = m.span.start() as usize;
            source[..offset].lines().count()
        };
        MethodInfo {
            signature: format_signature(&m.selector, &m.parameters),
            doc_comment: m.doc_comment.clone(),
            line_number: Some(line_number),
            is_sealed: m.is_sealed,
        }
    };

    let methods = class.methods.iter().map(&make_method_info).collect();
    let class_methods = class.class_methods.iter().map(&make_method_info).collect();

    Ok(Some(ClassInfo {
        name: class.name.name.to_string(),
        superclass: class.superclass.as_ref().map(|s| s.name.to_string()),
        is_sealed: class.is_sealed,
        is_abstract: class.is_abstract,
        doc_comment: class.doc_comment.clone(),
        methods,
        class_methods,
        source_file,
        source_root: Some(root.as_str().to_string()),
    }))
}

/// Format a method signature for display.
///
/// Names only, no types, no return arrow — `beamtalk doc`'s listing style is
/// intentionally less detailed than hover/stub generation (BT-3097): it
/// composes through the shared
/// [`beamtalk_core::unparse::render_signature_text`] core with
/// [`beamtalk_core::unparse::SignatureRenderOptions::NAMES_ONLY`], the same
/// composer every other signature-text consumer in the codebase now shares.
pub(super) fn format_signature(
    selector: &beamtalk_core::ast::MessageSelector,
    parameters: &[beamtalk_core::ast::ParameterDefinition],
) -> String {
    use beamtalk_core::ast::MessageSelector;
    use beamtalk_core::unparse::{
        SignatureParam, SignatureRenderOptions, SignatureSelector, render_signature_text,
    };

    match selector {
        MessageSelector::Unary(name) => render_signature_text(
            SignatureSelector::Unary(name),
            None,
            &SignatureRenderOptions::NAMES_ONLY,
        ),
        MessageSelector::Binary(op) => {
            let params = [SignatureParam {
                keyword: op,
                name: parameters.first().map(|p| p.name.name.as_str()),
                type_text: None,
            }];
            render_signature_text(
                SignatureSelector::Params(&params),
                None,
                &SignatureRenderOptions::NAMES_ONLY,
            )
        }
        MessageSelector::Keyword(parts) => {
            let params: Vec<SignatureParam<'_>> = parts
                .iter()
                .enumerate()
                .map(|(i, part)| SignatureParam {
                    keyword: &part.keyword,
                    name: parameters.get(i).map(|p| p.name.name.as_str()),
                    type_text: None,
                })
                .collect();
            render_signature_text(
                SignatureSelector::Params(&params),
                None,
                &SignatureRenderOptions::NAMES_ONLY,
            )
        }
    }
}

/// Collect inherited methods by walking the class hierarchy.
///
/// Includes cycle detection to prevent infinite loops from malformed hierarchies.
pub(super) fn collect_inherited_methods<'a>(
    class: &ClassInfo,
    hierarchy: &'a HashMap<String, String>,
    methods_by_class: &'a HashMap<String, &'a ClassInfo>,
) -> Vec<(&'a str, &'a [MethodInfo])> {
    let mut inherited = Vec::new();
    let mut visited = HashSet::new();

    let Some(ref superclass_name) = class.superclass else {
        return inherited;
    };

    // Walk from superclass upward
    let mut current: Option<&'a String> = hierarchy
        .keys()
        .find(|k| k.as_str() == superclass_name.as_str());

    // If superclass not in hierarchy keys, try looking it up in methods_by_class directly
    if current.is_none() {
        if let Some((key, parent)) = methods_by_class.get_key_value(superclass_name.as_str()) {
            if !parent.methods.is_empty() {
                inherited.push((key.as_str(), parent.methods.as_slice()));
            }
        }
        return inherited;
    }

    while let Some(parent_name) = current {
        if !visited.insert(parent_name.as_str()) {
            break; // cycle detected
        }
        if let Some(parent) = methods_by_class.get(parent_name.as_str()) {
            if !parent.methods.is_empty() {
                inherited.push((parent_name.as_str(), parent.methods.as_slice()));
            }
        }
        current = hierarchy.get(parent_name.as_str());
    }

    inherited
}
