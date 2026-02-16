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
}

/// Find all `.bt` source files in a path.
pub(super) fn find_source_files(path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();

    if path.is_file() {
        if path.extension() == Some("bt") {
            files.push(path.to_path_buf());
        } else {
            miette::bail!("File '{}' is not a .bt source file", path);
        }
    } else if path.is_dir() {
        for entry in fs::read_dir(path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read directory '{path}'"))?
        {
            let entry = entry.into_diagnostic()?;
            let entry_path = Utf8PathBuf::from_path_buf(entry.path())
                .map_err(|_| miette::miette!("Non-UTF-8 path"))?;

            if entry_path.extension() == Some("bt") {
                files.push(entry_path);
            }
        }
    } else {
        miette::bail!("Path '{}' does not exist", path);
    }

    files.sort();
    Ok(files)
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
            source[..offset].lines().count() + 1
        };
        MethodInfo {
            signature: format_signature(&m.selector, &m.parameters),
            doc_comment: m.doc_comment.clone(),
            line_number: Some(line_number),
        }
    };

    let methods = class.methods.iter().map(&make_method_info).collect();
    let class_methods = class.class_methods.iter().map(&make_method_info).collect();

    Ok(Some(ClassInfo {
        name: class.name.name.to_string(),
        superclass: class.superclass.as_ref().map(|s| s.name.to_string()),
        doc_comment: class.doc_comment.clone(),
        methods,
        class_methods,
        source_file,
        source_root: Some(root.as_str().to_string()),
    }))
}

/// Format a method signature for display.
pub(super) fn format_signature(
    selector: &beamtalk_core::ast::MessageSelector,
    parameters: &[beamtalk_core::ast::ParameterDefinition],
) -> String {
    use beamtalk_core::ast::MessageSelector;
    match selector {
        MessageSelector::Unary(name) => name.to_string(),
        MessageSelector::Binary(op) => {
            if let Some(param) = parameters.first() {
                format!("{op} {}", param.name.name)
            } else {
                op.to_string()
            }
        }
        MessageSelector::Keyword(parts) => {
            let mut sig = String::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    sig.push(' ');
                }
                sig.push_str(&part.keyword);
                if let Some(param) = parameters.get(i) {
                    sig.push(' ');
                    sig.push_str(&param.name.name);
                }
            }
            sig
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
