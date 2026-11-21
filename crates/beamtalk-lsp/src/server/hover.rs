// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Hover documentation formatting.

use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_language_service::{HoverInfo, SimpleLanguageService};

/// Formats hover documentation for compact display in editor hovers.
///
/// VS Code controls hover font sizes, so we demote markdown heading levels
/// to reduce visual dominance of large section titles like `## Examples`.
pub(in crate::server) fn format_hover_documentation(doc: &str) -> String {
    doc.lines()
        .map(|line| {
            if let Some(rest) = line.strip_prefix("### ") {
                return format!("##### {rest}");
            }
            if let Some(rest) = line.strip_prefix("## ") {
                return format!("#### {rest}");
            }
            if let Some(rest) = line.strip_prefix("# ") {
                return format!("### {rest}");
            }
            line.to_string()
        })
        .collect::<Vec<_>>()
        .join("\n")
}
/// Adds an LSP-only stdlib profile note for class/method hovers.
///
/// This is intentionally editor policy (not compiler behavior): when hover
/// resolves to a built-in/stdlib class, show sealed/abstract traits so users
/// can better understand completion/diagnostic confidence. The class comes
/// from the hover's structured `class_name`, not from its rendered markdown.
pub(in crate::server) fn stdlib_hover_policy_note(
    service: &SimpleLanguageService,
    hover: &HoverInfo,
) -> Option<String> {
    let class_name = hover.class_name.as_deref()?;
    if !ClassHierarchy::is_builtin_class(class_name) {
        return None;
    }

    let class = service.project_index().hierarchy().get_class(class_name)?;
    let mut traits = Vec::new();
    if class.is_sealed {
        traits.push("sealed");
    }
    if class.is_abstract {
        traits.push("abstract");
    }

    if traits.is_empty() {
        return None;
    }

    let mut lines = vec![format!(
        "**Stdlib Profile:** `{class_name}` is {}.",
        traits.join(" + ")
    )];
    if class.is_sealed {
        lines.push("- Method surface is closed to subclass overrides.".to_string());
    }
    if class.is_abstract {
        lines.push("- Defines protocol to be implemented by concrete subclasses.".to_string());
    }
    if class.is_sealed && hover.method_resolved {
        lines.push("- Confidence: high (static, sealed stdlib dispatch).".to_string());
    }

    Some(lines.join("\n"))
}

// --- Type conversion helpers ---
