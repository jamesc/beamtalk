// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-category `[diagnostics]` severity-override policy (ADR 0100 Rule 3).
//!
//! **DDD Context:** Compilation
//!
//! These types and functions represent the parsed `[diagnostics]` section of
//! `beamtalk.toml` — a per-category severity-override table — plus the pass
//! that applies it to a list of diagnostics. They live in `beamtalk-core`
//! (not `beamtalk-cli`, where the table was first implemented, BT-2793) so
//! that both the CLI (`beamtalk build`) and the LSP (`beamtalk-lsp`) can
//! apply the same policy without `beamtalk-lsp` depending on `beamtalk-cli`
//! (see `docs/development/architecture-principles.md` — dependencies flow
//! downward only; `beamtalk-lsp` may depend on `beamtalk-core`, never on
//! `beamtalk-cli`). BT-2800 is the surface-parity fix that moved this here.

use crate::source_analysis::{Diagnostic, DiagnosticCategory, Severity};
use std::collections::BTreeMap;

/// A per-category diagnostic severity override (ADR 0100 Rule 3).
///
/// Values map to the `[diagnostics]` table strings in `beamtalk.toml`:
/// `"off"` (drop the diagnostic entirely), `"lint"` / `"hint"` / `"warn"` /
/// `"error"` (set that [`Severity`] as the category's *base* severity for the
/// package, ahead of Rule 1's completeness-ladder default and behind
/// site-level `@expect`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverityOverride {
    /// Drop diagnostics in this category entirely — never shown, never
    /// promoted by `--warnings-as-errors`.
    Off,
    /// Style/redundancy-lint severity (suppressed outside `beamtalk lint`).
    Lint,
    /// Informational-hint severity.
    Hint,
    /// Warning severity.
    Warn,
    /// Error severity — fails the build unconditionally, independent of
    /// `--warnings-as-errors`.
    Error,
}

impl DiagnosticSeverityOverride {
    /// Parses one of the four accepted `[diagnostics]` value strings.
    /// Returns `None` on an unrecognised string — the caller attaches the
    /// offending `key` context via [`DiagnosticsTableError::InvalidSeverityForKey`].
    fn parse(s: &str) -> Option<Self> {
        match s {
            "off" => Some(Self::Off),
            "lint" => Some(Self::Lint),
            "hint" => Some(Self::Hint),
            "warn" => Some(Self::Warn),
            "error" => Some(Self::Error),
            _ => None,
        }
    }
}

/// A parsed and validated `[diagnostics]` table (ADR 0100 Rule 3): per-category
/// severity overrides, keyed by [`DiagnosticCategory`]. Empty when the
/// section is absent — absence preserves today's Rule 1 defaults.
pub type DiagnosticsTable = BTreeMap<DiagnosticCategory, DiagnosticSeverityOverride>;

/// Errors parsing a `beamtalk.toml` `[diagnostics]` table (ADR 0100 Rule 3).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum DiagnosticsTableError {
    /// The manifest text itself is not valid TOML.
    #[error("invalid TOML: {0}")]
    InvalidToml(String),
    /// `[diagnostics]` is present but is not a table.
    #[error("[diagnostics] must be a table, not a {found}")]
    NotATable {
        /// The TOML value kind actually found (e.g. `"string"`).
        found: &'static str,
    },
    /// A `[diagnostics]` key is not one of the recognised kebab-case categories.
    #[error(
        "[diagnostics] has unknown category '{key}' — expected one of: {}",
        DIAGNOSTIC_CATEGORY_KEYS.join(", ")
    )]
    UnknownCategory {
        /// The offending key as written in the manifest.
        key: String,
    },
    /// A `[diagnostics]` value is not a string.
    #[error("[diagnostics] '{key}' must be a severity string, not a {found}")]
    NotAString {
        /// The key whose value has the wrong type.
        key: String,
        /// The TOML value kind actually found (e.g. `"integer"`).
        found: &'static str,
    },
    /// A `[diagnostics]` value is a string but not one of the four accepted
    /// severities.
    #[error(
        "[diagnostics] '{key}': invalid diagnostic severity '{severity}' — expected one of \
         \"off\", \"lint\", \"hint\", \"warn\", \"error\""
    )]
    InvalidSeverityForKey {
        /// The key whose value is invalid.
        key: String,
        /// The offending severity string.
        severity: String,
    },
}

/// Maps a `[diagnostics]` table key (kebab-case) to its [`DiagnosticCategory`].
///
/// Mirrors the `Debug`-derived `PascalCase` variant names
/// (`crate::source_analysis::DiagnosticCategory`), converted to kebab-case
/// for TOML key ergonomics (e.g. `UnresolvedClass` → `unresolved-class`).
fn diagnostic_category_from_kebab(key: &str) -> Option<DiagnosticCategory> {
    Some(match key {
        "dnu" => DiagnosticCategory::Dnu,
        "type" => DiagnosticCategory::Type,
        "unused" => DiagnosticCategory::Unused,
        "empty-body" => DiagnosticCategory::EmptyBody,
        "lint" => DiagnosticCategory::Lint,
        "dead-assignment" => DiagnosticCategory::DeadAssignment,
        "extension-conflict" => DiagnosticCategory::ExtensionConflict,
        "deprecation" => DiagnosticCategory::Deprecation,
        "actor-new" => DiagnosticCategory::ActorNew,
        "visibility" => DiagnosticCategory::Visibility,
        "unresolved-class" => DiagnosticCategory::UnresolvedClass,
        "unresolved-ffi" => DiagnosticCategory::UnresolvedFfi,
        "arity-mismatch" => DiagnosticCategory::ArityMismatch,
        "shadowed-class" => DiagnosticCategory::ShadowedClass,
        "type-annotation" => DiagnosticCategory::TypeAnnotation,
        "inheritance" => DiagnosticCategory::Inheritance,
        "sendability" => DiagnosticCategory::Sendability,
        _ => return None,
    })
}

/// All valid `[diagnostics]` table keys, in the same order as
/// [`diagnostic_category_from_kebab`] — used to build the "did you mean one
/// of ..." error message for an unrecognised key.
const DIAGNOSTIC_CATEGORY_KEYS: &[&str] = &[
    "dnu",
    "type",
    "unused",
    "empty-body",
    "lint",
    "dead-assignment",
    "extension-conflict",
    "deprecation",
    "actor-new",
    "visibility",
    "unresolved-class",
    "unresolved-ffi",
    "arity-mismatch",
    "shadowed-class",
    "type-annotation",
    "inheritance",
    "sendability",
];

/// Return a human-readable TOML type name for error messages.
fn value_type_name(value: &toml::Value) -> &'static str {
    match value {
        toml::Value::String(_) => "string",
        toml::Value::Integer(_) => "integer",
        toml::Value::Float(_) => "float",
        toml::Value::Boolean(_) => "boolean",
        toml::Value::Datetime(_) => "datetime",
        toml::Value::Array(_) => "array",
        toml::Value::Table(_) => "table",
    }
}

/// Parse and validate the `[diagnostics]` section of a manifest (ADR 0100 Rule 3).
///
/// `diagnostics` is the raw TOML value of the `[diagnostics]` key, already
/// extracted from a deserialized manifest (e.g. `beamtalk-cli`'s `Manifest`
/// struct). Returns an empty table if the section is missing or empty —
/// absence preserves today's Rule 1 completeness-ladder defaults.
///
/// # Errors
///
/// Returns [`DiagnosticsTableError`] if `[diagnostics]` is present but is not
/// a table, contains a key that isn't a recognised kebab-case category, or
/// contains a value that isn't one of the four accepted severity strings
/// (`"off"`, `"lint"`, `"hint"`, `"warn"`, `"error"`).
pub fn parse_diagnostics_table(
    diagnostics: Option<&toml::Value>,
) -> Result<DiagnosticsTable, DiagnosticsTableError> {
    let Some(raw_value) = diagnostics else {
        return Ok(BTreeMap::new());
    };

    let table = raw_value
        .as_table()
        .ok_or_else(|| DiagnosticsTableError::NotATable {
            found: value_type_name(raw_value),
        })?;

    if table.is_empty() {
        return Ok(BTreeMap::new());
    }

    let mut result = DiagnosticsTable::new();
    for (key, value) in table {
        let Some(category) = diagnostic_category_from_kebab(key) else {
            return Err(DiagnosticsTableError::UnknownCategory { key: key.clone() });
        };

        let severity_str = value
            .as_str()
            .ok_or_else(|| DiagnosticsTableError::NotAString {
                key: key.clone(),
                found: value_type_name(value),
            })?;

        let severity = DiagnosticSeverityOverride::parse(severity_str).ok_or_else(|| {
            DiagnosticsTableError::InvalidSeverityForKey {
                key: key.clone(),
                severity: severity_str.to_string(),
            }
        })?;

        result.insert(category, severity);
    }

    Ok(result)
}

/// Parse the `[diagnostics]` table directly out of a `beamtalk.toml` file's
/// raw TOML content (ADR 0100 Rule 3).
///
/// Unlike [`parse_diagnostics_table`] (which takes the pre-extracted
/// `[diagnostics]` sub-value from an already-deserialized manifest struct),
/// this parses the *whole* manifest text itself and pulls out just the
/// `[diagnostics]` key. This is the entry point for callers — like the LSP —
/// that have no `Manifest`/`PackageManifest` struct of their own to
/// deserialize into and must not depend on `beamtalk-cli` just to read one
/// section of `beamtalk.toml`.
///
/// # Errors
///
/// Returns [`DiagnosticsTableError`] if `manifest_toml` is not valid TOML, or
/// if its `[diagnostics]` section fails validation for any of the reasons
/// documented on [`parse_diagnostics_table`].
pub fn parse_diagnostics_table_from_manifest_toml(
    manifest_toml: &str,
) -> Result<DiagnosticsTable, DiagnosticsTableError> {
    let value: toml::Value = toml::from_str(manifest_toml)
        .map_err(|e: toml::de::Error| DiagnosticsTableError::InvalidToml(e.to_string()))?;
    parse_diagnostics_table(value.get("diagnostics"))
}

/// Parse `[package] name` directly out of a `beamtalk.toml` file's raw TOML
/// content (BT-2960), for the same reason
/// [`parse_diagnostics_table_from_manifest_toml`] exists: the LSP has no
/// `Manifest`/`PackageManifest` struct of its own to deserialize into and
/// must not depend on `beamtalk-cli` just to read one field of
/// `beamtalk.toml`.
///
/// Returns `None` for invalid TOML, a missing `[package]` table, or a
/// missing/non-string `name` key — every case is a "no known package name
/// for this root" signal to the caller, not a hard error; `beamtalk-lsp`
/// must keep working with the `CURRENT_PROJECT_PACKAGE_MARKER` fallback for
/// a workspace root with a malformed or absent manifest.
#[must_use]
pub fn parse_package_name_from_manifest_toml(manifest_toml: &str) -> Option<String> {
    let value: toml::Value = toml::from_str(manifest_toml).ok()?;
    value
        .get("package")?
        .get("name")?
        .as_str()
        .map(str::to_string)
}

/// Apply a package's `[diagnostics]` severity-override table (ADR 0100 Rule 3)
/// to a list of diagnostics.
///
/// For each diagnostic whose category has a table entry: `"off"` drops the
/// diagnostic; `"lint"` / `"hint"` / `"warn"` / `"error"` rewrite its
/// `severity` in place, becoming the category's *base* severity for the
/// package. Diagnostics with no category, or whose category has no table
/// entry, pass through unchanged — an empty table (no manifest, or no
/// `[diagnostics]` section) is a complete no-op, preserving today's Rule 1
/// defaults.
///
/// **Severity floor:** a diagnostic that already carries `Severity::Error`
/// is never touched by the table, even if its category has an entry. Rule 3
/// is an *escalation* mechanism for the soft, open-world diagnostics the
/// completeness ladder (Rule 1) produces (`Hint`/`Warning`) — it is not a
/// blanket switch that can quietly turn a hard structural compile error
/// (e.g. `ActorNew`, `Inheritance`, `EmptyBody`) into a passing build.
///
/// Must run after `@expect` suppression and before the
/// `--warnings-as-errors` promotion pass, which is a *final* pass over
/// whatever this step resolves to (ADR 0100 Rule 3).
#[must_use]
pub fn apply_diagnostics_table(
    diagnostics: Vec<Diagnostic>,
    table: &DiagnosticsTable,
) -> Vec<Diagnostic> {
    if table.is_empty() {
        return diagnostics;
    }

    diagnostics
        .into_iter()
        .filter_map(|mut diagnostic| {
            let Some(category) = diagnostic.category else {
                return Some(diagnostic);
            };
            // Severity floor: a diagnostic that already arrived as `Error`
            // (e.g. `ActorNew` — BT-1524's "Actor subclass must use spawn,
            // not new" — or `Inheritance` / `EmptyBody` hard-error checks) is
            // never a Rule 1 completeness-ladder soft diagnostic; it's a
            // structural compile error unrelated to open-world uncertainty.
            // ADR 0100 Rule 3 frames the table as opt-in *escalation* of soft
            // diagnostics, not silent de-escalation of hard ones — a `warn`
            // or `off` entry for one of these categories must not quietly
            // turn a guaranteed compile error into a passing build.
            if diagnostic.severity == Severity::Error {
                return Some(diagnostic);
            }
            match table.get(&category) {
                None => Some(diagnostic),
                Some(DiagnosticSeverityOverride::Off) => None,
                Some(DiagnosticSeverityOverride::Lint) => {
                    diagnostic.severity = Severity::Lint;
                    Some(diagnostic)
                }
                Some(DiagnosticSeverityOverride::Hint) => {
                    diagnostic.severity = Severity::Hint;
                    Some(diagnostic)
                }
                Some(DiagnosticSeverityOverride::Warn) => {
                    diagnostic.severity = Severity::Warning;
                    Some(diagnostic)
                }
                Some(DiagnosticSeverityOverride::Error) => {
                    diagnostic.severity = Severity::Error;
                    Some(diagnostic)
                }
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Span;

    fn dnu_diagnostic(severity: Severity) -> Diagnostic {
        let mut d = Diagnostic::warning("test dnu", Span::new(0, 1));
        d.severity = severity;
        d.category = Some(DiagnosticCategory::Dnu);
        d
    }

    #[test]
    fn parse_diagnostics_table_absent_is_empty() {
        let table = parse_diagnostics_table(None).unwrap();
        assert!(table.is_empty());
    }

    #[test]
    fn parse_diagnostics_table_all_categories() {
        let toml_str = r#"
dnu = "hint"
type = "hint"
unused = "warn"
empty-body = "error"
lint = "off"
dead-assignment = "warn"
extension-conflict = "error"
deprecation = "warn"
actor-new = "error"
visibility = "error"
unresolved-class = "warn"
unresolved-ffi = "warn"
arity-mismatch = "warn"
shadowed-class = "warn"
type-annotation = "hint"
inheritance = "error"
sendability = "hint"
"#;
        let value: toml::Value = toml::from_str(toml_str).unwrap();
        let table = parse_diagnostics_table(Some(&value)).unwrap();
        assert_eq!(table.len(), DIAGNOSTIC_CATEGORY_KEYS.len());
        assert_eq!(
            table[&DiagnosticCategory::Dnu],
            DiagnosticSeverityOverride::Hint
        );
        assert_eq!(
            table[&DiagnosticCategory::Inheritance],
            DiagnosticSeverityOverride::Error
        );
    }

    #[test]
    fn parse_diagnostics_table_rejects_unknown_category() {
        let value: toml::Value = toml::from_str("not-a-category = \"warn\"").unwrap();
        let err = parse_diagnostics_table(Some(&value)).unwrap_err();
        assert!(
            err.to_string().contains("unknown category"),
            "should mention unknown category: {err}"
        );
    }

    #[test]
    fn parse_diagnostics_table_rejects_invalid_severity() {
        let value: toml::Value = toml::from_str("dnu = \"critical\"").unwrap();
        let err = parse_diagnostics_table(Some(&value)).unwrap_err();
        assert!(
            err.to_string().contains("invalid diagnostic severity"),
            "should mention invalid severity: {err}"
        );
    }

    #[test]
    fn parse_diagnostics_table_rejects_non_string_value() {
        let value: toml::Value = toml::from_str("dnu = 42").unwrap();
        let err = parse_diagnostics_table(Some(&value)).unwrap_err();
        assert!(
            err.to_string().contains("severity string"),
            "should mention expected string type: {err}"
        );
    }

    #[test]
    fn parse_diagnostics_table_from_manifest_toml_extracts_section() {
        let manifest = r#"
[package]
name = "my_app"
version = "0.1.0"

[diagnostics]
dnu = "error"
"#;
        let table = parse_diagnostics_table_from_manifest_toml(manifest).unwrap();
        assert_eq!(table.len(), 1);
        assert_eq!(
            table[&DiagnosticCategory::Dnu],
            DiagnosticSeverityOverride::Error
        );
    }

    #[test]
    fn parse_diagnostics_table_from_manifest_toml_no_section_is_empty() {
        let manifest = "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n";
        let table = parse_diagnostics_table_from_manifest_toml(manifest).unwrap();
        assert!(table.is_empty());
    }

    #[test]
    fn parse_diagnostics_table_from_manifest_toml_rejects_invalid_toml() {
        let err = parse_diagnostics_table_from_manifest_toml("not [ valid toml").unwrap_err();
        assert!(
            matches!(err, DiagnosticsTableError::InvalidToml(_)),
            "expected InvalidToml, got {err:?}"
        );
    }

    #[test]
    fn parse_package_name_from_manifest_toml_extracts_name() {
        let manifest = "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n";
        assert_eq!(
            parse_package_name_from_manifest_toml(manifest),
            Some("my_app".to_string())
        );
    }

    #[test]
    fn parse_package_name_from_manifest_toml_missing_package_table_is_none() {
        assert_eq!(
            parse_package_name_from_manifest_toml("[diagnostics]\ndnu = \"error\"\n"),
            None
        );
    }

    #[test]
    fn parse_package_name_from_manifest_toml_non_string_name_is_none() {
        assert_eq!(
            parse_package_name_from_manifest_toml("[package]\nname = 42\n"),
            None
        );
    }

    #[test]
    fn parse_package_name_from_manifest_toml_invalid_toml_is_none() {
        assert_eq!(
            parse_package_name_from_manifest_toml("not [ valid toml"),
            None
        );
    }

    #[test]
    fn apply_diagnostics_table_empty_table_is_noop() {
        let diags = vec![dnu_diagnostic(Severity::Hint)];
        let table = DiagnosticsTable::new();
        let result = apply_diagnostics_table(diags.clone(), &table);
        assert_eq!(result.len(), diags.len());
        assert_eq!(result[0].severity, Severity::Hint);
    }

    #[test]
    fn apply_diagnostics_table_off_drops_diagnostic() {
        let diags = vec![dnu_diagnostic(Severity::Hint)];
        let mut table = DiagnosticsTable::new();
        table.insert(DiagnosticCategory::Dnu, DiagnosticSeverityOverride::Off);
        let result = apply_diagnostics_table(diags, &table);
        assert!(result.is_empty());
    }

    #[test]
    fn apply_diagnostics_table_error_promotes_severity() {
        let diags = vec![dnu_diagnostic(Severity::Hint)];
        let mut table = DiagnosticsTable::new();
        table.insert(DiagnosticCategory::Dnu, DiagnosticSeverityOverride::Error);
        let result = apply_diagnostics_table(diags, &table);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].severity, Severity::Error);
    }

    #[test]
    fn apply_diagnostics_table_never_touches_existing_hard_error() {
        // Severity floor: a hard structural Error (e.g. ActorNew) must never
        // be de-escalated by a table entry, even "off".
        let diags = vec![dnu_diagnostic(Severity::Error)];
        let mut table = DiagnosticsTable::new();
        table.insert(DiagnosticCategory::Dnu, DiagnosticSeverityOverride::Off);
        let result = apply_diagnostics_table(diags, &table);
        assert_eq!(result.len(), 1, "hard Error must survive an 'off' entry");
        assert_eq!(result[0].severity, Severity::Error);
    }
}
