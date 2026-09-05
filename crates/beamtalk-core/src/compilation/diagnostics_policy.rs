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

use crate::ast::{ExpectCategory, Expression, ExpressionStatement, Module};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Severity, Span};
use ecow::EcoString;
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
        "native-declaration-location" => DiagnosticCategory::NativeDeclarationLocation,
        "file-class-name-mismatch" => DiagnosticCategory::FileClassNameMismatch,
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
    "native-declaration-location",
    "file-class-name-mismatch",
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

/// Load and parse the `[diagnostics]` severity-override table from
/// `<root>/beamtalk.toml` (ADR 0100 Rule 3).
///
/// Lenient by design: a root with no `beamtalk.toml`, or one whose manifest
/// fails to parse, yields an empty table (Rule 1 defaults) — a malformed
/// manifest already fails loudly at `beamtalk build` time. Non-`NotFound` I/O
/// errors and parse failures are logged at `WARN` so the mismatch is
/// discoverable without blocking compilation or diagnostics.
///
/// This is the shared per-root loader for both `beamtalk-compiler-port` and
/// `beamtalk-lsp`, which each previously reimplemented the same pattern.
#[must_use]
pub fn load_diagnostics_table_for_root(root: &std::path::Path) -> DiagnosticsTable {
    let manifest_path = root.join("beamtalk.toml");
    let content = match std::fs::read_to_string(&manifest_path) {
        Ok(c) => c,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return DiagnosticsTable::new(),
        Err(e) => {
            tracing::warn!(
                path = %manifest_path.display(),
                error = %e,
                "could not read beamtalk.toml for [diagnostics] overrides; \
                 using Rule 1 defaults for this root"
            );
            return DiagnosticsTable::new();
        }
    };
    match parse_diagnostics_table_from_manifest_toml(&content) {
        Ok(table) => table,
        Err(e) => {
            tracing::warn!(
                path = %manifest_path.display(),
                error = %e,
                "failed to parse [diagnostics] table in beamtalk.toml; \
                 using Rule 1 defaults for this root"
            );
            DiagnosticsTable::new()
        }
    }
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

// BT-3361 (ADR 0117 Decision step 5): `compute_diagnostics`,
// `compute_diagnostics_with_known_vars`, `apply_expect_directives`, and its
// private helpers moved here from `beamtalk-language-service`'s
// `queries::diagnostic_provider` (which re-exports them under their
// original names/paths for existing call sites). They were always pure
// Compilation-context diagnostics post-processing — like
// `apply_diagnostics_table` above, their new neighbor — with zero
// Language-Service-specific types (no `Position`, `Completion`, ...); moving
// them here is what lets `beamtalk-core`'s own extensive
// `semantic_analysis::type_checker` unit-test suite (67+ call sites across
// its `tests/` submodule) keep computing full-pipeline `@expect`-suppressed
// diagnostics without `beamtalk-core` taking a dev-dependency on
// `beamtalk-language-service` — which would be a cyclic self-dependency for
// its own unit tests specifically (unlike the crate's existing self-`path`
// dev-dependency for `test_support`, which only integration tests under
// `tests/` exercise — see that entry's own comment in `Cargo.toml` — a unit
// test compiles as part of the same `--cfg test` compilation as the library
// itself, so an external crate it depends on would link a *different*,
// non-test-cfg build of this same library, and the two builds' otherwise-
// identical types are not interchangeable to rustc).

/// Computes diagnostics for a module.
///
/// This runs both parse-time and semantic analysis diagnostics.
///
/// # Arguments
///
/// * `module` - The parsed AST
/// * `parse_diagnostics` - Diagnostics from the parser
///
/// # Returns
///
/// A list of all diagnostics (errors and warnings).
///
/// # Examples
///
/// ```
/// use beamtalk_core::compilation::diagnostics_policy::compute_diagnostics;
/// use beamtalk_core::source_analysis::{lex_with_eof, parse};
///
/// let source = "x := 42";
/// let tokens = lex_with_eof(source);
/// let (module, parse_diags) = parse(tokens);
///
/// let diagnostics = compute_diagnostics(&module, parse_diags);
/// assert!(diagnostics.is_empty()); // Valid code has no errors
/// ```
#[must_use]
pub fn compute_diagnostics(module: &Module, parse_diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
    compute_diagnostics_with_known_vars(module, parse_diagnostics, &[])
}

/// Computes diagnostics with pre-defined REPL variables.
///
/// Variables in `known_vars` are treated as already defined, preventing
/// "Undefined variable" errors for REPL session variables.
#[must_use]
pub fn compute_diagnostics_with_known_vars(
    module: &Module,
    parse_diagnostics: Vec<Diagnostic>,
    known_vars: &[&str],
) -> Vec<Diagnostic> {
    let mut all_diagnostics = parse_diagnostics;

    // Run semantic analysis with known variables
    let analysis_result = crate::semantic_analysis::analyse_full(
        module,
        crate::semantic_analysis::AnalysisContext::default().with_known_vars(known_vars),
    );
    all_diagnostics.extend(analysis_result.diagnostics);

    apply_expect_directives(module, &mut all_diagnostics);

    all_diagnostics
}

/// Diagnostic categories that `beamtalk lint`'s dedicated lint passes
/// (`beamtalk_lint::run_lint_passes`) can produce for a shape `analyse_full`'s
/// semantic analysis itself never checks (BT-3384).
///
/// `DiagnosticCategory::DeadAssignment` is **not** exclusively lint-pass-only:
/// `analyse_full`'s own `warn_assignment_in_match_arms`
/// (`semantic_analysis/validators/match_validators.rs`) also produces it, for
/// an assignment as the direct body of a `match:` arm — every caller of this
/// module already runs that check, lint pass or not. The only shape genuinely
/// unreachable without `run_lint_passes` is `beamtalk-lint`'s
/// `DeadBlockAssignmentPass` (BT-3385): a captured outer local reassigned
/// inside a block literal that isn't passed to a selector the compiler's
/// state-threading recognizes — and that shape, by construction, always
/// contains a block literal. [`expect_category_unchecked`] uses this list
/// together with each directive's own `contains_block` flag (not the
/// category alone) so a plain match-arm `@expect dead_assignment` — which has
/// no block literal anywhere near it — stays subject to normal staleness
/// checking even under [`apply_expect_directives_excluding_lint_only`].
///
/// `beamtalk build`/`beamtalk test`, the LSP, and the REPL never call
/// `run_lint_passes`, so a `DeadBlockAssignmentPass` diagnostic can never
/// appear in the diagnostics list they hand to `apply_expect_directives`, and
/// an `@expect` suppressing one would always look "stale" from their point of
/// view even when `beamtalk lint` genuinely still needs it — there is no
/// state of the source that could satisfy both tools' staleness checks
/// simultaneously otherwise. [`apply_expect_directives_excluding_lint_only`]
/// is the staleness entry point for every pipeline that does not run lint
/// passes; only `beamtalk lint` itself (and any other caller that actually
/// calls `run_lint_passes` first, e.g. the MCP server's own lint-equivalent
/// path) should keep calling plain [`apply_expect_directives`], which
/// validates every category unconditionally.
const LINT_PASS_ONLY_CATEGORIES: &[DiagnosticCategory] = &[DiagnosticCategory::DeadAssignment];

/// Returns `true` if this specific directive's staleness cannot be evaluated
/// because the check that could confirm or refute it was never run in this
/// invocation (BT-3384).
///
/// `unchecked` names the categories this invocation didn't run lint passes
/// for (see [`LINT_PASS_ONLY_CATEGORIES`]); `contains_block` says whether
/// *this directive's own target expression* contains a block literal — the
/// only shape `beamtalk-lint`'s `DeadBlockAssignmentPass` can apply to. A
/// `dead_assignment` directive is only "unchecked" when both hold: the
/// category is lint-pass-only for this invocation, AND the target could
/// plausibly be the lint-only shape. A target with no block literal (e.g. a
/// bare match-arm assignment) can only ever be `analyse_full`'s own
/// match-arm check, which every caller already runs — so it's never
/// "unchecked", and an unmatched one is correctly reported stale.
///
/// `all` is deliberately exempted regardless of `contains_block`: narrowing
/// it here would silently defang staleness checking for *every* `@expect
/// all` in a build, not just the ones that happen to depend on a lint-only
/// category — BT-3384's fix stays scoped to the specific categories that are
/// genuinely lint-only.
///
/// This match is intentionally exhaustive (no `_` arm): adding a new
/// [`ExpectCategory`] variant is a compile error here until this function
/// says whether it's lint-only, so that fact can never silently drift from
/// [`LINT_PASS_ONLY_CATEGORIES`].
fn expect_category_unchecked(
    expect_cat: ExpectCategory,
    unchecked: &[DiagnosticCategory],
    contains_block: bool,
) -> bool {
    if unchecked.is_empty() {
        return false;
    }
    match expect_cat {
        ExpectCategory::DeadAssignment => {
            contains_block && unchecked.contains(&DiagnosticCategory::DeadAssignment)
        }
        // Every other category (including `all`, deliberately — see this
        // function's doc) is produced by `analyse_full`'s semantic analysis,
        // which every caller of this function always runs, so none of them
        // can ever be "unchecked".
        ExpectCategory::All
        | ExpectCategory::Dnu
        | ExpectCategory::Type
        | ExpectCategory::Unused
        | ExpectCategory::Deprecation
        | ExpectCategory::ActorNew
        | ExpectCategory::Visibility
        | ExpectCategory::UnresolvedClass
        | ExpectCategory::UnresolvedFfi
        | ExpectCategory::ArityMismatch
        | ExpectCategory::ShadowedClass
        | ExpectCategory::TypeAnnotation
        | ExpectCategory::Inheritance
        | ExpectCategory::Sendability => false,
    }
}

/// Applies `@expect` directives to suppress matching diagnostics.
///
/// For each `@expect category` directive in the module, any diagnostic
/// whose span is contained within the *following* expression's span and
/// whose category matches is removed from `diagnostics`. If no matching
/// diagnostic is found, the directive itself becomes an error ("stale @expect").
///
/// This is called by both the language service (LSP/diagnostic provider) and
/// the CLI compiler after all diagnostics have been collected.
///
/// Validates staleness for every category. Only appropriate for a pipeline
/// that has actually run `beamtalk_lint::run_lint_passes` (`beamtalk lint`,
/// and the MCP server's lint-equivalent path) — every other caller should use
/// [`apply_expect_directives_excluding_lint_only`] instead (BT-3384).
pub fn apply_expect_directives(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    apply_expect_directives_impl(module, diagnostics, &[]);
}

/// Like [`apply_expect_directives`], but an `@expect` directive whose
/// category is produced only by `beamtalk lint`'s dedicated lint passes (see
/// [`LINT_PASS_ONLY_CATEGORIES`]) is neither validated as stale nor treated
/// as satisfied — silently left alone — because this invocation never ran
/// the check that could confirm or refute it (BT-3384).
///
/// Use this from any pipeline that does not call
/// `beamtalk_lint::run_lint_passes` before checking staleness: `beamtalk
/// build`/`beamtalk test` and the LSP (`compute_project_diagnostics_with_analysis`),
/// and the REPL (`run_diagnostic_pipeline`).
pub fn apply_expect_directives_excluding_lint_only(
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
) {
    apply_expect_directives_impl(module, diagnostics, LINT_PASS_ONLY_CATEGORIES);
}

/// A collected `@expect` directive: its (possibly multi-category, BT-3387)
/// category list, optional reason, the directive's own span (for stale
/// warnings), the span of the expression/declaration it targets (for
/// diagnostic matching), and whether that target contains a block literal
/// (see [`expect_category_unchecked`]).
type ExpectDirectiveEntry = (Vec<ExpectCategory>, Option<EcoString>, Span, Span, bool);

/// A stale `@expect` directive pending its warning: category list, optional
/// reason, and the directive's own span.
type StaleExpectEntry = (Vec<ExpectCategory>, Option<EcoString>, Span);

fn apply_expect_directives_impl(
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
    unchecked_categories: &[DiagnosticCategory],
) {
    let mut directives: Vec<ExpectDirectiveEntry> = Vec::new();

    collect_directives_from_exprs(&module.expressions, &mut directives);
    for class in &module.classes {
        // BT-1856: Collect declaration-level @expect from state declarations.
        // directive_span = the @expect token span (for stale warnings),
        // target_span = the declaration span (for matching diagnostics).
        for state_decl in class.state.iter().chain(class.class_variables.iter()) {
            if let Some((ref cats, ref reason, expect_span)) = state_decl.expect {
                let contains_block = state_decl
                    .default_value
                    .as_ref()
                    .is_some_and(expression_contains_block);
                directives.push((
                    cats.clone(),
                    reason.clone(),
                    expect_span,
                    state_decl.span,
                    contains_block,
                ));
            }
        }
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            // BT-1856: Collect declaration-level @expect from method declarations.
            //
            // `contains_block` here is deliberately whole-method, matching a
            // declaration-level `@expect`'s own suppression scope (it covers
            // a matching diagnostic ANYWHERE in the method, not just an
            // adjacent statement — unlike a statement-level `@expect`, which
            // has no "whole body" to fall back to). This means an unrelated
            // block literal elsewhere in the same method (e.g. a `do:` loop)
            // can make a stale, block-free `@expect dead_assignment` (e.g.
            // one covering only a match-arm assignment) go unflagged by
            // `build`/`test`/LSP/REPL — `beamtalk lint` still catches it
            // regardless, and no real diagnostic is ever wrongly suppressed
            // either way. Accepted as a low-impact miss on the "you can
            // remove this now-unnecessary pragma" warning rather than
            // narrowing a method-level directive's target below what it
            // actually covers.
            if let Some((ref cats, ref reason, expect_span)) = method.expect {
                directives.push((
                    cats.clone(),
                    reason.clone(),
                    expect_span,
                    method.span,
                    exprs_contain_block(&method.body),
                ));
            }
            collect_directives_from_exprs(&method.body, &mut directives);
        }
    }
    for standalone in &module.method_definitions {
        // Same whole-method `contains_block` scoping as above, and the same
        // tradeoff — see that loop's comment.
        if let Some((ref cats, ref reason, expect_span)) = standalone.method.expect {
            directives.push((
                cats.clone(),
                reason.clone(),
                expect_span,
                standalone.method.span,
                exprs_contain_block(&standalone.method.body),
            ));
        }
        collect_directives_from_exprs(&standalone.method.body, &mut directives);
    }

    if directives.is_empty() {
        return;
    }

    let mut suppressed_indices: Vec<usize> = Vec::new();
    let mut stale_directives: Vec<StaleExpectEntry> = Vec::new();

    for (cats, reason, directive_span, target_span, contains_block) in &directives {
        let mut matched = false;
        for (i, diag) in diagnostics.iter().enumerate() {
            if target_span.contains(diag.span)
                && cats.iter().any(|cat| category_matches(*cat, diag.category))
            {
                suppressed_indices.push(i);
                matched = true;
            }
        }
        // BT-3387: a compound `@expect a, b` is only reported stale when
        // *none* of its categories could be validated as matching — if any
        // category matched a real diagnostic, the directive earns its keep
        // even though another listed category turned out unnecessary.
        let all_unchecked = cats
            .iter()
            .all(|cat| expect_category_unchecked(*cat, unchecked_categories, *contains_block));
        if !matched && !all_unchecked {
            stale_directives.push((cats.clone(), reason.clone(), *directive_span));
        }
    }

    // Remove suppressed diagnostics (in reverse order to preserve indices)
    suppressed_indices.sort_unstable();
    suppressed_indices.dedup();
    for i in suppressed_indices.into_iter().rev() {
        diagnostics.remove(i);
    }

    // Emit warnings for stale directives (BT-1412: warning, not error, so
    // compilation can proceed — the annotation is just unnecessary).
    for (cats, reason, span) in stale_directives {
        let cats_str = cats
            .iter()
            .map(|c| c.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        let message = if let Some(reason) = reason {
            format!(
                "stale @expect {cats_str} \"{reason}\": no matching diagnostic found on the following expression — consider removing it"
            )
        } else {
            format!(
                "stale @expect {cats_str}: no matching diagnostic found on the following expression — consider removing it"
            )
        };
        diagnostics.push(
            Diagnostic::warning(message, span)
                .with_hint("Remove the `@expect` directive if the diagnostic was fixed"),
        );
    }
}

/// Returns true if the `@expect` category matches a diagnostic category.
///
/// `@expect type` matches both type-mismatch warnings (`DiagnosticCategory::Type`)
/// and method-not-found hints (`DiagnosticCategory::Dnu`).  A common motivation
/// for the latter is type-erasure boundaries — e.g. `Result.unwrap` returns
/// `Object`, so any method call on the result produces a DNU hint — but the
/// suppression applies unconditionally whenever `@expect type` is written.
fn category_matches(expect_cat: ExpectCategory, diag_cat: Option<DiagnosticCategory>) -> bool {
    expect_cat == ExpectCategory::All
        || matches!(
            (expect_cat, diag_cat),
            // BT-1273: @expect type also covers method-not-found (Dnu) hints so that
            // callers can use a single annotation for all type-related suppressions.
            // BT-1918: @expect type also covers missing type-annotation warnings
            // (TypeAnnotation) for backward compatibility.
            (
                ExpectCategory::Dnu | ExpectCategory::Type,
                Some(DiagnosticCategory::Dnu)
            ) | (ExpectCategory::Type, Some(DiagnosticCategory::Type))
                | (
                    ExpectCategory::Type | ExpectCategory::TypeAnnotation,
                    Some(DiagnosticCategory::TypeAnnotation)
                )
                | (ExpectCategory::Unused, Some(DiagnosticCategory::Unused))
                | (
                    ExpectCategory::DeadAssignment,
                    Some(DiagnosticCategory::DeadAssignment)
                )
                | (
                    ExpectCategory::Deprecation,
                    Some(DiagnosticCategory::Deprecation)
                )
                | (ExpectCategory::ActorNew, Some(DiagnosticCategory::ActorNew))
                | (
                    ExpectCategory::Visibility,
                    Some(DiagnosticCategory::Visibility)
                )
                | (
                    ExpectCategory::UnresolvedClass,
                    Some(DiagnosticCategory::UnresolvedClass)
                )
                | (
                    ExpectCategory::UnresolvedFfi,
                    Some(DiagnosticCategory::UnresolvedFfi)
                )
                | (
                    ExpectCategory::ArityMismatch,
                    Some(DiagnosticCategory::ArityMismatch)
                )
                | (
                    ExpectCategory::ShadowedClass,
                    Some(DiagnosticCategory::ShadowedClass)
                )
                | (
                    ExpectCategory::Inheritance,
                    Some(DiagnosticCategory::Inheritance)
                )
                | (
                    ExpectCategory::Sendability,
                    Some(DiagnosticCategory::Sendability)
                )
        )
}

/// Collects `@expect` directives from an expression list.
///
/// For each `ExpectDirective` at index `i`, the target span is the span of
/// the expression at index `i + 1` (if present).
///
/// After scanning the flat statement list, recurses into expression subtrees
/// to find `@expect` directives inside block bodies (BT-2010).
fn collect_directives_from_exprs(
    exprs: &[ExpressionStatement],
    directives: &mut Vec<ExpectDirectiveEntry>,
) {
    for (i, stmt) in exprs.iter().enumerate() {
        if let Expression::ExpectDirective {
            categories,
            reason,
            span,
        } = &stmt.expression
        {
            if let Some(next) = exprs.get(i + 1) {
                directives.push((
                    categories.clone(),
                    reason.clone(),
                    *span,
                    next.expression.span(),
                    expression_contains_block(&next.expression),
                ));
            } else {
                // Trailing @expect with no following expression — treat as stale.
                // Use the directive's own span as the target span so it will
                // never match any real diagnostic and will always be reported stale.
                directives.push((categories.clone(), reason.clone(), *span, *span, false));
            }
        }
        // BT-2010: Recurse into expression subtrees to find block bodies
        // containing @expect directives.
        collect_directives_from_expr(&stmt.expression, directives);
    }
}

/// Returns `true` if `exprs` contains a block literal anywhere in its tree
/// (BT-3384) — see [`expression_contains_block`].
fn exprs_contain_block(exprs: &[ExpressionStatement]) -> bool {
    exprs
        .iter()
        .any(|stmt| expression_contains_block(&stmt.expression))
}

/// Returns `true` if `expr`'s tree contains an [`Expression::Block`] literal
/// anywhere — the only shape `beamtalk-lint`'s `DeadBlockAssignmentPass`
/// (BT-3385) can apply to, used by [`expect_category_unchecked`] (BT-3384) to
/// tell a plausibly-lint-only `@expect dead_assignment` target apart from one
/// `analyse_full`'s own match-arm check already covers.
fn expression_contains_block(expr: &Expression) -> bool {
    let mut found = false;
    crate::ast_walker::walk_expression(expr, &mut |e| {
        if matches!(e, Expression::Block(_)) {
            found = true;
        }
    });
    found
}

/// Recursively walks an expression tree to find nested `Block` bodies and
/// collects `@expect` directives from them (BT-2010).
///
/// This handles `@expect` inside `ifTrue: [...]`, `collect: [:x | ...]`,
/// nested blocks, match arms, and any other expression that contains
/// sub-expressions with block bodies.
fn collect_directives_from_expr(expr: &Expression, directives: &mut Vec<ExpectDirectiveEntry>) {
    match expr {
        Expression::Block(block) => {
            // Found a block body — scan it for @expect directives using the
            // same (i, i+1) semantics, then recurse into its children.
            collect_directives_from_exprs(&block.body, directives);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            collect_directives_from_expr(receiver, directives);
            for arg in arguments {
                collect_directives_from_expr(arg, directives);
            }
        }
        Expression::Assignment { target, value, .. } => {
            collect_directives_from_expr(target, directives);
            collect_directives_from_expr(value, directives);
        }
        Expression::Return { value, .. } | Expression::DestructureAssignment { value, .. } => {
            collect_directives_from_expr(value, directives);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_directives_from_expr(receiver, directives);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_directives_from_expr(arg, directives);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_directives_from_expr(expression, directives);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_directives_from_expr(receiver, directives);
        }
        Expression::Match { value, arms, .. } => {
            collect_directives_from_expr(value, directives);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_directives_from_expr(guard, directives);
                }
                collect_directives_from_expr(&arm.body, directives);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_directives_from_expr(&pair.key, directives);
                collect_directives_from_expr(&pair.value, directives);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                collect_directives_from_expr(elem, directives);
            }
            if let Some(t) = tail {
                collect_directives_from_expr(t, directives);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                collect_directives_from_expr(elem, directives);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    collect_directives_from_expr(e, directives);
                }
            }
        }
        // Leaf nodes — nothing to recurse into.
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Error { .. }
        | Expression::Spread { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{Span, lex_with_eof, parse};

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
native-declaration-location = "error"
file-class-name-mismatch = "error"
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

    #[test]
    fn load_diagnostics_table_for_root_missing_manifest_is_empty() {
        let dir = tempfile::tempdir().unwrap();
        let table = load_diagnostics_table_for_root(dir.path());
        assert!(table.is_empty());
    }

    #[test]
    fn load_diagnostics_table_for_root_invalid_toml_is_empty() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("beamtalk.toml"), "not [ valid toml").unwrap();
        let table = load_diagnostics_table_for_root(dir.path());
        assert!(table.is_empty());
    }

    #[test]
    fn load_diagnostics_table_for_root_manifest_without_diagnostics_section_is_empty() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let table = load_diagnostics_table_for_root(dir.path());
        assert!(table.is_empty());
    }

    #[test]
    fn load_diagnostics_table_for_root_io_error_other_than_not_found_is_empty() {
        // Place a directory where beamtalk.toml would live to trigger EISDIR
        // (a non-NotFound I/O error), exercising the warn-and-return branch.
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("beamtalk.toml")).unwrap();
        let table = load_diagnostics_table_for_root(dir.path());
        assert!(table.is_empty());
    }

    #[test]
    fn load_diagnostics_table_for_root_valid_diagnostics_section_is_parsed() {
        let dir = tempfile::tempdir().unwrap();
        let manifest =
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n[diagnostics]\ndnu = \"error\"\n";
        std::fs::write(dir.path().join("beamtalk.toml"), manifest).unwrap();
        let table = load_diagnostics_table_for_root(dir.path());
        assert_eq!(table.len(), 1);
        assert_eq!(
            table[&DiagnosticCategory::Dnu],
            DiagnosticSeverityOverride::Error
        );
    }

    // ── BT-3384: @expect staleness scoped to lint-vs-non-lint categories ───

    /// Plain `apply_expect_directives` (used by `beamtalk lint`, which always
    /// runs `beamtalk_lint::run_lint_passes` first) validates every category
    /// — with no matching diagnostic, `@expect dead_assignment` is genuinely
    /// stale. Mirrors `beamtalk-language-service`'s pre-existing
    /// `expect_dead_assignment_stale_when_no_diagnostic` (same source), kept
    /// here as the baseline the next test's fixed behavior contrasts with.
    #[test]
    fn apply_expect_directives_dead_assignment_stale_when_no_diagnostic() {
        let source = "@expect dead_assignment\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let mut diagnostics = parse_diags;
        apply_expect_directives(&module, &mut diagnostics);

        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("stale @expect")),
            "plain apply_expect_directives should flag dead_assignment stale \
             when no diagnostic exists, got: {diagnostics:?}"
        );
    }

    /// BT-3384: `beamtalk build`/`beamtalk test`/the LSP/the REPL never run
    /// `beamtalk_lint::run_lint_passes`, so a `DeadBlockAssignmentPass`
    /// diagnostic can never appear in their diagnostics list. An `@expect
    /// dead_assignment` whose target contains a block literal — the only
    /// shape that pass can apply to — must be silently left alone (neither
    /// stale nor satisfied) by `apply_expect_directives_excluding_lint_only`,
    /// unlike plain `apply_expect_directives` (previous test), which
    /// correctly flags the identical source stale since it assumes the lint
    /// pass ran.
    #[test]
    fn apply_expect_directives_excluding_lint_only_does_not_flag_dead_assignment_stale() {
        let source = "@expect dead_assignment\n[42] value";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let mut diagnostics = parse_diags;
        apply_expect_directives_excluding_lint_only(&module, &mut diagnostics);

        assert!(
            !diagnostics
                .iter()
                .any(|d| d.message.contains("stale @expect")),
            "apply_expect_directives_excluding_lint_only must not flag a \
             block-literal target as stale, got: {diagnostics:?}"
        );
    }

    /// BT-3384 review follow-up: `DiagnosticCategory::DeadAssignment` is not
    /// exclusively lint-pass-only — `analyse_full`'s own
    /// `warn_assignment_in_match_arms` also produces it, for an assignment
    /// inside a `match:` arm, and every caller already runs that check. A
    /// `@expect dead_assignment` whose target contains no block literal at
    /// all (so it can't possibly be the lint-only stored-block shape) must
    /// still be flagged stale by `apply_expect_directives_excluding_lint_only`
    /// when nothing matches it — exactly like plain `apply_expect_directives`
    /// — because there's no reason it could only be satisfiable by a check
    /// this invocation doesn't run.
    #[test]
    fn apply_expect_directives_excluding_lint_only_still_flags_dead_assignment_stale_without_a_block()
     {
        let source = "@expect dead_assignment\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let mut diagnostics = parse_diags;
        apply_expect_directives_excluding_lint_only(&module, &mut diagnostics);

        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("stale @expect")),
            "a dead_assignment @expect on a target with no block literal \
             (e.g. the match-arm shape analyse_full already checks) should \
             still be flagged stale, got: {diagnostics:?}"
        );
    }

    /// BT-3384's fix is scoped to categories that are genuinely lint-only: a
    /// category `beamtalk build` DOES check via `analyse_full` (`dnu`, here)
    /// must still be validated for staleness by the excluding variant, same
    /// as plain `apply_expect_directives` — only `dead_assignment` (today's
    /// sole [`LINT_PASS_ONLY_CATEGORIES`] entry) is exempted.
    #[test]
    fn apply_expect_directives_excluding_lint_only_still_flags_other_categories_stale() {
        let source = "@expect dnu\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let mut diagnostics = parse_diags;
        apply_expect_directives_excluding_lint_only(&module, &mut diagnostics);

        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("stale @expect")),
            "apply_expect_directives_excluding_lint_only should still flag a \
             non-lint-only category as stale, got: {diagnostics:?}"
        );
    }

    /// Defensive: if a `DeadAssignment` diagnostic genuinely *is* present on
    /// the `@expect`'s target expression, the excluding variant must still
    /// suppress it exactly like plain `apply_expect_directives` would — the
    /// exclusion only changes the "no matching diagnostic found" staleness
    /// verdict, never a real match.
    #[test]
    fn apply_expect_directives_excluding_lint_only_still_suppresses_a_present_dead_assignment_diagnostic()
     {
        let source = "@expect dead_assignment\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let mut diagnostics = parse_diags;
        let target_span = module.expressions.last().unwrap().expression.span();
        diagnostics.push(
            Diagnostic::lint("test dead assignment", target_span)
                .with_category(DiagnosticCategory::DeadAssignment),
        );
        apply_expect_directives_excluding_lint_only(&module, &mut diagnostics);

        assert!(
            !diagnostics
                .iter()
                .any(|d| d.category == Some(DiagnosticCategory::DeadAssignment)),
            "a real DeadAssignment diagnostic on the target expression must \
             still be suppressed, got: {diagnostics:?}"
        );
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.message.contains("stale @expect")),
            "got: {diagnostics:?}"
        );
    }

    // ── BT-3387: combined `@expect cat1, cat2` form ──

    /// The motivating case: a single expression that genuinely triggers two
    /// distinct diagnostic categories at once (e.g. an unresolved-FFI call
    /// whose return type is also inferred as Dynamic) can be fully
    /// suppressed by one `@expect unresolved_ffi, type` directive, instead
    /// of requiring an artificial second expression to attach a second
    /// `@expect` to.
    #[test]
    fn apply_expect_directives_combined_categories_suppresses_both() {
        let source = "@expect unresolved_ffi, type\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let mut diagnostics = parse_diags;
        let target_span = module.expressions.last().unwrap().expression.span();
        diagnostics.push(
            Diagnostic::warning("test unresolved ffi", target_span)
                .with_category(DiagnosticCategory::UnresolvedFfi),
        );
        diagnostics.push(
            Diagnostic::hint("test type", target_span).with_category(DiagnosticCategory::Type),
        );
        apply_expect_directives(&module, &mut diagnostics);

        assert!(
            diagnostics
                .iter()
                .all(|d| d.category != Some(DiagnosticCategory::UnresolvedFfi)
                    && d.category != Some(DiagnosticCategory::Type)),
            "both categories listed in a combined @expect should be suppressed, got: {diagnostics:?}"
        );
        assert!(
            !diagnostics
                .iter()
                .any(|d| d.message.contains("stale @expect")),
            "a combined @expect satisfied by real diagnostics must not be stale, got: {diagnostics:?}"
        );
    }

    /// A combined directive is not stale as long as *any* of its listed
    /// categories matches a real diagnostic — an unused category in the
    /// list doesn't make the whole directive stale.
    #[test]
    fn apply_expect_directives_combined_categories_not_stale_when_only_one_matches() {
        let source = "@expect unresolved_ffi, sendability\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let mut diagnostics = parse_diags;
        let target_span = module.expressions.last().unwrap().expression.span();
        diagnostics.push(
            Diagnostic::warning("test unresolved ffi", target_span)
                .with_category(DiagnosticCategory::UnresolvedFfi),
        );
        apply_expect_directives(&module, &mut diagnostics);

        assert!(
            !diagnostics
                .iter()
                .any(|d| d.message.contains("stale @expect")),
            "one matching category out of several listed must not be stale, got: {diagnostics:?}"
        );
    }

    /// When *none* of a combined directive's categories match anything, the
    /// whole directive is reported stale — and the warning names every
    /// category that was listed, not just the first.
    #[test]
    fn apply_expect_directives_combined_categories_stale_lists_all_categories() {
        let source = "@expect unused, sendability\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let mut diagnostics = parse_diags;
        apply_expect_directives(&module, &mut diagnostics);

        let stale = diagnostics
            .iter()
            .find(|d| d.message.contains("stale @expect"))
            .unwrap_or_else(|| panic!("expected a stale @expect warning, got: {diagnostics:?}"));
        assert!(
            stale.message.contains("unused") && stale.message.contains("sendability"),
            "stale warning should name every listed category, got: {}",
            stale.message
        );
    }
}
