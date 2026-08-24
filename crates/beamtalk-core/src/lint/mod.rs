// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint passes for Beamtalk source code.
//!
//! **DDD Context:** Compilation
//!
//! Lint checks are style/redundancy checks that are distinct from compiler
//! warnings. They are suppressed during normal `check`/`compile` and only
//! reported by `beamtalk lint`.
//!
//! # Adding a New Lint
//!
//! 1. Create `crates/beamtalk-core/src/lint/<your_lint>.rs`.
//! 2. Declare `pub(crate) struct YourLintPass;` implementing [`LintPass`].
//! 3. Add `mod your_lint;` below (keep alphabetical).
//! 4. Push `Box::new(your_lint::YourLintPass)` into `all_passes()` (keep alphabetical).
//!
//! Each branch touches only its own new file plus two sorted lines here —
//! merge conflicts are minimal and trivially resolved.

mod cascade_candidate;
mod dead_block_assignment;
mod effect_free_statement;
mod inspect_in_string_position;
mod near_miss_divider;
mod shadowed_block_param;
mod sync_send_in_timer_block;
mod trailing_caret;
mod unnecessary_parens;
mod value_like_object;
// ── add new lint modules here (alphabetical) ──────────────────────────────

use crate::ast::Module;
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::Diagnostic;

/// A single lint pass.
///
/// Implementors inspect `module` and push any [`Diagnostic`]s with
/// [`Severity::Lint`] into `diagnostics`.
pub(crate) trait LintPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>);
}

/// Build a [`ClassHierarchy`] for ancestor-aware class-kind resolution in lint passes.
///
/// `run_lint_passes` runs on the freshly-parsed module, before
/// `apply_class_kind_writeback` (which only runs inside codegen). So
/// `class.class_kind` is still `ClassKind::from_superclass_name`'s shallow,
/// direct-superclass-only placeholder — it misses indirect Actor/Value
/// subclasses (e.g. `class Foo extends Bar` where `Bar extends Actor`).
/// Lint passes that need correct actor/value classification should build a
/// hierarchy from this module and use `resolve_class_kind`, the single
/// authority for actor/value classification (BT-3086), which walks the full
/// ancestor chain (BT-3092, BT-3098).
pub(crate) fn hierarchy_for_lint(module: &Module) -> ClassHierarchy {
    let (hierarchy_result, _hierarchy_diagnostics) = ClassHierarchy::build(module);
    hierarchy_result.expect("ClassHierarchy::build is infallible")
}

/// Construct the ordered list of all active lint passes.
///
/// **To register a new pass:** append `Box::new(your_module::YourPass)` in
/// alphabetical order. This is the only line that needs to change per lint.
fn all_passes() -> Vec<Box<dyn LintPass>> {
    vec![
        Box::new(cascade_candidate::CascadeCandidatePass),
        Box::new(dead_block_assignment::DeadBlockAssignmentPass),
        Box::new(effect_free_statement::EffectFreeStatementPass),
        Box::new(inspect_in_string_position::InspectInStringPositionPass),
        Box::new(shadowed_block_param::ShadowedBlockParamPass),
        Box::new(sync_send_in_timer_block::SyncSendInTimerBlockPass),
        Box::new(trailing_caret::TrailingCaretPass),
        Box::new(unnecessary_parens::UnnecessaryParensPass),
        Box::new(value_like_object::ValueLikeObjectPass),
        // ── add new passes here (alphabetical) ────────────────────────────
    ]
}

/// Run all lint passes on a parsed module and return any lint diagnostics.
///
/// The returned diagnostics all have [`Severity::Lint`] and will not appear
/// during normal compilation.
#[must_use]
pub fn run_lint_passes(module: &Module) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for pass in all_passes() {
        pass.check(module, &mut diagnostics);
    }
    diagnostics
}

/// Runs the source-text near-miss-divider scan (BT-3240) and appends its
/// findings to `diagnostics`. See `near_miss_divider::scan_source`'s doc for
/// why this check takes `source` directly instead of a `Module`: `Comment::span`
/// in the AST is stamped with the *following declaration's* span, not the
/// comment's own, so only a source-text scan can give an accurate,
/// comment-sized span.
///
/// Every other pass in this module is [`Severity::Lint`]-only and reachable
/// solely through [`run_lint_passes`]. This one check instead has three
/// direct callers — `queries::diagnostic_provider` (so it also reaches the
/// LSP's `publishDiagnostics`), `beamtalk lint`'s `collect_diagnostics`, and
/// MCP's `run_module_analysis` (BT-3257) — because a silently-mis-parsed
/// section divider is cheap to fix the moment it's written and easy to miss
/// later, and every surface should point at the comment's own line rather
/// than the AST's imprecise span. It stays out of `beamtalk build`'s output:
/// `beam_compiler.rs` filters every `Severity::Lint` diagnostic there,
/// matching the "only shown by `beamtalk lint`" contract the rest of this
/// module relies on.
///
/// `pub` (not `pub(crate)`): called from `beamtalk-cli` and `beamtalk-mcp`,
/// not just from within this crate.
pub fn check_near_miss_dividers(source: &str, diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.extend(near_miss_divider::scan_source(source));
}
