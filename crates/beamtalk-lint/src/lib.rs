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
//! BT-3340 (ADR 0117 Decision step 2): extracted from `beamtalk-core::lint`
//! into its own crate — `lint` depended only on the Compilation bounded
//! context (`ast`, `ast_walker`, `semantic_analysis`, `source_analysis`) in
//! production, with no back-edges, so this is a mechanical move giving a
//! real, `cargo`-enforced boundary. The one exception is the
//! near-miss-divider check (BT-3240): it stayed behind as
//! `beamtalk_core::near_miss_divider`, a shared leaf module, because
//! `queries::diagnostic_provider` (which stays in `beamtalk-core`) calls it
//! directly — see that module's doc for why moving it here would have
//! created a crate cycle.
//!
//! # Adding a New Lint
//!
//! 1. Create `crates/beamtalk-lint/src/<your_lint>.rs`.
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
mod shadowed_block_param;
mod sync_send_in_timer_block;
mod trailing_caret;
mod unnecessary_parens;
mod value_like_object;
// ── add new lint modules here (alphabetical) ──────────────────────────────

use beamtalk_core::ast::Module;
use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_core::source_analysis::Diagnostic;

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
