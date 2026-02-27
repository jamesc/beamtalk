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

mod effect_free_statement;
mod shadowed_block_param;
mod trailing_caret;
mod unnecessary_parens;
// ── add new lint modules here (alphabetical) ──────────────────────────────

use crate::ast::Module;
use crate::source_analysis::Diagnostic;

/// A single lint pass.
///
/// Implementors inspect `module` and push any [`Diagnostic`]s with
/// [`Severity::Lint`] into `diagnostics`.
pub(crate) trait LintPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>);
}

/// Construct the ordered list of all active lint passes.
///
/// **To register a new pass:** append `Box::new(your_module::YourPass)` in
/// alphabetical order. This is the only line that needs to change per lint.
fn all_passes() -> Vec<Box<dyn LintPass>> {
    vec![
        Box::new(effect_free_statement::EffectFreeStatementPass),
        Box::new(shadowed_block_param::ShadowedBlockParamPass),
        Box::new(trailing_caret::TrailingCaretPass),
        Box::new(unnecessary_parens::UnnecessaryParensPass),
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
