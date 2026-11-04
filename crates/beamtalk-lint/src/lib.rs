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
//! A standalone crate (ADR 0117 Decision step 2) depending only on the
//! Compilation bounded context (`ast`, `ast_walker`, `semantic_analysis`,
//! `source_analysis`) in production, with no back-edges, so the boundary is
//! `cargo`-enforced. The one exception is the near-miss-divider check: it
//! stays in `beamtalk_core::near_miss_divider`, a shared leaf module,
//! because `queries::diagnostic_provider` (which stays in `beamtalk-core`)
//! calls it directly — see that module's doc for why moving it here would
//! have created a crate cycle.
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

mod actor_state_abroad;
mod cascade_candidate;
mod dead_block_assignment;
mod dead_returned_assignment;
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

/// Which tree a [`LintPass`] reads.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LintTree {
    /// The parsed module with every redundant send-operand wrapper removed
    /// (`beamtalk_core::ast::normalize`, BT-4238): the tree analysis reads,
    /// so `(e) foo` lints exactly like `e foo` and a span this pass takes
    /// from `semantic_analysis` (e.g. `local_threading_diagnostics`) names a
    /// node of the tree it walks.
    Normalised,
    /// The module as parsed, parentheses and all: only for a pass whose
    /// subject is the parentheses themselves (`unnecessary_parens`).
    Parsed,
}

/// What a [`LintPass`] may consult besides the module it walks.
///
/// `hierarchy` is the class hierarchy to resolve class names against —
/// actor/value kind (`resolve_class_kind`, the single authority, which walks
/// the full ancestor chain), `all_state`, class variables, `spawns_block`.
/// On `beamtalk lint` and the MCP lint tool it is the cross-file hierarchy
/// `analyse_full` already built for this file (out-of-file classes injected
/// via `add_from_beam_meta`), handed in through
/// [`run_lint_passes_with_hierarchy`] at no extra cost (BT-4183), so a
/// subclass whose actor ancestor lives in another file resolves as an actor.
/// [`run_lint_passes`] builds a module-local one instead.
///
/// That hierarchy is built from a trait-expanded clone of the module while a
/// pass walks the caller's tree; every lint lookup is by class name, so the
/// two agree.
pub(crate) struct LintContext<'a> {
    pub(crate) hierarchy: &'a ClassHierarchy,
}

/// A single lint pass.
///
/// Implementors inspect `module` and push any [`Diagnostic`]s with
/// [`Severity::Lint`] into `diagnostics`. A pass that needs class
/// resolution reads `ctx.hierarchy` (see [`LintContext`]), never
/// `class.class_kind`: the lints run on the freshly parsed module, before
/// `apply_class_kind_writeback` (codegen-only), so that field is still
/// `ClassKind::from_superclass_name`'s direct-superclass-only placeholder.
pub(crate) trait LintPass {
    fn check(&self, module: &Module, ctx: &LintContext<'_>, diagnostics: &mut Vec<Diagnostic>);

    /// The tree [`run_lint_passes_with_hierarchy`] hands to [`LintPass::check`].
    fn tree(&self) -> LintTree {
        LintTree::Normalised
    }
}

/// The module-local [`ClassHierarchy`]: this file's classes only, for
/// [`run_lint_passes`] (a caller with no cross-file context) and pass tests.
fn module_hierarchy(module: &Module) -> ClassHierarchy {
    let (hierarchy_result, _hierarchy_diagnostics) = ClassHierarchy::build(module);
    hierarchy_result.expect("ClassHierarchy::build is infallible")
}

/// The fix hint for a synchronous send to the actor that should be an async
/// cast, shared by the lints that flag one (`actor_state_abroad`,
/// `sync_send_in_timer_block`).
///
/// `cast` is the concrete cast spelling (`self bump!`) when one exists: only a
/// plain unary send has one — `!` terminates the full send, so the cast of
/// `self update: 42` is `self update: 42!`, never `self update:!`, and a
/// cascade message has no cast spelling. Without one the hint is generic.
pub(crate) fn async_cast_hint(cast: Option<&str>) -> String {
    cast.map_or_else(
        || "Use an async cast (`!` after the full send) instead (ADR 0137)".to_string(),
        |cast| format!("Use `{cast}` (async cast) instead (ADR 0137)"),
    )
}

/// Construct the ordered list of all active lint passes.
///
/// **To register a new pass:** append `Box::new(your_module::YourPass)` in
/// alphabetical order. This is the only line that needs to change per lint.
fn all_passes() -> Vec<Box<dyn LintPass>> {
    vec![
        Box::new(actor_state_abroad::ActorStateAbroadPass),
        Box::new(cascade_candidate::CascadeCandidatePass),
        Box::new(dead_block_assignment::DeadBlockAssignmentPass),
        Box::new(dead_returned_assignment::DeadReturnedAssignmentPass),
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

/// Run all lint passes on a parsed module against `hierarchy` and return
/// any lint diagnostics.
///
/// `hierarchy` resolves class names for the passes that need it (see
/// [`LintContext`]): the lint surfaces pass the cross-file hierarchy
/// `analyse_full` built for this file, through the hook of
/// `run_post_analysis_lint_pipeline` (BT-4183).
///
/// Each pass reads the tree its [`LintPass::tree`] names: the operand-
/// normalised module (computed once, BT-4238) unless it inspects the
/// parentheses themselves.
///
/// The returned diagnostics all have [`Severity::Lint`] and will not appear
/// during normal compilation.
#[must_use]
pub fn run_lint_passes_with_hierarchy(
    module: &Module,
    hierarchy: &ClassHierarchy,
) -> Vec<Diagnostic> {
    let normalised = beamtalk_core::ast::normalize::normalised(module);
    let ctx = LintContext { hierarchy };
    let mut diagnostics = Vec::new();
    for pass in all_passes() {
        pass.check(
            tree_for(pass.as_ref(), module, &normalised),
            &ctx,
            &mut diagnostics,
        );
    }
    diagnostics
}

/// [`run_lint_passes_with_hierarchy`] against this module's own hierarchy,
/// built once: for callers with no cross-file context (and pass tests). A
/// class whose ancestor is declared in another file resolves as if that
/// ancestor were unknown.
#[must_use]
pub fn run_lint_passes(module: &Module) -> Vec<Diagnostic> {
    run_lint_passes_with_hierarchy(module, &module_hierarchy(module))
}

/// The tree `pass` reads: `parsed` or its `normalised` form (see
/// [`LintTree`]).
fn tree_for<'m>(pass: &dyn LintPass, parsed: &'m Module, normalised: &'m Module) -> &'m Module {
    match pass.tree() {
        LintTree::Normalised => normalised,
        LintTree::Parsed => parsed,
    }
}

/// One pass on `module`, reading the tree [`run_lint_passes`] would hand it
/// and against the same module-local hierarchy:
/// for a pass's own tests.
#[cfg(test)]
pub(crate) fn run_pass(pass: &dyn LintPass, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    let normalised = beamtalk_core::ast::normalize::normalised(module);
    let hierarchy = module_hierarchy(module);
    pass.check(
        tree_for(pass, module, &normalised),
        &LintContext {
            hierarchy: &hierarchy,
        },
        diagnostics,
    );
}

/// The module-local hierarchy, for a pass test that adjusts it (injects an
/// out-of-file class, marks names shadowed) before linting.
#[cfg(test)]
pub(crate) fn module_hierarchy_for_test(module: &Module) -> ClassHierarchy {
    module_hierarchy(module)
}
