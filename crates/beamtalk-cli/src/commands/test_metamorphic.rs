// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Metamorphic testing harness (BT-3117).
//!
//! **DDD Context:** CLI / Test System
//!
//! Applies semantics-preserving AST transforms to `.btscript` `// =>`
//! expressions and asserts the transformed expression evaluates to the same
//! expected result as the original -- reusing `test_stdlib`'s exact
//! compile-to-Core-Erlang + `EUnit` execution pipeline for both, rather than a
//! second hand-rolled evaluator (ADR 0011).
//!
//! Each transform is a probe into closure conversion, variable scoping, and
//! state threading -- exactly the silently-wrong bug classes (ADR 0110's
//! NLR-relay bug losing a side effect while returning the right value,
//! state-threading bugs under mutation-in-control-flow) that hand-written
//! `// =>` literals can't catch on their own, because nobody writes a
//! second literal for "the same value as an equivalent-but-differently
//! shaped program".
//!
//! Transforms:
//! - **Block-wrap**: `expr` -> `[expr] value`
//! - **Rename-locals**: consistent alpha-renaming of every name *bound*
//!   inside the expression (block parameters, `:=` assignment targets,
//!   match-pattern bindings), scope- and shadowing-aware: an occurrence is
//!   only renamed if it lexically resolves to a binder introduced inside
//!   this expression. A name that's referenced but never bound *at that
//!   occurrence's scope* -- e.g. it may resolve to an earlier `// =>`
//!   unit's shared REPL-turn binding in the same file, even if some other,
//!   unrelated, non-enclosing part of this same expression happens to bind
//!   a block parameter with the identical name -- is left untouched, since
//!   renaming those would break correctness rather than test it.
//! - **Redundant-temp**: `expr` -> `[tmp := expr. tmp] value`, block-wrapped
//!   so the whole transform stays a single top-level expression (every
//!   other `.btscript` unit is exactly one `Expression`).
//!
//! Each candidate transform output is unparsed and re-parsed before use
//! (mirroring `unparse`'s own `unparse_roundtrip_preserves_structure`
//! property test) -- a transform that doesn't round-trip cleanly is
//! skipped for that unit rather than fed into the compiler.

use crate::beam_compiler::BeamCompiler;
use crate::commands::test_stdlib::{
    self, AssertionCase, CompiledTestFile, EunitBatchResult, TestRunOptions, compile_erl_files,
    compile_expression_to_core, compile_fixture, extract_assignment_var,
    generate_eunit_wrapper_for, parse_test_file, run_all_eunit_tests,
};
use crate::commands::util;
use beamtalk_core::ast::{
    Block, Expression, ExpressionStatement, Identifier, MatchArm, MessageSelector, Module, Pattern,
    StringSegment,
};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_core::unparse::unparse_module;
use camino::Utf8PathBuf;
use ecow::EcoString;
use miette::{IntoDiagnostic, Result, WrapErr};
use std::collections::{BTreeSet, HashMap};
use std::fs;

// ============================================================================
// Transforms
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Transform {
    BlockWrap,
    RenameLocals,
    RedundantTemp,
}

impl Transform {
    const ALL: [Transform; 3] = [Self::BlockWrap, Self::RenameLocals, Self::RedundantTemp];

    fn tag(self) -> &'static str {
        match self {
            Self::BlockWrap => "block_wrap",
            Self::RenameLocals => "rename_locals",
            Self::RedundantTemp => "redundant_temp",
        }
    }

    /// Applies the transform to `expr`, returning the transformed
    /// expression, or `None` if the transform does not apply to this
    /// particular expression (e.g. rename-locals when nothing is bound).
    fn apply(self, expr: &Expression) -> Option<Expression> {
        match self {
            Self::BlockWrap => Some(block_wrap(expr)),
            Self::RedundantTemp => Some(redundant_temp(expr)),
            Self::RenameLocals => rename_locals(expr),
        }
    }
}

/// `expr` -> `[expr] value`.
fn block_wrap(expr: &Expression) -> Expression {
    let span = expr.span();
    Expression::MessageSend {
        receiver: Box::new(Expression::Block(Block::new(
            vec![],
            vec![ExpressionStatement::bare(expr.clone())],
            span,
        ))),
        selector: MessageSelector::Unary("value".into()),
        arguments: vec![],
        is_cast: false,
        span,
    }
}

/// `expr` -> `[tmp := expr. tmp] value`, `tmp` chosen to avoid colliding
/// with any name already appearing in `expr` (bound or free).
fn redundant_temp(expr: &Expression) -> Expression {
    let span = expr.span();
    let mut used = BTreeSet::new();
    collect_all_names(expr, &mut used);
    let tmp: EcoString = fresh_name("mtTmp", &used).into();

    let assign = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(tmp.clone(), span))),
        value: Box::new(expr.clone()),
        type_annotation: None,
        span,
    };
    let read_tmp = Expression::Identifier(Identifier::new(tmp, span));

    Expression::MessageSend {
        receiver: Box::new(Expression::Block(Block::new(
            vec![],
            vec![
                ExpressionStatement::bare(assign),
                ExpressionStatement::bare(read_tmp),
            ],
            span,
        ))),
        selector: MessageSelector::Unary("value".into()),
        arguments: vec![],
        is_cast: false,
        span,
    }
}

/// Consistently alpha-renames every name *bound* somewhere inside `expr`
/// (block parameters, `:=` assignment targets, match-pattern bindings).
/// Renaming itself is scope-aware (see [`rename_names_mut`]): only
/// occurrences that lexically resolve to an in-tree binder are touched, so
/// a free reference is left alone even when some unrelated, non-enclosing
/// part of `expr` happens to bind a same-named local (e.g. a block
/// parameter that shadows an outer name only within its own body).
///
/// Returns `None` if `expr` binds nothing (nothing to rename).
fn rename_locals(expr: &Expression) -> Option<Expression> {
    let mut bound = BTreeSet::new();
    collect_bound_names(expr, &mut bound);
    if bound.is_empty() {
        return None;
    }
    let mut all_names = BTreeSet::new();
    collect_all_names(expr, &mut all_names);

    let mut mapping: HashMap<EcoString, EcoString> = HashMap::new();
    for name in &bound {
        let fresh: EcoString = fresh_name(&format!("{name}R"), &all_names).into();
        all_names.insert(fresh.clone());
        mapping.insert(name.clone(), fresh);
    }

    let mut renamed = expr.clone();
    let mut scope: ScopeStack = vec![BTreeSet::new()];
    rename_names_mut(&mut renamed, &mapping, &mut scope);
    Some(renamed)
}

/// Picks a name starting with `base` that isn't already in `used`,
/// appending a numeric suffix if needed.
fn fresh_name(base: &str, used: &BTreeSet<EcoString>) -> String {
    if !used.iter().any(|u| u.as_str() == base) {
        return base.to_string();
    }
    let mut n = 2;
    loop {
        let candidate = format!("{base}{n}");
        if !used.iter().any(|u| u.as_str() == candidate) {
            return candidate;
        }
        n += 1;
    }
}

// ============================================================================
// Name collection / renaming
//
// Two parallel tree walks over the same set of Expression/Pattern shapes:
// walk_names_ref (read-only, tags each name as a Binding or a Reference,
// used to collect the "what's bound"/"what's used at all" sets -- these are
// deliberately name-level and NOT scope-aware, since they only decide which
// names need a fresh replacement and what replacements are collision-free,
// not which occurrences get touched) and rename_names_mut (in-place,
// scope-aware: renames a binder occurrence unconditionally, but a reference
// occurrence only if it lexically resolves, respecting shadowing, to a
// binder introduced somewhere inside this same expression tree). They are
// kept as two direct recursive functions rather than one generic visitor:
// the two have genuinely different signatures (fold vs. scoped mutate) and
// this module's AST surface is small enough that a shared-abstraction
// visitor would cost more than it saves here.
//
// Scope tracking: a `ScopeStack` is a stack of "names bound directly in
// this lexical scope" sets, innermost last. `Block` and `Match` arms push a
// fresh scope on entry and pop it on exit (matching Beamtalk's actual
// lexical scoping, including block-parameter shadowing of an outer name);
// an `:=`/destructure target adds to the *current* (innermost) scope. A
// plain `Identifier` reference is only renamed if its name resolves in the
// scope stack (searched innermost-first) -- i.e. some binder for that exact
// name is currently in effect at this point in the tree -- which is exactly
// what distinguishes "shadows an in-tree binder" from "free reference that
// happens to share a name with an unrelated, non-enclosing in-tree binder".
// ============================================================================

/// Stack of "names bound directly in this scope", innermost (most deeply
/// nested `Block`/`Match`-arm) last.
type ScopeStack = Vec<BTreeSet<EcoString>>;

fn is_in_scope(scope: &ScopeStack, name: &str) -> bool {
    scope.iter().rev().any(|frame| frame.contains(name))
}

fn bind_in_current_scope(scope: &mut ScopeStack, name: EcoString) {
    if let Some(innermost) = scope.last_mut() {
        innermost.insert(name);
    }
}

#[derive(Clone, Copy)]
enum NameKind {
    Binding,
    Reference,
}

fn collect_all_names(expr: &Expression, names: &mut BTreeSet<EcoString>) {
    walk_names_ref(expr, &mut |n, _kind| {
        names.insert(n.clone());
    });
}

fn collect_bound_names(expr: &Expression, names: &mut BTreeSet<EcoString>) {
    walk_names_ref(expr, &mut |n, kind| {
        if matches!(kind, NameKind::Binding) {
            names.insert(n.clone());
        }
    });
}

fn walk_names_ref(expr: &Expression, on_name: &mut dyn FnMut(&EcoString, NameKind)) {
    match expr {
        Expression::Literal(_, _)
        | Expression::ClassReference { .. }
        | Expression::Super(_)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
        Expression::Identifier(id) => on_name(&id.name, NameKind::Reference),
        Expression::FieldAccess { receiver, .. } => walk_names_ref(receiver, on_name),
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            walk_names_ref(receiver, on_name);
            for a in arguments {
                walk_names_ref(a, on_name);
            }
        }
        Expression::Block(block) => {
            for p in &block.parameters {
                on_name(&p.name, NameKind::Binding);
            }
            for stmt in &block.body {
                walk_names_ref(&stmt.expression, on_name);
            }
        }
        Expression::Assignment { target, value, .. } => {
            if let Expression::Identifier(id) = target.as_ref() {
                on_name(&id.name, NameKind::Binding);
            } else {
                walk_names_ref(target, on_name);
            }
            walk_names_ref(value, on_name);
        }
        Expression::DestructureAssignment { pattern, value, .. } => {
            walk_pattern_names_ref(pattern, on_name);
            walk_names_ref(value, on_name);
        }
        Expression::Return { value, .. } => walk_names_ref(value, on_name),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            walk_names_ref(receiver, on_name);
            for m in messages {
                for a in &m.arguments {
                    walk_names_ref(a, on_name);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => walk_names_ref(expression, on_name),
        Expression::Match { value, arms, .. } => {
            walk_names_ref(value, on_name);
            for arm in arms {
                walk_pattern_names_ref(&arm.pattern, on_name);
                if let Some(guard) = &arm.guard {
                    walk_names_ref(guard, on_name);
                }
                walk_names_ref(&arm.body, on_name);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_names_ref(&pair.key, on_name);
                walk_names_ref(&pair.value, on_name);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for e in elements {
                walk_names_ref(e, on_name);
            }
            if let Some(t) = tail {
                walk_names_ref(t, on_name);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for e in elements {
                walk_names_ref(e, on_name);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let StringSegment::Interpolation(e) = seg {
                    walk_names_ref(e, on_name);
                }
            }
        }
    }
}

fn walk_pattern_names_ref(pattern: &Pattern, on_name: &mut dyn FnMut(&EcoString, NameKind)) {
    match pattern {
        Pattern::Wildcard(_)
        | Pattern::Literal(_, _)
        | Pattern::Nil(_)
        | Pattern::Binary { .. } => {}
        Pattern::Variable(id) => on_name(&id.name, NameKind::Binding),
        Pattern::Type { binding, .. } => on_name(&binding.name, NameKind::Binding),
        Pattern::Tuple { elements, .. } => {
            for e in elements {
                walk_pattern_names_ref(e, on_name);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for e in elements {
                walk_pattern_names_ref(e, on_name);
            }
            if let Some(r) = rest {
                walk_pattern_names_ref(r, on_name);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for e in elements {
                walk_pattern_names_ref(e, on_name);
            }
            if let Some(t) = tail {
                walk_pattern_names_ref(t, on_name);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                walk_pattern_names_ref(&pair.value, on_name);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_, p) in keywords {
                walk_pattern_names_ref(p, on_name);
            }
        }
    }
}

fn rename_names_mut(
    expr: &mut Expression,
    mapping: &HashMap<EcoString, EcoString>,
    scope: &mut ScopeStack,
) {
    match expr {
        Expression::Literal(_, _)
        | Expression::ClassReference { .. }
        | Expression::Super(_)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
        Expression::Identifier(id) => {
            // A read reference is only renamed if it currently resolves to
            // an in-tree binder -- otherwise it's free (may be an outer
            // REPL-turn binding) and must be left untouched even if the
            // same name is mapped because it's bound elsewhere, in an
            // unrelated non-enclosing scope.
            if is_in_scope(scope, &id.name) {
                rename_if_mapped(&mut id.name, mapping);
            }
        }
        Expression::FieldAccess { receiver, .. } => rename_names_mut(receiver, mapping, scope),
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            rename_names_mut(receiver, mapping, scope);
            for a in arguments {
                rename_names_mut(a, mapping, scope);
            }
        }
        Expression::Block(block) => rename_block_mut(block, mapping, scope),
        Expression::Assignment { target, value, .. } => {
            rename_assignment_mut(target, value, mapping, scope);
        }
        Expression::DestructureAssignment { pattern, value, .. } => {
            rename_names_mut(value, mapping, scope);
            rename_pattern_names_mut(pattern, mapping, scope);
        }
        Expression::Return { value, .. } => rename_names_mut(value, mapping, scope),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            rename_names_mut(receiver, mapping, scope);
            for m in messages {
                for a in &mut m.arguments {
                    rename_names_mut(a, mapping, scope);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => {
            rename_names_mut(expression, mapping, scope);
        }
        Expression::Match { value, arms, .. } => {
            rename_names_mut(value, mapping, scope);
            for arm in arms {
                rename_match_arm_mut(arm, mapping, scope);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                rename_names_mut(&mut pair.key, mapping, scope);
                rename_names_mut(&mut pair.value, mapping, scope);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for e in elements {
                rename_names_mut(e, mapping, scope);
            }
            if let Some(t) = tail {
                rename_names_mut(t, mapping, scope);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for e in elements {
                rename_names_mut(e, mapping, scope);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let StringSegment::Interpolation(e) = seg {
                    rename_names_mut(e, mapping, scope);
                }
            }
        }
    }
}

/// Renames a `Block`'s parameters (always in-tree binders) and threads a
/// fresh scope frame through its body so shadowed/free references inside
/// resolve correctly.
fn rename_block_mut(
    block: &mut Block,
    mapping: &HashMap<EcoString, EcoString>,
    scope: &mut ScopeStack,
) {
    scope.push(BTreeSet::new());
    for p in &mut block.parameters {
        bind_in_current_scope(scope, p.name.clone());
        rename_if_mapped(&mut p.name, mapping);
    }
    for stmt in &mut block.body {
        rename_names_mut(&mut stmt.expression, mapping, scope);
    }
    scope.pop();
}

/// Renames an `Assignment`'s target/value. The RHS is walked against the
/// scope as it stood *before* this assignment -- a brand-new implicit
/// local isn't visible to its own initializer.
///
/// The target is only renamed if it's already an in-tree binder (a true
/// reassignment/mutation of a name bound earlier in this same expression,
/// e.g. a shadowing block parameter). A target not currently in scope is
/// deliberately left untouched -- and NOT newly bound -- rather than
/// assumed to be a fresh implicit local: from this expression alone there
/// is no way to distinguish "first assignment to a genuinely fresh local"
/// from "mutation of an earlier `// =>` unit's REPL-turn binding" (e.g.
/// `count := count + 1` inside a later unit's `whileTrue:` body, mutating
/// an outer `count := 0` unit). Renaming the former is a missed
/// opportunity; renaming the latter silently decouples the mutation from
/// the binding it's meant to update. Since this transform's entire
/// purpose is to preserve semantics, an unresolvable target is treated
/// exactly like an unresolvable read: left alone.
fn rename_assignment_mut(
    target: &mut Expression,
    value: &mut Expression,
    mapping: &HashMap<EcoString, EcoString>,
    scope: &mut ScopeStack,
) {
    rename_names_mut(value, mapping, scope);
    if let Expression::Identifier(id) = target {
        if is_in_scope(scope, &id.name) {
            rename_if_mapped(&mut id.name, mapping);
        }
    } else {
        rename_names_mut(target, mapping, scope);
    }
}

/// Renames one `Match` arm. Its pattern bindings are scoped to just this
/// arm (pattern + guard + body), not visible to sibling arms.
fn rename_match_arm_mut(
    arm: &mut MatchArm,
    mapping: &HashMap<EcoString, EcoString>,
    scope: &mut ScopeStack,
) {
    scope.push(BTreeSet::new());
    rename_pattern_names_mut(&mut arm.pattern, mapping, scope);
    if let Some(guard) = &mut arm.guard {
        rename_names_mut(guard, mapping, scope);
    }
    rename_names_mut(&mut arm.body, mapping, scope);
    scope.pop();
}

fn rename_pattern_names_mut(
    pattern: &mut Pattern,
    mapping: &HashMap<EcoString, EcoString>,
    scope: &mut ScopeStack,
) {
    match pattern {
        Pattern::Wildcard(_)
        | Pattern::Literal(_, _)
        | Pattern::Nil(_)
        | Pattern::Binary { .. } => {}
        Pattern::Variable(id) => {
            bind_in_current_scope(scope, id.name.clone());
            rename_if_mapped(&mut id.name, mapping);
        }
        Pattern::Type { binding, .. } => {
            bind_in_current_scope(scope, binding.name.clone());
            rename_if_mapped(&mut binding.name, mapping);
        }
        Pattern::Tuple { elements, .. } => {
            for e in elements {
                rename_pattern_names_mut(e, mapping, scope);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for e in elements {
                rename_pattern_names_mut(e, mapping, scope);
            }
            if let Some(r) = rest {
                rename_pattern_names_mut(r, mapping, scope);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for e in elements {
                rename_pattern_names_mut(e, mapping, scope);
            }
            if let Some(t) = tail {
                rename_pattern_names_mut(t, mapping, scope);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                rename_pattern_names_mut(&mut pair.value, mapping, scope);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_, p) in keywords {
                rename_pattern_names_mut(p, mapping, scope);
            }
        }
    }
}

fn rename_if_mapped(name: &mut EcoString, mapping: &HashMap<EcoString, EcoString>) {
    if let Some(new_name) = mapping.get(name) {
        *name = new_name.clone();
    }
}

// ============================================================================
// Building transformed test cases
// ============================================================================

/// Applies `transform` to `case.expression`, unparses and re-parses the
/// result, and returns a new [`TestCase`](test_stdlib::TestCase) with the
/// transformed source text (same `expected`/`line` as the original --
/// applying a semantics-preserving transform must not change the value a
/// unit evaluates to).
///
/// Returns `None` if the transform doesn't apply to this expression shape
/// (see [`Transform::apply`]), if the unit is a top-level REPL-turn binding
/// (see below), or if the transformed text doesn't re-parse cleanly --
/// either way, this unit is skipped for this transform rather than fed
/// into the compiler.
///
/// **Top-level bindings are passed through unchanged, never transformed.**
/// `.btscript`'s cross-unit "REPL turn" semantics (module doc / architecture
/// note in `test_stdlib::extract_assignment_var`) are a text-pattern match
/// on each case's *source text*: a case shaped exactly `name := value` has
/// `name` threaded into the shared `Bindings` map later cases in the same
/// file can reference. Every transform here changes the unit's outer shape
/// (wraps it in `[... ] value`) or its bound names (rename-locals) -- either
/// would defeat that text match for a case that itself establishes a
/// binding, silently breaking every later case in the file that depends on
/// it. That is a `.btscript`-format artifact, not a real semantics change in
/// compiled Beamtalk, so cases of this exact shape are carried into the
/// transformed module verbatim -- still asserted (against their own
/// original `// =>` value, same as `test_stdlib`), just not themselves a
/// transform-under-test -- so later cases that reference the binding keep
/// seeing it. Detected by calling `test_stdlib::extract_assignment_var`
/// directly (rather than re-deriving the same fact from the parsed AST) so
/// the two can never disagree on what counts as a REPL-turn binding.
fn build_transformed_case(
    case: &test_stdlib::TestCase,
    transform: Transform,
) -> Option<test_stdlib::TestCase> {
    if extract_assignment_var(&case.expression).is_some() {
        return Some(test_stdlib::TestCase {
            expression: case.expression.clone(),
            expected: case.expected.clone(),
            line: case.line,
        });
    }

    let tokens = lex_with_eof(&case.expression);
    let (module, diagnostics) = parse(tokens);
    if diagnostics.iter().any(|d| d.severity == Severity::Error) {
        // Shouldn't happen: test_stdlib already compiled this expression
        // successfully as part of the corpus it was parsed from.
        return None;
    }
    let orig_expr = &module.expressions.first()?.expression;

    let transformed_expr = transform.apply(orig_expr)?;

    let synthetic = Module::new(
        vec![ExpressionStatement::bare(transformed_expr)],
        orig_expr.span(),
    );
    let expr_text = unparse_module(&synthetic).trim().to_string();

    // Round-trip check, mirroring unparse's own
    // unparse_roundtrip_preserves_structure property test: the transform
    // must produce text that re-parses cleanly.
    let reparsed_tokens = lex_with_eof(&expr_text);
    let (_reparsed_module, reparsed_diagnostics) = parse(reparsed_tokens);
    if reparsed_diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        return None;
    }

    Some(test_stdlib::TestCase {
        expression: expr_text,
        expected: case.expected.clone(),
        line: case.line,
    })
}

// ============================================================================
// Per-(file, transform) compilation
// ============================================================================

/// Everything `run_tests` needs to fold one successfully-compiled
/// (file, transform) variant into its running accumulators.
struct TransformVariant {
    core_files: Vec<Utf8PathBuf>,
    erl_file: Utf8PathBuf,
    compiled: CompiledTestFile,
    label: String,
}

/// Builds and compiles every case in `parsed_cases` under `transform` for
/// one source file, writing the resulting `.core`/`.erl` files into
/// `build_dir`. Returns `(skipped_count, None)` if every case was skipped
/// (nothing to compile for this transform), or `(skipped_count,
/// Some(variant))` with the compiled artifacts otherwise.
fn compile_transform_variant(
    test_file: &camino::Utf8Path,
    file_stem: &str,
    safe_stem: &str,
    parsed_cases: &[test_stdlib::TestCase],
    transform: Transform,
    build_dir: &camino::Utf8Path,
) -> Result<(usize, Option<TransformVariant>)> {
    let mut originals: Vec<&test_stdlib::TestCase> = Vec::new();
    let mut transformed_cases: Vec<test_stdlib::TestCase> = Vec::new();
    let mut skipped = 0usize;
    for case in parsed_cases {
        match build_transformed_case(case, transform) {
            Some(new_case) => {
                originals.push(case);
                transformed_cases.push(new_case);
            }
            None => skipped += 1,
        }
    }
    if transformed_cases.is_empty() {
        return Ok((skipped, None));
    }

    let mut eval_module_names = Vec::new();
    let mut core_files = Vec::new();
    for (i, case) in transformed_cases.iter().enumerate() {
        let module_name = format!("mt_{safe_stem}_{}_{i}", transform.tag());
        match compile_expression_to_core(&case.expression, &module_name) {
            Ok(core_erlang) => {
                let core_file = build_dir.join(format!("{module_name}.core"));
                fs::write(&core_file, core_erlang)
                    .into_diagnostic()
                    .wrap_err_with(|| {
                        format!(
                            "Failed to write Core Erlang for {}:{} [{}]",
                            test_file,
                            originals[i].line,
                            transform.tag()
                        )
                    })?;
                core_files.push(core_file);
                eval_module_names.push(module_name);
            }
            Err(err) => {
                // A transformed unit that unparsed/re-parsed cleanly
                // (checked in build_transformed_case) but still fails to
                // codegen is itself a real finding: it means the transform
                // produced a shape codegen can't yet handle. Surface it as
                // a hard failure rather than silently skipping.
                miette::bail!(
                    "Failed to compile metamorphic [{}] variant of {}:{}: {}\n  \
                     Original:    {}\n  Transformed: {}",
                    transform.tag(),
                    test_file,
                    originals[i].line,
                    err,
                    originals[i].expression,
                    case.expression
                );
            }
        }
    }

    let assertion_cases: Vec<AssertionCase<'_>> = transformed_cases
        .iter()
        .map(|c| AssertionCase {
            expression: &c.expression,
            expected: &c.expected,
            line: c.line,
        })
        .collect();

    let test_module_name = format!("mt_{safe_stem}_{}_tests", transform.tag());
    let source_path = format!("{test_file} [{}]", transform.tag());
    let eunit_source = generate_eunit_wrapper_for(
        &test_module_name,
        &format!("{test_module_name}_test_"),
        &format!(
            "%% Generated metamorphic [{}] variant of {}",
            transform.tag(),
            test_file
        ),
        &source_path,
        &assertion_cases,
        &eval_module_names,
    );
    let erl_file = build_dir.join(format!("{test_module_name}.erl"));
    fs::write(&erl_file, &eunit_source)
        .into_diagnostic()
        .wrap_err("Failed to write EUnit wrapper")?;

    Ok((
        skipped,
        Some(TransformVariant {
            core_files,
            erl_file,
            compiled: CompiledTestFile {
                source_file: test_file.to_owned(),
                module_name: test_module_name,
                assertion_count: assertion_cases.len(),
            },
            label: format!("{file_stem} [{}]", transform.tag()),
        }),
    ))
}

/// Compiles every `@load` fixture referenced by `parsed`, returning the
/// resulting `bt@...` module names.
fn compile_load_fixtures(
    parsed: &test_stdlib::ParsedTestFile,
    test_file: &camino::Utf8Path,
    build_dir: &camino::Utf8Path,
    opts: &TestRunOptions,
) -> Result<Vec<String>> {
    let mut fixture_modules = Vec::new();
    for load_path in &parsed.load_files {
        let fixture_path = Utf8PathBuf::from(load_path);
        if !fixture_path.exists() {
            miette::bail!(
                "Fixture file '{}' referenced by @load in '{}' not found",
                load_path,
                test_file
            );
        }
        let module_name = compile_fixture(
            &fixture_path,
            build_dir,
            opts.no_warnings,
            opts.warnings_as_errors,
        )?;
        fixture_modules.push(module_name);
    }
    Ok(fixture_modules)
}

/// Everything one source file contributes to `run_tests`'s running
/// accumulators, across all of `Transform::ALL`.
struct FileVariants {
    fixture_modules: Vec<String>,
    core_files: Vec<Utf8PathBuf>,
    erl_files: Vec<Utf8PathBuf>,
    compiled: Vec<CompiledTestFile>,
    labels: Vec<String>,
    skipped: usize,
}

/// Parses one `.btscript` file, compiles its `@load` fixtures, and builds +
/// compiles every transform variant of its cases.
fn process_test_file(
    test_file: &camino::Utf8Path,
    build_dir: &camino::Utf8Path,
    opts: &TestRunOptions,
) -> Result<FileVariants> {
    let content = fs::read_to_string(test_file)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{test_file}'"))?;
    let parsed = parse_test_file(&content);

    // Same enforcement as test_stdlib: a corpus file with expressions
    // missing assertions is a corpus regression, not something this
    // harness should silently work around (BT-3117).
    if !parsed.warnings.is_empty() {
        for warning in &parsed.warnings {
            eprintln!("⚠️  {test_file}: {warning}");
        }
        miette::bail!(
            "{} has {} expression(s) without assertions",
            test_file,
            parsed.warnings.len()
        );
    }

    let fixture_modules = compile_load_fixtures(&parsed, test_file, build_dir, opts)?;

    let file_stem = test_file
        .file_stem()
        .ok_or_else(|| miette::miette!("Test file has no name: {}", test_file))?;
    let safe_stem: String = file_stem
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect();

    let mut result = FileVariants {
        fixture_modules,
        core_files: Vec::new(),
        erl_files: Vec::new(),
        compiled: Vec::new(),
        labels: Vec::new(),
        skipped: 0,
    };
    for transform in Transform::ALL {
        let (skipped, variant) = compile_transform_variant(
            test_file,
            file_stem,
            &safe_stem,
            &parsed.cases,
            transform,
            build_dir,
        )?;
        result.skipped += skipped;
        if let Some(variant) = variant {
            result.core_files.extend(variant.core_files);
            result.erl_files.push(variant.erl_file);
            result.compiled.push(variant.compiled);
            result.labels.push(variant.label);
        }
    }
    Ok(result)
}

// ============================================================================
// Main entry point
// ============================================================================

/// Run the metamorphic testing harness (BT-3117).
///
/// Finds all `.btscript` files in the given path, applies each transform in
/// [`Transform::ALL`] to every `// =>` expression, and asserts each
/// transformed variant evaluates to the same expected result as the
/// original -- via the same compile-to-Core-Erlang + `EUnit` pipeline
/// `test_stdlib` uses for the originals.
pub fn run_tests(path: &str, opts: &TestRunOptions) -> Result<()> {
    let test_path = Utf8PathBuf::from(path);
    if !test_path.exists() {
        miette::bail!("Test path '{}' not found", test_path);
    }

    let test_files = util::find_files(&test_path, &["btscript"])?;
    if test_files.is_empty() {
        println!("No .btscript test files found in '{test_path}'");
        return Ok(());
    }

    if !opts.quiet {
        println!(
            "Generating metamorphic variants ({} transforms) for {} test file(s)...",
            Transform::ALL.len(),
            test_files.len()
        );
    }

    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory")?;
    let build_dir = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;

    let mut compiled_files: Vec<CompiledTestFile> = Vec::new();
    let mut labels: Vec<String> = Vec::new();
    let mut all_core_files = Vec::new();
    let mut all_erl_files = Vec::new();
    let mut all_fixture_modules = Vec::new();
    let mut skipped_units = 0usize;

    for test_file in &test_files {
        let variants = process_test_file(test_file, &build_dir, opts)?;
        all_fixture_modules.extend(variants.fixture_modules);
        all_core_files.extend(variants.core_files);
        all_erl_files.extend(variants.erl_files);
        compiled_files.extend(variants.compiled);
        labels.extend(variants.labels);
        skipped_units += variants.skipped;
    }

    all_fixture_modules.sort();
    all_fixture_modules.dedup();

    if !all_core_files.is_empty() {
        let compiler = BeamCompiler::new(build_dir.clone());
        compiler
            .compile_batch(&all_core_files)
            .wrap_err("Failed to batch-compile metamorphic expression modules to BEAM")?;
    }

    compile_erl_files(&all_erl_files, &build_dir)?;

    let test_module_names: Vec<&str> = compiled_files
        .iter()
        .map(|f| f.module_name.as_str())
        .collect();
    let total_tests: usize = compiled_files.iter().map(|f| f.assertion_count).sum();

    if test_module_names.is_empty() {
        println!(
            "No metamorphic-testable expressions found ({skipped_units} expression/transform \
             combination(s) skipped -- transform not applicable or didn't round-trip cleanly)."
        );
        return Ok(());
    }

    let eunit_result = run_all_eunit_tests(
        &test_module_names,
        &all_fixture_modules,
        &build_dir,
        opts.verbose,
    )?;

    report_metamorphic_results(
        &compiled_files,
        &labels,
        &eunit_result,
        test_files.len(),
        total_tests,
        skipped_units,
        opts.quiet,
    )
}

/// Print per-(file, transform) results and final summary. Mirrors
/// `test_stdlib::report_results`, but each row is labelled with its
/// transform (since one source file now produces up to
/// `Transform::ALL.len()` result rows) and the summary also surfaces how
/// many expression/transform combinations were skipped.
fn report_metamorphic_results(
    compiled_files: &[CompiledTestFile],
    labels: &[String],
    eunit_result: &EunitBatchResult,
    file_count: usize,
    total_tests: usize,
    skipped_units: usize,
    quiet: bool,
) -> Result<()> {
    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut failed_details = Vec::new();

    for (compiled, label) in compiled_files.iter().zip(labels.iter()) {
        if let Some(result) = eunit_result.module_results.get(&compiled.module_name) {
            total_passed += result.passed;
            total_failed += result.failed;
            if result.failed > 0 {
                println!(
                    "  {label}: {} tests, {} passed, {} failed ✗",
                    result.passed + result.failed,
                    result.passed,
                    result.failed,
                );
                for failure in &result.failures {
                    failed_details.push(failure.clone());
                }
            } else if !quiet {
                println!(
                    "  {label}: {} tests, {} passed ✓",
                    result.passed, result.passed
                );
            }
        } else if let Some(failure) = eunit_result.failed_modules.get(&compiled.module_name) {
            total_failed += compiled.assertion_count;
            println!("  {label}: {} tests, 0 passed ✗", compiled.assertion_count);
            failed_details.push(format!("FAIL {label}:\n  {failure}"));
        } else {
            total_passed += compiled.assertion_count;
            if !quiet {
                println!(
                    "  {label}: {} tests, {} passed ✓",
                    compiled.assertion_count, compiled.assertion_count
                );
            }
        }
    }

    println!();
    if skipped_units > 0 && !quiet {
        println!(
            "({skipped_units} expression/transform combination(s) skipped -- transform not \
             applicable or didn't round-trip cleanly)"
        );
    }
    if total_failed == 0 {
        println!(
            "{file_count} file(s), {total_tests} metamorphic assertions, {total_passed} passed, 0 failed"
        );
        Ok(())
    } else {
        for detail in &failed_details {
            eprintln!("{detail}");
        }
        eprintln!();
        eprintln!(
            "{file_count} file(s), {total_tests} metamorphic assertions, {total_passed} passed, {total_failed} failed"
        );
        miette::bail!(
            "{total_failed} metamorphic test(s) failed -- a semantics-preserving transform changed the result"
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expr(source: &str) -> Expression {
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);
        assert!(
            diagnostics.iter().all(|d| d.severity != Severity::Error),
            "test fixture failed to parse: {source:?}\n{diagnostics:?}",
        );
        module.expressions.first().unwrap().expression.clone()
    }

    /// Regression test (PR #3308 review): a name that's free at one point
    /// in the expression (referring to an earlier `// =>` unit's
    /// REPL-turn binding) but also happens to be bound as a block
    /// parameter elsewhere in the *same* expression must only have its
    /// bound occurrences renamed -- the free occurrence has to survive
    /// untouched, or the transformed unit ends up referencing a name
    /// nothing defines.
    #[test]
    fn rename_locals_leaves_shadowed_free_reference_untouched() {
        let expr = parse_expr("(1 to: 3) inject: n into: [:acc :n | acc + n]");
        let renamed = rename_locals(&expr).expect("block parameters are bound, should rename");

        let Expression::MessageSend { arguments, .. } = &renamed else {
            panic!("expected a keyword MessageSend, got {renamed:?}");
        };
        let [seed_arg, block_arg] = arguments.as_slice() else {
            panic!("expected exactly 2 arguments (inject:into:), got {arguments:?}");
        };

        // The free `n` (the inject: seed argument, referring to an outer
        // REPL-turn binding) must be untouched.
        let Expression::Identifier(seed_id) = seed_arg else {
            panic!("expected seed argument to remain a plain identifier, got {seed_arg:?}");
        };
        assert_eq!(
            seed_id.name.as_str(),
            "n",
            "free `n` argument must survive renaming unchanged"
        );

        // The block's own bound `n` must have been renamed to something
        // else, consistently, for both the parameter and its body
        // reference.
        let Expression::Block(block) = block_arg else {
            panic!("expected block argument, got {block_arg:?}");
        };
        // Both block parameters are bound only within this block (no free
        // counterpart), so -- like the purely-bound-name sanity check
        // below -- both get renamed; `acc`'s fresh name is just not the
        // one under test here.
        let renamed_acc = block.parameters[0].name.clone();
        let renamed_n = &block.parameters[1].name;
        assert_ne!(renamed_n.as_str(), "n", "block-bound `n` must be renamed");

        let Expression::MessageSend {
            receiver: body_receiver,
            arguments: body_args,
            ..
        } = &block.body[0].expression
        else {
            panic!(
                "expected `acc + n` body, got {:?}",
                block.body[0].expression
            );
        };
        assert!(
            matches!(body_receiver.as_ref(), Expression::Identifier(id) if id.name == renamed_acc)
        );
        let Expression::Identifier(body_ref) = &body_args[0] else {
            panic!("expected identifier reference in block body");
        };
        assert_eq!(
            body_ref.name, *renamed_n,
            "block body's reference to its own parameter must use the same fresh name"
        );
    }

    /// Sanity check: when a bound name has no unrelated free counterpart,
    /// every occurrence is still renamed together (basic case the old,
    /// non-scope-aware implementation also handled correctly).
    #[test]
    fn rename_locals_renames_all_occurrences_of_a_purely_bound_name() {
        let expr = parse_expr("[:x | x + x] value: 1");
        let renamed = rename_locals(&expr).expect("block parameter is bound, should rename");

        let Expression::MessageSend { receiver, .. } = &renamed else {
            panic!("expected `[...] value: 1`, got {renamed:?}");
        };
        let Expression::Block(block) = receiver.as_ref() else {
            panic!("expected block receiver, got {receiver:?}");
        };
        let renamed_param = &block.parameters[0].name;
        assert_ne!(renamed_param.as_str(), "x");

        let Expression::MessageSend {
            receiver: body_receiver,
            arguments: body_args,
            ..
        } = &block.body[0].expression
        else {
            panic!("expected `x + x` body, got {:?}", block.body[0].expression);
        };
        assert!(
            matches!(body_receiver.as_ref(), Expression::Identifier(id) if id.name == *renamed_param)
        );
        assert!(matches!(&body_args[0], Expression::Identifier(id) if id.name == *renamed_param));
    }

    /// Regression test (PR #3308 review, write side of the same hazard
    /// class as the shadowed-free-reference test above): mirrors the
    /// canonical `whileTrue:` mutation idiom
    /// (`docs/beamtalk-language-features.md` § Local Variable Mutations).
    /// `count` here is read once and assigned once, but never *bound*
    /// anywhere inside this expression -- in the real `.btscript` corpus
    /// it would be mutating an earlier unit's `count := 0` REPL-turn
    /// binding. `rename_assignment_mut` must not invent a decoupled fresh
    /// local for the assignment target: doing so would silently stop the
    /// loop condition from ever changing.
    #[test]
    fn rename_locals_leaves_free_assignment_target_untouched() {
        let expr = parse_expr("[count < 10] whileTrue: [count := count + 1]");
        let renamed = rename_locals(&expr).unwrap_or_else(|| expr.clone());

        let Expression::MessageSend {
            receiver,
            arguments,
            ..
        } = &renamed
        else {
            panic!("expected `[...] whileTrue: [...]`, got {renamed:?}");
        };
        let Expression::Block(cond_block) = receiver.as_ref() else {
            panic!("expected condition block receiver, got {receiver:?}");
        };
        let Expression::MessageSend {
            receiver: cond_recv,
            ..
        } = &cond_block.body[0].expression
        else {
            panic!(
                "expected `count < 10`, got {:?}",
                cond_block.body[0].expression
            );
        };
        assert!(
            matches!(cond_recv.as_ref(), Expression::Identifier(id) if id.name.as_str() == "count"),
            "free read of `count` in the loop condition must survive unchanged"
        );

        let Expression::Block(body_block) = &arguments[0] else {
            panic!("expected whileTrue: body block argument, got {arguments:?}");
        };
        let Expression::Assignment { target, value, .. } = &body_block.body[0].expression else {
            panic!(
                "expected `count := count + 1`, got {:?}",
                body_block.body[0].expression
            );
        };
        assert!(
            matches!(target.as_ref(), Expression::Identifier(id) if id.name.as_str() == "count"),
            "assignment target must stay `count`, not a decoupled fresh local: {target:?}"
        );
        let Expression::MessageSend {
            receiver: rhs_recv, ..
        } = value.as_ref()
        else {
            panic!("expected `count + 1`, got {value:?}");
        };
        assert!(
            matches!(rhs_recv.as_ref(), Expression::Identifier(id) if id.name.as_str() == "count"),
            "read of `count` on the assignment's RHS must survive unchanged"
        );
    }
}
