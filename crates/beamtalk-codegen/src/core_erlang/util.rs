// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Utility functions for Core Erlang code generation.
//!
//! This module provides helper functions for:
//! - Indentation management
//! - Variable name generation (fresh variables, temp variables)
//! - State variable threading (State, State1, State2, ...)
//! - Name conversions (class names, module names)
//! - Class identity (DDD Value Object bundling module + class names)

use std::fmt::Write as _;

use super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf::{atom, int_lit, string_lit};
use beamtalk_cerl_doc::{Document, join};
use beamtalk_core::ast::{
    ClassDefinition, Expression, ExpressionStatement, Module, SlotKind, StateDeclaration,
};

/// Builds a versioned Core Erlang variable name.
///
/// Returns `prefix` when `version == 0`, otherwise `prefix{version}`
/// (e.g. `"State"`, `"State1"`, `"StateAcc2"`, `"Self3"`).
///
/// Uses [`std::fmt::Write`] on a pre-allocated buffer rather than `format!()`
/// to comply with AGENTS.md: "NEVER use `format!()` to produce Core Erlang
/// fragments."  See also: `variable_context::VariableContext::fresh_var` which
/// uses the same `write!` pattern.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(versioned_var("State", 0), "State");
/// assert_eq!(versioned_var("State", 1), "State1");
/// assert_eq!(versioned_var("StateAcc", 2), "StateAcc2");
/// ```
pub(super) fn versioned_var(prefix: &str, version: usize) -> String {
    if version == 0 {
        prefix.to_string()
    } else {
        // Pre-allocate: prefix length + up to 4 digits (handles version ≤ 9999).
        let mut s = String::with_capacity(prefix.len() + 4);
        s.push_str(prefix);
        // Write the counter directly instead of using format!.
        let _ = write!(s, "{version}");
        s
    }
}

/// Builds the metaclass tag string `"{class_name} class"`.
///
/// This is the naming convention for metaclass atoms throughout the Beamtalk runtime
/// (e.g. `'Array class'`, `'Object class'`). Extracted as a named helper so the
/// convention is documented in one place and callers don't need to remember the
/// exact suffix.
pub(super) fn metaclass_tag(class_name: &str) -> String {
    // " class" is 6 bytes.
    let mut s = String::with_capacity(class_name.len() + 6);
    s.push_str(class_name);
    s.push_str(" class");
    s
}

/// Builds the name of a class's static call key (BT-4442),
/// `"{class_name} class static calls"`: the one `persistent_term` key a
/// static class send to `class_name` reads, whose value maps each installed
/// call-site key to its class reference. An atom, because hashing an atom
/// `persistent_term` key is much cheaper than hashing a tuple one; the spaces
/// keep it from colliding with a module's key or a class tag (no class name
/// contains a space). The runtime spells it in
/// `beamtalk_class_shadow_flags:static_calls_key/1`;
/// `GuardedStaticCallTest>>testCallSiteReadsTheRuntimesPerClassKey`
/// (`stdlib/test/guarded_static_call_test.bt`) fails if the two differ.
pub(super) fn static_calls_key(class_name: &str) -> String {
    let mut s = metaclass_tag(class_name);
    s.push_str(" static calls");
    s
}

/// The name a class's process is registered under,
/// `"beamtalk_class_{class_name}"` (`beamtalk_class_registry:registry_name/1`).
/// Built at compile time so a static call site looks the class's pid up with
/// one `erlang:whereis/1` instead of `whereis_class/1`'s per-call
/// `binary_to_atom` (BT-4301). Pinned to the Erlang rule, and
/// [`metaclass_tag`] to `class_object_tag/1`, by the shared fixture
/// `class_registered_name_conformance.json`.
pub(super) fn class_registered_name(class_name: &str) -> String {
    const PREFIX: &str = "beamtalk_class_";
    let mut s = String::with_capacity(PREFIX.len() + class_name.len());
    s.push_str(PREFIX);
    s.push_str(class_name);
    s
}

/// Builds a stable, self-contained extension binding name `"_Ext{idx}"`.
///
/// Using the per-module loop index keeps snapshot values stable across unrelated
/// codegen changes that would otherwise shift the global temp-var counter.
/// Collision with `fresh_var("Ext")` (yielding `_Ext1`, `_Ext2`, …) is not a
/// concern in practice because no call site uses `"Ext"` as a `fresh_var` base.
pub(super) fn ext_var(idx: usize) -> String {
    // "_Ext" is 4 bytes; reserve a few more for the digits.
    let mut s = String::with_capacity(8);
    s.push_str("_Ext");
    let _ = write!(s, "{idx}");
    s
}

/// Generate a `'beamtalk_class' = [{...}]` attribute fragment for the
/// module attributes section. Returns `Document::Nil` when classes is empty.
pub(super) fn beamtalk_class_attribute(classes: &[ClassDefinition]) -> Document<'static> {
    if classes.is_empty() {
        return Document::Nil;
    }
    let entries = classes.iter().map(|c| {
        docvec![
            "{",
            atom(c.name.name.to_string()),
            ", ",
            atom(c.superclass_name().to_string()),
            "}"
        ]
    });
    docvec![
        ",\n     'beamtalk_class' = [",
        join(entries, &Document::Str(", ")),
        "]"
    ]
}

/// Filters `@expect` directives from a statement body, returning only the
/// codegen-relevant expressions.  This is the canonical extraction point —
/// every call-site that previously inlined this `.iter().map().filter()` chain
/// should call this helper instead.
pub(super) fn collect_body_exprs(body: &[ExpressionStatement]) -> Vec<&Expression> {
    // The filter is core's, shared with the type checker's
    // `ends_with_self_field_write` (BT-4235).
    beamtalk_core::semantic_analysis::block_facts::body_exprs(body).collect()
}

/// Renders a `usize` index, arity, or size as a Core Erlang integer leaf.
///
/// Wraps [`int_lit`] for the many positional-index call sites in
/// destructuring and pattern-match codegen (`patterns::destructure`,
/// `patterns::match_lowering`) and block-body array destructuring
/// (`blocks::generate_block_array_destructure`), where the value comes from
/// an AST `Vec` length or position and is therefore always non-negative and
/// far below `i64::MAX`. Saturates rather than panicking on the unreachable
/// overflow case so codegen never aborts.
///
/// Split out into this shared leaf module — it
/// has no generator-state dependency, and is used by three separate
/// `core_erlang` submodules (AGENTS.md's no-duplicate-implementations rule).
pub(super) fn index_lit(n: usize) -> Document<'static> {
    int_lit(i64::try_from(n).unwrap_or(i64::MAX))
}

/// ADR 0124 §2/B2: `true` when `decl` (a `state:` or `classState:`
/// declaration) must be left out of an `init/1` state literal entirely,
/// rather than emitted as `<name> => 'nil'` or its default — i.e. a `late`
/// slot with no declared default. Its key then starts absent from the map
/// (ADR 0124 §2's chosen representation for "not yet assigned"), so a later
/// unguarded `maps:get/2` on it raises `badkey` (fixed by B3's guarded read,
/// not this predicate) instead of silently reading `'nil'`.
///
/// A `late` slot that DOES declare a default is not omitted — the default is
/// a real assignment, so the slot still starts present, exactly like an
/// eager one; only being unassigned is what `late` excuses.
///
/// Shared by every state-literal emitter across `core_erlang` (instance-side
/// `init/1` in `gen_server::state`, and the class-side `classState` map in
/// `class_registry`/`gen_server::native_facade`) per AGENTS.md's no-
/// duplicate-implementations rule — this is the one place that decides
/// "does this slot's key appear at all", so those four loops can't drift out
/// of sync on the rule.
pub(super) fn omit_late_defaultless_slot(decl: &StateDeclaration) -> bool {
    decl.slot_kind == SlotKind::Late && decl.default_value.is_none()
}

/// `'file'` module attribute helper for BEAM stacktrace file names.
impl CoreErlangGenerator {
    /// Returns `, 'file' = [{"<path>", 1}]` when `source_path` is set,
    /// or `Document::Nil` when no source path is available.
    ///
    /// The `erlc` compiler uses the `'file'` attribute to populate the BEAM
    /// `Line` chunk file table, making stacktraces show the `.bt` source file.
    pub(super) fn file_attr(&self) -> Document<'static> {
        match &self.source_path {
            Some(path) => {
                docvec![", 'file' = [{", string_lit(path), ", 1}]"]
            }
            None => Document::Nil,
        }
    }
}

/// Value Object: A class's compile-time identity.
///
/// **DDD Context:** Code Generation
///
/// Holds the user-facing class name (from the AST class definition).
/// This decouples class identity from the Erlang module name, which may
/// differ for stdlib classes (e.g., module `bt@stdlib@string` → class `String`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ClassIdentity {
    class_name: String,
    /// Whether this class is sealed (no subclasses allowed).
    /// Enables codegen optimizations: direct dispatch for self-sends.
    is_sealed: bool,
    /// Whether this class is abstract (cannot be instantiated).
    /// Enables codegen optimization: reduced `gen_server` scaffolding.
    is_abstract: bool,
}

impl ClassIdentity {
    /// Create from an AST class name.
    pub fn new(class_name: &str) -> Self {
        Self {
            class_name: class_name.to_string(),
            is_sealed: false,
            is_abstract: false,
        }
    }

    /// Create from an AST class definition with sealed/abstract flags.
    pub fn from_class_def(class_name: &str, is_sealed: bool, is_abstract: bool) -> Self {
        Self {
            class_name: class_name.to_string(),
            is_sealed,
            is_abstract,
        }
    }

    /// The user-facing class name (CamelCase).
    pub fn class_name(&self) -> &str {
        &self.class_name
    }

    /// Whether the class is sealed.
    pub fn is_sealed(&self) -> bool {
        self.is_sealed
    }
}

impl CoreErlangGenerator {
    /// Returns the expression as a `Document` for direct composition via `docvec!`.
    ///
    /// ADR 0018: Simple forwarding to `generate_expression`.
    ///
    /// ADR 0118 phase 1a: in a state-threading context the
    /// expression-level entry point is [`Self::threaded_expression`], which
    /// returns the value together with the prelude of `ThreadedStmt`s
    /// (real `Bind`s) its state-effecting sub-expressions need; the Actor
    /// method body (`lower_body_exprs_with_reply`) splices that prelude. This
    /// function stays a plain forwarder for now rather than becoming
    /// `threaded_expression(expr)?.close(..)` (§Decision 5): every consumer
    /// not yet migrated (branch arms, loop bodies, cascades, `match:`,
    /// interpolation — ADR 0118 phases 1b–4) still reaches nested self-sends
    /// through here, and each of those positions is a row of
    /// `stdlib/test/actor_self_send_position_matrix_test.bt` that `BUnit`
    /// compiles under the dev profile — a `close()` that reported
    /// `StateEffectEscapesExpression` through `report_threaded_ir_verify_errors`
    /// would `debug_assert!`-abort that whole fixture. The un-migrated
    /// positions therefore keep the byte-identical discarding fallback
    /// (`generate_discarding_self_dispatch`) until phase 2b removes the last
    /// `Document`-only consumer, at which point this becomes the `close()`
    /// site and the drop a verifier finding.
    ///
    /// # Errors
    ///
    /// Returns [`CodeGenError`](super::CodeGenError) if generating `expr` fails.
    // Public: `beamtalk-repl` compiles
    // individual expressions this way while assembling a REPL module.
    pub fn expression_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<beamtalk_cerl_doc::Document<'static>> {
        self.generate_expression(expr)
    }

    /// Generates an expression and returns whether it set `repl_loop_mutated`.
    ///
    /// Replaces the manual reset-before/read-after pattern on `repl_loop_mutated`.
    /// Mutation-threaded control flow (loops, conditionals, exception handlers) and inline
    /// value calls set `repl_loop_mutated` deep in the call stack. REPL codegen needs to
    /// know whether the expression returned a `{Result, State}` tuple that must be unpacked.
    /// This method encapsulates the side-channel into an explicit return value.
    ///
    /// # Errors
    ///
    /// Returns [`CodeGenError`](super::CodeGenError) if generating `expr` fails.
    // Public: `beamtalk-repl` calls this while
    // generating REPL-mode expression bodies.
    pub fn expression_doc_with_repl_mutation_tracking(
        &mut self,
        expr: &beamtalk_core::ast::Expression,
    ) -> Result<(beamtalk_cerl_doc::Document<'static>, bool)> {
        self.set_repl_loop_mutated(false);
        let previous = self.set_repl_unpack_site(Some(Self::repl_unpack_span(expr)));
        let doc = self.generate_expression(expr);
        self.set_repl_unpack_site(previous);
        let doc = doc?;
        let mutated = self.repl_loop_mutated();
        self.set_repl_loop_mutated(false);
        Ok((doc, mutated))
    }

    /// Generates a fresh variable name and binds it in the current scope.
    ///
    /// Use this for user-visible bindings (block parameters, assignments, etc.)
    /// where the name should be looked up later via `lookup_var`.
    pub(super) fn fresh_var(&mut self, base: &str) -> String {
        let var_name = self.var_context.fresh_var(base);
        // Bind it in the current scope
        self.var_context.bind(base, &var_name);
        var_name
    }

    /// Generates a fresh temporary variable name WITHOUT binding it in scope.
    ///
    /// Use this for internal codegen temporaries (loop variables, function bindings,
    /// etc.). The name is unique among temps and can never alias a variable
    /// derived from a user identifier (it carries
    /// [`super::variable_context::TEMP_MARKER`], which no Beamtalk identifier
    /// can contain — BT-4140); see
    /// [`super::variable_context::VariableContext::fresh_var`].
    // Public: `beamtalk-repl` mints internal
    // temporaries (e.g. result variables) while assembling a REPL module.
    pub fn fresh_temp_var(&mut self, base: &str) -> String {
        self.var_context.fresh_var(base)
    }

    /// Converts a Beamtalk identifier to a valid Core Erlang variable name.
    ///
    /// Core Erlang variables must start with an uppercase letter or underscore.
    /// This function capitalizes the first letter of the identifier.
    pub(super) fn to_core_erlang_var(name: &str) -> String {
        super::variable_context::VariableContext::to_core_var(name)
    }

    /// Builds an explicit wildcard fallback clause — `<Var> when
    /// 'true' -> call 'erlang':'error'({'case_clause', Var})` — for a `case`
    /// whose other clauses match specific literal/tagged-tuple shapes (e.g. a
    /// boolean `<'true'>`/`<'false'>` case, or a dispatch-result case
    /// matching only `{'reply',...}`/`{'error',...}` shapes) that are not
    /// *statically provable* exhaustive to the Core Erlang compiler, even
    /// when they are exhaustive over every shape the value can genuinely
    /// take at runtime.
    ///
    /// BT-4202: the same clause also sidesteps an OTP 28 `beam_clean` crash
    /// (`undefined_label` / `{jump,{f,0}}`): a boolean-only `case` with no
    /// catch-all whose scrutinee has a branch that provably matches neither
    /// clause (e.g. an inlined pure conditional answering a non-boolean
    /// literal) leaves a jump to a missing label. Pinned by
    /// `boolean_only_case_with_failing_branch_needs_catch_all`.
    ///
    /// Without an explicit wildcard, the compiler synthesizes an implicit
    /// one for the same fallback — but when the `case` sits inside a
    /// `try`'s protected region, OTP's `beam_validator` rejects the
    /// resulting BEAM with `ambiguous_catch_try_state` ("Internal
    /// consistency check failed"), a genuine BEAM-compiler edge case
    /// (confirmed by bisecting the emitted `.core`: on an otherwise-fixed
    /// `case`-in-`try` repro, adding vs. omitting only this clause was the
    /// one difference between a passing and a failing `erlc` run — nesting
    /// depth, tail position, and clause bodies were not the trigger). Making
    /// the fallback explicit is behavior-preserving (a non-matching value
    /// already raises `case_clause` via the compiler's own implicit clause)
    /// and sidesteps the validator bug unconditionally, so callers don't
    /// need to know whether a given call site happens to be nested inside a
    /// `try`.
    pub(super) fn case_clause_fallback(
        &mut self,
        var_prefix: &str,
    ) -> beamtalk_cerl_doc::Document<'static> {
        let var = self.fresh_temp_var(var_prefix);
        Self::case_clause_fallback_doc(var)
    }

    /// The fallback clause [`Self::case_clause_fallback`] builds, over a variable
    /// the caller already owns (a node that renders without a generator, such as
    /// the `on:do:` catch boundary, carries its own).
    pub(super) fn case_clause_fallback_doc(var: String) -> beamtalk_cerl_doc::Document<'static> {
        docvec![" ", Self::case_clause_fallback_clause(var)]
    }

    /// The fallback as a [`beamtalk_cerl_doc::form::clause`] — no leading
    /// space — for sites that build their `case` with `form::case_of`. The one
    /// owner of the fallback's text; [`Self::case_clause_fallback_doc`] adds
    /// the separator for the sites that append it to a hand-written `case`.
    pub(super) fn case_clause_fallback_clause(var: String) -> beamtalk_cerl_doc::Document<'static> {
        use beamtalk_cerl_doc::{form, leaf};
        form::clause(
            [leaf::var(var.clone())],
            None,
            form::call(
                "erlang",
                "error",
                [form::tuple([leaf::atom("case_clause"), leaf::var(var)])],
            ),
            form::Layout::Flat,
        )
    }

    /// Returns the class name for the currently compiled class.
    ///
    /// Prefers the AST-derived class identity when available (set during class
    /// compilation). Falls back to deriving from the module name for backward
    /// compatibility with compilation units that don't set class identity.
    ///
    /// # Examples
    ///
    /// - Module `"counter"` (no class identity) → `"Counter"`
    /// - Module `"bt@stdlib@string"` with class identity `"String"` → `"String"`
    pub(super) fn class_name(&self) -> String {
        if let Some(identity) = self.class_identity() {
            return identity.class_name().to_string();
        }
        // Fall back to deriving from module name (snake_case → CamelCase)
        self.module_name
            .split('_')
            .map(|s| {
                let mut chars = s.chars();
                match chars.next() {
                    None => String::new(),
                    Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                }
            })
            .collect()
    }

    /// Whether the current class is sealed.
    pub(super) fn is_class_sealed(&self) -> bool {
        self.class_identity().is_some_and(ClassIdentity::is_sealed)
    }

    /// Finds the AST class definition currently being generated.
    ///
    /// ADR 0119: replaces the deleted `module_matches_class`, which
    /// answered this same question by re-deriving and comparing module-name
    /// strings (and could spuriously match, e.g. `bt@other_pkg@util@math`
    /// against class `Math`). Under ADR 0040's one-class-per-file rule this
    /// needs no module-name comparison at all — `self.class_name()` (set once
    /// per module by `setup_class_identity`) already identifies the class
    /// being generated directly, so this is a straight identity check against
    /// `module.classes`, not a lookup.
    pub(super) fn current_class<'m>(&self, module: &'m Module) -> Option<&'m ClassDefinition> {
        let current_name = self.class_name();
        module.classes.iter().find(|c| c.name.name == current_name)
    }

    /// Build the `beamtalk_source` Core Erlang attribute fragment.
    ///
    /// Returns `, 'beamtalk_source' = ["<path>"]` when `source_path` is set
    /// and NOT in stdlib mode. Stdlib classes return nil from `sourceFile` at
    /// runtime, but still use `source_path` for line annotations and the
    /// `'file'` attribute (BEAM stacktrace debug info).
    pub(super) fn source_path_attr(&self) -> Document<'static> {
        if self.stdlib_mode() {
            return Document::Nil;
        }
        match &self.source_path {
            Some(path) => {
                docvec![", 'beamtalk_source' = [", string_lit(path), "]"]
            }
            None => Document::Nil,
        }
    }
}

/// Converts class name (`CamelCase`) to module name (`snake_case`).
///
/// Delegates to [`beamtalk_core::ast::to_module_name`] (Shared Kernel).
///
/// Visibility: `pub` to allow usage in IDE queries (hover, completion, etc.)
pub fn to_module_name(class_name: &str) -> String {
    beamtalk_core::ast::to_module_name(class_name)
}

/// Whether an Erlang module atom names a Beamtalk-owned module (BT-4153).
///
/// Delegates to [`beamtalk_core::ast::is_beamtalk_erlang_module`] (Shared
/// Kernel), which the type checker's `ffi-nested-string` hint also uses.
/// Mirrors `beamtalk_erlang_proxy:is_beamtalk_module/1`: `beamtalk_*`
/// hand-written runtime helpers and `bt@*` compiler-generated class modules.
/// Codegen precomputes this for statically-known FFI targets and emits the
/// `stock`/`beamtalk` coercion mode, so the runtime skips its per-call
/// module-name scan.
///
/// The conformance pair `test_is_beamtalk_module_conformance` (below) and
/// `is_beamtalk_module_conformance_test_` in
/// `runtime/apps/beamtalk_runtime/test/beamtalk_erlang_proxy_tests.erl`
/// both read from the shared JSON fixture at
/// `runtime/apps/beamtalk_runtime/test/fixtures/is_beamtalk_module_conformance.json`
/// — adding or changing a case means editing the JSON only.
pub(super) fn is_beamtalk_module(module_name: &str) -> bool {
    beamtalk_core::ast::is_beamtalk_erlang_module(module_name)
}

/// The `stock`/`beamtalk` coercion-mode atom to emit for an FFI call to
/// `module_name` (BT-4153). Precomputes [`is_beamtalk_module`] at compile
/// time for `direct_call/4` and `native_call/5`.
pub(super) fn ffi_coercion_mode(module_name: &str) -> &'static str {
    if is_beamtalk_module(module_name) {
        "beamtalk"
    } else {
        "stock"
    }
}

#[cfg(test)]
mod tests {
    /// BT-4301: [`super::class_registered_name`] and [`super::metaclass_tag`]
    /// agree with `beamtalk_class_registry:registry_name/1` and
    /// `class_object_tag/1`; the Erlang side asserts the identical cases in
    /// `beamtalk_class_registry_tests:class_registered_name_matches_shared_fixture_test/0`.
    #[test]
    fn class_registered_name_matches_shared_fixture() {
        let path = crate::core_erlang::tests::repo_root().join(
            "runtime/apps/beamtalk_runtime/test/fixtures/class_registered_name_conformance.json",
        );
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read fixture {}: {e}", path.display()));
        let cases: Vec<serde_json::Value> =
            serde_json::from_str(&raw).expect("fixture is a JSON array");
        assert!(!cases.is_empty(), "fixture must have cases");
        for case in &cases {
            let class = case["class"].as_str().expect("case.class");
            assert_eq!(
                super::class_registered_name(class),
                case["registered"].as_str().expect("case.registered")
            );
            assert_eq!(
                super::metaclass_tag(class),
                case["tag"].as_str().expect("case.tag")
            );
        }
    }

    use super::*;

    #[test]
    fn test_versioned_var_version_zero_returns_prefix() {
        assert_eq!(versioned_var("State", 0), "State");
        assert_eq!(versioned_var("StateAcc", 0), "StateAcc");
        assert_eq!(versioned_var("Self", 0), "Self");
    }

    #[test]
    fn test_versioned_var_appends_version_number() {
        assert_eq!(versioned_var("State", 1), "State1");
        assert_eq!(versioned_var("State", 2), "State2");
        assert_eq!(versioned_var("StateAcc", 1), "StateAcc1");
        assert_eq!(versioned_var("Self", 5), "Self5");
    }

    #[test]
    fn test_versioned_var_large_version() {
        // Verify capacity pre-allocation does not truncate for larger versions.
        assert_eq!(versioned_var("State", 100), "State100");
        assert_eq!(versioned_var("State", 9999), "State9999");
    }

    #[test]
    fn test_metaclass_tag_appends_class_suffix() {
        assert_eq!(metaclass_tag("Array"), "Array class");
        assert_eq!(metaclass_tag("Object"), "Object class");
        assert_eq!(metaclass_tag("MyApp"), "MyApp class");
    }

    #[test]
    fn test_ext_var_formats_index() {
        assert_eq!(ext_var(0), "_Ext0");
        assert_eq!(ext_var(1), "_Ext1");
        assert_eq!(ext_var(42), "_Ext42");
    }

    #[test]
    fn bt_3431_regression_current_class_survives_file_class_name_mismatch() {
        // The exact scenario this guards against: a `.bt` file whose basename disagrees
        // with the class it declares (`event.bt` containing `class
        // ExduraEvent`) used to silently break gen_server self-dispatch and
        // state-threading codegen. The deleted `module_matches_class`
        // answered "is this the class I'm generating right now?" by
        // re-deriving a module name from the *file-path*-derived module
        // name (`self.module_name`) and comparing it, structurally, against
        // a candidate built from the *class* name — so a real mismatch made
        // the class look like "not itself", with no diagnostic at the point
        // of failure.
        //
        // ADR 0119 replaced that comparison outright: `current_class`
        // identifies the class being generated directly from the parsed AST
        // (`self.class_name()`, set once per module by
        // `setup_class_identity` from the class's own declared name), never
        // by re-deriving and comparing a module-name string — so this exact
        // mismatch shape can no longer affect self-dispatch identity at all.
        let source = "Value subclass: ExduraEvent";
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);

        // The file-path-derived module name ("event", from a hypothetical
        // `event.bt`) deliberately disagrees with the class-name-derived one
        // ("exdura_event") — exactly the mismatch above.
        let mut generator = CoreErlangGenerator::new("event");
        generator.setup_class_identity(&module);

        let found = generator
            .current_class(&module)
            .expect("current_class must find ExduraEvent despite the file/class name mismatch");
        assert_eq!(found.name.name, "ExduraEvent");
    }

    #[test]
    fn test_is_beamtalk_module_conformance() {
        // Single source of truth: shared JSON corpus also used by
        // `is_beamtalk_module_conformance_test_` in
        // runtime/apps/beamtalk_runtime/test/beamtalk_erlang_proxy_tests.erl.
        // To add or change a case, edit the JSON only — both sides pick it up.
        let path = crate::core_erlang::tests::repo_root().join(
            "runtime/apps/beamtalk_runtime/test/fixtures/is_beamtalk_module_conformance.json",
        );
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
        let cases: Vec<serde_json::Value> = serde_json::from_str(&raw)
            .expect("is_beamtalk_module_conformance.json is a JSON array");
        assert!(!cases.is_empty(), "corpus must have cases");
        for case in &cases {
            let module = case["module"].as_str().expect("case.module is a string");
            let is_beamtalk = case["is_beamtalk"]
                .as_bool()
                .expect("case.is_beamtalk is a bool");
            assert_eq!(
                is_beamtalk_module(module),
                is_beamtalk,
                "is_beamtalk_module({module:?}) mismatch"
            );
        }
    }
}
