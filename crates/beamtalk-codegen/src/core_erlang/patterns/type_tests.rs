// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Pattern::Type` (`binding :: ClassName`) runtime-test strategies.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! BT-3465: split out of `expressions.rs`, no logic changes (ADR 0107
//! Phase A / BT-2855, BT-2870, BT-2882). [`CoreErlangGenerator::generate_type_pattern`]
//! picks and applies one of eight per-class runtime-test shapes — see its
//! own doc comment for the full strategy breakdown — dispatched from
//! `generate_match_chain` (`patterns::match_lowering`).

use super::super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Identifier, MatchArm};

/// Which of [`CoreErlangGenerator::generate_type_pattern`]'s runtime-test
/// shapes a class name selects — see that method's doc for the full
/// strategy breakdown. A class name absent from [`TYPE_TESTS`] (an ordinary
/// actor, `Supervisor`/`DynamicSupervisor` subclass, or tagged `Value`
/// subclass) falls through to
/// [`CoreErlangGenerator::dispatch_type_pattern_strategy`]'s
/// hierarchy-dependent default — that choice needs a `self.class_hierarchy`
/// lookup, not just the name, so it cannot be a static table entry (BT-3474).
#[derive(Clone, Copy)]
enum TypeTest {
    /// A guard-safe boolean-returning BIF (`is_binary`, `is_integer`, `is_float`,
    /// `is_list`, `is_function`, `is_pid`, `is_reference`, `is_port`).
    Bif(&'static str),
    /// `Tuple` — any tuple except the reserved actor/supervisor 4-tuple tags.
    Tuple,
    /// `Symbol` — `is_atom` minus `nil`/`true`/`false`.
    Symbol,
    /// `Boolean` — exact `'true'`/`'false'` literal match.
    Boolean,
    /// `True`/`False`/`Nil`/`UndefinedObject` — exact single-atom match.
    Atom(&'static str),
    /// `Dictionary` — tagged-class test for the "no `$beamtalk_class` key" tag.
    UntaggedMap,
}

/// BT-3474: data table replacing `dispatch_type_pattern_strategy`'s
/// class-name match arms for every class whose runtime-test shape depends
/// only on the name (not on `self.class_hierarchy`, unlike the actor/
/// supervisor/generic-tagged-class fallback — see [`TypeTest`]'s doc).
static TYPE_TESTS: &[(&str, TypeTest)] = &[
    ("String", TypeTest::Bif("is_binary")),
    ("Integer", TypeTest::Bif("is_integer")),
    ("Float", TypeTest::Bif("is_float")),
    ("List", TypeTest::Bif("is_list")),
    ("Block", TypeTest::Bif("is_function")),
    ("Pid", TypeTest::Bif("is_pid")),
    ("Reference", TypeTest::Bif("is_reference")),
    ("Port", TypeTest::Bif("is_port")),
    ("Tuple", TypeTest::Tuple),
    ("Symbol", TypeTest::Symbol),
    ("Boolean", TypeTest::Boolean),
    ("True", TypeTest::Atom("true")),
    ("False", TypeTest::Atom("false")),
    ("Nil", TypeTest::Atom("nil")),
    ("UndefinedObject", TypeTest::Atom("nil")),
    ("Dictionary", TypeTest::UntaggedMap),
];

impl CoreErlangGenerator {
    /// Compiles a single `Pattern::Type` match arm (`binding :: ClassName ->
    /// body`) — ADR 0107 Phase A / BT-2855.
    ///
    /// Dispatches to one of four runtime-test shapes based on `class`'s
    /// name, generalizing two existing codegen strategies (no new runtime
    /// mechanism is introduced):
    ///
    /// - **Guard-safe BIF primitives** (`String` → `is_binary`, `Integer` →
    ///   `is_integer`, `Float` → `is_float`, `List` → `is_list`): a
    ///   single-level `case call 'erlang':'is_X'(_Match) of <'true'> -> ...
    ///   <'false'> -> rest end` ([`Self::wrap_bif_test`]), generalizing
    ///   `generate_array_match_arm` (`patterns::match_lowering`)'s outer `is_map` check to an
    ///   arbitrary guard-safe boolean-returning BIF.
    /// - **`Symbol`**: `is_atom(X) andalso X =/= nil andalso X =/= true
    ///   andalso X =/= false` ([`Self::wrap_symbol_test`]), expressed as
    ///   four nested boolean-case tests rather than `andalso` (every
    ///   sub-test is total/side-effect-free, so nesting is equivalent and
    ///   avoids reproducing `andalso`'s try/catch desugaring) — this
    ///   excludes `nil`/`true`/`false` explicitly so an earlier `s ::
    ///   Symbol` arm can never shadow a later `nil ->`/`b :: Boolean` arm
    ///   (all four are plain Erlang atoms with no distinguishing runtime
    ///   tag).
    /// - **`Boolean`**: an exact 3-clause literal match on the scrutinee
    ///   (`'true'`/`'false'`/anything-else-falls-through,
    ///   [`Self::wrap_boolean_test`]) — not a bare `is_atom` guard, for the
    ///   same shadowing reason as `Symbol`.
    /// - **`Dictionary`/exact tagged `Value`/sealed classes**: `maps:get/3`
    ///   is not guard-safe, so [`Self::wrap_class_tag_test`] reuses the
    ///   exact nested-`case` shape `generate_array_match_arm` (`patterns::match_lowering`)
    ///   already uses for its class-tag check: an outer `is_map` case, and
    ///   — only in the `'true'` branch — an inner case on
    ///   `maps:get('$beamtalk_class', _Match, 'undefined')` matching
    ///   `'undefined'` for `Dictionary` (a bare map has no
    ///   `'$beamtalk_class'` key) or the class name atom for a tagged class
    ///   (generalizing `generate_constructor_pattern` (`patterns::match_lowering`)'s map-key
    ///   check, hardcoded to `Result` via `sealed_constructor_fields`, to
    ///   the pattern's `class` field), falling through to `rest` on any
    ///   other class tag.
    /// - **`Block`**: `is_function` — a Beamtalk block compiles to a plain
    ///   Erlang `fun`, never a map, so it needs its own guard-safe BIF
    ///   entry rather than falling into the tagged-class path.
    /// - **`True`/`False`/`Nil`/`UndefinedObject`**: `True`/`False` are
    ///   real (sealed, leaf) stdlib subclasses of `Boolean`, and
    ///   `UndefinedObject` (canonical) / `Nil` (legacy alias, BT-2016) are
    ///   the nil class — all four are resolvable, leaf class names a type
    ///   pattern can legally name, but all four compile to a bare atom
    ///   (`'true'`/`'false'`/`'nil'`), never a map, so
    ///   [`Self::wrap_single_atom_test`] tests the one exact atom directly
    ///   instead of the tagged-class `is_map` check.
    /// - **Actor-hierarchy classes**: an actor reference is
    ///   `{'beamtalk_object', ClassAtom, ModuleAtom, Pid}` — a 4-tuple, not
    ///   a map — so [`Self::wrap_actor_class_tag_test`] tests
    ///   `is_tuple`/`tuple_size`/`element(1)`/`element(2)` instead of the
    ///   map-tag check, reusing the same `is_tuple`/`element(1) ==
    ///   'beamtalk_object'` idiom `fieldAt:`'s actor-vs-map dispatch already
    ///   uses (`intrinsics.rs`), extended to also compare `element(2)`
    ///   (the class name) against the pattern's `class` field.
    /// - **`Supervisor`/`DynamicSupervisor`-hierarchy classes** (BT-2870): a
    ///   live supervisor reference is a third runtime shape, also a 4-tuple
    ///   but tagged `'beamtalk_supervisor'` (or transiently
    ///   `'beamtalk_supervisor_new'` — rewritten to `'beamtalk_supervisor'`
    ///   by `beamtalk_class_dispatch:class_send_dispatch/3` before *any*
    ///   caller, including `class initialize:` itself, ever observes it)
    ///   rather than `'beamtalk_object'` — [`Self::wrap_supervisor_class_tag_test`]
    ///   accepts either tag at `element(1)` before comparing `element(2)`
    ///   against the pattern's `class` field.
    pub(super) fn generate_type_pattern(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        binding: &Identifier,
        class: &Identifier,
        rest_arms: &[MatchArm],
        base_state: Option<&str>,
    ) -> Result<Document<'static>> {
        // Bind `binding` to the whole matched value and generate guard/body
        // FIRST (before rest_arms) so state_version/temp-var counters are
        // not advanced by later arms before this arm's own codegen — same
        // ordering rationale as `generate_array_match_arm`.
        self.push_scope();
        let core_binding = Self::to_core_erlang_var(&binding.name);
        self.bind_var(&binding.name, &core_binding);
        let body_doc = self.generate_match_arm_body(&arm.body, base_state)?;
        self.pop_scope();

        // Rest is generated AFTER the current arm to prevent state leakage.
        let rest_doc = self.generate_match_chain(match_var, rest_arms, base_state)?;

        // Use `case 'true' of <'true'> when GUARD -> body <'true'> when
        // 'true' -> rest end` (Core Erlang guard position) so guard
        // evaluation errors silently fail and fall through, matching Erlang
        // guard semantics — same shape `generate_array_match_arm` uses for
        // its own optional guard.
        let success_doc = if let Some(guard) = &arm.guard {
            let guard_doc = self.generate_guard_expression(guard)?;
            docvec![
                "case 'true' of ",
                "<'true'> when ",
                guard_doc,
                " -> ",
                body_doc,
                " <'true'> when 'true' -> ",
                rest_doc.clone(),
                " end"
            ]
        } else {
            body_doc
        };

        // `binding` denotes the whole matched value — not a decomposed
        // field, unlike `Pattern::Array`'s per-element extraction.
        let bound_success = docvec![
            "let ",
            leaf::var(core_binding),
            " = ",
            leaf::var(match_var.to_string()),
            " in ",
            success_doc
        ];

        Ok(self.dispatch_type_pattern_strategy(match_var, &class.name, bound_success, &rest_doc))
    }

    /// Picks (and applies) one of `generate_type_pattern`'s per-class
    /// runtime-test strategies — factored out purely to keep
    /// `generate_type_pattern` itself under the line-count lint; see that
    /// function's doc comment for the full strategy breakdown.
    ///
    /// BT-3474: `class_name` is looked up in [`TYPE_TESTS`] first — a class
    /// name whose test shape depends only on the name, not on
    /// `self.class_hierarchy`, is table-driven via [`Self::render_type_test`].
    /// The fallback below (actor/supervisor/generic tagged-class) stays a
    /// hand-written default: it needs a hierarchy lookup a static table
    /// keyed by name alone cannot express.
    fn dispatch_type_pattern_strategy(
        &mut self,
        match_var: &str,
        class_name: &str,
        bound_success: Document<'static>,
        rest_doc: &Document<'static>,
    ) -> Document<'static> {
        if let Some((_, test)) = TYPE_TESTS.iter().find(|(name, _)| *name == class_name) {
            return self.render_type_test(*test, match_var, bound_success, rest_doc);
        }

        // BT-2855: an actor reference is a 4-tuple (`{'beamtalk_object',
        // ClassAtom, ModuleAtom, Pid}`), not a map — the tagged-class
        // `is_map` check would never match a live actor instance, silently
        // miscompiling the single most common kind of leaf class in real
        // Beamtalk programs.
        //
        // BT-2882: `is_actor_subclass`/`is_supervisor_subclass`/
        // `is_dynamic_supervisor_subclass` are verified to resolve correctly
        // even when `class_name`'s Actor/Supervisor ancestor is declared in a
        // different file (or several files away) — see their doc comments in
        // `hierarchy_queries.rs` and the
        // `..._resolves_through_cross_file_stub_chain` codegen tests in
        // `tests/control_flow.rs`.
        let is_actor = self
            .class_hierarchy
            .as_ref()
            .is_some_and(|h| h.is_actor_subclass(class_name));
        // BT-2870: a Supervisor/DynamicSupervisor subclass reference is a
        // *different* 4-tuple, tagged `'beamtalk_supervisor'` (or
        // transiently `'beamtalk_supervisor_new'`) rather than
        // `'beamtalk_object'` — same reasoning as the actor case above, just
        // a different reserved tag.
        let is_supervisor = self.class_hierarchy.as_ref().is_some_and(|h| {
            h.is_supervisor_subclass(class_name) || h.is_dynamic_supervisor_subclass(class_name)
        });
        if is_actor {
            self.wrap_actor_class_tag_test(match_var, class_name, bound_success, rest_doc)
        } else if is_supervisor {
            self.wrap_supervisor_class_tag_test(match_var, class_name, bound_success, rest_doc)
        } else {
            self.wrap_class_tag_test(
                match_var,
                leaf::atom(class_name.to_string()),
                bound_success,
                rest_doc,
            )
        }
    }

    /// BT-3474: dispatches a [`TypeTest`] to its renderer — the single
    /// match [`TYPE_TESTS`]-driven strategies share, replacing what was a
    /// 14-arm match keyed directly on class-name string literals.
    fn render_type_test(
        &mut self,
        test: TypeTest,
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        match test {
            TypeTest::Bif(bif) => Self::wrap_bif_test(match_var, bif, success, rest),
            TypeTest::Tuple => self.wrap_tuple_test(match_var, success, rest),
            TypeTest::Symbol => Self::wrap_symbol_test(match_var, success, rest),
            TypeTest::Boolean => self.wrap_boolean_test(match_var, success, rest),
            TypeTest::Atom(atom) => self.wrap_single_atom_test(match_var, atom, success, rest),
            TypeTest::UntaggedMap => {
                self.wrap_class_tag_test(match_var, Document::Str("'undefined'"), success, rest)
            }
        }
    }

    /// `case call 'erlang':'BIF'(_Match) of <'true'> -> success <'false'> ->
    /// rest end` — the single-level generalization of
    /// `generate_array_match_arm`'s outer `is_map` check to an arbitrary
    /// guard-safe boolean-returning BIF (`is_binary`, `is_integer`,
    /// `is_float`, `is_list`). No wildcard fallback clause is needed: these
    /// BIFs only ever return `'true'`/`'false'`, exactly like the existing
    /// `is_map` check's 2-clause case.
    fn wrap_bif_test(
        match_var: &str,
        bif: &'static str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        docvec![
            "case call 'erlang':'",
            Document::Str(bif),
            "'(",
            leaf::var(match_var.to_string()),
            ") of ",
            "<'true'> when 'true' -> ",
            success,
            " <'false'> when 'true' -> ",
            rest.clone(),
            " end"
        ]
    }

    /// `is_atom(X) andalso X =/= nil andalso X =/= true andalso X =/=
    /// false` (ADR 0107 Phase A `Symbol` guard — see
    /// [`Self::generate_type_pattern`]'s doc for why this excludes the
    /// other atom-representation patterns). Expressed as four nested
    /// 2-clause boolean-case tests rather than `andalso`: every sub-test
    /// here is total and side-effect-free (an atom guard BIF or `=/=`), so
    /// nesting evaluates identically without reproducing `andalso`'s
    /// try/catch desugaring.
    fn wrap_symbol_test(
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let v = leaf::var(match_var.to_string());
        let tests: [Document<'static>; 4] = [
            docvec!["call 'erlang':'is_atom'(", v.clone(), ")"],
            docvec!["call 'erlang':'=/='(", v.clone(), ", 'nil')"],
            docvec!["call 'erlang':'=/='(", v.clone(), ", 'true')"],
            docvec!["call 'erlang':'=/='(", v, ", 'false')"],
        ];
        tests.into_iter().rev().fold(success, |acc, test| {
            docvec![
                "case ",
                test,
                " of ",
                "<'true'> when 'true' -> ",
                acc,
                " <'false'> when 'true' -> ",
                rest.clone(),
                " end"
            ]
        })
    }

    /// Exact literal match on `'true'`/`'false'` — not a bare `is_atom`
    /// guard, so an earlier `b :: Boolean` arm can never shadow a later
    /// `nil ->`/`s :: Symbol` arm (ADR 0107 Phase A; see
    /// [`Self::generate_type_pattern`]'s doc). Needs a fresh wildcard
    /// binder (unlike the pure-boolean tests above) because the scrutinee
    /// ranges over more than `{'true', 'false'}`.
    fn wrap_boolean_test(
        &mut self,
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let no_match = self.fresh_temp_var("NoMatch");
        docvec![
            "case ",
            leaf::var(match_var.to_string()),
            " of ",
            "<'true'> when 'true' -> ",
            success.clone(),
            " <'false'> when 'true' -> ",
            success,
            " <",
            leaf::var(no_match),
            "> when 'true' -> ",
            rest.clone(),
            " end"
        ]
    }

    /// `case is_map(_Match) of <'true'> -> case maps:get('$beamtalk_class',
    /// _Match, 'undefined') of <expected_tag> -> success <_> -> rest end
    /// <'false'> -> rest end` — the exact nested-`case` shape
    /// `generate_array_match_arm` already uses for its class-tag check,
    /// generalized to an arbitrary `expected_tag` atom: `'undefined'` for
    /// `Dictionary` (a bare map with no `'$beamtalk_class'` key — see
    /// [`Self::generate_type_pattern`]'s doc) or the class name atom for an
    /// exact tagged `Value`/sealed class (generalizing
    /// `generate_constructor_pattern` (`patterns::match_lowering`)'s map-key check to the
    /// pattern's `class` field).
    fn wrap_class_tag_test(
        &mut self,
        match_var: &str,
        expected_tag: Document<'static>,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let no_match_class = self.fresh_temp_var("NoMatch");
        docvec![
            "case call 'erlang':'is_map'(",
            leaf::var(match_var.to_string()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            leaf::var(match_var.to_string()),
            ", 'undefined') of ",
            "<",
            expected_tag,
            "> when 'true' -> ",
            success,
            " <",
            leaf::var(no_match_class),
            "> when 'true' -> ",
            rest.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            rest.clone(),
            " end"
        ]
    }

    /// Exact literal match on a single atom (`'true'`, `'false'`, or
    /// `'nil'`) — used for the `True`/`False` `Boolean` subclasses and the
    /// nil class (`UndefinedObject`, or its legacy alias `Nil`, BT-2016).
    /// None of these is a map, so [`Self::wrap_class_tag_test`]'s `is_map`
    /// check would never match; a bare `is_atom` guard would also be wrong
    /// (see [`Self::generate_type_pattern`]'s doc on why `Symbol`/`Boolean`
    /// need atom-exclusion rather than a bare `is_atom`) — this tests the
    /// exact expected atom directly, falling through to `rest` on any
    /// other value.
    fn wrap_single_atom_test(
        &mut self,
        match_var: &str,
        atom: &'static str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let no_match = self.fresh_temp_var("NoMatch");
        docvec![
            "case ",
            leaf::var(match_var.to_string()),
            " of ",
            "<'",
            Document::Str(atom),
            "'> when 'true' -> ",
            success,
            " <",
            leaf::var(no_match),
            "> when 'true' -> ",
            rest.clone(),
            " end"
        ]
    }

    /// Tests whether the scrutinee is an actor reference of exactly
    /// `class_name` — an actor reference is `{'beamtalk_object', ClassAtom,
    /// ModuleAtom, Pid}` (a 4-tuple, `beamtalk.hrl`'s `#beamtalk_object{}`
    /// record), never a map, so [`Self::wrap_class_tag_test`]'s `is_map`
    /// check would never match a live actor instance. Reuses the same
    /// `is_tuple`/`tuple_size`/`element(1) == 'beamtalk_object'` idiom
    /// `fieldAt:`'s actor-vs-map dispatch already uses (`intrinsics.rs`),
    /// guarding `tuple_size` before calling `element/2` (which raises
    /// `badarg` on an out-of-range index, unlike the guard-safe BIFs used
    /// elsewhere) so an arbitrary Erlang tuple a caller happens to pass in
    /// falls through to `rest` instead of crashing. `element(2)` (the class
    /// name) is then compared against `class_name` to complete the exact
    /// match.
    fn wrap_actor_class_tag_test(
        &mut self,
        match_var: &str,
        class_name: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        // `is_tuple`/`tuple_size == 4` are boolean-returning (2-clause,
        // `'true'`/`'false'` only — same convention as the outer `is_map`
        // check elsewhere); `element(1)`/`element(2)` return an arbitrary
        // atom, so each needs its own wildcard fallback var.
        let no_match_tag = self.fresh_temp_var("NoMatch");
        let no_match_class = self.fresh_temp_var("NoMatch");
        let v = leaf::var(match_var.to_string());
        docvec![
            "case call 'erlang':'is_tuple'(",
            v.clone(),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'erlang':'=='(call 'erlang':'tuple_size'(",
            v.clone(),
            "), 4) of ",
            "<'true'> when 'true' -> ",
            "case call 'erlang':'element'(1, ",
            v.clone(),
            ") of ",
            "<'beamtalk_object'> when 'true' -> ",
            "case call 'erlang':'element'(2, ",
            v,
            ") of ",
            "<",
            leaf::atom(class_name.to_string()),
            "> when 'true' -> ",
            success,
            " <",
            leaf::var(no_match_class),
            "> when 'true' -> ",
            rest.clone(),
            " end ",
            "<",
            leaf::var(no_match_tag),
            "> when 'true' -> ",
            rest.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            rest.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            rest.clone(),
            " end"
        ]
    }

    /// Tests whether the scrutinee is a supervisor reference (a
    /// `Supervisor`/`DynamicSupervisor` subclass instance) of exactly
    /// `class_name` (BT-2870). A live supervisor reference is always
    /// `{'beamtalk_supervisor', ClassAtom, ModuleAtom, Pid}` by the time any
    /// caller observes it — `beamtalk_class_dispatch:class_send_dispatch/3`
    /// rewrites the transient `{'beamtalk_supervisor_new', ClassAtom,
    /// ModuleAtom, Pid}` fresh-start tag to `'beamtalk_supervisor'` before
    /// `run_initialize` is even called, so `'beamtalk_supervisor_new'` is
    /// never observable by user code, not even inside `class initialize:`
    /// (see `beamtalk_supervisor.erl`'s `startLink/1` doc). The
    /// `'beamtalk_supervisor_new'` arm below is defensive dead code kept for
    /// symmetry with the tag pair, not a reachable runtime case. Neither
    /// shape is a map, so [`Self::wrap_class_tag_test`]'s `is_map` check
    /// would never match; this reuses the same
    /// `is_tuple`/`tuple_size`/`element(1)`/`element(2)` idiom
    /// [`Self::wrap_actor_class_tag_test`] uses, but accepts *either*
    /// reserved tag at `element(1)` before comparing `element(2)` (the
    /// class name) against `class_name`.
    fn wrap_supervisor_class_tag_test(
        &mut self,
        match_var: &str,
        class_name: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let no_match_tag = self.fresh_temp_var("NoMatch");
        let no_match_class = self.fresh_temp_var("NoMatch");
        let v = leaf::var(match_var.to_string());

        // Built once in source; cloned into both accepted tag arms below so
        // the element(2) class-name check isn't copy-pasted in Rust source.
        let class_check = docvec![
            "case call 'erlang':'element'(2, ",
            v.clone(),
            ") of ",
            "<",
            leaf::atom(class_name.to_string()),
            "> when 'true' -> ",
            success,
            " <",
            leaf::var(no_match_class),
            "> when 'true' -> ",
            rest.clone(),
            " end"
        ];

        docvec![
            "case call 'erlang':'is_tuple'(",
            v.clone(),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'erlang':'=='(call 'erlang':'tuple_size'(",
            v.clone(),
            "), 4) of ",
            "<'true'> when 'true' -> ",
            "case call 'erlang':'element'(1, ",
            v,
            ") of ",
            "<'beamtalk_supervisor'> when 'true' -> ",
            class_check.clone(),
            " <'beamtalk_supervisor_new'> when 'true' -> ",
            class_check,
            " <",
            leaf::var(no_match_tag),
            "> when 'true' -> ",
            rest.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            rest.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            rest.clone(),
            " end"
        ]
    }

    /// `Tuple` matches any Erlang tuple **except** the reserved 4-tuple
    /// shapes this compiler uses internally for actor references
    /// (`{'beamtalk_object', ...}`) and supervisor references
    /// (`{'beamtalk_supervisor' | 'beamtalk_supervisor_new', ...}`) —
    /// without this exclusion, a live actor/supervisor reference is *also*
    /// a plain Erlang tuple structurally, so `x :: Tuple` would incorrectly
    /// match it too. (BT-2870: `Supervisor`/`DynamicSupervisor` subclasses
    /// are themselves valid type-pattern `class` names now, handled by
    /// [`Self::wrap_supervisor_class_tag_test`] above — this exclusion
    /// still applies to an *unrelated* `x :: Tuple` arm that could see one
    /// of these values flow through.)
    fn wrap_tuple_test(
        &mut self,
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let v = leaf::var(match_var.to_string());
        let not_reserved = self.fresh_temp_var("NotReserved");
        docvec![
            "case call 'erlang':'is_tuple'(",
            v.clone(),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'erlang':'=='(call 'erlang':'tuple_size'(",
            v.clone(),
            "), 4) of ",
            "<'true'> when 'true' -> ",
            "case call 'erlang':'element'(1, ",
            v,
            ") of ",
            "<'beamtalk_object'> when 'true' -> ",
            rest.clone(),
            " <'beamtalk_supervisor'> when 'true' -> ",
            rest.clone(),
            " <'beamtalk_supervisor_new'> when 'true' -> ",
            rest.clone(),
            " <",
            leaf::var(not_reserved),
            "> when 'true' -> ",
            success.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            success,
            " end ",
            "<'false'> when 'true' -> ",
            rest.clone(),
            " end"
        ]
    }
}
