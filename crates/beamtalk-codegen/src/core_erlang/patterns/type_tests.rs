// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Pattern::Type` (`binding :: ClassName`) runtime-test strategies.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ADR 0107 Phase A. [`CoreErlangGenerator::generate_type_pattern`]
//! picks and applies one of eight per-class runtime-test shapes — see its
//! own doc comment for the full strategy breakdown — dispatched from
//! `generate_match_chain` (`patterns::match_lowering`).

use super::super::routing::{self, Guard};
use super::super::{CoreErlangGenerator, Result};
use super::match_lowering::MatchThreading;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Identifier, MatchArm};

fn true_doc() -> Document<'static> {
    Document::Str("'true'")
}

fn false_doc() -> Document<'static> {
    Document::Str("'false'")
}

impl CoreErlangGenerator {
    /// Compiles a single `Pattern::Type` match arm (`binding :: ClassName ->
    /// body`) — ADR 0107 Phase A.
    ///
    /// Dispatches to one of four runtime-test shapes based on `class`'s
    /// name, generalizing two existing codegen strategies (no new runtime
    /// mechanism is introduced):
    ///
    /// - **`String`** (BT-4173): exactly the `{'beamtalk_string', bytes}`
    ///   2-tuple tag ([`Self::wrap_string_test`]) — a bare binary is a
    ///   `Binary`, never a `String`.
    /// - **`Binary`** (BT-4228): subclass-aware — `is_binary`, else the
    ///   `String` tag test ([`Self::wrap_binary_test`]), so `b :: Binary`
    ///   agrees with `isKindOf: Binary` and also matches a `String`. Write
    ///   byte-vs-text discrimination String-arm-first.
    /// - **Guard-safe BIF primitives** (`Integer` →
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
    /// - **`Boolean`**: the guard-safe `is_boolean` BIF
    ///   ([`Self::wrap_bif_test`]), true for exactly `'true'`/`'false'` —
    ///   not a bare `is_atom` guard, for the same shadowing reason as
    ///   `Symbol`.
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
    ///   `UndefinedObject` (canonical) / `Nil` (legacy alias) are
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
    /// - **`Supervisor`/`DynamicSupervisor`-hierarchy classes**: a
    ///   live supervisor reference is a third runtime shape, also a 4-tuple
    ///   but tagged `'beamtalk_supervisor'` rather than `'beamtalk_object'`
    ///   — [`Self::wrap_supervisor_class_tag_test`] tests that tag at
    ///   `element(1)` before comparing `element(2)` against the pattern's
    ///   `class` field.
    pub(super) fn generate_type_pattern(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        binding: &Identifier,
        class: &Identifier,
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        // Bind `binding` to the whole matched value and generate guard/body
        // FIRST (before rest_arms) so state_version/temp-var counters are
        // not advanced by later arms before this arm's own codegen — same
        // ordering rationale as `generate_array_match_arm`.
        self.push_scope();
        let core_binding = Self::to_core_erlang_var(&binding.name);
        self.bind_var(&binding.name, &core_binding);
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        // Rest is generated AFTER the current arm to prevent state leakage,
        // and bound once: every test shape below falls through from several
        // sites (BT-4141).
        let shared = self.generate_shared_rest(match_var, rest_arms, threading)?;
        let rest_doc = shared.apply();

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

        let test_doc =
            self.dispatch_type_pattern_strategy(match_var, &class.name, bound_success, &rest_doc);
        Ok(shared.bind_around(test_doc))
    }

    /// Picks (and applies) one of `generate_type_pattern`'s per-class
    /// runtime-test strategies — factored out purely to keep
    /// `generate_type_pattern` itself under the line-count lint; see that
    /// function's doc comment for the full strategy breakdown.
    ///
    /// `class_name` is looked up in the routing table first
    /// ([`routing::type_test`], generated from the shared routing fixture,
    /// BT-4292) — a class name whose test shape depends only on the name,
    /// not on `self.class_hierarchy`, is table-driven via
    /// [`Self::render_type_test`]. The fallback below (actor/supervisor/
    /// generic tagged-class) stays a hand-written default: it needs a
    /// hierarchy lookup a static table keyed by name alone cannot express.
    fn dispatch_type_pattern_strategy(
        &mut self,
        match_var: &str,
        class_name: &str,
        bound_success: Document<'static>,
        rest_doc: &Document<'static>,
    ) -> Document<'static> {
        if let Some(test) = routing::type_test(class_name) {
            return self.render_type_test(test, match_var, bound_success, rest_doc);
        }

        // An actor reference is a 4-tuple (`{'beamtalk_object',
        // ClassAtom, ModuleAtom, Pid}`), not a map — the tagged-class
        // `is_map` check would never match a live actor instance, silently
        // miscompiling the single most common kind of leaf class in real
        // Beamtalk programs.
        //
        // `is_actor_subclass`/`is_supervisor_subclass`/
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
        // A Supervisor/DynamicSupervisor subclass reference is a
        // *different* 4-tuple, tagged `'beamtalk_supervisor'` rather than
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

    /// Dispatches a routing-table [`Guard`] to its renderer — the one
    /// renderer per guard kind the shared routing fixture names.
    fn render_type_test(
        &mut self,
        test: Guard,
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        match test {
            Guard::Bif(bif) => Self::wrap_bif_test(match_var, bif, success, rest),
            Guard::StringTag => self.wrap_string_test(match_var, success, rest),
            Guard::BinaryOrStringTag => self.wrap_binary_test(match_var, success, rest),
            Guard::UnalignedBitstring => Self::wrap_bitstring_test(match_var, success, rest),
            Guard::UnreservedTuple => self.wrap_tuple_test(match_var, success, rest),
            Guard::Symbol => Self::wrap_symbol_test(match_var, success, rest),
            Guard::ExactAtom(atom) => self.wrap_single_atom_test(match_var, atom, success, rest),
            Guard::UntaggedMap => self.wrap_untagged_map_test(match_var, success, rest),
        }
    }

    /// `case call 'erlang':'BIF'(_Match) of <'true'> -> success <'false'> ->
    /// rest end` — the single-level generalization of
    /// `generate_array_match_arm`'s outer `is_map` check to an arbitrary
    /// guard-safe boolean-returning BIF (`is_binary`, `is_boolean`, `is_integer`,
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

    /// Tests whether the scrutinee is exactly a `String` —
    /// `{'beamtalk_string', bytes}` (BT-4173). Reuses the same guarded
    /// `is_tuple`/`tuple_size`/`element(1)` idiom
    /// [`Self::wrap_actor_class_tag_test`] uses (`element/2` raises on an
    /// out-of-range index, so `tuple_size` is checked first and an
    /// arbitrary non-tuple falls through to `rest` instead of crashing).
    /// A bare binary never matches: it is a `Binary`, tested by the
    /// `is_binary` BIF entry.
    fn wrap_string_test(
        &mut self,
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let no_match_tag = self.fresh_temp_var("NoMatch");
        let v = leaf::var(match_var.to_string());
        docvec![
            "case call 'erlang':'is_tuple'(",
            v.clone(),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'erlang':'=='(call 'erlang':'tuple_size'(",
            v.clone(),
            "), 2) of ",
            "<'true'> when 'true' -> ",
            "case call 'erlang':'element'(1, ",
            v,
            ") of ",
            "<'beamtalk_string'> when 'true' -> ",
            success,
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

    /// Subclass-aware `Binary` test (BT-4228): `is_binary(X)`, else the
    /// [`Self::wrap_string_test`] tag test. `Binary` is sealed with `String`
    /// as its only subclass, so this two-shape test is exactly
    /// `isKindOf: Binary`. The two shapes compute one `'true'`/`'false'`
    /// that a single `case` branches on, so `success` is emitted once
    /// (BT-4141).
    fn wrap_binary_test(
        &mut self,
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let is_string = self.wrap_string_test(match_var, true_doc(), &false_doc());
        let is_binary = Self::wrap_bif_test(match_var, "is_binary", true_doc(), &is_string);
        Self::branch_on_test(is_binary, success, rest)
    }

    /// `Bitstring` test (ADR 0141): `is_bitstring(X) andalso not
    /// is_binary(X)`, as two nested guard-safe BIF tests computing one
    /// `'true'`/`'false'` that a single `case` branches on, so `success` is
    /// emitted once (BT-4141).
    fn wrap_bitstring_test(
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let not_binary = Self::wrap_bif_test(match_var, "is_binary", false_doc(), &true_doc());
        let is_bitstring = Self::wrap_bif_test(match_var, "is_bitstring", not_binary, &false_doc());
        Self::branch_on_test(is_bitstring, success, rest)
    }

    /// `case <test> of <'true'> -> success <'false'> -> rest end`, for a
    /// test shape rendered with `'true'`/`'false'` at its sites (BT-4141).
    fn branch_on_test(
        test: Document<'static>,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        docvec![
            "case ",
            test,
            " of ",
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
    /// nil class (`UndefinedObject`, or its legacy alias `Nil`).
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
    /// `class_name`. A live supervisor reference is always
    /// `{'beamtalk_supervisor', ClassAtom, ModuleAtom, Pid}`; the runtime has
    /// no other supervisor tag (ADR 0135 §1). It is not a map, so
    /// [`Self::wrap_class_tag_test`]'s `is_map` check would never match; this
    /// reuses the same `is_tuple`/`tuple_size`/`element(1)`/`element(2)`
    /// idiom [`Self::wrap_actor_class_tag_test`] uses, with the supervisor
    /// tag at `element(1)`, before comparing `element(2)` (the class name)
    /// against `class_name`.
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

    /// `Tuple` matches any Erlang tuple **except** the reserved shapes the
    /// runtime gives another meaning: a tuple of a reserved tag's size whose
    /// first element is that tag ([`routing::reserved_tuple_tags`], from the
    /// shared routing fixture — `{'beamtalk_string', _}` is a `String`,
    /// `{'beamtalk_future', _}` a future awaited before dispatch,
    /// `{'beamtalk_object', _, _, _}` an actor or class object and
    /// `{'beamtalk_supervisor', _, _, _}` a supervisor handle). Without these
    /// exclusions each is *also* a plain Erlang tuple structurally, so
    /// `x :: Tuple` would incorrectly match it too.
    /// (`Supervisor`/`DynamicSupervisor` subclasses
    /// are themselves valid type-pattern `class` names, handled by
    /// [`Self::wrap_supervisor_class_tag_test`] above — this exclusion
    /// still applies to an *unrelated* `x :: Tuple` arm that could see one
    /// of these values flow through.)
    ///
    /// The test computes one `'true'`/`'false'`, so `success` is emitted
    /// once (BT-4141).
    fn wrap_tuple_test(
        &mut self,
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let is_tuple = self.tuple_shape_test(match_var);
        Self::branch_on_test(is_tuple, success, rest)
    }

    /// The `Tuple` test's shape ([`Self::wrap_tuple_test`]), evaluating to
    /// `'true'`/`'false'`: `is_tuple`, then for each reserved tag size (in
    /// first-seen fixture order) a `tuple_size` check whose `'true'` branch
    /// rejects that size's tags at `element(1)`, falling through size by
    /// size to `'true'`.
    fn tuple_shape_test(&mut self, match_var: &str) -> Document<'static> {
        let v = leaf::var(match_var.to_string());
        let mut sizes: Vec<(u32, Vec<&'static str>)> = Vec::new();
        for reserved in routing::reserved_tuple_tags() {
            match sizes.iter_mut().find(|(size, _)| *size == reserved.size) {
                Some((_, tags)) => tags.push(reserved.tag),
                None => sizes.push((reserved.size, vec![reserved.tag])),
            }
        }
        let mut unreserved = true_doc();
        for (size, tags) in sizes.into_iter().rev() {
            let not_reserved = self.fresh_temp_var("NotReserved");
            let mut clauses: Vec<Document<'static>> = Vec::with_capacity(tags.len() + 1);
            for tag in tags {
                clauses.push(docvec!["<", leaf::atom(tag), "> when 'true' -> 'false' "]);
            }
            clauses.push(docvec![
                "<",
                leaf::var(not_reserved),
                "> when 'true' -> 'true'"
            ]);
            unreserved = docvec![
                "case call 'erlang':'=='(call 'erlang':'tuple_size'(",
                v.clone(),
                "), ",
                leaf::int_lit(i64::from(size)),
                ") of ",
                "<'true'> when 'true' -> ",
                "case call 'erlang':'element'(1, ",
                v.clone(),
                ") of ",
                Document::Vec(clauses),
                " end ",
                "<'false'> when 'true' -> ",
                unreserved,
                " end"
            ];
        }
        docvec![
            "case call 'erlang':'is_tuple'(",
            v,
            ") of ",
            "<'true'> when 'true' -> ",
            unreserved,
            " <'false'> when 'true' -> 'false' end"
        ]
    }

    /// `Dictionary`: a map from which `beamtalk_tagged_map:class_of/1` reads
    /// no class — no `'$beamtalk_class'` tag (a `maps:get/3` default of
    /// `'undefined'`, the same test [`Self::wrap_class_tag_test`] uses) **and**
    /// no `'__class_mod__'` key, which `class_of/1` honours for a migrated
    /// compiled actor's state (BT-4292). Conservative by one edge: a
    /// `'__class_mod__'` naming a module without `class_name/0` still
    /// dispatches as a `Dictionary` at run time but fails this test, so a
    /// guard built on it only misses (never mis-targets). `success` is
    /// emitted once.
    fn wrap_untagged_map_test(
        &mut self,
        match_var: &str,
        success: Document<'static>,
        rest: &Document<'static>,
    ) -> Document<'static> {
        let tagged = self.fresh_temp_var("Tagged");
        let v = leaf::var(match_var.to_string());
        let is_untagged = docvec![
            "case call 'erlang':'is_map'(",
            v.clone(),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            v.clone(),
            ", 'undefined') of ",
            "<'undefined'> when 'true' -> ",
            "call 'erlang':'not'(call 'erlang':'is_map_key'('__class_mod__', ",
            v,
            ")) ",
            "<",
            leaf::var(tagged),
            "> when 'true' -> 'false' end ",
            "<'false'> when 'true' -> 'false' end"
        ];
        Self::branch_on_test(is_untagged, success, rest)
    }
}
