// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `match:` arms whose map pattern has an optional key (ADR 0155).
//!
//! An Erlang map pattern cannot say "maybe absent", so an optional pair is
//! left out of the native pattern. The arm matches the required pairs
//! natively, checks that the value is a plain `Dictionary`, reads each
//! optional key, runs every type test in source order, and runs the `when:`
//! guard last with every binding in scope. Any miss falls through to the
//! later arms through the shared rest label ([`super::match_lowering`]).

use super::super::{CoreErlangGenerator, Result};
use super::match_lowering::MatchThreading;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Identifier, MapPatternPair, MatchArm, Pattern};

/// Whether `pattern` is a map pattern with an optional key. Optional keys
/// are only allowed at the top of an arm or destructure (the parser rejects
/// them below that), so only the top level is checked.
pub(super) fn pattern_has_optional_key(pattern: &Pattern) -> bool {
    matches!(pattern, Pattern::Map { pairs, .. } if pairs.iter().any(MapPatternPair::is_optional))
}

/// One test an optional-key arm runs after the native match, in source order.
enum ArmTest {
    /// A required pair's nested `x :: T`, hoisted to `var`.
    Class { var: String, class: Identifier },
    /// An optional pair's `x :: T`: `ok_var` is `'true'` when the key is
    /// absent or its value is a `T`.
    Optional { ok_var: String },
}

impl CoreErlangGenerator {
    /// Compiles a `match:` arm whose top-level map pattern has an optional
    /// key (ADR 0155), e.g. `#{"id" => id, ?"priority" => p :: Integer} -> ...`:
    ///
    /// ```text
    /// case _Match of
    ///   <~{ <"id"> := Id }~> when 'true' ->
    ///     case <untagged-map test on _Match> of
    ///       <'true'> when 'true' ->
    ///         let <_OptOk, P> = case call 'maps':'find'(<"priority">, _Match) of
    ///             <{'ok', V}> when 'true' -> <TEST(V), V>
    ///             <'error'> when 'true' -> <'true', 'nil'> end in
    ///         case _OptOk of <'true'> -> body  <'false'> -> rest end
    ///       <'false'> when 'true' -> rest
    ///     end
    ///   <_NoMatch> when 'true' -> rest
    /// end
    /// ```
    ///
    /// An optional pair without a type test is one
    /// `let P = call 'maps':'get'(K, _Match, Default)`.
    #[allow(clippy::too_many_lines)] // one linear lowering, in emission order
    pub(super) fn generate_optional_map_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        pairs: &[MapPatternPair],
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        // The native part: the required pairs, with their nested type tests
        // hoisted out exactly as `generate_nested_type_arm` does.
        let mut outer = Pattern::Map {
            pairs: pairs.iter().filter(|p| !p.is_optional()).cloned().collect(),
            span: arm.pattern.span(),
        };
        let mut hoisted = Vec::new();
        self.hoist_nested_types(&mut outer, &mut hoisted);
        let mut hoisted = hoisted.into_iter();

        // One lookup per optional pair and one test per type test, both in
        // source order.
        let mut lookups: Vec<(Vec<Document<'static>>, Document<'static>)> = Vec::new();
        let mut tests: Vec<ArmTest> = Vec::new();
        let mut optional_bindings: Vec<(String, String)> = Vec::new();
        for pair in pairs {
            if !pair.is_optional() {
                if matches!(pair.value, Pattern::Type { .. }) {
                    let (var, class) = hoisted
                        .next()
                        .expect("hoist_nested_types hoists each required type pair in order");
                    tests.push(ArmTest::Class { var, class });
                }
                continue;
            }
            let key = super::map_pattern_key_doc(&pair.key);
            let default = super::map_pattern_default_doc(pair);
            match &pair.value {
                Pattern::Variable(id) => {
                    let var = Self::to_core_erlang_var(&id.name);
                    optional_bindings.push((id.name.to_string(), var.clone()));
                    lookups.push((
                        vec![leaf::var(var)],
                        form::call(
                            "maps",
                            "get",
                            [key, leaf::var(match_var.to_string()), default],
                        ),
                    ));
                }
                Pattern::Type { binding, class, .. } => {
                    let var = if binding.name == "_" {
                        self.fresh_temp_var("TypeNested")
                    } else {
                        let var = Self::to_core_erlang_var(&binding.name);
                        optional_bindings.push((binding.name.to_string(), var.clone()));
                        var
                    };
                    let ok_var = self.fresh_temp_var("OptOk");
                    let found = self.fresh_temp_var("Found");
                    let is_class = self.dispatch_type_pattern_strategy(
                        &found,
                        &class.name,
                        leaf::atom("true"),
                        &leaf::atom("false"),
                    );
                    let find = form::case_of(
                        form::call("maps", "find", [key, leaf::var(match_var.to_string())]),
                        [
                            form::clause(
                                [form::tuple([leaf::atom("ok"), leaf::var(found.clone())])],
                                None,
                                form::values([is_class, leaf::var(found)]),
                                Layout::Flat,
                            ),
                            form::clause(
                                [leaf::atom("error")],
                                None,
                                form::values([leaf::atom("true"), default]),
                                Layout::Flat,
                            ),
                        ],
                        Layout::Flat,
                    );
                    lookups.push((vec![leaf::var(ok_var.clone()), leaf::var(var)], find));
                    tests.push(ArmTest::Optional { ok_var });
                }
                // `?k => _` binds and tests nothing (the parser warns).
                _ => {}
            }
        }

        // Guard and body see the required and the optional bindings.
        self.push_scope();
        Self::collect_pattern_variables(&outer, |name, core_var| {
            self.bind_var(name, core_var);
        });
        for (name, var) in &optional_bindings {
            self.bind_var(name, var);
        }
        let guard_doc = match &arm.guard {
            Some(guard) => Some(self.generate_guard_expression(guard)?),
            None => None,
        };
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        let shared = self.generate_shared_rest(match_var, rest_arms, threading)?;
        let rest_doc = shared.apply();
        let outer_doc = self.generate_pattern(&outer)?;

        // Innermost first: guard, then the tests (last test innermost), then
        // the lookups, then the `Dictionary` check.
        let mut inner = match guard_doc {
            Some(guard_doc) => Self::guarded_success_doc(guard_doc, body_doc, &rest_doc),
            None => body_doc,
        };
        for test in tests.into_iter().rev() {
            inner = match test {
                ArmTest::Class { var, class } => {
                    self.dispatch_type_pattern_strategy(&var, &class.name, inner, &rest_doc)
                }
                ArmTest::Optional { ok_var } => form::case_of(
                    leaf::var(ok_var),
                    [
                        form::clause([leaf::atom("true")], None, inner, Layout::Flat),
                        form::clause([leaf::atom("false")], None, rest_doc.clone(), Layout::Flat),
                    ],
                    Layout::Flat,
                ),
            };
        }
        for (vars, value) in lookups.into_iter().rev() {
            inner = if vars.len() == 1 {
                let var = vars.into_iter().next().expect("one variable");
                form::let_in(var, value, inner, Layout::Flat)
            } else {
                form::let_values(vars, value, inner, Layout::Flat)
            };
        }
        let is_dict = self.untagged_map_test_doc(match_var);
        let not_dict = self.fresh_temp_var("NotDict");
        inner = form::case_of(
            is_dict,
            [
                form::clause([leaf::atom("true")], None, inner, Layout::Flat),
                form::clause([leaf::var(not_dict)], None, rest_doc.clone(), Layout::Flat),
            ],
            Layout::Flat,
        );

        let fallthrough = self.fresh_temp_var("NoMatch");
        Ok(shared.bind_around(form::case_of(
            leaf::var(match_var.to_string()),
            [
                form::clause([outer_doc], None, inner, Layout::Flat),
                form::clause([leaf::var(fallthrough)], None, rest_doc, Layout::Flat),
            ],
            Layout::Flat,
        )))
    }
}
