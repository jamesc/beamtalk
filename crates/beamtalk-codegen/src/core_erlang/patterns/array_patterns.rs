// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Array-pattern match arms (`#[a, b]`, rest and nested element patterns) (split from `match_lowering`, BT-5041).
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::sentinel_tags;
use crate::core_erlang::util::index_lit;
use crate::core_erlang::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Identifier, MatchArm, Pattern};
use std::collections::HashSet;

use super::match_lowering::MatchThreading;

impl CoreErlangGenerator {
    /// Compiles a single `Pattern::Array` match arm.
    ///
    /// Emits a three-level conditional check (`is_map` → class → size), then element
    /// extractions via `at:` dispatch. Every failure branch applies the shared
    /// fallthrough label ([`Self::generate_shared_rest`]).
    ///
    /// Takes `rest_arms` (not a pre-built `rest_doc`) so that the current arm's body
    /// is fully generated before any later arm's codegen advances generator state.
    pub(super) fn generate_array_match_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        elements: &[Pattern],
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        let n = elements.len();

        // Bind all pattern variables (including from nested arrays) into scope
        // before generating guard/body so they can reference the bound names.
        // This MUST happen before rest_arms codegen so state is not leaked.
        self.push_scope();
        Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let guard_doc_opt = if let Some(guard) = &arm.guard {
            Some(self.generate_guard_expression(guard)?)
        } else {
            None
        };
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        // Generate rest AFTER the current arm body to prevent state leakage.
        let shared = self.generate_shared_rest(match_var, rest_arms, threading)?;
        let rest_doc = shared.apply();

        // Build body section: element extractions + optional guard check.
        // Use `case 'true' of <'true'> when GUARD ->` (Core Erlang guard position) so that
        // guard evaluation errors silently fail and fall through, matching Erlang guard semantics.
        // A plain `case GUARD of <'true'>` would propagate evaluation errors as exceptions.
        let success_doc = if let Some(guard_doc) = guard_doc_opt {
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

        // Recursively build element extraction + nested array checks
        let mut bound_vars: HashSet<String> = HashSet::new();
        let inner_body = self.build_array_arm_body(
            match_var,
            elements,
            0,
            success_doc,
            &rest_doc,
            &mut bound_vars,
        )?;

        let no_match_size = self.fresh_temp_var("NoMatch");
        let no_match_class = self.fresh_temp_var("NoMatch");

        Ok(shared.bind_around(docvec![
            "case call 'erlang':'is_map'(",
            leaf::var(match_var.to_string()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'(",
            leaf::atom(sentinel_tags::CLASS_TAG),
            ", ",
            leaf::var(match_var.to_string()),
            ", 'undefined') of ",
            "<'Array'> when 'true' -> ",
            "case call 'beamtalk_array':'size'(",
            leaf::var(match_var.to_string()),
            ") of ",
            "<",
            index_lit(n),
            "> when 'true' -> ",
            inner_body,
            " <",
            leaf::var(no_match_size),
            "> when 'true' -> ",
            rest_doc.clone(),
            " end ",
            "<",
            leaf::var(no_match_class),
            "> when 'true' -> ",
            rest_doc.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            rest_doc,
            " end"
        ]))
    }

    /// Handles a `Pattern::Array` element inside an outer array match arm.
    ///
    /// Extracts the element, verifies it is an `Array` of the right size, then
    /// recurses into its sub-elements before continuing with the outer arm.
    #[allow(clippy::too_many_arguments)]
    fn build_nested_array_element(
        &mut self,
        outer_array_var: &str,
        outer_elements: &[Pattern],
        start: usize,
        one_based: usize,
        inner_elems: &[Pattern],
        continuation: Document<'static>,
        failure_doc: &Document<'static>,
        already_bound: &mut HashSet<String>,
    ) -> Result<Document<'static>> {
        let nested_var = self.fresh_temp_var("ArrElem");
        let n_inner = inner_elems.len();

        let after_nested = self.build_array_arm_body(
            outer_array_var,
            outer_elements,
            start + 1,
            continuation,
            failure_doc,
            already_bound,
        )?;
        let inner_body = self.build_array_arm_body(
            &nested_var,
            inner_elems,
            0,
            after_nested,
            failure_doc,
            already_bound,
        )?;

        let no_match_size_n = self.fresh_temp_var("NoMatch");
        let no_match_class_n = self.fresh_temp_var("NoMatch");

        Ok(docvec![
            "let ",
            leaf::var(nested_var.clone()),
            " = ",
            // BT-4366: nested in a `let`/`case`, so the call carries the line itself.
            self.runtime_send_call(
                leaf::var(outer_array_var.to_string()),
                "at:",
                vec![index_lit(one_based)]
            ),
            " in ",
            "case call 'erlang':'is_map'(",
            leaf::var(nested_var.clone()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'(",
            leaf::atom(sentinel_tags::CLASS_TAG),
            ", ",
            leaf::var(nested_var.clone()),
            ", 'undefined') of ",
            "<'Array'> when 'true' -> ",
            "case call 'beamtalk_array':'size'(",
            leaf::var(nested_var.clone()),
            ") of ",
            "<",
            index_lit(n_inner),
            "> when 'true' -> ",
            inner_body,
            " <",
            leaf::var(no_match_size_n),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end ",
            "<",
            leaf::var(no_match_class_n),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            failure_doc.clone(),
            " end"
        ])
    }

    /// Emits the equality-check extraction for a duplicate `Pattern::Variable` in an
    /// array match arm: `let VarDup = at:[N] in case erlang:=:=(Var, VarDup) of ...`
    fn build_array_variable_element(
        &mut self,
        array_var: &str,
        core_var: String,
        one_based: usize,
        next: Document<'static>,
        failure_doc: &Document<'static>,
    ) -> Document<'static> {
        let dup_var = self.fresh_temp_var(&format!("{core_var}Dup"));
        let mismatch_var = self.fresh_temp_var("Mismatch");
        docvec![
            "let ",
            leaf::var(dup_var.clone()),
            " = ",
            // BT-4366: nested in a `let`/`case`, so the call carries the line itself.
            self.runtime_send_call(
                leaf::var(array_var.to_string()),
                "at:",
                vec![index_lit(one_based)]
            ),
            " in ",
            "case call 'erlang':'=:='(",
            leaf::var(core_var),
            ", ",
            leaf::var(dup_var),
            ") of ",
            "<'true'> when 'true' -> ",
            next,
            " <",
            leaf::var(mismatch_var),
            "> when 'true' -> ",
            failure_doc.clone(),
            " end"
        ]
    }

    /// BT-5294: an `x :: T` / `_ :: T` array element — extract it, then
    /// class-test it like a top-level type arm; a miss runs `failure_doc`.
    #[allow(clippy::too_many_arguments)]
    fn build_array_type_element(
        &mut self,
        array_var: &str,
        elements: &[Pattern],
        start: usize,
        (binding, class): (&Identifier, &Identifier),
        continuation: Document<'static>,
        failure_doc: &Document<'static>,
        already_bound: &mut HashSet<String>,
    ) -> Result<Document<'static>> {
        let discard = binding.name == "_";
        let core_var = if discard {
            self.fresh_temp_var("TypeElem")
        } else {
            Self::to_core_erlang_var(&binding.name)
        };
        // A repeated name needs an equality check against its first binding
        // (array arms allow duplicates), so extract into a fresh variable.
        let duplicate = !discard && already_bound.contains(binding.name.as_str());
        if !discard && !duplicate {
            already_bound.insert(binding.name.to_string());
        }
        let next = self.build_array_arm_body(
            array_var,
            elements,
            start + 1,
            continuation,
            failure_doc,
            already_bound,
        )?;
        let (elem_var, next) = if duplicate {
            let dup_var = self.fresh_temp_var("TypeDup");
            let mismatch_var = self.fresh_temp_var("Mismatch");
            let check = docvec![
                "case call 'erlang':'=:='(",
                leaf::var(core_var),
                ", ",
                leaf::var(dup_var.clone()),
                ") of <'true'> when 'true' -> ",
                next,
                " <",
                leaf::var(mismatch_var),
                "> when 'true' -> ",
                failure_doc.clone(),
                " end"
            ];
            (dup_var, check)
        } else {
            (core_var, next)
        };
        let tested = self.dispatch_type_pattern_strategy(&elem_var, &class.name, next, failure_doc);
        Ok(docvec![
            "let ",
            leaf::var(elem_var),
            " = ",
            self.runtime_send_call(
                leaf::var(array_var.to_string()),
                "at:",
                vec![index_lit(start + 1)]
            ),
            " in ",
            tested
        ])
    }

    /// Recursively builds element-extraction `let`-bindings for an array pattern arm.
    ///
    /// Handles nested `Pattern::Array` elements by wrapping the continuation in a
    /// sub-array check. Variables are pre-registered in scope by
    /// [`Self::generate_array_match_arm`] via [`Self::collect_pattern_variables`].
    ///
    /// `continuation` is what to execute after all elements are extracted.
    /// `failure_doc` is what to execute if any nested array check fails.
    /// `already_bound` tracks variable names already extracted in this arm; a second
    /// occurrence emits an `erlang:=:=` equality check rather than a new binding.
    fn build_array_arm_body(
        &mut self,
        array_var: &str,
        elements: &[Pattern],
        start: usize,
        continuation: Document<'static>,
        failure_doc: &Document<'static>,
        already_bound: &mut HashSet<String>,
    ) -> Result<Document<'static>> {
        if start >= elements.len() {
            return Ok(continuation);
        }

        let one_based = start + 1;
        match &elements[start] {
            Pattern::Variable(id) => {
                let core_var = Self::to_core_erlang_var(&id.name);
                if already_bound.contains(id.name.as_str()) {
                    // Duplicate: build rest first, then wrap with equality check.
                    let next = self.build_array_arm_body(
                        array_var,
                        elements,
                        start + 1,
                        continuation,
                        failure_doc,
                        already_bound,
                    )?;
                    Ok(self.build_array_variable_element(
                        array_var,
                        core_var,
                        one_based,
                        next,
                        failure_doc,
                    ))
                } else {
                    // First occurrence: register BEFORE recursing so later positions
                    // with the same name are recognised as duplicates.
                    already_bound.insert(id.name.to_string());
                    let next = self.build_array_arm_body(
                        array_var,
                        elements,
                        start + 1,
                        continuation,
                        failure_doc,
                        already_bound,
                    )?;
                    Ok(docvec![
                        "let ",
                        leaf::var(core_var),
                        " = ",
                        // BT-4366: nested in a `let`/`case`, so the call carries the line itself.
                        self.runtime_send_call(
                            leaf::var(array_var.to_string()),
                            "at:",
                            vec![index_lit(one_based)]
                        ),
                        " in ",
                        next
                    ])
                }
            }
            Pattern::Wildcard(_) => self.build_array_arm_body(
                array_var,
                elements,
                start + 1,
                continuation,
                failure_doc,
                already_bound,
            ),
            Pattern::Array {
                elements: inner_elems,
                ..
            } => {
                let inner_elems_clone: Vec<Pattern> = inner_elems.clone();
                self.build_nested_array_element(
                    array_var,
                    elements,
                    start,
                    one_based,
                    &inner_elems_clone,
                    continuation,
                    failure_doc,
                    already_bound,
                )
            }
            // BT-5294: `x :: T` / `_ :: T` element — extract the element, then
            // class-test it like a top-level type arm; a miss falls through.
            Pattern::Type { binding, class, .. } => self.build_array_type_element(
                array_var,
                elements,
                start,
                (binding, class),
                continuation,
                failure_doc,
                already_bound,
            ),
            elem => Err(CodeGenError::UnsupportedFeature {
                feature: "Unsupported pattern in Array match arm element".to_string(),
                span: Some(elem.span()),
            }),
        }
    }
}
