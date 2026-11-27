// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! General-purpose destructuring-assignment extraction.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! These are the shared helpers used by every body-generation loop (`gen_server` methods,
//! value-type methods, block bodies, loop/conditional/exception bodies,
//! and the REPL via `beamtalk-repl`) to lower a `pattern := value`
//! destructuring assignment to flat `let` bindings.
//!
//! Note: block-body-local destructure dispatch (`{a,b} := expr`/`#[a,b] :=
//! expr` inside a plain block body) lives in [`super::super::blocks`] and
//! calls back into this module's [`CoreErlangGenerator::generate_destructure_bindings`];
//! `match:` pattern lowering lives in [`super::match_lowering`].

use super::super::util::index_lit;
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MapPatternPair, Pattern};

impl CoreErlangGenerator {
    /// Generates flat `let` bindings for a destructuring assignment.
    ///
    /// This is the general-purpose helper used by all body-generation loops
    /// (`gen_server` methods, value-type methods, block bodies, etc.) to handle
    /// `DestructureAssignment` in non-last positions.
    ///
    /// For tuples, uses `erlang:element/2` to extract each position.
    /// For arrays, uses `beamtalk_message_dispatch:send` with `at:`.
    ///
    /// Returns a `Vec<Document>` of `let X = ... in` bindings.
    /// Variables are bound in the current scope for subsequent expressions.
    pub(in crate::core_erlang) fn generate_destructure_bindings(
        &mut self,
        pattern: &Pattern,
        value: &Expression,
    ) -> Result<Vec<Document<'static>>> {
        let (docs, _rhs_var) = self.generate_destructure_bindings_with_value(pattern, value)?;
        Ok(docs)
    }

    /// Like [`Self::generate_destructure_bindings`], also returning the
    /// variable bound to the evaluated right-hand side: the value of the
    /// destructuring assignment, which a body whose last statement it is
    /// answers (BT-3889; `{a, b} := expr` evaluates to `expr`, like
    /// `x := expr`).
    pub(in crate::core_erlang) fn generate_destructure_bindings_with_value(
        &mut self,
        pattern: &Pattern,
        value: &Expression,
    ) -> Result<(Vec<Document<'static>>, String)> {
        let (docs, rhs_var, _bound) =
            self.generate_destructure_extractions(pattern, value, "let ", " in ")?;
        Ok((docs, rhs_var))
    }

    /// Like [`Self::generate_destructure_bindings`] but takes a pre-evaluated variable name
    /// instead of an expression.  Used when the RHS has already been unpacked from a
    /// `{Value, State}` tuple (e.g., `DestructureAssignmentControlFlow`).
    pub(in crate::core_erlang) fn generate_destructure_bindings_from_var(
        &mut self,
        pattern: &Pattern,
        rhs_var: &str,
    ) -> Result<Vec<Document<'static>>> {
        let (docs, _bound) =
            self.generate_pattern_extractions_from_var(pattern, rhs_var, "let ", " in ")?;
        Ok(docs)
    }

    /// Evaluates `value` into a fresh temporary variable, returning the binding document and var name.
    ///
    /// Shared first step for [`Self::generate_destructure_extractions`] (compiled-code) and
    /// [`CoreErlangGenerator::generate_repl_destructure`] (REPL), which both need to evaluate
    /// the RHS before extracting individual elements.
    ///
    /// Returns `(binding_doc, var_name)` where:
    /// - `binding_doc` is the `<indent><var> = <value><terminator>` document
    /// - `var_name` is the temp variable holding the evaluated RHS
    pub(super) fn eval_rhs_to_temp_var(
        &mut self,
        value: &Expression,
        prefix: &str,
        indent: &'static str,
        terminator: &'static str,
    ) -> Result<(Document<'static>, String)> {
        let var = self.fresh_temp_var(prefix);
        let val_doc = self.expression_doc(value)?;
        let binding_doc = docvec![indent, leaf::var(var.clone()), " = ", val_doc, terminator];
        Ok((binding_doc, var))
    }

    /// Core extraction logic shared between compiled-code and REPL destructuring.
    ///
    /// Evaluates `value` into a fresh temp var via [`Self::eval_rhs_to_temp_var`], then
    /// delegates to [`Self::generate_pattern_extractions_from_var`] for the element bindings.
    ///
    /// The `indent` and `terminator` parameters control formatting:
    /// - Non-REPL: `indent = "let "`, `terminator = " in "` (inline expression style)
    /// - REPL: `indent = "    let "`, `terminator = " in\n"` (module body style)
    ///
    /// Returns `(extraction_docs, rhs_var_name, bound_pairs)` where:
    /// - `extraction_docs` is the flat let-binding chain (RHS eval + element extractions)
    /// - `rhs_var_name` is the Core Erlang var holding the evaluated RHS
    /// - `bound_pairs` is `Vec<(beamtalk_name, core_erlang_var)>` for each `Pattern::Variable` bound
    #[allow(clippy::type_complexity)]
    pub(in crate::core_erlang) fn generate_destructure_extractions(
        &mut self,
        pattern: &Pattern,
        value: &Expression,
        indent: &'static str,
        terminator: &'static str,
    ) -> Result<(Vec<Document<'static>>, String, Vec<(String, String)>)> {
        // BT-4366: the element extractions are calls nested in the let chain;
        // they carry the destructuring statement's line.
        let outer_span = self.current_send_span.replace(value.span());
        let result =
            self.generate_destructure_extractions_inner(pattern, value, indent, terminator);
        self.current_send_span = outer_span;
        result
    }

    #[allow(clippy::type_complexity)]
    fn generate_destructure_extractions_inner(
        &mut self,
        pattern: &Pattern,
        value: &Expression,
        indent: &'static str,
        terminator: &'static str,
    ) -> Result<(Vec<Document<'static>>, String, Vec<(String, String)>)> {
        let rhs_prefix = match pattern {
            Pattern::Array { .. } => "Arr",
            Pattern::Tuple { .. } => "Tup",
            Pattern::Map { .. } => "Map",
            Pattern::List { .. } => "List",
            _ => {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "Unsupported destructuring pattern kind".to_string(),
                    span: Some(pattern.span()),
                });
            }
        };
        let (rhs_doc, rhs_var) =
            self.eval_rhs_to_temp_var(value, rhs_prefix, indent, terminator)?;
        let mut docs = vec![rhs_doc];
        let (extraction_docs, bound_pairs) =
            self.generate_pattern_extractions_from_var(pattern, &rhs_var, indent, terminator)?;
        docs.extend(extraction_docs);
        Ok((docs, rhs_var, bound_pairs))
    }

    /// Generates element-extraction let-bindings for a pattern given a pre-evaluated RHS var.
    ///
    /// Unlike [`Self::generate_destructure_extractions`], this function does **not** evaluate
    /// the RHS expression — the caller is responsible for providing an already-evaluated
    /// `rhs_var` (e.g., after unwrapping a mutation-threaded result in REPL mode).
    ///
    /// Returns `(extraction_docs, bound_pairs)` where:
    /// - `extraction_docs` are the element-extraction let-bindings
    /// - `bound_pairs` is `Vec<(beamtalk_name, core_erlang_var)>` for each `Pattern::Variable` bound
    ///
    /// # Errors
    ///
    /// Returns [`CodeGenError`] if the pattern uses an unsupported shape or
    /// element extraction otherwise fails.
    #[allow(clippy::too_many_lines, clippy::type_complexity)]
    // Public: `beamtalk-repl` calls this while destructuring a REPL binding
    // pattern against an already-evaluated RHS.
    pub fn generate_pattern_extractions_from_var(
        &mut self,
        pattern: &Pattern,
        rhs_var: &str,
        indent: &'static str,
        terminator: &'static str,
    ) -> Result<(Vec<Document<'static>>, Vec<(String, String)>)> {
        let mut docs: Vec<Document<'static>> = Vec::new();
        let mut bound_pairs: Vec<(String, String)> = Vec::new();

        match pattern {
            Pattern::Array { elements, rest, .. } => {
                for (idx, elem) in elements.iter().enumerate() {
                    let one_based = index_lit(idx + 1);
                    match elem {
                        // `x :: T` binds like `x`: the annotation is a trusted declared type
                        // for the type checker, not a runtime test (BT-5284).
                        // `_ :: T` (BT-5294) binds nothing, like `_` below.
                        Pattern::Type { binding, .. } if binding.name == "_" => {}
                        Pattern::Variable(id) | Pattern::Type { binding: id, .. } => {
                            let core_var = Self::to_core_erlang_var(&id.name);
                            self.bind_var(&id.name, &core_var);
                            docs.push(docvec![
                                indent,
                                leaf::var(core_var.clone()),
                                " = ",
                                // BT-4366: nested in a `let`/`case`, so the call carries the line itself.
                                self.runtime_send_call(
                                    leaf::var(rhs_var.to_string()),
                                    "at:",
                                    vec![one_based]
                                ),
                                terminator,
                            ]);
                            bound_pairs.push((id.name.to_string(), core_var));
                        }
                        Pattern::Literal(lit, _span) => {
                            // Guard-check: extract the element and assert it equals the literal.
                            // If it doesn't match, raise `#pattern_mismatch` mirroring tuple
                            // literal destructuring.
                            let elem_var = self.fresh_temp_var("Elem");
                            let guard_ok_var = self.fresh_temp_var("GuardOk");
                            let mismatch_var = self.fresh_temp_var("Mismatch");
                            let lit_doc = Self::literal_doc(lit);
                            docs.push(docvec![
                                indent,
                                leaf::var(elem_var.clone()),
                                " = ",
                                // BT-4366: nested in a `let`/`case`, so the call carries the line itself.
                                self.runtime_send_call(
                                    leaf::var(rhs_var.to_string()),
                                    "at:",
                                    vec![one_based]
                                ),
                                terminator,
                            ]);
                            let mismatch_raise = self.pattern_mismatch_raise_doc(
                                rhs_var,
                                "destructure",
                                "value does not match the destructuring pattern",
                            );
                            docs.push(docvec![
                                indent,
                                leaf::var(guard_ok_var),
                                " = case ",
                                leaf::var(elem_var),
                                " of <",
                                lit_doc,
                                "> when 'true' -> 'ok' <",
                                leaf::var(mismatch_var),
                                "> when 'true' -> ",
                                mismatch_raise,
                                " end",
                                terminator,
                            ]);
                        }
                        Pattern::Wildcard(_) => {}
                        _ => {
                            return Err(CodeGenError::UnsupportedFeature {
                                feature: "Nested patterns in array destructuring".to_string(),
                                span: Some(elem.span()),
                            });
                        }
                    }
                }
                // Rest pattern: `...rest` binds remaining elements as a sub-array
                if let Some(rest_pat) = rest
                    && let Pattern::Variable(id) = rest_pat.as_ref()
                {
                    let core_var = Self::to_core_erlang_var(&id.name);
                    self.bind_var(&id.name, &core_var);
                    let from_idx = index_lit(elements.len() + 1);
                    docs.push(docvec![
                        indent,
                        leaf::var(core_var.clone()),
                        " = call 'beamtalk_array':'slice_from'(",
                        leaf::var(rhs_var.to_string()),
                        ", ",
                        from_idx,
                        ")",
                        terminator,
                    ]);
                    bound_pairs.push((id.name.to_string(), core_var));
                }
                // Pattern::Wildcard — no binding needed
            }
            Pattern::Tuple { elements, .. } => {
                // Arity check: verify the tuple has the expected number of elements.
                let expected_arity = index_lit(elements.len());
                let size_ok_var = self.fresh_temp_var("SizeOk");
                let bad_arity_var = self.fresh_temp_var("BadArity");
                let arity_raise = self.pattern_mismatch_raise_doc(
                    rhs_var,
                    "destructure",
                    "value does not match the destructuring pattern",
                );
                docs.push(docvec![
                    indent,
                    leaf::var(size_ok_var),
                    " = case call 'erlang':'tuple_size'(",
                    leaf::var(rhs_var.to_string()),
                    ") of <",
                    expected_arity,
                    "> when 'true' -> 'ok' <",
                    leaf::var(bad_arity_var),
                    "> when 'true' -> ",
                    arity_raise,
                    " end",
                    terminator,
                ]);
                for (idx, elem) in elements.iter().enumerate() {
                    let one_based = index_lit(idx + 1);
                    match elem {
                        // `x :: T` binds like `x`: the annotation is a trusted declared type
                        // for the type checker, not a runtime test (BT-5284).
                        // `_ :: T` (BT-5294) binds nothing, like `_` below.
                        Pattern::Type { binding, .. } if binding.name == "_" => {}
                        Pattern::Variable(id) | Pattern::Type { binding: id, .. } => {
                            let core_var = Self::to_core_erlang_var(&id.name);
                            self.bind_var(&id.name, &core_var);
                            docs.push(docvec![
                                indent,
                                leaf::var(core_var.clone()),
                                " = call 'erlang':'element'(",
                                one_based,
                                ", ",
                                leaf::var(rhs_var.to_string()),
                                ")",
                                terminator,
                            ]);
                            bound_pairs.push((id.name.to_string(), core_var));
                        }
                        Pattern::Literal(lit, _span) => {
                            // Guard-check: extract the element and assert it equals the literal.
                            // If it doesn't match, raise `#pattern_mismatch` like the arity
                            // check above.
                            let elem_var = self.fresh_temp_var("Elem");
                            let guard_ok_var = self.fresh_temp_var("GuardOk");
                            let mismatch_var = self.fresh_temp_var("Mismatch");
                            let lit_doc = Self::literal_doc(lit);
                            docs.push(docvec![
                                indent,
                                leaf::var(elem_var.clone()),
                                " = call 'erlang':'element'(",
                                one_based,
                                ", ",
                                leaf::var(rhs_var.to_string()),
                                ")",
                                terminator,
                            ]);
                            let mismatch_raise = self.pattern_mismatch_raise_doc(
                                rhs_var,
                                "destructure",
                                "value does not match the destructuring pattern",
                            );
                            docs.push(docvec![
                                indent,
                                leaf::var(guard_ok_var),
                                " = case ",
                                leaf::var(elem_var),
                                " of <",
                                lit_doc,
                                "> when 'true' -> 'ok' <",
                                leaf::var(mismatch_var),
                                "> when 'true' -> ",
                                mismatch_raise,
                                " end",
                                terminator,
                            ]);
                        }
                        Pattern::Wildcard(_) => {}
                        _ => {
                            return Err(CodeGenError::UnsupportedFeature {
                                feature: "Nested patterns in tuple destructuring".to_string(),
                                span: Some(elem.span()),
                            });
                        }
                    }
                }
            }
            Pattern::Map { pairs, .. } => {
                // ADR 0155: a pattern with an optional key first checks that the
                // value is a plain `Dictionary`, so an all-optional pattern does
                // not quietly bind `nil` from a value object or another tagged map.
                if pairs.iter().any(MapPatternPair::is_optional) {
                    let ok_var = self.fresh_temp_var("DictOk");
                    let not_dict_var = self.fresh_temp_var("NotDict");
                    let is_dict = self.untagged_map_test_doc(rhs_var);
                    let mismatch_raise = self.pattern_mismatch_raise_doc(
                        rhs_var,
                        "destructure",
                        "value does not match the destructuring pattern",
                    );
                    let check = form::case_of(
                        is_dict,
                        [
                            form::clause(
                                [leaf::atom("true")],
                                None,
                                leaf::atom("ok"),
                                Layout::Flat,
                            ),
                            form::clause(
                                [leaf::var(not_dict_var)],
                                None,
                                mismatch_raise,
                                Layout::Flat,
                            ),
                        ],
                        Layout::Flat,
                    );
                    docs.push(docvec![indent, leaf::var(ok_var), " = ", check, terminator]);
                }
                for pair in pairs {
                    match &pair.value {
                        // `x :: T` binds like `x`: the annotation is a trusted declared type
                        // for the type checker, not a runtime test (BT-5284).
                        // `_ :: T` (BT-5294) binds nothing, like `_` below.
                        Pattern::Type { binding, .. } if binding.name == "_" => {}
                        Pattern::Variable(id) | Pattern::Type { binding: id, .. } => {
                            let core_var = Self::to_core_erlang_var(&id.name);
                            self.bind_var(&id.name, &core_var);
                            let key_doc = super::map_pattern_key_doc(&pair.key);
                            let lookup = if pair.is_optional() {
                                // ADR 0155: an absent optional key binds its default.
                                form::call(
                                    "maps",
                                    "get",
                                    [
                                        key_doc,
                                        leaf::var(rhs_var.to_string()),
                                        super::map_pattern_default_doc(pair),
                                    ],
                                )
                            } else {
                                form::call(
                                    "erlang",
                                    "map_get",
                                    [key_doc, leaf::var(rhs_var.to_string())],
                                )
                            };
                            docs.push(docvec![
                                indent,
                                leaf::var(core_var.clone()),
                                " = ",
                                lookup,
                                terminator,
                            ]);
                            bound_pairs.push((id.name.to_string(), core_var));
                        }
                        Pattern::Wildcard(_) => {}
                        _ => {
                            return Err(CodeGenError::UnsupportedFeature {
                                feature: "Nested patterns in map destructuring".to_string(),
                                span: Some(pair.value.span()),
                            });
                        }
                    }
                }
            }
            Pattern::List { .. } => {
                let doc = self.list_destructure_doc(pattern, rhs_var, &mut bound_pairs)?;
                docs.push(docvec![indent, doc, terminator]);
            }
            _ => {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "Unsupported destructuring pattern kind".to_string(),
                    span: Some(pattern.span()),
                });
            }
        }

        Ok((docs, bound_pairs))
    }

    /// BT-5301: `<Vars> = case Rhs of <ListPattern> when 'true' -> <Vars>
    /// <Other> when 'true' -> raise #pattern_mismatch end` for a list
    /// (cons) destructure, binding every variable of the pattern at once.
    ///
    /// The list pattern is matched natively, so elements and the tail may be
    /// nested tuple, list and map patterns and literals. An element `x :: T`
    /// binds like `x`: in a destructuring assignment the annotation is a
    /// trusted declared type, not a runtime test (BT-5284), so the hoisted
    /// classes are dropped.
    fn list_destructure_doc(
        &mut self,
        pattern: &Pattern,
        rhs_var: &str,
        bound_pairs: &mut Vec<(String, String)>,
    ) -> Result<Document<'static>> {
        if let Some(span) = Self::unsupported_list_destructure_element(pattern) {
            return Err(CodeGenError::UnsupportedFeature {
                feature: "array, binary or constructor pattern nested in a list destructure \
                          — match the value with `match:` instead"
                    .to_string(),
                span: Some(span),
            });
        }
        let mut native = pattern.clone();
        self.hoist_nested_types(&mut native, &mut Vec::new());
        let pattern_doc = self.generate_pattern(&native)?;

        let mut vars: Vec<(String, String)> = Vec::new();
        Self::collect_pattern_variables(pattern, |name, core_var| {
            vars.push((name.to_string(), core_var.to_string()));
        });
        let var_docs = || vars.iter().map(|(_, v)| leaf::var(v.clone()));
        let (binder, result) = match vars.len() {
            0 => {
                let ok = self.fresh_temp_var("ListOk");
                (leaf::var(ok), leaf::atom("ok"))
            }
            1 => (leaf::var(vars[0].1.clone()), leaf::var(vars[0].1.clone())),
            _ => (form::values(var_docs()), form::values(var_docs())),
        };
        let other = self.fresh_temp_var("Mismatch");
        let raise = self.pattern_mismatch_raise_doc(
            rhs_var,
            "destructure",
            "value does not match the destructuring pattern",
        );
        let case_doc = form::case_of(
            leaf::var(rhs_var.to_string()),
            [
                form::clause([pattern_doc], None, result, Layout::Flat),
                form::clause([leaf::var(other)], None, raise, Layout::Flat),
            ],
            Layout::Flat,
        );
        for (name, core_var) in vars {
            self.bind_var(&name, &core_var);
            bound_pairs.push((name, core_var));
        }
        Ok(docvec![binder, " = ", case_doc])
    }

    /// The span of the first element a list destructure cannot match
    /// natively: an array (an opaque tagged map), a binary (a `String`
    /// subject would need unwrapping) or a constructor pattern.
    fn unsupported_list_destructure_element(
        pattern: &Pattern,
    ) -> Option<beamtalk_core::source_analysis::Span> {
        match pattern {
            Pattern::Array { span, .. }
            | Pattern::Binary { span, .. }
            | Pattern::Constructor { span, .. } => Some(*span),
            Pattern::Tuple { elements, .. } => elements
                .iter()
                .find_map(Self::unsupported_list_destructure_element),
            Pattern::List { elements, tail, .. } => elements
                .iter()
                .chain(tail.as_deref())
                .find_map(Self::unsupported_list_destructure_element),
            Pattern::Map { pairs, .. } => pairs
                .iter()
                .find_map(|p| Self::unsupported_list_destructure_element(&p.value)),
            Pattern::Wildcard(_)
            | Pattern::Literal(..)
            | Pattern::Variable(_)
            | Pattern::Nil(_)
            | Pattern::Type { .. } => None,
        }
    }
}
