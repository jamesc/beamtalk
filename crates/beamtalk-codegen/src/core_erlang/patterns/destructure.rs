// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! General-purpose destructuring-assignment extraction.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! BT-3465: split out of `expressions.rs`, no logic changes. These are the
//! shared helpers used by every body-generation loop (`gen_server` methods,
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
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, Pattern};

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
        let (docs, _rhs_var, _bound) =
            self.generate_destructure_extractions(pattern, value, "let ", " in ")?;
        Ok(docs)
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
    pub(super) fn generate_destructure_extractions(
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
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` calls this while
    // destructuring a REPL binding pattern against an already-evaluated RHS.
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
                        Pattern::Variable(id) => {
                            let core_var = Self::to_core_erlang_var(&id.name);
                            self.bind_var(&id.name, &core_var);
                            docs.push(docvec![
                                indent,
                                leaf::var(core_var.clone()),
                                " = call 'beamtalk_message_dispatch':'send'(",
                                leaf::var(rhs_var.to_string()),
                                ", 'at:', [",
                                one_based,
                                "])",
                                terminator,
                            ]);
                            bound_pairs.push((id.name.to_string(), core_var));
                        }
                        Pattern::Literal(lit, _span) => {
                            // Guard-check: extract the element and assert it equals the literal.
                            // If it doesn't match, raise `{badmatch, Array}` mirroring tuple
                            // literal destructuring.
                            let elem_var = self.fresh_temp_var("Elem");
                            let guard_ok_var = self.fresh_temp_var("GuardOk");
                            let mismatch_var = self.fresh_temp_var("Mismatch");
                            let lit_doc = self.generate_literal(lit)?;
                            docs.push(docvec![
                                indent,
                                leaf::var(elem_var.clone()),
                                " = call 'beamtalk_message_dispatch':'send'(",
                                leaf::var(rhs_var.to_string()),
                                ", 'at:', [",
                                one_based,
                                "])",
                                terminator,
                            ]);
                            docs.push(docvec![
                                indent,
                                leaf::var(guard_ok_var),
                                " = case ",
                                leaf::var(elem_var),
                                " of <",
                                lit_doc,
                                "> when 'true' -> 'ok' <",
                                leaf::var(mismatch_var),
                                "> when 'true' -> call 'erlang':'error'({'badmatch', ",
                                leaf::var(rhs_var.to_string()),
                                "}) end",
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
                if let Some(rest_pat) = rest {
                    if let Pattern::Variable(id) = rest_pat.as_ref() {
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
            }
            Pattern::Tuple { elements, .. } => {
                // Arity check: verify the tuple has the expected number of elements.
                let expected_arity = index_lit(elements.len());
                let size_ok_var = self.fresh_temp_var("SizeOk");
                let bad_arity_var = self.fresh_temp_var("BadArity");
                docs.push(docvec![
                    indent,
                    leaf::var(size_ok_var),
                    " = case call 'erlang':'tuple_size'(",
                    leaf::var(rhs_var.to_string()),
                    ") of <",
                    expected_arity,
                    "> when 'true' -> 'ok' <",
                    leaf::var(bad_arity_var),
                    "> when 'true' -> call 'erlang':'error'({'badmatch', ",
                    leaf::var(rhs_var.to_string()),
                    "}) end",
                    terminator,
                ]);
                for (idx, elem) in elements.iter().enumerate() {
                    let one_based = index_lit(idx + 1);
                    match elem {
                        Pattern::Variable(id) => {
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
                            // If it doesn't match, raise `{badmatch, Tuple}` to mirror the arity
                            // check error format above.
                            let elem_var = self.fresh_temp_var("Elem");
                            let guard_ok_var = self.fresh_temp_var("GuardOk");
                            let mismatch_var = self.fresh_temp_var("Mismatch");
                            let lit_doc = self.generate_literal(lit)?;
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
                            docs.push(docvec![
                                indent,
                                leaf::var(guard_ok_var),
                                " = case ",
                                leaf::var(elem_var),
                                " of <",
                                lit_doc,
                                "> when 'true' -> 'ok' <",
                                leaf::var(mismatch_var),
                                "> when 'true' -> call 'erlang':'error'({'badmatch', ",
                                leaf::var(rhs_var.to_string()),
                                "}) end",
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
                for pair in pairs {
                    match &pair.value {
                        Pattern::Variable(id) => {
                            let core_var = Self::to_core_erlang_var(&id.name);
                            self.bind_var(&id.name, &core_var);
                            let key_doc = super::map_pattern_key_doc(&pair.key);
                            docs.push(docvec![
                                indent,
                                leaf::var(core_var.clone()),
                                " = call 'erlang':'map_get'(",
                                key_doc,
                                ", ",
                                leaf::var(rhs_var.to_string()),
                                ")",
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
            _ => {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: "Unsupported destructuring pattern kind".to_string(),
                    span: Some(pattern.span()),
                });
            }
        }

        Ok((docs, bound_pairs))
    }
}
