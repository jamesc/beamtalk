// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method dispatch and routing code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the method table, `has_method/1`, `safe_dispatch/3`, and
//! `dispatch/4` functions for runtime message routing.

use super::super::document::{Document, INDENT, line, nest};
use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, MethodKind, Module};
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates the method table mapping selector names to arities.
    ///
    /// This creates a map from method selector names (as atoms) to their arities (integers).
    /// Used at runtime for reflection and method lookup.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'method_table'/0 = fun () ->
    ///     ~{'increment' => 0, 'value' => 0}~
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_method_table(
        &self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Collect methods from expression-based definitions (legacy)
        let mut methods: Vec<(String, usize)> = module
            .expressions
            .iter()
            .filter_map(|expr| {
                if let Expression::Assignment { target, value, .. } = expr {
                    if let (Expression::Identifier(id), Expression::Block(block)) =
                        (target.as_ref(), value.as_ref())
                    {
                        return Some((id.name.to_string(), block.arity()));
                    }
                }
                None
            })
            .collect();

        // Collect methods from class definitions
        for class in &module.classes {
            for method in &class.methods {
                // Only include primary methods in the method table for now
                if method.kind == MethodKind::Primary {
                    methods.push((method.selector.name().to_string(), method.selector.arity()));
                }
            }
        }

        // ADR 0006 Phase 1b: Reflection methods (class, respondsTo:, instVarNames,
        // instVarAt:, instVarAt:put:, perform:, perform:withArguments:) are now
        // inherited from Object via hierarchy walking.

        let entries: Vec<String> = methods
            .iter()
            .enumerate()
            .map(|(i, (name, arity))| {
                if i > 0 {
                    format!(", '{name}' => {arity}")
                } else {
                    format!("'{name}' => {arity}")
                }
            })
            .collect();
        let entries_str = entries.join("");

        let doc = docvec![
            "'method_table'/0 = fun () ->",
            nest(INDENT, docvec![line(), format!("~{{{entries_str}}}~"),]),
            "\n\n",
        ];

        Ok(doc)
    }

    /// Generates the `has_method/1` function for runtime reflection.
    ///
    /// This function enables the `respondsTo:` reflection method to check
    /// if an actor class implements a particular method. It returns `true`
    /// if the method exists, `false` otherwise.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'has_method'/1 = fun (Selector) ->
    ///     call 'lists':'member'(Selector, ['increment', 'decrement', 'getValue', 'setValue:'])
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_has_method(
        &self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Collect methods from expression-based definitions (legacy)
        let mut methods: Vec<String> = module
            .expressions
            .iter()
            .filter_map(|expr| {
                if let Expression::Assignment { target, value, .. } = expr {
                    if let (Expression::Identifier(id), Expression::Block(_)) =
                        (target.as_ref(), value.as_ref())
                    {
                        return Some(id.name.to_string());
                    }
                }
                None
            })
            .collect();

        // Collect methods from class definitions
        for class in &module.classes {
            for method in &class.methods {
                // Only include primary methods (matching method_table behavior)
                if method.kind == MethodKind::Primary {
                    methods.push(method.selector.name().to_string());
                }
            }
        }

        // ADR 0006 Phase 1b: Reflection methods are inherited from Object.

        let method_list: Vec<String> = methods
            .iter()
            .enumerate()
            .map(|(i, name)| {
                if i > 0 {
                    format!(", '{name}'")
                } else {
                    format!("'{name}'")
                }
            })
            .collect();
        let method_list_str = method_list.join("");

        let doc = docvec![
            "'has_method'/1 = fun (Selector) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    format!("call 'lists':'member'(Selector, [{method_list_str}])"),
                ]
            ),
            "\n\n",
        ];

        Ok(doc)
    }

    /// Generates the `safe_dispatch/3` function with error isolation.
    ///
    /// Per BT-29 design doc, errors in method dispatch are caught and returned
    /// to the caller rather than crashing the actor instance.
    ///
    /// Note: Core Erlang try expression uses simple variable patterns (not case-style).
    /// Stacktrace is captured but not returned to caller to prevent information leakage.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'safe_dispatch'/3 = fun (Selector, Args, State) ->
    ///     let Self = call 'beamtalk_actor':'make_self'(State) in
    ///     try call 'module':'dispatch'(Selector, Args, Self, State)
    ///     of Result -> Result
    ///     catch <Type, Error, _Stacktrace> ->
    ///         {'error', {Type, Error}, State}
    /// ```
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_safe_dispatch(
        &mut self,
    ) -> Result<Document<'static>> {
        let module_name = &self.module_name;

        let doc = docvec![
            "'safe_dispatch'/3 = fun (Selector, Args, State) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    // Construct Self object reference using beamtalk_actor:make_self/1 (BT-161)
                    "let Self = call 'beamtalk_actor':'make_self'(State) in",
                    line(),
                    // Core Erlang try uses simple variable patterns in of/catch, not case-style
                    format!("try call '{module_name}':'dispatch'(Selector, Args, Self, State)"),
                    line(),
                    "of Result -> Result",
                    line(),
                    // Capture but don't return stacktrace to prevent information leakage
                    "catch <Type, Error, _Stacktrace> -> {'error', {Type, Error}, State}",
                ]
            ),
            "\n\n",
        ];
        Ok(doc)
    }

    /// Generates the dispatch/4 function for message routing.
    ///
    /// This function is complex because it handles both legacy expression-based modules
    /// and class definitions, including the `doesNotUnderstand:args:` fallback per BT-29.
    #[expect(
        clippy::too_many_lines,
        reason = "dispatch codegen is inherently complex"
    )]
    pub(in crate::codegen::core_erlang) fn generate_dispatch(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let mut docs: Vec<Document<'static>> = Vec::new();

        // Write function header
        let header_doc = docvec![
            "'dispatch'/4 = fun (Selector, Args, Self, State) ->",
            nest(INDENT, docvec![line(), "case Selector of",]),
            "\n",
        ];
        docs.push(header_doc);

        // Case clauses are at indent level 2 (inside function + inside case)
        let case_clause_indent: isize = 2;

        // Generate case clause for each method in the module (legacy expression-based)
        for expr in &module.expressions {
            if let Expression::Assignment { target, value, .. } = expr {
                if let (Expression::Identifier(id), Expression::Block(block)) =
                    (target.as_ref(), value.as_ref())
                {
                    // Reset state version at the start of each method
                    self.reset_state_version();

                    // Push a new scope for this method's parameter bindings
                    self.push_scope();

                    // BT-470: Register parameters BEFORE generating body so field
                    // references resolve to parameter variables (not maps:get)
                    let param_vars: Vec<String> = block
                        .parameters
                        .iter()
                        .map(|p| self.fresh_var(&p.name))
                        .collect();

                    // BT-761: Detect whether any block in this method body contains ^.
                    let needs_nlr = block
                        .body
                        .iter()
                        .any(|expr| Self::expr_has_block_nlr(expr, false));

                    let nlr_token_var = if needs_nlr {
                        let token_var = self.fresh_temp_var("NlrToken");
                        self.current_nlr_token = Some(token_var.clone());
                        Some(token_var)
                    } else {
                        None
                    };

                    // Generate body as Document
                    let body_doc = self.generate_method_body_with_reply(block)?;

                    self.current_nlr_token = None;

                    // BT-761/BT-764: Wrap body in letrec function with try/catch
                    // via the shared helper.
                    // BT-774: Compose at Document level without intermediate string rendering.
                    let body_doc = if let Some(ref token_var) = nlr_token_var {
                        self.wrap_actor_body_with_nlr_catch(body_doc, token_var, true)
                    } else {
                        body_doc
                    };

                    // Build method clause as Document tree
                    let clause_doc = if param_vars.is_empty() {
                        docvec![
                            format!("<'{}'> when 'true' ->", id.name),
                            nest(INDENT, docvec![line(), body_doc,]),
                            "\n",
                        ]
                    } else {
                        let params_str = param_vars.join(", ");

                        docvec![
                            format!("<'{}'> when 'true' ->", id.name),
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    "case Args of",
                                    nest(
                                        INDENT,
                                        docvec![
                                            line(),
                                            format!("<[{params_str}]> when 'true' ->"),
                                            nest(INDENT, docvec![line(), body_doc,]),
                                            line(),
                                            "<_> when 'true' -> {'reply', {'error', 'bad_arity'}, State}",
                                        ]
                                    ),
                                    line(),
                                    "end",
                                ]
                            ),
                            "\n",
                        ]
                    };

                    let indent_spaces = case_clause_indent * INDENT;
                    #[allow(clippy::cast_sign_loss)] // indent_spaces is always non-negative
                    let indent_str = " ".repeat(indent_spaces as usize);
                    docs.push(docvec![indent_str, nest(indent_spaces, clause_doc)]);

                    // Pop the scope when done with this method
                    self.pop_scope();
                }
            }
        }

        // Generate case clauses for methods in class definitions
        for class in &module.classes {
            let doc = self.generate_class_method_dispatches(class, case_clause_indent)?;
            docs.push(doc);
        }

        // ADR 0006 Phase 1b: Reflection methods (class, respondsTo:, instVarNames,
        // instVarAt:, instVarAt:put:) are now inherited from Object via hierarchy
        // walking, no longer generated per-class.

        // Default case: extension check, hierarchy walk, then DNU fallback
        let class_name = self.class_name();
        let module_name = self.module_name.clone();
        let hint = "Check spelling or use 'respondsTo:' to verify method exists";
        let hint_binary = Self::binary_string_literal(hint);

        let default_body = docvec![
            "<OtherSelector> when 'true' ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "%% BT-229/ADR 0005: Check extension registry before hierarchy walk",
                    line(),
                    format!(
                        "let ExtLookup = try call 'beamtalk_extensions':'lookup'('{class_name}', OtherSelector)"
                    ),
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "of ExtLookupResult -> ExtLookupResult",
                            line(),
                            "catch <_EType, _EReason, _EStack> -> 'not_found'",
                        ]
                    ),
                    line(),
                    "in",
                    line(),
                    "case ExtLookup of",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<{'ok', ExtFun, _ExtOwner}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    "let ExtResult = apply ExtFun(Args, Self) in",
                                    line(),
                                    "{'reply', ExtResult, State}",
                                ]
                            ),
                            line(),
                            "<'not_found'> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    "%% ADR 0006: Try hierarchy walk before DNU",
                                    line(),
                                    format!(
                                        "case call 'beamtalk_dispatch':'super'(OtherSelector, Args, Self, State, '{class_name}') of"
                                    ),
                                    nest(
                                        INDENT,
                                        docvec![
                                            line(),
                                            "<{'reply', InheritedResult, InheritedState}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    "{'reply', InheritedResult, InheritedState}",
                                                ]
                                            ),
                                            line(),
                                            "<{'error', _DispatchError}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    "%% Not in hierarchy - try doesNotUnderstand:args: (BT-29)",
                                                    line(),
                                                    "let DnuSelector = 'doesNotUnderstand:args:' in",
                                                    line(),
                                                    "let Methods = call 'maps':'get'('__methods__', State) in",
                                                    line(),
                                                    "case call 'maps':'is_key'(DnuSelector, Methods) of",
                                                    nest(
                                                        INDENT,
                                                        docvec![
                                                            line(),
                                                            "<'true'> when 'true' ->",
                                                            nest(
                                                                INDENT,
                                                                docvec![
                                                                    line(),
                                                                    format!(
                                                                        "call '{module_name}':'dispatch'(DnuSelector, [OtherSelector, Args], Self, State)"
                                                                    ),
                                                                ]
                                                            ),
                                                            line(),
                                                            "<'false'> when 'true' ->",
                                                            nest(
                                                                INDENT,
                                                                docvec![
                                                                    line(),
                                                                    "%% No DNU handler - return #beamtalk_error{} record",
                                                                    line(),
                                                                    "let ClassName = call 'maps':'get'('$beamtalk_class', State) in",
                                                                    line(),
                                                                    "let Error0 = call 'beamtalk_error':'new'('does_not_understand', ClassName) in",
                                                                    line(),
                                                                    "let Error1 = call 'beamtalk_error':'with_selector'(Error0, OtherSelector) in",
                                                                    line(),
                                                                    format!(
                                                                        "let HintMsg = {hint_binary} in"
                                                                    ),
                                                                    line(),
                                                                    "let Error = call 'beamtalk_error':'with_hint'(Error1, HintMsg) in",
                                                                    line(),
                                                                    "{'error', Error, State}",
                                                                ]
                                                            ),
                                                        ]
                                                    ),
                                                    line(),
                                                    "end",
                                                ]
                                            ),
                                        ]
                                    ),
                                    line(),
                                    "end",
                                ]
                            ),
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
        ];
        let indent_spaces = case_clause_indent * INDENT;
        #[allow(clippy::cast_sign_loss)] // indent_spaces is always non-negative
        let indent_str = " ".repeat(indent_spaces as usize);
        docs.push(docvec![indent_str, nest(indent_spaces, default_body),]);

        // Close case and function
        let footer_doc = docvec![nest(INDENT, docvec![line(), "end",]), "\n\n",];
        docs.push(footer_doc);

        Ok(Document::Vec(docs))
    }
}
