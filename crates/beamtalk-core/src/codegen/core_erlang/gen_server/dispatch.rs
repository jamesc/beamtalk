// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method dispatch and routing code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the method table, `has_method/1`, `safe_dispatch/3`, and
//! `dispatch/4` functions for runtime message routing.

use super::super::document::{INDENT, line, nest};
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
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_method_table(
        &mut self,
        module: &Module,
    ) -> Result<()> {
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
        self.write_document(&doc);

        Ok(())
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
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_has_method(
        &mut self,
        module: &Module,
    ) -> Result<()> {
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
        self.write_document(&doc);

        Ok(())
    }

    /// Generates the `safe_dispatch/3` function with error isolation.
    ///
    /// Per BT-29 design doc (following LFE Flavors pattern), errors in method
    /// dispatch are caught and returned to the caller rather than crashing
    /// the actor instance.
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
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_safe_dispatch(&mut self) -> Result<()> {
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
        self.write_document(&doc);

        Ok(())
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
    ) -> Result<()> {
        use std::fmt::Write;

        writeln!(
            self.output,
            "'dispatch'/4 = fun (Selector, Args, Self, State) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Selector of")?;
        self.indent += 1;

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

                    self.write_indent()?;
                    write!(self.output, "<'{}'> when 'true' ->", id.name)?;
                    writeln!(self.output)?;
                    self.indent += 1;

                    // Bind block parameters from Args list
                    if !block.parameters.is_empty() {
                        self.write_indent()?;
                        write!(self.output, "case Args of")?;
                        writeln!(self.output)?;
                        self.indent += 1;

                        self.write_indent()?;
                        write!(self.output, "<[")?;
                        for (i, param) in block.parameters.iter().enumerate() {
                            if i > 0 {
                                write!(self.output, ", ")?;
                            }
                            let var_name = self.fresh_var(&param.name);
                            write!(self.output, "{var_name}")?;
                        }
                        write!(self.output, "]> when 'true' ->")?;
                        writeln!(self.output)?;
                        self.indent += 1;
                    }

                    // Generate the method body with state threading
                    self.write_indent()?;
                    self.generate_method_body_with_reply(block)?;
                    writeln!(self.output)?;

                    if !block.parameters.is_empty() {
                        self.indent -= 1;
                        self.write_indent()?;
                        writeln!(
                            self.output,
                            "<_> when 'true' -> {{'reply', {{'error', 'bad_arity'}}, State}}"
                        )?;
                        self.indent -= 1;
                        self.write_indent()?;
                        writeln!(self.output, "end")?;
                    }

                    self.indent -= 1;

                    // Pop the scope when done with this method
                    self.pop_scope();
                }
            }
        }

        // Generate case clauses for methods in class definitions
        for class in &module.classes {
            self.generate_class_method_dispatches(class)?;
        }

        // ADR 0006 Phase 1b: Reflection methods (class, respondsTo:, instVarNames,
        // instVarAt:, instVarAt:put:) are now inherited from Object via hierarchy
        // walking, no longer generated per-class.

        // Default case: extension check, hierarchy walk, then DNU fallback
        let class_name = self.class_name();
        let module_name = self.module_name.clone();
        let hint = "Check spelling or use 'respondsTo:' to verify method exists";
        let hint_binary = Self::binary_string_literal(hint);

        self.write_indent()?;
        write!(self.output, "<OtherSelector> when 'true' ->")?;
        self.indent += 1;

        let default_body = docvec![
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
        ];
        // Render at current indent level
        let indent_spaces = isize::try_from(self.indent).unwrap_or(0) * INDENT;
        self.write_document(&nest(indent_spaces, docvec![default_body, "\n"]));

        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }
}
