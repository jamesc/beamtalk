// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method dispatch and routing code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the method table, `has_method/1`, `safe_dispatch/3`, and
//! `dispatch/4` functions for runtime message routing.

use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, MethodKind, Module};
use std::fmt::Write;

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
    pub(in crate::codegen::core_erlang) fn generate_method_table(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        writeln!(self.output, "'method_table'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        write!(self.output, "~{{")?;

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

        for (i, (name, arity)) in methods.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "'{name}' => {arity}")?;
        }

        writeln!(self.output, "}}~")?;
        self.indent -= 1;
        writeln!(self.output)?;

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
    pub(in crate::codegen::core_erlang) fn generate_has_method(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        writeln!(self.output, "'has_method'/1 = fun (Selector) ->")?;
        self.indent += 1;
        self.write_indent()?;

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

        // Generate lists:member call with method list
        write!(self.output, "call 'lists':'member'(Selector, [")?;
        for (i, name) in methods.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "'{name}'")?;
        }
        writeln!(self.output, "])")?;

        self.indent -= 1;
        writeln!(self.output)?;

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
    pub(in crate::codegen::core_erlang) fn generate_safe_dispatch(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "'safe_dispatch'/3 = fun (Selector, Args, State) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        // Construct Self object reference using beamtalk_actor:make_self/1 (BT-161)
        writeln!(
            self.output,
            "let Self = call 'beamtalk_actor':'make_self'(State) in"
        )?;
        self.write_indent()?;
        // Core Erlang try uses simple variable patterns in of/catch, not case-style <Pattern> when Guard
        writeln!(
            self.output,
            "try call '{}':'dispatch'(Selector, Args, Self, State)",
            self.module_name
        )?;
        self.write_indent()?;
        writeln!(self.output, "of Result -> Result")?;
        self.write_indent()?;
        // Capture but don't return stacktrace to prevent information leakage to callers
        writeln!(
            self.output,
            "catch <Type, Error, _Stacktrace> -> {{'error', {{Type, Error}}, State}}"
        )?;
        self.indent -= 1;
        writeln!(self.output)?;

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
                    // For state threading to work, we can't wrap in "let Result = ... in"
                    // because that would put State{n} bindings out of scope for the reply.
                    // Instead, we generate the state threading let bindings directly,
                    // and then generate the reply tuple inline.
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
        self.write_indent()?;
        writeln!(self.output, "<OtherSelector> when 'true' ->")?;
        self.indent += 1;

        // BT-229: Check extension registry for this class first (ADR 0005)
        // Use try/catch to handle missing ETS table during early bootstrap
        self.write_indent()?;
        writeln!(
            self.output,
            "%% BT-229/ADR 0005: Check extension registry before hierarchy walk"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let ExtLookup = try call 'beamtalk_extensions':'lookup'('{class_name}', OtherSelector)"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "of ExtLookupResult -> ExtLookupResult")?;
        self.write_indent()?;
        writeln!(
            self.output,
            "catch <_EType, _EReason, _EStack> -> 'not_found'"
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "in")?;
        self.write_indent()?;
        writeln!(self.output, "case ExtLookup of")?;
        self.indent += 1;

        // Extension found - invoke it and wrap as gen_server reply
        self.write_indent()?;
        writeln!(self.output, "<{{'ok', ExtFun, _ExtOwner}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "let ExtResult = apply ExtFun(Args, Self) in")?;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', ExtResult, State}}")?;
        self.indent -= 1;

        // Extension not found - try hierarchy walk (ADR 0006)
        self.write_indent()?;
        writeln!(self.output, "<'not_found'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "%% ADR 0006: Try hierarchy walk before DNU")?;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'beamtalk_dispatch':'super'(OtherSelector, Args, Self, State, '{class_name}') of"
        )?;
        self.indent += 1;

        // Success case - inherited method found
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'reply', InheritedResult, InheritedState}}> when 'true' ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', InheritedResult, InheritedState}}")?;
        self.indent -= 1;

        // Error case - method not found in hierarchy, try DNU
        self.write_indent()?;
        writeln!(self.output, "<{{'error', _DispatchError}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "%% Not in hierarchy - try doesNotUnderstand:args: (BT-29)"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let DnuSelector = 'doesNotUnderstand:args:' in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Methods = call 'maps':'get'('__methods__', State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'maps':'is_key'(DnuSelector, Methods) of"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<'true'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "call '{}':'dispatch'(DnuSelector, [OtherSelector, Args], Self, State)",
            self.module_name
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<'false'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "%% No DNU handler - return #beamtalk_error{{}} record"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let ClassName = call 'maps':'get'('__class__', State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error0 = call 'beamtalk_error':'new'('does_not_understand', ClassName) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error1 = call 'beamtalk_error':'with_selector'(Error0, OtherSelector) in"
        )?;
        self.write_indent()?;
        // Generate hint message as Core Erlang binary
        let hint = "Check spelling or use 'respondsTo:' to verify method exists";
        let mut hint_binary = String::new();
        for (i, ch) in hint.chars().enumerate() {
            if i > 0 {
                hint_binary.push(',');
            }
            write!(
                &mut hint_binary,
                "#<{}>(8,1,'integer',['unsigned'|['big']])",
                ch as u32
            )?;
        }
        writeln!(self.output, "let HintMsg = #{{{hint_binary}}}# in",)?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error = call 'beamtalk_error':'with_hint'(Error1, HintMsg) in"
        )?;
        self.write_indent()?;
        writeln!(self.output, "{{'error', Error, State}}")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        // Close extension not_found branch
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        // Close outer case Selector of
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }
}
