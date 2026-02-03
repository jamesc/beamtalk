// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `gen_server` scaffolding code generation.
//!
//! This module generates the OTP `gen_server` boilerplate for Beamtalk actors:
//! - Module structure with exports and attributes
//! - `start_link/0`, `spawn/0`, `spawn/1` functions
//! - `init/1` callback
//! - `handle_cast/2` and `handle_call/3` callbacks
//! - `code_change/3` callback for hot code reloading
//! - `terminate/2` callback
//! - Message dispatch logic
//! - Method table generation

use super::{CoreErlangGenerator, Result};
use crate::ast::{Block, ClassDefinition, Expression, MethodDefinition, MethodKind, Module};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates the `spawn/0` class method for creating actor instances.
    ///
    /// This is a class-level method (not an instance method) that instantiates
    /// a new actor process. The function:
    /// 1. Calls `gen_server:start_link/3` with an empty init args map
    /// 2. Wraps the pid in a `#beamtalk_object{}` record with class metadata
    /// 3. Returns the object record, or throws error on failure
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/0 = fun () ->
    ///     case call 'gen_server':'start_link'('counter', ~{}~, []) of
    ///         <{'ok', Pid}> when 'true' ->
    ///             {'beamtalk_object', 'Counter', 'counter', Pid};
    ///         <{'error', Reason}> when 'true' ->
    ///             call 'erlang':'error'({'spawn_failed', Reason})
    ///     end
    /// ```
    pub(super) fn generate_spawn_function(&mut self, _module: &Module) -> Result<()> {
        writeln!(self.output, "'spawn'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Pass empty map as InitArgs - init/1 merges this with default state
        writeln!(
            self.output,
            "case call 'gen_server':'start_link'('{}', ~{{}}~, []) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'ok', Pid}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Return #beamtalk_object{} record instead of raw pid
        // Record syntax in Core Erlang: {RecordTag, Field1, Field2, ...}
        let class_name = self.to_class_name();
        writeln!(
            self.output,
            "{{'beamtalk_object', '{}', '{}', Pid}}",
            class_name, self.module_name
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Reason}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "call 'erlang':'error'({{'spawn_failed', Reason}})"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `spawn/1` class method for creating actor instances with init args.
    ///
    /// This is a class-level method that instantiates a new actor process
    /// with initialization arguments passed to `init/1`. The function:
    /// 1. Calls `gen_server:start_link/3` with the provided args
    /// 2. Wraps the pid in a `#beamtalk_object{}` record with class metadata
    /// 3. Returns the object record, or throws error on failure
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/1 = fun (InitArgs) ->
    ///     case call 'gen_server':'start_link'('counter', InitArgs, []) of
    ///         <{'ok', Pid}> when 'true' ->
    ///             {'beamtalk_object', 'Counter', 'counter', Pid};
    ///         <{'error', Reason}> when 'true' ->
    ///             call 'erlang':'error'({'spawn_failed', Reason})
    ///     end
    /// ```
    pub(super) fn generate_spawn_with_args_function(&mut self, _module: &Module) -> Result<()> {
        writeln!(self.output, "'spawn'/1 = fun (InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'gen_server':'start_link'('{}', InitArgs, []) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'ok', Pid}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Return #beamtalk_object{} record instead of raw pid
        let class_name = self.to_class_name();
        writeln!(
            self.output,
            "{{'beamtalk_object', '{}', '{}', Pid}}",
            class_name, self.module_name
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Reason}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "call 'erlang':'error'({{'spawn_failed', Reason}})"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `init/1` callback for `gen_server`.
    ///
    /// The init function:
    /// 1. Creates a default state map with `__class__`, `__methods__`, and default field values
    /// 2. Merges the `InitArgs` map into the default state (`InitArgs` values override defaults)
    /// 3. Returns `{ok, FinalState}`
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'init'/1 = fun (InitArgs) ->
    ///     let DefaultState = ~{
    ///         '__class__' => 'Counter',
    ///         '__methods__' => call 'counter':'method_table'(),
    ///         'value' => 0
    ///     }~
    ///     in let FinalState = call 'maps':'merge'(DefaultState, InitArgs)
    ///        in {'ok', FinalState}
    /// ```
    pub(super) fn generate_init_function(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'init'/1 = fun (InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "let DefaultState = ~{{")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'__class__' => '{}',", self.to_class_name())?;
        self.write_indent()?;
        writeln!(self.output, "'__class_mod__' => '{}',", self.module_name)?;
        self.write_indent()?;
        writeln!(
            self.output,
            "'__methods__' => call '{}':'method_table'()",
            self.module_name
        )?;

        // Initialize fields from module expressions
        self.generate_initial_state_fields(module)?;

        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "}}~")?;
        self.write_indent()?;
        // Merge InitArgs into DefaultState - InitArgs values override defaults
        writeln!(
            self.output,
            "in let FinalState = call 'maps':'merge'(DefaultState, InitArgs)"
        )?;
        self.write_indent()?;
        writeln!(self.output, "in {{'ok', FinalState}}")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `handle_cast/2` callback for async message sends.
    ///
    /// Per BT-29 design doc, uses `safe_dispatch/3` for error isolation and
    /// sends `{resolve, Result}` or `{reject, Error}` to the `FuturePid`.
    pub(super) fn generate_handle_cast(&mut self) -> Result<()> {
        writeln!(self.output, "'handle_cast'/2 = fun (Msg, State) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Msg of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{Selector, Args, FuturePid}}> when 'true' ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        // Use safe_dispatch for error isolation per BT-29
        writeln!(
            self.output,
            "case call '{}':'safe_dispatch'(Selector, Args, State) of",
            self.module_name
        )?;
        self.indent += 1;
        // Success case: resolve the future
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'reply', Result, NewState}}> when 'true' ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        // Send {resolve, Result} to match beamtalk_future protocol
        writeln!(
            self.output,
            "let _Ignored = call 'erlang':'!'(FuturePid, {{'resolve', Result}})"
        )?;
        self.write_indent()?;
        writeln!(self.output, "in {{'noreply', NewState}}")?;
        self.indent -= 1;
        // Error case: reject the future (error isolation - don't crash the instance)
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Error, NewState}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Send {reject, Error} to match beamtalk_future protocol
        writeln!(
            self.output,
            "let _Ignored = call 'erlang':'!'(FuturePid, {{'reject', Error}})"
        )?;
        self.write_indent()?;
        writeln!(self.output, "in {{'noreply', NewState}}")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `handle_call/3` callback for sync message sends.
    ///
    /// Per BT-29 design doc, uses `safe_dispatch/3` for error isolation and
    /// returns `{ok, Result}` or `{error, Error}` tuples.
    pub(super) fn generate_handle_call(&mut self) -> Result<()> {
        writeln!(self.output, "'handle_call'/3 = fun (Msg, _From, State) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Msg of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{Selector, Args}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        // Use safe_dispatch for error isolation per BT-29
        writeln!(
            self.output,
            "case call '{}':'safe_dispatch'(Selector, Args, State) of",
            self.module_name
        )?;
        self.indent += 1;
        // Success case: return {ok, Result}
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'reply', Result, NewState}}> when 'true' ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', {{'ok', Result}}, NewState}}")?;
        self.indent -= 1;
        // Error case: return {error, Error}
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Error, NewState}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', {{'error', Error}}, NewState}}")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `code_change/3` callback for hot code reload.
    pub(super) fn generate_code_change(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "'code_change'/3 = fun (_OldVsn, State, _Extra) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "%% TODO: Add state migration logic")?;
        self.write_indent()?;
        writeln!(self.output, "{{'ok', State}}")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `terminate/2` callback for `gen_server` shutdown.
    ///
    /// Per BT-29 design doc, this calls the `terminate` method if defined.
    /// Instance tracking cleanup (BT-96) is automatic via process monitor.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'terminate'/2 = fun (Reason, State) ->
    ///     %% Call terminate method if defined
    ///     let Self = call 'beamtalk_actor':'make_self'(State) in
    ///     case call 'module':'dispatch'('terminate', [Reason], Self, State) of
    ///         <{'reply', _Result, _State}> when 'true' -> 'ok'
    ///         <_Other> when 'true' -> 'ok'
    ///     end
    /// ```
    pub(super) fn generate_terminate(&mut self, _module: &Module) -> Result<()> {
        writeln!(self.output, "'terminate'/2 = fun (Reason, State) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "%% Call terminate method if defined (Flavors pattern)"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Self = call 'beamtalk_actor':'make_self'(State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call '{}':'dispatch'('terminate', [Reason], Self, State) of",
            self.module_name
        )?;
        self.indent += 1;
        self.write_indent()?;
        // Core Erlang doesn't allow duplicate _ patterns, use unique ignored vars
        writeln!(
            self.output,
            "<{{'reply', _TermResult, _TermState}}> when 'true' -> 'ok'"
        )?;
        self.write_indent()?;
        // dispatch returns 3-tuple {'error', Error, State}, not 2-tuple
        writeln!(
            self.output,
            "<{{'error', _TermError, _TermState2}}> when 'true' -> 'ok'"
        )?;
        self.write_indent()?;
        writeln!(self.output, "<_TermOther> when 'true' -> 'ok'")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

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
    pub(super) fn generate_method_table(&mut self, module: &Module) -> Result<()> {
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
    pub(super) fn generate_safe_dispatch(&mut self) -> Result<()> {
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

    /// Generates the dispatch/3 function for message routing.
    ///
    /// This function is complex because it handles both legacy expression-based modules
    /// and class definitions, including the `doesNotUnderstand:args:` fallback per BT-29.
    #[expect(
        clippy::too_many_lines,
        reason = "dispatch codegen is inherently complex"
    )]
    pub(super) fn generate_dispatch(&mut self, module: &Module) -> Result<()> {
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

        // Default case for unknown messages - try doesNotUnderstand:args: first (per BT-29)
        self.write_indent()?;
        writeln!(self.output, "<OtherSelector> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "%% Try doesNotUnderstand:args: fallback (BT-29)"
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
            "%% Call doesNotUnderstand:args: with [Selector, Args]"
        )?;
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
            "%% No DNU handler - return unknown_message error"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let ClassName = call 'maps':'get'('__class__', State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'error', {{'unknown_message', OtherSelector, ClassName}}, State}}"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates dispatch case clauses for all methods in a class definition.
    pub(super) fn generate_class_method_dispatches(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<()> {
        for method in &class.methods {
            // Only generate dispatch for primary methods for now
            if method.kind == MethodKind::Primary {
                self.generate_method_dispatch(method)?;
            }
        }
        Ok(())
    }

    /// Generates a single method dispatch case clause.
    pub(super) fn generate_method_dispatch(&mut self, method: &MethodDefinition) -> Result<()> {
        // Reset state version at the start of each method
        self.reset_state_version();

        // Push a new scope for this method's parameter bindings
        self.push_scope();

        let selector_name = method.selector.name();
        self.write_indent()?;
        write!(self.output, "<'{selector_name}'> when 'true' ->")?;
        writeln!(self.output)?;
        self.indent += 1;

        // Bind method parameters from Args list
        if !method.parameters.is_empty() {
            self.write_indent()?;
            write!(self.output, "case Args of")?;
            writeln!(self.output)?;
            self.indent += 1;

            self.write_indent()?;
            write!(self.output, "<[")?;
            for (i, param) in method.parameters.iter().enumerate() {
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
        self.generate_method_definition_body_with_reply(method)?;
        writeln!(self.output)?;

        if !method.parameters.is_empty() {
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

        Ok(())
    }

    /// Generates the initial state fields for the actor's state map.
    ///
    /// Includes:
    /// - Literal field values from module-level assignments
    /// - State declarations from class definitions with their default values
    pub(super) fn generate_initial_state_fields(&mut self, module: &Module) -> Result<()> {
        // Initialize fields from module expressions (assignments at top level)
        // Only include literal values - blocks are methods handled by dispatch/3
        for expr in &module.expressions {
            if let Expression::Assignment { target, value, .. } = expr {
                if let Expression::Identifier(id) = target.as_ref() {
                    // Only generate field if it's a simple literal (not a block/method)
                    if matches!(value.as_ref(), Expression::Literal(..)) {
                        self.write_indent()?;
                        write!(self.output, ", '{}' => ", id.name)?;
                        self.generate_expression(value)?;
                        writeln!(self.output)?;
                    }
                }
            }
        }

        // Initialize fields from class state declarations
        for class in &module.classes {
            for state in &class.state {
                self.write_indent()?;
                write!(self.output, ", '{}' => ", state.name.name)?;
                if let Some(ref default_value) = state.default_value {
                    self.generate_expression(default_value)?;
                } else {
                    // No default value - initialize to nil
                    write!(self.output, "'nil'")?;
                }
                writeln!(self.output)?;
            }
        }

        Ok(())
    }

    /// Generates a method definition body wrapped in a reply tuple.
    ///
    /// For `MethodDefinition` nodes with explicit body expressions.
    pub(super) fn generate_method_definition_body_with_reply(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<()> {
        if method.body.is_empty() {
            // Empty method body returns nil
            write!(
                self.output,
                "{{'reply', 'nil', {}}}",
                self.current_state_var()
            )?;
            return Ok(());
        }

        // Generate all expressions except the last with state threading
        for (i, expr) in method.body.iter().enumerate() {
            let is_last = i == method.body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);

            // Check for early return
            if let Expression::Return { value, .. } = expr {
                let final_state = self.current_state_var();
                write!(self.output, "let _ReturnValue = ")?;
                self.generate_expression(value)?;
                write!(self.output, " in {{'reply', _ReturnValue, {final_state}}}")?;
                return Ok(());
            }

            if is_last {
                // Last expression: bind to Result and generate reply tuple
                let final_state = self.current_state_var();

                // If the last expression is a field assignment, handle specially
                if is_field_assignment {
                    // Generate the assignment (leaves state binding open)
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let val_var = self.fresh_temp_var("Val");
                            let current_state = self.current_state_var();

                            write!(self.output, "let {val_var} = ")?;
                            self.generate_expression(value)?;

                            let new_state = self.next_state_var();
                            write!(
                                self.output,
                                " in let {new_state} = call 'maps':'put'('{}', {val_var}, {current_state}) in ",
                                field.name
                            )?;

                            // Reply tuple using the NEW state (after assignment)
                            write!(self.output, "{{'reply', {val_var}, {new_state}}}")?;
                        }
                    }
                } else if Self::is_super_message_send(expr) {
                    // Super message send as last expression: unpack {reply, Result, NewState}
                    write!(self.output, "let _SuperTuple = ")?;
                    self.generate_expression(expr)?;
                    write!(
                        self.output,
                        " in let _Result = call 'erlang':'element'(2, _SuperTuple)"
                    )?;
                    write!(
                        self.output,
                        " in let _NewState = call 'erlang':'element'(3, _SuperTuple)"
                    )?;
                    write!(self.output, " in {{'reply', _Result, _NewState}}")?;
                } else {
                    // Regular last expression: bind to Result and reply
                    write!(self.output, "let _Result = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in {{'reply', _Result, {final_state}}}")?;
                }
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                self.generate_field_assignment_open(expr)?;
            } else {
                // Non-field-assignment intermediate expression: wrap in let
                let tmp_var = self.fresh_temp_var("seq");
                write!(self.output, "let {tmp_var} = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in ")?;
            }
        }

        Ok(())
    }

    /// Generates a block-based method body with state threading and reply tuple.
    ///
    /// For methods using block syntax with implicit returns.
    #[expect(
        clippy::too_many_lines,
        reason = "method body generation handles many expression types and state threading"
    )]
    pub(super) fn generate_method_body_with_reply(&mut self, block: &Block) -> Result<()> {
        if block.body.is_empty() {
            let final_state = self.current_state_var();
            write!(self.output, "{{'reply', 'nil', {final_state}}}")?;
            return Ok(());
        }

        // Generate all expressions except the last with state threading
        for (i, expr) in block.body.iter().enumerate() {
            let is_last = i == block.body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);
            let is_local_assignment = Self::is_local_var_assignment(expr);

            if is_last {
                // Last expression: bind to Result and generate reply tuple
                let final_state = self.current_state_var();

                // If the last expression is a field assignment, handle specially
                if is_field_assignment {
                    // Generate the assignment (leaves state binding open)
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let val_var = self.fresh_temp_var("Val");
                            let current_state = self.current_state_var();

                            write!(self.output, "let {val_var} = ")?;
                            self.generate_expression(value)?;

                            let new_state = self.next_state_var();
                            write!(
                                self.output,
                                " in let {new_state} = call 'maps':'put'('{}', {val_var}, {current_state}) in ",
                                field.name
                            )?;

                            // Reply tuple using the NEW state (after assignment)
                            write!(self.output, "{{'reply', {val_var}, {new_state}}}")?;
                        }
                    }
                } else if Self::is_super_message_send(expr) {
                    // Super message send as last expression: unpack {reply, Result, NewState}
                    write!(self.output, "let _SuperTuple = ")?;
                    self.generate_expression(expr)?;
                    write!(
                        self.output,
                        " in let _Result = call 'erlang':'element'(2, _SuperTuple)"
                    )?;
                    write!(
                        self.output,
                        " in let _NewState = call 'erlang':'element'(3, _SuperTuple)"
                    )?;
                    write!(self.output, " in {{'reply', _Result, _NewState}}")?;
                } else {
                    // Regular last expression: bind to Result and reply
                    write!(self.output, "let _Result = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in {{'reply', _Result, {final_state}}}")?;
                }
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                self.generate_field_assignment_open(expr)?;
            } else if is_local_assignment {
                // Local variable assignment: generate with proper binding
                if let Expression::Assignment { target, value, .. } = expr {
                    // Check if we're storing a block with mutations (ERROR)
                    if let Expression::Block(block) = value.as_ref() {
                        Self::validate_stored_closure(block, format!("{:?}", expr.span()))?;
                    }

                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = &id.name;
                        // Reuse existing Core Erlang variable if already bound (e.g. parameter),
                        // otherwise create a new one following Core Erlang conventions.
                        let core_var = self
                            .lookup_var(var_name)
                            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                        // Generate: let VarName = <value> in ...
                        // Note: we delay updating the binding until after generating the RHS
                        // so that the RHS sees the prior value (for rebinding cases).
                        write!(self.output, "let {core_var} = ")?;
                        self.generate_expression(value)?;
                        // Now update the variable binding to point at the (possibly shadowing) let-bound var.
                        self.bind_var(var_name, &core_var);
                        write!(self.output, " in ")?;
                    }
                }
            } else if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(expr) {
                // whileTrue:/whileFalse:/timesRepeat: with mutations - need to rebind threaded vars after loop
                if threaded_vars.len() == 1 {
                    let var = &threaded_vars[0];
                    let core_var = self
                        .lookup_var(var)
                        .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                    write!(self.output, "let {core_var} = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in ")?;
                } else {
                    // Multiple threaded vars - fall back for now
                    let tmp_var = self.fresh_temp_var("seq");
                    write!(self.output, "let {tmp_var} = ")?;
                    self.generate_expression(expr)?;
                    write!(self.output, " in ")?;
                }
            } else if Self::is_super_message_send(expr) {
                // Super message send: must thread state from {reply, Result, NewState} tuple
                let super_result_var = self.fresh_temp_var("SuperReply");
                let current_state = self.current_state_var();
                let new_state = self.next_state_var();

                // Manually generate super_dispatch call with current state (not via generate_expression)
                if let Expression::MessageSend {
                    selector,
                    arguments,
                    ..
                } = expr
                {
                    // Use the domain service method for selector-to-atom conversion
                    let selector_atom = selector.to_erlang_atom();
                    write!(
                        self.output,
                        "let {super_result_var} = call 'beamtalk_class':'super_dispatch'({current_state}, '{selector_atom}', ["
                    )?;
                    for (i, arg) in arguments.iter().enumerate() {
                        if i > 0 {
                            write!(self.output, ", ")?;
                        }
                        self.generate_expression(arg)?;
                    }
                    write!(self.output, "])")?;
                }

                // Extract state from the {reply, Result, NewState} tuple using element/2
                write!(
                    self.output,
                    " in let {new_state} = call 'erlang':'element'(3, {super_result_var}) in "
                )?;
            } else {
                // Non-assignment intermediate expression: wrap in let
                let tmp_var = self.fresh_temp_var("seq");
                write!(self.output, "let {tmp_var} = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in ")?;
            }
        }
        Ok(())
    }

    /// Generates the `register_class/0` function for class registration.
    ///
    /// This function is called automatically via `-on_load` when the module loads.
    /// It registers the class with `beamtalk_class:start_link/2`, making the class
    /// available as a first-class object for reflection and metaprogramming.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'register_class'/0 = fun () ->
    ///     case call 'beamtalk_class':'start_link'('Counter', ~{
    ///         'name' => 'Counter',
    ///         'module' => 'counter',
    ///         'superclass' => 'Actor',
    ///         'instance_methods' => ~{
    ///             'increment' => ~{'arity' => 0}~,
    ///             'getValue' => ~{'arity' => 0}~
    ///         }~,
    ///         'instance_variables' => ['value'],
    ///         'class_methods' => ~{
    ///             'spawn' => ~{'arity' => 0}~,
    ///             'spawnWith:' => ~{'arity' => 1}~
    ///         }~
    ///     }~) of
    ///         <{'ok', _Pid}> when 'true' -> 'ok'
    ///         <{'error', Reason}> when 'true' ->
    ///             call 'erlang':'error'({'class_registration_failed', 'Counter', Reason})
    ///     end
    /// ```
    pub(super) fn generate_register_class(&mut self, module: &Module) -> Result<()> {
        // Skip if no class definitions
        if module.classes.is_empty() {
            return Ok(());
        }

        writeln!(self.output, "'register_class'/0 = fun () ->")?;
        self.indent += 1;

        // Generate registration for each class in the module
        for (i, class) in module.classes.iter().enumerate() {
            self.write_indent()?;
            // Use case expressions for proper error handling
            if i > 0 {
                write!(self.output, "in ")?;
            }
            write!(
                self.output,
                "let _Reg{} = case call 'beamtalk_class':'start_link'('{}', ~{{",
                i, class.name.name
            )?;
            writeln!(self.output)?;
            self.indent += 1;

            // Class name
            self.write_indent()?;
            writeln!(self.output, "'name' => '{}',", class.name.name)?;

            // Module name
            self.write_indent()?;
            writeln!(self.output, "'module' => '{}',", self.module_name)?;

            // Superclass
            self.write_indent()?;
            writeln!(self.output, "'superclass' => '{}',", class.superclass.name)?;

            // Instance methods
            self.write_indent()?;
            write!(self.output, "'instance_methods' => ~{{")?;
            let instance_methods: Vec<_> = class
                .methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .collect();

            for (i, method) in instance_methods.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                write!(
                    self.output,
                    "'{}' => ~{{'arity' => {}}}~",
                    method.selector.name(),
                    method.selector.arity()
                )?;
            }
            writeln!(self.output, "}}~,")?;

            // Instance variables
            self.write_indent()?;
            write!(self.output, "'instance_variables' => [")?;
            for (i, state_decl) in class.state.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ")?;
                }
                write!(self.output, "'{}'", state_decl.name.name)?;
            }
            writeln!(self.output, "],")?;

            // Class methods
            self.write_indent()?;
            writeln!(self.output, "'class_methods' => ~{{")?;
            self.indent += 1;
            self.write_indent()?;
            writeln!(self.output, "'spawn' => ~{{'arity' => 0}}~,")?;
            self.write_indent()?;
            writeln!(self.output, "'spawnWith:' => ~{{'arity' => 1}}~")?;
            self.indent -= 1;
            self.write_indent()?;
            writeln!(self.output, "}}~")?;

            self.indent -= 1;
            self.write_indent()?;
            writeln!(self.output, "}}~) of")?;

            // Handle success case
            self.indent += 1;
            self.write_indent()?;
            writeln!(self.output, "<{{'ok', _Pid{i}}}> when 'true' -> 'ok'")?;

            // Handle error case - fail module loading with descriptive error
            self.write_indent()?;
            writeln!(
                self.output,
                "<{{'error', Reason{i}}}> when 'true' -> call 'erlang':'error'({{'class_registration_failed', '{}', Reason{i}}})",
                class.name.name
            )?;
            self.indent -= 1;

            self.write_indent()?;
            writeln!(self.output, "end")?;
        }

        writeln!(self.output)?;
        self.write_indent()?;
        writeln!(self.output, "in 'ok'")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }
}
