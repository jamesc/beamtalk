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
use crate::ast::{Expression, MethodKind, Module};
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
        let class_name = self.module_name_to_class_name();
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
        let class_name = self.module_name_to_class_name();
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
    /// sends `{resolved, Result}` or `{rejected, Error}` to the `FuturePid`.
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
        writeln!(
            self.output,
            "let _Ignored = call 'erlang':'!'(FuturePid, {{'resolved', Result}})"
        )?;
        self.write_indent()?;
        writeln!(self.output, "in {{'noreply', NewState}}")?;
        self.indent -= 1;
        // Error case: reject the future (error isolation - don't crash the instance)
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Error, NewState}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let _Ignored = call 'erlang':'!'(FuturePid, {{'rejected', Error}})"
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
    ///     case call 'module':'dispatch'('terminate', [Reason], State) of
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
            "case call '{}':'dispatch'('terminate', [Reason], State) of",
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
}
