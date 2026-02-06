// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP `gen_server` callback code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the standard OTP callbacks: `init/1`, `handle_cast/2`,
//! `handle_call/3`, `code_change/3`, and `terminate/2`.

use super::super::{CoreErlangGenerator, Result};
use crate::ast::Module;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates the `init/1` callback for `gen_server`.
    ///
    /// For classes with non-Actor superclasses, the init function:
    /// 1. Calls parent's `init(InitArgs)` to get inherited state
    /// 2. Creates a map with this class's metadata and fields
    /// 3. Merges: parent defaults → child defaults → user `InitArgs` (user wins)
    /// 4. Returns `{ok, FinalState}` or propagates parent init errors
    ///
    /// For base classes (extending Actor), it generates a simple init:
    /// 1. Creates a default state map with `__class__`, `__methods__`, and field values
    /// 2. Merges `InitArgs` into the default state (`InitArgs` values override defaults)
    /// 3. Returns `{ok, FinalState}`
    ///
    /// # Generated Code (with inheritance)
    ///
    /// ```erlang
    /// 'init'/1 = fun (InitArgs) ->
    ///     case call 'counter':'init'(InitArgs) of
    ///         <{'ok', ParentState}> when 'true' ->
    ///             let ChildFields = ~{
    ///                 '__class__' => 'LoggingCounter',
    ///                 '__methods__' => call 'logging_counter':'method_table'(),
    ///                 'logCount' => 0
    ///             }~
    ///             in let MergedState = call 'maps':'merge'(ParentState, ChildFields)
    ///             in let FinalState = call 'maps':'merge'(MergedState, InitArgs)
    ///             in {'ok', FinalState}
    ///         <{'error', Reason}> when 'true' ->
    ///             {'error', Reason}
    ///     end
    /// ```
    ///
    /// # Generated Code (base class)
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
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_init_function(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        writeln!(self.output, "'init'/1 = fun (InitArgs) ->")?;
        self.indent += 1;

        // Find the current class to check for superclass
        // NOTE: This requires the .bt file to have an explicit class definition
        // like "Counter subclass: LoggingCounter" (see tests/fixtures/logging_counter.bt).
        // Module-level expressions without a class definition take the base class path below.
        let current_class = module.classes.iter().find(|c| {
            // Compare module names using the same conversion (PascalCase -> snake_case)
            use super::super::util::to_module_name;
            to_module_name(&c.name.name) == self.module_name
        });

        // Check if we have a superclass that's not Actor (base class)
        // When true, we'll call the parent's init to inherit state fields
        let has_parent_init = if let Some(class) = current_class {
            !class.superclass.name.eq_ignore_ascii_case("Actor")
                && !class.superclass.name.eq_ignore_ascii_case("Object")
        } else {
            false
        };

        if has_parent_init {
            // Call parent's init to get inherited state, then merge with our state
            // SAFETY: has_parent_init is true only when current_class.is_some(),
            // so this expect cannot fail unless there's a logic error
            let class = current_class.expect("has_parent_init implies current_class is Some");
            let parent_module = {
                use super::super::util::to_module_name;
                to_module_name(&class.superclass.name)
            };

            self.write_indent()?;
            writeln!(
                self.output,
                "%% Call parent init to get inherited state fields"
            )?;
            self.write_indent()?;
            writeln!(
                self.output,
                "case call '{parent_module}':'init'(InitArgs) of"
            )?;
            self.indent += 1;
            self.write_indent()?;
            writeln!(self.output, "<{{'ok', ParentState}}> when 'true' ->")?;
            self.indent += 1;
            self.write_indent()?;
            writeln!(
                self.output,
                "%% Merge parent state with this class's fields"
            )?;
            self.write_indent()?;
            writeln!(self.output, "let ChildFields = ~{{")?;
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

            // Add this class's own fields
            self.generate_own_state_fields(module)?;

            self.indent -= 1;
            self.write_indent()?;
            writeln!(self.output, "}}~")?;
            self.write_indent()?;
            writeln!(
                self.output,
                "in let MergedState = call 'maps':'merge'(ParentState, ChildFields)"
            )?;
            self.write_indent()?;
            // Merge InitArgs last so user-provided values override defaults
            // Order: parent defaults → child defaults → user overrides
            writeln!(
                self.output,
                "in let FinalState = call 'maps':'merge'(MergedState, InitArgs)"
            )?;
            self.write_indent()?;
            writeln!(self.output, "in {{'ok', FinalState}}")?;
            self.indent -= 1;
            self.write_indent()?;
            writeln!(self.output, "<{{'error', Reason}}> when 'true' ->")?;
            self.indent += 1;
            self.write_indent()?;
            writeln!(self.output, "%% Propagate parent init error")?;
            self.write_indent()?;
            writeln!(self.output, "{{'error', Reason}}")?;
            self.indent -= 2;
            self.write_indent()?;
            writeln!(self.output, "end")?;
        } else {
            // No parent, or parent is Actor base class - generate normal init
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
        }

        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `handle_cast/2` callback for async message sends.
    ///
    /// Per BT-29 design doc, uses `safe_dispatch/3` for error isolation and
    /// sends `{resolve, Result}` or `{reject, Error}` to the `FuturePid`.
    pub(in crate::codegen::core_erlang) fn generate_handle_cast(&mut self) -> Result<()> {
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
    pub(in crate::codegen::core_erlang) fn generate_handle_call(&mut self) -> Result<()> {
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
    pub(in crate::codegen::core_erlang) fn generate_code_change(&mut self) -> Result<()> {
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
    pub(in crate::codegen::core_erlang) fn generate_terminate(
        &mut self,
        _module: &Module,
    ) -> Result<()> {
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
}
