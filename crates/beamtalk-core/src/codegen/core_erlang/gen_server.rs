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

    /// Generates the `new/0` error method for actors (BT-217).
    ///
    /// Actors cannot be instantiated with `new` - they must use `spawn`.
    /// This function generates a method that throws a structured `#beamtalk_error{}`
    /// record with `kind=instantiation_error`.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'new'/0 = fun () ->
    ///     let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in
    ///     let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new') in
    ///     let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"Use spawn instead">>) in
    ///     call 'erlang':'error'(Error2)
    /// ```
    pub(super) fn generate_actor_new_error_method(&mut self) -> Result<()> {
        writeln!(self.output, "'new'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new') in"
        )?;
        self.write_indent()?;
        write!(self.output, "let Error2 = call 'beamtalk_error':'with_hint'(Error1, ")?;
        self.generate_binary_string("Use spawn instead")?;
        writeln!(self.output, ") in")?;
        self.write_indent()?;
        writeln!(self.output, "call 'erlang':'error'(Error2)")?;
        self.indent -= 1;

        Ok(())
    }

    /// Generates the `new/1` error method for actors (BT-217).
    ///
    /// Actors cannot be instantiated with `new:` - they must use `spawnWith:`.
    /// This function generates a method that throws a structured `#beamtalk_error{}`
    /// record with `kind=instantiation_error`.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'new'/1 = fun (_InitArgs) ->
    ///     let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in
    ///     let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new:') in
    ///     let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"Use spawnWith: instead">>) in
    ///     call 'erlang':'error'(Error2)
    /// ```
    pub(super) fn generate_actor_new_with_args_error_method(&mut self) -> Result<()> {
        writeln!(self.output, "'new'/1 = fun (_InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new:') in"
        )?;
        self.write_indent()?;
        write!(self.output, "let Error2 = call 'beamtalk_error':'with_hint'(Error1, ")?;
        self.generate_binary_string("Use spawnWith: instead")?;
        writeln!(self.output, ") in")?;
        self.write_indent()?;
        writeln!(self.output, "call 'erlang':'error'(Error2)")?;
        self.indent -= 1;

        Ok(())
    }

    /// Helper to generate a binary string literal in Core Erlang format.
    ///
    /// Generates: #{#<char1>(...), #<char2>(...), ...}#
    fn generate_binary_string(&mut self, s: &str) -> Result<()> {
        write!(self.output, "#{{")?;
        for (i, ch) in s.chars().enumerate() {
            if i > 0 {
                write!(self.output, ",")?;
            }
            write!(
                self.output,
                "#<{}>(8,1,'integer',['unsigned'|['big']])",
                ch as u32
            )?;
        }
        write!(self.output, "}}#")?;
        Ok(())
    }

    /// Generates the `init/1` callback for `gen_server`.
    ///
    /// For classes with non-Actor superclasses, the init function:
    /// 1. Calls parent's `init(InitArgs)` to get inherited state
    /// 2. Creates a map with this class's metadata and fields
    /// 3. Merges parent state with child fields (`ChildFields` override parent defaults)
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
    ///             in let FinalState = call 'maps':'merge'(ParentState, ChildFields)
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
    pub(super) fn generate_init_function(&mut self, module: &Module) -> Result<()> {
        writeln!(self.output, "'init'/1 = fun (InitArgs) ->")?;
        self.indent += 1;

        // Find the current class to check for superclass
        // NOTE: This requires the .bt file to have an explicit class definition
        // like "Counter subclass: LoggingCounter" (see tests/fixtures/logging_counter.bt).
        // Module-level expressions without a class definition take the base class path below.
        let current_class = module.classes.iter().find(|c| {
            // Compare module names using the same conversion (PascalCase -> snake_case)
            use super::util::to_module_name;
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
                use super::util::to_module_name;
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
                "in let FinalState = call 'maps':'merge'(ParentState, ChildFields)"
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
    pub(super) fn generate_has_method(&mut self, module: &Module) -> Result<()> {
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
    ///
    /// Generates only the current class's own state fields (not inherited).
    ///
    /// This is used when calling parent init - we only add fields defined in this class,
    /// not fields from parent classes (those come from parent's init).
    fn generate_own_state_fields(&mut self, module: &Module) -> Result<()> {
        // Find the current class being compiled
        let current_class = module.classes.iter().find(|c| {
            use super::util::to_module_name;
            to_module_name(&c.name.name) == self.module_name
        });

        if let Some(class) = current_class {
            // Only emit this class's own fields
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

    /// Generates all state fields including inherited ones (for base classes).
    ///
    /// This version includes fields from module-level assignments and recursively
    /// collects inherited fields from parent classes when they're in the same module.
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

        // Find the current class being compiled (matches module name)
        let current_class = module.classes.iter().find(|c| {
            use super::util::to_module_name;
            to_module_name(&c.name.name) == self.module_name
        });

        if let Some(class) = current_class {
            // Collect inherited fields from parent classes (recursively)
            let inherited_fields = Self::collect_inherited_fields(&class.superclass.name, module)?;

            // Emit inherited fields first
            for (field_name, default_value) in inherited_fields {
                self.write_indent()?;
                write!(self.output, ", '{field_name}' => ")?;
                self.generate_expression(&default_value)?;
                writeln!(self.output)?;
            }

            // Then emit this class's own fields (can override parent defaults)
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
        } else {
            // Fallback: if no matching class found (legacy modules), emit all class fields
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
        }

        Ok(())
    }

    /// Recursively collects all inherited state fields from parent classes.
    ///
    /// Returns a vector of `(field_name, default_value)` pairs in inheritance order
    /// (most distant ancestor first). This ensures parent fields are initialized
    /// before child fields, allowing children to override parent defaults.
    ///
    /// Only works when parent classes are defined in the same Module AST.
    /// For cross-file inheritance (e.g., from standard library classes), the
    /// parent's fields are not included - they must be provided via `InitArgs` or
    /// handled by a future import mechanism.
    fn collect_inherited_fields(
        parent_name: &str,
        module: &Module,
    ) -> Result<Vec<(String, Expression)>> {
        let mut fields = Vec::new();

        // Base case: Actor and other built-in types have no state fields
        if parent_name == "Actor" || parent_name == "Object" {
            return Ok(fields);
        }

        // Find parent class in the same module
        let parent_class = module
            .classes
            .iter()
            .find(|c| c.name.name.eq_ignore_ascii_case(parent_name));

        if let Some(parent) = parent_class {
            // Recursively collect grandparent fields first
            let grandparent_fields =
                Self::collect_inherited_fields(&parent.superclass.name, module)?;
            fields.extend(grandparent_fields);

            // Add this parent's fields
            for state in &parent.state {
                let default_value = if let Some(ref val) = state.default_value {
                    val.clone()
                } else {
                    // No default - use nil
                    Expression::Identifier(crate::ast::Identifier {
                        name: "nil".into(),
                        span: state.span,
                    })
                };
                fields.push((state.name.name.to_string(), default_value));
            }
        }
        // If parent not found in module, it's a cross-file reference - skip for now

        Ok(fields)
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
                } else if Self::is_error_message_send(expr) {
                    // Error message send: never returns, so just emit the call directly
                    // without wrapping in a reply tuple (would be unreachable code)
                    self.generate_expression(expr)?;
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
                } else if Self::is_error_message_send(expr) {
                    // Error message send: never returns, so just emit the call directly
                    // without wrapping in a reply tuple (would be unreachable code)
                    self.generate_expression(expr)?;
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
    /// The function is defensive - if `beamtalk_class` is not available (e.g., during
    /// early module loading), it returns `ok` to allow the module to load. Classes can
    /// be registered later via explicit calls if needed.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'register_class'/0 = fun () ->
    ///     try
    ///         case call 'beamtalk_class':'start_link'('Counter', ~{...}~) of
    ///             <{'ok', _Pid}> when 'true' -> 'ok'
    ///             <{'error', {'already_started', _}}> when 'true' -> 'ok'
    ///             <{'error', _Reason}> when 'true' -> 'ok'
    ///         end
    ///     catch <_,_,_> -> 'ok'
    /// ```
    pub(super) fn generate_register_class(&mut self, module: &Module) -> Result<()> {
        // Skip if no class definitions
        if module.classes.is_empty() {
            return Ok(());
        }

        writeln!(self.output, "'register_class'/0 = fun () ->")?;
        self.indent += 1;

        // Wrap in try-catch for defensive loading
        self.write_indent()?;
        writeln!(self.output, "try")?;
        self.indent += 1;

        // Generate registration for each class in the module
        for (i, class) in module.classes.iter().enumerate() {
            self.write_indent()?;
            // Use case expressions for error handling
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

            for (method_idx, method) in instance_methods.iter().enumerate() {
                if method_idx > 0 {
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
            for (state_idx, state_decl) in class.state.iter().enumerate() {
                if state_idx > 0 {
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

            // Handle error cases - allow module to load anyway
            self.write_indent()?;
            writeln!(
                self.output,
                "<{{'error', {{'already_started', _Existing{i}}}}}> when 'true' -> 'ok'"
            )?;

            self.write_indent()?;
            writeln!(self.output, "<{{'error', _Reason{i}}}> when 'true' -> 'ok'")?;
            self.indent -= 1;

            self.write_indent()?;
            writeln!(self.output, "end")?;
        }

        writeln!(self.output)?;
        self.write_indent()?;
        writeln!(self.output, "in 'ok'")?;

        // Catch any exceptions during registration
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "of RegResult -> RegResult")?;
        self.write_indent()?;
        writeln!(
            self.output,
            "catch <CatchType, CatchError, CatchStack> -> 'ok'"
        )?;

        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }
}
