// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor spawn and instantiation error code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates `spawn/0`, `spawn/1` class methods and `new/0`, `new/1` error
//! methods that prevent incorrect actor instantiation.

use super::super::{CoreErlangGenerator, Result};
use crate::ast::Module;
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
    pub(in crate::codegen::core_erlang) fn generate_spawn_function(
        &mut self,
        _module: &Module,
    ) -> Result<()> {
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
        let class_name = self.class_name();
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
    pub(in crate::codegen::core_erlang) fn generate_spawn_with_args_function(
        &mut self,
        _module: &Module,
    ) -> Result<()> {
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
        let class_name = self.class_name();
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
    pub(in crate::codegen::core_erlang) fn generate_actor_new_error_method(
        &mut self,
    ) -> Result<()> {
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
        write!(
            self.output,
            "let Error2 = call 'beamtalk_error':'with_hint'(Error1, "
        )?;
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
    pub(in crate::codegen::core_erlang) fn generate_actor_new_with_args_error_method(
        &mut self,
    ) -> Result<()> {
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
        write!(
            self.output,
            "let Error2 = call 'beamtalk_error':'with_hint'(Error1, "
        )?;
        self.generate_binary_string("Use spawnWith: instead")?;
        writeln!(self.output, ") in")?;
        self.write_indent()?;
        writeln!(self.output, "call 'erlang':'error'(Error2)")?;
        self.indent -= 1;

        Ok(())
    }

    /// Generates the `spawn/0` error method for abstract classes (BT-105).
    ///
    /// Abstract classes cannot be instantiated — they must be subclassed first.
    /// This function generates a method that throws a structured `#beamtalk_error{}`
    /// record with `kind=instantiation_error`.
    pub(in crate::codegen::core_erlang) fn generate_abstract_spawn_error_method(
        &mut self,
    ) -> Result<()> {
        let class_name = self.class_name();
        writeln!(self.output, "'spawn'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error0 = call 'beamtalk_error':'new'('instantiation_error', '{class_name}') in",
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'spawn') in"
        )?;
        self.write_indent()?;
        write!(
            self.output,
            "let Error2 = call 'beamtalk_error':'with_hint'(Error1, "
        )?;
        self.generate_binary_string("Abstract classes cannot be instantiated. Subclass it first.")?;
        writeln!(self.output, ") in")?;
        self.write_indent()?;
        writeln!(self.output, "call 'erlang':'error'(Error2)")?;
        self.indent -= 1;

        Ok(())
    }

    /// Generates the `spawn/1` error method for abstract classes (BT-105).
    pub(in crate::codegen::core_erlang) fn generate_abstract_spawn_with_args_error_method(
        &mut self,
    ) -> Result<()> {
        let class_name = self.class_name();
        writeln!(self.output, "'spawn'/1 = fun (_InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error0 = call 'beamtalk_error':'new'('instantiation_error', '{class_name}') in",
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'spawnWith:') in"
        )?;
        self.write_indent()?;
        write!(
            self.output,
            "let Error2 = call 'beamtalk_error':'with_hint'(Error1, "
        )?;
        self.generate_binary_string("Abstract classes cannot be instantiated. Subclass it first.")?;
        writeln!(self.output, ") in")?;
        self.write_indent()?;
        writeln!(self.output, "call 'erlang':'error'(Error2)")?;
        self.indent -= 1;

        Ok(())
    }

    /// Helper to generate a binary string literal in Core Erlang format.
    ///
    /// Generates: #{#<char1>(...), #<char2>(...), ...}#
    pub(in crate::codegen::core_erlang) fn generate_binary_string(
        &mut self,
        s: &str,
    ) -> Result<()> {
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

    /// Generates the `superclass/0` class method for reflection.
    ///
    /// Returns the superclass name as an atom, or `'nil'` for root classes.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'superclass'/0 = fun () -> 'Actor'
    /// ```
    pub(in crate::codegen::core_erlang) fn generate_superclass_function(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        let superclass_atom = module
            .classes
            .first()
            .and_then(|c| c.superclass.as_ref())
            .map_or("nil", |s| s.name.as_str());

        writeln!(
            self.output,
            "'superclass'/0 = fun () -> '{superclass_atom}'"
        )?;
        writeln!(self.output)?;

        Ok(())
    }
}
