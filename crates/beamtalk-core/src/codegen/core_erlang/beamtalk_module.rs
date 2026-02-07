// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk global class module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for the special `Beamtalk` global class,
//! which provides system reflection via class methods (not instances):
//! - `allClasses/0` - Returns list of all registered classes
//! - `classNamed:/1` - Looks up a class by name (symbol)
//! - `globals/0` - Returns global namespace dictionary

use super::{CoreErlangGenerator, Result};
use crate::ast::ClassDefinition;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates the Beamtalk global class module.
    ///
    /// BT-220: The Beamtalk class is a special value type that provides system
    /// reflection via class methods (not instances). It exports:
    /// - `allClasses/0` - Returns list of all registered classes
    /// - `classNamed:/1` - Looks up a class by name (symbol)
    /// - `globals/0` - Returns global namespace dictionary
    ///
    /// These are compiler primitives that call into the runtime class registry.
    pub(super) fn generate_beamtalk_module(&mut self, class: &ClassDefinition) -> Result<()> {
        // Module header with class method exports + registration
        writeln!(
            self.output,
            "module '{}' ['allClasses'/0, 'classNamed:'/1, 'globals'/0, 'register_class'/0]",
            self.module_name
        )?;
        writeln!(
            self.output,
            "  attributes ['on_load' = [{{'register_class', 0}}]]"
        )?;
        writeln!(self.output)?;

        // Generate registration function (BT-218 + BT-215)
        self.generate_beamtalk_registration(class)?;
        writeln!(self.output)?;

        // Generate allClasses/0 - returns list of all registered classes
        self.generate_beamtalk_all_classes()?;
        writeln!(self.output)?;

        // Generate classNamed:/1 - looks up class by name
        self.generate_beamtalk_class_named()?;
        writeln!(self.output)?;

        // Generate globals/0 - returns global namespace dictionary
        self.generate_beamtalk_globals()?;

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }

    /// Generates the registration function for Beamtalk class.
    ///
    /// Beamtalk is a special class with only class methods (no instances).
    fn generate_beamtalk_registration(&mut self, class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name();

        writeln!(self.output, "'register_class'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;

        writeln!(
            self.output,
            "case call 'beamtalk_object_class':'start_link'('{class_name}', ~{{"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'name' => '{class_name}',")?;
        self.write_indent()?;
        writeln!(self.output, "'module' => '{}',", self.module_name)?;
        self.write_indent()?;
        writeln!(self.output, "'superclass' => '{}',", class.superclass.name)?;
        self.write_indent()?;
        writeln!(self.output, "'instance_methods' => ~{{}}~,")?;
        self.write_indent()?;
        writeln!(self.output, "'class_methods' => ~{{")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'allClasses' => ~{{'arity' => 0}}~,")?;
        self.write_indent()?;
        writeln!(self.output, "'classNamed:' => ~{{'arity' => 1}}~,")?;
        self.write_indent()?;
        writeln!(self.output, "'globals' => ~{{'arity' => 0}}~")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "}}~,")?;
        self.write_indent()?;
        writeln!(self.output, "'instance_variables' => []")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "}}~) of")?;

        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'ok', _Pid}}> when 'true' -> 'ok'")?;
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'error', {{'already_started', _}}}}> when 'true' -> 'ok'"
        )?;
        self.write_indent()?;
        writeln!(self.output, "<{{'error', Reason}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "call 'erlang':'error'({{'class_registration_failed', '{class_name}', Reason}})"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `allClasses/0` class method for Beamtalk.
    ///
    /// Calls `beamtalk_class:all_classes/0` and wraps each class pid in a
    /// `#beamtalk_object{}` record with class metadata.
    fn generate_beamtalk_all_classes(&mut self) -> Result<()> {
        writeln!(self.output, "'allClasses'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "let MapFun = fun (Pid) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let ClassName = call 'beamtalk_object_class':'class_name'(Pid) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let ClassModName = call 'beamtalk_object_class':'module_name'(Pid) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'beamtalk_object', ClassName, ClassModName, Pid}}"
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "in call 'lists':'map'(MapFun, call 'beamtalk_object_class':'all_classes'())"
        )?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `classNamed:/1` class method for Beamtalk.
    ///
    /// Calls `beamtalk_class:whereis_class/1` and wraps the result in a
    /// `#beamtalk_object{}` record, or returns nil if class not found.
    fn generate_beamtalk_class_named(&mut self) -> Result<()> {
        writeln!(self.output, "'classNamed:'/1 = fun (ClassName) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'beamtalk_object_class':'whereis_class'(ClassName) of"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<'undefined'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'nil'")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<Pid> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let ClassModName = call 'beamtalk_object_class':'module_name'(Pid) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'beamtalk_object', ClassName, ClassModName, Pid}}"
        )?;
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `globals/0` class method for Beamtalk.
    ///
    /// Currently returns an empty map as a placeholder for future global namespace support.
    fn generate_beamtalk_globals(&mut self) -> Result<()> {
        writeln!(self.output, "'globals'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "~{{}}~  %% TODO: Implement global namespace")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }
}
