// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value type module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for value type classes — plain Erlang
//! terms (maps) with no process. They are created with `new` and `new:`,
//! not `spawn`, and methods are synchronous functions operating on maps.

use super::util::ClassIdentity;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{
    ClassDefinition, Expression, MessageSelector, MethodDefinition, MethodKind, Module,
};
use std::fmt::Write;

use super::variable_context;

impl CoreErlangGenerator {
    /// Generates a value type module (BT-213).
    ///
    /// Value types are plain Erlang terms (maps/records) with no process.
    /// They are created with `new` and `new:`, not `spawn`.
    ///
    /// ## Generated Structure
    ///
    /// ```erlang
    /// module 'point' ['new'/0, 'new'/1, 'plus'/2, ...]
    ///   attributes []
    ///
    /// 'new'/0 = fun () ->
    ///     ~{'$beamtalk_class' => 'Point', 'x' => 0, 'y' => 0}~
    ///
    /// 'new'/1 = fun (InitArgs) ->
    ///     DefaultState = call 'point':'new'(),
    ///     call 'maps':'merge'(DefaultState, InitArgs)
    ///
    /// 'plus'/2 = fun (Self, Other) ->
    ///     % Direct function call, no gen_server
    ///     NewX = call 'erlang':'+'(
    ///         call 'maps':'get'('x', Self),
    ///         call 'maps':'get'('x', Other)
    ///     ),
    ///     ...
    ///     ~{'$beamtalk_class' => 'Point', 'x' => NewX, 'y' => NewY}~
    /// ```
    ///
    /// ## Key Differences from Actors
    ///
    /// - No `gen_server` behavior
    /// - No `spawn/0` or `spawn/1` - use `new/0` and `new/1` instead
    /// - Methods are synchronous functions operating on maps
    /// - No state threading (value types are immutable)
    /// - Methods return new instances rather than mutating
    pub(super) fn generate_value_type_module(&mut self, module: &Module) -> Result<()> {
        // BT-213: Set context to ValueType for this module
        self.context = CodeGenContext::ValueType;

        let class = module
            .classes
            .first()
            .ok_or_else(|| CodeGenError::Internal("Value type module has no class".to_string()))?;

        // Set class identity early — needed by all code paths including the
        // Beamtalk special case below, so that class_name() returns the AST
        // class name rather than deriving from the module name.
        self.class_identity = Some(ClassIdentity::new(&class.name.name));

        // BT-220: Special case for Beamtalk global class
        // The Beamtalk class provides system reflection via class methods, not instances
        if class.name.name.as_str() == "Beamtalk" {
            return self.generate_beamtalk_module(class);
        }

        // Check if the class explicitly defines new/new: methods
        // (e.g., Object.bt defines `new => @primitive basicNew`)
        // If so, skip auto-generating constructors to avoid duplicate definitions
        // Only check Primary methods — advice (before/after/around) shouldn't suppress auto-generation
        let has_explicit_new = class.methods.iter().any(|m| {
            m.kind == MethodKind::Primary
                && matches!(&m.selector, MessageSelector::Unary(name) if name.as_str() == "new")
        });
        let has_explicit_new_with = class
            .methods
            .iter()
            .any(|m| m.kind == MethodKind::Primary && m.selector.name() == "new:");

        // Collect method exports
        let mut exports = Vec::new();
        if !has_explicit_new {
            exports.push("'new'/0".to_string());
        }
        if !has_explicit_new_with {
            exports.push("'new'/1".to_string());
        }

        // Add instance method exports (each takes Self as first parameter)
        for method in &class.methods {
            // All methods in a class are instance methods in Beamtalk
            // MethodKind is about method combination (Primary, Before, After, Around)
            let arity = method.parameters.len() + 1; // +1 for Self parameter
            let mangled = method.selector.to_erlang_atom();
            exports.push(format!("'{mangled}'/{arity}"));
        }

        // All value types export dispatch/3 and has_method/1
        // for runtime dispatch via superclass delegation chain
        exports.push("'dispatch'/3".to_string());
        exports.push("'has_method'/1".to_string());

        // All classes export superclass/0 for reflection
        exports.push("'superclass'/0".to_string());

        // BT-246: Value types register with class system for dynamic dispatch
        exports.push("'register_class'/0".to_string());

        // BT-411: Class method exports
        for method in &class.class_methods {
            if method.kind == MethodKind::Primary {
                let arity = method.parameters.len() + 1; // +1 for ClassSelf
                exports.push(format!("'class_{}'/{arity}", method.selector.name()));
            }
        }

        // Module header
        writeln!(
            self.output,
            "module '{}' [{}]",
            self.module_name,
            exports.join(", ")
        )?;
        // BT-246: on_load registers the class process
        writeln!(
            self.output,
            "  attributes ['on_load' = [{{'register_class', 0}}]]"
        )?;
        writeln!(self.output)?;

        // Generate new/0 - creates instance with default field values
        if !has_explicit_new {
            self.generate_value_type_new(class)?;
            writeln!(self.output)?;
        }

        // Generate new/1 - creates instance with initialization arguments
        if !has_explicit_new_with {
            self.generate_value_type_new_with_args()?;
            writeln!(self.output)?;
        }

        // Generate instance methods as pure functions
        for method in &class.methods {
            self.generate_value_type_method(method, class)?;
            writeln!(self.output)?;
        }

        // Generate dispatch/3 and has_method/1 for all value types
        // (superclass delegation chain — same pattern as actors)
        self.generate_primitive_dispatch(class)?;
        writeln!(self.output)?;
        self.generate_primitive_has_method(class)?;
        writeln!(self.output)?;

        // Generate superclass/0 for reflection
        let superclass_atom = class.superclass.as_ref().map_or("nil", |s| s.name.as_str());
        writeln!(
            self.output,
            "'superclass'/0 = fun () -> '{superclass_atom}'"
        )?;
        writeln!(self.output)?;

        // BT-411: Generate class-side method functions
        if !class.class_methods.is_empty() {
            self.generate_class_method_functions(class)?;
            writeln!(self.output)?;
        }

        // BT-246: Register value type class with the class system for dynamic dispatch
        self.generate_register_class(module)?;
        writeln!(self.output)?;

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }

    /// Generates the `new/0` function for a value type.
    ///
    /// Creates an instance map with `$beamtalk_class` and default field values.
    fn generate_value_type_new(&mut self, class: &ClassDefinition) -> Result<()> {
        writeln!(self.output, "'new'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;

        // Generate map literal with $beamtalk_class and fields
        write!(
            self.output,
            "~{{'$beamtalk_class' => '{}'",
            self.class_name()
        )?;

        // Add each field with its default value
        for field in &class.state {
            write!(self.output, ", '{}' => ", field.name.name)?;
            if let Some(default_value) = &field.default_value {
                self.generate_expression(default_value)?;
            } else {
                write!(self.output, "'nil'")?;
            }
        }

        writeln!(self.output, "}}~")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `new/1` function for a value type.
    ///
    /// Merges initialization arguments with default values.
    fn generate_value_type_new_with_args(&mut self) -> Result<()> {
        writeln!(self.output, "'new'/1 = fun (InitArgs) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let DefaultState = call '{}':'new'() in",
            self.module_name
        )?;
        self.write_indent()?;
        writeln!(self.output, "call 'maps':'merge'(DefaultState, InitArgs)")?;
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates an instance method for a value type.
    ///
    /// Value type methods are pure functions that take Self as first parameter
    /// and return a new instance (immutable semantics).
    fn generate_value_type_method(
        &mut self,
        method: &MethodDefinition,
        _class: &ClassDefinition,
    ) -> Result<()> {
        let mangled = method.selector.to_erlang_atom();
        let arity = method.parameters.len() + 1; // +1 for Self

        // Function signature: 'methodName'/Arity = fun (Self, Param1, Param2, ...) ->
        write!(self.output, "'{mangled}'/{arity}= fun (Self")?;
        for param in &method.parameters {
            let core_var = variable_context::VariableContext::to_core_var(&param.name);
            write!(self.output, ", {core_var}")?;
        }
        writeln!(self.output, ") ->")?;

        self.indent += 1;

        // Bind parameters in scope
        self.push_scope();
        // BT-295: Track method params for @primitive codegen
        self.current_method_params.clear();
        for param in &method.parameters {
            let core_var = variable_context::VariableContext::to_core_var(&param.name);
            self.bind_var(&param.name, &core_var);
            self.current_method_params.push(core_var);
        }

        // Generate method body expressions
        // For value types, there's no state threading - they're immutable
        // TODO(BT-213): Consider adding immutable update syntax (e.g., withX: newX) in future
        // Currently, field assignments are rejected at codegen (see generate_field_assignment)
        // Field reads work via CodeGenContext routing to Self parameter
        for (i, expr) in method.body.iter().enumerate() {
            let is_last = i == method.body.len() - 1;

            // Early return (^) — emit value and stop generating further expressions
            if let Expression::Return { value, .. } = expr {
                if i > 0 {
                    self.write_indent()?;
                }
                self.generate_expression(value)?;
                break;
            }

            if is_last {
                if i > 0 {
                    self.write_indent()?;
                }
                self.generate_expression(expr)?;
            } else {
                // Non-last expressions: wrap in let to sequence side effects
                let tmp_var = self.fresh_temp_var("seq");
                self.write_indent()?;
                write!(self.output, "let {tmp_var} = ")?;
                self.generate_expression(expr)?;
                writeln!(self.output, " in")?;
            }
        }

        self.pop_scope();
        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Returns true if the class is a primitive type (native Erlang value).
    ///
    /// Primitive types need `dispatch/3` and `has_method/1` for runtime
    /// dispatch via `beamtalk_primitive:send/3`.
    ///
    /// NOTE: Must stay in sync with `build_stdlib::is_primitive_type()`.
    fn is_primitive_type(class_name: &str) -> bool {
        matches!(
            class_name,
            "Integer"
                | "Float"
                | "String"
                | "True"
                | "False"
                | "UndefinedObject"
                | "Block"
                | "Symbol"
                | "Tuple"
                | "List"
                | "Dictionary"
                | "Set"
        )
    }

    /// Returns `true` for known non-primitive stdlib classes that compile to
    /// `bt_stdlib_{snake}` modules.
    ///
    /// NOTE: Must stay in sync with `build_stdlib::module_name_from_path()`.
    fn is_stdlib_nonprimitive_type(class_name: &str) -> bool {
        matches!(
            class_name,
            "Object"
                | "Number"
                | "Actor"
                | "File"
                | "Association"
                | "SystemDictionary"
                | "TranscriptStream"
                | "Exception"
                | "Error"
        )
    }

    /// Computes the module name for a superclass in the dispatch chain.
    ///
    /// Returns `None` for `ProtoObject` (root of the hierarchy — no module to
    /// delegate to). For stdlib classes, applies the naming convention from
    /// `build_stdlib::module_name_from_path`:
    /// - Primitive types → `beamtalk_{snake_case}`
    /// - Non-primitive stdlib types → `bt_stdlib_{snake_case}`
    ///
    /// For user-defined classes, falls back to `to_module_name(superclass)`
    /// so that dispatch delegates to the actual compiled module.
    ///
    /// NOTE: Must stay in sync with `build_stdlib::module_name_from_path()`.
    fn superclass_module_name(superclass: &str) -> Option<String> {
        if superclass == "ProtoObject" {
            return None;
        }
        let snake = super::util::to_module_name(superclass);
        if Self::is_primitive_type(superclass) {
            Some(format!("beamtalk_{snake}"))
        } else if Self::is_stdlib_nonprimitive_type(superclass) {
            Some(format!("bt_stdlib_{snake}"))
        } else {
            // User-defined superclass: use plain module name without prefix
            Some(snake)
        }
    }

    /// Encode a string as a Core Erlang binary literal.
    /// Core Erlang uses `#{#<charcode>(8,1,'integer',['unsigned'|['big']]),..}#`
    fn core_erlang_binary(s: &str) -> String {
        if s.is_empty() {
            return "#{}#".to_string();
        }
        let segments: Vec<String> = s
            .chars()
            .map(|ch| format!("#<{}>(8,1,'integer',['unsigned'|['big']])", ch as u32))
            .collect();
        format!("#{{{}}}#", segments.join(","))
    }

    /// Generates the `dispatch/3` function for a value type.
    ///
    /// This routes selectors to individual method functions, provides reflection
    /// methods (class, respondsTo:, instVarNames, instVarAt:, instVarAt:put:,
    /// perform:, perform:withArguments:), checks the extension registry for unknown
    /// selectors, and delegates to superclass dispatch/3 for inherited methods.
    /// Only raises `does_not_understand` at the hierarchy root (`ProtoObject`).
    #[allow(clippy::too_many_lines)]
    fn generate_primitive_dispatch(&mut self, class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let superclass_mod = Self::superclass_module_name(class.superclass_name());

        writeln!(self.output, "'dispatch'/3 = fun (Selector, Args, Self) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Selector of")?;

        // Reflection methods (inherited from Object for all primitives)
        self.indent += 1;

        // class
        self.write_indent()?;
        writeln!(self.output, "<'class'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'{class_name}'")?;
        self.indent -= 1;

        // respondsTo: — validate Args is a non-empty list
        self.write_indent()?;
        writeln!(self.output, "<'respondsTo'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Args of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<[RtSelector | _]> when 'true' -> call '{mod_name}':'has_method'(RtSelector)"
        )?;
        self.write_indent()?;
        writeln!(self.output, "<_> when 'true' -> 'false'")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        // asString — generate default for classes that don't define it
        let has_as_string = class
            .methods
            .iter()
            .any(|m| m.selector.name() == "asString");
        if !has_as_string {
            let default_str = match class_name.as_str() {
                "True" => "true",
                "False" => "false",
                "UndefinedObject" => "nil",
                "Block" => "a Block",
                _ => "",
            };
            if !default_str.is_empty() {
                self.write_indent()?;
                writeln!(self.output, "<'asString'> when 'true' ->")?;
                self.indent += 1;
                self.write_indent()?;
                let binary = Self::core_erlang_binary(default_str);
                writeln!(self.output, "{binary}")?;
                self.indent -= 1;
            }
        }

        // instVarNames — primitives have no instance variables
        self.write_indent()?;
        writeln!(self.output, "<'instVarNames'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "[]")?;
        self.indent -= 1;

        // instVarAt: — always returns nil for primitives
        self.write_indent()?;
        writeln!(self.output, "<'instVarAt'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'nil'")?;
        self.indent -= 1;

        // instVarAt:put: — error: primitives are immutable
        self.write_indent()?;
        writeln!(self.output, "<'instVarAt:put:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <ImmErr0> = call 'beamtalk_error':'new'('immutable_primitive', '{class_name}') in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <ImmErr1> = call 'beamtalk_error':'with_selector'(ImmErr0, 'instVarAt:put:') in"
        )?;
        self.write_indent()?;
        let immutable_hint = Self::core_erlang_binary(&format!(
            "{class_name}s are immutable. Use assignment (x := newValue) instead."
        ));
        writeln!(
            self.output,
            "let <ImmErr2> = call 'beamtalk_error':'with_hint'(ImmErr1, {immutable_hint}) in"
        )?;
        self.write_indent()?;
        writeln!(self.output, "call 'erlang':'error'(ImmErr2)")?;
        self.indent -= 1;

        // perform: — recursive dispatch
        self.write_indent()?;
        writeln!(self.output, "<'perform'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "let <PerfSel> = call 'erlang':'hd'(Args) in")?;
        self.write_indent()?;
        writeln!(
            self.output,
            "call '{mod_name}':'dispatch'(PerfSel, [], Self)"
        )?;
        self.indent -= 1;

        // perform:withArguments: — recursive dispatch with args
        self.write_indent()?;
        writeln!(self.output, "<'perform:withArguments:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "let <PwaSel> = call 'erlang':'hd'(Args) in")?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <PwaArgs> = call 'erlang':'hd'(call 'erlang':'tl'(Args)) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "call '{mod_name}':'dispatch'(PwaSel, PwaArgs, Self)"
        )?;
        self.indent -= 1;

        // Route each class-defined method to its individual function
        for method in &class.methods {
            let mangled = method.selector.to_erlang_atom();
            let arity = method.parameters.len() + 1; // +1 for Self
            self.write_indent()?;
            writeln!(self.output, "<'{mangled}'> when 'true' ->")?;
            self.indent += 1;
            self.write_indent()?;

            // Build argument list: Self, then each arg from Args list
            if method.parameters.is_empty() {
                writeln!(self.output, "call '{mod_name}':'{mangled}'(Self)")?;
            } else {
                // Extract args from Args list: hd(Args), hd(tl(Args)), ...
                for (i, _param) in method.parameters.iter().enumerate() {
                    let arg_var = format!("DispArg{i}");
                    let mut access = "Args".to_string();
                    for _ in 0..i {
                        access = format!("call 'erlang':'tl'({access})");
                    }
                    writeln!(
                        self.output,
                        "let <{arg_var}> = call 'erlang':'hd'({access}) in"
                    )?;
                    self.write_indent()?;
                }
                write!(self.output, "call '{mod_name}':'{mangled}'(Self")?;
                for i in 0..method.parameters.len() {
                    write!(self.output, ", DispArg{i}")?;
                }
                writeln!(self.output, ")")?;
            }

            let _ = arity;
            self.indent -= 1;
        }

        // Default case: extension fallback, then superclass delegation
        self.write_indent()?;
        writeln!(self.output, "<_Other> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'beamtalk_extensions':'lookup'('{class_name}', Selector) of"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<{{'ok', ExtFun, _ExtOwner}}> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "apply ExtFun(Args, Self)")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<'not_found'> when 'true' ->")?;
        self.indent += 1;

        if let Some(ref super_mod) = superclass_mod {
            // Delegate to superclass dispatch/3
            self.write_indent()?;
            writeln!(
                self.output,
                "call '{super_mod}':'dispatch'(Selector, Args, Self)"
            )?;
        } else {
            // Root of hierarchy — raise does_not_understand
            // Use runtime class_of(Self) to get the *receiver's* actual class,
            // not the module where the error is raised (which may be a superclass).
            self.write_indent()?;
            writeln!(
                self.output,
                "let <DnuClass> = call 'beamtalk_primitive':'class_of'(Self) in"
            )?;
            self.write_indent()?;
            writeln!(
                self.output,
                "let <DnuErr0> = call 'beamtalk_error':'new'('does_not_understand', DnuClass) in"
            )?;
            self.write_indent()?;
            writeln!(
                self.output,
                "let <DnuErr1> = call 'beamtalk_error':'with_selector'(DnuErr0, Selector) in"
            )?;
            self.write_indent()?;
            let dnu_hint = Self::core_erlang_binary(
                "Check spelling or use 'respondsTo:' to verify method exists",
            );
            writeln!(
                self.output,
                "let <DnuErr2> = call 'beamtalk_error':'with_hint'(DnuErr1, {dnu_hint}) in"
            )?;
            self.write_indent()?;
            writeln!(self.output, "call 'erlang':'error'(DnuErr2)")?;
        }

        self.indent -= 1;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        // Close the outer case (selector dispatch)
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;

        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates the `has_method/1` function for a value type.
    ///
    /// Returns `true` for all known selectors (class-defined + reflection +
    /// extensions + superclass methods via delegation).
    fn generate_primitive_has_method(&mut self, class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name().clone();
        let superclass_mod = Self::superclass_module_name(class.superclass_name());

        writeln!(self.output, "'has_method'/1 = fun (Selector) ->")?;
        self.indent += 1;
        self.write_indent()?;

        // Build list of all known selectors
        let mut selectors: Vec<String> = vec![
            // Reflection methods
            "'class'".to_string(),
            "'respondsTo'".to_string(),
            "'instVarNames'".to_string(),
            "'instVarAt'".to_string(),
            "'instVarAt:put:'".to_string(),
            "'perform'".to_string(),
            "'perform:withArguments:'".to_string(),
        ];

        // Include default asString if dispatch/3 generates one
        let has_explicit_as_string = class
            .methods
            .iter()
            .any(|m| m.selector.name() == "asString");
        if !has_explicit_as_string
            && matches!(
                class_name.as_str(),
                "True" | "False" | "UndefinedObject" | "Block"
            )
        {
            selectors.push("'asString'".to_string());
        }

        // Add class-defined methods
        for method in &class.methods {
            let mangled = method.selector.to_erlang_atom();
            selectors.push(format!("'{mangled}'"));
        }

        // Check lists:member first, then extensions, then superclass
        writeln!(
            self.output,
            "case call 'lists':'member'(Selector, [{}]) of",
            selectors.join(", ")
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<'true'> when 'true' -> 'true'")?;
        self.write_indent()?;
        writeln!(self.output, "<'false'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "case call 'beamtalk_extensions':'has'('{class_name}', Selector) of"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<'true'> when 'true' -> 'true'")?;
        self.write_indent()?;

        if let Some(ref super_mod) = superclass_mod {
            writeln!(
                self.output,
                "<'false'> when 'true' -> call '{super_mod}':'has_method'(Selector)"
            )?;
        } else {
            writeln!(self.output, "<'false'> when 'true' -> 'false'")?;
        }

        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;

        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }
}
