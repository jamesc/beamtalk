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
        // BT-446: All value types also export dispatch/4 for actor hierarchy walk.
        // The dispatch service checks function_exported(Module, dispatch, 4) and
        // skips modules that only have dispatch/3.
        exports.push("'dispatch'/4".to_string());
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
        // BT-446: Generate dispatch/4 for actor hierarchy walk.
        // The dispatch service only calls modules that export dispatch/4.
        self.generate_dispatch_4(class)?;
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
    /// - Non-instantiable primitives (Integer, String, etc.): raises `instantiation_error`
    /// - Collection primitives (Dictionary, List, Tuple): returns empty native value
    /// - Other value types: creates an instance map with `$beamtalk_class` and defaults
    fn generate_value_type_new(&mut self, class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name().clone();
        if Self::is_non_instantiable_primitive(&class_name) {
            return self.generate_primitive_new_error(&class_name, "new", 0);
        }
        if let Some(empty_val) = Self::collection_empty_value(&class_name) {
            return self.generate_collection_new(empty_val);
        }

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
    /// - Non-instantiable primitives: raises `instantiation_error`
    /// - Collection primitives: raises `instantiation_error` (no meaningful merge)
    /// - Other value types: merges initialization arguments with defaults
    fn generate_value_type_new_with_args(&mut self) -> Result<()> {
        let class_name = self.class_name().clone();
        if Self::is_non_instantiable_primitive(&class_name) {
            return self.generate_primitive_new_error(&class_name, "new:", 1);
        }
        if Self::collection_empty_value(&class_name).is_some() {
            return self.generate_primitive_new_error(&class_name, "new:", 1);
        }

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

    /// Generates a `new` or `new:` function that raises `instantiation_error`
    /// for primitive types that cannot be instantiated with `new`.
    fn generate_primitive_new_error(
        &mut self,
        class_name: &str,
        selector: &str,
        arity: usize,
    ) -> Result<()> {
        let hint =
            format!("{class_name} is a primitive type and cannot be instantiated with {selector}");
        if arity == 0 {
            writeln!(self.output, "'new'/0 = fun () ->")?;
        } else {
            writeln!(self.output, "'new'/1 = fun (_InitArgs) ->")?;
        }
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error0 = call 'beamtalk_error':'new'('instantiation_error', '{class_name}') in",
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let Error1 = call 'beamtalk_error':'with_selector'(Error0, '{selector}') in",
        )?;
        self.write_indent()?;
        write!(
            self.output,
            "let Error2 = call 'beamtalk_error':'with_hint'(Error1, "
        )?;
        self.generate_binary_string(&hint)?;
        writeln!(self.output, ") in")?;
        self.write_indent()?;
        writeln!(self.output, "call 'erlang':'error'(Error2)")?;
        self.indent -= 1;
        writeln!(self.output)?;
        Ok(())
    }

    /// Generates `new/0` for a collection type that returns an empty native value.
    fn generate_collection_new(&mut self, empty_value: &str) -> Result<()> {
        writeln!(self.output, "'new'/0 = fun () ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "{empty_value}")?;
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

    /// Returns true if the class is a non-instantiable primitive type.
    ///
    /// These types have no sensible default/empty value and cannot be
    /// instantiated with `new`. For example, `Integer new` makes no sense —
    /// integers are created via literals (`42`).
    ///
    /// Collection types (Dictionary, List, Tuple) are NOT included here
    /// because they have sensible empty values (e.g., `Dictionary new` → `#{}`).
    fn is_non_instantiable_primitive(class_name: &str) -> bool {
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
                | "CompiledMethod"
        )
    }

    /// Returns the Core Erlang literal for a collection type's empty value,
    /// or `None` if the class is not a collection primitive.
    fn collection_empty_value(class_name: &str) -> Option<&'static str> {
        match class_name {
            "Dictionary" => Some("~{}~"),
            "List" => Some("[]"),
            "Tuple" => Some("{}"),
            _ => None,
        }
    }

    /// Returns true if the class is a known stdlib type (ADR 0016).
    ///
    /// All stdlib types compile to `bt@stdlib@{snake_case}` modules.
    fn is_known_stdlib_type(class_name: &str) -> bool {
        Self::is_non_instantiable_primitive(class_name)
            || Self::collection_empty_value(class_name).is_some()
            || matches!(
                class_name,
                "Set"
                    | "ProtoObject"
                    | "Object"
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

    /// Computes the compiled module name for a class (ADR 0016).
    ///
    /// - Stdlib classes → `bt@stdlib@{snake_case}`
    /// - User-defined classes → `bt@{snake_case}`
    pub fn compiled_module_name(class_name: &str) -> String {
        let snake = super::util::to_module_name(class_name);
        if Self::is_known_stdlib_type(class_name) {
            format!("bt@stdlib@{snake}")
        } else {
            format!("bt@{snake}")
        }
    }

    /// Computes the module name for a superclass in the dispatch chain.
    ///
    /// Returns `None` for `ProtoObject` (root of the hierarchy — no module to
    /// delegate to). For stdlib classes, applies the ADR 0016 naming convention:
    /// all stdlib types → `bt@stdlib@{snake_case}`.
    ///
    /// For user-defined classes, uses `bt@{snake_case}` prefix.
    fn superclass_module_name(superclass: &str) -> Option<String> {
        if superclass == "ProtoObject" {
            return None;
        }
        Some(Self::compiled_module_name(superclass))
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
    ///
    /// BT-447: For classes with zero instance methods (e.g., File), generates a
    /// minimal stub that handles only `class` and `respondsTo:`, delegating
    /// everything else to the superclass.
    #[allow(clippy::too_many_lines)]
    fn generate_primitive_dispatch(&mut self, class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let superclass_mod = Self::superclass_module_name(class.superclass_name());

        // BT-447: Class-methods-only classes skip protocol boilerplate
        if class.methods.is_empty() && superclass_mod.is_some() {
            return self.generate_minimal_dispatch(class);
        }

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
        writeln!(self.output, "<'respondsTo:'> when 'true' ->")?;
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

        // instVarAt: — error: primitives are immutable
        self.write_indent()?;
        writeln!(self.output, "<'instVarAt:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <IvaErr0> = call 'beamtalk_error':'new'('immutable_value', '{class_name}') in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <IvaErr1> = call 'beamtalk_error':'with_selector'(IvaErr0, 'instVarAt:') in"
        )?;
        self.write_indent()?;
        let iva_hint = Self::core_erlang_binary(&format!(
            "{class_name}s are immutable and have no instance variables."
        ));
        writeln!(
            self.output,
            "let <IvaErr2> = call 'beamtalk_error':'with_hint'(IvaErr1, {iva_hint}) in"
        )?;
        self.write_indent()?;
        writeln!(self.output, "call 'erlang':'error'(IvaErr2)")?;
        self.indent -= 1;

        // instVarAt:put: — error: primitives are immutable
        self.write_indent()?;
        writeln!(self.output, "<'instVarAt:put:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <ImmErr0> = call 'beamtalk_error':'new'('immutable_value', '{class_name}') in"
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
        writeln!(self.output, "<'perform:'> when 'true' ->")?;
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

    /// Generates the `dispatch/4` function for a value type (BT-446).
    ///
    /// The dispatch service (`beamtalk_dispatch:invoke_method/6`) only calls
    /// modules that export `dispatch/4`. Without this, compiled value type
    /// modules are skipped during the actor hierarchy walk, preventing actors
    /// from inheriting Object-level methods.
    ///
    /// For Object specifically, this generates state-aware implementations
    /// for reflection primitives (instVarAt:, printString, etc.) that need
    /// access to the actor's state map. For all other value types, this is
    /// a simple wrapper that delegates to dispatch/3.
    fn generate_dispatch_4(&mut self, _class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();

        writeln!(
            self.output,
            "'dispatch'/4 = fun (Selector, Args, Self, State) ->"
        )?;
        self.indent += 1;

        if class_name == "Object" {
            self.generate_object_dispatch_4(&mod_name)?;
        } else {
            // Simple wrapper: delegate to dispatch/3, wrap result in {reply, _, State}
            self.generate_dispatch_4_wrapper(&mod_name)?;
        }

        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// Generates Object's dispatch/4 with state-aware reflection primitives.
    ///
    /// Actor instances inherit Object methods via the hierarchy walk. These
    /// methods need access to the actor's State map for proper reflection
    /// (field access, class name, etc.).
    #[allow(clippy::too_many_lines)]
    fn generate_object_dispatch_4(&mut self, mod_name: &str) -> Result<()> {
        self.write_indent()?;
        writeln!(self.output, "case Selector of")?;
        self.indent += 1;

        // --- class: return actual class from State ---
        self.write_indent()?;
        writeln!(self.output, "<'class'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'reply', call 'beamtalk_tagged_map':'class_of'(State), State}}"
        )?;
        self.indent -= 1;

        // --- respondsTo: validate [Selector] arg, check against actual class ---
        self.write_indent()?;
        writeln!(self.output, "<'respondsTo:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Args of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<[RtSel4|_]> when call 'erlang':'is_atom'(RtSel4) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <RtClass4> = call 'beamtalk_tagged_map':'class_of'(State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'reply', call 'beamtalk_dispatch':'responds_to'(RtSel4, RtClass4), State}}"
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<_RtBadArgs> when 'true' -> {{'reply', 'false', State}}"
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        // --- instVarNames: return actual field names from State ---
        self.write_indent()?;
        writeln!(self.output, "<'instVarNames'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'reply', call 'beamtalk_reflection':'field_names'(State), State}}"
        )?;
        self.indent -= 1;

        // --- instVarAt: validate [Name] arg, read field value from State ---
        self.write_indent()?;
        writeln!(self.output, "<'instVarAt:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Args of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<[IvaName4|_]> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'reply', call 'beamtalk_reflection':'read_field'(IvaName4, State), State}}"
        )?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<_IvaBadArgs> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        self.generate_arity_error("'instVarAt:'", 1)?;
        self.indent -= 1;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        // --- instVarAt:put: validate [Name, Value] args, write field value ---
        self.write_indent()?;
        writeln!(self.output, "<'instVarAt:put:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Args of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<[IvapName4|IvapRest4]> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case IvapRest4 of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "<[IvapVal4|_]> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <IvapTuple4> = call 'beamtalk_reflection':'write_field'(IvapName4, IvapVal4, State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <IvapResult4> = call 'erlang':'element'(1, IvapTuple4) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <IvapNewState4> = call 'erlang':'element'(2, IvapTuple4) in"
        )?;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', IvapResult4, IvapNewState4}}")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<_IvapBad2> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        self.generate_arity_error("'instVarAt:put:'", 2)?;
        self.indent -= 1;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<_IvapBad1> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        self.generate_arity_error("'instVarAt:put:'", 2)?;
        self.indent -= 1;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        // --- printString: use class name from State ---
        self.write_indent()?;
        writeln!(self.output, "<'printString'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <PsClass4> = call 'beamtalk_tagged_map':'class_of'(State, 'Object') in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <PsStr4> = call 'erlang':'iolist_to_binary'([{}|[call 'erlang':'atom_to_binary'(PsClass4, 'utf8')]]) in",
            Self::core_erlang_binary("a ")
        )?;
        self.write_indent()?;
        writeln!(self.output, "{{'reply', PsStr4, State}}")?;
        self.indent -= 1;

        // --- inspect: use class + fields from State ---
        self.write_indent()?;
        writeln!(self.output, "<'inspect'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "{{'reply', call 'beamtalk_reflection':'inspect_string'(State), State}}"
        )?;
        self.indent -= 1;

        // --- perform: validate [Selector] arg with is_atom guard ---
        self.write_indent()?;
        writeln!(self.output, "<'perform:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Args of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<[PerfSel4|_]> when call 'erlang':'is_atom'(PerfSel4) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <PerfClass4> = call 'beamtalk_tagged_map':'class_of'(State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <PerfResult4> = call 'beamtalk_dispatch':'lookup'(PerfSel4, [], Self, State, PerfClass4) in"
        )?;
        self.write_indent()?;
        // Normalize {error, Error} to {error, Error, State}
        writeln!(self.output, "case PerfResult4 of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'error', PerfErr4}}> when 'true' -> {{'error', PerfErr4, State}}"
        )?;
        self.write_indent()?;
        writeln!(self.output, "<PerfOther4> when 'true' -> PerfOther4")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<_PerfBadArgs> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        self.generate_type_error("'perform:'", "selector must be an atom")?;
        self.indent -= 1;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        // --- perform:withArguments: validate [Selector, ArgList] with type guards ---
        self.write_indent()?;
        writeln!(self.output, "<'perform:withArguments:'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Args of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<[PwaSel4|PwaRest4]> when call 'erlang':'is_atom'(PwaSel4) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case PwaRest4 of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<[PwaArgs4|_]> when call 'erlang':'is_list'(PwaArgs4) ->"
        )?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <PwaClass4> = call 'beamtalk_tagged_map':'class_of'(State) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <PwaResult4> = call 'beamtalk_dispatch':'lookup'(PwaSel4, PwaArgs4, Self, State, PwaClass4) in"
        )?;
        self.write_indent()?;
        writeln!(self.output, "case PwaResult4 of")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(
            self.output,
            "<{{'error', PwaErr4}}> when 'true' -> {{'error', PwaErr4, State}}"
        )?;
        self.write_indent()?;
        writeln!(self.output, "<PwaOther4> when 'true' -> PwaOther4")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<_PwaBadArgList> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        self.generate_type_error("'perform:withArguments:'", "arguments must be a list")?;
        self.indent -= 1;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "<_PwaBadSel> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        self.generate_type_error("'perform:withArguments:'", "selector must be an atom")?;
        self.indent -= 1;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        // --- Default: delegate to dispatch/3 with try-catch ---
        self.write_indent()?;
        writeln!(self.output, "<_OtherSel4> when 'true' ->")?;
        self.indent += 1;
        self.generate_dispatch_4_wrapper(mod_name)?;
        self.indent -= 1;

        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;

        Ok(())
    }

    /// Generates a simple dispatch/4 wrapper that delegates to dispatch/3.
    ///
    /// Uses try-catch to convert exceptions from dispatch/3 (e.g., `does_not_understand`
    /// calling `erlang:error`) into `{error, Error, State}` tuples expected by the
    /// dispatch service.
    fn generate_dispatch_4_wrapper(&mut self, mod_name: &str) -> Result<()> {
        self.write_indent()?;
        writeln!(
            self.output,
            "try call '{mod_name}':'dispatch'(Selector, Args, Self)"
        )?;
        self.write_indent()?;
        writeln!(self.output, "of <D4Result> -> {{'reply', D4Result, State}}")?;
        self.write_indent()?;
        writeln!(
            self.output,
            "catch <_D4Type, D4Error, _D4Stack> -> {{'error', D4Error, State}}"
        )?;

        Ok(())
    }

    /// Generates a structured `arity_mismatch` error for dispatch/4 argument validation.
    fn generate_arity_error(&mut self, selector: &str, expected_arity: u32) -> Result<()> {
        let hint = Self::core_erlang_binary(&format!(
            "Expected {expected_arity} argument(s) for {selector}"
        ));
        self.write_indent()?;
        writeln!(
            self.output,
            "let <ArErr0> = call 'beamtalk_error':'new'('arity_mismatch', call 'beamtalk_tagged_map':'class_of'(State, 'Object')) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <ArErr1> = call 'beamtalk_error':'with_selector'(ArErr0, {selector}) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <ArErr2> = call 'beamtalk_error':'with_hint'(ArErr1, {hint}) in"
        )?;
        self.write_indent()?;
        writeln!(self.output, "{{'error', ArErr2, State}}")?;
        Ok(())
    }

    /// Generates a structured `type_error` for dispatch/4 argument validation.
    fn generate_type_error(&mut self, selector: &str, hint_msg: &str) -> Result<()> {
        let hint = Self::core_erlang_binary(hint_msg);
        self.write_indent()?;
        writeln!(
            self.output,
            "let <TyErr0> = call 'beamtalk_error':'new'('type_error', call 'beamtalk_tagged_map':'class_of'(State, 'Object')) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <TyErr1> = call 'beamtalk_error':'with_selector'(TyErr0, {selector}) in"
        )?;
        self.write_indent()?;
        writeln!(
            self.output,
            "let <TyErr2> = call 'beamtalk_error':'with_hint'(TyErr1, {hint}) in"
        )?;
        self.write_indent()?;
        writeln!(self.output, "{{'error', TyErr2, State}}")?;
        Ok(())
    }

    /// Generates the `has_method/1` function for a value type.
    ///
    /// Returns `true` for all known selectors (class-defined + reflection +
    /// extensions + superclass methods via delegation).
    fn generate_primitive_has_method(&mut self, class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name().clone();
        let superclass_mod = Self::superclass_module_name(class.superclass_name());

        // BT-447: Class-methods-only classes delegate directly to superclass
        if class.methods.is_empty() && superclass_mod.is_some() {
            return self.generate_minimal_has_method(class);
        }

        writeln!(self.output, "'has_method'/1 = fun (Selector) ->")?;
        self.indent += 1;
        self.write_indent()?;

        // Build list of all known selectors
        let mut selectors: Vec<String> = vec![
            // Reflection methods
            "'class'".to_string(),
            "'respondsTo:'".to_string(),
            "'instVarNames'".to_string(),
            "'instVarAt:'".to_string(),
            "'instVarAt:put:'".to_string(),
            "'perform:'".to_string(),
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

    /// BT-447: Generates a minimal `dispatch/3` for classes with no instance methods.
    ///
    /// Handles `class`, `respondsTo:`, `perform:`, and `perform:withArguments:`
    /// locally, then checks extensions and delegates everything else to the
    /// superclass. `perform:` must re-dispatch through this module to preserve
    /// extension lookup and correct error attribution. Skips instVarNames,
    /// instVarAt:, instVarAt:put: since the superclass already handles them.
    fn generate_minimal_dispatch(&mut self, class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let super_mod = Self::superclass_module_name(class.superclass_name())
            .expect("minimal dispatch requires superclass");

        writeln!(self.output, "'dispatch'/3 = fun (Selector, Args, Self) ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "case Selector of")?;
        self.indent += 1;

        // class
        self.write_indent()?;
        writeln!(self.output, "<'class'> when 'true' ->")?;
        self.indent += 1;
        self.write_indent()?;
        writeln!(self.output, "'{class_name}'")?;
        self.indent -= 1;

        // respondsTo:
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

        // perform: — re-dispatch through this module to preserve extension lookup
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

        // perform:withArguments: — re-dispatch through this module
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

        // Default: extension check, then superclass delegation
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
        self.write_indent()?;
        writeln!(
            self.output,
            "call '{super_mod}':'dispatch'(Selector, Args, Self)"
        )?;
        self.indent -= 1;
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;
        self.indent -= 1;

        // Close outer case
        self.indent -= 1;
        self.write_indent()?;
        writeln!(self.output, "end")?;

        self.indent -= 1;
        writeln!(self.output)?;

        Ok(())
    }

    /// BT-447: Generates a minimal `has_method/1` for classes with no instance methods.
    ///
    /// Checks `class`, `respondsTo:`, `perform:`, `perform:withArguments:`,
    /// then extensions, then delegates to superclass.
    fn generate_minimal_has_method(&mut self, class: &ClassDefinition) -> Result<()> {
        let class_name = self.class_name().clone();
        let super_mod = Self::superclass_module_name(class.superclass_name())
            .expect("minimal has_method requires superclass");

        writeln!(self.output, "'has_method'/1 = fun (Selector) ->")?;
        self.indent += 1;
        self.write_indent()?;

        // class, respondsTo:, perform:, perform:withArguments: are handled locally
        writeln!(
            self.output,
            "case call 'lists':'member'(Selector, ['class', 'respondsTo', 'perform', 'perform:withArguments:']) of"
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
        writeln!(
            self.output,
            "<'false'> when 'true' -> call '{super_mod}':'has_method'(Selector)"
        )?;
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
