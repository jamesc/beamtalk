// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value type module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for value type classes — plain Erlang
//! terms (maps) with no process. They are created with `new` and `new:`,
//! not `spawn`, and methods are synchronous functions operating on maps.

use super::document::Document;
use super::util::ClassIdentity;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{
    ClassDefinition, Expression, MessageSelector, MethodDefinition, MethodKind, Module,
};
use crate::docvec;

// Auto-generated from lib/*.bt by build.rs — do not edit manually.
include!(concat!(env!("OUT_DIR"), "/stdlib_types.rs"));

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
    pub(super) fn generate_value_type_module(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // BT-213: Set context to ValueType for this module
        self.context = CodeGenContext::ValueType;

        let class = module
            .classes
            .first()
            .ok_or_else(|| CodeGenError::Internal("Value type module has no class".to_string()))?;

        // Set class identity early so that class_name() returns the AST
        // class name rather than deriving from the module name.
        self.class_identity = Some(ClassIdentity::new(&class.name.name));

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
                let arity = method.parameters.len() + 2; // +2 for ClassSelf + ClassVars
                exports.push(format!("'class_{}'/{arity}", method.selector.name()));
            }
        }

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Module header
        let module_name = self.module_name.clone();
        docs.push(docvec![
            format!("module '{}' [{}]\n", module_name, exports.join(", ")),
            "  attributes ['on_load' = [{'register_class', 0}]]\n",
            "\n",
        ]);

        // Generate new/0 - creates instance with default field values
        if !has_explicit_new {
            docs.push(self.generate_value_type_new(class)?);
            docs.push(Document::Str("\n"));
        }

        // Generate new/1 - creates instance with initialization arguments
        if !has_explicit_new_with {
            docs.push(self.generate_value_type_new_with_args()?);
            docs.push(Document::Str("\n"));
        }

        // Generate instance methods as pure functions
        for method in &class.methods {
            docs.push(self.generate_value_type_method(method, class)?);
            docs.push(Document::Str("\n"));
        }

        // Generate dispatch/3 and has_method/1 for all value types
        // (superclass delegation chain — same pattern as actors)
        docs.push(self.generate_primitive_dispatch(class)?);
        docs.push(Document::Str("\n"));
        // BT-446: Generate dispatch/4 for actor hierarchy walk.
        // The dispatch service only calls modules that export dispatch/4.
        docs.push(self.generate_dispatch_4(class)?);
        docs.push(Document::Str("\n"));
        docs.push(self.generate_primitive_has_method(class)?);
        docs.push(Document::Str("\n"));

        // Generate superclass/0 for reflection
        let superclass_atom = class.superclass.as_ref().map_or("nil", |s| s.name.as_str());
        docs.push(docvec![
            format!("'superclass'/0 = fun () -> '{superclass_atom}'\n"),
            "\n",
        ]);

        // BT-411: Generate class-side method functions
        if !class.class_methods.is_empty() {
            docs.push(self.generate_class_method_functions(class)?);
            docs.push(Document::Str("\n"));
        }

        // BT-246: Register value type class with the class system for dynamic dispatch
        docs.push(self.generate_register_class(module)?);
        docs.push(Document::Str("\n"));

        // Module end
        docs.push(Document::Str("end\n"));

        Ok(Document::Vec(docs))
    }

    /// Generates the `new/0` function for a value type.
    ///
    /// - Non-instantiable primitives (Integer, String, etc.): raises `instantiation_error`
    /// - Collection primitives (Dictionary, List, Tuple): returns empty native value
    /// - Other value types: creates an instance map with `$beamtalk_class` and defaults
    fn generate_value_type_new(&mut self, class: &ClassDefinition) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        if Self::is_non_instantiable_primitive(class_name.as_str()) {
            return self.generate_primitive_new_error(class_name.as_str(), "new", 0);
        }
        if let Some(empty_val) = Self::collection_empty_value(class_name.as_str()) {
            return Ok(Self::generate_collection_new(empty_val));
        }

        // Start building the document
        let mut field_parts = Vec::new();

        // Add each field with its default value
        for field in &class.state {
            let default_code = if let Some(default_value) = &field.default_value {
                self.capture_expression(default_value)?
            } else {
                "'nil'".to_string()
            };
            field_parts.push(format!(", '{}' => {}", field.name.name, default_code));
        }

        Ok(docvec![
            "'new'/0 = fun () ->\n",
            "    ~{'$beamtalk_class' => '",
            class_name,
            "'",
            field_parts.join(""),
            "}~\n",
            "\n",
        ])
    }

    /// Generates the `new/1` function for a value type.
    ///
    /// - Non-instantiable primitives: raises `instantiation_error`
    /// - Dictionary: returns `InitArgs` directly (dictionary IS a map)
    /// - List, Tuple: raises `instantiation_error` (no meaningful init-from-map)
    /// - Other value types: merges initialization arguments with defaults
    fn generate_value_type_new_with_args(&mut self) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        if Self::is_non_instantiable_primitive(class_name.as_str()) {
            return self.generate_primitive_new_error(class_name.as_str(), "new:", 1);
        }
        // Dictionary new: #{key => val} returns the map directly
        if class_name == "Dictionary" {
            return Ok(Self::generate_collection_new_with_args());
        }
        // List and Tuple have no meaningful init-from-map pattern
        if Self::collection_empty_value(class_name.as_str()).is_some() {
            return self.generate_primitive_new_error(class_name.as_str(), "new:", 1);
        }

        let module_name = self.module_name.clone();
        Ok(docvec![
            "'new'/1 = fun (InitArgs) ->\n",
            "    let DefaultState = call '",
            module_name,
            "':'new'() in\n",
            "    call 'maps':'merge'(DefaultState, InitArgs)\n",
            "\n",
        ])
    }

    /// Generates a `new` or `new:` function that raises `instantiation_error`
    /// for primitive types that cannot be instantiated with `new`.
    #[allow(clippy::unnecessary_wraps, clippy::unused_self)] // uniform codegen interface
    fn generate_primitive_new_error(
        &mut self,
        class_name: &str,
        selector: &str,
        arity: usize,
    ) -> Result<Document<'static>> {
        let hint =
            format!("{class_name} is a primitive type and cannot be instantiated with {selector}");
        let hint_binary = Self::core_erlang_binary(&hint);

        let function_head = if arity == 0 {
            "'new'/0 = fun () ->"
        } else {
            "'new'/1 = fun (_InitArgs) ->"
        };

        Ok(docvec![
            function_head,
            "\n",
            format!(
                "    let Error0 = call 'beamtalk_error':'new'('instantiation_error', '{class_name}') in\n"
            ),
            format!(
                "    let Error1 = call 'beamtalk_error':'with_selector'(Error0, '{selector}') in\n"
            ),
            "    let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
            hint_binary,
            ") in\n",
            "    call 'beamtalk_error':'raise'(Error2)\n",
            "\n",
        ])
    }

    /// Generates `new/0` for a collection type that returns an empty native value.
    fn generate_collection_new(empty_value: &str) -> Document<'static> {
        docvec![
            "'new'/0 = fun () ->\n",
            "    ",
            empty_value.to_string(),
            "\n",
            "\n",
        ]
    }

    /// Generates `new/1` for Dictionary: returns `InitArgs` directly.
    fn generate_collection_new_with_args() -> Document<'static> {
        docvec!["'new'/1 = fun (InitArgs) ->\n", "    InitArgs\n", "\n",]
    }

    /// Generates an instance method for a value type.
    ///
    /// Value type methods are pure functions that take Self as first parameter
    /// and return a new instance (immutable semantics).
    fn generate_value_type_method(
        &mut self,
        method: &MethodDefinition,
        _class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let mangled = method.selector.to_erlang_atom();
        let arity = method.parameters.len() + 1; // +1 for Self

        // Bind parameters via fresh_var (not to_core_var) so names go through
        // the counter and can't collide with sequencing temp vars — BT-369
        self.push_scope();
        // BT-295: Track method params for @primitive codegen
        self.current_method_params.clear();
        let mut params = vec!["Self".to_string()];
        for param in &method.parameters {
            let var_name = self.fresh_var(&param.name);
            self.current_method_params.push(var_name.clone());
            params.push(var_name);
        }

        // Build method body parts
        let mut body_parts: Vec<Document<'static>> = Vec::new();

        // Generate method body expressions
        // For value types, there's no state threading - they're immutable
        // TODO(BT-213): Consider adding immutable update syntax (e.g., withX: newX) in future
        // Currently, field assignments are rejected at codegen (see generate_field_assignment)
        // Field reads work via CodeGenContext routing to Self parameter
        for (i, expr) in method.body.iter().enumerate() {
            let is_last = i == method.body.len() - 1;

            // Early return (^) — emit value and stop generating further expressions
            if let Expression::Return { value, .. } = expr {
                let expr_code = self.capture_expression(value)?;
                if i > 0 {
                    body_parts.push(Document::Str("    "));
                }
                body_parts.push(Document::String(expr_code));
                break;
            }

            if is_last {
                let expr_code = self.capture_expression(expr)?;
                if i > 0 {
                    body_parts.push(Document::Str("    "));
                }
                body_parts.push(Document::String(expr_code));
            } else {
                // Non-last expressions: wrap in let to sequence side effects
                let tmp_var = self.fresh_temp_var("seq");
                let expr_code = self.capture_expression(expr)?;
                body_parts.push(Document::String(format!(
                    "    let {tmp_var} = {expr_code} in\n"
                )));
            }
        }

        self.pop_scope();

        Ok(docvec![
            format!("'{}'/{}= fun ({}) ->\n", mangled, arity, params.join(", ")),
            Document::Vec(body_parts),
            "\n",
        ])
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
    /// Derived automatically from `lib/*.bt` via `build.rs` (BT-472).
    fn is_known_stdlib_type(class_name: &str) -> bool {
        STDLIB_CLASS_NAMES.contains(&class_name)
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
    /// Delegates to the shared UTF-8 binary literal helper.
    fn core_erlang_binary(s: &str) -> String {
        Self::binary_string_literal(s)
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
    #[allow(clippy::too_many_lines)] // one arm per primitive selector
    fn generate_primitive_dispatch(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let superclass_mod = Self::superclass_module_name(class.superclass_name());

        // BT-447: Class-methods-only classes skip protocol boilerplate
        if class.methods.is_empty() && superclass_mod.is_some() {
            return self.generate_minimal_dispatch(class);
        }

        // asString — generate default for classes that don't define it
        let has_as_string = class
            .methods
            .iter()
            .any(|m| m.selector.name() == "asString");
        let as_string_branch: Document<'static> = if has_as_string {
            Document::Vec(Vec::new())
        } else {
            let default_str = match class_name.as_str() {
                "True" => "true",
                "False" => "false",
                "UndefinedObject" => "nil",
                "Block" => "a Block",
                _ => "",
            };
            if default_str.is_empty() {
                Document::Vec(Vec::new())
            } else {
                let binary = Self::core_erlang_binary(default_str);
                docvec![
                    "        <'asString'> when 'true' ->\n",
                    format!("            {binary}\n"),
                ]
            }
        };

        // instVarAt: error
        let iva_hint = Self::core_erlang_binary(&format!(
            "{class_name}s are immutable and have no instance variables."
        ));

        // instVarAt:put: error
        let immutable_hint = Self::core_erlang_binary(&format!(
            "{class_name}s are immutable. Use assignment (x := newValue) instead."
        ));

        // Route each class-defined method to its individual function
        let mut method_branches: Vec<Document<'static>> = Vec::new();
        for method in &class.methods {
            let mangled = method.selector.to_erlang_atom();
            method_branches.push(Document::String(format!(
                "        <'{mangled}'> when 'true' ->\n"
            )));

            if method.parameters.is_empty() {
                method_branches.push(Document::String(format!(
                    "            call '{mod_name}':'{mangled}'(Self)\n"
                )));
            } else {
                // Extract args from Args list: hd(Args), hd(tl(Args)), ...
                for (i, _param) in method.parameters.iter().enumerate() {
                    let arg_var = format!("DispArg{i}");
                    let mut access = "Args".to_string();
                    for _ in 0..i {
                        access = format!("call 'erlang':'tl'({access})");
                    }
                    method_branches.push(Document::String(format!(
                        "            let <{arg_var}> = call 'erlang':'hd'({access}) in\n"
                    )));
                }
                let mut call_str = format!("            call '{mod_name}':'{mangled}'(Self");
                for i in 0..method.parameters.len() {
                    use std::fmt::Write;
                    let _ = write!(call_str, ", DispArg{i}");
                }
                call_str.push_str(")\n");
                method_branches.push(Document::String(call_str));
            }
        }

        // Default case: extension fallback, then superclass delegation
        let not_found_branch: Document<'static> = if let Some(ref super_mod) = superclass_mod {
            Document::String(format!(
                "                    call '{super_mod}':'dispatch'(Selector, Args, Self)\n"
            ))
        } else {
            // Root of hierarchy — raise does_not_understand
            let dnu_hint = Self::core_erlang_binary(
                "Check spelling or use 'respondsTo:' to verify method exists",
            );
            docvec![
                "                    let <DnuClass> = call 'beamtalk_primitive':'class_of'(Self) in\n",
                "                    let <DnuErr0> = call 'beamtalk_error':'new'('does_not_understand', DnuClass) in\n",
                "                    let <DnuErr1> = call 'beamtalk_error':'with_selector'(DnuErr0, Selector) in\n",
                format!(
                    "                    let <DnuErr2> = call 'beamtalk_error':'with_hint'(DnuErr1, {dnu_hint}) in\n"
                ),
                "                    call 'beamtalk_error':'raise'(DnuErr2)\n",
            ]
        };

        let doc = docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->\n",
            "    case Selector of\n",
            // class
            "        <'class'> when 'true' ->\n",
            format!("            '{class_name}'\n"),
            // respondsTo:
            "        <'respondsTo:'> when 'true' ->\n",
            "            case Args of\n",
            format!(
                "                <[RtSelector | _]> when 'true' -> call '{mod_name}':'has_method'(RtSelector)\n"
            ),
            "                <_> when 'true' -> 'false'\n",
            "            end\n",
            // asString (conditional)
            as_string_branch,
            // instVarNames
            "        <'instVarNames'> when 'true' ->\n",
            "            []\n",
            // instVarAt:
            "        <'instVarAt:'> when 'true' ->\n",
            format!(
                "            let <IvaErr0> = call 'beamtalk_error':'new'('immutable_value', '{class_name}') in\n"
            ),
            "            let <IvaErr1> = call 'beamtalk_error':'with_selector'(IvaErr0, 'instVarAt:') in\n",
            format!(
                "            let <IvaErr2> = call 'beamtalk_error':'with_hint'(IvaErr1, {iva_hint}) in\n"
            ),
            "            call 'beamtalk_error':'raise'(IvaErr2)\n",
            // instVarAt:put:
            "        <'instVarAt:put:'> when 'true' ->\n",
            format!(
                "            let <ImmErr0> = call 'beamtalk_error':'new'('immutable_value', '{class_name}') in\n"
            ),
            "            let <ImmErr1> = call 'beamtalk_error':'with_selector'(ImmErr0, 'instVarAt:put:') in\n",
            format!(
                "            let <ImmErr2> = call 'beamtalk_error':'with_hint'(ImmErr1, {immutable_hint}) in\n"
            ),
            "            call 'beamtalk_error':'raise'(ImmErr2)\n",
            // perform:
            "        <'perform:'> when 'true' ->\n",
            "            let <PerfSel> = call 'erlang':'hd'(Args) in\n",
            format!("            call '{mod_name}':'dispatch'(PerfSel, [], Self)\n"),
            // perform:withArguments:
            "        <'perform:withArguments:'> when 'true' ->\n",
            "            let <PwaSel> = call 'erlang':'hd'(Args) in\n",
            "            let <PwaArgs> = call 'erlang':'hd'(call 'erlang':'tl'(Args)) in\n",
            format!("            call '{mod_name}':'dispatch'(PwaSel, PwaArgs, Self)\n"),
            // Class-defined method branches
            Document::Vec(method_branches),
            // Default case
            "        <_Other> when 'true' ->\n",
            format!(
                "            case call 'beamtalk_extensions':'lookup'('{class_name}', Selector) of\n"
            ),
            "                <{'ok', ExtFun, _ExtOwner}> when 'true' ->\n",
            "                    apply ExtFun(Args, Self)\n",
            "                <'not_found'> when 'true' ->\n",
            not_found_branch,
            "            end\n",
            "    end\n",
            "\n",
        ];

        Ok(doc)
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
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    fn generate_dispatch_4(&mut self, _class: &ClassDefinition) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();

        let header: Document<'static> =
            Document::Str("'dispatch'/4 = fun (Selector, Args, Self, State) ->\n");

        let body = if class_name == "Object" {
            self.generate_object_dispatch_4(mod_name.as_str())
        } else {
            Self::generate_dispatch_4_wrapper(mod_name.as_str())
        };

        Ok(docvec![header, body, "\n"])
    }

    /// Generates Object's dispatch/4 with state-aware reflection primitives.
    ///
    /// Actor instances inherit Object methods via the hierarchy walk. These
    /// methods need access to the actor's State map for proper reflection
    /// (field access, class name, etc.).
    #[allow(clippy::too_many_lines)] // Object dispatch with reflection primitives
    fn generate_object_dispatch_4(&self, mod_name: &str) -> Document<'static> {
        let a_space_binary = Self::core_erlang_binary("a ");

        // instVarAt: arity error
        let inst_var_at_err = self.arity_error_fragment("'instVarAt:'", 1, "                ");
        // instVarAt:put: arity error (used twice)
        let inst_var_at_put_err =
            self.arity_error_fragment("'instVarAt:put:'", 2, "                    ");
        let inst_var_at_put_err2 =
            self.arity_error_fragment("'instVarAt:put:'", 2, "                ");
        // perform: type error
        let perf_type_err =
            self.type_error_fragment("'perform:'", "selector must be an atom", "                ");
        // perform:withArguments: type errors
        let pwa_type_err_list = self.type_error_fragment(
            "'perform:withArguments:'",
            "arguments must be a list",
            "                        ",
        );
        let pwa_type_err_sel = self.type_error_fragment(
            "'perform:withArguments:'",
            "selector must be an atom",
            "                ",
        );

        docvec![
            "    case Selector of\n",
            // --- class ---
            "        <'class'> when 'true' ->\n",
            "            {'reply', call 'beamtalk_tagged_map':'class_of'(State), State}\n",
            // --- respondsTo: ---
            "        <'respondsTo:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[RtSel4|_]> when call 'erlang':'is_atom'(RtSel4) ->\n",
            "                    let <RtClass4> = call 'beamtalk_tagged_map':'class_of'(State) in\n",
            "                    {'reply', call 'beamtalk_dispatch':'responds_to'(RtSel4, RtClass4), State}\n",
            "                <_RtBadArgs> when 'true' -> {'reply', 'false', State}\n",
            "            end\n",
            // --- instVarNames ---
            "        <'instVarNames'> when 'true' ->\n",
            "            {'reply', call 'beamtalk_reflection':'field_names'(State), State}\n",
            // --- instVarAt: ---
            "        <'instVarAt:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[IvaName4|_]> when 'true' ->\n",
            "                    {'reply', call 'beamtalk_reflection':'read_field'(IvaName4, State), State}\n",
            "                <_IvaBadArgs> when 'true' ->\n",
            inst_var_at_err,
            "\n",
            "            end\n",
            // --- instVarAt:put: ---
            "        <'instVarAt:put:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[IvapName4|IvapRest4]> when 'true' ->\n",
            "                    case IvapRest4 of\n",
            "                        <[IvapVal4|_]> when 'true' ->\n",
            "                            let <IvapTuple4> = call 'beamtalk_reflection':'write_field'(IvapName4, IvapVal4, State) in\n",
            "                            let <IvapResult4> = call 'erlang':'element'(1, IvapTuple4) in\n",
            "                            let <IvapNewState4> = call 'erlang':'element'(2, IvapTuple4) in\n",
            "                            {'reply', IvapResult4, IvapNewState4}\n",
            "                        <_IvapBad2> when 'true' ->\n",
            inst_var_at_put_err,
            "\n",
            "                    end\n",
            "                <_IvapBad1> when 'true' ->\n",
            inst_var_at_put_err2,
            "\n",
            "            end\n",
            // --- printString ---
            "        <'printString'> when 'true' ->\n",
            format!(
                "            let <PsClass4> = call 'beamtalk_tagged_map':'class_of'(State, 'Object') in\n"
            ),
            format!(
                "            let <PsStr4> = call 'erlang':'iolist_to_binary'([{a_space_binary}|[call 'erlang':'atom_to_binary'(PsClass4, 'utf8')]]) in\n"
            ),
            "            {'reply', PsStr4, State}\n",
            // --- inspect ---
            "        <'inspect'> when 'true' ->\n",
            "            {'reply', call 'beamtalk_reflection':'inspect_string'(State), State}\n",
            // --- perform: ---
            "        <'perform:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[PerfSel4|_]> when call 'erlang':'is_atom'(PerfSel4) ->\n",
            "                    let <PerfClass4> = call 'beamtalk_tagged_map':'class_of'(State) in\n",
            "                    let <PerfResult4> = call 'beamtalk_dispatch':'lookup'(PerfSel4, [], Self, State, PerfClass4) in\n",
            "                    case PerfResult4 of\n",
            "                        <{'error', PerfErr4}> when 'true' -> {'error', PerfErr4, State}\n",
            "                        <PerfOther4> when 'true' -> PerfOther4\n",
            "                    end\n",
            "                <_PerfBadArgs> when 'true' ->\n",
            perf_type_err,
            "\n",
            "            end\n",
            // --- perform:withArguments: ---
            "        <'perform:withArguments:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[PwaSel4|PwaRest4]> when call 'erlang':'is_atom'(PwaSel4) ->\n",
            "                    case PwaRest4 of\n",
            "                        <[PwaArgs4|_]> when call 'erlang':'is_list'(PwaArgs4) ->\n",
            "                            let <PwaClass4> = call 'beamtalk_tagged_map':'class_of'(State) in\n",
            "                            let <PwaResult4> = call 'beamtalk_dispatch':'lookup'(PwaSel4, PwaArgs4, Self, State, PwaClass4) in\n",
            "                            case PwaResult4 of\n",
            "                                <{'error', PwaErr4}> when 'true' -> {'error', PwaErr4, State}\n",
            "                                <PwaOther4> when 'true' -> PwaOther4\n",
            "                            end\n",
            "                        <_PwaBadArgList> when 'true' ->\n",
            pwa_type_err_list,
            "\n",
            "                    end\n",
            "                <_PwaBadSel> when 'true' ->\n",
            pwa_type_err_sel,
            "\n",
            "            end\n",
            // --- Default: delegate to dispatch/3 ---
            "        <_OtherSel4> when 'true' ->\n",
            format!("            try call '{mod_name}':'dispatch'(Selector, Args, Self)\n"),
            "            of <D4Result> -> {'reply', D4Result, State}\n",
            "            catch <_D4Type, D4Error, _D4Stack> -> {'error', D4Error, State}\n",
            "    end\n",
        ]
    }

    /// Generates a simple dispatch/4 wrapper that delegates to dispatch/3.
    ///
    /// Uses try-catch to convert exceptions from dispatch/3 (e.g., `does_not_understand`
    /// calling `erlang:error`) into `{error, Error, State}` tuples expected by the
    /// dispatch service.
    fn generate_dispatch_4_wrapper(mod_name: &str) -> Document<'static> {
        docvec![
            format!("    try call '{mod_name}':'dispatch'(Selector, Args, Self)\n"),
            "    of <D4Result> -> {'reply', D4Result, State}\n",
            "    catch <_D4Type, D4Error, _D4Stack> -> {'error', D4Error, State}\n",
        ]
    }

    /// Generates a structured `arity_mismatch` error for dispatch/4 argument validation.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    fn arity_error_fragment(
        &self,
        selector: &str,
        expected_arity: u32,
        indent: &str,
    ) -> Document<'static> {
        let hint = Self::core_erlang_binary(&format!(
            "Expected {expected_arity} argument(s) for {selector}"
        ));
        docvec![
            format!(
                "{indent}let <ArErr0> = call 'beamtalk_error':'new'('arity_mismatch', call 'beamtalk_tagged_map':'class_of'(State, 'Object')) in\n"
            ),
            format!(
                "{indent}let <ArErr1> = call 'beamtalk_error':'with_selector'(ArErr0, {selector}) in\n"
            ),
            format!(
                "{indent}let <ArErr2> = call 'beamtalk_error':'with_hint'(ArErr1, {hint}) in\n"
            ),
            format!("{indent}{{'error', ArErr2, State}}"),
        ]
    }

    /// Generates a structured `type_error` for dispatch/4 argument validation.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    fn type_error_fragment(
        &self,
        selector: &str,
        hint_msg: &str,
        indent: &str,
    ) -> Document<'static> {
        let hint = Self::core_erlang_binary(hint_msg);
        docvec![
            format!(
                "{indent}let <TyErr0> = call 'beamtalk_error':'new'('type_error', call 'beamtalk_tagged_map':'class_of'(State, 'Object')) in\n"
            ),
            format!(
                "{indent}let <TyErr1> = call 'beamtalk_error':'with_selector'(TyErr0, {selector}) in\n"
            ),
            format!(
                "{indent}let <TyErr2> = call 'beamtalk_error':'with_hint'(TyErr1, {hint}) in\n"
            ),
            format!("{indent}{{'error', TyErr2, State}}"),
        ]
    }

    /// Generates the `has_method/1` function for a value type.
    ///
    /// Returns `true` for all known selectors (class-defined + reflection +
    /// extensions + superclass methods via delegation).
    fn generate_primitive_has_method(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let superclass_mod = Self::superclass_module_name(class.superclass_name());

        // BT-447: Class-methods-only classes delegate directly to superclass
        if class.methods.is_empty() && superclass_mod.is_some() {
            return self.generate_minimal_has_method(class);
        }

        // Build list of all known selectors
        let mut selectors: Vec<String> = vec![
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

        let false_branch: Document<'static> = if let Some(ref super_mod) = superclass_mod {
            Document::String(format!(
                "<'false'> when 'true' -> call '{super_mod}':'has_method'(Selector)\n"
            ))
        } else {
            Document::Str("<'false'> when 'true' -> 'false'\n")
        };

        let selectors_list = selectors.join(", ");
        let doc = docvec![
            "'has_method'/1 = fun (Selector) ->\n",
            format!("    case call 'lists':'member'(Selector, [{selectors_list}]) of\n"),
            "        <'true'> when 'true' -> 'true'\n",
            "        <'false'> when 'true' ->\n",
            format!(
                "            case call 'beamtalk_extensions':'has'('{class_name}', Selector) of\n"
            ),
            "                <'true'> when 'true' -> 'true'\n",
            "                ",
            false_branch,
            "            end\n",
            "    end\n",
            "\n",
        ];

        Ok(doc)
    }

    /// BT-447: Generates a minimal `dispatch/3` for classes with no instance methods.
    ///
    /// Handles `class`, `respondsTo:`, `perform:`, and `perform:withArguments:`
    /// locally, then checks extensions and delegates everything else to the
    /// superclass. `perform:` must re-dispatch through this module to preserve
    /// extension lookup and correct error attribution. Skips instVarNames,
    /// instVarAt:, instVarAt:put: since the superclass already handles them.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    fn generate_minimal_dispatch(&mut self, class: &ClassDefinition) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let super_mod = Self::superclass_module_name(class.superclass_name())
            .expect("minimal dispatch requires superclass");

        let doc = docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->\n",
            "    case Selector of\n",
            // class
            "        <'class'> when 'true' ->\n",
            format!("            '{class_name}'\n"),
            // respondsTo:
            "        <'respondsTo:'> when 'true' ->\n",
            "            case Args of\n",
            format!(
                "                <[RtSelector | _]> when 'true' -> call '{mod_name}':'has_method'(RtSelector)\n"
            ),
            "                <_> when 'true' -> 'false'\n",
            "            end\n",
            // perform:
            "        <'perform:'> when 'true' ->\n",
            "            let <PerfSel> = call 'erlang':'hd'(Args) in\n",
            format!("            call '{mod_name}':'dispatch'(PerfSel, [], Self)\n"),
            // perform:withArguments:
            "        <'perform:withArguments:'> when 'true' ->\n",
            "            let <PwaSel> = call 'erlang':'hd'(Args) in\n",
            "            let <PwaArgs> = call 'erlang':'hd'(call 'erlang':'tl'(Args)) in\n",
            format!("            call '{mod_name}':'dispatch'(PwaSel, PwaArgs, Self)\n"),
            // Default: extension check, then superclass delegation
            "        <_Other> when 'true' ->\n",
            format!(
                "            case call 'beamtalk_extensions':'lookup'('{class_name}', Selector) of\n"
            ),
            "                <{'ok', ExtFun, _ExtOwner}> when 'true' ->\n",
            "                    apply ExtFun(Args, Self)\n",
            "                <'not_found'> when 'true' ->\n",
            format!("                    call '{super_mod}':'dispatch'(Selector, Args, Self)\n"),
            "            end\n",
            "    end\n",
            "\n",
        ];

        Ok(doc)
    }

    /// BT-447: Generates a minimal `has_method/1` for classes with no instance methods.
    ///
    /// Checks `class`, `respondsTo:`, `perform:`, `perform:withArguments:`,
    /// then extensions, then delegates to superclass.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    fn generate_minimal_has_method(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let super_mod = Self::superclass_module_name(class.superclass_name())
            .expect("minimal has_method requires superclass");

        let doc = docvec![
            "'has_method'/1 = fun (Selector) ->\n",
            "    case call 'lists':'member'(Selector, ['class', 'respondsTo:', 'perform:', 'perform:withArguments:']) of\n",
            "        <'true'> when 'true' -> 'true'\n",
            "        <'false'> when 'true' ->\n",
            format!(
                "            case call 'beamtalk_extensions':'has'('{class_name}', Selector) of\n"
            ),
            "                <'true'> when 'true' -> 'true'\n",
            format!(
                "                <'false'> when 'true' -> call '{super_mod}':'has_method'(Selector)\n"
            ),
            "            end\n",
            "    end\n",
            "\n",
        ];

        Ok(doc)
    }
}
