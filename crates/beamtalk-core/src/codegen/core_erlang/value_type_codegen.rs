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
use super::spec_codegen;
use super::util::ClassIdentity;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{
    Block, CascadeMessage, ClassDefinition, Expression, MessageSelector, MethodDefinition,
    MethodKind, Module, StringSegment,
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
        // Only check Primary methods — check both instance and class methods
        let has_explicit_new = class.methods.iter().any(|m| {
            m.kind == MethodKind::Primary
                && matches!(&m.selector, MessageSelector::Unary(name) if name.as_str() == "new")
        });
        let has_explicit_class_new = class.class_methods.iter().any(|m| {
            m.kind == MethodKind::Primary
                && matches!(&m.selector, MessageSelector::Unary(name) if name.as_str() == "new")
        });
        let has_explicit_new_with = class
            .methods
            .iter()
            .any(|m| m.kind == MethodKind::Primary && m.selector.name() == "new:")
            || class
                .class_methods
                .iter()
                .any(|m| m.kind == MethodKind::Primary && m.selector.name() == "new:");

        // Collect method exports
        let mut exports = Vec::new();
        // new/0 is always needed: either the default constructor or a delegating one
        if !has_explicit_new {
            exports.push("'new'/0".to_string());
        }
        // new/1 (the keyword new: constructor, or the auto-generated initializer) is
        // suppressed if the class explicitly defines either new: (keyword, which emits
        // 'new'/1 itself) OR unary new (which also emits 'new'/1 via Self parameter).
        if !has_explicit_new_with && !has_explicit_new {
            exports.push("'new'/1".to_string());
        }

        // Add instance method exports (each takes Self as first parameter)
        for method in &class.methods {
            // All methods in a class are instance methods in Beamtalk
            // MethodKind indicates the method type (currently only Primary)
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

        // BT-942: Static reflection metadata for zero-process queries
        exports.push("'__beamtalk_meta'/0".to_string());

        // BT-411: Class method exports
        for method in &class.class_methods {
            if method.kind == MethodKind::Primary {
                let arity = method.parameters.len() + 2; // +2 for ClassSelf + ClassVars
                exports.push(format!("'class_{}'/{arity}", method.selector.name()));
            }
        }

        let mut docs: Vec<Document<'static>> = Vec::new();

        // BT-586: Generate spec attributes from type annotations
        let spec_attrs = spec_codegen::generate_class_specs(class, true);
        let spec_suffix: Document<'static> = spec_codegen::format_spec_attributes(&spec_attrs)
            .map_or(Document::Nil, |s| docvec![",\n     ", s]);

        // BT-745: Build beamtalk_class attribute for dependency-ordered bootstrap
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);

        // BT-845/BT-860: Build beamtalk_source attribute when source_path is set.
        let source_path_attr = self.source_path_attr();
        // BT-940: Build 'file' attribute so BEAM stacktraces show the .bt source file.
        let file_attr = self.file_attr();

        // Module header
        let module_name = self.module_name.clone();
        docs.push(docvec![
            format!("module '{}' [{}]\n", module_name, exports.join(", ")),
            "  attributes ['on_load' = [{'register_class', 0}]",
            beamtalk_class_attr,
            file_attr,
            source_path_attr,
            spec_suffix,
            "]\n",
            "\n",
        ]);

        // Generate new/0 - creates instance with default field values
        if !has_explicit_new {
            if has_explicit_class_new {
                // Class defines `class sealed new => @primitive "..."`, so new/0
                // delegates to the class method's primitive via class_new/2.
                docs.push(self.generate_delegating_new(class)?);
            } else {
                docs.push(self.generate_value_type_new(class)?);
            }
            docs.push(Document::Str("\n"));
        }

        // Generate new/1 - creates instance with initialization arguments.
        // Suppressed when a unary `new` instance method exists (has_explicit_new),
        // because that method compiles to 'new'/1 (Self parameter), which would clash
        // with the auto-generated 'new'/1 (InitArgs parameter).
        if !has_explicit_new_with && !has_explicit_new {
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

        // BT-942: Generate __beamtalk_meta/0 for zero-process reflection
        docs.push(self.generate_meta_function(module)?);

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

    /// Generates `new/0` that delegates to a class method `new` primitive.
    ///
    /// When a class defines `class sealed new => @primitive "new"`, the auto-generated
    /// `new/0` must call through to the primitive implementation rather than creating
    /// an empty tagged map. This ensures `handle_new_compiled` (which calls `Module:new/0`)
    /// uses the correct constructor.
    fn generate_delegating_new(&mut self, class: &ClassDefinition) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();

        // Find the class method `new` and its primitive selector
        let new_method = class
            .class_methods
            .iter()
            .find(|m| {
                m.kind == MethodKind::Primary
                    && matches!(&m.selector, MessageSelector::Unary(name) if name.as_str() == "new")
            })
            .ok_or_else(|| {
                CodeGenError::Internal("Expected class method 'new' not found".to_string())
            })?;

        // Extract the primitive name from the method body; this is required for delegating `new`
        let prim_name = new_method
            .body
            .iter()
            .find_map(|expr| {
                if let crate::ast::Expression::Primitive { name, .. } = expr {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .ok_or_else(|| {
                CodeGenError::Internal(
                    "Expected @primitive in class method 'new' for delegating constructor"
                        .to_string(),
                )
            })?;

        // Generate the BIF call for this primitive; failure is a hard error
        let bif_doc = super::primitives::generate_primitive_bif(&class_name, &prim_name, &[])
            .ok_or_else(|| {
                CodeGenError::Internal(format!(
                    "Failed to generate BIF for primitive '{prim_name}' in class method 'new'"
                ))
            })?;

        Ok(docvec![
            "'new'/0 = fun () ->\n",
            "    ",
            bif_doc,
            "\n",
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
    #[allow(clippy::too_many_lines)]
    fn generate_value_type_method(
        &mut self,
        method: &MethodDefinition,
        _class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let mangled = method.selector.to_erlang_atom();
        let arity = method.parameters.len() + 1; // +1 for Self

        // BT-833: Reset Self-threading version so each method starts with Self (version 0).
        self.reset_self_version();

        // Bind parameters via fresh_var (not to_core_var) so names go through
        // the counter and can't collide with sequencing temp vars — BT-369
        self.push_scope();
        // BT-295: Track method params for @primitive codegen
        self.current_method_params.clear();
        let mut params = vec!["Self".to_string()];
        for param in &method.parameters {
            let var_name = self.fresh_var(&param.name.name);
            self.current_method_params.push(var_name.clone());
            params.push(var_name);
        }

        // BT-754: Detect whether any block argument in this method body contains ^.
        // If so, set up a non-local return token so ^ inside blocks can throw to escape
        // the closure and return from the enclosing method.
        let needs_nlr = method
            .body
            .iter()
            .any(|expr| Self::expr_has_block_nlr(expr, false));

        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.current_nlr_token = Some(token_var.clone());
            Some(token_var)
        } else {
            None
        };

        // Build method body parts
        let mut body_parts: Vec<Document<'static>> = Vec::new();

        // Generate method body expressions.
        // BT-833: Value types now support Self-threading for field assignments.
        // Each `:=` produces a new Self{N} snapshot via maps:put (see generate_field_assignment).
        // Field reads use current_self_var() to reference the latest snapshot.
        // Filter out @expect directives — they are compile-time only and generate no code.
        let body: Vec<&Expression> = method
            .body
            .iter()
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();

        // If filtering leaves no executable expressions, emit a safe fallback to
        // avoid generating an empty Core Erlang function body which would be
        // syntactically invalid (e.g., `fun (...) ->\n\n`).
        if body.is_empty() {
            self.pop_scope();
            self.current_nlr_token = None;
            if let Some(token_var) = nlr_token_var {
                let catch_vars = self.wrap_value_type_body_with_nlr_catch(&token_var);
                // BT-854: NLR methods return {Result, Self} so the catch path
                // (which also returns {NlrVal, NlrState}) produces a consistent shape.
                let final_self = self.current_self_var();
                let body_doc = docvec!["    {'nil', ", Document::String(final_self), "}\n",];
                return Ok(docvec![
                    format!("'{}'/{} = fun ({}) ->\n", mangled, arity, params.join(", ")),
                    catch_vars.format_try_prefix(),
                    body_doc,
                    catch_vars.format_catch_suffix(),
                ]);
            }
            return Ok(docvec![
                format!("'{}'/{} = fun ({}) ->\n", mangled, arity, params.join(", ")),
                docvec!["    nil\n"],
                "\n",
            ]);
        }

        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;

            // Early return (^) at the method body level — emit value and stop generating.
            // Note: ^ inside a block is handled via the NLR throw mechanism (BT-754).
            if let Expression::Return { value, .. } = expr {
                if nlr_token_var.is_some() {
                    // BT-854: NLR methods return {Result, Self{N}} tuple so
                    // the normal and NLR catch paths produce the same shape.
                    let tmp = self.fresh_temp_var("EarlyResult");
                    let val_doc = self.expression_doc(value)?;
                    let final_self = self.current_self_var();
                    body_parts.push(docvec![
                        "    let ",
                        tmp.clone(),
                        " = ",
                        val_doc,
                        " in\n    {",
                        tmp,
                        ", ",
                        Document::String(final_self),
                        "}\n",
                    ]);
                } else {
                    let expr_code = self.capture_expression(value)?;
                    if i > 0 {
                        body_parts.push(Document::Str("    "));
                    }
                    body_parts.push(Document::String(expr_code));
                }
                break;
            }

            if is_last {
                // BT-900: When the last expression is a field assignment, the method
                // should return the updated Self (not the assigned value). This allows
                // setUp to return the modified instance. The field assignment expression
                // itself still evaluates to the assigned value (preserving `(self.x := 5) + 1`
                // semantics), but we generate it as a non-last open let chain and then
                // close with Self{N} as the method return value.
                if Self::is_field_assignment(expr) {
                    let doc = self.generate_vt_field_assignment_open(expr)?;
                    body_parts.push(doc);
                    let final_self = self.current_self_var();
                    if nlr_token_var.is_some() {
                        // BT-854: NLR methods return {Self{N}, Self{N}} tuple.
                        // The result IS the updated Self, and the state IS the updated Self.
                        body_parts.push(docvec![
                            "    {",
                            final_self.clone(),
                            ", ",
                            Document::String(final_self),
                            "}\n",
                        ]);
                    } else {
                        body_parts.push(docvec!["    ", Document::String(final_self), "\n"]);
                    }
                } else if nlr_token_var.is_some() {
                    // BT-854: NLR methods return {Result, Self{N}} tuple so
                    // the normal and NLR catch paths produce the same shape.
                    let tmp = self.fresh_temp_var("BodyResult");
                    let val_doc = self.expression_doc(expr)?;
                    let final_self = self.current_self_var();
                    body_parts.push(docvec![
                        "    let ",
                        tmp.clone(),
                        " = ",
                        val_doc,
                        " in\n    {",
                        tmp,
                        ", ",
                        Document::String(final_self),
                        "}\n",
                    ]);
                } else {
                    let expr_code = self.capture_expression(expr)?;
                    if i > 0 {
                        body_parts.push(Document::Str("    "));
                    }
                    body_parts.push(Document::String(expr_code));
                }
            } else if Self::is_field_assignment(expr) {
                // BT-833: Value type field assignment (non-last) — generate an open
                // Self-threading let chain so Self{N} stays in scope for subsequent exprs.
                let doc = self.generate_vt_field_assignment_open(expr)?;
                body_parts.push(doc);
            } else if Self::is_local_var_assignment(expr) {
                // BT-744: Local variable assignment — create a proper Core Erlang
                // let binding so the variable is accessible in subsequent expressions.
                if let Expression::Assignment { target, value, .. } = expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = &id.name;
                        let core_var = self
                            .lookup_var(var_name)
                            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                        let val_doc = self.expression_doc(value)?;
                        self.bind_var(var_name, &core_var);
                        body_parts.push(docvec!["    let ", core_var, " = ", val_doc, " in\n",]);
                    }
                }
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
        self.current_nlr_token = None;

        // BT-940: Annotate the `fun` expression (not just the body) with source line.
        // Annotating only the body would create invalid double-annotation when the body
        // is itself a single annotated MessageSend expression: `( ( e -| [...] ) -| [...] )`.
        let line_annotation = self.span_to_line(method.span);

        if let Some(token_var) = nlr_token_var {
            // BT-754/BT-764: Wrap the method body in try/catch to catch non-local returns
            // thrown by ^ inside block closures. Uses the shared value type NLR helper.
            let catch_vars = self.wrap_value_type_body_with_nlr_catch(&token_var);

            let body_doc = Document::Vec(body_parts);
            let fun_doc = docvec![
                format!("fun ({}) ->\n", params.join(", ")),
                catch_vars.format_try_prefix(),
                body_doc,
                catch_vars.format_catch_suffix(),
            ];
            let fun_doc = if let Some(line_num) = line_annotation {
                Self::annotate_with_line(fun_doc, line_num)
            } else {
                fun_doc
            };
            Ok(docvec![
                format!("'{}'/{} = ", mangled, arity),
                fun_doc,
                "\n",
            ])
        } else {
            let body_doc = Document::Vec(body_parts);
            let fun_doc = docvec![format!("fun ({}) ->\n", params.join(", ")), body_doc];
            let fun_doc = if let Some(line_num) = line_annotation {
                Self::annotate_with_line(fun_doc, line_num)
            } else {
                fun_doc
            };
            Ok(docvec![
                format!("'{}'/{} = ", mangled, arity),
                fun_doc,
                "\n",
            ])
        }
    }

    /// BT-833: Generates an open Self-threading let chain for a non-last field assignment.
    ///
    /// For `self.field := value`, produces:
    /// ```erlang
    ///     let _Val = <value> in let Self{N} = call 'maps':'put'('field', _Val, Self{N-1}) in
    /// ```
    ///
    /// The trailing `in` leaves the let chain open so subsequent `body_parts` expressions
    /// flow as the continuation, keeping `Self{N}` in scope.
    fn generate_vt_field_assignment_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");
                // Capture current Self BEFORE generating value so RHS field reads
                // (e.g., `self.x := self.x + 1`) see the current snapshot.
                let current_self = self.current_self_var();
                let val_doc = self.expression_doc(value)?;
                let new_self = self.next_self_var();
                return Ok(docvec![
                    "    ",
                    format!("let {val_var} = "),
                    val_doc,
                    format!(
                        " in let {new_self} = call 'maps':'put'('{}', {val_var}, {current_self}) in\n",
                        field.name
                    ),
                ]);
            }
        }
        // Fallback: sequence as a side-effecting expression
        let tmp_var = self.fresh_temp_var("seq");
        let expr_code = self.capture_expression(expr)?;
        Ok(Document::String(format!(
            "    let {tmp_var} = {expr_code} in\n"
        )))
    }

    /// Returns `true` if `expr` contains a `^` (Return) that is nested inside
    /// a block literal at any depth.
    ///
    /// Used by `generate_value_type_method` to decide whether to emit a
    /// non-local return try/catch wrapper (BT-754).
    ///
    /// * `inside_block` — whether we are already inside at least one block literal.
    ///   At the method-body level the caller passes `false`; `generate_block` sets
    ///   it to `true` for the block's body expressions.
    ///
    /// Each `Expression` variant is matched explicitly (no wildcard) so the Rust
    /// compiler enforces exhaustiveness: adding a new AST node forces an update here.
    pub(in crate::codegen::core_erlang) fn expr_has_block_nlr(
        expr: &Expression,
        inside_block: bool,
    ) -> bool {
        match expr {
            Expression::Return { value, .. } => {
                // ^ at method-body level is NOT an NLR — it is handled by the
                // early-return loop in generate_value_type_method.
                // ^ inside a block IS an NLR that needs the throw mechanism.
                inside_block || Self::expr_has_block_nlr(value, inside_block)
            }
            Expression::Block(block) => {
                // Entering a block: any ^ from here downward is a non-local return.
                Self::block_has_nlr(block)
            }
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                Self::expr_has_block_nlr(receiver, inside_block)
                    || arguments
                        .iter()
                        .any(|a| Self::expr_has_block_nlr(a, inside_block))
            }
            Expression::Assignment { target, value, .. } => {
                Self::expr_has_block_nlr(target, inside_block)
                    || Self::expr_has_block_nlr(value, inside_block)
            }
            Expression::Parenthesized { expression, .. } => {
                Self::expr_has_block_nlr(expression, inside_block)
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                Self::expr_has_block_nlr(receiver, inside_block)
                    || messages.iter().any(|m: &CascadeMessage| {
                        m.arguments
                            .iter()
                            .any(|a| Self::expr_has_block_nlr(a, inside_block))
                    })
            }
            Expression::FieldAccess { receiver, .. } => {
                Self::expr_has_block_nlr(receiver, inside_block)
            }
            Expression::Match { value, arms, .. } => {
                Self::expr_has_block_nlr(value, inside_block)
                    || arms.iter().any(|arm| {
                        arm.guard
                            .as_ref()
                            .is_some_and(|g| Self::expr_has_block_nlr(g, inside_block))
                            || Self::expr_has_block_nlr(&arm.body, inside_block)
                    })
            }
            Expression::MapLiteral { pairs, .. } => pairs.iter().any(|pair| {
                Self::expr_has_block_nlr(&pair.key, inside_block)
                    || Self::expr_has_block_nlr(&pair.value, inside_block)
            }),
            Expression::ListLiteral { elements, tail, .. } => {
                elements
                    .iter()
                    .any(|e| Self::expr_has_block_nlr(e, inside_block))
                    || tail
                        .as_ref()
                        .is_some_and(|t| Self::expr_has_block_nlr(t, inside_block))
            }
            Expression::ArrayLiteral { elements, .. } => elements
                .iter()
                .any(|e| Self::expr_has_block_nlr(e, inside_block)),
            Expression::StringInterpolation { segments, .. } => segments.iter().any(|s| match s {
                StringSegment::Literal(_) => false,
                StringSegment::Interpolation(e) => Self::expr_has_block_nlr(e, inside_block),
            }),
            // True leaf expressions — cannot contain sub-expressions.
            Expression::Literal(..)
            | Expression::Identifier(..)
            | Expression::ClassReference { .. }
            | Expression::Super(..)
            | Expression::Primitive { .. }
            | Expression::Error { .. }
            | Expression::ExpectDirective { .. } => false,
        }
    }

    /// Returns `true` if the block (or any expression inside it, recursively)
    /// contains a `^` (Return) expression.
    fn block_has_nlr(block: &Block) -> bool {
        block.body.iter().any(|e| Self::expr_has_block_nlr(e, true))
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
                | "Erlang"
                | "ErlangModule"
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

    /// Computes the compiled module name for a class (ADR 0016 / ADR 0026 / BT-794).
    ///
    /// Resolution order:
    /// 1. `class_module_index` — explicit mapping built during two-pass compilation.
    ///    This correctly handles classes in package subdirectories (e.g. `SchemeEnv`
    ///    → `bt@sicp_example@scheme@env`).
    /// 2. Stdlib classes → `bt@stdlib@{snake_case}`
    /// 3. User-defined classes in package mode → `bt@{package}@{snake_case}`
    ///    (package prefix extracted from `self.module_name`)
    /// 4. User-defined classes without package context → `bt@{snake_case}` (legacy)
    pub fn compiled_module_name(&self, class_name: &str) -> String {
        if let Some(module) = self.class_module_index.get(class_name) {
            return module.clone();
        }
        let snake = super::util::to_module_name(class_name);
        if Self::is_known_stdlib_type(class_name) {
            format!("bt@stdlib@{snake}")
        } else if let Some(prefix) = super::util::user_package_prefix(&self.module_name) {
            format!("{prefix}{snake}")
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
    /// For user-defined classes in package mode, uses `bt@{package}@{snake_case}`
    /// prefix (BT-794).
    fn superclass_module_name(&self, superclass: &str) -> Option<String> {
        if superclass == "ProtoObject" {
            return None;
        }
        Some(self.compiled_module_name(superclass))
    }

    /// Encode a string as a Core Erlang binary literal.
    /// Delegates to the shared UTF-8 binary literal helper.
    fn core_erlang_binary(s: &str) -> String {
        Self::binary_string_literal(s)
    }

    /// Generates the `dispatch/3` function for a value type.
    ///
    /// This routes selectors to individual method functions, provides reflection
    /// methods (class, respondsTo:, fieldNames, fieldAt:, fieldAt:put:,
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
        let superclass_mod = self.superclass_module_name(class.superclass_name());

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

        // fieldAt: error
        let iva_hint =
            Self::core_erlang_binary(&format!("{class_name}s are immutable and have no fields."));

        // fieldAt:put: error
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

            // BT-854: Methods with NLR return {Result, State} tuple — unwrap via case.
            let has_nlr = method
                .body
                .iter()
                .any(|expr| Self::expr_has_block_nlr(expr, false));

            // Build the method call arguments: (Self) or (Self, DispArg0, DispArg1, ...)
            if !method.parameters.is_empty() {
                // Extract args from Args list: hd(Args), hd(tl(Args)), ...
                for (i, _param) in method.parameters.iter().enumerate() {
                    let arg_var = format!("DispArg{i}");
                    let mut access = "Args".to_string();
                    for _ in 0..i {
                        access = format!("call 'erlang':'tl'({access})");
                    }
                    method_branches.push(docvec![
                        "            let <",
                        arg_var,
                        "> = call 'erlang':'hd'(",
                        Document::String(access),
                        ") in\n",
                    ]);
                }
            }

            // Build the call expression: call 'mod':'method'(Self, DispArg0, ...)
            let mut call_args: Vec<Document<'static>> = vec![Document::Str("Self")];
            for i in 0..method.parameters.len() {
                call_args.push(Document::String(format!(", DispArg{i}")));
            }
            let call_doc = docvec![
                "call '",
                mod_name.clone(),
                "':'",
                mangled.clone(),
                "'(",
                Document::Vec(call_args),
                ")",
            ];

            if has_nlr {
                // BT-854: Unwrap {Result, State} tuple from NLR-capable method
                method_branches.push(docvec![
                    "            case ",
                    call_doc,
                    " of <{DispR, _DispS}> when 'true' -> DispR end\n",
                ]);
            } else {
                method_branches.push(docvec!["            ", call_doc, "\n"]);
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
            // fieldNames
            "        <'fieldNames'> when 'true' ->\n",
            "            []\n",
            // fieldAt:
            "        <'fieldAt:'> when 'true' ->\n",
            format!(
                "            let <IvaErr0> = call 'beamtalk_error':'new'('immutable_value', '{class_name}') in\n"
            ),
            "            let <IvaErr1> = call 'beamtalk_error':'with_selector'(IvaErr0, 'fieldAt:') in\n",
            format!(
                "            let <IvaErr2> = call 'beamtalk_error':'with_hint'(IvaErr1, {iva_hint}) in\n"
            ),
            "            call 'beamtalk_error':'raise'(IvaErr2)\n",
            // fieldAt:put:
            "        <'fieldAt:put:'> when 'true' ->\n",
            format!(
                "            let <ImmErr0> = call 'beamtalk_error':'new'('immutable_value', '{class_name}') in\n"
            ),
            "            let <ImmErr1> = call 'beamtalk_error':'with_selector'(ImmErr0, 'fieldAt:put:') in\n",
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
    /// for reflection primitives (fieldAt:, printString, etc.) that need
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

        // fieldAt: arity error
        let inst_var_at_err = self.arity_error_fragment("'fieldAt:'", 1, "                ");
        // fieldAt:put: arity error (used twice)
        let inst_var_at_put_err =
            self.arity_error_fragment("'fieldAt:put:'", 2, "                    ");
        let inst_var_at_put_err2 =
            self.arity_error_fragment("'fieldAt:put:'", 2, "                ");
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
            // --- fieldNames ---
            "        <'fieldNames'> when 'true' ->\n",
            "            {'reply', call 'beamtalk_reflection':'field_names'(State), State}\n",
            // --- fieldAt: ---
            "        <'fieldAt:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[IvaName4|_]> when 'true' ->\n",
            "                    {'reply', call 'beamtalk_reflection':'read_field'(IvaName4, State), State}\n",
            "                <_IvaBadArgs> when 'true' ->\n",
            inst_var_at_err,
            "\n",
            "            end\n",
            // --- fieldAt:put: ---
            "        <'fieldAt:put:'> when 'true' ->\n",
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
        let superclass_mod = self.superclass_module_name(class.superclass_name());

        // BT-447: Class-methods-only classes delegate directly to superclass
        if class.methods.is_empty() && superclass_mod.is_some() {
            return self.generate_minimal_has_method(class);
        }

        // Build list of all known selectors
        let mut selectors: Vec<String> = vec![
            "'class'".to_string(),
            "'respondsTo:'".to_string(),
            "'fieldNames'".to_string(),
            "'fieldAt:'".to_string(),
            "'fieldAt:put:'".to_string(),
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
    /// extension lookup and correct error attribution. Skips fieldNames,
    /// fieldAt:, fieldAt:put: since the superclass already handles them.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    fn generate_minimal_dispatch(&mut self, class: &ClassDefinition) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let super_mod = self
            .superclass_module_name(class.superclass_name())
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
        let super_mod = self
            .superclass_module_name(class.superclass_name())
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
