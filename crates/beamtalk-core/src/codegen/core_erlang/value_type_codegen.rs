// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value type module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for value type classes — plain Erlang
//! terms (maps) with no process. They are created with `new` and `new:`,
//! not `spawn`, and methods are synchronous functions operating on maps.

use std::fmt::Write as FmtWrite;

use super::document::{Document, concat};
use super::intrinsics::validate_block_arity_exact;
use super::spec_codegen;
use super::util::ClassIdentity;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{
    ClassDefinition, ClassKind, Expression, MessageSelector, MethodDefinition, MethodKind, Module,
};
use crate::docvec;

/// Classification of how a value-type method body expression should be handled
/// for Self-threading.  Produced by [`CoreErlangGenerator::classify_vt_body_expr`]
/// and consumed by the unified body loop in [`CoreErlangGenerator::generate_value_type_method`].
enum VtBodyExprKind {
    /// `^ value` — early return from method.
    EarlyReturn,
    /// `self.field := value` — direct field assignment (Self-threading).
    FieldAssignment,
    /// `var := expr` — local variable assignment.
    LocalAssignment,
    /// `{a, b} := expr` — destructure assignment.
    DestructureAssignment,
    /// Non-last `do:` loop that mutates captured outer locals (BT-1053).
    DoWithLocalThreading,
    /// Non-last `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` with local mutations (BT-1392).
    ConditionalWithLocalThreading,
    /// Non-last block value with captured mutations (BT-1213).
    BlockWithCapturedMutations(Vec<String>),
    /// Regular expression with no special Self-threading needs.
    Pure,
}

/// Auto-generated slot methods for `Value subclass:` classes (ADR 0042).
///
/// Only populated when `class_kind == ClassKind::Value`.
/// Skips any slot whose getter/setter selector the user has already defined.
pub(super) struct AutoSlotMethods {
    /// Field names for which a getter `fieldName/1` is auto-generated.
    pub(super) getters: Vec<String>,
    /// Field names for which a `withFieldName:/2` setter is auto-generated.
    pub(super) setters: Vec<String>,
    /// Keyword constructor selector (e.g., `"x:y:"` for a Point with slots x, y),
    /// `None` if the class has no slots or the user already defined it.
    pub(super) keyword_constructor: Option<String>,
}

impl AutoSlotMethods {
    /// Computes the `with*:` selector name for a slot.
    ///
    /// Capitalises the first letter of the field name and prepends `"with"`:
    /// - `"x"` → `"withX:"`
    /// - `"firstName"` → `"withFirstName:"`
    pub(super) fn with_star_selector(field_name: &str) -> String {
        let mut chars = field_name.chars();
        match chars.next() {
            None => "with:".to_string(),
            Some(first) => {
                let cap: String = first.to_uppercase().collect();
                format!("with{}{}:", cap, chars.as_str())
            }
        }
    }

    /// Returns the keyword constructor selector for the given slot names.
    ///
    /// E.g. `["x", "y"]` → `"x:y:"`
    fn keyword_selector(slots: &[String]) -> String {
        let mut sel = String::new();
        for s in slots {
            sel.push_str(s);
            sel.push(':');
        }
        sel
    }
}

/// Computes which slot methods to auto-generate for a `Value subclass:` class.
///
/// Returns `None` for `ClassKind::Object` and `ClassKind::Actor` — only
/// `ClassKind::Value` classes get auto-generated slot accessors.
pub(super) fn compute_auto_slot_methods(class: &ClassDefinition) -> Option<AutoSlotMethods> {
    if class.class_kind != ClassKind::Value {
        return None;
    }

    // Collect selectors the user has already explicitly defined
    let user_instance_selectors: std::collections::HashSet<String> = class
        .methods
        .iter()
        .map(|m| m.selector.name().to_string())
        .collect();
    let user_class_selectors: std::collections::HashSet<String> = class
        .class_methods
        .iter()
        .map(|m| m.selector.name().to_string())
        .collect();

    let getters: Vec<String> = class
        .state
        .iter()
        .filter(|s| !user_instance_selectors.contains(s.name.name.as_str()))
        .map(|s| s.name.name.to_string())
        .collect();

    let setters: Vec<String> = class
        .state
        .iter()
        .filter(|s| {
            let with_name = AutoSlotMethods::with_star_selector(s.name.name.as_str());
            !user_instance_selectors.contains(&with_name)
        })
        .map(|s| s.name.name.to_string())
        .collect();

    let keyword_constructor = if class.state.is_empty() {
        None
    } else {
        let all_slots: Vec<String> = class
            .state
            .iter()
            .map(|s| s.name.name.to_string())
            .collect();
        let sel = AutoSlotMethods::keyword_selector(&all_slots);
        if user_class_selectors.contains(&sel) {
            None
        } else {
            Some(sel)
        }
    };

    Some(AutoSlotMethods {
        getters,
        setters,
        keyword_constructor,
    })
}

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
        self.set_class_identity(Some(ClassIdentity::new(&class.name.name)));

        // Check if the class explicitly defines new/new: methods
        // (e.g., Object.bt defines `new => @primitive basicNew`)
        // If so, skip auto-generating constructors to avoid duplicate definitions
        // Only check Primary methods — check both instance and class methods
        let cf = self.semantic_facts.class_facts(&class.name.name);
        let has_explicit_new = cf
            .and_then(|cf| cf.instance_method_index("new"))
            .is_some_and(|i| class.methods[i].kind == MethodKind::Primary);
        let has_explicit_class_new = cf
            .and_then(|cf| cf.class_method_index("new"))
            .is_some_and(|i| class.class_methods[i].kind == MethodKind::Primary);
        let has_explicit_new_with = cf
            .and_then(|cf| cf.instance_method_index("new:"))
            .is_some_and(|i| class.methods[i].kind == MethodKind::Primary)
            || cf
                .and_then(|cf| cf.class_method_index("new:"))
                .is_some_and(|i| class.class_methods[i].kind == MethodKind::Primary);

        // BT-923: Compute auto-generated slot methods for `Value subclass:` classes.
        // This is `None` for `ClassKind::Object` and `ClassKind::Actor`.
        let auto_methods = compute_auto_slot_methods(class);

        // Collect all function exports for the module header.
        let exports = Self::build_value_type_exports(
            class,
            auto_methods.as_ref(),
            has_explicit_new,
            has_explicit_new_with,
        );

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Module header with all attributes
        docs.push(self.build_value_type_module_header(module, class, &exports));

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

        // BT-923: Auto-generate getter and with*: setter functions for Value subclass:
        let class_name_for_kw = self.class_name().clone();
        if let Some(ref auto) = auto_methods {
            for field in &auto.getters {
                docs.push(Self::generate_slot_getter(field));
                docs.push(Document::Str("\n"));
            }
            for field in &auto.setters {
                docs.push(Self::generate_slot_setter(field));
                docs.push(Document::Str("\n"));
            }
        }

        // Generate dispatch/3 and has_method/1 for all value types
        // (superclass delegation chain — same pattern as actors)
        docs.push(self.generate_primitive_dispatch(class, auto_methods.as_ref())?);
        docs.push(Document::Str("\n"));
        // BT-446: Generate dispatch/4 for actor hierarchy walk.
        // The dispatch service only calls modules that export dispatch/4.
        docs.push(self.generate_dispatch_4(class)?);
        docs.push(Document::Str("\n"));
        docs.push(self.generate_primitive_has_method(class, auto_methods.as_ref())?);
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

        // BT-923: Auto-generate keyword constructor for Value subclass:
        if let Some(ref auto) = auto_methods {
            if let Some(ref kw_sel) = auto.keyword_constructor {
                let slots: Vec<String> = class
                    .state
                    .iter()
                    .map(|s| s.name.name.to_string())
                    .collect();
                docs.push(Self::generate_keyword_constructor_fn(
                    &class_name_for_kw,
                    kw_sel,
                    &slots,
                ));
                docs.push(Document::Str("\n"));
            }
        }

        // BT-246: Register value type class with the class system for dynamic dispatch
        // Value types never need supervisionSpec synthesis (they are not actors).
        docs.push(self.generate_register_class(module, false)?);
        docs.push(Document::Str("\n"));

        // BT-942: Generate __beamtalk_meta/0 for zero-process reflection
        docs.push(self.generate_meta_function(module, false)?);

        // Module end
        docs.push(Document::Str("end\n"));

        Ok(Document::Vec(docs))
    }

    /// Builds the export list for a value type module header.
    ///
    /// Includes constructors (`new/0`, `new/1`), instance methods, auto-generated
    /// slot methods (`dispatch/3`, `has_method/1`, `superclass/0`, `register_class/0`), and class methods.
    fn build_value_type_exports(
        class: &ClassDefinition,
        auto_methods: Option<&AutoSlotMethods>,
        has_explicit_new: bool,
        has_explicit_new_with: bool,
    ) -> Vec<String> {
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
            let arity = method.parameters.len() + 1; // +1 for Self parameter
            let mangled = method.selector.to_erlang_atom();
            exports.push(format!("'{mangled}'/{arity}"));
        }

        // BT-923: Auto-generated getter and with*: setter exports for Value subclass:
        if let Some(auto) = auto_methods {
            for field in &auto.getters {
                exports.push(format!("'{field}'/1"));
            }
            for field in &auto.setters {
                let with_sel = AutoSlotMethods::with_star_selector(field);
                exports.push(format!("'{with_sel}'/2"));
            }
            if let Some(ref kw_sel) = auto.keyword_constructor {
                let num_slots = class.state.len();
                let arity = num_slots + 2; // ClassSelf + ClassVars + N slot args
                // BT-1408: Hash long keyword constructor atoms to stay within Erlang's 255-char atom limit.
                let safe_fn = super::selector_mangler::safe_class_method_fn_name(kw_sel);
                exports.push(format!("'{safe_fn}'/{arity}"));
            }
        }

        // All value types export dispatch/3 and has_method/1
        // for runtime dispatch via superclass delegation chain
        exports.push("'dispatch'/3".to_string());
        // BT-446: All value types also export dispatch/4 for actor hierarchy walk.
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

        exports
    }

    /// Builds the Core Erlang `module 'Name' [...] attributes [...]` header document.
    ///
    /// Assembles spec attributes, type aliases, and source-path metadata into a single
    /// module-header document, including `on_load` and `beamtalk_class` annotations.
    fn build_value_type_module_header(
        &mut self,
        module: &Module,
        class: &ClassDefinition,
        exports: &[String],
    ) -> Document<'static> {
        // BT-586: Generate spec attributes from type annotations
        let spec_attrs = spec_codegen::generate_class_specs(class, true);
        let spec_suffix: Document<'static> = spec_codegen::format_spec_attributes(&spec_attrs)
            .map_or(Document::Nil, |s| docvec![",\n     ", s]);

        // BT-1156: Generate -type t() alias for Value classes with state: declarations.
        let class_name_for_type = self.class_name();
        let type_alias_opt = spec_codegen::generate_type_alias(class, &class_name_for_type);
        let export_type_suffix: Document<'static> = if type_alias_opt.is_some() {
            Document::Str(",\n     'export_type' = [{'t', 0}]")
        } else {
            Document::Nil
        };
        let type_alias_suffix: Document<'static> =
            type_alias_opt.map_or(Document::Nil, |s| docvec![",\n     ", s]);

        // BT-745: Build beamtalk_class attribute for dependency-ordered bootstrap
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);
        // BT-845/BT-860/BT-940: Source-path and file attributes for stacktraces
        let source_path_attr = self.source_path_attr();
        let file_attr = self.file_attr();

        let module_name = self.module_name.clone();
        docvec![
            format!("module '{}' [{}]\n", module_name, exports.join(", ")),
            "  attributes ['on_load' = [{'register_class', 0}]",
            beamtalk_class_attr,
            file_attr,
            source_path_attr,
            type_alias_suffix,
            export_type_suffix,
            spec_suffix,
            "]\n",
            "\n",
        ]
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

    /// Generates `new/0` that delegates to a class method `new`.
    ///
    /// When a class defines `class sealed new => <body>`, the auto-generated
    /// `new/0` must call through to the implementation rather than creating
    /// an empty tagged map. This ensures `handle_new_compiled` (which calls
    /// `Module:new/0`) uses the correct constructor.
    ///
    /// Two cases:
    /// 1. Body is `@primitive "selector"` — inline the BIF call directly.
    /// 2. Body is an FFI call (ADR 0055) — delegate to the compiled
    ///    `class_new/2` function with `undefined` for `ClassSelf` and `ClassVars`.
    fn generate_delegating_new(&mut self, class: &ClassDefinition) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();

        // Find the class method `new`
        let new_method = self
            .semantic_facts
            .class_facts(&class_name)
            .and_then(|cf| cf.class_method_index("new"))
            .and_then(|i| {
                let m = &class.class_methods[i];
                (m.kind == MethodKind::Primary).then_some(m)
            })
            .ok_or_else(|| {
                CodeGenError::Internal("Expected class method 'new' not found".to_string())
            })?;

        // Check whether the method body is a @primitive annotation.
        let prim_name = new_method.body.iter().find_map(|stmt| {
            if let crate::ast::Expression::Primitive { name, .. } = &stmt.expression {
                Some(name.clone())
            } else {
                None
            }
        });

        if let Some(prim_name) = prim_name {
            // Original path: inline the BIF call for the @primitive.
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
        } else {
            // ADR 0055 FFI path: delegate to the compiled class_new/2 function.
            // class_new/2 is generated from the `class sealed new => (Erlang M) fn`
            // method body. `ClassSelf` and `ClassVars` are unused for Object subclasses.
            let module_name = self.module_name.clone();
            Ok(docvec![
                "'new'/0 = fun () ->\n",
                "    call '",
                Document::String(module_name),
                "':'class_new'('undefined', 'undefined')\n",
                "\n",
            ])
        }
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

    // ──────────────────────────────────────────────────────────────────────────
    // BT-923: Auto-generated slot methods for `Value subclass:` classes
    // ──────────────────────────────────────────────────────────────────────────

    /// Generates an auto-getter function for a single slot (BT-923).
    ///
    /// ```erlang
    /// 'x'/1 = fun (Self) -> call 'maps':'get'('x', Self)
    /// ```
    fn generate_slot_getter(field_name: &str) -> Document<'static> {
        docvec![
            "'",
            Document::String(field_name.to_string()),
            "'/1 = fun (Self) ->\n",
            "    call 'maps':'get'('",
            Document::String(field_name.to_string()),
            "', Self)\n",
            "\n",
        ]
    }

    /// Generates an auto `with*:` functional setter for a single slot (BT-923).
    ///
    /// ```erlang
    /// 'withX:'/2 = fun (Self, NewVal) -> call 'maps':'put'('x', NewVal, Self)
    /// ```
    fn generate_slot_setter(field_name: &str) -> Document<'static> {
        let with_sel = AutoSlotMethods::with_star_selector(field_name);
        docvec![
            "'",
            Document::String(with_sel),
            "'/2 = fun (Self, NewVal) ->\n",
            "    call 'maps':'put'('",
            Document::String(field_name.to_string()),
            "', NewVal, Self)\n",
            "\n",
        ]
    }

    /// Generates the all-fields keyword constructor class method (BT-923).
    ///
    /// ```erlang
    /// 'class_x:y:'/4 = fun (ClassSelf, ClassVars, X, Y) ->
    ///     ~{'$beamtalk_class' => 'Point', 'x' => X, 'y' => Y}~
    /// ```
    fn generate_keyword_constructor_fn(
        class_name: &str,
        kw_selector: &str,
        slots: &[String],
    ) -> Document<'static> {
        let arity = slots.len() + 2; // _ClassSelf + _ClassVars + N slot args

        // Extra slot parameters appended after "_ClassSelf, _ClassVars": ", SlotArg0", ...
        let slot_param_docs: Vec<Document<'static>> = slots
            .iter()
            .enumerate()
            .flat_map(|(i, _)| [Document::Str(", "), Document::String(format!("SlotArg{i}"))])
            .collect();

        // Map literal body: '$beamtalk_class' => 'ClassName', 'x' => SlotArg0, ...
        let mut map_field_docs: Vec<Document<'static>> = vec![
            Document::Str("'$beamtalk_class' => '"),
            Document::String(class_name.to_string()),
            Document::Str("'"),
        ];
        for (i, slot_name) in slots.iter().enumerate() {
            map_field_docs.extend([
                Document::Str(", '"),
                Document::String(slot_name.clone()),
                Document::Str("' => "),
                Document::String(format!("SlotArg{i}")),
            ]);
        }

        // BT-1408: Hash long keyword constructor atoms to stay within Erlang's
        // 255-char atom limit.
        let safe_fn_name = super::selector_mangler::safe_class_method_fn_name(kw_selector);

        docvec![
            "'",
            Document::String(safe_fn_name),
            "'/",
            Document::String(arity.to_string()),
            " = fun (_ClassSelf, _ClassVars",
            concat(slot_param_docs),
            ") ->\n",
            "    ~{",
            concat(map_field_docs),
            "}~\n",
            "\n",
        ]
    }

    /// Generates dispatch arms for auto-generated getter and `with*:` setter methods (BT-923).
    ///
    /// Each arm follows the same pattern as user-defined methods in `generate_primitive_dispatch`.
    fn generate_auto_slot_dispatch_arms(
        mod_name: &str,
        auto: &AutoSlotMethods,
    ) -> Vec<Document<'static>> {
        let mut arms: Vec<Document<'static>> = Vec::new();

        for field in &auto.getters {
            arms.push(docvec![
                "        <'",
                Document::String(field.clone()),
                "'> when 'true' ->\n",
            ]);
            arms.push(docvec![
                "            call '",
                Document::String(mod_name.to_string()),
                "':'",
                Document::String(field.clone()),
                "'(Self)\n",
            ]);
        }

        for field in &auto.setters {
            let with_sel = AutoSlotMethods::with_star_selector(field);
            arms.push(docvec![
                "        <'",
                Document::String(with_sel.clone()),
                "'> when 'true' ->\n",
            ]);
            arms.push(Document::Str(
                "            let <DispArg0> = call 'erlang':'hd'(Args) in\n",
            ));
            arms.push(docvec![
                "            call '",
                Document::String(mod_name.to_string()),
                "':'",
                Document::String(with_sel),
                "'(Self, DispArg0)\n",
            ]);
        }

        arms
    }

    // ── BT-1445: Unified value-type method body classification ─────────

    /// Classify a value-type body expression for Self-threading dispatch.
    ///
    /// The order of checks matters: more specific patterns (e.g. field assignment,
    /// `do:` with local threading) must come before general ones (e.g. pure).
    fn classify_vt_body_expr(&self, expr: &Expression) -> VtBodyExprKind {
        if matches!(expr, Expression::Return { .. }) {
            return VtBodyExprKind::EarlyReturn;
        }
        if Self::is_field_assignment(expr) {
            return VtBodyExprKind::FieldAssignment;
        }
        if Self::is_local_var_assignment(expr) {
            return VtBodyExprKind::LocalAssignment;
        }
        if matches!(expr, Expression::DestructureAssignment { .. }) {
            return VtBodyExprKind::DestructureAssignment;
        }
        if self.is_do_with_vt_local_threading(expr) {
            return VtBodyExprKind::DoWithLocalThreading;
        }
        if self.is_conditional_with_vt_local_threading(expr) {
            return VtBodyExprKind::ConditionalWithLocalThreading;
        }
        if let Some(mutations) = Self::inline_block_captured_mutations(expr) {
            return VtBodyExprKind::BlockWithCapturedMutations(mutations);
        }
        VtBodyExprKind::Pure
    }

    /// Emit a "last expression" in a value-type method body.
    ///
    /// When NLR is active, wraps the expression in a `{Result, Self{N}}` tuple.
    /// Otherwise, emits the expression value directly (with proper indentation).
    fn emit_vt_last_expr(
        &mut self,
        expr: &Expression,
        index: usize,
        has_nlr: bool,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        if has_nlr {
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
            if index > 0 {
                body_parts.push(Document::Str("    "));
            }
            body_parts.push(Document::String(expr_code));
        }
        Ok(())
    }

    /// Generate a fallback body for an empty value-type method.
    ///
    /// Returns `{nil, Self}` when NLR is active, `nil` otherwise.
    fn generate_vt_empty_body(&self, has_nlr: bool) -> Vec<Document<'static>> {
        if has_nlr {
            let final_self = self.current_self_var();
            vec![docvec!["    {'nil', ", Document::String(final_self), "}\n",]]
        } else {
            vec![docvec!["    nil\n"]]
        }
    }

    /// Generate classified body expressions for a value-type method.
    ///
    /// Classifies each expression via [`Self::classify_vt_body_expr`] and dispatches
    /// to the appropriate handler. Classification happens inline (not upfront) because
    /// some checks (e.g. `is_conditional_with_vt_local_threading`) depend on scope
    /// bindings established by earlier expressions.
    fn generate_vt_body_exprs(
        &mut self,
        body: &[&Expression],
        has_nlr: bool,
    ) -> Result<Vec<Document<'static>>> {
        let mut body_parts: Vec<Document<'static>> = Vec::new();
        let body_len = body.len();

        for (i, expr) in body.iter().enumerate() {
            let kind = self.classify_vt_body_expr(expr);
            let is_last = i == body_len - 1;

            // Early return (^) at the method body level — emit value and stop generating.
            // Note: ^ inside a block is handled via the NLR throw mechanism (BT-754).
            if matches!(kind, VtBodyExprKind::EarlyReturn) {
                if let Expression::Return { value, .. } = expr {
                    if has_nlr {
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
                }
                break;
            }

            self.emit_vt_body_expr(expr, &kind, i, is_last, has_nlr, &mut body_parts)?;
        }

        Ok(body_parts)
    }

    /// Emit code for a single classified value-type body expression.
    ///
    /// Dispatches on the [`VtBodyExprKind`] to generate the appropriate
    /// Self-threading code for each expression kind.
    ///
    /// `EarlyReturn` is handled by the caller (`generate_vt_body_exprs`)
    /// before calling this method, so it is not matched here.
    fn emit_vt_body_expr(
        &mut self,
        expr: &Expression,
        kind: &VtBodyExprKind,
        index: usize,
        is_last: bool,
        has_nlr: bool,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        match kind {
            VtBodyExprKind::EarlyReturn => unreachable!("handled before dispatch"),
            VtBodyExprKind::FieldAssignment => {
                // BT-833/BT-900: Value type field assignment.
                // Non-last: open Self-threading let chain so Self{N} stays in scope.
                // Last: return the updated Self (not the assigned value).
                let doc = self.generate_vt_field_assignment_open(expr)?;
                body_parts.push(doc);
                if is_last {
                    let final_self = self.current_self_var();
                    if has_nlr {
                        // BT-854: NLR methods return {Self{N}, Self{N}} tuple.
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
                }
            }
            VtBodyExprKind::LocalAssignment => {
                if is_last {
                    self.emit_vt_last_expr(expr, index, has_nlr, body_parts)?;
                } else {
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
                            body_parts.push(docvec!["    let ", core_var, " = ", val_doc, " in\n"]);
                        }
                    }
                }
            }
            VtBodyExprKind::DestructureAssignment => {
                if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                    let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                    if is_last {
                        // Destructuring as last expression: emit bindings then return Self
                        for d in binding_docs {
                            body_parts.push(docvec!["    ", d]);
                        }
                        let final_self = self.current_self_var();
                        if has_nlr {
                            body_parts.push(docvec![
                                "    {'nil', ",
                                Document::String(final_self),
                                "}\n",
                            ]);
                        } else {
                            body_parts.push(docvec!["    ", Document::String(final_self), "\n"]);
                        }
                    } else {
                        for d in binding_docs {
                            body_parts.push(d);
                        }
                    }
                }
            }
            VtBodyExprKind::DoWithLocalThreading => {
                // BT-1053: Non-last `do:` loop that mutates captured outer locals.
                if is_last {
                    self.emit_vt_last_expr(expr, index, has_nlr, body_parts)?;
                } else {
                    let doc = self.generate_value_type_do_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::ConditionalWithLocalThreading => {
                // BT-1392: Non-last conditional with captured local mutations.
                if is_last {
                    self.emit_vt_last_expr(expr, index, has_nlr, body_parts)?;
                } else {
                    let doc = self.generate_vt_conditional_open(expr)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::BlockWithCapturedMutations(mutations) => {
                // BT-1213: Non-last block value with captured mutations.
                if is_last {
                    self.emit_vt_last_expr(expr, index, has_nlr, body_parts)?;
                } else {
                    let doc = self.generate_vt_block_value_open(expr, mutations)?;
                    body_parts.push(doc);
                }
            }
            VtBodyExprKind::Pure => {
                if is_last {
                    self.emit_vt_last_expr(expr, index, has_nlr, body_parts)?;
                } else {
                    // Non-last expressions: wrap in let to sequence side effects
                    let tmp_var = self.fresh_temp_var("seq");
                    let expr_code = self.capture_expression(expr)?;
                    body_parts.push(Document::String(format!(
                        "    let {tmp_var} = {expr_code} in\n"
                    )));
                }
            }
        }
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
        // BT-1435: Track current method selector for Logger intrinsic metadata.
        self.current_method_selector = Some(method.selector.name().to_string());
        let mut params = vec!["Self".to_string()];
        for param in &method.parameters {
            let var_name = self.fresh_var(&param.name.name);
            self.current_method_params.push(var_name.clone());
            params.push(var_name);
        }

        // BT-754: Detect whether any block argument in this method body contains ^.
        // If so, set up a non-local return token so ^ inside blocks can throw to escape
        // the closure and return from the enclosing method.
        let needs_nlr = self
            .semantic_facts
            .has_block_nlr_or_walk(&method.span, &method.body);

        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.set_current_nlr_token(Some(token_var.clone()));
            Some(token_var)
        } else {
            None
        };

        // Generate method body expressions.
        // BT-833: Value types now support Self-threading for field assignments.
        // Each `:=` produces a new Self{N} snapshot via maps:put (see generate_field_assignment).
        // Field reads use current_self_var() to reference the latest snapshot.
        // Filter out @expect directives — they are compile-time only and generate no code.
        let body = super::util::collect_body_exprs(&method.body);

        let has_nlr = nlr_token_var.is_some();

        // If filtering leaves no executable expressions, emit a safe fallback to
        // avoid generating an empty Core Erlang function body which would be
        // syntactically invalid (e.g., `fun (...) ->\n\n`).
        // BT-1482: Capture result so we can clean up scope/NLR unconditionally,
        // then propagate error afterwards.
        let body_result = if body.is_empty() {
            Ok(self.generate_vt_empty_body(has_nlr))
        } else {
            self.generate_vt_body_exprs(&body, has_nlr)
        };

        self.pop_scope();
        self.current_method_selector = None;
        self.set_current_nlr_token(None);

        let body_parts = body_result?;

        // BT-940: Annotate the `fun` expression (not just the body) with source line.
        // Annotating only the body would create invalid double-annotation when the body
        // is itself a single annotated MessageSend expression: `( ( e -| [...] ) -| [...] )`.
        let line_annotation = self.span_to_line(method.span);

        // BT-754/BT-764: Wrap the method body in try/catch to catch non-local
        // returns thrown by ^ inside block closures.
        let body_doc = Document::Vec(body_parts);
        let fun_doc = if let Some(token_var) = nlr_token_var {
            let catch_vars = self.wrap_value_type_body_with_nlr_catch(&token_var);
            docvec![
                format!("fun ({}) ->\n", params.join(", ")),
                catch_vars.format_try_prefix(),
                body_doc,
                catch_vars.format_catch_suffix(),
            ]
        } else {
            docvec![format!("fun ({}) ->\n", params.join(", ")), body_doc]
        };
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

    /// BT-1213: Generate an open let-chain for a non-last `[block_withmutations] value`
    /// in `ValueType` context. Each assignment in the block body becomes a top-level
    /// `let Var = val in\n` so the mutation is visible to subsequent expressions.
    fn generate_vt_block_value_open(
        &mut self,
        expr: &Expression,
        mutations: &[String],
    ) -> Result<Document<'static>> {
        let (block, arguments) = if let Expression::MessageSend {
            receiver,
            arguments,
            ..
        } = expr
        {
            if let Expression::Block(block) = receiver.as_ref() {
                (block, arguments.as_slice())
            } else {
                // Fallback: sequence as side effect
                let tmp_var = self.fresh_temp_var("seq");
                let doc = self.expression_doc(expr)?;
                return Ok(docvec![
                    "    let ",
                    Document::String(tmp_var),
                    " = ",
                    doc,
                    " in\n",
                ]);
            }
        } else {
            let tmp_var = self.fresh_temp_var("seq");
            let doc = self.expression_doc(expr)?;
            return Ok(docvec![
                "    let ",
                Document::String(tmp_var),
                " = ",
                doc,
                " in\n",
            ]);
        };

        let mut parts: Vec<Document<'static>> = Vec::new();

        // Use a nested scope so block parameters and block-local variables
        // don't leak into the method scope. Only captured mutations are
        // re-bound in the outer scope after the block finishes.
        self.push_scope();

        // Bind block parameters to arguments (for value: variants)
        for (i, param) in block.parameters.iter().enumerate() {
            if i < arguments.len() {
                let arg_var = self.fresh_temp_var(&param.name);
                let arg_doc = self.expression_doc(&arguments[i])?;
                parts.push(docvec![
                    "    let ",
                    Document::String(arg_var.clone()),
                    " = ",
                    arg_doc,
                    " in\n",
                ]);
                self.bind_var(&param.name, &arg_var);
            }
        }

        // Emit block body as open let-chains.
        // Track which variables are captured mutations so we can re-bind them
        // in the outer scope after popping the parameter scope.
        let mut rebound_vars: Vec<(String, String)> = Vec::new();

        let body = super::util::collect_body_exprs(&block.body);

        for body_expr in &body {
            if Self::is_local_var_assignment(body_expr) {
                if let Expression::Assignment { target, value, .. } = body_expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let core_var = self
                            .lookup_var(&id.name)
                            .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
                        let val_doc = self.expression_doc(value)?;
                        parts.push(docvec![
                            "    let ",
                            Document::String(core_var.clone()),
                            " = ",
                            val_doc,
                            " in\n",
                        ]);
                        self.bind_var(&id.name, &core_var);
                        // Track for re-binding in outer scope
                        if mutations.iter().any(|m| m == id.name.as_str()) {
                            rebound_vars.push((id.name.to_string(), core_var));
                        }
                    }
                }
            } else {
                let tmp = self.fresh_temp_var("seq");
                let doc = self.expression_doc(body_expr)?;
                parts.push(docvec![
                    "    let ",
                    Document::String(tmp),
                    " = ",
                    doc,
                    " in\n",
                ]);
            }
        }

        // Pop the block scope, then re-bind only captured mutations in the outer scope
        self.pop_scope();
        for (name, core_var) in &rebound_vars {
            self.bind_var(name, core_var);
        }

        Ok(Document::Vec(parts))
    }

    /// BT-1053: Returns `true` if `expr` is a `do:` message send with a literal
    /// block that, in `ValueType` context, captures and mutates outer local variables.
    ///
    /// Used by `generate_value_type_method` and `generate_class_method_body` (BT-1414)
    /// to select the open-let-chain path for non-last `do:` loops so the mutated
    /// locals are visible to subsequent exprs.
    pub(in crate::codegen::core_erlang) fn is_do_with_vt_local_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        // BT-1414: Allow class methods regardless of module context (Actor classes
        // have context=Actor but class methods still need local-map threading).
        if !self.in_class_method() && !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        if let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            if sel == "do:" {
                if let Some(Expression::Block(body)) = arguments.first() {
                    return !self.compute_threaded_locals_for_loop(body, None).is_empty();
                }
            }
        }
        false
    }

    /// BT-1053/BT-1414: Generates a non-last `do:` loop (with value-type/class-method
    /// captured local threading) as an **open let chain**.
    ///
    /// Unlike the closed form (which buries the extracted locals inside `_seqN = (... 'nil')`)
    /// this form emits the foldl and the `let X = maps:get(...)` extractions directly in
    /// the method body, ending with `in ` so the caller's next `body_part` provides the
    /// continuation.  This makes the updated locals visible to all subsequent expressions.
    pub(in crate::codegen::core_erlang) fn generate_value_type_do_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let (receiver, block_expr, body) = match expr {
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } if matches!(arguments.first(), Some(Expression::Block(_))) => {
                let block_arg = arguments.first().expect("checked above");
                let Some(Expression::Block(b)) = arguments.first() else {
                    return Ok(Document::Nil);
                };
                (receiver.as_ref(), block_arg, b)
            }
            _ => return Ok(Document::Nil),
        };

        // BT-1053 / Copilot review: validate that the block has exactly 1 parameter,
        // matching the check in the normal generate_list_do path.
        validate_block_arity_exact(
            block_expr,
            1,
            "do:",
            "The do: block must take exactly one argument: [:each | ...]",
        )?;

        let threaded_locals = self.compute_threaded_locals_for_loop(body, None);

        // Phase 1: create a fresh map and pack each captured local into it.
        let init_map_var = self.fresh_temp_var("InitMap");
        let mut pack_prefix = String::new();
        let _ = write!(pack_prefix, "let {init_map_var} = call 'maps':'new'() in ");
        let mut current = init_map_var;
        for var_name in &threaded_locals {
            let packed_var = self.fresh_temp_var("Packed");
            let core_var = self
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
            let key = Self::local_state_key(var_name);
            let _ = write!(
                pack_prefix,
                "let {packed_var} = call 'maps':'put'('{key}', {core_var}, {current}) in "
            );
            current = packed_var;
        }
        let init_state_code = current;

        // Phase 2: generate receiver + list guard + lambda.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        let mut docs: Vec<Document<'static>> = vec![
            Document::String(pack_prefix),
            docvec![
                format!("let {list_var} = "),
                recv_code,
                format!(
                    " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                     <'true'> when 'true' -> {list_var} \
                     <'false'> when 'true' -> \
                     call 'beamtalk_collection':'to_list'({list_var}) end \
                     in let {lambda_var} = fun ({item_var}, StateAcc) -> "
                ),
            ],
        ];

        // Phase 3: generate foldl lambda body (reuses existing threading helper).
        let body_doc = self.generate_list_do_body_with_threading(body, &item_var)?;
        docs.push(body_doc);

        // Phase 4: call foldl, then extract each local as an open `let X = ... in `.
        // These `let` bindings are emitted into the method body scope, not inside an
        // expression, so they shadow the pre-loop bindings and are visible to all
        // subsequent `body_parts`.
        let fold_result = self.fresh_temp_var("FoldResult");
        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {init_state_code}, {safe_list_var}) in "
        );
        for var_name in &threaded_locals {
            let core_var = Self::to_core_erlang_var(var_name);
            // Update the scope so subsequent method-body expressions look up
            // this Erlang variable name.
            self.bind_var(var_name, &core_var);
            let key = Self::local_state_key(var_name);
            let _ = write!(
                post_code,
                "let {core_var} = call 'maps':'get'('{key}', {fold_result}) in "
            );
        }
        // Intentionally open — the caller's next body_part provides the continuation.
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    /// BT-1392: Returns `true` if `expr` is an `ifTrue:`, `ifFalse:`, or `ifTrue:ifFalse:`
    /// message send where at least one block argument writes to a local variable that is
    /// already bound in the enclosing scope, in value-type or class-method context.
    ///
    /// Unlike `captured_mutations_for_block` (which requires `captured_reads ∩ local_writes`),
    /// this also catches pure assignments like `[x := 2]` where `x` is only written, not
    /// read, inside the block.
    pub(in crate::codegen::core_erlang) fn is_conditional_with_vt_local_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        if !self.in_class_method() && !matches!(self.context, CodeGenContext::ValueType) {
            return false;
        }
        if let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            match sel.as_str() {
                "ifTrue:" | "ifFalse:" => {
                    if let Some(Expression::Block(block)) = arguments.first() {
                        return !self.outer_scope_mutations_in_block(block).is_empty();
                    }
                }
                "ifTrue:ifFalse:" => {
                    let has_mutations = arguments.iter().any(|a| {
                        if let Expression::Block(b) = a {
                            !self.outer_scope_mutations_in_block(b).is_empty()
                        } else {
                            false
                        }
                    });
                    return has_mutations;
                }
                _ => {}
            }
        }
        false
    }

    /// BT-1392: Returns the list of local variables written in `block` that are already
    /// bound in the enclosing scope. This is scope-aware — it checks `lookup_var` to
    /// distinguish "reassigning outer x" from "defining new local y".
    fn outer_scope_mutations_in_block(&self, block: &crate::ast::Block) -> Vec<String> {
        use crate::codegen::core_erlang::block_analysis::analyze_block;
        let analysis = analyze_block(block);
        let mut result: Vec<String> = analysis
            .local_writes
            .iter()
            .filter(|var| self.lookup_var(var).is_some())
            .cloned()
            .collect();
        result.sort();
        result
    }

    /// BT-1392: Generates a non-last `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` with captured
    /// local mutations as an **open let chain** in value-type or class-method context.
    ///
    /// Generates an inline `case` expression where:
    /// - Each branch inlines the block body as sequential let-bindings
    /// - The case returns the updated values of captured variables (tuple for N>1)
    /// - After the case, variables are rebound via `element/N` extraction
    ///
    /// This makes the updated locals visible to all subsequent method body expressions.
    pub(in crate::codegen::core_erlang) fn generate_vt_conditional_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let (receiver, selector_name, arguments) = match expr {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } => {
                let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                (receiver.as_ref(), sel, arguments)
            }
            _ => return Ok(Document::Nil),
        };

        // Collect all outer-scope mutations across all block arguments.
        let mut all_mutations: Vec<String> = Vec::new();
        for arg in arguments {
            if let Expression::Block(block) = arg {
                for var in self.outer_scope_mutations_in_block(block) {
                    if !all_mutations.contains(&var) {
                        all_mutations.push(var);
                    }
                }
            }
        }
        if all_mutations.is_empty() {
            return Ok(Document::Nil);
        }

        let cond_var = self.fresh_temp_var("Cond");
        let cond_doc = self.expression_doc(receiver)?;
        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(docvec![
            "let ",
            Document::String(cond_var.clone()),
            " = ",
            cond_doc,
            " in ",
        ]);

        // Generate the true and false branch documents based on the selector.
        let (true_branch, false_branch) = match selector_name.as_str() {
            "ifTrue:" => {
                let Expression::Block(block) = &arguments[0] else {
                    return Ok(Document::Nil);
                };
                let tb = self.generate_vt_conditional_branch(block, &all_mutations)?;
                let fb = self.generate_vt_conditional_passthrough(&all_mutations);
                (tb, fb)
            }
            "ifFalse:" => {
                let Expression::Block(block) = &arguments[0] else {
                    return Ok(Document::Nil);
                };
                let passthrough = self.generate_vt_conditional_passthrough(&all_mutations);
                let fb = self.generate_vt_conditional_branch(block, &all_mutations)?;
                (passthrough, fb)
            }
            "ifTrue:ifFalse:" => {
                let Expression::Block(true_block) = &arguments[0] else {
                    return Ok(Document::Nil);
                };
                let Expression::Block(false_block) = &arguments[1] else {
                    return Ok(Document::Nil);
                };
                let tb = self.generate_vt_conditional_branch(true_block, &all_mutations)?;
                let fb = self.generate_vt_conditional_branch(false_block, &all_mutations)?;
                (tb, fb)
            }
            _ => return Ok(Document::Vec(docs)),
        };

        // Generate the case expression and rebind variables after it.
        let result_var = self.fresh_temp_var("CondResult");
        docs.push(docvec![
            "let ",
            Document::String(result_var.clone()),
            " = case ",
            Document::String(cond_var),
            " of <'true'> when 'true' -> ",
            true_branch,
            " <'false'> when 'true' -> ",
            false_branch,
            " end in ",
        ]);

        self.rebind_vt_conditional_mutations(&mut docs, &all_mutations, &result_var);

        Ok(Document::Vec(docs))
    }

    /// Generates a branch body for a VT conditional that inlines the block and returns
    /// the final values of all captured mutations as a value or tuple.
    fn generate_vt_conditional_branch(
        &mut self,
        block: &crate::ast::Block,
        all_mutations: &[String],
    ) -> Result<Document<'static>> {
        self.push_scope();
        let result = self.build_vt_conditional_branch_parts(block, all_mutations);
        self.pop_scope();
        result
    }

    /// Inner implementation for `generate_vt_conditional_branch`.
    ///
    /// Separated so that `push_scope`/`pop_scope` always bracket the fallible
    /// work regardless of whether `?` propagates an error.
    fn build_vt_conditional_branch_parts(
        &mut self,
        block: &crate::ast::Block,
        all_mutations: &[String],
    ) -> Result<Document<'static>> {
        let body = super::util::collect_body_exprs(&block.body);

        let mut parts: Vec<Document<'static>> = Vec::new();
        for body_expr in &body {
            if Self::is_local_var_assignment(body_expr) {
                if let Expression::Assignment { target, value, .. } = body_expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let core_var = self
                            .lookup_var(&id.name)
                            .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
                        let val_doc = self.expression_doc(value)?;
                        parts.push(docvec![
                            "let ",
                            Document::String(core_var.clone()),
                            " = ",
                            val_doc,
                            " in ",
                        ]);
                        self.bind_var(&id.name, &core_var);
                    }
                }
            } else {
                let tmp = self.fresh_temp_var("seq");
                let doc = self.expression_doc(body_expr)?;
                parts.push(docvec!["let ", Document::String(tmp), " = ", doc, " in ",]);
            }
        }

        // Return the final values of all captured mutations
        let return_doc = Self::build_vt_mutation_return(self, all_mutations);
        parts.push(return_doc);
        Ok(Document::Vec(parts))
    }

    /// Generates a pass-through branch that returns the current (pre-branch) values of
    /// all captured mutations, leaving them unchanged.
    fn generate_vt_conditional_passthrough(&self, all_mutations: &[String]) -> Document<'static> {
        Self::build_vt_mutation_return(self, all_mutations)
    }

    /// Builds a return expression for the current values of `all_mutations`:
    /// a single variable if there is one mutation, or a tuple if there are multiple.
    fn build_vt_mutation_return(cg: &Self, all_mutations: &[String]) -> Document<'static> {
        if all_mutations.len() == 1 {
            let var = &all_mutations[0];
            let core_var = cg
                .lookup_var(var)
                .cloned()
                .unwrap_or_else(|| Self::to_core_erlang_var(var));
            Document::String(core_var)
        } else {
            let mut tuple_parts: Vec<Document<'static>> = vec![Document::Str("{")];
            for (i, var) in all_mutations.iter().enumerate() {
                if i > 0 {
                    tuple_parts.push(Document::Str(", "));
                }
                let core_var = cg
                    .lookup_var(var)
                    .cloned()
                    .unwrap_or_else(|| Self::to_core_erlang_var(var));
                tuple_parts.push(Document::String(core_var));
            }
            tuple_parts.push(Document::Str("}"));
            Document::Vec(tuple_parts)
        }
    }

    /// Appends `let VAR = <result_var>` or `let VAR = element(N, <result_var>)` bindings
    /// to `docs`, updating the scope so subsequent expressions see the new variable names.
    fn rebind_vt_conditional_mutations(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        all_mutations: &[String],
        result_var: &str,
    ) {
        if all_mutations.len() == 1 {
            // Single variable: case returns the value directly
            let var = &all_mutations[0];
            let core_var = Self::to_core_erlang_var(var);
            self.bind_var(var, &core_var);
            docs.push(docvec![
                "let ",
                Document::String(core_var),
                " = ",
                Document::String(result_var.to_string()),
                " in ",
            ]);
        } else {
            // Multiple variables: case returns a tuple, extract with element/N
            for (i, var) in all_mutations.iter().enumerate() {
                let core_var = Self::to_core_erlang_var(var);
                self.bind_var(var, &core_var);
                docs.push(docvec![
                    "let ",
                    Document::String(core_var),
                    " = call 'erlang':'element'(",
                    Document::String((i + 1).to_string()),
                    ", ",
                    Document::String(result_var.to_string()),
                    ") in ",
                ]);
            }
        }
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
        if let Some(module) = self.class_module_index().get(class_name) {
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
    fn generate_primitive_dispatch(
        &mut self,
        class: &ClassDefinition,
        auto_methods: Option<&AutoSlotMethods>,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let superclass_mod = self.superclass_module_name(class.superclass_name());

        // BT-447: Class-methods-only classes skip protocol boilerplate —
        // but only when there are no auto-generated slot methods either.
        let has_auto_instance_methods =
            auto_methods.is_some_and(|a| !a.getters.is_empty() || !a.setters.is_empty());
        if class.methods.is_empty() && !has_auto_instance_methods && superclass_mod.is_some() {
            return self.generate_minimal_dispatch(class);
        }

        // asString — generate default for classes that don't define it
        let has_as_string = self
            .semantic_facts
            .class_facts(&class_name)
            .is_some_and(|cf| cf.has_instance_method("asString"));
        let as_string_branch = Self::generate_dispatch_as_string_branch(&class_name, has_as_string);

        // BT-924: User-defined value objects (ClassKind::Value) store slots in the
        // underlying map and support read-only reflection via fieldAt: and fieldNames.
        // Stdlib primitive types (ClassKind::Object) have no map slots — block both.
        let is_value_class = class.class_kind == ClassKind::Value;
        let (field_names_branch, field_at_branch) =
            Self::generate_dispatch_reflection_branches(&class_name, is_value_class);

        // fieldAt:put: hint — suggest with*: for value objects, assignment for primitives
        let immutable_hint = if is_value_class {
            Self::core_erlang_binary(
                "Cannot modify slot on value type \u{2014} use withSlot: to create a new instance",
            )
        } else {
            Self::core_erlang_binary(&format!(
                "{class_name}s are immutable. Use assignment (x := newValue) instead."
            ))
        };

        // Route each class-defined method to its individual function
        let method_branches = self.generate_dispatch_method_branches(class, &mod_name);

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
            // fieldNames — BT-924: delegates to beamtalk_reflection for value objects
            field_names_branch,
            // fieldAt: — BT-924: reads from map for value objects, error for primitives
            field_at_branch,
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
            // BT-923: Auto-generated getter and with*: dispatch arms
            Document::Vec(
                auto_methods
                    .map(|a| Self::generate_auto_slot_dispatch_arms(&mod_name, a))
                    .unwrap_or_default(),
            ),
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

    /// Generates the `asString` dispatch arm if the class does not define it itself.
    ///
    /// Returns `Document::Nil` (empty vec) when the class already provides `asString`,
    /// or when there is no built-in default for the class name.
    fn generate_dispatch_as_string_branch(
        class_name: &str,
        has_as_string: bool,
    ) -> Document<'static> {
        if has_as_string {
            return Document::Vec(Vec::new());
        }
        let default_str = match class_name {
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
    }

    /// Generates the `fieldNames` and `fieldAt:` dispatch arms.
    ///
    /// Value objects (`ClassKind::Value`) support reflective field access via
    /// `beamtalk_reflection`. Primitive types raise an immutable-value error.
    fn generate_dispatch_reflection_branches(
        class_name: &str,
        is_value_class: bool,
    ) -> (Document<'static>, Document<'static>) {
        let field_names_branch: Document<'static> = if is_value_class {
            docvec![
                "        <'fieldNames'> when 'true' ->\n",
                "            call 'beamtalk_reflection':'field_names'(Self)\n",
            ]
        } else {
            docvec![
                "        <'fieldNames'> when 'true' ->\n",
                "            []\n",
            ]
        };

        let field_at_branch: Document<'static> = if is_value_class {
            docvec![
                "        <'fieldAt:'> when 'true' ->\n",
                "            let <FaName> = call 'erlang':'hd'(Args) in\n",
                "            call 'beamtalk_reflection':'read_field'(FaName, Self)\n",
            ]
        } else {
            let iva_hint = Self::core_erlang_binary(&format!(
                "{class_name}s are immutable and have no fields."
            ));
            docvec![
                "        <'fieldAt:'> when 'true' ->\n",
                "            let <IvaErr0> = call 'beamtalk_error':'new'('immutable_value', '",
                Document::String(class_name.to_string()),
                "') in\n",
                "            let <IvaErr1> = call 'beamtalk_error':'with_selector'(IvaErr0, 'fieldAt:') in\n",
                "            let <IvaErr2> = call 'beamtalk_error':'with_hint'(IvaErr1, ",
                Document::String(iva_hint),
                ") in\n",
                "            call 'beamtalk_error':'raise'(IvaErr2)\n",
            ]
        };

        (field_names_branch, field_at_branch)
    }

    /// Generates dispatch case arms for all class-defined instance methods.
    ///
    /// Each arm routes to the module-level function for that method, unwrapping
    /// `{Result, State}` tuples for NLR-capable methods (BT-854).
    fn generate_dispatch_method_branches(
        &self,
        class: &ClassDefinition,
        mod_name: &str,
    ) -> Vec<Document<'static>> {
        let mut method_branches: Vec<Document<'static>> = Vec::new();
        for method in &class.methods {
            let mangled = method.selector.to_erlang_atom();
            method_branches.push(Document::String(format!(
                "        <'{mangled}'> when 'true' ->\n"
            )));

            // BT-854: Methods with NLR return {Result, State} tuple — unwrap via case.
            let has_nlr = self
                .semantic_facts
                .has_block_nlr_or_walk(&method.span, &method.body);

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
                Document::String(format!("call '{mod_name}':'")),
                Document::String(mangled.clone()),
                Document::Str("'("),
                Document::Vec(call_args),
                Document::Str(")"),
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
        method_branches
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
        auto_methods: Option<&AutoSlotMethods>,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let superclass_mod = self.superclass_module_name(class.superclass_name());

        // BT-447: Class-methods-only classes delegate directly to superclass —
        // but only when there are no auto-generated slot methods either.
        let has_auto_instance_methods =
            auto_methods.is_some_and(|a| !a.getters.is_empty() || !a.setters.is_empty());
        if class.methods.is_empty() && !has_auto_instance_methods && superclass_mod.is_some() {
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
        let has_explicit_as_string = self
            .semantic_facts
            .class_facts(&class_name)
            .is_some_and(|cf| cf.has_instance_method("asString"));
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

        // BT-923: Add auto-generated getter and with*: setter selectors
        if let Some(auto) = auto_methods {
            for field in &auto.getters {
                selectors.push(format!("'{field}'"));
            }
            for field in &auto.setters {
                let with_sel = AutoSlotMethods::with_star_selector(field);
                selectors.push(format!("'{with_sel}'"));
            }
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

#[cfg(test)]
mod tests {
    use crate::ast::{
        ClassDefinition, ClassKind, DeclaredKeyword, Identifier, Literal, Module, StateDeclaration,
    };
    use crate::codegen::core_erlang::CoreErlangGenerator;
    use crate::codegen::core_erlang::value_type_codegen::{
        AutoSlotMethods, compute_auto_slot_methods,
    };
    use crate::source_analysis::Span;

    fn s() -> Span {
        Span::new(0, 0)
    }

    fn make_value_class(name: &str, slots: &[&str]) -> ClassDefinition {
        let state = slots
            .iter()
            .map(|slot_name| StateDeclaration {
                name: Identifier::new(*slot_name, s()),
                type_annotation: None,
                default_value: Some(crate::ast::Expression::Literal(Literal::Integer(0), s())),
                comments: crate::ast::CommentAttachment::default(),
                doc_comment: None,
                declared_keyword: DeclaredKeyword::default(),
                span: s(),
            })
            .collect();
        let mut class = ClassDefinition::new(
            Identifier::new(name, s()),
            Identifier::new("Value", s()),
            state,
            vec![],
            s(),
        );
        // Explicitly set class_kind to avoid relying on constructor inference
        class.class_kind = ClassKind::Value;
        class
    }

    fn make_actor_class(name: &str) -> ClassDefinition {
        ClassDefinition::new(
            Identifier::new(name, s()),
            Identifier::new("Actor", s()),
            vec![],
            vec![],
            s(),
        )
    }

    #[test]
    fn test_with_star_selector_single_char() {
        assert_eq!(AutoSlotMethods::with_star_selector("x"), "withX:");
    }

    #[test]
    fn test_with_star_selector_multi_char() {
        assert_eq!(
            AutoSlotMethods::with_star_selector("firstName"),
            "withFirstName:"
        );
    }

    #[test]
    fn test_compute_auto_slot_methods_actor_returns_none() {
        let class = make_actor_class("Counter");
        assert!(
            compute_auto_slot_methods(&class).is_none(),
            "actor classes should not get auto slot methods"
        );
    }

    #[test]
    fn test_compute_auto_slot_methods_value_class_returns_getters_setters() {
        let class = make_value_class("Point", &["x", "y"]);
        let auto = compute_auto_slot_methods(&class).unwrap();
        assert!(auto.getters.contains(&"x".to_string()));
        assert!(auto.getters.contains(&"y".to_string()));
        assert!(auto.setters.contains(&"x".to_string()));
        assert!(auto.setters.contains(&"y".to_string()));
    }

    #[test]
    fn test_compute_auto_slot_methods_keyword_constructor() {
        let class = make_value_class("Point", &["x", "y"]);
        let auto = compute_auto_slot_methods(&class).unwrap();
        assert_eq!(
            auto.keyword_constructor,
            Some("x:y:".to_string()),
            "should generate keyword constructor selector from slot names"
        );
    }

    #[test]
    fn test_compute_auto_slot_methods_no_slots() {
        let class = make_value_class("Empty", &[]);
        let auto = compute_auto_slot_methods(&class).unwrap();
        assert!(auto.getters.is_empty());
        assert!(auto.setters.is_empty());
        assert!(auto.keyword_constructor.is_none());
    }

    #[test]
    fn test_generate_value_type_module_includes_class_name() {
        let class = make_value_class("Point", &["x", "y"]);
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let mut generator = CoreErlangGenerator::new("point");
        let doc = generator.generate_value_type_module(&module).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'Point'"),
            "generated module should reference class name. Got: {output}"
        );
        assert!(
            output.contains("'new'"),
            "generated module should include new constructor. Got: {output}"
        );
    }
}
