// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for actor classes, producing
//! `gen_server`-based Erlang modules with async messaging, error isolation,
//! and hot code reload support.

use super::document::{Document, INDENT, line, nest};
use super::spec_codegen;
use super::util::ClassIdentity;
use super::{CodeGenContext, CoreErlangGenerator, Result};
use crate::ast::{MethodKind, Module};
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates a full actor module with `gen_server` behaviour.
    ///
    /// Per BT-29 design doc, each actor class generates a `gen_server` module with:
    /// - Error isolation via `safe_dispatch/3`
    /// - `doesNotUnderstand:args:` fallback dispatch
    /// - `terminate/2` with method call support
    ///
    /// ## Object References (BT-100)
    ///
    /// The `#beamtalk_object{}` record is defined in `runtime/include/beamtalk.hrl`
    /// and is now used by generated code:
    /// - `spawn/0` and `spawn/1` return `#beamtalk_object{class, class_mod, pid}` records
    /// - Message sends extract the pid using `call 'erlang':'element'(4, Obj)`
    /// - This enables reflection (`obj class`) and proper object semantics
    pub(super) fn generate_actor_module(&mut self, module: &Module) -> Result<Document<'static>> {
        // BT-213: Set context to Actor for this module
        self.context = CodeGenContext::Actor;
        self.setup_class_identity(module);

        // Check if module has class definitions for registration
        let has_classes = !module.classes.is_empty();
        // BT-403: Build sealed method exports
        let sealed_export_str = self.build_sealed_export_str(module);
        // BT-411: Build class method exports
        let class_method_export_str = Self::build_class_method_export_str(module);
        // BT-105: Check if class is abstract
        let is_abstract = module.classes.first().is_some_and(|c| c.is_abstract);

        // Module header with expanded exports per BT-29
        // BT-217: Add 'new'/0 and 'new'/1 exports for error methods
        // BT-242: Add 'has_method'/1 export for reflection
        let base_exports = "'start_link'/1, 'init'/1, 'handle_cast'/2, 'handle_call'/3, \
                            'code_change'/3, 'terminate'/2, 'dispatch'/4, 'safe_dispatch'/3, \
                            'method_table'/0, 'has_method'/1, 'spawn'/0, 'spawn'/1, 'new'/0, 'new'/1, \
                            'superclass'/0";

        let mut docs: Vec<Document<'static>> = Vec::new();

        // BT-586: Generate spec attributes from type annotations
        let spec_attrs = module
            .classes
            .first()
            .map(|class| spec_codegen::generate_class_specs(class, false))
            .unwrap_or_default();
        let spec_suffix: Document<'static> = spec_codegen::format_spec_attributes(&spec_attrs)
            .map_or(Document::Nil, |s| docvec![",\n     ", s]);

        // BT-745: Build beamtalk_class attribute for dependency-ordered bootstrap
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);

        // Module header with exports and attributes
        let module_header = if has_classes {
            docvec![
                format!(
                    "module '{}' [{base_exports}{sealed_export_str}{class_method_export_str}, 'register_class'/0]",
                    self.module_name
                ),
                "\n",
                "  attributes ['behaviour' = ['gen_server'], \
                 'on_load' = [{'register_class', 0}]",
                beamtalk_class_attr,
                spec_suffix,
                "]\n",
            ]
        } else {
            docvec![
                format!(
                    "module '{}' [{base_exports}{sealed_export_str}{class_method_export_str}]",
                    self.module_name
                ),
                "\n",
                "  attributes ['behaviour' = ['gen_server']",
                spec_suffix,
                "]\n",
            ]
        };
        docs.push(module_header);
        docs.push(Document::Str("\n"));

        // Helper: push a generated function + newline separator
        let mut push_fn = |doc: Document<'static>| -> Result<()> {
            docs.push(doc);
            docs.push(Document::Str("\n"));
            Ok(())
        };

        push_fn(self.generate_start_link_doc())?;

        if is_abstract {
            push_fn(self.generate_abstract_spawn_error_method()?)?;
            push_fn(self.generate_abstract_spawn_with_args_error_method()?)?;
        } else {
            push_fn(self.generate_spawn_function(module)?)?;
            push_fn(self.generate_spawn_with_args_function(module)?)?;
        }

        push_fn(self.generate_actor_new_error_method()?)?;
        push_fn(self.generate_actor_new_with_args_error_method()?)?;
        push_fn(self.generate_superclass_function(module)?)?;
        push_fn(self.generate_init_function(module)?)?;

        // BT-403: Abstract classes skip gen_server callback scaffolding.
        if is_abstract {
            push_fn(self.generate_abstract_callbacks_doc())?;
        } else {
            push_fn(self.generate_handle_cast()?)?;
            push_fn(self.generate_handle_call()?)?;
            push_fn(self.generate_code_change()?)?;
            push_fn(self.generate_terminate(module)?)?;
            push_fn(self.generate_safe_dispatch()?)?;
        }

        push_fn(self.generate_dispatch(module)?)?;
        push_fn(self.generate_method_table(module)?)?;
        // has_method/1 — no trailing newline needed
        docs.push(self.generate_has_method(module)?);

        // BT-403: Generate sealed method standalone functions
        if !self.sealed_method_selectors.is_empty() {
            docs.push(self.generate_sealed_method_functions_doc(module)?);
        }

        // BT-411: Generate class-side method standalone functions
        if let Some(class) = module.classes.first() {
            if !class.class_methods.is_empty() {
                docs.push(self.generate_class_method_functions(class)?);
            }
        }

        // Generate class registration function (BT-218)
        if !module.classes.is_empty() {
            docs.push(Document::Str("\n"));
            docs.push(self.generate_register_class(module)?);
        }

        // Module end
        docs.push(Document::Str("end\n"));

        Ok(Document::Vec(docs))
    }

    /// Sets up class identity and sealed method selectors from the module's class definition.
    /// BT-295: Set class identity for @primitive codegen.
    /// BT-403: Include sealed/abstract flags and collect sealed method selectors.
    fn setup_class_identity(&mut self, module: &Module) {
        if let Some(class) = module.classes.first() {
            self.class_identity = Some(ClassIdentity::from_class_def(
                &class.name.name,
                class.is_sealed,
                class.is_abstract,
            ));

            // Collect sealed method selectors for direct-call optimization.
            // Only sealed classes benefit: the dispatch fast path checks is_class_sealed().
            self.sealed_method_selectors.clear();
            if class.is_sealed {
                for method in &class.methods {
                    if method.kind == MethodKind::Primary {
                        self.sealed_method_selectors
                            .insert(method.selector.name().to_string());
                    }
                }
            }
        }
    }

    /// Builds the export string for sealed method standalone functions (BT-403).
    fn build_sealed_export_str(&self, module: &Module) -> String {
        // Sort selectors for deterministic output across builds
        let mut selectors: Vec<&String> = self.sealed_method_selectors.iter().collect();
        selectors.sort();
        let sealed_exports: Vec<String> = selectors
            .iter()
            .map(|sel| {
                let arity = module.classes.first().map_or(0, |c| {
                    c.methods
                        .iter()
                        .find(|m| m.selector.name() == sel.as_str())
                        .map_or(0, |m| m.selector.arity())
                });
                // Standalone function takes Args + Self + State params
                format!("'__sealed_{sel}'/{}", arity + 2)
            })
            .collect();
        if sealed_exports.is_empty() {
            String::new()
        } else {
            format!(", {}", sealed_exports.join(", "))
        }
    }

    /// Builds the export string for class-side method functions (BT-411).
    fn build_class_method_export_str(module: &Module) -> String {
        if let Some(class) = module.classes.first() {
            let class_method_exports: Vec<String> = class
                .class_methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .map(|m| {
                    // BT-412: Class method takes ClassSelf + ClassVars + user params
                    format!("'class_{}'/{}", m.selector.name(), m.selector.arity() + 2)
                })
                .collect();
            if class_method_exports.is_empty() {
                String::new()
            } else {
                format!(", {}", class_method_exports.join(", "))
            }
        } else {
            String::new()
        }
    }

    /// Generates minimal `gen_server` callbacks for abstract classes (BT-403).
    ///
    /// Abstract classes can't be instantiated, so these callbacks will never
    /// be called. But `gen_server` behaviour requires them to be exported.
    fn generate_abstract_callbacks_doc(&self) -> Document<'static> {
        let module_name = &self.module_name;
        docvec![
            // handle_cast - never called for abstract classes
            "'handle_cast'/2 = fun (_Msg, State) -> {'noreply', State}",
            "\n\n",
            // handle_call - never called for abstract classes
            "'handle_call'/3 = fun (_Msg, _From, State) -> {'reply', 'nil', State}",
            "\n\n",
            // code_change
            "'code_change'/3 = fun (_OldVsn, State, _Extra) -> {'ok', State}",
            "\n\n",
            // terminate
            "'terminate'/2 = fun (_Reason, _State) -> 'ok'",
            "\n\n",
            // safe_dispatch - abstract classes use dispatch directly (no error isolation needed)
            "'safe_dispatch'/3 = fun (Selector, Args, State) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "let Self = call 'beamtalk_actor':'make_self'(State) in",
                    line(),
                    format!("call '{module_name}':'dispatch'(Selector, Args, Self, State)"),
                ]
            ),
            "\n\n",
        ]
    }

    /// Generates standalone functions for sealed methods (BT-403).
    ///
    /// Each sealed method gets a `'__sealed_{selector}'/N` function that
    /// can be called directly from self-sends, bypassing both `safe_dispatch/3`
    /// and the `dispatch/4` case selector matching.
    fn generate_sealed_method_functions_doc(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let Some(class) = module.classes.first() else {
            return Ok(Document::Nil);
        };

        let mut docs: Vec<Document<'static>> = Vec::new();

        for method in &class.methods {
            if method.kind != MethodKind::Primary {
                continue;
            }
            let selector_name = method.selector.name().to_string();
            if !self.sealed_method_selectors.contains(&selector_name) {
                continue;
            }

            // Reset state version for this method
            let arity = method.selector.arity() + 2; // + Self + State
            self.reset_state_version();
            self.push_scope();
            self.current_method_params.clear();

            // Generate parameter list and populate current_method_params
            // (needed for @primitive codegen which reads current_method_params)
            let mut params = Vec::new();
            for param in &method.parameters {
                let var_name = self.fresh_var(&param.name.name);
                self.current_method_params.push(var_name.clone());
                params.push(var_name);
            }

            // Parameters, then Self, then State
            let mut all_params: Vec<String> = params.clone();
            all_params.push("Self".to_string());
            all_params.push("State".to_string());

            // Generate: '__sealed_{selector}'/N = fun (Arg1, ..., Self, State) ->
            let header = docvec![
                "\n",
                format!("'__sealed_{selector_name}'/{arity}  = fun ("),
                "\n",
                all_params.join(", "),
                ") ->",
                "\n",
            ];
            docs.push(header);

            // BT-761: Detect NLR in sealed method body
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

            // Generate method body with reply tuple (reuse existing codegen)
            let method_body_doc = self.generate_method_definition_body_with_reply(method)?;

            self.current_nlr_token = None;

            // BT-761/BT-764: Sealed methods are standalone functions (not inside case arms),
            // so the try/catch can be placed directly at function level (no letrec needed).
            // BT-774: Compose at Document level without intermediate string rendering.
            let method_body_doc = if let Some(ref token_var) = nlr_token_var {
                self.wrap_actor_body_with_nlr_catch(method_body_doc, token_var, false)
            } else {
                method_body_doc
            };

            let body_doc = docvec![nest(INDENT, docvec![line(), method_body_doc,]), "\n",];
            docs.push(body_doc);

            self.pop_scope();
        }

        Ok(Document::Vec(docs))
    }
}
