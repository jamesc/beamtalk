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
    #[allow(clippy::too_many_lines)]
    pub(super) fn generate_actor_module(&mut self, module: &Module) -> Result<Document<'static>> {
        // BT-1220: Concrete Supervisor/DynamicSupervisor subclasses generate OTP supervisor
        // behaviour, not gen_server. Abstract base classes compile as value types.
        if module
            .classes
            .first()
            .is_some_and(|c| c.supervisor_kind.is_some() && !c.is_abstract)
        {
            return self.generate_supervisor_module(module);
        }

        // ADR 0056: Native Erlang-backed actors generate a thin facade module
        // instead of a full gen_server module.
        // Validate uniform native-ness: all classes must be either native or non-native.
        let native_count = module
            .classes
            .iter()
            .filter(|c| c.backing_module.is_some())
            .count();
        if native_count > 0 {
            if native_count != module.classes.len() {
                return Err(super::CodeGenError::Internal(format!(
                    "module '{}' mixes native and non-native classes; \
                     all classes in a module must be uniformly native or non-native",
                    self.module_name
                )));
            }
            return self.generate_native_facade_module(module);
        }

        // BT-213: Set context to Actor for this module
        self.context = CodeGenContext::Actor;
        self.setup_class_identity(module);

        // Check if module has class definitions for registration
        let has_classes = !module.classes.is_empty();
        // BT-1610: Protocol-only files also need register_class/0 for protocol registration
        let has_protocols = !module.protocols.is_empty();
        let needs_register_class = has_classes || has_protocols;
        // BT-403: Build sealed method exports
        let sealed_export_doc = self.build_sealed_export_doc(module);
        // BT-411: Build class method exports
        let class_method_export_doc = Self::build_class_method_export_doc(module);
        // BT-1218: Synthesize class_supervisionSpec for Actor subclasses that don't define it
        let needs_spec_synthesis = self.needs_supervision_spec_synthesis(module);
        let supervision_spec_export: Document<'static> = if needs_spec_synthesis {
            Document::Str(", 'class_supervisionSpec'/2")
        } else {
            Document::Nil
        };
        // BT-105: Check if class is abstract
        let is_abstract = module.classes.first().is_some_and(|c| c.is_abstract);

        // Module header with expanded exports per BT-29
        // BT-217: Add 'new'/0 and 'new'/1 exports for error methods
        // BT-242: Add 'has_method'/1 export for reflection
        let has_primitive_methods = self.class_has_primitive_instance_methods(module);

        let dispatch_3_export: Document<'static> = if has_primitive_methods {
            Document::Str(", 'dispatch'/3")
        } else {
            Document::Nil
        };

        // BT-942: Only export __beamtalk_meta/0 for class-based modules
        let meta_export: Document<'static> = if has_classes {
            Document::Str(", '__beamtalk_meta'/0")
        } else {
            Document::Nil
        };

        let base_exports: Document<'static> = docvec![
            "'start_link'/1, 'start_link'/2, 'init'/1, 'handle_continue'/2, \
             'handle_cast'/2, 'handle_call'/3, \
             'handle_info'/2, 'code_change'/3, 'terminate'/2, 'dispatch'/4",
            dispatch_3_export,
            ", 'safe_dispatch'/3, \
             'method_table'/0, 'has_method'/1, 'class_name'/0, \
             'spawn'/0, 'spawn'/1, 'new'/0, 'new'/1, \
             'superclass'/0",
            meta_export,
        ];

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

        // BT-845/BT-860: Build beamtalk_source attribute when source_path is set.
        let source_path_attr = self.source_path_attr();
        // BT-940: Build 'file' attribute so BEAM stacktraces show the .bt source file.
        let file_attr = self.file_attr();

        // Module header with exports and attributes
        // BT-1610: Include register_class/0 export and on_load attribute when
        // the module has classes OR protocols (protocol-only files need it too).
        let module_header = if needs_register_class {
            docvec![
                "module '",
                Document::Eco(self.module_name.clone()),
                "' [",
                base_exports,
                sealed_export_doc,
                class_method_export_doc,
                supervision_spec_export,
                ", 'register_class'/0]",
                "\n",
                "  attributes ['behaviour' = ['gen_server'], \
                 'on_load' = [{'register_class', 0}]",
                beamtalk_class_attr,
                file_attr,
                source_path_attr,
                spec_suffix,
                "]\n",
            ]
        } else {
            docvec![
                "module '",
                Document::Eco(self.module_name.clone()),
                "' [",
                base_exports,
                sealed_export_doc,
                class_method_export_doc,
                supervision_spec_export,
                "]",
                "\n",
                "  attributes ['behaviour' = ['gen_server']",
                file_attr,
                source_path_attr,
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
        push_fn(self.generate_start_link_named_doc())?;

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
        push_fn(self.generate_class_name_function(module)?)?;
        push_fn(self.generate_init_function(module)?)?;

        // BT-403: Abstract classes skip gen_server callback scaffolding.
        if is_abstract {
            push_fn(self.generate_abstract_callbacks_doc())?;
        } else {
            // BT-1541: handle_continue dispatches initialize after the loop starts
            push_fn(self.generate_handle_continue()?)?;
            push_fn(self.generate_handle_cast()?)?;
            push_fn(self.generate_handle_call()?)?;
            push_fn(self.generate_handle_info()?)?;
            push_fn(self.generate_code_change()?)?;
            push_fn(self.generate_terminate(module)?)?;
            push_fn(self.generate_safe_dispatch()?)?;
        }

        push_fn(self.generate_dispatch(module)?)?;
        if has_primitive_methods {
            push_fn(self.generate_primitive_dispatch_3_doc())?;
        }
        push_fn(self.generate_method_table(module)?)?;
        // has_method/1 — no trailing newline needed
        docs.push(self.generate_has_method(module)?);

        // BT-403: Generate sealed method standalone functions
        if !self.sealed_method_selectors().is_empty() {
            docs.push(self.generate_sealed_method_functions_doc(module)?);
        }

        // BT-411: Generate class-side method standalone functions
        if let Some(class) = module.classes.first() {
            if !class.class_methods.is_empty() {
                docs.push(self.generate_class_method_functions(class)?);
            }
        }

        // BT-1218: Synthesize class_supervisionSpec for Actor subclasses
        if needs_spec_synthesis {
            docs.push(Document::Str("\n"));
            // Determine which module's class_supervisionPolicy to call.
            // We check within the current file's AST (both inline class methods and
            // standalone method definitions); cross-file inherited overrides are not
            // resolved here (BT-1218 Phase 1 limitation — see the doc comment on
            // generate_supervision_spec_synthesis).
            let has_local_policy_override = module.classes.first().is_some_and(|c| {
                let class_name = &c.name.name;
                let has_inline = self
                    .semantic_facts
                    .class_facts(class_name)
                    .is_some_and(|cf| cf.has_class_method("supervisionPolicy"));
                let has_standalone = module.method_definitions.iter().any(|md| {
                    md.is_class_method
                        && &md.class_name.name == class_name
                        && md.method.selector.name() == "supervisionPolicy"
                });
                has_inline || has_standalone
            });
            docs.push(self.generate_supervision_spec_synthesis(has_local_policy_override));
        }

        // Generate class registration and metadata functions
        if !module.classes.is_empty() {
            // BT-942: Generate __beamtalk_meta/0 for zero-process reflection
            docs.push(Document::Str("\n"));
            docs.push(self.generate_meta_function(module, needs_spec_synthesis)?);
            // BT-218: Generate register_class/0 for class system registration
            docs.push(self.generate_register_class(module, needs_spec_synthesis)?);
        } else if has_protocols {
            // BT-1610: Protocol-only modules need register_class/0 for protocol
            // registration even though there are no classes to register.
            docs.push(Document::Str("\n"));
            docs.push(self.generate_register_class(module, false)?);
        }

        // Module end
        docs.push(Document::Str("end\n"));

        Ok(Document::Vec(docs))
    }

    /// Returns true if the class has any instance methods with a selector-based `@primitive` body.
    ///
    /// Used to decide whether to generate `dispatch/3` in the actor module.
    /// Selector-based primitives (quoted, e.g. `@primitive "show:"`) generate calls to
    /// `Module:dispatch/3` at runtime; structural intrinsics (unquoted) do not.
    fn class_has_primitive_instance_methods(&self, module: &Module) -> bool {
        module.classes.first().is_some_and(|c| {
            self.semantic_facts
                .class_facts(&c.name.name)
                .is_some_and(|cf| cf.has_primitive_instance_methods)
        })
    }

    /// Sets up class identity and sealed method selectors from the module's class definition.
    /// BT-295: Set class identity for @primitive codegen.
    /// BT-403: Include sealed/abstract flags and collect sealed method selectors.
    pub(in crate::codegen::core_erlang) fn setup_class_identity(&mut self, module: &Module) {
        if let Some(class) = module.classes.first() {
            self.set_class_identity(Some(ClassIdentity::from_class_def(
                &class.name.name,
                class.is_sealed,
                class.is_abstract,
            )));

            // Collect sealed method selectors for direct-call optimization.
            // Only sealed classes benefit: the dispatch fast path checks is_class_sealed().
            self.sealed_method_selectors_mut().clear();
            if class.is_sealed {
                for method in &class.methods {
                    if method.kind == MethodKind::Primary {
                        self.sealed_method_selectors_mut()
                            .insert(method.selector.name().to_string());
                    }
                }
            }
        }
    }

    /// Builds the export fragment for sealed method standalone functions (BT-403).
    fn build_sealed_export_doc(&self, module: &Module) -> Document<'static> {
        // Sort selectors for deterministic output across builds
        let mut selectors: Vec<String> = self.sealed_method_selectors().iter().cloned().collect();
        selectors.sort();
        let mut parts: Vec<Document<'static>> = Vec::new();
        for sel in selectors {
            let arity = module.classes.first().map_or(0, |c| {
                self.semantic_facts
                    .class_facts(&c.name.name)
                    .and_then(|cf| cf.instance_method_index(sel.as_str()))
                    .map_or(0, |i| c.methods[i].selector.arity())
            });
            // Standalone function takes Args + Self + State params
            parts.push(docvec![
                ", '__sealed_",
                Document::String(sel),
                "'/",
                Document::String((arity + 2).to_string()),
            ]);
        }
        Document::Vec(parts)
    }

    /// Builds the export fragment for class-side method functions (BT-411).
    pub(in crate::codegen::core_erlang) fn build_class_method_export_doc(
        module: &Module,
    ) -> Document<'static> {
        let Some(class) = module.classes.first() else {
            return Document::Nil;
        };
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(class.class_methods.len());
        for m in class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
        {
            // BT-412: Class method takes ClassSelf + ClassVars + user params
            parts.push(docvec![
                ", 'class_",
                Document::Eco(m.selector.name()),
                "'/",
                Document::String((m.selector.arity() + 2).to_string()),
            ]);
        }
        Document::Vec(parts)
    }

    /// Generates minimal `gen_server` callbacks for abstract classes (BT-403).
    ///
    /// Abstract classes can't be instantiated, so these callbacks will never
    /// be called. But `gen_server` behaviour requires them to be exported.
    fn generate_abstract_callbacks_doc(&self) -> Document<'static> {
        let module_name = &self.module_name;
        docvec![
            // handle_continue - never called for abstract classes
            "'handle_continue'/2 = fun (_Continue, State) -> {'noreply', State}",
            "\n\n",
            // handle_cast - never called for abstract classes
            "'handle_cast'/2 = fun (_Msg, State) -> {'noreply', State}",
            "\n\n",
            // handle_call - never called for abstract classes
            "'handle_call'/3 = fun (_Msg, _From, State) -> {'reply', 'nil', State}",
            "\n\n",
            // handle_info - delegate to base, never called for abstract classes
            "'handle_info'/2 = fun (Msg, State) -> call 'beamtalk_actor':'handle_info'(Msg, State)",
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
                    docvec![
                        "call '",
                        Document::Eco(module_name.clone()),
                        "':'dispatch'(Selector, Args, Self, State)",
                    ],
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

        let mut docs: Vec<Document<'static>> = Vec::with_capacity(class.methods.len());

        for method in &class.methods {
            if method.kind != MethodKind::Primary {
                continue;
            }
            let selector_name = method.selector.name().to_string();
            if !self.sealed_method_selectors().contains(&selector_name) {
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

            // BT-761: Detect NLR in sealed method body
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

            // Generate method body with reply tuple (reuse existing codegen)
            // BT-1482: Capture result so we can clean up NLR token and scope
            // unconditionally, then propagate error afterwards.
            let method_body_result = self.generate_method_definition_body_with_reply(method);

            self.set_current_nlr_token(None);

            // If codegen failed, pop scope before propagating the error.
            let method_body_doc = match method_body_result {
                Ok(doc) => doc,
                Err(e) => {
                    self.pop_scope();
                    return Err(e);
                }
            };

            // BT-761/BT-764: Sealed methods are standalone functions (not inside case arms),
            // so the try/catch can be placed directly at function level (no letrec needed).
            // BT-774: Compose at Document level without intermediate string rendering.
            let method_body_doc = if let Some(ref token_var) = nlr_token_var {
                self.wrap_actor_body_with_nlr_catch(method_body_doc, token_var, false)
            } else {
                method_body_doc
            };

            // BT-940: Annotate the `fun` expression (not just the body) with source line.
            // Annotating the body would create invalid `( ( e -| [...] ) -| [...] )` when the
            // body is itself a single annotated MessageSend expression.
            let fun_doc = docvec![
                "fun (",
                all_params.join(", "),
                ") ->",
                "\n",
                nest(INDENT, docvec![line(), method_body_doc,]),
            ];
            let fun_doc = if let Some(line_num) = self.span_to_line(method.span) {
                self.annotate_with_line(fun_doc, line_num)
            } else {
                fun_doc
            };

            // Generate: '__sealed_{selector}'/N = fun (Arg1, ..., Self, State) ->
            let method_entry = docvec![
                "\n",
                "'__sealed_",
                Document::String(selector_name.clone()),
                "'/",
                Document::String(arity.to_string()),
                "  = ",
                fun_doc,
                "\n",
            ];
            docs.push(method_entry);

            self.pop_scope();
        }

        Ok(Document::Vec(docs))
    }

    /// Returns true if this actor module needs a synthesized `class_supervisionSpec/2`.
    ///
    /// Synthesis is skipped when the class already defines `class supervisionSpec`
    /// explicitly (e.g. the `Actor` base class itself), either as an inline class
    /// method or as a standalone class-side method definition.
    pub(in crate::codegen::core_erlang) fn needs_supervision_spec_synthesis(
        &self,
        module: &Module,
    ) -> bool {
        let Some(class) = module.classes.first() else {
            return false;
        };
        let class_name = &class.name.name;
        // Skip if supervisionSpec is defined as an inline class method
        let has_inline = self
            .semantic_facts
            .class_facts(class_name)
            .is_some_and(|cf| cf.has_class_method("supervisionSpec"));
        // Skip if supervisionSpec is defined as a standalone class-side method
        let has_standalone = module.method_definitions.iter().any(|md| {
            md.is_class_method
                && &md.class_name.name == class_name
                && md.method.selector.name() == "supervisionSpec"
        });
        !has_inline && !has_standalone
    }

    /// Generates the synthesized `class_supervisionSpec/2` function (BT-1218).
    ///
    /// Calls `class_supervisionPolicy` **directly** to avoid a `gen_server` re-entrant
    /// deadlock: class methods execute inside a `gen_server:call` handler, so routing
    /// back through `class_send` would block the process waiting for itself.
    ///
    /// When `has_local_policy_override` is true, calls the current module's own
    /// `class_supervisionPolicy` (the subclass override). When false, the subclass
    /// has no local override, so we call Actor's `class_supervisionPolicy` directly
    /// which returns `#temporary`.
    ///
    /// **Phase 1 limitation**: Only local (same-file) `class supervisionPolicy`
    /// overrides are detected. If an intermediate superclass in another file defines
    /// `supervisionPolicy`, the synthesized `class_supervisionSpec` will call Actor's
    /// default and return `#temporary` instead of the inherited policy. This affects
    /// multi-level inheritance hierarchies where the intermediate class is compiled
    /// separately. A future phase should resolve the defining class via the full
    /// cross-module class hierarchy and call the correct module directly.
    ///
    /// ```erlang
    /// %% With local override (has_local_policy_override = true):
    /// 'class_supervisionSpec'/2 = fun (ClassSelf, ClassVars) ->
    ///     let CMR = call 'bt@my_actor':'class_supervisionPolicy'(ClassSelf, ClassVars) in
    ///     ...
    ///
    /// %% Without local override (has_local_policy_override = false):
    /// 'class_supervisionSpec'/2 = fun (ClassSelf, ClassVars) ->
    ///     let CMR = call 'bt@stdlib@actor':'class_supervisionPolicy'(ClassSelf, ClassVars) in
    ///     ...
    /// ```
    fn generate_supervision_spec_synthesis(
        &self,
        has_local_policy_override: bool,
    ) -> Document<'static> {
        let spec_mod = self.compiled_module_name("SupervisionSpec");
        let policy_mod: Document<'static> = if has_local_policy_override {
            Document::Eco(self.module_name.clone())
        } else {
            Document::String(self.compiled_module_name("Actor"))
        };
        docvec![
            "'class_supervisionSpec'/2 = fun (ClassSelf, ClassVars) ->\n",
            "    let CMR = call '",
            policy_mod,
            "':'class_supervisionPolicy'(ClassSelf, ClassVars) in\n",
            "    case CMR of\n",
            "      <{'class_var_result', Policy, NewClassVars}> when 'true' ->\n",
            "        let Spec0 = call '",
            Document::String(spec_mod.clone()),
            "':'new'() in\n",
            "        let Spec1 = call '",
            Document::String(spec_mod.clone()),
            "':'withActorClass:'(Spec0, ClassSelf) in\n",
            "        let Spec2 = call '",
            Document::String(spec_mod.clone()),
            "':'withRestart:'(Spec1, Policy) in\n",
            "        {'class_var_result', Spec2, NewClassVars}\n",
            "      <Policy> when 'true' ->\n",
            "        let Spec0 = call '",
            Document::String(spec_mod.clone()),
            "':'new'() in\n",
            "        let Spec1 = call '",
            Document::String(spec_mod.clone()),
            "':'withActorClass:'(Spec0, ClassSelf) in\n",
            "        call '",
            Document::String(spec_mod),
            "':'withRestart:'(Spec1, Policy)\n",
            "    end\n",
        ]
    }
}
