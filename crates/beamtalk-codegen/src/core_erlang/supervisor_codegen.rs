// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP supervisor module code generation (ADR 0059 Phase 3, ADR 0135 §2).
//!
//! **DDD Context:** Code Generation
//!
//! Generates `-behaviour(supervisor)` modules for `Supervisor subclass:` and
//! `DynamicSupervisor subclass:` classes. The generated module is intentionally
//! thin — all restart logic lives in the stdlib and `beamtalk_supervisor.erl`.
//!
//! ## Generated structure
//!
//! For `Supervisor subclass: WebApp` (and likewise `DynamicSupervisor subclass:`):
//! - `start_link/0` — `supervisor:start_link({local, ?MODULE}, ?MODULE, nil)`
//!   (the class-named singleton `supervise` starts; its start args are `nil`)
//! - `start_link/1` — `supervisor:start_link(?MODULE, Args)` (anonymous; lets a
//!   parent pass start args, ADR 0135)
//! - `init/1` — forwards the start args to `beamtalk_supervisor:static_init/3`
//!   (or `dynamic_init/3`), which reads the configuration (`childSpecs`,
//!   `strategy`, …) by sending it to the supervisor instance being started
//! - the instance methods the class defines (configuration, `initialize`, …)
//!   plus `dispatch/3,4`, `has_method/1` and `has_method_local/1`, so instance
//!   dispatch on a supervisor handle reaches them before the stdlib base class

use std::cell::RefCell;
use std::collections::HashSet;

use super::dispatch_spec::{self, DispatchSpec, SuperclassDelegation};
use super::util::ClassIdentity;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, spec_codegen};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf::{self, atom};
use beamtalk_core::ast::{ClassDefinition, Module, SupervisorKind};

impl CoreErlangGenerator {
    /// Generates an OTP supervisor module for `Supervisor subclass:` or
    /// `DynamicSupervisor subclass:` classes.
    ///
    /// The `supervisor_kind` field set by semantic analysis picks the
    /// `init/1` target (`static_init/3` or `dynamic_init/3`).
    pub(super) fn generate_supervisor_module(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Use ValueType context — no actor state threading for supervisors.
        self.context = CodeGenContext::ValueType;

        let class = module.classes.first().ok_or_else(|| {
            CodeGenError::Internal("supervisor module must have a class".to_string())
        })?;
        self.set_class_identity(Some(ClassIdentity::new(&class.name.name)));
        // Record field declared types so `self.<field>`
        // comparisons/arithmetic on object-typed fields dispatch here too,
        // consistent with value-type and actor modules.
        self.set_class_field_types(class);

        let kind = class.supervisor_kind.as_ref().ok_or_else(|| {
            CodeGenError::Internal("supervisor_kind must be set for supervisor module".to_string())
        })?;
        let init_fn = match kind {
            SupervisorKind::Static => "static_init",
            SupervisorKind::Dynamic => "dynamic_init",
        };

        let class_name = class.name.name.clone();
        let module_name = self.module_name.clone();

        // Build export list as Documents (CLAUDE.md: use Document/docvec!, never format!)
        let class_method_exports: Document<'static> = Self::build_class_method_export_doc(module);
        let instance_method_exports = Self::build_supervisor_instance_exports(class);

        // Module attributes
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);
        let file_attr = self.file_attr();
        let source_path_attr = self.source_path_attr();
        // Use the generator's cross-module-aware alias
        // registry (this module's own `type_aliases` merged with any
        // pre-loaded aliases from other modules in the same compilation
        // unit — see `CoreErlangGenerator::alias_registry`'s doc) so an
        // alias-named annotation resolves to a `user_type` reference (ADR
        // 0108) instead of falling through to `any()`, regardless of which
        // module declared the alias.
        // Tracks which alias names the specs below actually
        // reference, so `generate_alias_type_attrs` only emits `-type`
        // declarations for those (plus transitive deps) instead of every
        // pre-loaded alias in the compilation unit.
        let referenced_aliases: RefCell<HashSet<_>> = RefCell::new(HashSet::new());
        let spec_attrs = spec_codegen::generate_class_specs(
            class,
            true,
            Some(&self.alias_registry),
            Some(&referenced_aliases),
        );
        let spec_suffix: Document<'static> = spec_codegen::format_spec_attributes(&spec_attrs)
            .map_or(Document::Nil, |s| docvec![",\n     ", s]);
        // Every class module that could contain a `user_type`
        // reference must also declare the matching named `-type` in the same
        // module attribute list (an `erlc` compile error otherwise) — empty
        // for a module with no `type_aliases`, so this is a no-op change for
        // the common case.
        let alias_type_attrs =
            spec_codegen::generate_alias_type_attrs(&self.alias_registry, &referenced_aliases);
        let alias_type_suffix: Document<'static> =
            spec_codegen::format_alias_type_attributes(&alias_type_attrs)
                .map_or(Document::Nil, |s| docvec![",\n     ", s]);

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Module header
        docs.push(docvec![
            "module ",
            atom(module_name.to_string()),
            " ['start_link'/0, 'start_link'/1, 'init'/1",
            instance_method_exports,
            class_method_exports,
            ", 'superclass'/0, '__beamtalk_meta'/0, 'register_class'/0]\n",
            "  attributes ['behaviour' = ['supervisor'], 'on_load' = [{'register_class', 0}]",
            beamtalk_class_attr,
            file_attr,
            source_path_attr,
            alias_type_suffix,
            spec_suffix,
            "]\n",
            "\n",
        ]);

        // start_link/0, start_link/1
        docs.push(Self::generate_sup_start_link(&module_name));
        docs.push(Document::Str("\n\n"));

        // init/1
        docs.push(Self::generate_sup_init(init_fn, &class_name, &module_name));
        docs.push(Document::Str("\n\n"));

        // Instance methods (configuration, initialize, ...) and their dispatch
        docs.push(self.generate_supervisor_instance_side(class)?);

        // User-defined class-side method functions
        if !class.class_methods.is_empty() {
            docs.push(self.generate_class_method_functions(class)?);
            docs.push(Document::Str("\n"));
        }

        // superclass/0, __beamtalk_meta/0, register_class/0
        docs.push(self.generate_superclass_function(module)?);
        docs.push(Document::Str("\n"));
        docs.push(self.generate_meta_function(module, false)?);
        docs.push(self.generate_register_class(module, false)?);

        docs.push(Document::Str("end\n"));
        Ok(Document::Vec(docs))
    }

    /// Export entries for a supervisor's instance side: one function per
    /// instance method (`Self` plus its parameters) and the dispatch entry
    /// points `beamtalk_dispatch` looks for.
    fn build_supervisor_instance_exports(class: &ClassDefinition) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for method in &class.methods {
            parts.push(docvec![
                ", ",
                leaf::fname(
                    method.selector.name().to_string(),
                    method.parameters.len() + 1
                ),
            ]);
        }
        parts.push(Document::Str(
            ", 'dispatch'/3, 'dispatch'/4, 'has_method'/1, 'has_method_local'/1",
        ));
        Document::Vec(parts)
    }

    /// Generates a supervisor's instance side (ADR 0135 §2).
    ///
    /// Supervisor handles (`{beamtalk_supervisor, Class, Module, Pid}`) are
    /// dispatched in the caller's process by `beamtalk_dispatch:lookup/5`,
    /// which probes each class with `has_method_local/1` and calls its
    /// `dispatch/4`. Each instance method compiles exactly like a value-type
    /// method (a pure function of `Self` and its arguments); `dispatch/3`
    /// routes the class's own selectors and delegates everything else to the
    /// superclass module, ending at the stdlib `Supervisor` /
    /// `DynamicSupervisor` base.
    ///
    /// `has_method_local/1` answers only for the class's own selectors, so a
    /// selector the class does not define (e.g. `printString`) still resolves
    /// with the base class as its owner, where `beamtalk_dispatch` applies the
    /// supervisor-specific rendering.
    fn generate_supervisor_instance_side(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let super_mod = self
            .superclass_module_name(class.superclass_name())
            .ok_or_else(|| {
                CodeGenError::Internal("supervisor class must have a superclass".to_string())
            })?;

        let mut docs: Vec<Document<'static>> = Vec::new();
        for method in &class.methods {
            docs.push(self.generate_value_type_method(method, class)?);
            docs.push(Document::Str("\n"));
        }

        let method_branches = self.generate_dispatch_method_branches(class, &mod_name);
        docs.push(docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->\n",
            "    case Selector of\n",
            Document::Vec(method_branches),
            // Default: an extension on this class (which has_method_local/1
            // also answers for), then the superclass.
            "        <_Other> when 'true' ->\n",
            "            case call 'beamtalk_extensions':'lookup'(",
            atom(class_name.clone()),
            ", Selector) of\n",
            "                <{'ok', ExtFun, _ExtOwner}> when 'true' ->\n",
            "                    apply ExtFun(Args, Self)\n",
            "                <'not_found'> when 'true' ->\n",
            "                    call ",
            atom(super_mod.clone()),
            ":'dispatch'(Selector, Args, Self)\n",
            "            end\n",
            "    end\n",
            "\n",
        ]);
        docs.push(docvec![
            "'dispatch'/4 = fun (Selector, Args, Self, State) ->\n",
            Self::generate_dispatch_4_wrapper(mod_name.as_str()),
            "\n",
        ]);

        let own_methods: Vec<String> = class
            .methods
            .iter()
            .map(|m| m.selector.name().to_string())
            .collect();
        docs.push(dispatch_spec::generate_has_method_from_spec(
            &own_methods,
            &DispatchSpec {
                reflection: &[],
                class_name: &class_name,
                superclass: Some(SuperclassDelegation::Static(&super_mod)),
                dnu: false,
                auto_slots: None,
                emit_local_probe: true,
            },
        ));
        Ok(Document::Vec(docs))
    }

    /// Generates `start_link/0` and `start_link/1` for both static and
    /// dynamic supervisors.
    ///
    /// `start_link/0` registers the supervisor locally under `?MODULE` (the
    /// Erlang module atom), making it findable via `whereis/1` and
    /// `beamtalk_supervisor:current/1`; its start args are `nil`.
    /// `start_link/1` starts an anonymous instance with the given start args
    /// (ADR 0135), which `init/1` hands to the configuration methods as
    /// `self args`.
    ///
    /// ```erlang
    /// start_link() ->
    ///     supervisor:start_link({local, 'bt@webapp'}, 'bt@webapp', nil).
    /// start_link(Args) ->
    ///     supervisor:start_link('bt@webapp', Args).
    /// ```
    fn generate_sup_start_link(module_name: &str) -> Document<'static> {
        docvec![
            "'start_link'/0 = fun () ->",
            "\n    call 'supervisor':'start_link'({'local', ",
            atom(module_name.to_string()),
            "}, ",
            atom(module_name.to_string()),
            ", 'nil')",
            "\n\n",
            "'start_link'/1 = fun (Args) ->",
            "\n    call 'supervisor':'start_link'(",
            atom(module_name.to_string()),
            ", Args)",
        ]
    }

    /// Generates `init/1`, forwarding the start args to
    /// `beamtalk_supervisor:static_init/3` or `dynamic_init/3`.
    ///
    /// The runtime reads each configuration setting by sending it to the
    /// supervisor being started (instance dispatch in this process), never
    /// through the class `gen_server`, which may be blocked waiting for
    /// `supervisor:start_link` to return (deadlock).
    ///
    /// ```erlang
    /// init(Args) ->
    ///     beamtalk_supervisor:static_init('bt@webapp', 'WebApp', Args).
    /// ```
    fn generate_sup_init(
        init_fn: &'static str,
        class_name: &str,
        module_name: &str,
    ) -> Document<'static> {
        docvec![
            "'init'/1 = fun (Args) ->",
            "\n    call 'beamtalk_supervisor':",
            atom(init_fn),
            "(",
            atom(module_name.to_string()),
            ", ",
            atom(class_name.to_string()),
            ", Args)",
        ]
    }
}
