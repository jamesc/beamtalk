// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP supervisor module code generation (BT-1220, ADR 0059 Phase 3).
//!
//! **DDD Context:** Code Generation
//!
//! Generates `-behaviour(supervisor)` modules for `Supervisor subclass:` and
//! `DynamicSupervisor subclass:` classes. The generated module is intentionally
//! thin (~60 lines of Core Erlang) — all restart logic lives in the stdlib and
//! `beamtalk_supervisor.erl`.
//!
//! ## Generated structure
//!
//! For `Supervisor subclass: WebApp`:
//! - `start_link/0` — calls `supervisor:start_link({local, ?MODULE}, ?MODULE, [])`
//! - `init/1` — gets children/strategy/maxRestarts/restartWindow via class dispatch,
//!   builds child specs via `beamtalk_supervisor:build_child_specs/1`
//!
//! For `DynamicSupervisor subclass: WorkerPool`:
//! - `start_link/0` — same
//! - `init/1` — `simple_one_for_one` with `childClass`, `maxRestarts`, `restartWindow`

use super::document::Document;
use super::util::ClassIdentity;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, spec_codegen};
use crate::ast::{MethodKind, Module, SupervisorKind};
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates an OTP supervisor module for `Supervisor subclass:` or
    /// `DynamicSupervisor subclass:` classes (BT-1220).
    ///
    /// Delegates to `generate_static_supervisor` or `generate_dynamic_supervisor`
    /// based on the `supervisor_kind` field set by semantic analysis (BT-1218).
    pub(super) fn generate_supervisor_module(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Use ValueType context — no actor state threading for supervisors.
        self.context = CodeGenContext::ValueType;

        let class = module.classes.first().ok_or_else(|| {
            CodeGenError::Internal("supervisor module must have a class".to_string())
        })?;
        self.class_identity = Some(ClassIdentity::new(&class.name.name));

        match class.supervisor_kind.as_ref().ok_or_else(|| {
            CodeGenError::Internal("supervisor_kind must be set for supervisor module".to_string())
        })? {
            SupervisorKind::Static => self.generate_static_supervisor(module),
            SupervisorKind::Dynamic => self.generate_dynamic_supervisor(module),
        }
    }

    /// Generates a static supervisor module (`Supervisor subclass:`).
    fn generate_static_supervisor(&mut self, module: &Module) -> Result<Document<'static>> {
        let class = module.classes.first().ok_or_else(|| {
            CodeGenError::Internal("static supervisor module must have a class".to_string())
        })?;
        let class_name = class.name.name.clone();
        let module_name = self.module_name.clone();

        // Build export list as Documents (CLAUDE.md: use Document/docvec!, never format!)
        let mut class_method_exports: Document<'static> = Document::Nil;
        for m in class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
        {
            let arity = m.selector.arity() + 2; // +2 for ClassSelf + ClassVars
            class_method_exports = docvec![
                class_method_exports,
                ", 'class_",
                Document::String(m.selector.name().to_string()),
                "'/",
                Document::String(arity.to_string()),
            ];
        }

        // Module attributes
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);
        let file_attr = self.file_attr();
        let source_path_attr = self.source_path_attr();
        let spec_attrs = spec_codegen::generate_class_specs(class, true);
        let spec_suffix: Document<'static> = spec_codegen::format_spec_attributes(&spec_attrs)
            .map_or(Document::Nil, |s| docvec![",\n     ", s]);

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Module header
        docs.push(docvec![
            "module '",
            Document::String(module_name.clone()),
            "' ['start_link'/0, 'init'/1",
            class_method_exports,
            ", 'superclass'/0, '__beamtalk_meta'/0, 'register_class'/0]\n",
            "  attributes ['behaviour' = ['supervisor'], 'on_load' = [{'register_class', 0}]",
            beamtalk_class_attr,
            file_attr,
            source_path_attr,
            spec_suffix,
            "]\n",
            "\n",
        ]);

        // start_link/0
        docs.push(Self::generate_sup_start_link(&module_name));
        docs.push(Document::Str("\n\n"));

        // init/1 for static supervisor
        docs.push(Self::generate_static_sup_init(&class_name));
        docs.push(Document::Str("\n"));

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

    /// Generates a dynamic supervisor module (`DynamicSupervisor subclass:`).
    fn generate_dynamic_supervisor(&mut self, module: &Module) -> Result<Document<'static>> {
        let class = module.classes.first().ok_or_else(|| {
            CodeGenError::Internal("dynamic supervisor module must have a class".to_string())
        })?;
        let class_name = class.name.name.clone();
        let module_name = self.module_name.clone();

        // Build export list as Documents (CLAUDE.md: use Document/docvec!, never format!)
        let mut class_method_exports: Document<'static> = Document::Nil;
        for m in class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
        {
            let arity = m.selector.arity() + 2;
            class_method_exports = docvec![
                class_method_exports,
                ", 'class_",
                Document::String(m.selector.name().to_string()),
                "'/",
                Document::String(arity.to_string()),
            ];
        }

        // Module attributes
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);
        let file_attr = self.file_attr();
        let source_path_attr = self.source_path_attr();
        let spec_attrs = spec_codegen::generate_class_specs(class, true);
        let spec_suffix: Document<'static> = spec_codegen::format_spec_attributes(&spec_attrs)
            .map_or(Document::Nil, |s| docvec![",\n     ", s]);

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Module header
        // BT-1220: childClass/0 is called directly by beamtalk_supervisor:startChild/1,2
        // (SupMod:'childClass'() at `beamtalk_supervisor.erl:217`)
        docs.push(docvec![
            "module '",
            Document::String(module_name.clone()),
            "' ['start_link'/0, 'init'/1, 'childClass'/0",
            class_method_exports,
            ", 'superclass'/0, '__beamtalk_meta'/0, 'register_class'/0]\n",
            "  attributes ['behaviour' = ['supervisor'], 'on_load' = [{'register_class', 0}]",
            beamtalk_class_attr,
            file_attr,
            source_path_attr,
            spec_suffix,
            "]\n",
            "\n",
        ]);

        // start_link/0
        docs.push(Self::generate_sup_start_link(&module_name));
        docs.push(Document::Str("\n\n"));

        // init/1 for dynamic supervisor
        docs.push(Self::generate_dynamic_sup_init(&class_name));
        docs.push(Document::Str("\n\n"));

        // childClass/0 — called by beamtalk_supervisor:startChild to resolve the child module
        docs.push(Self::generate_dynamic_child_class(&class_name));
        docs.push(Document::Str("\n"));

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

    /// Generates `start_link/0` for both static and dynamic supervisors.
    ///
    /// Registers the supervisor locally under `?MODULE` (the Erlang module atom),
    /// making it findable via `whereis/1` and `beamtalk_supervisor:current/1`.
    ///
    /// ```erlang
    /// start_link() ->
    ///     supervisor:start_link({local, 'bt@webapp'}, 'bt@webapp', []).
    /// ```
    fn generate_sup_start_link(module_name: &str) -> Document<'static> {
        docvec![
            "'start_link'/0 = fun () ->",
            "\n    call 'supervisor':'start_link'({'local', '",
            Document::String(module_name.to_string()),
            "'}, '",
            Document::String(module_name.to_string()),
            "', [])",
        ]
    }

    /// Generates `init/1` for a static supervisor (`Supervisor subclass:`).
    ///
    /// Calls `children`, `strategy`, `maxRestarts`, `restartWindow` via the
    /// class dispatch system (so inherited defaults from `Supervisor` are used
    /// automatically) and delegates child spec construction to
    /// `beamtalk_supervisor:build_child_specs/1`.
    ///
    /// ```erlang
    /// init([]) ->
    ///     SupCp = beamtalk_class_registry:whereis_class('WebApp'),
    ///     Children = beamtalk_object_class:class_send(SupCp, children, []),
    ///     Strategy = beamtalk_object_class:class_send(SupCp, strategy, []),
    ///     MaxR = beamtalk_object_class:class_send(SupCp, maxRestarts, []),
    ///     MaxT = beamtalk_object_class:class_send(SupCp, restartWindow, []),
    ///     SupFlags = #{strategy => Strategy, intensity => MaxR, period => MaxT},
    ///     Specs = beamtalk_supervisor:build_child_specs(Children),
    ///     {ok, {SupFlags, Specs}}.
    /// ```
    fn generate_static_sup_init(class_name: &str) -> Document<'static> {
        docvec![
            "'init'/1 = fun (_Args) ->",
            "\n    let SupCp = call 'beamtalk_class_registry':'whereis_class'('",
            Document::String(class_name.to_string()),
            "') in",
            "\n    let SupChildren = call 'beamtalk_object_class':'class_send'(SupCp, 'children', []) in",
            "\n    let SupStrategy = call 'beamtalk_object_class':'class_send'(SupCp, 'strategy', []) in",
            "\n    let SupMaxR = call 'beamtalk_object_class':'class_send'(SupCp, 'maxRestarts', []) in",
            "\n    let SupMaxT = call 'beamtalk_object_class':'class_send'(SupCp, 'restartWindow', []) in",
            "\n    let SupFlags = ~{'strategy' => SupStrategy, 'intensity' => SupMaxR, 'period' => SupMaxT}~ in",
            "\n    let SupSpecs = call 'beamtalk_supervisor':'build_child_specs'(SupChildren) in",
            "\n    {'ok', {SupFlags, SupSpecs}}",
        ]
    }

    /// Generates `init/1` for a dynamic supervisor (`DynamicSupervisor subclass:`).
    ///
    /// Uses `simple_one_for_one` strategy with a single child spec template
    /// built from `childClass` via `beamtalk_supervisor:build_child_specs/1`.
    ///
    /// ```erlang
    /// init([]) ->
    ///     SupCp = beamtalk_class_registry:whereis_class('WorkerPool'),
    ///     ChildClass = beamtalk_object_class:class_send(SupCp, childClass, []),
    ///     MaxR = beamtalk_object_class:class_send(SupCp, maxRestarts, []),
    ///     MaxT = beamtalk_object_class:class_send(SupCp, restartWindow, []),
    ///     SupFlags = #{strategy => simple_one_for_one, intensity => MaxR, period => MaxT},
    ///     [Spec] = beamtalk_supervisor:build_child_specs([ChildClass]),
    ///     {ok, {SupFlags, [Spec]}}.
    /// ```
    fn generate_dynamic_sup_init(class_name: &str) -> Document<'static> {
        docvec![
            "'init'/1 = fun (_Args) ->",
            "\n    let SupCp = call 'beamtalk_class_registry':'whereis_class'('",
            Document::String(class_name.to_string()),
            "') in",
            "\n    let SupChildClass = call 'beamtalk_object_class':'class_send'(SupCp, 'childClass', []) in",
            "\n    let SupMaxR = call 'beamtalk_object_class':'class_send'(SupCp, 'maxRestarts', []) in",
            "\n    let SupMaxT = call 'beamtalk_object_class':'class_send'(SupCp, 'restartWindow', []) in",
            "\n    let SupFlags = ~{'strategy' => 'simple_one_for_one', 'intensity' => SupMaxR, 'period' => SupMaxT}~ in",
            "\n    let SupSpecs = call 'beamtalk_supervisor':'build_child_specs'([SupChildClass]) in",
            "\n    {'ok', {SupFlags, SupSpecs}}",
        ]
    }

    /// Generates `childClass/0` for dynamic supervisors.
    ///
    /// Called directly by `beamtalk_supervisor:startChild/1,2` as `SupMod:'childClass'()`
    /// to resolve the child class object at runtime (see `beamtalk_supervisor.erl:217`).
    ///
    /// ```erlang
    /// childClass() ->
    ///     SupCp = beamtalk_class_registry:whereis_class('WorkerPool'),
    ///     beamtalk_object_class:class_send(SupCp, childClass, []).
    /// ```
    fn generate_dynamic_child_class(class_name: &str) -> Document<'static> {
        docvec![
            "'childClass'/0 = fun () ->",
            "\n    let SupCp = call 'beamtalk_class_registry':'whereis_class'('",
            Document::String(class_name.to_string()),
            "') in",
            "\n    call 'beamtalk_object_class':'class_send'(SupCp, 'childClass', [])",
        ]
    }
}
