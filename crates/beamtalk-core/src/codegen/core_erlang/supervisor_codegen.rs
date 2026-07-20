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

use std::cell::RefCell;
use std::collections::HashSet;

use super::document::Document;
use super::document::leaf::{atom, fname};
use super::selector_mangler::safe_class_method_fn_name;
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
        self.set_class_identity(Some(ClassIdentity::new(&class.name.name)));
        // BT-2710 follow-up: record field declared types so `self.<field>`
        // comparisons/arithmetic on object-typed fields dispatch here too,
        // consistent with value-type and actor modules.
        self.set_class_field_types(&class.state);

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
                ", ",
                fname(
                    safe_class_method_fn_name(&m.selector.to_erlang_atom()),
                    arity
                ),
            ];
        }

        // Module attributes
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);
        let file_attr = self.file_attr();
        let source_path_attr = self.source_path_attr();
        // BT-2909/BT-2932: use the generator's cross-module-aware alias
        // registry (this module's own `type_aliases` merged with any
        // pre-loaded aliases from other modules in the same compilation
        // unit — see `CoreErlangGenerator::alias_registry`'s doc) so an
        // alias-named annotation resolves to a `user_type` reference (ADR
        // 0108) instead of falling through to `any()`, regardless of which
        // module declared the alias.
        // BT-2940: tracks which alias names the specs below actually
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
        // BT-2909: every class module that could contain a `user_type`
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
            " ['start_link'/0, 'init'/1",
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

        // start_link/0
        docs.push(Self::generate_sup_start_link(&module_name));
        docs.push(Document::Str("\n\n"));

        // init/1 for static supervisor
        docs.push(Self::generate_static_sup_init(&class_name, &module_name));
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
                ", ",
                fname(
                    safe_class_method_fn_name(&m.selector.to_erlang_atom()),
                    arity
                ),
            ];
        }

        // Module attributes
        let beamtalk_class_attr = super::util::beamtalk_class_attribute(&module.classes);
        let file_attr = self.file_attr();
        let source_path_attr = self.source_path_attr();
        // BT-2909/BT-2932: use the generator's cross-module-aware alias
        // registry (this module's own `type_aliases` merged with any
        // pre-loaded aliases from other modules in the same compilation
        // unit — see `CoreErlangGenerator::alias_registry`'s doc) so an
        // alias-named annotation resolves to a `user_type` reference (ADR
        // 0108) instead of falling through to `any()`, regardless of which
        // module declared the alias.
        // BT-2940: tracks which alias names the specs below actually
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
        // BT-2909: every class module that could contain a `user_type`
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
        // BT-1220: childClass/0 is called directly by beamtalk_supervisor:startChild/1,2
        // (SupMod:'childClass'() at `beamtalk_supervisor.erl (startChild/1,2)`)
        docs.push(docvec![
            "module ",
            atom(module_name.to_string()),
            " ['start_link'/0, 'init'/1, 'childClass'/0",
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

        // start_link/0
        docs.push(Self::generate_sup_start_link(&module_name));
        docs.push(Document::Str("\n\n"));

        // init/1 for dynamic supervisor
        docs.push(Self::generate_dynamic_sup_init(&class_name, &module_name));
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
            "\n    call 'supervisor':'start_link'({'local', ",
            atom(module_name.to_string()),
            "}, ",
            atom(module_name.to_string()),
            ", [])",
        ]
    }

    /// Generates `init/1` for a static supervisor (`Supervisor subclass:`).
    ///
    /// Delegates to `beamtalk_supervisor:static_init/2` which calls class module
    /// functions directly (bypassing the class `gen_server`) to avoid the deadlock
    /// that would occur if `init/1` used `class_send` — the class `gen_server` is
    /// blocked waiting for `supervisor:start_link` to return.
    ///
    /// ```erlang
    /// init([]) ->
    ///     beamtalk_supervisor:static_init('bt@webapp', 'WebApp').
    /// ```
    fn generate_static_sup_init(class_name: &str, module_name: &str) -> Document<'static> {
        docvec![
            "'init'/1 = fun (_Args) ->",
            "\n    call 'beamtalk_supervisor':'static_init'(",
            atom(module_name.to_string()),
            ", ",
            atom(class_name.to_string()),
            ")",
        ]
    }

    /// Generates `init/1` for a dynamic supervisor (`DynamicSupervisor subclass:`).
    ///
    /// Delegates to `beamtalk_supervisor:dynamic_init/2` which calls class module
    /// functions directly (bypassing the class `gen_server`) to avoid the deadlock
    /// that would occur if `init/1` used `class_send`.
    ///
    /// ```erlang
    /// init([]) ->
    ///     beamtalk_supervisor:dynamic_init('bt@workerpool', 'WorkerPool').
    /// ```
    fn generate_dynamic_sup_init(class_name: &str, module_name: &str) -> Document<'static> {
        docvec![
            "'init'/1 = fun (_Args) ->",
            "\n    call 'beamtalk_supervisor':'dynamic_init'(",
            atom(module_name.to_string()),
            ", ",
            atom(class_name.to_string()),
            ")",
        ]
    }

    /// Generates `childClass/0` for dynamic supervisors.
    ///
    /// Called directly by `beamtalk_supervisor:startChild/1,2` as `SupMod:'childClass'()`
    /// to resolve the child class object at runtime (see `beamtalk_supervisor.erl (startChild/1,2)`).
    ///
    /// ```erlang
    /// childClass() ->
    ///     SupCp = beamtalk_class_registry:whereis_class('WorkerPool'),
    ///     beamtalk_object_class:class_send(SupCp, childClass, []).
    /// ```
    fn generate_dynamic_child_class(class_name: &str) -> Document<'static> {
        docvec![
            "'childClass'/0 = fun () ->",
            "\n    let SupCp = call 'beamtalk_class_registry':'whereis_class'(",
            atom(class_name.to_string()),
            ") in",
            "\n    call 'beamtalk_object_class':'class_send'(SupCp, 'childClass', [])",
        ]
    }
}
