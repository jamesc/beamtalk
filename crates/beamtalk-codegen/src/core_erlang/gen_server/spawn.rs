// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor spawn and instantiation error code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates `spawn/0`, `spawn/1` class methods and `new/0`, `new/1` error
//! methods that prevent incorrect actor instantiation.

use super::super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::Module;

/// Which family of per-actor-module spawn entry points to generate or call.
///
/// `Plain` is `spawn/0,1` (`spawn` / `spawnWith:`); `Monitored` is
/// `spawn_monitored/0,1` (`spawnMonitored` / `spawnMonitoredWith:`, BT-4104),
/// which answers `{Actor, MonitorRef}` with the monitor created atomically
/// with the process (`beamtalk_actor:safe_spawn_monitored/2`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum SpawnVariant {
    Plain,
    Monitored,
}

impl SpawnVariant {
    /// The variant an anonymous class-side spawn selector belongs to, or
    /// `None` when the selector/arity is not one.
    pub(in crate::core_erlang) fn of_selector(selector: &str, arity: usize) -> Option<Self> {
        match (selector, arity) {
            ("spawn", 0) | ("spawnWith:", 1) => Some(Self::Plain),
            ("spawnMonitored", 0) | ("spawnMonitoredWith:", 1) => Some(Self::Monitored),
            _ => None,
        }
    }

    /// The exported per-module function name.
    pub(in crate::core_erlang) fn function_name(self) -> &'static str {
        match self {
            Self::Plain => "spawn",
            Self::Monitored => "spawn_monitored",
        }
    }

    /// The `beamtalk_class_instantiation` helper a class-method self-send
    /// lowers to.
    pub(in crate::core_erlang) fn class_self_helper(self) -> &'static str {
        match self {
            Self::Plain => "class_self_spawn",
            Self::Monitored => "class_self_spawn_monitored",
        }
    }

    /// The public selector of the no-args form.
    fn selector(self) -> &'static str {
        match self {
            Self::Plain => "spawn",
            Self::Monitored => "spawnMonitored",
        }
    }

    /// The public selector of the init-args form.
    fn with_args_selector(self) -> &'static str {
        match self {
            Self::Plain => "spawnWith:",
            Self::Monitored => "spawnMonitoredWith:",
        }
    }

    /// The `beamtalk_actor` start helper.
    fn runtime_function(self) -> &'static str {
        match self {
            Self::Plain => "safe_spawn",
            Self::Monitored => "safe_spawn_monitored",
        }
    }

    /// The start helper's success pattern.
    fn ok_pattern(self) -> &'static str {
        match self {
            Self::Plain => "<{'ok', Pid}> when 'true' ->",
            Self::Monitored => "<{'ok', {Pid, MonRef}}> when 'true' ->",
        }
    }

    /// The head of an always-raising stub for this variant's no-args
    /// (`with_args` false) or init-args function, with the public selector
    /// its error names.
    fn stub_head(self, with_args: bool) -> (Document<'static>, &'static str) {
        if with_args {
            (
                docvec![
                    leaf::fname(self.function_name(), 1),
                    " = fun (_InitArgs) ->"
                ],
                self.with_args_selector(),
            )
        } else {
            (
                docvec![leaf::fname(self.function_name(), 0), " = fun () ->"],
                self.selector(),
            )
        }
    }

    /// The generated function's answer for the actor object `obj`.
    fn answer(self, obj: Document<'static>) -> Document<'static> {
        match self {
            Self::Plain => obj,
            Self::Monitored => docvec!["{", obj, ", MonRef}"],
        }
    }
}

impl CoreErlangGenerator {
    /// Generates a Document for registering an actor instance with the hot reload
    /// tracking registry. Uses try-catch to handle cases where the registry isn't
    /// running (e.g., stdlib tests).
    pub(super) fn instance_registration_doc(class_name: &str) -> Document<'static> {
        docvec![
            docvec![
                "let _InstReg = try call 'beamtalk_object_instances':'register'(",
                leaf::atom(class_name.to_string()),
                ", Pid)",
            ],
            nest(
                INDENT,
                docvec![
                    line(),
                    "of _RegOk -> _RegOk",
                    line(),
                    "catch <_RegT, _RegE, _RegS> -> 'ok'",
                ]
            ),
            line(),
            "in",
        ]
    }

    /// The `#beamtalk_object{}` a successful spawn of this class answers for
    /// the started `Pid`.
    fn spawned_object_doc(&mut self) -> Document<'static> {
        docvec![
            "{'beamtalk_object', ",
            leaf::atom(self.class_name()),
            ", ",
            leaf::atom(self.module_name.clone()),
            ", Pid}",
        ]
    }

    /// The shared start-failure arm for `spawn/0,1` and
    /// `spawn_monitored/0,1` (`selector` unquoted, e.g. `"spawnWith:"`).
    ///
    /// BT-4030: `init/1`'s own `#unknown_field` rejection passes through
    /// unwrapped; every other start failure keeps the `instantiation_error`
    /// wrap as before (e.g. a throwing `initialize` — see
    /// `actor_initialize_test.bt`).
    fn spawn_error_arm_doc(class_name: &str, selector: &'static str) -> Document<'static> {
        docvec![
            "<{'error', Reason}> when 'true' ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "let InitErr = call 'beamtalk_error':'extract_beamtalk_error'(Reason, 4) in",
                    line(),
                    "case call 'beamtalk_error':'is_unknown_field'(InitErr) of",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<'true'> when 'true' -> call 'beamtalk_error':'raise'(InitErr)",
                            line(),
                            "<'false'> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    // Include the actual error reason in the hint
                                    super::super::errors::beamtalk_error_doc(
                                        Document::Str("SpawnErr0"),
                                        Document::Str("SpawnErr0"),
                                        Document::Str("SpawnErr1"),
                                        Document::Str("SpawnErr1"),
                                        Document::Str("SpawnErr2"),
                                        "instantiation_error",
                                        leaf::atom(class_name.to_owned()),
                                        leaf::atom(selector),
                                        Document::Str("Reason"),
                                        line(),
                                    ),
                                    "call 'beamtalk_error':'raise'(SpawnErr2)",
                                ]
                            ),
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
        ]
    }

    /// Generates the `spawn/0` class method for creating actor instances.
    ///
    /// This is a class-level method (not an instance method) that instantiates
    /// a new actor process. The function:
    /// 1. Calls `gen_server:start_link/3` with an empty init args map
    /// 2. Wraps the pid in a `#beamtalk_object{}` record with class metadata
    /// 3. Returns the object record, or throws error on failure
    ///
    /// `actor.bt` separately declares a real `class sealed spawn`
    /// body (`(Erlang beamtalk_actor) doSpawn: self`) as the documented,
    /// xref-visible definition of dynamic-dispatch `spawn` — but this
    /// per-actor-module compiled function remains the one every dispatch
    /// path actually calls: the compiler's static `Counter spawn` fast path
    /// (`try_handle_spawn_await`), the class `gen_server`'s `{spawn, Args}`
    /// handler (`beamtalk_class_instantiation:handle_spawn/4`, via
    /// `erlang:apply(Module, spawn, Args)`), and `self spawn` in a class
    /// method (`class_self_spawn`) all call *this* function, never the
    /// inherited class-method body. Same "keeps the compiled artifact as the
    /// real definition" choice made for `new`/`new:` — see
    /// `beamtalk_actor:doSpawn/1`'s doc for the rationale.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/0 = fun () ->
    ///     case call 'gen_server':'start_link'('counter', ~{}~, []) of
    ///         <{'ok', Pid}> when 'true' ->
    ///             {'beamtalk_object', 'Counter', 'counter', Pid};
    ///         <{'error', Reason}> when 'true' ->
    ///             %% BT-4030: init/1's own unknown_field re-raised;
    ///             %% everything else wraps as instantiation_error
    ///             let SpawnErr0 = call 'beamtalk_error':'new'('instantiation_error', 'Counter') in
    ///             let SpawnErr1 = call 'beamtalk_error':'with_selector'(SpawnErr0, 'spawn') in
    ///             call 'beamtalk_error':'raise'(SpawnErr1)
    ///     end
    /// ```
    ///
    /// `SpawnVariant::Monitored` generates `spawn_monitored/0` (BT-4104)
    /// the same way over `beamtalk_actor:safe_spawn_monitored/2`, answering
    /// `{ActorObject, MonRef}`.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_spawn_function(
        &mut self,
        _module: &Module,
        variant: SpawnVariant,
    ) -> Result<Document<'static>> {
        // initialize is now called inside init/1, not here.
        // This ensures all spawn paths (direct, supervised, named) run initialize.
        let class_name = self.class_name();
        let module_name = self.module_name.clone();

        let ok_body = variant.answer(self.spawned_object_doc());

        // Use safe_spawn which handles trap_exit + await_initialize
        let doc = docvec![
            leaf::fname(variant.function_name(), 0),
            " = fun () ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "case call 'beamtalk_actor':",
                        leaf::atom(variant.runtime_function()),
                        "(",
                        leaf::atom(module_name.clone()),
                        ", ~{}~) of",
                    ],
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            variant.ok_pattern(),
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    // Register instance for hot reload tracking
                                    Self::instance_registration_doc(&class_name),
                                    line(),
                                    ok_body,
                                ]
                            ),
                            line(),
                            Self::spawn_error_arm_doc(&class_name, variant.selector()),
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `spawn/1` class method for creating actor instances with init args.
    ///
    /// This is a class-level method that instantiates a new actor process
    /// with initialization arguments passed to `init/1`. The function:
    /// 1. Calls `gen_server:start_link/3` with the provided args
    /// 2. Wraps the pid in a `#beamtalk_object{}` record with class metadata
    /// 3. Returns the object record, or throws error on failure
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/1 = fun (InitArgs) ->
    ///     case call 'erlang':'is_map'(InitArgs) of
    ///       <'false'> when 'true' ->
    ///         %% BT-473: type_error for non-map arguments
    ///       <'true'> when 'true' ->
    ///         case call 'gen_server':'start_link'('counter', InitArgs, []) of
    ///             <{'ok', Pid}> when 'true' ->
    ///                 {'beamtalk_object', 'Counter', 'counter', Pid};
    ///             <{'error', Reason}> when 'true' ->
    ///                 %% BT-4030: init/1's own unknown_field re-raised;
    ///                 %% everything else wraps as instantiation_error
    ///         end
    ///     end
    /// ```
    ///
    /// `SpawnVariant::Monitored` generates `spawn_monitored/1` (BT-4104)
    /// the same way over `beamtalk_actor:safe_spawn_monitored/2`, answering
    /// `{ActorObject, MonRef}`.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_spawn_with_args_function(
        &mut self,
        _module: &Module,
        variant: SpawnVariant,
    ) -> Result<Document<'static>> {
        // initialize is now called inside init/1, not here.
        let class_name = self.class_name();
        let module_name = self.module_name.clone();
        let selector = variant.with_args_selector();

        let ok_body = variant.answer(self.spawned_object_doc());

        // Validate InitArgs is a map before passing to gen_server
        // This is the single source of truth for spawnWith: argument validation.
        // The runtime (beamtalk_object_class.erl handle_call({spawn, Args})) delegates
        // validation to this generated code for both static and dynamic dispatch paths.
        // (BT-4030's unknown-key check lives in init/1 instead, where the
        // metadata-resolved declared set — own, inherited, and late — is
        // available; the start-failure arm below re-raises it unwrapped.)
        let doc = docvec![
            leaf::fname(variant.function_name(), 1),
            " = fun (InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "case call 'erlang':'is_map'(InitArgs) of",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<'false'> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    super::super::errors::beamtalk_error_doc(
                                        Document::Str("TypeErr0"),
                                        Document::Str("TypeErr0"),
                                        Document::Str("TypeErr1"),
                                        Document::Str("TypeErr1"),
                                        Document::Str("TypeErr2"),
                                        "type_error",
                                        leaf::atom(class_name.clone()),
                                        leaf::atom(selector),
                                        leaf::binary_lit(format!(
                                            "{selector} expects a Dictionary argument"
                                        )),
                                        line(),
                                    ),
                                    "call 'beamtalk_error':'raise'(TypeErr2)",
                                ]
                            ),
                            line(),
                            // Use safe_spawn which handles trap_exit + await_initialize
                            "<'true'> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    docvec![
                                        "case call 'beamtalk_actor':",
                                        leaf::atom(variant.runtime_function()),
                                        "(",
                                        leaf::atom(module_name.clone()),
                                        ", InitArgs) of",
                                    ],
                                    nest(
                                        INDENT,
                                        docvec![
                                            line(),
                                            variant.ok_pattern(),
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    // Register instance for hot reload tracking
                                                    Self::instance_registration_doc(&class_name),
                                                    line(),
                                                    ok_body,
                                                ]
                                            ),
                                            line(),
                                            Self::spawn_error_arm_doc(&class_name, selector),
                                        ]
                                    ),
                                    line(),
                                    "end",
                                ]
                            ),
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `new/0` error method for actors.
    ///
    /// Actors cannot be instantiated with `new` - they must use `spawn`.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_actor_new_error_method(
        &self,
    ) -> Result<Document<'static>> {
        Ok(Self::instantiation_error_stub(
            Document::Str("'new'/0 = fun () ->"),
            "Actor",
            "new",
            "Use spawn instead",
        ))
    }

    /// Generates the `new/1` error method for actors.
    ///
    /// Actors cannot be instantiated with `new:` - they must use `spawnWith:`.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_actor_new_with_args_error_method(
        &self,
    ) -> Result<Document<'static>> {
        Ok(Self::instantiation_error_stub(
            Document::Str("'new'/1 = fun (_InitArgs) ->"),
            "Actor",
            "new:",
            "Use spawnWith: instead",
        ))
    }

    /// Generates the `spawn/0` (or, `Monitored`, `spawn_monitored/0`) error
    /// method for abstract classes.
    ///
    /// Abstract classes cannot be instantiated — they must be subclassed first.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_abstract_spawn_error_method(
        &mut self,
        variant: SpawnVariant,
    ) -> Result<Document<'static>> {
        let (fun_decl, selector) = variant.stub_head(false);
        Ok(Self::instantiation_error_stub(
            fun_decl,
            self.class_name(),
            selector,
            "Abstract classes cannot be instantiated. Subclass it first.",
        ))
    }

    /// Generates the `spawn/1` (or, `Monitored`, `spawn_monitored/1`) error
    /// method for abstract classes.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_abstract_spawn_with_args_error_method(
        &mut self,
        variant: SpawnVariant,
    ) -> Result<Document<'static>> {
        let (fun_decl, selector) = variant.stub_head(true);
        Ok(Self::instantiation_error_stub(
            fun_decl,
            self.class_name(),
            selector,
            "Abstract classes cannot be instantiated. Subclass it first.",
        ))
    }

    /// Generates `spawn_monitored/0` (`with_args` false) or
    /// `spawn_monitored/1` for a module that cannot start atomically
    /// monitored — a `native:` facade, whose backing module owns its own
    /// start (BT-4104). Raises `instantiation_error` rather than leaving the
    /// export missing (`undef`).
    pub(in crate::core_erlang) fn generate_unsupported_spawn_monitored_method(
        &mut self,
        with_args: bool,
    ) -> Document<'static> {
        let (fun_decl, selector) = SpawnVariant::Monitored.stub_head(with_args);
        Self::instantiation_error_stub(
            fun_decl,
            self.class_name(),
            selector,
            "Native actors cannot be spawned monitored. Use spawn, then monitor.",
        )
    }

    /// Builds the 4-step `instantiation_error` let-chain expression that always raises.
    ///
    /// Shared by all four actor/abstract instantiation guard *functions* (via
    /// [`Self::instantiation_error_stub`]) — the per-actor-module compiled
    /// `new/0`/`new/1` stubs still required by `handle_new_compiled`'s
    /// `erlang:apply(Module, new, Args)` fast path, which bypasses
    /// class-method dispatch entirely.
    ///
    /// actor.bt's own `class sealed new` / `class sealed
    /// new:` declarations used to compile through this same helper (via an
    /// `@intrinsic` marker), guaranteeing the two could never drift. They now
    /// send `Exception signalKind:class:selector:hint:` instead — a plain
    /// Beamtalk expression, not an intrinsic — so they are a *separately
    /// written* equivalent of this raise, not compiler-enforced to match. If
    /// the hint text or kind here ever changes, update actor.bt's `new`/
    /// `new:` bodies to match by hand (low risk: `actor.bt`'s declared bodies
    /// are unreachable via any current dispatch path — see the doc comments
    /// on those methods).
    /// Generates just the expression, no `fun` wrapper:
    /// ```text
    /// let Error0 = call 'beamtalk_error':'new'('instantiation_error', '<class>') in
    /// let Error1 = call 'beamtalk_error':'with_selector'(Error0, '<selector>') in
    /// let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"hint">>) in
    /// call 'beamtalk_error':'raise'(Error2)
    /// ```
    pub(in crate::core_erlang) fn instantiation_error_expr(
        class_name: impl Into<String>,
        selector: impl Into<String>,
        hint: &str,
    ) -> Document<'static> {
        docvec![
            super::super::errors::beamtalk_error_doc(
                Document::Str("Error0"),
                Document::Str("Error0"),
                Document::Str("Error1"),
                Document::Str("Error1"),
                Document::Str("Error2"),
                "instantiation_error",
                leaf::atom(class_name),
                leaf::atom(selector),
                leaf::binary_lit(hint),
                line(),
            ),
            "call 'beamtalk_error':'raise'(Error2)",
        ]
    }

    /// Builds the 4-step `instantiation_error` let-chain for a stub method that always raises.
    ///
    /// Shared by all four actor/abstract instantiation guard methods. Generates:
    /// ```text
    /// '<method>'/N = fun (...) ->
    ///     let Error0 = call 'beamtalk_error':'new'('instantiation_error', '<class>') in
    ///     let Error1 = call 'beamtalk_error':'with_selector'(Error0, '<selector>') in
    ///     let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"hint">>) in
    ///     call 'beamtalk_error':'raise'(Error2)
    /// ```
    fn instantiation_error_stub(
        fun_decl: Document<'static>,
        class_name: impl Into<String>,
        selector: impl Into<String>,
        hint: &str,
    ) -> Document<'static> {
        docvec![
            fun_decl,
            nest(
                INDENT,
                docvec![
                    line(),
                    Self::instantiation_error_expr(class_name, selector, hint)
                ]
            ),
            "\n",
        ]
    }

    /// Generates the `superclass/0` class method for reflection.
    ///
    /// Returns the superclass name as an atom, or `'nil'` for root classes.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'superclass'/0 = fun () -> 'Actor'
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_superclass_function(
        &self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let superclass_atom = module
            .classes
            .first()
            .and_then(|c| c.superclass.as_ref())
            .map_or("nil", |s| s.name.as_str());

        let doc = docvec![
            docvec![
                "'superclass'/0 = fun () -> ",
                leaf::atom(superclass_atom.to_string()),
            ],
            "\n",
            "\n",
        ];
        Ok(doc)
    }
}

#[cfg(test)]
mod tests {
    use super::SpawnVariant;
    use crate::core_erlang::CoreErlangGenerator;
    use beamtalk_core::ast::{ClassDefinition, ClassKind, CommentAttachment, Identifier, Module};
    use beamtalk_core::source_analysis::Span;

    fn generator() -> CoreErlangGenerator {
        CoreErlangGenerator::new("counter")
    }

    fn empty_module() -> Module {
        Module {
            classes: vec![],
            method_definitions: vec![],
            protocols: vec![],
            type_aliases: vec![],
            native_declarations: vec![],
            expressions: vec![],
            span: Span::new(0, 0),
            file_leading_comments: vec![],
            file_trailing_comments: vec![],
        }
    }

    fn module_with_superclass(sc: &str) -> Module {
        let class = ClassDefinition {
            name: Identifier::new("Counter", Span::new(0, 0)),
            superclass: Some(Identifier::new(sc, Span::new(0, 0))),
            superclass_package: None,
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            is_internal: false,
            supervisor_kind: None,
            state: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            uses: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            handle_scope: None,
            shape_version: None,
            span: Span::new(0, 0),
        };
        Module {
            classes: vec![class],
            method_definitions: vec![],
            protocols: vec![],
            type_aliases: vec![],
            native_declarations: vec![],
            expressions: vec![],
            span: Span::new(0, 0),
            file_leading_comments: vec![],
            file_trailing_comments: vec![],
        }
    }

    #[test]
    fn instance_registration_doc_contains_register_call() {
        let doc = CoreErlangGenerator::instance_registration_doc("counter");
        let out = doc.to_pretty_string();
        assert!(
            out.contains("beamtalk_object_instances"),
            "expected registry module: {out}"
        );
        assert!(out.contains("register"), "expected 'register': {out}");
        assert!(out.contains("Pid"), "expected Pid variable: {out}");
    }

    #[test]
    fn instance_registration_doc_contains_try_catch() {
        let doc = CoreErlangGenerator::instance_registration_doc("counter");
        let out = doc.to_pretty_string();
        assert!(out.contains("try"), "expected try keyword: {out}");
        assert!(out.contains("catch"), "expected catch clause: {out}");
    }

    #[test]
    fn generate_spawn_function_declares_spawn_0() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'spawn'/0 = fun ()"),
            "expected spawn/0 decl: {out}"
        );
    }

    #[test]
    fn generate_spawn_function_uses_safe_spawn() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("beamtalk_actor"),
            "expected beamtalk_actor call: {out}"
        );
        assert!(out.contains("safe_spawn"), "expected safe_spawn: {out}");
    }

    #[test]
    fn generate_spawn_function_ok_arm_returns_beamtalk_object() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("beamtalk_object"),
            "expected beamtalk_object record: {out}"
        );
        assert!(out.contains("'Counter'"), "expected class atom: {out}");
        assert!(out.contains("'counter'"), "expected module atom: {out}");
        assert!(out.contains("Pid"), "expected Pid: {out}");
    }

    #[test]
    fn generate_spawn_function_error_arm_raises_instantiation_error() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("instantiation_error"),
            "expected instantiation_error: {out}"
        );
        assert!(out.contains("raise"), "expected raise call: {out}");
    }

    #[test]
    fn generate_spawn_function_includes_instance_registration() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("beamtalk_object_instances"),
            "expected instance registration: {out}"
        );
    }

    #[test]
    fn generate_spawn_with_args_validates_is_map() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_with_args_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'spawn'/1 = fun (InitArgs)"),
            "expected spawn/1 decl: {out}"
        );
        assert!(out.contains("is_map"), "expected is_map check: {out}");
    }

    #[test]
    fn generate_spawn_with_args_reraises_init_unknown_field() {
        // BT-4030: init/1's own unknown_field rejection passes through
        // unwrapped; every other start failure keeps the
        // instantiation_error wrap.
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_with_args_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("call 'beamtalk_error':'extract_beamtalk_error'(Reason, 4)"),
            "expected extract_beamtalk_error call: {out}"
        );
        assert!(
            out.contains("call 'beamtalk_error':'is_unknown_field'(InitErr)"),
            "expected is_unknown_field dispatch: {out}"
        );
        assert!(
            out.contains("<'true'> when 'true' -> call 'beamtalk_error':'raise'(InitErr)"),
            "expected transparent unknown_field re-raise: {out}"
        );
        assert!(
            out.contains("instantiation_error"),
            "other failures must keep the instantiation_error wrap: {out}"
        );
    }

    #[test]
    fn generate_spawn_no_args_reraises_init_unknown_field() {
        // BT-4030: spawn/0 shares spawn/1's start-failure arm — init/1's
        // unknown_field passes through unwrapped there too.
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("call 'beamtalk_error':'extract_beamtalk_error'(Reason, 4)"),
            "expected extract_beamtalk_error call: {out}"
        );
        assert!(
            out.contains("<'true'> when 'true' -> call 'beamtalk_error':'raise'(InitErr)"),
            "expected transparent unknown_field re-raise: {out}"
        );
        assert!(
            out.contains("instantiation_error"),
            "other failures must keep the instantiation_error wrap: {out}"
        );
    }

    #[test]
    fn generate_spawn_with_args_false_branch_raises_type_error() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_with_args_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(out.contains("type_error"), "expected type_error: {out}");
        assert!(out.contains("spawnWith:"), "expected selector: {out}");
    }

    #[test]
    fn generate_spawn_with_args_ok_arm_registers_and_returns_object() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_with_args_function(&m, SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("beamtalk_object"),
            "expected beamtalk_object: {out}"
        );
        assert!(
            out.contains("beamtalk_object_instances"),
            "expected registration: {out}"
        );
    }

    #[test]
    fn generate_actor_new_error_method_raises_instantiation_error() {
        let g = generator();
        let out = g
            .generate_actor_new_error_method()
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'new'/0 = fun ()"),
            "expected new/0 decl: {out}"
        );
        assert!(
            out.contains("instantiation_error"),
            "expected instantiation_error: {out}"
        );
        assert!(out.contains("raise"), "expected raise: {out}");
    }

    #[test]
    fn generate_actor_new_with_args_error_method_raises_instantiation_error() {
        let g = generator();
        let out = g
            .generate_actor_new_with_args_error_method()
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'new'/1 = fun (_InitArgs)"),
            "expected new/1 decl: {out}"
        );
        assert!(
            out.contains("instantiation_error"),
            "expected instantiation_error: {out}"
        );
    }

    #[test]
    fn generate_abstract_spawn_error_method_raises_instantiation_error() {
        let mut g = generator();
        let out = g
            .generate_abstract_spawn_error_method(SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'spawn'/0 = fun ()"),
            "expected spawn/0 decl: {out}"
        );
        assert!(
            out.contains("instantiation_error"),
            "expected instantiation_error: {out}"
        );
        assert!(out.contains("with_hint"), "expected hint call: {out}");
    }

    #[test]
    fn generate_abstract_spawn_with_args_error_method_raises_instantiation_error() {
        let mut g = generator();
        let out = g
            .generate_abstract_spawn_with_args_error_method(SpawnVariant::Plain)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'spawn'/1 = fun (_InitArgs)"),
            "expected spawn/1 decl: {out}"
        );
        assert!(
            out.contains("instantiation_error"),
            "expected instantiation_error: {out}"
        );
    }

    // === BT-4104: spawn_monitored/0,1 ===

    #[test]
    fn generate_spawn_monitored_0_uses_safe_spawn_monitored_and_answers_pair() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_function(&m, SpawnVariant::Monitored)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'spawn_monitored'/0 = fun ()"),
            "expected spawn_monitored/0 decl: {out}"
        );
        assert!(
            out.contains("call 'beamtalk_actor':'safe_spawn_monitored'('counter', ~{}~)"),
            "expected safe_spawn_monitored: {out}"
        );
        assert!(
            out.contains("<{'ok', {Pid, MonRef}}> when 'true' ->"),
            "expected the {{Pid, MonRef}} success pattern: {out}"
        );
        assert!(
            out.contains("{{'beamtalk_object', 'Counter', 'counter', Pid}, MonRef}"),
            "expected the {{Actor, MonRef}} answer: {out}"
        );
        assert!(
            out.contains("beamtalk_object_instances"),
            "expected hot-reload instance registration: {out}"
        );
        assert!(
            out.contains("'spawnMonitored'"),
            "start failures must name spawnMonitored: {out}"
        );
        assert!(
            out.contains("instantiation_error"),
            "expected instantiation_error wrap: {out}"
        );
    }

    #[test]
    fn generate_spawn_monitored_1_validates_map_and_names_selector() {
        let mut g = generator();
        let m = empty_module();
        let out = g
            .generate_spawn_with_args_function(&m, SpawnVariant::Monitored)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'spawn_monitored'/1 = fun (InitArgs)"),
            "expected spawn_monitored/1 decl: {out}"
        );
        assert!(out.contains("is_map"), "expected is_map check: {out}");
        assert!(
            out.contains(&beamtalk_cerl_doc::binary::binary_string_literal(
                "spawnMonitoredWith: expects a Dictionary argument"
            )),
            "expected the monitored type_error hint: {out}"
        );
        assert!(
            out.contains("call 'beamtalk_actor':'safe_spawn_monitored'('counter', InitArgs)"),
            "expected safe_spawn_monitored: {out}"
        );
        assert!(
            out.contains("<'true'> when 'true' -> call 'beamtalk_error':'raise'(InitErr)"),
            "expected transparent unknown_field re-raise: {out}"
        );
        assert!(
            !out.contains("'spawnWith:'"),
            "must not name the plain selector: {out}"
        );
    }

    #[test]
    fn generate_abstract_spawn_monitored_stubs_raise() {
        let mut g = generator();
        let out0 = g
            .generate_abstract_spawn_error_method(SpawnVariant::Monitored)
            .unwrap()
            .to_pretty_string();
        assert!(
            out0.contains("'spawn_monitored'/0 = fun ()"),
            "expected spawn_monitored/0 stub: {out0}"
        );
        assert!(out0.contains("'spawnMonitored'"), "selector: {out0}");
        assert!(
            out0.contains(&beamtalk_cerl_doc::binary::binary_string_literal(
                "Abstract classes cannot be instantiated. Subclass it first."
            )),
            "hint: {out0}"
        );
        let out1 = g
            .generate_abstract_spawn_with_args_error_method(SpawnVariant::Monitored)
            .unwrap()
            .to_pretty_string();
        assert!(
            out1.contains("'spawn_monitored'/1 = fun (_InitArgs)"),
            "expected spawn_monitored/1 stub: {out1}"
        );
        assert!(out1.contains("'spawnMonitoredWith:'"), "selector: {out1}");
    }

    #[test]
    fn generate_unsupported_spawn_monitored_stubs_raise() {
        let mut g = generator();
        let out0 = g
            .generate_unsupported_spawn_monitored_method(false)
            .to_pretty_string();
        assert!(out0.contains("'spawn_monitored'/0 = fun ()"), "{out0}");
        assert!(out0.contains("instantiation_error"), "{out0}");
        assert!(
            out0.contains(&beamtalk_cerl_doc::binary::binary_string_literal(
                "Native actors cannot be spawned monitored. Use spawn, then monitor."
            )),
            "{out0}"
        );
        let out1 = g
            .generate_unsupported_spawn_monitored_method(true)
            .to_pretty_string();
        assert!(
            out1.contains("'spawn_monitored'/1 = fun (_InitArgs)"),
            "{out1}"
        );
        assert!(out1.contains("'spawnMonitoredWith:'"), "{out1}");
    }

    #[test]
    fn instantiation_error_expr_builds_error_chain() {
        let out =
            CoreErlangGenerator::instantiation_error_expr("MyClass", "new", "Use spawn instead")
                .to_pretty_string();
        assert!(
            out.contains("instantiation_error"),
            "expected error kind: {out}"
        );
        assert!(out.contains("'MyClass'"), "expected class atom: {out}");
        assert!(out.contains("'new'"), "expected selector atom: {out}");
        assert!(out.contains("with_hint"), "expected hint call: {out}");
        assert!(out.contains("raise"), "expected raise call: {out}");
    }

    #[test]
    fn generate_superclass_function_no_classes_returns_nil() {
        let g = generator();
        let m = empty_module();
        let out = g
            .generate_superclass_function(&m)
            .unwrap()
            .to_pretty_string();
        assert!(
            out.contains("'superclass'/0 = fun ()"),
            "expected superclass/0: {out}"
        );
        assert!(out.contains("'nil'"), "expected nil atom: {out}");
    }

    #[test]
    fn generate_superclass_function_returns_superclass_atom() {
        let g = generator();
        let m = module_with_superclass("Actor");
        let out = g
            .generate_superclass_function(&m)
            .unwrap()
            .to_pretty_string();
        assert!(out.contains("'Actor'"), "expected Actor atom: {out}");
    }
}
