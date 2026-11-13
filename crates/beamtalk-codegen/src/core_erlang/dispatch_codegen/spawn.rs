// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Spawn and instantiation sends.
//!
//! Lowers `spawn`/`spawnWith:`/`await` and the other instantiation intrinsics
//! sent to a class reference or to `self` in a class method, including
//! supervisor starts and named-registration spawns.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::gen_server::SpawnVariant;
use crate::core_erlang::{CoreErlangGenerator, Result};

use super::ClassSendArgs;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MessageSelector};

/// A named-registration spawn sent to `self` in a class method
/// (`spawnAs:` / `spawnWith:as:`), lowered to a
/// `beamtalk_class_instantiation` helper.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum NamedSpawn {
    /// `spawnAs:`.
    As,
    /// `spawnWith:as:`.
    WithAs,
}

impl NamedSpawn {
    /// Every form, for the runtime export contract
    /// (`runtime_export_contract_tests`). The exhaustive `match` makes a new
    /// form a compile error here until it is listed.
    #[cfg(test)]
    pub(in crate::core_erlang) const ALL: [Self; 2] = {
        match Self::As {
            Self::As | Self::WithAs => {}
        }
        [Self::As, Self::WithAs]
    };

    /// The `beamtalk_class_instantiation` helper it calls.
    pub(in crate::core_erlang) fn helper(self) -> &'static str {
        match self {
            Self::As => "class_self_spawn_as",
            Self::WithAs => "class_self_spawn_with",
        }
    }

    /// The selector, as the atom passed to the helper.
    pub(super) fn selector(self) -> &'static str {
        match self {
            Self::As => "spawnAs:",
            Self::WithAs => "spawnWith:as:",
        }
    }

    /// The prefix of the result variable.
    fn result_prefix(self) -> &'static str {
        match self {
            Self::As => "SpawnAsRes",
            Self::WithAs => "SpawnWithAsRes",
        }
    }
}

/// The call shape a spawn-family send to a class reference was lowered to,
/// passed to `beamtalk_class_instantiation:resolve_spawn_module_or_raise/3`
/// so a shadowed class resolved at call time is checked against it (BT-4404).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum SpawnTargetKind {
    /// `Module:spawn/0,1` or `Module:spawn_monitored/0,1`: needs an actor.
    Actor,
    /// `beamtalk_supervisor:class_start/4`: needs a supervisor.
    Supervisor,
}

impl SpawnTargetKind {
    /// The `Kind` atom the runtime helper takes.
    pub(super) fn runtime_atom(self) -> &'static str {
        match self {
            Self::Actor => "actor",
            Self::Supervisor => "supervisor",
        }
    }
}

/// The class-side spawn-family selectors that start a supervisor instance
/// (ADR 0135 §1), with their arities: lowered for supervisor receivers to
/// `beamtalk_supervisor:class_start/4` instead of the actor spawn intrinsics.
/// `spawnMonitored`/`spawnMonitoredWith:` (BT-4104) are here only so a
/// supervisor receiver reaches `class_start/4`'s clear unsupported error
/// rather than a `spawn_monitored` export supervisor modules do not have.
pub(in crate::core_erlang) fn is_supervisor_spawn_selector(
    selector_atom: &str,
    arity: usize,
) -> bool {
    matches!(
        (selector_atom, arity),
        ("spawn" | "spawnMonitored" | "spawnMonitoredAsync", 0)
            | (
                "spawnWith:" | "spawnAs:" | "spawnMonitoredWith:" | "spawnMonitoredAsyncWith:",
                1,
            )
            | ("spawnWith:as:", 2)
    )
}

impl CoreErlangGenerator {
    /// Handles spawn, spawnWith:, await, awaitForever, and await: intrinsics.
    ///
    /// Returns `Some(())` if the message was handled, `None` if it should
    /// fall through to the next dispatch strategy.
    pub(super) fn try_handle_spawn_await(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // ADR 0135 §1: the spawn family on a Supervisor/DynamicSupervisor
        // class reference starts the tree in the caller via the runtime
        // start helper (a supervisor module has no `spawn/0,1`).
        if let Expression::ClassReference { name, package, .. } = receiver {
            let selector_atom = selector.name();
            if is_supervisor_spawn_selector(&selector_atom, arguments.len())
                && self.is_supervisor_class(&name.name)
            {
                let pkg = package.as_ref().map(|p| p.name.as_str());
                let module_doc = self.class_reference_spawn_module_doc(
                    &name.name,
                    pkg,
                    &selector_atom,
                    SpawnTargetKind::Supervisor,
                );
                let doc = self.generate_supervisor_class_start(
                    leaf::atom(name.name.to_string()),
                    module_doc,
                    &selector_atom,
                    ClassSendArgs::Exprs(arguments),
                )?;
                return Ok(Some(doc));
            }
        }

        // `spawn`/`spawnWith:` and (BT-4104) `spawnMonitored`/
        // `spawnMonitoredWith:` on a class reference call the module's own
        // `spawn/0,1` / `spawn_monitored/0,1` in the calling process — so a
        // monitored spawn's monitor belongs to the sender. Only match
        // ClassReference, not Identifier.
        if let Expression::ClassReference { name, package, .. } = receiver
            && let Some(variant) = SpawnVariant::of_selector(&selector.name(), arguments.len())
        {
            let pkg = package.as_ref().map(|p| p.name.as_str());
            let doc = self.generate_actor_spawn_qualified(
                &name.name,
                pkg,
                &selector.name(),
                arguments.first(),
                variant,
            )?;
            return Ok(Some(doc));
        }

        // Unary await messages
        if let MessageSelector::Unary(name) = selector {
            if name == "await" && arguments.is_empty() {
                let doc = self.generate_await(receiver)?;
                return Ok(Some(doc));
            }
            if name == "awaitForever" && arguments.is_empty() {
                let doc = self.generate_await_forever(receiver)?;
                return Ok(Some(doc));
            }
        }

        // Keyword await: messages
        if let MessageSelector::Keyword(parts) = selector
            && parts.len() == 1
            && parts[0].keyword == "await:"
            && arguments.len() == 1
        {
            let doc = self.generate_await_with_timeout(receiver, &arguments[0])?;
            return Ok(Some(doc));
        }

        Ok(None)
    }

    /// Lowers instantiation-like selectors (`new`, `spawn`, `spawnAs:`, ...) into
    /// direct calls on `beamtalk_class_instantiation`, bypassing the class
    /// `gen_server` to avoid deadlock from within a class method.
    ///
    /// ClassName/Module create an instance of the CALLING class (the
    /// running class `gen_server` process) for inherited factory methods.
    ///
    /// ADR 0109 amendment: ClassName/Module/IsAbstract are derived from
    /// `ClassSelf` (closure-captured) rather than read from the process
    /// dictionary, so a block invoked from a *different* class's process still
    /// resolves against the block's own lexical class — preserving that intent
    /// rather than overriding it (`ClassSelf` already carries the same value the
    /// process dictionary did in every non-block case).
    pub(super) fn try_instantiation_intrinsic(
        &mut self,
        selector_atom: &str,
        arguments: ClassSendArgs<'_>,
    ) -> Result<Option<Document<'static>>> {
        // ADR 0135 §1: `self spawn…` in a class method of a supervisor class
        // starts a supervisor instance of the receiving class (a subclass
        // when inherited) in the calling process.
        if is_supervisor_spawn_selector(selector_atom, arguments.len())
            && self.is_supervisor_class(&self.class_name())
        {
            let doc = self.generate_supervisor_class_start(
                Self::class_self_name_doc(),
                Self::class_self_module_doc(selector_atom),
                selector_atom,
                arguments,
            )?;
            return Ok(Some(doc));
        }
        match selector_atom {
            "new" | "new:" => {
                // Hoist preambles from sub-expression class var mutations.
                let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;
                let call_doc = docvec![
                    "call 'beamtalk_class_instantiation':'class_self_new'(",
                    Self::class_self_name_doc(),
                    ", ",
                    Self::class_self_module_doc(selector_atom),
                    ", [",
                    args_doc,
                    "])"
                ];
                Ok(Some(self.close_prelude(&args_preamble, call_doc, "NewRes")))
            }
            // BT-4104: `spawnMonitored`/`spawnMonitoredWith:` lower the same
            // way to `class_self_spawn_monitored` — the monitor belongs to the
            // process running the class method (the class process).
            "spawn"
            | "spawnWith:"
            | "spawnMonitored"
            | "spawnMonitoredWith:"
            | "spawnMonitoredAsync"
            | "spawnMonitoredAsyncWith:" => {
                let Some(variant) = SpawnVariant::of_selector(selector_atom, arguments.len())
                else {
                    return Ok(None);
                };
                let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;
                let call_doc = docvec![
                    "call 'beamtalk_class_instantiation':",
                    leaf::atom(variant.class_self_helper()),
                    "(",
                    Self::class_self_name_doc(),
                    ", ",
                    Self::class_self_module_doc(selector_atom),
                    ", call 'beamtalk_class_instantiation':'resolve_is_abstract_or_raise'(",
                    Self::class_self_name_doc(),
                    ", ",
                    leaf::atom(selector_atom),
                    "), [",
                    args_doc,
                    "])"
                ];
                Ok(Some(self.close_prelude(
                    &args_preamble,
                    call_doc,
                    "SpawnRes",
                )))
            }
            // Named-registration spawn variants inherited from Actor.
            // Without these arms, the fallthrough in the caller emits
            // `call 'CURRENT_MODULE':'spawnAs' / 'spawnWithas'` — neither function
            // exists, so calls crash at runtime with `undef`.
            "spawnAs:" => Ok(Some(
                self.generate_class_self_named_spawn(NamedSpawn::As, arguments)?,
            )),
            "spawnWith:as:" => Ok(Some(
                self.generate_class_self_named_spawn(NamedSpawn::WithAs, arguments)?,
            )),
            _ => Ok(None),
        }
    }

    /// True when `class_name` is `Supervisor`, `DynamicSupervisor` or a
    /// subclass of either (ADR 0135 §1 carve-out from the actor spawn
    /// intrinsics).
    fn is_supervisor_class(&self, class_name: &str) -> bool {
        self.supervisor_classes.contains(class_name)
            || self.class_hierarchy.as_ref().is_some_and(|h| {
                h.is_supervisor_subclass(class_name) || h.is_dynamic_supervisor_subclass(class_name)
            })
    }

    /// True when the compiler can prove `class_name` is neither an actor nor
    /// a supervisor: it has a `ClassInfo` whose superclass chain is fully
    /// known and reaches neither `Actor` nor a supervisor base (BT-4443). An
    /// unknown class or an incomplete chain is not proof, matching the
    /// runtime's `beamtalk_supervisor:spawn_kind/1` treating `unknown` as
    /// "decides nothing". The hierarchy is keyed by bare name, so callers
    /// only ask this of an unqualified reference.
    fn is_known_non_spawnable_class(&self, class_name: &str) -> bool {
        !self.is_supervisor_class(class_name)
            && self.class_hierarchy.as_ref().is_some_and(|h| {
                h.has_class(class_name)
                    && !h.has_cross_file_parent(class_name)
                    && !h.is_actor_subclass(class_name)
            })
    }

    /// Whether a spawn-family send to the class reference `class_name`
    /// (optionally `package`-qualified) resolves the module it starts its
    /// process from at call time rather than pinning it (BT-4278).
    ///
    /// Normally the module is the compile-time one
    /// (`compiled_module_name_qualified`). An *unqualified* reference to a shadowed name
    /// ([`Self::is_shadowed_class`]) is one last-wins runtime class, so the
    /// compile-time package's module is not necessarily the one a dynamic
    /// send reaches: the module is resolved at call time from the class
    /// registry instead, through the same lookup helper as the class-side
    /// `self spawn` family ([`Self::class_module_lookup_doc`]) —
    ///
    /// ```erlang
    /// call 'beamtalk_class_instantiation':'resolve_spawn_module_or_raise'('Counter', 'spawn', 'actor')
    /// ```
    ///
    /// The trailing kind lets the runtime reject a winning class of a
    /// different kind (an actor where a supervisor start was compiled, a
    /// Value class, …) with a structured error (BT-4404).
    ///
    /// The spawn itself stays in the caller (a dynamic `class_send` would
    /// start it inside the class `gen_server`, and has no monitored-spawn
    /// clause). A package-qualified reference (`json@Counter spawn`) names
    /// its module explicitly and keeps it.
    ///
    /// BT-4443: an unqualified reference to a class the compiler knows is
    /// not spawnable ([`Self::is_known_non_spawnable_class`]) resolves at
    /// call time too. A pinned `Module:spawn/0` on a Value or Object class
    /// would fail with a raw `undef` (the type checker's class-side
    /// does-not-understand is only a lint, not a build error), whereas the
    /// lookup raises the same structured `does_not_understand` as the dynamic
    /// route (`beamtalk_class_instantiation:do_handle_spawn/5`) — and still
    /// spawns in the caller if the class has since been redefined as an
    /// actor.
    fn spawn_module_resolved_at_runtime(&self, class_name: &str, package: Option<&str>) -> bool {
        package.is_none()
            && (self.is_shadowed_class(class_name) || self.is_known_non_spawnable_class(class_name))
    }

    /// See [`Self::spawn_module_resolved_at_runtime`] for when the module is
    /// looked up at call time rather than pinned.
    ///
    /// `kind` is the call shape the caller emits around the module; the
    /// runtime lookup checks the winning class can take it (BT-4404).
    pub(super) fn class_reference_spawn_module_doc(
        &self,
        class_name: &str,
        package: Option<&str>,
        selector_atom: &str,
        kind: SpawnTargetKind,
    ) -> Document<'static> {
        if self.spawn_module_resolved_at_runtime(class_name, package) {
            Self::class_module_lookup_doc(
                leaf::atom(class_name.to_string()),
                selector_atom,
                Some(kind),
            )
        } else {
            leaf::atom(self.compiled_module_name_qualified(class_name, package))
        }
    }

    /// Lowers a spawn-family send on a supervisor class (ADR 0135 §1) to the
    /// runtime start helper, run in the caller's process — never class-method
    /// dispatch, which would run the whole tree start inside the class
    /// `gen_server`:
    ///
    /// ```erlang
    /// call 'beamtalk_supervisor':'class_start'('Pool', 'bt@pool', 'spawnWith:', [Args])
    /// ```
    fn generate_supervisor_class_start(
        &mut self,
        class_doc: Document<'static>,
        module_doc: Document<'static>,
        selector_atom: &str,
        arguments: ClassSendArgs<'_>,
    ) -> Result<Document<'static>> {
        let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;
        let call_doc = docvec![
            "call 'beamtalk_supervisor':'class_start'(",
            class_doc,
            ", ",
            module_doc,
            ", ",
            leaf::atom(selector_atom.to_string()),
            ", [",
            args_doc,
            "])"
        ];
        Ok(self.close_prelude(&args_preamble, call_doc, "SupStart"))
    }

    /// Shared emitter for `self spawnAs:` and `self spawnWith:as:` in
    /// class-method context. Emits a call to `beamtalk_class_instantiation`'s
    /// Result-returning helper with ClassName/Module/IsAbstract derived from
    /// `ClassSelf` (ADR 0109 amendment — see `try_instantiation_intrinsic`),
    /// followed by the Beamtalk-level arguments.
    fn generate_class_self_named_spawn(
        &mut self,
        named: NamedSpawn,
        arguments: ClassSendArgs<'_>,
    ) -> Result<Document<'static>> {
        let selector_atom = named.selector();
        let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;
        let call_doc = docvec![
            "call 'beamtalk_class_instantiation':'",
            Document::Str(named.helper()),
            "'(",
            Self::class_self_name_doc(),
            ", ",
            Self::class_self_module_doc(selector_atom),
            ", call 'beamtalk_class_instantiation':'resolve_is_abstract_or_raise'(",
            Self::class_self_name_doc(),
            ", ",
            leaf::atom(selector_atom),
            "), ",
            args_doc,
            ")"
        ];
        Ok(self.close_prelude(&args_preamble, call_doc, named.result_prefix()))
    }

    /// Generates code for actor spawn with conditional REPL registry integration.
    ///
    /// When spawning an actor in the REPL, check for `__repl_actor_registry__` in
    /// bindings and register the spawned actor. In all cases, the module's own
    /// `spawn/0` or `spawn/1` is called (which handles initialize protocol).
    /// In non-REPL contexts (regular code, tests), fall back to normal `module:spawn`.
    ///
    /// The emitted module atom is computed dynamically via `compiled_module_name`:
    /// - Package mode: `bt@{package}@{class}` (e.g. `bt@my_pkg@counter`)
    /// - Workspace/legacy mode: `bt@{class}` (e.g. `bt@counter`)
    ///
    /// # Arguments
    ///
    /// * `class_name` - The Beamtalk class name (e.g., "Counter")
    /// * `init_args` - Optional initialization arguments for spawnWith:
    ///
    /// # Generated Code (REPL context, package mode with package `my_pkg`)
    ///
    /// ```erlang
    /// case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of
    ///   <'undefined'> when 'true' ->
    ///     call 'bt@my_pkg@counter':'spawn'()
    ///   <RegistryPid> when 'true' ->
    ///     let SpawnResult = call 'bt@my_pkg@counter':'spawn'() in
    ///     let {'beamtalk_object', _, _, SpawnPid} = SpawnResult in
    ///     let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, 'Counter', 'bt@my_pkg@counter') in
    ///     SpawnResult
    /// end
    /// ```
    ///
    /// # Generated Code (non-REPL context, package mode with package `my_pkg`)
    ///
    /// ```erlang
    /// call 'bt@my_pkg@counter':'spawn'()
    /// ```
    /// Generates actor spawn with optional package qualifier (ADR 0070 Phase 2).
    ///
    /// When `package` is `Some`, uses `resolve_qualified_module_name` to compute
    /// the BEAM module name directly (e.g., `json@Parser` → `bt@json@parser`).
    /// When `None`, falls back to `compiled_module_name` for standard resolution.
    ///
    /// In REPL context, registers the spawned actor with the REPL actor registry.
    /// In non-REPL contexts, calls the module's `spawn/0` or `spawn/1` directly.
    ///
    /// `SpawnVariant::Monitored` (BT-4104) calls `spawn_monitored/0,1`
    /// instead; its answer is `{ActorObject, MonRef}`, so the REPL arm takes
    /// the pid from the pair's actor.
    ///
    /// The module comes from [`Self::class_reference_spawn_module_doc`]: an
    /// unqualified shadowed class name (BT-4278) resolves it at call time
    /// into a `let`-bound temporary, so the spawn still runs in the caller.
    pub(in crate::core_erlang) fn generate_actor_spawn_qualified(
        &mut self,
        class_name: &str,
        package: Option<&str>,
        selector_atom: &str,
        init_args: Option<&Expression>,
        variant: SpawnVariant,
    ) -> Result<Document<'static>> {
        let module_expr = self.class_reference_spawn_module_doc(
            class_name,
            package,
            selector_atom,
            SpawnTargetKind::Actor,
        );
        // An atom is used as-is; a runtime lookup is bound once, since the
        // REPL arm names the module twice.
        let (module_binding, module_doc): (Option<(String, Document<'static>)>, Document<'static>) =
            if self.spawn_module_resolved_at_runtime(class_name, package) {
                let var = self.fresh_temp_var("SpawnMod");
                (Some((var.clone(), module_expr)), leaf::var(var))
            } else {
                (None, module_expr)
            };
        let body = self.generate_actor_spawn_call(class_name, &module_doc, init_args, variant)?;
        Ok(match module_binding {
            Some((var, lookup)) => docvec!["let ", leaf::var(var), " = ", lookup, " in ", body],
            None => body,
        })
    }

    /// Body of [`Self::generate_actor_spawn_qualified`] once the module
    /// expression (`module_doc`, an atom or a bound variable) is known.
    fn generate_actor_spawn_call(
        &mut self,
        class_name: &str,
        module_doc: &Document<'static>,
        init_args: Option<&Expression>,
        variant: SpawnVariant,
    ) -> Result<Document<'static>> {
        let in_repl_context = self.lookup_var("__bindings__").is_some();
        let function = leaf::atom(variant.function_name());

        let args_doc = match init_args {
            Some(args) => self.expression_doc(args)?,
            None => Document::Nil,
        };

        if in_repl_context {
            let spawned_object: Document<'static> = match variant {
                SpawnVariant::Plain => Document::Str("SpawnResult"),
                SpawnVariant::Monitored | SpawnVariant::MonitoredAsync => {
                    Document::Str("call 'erlang':'element'(1, SpawnResult)")
                }
            };
            let doc = docvec![
                "case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of ",
                "<'undefined'> when 'true' -> call ",
                module_doc.clone(),
                ":",
                function.clone(),
                "(",
                args_doc.clone(),
                ") <RegistryPid> when 'true' -> let SpawnResult = call ",
                module_doc.clone(),
                ":",
                function,
                "(",
                args_doc,
                ") in ",
                "let SpawnPid = call 'erlang':'element'(4, ",
                spawned_object,
                ") in ",
                "let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, ",
                leaf::atom(class_name.to_string()),
                ", ",
                module_doc.clone(),
                ") in ",
                "SpawnResult ",
                "end"
            ];
            Ok(doc)
        } else {
            let doc = docvec![
                "call ",
                module_doc.clone(),
                ":",
                function,
                "(",
                args_doc,
                ")"
            ];
            Ok(doc)
        }
    }
}
