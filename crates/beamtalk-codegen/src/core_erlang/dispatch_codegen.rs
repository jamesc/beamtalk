// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Message sending and dispatch compilation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This domain service handles the **core domain operation** of Beamtalk: message
//! sending. In Smalltalk and Beamtalk, message sending is THE fundamental operation,
//! not method calls or function invocation.
//!
//! # Message Send Protocol (ADR 0007 Phase 4)
//!
//! Messages are dispatched through the following strategy:
//!
//! 1. **Compiler intrinsics**: Language-level constructs that the compiler must
//!    generate inline code for (binary operators, block evaluation, spawn/await,
//!    class/nil testing). These are structural requirements, not type-specific dispatch.
//!
//! 2. **Runtime dispatch**: All other messages go through the unified entry point
//!    `beamtalk_message_dispatch:send/3`, which routes to:
//!    - **Actors** (`beamtalk_object` records): Sync via `beamtalk_actor:sync_send/3` (ADR 0043)
//!    - **Class objects**: Sync via `beamtalk_object_class:class_send/3`
//!    - **Primitives** (everything else): Sync via `beamtalk_primitive:send/3`
//!
//! The primitive binding table from `lib/*.bt` (ADR 0007) drives stdlib method
//! compilation, while call-site dispatch uses runtime type checking — except
//! for receivers with a statically-known tagged-map (Value/Object) class
//! type, which compile to a guarded direct `Module:dispatch/3` call (BT-4069,
//! widened to open classes by BT-4293), and receivers statically typed as a
//! representation-routed built-in (`List`, `Dictionary`, `Integer`, …), which
//! compile to a direct call guarded by the representation test the dynamic
//! path routes on (BT-4294; both are the `typed_guarded` handler below).
//!
//! # Special Cases (Compiler Intrinsics)
//!
//! - **Binary operators**: `+`, `-`, `*`, `/` → Direct Erlang arithmetic
//! - **Block evaluation**: `value`, `whileTrue:`, `repeat` → Direct function calls
//! - **ProtoObject/Object**: `class`, `isNil`, `respondsTo:` → Pattern matching
//! - **Spawn messages**: `Class spawn`, `Class spawnWith: args` → `gen_server:start_link`
//! - **Await messages**: `future await` → Blocking future resolution
//! - **Super sends**: `super methodName:` → Parent class dispatch

use super::control_flow::{Closure, FieldWriteSite};
use super::expr_shape::is_character_typed_receiver;
use super::gen_server::SpawnVariant;
use super::method_frame::CLASS_SELF_VAR;
use super::routing;
use super::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ThreadedValue, ValueRef, VerifyError, VersionPrefix,
    VersionedVar,
};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{ClassKind, Expression, MessageSelector, WellKnownSelector};
use beamtalk_core::semantic_analysis::TypeProvenance;
use beamtalk_core::semantic_analysis::class_hierarchy::{ClassHierarchy, ClassInfo};
use beamtalk_core::semantic_analysis::type_checker::{
    FFI_ARG_RUNTIME_GUARDS, ffi_arg_passes_unchanged,
};
use beamtalk_core::source_analysis::Span;

/// One `generate_message_send` dispatch strategy: `Some(doc)` claims the
/// send, `None` defers to the next entry in [`HANDLERS`].
type SendHandler = fn(
    &mut CoreErlangGenerator,
    &Expression,
    &MessageSelector,
    &[Expression],
) -> Result<Option<Document<'static>>>;

/// The arguments of a class-side send: the source expressions, threaded by
/// the lowering that consumes them, or (BT-4317) already-bound variable
/// references, when one argument binding feeds several lowerings spliced into
/// the arms of a runtime `case` (each arm would otherwise thread, and so
/// compile, the same expressions again).
#[derive(Clone, Copy)]
pub(super) enum ClassSendArgs<'a> {
    /// Source expressions, threaded at the use site.
    Exprs(&'a [Expression]),
    /// Variables or literals already bound ahead of the send.
    Bound(&'a [Document<'static>]),
}

impl ClassSendArgs<'_> {
    fn len(self) -> usize {
        match self {
            Self::Exprs(exprs) => exprs.len(),
            Self::Bound(docs) => docs.len(),
        }
    }

    fn is_empty(self) -> bool {
        self.len() == 0
    }
}

/// `generate_message_send`'s dispatch priority as data, replacing
/// what were 15 source-ordered `if let Some(doc) = self.try_*()?` calls (two
/// of them a duplicated `is_character_typed_receiver` check — see
/// [`CoreErlangGenerator::try_handle_character_typed_message`]'s doc, now
/// this table's single `character_typed` entry). Order is significant and
/// load-bearing: earlier entries intentionally preempt later, more generic
/// ones (e.g. `character_typed` before `protoobject`/`object`, `dict` before
/// `list`, `class_method_self_send` before `self_dispatch`) — see each
/// handler's own doc for why. `typed_guarded` is last deliberately: it only
/// converts sends that would otherwise be fully dynamic, so every earlier,
/// more specific handler keeps its sends. A send that no entry claims falls
/// through to `generate_message_send`'s `generate_runtime_dispatch` default.
/// Pinned by `typed_sealed::send_handlers_keep_priority_positions`.
pub(in crate::core_erlang) const HANDLERS: &[(&str, SendHandler)] = &[
    (
        "character_typed",
        CoreErlangGenerator::try_handle_character_typed_message,
    ),
    (
        "protoobject",
        CoreErlangGenerator::try_generate_protoobject_message,
    ),
    ("object", CoreErlangGenerator::try_generate_object_message),
    ("block", CoreErlangGenerator::try_generate_block_message),
    ("dict", CoreErlangGenerator::try_generate_dict_message),
    ("list", CoreErlangGenerator::try_generate_list_message),
    (
        "boolean_protocol",
        CoreErlangGenerator::try_generate_boolean_protocol,
    ),
    ("spawn_await", CoreErlangGenerator::try_handle_spawn_await),
    (
        "erlang_interop",
        CoreErlangGenerator::try_handle_erlang_interop,
    ),
    (
        "logger_intrinsic",
        CoreErlangGenerator::try_generate_logger_intrinsic,
    ),
    (
        "class_reference",
        CoreErlangGenerator::try_handle_class_reference,
    ),
    (
        "class_method_self_send",
        CoreErlangGenerator::try_handle_class_method_self_send,
    ),
    (
        "self_dispatch",
        CoreErlangGenerator::try_handle_self_dispatch,
    ),
    (
        "typed_guarded",
        CoreErlangGenerator::try_handle_typed_guarded_send,
    ),
];

// BT-4069's exclusion of classes whose instances are never
// `'$beamtalk_class'`-tagged maps carrying their own name (raw Erlang terms,
// the untagged-map `Dictionary`, the `Class`/`Metaclass` records, the
// `ErlangModule` native proxy), and the exception-family root, live in the
// routing table generated from the shared fixture
// `runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json`
// (BT-4200, BT-4292): see `routing::is_non_tagged_map_class` and
// `routing::EXCEPTION_ROOT_CLASS`. The `EUnit`
// `beamtalk_tagged_map_routing_conformance_tests` checks the runtime routes
// each fixture row as the fixture says. `Future` is deliberately not a row
// (runtime-only, no compiled module — ruled out by the builtin/generated
// check in [`CoreErlangGenerator::typed_tagged_target`]), and neither
// is the rest of the `Exception` family (ruled out by a superclass-chain walk
// there, so future exception subclasses are covered without listing them).
// The fixture's supervisor-family rows are not non-tagged-map exclusions
// either: their subclasses share the `beamtalk_supervisor` tuple, so they are
// ruled out by a superclass-chain walk too ([`supervisor_family_root`]).
//
// BT-4294: a non-tagged-map row is not only an exclusion. When the fixture
// gives it a representation test and a single target module
// (`routing::builtin_send_route`), a typed send to it becomes a direct call
// guarded by that test ([`TypedSendTarget::Builtin`]) — for every selector
// outside the row's `special_selectors` (BT-4390).

/// BT-4292: why a send whose receiver did not resolve to a
/// [`TypedSendTarget`] stays dynamic — one variant per gate of
/// [`CoreErlangGenerator::typed_send_target`], so the dynamic-site
/// report (`BEAMTALK_CODEGEN_DIAGNOSTICS=1`) names the gate the send failed
/// rather than re-deriving it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum StaysDynamic {
    /// No trusted static type: none recorded, `Dynamic`, a union, a
    /// negation, a metatype, or a `Known` type of untrusted provenance.
    Untyped,
    /// A binary selector: lowered by the operator path, never a guarded call.
    BinarySelector,
    /// `fieldAt:put:`, whose dynamic path raises a different hint.
    FieldAtPut,
    /// The `perform:` family, or a selector the dynamic entry point rewrites
    /// before classifying the receiver (`perform:withArguments:timeout:`,
    /// from the routing fixture): the dynamic send does not reach the
    /// receiver's `dispatch/3` with that selector (BT-4294).
    PerformFamily,
    /// A class the routing table says is not a tagged map of its own name
    /// and that has no guarded direct route, with the fixture's reason
    /// (BT-4294).
    RepresentationRouted(routing::NoBuiltinRoute),
    /// A runtime-only built-in with no compiled module (`Future`).
    RuntimeOnlyBuiltin,
    /// The class is not in the hierarchy this unit sees.
    UnknownClass,
    /// An `abstract` class: it has no instances of its own, so no value
    /// carries its tag and a guard on it could never hit (BT-4293).
    Abstract,
    /// An ancestor of a representation-routed built-in (`Object`, `Value`,
    /// `ProtoObject`, …): the static type admits integers, strings and the
    /// rest, which no class-tag guard matches, so a guard would almost never
    /// hit (BT-4293).
    RepresentationAncestor,
    /// `Supervisor`, `DynamicSupervisor` or a subclass: instances are
    /// `{beamtalk_supervisor, …}` tuples, never tagged maps (BT-4293).
    SupervisorFamily,
    /// The class name is declared by more than one package in scope
    /// (BT-4151).
    AmbiguousPackage,
    /// A module-less (`ClassBuilder`) class (BT-4149).
    ModuleLess,
    /// Not a `Value`/`Object` kind: an actor, whose instances are
    /// `#beamtalk_object{}` records.
    NotValueKind,
    /// The `Exception` family, routed to `bt@stdlib@exception:dispatch`.
    ExceptionFamily,
    /// An ancestor is declared in another file, so the chain is not fully
    /// known.
    CrossFileParent,
}

/// Whether `class` is `Exception` or one of its subclasses — instances route
/// to `bt@stdlib@exception:dispatch`, so a guarded direct call never applies.
/// The one rule behind [`StaysDynamic::ExceptionFamily`], shared with the
/// dynamic-site report (BT-4292).
pub(in crate::core_erlang) fn is_exception_family(hierarchy: &ClassHierarchy, class: &str) -> bool {
    class == routing::EXCEPTION_ROOT_CLASS
        || hierarchy
            .superclass_chain(class)
            .iter()
            .any(|ancestor| ancestor.as_str() == routing::EXCEPTION_ROOT_CLASS)
}

/// The routing-fixture class whose `beamtalk_supervisor` tuple represents
/// `class`'s instances — `class` itself or the nearest ancestor with a
/// supervisor row (`Supervisor`, `DynamicSupervisor`) — if any. The one rule
/// behind [`StaysDynamic::SupervisorFamily`], shared with the dynamic-site
/// report; the representation fact is read from the routing fixture, not
/// restated here (BT-4293).
pub(in crate::core_erlang) fn supervisor_family_root(
    hierarchy: &ClassHierarchy,
    class: &str,
) -> Option<ecow::EcoString> {
    std::iter::once(class.into())
        .chain(hierarchy.superclass_chain(class))
        .find(|ancestor: &ecow::EcoString| {
            routing::row(ancestor.as_str())
                .is_some_and(|row| row.representation == routing::Representation::SupervisorTuple)
        })
}

/// Whether `class` is a proper ancestor of a routing-fixture class whose
/// instances are not tagged maps of its own name (`Object` above `Symbol`,
/// `Value` above `Integer`, …). Such a class is not abstract, but a value of
/// its static type is almost never an instance carrying its own tag, so the
/// typed-tagged path leaves it dynamic ([`StaysDynamic::RepresentationAncestor`]).
/// Read from the routing fixture, so a new row needs no change here (BT-4293).
pub(in crate::core_erlang) fn is_representation_ancestor(
    hierarchy: &ClassHierarchy,
    class: &str,
) -> bool {
    routing::rows().iter().any(|row| {
        hierarchy
            .superclass_chain(row.class)
            .iter()
            .any(|ancestor| ancestor.as_str() == class)
    })
}

/// BT-4069: a send receiver's statically-known tagged-map dispatch target — the
/// class tag to guard on plus the module to direct-call. Resolved by
/// [`CoreErlangGenerator::typed_tagged_target`]; BT-4127 reuses the
/// same hook to inline `obj class` for typed receivers.
pub(in crate::core_erlang) struct TypedTaggedTarget {
    /// The class name: the `'$beamtalk_class'` tag the guard matches.
    pub(in crate::core_erlang) class_name: String,
    /// The class's compiled module: this unit's own registry entry when
    /// present (the module the unit actually emits — BT-4152), else the
    /// qualified derivation (registry under the referenced package, else
    /// the deterministic `bt@{package}@{snake}` composition).
    pub(in crate::core_erlang) module_name: String,
}

/// BT-4294: the guarded direct call a typed send compiles to — the one
/// decision [`CoreErlangGenerator::typed_send_target`] makes, shared by the
/// `typed_guarded` handler and the dynamic-site report.
pub(in crate::core_erlang) enum TypedSendTarget {
    /// A tagged-map class: guard on its `'$beamtalk_class'` tag (BT-4069).
    Tagged(TypedTaggedTarget),
    /// A representation-routed built-in: guard on the representation test
    /// the dynamic path routes on, call the module it would call.
    Builtin(routing::BuiltinRoute),
}

impl CoreErlangGenerator {
    /// Generates the `<{'error',..., _}>` case clauses shared by all
    /// self-dispatch call sites (`safe_dispatch`/`dispatch` error branches).
    ///
    /// The dispatched call's error branch has two distinct shapes, mirroring the
    /// two `<{'error', ...}>` clauses already used at the `handle_cast`/
    /// `handle_info` boundary (see `gen_server/callbacks.rs`):
    ///
    /// 1. **Caught exception**: `safe_dispatch/3` packs a caught exception as a
    ///    3-tuple `{Type, Reason, Stacktrace}` in the middle element of its
    ///    `{'error', ..., State}` return (see `generate_safe_dispatch`). Passing
    ///    that whole triple straight to `beamtalk_error:'raise'/1` — which only
    ///    accepts a raw `#beamtalk_error{}` record — crashes with
    ///    `function_clause` instead of propagating the real error.
    ///    Destructuring the triple and routing it through
    ///    `beamtalk_exception_handler:'reraise'/4` mirrors the cross-actor call
    ///    boundary (`beamtalk_actor:sync_send_remote/3`), which correctly
    ///    classifies raw Erlang errors and preserves already-wrapped
    ///    `#beamtalk_error{}` values.
    /// 2. **Plain returned error**: `dispatch/4`'s DNU fallback (and other
    ///    non-exception error paths) *returns* `{'error', Error, State}` where
    ///    `Error` is a bare `#beamtalk_error{}` record — not a 3-tuple, so it
    ///    never reaches `safe_dispatch`'s try/catch at all. This fallback clause
    ///    must stay, or a self-send that resolves to DNU crashes with
    ///    `case_clause` instead of raising the DNU error.
    ///
    /// Passes a `selector`/`class` breadcrumb `Context` map to
    /// `reraise/4` — mirroring `sync_send_remote/3`'s
    /// `#{selector => Selector, class => Class}` construction — so a raw
    /// Erlang error escaping a self-send forwarding hop gets the same
    /// `ClassName>>selector: ...` location prefix (via `wrap_raw/2` /
    /// `located/3`) as the cross-actor equivalent.
    ///
    /// `class` is resolved via a *runtime* `beamtalk_actor:lookup_class/1`
    /// call on `self()`, not a compile-time literal atom. For a self-send inside
    /// a method a subclass inherits without overriding, the inherited method's
    /// code lives in the superclass module, so a literal `class_name()` atom
    /// would yield the superclass instead of the actor's actual runtime class —
    /// unlike `sync_send_remote/3`, which resolves the runtime class via the
    /// same `lookup_class/1` ETS reverse-lookup on `beamtalk_instance_registry`.
    /// Emitting the same runtime lookup here keeps the self-send breadcrumb's
    /// `class` value in parity with the cross-actor path for inherited methods.
    /// `selector_atom` is still known at compile time and stays a literal atom.
    ///
    /// Known caveat: `lookup_class/1` reads `beamtalk_instance_registry`,
    /// which the *spawner* populates only after `beamtalk_actor:safe_spawn/2`'s
    /// `await_initialize/3` confirms `initialize` has returned (see
    /// `gen_server/spawn.rs`'s `instance_registration_doc`). A self-send that
    /// raises *during* `initialize` — or in a continuation `initialize`
    /// requested, which since BT-3992 runs concurrently with the spawner's
    /// registration — can therefore run before this actor's own registry
    /// entry exists, so `lookup_class(self())` falls back to `'unknown'` for
    /// that narrow window — trading a guaranteed-
    /// correct compile-time atom (for non-inherited classes only) for a
    /// safe-but-less-specific fallback, in exchange for a correct answer in
    /// every other case (including all inherited-method self-sends, the
    /// actual bug this fixes). `lookup_class/1` never crashes either way.
    ///
    /// Core Erlang map literals (`~{...}~`) cannot contain `call` expressions
    /// (see `lifecycle_start_telemetry_doc` in `gen_server/callbacks.rs` for the
    /// same constraint), so the lookup is hoisted into a `let` binding inside
    /// the clause body before the map is constructed. This only runs on the
    /// error path — no overhead on the success path.
    ///
    /// ## NLR relay (BT-3580)
    ///
    /// A `^` inside a block passed through this self-send to a user HOM
    /// (e.g. `self do: [:each | ... ifTrue: [^each]]`) throws the 4-tuple
    /// `{'$bt_nlr', Token, Value, State}` (ADR 0041). `generate_safe_dispatch`
    /// (`gen_server/dispatch.rs`) already special-cases that exact throw
    /// shape ahead of its own catch-all (BT-3580/BT-3582) and returns it as
    /// a *bare* value — `{'error', {'$bt_nlr', Token, Value, State}, State}`
    /// — instead of packing it into the ordinary `{Type, Reason,
    /// Stacktrace}` triple every other caught exception gets. This call
    /// site must recognize that same bare shape and re-throw it (a plain
    /// `erlang:throw/1` — there is no stacktrace left to preserve; `^` is
    /// intentional control flow, not a reported error, matching the
    /// existing relays in `beamtalk_actor.erl` and
    /// `beamtalk_class_dispatch.erl`, neither of which preserves one
    /// either) so the enclosing method's own NLR catch (which only matches
    /// `throw:{'$bt_nlr', ...}` — see `nlr.rs`'s `wrap_body_with_nlr_catch`)
    /// sees it, whether it owns this token or is itself just another
    /// self-dispatch hop relaying it further up — the same relay
    /// `beamtalk_class_dispatch.erl` already performs for class-method
    /// self-dispatch hops (ADR 0110), now applied to instance actor
    /// self-sends. A leading clause here must come before the "plain
    /// returned error" catch-all below, which would otherwise pass the raw
    /// NLR tuple to `beamtalk_error:'raise'/1` — a `function_clause` crash,
    /// since that function only accepts a `#beamtalk_error{}` record.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// <{'error', {'$bt_nlr', NlrTok, NlrVal, NlrSt}, _}> when 'true' ->
    ///     call 'erlang':'throw'({'$bt_nlr', NlrTok, NlrVal, NlrSt})
    /// <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///     let Class = call 'beamtalk_actor':'lookup_class'(call 'erlang':'self'()) in
    ///     call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace,
    ///         ~{'selector' => 'selector:', 'class' => Class}~)
    /// <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// <NoMatch> when 'true' -> call 'erlang':'error'({'case_clause', NoMatch})
    /// ```
    ///
    /// the trailing wildcard clause is not reachable at runtime
    /// (`safe_dispatch/3` only ever returns `{'reply', _, _}` or one of the
    /// two `{'error', _, _}` shapes matched above) but is required to make
    /// the `case` *statically* exhaustive — see `case_clause_fallback`'s
    /// doc comment for why an implicit fallback isn't good enough here.
    fn generate_self_dispatch_error_clause(
        &mut self,
        var_prefix: &str,
        selector_atom: &str,
    ) -> Document<'static> {
        let nlr_token_var = self.fresh_var(&format!("{var_prefix}NlrTok"));
        let nlr_value_var = self.fresh_var(&format!("{var_prefix}NlrVal"));
        let nlr_state_var = self.fresh_var(&format!("{var_prefix}NlrSt"));
        let type_var = self.fresh_var(&format!("{var_prefix}Type"));
        let reason_var = self.fresh_var(&format!("{var_prefix}Reason"));
        let stack_var = self.fresh_var(&format!("{var_prefix}Stack"));
        let plain_error_var = self.fresh_var(&format!("{var_prefix}Plain"));
        let class_var = self.fresh_var(&format!("{var_prefix}Class"));
        let no_match_fallback = self.case_clause_fallback(&format!("{var_prefix}NoMatch"));
        let nlr_tuple_doc = || {
            docvec![
                "{'$bt_nlr', ",
                leaf::var(nlr_token_var.clone()),
                ", ",
                leaf::var(nlr_value_var.clone()),
                ", ",
                leaf::var(nlr_state_var.clone()),
                "}",
            ]
        };
        docvec![
            "<{'error', ",
            nlr_tuple_doc(),
            ", _}> when 'true' -> call 'erlang':'throw'(",
            nlr_tuple_doc(),
            ") ",
            "<{'error', {",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(reason_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "}, _}> when 'true' -> let ",
            leaf::var(class_var.clone()),
            " = call 'beamtalk_actor':'lookup_class'(call 'erlang':'self'()) in ",
            "call 'beamtalk_exception_handler':'reraise'(",
            leaf::var(type_var),
            ", ",
            leaf::var(reason_var),
            ", ",
            leaf::var(stack_var),
            ", ~{'selector' => ",
            leaf::atom(selector_atom.to_string()),
            ", 'class' => ",
            leaf::var(class_var),
            "}~) ",
            "<{'error', ",
            leaf::var(plain_error_var.clone()),
            ", _}> when 'true' -> call 'beamtalk_error':'raise'(",
            leaf::var(plain_error_var),
            ")",
            no_match_fallback,
            " ",
        ]
    }

    /// Rebinds `Self{N}` from an
    /// already-produced value `Document`: a construct's own returned trailing
    /// tuple slot, carrying the `self.field := ...` mutations threaded out of
    /// its inlined body. This is what makes the construct's final `Self` the
    /// method's new LIVE `Self` for every statement that follows — the direct
    /// counterpart of Actor's `let State1 = element(2, _CF10) in`.
    ///
    /// Two callers, one shape: BT-3484's Letrec loop (threaded through its
    /// recursive tail call) and BT-3486's `on:do:`/`ensure:` (threaded through
    /// the `try`'s result tuple).
    ///
    /// Verifies through the plain [`Self::check_simple_field_bind_invariant`]
    /// every other `Self{N}`/`State{N}` version step already uses.
    pub(super) fn rebind_value_self_from_doc(
        &mut self,
        value_doc: Document<'static>,
        span: beamtalk_core::source_analysis::Span,
    ) -> Document<'static> {
        let source_version = self.self_version();
        self.next_self_var();
        let target_version = self.self_version();
        self.check_simple_field_bind_invariant(
            super::threaded_ir::VersionPrefix::SelfVt,
            source_version,
            target_version,
            "value-type Self rebind from a threading construct's result",
            span,
        );
        let bind = ThreadedStmt::Bind {
            target: super::threaded_ir::VersionedVar::new(
                super::threaded_ir::VersionPrefix::SelfVt,
                target_version,
                FrameId::ROOT,
            ),
            source: super::threaded_ir::VersionedVar::new(
                super::threaded_ir::VersionPrefix::SelfVt,
                source_version,
                FrameId::ROOT,
            ),
            op: super::threaded_ir::BindOp::Direct(super::threaded_ir::ValueRef::Doc(value_doc)),
            span,
        };
        let mut ctx = super::threaded_ir::RenderCtx::new(self);
        super::threaded_ir::render(std::slice::from_ref(&bind), &mut ctx)
    }

    /// Generates code for a message send.
    ///
    /// This is the **main entry point** for message compilation. It dispatches
    /// to specialized handlers for different message patterns, and falls back
    /// to runtime dispatch via `beamtalk_message_dispatch:send/3`
    /// which handles actors, class objects, and primitives uniformly.
    ///
    /// # Message Dispatch Strategy (ADR 0007 Phase 4)
    ///
    /// 1. **Super sends** → `generate_super_send`
    /// 2. **Binary operators** → `generate_binary_op` (synchronous Erlang ops)
    /// 3. **`HANDLERS`** → priority-ordered table; see its doc for
    ///    the full breakdown (Character-typed dispatch, `ProtoObject`/Object
    ///    messages, Block/Dictionary/List messages, Boolean conditionals,
    ///    spawn/await, Erlang interop, Logger intrinsics, class references,
    ///    class-method and actor self-sends)
    /// 4. **Default** → Runtime dispatch (actor vs primitive check)
    pub(super) fn generate_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // ADR 0137: note the call for the bracket check (inert unless the flag is on).
        self.note_send(receiver, selector, arguments, false, receiver.span());
        // Special case: super message send
        // Super calls invoke the superclass implementation
        if matches!(receiver, Expression::Super(_)) {
            return self.generate_super_send(selector, arguments);
        }

        // Compile-time type assertion: `expr asType: SomeClass` (ADR 0025 Phase 2b)
        // Erased at codegen — generates only the receiver expression (zero runtime cost)
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "asType:" && arguments.len() == 1 {
                return self.expression_doc(receiver);
            }
        }

        // For binary operators, use Erlang's built-in operators (these are synchronous)
        if let MessageSelector::Binary(op) = selector {
            // Method lookup via `>>` operator (e.g., Counter >> #increment)
            // Support `>>` on any expression, not just class literals
            if super::operators::structural_binary(op)
                == Some(super::operators::StructuralBinary::MethodLookup)
            {
                if let Expression::ClassReference { name, .. } = receiver {
                    return self.generate_method_lookup(&name.name, arguments);
                }
                // Runtime fallback: evaluate receiver and call method/2
                return self.generate_runtime_method_lookup(receiver, arguments);
            }

            // BT-3985: a user-defined binary selector sent to `self` in an
            // actor instance method is a self-dispatch like any other (in a
            // closure it runs against the actor-state cell, ADR 0137); in a
            // class method it is a direct class-side self-send (a send
            // through the class's own gen_server would deadlock). Both take
            // the HANDLERS path
            // (`try_handle_self_dispatch` / `try_handle_class_method_self_send`)
            // instead of the operator lowering. The threaded positions already route it through
            // `generate_self_dispatch` via `is_dispatching_actor_self_send`.
            // BT-4005: an own-class reference (`C + x` inside `C`'s class
            // method) is the dispatching self too, and reaches
            // `try_handle_class_reference`'s direct class-side call.
            if !super::expr_shape::binary_send_dispatches_to_self(&self.shape_ctx(), receiver, op) {
                let doc = self.generate_binary_op(op, receiver, arguments)?;
                return Ok(doc);
            }
        }

        // dispatch priority for every remaining message shape is
        // `HANDLERS` — see its doc for why `character_typed` leads the list.
        for (_, handler) in HANDLERS {
            if let Some(doc) = handler(self, receiver, selector, arguments)? {
                return Ok(doc);
            }
        }

        // Unified dispatch via beamtalk_message_dispatch:send/3
        self.generate_runtime_dispatch(receiver, selector, arguments)
    }

    /// Character-typed-receiver dispatch —
    /// `HANDLERS`' first (and only) entry that ever fires on
    /// `is_character_typed_receiver` (a Character literal, a `Character
    /// value:` factory call, an `uppercase`/`lowercase` chain, or a
    /// statically Character-typed `ifTrue:ifFalse:` conditional). Collapses
    /// what used to be two separate
    /// `is_character_typed_receiver` checks in `generate_message_send` (one
    /// preempting [`Self::try_generate_protoobject_message`]'s `class` and
    /// [`Self::try_generate_object_message`]'s `respondsTo:`/`perform:`
    /// family for a Character receiver, one catching every other selector
    /// after the full non-character priority chain ran) into the one table
    /// entry `HANDLERS`' ordering now makes explicit:
    ///
    /// 1. `class`/`respondsTo:`/`perform:` family MUST be decided here,
    ///    before the rest of `HANDLERS` runs — `try_generate_protoobject_message`/
    ///    `try_generate_object_message` handle those selectors generically,
    ///    keyed on runtime `class_of/1` (which returns `'Integer'` for any
    ///    Character receiver, at the BEAM level a plain integer), for every
    ///    *other* receiver shape.
    /// 2. Any other selector runs the normal non-character priority chain
    ///    first (every other `HANDLERS` entry, skipping this one by name) —
    ///    e.g. `isNil`/`hash`/`error:` are receiver-agnostic Object-protocol
    ///    methods and must stay generic even for a Character receiver.
    /// 3. Only once nothing in that chain claims the selector does this
    ///    fall back to [`Self::generate_character_typed_dispatch`], instead
    ///    of [`Self::generate_runtime_dispatch`]'s `class_of/1`-keyed
    ///    fallback (which would treat the receiver as an `Integer`).
    fn try_handle_character_typed_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if !is_character_typed_receiver(receiver) {
            return Ok(None);
        }

        match selector.well_known() {
            Some(WellKnownSelector::Class) => {
                // Hoist any side effects in the receiver expression
                // (none for a literal, but capture preserves the contract).
                let (preamble, _) = self.thread_subexprs(&[receiver], "CharCls")?;
                // Resolve to the Character class object so equality with
                // the `Character` class reference holds — `class_of_object`
                // for raw integer 65 would otherwise return Integer's
                // class object via `class_of/1 == 'Integer'`.
                let call_doc = Document::Str(
                    "call 'beamtalk_primitive':'class_of_object_by_name'('Character')",
                );
                return Ok(Some(self.close_prelude(&preamble, call_doc, "CharClsRes")));
            }
            Some(WellKnownSelector::RespondsTo) => {
                let exprs: [&Expression; 2] = [receiver, &arguments[0]];
                let mut seq = self.sequence_call(&exprs, "CharResp")?;
                let _recv = seq.next();
                let sel_doc = seq.next();
                let call_doc = docvec!["call 'bt@stdlib@character':'has_method'(", sel_doc, ")"];
                return Ok(Some(seq.close(self, call_doc, "CharRespRes")));
            }
            Some(
                WellKnownSelector::Perform
                | WellKnownSelector::PerformWithArgs
                | WellKnownSelector::PerformLocallyWithArgs,
            ) => {
                return Ok(Some(self.generate_character_typed_dispatch(
                    receiver, selector, arguments,
                )?));
            }
            _ => {}
        }

        // Skip by name, not position (`&HANDLERS[1..]`) — this entry's own
        // position in `HANDLERS` is exactly what's being looked up, so
        // slicing past index 0 would silently self-recurse if a reorder
        // ever moved `character_typed` elsewhere in the table.
        for (name, handler) in HANDLERS {
            if *name == "character_typed" {
                continue;
            }
            if let Some(doc) = handler(self, receiver, selector, arguments)? {
                return Ok(Some(doc));
            }
        }

        // at the BEAM level a Character value is a plain
        // integer, so the generic fallback (`generate_runtime_dispatch`,
        // keyed on runtime `class_of/1`) would route it to
        // `bt@stdlib@integer:dispatch/3`. Reach the Character module's
        // dispatch instead, e.g. for `$A asInteger`, `$A printString`, `$A
        // uppercase`, `(Character value: 10) asString`.
        Ok(Some(self.generate_character_typed_dispatch(
            receiver, selector, arguments,
        )?))
    }

    /// Routes a non-binary message to the Character module's
    /// `dispatch/3`, for any receiver `is_character_typed_receiver` recognizes
    /// (a Character literal, a `Character value:` factory call, an
    /// `uppercase`/`lowercase` chain, or a statically Character-typed
    /// `ifTrue:ifFalse:` conditional).
    ///
    /// At the BEAM level, Character values are plain integers, so the default
    /// runtime dispatch path (keyed on `is_integer/1`) sends them to the Integer
    /// module. This codegen specialization restores the static type by emitting
    /// a direct call to `bt@stdlib@character:dispatch/3` instead. The receiver
    /// expression itself is compiled normally (`capture_subexpr_sequence`), so
    /// this works whether the receiver is a bare literal or an arbitrary
    /// expression statically known to produce a Character.
    fn generate_character_typed_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();

        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        let mut seq = self.sequence_call(&all_exprs, "CharDisp")?;
        let actual_receiver = seq.next();
        let args_doc = Self::join_docs_with_commas(seq.rest());

        let call_doc = docvec![
            "call 'bt@stdlib@character':'dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            actual_receiver,
            ")"
        ];

        Ok(seq.close(self, call_doc, "CharDispRes"))
    }

    /// BT-4069, BT-4294: the receiver's trusted static class — the class
    /// the type checker's `TypeMap` records as `Known` for the receiver's
    /// span with a trusted provenance (`Declared`, `Inferred`, `Substituted`
    /// or `Aliased` — a dispatch-owned rule: xref wants recall while dispatch
    /// wants precision, so the two must not share one predicate) — or
    /// [`StaysDynamic::Untyped`].
    ///
    /// BT-4412: `Aliased` is trusted because the type checker attaches it
    /// only at the point a user-written annotation naming an alias is
    /// expanded (ADR 0108), so it is a declared type under another name.
    /// `Extracted` (`-spec`-derived) stays untrusted.
    pub(in crate::core_erlang) fn trusted_static_class(
        &self,
        receiver: &Expression,
    ) -> std::result::Result<&ecow::EcoString, StaysDynamic> {
        let beamtalk_core::semantic_analysis::InferredType::Known {
            class_name,
            provenance,
            ..
        } = self
            .type_map
            .get(receiver.span())
            .ok_or(StaysDynamic::Untyped)?
        else {
            return Err(StaysDynamic::Untyped);
        };
        if !matches!(
            provenance,
            TypeProvenance::Declared(_)
                | TypeProvenance::Inferred(_)
                | TypeProvenance::Substituted(_)
                | TypeProvenance::Aliased { .. }
        ) {
            return Err(StaysDynamic::Untyped);
        }
        Ok(class_name)
    }

    /// BT-4069, BT-4293: resolves a receiver's trusted static class that is
    /// not a non-tagged routing-fixture row to a static tagged-map dispatch
    /// target, or the [`StaysDynamic`] gate it failed, unless every condition
    /// holds:
    ///
    /// 1. The receiver has a trusted static class
    ///    ([`Self::trusted_static_class`]).
    /// 2. The class is in the hierarchy and `Value`- or `Object`-kind (both
    ///    are `'$beamtalk_class'`-tagged maps with the same dynamic path).
    ///    It need **not** be sealed (BT-4293, ADR 0025 amendment §1): the tag
    ///    names the exact class, so a subclass instance simply misses the
    ///    guard and takes the dynamic fallback — `sealed` only changes how
    ///    often the guard hits, never what the send means.
    /// 3. Its instances really are tagged maps of its own name: not in the
    ///    routing table ([`routing::is_non_tagged_map_class`], decided by
    ///    [`Self::typed_send_target`] before this runs), not a
    ///    runtime-only builtin without a compiled module (`Future`), not
    ///    `abstract` (no instance carries an abstract class's tag:
    ///    `Collection`, `Number`, `Boolean`, …), not an ancestor of a
    ///    representation-routed built-in ([`is_representation_ancestor`]:
    ///    `Object`, `Value`, whose static types admit raw terms), not in the
    ///    supervisor family
    ///    ([`supervisor_family_root`]: `beamtalk_supervisor` tuples, read
    ///    from the routing fixture), and not the `Exception` family (whose
    ///    dynamic sends route via `bt@stdlib@exception:dispatch`, a different
    ///    entry point). The abstract, ancestor and supervisor gates are not
    ///    needed for soundness — a guard on them would (almost) never hit —
    ///    but they keep the emitted code free of dead guards.
    /// 4. Its ancestry is fully known (`has_cross_file_parent` fails closed —
    ///    a missing link could hide an `Exception` ancestor).
    /// 5. It has a compiled module (`is_module_less` fails closed — a live
    ///    `ClassBuilder` class dispatches through funs in its class process,
    ///    so a direct `Module:dispatch/3` would `undef`).
    /// 6. Its name is declared by only one package in scope (BT-4151). Two
    ///    packages' same-named classes are legal under qualified references
    ///    (`json@Parser` + `xml@Parser`) but collide into one last-wins
    ///    runtime class, so the tag guard cannot tell their instances apart:
    ///    a direct call would follow the compile-time package where dynamic
    ///    dispatch follows load order.
    ///
    /// Why this is sound (the guard, not the annotation, is trusted):
    ///
    /// - The guard verifies the runtime value's tag; a mismatch (violated
    ///   annotation, rebound parameter, `asType:` lie) falls back to a
    ///   byte-identical dynamic send, so no rebind analysis is needed.
    /// - A tag match means the dynamic path would route to the same module's
    ///   `dispatch/3` (`tagged_map_dispatch` → `tagged_class_dispatch`, whose
    ///   only divergences — proxy, exception entry, cross-package
    ///   shadowing — are excluded above; stdlib shadowing is a build error),
    ///   and `dispatch/3` preserves DNU, extensions, superclass
    ///   delegation and `perform:` identically — which is why no
    ///   method-resolution check is needed. (`fieldAt:put:` is the one
    ///   selector whose dynamic path diverges — a different hint via
    ///   `is_ivar_method` — so the handler below excludes it.)
    /// - Every hierarchy entry that can reach here has a compiled module
    ///   exporting `dispatch/3`: batch hierarchies contain only compiled
    ///   classes, and live module-less (`ClassBuilder`) classes arriving via
    ///   a REPL or `compile:source:` ambient hierarchy are excluded by
    ///   condition 5 (only bare REPL *expressions* leave `type_map` empty;
    ///   inline class definitions and compiled methods populate it). The
    ///   remote `Mod:dispatch/3` call is late-bound, so hot reload needs no
    ///   invalidation — provided the module *name* is unchanged. A class
    ///   redefined into a different module is handled by pinning the unit's
    ///   own class to its emitted module (`register_own_class_module`,
    ///   BT-4200) and by deciding module-lessness from the live index first.
    ///
    /// Known limitation (BT-4151): condition 6 sees the scope its producer
    /// hands in. A batch build or `beamtalk test` passes every package's
    /// declarations; a REPL or `compile:source:` unit sees the live node,
    /// where a name already holds one (last-wins) class, so a same-named
    /// class from another package loaded *after* this unit compiled is not
    /// visible — the runtime's collision warning is then the only signal.
    ///
    /// Known limitation (review A3, BT-4149; shared with `class sealed` direct calls):
    /// the target module is pinned at compile time, so if it is removed
    /// afterwards (`removeFromSystem`) a tag-matching send raises raw `undef`
    /// where the dynamic path would degrade to `does_not_understand`.
    fn typed_tagged_target(
        &self,
        class_name: &ecow::EcoString,
    ) -> std::result::Result<TypedTaggedTarget, StaysDynamic> {
        let hierarchy = self
            .class_hierarchy
            .as_ref()
            .ok_or(StaysDynamic::UnknownClass)?;
        if ClassHierarchy::is_builtin_class(class_name)
            && !ClassHierarchy::is_generated_builtin_class(class_name)
        {
            return Err(StaysDynamic::RuntimeOnlyBuiltin);
        }
        let info = hierarchy
            .get_class(class_name)
            .ok_or(StaysDynamic::UnknownClass)?;
        if info.is_abstract {
            return Err(StaysDynamic::Abstract);
        }
        if is_representation_ancestor(hierarchy, class_name) {
            return Err(StaysDynamic::RepresentationAncestor);
        }
        if supervisor_family_root(hierarchy, class_name).is_some() {
            return Err(StaysDynamic::SupervisorFamily);
        }
        if self.is_shadowed_class(class_name) {
            return Err(StaysDynamic::AmbiguousPackage);
        }
        // BT-4200: one consistent source for the module-less gate. The live
        // class-module index (REPL: `build_class_module_index/0`, read from
        // the running class processes) is fresher than `info.is_module_less`,
        // which comes from compiler-server metadata that a `ClassBuilder`
        // re-registration over an existing compiled class never refreshes
        // (`update_class` finds no `__beamtalk_meta/0` to re-notify with).
        // So when the index has an entry, it decides (by the same rule the
        // meta decode applies, `registered_module_is_module_less`); metadata
        // is only the fallback for a class the index does not know.
        let indexed_module = self
            .class_module_registry()
            .module_for_class(&self.own_package_id(), class_name);
        let module_less = indexed_module.map_or(info.is_module_less, |module| {
            ClassInfo::registered_module_is_module_less(module.as_str(), class_name.as_str())
        });
        if module_less {
            return Err(StaysDynamic::ModuleLess);
        }
        if !matches!(
            hierarchy.resolve_class_kind(class_name),
            ClassKind::Value | ClassKind::Object
        ) {
            return Err(StaysDynamic::NotValueKind);
        }
        if is_exception_family(hierarchy, class_name) {
            return Err(StaysDynamic::ExceptionFamily);
        }
        if hierarchy.has_cross_file_parent(class_name) {
            return Err(StaysDynamic::CrossFileParent);
        }
        // BT-4152: resolve through this unit's own registry entry first —
        // it names the module the unit actually emits. `beamtalk test`
        // emits fixture/test modules unqualified (`bt@<stem>`, keyed under
        // this unit's own `PackageId`) while stamping the file's own class
        // with `package = Some(current_package)`; re-deriving from
        // `info.package` alone would target the never-emitted
        // `bt@<package>@<class>`. The qualified derivation stays as the
        // fallback for a packaged class with no index entry.
        //
        // BT-4293: the unit's own class comes first of all — its `dispatch/3`
        // is in the module this unit emits. `beamtalk test` emits a test
        // file's class unqualified with no index entry, so neither the
        // registry nor the qualified derivation names it (with the sealed
        // gate gone, every `TestCase` self-send reaches here).
        let module_name = self.own_class_module(class_name).unwrap_or_else(|| {
            self.class_module_registry()
                .module_for_class(&self.own_package_id(), class_name)
                .map_or_else(
                    || self.compiled_module_name_qualified(class_name, info.package.as_deref()),
                    |module| module.as_str().to_string(),
                )
        });
        Ok(TypedTaggedTarget {
            class_name: class_name.to_string(),
            module_name,
        })
    }

    /// BT-4292, BT-4294: the one decision whether a send becomes a guarded
    /// direct call, and to what — shared by the `typed_guarded` handler and
    /// the dynamic-site report so the report can never disagree with what was
    /// emitted.
    ///
    /// The selector gates come first. Binary operators never reach the
    /// dynamic fallthrough (preserving `generate_runtime_dispatch`'s internal
    /// error exactly). `fieldAt:put:`'s dynamic path raised a different hint
    /// via the runtime's `is_ivar_method` check (unreachable today: the
    /// object-reflection intrinsic claims it first; kept as enforcement). The
    /// `perform:` family is claimed by intrinsics first too, and the
    /// fixture's rewritten selectors (`perform:withArguments:timeout:`) never
    /// reach a `dispatch/3` through the dynamic path, which turns them into a
    /// `send/4` of the target selector.
    ///
    /// Then the receiver's trusted static class decides the target. A
    /// routing-fixture row whose instances are not tagged maps of its own
    /// name ([`routing::is_non_tagged_map_class`]) takes the fixture's
    /// built-in route, or stays dynamic for the fixture's reason
    /// ([`routing::builtin_send_route`]); every other class is a tagged-map
    /// candidate ([`Self::typed_tagged_target`]).
    ///
    /// Why a built-in route is sound: its guard is the clause the dynamic
    /// path (`beamtalk_message_dispatch:send/3` → `beamtalk_primitive:send/3`
    /// → `module_for_value/2`, or `send_map/3` → `tagged_map_dispatch/1`)
    /// takes to reach `target_module:dispatch/3`, which the runtime half of
    /// the routing fixture checks; a value outside it (a lying annotation, a
    /// reserved tuple, `nil` typed as `Symbol`) misses and takes the dynamic
    /// send. A selector the row's dynamic path answers elsewhere (`Pid`'s
    /// future protocol, `String`'s shared byte-level selectors — the
    /// fixture's `special_selectors`, BT-4390) stays dynamic; every other
    /// selector on those rows is routed. A row whose test spans two routes
    /// (`Binary`: a bare binary or the `String` tag) gets both, guarded one
    /// after the other (BT-4510). Rows that carry a `dynamic_only` reason
    /// (`Boolean`, `Bitstring`, `Character`, the records, the proxy) stay
    /// dynamic. A stdlib built-in's module can be neither removed nor
    /// redefined into another module, so the pinned target never goes stale.
    pub(in crate::core_erlang) fn typed_send_target(
        &self,
        receiver: &Expression,
        selector: &MessageSelector,
    ) -> std::result::Result<TypedSendTarget, StaysDynamic> {
        if matches!(selector, MessageSelector::Binary(_)) {
            return Err(StaysDynamic::BinarySelector);
        }
        match selector.well_known() {
            Some(WellKnownSelector::FieldAtPut) => return Err(StaysDynamic::FieldAtPut),
            Some(
                WellKnownSelector::Perform
                | WellKnownSelector::PerformWithArgs
                | WellKnownSelector::PerformLocallyWithArgs,
            ) => return Err(StaysDynamic::PerformFamily),
            _ => {}
        }
        if routing::rewritten_selectors().contains(&selector.name().as_str()) {
            return Err(StaysDynamic::PerformFamily);
        }
        let class_name = self.trusted_static_class(receiver)?;
        if routing::is_non_tagged_map_class(class_name.as_str()) {
            return match routing::builtin_send_route(class_name.as_str(), &selector.name()) {
                Some(Ok(route)) => Ok(TypedSendTarget::Builtin(route)),
                Some(Err(why)) => Err(StaysDynamic::RepresentationRouted(why)),
                None => Err(StaysDynamic::RepresentationRouted(
                    routing::NoBuiltinRoute::NoSingleRoute,
                )),
            };
        }
        self.typed_tagged_target(class_name)
            .map(TypedSendTarget::Tagged)
    }

    /// BT-4069: `HANDLERS`' last entry. Converts a send to a receiver
    /// statically typed as a tagged-map class (sealed or open, BT-4293) or a
    /// representation-routed built-in (BT-4294) into a guarded direct
    /// `Module:dispatch/3` call ([`Self::typed_send_target`] decides). Only
    /// fires when nothing earlier claimed the send, so it purely converts
    /// would-be-dynamic sends.
    fn try_handle_typed_guarded_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        let Ok(target) = self.typed_send_target(receiver, selector) else {
            return Ok(None);
        };
        Ok(Some(self.generate_typed_guarded_send(
            receiver, selector, arguments, &target,
        )?))
    }

    /// BT-4069, BT-4294: emits the guarded direct call for a
    /// [`TypedSendTarget`]. A built-in route's guard is its representation
    /// test, rendered by the type-pattern renderer of its fixture kind
    /// (`render_send_guard`), with the dynamic send in every miss arm. A
    /// [`TypedTaggedTarget`]'s guard is an `is_map` + `maps:get/3`-default case nest
    /// (the `match_lowering` idiom — `maps:get/3` never raises, so no guard
    /// position is needed) selecting between the direct `Module:dispatch/3`
    /// call and a dynamic-send fallback in each mismatch arm:
    ///
    /// ```erlang
    /// case call 'erlang':'is_map'(R) of
    ///     <'true'> when 'true' ->
    ///         case call 'maps':'get'('$beamtalk_class', R, 'undefined') of
    ///             <'SealedWidget'> when 'true' ->
    ///                 call 'bt@test@sealed_widget':'dispatch'('x', [], R)
    ///             <NoMatch> when 'true' ->
    ///                 call 'beamtalk_message_dispatch':'send'(R, 'x', [])
    ///         end
    ///     <'false'> when 'true' ->
    ///         call 'beamtalk_message_dispatch':'send'(R, 'x', [])
    ///     <NoMatch> when 'true' -> call 'erlang':'error'({'case_clause', NoMatch})
    /// end
    /// ```
    ///
    /// Receiver and arguments are sequenced once in evaluation order, then
    /// bound to fresh temporaries (mirroring `guarded_op_doc`): each operand
    /// is referenced up to four times below, so unlike the single-use dynamic
    /// send they cannot stay inline. The outer boolean case carries an
    /// explicit `case_clause_fallback` like `guarded_op_doc`'s guards; the
    /// inner case is statically exhaustive via its wildcard arm.
    fn generate_typed_guarded_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        target: &TypedSendTarget,
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();

        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        all_exprs.extend(arguments.iter());
        let (preamble, mut docs) = self.thread_subexprs(&all_exprs, "TTagged")?;
        let mut docs_iter = docs.drain(..);
        let receiver_doc = docs_iter.next().ok_or_else(|| {
            CodeGenError::Internal("thread_subexprs dropped the receiver".to_string())
        })?;
        let arg_docs: Vec<Document<'static>> = docs_iter.collect();

        let receiver_var = self.fresh_temp_var("TTaggedRecv");
        let arg_vars: Vec<String> = arg_docs
            .iter()
            .map(|_| self.fresh_temp_var("TTaggedArg"))
            .collect();
        let case_doc = self.guarded_send_on_vars(target, &selector_atom, &receiver_var, &arg_vars);

        // BT-4279: a unary send that may be an auto-generated slot getter of a
        // tagged Value class. Its fast path reads the slot with no bracket
        // when the receiver is tagged exactly that class and the loaded
        // module's `__is_slot/1` says the selector is still a plain slot read
        // (a reload that turns it into a method answers `false`).
        if let TypedSendTarget::Tagged(tagged) = target
            && arguments.is_empty()
            && preamble.is_empty()
            && self.class_declares_value_slot(&tagged.class_name, &selector_atom)
        {
            let test = Self::slot_read_test_doc(&receiver_var, tagged, &selector_atom);
            let fast = docvec![
                "call 'maps':'get'(",
                leaf::atom(selector_atom.clone()),
                ", ",
                leaf::var(receiver_var.clone()),
                ")",
            ];
            let bindings = docvec![
                "let ",
                leaf::var(receiver_var.clone()),
                " = ",
                receiver_doc.clone(),
                " in ",
            ];
            self.offer_fast_path(receiver, bindings, test, fast, case_doc.clone());
        }

        let mut body = case_doc;
        for (var, doc) in arg_vars.into_iter().zip(arg_docs).rev() {
            body = docvec!["let ", leaf::var(var), " = ", doc, " in ", body];
        }
        body = docvec![
            "let ",
            leaf::var(receiver_var),
            " = ",
            receiver_doc,
            " in ",
            body
        ];
        Ok(self.close_prelude(&preamble, body, "TTaggedRes"))
    }

    /// BT-4069, BT-4294, BT-4510: the guarded direct call of `selector_atom`
    /// to the value bound to `receiver_var`, with `arg_vars` as arguments,
    /// for a [`TypedSendTarget`]: the target's guard (a class tag, or a
    /// built-in's representation test, then its `String`-tag half if any)
    /// around the direct call, and the dynamic send in every miss arm. Shared
    /// by [`Self::generate_typed_guarded_send`] and the sends the compiler
    /// makes itself on an already-bound value (a string interpolation
    /// segment's `displayString`).
    pub(in crate::core_erlang) fn guarded_send_on_vars(
        &mut self,
        target: &TypedSendTarget,
        selector_atom: &str,
        receiver_var: &str,
        arg_vars: &[String],
    ) -> Document<'static> {
        let args_list_doc = Self::join_docs_with_commas(
            arg_vars.iter().map(|var| leaf::var(var.clone())).collect(),
        );
        let module_name = match target {
            TypedSendTarget::Tagged(tagged) => tagged.module_name.clone(),
            TypedSendTarget::Builtin(route) => route.target_module.to_string(),
        };
        // BT-4473: a send to an instance of this unit's own class whose
        // selector one of its `dispatch/3` method arms answers calls that
        // arm's function directly; every other tagged target, and every
        // built-in route, calls `dispatch/3`.
        let own_arm = match target {
            TypedSendTarget::Tagged(tagged) => {
                self.own_dispatch_arm(&tagged.class_name, selector_atom, arg_vars.len())
            }
            TypedSendTarget::Builtin(_) => None,
        };
        let direct_call = match own_arm {
            Some(arm) => Self::own_arm_call_doc(selector_atom, arm, receiver_var, arg_vars),
            None => {
                Self::dispatch3_call_doc(module_name, selector_atom, &args_list_doc, receiver_var)
            }
        };
        // BT-4366: both calls sit inside the type-test `case`, so each carries
        // the send's line itself.
        let direct_doc = self.annotate_current_send_call(direct_call);
        let fallback_doc = self.annotate_current_send_call(docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            leaf::var(receiver_var.to_string()),
            ", ",
            leaf::atom(selector_atom.to_string()),
            ", [",
            args_list_doc.clone(),
            "])",
        ]);

        match target {
            TypedSendTarget::Tagged(tagged) => self.class_tag_guarded_doc(
                receiver_var,
                &tagged.class_name,
                direct_doc,
                fallback_doc,
            ),
            TypedSendTarget::Builtin(route) => {
                // BT-4510: a `Binary` receiver that is a `String` at run time
                // misses the bare-binary guard and tries the `String` route
                // before the dynamic send.
                let miss_doc = match route.string_tag_route {
                    Some(string_route) => {
                        let string_call =
                            self.annotate_current_send_call(Self::dispatch3_call_doc(
                                string_route.target_module.to_string(),
                                selector_atom,
                                &args_list_doc,
                                receiver_var,
                            ));
                        self.render_send_guard(
                            string_route.guard,
                            receiver_var,
                            string_call,
                            &fallback_doc,
                        )
                    }
                    None => fallback_doc,
                };
                self.render_send_guard(route.guard, receiver_var, direct_doc, &miss_doc)
            }
        }
    }

    /// `call 'Module':'dispatch'('sel', [Args], Recv)` — the remote
    /// `dispatch/3` call every guarded direct send makes (BT-4510).
    fn dispatch3_call_doc(
        module_name: String,
        selector_atom: &str,
        args_list_doc: &Document<'static>,
        receiver_var: &str,
    ) -> Document<'static> {
        docvec![
            "call ",
            leaf::atom(module_name),
            ":'dispatch'(",
            leaf::atom(selector_atom.to_string()),
            ", [",
            args_list_doc.clone(),
            "], ",
            leaf::var(receiver_var.to_string()),
            ")",
        ]
    }

    /// BT-4473: the own-class `dispatch/3` method arm that answers
    /// `selector` with `arity` arguments for an instance of `class_name`, when
    /// `class_name` is the class this unit compiles and the code being
    /// generated is that class's own (not an extension body re-scoped to
    /// another class).
    ///
    /// Why calling the arm's function is the send: behind the class-tag guard
    /// the receiver is an instance of exactly this class, so the dynamic send
    /// reaches this module's `dispatch/3` (see [`Self::typed_tagged_target`]),
    /// which answers the selector from that arm before its default arm — so
    /// no extension or superclass is consulted, and nothing installed later
    /// can change that. The function is called locally, so caller and callee
    /// are always the same module version and the arm this unit compiled
    /// against is the one it calls. One difference from
    /// `dispatch/3`, whose arms are remote calls: code still running in the
    /// old version after a reload (a long loop, an old block) keeps calling
    /// the old method, where `dispatch/3` would have reached the new one.
    /// Calls made after the reload, from new code, reach the new method.
    fn own_dispatch_arm(
        &self,
        class_name: &str,
        selector: &str,
        arity: usize,
    ) -> Option<super::value_type_codegen::OwnDispatchArm> {
        self.own_class_module(class_name)?;
        if self.class_name() != class_name {
            return None;
        }
        self.class_context
            .as_ref()?
            .own_dispatch_arms
            .get(selector)
            .copied()
            .filter(|arm| arm.arity == arity)
    }

    /// BT-4473: the local call of an own-class method arm's function —
    /// `apply 'sel'/N(R, A1, …)`, which answers the send's result (BT-4503).
    fn own_arm_call_doc(
        selector: &str,
        arm: super::value_type_codegen::OwnDispatchArm,
        receiver_var: &str,
        arg_vars: &[String],
    ) -> Document<'static> {
        let mut call_args: Vec<Document<'static>> = vec![leaf::var(receiver_var.to_string())];
        call_args.extend(arg_vars.iter().map(|var| leaf::var(var.clone())));
        docvec![
            "apply ",
            leaf::fname(selector.to_string(), arm.arity + 1),
            "(",
            Self::join_docs_with_commas(call_args),
            ")",
        ]
    }

    /// BT-4279: whether `class_name` is a Value class that declares a field
    /// named `selector`, so a unary send of it may reach the class's
    /// auto-generated getter (whether it does is the loaded module's
    /// `__is_slot/1` answer at run time).
    fn class_declares_value_slot(&self, class_name: &str, selector: &str) -> bool {
        self.class_hierarchy.as_ref().is_some_and(|h| {
            h.get_class(class_name)
                .is_some_and(|c| c.is_value && c.state.iter().any(|f| f.as_str() == selector))
        })
    }

    /// BT-4279: the slot-read fast path's test: the receiver is a map tagged
    /// exactly the target class, and that class's loaded module answers
    /// `true` from `__is_slot/1` for the selector. A module compiled before
    /// `__is_slot/1` existed answers `false` (the export check).
    fn slot_read_test_doc(
        receiver_var: &str,
        tagged: &TypedTaggedTarget,
        selector: &str,
    ) -> Document<'static> {
        let module = leaf::atom(tagged.module_name.clone());
        docvec![
            "case call 'erlang':'is_map'(",
            leaf::var(receiver_var.to_string()),
            ") of <'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            leaf::var(receiver_var.to_string()),
            ", 'undefined') of <",
            leaf::atom(tagged.class_name.clone()),
            "> when 'true' -> ",
            "case call 'erlang':'function_exported'(",
            module.clone(),
            ", '__is_slot', 1) of <'true'> when 'true' -> call ",
            module,
            ":'__is_slot'(",
            leaf::atom(selector.to_string()),
            ") <_> when 'true' -> 'false' end ",
            "<_> when 'true' -> 'false' end ",
            "<_> when 'true' -> 'false' end",
        ]
    }

    /// The tagged-map guard of [`Self::generate_typed_guarded_send`]:
    /// `direct_doc` when `receiver_var` is a map tagged exactly `class_name`,
    /// `fallback_doc` otherwise.
    fn class_tag_guarded_doc(
        &mut self,
        receiver_var: &str,
        class_name: &str,
        direct_doc: Document<'static>,
        fallback_doc: Document<'static>,
    ) -> Document<'static> {
        let no_match_inner = self.fresh_temp_var("TTaggedNoMatch");
        let no_match_outer = self.case_clause_fallback("TTaggedOuterNoMatch");
        docvec![
            "case call 'erlang':'is_map'(",
            leaf::var(receiver_var.to_string()),
            ") of ",
            "<'true'> when 'true' -> ",
            "case call 'maps':'get'('$beamtalk_class', ",
            leaf::var(receiver_var.to_string()),
            ", 'undefined') of ",
            "<",
            leaf::atom(class_name.to_string()),
            "> when 'true' -> ",
            direct_doc,
            " <",
            leaf::var(no_match_inner),
            "> when 'true' -> ",
            fallback_doc.clone(),
            " end ",
            "<'false'> when 'true' -> ",
            fallback_doc,
            no_match_outer,
            " end",
        ]
    }

    /// `call 'beamtalk_primitive':'class_of_object'(Receiver)`: the dynamic
    /// `obj class` — the protoobject `class` arm's plain lowering and the
    /// miss arm of [`Self::generate_typed_class_of`].
    pub(in crate::core_erlang) fn class_of_object_call_doc(
        receiver_doc: Document<'static>,
    ) -> Document<'static> {
        docvec![
            "call 'beamtalk_primitive':'class_of_object'(",
            receiver_doc,
            ")",
        ]
    }

    /// BT-4127: `obj class` for a receiver typed as a tagged-map class
    /// ([`TypedSendTarget::Tagged`], decided by [`Self::typed_send_target`]):
    /// the class object built inline behind [`Self::class_tag_guarded_doc`]'s
    /// tag guard, instead of `class_of_object/1`'s derived-cache lookup.
    ///
    /// ```erlang
    /// let R = <receiver> in
    /// case call 'erlang':'is_map'(R) of
    ///     <'true'> when 'true' ->
    ///         case call 'maps':'get'('$beamtalk_class', R, 'undefined') of
    ///             <'Pt'> when 'true' ->
    ///                 case call 'erlang':'whereis'('beamtalk_class_Pt') of
    ///                     <'undefined'> when 'true' ->
    ///                         call 'beamtalk_primitive':'class_of_object'(R)
    ///                     <Pid> when 'true' ->
    ///                         {'beamtalk_object', 'Pt class', 'bt@pt', Pid}
    ///                 end
    ///             <NoMatch> when 'true' -> call 'beamtalk_primitive':'class_of_object'(R)
    ///         end
    ///     <'false'> when 'true' -> call 'beamtalk_primitive':'class_of_object'(R)
    ///     ...
    /// end
    /// ```
    ///
    /// On a tag match this is the tuple `class_of_object_by_name/1` derives
    /// (`resolve_class_object/1`): the tag is `metaclass_tag` of the class
    /// name (pinned to `class_object_tag/1` by the shared conformance
    /// fixture), the pid is looked up live on every evaluation (a restarted
    /// class process is found), and the module is the compile-time pin of
    /// BT-4069's direct `dispatch/3` with the same trust model (see
    /// [`Self::typed_tagged_target`]): a same-module redefinition or reload
    /// keeps it current. A removed class (`whereis` answers `'undefined'`)
    /// and every guard miss take the dynamic call, so a lying annotation, a
    /// subclass instance or a future changes nothing.
    ///
    /// Known limitation (BT-4200, shared with the direct `dispatch/3`): a class
    /// redefined into a *different* module after this unit compiled leaves
    /// the pin stale, and the answer then differs from the class reference
    /// in its module field until the caller is recompiled.
    pub(in crate::core_erlang) fn generate_typed_class_of(
        &mut self,
        receiver: &Expression,
        target: &TypedTaggedTarget,
    ) -> Result<Document<'static>> {
        let mut seq = self.sequence_call(&[receiver], "Obj")?;
        let receiver_doc = seq.next();
        let receiver_var = self.fresh_temp_var("TClassRecv");
        let pid_var = self.fresh_temp_var("TClassPid");
        let fallback_doc = Self::class_of_object_call_doc(leaf::var(receiver_var.clone()));
        let class_object_doc = docvec![
            "case ",
            Self::registered_class_pid_doc(&target.class_name),
            " of <'undefined'> when 'true' -> ",
            fallback_doc.clone(),
            " <",
            leaf::var(pid_var.clone()),
            "> when 'true' -> ",
            Self::class_object_tuple_doc(
                &target.class_name,
                leaf::atom(target.module_name.clone()),
                leaf::var(pid_var),
            ),
            " end",
        ];
        let case_doc = self.class_tag_guarded_doc(
            &receiver_var,
            &target.class_name,
            class_object_doc,
            fallback_doc,
        );
        let body = docvec![
            "let ",
            leaf::var(receiver_var),
            " = ",
            receiver_doc,
            " in ",
            case_doc
        ];
        Ok(seq.close(self, body, "ClassRes"))
    }

    /// Generates a cast (fire-and-forget) message send.
    ///
    /// Called when the AST `MessageSend` node has `is_cast: true` (the `!` suffix).
    ///
    /// # Dispatch Strategy
    ///
    /// - **All sends, including `self someMethod!`** (BT-3998): Routes through `beamtalk_message_dispatch:cast/3`, which
    ///   extracts the actor PID and calls `beamtalk_actor:cast_send/3`. Non-actor
    ///   receivers are silently ignored.
    ///
    /// Cast sends always evaluate to `'ok'`.
    pub(super) fn generate_cast_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Every cast, including `self foo!` (BT-3998), routes through
        // beamtalk_message_dispatch:cast/3 to the receiver's mailbox. A
        // self-cast therefore lands after the current message (what `!`
        // means), instead of running now and dropping its State.
        self.generate_runtime_cast(receiver, selector, arguments)
    }

    /// Generates unified runtime cast via `beamtalk_message_dispatch:cast/3`.
    ///
    /// Fire-and-forget path: routes to the actor's message queue via
    /// `beamtalk_actor:cast_send/3`. Non-actor receivers are silently ignored.
    /// Always returns `'ok'`.
    fn generate_runtime_cast(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();
        // Capture receiver + args as one ordered sub-expression
        // sequence so left-to-right evaluation order is preserved when ANY
        // sub-expression has an open scope from a class method self-send.
        // capture_subexpr_sequence force-hoists every sub-expression in that
        // case; the fast path (no open scopes) leaves them inline.
        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        let mut seq = self.sequence_call(&all_exprs, "Cast")?;
        let actual_receiver = seq.next();
        let args_doc = Self::join_docs_with_commas(seq.rest());

        let call_doc = docvec![
            "call 'beamtalk_message_dispatch':'cast'(",
            actual_receiver,
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])",
        ];

        Ok(seq.close(self, call_doc, "CastRes"))
    }

    /// BT-4070: the `beamtalk_message_dispatch:send/3` call itself, over
    /// already-compiled operand documents — the one construction of the
    /// runtime-dispatch call shape, shared by
    /// [`Self::generate_runtime_dispatch`] (which sequences `&Expression`
    /// operands first) and the pure-conditional fallback arm (whose receiver
    /// is the precomputed `Cond` variable and whose closures compile where
    /// the arm is taken). The call is byte-identical however the operands
    /// were sourced; only the sourcing differs.
    ///
    /// BT-4366: the call carries the enclosing send's line itself — the
    /// pure-conditional fallback arm nests it inside a `case`.
    pub(in crate::core_erlang) fn runtime_send_call(
        &self,
        receiver_doc: Document<'static>,
        selector_atom: &str,
        arg_docs: Vec<Document<'static>>,
    ) -> Document<'static> {
        self.annotate_current_send_call(docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            receiver_doc,
            ", ",
            leaf::atom(selector_atom),
            ", [",
            Self::join_docs_with_commas(arg_docs),
            "])"
        ])
    }

    /// Generates unified runtime dispatch via `beamtalk_message_dispatch:send/3`.
    ///
    /// This is the fallback path for messages that don't match any compiler intrinsic.
    /// Routes through the unified entry point which handles actors (sync via `gen_server:call`),
    /// class objects (sync), and primitives (sync). Returns a value directly — no Future
    /// wrapping (ADR 0043).
    fn generate_runtime_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();
        if matches!(selector, MessageSelector::Binary(_)) {
            return Err(CodeGenError::Internal(format!(
                "unexpected binary selector in generate_message_send: {selector_atom}"
            )));
        }

        // Emit the dynamic-site report entry (BT-4292): the receiver's static
        // type and the gate that kept the send dynamic.
        if self.codegen_diagnostics_enabled {
            let span = receiver.span();
            let line_info = self
                .span_to_line(span)
                .map_or(String::new(), |l| format!(" at line {l}"));
            let description = self.dynamic_site_description(receiver, selector);
            self.emit_codegen_diagnostic(
                format!("Send '{selector_atom}'{line_info}: dynamic dispatch ({description})"),
                span,
            );
        }

        // Capture receiver + args as one ordered sub-expression
        // sequence so left-to-right evaluation order is preserved.
        let mut all_exprs: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
        all_exprs.push(receiver);
        for arg in arguments {
            all_exprs.push(arg);
        }
        // BT-4524 (ADR 0146 P0): a send no bracket covers (a conditional the
        // compiler did not inline, say) that passes a block holding a `^`
        // stores the state an inline `^` here would store, right before the
        // call, so the closure's throw (which carries the cell) carries it.
        let nlr_blocks: Vec<&beamtalk_core::ast::Block> = all_exprs
            .iter()
            .filter_map(|e| match e {
                Expression::Block(block) => Some(block),
                _ => None,
            })
            .collect();
        // BT-4532: emitted as a store-only `SyncedCall` around the call, and
        // covering only those block literals (`with_nlr_closure_cover`).
        // Whether it stores is decided before the operands compile (the
        // cover depends on it); what it stores is read after them, where the
        // store is emitted, since an operand may step `State` (a
        // field-writing self-send) and the store must carry that step.
        let at = receiver.span();
        let prestore = if self.synced.window_depth == 0 {
            self.nlr_closure_prestore(&nlr_blocks, at)
        } else {
            None
        };
        let cover: Vec<_> = if prestore.is_some() {
            nlr_blocks.iter().map(|block| block.span).collect()
        } else {
            Vec::new()
        };
        let mut seq =
            self.with_nlr_closure_cover(&cover, |this| this.sequence_call(&all_exprs, "Disp"))?;
        let actual_receiver = seq.next();

        // BT-4070: the call shape itself lives in `runtime_send_call`,
        // shared with the pure-conditional fallback arm — byte-identical
        // to the inline splice this replaces.
        let call_doc = self.runtime_send_call(actual_receiver, &selector_atom, seq.rest());
        let call_doc = match prestore {
            Some(prestore) => self.nlr_closure_prestored_call(prestore, call_doc),
            None => call_doc,
        };

        Ok(seq.close(self, call_doc, "DispRes"))
    }

    /// Handles spawn, spawnWith:, await, awaitForever, and await: intrinsics.
    ///
    /// Returns `Some(())` if the message was handled, `None` if it should
    /// fall through to the next dispatch strategy.
    fn try_handle_spawn_await(
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
        if let Expression::ClassReference { name, package, .. } = receiver {
            if let Some(variant) = SpawnVariant::of_selector(&selector.name(), arguments.len()) {
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
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "await:" && arguments.len() == 1 {
                let doc = self.generate_await_with_timeout(receiver, &arguments[0])?;
                return Ok(Some(doc));
            }
        }

        Ok(None)
    }

    /// ADR 0028: Handles `Erlang` class reference for BEAM interop.
    ///
    /// Two cases are handled:
    ///
    /// 1. **Direct call optimization (ADR 0028 Phase 4):** When the
    ///    receiver is `MessageSend(ClassReference("Erlang"), Unary(module))` and
    ///    the outer selector is a function call, emits a direct BEAM call:
    ///    ```erlang
    ///    call 'lists':'reverse'(Xs)
    ///    ```
    ///    This eliminates proxy map allocation entirely.
    ///
    /// 2. **Proxy construction:** When the receiver is
    ///    `ClassReference("Erlang")` and the message is a unary module name,
    ///    generates an inline `ErlangModule` proxy map:
    ///    ```erlang
    ///    ~{'$beamtalk_class' => 'ErlangModule', 'module' => 'lists'}~
    ///    ```
    ///    This fallback handles `proxy := Erlang lists` (standalone proxy).
    ///
    /// Standard class-protocol selectors (e.g. `class`, `new`, `superclass`)
    /// fall through to normal class dispatch so that `Erlang class` returns the
    /// metaclass rather than a proxy for module `'class'`.
    ///
    /// FFI receiver recognition (the class-protocol filter, the
    /// package-qualification check, and parenthesized-receiver peeling) is
    /// centralized in [`beamtalk_core::ffi_receiver`] — this is the only place those
    /// rules are implemented.
    fn try_handle_erlang_interop(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // Direct call optimization — `Erlang lists reverse: xs` (and the
        // parenthesized `(Erlang lists) reverse: xs`) → `call 'lists':'reverse'(Xs)`
        // with no proxy map allocation. Only when the module name is a
        // compile-time literal (ClassReference path).
        if let Some(module_name) = beamtalk_core::ffi_receiver::erlang_module_of_receiver(receiver)
        {
            return self.generate_direct_erlang_call(module_name, selector, arguments);
        }

        // Proxy construction — `Erlang lists` → inline proxy map
        if let Expression::ClassReference { name, package, .. } = receiver {
            if package.is_some() || name.name != "Erlang" {
                return Ok(None);
            }
            match selector {
                MessageSelector::Unary(module_name)
                    if !beamtalk_core::ffi_receiver::is_class_protocol_selector(module_name) =>
                {
                    let doc = docvec![
                        "~{'$beamtalk_class' => 'ErlangModule', 'module' => ",
                        leaf::atom(module_name.clone()),
                        "}~",
                    ];
                    Ok(Some(doc))
                }
                _ => {
                    // Keyword/binary on Erlang class itself, or a class-protocol
                    // selector, falls through to normal class dispatch.
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }

    /// Generates a proxy-routed call for Erlang interop.
    ///
    /// Converts Beamtalk selectors to Erlang function names and routes through
    /// `beamtalk_erlang_proxy:direct_call/4` for automatic binary→charlist coercion:
    /// - Unary: `node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [], 'stock')` (zero-arg)
    /// - Keyword single: `reverse:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'reverse', [Xs], 'stock')`
    /// - Keyword multi: `seq:with:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'seq', [1, 10], 'stock')`
    ///
    /// The trailing mode atom is the `stock`/`beamtalk` coercion mode,
    /// precomputed from the statically-known module (BT-4153).
    ///
    /// Returns `None` for selectors that are Object/ProtoObject protocol methods
    /// (e.g. `printString`, `asString`) — these must go through runtime dispatch
    /// so the proxy's inherited protocol methods are called, not a non-existent
    /// Erlang function.
    ///
    /// Block literal arguments are generated by
    /// [`Self::generate_erlang_interop_block`]: always Tier 1, since a block
    /// that writes a captured local is a compile error (ADR 0133, BT-3917).
    fn generate_direct_erlang_call(
        &mut self,
        module_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        /// Object protocol selectors that must NOT be optimized as direct Erlang
        /// calls. These are inherited from ProtoObject/Object and handled by
        /// runtime dispatch. Selectors already handled as compiler intrinsics
        /// (class, isNil, notNil, hash, yourself, respondsTo:, error:) never
        /// reach here — they're intercepted earlier in the dispatch chain.
        const OBJECT_PROTOCOL_SELECTORS: &[&str] = &["printString", "asString", "inspect"];

        match selector {
            MessageSelector::Unary(function_name) => {
                if OBJECT_PROTOCOL_SELECTORS.contains(&function_name.as_str()) {
                    return Ok(None);
                }
                if let Some(doc) = self.proved_direct_ffi_call(module_name, function_name, &[])? {
                    return Ok(Some(doc));
                }
                // Route zero-arg calls through proxy (consistent with keyword sends).
                // `Erlang erlang node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [], 'stock')`
                let mode = super::util::ffi_coercion_mode(module_name);
                let doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'(",
                    leaf::atom(module_name.to_string()),
                    ", ",
                    leaf::atom(function_name.to_string()),
                    ", [], ",
                    leaf::atom(mode),
                    ")"
                ];
                Ok(Some(doc))
            }
            MessageSelector::Keyword(parts) => {
                // Extract function name from first keyword (before the colon)
                let function_name = parts[0].keyword.trim_end_matches(':');

                if let Some(doc) =
                    self.proved_direct_ffi_call(module_name, function_name, arguments)?
                {
                    return Ok(Some(doc));
                }

                // Process arguments individually so Block arguments can be
                // generated as Tier 1 funs (generate_erlang_interop_block) before crossing the
                // Erlang boundary. Non-block arguments pass through unchanged.
                let mut preamble_docs: Vec<Document<'static>> = Vec::new();
                let mut arg_parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
                // BT-4296: the arguments land in a list, which erlc evaluates
                // last-first, so every argument before the last one that may
                // run code is bound to a temp in source order.
                let arg_exprs: Vec<&Expression> = arguments.iter().collect();
                let racing_last = self.last_racing_operand(&arg_exprs);

                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        arg_parts.push(Document::Str(", "));
                    }
                    if let Some(block) = Self::extract_block_literal(arg) {
                        let wrapped_doc = self.generate_erlang_interop_block(block)?;
                        // Bind the wrapper to a temp var to avoid repeating complex exprs.
                        let wrapper_var = self.fresh_temp_var("ErlWrapper");
                        preamble_docs.push(docvec![
                            "let ",
                            leaf::var(wrapper_var.clone()),
                            " = ",
                            wrapped_doc,
                            " in "
                        ]);
                        arg_parts.push(leaf::var(wrapper_var));
                    } else if racing_last.is_some_and(|last| i < last)
                        && !Self::is_trivial_subexpr(arg)
                    {
                        let doc = self.expression_doc(arg)?;
                        let (binding, var) = self.bind_subexpr_to_temp("ErlArg", doc);
                        preamble_docs.push(binding);
                        arg_parts.push(leaf::var(var));
                    } else {
                        arg_parts.push(self.expression_doc(arg)?);
                    }
                }

                // Route through beamtalk_erlang_proxy:direct_call/4 to
                // enable binary→charlist coercion for functions like os:cmd/1.
                // Args are wrapped in a list: call 'proxy':'direct_call'('M','F',[args],Mode)
                let mode = super::util::ffi_coercion_mode(module_name);
                let call_doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'(",
                    leaf::atom(module_name.to_string()),
                    ", ",
                    leaf::atom(function_name.to_string()),
                    ", [",
                    Document::Vec(arg_parts),
                    "], ",
                    leaf::atom(mode),
                    ")"
                ];

                let doc = if preamble_docs.is_empty() {
                    call_doc
                } else {
                    docvec![Document::Vec(preamble_docs), call_doc]
                };

                Ok(Some(doc))
            }
            MessageSelector::Binary(_) => {
                // Binary operators on Erlang module proxy — fall through to runtime
                Ok(None)
            }
        }
    }

    /// BT-4485: the direct remote call for an inline FFI send whose result the
    /// imported spec proves needs no coercion and whose arguments are proved
    /// to reach the callee unchanged — `None` keeps the proxy.
    ///
    /// Both proofs are the FFI type table's
    /// ([`NativeTypeRegistry::proves_no_result_coercion`] and
    /// [`ffi_arg_passes_unchanged`] on each argument's checked type); this
    /// only asks them. Static types are not enforced at run time, so each
    /// argument is also checked with the table's
    /// [`FFI_ARG_RUNTIME_GUARDS`] and the call falls back to
    /// `direct_call/4` when one fails. The call keeps the proxy's error
    /// translation: a raise is handed, with the arguments and the call
    /// site's frame (read only on that path), to
    /// `beamtalk_erlang_proxy:ffi_call_raised/8`, which runs the same
    /// classifier (and `badarg` charlist retry) as `direct_call/4`.
    /// Arguments are bound before the `try`, so a raise while evaluating one
    /// is not translated, as on the proxy path.
    ///
    /// ```text
    /// let <A1> = <arg1> in
    /// case <> of
    ///   <> when is_number(A1) or is_atom(A1) or is_pid(A1) ->
    ///     try call 'M':'F'(A1)
    ///     of <R> -> R
    ///     catch <C, E, Raw> ->
    ///       let S = primop 'build_stacktrace'(Raw) in
    ///       let Site = call 'erlang':'process_info'(self(), 'current_stacktrace') in
    ///       call 'beamtalk_erlang_proxy':'ffi_call_raised'(C, E, S, 'M', 'F', [A1], Mode, Site)
    ///   <> when 'true' -> call 'beamtalk_erlang_proxy':'direct_call'('M', 'F', [A1], Mode)
    /// end
    /// ```
    ///
    /// [`NativeTypeRegistry::proves_no_result_coercion`]: beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::proves_no_result_coercion
    /// [`ffi_arg_passes_unchanged`]: beamtalk_core::semantic_analysis::type_checker::ffi_arg_passes_unchanged
    /// [`FFI_ARG_RUNTIME_GUARDS`]: beamtalk_core::semantic_analysis::type_checker::FFI_ARG_RUNTIME_GUARDS
    fn proved_direct_ffi_call(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        let Some(registry) = self.native_type_registry.as_ref() else {
            return Ok(None);
        };
        let Ok(arity) = u8::try_from(arguments.len()) else {
            return Ok(None);
        };
        if !registry.proves_no_result_coercion(module_name, function_name, arity) {
            return Ok(None);
        }
        let args_proved = arguments.iter().all(|arg| {
            self.type_map
                .get(arg.span())
                .is_some_and(ffi_arg_passes_unchanged)
        });
        if !args_proved {
            return Ok(None);
        }

        let mut bindings: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        let mut arg_vars: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        let mut arg_names: Vec<String> = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            let var = self.fresh_temp_var("FfiArg");
            bindings.push(docvec![
                "let ",
                leaf::var(var.clone()),
                " = ",
                self.expression_doc(arg)?,
                " in "
            ]);
            if i > 0 {
                arg_vars.push(Document::Str(", "));
            }
            arg_vars.push(leaf::var(var.clone()));
            arg_names.push(var);
        }
        let module_atom = leaf::atom(module_name.to_string());
        let function_atom = leaf::atom(function_name.to_string());
        let mode_atom = leaf::atom(super::util::ffi_coercion_mode(module_name));
        let direct = self.ffi_try_direct_doc(&module_atom, &function_atom, &mode_atom, &arg_vars);
        if arg_names.is_empty() {
            return Ok(Some(docvec![Document::Vec(bindings), direct]));
        }
        // Static types are not enforced at run time: check each argument with
        // the table's runtime guards and keep the proxy for anything else.
        let guard = ffi_arg_runtime_guard(&arg_names);
        Ok(Some(docvec![
            Document::Vec(bindings),
            "case <> of <> when ",
            guard,
            " -> ",
            direct,
            " <> when 'true' -> call 'beamtalk_erlang_proxy':'direct_call'(",
            module_atom,
            ", ",
            function_atom,
            ", [",
            Document::Vec(arg_vars),
            "], ",
            mode_atom,
            ") end",
        ]))
    }

    /// The `try` of a direct FFI call (see [`Self::proved_direct_ffi_call`]):
    /// the remote call, and on any exception the hand-off to
    /// `beamtalk_erlang_proxy:ffi_call_raised/8` with the built stacktrace and
    /// the call site's frame.
    fn ffi_try_direct_doc(
        &mut self,
        module_atom: &Document<'static>,
        function_atom: &Document<'static>,
        mode_atom: &Document<'static>,
        arg_vars: &[Document<'static>],
    ) -> Document<'static> {
        let result_var = self.fresh_temp_var("FfiRes");
        let cls_var = self.fresh_temp_var("FfiCls");
        let err_var = self.fresh_temp_var("FfiErr");
        let stk_var = self.fresh_temp_var("FfiStk");
        let built_var = self.fresh_temp_var("FfiBuiltStk");
        let self_var = self.fresh_temp_var("FfiSelf");
        let site_var = self.fresh_temp_var("FfiSite");
        docvec![
            "try call ",
            module_atom.clone(),
            ":",
            function_atom.clone(),
            "(",
            Document::Vec(arg_vars.to_vec()),
            ") of <",
            leaf::var(result_var.clone()),
            "> -> ",
            leaf::var(result_var),
            " catch <",
            leaf::var(cls_var.clone()),
            ", ",
            leaf::var(err_var.clone()),
            ", ",
            leaf::var(stk_var.clone()),
            "> -> let ",
            leaf::var(built_var.clone()),
            " = primop 'build_stacktrace'(",
            leaf::var(stk_var),
            ") in let ",
            leaf::var(self_var.clone()),
            " = call 'erlang':'self'() in let ",
            leaf::var(site_var.clone()),
            " = call 'erlang':'process_info'(",
            leaf::var(self_var),
            ", 'current_stacktrace') in call 'beamtalk_erlang_proxy':'ffi_call_raised'(",
            leaf::var(cls_var),
            ", ",
            leaf::var(err_var),
            ", ",
            leaf::var(built_var),
            ", ",
            module_atom.clone(),
            ", ",
            function_atom.clone(),
            ", [",
            Document::Vec(arg_vars.to_vec()),
            "], ",
            mode_atom.clone(),
            ", ",
            leaf::var(site_var),
            ")",
        ]
    }

    /// ADR 0109: lower `File open:do:` / `File open:mode:do:` to a
    /// direct call rather than a class send, so the block runs in the caller.
    ///
    /// A class send is a `gen_server:call` into the singleton class process, so
    /// the method body — *including the user's block* — executes there. For a
    /// block-scoped resource method that is three separate problems: the block
    /// cannot message `File` again (deadlock), it holds the class process for
    /// its whole duration (every `File` call in the node queues behind it), and
    /// it must finish inside the 60-second class-call timeout.
    ///
    /// Emitting the same `native_call` the class-method body would have emitted
    /// — just here, in the caller — removes all three. The Erlang side is
    /// unchanged: `beamtalk_file:open/2,3` still performs the open, the
    /// `try`/`after` and the close, so the intercepted path and the
    /// `perform:`-style dynamic path stay semantically identical.
    ///
    /// Deliberately a hard-coded selector list, per ADR 0109's "Not in scope":
    /// the general mechanism (a continuation protocol for any Block-taking class
    /// method) would change the hottest dispatch path in the language to benefit
    /// the ~1% of class methods that take a Block. A fourth block-scoped method
    /// is the trigger to revisit that.
    ///
    /// Scoped to the unqualified stdlib `File`: a package-qualified receiver
    /// (`mylib@File open: p do: blk`) is some other class that happens to share
    /// the name, and must keep its own implementation. Same reasoning as the
    /// `pkg.is_none()` guard on the self-send case below.
    fn try_generate_block_scoped_open(
        &mut self,
        class_name: &str,
        package: Option<&str>,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if class_name != "File" || package.is_some() {
            return Ok(None);
        }
        let selector_atom = selector.name().to_string();
        if !matches!(selector_atom.as_str(), "open:do:" | "open:mode:do:") {
            return Ok(None);
        }

        let mut arg_docs = Vec::with_capacity(arguments.len());
        for argument in arguments {
            arg_docs.push(self.expression_doc(argument)?);
        }
        let args = Self::join_docs_with_commas(arg_docs);

        // Mirrors `native_delegate_body_doc`: the Erlang shim name is the first
        // keyword without its colon, and the `{Class, Selector}` context makes a
        // wrapped error read `File>>open:mode:do:` rather than a bare MFA.
        // The mode is computed, not hardcoded, so a rename of the shim or a
        // prefix-rule change cannot diverge the two (BT-4153 review).
        let mode = super::util::ffi_coercion_mode("beamtalk_file");
        Ok(Some(docvec![
            "call 'beamtalk_erlang_proxy':'native_call'(",
            leaf::atom("beamtalk_file"),
            ", ",
            leaf::atom("open"),
            ", [",
            args,
            "], {",
            leaf::atom("File"),
            ", ",
            leaf::atom(selector_atom),
            "}, ",
            leaf::atom(mode),
            ")"
        ]))
    }

    /// Handles `ClassReference` receivers as class method calls.
    ///
    /// ADR 0019 Phase 3: In REPL context, checks REPL bindings first for
    /// convenience names (Transcript, Beamtalk, Workspace). If found in bindings,
    /// dispatches via `beamtalk_message_dispatch:send/3` (instance dispatch).
    /// Falls back to `class_send` for actual class names.
    ///
    /// In actor/value-type methods compiled in workspace mode, uses `class_send`
    /// with fallback to workspace binding for convenience names.
    ///
    /// Returns `Some(doc)` if the receiver is a `ClassReference`, `None` otherwise.
    fn try_handle_class_reference(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if let Expression::ClassReference { name, package, .. } = receiver {
            let pkg = package.as_ref().map(|p| p.name.as_str());
            // ADR 0109: block-scoped `File open:…do:` must not reach
            // the File class gen_server, or the user's block runs there. Checked
            // ahead of every class-send path below, because the deadlock, the
            // serialization and the 60s class-call ceiling apply to all of them.
            if let Some(doc) =
                self.try_generate_block_scoped_open(&name.name, pkg, selector, arguments)?
            {
                return Ok(Some(doc));
            }
            // BT-4317: inside a class method, an explicit send to the class
            // being compiled (`Base foo`) denotes `Base` whatever the receiver:
            // the own-class direct path (which avoids a `gen_server:call` back
            // into the running class process) when the receiver is `Base`, an
            // ordinary literal class send otherwise.
            if super::expr_shape::is_own_class_reference(&self.shape_ctx(), receiver) {
                // ADR 0118 phase 5b: reached through ordinary
                // `generate_expression`/`generate_message_send` (not
                // `threaded_expression`'s own producer recognition), so the
                // producer's prelude is closed inline into a self-contained
                // `Document` here rather than left open.
                let tv = self.generate_own_class_literal_send(&name.name, selector, arguments)?;
                return Ok(Some(self.close_threaded_value_doc(tv)));
            }
            // ADR 0129 §7: only REPL top-level expressions can name a session
            // binding; everything else resolves the name as a class.
            if self.context == CodeGenContext::Repl {
                let doc = self.generate_repl_bound_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            // ADR 0070 Phase 2: Class method calls always go through the class
            // registry using the short class name. The package qualifier doesn't
            // affect dispatch — it's used for module name resolution in spawns and
            // standalone references, not for class method calls.
            let doc = self.generate_class_method_call(
                &name.name,
                selector,
                ClassSendArgs::Exprs(arguments),
            )?;
            return Ok(Some(doc));
        }
        Ok(None)
    }

    /// An explicit own-class send (`Base foo` inside a class method of `Base`),
    /// BT-4317: a class literal always denotes that class, so `foo` runs with
    /// `self == Base` even when the method running is inherited by a subclass
    /// `C` (its `ClassSelf` is `C`'s reference). Only `self foo` late-binds to
    /// the receiver.
    ///
    /// ```erlang
    /// let <Arg1> = … in
    /// case call 'erlang':'element'(2, ClassSelf) of
    ///   <'Base class'> when 'true' -> <own-class direct send, Arg1>
    ///   <_> when 'true' -> <literal class send to 'Base', Arg1>
    /// end
    /// ```
    ///
    /// When the receiver is `Base`, the own-class lowering of
    /// [`Self::generate_class_method_self_send`] runs it directly in this
    /// process: for a class with class variables that process is `Base`'s own,
    /// and a `class_send` would `gen_server:call` itself. Otherwise the send is
    /// the ordinary literal class send ([`Self::generate_class_method_call`]):
    /// in the caller for a stateless
    /// chain, in `Base`'s class process (not the one running) when the chain
    /// declares class variables. The arguments are bound once, ahead of the
    /// `case`, and both arms read the same variables.
    fn generate_own_class_literal_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<ThreadedValue> {
        let (args_preamble, arg_refs) = self.thread_args_bound(arguments, "Arg")?;
        let bound = ClassSendArgs::Bound(&arg_refs);
        let own_class = self.generate_class_method_self_send(selector, bound, false)?;
        let own_class_doc = self.close_threaded_value_doc(own_class);
        let literal_doc = self.generate_class_method_call(class_name, selector, bound)?;
        let call_doc = docvec![
            "case call 'erlang':'element'(2, ",
            leaf::var(CLASS_SELF_VAR),
            ") of <",
            leaf::atom(super::util::metaclass_tag(class_name)),
            "> when 'true' -> ",
            own_class_doc,
            " <_> when 'true' -> ",
            literal_doc,
            " end"
        ];
        Ok(self.class_self_send_value(&args_preamble, call_doc))
    }

    /// Handles self-sends inside actor methods.
    ///
    /// Returns `Some(())` if the receiver is `self` in an Actor context, `None` otherwise.
    ///
    /// ADR 0118 phase 2b: every position that threads a
    /// dispatching self-send's `NewState` now compiles it through
    /// [`Self::generate_self_dispatch`]'s producer directly (via
    /// `threaded_expression`/`thread_ahead`), substituting the already-
    /// sequenced value via `precompiled_subexprs` before `generate_message_send`
    /// ever reaches this function — so a self-send that gets here always
    /// falls through to [`Self::generate_discarding_self_dispatch`], the
    /// same *discarding* dispatch every un-migrated position always used.
    fn try_handle_self_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // ADR 0137 §3: a self-send in an actor closure is an ordinary message
        // to `Self` (the cell is current at home; a message to the actor away),
        // handled by the generic dispatch below.
        if self.context == CodeGenContext::Actor && !self.in_cell_closure() {
            if let Expression::Identifier(id) = receiver {
                if id.name == "self" {
                    let doc = self.generate_discarding_self_dispatch(selector, arguments)?;
                    return Ok(Some(doc));
                }
            }
        }
        Ok(None)
    }

    /// Handles self-sends in class method context.
    ///
    /// When a class method sends a message to `self` (the class object),
    /// we call the module function directly (not through `gen_server`) to avoid
    /// deadlock since class methods execute inside a `gen_server:call` handler.
    ///
    /// For user-defined class methods, generates `class_<selector>(ClassSelf, ...)`.
    /// For built-in exports (spawn, new, etc.), generates `module:selector(...)`.
    fn try_handle_class_method_self_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if !self.in_class_method() {
            return Ok(None);
        }
        if let Expression::Identifier(id) = receiver {
            if id.name == "self" {
                // ADR 0118 phase 5b: reached through ordinary
                // `generate_expression`, not `threaded_expression`'s own
                // producer recognition — close the prelude inline.
                let tv = self.generate_class_method_self_send(
                    selector,
                    ClassSendArgs::Exprs(arguments),
                    true,
                )?;
                return Ok(Some(self.close_threaded_value_doc(tv)));
            }
        }
        Ok(None)
    }

    /// Core logic for direct dispatch of class method calls.
    ///
    /// Used by `self` sends (`receiver_is_self`) and, for the arm where the
    /// running receiver is this class, explicit own-class sends
    /// ([`Self::generate_own_class_literal_send`], BT-4317) within class
    /// methods. Generates direct module function calls to avoid deadlock
    /// since class methods may execute inside a `gen_server:call` handler.
    ///
    /// ADR 0118 phase 5b: returns a [`ThreadedValue`] whose
    /// prelude is real `ThreadedStmt`s throughout — every branch threads
    /// its arguments via [`Self::thread_args`] and closes the argument
    /// prelude into a self-contained call `Document` (
    /// [`Self::class_self_send_value`] / [`Self::close_prelude`]), wrapped as
    /// a pure `ThreadedValue` — a class-side send passes and rebinds nothing
    /// (ADR 0130 §3).
    #[allow(clippy::too_many_lines)] // Multiple dispatch branches share args-capture scaffolding.
    pub(super) fn generate_class_method_self_send(
        &mut self,
        selector: &MessageSelector,
        arguments: ClassSendArgs<'_>,
        receiver_is_self: bool,
    ) -> Result<ThreadedValue> {
        let selector_atom = selector.name().to_string();

        // ADR 0084: inside a programmatic ClassBuilder class-method fun
        // there is no `class_<sel>` module export to call, so self-sends route
        // through the runtime dispatch helper (own runtime fun first, then the
        // super/inherited chain). Instantiation intrinsics (self new / spawn)
        // still use the process-dict-backed helpers (no export needed).
        //
        // ADR 0130 §3: a class-side self-send of any kind (this one, a direct
        // call, a hierarchy walk, `super`, an own-class reference) passes
        // nothing and rebinds nothing; its reply is the bare result.
        if let Some(builder_class) = self.builder_class_method_class() {
            if let Some(doc) = self.try_instantiation_intrinsic(&selector_atom, arguments)? {
                return Ok(ThreadedValue {
                    prelude: Vec::new(),
                    value: ValueRef::Doc(doc),
                });
            }
            let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;
            // BT-4301: the fun's own `ClassSelf` is the live receiver, passed
            // as is (no registry read rebuilds it).
            let call_doc = docvec![
                "call 'beamtalk_class_dispatch':'class_self_dispatch_local'(",
                leaf::atom(builder_class),
                ", ",
                leaf::atom(selector_atom),
                ", [",
                args_doc,
                "], ",
                leaf::var(CLASS_SELF_VAR),
                ")"
            ];
            return Ok(self.class_self_send_value(&args_preamble, call_doc));
        }

        // BT-3666 / BT-4301: a class-side send to a selector this class
        // defines calls `class_<sel>` directly only behind the
        // `class_self_direct_ok/4` guard, which holds while nothing shadows
        // the compiled method (a class-side extension, an ADR 0084 runtime
        // fun). Sealed classes and `class sealed` methods get the same guard:
        // no class-side direct call is unguarded (ADR 0142 §2), so an
        // extension shadowing one is honoured (BT-4133).
        //
        // `self foo` also requires the receiving class to be exactly this
        // class (an inherited method's `self foo` must reach a subclass
        // override, the template-method pattern); its fallback is the runtime
        // walk from the receiver. An explicit own-class reference (`Base foo`)
        // reaches here only when the receiver is this class (BT-4317,
        // `generate_own_class_literal_send`); it binds the method statically
        // to this class, so its guard passes this class's tag as the
        // receiver's, and its fallback resolves `foo` from this class (its
        // extension, its runtime fun, then its own method and the chain above
        // it). Both pass the method's own `ClassSelf`, the receiver.
        let defines_selector = self.class_method_selectors().contains(&selector_atom);
        if defines_selector {
            // Bind every argument once, before the guarded `case`: the doc is
            // spliced into both arms, and an inline block argument containing
            // further class-side self-sends would otherwise double per nesting
            // level (2^depth code growth).
            let (args_preamble, arg_refs) = self.thread_class_send_args_bound(arguments)?;
            let args_doc = Self::join_docs_with_commas(arg_refs);
            let class_name = self.class_name();
            let class_tag = super::util::metaclass_tag(&class_name);
            let (receiver_tag_doc, fallback_doc) = if receiver_is_self {
                (
                    docvec![
                        "call 'erlang':'element'(2, ",
                        leaf::var(CLASS_SELF_VAR),
                        ")"
                    ],
                    Self::class_self_send_call_doc(&selector_atom, args_doc.clone()),
                )
            } else {
                (
                    leaf::atom(class_tag.clone()),
                    docvec![
                        "call 'beamtalk_class_dispatch':'class_self_dispatch_local'(",
                        leaf::atom(class_name.clone()),
                        ", ",
                        leaf::atom(selector_atom.clone()),
                        ", [",
                        args_doc.clone(),
                        "], ",
                        leaf::var(CLASS_SELF_VAR),
                        ")"
                    ],
                )
            };
            let direct_doc = Self::class_direct_call_doc(
                &self.module_name,
                CLASS_SELF_VAR,
                &selector_atom,
                args_doc,
                !arguments.is_empty(),
            );
            let call_doc = docvec![
                "case call 'beamtalk_class_dispatch':'class_self_direct_ok'(",
                receiver_tag_doc,
                ", ",
                leaf::atom(class_tag),
                ", ",
                leaf::atom(class_name),
                ", ",
                leaf::atom(selector_atom.clone()),
                ") of <'true'> when 'true' -> ",
                direct_doc,
                " <_> when 'true' -> ",
                fallback_doc,
                " end"
            ];
            return Ok(self.class_self_send_value(&args_preamble, call_doc));
        }
        // Auto-generated keyword constructor for Value subclass: classes.
        // `ClassName slot: value` inside a class method routes here when the selector
        // matches the auto-generated slot keyword constructor (e.g. `symName:` → `class_symName:/2`).
        if self
            .class_slot_constructor_selector()
            .map(String::as_str)
            .is_some_and(|kw| kw == selector_atom)
        {
            let module = self.module_name.clone();
            let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;
            let comma = if arguments.is_empty() { "" } else { ", " };
            // Hash long keyword constructor atoms to stay within
            // Erlang's 255-char atom limit.
            let safe_fn = super::selector_mangler::safe_class_method_fn_name(&selector_atom);
            // The generated keyword constructor ignores its class-self
            // argument (`fun (_ClassSelf, ...)`, `value_accessors`); it gets
            // the receiver like every other class method (BT-4301: no `nil`
            // class self).
            let call_doc = docvec![
                "call ",
                leaf::atom(module),
                ":",
                leaf::atom(safe_fn),
                "(",
                leaf::var(CLASS_SELF_VAR),
                comma,
                args_doc,
                ")"
            ];
            let doc = self.close_prelude(&args_preamble, call_doc, "Slot");
            return Ok(ThreadedValue {
                prelude: Vec::new(),
                value: ValueRef::Doc(doc),
            });
        }
        // Instantiation selectors (new, new:, spawn, spawnWith:) must bypass
        // gen_server to avoid deadlock — route through class_self_new/class_self_spawn
        // (and class_self_spawn_as/class_self_spawn_with for the named-
        // registration variants).
        if let Some(doc) = self.try_instantiation_intrinsic(&selector_atom, arguments)? {
            return Ok(ThreadedValue {
                prelude: Vec::new(),
                value: ValueRef::Doc(doc),
            });
        }

        // Inherited class method — walk the hierarchy at runtime and
        // apply the defining module's class_<sel>(ClassSelf, Args...).
        // The one remaining auto-generated 0-arity export reachable via plain
        // self-send (`class_name/0`) stays on the direct-call path: it returns
        // the raw atom straight from the module. The other auto-exports
        // (`method_table/0`, `has_method/1`, `register_class/0`,
        // `__beamtalk_meta/0`) are codegen-internal reflection and metadata
        // accessors without a stable user-level API, so they are deliberately
        // NOT on the direct-call path — `is_class_auto_export_selector` must
        // stay in sync with the actual reachable set. Anything else that falls
        // through here — inherited, Behaviour protocol, or missing — routes
        // through class_self_send/3: after the class-method chain misses it
        // dispatches through the class-object protocol (`Class` ->
        // `Behaviour` -> ...) exactly as an external `C name` does (BT-4252),
        // so `self name` / `self superclass` answer the same value, and it
        // raises a structured does_not_understand for genuine DNU.
        if is_class_auto_export_selector(&selector_atom, arguments.len()) {
            // Hoist preambles from sub-expression class var mutations.
            let module = self.module_name.clone();
            let fun_name = selector_atom.replace(':', "");
            let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;

            let call_doc = docvec![
                "call ",
                leaf::atom(module),
                ":",
                leaf::atom(fun_name),
                "(",
                args_doc,
                ")"
            ];
            let doc = self.close_prelude(&args_preamble, call_doc, "ClassFn");
            return Ok(ThreadedValue {
                prelude: Vec::new(),
                value: ValueRef::Doc(doc),
            });
        }

        let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;
        let call_doc = Self::class_self_send_call_doc(&selector_atom, args_doc);
        Ok(self.class_self_send_value(&args_preamble, call_doc))
    }

    /// [`Self::thread_args`] over [`ClassSendArgs`]: bound arguments need no
    /// threading, so their prelude is empty.
    fn thread_class_send_args(
        &mut self,
        args: ClassSendArgs<'_>,
    ) -> Result<(Vec<ThreadedStmt>, Document<'static>)> {
        match args {
            ClassSendArgs::Exprs(exprs) => self.thread_args(exprs),
            ClassSendArgs::Bound(docs) => {
                Ok((Vec::new(), Self::join_docs_with_commas(docs.to_vec())))
            }
        }
    }

    /// [`Self::thread_args_bound`] over [`ClassSendArgs`]: bound arguments
    /// already are variable references.
    fn thread_class_send_args_bound(
        &mut self,
        args: ClassSendArgs<'_>,
    ) -> Result<(Vec<ThreadedStmt>, Vec<Document<'static>>)> {
        match args {
            ClassSendArgs::Exprs(exprs) => self.thread_args_bound(exprs, "Arg"),
            ClassSendArgs::Bound(docs) => Ok((Vec::new(), docs.to_vec())),
        }
    }

    /// The value of a class-side self-send (ADR 0130 §3): the call's bare
    /// result, with any hoisted argument prelude closed into it. Nothing is
    /// passed to the callee and nothing is rebound.
    fn class_self_send_value(
        &mut self,
        args_preamble: &[ThreadedStmt],
        call_doc: Document<'static>,
    ) -> ThreadedValue {
        let doc = self.close_prelude(args_preamble, call_doc, "Send");
        ThreadedValue {
            prelude: Vec::new(),
            value: ValueRef::Doc(doc),
        }
    }

    /// Direct call `class_<sel>(<class_self>, Args...)` into this module. Long
    /// selectors are hashed to stay within Erlang's 255-char atom limit (same
    /// guard as the keyword-constructor path).
    fn class_direct_call_doc(
        module: &str,
        class_self: &str,
        selector_atom: &str,
        args_doc: Document<'static>,
        has_args: bool,
    ) -> Document<'static> {
        let safe_fn = super::selector_mangler::safe_class_method_fn_name(selector_atom);
        let comma = if has_args { ", " } else { "" };
        docvec![
            "call ",
            leaf::atom(module.to_string()),
            ":",
            leaf::atom(safe_fn),
            "(",
            leaf::var(class_self.to_string()),
            comma,
            args_doc,
            ")"
        ]
    }

    /// The runtime hierarchy-walk call behind every class-side self-send that
    /// is not bound statically: `class_self_send/3` starting at the
    /// receiving class (BT-3666: so a subclass override wins over the method
    /// compiled here).
    ///
    /// The receiver is the method's own `ClassSelf` (closure-captured, so
    /// correct even when this self-send executes inside a block running in
    /// another process, ADR 0109 amendment), passed as is, so the runtime
    /// builds nothing from a name and reads no registry (BT-4301).
    fn class_self_send_call_doc(
        selector_atom: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        docvec![
            "call 'beamtalk_class_dispatch':'class_self_send'(",
            leaf::var(CLASS_SELF_VAR),
            ", ",
            leaf::atom(selector_atom.to_string()),
            ", [",
            args_doc,
            "])"
        ]
    }

    /// ADR 0109 amendment: the class-name expression derived from
    /// `ClassSelf` (closure-captured, so correct even inside a block executing in
    /// a foreign class's process), for inlining at instantiation-intrinsic call
    /// sites. Deliberately inlined rather than let-bound: `close_prelude`
    /// treats *any* non-empty prelude as needing a closing `let` the caller
    /// must produce, which only the argument-hoisting prelude from
    /// `thread_args` is guaranteed to be consumed correctly for — a
    /// zero-argument call (e.g. bare `self new`) produces an empty args
    /// prelude and must stay a *closed* expression. Recomputing this
    /// cheap expression (a suffix check + `binary_to_existing_atom`) inline at
    /// each use — up to three times per call site for the `spawn`/`spawnAs:`/
    /// `spawnWith:as:` intrinsics, which also resolve `is_abstract` — is
    /// negligible and matches the pre-existing style at these same sites, which
    /// already repeated process-dictionary reads inline rather than hoisting
    /// them through a `let`.
    pub(super) fn class_self_name_doc() -> Document<'static> {
        docvec![
            "call 'beamtalk_primitive':'class_name_from_tag'(call 'erlang':'element'(2, ",
            leaf::var("ClassSelf"),
            "))"
        ]
    }

    /// ADR 0109 amendment: the calling class's own compiled module,
    /// resolved by name via `beamtalk_class_metadata:lookup_module/1` — **not**
    /// `element(3, ClassSelf)` (`class_mod`). That field is not reliably "the
    /// calling class's own module": at the inherited-class-method dispatch site
    /// (`beamtalk_class_dispatch:apply_class_method_in_context/6`), `ClassSelf`
    /// is constructed with `class_mod = DefiningModule` (the ancestor whose code
    /// is executing), while `class` (used by `class_self_name_doc`) is correctly
    /// the calling subclass's own tag. Using `class_mod` here would construct an
    /// instance via the wrong module (e.g. `Point new: aMap` building a bare
    /// `Value`-shaped map missing `x`/`y` — caught as a regression while
    /// implementing this amendment). `Selector` names the call for the
    /// structured error `resolve_module_or_raise/2` raises on a metadata miss.
    pub(super) fn class_self_module_doc(selector_atom: &str) -> Document<'static> {
        Self::class_module_lookup_doc(Self::class_self_name_doc(), selector_atom, None)
    }

    /// The call-time module lookup shared by the `ClassSelf` instantiation
    /// intrinsics ([`Self::class_self_module_doc`], `spawn_kind = None`) and
    /// spawn-family sends to a shadowed class reference
    /// ([`Self::class_reference_spawn_module_doc`], `spawn_kind = Some(_)`):
    ///
    /// ```erlang
    /// call 'beamtalk_class_instantiation':'resolve_module_or_raise'(Name, 'new')
    /// call 'beamtalk_class_instantiation':'resolve_spawn_module_or_raise'(Name, 'spawn', 'actor')
    /// ```
    ///
    /// The spawn form also checks that the runtime class is the kind the
    /// emitted call shape needs (BT-4404), raising a structured error instead
    /// of a raw `undef` when a last-wins shadowing class is a different kind.
    fn class_module_lookup_doc(
        name_doc: Document<'static>,
        selector_atom: &str,
        spawn_kind: Option<SpawnTargetKind>,
    ) -> Document<'static> {
        match spawn_kind {
            None => docvec![
                "call 'beamtalk_class_instantiation':'resolve_module_or_raise'(",
                name_doc,
                ", ",
                leaf::atom(selector_atom.to_string()),
                ")"
            ],
            Some(kind) => docvec![
                "call 'beamtalk_class_instantiation':'resolve_spawn_module_or_raise'(",
                name_doc,
                ", ",
                leaf::atom(selector_atom.to_string()),
                ", ",
                leaf::atom(kind.runtime_atom()),
                ")"
            ],
        }
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
    fn try_instantiation_intrinsic(
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
            "spawn" | "spawnWith:" | "spawnMonitored" | "spawnMonitoredWith:" => {
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
            "spawnAs:" => Ok(Some(self.generate_class_self_named_spawn(
                "class_self_spawn_as",
                "SpawnAsRes",
                "spawnAs:",
                arguments,
            )?)),
            "spawnWith:as:" => Ok(Some(self.generate_class_self_named_spawn(
                "class_self_spawn_with",
                "SpawnWithAsRes",
                "spawnWith:as:",
                arguments,
            )?)),
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
    fn class_reference_spawn_module_doc(
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
        helper: &'static str,
        result_prefix: &'static str,
        selector_atom: &'static str,
        arguments: ClassSendArgs<'_>,
    ) -> Result<Document<'static>> {
        let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;
        let call_doc = docvec![
            "call 'beamtalk_class_instantiation':'",
            Document::Str(helper),
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
        Ok(self.close_prelude(&args_preamble, call_doc, result_prefix))
    }

    /// Generates synchronous self-dispatch for actor self-sends.
    ///
    /// When an actor method sends a message to `self`, we bypass the async
    /// `gen_server:cast` path and call `safe_dispatch/3` directly. This ensures
    /// the result is a value (not a Future), enabling recursive algorithms like
    /// factorial and fibonacci to work correctly.
    ///
    /// # Sealed Class Optimization
    ///
    /// ADR 0118 §Decision 2: the state-effecting *producer* for
    /// an Actor self-send — the one place a dispatching self-send is
    /// compiled in a state-threading context. Returns a [`ThreadedValue`]
    /// whose prelude is
    ///
    /// ```text
    /// Statement(let _SDn = case call 'm':'safe_dispatch'('sel', [Args], StateK) of … end in)
    /// Bind(State{K+1} <- call 'erlang':'element'(2, _SDn))
    /// ```
    ///
    /// and whose value is `call 'erlang':'element'(1, _SDn)` — exactly the
    /// `Statement` + real `Bind` pair `dispatch_self_send_as_bind`
    /// (`control_flow/conditionals.rs`) built for the planner, now owned
    /// here; that function is a thin adapter over
    /// [`Self::generate_self_dispatch_parts`].
    ///
    /// `arguments` are compiled by
    /// [`Self::generate_self_dispatch_call_doc_for`] exactly as before; a
    /// caller that must sequence state-effecting *arguments* ahead of the
    /// dispatch (the ADR §Decision 3 rule) does so before calling this —
    /// see `threaded_expression` (`util.rs`).
    ///
    /// `frame` is the [`FrameId`] the `Bind` belongs to — `FrameId::ROOT`
    /// for the flat Actor method body, a branch arm's own frame otherwise;
    /// `span` is the source span both prelude nodes are attributed to.
    pub(super) fn generate_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
        frame: FrameId,
        span: Span,
    ) -> Result<ThreadedValue> {
        let (prelude, dispatch_var) =
            self.generate_self_dispatch_parts(selector, arguments, frame, span)?;
        Ok(ThreadedValue {
            prelude,
            value: Self::self_dispatch_result_value(&dispatch_var),
        })
    }

    /// The pure `element(1, _SDn)` reference to a self-dispatch's reply —
    /// the single spelling shared by [`Self::generate_self_dispatch`]'s
    /// value and [`Self::try_handle_self_dispatch`]'s planner substitution.
    pub(super) fn self_dispatch_result_value(dispatch_var: &str) -> ValueRef {
        ValueRef::Doc(docvec![
            "call 'erlang':'element'(1, ",
            leaf::var(dispatch_var.to_string()),
            ")",
        ])
    }

    /// The `Statement` + `Bind` pair behind [`Self::generate_self_dispatch`],
    /// plus the dispatch tuple's variable name for callers that register
    /// it (the planner's `hoisted_self_send_results`). Mint order is
    /// unchanged from `dispatch_self_send_as_bind`: the dispatch temps and
    /// the `next_state_var` bump all happen inside
    /// [`Self::generate_self_dispatch_call_doc_for`], and the `Bind`'s
    /// source/target versions are read off the live counter either side
    /// of it.
    pub(super) fn generate_self_dispatch_parts(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
        frame: FrameId,
        span: Span,
    ) -> Result<(Vec<ThreadedStmt>, String)> {
        let source_version = self.state_version();
        let (call_doc, dispatch_var) =
            self.generate_self_dispatch_call_doc_for(selector, arguments)?;
        let target_version = self.state_version();
        // ADR 0137: a self-send that may reach `dispatch/4`'s default arm keeps its
        // explicit `{reply, R, State}` threading and has the cell stored before it
        // runs; built here, the one place every self-dispatch prelude is made.
        let dispatch = self.self_dispatch_statement(
            selector,
            call_doc,
            VersionedVar::new(VersionPrefix::State, source_version, frame),
            span,
        );
        let prelude = vec![
            dispatch,
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'erlang':'element'(2, ",
                    leaf::var(dispatch_var.clone()),
                    ")",
                ])),
                span,
            },
        ];
        Ok((prelude, dispatch_var))
    }

    /// For sealed classes, we skip the `safe_dispatch/3` try/catch overhead and
    /// call `dispatch/4` directly. Since sealed classes have all methods known at
    /// compile time, the error isolation overhead is unnecessary.
    ///
    /// # Generated Code (normal)
    ///
    /// ```erlang
    /// case call 'module':'safe_dispatch'('selector', [Args], State) of
    ///   <{'reply', Result, _NewState}> when 'true' -> Result
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end
    /// ```
    ///
    /// # Generated Code (sealed class)
    ///
    /// ```erlang
    /// let Self = call 'beamtalk_actor':'make_self'(State) in
    /// case call 'module':'dispatch'('selector', [Args], Self, State) of
    ///   <{'reply', Result, _NewState}> when 'true' -> Result
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end
    /// ```
    ///
    /// ADR 0118: this is the *discarding* form — the `NewState`
    /// the dispatch returns is dropped. It is reached only from
    /// [`Self::try_handle_self_dispatch`]'s fallback, i.e. for a self-send
    /// in a position no consumer has yet migrated to
    /// [`Self::generate_self_dispatch`]'s prelude form (a cascade message,
    /// a `match:` scrutinee, a `sort:` comparator, an interpolation
    /// segment after an effectful one, …) and that no planner pre-hoisted.
    /// Every such position is a row of
    /// `stdlib/test/actor_self_send_position_matrix_test.bt` gated on the
    /// ADR 0118 phase that migrates it; once phase 2b removes the last
    /// `Document`-only consumer this fallback becomes
    /// `generate_self_dispatch(..).close(.., CloseContext::Opaque)` and the
    /// drop a [`super::threaded_ir::VerifyError::StateEffectEscapesExpression`]
    /// (§Decision 5). Byte-identical to the pre-ADR-0118 output by
    /// construction, so un-migrated positions are untouched.
    fn generate_discarding_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        self.note_self_dispatch(selector, arguments);
        // Sealed class optimization — skip safe_dispatch try/catch
        if self.is_class_sealed() {
            return self.generate_sealed_self_dispatch(selector, arguments);
        }

        let selector_atom = selector.name().to_string();
        let result_var = self.fresh_var("SelfResult");
        let state_var = self.fresh_var("SelfState");
        let current_state = self.current_state_var();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let error_clause = self.generate_self_dispatch_error_clause("SelfError", &selector_atom);

        let reply_body = leaf::var(result_var.clone());
        let doc = docvec![
            "case ",
            self.safe_dispatch_call_doc(selector_atom, args_doc, current_state),
            " of ",
            "<{'reply', ",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}> when 'true' -> ",
            reply_body,
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// ADR 0111 Addendum 5 (shape E2): the dispatch-call/
    /// case-clause portion of a self-dispatch, WITHOUT the trailing
    /// state-extraction `let` (the former `generate_self_dispatch_open` text;
    /// BT-3809 removed that wrapper once every caller took the `Bind` form) —
    /// factored out so
    /// `exception_handling.rs`'s per-arm `ThreadedIr` lowering can model the
    /// state-version bump as a real `Bind` (`BindOp::Direct`) instead of
    /// baking it into an opaque `Statement`'s text. Still mints the version
    /// bump itself (`next_state_var()`, right before
    /// `generate_self_dispatch_error_clause`, exactly where the un-split
    /// function always minted it) so every mint *after* this call keeps its
    /// original position — callers that need the bumped state's rendered
    /// name read it back via `current_state_var()` rather than consuming a
    /// return value, since a real `Bind`'s `render_bind` re-derives the same
    /// name from the version number, not from a string this function hands
    /// back.
    ///
    /// Takes the decomposed selector/arguments so callers that only hold those
    /// (not an owning `MessageSend` node) share one dispatch-call shape.
    #[allow(clippy::too_many_lines)] // Document-based sealed/normal dispatch branches
    pub(super) fn generate_self_dispatch_call_doc_for(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<(Document<'static>, String)> {
        {
            let selector_atom = selector.name().to_string();
            // `selector_atom` is moved into `call_doc` below (some
            // branches consume it via `leaf::atom`), so clone the value
            // needed for the error-clause breadcrumb before that happens.
            let selector_atom_for_error = selector_atom.clone();
            let dispatch_var = self.fresh_temp_var("SD");
            let result_var = self.fresh_var("SDResult");
            let state_var = self.fresh_var("SDState");
            let current_state = self.current_state_var();

            // Capture arguments via bridge (ADR 0018 Phase 0)
            let args_doc = self.capture_argument_list_doc(arguments)?;

            // Build the dispatch call (varies by sealed optimization level)
            let call_doc = if self.is_class_sealed() {
                let selector_name = selector.name().to_string();
                if self.sealed_method_selectors().contains(&selector_name) {
                    // Level 1: Direct __sealed_ call
                    let self_var = self.fresh_temp_var("SealedSelf");
                    let module = self.module_name.clone();
                    let comma = if arguments.is_empty() { "" } else { ", " };
                    docvec![
                        "let ",
                        leaf::var(self_var.clone()),
                        " = call 'beamtalk_actor':'make_self'(",
                        leaf::var(current_state.clone()),
                        ") in ",
                        "let ",
                        leaf::var(dispatch_var.clone()),
                        " = case call ",
                        leaf::atom(module),
                        ":",
                        leaf::atom(super::selector_mangler::sealed_fn_name(&selector_name)),
                        "(",
                        args_doc,
                        Document::Str(comma),
                        leaf::var(self_var),
                        ", ",
                        leaf::var(current_state),
                        ") of ",
                    ]
                } else {
                    // Level 2: Direct dispatch/4 call
                    let self_var = self.fresh_temp_var("SealedSelf");
                    let module = self.module_name.clone();
                    docvec![
                        "let ",
                        leaf::var(self_var.clone()),
                        " = call 'beamtalk_actor':'make_self'(",
                        leaf::var(current_state.clone()),
                        ") in ",
                        "let ",
                        leaf::var(dispatch_var.clone()),
                        " = case call ",
                        leaf::atom(module),
                        ":'dispatch'(",
                        leaf::atom(selector_atom),
                        ", [",
                        args_doc,
                        "], ",
                        leaf::var(self_var),
                        ", ",
                        leaf::var(current_state),
                        ") of ",
                    ]
                }
            } else {
                // Normal: safe_dispatch/3
                docvec![
                    "let ",
                    leaf::var(dispatch_var.clone()),
                    " = case ",
                    self.safe_dispatch_call_doc(selector_atom, args_doc, current_state),
                    " of ",
                ]
            };

            // Result/error clauses. The state-version bump
            // (`next_state_var()`) stays exactly here — mint-order fidelity
            // — but its returned name is no longer consumed for rendering;
            // the `Bind`-based callers re-derive it from the version number
            // via `render_bind`.
            let _ = self.next_state_var();
            let ok_doc = docvec![
                "{",
                leaf::var(result_var.clone()),
                ", ",
                leaf::var(state_var.clone()),
                "}"
            ];
            let reply_body = ok_doc;
            let error_clause =
                self.generate_self_dispatch_error_clause("SDError", &selector_atom_for_error);
            let doc = docvec![
                call_doc,
                "<{'reply', ",
                leaf::var(result_var.clone()),
                ", ",
                leaf::var(state_var.clone()),
                "}> when 'true' -> ",
                reply_body,
                " ",
                error_clause,
                "end in ",
            ];

            Ok((doc, dispatch_var))
        }
    }

    /// Sealed-class self-dispatch (value-discarding — see
    /// `generate_self_dispatch`'s call site).
    ///
    /// Two levels of optimization:
    /// 1. **Known sealed method**: Direct function call to `__sealed_{selector}`,
    ///    bypassing both `safe_dispatch/3` and `dispatch/4` case matching.
    /// 2. **Unknown method** (inherited): Direct `dispatch/4` call, skipping
    ///    only the `safe_dispatch/3` try/catch overhead.
    fn generate_sealed_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_name = selector.name().to_string();

        // Level 1: Direct call to standalone sealed method function
        if self.sealed_method_selectors().contains(&selector_name) {
            let selector_atom = selector.name().to_string();
            return self.generate_direct_sealed_call(&selector_name, &selector_atom, arguments);
        }

        // Level 2: Direct dispatch/4 call (skip safe_dispatch try/catch)
        let selector_atom = selector.name().to_string();
        let result_var = self.fresh_var("SealedResult");
        let self_var = self.fresh_temp_var("SealedSelf");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let error_clause = self.generate_self_dispatch_error_clause("SealedError", &selector_atom);
        let state_var = self.fresh_var("SealedState");
        let reply_body = leaf::var(result_var.clone());

        let doc = docvec![
            "let ",
            leaf::var(self_var.clone()),
            " = call 'beamtalk_actor':'make_self'(",
            leaf::var(current_state.clone()),
            ") in ",
            "case call ",
            leaf::atom(module),
            ":'dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            leaf::var(self_var),
            ", ",
            leaf::var(current_state),
            ") of ",
            "<{'reply', ",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}> when 'true' -> ",
            reply_body,
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// Generates a direct call to a sealed method's standalone function.
    ///
    /// This is the most optimized self-dispatch path: calls `__sealed_{selector}`
    /// directly, bypassing `safe_dispatch`, dispatch, and case selector matching.
    fn generate_direct_sealed_call(
        &mut self,
        selector_name: &str,
        selector_atom: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let result_var = self.fresh_var("SealedResult");
        let self_var = self.fresh_temp_var("SealedSelf");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let comma = if arguments.is_empty() { "" } else { ", " };
        // `selector_atom` (from `MessageSelector::name`) is
        // the breadcrumb value — kept independent of `selector_name` (used
        // below for `sealed_fn_name` mangling) so a future change to either
        // mangling scheme can't silently desync the breadcrumb from the
        // dispatch atom used at the other call sites.
        let error_clause =
            self.generate_self_dispatch_error_clause("SealedDirectError", selector_atom);
        let state_var = self.fresh_var("SealedState");
        let reply_body = leaf::var(result_var.clone());

        let doc = docvec![
            "let ",
            leaf::var(self_var.clone()),
            " = call 'beamtalk_actor':'make_self'(",
            leaf::var(current_state.clone()),
            ") in ",
            "case call ",
            leaf::atom(module),
            ":",
            leaf::atom(super::selector_mangler::sealed_fn_name(selector_name)),
            "(",
            args_doc,
            Document::Str(comma),
            leaf::var(self_var),
            ", ",
            leaf::var(current_state),
            ") of ",
            "<{'reply', ",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}> when 'true' -> ",
            reply_body,
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// Generates the opening part of a field assignment with state threading.
    ///
    /// For `self.field := value`, generates:
    /// ```erlang
    /// let _Val = <value> in
    /// let StateN = call 'maps':'put'('field', _Val, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression (generating the body
    /// that uses the new state).
    ///
    /// the plain `Actor`/`ValueType` tail (the `else` fallthrough
    /// below) is now a thin `Closure::Open` call into
    /// [`Self::lower_field_write`] — the single lowering core this,
    /// `expressions.rs`'s `generate_field_assignment` (`Closure::Closed`),
    /// and `control_flow::conditionals`'s `lower_field_assignment_bind` (the
    /// un-rendered-`Bind`-push consumption style) all now share. Before this
    /// issue this branch was hand-built and Actor-only — a `ValueType` field
    /// write reaching it (from inside a loop/conditional/block body) would
    /// have silently threaded through the actor `State`/`StateAcc` map, a
    /// variable that does not exist in a value-type method, instead of
    /// `Self` — the missing `ValueType` arm `lower_field_write`'s
    /// [`FieldWriteSite`] dispatch now supplies.
    pub(super) fn generate_field_assignment_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                // ADR 0130 §2: a class-variable write is an in-place `put`,
                // an expression like any other: bind its value, no rebinding.
                if self.in_class_method() {
                    let val_var = self.fresh_temp_var("Val");
                    let write_doc = self.generate_field_assignment(&field.name, value)?;
                    return Ok((
                        docvec!["let ", leaf::var(val_var.clone()), " = ", write_doc, " in "],
                        val_var,
                    ));
                }
                // Full-extract mode — rebind field param instead of maps:put.
                // When the field is in hybrid_mutated_fields, the field has been extracted
                // to a direct fun parameter. We rebind it to a fresh variable and update
                // the readonly params map so subsequent reads use the new variable.
                if self.loop_mode.in_hybrid_loop
                    && self
                        .loop_mode
                        .hybrid_mutated_fields
                        .contains(field.name.as_str())
                {
                    // BT-3811: the one hybrid-rebind lowering
                    // (`lower_hybrid_field_rebind`), spliced as real
                    // `ThreadedStmt`s in the caller's frame and rendered
                    // here for this `Document`-returning API.
                    let frame = self.current_frame();
                    let span = expr.span();
                    let mut stmts = Vec::new();
                    let val_var = self.lower_hybrid_field_rebind(
                        &field.name,
                        value,
                        frame,
                        span,
                        &mut stmts,
                    )?;
                    let doc = self.rendered_field_rebind_doc(&stmts, frame, span);
                    return Ok((doc, val_var));
                }

                // `ValueType` gains the `Self`-threading arm it
                // lacked before this issue (see this function's own doc
                // comment) — `FieldWriteSite::for_context` is the same
                // dispatch `generate_field_assignment`'s (the `Closed`
                // sibling's) plain-write default uses.
                let site = FieldWriteSite::for_context(self.context);
                // `lower_field_write` returns the val var so callers
                // (e.g. cascade codegen) can reference the assigned value
                // after hoisting the binding.
                return self.lower_field_write(
                    site,
                    Closure::Open,
                    &field.name,
                    value,
                    FrameId::ROOT,
                );
            }
        }
        Err(CodeGenError::Internal(
            "generate_field_assignment_open called on non-field-assignment expression".to_string(),
        ))
    }

    /// Generates the opening part of a `self fieldAt: name put: value` with state threading.
    ///
    /// Similar to `generate_field_assignment_open` but with a dynamic field name.
    /// Generates:
    /// ```erlang
    /// let _Name = <name> in
    /// let _Val = <value> in
    /// let StateN = call 'maps':'put'(_Name, _Val, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression.
    pub(super) fn generate_self_field_at_put_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend { arguments, .. } = expr {
            // BT-4369: a dynamic-key field write records itself for the loop checkout.
            self.note_field_write("<dynamic>", expr.span());
            let name_var = self.fresh_var("Name");
            let val_var = self.fresh_temp_var("Val");
            let name_code = self.expression_doc(&arguments[0])?;
            // The state the put builds on. Every caller has sequenced the name
            // and value first (BT-4310, `thread_send_arguments`), so a value
            // that changes the state (a self-send, a nested field write, a
            // State-only fold) has already stepped it, and this is the state
            // after the value: its change survives the put. (Before BT-4310
            // this read the state from before the value and dropped it.)
            let current_state = self.current_state_var();
            let val_code = self.expression_doc(&arguments[1])?;
            let new_state = self.next_state_var();

            let doc = docvec![
                "let ",
                leaf::var(name_var.clone()),
                " = ",
                name_code,
                " in let ",
                leaf::var(val_var.clone()),
                " = ",
                val_code,
                " in let ",
                leaf::var(new_state),
                " = call 'maps':'put'(",
                leaf::var(name_var),
                ", ",
                leaf::var(val_var.clone()),
                ", ",
                leaf::var(current_state),
                ") in ",
            ];

            return Ok((doc, val_var));
        }
        Err(CodeGenError::Internal(
            "generate_self_field_at_put_open called on non-fieldAt:put: expression".to_string(),
        ))
    }

    /// Generates the opening part of `self clearField: name` with state
    /// threading (ADR 0124 §1/§4f, B4) — the instance-`State` counterpart to
    /// [`Self::generate_self_field_at_put_open`], which this mirrors exactly
    /// except for the map op (`maps:remove` in place of `maps:put`) and arity
    /// (one argument, no value to thread). `clearField:` returns `Self`
    /// (`Object>>clearField: -> Self`, matching `fieldAt:put:`'s convention)
    /// rather than a written value, since a clear has none.
    ///
    /// Generates:
    /// ```erlang
    /// let _Name = <name> in
    /// let StateN = call 'maps':'remove'(_Name, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression.
    pub(super) fn generate_self_clear_field_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend { arguments, .. } = expr {
            // BT-4369: a dynamic-key field write records itself for the loop checkout.
            self.note_field_write("<dynamic>", expr.span());
            let name_var = self.fresh_var("Name");
            let name_code = self.expression_doc(&arguments[0])?;
            let current_state = self.current_state_var();
            let new_state = self.next_state_var();

            let doc = docvec![
                "let ",
                leaf::var(name_var.clone()),
                " = ",
                name_code,
                " in let ",
                leaf::var(new_state),
                " = call 'maps':'remove'(",
                leaf::var(name_var),
                ", ",
                leaf::var(current_state),
                ") in ",
            ];

            return Ok((doc, "Self".to_string()));
        }
        Err(CodeGenError::Internal(
            "generate_self_clear_field_open called on non-clearField: expression".to_string(),
        ))
    }

    /// Generates code for a super message send.
    ///
    /// Super calls use `beamtalk_dispatch:super/5` to invoke the superclass
    /// implementation via hierarchy walking (ADR 0006).
    ///
    /// # Example
    ///
    /// ```beamtalk
    /// super increment
    /// super getValue
    /// super at: 1 put: value
    /// ```
    ///
    /// Generates:
    ///
    /// ```erlang
    /// call 'beamtalk_dispatch':'super'('increment', [], Self, State, 'Counter')
    /// call 'beamtalk_dispatch':'super'('getValue', [], Self, State, 'Counter')
    /// call 'beamtalk_dispatch':'super'('at:put:', [1, Value], Self, State, 'Counter')
    /// ```
    pub(super) fn generate_super_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Taken before any argument compiles, so a nested `super` send is
        // judged on its own.
        let from_producer = std::mem::take(&mut self.in_super_send_producer);
        let selector_atom = selector.name().to_string();

        // A class-side `super` resolves up the metaclass chain via the runtime
        // helper, keyed on the class the method belongs to (`class_var_home`):
        // the builder class inside a programmatic ClassBuilder class-method
        // fun (ADR 0084), else a class-side extension's target, else the
        // class being compiled (BT-4001). `class_super_dispatch/4` begins the
        // walk at that class's superclass and never consults the class itself
        // (not even its own class-side extensions, so an extension override
        // that sends `super` cannot recurse into itself); at each ancestor it
        // checks that ancestor's class-side extensions, then its class methods.
        // The live receiver (`ClassSelf`) is passed as is (BT-4301), so a
        // subclass receiver runs the superclass method under its own
        // class-variable key with its real reference. A class method binds no instance `Self`/`State`,
        // so it must never reach the `beamtalk_dispatch:super/5` instance path
        // below.
        let class_side_super_class = (self.builder_class_method_class().is_some()
            || self.in_class_method())
        .then(|| self.class_var_home());
        if let Some(defining_class) = class_side_super_class {
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let call_doc = docvec![
                "call 'beamtalk_class_dispatch':'class_super_dispatch'(",
                leaf::atom(defining_class),
                ", ",
                leaf::atom(selector_atom),
                ", [",
                args_doc,
                "], ",
                leaf::var(CLASS_SELF_VAR),
                ")"
            ];
            // ADR 0130 §3: `super` passes nothing and rebinds nothing.
            let tv = self.class_self_send_value(&args_preamble, call_doc);
            return Ok(self.close_threaded_value_doc(tv));
        }

        let class_name = self.class_name();

        // value/primitive-context funs (`fun(Args, Self) -> Result`)
        // have no `State` binding, so `super` must not reference one. Route to
        // `super_value/4`, which walks the same chain and returns a plain value.
        // BT-4296: its arguments are sequenced (`thread_args`) so the argument
        // list evaluates them in source order.
        if self.context == CodeGenContext::ValueType {
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let call_doc = docvec![
                "call 'beamtalk_dispatch':'super_value'(",
                leaf::atom(selector_atom),
                ", [",
                args_doc,
                "], Self, ",
                leaf::atom(class_name),
                ")",
            ];
            return Ok(self.close_prelude(&args_preamble, call_doc, "SuperRes"));
        }

        // ADR 0137 §3: inside an actor closure a `super` send runs the
        // superclass method against the cell (the actor's current state at
        // home), stores its answer back, and answers its value. A block or loop
        // body threads no `State` for a `super` send either (BT-3995): it runs
        // against the cell, which the enclosing call brackets
        // (`classify_super_send`), and answers its value. Sends there are not
        // sync-bracketed, so the arguments are sequenced (`thread_args`) to
        // evaluate in source order (BT-4296).
        if !self.super_send_threads_state() {
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            // The prelude closes around this call, so a threaded-state rebind in
            // it would be scoped away from later statements (BT-3809): report
            // it, as `capture_argument_list_doc` does for its arguments.
            let escaped = super::threaded_ir::opaque_escapes(&args_preamble, false);
            let at = arguments
                .first()
                .map_or_else(Span::default, Expression::span);
            self.report_threaded_ir_verify_errors(
                &escaped,
                "super-send argument state rebind trapped in a closed argument list",
                at,
            );
            let call_doc = self.cell_super_send_doc(&selector_atom, &class_name, args_doc);
            return Ok(self.close_prelude(&args_preamble, call_doc, "SuperRes"));
        }

        // BT-4318: this form answers `{reply, Result, NewState}`; only
        // `super_send_producer` unpacks it into a `State` `Bind`. Anywhere
        // else the tuple leaks as the value and the state change is lost.
        if !from_producer {
            let at = self.current_send_span.unwrap_or_default();
            self.report_threaded_ir_verify_errors(
                &[VerifyError::StateEffectEscapesExpression {
                    prefix: VersionPrefix::State,
                    at,
                }],
                "state-threading super send compiled outside its producer",
                at,
            );
        }

        // A method-body `super` send's arguments are sequenced ahead by
        // `super_send_producer` (`sequence_children`) and read back here.
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let current_state = self.current_state_var();
        let doc = docvec![
            "call 'beamtalk_dispatch':'super'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], Self, ",
            leaf::var(current_state),
            ", ",
            leaf::atom(class_name),
            ")",
        ];
        // ADR 0137: a `super` send that may reach the default arm is bracketed.
        // A `super` send in a closure runs against the cell (the early returns
        // above), so its change is kept and no guard is needed (BT-3985 retired).
        Ok(self.bracket_super_send(selector, doc, Span::default()))
    }

    /// Whether a `super` send compiled now threads the method's own `State`:
    /// `generate_super_send` answers the raw `{reply, Result, NewState}`
    /// tuple of `beamtalk_dispatch:super/5`, which the caller must unpack
    /// (BT-4318) — through [`Self::threaded_expression`]'s `super` producer
    /// in any value position. False on the class side, in a value type, and
    /// wherever the send runs against the actor-state cell (a closure, a
    /// block or loop body), all of which answer a plain value.
    pub(super) fn super_send_threads_state(&self) -> bool {
        self.builder_class_method_class().is_none()
            && !self.in_class_method()
            && self.context != CodeGenContext::ValueType
            && !self.in_cell_closure()
            && !self.super_send_uses_cell()
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
    pub(super) fn generate_actor_spawn_qualified(
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
                SpawnVariant::Monitored => Document::Str("call 'erlang':'element'(1, SpawnResult)"),
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

    /// Generates a method lookup via `>>` operator.
    ///
    /// `Counter >> #increment` compiles to:
    /// ```erlang
    /// call 'beamtalk_method_resolver':'resolve'('Counter', 'increment')
    /// ```
    ///
    /// Returns a `CompiledMethod` map with selector, source, and arity metadata.
    fn generate_method_lookup(
        &mut self,
        class_name: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        let arg_doc = self.expression_doc(&arguments[0])?;
        let doc = docvec![
            "call 'beamtalk_method_resolver':'resolve'(",
            leaf::atom(class_name.to_string()),
            ", ",
            arg_doc,
            ")"
        ];
        Ok(doc)
    }

    /// Generates a runtime method resolution via `>>` for non-class-literal receivers.
    ///
    /// `cls >> #increment` (where cls holds a class object) compiles to:
    /// ```erlang
    /// call 'beamtalk_method_resolver':'resolve'(cls, 'increment')
    /// ```
    ///
    /// The `MethodResolver` domain service accepts pids, atoms, and class object tuples.
    fn generate_runtime_method_lookup(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        // Capture receiver + arg as one ordered sequence so
        // left-to-right evaluation order is preserved.
        let exprs: [&Expression; 2] = [receiver, &arguments[0]];
        let (preamble, mut docs) = self.thread_subexprs(&exprs, "Lookup")?;
        let arg_doc = docs.pop().expect("arg");
        let actual_receiver = docs.pop().expect("receiver");

        let call_doc = docvec![
            "call 'beamtalk_method_resolver':'resolve'(",
            actual_receiver,
            ", ",
            arg_doc,
            ")"
        ];
        Ok(self.close_prelude(&preamble, call_doc, "MethodLookup"))
    }

    /// Generates a class-side send at REPL top level, where the receiver name may
    /// be a session binding (`Workspace bind:as:`) rather than a class
    /// (ADR 0129 §7: session locals, then `bind:as:` entries, then the class
    /// registry).
    ///
    /// Only reached for [`CodeGenContext::Repl`]; methods and batch-compiled
    /// code never consult the session map and go straight through
    /// [`Self::generate_class_method_call`]. There is no singleton step: the
    /// system facades are ordinary classes (ADR 0129).
    ///
    /// ```erlang
    /// let Lookup = call 'maps':'find'('Name', State) in
    /// case Lookup of
    ///   <{'ok', Bound}> -> call 'beamtalk_message_dispatch':'send'(Bound, Sel, Args)
    ///   <'error'>       -> %% direct call or class_send, as generate_class_method_call
    /// end
    /// ```
    fn generate_repl_bound_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // The bound-value branch dispatches to an instance, so it uses the raw
        // selector; the class fallback uses the class-method mangled selector.
        let raw = selector.name().to_string();
        let instance_selector = super::selector_mangler::safe_atom_name(&raw);
        let bound_var = self.fresh_var("BindingVal");
        let state_var = self.current_state_var();
        let lookup_var = self.fresh_temp_var("Lookup");

        // Receiver first, then args: bind the session lookup before the arg
        // preamble, and bind args to temps so each is evaluated exactly once.
        let (arg_prelude, arg_refs) = self.thread_args_bound(arguments, "BindArg")?;
        let args_doc = Self::join_docs_with_commas(arg_refs);

        let class_fallback: Document<'static> =
            if let Some(module_name) = self.direct_call_eligible_module(class_name, &raw) {
                self.guarded_class_direct_call_doc(
                    class_name,
                    module_name,
                    &raw,
                    args_doc.clone(),
                    arguments.len(),
                )
            } else {
                self.generate_class_send_fallback(class_name, &raw, args_doc.clone())
            };

        let lookup_binding = docvec![
            "let ",
            leaf::var(lookup_var.clone()),
            " = call 'maps':'find'(",
            leaf::atom(class_name.to_string()),
            ", ",
            leaf::var(state_var),
            ") in ",
        ];
        let case_doc = docvec![
            "case ",
            leaf::var(lookup_var),
            " of ",
            "<{'ok', ",
            leaf::var(bound_var.clone()),
            "}> when 'true' -> ",
            "call 'beamtalk_message_dispatch':'send'(",
            leaf::var(bound_var),
            ", ",
            leaf::atom(instance_selector),
            ", [",
            args_doc,
            "]) ",
            "<'error'> when 'true' -> ",
            class_fallback,
            " end"
        ];

        // ADR 0118 phase 5b: thread the lookup ahead of the arg prelude, then
        // close (this returns a bare `Document`).
        let mut prelude = vec![ThreadedStmt::Statement(
            lookup_binding,
            beamtalk_core::source_analysis::Span::default(),
        )];
        prelude.extend(arg_prelude);
        Ok(self.close_prelude(&prelude, case_doc, "BindClassRes"))
    }

    /// Generates a class-level method call to a literal class receiver.
    ///
    /// ADR 0142 §2 (BT-4301): when the class itself defines the selector and
    /// may run it in the caller ([`Self::direct_call_eligible_module`], the
    /// one `ClassHierarchy::is_direct_call_eligible` rule), the send compiles
    /// to the guarded direct call of [`Self::guarded_class_direct_call_doc`].
    /// Everything else, an inherited class method included, takes the dynamic
    /// path (registered-name `whereis` + `class_send`), which itself runs a stateless
    /// class method in the caller (BT-4300). A send that resolves to one of
    /// `Exception`'s caller-safe signal primitives, own or inherited
    /// (`Error signal: m`), raises in the caller behind its own key instead
    /// ([`Self::guarded_caller_safe_raise_doc`], BT-4115, BT-4270).
    ///
    /// # Generated Code (dynamic path)
    ///
    /// ```erlang
    /// let ClassPid = call 'erlang':'whereis'('beamtalk_class_Tracing') in
    /// call 'beamtalk_object_class':'class_send'(ClassPid, 'setContext:', [Ctx])
    /// ```
    ///
    /// `arguments` may be already bound ([`ClassSendArgs::Bound`]): the
    /// explicit own-class send of [`Self::generate_own_class_literal_send`]
    /// splices this lowering into one arm of its receiver-tag `case`.
    fn generate_class_method_call(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: ClassSendArgs<'_>,
    ) -> Result<Document<'static>> {
        let raw_selector = selector.name().to_string();

        // nullary `new` on an eligible Value builds via a direct
        // `Module:'new'()` call, behind the same static call key as every
        // other guarded class send (BT-4409). (`new:` stays on `class_send`:
        // its chain verdict is decided at run time, on the dynamic path.)
        if raw_selector == "new" && arguments.is_empty() {
            if let Some(module_name) = self.direct_new0_module(class_name) {
                return Ok(self.guarded_direct_new_doc(class_name, module_name));
            }
        }

        if let Some((module_name, primitive)) =
            self.caller_safe_signal_target(class_name, &raw_selector)
        {
            let (args_preamble, arg_refs) = self.thread_class_send_args_bound(arguments)?;
            let call_doc = self.guarded_caller_safe_raise_doc(
                class_name,
                module_name,
                &raw_selector,
                &primitive,
                arg_refs,
            )?;
            return Ok(self.close_prelude(&args_preamble, call_doc, "SignalCall"));
        }

        if let Some(module_name) = self.direct_call_eligible_module(class_name, &raw_selector) {
            // Bind every argument once: the doc is spliced into both arms of
            // the guard (an inline block argument holding further guarded
            // sends would otherwise double per nesting level).
            let (args_preamble, arg_refs) = self.thread_class_send_args_bound(arguments)?;
            let args_doc = Self::join_docs_with_commas(arg_refs);
            let call_doc = self.guarded_class_direct_call_doc(
                class_name,
                module_name,
                &raw_selector,
                args_doc,
                arguments.len(),
            );
            return Ok(self.close_prelude(&args_preamble, call_doc, "DirectCall"));
        }

        // Fallback: gen_server dispatch via class_send
        // Hash long selector atoms (e.g. keyword constructors with many
        // fields) to stay within Erlang's 255-char atom limit.
        // Hoist preambles from sub-expression class var mutations.
        let selector_atom = super::selector_mangler::safe_class_method_selector(&raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;

        let call_doc = docvec![
            "let ",
            leaf::var(class_pid_var.clone()),
            " = ",
            Self::registered_class_pid_doc(class_name),
            " in ",
            self.class_send_call_doc(&class_pid_var, selector_atom, args_doc),
        ];

        Ok(self.close_prelude(&args_preamble, call_doc, "ClassCall"))
    }

    /// ADR 0142 §2 (BT-4301, BT-4316): the guarded direct call of a static
    /// send of `raw_selector`, which `class_name` itself defines, to the
    /// literal class `class_name` compiled to `module_name`. `args_doc` must
    /// be variables or literals: it is spliced into both arms.
    ///
    /// ```erlang
    /// let StaticSelf = case call 'persistent_term':'get'(
    ///         'Point class static calls', ~{}~) of
    ///     <~{{'bt@point', 'class_origin', 'Point class'} := StaticRef}~> when 'true' ->
    ///       StaticRef
    ///     <_> when 'true' ->
    ///       call 'beamtalk_class_shadow_flags':'static_call_target'(
    ///           {'bt@point', 'class_origin', 'Point class'}, 'Point', 'origin', 1)
    ///   end
    /// in case StaticSelf of
    ///   <'false'> when 'true' ->
    ///     let ClassPid = call 'erlang':'whereis'('beamtalk_class_Point') in
    ///     call 'beamtalk_object_class':'class_send'(ClassPid, 'origin', [])
    ///   <_> when 'true' -> call 'bt@point':'class_origin'(StaticSelf)
    /// end
    /// ```
    ///
    /// The guard is the contract (ADR 0025 amendment). The hit path is one
    /// `persistent_term` read of the class's static call map (BT-4442: one
    /// key per class, `'<Name> class static calls'`, so a metadata write
    /// erases one key per class, not one per warm target) and a map match of
    /// the call target's key (BT-4316), whose value is the class reference
    /// the callee gets as `ClassSelf`. The runtime installs the key
    /// (`beamtalk_class_shadow_flags:static_call_target/4`) only while
    /// `direct_call_ok/2` holds (nothing shadows the compiled method: no
    /// class-side extension, no ADR 0084 runtime fun; neither the class nor
    /// an ancestor declares class variables), the dynamic path would run
    /// exactly this function in the caller, the function is exported and the
    /// class process is alive, and erases it on every class-metadata write
    /// that could change one of those answers. Otherwise the fallback is the
    /// dynamic `class_send`, byte for byte, so a removed method answers what a
    /// dynamic send answers instead of `undef`. The direct call stays in tail
    /// position: tail recursion through a literal class reference runs in
    /// constant stack.
    ///
    /// The target's map key is the call-site literal `{Module, FunName, Tag}`,
    /// passed whole to the runtime, which never spells it. The metaclass tag keeps a call
    /// site naming one class from hitting a key another class installed for
    /// the same module (after `renameTo:`, or a removed class's module name
    /// reused): such a site misses, and its own miss path answers for the
    /// class it names (`class_not_found` once that class is gone). The callee gets the real class
    /// reference (its tag, its own module and its registered pid, the value a
    /// class literal evaluates to), never `nil` (BT-4037); the pid is never
    /// baked: the key is erased when the class process goes down, so a
    /// restarted class process is found.
    pub(super) fn guarded_class_direct_call_doc(
        &mut self,
        class_name: &str,
        module_name: String,
        raw_selector: &str,
        args_doc: Document<'static>,
        arg_count: usize,
    ) -> Document<'static> {
        if let Some(value) = self.constant_class_method_value(class_name, raw_selector) {
            return self.guarded_constant_doc(
                class_name,
                module_name,
                raw_selector,
                &value,
                args_doc,
                arg_count,
            );
        }
        let safe_fn = super::selector_mangler::safe_class_method_fn_name(raw_selector);
        let class_self_var = self.fresh_temp_var("StaticSelf");
        let comma = if arg_count == 0 { "" } else { ", " };
        let key_doc = docvec![
            "{",
            leaf::atom(module_name.clone()),
            ", ",
            leaf::atom(safe_fn.clone()),
            ", ",
            leaf::atom(super::util::metaclass_tag(class_name)),
            "}"
        ];
        // BT-4366: the direct call sits inside the guard `case`, so it carries
        // the send's line itself.
        let direct_call = self.annotate_current_send_call(docvec![
            "call ",
            leaf::atom(module_name),
            ":",
            leaf::atom(safe_fn),
            "(",
            leaf::var(class_self_var.clone()),
            comma,
            args_doc.clone(),
            ")"
        ]);
        self.keyed_static_send_doc(
            class_name,
            raw_selector,
            &key_doc,
            &class_self_var,
            args_doc,
            arg_count + 1,
            direct_call,
        )
    }

    /// The literal that `class_name`'s own class-side method `raw_selector`
    /// is, when its body is exactly that scalar literal
    /// (`MethodInfo::constant_value`, BT-4363; every kind of
    /// [`ConstantValue`](beamtalk_core::ast::ConstantValue) since BT-4504).
    /// Only a compile-time candidate: the runtime key of
    /// [`Self::guarded_constant_doc`] decides each send.
    fn constant_class_method_value(
        &self,
        class_name: &str,
        raw_selector: &str,
    ) -> Option<beamtalk_core::ast::ConstantValue> {
        self.class_hierarchy
            .as_ref()?
            .get_class(class_name)?
            .class_methods
            .iter()
            .find(|m| m.selector == raw_selector)?
            .constant_value
            .clone()
    }

    /// BT-4363: a static send of `raw_selector` to the literal class
    /// `class_name`, whose own class-side method compiled to `module_name`
    /// has the scalar literal `value` as its whole body
    /// ([`Self::constant_class_method_value`]), answers `value` in place,
    /// behind the keyed shape of [`Self::guarded_class_direct_call_doc`]:
    ///
    /// ```erlang
    /// let StaticSelf = case call 'persistent_term':'get'('Errs class static calls', ~{}~) of
    ///     <~{{'constant_value', 1, 'bt@errs', 'class_protocolError', 'Errs class'}
    ///         := StaticRef}~> when 'true' -> StaticRef
    ///     <_> when 'true' ->
    ///       call 'beamtalk_class_shadow_flags':'static_call_target'(
    ///           <same key>, 'Errs', 'protocolError', 1)
    ///   end
    /// in case StaticSelf of
    ///   <'false'> when 'true' -> <the dynamic class_send to 'Errs'>
    ///   <_> when 'true' -> 1
    /// end
    /// ```
    ///
    /// The guard is the contract (ADR 0025 amendment): the runtime installs
    /// the key only while every condition of a direct-call key holds (the
    /// dynamic path would run exactly `module_name`'s compiled function in
    /// the caller) and the class's published metadata row maps the selector
    /// to this same value (`constant_class_methods`, published from the
    /// module's `__beamtalk_meta/0` on registration and reload;
    /// `beamtalk_class_shadow_flags`, `key_body_holds/4`). A class-side
    /// extension, a runtime fun (`put_class_method`), `removeSelector:` or a
    /// reload that changes the body erases or declines the key, and the send
    /// then answers what the dynamic send answers. Sealing plays no part: the
    /// guard already proves the target (ADR 0025 amendment §3).
    ///
    /// The key and the hit arm carry `value` as the term the method body
    /// compiles to ([`Self::constant_value_doc`], BT-4504): a string is the
    /// tagged `{'beamtalk_string', #{…}#}`, a symbol, `nil`, `true` or
    /// `false` an atom, a character its code point. Map-key matching and the
    /// runtime certification are both exact (`=:=`), so `1` never answers
    /// for `1.0`.
    ///
    /// `args_doc` must be variables or literals (bound by the caller, so a
    /// side-effecting argument still runs once); the hit arm ignores them.
    fn guarded_constant_doc(
        &mut self,
        class_name: &str,
        module_name: String,
        raw_selector: &str,
        value: &beamtalk_core::ast::ConstantValue,
        args_doc: Document<'static>,
        arg_count: usize,
    ) -> Document<'static> {
        let value_doc = Self::constant_value_doc(value);
        let key_doc = docvec![
            "{'constant_value', ",
            value_doc.clone(),
            ", ",
            leaf::atom(module_name),
            ", ",
            leaf::atom(super::selector_mangler::safe_class_method_fn_name(
                raw_selector
            )),
            ", ",
            leaf::atom(super::util::metaclass_tag(class_name)),
            "}"
        ];
        let class_self_var = self.fresh_temp_var("StaticSelf");
        self.keyed_static_send_doc(
            class_name,
            raw_selector,
            &key_doc,
            &class_self_var,
            args_doc,
            arg_count + 1,
            value_doc,
        )
    }

    /// BT-4409 (ADR 0142 rule 3): a static nullary `new` sent to the literal
    /// class `class_name`, whose synthesized `new/0` in `module_name` may
    /// build in the caller ([`Self::direct_new0_module`]), compiles to the
    /// keyed shape of [`Self::guarded_class_direct_call_doc`]:
    ///
    /// ```erlang
    /// let StaticSelf = case call 'persistent_term':'get'(
    ///         'Point class static calls', ~{}~) of
    ///     <~{{'bt@point', 'new', 'Point class'} := StaticRef}~> when 'true' -> StaticRef
    ///     <_> when 'true' ->
    ///       call 'beamtalk_class_shadow_flags':'static_call_target'(
    ///           {'bt@point', 'new', 'Point class'}, 'Point', 'new', 0)
    ///   end
    /// in case StaticSelf of
    ///   <'false'> when 'true' ->
    ///     let ClassPid = call 'erlang':'whereis'('beamtalk_class_Point') in
    ///     call 'beamtalk_object_class':'class_send'(ClassPid, 'new', [])
    ///   <_> when 'true' -> call 'bt@point':'new'()
    /// end
    /// ```
    ///
    /// `new/0` takes no `ClassSelf`, so the key's class reference is only the
    /// "installed" witness. The runtime installs the key only while the
    /// dynamic path would build in the caller too
    /// (`beamtalk_class_dispatch:lookup_direct_constructor/3`, which needs
    /// `direct_call_ok/2`: no class-side extension, no ADR 0084 runtime fun,
    /// no class variables in the chain), so a later extension or
    /// `put_class_method` of `new` is honoured through the fallback.
    fn guarded_direct_new_doc(
        &mut self,
        class_name: &str,
        module_name: String,
    ) -> Document<'static> {
        let class_self_var = self.fresh_temp_var("StaticSelf");
        let key_doc = docvec![
            "{",
            leaf::atom(module_name.clone()),
            ", ",
            leaf::atom("new".to_string()),
            ", ",
            leaf::atom(super::util::metaclass_tag(class_name)),
            "}"
        ];
        let direct_call = self.annotate_current_send_call(docvec![
            "call ",
            leaf::atom(module_name),
            ":",
            leaf::atom("new".to_string()),
            "()"
        ]);
        self.keyed_static_send_doc(
            class_name,
            "new",
            &key_doc,
            &class_self_var,
            Document::Nil,
            0,
            direct_call,
        )
    }

    /// BT-4270: a class-literal send that resolves to one of `Exception`'s
    /// caller-safe signal primitives ([`Self::caller_safe_signal_target`])
    /// raises in the caller, as BT-4115 made it, but only behind a static
    /// call key, so a later `class signal:` override, class-side extension,
    /// runtime fun or reload is honoured:
    ///
    /// ```erlang
    /// let StaticSelf = case call 'persistent_term':'get'(
    ///         'Error class static calls', ~{}~) of
    ///     <~{{'caller_safe_raise', 'classSignal:', 'bt@stdlib@exception', 'class_signal:',
    ///         'Error class'} := StaticRef}~> when 'true' -> StaticRef
    ///     <_> when 'true' ->
    ///       call 'beamtalk_class_shadow_flags':'static_call_target'(<same key>, 'Error', 'signal:', 2)
    ///   end
    /// in case StaticSelf of
    ///   <'false'> when 'true' -> <the dynamic class_send to 'Error'>
    ///   <_> when 'true' ->
    ///     call 'beamtalk_exception_handler':'class_signal_message'(Arg1, 'Error')
    /// end
    /// ```
    ///
    /// The key names the defining class's compiled method and the receiving
    /// class's tag; the runtime installs it only while every condition of a
    /// direct-call key holds for that method (the dynamic path would run it in
    /// the caller for this receiver) and the defining module's metadata maps
    /// the selector to the key's primitive, i.e. its body is the raise the hit
    /// arm performs (`beamtalk_class_shadow_flags`, `key_body_holds/3`).
    /// The hit arm raises exactly what that body raises: the primitive with
    /// the receiving class's name in place of its `ClassSelf`. `arg_refs` must
    /// be variables or literals: they are spliced into both arms.
    fn guarded_caller_safe_raise_doc(
        &mut self,
        class_name: &str,
        module_name: String,
        raw_selector: &str,
        primitive: &str,
        arg_refs: Vec<Document<'static>>,
    ) -> Result<Document<'static>> {
        let arg_count = arg_refs.len();
        let raise = super::primitives::generate_class_signal_bif(
            primitive,
            &arg_refs,
            leaf::atom(class_name.to_string()),
        )
        .ok_or_else(|| {
            CodeGenError::Internal(format!(
                "caller-safe class primitive `{primitive}` did not lower for `{class_name} {raw_selector}`"
            ))
        })?;
        let raise = self.annotate_current_send_call(raise);
        let key_doc = docvec![
            "{'caller_safe_raise', ",
            leaf::atom(primitive.to_string()),
            ", ",
            leaf::atom(module_name),
            ", ",
            leaf::atom(super::selector_mangler::safe_class_method_fn_name(
                raw_selector
            )),
            ", ",
            leaf::atom(super::util::metaclass_tag(class_name)),
            "}"
        ];
        let class_self_var = self.fresh_temp_var("StaticSelf");
        Ok(self.keyed_static_send_doc(
            class_name,
            raw_selector,
            &key_doc,
            &class_self_var,
            Self::join_docs_with_commas(arg_refs),
            arg_count + 1,
            raise,
        ))
    }

    /// The shared shape of a keyed static class send (BT-4316): read the
    /// class's static call map (`'<Name> class static calls'`, BT-4442),
    /// match the call target's key in it, install it on a miss
    /// (`beamtalk_class_shadow_flags:static_call_target/4`), and run `hit_doc`
    /// (which may read `class_self_var`, the key's class reference) when it
    /// is present, else the dynamic `class_send` fallback. `fun_arity` is the
    /// arity of the function the key names (`ClassSelf` included, except for
    /// a constructor key's `new/0`, BT-4409).
    #[allow(clippy::too_many_arguments)] // One shape, three hit kinds: the parts are all distinct.
    fn keyed_static_send_doc(
        &mut self,
        class_name: &str,
        raw_selector: &str,
        key_doc: &Document<'static>,
        class_self_var: &str,
        args_doc: Document<'static>,
        fun_arity: usize,
        hit_doc: Document<'static>,
    ) -> Document<'static> {
        let static_ref_var = self.fresh_temp_var("StaticRef");
        let selector_atom = super::selector_mangler::safe_class_method_selector(raw_selector);
        let fallback = self.generate_class_send_fallback(class_name, raw_selector, args_doc);
        // BT-4442: the class's one static call key (`util::static_calls_key`).
        let class_key_doc = leaf::atom(super::util::static_calls_key(class_name));
        docvec![
            "let ",
            leaf::var(class_self_var.to_string()),
            " = case call 'persistent_term':'get'(",
            class_key_doc,
            ", ~{}~) of <~{",
            key_doc.clone(),
            " := ",
            leaf::var(static_ref_var.clone()),
            "}~> when 'true' -> ",
            leaf::var(static_ref_var),
            " <_> when 'true' -> ",
            "call 'beamtalk_class_shadow_flags':'static_call_target'(",
            key_doc.clone(),
            ", ",
            leaf::atom(class_name.to_string()),
            ", ",
            leaf::atom(selector_atom),
            ", ",
            leaf::int_lit(i64::try_from(fun_arity).unwrap_or(i64::MAX)),
            ") end in case ",
            leaf::var(class_self_var.to_string()),
            " of <'false'> when 'true' -> ",
            fallback,
            " <_> when 'true' -> ",
            hit_doc,
            " end"
        ]
    }

    /// BT-4270: the defining module and primitive of the class-side method a
    /// class-literal send of `raw_selector` to `class_name` resolves to, when
    /// that method (own or inherited) is one of `Exception`'s caller-safe
    /// signal primitives that compiles to that raise in its defining class
    /// ([`super::primitives::lowers_to_caller_safe_raise`], the primitive read
    /// from its source, `MethodInfo::primitive`) and its defining class
    /// may run it in the caller ([`Self::direct_call_eligible_module`]). The
    /// send then compiles to [`Self::guarded_caller_safe_raise_doc`]: this is
    /// only the compile-time candidate; the runtime key decides each send.
    fn caller_safe_signal_target(
        &self,
        class_name: &str,
        raw_selector: &str,
    ) -> Option<(String, String)> {
        if self.is_shadowed_class(class_name) {
            return None;
        }
        let method = self
            .class_hierarchy
            .as_ref()?
            .find_class_method(class_name, raw_selector)?;
        let primitive = method
            .primitive
            .as_deref()
            .filter(|p| {
                super::primitives::lowers_to_caller_safe_raise(method.defined_in.as_str(), p)
            })?
            .to_string();
        let module = self.direct_call_eligible_module(method.defined_in.as_str(), raw_selector)?;
        Some((module, primitive))
    }

    /// Generates the `gen_server` `class_send` fallback for binding-aware dispatch.
    ///
    /// Used when a class method is not eligible for direct call optimization.
    fn generate_class_send_fallback(
        &mut self,
        class_name: &str,
        raw_selector: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        let class_selector = super::selector_mangler::safe_class_method_selector(raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        docvec![
            "let ",
            leaf::var(class_pid_var.clone()),
            " = ",
            Self::registered_class_pid_doc(class_name),
            " in ",
            self.class_send_call_doc(&class_pid_var, class_selector, args_doc),
        ]
    }

    /// Builds the shared `call
    /// 'beamtalk_object_class':'class_send'(ClassPid, Selector, [Args])`
    /// fragment — the runtime `gen_server` dispatch every class-send fallback
    /// eventually reaches, once a live `ClassPid` is in hand.
    ///
    /// BT-4366: every caller nests this call inside a `let` (the `ClassPid`
    /// binding) or `case` (the direct-call guard), where the send's
    /// whole-document line annotation never reaches the call node — so the
    /// node carries the line itself.
    fn class_send_call_doc(
        &self,
        class_pid_var: &str,
        selector_atom: String,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        self.annotate_current_send_call(docvec![
            "call 'beamtalk_object_class':'class_send'(",
            leaf::var(class_pid_var.to_string()),
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])"
        ])
    }

    /// The `direct_call_eligible` entry a send to the class *reference*
    /// `class_name` may pin, or `None` when the name is shadowed
    /// ([`Self::is_shadowed_class`], BT-4271): a shadowed name is one
    /// last-wins runtime class, so the compile-time package's module is not
    /// necessarily the one a dynamic send reaches. The single gate behind
    /// [`Self::direct_call_eligible_module`] and [`Self::direct_new0_module`].
    fn class_reference_direct_call_info(
        &self,
        class_name: &str,
    ) -> Option<&super::generator::DirectCallClassInfo> {
        if self.is_shadowed_class(class_name) {
            return None;
        }
        self.direct_call_eligible.get(class_name)
    }

    /// Shared `direct_call_eligible` gate — returns the target module name
    /// when a send of `raw_selector` to the class reference `class_name`
    /// compiles to the guarded direct call (ADR 0142 §2: the class defines the
    /// method, no class variables in its chain, not process-bound; name not
    /// shadowed).
    /// Used by every class-reference direct-call gate check
    /// (`generate_repl_bound_class_send`, `generate_class_method_call`)
    /// instead of each repeating the same lookup.
    fn direct_call_eligible_module(&self, class_name: &str, raw_selector: &str) -> Option<String> {
        self.class_reference_direct_call_info(class_name)
            .and_then(|info| {
                info.selectors
                    .contains(raw_selector)
                    .then(|| info.module_name.to_string())
            })
    }

    /// Module for a direct nullary-`new` call, if eligible.
    ///
    /// Companion to [`direct_call_eligible_module`](Self::direct_call_eligible_module)
    /// for the `new` constructor, which cannot live in `selectors` (its
    /// direct form takes no `ClassSelf` argument).
    fn direct_new0_module(&self, class_name: &str) -> Option<String> {
        self.class_reference_direct_call_info(class_name)
            .and_then(|info| info.direct_new0.then(|| info.module_name.to_string()))
    }

    /// Builds the shared `safe_dispatch/4` self-send fragment used by every
    /// non-sealed self-dispatch call site (self-cast, discarding
    /// self-dispatch and open self-dispatch).
    ///
    /// BT-3666: the callee module is looked up at run time from the actor's
    /// own `'__class_mod__'` state key — the module of the *instance's* class
    /// (each compiled actor's `init/1` stamps its own module there) — rather
    /// than bound statically to the module the send was compiled in. A
    /// method inherited from a superclass (or flattened in from a trait)
    /// that does `self foo` therefore reaches a subclass override of `foo`
    /// (the template-method pattern), exactly as `Actor>>self_dispatch/2`
    /// (aliased self-sends) already does. The `'maps':'get'` default is the
    /// compiling module, so a state without the key keeps the old binding.
    ///
    /// ```erlang
    /// let _Args = [Args] in
    /// let _CM = call 'maps':'get'('__class_mod__', State, 'module') in
    ///   case call 'erlang':'function_exported'(_CM, 'safe_dispatch', 4) of
    ///     <'true'>  -> call _CM:'safe_dispatch'('selector', _Args, Self, State)
    ///     <_>       -> call _CM:'safe_dispatch'('selector', _Args, State)
    ///   end
    /// ```
    ///
    /// BT-3692: the `function_exported` arm keeps a receiver module compiled by an
    /// older compiler (no `safe_dispatch/4`, e.g. a stale package beam subclassing a
    /// recompiled class) working through `/3` instead of raising `undef`.
    ///
    /// Sealed classes never reach this helper: they cannot be subclassed, so
    /// their self-sends stay statically bound (see the sealed branches).
    fn safe_dispatch_call_doc(
        &mut self,
        selector_atom: impl Into<String>,
        args_doc: Document<'static>,
        state_var: String,
    ) -> Document<'static> {
        let class_mod_var = self.fresh_temp_var("ClassMod");
        let args_var = self.fresh_temp_var("SDArgs");
        let selector = selector_atom.into();
        // The args are bound once so the two arms below do not duplicate (and
        // re-evaluate) the argument expressions.
        docvec![
            "let ",
            leaf::var(args_var.clone()),
            " = [",
            args_doc,
            "] in let ",
            leaf::var(class_mod_var.clone()),
            " = call 'maps':'get'('__class_mod__', ",
            leaf::var(state_var.clone()),
            ", ",
            leaf::atom(self.module_name.clone()),
            ") in case call 'erlang':'function_exported'(",
            leaf::var(class_mod_var.clone()),
            ", 'safe_dispatch', 4) of <'true'> when 'true' -> call ",
            leaf::var(class_mod_var.clone()),
            ":'safe_dispatch'(",
            leaf::atom(selector.clone()),
            ", ",
            leaf::var(args_var.clone()),
            ", ",
            // BT-3692: pass the in-scope `Self` instead of making the callee
            // rebuild it with `beamtalk_actor:make_self/1`.
            leaf::var("Self"),
            ", ",
            leaf::var(state_var.clone()),
            ") <_> when 'true' -> call ",
            leaf::var(class_mod_var),
            ":'safe_dispatch'(",
            leaf::atom(selector),
            ", ",
            leaf::var(args_var),
            ", ",
            leaf::var(state_var),
            ") end"
        ]
    }
}

// NOTE: class_method_module_name and related helpers (is_primitive_stdlib_class,
// is_bt_stdlib_class, is_erlang_stdlib_module) have been removed.
// Class dispatch now goes through runtime class_send/3 instead of
// compile-time module name resolution.

/// Class-module auto-exports reachable via `self <sel>`
/// from inside a class method.
///
/// Every compiled class module carries a small set of 0-arity functions
/// generated by codegen (not user-defined), used for reflection. These are
/// not installed in `class_method_selectors` and the runtime chain walker
/// does not index them (it reads only user-defined `class_methods` on each
/// ancestor `gen_server`).
///
/// `superclass` is not on this path: its raw export returns the bare
/// class-name atom, not the `#beamtalk_object{}` a Beamtalk-level
/// `self superclass` must produce. It takes the virtual self-send path,
/// whose class-object protocol fallthrough (BT-4252) answers what an
/// external `C superclass` answers. `class_name` stays here, returning the
/// raw atom from its export.
///
/// Intentionally narrow. The three audited exports on every user class
/// module are `superclass/0`, `class_name/0`, and `method_table/0` — but
/// only `class_name` is both plausibly reachable via Beamtalk source and
/// safe to serve from its raw export (`method_table` is a codegen-internal
/// metadata accessor without a stable user-level API; `__beamtalk_meta`,
/// `register_class`, `has_method` are similarly internal). `methods` was
/// initially in this list by mistake — no class module exports `methods/0`,
/// so including it would have compiled `self methods` to a non-existent
/// direct call (would crash with `undef`). Any auto-export added later that
/// is reachable via plain self-send needs to be added here only if its raw
/// export form is the desired behavior; a Behaviour-protocol selector needs
/// nothing, since the virtual self-send path already answers it. Unknown
/// selectors take the inherited-dispatch path, which raises structured DNU
/// on miss — strictly better than the old fallthrough (direct call
/// → runtime `undef`).
pub(super) fn is_class_auto_export_selector(selector_atom: &str, arity: usize) -> bool {
    arity == 0 && selector_atom == "class_name"
}

/// The call shape a spawn-family send to a class reference was lowered to,
/// passed to `beamtalk_class_instantiation:resolve_spawn_module_or_raise/3`
/// so a shadowed class resolved at call time is checked against it (BT-4404).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SpawnTargetKind {
    /// `Module:spawn/0,1` or `Module:spawn_monitored/0,1`: needs an actor.
    Actor,
    /// `beamtalk_supervisor:class_start/4`: needs a supervisor.
    Supervisor,
}

impl SpawnTargetKind {
    /// The `Kind` atom the runtime helper takes.
    fn runtime_atom(self) -> &'static str {
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
pub(super) fn is_supervisor_spawn_selector(selector_atom: &str, arity: usize) -> bool {
    matches!(
        (selector_atom, arity),
        ("spawn" | "spawnMonitored", 0)
            | ("spawnWith:" | "spawnAs:" | "spawnMonitoredWith:", 1)
            | ("spawnWith:as:", 2)
    )
}

#[cfg(test)]
mod tests;

/// BT-4485: the Core Erlang guard that holds when every one of `arg_names`
/// passes one of the FFI type table's [`FFI_ARG_RUNTIME_GUARDS`].
fn ffi_arg_runtime_guard(arg_names: &[String]) -> Document<'static> {
    arg_names
        .iter()
        .map(|name| {
            FFI_ARG_RUNTIME_GUARDS
                .iter()
                .map(|bif| {
                    docvec![
                        "call 'erlang':",
                        leaf::atom(*bif),
                        "(",
                        leaf::var(name.clone()),
                        ")"
                    ]
                })
                .reduce(|acc, test| docvec!["call 'erlang':'or'(", acc, ", ", test, ")"])
                .unwrap_or(Document::Str("'true'"))
        })
        .reduce(|acc, test| docvec!["call 'erlang':'and'(", acc, ", ", test, ")"])
        .unwrap_or(Document::Str("'true'"))
}
