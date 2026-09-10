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
//! compilation, while call-site dispatch uses runtime type checking since we
//! don't have static type information.
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
use super::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, VersionedVar,
};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MessageSelector, WellKnownSelector};
use beamtalk_core::source_analysis::Span;

/// One `generate_message_send` dispatch strategy: `Some(doc)` claims the
/// send, `None` defers to the next entry in [`HANDLERS`].
type SendHandler = fn(
    &mut CoreErlangGenerator,
    &Expression,
    &MessageSelector,
    &[Expression],
) -> Result<Option<Document<'static>>>;

/// `generate_message_send`'s dispatch priority as data, replacing
/// what were 15 source-ordered `if let Some(doc) = self.try_*()?` calls (two
/// of them a duplicated `is_character_typed_receiver` check — see
/// [`CoreErlangGenerator::try_handle_character_typed_message`]'s doc, now
/// this table's single `character_typed` entry). Order is significant and
/// load-bearing: earlier entries intentionally preempt later, more generic
/// ones (e.g. `character_typed` before `protoobject`/`object`, `dict` before
/// `list`, `class_method_self_send` before `self_dispatch`) — see each
/// handler's own doc for why. A send that no entry claims falls through to
/// `generate_message_send`'s `generate_runtime_dispatch` default.
const HANDLERS: &[(&str, SendHandler)] = &[
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
];

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
    /// `await_initialize/1` confirms `handle_continue(initialize, _)` has
    /// finished (see `gen_server/spawn.rs`'s `instance_registration_doc`). A
    /// self-send that raises *during* `initialize` therefore runs before this
    /// actor's own registry entry exists, so `lookup_class(self())` falls
    /// back to `'unknown'` for that narrow window — trading a guaranteed-
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
    /// # Generated Code
    ///
    /// ```erlang
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
        let type_var = self.fresh_var(&format!("{var_prefix}Type"));
        let reason_var = self.fresh_var(&format!("{var_prefix}Reason"));
        let stack_var = self.fresh_var(&format!("{var_prefix}Stack"));
        let plain_error_var = self.fresh_var(&format!("{var_prefix}Plain"));
        let class_var = self.fresh_var(&format!("{var_prefix}Class"));
        let no_match_fallback = self.case_clause_fallback(&format!("{var_prefix}NoMatch"));
        docvec![
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

    /// Wrap a class-method call that may return either a
    /// plain value or a `{'class_var_result', Result, NewClassVars}` tuple,
    /// threading the new class-var binding and exposing the unwrapped result.
    ///
    /// Emits (preamble elided):
    ///
    /// ```erlang
    /// let _CMR       = <call_doc> in
    /// let ClassVarsN = case _CMR of
    ///                    <{'class_var_result', _MR, _CV}> when 'true' -> _CV
    ///                    <_PCV>                            when 'true' -> ClassVars<current>
    ///                  end in
    /// let _Unwrapped = case _CMR of
    ///                    <{'class_var_result', _WR, _}>    when 'true' -> _WR
    ///                    <_PR>                             when 'true' -> _PR
    ///                  end in
    /// ```
    ///
    /// ADR 0118 phase 5a/5b: returns a [`ThreadedValue`]
    /// whose prelude carries the real `ClassVars` `Bind` this call rebinds —
    /// `_Unwrapped` is the value, with no consuming body of its own. Callers
    /// splice the prelude into their own frame, or close it
    /// ([`Self::close_threaded_value_doc`]) so `ClassVarsN` stays visible to
    /// the continuation. Shared by the local-class-method branch (branch 1)
    /// and the inherited-dispatch branch in
    /// [`generate_class_method_self_send`](Self::generate_class_method_self_send).
    pub(super) fn emit_class_var_result_unwrap(
        &mut self,
        args_prelude: Vec<ThreadedStmt>,
        call_doc: Document<'static>,
    ) -> ThreadedValue {
        let call_result = self.fresh_temp_var("CMR");
        let cv = self.current_class_var();
        // the version numbers driving both verify() and the real
        // Bind rendered below — captured before minting, matching the old
        // `cv`/`new_cv` name-capture ordering exactly (fresh_temp_var call
        // order for CV/MR/PCV below is unaffected: `class_var_version`
        // mints from an entirely separate counter).
        let source_version = self.class_var_version();
        let inner_cv = self.fresh_temp_var("CV");
        let inner_res = self.fresh_temp_var("MR");
        let plain_cv = self.fresh_temp_var("PCV");

        // The class-var rebind's opaque RHS (ADR 0111 Addendum 2 "Gap 1"'s
        // `ValueRef::Doc` precedent): a `case` expression selecting the
        // inherited self-dispatch call's own returned class vars when
        // present, falling back to the current `ClassVars` otherwise — not
        // representable by a bare `ValueRef::Var`/`Version`/`Literal`.
        let class_var_case_doc = docvec![
            "case ",
            leaf::var(call_result.clone()),
            " of <{'class_var_result', ",
            leaf::var(inner_res),
            ", ",
            leaf::var(inner_cv.clone()),
            "}> when 'true' -> ",
            leaf::var(inner_cv.clone()),
            " <",
            leaf::var(plain_cv),
            "> when 'true' -> ",
            leaf::var(cv),
            " end",
        ];

        self.next_class_var();
        let target_version = self.class_var_version();

        // ADR 0111 Phase D: construct, verify, and
        // render this Bind through the real `threaded_ir` pipeline — no
        // second, hand-rolled `Document` reconstructs it. This site never
        // itself needs the ADR 0110 shadow write — it rebinds `ClassVarsN`
        // from an inherited self-dispatched call's own `class_var_result`
        // return, and that call runs in this same class's gen_server
        // process, so any mutation it made was already shadow-written by
        // the *callee's own* `generate_field_assignment` under the
        // identical `ClassSelf`-tagged key (see
        // `threaded_ir::construct_and_verify_class_var_bind`'s doc comment
        // / ADR 0110 §Runtime change). `shadow_write_eligible: false` is
        // what deliberately exempts this Bind from the shadow-write check
        // unconditionally, since it never carries a shadow-write obligation
        // of its own regardless of block_depth (ADR 0111 Addendum 9,
        // Question 2).
        //
        // ADR 0111 Addendum 9, Questions 2/5: `frame` is
        // `FrameId::ROOT` at a class method's own top level, but this same
        // function is also reached from INSIDE a `do:`/`collect:`/`select:`/
        // `inject:into:` fold body's closure (a same-class self-send used or
        // discarded there compiles through the identical
        // `generate_class_method_self_send` path) — `self.in_loop_body`
        // distinguishes the two: a fold-body call site's real, already-minted
        // frame is `self.current_branch_frame()`, never `ROOT` (loop bodies
        // always run inside `with_branch_context`). Passing `ROOT` there
        // would be dishonest about this rebind's real nesting identity and
        // would let a second, unrelated mutation later in the SAME iteration
        // spuriously collide during verification (Question 2's own
        // `UnboundVersion` finding for the analogous top-frame field-write
        // call site).
        let frame = if self.in_loop_body {
            self.current_branch_frame()
        } else {
            super::threaded_ir::FrameId::ROOT
        };
        let (bind, rebind_errors) = super::threaded_ir::construct_and_verify_class_var_bind(
            super::threaded_ir::BindOp::Direct(super::threaded_ir::ValueRef::Doc(
                class_var_case_doc,
            )),
            false,
            frame,
            false,
            source_version,
            target_version,
            beamtalk_core::source_analysis::Span::default(),
        );
        self.report_threaded_ir_verify_errors(
            &rebind_errors,
            "class-var rebind from inherited self-dispatch result",
            beamtalk_core::source_analysis::Span::default(),
        );

        let result = self.fresh_temp_var("Unwrapped");
        let wrapped_res = self.fresh_temp_var("WR");
        let plain_res = self.fresh_temp_var("PR");

        // ADR 0118 phase 5a: the call-setup and unwrap steps stay opaque
        // `Statement`s (the SAME `Document` text this function always built,
        // byte-for-byte); `bind` is no longer rendered eagerly into the
        // middle of one big `Document` — it is a real, un-rendered
        // `ThreadedStmt::Bind` in the returned prelude, so the class-var
        // mutation this call rebinds is visible to whichever `ThreadedIr`
        // frame the caller splices the prelude into (ADR 0118 §Decision 4),
        // not just to this producer's own isolated `construct_and_verify_class_var_bind`
        // check above.
        let call_stmt_doc = docvec![
            "let ",
            leaf::var(call_result.clone()),
            " = ",
            call_doc,
            " in ",
        ];
        let unwrap_stmt_doc = docvec![
            "let ",
            leaf::var(result.clone()),
            " = case ",
            leaf::var(call_result),
            " of <{'class_var_result', ",
            leaf::var(wrapped_res.clone()),
            ", _}> when 'true' -> ",
            leaf::var(wrapped_res),
            " <",
            leaf::var(plain_res.clone()),
            "> when 'true' -> ",
            leaf::var(plain_res),
            " end in ",
        ];
        let span = beamtalk_core::source_analysis::Span::default();
        let mut prelude = args_prelude;
        prelude.push(ThreadedStmt::Statement(call_stmt_doc, span));
        prelude.push(bind);
        prelude.push(ThreadedStmt::Statement(unwrap_stmt_doc, span));
        ThreadedValue {
            prelude,
            value: ValueRef::Var(result),
        }
    }

    /// ADR 0111 Addendum 9, Questions 2/3: rebinds `ClassVarsN`
    /// from an already-produced value Document — a Letrec loop construct's
    /// own returned tuple slot carrying the `ClassVars` mutations threaded
    /// through its recursive tail call (`while_loops.rs`/`counted_loops.rs`
    /// via `generate_counted_stateful_loop`). Mirrors
    /// [`Self::emit_class_var_result_unwrap`]'s inherited-self-dispatch
    /// rebind: never itself a shadow-write producer (each loop iteration's
    /// own class-var write, inside the loop body, already shadow-wrote it
    /// under the identical `ClassSelf`-tagged key — ADR 0110 §Runtime
    /// change) and never claims a real nested frame identity of its own
    /// (`FrameId::ROOT`, `shadow_write_eligible: false`, per ADR 0111
    /// Addendum 9 Question 2).
    pub(super) fn rebind_class_vars_from_doc(
        &mut self,
        value_doc: Document<'static>,
        span: beamtalk_core::source_analysis::Span,
    ) -> Document<'static> {
        let source_version = self.class_var_version();
        self.next_class_var();
        let target_version = self.class_var_version();
        let (bind, errors) = super::threaded_ir::construct_and_verify_class_var_bind(
            super::threaded_ir::BindOp::Direct(super::threaded_ir::ValueRef::Doc(value_doc)),
            false,
            super::threaded_ir::FrameId::ROOT,
            false,
            source_version,
            target_version,
            span,
        );
        self.report_threaded_ir_verify_errors(
            &errors,
            "class-var rebind from a loop construct's threaded result",
            span,
        );
        let mut ctx = super::threaded_ir::RenderCtx::new(self);
        super::threaded_ir::render(std::slice::from_ref(&bind), &mut ctx)
    }

    /// the value-type `Self` mirror of
    /// [`Self::rebind_class_vars_from_doc`] — rebinds `Self{N}` from an
    /// already-produced value `Document` (a Letrec loop construct's own
    /// returned trailing tuple slot, carrying the `self.field := ...`
    /// mutations threaded through its recursive tail call). This is what
    /// makes the loop's final `Self` the method's new LIVE `Self` for every
    /// statement that follows — the direct counterpart of Actor's `let
    /// State1 = element(2, _CF10) in`.
    ///
    /// Simpler than the class-var sibling: `SelfVt` carries none of ADR
    /// 0110's shadow-write obligation, so this verifies through the same
    /// plain [`Self::check_simple_field_bind_invariant`] every other
    /// `Self{N}`/`State{N}` version step already uses.
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
            "value-type Self rebind from a loop construct's threaded result",
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
            shadow_write: false,
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
            if op.as_str() == ">>" {
                if let Expression::ClassReference { name, .. } = receiver {
                    return self.generate_method_lookup(&name.name, arguments);
                }
                // Runtime fallback: evaluate receiver and call method/2
                return self.generate_runtime_method_lookup(receiver, arguments);
            }

            let doc = self.generate_binary_op(op, receiver, arguments)?;
            return Ok(doc);
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
    /// `is_character_typed_receiver` (a Character literal, or a `Character
    /// value:` factory call). Collapses what used to be two separate
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
    /// (a Character literal or a `Character value:` factory call).
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

    /// Generates a cast (fire-and-forget) message send.
    ///
    /// Called when the AST `MessageSend` node has `is_cast: true` (the `!` suffix).
    ///
    /// # Dispatch Strategy
    ///
    /// - **Self-sends in actor context** (`self someMethod!`): Calls `safe_dispatch` directly
    ///   but discards both the result and any state update — fire-and-forget semantics
    ///   within the same process.
    /// - **All other sends**: Routes through `beamtalk_message_dispatch:cast/3`, which
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
        // Self-sends with ! in actor context: direct dispatch, discard result.
        // Only use direct safe_dispatch when NOT inside a block (block_depth == 0).
        // Blocks may execute in a different process (Timer callbacks, cross-actor callbacks),
        // so self-cast sends inside blocks must route through the actor mailbox via
        // beamtalk_message_dispatch:cast/3 to reach the actor's gen_server process.
        if self.context == CodeGenContext::Actor && self.block_depth == 0 {
            if let Expression::Identifier(id) = receiver {
                if id.name == "self" {
                    return self.generate_self_cast_send(selector, arguments);
                }
            }
        }

        // Non-self cast sends (and self-casts inside blocks): route through
        // beamtalk_message_dispatch:cast/3
        self.generate_runtime_cast(receiver, selector, arguments)
    }

    /// Generates a self-cast send in actor context.
    ///
    /// Calls `safe_dispatch` synchronously but discards the result (and any state
    /// mutation from the callee). Returns `'ok'` as the expression value.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _Cast0 = call 'module':'safe_dispatch'('selector', [Args], State) in 'ok'
    /// ```
    fn generate_self_cast_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.name().to_string();
        let discard_var = self.fresh_temp_var("Cast");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            "let ",
            leaf::var(discard_var),
            " = ",
            Self::safe_dispatch_call_doc(module, selector_atom, args_doc, current_state),
            " in 'ok'",
        ];

        Ok(doc)
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

        // Emit dynamic dispatch fallback diagnostic.
        if self.codegen_diagnostics_enabled {
            let span = receiver.span();
            let line_info = self
                .span_to_line(span)
                .map_or(String::new(), |l| format!(" at line {l}"));
            self.emit_codegen_diagnostic(
                format!(
                    "Send '{selector_atom}'{line_info}: dynamic dispatch (receiver type unknown)"
                ),
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
        let mut seq = self.sequence_call(&all_exprs, "Disp")?;
        let actual_receiver = seq.next();
        let args_doc = Self::join_docs_with_commas(seq.rest());

        let call_doc = docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            actual_receiver,
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])"
        ];

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
        // Unary spawn/await messages
        if let MessageSelector::Unary(name) = selector {
            // Only match ClassReference, not Identifier.
            if name == "spawn" && arguments.is_empty() {
                if let Expression::ClassReference { name, package, .. } = receiver {
                    let pkg = package.as_ref().map(|p| p.name.as_str());
                    let doc = self.generate_actor_spawn_qualified(&name.name, pkg, None)?;
                    return Ok(Some(doc));
                }
            }
            if name == "await" && arguments.is_empty() {
                let doc = self.generate_await(receiver)?;
                return Ok(Some(doc));
            }
            if name == "awaitForever" && arguments.is_empty() {
                let doc = self.generate_await_forever(receiver)?;
                return Ok(Some(doc));
            }
        }

        // Keyword await:/spawnWith: messages
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "await:" && arguments.len() == 1 {
                let doc = self.generate_await_with_timeout(receiver, &arguments[0])?;
                return Ok(Some(doc));
            }
            // Only match ClassReference, not Identifier.
            if parts.len() == 1 && parts[0].keyword == "spawnWith:" && arguments.len() == 1 {
                if let Expression::ClassReference { name, package, .. } = receiver {
                    let pkg = package.as_ref().map(|p| p.name.as_str());
                    let doc =
                        self.generate_actor_spawn_qualified(&name.name, pkg, Some(&arguments[0]))?;
                    return Ok(Some(doc));
                }
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
    /// `beamtalk_erlang_proxy:direct_call/3` for automatic binary→charlist coercion:
    /// - Unary: `node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [])` (zero-arg)
    /// - Keyword single: `reverse:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'reverse', [Xs])`
    /// - Keyword multi: `seq:with:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'seq', [1, 10])`
    ///
    /// Returns `None` for selectors that are Object/ProtoObject protocol methods
    /// (e.g. `printString`, `asString`) — these must go through runtime dispatch
    /// so the proxy's inherited protocol methods are called, not a non-existent
    /// Erlang function.
    ///
    /// Block arguments are automatically wrapped via
    /// [`generate_erlang_interop_wrapper`] to strip the Tier 2 `StateAcc` protocol.
    /// A diagnostic warning is emitted when a stateful block (one with captured
    /// mutations) crosses the Erlang boundary, since mutations will be dropped.
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
                // Route zero-arg calls through proxy (consistent with keyword sends).
                // `Erlang erlang node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [])`
                let doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'(",
                    leaf::atom(module_name.to_string()),
                    ", ",
                    leaf::atom(function_name.to_string()),
                    ", [])"
                ];
                Ok(Some(doc))
            }
            MessageSelector::Keyword(parts) => {
                // Extract function name from first keyword (before the colon)
                let function_name = parts[0].keyword.trim_end_matches(':');

                // Process arguments individually so Block arguments can be
                // wrapped via generate_erlang_interop_wrapper before crossing the
                // Erlang boundary. Non-block arguments pass through unchanged.
                let mut preamble_docs: Vec<Document<'static>> = Vec::new();
                let mut arg_parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());

                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        arg_parts.push(Document::Str(", "));
                    }
                    if let Some(block) = Self::extract_block_literal(arg) {
                        // A block crossing the Erlang
                        // interop boundary here goes through
                        // `generate_erlang_interop_wrapper` → `generate_block`,
                        // the same same-process, in-process closure mechanism as
                        // a `select:`/`do:` argument — see
                        // `check_no_unsafe_class_method_self_sends`'s doc
                        // comment.
                        let analysis = crate::core_erlang::block_analysis::analyze_block(block);
                        self.check_no_unsafe_class_method_self_sends(&analysis, block.span)?;
                        let (wrapped_doc, is_stateful) =
                            self.generate_erlang_interop_wrapper(block)?;
                        if is_stateful {
                            self.warn_stateful_block_at_erlang_boundary(
                                &format!("'{module_name}':'{function_name}'"),
                                block.span,
                            );
                        }
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
                    } else {
                        arg_parts.push(self.expression_doc(arg)?);
                    }
                }

                // Route through beamtalk_erlang_proxy:direct_call/3 to
                // enable binary→charlist coercion for functions like os:cmd/1.
                // Args are wrapped in a list: call 'proxy':'direct_call'('M','F',[args])
                let call_doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'(",
                    leaf::atom(module_name.to_string()),
                    ", ",
                    leaf::atom(function_name.to_string()),
                    ", [",
                    Document::Vec(arg_parts),
                    "])"
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
            "})"
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
            // When inside a class method and the explicit class name matches
            // the current class, use direct dispatch (same as `self` sends) to avoid
            // deadlock. The class actor is already processing the outer call, so
            // routing through class_send would deadlock on gen_server:call.
            if self.in_class_method() && name.name == self.class_name() && pkg.is_none() {
                // ADR 0118 phase 5b: reached through ordinary
                // `generate_expression`/`generate_message_send` (not
                // `threaded_expression`'s own producer recognition), so the
                // producer's prelude is closed inline into a self-contained
                // `Document` here rather than left open.
                let tv = self.generate_class_method_self_send(selector, arguments)?;
                return Ok(Some(self.close_threaded_value_doc(tv)));
            }
            if self.workspace_mode() && self.context == CodeGenContext::Repl {
                // REPL top-level: check session bindings first
                let doc =
                    self.generate_binding_aware_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            if self.workspace_mode() {
                // Actor/ValueType methods in workspace mode: try class_send,
                // fall back to workspace binding for convenience names
                let doc = self.generate_workspace_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            // ADR 0070 Phase 2: Class method calls always go through the class
            // registry using the short class name. The package qualifier doesn't
            // affect dispatch — it's used for module name resolution in spawns and
            // standalone references, not for class method calls.
            let doc = self.generate_class_method_call(&name.name, selector, arguments)?;
            return Ok(Some(doc));
        }
        Ok(None)
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
        if self.context == CodeGenContext::Actor {
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
    /// For user-defined class methods, generates `class_<selector>(ClassSelf, ClassVars, ...)`.
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
                let tv = self.generate_class_method_self_send(selector, arguments)?;
                return Ok(Some(self.close_threaded_value_doc(tv)));
            }
        }
        Ok(None)
    }

    /// Core logic for direct dispatch of class method calls.
    ///
    /// Used by both `self` sends and explicit class name sends within
    /// class methods. Generates direct module function calls to avoid deadlock
    /// since class methods execute inside a `gen_server:call` handler.
    ///
    /// ADR 0118 phase 5b: returns a [`ThreadedValue`] whose
    /// prelude is real `ThreadedStmt`s throughout — every branch threads
    /// its arguments via [`Self::thread_args`] and either folds the
    /// resulting prelude into its own class-var `Bind`
    /// ([`Self::emit_class_var_result_unwrap`]) or, for a branch with no
    /// class-var `Bind` of its own (instantiation intrinsics, reflective
    /// primitives, auto-exports, the slot constructor), closes the
    /// argument prelude into a self-contained call `Document`
    /// ([`Self::close_prelude`]) and wraps it as a pure `ThreadedValue`.
    #[allow(clippy::too_many_lines)] // Multiple dispatch branches share args-capture scaffolding.
    pub(super) fn generate_class_method_self_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<ThreadedValue> {
        let selector_atom = selector.name().to_string();

        // ADR 0084: inside a programmatic ClassBuilder class-method fun
        // there is no `class_<sel>` module export to call, so self-sends route
        // through the runtime dispatch helper (own runtime fun first, then the
        // super/inherited chain), threading ClassVars via the standard
        // `{class_var_result, …}` unwrap. Instantiation intrinsics (self new /
        // spawn) still use the process-dict-backed helpers (no export needed).
        if let Some(builder_class) = self.builder_class_method_class() {
            if let Some(doc) = self.try_instantiation_intrinsic(&selector_atom, arguments)? {
                return Ok(ThreadedValue {
                    prelude: Vec::new(),
                    value: ValueRef::Doc(doc),
                });
            }
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let cv = self.current_class_var();
            let call_doc = docvec![
                "call 'beamtalk_class_dispatch':'class_self_dispatch_local'(",
                leaf::atom(builder_class),
                ", ",
                leaf::atom(selector_atom),
                ", ",
                leaf::var(cv),
                ", [",
                args_doc,
                "])"
            ];
            return Ok(self.emit_class_var_result_unwrap(args_preamble, call_doc));
        }

        if self.class_method_selectors().contains(&selector_atom) {
            // Route to class_<selector>(ClassSelf, ClassVars, ...)
            let module = self.module_name.clone();
            // Hoist any open let-chains from sub-expression class
            // method self-sends in the args. The preamble must be emitted
            // before our own `let _CMR = ...` so the ClassVarsN bindings it
            // produces stay in scope at the outer level. capture_args_with_preamble
            // does NOT roll back class_var_version, so the snapshot we take
            // afterwards (`cv`) reflects the post-args version — that is the
            // ClassVars binding to thread into the callee.
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let cv = self.current_class_var();
            let comma = if arguments.is_empty() { "" } else { ", " };

            // Apply the same atom-length guard used by the
            // keyword-constructor path below — long selectors must be hashed to
            // stay within Erlang's 255-char atom limit.
            let safe_fn = super::selector_mangler::safe_class_method_fn_name(&selector_atom);
            let call_doc = docvec![
                "call ",
                leaf::atom(module),
                ":",
                leaf::atom(safe_fn),
                "(ClassSelf, ",
                leaf::var(cv),
                comma,
                args_doc,
                ")"
            ];
            // NOTE: prelude is OPEN — caller splices or open-scope-converts it.
            return Ok(self.emit_class_var_result_unwrap(args_preamble, call_doc));
        }
        // Auto-generated keyword constructor for Value subclass: classes.
        // `ClassName slot: value` inside a class method routes here when the selector
        // matches the auto-generated slot keyword constructor (e.g. `symName:` → `class_symName:/3`).
        // The constructor returns a plain map (not a `class_var_result` tuple), so no
        // class-var threading boilerplate is needed.
        if self
            .class_slot_constructor_selector()
            .map(String::as_str)
            .is_some_and(|kw| kw == selector_atom)
        {
            let module = self.module_name.clone();
            // Hoist preambles from sub-expression class var mutations
            // in the args. cv is read AFTER capture_args_with_preamble so it
            // reflects the post-args ClassVars version.
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let cv = self.current_class_var();
            let comma = if arguments.is_empty() { "" } else { ", " };
            // Hash long keyword constructor atoms to stay within
            // Erlang's 255-char atom limit.
            let safe_fn = super::selector_mangler::safe_class_method_fn_name(&selector_atom);
            let call_doc = docvec![
                "call ",
                leaf::atom(module),
                ":",
                leaf::atom(safe_fn),
                "(ClassSelf, ",
                leaf::var(cv),
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

        // Behaviour-protocol reflective primitives (`superclass`,
        // `includesSelector:`, ...) are not compiled class exports — they are
        // `@primitive`-backed methods inherited from `Behaviour`/`Class` and
        // normally resolved via `try_class_chain_fallthrough`'s
        // `beamtalk_dispatch:lookup/5` walk. That walk needs a `ClassPid` that
        // isn't `self()` (it goes through `gen_server:call` for the class
        // method table), so it cannot run from inside the class's own
        // process. Route these selectors directly to the real
        // `beamtalk_behaviour_intrinsics` implementation instead, passing the
        // closure-captured `ClassSelf` as receiver — every entry here is
        // deadlock-safe specifically because its implementation resolves the
        // class module via `beamtalk_object_class:module_name_safe/1` (which
        // has a `ClassPid =:= self()` fast path reading the process
        // dictionary) and looks up class metadata from
        // `__beamtalk_meta/0` / ETS rather than calling back into this
        // process's own gen_server. `class_self_send_reflective_primitive`
        // must stay in sync with that safety property — do not add a
        // selector here whose intrinsic can call `gen_server:call(ClassPid,
        // ...)` unconditionally (e.g. `classSubclasses/1`,
        // `classAllSubclasses/1`, `className/1` as of this writing).
        if let Some(fun_name) =
            class_self_send_reflective_primitive(&selector_atom, arguments.len())
        {
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let comma = if arguments.is_empty() { "" } else { ", " };
            let call_doc = docvec![
                "call 'beamtalk_behaviour_intrinsics':",
                leaf::atom(fun_name),
                "(",
                leaf::var("ClassSelf"),
                comma,
                args_doc,
                ")"
            ];
            let doc = self.close_prelude(&args_preamble, call_doc, "ReflectivePrimitive");
            return Ok(ThreadedValue {
                prelude: Vec::new(),
                value: ValueRef::Doc(doc),
            });
        }

        // Inherited class method — walk the hierarchy at runtime and
        // apply the defining module's class_<sel>(ClassSelf, ClassVars, Args...).
        // The one remaining auto-generated 0-arity export reachable via plain
        // self-send (`class_name/0`) stays on the direct-call path because the
        // chain walker only looks at user-defined class_methods, and its
        // intrinsic (`className/1`) is not deadlock-safe from inside the
        // class's own process (see the reflective-primitive branch above) —
        // `class_name/0` sidesteps that by returning the raw atom directly
        // rather than going through the primitive. The other auto-exports
        // (`method_table/0`, `has_method/1`, `register_class/0`,
        // `__beamtalk_meta/0`) are codegen-internal reflection and metadata
        // accessors without a stable user-level API, so they are deliberately
        // NOT on the direct-call path — `is_class_auto_export_selector` must
        // stay in sync with the actual reachable set. Anything else that falls
        // through here — inherited or missing — routes through
        // class_self_dispatch/4, which raises a structured does_not_understand
        // error for genuine DNU.
        if is_class_auto_export_selector(&selector_atom, arguments.len()) {
            // Hoist preambles from sub-expression class var mutations.
            let module = self.module_name.clone();
            let fun_name = selector_atom.replace(':', "");
            let (args_preamble, args_doc) = self.thread_args(arguments)?;

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

        let (args_preamble, args_doc) = self.thread_args(arguments)?;
        let cv = self.current_class_var();
        // ADR 0109 amendment: derive the target class from `ClassSelf`
        // (closure-captured, so correct even when this self-send executes inside a
        // block running in a foreign class's process) instead of
        // `erlang:get('beamtalk_class_name')` (the *executing process's* identity,
        // which is only the same thing outside a block). `class_name_from_tag/1`
        // strips the `' class'` metaclass tag `element(2, ClassSelf)` carries.
        let call_doc = docvec![
            "call 'beamtalk_class_dispatch':'class_self_dispatch'(",
            "call 'beamtalk_primitive':'class_name_from_tag'(call 'erlang':'element'(2, ",
            leaf::var("ClassSelf"),
            ")), ",
            leaf::atom(selector_atom),
            ", ",
            leaf::var(cv),
            ", [",
            args_doc,
            "])"
        ];
        // NOTE: prelude stays real `ThreadedStmt`s here — the caller splices
        // it into its own frame or closes it (matches the local-class-method
        // branch above).
        Ok(self.emit_class_var_result_unwrap(args_preamble, call_doc))
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
        docvec![
            "call 'beamtalk_class_instantiation':'resolve_module_or_raise'(",
            Self::class_self_name_doc(),
            ", ",
            leaf::atom(selector_atom),
            ")"
        ]
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
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector_atom {
            "new" | "new:" => {
                // Hoist preambles from sub-expression class var mutations.
                let (args_preamble, args_doc) = self.thread_args(arguments)?;
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
            "spawn" | "spawnWith:" => {
                let (args_preamble, args_doc) = self.thread_args(arguments)?;
                let call_doc = docvec![
                    "call 'beamtalk_class_instantiation':'class_self_spawn'(",
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
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let (args_preamble, args_doc) = self.thread_args(arguments)?;
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
        let prelude = vec![
            ThreadedStmt::Statement(call_doc, span),
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'erlang':'element'(2, ",
                    leaf::var(dispatch_var.clone()),
                    ")",
                ])),
                shadow_write: false,
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
        // Sealed class optimization — skip safe_dispatch try/catch
        if self.is_class_sealed() {
            return self.generate_sealed_self_dispatch(selector, arguments);
        }

        let selector_atom = selector.name().to_string();
        let result_var = self.fresh_var("SelfResult");
        let state_var = self.fresh_var("SelfState");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let error_clause = self.generate_self_dispatch_error_clause("SelfError", &selector_atom);

        let doc = docvec![
            "case ",
            Self::safe_dispatch_call_doc(module, selector_atom, args_doc, current_state),
            " of ",
            "<{'reply', ",
            leaf::var(result_var.clone()),
            ", ",
            leaf::var(state_var),
            "}> when 'true' -> ",
            leaf::var(result_var),
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// Generates self-dispatch with state threading (open binding pattern).
    ///
    /// Like `generate_self_dispatch`, but captures the new state from the dispatch
    /// result and advances the state version. The let binding is left open so
    /// subsequent expressions see the updated state.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _SD0 = case call 'module':'safe_dispatch'('sel', [Args], State) of
    ///   <{'reply', R, S}> when 'true' -> {R, S}
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end in let State1 = call 'erlang':'element'(2, _SD0) in
    /// ```
    ///
    /// The expression value `call 'erlang':'element'(1, _SD0)` is NOT emitted —
    /// it's discarded since this is used for non-last expressions in block bodies.
    ///
    /// Uses Document/docvec! (ADR 0018) for composable rendering.
    pub(super) fn generate_self_dispatch_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            return self.generate_self_dispatch_open_for(selector, arguments);
        }
        Err(CodeGenError::Internal(
            "generate_self_dispatch_open called on non-MessageSend expression".to_string(),
        ))
    }

    /// selector/arguments-based counterpart of
    /// [`Self::generate_self_dispatch_open`], for callers that only have the
    /// decomposed selector/arguments of a self-send, not the owning
    /// `Expression::MessageSend` node itself — e.g.
    /// `control_flow/conditionals.rs`'s `compile_conditional_receiver`, which
    /// destructures a conditional's (unwrapped-of-parens) receiver expression
    /// itself. `generate_self_dispatch_open` is now a thin wrapper over this.
    pub(super) fn generate_self_dispatch_open_for(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<(Document<'static>, String)> {
        let (call_doc, dispatch_var) =
            self.generate_self_dispatch_call_doc_for(selector, arguments)?;
        let new_state = self.current_state_var();
        let doc = docvec![
            call_doc,
            "let ",
            leaf::var(new_state),
            " = call 'erlang':'element'(2, ",
            leaf::var(dispatch_var.clone()),
            ") in "
        ];
        Ok((doc, dispatch_var))
    }

    /// ADR 0111 Addendum 5 (shape E2): the dispatch-call/
    /// case-clause portion of [`Self::generate_self_dispatch_open`],
    /// WITHOUT the trailing state-extraction `let` — factored out so
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
    /// back. `generate_self_dispatch_open` is unchanged in every other
    /// respect (same mint order, same returned bytes) — it now simply
    /// delegates here and appends the extraction `let` it used to build
    /// inline.
    pub(super) fn generate_self_dispatch_call_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            return self.generate_self_dispatch_call_doc_for(selector, arguments);
        }
        Err(CodeGenError::Internal(
            "generate_self_dispatch_call_doc called on non-MessageSend expression".to_string(),
        ))
    }

    /// selector/arguments-based core of
    /// [`Self::generate_self_dispatch_call_doc`] — see that function's doc
    /// comment (ADR 0111 Addendum 5) for the shape this builds and
    /// why the state-version bump (`next_state_var()`) mints exactly where it
    /// does. Factored out so [`Self::generate_self_dispatch_open_for`] (used
    /// by `compile_conditional_receiver`, which only has the decomposed
    /// selector/arguments of an unwrapped-of-parens receiver expression, not
    /// an owning `MessageSend` node) can reuse the exact same dispatch-call/
    /// state-threading shape instead of re-deriving a parallel
    /// implementation (CLAUDE.md's no-duplicate-implementations rule).
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
                let module = self.module_name.clone();
                docvec![
                    "let ",
                    leaf::var(dispatch_var.clone()),
                    " = case ",
                    Self::safe_dispatch_call_doc(module, selector_atom, args_doc, current_state),
                    " of ",
                ]
            };

            // Result/error clauses. The state-version bump
            // (`next_state_var()`) stays exactly here — mint-order fidelity
            // — but its returned name is no longer consumed for rendering;
            // `generate_self_dispatch_open` re-reads it via
            // `current_state_var()`, and the E2 `Bind`-based caller
            // (`exception_handling.rs`) re-derives it from the version
            // number via `render_bind`.
            let _ = self.next_state_var();
            let error_clause =
                self.generate_self_dispatch_error_clause("SDError", &selector_atom_for_error);
            let doc = docvec![
                call_doc,
                "<{'reply', ",
                leaf::var(result_var.clone()),
                ", ",
                leaf::var(state_var.clone()),
                "}> when 'true' -> {",
                leaf::var(result_var),
                ", ",
                leaf::var(state_var),
                "} ",
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
            leaf::var(result_var.clone()),
            ", _}> when 'true' -> ",
            leaf::var(result_var),
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
            leaf::var(result_var.clone()),
            ", _}> when 'true' -> ",
            leaf::var(result_var),
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// Generates the RHS `Document` for a `self.field := value`
    /// assignment, special-casing a block literal with field writes (and no
    /// captured-local mutations): it's generated via `generate_block_stateful`
    /// directly, bypassing `generate_block`'s "unsupported block" rejection.
    ///
    /// This is safe unconditionally for the field-writes-only case — no
    /// same-method-only safety analysis is needed here, unlike the local-var
    /// case in `gen_server/methods.rs`'s `prescan_tier2_local_vars` — because
    /// every `self.field value(:...)` call site now runtime-discriminates
    /// Tier 1 vs Tier 2 (`generate_block_value_call_runtime_discriminated`,
    /// `intrinsics.rs`), regardless of which method performs the call. The
    /// residual gap — reading the field into a local var and invoking *that*
    /// — is the same pre-existing class of gap as any other block value
    /// flowing through an untracked opaque channel.
    ///
    /// A block that *also* captures and
    /// mutates an outer local (in addition to writing a field) is NOT safe
    /// here and must fall through to `expression_doc` → `generate_block` →
    /// `validate_stored_closure`/the block-analyzer diagnostic instead.
    /// `generate_block_stateful` reads a captured local's
    /// `'__local__<var>'` key from the calling method's `StateAcc`, falling
    /// back to the value closed over at block-*definition* time when that
    /// key is absent. A field-stored block can be invoked from a different
    /// method than the one that stored it, so that fallback fires forever
    /// (stale definition-time value) and, once the returned state is merged
    /// back into the actor's persistent state, the `'__local__<var>'` key
    /// leaks into it permanently. See the matching fix in
    /// `semantic_analysis/block_analyzer.rs`.
    ///
    /// Scoped to `Actor` context only, matching the call-site fix: `ValueType`
    /// field-write Tier 2 support has not been verified safe.
    pub(super) fn generate_field_assignment_value_doc(
        &mut self,
        value: &Expression,
    ) -> Result<Document<'static>> {
        if self.context == CodeGenContext::Actor {
            if let Expression::Block(block) = value {
                let captured_mutations = Self::captured_mutations_for_block(block);
                let field_writes = super::block_analysis::analyze_block(block).field_writes;
                if captured_mutations.is_empty() && !field_writes.is_empty() {
                    return self.generate_block_stateful(block, &[]);
                }
            }
        }
        self.expression_doc(value)
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
                // ADR 0111 Addendum 9, Questions 2/3: a class-var
                // write directly inside a Letrec loop body that threads
                // `ClassVars` through the loop's own recursive tail call —
                // threaded via the SAME shared helper the method's own
                // top-frame write uses (`lower_class_var_field_assignment_bind`,
                // reached through `lower_field_write`'s `ClassVar` arm), but
                // tagged with the loop's real, already-minted frame
                // (`current_branch_frame()`) instead of `FrameId::ROOT`, per
                // Question 2's resolution. `loop_threads_class_vars` scopes
                // this to exactly the Letrec loop-body call path — see its
                // own doc comment for why it can never leak into a nested
                // Foldl body, conditional, or block literal (all of which
                // still hit `reject_class_var_field_assignment` below,
                // unchanged).
                if self.is_class_var_assignment(expr) && self.loop_mode.loop_threads_class_vars {
                    let frame = self.current_branch_frame();
                    return self.lower_field_write(
                        FieldWriteSite::ClassVar,
                        Closure::Open,
                        &field.name,
                        value,
                        frame,
                    );
                }
                self.reject_class_var_field_assignment(expr, field)?;
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
                    let val_var = self.fresh_temp_var("Val");
                    // Snapshot field params before evaluating RHS so nested field
                    // assignments (e.g. `self.x := (self.y := 42)`) don't leak
                    // inner updates past the outer assignment.
                    let saved_field_params = self.loop_mode.hybrid_readonly_field_params.clone();
                    let val_doc = self.expression_doc(value)?;
                    self.loop_mode.hybrid_readonly_field_params = saved_field_params;
                    let new_field_var = self
                        .fresh_temp_var(&format!("{}Field", Self::to_core_erlang_var(&field.name)));
                    // Update the param map so subsequent reads use the new var.
                    self.loop_mode
                        .hybrid_readonly_field_params
                        .insert(field.name.to_string(), new_field_var.clone());
                    return Ok((
                        docvec![
                            "let ",
                            leaf::var(val_var.clone()),
                            " = ",
                            val_doc,
                            " in let ",
                            leaf::var(new_field_var),
                            " = ",
                            leaf::var(val_var.clone()),
                            " in ",
                        ],
                        val_var,
                    ));
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
            let name_var = self.fresh_var("Name");
            let val_var = self.fresh_temp_var("Val");
            let name_code = self.expression_doc(&arguments[0])?;
            // Capture state before value expression, consistent with
            // generate_field_assignment_open. If the value expression itself
            // threads state (e.g., contains a nested field assignment), the
            // maps:put uses the pre-value state — same semantics as self.x := expr.
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
        let selector_atom = selector.name().to_string();

        // ADR 0084: `super` inside a builder class-method fun resolves
        // up the metaclass chain via the runtime helper, keyed on the builder
        // class name — `class_self_dispatch/4` begins the walk at that class's
        // superclass, which is exactly super semantics. The fun has no module
        // export, so this must not use the compiled `beamtalk_dispatch:super/5`
        // instance path below.
        if let Some(builder_class) = self.builder_class_method_class() {
            let (args_preamble, args_doc) = self.thread_args(arguments)?;
            let cv = self.current_class_var();
            let call_doc = docvec![
                "call 'beamtalk_class_dispatch':'class_self_dispatch'(",
                leaf::atom(builder_class),
                ", ",
                leaf::atom(selector_atom),
                ", ",
                leaf::var(cv),
                ", [",
                args_doc,
                "])"
            ];
            // ADR 0118 phase 5b: reached through ordinary
            // `generate_expression` — close the producer's prelude inline.
            let tv = self.emit_class_var_result_unwrap(args_preamble, call_doc);
            return Ok(self.close_threaded_value_doc(tv));
        }

        let class_name = self.class_name();
        let args_doc = self.capture_argument_list_doc(arguments)?;

        // value/primitive-context funs (`fun(Args, Self) -> Result`)
        // have no `State` binding, so `super` must not reference one. Route to
        // `super_value/4`, which walks the same chain and returns a plain value.
        if self.context == CodeGenContext::ValueType {
            return Ok(docvec![
                "call 'beamtalk_dispatch':'super_value'(",
                leaf::atom(selector_atom),
                ", [",
                args_doc,
                "], Self, ",
                leaf::atom(class_name),
                ")",
            ]);
        }

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
        Ok(doc)
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
    pub(super) fn generate_actor_spawn_qualified(
        &mut self,
        class_name: &str,
        package: Option<&str>,
        init_args: Option<&Expression>,
    ) -> Result<Document<'static>> {
        let module_name = self.compiled_module_name_qualified(class_name, package);
        let in_repl_context = self.lookup_var("__bindings__").is_some();

        let args_doc = match init_args {
            Some(args) => self.expression_doc(args)?,
            None => Document::Nil,
        };

        if in_repl_context {
            let doc = docvec![
                "case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of ",
                "<'undefined'> when 'true' -> call ",
                leaf::atom(module_name.clone()),
                ":'spawn'(",
                args_doc.clone(),
                ") <RegistryPid> when 'true' -> let SpawnResult = call ",
                leaf::atom(module_name.clone()),
                ":'spawn'(",
                args_doc,
                ") in ",
                "let SpawnPid = call 'erlang':'element'(4, SpawnResult) in ",
                "let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, ",
                leaf::atom(class_name.to_string()),
                ", ",
                leaf::atom(module_name),
                ") in ",
                "SpawnResult ",
                "end"
            ];
            Ok(doc)
        } else {
            let doc = docvec!["call ", leaf::atom(module_name), ":'spawn'(", args_doc, ")"];
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

    /// Generates a binding-aware class method call (ADR 0019 Phase 3).
    ///
    /// In workspace mode, checks REPL bindings first for convenience names.
    /// If the name is found in bindings, it's an instance (e.g., Transcript is a
    /// `TranscriptStream` actor), so dispatch via `beamtalk_message_dispatch:send/3`.
    /// If not found, fall back to direct call or `class_send`.
    ///
    /// ```erlang
    /// case call 'maps':'find'('Name', State) of
    ///   <{'ok', BindingVal}> -> call 'beamtalk_message_dispatch':'send'(BindingVal, Sel, Args)
    ///   <'error'> -> call 'module':'class_selector'('nil', ~{}~, Args)  %% BT-1639 direct
    ///                %% OR: class_send fallback for non-eligible classes
    /// end
    /// ```
    fn generate_binding_aware_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // The binding branch dispatches to instances via
        // beamtalk_message_dispatch:send — use the raw selector (only hashed
        // if the selector itself exceeds the atom limit) so instance method
        // lookup works normally.  The class_send fallback uses the class-method
        // mangled selector which triggers earlier (when "class_" + selector
        // exceeds the limit).
        let raw = selector.name().to_string();
        let instance_selector = super::selector_mangler::safe_atom_name(&raw);
        let binding_val_var = self.fresh_var("BindingVal");
        let state_var = self.current_state_var();
        let lookup_var = self.fresh_temp_var("Lookup");

        // Preserve the "receiver first, then args" evaluation order
        // expected by Smalltalk/Beamtalk message-send semantics. The receiver
        // here is the class-binding lookup (`maps:find(ClassName, State)`),
        // which we bind to a temp BEFORE the arg preamble runs so a dispatch
        // whose class name is unresolved still evaluates the lookup first.
        // We then bind arguments to temp vars so they are evaluated exactly
        // once (fixing a pre-existing double-compilation of `args_doc` in both
        // `case` branches) and so open let-chains from class method self-sends
        // propagate to the surrounding scope.
        let (arg_prelude, arg_refs) = self.thread_args_bound(arguments, "BindArg")?;
        let args_doc = Self::join_docs_with_commas(arg_refs);

        // Build the class-side fallback: direct call or gen_server
        let class_fallback: Document<'static> =
            if let Some(module_name) = self.direct_call_eligible_module(class_name, &raw) {
                let safe_fn = super::selector_mangler::safe_class_method_fn_name(&raw);
                Self::direct_class_method_call_doc(
                    module_name,
                    safe_fn,
                    args_doc.clone(),
                    arguments.is_empty(),
                )
            } else {
                self.generate_class_send_fallback(class_name, &raw, args_doc.clone())
            };

        // ADR 0081 Phase 1: resolve the receiver — session locals first,
        // then lazy singleton resolution — BEFORE the arg preamble runs, so the
        // receiver is fully determined ahead of any argument side effects (the
        // "receiver first, then args" evaluation order). Singletons
        // (Transcript/Beamtalk/Workspace) are no longer eagerly injected into the
        // session map, so `Workspace bind:as:` would otherwise mis-route to a
        // non-existent `Workspace` class. resolve_singleton_instance/1 returns
        // `{ok, Instance}` for a singleton name and `error` for any other name,
        // so real class names (`Counter someClassMethod`) still fall through to
        // class_fallback.
        //
        // The combined lookup binds `Lookup` to `{ok, Receiver}` (from locals or
        // singleton registry) or `error` (use class-side fallback):
        //
        //   let Lookup = case maps:find(ClassName, State) of
        //                  {ok, V} -> {ok, V}
        //                  error   -> resolve_singleton_instance(ClassName)
        //                end
        //   in <arg_preamble>
        //   case Lookup of
        //     {ok, Receiver} -> beamtalk_message_dispatch:send(Receiver, Sel, Args)
        //     error          -> class_fallback
        //   end
        let singleton_val_var = self.fresh_var("SingletonVal");
        let lookup_binding = docvec![
            "let ",
            leaf::var(lookup_var.clone()),
            " = case call 'maps':'find'(",
            leaf::atom(class_name.to_string()),
            ", ",
            leaf::var(state_var),
            ") of ",
            "<{'ok', ",
            leaf::var(singleton_val_var.clone()),
            "}> when 'true' -> {'ok', ",
            leaf::var(singleton_val_var),
            "} ",
            "<'error'> when 'true' -> call 'beamtalk_workspace':'resolve_singleton_instance'(",
            leaf::atom(class_name.to_string()),
            ") ",
            "end in ",
        ];
        let case_doc = docvec![
            "case ",
            leaf::var(lookup_var),
            " of ",
            "<{'ok', ",
            leaf::var(binding_val_var.clone()),
            "}> when 'true' -> ",
            "call 'beamtalk_message_dispatch':'send'(",
            leaf::var(binding_val_var),
            ", ",
            leaf::atom(instance_selector),
            ", [",
            args_doc,
            "]) ",
            "<'error'> when 'true' -> ",
            class_fallback,
            " end"
        ];

        // ADR 0118 phase 5b: thread the lookup binding ahead of the arg
        // prelude (same order the pre-migration code built by hand), then
        // close — this function returns a bare `Document`, so any `ClassVars`
        // mutation an argument performed cannot stay visible beyond it.
        let mut prelude = vec![ThreadedStmt::Statement(
            lookup_binding,
            beamtalk_core::source_analysis::Span::default(),
        )];
        prelude.extend(arg_prelude);
        Ok(self.close_prelude(&prelude, case_doc, "BindClassRes"))
    }

    /// Generates workspace-mode class send for actor/value-type methods.
    ///
    /// For sealed classes eligible for direct call, generates a direct
    /// function call instead of `gen_server` dispatch. Otherwise tries `class_send`
    /// first (for real class names like `Counter`), returns nil for unresolved names.
    /// ADR 0019 Phase 4: No `persistent_term` fallback — convenience names resolve
    /// via session bindings in REPL context.
    fn generate_workspace_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let raw_selector = selector.name().to_string();

        // Direct call optimization for sealed class methods
        if let Some(module_name) = self.direct_call_eligible_module(class_name, &raw_selector) {
            return self.generate_direct_class_method_call(&module_name, &raw_selector, arguments);
        }

        // Hash long selector atoms to stay within Erlang's 255-char atom limit.
        let selector_atom = super::selector_mangler::safe_class_method_selector(&raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        let lookup_var = self.fresh_temp_var("WsLookup");
        // Bind the class registry lookup to a temp BEFORE evaluating
        // args, preserving "receiver first, then args" message-send semantics.
        // Then bind args to temp vars so they are evaluated once and their open
        // let-chains propagate upward.
        let (arg_prelude, arg_refs) = self.thread_args_bound(arguments, "WsArg")?;
        let args_doc = Self::join_docs_with_commas(arg_refs);

        let lookup_binding = docvec![
            "let ",
            leaf::var(lookup_var.clone()),
            " = call 'beamtalk_class_registry':'whereis_class'(",
            leaf::atom(class_name.to_string()),
            ") in ",
        ];
        let case_doc = docvec![
            "case ",
            leaf::var(lookup_var),
            " of ",
            "<'undefined'> when 'true' -> 'nil' ",
            "<",
            leaf::var(class_pid_var.clone()),
            "> when 'true' -> ",
            Self::class_send_call_doc(&class_pid_var, selector_atom, args_doc),
            " end"
        ];

        // ADR 0118 phase 5b: see the analogous binding-send helper above —
        // this function also returns a bare `Document`, so close.
        let mut prelude = vec![ThreadedStmt::Statement(
            lookup_binding,
            beamtalk_core::source_analysis::Span::default(),
        )];
        prelude.extend(arg_prelude);
        Ok(self.close_prelude(&prelude, case_doc, "WsClassRes"))
    }

    /// Generates a class-level method call.
    ///
    /// For sealed classes with no class variables, generates a direct
    /// function call to `module:class_<selector>(nil, #{}, Args...)`, bypassing
    /// the `gen_server` round-trip. This is safe because the methods are pure functions.
    ///
    /// For all other classes (or unrecognized selectors), falls back to the
    /// `gen_server` dispatch path via `beamtalk_object_class:class_send/3`.
    ///
    /// # Generated Code (direct call)
    ///
    /// ```erlang
    /// call 'bt@stdlib@tracing':'class_setContext:'('nil', ~{}~, Ctx)
    /// ```
    ///
    /// # Generated Code (`gen_server` fallback)
    ///
    /// ```erlang
    /// let ClassPid = call 'beamtalk_class_registry':'whereis_class'('Tracing') in
    /// call 'beamtalk_object_class':'class_send'(ClassPid, 'setContext:', [Ctx])
    /// ```
    fn generate_class_method_call(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let raw_selector = selector.name().to_string();

        // Check if this class method is eligible for direct call optimization.
        if let Some(module_name) = self.direct_call_eligible_module(class_name, &raw_selector) {
            return self.generate_direct_class_method_call(&module_name, &raw_selector, arguments);
        }

        // Fallback: gen_server dispatch via class_send
        // Hash long selector atoms (e.g. keyword constructors with many
        // fields) to stay within Erlang's 255-char atom limit.
        // Hoist preambles from sub-expression class var mutations.
        let selector_atom = super::selector_mangler::safe_class_method_selector(&raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        let (args_preamble, args_doc) = self.thread_args(arguments)?;

        let call_doc = docvec![
            "let ",
            leaf::var(class_pid_var.clone()),
            " = call 'beamtalk_class_registry':'whereis_class'(",
            leaf::atom(class_name.to_string()),
            ") in ",
            Self::class_send_call_doc(&class_pid_var, selector_atom, args_doc),
        ];

        Ok(self.close_prelude(&args_preamble, call_doc, "ClassCall"))
    }

    /// Generates a direct function call to a sealed class method.
    ///
    /// Passes `nil` for `ClassSelf` and `#{}` for `ClassVars` since sealed classes
    /// with no class variables never reference these parameters.
    ///
    /// ```erlang
    /// call 'module':'class_<selector>'('nil', #{}, Args...)
    /// ```
    fn generate_direct_class_method_call(
        &mut self,
        module_name: &str,
        selector: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Hash long selector atoms to stay within Erlang's 255-char atom limit.
        // Hoist preambles from sub-expression class var mutations.
        let safe_fn = super::selector_mangler::safe_class_method_fn_name(selector);
        let (args_preamble, args_doc) = self.thread_args(arguments)?;
        let call_doc = Self::direct_class_method_call_doc(
            module_name.to_string(),
            safe_fn,
            args_doc,
            arguments.is_empty(),
        );

        Ok(self.close_prelude(&args_preamble, call_doc, "DirectCall"))
    }

    /// Builds the `call Module:safe_fn('nil', ~{}~, Args...)` direct-call
    /// fragment shared by [`Self::generate_direct_class_method_call`] and
    /// `generate_binding_aware_class_send`'s already-threaded-args inline
    /// direct-call branch.
    fn direct_class_method_call_doc(
        module_name: String,
        safe_fn: String,
        args_doc: Document<'static>,
        args_empty: bool,
    ) -> Document<'static> {
        let comma = if args_empty { "" } else { ", " };
        // Core Erlang empty map is ~{}~ (not #{} which is Erlang source syntax)
        docvec![
            "call ",
            leaf::atom(module_name),
            ":",
            leaf::atom(safe_fn),
            "('nil', ~{}~",
            comma,
            args_doc,
            ")"
        ]
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
            " = call 'beamtalk_class_registry':'whereis_class'(",
            leaf::atom(class_name.to_string()),
            ") in ",
            Self::class_send_call_doc(&class_pid_var, class_selector, args_doc),
        ]
    }

    /// Builds the shared `call
    /// 'beamtalk_object_class':'class_send'(ClassPid, Selector, [Args])`
    /// fragment — the runtime `gen_server` dispatch every class-send fallback
    /// eventually reaches, once a live `ClassPid` is in hand.
    fn class_send_call_doc(
        class_pid_var: &str,
        selector_atom: String,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        docvec![
            "call 'beamtalk_object_class':'class_send'(",
            leaf::var(class_pid_var.to_string()),
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])"
        ]
    }

    /// Shared `direct_call_eligible` gate — returns the target module name
    /// when `class_name`/`raw_selector` is eligible for the sealed-class
    /// direct-call optimization (no class variables, no `gen_server`
    /// round-trip needed). Used by every direct-call gate check
    /// (`generate_binding_aware_class_send`, `generate_workspace_class_send`,
    /// `generate_class_method_call`) instead of each repeating the same
    /// `.get(class_name)` / `.selectors.contains(...)` lookup.
    fn direct_call_eligible_module(&self, class_name: &str, raw_selector: &str) -> Option<String> {
        self.direct_call_eligible.get(class_name).and_then(|info| {
            info.selectors
                .contains(raw_selector)
                .then(|| info.module_name.to_string())
        })
    }

    /// Pre-scans a class for self-sends that pass Tier 2 (stateful) block arguments.
    ///
    /// Walks all method bodies looking for `self <selector>: args` where an argument
    /// is a literal block with captured mutations (`captured_reads ∩ local_writes` non-empty).
    /// Records the target method selector and parameter position in `tier2_method_info`.
    pub(super) fn scan_class_for_tier2_blocks(
        &mut self,
        class: &beamtalk_core::ast::ClassDefinition,
    ) {
        use super::block_analysis::analyze_block;

        // Clear previous class's info to avoid cross-class pollution in multi-class modules
        self.tier2_method_info.clear();

        for method in &class.methods {
            for stmt in &method.body {
                self.scan_expr_for_tier2(&stmt.expression, &analyze_block);
            }
        }
    }

    /// Recursively scans an expression for Tier 2 block arguments in self-sends.
    fn scan_expr_for_tier2(
        &mut self,
        expr: &Expression,
        analyze: &dyn Fn(
            &beamtalk_core::ast::Block,
        ) -> super::block_analysis::BlockMutationAnalysis,
    ) {
        match expr {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                // Check for self-sends
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if id.name == "self" {
                        let sel_name = selector.name().to_string();
                        for (i, arg) in arguments.iter().enumerate() {
                            if let Expression::Block(block) = arg {
                                let analysis = analyze(block);
                                let has_captured_mutations = analysis
                                    .local_writes
                                    .intersection(&analysis.captured_reads)
                                    .next()
                                    .is_some();
                                // Also promote blocks with field writes to Tier 2.
                                let has_field_writes = !analysis.field_writes.is_empty();
                                if has_captured_mutations || has_field_writes {
                                    self.tier2_method_info
                                        .entry(sel_name.clone())
                                        .or_default()
                                        .push(i);
                                }
                            }
                        }
                    }
                }
                // Recurse into receiver and arguments
                self.scan_expr_for_tier2(receiver, analyze);
                for arg in arguments {
                    self.scan_expr_for_tier2(arg, analyze);
                }
            }
            Expression::Assignment { target, value, .. } => {
                self.scan_expr_for_tier2(target, analyze);
                self.scan_expr_for_tier2(value, analyze);
            }
            Expression::Block(block) => {
                for body_stmt in &block.body {
                    self.scan_expr_for_tier2(&body_stmt.expression, analyze);
                }
            }
            Expression::Return { value, .. } => {
                self.scan_expr_for_tier2(value, analyze);
            }
            Expression::Parenthesized { expression, .. } => {
                self.scan_expr_for_tier2(expression, analyze);
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // Detect cascaded self-sends as Tier 2 call sites
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if id.name == "self" {
                        for msg in messages {
                            let sel_name = msg.selector.name().to_string();
                            for (i, arg) in msg.arguments.iter().enumerate() {
                                if let Expression::Block(block) = arg {
                                    let analysis = analyze(block);
                                    let has_captured_mutations = analysis
                                        .local_writes
                                        .intersection(&analysis.captured_reads)
                                        .next()
                                        .is_some();
                                    // Also promote blocks with field writes to Tier 2.
                                    let has_field_writes = !analysis.field_writes.is_empty();
                                    if has_captured_mutations || has_field_writes {
                                        self.tier2_method_info
                                            .entry(sel_name.clone())
                                            .or_default()
                                            .push(i);
                                    }
                                }
                            }
                        }
                    }
                }
                // Recurse into receiver and arguments
                self.scan_expr_for_tier2(receiver, analyze);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.scan_expr_for_tier2(arg, analyze);
                    }
                }
            }
            Expression::Match { value, arms, .. } => {
                self.scan_expr_for_tier2(value, analyze);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.scan_expr_for_tier2(guard, analyze);
                    }
                    self.scan_expr_for_tier2(&arm.body, analyze);
                }
            }
            _ => {}
        }
    }

    /// Checks if an expression is a self-send with Tier 2 block arguments.
    ///
    /// Returns the captured-mutated variable names for each Tier 2 block argument
    /// if this is a Tier 2 self-send, or `None` if it's a regular self-send.
    ///
    /// Also promotes literal Tier 1 blocks at call sites where the target
    /// method is a known Tier 2 HOM (present in `tier2_method_info`). A promoted
    /// block is compiled with the Tier 2 signature (`fun(Args, StateAcc) -> {Result, StateAcc}`)
    /// even though it has no captured mutations, ensuring the callee's arity expectation is met.
    pub(super) fn detect_tier2_self_send(
        &self,
        expr: &Expression,
    ) -> Option<Vec<(usize, Vec<String>)>> {
        use super::block_analysis::analyze_block;

        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            if let Expression::Identifier(id) = receiver.as_ref() {
                if id.name == "self" {
                    let sel_name = selector.name().to_string();
                    // Collect positions the scanner identified as Tier 2 for this selector.
                    let hom_positions: std::collections::HashSet<usize> = self
                        .tier2_method_info
                        .get(&sel_name)
                        .map(|positions| positions.iter().copied().collect())
                        .unwrap_or_default();

                    let mut tier2_args = Vec::new();
                    for (i, arg) in arguments.iter().enumerate() {
                        if let Expression::Block(block) = arg {
                            let analysis = analyze_block(block);
                            let captured_mutations: Vec<String> = analysis
                                .local_writes
                                .intersection(&analysis.captured_reads)
                                .cloned()
                                .collect::<std::collections::BTreeSet<_>>()
                                .into_iter()
                                .collect();
                            if !captured_mutations.is_empty() {
                                tier2_args.push((i, captured_mutations));
                            } else if !analysis.field_writes.is_empty() {
                                // Field-write block — promote to Tier 2 with no local
                                // vars. The actor State IS the StateAcc; field reads/writes
                                // are threaded through it automatically inside the block body.
                                tier2_args.push((i, vec![]));
                            } else if hom_positions.contains(&i) {
                                // Block has no mutations but this position is a known
                                // Tier 2 HOM param. Promote to Tier 2 with empty captured vars
                                // so it gets `fun(Args, StateAcc) -> {Result, StateAcc}` signature
                                // (StateAcc passthrough), matching the callee's arity expectation.
                                tier2_args.push((i, vec![]));
                            }
                        } else if let Expression::Identifier(arg_id) = arg {
                            // If the argument is an identifier that is a known Tier 2
                            // block parameter of the current method, treat it as a Tier 2 HOM
                            // argument with no captured mutations. This handles nested HOMs where
                            // one method delegates a Tier 2 block to another (e.g.
                            // `outerEachItem: aBlock => self eachItem: aBlock`). The block was
                            // already compiled as Tier 2 by the outer caller; we need to ensure
                            // the returned state is threaded back through the delegation chain.
                            if self.tier2_block_params.contains(arg_id.name.as_str()) {
                                tier2_args.push((i, vec![]));
                            }
                        }
                    }
                    if !tier2_args.is_empty() {
                        return Some(tier2_args);
                    }
                }
            }
        }
        None
    }

    /// Generates a self-dispatch with Tier 2 block arguments and state threading.
    ///
    /// Before the self-send:
    /// 1. Packs captured-mutated locals into State
    /// 2. Generates block arguments with Tier 2 stateful signature
    ///
    /// After the self-send:
    /// 1. Extracts captured-mutated locals from the returned State
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let State1 = call 'maps':'put'('__local__count', Count, State) in
    /// let _SD0 = case call 'module':'safe_dispatch'('applyBlock:to:',
    ///     [fun (X, StateAcc) -> ... {Result, StateAcc1} end, 5], State1) of
    ///   <{'reply', R, S}> when 'true' -> {R, S}
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end in let State2 = call 'erlang':'element'(2, _SD0) in
    /// let Count = call 'maps':'get'('__local__count', State2) in
    /// ```
    pub(super) fn generate_tier2_self_send_open(
        &mut self,
        expr: &Expression,
        tier2_args: &[(usize, Vec<String>)],
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            let mut docs: Vec<Document<'static>> = Vec::new();

            // Step 1: Pack captured-mutated locals into State
            for (_pos, captured_vars) in tier2_args {
                for var_name in captured_vars {
                    let core_var = self
                        .lookup_var(var_name)
                        .cloned()
                        .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                    let key = Self::local_state_key(var_name);
                    let current_state = self.current_state_var();
                    let new_state = self.next_state_var();
                    docs.push(docvec![
                        "let ",
                        leaf::var(new_state),
                        " = call 'maps':'put'(",
                        leaf::atom(key),
                        ", ",
                        leaf::var(core_var),
                        ", ",
                        leaf::var(current_state),
                        ") in "
                    ]);
                }
            }

            // Step 2: Generate argument list with Tier 2 blocks
            let selector_atom = selector.name().to_string();
            let dispatch_var = self.fresh_temp_var("SD");
            let result_var = self.fresh_var("SDResult");
            let state_var = self.fresh_var("SDState");
            let current_state = self.current_state_var();
            let module = self.module_name.clone();
            let args_doc = self.generate_tier2_args(arguments, tier2_args)?;

            // Step 3: Generate the self-dispatch (using safe_dispatch or sealed path)
            let call_doc = self.generate_tier2_dispatch_call(
                selector,
                arguments.is_empty(),
                &selector_atom,
                &dispatch_var,
                &current_state,
                &module,
                args_doc,
            );

            // Result/error clauses + state extraction
            let new_state = self.next_state_var();
            let error_clause = self.generate_self_dispatch_error_clause("SDError", &selector_atom);
            docs.push(docvec![
                call_doc,
                "<{'reply', ",
                leaf::var(result_var.clone()),
                ", ",
                leaf::var(state_var.clone()),
                "}> when 'true' -> {",
                leaf::var(result_var),
                ", ",
                leaf::var(state_var),
                "} ",
                error_clause,
                "end in let ",
                leaf::var(new_state),
                " = call 'erlang':'element'(2, ",
                leaf::var(dispatch_var.clone()),
                ") in "
            ]);

            // Step 4: Extract captured-mutated locals from the returned State
            let final_state = self.current_state_var();
            for (_pos, captured_vars) in tier2_args {
                for var_name in captured_vars {
                    let core_var = self
                        .lookup_var(var_name)
                        .cloned()
                        .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                    let key = Self::local_state_key(var_name);
                    docs.push(docvec![
                        "let ",
                        leaf::var(core_var),
                        " = call 'maps':'get'(",
                        leaf::atom(key),
                        ", ",
                        leaf::var(final_state.clone()),
                        ") in "
                    ]);
                }
            }

            return Ok((Document::Vec(docs), dispatch_var));
        }
        Err(CodeGenError::Internal(
            "generate_tier2_self_send_open called on non-MessageSend expression".to_string(),
        ))
    }

    /// Builds argument list for a Tier 2 self-send, using stateful block
    /// generation for marked positions.
    fn generate_tier2_args(
        &mut self,
        arguments: &[Expression],
        tier2_args: &[(usize, Vec<String>)],
    ) -> Result<Document<'static>> {
        let tier2_positions: std::collections::HashSet<usize> =
            tier2_args.iter().map(|(pos, _)| *pos).collect();
        let tier2_vars_by_pos: std::collections::HashMap<usize, &Vec<String>> =
            tier2_args.iter().map(|(pos, vars)| (*pos, vars)).collect();

        let mut arg_parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                arg_parts.push(Document::Str(", "));
            }
            if tier2_positions.contains(&i) {
                if let Expression::Block(block) = arg {
                    let captured_vars = tier2_vars_by_pos[&i];
                    arg_parts.push(self.generate_block_stateful(block, captured_vars)?);
                } else {
                    arg_parts.push(self.expression_doc(arg)?);
                }
            } else {
                arg_parts.push(self.expression_doc(arg)?);
            }
        }
        Ok(Document::Vec(arg_parts))
    }

    /// Generates the dispatch call for a Tier 2 self-send.
    ///
    /// Handles sealed (direct/dispatch) and non-sealed (`safe_dispatch`) paths.
    #[allow(clippy::too_many_arguments)]
    fn generate_tier2_dispatch_call(
        &mut self,
        selector: &MessageSelector,
        no_args: bool,
        selector_atom: &str,
        dispatch_var: &str,
        current_state: &str,
        module: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        if self.is_class_sealed() {
            let selector_name = selector.name().to_string();
            // The `make_self`/`let DispatchVar = case call Module:...`
            // scaffolding is identical whichever sealed callee is chosen below —
            // only the callee expression (`callee_doc`) differs.
            let self_var = self.fresh_temp_var("SealedSelf");
            let callee_doc = if self.sealed_method_selectors().contains(&selector_name) {
                let comma = if no_args { "" } else { ", " };
                docvec![
                    leaf::atom(module.to_string()),
                    ":",
                    leaf::atom(super::selector_mangler::sealed_fn_name(&selector_name)),
                    "(",
                    args_doc,
                    comma,
                    leaf::var(self_var.clone()),
                    ", ",
                    leaf::var(current_state.to_string()),
                    ")"
                ]
            } else {
                docvec![
                    leaf::atom(module.to_string()),
                    ":'dispatch'(",
                    leaf::atom(selector_atom.to_string()),
                    ", [",
                    args_doc,
                    "], ",
                    leaf::var(self_var.clone()),
                    ", ",
                    leaf::var(current_state.to_string()),
                    ")"
                ]
            };
            docvec![
                "let ",
                leaf::var(self_var),
                " = call 'beamtalk_actor':'make_self'(",
                leaf::var(current_state.to_string()),
                ") in let ",
                leaf::var(dispatch_var.to_string()),
                " = case call ",
                callee_doc,
                " of "
            ]
        } else {
            docvec![
                "let ",
                leaf::var(dispatch_var.to_string()),
                " = case ",
                Self::safe_dispatch_call_doc(
                    module.to_string(),
                    selector_atom.to_string(),
                    args_doc,
                    current_state.to_string()
                ),
                " of "
            ]
        }
    }

    /// builds the shared `call Module:'safe_dispatch'(Selector,
    /// [Args], State)` fragment used by every non-sealed self-dispatch call
    /// site (self-cast, discarding self-dispatch, open self-dispatch, and the
    /// Tier 2 dispatch call above).
    fn safe_dispatch_call_doc(
        module: impl Into<String>,
        selector_atom: impl Into<String>,
        args_doc: Document<'static>,
        state_var: String,
    ) -> Document<'static> {
        docvec![
            "call ",
            leaf::atom(module),
            ":'safe_dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            leaf::var(state_var),
            ")"
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
/// `superclass` moved off this path — its raw export returns the
/// bare class-name atom, not the `#beamtalk_object{}` a Beamtalk-level
/// `self superclass` must produce, and callers comparing it against a real
/// class object got a silent `equals:`/`==` mismatch. It is now handled by
/// `class_self_send_reflective_primitive`, which calls the real
/// `beamtalk_behaviour_intrinsics:classSuperclass/1` intrinsic. `class_name`
/// stays here: its intrinsic (`className/1`) always calls
/// `gen_server:call(ClassPid, class_name)`, which would deadlock from inside
/// the class's own process, so returning the raw atom directly is the
/// correct (deadlock-safe) behavior for this one, not a bug.
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
/// is reachable via plain self-send needs to be added here (if its raw
/// export form is the desired behavior) or to
/// `class_self_send_reflective_primitive` (if it must produce the same
/// value the non-self-send Behaviour-protocol dispatch would). Unknown
/// selectors take the inherited-dispatch path, which raises structured DNU
/// on miss — strictly better than the old fallthrough (direct call
/// → runtime `undef`).
pub(super) fn is_class_auto_export_selector(selector_atom: &str, arity: usize) -> bool {
    arity == 0 && selector_atom == "class_name"
}

/// Behaviour-protocol reflective primitives that are safe to
/// dispatch directly from a class-method self-send.
///
/// Unlike `is_class_auto_export_selector`'s raw module exports, these
/// selectors are ordinary `@primitive`-backed methods inherited from
/// `Behaviour`/`Class` (see `stdlib/src/behaviour.bt`) that non-self-send
/// dispatch resolves via `try_class_chain_fallthrough`'s
/// `beamtalk_dispatch:lookup/5` walk — a walk that itself is not reachable
/// from inside the class's own process (it round-trips through
/// `gen_server:call` for the class method table). Each selector listed here
/// is deadlock-safe to call directly with `ClassSelf` because its
/// `beamtalk_behaviour_intrinsics` implementation resolves everything it
/// needs (module, metadata) through `beamtalk_object_class:module_name_safe/1`
/// (which has a `ClassPid =:= self()` fast path) and
/// `__beamtalk_meta/0`/ETS lookups rather than an unconditional
/// `gen_server:call(ClassPid, ...)`.
///
/// Deliberately does NOT include every Behaviour-protocol selector:
/// `classSubclasses/1` and `classAllSubclasses/1` unconditionally call
/// `gen_server:call(ClassPid, class_name)` with no metadata-first fast
/// path, so adding `subclasses`/`allSubclasses` here today would trade a
/// `does_not_understand` for a real self-call deadlock. Extending this list
/// requires auditing the target intrinsic for that property first.
///
/// Note the metadata-first fast path itself still has a `not_available`
/// fallback in `classSuperclass/1`/`classIncludesSelector/2` that calls
/// `gen_server:call(ClassPid, ...)` — safe here only because every class
/// module this codegen emits unconditionally exports `__beamtalk_meta/0`
/// (see `native_facade.rs` / the `gen_server` codegen), so `meta_for_module/1`
/// never actually falls through to it for a compiled class. If that
/// invariant ever changes, this function's safety claim needs re-auditing.
pub(super) fn class_self_send_reflective_primitive(
    selector_atom: &str,
    arity: usize,
) -> Option<&'static str> {
    match (selector_atom, arity) {
        ("superclass", 0) => Some("classSuperclass"),
        ("includesSelector:", 1) => Some("classIncludesSelector"),
        _ => None,
    }
}

#[cfg(test)]
mod tests;
