// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Core Erlang code generation for Beamtalk.
//!
//! This module transforms Beamtalk AST into Core Erlang, which is then
//! compiled to BEAM bytecode by `erlc`. The generated code follows the
//! actor runtime model using OTP's `gen_server` behaviour.
//!
//! # Architecture
//!
//! Each Beamtalk module becomes an Erlang module implementing `gen_server`:
//!
//! - **Actor State**: A map containing `$beamtalk_class`, `__methods__`, and actor fields
//! - **Message Dispatch**: Messages route through `handle_cast` or `handle_call`
//! - **Hot Code Reload**: The `code_change/3` callback handles state migration
//!
//! # Example
//!
//! Beamtalk source:
//! ```beamtalk
//! value := 0.
//! increment := [ self.value := self.value + 1. ^self.value ].
//! ```
//!
//! Generated Core Erlang:
//! ```erlang
//! module 'counter' ['init'/1, 'handle_cast'/2, 'handle_call'/3,
//!                   'code_change'/3, 'dispatch'/3, 'method_table'/0, 'spawn'/0]
//!   attributes ['behaviour' = ['gen_server']]
//!
//! 'init'/1 = fun (_Args) ->
//!     let InitialState = ~{
//!       '$beamtalk_class' => 'Counter',
//!       '__methods__' => call 'counter':'method_table'(),
//!       'value' => 0
//!     }~
//!     in {'ok', InitialState}
//!
//! 'handle_cast'/2 = fun (Msg, State) ->
//!     case Msg of
//!       <{Selector, Args, FuturePid}> when 'true' ->
//!         case call 'counter':'dispatch'(Selector, Args, State) of
//!           <{'reply', Result, NewState}> when 'true' ->
//!             let _ = call 'erlang':'!'(FuturePid, {'resolve', Result})
//!             in {'noreply', NewState}
//!         end
//!     end
//! ```
//!
//! # Core Erlang Syntax
//!
//! Core Erlang is a simplified functional IR for Erlang:
//!
//! - **Atoms**: `'atom_name'` (always quoted)
//! - **Variables**: `VariableName` (starts with uppercase)
//! - **Function calls**: `call 'module':'function'(args)`
//! - **Let bindings**: `let Var = Expr in Body`
//! - **Case expressions**: `case Expr of Pattern -> Body end`
//! - **Maps**: `~{'key' => value}~`
//! - **Tuples**: `{'tuple', 'elements'}`
//! - **Lists**: `[1, 2, 3]` or `[Head | Tail]`
//!
//! # Module Organization (Domain-Driven Design)
//!
//! The code generator is organized around **bounded contexts** following DDD:
//!
//! ## Core Domain Modules
//!
//! - [`control_flow`] - Control flow compilation (iteration, loops, mutation analysis)
//! - [`dispatch_codegen`] - Message sending and dispatch (the core Beamtalk operation)
//! - [`dispatch_spec`] - `DispatchSpec`, the shared `has_method/1` emitter
//!   for both actor (`gen_server`) and value-type classes (ADR 0006)
//! - [`variable_context`] - Variable binding and scope management aggregate
//! - [`threaded_ir`] - `VersionCounter`, the single implementation behind
//!   the state/class-var/self-type-threaded version counters (formerly `state_codegen`)
//!
//! ## Supporting Modules
//!
//! - [`expressions`] - Expression code generation (literals, identifiers, maps, cascades)
//! - [`blocks`] - block (closure) compilation — Tier 1/Tier 2 codegen,
//!   the Erlang-interop wrapper, and block-body statement sequencing
//! - [`patterns`] - `match:` compilation, native `Pattern` lowering,
//!   `Pattern::Type` runtime-test strategies, and destructuring extraction
//! - [`gen_server`] - OTP `gen_server` scaffolding (spawn, init, callbacks)
//! - [`intrinsics`] - Compiler intrinsics (block evaluation, `ProtoObject`, `Object`)
//! - [`operators`] - Binary operator compilation (arithmetic, comparison, string concat)
//! - [`block_analysis`] - Block mutation analysis for control flow
//! - [`sequencing`] - sub-expression sequencing primitives shared across
//!   dispatch, operator, and expression codegen (ADR 0118)
//! - [`expr_shape`] - expression-shape predicates (`is_field_assignment`,
//!   `is_actor_self_send`, …) shared across the whole crate
//! - [`util`] - Utility functions (indentation, name conversions)
//!
//! ## Structural Split
//!
//! `mod.rs` shrinks to re-exports plus [`CoreErlangGenerator::generate_expression`]
//! and its tightly-coupled `ClassReference` helpers. The rest is organized by kind:
//!
//! - [`error`] - [`CodeGenError`] and the [`Result`] alias
//! - [`options`] - [`CodegenOptions`] and [`GeneratedModule`]
//! - [`driver`] - [`generate_module`], [`generate_module_with_warnings`], [`generate`]
//! - [`nlr`] - non-local-return try/catch scaffolding (`wrap_*_with_nlr_catch`)
//! - [`threading_analysis`] - control-flow threaded-variable analysis predicates
//! - [`generator`] - the [`CoreErlangGenerator`] struct, its context structs,
//!   [`generator::branch_guard::BranchContextGuard`], accessors, and version helpers
//! - [`primitives::intrinsic_bodies`] - `@primitive`/Logger `@intrinsic` body lowering
//!
//! # References
//!
//! - [Core Erlang Specification](https://www.it.uu.se/research/group/hipe/cerl/)
//! - [Gleam Erlang Codegen](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/erlang.rs)

mod actor_codegen;
mod actor_state_access;
mod binary_construction;
mod binary_segments;
mod block_analysis;
mod blocks;
mod class_builder_source;
mod class_meta;
mod class_registry;
mod class_var_access;
pub mod class_var_keys;
mod control_flow;
mod dispatch_codegen;
mod dispatch_spec;
mod driver;
#[cfg(any(test, feature = "test"))]
pub mod dropped_assignment_probe;
mod dynamic_site_report;
pub mod ensure_nlr_relay;
pub mod erlang_types;
mod error;
mod errors;
mod expr_shape;
mod expressions;
mod field_types;
mod gen_server;
pub(in crate::core_erlang) mod generator;
mod guard_returns;
mod intrinsics;
mod method_frame;
pub mod module_shell;
mod nlr;
mod operand_order_check;
mod operators;
mod options;
mod patterns;
pub mod primitive_bindings;
mod primitives;
pub mod repl_nlr;
mod routing;
#[cfg(test)]
mod runtime_export_contract_tests;
pub mod selector_mangler;
mod sequencing;
mod spec_codegen;
mod supervisor_codegen;
mod synced_call;
mod threaded_expr;
mod threaded_ir;
mod threading_analysis;
mod util;
mod value_accessors;
mod value_type_codegen;
mod variable_context;
mod xref;

// Re-export utility functions for IDE queries
pub use beamtalk_cerl_doc::escape::{escape_atom_chars, escape_erlang_string};
pub use util::to_module_name;

// Re-exports preserving every `core_erlang::X` / `super::X` path these
// items had before moving into sibling modules — used throughout this crate
// and, for the fully `pub` items, by `beamtalk-repl`/`beamtalk-cli`/
// `beamtalk-compiler-port` — so no call site elsewhere needs to change.
pub use driver::{
    generate, generate_module, generate_module_surfacing_verifier, generate_module_with_warnings,
};
pub use error::CodeGenError;
pub use error::Result;
pub(in crate::core_erlang) use generator::DirectCallClassInfo;
pub(in crate::core_erlang) use generator::version::render_state_prefix;
pub use generator::{CodeGenContext, CoreErlangGenerator};
pub(in crate::core_erlang) use nlr::NlrBoundary;
pub use options::{CodegenOptions, GeneratedModule};
pub use threaded_expr::ReplProducerDoc;

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::Expression;

impl CoreErlangGenerator {
    ///
    /// Generates code for an expression by dispatching to the appropriate handler.
    ///
    /// This is the main expression dispatcher that routes each AST node type
    /// to its specialized code generation method.
    ///
    /// ADR 0018 Phase 3: Returns `Document<'static>` directly for composable
    /// code generation without string buffer intermediaries.
    pub(in crate::core_erlang) fn generate_expression(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        // BT-4366: the innermost expression being lowered names the line of
        // the call nodes it nests inside a `let`/`case` (see
        // `current_send_span`); a `MessageSend` sets its own below.
        let outer_span = self.current_send_span.replace(expr.span());
        let result = self.generate_expression_inner(expr);
        self.current_send_span = outer_span;
        result
    }

    #[allow(clippy::too_many_lines)]
    fn generate_expression_inner(&mut self, expr: &Expression) -> Result<Document<'static>> {
        // ADR 0118 phase 1a: a sub-expression the sequencing rule
        // already compiled ahead of this parent substitutes its value here
        // — see `precompiled_subexprs`.
        if let Some(doc) = self.take_precompiled_subexpr(expr) {
            return Ok(doc);
        }
        match expr {
            Expression::Literal(lit, _) => Ok(Self::literal_doc(lit)),
            Expression::Identifier(id) => self.generate_identifier(id),
            // BT-4074: a package qualifier (`pkg@Foo`) selects nothing at
            // runtime — the class process is registered under the bare name —
            // so a qualified reference yields the same canonical class object
            // as `Foo`.
            Expression::ClassReference { name, .. } => self.generate_class_reference(&name.name),
            Expression::Super(_) => {
                // Super by itself is not a valid expression - it must be used
                // as a message receiver (e.g., `super increment`). Semantic
                // analysis rejects every shape that reaches here (`super`
                // outside a method, a cascade on `super` (BT-4251), and
                // `super` in any position but a send's receiver (BT-4258)),
                // so this is an internal-error backstop.
                Err(CodeGenError::UnsupportedFeature {
                    feature: "'super' must be used with a message send".to_string(),
                    span: Some(expr.span()),
                })
            }
            Expression::Block(block) => self.generate_block(block),
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                span,
                is_cast,
                ..
            } => {
                // ADR 0131 §5 (Phase 4): `Result tryDo: [...]` writing a REPL
                // binding compiles to its `{Value, Bindings}` construct tuple.
                let repl_try_do = if *is_cast {
                    None
                } else {
                    self.repl_try_do_tier2_send(expr)?
                };
                let doc = if let Some(doc) = repl_try_do {
                    doc
                } else if *is_cast {
                    self.generate_cast_send(receiver, selector, arguments)?
                } else {
                    self.generate_message_send(receiver, selector, arguments)?
                };
                Ok(self.annotate_send_with_line(doc, *span))
            }
            Expression::Assignment { target, value, .. } => {
                // Tier 2 only ever supported captured-local mutations, not
                // field writes — a stored block with `self.field :=` is rejected by
                // generate_block()'s validate_stored_closure call, not silently
                // accepted. No validation needed *here* only because that check
                // already lives inside generate_block() itself.

                // Check if this is a field assignment (self.field := value)
                if let Expression::FieldAccess {
                    receiver, field, ..
                } = target.as_ref()
                {
                    // Verify the receiver is 'self'
                    if let Expression::Identifier(recv_id) = receiver.as_ref() {
                        if recv_id.name == "self" {
                            // Field assignment: self.field := value
                            // Generate state-threaded update:
                            // let _Val = <value> in let State{n} = maps:put('field', _Val, State{n-1}) in _Val
                            return self.generate_field_assignment(&field.name, value);
                        }
                    }
                    // Field assignment to non-self receiver (e.g., other.field := value)
                    // This is not supported in the current implementation - actors can
                    // only mutate their own state, not the state of other objects.
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "field assignment to non-self receiver".to_string(),
                        span: Some(target.span()),
                    });
                }
                // An identifier assignment reaches here only as a statement the
                // enclosing body lowering binds itself (the REPL's bindings,
                // a block body's `let`), where only its value is compiled. A
                // value-position one is a producer (BT-3910): it is lowered by
                // `threaded_expression` as a `LocalRebind` of the enclosing
                // frame, so reaching here with one drops the write — reported,
                // never silent (ADR 0131 §4, `UnthreadedLocalAssignment`).
                if let Expression::Identifier(id) = target.as_ref() {
                    if self.value_assign_sites.contains(&expr.span()) {
                        let errors = [threaded_ir::VerifyError::UnthreadedLocalAssignment {
                            local: id.name.to_string(),
                            span: expr.span(),
                        }];
                        self.report_threaded_ir_verify_errors(
                            &errors,
                            "a value-position local assignment must be lowered as a producer",
                            expr.span(),
                        );
                    }
                }
                #[cfg(any(test, feature = "test"))]
                dropped_assignment_probe::record(expr.span());
                self.generate_expression(value)
            }
            Expression::Return { value, span, .. } => {
                // If inside a block with NLR infrastructure active, generate a throw
                // so the return escapes from the block closure back to the enclosing method.
                // Otherwise (at method body level, or no NLR), just emit the value.
                // BT-4288 / BT-4312: a REPL `^` run in place throws to the
                // eval's boundary (`repl_nlr`); a closure's `^` and one that
                // is the input's whole last statement have none.
                let nlr_token = match self.current_nlr_token().cloned() {
                    Some(token) => Some(token),
                    None => self.repl_return_token(*span),
                };
                if let Some(nlr_token) = nlr_token {
                    // Emit diagnostic for NLR throw/catch generation.
                    self.emit_codegen_diagnostic(
                        {
                            let line_info = self
                                .span_to_line(*span)
                                .map_or(String::new(), |l| format!(" at line {l}"));
                            format!(
                                "Non-local return{line_info}: compiled via throw/catch, \
                                 may inhibit JIT optimization"
                            )
                        },
                        *span,
                    );
                    // Generalized by ADR 0118 phase 1b
                    // and phase 5b: `value` may itself dispatch a
                    // self-send that threads new state (Actor `State`) —
                    // nested anywhere inside it
                    // (`^ self.items at: (self bump)`), not just at its own
                    // top level. `state` below must be computed AFTER
                    // `value` threads, so it reflects whatever `Bind` that
                    // dispatch just produced — `threaded_expression` is the
                    // one mechanism that threads it correctly for whichever
                    // context this `^` runs in.
                    //
                    // ADR 0118 phase 2a: when THIS `Return` node
                    // is itself the sole child `threaded_expression`'s own
                    // `single_sequenced_child` branch is sequencing (e.g.
                    // `thread_ahead`'s C12-catch-all reaching `^self.items
                    // at: (self bump)` nested in a conditional branch —
                    // `single_sequenced_child` already ran
                    // `sequence_children` on `value` and is about to compile
                    // THIS WHOLE node via `generate_expression`), `value`'s
                    // span already carries a live `precompiled_subexprs`
                    // registration from that outer call. Re-threading it
                    // here via `threaded_expression` would dispatch any
                    // nested self-send a SECOND time and leave the outer
                    // registration's `finish_precompiled_scope` check
                    // failing with "never substituted" (confirmed by a
                    // `just test-bunit` failure on this exact shape during
                    // this migration) — so the "already sequenced" case
                    // instead just reads that registration back via the
                    // ordinary `expression_doc` (`generate_expression`'s
                    // `take_precompiled_subexpr` entry), with no prelude of
                    // its own to add here (it already ran in the outer
                    // frame).
                    let value_already_sequenced = self
                        .precompiled_subexprs
                        .contains_key(&value.unwrap_parens().span());
                    let (val_preamble, value_doc) = if value_already_sequenced {
                        (Document::Nil, self.expression_doc(value)?)
                    } else if let Some(tv) = self.nlr_return_construct_value(value)? {
                        // BT-3813: a loop or fold `^` value in a class or
                        // value-type method answers its construct tuple; the
                        // throw carries its value (element 1), never the
                        // carrier, wherever the `^` sits.
                        let prelude_doc = self.verified_root_prelude_doc(&tv.prelude, *span);
                        (prelude_doc, self.threaded_value_doc(&tv.value))
                    } else if self.threads_no_actor_state() {
                        // A class method threads nothing (ADR 0130 §3): the
                        // value is compiled as an ordinary expression, with
                        // no prelude of its own.
                        (Document::Nil, self.expression_doc(value)?)
                    } else {
                        // ADR 0118 phase 2a: `current_frame()` —
                        // this generic `Return` handler fires for a `^`
                        // reached from any nesting, not only the flat
                        // method body, now that branch/exception/
                        // stateful-block arms have their own `threaded_expression`
                        // consumers too.
                        let frame = self.current_frame();
                        let tv = self.threaded_expression(value, frame)?;
                        let dispatch_doc = self.threaded_prelude_doc(&tv.prelude);
                        let result_doc = self.threaded_value_doc(&tv.value);
                        (dispatch_doc, result_doc)
                    };
                    // All NLR throws carry state as a 4-tuple (`nlr_throw_state_doc`).
                    let state_doc = self.nlr_throw_state_doc(*span);
                    let throw_doc = docvec![
                        "call 'erlang':'throw'({'$bt_nlr', ",
                        leaf::var(nlr_token),
                        ", ",
                        value_doc,
                        ", ",
                        state_doc.clone(),
                        "})"
                    ];
                    // ADR 0137 §5: a `^` stores the current version to the cell
                    // before the throw, so a closure that runs while it unwinds (an
                    // `ensure:` cleanup) sees every write made before it.
                    // BT-4122: a merged Tier 2 fun stores its merged map too
                    // (it holds no versions, so the active check misses it).
                    let throw_doc = if self.nlr_throw_stores_cell() {
                        docvec![
                            "let _ = ",
                            threaded_ir::cell_put_call(state_doc),
                            " in ",
                            throw_doc
                        ]
                    } else {
                        throw_doc
                    };
                    Ok(docvec![val_preamble, throw_doc])
                } else {
                    // Return in Core Erlang is just the value
                    self.generate_expression(value)
                }
            }
            Expression::FieldAccess {
                receiver, field, ..
            } => self.generate_field_access(receiver, field),
            Expression::Parenthesized { expression, .. } => self.generate_expression(expression),
            Expression::MapLiteral { pairs, .. } => self.generate_map_literal(pairs),
            Expression::ListLiteral { elements, tail, .. } => {
                self.generate_list_literal(elements, tail.as_deref())
            }
            Expression::ArrayLiteral { elements, .. } => self.generate_array_literal(elements),
            Expression::TupleLiteral { elements, .. } => self.generate_tuple_literal(elements),
            Expression::Cascade {
                receiver, messages, ..
            } => self.generate_cascade(receiver, messages),
            Expression::Primitive {
                name,
                is_quoted,
                span,
                ..
            } => self.generate_primitive(name, *is_quoted, *span),
            Expression::Match { value, arms, .. } => self.generate_match(value, arms),
            Expression::StringInterpolation { segments, .. } => {
                self.generate_string_interpolation(segments)
            }
            // ADR 0143: `<<...>>` in expression position builds a binary.
            Expression::BinaryConstruction { segments, .. } => {
                self.generate_binary_construction(segments)
            }
            Expression::DestructureAssignment { span, .. } => {
                // DestructureAssignment is only valid as a statement in a body context.
                // All statement-body generators handle it explicitly. Reaching here means
                // it appeared in a pure expression position, which is not supported.
                Err(CodeGenError::UnsupportedFeature {
                    feature: "destructuring assignment in expression position".to_string(),
                    span: Some(*span),
                })
            }
            Expression::Error { message, span, .. } => Err(CodeGenError::UnsupportedFeature {
                feature: format!("expression error: {message}"),
                span: Some(*span),
            }),
            Expression::ExpectDirective { .. } => Ok(Document::Nil),
            Expression::Spread { name, .. } => Err(CodeGenError::UnsupportedFeature {
                feature: format!("spread expression: {}", name.name),
                span: Some(name.span),
            }),
        }
    }

    /// Generates code for a standalone `ClassReference`.
    ///
    /// ADR 0019 Phase 3: In workspace mode, checks REPL bindings first for
    /// convenience names (Transcript, Beamtalk, Workspace), then falls back
    /// to class registry lookup. In batch mode, goes directly to the registry.
    ///
    /// A package-qualified reference (`pkg@Foo`, ADR 0070) compiles exactly
    /// like `Foo`: the registry lookup is by bare name, and the class object
    /// carries the canonical `'Foo class'` tag so it compares equal to every
    /// runtime-built class object for the same class (BT-4074). Package-aware
    /// registry disambiguation is a future phase.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_class_reference(
        &mut self,
        class_name: &str,
    ) -> Result<Document<'static>> {
        // ADR 0129 §7: REPL top-level expressions resolve a name through the
        // session map (locals and `bind:as:` entries) before the class registry.
        // Methods and batch-compiled code use the registry only, so a missing
        // class raises `class_not_found` identically everywhere (§10).
        if self.context == CodeGenContext::Repl {
            let class_pid_var = self.fresh_var("ClassPid");
            let class_mod_var = self.fresh_var("ClassModName");
            let state_var = self.current_state_var();
            let error_doc = self.class_not_found_error_doc(class_name);

            return Ok(docvec![
                "case call 'maps':'find'(",
                leaf::atom(class_name.to_string()),
                ", ",
                leaf::var(state_var),
                ") of ",
                "<{'ok', _BindingVal}> when 'true' -> _BindingVal ",
                "<'error'> when 'true' -> ",
                "case ",
                Self::registered_class_pid_doc(class_name),
                " of ",
                error_doc,
                Self::class_object_from_registry_clause(&class_pid_var, &class_mod_var, class_name),
                "end end",
            ]);
        }

        let class_pid_var = self.fresh_var("ClassPid");
        let object_clause = if let Some(module) = self.pinned_builtin_class_module(class_name) {
            Self::class_object_with_module_clause(&class_pid_var, module, class_name)
        } else {
            let class_mod_var = self.fresh_var("ClassModName");
            Self::class_object_from_registry_clause(&class_pid_var, &class_mod_var, class_name)
        };
        let error_doc = self.class_not_found_error_doc(class_name);

        Ok(docvec![
            "case ",
            Self::registered_class_pid_doc(class_name),
            " of ",
            error_doc,
            object_clause,
            "end",
        ])
    }

    /// BT-4411: the compile-time module of a built-in class literal, when its
    /// routing-fixture row pins one ([`routing::pinned_class_module`]) and that
    /// module is the class's own compiled module (the one
    /// `module_name_safe/1` would read at run time). `None` for every other
    /// class, and for a name declared by more than one package
    /// ([`Self::is_shadowed_class`]): those keep the run-time module lookup.
    fn pinned_builtin_class_module(&self, class_name: &str) -> Option<&'static str> {
        if self.is_shadowed_class(class_name) {
            return None;
        }
        let module = routing::pinned_class_module(class_name)?;
        (self.compiled_module_name(class_name) == module).then_some(module)
    }

    /// `call 'erlang':'whereis'('beamtalk_class_<Name>')`: the pid of the class
    /// process named `class_name`, or `'undefined'`.
    ///
    /// BT-4295: the registered name is built at compile time
    /// ([`util::class_registered_name`], pinned to
    /// `beamtalk_class_registry:registry_name/1` by the shared conformance
    /// fixture), so a class reference costs one `erlang:whereis/1` instead of
    /// `whereis_class/1`'s per-use `binary_to_atom`. Only the *name* is
    /// compile-time: the pid is looked up on every use, never baked, so a
    /// restarted class process is found (ADR 0025 amendment §3). The module
    /// stays a run-time lookup for user classes, so a REPL redefinition into a
    /// new module is seen; a stdlib built-in whose routing-fixture row pins its
    /// own module bakes it (BT-4411), since a stdlib class cannot be redefined.
    pub(in crate::core_erlang) fn registered_class_pid_doc(class_name: &str) -> Document<'static> {
        docvec![
            "call 'erlang':'whereis'(",
            leaf::atom(util::class_registered_name(class_name)),
            ")",
        ]
    }

    /// BT-4295: a class literal used only for its name, such as an `on:do:`
    /// filter (`beamtalk_exception_handler:matches_class/2` takes the class
    /// name atom as well as the class object, and reads only the name):
    ///
    /// ```erlang
    /// case call 'erlang':'whereis'('beamtalk_class_Error') of
    ///   <'undefined'> when 'true' -> <class_not_found raise>
    ///   <_> when 'true' -> 'Error'
    /// end
    /// ```
    ///
    /// The registry check stays, so an undefined class raises
    /// `class_not_found` exactly as [`Self::generate_class_reference`] does;
    /// only the `module_name_safe/1` ETS reads and the class-object tuple are
    /// dropped. Returns `None` where the full class object is still needed:
    /// a REPL top-level expression (a session binding may shadow the name,
    /// ADR 0129 §7). A package-qualified reference resolves by its bare name
    /// like any other (BT-4074).
    pub(in crate::core_erlang) fn class_name_only_reference(
        &mut self,
        expr: &Expression,
    ) -> Option<Document<'static>> {
        let Expression::ClassReference { name, .. } = expr else {
            return None;
        };
        if self.context == CodeGenContext::Repl {
            return None;
        }
        let class_name = name.name.as_str();
        let error_doc = self.class_not_found_error_doc(class_name);
        Some(docvec![
            "case ",
            Self::registered_class_pid_doc(class_name),
            " of ",
            error_doc,
            "<_> when 'true' -> ",
            leaf::atom(class_name.to_string()),
            " end",
        ])
    }

    /// Builds the `<ClassPid> when 'true' -> let ClassModName =
    /// module_name_safe(ClassPid) in {'beamtalk_object', Tag, ClassModName,
    /// ClassPid}` case clause shared by both registry-resolved
    /// branches of [`Self::generate_class_reference`] above.
    ///
    /// `module_name_safe/1` reads the module from the class metadata table
    /// (two ETS reads), not by a `gen_server:call` into the class process:
    /// every class reference in the node (`on: Exception do:`, a class passed
    /// as an argument) used to queue on that one process (BT-4065).
    pub(in crate::core_erlang) fn class_object_from_registry_clause(
        class_pid_var: &str,
        class_mod_var: &str,
        class_name: &str,
    ) -> Document<'static> {
        docvec![
            "<",
            leaf::var(class_pid_var.to_string()),
            "> when 'true' -> ",
            "let ",
            leaf::var(class_mod_var.to_string()),
            " = call 'beamtalk_object_class':'module_name_safe'(",
            leaf::var(class_pid_var.to_string()),
            ") in ",
            Self::class_object_tuple_doc(
                class_name,
                leaf::var(class_mod_var.to_string()),
                leaf::var(class_pid_var.to_string()),
            ),
            " ",
        ]
    }

    /// `{'beamtalk_object', '<Name> class', Module, ClassPid}`: the class object
    /// for `class_name`, the shape `beamtalk_object_class` builds at run time
    /// (tag, module, pid). Every codegen site that builds a class object from
    /// a class pid uses this, so the field order lives in one place: the class
    /// literal ([`Self::generate_class_reference`]), the typed `class` send
    /// (`generate_typed_class_of`) and the guarded class direct call
    /// (`guarded_class_direct_call_doc`).
    pub(in crate::core_erlang) fn class_object_tuple_doc(
        class_name: &str,
        module: Document<'static>,
        class_pid: Document<'static>,
    ) -> Document<'static> {
        docvec![
            "{'beamtalk_object', ",
            leaf::atom(util::metaclass_tag(class_name)),
            ", ",
            module,
            ", ",
            class_pid,
            "}",
        ]
    }

    /// BT-4411: the `<ClassPid> when 'true' -> {'beamtalk_object', Tag,
    /// 'module', ClassPid}` clause for a built-in whose module is known at
    /// compile time ([`Self::pinned_builtin_class_module`]). The pid is still
    /// the run-time `whereis` result, so a restarted class process is seen
    /// (ADR 0025 amendment §3); only the `module_name_safe/1` lookup is
    /// dropped. The tuple is `=:=` to the one
    /// [`Self::class_object_from_registry_clause`] builds.
    fn class_object_with_module_clause(
        class_pid_var: &str,
        module: &'static str,
        class_name: &str,
    ) -> Document<'static> {
        docvec![
            "<",
            leaf::var(class_pid_var.to_string()),
            "> when 'true' -> ",
            Self::class_object_tuple_doc(
                class_name,
                leaf::atom(module),
                leaf::var(class_pid_var.to_string()),
            ),
            " ",
        ]
    }

    /// Generates Core Erlang code that raises a `class_not_found` error for undefined classes.
    ///
    /// Returns the document fragment for the `<'undefined'>` case branch.
    pub(in crate::core_erlang) fn class_not_found_error_doc(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let err0_var = self.fresh_var("CnfErr");
        let err1_var = self.fresh_var("CnfErr");
        let hint = format!("Define {class_name} with: Object subclass: {class_name}");

        docvec![
            "<'undefined'> when 'true' -> let ",
            leaf::var(err0_var.clone()),
            " = call 'beamtalk_error':'new'('class_not_found', ",
            leaf::atom(class_name.to_string()),
            ") in ",
            "let ",
            leaf::var(err1_var.clone()),
            " = call 'beamtalk_error':'with_hint'(",
            leaf::var(err0_var),
            ", ",
            leaf::binary_lit(hint),
            ") in ",
            "call 'beamtalk_error':'raise'(",
            leaf::var(err1_var),
            ") ",
        ]
    }
}

#[cfg(test)]
mod tests;
