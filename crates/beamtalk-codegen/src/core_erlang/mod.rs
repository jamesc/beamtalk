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
//!   `is_class_var_assignment`, …) shared across the whole crate
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
mod block_analysis;
mod blocks;
mod class_builder_source;
mod class_meta;
mod class_registry;
mod control_flow;
mod dispatch_codegen;
mod dispatch_spec;
mod driver;
pub mod erlang_types;
mod error;
mod errors;
mod expr_shape;
mod expressions;
mod gen_server;
pub(in crate::core_erlang) mod generator;
mod intrinsics;
mod method_frame;
mod nlr;
mod operators;
mod options;
mod patterns;
pub mod primitive_bindings;
mod primitives;
pub mod selector_mangler;
mod sequencing;
mod spec_codegen;
mod supervisor_codegen;
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
pub use driver::{generate, generate_module, generate_module_with_warnings};
pub use error::CodeGenError;
pub use error::Result;
pub(in crate::core_erlang) use generator::DirectCallClassInfo;
pub(in crate::core_erlang) use generator::version::render_state_prefix;
pub use generator::{CodeGenContext, CoreErlangGenerator};
pub(in crate::core_erlang) use nlr::NlrBoundary;
pub use options::{CodegenOptions, GeneratedModule};

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
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_expression(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        // ADR 0118 phase 1a: a sub-expression the sequencing rule
        // already compiled ahead of this parent substitutes its value here
        // — see `precompiled_subexprs`.
        if let Some(doc) = self.take_precompiled_subexpr(expr) {
            return Ok(doc);
        }
        match expr {
            Expression::Literal(lit, _) => self.generate_literal(lit),
            Expression::Identifier(id) => self.generate_identifier(id),
            Expression::ClassReference { name, package, .. } => {
                self.generate_class_reference(&name.name, package.as_ref().map(|p| p.name.as_str()))
            }
            Expression::Super(_) => {
                // Super by itself is not a valid expression - it must be used
                // as a message receiver (e.g., `super increment`)
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
                let doc = if *is_cast {
                    self.generate_cast_send(receiver, selector, arguments)
                } else {
                    self.generate_message_send(receiver, selector, arguments)
                }?;
                // Annotate message sends with source line for BEAM stacktraces.
                // Only annotate CLOSED expressions — see `can_annotate_closed_expression`.
                if self.can_annotate_closed_expression() {
                    if let Some(line_num) = self.span_to_line(*span) {
                        return Ok(self.annotate_with_line(doc, line_num));
                    }
                }
                Ok(doc)
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
                // For identifier assignments (e.g., local variables in REPL like `x := 1`),
                // just return the value - REPL handles binding updates externally.
                // In compiled code, local variable assignments should be handled by
                // the block/method scope, but for now we generate just the value.
                self.generate_expression(value)
            }
            Expression::Return { value, span, .. } => {
                // If inside a block with NLR infrastructure active, generate a throw
                // so the return escapes from the block closure back to the enclosing method.
                // Otherwise (at method body level, or no NLR), just emit the value.
                if let Some(nlr_token) = self.current_nlr_token().cloned() {
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
                    // self-send that threads new state (Actor `State` or,
                    // since phase 5b, class-method `ClassVars`) — nested
                    // anywhere inside it (`^ self.items at: (self bump)`),
                    // not just at its own top level. `state` below must be
                    // computed AFTER `value` threads, so it reflects
                    // whatever `Bind` that dispatch just produced —
                    // `threaded_expression` is the one mechanism that
                    // threads BOTH prefixes correctly for whichever context
                    // this `^` runs in.
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
                    } else if self.in_class_method()
                        && !(self.is_class_var_assignment(value.unwrap_parens())
                            || self.is_class_method_self_send(value.unwrap_parens()))
                    {
                        // ADR 0118 phase 5b: `value` is not ITSELF
                        // a recognized producer at its own top level (e.g.
                        // `^self foo` where `foo` is inherited, so
                        // `is_class_method_self_send`'s `class_method_selectors()`
                        // check excludes it) — `threaded_expression` would
                        // still dispatch it, but through the opaque
                        // `sequenced_send_children` fallback, which closes
                        // over any `ClassVarsN` it rebinds internally: the
                        // compiler's OWN `current_class_var()` bookkeeping
                        // advances to track that rebind regardless, so
                        // reading it below (`state`) would reference a name
                        // never bound in THIS scope.
                        // `refresh_class_var_after_opaque_scope` recovers
                        // the live value via the ADR 0110 shadow write and
                        // re-binds it to a name that IS in scope here.
                        let cv_version_before = self.class_var_version();
                        let result_doc = self.expression_doc(value)?;
                        let refresh = self
                            .refresh_class_var_after_opaque_scope(cv_version_before)
                            .unwrap_or(Document::Nil);
                        (refresh, result_doc)
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
                    // All NLR throws carry state as a 4-tuple.
                    // Actor methods use the current gen_server state; value type
                    // methods use the latest Self{N} snapshot so field mutations
                    // accumulated before the ^ are preserved.
                    // Class methods use the current ClassVars snapshot
                    // — computed after the value above so it reflects any
                    // rebind that value's evaluation just performed.
                    let state = if self.in_class_method() {
                        self.current_class_var()
                    } else if self.context == CodeGenContext::Actor {
                        self.current_state_var()
                    } else {
                        self.current_self_var()
                    };
                    let throw_doc = docvec![
                        "call 'erlang':'throw'({'$bt_nlr', ",
                        leaf::var(nlr_token),
                        ", ",
                        value_doc,
                        ", ",
                        leaf::var(state),
                        "})"
                    ];
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
    /// ADR 0070 Phase 2: When `package` is `Some`, the class is from a known
    /// dependency and the module name is deterministic (`bt@{pkg}@{snake}`).
    /// The class registry lookup uses the class name for now — package-aware
    /// registry disambiguation is a future phase.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_class_reference(
        &mut self,
        class_name: &str,
        package: Option<&str>,
    ) -> Result<Document<'static>> {
        // ADR 0070 Phase 2: For package-qualified references, we know the exact
        // display name to use in the class object tuple.
        let display_name = match package {
            Some(pkg) => format!("{pkg}@{class_name}"),
            None => class_name.to_string(),
        };

        // ADR 0019 Phase 3: Only check bindings in REPL top-level context.
        // Actor methods compiled in workspace mode should NOT check REPL bindings.
        //
        // ADR 0081 Phase 1: for an unqualified class reference, check the
        // session locals map first so a session local of the same name takes
        // precedence. (A capitalised name parses as a ClassReference, not an
        // assignment target, so it cannot itself be rebound via `:=`; the locals
        // check is for symmetry with resolve_name/2 and is essentially always a
        // miss.) On a miss, delegate to the shared runtime resolver, which consults
        // the live singleton + class registries — the singletons
        // (Transcript/Beamtalk/Workspace) are no longer eagerly injected into
        // State, so this lazy lookup replaces the old inline class-registry branch.
        // The resolver raises the same class_not_found error for a genuinely
        // unknown class, preserving REPL output. Package-qualified references
        // (`json@Parser`) keep the inline path below because the resolver does not
        // carry the package-qualified display name.
        if self.workspace_mode() && self.context == CodeGenContext::Repl && package.is_none() {
            let state_var = self.current_state_var();
            let resolved_var = self.fresh_var("ResolvedClass");

            Ok(docvec![
                "case call 'maps':'find'(",
                leaf::atom(class_name.to_string()),
                ", ",
                leaf::var(state_var.clone()),
                ") of ",
                "<{'ok', ",
                leaf::var(resolved_var.clone()),
                "}> when 'true' -> ",
                leaf::var(resolved_var),
                " <'error'> when 'true' -> call 'beamtalk_workspace':'resolve_class_reference'(",
                leaf::var(state_var),
                ", ",
                leaf::atom(class_name.to_string()),
                ") ",
                "end",
            ])
        } else if self.workspace_mode() && self.context == CodeGenContext::Repl {
            // Package-qualified REPL class reference: keep the original
            // locals-then-registry path with the package-qualified display name.
            let class_pid_var = self.fresh_var("ClassPid");
            let class_mod_var = self.fresh_var("ClassModName");
            let state_var = self.current_state_var();
            let error_doc = self.class_not_found_error_doc(class_name);

            Ok(docvec![
                "case call 'maps':'find'(",
                leaf::atom(class_name.to_string()),
                ", ",
                leaf::var(state_var),
                ") of ",
                "<{'ok', _BindingVal}> when 'true' -> _BindingVal ",
                "<'error'> when 'true' -> ",
                "case call 'beamtalk_class_registry':'whereis_class'(",
                leaf::atom(class_name.to_string()),
                ") of ",
                error_doc,
                Self::class_object_from_registry_clause(
                    &class_pid_var,
                    &class_mod_var,
                    &display_name
                ),
                "end end",
            ])
        } else {
            // Actor/ValueType methods in workspace mode and batch mode both use
            // registry-only lookup. ADR 0019 Phase 4: No persistent_term fallback.
            let class_pid_var = self.fresh_var("ClassPid");
            let class_mod_var = self.fresh_var("ClassModName");
            let error_doc = self.class_not_found_error_doc(class_name);

            Ok(docvec![
                "case call 'beamtalk_class_registry':'whereis_class'(",
                leaf::atom(class_name.to_string()),
                ") of ",
                error_doc,
                Self::class_object_from_registry_clause(
                    &class_pid_var,
                    &class_mod_var,
                    &display_name
                ),
                "end",
            ])
        }
    }

    /// Builds the `<ClassPid> when 'true' -> let ClassModName =
    /// module_name(ClassPid) in {'beamtalk_object', Tag, ClassModName,
    /// ClassPid}` case clause shared by both `whereis_class`-resolved
    /// branches of [`Self::generate_class_reference`] above.
    pub(in crate::core_erlang) fn class_object_from_registry_clause(
        class_pid_var: &str,
        class_mod_var: &str,
        display_name: &str,
    ) -> Document<'static> {
        docvec![
            "<",
            leaf::var(class_pid_var.to_string()),
            "> when 'true' -> ",
            "let ",
            leaf::var(class_mod_var.to_string()),
            " = call 'beamtalk_object_class':'module_name'(",
            leaf::var(class_pid_var.to_string()),
            ") in ",
            "{'beamtalk_object', ",
            leaf::atom(util::metaclass_tag(display_name)),
            ", ",
            leaf::var(class_mod_var.to_string()),
            ", ",
            leaf::var(class_pid_var.to_string()),
            "} ",
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
