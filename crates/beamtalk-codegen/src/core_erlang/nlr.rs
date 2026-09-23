// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Non-local-return (NLR) try/catch scaffolding shared by the Actor,
//! class-method, and value-type calling conventions.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! The NLR value objects ([`NlrCatchVars`], [`NlrBoundary`]) and the
//! `wrap_*_with_nlr_catch` family of [`CoreErlangGenerator`] methods that
//! build the try/catch scaffolding around a 4-tuple
//! `{'$bt_nlr', Token, Value, State}` throw. See ADR 0041.

use crate::core_erlang::generator::CoreErlangGenerator;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_cerl_doc::{Document, INDENT, line, nest};

/// Fresh temporary variable names shared by all three NLR try/catch wrappers
/// (Actor, class-method, and value-type).
///
/// Allocated by [`CoreErlangGenerator::alloc_nlr_catch_vars`] and consumed by
/// its `format_try_prefix`/`format_catch_suffix` methods — the single place
/// the try/catch scaffolding is built, shared by
/// [`CoreErlangGenerator::wrap_body_with_nlr_catch`] and
/// [`CoreErlangGenerator::wrap_value_type_body_with_nlr_catch`] so neither
/// hand-rolls its own copy of the template.
#[allow(clippy::struct_field_names)]
pub(in crate::core_erlang) struct NlrCatchVars {
    pub token_var: String,
    pub result_var: String,
    pub cls_var: String,
    pub err_var: String,
    pub stk_var: String,
    pub ctk_var: String,
    pub val_var: String,
    /// State variable captured from the 4-tuple NLR throw.
    pub state_var: String,
    pub ot_pair_var: String,
}

/// The per-context NLR boundary — the *only* thing that differs between the
/// Actor, class-method and value-type non-local-return catch wrappers once the catch
/// vars are shared.
///
/// All three contexts catch the same 4-tuple throw `{'$bt_nlr', Token, Value, State}`
/// (ADR 0041's state-carrying NLR convention); they disagree only about the Document
/// the matching catch arm yields. This enum captures that single axis so the catch
/// scaffolding can be written once (see [`nlr_arm_result`]) instead of being
/// copy-evolved per context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum NlrBoundary {
    /// Actor (`gen_server`) methods: the catch arm yields `{'reply', Value, State}`.
    ActorReply,
    /// Class methods: the catch arm yields `Value` (no class vars) or
    /// `{'class_var_result', Value, State}` when class vars were mutated.
    ClassMethod { has_class_vars: bool },
    /// Value-type methods: the catch arm yields `{Value, State}` so the normal and
    /// NLR-catch paths produce the same `{Result, Self{N}}` shape.
    ValueType,
}

/// Builds the Document the matching NLR catch arm yields for `boundary`.
///
/// This is the single place the per-context divergence between the three former
/// `wrap_*_body_with_nlr_catch` wrappers lives. `val_var`/`state_var` are the
/// catch-bound `Value`/`State` extracted from the 4-tuple throw. Shared by the
/// gen-server wrapper ([`CoreErlangGenerator::wrap_body_with_nlr_catch`]) and
/// [`NlrCatchVars::format_catch_suffix`].
///
/// Use Document/docvec! — never format!() for Core Erlang fragments.
fn nlr_arm_result(val_var: &str, state_var: &str, boundary: NlrBoundary) -> Document<'static> {
    match boundary {
        NlrBoundary::ActorReply => docvec![
            "{'reply', ",
            leaf::var(val_var.to_string()),
            ", ",
            leaf::var(state_var.to_string()),
            "}",
        ],
        NlrBoundary::ClassMethod {
            has_class_vars: true,
        } => docvec![
            "{'class_var_result', ",
            leaf::var(val_var.to_string()),
            ", ",
            leaf::var(state_var.to_string()),
            "}",
        ],
        NlrBoundary::ClassMethod {
            has_class_vars: false,
        } => leaf::var(val_var.to_string()),
        NlrBoundary::ValueType => docvec![
            "{",
            leaf::var(val_var.to_string()),
            ", ",
            leaf::var(state_var.to_string()),
            "}",
        ],
    }
}

impl NlrCatchVars {
    /// Formats the try prefix for the NLR wrapper.
    ///
    /// Returns `Document` instead of `String` for composable codegen.
    ///
    /// ```text
    /// let TokenVar = call 'erlang':'make_ref'() in
    /// try
    /// ```
    pub fn format_try_prefix(&self) -> Document<'static> {
        docvec![
            "    let ",
            leaf::var(self.token_var.clone()),
            " = call 'erlang':'make_ref'() in",
            nest(INDENT, line()),
            "try",
            line(),
        ]
    }

    /// Formats the catch suffix for the NLR wrapper.
    ///
    /// Returns `Document` instead of `String` for composable codegen.
    /// Catches 4-tuple NLR throws and yields `boundary`'s matching-arm
    /// result (see [`nlr_arm_result`]) — the single place this scaffolding
    /// is built for the Actor, class-method, and value-type boundaries
    /// alike.
    ///
    /// ```text
    /// of Result -> Result
    /// catch <Cls, Err, Stk> ->
    ///   case {Cls, Err} of
    ///     <{'throw', {'$bt_nlr', CatchTok, Val, State}}> when ... -> <boundary's arm>
    ///     <Other> when 'true' -> primop 'raw_raise'(Cls, Err, Stk)
    ///   end
    /// ```
    pub fn format_catch_suffix(&self, boundary: NlrBoundary) -> Document<'static> {
        docvec![
            nest(INDENT, line()),
            "of ",
            leaf::var(self.result_var.clone()),
            " -> ",
            leaf::var(self.result_var.clone()),
            nest(INDENT, line()),
            "catch <",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            ", ",
            leaf::var(self.stk_var.clone()),
            "> ->",
            nest(INDENT + 2, line()),
            "case {",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            "} of",
            nest(INDENT + 4, line()),
            "<{'throw', {'$bt_nlr', ",
            leaf::var(self.ctk_var.clone()),
            ", ",
            leaf::var(self.val_var.clone()),
            ", ",
            leaf::var(self.state_var.clone()),
            "}}> ",
            "when call 'erlang':'=:='(",
            leaf::var(self.ctk_var.clone()),
            ", ",
            leaf::var(self.token_var.clone()),
            ") -> ",
            nlr_arm_result(&self.val_var, &self.state_var, boundary),
            nest(INDENT + 4, line()),
            "<",
            leaf::var(self.ot_pair_var.clone()),
            "> when 'true' -> ",
            "primop 'raw_raise'(",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            ", ",
            leaf::var(self.stk_var.clone()),
            ")",
            nest(INDENT + 2, line()),
            "end",
            line(),
        ]
    }
}

impl CoreErlangGenerator {
    /// Allocates fresh temporary variable names for an NLR try/catch wrapper.
    fn alloc_nlr_catch_vars(&mut self, token_var: &str) -> NlrCatchVars {
        NlrCatchVars {
            token_var: token_var.to_string(),
            result_var: self.fresh_temp_var("NlrResult"),
            cls_var: self.fresh_temp_var("NlrCls"),
            err_var: self.fresh_temp_var("NlrErr"),
            stk_var: self.fresh_temp_var("NlrStk"),
            ctk_var: self.fresh_temp_var("CatchTok"),
            val_var: self.fresh_temp_var("NlrVal"),
            state_var: self.fresh_temp_var("NlrState"),
            ot_pair_var: self.fresh_temp_var("OtherPair"),
        }
    }

    /// The single boundary-parameterised NLR try/catch wrapper.
    ///
    /// Shares its try/catch scaffolding — via [`NlrCatchVars::format_try_prefix`]
    /// and [`NlrCatchVars::format_catch_suffix`] — with
    /// [`CoreErlangGenerator::wrap_value_type_body_with_nlr_catch`]'s callers,
    /// so the make-token/`try`/`of`/`catch` the 4-tuple
    /// `{'$bt_nlr', Token, Value, State}` throw template is written exactly
    /// once; only the matching arm's result Document varies, which
    /// `boundary` selects via [`nlr_arm_result`].
    ///
    /// Use Document/docvec! — never format!() for Core Erlang fragments.
    pub(in crate::core_erlang) fn wrap_body_with_nlr_catch(
        &mut self,
        body_doc: Document<'static>,
        token_var: &str,
        boundary: NlrBoundary,
    ) -> Document<'static> {
        // ADR 0111 Phase D: this is the true call site
        // `ThreadedStmt::NlrCatch` faithfully models (module docs on
        // `threaded_ir::ThreadedStmt::NlrCatch`) — every NLR try/catch this
        // generator ever emits (Actor/ClassMethod/ValueType alike) is built
        // here; its `boundary` shape is also what
        // `threaded_ir::construct_and_verify_class_var_bind`'s synthetic
        // marker (built from its `frame`/`shadow_write_eligible` pair, ADR
        // 0111 Addendum 9) reconstructs at the class-var
        // Bind-emission sites (`expressions.rs`, `dispatch_codegen.rs`) for
        // the ADR 0110 ShadowWriteMissing contract. No standalone
        // `verify()` call here: a
        // lone `NlrCatch` with no `Bind` can never trigger any
        // `VerifyError` (`walk_stmt` treats it as a no-op), so constructing
        // one on every NLR-catch wrap — a hot path — would pay a real
        // allocation for a check that can't fire (caught in review).

        let vars = self.alloc_nlr_catch_vars(token_var);
        docvec![
            vars.format_try_prefix(),
            body_doc,
            vars.format_catch_suffix(boundary),
        ]
    }

    /// Wraps a value type method body with NLR (non-local return) try/catch.
    ///
    /// Value type NLR uses a 4-element throw tuple `{$bt_nlr, Token, Value, State}`
    /// and catches it to return `{Value, State}`. Returns the allocated
    /// [`NlrCatchVars`] so the caller can emit `format_try_prefix()`, the
    /// body, and `format_catch_suffix(NlrBoundary::ValueType)` around its own
    /// surrounding `fun` scaffolding.
    pub(in crate::core_erlang) fn wrap_value_type_body_with_nlr_catch(
        &mut self,
        token_var: &str,
    ) -> NlrCatchVars {
        self.alloc_nlr_catch_vars(token_var)
    }

    /// Returns the current NLR token variable name, if any.
    pub(in crate::core_erlang) fn current_nlr_token(&self) -> Option<&String> {
        self.value_type_context
            .as_ref()
            .and_then(|ctx| ctx.current_nlr_token.as_ref())
    }

    /// Sets the current NLR token variable name.
    pub(in crate::core_erlang) fn set_current_nlr_token(&mut self, token: Option<String>) {
        self.value_type_context_mut().current_nlr_token = token;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nlr_arm_result_actor_reply_yields_reply_tuple() {
        let doc = nlr_arm_result("Val", "St", NlrBoundary::ActorReply);
        assert_eq!(doc.to_pretty_string(), "{'reply', Val, St}");
    }

    #[test]
    fn nlr_arm_result_class_method_with_vars_yields_class_var_result_tuple() {
        let doc = nlr_arm_result(
            "V",
            "S",
            NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
        );
        assert_eq!(doc.to_pretty_string(), "{'class_var_result', V, S}");
    }

    #[test]
    fn nlr_arm_result_class_method_without_vars_yields_bare_value() {
        let doc = nlr_arm_result(
            "V",
            "S",
            NlrBoundary::ClassMethod {
                has_class_vars: false,
            },
        );
        assert_eq!(doc.to_pretty_string(), "V");
    }

    #[test]
    fn nlr_arm_result_value_type_yields_value_state_tuple() {
        let doc = nlr_arm_result("V", "S", NlrBoundary::ValueType);
        assert_eq!(doc.to_pretty_string(), "{V, S}");
    }

    fn make_test_vars() -> NlrValueTypeCatchVars {
        NlrValueTypeCatchVars {
            token_var: "Tok0".to_string(),
            result_var: "Res0".to_string(),
            cls_var: "Cls0".to_string(),
            err_var: "Err0".to_string(),
            stk_var: "Stk0".to_string(),
            ctk_var: "CTok0".to_string(),
            val_var: "Val0".to_string(),
            state_var: "St0".to_string(),
            ot_pair_var: "OtP0".to_string(),
        }
    }

    #[test]
    fn format_try_prefix_binds_token_with_make_ref() {
        let doc = make_test_vars().format_try_prefix().to_pretty_string();
        assert!(
            doc.contains("Tok0"),
            "prefix should bind token var. Got:\n{doc}"
        );
        assert!(
            doc.contains("call 'erlang':'make_ref'()"),
            "prefix should call erlang:make_ref/0 to mint the NLR token. Got:\n{doc}"
        );
        assert!(
            doc.contains("try"),
            "prefix should open a try block. Got:\n{doc}"
        );
    }

    #[test]
    fn format_catch_suffix_passthrough_arm_uses_result_var() {
        let doc = make_test_vars().format_catch_suffix().to_pretty_string();
        // The `of` arm passes through non-throw results unchanged: `of Res0 -> Res0`.
        assert!(
            doc.contains("of Res0 -> Res0"),
            "catch suffix `of` arm must pass through the result_var unchanged. Got:\n{doc}"
        );
    }

    #[test]
    fn format_catch_suffix_nlr_arm_yields_value_state_pair() {
        let doc = make_test_vars().format_catch_suffix().to_pretty_string();
        // Matching NLR throw: pattern must name the '$bt_nlr' tag.
        assert!(
            doc.contains("'$bt_nlr'"),
            "NLR pattern must match the '$bt_nlr' tag. Got:\n{doc}"
        );
        // Token guard: catch-token must equal the method's own token.
        assert!(
            doc.contains("'=:='(CTok0, Tok0)"),
            "catch suffix should guard with CTok0 =:= Tok0. Got:\n{doc}"
        );
        // ValueType boundary: NLR arm returns {Value, State}.
        assert!(
            doc.contains("{Val0, St0}"),
            "ValueType NLR arm must yield {{Value, State}} pair. Got:\n{doc}"
        );
    }

    #[test]
    fn format_catch_suffix_reraises_non_nlr_exceptions() {
        let doc = make_test_vars().format_catch_suffix().to_pretty_string();
        // Non-NLR exceptions fall through to the wildcard arm and are re-raised.
        assert!(
            doc.contains("primop 'raw_raise'(Cls0, Err0, Stk0)"),
            "catch suffix must re-raise non-NLR exceptions via primop raw_raise. Got:\n{doc}"
        );
    }
}
