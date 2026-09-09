// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary operator code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf::{atom, var};
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

/// BT-2709/BT-2710: Which runtime guard a dispatchable binary operator emits
/// for an unknown/generic receiver. The two variants differ only in the guard
/// predicate and which `case` branch dispatches vs. takes the bare BIF.
#[derive(Clone, Copy)]
enum OperatorGuard {
    /// `+ - * /`: `is_number` → bare BIF; non-number → message dispatch.
    Arithmetic,
    /// `< > <= >=`: `is_object` → message dispatch; primitive → bare BIF.
    Comparison,
}

/// The runtime test a guarded operator emits: the discriminating predicate plus
/// which `case` branch dispatches (vs. takes the bare BIF). Named fields rather
/// than a positional tuple so the branch order isn't an ambiguous bare `bool`.
struct GuardSpec {
    /// Module of the predicate (`erlang` / `beamtalk_primitive`).
    predicate_module: &'static str,
    /// Predicate function name (`is_number` / `is_object`).
    predicate_fn: &'static str,
    /// When `true`, the `'true'` (predicate-holds) branch is the message
    /// dispatch and the `'false'` branch is the bare BIF; when `false`, swapped.
    dispatch_on_true: bool,
}

impl OperatorGuard {
    /// The predicate + branch order for this guard. All strings are static, so
    /// they are safe as `Document::Str` leaves.
    fn spec(self) -> GuardSpec {
        match self {
            Self::Arithmetic => GuardSpec {
                predicate_module: "erlang",
                predicate_fn: "is_number",
                dispatch_on_true: false,
            },
            Self::Comparison => GuardSpec {
                predicate_module: "beamtalk_primitive",
                predicate_fn: "is_object",
                dispatch_on_true: true,
            },
        }
    }
}

impl CoreErlangGenerator {
    /// Generates code for binary operators.
    ///
    /// Maps Beamtalk binary operators to Erlang's built-in operators:
    /// - Arithmetic: `+`, `-`, `*`, `/`, `%` (rem), `**` (exponentiation)
    /// - Comparison: `==`, `=:=` (strict), `/=` (inequality), `=/=` (strict inequality), `<`, `>`, `<=`, `>=`
    /// - Concatenation: `++` (list append via `erlang:'++'`, string via `iolist_to_binary`)
    ///
    /// # Arguments
    ///
    /// * `op` - The binary operator symbol
    /// * `left` - The left operand expression
    /// * `arguments` - Array containing the right operand (must have exactly one element)
    ///
    /// # Errors
    ///
    /// Returns error if arguments length is not exactly 1 or operator is unsupported.
    pub(in crate::core_erlang) fn generate_binary_op(
        &mut self,
        op: &str,
        left: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(
                "binary operator must have exactly one argument".to_string(),
            ));
        }

        // Special case: ** uses math:pow (no direct Erlang operator)
        if op == "**" {
            return self.generate_power_op(left, &arguments[0]);
        }

        // Special case: ++ works on both lists and strings
        if op == "++" {
            return self.generate_concat_op(left, &arguments[0]);
        }

        let erlang_op = match op {
            "+" => "+",
            "-" => "-",
            "*" => "*",
            "/" => "/",
            "%" => "rem",
            "==" => "==",
            "=:=" => "=:=", // Strict equality (ADR 0002)
            "/=" => "/=",   // Loose inequality (ADR 0002)
            "=/=" => "=/=", // Strict inequality (ADR 0002)
            "<" => "<",
            ">" => ">",
            "<=" => "=<",
            ">=" => ">=",
            _ => {
                let right = &arguments[0];
                return Err(CodeGenError::UnsupportedFeature {
                    feature: format!("binary operator: {op}"),
                    span: Some(Span::new(left.span().start(), right.span().end())),
                });
            }
        };

        // BT-2709/BT-2710: Arithmetic (`+ - * /`) and comparison (`< > <= >=`)
        // operators are dispatchable messages so user value-types can overload
        // them. When the receiver is *statically* known to be a builtin (a
        // literal, `self` inside a builtin class, or a suitably-annotated
        // parameter) we keep the bare BIF fast path (zero cost — all stdlib hot
        // paths). Otherwise we emit a runtime guard.
        //
        // The two guards differ in *predicate* and *branch order* (BT-2710):
        //   * Arithmetic — `is_number`: non-numbers `badarith`, so number→BIF,
        //     object→dispatch is the safe discriminator.
        //   * Comparison — `is_object`: Erlang `<` is a total order over every
        //     term and never raises, so a bare BIF on an object would *silently*
        //     term-order it. The guard inverts to object→dispatch, builtin→BIF.
        // `%`/`**`/equality are not message-dispatched here.
        let is_arithmetic = matches!(op, "+" | "-" | "*" | "/");
        let is_comparison = matches!(op, "<" | ">" | "<=" | ">=");
        let guard = if is_arithmetic && !self.receiver_is_statically_numeric(left) {
            Some(OperatorGuard::Arithmetic)
        } else if is_comparison && !self.receiver_is_statically_comparable(left) {
            Some(OperatorGuard::Comparison)
        } else {
            None
        };

        // ADR 0116/BT-3263: number-on-the-left coercion. Every arithmetic call
        // site that can reach a bare BIF with the *left* operand numeric (the
        // always-bare path below, and the `is_number`-true branch inside
        // `guarded_op_doc`'s Arithmetic case) must also account for a
        // non-numeric *right* operand — `5 + aVector` — which today emits a
        // bare `erlang:'+'` with no guard at all and crashes with a raw
        // `badarith`. `receiver_is_statically_numeric` applied to the right
        // operand is the same compile-time skip already used for the left
        // operand: a `total + delta`-shaped call site (right operand a numeric
        // literal or `:: Number`-family typed) stays a bare BIF, unchanged.
        // Only a right operand whose type is genuinely unknown reaches the new
        // `try`/`catch` mechanism (`Self::number_coercion_try_catch`).
        let right_is_statically_numeric = self.receiver_is_statically_numeric(&arguments[0]);

        // BT-1937: Capture both operands in evaluation order. When either
        // operand needs a real prelude (e.g., a class method self-send
        // mutating a class var), `thread_subexprs` force-hoists BOTH
        // operands into that preamble so left-to-right evaluation order is
        // preserved. When neither needs one, both operands stay inline and
        // there is no hoisting overhead.
        let exprs: [&Expression; 2] = [left, &arguments[0]];
        let (preamble, mut docs) = self.thread_subexprs(&exprs, "BinOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");

        let call_doc = if let Some(guard) = guard {
            self.guarded_op_doc(
                guard,
                op,
                erlang_op,
                left_code,
                right_code,
                right_is_statically_numeric,
            )
        } else if is_arithmetic && !right_is_statically_numeric {
            // ADR 0116/BT-3263: left is statically numeric but the right
            // operand's type is genuinely unknown — wrap the bare BIF in the
            // badarith-catching number-on-the-left coercion mechanism instead
            // of emitting it unguarded.
            self.number_coercion_bare_path(op, erlang_op, left_code, right_code)
        } else {
            // CLAUDE.md: Core Erlang fragments MUST use Document/docvec!, never
            // format!(). erlang_op is one of the static literals in the match
            // arms above, so Document::Str is safe.
            docvec![
                "call 'erlang':'",
                Document::Str(erlang_op),
                "'(",
                left_code,
                ", ",
                right_code,
                ")",
            ]
        };

        Ok(self.close_prelude(&preamble, call_doc, "BinOp"))
    }

    /// BT-2709: Whether `expr` is statically known to evaluate to a number, so
    /// the arithmetic fast path can skip the runtime `is_number` guard.
    ///
    /// Mirrors the gradual-typing contract: an annotation (or a syntactic fact)
    /// *removes* the guard; its absence keeps the guard, which is always correct.
    /// Recognises sources of numeric certainty already present in the
    /// AST / codegen context — no new analysis pass:
    /// * a numeric literal receiver (`1 + x`),
    /// * `self` inside a numeric class (`Integer`/`Float` method bodies),
    /// * an identifier bound to a `:: Integer/Float/Number` parameter, and
    /// * a `self.<field>` read whose field is numeric-typed or untyped.
    ///
    /// The `self.<field>` case keeps numeric / untyped instance-field arithmetic
    /// on the bare path — both to avoid regressing the broad
    /// `self.count := self.count + 1` counter pattern (and the dependent
    /// `ifTrue:` inline-case state-threading optimisation) and because untyped
    /// fields carry no type information. A field with an explicit **non-numeric**
    /// declared type (e.g. `field: lo :: Money`) instead routes through the
    /// `is_number` guard so `self.lo + x` dispatches to the field type's `+`. (On
    /// the arithmetic path a non-number field would otherwise `badarith`; on the
    /// comparison path the analogous miss is *silently wrong* — see
    /// [`Self::receiver_is_statically_comparable`].)
    ///
    /// ADR 0116/BT-3263 reuses this same predicate, unmodified, to gate the
    /// number-on-the-left coercion mechanism's *right* operand — per the
    /// ADR's own "Trigger condition, refined" spec, which explicitly folds
    /// "a numeric/untyped field" into "the exact same rule already applied
    /// to the left operand." That reuse means the untyped-field leniency
    /// above (deliberately tuned for the left operand's hot-loop counter
    /// pattern) reproduces on the right side too: an untyped `self.extra` in
    /// `self.total + self.extra` is trusted as numeric and never reaches the
    /// new `try`/`catch`, so a non-numeric `extra` still raw-crashes on
    /// `badarith` at runtime instead of dispatching to `plusFromNumber:` —
    /// tracked as BT-3266, not fixed here.
    pub(in crate::core_erlang) fn receiver_is_statically_numeric(&self, expr: &Expression) -> bool {
        use beamtalk_core::ast::Literal;
        match expr {
            Expression::Literal(Literal::Integer(_) | Literal::Float(_), _) => true,
            Expression::Identifier(id) => {
                if id.name == "self" {
                    matches!(self.class_name().as_str(), "Integer" | "Float")
                } else {
                    self.param_is_numeric(&id.name)
                }
            }
            // `self.<field>` stays bare only when the field is numeric-typed or
            // untyped (see doc above); explicit object-typed fields are guarded.
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
                    && self.field_is_numeric(&field.name)
            }
            _ => false,
        }
    }

    /// BT-2710: Whether `expr` is statically known to evaluate to a value with a
    /// builtin total order, so the comparison fast path can skip the runtime
    /// `is_object` guard and emit a bare comparison BIF.
    ///
    /// A broader set than [`Self::receiver_is_statically_numeric`]: bare
    /// `erlang:'<'` is correct for *every* primitive-ordered type, so this also
    /// accepts `Character`/`String` (both define `< <=` as `@primitive`) and
    /// their literals — only Beamtalk objects need dispatch. Recognises:
    /// * numeric / character / string literals,
    /// * `self` inside a builtin comparable class
    ///   (`Integer`/`Float`/`Character`/`String`),
    /// * an identifier bound to a `:: Integer/Float/Number/Character/String`
    ///   parameter, and
    /// * a `self.<field>` read whose field is primitive-ordered-typed or untyped.
    ///
    /// The `self.<field>` case is **type-aware** (BT-2710 follow-up), and the
    /// stakes are higher here than for arithmetic: `erlang:'<'` is a total order
    /// over every term and never raises, so a bare comparison on an object-typed
    /// field would *silently* term-order the tagged map instead of dispatching —
    /// a wrong boolean, not a `badarith`. So a field with an explicit
    /// non-primitive declared type (e.g. `field: lo :: Money`) routes through the
    /// `is_object` guard, making `self.lo < other lo` dispatch to `Money>><`.
    /// Numeric / `Character` / `String` fields, and untyped fields (no info, kept
    /// bare for status quo), stay on the bare comparison BIF.
    pub(in crate::core_erlang) fn receiver_is_statically_comparable(
        &self,
        expr: &Expression,
    ) -> bool {
        use beamtalk_core::ast::Literal;
        match expr {
            Expression::Literal(
                Literal::Integer(_)
                | Literal::Float(_)
                | Literal::String(_)
                | Literal::Character(_),
                _,
            ) => true,
            Expression::Identifier(id) => {
                if id.name == "self" {
                    matches!(
                        self.class_name().as_str(),
                        "Integer" | "Float" | "Character" | "String"
                    )
                } else {
                    self.param_is_comparable(&id.name)
                }
            }
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
                    && self.field_is_comparable(&field.name)
            }
            _ => false,
        }
    }

    /// BT-2709/BT-2710: Builds a runtime-guarded operator dispatch for an
    /// unknown/generic receiver, mirroring `generate_concat_op`'s `is_list` arm.
    /// One helper serves both arithmetic and comparison; the [`OperatorGuard`]
    /// selects the predicate and which branch dispatches:
    ///
    /// ```erlang
    /// let <Lhs> = <left> in
    ///   let <Rhs> = <right> in
    ///     case call '<Mod>':'<Pred>'(Lhs) of
    ///       <'true'>  when 'true' -> <true branch>
    ///       <'false'> when 'true' -> <false branch>
    ///     end
    /// ```
    ///
    /// * Arithmetic (`is_number`): true → bare BIF, false → dispatch. Numbers
    ///   take the BIF; objects route through dispatch (overload or DNU).
    /// * Comparison (`is_object`): true → dispatch, false → bare BIF. Objects
    ///   dispatch (so `aMoney < bMoney` reaches `Money>><` and an unknown type
    ///   raises a DNU); primitives keep Erlang's total term-order.
    ///
    /// `erlang_op` and the predicate module/function are static literals (safe as
    /// `Document::Str`); the dispatch selector uses `leaf::atom` for the original
    /// Beamtalk operator.
    ///
    /// ADR 0116/BT-3263: for the `Arithmetic` guard specifically, the
    /// `is_number`-true (bare-BIF) branch is itself replaced by the
    /// number-on-the-left `try`/`catch` coercion wrapper
    /// ([`Self::number_coercion_try_catch`]) whenever `right_is_statically_numeric`
    /// is `false` — a numeric-at-runtime *left* operand (`x + y` where `x`
    /// turns out to be a number) still needs the same badarith-catching
    /// mechanism the always-bare path uses when the right operand's type
    /// isn't known at compile time. When `right_is_statically_numeric` is
    /// `true` (or the guard is `Comparison`), this branch is the bare BIF
    /// exactly as before — the zero-cost guarantee is structural, not
    /// approximate. `left_var`/`right_var` are already let-bound above, so
    /// the wrapper reuses them directly rather than re-binding.
    ///
    /// BT-3163: this `case` matches only `<'true'>`/`<'false'>`, no wildcard —
    /// the same non-exhaustive-to-the-compiler shape `case_clause_fallback`
    /// exists for (see its doc comment and ADR 0111 Addendum 5, "Production
    /// bugs found", bug 3 / BT-3161). Confirmed empirically reachable for the
    /// **comparison** guard (`beamtalk_primitive:is_object/1`, a plain
    /// function the Core Erlang compiler cannot prove exhaustive) as a try
    /// body's last statement, e.g. `[... . a < b] ensure: [...]` with
    /// `a`/`b` not statically comparable: erlc's `ambiguous_catch_try_state`
    /// `beam_validator` bug. The **arithmetic** guard's `erlang:is_number/1`
    /// predicate happens not to trigger it — the compiler's BIF return-type
    /// inference already proves that case exhaustive without a wildcard — but
    /// the explicit fallback is added to both branches uniformly (cheap,
    /// behavior-preserving, and doesn't depend on that inference continuing
    /// to hold in some future OTP release).
    fn guarded_op_doc(
        &mut self,
        guard: OperatorGuard,
        op: &str,
        erlang_op: &'static str,
        left_code: Document<'static>,
        right_code: Document<'static>,
        right_is_statically_numeric: bool,
    ) -> Document<'static> {
        let GuardSpec {
            predicate_module: pred_module,
            predicate_fn: pred_fn,
            dispatch_on_true,
        } = guard.spec();
        let left_var = self.fresh_temp_var("BinLeft");
        let right_var = self.fresh_temp_var("BinRight");

        let bare_bif = docvec![
            "call 'erlang':'",
            Document::Str(erlang_op),
            "'(",
            var(left_var.clone()),
            ", ",
            var(right_var.clone()),
            ")",
        ];
        // ADR 0116/BT-3263: see this function's doc comment — the arithmetic
        // guard's bare-BIF branch becomes the coercion `try`/`catch` when the
        // right operand's type is genuinely unknown; every other case (the
        // comparison guard, or a statically-numeric right operand) keeps the
        // bare BIF unchanged.
        let bif_branch =
            if matches!(guard, OperatorGuard::Arithmetic) && !right_is_statically_numeric {
                self.number_coercion_try_catch(
                    op,
                    erlang_op,
                    var(left_var.clone()),
                    var(right_var.clone()),
                )
            } else {
                bare_bif
            };
        let send_branch = docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            var(left_var.clone()),
            ", ",
            atom(op.to_string()),
            ", [",
            var(right_var.clone()),
            "])",
        ];
        let (true_branch, false_branch) = if dispatch_on_true {
            (send_branch, bif_branch)
        } else {
            (bif_branch, send_branch)
        };
        // BT-3163: explicit wildcard so this boolean `case` is statically
        // exhaustive — see this function's doc comment and
        // `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("BinOpNoMatch");

        docvec![
            "let ",
            var(left_var.clone()),
            " = ",
            left_code,
            " in let ",
            var(right_var.clone()),
            " = ",
            right_code,
            " in case call '",
            Document::Str(pred_module),
            "':'",
            Document::Str(pred_fn),
            "'(",
            var(left_var),
            ") of <'true'> when 'true' -> ",
            true_branch,
            " <'false'> when 'true' -> ",
            false_branch,
            no_match_fallback,
            " end",
        ]
    }

    /// ADR 0116/BT-3263: maps an arithmetic operator to its number-on-the-left
    /// reflected-method selector (`n <op>FromNumber: self`, called on the
    /// *right* operand with the *left* operand as the argument — see the
    /// ADR's § Reflected method protocol for why the operand order is
    /// reversed). Callers only reach this for `+ - * /`, already validated by
    /// `is_arithmetic` in `generate_binary_op`; any other operator is a
    /// caller bug, not a runtime possibility.
    fn coercion_selector(op: &str) -> &'static str {
        match op {
            "+" => "plusFromNumber:",
            "-" => "minusFromNumber:",
            "*" => "timesFromNumber:",
            "/" => "divFromNumber:",
            _ => unreachable!("coercion_selector called for non-arithmetic operator: {op}"),
        }
    }

    /// ADR 0116/BT-3263: builds the number-on-the-left coercion `try`/`catch`
    /// around a single arithmetic BIF call, for the residual case where the
    /// right operand's type is genuinely unknown at compile time.
    ///
    /// `left_doc`/`right_doc` **must** already be safe to reference more than
    /// once — a bound variable, not an arbitrary expression — since both are
    /// referenced again inside the catch handler (`is_number(right)`, and
    /// both operands as `send_number_coercion/4` arguments). Callers that
    /// don't already have bound operands (the always-bare path) must
    /// `let`-bind them first; see [`Self::number_coercion_bare_path`].
    /// `guarded_op_doc` already has `left_var`/`right_var` bound and passes
    /// those directly.
    ///
    /// Spike-verified shape (ADR 0116 § Implementation, de-risking spike):
    /// ```erlang
    /// try
    ///     call 'erlang':'+'(BinLeft, BinRight)
    /// of <TryResult> -> TryResult
    /// catch <Type, Error, Stack> ->
    ///     case {Type, Error} of
    ///         <{'error', 'badarith'}> when 'true' ->
    ///             case call 'erlang':'is_number'(BinRight) of
    ///                 <'true'> when 'true' ->
    ///                     %% BinRight IS a number — badarith wasn't a coercion
    ///                     %% miss (e.g. `5 / 0`, or float overflow). Re-raise
    ///                     %% unchanged so the existing badarith classification
    ///                     %% (ADR 0028/BT-2704) still handles it.
    ///                     primop 'raw_raise'(Type, Error, Stack)
    ///                 <'false'> when 'true' ->
    ///                     call 'beamtalk_message_dispatch':'send_number_coercion'(
    ///                         BinRight, 'plusFromNumber:', [BinLeft], '+')
    ///                 <NoMatch> when 'true' ->
    ///                     call 'erlang':'error'({'case_clause', NoMatch})
    ///             end
    ///         <OtherPair> when 'true' ->
    ///             primop 'raw_raise'(Type, Error, Stack)
    ///     end
    /// ```
    ///
    /// Note there is **no** trailing `end` for the `try` itself — unlike
    /// `case`/`let`, Core Erlang's `try...of...catch...` has no closing
    /// keyword of its own; it terminates wherever the catch clause's body
    /// expression terminates (confirmed against `erlc`'s own `+to_core`
    /// output for a hand-written `try`/`catch` — a bare `end` there is a
    /// dangling token and a syntax error, not the harmless extra bracket the
    /// ADR's illustrative snippet might suggest).
    ///
    /// The mandatory `of <TryResult> -> TryResult` clause (Core Erlang, unlike
    /// Erlang source, requires it explicitly — its absence is a syntax error)
    /// and the mandatory `when` guard on every `case` clause were both
    /// confirmed via `erlc` in the spike. The re-raise arms below call
    /// `control_flow::exception_handling::CoreErlangGenerator::emit_raw_raise`
    /// (never `erlang:raise/3`, which expects a pre-built stacktrace term
    /// rather than the raw trace a catch clause binds) — the same shared
    /// helper `on_do_catch_preamble` uses, not a re-implementation of its
    /// `primop 'raw_raise'` shape. BT-3163's `case_clause_fallback`
    /// convention (`erlang:error({case_clause, _})`, not `raw_raise`) applies
    /// only to the inner `is_number` boolean case's defensive third arm — an
    /// internal-invariant guard, not a real exception to propagate — matching
    /// `guarded_op_doc`'s own precedent for the identical shape. The outer
    /// `{Type, Error}` case's fallback arm binds a fresh throwaway variable
    /// (not `Type`/`Error` again) and re-raises via `raw_raise`, since it is a
    /// real exception (anything that isn't `badarith`) that must propagate
    /// unchanged.
    fn number_coercion_try_catch(
        &mut self,
        op: &str,
        erlang_op: &'static str,
        left_doc: Document<'static>,
        right_doc: Document<'static>,
    ) -> Document<'static> {
        let selector = Self::coercion_selector(op);
        let try_result_var = self.fresh_temp_var("BinCoerceTry");
        let type_var = self.fresh_temp_var("BinCoerceType");
        let error_var = self.fresh_temp_var("BinCoerceError");
        let stack_var = self.fresh_temp_var("BinCoerceStack");
        let other_pair_var = self.fresh_temp_var("BinCoerceOther");
        // BT-3163: explicit wildcard for the inner is_number boolean case —
        // see this function's doc comment and `case_clause_fallback`'s own.
        let inner_no_match = self.case_clause_fallback("BinCoerceNoMatch");

        docvec![
            "try call 'erlang':'",
            Document::Str(erlang_op),
            "'(",
            left_doc.clone(),
            ", ",
            right_doc.clone(),
            ") of <",
            var(try_result_var.clone()),
            "> -> ",
            var(try_result_var),
            " catch <",
            var(type_var.clone()),
            ", ",
            var(error_var.clone()),
            ", ",
            var(stack_var.clone()),
            "> -> case {",
            var(type_var.clone()),
            ", ",
            var(error_var.clone()),
            "} of <{'error', 'badarith'}> when 'true' -> case call 'erlang':'is_number'(",
            right_doc.clone(),
            ") of <'true'> when 'true' -> ",
            Self::emit_raw_raise(type_var.clone(), error_var.clone(), stack_var.clone()),
            " <'false'> when 'true' -> call 'beamtalk_message_dispatch':'send_number_coercion'(",
            right_doc,
            ", ",
            atom(selector),
            ", [",
            left_doc,
            "], ",
            atom(op.to_string()),
            ")",
            inner_no_match,
            " end <",
            var(other_pair_var),
            "> when 'true' -> ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
            " end",
        ]
    }

    /// ADR 0116/BT-3263: the always-bare arithmetic path's number-on-the-left
    /// entry point — used by `generate_binary_op` when the *left* operand is
    /// statically numeric (so no runtime guard is needed for it) but the
    /// *right* operand's type is genuinely unknown. Unlike `guarded_op_doc`
    /// (which already has `left_var`/`right_var` bound for its own guard),
    /// this path's `left_code`/`right_code` may be arbitrary un-hoisted
    /// expression documents — referencing either twice inside
    /// [`Self::number_coercion_try_catch`]'s catch handler without binding
    /// first would double-evaluate a non-trivial right operand (e.g. a
    /// message send). So both operands are `let`-bound to fresh temporaries
    /// first, mirroring `guarded_op_doc`'s own `left_var`/`right_var` binding.
    fn number_coercion_bare_path(
        &mut self,
        op: &str,
        erlang_op: &'static str,
        left_code: Document<'static>,
        right_code: Document<'static>,
    ) -> Document<'static> {
        let left_var = self.fresh_temp_var("BinLeft");
        let right_var = self.fresh_temp_var("BinRight");
        let try_catch = self.number_coercion_try_catch(
            op,
            erlang_op,
            var(left_var.clone()),
            var(right_var.clone()),
        );
        docvec![
            "let ",
            var(left_var),
            " = ",
            left_code,
            " in let ",
            var(right_var),
            " = ",
            right_code,
            " in ",
            try_catch,
        ]
    }

    /// Generates `**` exponentiation via `math:pow/2` + `erlang:round/1`.
    ///
    /// Converts both operands to float for `math:pow`, then rounds the result
    /// back to integer for consistent integer exponentiation behavior.
    ///
    /// Note: `math:pow` uses IEEE 754 floats, so very large exponents (e.g.,
    /// `2 ** 100`) may lose precision. A future improvement could use repeated
    /// multiplication for exact arbitrary-precision integer results.
    fn generate_power_op(
        &mut self,
        left: &Expression,
        right: &Expression,
    ) -> Result<Document<'static>> {
        // BT-1937: Capture both operands preserving evaluation order.
        let exprs: [&Expression; 2] = [left, right];
        let (preamble, mut docs) = self.thread_subexprs(&exprs, "PowOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");
        let call_doc = docvec![
            "call 'erlang':'round'(call 'math':'pow'(call 'erlang':'float'(",
            left_code,
            "), call 'erlang':'float'(",
            right_code,
            ")))",
        ];
        Ok(self.close_prelude(&preamble, call_doc, "PowRes"))
    }

    /// Generates `++` concatenation with runtime type dispatch.
    ///
    /// Lists use `erlang:'++'`, strings use `iolist_to_binary`.
    /// When the receiver type is known at compile time (literal), we emit
    /// the optimal path directly. Otherwise, a runtime `is_list` check selects.
    fn generate_concat_op(
        &mut self,
        left: &Expression,
        right: &Expression,
    ) -> Result<Document<'static>> {
        use beamtalk_core::ast::Literal;

        // Compile-time optimization: detect known types from AST
        let is_list = matches!(
            left,
            Expression::ListLiteral { .. } | Expression::Literal(Literal::List(_), _)
        );
        let is_string = matches!(left, Expression::Literal(Literal::String(_), _));

        // BT-1937: Capture both operands preserving evaluation order. When
        // either operand has an open scope, BOTH are force-hoisted into the
        // preamble so left-to-right evaluation order is preserved.
        let exprs: [&Expression; 2] = [left, right];
        let (preamble, mut docs) = self.thread_subexprs(&exprs, "ConcatOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");

        let call_doc = if is_list {
            // List concatenation: erlang:'++'
            docvec!["call 'erlang':'++'(", left_code, ", ", right_code, ")",]
        } else if is_string {
            // String concatenation: iolist_to_binary
            docvec![
                "call 'erlang':'iolist_to_binary'([call 'erlang':'binary_to_list'(",
                left_code,
                "), call 'erlang':'binary_to_list'(",
                right_code,
                ")])",
            ]
        } else {
            // Runtime dispatch: check is_list at runtime.
            // CLAUDE.md: built entirely with Document/docvec!, no format!().
            let left_var = self.fresh_temp_var("ConcatLeft");
            let right_var = self.fresh_temp_var("ConcatRight");
            docvec![
                "let ",
                var(left_var.clone()),
                " = ",
                left_code,
                " in let ",
                var(right_var.clone()),
                " = ",
                right_code,
                " in case call 'erlang':'is_list'(",
                var(left_var.clone()),
                ") of <'true'> when 'true' -> call 'erlang':'++'(",
                var(left_var.clone()),
                ", ",
                var(right_var.clone()),
                ") <'false'> when 'true' -> \
                   call 'erlang':'iolist_to_binary'([call 'erlang':'binary_to_list'(",
                var(left_var),
                "), call 'erlang':'binary_to_list'(",
                var(right_var),
                ")]) end",
            ]
        };

        Ok(self.close_prelude(&preamble, call_doc, "ConcatRes"))
    }
}
