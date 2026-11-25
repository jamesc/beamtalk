// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary operator code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::exception_forms::{CatchVars, try_catch_doc};
use super::field_types::ComparisonKinds;
use super::patterns::guard_boolean_test;
use super::routing;
use super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::leaf::{atom, var};
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::number_coercion::{OperatorFamily, reflected_operator};
use beamtalk_core::source_analysis::Span;

/// Which runtime guard a dispatchable binary operator emits
/// for an unknown/generic receiver. Both guards share one shape — a
/// primitive-type test whose `'true'` branch is the bare Erlang BIF and whose
/// `'false'` branch is the message send — and differ only in the test.
#[derive(Clone, Copy)]
pub(in crate::core_erlang) enum OperatorGuard {
    /// `+ - * /`: `is_number` → bare BIF; anything else → message dispatch.
    Arithmetic,
    /// `< > <= >=`: primitive-comparable (`is_number` or `is_binary`) → bare
    /// BIF; anything else → message dispatch (BT-3924).
    Comparison,
    /// `< > <= >=` on a receiver whose kind is *declared* (a typed parameter
    /// or field) but not proven: the same shape as [`Self::Comparison`] with
    /// a test for exactly the declared kinds, so a value that violates the
    /// declaration (`nil` in an `:: Integer` parameter) dispatches and raises
    /// `does_not_understand` instead of term-ordering (BT-4823).
    DeclaredComparison(ComparisonKinds),
    /// `< > <= >=` on a receiver whose kind the compiler proves (a number,
    /// character or `String` literal, `self` in `Integer`/`Float`/
    /// `Character`/`String`) and whose right operand it does not prove of
    /// the same kind: no receiver test, only the right-operand test of ADR
    /// 0116 § Addendum. A number receiver sends a non-number its reflected
    /// comparison hook (BT-5176); a `String` receiver raises a `type_error`
    /// for anything but a `String` (BT-5213).
    Proven(ProvenKind),
}

impl OperatorGuard {
    /// The guard's runtime test on the receiver variable `subject`: `'true'`
    /// selects the bare BIF, `'false'` the message send. Built only from
    /// `erlang` type-test BIFs, so it is inline (no cross-module call).
    ///
    /// * Arithmetic — `is_number(L)`: non-numbers would `badarith` in the BIF,
    ///   so they dispatch (user overload or DNU).
    /// * Comparison — `is_number(L) or is_binary(L)`: exactly the receivers
    ///   whose builtin `<` *is* the Erlang term-order BIF — `Integer`/`Float`,
    ///   `Character` (a codepoint integer at runtime, see
    ///   `beamtalk_primitive:class_of/1`) and `String`/`Binary` (binaries).
    ///   Erlang's `<` is a total order over every term and never raises, so
    ///   any other receiver on the bare BIF would *silently* term-order it
    ///   (`nil < 5` used to answer `false`). Everything else therefore
    ///   dispatches: `nil`/booleans/lists/tuples raise `does_not_understand`,
    ///   a `Symbol` reaches `Symbol>><`, and objects reach their overload
    ///   (`Money>><`).
    fn predicate_doc(self, subject: &str) -> Document<'static> {
        let test = |bif: &'static str| form::call("erlang", bif, [var(subject.to_string())]);
        let either = |first: Document<'static>, second: Document<'static>| {
            form::call("erlang", "or", [first, second])
        };
        match self {
            Self::Arithmetic => test("is_number"),
            Self::Comparison | Self::DeclaredComparison(_) => {
                let (first, second) = self
                    .comparison_kind_tests()
                    .expect("a receiver-tested comparison guard has kind tests");
                match second {
                    Some(second) => either(test(first), test(second)),
                    None => test(first),
                }
            }
            // No receiver test: the receiver's kind is proven. (Its guard is
            // the right-operand test, `right_tested_comparison`.)
            Self::Proven(_) => atom("true"),
        }
    }

    /// The comparison guard's receiver tests split by kind (ADR 0116
    /// § Addendum, BT-5176): the number arm's test, which gains the
    /// right-operand test, and the `String` arm's, which does not. `None`
    /// for a guard with no number arm (a declared `String`) or no receiver
    /// test at all ([`Self::Proven`]), and for arithmetic.
    fn number_and_string_tests(self) -> Option<(&'static str, Option<&'static str>)> {
        match self {
            Self::DeclaredComparison(kinds) if !kinds.has_number_kind() => None,
            _ => self.comparison_kind_tests(),
        }
    }

    /// The comparison guard's receiver kind tests: the first, and a second
    /// to `or` with it (always `is_binary` when present). The one table
    /// behind [`Self::predicate_doc`] and [`Self::number_and_string_tests`].
    fn comparison_kind_tests(self) -> Option<(&'static str, Option<&'static str>)> {
        match self {
            Self::Comparison => Some(("is_number", Some("is_binary"))),
            Self::DeclaredComparison(kinds) => Some(kinds.type_tests()),
            Self::Arithmetic | Self::Proven(_) => None,
        }
    }
}

// The structurally lowered binary selectors live in `beamtalk-core`
// (`state_threading_selectors`), below both codegen and the block analysis that
// classifies which sends may run user code (ADR 0137).
pub(in crate::core_erlang) use beamtalk_core::state_threading_selectors::{
    StructuralBinary, structural_binary,
};

/// Whether the binary selector `op` is lowered structurally by the compiler
/// ([`structural_binary`]) rather than dispatched as a message. Every other
/// binary selector (arithmetic, comparison, user-defined) dispatches.
/// BT-3985: the one predicate behind `binary_self_send_dispatches` (a binary
/// selector sent to `self`).
pub(in crate::core_erlang) fn is_structurally_lowered_binary(op: &str) -> bool {
    structural_binary(op).is_some()
}

/// Whether `op` is an ordering comparison (`< > <= >=`): a dispatchable
/// message whose builtin is the Erlang comparison BIF.
pub(in crate::core_erlang) fn is_ordering_comparison(op: &str) -> bool {
    OperatorFamily::of(op) == Some(OperatorFamily::Ordering)
}

/// ADR 0116 § Addendum: the `beamtalk_message_dispatch` function a
/// number-on-the-left fallback of `family` calls.
fn number_fallback_entry_point(family: OperatorFamily) -> &'static str {
    match family {
        OperatorFamily::Arithmetic => "send_number_coercion",
        OperatorFamily::Ordering => "send_number_comparison",
    }
}

/// ADR 0116: the one call that sends the right operand `right` of `left op
/// right` its reflected selector, through the family's runtime entry point:
/// `beamtalk_message_dispatch:<entry>(Right, '<selector>', [Left], '<op>')`.
/// Shared by the inlined path ([`CoreErlangGenerator::number_comparison_arm`])
/// and the primitive bodies (`primitives::generate_ordering_comparison_bif`).
pub(in crate::core_erlang) fn reflected_hook_call(
    reflected: beamtalk_core::semantic_analysis::number_coercion::ReflectedOperator,
    op: &str,
    right: Document<'static>,
    left: Document<'static>,
) -> Document<'static> {
    form::call(
        "beamtalk_message_dispatch",
        number_fallback_entry_point(reflected.family),
        [
            right,
            atom(reflected.selector),
            docvec!["[", left, "]"],
            atom(op.to_string()),
        ],
    )
}

/// The kind an ordering comparison's right operand must have for the bare
/// BIF to answer (ADR 0116 § Addendum): the receiver's own kind. Erlang's
/// `<` is a total order over every term and never raises, so a right operand
/// of any other kind is tested for and never reaches the BIF.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum OrderingKind {
    /// `Integer`, `Float`, `Character`: any number (a `Character` is a
    /// codepoint integer). Anything else is sent its reflected hook
    /// (BT-5176).
    Number,
    /// `String`: the tagged `{beamtalk_string, Bin}` term (BT-4170).
    /// Anything else raises a `type_error` (BT-5213).
    String,
    /// `Symbol`: an atom other than `nil`, `true` and `false` (which are
    /// `UndefinedObject` and `Boolean`). Anything else raises a `type_error`
    /// (BT-5213).
    Symbol,
}

impl OrderingKind {
    /// The inline test that `v` is of this kind, a guard-safe boolean: no
    /// call, so the arm it selects stays frame-free. `String` and `Symbol`
    /// use their classes' routing guards (the shared fixture's
    /// [`routing::type_test`], in expression form), so "is a `String`"
    /// has one definition.
    pub(in crate::core_erlang) fn test_doc(self, v: &str) -> Document<'static> {
        let class = match self {
            Self::Number => return form::call("erlang", "is_number", [var(v.to_string())]),
            Self::String => "String",
            Self::Symbol => "Symbol",
        };
        routing::type_test(class)
            .and_then(|guard| guard_boolean_test(guard, v))
            .unwrap_or_else(|| unreachable!("{class} has a boolean routing guard"))
    }
}

impl From<ProvenKind> for OrderingKind {
    fn from(kind: ProvenKind) -> Self {
        match kind {
            ProvenKind::Number => Self::Number,
            ProvenKind::String => Self::String,
        }
    }
}

/// The head of the raw error term [`ordering_type_error_raise`] raises. The
/// runtime names it `?BT_ORDERING_TYPE_ERROR_TAG` (`beamtalk.hrl`), and
/// `beamtalk_exception_handler:wrap_raw/2` turns the term into the
/// structured `type_error`; `stdlib/test/string_symbol_comparison_test.bt`
/// asserts that error's kind and hint, so the two names cannot drift apart
/// unnoticed.
pub(in crate::core_erlang) const ORDERING_TYPE_ERROR_TAG: &str = "beamtalk_ordering_type_error";

/// BT-5213: the one expression that raises the `type_error` of a `String`
/// or `Symbol` ordering comparison whose right operand is of another kind:
/// `erlang:error({'beamtalk_ordering_type_error', Left, '<op>', Right})`.
/// The arm is frame-free, as the owner chose (no reflected hook, ADR 0116
/// § The failing arm): `erlang:error/1` never returns, so a site whose other
/// arm is the bare BIF stays call-free. The runtime builds the error, its
/// message and hint when the raw term is caught
/// (`beamtalk_exception_handler:wrap_raw/2`). Shared by the inlined path (a
/// proven `String` receiver) and the `@primitive` bodies of `String` and
/// `Symbol` (`primitives::generate_ordering_comparison_bif`).
pub(in crate::core_erlang) fn ordering_type_error_raise(
    op: &str,
    left: Document<'static>,
    right: Document<'static>,
) -> Document<'static> {
    form::call(
        "erlang",
        "error",
        [form::tuple([
            atom(ORDERING_TYPE_ERROR_TAG),
            left,
            atom(op.to_string()),
            right,
        ])],
    )
}

/// Whether `op` is a coercing arithmetic operator (`+ - * /`, ADR 0116):
/// `%` and `**` are not.
pub(in crate::core_erlang) fn is_arithmetic_operator(op: &str) -> bool {
    OperatorFamily::of(op) == Some(OperatorFamily::Arithmetic)
}

/// What the compiler knows about a comparison receiver's kind
/// ([`CoreErlangGenerator::comparison_receiver`], BT-4823).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum ComparisonReceiver {
    /// Proven by the compiler (a literal, `self` in a builtin class), of
    /// this kind ([`proven_comparison_kind`]).
    Proven(ProvenKind),
    /// Declared but not proven (a typed parameter or field): kind-tested.
    Declared(ComparisonKinds),
    /// Not known: the general primitive-comparable guard.
    Unknown,
}

/// The kind of a comparison operand the compiler *proves*, from its syntax
/// and the class being compiled alone ([`proven_comparison_kind`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum ProvenKind {
    /// A number at run time: an `Integer`, `Float` or `Character` (a
    /// codepoint integer) literal, or `self` in one of those classes.
    Number,
    /// A `String`: a `String` literal (the tagged `{beamtalk_string, Bin}`
    /// tuple at run time, BT-4170, which the bare BIF term-orders as a
    /// tuple), or `self` in `String`.
    String,
}

/// The kind the compiler proves a comparison operand `expr` has, or `None`
/// when only a declaration (or nothing) says so. `class_name` is the class
/// being compiled, for `self`; `None` proves nothing about `self`.
///
/// Only a proof counts. A declared operand (`n :: Integer`) is not proven:
/// a declaration is not enforced at run time and an ordering BIF cannot
/// raise on a wrong kind, so it would answer silently (BT-4823 for the
/// receiver, ADR 0116 § Addendum "Elision: proven, not declared" for the
/// right operand). This is deliberately not
/// [`CoreErlangGenerator::receiver_is_statically_numeric`], which trusts
/// declarations because arithmetic still raises `badarith` on a wrong one.
pub(in crate::core_erlang) fn proven_comparison_kind(
    expr: &Expression,
    class_name: Option<&str>,
) -> Option<ProvenKind> {
    use beamtalk_core::ast::Literal;
    match expr {
        Expression::Literal(
            Literal::Integer(_)
            | Literal::BigInteger(_)
            | Literal::Float(_)
            | Literal::Character(_),
            _,
        ) => Some(ProvenKind::Number),
        Expression::Literal(Literal::String(_), _) => Some(ProvenKind::String),
        _ if expr.is_self() => match class_name? {
            "Integer" | "Float" | "Character" => Some(ProvenKind::Number),
            "String" => Some(ProvenKind::String),
            _ => None,
        },
        _ => None,
    }
}

/// "Proven numeric" (ADR 0116 § Addendum, BT-5176): whether the compiler
/// proves `expr` is a number, so a numeric comparison with it on the right
/// needs no right-operand test.
pub(in crate::core_erlang) fn is_proven_numeric(
    expr: &Expression,
    class_name: Option<&str>,
) -> bool {
    proven_comparison_kind(expr, class_name) == Some(ProvenKind::Number)
}

/// The one "this comparison is bare and answers a Boolean" predicate (ADR
/// 0116 § Addendum, BT-5176): whether `left op right`, an ordering
/// comparison (`< > <= >=`), runs no user code and answers `true` or
/// `false` — or raises, which is neither user code nor a non-Boolean answer.
/// That needs a proven receiver. A proven number receiver also needs a
/// proven numeric right operand: an unproven one is tested and, when it is
/// not a number, sent its reflected hook (`lessThanFromNumber:` …), which
/// runs user code and may answer anything. A proven `String` receiver holds
/// for any right operand: an unproven one is tested too, but a non-`String`
/// raises a frame-free `type_error` (BT-5213), so it stays call-free.
///
/// Read by every site that relies on a Boolean, call-free comparison:
/// `generate_binary_op`'s BT-4452 lowering report, `classify_binary`
/// (`synced_call.rs`) and the guard-return rewrite's `typed_comparison`
/// (`guard_returns.rs`). [`CoreErlangGenerator::operator_guard`] decides
/// the tests themselves.
pub(in crate::core_erlang) fn comparison_is_bare(
    left: &Expression,
    right: &Expression,
    class_name: Option<&str>,
) -> bool {
    match proven_comparison_kind(left, class_name) {
        Some(ProvenKind::String) => true,
        Some(ProvenKind::Number) => is_proven_numeric(right, class_name),
        None => false,
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

        // The structurally lowered operators (`structural_binary`, the one table).
        let structural = structural_binary(op);
        match structural {
            // `**` uses math:pow (no direct Erlang operator)
            Some(StructuralBinary::Power) => {
                return self.generate_power_op(left, &arguments[0]);
            }
            // `++` works on both lists and strings
            Some(StructuralBinary::Concat) => {
                return self.generate_concat_op(left, &arguments[0]);
            }
            // `>>` is lowered by `generate_message_send` before it gets here.
            Some(StructuralBinary::MethodLookup) => {
                return Err(CodeGenError::Internal(
                    "`>>` is lowered by generate_message_send, not generate_binary_op".to_string(),
                ));
            }
            Some(StructuralBinary::Bif(_)) | None => {}
        }

        let erlang_op = match (structural, op) {
            (Some(StructuralBinary::Bif(bif)), _) => bif,
            (_, "+") => "+",
            (_, "-") => "-",
            (_, "*") => "*",
            (_, "/") => "/",
            (_, "<") => "<",
            (_, ">") => ">",
            (_, "<=") => "=<",
            (_, ">=") => ">=",
            _ => {
                let right = &arguments[0];
                return Err(CodeGenError::UnsupportedFeature {
                    feature: format!("binary operator: {op}"),
                    span: Some(Span::new(left.span().start(), right.span().end())),
                });
            }
        };

        // Arithmetic (`+ - * /`) and comparison (`< > <= >=`)
        // operators are dispatchable messages so user value-types can overload
        // them. When the receiver is *statically* known to be a builtin (a
        // literal, `self` inside a builtin class, or a suitably-annotated
        // parameter) we keep the bare BIF fast path (zero cost — all stdlib hot
        // paths). Otherwise we emit a runtime guard.
        //
        // Both guards test for the primitive types whose builtin operator *is*
        // the bare BIF and dispatch everything else:
        //   * Arithmetic — `is_number`: number→BIF, anything else→dispatch.
        //   * Comparison — `is_number or is_binary` (numbers, characters,
        //     binaries): Erlang `<` is a total order over every term and never
        //     raises, so a bare BIF on any other receiver would *silently*
        //     term-order it (`nil < 5` → `false`). Every other receiver —
        //     objects, but also `nil`, symbols, lists, tuples (including
        //     tagged `String`s, BT-4170 — dispatch term-compares them with
        //     the same outcome, one hop slower) — dispatches, so
        //     it reaches its own `<` or raises `does_not_understand` (BT-3924).
        // `%`/`**`/equality are not message-dispatched here.
        let is_arithmetic = is_arithmetic_operator(op);
        let guard = self.operator_guard(op, left, &arguments[0]);
        // BT-4452: the guard-return rewrite took some comparisons for
        // Booleans by construction; report whether this one is, from the one
        // shared predicate (ADR 0116 § Addendum, BT-5176) — not from
        // `guard.is_none()`, which reads only the receiver side.
        let bare = if is_ordering_comparison(op) {
            self.comparison_is_bare(left, &arguments[0])
        } else {
            guard.is_none()
        };
        self.record_comparison_lowering(left, bare);

        // ADR 0116: number-on-the-left coercion. Every arithmetic call
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

        // Capture both operands in evaluation order. When either
        // operand needs a real prelude (e.g., a class method self-send
        // mutating a class var), `thread_subexprs` force-hoists BOTH
        // operands into that preamble so left-to-right evaluation order is
        // preserved. When neither needs one, both operands stay inline and
        // there is no hoisting overhead.
        let exprs: [&Expression; 2] = [left, &arguments[0]];
        let (preamble, mut docs) = self.thread_subexprs(&exprs, "BinOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");

        // ADR 0116 § Addendum (BT-5176): an ordering comparison tests its
        // right operand unless the compiler proves it numeric. A declared
        // right operand (`n :: Integer`) is tested, not trusted. A proven
        // receiver is guarded only when its right operand is not proven of
        // its kind (`"a" < 5`, BT-5213), so it is always tested.
        let right_tested = is_ordering_comparison(op)
            && (matches!(guard, Some(OperatorGuard::Proven(_)))
                || !self.is_proven_numeric(&arguments[0]));

        let call_doc = if let Some(guard) = guard {
            let parts = self.guarded_op_doc(
                guard,
                op,
                erlang_op,
                left_code,
                right_code,
                RightOperand {
                    statically_numeric: right_is_statically_numeric,
                    tested: right_tested,
                },
            );
            // BT-4279: when the guard's true arm is the bare BIF (it runs no
            // user code) and the operands needed no prelude, the bracket
            // around this send skips its store and reload on that arm.
            if let Some((test, fast)) = parts.fast_path.clone()
                && preamble.is_empty()
            {
                self.offer_fast_path(
                    left,
                    parts.bindings.clone(),
                    test,
                    fast,
                    parts.case_doc.clone(),
                );
            }
            docvec![parts.bindings, parts.case_doc]
        } else if is_arithmetic && !right_is_statically_numeric {
            // ADR 0116: left is statically numeric but the right
            // operand's type is genuinely unknown — wrap the bare BIF in the
            // badarith-catching number-on-the-left coercion mechanism instead
            // of emitting it unguarded.
            self.number_coercion_bare_path(op, erlang_op, left_code, right_code)
        } else {
            // AGENTS.md: Core Erlang fragments MUST use Document/docvec!, never
            // format!(). erlang_op is one of the static literals in the match
            // arms above, so Document::Str is safe.
            // BT-4836: annotate the BIF node (see `guarded_op_doc`).
            self.annotate_current_send_call(docvec![
                "call 'erlang':'",
                Document::Str(erlang_op),
                "'(",
                left_code,
                ", ",
                right_code,
                ")",
            ])
        };

        Ok(self.close_prelude(&preamble, call_doc, "BinOp"))
    }

    /// The runtime guard the dispatchable operator `op` needs when its receiver
    /// `left` is not statically a builtin, or `None` when the bare BIF is right
    /// (a literal or suitably typed receiver, or an operator that is not message
    /// dispatched). The one rule behind `generate_binary_op` and the cell-bracket
    /// classification (`classify_binary`): the guard's dispatching branch is where
    /// user code can run.
    ///
    /// For a comparison, `None` exactly when both operands are proven of the
    /// same kind: a proven receiver whose right operand is not gets
    /// [`OperatorGuard::Proven`], the right-operand test (ADR 0116
    /// § Addendum, BT-5176, BT-5213).
    pub(in crate::core_erlang) fn operator_guard(
        &self,
        op: &str,
        left: &Expression,
        right: &Expression,
    ) -> Option<OperatorGuard> {
        let is_arithmetic = is_arithmetic_operator(op);
        let is_comparison = is_ordering_comparison(op);
        if is_arithmetic && !self.receiver_is_statically_numeric(left) {
            Some(OperatorGuard::Arithmetic)
        } else if is_comparison {
            match self.comparison_receiver(left) {
                ComparisonReceiver::Proven(kind) => {
                    let class = self.self_instance_class();
                    (proven_comparison_kind(right, class.as_deref()) != Some(kind))
                        .then_some(OperatorGuard::Proven(kind))
                }
                ComparisonReceiver::Declared(kinds) => {
                    Some(OperatorGuard::DeclaredComparison(kinds))
                }
                ComparisonReceiver::Unknown => Some(OperatorGuard::Comparison),
            }
        } else {
            None
        }
    }

    /// [`comparison_is_bare`] in the class being compiled.
    pub(in crate::core_erlang) fn comparison_is_bare(
        &self,
        left: &Expression,
        right: &Expression,
    ) -> bool {
        comparison_is_bare(left, right, self.self_instance_class().as_deref())
    }

    /// [`is_proven_numeric`] in the class being compiled.
    pub(in crate::core_erlang) fn is_proven_numeric(&self, expr: &Expression) -> bool {
        is_proven_numeric(expr, self.self_instance_class().as_deref())
    }

    /// The class whose instance `self` is, for proving `self`'s kind: the
    /// class being compiled in an instance method, `None` in a class method
    /// (where `self` is the class object, never a number or string).
    fn self_instance_class(&self) -> Option<String> {
        (!self.in_class_method()).then(|| self.class_name())
    }

    /// Whether `expr` is statically known to evaluate to a number, so
    /// the arithmetic fast path can skip the runtime `is_number` guard.
    ///
    /// Mirrors the gradual-typing contract: an annotation (or a syntactic fact)
    /// *removes* the guard; its absence keeps the guard, which is always correct.
    /// Recognises sources of numeric certainty already present in the
    /// AST / codegen context — no type-inference pass:
    /// * a numeric literal receiver (`1 + x`),
    /// * `self` inside a numeric class (`Integer`/`Float` method bodies),
    /// * an identifier bound to a `:: Integer/Float/Number` parameter whose
    ///   name the method body never rebinds (BT-3921), and
    /// * a `self.<field>` read whose field is known numeric
    ///   ([`FieldTypeFact`](crate::core_erlang::field_types::FieldTypeFact)).
    ///
    /// A field is known numeric only when its declared type is numeric (a
    /// union counts when every non-`Nil` member is). An **untyped** field is
    /// never trusted, whatever its default: `spawnWith:`, a keyword
    /// constructor, a subclass or an extension can store an object in it
    /// (BT-3925). So `state: count :: Integer = 0` keeps
    /// `self.count := self.count + 1` on the bare BIF, while an untyped field
    /// (`state: count = 0`, `field: p = nil` set to a `Point` in `setUp`), a
    /// union with an object member (`:: Point | Nil`), or an explicit object
    /// type (`:: Money`) routes through the `is_number` guard so `self.p + x`
    /// dispatches to the object's `+` (BT-3892). The guard only wraps the
    /// operator: an `ifTrue:` on a guarded comparison still compiles to the
    /// same inline state-threading `case`, and the guarded Actor counter
    /// measured no slower than the bare one (BT-3925,
    /// `docs/development/benchmarks.md`).
    ///
    /// ADR 0116 reuses this same predicate, unmodified, to gate the
    /// number-on-the-left coercion mechanism's *right* operand — per the
    /// ADR's own "Trigger condition, refined" spec ("a numeric/untyped field —
    /// the exact same rule already applied to the left operand"). Since an
    /// untyped field is never trusted (BT-3892, BT-3925), an untyped
    /// `self.extra` in `self.total + self.extra` reaches the `try`/`catch` and a
    /// non-numeric `extra` dispatches to `plusFromNumber:` (closing the
    /// BT-3266 gap).
    pub(in crate::core_erlang) fn receiver_is_statically_numeric(&self, expr: &Expression) -> bool {
        use beamtalk_core::ast::Literal;
        match expr {
            Expression::Literal(
                Literal::Integer(_) | Literal::BigInteger(_) | Literal::Float(_),
                _,
            ) => true,
            _ if expr.is_self() => matches!(
                self.self_instance_class().as_deref(),
                Some("Integer" | "Float")
            ),
            Expression::Identifier(id) => self.param_is_numeric(&id.name),
            // `self.<field>` stays bare only when the field is known numeric
            // (see doc above); every other field is guarded.
            Expression::FieldAccess {
                receiver, field, ..
            } => receiver.is_self() && self.field_is_numeric(&field.name),
            _ => false,
        }
    }

    /// How much the compiler knows about the kind of a comparison receiver
    /// `expr` (`< > <= >=`), which decides the guard in front of the bare
    /// comparison BIF. Erlang's `<` is a total order over every term and never
    /// raises, so a bare BIF on a value of the wrong kind answers silently and
    /// wrongly (`nil > 10` → `true`); only a *proven* kind may skip the guard.
    ///
    /// * [`ComparisonReceiver::Proven`] — the compiler knows the kind: a
    ///   numeric / character / string literal, or `self` inside a builtin
    ///   comparable class (`Integer`/`Float`/`Character`/`String`)
    ///   ([`proven_comparison_kind`]). No receiver test.
    /// * [`ComparisonReceiver::Declared`] — only a declaration says so, and a
    ///   declared type is not enforced at runtime (a `Dynamic` argument, an
    ///   FFI result or a cast can violate it, BT-4823): an identifier bound to
    ///   a `:: Integer/Float/Number/Character/String` parameter whose name the
    ///   method body never rebinds (BT-3921), or a `self.<field>` read whose
    ///   field is declared with such a type (BT-3892, BT-3925; unlike
    ///   arithmetic, no union with a `Nil` member, BT-3924). The guard tests
    ///   for the declared kinds.
    /// * [`ComparisonReceiver::Unknown`] — everything else, untyped and
    ///   object-typed fields included: the general primitive-comparable guard.
    ///
    /// This is the receiver side only. The right operand of a numeric
    /// comparison is tested separately unless proven numeric
    /// ([`is_proven_numeric`], ADR 0116 § Addendum, BT-5176).
    pub(in crate::core_erlang) fn comparison_receiver(
        &self,
        expr: &Expression,
    ) -> ComparisonReceiver {
        if let Some(kind) = proven_comparison_kind(expr, self.self_instance_class().as_deref()) {
            return ComparisonReceiver::Proven(kind);
        }
        match expr {
            _ if expr.is_self() => ComparisonReceiver::Unknown,
            Expression::Identifier(id) => self
                .param_comparison_kinds(&id.name)
                .map_or(ComparisonReceiver::Unknown, ComparisonReceiver::Declared),
            Expression::FieldAccess {
                receiver, field, ..
            } if receiver.is_self() => self
                .field_comparison_kinds(&field.name)
                .map_or(ComparisonReceiver::Unknown, ComparisonReceiver::Declared),
            _ => ComparisonReceiver::Unknown,
        }
    }

    /// Builds a runtime-guarded operator dispatch for an
    /// unknown/generic receiver, mirroring `generate_concat_op`'s `is_list` arm.
    /// One helper serves both arithmetic and comparison; the [`OperatorGuard`]
    /// selects the primitive-type test ([`OperatorGuard::predicate_doc`]):
    ///
    /// ```erlang
    /// let <Lhs> = <left> in
    ///   let <Rhs> = <right> in
    ///     case <primitive test on Lhs> of
    ///       <'true'>  when 'true' -> <bare BIF>
    ///       <'false'> when 'true' -> <message send>
    ///     end
    /// ```
    ///
    /// * Arithmetic (`is_number`): numbers take the BIF; everything else routes
    ///   through dispatch (overload or DNU).
    /// * Comparison (`is_number or is_binary`): numbers, characters and strings
    ///   take the BIF; everything else dispatches (so `aMoney < bMoney` reaches
    ///   `Money>><`, `#a < #b` reaches `Symbol>><`, and `nil < 5` raises a DNU
    ///   instead of silently term-ordering — BT-3924).
    ///
    /// `erlang_op` and the predicate BIF names are static literals (safe as
    /// `Document::Str`); the dispatch selector uses `leaf::atom` for the original
    /// Beamtalk operator.
    ///
    /// ADR 0116: for the `Arithmetic` guard specifically, the
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
    /// This `case` matches only `<'true'>`/`<'false'>`, no wildcard —
    /// the same non-exhaustive-to-the-compiler shape `case_clause_fallback`
    /// exists for (see its doc comment and ADR 0111 Addendum 5, "Production
    /// bugs found", bug 3). It was confirmed empirically reachable for the
    /// comparison guard when that guard called `beamtalk_primitive:is_object/1`
    /// (a plain function the Core Erlang compiler cannot prove exhaustive) as a
    /// try body's last statement, e.g. `[... . a < b] ensure: [...]` with
    /// `a`/`b` not statically comparable: erlc's `ambiguous_catch_try_state`
    /// `beam_validator` bug. Both guards now test with `erlang` type-test BIFs,
    /// whose boolean result the compiler's BIF return-type inference can
    /// usually prove exhaustive without a wildcard — but the explicit fallback
    /// is kept on both uniformly (cheap, behavior-preserving, and doesn't
    /// depend on that inference holding in some future OTP release).
    fn guarded_op_doc(
        &mut self,
        guard: OperatorGuard,
        op: &str,
        erlang_op: &'static str,
        left_code: Document<'static>,
        right_code: Document<'static>,
        right: RightOperand,
    ) -> GuardedOpParts {
        let RightOperand {
            statically_numeric: right_is_statically_numeric,
            tested: right_tested,
        } = right;
        let left_var = self.fresh_temp_var("BinLeft");
        let right_var = self.fresh_temp_var("BinRight");
        let bindings = docvec![
            form::let_prefix(var(left_var.clone()), left_code),
            form::let_prefix(var(right_var.clone()), right_code),
        ];

        // BT-4836: the BIF node itself carries the send's line — the OTP
        // compiler takes a BIF failure's frame location from the `call`
        // node, never from an annotation on an enclosing `let`/`case`.
        let bare_bif = self.annotate_current_send_call(docvec![
            "call 'erlang':'",
            Document::Str(erlang_op),
            "'(",
            var(left_var.clone()),
            ", ",
            var(right_var.clone()),
            ")",
        ]);
        // ADR 0116 § Addendum (BT-5176): a numeric comparison whose right
        // operand is not proven numeric tests it on its number arm.
        if right_tested
            && let Some(parts) =
                self.right_tested_comparison(guard, op, &left_var, &right_var, &bare_bif)
        {
            return GuardedOpParts {
                bindings,
                case_doc: parts.0,
                fast_path: Some(parts.1),
            };
        }
        // ADR 0116: see this function's doc comment — the arithmetic
        // guard's bare-BIF branch becomes the coercion `try`/`catch` when the
        // right operand's type is genuinely unknown; every other case (a
        // comparison with a proven numeric right operand, a declared `String`,
        // or a statically-numeric right operand) keeps the bare BIF unchanged.
        let coerces = matches!(guard, OperatorGuard::Arithmetic) && !right_is_statically_numeric;
        let bif_branch = if coerces {
            self.number_coercion_try_catch(
                op,
                erlang_op,
                var(left_var.clone()),
                var(right_var.clone()),
            )
        } else {
            bare_bif.clone()
        };
        // BT-4366: nested in the guard `case`, so the node carries the line.
        let send_branch =
            self.runtime_send_call(var(left_var.clone()), op, vec![var(right_var.clone())]);
        let predicate = guard.predicate_doc(&left_var);
        // Explicit wildcard so this boolean `case` is statically
        // exhaustive — see this function's doc comment and
        // `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("BinOpNoMatch");

        // BT-4279: the fast path, a test under which the send runs no user
        // code and the value then. The bare BIF under the guard's own test;
        // under the coercion `try`/`catch` (whose `catch` dispatches a
        // reflected selector) only integer `+ - *`, which never raise.
        let fast_path = if !coerces {
            Some((predicate.clone(), bare_bif))
        } else if matches!(op, "+" | "-" | "*") {
            let is_integer =
                |v: &str| docvec!["call 'erlang':'is_integer'(", var(v.to_string()), ")"];
            Some((
                docvec![
                    "call 'erlang':'and'(",
                    is_integer(&left_var),
                    ", ",
                    is_integer(&right_var),
                    ")",
                ],
                bare_bif,
            ))
        } else {
            None
        };
        GuardedOpParts {
            bindings,
            case_doc: docvec![
                "case ",
                predicate.clone(),
                " of <'true'> when 'true' -> ",
                bif_branch,
                " <'false'> when 'true' -> ",
                send_branch,
                no_match_fallback,
                " end",
            ],
            fast_path,
        }
    }
}

impl CoreErlangGenerator {
    /// ADR 0116 § Addendum (BT-5176): the guard `case` of an ordering
    /// comparison whose right operand is tested, and its BT-4279 fast path
    /// `(test, bare BIF)`. `None` for a guard with no number arm (a declared
    /// `String`, which is unchanged), so the caller keeps its shape.
    ///
    /// The number arm is [`Self::number_comparison_arm`]. Around it, by
    /// receiver:
    ///
    /// * proven number — the number arm alone;
    /// * proven `String` — the bare BIF when the right operand is a `String`,
    ///   else [`ordering_type_error_raise`] (BT-5213);
    /// * unknown, or declared with a number kind (a union with `String`
    ///   too) — split by kind:
    ///
    /// ```erlang
    /// case call 'erlang':'is_number'(BinLeft) of      %% the declared kind's test
    ///   <'true'> when 'true' -> <number arm>
    ///   <'false'> when 'true' ->
    ///     case call 'erlang':'is_binary'(BinLeft) of  %% only when a String is held
    ///       <'true'> when 'true' -> <bare BIF>        %% String receiver: unchanged
    ///       <'false'> when 'true' -> <dispatch op>    %% BT-3924
    ///     end
    /// end
    /// ```
    ///
    /// The fast path's test implies the bare BIF arm: the right operand's
    /// [`OrderingKind::test_doc`] for a proven receiver; otherwise `<kind test>(L) and is_number(R)`, `or`
    /// `is_binary(L)` when a `String` is held.
    fn right_tested_comparison(
        &mut self,
        guard: OperatorGuard,
        op: &str,
        left_var: &str,
        right_var: &str,
        bare_bif: &Document<'static>,
    ) -> Option<(Document<'static>, (Document<'static>, Document<'static>))> {
        let test = |bif: &'static str, v: &str| form::call("erlang", bif, [var(v.to_string())]);
        let right_is_number = test("is_number", right_var);
        if let OperatorGuard::Proven(kind) = guard {
            let arm = match kind {
                ProvenKind::Number => {
                    self.number_comparison_arm(op, left_var, right_var, bare_bif.clone())
                }
                ProvenKind::String => {
                    self.wrong_kind_raising_arm(op, left_var, right_var, bare_bif.clone())
                }
            };
            let kind_test = OrderingKind::from(kind).test_doc(right_var);
            return Some((arm, (kind_test, bare_bif.clone())));
        }
        let (number_test, string_test) = guard.number_and_string_tests()?;
        let number_arm = self.number_comparison_arm(op, left_var, right_var, bare_bif.clone());
        let dispatch = self.runtime_send_call(
            var(left_var.to_string()),
            op,
            vec![var(right_var.to_string())],
        );
        let boolean_case = |this: &mut Self, scrutinee, on_true, on_false| {
            let fallback = this.fresh_temp_var("BinOpNoMatch");
            form::case_of(
                scrutinee,
                [
                    form::clause([atom("true")], None, on_true, form::Layout::Flat),
                    form::clause([atom("false")], None, on_false, form::Layout::Flat),
                    Self::case_clause_fallback_clause(fallback),
                ],
                form::Layout::Flat,
            )
        };
        let not_a_number = match string_test {
            Some(string_test) => boolean_case(
                self,
                test(string_test, left_var),
                bare_bif.clone(),
                dispatch,
            ),
            None => dispatch,
        };
        let case_doc = boolean_case(self, test(number_test, left_var), number_arm, not_a_number);
        let number_fast = form::call(
            "erlang",
            "and",
            [test(number_test, left_var), right_is_number],
        );
        let fast_test = match string_test {
            Some(string_test) => {
                form::call("erlang", "or", [number_fast, test(string_test, left_var)])
            }
            None => number_fast,
        };
        Some((case_doc, (fast_test, bare_bif.clone())))
    }

    /// The number arm of a right-tested ordering comparison (ADR 0116
    /// § Addendum, BT-5176): the bare BIF when the right operand is a
    /// number, else one call to the runtime helper, which sends the right
    /// operand its reflected hook (`5 < x` → `x lessThanFromNumber: 5`) and
    /// raises a hinted `does_not_understand` when it has none.
    ///
    /// ```erlang
    /// case call 'erlang':'is_number'(BinRight) of
    ///   <'true'> when 'true' -> call 'erlang':'<'(BinLeft, BinRight)
    ///   <'false'> when 'true' ->
    ///     call 'beamtalk_message_dispatch':'send_number_comparison'(
    ///         BinRight, 'lessThanFromNumber:', [BinLeft], '<')
    /// end
    /// ```
    fn number_comparison_arm(
        &mut self,
        op: &str,
        left_var: &str,
        right_var: &str,
        bare_bif: Document<'static>,
    ) -> Document<'static> {
        // The shared reflected-selector table (Phase 2a); callers only reach
        // this for `< > <= >=`.
        let reflected = reflected_operator(op)
            .filter(|r| r.family == OperatorFamily::Ordering)
            .unwrap_or_else(|| {
                unreachable!("number_comparison_arm called for a non-ordering operator: {op}")
            });
        // BT-4366: nested in a `case`, so the call node carries the line.
        let hook = self.annotate_current_send_call(reflected_hook_call(
            reflected,
            op,
            var(right_var.to_string()),
            var(left_var.to_string()),
        ));
        let fallback = self.fresh_temp_var("BinOpNoMatch");
        right_tested_comparison(OrderingKind::Number, right_var, bare_bif, hook, fallback)
    }

    /// The arm of a right-tested `String` comparison (BT-5213): the bare BIF
    /// when the right operand is a `String`, else
    /// [`ordering_type_error_raise`] (a frame-free `type_error`).
    fn wrong_kind_raising_arm(
        &mut self,
        op: &str,
        left_var: &str,
        right_var: &str,
        bare_bif: Document<'static>,
    ) -> Document<'static> {
        // BT-4366: nested in a `case`, so the call node carries the line.
        let raise = self.annotate_current_send_call(ordering_type_error_raise(
            op,
            var(left_var.to_string()),
            var(right_var.to_string()),
        ));
        let fallback = self.fresh_temp_var("BinOpNoMatch");
        right_tested_comparison(OrderingKind::String, right_var, bare_bif, raise, fallback)
    }
}

/// The one right-operand test of an ordering comparison (ADR 0116
/// § Addendum): `bare_bif` when `right_var` is of `kind`
/// ([`OrderingKind::test_doc`]), else `wrong_kind` — a
/// [`reflected_hook_call`] for a number (BT-5176), an
/// [`ordering_type_error_raise`] for a `String` or `Symbol` (BT-5213). Built
/// by both send paths: the inlined one
/// ([`CoreErlangGenerator::number_comparison_arm`],
/// [`CoreErlangGenerator::wrong_kind_raising_arm`]) and the `@primitive`
/// bodies of `Integer`, `Float`, `Character`, `String` and `Symbol`
/// (`primitives::generate_ordering_comparison_bif`, BT-5211), so the two
/// agree.
pub(in crate::core_erlang) fn right_tested_comparison(
    kind: OrderingKind,
    right_var: &str,
    bare_bif: Document<'static>,
    wrong_kind: Document<'static>,
    fallback_var: String,
) -> Document<'static> {
    form::case_of(
        kind.test_doc(right_var),
        [
            form::clause([atom("true")], None, bare_bif, form::Layout::Flat),
            form::clause([atom("false")], None, wrong_kind, form::Layout::Flat),
            CoreErlangGenerator::case_clause_fallback_clause(fallback_var),
        ],
        form::Layout::Flat,
    )
}

/// What [`CoreErlangGenerator::guarded_op_doc`] knows of the right operand.
#[derive(Clone, Copy)]
struct RightOperand {
    /// Arithmetic: numeric by literal or declaration (ADR 0116, no coercion).
    statically_numeric: bool,
    /// Ordering comparison: not proven numeric, so tested (ADR 0116
    /// § Addendum, BT-5176).
    tested: bool,
}

/// The pieces of a guarded operator ([`CoreErlangGenerator::guarded_op_doc`]):
/// `bindings` then `case_doc` is the whole send.
struct GuardedOpParts {
    /// `let BinLeft = .. in let BinRight = .. in`.
    bindings: Document<'static>,
    /// The guard `case`: the `true` arm, the dispatching arm, the fallback.
    case_doc: Document<'static>,
    /// BT-4279: `(test, value)` when some test proves the send runs no user
    /// code: the bare BIF under the guard's test, or under an integer test
    /// for a coercing `+ - *`. `None` for a coercing `/`.
    fast_path: Option<(Document<'static>, Document<'static>)>,
}

impl CoreErlangGenerator {
    /// ADR 0116: builds the number-on-the-left coercion `try`/`catch`
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
    /// `primop 'raw_raise'` shape. The `case_clause_fallback`
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
        // ADR 0116: the shared reflected-selector table. Callers only reach
        // this for `+ - * /` (`is_arithmetic_operator` in `generate_binary_op`
        // and `operator_guard`); any other operator is a caller bug.
        let reflected = reflected_operator(op)
            .filter(|r| r.family == OperatorFamily::Arithmetic)
            .unwrap_or_else(|| {
                unreachable!("number_coercion_try_catch called for non-arithmetic operator: {op}")
            });
        let try_result_var = self.fresh_temp_var("BinCoerceTry");
        let type_var = self.fresh_temp_var("BinCoerceType");
        let error_var = self.fresh_temp_var("BinCoerceError");
        let stack_var = self.fresh_temp_var("BinCoerceStack");
        let other_pair_var = self.fresh_temp_var("BinCoerceOther");
        // Explicit wildcard for the inner is_number boolean case —
        // see this function's doc comment and `case_clause_fallback`'s own.
        let inner_no_match = self.case_clause_fallback("BinCoerceNoMatch");

        // BT-4836: the guarded BIF inside the `try` carries the send's line.
        let guarded_bif = self.annotate_current_send_call(form::call(
            "erlang",
            erlang_op,
            [left_doc.clone(), right_doc.clone()],
        ));
        let raise = Self::emit_raw_raise(type_var.clone(), error_var.clone(), stack_var.clone());
        let flat_clause = |pattern, body| form::clause([pattern], None, body, form::Layout::Flat);
        let coercion = docvec![
            reflected_hook_call(reflected, op, right_doc.clone(), left_doc),
            inner_no_match,
        ];
        let is_number_case = form::case_of(
            form::call("erlang", "is_number", [right_doc]),
            [
                flat_clause(atom("true"), raise.clone()),
                flat_clause(atom("false"), coercion),
            ],
            form::Layout::Flat,
        );
        let handler = form::case_of(
            form::tuple([var(type_var.clone()), var(error_var.clone())]),
            [
                flat_clause(
                    form::tuple([atom("error"), atom("badarith")]),
                    is_number_case,
                ),
                flat_clause(var(other_pair_var), raise),
            ],
            form::Layout::Flat,
        );
        try_catch_doc(
            guarded_bif,
            var(try_result_var),
            CatchVars {
                class: var(type_var),
                reason: var(error_var),
                stack: var(stack_var),
            },
            handler,
            form::Layout::Flat,
        )
    }

    /// ADR 0116: the always-bare arithmetic path's number-on-the-left
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
            form::let_prefix(var(left_var), left_code),
            form::let_prefix(var(right_var), right_code),
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
        // Capture both operands preserving evaluation order.
        let exprs: [&Expression; 2] = [left, right];
        let (preamble, mut docs) = self.thread_subexprs(&exprs, "PowOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");
        // BT-4836: annotate the call nodes that can raise (`float/1` on a
        // non-number, `math:pow` overflow).
        let float_left =
            self.annotate_current_send_call(docvec!["call 'erlang':'float'(", left_code, ")"]);
        let float_right =
            self.annotate_current_send_call(docvec!["call 'erlang':'float'(", right_code, ")"]);
        let pow = self.annotate_current_send_call(docvec![
            "call 'math':'pow'(",
            float_left,
            ", ",
            float_right,
            ")"
        ]);
        let call_doc = self.annotate_current_send_call(docvec!["call 'erlang':'round'(", pow, ")"]);
        Ok(self.close_prelude(&preamble, call_doc, "PowRes"))
    }

    /// BT-4851: `string_from_binary(iolist_to_binary([string_to_binary(L),
    /// string_to_binary(R)]))`, with the send's line on each call that can
    /// raise. Shared by the literal-string and runtime-dispatch arms.
    fn string_concat_doc(
        &self,
        left: Document<'static>,
        right: Document<'static>,
    ) -> Document<'static> {
        let l = self.annotate_current_send_call(docvec![
            "call 'beamtalk_primitive':'string_to_binary'(",
            left,
            ")"
        ]);
        let r = self.annotate_current_send_call(docvec![
            "call 'beamtalk_primitive':'string_to_binary'(",
            right,
            ")"
        ]);
        let io = self.annotate_current_send_call(docvec![
            "call 'erlang':'iolist_to_binary'([",
            l,
            ", ",
            r,
            "])"
        ]);
        docvec!["call 'beamtalk_primitive':'string_from_binary'(", io, ")"]
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

        // Capture both operands preserving evaluation order. When
        // either operand has an open scope, BOTH are force-hoisted into the
        // preamble so left-to-right evaluation order is preserved.
        let exprs: [&Expression; 2] = [left, right];
        let (preamble, mut docs) = self.thread_subexprs(&exprs, "ConcatOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");

        // BT-4173: `String` is the tagged term `{beamtalk_string, Bin}`.
        // Unwrap both operands to bytes, concatenate, and re-tag the result.
        // (Raw binaries are valid iodata, so no `binary_to_list` is needed.)
        // BT-4851: the calls that can raise (`string_to_binary` on a non-String
        // operand, `iolist_to_binary`) carry the send's line; the OTP compiler
        // takes a frame's location from the failing `call` node (BT-4836).
        let call_doc = if is_list {
            // List concatenation: erlang:'++'
            docvec!["call 'erlang':'++'(", left_code, ", ", right_code, ")",]
        } else if is_string {
            // String concatenation: iolist_to_binary
            self.string_concat_doc(left_code, right_code)
        } else {
            // Runtime dispatch: check is_list at runtime.
            // AGENTS.md: built entirely with Document/docvec!, no format!().
            let left_var = self.fresh_temp_var("ConcatLeft");
            let right_var = self.fresh_temp_var("ConcatRight");
            let string_arm = self.string_concat_doc(var(left_var.clone()), var(right_var.clone()));
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
                var(left_var),
                ", ",
                var(right_var),
                ") <'false'> when 'true' -> ",
                string_arm,
                " end",
            ]
        };

        Ok(self.close_prelude(&preamble, call_doc, "ConcatRes"))
    }
}

#[cfg(test)]
mod number_fallback_tests {
    use super::*;

    #[test]
    fn entry_point_follows_the_family() {
        assert_eq!(
            number_fallback_entry_point(OperatorFamily::Arithmetic),
            "send_number_coercion"
        );
        assert_eq!(
            number_fallback_entry_point(OperatorFamily::Ordering),
            "send_number_comparison"
        );
    }
}
