// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test helpers for use in beamtalk-core and dependent crate tests.

use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

/// Creates a unique temporary directory path (does not create it on disk).
/// Uses PID + nanosecond timestamp to avoid collisions between parallel tests.
///
/// # Panics
///
/// Panics if the system clock is set before the Unix epoch.
pub fn unique_temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time")
        .as_nanos();
    std::env::temp_dir().join(format!("{prefix}_{}_{}", std::process::id(), nanos))
}

/// Test-only helpers: parsing, codegen assertions, and AST builders.
///
/// Gated on `#[cfg(any(test, feature = "test"))]` to avoid prod binary
/// bloat while allowing dependent crates to opt in via the `test` Cargo
/// feature in their `[dev-dependencies]`.
#[cfg(any(test, feature = "test"))]
pub mod test_support {
    use crate::ast::{
        Block, BlockParameter, ClassDefinition, ClassModifiers, Expression, ExpressionStatement,
        Identifier, KeywordPart, Literal, MessageSelector, MethodDefinition, Module,
    };
    use crate::semantic_analysis::class_hierarchy::DeclaredType;
    use crate::source_analysis::{Severity, Span, lex_with_eof, parse};

    /// Parses a Beamtalk source string and returns the [`Module`] AST.
    ///
    /// Panics with a helpful message if any parse diagnostics are emitted,
    /// including the source and all diagnostic messages.
    ///
    /// # Panics
    ///
    /// Panics if the source contains parse errors.
    pub fn parse_bt(source: &str) -> Module {
        let (module, diagnostics) = parse(lex_with_eof(source));
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        if !errors.is_empty() {
            let msgs: Vec<_> = diagnostics.iter().map(|d| format!("{d:?}")).collect();
            panic!(
                "parse_bt: source produced {} parse error(s):\n  {}\n\nSource:\n{}",
                errors.len(),
                msgs.join("\n  "),
                source
            );
        }
        module
    }

    /// Asserts that `output` contains `fragment`, panicking with a clear diff
    /// message on failure.
    ///
    /// # Panics
    ///
    /// Panics if `output` does not contain `fragment`.
    pub fn assert_codegen_contains(output: &str, fragment: &str) {
        assert!(
            output.contains(fragment),
            "assert_codegen_contains failed.\n\nExpected fragment:\n  {fragment}\n\nActual output:\n{output}"
        );
    }

    /// Returns a zero-length [`Span`] suitable for test AST nodes.
    #[must_use]
    pub fn test_span() -> Span {
        Span::new(0, 0)
    }

    /// Builds a minimal [`ClassDefinition`] with the given name, no superclass,
    /// no state, and no methods.
    #[must_use]
    pub fn make_class(name: &str) -> ClassDefinition {
        let span = test_span();
        ClassDefinition::with_modifiers(
            Identifier::new(name, span),
            None,
            ClassModifiers::default(),
            Vec::new(),
            Vec::new(),
            span,
        )
    }

    /// Builds a minimal Actor [`ClassDefinition`] with the given name, no state, and no methods.
    ///
    /// Equivalent to `Actor subclass: <name>` with no declarations — the standard fixture for
    /// codegen tests that need an actor class but don't care about its shape.
    #[must_use]
    pub fn make_actor_class(name: &str) -> ClassDefinition {
        let span = test_span();
        ClassDefinition::new(
            Identifier::new(name, span),
            Identifier::new("Actor", span),
            Vec::new(),
            Vec::new(),
            span,
        )
    }

    /// Builds a minimal [`MethodDefinition`] with a unary selector and empty body.
    #[must_use]
    pub fn make_method(selector: &str) -> MethodDefinition {
        let span = test_span();
        MethodDefinition::new(
            MessageSelector::Unary(selector.into()),
            Vec::new(),
            Vec::new(),
            span,
        )
    }

    /// Builds an [`Expression::MessageSend`] representing a unary message send.
    ///
    /// `receiver` is an identifier expression; `selector` is the unary method name.
    #[must_use]
    pub fn make_unary_send(receiver: &str, selector: &str) -> Expression {
        let span = test_span();
        Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(receiver, span))),
            selector: MessageSelector::Unary(selector.into()),
            arguments: Vec::new(),
            is_cast: false,
            span,
        }
    }

    /// Wraps an [`Expression`] in a bare [`ExpressionStatement`].
    #[must_use]
    pub fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    /// Arbitrary [`DeclaredType`] values, recursively covering every
    /// grouping shape BT-3100's type-string fidelity properties exercise:
    /// union, generic, `FalseOr`/optional, difference, intersection, and
    /// the `Self`-family leaves (`Self`, `Self class`, `<Name> class`).
    ///
    /// Shared by `declared_type.rs`'s own `parse`/`Display` fixed-point
    /// property tests and `beamtalk-compiler-port`'s wire-fidelity property
    /// test (ETF encode/decode round trip) — both boundaries need the exact
    /// same generated shape space, so it lives here once rather than being
    /// copied into both crates (this repo's "No duplicate implementations"
    /// rule).
    pub fn arb_declared_type() -> impl proptest::strategy::Strategy<Value = DeclaredType> {
        use proptest::prelude::*;

        let leaf = prop_oneof![
            "[A-Za-z][A-Za-z0-9]{0,4}".prop_map(DeclaredType::simple),
            "[a-z][a-z0-9]{0,4}".prop_map(DeclaredType::singleton),
            Just(DeclaredType::SelfType),
            Just(DeclaredType::SelfClass),
            "[A-Za-z][A-Za-z0-9]{0,4}".prop_map(|n| DeclaredType::ClassOf(n.into())),
        ];

        leaf.prop_recursive(4, 32, 4, |inner| {
            prop_oneof![
                prop::collection::vec(inner.clone(), 2..4).prop_map(DeclaredType::Union),
                (
                    "[A-Za-z][A-Za-z0-9]{0,4}",
                    prop::collection::vec(inner.clone(), 1..3),
                )
                    .prop_map(|(base, params)| DeclaredType::generic(base, params)),
                inner
                    .clone()
                    .prop_map(|t| DeclaredType::FalseOr(Box::new(t))),
                (inner.clone(), inner.clone()).prop_map(|(base, excluded)| {
                    DeclaredType::Difference {
                        base: Box::new(base),
                        excluded: Box::new(excluded),
                    }
                }),
                (inner.clone(), inner.clone()).prop_map(|(left, right)| {
                    DeclaredType::Intersection {
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                }),
            ]
        })
    }

    // ========================================================================
    // Near-valid Beamtalk fragment generator (BT-3344)
    // ========================================================================
    //
    // A small, fast "fuzz-adjacent robustness" generator: near-valid source
    // snippets built from a hand-curated fragment list plus truncation and
    // concatenation. Shared by `beamtalk-core`'s and `beamtalk-repl`'s own
    // `tests/codegen_property_tests.rs` (both need the exact same "never
    // panics on near-valid input" properties, one for `generate_module`, one
    // for `generate_repl_expression`) via this crate's `test` feature, the
    // same mechanism [`arb_declared_type`] established (BT-3100) — see
    // `beamtalk-core/Cargo.toml`'s `[features] test` entry for how a
    // dependent crate opts in from its own `[dev-dependencies]`.
    //
    // Deliberately *not* what [`arb_program`] below exists for: `arb_program`
    // explores the semantic space (nested blocks, `^`, state threading) that
    // this flat fragment list can't reach. This generator is for properties
    // that only need cheap, varied "probably still tokenizes/mostly parses"
    // input, not grammar-driven coverage.

    /// Near-valid Beamtalk fragments for property-test seed generation.
    const NEAR_VALID_BEAMTALK_FRAGMENTS: &[&str] = &[
        "42",
        "\"hello\"",
        "true",
        "false",
        "nil",
        "x := 42",
        "x + y",
        "[:x | x + 1]",
        "Object subclass: Foo\n  state: x = 0\n  bar => x",
        "Actor subclass: Counter\n  state: count = 0\n  increment => count := count + 1",
        "#(1, 2, 3)",
        "#{#a => 1}",
        "self",
        "^42",
        "3 timesRepeat: [x := x + 1]",
        "#[first, ...rest] := #[1, 2, 3]",
        "[1] ensure: [nil]",
        "x match: { 1 => \"one\", _ => \"other\" }",
    ];

    fn valid_beamtalk_fragment() -> impl Strategy<Value = String> {
        prop::sample::select(NEAR_VALID_BEAMTALK_FRAGMENTS)
            .prop_map(std::string::ToString::to_string)
    }

    /// Near-valid Beamtalk source: a valid fragment, a valid fragment
    /// truncated at a random (char-boundary-safe) point, or two valid
    /// fragments concatenated on separate lines.
    pub fn near_valid_beamtalk() -> impl Strategy<Value = String> {
        prop_oneof![
            valid_beamtalk_fragment(),
            // Truncated
            valid_beamtalk_fragment().prop_flat_map(|s| {
                let len = s.len();
                if len <= 1 {
                    Just(s).boxed()
                } else {
                    (1..len)
                        .prop_map(move |cut| {
                            // MSRV-1.85-compatible stand-in for
                            // `str::floor_char_boundary` (stable since 1.91,
                            // past this crate's pinned MSRV). Always floors
                            // to a valid boundary, including 0 (an empty
                            // prefix) -- no special-casing needed, unlike an
                            // earlier version of this generator that
                            // returned the untruncated string at that
                            // boundary instead (BT-3344 code review).
                            let mut safe_cut = cut;
                            while safe_cut > 0 && !s.is_char_boundary(safe_cut) {
                                safe_cut -= 1;
                            }
                            s[..safe_cut].to_string()
                        })
                        .boxed()
                }
            }),
            // Multiple fragments
            (valid_beamtalk_fragment(), valid_beamtalk_fragment())
                .prop_map(|(a, b)| format!("{a}\n{b}")),
        ]
    }

    // ========================================================================
    // Grammar-driven Beamtalk program generator (BT-3116)
    // ========================================================================
    //
    // Every proptest in the compiler previously built inputs from a
    // hardcoded FRAGMENTS array of near-valid snippets (see
    // `core_erlang_validity_tests.rs`) or raw regex strategies -- "fuzz-
    // adjacent robustness" that can't explore the semantic space where real
    // bugs live: nested blocks with captures, `^` inside nested closures,
    // multi-statement bodies threading local state. This generator builds
    // well-formed Beamtalk **method bodies** directly as typed AST values
    // (so proptest's shrinking works structurally on the tree, not by
    // truncating strings), then renders them via `unparse`.
    //
    // Design: every generated program is a single `run` method on an
    // `Object subclass: <NAME>` (see [`arb_program`]) -- wrapping the
    // generated expression tree in a real method body, rather than a bare
    // top-level script expression, is what makes `^` (non-local return) a
    // *legal* construct to generate at all: `^` only means something
    // relative to an enclosing method.
    //
    // Scoping: identifiers are only ever generated as references to a name
    // *known to be in scope* -- either an outer scope entry threaded in by
    // the caller (block/method parameters) or a `name := value` binding
    // introduced earlier in the same body ([`arb_body`]'s staged
    // prelude-then-tail shape). There is no free-variable generation, so a
    // successfully-generated program never contains an undefined-variable
    // reference by construction.
    use proptest::prelude::*;

    /// Depth budget for recursive expression generation. Matches the
    /// `arb_declared_type` precedent above: small enough that generation
    /// stays fast and shrinking stays fast, large enough to reach nested
    /// blocks-within-blocks and `ifTrue:ifFalse:`-within-a-block shapes.
    const PROGRAM_GEN_MAX_DEPTH: u32 = 3;

    fn zero_span() -> Span {
        Span::new(0, 0)
    }

    fn ident(name: impl Into<ecow::EcoString>) -> Identifier {
        Identifier::new(name, zero_span())
    }

    /// A leaf expression: an integer/string literal, or (once `scope` is
    /// non-empty) a reference to one of its names -- including the
    /// reserved-word identifiers `true`/`false`/`nil`, which Beamtalk parses
    /// as plain identifiers rather than dedicated literal nodes.
    fn arb_leaf_expr(scope: Vec<EcoStr>) -> BoxedStrategy<Expression> {
        let literals = prop_oneof![
            any::<i32>()
                .prop_map(|n| Expression::Literal(Literal::Integer(i64::from(n)), zero_span())),
            "[a-zA-Z0-9 ]{0,8}"
                .prop_map(|s| Expression::Literal(Literal::String(s.into()), zero_span())),
            Just(Expression::Identifier(ident("true"))),
            Just(Expression::Identifier(ident("false"))),
            Just(Expression::Identifier(ident("nil"))),
        ];
        if scope.is_empty() {
            literals.boxed()
        } else {
            let scope_ref = prop::sample::select(scope)
                .prop_map(|name| Expression::Identifier(ident(name.as_str())));
            prop_oneof![3 => literals, 2 => scope_ref].boxed()
        }
    }

    /// `receiver unarySelector` for a small representative set of unary
    /// selectors. Codegen doesn't type-check message sends (an unrecognised
    /// selector is a *runtime* `does_not_understand`, not a compile error —
    /// see `docs/beamtalk-language-features.md` § DNU), so the exact
    /// selector choice only needs to be syntactically valid, not
    /// type-correct for whatever the receiver turns out to be.
    fn arb_unary_send(depth: u32, scope: Vec<EcoStr>) -> BoxedStrategy<Expression> {
        (
            arb_expr(depth - 1, scope),
            prop_oneof![
                Just(EcoStr::from("printString")),
                Just(EcoStr::from("class")),
                Just(EcoStr::from("isNil")),
                Just(EcoStr::from("negated")),
                Just(EcoStr::from("size")),
            ],
        )
            .prop_map(|(receiver, selector)| Expression::MessageSend {
                receiver: Box::new(receiver),
                selector: MessageSelector::Unary(selector),
                arguments: vec![],
                is_cast: false,
                span: zero_span(),
            })
            .boxed()
    }

    /// `left binOp right` for a small set of binary selectors.
    fn arb_binary_send(depth: u32, scope: Vec<EcoStr>) -> BoxedStrategy<Expression> {
        (
            arb_expr(depth - 1, scope.clone()),
            prop_oneof![
                Just(EcoStr::from("+")),
                Just(EcoStr::from("-")),
                Just(EcoStr::from("*")),
                Just(EcoStr::from("=:=")),
                Just(EcoStr::from("<")),
            ],
            arb_expr(depth - 1, scope),
        )
            .prop_map(|(left, selector, right)| Expression::MessageSend {
                receiver: Box::new(left),
                selector: MessageSelector::Binary(selector),
                arguments: vec![right],
                is_cast: false,
                span: zero_span(),
            })
            .boxed()
    }

    /// `receiver at: arg` or `receiver at: arg1 put: arg2` -- representative
    /// one- and two-part keyword sends.
    fn arb_keyword_send(depth: u32, scope: Vec<EcoStr>) -> BoxedStrategy<Expression> {
        let one_part = (
            arb_expr(depth - 1, scope.clone()),
            arb_expr(depth - 1, scope.clone()),
        )
            .prop_map(|(receiver, arg)| Expression::MessageSend {
                receiver: Box::new(receiver),
                selector: MessageSelector::Keyword(vec![KeywordPart::new("at:", zero_span())]),
                arguments: vec![arg],
                is_cast: false,
                span: zero_span(),
            });
        let two_part = (
            arb_expr(depth - 1, scope.clone()),
            arb_expr(depth - 1, scope.clone()),
            arb_expr(depth - 1, scope),
        )
            .prop_map(|(receiver, arg1, arg2)| Expression::MessageSend {
                receiver: Box::new(receiver),
                selector: MessageSelector::Keyword(vec![
                    KeywordPart::new("at:", zero_span()),
                    KeywordPart::new("put:", zero_span()),
                ]),
                arguments: vec![arg1, arg2],
                is_cast: false,
                span: zero_span(),
            });
        prop_oneof![one_part, two_part].boxed()
    }

    /// `cond ifTrue: [thenBody] ifFalse: [elseBody]`. The two branch blocks
    /// are zero-parameter (`ifTrue:ifFalse:` blocks never take arguments in
    /// Beamtalk) and may themselves contain `^` -- non-local return through
    /// a conditional's branch block is exactly the "fragile machinery"
    /// (state threading + control flow) this generator exists to probe.
    fn arb_if_true_if_false(depth: u32, scope: Vec<EcoStr>) -> BoxedStrategy<Expression> {
        (
            arb_expr(depth - 1, scope.clone()),
            arb_body(depth - 1, scope.clone(), true),
            arb_body(depth - 1, scope, true),
        )
            .prop_map(|(cond, then_body, else_body)| Expression::MessageSend {
                receiver: Box::new(cond),
                selector: MessageSelector::Keyword(vec![
                    KeywordPart::new("ifTrue:", zero_span()),
                    KeywordPart::new("ifFalse:", zero_span()),
                ]),
                arguments: vec![
                    Expression::Block(Block::new(vec![], then_body, zero_span())),
                    Expression::Block(Block::new(vec![], else_body, zero_span())),
                ],
                is_cast: false,
                span: zero_span(),
            })
            .boxed()
    }

    /// A block taking 0-2 parameters, immediately invoked via
    /// `value`/`value:`/`value:value:` -- the block body can reference both
    /// its own parameters and everything already in `scope`, so this is the
    /// generator's primary source of **captured** variables (a closure
    /// reading/mutating a name bound outside itself).
    fn arb_block_value_call(depth: u32, scope: Vec<EcoStr>) -> BoxedStrategy<Expression> {
        (0usize..=2)
            .prop_flat_map(move |arity| {
                let param_names: Vec<EcoStr> =
                    (0..arity).map(|i| EcoStr::from(format!("p{i}"))).collect();
                let params: Vec<BlockParameter> = param_names
                    .iter()
                    .map(|n| BlockParameter::new(n.as_str(), zero_span()))
                    .collect();
                let block_scope: Vec<EcoStr> =
                    scope.iter().cloned().chain(param_names.clone()).collect();
                let args_scope = scope.clone();
                (
                    arb_body(depth - 1, block_scope, true),
                    prop::collection::vec(arb_expr(depth - 1, args_scope), arity),
                )
                    .prop_map(move |(body, call_args)| {
                        let block =
                            Expression::Block(Block::new(params.clone(), body, zero_span()));
                        let selector = match arity {
                            0 => MessageSelector::Unary("value".into()),
                            1 => MessageSelector::Keyword(vec![KeywordPart::new(
                                "value:",
                                zero_span(),
                            )]),
                            _ => MessageSelector::Keyword(vec![
                                KeywordPart::new("value:", zero_span()),
                                KeywordPart::new("value:", zero_span()),
                            ]),
                        };
                        Expression::MessageSend {
                            receiver: Box::new(block),
                            selector,
                            arguments: call_args,
                            is_cast: false,
                            span: zero_span(),
                        }
                    })
            })
            .boxed()
    }

    /// The core expression grammar (BT-3116 tier 1): literals/identifiers,
    /// unary/binary/keyword sends, `ifTrue:ifFalse:`, and self-invoking
    /// blocks -- recursing with a shrinking `depth` budget so generation
    /// always terminates.
    fn arb_expr(depth: u32, scope: Vec<EcoStr>) -> BoxedStrategy<Expression> {
        if depth == 0 {
            return arb_leaf_expr(scope);
        }
        prop_oneof![
            3 => arb_leaf_expr(scope.clone()),
            2 => arb_unary_send(depth, scope.clone()),
            2 => arb_binary_send(depth, scope.clone()),
            2 => arb_keyword_send(depth, scope.clone()),
            1 => arb_if_true_if_false(depth, scope.clone()),
            1 => arb_block_value_call(depth, scope),
        ]
        .boxed()
    }

    /// The final statement of a body: usually a plain expression, but
    /// (when `allow_return`) sometimes `^expr` -- legal here because every
    /// generated body ultimately lives inside [`arb_program`]'s method.
    fn arb_tail_statement(
        depth: u32,
        scope: Vec<EcoStr>,
        allow_return: bool,
    ) -> BoxedStrategy<ExpressionStatement> {
        let plain = arb_expr(depth, scope.clone()).prop_map(ExpressionStatement::bare);
        if !allow_return {
            return plain.boxed();
        }
        let ret = arb_expr(depth, scope).prop_map(|value| {
            ExpressionStatement::bare(Expression::Return {
                value: Box::new(value),
                span: zero_span(),
            })
        });
        prop_oneof![4 => plain, 1 => ret].boxed()
    }

    /// A statement sequence (block/method body): 0-2 `locN := value`
    /// prelude assignments (each using only the scope from *before* this
    /// body started, so they can be generated independently/in parallel —
    /// no prelude statement can reference another prelude statement's
    /// binding), followed by one tail statement that sees the full
    /// prelude-extended scope. This is the generator's primary source of
    /// **state threading** (a local introduced by one statement, read or
    /// mutated by a later one) without needing fully general sequential
    /// dependent generation.
    fn arb_body(
        depth: u32,
        scope: Vec<EcoStr>,
        allow_return: bool,
    ) -> BoxedStrategy<Vec<ExpressionStatement>> {
        if depth == 0 {
            return arb_tail_statement(0, scope, allow_return)
                .prop_map(|stmt| vec![stmt])
                .boxed();
        }
        let prelude_scope = scope.clone();
        prop::collection::vec(arb_expr(depth - 1, prelude_scope), 0..=2)
            .prop_flat_map(move |prelude_values| {
                let scope = scope.clone();
                let prelude_names: Vec<EcoStr> = (0..prelude_values.len())
                    .map(|i| EcoStr::from(format!("loc{i}")))
                    .collect();
                let prelude_stmts: Vec<ExpressionStatement> = prelude_names
                    .iter()
                    .zip(prelude_values)
                    .map(|(name, value)| {
                        ExpressionStatement::bare(Expression::Assignment {
                            target: Box::new(Expression::Identifier(ident(name.as_str()))),
                            value: Box::new(value),
                            type_annotation: None,
                            span: zero_span(),
                        })
                    })
                    .collect();
                let extended_scope: Vec<EcoStr> = scope.into_iter().chain(prelude_names).collect();
                arb_tail_statement(depth - 1, extended_scope, allow_return).prop_map(move |tail| {
                    let mut stmts = prelude_stmts.clone();
                    stmts.push(tail);
                    stmts
                })
            })
            .boxed()
    }

    /// Generates a complete, well-formed Beamtalk **program**: an
    /// `Object subclass: <name>` with a single unary `run` method whose
    /// body is a grammar-driven statement sequence (BT-3116).
    ///
    /// Render with [`crate::unparse::unparse_module`] to get source text
    /// guaranteed to parse back with zero diagnostics — see
    /// `core_erlang_validity_tests.rs`'s `program_gen_round_trip` /
    /// `program_gen_codegen_validity` properties for the properties this
    /// guarantee is checked against.
    pub fn arb_program(class_name: &'static str) -> impl Strategy<Value = Module> {
        arb_body(PROGRAM_GEN_MAX_DEPTH, Vec::new(), true).prop_map(move |body| {
            let method = MethodDefinition::new(
                MessageSelector::Unary("run".into()),
                vec![],
                body,
                zero_span(),
            );
            let class = ClassDefinition::new(
                ident(class_name),
                ident("Object"),
                vec![],
                vec![method],
                zero_span(),
            );
            let mut module = Module::new(vec![], zero_span());
            module.classes.push(class);
            module
        })
    }

    /// `EcoString` alias local to this generator: proptest's `Strategy`
    /// trait needs `Clone + Debug` values threaded through closures a lot
    /// here, and the crate's `ecow::EcoString` already satisfies that
    /// cheaply (cheap `.clone()`, small-string-optimised) -- reused rather
    /// than plain `String` to match every other identifier field in the AST.
    type EcoStr = ecow::EcoString;

    /// Patterns that should never appear in valid Core Erlang output — Rust
    /// Debug/Display leaks (BT-875).
    ///
    /// Shared by `core_erlang_validity_tests.rs`'s proptest suite and the
    /// `compile_pipeline` fuzz target (BT-3124) so the two never drift.
    pub const CORE_ERLANG_FORMAT_ARTIFACT_PATTERNS: &[&str] = &[
        "{:?}",
        "Document::",
        "BinaryOp(",
        "Expression::",
        "Literal::",
        "MessageSelector::",
        "Pattern::",
        "TokenKind::",
    ];

    /// Checks that parentheses, brackets, and braces are balanced in Core
    /// Erlang output, skipping the contents of single- and double-quoted
    /// atom/string literals (which may themselves contain unbalanced
    /// delimiter characters).
    ///
    /// Shared by `core_erlang_validity_tests.rs`'s proptest suite and the
    /// `compile_pipeline` fuzz target (BT-3124) so the two never drift.
    #[must_use]
    pub fn core_erlang_has_balanced_delimiters(s: &str) -> bool {
        let mut stack = Vec::new();
        let mut chars = s.chars().peekable();
        let mut in_single_quote = false;
        let mut in_double_quote = false;

        while let Some(ch) = chars.next() {
            match ch {
                '\'' if !in_double_quote => in_single_quote = !in_single_quote,
                '"' if !in_single_quote => in_double_quote = !in_double_quote,
                '\\' if in_single_quote || in_double_quote => {
                    // Skip escaped character
                    chars.next();
                }
                _ if in_single_quote || in_double_quote => {}
                '(' => stack.push(')'),
                '[' => stack.push(']'),
                '{' => stack.push('}'),
                ')' | ']' | '}' => {
                    if stack.pop() != Some(ch) {
                        return false;
                    }
                }
                _ => {}
            }
        }
        stack.is_empty() && !in_single_quote && !in_double_quote
    }

    /// Checks the structural-validity properties `generate_module`'s output
    /// must satisfy whenever it returns `Ok`: starts with `module`, ends
    /// with `end`, has balanced delimiters, and contains no Rust format
    /// artifacts. Returns a list of human-readable violation descriptions —
    /// empty means the output is structurally valid.
    ///
    /// Shared by `core_erlang_validity_tests.rs`'s proptest suite and the
    /// `compile_pipeline` fuzz target (BT-3124) so the two never drift.
    #[must_use]
    pub fn core_erlang_structural_issues(output: &str) -> Vec<String> {
        let mut issues = Vec::new();
        let trimmed = output.trim();

        if !trimmed.starts_with("module") {
            issues.push(format!(
                "does not start with 'module':\n{}",
                char_prefix(trimmed, 200)
            ));
        }
        if !trimmed.ends_with("end") {
            issues.push(format!(
                "does not end with 'end':\n...{}",
                char_suffix(trimmed, 200)
            ));
        }
        if !core_erlang_has_balanced_delimiters(output) {
            issues.push("has unbalanced delimiters".to_string());
        }
        for pattern in CORE_ERLANG_FORMAT_ARTIFACT_PATTERNS {
            if output.contains(pattern) {
                issues.push(format!("contains format artifact {pattern:?}"));
            }
        }

        issues
    }

    /// Returns the first `max_chars` characters of `s` (fewer if shorter),
    /// respecting UTF-8 boundaries. A hand-rolled, MSRV-1.85-compatible
    /// stand-in for the standard library's `floor_char_boundary` (stable
    /// since 1.91, past this crate's pinned MSRV).
    fn char_prefix(s: &str, max_chars: usize) -> &str {
        match s.char_indices().nth(max_chars) {
            Some((byte_idx, _)) => &s[..byte_idx],
            None => s,
        }
    }

    /// Returns the last `max_chars` characters of `s` (fewer if shorter),
    /// respecting UTF-8 boundaries. A hand-rolled, MSRV-1.85-compatible
    /// stand-in for the standard library's `ceil_char_boundary` (stable
    /// since 1.91, past this crate's pinned MSRV).
    fn char_suffix(s: &str, max_chars: usize) -> &str {
        let char_count = s.chars().count();
        let skip = char_count.saturating_sub(max_chars);
        match s.char_indices().nth(skip) {
            Some((byte_idx, _)) => &s[byte_idx..],
            None => "",
        }
    }

    /// Returns the standard proptest configuration used across beamtalk-core property tests.
    ///
    /// Sets `cases` to at least 512 (overridable via `PROPTEST_CASES` env var).
    ///
    /// Available to dependent crates the same way as [`arb_declared_type`] and
    /// [`near_valid_beamtalk`]: enable this crate's `test` feature (which pulls
    /// in `proptest` as a normal, non-dev dependency via `dep:proptest`) rather
    /// than `#[cfg(test)]`-only, which would make it invisible to a dependent
    /// crate's own integration tests (BT-3344).
    #[must_use]
    pub fn proptest_config_default() -> proptest::prelude::ProptestConfig {
        let default = proptest::prelude::ProptestConfig::default();
        proptest::prelude::ProptestConfig {
            cases: default.cases.max(512),
            ..default
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn parse_bt_parses_class() {
            let module = parse_bt("Object subclass: Foo\n  greet => 42\n");
            assert_eq!(module.classes.len(), 1);
            assert_eq!(module.classes[0].name.name.as_str(), "Foo");
        }

        #[test]
        #[should_panic(expected = "parse_bt: source produced")]
        fn parse_bt_panics_on_invalid_source() {
            parse_bt("Object subclass:");
        }

        #[test]
        fn assert_codegen_contains_passes_on_match() {
            assert_codegen_contains("call 'erlang':'+'(1, 2)", "erlang");
        }

        #[test]
        #[should_panic(expected = "assert_codegen_contains failed")]
        fn assert_codegen_contains_fails_on_mismatch() {
            assert_codegen_contains("hello world", "missing");
        }

        #[test]
        fn make_class_builds_minimal_class() {
            let class = make_class("Counter");
            assert_eq!(class.name.name.as_str(), "Counter");
            assert!(class.superclass.is_none());
            assert!(class.methods.is_empty());
            assert!(class.state.is_empty());
        }

        #[test]
        fn make_actor_class_builds_minimal_actor_class() {
            let class = make_actor_class("CounterActor");
            assert_eq!(class.name.name.as_str(), "CounterActor");
            assert_eq!(
                class
                    .superclass
                    .as_ref()
                    .map(|superclass| superclass.name.as_str()),
                Some("Actor")
            );
            assert_eq!(class.class_kind, crate::ast::ClassKind::Actor);
            assert!(class.methods.is_empty());
            assert!(class.state.is_empty());
        }

        #[test]
        fn make_method_builds_unary_method() {
            let method = make_method("increment");
            assert_eq!(method.selector, MessageSelector::Unary("increment".into()));
            assert!(method.parameters.is_empty());
            assert!(method.body.is_empty());
        }

        #[test]
        fn make_unary_send_builds_message_send() {
            let expr = make_unary_send("obj", "size");
            match &expr {
                Expression::MessageSend {
                    selector,
                    arguments,
                    is_cast,
                    ..
                } => {
                    assert_eq!(*selector, MessageSelector::Unary("size".into()));
                    assert!(arguments.is_empty());
                    assert!(!is_cast);
                }
                other => panic!("expected MessageSend, got {other:?}"),
            }
        }
    }
}
