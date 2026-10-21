// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared selector classification helpers for state-threading control flow.
//!
//! Used by both `semantic_analysis` (for `DispatchKind::ControlFlow` classification)
//! and `codegen` (for block-mutation analysis and state-threading code generation).
//!
//! The module and its four predicates are `pub`, not `pub(crate)` (ADR 0117
//! Decision step 5): `codegen`'s consumer lives in the standalone
//! `beamtalk-codegen` crate, outside this one, so it needs genuine `pub`
//! visibility to reach them.
//!
//! Predicates in this module classify selectors for block-mutation analysis.
//! Where a selector is also part of [`WellKnownSelector`](crate::ast::WellKnownSelector)
//! — conditionals, `on:do:` — the predicate matches on the enum via
//! [`WellKnownSelector::from_name`] so a typo in either place is a compile
//! error at the call site. Loop and iteration selectors (`whileTrue:`, `do:`,
//! `collect:`, etc.) are not classified as well-known and so remain matched
//! by string; the split is deliberate — `WellKnownSelector` is reserved for
//! selectors the type-checker/codegen *intrinsify*, not every selector the
//! compiler happens to recognise.
//!
//! `and:`/`or:` join the string-matched group for the same reason
//! as the loop/iteration selectors — they are ordinary self-hosted
//! `Boolean` methods (`boolean.bt`'s `and:`/`or:`, each defined in terms of
//! `ifTrue:ifFalse:`), not selectors the type-checker itself intrinsifies.
//! Codegen still inlines their literal call shape the same way it inlines
//! `ifTrue:`/`ifFalse:` (see `try_generate_boolean_protocol`'s `and:`/`or:`
//! arms in `beamtalk-codegen`), so they need the same state-threading
//! classification here — both as a state-threading keyword selector (so
//! `check_actor_field_mutation_in_closure`/`DispatchKind::ControlFlow`
//! treat a mutation inside their block argument as safe) and as a
//! conditional selector (so `control_flow_has_mutations`/
//! `classify_body_expr` route the enclosing statement through the same
//! `{Result, NewState}` unpacking as `ifTrue:`/`ifFalse:`).

use crate::ast::{Expression, MessageSelector, WellKnownSelector};

/// The conditional selectors the type-checker intrinsifies: the one list
/// [`is_state_threading_keyword_selector`], [`is_conditional_selector`] and
/// [`assignment_threading_constructs`] read.
const CONDITIONAL_WELL_KNOWN: &[WellKnownSelector] = &[
    WellKnownSelector::IfTrue,
    WellKnownSelector::IfFalse,
    WellKnownSelector::IfTrueIfFalse,
    // ADR 0118 phase 4: `ifNil:`/`ifNil:ifNotNil:`/`ifNotNil:ifNil:` get the
    // same `_with_mutations` inline-case treatment as `ifNotNil:`: their block
    // argument(s) are analysed for field mutations the same way.
    WellKnownSelector::IfNil,
    WellKnownSelector::IfNotNil,
    WellKnownSelector::IfNilIfNotNil,
    WellKnownSelector::IfNotNilIfNil,
];

/// The state-threading keyword selectors that are control constructs but not
/// [`WellKnownSelector`]s: loops, `ensure:`, and `and:`/`or:` (see the module
/// doc comment). With [`CONDITIONAL_WELL_KNOWN`], `on:do:` and
/// [`COLLECTION_KEYWORD_SELECTORS`], the one list behind
/// [`is_state_threading_keyword_selector`].
const CONTROL_KEYWORD_SELECTORS: &[&str] = &[
    "and:",
    "or:",
    "whileTrue:",
    "whileFalse:",
    "timesRepeat:",
    "to:do:",
    "to:by:do:",
    "ensure:",
];

/// The state-threading keyword selectors that are collection messages.
const COLLECTION_KEYWORD_SELECTORS: &[&str] = &[
    "do:",
    "collect:",
    "select:",
    "reject:",
    "detect:",
    "detect:ifNone:",
    "inject:into:",
    "count:",
    "anySatisfy:",
    "allSatisfy:",
    "flatMap:",
    "takeWhile:",
    "dropWhile:",
    "groupBy:",
    "partition:",
    "sort:",
    "eachWithIndex:",
    "do:separatedBy:",
    "doWithKey:",
    "keysAndValuesDo:",
];

/// The collection messages a class can override and the stdlib's own
/// collection methods can reach through a self-send (BT-3916): every
/// state-threading collection selector but `do:`, which is the fold's own
/// source. The list-op receiver guard refuses a writing block sent any of them
/// to a receiver whose class defines one of these outside the stdlib, because
/// the stdlib's `collect:`/`select:`/`count:`/`eachWithIndex:`/
/// `do:separatedBy:` run through `self inject:into:` and `reject:` through
/// `self select:`. Codegen records the answer per class
/// (`overrides_collection_selector` in `__beamtalk_meta/0`) from this list, so
/// the runtime holds no copy.
pub fn overridable_collection_selectors() -> impl Iterator<Item = &'static str> {
    COLLECTION_KEYWORD_SELECTORS
        .iter()
        .copied()
        .filter(|sel| *sel != "do:")
}

/// The collection messages whose stdlib provision in `Collection` ends in
/// `self species withAll: result` (BT-3951): `collect:` and `select:`, and
/// `reject:` through `self select:`. `Enumerable`'s provisions of the same names
/// forward to `self elements` and never reach `species`, and the other
/// collection messages (`inject:into:`, `detect:`, `anySatisfy:`, ...) build no
/// collection of the receiver's species. Every one is also in
/// [`overridable_collection_selectors`].
const SPECIES_DEPENDENT_SELECTORS: &[&str] = &["collect:", "select:", "reject:"];

/// Instance-side messages a class can define that change what
/// [`species_dependent_collection_selectors`] answer (BT-3951): `species`.
const SPECIES_CONSTRUCTION_SELECTORS: &[&str] = &["species"];

/// Class-side counterpart: `species` defaults to `self class`, so a user
/// collection usually overrides class-side `withAll:` rather than `species`.
/// `Collection`'s provisions send no other class-side message to `species`.
const SPECIES_CONSTRUCTION_CLASS_SELECTORS: &[&str] = &["withAll:"];

/// The messages whose stdlib `Collection` provision builds its answer with
/// `self species withAll:`. A class that defines a species-construction
/// message outside the stdlib poisons exactly these, not the rest of the
/// collection messages, and only when the receiver gets them from the
/// `Collection` provision rather than a trait such as `Enumerable`.
pub fn species_dependent_collection_selectors() -> impl Iterator<Item = &'static str> {
    SPECIES_DEPENDENT_SELECTORS.iter().copied()
}

/// The instance-side species-construction trigger selectors (`species`).
/// Codegen records the answer per class (`overrides_species_construction` in
/// `__beamtalk_meta/0`) from this list and
/// [`species_construction_class_selectors`].
pub fn species_construction_selectors() -> impl Iterator<Item = &'static str> {
    SPECIES_CONSTRUCTION_SELECTORS.iter().copied()
}

/// The class-side species-construction trigger selectors (`withAll:`).
pub fn species_construction_class_selectors() -> impl Iterator<Item = &'static str> {
    SPECIES_CONSTRUCTION_CLASS_SELECTORS.iter().copied()
}

/// Returns `true` if `sel` is a state-threading keyword selector.
///
/// These selectors receive block arguments that are analysed for field mutations
/// as part of state-threading control flow.
#[must_use]
pub fn is_state_threading_keyword_selector(sel: &str) -> bool {
    WellKnownSelector::from_name(sel)
        .is_some_and(|w| w == WellKnownSelector::OnDo || CONDITIONAL_WELL_KNOWN.contains(&w))
        || CONTROL_KEYWORD_SELECTORS.contains(&sel)
        || COLLECTION_KEYWORD_SELECTORS.contains(&sel)
}

/// ADR 0133: the messages whose block literal argument (or, for `value` and
/// the loops, receiver) may assign a captured variable, grouped for the
/// escaping-block diagnostic's note. Read off the same lists as
/// [`is_state_threading_keyword_selector`],
/// [`is_state_threaded_block_receiver`] and [`LOOKUP_SHAPES`] (plus
/// `Result tryDo:`, [`is_try_do_selector`]), so the note cannot name a message the compiler does not
/// thread, nor leave out one it does.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignmentThreadingConstructs {
    /// Conditionals, loops, exception handling, `Result tryDo:` and a block
    /// literal sent `value`.
    pub control: Vec<&'static str>,
    /// The collection messages, the inlined `Dictionary` lookups included.
    pub collection: Vec<&'static str>,
}

/// See [`AssignmentThreadingConstructs`].
#[must_use]
pub fn assignment_threading_constructs() -> AssignmentThreadingConstructs {
    let mut control: Vec<&'static str> =
        CONDITIONAL_WELL_KNOWN.iter().map(|w| w.as_str()).collect();
    control.extend(CONTROL_KEYWORD_SELECTORS);
    control.extend(UNARY_LOOP_SELECTORS);
    control.push(WellKnownSelector::OnDo.as_str());
    // `tryDo:` threads only when sent to the `Result` class (ADR 0131 §5).
    control.push("Result tryDo:");
    control.extend(
        [
            WellKnownSelector::Value,
            WellKnownSelector::ValueColon,
            WellKnownSelector::ValueValue,
            WellKnownSelector::ValueValueValue,
        ]
        .into_iter()
        .map(WellKnownSelector::as_str)
        .filter(|sel| is_state_threaded_block_receiver(sel)),
    );
    let mut collection: Vec<&'static str> = COLLECTION_KEYWORD_SELECTORS.to_vec();
    collection.extend(
        LOOKUP_SHAPES
            .iter()
            .filter(|row| row.dictionary_inline)
            .map(|row| row.selector),
    );
    AssignmentThreadingConstructs {
        control,
        collection,
    }
}

/// Returns `true` if `sel` is a state-threading unary selector.
#[must_use]
pub fn is_state_threading_unary_selector(sel: &str) -> bool {
    UNARY_LOOP_SELECTORS.contains(&sel)
}

/// The state-threading unary selectors: the loops whose receiver block is the
/// body.
const UNARY_LOOP_SELECTORS: &[&str] = &["whileTrue", "whileFalse", "timesRepeat"];

/// Returns `true` if `sel` is an exception-handling selector (`on:do:` or `ensure:`).
///
/// For these selectors the *receiver* (try body) is a block that must also be
/// analysed for field mutations, in addition to the argument blocks.
#[must_use]
pub fn is_exception_selector(sel: &str) -> bool {
    // `on:do:` is a well-known selector; `ensure:` is not (it's not
    // intrinsified by the type-checker, only by codegen's state-threading).
    matches!(
        WellKnownSelector::from_name(sel),
        Some(WellKnownSelector::OnDo)
    ) || sel == "ensure:"
}

/// Returns `true` if `sel` is a Boolean/optional conditional selector.
///
/// For these selectors every block argument must be analysed independently,
/// because mutations may appear in the first branch but not the second.
///
/// `and:`/`or:` are not `WellKnownSelector`s (see module doc
/// comment) so they're matched by string, the same way `is_exception_selector`
/// folds in `ensure:` alongside the well-known `on:do:`.
///
/// ADR 0118 phase 4: `ifNil:`/`ifNil:ifNotNil:`/`ifNotNil:ifNil:`
/// join `ifNotNil:` here — each has its own `_with_mutations` inline-case
/// generator (`generate_nil_conditional_with_mutations` in
/// `beamtalk-codegen`), so a mutation in any of their block(s) threads the
/// same way `ifTrue:`/`ifFalse:` does.
#[must_use]
pub fn is_conditional_selector(sel: &str) -> bool {
    WellKnownSelector::from_name(sel).is_some_and(|w| CONDITIONAL_WELL_KNOWN.contains(&w))
        || matches!(sel, "and:" | "or:")
}

/// ADR 0118 §Decision 7: the single "which selectors thread which
/// block-argument positions" table. `beamtalk-core::ast::well_known::is_state_threaded_block_arg`
/// (consulted by `beamtalk-codegen`'s `block_arg_for_selector`) and (via a
/// thin per-crate wrapper) `beamtalk-lint`'s `DeadAssignment` check must both
/// stay derived from this table rather than keeping their own parallel copy,
/// so neither one silently drifts out of step with `and:`/`or:` or any
/// future addition.
///
/// Returns the 0-based argument indices of `sel` whose block-literal
/// argument's outer-local mutations are threaded back to the caller via the
/// `StateAcc` map (not silently lost). An empty slice at a given index does
/// **not** mean that argument is compiled as an isolated closure — see the
/// caveat below for the one selector where that distinction bites
/// (`detect:ifNone:`). An index can be excluded from this table because:
/// `sel` isn't a state-threading selector at all; its threading is
/// context-dependent (`eachWithIndex:`/`do:separatedBy:` only thread inside
/// an Actor's own fold, not always — see
/// `CoreErlangGenerator::enumeration_threads_actor_state` in
/// `beamtalk-codegen`); the threaded block is the *receiver*, not an
/// argument (`whileTrue:`/`whileFalse:`'s condition block, `on:do:`/
/// `ensure:`'s try/protected block, `value`/`value:`'s block) — receiver
/// position is a separate table, [`is_state_threaded_block_receiver`],
/// which this one never answers for; or (`detect:ifNone:`'s `ifNone:`
/// handler, index 1) the
/// argument's own local-variable writes simply aren't part of the fold this
/// table tracks, even though the block itself is still compiled inline
/// (not a closure) once codegen detects a mutation in *either* argument —
/// see [`crate::semantic_analysis::facts::StateEffects`]'s doc comment,
/// which answers the "is this a closure boundary" question this table does
/// NOT answer, and deliberately does not derive its answer from this one.
///
/// Gated on [`is_state_threading_keyword_selector`] so this table can never
/// claim an argument index for a selector that isn't classified as
/// state-threading in the first place — the two are derived from the same
/// selector data, even though the index mapping below is necessarily
/// selector-specific (arities and block-argument positions vary).
#[must_use]
pub fn state_threaded_block_arg_indices(sel: &str) -> &'static [usize] {
    if !is_state_threading_keyword_selector(sel) {
        return &[];
    }
    match sel {
        // Context-dependent — see this function's doc comment.
        "eachWithIndex:" | "do:separatedBy:" => &[],
        "to:do:" | "inject:into:" | "on:do:" => &[1],
        "to:by:do:" => &[2],
        // Two-block conditionals: `generate_*_with_mutations` threads both
        // branches (for `ifTrue:ifFalse:`; `generate_nil_conditional_with_mutations`
        // for the `ifNil:` two-block forms).
        "ifTrue:ifFalse:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:" => &[0, 1],
        // `and:`/`or:` join `ifTrue:`/`ifFalse:` here —
        // same single-block-argument shape.
        _ => &[0],
    }
}

/// Returns `true` if a block-literal argument at `arg_index` for `selector`
/// is state-threaded per [`state_threaded_block_arg_indices`].
#[must_use]
pub fn is_state_threaded_block_arg(selector: &str, arg_index: usize) -> bool {
    state_threaded_block_arg_indices(selector).contains(&arg_index)
}

/// The receiver-position companion to [`state_threaded_block_arg_indices`]:
/// returns `true` if a block *literal* that is the **receiver** of `selector`
/// has its outer-local mutations threaded back to the caller, so a
/// reassignment inside it is visible after the send returns.
///
/// Codegen inlines the receiver block for each of these instead of
/// compiling it as a closure:
///
/// - `value`, `value:`, `value:value:`, `value:value:value:` (the arities
///   `Block` defines) — `generate_block_value_inline_with_mutations`
///   (`beamtalk-codegen`'s `intrinsics.rs`), reached from both the unary and
///   keyword `value` handlers whenever the literal captures and mutates an
///   outer local.
/// - `on:do:` / `ensure:` — the try/protected body in
///   `generate_on_do_with_mutations` / `generate_ensure_with_mutations`
///   (`control_flow/exception_handling.rs`).
///
/// Deliberately **not** listed: `valueWithArguments:` (its receiver goes
/// through `thread_value_call_receiver` — a runtime `apply`, never inlined);
/// `repeat` and the unary `whileTrue`/`whileFalse` (no receiver inlining
/// path in codegen today); and the keyword `whileTrue:`/`whileFalse:`
/// CONDITION block — ADR 0118 phase 3 routes a condition-only self-send or
/// field write through `generate_while_loop_with_mutations`, but a plain
/// outer-*local* write in the condition (`[i := i + 1. i < 3] whileTrue:
/// [nil]` in a Value/`TestCase` context) still compiles the condition as a
/// Tier 2 closure and crashes at runtime with "function expects 1 arguments
/// but was called with 0" (BT-3782). ADR 0131 §6 rejects that shape at
/// compile time (`Walker::while_condition_threads`), so it never reaches
/// codegen; it stays out of this table until the condition is lowered as a
/// producer (BT-3743). A block
/// literal that is merely *stored* and later sent `value` is not a literal
/// receiver at all and is likewise outside this table — `beamtalk-lint`'s
/// `DeadAssignment` check keeps flagging it.
///
/// Runtime pins: `stdlib/test/dead_assignment_receiver_threading_test.bt`
/// asserts every selector listed here actually threads, so this table and
/// the codegen it describes cannot silently drift apart.
#[must_use]
pub fn is_state_threaded_block_receiver(selector: &str) -> bool {
    WellKnownSelector::from_name(selector).is_some_and(WellKnownSelector::is_block_value)
        || matches!(selector, "on:do:" | "ensure:")
}

/// The single "which collection HOM, which argument position" table —
/// returns the callable argument of a `do:`/`collect:`/`select:`/
/// `inject:into:`/`detect:`/`detect:ifNone:`/`count:`/`anySatisfy:`/
/// `allSatisfy:` send, or `None` for any other selector (or the wrong
/// argument count).
///
/// Introduced for the ADR 0128 opaque-callable fold, which ADR 0133 removed
/// (BT-3908). Its remaining readers are [`is_opaque_callable_hom_send`] and
/// `semantic_analysis::block_facts`'s `callee_threads_block_param` (the
/// field-writing self-send callee rule, BT-3922).
#[must_use]
pub fn opaque_fold_callable_arg<'a>(
    selector: &str,
    arguments: &'a [Expression],
) -> Option<&'a Expression> {
    match (selector, arguments) {
        (
            "do:" | "collect:" | "select:" | "detect:" | "count:" | "anySatisfy:" | "allSatisfy:",
            [callable],
        )
        | ("detect:ifNone:", [callable, _])
        | ("inject:into:", [_, callable]) => Some(callable),
        _ => None,
    }
}

/// `true` if `expr` (paren-unwrapped) is a collection-HOM send
/// ([`opaque_fold_callable_arg`]) whose callable argument is NOT a block
/// literal — an opaque callable (a parameter, a local, any other
/// expression).
///
/// Purely syntactic. Codegen no longer reads it: since ADR 0133 (BT-3908)
/// such a send compiles to the plain list op in every context. Its only
/// reader is `semantic_analysis::block_facts`'s `callee_threads_block_param`
/// (the field-writing self-send callee rule, which BT-3922 replaces with the
/// escaping-block diagnostic). A bare-`self` receiver is excluded (a
/// `SelfSend`), as are a `super` receiver and a cast (`!`) send.
#[must_use]
pub fn is_opaque_callable_hom_send(expr: &Expression) -> bool {
    let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        is_cast: false,
        ..
    } = expr.unwrap_parens()
    else {
        return false;
    };
    if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
        || matches!(receiver.as_ref(), Expression::Super(_))
    {
        return false;
    }
    let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
    opaque_fold_callable_arg(&sel, arguments)
        .is_some_and(|callable| !matches!(callable.unwrap_parens(), Expression::Block(_)))
}

/// ADR 0131 Phase 2 (BT-3749): the loops and folds whose `{Value, StateAcc}`
/// result codegen unwraps as a local-threading producer (`whileTrue:`/
/// `whileFalse:`, the counted loops, `do:` (BT-3796; it answers `nil`) and the
/// value-returning folds). Returns the 0-based index of the body block
/// argument of a send of `sel` with `arity` arguments, or `None` for any other
/// selector or arity.
///
/// `keysAndValuesDo:`/`doWithKey:` join the table with BT-3841 (their dictionary
/// fold answers the same `{nil, StateAcc}` carrier as `do:`), and
/// `anySatisfy:`/`allSatisfy:` (their fold answers `{Bool, StateAcc}` and, like
/// the `detect:` folds, stops running the block at the first decisive element,
/// BT-3805).
///
/// `eachWithIndex:` and `do:separatedBy:` are listed too (BT-3841): codegen
/// desugars them to a synthetic `inject:into:` fold and lowers that fold as the
/// producer, keyed on the original send.
///
/// The body block is the one whose outer-local writes the construct packs
/// (with, for `whileTrue:`/`whileFalse:`, the condition block receiver).
/// `detect:ifNone:`'s `ifNone:` handler is not a body; it is the producer's
/// handler ([`producer_handler_arg_index`]), packed with the body.
///
/// The one table: codegen's `counted_loop_body_block`/`foldl_list_op_body_block`
/// and the Phase 0 allow-set check (`semantic_analysis/validators/local_threading.rs`)
/// both read it, so the producer and the diagnostic accept the same sends.
#[must_use]
pub fn producer_body_arg_index(sel: &str, arity: usize) -> Option<usize> {
    match (sel, arity) {
        (
            "whileTrue:" | "whileFalse:" | "timesRepeat:" | "do:" | "collect:" | "select:"
            | "reject:" | "count:" | "detect:" | "keysAndValuesDo:" | "doWithKey:" | "anySatisfy:"
            | "allSatisfy:" | "eachWithIndex:",
            1,
        )
        | ("detect:ifNone:" | "do:separatedBy:", 2) => Some(0),
        ("to:do:" | "inject:into:", 2) => Some(1),
        ("to:by:do:", 3) => Some(2),
        _ => None,
    }
}

/// ADR 0131 Phase 2c (BT-3794): the handler block a producer fold packs
/// besides its body. Returns the 0-based index of the literal handler
/// argument of a send of `sel` with `arity` arguments (`detect:ifNone:`'s
/// `ifNone:` block), or `None` for any other selector or arity.
///
/// The handler runs once after the fold, in the fold's activation, so a write
/// in it is threaded exactly like a write in the body: the fold's plan packs
/// it into the same accumulator and the producer rebinds it. Together with
/// [`producer_body_arg_index`] this is the one table of the blocks a
/// producer packs: core's `producer_packed_blocks` reads both, and the Phase 0
/// allow-set check and codegen (`lowered_construct_blocks`, the fold plans)
/// read that, so the diagnostic and the lowering accept the same sends.
#[must_use]
pub fn producer_handler_arg_index(sel: &str, arity: usize) -> Option<usize> {
    match (sel, arity) {
        ("detect:ifNone:" | "do:separatedBy:", 2) => Some(1),
        _ => None,
    }
}

/// BT-3855: whether the handler of a send of `sel` with `arity` arguments
/// ([`producer_handler_arg_index`]) runs *inside the producer's body frame*,
/// as the arm of a conditional statement of the body, rather than once after
/// the fold. `do:separatedBy:`'s separator is: codegen desugars the send to an
/// `inject:into:` fold whose body starts with `first ifFalse: [separator]`.
/// `detect:ifNone:`'s `ifNone:` handler is not.
#[must_use]
pub fn producer_handler_is_body_arm(sel: &str, arity: usize) -> bool {
    matches!((sel, arity), ("do:separatedBy:", 2))
}

/// Whether `sel` is a counted loop in [`producer_body_arg_index`]'s table
/// (`timesRepeat:`, `to:do:`, `to:by:do:`).
#[must_use]
pub fn is_counted_loop_selector(sel: &str) -> bool {
    matches!(sel, "timesRepeat:" | "to:do:" | "to:by:do:")
}

/// One row of [`LOOKUP_SHAPES`]: a block-taking lookup selector.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LookupShape {
    /// The selector.
    pub selector: &'static str,
    /// The 0-based indices of its block arguments.
    pub block_args: &'static [usize],
    /// The parameter count of each block argument, parallel to
    /// [`Self::block_args`]: what the selector calls it with (`at:ifPresent:`
    /// hands its block the found value, `at:ifAbsent:` hands its block
    /// nothing). The inline lowering binds the parameter to the found value.
    pub block_params: &'static [usize],
    /// Whether the lookup is a local-threading producer for a `Dictionary`
    /// receiver (ADR 0131 Phase 2d, BT-3798): codegen lowers it inline, guarded
    /// by a runtime receiver check, and its handler block is an arm frame.
    /// Only a selector `Dictionary` itself implements can be: the lowering
    /// answers what the primitive answers.
    pub dictionary_inline: bool,
}

/// ADR 0131 §1: the block-taking lookup selectors — a send that runs one of
/// its block literal arguments only when a key (or element) is missing or
/// present. The ONE table of them: [`lookup_block_arg_indices`] and
/// [`lookup_shape`] read it, core's `local_threading_construct` and
/// `conditional_arm_blocks` read those, and the Phase 0 check and codegen's
/// lowering read those, so the diagnostic and the lowering cannot accept
/// different sends.
///
/// The receiver type is unknown at compile time: the selector is implemented
/// by the `Dictionary` primitive (`primitives/dictionary.rs`), by stdlib
/// methods that call `block value` themselves (`Ets at:ifAbsent:`) and by any
/// user class, and an inline lowering is valid only for the first. ADR 0131
/// Phase 2d (BT-3798) therefore lowers a [`LookupShape::dictionary_inline`]
/// lookup inline behind a runtime guard (`beamtalk_map:lookup_for_threading/3`):
/// a `Dictionary` receiver takes the inline path, any other receiver (a
/// value-type instance is also a map, but tagged) raises a structured
/// `#beamtalk_error{}` when the handler writes an outer local.
///
/// `Dictionary` implements `at:ifAbsent:`, `at:ifPresent:` and
/// `at:ifPresent:ifAbsent:` (BT-3824); those three rows are
/// `dictionary_inline`. `at:ifAbsentPut:` and `removeKey:ifAbsent:` are not,
/// permanently: they assume a mutable dictionary, `Dictionary` is immutable
/// and will not define them (users write `at:ifAbsent:` then `at:put:`), so
/// there is no `Dictionary` behaviour for an inline lowering to answer and
/// they stay rejected at compile time (ADR 0131 §6) when their block writes
/// an outer local. The `dictionary_inline_rows_are_dictionary_selectors` test
/// pins the invariant against the class hierarchy.
pub const LOOKUP_SHAPES: &[LookupShape] = &[
    LookupShape {
        selector: "at:ifAbsent:",
        block_args: &[1],
        block_params: &[0],
        dictionary_inline: true,
    },
    // Permanently not `dictionary_inline`: not a `Dictionary` selector.
    LookupShape {
        selector: "at:ifAbsentPut:",
        block_args: &[1],
        block_params: &[0],
        dictionary_inline: false,
    },
    LookupShape {
        selector: "at:ifPresent:",
        block_args: &[1],
        block_params: &[1],
        dictionary_inline: true,
    },
    LookupShape {
        selector: "at:ifPresent:ifAbsent:",
        block_args: &[1, 2],
        block_params: &[1, 0],
        dictionary_inline: true,
    },
    // Permanently not `dictionary_inline`: not a `Dictionary` selector.
    LookupShape {
        selector: "removeKey:ifAbsent:",
        block_args: &[1],
        block_params: &[0],
        dictionary_inline: false,
    },
];

/// The [`LOOKUP_SHAPES`] row of `sel`, or `None` for any other selector.
#[must_use]
pub fn lookup_shape(sel: &str) -> Option<&'static LookupShape> {
    LOOKUP_SHAPES.iter().find(|row| row.selector == sel)
}

/// The 0-based indices of the block arguments of the lookup selector `sel`
/// ([`LOOKUP_SHAPES`]), empty for any other selector.
///
/// None of the lookups is threaded unless its row is
/// [`LookupShape::dictionary_inline`]: codegen compiles the others' blocks as
/// closures, and the §6 check rejects a block that writes an outer local
/// there. `detect:ifNone:` is not listed: it is a fold
/// ([`is_state_threading_keyword_selector`]).
#[must_use]
pub fn lookup_block_arg_indices(sel: &str) -> &'static [usize] {
    lookup_shape(sel).map_or(&[], |row| row.block_args)
}

/// Whether `sel` is a lookup that codegen lowers inline for a `Dictionary`
/// receiver ([`LookupShape::dictionary_inline`]).
#[must_use]
pub fn is_dictionary_inline_lookup(sel: &str) -> bool {
    lookup_shape(sel).is_some_and(|row| row.dictionary_inline)
}

/// ADR 0131 §5: `Result tryDo:`, a catch-boundary construct. Keyed on the
/// selector, not on the receiver being spelled `Result`.
#[must_use]
pub fn is_try_do_selector(sel: &str) -> bool {
    sel == "tryDo:"
}

#[cfg(test)]
mod tests {
    use super::*;

    /// BT-3951: the runtime's list of overridable collection selectors
    /// (`beamtalk_collection_selectors.erl`, read when a runtime extension is
    /// checked) is the compiler's one list, in the same order. Edit the Erlang
    /// module when this fails.
    #[test]
    fn runtime_overridable_selector_list_matches_the_compiler_list() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../runtime/apps/beamtalk_runtime/src/beamtalk_collection_selectors.erl"
        );
        let src = std::fs::read_to_string(path).expect("read beamtalk_collection_selectors.erl");
        let atoms = |name: &str| -> Vec<String> {
            let body = src
                .split(&format!("%% BEGIN {name}"))
                .nth(1)
                .and_then(|rest| rest.split(&format!("%% END {name}")).next())
                .unwrap_or_else(|| panic!("markers for {name}"));
            body.lines()
                .filter_map(|line| {
                    let line = line.trim().trim_end_matches([',', ']']);
                    Some(line.strip_prefix('\'')?.strip_suffix('\'')?.to_string())
                })
                .collect()
        };
        let strs = |it: &mut dyn Iterator<Item = &'static str>| -> Vec<String> {
            it.map(str::to_string).collect()
        };
        assert_eq!(
            atoms("SELECTORS"),
            strs(&mut overridable_collection_selectors())
        );
        assert_eq!(
            atoms("SPECIES DEPENDENT"),
            strs(&mut species_dependent_collection_selectors())
        );
        assert_eq!(
            atoms("SPECIES INSTANCE"),
            strs(&mut species_construction_selectors())
        );
        assert_eq!(
            atoms("SPECIES CLASS"),
            strs(&mut species_construction_class_selectors())
        );
        // Every species-dependent selector is also an overridable one.
        for sel in species_dependent_collection_selectors() {
            assert!(
                overridable_collection_selectors().any(|s| s == sel),
                "{sel}"
            );
        }
    }

    fn parse_expr(src: &str) -> Expression {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, diagnostics) = crate::source_analysis::parse(tokens);
        assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
        module
            .expressions
            .into_iter()
            .next()
            .expect("one expression")
            .expression
    }

    #[test]
    fn dictionary_inline_rows_are_dictionary_selectors() {
        // BT-3798: the inline lowering answers what the `Dictionary` primitive
        // answers, so a row is `dictionary_inline` exactly when `Dictionary`
        // implements the selector. BT-3824: `Dictionary` defines
        // `at:ifPresent:` and `at:ifPresent:ifAbsent:`; `at:ifAbsentPut:` and
        // `removeKey:ifAbsent:` are permanently not `Dictionary` selectors (an
        // immutable `Dictionary` has no such behaviour), so their rows stay
        // `dictionary_inline: false`.
        let hierarchy = crate::semantic_analysis::ClassHierarchy::with_builtins();
        let inline: Vec<&str> = LOOKUP_SHAPES
            .iter()
            .filter(|row| row.dictionary_inline)
            .map(|row| row.selector)
            .collect();
        assert_eq!(
            inline,
            ["at:ifAbsent:", "at:ifPresent:", "at:ifPresent:ifAbsent:"]
        );
        for row in LOOKUP_SHAPES {
            assert_eq!(
                row.block_args.len(),
                row.block_params.len(),
                "{}",
                row.selector
            );
            assert_eq!(
                hierarchy.find_method("Dictionary", row.selector).is_some(),
                row.dictionary_inline,
                "{}",
                row.selector
            );
            assert_eq!(lookup_block_arg_indices(row.selector), row.block_args);
            assert_eq!(
                is_dictionary_inline_lookup(row.selector),
                row.dictionary_inline
            );
        }
        assert!(lookup_shape("at:put:").is_none());
        assert!(lookup_block_arg_indices("at:put:").is_empty());
    }

    #[test]
    fn opaque_fold_callable_arg_picks_block_position() {
        for (src, callable) in [
            ("items do: blk", "blk"),
            ("items collect: blk", "blk"),
            ("items select: blk", "blk"),
            ("items detect: blk", "blk"),
            ("items count: blk", "blk"),
            ("items anySatisfy: blk", "blk"),
            ("items allSatisfy: blk", "blk"),
            ("items detect: blk ifNone: other", "blk"),
            ("items inject: seed into: blk", "blk"),
        ] {
            let Expression::MessageSend {
                selector,
                arguments,
                ..
            } = parse_expr(src)
            else {
                panic!("{src}: not a message send");
            };
            let arg = opaque_fold_callable_arg(&selector.name(), &arguments)
                .unwrap_or_else(|| panic!("{src}: expected a callable argument"));
            assert!(
                matches!(arg, Expression::Identifier(id) if id.name == callable),
                "{src}: picked {arg:?}"
            );
        }
        let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = parse_expr("items reject: blk")
        else {
            panic!("not a message send");
        };
        assert!(opaque_fold_callable_arg(&selector.name(), &arguments).is_none());
    }

    #[test]
    fn is_opaque_callable_hom_send_requires_non_literal_non_self() {
        assert!(is_opaque_callable_hom_send(&parse_expr("items do: blk")));
        assert!(is_opaque_callable_hom_send(&parse_expr(
            "items inject: 0 into: blk"
        )));
        assert!(is_opaque_callable_hom_send(&parse_expr(
            "(items detect: blk ifNone: [0])"
        )));
        // A literal block (even parenthesized) is analysed statically instead.
        assert!(!is_opaque_callable_hom_send(&parse_expr(
            "items do: [:x | x]"
        )));
        assert!(!is_opaque_callable_hom_send(&parse_expr(
            "items do: ([:x | x])"
        )));
        // `inject:into:`'s callable is the SECOND argument.
        assert!(!is_opaque_callable_hom_send(&parse_expr(
            "items inject: seed into: [:a :b | a]"
        )));
        // A bare-`self` receiver is a self-send, not the list-op fold.
        assert!(!is_opaque_callable_hom_send(&parse_expr("self do: blk")));
        assert!(!is_opaque_callable_hom_send(&parse_expr("super do: blk")));
        // Not a covered selector.
        assert!(!is_opaque_callable_hom_send(&parse_expr(
            "items reject: blk"
        )));
    }

    #[test]
    fn keyword_selectors() {
        assert!(is_state_threading_keyword_selector("whileTrue:"));
        assert!(is_state_threading_keyword_selector("do:"));
        assert!(is_state_threading_keyword_selector("on:do:"));
        assert!(is_state_threading_keyword_selector("ifTrue:"));
        assert!(is_state_threading_keyword_selector("doWithKey:"));
        assert!(is_state_threading_keyword_selector("keysAndValuesDo:"));
        // New block-accepting selectors
        assert!(is_state_threading_keyword_selector("detect:"));
        assert!(is_state_threading_keyword_selector("detect:ifNone:"));
        assert!(is_state_threading_keyword_selector("count:"));
        assert!(is_state_threading_keyword_selector("flatMap:"));
        // Medium-risk list selectors
        assert!(is_state_threading_keyword_selector("takeWhile:"));
        assert!(is_state_threading_keyword_selector("dropWhile:"));
        assert!(is_state_threading_keyword_selector("groupBy:"));
        assert!(is_state_threading_keyword_selector("partition:"));
        assert!(is_state_threading_keyword_selector("sort:"));
        // Enumeration helpers self-hosted on inject:into:
        assert!(is_state_threading_keyword_selector("eachWithIndex:"));
        assert!(is_state_threading_keyword_selector("do:separatedBy:"));
        assert!(!is_state_threading_keyword_selector("perform:"));
        // `and:`/`or:` block arguments compile inline the same way.
        assert!(is_state_threading_keyword_selector("and:"));
        assert!(is_state_threading_keyword_selector("or:"));
        // ifNil:/ifNil:ifNotNil:/ifNotNil:ifNil: block(s) compile
        // inline the same way as ifTrue:/ifNotNil:.
        assert!(is_state_threading_keyword_selector("ifNil:"));
        assert!(is_state_threading_keyword_selector("ifNil:ifNotNil:"));
        assert!(is_state_threading_keyword_selector("ifNotNil:ifNil:"));
    }

    #[test]
    fn unary_selectors() {
        assert!(is_state_threading_unary_selector("whileTrue"));
        assert!(is_state_threading_unary_selector("whileFalse"));
        assert!(is_state_threading_unary_selector("timesRepeat"));
        assert!(!is_state_threading_unary_selector("do:"));
    }

    #[test]
    fn exception_selectors() {
        assert!(is_exception_selector("on:do:"));
        assert!(is_exception_selector("ensure:"));
        assert!(!is_exception_selector("ifTrue:"));
        assert!(!is_exception_selector("do:"));
    }

    #[test]
    fn conditional_selectors() {
        assert!(is_conditional_selector("ifTrue:"));
        assert!(is_conditional_selector("ifFalse:"));
        assert!(is_conditional_selector("ifTrue:ifFalse:"));
        assert!(is_conditional_selector("ifNotNil:"));
        assert!(!is_conditional_selector("on:do:"));
        assert!(!is_conditional_selector("do:"));
        assert!(is_conditional_selector("and:"));
        assert!(is_conditional_selector("or:"));
        // ADR 0118 phase 4
        assert!(is_conditional_selector("ifNil:"));
        assert!(is_conditional_selector("ifNil:ifNotNil:"));
        assert!(is_conditional_selector("ifNotNil:ifNil:"));
    }

    // ---------------------------------------------------------------
    // state_threaded_block_arg_indices / is_state_threaded_block_arg
    // (ADR 0118 §7 — the one selector table)
    // ---------------------------------------------------------------

    #[test]
    fn threaded_block_arg_single_block_selectors_are_index_zero() {
        for sel in [
            "whileTrue:",
            "whileFalse:",
            "timesRepeat:",
            "do:",
            "collect:",
            "select:",
            "reject:",
            "anySatisfy:",
            "allSatisfy:",
            "detect:",
            "detect:ifNone:",
            "count:",
            "flatMap:",
            "takeWhile:",
            "dropWhile:",
            "groupBy:",
            "partition:",
            "sort:",
            "doWithKey:",
            "keysAndValuesDo:",
            "ensure:",
            "ifTrue:",
            "ifFalse:",
            "ifNotNil:",
            "ifNil:",
            "and:",
            "or:",
        ] {
            assert_eq!(
                state_threaded_block_arg_indices(sel),
                &[0],
                "expected {sel:?} to thread only its argument at index 0"
            );
            assert!(is_state_threaded_block_arg(sel, 0), "{sel:?} arg 0");
            assert!(!is_state_threaded_block_arg(sel, 1), "{sel:?} arg 1");
        }
    }

    #[test]
    fn threaded_block_receiver_selectors() {
        for sel in [
            "value",
            "value:",
            "value:value:",
            "value:value:value:",
            "on:do:",
            "ensure:",
        ] {
            assert!(is_state_threaded_block_receiver(sel), "{sel:?}");
        }
        // Documented exclusions: never inlined at the receiver position
        // (`value:value:value:value:` is not a `Block` selector at all; a
        // keyword `whileTrue:`/`whileFalse:` condition with a local write
        // crashes at runtime — see the function's doc comment).
        for sel in [
            "value:value:value:value:",
            "valueWithArguments:",
            "repeat",
            "whileTrue",
            "whileFalse",
            "whileTrue:",
            "whileFalse:",
            "perform:",
            "do:",
            "ifTrue:",
        ] {
            assert!(!is_state_threaded_block_receiver(sel), "{sel:?}");
        }
    }

    #[test]
    fn threaded_block_arg_two_block_selectors() {
        for sel in ["ifTrue:ifFalse:", "ifNil:ifNotNil:", "ifNotNil:ifNil:"] {
            assert_eq!(state_threaded_block_arg_indices(sel), &[0, 1], "{sel:?}");
            assert!(is_state_threaded_block_arg(sel, 0));
            assert!(is_state_threaded_block_arg(sel, 1));
            assert!(!is_state_threaded_block_arg(sel, 2));
        }
    }

    #[test]
    fn only_the_separator_is_a_body_arm_handler() {
        // BT-3855: `do:separatedBy:`'s separator runs inside the fold body;
        // `detect:ifNone:`'s handler runs once after the fold.
        assert!(producer_handler_is_body_arm("do:separatedBy:", 2));
        assert!(!producer_handler_is_body_arm("detect:ifNone:", 2));
        assert!(!producer_handler_is_body_arm("do:separatedBy:", 1));
        assert!(!producer_handler_is_body_arm("do:", 1));
    }

    #[test]
    fn threaded_block_arg_second_arg_selectors() {
        assert_eq!(state_threaded_block_arg_indices("to:do:"), &[1]);
        assert_eq!(state_threaded_block_arg_indices("inject:into:"), &[1]);
        assert_eq!(state_threaded_block_arg_indices("on:do:"), &[1]);
        assert!(!is_state_threaded_block_arg("to:do:", 0));
        assert!(is_state_threaded_block_arg("to:do:", 1));
    }

    #[test]
    fn threaded_block_arg_third_arg_selector() {
        assert_eq!(state_threaded_block_arg_indices("to:by:do:"), &[2]);
        assert!(is_state_threaded_block_arg("to:by:do:", 2));
        assert!(!is_state_threaded_block_arg("to:by:do:", 1));
    }

    #[test]
    fn threaded_block_arg_context_dependent_selectors_are_empty() {
        // eachWithIndex:/do:separatedBy: only thread inside an Actor's own
        // fold (see this function's doc comment) — not unconditionally, so
        // they're excluded from this table rather than claimed incorrectly.
        assert_eq!(
            state_threaded_block_arg_indices("eachWithIndex:"),
            &[] as &[usize]
        );
        assert_eq!(
            state_threaded_block_arg_indices("do:separatedBy:"),
            &[] as &[usize]
        );
    }

    #[test]
    fn threaded_block_arg_non_state_threading_selector_is_empty() {
        assert_eq!(
            state_threaded_block_arg_indices("customLoop:"),
            &[] as &[usize]
        );
        assert!(!is_state_threaded_block_arg("customLoop:", 0));
    }

    /// ADR 0118 §7 conformance test: enumerates every
    /// [`WellKnownSelector`] variant (via an exhaustive `match` — adding a
    /// new variant without extending this match is a compile error, so this
    /// table can never silently fall behind the enum it classifies) and
    /// asserts [`state_threaded_block_arg_indices`] agrees with the expected
    /// arg-index classification for each one.
    ///
    /// This is the selector-table half of "does the codegen threaded-vars
    /// map and the selector predicate agree on which arg indices thread":
    /// `beamtalk-codegen`'s `threaded_locals_of` (`threading_analysis.rs`)
    /// reads `state_threaded_block_arg_indices` directly for every
    /// `WellKnownSelector`-backed selector below except the conditional
    /// family (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`/`ifNotNil:`/`ifNil:`/
    /// `ifNil:ifNotNil:`/`ifNotNil:ifNil:`) and `on:do:`, which it instead
    /// routes through `is_conditional_selector`/`is_exception_selector` —
    /// so for those the "agreement" is that this table's indices match what
    /// those two predicates + their dedicated `generate_*_with_mutations`
    /// codegen actually thread (documented per-arm below), not a literal
    /// shared code path. Either way, a selector reaching neither special
    /// case falls through to this table verbatim, so the two representations
    /// cannot drift apart for it.
    #[test]
    fn well_known_selector_arg_indices_are_exhaustively_classified() {
        fn expected_indices(sel: WellKnownSelector) -> &'static [usize] {
            match sel {
                // Two-block conditionals.
                WellKnownSelector::IfTrueIfFalse
                | WellKnownSelector::IfNilIfNotNil
                | WellKnownSelector::IfNotNilIfNil => &[0, 1],
                // One-block conditionals, plus `whileTrue:`/`whileFalse:`
                // (KEYWORD selectors despite the name resemblance to
                // `WhileTrue`/`WhileFalse` — their receiver is the
                // condition block, handled separately by codegen; their one
                // keyword argument, the loop body, is arg0 here) and
                // `ensure:` (its one argument, the cleanup block, is arg0).
                WellKnownSelector::IfTrue
                | WellKnownSelector::IfFalse
                | WellKnownSelector::IfNotNil
                | WellKnownSelector::IfNil
                | WellKnownSelector::WhileTrue
                | WellKnownSelector::WhileFalse
                | WellKnownSelector::Ensure => &[0],
                // `on:do:`: arg0 is the exception class, arg1 the handler block.
                WellKnownSelector::OnDo => &[1],
                // Every other WellKnownSelector variant is either not a
                // keyword selector at all (unary/no block argument:
                // `isNil`, `class`, `repeat`, `hash`, `fieldNames`,
                // `perform:` family, block-`value` family, `isOk`/`isError`
                // family) or a keyword selector whose argument is never a
                // state-threaded block (`isKindOf:`, `respondsTo:`,
                // `error:`, `fieldAt:`/`fieldAt:put:`) — none carry a block
                // argument this table recognizes.
                WellKnownSelector::IsNil
                | WellKnownSelector::NotNil
                | WellKnownSelector::IsKindOf
                | WellKnownSelector::Class
                | WellKnownSelector::RespondsTo
                | WellKnownSelector::Value
                | WellKnownSelector::ValueColon
                | WellKnownSelector::ValueValue
                | WellKnownSelector::ValueValueValue
                | WellKnownSelector::IsOk
                | WellKnownSelector::IsError
                | WellKnownSelector::IsOkColon
                | WellKnownSelector::IsErrorColon
                | WellKnownSelector::Repeat
                | WellKnownSelector::Hash
                | WellKnownSelector::Error
                | WellKnownSelector::FieldAt
                | WellKnownSelector::FieldAtPut
                | WellKnownSelector::FieldNames
                // Neither `hasField:` nor `clearField:` carries a
                // block-literal argument — both take a field-name Symbol
                // (ADR 0124 §1/B4).
                | WellKnownSelector::HasField
                | WellKnownSelector::ClearField
                | WellKnownSelector::Perform
                | WellKnownSelector::PerformWithArgs
                | WellKnownSelector::PerformLocallyWithArgs
                // Neither carries a block-literal argument —
                // `withTimeout:`'s argument is a duration, `at:`'s an index/key.
                | WellKnownSelector::WithTimeout
                | WellKnownSelector::At => &[],
            }
        }

        // Mirrors `beamtalk_core::ast::well_known::tests::ALL_VARIANTS` — kept
        // as a literal list here (rather than importing a test-only const
        // across the module boundary) since this test's whole point is to
        // force a compile error in `expected_indices` above when a new
        // variant appears; a shared list wouldn't need the array below to
        // change, but `expected_indices`'s `match` still would.
        const ALL_VARIANTS: &[WellKnownSelector] = &[
            WellKnownSelector::IsNil,
            WellKnownSelector::NotNil,
            WellKnownSelector::IfNil,
            WellKnownSelector::IfNotNil,
            WellKnownSelector::IfNilIfNotNil,
            WellKnownSelector::IfNotNilIfNil,
            WellKnownSelector::IsKindOf,
            WellKnownSelector::Class,
            WellKnownSelector::RespondsTo,
            WellKnownSelector::IfTrue,
            WellKnownSelector::IfFalse,
            WellKnownSelector::IfTrueIfFalse,
            WellKnownSelector::OnDo,
            WellKnownSelector::Value,
            WellKnownSelector::ValueColon,
            WellKnownSelector::ValueValue,
            WellKnownSelector::ValueValueValue,
            WellKnownSelector::IsOk,
            WellKnownSelector::IsError,
            WellKnownSelector::IsOkColon,
            WellKnownSelector::IsErrorColon,
            WellKnownSelector::WhileTrue,
            WellKnownSelector::WhileFalse,
            WellKnownSelector::Repeat,
            WellKnownSelector::Ensure,
            WellKnownSelector::Hash,
            WellKnownSelector::Error,
            WellKnownSelector::FieldAt,
            WellKnownSelector::FieldAtPut,
            WellKnownSelector::FieldNames,
            WellKnownSelector::HasField,
            WellKnownSelector::ClearField,
            WellKnownSelector::Perform,
            WellKnownSelector::PerformWithArgs,
            WellKnownSelector::PerformLocallyWithArgs,
            WellKnownSelector::WithTimeout,
            WellKnownSelector::At,
        ];

        for &sel in ALL_VARIANTS {
            let name = sel.as_str();
            assert_eq!(
                state_threaded_block_arg_indices(name),
                expected_indices(sel),
                "state_threaded_block_arg_indices({name:?}) disagreed with the \
                 expected WellKnownSelector classification"
            );
        }
    }
}

// ─── ADR 0137 (BT-4020): which sends may run user code ──────────────────────

/// ADR 0137 §1–2: whether a send, as a *call*, may run one of this actor's
/// blocks or methods in the calling process — the classification the cell
/// bracket (`SyncedCall`) and its verifier (`UnbracketedUserCodeSend`) key on.
/// Derived entirely from the tables above and [`WellKnownSelector`], so there
/// is no second list to keep in step.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SendUserCode {
    /// An intrinsic that can never dispatch to user code, whatever its
    /// arguments are (`isNil`, `class`, `hash`, `error:`, the field
    /// reflection family, `spawn`/`await`, `asType:`, `yourself`).
    Never,
    /// A construct the compiler lowers in place (`ifTrue:`, `whileTrue:`, an
    /// inlined `do:`, `on:do:`, a literal block's `value`, ...). It is not a
    /// call; the sends in its blocks are classified where they are compiled.
    Inlined,
    /// Anything else: bracket it. This is the default (ADR 0137 §2): a
    /// primitive that takes a block or can dispatch to user code for some
    /// argument, a non-inlined `value`, `perform:`, a send to an unknown or
    /// in-process receiver, an FFI call.
    MayRun,
}

/// Classifies the send `receiver selector arguments`. A receiver of `self`
/// or `super` is the caller's to judge: a compiled self-send is explicit
/// threading, not a call into the cell.
#[must_use]
pub fn send_user_code(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> SendUserCode {
    let is_block = |e: &Expression| matches!(e.unwrap_parens(), Expression::Block(_));
    if let Some(wk) = selector.well_known() {
        if matches!(
            wk,
            WellKnownSelector::Class
                | WellKnownSelector::RespondsTo
                | WellKnownSelector::IsNil
                | WellKnownSelector::NotNil
                | WellKnownSelector::Hash
                | WellKnownSelector::Error
                | WellKnownSelector::FieldAt
                | WellKnownSelector::FieldAtPut
                | WellKnownSelector::FieldNames
                | WellKnownSelector::HasField
                | WellKnownSelector::ClearField
        ) {
            return SendUserCode::Never;
        }
        if wk.is_block_value() {
            return if is_block(receiver) {
                SendUserCode::Inlined
            } else {
                SendUserCode::MayRun
            };
        }
    }
    let name = selector.name();
    let name = name.as_str();
    if matches!(
        name,
        "asType:" | "yourself" | "spawn" | "spawnWith:" | "await" | "awaitForever" | "await:"
    ) {
        return SendUserCode::Never;
    }
    // Collection messages: inlined as a fold when the callable is a literal block.
    if COLLECTION_KEYWORD_SELECTORS.contains(&name) {
        return match opaque_fold_callable_arg(name, arguments) {
            Some(callable) if is_block(callable) => SendUserCode::Inlined,
            // `eachWithIndex:`/`do:separatedBy:`/... take their blocks by position.
            None if !arguments.is_empty() && arguments.iter().all(is_block) => {
                SendUserCode::Inlined
            }
            _ => SendUserCode::MayRun,
        };
    }
    if is_state_threading_keyword_selector(name)
        || is_state_threading_unary_selector(name)
        || name == "repeat"
    {
        // Loops and `ensure:`/`on:do:` run their receiver block in place.
        let receiver_inlined = !(is_state_threaded_block_receiver(name)
            || matches!(
                name,
                "whileTrue:" | "whileFalse:" | "whileTrue" | "whileFalse" | "timesRepeat" | "repeat"
            ))
            || is_block(receiver);
        let args_inlined = state_threaded_block_arg_indices(name)
            .iter()
            .all(|&i| arguments.get(i).is_some_and(is_block));
        return if receiver_inlined && args_inlined {
            SendUserCode::Inlined
        } else {
            SendUserCode::MayRun
        };
    }
    SendUserCode::MayRun
}

// ─── Structurally lowered binary selectors (shared leaf, BT-4020) ───────────

/// How the compiler lowers a binary selector structurally, i.e. without
/// dispatching it as a message.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructuralBinary {
    /// `>>`: method lookup (`generate_message_send`).
    MethodLookup,
    /// `**`: `math:pow` (`generate_power_op`).
    Power,
    /// `++`: list/string concatenation (`generate_concat_op`).
    Concat,
    /// Remainder and the equality/identity operators (ADR 0002): a bare Erlang BIF.
    Bif(&'static str),
}

/// The one table of structurally lowered binary selectors. Both lowering sites
/// (`generate_message_send` for `>>`, `generate_binary_op` for the rest) and
/// [`is_structurally_lowered_binary`] key off it.
#[must_use]
pub fn structural_binary(op: &str) -> Option<StructuralBinary> {
    match op {
        ">>" => Some(StructuralBinary::MethodLookup),
        "**" => Some(StructuralBinary::Power),
        "++" => Some(StructuralBinary::Concat),
        "%" => Some(StructuralBinary::Bif("rem")),
        "==" => Some(StructuralBinary::Bif("==")),
        "=:=" => Some(StructuralBinary::Bif("=:=")), // Strict equality (ADR 0002)
        "/=" => Some(StructuralBinary::Bif("/=")),   // Loose inequality (ADR 0002)
        "=/=" => Some(StructuralBinary::Bif("=/=")), // Strict inequality (ADR 0002)
        _ => None,
    }
}

