// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Exception handling code generation (Block `on:do:` and `ensure:`).
//!
//! **DDD Compilation Context:** Code Generation
//!
//! Generates Core Erlang `try/catch` for `on:do:` and `try/after` for `ensure:`.
//! These are structural intrinsics because they must wrap the block execution
//! in Core Erlang exception handling constructs at compile time.
//!
//! # `on:do:` — Exception Handling (try/catch)
//!
//! ```beamtalk
//! [risky operation] on: Exception do: [:e | handle error]
//! ```
//!
//! Generates:
//! ```erlang
//! let _BlockFun = <receiver> in
//! let _ExClass = <exClass> in
//! let _HandlerFun = <handler> in
//! try apply _BlockFun ()
//! of _Result -> _Result
//! catch <_Type, _Error, _RawStack> ->
//!     let _BuiltStack = primop 'build_stacktrace'(_RawStack) in
//!     let _ExObj = call 'beamtalk_exception_handler':'ensure_wrapped'(_Type, _Error, _BuiltStack) in
//!     case matches_class(ExClass, ExObj) of
//!         true  -> apply _HandlerFun (_ExObj)
//!         false -> primop 'raw_raise'(_Type, _Error, _RawStack)
//! ```
//!
//! # `ensure:` — Cleanup (try/after)
//!
//! ```beamtalk
//! [operation] ensure: [cleanup]
//! ```
//!
//! Generates:
//! ```erlang
//! let _BlockFun = <receiver> in
//! let _CleanupFun = <cleanup> in
//! try
//!     let _TryResult = apply _BlockFun () in _TryResult
//! of _Result -> let _ = apply _CleanupFun () in _Result
//! catch <_Type, _Error, _Stacktrace> ->
//!     do apply _CleanupFun ()
//!     primop 'raw_raise'(_Type, _Error, _Stacktrace)
//! ```

use super::super::intrinsics::{
    STATEFUL_BLOCK_DISPATCH_HINT, validate_block_arity_exact, validate_on_do_handler,
};
use super::super::threaded_ir::{
    BindOp, FrameId, RenderCtx, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar,
};
use super::super::{CoreErlangGenerator, Result, block_analysis};
use super::analysis::ThreadedFamilies;
use super::family_slots;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{join, leaf};
use beamtalk_core::ast::{Block, Expression};

/// ADR 0122 / BT-3506: an arm's (or the construct's own) contribution to the
/// trailing family slot — the family this construct threads (`ClassVars`/
/// `SelfVt`) paired with the version to render there, or `None` when the
/// construct threads neither. A named alias (not a bare tuple type) purely
/// so the several functions passing this shape around don't each spell out
/// `Option<(VersionPrefix, usize)>` (clippy's `type_complexity`).
type FamilySlot = Option<(VersionPrefix, usize)>;

/// BT-3486/BT-3506 (ADR 0122): one inlined `on:do:`/`ensure:` arm (a try
/// body, an `on:do:` handler, or one of `ensure:`'s two cleanup runs), as
/// produced by [`CoreErlangGenerator::generate_exception_body_with_threading`].
///
/// `family_mutated_version` is the arm's own final `(prefix, version)` for
/// whichever EXTRA storage family (`ClassVars`/`SelfVt`) the construct
/// threads through its trailing tuple slot, when the arm actually advanced
/// that family past what it inherited on entry — `None` when it did not.
/// Generalizes BT-3486's `self_mutated_version` (`SelfVt`-only) to also
/// cover `ClassVars`, the `cv_mutated_version` / `self_mutated_version` pair
/// `value_type_codegen.rs`'s `VtBranchPieces` carries for a conditional's
/// branch arms (BT-3159/BT-3484), and read the same way: an arm that did
/// not itself mutate the family carries the construct's pre-`try` baseline
/// in the slot instead, so both arms agree on the tuple shape. At most one
/// entry is ever meaningful for this construct — `ClassVars`/`SelfVt` are
/// mutually exclusive by construction, see
/// [`CoreErlangGenerator::eligible_families`].
struct ExceptionArm {
    doc: Document<'static>,
    result_var: String,
    state_version: usize,
    family_mutated_version: FamilySlot,
}

impl CoreErlangGenerator {
    fn state_acc_var_doc(state_version: usize) -> Document<'static> {
        match state_version {
            0 => docvec!["StateAcc"],
            n => leaf::var(super::super::util::versioned_var("StateAcc", n)),
        }
    }

    /// ADR 0122 Decision 4, applied to the two EXTRA storage families
    /// on:do:/ensure: can carry through its trailing tuple slot(s): reads
    /// `prefix`'s live version off the generator. `ClassVars`/`SelfVt` only
    /// — every other prefix is unreachable here, since
    /// [`Self::exception_construct_families`] only ever answers with these
    /// two (State is filtered out; see that function's doc comment).
    fn family_version(&self, prefix: &VersionPrefix) -> usize {
        match prefix {
            VersionPrefix::ClassVars => self.class_var_version(),
            VersionPrefix::SelfVt => self.self_version(),
            other => unreachable!(
                "on:do:/ensure: only ever threads ClassVars/SelfVt as extra \
                 storage families, got {other:?}"
            ),
        }
    }

    /// The write half of [`Self::family_version`].
    fn set_family_version(&mut self, prefix: &VersionPrefix, version: usize) {
        match prefix {
            VersionPrefix::ClassVars => self.set_class_var_version(version),
            VersionPrefix::SelfVt => self.set_self_version(version),
            other => unreachable!(
                "on:do:/ensure: only ever threads ClassVars/SelfVt as extra \
                 storage families, got {other:?}"
            ),
        }
    }

    /// The bare (version-0) Core Erlang variable name for `prefix` — `"Self"`
    /// or `"ClassVars"`, the same spelling [`VersionedVar::render_name`]
    /// gives version 0 of either prefix, kept as a literal here (rather than
    /// rendering a version-0 `VersionedVar`) only because
    /// [`Self::seed_exception_arm_family`] needs the BARE name as the shadow
    /// binding's own target identifier, not a value to render.
    fn family_bare_var_name(prefix: &VersionPrefix) -> &'static str {
        match prefix {
            VersionPrefix::ClassVars => "ClassVars",
            VersionPrefix::SelfVt => "Self",
            other => unreachable!(
                "on:do:/ensure: only ever threads ClassVars/SelfVt as extra \
                 storage families, got {other:?}"
            ),
        }
    }

    /// ADR 0122 / BT-3506: this arm's own contribution to the construct's
    /// trailing family slot — the arm's own mutated `(prefix, version)` if it
    /// actually advanced `families`' one member, else that member paired
    /// with `outer_version` (the version live immediately before the `try`,
    /// which every arm inherits and which `with_branch_context` restores on
    /// exit). `None` when `families` is empty (every Actor-instance-method
    /// and every value-type/class-method construct whose blocks mutate
    /// neither family) — the generalization of BT-3486's
    /// `exception_self_slot` to any of [`ThreadedFamilies`]' members via
    /// [`ThreadedFamilies`] instead of a bare `bool`.
    fn exception_family_slot(
        families: &ThreadedFamilies,
        outer_version: usize,
        arm: &ExceptionArm,
    ) -> FamilySlot {
        let prefix = families.as_slice().first()?;
        Some(match &arm.family_mutated_version {
            Some((p, v)) if p == prefix => (p.clone(), *v),
            _ => (prefix.clone(), outer_version),
        })
    }

    /// ADR 0122 / BT-3506: generates one exception arm — seeding whichever
    /// extra storage family `families` names, then pushing the seed and the
    /// arm's own code onto `docs` in that order — and returns the three
    /// pieces the caller needs to close the arm's result tuple: the variable
    /// holding its last expression's value, its final `StateAcc` version,
    /// and the trailing family slot it contributes
    /// ([`Self::exception_family_slot`]). Generalizes BT-3486's
    /// `push_exception_arm` (`SelfVt`-only, via a `bool`) to any of
    /// [`ThreadedFamilies`]' members.
    ///
    /// The seed / generate / slot sequence is identical for every arm that
    /// starts from the construct's own pre-`try` baseline — both `on:do:`
    /// arms and `ensure:`'s error-path cleanup run. `ensure:`'s success-path
    /// cleanup is the one exception: it re-seeds from the try tuple's
    /// trailing slot rather than from `outer_version`, so it open-codes the
    /// same steps with that different seed.
    fn push_exception_arm(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        body: &Block,
        families: &ThreadedFamilies,
        outer_version: usize,
    ) -> Result<(String, usize, FamilySlot)> {
        let seed = self.seed_exception_arm_family(families, outer_version);
        let arm = self.generate_exception_body_with_threading(body, families)?;
        let slot = Self::exception_family_slot(families, outer_version, &arm);
        docs.push(seed);
        docs.push(arm.doc);
        Ok((arm.result_var, arm.state_version, slot))
    }

    /// ADR 0122 / BT-3506: closes a `{Result, StateAcc` open prefix (`base`)
    /// into the construct's full result-tuple `Document`, appending `slot`'s
    /// family/version (when `families` is non-empty) through the shared
    /// [`family_slots::append_family_slots`] helper — ADR 0122's FIRST real
    /// emission consumer of it. Generalizes BT-3486's
    /// `exception_self_slot_doc` (a bare `Option<usize>` always spelled
    /// `Self`) to whichever family `families` names.
    ///
    /// `slot` must be `Some` exactly when `families` is non-empty — the
    /// invariant [`Self::exception_family_slot`]'s own callers already
    /// maintain (it returns `None` only when `families` is empty).
    fn close_exception_result_tuple(
        &mut self,
        result_var: String,
        state_final: usize,
        families: &ThreadedFamilies,
        slot: &FamilySlot,
    ) -> Document<'static> {
        let base = docvec![
            " {",
            leaf::var(result_var),
            ", ",
            Self::state_acc_var_doc(state_final)
        ];
        let ctx = RenderCtx::new(self);
        let closed = family_slots::append_family_slots(
            base,
            families,
            |prefix| {
                let (_, version) = slot
                    .as_ref()
                    .filter(|(p, _)| p == prefix)
                    .expect("families non-empty implies a slot for its one member");
                VersionedVar::new(prefix.clone(), *version, FrameId::ROOT)
            },
            &ctx,
        );
        docvec![closed, " "]
    }

    /// ADR 0122 / BT-3506: emits the arm-local `let <bare> = <bare>{N} in `
    /// shadow that gives an exception arm the version-0 entry parameter its
    /// own [`threaded_ir`](super::super::threaded_ir) frame requires for
    /// whichever extra family `families` names — `verify`'s `check_use`
    /// treats version 0, and only version 0, as a frame's implicit
    /// always-bound entry, so an arm handed a live `<bare>{N}` baseline would
    /// consume a version its frame never produces — and resets the counter
    /// so the arm mints version 1, 2, … from there. Generalizes BT-3486's
    /// `seed_exception_arm_self` (`SelfVt`-only) to `ClassVars` too — closing
    /// exactly the cross-frame `UnboundVersion` gap a class-method self-send
    /// inside `on:do:`/`ensure:` hit before this fix (BT-3506's shape (c)):
    /// `emit_class_var_result_unwrap`'s own rebind tags its `Bind` with the
    /// arm's `current_branch_frame()`, but without this reset its SOURCE
    /// version was whatever `class_var_version()` inherited from the
    /// enclosing method's own top frame — a version this arm's frame never
    /// produced.
    ///
    /// This is the exact family-generic counterpart of the
    /// `let StateAcc = <outer> in ` rebind every one of these constructs
    /// already performs for the state accumulator: same name shadowing, same
    /// version-0 restart, same reason.
    ///
    /// [`Document::Nil`] (and no reset) when `families` is empty, or when the
    /// live version is already 0 — the common case, where the bare name is
    /// the method's own fun parameter (or, for `ClassVars`, its own entry)
    /// and there is nothing to shadow. `live > 0` happens when an earlier
    /// statement in the same method already mutated the family, including a
    /// preceding `on:do:`/`ensure:` whose own mutation this issue threads
    /// out.
    fn seed_exception_arm_family(
        &mut self,
        families: &ThreadedFamilies,
        live: usize,
    ) -> Document<'static> {
        let Some(prefix) = families.as_slice().first() else {
            return Document::Nil;
        };
        if live == 0 {
            return Document::Nil;
        }
        let bare = Self::family_bare_var_name(prefix);
        self.set_family_version(prefix, 0);
        docvec![
            "let ",
            leaf::var(bare.to_string()),
            " = ",
            leaf::var(super::super::util::versioned_var(bare, live)),
            " in ",
        ]
    }

    /// ADR 0122 / BT-3506: the storage families (`ClassVars`/`SelfVt`) this
    /// `on:do:`/`ensure:`'s inlined blocks thread out through the
    /// construct's own trailing tuple slot(s) — the unified,
    /// [`ThreadedFamilies`]-based successor to BT-3486's
    /// `exception_blocks_thread_value_self` (`SelfVt`-only, a bare `bool`).
    ///
    /// Built from [`Self::eligible_families`] (this generator's CURRENT
    /// context), minus `State`: an Actor instance method's own `StateAcc` IS
    /// this construct's second tuple slot already (unconditionally, via
    /// [`Self::exception_body_outer_state`]) — never one of these EXTRA
    /// trailing ones, so `State` is never itself a family this function can
    /// answer for. `ClassVars`/`SelfVt` are mutually exclusive by
    /// construction (`eligible_families`'s own doc comment: `in_class_method`
    /// alone decides one from the other), so the result never carries more
    /// than one entry for this construct.
    ///
    /// A family counts as mutated when EITHER block mutates it ANYWHERE,
    /// per ADR 0122's unified recursive detector
    /// [`Self::body_threaded_families`] (BT-3522). It replaced a
    /// deliberately top-level-only walk of its own
    /// (`block_top_level_mutates_family`, deleted with that migration), whose
    /// blindness below depth 0 meant a nested mutation got no slot allocated
    /// at all and was silently discarded on normal return.
    ///
    /// Detection and carry-capability stay separate questions (ADR 0122
    /// §Decision 1), and the two have genuinely different answers here:
    ///
    /// * a mutation in a top-level statement's own SUB-EXPRESSION
    ///   (`t := 1 + (self bump)`) is carried end-to-end once a slot exists,
    ///   because ADR 0118's `thread_ahead` already lowers it into a real
    ///   `Bind` in the arm's own frame — before BT-3522 it had no slot to
    ///   land in and tripped `verify()`'s `UnboundVersion`;
    /// * a mutation inside a NESTED BLOCK is not carried, and is rejected
    ///   per-statement by
    ///   [`Self::reject_unthreadable_value_self_field_write`] (`SelfVt`) /
    ///   [`Self::reject_unthreadable_class_var_mutation`] (`ClassVars`) in
    ///   [`Self::generate_exception_body_with_threading_inner`], never
    ///   silently dropped.
    ///
    /// Shared with the consumer side
    /// (`value_type_codegen.rs`'s `exception_construct_threaded_families`) so
    /// wherever a consumer runs, the tuple SHAPE the emitter writes and the
    /// extraction the consumer emits agree by construction (the same
    /// arrangement `loop_body_threads_value_self` already has for BT-3484's
    /// loops).
    pub(in crate::core_erlang) fn exception_construct_families(
        &self,
        blocks: &[&Block],
    ) -> ThreadedFamilies {
        let eligible: Vec<VersionPrefix> = self
            .eligible_families()
            .into_iter()
            .filter(|prefix| !matches!(prefix, VersionPrefix::State))
            .collect();
        let matches: Vec<VersionPrefix> = blocks
            .iter()
            .flat_map(|block| {
                self.body_threaded_families(block, &eligible)
                    .as_slice()
                    .to_vec()
            })
            .collect();
        ThreadedFamilies::from_matches(&matches)
    }

    /// Emits `primop 'raw_raise'(Type, Error, Stack)` — the shared re-raise
    /// shape for a caught `<Type, Error, Stack>` triple, used by every
    /// catch-clause fallback arm in the codebase (never `erlang:raise/3`,
    /// which expects a pre-built stacktrace term rather than the raw trace a
    /// catch clause binds). `pub(in crate::core_erlang)` so sibling
    /// modules building their own `try`/`catch` (e.g. `operators.rs`'s
    /// number-on-the-left coercion wrapper, ADR 0116) reuse this
    /// instead of re-emitting the same three-arg primop inline.
    pub(in crate::core_erlang) fn emit_raw_raise(
        type_var: String,
        error_var: String,
        stack_var: String,
    ) -> Document<'static> {
        docvec![
            "primop 'raw_raise'(",
            leaf::var(type_var),
            ", ",
            leaf::var(error_var),
            ", ",
            leaf::var(stack_var),
            ")",
        ]
    }

    /// Generates the NLR-passthrough catch clause preamble shared by both
    /// `generate_on_do` and `generate_on_do_with_mutations`.
    ///
    /// Produces an open-ended fragment; caller appends the `<'true'>` branch
    /// body, the `<'false'>` re-raise arm, and the closing `end end`.
    ///
    /// NLR throws (`{'$bt_nlr', ...}`) must bypass
    /// on:do: so the enclosing method's NLR handler can intercept them.
    #[allow(clippy::too_many_arguments)]
    fn on_do_catch_preamble(
        type_var: &str,
        error_var: &str,
        stack_var: String,
        nlr_tok_var: String,
        nlr_val_var: String,
        nlr_state_var: String,
        nlr_tok_var2: String,
        nlr_val_var2: String,
        other_pair_var: String,
        built_stack_var: String,
        ex_obj_var: String,
        match_var: String,
        ex_class_var: String,
    ) -> Document<'static> {
        docvec![
            "catch <",
            leaf::var(type_var.to_string()),
            ", ",
            leaf::var(error_var.to_string()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> ",
            "case {",
            leaf::var(type_var.to_string()),
            ", ",
            leaf::var(error_var.to_string()),
            "} of ",
            "<{'throw', {'$bt_nlr', ",
            leaf::var(nlr_tok_var),
            ", ",
            leaf::var(nlr_val_var),
            ", ",
            leaf::var(nlr_state_var),
            "}}> when 'true' -> ",
            Self::emit_raw_raise(
                type_var.to_string(),
                error_var.to_string(),
                stack_var.clone(),
            ),
            " ",
            "<{'throw', {'$bt_nlr', ",
            leaf::var(nlr_tok_var2),
            ", ",
            leaf::var(nlr_val_var2),
            "}}> when 'true' -> ",
            Self::emit_raw_raise(
                type_var.to_string(),
                error_var.to_string(),
                stack_var.clone(),
            ),
            " ",
            "<",
            leaf::var(other_pair_var),
            "> when 'true' -> ",
            "let ",
            leaf::var(built_stack_var.clone()),
            " = primop 'build_stacktrace'(",
            leaf::var(stack_var),
            ") in ",
            "let ",
            leaf::var(ex_obj_var.clone()),
            " = call 'beamtalk_exception_handler':'ensure_wrapped'(",
            leaf::var(type_var.to_string()),
            ", ",
            leaf::var(error_var.to_string()),
            ", ",
            leaf::var(built_stack_var),
            ") in ",
            "let ",
            leaf::var(match_var.clone()),
            " = call 'beamtalk_exception_handler':'matches_class'(",
            leaf::var(ex_class_var),
            ", ",
            leaf::var(ex_obj_var),
            ") in ",
            "case ",
            leaf::var(match_var),
            " of ",
            "<'true'> when 'true' -> ",
        ]
    }

    /// Builds the `apply HandlerFun (ExObj)` or `apply HandlerFun ()` fragment
    /// for `on:do:` exception handlers.
    fn make_handler_apply(
        handler_var: String,
        ex_obj_var: String,
        takes_arg: bool,
    ) -> Document<'static> {
        if takes_arg {
            docvec![
                "apply ",
                leaf::var(handler_var),
                " (",
                leaf::var(ex_obj_var),
                ")",
            ]
        } else {
            docvec!["apply ", leaf::var(handler_var), " ()"]
        }
    }

    /// ADR 0122 / BT-3506: whether `block` needs `on:do:`/`ensure:`'s inlined,
    /// state-threading compilation strategy rather than the plain
    /// closure-based one — [`Self::needs_mutation_threading`]'s own answer,
    /// widened for class methods to ALSO cover a `ClassVars` mutation
    /// (`self.classVar := ...` or a same-class self-send).
    ///
    /// [`Self::needs_mutation_threading`]'s class-method branch only ever
    /// checks for a captured-outer-local read+write (the ADR 0110 gap this
    /// widening closes has nothing to do with local variables) — mirroring
    /// the Actor branch's own `analysis.has_state_effects()` check, but only
    /// for `on:do:`/`ensure:`, not the (many) other callers of the shared
    /// `needs_mutation_threading` (loops, list-ops, conditionals, …), whose
    /// own class-method self-send/field-write threading is each a separate,
    /// already-settled question this issue does not reopen.
    ///
    /// Without this, a class-method `on:do:`/`ensure:` whose ONLY mutation
    /// is a same-class self-send or a bare class-var write took the plain
    /// closure path regardless of [`Self::exception_construct_families`]'
    /// own answer — silently discarding a self-send's mutation (BT-3506
    /// shape (b): the closure's own body simply evaluates to its last
    /// expression, with no `{Result, StateAcc, ClassVars}` tuple to carry it
    /// out) or misrouting a bare direct write to the generic
    /// stored-closure-body rejection (`FieldAssignmentInUnsupportedBlock`,
    /// BT-2792's `validate_stored_closure`) instead of the accurate
    /// `ClassVarAssignmentInThreadedBody` this construct's own E1 dispatch
    /// (`generate_exception_body_with_threading_inner`) already produces for
    /// every OTHER threaded body.
    fn block_needs_exception_threading(&self, block: &Block) -> bool {
        let analysis = block_analysis::analyze_block(block);
        self.needs_mutation_threading(&analysis)
            || (self.in_class_method() && analysis.has_state_effects())
    }

    /// Generates `on:do:` — wraps block in try/catch, wraps error as Exception
    /// object and passes to handler block.
    ///
    /// Analyzes both receiver and handler blocks for state mutations and chooses
    /// the appropriate compilation strategy (closure-based vs inlined with state threading).
    pub(in crate::core_erlang) fn generate_on_do(
        &mut self,
        receiver: &Expression,
        ex_class: &Expression,
        handler: &Expression,
    ) -> Result<Document<'static>> {
        // Validate protected block arity (must be 0-arg)
        validate_block_arity_exact(
            receiver,
            0,
            "on:do:",
            "Fix: The protected block must take no arguments:\n\
             \x20 [riskyOperation] on: Exception do: [:e | handle error]",
        )?;
        // Validate handler block arity (must be 0 or 1-arg)
        // Returns true if handler takes an argument, false for 0-arg
        let handler_takes_arg = validate_on_do_handler(handler, "on:do:")?;

        // Check both blocks for field/state mutations
        let receiver_needs = if let Expression::Block(b) = receiver {
            self.block_needs_exception_threading(b)
        } else {
            false
        };
        let handler_needs = if let Expression::Block(b) = handler {
            self.block_needs_exception_threading(b)
        } else {
            false
        };

        if receiver_needs || handler_needs {
            if let (Expression::Block(recv_block), Expression::Block(handler_block)) =
                (receiver, handler)
            {
                return self.generate_on_do_with_mutations(recv_block, ex_class, handler_block);
            }
        }

        // Simple case: no mutations, use closure-based approach
        let block_var = self.fresh_temp_var("BlockFun");
        let ex_class_var = self.fresh_temp_var("ExClass");
        let handler_var = self.fresh_temp_var("HandlerFun");
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let built_stack_var = self.fresh_temp_var("BuiltStack");
        let ex_obj_var = self.fresh_temp_var("ExObj");
        let match_var = self.fresh_temp_var("Match");

        // Capture expression outputs (ADR 0018 bridge pattern)
        let receiver_code = self.expression_doc(receiver)?;
        let ex_class_code = self.expression_doc(ex_class)?;
        let handler_code = self.expression_doc(handler)?;

        let handler_apply =
            Self::make_handler_apply(handler_var.clone(), ex_obj_var.clone(), handler_takes_arg);

        // Fresh variable names for the NLR pattern guard (Core Erlang
        // does not support anonymous `_` wildcards — each must be unique).
        let nlr_tok_var = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var = self.fresh_temp_var("NlrCheckVal");
        // Actor NLR throws include state as a 4th element.
        let nlr_state_var = self.fresh_temp_var("NlrCheckState");
        let nlr_tok_var2 = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var2 = self.fresh_temp_var("NlrCheckVal");
        // Fallback pattern: ONE variable binding the whole 2-tuple (not two separate elements).
        let other_pair_var = self.fresh_temp_var("OtherPair");

        Ok(docvec![
            "let ",
            leaf::var(block_var.clone()),
            " = ",
            receiver_code,
            " in let ",
            leaf::var(ex_class_var.clone()),
            " = ",
            ex_class_code,
            " in let ",
            leaf::var(handler_var),
            " = ",
            handler_code,
            " in try apply ",
            leaf::var(block_var),
            " () ",
            "of ",
            leaf::var(result_var.clone()),
            " -> ",
            leaf::var(result_var),
            " ",
            Self::on_do_catch_preamble(
                &type_var,
                &error_var,
                stack_var.clone(),
                nlr_tok_var,
                nlr_val_var,
                nlr_state_var,
                nlr_tok_var2,
                nlr_val_var2,
                other_pair_var,
                built_stack_var,
                ex_obj_var,
                match_var,
                ex_class_var,
            ),
            handler_apply,
            " ",
            "<'false'> when 'true' -> ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
            " end ",
            "end",
        ])
    }

    /// the real value to seed this construct's `StateAcc` scratch
    /// map from — the actor's own `State` parameter in Actor context
    /// (`current_state_var()`, matching `render_state_prefix`'s bare
    /// `"State"` at version 0), or a fresh empty map everywhere else.
    ///
    /// This scratch map's only job is carrying a try/ensure body's own
    /// local-var mutations (`t := t + 1`) across the real Core Erlang
    /// `try`/`catch` boundary — Erlang bindings made inside `try` are not
    /// visible in `catch`/after, so *some* map is needed regardless of
    /// context. Field writes route through this same map in Actor context
    /// (reusing the real `State`); value-type field mutations
    /// (`VersionPrefix::SelfVt`, BT-3486) and class-method class-var
    /// mutations (`VersionPrefix::ClassVars`, BT-3506) instead ride the
    /// construct's own trailing tuple slot
    /// ([`Self::close_exception_result_tuple`]) — never this map — so an
    /// empty seed outside Actor context is correct, not just a stopgap.
    ///
    /// **Corrected (BT-3506):** this comment previously claimed a
    /// class-method self-send's `ClassVars` rebind is "threaded entirely
    /// separately" via `emit_class_var_result_unwrap`'s own let-chain and so
    /// never needs a slot on this construct's result tuple. That was false —
    /// the let-chain's rebinding happens INSIDE the try-body's own rendered
    /// `Document`, and an Erlang binding made inside `try` is not visible
    /// after it closes, so without the trailing `ClassVars` slot the
    /// mutation was silently discarded on every normal return (confirmed
    /// empirically; see BT-3506's repro). The slot fixes it the same way
    /// BT-3486 already fixed the identical gap for `SelfVt`.
    ///
    /// Before BT-3486's fix, both callers unconditionally called
    /// `current_state_var()`, which at version 0 renders as the bare
    /// identifier `"State"` regardless of context — valid only in Actor
    /// context, where a real `State` parameter exists; in class-method or
    /// value-type context this produced a reference to a nonexistent
    /// variable (`erlc: unbound variable 'State'`).
    fn exception_body_outer_state(&mut self) -> String {
        if self.in_actor_instance_context() {
            self.current_state_var()
        } else {
            "~{}~".to_string()
        }
    }

    /// Generates `on:do:` with state mutation threading.
    ///
    /// Inlines receiver (try body) and handler block bodies with state threading
    /// instead of wrapping them as closures. This ensures field mutations in
    /// handler blocks are properly threaded back to the actor state.
    ///
    /// Generated Core Erlang:
    /// ```erlang
    /// let _ExClass = <ex_class> in
    /// let StateAcc = <current_state> in
    /// try
    ///     <inlined receiver body with state threading>
    ///     StateAccN
    /// of StateAfterTry -> StateAfterTry
    /// catch <Type, Error, RawStack> ->
    ///     let BuiltStack = primop 'build_stacktrace'(RawStack) in
    ///     let ExObj = call 'beamtalk_exception_handler':'ensure_wrapped'(Type, Error, BuiltStack) in
    ///     let Match = call 'beamtalk_exception_handler':'matches_class'(ExClass, ExObj) in
    ///     case Match of
    ///         true  -> let _e = ExObj in <handler body with threading> StateAccM
    ///         false -> primop 'raw_raise'(Type, Error, RawStack)
    /// ```
    ///
    /// ADR 0122 / BT-3506: when either block mutates a `ClassVars`/`SelfVt`
    /// family the current context makes eligible, both returned tuples grow
    /// a trailing slot carrying that arm's own version — see
    /// [`Self::exception_construct_families`].
    fn generate_on_do_with_mutations(
        &mut self,
        receiver_block: &Block,
        ex_class: &Expression,
        handler_block: &Block,
    ) -> Result<Document<'static>> {
        if self.is_repl_mode() {
            self.set_repl_loop_mutated(true);
        }

        let ex_class_var = self.fresh_temp_var("ExClass");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let built_stack_var = self.fresh_temp_var("BuiltStack");
        let ex_obj_var = self.fresh_temp_var("ExObj");
        let match_var = self.fresh_temp_var("Match");
        let state_after_try = self.fresh_temp_var("StateAfterTry");
        // Unique names for NLR pattern variables (no anonymous _ in Core Erlang).
        let nlr_tok_var = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var = self.fresh_temp_var("NlrCheckVal");
        // Actor NLR throws include state as a 4th element.
        let nlr_state_var = self.fresh_temp_var("NlrCheckState");
        let nlr_tok_var2 = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var2 = self.fresh_temp_var("NlrCheckVal");
        // Fallback pattern: ONE variable binding the whole 2-tuple (not two separate elements).
        let other_pair_var = self.fresh_temp_var("OtherPair");

        // Bind exception class
        let ex_class_code = self.expression_doc(ex_class)?;
        // Rename current state to StateAcc for uniform threading
        let current_state = self.exception_body_outer_state();
        // seed `__local__` keys for outer locals mutated by either the
        // try (receiver) block or the handler block — only one of the two ever
        // runs at a given call, so (mirroring `ifTrue:ifFalse:`'s two branches)
        // the key must be present in the base state even on the path that
        // didn't itself write it, or the method-body sequencer's extraction
        // `maps:get/2` would hit a missing key.
        let (seed_doc, base_state) =
            self.seed_conditional_locals(&[receiver_block, handler_block], &current_state);
        // ADR 0122 / BT-3506: decided from the AST BEFORE either arm is
        // generated, so both arms' return tuples agree on the shape even
        // though only one of them may actually contain the mutation — the
        // extraction the consumer emits after the construct is fixed at
        // compile time and runs whichever arm executed.
        let families = self.exception_construct_families(&[receiver_block, handler_block]);
        // The construct's own live baseline for whichever family `families`
        // names (0 when `families` is empty; never read in that case) —
        // restored between arms (they are siblings: only one ever runs) and
        // once more after the construct. `outer_self`/`outer_class_var` are
        // ALSO captured unconditionally (regardless of which family, if any,
        // is active) so both counters can be restored between/after arms the
        // same way the original `SelfVt`-only code always restored
        // `self_version` even in contexts where it never moved.
        let outer_self = self.self_version();
        let outer_class_var = self.class_var_version();
        let outer_version = families
            .as_slice()
            .first()
            .map_or(0, |prefix| self.family_version(prefix));

        let mut docs: Vec<Document<'static>> = vec![docvec![
            "let ",
            leaf::var(ex_class_var.clone()),
            " = ",
            ex_class_code,
            " in ",
            seed_doc,
            "let StateAcc = ",
            leaf::var(base_state),
            " in try ",
        ]];

        // Generate try body (receiver block) with state threading
        let (try_result_var, try_final, try_slot) =
            self.push_exception_arm(&mut docs, receiver_block, &families, outer_version)?;
        // Return {Result, State[, Family]} from try body
        // Success: pass the tuple through + catch clause with NLR passthrough.
        // NLR re-raise via on_do_catch_preamble (see generate_on_do).
        docs.push(self.close_exception_result_tuple(
            try_result_var,
            try_final,
            &families,
            &try_slot,
        ));
        docs.push(docvec![
            "of ",
            leaf::var(state_after_try.clone()),
            " -> ",
            leaf::var(state_after_try),
            " ",
            Self::on_do_catch_preamble(
                &type_var,
                &error_var,
                stack_var.clone(),
                nlr_tok_var,
                nlr_val_var,
                nlr_state_var,
                nlr_tok_var2,
                nlr_val_var2,
                other_pair_var,
                built_stack_var,
                ex_obj_var.clone(),
                match_var,
                ex_class_var,
            ),
        ]);
        // Bind handler parameter (e.g., [:e | ...] binds e to exception object)
        self.push_scope();
        if let Some(param) = handler_block.parameters.first() {
            let param_var = Self::to_core_erlang_var(&param.name);
            self.bind_var(&param.name, &param_var);
            docs.push(docvec![
                "let ",
                leaf::var(param_var),
                " = ",
                leaf::var(ex_obj_var),
                " in ",
            ]);
        }

        // Generate handler body with state threading (from original StateAcc)
        self.set_self_version(outer_self);
        self.set_class_var_version(outer_class_var);
        let (handler_result_var, handler_final, handler_slot) =
            self.push_exception_arm(&mut docs, handler_block, &families, outer_version)?;
        // Return {Result, State[, Family]} from handler
        docs.push(self.close_exception_result_tuple(
            handler_result_var,
            handler_final,
            &families,
            &handler_slot,
        ));
        self.pop_scope();

        // ADR 0111 Addendum 5: the try body and the handler body
        // are sibling with_branch_context frames (only one of them ever
        // actually runs at a given call, but both are compiled) — each
        // `generate_exception_body_with_threading` call mints its own fresh
        // FrameId (`current_branch_frame`) and `verify()`s its own real IR
        // internally, so either arm independently reaching the same
        // StateAcc version number as the other is correctly NOT a
        // NonLinearVersion violation — the check that used to run here
        // (`check_branch_frame_linearity`) is gone; real per-frame
        // verification now happens where the IR is actually built (inside
        // `generate_exception_body_with_threading_inner`).

        // Re-raise non-matching exceptions; close the matches_class case and the outer NLR case.
        docs.push(docvec![
            "<'false'> when 'true' -> ",
            Self::emit_raw_raise(type_var.clone(), error_var.clone(), stack_var),
            " end end",
        ]);
        // ADR 0122 / BT-3506: an arm's `let <bare> = … in ` shadow and its
        // own version chain live entirely inside that arm's Core Erlang
        // scope; the construct's single legitimate advance of the method's
        // live version is the consumer's post-construct rebind
        // (`generate_vt_exception_construct_open`), so leave both counters
        // as found.
        self.set_self_version(outer_self);
        self.set_class_var_version(outer_class_var);

        Ok(Document::Vec(docs))
    }

    /// Generates `ensure:` — wraps block in try, always runs cleanup block.
    ///
    /// Analyzes both receiver and cleanup blocks for state mutations and chooses
    /// the appropriate compilation strategy.
    pub(in crate::core_erlang) fn generate_ensure(
        &mut self,
        receiver: &Expression,
        cleanup: &Expression,
    ) -> Result<Document<'static>> {
        // Validate cleanup block arity (must be 0-arg)
        validate_block_arity_exact(
            cleanup,
            0,
            "ensure:",
            "Fix: The cleanup block must take no arguments:\n\
             \x20 [operation] ensure: [resource close]",
        )?;

        // Check both blocks for field/state mutations
        let receiver_needs = if let Expression::Block(b) = receiver {
            self.block_needs_exception_threading(b)
        } else {
            false
        };
        let cleanup_needs = if let Expression::Block(b) = cleanup {
            self.block_needs_exception_threading(b)
        } else {
            false
        };

        if receiver_needs || cleanup_needs {
            if let (Expression::Block(recv_block), Expression::Block(cleanup_block)) =
                (receiver, cleanup)
            {
                return self.generate_ensure_with_mutations(recv_block, cleanup_block);
            }
        }

        // Simple case: no mutations
        let block_var = self.fresh_temp_var("BlockFun");
        let cleanup_var = self.fresh_temp_var("CleanupFun");
        let try_result_var = self.fresh_temp_var("TryResult");
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");

        // Capture expression outputs (ADR 0018 bridge pattern)
        let receiver_code = self.expression_doc(receiver)?;
        let cleanup_code = self.expression_doc(cleanup)?;

        Ok(docvec![
            "let ",
            leaf::var(block_var.clone()),
            " = ",
            receiver_code,
            " in let ",
            leaf::var(cleanup_var.clone()),
            " = ",
            cleanup_code,
            " in try let ",
            leaf::var(try_result_var.clone()),
            " = apply ",
            leaf::var(block_var),
            " () in ",
            leaf::var(try_result_var),
            " ",
            "of ",
            leaf::var(result_var.clone()),
            " -> let _ = apply ",
            leaf::var(cleanup_var.clone()),
            " () in ",
            leaf::var(result_var),
            " ",
            "catch <",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(error_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> do apply ",
            leaf::var(cleanup_var),
            " () ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
        ])
    }

    /// Generates `ensure:` with state mutation threading.
    ///
    /// Inlines receiver (try body) and cleanup block bodies with state threading.
    /// On success, cleanup runs with the try body's final state.
    /// On error, cleanup runs with the original state, then re-raises.
    ///
    /// Generated Core Erlang:
    /// ```erlang
    /// let StateAcc = <current_state> in
    /// try
    ///     <inlined try body with state threading>
    ///     StateAccN
    /// of StateAfterTry ->
    ///     let StateAcc = StateAfterTry in
    ///     <inlined cleanup with state threading>
    ///     StateAccM
    /// catch <Type, Error, Stack> ->
    ///     <inlined cleanup with state threading from original StateAcc>
    ///     primop 'raw_raise'(Type, Error, Stack)
    /// ```
    ///
    /// ADR 0122 / BT-3506: when either block mutates a `ClassVars`/`SelfVt`
    /// family the current context makes eligible, every returned tuple above
    /// grows a trailing slot carrying that arm's own version — see
    /// [`Self::exception_construct_families`].
    fn generate_ensure_with_mutations(
        &mut self,
        receiver_block: &Block,
        cleanup_block: &Block,
    ) -> Result<Document<'static>> {
        if self.is_repl_mode() {
            self.set_repl_loop_mutated(true);
        }

        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let state_after_try = self.fresh_temp_var("StateAfterTry");

        // Rename current state to StateAcc
        let current_state = self.exception_body_outer_state();
        // seed `__local__` keys for outer locals mutated by the try
        // (receiver) block or the cleanup block — mirrors `on:do:`'s seeding
        // (see `generate_on_do_with_mutations`) so a local written only
        // conditionally within one of the blocks (e.g. behind a nested
        // `ifTrue:`) still has its key present for the method-body
        // sequencer's extraction `maps:get/2`, even though the two blocks
        // here run sequentially rather than as alternatives.
        let (seed_doc, base_state) =
            self.seed_conditional_locals(&[receiver_block, cleanup_block], &current_state);
        // ADR 0122 / BT-3506: see `generate_on_do_with_mutations` — decided
        // from the AST up front so every arm's return tuple agrees on the
        // shape.
        let families = self.exception_construct_families(&[receiver_block, cleanup_block]);
        // The construct's own live baselines — restored between arms and
        // once more after the construct (see `generate_on_do_with_mutations`).
        let outer_self = self.self_version();
        let outer_class_var = self.class_var_version();
        let outer_version = families
            .as_slice()
            .first()
            .map_or(0, |prefix| self.family_version(prefix));
        let mut docs: Vec<Document<'static>> = vec![docvec![
            seed_doc,
            "let StateAcc = ",
            leaf::var(base_state),
            " in try ",
        ]];

        // Generate try body with state threading
        let (try_result_var, try_final, try_slot) =
            self.push_exception_arm(&mut docs, receiver_block, &families, outer_version)?;
        // Return {Result, State[, Family]} from try body
        docs.push(self.close_exception_result_tuple(
            try_result_var,
            try_final,
            &families,
            &try_slot,
        ));

        // Success: run cleanup starting from try body's state
        // Extract Result and State from {Result, State} tuple using element/N
        let result_from_try = self.fresh_temp_var("TryResult");
        docs.push(docvec![
            "of ",
            leaf::var(state_after_try.clone()),
            " -> let ",
            leaf::var(result_from_try.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(state_after_try.clone()),
            ") in let StateAcc = call 'erlang':'element'(2, ",
            leaf::var(state_after_try.clone()),
            ") in ",
        ]);

        // On the SUCCESS path the cleanup runs after the try body, so
        // it must see the try body's mutation — but an Erlang binding made
        // inside `try` is not in scope in the `of` arm, so the try's own
        // version is unreachable here. Re-seed from the tuple's trailing
        // slot, shadowing the *unversioned* bare name exactly as the
        // `StateAcc = element(2, …)` line immediately above shadows `StateAcc`
        // — and for the same reason: an arm's `ThreadedIr` frame treats
        // version 0 as its implicit, always-bound entry parameter (see
        // `check_use` in `threaded_ir/verify.rs`), so a cleanup arm handed a
        // version > 0 baseline would consume a version its own frame never
        // produces. Version 0 in, the arm's own version 1, 2, … out.
        let cleanup_success_baseline = if let Some(prefix) = families.as_slice().first() {
            let bare = Self::family_bare_var_name(prefix);
            docs.push(docvec![
                "let ",
                leaf::var(bare.to_string()),
                " = call 'erlang':'element'(3, ",
                leaf::var(state_after_try),
                ") in ",
            ]);
            self.set_family_version(prefix, 0);
            0
        } else {
            outer_version
        };

        let cleanup_success_arm =
            self.generate_exception_body_with_threading(cleanup_block, &families)?;
        let cleanup_success_slot =
            Self::exception_family_slot(&families, cleanup_success_baseline, &cleanup_success_arm);
        docs.push(cleanup_success_arm.doc);
        // Return try body result with cleanup's final state
        docs.push(self.close_exception_result_tuple(
            result_from_try,
            cleanup_success_arm.state_version,
            &families,
            &cleanup_success_slot,
        ));
        // ADR 0122 / BT-3506: the error path below is a SIBLING of the whole
        // `of` arm, not its successor, so it starts again from the
        // construct's own pre-`try` baselines rather than from the success
        // path's re-seed.
        self.set_self_version(outer_self);
        self.set_class_var_version(outer_class_var);

        // Error: run cleanup for side effects (from original StateAcc), then re-raise
        docs.push(docvec![
            "catch <",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(error_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> ",
        ]);

        // Cleanup body generates state mutations that are discarded (re-raise
        // follows) — including any extra-family version it mints: this path
        // re-raises rather than returning a tuple, so it has no trailing
        // slot to carry one (BT-3486/BT-3506). It still gets the version-0
        // arm seed, since its own frame must verify like any other.
        self.push_exception_arm(&mut docs, cleanup_block, &families, outer_version)?;

        // ADR 0111 Addendum 5: three sibling with_branch_context
        // frames — the try body, the success-path cleanup run, and the
        // error-path cleanup run (`cleanup_block` is compiled twice, once
        // per path, each its own arm) — each `generate_exception_body_with_threading`
        // call mints its own fresh FrameId and `verify()`s its own real IR
        // internally, so any two independently reaching the same StateAcc
        // version number is correctly NOT a NonLinearVersion violation —
        // see the matching comment in `generate_on_do_with_mutations`.

        docs.push(docvec![
            " ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
        ]);
        // ADR 0122 / BT-3506: leave the method's live versions as found — see
        // the matching restore at the end of `generate_on_do_with_mutations`.
        self.set_self_version(outer_self);
        self.set_class_var_version(outer_class_var);

        Ok(Document::Vec(docs))
    }

    /// Builds the Tier 1 (pure protected block) `try`/`catch` body for
    /// `generate_on_do_structural_fallback` — factored out to keep that
    /// function under clippy's line-count limit. Reuses
    /// `on_do_catch_preamble`'s NLR-passthrough + `matches_class` structure;
    /// only the handler's tier (arity 0 = pure 0-arg, arity 1 = pure 1-arg,
    /// anything else = stateful) is discriminated dynamically here, since it
    /// isn't known statically the way `generate_on_do` knows it from the
    /// literal block AST.
    fn generate_on_do_tier1_try(
        &mut self,
        self_var: &'static str,
        ex_class_param: String,
        handler_param: String,
        class_name: &str,
    ) -> Document<'static> {
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let built_stack_var = self.fresh_temp_var("BuiltStack");
        let ex_obj_var = self.fresh_temp_var("ExObj");
        let match_var = self.fresh_temp_var("Match");
        // Two NLR throw shapes `on_do_catch_preamble` matches against: the
        // 4-tuple actor-NLR-with-state variant and the plain 3-tuple
        // variant — not nesting levels, hence the `_with_state`/
        // `_no_state` naming rather than a generic numeric suffix.
        let nlr_tok_with_state_var = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_with_state_var = self.fresh_temp_var("NlrCheckVal");
        let nlr_state_var = self.fresh_temp_var("NlrCheckState");
        let nlr_tok_no_state_var = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_no_state_var = self.fresh_temp_var("NlrCheckVal");
        let other_pair_var = self.fresh_temp_var("OtherPair");

        // arity 1 is ambiguous between a pure 1-arg handler and a stateful
        // 0-arg handler — same documented ambiguity as Block's blockValue*
        // family (deferred disambiguation); anything else is Tier 2.
        let handler_stateful_error = self.generate_stateful_block_dispatch_error(
            "on:do:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let handler_dispatch = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(handler_param.clone()),
            ", 0) of <'true'> when 'true' -> apply ",
            leaf::var(handler_param.clone()),
            " () <'false'> when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(handler_param.clone()),
            ", 1) of <'true'> when 'true' -> apply ",
            leaf::var(handler_param),
            " (",
            leaf::var(ex_obj_var.clone()),
            ") <'false'> when 'true' -> ",
            handler_stateful_error,
            " end end",
        ];

        let catch_preamble = Self::on_do_catch_preamble(
            &type_var,
            &error_var,
            stack_var.clone(),
            nlr_tok_with_state_var,
            nlr_val_with_state_var,
            nlr_state_var,
            nlr_tok_no_state_var,
            nlr_val_no_state_var,
            other_pair_var,
            built_stack_var,
            ex_obj_var,
            match_var,
            ex_class_param,
        );

        docvec![
            "try apply ",
            Document::Str(self_var),
            " () of ",
            leaf::var(result_var.clone()),
            " -> ",
            leaf::var(result_var),
            " ",
            catch_preamble,
            handler_dispatch,
            " <'false'> when 'true' -> ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
            " end end",
        ]
    }

    /// Generates the fallback method body for `onDo` — Block's
    /// `on:do:`. Reached only via generic dispatch bypassing the call-site
    /// interception `generate_on_do` normally provides (e.g. `perform:`). See
    /// `generate_block_value_structural_fallback` for the general
    /// Tier 1/Tier 2 discrimination rationale.
    ///
    /// Reuses `on_do_catch_preamble`'s NLR-passthrough + `matches_class`
    /// structure so the Tier 1 (pure) case stays behaviourally identical to
    /// the AST-driven `generate_on_do` — only the receiver/handler *tier*
    /// discrimination differs, since a generically dispatched handler's
    /// declared arity (0 or 1) isn't known statically the way it is when
    /// `generate_on_do` reads it straight off the literal block AST.
    /// `current_method_params` are `[ExClass, Handler]` (the `on:`/`do:`
    /// keyword arguments); `Self` is the protected block.
    pub(in crate::core_erlang) fn generate_on_do_structural_fallback(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let ex_class_param = self
            .current_method_params
            .first()
            .cloned()
            .unwrap_or_else(|| "ExClass".to_string());
        let handler_param = self
            .current_method_params
            .get(1)
            .cloned()
            .unwrap_or_else(|| "Handler".to_string());

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let params_doc = join(
            [
                leaf::var(ex_class_param.clone()),
                leaf::var(handler_param.clone()),
            ],
            &Document::Str(", "),
        );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'('onDo', [",
            params_doc,
            "], ",
            Document::Str(self_var),
            ")",
        ];

        let tier1_try =
            self.generate_on_do_tier1_try(self_var, ex_class_param, handler_param, class_name);

        let self_stateful_error = self.generate_stateful_block_dispatch_error(
            "on:do:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 0) of <'true'> when 'true' -> ",
            tier1_try,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 1) of <'true'> when 'true' -> ",
            self_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
    }

    /// Generates the fallback method body for `ensure` — Block's
    /// `ensure:`. Reached only via generic dispatch bypassing the call-site
    /// interception `generate_ensure` normally provides (e.g. `perform:`).
    /// See `generate_block_value_structural_fallback` for the
    /// general Tier 1/Tier 2 discrimination rationale.
    ///
    /// Both receiver and cleanup block must be Tier 1 (pure, 0-arg funs) for
    /// the generic try/catch below to be correct — Core Erlang's try/catch
    /// mechanics don't themselves need the block's AST, only ADR-0041's
    /// state-threading convention does, so the pure case is fully generic.
    /// `current_method_params[0]` is the cleanup block; `Self` is the
    /// protected block.
    pub(in crate::core_erlang) fn generate_ensure_structural_fallback(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let cleanup_param = self
            .current_method_params
            .first()
            .cloned()
            .unwrap_or_else(|| "CleanupBlock".to_string());

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'('ensure', [",
            leaf::var(cleanup_param.clone()),
            "], ",
            Document::Str(self_var),
            ")",
        ];

        let try_result_var = self.fresh_temp_var("TryResult");
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");

        let tier1_try = docvec![
            "try let ",
            leaf::var(try_result_var.clone()),
            " = apply ",
            Document::Str(self_var),
            " () in ",
            leaf::var(try_result_var),
            " of ",
            leaf::var(result_var.clone()),
            " -> let _ = apply ",
            leaf::var(cleanup_param.clone()),
            " () in ",
            leaf::var(result_var),
            " catch <",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(error_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> do apply ",
            leaf::var(cleanup_param.clone()),
            " () ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
        ];

        let cleanup_stateful_error = self.generate_stateful_block_dispatch_error(
            "ensure:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let cleanup_tier_check = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(cleanup_param.clone()),
            ", 0) of <'true'> when 'true' -> ",
            tier1_try,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(cleanup_param),
            ", 1) of <'true'> when 'true' -> ",
            cleanup_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch.clone(),
            " end end",
        ];

        let self_stateful_error = self.generate_stateful_block_dispatch_error(
            "ensure:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 0) of <'true'> when 'true' -> ",
            cleanup_tier_check,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 1) of <'true'> when 'true' -> ",
            self_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
    }

    /// Generates block body expressions with state mutation threading.
    ///
    /// Follows the same pattern as `generate_while_body_with_threading`:
    /// - Sets `in_loop_body = true` so field reads/writes use `StateAcc`
    /// - Resets `state_version` to 0 (`StateAcc` is version 0)
    /// - Threads field assignments, self-sends, and local var assignments
    /// - Returns an [`ExceptionArm`] — the Document holding the generated
    ///   code, the variable holding the last expression's result, the final
    ///   state version number, and (BT-3486/BT-3506) the arm's final version
    ///   of whichever extra family `families` names, when it actually
    ///   mutated it
    ///
    /// The caller must have already bound `StateAcc` to the current state
    /// before calling this function.
    ///
    /// BT-3486/BT-3506: the extra family's version must be read from INSIDE
    /// `with_branch_context` — that guard lets each arm inherit the outer
    /// version on entry (so both arms are true siblings, both sourcing their
    /// mutation chains from the same pre-`try` baseline) and restores it on
    /// exit (so an arm's own in-`try` version, unreachable from outside the
    /// Core Erlang `try`, never leaks into the enclosing method's live
    /// version). That restore is exactly why the caller cannot read it back
    /// afterwards, and why it is returned here.
    fn generate_exception_body_with_threading(
        &mut self,
        body: &Block,
        families: &ThreadedFamilies,
    ) -> Result<ExceptionArm> {
        self.with_branch_context(|this| {
            this.generate_exception_body_with_threading_inner(body, families)
        })
    }

    /// Inner implementation called inside `with_branch_context`.
    ///
    /// ADR 0111 Addendum 5: this arm's mutation sequence is built
    /// as real [`ThreadedStmt`]s — the E1–E7 per-shape decomposition table
    /// (E1/E3 reuse `conditionals.rs`'s C1/C2 helpers,
    /// `lower_field_assignment_bind`/`lower_local_var_assignment_bind`,
    /// exactly — same shape, same mint order) — then `verify()`d and
    /// `render()`d via `conditionals.rs`'s `verify_and_render_branch_arm`.
    /// Rule 2's separator divergence (this file inserts a literal `" "`
    /// between SOURCE-level statements, `conditionals.rs` inserts none at
    /// all) is reproduced by pushing that literal space as its own
    /// `ThreadedStmt::Statement` at each source-statement boundary —
    /// **not** by feeding the flat per-shape `Bind`/`Statement` sequence
    /// through `render_loop_body_statements` (that function separates every
    /// *raw* `ThreadedStmt` entry, which would inject a spurious extra
    /// space inside any shape that itself decomposes into more than one
    /// entry, e.g. E1's Statement+Bind pair — a real double-space bug, not
    /// a cosmetic one).
    /// `verify_and_render_branch_arm`'s plain `render()` has no separator
    /// of its own, so the manually-inserted space is the only one that
    /// ends up in the output, at exactly the position this same
    /// `if i > 0 { docs.push(" ") }` loop below has always put it — this
    /// is the loop's own IR-producing form, not a match against some
    /// other, deleted implementation.
    #[allow(clippy::too_many_lines)]
    fn generate_exception_body_with_threading_inner(
        &mut self,
        body: &Block,
        families: &ThreadedFamilies,
    ) -> Result<ExceptionArm> {
        let frame = self.current_branch_frame();
        // the baseline this arm inherits, for whichever extra family
        // `families` names (see `generate_exception_body_with_threading`'s
        // doc comment) — compared against the post-body version below to
        // detect a mutation, exactly as `build_vt_conditional_branch_pieces_inner`
        // does for a conditional arm's own `SelfVt`/`ClassVars` diff.
        let family_before = families
            .as_slice()
            .first()
            .map(|prefix| (prefix.clone(), self.family_version(prefix)));
        // push a scope so a local-var assignment's `bind_var` rebind
        // (from `lower_local_var_assignment_bind`) is scoped to this try
        // body and doesn't leak into the enclosing method scope — matching the
        // bracket `generate_conditional_branch_inline` already has (conditionals.rs).
        self.push_scope();

        let has_direct_field_assignments = body
            .body
            .iter()
            .any(|s| Self::is_field_assignment(&s.expression));

        let mut result_var = "'nil'".to_string();
        let mut stmts: Vec<ThreadedStmt> = Vec::new();

        for (i, stmt) in body.body.iter().enumerate() {
            let expr = &stmt.expression;
            let span = expr.span();
            if i > 0 {
                // Rule 2: the literal space `generate_exception_body_with_threading_inner`
                // has always inserted between source-level statements —
                // modeled as its own opaque Statement so it renders exactly
                // once per statement boundary, never inside a shape's own
                // multi-entry decomposition.
                stmts.push(ThreadedStmt::Statement(Document::Str(" "), span));
            }
            let is_last = i == body.body.len() - 1;

            // A value-type `self.field := ...` write nested inside a further
            // construct of this arm's own body — most notably another
            // `on:do:`/`ensure:` — is not a bare top-level statement, so
            // `block_writes_vt_self_field` (top-level-only, same reason as
            // `loop_body_threads_value_self`) cannot see it and the
            // enclosing construct's `Self` slot misses the mutation. Reject
            // it with the same diagnostic
            // [`CoreErlangGenerator::reject_unthreadable_value_self_field_write`]
            // gives the identical loop-nesting shape. This single
            // per-statement pass covers every exception-construct arm (try
            // body, `on:do:` handler, `ensure:` cleanup), since all three
            // lower through this function. A no-op outside value-type
            // context, and a no-op for this statement when it is itself the
            // bare top-level write `exception_construct_families`
            // already threads.
            self.reject_unthreadable_value_self_field_write(expr, Self::is_field_assignment(expr))?;
            // BT-3522: the `ClassVars` half of the same safety net, which
            // this loop was missing — only `SelfVt` had one. A class-var
            // mutation (bare write or same-class self-send) buried inside a
            // NESTED BLOCK of this statement is invisible to E1..E7's own
            // per-shape Bind construction, so the construct's trailing
            // `ClassVars` slot never carries it and it was silently
            // discarded on normal return. See
            // [`CoreErlangGenerator::reject_unthreadable_class_var_mutation`]
            // for why this one walks nested blocks specifically rather than
            // the whole statement subtree (a mutation in the statement's own
            // sub-expression IS carried, via `thread_ahead`).
            self.reject_unthreadable_class_var_mutation(expr)?;

            if Self::is_field_assignment(expr) {
                // E1 — same shape/mint-order as C1; reused directly.
                let _val_var = self.lower_field_assignment_bind(expr, frame, span, &mut stmts)?;
                if is_last {
                    // Field assignment returns the assigned value
                    // The val was already bound by lower_field_assignment_bind.
                    // Use the current state var for the state, and the assigned value as result
                    // Note: lower_field_assignment_bind binds _ValN = <value>
                    // We need to capture what was assigned - use nil since field assignment
                    // semantically returns the value but we don't easily have the var name here
                    result_var = "'nil'".to_string();
                }
            } else if self.is_actor_self_send(expr) {
                // E2 — the dispatch-open helper both emits the dispatch
                // Statement and bumps the state version; split via
                // `generate_self_dispatch_call_doc` so the bump becomes a
                // real Bind (Direct rebind, `element(2, _SD)`) instead of
                // living inside an opaque Statement's text.
                //
                // ADR 0118 phase 2a: `self log: (self nextId)` —
                // thread the arguments' own self-sends ahead of this
                // dispatch via [`Self::sequence_children`] (the sequencing
                // rule) instead of the planner's `hoist_self_send_arguments`.
                // `expr` itself stays on the dispatch call below, NOT
                // `threaded_expression`'s producer path: `is_actor_self_send`
                // (this arm's guard) is WIDER than `is_dispatching_actor_self_send`
                // (the producer's gate) — it also matches a well-known
                // selector (`self class`, `self hash`, …) that
                // `generate_self_dispatch_call_doc` dispatches uniformly
                // through `safe_dispatch` here (an `on:do:`/`ensure:` body's
                // own long-standing behavior, unrelated to this migration),
                // where the producer would instead intercept it specially —
                // a real behavior change this call site must not make.
                let arg_children: Vec<&Expression> =
                    if let Expression::MessageSend { arguments, .. } = expr.unwrap_parens() {
                        arguments.iter().collect()
                    } else {
                        Vec::new()
                    };
                let (arg_prelude, arg_scope) = self.sequence_children(&arg_children, frame)?;
                stmts.extend(arg_prelude);
                let source_version = self.state_version();
                let (call_doc, dispatch_var) = self.generate_self_dispatch_call_doc(expr)?;
                self.finish_precompiled_scope(arg_scope)?;
                stmts.push(ThreadedStmt::Statement(call_doc, span));
                let target_version = self.state_version();
                stmts.push(ThreadedStmt::Bind {
                    target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                    source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                    op: BindOp::Direct(ValueRef::Doc(docvec![
                        "call 'erlang':'element'(2, ",
                        leaf::var(dispatch_var.clone()),
                        ")",
                    ])),
                    shadow_write: false,
                    span,
                });
                if is_last {
                    // Self-dispatch result is in dispatch_var
                    let rv = self.fresh_temp_var("ExResult");
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "let ",
                            leaf::var(rv.clone()),
                            " = call 'erlang':'element'(1, ",
                            leaf::var(dispatch_var),
                            ") in ",
                        ],
                        span,
                    ));
                    result_var = rv;
                }
            } else if Self::is_local_var_assignment(expr) {
                // E3 — same shape/mint-order as C2/C3/C4; reused directly.
                let _val_var =
                    self.lower_local_var_assignment_bind(expr, frame, span, &mut stmts)?;
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                // E4 — exempt from Bind modeling, same as C5: no state
                // version is produced or consumed, every binding is plain.
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    stmts.push(ThreadedStmt::Statement(d, span));
                }
            } else if is_last {
                if has_direct_field_assignments {
                    // E6 — has_direct_field_assignments sub-branch.
                    // ADR 0118 phase 2a: thread every
                    // state-effecting sub-expression (`1 + (self bump)`) —
                    // AND, since ADR 0118 phase 5b widened
                    // `subexpr_needs_prelude` to recognize a class-var
                    // producer too, a bare class-method self-send with no
                    // enclosing assignment (e.g. `self bump` as this try
                    // body's last statement) — as real `Bind`s ahead of the
                    // compile, via `thread_ahead`. The plain compile below
                    // then reads the already-threaded value back via
                    // `precompiled_subexprs` substitution; no open let-chain
                    // reaches this point any more.
                    let hoist_scope = self.thread_ahead(expr, &mut stmts, frame)?;
                    let rv = self.fresh_temp_var("ExResult");
                    let expr_doc = self.expression_doc(expr)?;
                    self.finish_precompiled_scope(hoist_scope)?;
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let ", leaf::var(rv.clone()), " = ", expr_doc, " in"],
                        span,
                    ));
                    result_var = rv;
                } else {
                    // Last expression with no direct field assignments.
                    // If this is a nested control flow construct returning {Result, State},
                    // destructure it. Otherwise just capture the result.
                    if self.control_flow_has_mutations(expr) {
                        // E5 — nested mutation construct returns {Result,
                        // State} tuple; C10-last's shape with ExResult
                        // naming. Legacy read the target state name via
                        // `peek_next_state_var()` (no mint) before building
                        // the expr doc purely to have the LHS text ready —
                        // the Bind below re-derives the same rendered name
                        // from the version number instead, so the peek call
                        // itself is no longer needed (it never minted
                        // anything; only the ordering of the real mints
                        // around it matters, and that ordering — Tuple,
                        // ExResult, expr doc's own mints, then the bump —
                        // is unchanged below).
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let rv = self.fresh_temp_var("ExResult");
                        let expr_doc = self.expression_doc(expr)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                expr_doc,
                                " in let ",
                                leaf::var(rv.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            shadow_write: false,
                            span,
                        });
                        result_var = rv;
                    } else {
                        // E6/E7 — plain expression, no direct field
                        // assignments in this body. See the E6
                        // has_direct_field_assignments sub-branch above.
                        let hoist_scope = self.thread_ahead(expr, &mut stmts, frame)?;
                        let rv = self.fresh_temp_var("ExResult");
                        let expr_doc = self.expression_doc(expr)?;
                        self.finish_precompiled_scope(hoist_scope)?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["let ", leaf::var(rv.clone()), " = ", expr_doc, " in"],
                            span,
                        ));
                        result_var = rv;
                    }
                }
            } else {
                // E7 — non-last plain expression. A discarded
                // non-last statement must keep a class-var mutation visible
                // to later statements in this same try body (a second
                // self-send later must see the first one's already-bumped
                // `ClassVarsN`). ADR 0118 phase 5b: `thread_ahead`
                // now threads any such producer into `stmts` as a real
                // `Bind`, in the SAME frame every later statement in this
                // body shares — visible to them by construction, without
                // the old lexical-nesting trick — so the plain compile
                // below never has an open scope to propagate.
                // ADR 0118 phase 2a: see the E6 sub-branch above.
                let hoist_scope = self.thread_ahead(expr, &mut stmts, frame)?;
                let expr_doc = self.expression_doc(expr)?;
                self.finish_precompiled_scope(hoist_scope)?;
                stmts.push(ThreadedStmt::Statement(
                    docvec!["let _ = ", expr_doc, " in"],
                    span,
                ));
            }
        }

        let final_state_version = self.state_version();
        let family_mutated_version = family_before.and_then(|(prefix, before)| {
            let after = self.family_version(&prefix);
            (after != before).then_some((prefix, after))
        });
        self.pop_scope();
        let (doc, _) =
            self.verify_and_render_branch_arm(stmts, frame, final_state_version, body.span);
        Ok(ExceptionArm {
            doc,
            result_var,
            state_version: final_state_version,
            family_mutated_version,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::threaded_ir::{self, ThreadingMode};
    use super::super::StateAccFallbackReason;
    use super::*;
    use crate::core_erlang::tests::codegen;
    use beamtalk_core::source_analysis::Span;

    #[test]
    fn test_on_do_generates_try_catch_with_nlr_passthrough() {
        // on:do: generates a try/catch via closure approach with exception wrapping,
        // class matching, and NLR passthrough (re-raises $bt_nlr throws)
        let src =
            "Actor subclass: Srv\n  state: x = 0\n\n  run =>\n    [42] on: Error do: [:e | 0]\n";
        let code = codegen(src);
        assert!(
            code.contains("try apply"),
            "on:do: should generate a try/catch via apply. Got:\n{code}"
        );
        assert!(
            code.contains("'beamtalk_exception_handler':'ensure_wrapped'"),
            "on:do: should wrap the error via ensure_wrapped. Got:\n{code}"
        );
        assert!(
            code.contains("'beamtalk_exception_handler':'matches_class'"),
            "on:do: should check the class via matches_class. Got:\n{code}"
        );
        // NLR throws must be re-raised, not caught by on:do:
        assert!(
            code.contains("'$bt_nlr'"),
            "on:do: should detect NLR throws. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "on:do: should re-raise NLR throws via raw_raise. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_in_class_method_simple() {
        // ensure: in a class method (no mutations) should compile
        let src = "Object subclass: Foo\n\n  class bar =>\n    [42] ensure: [nil]\n";
        let code = codegen(src);
        assert!(
            code.contains("try"),
            "class method ensure: should generate try. Got:\n{code}"
        );
        // Must NOT reference State (class methods have no actor state)
        assert!(
            !code.contains("let StateAcc = State"),
            "class method ensure: must not reference actor State. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_in_class_method_with_captured_local_mutation() {
        // ensure: in a class method where locals declared outside
        // the block are reassigned inside — must use closure path, not mutation threading
        let src = "\
Actor subclass: Foo
  state: x = 0

  class build: block =>
    routeList := nil
    nfHandler := nil
    [
      routeList := 42
      nfHandler := 99
    ] ensure: [nil]
    routeList
";
        let code = codegen(src);
        assert!(
            code.contains("try"),
            "class method ensure: with captured mutation should generate try. Got:\n{code}"
        );
        // Must NOT reference State (class methods have no actor state)
        assert!(
            !code.contains("let StateAcc = State"),
            "class method ensure: must not reference actor State. Got:\n{code}"
        );
        // Should use closure-based approach (BlockFun/CleanupFun), not mutation threading
        assert!(
            code.contains("apply") && code.contains("do apply"),
            "class method ensure: should use closure-based try/catch. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_generates_try_of_catch_with_cleanup() {
        // ensure: generates a try/of/catch (not Core Erlang try/after) with cleanup
        // applied in both success and error paths
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run =>\n    [42] ensure: [0]\n";
        let code = codegen(src);
        // ensure:-specific pattern: cleanup is applied via `do apply` in the catch clause
        assert!(
            code.contains("do apply"),
            "ensure: catch should run cleanup via 'do apply'. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "ensure: catch should re-raise after cleanup. Got:\n{code}"
        );
    }

    #[test]
    fn test_on_do_zero_arg_handler_no_exception_binding() {
        // Handler block takes no argument: [0] instead of [:e | 0].
        // make_handler_apply is called with takes_arg=false, generating
        // `apply HandlerFun ()` rather than `apply HandlerFun (ExObj)`.
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run =>\n    [42] on: Error do: [0]\n";
        let code = codegen(src);
        assert!(
            code.split("apply _HandlerFun").skip(1).any(|suffix| {
                suffix
                    .trim_start_matches(|ch: char| ch.is_ascii_digit())
                    .starts_with(" ()")
            }),
            "on:do: with 0-arg handler should apply handler with empty args. Got:\n{code}"
        );
        assert!(
            !code.contains(" (_ExObj"),
            "on:do: with 0-arg handler must not pass exception object to handler apply. Got:\n{code}"
        );
        assert!(
            code.contains("try apply"),
            "on:do: with 0-arg handler should generate try/catch via apply. Got:\n{code}"
        );
        assert!(
            code.contains("'$bt_nlr'"),
            "on:do: with 0-arg handler should detect NLR throws. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "on:do: with 0-arg handler should re-raise non-matching exceptions. Got:\n{code}"
        );
    }

    #[test]
    fn test_on_do_with_state_mutation_in_handler_uses_threading() {
        // Handler block mutates actor field — triggers generate_on_do_with_mutations,
        // which inlines block bodies with StateAcc threading instead of wrapping as
        // closures. Also exercises on_do_catch_preamble and
        // generate_exception_body_with_threading.
        let src = "\
Actor subclass: Srv
  state: count = 0

  run =>
    [42] on: Error do: [:e | self.count := self.count + 1]
";
        let code = codegen(src);
        assert!(
            code.contains("StateAcc"),
            "on:do: with handler state mutation must use StateAcc threading. Got:\n{code}"
        );
        assert!(
            code.contains("'beamtalk_exception_handler':'ensure_wrapped'"),
            "on:do: with mutation must still wrap exceptions. Got:\n{code}"
        );
        assert!(
            code.contains("'$bt_nlr'"),
            "on:do: with mutation must still detect NLR throws. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "on:do: with mutation must re-raise non-matching exceptions. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_with_state_mutation_in_cleanup_uses_threading() {
        // Cleanup block mutates actor field — triggers generate_ensure_with_mutations,
        // which inlines both try body and cleanup with StateAcc threading.
        let src = "\
Actor subclass: Srv
  state: count = 0

  run =>
    [42] ensure: [self.count := self.count + 1]
";
        let code = codegen(src);
        assert!(
            code.contains("StateAcc"),
            "ensure: with cleanup state mutation must use StateAcc threading. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "ensure: with mutation must re-raise after cleanup on error. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_with_state_mutation_in_try_body_uses_threading() {
        // Try body (receiver block) mutates actor field — also triggers
        // generate_ensure_with_mutations. Exercises the try-body threading branch
        // (generate_exception_body_with_threading called for receiver_block).
        let src = "\
Actor subclass: Srv
  state: count = 0

  run =>
    [self.count := self.count + 1] ensure: [nil]
";
        let code = codegen(src);
        assert!(
            code.contains("StateAcc"),
            "ensure: with try-body state mutation must use StateAcc threading. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "ensure: with mutation must re-raise after cleanup on error. Got:\n{code}"
        );
    }

    // ── ADR 0111 Addendum 5: NonLinearVersion is now a LIVE check
    // for `on:do:`/`ensure:` arms (previously scaffolding-only —
    // `check_branch_frame_linearity`'s scalar synthesis always allocated a
    // fresh, distinct FrameId per arm by construction, so two arms could
    // never collide, by construction, regardless of what the generator
    // actually produced). Mirrors `conditionals.rs`'s
    // `test_bt3146_nonlinear_version_detected_via_production_lowering_types`.

    #[test]
    fn test_bt3165_sibling_try_and_handler_arms_reaching_same_version_do_not_trip_nonlinear_version()
     {
        // THE acceptance-criteria case `check_branch_frame_linearity` used
        // to guarantee only by never exercising real IR: on:do:'s try body
        // and handler body each perform exactly one field mutation, so BOTH
        // sibling with_branch_context arms independently produce
        // "StateAcc1" — in disjoint frames minted by `current_branch_frame`.
        // `generate_exception_body_with_threading_inner` now `verify()`s
        // each arm's REAL IR (via `verify_and_render_branch_arm`); a
        // regression that collapsed frame identity across arms (e.g. reused
        // the same FrameId, or dropped the per-with_branch_context mint)
        // would trip `VerifyError::NonLinearVersion`, hard-failing via
        // `report_threaded_ir_verify_errors`'s `debug_assert!` and panicking
        // this test in this debug build.
        let src = "\
Actor subclass: Srv
  state: count = 0

  run =>
    [self.count := self.count + 1] on: Error do: [:e | self.count := self.count + 2]
";
        let code = codegen(src);
        assert!(
            code.contains("StateAcc1"),
            "both sibling arms should independently reach StateAcc1. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt3165_sibling_ensure_arms_reaching_same_version_do_not_trip_nonlinear_version() {
        // `ensure:`'s three-sibling-arm shape: try body, success-path
        // cleanup, error-path cleanup (`cleanup_block` compiled twice) —
        // each performing exactly one field mutation, so all three
        // independently reach "StateAcc1" in three disjoint frames.
        let src = "\
Actor subclass: Srv
  state: count = 0

  run =>
    [self.count := self.count + 1] ensure: [self.count := self.count + 2]
";
        let code = codegen(src);
        assert!(
            code.contains("StateAcc1"),
            "all three sibling arms should independently reach StateAcc1. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt3165_nonlinear_version_detected_via_production_lowering_types() {
        // Built through the exact production types/constructors
        // `generate_exception_body_with_threading_inner`'s lowering uses — a
        // real `CoreErlangGenerator::with_branch_context` call for frame
        // allocation (`current_branch_frame`), real `VersionedVar`/
        // `ThreadedStmt::Bind`/`BindOp::Put` construction — not an isolated
        // hand-fixture disconnected from any real construction path. Mirrors
        // `conditionals.rs`'s
        // `test_bt3146_nonlinear_version_detected_via_production_lowering_types`.
        let mut generator = CoreErlangGenerator::new("bt3165_regression_nonlinear");
        let errors = generator.with_branch_context(|this| {
            let frame = this.current_branch_frame();
            // Two field-mutation Binds that BOTH (incorrectly) target
            // State(1)@frame from State(0)@frame — the exact shape a broken
            // lowering (e.g. forgetting to call `next_state_var()` between
            // two field assignments in the same exception-body arm) would
            // produce. Mirrors E1's real `BindOp::Put` shape exactly.
            let source = VersionedVar::new(VersionPrefix::State, 0, frame);
            let target = VersionedVar::new(VersionPrefix::State, 1, frame);
            let make_put = |field: &str, val: &str, target: VersionedVar, source: VersionedVar| {
                ThreadedStmt::Bind {
                    target,
                    source,
                    op: BindOp::Put {
                        field: field.to_string(),
                        value: ValueRef::Var(val.to_string()),
                        class_tag: ValueRef::Literal("'nil'"),
                    },
                    shadow_write: false,
                    span: Span::default(),
                }
            };
            let wrapper = vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                shadow_write_eligible: true, // State-prefix fixture, not class-var — inert
                body: vec![
                    make_put("count", "_Val1", target.clone(), source.clone()),
                    make_put("count", "_Val2", target.clone(), source),
                ],
                produces: vec![target],
                span: Span::default(),
            }];
            threaded_ir::verify(&wrapper)
        });

        assert!(
            errors.iter().any(|e| matches!(
                e,
                threaded_ir::VerifyError::NonLinearVersion { producers: 2, .. }
            )),
            "expected NonLinearVersion(producers: 2) for the duplicate State(1) producer, \
             got: {errors:?}"
        );
    }

    #[test]
    fn test_bt3165_unbound_version_detected_via_production_lowering_types() {
        // Mirrors `conditionals.rs`'s
        // `test_bt3146_unbound_version_detected_via_production_lowering_types`:
        // a Bind whose source references a version this frame never
        // produced — the exact shape a broken lowering (e.g. reading a
        // stale `state_version()` snapshot from before an earlier E1/E3
        // mutation actually landed) would produce.
        let mut generator = CoreErlangGenerator::new("bt3165_regression_unbound");
        let errors = generator.with_branch_context(|this| {
            let frame = this.current_branch_frame();
            let phantom_source = VersionedVar::new(VersionPrefix::State, 5, frame);
            let target = VersionedVar::new(VersionPrefix::State, 6, frame);
            let bind = ThreadedStmt::Bind {
                target: target.clone(),
                source: phantom_source,
                op: BindOp::Put {
                    field: "count".to_string(),
                    value: ValueRef::Var("_Val1".to_string()),
                    class_tag: ValueRef::Literal("'nil'"),
                },
                shadow_write: false,
                span: Span::default(),
            };
            let wrapper = vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                shadow_write_eligible: true, // State-prefix fixture, not class-var — inert
                body: vec![bind],
                produces: vec![target],
                span: Span::default(),
            }];
            threaded_ir::verify(&wrapper)
        });

        assert!(
            errors
                .iter()
                .any(|e| matches!(e, threaded_ir::VerifyError::UnboundVersion { .. })),
            "expected UnboundVersion for the phantom State(5) source, got: {errors:?}"
        );
    }
}
