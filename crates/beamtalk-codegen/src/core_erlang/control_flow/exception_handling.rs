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

use super::super::intrinsics::{validate_block_arity_exact, validate_on_do_handler};
use super::super::synced_call::ActorStateSource;
use super::super::threaded_ir::{
    self, CatchClause, CatchStep, FrameId, NlrThrowShape, OnDoCatchVars, RenderCtx, StateReadSite,
    ThreadedStmt, TryRegion, VersionPrefix, VersionedVar,
};
use super::super::{CodeGenError, CoreErlangGenerator, Result, block_analysis};
use super::analysis::ThreadedFamilies;
use super::conditionals::ArmFrame;
use super::family_slots;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression, MessageSelector};
use beamtalk_core::semantic_analysis::block_facts::LocalThreadingFamily;

/// ADR 0122 / BT-3506: an arm's (or the construct's own) contribution to the
/// trailing family slot — the family this construct threads (`SelfVt`)
/// paired with the version to render there, or `None` when the construct
/// threads none. A named alias (not a bare tuple type) purely
/// so the several functions passing this shape around don't each spell out
/// `Option<(VersionPrefix, usize)>` (clippy's `type_complexity`).
pub(super) type FamilySlot = Option<(VersionPrefix, usize)>;

/// BT-3486/BT-3506 (ADR 0122): one inlined `on:do:`/`ensure:` arm (a try
/// body, an `on:do:` handler, or one of `ensure:`'s two cleanup runs), as
/// produced by [`CoreErlangGenerator::generate_exception_body_with_threading`].
///
/// `family_mutated_version` is the arm's own final `(prefix, version)` for
/// whichever EXTRA storage family (`SelfVt`) the construct threads through
/// its trailing tuple slot, when the arm actually advanced that family past
/// what it inherited on entry — `None` when it did not. Read the same way as
/// `value_type_codegen.rs`'s `VtBranchPieces::self_mutated_version` for a
/// conditional's branch arms (BT-3159/BT-3484): an arm that did not itself
/// mutate the family carries the construct's pre-`try` baseline in the slot
/// instead, so both arms agree on the tuple shape.
pub(super) struct ExceptionArm {
    pub(super) doc: Document<'static>,
    pub(super) result_var: String,
    pub(super) state_version: usize,
    pub(super) family_mutated_version: FamilySlot,
}

/// ADR 0131 §2 (Phase 3b): how an arm that does not inherit its locals
/// lexically is entered. `ensure:`'s success-path cleanup runs after the try
/// body, whose writes were made inside the Core Erlang `try` and are not in
/// scope in its `of` clause: the arm reads the locals the construct threads back
/// from the `StateAcc` the try body returned, through `LocalRebind`s of a
/// construct tuple (the try body's result, `tuple_var`).
pub(super) struct ArmEntry {
    /// The variable bound to the try body's `{Result, StateAcc[, Family]}`
    /// tuple.
    pub(super) tuple_var: String,
    /// The locals the construct threads ([`CoreErlangGenerator::branch_threaded_locals`]
    /// over the try and cleanup blocks).
    pub(super) locals: Vec<String>,
}

/// The state an `on:do:` / `ensure:` / `tryDo:` scratch map starts from
/// ([`CoreErlangGenerator::exception_body_outer_state`]).
pub(super) struct ExceptionOuterState {
    /// Bound ahead of the construct's seed (the cell `take` of a bracketed
    /// region), else [`Document::Nil`].
    pub(super) prefix: Document<'static>,
    /// The variable the scratch map starts from, or the empty map literal
    /// when it is private.
    pub(super) state: String,
    /// The scratch map is private (`~{}~`): the construct sits in a frame
    /// with no `State` chain (a class or value-type method, a direct-params
    /// loop body), so its writes to it die with the construct.
    pub(super) private_scratch: bool,
}

impl ExceptionOuterState {
    /// A scratch map that continues the enclosing `state`.
    fn continues(prefix: Document<'static>, state: String) -> Self {
        Self {
            prefix,
            state,
            private_scratch: false,
        }
    }
}

impl CoreErlangGenerator {
    fn state_acc_var_doc(state_version: usize) -> Document<'static> {
        match state_version {
            0 => docvec!["StateAcc"],
            n => leaf::var(super::super::util::versioned_var("StateAcc", n)),
        }
    }

    /// ADR 0122 Decision 4, applied to the EXTRA storage family on:do:/ensure:
    /// can carry through its trailing tuple slot: reads `prefix`'s live
    /// version off the generator. `SelfVt` only — every other prefix is
    /// unreachable here, since [`Self::exception_construct_families`] only
    /// ever answers with it (State is filtered out; see that function's doc
    /// comment).
    pub(super) fn family_version(&self, prefix: &VersionPrefix) -> usize {
        match prefix {
            VersionPrefix::SelfVt => self.self_version(),
            other => unreachable!(
                "on:do:/ensure: only ever threads SelfVt as an extra \
                 storage family, got {other:?}"
            ),
        }
    }

    /// The write half of [`Self::family_version`].
    pub(super) fn set_family_version(&mut self, prefix: &VersionPrefix, version: usize) {
        match prefix {
            VersionPrefix::SelfVt => self.set_self_version(version),
            other => unreachable!(
                "on:do:/ensure: only ever threads SelfVt as an extra \
                 storage family, got {other:?}"
            ),
        }
    }

    /// The bare (version-0) Core Erlang variable name for `prefix` — `"Self"`,
    /// the same spelling [`VersionedVar::render_name`]
    /// gives version 0 of the prefix, kept as a literal here (rather than
    /// rendering a version-0 `VersionedVar`) only because
    /// [`Self::seed_exception_arm_family`] needs the BARE name as the shadow
    /// binding's own target identifier, not a value to render.
    pub(super) fn family_bare_var_name(prefix: &VersionPrefix) -> &'static str {
        match prefix {
            VersionPrefix::SelfVt => "Self",
            other => unreachable!(
                "on:do:/ensure: only ever threads SelfVt as an extra \
                 storage family, got {other:?}"
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
    pub(super) fn exception_family_slot(
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
    pub(super) fn push_exception_arm(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        body: &Block,
        families: &ThreadedFamilies,
        outer_version: usize,
        frame: Option<&ArmFrame>,
    ) -> Result<(String, usize, FamilySlot)> {
        let seed = self.seed_exception_arm_family(families, outer_version);
        let arm = self.generate_exception_body_with_threading(body, families, frame, None)?;
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
    pub(super) fn close_exception_result_tuple(
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
    ///
    /// This is the exact family-generic counterpart of the
    /// `let StateAcc = <outer> in ` rebind every one of these constructs
    /// already performs for the state accumulator: same name shadowing, same
    /// version-0 restart, same reason.
    ///
    /// [`Document::Nil`] (and no reset) when `families` is empty, or when the
    /// live version is already 0 — the common case, where the bare name is
    /// the method's own fun parameter and there is nothing to shadow. `live > 0` happens when an earlier
    /// statement in the same method already mutated the family, including a
    /// preceding `on:do:`/`ensure:` whose own mutation this issue threads
    /// out.
    pub(super) fn seed_exception_arm_family(
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
        docvec![form::let_prefix(
            leaf::var(bare.to_string()),
            leaf::var(super::super::util::versioned_var(bare, live))
        )]
    }

    /// ADR 0122 / BT-3506: the storage families (`SelfVt`) this
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
    /// answer for. Class methods thread no family (ADR 0130 §3), so the
    /// result carries at most one entry (`SelfVt`) for this construct.
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
    ///   [`Self::reject_unthreadable_value_self_field_write`] in
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
        beamtalk_cerl_doc::form::primop(
            "raw_raise",
            [
                leaf::var(type_var),
                leaf::var(error_var),
                leaf::var(stack_var),
            ],
        )
    }

    /// Mints the Core Erlang variables of one compiled `on:do:`'s catch clause.
    /// `ex_class_term` is the variable holding the exception class.
    pub(super) fn fresh_on_do_catch_vars(&mut self, ex_class_term: String) -> OnDoCatchVars {
        OnDoCatchVars {
            type_var: self.fresh_temp_var("Type"),
            error_var: self.fresh_temp_var("Error"),
            stack_var: self.fresh_temp_var("Stack"),
            built_stack_var: self.fresh_temp_var("BuiltStack"),
            ex_obj_var: self.fresh_temp_var("ExObj"),
            match_var: self.fresh_temp_var("Match"),
            // Unique names for the NLR pattern variables (no anonymous `_` in
            // Core Erlang). Actor NLR throws include state as a 4th element.
            nlr_tok_var: self.fresh_temp_var("NlrCheckTok"),
            nlr_val_var: self.fresh_temp_var("NlrCheckVal"),
            nlr_state_var: self.fresh_temp_var("NlrCheckState"),
            nlr_tok_var2: self.fresh_temp_var("NlrCheckTok"),
            nlr_val_var2: self.fresh_temp_var("NlrCheckVal"),
            // Fallback pattern: ONE variable binding the whole 2-tuple (not
            // two separate elements).
            other_pair_var: self.fresh_temp_var("OtherPair"),
            // Derived from the (already unique) exception-class variable, not
            // minted from the module counter, so it renumbers none of the
            // module's other temporaries.
            filter_fallback_var: [ex_class_term.as_str(), "NoMatch"].concat(),
            ex_class_term,
        }
    }

    /// Generates the `try`/`catch` shared by every compiled `on:do:`
    /// (`generate_on_do`, `generate_on_do_with_mutations`,
    /// `generate_on_do_tier1_try`): the single owner of the catch boundary,
    /// built as a [`ThreadedStmt::OnDoCatch`] node, [`threaded_ir::verify`]d and
    /// rendered.
    ///
    /// Produces `try <body> of R -> R catch ...` as one unit. `try_region` is
    /// the protected expression and its `of` variable, and `handler` the body
    /// of the class filter's `'true'` arm (the handler application). The node
    /// owns the filter `case` end to end, so its `'false'` re-raise arm and the
    /// exhaustive wildcard clause that keeps `erlc` from rejecting an `on:do:`
    /// nested in another protected region (`ambiguous_catch_try_state`) are
    /// present by construction, and `verify()` rejects a node without them
    /// (`VerifyError::CatchFilterNotClosed`). The node orders the clauses: NLR
    /// throws (`{'$bt_nlr', ...}`, both the actor-shaped 4-tuple and the
    /// 3-tuple) bypass the catch untouched so the enclosing method's NLR
    /// handler can intercept them (`VerifyError::CatchNlrNotPassedThrough`);
    /// every other exception restores the actor-state cell where there is one
    /// (ADR 0137 §5) and only then wraps the exception and runs the class
    /// filter.
    pub(super) fn on_do_catch_boundary(
        &mut self,
        vars: OnDoCatchVars,
        pre_region_state: &str,
        try_region: TryRegion,
        span: beamtalk_core::source_analysis::Span,
        handler: Document<'static>,
    ) -> Document<'static> {
        let cell = self.on_do_cell_boundary(&vars.ex_class_term, pre_region_state, span);
        // ADR 0137 §5: the cell's half, restored first, before the exception
        // is wrapped.
        let mut steps: Vec<CatchStep> = cell
            .clone()
            .map(CatchStep::CellRestore)
            .into_iter()
            .collect();
        steps.extend([
            CatchStep::WrapException,
            CatchStep::ClassFilter,
            CatchStep::FilterHandler(handler),
            CatchStep::FilterMiss,
        ]);
        let node = ThreadedStmt::OnDoCatch {
            vars: Box::new(vars),
            cell,
            try_region,
            clauses: vec![
                CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
                CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
                CatchClause::NonNlr { steps },
            ],
            span,
        };
        let ir = [node];
        let errors = self.verify_threaded_ir(&ir);
        self.report_threaded_ir_verify_errors(&errors, "on:do: catch boundary", span);
        threaded_ir::render(&ir, &mut RenderCtx::new(self))
    }

    /// Builds the `apply HandlerFun (ExObj)` or `apply HandlerFun ()` fragment
    /// for `on:do:` exception handlers.
    ///
    /// BT-4366: the apply sits inside the catch clause's `case`, so it carries
    /// the `on:do:` send's line itself.
    fn make_handler_apply(
        &self,
        handler_var: String,
        ex_obj_var: String,
        takes_arg: bool,
    ) -> Document<'static> {
        self.annotate_current_send_call(if takes_arg {
            docvec![
                "apply ",
                leaf::var(handler_var),
                " (",
                leaf::var(ex_obj_var),
                ")",
            ]
        } else {
            docvec!["apply ", leaf::var(handler_var), " ()"]
        })
    }

    /// Whether `block` needs `on:do:`/`ensure:`'s inlined, state-threading
    /// compilation strategy rather than the plain closure-based one:
    /// [`Self::needs_mutation_threading`]'s own answer, widened for class
    /// methods to every block that writes an outer local, which is what the
    /// construct's result-unpacking side (`threaded_locals_of`)
    /// keys on. ADR 0130 §3: class variables are not threaded, so a class-
    /// variable write or a self-send never selects this strategy.
    pub(in crate::core_erlang) fn block_needs_exception_threading(&self, block: &Block) -> bool {
        let analysis = block_analysis::analyze_block(block);
        self.needs_mutation_threading(&analysis)
            // ADR 0130 §3: a class method threads only its outer locals (class
            // variables are written in place). The extraction side
            // (`threaded_locals_of`) unpacks the construct's
            // `{Result, StateAcc}` tuple exactly when the blocks write an outer
            // local, so the construct must produce that tuple in the same case,
            // including for a write-only local that `needs_mutation_threading`
            // does not count.
            || (self.threads_no_actor_state() && !self.branch_threaded_locals(&[block]).is_empty())
            // BT-3823: in the REPL, a block that writes a binding anywhere,
            // a nested loop or fold included (`analyze_block` does not see
            // through one), is inlined over the bindings map: a REPL arm whose
            // producers adopt it.
            || (self.is_repl_mode() && self.repl_blocks_write_bindings(&[block]))
    }

    /// Whether an `on:do:`/`ensure:` with operands `receiver` and `other` is
    /// lowered in place with its state threaded, rather than as closures: a
    /// block literal operand needs exception threading and both operands are
    /// block literals (BT-4430's `inlined_exception_blocks`, the one inlining
    /// rule the analyser's Phase 0 check reads). The one decision
    /// [`Self::generate_on_do`], [`Self::generate_ensure`] and the bracket
    /// classifier (`classify_inlined`, BT-4524) read.
    pub(in crate::core_erlang) fn exception_construct_inlines(
        &self,
        receiver: &Expression,
        other: &Expression,
    ) -> bool {
        let needs = |e: &Expression| match e {
            Expression::Block(b) => self.block_needs_exception_threading(b),
            _ => false,
        };
        (needs(receiver) || needs(other))
            && beamtalk_core::semantic_analysis::block_facts::inlined_exception_blocks(
                receiver, other,
            )
            .is_some()
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
        let handler_takes_arg = Self::validate_on_do_blocks(receiver, handler)?;

        if self.exception_construct_inlines(receiver, handler)
            && let Some((recv_block, handler_block)) =
                beamtalk_core::semantic_analysis::block_facts::inlined_exception_blocks(
                    receiver, handler,
                )
        {
            return self.generate_on_do_with_mutations(recv_block, ex_class, handler_block, false);
        }

        // Simple case: no mutations, use closure-based approach
        let block_var = self.fresh_temp_var("BlockFun");
        let ex_class_term = self.fresh_temp_var("ExClass");
        let handler_var = self.fresh_temp_var("HandlerFun");
        let result_var = self.fresh_temp_var("Result");
        let catch_vars = self.fresh_on_do_catch_vars(ex_class_term.clone());
        let ex_obj_var = catch_vars.ex_obj_var.clone();

        // Capture expression outputs (ADR 0018 bridge pattern). BT-4524: the
        // catch boundary stores the pre-region `State` on entry
        // (`CellBoundary::PreState`) wherever the cell protocol applies, so a
        // `^` in either closure carries the method's state: covered. BT-4532:
        // only the operand that is itself a block literal; a closure nested in
        // a non-block operand runs as that operand is evaluated, before the
        // boundary's store, and its own call has to store.
        let covered = |this: &Self, operand: &Expression| match operand {
            Expression::Block(block) if this.synced_calls_active() => vec![block.span],
            _ => Vec::new(),
        };
        let receiver_cover = covered(self, receiver);
        let receiver_code =
            self.with_nlr_closure_cover(&receiver_cover, |this| this.expression_doc(receiver))?;
        let ex_class_code = self.on_do_filter_doc(ex_class)?;
        let handler_cover = covered(self, handler);
        let handler_code =
            self.with_nlr_closure_cover(&handler_cover, |this| this.expression_doc(handler))?;

        let handler_apply =
            self.make_handler_apply(handler_var.clone(), ex_obj_var, handler_takes_arg);
        let pre_region_state = self.state_var_to_store();
        let catch_boundary = self.on_do_catch_boundary(
            catch_vars,
            &pre_region_state,
            TryRegion {
                // BT-4366: the protected block's apply is nested in the `try`.
                body: docvec![
                    self.annotate_current_send_call(docvec![
                        "apply ",
                        leaf::var(block_var.clone()),
                        " ()"
                    ]),
                    " "
                ],
                result_var,
            },
            receiver.span(),
            handler_apply,
        );

        Ok(docvec![
            form::let_prefix(leaf::var(block_var.clone()), receiver_code),
            form::let_prefix(leaf::var(ex_class_term.clone()), ex_class_code),
            form::let_prefix(leaf::var(handler_var), handler_code),
            catch_boundary
        ])
    }

    /// The `on:do:` class filter. A literal class is passed by name
    /// (BT-4295, [`Self::class_name_only_reference`]): `matches_class/2`
    /// reads only the name, so building the class object would cost a
    /// `module_name_safe/1` lookup and a tuple for nothing. Any other filter
    /// (a variable, a REPL top-level class literal) is evaluated as an
    /// ordinary expression. A package-qualified literal (`pkg@Error`) is
    /// passed by its bare name like any other (BT-4074).
    fn on_do_filter_doc(&mut self, ex_class: &Expression) -> Result<Document<'static>> {
        match self.class_name_only_reference(ex_class) {
            Some(doc) => Ok(doc),
            None => self.expression_doc(ex_class),
        }
    }

    /// Validates the arities of an `on:do:`'s protected block (0) and handler (0
    /// or 1), returning whether the handler takes the exception. The one
    /// validation shared by [`Self::generate_on_do`] and the producer
    /// ([`Self::exception_producer_tuple_doc`]).
    fn validate_on_do_blocks(receiver: &Expression, handler: &Expression) -> Result<bool> {
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
        validate_on_do_handler(handler, "on:do:")
    }

    /// Validates the arity of an `ensure:`'s cleanup block (0). Shared by
    /// [`Self::generate_ensure`] and the producer.
    pub(super) fn validate_ensure_cleanup(cleanup: &Expression) -> Result<()> {
        // Validate cleanup block arity (must be 0-arg)
        validate_block_arity_exact(
            cleanup,
            0,
            "ensure:",
            "Fix: The cleanup block must take no arguments:\n\
             \x20 [operation] ensure: [resource close]",
        )
    }

    /// ADR 0131 Phase 3b (BT-3817): the construct tuple `{Result, StateAcc
    /// [, Self]}` of an `on:do:`/`ensure:` that is a local-threading producer
    /// ([`Self::exception_producer_locals`]), whose protected body, handler
    /// and cleanup are `BranchArm` frames. Unlike [`Self::generate_on_do`] it
    /// always takes the mutation-threading form: the producer's set, not the
    /// statement-position gate, decides that the construct threads (a
    /// write-only arm threads `t` all the same).
    pub(in crate::core_erlang) fn exception_producer_tuple_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr.unwrap_parens()
        else {
            return Err(CodeGenError::Internal(
                "exception producer reached a non-keyword send".to_string(),
            ));
        };
        let selector: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        match (selector.as_str(), receiver.as_ref(), arguments.as_slice()) {
            // ADR 0131 §5 (Phase 4, BT-3750): `Result tryDo: [...]` whose block
            // writes an outer local.
            ("tryDo:", recv @ Expression::ClassReference { .. }, [Expression::Block(body)])
                if beamtalk_core::semantic_analysis::block_facts::is_result_try_do(
                    recv, "tryDo:",
                ) =>
            {
                self.try_do_tier2_tuple_doc(body)
            }
            (
                "on:do:",
                Expression::Block(protected),
                [ex_class, handler_expr @ Expression::Block(handler)],
            ) => {
                Self::validate_on_do_blocks(receiver, handler_expr)?;
                self.generate_on_do_with_mutations(protected, ex_class, handler, true)
            }
            (
                "ensure:",
                Expression::Block(protected),
                [cleanup_expr @ Expression::Block(cleanup)],
            ) => {
                Self::validate_ensure_cleanup(cleanup_expr)?;
                self.generate_ensure_with_mutations(protected, cleanup, true)
            }
            // The producer recognizer classified this send as a `tryDo:`
            // producer but `is_result_try_do` rejects its receiver (or its
            // argument is not a block literal): the two disagree.
            ("tryDo:", _, _) => Err(CodeGenError::Internal(
                "exception producer reached `tryDo:` whose receiver is not `Result` \
                 or whose argument is not a block literal (the producer recognizer \
                 and `is_result_try_do` disagree)"
                    .to_string(),
            )),
            _ => Err(CodeGenError::Internal(format!(
                "exception producer reached `{selector}` with non-literal blocks"
            ))),
        }
    }

    /// ADR 0131 §5 (Phase 4, BT-3750): the construct tuple `{Result, StateAcc}`
    /// of `Result tryDo: [...]` whose block writes an outer local. The catch
    /// boundary stays in the runtime; codegen only chooses the arity:
    ///
    /// ```erlang
    /// let SeededState = <scratch map with the block's locals> in
    /// call 'beamtalk_result':'tryDo'(fun (StateAcc) -> {Value, StateAcc1}, SeededState)
    /// ```
    ///
    /// The scratch map is seeded like an `on:do:`'s ([`Self::exception_body_outer_state`],
    /// `seed_conditional_locals`): the actor `State` (or the REPL bindings map)
    /// continued, or a private map in a class or value-type method. A raise
    /// answers the entry map, so the block's local writes are discarded.
    fn try_do_tier2_tuple_doc(&mut self, body: &Block) -> Result<Document<'static>> {
        if self.is_repl_mode() {
            return self.repl_try_do_tier2_tuple_doc(body);
        }
        // The block is a `BranchArm` frame over the `StateAcc` parameter, like
        // the protected body of an `on:do:` producer: a loop, fold, conditional
        // or exception in it rebinds its locals into that frame.
        let arm = self.exception_arm_frame(&[body], true);
        let ExceptionOuterState {
            prefix: outer_prefix,
            state: current_state,
            ..
        } = self.exception_body_outer_state(body.span);
        let (seed_doc, base_state) = self.seed_conditional_locals(&[body], &current_state);
        let seed_doc = docvec![outer_prefix, seed_doc];
        let families = ThreadedFamilies::default();
        let outer_self = self.self_version();
        let mut fun_docs: Vec<Document<'static>> = Vec::new();
        let (result_var, final_version, slot) =
            self.push_exception_arm(&mut fun_docs, body, &families, 0, arm.as_ref())?;
        fun_docs.push(self.close_exception_result_tuple(
            result_var,
            final_version,
            &families,
            &slot,
        ));
        self.set_self_version(outer_self);
        Ok(docvec![
            seed_doc,
            Self::try_do_tier2_call(Document::Vec(fun_docs), base_state)
        ])
    }

    /// `call 'beamtalk_result':'tryDo'(fun (StateAcc) -> <body>, <entry>)`:
    /// the one spelling of the Tier 2 call, shared by the method-body and REPL
    /// producers. `body` is the fun's `{Value, StateAcc1}` arm; `entry` the
    /// `StateAcc` variable it starts from.
    fn try_do_tier2_call(body: Document<'static>, entry: String) -> Document<'static> {
        beamtalk_cerl_doc::form::call(
            "beamtalk_result",
            "tryDo",
            [
                form::fun_of([leaf::var("StateAcc")], body, Layout::Flat),
                leaf::var(entry),
            ],
        )
    }

    /// [`Self::try_do_tier2_tuple_doc`] at the REPL root, whose locals live in
    /// the bindings map under their plain names (the REPL's inlined arm
    /// statements read and write plain keys, as an `on:do:` arm does). The
    /// block body is the inlined arm over a `StateAcc` that is the bindings
    /// map itself, so `tryDo/2` answers the `{Value, Bindings}` tuple the REPL
    /// assembler unpacks, with the entry bindings on a raise:
    ///
    /// ```erlang
    /// call 'beamtalk_result':'tryDo'(fun (StateAcc) -> {Value, StateAcc1}, Bindings)
    /// ```
    ///
    /// Sets `repl_loop_mutated`.
    fn repl_try_do_tier2_tuple_doc(&mut self, body: &Block) -> Result<Document<'static>> {
        self.set_repl_loop_mutated(true);
        let bindings = self.current_state_var();
        let (arm_doc, _final_version) =
            self.with_branch_context(|this| this.generate_repl_arm_inline(body))?;
        Ok(Self::try_do_tier2_call(arm_doc, bindings))
    }

    /// ADR 0131 §5 (Phase 4, BT-3750): the REPL's hook for
    /// `Result tryDo: [...]` whose block writes a binding: its
    /// [`Self::repl_try_do_tier2_tuple_doc`] construct tuple wherever the REPL
    /// compiles the send, or `None` for any other expression. The Phase 0 check
    /// admits it only where the REPL assembler or producer unpacks the tuple.
    pub(in crate::core_erlang) fn repl_try_do_tier2_send(
        &mut self,
        expr: &Expression,
    ) -> Result<Option<Document<'static>>> {
        // Only where the REPL unpacks the tuple (a statement, an assignment's
        // value, a REPL producer's construct, the root's or a REPL arm site's,
        // BT-3823): never in a closure or a plain argument, whatever the Phase 0
        // check accepts.
        if !self.is_repl_mode() || !self.is_repl_unpack_site(expr.span()) {
            return Ok(None);
        }
        let is_try_do =
            beamtalk_core::semantic_analysis::block_facts::local_threading_construct(expr)
                .is_some_and(|c| c.family == LocalThreadingFamily::TryDo);
        if !is_try_do
            || beamtalk_core::semantic_analysis::block_facts::exception_arm_blocks(expr).is_none()
            || self.threaded_locals_of(expr).is_none()
        {
            return Ok(None);
        }
        self.exception_producer_tuple_doc(expr).map(Some)
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
    /// (`VersionPrefix::SelfVt`, BT-3486) instead ride the
    /// construct's own trailing tuple slot
    /// ([`Self::close_exception_result_tuple`]) — never this map — so an
    /// empty seed outside Actor context is correct, not just a stopgap.
    ///
    /// Before BT-3486's fix, both callers unconditionally called
    /// `current_state_var()`, which at version 0 renders as the bare
    /// identifier `"State"` regardless of context — valid only in Actor
    /// context, where a real `State` parameter exists; in class-method or
    /// value-type context this produced a reference to a nonexistent
    /// variable (`erlc: unbound variable 'State'`).
    ///
    /// BT-4180: answered with a prefix the caller splices ahead of its seed:
    /// inside a bracketed region (a loop-condition closure of a loop that
    /// brackets guarded operators) the plain closure's state is the cell,
    /// bound by the prefix (`let TakeOuter = take() in `), not the captured
    /// entry `State`. [`Document::Nil`] everywhere else.
    pub(super) fn exception_body_outer_state(
        &mut self,
        at: beamtalk_core::source_analysis::Span,
    ) -> ExceptionOuterState {
        // ADR 0131 §2 (`MethodBody (REPL)` row): the REPL root threads its
        // bindings map, so the construct continues it (as a REPL conditional or
        // loop does) and its element 2 is the next bindings map.
        //
        // In an actor instance method it continues `State`, unless the frame it
        // sits in has no `State` chain (a direct-params or tuple-acc loop body,
        // ADR 0131 §2): its body has no state effect, so the scratch map is
        // private there as in a class method.
        if self.closure_scratch_continues_state(self.current_frame()) {
            return match self.actor_state_source(StateReadSite::ExceptionBodyOuterState, at) {
                ActorStateSource::Var(var) => ExceptionOuterState::continues(Document::Nil, var),
                ActorStateSource::Cell => {
                    let take_var = self.fresh_temp_var("TakeOuter");
                    let prefix = docvec![form::let_prefix(
                        leaf::var(take_var.clone()),
                        threaded_ir::cell_take_call()
                    )];
                    ExceptionOuterState::continues(prefix, take_var)
                }
            };
        }
        let state_chain = self
            .rebind_frames
            .get(&self.current_frame())
            .is_none_or(|frame| frame.kind.extends_state_chain());
        if (self.in_actor_instance_context() && state_chain) || self.is_repl_mode() {
            ExceptionOuterState::continues(Document::Nil, self.current_state_var())
        } else {
            ExceptionOuterState {
                prefix: Document::Nil,
                state: "~{}~".to_string(),
                private_scratch: true,
            }
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
    /// ADR 0122 / BT-3506: when either block mutates the `SelfVt`
    /// family the current context makes eligible, both returned tuples grow
    /// a trailing slot carrying that arm's own version — see
    /// [`Self::exception_construct_families`].
    fn generate_on_do_with_mutations(
        &mut self,
        receiver_block: &Block,
        ex_class: &Expression,
        handler_block: &Block,
        producer: bool,
    ) -> Result<Document<'static>> {
        if self.is_repl_mode() {
            self.set_repl_loop_mutated(true);
        }
        // ADR 0131 §2 (Phase 3b): a producer's protected body and handler are
        // `BranchArm` frames whose threads are the seeded set below.
        let arm = self.exception_arm_frame(&[receiver_block, handler_block], producer);

        let ex_class_term = self.fresh_temp_var("ExClass");
        let state_after_try = self.fresh_temp_var("StateAfterTry");
        let catch_vars = self.fresh_on_do_catch_vars(ex_class_term.clone());
        let ex_obj_var = catch_vars.ex_obj_var.clone();

        // Bind exception class
        let ex_class_code = self.on_do_filter_doc(ex_class)?;
        // Rename current state to StateAcc for uniform threading
        let ExceptionOuterState {
            prefix: outer_prefix,
            state: current_state,
            private_scratch,
        } = self.exception_body_outer_state(receiver_block.span);
        // seed `__local__` keys for outer locals mutated by either the
        // try (receiver) block or the handler block — only one of the two ever
        // runs at a given call, so (mirroring `ifTrue:ifFalse:`'s two branches)
        // the key must be present in the base state even on the path that
        // didn't itself write it, or the method-body sequencer's extraction
        // `maps:get/2` would hit a missing key.
        let (seed_doc, base_state) =
            self.seed_conditional_locals(&[receiver_block, handler_block], &current_state);
        let seed_doc = docvec![outer_prefix, seed_doc];
        // ADR 0122 / BT-3506: decided from the AST BEFORE either arm is
        // generated, so both arms' return tuples agree on the shape even
        // though only one of them may actually contain the mutation — the
        // extraction the consumer emits after the construct is fixed at
        // compile time and runs whichever arm executed.
        let families = self.exception_construct_families(&[receiver_block, handler_block]);
        // The construct's own live baseline for whichever family `families`
        // names (0 when `families` is empty; never read in that case) —
        // restored between arms (they are siblings: only one ever runs) and
        // once more after the construct. `outer_self` is ALSO captured
        // unconditionally (regardless of whether a family is active) so the
        // counter can be restored between/after arms the same way the
        // original `SelfVt`-only code always restored `self_version` even in
        // contexts where it never moved.
        let outer_self = self.self_version();
        let outer_version = families
            .as_slice()
            .first()
            .map_or(0, |prefix| self.family_version(prefix));

        let mut docs: Vec<Document<'static>> = vec![docvec![
            form::let_prefix(leaf::var(ex_class_term.clone()), ex_class_code),
            seed_doc,
            form::let_prefix(leaf::var("StateAcc"), leaf::var(base_state)),
        ]];

        // Generate try body (receiver block) with state threading. The
        // snapshot `let`, the `try` and its `of` clause come from the catch
        // boundary node (`on_do_catch_boundary`), which owns both halves.
        let mut try_docs: Vec<Document<'static>> = Vec::new();
        let (try_result_var, try_final, try_slot) = self.push_exception_arm(
            &mut try_docs,
            receiver_block,
            &families,
            outer_version,
            arm.as_ref(),
        )?;
        // Return {Result, State[, Family]} from try body; on success the
        // `of` clause passes the tuple through.
        try_docs.push(self.close_exception_result_tuple(
            try_result_var,
            try_final,
            &families,
            &try_slot,
        ));
        // Bind handler parameter (e.g., [:e | ...] binds e to exception object)
        let mut handler_docs: Vec<Document<'static>> = Vec::new();
        self.push_scope();
        if let Some(param) = handler_block.parameters.first() {
            let param_var = Self::to_core_erlang_var(&param.name);
            self.bind_var(&param.name, &param_var);
            handler_docs.push(docvec![form::let_prefix(
                leaf::var(param_var),
                leaf::var(ex_obj_var)
            )]);
        }

        // Generate handler body with state threading (from original StateAcc)
        self.set_self_version(outer_self);
        let (handler_result_var, handler_final, handler_slot) = self.push_exception_arm(
            &mut handler_docs,
            handler_block,
            &families,
            outer_version,
            arm.as_ref(),
        )?;
        // Return {Result, State[, Family]} from handler
        handler_docs.push(self.close_exception_result_tuple(
            handler_result_var,
            handler_final,
            &families,
            &handler_slot,
        ));
        self.pop_scope();
        // The catch boundary is one node that owns the snapshot `let`, the
        // `try` and the class filter end to end: NLR pass-through, restore,
        // wrap, filter, this handler, the `'false'` re-raise and the
        // exhaustive fallback.
        // (A frame with no `State` chain holds a private scratch map as its outer
        // state; the cell's pre-region version is then the enclosing `State`.)
        let pre_region_state = if private_scratch {
            self.state_var_to_store()
        } else {
            current_state.clone()
        };
        docs.push(self.on_do_catch_boundary(
            catch_vars,
            &pre_region_state,
            TryRegion {
                body: Document::Vec(try_docs),
                result_var: state_after_try,
            },
            receiver_block.span,
            Document::Vec(handler_docs),
        ));

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

        // ADR 0122 / BT-3506: an arm's `let <bare> = … in ` shadow and its
        // own version chain live entirely inside that arm's Core Erlang
        // scope; the construct's single legitimate advance of the method's
        // live version is the consumer's post-construct rebind
        // (`generate_vt_exception_construct_open`), so leave both counters
        // as found.
        self.set_self_version(outer_self);

        Ok(Document::Vec(docs))
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::threaded_ir::{self, BindOp, ThreadingMode, ValueRef};
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

    /// BT-4295: a literal `on:do:` filter is passed by name — one
    /// compile-time-named `whereis` keeps `class_not_found`, and no
    /// `module_name_safe` lookup or class-object tuple is built.
    #[test]
    fn test_on_do_literal_filter_is_name_only() {
        for src in [
            // closure form
            "Object subclass: Srv\n  run =>\n    [42] on: Error do: [:e | 0]\n",
            // state-threading form (the handler writes an outer local)
            "Object subclass: Srv\n  run =>\n    x := 0.\n    [42] on: Error do: [:e | x := 1].\n    x\n",
        ] {
            let code = codegen(src);
            assert!(
                code.contains("case call 'erlang':'whereis'('beamtalk_class_Error') of"),
                "the filter must look the class up by its registered name. Got:\n{code}"
            );
            assert!(
                code.contains("'class_not_found', 'Error'"),
                "an undefined filter class must still raise class_not_found. Got:\n{code}"
            );
            assert!(
                code.contains("<_> when 'true' -> 'Error' end"),
                "the filter must be the class name atom. Got:\n{code}"
            );
            assert!(
                !code.contains("'Error class'")
                    && !code.contains("module_name_safe")
                    && !code.contains("whereis_class"),
                "a name-only filter must build no class object. Got:\n{code}"
            );
        }
    }

    /// A non-literal filter is still evaluated as an expression.
    #[test]
    fn test_on_do_variable_filter_is_evaluated() {
        let src = "Object subclass: Srv\n  run: cls =>\n    [42] on: cls do: [:e | 0]\n";
        let code = codegen(src);
        assert!(
            !code.contains("'erlang':'whereis'"),
            "a variable filter must not be looked up by name. Got:\n{code}"
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
        // ensure: in a class method where locals declared outside the block are
        // reassigned inside: the construct threads those locals through its own
        // `{Result, StateAcc}` tuple (the post-construct unpacking keys on the
        // same locals), never through an actor `State`.
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
        assert!(
            code.contains("'__local__routeList'") && code.contains("'__local__nfHandler'"),
            "the written locals ride the construct's StateAcc map. Got:\n{code}"
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
            code.split("apply _HandlerFun@").skip(1).any(|suffix| {
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
        // closures. Also exercises on_do_catch_boundary and
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
        // each arm's REAL IR (via `render_branch_arm`); a
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
                    },
                    span: Span::default(),
                }
            };
            let wrapper = vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                threads: Vec::new(),
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
                },
                span: Span::default(),
            };
            let wrapper = vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                threads: Vec::new(),
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
