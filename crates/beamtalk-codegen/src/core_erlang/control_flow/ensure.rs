// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ensure:` lowering: the plain, mutation-threaded and relay forms
//! (split from `exception_handling.rs`).

use super::super::actor_state_access::{actor_state_call, actor_state_helper};
use super::super::ensure_nlr_relay::EnsureCatchTriple;
use super::super::threaded_ir::{
    self, EnsureCleanup, EnsureForeignNlr, EnsureNlrRelay, ForeignCleanupBody, NlrCarry,
    RethrowSlot, VersionPrefix,
};
use super::super::{CoreErlangGenerator, Result};
use super::analysis::ThreadedFamilies;
use super::conditionals::ArmFrame;
use super::exception_handling::{ArmEntry, ExceptionArm, ExceptionOuterState};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

impl CoreErlangGenerator {
    /// Generates `ensure:` — wraps block in try, always runs cleanup block.
    ///
    /// Analyzes both receiver and cleanup blocks for state mutations and chooses
    /// the appropriate compilation strategy.
    pub(in crate::core_erlang) fn generate_ensure(
        &mut self,
        receiver: &Expression,
        cleanup: &Expression,
    ) -> Result<Document<'static>> {
        Self::validate_ensure_cleanup(cleanup)?;

        if self.exception_construct_inlines(receiver, cleanup)
            && let Some((recv_block, cleanup_block)) =
                beamtalk_core::semantic_analysis::block_facts::inlined_exception_blocks(
                    receiver, cleanup,
                )
        {
            return self.generate_ensure_with_mutations(recv_block, cleanup_block, false);
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

        // BT-4312: the cleanup closure threads no version; what it writes it
        // writes in place, through the actor-state cell. Where the state a
        // `^` of this scope carries is the cell, or an actor method's `State`
        // (which the `^` stored to the cell before it threw), the throw resumes
        // with the carried state under the cell as the cleanup left it
        // (`ensure_nlr_relay`). The REPL's bindings and a value type's `Self`
        // are not in the cell, and no closure can write them. A cleanup that
        // is a block literal touching neither the actor's state nor user code
        // writes nothing.
        let carry = self.nlr_slot().carry();
        let writes_cell = Self::extract_block_literal(cleanup)
            .is_none_or(|block| self.block_touches_actor_state(block));
        let relays = writes_cell
            && matches!(carry, Some(NlrCarry::Live | NlrCarry::ActorState))
            && self.scope_nlr_token().is_some();
        let relay = relays.then(|| {
            let cleanup_apply = self.annotate_current_send_call(docvec![
                "apply ",
                leaf::var(cleanup_var.clone()),
                " ()"
            ]);
            EnsureNlrRelay {
                seed: Document::Nil,
                cleanup: form::let_header(leaf::var("_"), cleanup_apply),
                cell_store: false,
                rethrow: RethrowSlot::CellOverCarried,
            }
        });
        let other = docvec![
            "do ",
            self.annotate_current_send_call(docvec![
                "apply ",
                leaf::var(cleanup_var.clone()),
                " ()"
            ]),
            " ",
            Self::emit_raw_raise(type_var.clone(), error_var.clone(), stack_var.clone()),
        ];
        let catch = self.ensure_catch(
            EnsureCatchTriple {
                type_var,
                error_var,
                stack_var,
                carried_var: None,
            },
            (carry, false, self.synced_calls_active()),
            EnsureCleanup::Closure { writes_cell },
            (relay, None),
            other,
            receiver.span(),
        );

        Ok(docvec![
            form::let_prefix(leaf::var(block_var.clone()), receiver_code),
            form::let_prefix(leaf::var(cleanup_var.clone()), cleanup_code),
            // Not `form::try_of_catch`: `of R` is the bare spelling (not
            // `of <R>`), and the `catch` clause arrives as a finished document.
            "try ",
            form::let_in(
                leaf::var(try_result_var.clone()),
                // BT-4366: each apply is nested in the `try`, so it carries the
                // `ensure:` send's line itself.
                self.annotate_current_send_call(docvec!["apply ", leaf::var(block_var), " ()"]),
                leaf::var(try_result_var),
                Layout::Flat,
            ),
            " of ",
            leaf::var(result_var.clone()),
            " -> ",
            form::let_discard(self.annotate_current_send_call(docvec![
                "apply ",
                leaf::var(cleanup_var.clone()),
                " ()"
            ])),
            leaf::var(result_var),
            " ",
            catch,
        ])
    }

    /// The success path of `ensure:`'s `try ... of`: the cleanup runs after the
    /// try body, starting from the `StateAcc` the body returned
    /// (`state_after_try`'s element 2), and answers the try body's result with the
    /// cleanup's final state (`{Result, StateAcc[, Family]}`). Pushes the `of`
    /// clause and the cleanup arm onto `docs`.
    ///
    /// A local the body wrote must be read back from the returned `StateAcc`: the
    /// cleanup's own reads of it are otherwise the pre-`try` binding, because a
    /// binding made inside the `try` is not in scope in its `of` clause
    /// (BT-3718). The cleanup arm reads the construct's `locals` as
    /// `LocalRebind`s of the try body's tuple on entry ([`ArmEntry`]), lowered by
    /// its frame (ADR 0131 §2).
    #[allow(clippy::too_many_arguments)] // the pieces of one `try ... of` clause
    fn push_ensure_success_cleanup(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        state_after_try: String,
        cleanup_block: &Block,
        families: &ThreadedFamilies,
        outer_version: usize,
        arm: Option<&ArmFrame>,
        locals: Vec<String>,
    ) -> Result<()> {
        // Extract Result and State from {Result, State} tuple using element/N
        let result_from_try = self.fresh_temp_var("TryResult");
        docs.push(docvec![
            "of ",
            leaf::var(state_after_try.clone()),
            " -> ",
            form::let_prefix(
                leaf::var(result_from_try.clone()),
                form::element(1, leaf::var(state_after_try.clone())),
            ),
            form::let_prefix(
                leaf::var("StateAcc"),
                form::element(2, leaf::var(state_after_try.clone())),
            ),
        ]);
        let entry = ArmEntry {
            tuple_var: state_after_try.clone(),
            locals,
        };

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
            docs.push(docvec![form::let_prefix(
                leaf::var(bare.to_string()),
                form::element(3, leaf::var(state_after_try))
            )]);
            self.set_family_version(prefix, 0);
            0
        } else {
            outer_version
        };

        let cleanup_success_arm = self.generate_exception_body_with_threading(
            cleanup_block,
            families,
            arm,
            Some(&entry),
        )?;
        let cleanup_success_slot =
            Self::exception_family_slot(families, cleanup_success_baseline, &cleanup_success_arm);
        docs.push(cleanup_success_arm.doc);
        // Return try body result with cleanup's final state
        docs.push(self.close_exception_result_tuple(
            result_from_try,
            cleanup_success_arm.state_version,
            families,
            &cleanup_success_slot,
        ));
        Ok(())
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
    /// ADR 0122 / BT-3506: when either block mutates the `SelfVt`
    /// family the current context makes eligible, every returned tuple above
    /// grows a trailing slot carrying that arm's own version — see
    /// [`Self::exception_construct_families`].
    pub(super) fn generate_ensure_with_mutations(
        &mut self,
        receiver_block: &Block,
        cleanup_block: &Block,
        producer: bool,
    ) -> Result<Document<'static>> {
        if self.is_repl_mode() {
            self.set_repl_loop_mutated(true);
        }
        // ADR 0131 §2 (Phase 3b): see `generate_on_do_with_mutations`. The try
        // body and both cleanup runs are `BranchArm` frames.
        let arm = self.exception_arm_frame(&[receiver_block, cleanup_block], producer);

        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let state_after_try = self.fresh_temp_var("StateAfterTry");

        // Rename current state to StateAcc
        let ExceptionOuterState {
            prefix: outer_prefix,
            state: current_state,
            private_scratch,
        } = self.exception_body_outer_state(receiver_block.span);
        // seed `__local__` keys for outer locals mutated by the try
        // (receiver) block or the cleanup block — mirrors `on:do:`'s seeding
        // (see `generate_on_do_with_mutations`) so a local written only
        // conditionally within one of the blocks (e.g. behind a nested
        // `ifTrue:`) still has its key present for the method-body
        // sequencer's extraction `maps:get/2`, even though the two blocks
        // here run sequentially rather than as alternatives.
        let (seed_doc, base_state) =
            self.seed_conditional_locals(&[receiver_block, cleanup_block], &current_state);
        let seed_doc = docvec![outer_prefix, seed_doc];
        // ADR 0122 / BT-3506: see `generate_on_do_with_mutations` — decided
        // from the AST up front so every arm's return tuple agrees on the
        // shape.
        let families = self.exception_construct_families(&[receiver_block, cleanup_block]);
        // The construct's own live baselines — restored between arms and
        // once more after the construct (see `generate_on_do_with_mutations`).
        let outer_self = self.self_version();
        let outer_version = families
            .as_slice()
            .first()
            .map_or(0, |prefix| self.family_version(prefix));
        // ADR 0137 §5: entering the `ensure:` stores the current `State` to the
        // cell, so a closure the protected body runs reads it, and the cleanup an
        // error runs starts from it.
        let cell_boundary = self.synced_calls_active() && !private_scratch;
        let entry_store = if cell_boundary {
            form::let_discard(threaded_ir::cell_put_call(leaf::var("StateAcc")))
        } else {
            Document::Nil
        };
        let mut docs: Vec<Document<'static>> = vec![docvec![
            seed_doc,
            form::let_prefix(leaf::var("StateAcc"), leaf::var(base_state)),
            entry_store,
            "try ",
        ]];

        // Generate try body with state threading; every field write in it also
        // stores to the cell (write-through).
        self.ensure_write_through += usize::from(cell_boundary);
        let try_arm = self.push_exception_arm(
            &mut docs,
            receiver_block,
            &families,
            outer_version,
            arm.as_ref(),
        );
        self.ensure_write_through -= usize::from(cell_boundary);
        let (try_result_var, try_final, try_slot) = try_arm?;
        // Return {Result, State[, Family]} from try body
        docs.push(self.close_exception_result_tuple(
            try_result_var,
            try_final,
            &families,
            &try_slot,
        ));

        // Success: run cleanup starting from try body's state
        let entry_locals = self.branch_threaded_locals(&[receiver_block, cleanup_block]);
        self.push_ensure_success_cleanup(
            &mut docs,
            state_after_try,
            cleanup_block,
            &families,
            outer_version,
            arm.as_ref(),
            entry_locals,
        )?;
        // ADR 0122 / BT-3506: the error path below is a SIBLING of the whole
        // `of` arm, not its successor, so it starts again from the
        // construct's own pre-`try` baselines rather than from the success
        // path's re-seed.
        self.set_self_version(outer_self);

        // ADR 0111 Addendum 5: sibling with_branch_context frames — the try
        // body, the success-path cleanup run, the error-path cleanup run and
        // the relay's (`cleanup_block` is compiled once per path, each its own
        // arm) — each `generate_exception_body_with_threading` call mints its
        // own fresh FrameId and `verify()`s its own real IR internally, so any
        // two independently reaching the same StateAcc version number is
        // correctly NOT a NonLinearVersion violation — see the matching
        // comment in `generate_on_do_with_mutations`.
        let catch = self.ensure_with_mutations_catch(
            EnsureCatchTriple {
                type_var,
                error_var,
                stack_var,
                carried_var: None,
            },
            cleanup_block,
            &families,
            arm.as_ref(),
            (cell_boundary, private_scratch),
            outer_version,
            receiver_block.span,
        )?;
        docs.push(catch);
        // ADR 0122 / BT-3506: leave the method's live versions as found — see
        // the matching restore at the end of `generate_on_do_with_mutations`.
        self.set_self_version(outer_self);

        Ok(Document::Vec(docs))
    }

    /// The catch of [`Self::generate_ensure_with_mutations`]: the error path
    /// (the cleanup from the pre-`try` state, or the cell refresh where the
    /// cell is live, then the re-raise) and, for a `^` of this scope, the
    /// BT-4312 relay ([`Self::ensure_relay`]), as one
    /// [`ThreadedStmt::EnsureCatch`]. `(cell_boundary, private_scratch)`:
    /// whether the actor-state cell is live here, and whether the construct's
    /// scratch map is private (`~{}~`). Leaves the `SelfVt` version at the
    /// construct's own baseline.
    #[allow(clippy::too_many_arguments)] // the pieces of one catch clause
    fn ensure_with_mutations_catch(
        &mut self,
        triple: EnsureCatchTriple,
        cleanup_block: &Block,
        families: &ThreadedFamilies,
        arm: Option<&ArmFrame>,
        (cell_boundary, private_scratch): (bool, bool),
        outer_version: usize,
        span: beamtalk_core::source_analysis::Span,
    ) -> Result<Document<'static>> {
        let outer_self = self.self_version();
        // Error: run cleanup for side effects (from original StateAcc), then
        // re-raise. ADR 0137 §5: the cleanup of a failed body starts from the
        // cell, which holds the state at entry plus the body's own writes made
        // before the error, keeping the locals the arm's scratch map seeded.
        let mut other: Vec<Document<'static>> = Vec::new();
        if cell_boundary {
            other.push(Self::ensure_cell_refresh_doc());
        }
        // The cleanup's state mutations are discarded (re-raise follows) —
        // including any extra-family version it mints: this path re-raises
        // rather than returning a tuple, so it has no trailing slot to carry
        // one (BT-3486/BT-3506). It still gets the version-0 arm seed, since
        // its own frame must verify like any other.
        other.push(self.seed_exception_arm_family(families, outer_version));
        let error_arm =
            self.generate_exception_body_with_threading(cleanup_block, families, arm, None)?;
        other.push(error_arm.doc);
        other.push(docvec![
            " ",
            Self::emit_raw_raise(
                triple.type_var.clone(),
                triple.error_var.clone(),
                triple.stack_var.clone()
            ),
        ]);
        self.set_self_version(outer_self);

        // BT-4312: a `^` of this scope carries its state to the boundary that
        // answers it, so the cleanup it runs threads from that state and the
        // throw resumes with the cleanup's result (`ensure_nlr_relay`). The
        // carried family is what the scope's throw carries (`NlrSlot`), and
        // nothing where this construct's scratch map is private (a
        // direct-params loop body, a class method): its writes die with the
        // scope whatever the boundary answers. The node records the carry and
        // `private_scratch` as they are; the verifier applies the same rule.
        let scope_carry = self.nlr_slot().carry();
        let carry = scope_carry.filter(|carry| carry.writable_by_cleanup(private_scratch));
        let cleanup_final = match carry {
            Some(NlrCarry::Bindings | NlrCarry::ActorState) => {
                (error_arm.state_version > 0).then_some(error_arm.state_version)
            }
            Some(NlrCarry::SelfVt) => error_arm.family_mutated_version.map(|(_, v)| v),
            // A scope's own throw is never foreign: only the
            // `Block>>ensure:` fallback relays a foreign one.
            Some(NlrCarry::Live) | None => None,
        };
        let writes_cell = self.block_touches_actor_state(cleanup_block);
        let relays = self.scope_nlr_token().is_some()
            && (cleanup_final.is_some() || (carry == Some(NlrCarry::Live) && writes_cell));
        let carried_var = relays.then(|| self.ensure_relay_carried_var());
        let relay = match (carry, &carried_var) {
            (Some(carry), Some(carried)) => Some(self.ensure_relay(
                carry,
                carried,
                cleanup_block,
                families,
                arm,
                cell_boundary,
                outer_version,
            )?),
            _ => None,
        };
        self.set_self_version(outer_self);
        // ADR 0146 R1: where the cell is live, a `^` the relay does not take
        // (one of another method of this actor, from a block it passed in)
        // must leave the cell holding the cleanup's final `StateAcc`: the
        // error path runs the cleanup with write-through off, so its writes
        // would die with the re-raise. A cleanup that threads no write has
        // nothing to store.
        //
        // BT-4598: where the relay of the scope's own throw already runs the
        // cleanup from the cell (`cell_store`), the foreign clause shares that
        // one compiled body instead of compiling the cleanup once more: the
        // two seeds are the same cell refresh, so their final `StateAcc`
        // versions are the same too. Each extra copy multiplies a nested
        // `ensure:` in the cleanup, so the cleanup is compiled 3^depth times,
        // not 4^depth.
        let foreign = match &relay {
            _ if !cell_boundary || cleanup_final.is_none() => None,
            Some(EnsureNlrRelay {
                cell_store: true,
                rethrow: RethrowSlot::Version(version),
                ..
            }) => Some(EnsureForeignNlr {
                body: ForeignCleanupBody::SharedWithRelay,
                store: Some(*version),
            }),
            _ => {
                let (seed, cleanup) =
                    self.ensure_cleanup_from_cell(cleanup_block, families, arm, outer_version)?;
                self.set_self_version(outer_self);
                Some(EnsureForeignNlr {
                    body: ForeignCleanupBody::Own {
                        seed,
                        cleanup: cleanup.doc,
                    },
                    store: Some(cleanup.state_version),
                })
            }
        };
        Ok(self.ensure_catch(
            EnsureCatchTriple {
                carried_var,
                ..triple
            },
            (scope_carry, private_scratch, cell_boundary),
            EnsureCleanup::Inlined {
                final_version: cleanup_final,
                writes_cell,
            },
            (relay, foreign),
            Document::Vec(other),
            span,
        ))
    }

    /// The cleanup of an inlined `ensure:` compiled once more, seeded from the
    /// actor-state cell (ADR 0137 §5): the cell refresh into `StateAcc` and
    /// the arm's family seed, then the cleanup's arm. Where a `^` unwinds
    /// through the `ensure:` with the cell live, the cell holds the state at
    /// the throw (a `^` stores before it throws, and every clause further in
    /// stores its own result), so this is the cleanup's entry state for the
    /// scope's own throw ([`Self::ensure_relay`]) and a foreign one alike.
    fn ensure_cleanup_from_cell(
        &mut self,
        cleanup_block: &Block,
        families: &ThreadedFamilies,
        arm: Option<&ArmFrame>,
        outer_version: usize,
    ) -> Result<(Document<'static>, ExceptionArm)> {
        let family_seed = self.seed_exception_arm_family(families, outer_version);
        let cleanup =
            self.generate_exception_body_with_threading(cleanup_block, families, arm, None)?;
        Ok((
            docvec![Self::ensure_cell_refresh_doc(), family_seed],
            cleanup,
        ))
    }

    /// ADR 0137 §5: `let StateAcc = refresh(Self, StateAcc) in`, the cell's
    /// fields over the scratch map's locals: where an `ensure:` cleanup that
    /// an error or a `^` runs starts from in an actor method (a `^` stores
    /// its state to the cell before it throws).
    fn ensure_cell_refresh_doc() -> Document<'static> {
        form::let_prefix(
            leaf::var("StateAcc"),
            actor_state_call(
                actor_state_helper::REFRESH,
                [leaf::var("Self"), leaf::var("StateAcc")],
            ),
        )
    }

    /// BT-4312: the `$bt_nlr` relay of an `ensure:` with state threading
    /// (`ensure_nlr_relay`): the cleanup compiled once more, seeded from the
    /// state the scope's own throw carries (`carried`, the clause's pattern
    /// variable), and the slot the throw resumes with.
    ///
    /// - [`NlrCarry::Bindings`] / [`NlrCarry::ActorState`]: the REPL's
    ///   bindings map, or an actor method's `State`. Seeded from the carried
    ///   map; where the actor-state
    ///   cell is live, from the cell refresh the error path uses, which holds
    ///   the carried state (a `^` stores it before it throws, and every relay
    ///   further in stores its own result). Re-throws the cleanup's final
    ///   `StateAcc`, stored to the cell first where it is live.
    /// - [`NlrCarry::SelfVt`]: a value-type method's `Self`, seeded from the
    ///   carried one; re-throws the cleanup's final `Self`.
    /// - [`NlrCarry::Live`]: the state is the cell, which the cleanup writes
    ///   in place; re-throws the carried state with the cell's fields over it
    ///   ([`RethrowSlot::CellOverCarried`]), so the `__local__` keys a merged
    ///   Tier 2 fun's `^` flushed keep the values they had at the `^`
    ///   (BT-4122), not the ones bound at this `ensure:`.
    #[allow(clippy::too_many_arguments)] // the pieces of one relay clause
    fn ensure_relay(
        &mut self,
        carry: NlrCarry,
        carried: &str,
        cleanup_block: &Block,
        families: &ThreadedFamilies,
        arm: Option<&ArmFrame>,
        cell_boundary: bool,
        outer_version: usize,
    ) -> Result<EnsureNlrRelay> {
        let cell_seed = || {
            if cell_boundary {
                Self::ensure_cell_refresh_doc()
            } else {
                Document::Nil
            }
        };
        let relay = match carry {
            NlrCarry::Bindings | NlrCarry::ActorState => {
                let (seed, cleanup) = if cell_boundary {
                    self.ensure_cleanup_from_cell(cleanup_block, families, arm, outer_version)?
                } else {
                    let family_seed = self.seed_exception_arm_family(families, outer_version);
                    let cleanup = self.generate_exception_body_with_threading(
                        cleanup_block,
                        families,
                        arm,
                        None,
                    )?;
                    (
                        docvec![
                            form::let_prefix(leaf::var("StateAcc"), leaf::var(carried.to_string())),
                            family_seed
                        ],
                        cleanup,
                    )
                };
                EnsureNlrRelay {
                    seed,
                    cleanup: cleanup.doc,
                    cell_store: cell_boundary,
                    rethrow: RethrowSlot::Version(cleanup.state_version),
                }
            }
            NlrCarry::SelfVt => {
                // The seed below binds the bare `Self` to the carried one, so
                // the cleanup's arm mints `Self1`, `Self2`, … from there. Looked
                // up by name: `families` is ordered, and `SelfVt` need not be
                // its first member.
                if families.contains(&VersionPrefix::SelfVt) {
                    self.set_family_version(&VersionPrefix::SelfVt, 0);
                }
                let cleanup = self.generate_exception_body_with_threading(
                    cleanup_block,
                    families,
                    arm,
                    None,
                )?;
                EnsureNlrRelay {
                    seed: docvec![
                        cell_seed(),
                        form::let_prefix(leaf::var("Self"), leaf::var(carried.to_string())),
                    ],
                    cleanup: cleanup.doc,
                    cell_store: false,
                    rethrow: RethrowSlot::Version(
                        cleanup.family_mutated_version.map_or(0, |(_, v)| v),
                    ),
                }
            }
            NlrCarry::Live => {
                let family_seed = self.seed_exception_arm_family(families, outer_version);
                let cleanup = self.generate_exception_body_with_threading(
                    cleanup_block,
                    families,
                    arm,
                    None,
                )?;
                EnsureNlrRelay {
                    seed: docvec![cell_seed(), family_seed],
                    cleanup: cleanup.doc,
                    cell_store: false,
                    rethrow: RethrowSlot::CellOverCarried,
                }
            }
        };
        Ok(relay)
    }
}
