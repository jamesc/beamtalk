// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Exception / local-threading producer lowering and the REPL producers
//! (ADR 0131), split from `threaded_expr.rs`.

use super::threaded_ir::{
    self, BindOp, CarrierSlot, FrameId, RebindFrame, RebindLowering, ThreadedLocalSlot,
    ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, VersionedVar,
};
use super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::{Block, Expression};
use beamtalk_core::source_analysis::Span;

#[cfg(any(test, feature = "test"))]
use super::fault;
use super::threaded_expr::{ProducerTail, ReplParamSeed, ReplProducerDoc};

impl CoreErlangGenerator {
    /// The producer lowering of an `on:do:`/`ensure:` in `frame`
    /// ([`Self::exception_producer_locals`]), or `Ok(None)` when it is not one
    /// here.
    pub(super) fn exception_producer_value(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        let inner = expr.unwrap_parens();
        let Some(locals) = self.exception_producer_locals(inner) else {
            return Ok(None);
        };
        let (prelude, value) = self.lower_exception_producer(inner, &locals, tail, frame)?;
        self.report_rebinds_visible(&prelude, inner.span());
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// ADR 0131 §1/§2 (Phase 3b): the local-threading producer for an
    /// `on:do:`/`ensure:`:
    ///
    /// ```text
    /// prelude: [ ConstructTuple { carrier: CF, doc: <try/catch tuple>, threads },
    ///            Bind State_{n+1} ← element(2, CF)           (actor instance only),
    ///            LocalRebind { t ← maps:get('__local__t', …) }, …,
    ///            Statement(Self ← element(3, CF))             (value-type `SelfVt` only) ]
    /// value:   element(1, CF)
    /// ```
    ///
    /// The protected body, handler and cleanup are `BranchArm` frames
    /// (`generate_exception_body_with_threading`) whose threads are the
    /// construct's seeded set; the catch boundary (`OnDoCatch`, the class-variable
    /// snapshot and restore of ADR 0130) is untouched. In a class or value-type
    /// method the scratch map is private (`exception_body_outer_state`), so no
    /// `State` `Bind` exists; in an actor instance method element 2 is the next
    /// `State` version and each rebind reads through it.
    pub(super) fn lower_exception_producer(
        &mut self,
        expr: &Expression,
        locals: &[String],
        tail: ProducerTail,
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, ValueRef)> {
        #[cfg(any(test, feature = "test"))]
        fault::record_producer(expr.span(), locals);
        let span = expr.span();
        let actor = self.in_actor_instance_context();
        let enclosing = self.rebind_frame_of(frame)?;
        let self_families = self.exception_construct_threaded_families(expr);
        let source_version = self.state_version();
        let doc = self.exception_producer_tuple_doc(expr)?;
        let carrier = self.fresh_temp_var("ThreadedResult");
        let element = |k: usize, carrier: &str| form::element(k, leaf::var(carrier.to_string()));
        let mut family_binds = Vec::new();
        if actor && enclosing.kind.extends_state_chain() {
            let _ = self.next_state_var();
            family_binds.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, self.state_version(), frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(element(2, &carrier))),
                span,
            });
        }
        let mut prelude = self.construct_rebind_prelude(
            &enclosing,
            &carrier,
            doc,
            locals,
            tail,
            family_binds,
            frame,
            span,
        );
        if let Some(rebind) = self.extract_vt_loop_family_slot(&carrier, &self_families, span) {
            prelude.push(ThreadedStmt::Statement(docvec![rebind, " "], span));
        }
        Ok((prelude, ValueRef::Doc(element(1, &carrier))))
    }

    /// Lowers the loop or fold `expr` to its construct-tuple prelude (ADR
    /// 0131 §1) and its logical value, `element(1, CF)`. `locals` is the set
    /// the construct packs ([`Self::construct_tuple_locals`]); `tail` says
    /// whether they are rebound or, at the method root's last expression,
    /// discarded.
    ///
    /// The frame is the method root ([`RebindFrame::implicit_root`]): outside
    /// the REPL it threads nothing, so ADR 0131 §2's table lowers every
    /// rebind to a plain `let` of a fresh variable that `local` is then bound
    /// to. In an actor instance method the construct's element 2 is also the
    /// `State` family's next version: its `Bind` comes first and each
    /// rebind reads through it (`state_first`). In a value-type method a
    /// Letrec loop that threads `Self` carries it at element 3, rebound last.
    pub(super) fn lower_local_threading_construct(
        &mut self,
        expr: &Expression,
        locals: &[String],
        tail: ProducerTail,
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, ValueRef)> {
        // A REPL producer is recorded by `lower_repl_construct_tuple` once it
        // knows the construct produced its `{Value, Bindings}` carrier.
        #[cfg(any(test, feature = "test"))]
        if !self.is_repl_mode() {
            fault::record_producer(expr.span(), locals);
        }
        // The construct's own receiver and non-block arguments evaluate
        // before it packs its locals (ADR 0118 §Decision 3): they are
        // sequenced here, for every tail, so `Rebind` and `Discard` cannot
        // diverge on a producer nested in an operand.
        let children = Self::sequenced_send_children(expr).unwrap_or_default();
        let (mut prelude, scope) = self.sequence_children(&children, frame)?;
        let (stmts, value) = if self.is_repl_mode() {
            self.lower_repl_construct_tuple(expr, frame)?
        } else {
            self.lower_construct_tuple(expr, locals, tail, frame)?
        };
        self.finish_precompiled_scope(scope)?;
        prelude.extend(stmts);
        Ok((prelude, value))
    }

    /// The REPL root's half of [`Self::lower_construct_tuple`] (ADR 0131
    /// Phase 2d, BT-3795; §2's `MethodBody (REPL)` row).
    ///
    /// The REPL threads a loop or fold through its bindings map
    /// (`KeyStyle::ReplPlain`): when the construct compiles to a
    /// `{Value, Bindings}` tuple, the builder says so by setting
    /// `repl_loop_mutated`, the flag the REPL assembler reads to unpack a
    /// statement-level construct. This producer is the unpacker for one at an
    /// operand site, so it consumes the flag: the tuple is bound, element 2 —
    /// the whole new bindings map — becomes the next `State` version, and the
    /// value is element 1.
    ///
    /// The bindings are adopted whole, not rebound one `Put` per local: a name
    /// the construct introduces (`xs do: [:x | y := x]`) is in the map only
    /// when the loop ran, and a per-key `maps:get` would crash on an empty
    /// one. The statement-level REPL path adopts element 2 the same way.
    /// Hence no `LocalRebind`, and an empty `threads`: the construct threads
    /// the `State` family (the bindings map), as an actor fold does.
    ///
    /// A construct that does not compile to a tuple (the flag stayed unset)
    /// is a plain value with no prelude.
    ///
    /// `frame` is the frame the prelude is spliced into: the REPL root, or
    /// (BT-3823) a REPL arm, whose `StateAcc` is the bindings map itself
    /// ([`Self::with_repl_arm_producer_sites`]), so the adoption is the same
    /// there: element 2 is the arm's next `StateAcc`.
    ///
    /// BT-4461: a REPL frame other than the root also holds the bindings it
    /// has written in Core variables (`m := 1` in an arm binds `m` to the
    /// assigned value), and AST-directed code reads a bound name from its
    /// variable rather than from the map. The invariant, in every REPL frame:
    /// a binding the frame holds in a variable and the construct writes
    /// ([`Self::repl_cached_bindings_written_by`]) is read out of the map the
    /// construct threads inside it, and out of the adopted map after it. So the
    /// construct compiles with those variables dropped (its loop condition and
    /// body read the binding from their own `StateAcc`), its `ConstructTuple`
    /// threads them, and one `LocalRebind` each (§2's `StateAcc` cell, held in
    /// the adopted map) binds them anew. Before, the frame kept reading the
    /// value from before the construct (`c ifTrue: [m := 1. c ifTrue: [m := 5]
    /// ifFalse: [nil]. m] ifFalse: [0]` answered 1; a `[m < 5] whileTrue:
    /// [m := m + 1]` after `m := 1` in an arm never ended), which
    /// `VerifyError::ReplCachedBindingStale` reports. At the root no binding is
    /// held in a variable, so nothing changes there.
    fn lower_repl_construct_tuple(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, ValueRef)> {
        let span = expr.span();
        // BT-4499: a block parameter the construct writes joins the bindings
        // map for the construct (and leaves it after).
        let params = self.repl_param_bindings_written_by(expr);
        let (seed_prelude, seeds) = self.seed_repl_params(&params, frame, span)?;
        let cached = self.repl_producer_cached_bindings(expr);
        let dropped = self.drop_cached_repl_bindings(&cached);
        let enclosing = self.repl_loop_mutated();
        self.set_repl_loop_mutated(false);
        let source_version = self.state_version();
        let previous = self.set_repl_unpack_site(Some(Self::repl_unpack_span(expr)));
        let doc = self.with_cleared_direct_params_loop(|this| this.expression_doc(expr));
        self.set_repl_unpack_site(previous);
        let doc = doc?;
        let yields_tuple = self.repl_loop_mutated();
        self.set_repl_loop_mutated(enclosing);
        if !yields_tuple {
            // No bindings map comes back, so the frame's map and the variables
            // it held are unchanged.
            for (local, var) in &dropped {
                self.bind_var(local, var);
            }
            if seeds.is_empty() {
                return Ok((Vec::new(), ValueRef::Doc(doc)));
            }
            // The value reads the seeded map, so it is evaluated before the
            // seeds are undone.
            let result = self.fresh_temp_var("Value");
            let mut prelude = seed_prelude;
            prelude.push(ThreadedStmt::Statement(
                docvec!["let ", leaf::var(result.clone()), " = ", doc, " in "],
                span,
            ));
            prelude.extend(self.unseed_repl_params(&seeds, frame, span));
            return Ok((prelude, ValueRef::Var(result)));
        }
        // BT-3823 review: recorded only once the carrier exists, so an
        // agreement test counts a REPL site as lowered only when its bindings
        // map is adopted (a site the producer reached without a carrier would
        // drop its write). The bindings the construct writes are recorded: the
        // REPL producer threads no local of its own.
        #[cfg(any(test, feature = "test"))]
        fault::record_producer(
            span,
            &self
                .threaded_locals_of(expr)
                .map(|set| set.names)
                .unwrap_or_default(),
        );
        let carrier = self.fresh_temp_var("ThreadedResult");
        let element = |k: usize, carrier: &str| form::element(k, leaf::var(carrier.to_string()));
        let _ = self.next_state_var();
        let adopt = ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, self.state_version(), frame),
            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
            op: BindOp::Direct(ValueRef::Doc(element(2, &carrier))),
            span,
        };
        let adoption =
            self.repl_adoption_prelude(&carrier, doc, &cached, vec![adopt], frame, span)?;
        let mut prelude = seed_prelude;
        prelude.extend(adoption);
        prelude.extend(self.unseed_repl_params(&seeds, frame, span));
        Ok((prelude, ValueRef::Doc(element(1, &carrier))))
    }

    /// BT-4461: the bindings a REPL construct `expr` writes (the REPL's one
    /// producer rule, `repl_producer_writes`, for every family `match:`
    /// included, with [`Self::threaded_locals_of`]'s set) that the frame
    /// lowering it holds in a Core variable. Empty at the REPL root, which reads
    /// every binding from the bindings map, and outside the REPL. Read just
    /// before `expr` is lowered, by its producer
    /// ([`Self::lower_repl_construct_tuple`], and a REPL `match:`'s) and by the
    /// REPL frame's producer site check ([`Self::repl_frame_producer_sites`]),
    /// so the two agree.
    pub(super) fn repl_cached_bindings_written_by(&self, expr: &Expression) -> Vec<String> {
        let mut names = self.repl_written_locals(expr);
        names.retain(|local| self.holds_repl_map_binding(local));
        names
    }

    /// BT-4499: the locals a REPL construct `expr` writes (sorted, no
    /// duplicates), before the frame's own variables are consulted. Empty
    /// outside the REPL.
    fn repl_written_locals(&self, expr: &Expression) -> Vec<String> {
        if !self.is_repl_mode() {
            return Vec::new();
        }
        let mut names =
            beamtalk_core::semantic_analysis::block_facts::repl_producer_writes(expr, &|name| {
                self.binds_threadable_local(name)
            })
            .map(Self::write_names)
            .unwrap_or_default();
        if let Some(set) = self.threaded_locals_of(expr) {
            names.extend(set.names);
        }
        names.sort();
        names.dedup();
        names
    }

    /// BT-4499: the bindings a REPL construct `expr` writes that the frame holds
    /// in a Core variable, whether or not that variable mirrors a key of the
    /// bindings map: what the frame's producer site check
    /// ([`Self::repl_frame_producer_sites`]) requires the construct to thread.
    pub(super) fn repl_frame_bindings_written_by(&self, expr: &Expression) -> Vec<String> {
        let mut names = self.repl_written_locals(expr);
        names.retain(|local| self.lookup_var(local).is_some());
        names
    }

    /// BT-4499: the bindings `expr` writes that the frame holds in a variable
    /// that is not a key of the bindings map: a block parameter (a fold's `:i`
    /// or `:acc`, an `ifNotNil:` `:v`) or a block temporary. The construct
    /// reads and writes the bindings map, so these are seeded into it first
    /// ([`Self::seed_repl_params`]).
    pub(super) fn repl_param_bindings_written_by(&self, expr: &Expression) -> Vec<String> {
        if fault!(ReplParamsNotSeeded) {
            return Vec::new();
        }
        let mut names = self.repl_written_locals(expr);
        names.retain(|local| {
            self.lookup_var(local)
                .is_some_and(|var| !self.repl_map_vars.contains(var))
        });
        names
    }

    /// BT-4461: whether the frame holds `local` in a Core variable that mirrors
    /// its key in the bindings map ([`Self::repl_map_vars`]). A block
    /// parameter is bound to a variable too, but is not a map key until
    /// [`Self::seed_repl_params`] puts it there for the construct that writes it.
    fn holds_repl_map_binding(&self, local: &str) -> bool {
        self.lookup_var(local)
            .is_some_and(|var| self.repl_map_vars.contains(var))
    }

    /// BT-4499: makes each of `params` (block parameters a REPL construct
    /// writes, [`Self::repl_param_bindings_written_by`]) a member of the
    /// frame's bindings map for the duration of the construct, so the
    /// construct's loop condition and body read it from the map they thread and
    /// the frame reads it back out of the adopted map, as for any binding the
    /// frame holds in a variable ([`Self::repl_cached_bindings_written_by`]).
    /// Answers the prelude, which saves the key's previous entry (a workspace
    /// binding the parameter shadows) and `Put`s the parameter, and the seeds
    /// [`Self::unseed_repl_params`] undoes. The parameter is a map-mirroring
    /// variable from here on.
    pub(super) fn seed_repl_params(
        &mut self,
        params: &[String],
        frame: FrameId,
        span: Span,
    ) -> Result<(Vec<ThreadedStmt>, Vec<ReplParamSeed>)> {
        let mut prelude = Vec::new();
        let mut seeds = Vec::new();
        for local in params {
            let Some(original) = self.lookup_var(local).cloned() else {
                continue;
            };
            let previous = self.fresh_temp_var("Previous");
            prelude.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(previous.clone()),
                    " = call 'maps':'find'(",
                    leaf::atom(local.clone()),
                    ", ",
                    leaf::var(self.current_state_var()),
                    ") in ",
                ],
                span,
            ));
            let repl_root = frame == FrameId::ROOT;
            let enclosing = if repl_root {
                RebindFrame::implicit_root()
            } else {
                self.rebind_frame_of(frame)?
            };
            let enclosing = enclosing.with_repl_binding(local);
            let carrier = self.fresh_temp_var("Param");
            let slot = ThreadedLocalSlot {
                local: local.clone(),
                slot: CarrierSlot::Whole,
                value_var: self.fresh_temp_var(local),
            };
            let rebind = threaded_ir::build_local_threading_prelude(
                &enclosing,
                &carrier,
                leaf::var(original.clone()),
                std::slice::from_ref(&slot),
                Vec::new(),
                span,
                |slot, shape| self.mint_rebind_lowering(slot, shape, frame),
            );
            self.note_repl_map_rebinds(&rebind);
            prelude.extend(rebind);
            self.bind_var(local, &slot.value_var);
            seeds.push(ReplParamSeed {
                local: local.clone(),
                previous,
            });
        }
        // A `^` thrown inside the construct carries the bindings map with
        // these seeds undone (`Self::repl_nlr_state_doc`).
        self.repl_active_seeds.extend(seeds.iter().cloned());
        Ok((prelude, seeds))
    }

    /// BT-4499: `state` (a bindings map) with `seed`'s key put back to what it
    /// held before the seed.
    fn repl_seed_restore_doc(&mut self, seed: &ReplParamSeed, state: &str) -> Document<'static> {
        let saved = self.fresh_temp_var("Saved");
        docvec![
            "case ",
            leaf::var(seed.previous.clone()),
            " of <{'ok', ",
            leaf::var(saved.clone()),
            "}> when 'true' -> call 'maps':'put'(",
            leaf::atom(seed.local.clone()),
            ", ",
            leaf::var(saved),
            ", ",
            leaf::var(state.to_string()),
            ") <'error'> when 'true' -> call 'maps':'remove'(",
            leaf::atom(seed.local.clone()),
            ", ",
            leaf::var(state.to_string()),
            ") end",
        ]
    }

    /// BT-4499: the bindings map a REPL `^` throws from inside a seeded
    /// construct: the current map with every live seed undone, so the eval
    /// commits no block parameter (and keeps a workspace binding it shadows).
    /// Answers the bare map variable when no seed is live.
    pub(in crate::core_erlang) fn repl_nlr_state_doc(&mut self) -> Document<'static> {
        let seeds = self.repl_active_seeds.clone();
        let mut state = self.current_state_var();
        let mut lets = Vec::new();
        for seed in seeds.iter().rev() {
            let next = self.fresh_temp_var("NlrState");
            let restore = self.repl_seed_restore_doc(seed, &state);
            lets.push(docvec![
                "let ",
                leaf::var(next.clone()),
                " = ",
                restore,
                " in "
            ]);
            state = next;
        }
        docvec![Document::Vec(lets), leaf::var(state)]
    }

    /// BT-4499: ends the seeds [`Self::seed_repl_params`] made, after their
    /// construct: each parameter's key goes back to what it held before (the
    /// workspace binding the parameter shadows, or no key), so a parameter
    /// never leaks into the workspace bindings, and the parameter's variable no
    /// longer mirrors a key.
    pub(super) fn unseed_repl_params(
        &mut self,
        seeds: &[ReplParamSeed],
        frame: FrameId,
        span: Span,
    ) -> Vec<ThreadedStmt> {
        let mut prelude = Vec::new();
        let keep = self.repl_active_seeds.len().saturating_sub(seeds.len());
        self.repl_active_seeds.truncate(keep);
        for seed in seeds {
            if let Some(var) = self.lookup_var(&seed.local).cloned() {
                self.repl_map_vars.remove(&var);
            }
            let source_version = self.state_version();
            let state = self.current_state_var();
            let _ = self.next_state_var();
            let restore = self.repl_seed_restore_doc(seed, &state);
            prelude.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, self.state_version(), frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(restore)),
                span,
            });
        }
        prelude
    }

    /// BT-4461: records the variables of `prelude`'s `LocalRebind`s that step
    /// or are held in the frame's map (`MapPut`/`MapHeld`): in the REPL they
    /// mirror their binding's key ([`Self::repl_map_vars`]).
    pub(super) fn note_repl_map_rebinds(&mut self, prelude: &[ThreadedStmt]) {
        if !self.is_repl_mode() {
            return;
        }
        for stmt in prelude {
            if let ThreadedStmt::LocalRebind {
                value_var,
                lowering: RebindLowering::MapPut { .. } | RebindLowering::MapHeld { .. },
                ..
            } = stmt
            {
                self.repl_map_vars.insert(value_var.clone());
            }
        }
    }

    /// [`Self::repl_cached_bindings_written_by`] as a REPL producer rethreads
    /// them: none under the test-only fault `ReplCachedBindingsNotRethreaded`,
    /// while the frame's site check still reads them.
    pub(super) fn repl_producer_cached_bindings(&self, expr: &Expression) -> Vec<String> {
        if fault!(ReplCachedBindingsNotRethreaded) {
            return Vec::new();
        }
        self.repl_cached_bindings_written_by(expr)
    }

    /// BT-4461: hides the frame's Core variables for `cached`
    /// ([`Self::repl_cached_bindings_written_by`]) before their construct is
    /// compiled, so the construct reads them out of the map it threads (a loop
    /// condition and body see the loop's own writes). Answers each hidden
    /// `(binding, variable)`, for a construct that hands no map back to bind
    /// again.
    pub(super) fn drop_cached_repl_bindings(&mut self, cached: &[String]) -> Vec<(String, String)> {
        cached
            .iter()
            .filter_map(|local| self.hide_var(local).map(|var| (local.clone(), var)))
            .collect()
    }

    /// BT-4461: [`Self::repl_producer_cached_bindings`] for an inline
    /// conditional producer handed its blocks rather than its send: the
    /// bindings the blocks write (the [`Self::threaded_locals_of`] kernel,
    /// `threaded_block_writes`) that the frame holds in a variable. Read once
    /// the construct is compiled, which leaves the frame's scope as it found
    /// it.
    pub(super) fn repl_producer_cached_bindings_of_blocks(&self, blocks: &[&Block]) -> Vec<String> {
        if fault!(ReplCachedBindingsNotRethreaded) {
            return Vec::new();
        }
        if !self.is_repl_mode() || blocks.is_empty() {
            return Vec::new();
        }
        let mut names = Self::write_names(
            beamtalk_core::semantic_analysis::block_facts::threaded_block_writes(blocks, &|name| {
                self.binds_threadable_local(name)
            }),
        );
        names.retain(|local| self.holds_repl_map_binding(local));
        names
    }

    /// BT-4461: the prelude of a REPL producer that adopts the bindings map
    /// whole: the construct tuple bound to `carrier`, threading `cached`, the
    /// adoption `adopt` (the `State` `Bind` of `element(2, carrier)`) and one
    /// `LocalRebind` per cached binding, which the frame then reads. Every
    /// binding is a member of a REPL frame's map
    /// (`RebindFrame::with_repl_binding`), so each rebind is §2's `StateAcc`
    /// cell, held in the map just adopted (`MapHeld`). The one lowering of
    /// [`Self::lower_repl_construct_tuple`] and a REPL `match:`.
    pub(super) fn repl_adoption_prelude(
        &mut self,
        carrier: &str,
        doc: Document<'static>,
        cached: &[String],
        adopt: Vec<ThreadedStmt>,
        frame: FrameId,
        span: Span,
    ) -> Result<Vec<ThreadedStmt>> {
        let enclosing = if cached.is_empty() {
            RebindFrame::implicit_root()
        } else {
            cached
                .iter()
                .fold(self.rebind_frame_of(frame)?, |f, local| {
                    f.with_repl_binding(local)
                })
        };
        let slots: Vec<ThreadedLocalSlot> = cached
            .iter()
            .map(|local| ThreadedLocalSlot {
                local: local.clone(),
                slot: CarrierSlot::Key(self.local_scratch_key(local)),
                value_var: self.fresh_temp_var(local),
            })
            .collect();
        let prelude = threaded_ir::build_local_threading_prelude(
            &enclosing,
            carrier,
            doc,
            &slots,
            adopt,
            span,
            |slot, shape| self.mint_rebind_lowering(slot, shape, frame),
        );
        self.note_repl_map_rebinds(&prelude);
        for slot in &slots {
            self.bind_var(&slot.local, &slot.value_var);
        }
        Ok(prelude)
    }

    /// ADR 0131 Phase 2d (BT-3795): compiles `target`, an expression of the
    /// REPL input statement `stmt` (the statement itself, or the value of an
    /// assignment in a multi-statement input), when `stmt` holds a
    /// local-threading producer at one of its `repl_operand_sites`: through
    /// [`Self::threaded_expression`], returning the producer's prelude and the
    /// value, for the REPL assembler to emit ahead of its own
    /// `let Result = <value> in`. `None`, with nothing compiled, for any other
    /// statement — it keeps the assembler's lowering, byte for byte.
    ///
    /// `mutated` is `repl_loop_mutated` after the compile: whether the value
    /// is itself a `{Value, Bindings}` tuple (a construct that is the
    /// statement or the whole value of an assignment, which is not a site),
    /// exactly as `expression_doc_with_repl_mutation_tracking` reports it.
    ///
    /// # Errors
    ///
    /// Returns [`CodeGenError`] if compiling `target` fails.
    pub fn repl_statement_with_producers(
        &mut self,
        stmt: &Expression,
        target: &Expression,
    ) -> Result<Option<ReplProducerDoc>> {
        let sites: std::collections::HashSet<Span> =
            beamtalk_core::semantic_analysis::block_facts::repl_operand_sites(stmt)
                .into_iter()
                .map(Expression::span)
                .collect();
        let saved = std::mem::replace(&mut self.local_producer_sites, sites);
        let result = self.repl_statement_in_sites(stmt, target);
        self.local_producer_sites = saved;
        result
    }

    /// [`Self::repl_statement_with_producers`] once the sites are set.
    fn repl_statement_in_sites(
        &mut self,
        stmt: &Expression,
        target: &Expression,
    ) -> Result<Option<ReplProducerDoc>> {
        if !self.statement_has_repl_producer(stmt) {
            return Ok(None);
        }
        self.set_repl_loop_mutated(false);
        let tv = self.threaded_expression(target, FrameId::ROOT)?;
        let mutated = self.repl_loop_mutated();
        self.set_repl_loop_mutated(false);
        let prelude = self.verified_repl_root_prelude_doc(tv.prelude, target.span());
        let value = self.threaded_value_doc(&tv.value);
        Ok(Some(ReplProducerDoc {
            prelude,
            value,
            mutated,
        }))
    }

    /// Whether any `repl_operand_sites` site of `stmt` is a REPL producer.
    fn statement_has_repl_producer(&self, stmt: &Expression) -> bool {
        // BT-3910: a value-position local assignment in the statement.
        !self.contained_local_assignment_targets(stmt).is_empty()
            || beamtalk_core::semantic_analysis::block_facts::repl_operand_sites(stmt)
                .into_iter()
                .any(|site| self.local_producer_locals(site).is_some())
    }
}
