// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test-only observation probes for the synced-call lowering (BT-4688).
//!
//! Moved out of `synced_call.rs` so the production lowering carries none of
//! this: the whole module is compiled only under the test configuration
//! (`cfg(any(test, feature = "test"))`, see [`crate::core_erlang::fault`]).
//! The faults themselves (`Fault::NoBrackets`, ...) live in the shared
//! registry; this module only observes — count-only mode, the gap site log,
//! the backtrace that names a gap's lowering site, and the counters the
//! `nlr_closure_call_corpus` asserts on.

// Used by this crate's unit tests only; a dependent crate that enables the
// `test` feature compiles the probes but never calls them.
#![cfg_attr(not(test), allow(dead_code))]

use super::{CoreErlangGenerator, PrestoreRead, UserCodeRegion, block_literal_operands};
use beamtalk_core::ast::{Expression, MessageSelector};
use beamtalk_core::source_analysis::Span;

/// Where in the code generator a gap was compiled: [`capture_site`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(in crate::core_erlang) struct GapSite {
    /// `file:function` of the nearest lowering frame that is not generic plumbing.
    pub(in crate::core_erlang) site: String,
    /// The codegen frames, innermost first, for choosing the plumbing list.
    pub(in crate::core_erlang) chain: String,
}

/// One gap as the invariant probe sees it (`tests/synced_call_sites.rs`).
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct SiteRecord {
    pub(in crate::core_erlang) site: String,
    pub(in crate::core_erlang) chain: String,
    pub(in crate::core_erlang) selector: String,
    pub(in crate::core_erlang) region: UserCodeRegion,
}

thread_local! {
    /// Test-only count of [`CoreErlangGenerator::classify_send`] calls
    /// (BT-4524), so a test can show the probes classify each send a
    /// bounded number of times rather than once per enclosing probe.
    pub(in crate::core_erlang) static CLASSIFY_SEND_CALLS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
    /// Every send [`CoreErlangGenerator::note_dispatch_nlr_literals`] logged:
    /// its selector, and whether it runs none of its operands
    /// ([`CoreErlangGenerator::send_runs_no_operand`]).
    pub(in crate::core_erlang) static DISPATCH_NLR_LITERALS:
        std::cell::RefCell<Vec<(String, bool)>> = const { std::cell::RefCell::new(Vec::new()) };
    /// The reads `Fault::PrestoreReadsBeforeOperands` stores instead, by the
    /// conditional's first block argument (the pre-store's `at`)
    /// ([`CoreErlangGenerator::publish_fault_prestore_read`]); cleared with
    /// the fault.
    pub(in crate::core_erlang) static PRESTORE_EARLY_READ: std::cell::RefCell<
        std::collections::HashMap<Span, Option<PrestoreRead>>,
    > = std::cell::RefCell::new(std::collections::HashMap::new());
}

thread_local! {
    /// Count-only mode (the invariant probe): gaps and other verifier findings
    /// are recorded, not failed on.
    static COUNT_ONLY: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Restores count-only mode on drop.
#[derive(Debug)]
pub(in crate::core_erlang) struct CountOnly(bool);

impl Drop for CountOnly {
    fn drop(&mut self) {
        COUNT_ONLY.with(|c| c.set(self.0));
    }
}

/// Puts the current thread in count-only mode until the guard drops.
pub(in crate::core_erlang) fn count_only() -> CountOnly {
    CountOnly(COUNT_ONLY.with(|c| c.replace(true)))
}

thread_local! {
    /// Every gap a count-only compile on this thread recorded.
    pub(in crate::core_erlang) static SITE_LOG: std::cell::RefCell<Vec<SiteRecord>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Frames that are plumbing every expression passes through, never the owning
/// lowering: skipped when naming a gap's site.
const PLUMBING_FRAMES: &[&str] = &[
    "capture_site",
    "record_gap",
    "note_send",
    "note_self_dispatch",
    "generate_message_send",
    "generate_expression",
    "generate_expression_inner",
    "expression_doc",
    "generate_runtime_dispatch",
    "generate_binary_op",
    "guarded_op_doc",
];

/// The `file:function` site of the code generator frame that compiled the
/// current call (count-only mode; a backtrace per gap is too slow otherwise).
pub(in crate::core_erlang) fn capture_site() -> GapSite {
    let text = std::backtrace::Backtrace::force_capture().to_string();
    let mut frames: Vec<(String, String)> = Vec::new();
    let mut lines = text.lines().peekable();
    while let Some(line) = lines.next() {
        let trimmed = line.trim_start();
        let Some((index, symbol)) = trimmed.split_once(": ") else {
            continue;
        };
        if !index.chars().all(|c| c.is_ascii_digit()) {
            continue;
        }
        // The frame's source line follows its symbol; only code generator frames count.
        let Some(at) = lines
            .peek()
            .and_then(|next| next.trim_start().strip_prefix("at "))
        else {
            continue;
        };
        let path = at.rsplitn(3, ':').last().unwrap_or(at).replace('\\', "/");
        let Some((_, file)) = path.split_once("src/core_erlang/") else {
            continue;
        };
        let function = symbol
            .split('<')
            .next()
            .unwrap_or(symbol)
            .trim()
            .to_string();
        frames.push((file.to_string(), function));
    }
    let chain = frames
        .iter()
        .take(14)
        .map(|(file, function)| format!("{file}:{function}"))
        .collect::<Vec<_>>()
        .join(" < ");
    let site = frames
        .iter()
        .find(|(_, function)| {
            !PLUMBING_FRAMES.contains(&function.as_str())
                && !function.starts_with('{')
                && !function.starts_with("with_")
        })
        .map(|(file, function)| format!("{file}:{function}"))
        .unwrap_or_default();
    GapSite { site, chain }
}

impl CoreErlangGenerator {
    /// BT-4612 (test-only): logs a send that reaches the runtime dispatch
    /// (or, BT-4633, the typed guarded send) with a `^`-holding block literal
    /// among its operands that no store covers, where the cell protocol
    /// applies, with whether it runs none of its operands
    /// ([`DISPATCH_NLR_LITERALS`]). Neither dispatch has a pre-store, so
    /// every logged send must be one that runs none
    /// ([`Self::send_runs_no_operand`]: a [`SendUserCode::Never`] send, or a
    /// `no_user_code` primitive, BT-4633).
    ///
    /// BT-4632: a bracketed send's own block literals are covered by its
    /// window ([`Self::open_sync_window`]); a send nested in its operands is
    /// logged like one outside every window, since the window's store
    /// covers none of its closures.
    pub(in crate::core_erlang) fn note_dispatch_nlr_literals(
        &self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) {
        let blocks: Vec<&beamtalk_core::ast::Block> = block_literal_operands(receiver, arguments)
            .filter(|block| !self.synced.nlr_cover.contains(&block.span))
            .collect();
        if !self.nlr_closure_prestore_applies(&blocks) {
            return;
        }
        let runs_no_operand = self.send_runs_no_operand(receiver, selector, arguments);
        DISPATCH_NLR_LITERALS.with(|log| {
            log.borrow_mut()
                .push((selector.name().to_string(), runs_no_operand));
        });
    }

    /// BT-4612 fault hook (`Fault::PrestoreReadsBeforeOperands`), called
    /// where the conditional `send` sequences its receiver ahead (the operand
    /// its non-Boolean fallback's pre-store follows): the pre-store's `at`
    /// (the conditional's first block argument) and the read a pre-store made
    /// here would take, for [`Self::publish_fault_prestore_read`] once the
    /// receiver is sequenced. `None` with the fault off.
    pub(in crate::core_erlang) fn fault_prestore_read_before_operands(
        &self,
        send: &Expression,
    ) -> Option<(Span, Option<PrestoreRead>)> {
        if !fault!(PrestoreReadsBeforeOperands) {
            return None;
        }
        let Expression::MessageSend { arguments, .. } = send else {
            return None;
        };
        let at = arguments.iter().find_map(|arg| match arg {
            Expression::Block(block) => Some(block.span),
            _ => None,
        })?;
        Some((at, self.nlr_prestore_read(at)))
    }

    /// BT-4612 fault hook: makes `early` (taken by
    /// [`Self::fault_prestore_read_before_operands`]) the read the pre-store
    /// at its span stores.
    pub(in crate::core_erlang) fn publish_fault_prestore_read(
        early: Option<(Span, Option<PrestoreRead>)>,
    ) {
        if let Some((at, read)) = early {
            PRESTORE_EARLY_READ.with(|slot| slot.borrow_mut().insert(at, read));
        }
    }
}

/// Whether the current thread is in count-only mode (see [`count_only`]).
pub(in crate::core_erlang) fn is_count_only() -> bool {
    COUNT_ONLY.with(std::cell::Cell::get)
}
