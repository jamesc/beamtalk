// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `unparse` tests, split by feature per BT-3450 (mirroring the
//! `type_checker/tests/` layout from BT-2061).
//!
//! Shared fixtures live in `common`. Each feature module is kept well under
//! the ~2000-line guideline in `rust-guidelines.md` § Testing so parallel PRs
//! adding tests rarely conflict on the same region.

mod common;

mod corpus_conformance_tests;
mod declaration_order_and_blank_lines;
mod format_source_and_comments;
mod keyword_and_protocol_round_trip;
mod property_tests;
mod reindent_and_literal_display;
mod round_trip_and_idempotency;
mod statements_and_class_formatting;
