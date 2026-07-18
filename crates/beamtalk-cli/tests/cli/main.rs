// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Single integration-test binary for the `beamtalk-cli` subprocess test
//! suite (BT-2823). Consolidating what used to be 11 separate `tests/*.rs`
//! binaries into modules of one binary avoids re-linking the full
//! dependency graph (clap, miette, `assert_cmd`, ...) once per file, which
//! was the dominant contributor to `target/debug` size.
//!
//! `repl_protocol.rs` and `spec_validation.rs` stay as their own binaries:
//! `repl_protocol` is invoked by name in the Justfile; `spec_validation`
//! is referenced in `docs/development/common-tasks.md`.
//!
//! Unlike before, these modules' tests now run as threads of one process
//! rather than as separate processes, so a test must never mutate
//! process-global state (`std::env::set_var`, `std::env::set_current_dir`)
//! directly — use `Command::env`/`Command::current_dir` on the child
//! `beamtalk` process instead, as every existing test here already does.

#[path = "../cli_common/mod.rs"]
mod cli_common;

mod cli_build;
mod cli_doc;
mod cli_doctor;
mod cli_fmt;
mod cli_lint;
mod cli_new;
mod cli_run;
mod cli_test;
mod cli_transcript;
mod gen_native;
mod native_type_extraction;
