// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Single integration-test binary for the `beamtalk-cli` subprocess test
//! suite (BT-2823). Consolidating what used to be 11 separate `tests/*.rs`
//! binaries into modules of one binary avoids re-linking the full
//! dependency graph (clap, miette, assert_cmd, ...) once per file, which
//! was the dominant contributor to `target/debug` size.
//!
//! `repl_protocol.rs` and `spec_validation.rs` stay as their own binaries:
//! both are invoked by name elsewhere (`cargo test --test repl_protocol`,
//! `--test spec_validation`) in the Justfile and docs.

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
