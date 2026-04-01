// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI command implementations.

pub mod app_file;
pub mod attach;
pub mod beam_environment;
pub mod build;
pub(crate) mod build_cache;
pub mod build_layout;
pub mod build_stdlib;
pub mod deps;
pub mod doc;
pub(crate) mod doc_tests;
pub mod doctor;
pub mod fmt;
pub mod lint;
pub mod logs;
pub mod manifest;
pub mod new;
pub mod protocol;
pub mod repl;
pub mod run;
pub mod test;
pub mod test_docs;
pub mod test_stdlib;
pub mod transcript;
pub(crate) mod util;
pub mod workspace;
