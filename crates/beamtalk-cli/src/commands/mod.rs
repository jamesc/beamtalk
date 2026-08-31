// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI command implementations.

/// Output format shared by commands that support `--format text|json`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputFormat {
    /// Human-readable text output (default).
    #[default]
    Text,
    /// Machine-readable JSON output.
    Json,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "text" => Ok(Self::Text),
            "json" => Ok(Self::Json),
            other => Err(format!(
                "unknown format '{other}': expected 'text' or 'json'"
            )),
        }
    }
}

pub mod app_file;
pub mod attach;
pub mod beam_environment;
pub mod build;
pub(crate) mod build_cache;
// BT-2823: `build_layout` moved to the `beamtalk_cli` lib crate so
// `beamtalk-mcp` can share it for offline dependency-class resolution.
// Re-exported here so existing `crate::commands::build_layout` /
// `super::build_layout` references throughout this module tree keep working
// unchanged.
pub use beamtalk_cli::build_layout;
pub(crate) mod build_stamp;
pub mod build_stdlib;
pub mod clean;
pub mod deps;
pub mod doc;
pub(crate) mod doc_tests;
pub mod doctor;
pub(crate) mod erlang_eval;
pub(crate) mod erlang_lint;
pub(crate) mod erlfmt;
pub mod escript;
pub mod fmt;
pub mod generate;
pub mod lint;
pub mod logs;
// BT-2823: `manifest` moved to the `beamtalk_cli` lib crate for the same
// reason as `build_layout` above.
pub use beamtalk_cli::manifest;
pub mod new;
pub mod protocol;
pub mod publish;
pub mod registry;
pub mod repl;
pub mod run;
pub mod test;
pub mod test_docs;
pub mod test_metamorphic;
pub mod test_stdlib;
#[cfg(test)]
pub(crate) mod test_support;
pub(crate) mod toml_utils;
pub mod transcript;
pub mod type_coverage;
pub(crate) mod util;
pub mod version;
pub mod warm_otp_cache;
pub mod workspace;
