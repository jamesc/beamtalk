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

/// The warning-handling flags shared by every command that compiles source
/// (`build`, `check`, `test`, `build-stdlib` and the hidden test commands).
///
/// One definition, flattened into each command, so the spelling, help text and
/// the `--no-warnings` / `--warnings-as-errors` exclusion cannot drift
/// (BT-5045). `lint` keeps its own `--warnings-as-errors`: it gates on a
/// different diagnostic set.
#[derive(Debug, Clone, Copy, Default, clap::Args)]
pub struct WarningFlags {
    /// Suppress warning diagnostics
    #[arg(long, conflicts_with = "warnings_as_errors")]
    pub no_warnings: bool,

    /// Treat warnings and hints as errors: fail if any are emitted
    #[arg(long)]
    pub warnings_as_errors: bool,
}

/// The flags shared by the hidden `.btscript` / doctest / metamorphic test
/// commands: the warning flags plus output control.
#[derive(Debug, Clone, Copy, Default, clap::Args)]
pub struct TestCommandFlags {
    #[command(flatten)]
    pub warnings: WarningFlags,

    /// Suppress per-file output, show only summary
    #[arg(long, short)]
    pub quiet: bool,

    /// Show detailed test output including `EUnit` verbose mode
    #[arg(long = "show-output")]
    pub show_output: bool,
}

impl From<TestCommandFlags> for test_stdlib::TestRunOptions {
    fn from(flags: TestCommandFlags) -> Self {
        Self {
            no_warnings: flags.warnings.no_warnings,
            warnings_as_errors: flags.warnings.warnings_as_errors,
            quiet: flags.quiet,
            verbose: flags.show_output,
        }
    }
}

impl WarningFlags {
    /// `CompilerOptions` carrying these flags (everything else default).
    pub fn compiler_options(self) -> beamtalk_core::CompilerOptions {
        beamtalk_core::CompilerOptions {
            suppress_warnings: self.no_warnings,
            warnings_as_errors: self.warnings_as_errors,
            ..Default::default()
        }
    }
}

pub mod app_file;
pub mod attach;
pub mod beam_environment;
pub mod build;
pub(crate) mod build_cache;
pub(crate) mod build_stamp;
pub mod build_stdlib;
pub mod check;
pub mod clean;
pub mod deps;
pub(crate) mod dir_lock;
pub mod doc;
pub(crate) mod doc_tests;
pub mod doctor;
pub(crate) mod erlang_eval;
pub(crate) mod erlfmt;
pub mod escript;
pub(crate) mod fixture;
pub mod fmt;
pub mod generate;
pub mod lint;
pub mod logs;
pub mod new;
pub mod protocol;
pub mod publish;
pub mod registry;
pub mod release;
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
