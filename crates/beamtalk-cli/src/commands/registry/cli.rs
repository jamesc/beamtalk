// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI interface for registry-site commands (BT-2990).
//!
//! **DDD Context:** CLI / Documentation
//!
//! Distinct from `beamtalk deps` (consuming a registry dependency): these
//! commands render the registry index itself into a browsable static site.

use clap::Subcommand;
use miette::Result;

/// Registry-site subcommands.
#[derive(Debug, Subcommand)]
pub enum RegistryCommand {
    /// Render the registry index as a static HTML site (read-only, no server)
    ///
    /// Mirrors `beamtalk doc --site`: hand-rolled HTML/CSS, no templating
    /// crate, no server — just files written to `--output`. `--index` takes
    /// the same kind of value `BEAMTALK_REGISTRY`/`[registry] url` do: an
    /// existing local directory is read in place, anything else is treated
    /// as a git URL and cloned.
    Site {
        /// Registry index location — a local directory or a git URL
        #[arg(long)]
        index: String,

        /// Output directory for the generated site
        #[arg(long)]
        output: String,
    },
}

/// Run the given registry subcommand.
pub fn run(command: RegistryCommand) -> Result<()> {
    match command {
        RegistryCommand::Site { index, output } => super::run_site(&index, &output),
    }
}
