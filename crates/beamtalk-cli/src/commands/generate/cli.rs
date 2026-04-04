// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI interface for code generation commands (ADR 0075 Phase 3).
//!
//! **DDD Context:** Compilation
//!
//! Provides `beamtalk generate {native, stubs}` subcommands for generating
//! Erlang boilerplate from Beamtalk source files.

use clap::Subcommand;
use miette::Result;

/// Code generation subcommands.
#[derive(Debug, Subcommand)]
pub enum GenerateCommand {
    /// Generate a skeleton Erlang `gen_server` from a `native:` Actor class
    ///
    /// Reads `<ClassName>.bt`, extracts `native:` declaration and `self delegate`
    /// methods, and writes a skeleton `.erl` file with matching `handle_call/3` clauses.
    Native {
        /// Class name (e.g., `MyActor`) -- looks for `MyActor.bt`, `src/MyActor.bt`, or `lib/MyActor.bt`
        class_name: String,
    },

    /// Generate Beamtalk stub definitions from `.beam` abstract code (ADR 0075)
    ///
    /// Reads `-spec` attributes and parameter names from compiled `.beam` files
    /// and generates `.bt` stub files with `declare native:` forms.
    ///
    /// Examples:
    ///   beamtalk generate stubs lists maps string
    ///   beamtalk generate stubs --native-dir native/
    ///   beamtalk generate stubs lists -o `my_stubs`/
    Stubs {
        /// OTP module names to generate stubs for (e.g., `lists maps string`)
        modules: Vec<String>,

        /// Directory containing native `.beam` files to generate stubs for
        #[arg(long)]
        native_dir: Option<String>,

        /// Output directory for generated `.bt` stubs (default: `stubs/`)
        #[arg(short, long, default_value = "stubs")]
        output: String,
    },
}

/// Dispatch to the appropriate generate subcommand.
pub fn run(command: GenerateCommand) -> Result<()> {
    match command {
        GenerateCommand::Native { class_name } => super::native::run(&class_name),
        GenerateCommand::Stubs {
            modules,
            native_dir,
            output,
        } => super::stubs::run(&modules, native_dir.as_deref(), &output),
    }
}
