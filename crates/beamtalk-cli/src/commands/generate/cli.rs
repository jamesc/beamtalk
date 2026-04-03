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

    /// Generate Beamtalk stub definitions from an FFI spec file (placeholder)
    ///
    /// Reads an FFI type-definition file and generates `.bt` class stubs
    /// with method signatures and type annotations.
    Stubs {
        /// Path to the FFI spec file (e.g., `ffi/my_module.toml`)
        spec_path: String,

        /// Output directory for generated `.bt` stubs (default: `src/`)
        #[arg(long, default_value = "src")]
        output: String,
    },
}

/// Dispatch to the appropriate generate subcommand.
pub fn run(command: GenerateCommand) -> Result<()> {
    match command {
        GenerateCommand::Native { class_name } => super::native::run(&class_name),
        GenerateCommand::Stubs { spec_path, output } => super::stubs::run(&spec_path, &output),
    }
}
