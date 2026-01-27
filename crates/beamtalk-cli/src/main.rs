// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk compiler command-line interface.
//!
//! This is the main entry point for the `beamtalk` command.

use clap::{Parser, Subcommand};
use miette::Result;

/// Beamtalk: A Smalltalk-inspired language for the BEAM VM
#[derive(Debug, Parser)]
#[command(name = "beamtalk")]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Compile Beamtalk source files to BEAM bytecode
    Build {
        /// Source file or directory to compile
        #[arg(default_value = ".")]
        path: String,
    },

    /// Start an interactive REPL
    Repl,

    /// Check source files for errors without compiling
    Check {
        /// Source file or directory to check
        #[arg(default_value = ".")]
        path: String,
    },
}

fn main() -> Result<()> {
    // Install miette's fancy error handler
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(true)
                .context_lines(3)
                .build(),
        )
    }))?;

    let cli = Cli::parse();

    match cli.command {
        Command::Build { path } => {
            println!("Building: {path}");
            println!("(Not yet implemented)");
        }
        Command::Repl => {
            println!("Beamtalk REPL");
            println!("(Not yet implemented)");
        }
        Command::Check { path } => {
            println!("Checking: {path}");
            println!("(Not yet implemented)");
        }
    }

    Ok(())
}
