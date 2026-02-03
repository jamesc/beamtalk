// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk compiler command-line interface.
//!
//! This is the main entry point for the `beamtalk` command.

use clap::{Parser, Subcommand};
use miette::Result;

pub mod beam_compiler;
mod commands;
mod diagnostic;
mod paths;

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

    /// Compile and run a Beamtalk program
    Run {
        /// Source file or directory to compile and run
        #[arg(default_value = ".")]
        path: String,
    },

    /// Create a new Beamtalk project
    New {
        /// Name of the project to create
        name: String,
    },

    /// Start an interactive REPL
    Repl {
        /// Port for the REPL backend (default: 49152, or `BEAMTALK_REPL_PORT` env var)
        #[arg(long)]
        port: Option<u16>,

        /// Node name for Erlang distribution (default: `BEAMTALK_NODE_NAME` env var)
        #[arg(long)]
        node: Option<String>,
    },

    /// Check source files for errors without compiling
    Check {
        /// Source file or directory to check
        #[arg(default_value = ".")]
        path: String,
    },

    /// Manage the compiler daemon
    Daemon {
        #[command(subcommand)]
        action: commands::daemon::DaemonAction,
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

    let result = match cli.command {
        Command::Build { path } => commands::build::build(&path),
        Command::Run { path } => commands::run::run(&path),
        Command::New { name } => commands::new::new_project(&name),
        Command::Repl { port, node } => commands::repl::run(port, node.clone()),
        Command::Check { path } => {
            println!("Checking: {path}");
            println!("(Not yet implemented)");
            Ok(())
        }
        Command::Daemon { action } => commands::daemon::run(action),
    };

    // Exit with appropriate code
    match result {
        Ok(()) => std::process::exit(0),
        Err(e) => {
            eprintln!("{e:?}");
            std::process::exit(1);
        }
    }
}
