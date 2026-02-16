// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk compiler command-line interface.
//!
//! This is the main entry point for the `beamtalk` command.

use clap::{Parser, Subcommand};
use miette::Result;

/// BEAM bytecode compiler integration (Core Erlang → `.beam`).
pub mod beam_compiler;
/// CLI subcommand implementations (build, repl, test, etc.).
mod commands;
/// Diagnostic formatting for compiler errors and warnings.
mod diagnostic;
/// Shared path utilities (`~/.beamtalk/` and workspace directories).
mod paths;

/// Beamtalk: A Smalltalk-inspired language for the BEAM VM
#[derive(Debug, Parser)]
#[command(name = "beamtalk")]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

/// Available CLI subcommands.
#[derive(Debug, Subcommand)]
enum Command {
    /// Compile Beamtalk source files to BEAM bytecode
    Build {
        /// Source file or directory to compile
        #[arg(default_value = ".")]
        path: String,

        /// Allow @primitive pragmas in non-stdlib code (advanced FFI use)
        #[arg(long)]
        allow_primitives: bool,

        /// Compile in stdlib mode (enables @primitive without warnings)
        #[arg(long)]
        stdlib_mode: bool,
    },

    /// Compile the standard library (`lib/*.bt` → `runtime/apps/beamtalk_stdlib/ebin/`)
    BuildStdlib,

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
        /// Port for the REPL backend (default: 0 = OS-assigned, or `BEAMTALK_REPL_PORT` env var)
        #[arg(long)]
        port: Option<u16>,

        /// Node name for Erlang distribution (default: `BEAMTALK_NODE_NAME` env var)
        #[arg(long)]
        node: Option<String>,

        /// Start node in foreground instead of detached workspace (for debugging)
        #[arg(long)]
        foreground: bool,

        /// Explicit workspace name (default: auto-detect from current directory)
        #[arg(long)]
        workspace: Option<String>,

        /// Disable auto-cleanup (workspace persists even when idle)
        #[arg(long)]
        persistent: bool,

        /// Maximum idle timeout in seconds before auto-cleanup (default: 14400 = 4 hours)
        /// Can also be set via `BEAMTALK_WORKSPACE_TIMEOUT` environment variable
        #[arg(long)]
        timeout: Option<u64>,

        /// Disable colored output (also respects `NO_COLOR` environment variable)
        #[arg(long)]
        no_color: bool,
    },

    /// Check source files for errors without compiling
    Check {
        /// Source file or directory to check
        #[arg(default_value = ".")]
        path: String,
    },

    /// Stream Transcript output from a running workspace
    Transcript {
        /// Explicit workspace name (default: auto-detect from current directory)
        #[arg(long)]
        workspace: Option<String>,

        /// Display last N entries from ring buffer on connect
        #[arg(long)]
        recent: Option<usize>,
    },

    /// Manage workspaces (list, stop, status, create)
    Workspace {
        #[command(subcommand)]
        action: commands::workspace::cli::WorkspaceCommand,
    },

    /// Run compiled stdlib tests (ADR 0014 Phase 1)
    TestStdlib {
        /// Directory containing .bt test files
        #[arg(default_value = "tests/stdlib")]
        path: String,
    },

    /// Run `BUnit` tests — discover and run `TestCase` subclasses (ADR 0014 Phase 2)
    Test {
        /// Test file or directory containing .bt test files
        #[arg(default_value = "test")]
        path: String,
    },

    /// Generate HTML API documentation from source files (ADR 0008)
    Doc {
        /// Source file or directory containing .bt files
        #[arg(default_value = "lib")]
        path: String,

        /// Output directory for generated HTML
        #[arg(long, default_value = "docs/api")]
        output: String,
    },
}

/// CLI entry point: parse arguments and dispatch to the appropriate subcommand.
fn main() -> Result<()> {
    // Initialize tracing subscriber only if RUST_LOG is explicitly set
    // This avoids stderr interference with E2E tests
    if std::env::var("RUST_LOG").is_ok() {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(
                tracing_subscriber::EnvFilter::try_from_default_env()
                    .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("warn")),
            )
            .try_init();
    }

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
        Command::Build {
            path,
            allow_primitives,
            stdlib_mode,
        } => {
            let options = beamtalk_core::CompilerOptions {
                stdlib_mode,
                allow_primitives,
                workspace_mode: false,
            };
            commands::build::build(&path, &options)
        }
        Command::BuildStdlib => commands::build_stdlib::build_stdlib(),
        Command::Run { path } => commands::run::run(&path),
        Command::New { name } => commands::new::new_project(&name),
        Command::Repl {
            port,
            node,
            foreground,
            workspace,
            persistent,
            timeout,
            no_color,
        } => commands::repl::run(
            port,
            node,
            foreground,
            workspace.as_deref(),
            persistent,
            timeout,
            no_color,
        ),
        Command::Transcript { workspace, recent } => {
            commands::transcript::run(workspace.as_deref(), recent)
        }
        Command::Check { path } => {
            println!("Checking: {path}");
            println!("(Not yet implemented)");
            Ok(())
        }
        Command::Workspace { action } => commands::workspace::cli::run(action),
        Command::TestStdlib { path } => commands::test_stdlib::run_tests(&path),
        Command::Test { path } => commands::test::run_tests(&path),
        Command::Doc { path, output } => commands::doc::run(&path, &output),
    };

    // Exit with appropriate code
    match result {
        Ok(()) => std::process::exit(0),
        Err(e) => {
            // miette already provides nice error formatting, just display it
            eprintln!("{e:?}");
            std::process::exit(1);
        }
    }
}
