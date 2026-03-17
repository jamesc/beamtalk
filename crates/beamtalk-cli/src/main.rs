// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk compiler command-line interface.
//!
//! This is the main entry point for the `beamtalk` command.

use clap::{ArgAction, Parser, Subcommand};
use miette::Result;
use std::path::PathBuf;

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
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count, global = true)]
    verbose: u8,

    /// Print the installation prefix (sysroot) and exit
    #[arg(long)]
    print_sysroot: bool,

    #[command(subcommand)]
    command: Option<Command>,
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

        /// Suppress warning diagnostics
        #[arg(long)]
        no_warnings: bool,
    },

    /// Compile the standard library (`lib/*.bt` → `runtime/apps/beamtalk_stdlib/ebin/`)
    #[command(hide = true)]
    BuildStdlib {
        /// Suppress per-file compilation output
        #[arg(long, short)]
        quiet: bool,
    },

    /// Compile and run a Beamtalk program
    ///
    /// Script mode: `beamtalk run ClassName selector` — starts a workspace in run mode
    /// (no REPL server), calls ClassName>>selector, exits when it returns.
    ///
    /// Service mode: `beamtalk run .` — starts a persistent workspace with the
    /// package's `[application]` supervisor and returns the shell (service keeps running).
    Run {
        /// Class name for script mode (e.g. `Main`), or `.` for service mode
        #[arg(default_value = ".")]
        class_or_dot: String,

        /// Selector to call in script mode (e.g. `run`)
        selector: Option<String>,
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

        /// Request automatic cleanup of detached workspace on REPL exit (ephemeral mode).
        /// In detached workspace mode (default, without --foreground), attempts to stop the associated BEAM node when the REPL session ends; failures are logged to stderr.
        #[arg(long, short = 'e')]
        ephemeral: bool,

        /// Maximum idle timeout in seconds before auto-cleanup (default: 14400 = 4 hours)
        /// Can also be set via `BEAMTALK_WORKSPACE_TIMEOUT` environment variable
        #[arg(long)]
        timeout: Option<u64>,

        /// Disable colored output (also respects `NO_COLOR` environment variable)
        #[arg(long)]
        no_color: bool,

        /// Bind address for the REPL WebSocket server.
        /// Use "tailscale" to auto-detect Tailscale interface,
        /// or an IP address like "192.168.1.5" or "0.0.0.0".
        /// Default: 127.0.0.1 (localhost only)
        #[arg(long)]
        bind: Option<String>,

        /// Suppress the safety warning when binding to a non-loopback address
        #[arg(long)]
        confirm_network: bool,

        /// Start browser workspace (serves HTML UI at `http://localhost:<port>/`)
        #[arg(long)]
        web: bool,

        /// Port for the browser workspace HTTP server (default: same as REPL WebSocket port).
        /// Only used with --web.
        #[arg(long, requires = "web")]
        web_port: Option<u16>,
    },

    /// Check source files for errors without compiling
    Check {
        /// Source file or directory to check
        #[arg(default_value = ".")]
        path: String,
    },

    /// Format Beamtalk source files in-place
    Fmt {
        /// Source files or directories to format
        #[arg(default_value = ".")]
        paths: Vec<String>,

        /// Use `beamtalk fmt-check` instead
        #[arg(long, hide = true)]
        check: bool,
    },

    /// Check Beamtalk source formatting without modifying files
    ///
    /// Prints a unified diff for every file that would change and exits
    /// non-zero if any files need reformatting.
    FmtCheck {
        /// Source files or directories to check
        #[arg(default_value = ".")]
        paths: Vec<String>,
    },

    /// View workspace log files
    ///
    /// Shows the most recent workspace's log file. Defaults to the last 50 lines.
    /// Use `--follow` to stream new lines, `--level` to filter by severity,
    /// or `--path` to print the log file path.
    Logs {
        /// Select a specific workspace by name or ID
        #[arg(long)]
        workspace: Option<String>,

        /// Stream new log lines as they appear (like `tail -f`)
        #[arg(long, short)]
        follow: bool,

        /// Filter by minimum severity level (debug, info, notice, warning, error)
        #[arg(long)]
        level: Option<String>,

        /// Print the log file path and exit
        #[arg(long)]
        path: bool,
    },

    /// Run style/redundancy lint checks on source files
    Lint {
        /// Source file or directory to lint
        #[arg(default_value = ".")]
        path: String,

        /// Output format for lint diagnostics
        #[arg(long, default_value = "text")]
        format: commands::lint::OutputFormat,
    },

    /// Manage workspaces (list, stop, status, attach, transcript, create)
    Workspace {
        #[command(subcommand)]
        action: commands::workspace::cli::WorkspaceCommand,
    },

    /// Run `.btscript` expression tests (assertions via `// =>`)
    #[command(name = "test-script", alias = "test-stdlib", hide = true)]
    TestScript {
        /// File or directory containing .btscript test files
        #[arg(default_value = "bootstrap-test")]
        path: String,

        /// Suppress warning diagnostics when compiling test fixtures
        #[arg(long)]
        no_warnings: bool,

        /// Suppress per-file output, show only summary
        #[arg(long, short)]
        quiet: bool,

        /// Show detailed test output including `EUnit` verbose mode
        #[arg(long = "show-output")]
        show_output: bool,
    },

    /// Run doctests extracted from Markdown files (` ```beamtalk ` blocks with `// =>` assertions)
    #[command(hide = true)]
    TestDocs {
        /// File or directory containing .md files
        #[arg(default_value = ".")]
        path: String,

        /// Suppress warning diagnostics when compiling test fixtures
        #[arg(long)]
        no_warnings: bool,

        /// Suppress per-file output, show only summary
        #[arg(long, short)]
        quiet: bool,

        /// Show detailed test output including `EUnit` verbose mode
        #[arg(long = "show-output")]
        show_output: bool,
    },

    /// Run `BUnit` tests — discover and run `TestCase` subclasses
    Test {
        /// Test file or directory containing .bt test files
        #[arg(default_value = "test")]
        path: String,

        /// Treat warnings and hints as errors — fail if any are emitted
        #[arg(long)]
        warnings_as_errors: bool,
    },

    /// Check environment setup (Erlang, runtime, stdlib)
    Doctor {
        /// Include developer toolchain checks (rebar3, just, rustc)
        #[arg(long)]
        dev: bool,
    },

    /// Generate HTML API documentation from source files
    Doc {
        /// Source file or directory containing .bt files
        #[arg(default_value = "src")]
        path: String,

        /// Output directory for generated HTML
        #[arg(long, default_value = "docs/api")]
        output: String,

        /// Build full documentation site (landing page + API docs + prose docs)
        #[arg(long)]
        site: bool,

        /// Directory containing prose documentation markdown files
        #[arg(long, default_value = "docs")]
        docs_path: String,
    },
}

/// The default stack size (1 MiB on Windows) is too small for deep AST recursion
/// when compiling large stdlib test suites. Spawn a thread with 8 MiB and run
/// everything there so every platform gets the same headroom.
const STACK_SIZE: usize = 8 * 1024 * 1024;

fn main() {
    let result = std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .expect("failed to spawn main thread")
        .join()
        .expect("main thread panicked");

    match result {
        Ok(()) => std::process::exit(0),
        Err(e) => {
            eprintln!("{e:?}");
            std::process::exit(1);
        }
    }
}

/// Returns the installation prefix by walking up from the running binary:
/// `{sysroot}/bin/beamtalk` → `{sysroot}`.
///
/// Emits a warning to stderr if the sysroot cannot be derived from the
/// running binary, falling back to `/usr/local`.
fn compute_sysroot() -> PathBuf {
    match std::env::current_exe() {
        Ok(exe) => {
            if let Some(sysroot) = exe.parent().and_then(|bin| bin.parent()) {
                return sysroot.to_path_buf();
            }
            eprintln!(
                "warning: unable to determine sysroot from '{}'; falling back to /usr/local",
                exe.display()
            );
        }
        Err(err) => {
            eprintln!("warning: failed to read executable path: {err}; falling back to /usr/local");
        }
    }
    PathBuf::from("/usr/local")
}

/// CLI entry point: parse arguments and dispatch to the appropriate subcommand.
#[expect(
    clippy::too_many_lines,
    reason = "top-level dispatch — each arm is a one-liner"
)]
fn run() -> Result<()> {
    let cli = Cli::parse();

    if cli.print_sysroot {
        println!("{}", compute_sysroot().display());
        return Ok(());
    }

    let Some(command) = cli.command else {
        miette::bail!("no subcommand provided. Run `beamtalk --help` for usage.");
    };

    // Initialize tracing when explicitly requested.
    // Default is still no logs to avoid stderr interference with E2E tests.
    let has_rust_log = std::env::var("RUST_LOG").is_ok();
    if has_rust_log || cli.verbose > 0 {
        let env_filter = if has_rust_log {
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("warn"))
        } else {
            // Target must match Rust module paths (`beamtalk_cli`, `beamtalk_core`).
            // `beamtalk=…` only matches `beamtalk::*`, not `beamtalk_cli`.
            let directive = if cli.verbose == 1 {
                "beamtalk_cli=debug,beamtalk_core=debug"
            } else {
                "beamtalk_cli=trace,beamtalk_core=trace"
            };
            tracing_subscriber::EnvFilter::new(directive)
        };

        let _ = tracing_subscriber::fmt()
            .with_env_filter(env_filter)
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

    match command {
        Command::Build {
            path,
            allow_primitives,
            stdlib_mode,
            no_warnings,
        } => {
            let options = beamtalk_core::CompilerOptions {
                stdlib_mode,
                allow_primitives,
                workspace_mode: false,
                suppress_warnings: no_warnings,
                ..Default::default()
            };
            commands::build::build(&path, &options)
        }
        Command::BuildStdlib { quiet } => commands::build_stdlib::build_stdlib(quiet),
        Command::Run {
            class_or_dot,
            selector,
        } => commands::run::run(&class_or_dot, selector.as_deref()),
        Command::New { name } => commands::new::new_project(&name),
        Command::Repl {
            port,
            node,
            foreground,
            workspace,
            persistent,
            ephemeral,
            timeout,
            no_color,
            bind,
            confirm_network,
            web,
            web_port,
        } => commands::repl::run(
            port,
            node,
            foreground,
            workspace.as_deref(),
            persistent,
            ephemeral,
            timeout,
            no_color,
            bind.as_deref(),
            confirm_network,
            web,
            web_port,
        ),
        Command::Check { path } => {
            println!("Checking: {path}");
            println!("(Not yet implemented)");
            Ok(())
        }
        Command::Fmt { paths, check } => {
            if check {
                miette::bail!(
                    "`beamtalk fmt --check` is not supported. Use `beamtalk fmt-check` instead."
                );
            }
            commands::fmt::run_fmt(&paths, false)
        }
        Command::FmtCheck { paths } => commands::fmt::run_fmt(&paths, true),
        Command::Logs {
            workspace,
            follow,
            level,
            path,
        } => commands::logs::run(workspace.as_deref(), follow, level.as_deref(), path),
        Command::Lint { path, format } => commands::lint::run_lint(&path, format),
        Command::Workspace { action } => commands::workspace::cli::run(action),
        Command::TestScript {
            path,
            no_warnings,
            quiet,
            show_output,
        } => commands::test_stdlib::run_tests(&path, no_warnings, quiet, show_output),
        Command::TestDocs {
            path,
            no_warnings,
            quiet,
            show_output,
        } => commands::test_docs::run_tests(&path, no_warnings, quiet, show_output),
        Command::Test {
            path,
            warnings_as_errors,
        } => commands::test::run_tests(&path, warnings_as_errors),
        Command::Doctor { dev } => commands::doctor::run(dev),
        Command::Doc {
            path,
            output,
            site,
            docs_path,
        } => {
            if site {
                commands::doc::run_site(&path, &docs_path, &output)
            } else {
                commands::doc::run(&path, &output)
            }
        }
    }
}
