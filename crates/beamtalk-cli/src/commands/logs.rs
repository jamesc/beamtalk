// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk logs` — view workspace log files without a running REPL session.
//!
//! **DDD Context:** CLI
//!
//! A pure file-reading command that locates workspace log files under
//! `~/.beamtalk/workspaces/{id}/workspace.log` and displays them with
//! optional filtering and follow mode.

use std::fs::{self, File};
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::path::PathBuf;
use std::thread;
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};

use super::workspace;

/// Minimum severity levels for log filtering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Debug,
    Info,
    Notice,
    Warning,
    Error,
}

impl LogLevel {
    /// Parse a log level from a CLI argument string.
    pub fn from_str_arg(s: &str) -> Result<Self> {
        match s.to_lowercase().as_str() {
            "debug" => Ok(Self::Debug),
            "info" => Ok(Self::Info),
            "notice" => Ok(Self::Notice),
            "warning" | "warn" => Ok(Self::Warning),
            "error" => Ok(Self::Error),
            _ => Err(miette!(
                "Unknown log level '{s}'. Valid levels: debug, info, notice, warning, error"
            )),
        }
    }

    /// Parse the level from a log line by looking for `[level]` patterns.
    fn from_log_line(line: &str) -> Option<Self> {
        let lower = line.to_lowercase();
        if lower.contains("[debug]") {
            Some(Self::Debug)
        } else if lower.contains("[info]") {
            Some(Self::Info)
        } else if lower.contains("[notice]") {
            Some(Self::Notice)
        } else if lower.contains("[warning]") || lower.contains("[warn]") {
            Some(Self::Warning)
        } else if lower.contains("[error]") {
            Some(Self::Error)
        } else {
            None
        }
    }
}

/// Run the `beamtalk logs` command.
pub fn run(
    workspace_id: Option<&str>,
    follow: bool,
    level: Option<&str>,
    path_only: bool,
) -> Result<()> {
    let min_level = level.map(LogLevel::from_str_arg).transpose()?;

    let log_path = resolve_log_path(workspace_id)?;

    if path_only {
        println!("{}", log_path.display());
        return Ok(());
    }

    if !log_path.exists() {
        return Err(miette!(
            "No log file found at {}\n\
             Hint: the workspace may not have generated any logs yet.",
            log_path.display()
        ));
    }

    if follow {
        tail_follow(&log_path, min_level)
    } else {
        show_last_lines(&log_path, 50, min_level)
    }
}

/// Resolve the log file path for a workspace.
///
/// If `workspace_id` is provided, uses that workspace directly.
/// Otherwise, resolves the workspace from the current directory
/// (same logic as `beamtalk attach`).
fn resolve_log_path(workspace_id: Option<&str>) -> Result<PathBuf> {
    let ws_id = workspace::lifecycle::resolve_workspace_id_or_cwd(workspace_id)?;

    if !workspace::workspace_exists(&ws_id)? {
        return Err(match workspace_id {
            Some(name) => miette!(
                "Workspace '{name}' does not exist. \
                 Use `beamtalk workspace list` to see available workspaces."
            ),
            None => miette!(
                "No workspace found for current directory. \
                 Specify a workspace: `beamtalk logs --workspace <id>`"
            ),
        });
    }

    let ws_dir = workspace::storage::workspace_dir(&ws_id)?;
    Ok(ws_dir.join("workspace.log"))
}

/// Show the last `n` lines of the log file, optionally filtered by level.
fn show_last_lines(path: &PathBuf, n: usize, min_level: Option<LogLevel>) -> Result<()> {
    let content = fs::read_to_string(path).into_diagnostic()?;
    let lines: Vec<&str> = content.lines().collect();

    let filtered: Vec<&str> = if let Some(min) = min_level {
        lines
            .into_iter()
            .filter(|line| matches_level(line, min))
            .collect()
    } else {
        lines
    };

    let start = filtered.len().saturating_sub(n);
    for line in &filtered[start..] {
        println!("{line}");
    }

    Ok(())
}

/// Check whether a log line matches the minimum severity level.
///
/// Lines without a recognized level marker are always shown (they may be
/// continuation lines from a multi-line log entry).
fn matches_level(line: &str, min: LogLevel) -> bool {
    match LogLevel::from_log_line(line) {
        Some(line_level) => line_level >= min,
        None => true, // show continuation lines / lines without a level
    }
}

/// Follow the log file, streaming new lines as they appear (poll-based).
fn tail_follow(path: &PathBuf, min_level: Option<LogLevel>) -> Result<()> {
    let file = File::open(path).into_diagnostic()?;
    let mut reader = BufReader::new(file);

    // Seek to end — we only want new content
    reader.seek(SeekFrom::End(0)).into_diagnostic()?;

    let mut line = String::new();
    loop {
        line.clear();
        match reader.read_line(&mut line) {
            Ok(0) => {
                // No new data — poll again after a short sleep
                thread::sleep(Duration::from_millis(200));
            }
            Ok(_) => {
                let trimmed = line.trim_end_matches('\n').trim_end_matches('\r');
                if let Some(min) = min_level {
                    if matches_level(trimmed, min) {
                        println!("{trimmed}");
                    }
                } else {
                    println!("{trimmed}");
                }
            }
            Err(e) => {
                return Err(miette!("Error reading log file: {e}"));
            }
        }
    }
}
