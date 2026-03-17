// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk workspace logs` — view workspace log files without a running REPL session.
//!
//! **DDD Context:** CLI
//!
//! A pure file-reading command that locates workspace log files under
//! `~/.beamtalk/workspaces/{id}/workspace.log` and displays them with
//! optional filtering and follow mode.

use std::fs::{self, File};
use std::io::{BufRead, BufReader, Seek, SeekFrom};
#[cfg(unix)]
use std::os::unix::fs::MetadataExt;
use std::path::PathBuf;
use std::thread;
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};

use super::workspace;

/// Output format for log display.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogFormat {
    Text,
    Json,
}

impl LogFormat {
    /// Parse a log format from a CLI argument string.
    pub fn from_str_arg(s: &str) -> Result<Self> {
        match s.to_lowercase().as_str() {
            "text" => Ok(Self::Text),
            "json" => Ok(Self::Json),
            _ => Err(miette!(
                "Unknown log format '{s}'. Valid formats: text, json"
            )),
        }
    }
}

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

/// Run the `beamtalk workspace logs` command.
pub fn run(
    workspace_id: Option<&str>,
    follow: bool,
    level: Option<&str>,
    format: Option<&str>,
    path_only: bool,
) -> Result<()> {
    let min_level = level.map(LogLevel::from_str_arg).transpose()?;
    let log_format = format.map(LogFormat::from_str_arg).transpose()?;

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
        tail_follow(&log_path, min_level, log_format)
    } else {
        show_last_lines(&log_path, 50, min_level, log_format)
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
                 Specify a workspace: `beamtalk workspace logs --workspace <id>`"
            ),
        });
    }

    let ws_dir = workspace::storage::workspace_dir(&ws_id)?;
    Ok(ws_dir.join("workspace.log"))
}

/// Show the last `n` lines of the log file, optionally filtered by level.
fn show_last_lines(
    path: &PathBuf,
    n: usize,
    min_level: Option<LogLevel>,
    format: Option<LogFormat>,
) -> Result<()> {
    let content = fs::read_to_string(path).into_diagnostic()?;
    let lines: Vec<&str> = content.lines().collect();

    let filtered: Vec<&str> = if let Some(min) = min_level {
        lines
            .into_iter()
            .filter(|line| matches_level(line, min, format))
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
///
/// When `format` is `Json`, attempts to extract the `"level"` field from
/// the JSON object for filtering. Falls back to text-based detection.
fn matches_level(line: &str, min: LogLevel, format: Option<LogFormat>) -> bool {
    if format == Some(LogFormat::Json) {
        if let Some(level) = extract_json_level(line) {
            return level >= min;
        }
    }
    match LogLevel::from_log_line(line) {
        Some(line_level) => line_level >= min,
        None => true, // show continuation lines / lines without a level
    }
}

/// Extract the log level from a JSON log line by looking for `"level":"..."`.
///
/// Uses a simple substring search to avoid pulling in a JSON parser on the
/// CLI side.
fn extract_json_level(line: &str) -> Option<LogLevel> {
    // Look for "level":"<value>" pattern
    let marker = "\"level\":\"";
    let start = line.find(marker)? + marker.len();
    let end = line[start..].find('"')? + start;
    let level_str = &line[start..end];
    match level_str {
        "debug" => Some(LogLevel::Debug),
        "info" => Some(LogLevel::Info),
        "notice" => Some(LogLevel::Notice),
        "warning" => Some(LogLevel::Warning),
        "error" => Some(LogLevel::Error),
        _ => None,
    }
}

/// Return the inode of an open file (Unix) or 0 (non-Unix).
#[cfg(unix)]
fn file_inode(file: &File) -> u64 {
    file.metadata().map(|m| m.ino()).unwrap_or(0)
}

#[cfg(not(unix))]
fn file_inode(_file: &File) -> u64 {
    0
}

/// Return the inode of a path (Unix) or 0 (non-Unix).
#[cfg(unix)]
fn path_inode(path: &PathBuf) -> u64 {
    fs::metadata(path).map(|m| m.ino()).unwrap_or(0)
}

#[cfg(not(unix))]
fn path_inode(_path: &PathBuf) -> u64 {
    0
}

/// Follow the log file, streaming new lines as they appear (poll-based).
///
/// Detects log rotation (OTP `logger_std_h` renames the file and creates a
/// new one) by comparing inodes. When rotation is detected, re-opens the
/// new file from the beginning so no log lines are lost.
fn tail_follow(
    path: &PathBuf,
    min_level: Option<LogLevel>,
    format: Option<LogFormat>,
) -> Result<()> {
    let file = File::open(path).into_diagnostic()?;
    let mut reader = BufReader::new(file);
    let mut current_inode = file_inode(reader.get_ref());

    // Seek to end — we only want new content
    reader.seek(SeekFrom::End(0)).into_diagnostic()?;

    let mut line = String::new();
    let mut stale_polls: u32 = 0;
    loop {
        line.clear();
        match reader.read_line(&mut line) {
            Ok(0) => {
                // No new data — check for rotation every ~1s (5 polls)
                stale_polls += 1;
                if stale_polls >= 5 {
                    stale_polls = 0;
                    let disk_inode = path_inode(path);
                    if disk_inode != 0 && disk_inode != current_inode {
                        // File was rotated — drain remaining lines from old file,
                        // then reopen the new file from the start
                        loop {
                            line.clear();
                            match reader.read_line(&mut line) {
                                Ok(0) | Err(_) => break,
                                Ok(_) => print_if_matches(&line, min_level, format),
                            }
                        }
                        let new_file = File::open(path).into_diagnostic()?;
                        current_inode = file_inode(&new_file);
                        reader = BufReader::new(new_file);
                        continue;
                    }
                }
                thread::sleep(Duration::from_millis(200));
            }
            Ok(_) => {
                stale_polls = 0;
                print_if_matches(&line, min_level, format);
            }
            Err(e) => {
                return Err(miette!("Error reading log file: {e}"));
            }
        }
    }
}

/// Print a log line if it passes the level filter.
fn print_if_matches(line: &str, min_level: Option<LogLevel>, format: Option<LogFormat>) {
    let trimmed = line.trim_end_matches('\n').trim_end_matches('\r');
    if let Some(min) = min_level {
        if matches_level(trimmed, min, format) {
            println!("{trimmed}");
        }
    } else {
        println!("{trimmed}");
    }
}
