// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ANSI color support for the Beamtalk REPL.
//!
//! **DDD Context:** REPL — Presentation
//!
//! Provides ANSI color codes and a global color-enabled flag that respects
//! the `NO_COLOR` environment variable and `--no-color` CLI flag.

use std::sync::atomic::{AtomicBool, Ordering};

/// Global flag controlling whether color output is enabled.
/// Made `pub(super)` for test access in sibling modules.
pub(super) static COLOR_ENABLED: AtomicBool = AtomicBool::new(true);

/// Initialize color support based on CLI flag and environment.
///
/// Color is disabled if:
/// - `no_color_flag` is true (`--no-color` CLI argument)
/// - `NO_COLOR` environment variable is set (per <https://no-color.org/>)
/// - stdout is not a terminal (piped output)
pub fn init(no_color_flag: bool) {
    let enabled = !no_color_flag
        && std::env::var_os("NO_COLOR").is_none()
        && std::io::IsTerminal::is_terminal(&std::io::stdout());
    COLOR_ENABLED.store(enabled, Ordering::Relaxed);
}

/// Returns whether color output is currently enabled.
#[must_use]
pub fn is_enabled() -> bool {
    COLOR_ENABLED.load(Ordering::Relaxed)
}

// ANSI color codes
pub const RESET: &str = "\x1b[0m";
pub const BOLD: &str = "\x1b[1m";
pub const DIM: &str = "\x1b[2m";

pub const RED: &str = "\x1b[31m";
pub const GREEN: &str = "\x1b[32m";
pub const YELLOW: &str = "\x1b[33m";
pub const MAGENTA: &str = "\x1b[35m";
pub const CYAN: &str = "\x1b[36m";
pub const GRAY: &str = "\x1b[90m";

pub const BOLD_BLUE: &str = "\x1b[1;34m";
pub const BOLD_RED: &str = "\x1b[1;31m";
pub const BOLD_CYAN: &str = "\x1b[1;36m";

/// Wrap text with color codes if color is enabled.
#[must_use]
pub fn paint(color: &str, text: &str) -> String {
    if is_enabled() {
        format!("{color}{text}{RESET}")
    } else {
        text.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paint_with_color_disabled() {
        COLOR_ENABLED.store(false, Ordering::Relaxed);
        assert_eq!(paint(RED, "hello"), "hello");
        COLOR_ENABLED.store(true, Ordering::Relaxed);
    }

    #[test]
    fn paint_with_color_enabled() {
        COLOR_ENABLED.store(true, Ordering::Relaxed);
        assert_eq!(paint(RED, "hello"), "\x1b[31mhello\x1b[0m");
    }
}
