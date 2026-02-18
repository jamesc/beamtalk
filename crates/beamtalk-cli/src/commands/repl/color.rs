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
/// Made `pub(crate)` for test access in sibling modules.
pub(crate) static COLOR_ENABLED: AtomicBool = AtomicBool::new(true);

/// Initialize color support based on CLI flag and environment.
///
/// Color is disabled if:
/// - `no_color_flag` is true (`--no-color` CLI argument)
/// - `NO_COLOR` environment variable is set (per <https://no-color.org/>)
/// - `TERM` environment variable is `dumb` (minimal terminal)
/// - stdout is not a terminal (piped output)
/// - stderr is not a terminal (error output is piped)
pub fn init(no_color_flag: bool) {
    let is_dumb_term = std::env::var("TERM").map(|v| v == "dumb").unwrap_or(false);
    let enabled = !no_color_flag
        && std::env::var_os("NO_COLOR").is_none()
        && !is_dumb_term
        && std::io::IsTerminal::is_terminal(&std::io::stdout())
        && std::io::IsTerminal::is_terminal(&std::io::stderr());
    COLOR_ENABLED.store(enabled, Ordering::Relaxed);
}

/// Returns whether color output is currently enabled.
#[must_use]
pub fn is_enabled() -> bool {
    COLOR_ENABLED.load(Ordering::Relaxed)
}

// ANSI color codes

/// ANSI escape sequence to reset all text attributes.
pub const RESET: &str = "\x1b[0m";
/// ANSI escape sequence for bold text.
pub const BOLD: &str = "\x1b[1m";
/// ANSI escape sequence for dim (faint) text.
pub const DIM: &str = "\x1b[2m";

/// ANSI escape sequence for red foreground text.
pub const RED: &str = "\x1b[31m";
/// ANSI escape sequence for green foreground text.
pub const GREEN: &str = "\x1b[32m";
/// ANSI escape sequence for yellow foreground text.
pub const YELLOW: &str = "\x1b[33m";
/// ANSI escape sequence for magenta foreground text.
pub const MAGENTA: &str = "\x1b[35m";
/// ANSI escape sequence for cyan foreground text.
pub const CYAN: &str = "\x1b[36m";
/// ANSI escape sequence for gray (bright black) foreground text.
pub const GRAY: &str = "\x1b[90m";

/// ANSI escape sequence for bold blue foreground text.
pub const BOLD_BLUE: &str = "\x1b[1;34m";
/// ANSI escape sequence for bold red foreground text.
pub const BOLD_RED: &str = "\x1b[1;31m";
/// ANSI escape sequence for bold cyan foreground text.
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
    use serial_test::serial;

    /// RAII guard that saves and restores `COLOR_ENABLED` on drop.
    struct ColorGuard {
        prev: bool,
    }

    impl ColorGuard {
        fn disabled() -> Self {
            let prev = COLOR_ENABLED.load(Ordering::Relaxed);
            COLOR_ENABLED.store(false, Ordering::Relaxed);
            Self { prev }
        }

        fn enabled() -> Self {
            let prev = COLOR_ENABLED.load(Ordering::Relaxed);
            COLOR_ENABLED.store(true, Ordering::Relaxed);
            Self { prev }
        }
    }

    impl Drop for ColorGuard {
        fn drop(&mut self) {
            COLOR_ENABLED.store(self.prev, Ordering::Relaxed);
        }
    }

    #[test]
    #[serial(color)]
    fn paint_with_color_disabled() {
        let _guard = ColorGuard::disabled();
        assert_eq!(paint(RED, "hello"), "hello");
    }

    #[test]
    #[serial(color)]
    fn paint_with_color_enabled() {
        let _guard = ColorGuard::enabled();
        assert_eq!(paint(RED, "hello"), "\x1b[31mhello\x1b[0m");
    }
}
