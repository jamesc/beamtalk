// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package name validation (ADR 0026 §8).
//!
//! **DDD Context:** Build System
//!
//! Shared by `beamtalk-cli`'s manifest parsing and by the `[dependencies]`
//! parsing in [`crate::dependencies`], which every tool that reads a
//! manifest's dependency table (CLI, MCP, LSP) uses.

use std::fmt;

/// Format a package name validation error with an optional suggestion.
pub fn format_name_error(name: &str, error: &PackageNameError) -> String {
    let msg = format!("Invalid package name '{name}': {error}");
    match suggest_package_name(name) {
        Some(s) => format!("{msg} (try '{s}')"),
        None => msg,
    }
}

/// Reserved beamtalk package names that conflict with core components.
pub const RESERVED_NAMES: &[&str] = &[
    "beamtalk",
    "stdlib",
    "kernel",
    "runtime",
    "workspace",
    "compiler",
];

/// Erlang standard application names that would conflict with OTP.
pub const ERLANG_APPS: &[&str] = &[
    "common_test",
    "crypto",
    "dialyzer",
    "eunit",
    "inets",
    "mnesia",
    "observer",
    "public_key",
    "sasl",
    "ssh",
    "ssl",
    "tools",
    "xmerl",
];

/// Error returned when a package name is invalid.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageNameError {
    Empty,
    TooLong(usize),
    StartsWithNonLetter(char),
    InvalidCharacter(char),
    ReservedName,
    ErlangAppConflict,
}

impl fmt::Display for PackageNameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "package name must not be empty"),
            Self::TooLong(len) => {
                write!(f, "package name is too long ({len} chars, max 64)")
            }
            Self::StartsWithNonLetter(c) => {
                write!(
                    f,
                    "package name must start with a lowercase letter, found '{c}'"
                )
            }
            Self::InvalidCharacter(c) => {
                write!(
                    f,
                    "package name must contain only lowercase letters, digits, and underscores — found '{c}'"
                )
            }
            Self::ReservedName => {
                write!(
                    f,
                    "is a reserved package name (conflicts with Beamtalk core)"
                )
            }
            Self::ErlangAppConflict => {
                write!(f, "conflicts with an Erlang/OTP standard application")
            }
        }
    }
}

/// Validate a package name according to ADR 0026 §8.
///
/// Package names must be:
/// - Lowercase ASCII letters, digits, and underscores only
/// - Start with a letter
/// - 1–64 characters
/// - Not a reserved Beamtalk name
/// - Not an Erlang standard application name
///
/// # Errors
///
/// Returns a [`PackageNameError`] describing which rule `name` violates.
///
/// # Panics
///
/// Never panics: the internal `.unwrap()` on the first character is only
/// reached after the preceding empty-name check has already returned.
pub fn validate_package_name(name: &str) -> Result<(), PackageNameError> {
    if name.is_empty() {
        return Err(PackageNameError::Empty);
    }

    if name.len() > 64 {
        return Err(PackageNameError::TooLong(name.len()));
    }

    let first = name.chars().next().unwrap();
    if !first.is_ascii_lowercase() {
        return Err(PackageNameError::StartsWithNonLetter(first));
    }

    for c in name.chars() {
        if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_') {
            return Err(PackageNameError::InvalidCharacter(c));
        }
    }

    if RESERVED_NAMES.contains(&name) {
        return Err(PackageNameError::ReservedName);
    }

    if ERLANG_APPS.contains(&name) {
        return Err(PackageNameError::ErlangAppConflict);
    }

    Ok(())
}

/// Suggest a corrected package name from an invalid one.
///
/// Converts CamelCase to `snake_case`, replaces non-alphanumeric chars with
/// underscores, strips leading non-letter chars, and collapses consecutive
/// underscores. Returns `None` if no reasonable suggestion can be made.
pub fn suggest_package_name(name: &str) -> Option<String> {
    // Insert underscores before uppercase letters (CamelCase → snake_case)
    let mut expanded = String::new();
    let mut prev_char: Option<char> = None;
    for c in name.chars() {
        if c.is_ascii_uppercase()
            && let Some(p) = prev_char
            && (p.is_ascii_lowercase() || p.is_ascii_digit())
        {
            expanded.push('_');
        }
        expanded.push(c);
        prev_char = Some(c);
    }

    let suggested: String = expanded
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '_'
            }
        })
        .collect();

    // Strip leading non-letter characters
    let suggested = suggested.trim_start_matches(|c: char| !c.is_ascii_lowercase());

    // Collapse consecutive underscores and trim trailing ones
    let mut result = String::new();
    let mut prev_underscore = false;
    for c in suggested.chars() {
        if c == '_' {
            if !prev_underscore {
                result.push(c);
            }
            prev_underscore = true;
        } else {
            result.push(c);
            prev_underscore = false;
        }
    }
    let result = result.trim_end_matches('_').to_string();

    if result.is_empty() || result == name {
        return None;
    }

    // Only suggest if the suggestion is actually valid
    if validate_package_name(&result).is_ok() {
        Some(result)
    } else {
        None
    }
}
