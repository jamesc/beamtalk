// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! hex.pm version-constraint validation and matching.
//!
//! **DDD Context:** Build System
//!
//! Pure logic (no I/O): validates the constraint strings accepted in
//! `[native.dependencies]` and tests a resolved version against one. Moved out
//! of `manifest.rs` (BT-4700); behaviour is unchanged.

use miette::Result;

/// Validate a hex.pm version constraint string.
///
/// Accepted forms:
/// - Exact version: `"2.12.0"`, `"1.0"`, `"3"`
/// - Pessimistic (tilde-arrow): `"~> 2.1"`, `"~> 2.1.0"`
/// - Comparison operators: `">= 1.0.0"`, `"> 1.0"`, `"< 2.0.0"`, `"<= 3.0"`
/// - Compound constraints: `">= 1.0.0 and < 2.0.0"`
///
/// Version segments must be non-negative integers separated by dots (max 3 segments).
pub(super) fn validate_hex_constraint(constraint: &str) -> Result<(), String> {
    let trimmed = constraint.trim();
    if trimmed.is_empty() {
        return Err("version constraint must not be empty".to_string());
    }

    // Check for compound constraints with "and"
    if trimmed.contains(" and ") {
        let parts: Vec<&str> = trimmed.split(" and ").collect();
        if parts.len() != 2 {
            return Err(
                "compound constraints must have exactly two parts separated by 'and'".to_string(),
            );
        }
        for part in &parts {
            validate_single_constraint(part.trim())?;
            // Compound parts must use comparison operators, not ~> or exact versions
            let p = part.trim();
            if p.starts_with("~>") {
                return Err(
                    "'~>' (pessimistic) constraints cannot be used in compound expressions"
                        .to_string(),
                );
            }
            if !p.starts_with(">=")
                && !p.starts_with('>')
                && !p.starts_with("<=")
                && !p.starts_with('<')
            {
                return Err(
                    "compound constraints must use comparison operators (>=, >, <, <=)".to_string(),
                );
            }
        }
        return Ok(());
    }

    validate_single_constraint(trimmed)
}

/// Whether a resolved hex `version` satisfies a `constraint` already
/// accepted by [`validate_hex_constraint`] (hex.pm semantics: `~> 2.1` is
/// `>= 2.1.0 and < 3.0.0`, `~> 2.1.0` is `>= 2.1.0 and < 2.2.0`).
///
/// A pre-release version (`2.1.0-rc.1`) sorts before its release and never
/// matches a bare `~>` or exact constraint, so a lock holding one is simply
/// re-resolved. Returns `false` for anything unparseable.
pub fn hex_constraint_matches(constraint: &str, version: &str) -> bool {
    let Some(version) = HexVersion::parse(version) else {
        return false;
    };
    constraint
        .trim()
        .split(" and ")
        .all(|part| single_constraint_matches(part.trim(), &version))
}

/// A hex version for comparison: numeric `major.minor.patch` (missing
/// segments are 0) plus whether it is a release (pre-releases sort first).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct HexVersion {
    numbers: [u64; 3],
    is_release: bool,
}

impl HexVersion {
    fn parse(version: &str) -> Option<Self> {
        let version = version.trim();
        // Build metadata never affects precedence.
        let version = version.split('+').next()?;
        let (core, pre) = match version.split_once('-') {
            Some((core, pre)) => (core, Some(pre)),
            None => (version, None),
        };
        let mut numbers = [0; 3];
        let mut count = 0;
        for (slot, segment) in numbers.iter_mut().zip(core.split('.')) {
            *slot = segment.parse().ok()?;
            count += 1;
        }
        if count == 0 || core.split('.').count() > 3 {
            return None;
        }
        Some(Self {
            numbers,
            is_release: pre.is_none(),
        })
    }

    fn release(numbers: [u64; 3]) -> Self {
        Self {
            numbers,
            is_release: true,
        }
    }
}

fn single_constraint_matches(s: &str, version: &HexVersion) -> bool {
    let bound = |rest: &str| HexVersion::parse(rest);
    if let Some(rest) = s.strip_prefix("~>") {
        let rest = rest.trim();
        let Some(lower) = bound(rest) else {
            return false;
        };
        let [major, minor, _] = lower.numbers;
        // `~> X.Y` allows any X.*; `~> X.Y.Z` allows any X.Y.*.
        let upper = if rest.split('.').count() >= 3 {
            [major, minor + 1, 0]
        } else {
            [major + 1, 0, 0]
        };
        version.is_release && *version >= lower && *version < HexVersion::release(upper)
    } else if let Some(rest) = s.strip_prefix(">=") {
        bound(rest).is_some_and(|b| *version >= b)
    } else if let Some(rest) = s.strip_prefix("<=") {
        bound(rest).is_some_and(|b| *version <= b)
    } else if let Some(rest) = s.strip_prefix('>') {
        bound(rest).is_some_and(|b| *version > b)
    } else if let Some(rest) = s.strip_prefix('<') {
        bound(rest).is_some_and(|b| *version < b)
    } else {
        bound(s).is_some_and(|b| *version == b)
    }
}

/// Validate a single (non-compound) version constraint.
fn validate_single_constraint(s: &str) -> Result<(), String> {
    if let Some(rest) = s.strip_prefix("~>") {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'~>' must be followed by a version number".to_string());
        }
        validate_version_string(version)?;
        // ~> requires at least 2 segments (e.g. ~> 2.1, not ~> 2)
        let segments: Vec<&str> = version.split('.').collect();
        if segments.len() < 2 {
            return Err("'~>' requires at least a major.minor version (e.g. '~> 2.1')".to_string());
        }
        Ok(())
    } else if let Some(rest) = s.strip_prefix(">=") {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'>=' must be followed by a version number".to_string());
        }
        validate_version_string(version)
    } else if let Some(rest) = s.strip_prefix("<=") {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'<=' must be followed by a version number".to_string());
        }
        validate_version_string(version)
    } else if let Some(rest) = s.strip_prefix('>') {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'>' must be followed by a version number".to_string());
        }
        validate_version_string(version)
    } else if let Some(rest) = s.strip_prefix('<') {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'<' must be followed by a version number".to_string());
        }
        validate_version_string(version)
    } else {
        // Must be an exact version
        validate_version_string(s)
    }
}

/// Validate a bare version string like `"2.1.0"`, `"2.1"`, or `"2"`.
///
/// Segments must be non-negative integers, 1–3 segments allowed.
fn validate_version_string(version: &str) -> Result<(), String> {
    let trimmed = version.trim();
    if trimmed.is_empty() {
        return Err("version must not be empty".to_string());
    }

    let segments: Vec<&str> = trimmed.split('.').collect();
    if segments.len() > 3 {
        return Err(format!(
            "version '{trimmed}' has too many segments (max 3: major.minor.patch)"
        ));
    }

    for (i, segment) in segments.iter().enumerate() {
        if segment.is_empty() {
            return Err(format!(
                "version '{trimmed}' has an empty segment at position {i}"
            ));
        }
        if segment.parse::<u64>().is_err() {
            return Err(format!(
                "version '{trimmed}' has non-numeric segment '{segment}'"
            ));
        }
    }

    Ok(())
}
