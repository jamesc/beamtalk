// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `manifest` tests: hex.

use super::*;
use crate::manifest::hex::validate_hex_constraint;

#[test]
fn test_hex_constraint_matches() {
    let cases = [
        ("~> 2.1", "2.1.0", true),
        ("~> 2.1", "2.9.3", true),
        ("~> 2.1", "3.0.0", false),
        ("~> 2.1", "2.0.9", false),
        ("~> 2.1.0", "2.1.7", true),
        ("~> 2.1.0", "2.2.0", false),
        ("~> 2.1", "2.2.0-rc.1", false),
        ("2.12.0", "2.12.0", true),
        ("2.12", "2.12.0", true),
        ("2.12.0", "2.12.1", false),
        (">= 1.0.0 and < 2.0.0", "1.5.0", true),
        (">= 1.0.0 and < 2.0.0", "2.0.0", false),
        ("> 1.0", "1.0.0", false),
        ("<= 3.0", "3.0.0", true),
        ("~> 2.1", "not-a-version", false),
    ];
    for (constraint, version, expected) in cases {
        assert_eq!(
            hex_constraint_matches(constraint, version),
            expected,
            "{constraint} vs {version}"
        );
    }
}

// --- Hex constraint validation unit tests ---

#[test]
fn test_validate_hex_constraint_valid() {
    let valid = [
        "2.12.0",
        "1.0",
        "3",
        "~> 2.1",
        "~> 2.1.0",
        ">= 1.0.0",
        "> 1.0",
        "< 2.0.0",
        "<= 3.0",
        ">= 1.0.0 and < 2.0.0",
        ">= 0.1 and <= 1.0",
    ];
    for c in valid {
        assert!(
            validate_hex_constraint(c).is_ok(),
            "expected '{c}' to be valid"
        );
    }
}

#[test]
fn test_validate_hex_constraint_invalid() {
    let invalid = [
        ("", "empty"),
        ("abc", "non-numeric"),
        ("1.2.3.4", "too many segments"),
        ("~> 2", "major.minor"),
        ("~>", "must be followed"),
        (">=", "must be followed"),
        (">= 1.0.0 and ~> 2.0", "pessimistic"),
        (">= 1.0 and 2.0", "comparison operators"),
    ];
    for (constraint, expected_msg) in invalid {
        let result = validate_hex_constraint(constraint);
        assert!(result.is_err(), "expected '{constraint}' to be invalid");
        let err = result.unwrap_err();
        assert!(
            err.contains(expected_msg),
            "error for '{constraint}' should contain '{expected_msg}', got: {err}"
        );
    }
}
