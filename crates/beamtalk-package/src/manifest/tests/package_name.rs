// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `manifest` tests: package-name validation.

use super::*;

// --- Package name validation tests ---

#[test]
fn test_valid_package_names() {
    let valid = [
        "a",
        "my_app",
        "json_parser",
        "web_utils",
        "counter",
        "x123",
        "app_v2",
        "a_b_c_d",
    ];
    for name in valid {
        assert!(
            validate_package_name(name).is_ok(),
            "expected '{name}' to be valid"
        );
    }
}

#[test]
fn test_empty_name() {
    assert_eq!(validate_package_name(""), Err(PackageNameError::Empty));
}

#[test]
fn test_too_long_name() {
    let long_name = "a".repeat(65);
    assert_eq!(
        validate_package_name(&long_name),
        Err(PackageNameError::TooLong(65))
    );
    // Exactly 64 is fine
    let max_name = "a".repeat(64);
    assert!(validate_package_name(&max_name).is_ok());
}

#[test]
fn test_starts_with_digit() {
    assert_eq!(
        validate_package_name("123app"),
        Err(PackageNameError::StartsWithNonLetter('1'))
    );
}

#[test]
fn test_starts_with_underscore() {
    assert_eq!(
        validate_package_name("_private"),
        Err(PackageNameError::StartsWithNonLetter('_'))
    );
}

#[test]
fn test_uppercase_letters() {
    assert_eq!(
        validate_package_name("MyApp"),
        Err(PackageNameError::StartsWithNonLetter('M'))
    );
}

#[test]
fn test_mixed_case() {
    assert_eq!(
        validate_package_name("myApp"),
        Err(PackageNameError::InvalidCharacter('A'))
    );
}

#[test]
fn test_dashes() {
    assert_eq!(
        validate_package_name("my-app"),
        Err(PackageNameError::InvalidCharacter('-'))
    );
}

#[test]
fn test_dots() {
    assert_eq!(
        validate_package_name("my.app"),
        Err(PackageNameError::InvalidCharacter('.'))
    );
}

#[test]
fn test_spaces() {
    assert_eq!(
        validate_package_name("my app"),
        Err(PackageNameError::InvalidCharacter(' '))
    );
}

#[test]
fn test_reserved_names() {
    for name in RESERVED_NAMES {
        assert_eq!(
            validate_package_name(name),
            Err(PackageNameError::ReservedName),
            "expected '{name}' to be rejected as reserved"
        );
    }
}

#[test]
fn test_erlang_app_names() {
    for name in ERLANG_APPS {
        assert_eq!(
            validate_package_name(name),
            Err(PackageNameError::ErlangAppConflict),
            "expected '{name}' to be rejected as Erlang app"
        );
    }
}

// --- Suggestion tests ---

#[test]
fn test_suggest_camel_case() {
    assert_eq!(suggest_package_name("MyApp"), Some("my_app".to_string()));
}

#[test]
fn test_suggest_dashes() {
    assert_eq!(
        suggest_package_name("my-cool-app"),
        Some("my_cool_app".to_string())
    );
}

#[test]
fn test_suggest_leading_digits() {
    assert_eq!(suggest_package_name("123app"), Some("app".to_string()));
}

#[test]
fn test_suggest_mixed() {
    assert_eq!(
        suggest_package_name("My-Cool_App123"),
        Some("my_cool_app123".to_string())
    );
}

#[test]
fn test_suggest_reserved_returns_none() {
    // Can't suggest a fix for "stdlib" → "stdlib"
    assert_eq!(suggest_package_name("stdlib"), None);
}

#[test]
fn test_suggest_no_letters_returns_none() {
    assert_eq!(suggest_package_name("123"), None);
}

#[test]
fn test_suggest_already_valid_returns_none() {
    assert_eq!(suggest_package_name("my_app"), None);
}

#[test]
fn test_suggest_unicode_does_not_panic() {
    // Non-ASCII input should not panic despite char/byte index mismatch
    let _ = suggest_package_name("café");
    let _ = suggest_package_name("über_app");
    let _ = suggest_package_name("日本語");
}
