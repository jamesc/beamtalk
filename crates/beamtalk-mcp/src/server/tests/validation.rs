// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Argument-validation tests for `validate_class_name`, `validate_erlang_module_name`, and `validate_selector`.

use super::*;

// --- validate_class_name ---

#[test]
fn validate_class_name_valid_simple() {
    assert!(validate_class_name("Counter").is_ok());
}

#[test]
fn validate_class_name_valid_with_digits() {
    assert!(validate_class_name("Counter2").is_ok());
}

#[test]
fn validate_class_name_valid_with_underscore() {
    assert!(validate_class_name("My_Class").is_ok());
}

#[test]
fn validate_class_name_valid_single_char() {
    assert!(validate_class_name("A").is_ok());
}

#[test]
fn validate_class_name_empty_is_error() {
    assert!(validate_class_name("").is_err());
}

#[test]
fn validate_class_name_lowercase_start_is_error() {
    let err = validate_class_name("counter");
    assert!(err.is_err());
    let msg = err.unwrap_err().message;
    assert!(msg.contains("counter"), "error should mention the name");
}

#[test]
fn validate_class_name_digit_start_is_error() {
    assert!(validate_class_name("1Counter").is_err());
}

#[test]
fn validate_class_name_space_is_error() {
    assert!(validate_class_name("My Class").is_err());
}

#[test]
fn validate_class_name_hyphen_is_error() {
    assert!(validate_class_name("My-Class").is_err());
}

#[test]
fn validate_class_name_colon_is_error() {
    // Colons are not allowed in class names (only selectors)
    assert!(validate_class_name("My:Class").is_err());
}

#[test]
fn validate_class_name_unicode_is_error() {
    // Non-ASCII chars are not valid
    assert!(validate_class_name("Clàss").is_err());
}

#[test]
fn validate_class_name_error_message_contains_name() {
    let err = validate_class_name("bad name").unwrap_err();
    assert!(
        err.message.contains("bad name"),
        "error message should include the invalid name: {}",
        err.message
    );
}

// --- validate_erlang_module_name ---

#[test]
fn validate_erlang_module_name_valid() {
    assert!(validate_erlang_module_name("lists").is_ok());
    assert!(validate_erlang_module_name("maps").is_ok());
    assert!(validate_erlang_module_name("beamtalk_runtime").is_ok());
    assert!(validate_erlang_module_name("_private").is_ok());
}

#[test]
fn validate_erlang_module_name_empty_is_error() {
    assert!(validate_erlang_module_name("").is_err());
}

#[test]
fn validate_erlang_module_name_uppercase_start_is_error() {
    assert!(validate_erlang_module_name("Lists").is_err());
}

#[test]
fn validate_erlang_module_name_space_is_error() {
    assert!(validate_erlang_module_name("my module").is_err());
}

#[test]
fn validate_erlang_module_name_hyphen_is_error() {
    assert!(validate_erlang_module_name("my-module").is_err());
}

#[test]
fn validate_erlang_module_name_error_contains_name() {
    let err = validate_erlang_module_name("BadModule").unwrap_err();
    assert!(err.message.contains("BadModule"));
}

// --- validate_selector ---

#[test]
fn validate_selector_empty_is_error() {
    assert!(validate_selector("").is_err());
}

#[test]
fn validate_selector_unary_valid() {
    assert!(validate_selector("increment").is_ok());
    assert!(validate_selector("size").is_ok());
    assert!(validate_selector("isEmpty").is_ok());
}

#[test]
fn validate_selector_keyword_valid() {
    assert!(validate_selector("at:").is_ok());
    assert!(validate_selector("at:put:").is_ok());
    assert!(validate_selector("ifTrue:ifFalse:").is_ok());
}

#[test]
fn validate_selector_binary_operator_valid() {
    assert!(validate_selector("+").is_ok());
    assert!(validate_selector("-").is_ok());
    assert!(validate_selector("*").is_ok());
    assert!(validate_selector("/").is_ok());
    assert!(validate_selector("<").is_ok());
    assert!(validate_selector(">").is_ok());
    assert!(validate_selector("=").is_ok());
    assert!(validate_selector(">=").is_ok());
    assert!(validate_selector("<=").is_ok());
    assert!(validate_selector("**").is_ok());
    assert!(validate_selector("~=").is_ok());
}

#[test]
fn validate_selector_binary_with_alphanumeric_is_error() {
    // Starts with operator char but mixes in alphanumeric — invalid binary
    let err = validate_selector("+foo");
    assert!(err.is_err());
    let msg = err.unwrap_err().message;
    assert!(msg.contains("+foo"), "error should mention the selector");
}

#[test]
fn validate_selector_keyword_with_space_is_error() {
    assert!(validate_selector("at: put:").is_err());
}

#[test]
fn validate_selector_keyword_with_hash_is_error() {
    assert!(validate_selector("#size").is_err());
}

#[test]
fn validate_selector_keyword_starting_with_digit_is_ok() {
    // digits are alphanumeric — "1x:" is technically accepted by current impl
    // (no leading-char restriction on selectors, only on class names)
    assert!(validate_selector("at1:").is_ok());
}

#[test]
fn validate_selector_error_message_contains_selector() {
    let err = validate_selector("bad selector!").unwrap_err();
    assert!(
        err.message.contains("bad selector!"),
        "error message should include the invalid selector: {}",
        err.message
    );
}
