// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for `gen_server` module and class-level Core Erlang code generation.
//!
//! Split by feature (see `docs/development/rust-guidelines.md` § Testing);
//! each submodule stays under ~1,500 lines:
//! - [`class_registration`] — empty-module and class-registration codegen,
//!   multi-class registration order and error short-circuiting
//! - [`class_kind_and_value_accessors`] — actor/value class-kind
//!   classification and value-subclass auto-accessor/writeback metadata
//! - [`type_aliases`] — type-alias emission for class fields
//! - [`extensions`] — same-module and cross-module (foreign) class
//!   extension codegen
//! - [`state_threading_loops`] — state threading through loop constructs
//! - [`class_var_shadow_writes`] — ADR 0110 class-variable shadow-write
//!   codegen and its nested-loop compile-error cases
//! - [`native_facade`] — `native:`-backed class facade codegen
//! - [`protocol_and_xref_metadata`] — protocol-only modules and
//!   `methodXref`/`stateVarXref` metadata
//! - [`actor_init_and_field_validation`] — actor initialize chaining and
//!   typed-field-without-default validation
//! - [`dispatch_and_lifecycle`] — `gen_server` lifecycle callbacks and
//!   dispatch-structure codegen
//! - [`tier2_nested_blocks`] — Tier 2 stateful nested-block bodies
//! - [`abstract_actor_stubs`] — abstract actor stub codegen
//! - [`state_threading_self_dispatch`] — state threading for self-dispatch
//!   sequencing

use super::*;

/// Extract a Core Erlang function body from generated code by cutting at the
/// next function header rather than relying on blank-line formatting.
///
/// Given a `marker` like `"'has_method'/1 = fun"`, returns the text from
/// that marker to the next top-level function definition (`'name'/N = fun`).
fn extract_core_fn<'a>(code: &'a str, marker: &str) -> Option<&'a str> {
    let start = code.find(marker)?;
    let body = &code[start + marker.len()..];
    // Scan for the next Core Erlang function header: a line starting with
    // `'<name>'/<digits> = fun`.  Track byte offset via cumulative line lengths
    // to avoid ambiguous substring matching.
    let mut offset = 0;
    for (i, line) in body.split('\n').enumerate() {
        if i == 0 {
            offset += line.len() + 1;
            continue;
        }
        let trimmed = line.trim_start();
        if trimmed.starts_with('\'') && trimmed.contains("'/") && trimmed.contains("= fun") {
            return Some(&body[..offset]);
        }
        offset += line.len() + 1;
    }
    Some(body)
}

/// Extract the module-header export list from generated Core Erlang.
///
/// A Core Erlang module header looks like:
///   module 'Name' ['export1'/0, 'export2'/1, ...]
///     attributes [...]
///
/// Returns the bracketed export list as a string (without the surrounding
/// brackets), or an empty string if no header is found. Used by tests that
/// want to assert on the exported API surface without false-positive matches
/// against function definitions deeper in the module body.
fn extract_module_exports(code: &str) -> String {
    let Some(module_start) = code.find("module '") else {
        return String::new();
    };
    let after_module = &code[module_start..];
    let Some(bracket_open) = after_module.find('[') else {
        return String::new();
    };
    let Some(bracket_close) = after_module[bracket_open..].find(']') else {
        return String::new();
    };
    after_module[bracket_open + 1..bracket_open + bracket_close].to_string()
}

mod abstract_actor_stubs;
mod actor_init_and_field_validation;
mod class_kind_and_value_accessors;
mod class_registration;
mod class_var_shadow_writes;
mod dispatch_and_lifecycle;
mod extensions;
mod native_facade;
mod protocol_and_xref_metadata;
mod state_threading_loops;
mod state_threading_self_dispatch;
mod tier2_nested_blocks;
mod type_aliases;
