// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0153 (BT-5152): `classState:` and class-side `self.x` are compile errors.

use super::*;

fn errors(src: &str) -> Vec<(String, Option<String>)> {
    analyse(&parse_bt(src))
        .diagnostics
        .into_iter()
        .filter(|d| d.severity == crate::source_analysis::Severity::Error)
        .map(|d| (d.message.to_string(), d.hint.map(|h| h.to_string())))
        .collect()
}

#[test]
fn self_field_read_in_class_method_is_an_error() {
    let errs = errors("Object subclass: Foo\n  class count => self.count\n");
    assert_eq!(errs.len(), 1, "{errs:?}");
    assert_eq!(
        errs[0].0,
        "`self.count` in a class method: a class has no slots."
    );
    assert_eq!(
        errs[0].1.as_deref(),
        Some("`self.x` reads an instance's state. Class methods have none.")
    );
}

#[test]
fn self_field_write_inside_a_block_in_class_method_is_an_error() {
    let errs = errors("Object subclass: Foo\n  class bump => [self.n := 1] value\n");
    assert!(
        errs.iter()
            .any(|(m, _)| m == "`self.n` in a class method: a class has no slots."),
        "{errs:?}"
    );
}

#[test]
fn self_field_in_instance_method_is_untouched() {
    let errs = errors("Actor subclass: Foo\n  state: n = 0\n  get => self.n\n");
    assert!(errs.is_empty(), "{errs:?}");
}
