// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Array primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "selector"` annotations for the `Array` class to
//! calls on `beamtalk_array`. Arrays are stored as tagged maps with a
//! canonical index→value `'data'` map (ADR 0090):
//!   `#{'$beamtalk_class' => 'Array', 'data' => #{0 => V0, 1 => V1, ...}}`

use super::param;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// Array primitive implementations.
pub(crate) fn generate_array_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "size" => Some(Document::Str("call 'beamtalk_array':'size'(Self)")),
        "isEmpty" => Some(Document::Str("call 'beamtalk_array':'is_empty'(Self)")),
        "do:" => Some(docvec![
            "call 'beamtalk_array':'do'(Self, ",
            leaf::var(p0.to_string()),
            ")"
        ]),
        "at:" => Some(docvec![
            "call 'beamtalk_array':'at'(Self, ",
            leaf::var(p0.to_string()),
            ")"
        ]),
        // Mirrors String's "first"/"last" — both raise `empty_collection`
        // on an empty Array.
        "first" => Some(Document::Str("call 'beamtalk_array':'first'(Self)")),
        "last" => Some(Document::Str("call 'beamtalk_array':'last'(Self)")),
        "at:put:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_array':'at_put'(Self, ",
                leaf::var(p0.to_string()),
                ", ",
                leaf::var(p1.to_string()),
                ")"
            ])
        }
        "withAll:" => Some(docvec![
            "call 'beamtalk_array':'from_list'(",
            leaf::var(p0.to_string()),
            ")"
        ]),
        "collect:" => Some(docvec![
            "call 'beamtalk_array':'collect'(Self, ",
            leaf::var(p0.to_string()),
            ")"
        ]),
        "select:" => Some(docvec![
            "call 'beamtalk_array':'select'(Self, ",
            leaf::var(p0.to_string()),
            ")"
        ]),
        "inject:into:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_array':'inject_into'(Self, ",
                leaf::var(p0.to_string()),
                ", ",
                leaf::var(p1.to_string()),
                ")"
            ])
        }
        "includes:" => Some(docvec![
            "call 'beamtalk_array':'includes'(Self, ",
            leaf::var(p0.to_string()),
            ")"
        ]),
        "printString" => Some(Document::Str("call 'beamtalk_array':'print_string'(Self)")),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::generate_array_bif;
    use crate::core_erlang::primitives::doc_to_string;

    fn check(selector: &str, params: &[&str], expected: &str) {
        let params: Vec<String> = params.iter().map(|s| (*s).to_string()).collect();
        assert_eq!(
            doc_to_string(generate_array_bif(selector, &params)),
            Some(expected.to_string()),
            "selector: {selector:?}"
        );
    }

    #[test]
    fn test_do_block() {
        check("do:", &["Block"], "call 'beamtalk_array':'do'(Self, Block)");
    }

    #[test]
    fn test_at_index() {
        check("at:", &["Index"], "call 'beamtalk_array':'at'(Self, Index)");
    }

    #[test]
    fn test_at_put() {
        check(
            "at:put:",
            &["Index", "Value"],
            "call 'beamtalk_array':'at_put'(Self, Index, Value)",
        );
    }

    #[test]
    fn test_with_all() {
        check(
            "withAll:",
            &["List"],
            "call 'beamtalk_array':'from_list'(List)",
        );
    }

    #[test]
    fn test_collect() {
        check(
            "collect:",
            &["Block"],
            "call 'beamtalk_array':'collect'(Self, Block)",
        );
    }

    #[test]
    fn test_select() {
        check(
            "select:",
            &["Block"],
            "call 'beamtalk_array':'select'(Self, Block)",
        );
    }

    #[test]
    fn test_inject_into() {
        check(
            "inject:into:",
            &["Init", "Block"],
            "call 'beamtalk_array':'inject_into'(Self, Init, Block)",
        );
    }

    #[test]
    fn test_includes() {
        check(
            "includes:",
            &["Elem"],
            "call 'beamtalk_array':'includes'(Self, Elem)",
        );
    }

    #[test]
    fn test_unknown_selector_returns_none() {
        assert!(generate_array_bif("notAMethod", &[]).is_none());
    }
}
