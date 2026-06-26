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

use super::super::document::Document;
use super::super::document::leaf;
use super::param;
use crate::docvec;

/// Array primitive implementations (BT-822).
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
