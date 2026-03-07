// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Array primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "selector"` annotations for the `Array` class to
//! calls on `beamtalk_array`. Arrays are backed by Erlang's `array`
//! module and stored as tagged maps:
//!   `#{'$beamtalk_class' => 'Array', 'data' => ErlangArray}`

use super::super::document::Document;
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
            p0.to_string(),
            ")"
        ]),
        "at:" => Some(docvec![
            "call 'beamtalk_array':'at'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "at:put:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_array':'at_put'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "withAll:" => Some(docvec![
            "call 'beamtalk_array':'from_list'(",
            p0.to_string(),
            ")"
        ]),
        "collect:" => Some(docvec![
            "call 'beamtalk_array':'collect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "select:" => Some(docvec![
            "call 'beamtalk_array':'select'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "inject:into:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_array':'inject_into'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "includes:" => Some(docvec![
            "call 'beamtalk_array':'includes'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "printString" => Some(Document::Str("call 'beamtalk_array':'print_string'(Self)")),
        _ => None,
    }
}
