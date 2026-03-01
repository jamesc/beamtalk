// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Array primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "selector"` annotations for the `Array` class to
//! calls on `beamtalk_array_ops`. Arrays are backed by Erlang's `array`
//! module and stored as tagged maps:
//!   `#{'$beamtalk_class' => 'Array', 'data' => ErlangArray}`

use super::super::document::Document;
use crate::docvec;

/// Array primitive implementations (BT-822).
pub(crate) fn generate_array_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "size" => Some(Document::Str("call 'beamtalk_array_ops':'size'(Self)")),
        "isEmpty" => Some(Document::Str("call 'beamtalk_array_ops':'is_empty'(Self)")),
        "do:" => Some(docvec![
            "call 'beamtalk_array_ops':'do'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "at:" => Some(docvec![
            "call 'beamtalk_array_ops':'at'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "at:put:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_array_ops':'at_put'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "withAll:" => Some(docvec![
            "call 'beamtalk_array_ops':'from_list'(",
            p0.to_string(),
            ")"
        ]),
        "collect:" => Some(docvec![
            "call 'beamtalk_array_ops':'collect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "select:" => Some(docvec![
            "call 'beamtalk_array_ops':'select'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "inject:into:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_array_ops':'inject_into'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "includes:" => Some(docvec![
            "call 'beamtalk_array_ops':'includes'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "printString" => Some(Document::Str(
            "call 'beamtalk_array_ops':'print_string'(Self)",
        )),
        _ => None,
    }
}
