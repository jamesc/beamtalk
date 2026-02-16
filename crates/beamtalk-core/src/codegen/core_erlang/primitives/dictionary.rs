// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dictionary primitive implementations (BT-418).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Dictionaries are Erlang maps — immutable key-value collections.

use super::super::document::Document;
use crate::docvec;

/// Dictionary primitive implementations (BT-418).
pub(crate) fn generate_dictionary_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'map_size'(Self)")),
        "keys" => Some(Document::Str("call 'maps':'keys'(Self)")),
        "values" => Some(Document::Str("call 'maps':'values'(Self)")),
        "at:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(docvec!["call 'maps':'get'(", p0.to_string(), ", Self)"])
        }
        "at:ifAbsent:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            let p1 = params.get(1).map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_map_ops':'at_if_absent'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "at:put:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            let p1 = params.get(1).map_or("_Value", String::as_str);
            Some(docvec![
                "call 'maps':'put'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", Self)"
            ])
        }
        "includesKey:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(docvec!["call 'maps':'is_key'(", p0.to_string(), ", Self)"])
        }
        "removeKey:" => {
            let p0 = params.first().map_or("_Key", String::as_str);
            Some(docvec!["call 'maps':'remove'(", p0.to_string(), ", Self)"])
        }
        "merge:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec!["call 'maps':'merge'(Self, ", p0.to_string(), ")"])
        }
        "keysAndValuesDo:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_map_ops':'keys_and_values_do'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_map_ops':'do'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "includes:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_map_ops':'includes'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "printString" => Some(Document::Str(
            "call 'beamtalk_map_ops':'print_string'(Self)",
        )),
        // Streaming (BT-514)
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        _ => None,
    }
}
