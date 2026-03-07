// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dictionary primitive implementations (BT-418).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Dictionaries are Erlang maps — immutable key-value collections.

use super::super::document::Document;
use super::param;
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
            let p0 = param(params, 0, "_Key");
            Some(docvec!["call 'maps':'get'(", p0.to_string(), ", Self)"])
        }
        "at:ifAbsent:" => {
            let p0 = param(params, 0, "_Key");
            let p1 = param(params, 1, "_Block");
            Some(docvec![
                "call 'beamtalk_map':'at_if_absent'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "at:put:" => {
            let p0 = param(params, 0, "_Key");
            let p1 = param(params, 1, "_Value");
            Some(docvec![
                "call 'maps':'put'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", Self)"
            ])
        }
        "includesKey:" => {
            let p0 = param(params, 0, "_Key");
            Some(docvec!["call 'maps':'is_key'(", p0.to_string(), ", Self)"])
        }
        "removeKey:" => {
            let p0 = param(params, 0, "_Key");
            Some(docvec!["call 'maps':'remove'(", p0.to_string(), ", Self)"])
        }
        "merge:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec!["call 'maps':'merge'(Self, ", p0.to_string(), ")"])
        }
        "doWithKey:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_map':'do_with_key'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "do:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_map':'do'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "includes:" => {
            let p0 = param(params, 0, "_Element");
            Some(docvec![
                "call 'beamtalk_map':'includes'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "printString" => Some(Document::Str("call 'beamtalk_map':'print_string'(Self)")),
        _ => None,
    }
}
