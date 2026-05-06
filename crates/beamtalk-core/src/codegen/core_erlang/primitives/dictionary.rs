// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dictionary primitive implementations (BT-418).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Dictionaries are Erlang maps — immutable key-value collections.

use super::super::document::Document;
use super::{call_p0_self, call_self_p0, param};
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
        "at:" => Some(call_p0_self("maps", "get", param(params, 0, "_Key"))),
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
        "includesKey:" => Some(call_p0_self("maps", "is_key", param(params, 0, "_Key"))),
        "removeKey:" => Some(call_p0_self("maps", "remove", param(params, 0, "_Key"))),
        "merge:" => Some(call_self_p0("maps", "merge", param(params, 0, "_Other"))),
        "doWithKey:" => Some(call_self_p0(
            "beamtalk_map",
            "do_with_key",
            param(params, 0, "_Block"),
        )),
        "do:" => Some(call_self_p0(
            "beamtalk_map",
            "do",
            param(params, 0, "_Block"),
        )),
        "includes:" => Some(call_self_p0(
            "beamtalk_map",
            "includes",
            param(params, 0, "_Element"),
        )),
        "printString" => Some(Document::Str("call 'beamtalk_map':'print_string'(Self)")),
        _ => None,
    }
}
