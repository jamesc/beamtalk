// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;
use crate::docvec;

/// Block primitive implementations.
pub(crate) fn generate_block_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "arity" => {
            // erlang:fun_info(Self, arity) returns {arity, N}
            Some(Document::Str(
                "let <ArityTuple> = call 'erlang':'fun_info'(Self, 'arity') in \
                 call 'erlang':'element'(2, ArityTuple)",
            ))
        }
        "valueWithArguments:" => {
            let p0 = params.first().map_or("_Args", String::as_str);
            Some(docvec!["call 'erlang':'apply'(Self, ", p0.to_string(), ")"])
        }
        // on:do: and ensure: are structural intrinsics handled at the call site
        // (see control_flow/exception_handling.rs), not here.
        _ => None,
    }
}
