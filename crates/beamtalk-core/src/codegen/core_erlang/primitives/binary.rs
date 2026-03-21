// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary primitive implementations (ADR 0069 Phase 2b).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "selector"` annotations on Binary class to direct calls
//! to the `beamtalk_binary` Erlang runtime module.

use super::super::document::Document;
use super::param;
use crate::docvec;

/// Binary primitive implementations.
pub(crate) fn generate_binary_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Instance methods
        "byte_size" => Some(Document::Str("call 'erlang':'byte_size'(Self)")),
        "do" => {
            let block = param(params, 0, "_Block").to_string();
            Some(docvec!["call 'beamtalk_binary':'do'(Self, ", block, ")",])
        }
        "print_string" => Some(Document::Str("call 'beamtalk_binary':'print_string'(Self)")),
        "at" => {
            let index = param(params, 0, "_Index").to_string();
            Some(docvec!["call 'beamtalk_binary':'at'(Self, ", index, ")",])
        }
        "byte_at" => {
            let offset = param(params, 0, "_Offset").to_string();
            Some(docvec![
                "call 'beamtalk_binary':'byte_at'(Self, ",
                offset,
                ")",
            ])
        }
        "part" => {
            let offset = param(params, 0, "_Offset").to_string();
            let size = param(params, 1, "_Size").to_string();
            Some(docvec![
                "call 'beamtalk_binary':'part'(Self, ",
                offset,
                ", ",
                size,
                ")",
            ])
        }
        "concat" => {
            let other = param(params, 0, "_Other").to_string();
            Some(docvec![
                "call 'beamtalk_binary':'concat'(Self, ",
                other,
                ")",
            ])
        }
        "to_bytes" => Some(Document::Str("call 'beamtalk_binary':'to_bytes'(Self)")),
        "as_string" => Some(Document::Str("call 'beamtalk_binary':'as_string'(Self)")),
        "as_string_unchecked" => Some(Document::Str(
            "call 'beamtalk_binary':'as_string_unchecked'(Self)",
        )),
        // Class methods
        "deserialize_with_used" => {
            let binary = param(params, 0, "_Binary").to_string();
            Some(docvec![
                "call 'beamtalk_binary':'deserialize_with_used'(",
                binary,
                ")",
            ])
        }
        "from_bytes" => {
            let list = param(params, 0, "_List").to_string();
            Some(docvec!["call 'beamtalk_binary':'from_bytes'(", list, ")",])
        }
        _ => None,
    }
}
