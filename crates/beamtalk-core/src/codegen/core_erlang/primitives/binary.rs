// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary primitive implementations (ADR 0069 Phase 1).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive` selectors on Binary to their Erlang runtime
//! functions in `beamtalk_binary`.

use super::super::document::Document;
use super::param;
use crate::docvec;

/// Binary primitive implementations (ADR 0069).
pub(crate) fn generate_binary_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Collection subclass-responsibility (size) and byte-level alias (byteSize)
        "size" | "byteSize" => Some(Document::Str("call 'erlang':'byte_size'(Self)")),
        "do:" => {
            let block = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_binary':'do'(Self, ",
                block.to_string(),
                ")",
            ])
        }
        "printString" => Some(Document::Str("call 'beamtalk_binary':'print_string'(Self)")),

        // Byte-level access
        "at:" => {
            let index = param(params, 0, "_Index");
            Some(docvec![
                "call 'beamtalk_binary':'at'(Self, ",
                index.to_string(),
                ")",
            ])
        }
        "byteAt:" => {
            let offset = param(params, 0, "_Offset");
            Some(docvec![
                "call 'beamtalk_binary':'byte_at'(Self, ",
                offset.to_string(),
                ")",
            ])
        }
        // Slicing and concatenation
        "part:size:" => {
            let offset = param(params, 0, "_Offset");
            let length = param(params, 1, "_Length");
            Some(docvec![
                "call 'beamtalk_binary':'part'(Self, ",
                offset.to_string(),
                ", ",
                length.to_string(),
                ")",
            ])
        }
        "concat:" => {
            let other = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_binary':'concat'(Self, ",
                other.to_string(),
                ")",
            ])
        }

        // Conversion
        "toBytes" => Some(Document::Str("call 'beamtalk_binary':'to_bytes'(Self)")),
        "asString" => Some(Document::Str("call 'beamtalk_binary':'as_string'(Self)")),
        "asStringUnchecked" => Some(Document::Str(
            "call 'beamtalk_binary':'as_string_unchecked'(Self)",
        )),

        _ => None,
    }
}
