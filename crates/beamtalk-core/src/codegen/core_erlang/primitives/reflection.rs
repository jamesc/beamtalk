// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Reflection primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for introspection and meta-level classes:
//! `CompiledMethod`, `Symbol`.

use super::super::document::Document;
use super::super::document::leaf;
use super::binary_bif;
use crate::docvec;

/// Symbol primitive implementations (BT-273).
///
/// Symbols are Erlang atoms — interned, immutable identifiers.
pub(crate) fn generate_symbol_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Comparison (ADR 0002: Erlang operators)
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "=/=" => binary_bif("=/=", params),
        // Conversion
        "asString" => Some(Document::Str(
            "call 'erlang':'atom_to_binary'(Self, 'utf8')",
        )),
        "asAtom" => {
            // Identity — symbols are already atoms
            Some(Document::Str("Self"))
        }
        // Display — delegate to runtime primitive for consistent formatting
        "printString" => Some(super::PRINT_STRING),
        // Identity
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => None,
    }
}

/// `CompiledMethod` primitive implementations.
///
/// All selectors delegate to `beamtalk_compiled_method_ops:dispatch/3`
/// (the hand-written Erlang runtime helper) to avoid recursion through
/// the compiled stdlib module's own dispatch/3.
pub(crate) fn generate_compiled_method_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    let _ = params; // all CompiledMethod selectors are zero-param instance reads
    match selector {
        "selector" | "source" | "doc" | "argumentCount" | "printString" | "asString" => {
            Some(docvec![
                "call 'beamtalk_compiled_method_ops':'dispatch'(",
                leaf::atom(selector.to_owned()),
                ", [], Self)",
            ])
        }
        _ => None,
    }
}
