// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Reflection primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for introspection and meta-level classes:
//! `CompiledMethod`, `JSON`, `Regex`, `Symbol`.

use super::super::document::Document;
use super::{binary_bif, param};
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
        "printString" => Some(Document::Str(
            "call 'beamtalk_primitive':'print_string'(Self)",
        )),
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
        "selector" => Some(Document::Str(
            "call 'beamtalk_compiled_method_ops':'dispatch'('selector', [], Self)",
        )),
        "source" => Some(Document::Str(
            "call 'beamtalk_compiled_method_ops':'dispatch'('source', [], Self)",
        )),
        "doc" => Some(Document::Str(
            "call 'beamtalk_compiled_method_ops':'dispatch'('doc', [], Self)",
        )),
        "argumentCount" => Some(Document::Str(
            "call 'beamtalk_compiled_method_ops':'dispatch'('argumentCount', [], Self)",
        )),
        "printString" => Some(Document::Str(
            "call 'beamtalk_compiled_method_ops':'dispatch'('printString', [], Self)",
        )),
        "asString" => Some(Document::Str(
            "call 'beamtalk_compiled_method_ops':'dispatch'('asString', [], Self)",
        )),
        _ => None,
    }
}

/// JSON primitive implementations (BT-711).
///
/// JSON class methods delegate directly to `beamtalk_json` runtime module.
/// These are class-level methods (no Self parameter needed).
pub(crate) fn generate_json_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "parse:" => Some(docvec![
            "call 'beamtalk_json':'parse:'(",
            p0.to_string(),
            ")"
        ]),
        "generate:" => Some(docvec![
            "call 'beamtalk_json':'generate:'(",
            p0.to_string(),
            ")"
        ]),
        "prettyPrint:" => Some(docvec![
            "call 'beamtalk_json':'prettyPrint:'(",
            p0.to_string(),
            ")"
        ]),
        _ => None,
    }
}

/// Regex primitive implementations (BT-709).
///
/// Regex class methods delegate to `beamtalk_regex` runtime module.
/// Instance methods (source, printString) operate on Self.
pub(crate) fn generate_regex_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        "from:" => Some(docvec![
            "call 'beamtalk_regex':'from:'(",
            p0.to_string(),
            ")"
        ]),
        "from:options:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_regex':'from:options:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "source" => Some(Document::Str("call 'beamtalk_regex':'source'(Self)")),
        "printString" => Some(Document::Str("call 'beamtalk_regex':'printString'(Self)")),
        _ => None,
    }
}
