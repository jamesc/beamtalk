// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive BIF mappings for the `Telemetry` stdlib facade (ADR 0069,
//! amended by BT-4025).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps the `@primitive "telemetryXxx"` class method bodies in
//! `stdlib/src/telemetry.bt` to direct calls into `beamtalk_telemetry`, so an
//! emit never goes through the `(Erlang …)` FFI proxy (BT-4021).

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::join;
use beamtalk_cerl_doc::leaf;

/// Generates Core Erlang for a `Telemetry` class method primitive: a direct
/// `call 'beamtalk_telemetry':'fn'(Params...)`, with the parameter count
/// checked against the runtime function's arity.
pub fn generate_telemetry_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let (function, arity) = match selector {
        "telemetryExecute" => ("execute", 3),
        "telemetrySpan" => ("span", 3),
        "telemetryAttach" => ("attach", 3),
        "telemetryAttachMany" => ("attach_many", 3),
        "telemetryDetach" => ("detach", 1),
        _ => return None,
    };
    if params.len() != arity {
        return None;
    }
    Some(docvec![
        "call 'beamtalk_telemetry':",
        leaf::atom(function),
        "(",
        join(
            params.iter().map(|p| leaf::var(p.clone())),
            &Document::Str(", ")
        ),
        ")"
    ])
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    fn params(names: &[&str]) -> Vec<String> {
        names.iter().map(|s| (*s).to_string()).collect()
    }

    #[test]
    fn test_telemetry_execute() {
        let result = doc_to_string(generate_telemetry_bif(
            "telemetryExecute",
            &params(&["Name", "M", "Md"]),
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_telemetry':'execute'(Name, M, Md)".to_string())
        );
    }

    #[test]
    fn test_telemetry_span() {
        let result = doc_to_string(generate_telemetry_bif(
            "telemetrySpan",
            &params(&["P", "Md", "B"]),
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_telemetry':'span'(P, Md, B)".to_string())
        );
    }

    #[test]
    fn test_telemetry_attach_and_attach_many() {
        assert_eq!(
            doc_to_string(generate_telemetry_bif(
                "telemetryAttach",
                &params(&["Id", "E", "B"])
            )),
            Some("call 'beamtalk_telemetry':'attach'(Id, E, B)".to_string())
        );
        assert_eq!(
            doc_to_string(generate_telemetry_bif(
                "telemetryAttachMany",
                &params(&["Id", "Es", "B"])
            )),
            Some("call 'beamtalk_telemetry':'attach_many'(Id, Es, B)".to_string())
        );
    }

    #[test]
    fn test_telemetry_detach() {
        assert_eq!(
            doc_to_string(generate_telemetry_bif("telemetryDetach", &params(&["Id"]))),
            Some("call 'beamtalk_telemetry':'detach'(Id)".to_string())
        );
    }

    #[test]
    fn test_telemetry_wrong_arity_or_unknown_is_none() {
        assert!(generate_telemetry_bif("telemetryDetach", &[]).is_none());
        assert!(generate_telemetry_bif("telemetryNope", &params(&["X"])).is_none());
    }
}
