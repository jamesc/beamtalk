// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive BIF mappings for Protocol stdlib class (ADR 0068 Phase 2c).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "protocolXxx"` declarations in `stdlib/src/Protocol.bt`
//! to direct calls into `beamtalk_protocol_registry`.

use super::super::document::Document;
use crate::docvec;

/// Generates Core Erlang for a Protocol class method primitive.
///
/// These back the `@primitive "protocolXxx"` class method bodies in `Protocol.bt`.
/// Each maps to a direct call to `beamtalk_protocol_registry:Func(Arg)`.
pub fn generate_protocol_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "protocolRequiredMethods" => {
            let arg = params.first()?;
            Some(docvec![
                "call 'beamtalk_protocol_registry':'required_methods'(",
                arg.clone(),
                ")"
            ])
        }
        "protocolConformingClasses" => {
            let arg = params.first()?;
            // Returns class name atoms — runtime converts to class objects
            Some(docvec![
                "call 'beamtalk_protocol_registry':'conforming_classes'(",
                arg.clone(),
                ")"
            ])
        }
        "protocolIsProtocol" => {
            let arg = params.first()?;
            Some(docvec![
                "call 'beamtalk_protocol_registry':'is_protocol'(",
                arg.clone(),
                ")"
            ])
        }
        "protocolAllProtocols" => Some(Document::Str(
            "call 'beamtalk_protocol_registry':'all_protocol_names'()",
        )),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    #[test]
    fn test_protocol_required_methods() {
        let result = doc_to_string(generate_protocol_bif(
            "protocolRequiredMethods",
            &["ProtocolName".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_protocol_registry':'required_methods'(ProtocolName)".to_string())
        );
    }

    #[test]
    fn test_protocol_conforming_classes() {
        let result = doc_to_string(generate_protocol_bif(
            "protocolConformingClasses",
            &["ProtocolName".to_string()],
        ));
        assert_eq!(
            result,
            Some(
                "call 'beamtalk_protocol_registry':'conforming_classes'(ProtocolName)".to_string()
            )
        );
    }

    #[test]
    fn test_protocol_is_protocol() {
        let result = doc_to_string(generate_protocol_bif(
            "protocolIsProtocol",
            &["Name".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_protocol_registry':'is_protocol'(Name)".to_string())
        );
    }

    #[test]
    fn test_protocol_all_protocols() {
        let result = doc_to_string(generate_protocol_bif("protocolAllProtocols", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_protocol_registry':'all_protocol_names'()".to_string())
        );
    }

    #[test]
    fn test_unknown_protocol_selector() {
        let result = doc_to_string(generate_protocol_bif("unknownMethod", &[]));
        assert!(result.is_none());
    }
}
