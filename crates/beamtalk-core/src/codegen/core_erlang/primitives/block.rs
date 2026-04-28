// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;
use super::param;
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
            let p0 = param(params, 0, "_Args");
            Some(docvec!["call 'erlang':'apply'(Self, ", p0.to_string(), ")"])
        }
        // on:do: and ensure: are structural intrinsics handled at the call site
        // (see control_flow/exception_handling.rs), not here.
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    #[test]
    fn test_arity() {
        let result = doc_to_string(generate_block_bif("arity", &[]));
        assert_eq!(
            result,
            Some(
                "let <ArityTuple> = call 'erlang':'fun_info'(Self, 'arity') in \
                 call 'erlang':'element'(2, ArityTuple)"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_value_with_arguments() {
        let result = doc_to_string(generate_block_bif(
            "valueWithArguments:",
            &["MyArgs".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'erlang':'apply'(Self, MyArgs)".to_string())
        );
    }

    #[test]
    fn test_value_with_arguments_uses_default_when_no_params() {
        let result = doc_to_string(generate_block_bif("valueWithArguments:", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'apply'(Self, _Args)".to_string())
        );
    }

    #[test]
    fn test_unknown_selector_returns_none() {
        assert!(generate_block_bif("on:do:", &[]).is_none());
        assert!(generate_block_bif("ensure:", &[]).is_none());
        assert!(generate_block_bif("value", &[]).is_none());
    }
}
