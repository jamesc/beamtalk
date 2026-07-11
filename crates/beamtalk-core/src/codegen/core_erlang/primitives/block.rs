// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::document::Document;

/// Block primitive implementations.
pub(crate) fn generate_block_bif(selector: &str, _params: &[String]) -> Option<Document<'static>> {
    match selector {
        "arity" => {
            // erlang:fun_info(Self, arity) returns {arity, N}
            Some(Document::Str(
                "let <ArityTuple> = call 'erlang':'fun_info'(Self, 'arity') in \
                 call 'erlang':'element'(2, ArityTuple)",
            ))
        }
        // valueWithArguments: is a structural intrinsic (BT-2803) handled at
        // the call site (see intrinsics.rs's
        // try_generate_block_value_with_arguments_keyword /
        // generate_block_value_with_arguments_call_runtime_discriminated),
        // like on:do: and ensure: below — not here.
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
    fn test_unknown_selector_returns_none() {
        assert!(generate_block_bif("on:do:", &[]).is_none());
        assert!(generate_block_bif("ensure:", &[]).is_none());
        assert!(generate_block_bif("value", &[]).is_none());
        // BT-2803: valueWithArguments: is now a call-site-intercepted
        // intrinsic (intrinsics.rs), no longer a bare-primitive bif.
        assert!(generate_block_bif("valueWithArguments:", &[]).is_none());
    }
}
