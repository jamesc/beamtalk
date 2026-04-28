// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Collection primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! BT-815: Most abstract Collection protocol methods are now self-hosted as
//! pure Beamtalk in `Collection.bt`.  The exception is `inject:into:`: the
//! pure-BT implementation requires local-variable mutation threading through
//! an abstract `do:` call, which the compiler does not yet support for
//! abstract-class methods (it generates `lists:foreach` rather than
//! `lists:foldl`).  `inject:into:` therefore remains a `@primitive` backed
//! by `beamtalk_collection:inject_into/3`.
//!
//! Concrete subclasses (List, Set, Dictionary) retain their own `@primitive`
//! overrides handled by `list.rs`, `misc.rs`, and `dictionary.rs`.

use super::super::document::Document;
use super::param;
use crate::docvec;

/// Collection primitive implementations.
pub(crate) fn generate_collection_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "inject:into:" => {
            let p0 = param(params, 0, "_Initial");
            let p1 = param(params, 1, "_Block");
            Some(docvec![
                "call 'beamtalk_collection':'inject_into'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::*;

    #[test]
    fn test_inject_into_with_params() {
        let result = doc_to_string(generate_collection_bif(
            "inject:into:",
            &["Initial".to_string(), "Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_collection':'inject_into'(Self, Initial, Block)".to_string())
        );
    }

    #[test]
    fn test_inject_into_uses_defaults_when_no_params() {
        let result = doc_to_string(generate_collection_bif("inject:into:", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_collection':'inject_into'(Self, _Initial, _Block)".to_string())
        );
    }

    #[test]
    fn test_unknown_selector_returns_none() {
        assert!(generate_collection_bif("do:", &[]).is_none());
        assert!(generate_collection_bif("collect:", &[]).is_none());
    }
}
