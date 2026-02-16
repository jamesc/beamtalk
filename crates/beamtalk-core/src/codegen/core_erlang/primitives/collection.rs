// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Collection primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Default implementations for abstract Collection protocol methods.
//! These dispatch `do:` on the actual Self value, which routes to
//! the concrete subclass (Set, Dictionary, Tuple).

use super::super::document::Document;
use crate::docvec;

/// Collection primitive implementations.
pub(crate) fn generate_collection_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "includes:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'includes'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "inject:into:" => {
            let p0 = params.first().map_or("_Initial", String::as_str);
            let p1 = params.get(1).map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'inject_into'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "collect:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'collect'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "select:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'select'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "reject:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'reject'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "detect:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'detect'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "detect:ifNone:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            let p1 = params.get(1).map_or("_NoneBlock", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'detect_if_none'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "anySatisfy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'any_satisfy'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "allSatisfy:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_collection_ops':'all_satisfy'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        _ => None,
    }
}
