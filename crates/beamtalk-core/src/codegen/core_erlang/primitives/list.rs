// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List primitive implementations (BT-419).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Lists are Erlang linked lists — fast prepend, sequential access.
//! Complex operations delegate to `beamtalk_list` helper module.

use super::super::document::Document;
use super::{build_dnu_error_doc, param};
use crate::docvec;

/// List primitive implementations (BT-419).
pub(crate) fn generate_list_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Access and ordering
        "size" | "isEmpty" | "first" | "rest" | "last" | "at:" | "includes:" | "sort" | "sort:"
        | "reversed" | "unique" => generate_list_access_bif(selector, params),
        // Search, iteration, functional
        "detect:" | "detect:ifNone:" | "do:" | "collect:" | "select:" | "reject:"
        | "inject:into:" | "take:" | "drop:" | "flatten" | "flatMap:" | "count:"
        | "anySatisfy:" | "allSatisfy:" => generate_list_iteration_bif(selector, params),
        // Advanced, mutation, concatenation, subsequence, misc
        _ => generate_list_misc_bif(selector, params),
    }
}

fn generate_list_access_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'length'(Self)")),
        "isEmpty" => Some(Document::Str("call 'erlang':'=:='(Self, [])")),
        "first" => {
            let error =
                build_dnu_error_doc("List", "first", "Cannot get first element of empty list");
            Some(docvec![
                "case Self of \
                 <[H|_T]> when 'true' -> H \
                 <[]> when 'true' -> \
                   ",
                error,
                " \
                 end",
            ])
        }
        "rest" => Some(Document::Str(
            "case Self of \
             <[_H|T]> when 'true' -> T \
             <[]> when 'true' -> [] \
             end",
        )),
        "last" => {
            let error =
                build_dnu_error_doc("List", "last", "Cannot get last element of empty list");
            Some(docvec![
                "case Self of \
                 <[]> when 'true' -> \
                   ",
                error,
                " \
                 <_> when 'true' -> \
                   call 'lists':'last'(Self) \
                 end",
            ])
        }
        "at:" => {
            let p0 = param(params, 0, "_N");
            Some(docvec![
                "call 'beamtalk_list':'at'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "includes:" => {
            let p0 = param(params, 0, "_Item");
            Some(docvec!["call 'lists':'member'(", p0.to_string(), ", Self)"])
        }
        "sort" => Some(Document::Str("call 'lists':'sort'(Self)")),
        "sort:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_list':'sort_with'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "reversed" => Some(Document::Str("call 'lists':'reverse'(Self)")),
        "unique" => Some(Document::Str("call 'lists':'usort'(Self)")),
        _ => None,
    }
}

fn generate_list_iteration_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Block");
    match selector {
        "detect:" => Some(docvec![
            "call 'beamtalk_list':'detect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "detect:ifNone:" => {
            let p1 = param(params, 1, "_Default");
            Some(docvec![
                "call 'beamtalk_list':'detect_if_none'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "do:" => Some(docvec![
            "call 'beamtalk_list':'do'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "collect:" => Some(docvec!["call 'lists':'map'(", p0.to_string(), ", Self)"]),
        "select:" => Some(docvec!["call 'lists':'filter'(", p0.to_string(), ", Self)"]),
        "reject:" => Some(docvec![
            "call 'beamtalk_list':'reject'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "inject:into:" => {
            let p1 = param(params, 1, "_Block");
            Some(docvec![
                "call 'beamtalk_collection':'inject_into'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "take:" => Some(docvec![
            "call 'beamtalk_list':'take'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "drop:" => Some(docvec![
            "call 'beamtalk_list':'drop'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "flatten" => Some(Document::Str("call 'lists':'flatten'(Self)")),
        "flatMap:" => Some(docvec![
            "call 'lists':'flatmap'(",
            p0.to_string(),
            ", Self)"
        ]),
        "count:" => Some(docvec![
            "call 'erlang':'length'(call 'lists':'filter'(",
            p0.to_string(),
            ", Self))",
        ]),
        "anySatisfy:" => Some(docvec!["call 'lists':'any'(", p0.to_string(), ", Self)"]),
        "allSatisfy:" => Some(docvec!["call 'lists':'all'(", p0.to_string(), ", Self)"]),
        _ => None,
    }
}

fn generate_list_misc_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "zip:" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec![
                "call 'beamtalk_list':'zip'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "groupBy:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_list':'group_by'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "partition:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'beamtalk_list':'partition'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "takeWhile:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'lists':'takewhile'(",
                p0.to_string(),
                ", Self)"
            ])
        }
        "dropWhile:" => {
            let p0 = param(params, 0, "_Block");
            Some(docvec![
                "call 'lists':'dropwhile'(",
                p0.to_string(),
                ", Self)"
            ])
        }
        "intersperse:" => {
            let p0 = param(params, 0, "_Sep");
            Some(docvec![
                "call 'beamtalk_list':'intersperse'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "addFirst:" => {
            let p0 = param(params, 0, "_Item");
            Some(docvec!["[", p0.to_string(), "|Self]"])
        }
        "add:" => {
            let p0 = param(params, 0, "_Item");
            Some(docvec![
                "call 'erlang':'++'(Self, [",
                p0.to_string(),
                "|[]])"
            ])
        }
        "++" => {
            let p0 = param(params, 0, "_Other");
            Some(docvec!["call 'erlang':'++'(Self, ", p0.to_string(), ")"])
        }
        "from:to:" => {
            let p0 = param(params, 0, "_Start");
            let p1 = param(params, 1, "_End");
            Some(docvec![
                "call 'beamtalk_list':'from_to'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "printString" => Some(super::PRINT_STRING),
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        "atRandom" => Some(Document::Str("call 'beamtalk_random':'atRandom'(Self)")),
        "join" => Some(Document::Str("call 'erlang':'iolist_to_binary'(Self)")),
        "join:" => {
            let p0 = param(params, 0, "_Sep");
            Some(docvec![
                "call 'erlang':'iolist_to_binary'(call 'lists':'join'(",
                p0.to_string(),
                ", Self))"
            ])
        }
        // Class-side factory: List class withAll: list is identity (list is already a List)
        "withAll:" => {
            let p0 = param(params, 0, "_List");
            Some(Document::String(p0.to_string()))
        }
        _ => None,
    }
}
