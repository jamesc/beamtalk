// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List primitive implementations (BT-419).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Lists are Erlang linked lists — fast prepend, sequential access.
//! Complex operations delegate to `beamtalk_list` helper module.

use super::super::document::Document;
use super::{build_dnu_error_doc, call_p0_self, call_self_p0, param};
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
        "at:" => Some(call_self_p0("beamtalk_list", "at", param(params, 0, "_N"))),
        "includes:" => Some(call_p0_self("lists", "member", param(params, 0, "_Item"))),
        "sort" => Some(Document::Str("call 'lists':'sort'(Self)")),
        "sort:" => Some(call_self_p0(
            "beamtalk_list",
            "sort_with",
            param(params, 0, "_Block"),
        )),
        "reversed" => Some(Document::Str("call 'lists':'reverse'(Self)")),
        "unique" => Some(Document::Str("call 'lists':'usort'(Self)")),
        _ => None,
    }
}

fn generate_list_iteration_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Block");
    match selector {
        "detect:" => Some(call_self_p0("beamtalk_list", "detect", p0)),
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
        "do:" => Some(call_self_p0("beamtalk_list", "do", p0)),
        "collect:" => Some(call_p0_self("lists", "map", p0)),
        "select:" => Some(call_p0_self("lists", "filter", p0)),
        "reject:" => Some(call_self_p0("beamtalk_list", "reject", p0)),
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
        "take:" => Some(call_self_p0("beamtalk_list", "take", p0)),
        "drop:" => Some(call_self_p0("beamtalk_list", "drop", p0)),
        "flatten" => Some(Document::Str("call 'lists':'flatten'(Self)")),
        "flatMap:" => Some(call_p0_self("lists", "flatmap", p0)),
        "count:" => Some(docvec![
            "call 'erlang':'length'(call 'lists':'filter'(",
            p0.to_string(),
            ", Self))",
        ]),
        "anySatisfy:" => Some(call_p0_self("lists", "any", p0)),
        "allSatisfy:" => Some(call_p0_self("lists", "all", p0)),
        _ => None,
    }
}

fn generate_list_misc_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "zip:" => Some(call_self_p0(
            "beamtalk_list",
            "zip",
            param(params, 0, "_Other"),
        )),
        "groupBy:" => Some(call_self_p0(
            "beamtalk_list",
            "group_by",
            param(params, 0, "_Block"),
        )),
        "partition:" => Some(call_self_p0(
            "beamtalk_list",
            "partition",
            param(params, 0, "_Block"),
        )),
        "takeWhile:" => Some(call_p0_self(
            "lists",
            "takewhile",
            param(params, 0, "_Block"),
        )),
        "dropWhile:" => Some(call_p0_self(
            "lists",
            "dropwhile",
            param(params, 0, "_Block"),
        )),
        "intersperse:" => Some(call_self_p0(
            "beamtalk_list",
            "intersperse",
            param(params, 0, "_Sep"),
        )),
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
        "++" => Some(call_self_p0("erlang", "++", param(params, 0, "_Other"))),
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
