// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Lists are Erlang linked lists — fast prepend, sequential access.
//! Complex operations delegate to `beamtalk_list` helper module.

use super::{call_p0_self, call_self_p0, param};
use crate::core_erlang::errors::{BeamtalkErrorChain, ErrorBinding};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// List primitive implementations.
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

/// The `empty_collection` raise of `first`/`last` on an empty list:
/// `new → with_selector → with_hint → raise`.
fn empty_list_raise_doc(selector: &'static str) -> Document<'static> {
    docvec![
        BeamtalkErrorChain::new("empty_collection", leaf::atom("List"))
            .selector(leaf::atom(selector))
            .hint(leaf::binary_lit(
                "List is empty; guard with `isEmpty` or use `detect:ifNone:`"
            ))
            .build(
                vec![
                    ErrorBinding::plain("Error0"),
                    ErrorBinding::plain("Error1"),
                    ErrorBinding::plain("Error2"),
                ],
                Document::Str(" "),
            ),
        "call 'beamtalk_error':'raise'(Error2)",
    ]
}

fn generate_list_access_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'length'(Self)")),
        "isEmpty" => Some(Document::Str("call 'erlang':'=:='(Self, [])")),
        // `first`/`last` raise `empty_collection`, not
        // `does_not_understand` — the receiver *does* understand the selector,
        // it just has no element to answer. `rest` is deliberately total (see
        // below), so only the element accessors raise here.
        "first" => {
            let error = empty_list_raise_doc("first");
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
        // `rest` is a *subsequence* op, not an element accessor — it
        // has a well-defined total answer on empty (the empty list), matching
        // `drop:`/`take:`. Only `first`/`last`/`at:` raise on empty.
        "rest" => Some(Document::Str(
            "case Self of \
             <[_H|T]> when 'true' -> T \
             <[]> when 'true' -> [] \
             end",
        )),
        "last" => {
            let error = empty_list_raise_doc("last");
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
        // Honours an `equals:` override on the elements. Takes
        // `lists:member/2` first and only dispatches on a raw miss, so the
        // positive case keeps the BIF's speed.
        "includes:" => Some(call_p0_self(
            "beamtalk_equality",
            "member",
            param(params, 0, "_Item"),
        )),
        "sort" => Some(Document::Str("call 'lists':'sort'(Self)")),
        "sort:" => Some(call_self_p0(
            "beamtalk_list",
            "sort_with",
            param(params, 0, "_Block"),
        )),
        "reversed" => Some(Document::Str("call 'lists':'reverse'(Self)")),
        // `lists:usort/1` deduplicates by term order (`==`), which
        // collapses `1` and `1.0`. Beamtalk element identity is `=:=`.
        "unique" => Some(Document::Str("call 'beamtalk_list':'unique'(Self)")),
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
                leaf::var(p0.to_string()),
                ", ",
                leaf::var(p1.to_string()),
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
                leaf::var(p0.to_string()),
                ", ",
                leaf::var(p1.to_string()),
                ")"
            ])
        }
        "take:" => Some(call_self_p0("beamtalk_list", "take", p0)),
        "drop:" => Some(call_self_p0("beamtalk_list", "drop", p0)),
        "flatten" => Some(Document::Str("call 'lists':'flatten'(Self)")),
        "flatMap:" => Some(call_p0_self("lists", "flatmap", p0)),
        "count:" => Some(docvec![
            "call 'erlang':'length'(call 'lists':'filter'(",
            leaf::var(p0.to_string()),
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
            Some(docvec!["[", leaf::var(p0.to_string()), "|Self]"])
        }
        "add:" => {
            let p0 = param(params, 0, "_Item");
            Some(docvec![
                "call 'erlang':'++'(Self, [",
                leaf::var(p0.to_string()),
                "|[]])"
            ])
        }
        "++" => Some(call_self_p0("erlang", "++", param(params, 0, "_Other"))),
        "from:to:" => {
            let p0 = param(params, 0, "_Start");
            let p1 = param(params, 1, "_End");
            Some(docvec![
                "call 'beamtalk_list':'from_to'(Self, ",
                leaf::var(p0.to_string()),
                ", ",
                leaf::var(p1.to_string()),
                ")",
            ])
        }
        "printString" => Some(super::PRINT_STRING),
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        "atRandom" => Some(Document::Str("call 'beamtalk_random':'atRandom'(Self)")),
        // BT-4170: elements are tagged `String`s — join strictly through
        // `beamtalk_string:join/2` (unwraps, assembles, wraps).
        "join" => Some(Document::Str("call 'beamtalk_string':'join'(Self, 'join')")),
        "join:" => {
            let p0 = param(params, 0, "_Sep");
            Some(docvec![
                "call 'beamtalk_string':'join'(call 'lists':'join'(",
                leaf::var(p0.to_string()),
                ", Self), 'join:')",
            ])
        }
        // Class-side factory: List class withAll: list is identity (list is already a List)
        "withAll:" => {
            let p0 = param(params, 0, "_List");
            Some(leaf::var(p0.to_string()))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::doc_to_string;
    use super::super::{assert_bif_none_table, assert_bif_table};
    use super::*;

    #[test]
    fn test_first() {
        let result = doc_to_string(generate_list_bif("first", &[]));
        let output = result.expect("first should produce code");
        assert!(output.contains("case Self of"));
        assert!(output.contains("<[H|_T]> when 'true' -> H"));
        // Empty-collection access is not a dispatch failure.
        assert!(output.contains("'empty_collection'"));
        assert!(!output.contains("'does_not_understand'"));
        assert!(output.contains("'first'"));
    }

    #[test]
    fn test_last() {
        let result = doc_to_string(generate_list_bif("last", &[]));
        let output = result.expect("last should produce code");
        assert!(output.contains("case Self of"));
        assert!(output.contains("call 'lists':'last'(Self)"));
        // Empty-collection access is not a dispatch failure.
        assert!(output.contains("'empty_collection'"));
        assert!(!output.contains("'does_not_understand'"));
        assert!(output.contains("'last'"));
    }

    #[test]
    #[allow(clippy::too_many_lines)] // one contiguous table; splitting hurts readability
    fn generate_list_bif_table() {
        assert_bif_table(
            generate_list_bif,
            &[
                // Access ops
                ("test_size", "size", &[], "call 'erlang':'length'(Self)"),
                (
                    "test_is_empty",
                    "isEmpty",
                    &[],
                    "call 'erlang':'=:='(Self, [])",
                ),
                (
                    "test_rest",
                    "rest",
                    &[],
                    "case Self of \
                 <[_H|T]> when 'true' -> T \
                 <[]> when 'true' -> [] \
                 end",
                ),
                (
                    "test_at",
                    "at:",
                    &["N"],
                    "call 'beamtalk_list':'at'(Self, N)",
                ),
                // Routes through `beamtalk_equality:member/2`, not bare
                // `lists:member/2`, so an `equals:` override on the element class is
                // honoured. That helper still takes `lists:member/2` first, so the
                // positive case costs the same as before.
                (
                    "test_includes",
                    "includes:",
                    &["Item"],
                    "call 'beamtalk_equality':'member'(Item, Self)",
                ),
                ("test_sort", "sort", &[], "call 'lists':'sort'(Self)"),
                (
                    "test_sort_with",
                    "sort:",
                    &["Block"],
                    "call 'beamtalk_list':'sort_with'(Self, Block)",
                ),
                (
                    "test_reversed",
                    "reversed",
                    &[],
                    "call 'lists':'reverse'(Self)",
                ),
                // collapses `1` and `1.0`. `beamtalk_list:unique/1` sorts the same way
                // but deduplicates with `=:=`.
                (
                    "test_unique",
                    "unique",
                    &[],
                    "call 'beamtalk_list':'unique'(Self)",
                ),
                // Iteration ops
                (
                    "test_detect",
                    "detect:",
                    &["Block"],
                    "call 'beamtalk_list':'detect'(Self, Block)",
                ),
                (
                    "test_detect_if_none",
                    "detect:ifNone:",
                    &["Block", "Default"],
                    "call 'beamtalk_list':'detect_if_none'(Self, Block, Default)",
                ),
                (
                    "test_do",
                    "do:",
                    &["Block"],
                    "call 'beamtalk_list':'do'(Self, Block)",
                ),
                (
                    "test_collect",
                    "collect:",
                    &["Block"],
                    "call 'lists':'map'(Block, Self)",
                ),
                (
                    "test_select",
                    "select:",
                    &["Block"],
                    "call 'lists':'filter'(Block, Self)",
                ),
                (
                    "test_reject",
                    "reject:",
                    &["Block"],
                    "call 'beamtalk_list':'reject'(Self, Block)",
                ),
                (
                    "test_inject_into",
                    "inject:into:",
                    &["Init", "Block"],
                    "call 'beamtalk_collection':'inject_into'(Self, Init, Block)",
                ),
                (
                    "test_take",
                    "take:",
                    &["N"],
                    "call 'beamtalk_list':'take'(Self, N)",
                ),
                (
                    "test_drop",
                    "drop:",
                    &["N"],
                    "call 'beamtalk_list':'drop'(Self, N)",
                ),
                (
                    "test_flatten",
                    "flatten",
                    &[],
                    "call 'lists':'flatten'(Self)",
                ),
                (
                    "test_flat_map",
                    "flatMap:",
                    &["Block"],
                    "call 'lists':'flatmap'(Block, Self)",
                ),
                (
                    "test_count",
                    "count:",
                    &["Block"],
                    "call 'erlang':'length'(call 'lists':'filter'(Block, Self))",
                ),
                (
                    "test_any_satisfy",
                    "anySatisfy:",
                    &["Block"],
                    "call 'lists':'any'(Block, Self)",
                ),
                (
                    "test_all_satisfy",
                    "allSatisfy:",
                    &["Block"],
                    "call 'lists':'all'(Block, Self)",
                ),
                // Misc ops
                (
                    "test_zip",
                    "zip:",
                    &["Other"],
                    "call 'beamtalk_list':'zip'(Self, Other)",
                ),
                (
                    "test_group_by",
                    "groupBy:",
                    &["Block"],
                    "call 'beamtalk_list':'group_by'(Self, Block)",
                ),
                (
                    "test_partition",
                    "partition:",
                    &["Block"],
                    "call 'beamtalk_list':'partition'(Self, Block)",
                ),
                (
                    "test_take_while",
                    "takeWhile:",
                    &["Block"],
                    "call 'lists':'takewhile'(Block, Self)",
                ),
                (
                    "test_drop_while",
                    "dropWhile:",
                    &["Block"],
                    "call 'lists':'dropwhile'(Block, Self)",
                ),
                (
                    "test_intersperse",
                    "intersperse:",
                    &["Sep"],
                    "call 'beamtalk_list':'intersperse'(Self, Sep)",
                ),
                ("test_add_first", "addFirst:", &["Item"], "[Item|Self]"),
                (
                    "test_add",
                    "add:",
                    &["Item"],
                    "call 'erlang':'++'(Self, [Item|[]])",
                ),
                (
                    "test_concat",
                    "++",
                    &["Other"],
                    "call 'erlang':'++'(Self, Other)",
                ),
                (
                    "test_from_to",
                    "from:to:",
                    &["Start", "End"],
                    "call 'beamtalk_list':'from_to'(Self, Start, End)",
                ),
                (
                    "test_print_string",
                    "printString",
                    &[],
                    "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_primitive':'print_string'(Self))",
                ),
                (
                    "test_stream",
                    "stream",
                    &[],
                    "call 'beamtalk_stream':'on'(Self)",
                ),
                (
                    "test_at_random",
                    "atRandom",
                    &[],
                    "call 'beamtalk_random':'atRandom'(Self)",
                ),
                (
                    "test_join",
                    "join",
                    &[],
                    "call 'beamtalk_string':'join'(Self, 'join')",
                ),
                (
                    "test_join_with_sep",
                    "join:",
                    &["Sep"],
                    "call 'beamtalk_string':'join'(call 'lists':'join'(Sep, Self), 'join:')",
                ),
                ("test_with_all_factory", "withAll:", &["List"], "List"),
            ],
        );
    }

    #[test]
    fn generate_list_bif_none_table() {
        assert_bif_none_table(
            generate_list_bif,
            &[
                // Edge cases
                ("test_unknown_selector_returns_none", "notAMethod", &[]),
            ],
        );
    }
}
