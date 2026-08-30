// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List primitive implementations (BT-419).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Lists are Erlang linked lists — fast prepend, sequential access.
//! Complex operations delegate to `beamtalk_list` helper module.

use super::{build_error_doc, call_p0_self, call_self_p0, param};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

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
        // BT-3021: `first`/`last` raise `empty_collection`, not
        // `does_not_understand` — the receiver *does* understand the selector,
        // it just has no element to answer. `rest` is deliberately total (see
        // below), so only the element accessors raise here.
        "first" => {
            let error = build_error_doc(
                "empty_collection",
                "List",
                "first",
                "List is empty; guard with `isEmpty` or use `detect:ifNone:`",
            );
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
        // BT-3021: `rest` is a *subsequence* op, not an element accessor — it
        // has a well-defined total answer on empty (the empty list), matching
        // `drop:`/`take:`. Only `first`/`last`/`at:` raise on empty.
        "rest" => Some(Document::Str(
            "case Self of \
             <[_H|T]> when 'true' -> T \
             <[]> when 'true' -> [] \
             end",
        )),
        "last" => {
            let error = build_error_doc(
                "empty_collection",
                "List",
                "last",
                "List is empty; guard with `isEmpty` or use `detect:ifNone:`",
            );
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
        // BT-2997: honours an `equals:` override on the elements. Takes
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
        // BT-2997: `lists:usort/1` deduplicates by term order (`==`), which
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
        "join" => Some(Document::Str("call 'erlang':'iolist_to_binary'(Self)")),
        "join:" => {
            let p0 = param(params, 0, "_Sep");
            Some(docvec![
                "call 'erlang':'iolist_to_binary'(call 'lists':'join'(",
                leaf::var(p0.to_string()),
                ", Self))"
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
    use super::*;

    // Access ops

    #[test]
    fn test_size() {
        let result = doc_to_string(generate_list_bif("size", &[]));
        assert_eq!(result, Some("call 'erlang':'length'(Self)".to_string()));
    }

    #[test]
    fn test_is_empty() {
        let result = doc_to_string(generate_list_bif("isEmpty", &[]));
        assert_eq!(result, Some("call 'erlang':'=:='(Self, [])".to_string()));
    }

    #[test]
    fn test_first() {
        let result = doc_to_string(generate_list_bif("first", &[]));
        let output = result.expect("first should produce code");
        assert!(output.contains("case Self of"));
        assert!(output.contains("<[H|_T]> when 'true' -> H"));
        // BT-3021: empty-collection access is not a dispatch failure.
        assert!(output.contains("'empty_collection'"));
        assert!(!output.contains("'does_not_understand'"));
        assert!(output.contains("'first'"));
    }

    #[test]
    fn test_rest() {
        let result = doc_to_string(generate_list_bif("rest", &[]));
        assert_eq!(
            result,
            Some(
                "case Self of \
                 <[_H|T]> when 'true' -> T \
                 <[]> when 'true' -> [] \
                 end"
                .to_string()
            )
        );
    }

    #[test]
    fn test_last() {
        let result = doc_to_string(generate_list_bif("last", &[]));
        let output = result.expect("last should produce code");
        assert!(output.contains("case Self of"));
        assert!(output.contains("call 'lists':'last'(Self)"));
        // BT-3021: empty-collection access is not a dispatch failure.
        assert!(output.contains("'empty_collection'"));
        assert!(!output.contains("'does_not_understand'"));
        assert!(output.contains("'last'"));
    }

    #[test]
    fn test_at() {
        let result = doc_to_string(generate_list_bif("at:", &["N".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'at'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_includes() {
        // BT-2997: routes through `beamtalk_equality:member/2`, not bare
        // `lists:member/2`, so an `equals:` override on the element class is
        // honoured. That helper still takes `lists:member/2` first, so the
        // positive case costs the same as before.
        let result = doc_to_string(generate_list_bif("includes:", &["Item".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_equality':'member'(Item, Self)".to_string())
        );
    }

    #[test]
    fn test_sort() {
        let result = doc_to_string(generate_list_bif("sort", &[]));
        assert_eq!(result, Some("call 'lists':'sort'(Self)".to_string()));
    }

    #[test]
    fn test_sort_with() {
        let result = doc_to_string(generate_list_bif("sort:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'sort_with'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_reversed() {
        let result = doc_to_string(generate_list_bif("reversed", &[]));
        assert_eq!(result, Some("call 'lists':'reverse'(Self)".to_string()));
    }

    #[test]
    fn test_unique() {
        // BT-2997: `lists:usort/1` deduplicates by term order (`==`), which
        // collapses `1` and `1.0`. `beamtalk_list:unique/1` sorts the same way
        // but deduplicates with `=:=`.
        let result = doc_to_string(generate_list_bif("unique", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'unique'(Self)".to_string())
        );
    }

    // Iteration ops

    #[test]
    fn test_detect() {
        let result = doc_to_string(generate_list_bif("detect:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'detect'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_detect_if_none() {
        let result = doc_to_string(generate_list_bif(
            "detect:ifNone:",
            &["Block".to_string(), "Default".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'detect_if_none'(Self, Block, Default)".to_string())
        );
    }

    #[test]
    fn test_do() {
        let result = doc_to_string(generate_list_bif("do:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'do'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_collect() {
        let result = doc_to_string(generate_list_bif("collect:", &["Block".to_string()]));
        assert_eq!(result, Some("call 'lists':'map'(Block, Self)".to_string()));
    }

    #[test]
    fn test_select() {
        let result = doc_to_string(generate_list_bif("select:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'lists':'filter'(Block, Self)".to_string())
        );
    }

    #[test]
    fn test_reject() {
        let result = doc_to_string(generate_list_bif("reject:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'reject'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_inject_into() {
        let result = doc_to_string(generate_list_bif(
            "inject:into:",
            &["Init".to_string(), "Block".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_collection':'inject_into'(Self, Init, Block)".to_string())
        );
    }

    #[test]
    fn test_take() {
        let result = doc_to_string(generate_list_bif("take:", &["N".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'take'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_drop() {
        let result = doc_to_string(generate_list_bif("drop:", &["N".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'drop'(Self, N)".to_string())
        );
    }

    #[test]
    fn test_flatten() {
        let result = doc_to_string(generate_list_bif("flatten", &[]));
        assert_eq!(result, Some("call 'lists':'flatten'(Self)".to_string()));
    }

    #[test]
    fn test_flat_map() {
        let result = doc_to_string(generate_list_bif("flatMap:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'lists':'flatmap'(Block, Self)".to_string())
        );
    }

    #[test]
    fn test_count() {
        let result = doc_to_string(generate_list_bif("count:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'erlang':'length'(call 'lists':'filter'(Block, Self))".to_string())
        );
    }

    #[test]
    fn test_any_satisfy() {
        let result = doc_to_string(generate_list_bif("anySatisfy:", &["Block".to_string()]));
        assert_eq!(result, Some("call 'lists':'any'(Block, Self)".to_string()));
    }

    #[test]
    fn test_all_satisfy() {
        let result = doc_to_string(generate_list_bif("allSatisfy:", &["Block".to_string()]));
        assert_eq!(result, Some("call 'lists':'all'(Block, Self)".to_string()));
    }

    // Misc ops

    #[test]
    fn test_zip() {
        let result = doc_to_string(generate_list_bif("zip:", &["Other".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'zip'(Self, Other)".to_string())
        );
    }

    #[test]
    fn test_group_by() {
        let result = doc_to_string(generate_list_bif("groupBy:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'group_by'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_partition() {
        let result = doc_to_string(generate_list_bif("partition:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'partition'(Self, Block)".to_string())
        );
    }

    #[test]
    fn test_take_while() {
        let result = doc_to_string(generate_list_bif("takeWhile:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'lists':'takewhile'(Block, Self)".to_string())
        );
    }

    #[test]
    fn test_drop_while() {
        let result = doc_to_string(generate_list_bif("dropWhile:", &["Block".to_string()]));
        assert_eq!(
            result,
            Some("call 'lists':'dropwhile'(Block, Self)".to_string())
        );
    }

    #[test]
    fn test_intersperse() {
        let result = doc_to_string(generate_list_bif("intersperse:", &["Sep".to_string()]));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'intersperse'(Self, Sep)".to_string())
        );
    }

    #[test]
    fn test_add_first() {
        let result = doc_to_string(generate_list_bif("addFirst:", &["Item".to_string()]));
        assert_eq!(result, Some("[Item|Self]".to_string()));
    }

    #[test]
    fn test_add() {
        let result = doc_to_string(generate_list_bif("add:", &["Item".to_string()]));
        assert_eq!(
            result,
            Some("call 'erlang':'++'(Self, [Item|[]])".to_string())
        );
    }

    #[test]
    fn test_concat() {
        let result = doc_to_string(generate_list_bif("++", &["Other".to_string()]));
        assert_eq!(result, Some("call 'erlang':'++'(Self, Other)".to_string()));
    }

    #[test]
    fn test_from_to() {
        let result = doc_to_string(generate_list_bif(
            "from:to:",
            &["Start".to_string(), "End".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_list':'from_to'(Self, Start, End)".to_string())
        );
    }

    #[test]
    fn test_print_string() {
        let result = doc_to_string(generate_list_bif("printString", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_primitive':'print_string'(Self)".to_string())
        );
    }

    #[test]
    fn test_stream() {
        let result = doc_to_string(generate_list_bif("stream", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_stream':'on'(Self)".to_string())
        );
    }

    #[test]
    fn test_at_random() {
        let result = doc_to_string(generate_list_bif("atRandom", &[]));
        assert_eq!(
            result,
            Some("call 'beamtalk_random':'atRandom'(Self)".to_string())
        );
    }

    #[test]
    fn test_join() {
        let result = doc_to_string(generate_list_bif("join", &[]));
        assert_eq!(
            result,
            Some("call 'erlang':'iolist_to_binary'(Self)".to_string())
        );
    }

    #[test]
    fn test_join_with_sep() {
        let result = doc_to_string(generate_list_bif("join:", &["Sep".to_string()]));
        assert_eq!(
            result,
            Some("call 'erlang':'iolist_to_binary'(call 'lists':'join'(Sep, Self))".to_string())
        );
    }

    #[test]
    fn test_with_all_factory() {
        let result = doc_to_string(generate_list_bif("withAll:", &["List".to_string()]));
        assert_eq!(result, Some("List".to_string()));
    }

    // Edge cases

    #[test]
    fn test_unknown_selector_returns_none() {
        assert_eq!(doc_to_string(generate_list_bif("notAMethod", &[])), None);
    }
}
