// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-variable access lowering (ADR 0130 §2).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A class variable is read and written in place in the class process's
//! dictionary, so a class method's access to `self.n` is an expression like any
//! other: no `ClassVars` argument, no rebinding, no `{class_var_result, ..}`.
//!
//! Phase 0 gate 3 measured helper calls at 2.3x and the inlined form at 0.63x
//! of the cost of today's lexical `ClassVars` access, so the common case is
//! inlined (`erlang:get/1` on the class key plus `maps:find/2` / `maps:put/3`)
//! and every other case (key absent, read-only marker, name absent from the
//! map, an unassigned `late` variable) falls back to the one runtime owner,
//! `beamtalk_class_vars`, which holds the declared-set check and every
//! structured error. The key shape is owned by [`super::class_var_keys`]; the
//! key is derived from `ClassSelf`'s metaclass tag with a single `element/2`,
//! never by recovering the class name per access.
//!
//! `clearField:` and `hasField:` are rare and stay helper calls.

use super::CoreErlangGenerator;
use super::class_var_keys;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

impl CoreErlangGenerator {
    /// `call 'beamtalk_class_vars':'<function>'(ClassSelf, <args>..)`.
    pub(super) fn class_var_helper_call_doc(
        function: &str,
        args: Vec<Document<'static>>,
    ) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = vec![
            Document::Str("call 'beamtalk_class_vars':"),
            leaf::atom(function),
            Document::Str("("),
            leaf::var("ClassSelf"),
        ];
        for arg in args {
            parts.push(Document::Str(", "));
            parts.push(arg);
        }
        parts.push(Document::Str(")"));
        Document::Vec(parts)
    }

    /// Read of class variable `name`: the inlined hit path, the helper otherwise.
    ///
    /// ```erlang
    /// case call 'erlang':'get'({'$bt_class_vars', element(2, ClassSelf)}) of
    ///   <CVMap> when call 'erlang':'is_map'(CVMap) ->
    ///     case call 'maps':'find'('n', CVMap) of
    ///       <{'ok', CVVal}> when 'true' -> CVVal
    ///       <_> when 'true' -> call 'beamtalk_class_vars':'get'(ClassSelf, 'n')
    ///     end
    ///   <_> when 'true' -> call 'beamtalk_class_vars':'get'(ClassSelf, 'n')
    /// end
    /// ```
    ///
    /// A `late` variable (ADR 0124) additionally treats a stored `nil` as a
    /// miss, so `get_late/2` raises the same `uninitialized_state_error` for an
    /// unassigned (`nil` or absent) variable.
    pub(super) fn class_var_read_doc(&mut self, name: &str, late: bool) -> Document<'static> {
        let map_var = self.fresh_temp_var("CVMap");
        let val_var = self.fresh_temp_var("CVVal");
        let (function, hit_guard) = if late {
            (
                "get_late",
                docvec![
                    "call 'erlang':'=/='(",
                    leaf::var(val_var.clone()),
                    ", 'nil')"
                ],
            )
        } else {
            ("get", Document::Str("'true'"))
        };
        let fallback = || Self::class_var_helper_call_doc(function, vec![leaf::atom(name)]);
        docvec![
            "case call 'erlang':'get'(",
            class_var_keys::key_doc("ClassSelf"),
            ") of <",
            leaf::var(map_var.clone()),
            "> when call 'erlang':'is_map'(",
            leaf::var(map_var.clone()),
            ") -> case call 'maps':'find'(",
            leaf::atom(name),
            ", ",
            leaf::var(map_var),
            ") of <{'ok', ",
            leaf::var(val_var.clone()),
            "}> when ",
            hit_guard,
            " -> ",
            leaf::var(val_var),
            " <_> when 'true' -> ",
            fallback(),
            " end <_> when 'true' -> ",
            fallback(),
            " end"
        ]
    }

    /// Write of class variable `name` to the value `value_doc` evaluates to; the
    /// expression's own value is the assigned value (`self.n := v` answers `v`).
    ///
    /// ```erlang
    /// let CVVal = <value> in
    /// case call 'erlang':'get'({'$bt_class_vars', element(2, ClassSelf)}) of
    ///   <CVMap> when call 'erlang':'is_map'(CVMap) ->
    ///     let CVOld = call 'erlang':'put'(<key>, call 'maps':'put'('n', CVVal, CVMap)) in CVVal
    ///   <_> when 'true' -> call 'beamtalk_class_vars':'put'(ClassSelf, 'n', CVVal)
    /// end
    /// ```
    ///
    /// The helper is the miss path: it raises `class_state_unreachable` /
    /// `class_state_read_only` exactly as the runtime owns them.
    pub(super) fn class_var_write_doc(
        &mut self,
        name: &str,
        value_doc: Document<'static>,
    ) -> Document<'static> {
        let val_var = self.fresh_temp_var("CVVal");
        let map_var = self.fresh_temp_var("CVMap");
        let old_var = self.fresh_temp_var("CVOld");
        docvec![
            "let ",
            leaf::var(val_var.clone()),
            " = ",
            value_doc,
            " in case call 'erlang':'get'(",
            class_var_keys::key_doc("ClassSelf"),
            ") of <",
            leaf::var(map_var.clone()),
            "> when call 'erlang':'is_map'(",
            leaf::var(map_var.clone()),
            ") -> let ",
            leaf::var(old_var),
            " = call 'erlang':'put'(",
            class_var_keys::key_doc("ClassSelf"),
            ", call 'maps':'put'(",
            leaf::atom(name),
            ", ",
            leaf::var(val_var.clone()),
            ", ",
            leaf::var(map_var),
            ")) in ",
            leaf::var(val_var.clone()),
            " <_> when 'true' -> ",
            Self::class_var_helper_call_doc("put", vec![leaf::atom(name), leaf::var(val_var)]),
            " end"
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generator() -> CoreErlangGenerator {
        CoreErlangGenerator::new("test_module")
    }

    #[test]
    fn read_inlines_the_hit_path_and_falls_back_to_the_helper() {
        let doc = generator()
            .class_var_read_doc("count", false)
            .to_pretty_string();
        assert!(
            doc.starts_with(
                "case call 'erlang':'get'({'$bt_class_vars', call 'erlang':'element'(2, ClassSelf)}) of"
            ),
            "{doc}"
        );
        assert!(doc.contains("call 'maps':'find'('count', "), "{doc}");
        assert_eq!(
            doc.matches("call 'beamtalk_class_vars':'get'(ClassSelf, 'count')")
                .count(),
            2,
            "key-absent and name-absent both fall back to the helper: {doc}"
        );
        assert!(!doc.contains("class_name_from_tag"), "{doc}");
    }

    #[test]
    fn late_read_treats_nil_as_a_miss() {
        let doc = generator()
            .class_var_read_doc("cache", true)
            .to_pretty_string();
        assert!(doc.contains("when call 'erlang':'=/='("), "{doc}");
        assert!(doc.contains(", 'nil')"), "{doc}");
        assert_eq!(
            doc.matches("call 'beamtalk_class_vars':'get_late'(ClassSelf, 'cache')")
                .count(),
            2,
            "{doc}"
        );
    }

    #[test]
    fn write_puts_in_place_and_answers_the_value() {
        let doc = generator()
            .class_var_write_doc("count", Document::Str("42"))
            .to_pretty_string();
        assert!(doc.starts_with("let "), "{doc}");
        assert!(
            doc.contains(" = 42 in case call 'erlang':'get'({'$bt_class_vars', "),
            "{doc}"
        );
        assert!(
            doc.contains("call 'erlang':'put'({'$bt_class_vars', "),
            "{doc}"
        );
        assert!(doc.contains("call 'maps':'put'('count', "), "{doc}");
        assert!(
            doc.contains("call 'beamtalk_class_vars':'put'(ClassSelf, 'count', "),
            "{doc}"
        );
    }
}
