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
use beamtalk_core::ast::Block;

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

    /// The miss-path helper call of a *read* (`get`, `get_late`, `has`): the
    /// 2-arity form at method level, the 3-arity captured-fallback form
    /// (ADR 0130 §5) inside a block literal that bound a capture, so a block
    /// carried to another process answers the values its class variables had
    /// when it was created. Writes never take a capture (`put`/`clear`).
    pub(super) fn class_var_read_helper_call_doc(
        &self,
        function: &str,
        mut args: Vec<Document<'static>>,
    ) -> Document<'static> {
        if let Some(capture) = &self.class_var_capture {
            args.push(leaf::var(capture.clone()));
        }
        Self::class_var_helper_call_doc(function, args)
    }

    /// `let Snap = call 'beamtalk_class_vars':'snapshot'() in ` — the catch
    /// boundary's entry half (ADR 0130 §4), emitted before every compiled
    /// `on:do:`'s `try`.
    pub(super) fn class_var_snapshot_let_doc(snapshot_var: &str) -> Document<'static> {
        docvec![
            "let ",
            leaf::var(snapshot_var.to_string()),
            " = call 'beamtalk_class_vars':'snapshot'() in "
        ]
    }

    /// `do call 'beamtalk_class_vars':'restore'(Snap) ` — the catch boundary's
    /// exit half (ADR 0130 §4), the first statement of a catch's non-NLR arm.
    pub(super) fn class_var_restore_doc(snapshot_var: &str) -> Document<'static> {
        docvec![
            "do call 'beamtalk_class_vars':'restore'(",
            leaf::var(snapshot_var.to_string()),
            ") "
        ]
    }

    /// `let CVCapture = call 'beamtalk_class_vars':'capture'(ClassSelf, Outer) in `
    /// — the block-creation capture (ADR 0130 §5); `outer` is the enclosing
    /// block's capture variable, or `'none'` at method level.
    pub(super) fn class_var_capture_let_doc(
        capture_var: &str,
        outer: Option<&str>,
    ) -> Document<'static> {
        docvec![
            "let ",
            leaf::var(capture_var.to_string()),
            " = call 'beamtalk_class_vars':'capture'(ClassSelf, ",
            match outer {
                Some(var) => leaf::var(var.to_string()),
                None => leaf::atom("none"),
            },
            ") in "
        ]
    }

    /// Whether `block` (including every nested block literal) reads a class
    /// variable of the class being compiled: `self.n` outside an assignment
    /// target, or `self hasField: ...`. A write-only block reads nothing and
    /// binds no capture ("blocks that read no class variable bind nothing").
    pub(super) fn block_reads_class_var(&self, block: &Block) -> bool {
        use beamtalk_core::ast::Expression;
        use beamtalk_core::ast::well_known::WellKnownSelector;
        let is_self = |e: &Expression| matches!(e, Expression::Identifier(id) if id.name == "self");
        let mut assigned_targets: Vec<beamtalk_core::source_analysis::Span> = Vec::new();
        let mut found = false;
        for stmt in &block.body {
            beamtalk_core::ast_walker::walk_expression(&stmt.expression, &mut |e| match e {
                Expression::Assignment { target, .. } => {
                    if let Expression::FieldAccess { receiver, .. } = target.as_ref() {
                        if is_self(receiver) {
                            assigned_targets.push(target.span());
                        }
                    }
                }
                Expression::FieldAccess {
                    receiver, field, ..
                } => {
                    if is_self(receiver)
                        && self.class_var_names().contains(field.name.as_str())
                        && !assigned_targets.contains(&e.span())
                    {
                        found = true;
                    }
                }
                Expression::MessageSend {
                    receiver, selector, ..
                } => {
                    if is_self(receiver)
                        && selector.well_known() == Some(WellKnownSelector::HasField)
                    {
                        found = true;
                    }
                }
                _ => {}
            });
        }
        found
    }

    /// Runs `build_fun` (which generates the `fun` of block literal `block`) inside
    /// the block's class-variable capture scope and binds the capture around the
    /// result: `let CVCapture = capture(ClassSelf, Outer) in fun (...) -> ... end`.
    ///
    /// The capture is generator state for the whole lexical extent of the
    /// closure, so every access lowered inside it, in a straight-line statement,
    /// a conditional arm, a loop or fold body, an `on:do:` arm or a nested
    /// inlined block alike, takes the same miss-path fallback; a nested closure
    /// that reads binds its own capture with this one as `Outer`, so a block
    /// created abroad inherits its parent's. Not in a class method, or for a
    /// block that reads no class variable, nothing is bound.
    pub(super) fn with_class_var_capture(
        &mut self,
        block: &Block,
        build_fun: impl FnOnce(&mut Self) -> super::Result<Document<'static>>,
    ) -> super::Result<Document<'static>> {
        if !self.in_class_method() || !self.block_reads_class_var(block) {
            return build_fun(self);
        }
        let capture_var = self.fresh_temp_var("CVCapture");
        let outer = self.class_var_capture.replace(capture_var.clone());
        let built = build_fun(self);
        self.class_var_capture.clone_from(&outer);
        let fun = built?;
        Ok(docvec![
            Self::class_var_capture_let_doc(&capture_var, outer.as_deref()),
            fun
        ])
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
        let fallback = || self.class_var_read_helper_call_doc(function, vec![leaf::atom(name)]);
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
