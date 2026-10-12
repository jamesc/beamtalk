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
//! never by recovering the class name per access, and is bound once per method
//! body (`let CVKey = {'$bt_class_vars', element(2, ClassSelf)} in ...`, BT-3719)
//! so an inlined access reuses it instead of rebuilding the tuple.
//!
//! `clearField:` and `hasField:` are rare and stay helper calls.

use super::CoreErlangGenerator;
use super::class_var_keys::{self, KeyScope};
use super::erlang_types::ErlangVar;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression, Identifier};
use beamtalk_core::semantic_analysis::block_facts::class_var_accesses;

/// The one emitter of a `call 'beamtalk_class_vars':'<function>'(<receiver>,
/// <args>..)` expression; every helper-call doc below goes through it.
/// `receiver` is the leading argument (`ClassSelf`), or `None` for the
/// receiver-less `snapshot`/`restore` boundary calls.
fn beamtalk_class_vars_call(
    function: &str,
    receiver: Option<Document<'static>>,
    args: Vec<Document<'static>>,
) -> Document<'static> {
    let mut parts: Vec<Document<'static>> = vec![
        Document::Str("call 'beamtalk_class_vars':"),
        leaf::atom(function),
        Document::Str("("),
    ];
    for (i, arg) in receiver.into_iter().chain(args).enumerate() {
        if i > 0 {
            parts.push(Document::Str(", "));
        }
        parts.push(arg);
    }
    parts.push(Document::Str(")"));
    Document::Vec(parts)
}

impl CoreErlangGenerator {
    /// `call 'beamtalk_class_vars':'<function>'(ClassSelf, <args>..)`.
    pub(super) fn class_var_helper_call_doc(
        function: &str,
        args: Vec<Document<'static>>,
    ) -> Document<'static> {
        beamtalk_class_vars_call(function, Some(leaf::var("ClassSelf")), args)
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
    /// `on:do:`'s `try` by its `ThreadedStmt::OnDoCatch` node.
    pub(super) fn class_var_snapshot_let_doc(snapshot_var: &ErlangVar) -> Document<'static> {
        docvec![
            "let ",
            leaf::var(snapshot_var.name()),
            " = ",
            beamtalk_class_vars_call("snapshot", None, vec![]),
            " in "
        ]
    }

    /// `do call 'beamtalk_class_vars':'restore'(Snap) ` — the catch boundary's
    /// exit half (ADR 0130 §4), the first statement of a catch's non-NLR arm.
    pub(super) fn class_var_restore_doc(snapshot_var: &ErlangVar) -> Document<'static> {
        docvec![
            "do ",
            beamtalk_class_vars_call("restore", None, vec![leaf::var(snapshot_var.name())]),
            " "
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
            " = ",
            Self::class_var_helper_call_doc(
                "capture",
                vec![match outer {
                    Some(var) => leaf::var(var.to_string()),
                    None => leaf::atom("none"),
                }]
            ),
            " in "
        ]
    }

    /// The class key of an inlined access: the method's bound `CVKey` variable
    /// inside a class-method body (minted at the first access and bound by
    /// [`Self::with_class_var_key_binding`]), the inline key tuple elsewhere.
    ///
    /// `KeyScope::Unscoped` is the default for a generator that is not lowering
    /// a class-method body (unit tests that lower a bare access); every
    /// production class-method body runs under `with_class_var_key_binding`.
    fn class_var_key_ref_doc(&mut self) -> Document<'static> {
        match self.class_var_key_scope.clone() {
            KeyScope::Unscoped => class_var_keys::key_doc("ClassSelf"),
            KeyScope::Bound(var) => leaf::var(var),
            KeyScope::Open => {
                let var = self.fresh_temp_var("CVKey");
                self.class_var_key_scope = KeyScope::Bound(var.clone());
                leaf::var(var)
            }
        }
    }

    /// Opens the per-method key scope around `lower`, which lowers one
    /// class-method body, and binds the key once around the result when an
    /// access used it (BT-3719). The enclosing scope (a `ClassBuilder` fun
    /// lowered inside another class method) is restored afterwards.
    pub(super) fn with_class_var_key_binding(
        &mut self,
        lower: impl FnOnce(&mut Self) -> super::Result<Document<'static>>,
    ) -> super::Result<Document<'static>> {
        let outer = std::mem::replace(&mut self.class_var_key_scope, KeyScope::Open);
        let lowered = lower(self);
        let inner = std::mem::replace(&mut self.class_var_key_scope, outer);
        let body = lowered?;
        Ok(match inner {
            KeyScope::Bound(var) => {
                docvec![class_var_keys::key_binding_doc(&var, "ClassSelf"), body]
            }
            KeyScope::Open | KeyScope::Unscoped => body,
        })
    }

    /// `self` as the receiver of a class-side access.
    fn is_self_receiver(expr: &Expression) -> bool {
        matches!(expr, Expression::Identifier(id) if id.name == "self")
    }

    /// THE predicate for "`receiver.field` lowers to a class-variable read":
    /// in a class method, `self.<declared class variable>`. Both the lowering
    /// (`generate_field_access`) and the capture walker
    /// ([`Self::block_reads_class_var`], through `block_facts::class_var_accesses`,
    /// which applies the same `self` + declared-name test) agree, so a block
    /// binds a capture exactly when something inside it lowers to a read.
    pub(super) fn is_class_var_field_read(
        &self,
        receiver: &Expression,
        field: &Identifier,
    ) -> bool {
        self.in_class_method()
            && Self::is_self_receiver(receiver)
            && self.class_var_names().contains(field.name.as_str())
    }

    /// Whether the class method being compiled is direct-called (ADR 0129
    /// Phase 0b): its class is `sealed` with no class variables and the method
    /// is `class sealed`, the one `ClassInfo::is_direct_call_eligible` rule
    /// that `compute_direct_call_eligible` applies to build
    /// `direct_call_eligible`. Only those methods run with `ClassSelf = nil`
    /// (ADR 0130 §2). Every other class method, including an inheritable one
    /// of a class that has no class variables of its own, runs with its
    /// receiver class as `ClassSelf`.
    fn current_class_method_is_direct_called(&self) -> bool {
        // A ClassBuilder class-method fun always runs with the built class as
        // `ClassSelf`; `current_method_selector` and `class_name()` still
        // describe the enclosing compiled method there.
        if self.builder_class_method_class().is_some() {
            return false;
        }
        let Some(selector) = self.current_method_selector.as_deref() else {
            return false;
        };
        self.direct_call_eligible
            .get(&self.class_name())
            .is_some_and(|info| info.selectors.contains(selector))
    }

    /// THE predicate for "`receiver hasField: ...` lowers to
    /// `beamtalk_class_vars:has`": in a class method that is not direct-called,
    /// a `self` receiver. The `HasField` intrinsic calls it; the capture walker
    /// ([`Self::block_reads_class_var`]) applies the same rule.
    ///
    /// A direct-called method is excluded because its `ClassSelf` is `nil`,
    /// which `beamtalk_class_vars:has` rejects; its `hasField:` is a constant,
    /// see [`Self::is_class_method_has_field_direct_called`].
    pub(super) fn is_class_var_has_field(&self, receiver: &Expression) -> bool {
        self.in_class_method()
            && Self::is_self_receiver(receiver)
            && !self.current_class_method_is_direct_called()
    }

    /// `self hasField: ...` in a direct-called class method: the class has no
    /// class variables and there is no `ClassSelf` to ask, so it lowers to
    /// `false` without touching the class-variable home. Complements
    /// [`Self::is_class_var_has_field`]; exactly one of the two holds for a
    /// class-method `self hasField:`.
    pub(super) fn is_class_method_has_field_direct_called(&self, receiver: &Expression) -> bool {
        self.in_class_method()
            && Self::is_self_receiver(receiver)
            && self.current_class_method_is_direct_called()
    }

    /// Whether `block` (including every nested block literal) reads a class
    /// variable of the class being compiled, so its creation binds a capture.
    /// The walk is `beamtalk-core`'s
    /// [`class_var_accesses`](beamtalk_core::semantic_analysis::block_facts::class_var_accesses),
    /// the one the `class-state-abroad` lint uses too (BT-3761): a `self.<class
    /// variable>` read outside an assignment target (exactly the nodes for which
    /// [`Self::is_class_var_field_read`] holds, the caller having checked
    /// [`Self::in_class_method`]), or a non-cascade `self hasField:` probe, which
    /// lowers to `beamtalk_class_vars:has` exactly when
    /// [`Self::is_class_var_has_field`] holds, i.e. unless the method is
    /// direct-called. A write-only block reads nothing and binds no capture.
    pub(super) fn block_reads_class_var(&self, block: &Block) -> bool {
        let accesses = class_var_accesses(block, self.class_var_names());
        !accesses.reads.is_empty()
            || (!accesses.has_field_probes.is_empty()
                && !self.current_class_method_is_direct_called())
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
        let key = self.class_var_key_ref_doc();
        let fallback = || self.class_var_read_helper_call_doc(function, vec![leaf::atom(name)]);
        docvec![
            "case call 'erlang':'get'(",
            key,
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
        let key = self.class_var_key_ref_doc();
        docvec![
            "let ",
            leaf::var(val_var.clone()),
            " = ",
            value_doc,
            " in case call 'erlang':'get'(",
            key.clone(),
            ") of <",
            leaf::var(map_var.clone()),
            "> when call 'erlang':'is_map'(",
            leaf::var(map_var.clone()),
            ") -> let ",
            leaf::var(old_var),
            " = call 'erlang':'put'(",
            key,
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
