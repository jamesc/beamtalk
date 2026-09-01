// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! All-sends query — extract every message send within a single method's source.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation unimplementedSelectors` (BT-2206). Where
//! [`crate::queries::senders_query`] filters by one known selector, this query
//! collects EVERY [`Expression::MessageSend`] and [`Cascade`] message in a
//! single pass — selector name, 1-based line number (relative to the input
//! source), and the syntactic kind of receiver
//! (`self` / `super` / Erlang FFI / other).
//!
//! The single-pass design lets the typo-finder compute
//! `allSentSelectors − allDefinedSelectors` without re-parsing each method
//! once per candidate selector.
//!
//! # Implementation
//!
//! The implementation lives in [`beamtalk_core::method_source_walker`], a shared leaf
//! module below both this Language Service context and the Code Generation
//! context that also needs these results for xref codegen (ADR 0087 Phase 2).
//! See `docs/development/architecture-principles.md` §6.

pub use beamtalk_core::method_source_walker::{ReceiverKind, SendHit, find_all_sends_in_source};

#[cfg(test)]
mod tests {
    use super::*;

    fn selectors(hits: &[SendHit]) -> Vec<&str> {
        hits.iter().map(|h| h.selector.as_str()).collect()
    }

    #[test]
    fn finds_self_send() {
        let hits = find_all_sends_in_source("greet => self name");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "name");
        assert_eq!(hits[0].line, 1);
        assert_eq!(hits[0].receiver, ReceiverKind::SelfReceiver);
    }

    #[test]
    fn finds_super_send() {
        let hits = find_all_sends_in_source("increment => super increment");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "increment");
        assert_eq!(hits[0].receiver, ReceiverKind::SuperReceiver);
    }

    #[test]
    fn finds_other_receiver_send() {
        let hits = find_all_sends_in_source("describe => Transcript show");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "show");
        assert_eq!(hits[0].receiver, ReceiverKind::Other);
    }

    #[test]
    fn finds_unary_keyword_and_binary_sends() {
        // `self items at: index put: value` — keyword send on self, plus the
        // `index`/`value` identifiers are not sends.
        let hits = find_all_sends_in_source("store: value => self items at: 1 put: value");
        let sels = selectors(&hits);
        // `items` (unary on self) and `at:put:` (keyword on the `items` result).
        assert!(sels.contains(&"items"), "got {sels:?}");
        assert!(sels.contains(&"at:put:"), "got {sels:?}");
        // `items` is sent to self; `at:put:` is sent to the `items` result.
        let items = hits.iter().find(|h| h.selector == "items").unwrap();
        assert_eq!(items.receiver, ReceiverKind::SelfReceiver);
        let atput = hits.iter().find(|h| h.selector == "at:put:").unwrap();
        assert_eq!(atput.receiver, ReceiverKind::Other);
    }

    #[test]
    fn finds_binary_send() {
        let hits = find_all_sends_in_source("double: n => n * 2");
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "*");
        assert_eq!(hits[0].receiver, ReceiverKind::Other);
    }

    #[test]
    fn finds_cascade_sends_with_shared_receiver_kind() {
        let src = "report =>\n  self\n    show: \"a\";\n    show: \"b\"";
        let hits = find_all_sends_in_source(src);
        let shows: Vec<&SendHit> = hits.iter().filter(|h| h.selector == "show:").collect();
        assert_eq!(shows.len(), 2, "got {hits:?}");
        // Both cascade messages share the cascade's `self` receiver.
        assert!(
            shows
                .iter()
                .all(|h| h.receiver == ReceiverKind::SelfReceiver)
        );
        assert_eq!(shows[0].line, 3);
        assert_eq!(shows[1].line, 4);
    }

    #[test]
    fn finds_send_inside_nested_block() {
        let src = "shout =>\n  [:x | x asString] value: 42";
        let hits = find_all_sends_in_source(src);
        let sels = selectors(&hits);
        assert!(sels.contains(&"asString"), "got {sels:?}");
        assert!(sels.contains(&"value:"), "got {sels:?}");
        let as_string = hits.iter().find(|h| h.selector == "asString").unwrap();
        assert_eq!(as_string.line, 2);
        assert_eq!(as_string.receiver, ReceiverKind::Other);
    }

    #[test]
    fn collects_multiple_sends_in_source_order() {
        let src = "report =>\n  a printString\n  b printString\n  c printString";
        let hits = find_all_sends_in_source(src);
        let prints: Vec<&SendHit> = hits
            .iter()
            .filter(|h| h.selector == "printString")
            .collect();
        assert_eq!(prints.len(), 3);
        assert_eq!(prints[0].line, 2);
        assert_eq!(prints[1].line, 3);
        assert_eq!(prints[2].line, 4);
    }

    #[test]
    fn finds_send_in_class_method_with_return_type() {
        let src = "default -> SystemNavigation =>\n  self new";
        let hits = find_all_sends_in_source(src);
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].selector, "new");
        assert_eq!(hits[0].line, 2);
        assert_eq!(hits[0].receiver, ReceiverKind::SelfReceiver);
    }

    #[test]
    fn finds_send_with_leading_doc_comment_and_expect() {
        let src = "/// doc line\ndefault -> SystemNavigation =>\n  @expect dnu\n  self new";
        let hits = find_all_sends_in_source(src);
        let new = hits.iter().find(|h| h.selector == "new");
        assert!(new.is_some(), "expected `new`, got {hits:?}");
    }

    #[test]
    fn unparseable_source_returns_empty() {
        let hits = find_all_sends_in_source(")@!");
        assert!(hits.is_empty(), "got {hits:?}");
    }

    #[test]
    fn empty_body_returns_empty() {
        // A method with no sends in its body produces no hits.
        let hits = find_all_sends_in_source("answer => 42");
        assert!(hits.is_empty(), "got {hits:?}");
    }

    #[test]
    fn erlang_ffi_module_name_is_tagged() {
        // `Erlang beamtalk_interface` — the `beamtalk_interface` send has the
        // `Erlang` class reference as its receiver.
        let hits = find_all_sends_in_source("classes => Erlang beamtalk_interface");
        let hit = hits
            .iter()
            .find(|h| h.selector == "beamtalk_interface")
            .unwrap();
        assert_eq!(hit.receiver, ReceiverKind::ErlangFfi);
        // The module-name send carries the target module too (its own selector).
        assert_eq!(hit.target_module.as_deref(), Some("beamtalk_interface"));
    }

    #[test]
    fn erlang_ffi_chained_call_is_tagged() {
        // `(Erlang beamtalk_interface) allSendsIn: src` — the `allSendsIn:`
        // send's receiver is the `(Erlang beamtalk_interface)` send chain.
        let hits =
            find_all_sends_in_source("run: src => (Erlang beamtalk_interface) allSendsIn: src");
        let chained = hits.iter().find(|h| h.selector == "allSendsIn:").unwrap();
        assert_eq!(chained.receiver, ReceiverKind::ErlangFfi);
        // The function send resolves the target module from its receiver chain.
        assert_eq!(chained.target_module.as_deref(), Some("beamtalk_interface"));
        let module = hits
            .iter()
            .find(|h| h.selector == "beamtalk_interface")
            .unwrap();
        assert_eq!(module.receiver, ReceiverKind::ErlangFfi);
        assert_eq!(module.target_module.as_deref(), Some("beamtalk_interface"));
    }

    #[test]
    fn erlang_ffi_keyword_call_resolves_module() {
        // `(Erlang lists) reverse: xs` — the keyword function send's module is
        // `lists`, recovered from the parenthesised receiver chain.
        let hits = find_all_sends_in_source("rev: xs => (Erlang lists) reverse: xs");
        let call = hits.iter().find(|h| h.selector == "reverse:").unwrap();
        assert_eq!(call.receiver, ReceiverKind::ErlangFfi);
        assert_eq!(call.target_module.as_deref(), Some("lists"));
    }

    #[test]
    fn non_erlang_class_reference_is_other_not_ffi() {
        // A real class-reference receiver (`Integer new`) is `Other`, not FFI.
        let hits = find_all_sends_in_source("make => Integer new");
        let hit = hits.iter().find(|h| h.selector == "new").unwrap();
        assert_eq!(hit.receiver, ReceiverKind::Other);
        // Non-FFI sends never carry a target module.
        assert_eq!(hit.target_module, None);
    }

    #[test]
    fn erlang_class_protocol_selector_is_other_not_ffi() {
        // BT-3079 regression: `Erlang class` must NOT be reported as an FFI
        // call to a module named "class" — it dispatches to the class
        // protocol (metaclass), like `Erlang new`, `Erlang printString`, etc.
        let hits = find_all_sends_in_source("meta => Erlang class");
        let hit = hits.iter().find(|h| h.selector == "class").unwrap();
        assert_eq!(hit.receiver, ReceiverKind::Other);
        assert_eq!(hit.target_module, None);
    }

    #[test]
    fn erlang_new_class_protocol_selector_is_other_not_ffi() {
        // Same as above for `new` — the other class-protocol selector most
        // likely to collide with a real Erlang module name.
        let hits = find_all_sends_in_source("make => Erlang new");
        let hit = hits.iter().find(|h| h.selector == "new").unwrap();
        assert_eq!(hit.receiver, ReceiverKind::Other);
        assert_eq!(hit.target_module, None);
    }

    #[test]
    fn package_qualified_erlang_is_other_not_ffi() {
        // BT-3079 regression: `json@Erlang lists` names a package-scoped
        // `Erlang` class, not the compiler's built-in FFI bridge, so it must
        // not be tagged `ErlangFfi`.
        let hits = find_all_sends_in_source("go => json@Erlang lists");
        let hit = hits.iter().find(|h| h.selector == "lists").unwrap();
        assert_eq!(hit.receiver, ReceiverKind::Other);
        assert_eq!(hit.target_module, None);
    }

    #[test]
    fn erlang_ffi_cascade_reuses_module_across_messages() {
        // BT-3079 regression: `(Erlang lists) reverse: xs; flatten: xs` — the
        // shared cascade receiver resolves to FFI module `lists` once and
        // both cascade messages (including the second, which never sees the
        // receiver syntactically) must carry it.
        let hits = find_all_sends_in_source("rev: xs => (Erlang lists) reverse: xs; flatten: xs");
        let reverse = hits.iter().find(|h| h.selector == "reverse:").unwrap();
        let flatten = hits.iter().find(|h| h.selector == "flatten:").unwrap();
        assert_eq!(reverse.receiver, ReceiverKind::ErlangFfi);
        assert_eq!(reverse.target_module.as_deref(), Some("lists"));
        assert_eq!(flatten.receiver, ReceiverKind::ErlangFfi);
        assert_eq!(flatten.target_module.as_deref(), Some("lists"));
    }

    #[test]
    fn erlang_class_protocol_cascade_is_other_not_ffi() {
        // BT-3079 regression: `Erlang class; foo` — the shared cascade
        // receiver's first message is the class-protocol selector `class`,
        // so neither cascade message may be tagged FFI.
        let hits = find_all_sends_in_source("meta => Erlang class; foo");
        let class_msg = hits.iter().find(|h| h.selector == "class").unwrap();
        let foo_msg = hits.iter().find(|h| h.selector == "foo").unwrap();
        assert_eq!(class_msg.receiver, ReceiverKind::Other);
        assert_eq!(class_msg.target_module, None);
        assert_eq!(foo_msg.receiver, ReceiverKind::Other);
        assert_eq!(foo_msg.target_module, None);
    }

    #[test]
    fn native_typed_signature_phantom_sends_are_skipped() {
        // The `native:` selector collides with the synthetic class grammar
        // (it is consumed as a class backing-module declaration), so the typed
        // return type `Symbol -> Symbol` parses into phantom `Symbol` / `->`
        // sends rooted in a parse error. Those must not be reported.
        let src = "native: m :: Symbol -> Symbol =>\n  self.backingModule := m";
        let hits = find_all_sends_in_source(src);
        let sels = selectors(&hits);
        assert!(
            !sels.contains(&"Symbol"),
            "phantom `Symbol` leaked: {sels:?}"
        );
        assert!(!sels.contains(&"->"), "phantom `->` leaked: {sels:?}");
    }

    #[test]
    fn error_rooted_send_is_skipped_but_clean_body_survives() {
        // A typed signature with a *non-colliding* selector parses cleanly and
        // its body sends are still collected.
        let hits = find_all_sends_in_source("store: m :: Symbol -> Symbol =>\n  self register: m");
        let sels = selectors(&hits);
        assert!(
            sels.contains(&"register:"),
            "clean body send lost: {sels:?}"
        );
        assert!(!sels.contains(&"Symbol"), "phantom leaked: {sels:?}");
    }

    #[test]
    fn perform_argument_symbol_is_not_a_send() {
        // The dynamic selector argument to `perform:` is a symbol literal, not
        // an AST send, so only `perform:` itself is collected — never the
        // symbol it carries. This is what makes the typo-finder's `perform:`
        // exclusion fall out for free.
        let hits = find_all_sends_in_source("run => self perform: #anythingXyzzy");
        let sels = selectors(&hits);
        assert!(sels.contains(&"perform:"), "got {sels:?}");
        assert!(!sels.contains(&"anythingXyzzy"), "got {sels:?}");
    }
}
