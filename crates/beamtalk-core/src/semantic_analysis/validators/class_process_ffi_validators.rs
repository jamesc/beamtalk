// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `class-process-ffi` lint (BT-4031, ADR 0129).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A class method of a class that is not `sealed`, or a method that is not
//! `class sealed`, runs in the class's `gen_server` process, not the caller's.
//! For a stateless FFI-namespace class that silently changes the meaning of
//! any Erlang call that depends on the calling process: `gen_tcp:accept/1`
//! blocks the one shared class process, `gen_tcp:controlling_process/2` and
//! `file:sendfile/5` fail with `not_owner` (the caller owns the socket, the
//! class process does not), and `erlang:self/0`, the process dictionary,
//! `link` and `monitor` all see the class process.
//!
//! **Fire condition** (all must hold):
//!
//! 1. the method is class-side: in `class.class_methods`, or a standalone
//!    `Sub class >> m` definition (`ClassBuilder classMethods:` blocks are not
//!    inspected in v1);
//! 2. it is not direct-called per [`ClassHierarchy::is_direct_call_eligible`]
//!    (the single authority for the rule: sealed class, no class variables,
//!    non-constructor, `class sealed` method);
//! 3. sealing would fix it: the class declares no class variables of its own
//!    (BT-3762, so the body cannot touch any), and the selector is not a
//!    process-state constructor (`new`, `startLink`, ...) that is never
//!    direct-called;
//! 4. the body contains a direct `(Erlang mod) fn ...` call listed in
//!    [`PROCESS_SENSITIVE_FFI`]. Transitive calls (a helper does the FFI) are
//!    not followed in v1.
//!
//! The hint names exactly the missing piece(s): `sealed` on the class,
//! `class sealed` on the method, or both.
//!
//! Running FFI in the class process is sometimes intentional (deliberate
//! serialisation), so the warning is suppressible with
//! `@expect class_process_ffi`.
//!
//! **Deliberately excluded from the table** (flagging them would fire on the
//! blessed "FFI namespace" pattern): `file:open/read_file/write_file/close`
//! and every pure or query FFI function (`lists`, `maps`, `string`, ...);
//! `inet` entirely (a per-function audit is deferred); and dataflow-sensitive
//! cases such as `process_info(self())` (needs argument tracking).

use crate::ast::{Expression, MethodDefinition, MethodKind, Module};
use crate::ast_walker::walk_expression;
use crate::ffi_receiver::erlang_module_of_receiver;
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::class_hierarchy::ClassInfo;
use crate::semantic_analysis::validators::structural_validators::{
    erlang_arity, erlang_function_name,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

/// Why a call is wrong in the class process; selects the message wording.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Sensitivity {
    /// The call blocks the calling process, so a shared class process
    /// serialises every caller behind it.
    Blocking,
    /// The call depends on the identity or ownership of the calling process
    /// (socket owner, mailbox, process dictionary, links, monitors).
    Ownership,
}

/// Curated v1 allow-list of process-sensitive Erlang functions, as
/// `(module, function, sensitivity)`. Modelled on `KNOWN_ARITIES`: a
/// spec-derived attribute is a possible follow-up. See the module docs for
/// what is deliberately left out.
const PROCESS_SENSITIVE_FFI: &[(&str, &str, Sensitivity)] = &[
    ("erlang", "self", Sensitivity::Ownership),
    ("erlang", "send", Sensitivity::Ownership),
    ("erlang", "get", Sensitivity::Ownership),
    ("erlang", "put", Sensitivity::Ownership),
    ("erlang", "erase", Sensitivity::Ownership),
    ("erlang", "process_flag", Sensitivity::Ownership),
    ("erlang", "link", Sensitivity::Ownership),
    ("erlang", "unlink", Sensitivity::Ownership),
    ("erlang", "monitor", Sensitivity::Ownership),
    ("erlang", "demonitor", Sensitivity::Ownership),
    ("gen_tcp", "accept", Sensitivity::Blocking),
    ("gen_tcp", "connect", Sensitivity::Blocking),
    ("gen_tcp", "listen", Sensitivity::Ownership),
    ("gen_tcp", "recv", Sensitivity::Blocking),
    ("gen_tcp", "send", Sensitivity::Ownership),
    ("gen_tcp", "close", Sensitivity::Ownership),
    ("gen_tcp", "shutdown", Sensitivity::Ownership),
    ("gen_tcp", "controlling_process", Sensitivity::Ownership),
    ("gen_udp", "open", Sensitivity::Ownership),
    ("gen_udp", "recv", Sensitivity::Blocking),
    ("gen_udp", "send", Sensitivity::Ownership),
    ("gen_udp", "close", Sensitivity::Ownership),
    ("gen_udp", "controlling_process", Sensitivity::Ownership),
    ("ssl", "connect", Sensitivity::Blocking),
    ("ssl", "handshake", Sensitivity::Blocking),
    ("ssl", "recv", Sensitivity::Blocking),
    ("ssl", "send", Sensitivity::Ownership),
    ("ssl", "close", Sensitivity::Ownership),
    ("ssl", "shutdown", Sensitivity::Ownership),
    ("ssl", "controlling_process", Sensitivity::Ownership),
    ("file", "sendfile", Sensitivity::Ownership),
];

fn sensitivity_of(module: &str, function: &str) -> Option<Sensitivity> {
    PROCESS_SENSITIVE_FFI
        .iter()
        .find(|(m, f, _)| *m == module && *f == function)
        .map(|(_, _, s)| *s)
}

/// Warns about class-side methods that run in the class process and directly
/// call a process-sensitive Erlang function (see the module docs).
pub(crate) fn check_class_process_ffi(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        for method in class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
        {
            check_method(class.name.name.as_str(), method, hierarchy, diagnostics);
        }
    }
    // Standalone `Foo class >> bar => ...` definitions are class methods of
    // `Foo` too. Package-qualified extensions are left alone: their class is
    // resolved by package, which the simple-name hierarchy lookup does not
    // model (same rule as `class-state-abroad`).
    for standalone in &module.method_definitions {
        if standalone.is_class_method
            && standalone.package.is_none()
            && standalone.method.kind == MethodKind::Primary
        {
            check_method(
                standalone.class_name.name.as_str(),
                &standalone.method,
                hierarchy,
                diagnostics,
            );
        }
    }
}

fn check_method(
    class_name: &str,
    method: &MethodDefinition,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let selector = method.selector.name();
    if ClassInfo::is_direct_call_excluded_selector(&selector)
        || hierarchy.is_direct_call_eligible(class_name, &selector)
        // A class with state cannot be sealed-and-direct-called: no
        // unactionable warning (own variables only, BT-3762).
        || !hierarchy.own_class_variable_names(class_name).is_empty()
    {
        return;
    }
    let class_sealed = hierarchy.get_class(class_name).is_some_and(|c| c.is_sealed);
    let hint = match (class_sealed, method.is_sealed) {
        (false, false) => format!(
            "add `sealed` to class `{class_name}` and `class sealed` to `{selector}` so it \
             direct-calls in the caller's process"
        ),
        (false, true) => format!(
            "add `sealed` to class `{class_name}` so `{selector}` direct-calls in the caller's \
             process"
        ),
        (true, false) => {
            format!("add `class sealed` to `{selector}` so it direct-calls in the caller's process")
        }
        // Both present yet not direct-call-eligible: sealing cannot help.
        (true, true) => return,
    };

    for stmt in &method.body {
        walk_expression(&stmt.expression, &mut |expr| {
            let Expression::MessageSend {
                receiver,
                selector: ffi_selector,
                arguments,
                ..
            } = expr
            else {
                return;
            };
            let (Some(ffi_module), Some(function)) = (
                erlang_module_of_receiver(receiver),
                erlang_function_name(ffi_selector),
            ) else {
                return;
            };
            let Some(sensitivity) = sensitivity_of(ffi_module, &function) else {
                return;
            };
            let arity = erlang_arity(ffi_selector, arguments.len());
            let (why, consequence) = match sensitivity {
                Sensitivity::Blocking => (
                    "which blocks the calling process",
                    "so every caller queues behind one process",
                ),
                Sensitivity::Ownership => (
                    "which depends on the identity or ownership of the calling process",
                    "so it acts on the class process instead of the caller",
                ),
            };
            diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "class method `{class_name} {selector}` calls \
                         `{ffi_module}:{function}/{arity}`, {why} — but this method runs in the \
                         class's gen_server process, not the caller's, {consequence}"
                    ),
                    expr.span(),
                )
                .with_hint(hint.clone())
                .with_category(DiagnosticCategory::ClassProcessFfi),
            );
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compilation::diagnostics_policy::apply_expect_directives;
    use crate::semantic_analysis::analyse;
    use crate::source_analysis::{lex_with_eof, parse};

    fn module_of(src: &str) -> Module {
        let (module, parse_diags) = parse(lex_with_eof(src));
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        module
    }

    fn findings(src: &str) -> Vec<Diagnostic> {
        analyse(&module_of(src))
            .diagnostics
            .into_iter()
            .filter(|d| d.category == Some(DiagnosticCategory::ClassProcessFfi))
            .collect()
    }

    fn hint(d: &Diagnostic) -> &str {
        d.hint.as_deref().unwrap_or("")
    }

    // ── fires ────────────────────────────────────────────────────────────

    #[test]
    fn issue_repro_open_class_fires_for_each_method() {
        let src = "Object subclass: Tcp
  class accept: ls => (Erlang gen_tcp) accept: ls
  class handOff: s to: pid => (Erlang gen_tcp) controlling_process: s with: pid
  class sendfile: s path: p =>
    fd := ((Erlang file) open: p with: #(#read, #raw, #binary)) unwrap
    (Erlang file) sendfile: fd with: s with: 0 with: 0 with: #()
";
        let diags = findings(src);
        assert_eq!(diags.len(), 3, "{diags:?}");
        assert!(diags[0].message.contains("`Tcp accept:`"), "{diags:?}");
        assert!(diags[0].message.contains("`gen_tcp:accept/1`"));
        assert!(diags[0].message.contains("blocks the calling process"));
        assert!(diags[0].message.contains("queues behind one process"));
        assert!(diags[1].message.contains("`gen_tcp:controlling_process/2`"));
        assert!(diags[1].message.contains("ownership"));
        assert!(diags[2].message.contains("`file:sendfile/5`"));
        for d in &diags {
            assert_eq!(d.severity, crate::source_analysis::Severity::Warning);
            assert!(
                hint(d).contains("add `sealed` to class `Tcp` and `class sealed` to"),
                "{d:?}"
            );
        }
    }

    #[test]
    fn unary_erlang_self_in_open_class_fires() {
        let diags = findings("Object subclass: OpenC\n  class me => Erlang erlang self\n");
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("`erlang:self/0`"));
    }

    #[test]
    fn process_dictionary_link_and_flag_fire() {
        let src = "Object subclass: Procy
  class a => (Erlang erlang) put: #k with: 1
  class b: pid => (Erlang erlang) link: pid
  class c => (Erlang erlang) process_flag: #trap_exit with: true
";
        assert_eq!(findings(src).len(), 3);
    }

    #[test]
    fn sealed_class_unsealed_method_hint_names_class_sealed_only() {
        let diags =
            findings("sealed Object subclass: SealedC\n  class me2 => Erlang erlang self\n");
        assert_eq!(diags.len(), 1, "{diags:?}");
        let h = hint(&diags[0]);
        assert!(h.contains("add `class sealed` to `me2`"), "{h}");
        assert!(!h.contains("add `sealed` to class"), "{h}");
    }

    #[test]
    fn open_class_sealed_method_hint_names_sealed_only() {
        let diags = findings("Object subclass: OpenC\n  class sealed me => Erlang erlang self\n");
        assert_eq!(diags.len(), 1, "{diags:?}");
        let h = hint(&diags[0]);
        assert!(h.contains("add `sealed` to class `OpenC`"), "{h}");
        assert!(!h.contains("`class sealed` to"), "{h}");
    }

    #[test]
    fn standalone_class_method_definition_fires() {
        let src = "Object subclass: Host\n\nHost class >> me => Erlang erlang self\n";
        let diags = findings(src);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("`Host me`"));
    }

    // ── silent ───────────────────────────────────────────────────────────

    #[test]
    fn sealed_class_with_class_sealed_method_is_silent() {
        let src = "sealed Object subclass: SealedC
  class sealed me => Erlang erlang self
  class sealed accept: ls => (Erlang gen_tcp) accept: ls
";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn class_with_class_state_is_silent() {
        let src = "Object subclass: Stateful
  classState: n = 0

  class me => Erlang erlang self
";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn pure_ffi_in_open_class_is_silent() {
        let src = "Object subclass: Lists
  class rev: xs => (Erlang lists) reverse: xs
  class read: p => (Erlang file) read_file: p
  class open: p => (Erlang file) open: p with: #(#read)
";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn non_ffi_class_methods_are_silent() {
        let src = "Object subclass: Plain
  class one => 1
  class two: x => x + 1
";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn constructors_are_silent() {
        let src = "Object subclass: Ctor
  class new => Erlang erlang self
  class startLink => Erlang erlang self
";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn instance_side_methods_are_silent() {
        let src = "Object subclass: Inst\n  me => Erlang erlang self\n";
        assert!(findings(src).is_empty());
    }

    // ── category / policy / @expect ──────────────────────────────────────

    #[test]
    fn expect_category_name_round_trips() {
        assert_eq!(
            crate::ast::ExpectCategory::from_name("class_process_ffi"),
            Some(crate::ast::ExpectCategory::ClassProcessFfi)
        );
        assert!(crate::ast::ExpectCategory::valid_names().contains(&"class_process_ffi"));
    }

    #[test]
    fn expect_class_process_ffi_suppresses_the_warning() {
        let src = "Object subclass: OpenC
  class me =>
    @expect class_process_ffi
    Erlang erlang self
";
        let module = module_of(src);
        let mut diags = analyse(&module).diagnostics;
        apply_expect_directives(&module, &mut diags);
        assert!(
            !diags
                .iter()
                .any(|d| d.category == Some(DiagnosticCategory::ClassProcessFfi)),
            "{diags:?}"
        );
        assert!(
            !diags.iter().any(|d| d.message.contains("stale @expect")),
            "the directive matched: {diags:?}"
        );
    }
}
