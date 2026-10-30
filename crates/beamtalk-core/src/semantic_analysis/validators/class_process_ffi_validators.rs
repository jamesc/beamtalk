// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-method FFI lints: `class-process-ffi` (BT-4031) and its ADR 0142
//! inversion `class-method-process-resource` (BT-4302).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Under ADR 0142 a class method runs in its class's `gen_server` process
//! only when the class needs that process: it declares class variables
//! (`classState:`), or the selector is process-bound (`new`, `startLink`,
//! `spawn`, ... — ADR 0142 §1 rule 3). Every other class method runs in the
//! **caller**. The two lints split on that one fact,
//! [`ClassInfo::has_class_variables`] (shared with
//! [`ClassInfo::is_direct_call_eligible`]), so a call gets at most one of
//! them:
//!
//! - **`class-process-ffi`** — the class declares class variables, so the
//!   method runs in the class process, and it calls a function that depends
//!   on the *calling* process: `gen_tcp:accept/1` blocks the one shared class
//!   process, `gen_tcp:controlling_process/2` and `file:sendfile/5` fail with
//!   `not_owner` (the caller owns the socket), and `erlang:self/0`, the
//!   process dictionary, `link` and `monitor` all see the class process.
//!   Suppress deliberate serialisation with `@expect class_process_ffi`.
//! - **`class-method-process-resource`** — the class declares no class
//!   variables, so the method runs in the caller, and it creates a
//!   process-owned resource (`ets:new`, `open_port`, `spawn_link`, `link`,
//!   `monitor`, `register`, `erlang:send_after`/`start_timer`, the `timer`
//!   timer constructors) or relies on `self()` or the process dictionary. The
//!   resource belongs to the caller and dies with it. The fix is an Actor
//!   that owns the resource, or `classState:` to keep the class process.
//!   Suppress with `@expect class_method_process_resource`.
//!
//! **Fire condition** (both lints):
//!
//! 1. the method is class-side: in `class.class_methods`, or a standalone
//!    `Sub class >> m` definition (`ClassBuilder classMethods:` blocks are not
//!    inspected in v1);
//! 2. the class is known to the hierarchy and the selector is not
//!    process-bound ([`ClassInfo::is_process_bound_class_selector`]: those
//!    keep their routing whatever the class declares, so neither fix helps);
//! 3. the body contains a direct `(Erlang mod) fn ...` call listed in
//!    [`CLASS_METHOD_FFI`] with an effect for the process the method runs in.
//!    Transitive calls (a helper does the FFI) are not followed in v1.
//!
//! **Deliberately excluded from the table** (flagging them would fire on the
//! blessed "FFI namespace" pattern): `file:open/read_file/write_file/close`
//! and every pure or query FFI function (`lists`, `maps`, `string`, ...);
//! `inet` entirely (a per-function audit is deferred); the `timer` helpers
//! that create nothing (`timer:seconds/1`, `timer:tc/1`, `timer:sleep/1`,
//! ...); and dataflow-sensitive cases such as `process_info(self())` (needs
//! argument tracking).

use crate::ast::{Expression, MethodDefinition, MethodKind, Module};
use crate::ast_walker::walk_expression;
use crate::ffi_receiver::erlang_module_of_receiver;
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::class_hierarchy::ClassInfo;
use crate::semantic_analysis::validators::structural_validators::{
    erlang_arity, erlang_function_name,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

/// Why a call is wrong in the **class process** (`class-process-ffi`);
/// selects the message wording.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Sensitivity {
    /// The call blocks the calling process, so a shared class process
    /// serialises every caller behind it.
    Blocking,
    /// The call depends on the identity or ownership of the calling process
    /// (socket owner, mailbox, process dictionary, links, monitors).
    Ownership,
}

/// Why a call is surprising in the **caller**
/// (`class-method-process-resource`); selects the message wording.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CallerEffect {
    /// The call creates a resource owned by the calling process (ETS table,
    /// port, linked process, link, monitor, registered name, timer), so it
    /// dies with the caller.
    Resource,
    /// The call reads or writes the calling process's identity or process
    /// dictionary (`self()`, `put`/`get`/`erase`).
    Identity,
}

/// One curated row: the Erlang function and its effect in each process a
/// class method can run in. `None` means that process's lint does not fire.
struct ClassMethodFfi {
    module: &'static str,
    function: &'static str,
    /// Effect when the method runs in the class process (`class-process-ffi`).
    in_class_process: Option<Sensitivity>,
    /// Effect when the method runs in the caller
    /// (`class-method-process-resource`).
    in_caller: Option<CallerEffect>,
}

/// Builds a [`ClassMethodFfi`] row (keeps [`CLASS_METHOD_FFI`] one line per
/// function).
const fn row(
    module: &'static str,
    function: &'static str,
    in_class_process: Option<Sensitivity>,
    in_caller: Option<CallerEffect>,
) -> ClassMethodFfi {
    ClassMethodFfi {
        module,
        function,
        in_class_process,
        in_caller,
    }
}

const BLOCKING: Option<Sensitivity> = Some(Sensitivity::Blocking);
const OWNERSHIP: Option<Sensitivity> = Some(Sensitivity::Ownership);
const RESOURCE: Option<CallerEffect> = Some(CallerEffect::Resource);
const IDENTITY: Option<CallerEffect> = Some(CallerEffect::Identity);

/// The single curated v1 table of process-sensitive Erlang functions, read by
/// both lints: a function both lints care about (`self`, the process
/// dictionary, `link`, `monitor`) has one row with both effects. Modelled on
/// `KNOWN_ARITIES`: a spec-derived attribute is a possible follow-up. See the
/// module docs for what is deliberately left out.
const CLASS_METHOD_FFI: &[ClassMethodFfi] = &[
    row("erlang", "self", OWNERSHIP, IDENTITY),
    row("erlang", "send", OWNERSHIP, None),
    row("erlang", "get", OWNERSHIP, IDENTITY),
    row("erlang", "put", OWNERSHIP, IDENTITY),
    row("erlang", "erase", OWNERSHIP, IDENTITY),
    row("erlang", "process_flag", OWNERSHIP, None),
    row("erlang", "link", OWNERSHIP, RESOURCE),
    row("erlang", "unlink", OWNERSHIP, None),
    row("erlang", "monitor", OWNERSHIP, RESOURCE),
    row("erlang", "demonitor", OWNERSHIP, None),
    row("erlang", "spawn_link", None, RESOURCE),
    row("erlang", "open_port", None, RESOURCE),
    row("erlang", "register", None, RESOURCE),
    row("erlang", "send_after", None, RESOURCE),
    row("erlang", "start_timer", None, RESOURCE),
    row("ets", "new", None, RESOURCE),
    row("timer", "apply_after", None, RESOURCE),
    row("timer", "apply_interval", None, RESOURCE),
    row("timer", "apply_repeatedly", None, RESOURCE),
    row("timer", "send_after", None, RESOURCE),
    row("timer", "send_interval", None, RESOURCE),
    row("timer", "exit_after", None, RESOURCE),
    row("timer", "kill_after", None, RESOURCE),
    row("gen_tcp", "accept", BLOCKING, None),
    row("gen_tcp", "connect", BLOCKING, None),
    row("gen_tcp", "listen", OWNERSHIP, None),
    row("gen_tcp", "recv", BLOCKING, None),
    row("gen_tcp", "send", OWNERSHIP, None),
    row("gen_tcp", "close", OWNERSHIP, None),
    row("gen_tcp", "shutdown", OWNERSHIP, None),
    row("gen_tcp", "controlling_process", OWNERSHIP, None),
    row("gen_udp", "open", OWNERSHIP, None),
    row("gen_udp", "recv", BLOCKING, None),
    row("gen_udp", "send", OWNERSHIP, None),
    row("gen_udp", "close", OWNERSHIP, None),
    row("gen_udp", "controlling_process", OWNERSHIP, None),
    row("ssl", "connect", BLOCKING, None),
    row("ssl", "handshake", BLOCKING, None),
    row("ssl", "recv", BLOCKING, None),
    row("ssl", "send", OWNERSHIP, None),
    row("ssl", "close", OWNERSHIP, None),
    row("ssl", "shutdown", OWNERSHIP, None),
    row("ssl", "controlling_process", OWNERSHIP, None),
    row("file", "sendfile", OWNERSHIP, None),
];

fn table_row(module: &str, function: &str) -> Option<&'static ClassMethodFfi> {
    CLASS_METHOD_FFI
        .iter()
        .find(|r| r.module == module && r.function == function)
}

/// Runs both class-method FFI lints over every class-side method of `module`
/// (see the module docs).
pub(crate) fn check_class_method_ffi(
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
    if ClassInfo::is_process_bound_class_selector(&selector) {
        return;
    }
    let Some(class) = hierarchy.get_class(class_name) else {
        return;
    };
    // ADR 0142 §1: the class needs its process exactly when it declares
    // class variables of its own; otherwise the method runs in the caller.
    let runs_in_class_process = class.has_class_variables();

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
            let Some(entry) = table_row(ffi_module, &function) else {
                return;
            };
            let arity = erlang_arity(ffi_selector, arguments.len());
            let site = Site {
                class_name,
                selector: &selector,
                call: format!("`{ffi_module}:{function}/{arity}`"),
                expr,
            };
            let diagnostic = if runs_in_class_process {
                entry.in_class_process.map(|s| site.class_process(s))
            } else {
                entry.in_caller.map(|e| site.caller(e))
            };
            diagnostics.extend(diagnostic);
        });
    }
}

/// A matched FFI call inside a class method: what both messages name.
struct Site<'a> {
    class_name: &'a str,
    selector: &'a str,
    /// The call, rendered as `` `mod:fn/arity` ``.
    call: String,
    expr: &'a Expression,
}

impl Site<'_> {
    /// `class-process-ffi`: the method runs in the class process (the class
    /// declares class variables) and the call depends on the calling process.
    fn class_process(&self, sensitivity: Sensitivity) -> Diagnostic {
        let Self {
            class_name,
            selector,
            call,
            expr,
        } = self;
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
        Diagnostic::warning(
            format!(
                "class method `{class_name} {selector}` calls {call}, {why} — but \
                 `{class_name}` declares class variables, so this method runs in the class's \
                 gen_server process, not the caller's, {consequence}"
            ),
            expr.span(),
        )
        .with_hint(format!(
            "move the call into an instance method or an Actor, or move the class variables \
             out of `{class_name}`: a class without `classState:` runs its class methods in the \
             caller's process (ADR 0142)"
        ))
        .with_category(DiagnosticCategory::ClassProcessFfi)
    }

    /// `class-method-process-resource`: the method runs in the caller (the
    /// class declares no class variables) and the call creates a resource
    /// owned by, or relies on the identity of, the calling process.
    fn caller(&self, effect: CallerEffect) -> Diagnostic {
        let Self {
            class_name,
            selector,
            call,
            expr,
        } = self;
        let (why, consequence) = match effect {
            CallerEffect::Resource => (
                "which creates a resource owned by the calling process",
                "so the resource belongs to the caller and dies with it",
            ),
            CallerEffect::Identity => (
                "which relies on the calling process (`self()` or the process dictionary)",
                "so it sees the caller, not the class process",
            ),
        };
        Diagnostic::warning(
            format!(
                "class method `{class_name} {selector}` calls {call}, {why} — `{class_name}` \
                 declares no class variables, so this method runs in the caller's process \
                 (ADR 0142), {consequence}"
            ),
            expr.span(),
        )
        .with_hint(format!(
            "move ownership to an Actor (create the resource in its `initialize`), or declare \
             `classState:` on `{class_name}` to keep its class methods in the class process"
        ))
        .with_category(DiagnosticCategory::ClassMethodProcessResource)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compilation::diagnostics_policy::apply_expect_directives;
    use crate::semantic_analysis::analyse;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn module_of(src: &str) -> Module {
        let (module, parse_diags) = parse(lex_with_eof(src));
        assert!(
            parse_diags.is_empty(),
            "Parse failed: {parse_diags:?}\n{src}"
        );
        module
    }

    fn of_category(src: &str, category: DiagnosticCategory) -> Vec<Diagnostic> {
        analyse(&module_of(src))
            .diagnostics
            .into_iter()
            .filter(|d| d.category == Some(category))
            .collect()
    }

    /// `class-process-ffi` findings.
    fn in_class_process(src: &str) -> Vec<Diagnostic> {
        of_category(src, DiagnosticCategory::ClassProcessFfi)
    }

    /// `class-method-process-resource` findings.
    fn in_caller(src: &str) -> Vec<Diagnostic> {
        of_category(src, DiagnosticCategory::ClassMethodProcessResource)
    }

    /// Diagnostics left after `@expect` directives are applied.
    fn after_expect(src: &str) -> Vec<Diagnostic> {
        let module = module_of(src);
        let mut diags = analyse(&module).diagnostics;
        apply_expect_directives(&module, &mut diags);
        diags
    }

    fn hint(d: &Diagnostic) -> &str {
        d.hint.as_deref().unwrap_or("")
    }

    /// A one-method class calling `(Erlang module) function: x` from
    /// `class probe: x`, with or without a class variable.
    fn probe_class(module: &str, function: &str, stateful: bool) -> String {
        let state = if stateful {
            "  classState: n = 0\n\n"
        } else {
            ""
        };
        format!(
            "Object subclass: Probe\n{state}  class probe: x => (Erlang {module}) {function}: x\n"
        )
    }

    // ── table: one positive and one negative per row ─────────────────────

    #[test]
    fn every_caller_row_fires_in_a_stateless_class_and_not_with_class_state() {
        for entry in CLASS_METHOD_FFI.iter().filter(|r| r.in_caller.is_some()) {
            let (m, f) = (entry.module, entry.function);
            let fires = in_caller(&probe_class(m, f, false));
            assert_eq!(fires.len(), 1, "{m}:{f} in a stateless class: {fires:?}");
            assert!(
                fires[0].message.contains(&format!("`{m}:{f}/1`")),
                "{fires:?}"
            );
            assert_eq!(fires[0].severity, Severity::Warning);
            let silent = in_caller(&probe_class(m, f, true));
            assert!(silent.is_empty(), "{m}:{f} with classState: {silent:?}");
        }
    }

    #[test]
    fn rows_without_a_caller_effect_never_fire_class_method_process_resource() {
        for entry in CLASS_METHOD_FFI.iter().filter(|r| r.in_caller.is_none()) {
            let (m, f) = (entry.module, entry.function);
            let silent = in_caller(&probe_class(m, f, false));
            assert!(silent.is_empty(), "{m}:{f}: {silent:?}");
        }
    }

    #[test]
    fn every_class_process_row_fires_with_class_state_and_not_without() {
        for entry in CLASS_METHOD_FFI
            .iter()
            .filter(|r| r.in_class_process.is_some())
        {
            let (m, f) = (entry.module, entry.function);
            let fires = in_class_process(&probe_class(m, f, true));
            assert_eq!(fires.len(), 1, "{m}:{f} with classState: {fires:?}");
            assert!(
                fires[0].message.contains(&format!("`{m}:{f}/1`")),
                "{fires:?}"
            );
            // Retired for stateless classes: they run in the caller (ADR 0142).
            let silent = in_class_process(&probe_class(m, f, false));
            assert!(
                silent.is_empty(),
                "{m}:{f} in a stateless class: {silent:?}"
            );
        }
    }

    #[test]
    fn rows_without_a_class_process_effect_never_fire_class_process_ffi() {
        for entry in CLASS_METHOD_FFI
            .iter()
            .filter(|r| r.in_class_process.is_none())
        {
            let (m, f) = (entry.module, entry.function);
            let silent = in_class_process(&probe_class(m, f, true));
            assert!(silent.is_empty(), "{m}:{f}: {silent:?}");
        }
    }

    #[test]
    fn table_has_no_duplicate_rows() {
        for (i, a) in CLASS_METHOD_FFI.iter().enumerate() {
            assert!(
                a.in_class_process.is_some() || a.in_caller.is_some(),
                "{}:{} fires nowhere",
                a.module,
                a.function
            );
            for b in &CLASS_METHOD_FFI[i + 1..] {
                assert!(
                    (a.module, a.function) != (b.module, b.function),
                    "duplicate row {}:{}",
                    a.module,
                    a.function
                );
            }
        }
    }

    // ── class-method-process-resource ────────────────────────────────────

    #[test]
    fn stateless_resource_message_and_hint_name_the_fix() {
        let src = "Object subclass: Tables\n  class make => (Erlang ets) new: #t with: #()\n";
        let diags = in_caller(src);
        assert_eq!(diags.len(), 1, "{diags:?}");
        let msg = &diags[0].message;
        assert!(msg.contains("`Tables make`"), "{msg}");
        assert!(msg.contains("`ets:new/2`"), "{msg}");
        assert!(msg.contains("creates a resource owned by the calling process"));
        assert!(msg.contains("runs in the caller's process"));
        assert!(msg.contains("dies with it"));
        let h = hint(&diags[0]);
        assert!(h.contains("Actor"), "{h}");
        assert!(h.contains("`classState:` on `Tables`"), "{h}");
    }

    #[test]
    fn unary_erlang_self_in_stateless_class_relies_on_the_caller() {
        let diags = in_caller("Object subclass: OpenC\n  class me => Erlang erlang self\n");
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("`erlang:self/0`"));
        assert!(
            diags[0]
                .message
                .contains("`self()` or the process dictionary")
        );
        assert!(diags[0].message.contains("sees the caller"));
    }

    #[test]
    fn sealing_plays_no_part() {
        // ADR 0142: `sealed`/`class sealed` no longer decide where a class
        // method runs; a stateless class runs it in the caller either way.
        let src = "sealed Object subclass: SealedC
  class sealed me => Erlang erlang self
  class open: p => (Erlang erlang) open_port: p with: #()
";
        assert_eq!(in_caller(src).len(), 2);
        assert!(in_class_process(src).is_empty());
    }

    #[test]
    fn standalone_class_method_definition_fires_in_caller() {
        let src = "Object subclass: Host\n\nHost class >> me => Erlang erlang self\n";
        let diags = in_caller(src);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("`Host me`"));
    }

    #[test]
    fn pure_ffi_and_pure_timer_helpers_are_silent() {
        let src = "Object subclass: Lists
  class rev: xs => (Erlang lists) reverse: xs
  class read: p => (Erlang file) read_file: p
  class secs: n => (Erlang timer) seconds: n
  class nap: n => (Erlang timer) sleep: n
";
        assert!(in_caller(src).is_empty());
        assert!(in_class_process(src).is_empty());
    }

    #[test]
    fn process_bound_selectors_are_silent() {
        let src = "Object subclass: Ctor
  class new => (Erlang ets) new: #t with: #()
  class startLink => Erlang erlang self
  class spawn => Erlang erlang self
  class run: x => (Erlang erlang) link: x
";
        assert!(in_caller(src).is_empty());
        let stateful = "Object subclass: Ctor2
  classState: n = 0

  class new => Erlang erlang self
  class startLink => (Erlang gen_tcp) accept: 1
";
        assert!(in_class_process(stateful).is_empty());
    }

    #[test]
    fn instance_side_methods_are_silent() {
        let src = "Object subclass: Inst\n  me => Erlang erlang self\n";
        assert!(in_caller(src).is_empty());
        let stateful =
            "Object subclass: Inst2\n  classState: n = 0\n\n  me => Erlang erlang self\n";
        assert!(in_class_process(stateful).is_empty());
    }

    // ── class-process-ffi (classes with class variables) ─────────────────

    #[test]
    fn issue_repro_with_class_state_fires_for_each_method() {
        let src = "Object subclass: Tcp
  classState: count = 0

  class accept: ls => (Erlang gen_tcp) accept: ls
  class handOff: s to: pid => (Erlang gen_tcp) controlling_process: s with: pid
  class sendfile: s path: p =>
    fd := ((Erlang file) open: p with: #(#read, #raw, #binary)) unwrap
    (Erlang file) sendfile: fd with: s with: 0 with: 0 with: #()
";
        let diags = in_class_process(src);
        assert_eq!(diags.len(), 3, "{diags:?}");
        assert!(diags[0].message.contains("`Tcp accept:`"), "{diags:?}");
        assert!(diags[0].message.contains("`gen_tcp:accept/1`"));
        assert!(diags[0].message.contains("blocks the calling process"));
        assert!(diags[0].message.contains("queues behind one process"));
        assert!(diags[0].message.contains("declares class variables"));
        assert!(diags[1].message.contains("`gen_tcp:controlling_process/2`"));
        assert!(diags[1].message.contains("ownership"));
        assert!(diags[2].message.contains("`file:sendfile/5`"));
        for d in &diags {
            assert_eq!(d.severity, Severity::Warning);
            assert!(
                hint(d).contains("move the class variables out of `Tcp`"),
                "{d:?}"
            );
        }
        assert!(in_caller(src).is_empty());
    }

    #[test]
    fn stateless_open_class_no_longer_fires_class_process_ffi() {
        let src = "Object subclass: Tcp
  class accept: ls => (Erlang gen_tcp) accept: ls
  class me => Erlang erlang self
";
        assert!(in_class_process(src).is_empty());
        // `self` is the caller's now: the inverted lint takes over.
        assert_eq!(in_caller(src).len(), 1);
    }

    #[test]
    fn standalone_class_method_of_stateful_class_fires_in_class_process() {
        let src = "Object subclass: Host\n  classState: n = 0\n\nHost class >> me => Erlang erlang self\n";
        let diags = in_class_process(src);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("`Host me`"));
    }

    // ── category / policy / @expect ──────────────────────────────────────

    #[test]
    fn expect_category_names_round_trip() {
        for (name, cat) in [
            (
                "class_process_ffi",
                crate::ast::ExpectCategory::ClassProcessFfi,
            ),
            (
                "class_method_process_resource",
                crate::ast::ExpectCategory::ClassMethodProcessResource,
            ),
        ] {
            assert_eq!(crate::ast::ExpectCategory::from_name(name), Some(cat));
            assert_eq!(cat.as_str(), name);
            assert!(crate::ast::ExpectCategory::valid_names().contains(&name));
        }
    }

    #[test]
    fn expect_class_process_ffi_suppresses_the_warning_with_class_state() {
        let src = "Object subclass: Stateful
  classState: n = 0

  class me =>
    @expect class_process_ffi
    Erlang erlang self
";
        let diags = after_expect(src);
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

    #[test]
    fn expect_class_method_process_resource_suppresses_the_warning() {
        let src = "Object subclass: OpenC
  class me =>
    @expect class_method_process_resource
    Erlang erlang self
";
        let diags = after_expect(src);
        assert!(
            !diags
                .iter()
                .any(|d| d.category == Some(DiagnosticCategory::ClassMethodProcessResource)),
            "{diags:?}"
        );
        assert!(
            !diags.iter().any(|d| d.message.contains("stale @expect")),
            "the directive matched: {diags:?}"
        );
    }

    #[test]
    fn expect_class_process_ffi_in_stateless_class_is_reported_unused() {
        let src = "Object subclass: OpenC
  class accept: ls =>
    @expect class_process_ffi
    (Erlang gen_tcp) accept: ls
";
        let diags = after_expect(src);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("stale @expect class_process_ffi")),
            "{diags:?}"
        );
    }
}
